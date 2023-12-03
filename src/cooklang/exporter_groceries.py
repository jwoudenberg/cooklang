import cooklang.util as util
import cooklang.parser as parser
import re


def to_groceries(recipeText, portions=None, multiplier=None, existing_groceries=None):
    groceries = (
        Groceries()
        if existing_groceries is None
        else parse_groceries(existing_groceries)
    )

    def create_recipe(metadata):
        ingredient_multiplier = util.getMultiplier(
            metadata.get("servings"), portions, multiplier
        )
        return ParsedRecipe(ingredient_multiplier, groceries)

    return parser.parseRecipe(recipeText, create_recipe).print()


class ParsedRecipe:
    def __init__(self, ingredient_multiplier, groceries):
        self.ingredient_multiplier = ingredient_multiplier
        self.groceries = groceries

    def print(self):
        return self.groceries.print()

    def appendInstruction(self, text):
        return None

    def addIngredient(self, ingredient):
        if ingredient.get("quantity", None) is not None:
            ingredient["quantity"] *= self.ingredient_multiplier
            ingredient["name"] = ingredient["name"].lower()
        self.groceries.add(ingredient)

    def addCookware(self, cookware):
        return None

    def addTimer(self, timer):
        return None


class Groceries:
    def __init__(self):
        self.groceries_by_key = {}

    def add(self, original_ingredient):
        ingredient = standardize_unit(original_ingredient)
        key = self.key(ingredient)
        if key in self.groceries_by_key:
            if ingredient.get("quantity", None) is not None:
                self.groceries_by_key[key]["quantity"] = (
                    self.groceries_by_key[key].get("quantity", 0)
                    + ingredient["quantity"]
                )
        else:
            self.groceries_by_key[key] = ingredient

    def key(self, ingredient):
        return f"{ingredient['name']}-{ingredient.get('unit', None)}"

    def print(self):
        lines = []
        for original_ingredient in self.groceries_by_key.values():
            ingredient = presentation_unit(original_ingredient)
            line = ingredient.get("description", ingredient["name"])
            quantity = ingredient.get("quantity", None)
            unit = ingredient.get("unit", None)
            if quantity is not None:
                line += f", {util.formatNumber(quantity)}"
            if unit is not None:
                line += f" {unit}"
            lines.append(line)

        lines = "\n".join(sorted(lines))
        return f"{lines}\n".encode("utf8")


def parse_groceries(string):
    lines = string.split("\n")
    groceries = Groceries()
    for line in lines:
        if line == "":
            continue
        groceries.add(parse_grocery_line(line))
    return groceries


def parse_grocery_line(line):
    """
    grocery with only ingredient name
    >>> parse_grocery_line('onions')
    {'description': 'onions', 'quantity': None, 'unit': None, 'name': 'onions'}

    grocery with name and quantity
    >>> parse_grocery_line('onions, 3.5')
    {'description': 'onions', 'quantity': 3.5, 'unit': None, 'name': 'onions'}

    grocery with name, quantity, and unit
    >>> parse_grocery_line('onions, 3 kg')
    {'description': 'onions', 'quantity': 3.0, 'unit': 'kg', 'name': 'onions'}

    grocery description using todo.txt tags
    >>> parse_grocery_line('@yummy red +colorfull onions, 3 kg')
    {'description': '@yummy red +colorfull onions'\
, 'quantity': 3.0, 'unit': 'kg', 'name': 'red onions'}

    lowercases ingredient names but not units
    >>> parse_grocery_line('Water, 5 L')
    {'description': 'water', 'quantity': 5.0, 'unit': 'L', 'name': 'water'}


    passes through empty lines
    >>> parse_grocery_line('')
    {'name': ''}

    passes through unparsable lines
    >>> parse_grocery_line(',')
    {'name': ','}

    passes through unparsable quantities
    >>> parse_grocery_line('onions , three kg')
    {'name': 'onions , three kg'}
    """
    match = re.fullmatch(
        "(?P<description>[^,]+)(,\\s*(?P<quantity>[\\d\\.,]+)(\\s+(?P<unit>\\w+))?)?",
        line,
    )
    if match is None:
        return {"name": line}
    description = match.group("description").lower()
    quantity_string = match.group("quantity")
    try:
        quantity = float(quantity_string) if quantity_string is not None else None
    except ValueError:
        return {"name": line}
    unit = match.group("unit")
    name = re.compile("[+@]\\w+\\s*").sub("", description)
    ingredient = {
        "description": description,
        "quantity": quantity,
        "unit": unit,
        "name": name,
    }
    return ingredient


def standardize_unit(ingredient):
    """
    Convert a weight in kg to mg
    >>> standardize_unit({'quantity': 2.5, 'unit': 'kg'})
    {'quantity': 2500000.0, 'unit': 'mg'}

    Leave 'None' units alone
    >>> standardize_unit({'quantity': 2, 'unit': None})
    {'quantity': 2, 'unit': None}

    Leave unknown units alone
    >>> standardize_unit({'quantity': 2, 'unit': 'Volt'})
    {'quantity': 2, 'unit': 'Volt'}

    Leave unknown quantities alone
    >>> standardize_unit({'quantity':None, 'unit':None})
    {'quantity': None, 'unit': None}
    """
    quantity = ingredient.get("quantity", None)
    unit = ingredient.get("unit", None)
    conversions = {
        # Weights
        "kg": lambda: (quantity * 1e6, "mg"),
        "g": lambda: (quantity * 1e3, "mg"),
        "mespunt": lambda: (quantity * 1.5e3, "mg"),
        "snuf": lambda: (quantity * 400, "mg"),
        # Volumes
        "L": lambda: (quantity * 1e3, "mL"),
        "dL": lambda: (quantity * 10, "mL"),
        "mL": lambda: (quantity, "mL"),
        "tl": lambda: (quantity * 5, "mL"),
        "el": lambda: (quantity * 15, "mL"),
        "druppel": lambda: (quantity * 0.05, "mL"),
    }
    new_quantity, new_unit = conversions.get(unit, lambda: (quantity, unit))()
    ingredient_copy = ingredient.copy()
    ingredient_copy["quantity"] = new_quantity
    ingredient_copy["unit"] = new_unit
    return ingredient_copy


def presentation_unit(original_ingredient):
    """
    >>> presentation_unit({'quantity': 20, 'unit': 'mg'})
    {'quantity': 20, 'unit': 'mg'}

    >>> presentation_unit({'quantity': 2e3, 'unit': 'mg'})
    {'quantity': 2.0, 'unit': 'g'}

    >>> presentation_unit({'quantity': 2e6, 'unit': 'mg'})
    {'quantity': 2.0, 'unit': 'kg'}

    >>> presentation_unit({'quantity': 2, 'unit': 'mL'})
    {'quantity': 2, 'unit': 'mL'}

    >>> presentation_unit({'quantity': 20, 'unit': 'mL'})
    {'quantity': 2.0, 'unit': 'dL'}

    >>> presentation_unit({'quantity': 2000, 'unit': 'mL'})
    {'quantity': 2.0, 'unit': 'L'}

    Leave 'None' units alone
    >>> presentation_unit({'quantity': 2, 'unit': None})
    {'quantity': 2, 'unit': None}

    Leave unknown units alone
    >>> presentation_unit({'quantity': 2, 'unit': 'Volt'})
    {'quantity': 2, 'unit': 'Volt'}
    """
    ingredient = original_ingredient.copy()
    quantity = ingredient.get("quantity", None)
    unit = ingredient.get("unit", None)
    if unit == "mg":
        if quantity < 1e3:
            next
        elif quantity < 1e6:
            ingredient["quantity"] /= 1e3
            ingredient["unit"] = "g"
        else:
            ingredient["quantity"] /= 1e6
            ingredient["unit"] = "kg"
    elif unit == "mL":
        if quantity < 10:
            next
        elif quantity < 1e2:
            ingredient["quantity"] /= 10
            ingredient["unit"] = "dL"
        else:
            ingredient["quantity"] /= 1e3
            ingredient["unit"] = "L"
    return ingredient
