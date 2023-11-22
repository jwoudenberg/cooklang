import cooklang.util as util
import cooklang.parser as parser
from cooklang.builder import Builder


def to_groceries(recipeText, portions=None):
    def create_recipe(metadata):
        if portions is None:
            return RecipeGroceries(1)
        else:
            servings = int(metadata.get("servings", 1))
            return RecipeGroceries(portions / servings)

    return parser.parseRecipe(recipeText, create_recipe).groceries()


class RecipeGroceries:
    def __init__(self, ingredient_multiplier):
        self.builder = Builder()
        self.ingredient_multiplier = ingredient_multiplier

    def groceries(self):
        return self.builder.tobytes()

    def append_str(self, string):
        self.builder.append(string.encode("utf8"))

    def appendInstruction(self, text):
        return None

    def addIngredient(self, ingredient):
        name = ingredient["name"]
        quantity = ingredient.get("quantity", None)
        unit = ingredient.get("unit", None)
        self.append_str(name.lower())
        if quantity is not None:
            quantity = self.ingredient_multiplier * quantity
            self.append_str(f", {util.formatNumber(quantity)}")
        if unit is not None:
            self.append_str(f" {unit}")
        self.builder.append(b"\n")

    def addCookware(self, cookware):
        return None

    def addTimer(self, timer):
        return None
