import html
import cooklang.util as util
import cooklang.parser as parser
from cooklang.builder import Builder


def toHtml(recipeText, **kwargs):
    def create_recipe(metadata):
        return HtmlRecipe(metadata, **kwargs)

    return parser.parseRecipe(recipeText, create_recipe).html()


class HtmlRecipe:
    def __init__(
        self,
        metadata,
        portions=None,
        multiplier=None,
        i18n_ingredients="Ingredients",
        i18n_instructions="Instructions",
        i18n_servings="Serves $servings",
        l10n_lang="en-US",
    ):
        self.i18n_ingredients = i18n_ingredients
        self.i18n_instructions = i18n_instructions
        self.i18n_servings = i18n_servings
        self.l10n_lang = l10n_lang

        self.instructions = Builder()
        self.ingredients = []
        self.title = metadata.pop("title", None)
        self.servings = metadata.pop("servings", None)
        self.metadata = metadata
        self.ingredient_multiplier = util.getMultiplier(
            self.servings, portions, multiplier
        )
        if self.servings is not None:
            self.servings = util.formatNumber(
                float(self.servings) * self.ingredient_multiplier
            )

    def html(self):
        builder = Builder()
        builder.append(b"<!DOCTYPE html>")
        return tag(builder, b"html", self.printHtml, {"lang": self.l10n_lang}).tobytes()

    def printIngredients(self, builder):
        for ingredient in self.ingredients:
            tag(builder, b"li", ingredientToText(ingredient))

    def printHtml(self, html):
        tag(html, b"head", self.printHead)
        tag(html, b"body", self.printBody)

    def printBody(self, builder):
        if self.title is not None:
            tag(builder, b"h1", self.title)
        tag(builder, b"h2", self.i18n_ingredients)
        if self.servings is not None:
            tag(builder, b"p", self.i18n_servings.replace("$servings", self.servings))
        tag(builder, b"ul", self.printIngredients)
        tag(builder, b"h2", self.i18n_instructions)
        tag(
            builder,
            b"p",
            lambda builder: builder.extend(self.instructions),
            {"tabindex": "0"},
        )

    def printHead(self, builder):
        tag(builder, b"meta", None, {"charset": "utf-8"})
        for key, value in self.metadata.items():
            tag(builder, b"meta", None, {"name": key, "content": value})
        if self.title is not None:
            tag(builder, b"title", self.title)

    def appendEscaped(self, text):
        escaped = (
            html.escape(text).replace("\n\n", '</p><p tabindex="0">').encode("utf8")
        )
        self.instructions.append(escaped)

    def appendInstruction(self, text):
        self.appendEscaped(bytes(text).decode("utf8"))

    def addIngredient(self, ingredient):
        name = ingredient["name"]
        quantity = ingredient.get("quantity", None)
        unit = ingredient.get("unit", None)
        if quantity is not None:
            ingredient["quantity"] = self.ingredient_multiplier * quantity
            self.appendEscaped(f"{util.formatNumber(ingredient['quantity'])} ")
        if unit is not None:
            self.appendEscaped(f"{unit} ")
        self.ingredients.append(ingredient)
        self.appendEscaped(name)

    def addCookware(self, cookware):
        self.appendEscaped(cookware["name"])

    def addTimer(self, timer):
        quantity = timer["quantity"]
        unit = timer["unit"]
        self.appendEscaped(f"{util.formatNumber(quantity)} {unit}")


def tag(builder, tagname, inTag=lambda _: {}, attributes={}):
    """
    Render an empty html tag

    >>> tag(Builder(), b'art').tobytes().decode('utf8')
    '<art></art>'

    Render an html tag with text contents

    >>> tag(Builder(), b'art', 'Hello <<name>>').tobytes().decode('utf8')
    '<art>Hello &lt;&lt;name&gt;&gt;</art>'

    Render an html tag containing another tag

    >>> tag(Builder(), b'art', lambda b: tag(b, b'red')).tobytes().decode('utf8')
    '<art><red></red></art>'

    Render an empty html tag when third argument is explicit 'None'

    >>> tag(Builder(), b'art', None).tobytes().decode('utf8')
    '<art/>'

    Render an html tag with attributes

    >>> tag(Builder(), b'meta', None, { 'charset': "utf8" }).tobytes().decode('utf8')
    '<meta charset="utf8"/>'
    """

    def appendCloseTag():
        builder.append(b"</")
        builder.append(tagname)
        builder.append(b">")

    builder.append(b"<")
    builder.append(tagname)
    for key, value in attributes.items():
        builder.append(b" ")
        builder.append(key.encode("utf8"))
        builder.append(b'="')
        builder.append(html.escape(str(value)).encode("utf8"))
        builder.append(b'"')
    if callable(inTag):
        builder.append(b">")
        inTag(builder)
        appendCloseTag()
    elif inTag is not None:
        builder.append(b">")
        builder.append(html.escape(inTag).encode("utf8"))
        appendCloseTag()
    else:
        builder.append(b"/>")

    return builder


def ingredientToText(ingredient):
    """
    Return the ingredient name as a string

    >>> ingredientToText({ 'name': 'carrots' })
    'carrots'

    Return ingredient with quantity

    >>> ingredientToText({ 'name': 'carrots', 'quantity': 0.5 })
    'carrots, 0.5'

    Return ingredient with quantity and unit

    >>> ingredientToText({ 'name': 'carrots', 'quantity': 0.5, 'unit': 'kg' })
    'carrots, 0.5 kg'

    Return empty string if no ingredient name is present

    >>> ingredientToText({})
    ''

    Lowercase the ingredient name

    >>> ingredientToText({ 'name': 'Carrots' })
    'carrots'
    """

    name = ingredient.get("name", "").lower()
    amount = ingredient.get("quantity", None)
    unit = ingredient.get("unit", None)
    if amount is None and unit is None:
        return name
    elif unit is None:
        return f"{name}, {util.formatNumber(amount)}"
    else:
        return f"{name}, {util.formatNumber(amount)} {unit}"
