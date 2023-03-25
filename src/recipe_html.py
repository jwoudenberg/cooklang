import html
import cooklang
from builder import Builder


def toHtml(recipeText):
    return cooklang.parseRecipe(recipeText, HtmlRecipe).html()


class HtmlRecipe:
    def __init__(self, metadata):
        self.instructions = Builder()
        self.ingredients = []
        self.title = metadata.pop("title", None)
        self.servings = metadata.pop("servings", None)
        self.metadata = metadata

    def html(self):
        html = Builder()
        html.append(b"<!DOCTYPE html>")
        tag(html, b"head", self.printHead)
        tag(html, b"body", self.printBody)
        return html.tobytes()

    def printIngredients(self, builder):
        for ingredient in self.ingredients:
            tag(builder, b"li", ingredientToText(ingredient))

    def printBody(self, builder):
        if self.title is not None:
            tag(builder, b"h1", self.title)
        if self.servings is not None:
            tag(builder, b"h2", f"Ingredients (serves {self.servings})")
        else:
            tag(builder, b"h2", "Ingredients")
        tag(builder, b"ul", self.printIngredients)
        tag(builder, b"h2", "Instructions")
        tag(builder, b"p", lambda builder: builder.extend(self.instructions))

    def printHead(self, builder):
        tag(builder, b"meta", None, {"charset": "utf-8"})
        for key, value in self.metadata.items():
            tag(builder, b"meta", None, {"name": key, "content": value})
        if self.title is not None:
            tag(builder, b"title", self.title)

    def appendInstruction(self, text):
        self.instructions.append(text)

    def addIngredient(self, ingredient):
        name = ingredient["name"]
        quantity = ingredient.get("quantity", None)
        unit = ingredient.get("unit", None)
        if quantity is not None:
            self.instructions.append(bytes(f"{quantity} ", encoding="utf8"))
        if unit is not None:
            self.instructions.append(bytes(f"{unit} ", encoding="utf8"))
        self.ingredients.append(ingredient)
        self.instructions.append(bytes(name, encoding="utf8"))

    def addCookware(self, cookware):
        self.instructions.append(bytes(cookware["name"], encoding="utf8"))

    def addTimer(self, timer):
        quantity = timer["quantity"]
        unit = timer["unit"]
        self.instructions.append(bytes(f"{quantity} {unit}", encoding="utf8"))


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
    """

    name = ingredient.get("name", "")
    amount = ingredient.get("quantity", None)
    unit = ingredient.get("unit", None)
    if amount is None and unit is None:
        return name
    elif unit is None:
        return f"{name}, {amount}"
    else:
        return f"{name}, {amount} {unit}"
