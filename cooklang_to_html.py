import html
from builder import Builder


def toHtml(recipe):
    """
    Render a recipe as an HTML page.

    >>> toHtml({ 'instructions': 'Chop onions', 'ingredients': [{'name': 'onions'}] })
    '<!DOCTYPE html><html><body>\
<h2>Ingredients</h2>\
<ul><li>onions</li></ul>\
<h2>Instructions</h2>\
<p>Chop onions</p>\
</body></html>'
    """

    ingredients = recipe.get("ingredients", None)
    instructions = recipe.get("instructions", None)

    def printIngredients(builder):
        for ingredient in ingredients:
            tag(builder, b"li", ingredientToText(ingredient))

    def printBody(builder):
        if ingredients is not None:
            tag(builder, b"h2", "Ingredients")
            tag(builder, b"ul", printIngredients)
        if instructions is not None:
            tag(builder, b"h2", "Instructions")
            tag(builder, b"p", instructions)

    builder = Builder()
    builder.append(b"<!DOCTYPE html>")
    tag(builder, b"html", lambda builder: tag(builder, b"body", printBody))
    return builder.tobytes().decode("utf8")


def tag(builder, tagname, inTag=lambda _: {}):
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
    """

    builder.append(b"<")
    builder.append(tagname)
    builder.append(b">")
    if callable(inTag):
        inTag(builder)
    else:
        builder.append(html.escape(inTag).encode("utf8"))
    builder.append(b"</")
    builder.append(tagname)
    builder.append(b">")
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