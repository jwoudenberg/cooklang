import html
from builder import Builder


def toHtml(recipe):
    """
    Render a recipe as an HTML page.

    >>> toHtml({ 'instructions': 'Chop onions', 'ingredients': [{'name': 'onions'}] })
    '<!DOCTYPE html><html>\
<head>\
<meta charset="utf8"></meta>\
</head>\
<body>\
<h2>Ingredients</h2>\
<ul><li>onions</li></ul>\
<h2>Instructions</h2>\
<p>Chop onions</p>\
</body></html>'

    Show ingredients with servings

    >>> toHtml({ 'ingredients': [{'name': 'onions'}], 'metadata': {'servings': 3} })
    '<!DOCTYPE html><html>\
<head>\
<meta charset="utf8"></meta>\
</head>\
<body>\
<h2>Ingredients (serves 3)</h2>\
<ul><li>onions</li></ul>\
</body></html>'

    Show a recipe title.

    >>> toHtml({ 'metadata': { 'title': 'Food' } })
    '<!DOCTYPE html><html>\
<head>\
<meta charset="utf8"></meta>\
<title>Food</title>\
</head>\
<body>\
<h1>Food</h1>\
</body></html>'

    Adds <meta/> tags for metadata entries

    >>> toHtml({ 'metadata': { 'desert': True } })
    '<!DOCTYPE html><html>\
<head>\
<meta charset="utf8"></meta>\
<meta name="desert" content="True"></meta>\
</head>\
<body>\
</body></html>'
    """

    ingredients = recipe.get("ingredients", None)
    instructions = recipe.get("instructions", None)
    metadata = recipe.get("metadata", {})
    title = metadata.pop("title", None)
    servings = metadata.pop("servings", None)

    def printIngredients(builder):
        for ingredient in ingredients:
            tag(builder, b"li", ingredientToText(ingredient))

    def printBody(builder):
        if title is not None:
            tag(builder, b"h1", title)
        if ingredients is not None:
            if servings is not None:
                tag(builder, b"h2", f"Ingredients (serves {servings})")
            else:
                tag(builder, b"h2", "Ingredients")
            tag(builder, b"ul", printIngredients)
        if instructions is not None:
            tag(builder, b"h2", "Instructions")
            tag(builder, b"p", instructions)

    def printHead(builder):
        tag(builder, b"meta", None, {"charset": "utf8"})
        for key, value in metadata.items():
            tag(builder, b"meta", None, {"name": key, "content": value})
        if title is not None:
            tag(builder, b"title", title)

    def printHtml(builder):
        tag(builder, b"head", printHead)
        tag(builder, b"body", printBody)

    builder = Builder()
    builder.append(b"<!DOCTYPE html>")
    tag(builder, b"html", printHtml)
    return builder.tobytes().decode("utf8")


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
    '<art></art>'

    Render an html tag with attributes

    >>> tag(Builder(), b'meta', None, { 'charset': "utf8" }).tobytes().decode('utf8')
    '<meta charset="utf8"></meta>'
    """

    builder.append(b"<")
    builder.append(tagname)
    for key, value in attributes.items():
        builder.append(b" ")
        builder.append(key.encode("utf8"))
        builder.append(b'="')
        builder.append(html.escape(str(value)).encode("utf8"))
        builder.append(b'"')
    builder.append(b">")
    if callable(inTag):
        inTag(builder)
    elif inTag is not None:
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
