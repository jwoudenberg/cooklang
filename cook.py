from collections import namedtuple

Recipe = namedtuple("Recipe", ["instructions", "ingredients"])


def parseRecipe(text):
    """
    Parse a cooklang recipe text

    >>> parseRecipe(b'Add the @onions to the pan')
    b'onions'
    """

    recipe = Recipe(instructions="", ingredients=[])
    text = memoryview(text)
    (_, text) = takeWhile(text, lambda char: char != ord("@"))
    (result, text) = parseIngredient(memoryview(text))
    return result.tobytes()


def parseIngredient(text):
    r"""
    Parse a cooklang ingredient"

    >>> parseIngredient(b'@onions to the pan')
    (b'onions', b' to the pan')

    >>> parseIngredient(b'@onions')
    (b'onions', b'')

    >>> parseIngredient(b'@onions\nare delicious')
    (b'onions', b'\nare delicious')
    """

    (atSign, text) = take(text, 1)
    if atSign != b"@":
        raise ValueError("Expected text to start with a '@', but it did not")

    (ingredient, text) = takeWhile(text, lambda char: char not in b" \n")
    return (ingredient, text)


def take(text, n):
    """
    Split a string after the provided number of characters.

    >>> take('what a day', 4)
    ('what', ' a day')
    """
    return (text[0:n], text[n:])


def takeWhile(text, predicate):
    """
    Split a string on the first character that matches a predicate.

    >>> takeWhile('what a day', lambda char: char != "a")
    ('wh', 'at a day')

    >>> takeWhile('aaaa aa', lambda char: char != "b")
    ('aaaa aa', '')
    """

    index = 0
    try:
        while predicate(text[index]):
            index += 1
    except IndexError:
        pass
    result = text[0:index]
    remaining = text[index:]
    return (result, remaining)


# Example and tests
import doctest

doctest.testmod()

parsed = parseRecipe(b"Then add @salt and @ground black pepper{} to taste.")
print(parsed)
