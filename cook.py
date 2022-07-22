from collections import namedtuple
from sys import intern

Recipe = namedtuple("Recipe", ["instructions", "ingredients"])

Ingredient = namedtuple("Ingredient", ["name", "amount"])

Amount = namedtuple("Amount", ["quantity", "unit"])


def parseRecipe(text):
    """
    Parse a cooklang recipe text

    >>> parseRecipe(b'Add the @chopped onions{} to the @garlic')
    Recipe(instructions='Add the chopped onions to the garlic', ingredients=[\
Ingredient(name='chopped onions', amount=None), \
Ingredient(name='garlic', amount=None)\
])
    """

    # Normally when taking a slice out of a bytestring python copies the slice
    # out of the original bytestring into a new one. A bytestring wrapped in a
    # memoryview does not create such copies: slices will reference bits of the
    # original bytestring. Because we will create a lot of slices during parsing
    # this seems a good optimization.
    text = memoryview(text)
    ingredients = []
    instructionBuilder = Builder()

    while True:
        (instruction, text) = takeWhile(text, lambda char: char != ord("@"))
        if text == b"":
            break
        instructionBuilder.append(instruction)
        (ingredient, text) = parseIngredient(memoryview(text))
        ingredients.append(ingredient)
        instructionBuilder.append(bytes(ingredient.name, encoding="utf8"))

    instructions = instructionBuilder.tobytes().decode("utf8")
    recipe = Recipe(instructions=instructions, ingredients=ingredients)
    return recipe


class Builder:
    """
    Helper for constructing a bytestring from a list of memoryview slices.
    This approach directly copies each slice into the result bytestring.

    >>> str = memoryview(b"Hello there, check out our world beating prices!")
    >>> builder = Builder()
    >>> builder.append(str[0:6])
    >>> builder.append(str[27:32])
    >>> builder.tobytes().decode('utf8')
    'Hello world'
    """

    def __init__(self):
        self.chunks = []
        self.size = 0

    def append(self, chunk):
        self.chunks.append(chunk)
        self.size += len(chunk)

    def tobytes(self):
        result = bytearray(self.size)
        index = 0
        for chunk in self.chunks:
            result[index : index + len(chunk)] = chunk
            index += len(chunk)
        return result


def parseIngredient(text):
    r"""
    Parse a cooklang ingredient"

    Ingredients are whitespace-separated words starting with @

    >>> parseIngredient(b'@onions to the pan')
    (Ingredient(name='onions', amount=None), b' to the pan')

    >>> parseIngredient(b'@onions')
    (Ingredient(name='onions', amount=None), b'')

    >>> parseIngredient(b'@onions\nare delicious')
    (Ingredient(name='onions', amount=None), b'\nare delicious')

    Alternatively multi-word ingredients are written between @ and {}

    >>> parseIngredient(b'@chopped onions{}')
    (Ingredient(name='chopped onions', amount=None), b'')

    >>> parseIngredient(b'@chopped onions{} to the pan')
    (Ingredient(name='chopped onions', amount=None), b' to the pan')

    >>> parseIngredient(b'@garlic and @chopped onions{}')
    (Ingredient(name='garlic', amount=None), b' and @chopped onions{}')

    Ingredient amounts can be specified between curly braces

    >>> parseIngredient(b'@onions{2}')
    (Ingredient(name='onions', amount=Amount(quantity=2.0, unit=None)), b'')

    A missing closing } results in an error

    >>> parseIngredient(b'@chopped onions{')
    Traceback (most recent call last):
    ...
    ParseException: Expected b'}' but got b''
    """

    text = exactly(text, b"@")
    amount = None
    (ingredient, remaining) = takeWhile(text, lambda char: char not in b"{@\n")
    if len(remaining) > 0 and remaining[0] == ord("{"):
        (amount, remaining) = parseAmount(remaining)
    else:
        (ingredient, remaining) = takeWhile(text, lambda char: char not in b" \n")

    name = intern(bytes(ingredient).decode("utf8"))
    return (Ingredient(name=name, amount=amount), remaining)


def parseAmount(text):
    """
    Parse an ingredient amount between curly braces.

    >>> parseAmount(b'{} hi')
    (None, b' hi')

    >>> parseAmount(b'{2.5}')
    (Amount(quantity=2.5, unit=None), b'')

    A unit can be provided behind a percentage sign.

    >>> parseAmount(b"{2%kg}")
    (Amount(quantity=2.0, unit='kg'), b'')

    Passing an invalid amount raises an exception.

    >>> parseAmount(b'{hi}')
    Traceback (most recent call last):
    ...
    ParseException: Expected a number but got b'hi'
    """

    text = exactly(text, b"{")
    (amountString, text) = takeWhile(text, lambda char: char not in b"}\n")
    amount = None
    if amountString != b"":
        (quantityString, rest) = takeWhile(amountString, lambda char: char != ord("%"))
        unit = None
        if len(rest) > 0:
            unit = intern(bytes(rest[1:]).decode("utf8"))
        quantity = number(quantityString)
        amount = Amount(quantity=quantity, unit=unit)
    text = exactly(text, b"}")

    return (amount, text)


# Parser helper functions unrelated to recipes.


class ParseException(Exception):
    pass


def number(text):
    """
    Parse a number

    >>> number("2")
    2.0

    >>> number("2.5")
    2.5

    >>> number(b"hi")
    Traceback (most recent call last):
    ...
    ParseException: Expected a number but got b'hi'
    """

    try:
        return float(text)
    except ValueError:
        raise ParseException(f"Expected a number but got {text}")


def exactly(text, expected):
    """
    Parse an exact set of characters, or raise an error.

    >>> exactly(b"hi there", b"hi")
    b' there'

    >>> exactly(b"hi there", b"ho") is None
    Traceback (most recent call last):
    ...
    ParseException: Expected b'ho' but got b'hi'
    """

    expectedSize = len(expected)
    if text[0:expectedSize] == expected:
        return text[expectedSize:]
    else:
        raise ParseException(f"Expected {expected} but got {text[0:expectedSize]}")


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
