from collections import namedtuple
from functools import reduce
from sys import intern

Recipe = namedtuple("Recipe", ["instructions", "ingredients"])

Ingredient = namedtuple("Ingredient", ["name", "amount"])


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
    """

    (atSign, text) = take(text, 1)
    if atSign != b"@":
        raise ValueError("Expected text to start with a '@', but it did not")

    (ingredient, remaining) = takeWhile(text, lambda char: char not in b"{@\n")
    if remaining[0:2] == b"{}":
        remaining = remaining[2:]
    else:
        (ingredient, remaining) = takeWhile(text, lambda char: char not in b" \n")

    name = intern(bytes(ingredient).decode("utf8"))
    return (Ingredient(name=name, amount=None), remaining)


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
