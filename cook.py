from collections import namedtuple
from functools import reduce

Recipe = namedtuple("Recipe", ["instructions", "ingredients"])


def parseRecipe(text):
    """
    Parse a cooklang recipe text

    >>> parseRecipe(b'Add the @onions to the @garlic')
    Recipe(instructions='Add the onions to the garlic', ingredients=[b'onions', b'garlic'])
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
        ingredients.append(ingredient.tobytes())
        instructionBuilder.append(ingredient)

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
        self.size += chunk.nbytes

    def tobytes(self):
        result = bytearray(self.size)
        index = 0
        for chunk in self.chunks:
            result[index : index + chunk.nbytes] = chunk
            index += chunk.nbytes
        return result


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
