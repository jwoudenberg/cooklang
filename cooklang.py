from sys import intern
from builder import Builder

"""
A parser for the cooklang language, as specified here:
https://cooklang.org/docs/spec/
"""


def parseRecipe(text):
    """
    Parse a cooklang recipe text

    >>> parseRecipe(b'Add the @chopped onions{} to a #pan and simmer for ~{5%minutes}')
    {'instructions': 'Add the chopped onions to a pan and simmer for 5.0 minutes'\
, 'ingredients': [{'name': 'chopped onions'}]\
, 'cookwares': [{'name': 'pan'}]\
, 'timers': [{'quantity': 5.0, 'unit': 'minutes'}]\
, 'metadata': {}\
}

    Line-comments are ignored. Single hyphens don't mess things up

    >>> parseRecipe(b'Add the six-pack of @alcohol-free beer{} -- or subtract them')
    {'instructions': 'Add the six-pack of alcohol-free beer '\
, 'ingredients': [{'name': 'alcohol-free beer'}]\
, 'cookwares': []\
, 'timers': []\
, 'metadata': {}\
}

    Block-comments are ignored. Single [ brackets don't mess things up.

    >>> parseRecipe(b'Add the [nice] @onions [- TODO use spring onions -] and stir')
    {'instructions': 'Add the [nice] onions  and stir'\
, 'ingredients': [{'name': 'onions'}]\
, 'cookwares': []\
, 'timers': []\
, 'metadata': {}\
}

    Block-comments without closing bracket are accepted.

    >>> parseRecipe(b'Add the [nice] @onions [- TODO use spring onions')
    {'instructions': 'Add the [nice] onions '\
, 'ingredients': [{'name': 'onions'}]\
, 'cookwares': []\
, 'timers': []\
, 'metadata': {}\
}

    The start of the recipe can contain metadata.

    >>> parseRecipe(b'>> servings: 2\\n>>course: dinner\\nAdd the @onions')
    {'instructions': 'Add the onions'\
, 'ingredients': [{'name': 'onions'}]\
, 'cookwares': []\
, 'timers': []\
, 'metadata': {'servings': '2', 'course': 'dinner'}\
}
    """

    # Normally when taking a slice out of a bytestring python copies the slice
    # out of the original bytestring into a new one. A bytestring wrapped in a
    # memoryview does not create such copies: slices will reference bits of the
    # original bytestring. Because we will create a lot of slices during parsing
    # this seems a good optimization.
    text = memoryview(text)
    ingredients = []
    cookwares = []
    timers = []
    metadata = {}
    instructionBuilder = Builder()

    # Parse metadata
    while text[0:2] == b">>":
        text = text[2:]
        (_, text) = whitespace(text)
        (key, text) = takeWhile(text, lambda char: char != ord(":"))
        text = text[1:]
        (_, text) = whitespace(text)
        (val, text) = takeWhile(text, lambda char: char != ord("\n"))
        (_, text) = whitespace(text)
        metadata[key.tobytes().decode("utf8")] = val.tobytes().decode("utf8")

    # Parse the rest of the recipe
    while True:
        (instruction, text) = takeWhile(text, lambda char: char not in b"@#~-[")
        instructionBuilder.append(instruction)
        match text[0:1]:
            case b"":
                break
            case b"-":
                if text[0:2] == b"--":
                    (_, text) = takeWhile(text, lambda char: char != ord("\n"))
                else:
                    instructionBuilder.append(b"-")
                    text = text[1:]
            case b"[":
                if text[0:2] == b"[-":
                    while not (text[0:2] == b"" or text[0:2] == b"-]"):
                        text = text[1:]
                    text = text[2:]
                else:
                    instructionBuilder.append(b"[")
                    text = text[1:]
            case b"@":
                (ingredient, text) = parseTerm(text[1:])
                ingredients.append(ingredient)
                instructionBuilder.append(bytes(ingredient["name"], encoding="utf8"))
            case b"#":
                (cookware, text) = parseTerm(text[1:])
                cookwares.append(cookware)
                instructionBuilder.append(bytes(cookware["name"], encoding="utf8"))
            case b"~":
                (timer, text) = parseTerm(text[1:])
                timers.append(timer)
                quantity = timer["quantity"]
                unit = timer["unit"]
                instructionBuilder.append(bytes(f"{quantity} {unit}", encoding="utf8"))
            case next:
                raise ValueError(
                    f"Expected a @, #, or ~ symbol but got {next.tobytes()}"
                )

    instructions = instructionBuilder.tobytes().decode("utf8")
    recipe = {
        "instructions": instructions,
        "ingredients": ingredients,
        "cookwares": cookwares,
        "timers": timers,
        "metadata": metadata,
    }
    return recipe


def parseTerm(text):
    r"""
    Parse a cooklang ingredient, cookware, or timer.

    >>> parseTerm(b'onions to the pan')
    ({'name': 'onions'}, b' to the pan')

    >>> parseTerm(b'onions')
    ({'name': 'onions'}, b'')

    >>> parseTerm(b'onions\nare delicious')
    ({'name': 'onions'}, b'\nare delicious')

    Interpunction after a name is not part of the name.

    >>> parseTerm(b"onions, they're the best")
    ({'name': 'onions'}, b", they're the best")

    Alternatively multi-word terms are ended by {}

    >>> parseTerm(b'chopped onions{}')
    ({'name': 'chopped onions'}, b'')

    >>> parseTerm(b'chopped onions{} to the pan')
    ({'name': 'chopped onions'}, b' to the pan')

    >>> parseTerm(b'garlic and @chopped onions{}')
    ({'name': 'garlic'}, b' and @chopped onions{}')

    Amounts can be specified between curly braces

    >>> parseTerm(b'onions{2%kg}')
    ({'quantity': 2.0, 'unit': 'kg', 'name': 'onions'}, b'')

    The name is optional (to support anonymous timers)

    >>> parseTerm(b'{2%minutes} hi')
    ({'quantity': 2.0, 'unit': 'minutes'}, b' hi')

    A missing closing } results in an error

    >>> parseTerm(b'chopped onions{')
    Traceback (most recent call last):
    ...
    cooklang.ParseException: Expected b'}' but got b''
    """

    term = {}
    (name, remaining) = takeWhile(text, lambda char: char not in b"{@#~\n")

    if remaining[0:1] == b"{":
        (amount, remaining) = parseAmount(remaining)
        if amount is not None:
            term = amount
    else:
        (name, remaining) = takeWhile(
            text, lambda char: chr(char).isalpha() or char in b"-"
        )

    if name != b"":
        term["name"] = intern(bytes(name).decode("utf8"))

    return (term, remaining)


def parseAmount(text):
    """
    Parse an ingredient amount between curly braces.

    >>> parseAmount(b'{} hi')
    (None, b' hi')

    >>> parseAmount(b'{2.5}')
    ({'quantity': 2.5}, b'')

    A unit can be provided behind a percentage sign.

    >>> parseAmount(b"{2%kg}")
    ({'quantity': 2.0, 'unit': 'kg'}, b'')

    Passing an invalid amount raises an exception.

    >>> parseAmount(b'{hi}')
    Traceback (most recent call last):
    ...
    cooklang.ParseException: Expected a number but got b'hi'
    """

    text = exactly(text, b"{")
    (amountString, text) = takeWhile(text, lambda char: char not in b"}\n")
    amount = None
    if amountString != b"":
        (quantityString, rest) = takeWhile(amountString, lambda char: char != ord("%"))
        amount = {"quantity": number(quantityString)}
        if len(rest) > 0:
            amount["unit"] = intern(bytes(rest[1:]).decode("utf8"))
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
    cooklang.ParseException: Expected a number but got b'hi'
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
    cooklang.ParseException: Expected b'ho' but got b'hi'
    """

    expectedSize = len(expected)
    if text[0:expectedSize] == expected:
        return text[expectedSize:]
    else:
        raise ParseException(f"Expected {expected} but got {text[0:expectedSize]}")


def whitespace(text):
    r"""
    Parse any amount of whitespace.

    >>> whitespace(b'   \t \n\n hi there')
    (b'   \t \n\n ', b'hi there')
    """

    return takeWhile(text, lambda char: char in b" \t\n")


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
