import sys


def getMultiplier(recipe_servings=None, desired_portions=None, desired_multiplier=None):
    """
    >>> getMultiplier(1, 2)
    2.0

    >>> getMultiplier(1, None, 3.0)
    3.0

    >>> getMultiplier(1, None, None)
    1.0

    >>> getMultiplier("1", 2)
    2.0
    """
    if desired_portions is not None and desired_multiplier is not None:
        sys.stderr.write("Specify at most one of --portions and --multiplier.\n")
        sys.exit(1)

    if recipe_servings is None and desired_portions is not None:
        sys.stderr.write("Cannot use --portions for recipe without servings.\n")
        sys.exit(1)

    if desired_portions is not None:
        return desired_portions / float(recipe_servings)

    if desired_multiplier is not None:
        return desired_multiplier

    return 1.0


def formatNumber(number):
    """
    Return number without trailing zeroes

    >>> formatNumber(2.0)
    '2'

    Return number rounded to two signficant digits

    >>> formatNumber(2.111)
    '2.1'

    Return numbers between 10 and 100 without digits

    >>> formatNumber(88.81)
    '89'

    >>> formatNumber(2.111)
    '2.1'

    Return large numbers without scientific notation

    >>> formatNumber(2000)
    '2000'
    """
    if number >= 100:
        return f"{number:.0f}"
    else:
        return f"{number:.2g}"
