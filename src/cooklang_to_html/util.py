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
