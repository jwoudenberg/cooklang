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

    def getChunks(self):
        return self.chunks

    def extend(self, builder):
        self.chunks.extend(builder.getChunks())

    def tobytes(self):
        result = bytearray(self.size)
        index = 0
        for chunk in self.chunks:
            result[index : index + len(chunk)] = chunk
            index += len(chunk)
        return result
