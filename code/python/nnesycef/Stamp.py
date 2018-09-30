class Stamp(object):
    def __init__(self, arr):
        self._arr = arr

    @staticmethod
    def merge(a, b):
        zipped = []

        for i in range(0, len(a._arr) + len(b._arr)):
            if (i % 2) == 0:
                zipped.append(a[int(i/2)])
            else:
                zipped.append(b[int(i/2)])

        ## limit length
        zipped = zipped[:100]

        return Stamp(zipped)

    @staticmethod
    def checkOverlap(a, b):
        set_ = set()
        for iA in a._arr:
            set_.add(iA)

        for iB in b._arr:
            if iB in set_:
                return True

        return False
