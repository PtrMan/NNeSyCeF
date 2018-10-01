class Stamp(object):
    def __init__(self, arr):
        self._arr = arr

    def __getitem__(self, idx):
        return self._arr[idx]

    @staticmethod
    def merge(a, b):
        zipped = []

        ia = 0
        ib = 0

        for i in range(0, min(len(a._arr), len(b._arr))):
            if (i % 2) == 0:
                zipped.append(a[ia])
                ia+=1
            else:
                zipped.append(b[ib])
                ib+=1

        ## append remaining part of either stamp
        zipped.extend(a._arr[ia:])
        zipped.extend(b._arr[ib:])

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
