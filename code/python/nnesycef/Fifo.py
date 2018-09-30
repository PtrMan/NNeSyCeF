class Fifo(object):
    def __init__(self, size):
        self._size = size
        self._arr = []

        ## used for iteration
        self._currentIdx = 0


    def append(self, e):
        self._arr.append(e)
        self._arr = self._arr[max(0, len(self._arr) - self._size):] # limit length of FIFO

    def take(self):
        result = self._arr[0]
        self._arr = self._arr[1:]

    def __getitem__(self, idx):
        return self._arr[idx]

    def __len__(self):
        return len(self._arr)

    def __iter__(self):
        self._currentIdx = 0
        return self

    def __next__(self):
        if self._currentIdx >= len(self):
            raise StopIteration
        else:
            self._currentIdx += 1
            return self._arr[self._currentIdx-1]