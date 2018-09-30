# see https://github.com/opennars/opennars/blob/master/src/main/java/org/opennars/entity/TruthValue.java
class TruthValue(object):
    def __init__(self, frequency, confidence):
        self.frequency = frequency
        self.confidence = confidence

    @staticmethod
    def abduction(a, b):
        f1 = a.frequency
        f2 = b.frequency
        c1 = a.confidence
        c2 = b.confidence
        w = TruthFunctions.and_(f2, TruthFunctions.and_(c1, c2))
        c = TruthFunctions.convWToC(w)
        return TruthValue(f1, c)

    # see https://github.com/patham9/ANSNA/blob/4315274cc6f82840aa0e2b66aeeabac7c03e6167/src/Inference.c#L71
    @staticmethod
    def intersection(a, b):
        f1 = a.frequency
        f2 = b.frequency
        c1 = a.confidence
        c2 = b.confidence
        f = TruthFunctions.and_(f1, f2)
        c = TruthFunctions.and_(c1, c2)
        return TruthValue(f, c)

    # see https://github.com/patham9/ANSNA/blob/4315274cc6f82840aa0e2b66aeeabac7c03e6167/src/Inference.c#L71
    @staticmethod
    def deduction(a, b):
        f1 = a.frequency
        f2 = b.frequency
        c1 = a.confidence
        c2 = b.confidence
        f = TruthFunctions.and_(f1, f2)
        c = TruthFunctions.and_(TruthFunctions.and_(c1, c2), f)
        return TruthValue(f, c)

    # see https://github.com/patham9/ANSNA/blob/a79eff7678f20e52ddd6f9db756eae9fc29d224c/src/Truth.c#L28
    @staticmethod
    def revision(a, b):
        f1 = a.frequency
        f2 = b.frequency
        c1 = a.confidence
        c2 = b.confidence
        w = w1 + w2
        f = (w1 * f1 + w2 * f2) / w
        c = TruthFunctions.convWToC(w)
        return TruthValue(f, c)

# TODO< put into Parameters class and reference acordingly >
EVIDENTAL_HORIZON = 1.0

class TruthFunctions(object):
    @staticmethod
    def and_(a, b):
        return a*b;

    @staticmethod
    def or_(a, b):
        return 1 - ((1 - a) * (1 - b))

    @staticmethod
    def convWToC(w):
        return w / (w + EVIDENTAL_HORIZON)

    @staticmethod
    def convCToW(c):
        return EVIDENTAL_HORIZON * c / (1 - c)
