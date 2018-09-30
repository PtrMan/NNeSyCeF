from TruthValue import *
from Task import *
from Stamp import *
from Attention import AttentionValue

DEBUG = False

# see https://github.com/patham9/ANSNA/blob/e4f8c9879013754467ff149172b9eb10aa988bbf/src/Inference.c#L94
#    {Event task a!, Precondition belief <a =/> b>.}
# |-
#    Derived event task b!
#
# /param compoundTask is some object with sdr,truth,stamp
# /param componentTask is some object with sdr,truth,stamp
def eventAbduction(compoundTask, componentTask):
    # TODO< assert on types of task >

    resultTaskStamp = Stamp.merge(compoundTask.stamp, componentTask.stamp)
    resultTask = Task(Task.ENUMTYPE_GOAL, resultTaskStamp)
    resultTask.sdr = sdrDetachmentForward(compoundTask.sdr, componentTask.sdr)
    resultTask.identifyingSdr = resultTask.sdr # we do this because they are the same for NARS tasks!
    resultTask.truth = TruthValue.abduction(compoundTask.truth, componentTask.truth)
    resultTask.attention = AttentionValue()

    if DEBUG:
        ## TODO< used debug cookie of name of SDR if possible >
        print("[t] event ABDUCTION   {{{}., {}!}} |- {}!".format(compoundTask.retHumanReadableId(), componentTask.retHumanReadableId(), resultTask.sdr.retHumanReadableId()))

    return resultTask


# see https://github.com/patham9/ANSNA/blob/e4f8c9879013754467ff149172b9eb10aa988bbf/src/Inference.c#L94
#    {Event task a., Postcondition belief <a =/> b>.}
# |-
#    Derived event task b.
#
# /param compound is some object with sdr,truth,stamp
# /param component is some object with sdr,truth,stamp
def eventDeduction(compound, componentTask):
    # TODO< assert on types of task >

    resultTaskStamp = Stamp.merge(compound.stamp, componentTask.stamp)
    resultTask = Task(Task.ENUMTYPE_JUDGEMENT, resultTaskStamp)
    resultTask.sdr = sdrDetachmentForward(compound.sdr, componentTask.sdr)
    resultTask.identifyingSdr = resultTask.sdr # we do this because they are the same for NARS tasks!
    resultTask.truth = TruthValue.deduction(compound.truth, componentTask.truth)
    resultTask.attention = AttentionValue()

    if DEBUG:
        ## TODO< used debug cookie of name of SDR if possible >
        print("[t] event DEDUCTION   {{{}., {}.}} |- {}.".format(compound.retHumanReadableId(), componentTask.retHumanReadableId(), resultTask.retHumanReadableId()))

    return resultTask

#    {Event task conditional., (Event) belief predicted.}
# |-
#    Derived event task (&/,conditional,predicted).
#
# /param conditional event - which happens before predicted
# /param predicted happens after conditional
def eventIntersection(conditional, predicted):
    resultTaskStamp = Stamp.merge(conditional.stamp, predicted.stamp)
    resultTask = Task(Task.ENUMTYPE_JUDGEMENT, resultTaskStamp)
    resultTask.sdr = sdrSequence([conditional.sdr, predicted.sdr])
    resultTask.identifyingSdr = resultTask.sdr # we do this because they are the same for NARS tasks!
    resultTask.truth = TruthValue.intersection(conditional.truth, predicted.truth)
    resultTask.attention = AttentionValue()

    if DEBUG:
        ## TODO< used debug cookie of name of SDR if possible >
        print("[t] event INTERSECTION   {{{}., {}.}} |- {}.".format(conditional.retHumanReadableId(), predicted.retHumanReadableId(), resultTask.retHumanReadableId()))


    return resultTask


#    {Precondition or Postcondition belief a., Precondition or Postcondition belief a.}
# |- 
#    Precondition or Postcondition belief a.
#
# /param a belief NARS-task
# /param b belief NARS-task
def eventRevision(a, b):
    assert isinstance(a, Task)
    assert isinstance(b, Task)

    ## revision doesn't make any sense if the SDR's don't match
    assert a.sdr == b.sdr

    resultTaskStamp = Stamp.merge(a.stamp, b.stamp)
    resultTask = Task(Task.ENUMTYPE_JUDGEMENT, resultTaskStamp)

    resultTask.sdr = a.sdr
    resultTask.identifyingSdr = resultTask.sdr # we do this because they are the same for NARS tasks!
    resultTask.truth = TruthValue.revision(a.truth, b.truth)
    # TODO< how to calculate the attention value? >
    resultTask.attention = AttentionValue()

    if DEBUG:
        ## TODO< used debug cookie of name of SDR if possible >
        print("[t] event REVISION   {{{}., {}.}} |- {}.".format(a.retHumanReadableId(), b.retHumanReadableId(), resultTask.retHumanReadableId()))


    return resultTask





from Distributed import *

concurrent = genRandom()
sequence = genRandom()
implication = genRandom()

# helper
# see http://aleph.sagemath.org/?z=eJy1Vttu4zYQfTfgfxg4QCoCzG1RFIVR76Jo-1CgXRR13oyFQVF0zMK6LEkl3W367x1eRFGxpXVaNA-xNJw5Z3g4M6Ism1oZUKwq6nI-m88udlJpQ6FkDRihSg2mhgdRCcWMKGD94-9faUDv8ISL3mtXK2hqIyoj2QHEQZT4CI2SaKge5jPrtUXQ1V9_hxctPwtYwZvbr7-dzxpV20RkXW3v0Hh7fXs3nxVih9TmHr2zipWCLOczwL-HQ50jSQfpjXIH1gdkFReCu_1TwrSqX9lY1w9-eWhD8s2dBfOSXPufjHw3yFActIBbt-ncMqLXg8jitkiAHmFFldc1ppozLbkVEurG6ovYXkgtjNPYtM1BgKh4XTgRrSDo_qusWp0xmneCBJ5Nyf7MbinbyA9XOf4jDkyOJBjA1sKcgrIgzxZkEkOLj-0Wcy9Rtt_wpzV-F4lXUHAbSiIjSdRWVo8YGd-v8V0oLaxTSO_eSnAiwZh5Es24Jc9yQpzG7-snMHsB77__xRcpIvxQV7xVytbmKtbWorcuAjOPlnHuPoqmQiLCGlPCUxMpR2frGHR4H8fvImiqAziCn8vmILnTOuVIzB2N7E3jTEncCbKLtQUROBIM3yelilDOdL9XQu_rQ-Ea95u-TO1i1jBl8IXu2sMBf8kSLjyQXaBgzcBZBXv2iBQtLpS1EnAHuTSaugNEGbTbqtTwtBdoUs7ufaDeuTeLBwxD7brw29Qt50JrW2NtmW1CLljVlyGd6QInNy_Cpnw944UbebBAlbitMrfXJSxoSOW6yggMzqDL8S0M1exbAEnuPzUjleJVtksx4KePLTu4ivhCRDfIvSWnrMfA4jZMVnrd5uYcJNQlGo46khGX3nzG0mpltkbz1JJbC7beH1tm7S-a0H4lnLYWZUGBRYN1XgCWa7Rkl8_U5bVCx4CYxGMhab8xWz2d87sFPb31AEASwlch5McIMcEhUD6CEOKdCiiv12cwQgJyQnDTK-BDJgS4mRbAx0_tfxIgPwKI2SEOixt5Bz4-VnwcgdPxfaG4s24rV3ZHaMnAjng42ryYLwflkZ5Ydau3ECir1P-IKJ2nnoCM4Jyx9QiAk9iqeYXXBryJFcIwvne3rKwQReu2TEAb0SwhcGCPwPOVbQvb1X3EFsfYE1MFlhXe_9qqoO6hwqWuyeVuTLouhhxfr-LdJD6cIKA9FjnJFXc-wTS4QnRj5jz6Dp6cZk_P7v9JIGEgxELGUsu3SnB7CVydPiw_AxgZDr507uVvvCEAJf1y6fqF5q4guo-O_Qp-Qi4lH0Wx9BXYfz1w3AQccmaKvq3-ZYa-oyn7jxny9IvCF2Fg8uHE5P0E8A9f3BYCUE56Ac7QIbi8Ro5TqnDaaUMot-LE4XeWRsNcYGqSdq2xHB1GMEAj5-X5uiMdKmHvoPPZPzTNvkM=&lang=sage
def sdrCheckTermType(a, b):
    # patricks code did:   #return SDRMatch(a,b)
    # we just check symetric similarity
    return similarity(a, b)

# see http://aleph.sagemath.org/?z=eJy1Vttu4zYQfTfgfxg4QCoCzG1RFIVR76Jo-1CgXRR13oyFQVF0zMK6LEkl3W367x1eRFGxpXVaNA-xNJw5Z3g4M6Ism1oZUKwq6nI-m88udlJpQ6FkDRihSg2mhgdRCcWMKGD94-9faUDv8ISL3mtXK2hqIyoj2QHEQZT4CI2SaKge5jPrtUXQ1V9_hxctPwtYwZvbr7-dzxpV20RkXW3v0Hh7fXs3nxVih9TmHr2zipWCLOczwL-HQ50jSQfpjXIH1gdkFReCu_1TwrSqX9lY1w9-eWhD8s2dBfOSXPufjHw3yFActIBbt-ncMqLXg8jitkiAHmFFldc1ppozLbkVEurG6ovYXkgtjNPYtM1BgKh4XTgRrSDo_qusWp0xmneCBJ5Nyf7MbinbyA9XOf4jDkyOJBjA1sKcgrIgzxZkEkOLj-0Wcy9Rtt_wpzV-F4lXUHAbSiIjSdRWVo8YGd-v8V0oLaxTSO_eSnAiwZh5Es24Jc9yQpzG7-snMHsB77__xRcpIvxQV7xVytbmKtbWorcuAjOPlnHuPoqmQiLCGlPCUxMpR2frGHR4H8fvImiqAziCn8vmILnTOuVIzB2N7E3jTEncCbKLtQUROBIM3yelilDOdL9XQu_rQ-Ea95u-TO1i1jBl8IXu2sMBf8kSLjyQXaBgzcBZBXv2iBQtLpS1EnAHuTSaugNEGbTbqtTwtBdoUs7ufaDeuTeLBwxD7brw29Qt50JrW2NtmW1CLljVlyGd6QInNy_Cpnw944UbebBAlbitMrfXJSxoSOW6yggMzqDL8S0M1exbAEnuPzUjleJVtksx4KePLTu4ivhCRDfIvSWnrMfA4jZMVnrd5uYcJNQlGo46khGX3nzG0mpltkbz1JJbC7beH1tm7S-a0H4lnLYWZUGBRYN1XgCWa7Rkl8_U5bVCx4CYxGMhab8xWz2d87sFPb31AEASwlch5McIMcEhUD6CEOKdCiiv12cwQgJyQnDTK-BDJgS4mRbAx0_tfxIgPwKI2SEOixt5Bz4-VnwcgdPxfaG4s24rV3ZHaMnAjng42ryYLwflkZ5Ydau3ECir1P-IKJ2nnoCM4Jyx9QiAk9iqeYXXBryJFcIwvne3rKwQReu2TEAb0SwhcGCPwPOVbQvb1X3EFsfYE1MFlhXe_9qqoO6hwqWuyeVuTLouhhxfr-LdJD6cIKA9FjnJFXc-wTS4QnRj5jz6Dp6cZk_P7v9JIGEgxELGUsu3SnB7CVydPiw_AxgZDr507uVvvCEAJf1y6fqF5q4guo-O_Qp-Qi4lH0Wx9BXYfz1w3AQccmaKvq3-ZYa-oyn7jxny9IvCF2Fg8uHE5P0E8A9f3BYCUE56Ac7QIbi8Ro5TqnDaaUMot-LE4XeWRsNcYGqSdq2xHB1GMEAj5-X5uiMdKmHvoPPZPzTNvkM=&lang=sage
def sdrCreateTuple(a, b):
    return union(a, permutate(b))


# see http://aleph.sagemath.org/?z=eJy1Vttu4zYQfTfgfxg4QCoCzG1RFIVR76Jo-1CgXRR13oyFQVF0zMK6LEkl3W367x1eRFGxpXVaNA-xNJw5Z3g4M6Ism1oZUKwq6nI-m88udlJpQ6FkDRihSg2mhgdRCcWMKGD94-9faUDv8ISL3mtXK2hqIyoj2QHEQZT4CI2SaKge5jPrtUXQ1V9_hxctPwtYwZvbr7-dzxpV20RkXW3v0Hh7fXs3nxVih9TmHr2zipWCLOczwL-HQ50jSQfpjXIH1gdkFReCu_1TwrSqX9lY1w9-eWhD8s2dBfOSXPufjHw3yFActIBbt-ncMqLXg8jitkiAHmFFldc1ppozLbkVEurG6ovYXkgtjNPYtM1BgKh4XTgRrSDo_qusWp0xmneCBJ5Nyf7MbinbyA9XOf4jDkyOJBjA1sKcgrIgzxZkEkOLj-0Wcy9Rtt_wpzV-F4lXUHAbSiIjSdRWVo8YGd-v8V0oLaxTSO_eSnAiwZh5Es24Jc9yQpzG7-snMHsB77__xRcpIvxQV7xVytbmKtbWorcuAjOPlnHuPoqmQiLCGlPCUxMpR2frGHR4H8fvImiqAziCn8vmILnTOuVIzB2N7E3jTEncCbKLtQUROBIM3yelilDOdL9XQu_rQ-Ea95u-TO1i1jBl8IXu2sMBf8kSLjyQXaBgzcBZBXv2iBQtLpS1EnAHuTSaugNEGbTbqtTwtBdoUs7ufaDeuTeLBwxD7brw29Qt50JrW2NtmW1CLljVlyGd6QInNy_Cpnw944UbebBAlbitMrfXJSxoSOW6yggMzqDL8S0M1exbAEnuPzUjleJVtksx4KePLTu4ivhCRDfIvSWnrMfA4jZMVnrd5uYcJNQlGo46khGX3nzG0mpltkbz1JJbC7beH1tm7S-a0H4lnLYWZUGBRYN1XgCWa7Rkl8_U5bVCx4CYxGMhab8xWz2d87sFPb31AEASwlch5McIMcEhUD6CEOKdCiiv12cwQgJyQnDTK-BDJgS4mRbAx0_tfxIgPwKI2SEOixt5Bz4-VnwcgdPxfaG4s24rV3ZHaMnAjng42ryYLwflkZ5Ydau3ECir1P-IKJ2nnoCM4Jyx9QiAk9iqeYXXBryJFcIwvne3rKwQReu2TEAb0SwhcGCPwPOVbQvb1X3EFsfYE1MFlhXe_9qqoO6hwqWuyeVuTLouhhxfr-LdJD6cIKA9FjnJFXc-wTS4QnRj5jz6Dp6cZk_P7v9JIGEgxELGUsu3SnB7CVydPiw_AxgZDr507uVvvCEAJf1y6fqF5q4guo-O_Qp-Qi4lH0Wx9BXYfz1w3AQccmaKvq3-ZYa-oyn7jxny9IvCF2Fg8uHE5P0E8A9f3BYCUE56Ac7QIbi8Ro5TqnDaaUMot-LE4XeWRsNcYGqSdq2xHB1GMEAj5-X5uiMdKmHvoPPZPzTNvkM=&lang=sage
#term-based detachment (deduction) step: a ==> b, a |- b
def sdrDetachmentForward(compound, component):
    if sdrCheckTermType(concurrent, compound):
        return minus(minus(compound,component), concurrent)
    elif sdrCheckTermType(sequence, compound):
        return invPermutate(minus(minus(compound,component), sequence))
    elif sdrCheckTermType(implication,compound):
        return invPermutate(minus(minus(compound,component), implication))

def sdrSequence(components):
    assert len(components) == 2 # is just implemented for two, TODO<>

    return union(sequence, sdrCreateTuple(components[0], components[1]))