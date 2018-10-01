import random

DISTRIBUTED_BITS = 2048
DISTRIBUTED_RATIO = 0.06

# manhantance distance of distributed vectors
def distanceAbs(a, b):
	d = 0
	for i in range(0, DISTRIBUTED_BITS):
		d += a[i] ^ b[i]

	return d

# similarity as defined by HTM theory (number of overlaping on bits)
def similarity2(a, b):
	BITS = 2048

	cdef int i, d
	d = 0
	i = 0
	while i < BITS:
		d += a[i] & b[i]
		i += 1
	
	return float(d) / (BITS * DISTRIBUTED_RATIO)

cdef float similarity(unsigned char[:] a, unsigned char[:] b):
	BITS = 2048

	cdef int i, d
	d = 0
	i = 0
	while i < BITS:
		d += a[i] & b[i]
		i += 1
	
	return float(d) / (BITS * DISTRIBUTED_RATIO)

# similarity as defined by HTM theory (number of overlaping on bits)
#def similarity(a, b):
#	d = similarity_(a, b)
#
#	return float(d) / (DISTRIBUTED_BITS * DISTRIBUTED_RATIO)

def genNull():
	return bytearray(DISTRIBUTED_BITS)

def union(a, b):
	l = len(a)

	res = bytearray(DISTRIBUTED_BITS)
	for i in range(0, l):
		res[i] = a[i] | b[i]
	return res

def xor_(a, b):
	l = len(a)

	res = bytearray(DISTRIBUTED_BITS)
	for i in range(0, l):
		res[i] = a[i] ^ b[i]
	return res


def minus(a, b):
	l = len(a)

	res = bytearray(DISTRIBUTED_BITS)
	for i in range(0, l):
		res[i] = a[i] and (not b[i])
	return res

def genRandom():
	res = bytearray(DISTRIBUTED_BITS)

	for i in range(0, int(DISTRIBUTED_BITS * DISTRIBUTED_RATIO)):
		idx = random.randint(0, DISTRIBUTED_BITS-1)
		res[idx] = True

	return res

def retIndicesOfBits(v):
	res = []

	for i in range(0, len(v)):
		if v[i]:
			res.append(i)

	return res


def checkSame(a, b):
	# TODO< check if a and b are arrays and contain only booleans >

	return a == b


def distance(a, b):
	return float(distanceAbs(a, b)) / DISTRIBUTED_BITS


permutation = []
invPermutation = []

# setup permutation
permutation2 = []
for i in range(0, DISTRIBUTED_BITS):
	permutation2.append(i)

while len(permutation2) > 0:
	idx = random.randint(0, len(permutation2)-1)

	invPermutation.append(idx)
	permutation.append(permutation2[idx])

	del permutation2[idx]
del permutation2

def permutate(v):
	result = bytearray(DISTRIBUTED_BITS)

	for idxV in range(0, DISTRIBUTED_BITS-1):
		idx = permutation[idxV]
		result[idx] = v[idxV]

	return result

def invPermutate(v):
	result = bytearray(DISTRIBUTED_BITS)

	for idxV in range(0, DISTRIBUTED_BITS-1):
		idx = invPermutation[idxV]
		result[idx] = v[idxV]

	return result


# TODO< unittest and make sure that the inverse valid for the permutation >


# produce a SDR by a integer
def genSdrByIntId(id_):
	id2 = id_

	res = bytearray(DISTRIBUTED_BITS)

	idx2 = 1503495

	for i in range(0, int(DISTRIBUTED_BITS * DISTRIBUTED_RATIO)):
		idx = idx2 % DISTRIBUTED_BITS
		res[idx] = True

		idx2 = id2 * 7 + idx2 * 5 ^ (idx2 + 5)

	return res


# converts any SDR to a short human reable descriptive name (with the same length)
def convToHumanReadableName(sdr):
	# we just count how often a bit occured for a slice which maps to one char in the final string

	nameAsIntegers = [0] * int(len(sdr) / 256)

	idx=0
	for iBit in sdr:
		if iBit:
			nameAsIntegers[int(idx / 256)] += 1

		idx+=1

	# convert to string
	nameAsString = ["a"] * int(len(sdr) / 256)

	idx=0
	for iInteger in nameAsIntegers:
		charCandidates = "abcdefghijklmnopqrstuvwxyz0123456789"

		nameAsString[idx] = charCandidates[iInteger % len(charCandidates)]

		idx+=1

	return "".join(nameAsString)