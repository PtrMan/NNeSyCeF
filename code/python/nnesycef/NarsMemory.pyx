import Distributed

# TODO< build acceleration structures for the symetric and asymetric acceleration >
# memory which stores objects and retrieves them based on distance
class NarsMemory(object):
	def __init__(self):
		self.objects = []

		self.retrieveBeliefMinimalOverlap = 0.0 # 0.5

	def appendObject(self, object_):
		self.objects.append(object_)

	# /param sdr the searched sdr by overlap
	def retrieveBySdr(self, sdr):
		# TODO TODO TODO TODO
		# TODO< retieve from global storage >

		# we retrieve from global storage by searching for the best fit with the minimal overlap

		# TODO< search assymetric as done in ANSNA >

		# TODO< we need a class with the 2 bit tuple acceleration for symetric retrival too >

		if len(self.objects) == 0:
			return None

		bestObject = self.objects[0]
		bestObjectOverlap = 0.0 # degree of overlap

		for iObject in self.objects[1:]:
			amountOfOverlap = Distributed.distance(iObject.sdr, sdr)

			# TODO< treat as no overlap if the number of overlapping bits is to small >
			## we need to do this because it lowers the probability that the reasoning process
			## derives non-sense just because one or to many bits were identically by chance

			if amountOfOverlap > bestObjectOverlap:
				bestObjectOverlap = amountOfOverlap
				bestObject = iObject


		if bestObjectOverlap < self.retrieveBeliefMinimalOverlap:
			return None

		return bestObject