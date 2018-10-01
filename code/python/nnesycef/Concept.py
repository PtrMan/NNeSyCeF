import Distributed
from Fifo import Fifo

class Concept(object):
	EVENTBELIEFS_MAX = 512

	# maximal sizes of pre and post-condition beliefs
	PRECONDITION_MAX = 512
	POSTCONDITION_MAX = 512

	def __init__(self, sdr):
		self.eventBeliefs = Fifo(Concept.EVENTBELIEFS_MAX)

		# see https://github.com/patham9/ANSNA/blob/47fadf59f8f8322db64bc8ecd3d7d29721a68be2/src/Concept.h#L30
		self.beliefsPrecondition = Fifo(Concept.PRECONDITION_MAX) # Task's
		self.beliefsPostcondition = Fifo(Concept.POSTCONDITION_MAX) # Task's

		self.sdr = sdr

	# returns a human readable short name of the sdr
	# /return string of a human readable "name" of the SDR
	def retHumanReadableId(self):
		return Distributed.convToHumanReadableName(self.sdr)
