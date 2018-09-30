import Distributed
from Fifo import Fifo

class Concept(object):
	# maximal sizes of pre and post-condition beliefs
	PRECONDITION_MAX = 512
	POSTCONDITION_MAX = 512

	def __init__(self, sdr):
		self.eventBeliefs = Fifo(512)

		# see https://github.com/patham9/ANSNA/blob/47fadf59f8f8322db64bc8ecd3d7d29721a68be2/src/Concept.h#L30
		self.beliefsPrecondition = Fifo(512) # Task's
		self.beliefsPostcondition = Fifo(512) # Task's

		self.sdr = sdr

	# returns a human readable short name of the sdr
	# /return string of a human readable "name" of the SDR
	def retHumanReadableId(self):
		return Distributed.convToHumanReadableName(self.sdr)
