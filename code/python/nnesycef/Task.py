from Stamp import Stamp
import Distributed

class Task(object):
	ENUMTYPE_GOAL = 0
	ENUMTYPE_JUDGEMENT = 1

	def __init__(self, type_, stamp):
		self.type_ = type_
		self.stamp = stamp

		self.truth = None
		self.sdr = None

		self.attention = None

		# it is a event task if it is not eternal
		#self.isEternal = False # commented because not used



		# identifying sdrs are used to check if a task is already resent in the system
		# matching is done and exact
		#     more abstract tasks (like computing something) don't necessarly have an sdr and
		#     truth but they must have an identifyingSdr
		self.identifyingSdr = None

	# tick gives the Task a timeslice to prcess information
	def tick(self):
		pass # doesn't do anything for NARS-tasks


	def process(self, reasoner):
		pass

		## TODO< revise after working cycle of ANSNA >
		"""


		print("[i] PROCESSING TASK")
		print("[i] task stamp = " + str(self.stamp._arr))


		# try to find belief for eventAbduction
		belief = reasoner._conceptMemory.retrieveBeliefAsymBySubjectSdr(self.sdr)
		if belief == None:
			return

		# we can't derive something if the stamps overlap
		if Stamp.checkOverlap(self.stamp, belief.stamp):
			print("[d] stamp overlap    - no derivation done")

			return

		print("[i]    with belief")
		print("[i]    belief stamp = " + str(belief.stamp._arr))



		conclusionTask = None
		
		if self.type_ == Task.ENUMTYPE_GOAL:
			conclusionTask = NarsInference.eventAbduction(belief, self)
			tryAddDerivedTask(conclusionTask, "event abduction")

		"""

	# returns a human readable short name of the sdr
	# /return string of a human readable "name" of the SDR
	def retHumanReadableId(self):
		return Distributed.convToHumanReadableName(self.sdr)