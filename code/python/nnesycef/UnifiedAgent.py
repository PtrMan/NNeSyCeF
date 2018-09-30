# (Draft) of a unified agent similar to NARS

from random import seed, randint

from Distributed import *
from SequenceMemory import *

# initialize random number generator
seed()

# TODO< sort concepts >

# TODO< add time >
# TODO< add time of events/tasks >

# TODO< feedback of priority after derivation in the attention system >

# TODO< fix permutation issue patrick for sequences was describing with 
#       Perm1(a) xor (Perm2(b) xor c)
# >

# TODO< decorate task with temporal information for for ex: <(&/, a, +5) =/> b>
#       must be a list of tuples which describe the coordinates in the (demangled) SDR of the time interval
#     ----
#       a better idea may be to use a sdr representation of ordered lists
#     >

# TODO< overhaul event revision    https://github.com/patham9/ANSNA/wiki/Event-Revision
#     >

# TODO< ignore overlapping sdr's if the number of bits is less than a threshold ? >

# TODO< use concepts for pre and post condition decision making >

# TODO< fix inference bugs (revision seems to be taking place when it shouldn't for example) >

# TODO< check if stamps work correctly - we need unittests and a message >

# TODO< unittest permutations >

# TODO< implement all rules 
#       #term-based detachment (deduction) step: a ==> b, b |- a
#

# TODO< test   revision >

# TODO LONGTERM< tinker with attention subsystem and try various (automated) experiments >




"""
>>> Temporal induction between a (all events in the FIFO) and b happens, 

(me)
this doesn't make any sense to me
it just spams the system with derivations
it would be fine if the derivations would be filtered like in event buffer
by relative frequency (where frequency is a float which is incremented by 1.0 when seen and slowly decays)
hm this makes sense - it is like my compression idea (for events) but works in realtime (without a delay because of the fifo for compressed candidates)
I may implement it this way

"""



def convStrToId(str_):
	res = 0
	for i in str_:
		res += 41 * ord(i)
	return res

def genSdrByStringName(str_):
	id_ = convStrToId(str_)
	return genSdrByIntId(id_)


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
			amountOfOverlap = distance(iObject.sdr, sdr)

			# TODO< treat as no overlap if the number of overlapping bits is to small >
			## we need to do this because it lowers the probability that the reasoning process
			## derives non-sense just because one or to many bits were identically by chance

			if amountOfOverlap > bestObjectOverlap:
				bestObjectOverlap = amountOfOverlap
				bestObject = iObject


		if bestObjectOverlap < self.retrieveBeliefMinimalOverlap:
			return None

		return bestObject





import NarsInference
from TruthValue import TruthValue
from Task import Task
from Stamp import Stamp
from Attention import AttentionSystem, AttentionValue
from Concept import Concept

class Reasoner(object):
	def __init__(self):
		self._stampCounter = 0 # counter for the creation of stamps

		# list of all concepts
		# these are all beliefs
		self._conceptMemory = NarsMemory()


		self._lastCycleEventTasks = [] # events in last cycle
		self._thisCycleEventTasks = [] # events in this cycle


		# attention (sub)system
		## handes all the details of the attention mechanism(s)
		self._temporalAttentionSystem = AttentionSystem(AttentionSystem.TYPE_TEMPORAL)

		# maximal number of (top) Concepts to which the tasks are sent for temporal processing
		self._temporalInferenceAttentionMaxCandidates = 10

	# /param origin string, indicates the origin of the derivation and it is only used for debugging
	def _tryAddDerivedTask(self, derivedTask, origin):
		# tests if a task in question is unique and new to the system
		# used to get rid of adding the same task to the system multiple times
		def checkIfTaskIsUnique(checkedTask):
			return True

		print("[d] derived " + origin)
		print(str(derivedTask.truth.frequency) + " " + str(derivedTask.truth.confidence))

		if checkIfTaskIsUnique(derivedTask):
			print("[d]: derived task was added")
		else:
			print("[d]: derived task was not added because it was not unique")

		if checkIfTaskIsUnique(derivedTask):
			# we need to conceptualize it to make the system aware of the new knowledge
			self._conceptualize(derivedTask)

			# ATTENTION
			## we just need to add the task to all tasks know to the attention system
			self._temporalAttentionSystem.put(derivedTask)



	def cycle(self):

		for iThisCycleEventTask in self._thisCycleEventTasks:
			# TODO< check if we need to conceptualize in ANSNA and this codebase >
			self._conceptualize(iThisCycleEventTask)

			## we need to inform the attention system about the new event
			## ASK< this may not be compatible with parallel event handling because it
			##      may mess up the (temporal) attention
			##    >

			self._activateConceptByTask(iThisCycleEventTask)




		# ATTENTION
		## TODO< we don't need to step temporal tasks because they are never executable! - we just need it to do for declarative tasks >
		## apply attention to tasks
		for iTask in self._temporalAttentionSystem._level0._queue:
			self._temporalAttentionSystem.stepForTask(iTask.attention)

		## apply attention to beliefs
		## TODO< do we need to go over the concepts? - I think so >
		
		""" commented because old code
		for iTask in self._conceptMemory.objects:
			self._temporalAttentionSystem.stepForTask(iTask.attention)
		"""


		# TICK
		## tick tasks for non-NARS inference/reasoning
		for iTask in self._temporalAttentionSystem._level0._queue:
			iTask.tick()



		# INFERENCE and ATTENTION
		## we call into attention because it will select the tasks for processing
		#
		## commented because we can only do the attention cycle for DECLARATIVE tasks
		#self._temporalAttentionSystem.attentionCycle(self)




		# event cycle logic
		## we just clean it up
		self._thisCycleEventTasks = []


	# activates a concept by sdr
	def _activateConceptByTask(self, task):
		# retrives/fetches a concept by it's sdr
		def fetchConceptBySdr(sdr):
			fetchedConcept = reasoner._conceptMemory.retrieveBySdr(task.sdr)

			## make sure that it is a concept
			assert isinstance(fetchedConcept, Concept), "retrieved Object must be a Concept!"

			return fetchedConcept

		# sends a event to a concept for temporal inference
		# see https://github.com/patham9/ANSNA/wiki/Working-cycle
		# /param receiverConcept the "root" concept which receives the task
		# /param occuredEvent the occured event which happened now
		# /param conceptForOccuredEvent the concept for the occured event which happened now
		## TODO< annotate types with dummy annotation >
		## receiverConcept is Concept
		## occuredEvent is Task
		def processEventForTemporalInference(receiverConcept, occuredEvent):
			# called when a task was derived
			# TODO< rename to derive and add a argument to the function which s used for derivation > 

			def derived(derivedTask, origin):
				## because we need to add it to the system
				self._tryAddDerivedTask(derivedTask, origin)

				## because we need to boost the attention of the assiciated concepts
				self._temporalAttentionSystem.boostAttentionByDerivedTask(derivedTask)


			assert isinstance(receiverConcept, Concept)
			assert isinstance(occuredEvent, Task)

			## fetch concept for occuredEvent
			conceptForOccuredEvent = fetchConceptBySdr(occuredEvent.sdr)

			print("[d] processEventForTemporalInference(), receiverConcept=" + receiverConcept.retHumanReadableId() + " occuredEvent=" + occuredEvent.retHumanReadableId() + " conceptForOccuredEvent=" + conceptForOccuredEvent.retHumanReadableId())

			print("[d] eventBeliefs#={}".format(len(receiverConcept.eventBeliefs)))

			print(receiverConcept.eventBeliefs._arr)

			for iEventBelief in receiverConcept.eventBeliefs:
				print(iEventBelief)
				print("[d]   eventBelief.sdr=" + iEventBelief.retHumanReadableId())

			# temporal induction between all events in FIFO and occuredEvent
			for iEventBelief in receiverConcept.eventBeliefs:
				# checks if REVISION has to be done between Belief(Task)
				def checkIsRevisable(a, b):
					## for now we just revise if the SDR's are exactly the same
					## TODO< should this policy be another one instead? >
					sdrsOverlap = a.sdr == b.sdr

					# TODO< take time distance into account 
					#     patham9: FIFO revision should happen up to a certain max. amount of time distance between the events.
					# >
					return sdrsOverlap

				print("[d2] considering sdr={} and sdr={}".format(iEventBelief.retHumanReadableId(), occuredEvent.retHumanReadableId()))

				if iEventBelief.sdr == occuredEvent.sdr:
					print("[d2] ... rejected because SDR's are the same")

					# because it can't do inference with itself
					continue

				print("[d2] ... applying event deduction")

				derivedTask = NarsInference.eventDeduction(iEventBelief, occuredEvent)
				
				derived(derivedTask, "event deduction")

				# add derived task to pre and post conditions of premises
				# this is required for DECISION MAKING
				# (we have here the only oportunity to do this)
				#
				## TODO< refactor iteration over fifo and adding into function because it is done twice >

				# we check for oportunities to apply REVISION with existing ones with the same content
				wasRevised = False
				for iBeliefIdx in range(0, len(conceptForOccuredEvent.beliefsPrecondition)):
					iBelief = conceptForOccuredEvent.beliefsPrecondition[iBeliefIdx]

					if checkIsRevisable(iEventBelief, iBelief):
						revisedBelief = NarsInference.eventRevision(iEventBelief, iBelief)

						## ASK< is this fine here >
						derived(revisedBelief, "event revision")

						## replace with revised one
						conceptForOccuredEvent.beliefsPrecondition[iBeliefIdx] = revisedBelief

						wasRevised = True

						## we don't need to check any others for revision
						break

				if not wasRevised:
					conceptForOccuredEvent.beliefsPrecondition.append(iEventBelief)
				



				# we check for oportunities to apply REVISION with existing ones with the same content
				wasRevised = False
				for iBeliefIdx in range(0, len(receiverConcept.beliefsPostcondition)):
					iBelief = receiverConcept.beliefsPostcondition[iBeliefIdx]

					if checkIsRevisable(occuredEvent, iBelief):
						revisedBelief = NarsInference.eventRevision(occuredEvent, iBelief)

						## ASK< is this fine here >
						derived(revisedBelief, "event revision")

						## replace with revised one
						receiverConcept.beliefsPostcondition[iBeliefIdx] = revisedBelief

						wasRevised = True

						## we don't need to check any others for revision
						break

				if not wasRevised:
					receiverConcept.beliefsPostcondition.append(occuredEvent)

				# TODO< do we need to active some concepts here - I think so >

			# derive event intersections
			for iEventBelief in receiverConcept.eventBeliefs:
				print("[d2] considering sdr={} and sdr={}".format(iEventBelief.retHumanReadableId(), occuredEvent.retHumanReadableId()))

				if iEventBelief.sdr == occuredEvent.sdr:
					print("[d2] ... rejected because SDR's are the same")

					# because it can't do inference with itself
					continue

				print("[d2] ... applying event intersection")

				derivedTask = NarsInference.eventIntersection(iEventBelief, occuredEvent)
				
				derived(derivedTask, "event intersection")

				# TODO< do we need to active some concepts here - I thnk so >

			


		# implemented (hopefully) EXACTLY like ANSNA, see https://github.com/patham9/ANSNA/wiki/Working-cycle

		# TODO< print small humanreadable hash of sdr for debugging purposes >
		print("[d] activate concept by task sdr = %s" % task.retHumanReadableId())


		# find concept by sdr
		selectedConcept = fetchConceptBySdr(task.sdr)

		# put it into the event FIFO of the concept
		# like in ANSNA
		selectedConcept.eventBeliefs.append(task)


		## TODO< mitit to sane amount based on configuration or static variable >
		if len(selectedConcept.eventBeliefs) > 100000000: # limit length of FIFO
			selectedConcept.eventBeliefs = selectedConcept.eventBeliefs[1:]


		# ANSNA takes the tasks with the highest priority from the (temporal) attention-buffer
		#
		# TonyLo: tasks come from attention buffer and
		#         concepts come from a priority queue also,
		#         which is really another attention buffer for concepts
		# 
		## send to beliefs of the associated concepts of the tasks
		for iConcept in self._retHighestPriorityConcepts():
			## we must ignore if if it is the same task because a event can't be sent to it's own concept!
			if iConcept.sdr == task.sdr:
				continue

			processEventForTemporalInference(iConcept, task)


	# returns the top n (where n is a system parameter) concepts
	# as required by ANSNA control theory
	def _retHighestPriorityConcepts(self):
		# TODO< concepts need to be ordered by priority in the concept memory >

		n = self._temporalInferenceAttentionMaxCandidates
		return self._conceptMemory.objects[:n]







	# generates a new concept and adds it to memory
	# /param belief belief task which has to be conceptualized
	def _conceptualize(self, belief):
		print("[d] conceptualize belief task sdr = %s" % belief.retHumanReadableId())

		createdConcept = Concept(belief.sdr)

		## we need to add it to memory
		self._conceptMemory.appendObject(createdConcept)

	# generates and returns a new stamp
	def _genStamp(self):
		stamp = Stamp([self._stampCounter])
		self._stampCounter += 1
		return stamp


	# retrives best fitting concept based on SDR distance
	def _retrieveBestConceptBySdr(self, sdr):
		return self._memory.retrieveBeliefAsymBySubjectSdr(sdr)




reasoner = Reasoner()











import time

cycleCounter = 0
while True:
	t = time.process_time()




	# TODO< receie events from environment >
	# we just receive random fake events
	stamp = reasoner._genStamp()
	eventTask = Task(Task.ENUMTYPE_JUDGEMENT, stamp)
	eventTask.sdr = genRandom()
	eventTask.truth = TruthValue(0.5, 0.5)
	eventTask.attention = AttentionValue()

	reasoner._thisCycleEventTasks.append(eventTask)


	print("")
	print("")
	print("")
	print("")
	print("")
	print("[i] cycle")
	reasoner.cycle()





	elapsed_time = time.process_time() - t

	if (cycleCounter % 1) == 0:
		if False:
			print(elapsed_time)

	cycleCounter += 1


	# we just let the system run a limited amount of time for testing
	if cycleCounter > 5:
		exit()