from random import seed, randint
import NarsInference
from NarsMemory import NarsMemory
from TruthValue import TruthValue
from Task import Task
from Stamp import Stamp
from Attention import AttentionSystem, AttentionValue
from Concept import Concept
import Distributed
import Debug

# checks if REVISION has to be done between Belief(Task)
cdef _checkIsRevisable(a, b):
	## the SDR's must be the same
	sdrsEqual = a.sdr == b.sdr
	if not sdrsEqual:
		return False

	# TODO< check evidence overlap
	#       see https://github.com/patham9/ANSNA/wiki/Event-Revision
	#     >

	# TODO< take time distance into account 
	#     patham9: FIFO revision should happen up to a certain max. amount of time distance between the events.
	#       see https://github.com/patham9/ANSNA/wiki/Event-Revision
	# >
	return True


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

		Debug.msg("d", 0, "derived {} with f={},c={}".format(origin, derivedTask.truth.frequency, derivedTask.truth.confidence))

		if checkIfTaskIsUnique(derivedTask):
			Debug.msg("d", 0, "derived task was added")
		else:
			Debug.msg("d", 0, "derived task was not added because it was not unique")

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


	# retrives/fetches a concept by it's sdr
	def _fetchConceptBySdr(self, task, sdr):
		fetchedConcept = self._conceptMemory.retrieveBySdr(task.sdr)

		## make sure that it is a concept
		assert isinstance(fetchedConcept, Concept), "retrieved Object must be a Concept!"

		return fetchedConcept



	# activates a concept by sdr
	def _activateConceptByTask(self, task):
		

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
			conceptForOccuredEvent = self._fetchConceptBySdr(task, occuredEvent.sdr)

			if False:
				Debug.msg("d", 0, "processEventForTemporalInference(), receiverConcept=" + receiverConcept.retHumanReadableId() + " occuredEvent=" + occuredEvent.retHumanReadableId() + " conceptForOccuredEvent=" + conceptForOccuredEvent.retHumanReadableId())

			if False:
				Debug.msg("d", 0, "eventBeliefs#={}".format(len(receiverConcept.eventBeliefs)))

			for iEventBelief in receiverConcept.eventBeliefs:
				if False:
					Debug.msg("d", 0, "  eventBelief.sdr=" + iEventBelief.retHumanReadableId())

			# temporal induction between all events in FIFO and occuredEvent
			for iEventBelief in receiverConcept.eventBeliefs:
				if False:
					Debug.msg("d", 1, "considering sdr={} and sdr={}".format(iEventBelief.retHumanReadableId(), occuredEvent.retHumanReadableId()))

				if iEventBelief.sdr == occuredEvent.sdr:
					if False:
						Debug.msg("d", 1, " ... rejected because SDR's are the same")

					# because it can't do inference with itself
					continue

				if False:
					Debug.msg("d", 1, " ... applying event deduction")

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

					if _checkIsRevisable(iEventBelief, iBelief):
						revisedBelief = NarsInference.eventRevision(iEventBelief, iBelief)

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

					if _checkIsRevisable(occuredEvent, iBelief):
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
				if False:
					Debug.msg("d", 1, "considering sdr={} and sdr={}".format(iEventBelief.retHumanReadableId(), occuredEvent.retHumanReadableId()))

				if iEventBelief.sdr == occuredEvent.sdr:
					if False:
						Debug.msg("d", 1, "... rejected because SDR's are the same")

					# because it can't do inference with itself
					continue

				if False:
					Debug.msg("d", 1, "... applying event intersection")

				derivedTask = NarsInference.eventIntersection(iEventBelief, occuredEvent)
				
				derived(derivedTask, "event intersection")

				# TODO< do we need to active some concepts here - I thnk so >

			


		# implemented (hopefully) EXACTLY like ANSNA, see https://github.com/patham9/ANSNA/wiki/Working-cycle


		Debug.msg("d", 0, "activate concept by task sdr = %s" % task.retHumanReadableId())


		# find concept by sdr
		selectedConcept = self._fetchConceptBySdr(task, task.sdr)

		# put it into the event FIFO of the concept
		# like in ANSNA
		selectedConcept.eventBeliefs.append(task)



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
		if False:
			Debug.msg("d", 0, "conceptualize belief task sdr = %s" % belief.retHumanReadableId())

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
