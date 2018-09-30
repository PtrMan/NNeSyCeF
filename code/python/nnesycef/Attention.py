from heapq import *
from random import uniform



"""

Attention:

key focus
* don't tend to forget elements which were rated as novel
  this is important for one shot learning and novel situations which just happen a few times

values:

novelity  0.0   1.0      starts at 1.0 and gets decreased by every observation by a factor

passiveAttention 0.0  1.0   starts at 1.0 and gets decreased by every timestep by a fixed factor (can be set by user system too for each task)


"""




# attention subsystem
class AttentionSystem(object):
	# we need 
	TYPE_TEMPORAL = 0
	TYPE_DECLARATIVE = 1

	def __init__(self, type_):
		self.decayFactorPerStep = 0.97

		level0Capacity = 100
		self._level0 = Level0(level0Capacity)

		self.type_ = type_

	# registers a task into the attention system
	def put(self, task):
		if self.type_ == AttentionSystem.TYPE_TEMPORAL:
			# we just need to append it to the queue because it will get processed by attentionCycle() in a batch
			self._level0._queue.append(task)
		else:
			# we need to add it to the sorted priority queue for processing by retHighestPriorityTasks()
			heappush(self._level0._priorityQueue, (self.computeAbsoluteAttentionMeasure(task.attention), task))


	# returns all tasks with the highest priority
	def retHighestPriorityTasks(self):
		# we can't efficiently recalculate the absolute attention measure for the declarative attention-buffer
		assert self.type_ == AttentionSystem.TYPE_TEMPORAL, "attentionCycle() must not get called for the (declarative) attention-buffer"

		if len(self._level0._priorityQueue) == 0:
			return []

		highestPriorityTasks = []
		highestPriorityTuple = heappop(self._level0._priorityQueue)

		highestPriority = highestPriorityTuple[0] ## take priority
		highestPriorityTasks.append(highestPriorityTuple[1]) ## take task

		## take from queue as long as the priority is the same (as the highest)
		while True:
			if len(self._level0._priorityQueue) == 0: ## we can't pop if length is zero
				## return because we are finished
				return highestPriorityTasks

			candidateTaskPriority, candidateTask = heappop(self._level0._priorityQueue)

			if candidateTaskPriority < highestPriority:
				## we need to push it pack because we just want the tasks with the highest priority
				heappush(self._level0._priorityQueue, (candidateTaskPriority, candidateTask))

				## return because we are finished because we don't find any more tasks
				return highestPriorityTasks

			highestPriorityTasks.append(candidateTask)


	# computes one cycle of the attention system
	# may be relativly expensive but runs in the same time due to the AIKR nature of the system
	def attentionCycle(self, reasoner):
		# we can't use the attention cycle logic for the (temporal) attention-buffer
		assert self.type_ == AttentionSystem.TYPE_DECLARATIVE, "attentionCycle() must not get called for the (temporal) attention-buffer"



		## copy over the tasks from the queue and decorate it with absolute attention
		queuedRemainingTasksWithMeasure = [(self.computeAbsoluteAttentionMeasure(i.attention), i) for i in self._level0._queue]

		## we also need this for normalization the probabilistic processing selection
		queuedRemainingTasksWithMeasureAttentionMeasureSum = sum([i[0] for i in queuedRemainingTasksWithMeasure])


		# give attention to all tasks up to all existing tasks
		## we need to store the number of tasks because we don't want to process the new tasks which may get created
		## 
		numberOfProcessingTasks = len(queuedRemainingTasksWithMeasure)

		# TODO< repeat a few times to give tasks more fair chances >

		i = 0
		while i < numberOfProcessingTasks:
			attentionMeasureOfSelectedTask, selectedTask = queuedRemainingTasksWithMeasure[i]


			# give the task computation time
			## we draw a random number and decide on it to give tasks with a higher priority a relativly higher chance
			taskIsSelected = uniform(0.0, queuedRemainingTasksWithMeasureAttentionMeasureSum) <= attentionMeasureOfSelectedTask
			if taskIsSelected:
				selectedTask.process(reasoner)


				## we need to rebalance the sum
				queuedRemainingTasksWithMeasureAttentionMeasureSum -= attentionMeasureOfSelectedTask

				# remove task from range of candidates
				del queuedRemainingTasksWithMeasure[i]
				i -= 1

				numberOfProcessingTasks-=1

			i += 1


		# transfer tasks with a low attention measure to the lower level

		## NOTE< we just ignore the tasks to forget them because we don't have any more levels >
		##       we would transfer them to level 1 and give tasks in level 1 a chance

		sortedQueuedTasksWithMeasure = [(self.computeAbsoluteAttentionMeasure(i.attention), i) for i in self._level0._queue]
		sortedQueuedTasksWithMeasure = sorted(sortedQueuedTasksWithMeasure, key=lambda x: x[0], reverse=True)

		sortedQueuedTasksWithMeasure = sortedQueuedTasksWithMeasure[:self._level0.capacity]
		## all tasks which fall out will be transfered to level 1
		transferedFromLevel0Tasks = sortedQueuedTasksWithMeasure[self._level0.capacity:]

		## tasks which didn't fall out stay in queue
		self._level0._queue = [i[1] for i in sortedQueuedTasksWithMeasure]

	# does a timestep for something which has a attentionValue
	def stepForTask(self, attentionValue):
		## tasks may have no attention value because they are non-NARS tasks
		if attentionValue == None:
			return

		# decay passive attention
		attentionValue.passiveAttention *= self.decayFactorPerStep

	# computes the absolute attention out of the attention value which is attached to all NARS-tasks
	# the computation should be fast and the algorithm/policy can be arbitrary
	def computeAbsoluteAttentionMeasure(self, attentionValue):
		## just return passive attention for now because it is enough
		return attentionValue.passiveAttention

# holds the attention values
class AttentionValue(object):
	def __init__(self):
		self.novelity = 1.0
		self.passiveAttention = 1.0


# Level 0 storage is for short term memory of extremely important tasks
class Level0(object):
	def __init__(self, capacity):
		self.capacity = capacity

		## either the queue or the priority queue are active - depending on the mode of the attention system (declarative or temporal)

		# TODO< set both to None and let the construction by done by a static make function >

		# queue to store to be processed tasks
		self._queue = []

		# priority queue to store tasks in a online-ordered fashion
		self._priorityQueue = []
