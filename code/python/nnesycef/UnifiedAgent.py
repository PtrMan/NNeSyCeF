# (Draft) of a unified agent similar to NARS

from random import seed, randint
import numba

import Debug

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





from Reasoner import Reasoner



reasoner = Reasoner()









from Task import Task
import Distributed
from TruthValue import TruthValue
from Attention import *

import time

cycleCounter = 0
while True:
	t = time.process_time()




	# TODO< receie events from environment >
	# we just receive random fake events
	stamp = reasoner._genStamp()
	eventTask = Task(Task.ENUMTYPE_JUDGEMENT, stamp)
	eventTask.sdr = Distributed.genRandom()
	eventTask.truth = TruthValue(0.5, 0.5)
	eventTask.attention = AttentionValue()

	reasoner._thisCycleEventTasks.append(eventTask)

	if True:
		print("")
		print("")
		print("")
		print("")
		print("")

	Debug.msg("i", 0, "cycle")
	reasoner.cycle()





	elapsed_time = time.process_time() - t

	if (cycleCounter % 1) == 0:
		if True:
			print(elapsed_time)

	cycleCounter += 1


	# we just let the system run a limited amount of time for testing
	if cycleCounter > 20:
		exit()