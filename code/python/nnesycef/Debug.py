# encapsulates the diagnostic and debugging functionality for development

DEBUGLEVEL = -1

# /param type_ type of message "d" for debug, "i" for info
def msg(type_, level, message):
	if DEBUGLEVEL == -1:
		return

	print("[{}{}]: {}".format(type_, level, message))
