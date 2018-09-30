# encapsulates the diagnostic and debugging functionality for development


# /param type_ type of message "d" for debug, "i" for info
def msg(type_, level, message):
	print("[{}{}]: {}".format(type_, level, message))
