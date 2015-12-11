import hashlib

prefix = 'ckczppom'

for ans in xrange(10**7):
	h = hashlib.md5(prefix + str(ans)).hexdigest()
	if h.startswith('0' * 6):
		print ans
		break