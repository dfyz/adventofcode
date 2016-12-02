import sys
msg = sys.stdin.readline().strip()
balance = 0
for i, ch in enumerate(msg):
	balance += {'(': 1, ')': -1}[ch]
	if balance < 0:
		print i + 1
		break