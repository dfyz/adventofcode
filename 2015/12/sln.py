import json
import sys

def traverse(node, ignore_red):
	if isinstance(node, (list, dict)):
		if ignore_red and isinstance(node, dict) and 'red' in node.itervalues():
			return 0
		values = node.itervalues() if isinstance(node, dict) else node
		return sum(traverse(sub_node, ignore_red) for sub_node in values)
	return node if isinstance(node, (int, long)) else 0

j = json.load(sys.stdin)
print traverse(j, False)
print traverse(j, True)