name = raw_input("Enter file:")
if len(name) < 1 : name = "mbox-short.txt"
handle = open(name)
hour = list()
for lines in handle:
	lines = lines.rstrip()
	if not lines.startswith("From "): continue
	time = lines.split()[5]
	hour.append(time.split(':')[0])
	
counts = dict()
for h in hour:
	counts[h] = counts.get(h,0) + 1
	
result = sorted([(v,k) for v,k in counts.items()])

for a,b in result:
	print a,b