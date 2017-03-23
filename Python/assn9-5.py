name = raw_input("Enter file:")
if len(name) < 1 : name = "mbox-short.txt"
handle = open(name)
names = list()
for line in handle:
	line = line.rstrip()
	if not line.startswith("From "): continue
	words = line.split()
	names.append(words[1])

counts = dict()
for name in names:
	counts[name] = counts.get(name,0) + 1
	
maxcount = None
maxname = None
for name,count in counts.items():
	if maxcount is None or count>maxcount:
		maxname = name
		maxcount = count

print maxname, maxcount
