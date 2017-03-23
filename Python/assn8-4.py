fname = raw_input("Enter file name: ")
fh = open(fname)
lst = list()
wdlst = list()
for line in fh:
	words = line.split()
	for word in words:
		if wdlst.count(word): continue
		wdlst.append(word)
wdlst.sort()
print wdlst
		