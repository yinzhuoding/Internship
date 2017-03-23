# Use words.txt as the file name
fname = raw_input("Enter file name: ")
fh = open(fname)
for lines in fh:
	line = lines.rstrip()
	line = line.upper()
	print(line)