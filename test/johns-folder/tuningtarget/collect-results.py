#!/usr/bin/python3

from os import listdir, getcwd
from os.path import isfile, join

resdir = join(getcwd(), "raw-results")
rawresfiles = [f for f in listdir(resdir) if isfile(join(resdir, f))]

reslines = []
for fname in rawresfiles:
	with open(join(resdir, fname), "r") as f:
		t_user = 0.0
		t_sys = 0.0
		for line in f.readlines():
			parts = line.strip().split()
			if len(parts) == 2:
				if parts[0] == "user":
					t_user = float(parts[1])
				if parts[0] == "sys":
					t_sys = float(parts[1])

		t_sum = t_user + t_sys
		t_adj = t_sum / 50.0
		l = "%s: %.03f seconds (adj: %.03f)" % (fname, t_sum, t_adj)
		reslines.append(l)

reslines = sorted(reslines)
reslines.append("")

with open("collected.txt", "w+") as f:
	f.write("\n".join(reslines))
