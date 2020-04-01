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
		c_cuda = -1
		c_ocaml = -1
		for line in f.readlines():
			parts = line.strip().split()
			if len(parts) == 2:
				def tts(s):
					t_min = s[0:s.find("m")]
					t_sec = s[s.find("m")+1:s.find("s")].replace(",", ".")
					return (float(t_min) * 60.0) + float(t_sec)

				if parts[0] == "user":
					t_user = tts(parts[1])
				elif parts[0] == "sys":
					t_sys = tts(parts[1])
				elif "cudacost" in parts[0]:
					c_cuda = int(parts[1])
				elif "ocamlcost" in parts[0]:
					c_ocaml = int(parts[1])

		t_sum = t_user + t_sys
		t_adj = 0.0

		if "-it20" in fname:
			t_adj = t_sum / 20.0
		if "-it50" in fname:
			t_adj = t_sum / 50.0

		l = "%s: %.03f seconds (adj: %.03f)" % (fname, t_sum, t_adj)
		if c_cuda != -1:
			l += " (c_cuda: %d)" % c_cuda
		if c_ocaml != -1:
			l += " (c_ocaml: %d)" % c_ocaml
		if (c_cuda != -1) and (c_ocaml != -1):
			l += " (c_ocaml / c_cuda: %.03f)" % (float(c_ocaml) / float(c_cuda))

		reslines.append(l)

reslines = sorted(reslines)
reslines.append("")

with open("result.txt", "w+") as f:
	f.write("\n".join(reslines))
