monitor9.hex : monitor9.rel disasm.rel
#	asz80 -l -o monitor9.rel monitor9.s
	aslink -i -m -u monitor9 $^
	cp monitor9.ihx monitor9.hex
	
#.rel.s:
%.rel : %.s
	asz80 -l -o $<