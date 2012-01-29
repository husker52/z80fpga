z8tbas_2.hex : z8tbas_2.rel
	aslink -i -m -u z8tbas_2.rel
	cp z8tbas_2.ihx z8tbas_2.hex
	objcopy -I ihex -O binary z8tbas_2.ihx z8tbas_2.bin
	
monitor9.hex : monitor9.rel disasm.rel
#	asz80 -l -o monitor9.rel monitor9.s
	aslink -i -m -u monitor9 $^
	cp monitor9.ihx monitor9.hex

clean:
	rm 
		
#.rel.s:
%.rel : %.s
	asz80 -l -o $<