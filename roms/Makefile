ifeq ($(OS),Windows_NT)
	CP=copy
	RM=del
else
	CP=cp
	RM=rm
endif

ASM=sdasz80
LD=sdldz80

ASM=asz80
LD=aslink

all: monitor9.hex z8tbas_2.hex 

z8tbas_2.hex : z8tbas_2.rel
	aslink -i -m -u z8tbas_2.rel
	$(CP) z8tbas_2.ihx z8tbas_2.hex
	objcopy -I ihex -O binary z8tbas_2.ihx z8tbas_2.bin
	
monitor9.hex : monitor9.rel disasm.rel
#	asz80 -l -o monitor9.rel monitor9.s
	$(LD) -i -m -u monitor9 $^
	$(CP) monitor9.ihx monitor9.hex

clean:
	$(RM) *.hex *.rel
		
#.rel.s:
%.rel : %.s
	$(ASM) -l -o $<