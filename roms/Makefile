ASMPATH=$(CURDIR)/../../z80support/asxv5p00/asxv5pxx/asxmak/vc6/exe/

ifeq ($(OS),Windows_NT)
	CP=copy
	RM=del
else
	CP=cp
	RM=rm
endif

ASM=sdasz80
LD=sdldz80

#ASM=$(ASMPATH)asz80
#LD=$(ASMPATH)aslink

all: monitor9.hex

z8tbas_2.hex : z8tbas_2.rel
	$(LD) -i -m -u z8tbas_2.rel
	$(CP) z8tbas_2.ihx z8tbas_2.hex
	objcopy -I ihex -O binary z8tbas_2.ihx z8tbas_2.bin
	
monitor9.hex : monitor9.rel disasm.rel 
	$(LD) -i -m -b _CODE=INTEND -u monitor9 $^
	$(CP) monitor9.ihx monitor9.hex

clean:
	$(RM) *.hex *.rel
		
#.rel.s:
%.rel : %.s
	$(ASM) -l -o -s $<