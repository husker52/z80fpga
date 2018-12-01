;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; Short Disassembler routine, by J.Kerr

;------------------------------------------------------------------------------
	.MODULE DISASM
	.globl	DISAS

	.globl	GETXY
	.globl	CHROP
	.globl	CPHLDE
	.globl	TXCRLF
	
	.area	CODE
;------------------------------------------------------------------------------
DISAS:	NOP			
DISASS:	CALL	GETXY		;GET XXXX=DE (START), YYYY=HL (END)
	INC	HL
	PUSH	HL		;STORE END OF DISASSEMBLE HUNT

BLOOP:	PUSH	DE
	CALL	DISZ80		;DISASSEMBLE INSTRUCTION AT (DE)

	LD	B,#3
GAP:	LD	A,#0x20		;<SPACE>
	CALL	CHROP		;PRINT 3 SPACES
	DJNZ	GAP

	POP	HL		;REGAIN THE OLD DE START OF CURRENT INSTRUCTION
TEXTLP:	LD	A,(HL)		;SHOW THE ASCII CHARS TO HELP
	CP	#0x20		;< SPC?
	JP	M,BAD		
	CP	#0x7E		;> TILDE?
	JP	M,GOOD		;MMMM, GOOD
BAD:	LD	A,#"."		;UNPRINTABLE, PUT A "." THERE INSTEAD.
GOOD:	CALL	CHROP		;TX IT
	INC	HL
	CALL	CPHLDE		;HL=DE?	
	JR	Z,TEXTEND
	JR	TEXTLP

TEXTEND:
	CALL	TXCRLF		;TXCRLF
	POP	HL		;RETRIEVE END OF DISASSEMBLY HUNT
	PUSH	HL		;AND RE-STORE IT
	AND	A		;RESET CY
	SBC	HL,DE
	RET	Z		;IF HL=DE, WE'RE FINISHED
	RET	C		;DECODE OF INST SENT DE BEYOND HL, FINISHED
	JR	BLOOP		;GO TO NEXT INSTRUCTION

;----------------------------------------------------------------------------------------
; Disassemble Z-80 Instruction pointed to by (DE)
;------------------------------------------------------------------------------
DISZ80:
	CALL 	ADRSP		;Convert the current adr in DE to HEX, print
       	LD 	BC,#0x0900		;9 pairs of spaces onto stack
       	LD 	HL,#0x2020		;$20 <space>
BUFFER:
	PUSH 	HL		;Place pair on stack
       	DJNZ 	BUFFER		;until all 9 done
       	LD 	H,B		;BC=0 after countdown
       	LD 	L,C		;Clear HL
       	ADD 	HL,SP		;Moves current SP value into HL
       	PUSH 	BC		;Save the zero
       	EX 	(SP),IX		;and move it into IX
       	PUSH 	BC		;Save 4 more bytes of zeros
       	PUSH 	BC
       	ADD 	IX,SP		;Move updated SP into IX
       	PUSH 	HL
       	LD 	HL,#GROUP3	;Group 3 
;----------------------------------------------------------------------------------------
TRYNDX:	CALL 	FETCH		;Convert byte at DE to Hex,print,update pointers
       	LD 	B,C
       	CP 	#0xED		;Contains two-byte Z80 specific op codes
       	JR 	Z,CONFLG

       	INC 	B
       	CP 	#0xDD		;IX specific op codes
       	JR 	Z,CONFLG

       	INC 	B
       	CP 	#0xFD		;IY specific op codes
       	JR 	NZ,NOTNDX		;Else it must be some other code
;----------------------------------------------------------------------------------------
CONFLG:	LD 	1(IX),B
       	INC 	B
       	DJNZ 	TRYNDX

       	JR 	NXBYTE
;----------------------------------------------------------------------------------------
NOTNDX:	LD 	C,A		;Save op code byte to C
       	LD 	A,1(IX)
       	OR 	A
       	JR 	Z,NODISP		;No two-byte op code

       	LD 	A,C
       	CP 	#0xCB		;CB+second byte op code
       	JR 	Z,GETDIS

       	AND 	#0x44
       	CP 	#4
       	JR 	Z,GETDIS

       	LD 	A,C
       	AND 	#0xC0
       	CP 	#0x40
       	JR 	NZ,NODISP
;----------------------------------------------------------------------------------------
GETDIS:	CALL 	FETCH		;Get second byte of two byte op code
       	LD 	2(IX),A	;Save it

NODISP:	LD 	HL,#GROUP1
       	LD 	A,C
       	CP 	#0xCB
       	JR 	NZ,NEWMSK

       	LD 	HL,#GROUP2	;Group 2

NXBYTE:	CALL 	FETCH
       	LD 	C,A

NEWMSK:	LD 	A,(HL)		;Get byte at (HL)
       	OR 	A		;Set Z flag if zero
       	JR 	Z,TABEND

       	AND 	C
       	INC 	HL

NEWMOD:	LD 	B,(HL)
       	INC 	HL
       	INC 	B
       	JR 	Z,NEWMSK

TRYMAT:	CP 	(HL)
       	INC 	HL
       	JR 	Z,GETNDX

       	BIT 	7,(HL)
       	INC 	HL
       	JR 	Z,TRYMAT

       	JR 	NEWMOD

GETNDX:	LD 	A,(HL)		;Load entry in reference table
       	AND 	#0x7F
       	DEC 	B

TABEND:	POP 	HL
       	PUSH 	DE
       	PUSH 	HL

       	EX 	DE,HL
       	LD 	HL,#MONICS		;Start of ASCII Op Code List
       	CALL 	XTRACT		;Store the code at 

       	POP 	HL
       	LD 	DE,#5
       	ADD 	HL,DE
       	POP 	DE

       	LD 	A,B
       	AND 	#0xF0
       	JR 	Z,SECOND

       	RRA
       	RRA
       	RRA
       	RRA
       	PUSH 	BC

       	LD 	B,A
       	LD 	A,C
       	CALL 	OPRND1

       	POP 	BC
       	LD 	A,B
       	AND 	#0x0F
       	JR 	Z,OPDONE

       	LD 	(HL),#44		;ASCII <comma>
       	INC 	HL
;----------------------------------------------------------------------------------------
SECOND:	LD 	A,B
       	AND 	#0x0F

       	LD 	B,A
       	LD 	A,C
       	CALL 	NZ,OPRND2
;----------------------------------------------------------------------------------------
OPDONE:	LD 	A,#3
       	SUB 	(IX)

       	POP 	HL
       	POP 	HL
       	POP 	IX

       	JR 	C,OUTEXT

       	INC 	A
       	LD 	B,A
       	ADD 	A,B
       	ADD 	A,B
       	LD 	B,A
;----------------------------------------------------------------------------------------
SPACES:	LD 	A,#0x20		;Print (B) spaces
       	CALL 	CHROP
       	DJNZ 	SPACES		;until done
;----------------------------------------------------------------------------------------
OUTEXT:	LD 	B,#18		;Outputs 9 characters of text off stack
PUTOUT:	DEC 	SP		;Keep up with stack pointer
       	POP 	HL		;Pop data into HL, text in H
       	LD 	A,H		;Move text to Accum for printing
       	CALL 	CHROP		;and call it
       	DJNZ 	PUTOUT		;keep it up until all 18 spots popped off stack
       	RET			;Finish out
;----------------------------------------------------------------------------------------
GROUP2:	.BYTE 	0xC0,0x36,0x40
	.BYTE 	0x04,0x80,0x2D,0xC0,0xBE
	.BYTE 	0xFF,0xF8,0x06,0x00,0x33
	.BYTE 	0x08,0x38,0x10,0x35,0x18
	.BYTE 	0x3A,0x20,0x3F,0x28,0x40
	.BYTE 	0x30,0x00,0x38,0xC1
;----------------------------------------------------------------------------------------
GROUP1:	.BYTE 	0xFF,0x00,0x00
	.BYTE 	0x24,0x07,0x32,0x0F,0x37
	.BYTE 	0x17,0x31,0x1F,0x36,0x27
	.BYTE 	0x0D,0x2F,0x0B,0x37,0x3D
	.BYTE 	0x3F,0x06,0x76,0x14,0xC9
	.BYTE 	0x30,0xD9,0x12,0xF3,0x0F
	.BYTE 	0xFB,0x91,0x72,0xC6,0x02
	.BYTE 	0xCE,0x01,0xDE,0xBC,0x02
	.BYTE 	0xD6,0x42,0xE6,0x03,0xEE
	.BYTE 	0x43,0xF6,0x25,0xFE,0x8C
	.BYTE 	0x04,0x08,0x93,0x01,0x10
	.BYTE 	0x10,0x18,0x9D,0xAF,0x22
	.BYTE 	0xA2,0xFA,0x2A,0xA2,0xA7
	.BYTE 	0x32,0xA2,0x7A,0x3A,0xA2
	.BYTE 	0x03,0xC3,0x1C,0xCD,0x85
	.BYTE 	0x97,0xD3,0xAA,0x79,0xDB
	.BYTE 	0x9B,0x5F,0xE3,0x93,0x0E
	.BYTE 	0xE9,0x9C,0x05,0xEB,0x93
	.BYTE 	0xDF,0xF9,0xA2,0xFF,0xC0
	.BYTE 	0xB6,0x40,0xA2,0xFF,0xF8
	.BYTE 	0x76,0x80,0x02,0x88,0x01
	.BYTE 	0x98,0xBC,0x06,0x90,0x42
	.BYTE 	0xA0,0x03,0xA8,0x43,0xB0
	.BYTE 	0x25,0xB8,0x8C,0xFF,0xC7
	.BYTE 	0x0B,0x04,0x16,0x05,0x8E
	.BYTE 	0xB2,0x06,0xA2,0x20,0xC0
	.BYTE 	0xB0,0x23,0xC2,0x1C,0xC4
	.BYTE 	0x85,0x10,0xC7,0xBB,0xFF
	.BYTE 	0xCF,0xD3,0x01,0xA2,0x0D
	.BYTE 	0x03,0x16,0x0B,0x8E,0xFD
	.BYTE 	0x09,0x82,0x60,0xC1,0x2B
	.BYTE 	0xC5,0xAC,0xFF,0xE7,0x21
	.BYTE 	0x20,0x9D,0xFF,0xEF,0xE7
	.BYTE 	0x02,0xA2,0x7E,0x0A,0xA2
;----------------------------------------------------------------------------------------
GROUP3:	.BYTE 	0xFF,0x00,0x44
	.BYTE 	0x23,0x45,0x2F,0x4D,0x2E
	.BYTE 	0x4E,0x00,0x67,0x39,0x6F
	.BYTE 	0x34,0x70,0x00,0x71,0x00
	.BYTE 	0xA0,0x21,0xA1,0x0A,0xA2
	.BYTE 	0x1A,0xA3,0x29,0xA8,0x1F
	.BYTE 	0xA9,0x08,0xAA,0x18,0xAB
	.BYTE 	0x28,0xB0,0x20,0xB1,0x09
	.BYTE 	0xB2,0x19,0xB3,0x27,0xB8
	.BYTE 	0x1E,0xB9,0x07,0xBA,0x17
	.BYTE 	0xBB,0xA6,0xFF,0xC7,0xB8
	.BYTE 	0x40,0x9B,0x8B,0x41,0xAA
	.BYTE 	0xFF,0xCF,0xFD,0x42,0x3C
	.BYTE 	0x4A,0x81,0xAD,0x43,0xA2
	.BYTE 	0xDA,0x4B,0xA2,0xFF,0xE7
	.BYTE 	0x40,0x46,0x95,0xFF,0xF7
	.BYTE 	0xC7,0x47,0xA2,0x7C,0x57
	.BYTE 	0xA2,0xFF,0x00
;----------------------------------------------------------------------------------------
MONICS:	.BYTE 	0xBF
	.BYTE 	"A","D","C"+0x80   	; ADC 
	.BYTE 	"A","D","D"+0x80   	; ADD 
	.BYTE 	"A","N","D"+0x80   	; AND 
	.BYTE 	"B","I","T"+0x80   	; BIT 
	.BYTE 	"C","A","L","L"+0x80	; CALL 
	.BYTE 	"C","C","F"+0x80   	; CCF
	.BYTE 	"C","P","D","R"+0x80	; CPDR
	.BYTE 	"C","P","D"+0x80   	; CPD
	.BYTE 	"C","P","I","R"+0x80	; CPIR
	.BYTE 	"C","P","I"+0x80   	; CPI
	.BYTE 	"C","P","L"+0x80   	; CPL
	.BYTE 	"C","P"+0x80      	; CP 
	.BYTE 	"D","A","A"+0x80   	; DAA
	.BYTE 	"D","E","C"+0x80   	; DEC 
	.BYTE 	"D","I"+0x80      	; DI
	.BYTE 	"D","J","N","Z"+0x80	; DJNZ 
	.BYTE 	"E","I"+0x80      	; EI
	.BYTE 	"E","X","X"+0x80   	; EXX
	.BYTE 	"E","X"+0x80      	; EX 
	.BYTE 	"H","A","L","T"+0x80	; HALT
	.BYTE 	"I","M"+0x80      	; IM 
	.BYTE 	"I","N","C"+0x80   	; INC 
	.BYTE 	"I","N","D","R"+0x80	; INDR
	.BYTE 	"I","N","D"+0x80   	; IND
	.BYTE 	"I","N","I","R"+0x80	; INIR
	.BYTE 	"I","N","I"+0x80   	; INI
	.BYTE 	"I","N"+0x80      	; IN 
	.BYTE 	"J","P"+0x80      	; JP 
	.BYTE 	"J","R"+0x80      	; JR 
	.BYTE 	"L","D","D","R"+0x80	; LDDR
	.BYTE 	"L","D","D"+0x80   	; LDD
	.BYTE 	"L","D","I","R"+0x80	; LDIR
	.BYTE 	"L","D","I"+0x80   	; LDI
	.BYTE 	"L","D"+0x80      	; LD 
	.BYTE 	"N","E","G"+0x80   	; NEG
	.BYTE 	"N","O","P"+0x80   	; NOP
	.BYTE 	"O","R"+0x80      	; OR 
	.BYTE 	"O","T","D","R"+0x80	; OTDR
	.BYTE 	"O","T","I","R"+0x80	; OTIR
	.BYTE 	"O","U","T","D"+0x80	; OUTD
	.BYTE 	"O","U","T","I"+0x80	; OUTI
	.BYTE 	"O","U","T"+0x80   	; OUT 
	.BYTE 	"P","O","P"+0x80   	; POP 
	.BYTE 	"P","U","S","H"+0x80	; PUSH 
	.BYTE 	"R","E","S"+0x80   	; RES 
	.BYTE 	"R","E","T","I"+0x80	; RETI
	.BYTE 	"R","E","T","N"+0x80	; RETN
	.BYTE 	"R","E","T"+0x80   	; RET
	.BYTE 	"R","L","A"+0x80   	; RLA
	.BYTE 	"R","L","C","A"+0x80	; RLCA
	.BYTE 	"R","L","C"+0x80   	; RLC 
	.BYTE 	"R","L","D"+0x80   	; RLD
	.BYTE 	"R","L"+0x80      	; RL 
	.BYTE 	"R","R","A"+0x80   	; RRA
	.BYTE 	"R","R","C","A"+0x80	; RA
	.BYTE 	"R","R","C"+0x80   	; RRC 
	.BYTE 	"R","R","D"+0x80   	; RRD
	.BYTE 	"R","R"+0x80      	; RR 
	.BYTE 	"R","S","T"+0x80   	; RST 
	.BYTE 	"S","B","C"+0x80   	; SBC 
	.BYTE 	"S","C","F"+0x80   	; SCF
	.BYTE 	"S","E","T"+0x80   	; SET 
	.BYTE 	"S","L","A"+0x80   	; SLA 
	.BYTE 	"S","R","A"+0x80   	; SRA 
	.BYTE 	"S","R","L"+0x80   	; SRL 
	.BYTE 	"S","U","B"+0x80   	; SUB 
	.BYTE 	"X","O","R"+0x80   	; XOR 
;----------------------------------------------------------------------------------------
OPRND1:	DJNZ 	CONDIT

RSTADR:	AND 	#0x38
       	JR 	DA

OPRND2:	DJNZ 	DAT8

RELADR:	CALL 	FETCH
       	LD 	C,A
       	RLA
       	SBC 	A,A
       	LD 	B,A
       	EX 	DE,HL
       	PUSH 	HL
       	ADD 	HL,BC
       	JR 	DHL

CONDIT:	RRA
       	RRA
       	RRA
       	DJNZ 	BITNUM

       	BIT 	4,A
       	JR 	NZ,ABS

       	AND 	#3
	
ABS:   	AND 	#7
       	ADD 	A,#0x14
       	JR 	PS1

DAT8:  	DJNZ 	DAT16

D8:    	CALL 	FETCH
       	JR 	DA

BITNUM:	DJNZ 	INTMOD
       	AND 	#7

DA:    	LD 	C,A
       	SUB 	A
       	JR 	DAC

DAT16: 	DJNZ 	EXAF
	
D16:   	CALL 	FETCH
       	LD 	C,A
       	CALL 	FETCH

DAC:   	EX 	DE,HL
       	PUSH 	HL
       	LD 	H,A
       	LD 	L,C

DHL:   	LD 	C,#0xF8
       	PUSH 	HL
       	CALL 	CONVHL
       	POP 	HL
       	LD 	BC,#0x000A
       	OR 	A		;Clear CY
       	SBC 	HL,BC
       	POP 	HL
       	EX 	DE,HL
       	RET 	C

       	LD 	(HL),#"H"		;Loads Char "H" into string
       	INC 	HL		;Point to next location
       	RET
;----------------------------------------------------------------------------------------
INTMOD:	DJNZ 	STKTOP
       	AND 	#3
       	ADD 	A,#0x1C
	
PS1:   	JR 	PS3

STKTOP:	LD 	C,#0x13
       	DEC 	B
       	JR 	Z,PS2

REG16P:	DJNZ 	COMMON
       	RRA
       	AND 	#3
       	CP 	#3
       	JR 	NZ,RX

       	DEC 	A
       	JR 	RNX

EXAF:  	LD 	C,#0x0A		;Exchange AF,AF'
       	DEC 	B
       	JR 	Z,PS2

EXDE:  	INC 	C		;Exchange DE,HL
       	DEC 	B
       	JR 	Z,PS2

REG8S: 	DJNZ 	ACCUM

R8:    	AND 	#7
       	CP 	#6
       	JR 	NZ,PS3

       	LD 	(HL),#"("		;Bracket
       	INC 	HL
       	CALL 	REGX
       	LD 	A,2(IX)
       	OR 	A
       	JR 	Z,RP

       	LD 	(HL),#43 		;+ Plus sign
       	RLCA
       	RRCA
       	JR 	NC,POS

       	LD 	(HL),#45		;- Minus sign
       	NEG			;Negate the Accumulator

POS:   	INC 	HL
       	EX 	DE,HL
       	PUSH 	HL
       	LD 	H,B
       	LD 	L,A
       	LD 	C,#0xFB
       	CALL 	CONVHL
       	POP 	HL
       	EX 	DE,HL
       	JR 	RP

ACCUM: 	RRA
       	RRA
       	RRA

COMMON:	LD 	C,#7
       	DEC 	B
       	JR 	Z,PS2

PORTC: 	DEC 	C
       	DJNZ 	IDAT8

PS2:   	LD 	A,C
PS3:   	JR 	PS4

IDAT8: 	DJNZ 	IDAT16
       	LD 	(HL),#"("
       	INC 	HL
       	CALL 	D8
       	JR 	RP

IDAT16:	DJNZ 	REG8
       	LD 	(HL),#"("
       	INC 	HL
       	CALL 	D16
       	JR 	RP

REG8:  	DEC 	B
       	JR 	Z,R8

IPAREF:	DJNZ 	REG16
       	AND 	#9
       	JR 	PS4

REG16: 	RRA
       	DJNZ 	IREG16

R16:   	AND 	#3
RX:    	CP  	#2
       	JR 	Z,REGX

RNX:   	ADD 	A,#0x0C
       	JR 	PS4

IREG16:	DJNZ 	REGX
       	LD 	(HL),#"("
       	INC 	HL
       	CALL 	R16

RP:    	LD 	(HL),#")"
       	INC 	HL
       	RET

REGX:  	LD 	A,1(IX)
       	ADD 	A,#0x10

PS4:   	EX 	DE,HL
       	PUSH 	HL
       	LD 	HL,#RGSTRS		;Point to Registers Table
       	CALL 	XTRACT		;Get register name
       	POP 	HL
       	EX 	DE,HL
       	RET
;----------------------------------------------------------------------------------------
RGSTRS:	.BYTE 	"B"+#0x80
	.BYTE 	"C"+#0x80
	.BYTE 	"D"+#0x80
	.BYTE 	"E"+#0x80
	.BYTE 	"H"+#0x80
	.BYTE 	"L"+#0x80
	.BYTE 	"(","C",")"+#0x80
	.BYTE 	"A"+#0x80
	.BYTE 	"I"+#0x80
	.BYTE 	"R"+#0x80
	.BYTE 	"A","F",",","A","F","'"+#0x80
	.BYTE 	"D","E",",","H","L"	+#0x80
	.BYTE 	"B","C"+#0x80
	.BYTE 	"D","E"+#0x80
	.BYTE 	"A","F"+#0x80
	.BYTE 	"S","P"+#0x80
	.BYTE 	"H","L"+#0x80
	.BYTE 	"I","X"+#0x80
	.BYTE 	"I","Y"+#0x80
	.BYTE 	"(","S","P",")"+#0x80
	.BYTE 	"N","Z"+#0x80
	.BYTE 	"Z"+#0x80
	.BYTE 	"N","C"+#0x80
	.BYTE 	"C"+#0x80
	.BYTE 	"P","O"+#0x80
	.BYTE 	"P","E"+#0x80
	.BYTE 	"P"+#0x80
	.BYTE 	"M"+#0x80
	.BYTE 	"0"+#0x80
	.BYTE 	"?"+#0x80
	.BYTE 	"1"+#0x80
	.BYTE 	"2"+#0x80
;----------------------------------------------------------------------------------------
CONVHL:	SUB 	A		; Clear A
CVHL1: 	PUSH 	AF		; and store it
       	SUB 	A		; Clear A
       	LD 	B,#16		; 16 Iterations
CVHL2: 	ADD 	A,C
       	JR 	C,CVHL3
       	SUB 	C
CVHL3: 	ADC 	HL,HL
       	RLA
       	DJNZ 	CVHL2
       	JR 	NZ,CVHL1
       	CP 	#10		; Affects CY flag if A>10
       	INC 	B		; Does not affect CY
       	JR 	NC,CVHL1
CVHL4: 	CP 	#10		; Affects CY
       	SBC 	A,#0x69
       	DAA
       	LD 	(DE),A
       	INC 	DE
       	POP 	AF
       	JR 	NZ,CVHL4
       	RET
;----------------------------------------------------------------------------------------
XTRACT:	OR 	A		; When A=0, correct opcode,register reached in table
       	JR 	Z,COPY		; Copy the opcode to memory

SKIP:  	BIT 	7,(HL)		; If not at correct position
       	INC 	HL		; move forward in table
       	JR 	Z,SKIP		; until $80+char found

       	DEC 	A		; Down count through table
       	JR 	NZ,SKIP		; Execute skips over table until A=0
;----------------------------------------------------------------------------------------
COPY:  	LD 	A,(HL)		; Get the byte at (HL)
       	RLCA			; Rotates Left D7 into D0 and CY
       	SRL 	A		; Logic Shift Right 0 into D7,D0 into CY
       	LD 	(DE),A		; Stores the stripped char to (DE)
       	INC 	DE		; Increment both pointers
       	INC 	HL
       	JR 	NC,COPY		; Continues until D7="1", end of table entry
       	RET
;----------------------------------------------------------------------------------------
FETCH: 	LD 	A,(DE)		; Get byte at start of instruction
       	INC 	DE		; Point to next location
	INC	(IX)
       	PUSH 	AF		; Preserve the instruction
       	CALL 	BYTSP		; Decode it to Hex ASCII and print it
       	POP 	AF		; Retrieve the instruction
       	RET			; and return
;----------------------------------------------------------------------------------------
ADRSP: 	LD 	A,D		; Converts DE to ASCII Hex and prints it
       	CALL 	BYTOP		; Convert D first, 
       	LD 	A,E		; Then do E
;----------------------------------------------------------------------------------------
BYTSP: 	CALL 	BYTOP		; Convert the byte to ASCII
       	LD 	A,#0x20		; Print a <space>
       	JP 	CHROP		; output it
;----------------------------------------------------------------------------------------
BYTOP: 	PUSH 	AF		; Convert the upper nybble to Hex ASCII first
       	RRA			; Slowly
       	RRA			;   Rotate
       	RRA			;     It
       	RRA			;       Over to the right
       	CALL 	HEXOP		; Convert the nybble D3-D0 to Hex ASCII
       	POP 	AF		; Convert the lower nybble
;----------------------------------------------------------------------------------------
HEXOP: 	AND 	#0x0F		; Convert the nybble at D3-D2-D1-D0 to Hex ASCII char
       	CP 	#10		; Neat trick for converting nybble to ASCII
       	SBC 	A,#0x69
       	DAA			; Uses DAA trick
	RST	0x08
	RET
;------------------------------------------------------------------------------
; This routine is the Character Output (CHROP) 
;------------------------------------------------------------------------------
CHROP:	RST	0x08		; Output the Character
	RET			; Return
;------------------------------------------------------------------------------	
; END OF DISASSEMBLER
;------------------------------------------------------------------------------
