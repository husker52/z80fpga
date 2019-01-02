;------------------------------------------------------------------------------
;
; Space-Time Productions Single Board Computer
; Z80 Monitor Rom
;
; 10 June 2005 - Rev 7
; Baud rates on PIT changed to 0)38400, 1)4800 and 2)MIDI 31250 baud
; Rom checksums and LCD inits added, PIO's set on bootup
;
; 01 December 2005 - Rev 8
; New LCD drivers coded, Print command added, Unload command added
;
;------------------------------------------------------------------------------
; General Equates
;------------------------------------------------------------------------------
ROM0 = 0x0000
ROM1 = 0x1000
ROM2 = 0x2000
ROM3 = 0x3000

IOFLAG = 0x400E	; I/O Status Flag Word
;  0   0   0   0    1   0   0   1   $09 Default Value
; [7] [6] [5] [4]  [3] [2] [1] [0]
;  +   +   +   +    +   +   +   +--Enable RS232 Channel A Output
;  +   +   +   +    +   +   +------Enable RS232 Channel B Output
;  +   +   +   +    +   +----------Enable LCD Output
;  +   +   +   +    +--------------Enable Aux port printer... TBD
;  +   +   +   +-------------------
;  +   +   +-----------------------
;  +   +---------------------------Enable 6581 SID Update during IRQ
;  +-------------------------------Enable Time/Date Update during NMI

CHAR = 0x400F	; Storage for character to be printed

;------------------------------------------------------------------------------
;                         START OF MONITOR ROM
;------------------------------------------------------------------------------
	.globl	GETXY
	.globl	CHROP
	.globl	CPHLDE
	.globl	TXCRLF

	.globl	DISAS
	.globl	HLOUT
	.globl	TESTM
	.globl	INTEND
	.module mon

	.area	_HEADER(ABS)
	.ORG	0x0000		; MONITOR ROM RESET VECTOR
;------------------------------------------------------------------------------
; TX a character over RS232 Channel A [Host], wait for TXDONE first.
;------------------------------------------------------------------------------
RST00:	DI			;Disable INTerrupts
	IM	1		;INT vectors over to $0038
	JP	INIT		;Initialize Hardware and go
	NOP
	NOP
;------------------------------------------------------------------------------
; TX a character over RS232 Channel A [Host], wait for TXDONE first.
;------------------------------------------------------------------------------
RST08:	JP	TXA
	NOP
	NOP
	NOP
	NOP
	NOP
;------------------------------------------------------------------------------
; RX a character over RS232 Channel A [Console], hold here until char ready.
;------------------------------------------------------------------------------
RST10:	JP	RXA
	NOP
	NOP
	NOP
	NOP
	NOP
;------------------------------------------------------------------------------
; TX a character over MIDI (Serial Channel B), wait for TXDONE first.
;------------------------------------------------------------------------------
RST18:	JP	TXB
	NOP
	NOP
	NOP
	NOP
	NOP
;------------------------------------------------------------------------------
; RX a character from MIDI (Serial Channel B), hold here until char ready.
;------------------------------------------------------------------------------
RST20:	JP	RXB
	NOP
	NOP
	NOP
	NOP
	NOP
;------------------------------------------------------------------------------
; TX a character to the 8279 Display, check 8279 display ready prior to write.
;------------------------------------------------------------------------------
RST28:	JP	TX8279	; Transmit char in A out to 8279
	NOP
	NOP
	NOP
	NOP
	NOP
;------------------------------------------------------------------------------
; RX a character from the 8279 Keyboard, hold here until key is in buffer.
;------------------------------------------------------------------------------
RST30:	JP	RX8279	; Wait for char from 8279 keyboard
	NOP
	NOP
	NOP
	NOP
	NOP
;------------------------------------------------------------------------------
; RST 38 - INTERRUPT VECTOR [ for IM 1 ]
;------------------------------------------------------------------------------
RST38:	DI		; Breakpoint, Disable Interrupts	
	PUSH AF		; And Push all registers on Stack	
	PUSH BC
	PUSH DE              		
	PUSH HL              		
	PUSH IX              		
	PUSH IY              		
	EXX		; Push the alternate registers also
	EX   AF,AF'          		
	PUSH AF              		
	PUSH BC              		
	PUSH DE              		
	PUSH HL
	JR   NORMAL	; and Enter the Monitor at Prompt	
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------		
GOTO:	POP  HL		; GO command terminates here	
	POP  HL		; Pops all user register values
	POP  DE		; and hits RET instruction which
	POP  BC		; causes JP to user vector at SP+0018	
	POP  AF
	EX   AF,AF'
	EXX
	POP  IY
	POP  IX
	POP  HL
	POP  DE
	POP  BC
	POP  AF
	EI		; Enable INTerrupts
	RET		; and Return (the Ret vector at SP-0018	)
	RST  0x38	    	; is altered by GO cmd so that this RET	
	RST  0x38	    	; becomes the user's GOTO Vector)	
	RST  0x38
;------------------------------------------------------------------------------
; [NMI VECTOR]
; The NMI signal can cause a program to break and save all its register values
; The INT signal would work the same if the processor is put into IM 1, otherwise
; It is not set in this rom, and would default to the 8080 mode where, on INT-, the
; Interrupting device (or some other rigged hardware) would supply the necessary RST
;------------------------------------------------------------------------------
	.ORG 0x0066	; NMI VECTOR
RST66:	push	af
	push	bc
	push	de
	push	hl

;	LD   HL,TESTMSG	; Print a message
;	CALL PRT
	
	pop	hl
	pop	de
	pop	bc
	pop	af
	retn
INTEND:
	;JR   RST38	; Just play like it's an INT in IM 1

	
	.area _CODE
;------------------------------------------------------------------------------
; Monitor Entry point following an Error
;------------------------------------------------------------------------------
ERROR:	POP  HL		; Entry point for Monitor, after ERROR	
	LD   A,#0x3F	; Get a "?"	
	RST  0x08		; And TX it out.
;------------------------------------------------------------------------------
; Normal Monitor Entry Point
;------------------------------------------------------------------------------
NORMAL:	CALL TXCRLF	; Entry point for Monitor, Normal	
	LD   A,#0x3E	; Get a ">"	
	RST  0x08		; and TX it out.
	LD   HL,#NORMAL	; Save entry point for Monitor	
	PUSH HL		; This becomes the last RET address

NORM1:	RST  0x10		; Get user character
	
	CP   #0x20		; <spc>? 	
	JR   Z,NORM1	; Go back for something else
	
	CP   #0x3A		; ":"?
	JP   Z,LOAD1	; It's the first character of a load command
	
	CP   #0x61	; if upper case, convert to lower case
	JP   M,ALLUPR
	SUB  #0x20
ALLUPR:
;	AND  #0x5F		; Make character uppercase
	CP   #0x56		; Character > "U" ?	
	JR   NC,ERROR	; It's wrong, give an error and start over
	
	RST  0x08		; Print it back to the console
	
	SUB  #0x3F		; Take values from ascii A-U down to 0-20 decimal
	JR   C,ERROR	; Must have given a Minus (Char < "A"), go back	
	
	LD   HL,#CMDTBL	; Start at Keyboard Command Vector Table
	ADD  A,A		; Double the value remaining in A to give
	ADD  A,L		; Set HL to index in Vector Table
	LD   L,A		; Move new sum back to HL
	
	LD   E,(HL)	; Get Jump vector for key into DE	
	INC  HL		; Upper byte of vector
	LD   D,(HL)	; Load from the table into DE
	PUSH DE		; Create the new RET vector from DE.
	RET		; And jump there now.
;------------------------------------------------------------------------------
; Keyboard Command Vector Table
;------------------------------------------------------------------------------
CMDTBL:	
	.WORD	HELP	; <?> HELP
	.WORD	ERROR	; <@> NO CMD
	.WORD	RST00	; <A> 0x0000 RESET
	.WORD	CHGBAUD	; <B> CHANGE BAUD [CHAN],[DIVISOR]
	.WORD	MCOMP	; <C> COMPARE MEMORY
	.WORD	MEMDMP	; <D> DUMP MEMORY
	.WORD	EDITREG	; <E> EXAMINE/MODIFY REGISTERS
	.WORD	MEMFILL	; <F> FILL MEMORY WITH VALUE
	.WORD	GOCMD	; <G> GO FROM LAST BREAKPOINT OR ADDR
	.WORD	HXMATH	; <H> HEX SUM/DIFFERENCE
	.WORD	INPORT	; <I> INPUT FROM PORT
	.WORD	MEMTEST	; <J> JUSTIFY MEMORY (MEMORY TEST)
	.WORD	BRKPT	; <K> KILL/RESTORE LAST BREAKPOINT
	.WORD	LOAD	; <L> LOAD INTEL HEX PROGRAM
	.WORD	MOVMEM	; <M> MOVE MEMORY
	.WORD	DISAS	; <N> DISASSEMBLY VECTOR (HIJACKED THIS ONE)
	.WORD	OUTPORT	; <O> OUTPUT TO PORT
	.WORD	ASC2MEM	; <P> PUT ASCII IN MEMORY
	.WORD	USRSTK	; <Q> DISPLAY USER STACK LOCATION
	.WORD	DSPLREG	; <R> REGISTERS DISPLAY
	.WORD	SUBMEM	; <S> SUBSTITUTE MEMORY
	.WORD	TYPEMEM	; <T> TYPE ASCII FROM MEMORY
	.WORD     UNLOAD	; <U> UNLOAD ALL MEMORY

;------------------------------------------------------------------------------
; ENTRY POINT FOR <?> help screen
;------------------------------------------------------------------------------
HELP:	LD   HL,#HELPSCR	; Print HELP message
	CALL PRT
	RET
;------------------------------------------------------------------------------
; ENTRY POINT FOR <J> Memory Test
;------------------------------------------------------------------------------
MEMTEST:
	CALL GETXY	; Get XXXX to DE,YYYY to HL from user	
	RET  C		; on Carry <Ctrl-C> was hit, cancel.	
	EX   DE,HL	; Swap start, ending address
MEMTEST1:
	LD   A,(HL)	; Get byte from starting address	
	LD   B,A		; Save it	
	CPL	         	; Invert all 1's, 0's	
	LD   (HL),A	; Write it back	
	XOR  (HL)		; Get logical difference	
	JP   NZ,PRTMEM	; Write it out if there's a problem	
	LD   (HL),B        	; else, restore the original byte	
	CALL CPHLDE	; Check if HL=DE, Zero if =	
	INC  HL             ; Next location	
	JR   NZ,MEMTEST1	; HL<>DE, repeat the process	
	RET		; and get back to the prompt	
;------------------------------------------------------------------------------
; Dump command
;------------------------------------------------------------------------------
MEMDMP:	CALL GETXY	; ENTRY POINT FOR <D>, Get xxxx,yyyy	
	RET  C		; Exit if user Ctrl-C'd us.	
	EX   DE,HL	; Put start in HL, ending addr in DE
	LD   A,L		; Set starting low nybble at zero
	AND  #0xF0		; Strip off lower misc bits
	LD   L,A		; And save it back
	LD   A,E		; See if lower nybble zero
	OR   #0x0F		; Set to end of line
	LD   E,A		; Save back altered lower byte	
MEMDMP1:
	LD   A,L		; Are we at a new page?
	OR   A		; Test Z flag
	CALL Z,MEMDMP0	; If new page, print header row first
	CALL HLCOLN	; Print addr in HL plus ": "	
	PUSH HL		; Store HL
	POP  IX		; Retrieve it
	LD   B,#0x10	; Get 0-F bytes of data	
MEMDMP2:
	PUSH BC		; Save the byte counter	
	LD   A,(HL)	; Load memory from HL	
	CALL AOUT		; Convert byte to ASC in BC and TXA it	
	CALL TXSPC	; TXA a <space>	
	POP  BC		; Get the byte counter back	
	INC  HL		; Otherwise get next location	
	DEC  B		; Decrement byte count	
	JR   NZ,MEMDMP2	; And finish this row.
	LD   A,#0x20	; Print a space
	RST  0x08		; Output it
	LD   B,#0x10	; Get 0-F data
	PUSH IX		; Get back start of line
	POP  HL		; Into HL
MEMDMP3:
	LD   A,(HL)	; Get character
	CP   #0x20		; < Space?
	JP   M,MEMDMP4	; Yes, just print a '.'
	CP   #0x7E		; > ~ ?
	JP   M,MEMDMP5	; It's in between so print it
MEMDMP4:
	LD   A,#"."	; Get a period
MEMDMP5:
	RST  0x08		; Print the character
	INC  HL		; Increment pointer
	DJNZ MEMDMP3	; And loop until line done
	CALL TXCRLF	; Carriage Ret, Line Feed
	CALL CKSIOA	; See if SIO has a character ready to read.
	RET  C		; Exit out if there's a character
	CALL CPHLDE	; Check if HL=DE	
	RET  Z		; Finished if at ending address
	RET  NC		; We went beyond the ending address
	LD   A,H		; Is HL rolled over to $0000?
	OR   L		; Test Z flag
	RET  Z		; Finished if rolled past
	JR   MEMDMP1	; else, get the next row.
; Print header row
MEMDMP0:
	CALL TXCRLF	; Make a new line for it
	PUSH HL		; Store Start
	LD   HL,#DMPTEXT	; Get line of text
	CALL PRTSTR	; And print it
	POP  HL		; Retrieve starting address
	RET		; and RETurn		
;------------------------------------------------------------------------------
; Substitute Memory command
;------------------------------------------------------------------------------
SUBMEM:
	CALL GETX		; ENTRY POINT FOR <S>, Get xxxx from user.	
	RET  C		; Exit if he <Ctrl-C'd> us.	
SUBMEM1:
	CALL PRTMEM	; Print CRLF + "/ " + byte at (HL)	
	LD   A,#0x2D	; Character "-"	
	RST  0x08		; TXA it	
	EX   DE,HL	; Swap
	CALL GETHL	; Get 4 Hex chars from User, convert in HL	
	EX   DE,HL	; Place them in DE	
	JR   C,SUBMEM2	; If user entry term'd normally, replace	
	LD   (HL),E	; memory location with the new data in E	
SUBMEM2:
	INC  HL		; otherwise, skip ahead to the next location	
	CP   #0x20		; <spc>?	
	JR   Z,SUBMEM1	; Get next byte and location          		
	CP   #0x0A		; LF?
	JR   Z,SUBMEM1	; Get next byte and location
	RET
;------------------------------------------------------------------------------
; GO command
;------------------------------------------------------------------------------
GOCMD:
	CALL GETHL	; ENTRY POINT FOR <G>oto addr. Get XXXX from user.
	JR   NC,GOCMD1	; Must have given a good answer         	
	CP   #0x03		; <CTRL-C>, must have chickened out
	RET  Z
	JP   GOTO		; POP Everything and use SP-$0018 as GOTO address
		
GOCMD1:	EX   DE,HL	; Put GOTO address in DE
	LD   HL,#0x0018	; Locate in Memory above Stack where
	ADD  HL,SP	; RET from sub vector is located	
	DEC  HL              	
	LD   (HL),D	; Store User Jump Address, after JP to 0024H
	DEC  HL		; The RET is altered and becomes the return-to addr	
	LD   (HL),E
GOCMD2:	CP   #0x2C		; Comma? User is Entering a Breakpoint also	
	JP   NZ,GOTO	; End of Breakpoints? Pop everything and RET	
	CALL GETX		; Get XXXX from user for Breakpoint	
	RET  C		; Cancel if he <Ctrl-C>'s us	
	LD   B,A             		
	LD   A,(HL)	; Save original memory contents of breakpoint location	
	LD   (0x37FF),A	; At $37FF	
	LD   (0x37FD),HL	; And the address of the Breakpoint at $37FD-$37FE	
	LD   (HL),#0xFF	; Swap instruction at (HL) for a RST 38H.
	LD   A,B		; And Restore the Terminating character, or ","	
	JP   GOTO		; Do the GO
;------------------------------------------------------------------------------
; Kill / Restore last Breakpoint
;------------------------------------------------------------------------------
BRKPT:	LD   HL,(0x37FD)	; ENTRY POINT FOR <K>ill/Restore last breakpoint
	LD   A,(0x37FF)	; Recall original contents	
	LD   (HL),A	; Write it back	
	RET		; And finish.	
;------------------------------------------------------------------------------
; Display User Stack Register
;------------------------------------------------------------------------------
USRSTK:	CALL TXSPC	; ENTRY POINT FOR <Q> display user stack	
	LD   HL,#0x0018	; Set User Stack 18H below actual stack	
	ADD  HL,SP	; Get adjusted SP into HL
	JP   HLOUT	; Convert and TXA contents of HL	
;------------------------------------------------------------------------------
; Input Port command		
;------------------------------------------------------------------------------
INPORT:	CALL GETX		; ENTRY POINT FOR <I>nput Port	
	RET  C		; Get user port address
	CALL TXCRLF	; TXCRLF	
	LD   C,L		; Load the port address into C
	IN   A,(C)	; Get the data
	JP   AOUT		; CONVERT BYTE TO ASCII AND OUTPUT
;------------------------------------------------------------------------------
; Output Port command
;------------------------------------------------------------------------------
OUTPORT:
	CALL GETXY	; ENTRY POINT FOR <O>utput Port,Value	
	RET  C		; Get user port and data to output xxxx,yyyy		
	LD   C,E		; Port address into C		
	OUT  (C),L	; Output the yy data to the port address
	RET                  		
;------------------------------------------------------------------------------
; Hex Sum/Difference command
;------------------------------------------------------------------------------
HXMATH:	CALL GETXY	; ENTRY POINT FOR <H>ex Sum,Diff	
	RET  C		; Get xxxx (DE), yyyy (HL)
	CALL TXCRLF	; TXCRLF	
	EX   DE,HL	; Swap the values	
	ADD  HL,DE	; Do the addition		
	CALL HLCOLN	; Convert and print the contents of HL
	SUB  A		; Zero out A, clear CY (borrow flag)		
	SBC  HL,DE	; Do the subtract		
	SBC  HL,DE	; Do it again to get the actual subtract
	JP   HLOUT	; Convert and print contents of HL		
;------------------------------------------------------------------------------
; <P>ut ASCII string into memory
;------------------------------------------------------------------------------
ASC2MEM:
	CALL GETX		; Get last 4 hex chars typed into HL	
	RET  C		; Break if <Ctrl-C>	
	CALL TXCRLF	; TXCRLF	
A2MEM1:	CALL ECHO		; RX a Character	
	CP   #0x03		; Break?	
	JP   Z,HLOUT	; Convert and TXA contents of HL
	LD   (HL),A	; Store character into memory
	INC  HL		; Next location
	JR   A2MEM1	; Continue getting characters until user breaks out
 
;------------------------------------------------------------------------------
; Type ASCII from memory command
;  Modified to only print ASCII characters or "."
;------------------------------------------------------------------------------
TYPEMEM:
	CALL GETXY	; ENTRY POINT FOR <T>ype Ascii from memory	
	RET  C               		
	CALL TXCRLF	; TXCRLF	
	LD   A,E		; Clean up starting address
	AND  #0xF0		; Strip off lower nybble
	LD   E,A		; Store it back
	LD   A,L		; Round up L
	OR   #0x0F		; Set lower nybble
	LD   L,A		; Store it back
TYPMEM0:
	LD   A,D		; Print HL contents AAAA:
	CALL AOUT		; Print it
	LD   A,E		; Lower
	CALL AOUT		; Print it
	LD   A,#":"	; Colon
	RST  0x08		; Send that
	LD   A,#0x20	; Space
	RST  0x08		; Print that, too
	LD   B,#0x10	; 0-F bytes
TYPMEM1:
	LD   A,(DE)	; Load the Accum from location at DE
	CP   #0x20		; Printable characters should run from $20 (space)
	JP   M,TYPMEM2	; It's a control character, just print '.' instead
	
	CP   #0x7E		; $7E ASCII character set, ok to print it
	JP   M,TYPMEM3	; Otherwise, just
	
TYPMEM2:
	LD   A,#"."	; Print a "." instead.
TYPMEM3:
	RST  0x08		; And TXA it.               		
	INC  DE		; Increment pointer
	DJNZ TYPMEM1	; Continue until line is done
	
	CALL TXCRLF	; New line
	CALL CPHLDE	; Check if HL=DE	
	RET  Z		; Finished if at ending address
	RET  C		; We went beyond the ending address DE>HL
	LD   A,H		; Is HL rolled over to $0000?
	OR   L		; Test Z flag
	RET  Z		; Finished if rolled past
	RET  Z		; Exit when finished
	CALL CKSIOA	; Check SIO ready status	
	RET  C		; Exit if user has hit a key
	JR   TYPMEM0	; Otherwise, continue on until finished	
;------------------------------------------------------------------------------
; Register command
;------------------------------------------------------------------------------
DSPLREG:
	CALL TXCRLF	; CR,LF
	LD   HL,#RTXT_A	; "A ="
	CALL PRT		; Print it
	LD   A,(0xFFE3)	; Get A contents
	CALL AOUT		; Print it
	
	LD   HL,#RTXT_BC	; " BC ="
	CALL PRT		; Print it
	LD   HL,(0xFFE0)	; Get HL contents from BC storage
	CALL HLOUT	; Convert and print those contents
	
	LD   HL,#RTXT_DE	; " DE =";
	CALL PRT		; Print it
	LD   HL,(0xFFDE)	; Get HL contents from DE storage
	CALL HLOUT	; Print it
	
	LD   HL,#RTXT_HL	; " HL ="
	CALL PRT		; Print it
	LD   HL,(0xFFFC)	; Get HL contents from HL storage
	CALL HLOUT	; Print it
	
	LD   DE,#0xFFE2	; Main flag register storage
	CALL DSPLFLG	; Display flags
	
	CALL TXCRLF	; Next line
	
	LD   HL,#RTXT_AP	; "A'="
	CALL PRT		; Print it
	LD   A,(0xFFD7)	; Get A contents
	CALL AOUT		; Print it
	
	LD   HL,#RTXT_BCP	; " BC'="
	CALL PRT		; Print it
	LD   HL,(0xFFD4)	; Get HL contents from BC storage
	CALL HLOUT	; Convert and print those contents
	
	LD   HL,#RTXT_DEP	; " DE'=";
	CALL PRT		; Print it
	LD   HL,(0xFFD2)	; Get HL contents from DE storage
	CALL HLOUT	; Print it
	
	LD   HL,#RTXT_HLP	; " HL'="
	CALL PRT		; Print it
	LD   HL,(0xFFD0)	; Get HL contents from HL storage
	CALL HLOUT	; Print it
	
	LD   DE,#0xFFD6	; Main flag register storage
	CALL DSPLFLG	; Display flags
	
	CALL TXCRLF	; NEXT LINE
	
	LD   HL,#RTXT_PC	; "PC="
	CALL PRT		; Print it
	LD   HL,(0xFFE4)	; Get PC storage contents
	CALL HLOUT	; Print it
	
	LD   HL,#RTXT_IX	; " IX="
	CALL PRT		; Print it
	LD   HL,(0xFFDA)	; IX contents
	CALL HLOUT	; Print it
	
	LD   HL,#RTXT_IY	; " IY="
	CALL PRT		; Print it
	LD   HL,(0xFFD8)	; IY contents
	CALL HLOUT	; PRint it
	
	CALL TXCRLF	; New line
	RET
;------------------------------------------------------------------------------
; Display flag status
;------------------------------------------------------------------------------
DSPLFLG:
	LD   HL,#RTXT_S	; " S"
	CALL PRT		; Print it
	LD   A,(DE)	; Get Flag byte
	BIT  7,A		; Test sign
	CALL FLG1_0	; Print it
	
	LD   HL,#RTXT_Z	; " Z"
	CALL PRT		; Print it
	LD   A,(DE)	; Get Flag byte
	BIT  6,A		; Test Z sign
	CALL FLG1_0	; Print it
	
	LD   HL,#RTXT_H	; " H"
	CALL PRT		; Print it
	LD   A,(DE)	; Get Flag byte
	BIT  4,A		; Test H sign
	CALL FLG1_0	; Print it
	
	LD   HL,#RTXT_PV	; " P/V"
	CALL PRT		; Print it
	LD   A,(DE)	; Get Flag byte
	BIT  2,A		; Test H sign
	CALL FLG1_0	; Print it
	
	LD   HL,#RTXT_N	; " N"
	CALL PRT		; Print it
	LD   A,(DE)	; Get Flag byte
	BIT  1,A		; Test H sign
	CALL FLG1_0	; Print it
	
	LD   HL,#RTXT_C	; " C"
	CALL PRT		; Print it
	LD   A,(DE)	; Get Flag byte
	BIT  0,A		; Test H sign
	CALL FLG1_0	; Print it
	
	RET
;------------------------------------------------------------------------------	
FLG1_0:	JR   Z,FLG0	; It's zero
	LD   A,#"1"	; Else it's one
FLGPRT:	RST  0x08		; Print it
	RET		; and return
FLG0:	LD   A,#"0"	; Print a "0"
	JR   FLGPRT	; Finish it
;------------------------------------------------------------------------------	

;--------------------------------------------------------------------------------------
; Characters for <R>egister command to print,
;  leave this for Edit register command
;--------------------------------------------------------------------------------------
REGTXT:	.ascii	" PC=X  "
	.ascii	"A=   "
	.ascii	"F=   "
	.ascii	"B=   "
	.ascii	"C=   "
	.ascii	"D=   "
	.ascii	"E=  "
	.ascii	"HL=X "
	.ascii	"IX=X "
	.ascii	"IY=X"
	.BYTE	0xFF	; TERMINATING CHARACTER

REGTXTX:.BYTE	0x20
	.ascii	"A'="
	.BYTE	0x20,0x20
	.ascii	"F'="
	.BYTE	0x20,0x20
	.ascii	"B'="
	.BYTE	0x20,0x20
	.ascii	"C'="
	.BYTE	0x20,0x20
	.ascii	"D'="
	.BYTE	0x20,0x20
	.ascii	"E'="
	.BYTE	0x20
	.ascii	"HL'=X"
	.BYTE	0xFF	; TERMINATING CHARACTER
;--------------------------------------------------------------------------------------		
DREG1:	LD   C,#0x04	; Print Register Text 4 bytes at a time
DREG2:	LD   A,(DE)	; Load text at (DE)	
	INC  DE		; Get next location	
	RST  0x08		; Print current text byte	
	DEC  C		; Decrement the counter	
	JR   NZ,DREG2	; and Repeat until all 4 bytes are printed	
	DEC  HL		; Decrement the variable space pointer	
	LD   A,(HL)	; Load it's contents in the accumulator	
	CALL AOUT		; Convert A to ascii in BC and print it
	LD   A,(DE)	; Get next text string of registers	
	INC  DE		; Update the location	
	CP   #0x58		; An "X" indicates 2 byte value (HL,DE,IX,IY)
	RET  NZ		; Return if only 1 byte value needed	
	DEC  HL		; Else get pointer to next byte	
	LD   A,(HL)	; Load it into A	
	JP   AOUT		; Convert it to ASCII and print it.
		
DREG3:	CALL ECHO		; Continue <R> command, loop in/out a char <enter> or <'>

	LD   HL,#0x001A	; HL contains user storage area for registers
	ADD  HL,SP	; and DE contains the text for that register
	LD   DE,#REGTXT	; REGTXT starts text for Primary registers
	CP   #0x27		; Has user hit <'> for the Alternate Registers?
	RET  NZ		; If not, we've got the right ones then
	LD   HL,#0x000C	; Move pointer not quite so far for alternate registers
	ADD  HL,SP	; Stack plus the new start of registers
	LD   DE,#REGTXTX	; REGTXTX starts text for Alternate registers
	RET
;------------------------------------------------------------------------------
; Edit registers
;------------------------------------------------------------------------------
EDITREG:
	CALL DREG3	; ENTRY POINT FOR <E>dit Registers
L021B:	CALL TXCRLF	; TXCRLF
	LD   A,(DE)	; Load variable space info (user registers)
	CP   #0xFF		; Reached the end of the text yet?
	RET  Z		; Quit when finished.
	CALL DREG1	; Print register text 4 bytes at a time	
	CALL COLON	; Print "/ " + contents of BC
	PUSH HL		; Store register value location
	CALL GETHL	; Get 4 characters into HL
	PUSH HL		; and move it into
	POP  BC             ; B and C
	POP  HL		; Get original register value location
	PUSH AF		; Save last character user sent
	JR   NC,L023C	; Store the new data there
		
L0232:	POP  AF		; Get the last user character back
	CP   #0x20		; Was it a <space>?	
	JR   Z,L021B	; Get set up to do the next registers          		
	CP   #0x0A		; <LF>?
	JR   Z,L021B	; Space or LF continues on to edit other
	RET		; registers, otherwise we are finished
		
L023C:	LD   (HL),C	; Store new register info to memory
	DEC  DE		; Where it will be picked up for a Go command
	LD   A,(DE)	; Store it there
	INC  DE		; Get next location if needed for 16 bit registers
	CP   #0x58		; Is it "X" ?	
	JR   NZ,L0232	; Only 8 bits needed for basic reg (a,b,c,d,e)
	INC  HL		; Must be 16 bit register set (HL,SP,IX,IY)
	LD   (HL),B	; so write that byte also
	DEC  HL		; Set up for the next location
	JR   L0232	; And see what the user wants to do next
;------------------------------------------------------------------------------
; Move command
;------------------------------------------------------------------------------	
MOVMEM:	CALL GETXYZ	; ENTRY POINT FOR <M>ove, Get xxxx,yyyy, create BC, zzzz dest
	RET  C		; but exit if he chickens out <ctrl-c>
	POP  IX		; Value is $0072, pushed before user hit a menu key
	LDIR		; Make the big move
	JP   (IX)		; and go back to normal monitor entry point.
;------------------------------------------------------------------------------
; Fill command		
;------------------------------------------------------------------------------
MEMFILL:
	CALL GETXYZ	; ENTRY POINT FOR <F>ill, Get xxxx,yyyy, create BC, zz contents
	RET  C		; but exit if he changes his mind with <ctrl-c>
	POP  IX		; Get $0072 for normal monitor entry point.
L0259:	LD   (HL),E	; Load new fill value into destination memory
	INC  HL		; Increment to the next location
	DEC  BC		; Count down.
	LD   A,B		; Check BC to see if it's zero yet
	OR   C		
	JR   NZ,L0259	; Check to see if we're finished, else loop back.
	JP   (IX)		; Jump back to monitor normal entry point.
;------------------------------------------------------------------------------
; Compare command
;------------------------------------------------------------------------------	
MCOMP:	CALL GETXYZ	; ENTRY POINT FOR <C>ompare, Get xxxx,yyyy, create BC, zzzz other
	RET  C		; May quit and go home early today
MCOMP1:	LD   A,(DE)	; Get the byte from memory	
	CP   (HL)		; Check it against the other location
	JP   NZ,PRTMEM	; Jump out if something's not right
	INC  DE		; Otherwise proceed as normal, get next target
	INC  HL		; and the next source
	DEC  BC		; take one off the byte counter (BC)
	LD   A,B		; Check the byte counter to see where we are
	OR   C		; See if we're done yet
	JR   NZ,MCOMP1	; Continue the compare until we get all the way done
	RET		; If we are, we can RETurn home
;------------------------------------------------------------------------------
; UNLOAD - Clear out all ram
;------------------------------------------------------------------------------
UNLOAD:	XOR  A		; Zeroize Accum
	LD   HL,#0x3000	; Start of static ram
	LD   DE,#0x3001	; Next location
	LD   BC,#0x0800	; ALL static ram
	LD   (HL),A	; Store the zero
	LDIR		; Move it into all locations
	
	LD   HL,#0x4010	; Start of program ram
	LD   DE,#0x4011	; Next location
	LD   BC,#0xBFEF	; $FFFF-$4010
	LD   (HL),A	; Store the zero
	LDIR		; Clear it all out
	RET

;------------------------------------------------------------------------------

;                        S U B R O U T I N E S

;------------------------------------------------------------------------------

TESTM:
	PUSH HL
	PUSH AF
	
	LD   HL,#TESTMSG	; Print a message
	CALL PRTSTR
	POP AF
	POP HL
	RET
	
;------------------------------------------------------------------------------
; Print contents of HL registers, plus ": "
;   and the contents of memory at location (HL)
;------------------------------------------------------------------------------
PRTMEM:	CALL TXCRLF	; TXCRLF
	CALL HLCOLN	; Print HL contents + ": "
	LD   A,(HL)	; Get Memory contents at (HL)
	JR   AOUT		; Convert A to Ascii in BC and Print it	
		
HLCOLN:	CALL HLOUT	; Convert and Print Contents of HL
COLON:	LD   BC,#0x3A20	; Load ": " in BC

	JR   BCOUT	; and TX	it out.
;------------------------------------------------------------------------------		
PRTSGN:	LD   HL,#SIGNON	; Print SIGNON message
PRTMSG:	CALL TXCRLF	; TXCRLF
;------------------------------------------------------------------------------
; Print string of characters to Serial A until byte=$00, WITH CR, LF
;------------------------------------------------------------------------------
PRT:	LD   A,(HL)	; Get character
	OR   A		; Is it $00 ?
	RET  Z		; Then RETurn on terminator
	
	RST  0x08		; Print to TTY
	
	INC  HL		; Next Character
	JR   PRT		; Continue until $00 is hit
;------------------------------------------------------------------------------
; Print string of characters to Serial A until byte=$00, WITH CR, LF
;------------------------------------------------------------------------------
PRTSTR:	LD   A,(HL)	; Get character
	OR   A		; Is it $00 ?
	JR   Z,TXCRLF	; Then TXCRLF and RETurn
	
	RST  0x08		; Print to TTY
	
	INC  HL		; Next Character
	JR   PRTSTR	; Continue until $00 is hit
; HLOUT - Convert and transmit contents of HL registers
;------------------------------------------------------------------------------
HLOUT:	LD   A,H		; Convert and TXA contents of H and L
	CALL AOUT		; Convert H into ASCII BC and print it
	LD   A,L		; Convert L
AOUT:	CALL ATOBC	; Convert byte to ASCII
	JR   BCOUT	; Send B and C to TXA and use that RETurn
;------------------------------------------------------------------------------
; TXCRLF - Transmit a CR and LF character, new line
;------------------------------------------------------------------------------
TXCRLF:	LD   BC,#0x0D0A	; Entry point for "TXCRLF" $0D=CR, $0A=LF
;------------------------------------------------------------------------------
; BCOUT - Ascii characters in BC are transmitted
;------------------------------------------------------------------------------
BCOUT:	LD   A,B		; Entry point to print ascii chars in B and C
	RST  0x08		; Print character from B
	LD   A,C		; Get character from C
	RST  0x08		; Print character from C
	RET
;------------------------------------------------------------------------------
; This is a good routine to set up any move, fill, or LDIR command for the user
;------------------------------------------------------------------------------
GETXYZ:	CALL GETXY	; GET xxxx,yyyy Start and End address + Create Byte Count
	RET  C		; <Ctrl-C> exits
	SBC  HL,DE	; Create a byte count for this op
	INC  HL		; Correct for the subtraction
	PUSH HL		; and save it temporarily
	CALL GETX		; GET zzzz From User (destination or value)
	POP  BC		; Put byte counter in BC
	EX   DE,HL	; Flip so that Start=HL, End=DE
	RET		; and Byte Count=BC
;------------------------------------------------------------------------------
; Gets four Hex characters from the console, converts them to values in HL
;------------------------------------------------------------------------------
GETHL:	LD   HL,#0x0000	; Gets xxxx but sets Carry Flag on any Terminator
	CALL ECHO		; RX a Character
	CP   #0x20		; Did he give us some <space>?
	JR   Z,SETCY	; Set the carry and quit
	CP   #0x0A		; Did he feed us a linefeed <LF>?
	JR   Z,SETCY	; Set the carry and quit
	CP   #0x0D		; Is he trying to return his carriage <CR>?
	JR   NZ,GETX2	; Hmm must be some other key		
SETCY:	SCF		; Set Carry Flag
	RET                 ; and Return to main program		
;------------------------------------------------------------------------------
; This routine converts last four hex characters (0-9 A-F) user types into a value in HL
; Rotates the old out and replaces with the new until the user hits a terminating character
;------------------------------------------------------------------------------
GETX:	LD   HL,#0x0000	; CLEAR HL
GETX1:	CALL ECHO		; RX a character from the console
	CP   #0x20		; Is he <spc>'d out? Set the zero and
	RET  Z		; Get the heck out of Dodge
	CP   #0x0A		; Feeding us that old line again? <LF>?
	RET  Z		; Quit and go home
	CP   #0x0D		; Must be the carriage returning <CR>
	RET  Z		; Same thing again
	CP   #0x2C		; <,> can be used to safely quit for multiple entries
	RET  Z		; (Like filling both DE and HL from the user)
GETX2:	CP   #0x03		; Likewise, a <ctrl-C> will terminate clean, too, but
	JR   Z,SETCY	; It also sets the Carry Flag for testing later.
	ADD  HL,HL	; Otherwise, rotate the previous low nybble to high
	ADD  HL,HL	; rather slowly
	ADD  HL,HL	; until we get to the top
	ADD  HL,HL	; and then we can continue on.
	SUB  #0x30		; Convert ASCII to byte	value
	CP   #0x0A		; Are we in the 0-9 range?
	JR   C,GETX3	; Then we just need to sub $30, but if it is A-F
	SUB  #0x07		; We need to take off 7 more to get the value down to
GETX3:	AND  #0x0F		; to the right hex value
	ADD  A,L		; Add the high nybble to the low
	LD   L,A		; Move the byte back to A
	JR   GETX1		; and go back for next character until he terminates
;------------------------------------------------------------------------------	
; This routine is called many times for routines needing the user to enter a
; Starting address of four hex characters in DE and an ending address in HL
; "Get xxxx (DE), yyyy (HL)"
;------------------------------------------------------------------------------
GETXY:	CALL GETX	; Get the first four hex characters into HL
	RET  C		; But break out if he terminates
	EX   DE,HL	; Put it in DE, go back again for yyyy
	JR   GETX	; and get four more chars to put in HL.		
;------------------------------------------------------------------------------
; Convert ASCII characters in B C registers to a byte value in A
;------------------------------------------------------------------------------
BCTOA:	LD   A,B		; Move the hi order byte to A
	SUB  #0x30		; Take it down from Ascii
	CP   #0x0A		; Are we in the 0-9 range here?
	JR   C,BCTOA1	; If so, get the next nybble
	SUB  #0x07		; But if A-F, take it down some more
BCTOA1:	RLCA		; Rotate the nybble from low to high
	RLCA		; One bit at a time
	RLCA		; Until we
	RLCA		; Get there with it
	LD   B,A		; Save the converted high nybble
	LD   A,C		; Now get the low order byte
	SUB  #0x30		; Convert it down from Ascii
	CP   #0x0A		; 0-9 at this point?
	JR   C,BCTOA2	; Good enough then, but
	SUB  #0x07		; Take off 7 more if it's A-F
BCTOA2:	ADD  A,B		; Add in the high order nybble
	RET
;------------------------------------------------------------------------------
; Convert single byte in A to two Ascii characters in B and C
;------------------------------------------------------------------------------
ATOBC:	LD   B,A		; Save the byte original value
	AND  #0x0F		; Strip off the high order nybble
	CP   #0x0A		; 0-9 at this point?
	JR   C,ATOBC1	; We only need to add $30 then, or
	ADD  A,#0x07	; Seven more if it's in the A-F range
ATOBC1:	ADD  A,#0x30	; Take the value on up to ASCII
	LD   C,A		; Store the converted lo order character to C
	LD   A,B		; Get the original value back
	RRCA		; Rotate it to the right
	RRCA
	RRCA
	RRCA
	AND  #0x0F		; Mask off the upper trash
	CP   #0x0A		; 0-9 (< $0A)?
	JR   C,ATOBC2	; Only add $30 then
	ADD  A,#0x07	; And add it 7 if it's in the A-F range
ATOBC2:	ADD  A,#0x30	; Finish the conversion to ASCII
	LD   B,A		; Load the finished hi order character to B
	RET		; A is trashed, but the ASCII representation is in BC
;------------------------------------------------------------------------------
; CPHLDE
;------------------------------------------------------------------------------
CPHLDE:	LD   A,H		; Compares HL=DE, returns Zero flag if =	
	CP   D
	RET  NZ		; if high order is out, no use testing low	
	LD   A,L
	CP   E
	RET		; returns Zero flag if =
;------------------------------------------------------------------------------
TXSPC:	LD   A,#0x20	; TX a <spc> out the RS232	
	JP   RST08
;------------------------------------------------------------------------------
ECHO:	RST  0x10		; Loops a character in, RX Char	
	JP   RST08	; then TX it out, get RET at 000F		
;-------------------------------------------------------------------------------------
; LOAD Intel Hex format file from the console.
; [Intel Hex Format is:
; 1) Colon (Frame 0)
; 2) Record Length Field (Frames 1 and 2)
; 3) Load Address Field (Frames 3,4,5,6)
; 4) Record Type Field (Frames 7 and 8)
; 5) Data Field (Frames 9 to 9+2*(Record Length)-1
; 6) Checksum Field - Sum of all byte values from Record Length to and 
;   including Checksum Field = 0 ]
;------------------------------------------------------------------------------	
LOAD:	CALL TXCRLF	; TXCRLF
LOAD0:	CALL GETCHR	; See if user types <ctrl-c>, or <spc>, else RET
	CP   #0x03		; User must have chickened <ctrl-c> out	
	RET  Z		; Return him to the prompt
LOAD1:	CP   #0x3A		; Colon <:>? Start of line of Intel Hex file
	JR   NZ,LOADERR	; If not, must be error, Tell him and quit
	LD   E,#0		; First two Characters is the Record Length Field
	CALL GET2		; Get us two characters into BC, convert it to a byte <A>
	LD   D,A		; Load Record Length count into D
	CALL GET2		; Get next two characters, Memory Load Address <H>
	LD   H,A		; put value in H register.
	CALL GET2		; Get next two characters, Memory Load Address <L>
	LD   L,A		; put value in L register.
	CALL GET2		; Get next two characters, Record Field Type
	CP   #0x01		; Record Field Type 00 is Data, 01 is End of File
	JR   NZ,LOAD2	; Must be the end of that file
	CALL GET2		; Get next two characters, assemble into byte
	LD   A,E		; Recall the Checksum byte
	AND  A		; Is it Zero?
	RET  Z		; Must be o.k., go back for some more, else
	JR   LOADERR	; Checksums don't add up, Error out
		
LOAD2:	LD   A,D		; Retrieve line character counter	
	AND  A		; Are we done with this line?
	JR   Z,LOAD3	; Get two more ascii characters, build a byte and checksum

	CALL GET2	; Get next two chars, convert to byte in A, checksum it
	LD   (HL),A	; Checksum OK, move converted byte in A to memory location
	INC  HL		; Increment pointer to next memory location	
	DEC  D		; Decrement line character counter
	JR   LOAD2	; and keep loading into memory until line is complete
		
LOAD3:	CALL GET2	; Get two chars, build byte and checksum
	LD   A,E		; Check the checksum value
	AND  A		; Is it zero?
	JR   Z,LOAD0	; If the checksum is still ok, continue on, else
LOADERR:
	LD   HL,#CKSUMERR	; Get "Checksum Error" message
	JP   PRTMSG	; Print Message from (HL) and terminate the load
;------------------------------------------------------------------------------
; Get a character from the console, must be $20-$7F to be valid (no control characters)
; <Ctrl-c> breaks with the Zero Flag set
;------------------------------------------------------------------------------	
GETCHR:	RST  0x10		; RX a Character
	CP   #0x03		; <ctrl-c> User break?
	RET  Z			
	CP   #0x20		; <space> or better?
	JR   C,GETCHR	; Do it again until we get something usable
	RET
;------------------------------------------------------------------------------
; Gets two ASCII characters from the console (assuming them to be HEX 0-9 A-F)
; Moves them into B and C, converts them into a byte value in A and updates a
; Checksum value in E
;------------------------------------------------------------------------------
GET2:	CALL GETCHR	; Get us a valid character to work with
	LD   B,A		; Load it in B
	CALL GETCHR	; Get us another character
	LD   C,A		; load it in C
	CALL BCTOA	; Convert ASCII to byte
	LD   C,A		; Build the checksum
	LD   A,E
	SUB  C		; The checksum should always equal zero when checked
	LD   E,A		; Save the checksum back where it came from
	LD   A,C		; Retrieve the byte and go back
	RET


;------------------------------------------------------------------------------
; Time Delay Routine
;    Sets up an initial 60uSec delay, adds 20uSec for each value of DE
;    Min value for DE=$0001
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
TDLY:	DI			; 20uSec First Part
	PUSH	AF		; Save A & Flags
	PUSH	DE		; Preserves Count for subsequent
	NOP			;  routines to use
	NOP			;   stall
	NOP			;    stall

TLP:	DEC	DE		; 20uSec for Each Iteration of this blk
	BIT	0,A		;   stall
	BIT	0,A		;    stall
	LD	A,D		; Test DE to see if = zero
	OR	E		
	JP	NZ,TLP		; Loop until DE=0
	
	DEC	DE		; 20uSec Exit Portion
	POP	DE		; Retrieve Intial DE Value
	POP	AF		; And AF status
	EI			; Enable future INTerrupts
	RET
;--------------------------------------------------------------------------------------
; ANSI ESCAPE SEQUENCES
;--------------------------------------------------------------------------------------
; ESC[yy;xxH  - Sets cursor position at xx (0-79) and yy (0-23)
; ESC[nnA     - Moves cursor up nn rows
; ESC[nnB     - Moves cursor down nn rows
; ESC[nnC     - Moves cursor forward nn columns
; ESC[nnD     - Moves cursor backward nn columns
; ESC[2J      - Clears screen and homes cursor
; ESC[xx;xxm  - Where xx is parameters according to:
; 0 - All attributes off
; 1 - Bold on
; 4 - Underscore on
; 5 - Blink on
; 7 - Reverse video on
; Text color $30 BLK, $31 RED, $32 GRN, $33 YEL, $34 BLU, $35 VIO, $36 CYN, $37 WHT
; Bkgd color $40 BLK, $41 RED, $42 GRN, $43 YEL, $44 BLU, $45 VIO, $46 CYN, $47 WHT
;--------------------------------------------------------------------------------------
CONCLS:	CALL	BGRN		; BLUE BACKGROUND
	CALL	CSI		; Control Sequence Introducer (esc[)
	LD	A,#"2"		; CLEAR THE SCREEN AND HOME THE CURSOR
	RST	0x08
	LD	A,#"J"
	RST	0x08
	RET			; END THIS TEXT		
;--------------------------------------------------------------------------------------	
; SET COLORS ON ANSI SCREEN	
;--------------------------------------------------------------------------------------
TNRML:	XOR	A		; SET $00
	JR	COLOR
TDIM:	LD	A,#0x02		; DIM COLORS
	JR	COLOR
TBRT:	LD	A,#0x01		; BRIGHT COLORS
	JR	COLOR
TUSCR:	LD	A,#0x04		; UNDERSCORE
	JR	COLOR
TBLNK:	LD	A,#0x05		; BLINK
	JR	COLOR
TREV:	LD	A,#0x07		; REVERSE
	JR	COLOR				
BBLK:	LD	A,#0x40		; BLK BACKGROUND
	JR	COLOR
BRED:	LD	A,#0x41		; RED BACKGROUND
	JR	COLOR
BGRN:	LD	A,#0x42		; GREEN BACKGROUND
	JR	COLOR
BYEL:	LD	A,#0x43		; YELLOW BACKGROUND
	JR	COLOR
BBLU:	LD	A,#0x44		; BLUE BACKGROUND
	JR	COLOR
BVIO:	LD	A,#0x45		; VIOLET BACKGROUND
	JR	COLOR
BCYN:	LD	A,#0x46		; CYAN BACKGROUND
	JR	COLOR
BWHT:	LD	A,#0x47		; WHITE BACKGROUND
	JR	COLOR						
TBLK:	LD	A,#0x30		; BLK TEXT
	JR	COLOR
TRED:	LD	A,#0x31		; RED TEXT
	JR	COLOR
TGRN:	LD	A,#0x32		; GRN TEXT
	JR	COLOR
TYEL:	LD	A,#0x33		; YEL TEXT
	JR	COLOR
TBLU:	LD	A,#0x34		; BLU TEXT
	JR	COLOR
TVIO:	LD	A,#0x35		; VIO TEXT
	JR	COLOR
TCYN:	LD	A,#0x36		; CYN TEXT
	JR	COLOR
TWHT:	LD	A,#0x37		; WHT TEXT

COLOR:	PUSH	BC		; SAVE BC CONTENTS
	CALL	ATOBC		; CONVERT TO ASCII
	CALL	CSI		; SEND THE INTRODUCER CODE
	CALL	BCOUT		; SEND THE COLOR CODES
	LD	A,#"m"		; CLOSE THE SEQUENCE
	RST	0x08		; SEND IT
	POP	BC		; RETRIEVE BC
	RET			; AND FINISH
;--------------------------------------------------------------------------------------	
CSI:	LD	A,#0x1B		; ESCAPE
	RST	0x08
	LD	A,#0x5B		; " [ "
	RST	0x08
	RET
;------------------------------------------------------------------------------		
; PRINT - Prints a character to all available devices
;  Checks IOFLAG at $400E in non-volatile ram for status of devices
;  D0- TTYA, D1-TTYB (MIDI), D2-LCD Display [we are going to print ttya always]
;------------------------------------------------------------------------------
PRINT:	LD	(CHAR),A		; Store character temporarily
	LD	A,(IOFLAG)	; Check I/O Flag
	AND	#0x07		; See if any output is enabled?
	JR	NZ,PRINT0		; Something is enabled, which one
	OR	#0x01		; Set Serial A enabled if nothing is
	LD	(IOFLAG),A	; And store it there
	
PRINT0:	LD	A,(IOFLAG)	; Get I/O Flag word
	AND	#0x01		; Is Serial A enabled?
	JR	Z,PRINT1		; No, but something else is	
	RST	0x08		; Print it to Serial A

PRINT1:	LD	A,(IOFLAG)	; Get I/O Flag word
	AND	#0x02		; Is Serial B enabled?
	JR	Z,PRINT2		; No, jump over
	LD	A,(CHAR)		; Yes, Retrieve the character
	CALL	TXB		; And print it to Serial B

; Handle LCD special characters
PRINT2:	LD	A,(IOFLAG)	; Get I/O Flag word
	AND	#0x04		; Is the LCD Enabled?
	RET	Z		; If no, then we are finished here
	LD	A,(CHAR)		; Else, retrieve the character
	CP	#0x0D		; Carriage Return
	JP	Z,LCDHOME		; CR=HOME for now
	CP	#0x0A		; Line Feed
	JP	Z,LCDHOME		; LF=HOME for now
	
	CP	#0x0C		; FF=CLS
	JP	Z,LCDCLS		; Clear takes 1.64mSec, may see delay
	CP	#0x08		; Backspace
	JR	NZ,PRINT3		; Must be normal ASCII char
BKSPC:	IN	A,(#0x7C)		; Get DDRAM address
	OR	A		; See if zero
	RET	Z		; Already at home
	DEC	A		; Reduce address by one
	JP	LCDSETP		; Set the new position
PRINT3:	CALL	LCDCHAR		; Print it to the LCD Display
	RET			; Finished
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; RXA - Receive a byte over SIO/0 Ch A
;------------------------------------------------------------------------------
RXA:	CALL	CKSIOA		; Get the status word
	JR	NC,RXA		; Loop until a character arrives
	IN	A,(RBR)		; Get the character
	RET			; Char ready in A
;------------------------------------------------------------------------------
; TXA - Transmit a byte over SIO/0 Ch A
;------------------------------------------------------------------------------
TXA:	PUSH	AF		; Store character
	CALL	CKSIOA		; See if SIO channel A is finished transmitting
	JR	Z,TXA+1		; Loop until SIO flag signals ready
	POP	AF		; Retrieve character
	OUT	(THR),A		; Output the character
	RET
;------------------------------------------------------------------------------
; RXB - Receive a byte over SIO/0 Ch B
;------------------------------------------------------------------------------
RXB:	CALL	CKSIOB		; Get the status word
	JR	NC,RXB		; Loop until character arrives
	IN	A,(#0x75)		; Get the character
	RET			; Char ready in A	
;------------------------------------------------------------------------------
; TXB - Transmit a byte over SIO/0 Ch B
;------------------------------------------------------------------------------
TXB:	PUSH	AF		; Store character
	CALL	CKSIOB		; See if SIO channel B is finished transmitting
	JR	Z,TXB+1		; Loop until SIO flag signals ready
	POP	AF		; Retrieve character
	OUT	(#0x75),A		; Output the character
	RET
;------------------------------------------------------------------------------
; Check SIO Channel A status flag, RX char ready=CY, TX buffer clear=NZ
;------------------------------------------------------------------------------
CKSIOA:	IN	a,(LSR)		;Retrieve Status Word
	RRCA			;RX status into CY flag
	BIT	4,A		;TX Buffer Empty into Z flag
	RET
	
;	XOR	A		; Zeroize A
;	OUT	(#0x76),A		; Select Register 0
;	IN	A,(#0x76)		;Retrieve Status Word
;	RRCA			;RX status into CY flag
;	BIT	1,A		;TX Buffer Empty into Z flag
;	RET
;------------------------------------------------------------------------------
; Check SIO Channel B status flag, RX char ready=CY, TX buffer clear=NZ
;------------------------------------------------------------------------------
CKSIOB:	XOR	A		; Zeroize A
	OUT	(#0x77),A		; Select Register 0
	IN	A,(#0x77)		; Retrieve Status Word
	RRCA			; RX status into CY flag
	BIT	1,A		; TX Buffer Empty into Z flag
	RET
; Write character to 8279 Display, check for Display Available before write.
;------------------------------------------------------------------------------
TX8279:	PUSH	AF		; Save char and flags
	IN	A,(#0x79)		; Get 8279 status word
	BIT	7,A		; D7=Display Unavailable flag
	JR	NZ,TX8279+1	; Loop here until Display Available
	POP	AF		; Retrieve char and flags
	OUT	(#0x78),A		; Write char to display
	RET
;------------------------------------------------------------------------------
; Wait on 8279 for Keyboard character
;------------------------------------------------------------------------------
RX8279:	CALL	CK8279		; Get 8279 status, Z if queue empty
	JR	Z,RX8279		; Loop until key is down
	IN	A,(#0x78)		; Get the key value
	RET			; And return
;------------------------------------------------------------------------------
; Check 8279 for Keyboard character, Return Z set if nothing
;------------------------------------------------------------------------------
CK8279:	IN	A,(#0x79)		; Get 8279 status word
	AND	#0x07		; # of chars in buffer
	RET			; Return Z set if no characters

;------------------------------------------------------------------------------
;                         L C D ROUTINES
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; LCD Display - Write a Character
;------------------------------------------------------------------------------
LCDCHAR:
	PUSH	AF		; Save the character
	CALL	LCDBUSY		; Wait for the LCD to clear
	POP	AF		; Retrieve the character
	OUT	(#0x7D),A		; Output it
	RET			
;------------------------------------------------------------------------------
; LCD Display - Clear the screen
;------------------------------------------------------------------------------
LCDCLS:	LD	A,#0x01		; 1.64mSec to clear
	JR	LCDCMD		; Wait for LCD to perform and return
;------------------------------------------------------------------------------
; LCD Home Cursor - Command $02 takes 1.64mSec, or reposition takes 40uSec
;------------------------------------------------------------------------------
LCDHOME:
	XOR	A		; Set cursor position to zero
;------------------------------------------------------------------------------
; LCD Set Cursor Position - Range is $00-$27, $40-$67, or with $80 to set
;------------------------------------------------------------------------------
LCDSETP:
	OR	#0x80		; Set D7 to set DDRAM address, continue
;------------------------------------------------------------------------------
LCDCMD:	PUSH	AF		; Save the command
	CALL	LCDBUSY		; See if the LCD is still in use
	POP	AF
	OUT	(#0x7C),A		; Write a command to the LCD register
	RET
;------------------------------------------------------------------------------
LCDBUSY:
	IN	A,(#0x7C)		; Get status word
	AND	#0x80		; Strip off all but Busy Flag BF
	JR	NZ,LCDBUSY	; Wait until it finishes
	RET
;------------------------------------------------------------------------------
LCDGETS:
	IN	A,(#0x7C)		; Retrieves Status Word from LCD
	RET
;------------------------------------------------------------------------------
LCDGETD:
	CALL	LCDBUSY		; Wait on LCD
	IN	A,(#0x7D)		; Retrieve Data under cursor from LCD
	RET
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; Print the bootup message on the LCD
;------------------------------------------------------------------------------
LCDMSG:	CALL	LCDCLS		; Clear the LCD
	LD	HL,#LCDTXT1	; Point to first line of text
LCDMSG1:
	LD	A,(HL)		; Get character
	OR	A		; Terminator?
	JR	Z,LCDMSG2
	CALL	LCDCHAR		; Write the character
	INC	HL		; Next character
	JR	LCDMSG1		; Loop until line done
LCDMSG2:
	LD	HL,#LCDTXT2	; Point to next line of text
	LD	A,#0x40		; Second line
	CALL	LCDSETP		; Set the position
LCDMSG3:
	LD	A,(HL)		; Get the character
	OR	A		; Terminator?
	JR	Z,LCDMSG4		; Finished
	CALL	LCDCHAR		; Write the character
	INC	HL		; Next char
	JR	LCDMSG3		; Loop until finished		
LCDMSG4:
	RET
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

; INITIALIZE HARDWARE

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; The new PIT settings for this Rev Monitor Rom jacks up CTC0 to generate 
; PIT0=38400 baud, PIT1 to gen 4800 Baud, and PIT2 to generate a MIDI clock.
; - CTC0 and CTC1 must be driven off the 1.8432MHz Xtal, 
; - The MIDI clock must be driven off the 2.0MHz system clock.
;------------------------------------------------------------------------------
; Set the Programmable Interval Timer
;------------------------------------------------------------------------------
INIT:	LD   SP,#0xFFD0		; Set the Stack Pointer
	
INITPIT:
	LD   E,#0x02		; PIT(CTC) # 2	
	LD   HL,#0x0004		; 31,250 Baud (MIDI) x 16= 500,000 Hz (Countdown=4)	
	CALL PIT1			; Write it to PIT
	LD   E,#0x01		; PIT(CTC) # 1	
	LD   HL,#0x0018		; 4800 Baud x 16 = 76,800 Hz (Countdown=24)	
	CALL PIT1			; Write it to PIT
	LD   E,#0x00		; PIT(CTC) # 0	
	LD   HL,#0x0003		; 38400 Baud x 16 = 614,400 Hz (Countdown=3)
	CALL PIT1			; Write it to PIT

UART1_BASE	.EQU	#0x00
RBR	.EQU	UART1_BASE + #0x00
THR	.EQU	UART1_BASE + #0x00
IER	.EQU	UART1_BASE + #0x01
FCR	.EQU	UART1_BASE + #0x02
LCR	.EQU	UART1_BASE + #0x03
MCR	.EQU	UART1_BASE + #0x04
LSR	.EQU	UART1_BASE + #0x05
MSR	.EQU	UART1_BASE + #0x06
DLL	.EQU	UART1_BASE + #0x00
DLM	.EQU	UART1_BASE + #0x01

INIT16550:
	LD	A,#0x80
	OUT	(LCR),A		; set baud divisor latch
	LD	A,#0x00
	OUT	(DLM),A		; set baud dividor
	LD	A,#0x51
	OUT	(DLL),A		; set baud dividor
	LD	A,#0x03
	OUT	(LCR),A		; set divisor latch to 1 and 8 bit word length
	LD	A,#0x00
	OUT	(MCR),A		; set baud dividor

	LD	A,#0x30
	OUT	(THR),A		; set baud dividor
	
;------------------------------------------------------------------------------
; Set the SIO device settings
;------------------------------------------------------------------------------
INITSIO:
	LD   HL,#SIODAT		; Location of SIO control strings
	LD   C,#0x76		; Z80 SIO Port A Ctrl
	LD   B,#0x0A		; Number of control characters           		
	OUTI			; Output it to the port
	NOP			; Brief time delay
	OUTI			; Output the next byte
	NOP			; Again, delay briefly
	OTIR			; Then dump the rest of them to the SIO
;--------------------------------------------------------------------------------------
;INITIALIZE 8279 KEYBOARD/DISPLAY CONTROLLER
;--------------------------------------------------------------------------------------
INITKBD:
	LD	A,#0x01		;DECODED SCAN LINES 1 OF 4, LOW ON SEL
	OUT	(#0x79),A
	LD	A,#0x34		;DIVIDE CLOCK BY 20, 100KHz
	OUT	(#0x79),A
	LD	A,#0xD3		;CLEAR DISPLAY AND FIFO
	OUT	(#0x79),A
	LD	A,#0x40		;LOAD FIFO INTO DATA BUFFER
	OUT	(#0x79),A
;--------------------------------------------------------------------------------------
;INITIALIZE 8255 PIO'S
;--------------------------------------------------------------------------------------
INITPIO:
	LD	A,#0x89		;PORTS A,B=OUT C=INPUT
	OUT	(#0x63),A		;8255 # 1
	OUT	(#0x67),A		;8255 # 2
	OUT	(#0x6B),A		;8255 # 3
;------------------------------------------------------------------------------
; INITIALIZE THE LCD DISPLAY - HD44780 Controller 40 x 2 Display
;------------------------------------------------------------------------------
INITLCD:
	LD	A,#0x38		; ESTABLISH 8 BIT DATA PATHWAY
	LD	DE,#0x00FC		; BF Busy Flag can't be checked until
	OUT	(#0x7C),A		; After Interface function is set with
	CALL	TDLY		; the LCD display
	OUT	(#0x7C),A
	CALL	TDLY
	OUT	(#0x7C),A
	CALL	TDLY
	OUT	(#0x7C),A
	LD	A,#0x06		; Cursor Increments, No Disp. Shift
	OUT	(#0x7C),A
	CALL	LCDBUSY		; Check LCD Status for Busy
	LD	A,#0x0D		; Display On, Cursor Off, Blink On
	OUT	(#0x7C),A
	CALL	LCDBUSY		; Check LCD Status for Busy
	LD	A,#0x14		; Cursor moves, Shifts to the Right
	OUT	(#0x7C),A
	CALL	LCDBUSY		; Check LCD Status for Busy
	LD	A,#0x38		; 8-bit data, 2 Lines, 5x7 font
	OUT	(#0x7C),A
	CALL	LCDBUSY		; Check LCD Status for Busy
	LD	HL,#CGRAM		; Start Custom character map
	LD	A,#0x40		; Set start of CGRAM in LCD
	OUT	(#0x7C),A
INITLCD0:
	CALL	LCDBUSY		; Check LCD Status for Busy
	LD	A,(HL)		; Get byte for custom character
	CP	#0x80		; At end of list?
	JR	Z,INITLCD1	; Continue forward
	OUT	(#0x7D),A		; Write it to CGRAM
	INC	HL		; Next location
	JR	INITLCD0		; Loop thru until all chars output
	
INITLCD1:
	CALL	LCDCLS		; Clear the LCD Display
	
;------------------------------------------------------------------------------		
; Initialize the Screen Last, Run Rom Checksums
;------------------------------------------------------------------------------
INITFIN:
	CALL	LCDMSG		; Put message on the LCD
	CALL	TBRT		; Bright background
	CALL	BBLU		; Set Background to Blue
	CALL	TBRT		; Bright Text colors
	CALL	TWHT		; Bright White
	CALL 	PRTSGN		; Print "Single Board Monitor"
	CALL	TYEL		; Yellow text	
	CALL 	RMCKSM		; Print ROM checksums
	CALL	TWHT		; White text
	CALL	LCDCLS		; Clear the LCD
	JP	NORMAL		; We are finished
;------------------------------------------------------------------------------
; This routine sets up the Programmable Interval Timer, but allows the user to
; Change the divider value to effectively change the Baud rate of the SIO, serves as the
; Entry point for <B> change baud rate, use with care.
;------------------------------------------------------------------------------
CHGBAUD:
	CALL GETXY		; Get xxxx (DE), yyyy (HL)
	RET  C			; User may break out of this command
PIT1:	SUB  A			; Zero out A and clear the flags
	CP   E			; E contains PIT #
	LD   A,#0x36		; Data for PIT control
	LD   C,#0x6C		; PIT Counter 0
	LD   B,E
	JR   Z,PIT3		; Program that particular counter
PIT2:	ADD  A,#0x40		; Next PIT counter
	INC  C			; Next I/O Port address
	DJNZ PIT2            		

PIT3:	OUT  (#0x6F),A		; Output PIT control byte
	OUT  (C),L		; and load the counter low order byte
	OUT  (C),H		; Then the high order byte
	RET 
;------------------------------------------------------------------------------
; Calculate ROM checksums
;------------------------------------------------------------------------------
RMCKSM:	LD	HL,#ROM0		; THIS MONITOR ROM
	CALL	RCKSUM		; CALC THE CHECKSUM
	CALL	TXSPC		; PRINT A SPACE
;--------------------------------------------------------------------------------------	
	LD	HL,#ROM1		; BASIC ONE ROM
	CALL	RCKSUM		; GET THE CHECKSUM
	CALL	TXSPC		; PRINT A SPACE
;--------------------------------------------------------------------------------------		
	LD	HL,#ROM2
	CALL	RCKSUM		; GET THE CHECKSUM IN DE
	CALL	TXSPC		; PRINT ANOTHER SPACE
;--------------------------------------------------------------------------------------		
	LD	HL,#ROM3
	CALL	RCKSUM		; GET THE CHECKSUM IN DE
;--------------------------------------------------------------------------------------		
	CALL	TXCRLF		; TXCRLF
	RET
;--------------------------------------------------------------------------------------
RCKSUM:	LD	BC,#0x1000		; GENERATE THE CHECKSUM IN DE
	LD	DE,#0x0000		; Initialize DE to zero
CKSUM1:	LD	A,(HL)		; Load memory location
	ADD	A,E		; Add its contents to E
	LD	E,A		; Move it back to E
	JR	NC,NOADD		; See if there was a carry from that add
	INC	D		; Yes, increment D also
NOADD:	INC	HL		; Get next location
	DEC	BC		; Decrement the counter
	LD	A,B		; Check the counter
	OR	C		;  to see if it
	JR	NZ,CKSUM1		;   has reached zero yet
	PUSH	DE		; If done with the count then
	POP	HL		; Move the value to HL for
	CALL	HLOUT		; HLOUT Print routine
	RET			; And finish it	                 		
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; Text for Messages
;------------------------------------------------------------------------------
TESTMSG:
	.BYTE	0x0D,0x0A
	.ascii	"Hello World"
	.byte	0x0D,0x0A,0x00
;------------------------------------------------------------------------------
HELPSCR:
	.BYTE	0x0D,0x0A
	.ascii	"A			JUMP TO $0000"
	.byte	0x0D,0x0A
	.ascii	"Bxx,yyyy		CHANGE BAUD RATE"
	.byte	0x0D,0x0A
	.ascii	"Cxxxx,yyyy,zzzz	COMPARE MEM"
	.byte	0x0D,0x0A
	.ascii	"Dxxxx,yyyy		DISP MEM"
	.byte	0x0D,0x0A
	.ascii	"E			EDIT MEM"
	.byte	0x0D,0x0A
	.ascii	"Gxxxx,[yyyy]		EXECUTE MEM"
	.byte	0x0D,0x0A
	.ascii	"Hxxxx,yyyy		HEX SUM/DIFF"
	.byte	0x0D,0x0A
	.ascii	"Ixx			PORT INPUT"
	.byte	0x0D,0x0A
	.ascii	"Jxxxx,yyyy		RAM TEST"
	.byte	0x0D,0x0A
	.ascii	"K			KILL BRKPT"
	.byte	0x0D,0x0A
	.ascii	"L			LOAD IHEX"
	.byte	0x0D,0x0A
	.ascii	"Mxxxx,yyyy,ZZZZ	MOVE MEM"
	.byte	0x0D,0x0A
	.ascii	"Nxxxx,yyyy		DISASM MEM"
	.byte	0x0D,0x0A
	.ascii	"Oxx,yy			PORT OUTPUT"
	.byte	0x0D,0x0A
	.ascii	"Pxxxx,ASC STRING	ASCII TO MEM"
	.byte	0x0D,0x0A
	.ascii	"Q			DISP STACK"
	.byte	0x0D,0x0A
	.ascii	"R			DISP REGS"
	.byte	0x0D,0x0A
	.ascii	"Sxxxx			MODIFY MEM"
	.byte	0x0D,0x0A
	.ascii	"Txxxx,yyyy		TYPE ASCII"
	.byte	0x0D,0x0A
	.byte	0x00
;------------------------------------------------------------------------------
SIGNON:	.BYTE	0x0C
	.ascii	"Space-Time Productions"
	.BYTE	0x0D,0x0A
	.ascii	"Z-80 Monitor Rom - Rev 9.01"
	.BYTE	0x0D,0x0A
	.ascii	"(c) July 25, Joel Owens"
	.BYTE	0x0d,0x0a
	.ascii	"ASxxxx version (c) 8-mar-2011 by Brad Riensche"
	.BYTE	0x0D,0x0A,0x00
;------------------------------------------------------------------------------
CKSUMERR:
	.ascii	"Checksum Error"
	.BYTE	0x0D,0x0A,0x00
;------------------------------------------------------------------------------
LCDTXT1:
	.ascii	"Z-80 Monitor Rom - Rev 9.01"
	.BYTE	0x00
LCDTXT2:
	.ascii	"(c) July 25, 2006 "
	.ascii	"Joel Owens"
	.BYTE	0x0d,0x0a
	.ascii	"ASxxxx version by Brad Riensche"
	.BYTE	0x00	
;------------------------------------------------------------------------------
DMPTEXT:
	.ascii	"AAAA  00 01 02 03 04 05 "
	.ascii	"06 07 08 09 0A 0B 0C 0D "
	.ascii	"0E 0F  0123456789ABCDEF"
	.BYTE	0x0D,0x0A
	
	.ascii	"------------------------"
	.ascii	"------------------------"
	.ascii	"-----------------------"
	.BYTE	0x00
;------------------------------------------------------------------------------
RTXT_A:	.ascii	"A ="
	.byte	0x00
RTXT_BC:
	.ascii	" BC ="
	.byte	0x00
RTXT_DE:
	.ascii	" DE ="
	.byte	0x00
RTXT_HL:
	.ascii	" HL ="
	.byte	0x00

RTXT_AP:
	.byte	"A",0x27,"="
	.byte	0x00
RTXT_BCP:
	.ascii	" BC"
	.byte	0x27,"="
	.byte	0x00
RTXT_DEP:
	.ascii	" DE"
	.byte	0x27,"="
	.byte	0x00
RTXT_HLP:
	.ascii	" HL"
	.byte	0x27,"="
	.byte	0x00

RTXT_PC:
	.ascii	"PC="
	.byte	0x00
RTXT_IX:
	.ascii	" IX="
	.byte	0x00
RTXT_IY:
	.ascii	" IY="
	.byte	0x00

RTXT_S:
	.ascii	" S"
	.byte	0x00
RTXT_Z:
	.ascii	" Z"
	.byte	0x00
RTXT_H:
	.ascii	" H"
	.byte	0x00
RTXT_PV:
	.ascii	" P/V"
	.byte	0x00
RTXT_N:
	.ascii	" N"
	.byte	0x00
RTXT_C:
	.ascii	" C"
	.byte	0x00
;------------------------------------------------------------------------------
;BITMAPS FOR LCD CHARS #0x00-#0x07
;------------------------------------------------------------------------------
CGRAM:
	.BYTE	0x00		;CHAR(0) Space:1999 EAGLE TAIL
	.BYTE	0b11111110
	.BYTE	0b11111111
	.BYTE	0b11111111
	.BYTE	0b11111110
	.BYTE	0x00
	.BYTE	0x00
	.BYTE	0x00

	.BYTE	0b11101111		;CHAR(1) EAGLE 1ST LANDPAD
	.BYTE	0b11111111
	.BYTE	0b11110001
	.BYTE	0b11110001
	.BYTE	0b11110001
	.BYTE	0b11101110
	.BYTE	0b11101110
	.BYTE	0x00
	
	.BYTE	0xFF		;CHAR(2) EAGLE
	.BYTE	0b11100000
	.BYTE	0b11111111
	.BYTE	0b11111111
	.BYTE	0b11111111
	.BYTE	0b11100110
	.BYTE	0b11100000
	.BYTE	0x00
	
	.BYTE	0xFF		;CHAR(3) EAGLE
	.BYTE	0b11100000
	.BYTE	0b11111111
	.BYTE	0b11111111
	.BYTE	0b11111111	
	.BYTE	0b11100000
	.BYTE	0b11100000
	.BYTE	0x00

	.BYTE	0xFF		;CHAR(4) EAGLE
	.BYTE	0b11100000
	.BYTE	0b11111111
	.BYTE	0b11111111
	.BYTE	0b11111111
	.BYTE	0b11101100
	.BYTE	0b11100000
	.BYTE	0x00

	.BYTE	0b11111110		;CHAR(5) EAGLE 2ND LANDPAD
	.BYTE	0b11111111
	.BYTE	0b11110001
	.BYTE	0b11110001
	.BYTE	0b11110001
	.BYTE	0b11101110
	.BYTE	0b11101110
	.BYTE	0x00

	.BYTE	0b11100000		;CHAR(6) EAGLE COCKPIT
	.BYTE	0b11111110
	.BYTE	0b11110001
	.BYTE	0b11111111
	.BYTE	0b11110001
	.BYTE	0b11111110
	.BYTE	0b11100000
	.BYTE	0x00

	.BYTE	0b00010000		;CHAR(7) ARROW HEAD MARKER
	.BYTE	0b00011000
	.BYTE	0b00011100
	.BYTE	0b00011110
	.BYTE	0b00011100
	.BYTE	0b00011000
	.BYTE	0b00010000
	.BYTE	0x00
	.BYTE	0x80		;TERMINATOR FLAG
;------------------------------------------------------------------------------
; Data bytes for the SIO
;------------------------------------------------------------------------------
SIODAT:	.BYTE	0x18		; SIO CONTROL, RESET CH, SELECT REGISTER 0
	.BYTE	0x18		; WRITE DATA TO REGISTER 0
	.BYTE	0x04		; SELECT REGISTER 4
	.BYTE	0x44		; X 16 CLOCK, 1 STOP BIT
	.BYTE	0x05		; SELECT REGISTER 5
	.BYTE	0xEA		; DTR, TX 8 BITS/1 STOP, RTS, TX ENABLE
	.BYTE	0x03		; SELECT REGISTER 3
	.BYTE	0xC1		; RX 8 BITS/CHAR, RX ENABLE
	.BYTE	0x01		; SELECT REGISTER 1
	.BYTE	0x00		; DISABLE WAIT/READY AND INTERRUPT MODES
;------------------------------------------------------------------------------
TDATA:	.ascii	"Joel Owens"
	.BYTE	0x0D,0x0A
	.ascii	"1401 Herring"
	.BYTE	0x0D,0x0A
	.ascii	"Merkel, Texas 79536"
	.BYTE	0x0D,0x0A
	.ascii	"owens_joel@yahoo.com"
	.BYTE	0x0D,0x0A,0x00
;------------------------------------------------------------------------------	

FINIS:
;	.END
