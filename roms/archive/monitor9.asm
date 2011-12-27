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
ROM0	.EQU	$0000
ROM1	.EQU	$1000
ROM2	.EQU	$2000
ROM3	.EQU	$3000

IOFLAG	.EQU	$400E	; I/O Status Flag Word
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

CHAR	.EQU	$400F	; Storage for character to be printed

;------------------------------------------------------------------------------
;                         START OF MONITOR ROM
;------------------------------------------------------------------------------

MON	.ORG	$0000		; MONITOR ROM RESET VECTOR
;------------------------------------------------------------------------------
; TX a character over RS232 Channel A [Host], wait for TXDONE first.
;------------------------------------------------------------------------------
RST00	DI			;Disable INTerrupts
	IM	1		;INT vectors over to $0038
	JP	INIT		;Initialize Hardware and go
	NOP
	NOP
;------------------------------------------------------------------------------
; TX a character over RS232 Channel A [Host], wait for TXDONE first.
;------------------------------------------------------------------------------
RST08	JP	TXA
	NOP
	NOP
	NOP
	NOP
	NOP
;------------------------------------------------------------------------------
; RX a character over RS232 Channel A [Console], hold here until char ready.
;------------------------------------------------------------------------------
RST10	JP	RXA
	NOP
	NOP
	NOP
	NOP
	NOP
;------------------------------------------------------------------------------
; TX a character over MIDI (Serial Channel B), wait for TXDONE first.
;------------------------------------------------------------------------------
RST18	JP	TXB
	NOP
	NOP
	NOP
	NOP
	NOP
;------------------------------------------------------------------------------
; RX a character from MIDI (Serial Channel B), hold here until char ready.
;------------------------------------------------------------------------------
RST20	JP	RXB
	NOP
	NOP
	NOP
	NOP
	NOP
;------------------------------------------------------------------------------
; TX a character to the 8279 Display, check 8279 display ready prior to write.
;------------------------------------------------------------------------------
RST28	JP	TX8279	; Transmit char in A out to 8279
	NOP
	NOP
	NOP
	NOP
	NOP
;------------------------------------------------------------------------------
; RX a character from the 8279 Keyboard, hold here until key is in buffer.
;------------------------------------------------------------------------------
RST30	JP	RX8279	; Wait for char from 8279 keyboard
	NOP
	NOP
	NOP
	NOP
	NOP
;------------------------------------------------------------------------------
; RST 38 - INTERRUPT VECTOR [ for IM 1 ]
;------------------------------------------------------------------------------
RST38	DI		; Breakpoint, Disable Interrupts	
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
GOTO	POP  HL		; GO command terminates here	
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
	RST  38H	    	; is altered by GO cmd so that this RET	
	RST  38H	    	; becomes the user's GOTO Vector)	
	RST  38H
;------------------------------------------------------------------------------
; [NMI VECTOR]
; The NMI signal can cause a program to break and save all its register values
; The INT signal would work the same if the processor is put into IM 1, otherwise
; It is not set in this rom, and would default to the 8080 mode where, on INT-, the
; Interrupting device (or some other rigged hardware) would supply the necessary RST
;------------------------------------------------------------------------------
	.ORG $0066	; NMI VECTOR
RST66	push	af
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

	;JR   RST38	; Just play like it's an INT in IM 1

;------------------------------------------------------------------------------
; Monitor Entry point following an Error
;------------------------------------------------------------------------------
ERROR	POP  HL		; Entry point for Monitor, after ERROR	
	LD   A,$3F	; Get a "?"	
	RST  08H		; And TX it out.
;------------------------------------------------------------------------------
; Normal Monitor Entry Point
;------------------------------------------------------------------------------
NORMAL	CALL TXCRLF	; Entry point for Monitor, Normal	
	LD   A,$3E	; Get a ">"	
	RST  08H		; and TX it out.
	LD   HL,NORMAL	; Save entry point for Monitor	
	PUSH HL		; This becomes the last RET address

NORM1	RST  10H		; Get user character
	
	CP   $20		; <spc>? 	
	JR   Z,NORM1	; Go back for something else
	
	CP   $3A		; ":"?
	JP   Z,LOAD1	; It's the first character of a load command
	
	CP   $61	; if upper case, convert to lower case
	JP   NZ,ALLUPR
	SUB  $20
ALLUPR
;	AND  $5F		; Make character uppercase
	CP   $56		; Character > "U" ?	
	JR   NC,ERROR	; It's wrong, give an error and start over
	
	RST  08H		; Print it back to the console
	
	SUB  $3F		; Take values from ascii A-U down to 0-20 decimal
	JR   C,ERROR	; Must have given a Minus (Char < "A"), go back	
	
	LD   HL,CMDTBL	; Start at Keyboard Command Vector Table
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
CMDTBL	
	.WORD	HELP	; <?> HELP
	.WORD	ERROR	; <@> NO CMD
	.WORD	RST00	; <A> $0000 RESET
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
HELP	LD   HL,HELPSCR	; Print HELP message
	CALL PRT
	RET
	
;------------------------------------------------------------------------------
; ENTRY POINT FOR <J> Memory Test
;------------------------------------------------------------------------------
MEMTEST	CALL GETXY	; Get XXXX to DE,YYYY to HL from user	
	RET  C		; on Carry <Ctrl-C> was hit, cancel.	
	EX   DE,HL	; Swap start, ending address
MEMTEST1	LD   A,(HL)	; Get byte from starting address	
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
MEMDMP	CALL GETXY	; ENTRY POINT FOR <D>, Get xxxx,yyyy	
	RET  C		; Exit if user Ctrl-C'd us.	
	EX   DE,HL	; Put start in HL, ending addr in DE
	LD   A,L		; Set starting low nybble at zero
	AND  $F0		; Strip off lower misc bits
	LD   L,A		; And save it back
	LD   A,E		; See if lower nybble zero
	OR   $0F		; Set to end of line
	LD   E,A		; Save back altered lower byte	
MEMDMP1	LD   A,L		; Are we at a new page?
	OR   A		; Test Z flag
	CALL Z,MEMDMP0	; If new page, print header row first
	CALL HLCOLN	; Print addr in HL plus ": "	
	PUSH HL		; Store HL
	POP  IX		; Retrieve it
	LD   B,$10	; Get 0-F bytes of data	
MEMDMP2	PUSH BC		; Save the byte counter	
	LD   A,(HL)	; Load memory from HL	
	CALL AOUT		; Convert byte to ASC in BC and TXA it	
	CALL TXSPC	; TXA a <space>	
	POP  BC		; Get the byte counter back	
	INC  HL		; Otherwise get next location	
	DEC  B		; Decrement byte count	
	JR   NZ,MEMDMP2	; And finish this row.
	LD   A,$20	; Print a space
	RST  08H		; Output it
	LD   B,$10	; Get 0-F data
	PUSH IX		; Get back start of line
	POP  HL		; Into HL
MEMDMP3	LD   A,(HL)	; Get character
	CP   $20		; < Space?
	JP   M,MEMDMP4	; Yes, just print a '.'
	CP   $7E		; > ~ ?
	JP   M,MEMDMP5	; It's in between so print it
MEMDMP4	LD   A,'.'	; Get a period
MEMDMP5	RST  08H		; Print the character
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
MEMDMP0	CALL TXCRLF	; Make a new line for it
	PUSH HL		; Store Start
	LD   HL,DMPTEXT	; Get line of text
	CALL PRTSTR	; And print it
	POP  HL		; Retrieve starting address
	RET		; and RETurn		
;------------------------------------------------------------------------------
; Substitute Memory command
;------------------------------------------------------------------------------
SUBMEM	CALL GETX		; ENTRY POINT FOR <S>, Get xxxx from user.	
	RET  C		; Exit if he <Ctrl-C'd> us.	
SUBMEM1	CALL PRTMEM	; Print CRLF + "/ " + byte at (HL)	
	LD   A,$2D	; Character "-"	
	RST  08H		; TXA it	
	EX   DE,HL	; Swap
	CALL GETHL	; Get 4 Hex chars from User, convert in HL	
	EX   DE,HL	; Place them in DE	
	JR   C,SUBMEM2	; If user entry term'd normally, replace	
	LD   (HL),E	; memory location with the new data in E	
SUBMEM2	INC  HL		; otherwise, skip ahead to the next location	
	CP   $20		; <spc>?	
	JR   Z,SUBMEM1	; Get next byte and location          		
	CP   $0A		; LF?
	JR   Z,SUBMEM1	; Get next byte and location
	RET
;------------------------------------------------------------------------------
; GO command
;------------------------------------------------------------------------------
GOCMD	CALL GETHL	; ENTRY POINT FOR <G>oto addr. Get XXXX from user.
	JR   NC,GOCMD1	; Must have given a good answer         	
	CP   $03		; <CTRL-C>, must have chickened out
	RET  Z
	JP   GOTO		; POP Everything and use SP-$0018 as GOTO address
		
GOCMD1	EX   DE,HL	; Put GOTO address in DE
	LD   HL,$0018	; Locate in Memory above Stack where
	ADD  HL,SP	; RET from sub vector is located	
	DEC  HL              	
	LD   (HL),D	; Store User Jump Address, after JP to 0024H
	DEC  HL		; The RET is altered and becomes the return-to addr	
	LD   (HL),E
GOCMD2	CP   $2C		; Comma? User is Entering a Breakpoint also	
	JP   NZ,GOTO	; End of Breakpoints? Pop everything and RET	
	CALL GETX		; Get XXXX from user for Breakpoint	
	RET  C		; Cancel if he <Ctrl-C>'s us	
	LD   B,A             		
	LD   A,(HL)	; Save original memory contents of breakpoint location	
	LD   ($37FF),A	; At $37FF	
	LD   ($37FD),HL	; And the address of the Breakpoint at $37FD-$37FE	
	LD   (HL),$FF	; Swap instruction at (HL) for a RST 38H.
	LD   A,B		; And Restore the Terminating character, or ","	
	JP   GOTO		; Do the GO
;------------------------------------------------------------------------------
; Kill / Restore last Breakpoint
;------------------------------------------------------------------------------
BRKPT	LD   HL,($37FD)	; ENTRY POINT FOR <K>ill/Restore last breakpoint
	LD   A,($37FF)	; Recall original contents	
	LD   (HL),A	; Write it back	
	RET		; And finish.	
;------------------------------------------------------------------------------
; Display User Stack Register
;------------------------------------------------------------------------------
USRSTK	CALL TXSPC	; ENTRY POINT FOR <Q> display user stack	
	LD   HL,$0018	; Set User Stack 18H below actual stack	
	ADD  HL,SP	; Get adjusted SP into HL
	JP   HLOUT	; Convert and TXA contents of HL	
;------------------------------------------------------------------------------
; Input Port command		
;------------------------------------------------------------------------------
INPORT	CALL GETX		; ENTRY POINT FOR <I>nput Port	
	RET  C		; Get user port address
	CALL TXCRLF	; TXCRLF	
	LD   C,L		; Load the port address into C
	IN   A,(C)	; Get the data
	JP   AOUT		; CONVERT BYTE TO ASCII AND OUTPUT
;------------------------------------------------------------------------------
; Output Port command
;------------------------------------------------------------------------------
OUTPORT	CALL GETXY	; ENTRY POINT FOR <O>utput Port,Value	
	RET  C		; Get user port and data to output xxxx,yyyy		
	LD   C,E		; Port address into C		
	OUT  (C),L	; Output the yy data to the port address
	RET                  		
;------------------------------------------------------------------------------
; Hex Sum/Difference command
;------------------------------------------------------------------------------
HXMATH	CALL GETXY	; ENTRY POINT FOR <H>ex Sum,Diff	
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
ASC2MEM	CALL GETX		; Get last 4 hex chars typed into HL	
	RET  C		; Break if <Ctrl-C>	
	CALL TXCRLF	; TXCRLF	
A2MEM1	CALL ECHO		; RX a Character	
	CP   $03		; Break?	
	JP   Z,HLOUT	; Convert and TXA contents of HL
	LD   (HL),A	; Store character into memory
	INC  HL		; Next location
	JR   A2MEM1	; Continue getting characters until user breaks out
 
;------------------------------------------------------------------------------
; Type ASCII from memory command
;  Modified to only print ASCII characters or "."
;------------------------------------------------------------------------------
TYPEMEM	CALL GETXY	; ENTRY POINT FOR <T>ype Ascii from memory	
	RET  C               		
	CALL TXCRLF	; TXCRLF	
	LD   A,E		; Clean up starting address
	AND  $F0		; Strip off lower nybble
	LD   E,A		; Store it back
	LD   A,L		; Round up L
	OR   $0F		; Set lower nybble
	LD   L,A		; Store it back
TYPMEM0	LD   A,D		; Print HL contents AAAA:
	CALL AOUT		; Print it
	LD   A,E		; Lower
	CALL AOUT		; Print it
	LD   A,':'	; Colon
	RST  08H		; Send that
	LD   A,$20	; Space
	RST  08H		; Print that, too
	LD   B,$10	; 0-F bytes
TYPMEM1	LD   A,(DE)	; Load the Accum from location at DE
	CP   $20		; Printable characters should run from $20 (space)
	JP   M,TYPMEM2	; It's a control character, just print '.' instead
	
	CP   $7E		; $7E ASCII character set, ok to print it
	JP   M,TYPMEM3	; Otherwise, just
	
TYPMEM2	LD   A,'.'	; Print a "." instead.
TYPMEM3	RST  08H		; And TXA it.               		
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
DSPLREG	CALL TXCRLF	; CR,LF
	LD   HL,RTXT_A	; "A ="
	CALL PRT		; Print it
	LD   A,($FFE3)	; Get A contents
	CALL AOUT		; Print it
	
	LD   HL,RTXT_BC	; " BC ="
	CALL PRT		; Print it
	LD   HL,($FFE0)	; Get HL contents from BC storage
	CALL HLOUT	; Convert and print those contents
	
	LD   HL,RTXT_DE	; " DE =";
	CALL PRT		; Print it
	LD   HL,($FFDE)	; Get HL contents from DE storage
	CALL HLOUT	; Print it
	
	LD   HL,RTXT_HL	; " HL ="
	CALL PRT		; Print it
	LD   HL,($FFFC)	; Get HL contents from HL storage
	CALL HLOUT	; Print it
	
	LD   DE,$FFE2	; Main flag register storage
	CALL DSPLFLG	; Display flags
	
	CALL TXCRLF	; Next line
	
	LD   HL,RTXT_AP	; "A'="
	CALL PRT		; Print it
	LD   A,($FFD7)	; Get A contents
	CALL AOUT		; Print it
	
	LD   HL,RTXT_BCP	; " BC'="
	CALL PRT		; Print it
	LD   HL,($FFD4)	; Get HL contents from BC storage
	CALL HLOUT	; Convert and print those contents
	
	LD   HL,RTXT_DEP	; " DE'=";
	CALL PRT		; Print it
	LD   HL,($FFD2)	; Get HL contents from DE storage
	CALL HLOUT	; Print it
	
	LD   HL,RTXT_HLP	; " HL'="
	CALL PRT		; Print it
	LD   HL,($FFD0)	; Get HL contents from HL storage
	CALL HLOUT	; Print it
	
	LD   DE,$FFD6	; Main flag register storage
	CALL DSPLFLG	; Display flags
	
	CALL TXCRLF	; NEXT LINE
	
	LD   HL,RTXT_PC	; "PC="
	CALL PRT		; Print it
	LD   HL,($FFE4)	; Get PC storage contents
	CALL HLOUT	; Print it
	
	LD   HL,RTXT_IX	; " IX="
	CALL PRT		; Print it
	LD   HL,($FFDA)	; IX contents
	CALL HLOUT	; Print it
	
	LD   HL,RTXT_IY	; " IY="
	CALL PRT		; Print it
	LD   HL,($FFD8)	; IY contents
	CALL HLOUT	; PRint it
	
	CALL TXCRLF	; New line
	RET
;------------------------------------------------------------------------------
; Display flag status
;------------------------------------------------------------------------------
DSPLFLG	LD   HL,RTXT_S	; " S"
	CALL PRT		; Print it
	LD   A,(DE)	; Get Flag byte
	BIT  7,A		; Test sign
	CALL FLG1_0	; Print it
	
	LD   HL,RTXT_Z	; " Z"
	CALL PRT		; Print it
	LD   A,(DE)	; Get Flag byte
	BIT  6,A		; Test Z sign
	CALL FLG1_0	; Print it
	
	LD   HL,RTXT_H	; " H"
	CALL PRT		; Print it
	LD   A,(DE)	; Get Flag byte
	BIT  4,A		; Test H sign
	CALL FLG1_0	; Print it
	
	LD   HL,RTXT_PV	; " P/V"
	CALL PRT		; Print it
	LD   A,(DE)	; Get Flag byte
	BIT  2,A		; Test H sign
	CALL FLG1_0	; Print it
	
	LD   HL,RTXT_N	; " N"
	CALL PRT		; Print it
	LD   A,(DE)	; Get Flag byte
	BIT  1,A		; Test H sign
	CALL FLG1_0	; Print it
	
	LD   HL,RTXT_C	; " C"
	CALL PRT		; Print it
	LD   A,(DE)	; Get Flag byte
	BIT  0,A		; Test H sign
	CALL FLG1_0	; Print it
	
	RET
;------------------------------------------------------------------------------	
FLG1_0	JR   Z,FLG0	; It's zero
	LD   A,'1'	; Else it's one
FLGPRT	RST  08H		; Print it
	RET		; and return
FLG0	LD   A,'0'	; Print a "0"
	JR   FLGPRT	; Finish it
;------------------------------------------------------------------------------	

;--------------------------------------------------------------------------------------
; Characters for <R>egister command to print,
;  leave this for Edit register command
;--------------------------------------------------------------------------------------
REGTXT	.BYTE	$20,"PC=X",$20,$20
	.BYTE	"A=",$20,$20,$20
	.BYTE	"F=",$20,$20,$20
	.BYTE	"B=",$20,$20,$20
	.BYTE	"C=",$20,$20,$20
	.BYTE	"D=",$20,$20,$20
	.BYTE	"E=",$20,$20
	.BYTE	"HL=X",$20
	.BYTE	"IX=X",$20
	.BYTE	"IY=X"
	.BYTE	$FF	; TERMINATING CHARACTER

REGTXTX	.BYTE	$20,"A'="
	.BYTE	$20,$20
	.BYTE	"F'="
	.BYTE	$20,$20
	.BYTE	"B'="
	.BYTE	$20,$20
	.BYTE	"C'="
	.BYTE	$20,$20
	.BYTE	"D'="
	.BYTE	$20,$20
	.BYTE	"E'="
	.BYTE	$20
	.BYTE	"HL'=X"
	.BYTE	$FF	; TERMINATING CHARACTER
;--------------------------------------------------------------------------------------		
DREG1	LD   C,$04	; Print Register Text 4 bytes at a time
DREG2	LD   A,(DE)	; Load text at (DE)	
	INC  DE		; Get next location	
	RST  08H		; Print current text byte	
	DEC  C		; Decrement the counter	
	JR   NZ,DREG2	; and Repeat until all 4 bytes are printed	
	DEC  HL		; Decrement the variable space pointer	
	LD   A,(HL)	; Load it's contents in the accumulator	
	CALL AOUT		; Convert A to ascii in BC and print it
	LD   A,(DE)	; Get next text string of registers	
	INC  DE		; Update the location	
	CP   $58		; An "X" indicates 2 byte value (HL,DE,IX,IY)
	RET  NZ		; Return if only 1 byte value needed	
	DEC  HL		; Else get pointer to next byte	
	LD   A,(HL)	; Load it into A	
	JP   AOUT		; Convert it to ASCII and print it.
		
DREG3	CALL ECHO		; Continue <R> command, loop in/out a char <enter> or <'>

	LD   HL,$001A	; HL contains user storage area for registers
	ADD  HL,SP	; and DE contains the text for that register
	LD   DE,REGTXT	; REGTXT starts text for Primary registers
	CP   $27		; Has user hit <'> for the Alternate Registers?
	RET  NZ		; If not, we've got the right ones then
	LD   HL,$000C	; Move pointer not quite so far for alternate registers
	ADD  HL,SP	; Stack plus the new start of registers
	LD   DE,REGTXTX	; REGTXTX starts text for Alternate registers
	RET
;------------------------------------------------------------------------------
; Edit registers
;------------------------------------------------------------------------------
EDITREG	CALL DREG3	; ENTRY POINT FOR <E>dit Registers
L021B	CALL TXCRLF	; TXCRLF
	LD   A,(DE)	; Load variable space info (user registers)
	CP   $FF		; Reached the end of the text yet?
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
		
L0232	POP  AF		; Get the last user character back
	CP   $20		; Was it a <space>?	
	JR   Z,L021B	; Get set up to do the next registers          		
	CP   $0A		; <LF>?
	JR   Z,L021B	; Space or LF continues on to edit other
	RET		; registers, otherwise we are finished
		
L023C	LD   (HL),C	; Store new register info to memory
	DEC  DE		; Where it will be picked up for a Go command
	LD   A,(DE)	; Store it there
	INC  DE		; Get next location if needed for 16 bit registers
	CP   $58		; Is it "X" ?	
	JR   NZ,L0232	; Only 8 bits needed for basic reg (a,b,c,d,e)
	INC  HL		; Must be 16 bit register set (HL,SP,IX,IY)
	LD   (HL),B	; so write that byte also
	DEC  HL		; Set up for the next location
	JR   L0232	; And see what the user wants to do next
;------------------------------------------------------------------------------
; Move command
;------------------------------------------------------------------------------	
MOVMEM	CALL GETXYZ	; ENTRY POINT FOR <M>ove, Get xxxx,yyyy, create BC, zzzz dest
	RET  C		; but exit if he chickens out <ctrl-c>
	POP  IX		; Value is $0072, pushed before user hit a menu key
	LDIR		; Make the big move
	JP   (IX)		; and go back to normal monitor entry point.
;------------------------------------------------------------------------------
; Fill command		
;------------------------------------------------------------------------------
MEMFILL	CALL GETXYZ	; ENTRY POINT FOR <F>ill, Get xxxx,yyyy, create BC, zz contents
	RET  C		; but exit if he changes his mind with <ctrl-c>
	POP  IX		; Get $0072 for normal monitor entry point.
L0259	LD   (HL),E	; Load new fill value into destination memory
	INC  HL		; Increment to the next location
	DEC  BC		; Count down.
	LD   A,B		; Check BC to see if it's zero yet
	OR   C		
	JR   NZ,L0259	; Check to see if we're finished, else loop back.
	JP   (IX)		; Jump back to monitor normal entry point.
;------------------------------------------------------------------------------
; Compare command
;------------------------------------------------------------------------------	
MCOMP	CALL GETXYZ	; ENTRY POINT FOR <C>ompare, Get xxxx,yyyy, create BC, zzzz other
	RET  C		; May quit and go home early today
MCOMP1	LD   A,(DE)	; Get the byte from memory	
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
UNLOAD	XOR  A		; Zeroize Accum
	LD   HL,$3000	; Start of static ram
	LD   DE,$3001	; Next location
	LD   BC,$0800	; ALL static ram
	LD   (HL),A	; Store the zero
	LDIR		; Move it into all locations
	
	LD   HL,$4010	; Start of program ram
	LD   DE,$4011	; Next location
	LD   BC,$BFEF	; $FFFF-$4010
	LD   (HL),A	; Store the zero
	LDIR		; Clear it all out
	RET

;------------------------------------------------------------------------------

;                        S U B R O U T I N E S

;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Print contents of HL registers, plus ": "
;   and the contents of memory at location (HL)
;------------------------------------------------------------------------------
PRTMEM	CALL TXCRLF	; TXCRLF
	CALL HLCOLN	; Print HL contents + ": "
	LD   A,(HL)	; Get Memory contents at (HL)
	JR   AOUT		; Convert A to Ascii in BC and Print it	
		
HLCOLN	CALL HLOUT	; Convert and Print Contents of HL
COLON	LD   BC,$3A20	; Load ": " in BC

	JR   BCOUT	; and TX	it out.
;------------------------------------------------------------------------------		
PRTSGN	LD   HL,SIGNON	; Print SIGNON message
PRTMSG	CALL TXCRLF	; TXCRLF
;------------------------------------------------------------------------------
; Print string of characters to Serial A until byte=$00, WITH CR, LF
;------------------------------------------------------------------------------
PRT	LD   A,(HL)	; Get character
	OR   A		; Is it $00 ?
	RET  Z		; Then RETurn on terminator
	
	RST  08H		; Print to TTY
	
	INC  HL		; Next Character
	JR   PRT		; Continue until $00 is hit
;------------------------------------------------------------------------------
; Print string of characters to Serial A until byte=$00, WITH CR, LF
;------------------------------------------------------------------------------
PRTSTR	LD   A,(HL)	; Get character
	OR   A		; Is it $00 ?
	JR   Z,TXCRLF	; Then TXCRLF and RETurn
	
	RST  08H		; Print to TTY
	
	INC  HL		; Next Character
	JR   PRTSTR	; Continue until $00 is hit
; HLOUT - Convert and transmit contents of HL registers
;------------------------------------------------------------------------------
HLOUT	LD   A,H		; Convert and TXA contents of H and L
	CALL AOUT		; Convert H into ASCII BC and print it
	LD   A,L		; Convert L
AOUT	CALL ATOBC	; Convert byte to ASCII
	JR   BCOUT	; Send B and C to TXA and use that RETurn
;------------------------------------------------------------------------------
; TXCRLF - Transmit a CR and LF character, new line
;------------------------------------------------------------------------------
TXCRLF	LD   BC,$0D0A	; Entry point for "TXCRLF" $0D=CR, $0A=LF
;------------------------------------------------------------------------------
; BCOUT - Ascii characters in BC are transmitted
;------------------------------------------------------------------------------
BCOUT	LD   A,B		; Entry point to print ascii chars in B and C
	RST  08H		; Print character from B
	LD   A,C		; Get character from C
	RST  08H		; Print character from C
	RET
;------------------------------------------------------------------------------
; This is a good routine to set up any move, fill, or LDIR command for the user
;------------------------------------------------------------------------------
GETXYZ	CALL GETXY	; GET xxxx,yyyy Start and End address + Create Byte Count
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
GETHL	LD   HL,$0000	; Gets xxxx but sets Carry Flag on any Terminator
	CALL ECHO		; RX a Character
	CP   $20		; Did he give us some <space>?
	JR   Z,SETCY	; Set the carry and quit
	CP   $0A		; Did he feed us a linefeed <LF>?
	JR   Z,SETCY	; Set the carry and quit
	CP   $0D		; Is he trying to return his carriage <CR>?
	JR   NZ,GETX2	; Hmm must be some other key		
SETCY	SCF		; Set Carry Flag
	RET                 ; and Return to main program		
;------------------------------------------------------------------------------
; This routine converts last four hex characters (0-9 A-F) user types into a value in HL
; Rotates the old out and replaces with the new until the user hits a terminating character
;------------------------------------------------------------------------------
GETX	LD   HL,$0000	; CLEAR HL
GETX1	CALL ECHO		; RX a character from the console
	CP   $20		; Is he <spc>'d out? Set the zero and
	RET  Z		; Get the heck out of Dodge
	CP   $0A		; Feeding us that old line again? <LF>?
	RET  Z		; Quit and go home
	CP   $0D		; Must be the carriage returning <CR>
	RET  Z		; Same thing again
	CP   $2C		; <,> can be used to safely quit for multiple entries
	RET  Z		; (Like filling both DE and HL from the user)
GETX2	CP   $03		; Likewise, a <ctrl-C> will terminate clean, too, but
	JR   Z,SETCY	; It also sets the Carry Flag for testing later.
	ADD  HL,HL	; Otherwise, rotate the previous low nybble to high
	ADD  HL,HL	; rather slowly
	ADD  HL,HL	; until we get to the top
	ADD  HL,HL	; and then we can continue on.
	SUB  $30		; Convert ASCII to byte	value
	CP   $0A		; Are we in the 0-9 range?
	JR   C,GETX3	; Then we just need to sub $30, but if it is A-F
	SUB  $07		; We need to take off 7 more to get the value down to
GETX3	AND  $0F		; to the right hex value
	ADD  A,L		; Add the high nybble to the low
	LD   L,A		; Move the byte back to A
	JR   GETX1		; and go back for next character until he terminates
;------------------------------------------------------------------------------	
; This routine is called many times for routines needing the user to enter a
; Starting address of four hex characters in DE and an ending address in HL
; "Get xxxx (DE), yyyy (HL)"
;------------------------------------------------------------------------------
GETXY	CALL GETX	; Get the first four hex characters into HL
	RET  C		; But break out if he terminates
	EX   DE,HL	; Put it in DE, go back again for yyyy
	JR   GETX	; and get four more chars to put in HL.		
;------------------------------------------------------------------------------
; Convert ASCII characters in B C registers to a byte value in A
;------------------------------------------------------------------------------
BCTOA	LD   A,B		; Move the hi order byte to A
	SUB  $30		; Take it down from Ascii
	CP   $0A		; Are we in the 0-9 range here?
	JR   C,BCTOA1	; If so, get the next nybble
	SUB  $07		; But if A-F, take it down some more
BCTOA1	RLCA		; Rotate the nybble from low to high
	RLCA		; One bit at a time
	RLCA		; Until we
	RLCA		; Get there with it
	LD   B,A		; Save the converted high nybble
	LD   A,C		; Now get the low order byte
	SUB  $30		; Convert it down from Ascii
	CP   $0A		; 0-9 at this point?
	JR   C,BCTOA2	; Good enough then, but
	SUB  $07		; Take off 7 more if it's A-F
BCTOA2	ADD  A,B		; Add in the high order nybble
	RET
;------------------------------------------------------------------------------
; Convert single byte in A to two Ascii characters in B and C
;------------------------------------------------------------------------------
ATOBC	LD   B,A		; Save the byte original value
	AND  $0F		; Strip off the high order nybble
	CP   $0A		; 0-9 at this point?
	JR   C,ATOBC1	; We only need to add $30 then, or
	ADD  A,$07	; Seven more if it's in the A-F range
ATOBC1	ADD  A,$30	; Take the value on up to ASCII
	LD   C,A		; Store the converted lo order character to C
	LD   A,B		; Get the original value back
	RRCA		; Rotate it to the right
	RRCA
	RRCA
	RRCA
	AND  $0F		; Mask off the upper trash
	CP   $0A		; 0-9 (< $0A)?
	JR   C,ATOBC2	; Only add $30 then
	ADD  A,$07	; And add it 7 if it's in the A-F range
ATOBC2	ADD  A,$30	; Finish the conversion to ASCII
	LD   B,A		; Load the finished hi order character to B
	RET		; A is trashed, but the ASCII representation is in BC
;------------------------------------------------------------------------------
; CPHLDE
;------------------------------------------------------------------------------
CPHLDE	LD   A,H		; Compares HL=DE, returns Zero flag if =	
	CP   D
	RET  NZ		; if high order is out, no use testing low	
	LD   A,L
	CP   E
	RET		; returns Zero flag if =
;------------------------------------------------------------------------------
TXSPC	LD   A,$20	; TX a <spc> out the RS232	
	JP   RST08
;------------------------------------------------------------------------------
ECHO	RST  10H		; Loops a character in, RX Char	
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
LOAD	CALL TXCRLF	; TXCRLF
LOAD0	CALL GETCHR	; See if user types <ctrl-c>, or <spc>, else RET
	CP   $03		; User must have chickened <ctrl-c> out	
	RET  Z		; Return him to the prompt
LOAD1	CP   $3A		; Colon <:>? Start of line of Intel Hex file
	JR   NZ,LOADERR	; If not, must be error, Tell him and quit
	LD   E,0		; First two Characters is the Record Length Field
	CALL GET2		; Get us two characters into BC, convert it to a byte <A>
	LD   D,A		; Load Record Length count into D
	CALL GET2		; Get next two characters, Memory Load Address <H>
	LD   H,A		; put value in H register.
	CALL GET2		; Get next two characters, Memory Load Address <L>
	LD   L,A		; put value in L register.
	CALL GET2		; Get next two characters, Record Field Type
	CP   $01		; Record Field Type 00 is Data, 01 is End of File
	JR   NZ,LOAD2	; Must be the end of that file
	CALL GET2		; Get next two characters, assemble into byte
	LD   A,E		; Recall the Checksum byte
	AND  A		; Is it Zero?
	RET  Z		; Must be o.k., go back for some more, else
	JR   LOADERR	; Checksums don't add up, Error out
		
LOAD2	LD   A,D		; Retrieve line character counter	
	AND  A		; Are we done with this line?
	JR   Z,LOAD3	; Get two more ascii characters, build a byte and checksum

	CALL GET2	; Get next two chars, convert to byte in A, checksum it
	LD   (HL),A	; Checksum OK, move converted byte in A to memory location
	INC  HL		; Increment pointer to next memory location	
	DEC  D		; Decrement line character counter
	JR   LOAD2	; and keep loading into memory until line is complete
		
LOAD3	CALL GET2	; Get two chars, build byte and checksum
	LD   A,E		; Check the checksum value
	AND  A		; Is it zero?
	JR   Z,LOAD0	; If the checksum is still ok, continue on, else
LOADERR	LD   HL,CKSUMERR	; Get "Checksum Error" message
	JP   PRTMSG	; Print Message from (HL) and terminate the load
;------------------------------------------------------------------------------
; Get a character from the console, must be $20-$7F to be valid (no control characters)
; <Ctrl-c> breaks with the Zero Flag set
;------------------------------------------------------------------------------	
GETCHR	RST  10H		; RX a Character
	CP   $03		; <ctrl-c> User break?
	RET  Z			
	CP   $20		; <space> or better?
	JR   C,GETCHR	; Do it again until we get something usable
	RET
;------------------------------------------------------------------------------
; Gets two ASCII characters from the console (assuming them to be HEX 0-9 A-F)
; Moves them into B and C, converts them into a byte value in A and updates a
; Checksum value in E
;------------------------------------------------------------------------------
GET2	CALL GETCHR	; Get us a valid character to work with
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
;------------------------------------------------------------------------------

; Short Disassembler routine, by J.Kerr

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
DISAS	NOP			
	.MODULE DIS
DISASS	CALL	GETXY		;GET XXXX=DE (START), YYYY=HL (END)
	INC	HL
	PUSH	HL		;STORE END OF DISASSEMBLE HUNT

BLOOP	PUSH	DE
	CALL	DISZ80		;DISASSEMBLE INSTRUCTION AT (DE)

	LD	B,3
GAP	LD	A,20H		;<SPACE>
	CALL	CHROP		;PRINT 3 SPACES
	DJNZ	GAP

	POP	HL		;REGAIN THE OLD DE START OF CURRENT INSTRUCTION
TEXTLP	LD	A,(HL)		;SHOW THE ASCII CHARS TO HELP
	CP	20H		;< SPC?
	JP	M,BAD		
	CP	7EH		;> TILDE?
	JP	M,GOOD		;MMMM, GOOD
BAD	LD	A,'.'		;UNPRINTABLE, PUT A "." THERE INSTEAD.
GOOD	CALL	CHROP		;TX IT
	INC	HL
	CALL	CPHLDE		;HL=DE?	
	JR	Z,TEXTEND
	JR	TEXTLP

TEXTEND	CALL	TXCRLF		;TXCRLF
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
DISZ80 	CALL 	ADRSP		;Convert the current adr in DE to HEX, print
       	LD 	BC,$0900		;9 pairs of spaces onto stack
       	LD 	HL,$2020		;$20 <space>
BUFFER 	PUSH 	HL		;Place pair on stack
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
       	LD 	HL,GROUP3	;Group 3 
;----------------------------------------------------------------------------------------
TRYNDX 	CALL 	FETCH		;Convert byte at DE to Hex,print,update pointers
       	LD 	B,C
       	CP 	$ED		;Contains two-byte Z80 specific op codes
       	JR 	Z,CONFLG

       	INC 	B
       	CP 	$DD		;IX specific op codes
       	JR 	Z,CONFLG

       	INC 	B
       	CP 	$FD		;IY specific op codes
       	JR 	NZ,NOTNDX		;Else it must be some other code
;----------------------------------------------------------------------------------------
CONFLG 	LD 	(IX+1),B
       	INC 	B
       	DJNZ 	TRYNDX

       	JR 	NXBYTE
;----------------------------------------------------------------------------------------
NOTNDX 	LD 	C,A		;Save op code byte to C
       	LD 	A,(IX+1)
       	OR 	A
       	JR 	Z,NODISP		;No two-byte op code

       	LD 	A,C
       	CP 	$CB		;CB+second byte op code
       	JR 	Z,GETDIS

       	AND 	$44
       	CP 	4
       	JR 	Z,GETDIS

       	LD 	A,C
       	AND 	$C0
       	CP 	$40
       	JR 	NZ,NODISP
;----------------------------------------------------------------------------------------
GETDIS 	CALL 	FETCH		;Get second byte of two byte op code
       	LD 	(IX+2),A	;Save it

NODISP 	LD 	HL,GROUP1
       	LD 	A,C
       	CP 	$CB
       	JR 	NZ,NEWMSK

       	LD 	HL,GROUP2	;Group 2

NXBYTE 	CALL 	FETCH
       	LD 	C,A

NEWMSK 	LD 	A,(HL)		;Get byte at (HL)
       	OR 	A		;Set Z flag if zero
       	JR 	Z,TABEND

       	AND 	C
       	INC 	HL

NEWMOD 	LD 	B,(HL)
       	INC 	HL
       	INC 	B
       	JR 	Z,NEWMSK

TRYMAT 	CP 	(HL)
       	INC 	HL
       	JR 	Z,GETNDX

       	BIT 	7,(HL)
       	INC 	HL
       	JR 	Z,TRYMAT

       	JR 	NEWMOD

GETNDX 	LD 	A,(HL)		;Load entry in reference table
       	AND 	$7F
       	DEC 	B

TABEND 	POP 	HL
       	PUSH 	DE
       	PUSH 	HL

       	EX 	DE,HL
       	LD 	HL,MONICS		;Start of ASCII Op Code List
       	CALL 	XTRACT		;Store the code at 

       	POP 	HL
       	LD 	DE,5
       	ADD 	HL,DE
       	POP 	DE

       	LD 	A,B
       	AND 	$F0
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
       	AND 	$0F
       	JR 	Z,OPDONE

       	LD 	(HL),44		;ASCII <comma>
       	INC 	HL
;----------------------------------------------------------------------------------------
SECOND 	LD 	A,B
       	AND 	$0F

       	LD 	B,A
       	LD 	A,C
       	CALL 	NZ,OPRND2
;----------------------------------------------------------------------------------------
OPDONE 	LD 	A,3
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
SPACES 	LD 	A,$20		;Print (B) spaces
       	CALL 	CHROP
       	DJNZ 	SPACES		;until done
;----------------------------------------------------------------------------------------
OUTEXT 	LD 	B,18		;Outputs 9 characters of text off stack
PUTOUT 	DEC 	SP		;Keep up with stack pointer
       	POP 	HL		;Pop data into HL, text in H
       	LD 	A,H		;Move text to Accum for printing
       	CALL 	CHROP		;and call it
       	DJNZ 	PUTOUT		;keep it up until all 18 spots popped off stack
       	RET			;Finish out
;----------------------------------------------------------------------------------------
GROUP2 	.BYTE 	$C0,$36,$40
	.BYTE 	$04,$80,$2D,$C0,$BE
	.BYTE 	$FF,$F8,$06,$00,$33
	.BYTE 	$08,$38,$10,$35,$18
	.BYTE 	$3A,$20,$3F,$28,$40
	.BYTE 	$30,$00,$38,$C1
;----------------------------------------------------------------------------------------
GROUP1 	.BYTE 	$FF,$00,$00
	.BYTE 	$24,$07,$32,$0F,$37
	.BYTE 	$17,$31,$1F,$36,$27
	.BYTE 	$0D,$2F,$0B,$37,$3D
	.BYTE 	$3F,$06,$76,$14,$C9
	.BYTE 	$30,$D9,$12,$F3,$0F
	.BYTE 	$FB,$91,$72,$C6,$02
	.BYTE 	$CE,$01,$DE,$BC,$02
	.BYTE 	$D6,$42,$E6,$03,$EE
	.BYTE 	$43,$F6,$25,$FE,$8C
	.BYTE 	$04,$08,$93,$01,$10
	.BYTE 	$10,$18,$9D,$AF,$22
	.BYTE 	$A2,$FA,$2A,$A2,$A7
	.BYTE 	$32,$A2,$7A,$3A,$A2
	.BYTE 	$03,$C3,$1C,$CD,$85
	.BYTE 	$97,$D3,$AA,$79,$DB
	.BYTE 	$9B,$5F,$E3,$93,$0E
	.BYTE 	$E9,$9C,$05,$EB,$93
	.BYTE 	$DF,$F9,$A2,$FF,$C0
	.BYTE 	$B6,$40,$A2,$FF,$F8
	.BYTE 	$76,$80,$02,$88,$01
	.BYTE 	$98,$BC,$06,$90,$42
	.BYTE 	$A0,$03,$A8,$43,$B0
	.BYTE 	$25,$B8,$8C,$FF,$C7
	.BYTE 	$0B,$04,$16,$05,$8E
	.BYTE 	$B2,$06,$A2,$20,$C0
	.BYTE 	$B0,$23,$C2,$1C,$C4
	.BYTE 	$85,$10,$C7,$BB,$FF
	.BYTE 	$CF,$D3,$01,$A2,$0D
	.BYTE 	$03,$16,$0B,$8E,$FD
	.BYTE 	$09,$82,$60,$C1,$2B
	.BYTE 	$C5,$AC,$FF,$E7,$21
	.BYTE 	$20,$9D,$FF,$EF,$E7
	.BYTE 	$02,$A2,$7E,$0A,$A2
;----------------------------------------------------------------------------------------
GROUP3 	.BYTE 	$FF,$00,$44
	.BYTE 	$23,$45,$2F,$4D,$2E
	.BYTE 	$4E,$00,$67,$39,$6F
	.BYTE 	$34,$70,$00,$71,$00
	.BYTE 	$A0,$21,$A1,$0A,$A2
	.BYTE 	$1A,$A3,$29,$A8,$1F
	.BYTE 	$A9,$08,$AA,$18,$AB
	.BYTE 	$28,$B0,$20,$B1,$09
	.BYTE 	$B2,$19,$B3,$27,$B8
	.BYTE 	$1E,$B9,$07,$BA,$17
	.BYTE 	$BB,$A6,$FF,$C7,$B8
	.BYTE 	$40,$9B,$8B,$41,$AA
	.BYTE 	$FF,$CF,$FD,$42,$3C
	.BYTE 	$4A,$81,$AD,$43,$A2
	.BYTE 	$DA,$4B,$A2,$FF,$E7
	.BYTE 	$40,$46,$95,$FF,$F7
	.BYTE 	$C7,$47,$A2,$7C,$57
	.BYTE 	$A2,$FF,$00
;----------------------------------------------------------------------------------------
MONICS 	.BYTE 	$BF
	.BYTE 	'A','D','C'+$80   	; ADC 
	.BYTE 	'A','D','D'+$80   	; ADD 
	.BYTE 	'A','N','D'+$80   	; AND 
	.BYTE 	'B','I','T'+$80   	; BIT 
	.BYTE 	'C','A','L','L'+$80	; CALL 
	.BYTE 	'C','C','F'+$80   	; CCF
	.BYTE 	'C','P','D','R'+$80	; CPDR
	.BYTE 	'C','P','D'+$80   	; CPD
	.BYTE 	'C','P','I','R'+$80	; CPIR
	.BYTE 	'C','P','I'+$80   	; CPI
	.BYTE 	'C','P','L'+$80   	; CPL
	.BYTE 	'C','P'+$80      	; CP 
	.BYTE 	'D','A','A'+$80   	; DAA
	.BYTE 	'D','E','C'+$80   	; DEC 
	.BYTE 	'D','I'+$80      	; DI
	.BYTE 	'D','J','N','Z'+$80	; DJNZ 
	.BYTE 	'E','I'+$80      	; EI
	.BYTE 	'E','X','X'+$80   	; EXX
	.BYTE 	'E','X'+$80      	; EX 
	.BYTE 	'H','A','L','T'+$80	; HALT
	.BYTE 	'I','M'+$80      	; IM 
	.BYTE 	'I','N','C'+$80   	; INC 
	.BYTE 	'I','N','D','R'+$80	; INDR
	.BYTE 	'I','N','D'+$80   	; IND
	.BYTE 	'I','N','I','R'+$80	; INIR
	.BYTE 	'I','N','I'+$80   	; INI
	.BYTE 	'I','N'+$80      	; IN 
	.BYTE 	'J','P'+$80      	; JP 
	.BYTE 	'J','R'+$80      	; JR 
	.BYTE 	'L','D','D','R'+$80	; LDDR
	.BYTE 	'L','D','D'+$80   	; LDD
	.BYTE 	'L','D','I','R'+$80	; LDIR
	.BYTE 	'L','D','I'+$80   	; LDI
	.BYTE 	'L','D'+$80      	; LD 
	.BYTE 	'N','E','G'+$80   	; NEG
	.BYTE 	'N','O','P'+$80   	; NOP
	.BYTE 	'O','R'+$80      	; OR 
	.BYTE 	'O','T','D','R'+$80	; OTDR
	.BYTE 	'O','T','I','R'+$80	; OTIR
	.BYTE 	'O','U','T','D'+$80	; OUTD
	.BYTE 	'O','U','T','I'+$80	; OUTI
	.BYTE 	'O','U','T'+$80   	; OUT 
	.BYTE 	'P','O','P'+$80   	; POP 
	.BYTE 	'P','U','S','H'+$80	; PUSH 
	.BYTE 	'R','E','S'+$80   	; RES 
	.BYTE 	'R','E','T','I'+$80	; RETI
	.BYTE 	'R','E','T','N'+$80	; RETN
	.BYTE 	'R','E','T'+$80   	; RET
	.BYTE 	'R','L','A'+$80   	; RLA
	.BYTE 	'R','L','C','A'+$80	; RLCA
	.BYTE 	'R','L','C'+$80   	; RLC 
	.BYTE 	'R','L','D'+$80   	; RLD
	.BYTE 	'R','L'+$80      	; RL 
	.BYTE 	'R','R','A'+$80   	; RRA
	.BYTE 	'R','R','C','A'+$80	; RA
	.BYTE 	'R','R','C'+$80   	; RRC 
	.BYTE 	'R','R','D'+$80   	; RRD
	.BYTE 	'R','R'+$80      	; RR 
	.BYTE 	'R','S','T'+$80   	; RST 
	.BYTE 	'S','B','C'+$80   	; SBC 
	.BYTE 	'S','C','F'+$80   	; SCF
	.BYTE 	'S','E','T'+$80   	; SET 
	.BYTE 	'S','L','A'+$80   	; SLA 
	.BYTE 	'S','R','A'+$80   	; SRA 
	.BYTE 	'S','R','L'+$80   	; SRL 
	.BYTE 	'S','U','B'+$80   	; SUB 
	.BYTE 	'X','O','R'+$80   	; XOR 
;----------------------------------------------------------------------------------------
OPRND1 	DJNZ 	CONDIT

RSTADR 	AND 	$38
       	JR 	DA

OPRND2 	DJNZ 	DAT8

RELADR 	CALL 	FETCH
       	LD 	C,A
       	RLA
       	SBC 	A,A
       	LD 	B,A
       	EX 	DE,HL
       	PUSH 	HL
       	ADD 	HL,BC
       	JR 	DHL

CONDIT 	RRA
       	RRA
       	RRA
       	DJNZ 	BITNUM

       	BIT 	4,A
       	JR 	NZ,ABS

       	AND 	3
	
ABS    	AND 	7
       	ADD 	A,$14
       	JR 	PS1

DAT8   	DJNZ 	DAT16

D8     	CALL 	FETCH
       	JR 	DA

BITNUM 	DJNZ 	INTMOD
       	AND 	7

DA     	LD 	C,A
       	SUB 	A
       	JR 	DAC

DAT16  	DJNZ 	EXAF
	
D16    	CALL 	FETCH
       	LD 	C,A
       	CALL 	FETCH

DAC    	EX 	DE,HL
       	PUSH 	HL
       	LD 	H,A
       	LD 	L,C

DHL    	LD 	C,$F8
       	PUSH 	HL
       	CALL 	CONVHL
       	POP 	HL
       	LD 	BC,$000A
       	OR 	A		;Clear CY
       	SBC 	HL,BC
       	POP 	HL
       	EX 	DE,HL
       	RET 	C

       	LD 	(HL),'H'		;Loads Char "H" into string
       	INC 	HL		;Point to next location
       	RET
;----------------------------------------------------------------------------------------
INTMOD 	DJNZ 	STKTOP
       	AND 	3
       	ADD 	A,$1C
	
PS1    	JR 	PS3

STKTOP 	LD 	C,$13
       	DEC 	B
       	JR 	Z,PS2

REG16P 	DJNZ 	COMMON
       	RRA
       	AND 	3
       	CP 	3
       	JR 	NZ,RX

       	DEC 	A
       	JR 	RNX

EXAF   	LD 	C,$0A		;Exchange AF,AF'
       	DEC 	B
       	JR 	Z,PS2

EXDE   	INC 	C		;Exchange DE,HL
       	DEC 	B
       	JR 	Z,PS2

REG8S  	DJNZ 	ACCUM

R8     	AND 	7
       	CP 	6
       	JR 	NZ,PS3

       	LD 	(HL),'('		;Bracket
       	INC 	HL
       	CALL 	REGX
       	LD 	A,(IX+2)
       	OR 	A
       	JR 	Z,RP

       	LD 	(HL),43 		;+ Plus sign
       	RLCA
       	RRCA
       	JR 	NC,POS

       	LD 	(HL),45		;- Minus sign
       	NEG			;Negate the Accumulator

POS    	INC 	HL
       	EX 	DE,HL
       	PUSH 	HL
       	LD 	H,B
       	LD 	L,A
       	LD 	C,$FB
       	CALL 	CONVHL
       	POP 	HL
       	EX 	DE,HL
       	JR 	RP

ACCUM  	RRA
       	RRA
       	RRA

COMMON 	LD 	C,7
       	DEC 	B
       	JR 	Z,PS2

PORTC  	DEC 	C
       	DJNZ 	IDAT8

PS2    	LD 	A,C
PS3    	JR 	PS4

IDAT8  	DJNZ 	IDAT16
       	LD 	(HL),'('
       	INC 	HL
       	CALL 	D8
       	JR 	RP

IDAT16 	DJNZ 	REG8
       	LD 	(HL),'('
       	INC 	HL
       	CALL 	D16
       	JR 	RP

REG8   	DEC 	B
       	JR 	Z,R8

IPAREF 	DJNZ 	REG16
       	AND 	9
       	JR 	PS4

REG16  	RRA
       	DJNZ 	IREG16

R16    	AND 	3
RX     	CP  	2
       	JR 	Z,REGX

RNX    	ADD 	A,$0C
       	JR 	PS4

IREG16 	DJNZ 	REGX
       	LD 	(HL),'('
       	INC 	HL
       	CALL 	R16

RP     	LD 	(HL),')'
       	INC 	HL
       	RET

REGX   	LD 	A,(IX+1)
       	ADD 	A,$10

PS4    	EX 	DE,HL
       	PUSH 	HL
       	LD 	HL,RGSTRS		;Point to Registers Table
       	CALL 	XTRACT		;Get register name
       	POP 	HL
       	EX 	DE,HL
       	RET
;----------------------------------------------------------------------------------------
RGSTRS 	.BYTE 	'B'+$80
	.BYTE 	'C'+$80
	.BYTE 	'D'+$80
	.BYTE 	'E'+$80
	.BYTE 	'H'+$80
	.BYTE 	'L'+$80
	.BYTE 	"(","C",')'+$80
	.BYTE 	'A'+$80
	.BYTE 	'I'+$80
	.BYTE 	'R'+$80
	.BYTE 	"A","F",",","A","F",'''+$80
	.BYTE 	"D","E",",","H",'L'	+$80
	.BYTE 	"B",'C'+$80
	.BYTE 	"D",'E'+$80
	.BYTE 	"A",'F'+$80
	.BYTE 	"S",'P'+$80
	.BYTE 	"H",'L'+$80
	.BYTE 	"I",'X'+$80
	.BYTE 	"I",'Y'+$80
	.BYTE 	"(","S","P",')'+$80
	.BYTE 	"N",'Z'+$80
	.BYTE 	'Z'+$80
	.BYTE 	"N",'C'+$80
	.BYTE 	'C'+$80
	.BYTE 	"P",'O'+$80
	.BYTE 	"P",'E'+$80
	.BYTE 	'P'+$80
	.BYTE 	'M'+$80
	.BYTE 	'0'+$80
	.BYTE 	'?'+$80
	.BYTE 	'1'+$80
	.BYTE 	'2'+$80
;----------------------------------------------------------------------------------------
CONVHL 	SUB 	A		; Clear A
CVHL1  	PUSH 	AF		; and store it
       	SUB 	A		; Clear A
       	LD 	B,16		; 16 Iterations
CVHL2  	ADD 	A,C
       	JR 	C,CVHL3
       	SUB 	C
CVHL3  	ADC 	HL,HL
       	RLA
       	DJNZ 	CVHL2
       	JR 	NZ,CVHL1
       	CP 	10		; Affects CY flag if A>10
       	INC 	B		; Does not affect CY
       	JR 	NC,CVHL1
CVHL4  	CP 	10		; Affects CY
       	SBC 	A,$69
       	DAA
       	LD 	(DE),A
       	INC 	DE
       	POP 	AF
       	JR 	NZ,CVHL4
       	RET
;----------------------------------------------------------------------------------------
XTRACT 	OR 	A		; When A=0, correct opcode,register reached in table
       	JR 	Z,COPY		; Copy the opcode to memory

SKIP   	BIT 	7,(HL)		; If not at correct position
       	INC 	HL		; move forward in table
       	JR 	Z,SKIP		; until $80+char found

       	DEC 	A		; Down count through table
       	JR 	NZ,SKIP		; Execute skips over table until A=0
;----------------------------------------------------------------------------------------
COPY   	LD 	A,(HL)		; Get the byte at (HL)
       	RLCA			; Rotates Left D7 into D0 and CY
       	SRL 	A		; Logic Shift Right 0 into D7,D0 into CY
       	LD 	(DE),A		; Stores the stripped char to (DE)
       	INC 	DE		; Increment both pointers
       	INC 	HL
       	JR 	NC,COPY		; Continues until D7="1", end of table entry
       	RET
;----------------------------------------------------------------------------------------
FETCH  	LD 	A,(DE)		; Get byte at start of instruction
       	INC 	DE		; Point to next location
       	INC 	(IX+0)		;
       	PUSH 	AF		; Preserve the instruction
       	CALL 	BYTSP		; Decode it to Hex ASCII and print it
       	POP 	AF		; Retrieve the instruction
       	RET			; and return
;----------------------------------------------------------------------------------------
ADRSP  	LD 	A,D		; Converts DE to ASCII Hex and prints it
       	CALL 	BYTOP		; Convert D first, 
       	LD 	A,E		; Then do E
;----------------------------------------------------------------------------------------
BYTSP  	CALL 	BYTOP		; Convert the byte to ASCII
       	LD 	A,$20		; Print a <space>
       	JP 	CHROP		; output it
;----------------------------------------------------------------------------------------
BYTOP  	PUSH 	AF		; Convert the upper nybble to Hex ASCII first
       	RRA			; Slowly
       	RRA			;   Rotate
       	RRA			;     It
       	RRA			;       Over to the right
       	CALL 	HEXOP		; Convert the nybble D3-D0 to Hex ASCII
       	POP 	AF		; Convert the lower nybble
;----------------------------------------------------------------------------------------
HEXOP  	AND 	$0F		; Convert the nybble at D3-D2-D1-D0 to Hex ASCII char
       	CP 	10		; Neat trick for converting nybble to ASCII
       	SBC 	A,$69
       	DAA			; Uses DAA trick
	RST	08H
	RET
;------------------------------------------------------------------------------
; This routine is the Character Output (CHROP) 
;------------------------------------------------------------------------------
CHROP	RST	08H		; Output the Character
	RET			; Return
;------------------------------------------------------------------------------	
; END OF DISASSEMBLER
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Time Delay Routine
;    Sets up an initial 60uSec delay, adds 20uSec for each value of DE
;    Min value for DE=$0001
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
TDLY	DI			; 20uSec First Part
	PUSH	AF		; Save A & Flags
	PUSH	DE		; Preserves Count for subsequent
	NOP			;  routines to use
	NOP			;   stall
	NOP			;    stall

TLP	DEC	DE		; 20uSec for Each Iteration of this blk
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
CONCLS	CALL	BGRN		; BLUE BACKGROUND
	CALL	CSI		; Control Sequence Introducer (esc[)
	LD	A,'2'		; CLEAR THE SCREEN AND HOME THE CURSOR
	RST	08H
	LD	A,'J'
	RST	08H
	RET			; END THIS TEXT		
;--------------------------------------------------------------------------------------	
; SET COLORS ON ANSI SCREEN	
;--------------------------------------------------------------------------------------
TNRML	XOR	A		; SET $00
	JR	COLOR
TDIM	LD	A,$02		; DIM COLORS
	JR	COLOR
TBRT	LD	A,$01		; BRIGHT COLORS
	JR	COLOR
TUSCR	LD	A,$04		; UNDERSCORE
	JR	COLOR
TBLNK	LD	A,$05		; BLINK
	JR	COLOR
TREV	LD	A,$07		; REVERSE
	JR	COLOR				
BBLK	LD	A,$40		; BLK BACKGROUND
	JR	COLOR
BRED	LD	A,$41		; RED BACKGROUND
	JR	COLOR
BGRN	LD	A,$42		; GREEN BACKGROUND
	JR	COLOR
BYEL	LD	A,$43		; YELLOW BACKGROUND
	JR	COLOR
BBLU	LD	A,$44		; BLUE BACKGROUND
	JR	COLOR
BVIO	LD	A,$45		; VIOLET BACKGROUND
	JR	COLOR
BCYN	LD	A,$46		; CYAN BACKGROUND
	JR	COLOR
BWHT	LD	A,$47		; WHITE BACKGROUND
	JR	COLOR						
TBLK	LD	A,$30		; BLK TEXT
	JR	COLOR
TRED	LD	A,$31		; RED TEXT
	JR	COLOR
TGRN	LD	A,$32		; GRN TEXT
	JR	COLOR
TYEL	LD	A,$33		; YEL TEXT
	JR	COLOR
TBLU	LD	A,$34		; BLU TEXT
	JR	COLOR
TVIO	LD	A,$35		; VIO TEXT
	JR	COLOR
TCYN	LD	A,$36		; CYN TEXT
	JR	COLOR
TWHT	LD	A,$37		; WHT TEXT

COLOR	PUSH	BC		; SAVE BC CONTENTS
	CALL	ATOBC		; CONVERT TO ASCII
	CALL	CSI		; SEND THE INTRODUCER CODE
	CALL	BCOUT		; SEND THE COLOR CODES
	LD	A,'m'		; CLOSE THE SEQUENCE
	RST	08H		; SEND IT
	POP	BC		; RETRIEVE BC
	RET			; AND FINISH
;--------------------------------------------------------------------------------------	
CSI	LD	A,$1B		; ESCAPE
	RST	08H
	LD	A,$5B		; " [ "
	RST	08H
	RET
;------------------------------------------------------------------------------		
; PRINT - Prints a character to all available devices
;  Checks IOFLAG at $400E in non-volatile ram for status of devices
;  D0- TTYA, D1-TTYB (MIDI), D2-LCD Display [we are going to print ttya always]
;------------------------------------------------------------------------------
PRINT	LD	(CHAR),A		; Store character temporarily
	LD	A,(IOFLAG)	; Check I/O Flag
	AND	$07		; See if any output is enabled?
	JR	NZ,PRINT0		; Something is enabled, which one
	OR	$01		; Set Serial A enabled if nothing is
	LD	(IOFLAG),A	; And store it there
	
PRINT0	LD	A,(IOFLAG)	; Get I/O Flag word
	AND	$01		; Is Serial A enabled?
	JR	Z,PRINT1		; No, but something else is	
	RST	08H		; Print it to Serial A

PRINT1	LD	A,(IOFLAG)	; Get I/O Flag word
	AND	$02		; Is Serial B enabled?
	JR	Z,PRINT2		; No, jump over
	LD	A,(CHAR)		; Yes, Retrieve the character
	CALL	TXB		; And print it to Serial B

; Handle LCD special characters
PRINT2	LD	A,(IOFLAG)	; Get I/O Flag word
	AND	$04		; Is the LCD Enabled?
	RET	Z		; If no, then we are finished here
	LD	A,(CHAR)		; Else, retrieve the character
	CP	$0D		; Carriage Return
	JP	Z,LCDHOME		; CR=HOME for now
	CP	$0A		; Line Feed
	JP	Z,LCDHOME		; LF=HOME for now
	
	CP	$0C		; FF=CLS
	JP	Z,LCDCLS		; Clear takes 1.64mSec, may see delay
	CP	$08		; Backspace
	JR	NZ,PRINT3		; Must be normal ASCII char
BKSPC	IN	A,($7C)		; Get DDRAM address
	OR	A		; See if zero
	RET	Z		; Already at home
	DEC	A		; Reduce address by one
	JP	LCDSETP		; Set the new position
PRINT3	CALL	LCDCHAR		; Print it to the LCD Display
	RET			; Finished
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; RXA - Receive a byte over SIO/0 Ch A
;------------------------------------------------------------------------------
RXA	CALL	CKSIOA		; Get the status word
	JR	NC,RXA		; Loop until a character arrives
	IN	A,(RBR)		; Get the character
	RET			; Char ready in A
;------------------------------------------------------------------------------
; TXA - Transmit a byte over SIO/0 Ch A
;------------------------------------------------------------------------------
TXA	PUSH	AF		; Store character
	CALL	CKSIOA		; See if SIO channel A is finished transmitting
	JR	Z,TXA+1		; Loop until SIO flag signals ready
	POP	AF		; Retrieve character
	OUT	(THR),A		; Output the character
	RET
;------------------------------------------------------------------------------
; RXB - Receive a byte over SIO/0 Ch B
;------------------------------------------------------------------------------
RXB	CALL	CKSIOB		; Get the status word
	JR	NC,RXB		; Loop until character arrives
	IN	A,($75)		; Get the character
	RET			; Char ready in A	
;------------------------------------------------------------------------------
; TXB - Transmit a byte over SIO/0 Ch B
;------------------------------------------------------------------------------
TXB	PUSH	AF		; Store character
	CALL	CKSIOB		; See if SIO channel B is finished transmitting
	JR	Z,TXB+1		; Loop until SIO flag signals ready
	POP	AF		; Retrieve character
	OUT	($75),A		; Output the character
	RET
;------------------------------------------------------------------------------
; Check SIO Channel A status flag, RX char ready=CY, TX buffer clear=NZ
;------------------------------------------------------------------------------
CKSIOA	IN	a,(LSR)		;Retrieve Status Word
	RRCA			;RX status into CY flag
	BIT	4,A		;TX Buffer Empty into Z flag
	RET
	
;	XOR	A		; Zeroize A
;	OUT	($76),A		; Select Register 0
;	IN	A,($76)		;Retrieve Status Word
;	RRCA			;RX status into CY flag
;	BIT	1,A		;TX Buffer Empty into Z flag
;	RET
;------------------------------------------------------------------------------
; Check SIO Channel B status flag, RX char ready=CY, TX buffer clear=NZ
;------------------------------------------------------------------------------
CKSIOB	XOR	A		; Zeroize A
	OUT	($77),A		; Select Register 0
	IN	A,($77)		; Retrieve Status Word
	RRCA			; RX status into CY flag
	BIT	1,A		; TX Buffer Empty into Z flag
	RET
; Write character to 8279 Display, check for Display Available before write.
;------------------------------------------------------------------------------
TX8279	PUSH	AF		; Save char and flags
	IN	A,($79)		; Get 8279 status word
	BIT	7,A		; D7=Display Unavailable flag
	JR	NZ,TX8279+1	; Loop here until Display Available
	POP	AF		; Retrieve char and flags
	OUT	($78),A		; Write char to display
	RET
;------------------------------------------------------------------------------
; Wait on 8279 for Keyboard character
;------------------------------------------------------------------------------
RX8279	CALL	CK8279		; Get 8279 status, Z if queue empty
	JR	Z,RX8279		; Loop until key is down
	IN	A,($78)		; Get the key value
	RET			; And return
;------------------------------------------------------------------------------
; Check 8279 for Keyboard character, Return Z set if nothing
;------------------------------------------------------------------------------
CK8279	IN	A,($79)		; Get 8279 status word
	AND	$07		; # of chars in buffer
	RET			; Return Z set if no characters

;------------------------------------------------------------------------------
;                         L C D ROUTINES
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; LCD Display - Write a Character
;------------------------------------------------------------------------------
LCDCHAR	PUSH	AF		; Save the character
	CALL	LCDBUSY		; Wait for the LCD to clear
	POP	AF		; Retrieve the character
	OUT	($7D),A		; Output it
	RET			
;------------------------------------------------------------------------------
; LCD Display - Clear the screen
;------------------------------------------------------------------------------
LCDCLS	LD	A,$01		; 1.64mSec to clear
	JR	LCDCMD		; Wait for LCD to perform and return
;------------------------------------------------------------------------------
; LCD Home Cursor - Command $02 takes 1.64mSec, or reposition takes 40uSec
;------------------------------------------------------------------------------
LCDHOME	XOR	A		; Set cursor position to zero
;------------------------------------------------------------------------------
; LCD Set Cursor Position - Range is $00-$27, $40-$67, or with $80 to set
;------------------------------------------------------------------------------
LCDSETP	OR	$80		; Set D7 to set DDRAM address, continue
;------------------------------------------------------------------------------
LCDCMD	PUSH	AF		; Save the command
	CALL	LCDBUSY		; See if the LCD is still in use
	POP	AF
	OUT	($7C),A		; Write a command to the LCD register
	RET
;------------------------------------------------------------------------------
LCDBUSY	IN	A,($7C)		; Get status word
	AND	$80		; Strip off all but Busy Flag BF
	JR	NZ,LCDBUSY	; Wait until it finishes
	RET
;------------------------------------------------------------------------------
LCDGETS	IN	A,($7C)		; Retrieves Status Word from LCD
	RET
;------------------------------------------------------------------------------
LCDGETD	CALL	LCDBUSY		; Wait on LCD
	IN	A,($7D)		; Retrieve Data under cursor from LCD
	RET
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; Print the bootup message on the LCD
;------------------------------------------------------------------------------
LCDMSG	CALL	LCDCLS		; Clear the LCD
	LD	HL,LCDTXT1	; Point to first line of text
LCDMSG1	LD	A,(HL)		; Get character
	OR	A		; Terminator?
	JR	Z,LCDMSG2
	CALL	LCDCHAR		; Write the character
	INC	HL		; Next character
	JR	LCDMSG1		; Loop until line done
LCDMSG2	LD	HL,LCDTXT2	; Point to next line of text
	LD	A,$40		; Second line
	CALL	LCDSETP		; Set the position
LCDMSG3	LD	A,(HL)		; Get the character
	OR	A		; Terminator?
	JR	Z,LCDMSG4		; Finished
	CALL	LCDCHAR		; Write the character
	INC	HL		; Next char
	JR	LCDMSG3		; Loop until finished		
LCDMSG4	RET
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
INIT	LD   SP,$FFD0		; Set the Stack Pointer
	
INITPIT	LD   E,$02		; PIT(CTC) # 2	
	LD   HL,$0004		; 31,250 Baud (MIDI) x 16= 500,000 Hz (Countdown=4)	
	CALL PIT1			; Write it to PIT
	LD   E,$01		; PIT(CTC) # 1	
	LD   HL,$0018		; 4800 Baud x 16 = 76,800 Hz (Countdown=24)	
	CALL PIT1			; Write it to PIT
	LD   E,$00		; PIT(CTC) # 0	
	LD   HL,$0003		; 38400 Baud x 16 = 614,400 Hz (Countdown=3)
	CALL PIT1			; Write it to PIT

UART1_BASE	.EQU	$00
RBR	.EQU	UART1_BASE + $00
THR	.EQU	UART1_BASE + $00
IER	.EQU	UART1_BASE + $01
FCR	.EQU	UART1_BASE + $02
LCR	.EQU	UART1_BASE + $03
MCR	.EQU	UART1_BASE + $04
LSR	.EQU	UART1_BASE + $05
MSR	.EQU	UART1_BASE + $06
DLL	.EQU	UART1_BASE + $00
DLM	.EQU	UART1_BASE + $01

INIT16550
	LD	A,$80
	OUT	(LCR),A		; set baud divisor latch
	LD	A,$00
	OUT	(DLM),A		; set baud dividor
	LD	A,$51
	OUT	(DLL),A		; set baud dividor
	LD	A,$03
	OUT	(LCR),A		; set divisor latch to 1 and 8 bit word length
	LD	A,$00
	OUT	(MCR),A		; set baud dividor

	LD	A,$30
	OUT	(THR),A		; set baud dividor
	
;------------------------------------------------------------------------------
; Set the SIO device settings
;------------------------------------------------------------------------------
INITSIO	LD   HL,SIODAT		; Location of SIO control strings
	LD   C,$76		; Z80 SIO Port A Ctrl
	LD   B,$0A		; Number of control characters           		
	OUTI			; Output it to the port
	NOP			; Brief time delay
	OUTI			; Output the next byte
	NOP			; Again, delay briefly
	OTIR			; Then dump the rest of them to the SIO
;--------------------------------------------------------------------------------------
;INITIALIZE 8279 KEYBOARD/DISPLAY CONTROLLER
;--------------------------------------------------------------------------------------
INITKBD	LD	A,$01		;DECODED SCAN LINES 1 OF 4, LOW ON SEL
	OUT	($79),A
	LD	A,$34		;DIVIDE CLOCK BY 20, 100KHz
	OUT	($79),A
	LD	A,$D3		;CLEAR DISPLAY AND FIFO
	OUT	($79),A
	LD	A,$40		;LOAD FIFO INTO DATA BUFFER
	OUT	($79),A
;--------------------------------------------------------------------------------------
;INITIALIZE 8255 PIO'S
;--------------------------------------------------------------------------------------
INITPIO	LD	A,$89		;PORTS A,B=OUT C=INPUT
	OUT	($63),A		;8255 # 1
	OUT	($67),A		;8255 # 2
	OUT	($6B),A		;8255 # 3
;------------------------------------------------------------------------------
; INITIALIZE THE LCD DISPLAY - HD44780 Controller 40 x 2 Display
;------------------------------------------------------------------------------
INITLCD	LD	A,$38		; ESTABLISH 8 BIT DATA PATHWAY
	LD	DE,$00FC		; BF Busy Flag can't be checked until
	OUT	($7C),A		; After Interface function is set with
	CALL	TDLY		; the LCD display
	OUT	($7C),A
	CALL	TDLY
	OUT	($7C),A
	CALL	TDLY
	OUT	($7C),A
	LD	A,$06		; Cursor Increments, No Disp. Shift
	OUT	($7C),A
	CALL	LCDBUSY		; Check LCD Status for Busy
	LD	A,$0D		; Display On, Cursor Off, Blink On
	OUT	($7C),A
	CALL	LCDBUSY		; Check LCD Status for Busy
	LD	A,$14		; Cursor moves, Shifts to the Right
	OUT	($7C),A
	CALL	LCDBUSY		; Check LCD Status for Busy
	LD	A,$38		; 8-bit data, 2 Lines, 5x7 font
	OUT	($7C),A
	CALL	LCDBUSY		; Check LCD Status for Busy
	LD	HL,CGRAM		; Start Custom character map
	LD	A,$40		; Set start of CGRAM in LCD
	OUT	($7C),A
INITLCD0	CALL	LCDBUSY		; Check LCD Status for Busy
	LD	A,(HL)		; Get byte for custom character
	CP	$80		; At end of list?
	JR	Z,INITLCD1	; Continue forward
	OUT	($7D),A		; Write it to CGRAM
	INC	HL		; Next location
	JR	INITLCD0		; Loop thru until all chars output
	
INITLCD1	CALL	LCDCLS		; Clear the LCD Display
	
;------------------------------------------------------------------------------		
; Initialize the Screen Last, Run Rom Checksums
;------------------------------------------------------------------------------
INITFIN	CALL	LCDMSG		; Put message on the LCD
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
CHGBAUD	CALL GETXY		; Get xxxx (DE), yyyy (HL)
	RET  C			; User may break out of this command
PIT1	SUB  A			; Zero out A and clear the flags
	CP   E			; E contains PIT #
	LD   A,$36		; Data for PIT control
	LD   C,$6C		; PIT Counter 0
	LD   B,E
	JR   Z,PIT3		; Program that particular counter
PIT2	ADD  A,$40		; Next PIT counter
	INC  C			; Next I/O Port address
	DJNZ PIT2            		

PIT3	OUT  ($6F),A		; Output PIT control byte
	OUT  (C),L		; and load the counter low order byte
	OUT  (C),H		; Then the high order byte
	RET 
;------------------------------------------------------------------------------
; Calculate ROM checksums
;------------------------------------------------------------------------------
RMCKSM	LD	HL,ROM0		; THIS MONITOR ROM
	CALL	RCKSUM		; CALC THE CHECKSUM
	CALL	TXSPC		; PRINT A SPACE
;--------------------------------------------------------------------------------------	
	LD	HL,ROM1		; BASIC ONE ROM
	CALL	RCKSUM		; GET THE CHECKSUM
	CALL	TXSPC		; PRINT A SPACE
;--------------------------------------------------------------------------------------		
	LD	HL,ROM2
	CALL	RCKSUM		; GET THE CHECKSUM IN DE
	CALL	TXSPC		; PRINT ANOTHER SPACE
;--------------------------------------------------------------------------------------		
	LD	HL,ROM3
	CALL	RCKSUM		; GET THE CHECKSUM IN DE
;--------------------------------------------------------------------------------------		
	CALL	TXCRLF		; TXCRLF
	RET
;--------------------------------------------------------------------------------------
RCKSUM	LD	BC,$1000		; GENERATE THE CHECKSUM IN DE
	LD	DE,$0000		; Initialize DE to zero
CKSUM1	LD	A,(HL)		; Load memory location
	ADD	A,E		; Add its contents to E
	LD	E,A		; Move it back to E
	JR	NC,NOADD		; See if there was a carry from that add
	INC	D		; Yes, increment D also
NOADD	INC	HL		; Get next location
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
TESTMSG	.BYTE	$0D,$0A
	.BYTE	"Hello World",$0D,$0A
	.BYTE	$00
;------------------------------------------------------------------------------
HELPSCR	.BYTE	$0D,$0A
	.BYTE	"A			JUMP TO $0000",$0D,$0A
	.BYTE	"Bxx,yyyy		CHANGE BAUD RATE",$0D,$0A
	.BYTE	"Cxxxx,yyyy,zzzz	COMPARE MEM",$0D,$0A
	.BYTE	"Dxxxx,yyyy		DISP MEM",$0D,$0A
	.BYTE	"E			EDIT MEM",$0D,$0A
	.BYTE	"Gxxxx,[yyyy]		EXECUTE MEM",$0D,$0A
	.BYTE	"Hxxxx,yyyy		HEX SUM/DIFF",$0D,$0A
	.BYTE	"Ixx			PORT INPUT",$0D,$0A
	.BYTE	"Jxxxx,yyyy		RAM TEST",$0D,$0A
	.BYTE	"K			KILL BRKPT",$0D,$0A
	.BYTE	"L			LOAD IHEX",$0D,$0A
	.BYTE	"Mxxxx,yyyy,ZZZZ	MOVE MEM",$0D,$0A
	.BYTE	"Nxxxx,yyyy		DISASM MEM",$0D,$0A
	.BYTE	"Oxx,yy			PORT OUTPUT",$0D,$0A
	.BYTE	"Pxxxx,ASC STRING	ASCII TO MEM",$0D,$0A
	.BYTE	"Q			DISP STACK",$0D,$0A
	.BYTE	"R			DISP REGS",$0D,$0A
	.BYTE	"Sxxxx			MODIFY MEM",$0D,$0A
	.BYTE	"Txxxx,yyyy		TYPE ASCII",$0D,$0A
	.BYTE	$00
;------------------------------------------------------------------------------
SIGNON	.BYTE	$0C
	.BYTE	"Space-Time Productions"
	.BYTE	$0D,$0A
	.BYTE	"Z-80 Monitor Rom - Rev 9.01"
	.BYTE	$0D,$0A
	.BYTE	"(c) July 25, Joel Owens"
	.BYTE	$0D,$0A,$00
;------------------------------------------------------------------------------
CKSUMERR	.BYTE	"Checksum Error"
	.BYTE	$0D,$0A,$00
;------------------------------------------------------------------------------
LCDTXT1	.BYTE	"Z-80 Monitor Rom - Rev 9.01"
	.BYTE	$00
LCDTXT2	.BYTE	"(c) July 25, 2006 "
	.BYTE	"Joel Owens"
	.BYTE	$00	
;------------------------------------------------------------------------------
DMPTEXT	.TEXT	"AAAA  00 01 02 03 04 05 "
	.TEXT	"06 07 08 09 0A 0B 0C 0D "
	.TEXT	"0E 0F  0123456789ABCDEF"
	.BYTE	$0D,$0A
	
	.TEXT	"------------------------"
	.TEXT	"------------------------"
	.TEXT	"-----------------------"
	.BYTE	$00
;------------------------------------------------------------------------------
RTXT_A	.BYTE	"A =",$00
RTXT_BC	.BYTE	" BC =",$00
RTXT_DE	.BYTE	" DE =",$00
RTXT_HL	.BYTE	" HL =",$00

RTXT_AP	.BYTE	"A",$27,"=",$00
RTXT_BCP	.BYTE	" BC",$27,"=",$00
RTXT_DEP	.BYTE	" DE",$27,"=",$00
RTXT_HLP	.BYTE	" HL",$27,"=",$00

RTXT_PC	.BYTE	"PC=",$00
RTXT_IX	.BYTE	" IX=",$00
RTXT_IY	.BYTE	" IY=",$00

RTXT_S	.BYTE	" S",$00
RTXT_Z	.BYTE	" Z",$00
RTXT_H	.BYTE	" H",$00
RTXT_PV	.BYTE	" P/V",$00
RTXT_N	.BYTE	" N",$00
RTXT_C	.BYTE	" C",$00
;------------------------------------------------------------------------------
;BITMAPS FOR LCD CHARS $00-$07
;------------------------------------------------------------------------------
CGRAM	.BYTE	$00		;CHAR(0) Space:1999 EAGLE TAIL
	.BYTE	11111110B
	.BYTE	11111111B
	.BYTE	11111111B
	.BYTE	11111110B
	.BYTE	$00
	.BYTE	$00
	.BYTE	$00

	.BYTE	11101111B		;CHAR(1) EAGLE 1ST LANDPAD
	.BYTE	11111111B
	.BYTE	11110001B
	.BYTE	11110001B
	.BYTE	11110001B
	.BYTE	11101110B
	.BYTE	11101110B
	.BYTE	$00
	
	.BYTE	$FF		;CHAR(2) EAGLE
	.BYTE	11100000B
	.BYTE	11111111B
	.BYTE	11111111B
	.BYTE	11111111B
	.BYTE	11100110B
	.BYTE	11100000B
	.BYTE	$00
	
	.BYTE	$FF		;CHAR(3) EAGLE
	.BYTE	11100000B
	.BYTE	11111111B
	.BYTE	11111111B
	.BYTE	11111111B	
	.BYTE	11100000B
	.BYTE	11100000B
	.BYTE	$00

	.BYTE	$FF		;CHAR(4) EAGLE
	.BYTE	11100000B
	.BYTE	11111111B
	.BYTE	11111111B
	.BYTE	11111111B
	.BYTE	11101100B
	.BYTE	11100000B
	.BYTE	$00

	.BYTE	11111110B		;CHAR(5) EAGLE 2ND LANDPAD
	.BYTE	11111111B
	.BYTE	11110001B
	.BYTE	11110001B
	.BYTE	11110001B
	.BYTE	11101110B
	.BYTE	11101110B	
	.BYTE	$00

	.BYTE	11100000B		;CHAR(6) EAGLE COCKPIT
	.BYTE	11111110B
	.BYTE	11110001B
	.BYTE	11111111B
	.BYTE	11110001B
	.BYTE	11111110B
	.BYTE	11100000B
	.BYTE	$00

	.BYTE	00010000B		;CHAR(7) ARROW HEAD MARKER
	.BYTE	00011000B
	.BYTE	00011100B
	.BYTE	00011110B
	.BYTE	00011100B
	.BYTE	00011000B
	.BYTE	00010000B
	.BYTE	$00
	.BYTE	$80		;TERMINATOR FLAG
;------------------------------------------------------------------------------
; Data bytes for the SIO
;------------------------------------------------------------------------------
SIODAT 	.BYTE	$18		; SIO CONTROL, RESET CH, SELECT REGISTER 0
	.BYTE	$18		; WRITE DATA TO REGISTER 0
	.BYTE	$04		; SELECT REGISTER 4
	.BYTE	$44		; X 16 CLOCK, 1 STOP BIT
	.BYTE	$05		; SELECT REGISTER 5
	.BYTE	$EA		; DTR, TX 8 BITS/1 STOP, RTS, TX ENABLE
	.BYTE	$03		; SELECT REGISTER 3
	.BYTE	$C1		; RX 8 BITS/CHAR, RX ENABLE
	.BYTE	$01		; SELECT REGISTER 1
	.BYTE	$00		; DISABLE WAIT/READY AND INTERRUPT MODES
;------------------------------------------------------------------------------
TDATA	.BYTE	"Joel Owens"
	.BYTE	$0D,$0A
	.BYTE	"1401 Herring"
	.BYTE	$0D,$0A
	.BYTE	"Merkel, Texas 79536"
	.BYTE	$0D,$0A
	.BYTE	"owens_joel@yahoo.com"
	.BYTE	$0D,$0A,$00
;------------------------------------------------------------------------------	

FINIS       .END
