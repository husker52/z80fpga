sim	.equ	0
	.area	TEXT	(CSEG,ABS)
;	.local	RXA
;************************
;RAM .EQUATES:         ³Use these equates for Static Ram @ 8000H Version.
;OFFSET  = 0x8000;³Assembly Offset
;SEV_UP  = 0x000 ;³Clr bit 7 in Address tables. Will error if not done.
;MASK_7  = 0x0FF ;³Mask for above Assembly
;USR_OFF = 0x080 ;³Adjust USR Jump Addresses
;************************
;************************
;ROM .EQUATES:         ³Use these equates for EPROM @ 0000H Version.
OFFSET  = 0x0000;³Assembly offset
SEV_UP  = 0x8000 ;³Set bit seven in Address tables.
MASK_7  = 0x07F ;³Mask for above Assembly
USR_OFF = 0x000 ;³Adjust USR Jump Addresses
;************************
;---------------------------------------------------------
;- VERSION 2.0 UPDATED 26-AUG-93 FOR NEW VERS 2.0 ZLOAD  -
;- SAVE 9F80H TO 9FFFH FOR HOUSEKEEPING RAM FOR ZLOAD    -
;---------------------------------------------------------
;Z8TBASIC [Z80 Tiny Basic] Ver 1.1 by Don McKenzie Updated 30-July-92(c)
;29 Ellesmere Cres., Tullamarine 3043 Australia. Ph 03-338-6286
;Fixed 1.0 Zload to PC video comm. problems..
;
;Original Version 1.0, 13-May-92
;*************************************************************
PITBASE	= 0x60
COUNT   = 0x20

INPORT  = 0x60
OTPORT  = 0x61
HSHAKE  = 0x62
STATUS  = 0x63

CR      = 0x0D
LF      = 0x0A
ESC     = 0x1B

OPEN    = 15
DELETE  = 19
READD   = 20

;**************************************************************
        .ORG     0H+OFFSET       ;WE START HERE @ ADD ZERO
USR000:
        DI                      ;ADD 0 DISABLE INTS.
        JP	START
;        JP      DELAYS          ;ADD 1, 2, AND 3
;**************************************************************
        .ORG     0x4+OFFSET
FLMOVE: .BYTE    0x0FF            ;@ 04H DISABLE MOVE
RES5:   .BYTE    0x0FF            ;@ 05H ENABLE ;;WAS DISK
RES6:   .BYTE    0x0FF            ;@ 06H ENABLE ;;WAS KEYBOARD
RES7:   .BYTE    0x0FF            ;@ 07H ENABLE ;;WAS VIDEO

;**************************************************************
        .ORG     8*1+OFFSET      ;8H
TSTCHR: EX      (SP),HL         ;*** TSTC OR RST 1 ***
        RST	0x28             ; (8*5) ;IGNORE BLANKS AND
        CP      (HL)            ;TEST CHARACTER
        JP      TC1
;**************************************************************
        .ORG     8*2+OFFSET      ;0x10
TSTVAR: RST     0x28	;8*5             ;CALL IGNBLK FIRST; *** TSTV OR RST 2;(7) ***
        SUB     0x40             ;TEST VARIABLES
        RET     C               ;C:NOT A VARIABLE
        JP      TSTV1
;**************************************************************
        .ORG     8*3+OFFSET      ;0x18
EXP:    CALL    EXPR2           ;*** EXP OR OLD RST 3 ***
        PUSH    HL              ;EVALUATE AN EXPRESION
        JP      EXPR1           ;REST OF IT IS AT EXPR1
;**************************************************************
        .ORG     8*4+OFFSET      ;0x20
COMPAR: LD      A,H             ;*** COMP OR OLD RST 4 ***
        CP      D               ;COMPARE HL WITH DE
        RET     NZ              ;RETURN CORRECT C AND
        LD      A,L             ;Z FLAGS
        CP      E               ;BUT OLD A IS LOST
        RET
;**************************************************************
        .ORG     8*5+OFFSET      ;0x28
IGNBLK: LD      A,(DE)          ;*** IGNBLK/RST 5 ***
        CP      0x20             ;IGNORE BLANKS
        RET     NZ              ;IN TEXT (WHERE DE->)
        INC     DE              ;AND RETURN THE FIRST
        JP      IGNBLK          ;NON-BLANK CHAR. IN A
;**************************************************************
        .ORG     8*6+OFFSET      ;0x30
FINISH: POP     AF              ;*** FINISH/RST 6 ***
        CALL    FIN             ;CHECK END OF COMMAND
        JP      QWHAT           ;PRINT "WHAT?" IF WRONG
;**************************************************************
;        .BLOCK    0x38+OFFSET-$           ;PIN 16 555 TIMER @ 0x38
	.ORG	0x38 + OFFSET
INTVEC: DI
        PUSH    AF
        POP     AF
        EI
        RETI
;**************************************************************
	.ORG	0x40 + OFFSET
INIT_8255:

        PUSH    AF
        PUSH    BC
        LD      BC,PITBASE+0x03          ;SET STATUS PORT 3
        LD      A,0x0BD          ;A=IN, B=OUT, C=HSHAKE
        OUT     (C),A           ;WR PORT STATUS (3)         
        LD      A,PITBASE+0x005          ;SET BIT 2 (ENABLE O/P INT)
        OUT     (C),A           ;WR PORT          
        LD      A,PITBASE+0x009          ;SET BIT 4 (ENABLE I/P INT)
        OUT     (C),A           ;WR PORT          
        POP     BC
        POP     AF
        RET
;**************************************************************
;        .blkb	0x66+OFFSET-.    ;PIN 17 TEST @ 66H
		.org	0x66+OFFSET

;------------------------------------------------------------------------------
; [NMI VECTOR]
; The NMI signal can cause a program to break and save all its register values
; The INT signal would work the same if the processor is put into IM 1, otherwise
; It is not set in this rom, and would default to the 8080 mode where, on INT-, the
; Interrupting device (or some other rigged hardware) would supply the necessary RST
;------------------------------------------------------------------------------
RST66:	
	push	af
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
		
PIN17:  JP      NMIVEC
;**************************************************************
USR_TABLE:

USR105: JP      DISPL           ;CONV TO 7 SEG AND DISP LOW BYTE TO FPIO LEDS
USR108: JP      DISPH           ;CONV TO 7 SEG AND DISP HIGH BYTE TO FPIO LEDS
USR111: JP      GETIN           ;INPUT TO 8255 PORT 0
USR114: JP      LPRINT          ;PRINT VALUE VIA 8255 PORT 1
USR117: JP      DELAY           ;PASS DELAY VIA PARAM.
USR120: JP      INANDOUT        ;BYTE IN/BYTE OUT
USR123: JP      SETPRINT        ;PRINTER OR VIDEO?
USR126: JP      INIT_8255       ;8255 TO A=IN, B=OUT, C=HSHAKE
USR129: JP      CUSTOM_DIS      ;DISP HL TO FPIO LEDS

;**************************************************************
DISPL:  PUSH    AF
        LD      A,L
        JR      DISHP
;**************************************************************
DISPH:  PUSH    AF
        LD      A,H
DISHP:  CALL    DEBUG
        POP     AF
        RET
;**************************************************************
CUSTOM_DIS:

        PUSH    AF
        PUSH    BC
        PUSH    DE
        PUSH    HL              ;SAVE REGS
        LD      A,H
        RLCA                    ;SHIFT LEFT TO BIT 1
        RLCA    
        LD      D,A             ;SAVE IN D
        LD      A,L             ;SET UP FOR LSB
        CALL    LEDP
        POP     HL
        POP     DE
        POP     BC
        POP     AF
        RET
;**************************************************************
GETIN:  PUSH    AF
        PUSH    BC
WAIT4:  LD      BC,0x02          ;SET HSHAKE PORT
        IN      A,(C)           ;READ HANDSHAKE          
        BIT     3,A             ;IS I/P STROBE THERE?
        JR      Z,WAIT4
        LD      C,0
        IN      L,(C)
        LD      H,0
        POP     BC
        POP     AF
        RET
;**************************************************************
LPRINT: PUSH    AF
        PUSH    BC
LPLOOP: LD      BC,2           ;SET TO STATUS PORT
        IN      A,(C)          ;GET STATUS
        BIT     6,A            ;OUTPUT PORT BUSY?
        JR      NZ,LPLOOP      ;BUSY, GO AWAY
        BIT     1,A            ;O/P BUFFER FULL?
        JR      Z,LPLOOP       ;BUSY, GO AWAY
        LD      C,PITBASE+1            ;SET TO O/P PORT
        OUT     (C),L          ;O/P BYTE
        LD      B,0x20          ;SET A13
        IN      A,(C)          ;STROBE OUTPUT
        POP     BC
        POP     AF
        RET
;**************************************************************
DELAY:  PUSH    AF

HL_LOOP:
        DEC     HL
        CALL    DELAYING
        LD      A,H
        OR      L
        JR      NZ,HL_LOOP
        POP     AF
        RET

DELAYING:

        PUSH    BC
        LD      BC,0
DELOOP: DEC     BC
        LD      A,B
        OR      C
        JR      NZ,DELOOP
        POP     BC
        RET
;**************************************************************
INANDOUT:
        CALL    INIT_8255
INOUT:  CALL    GETIN
        CALL    LPRINT
        JP      INOUT
;**************************************************************
SETPRINT:
        CALL    INIT_8255
        PUSH    AF
        LD      A,L
        LD      (LPFLAG),A
        POP     AF
        RET
;**************************************************************


;**************************************************************
NMIVEC: LD      SP,STACK
        LD      BC,0
NMILOP: DEC     BC              ;GIVE TEST BUTTON
        LD      A,B             ;TIME TO SETTLE DOWN
        OR      C               ;       
        JR      NZ,NMILOP       ;
        CALL    INIT_8255

        CALL    ONE_ON          ;ENABLE LINE ONE
        JP      RSTART          ;READY PROMPT
;**************************************************************
FANFARE:
		.byte	CR
        .ascii	"Z8TBASIC [Z80 Tiny Basic] Ver "
VERS:   .ascii	"2.0 by Don McKenzie "
		.byte	CR,LF
;**************************************************************
DATE:   .ascii	"Updated 26-August-93(c)"
		.byte	CR,0
;**************************************************************
DELAYS: LD      BC,0
STRTLP: DEC     BC              ;GIVE SYSTEM
        LD      A,B             ;TIME TO SETTLE DOWN
        OR      C               ;       
        JR      NZ,STRTLP       ;
LOOP2:  DEC     BC              ;
        LD      A,B             ;
        OR      C               ;
        JR      NZ,LOOP2        ;
LOOP3:  DEC     BC              ;
        LD      A,B             ;
        OR      C               ;
        JR      NZ,LOOP3        ;APPROX 1 SEC DELAY
        JP      START
;**************************************************************
PRINTIT:
        LD      L,E
        CALL    LPRINT
        RET
;**************************************************************
VIDEX:  POP     AF
        RET
;**************************************************************
VIDEO:  PUSH    AF
        LD      A,(VIDEOF)      ;GET VIDEO FLAG
        OR      A               ;TEST IT
        JR      Z,VIDEX
        POP     AF
;**************************************************************
;BUG
;PORT B DATA BIT 0 IS DATA OUT
;D1 IS TOGGLE STROBE. DETECTS CHANGE OF STATE. STARTS HI, FINISHES HI.
OUTPUTM:
        LD      E,A
        LD      A,(LPFLAG)
        OR      A
        JR      NZ,PRINTIT
        LD      A,E

OUTPUT:

;        PUSH    AF
;        LD      BC,COUNT        ;DELAY
;DELOP3: DEC     BC
;        LD      A,B
;        OR      C
;        JR      NZ,DELOP3
;        POP     AF

        PUSH    AF
        LD      BC,2
OUTLO4:
        IN      A,(C)           ;READ
        BIT     1,A             ;O/P BUFFER FULL?
        JR      Z,OUTLO4        ;BUSY, GO AWAY
        POP     AF

        LD      E,A             ;SAVE BYTE
        LD      B,0             ;SET FOR ZLOAD PORT ADDRESSING
        LD      C,OTPORT        ;PORT B

        CALL    LOWER           ;LOWER STROBE ON XMIT 1
        CALL    RAISE           ;RAISE STROBE ON XMIT 2
        CALL    LOWER           ;LOWER STROBE ON XMIT 3
        CALL    RAISE           ;RAISE STROBE ON XMIT 4
        CALL    LOWER           ;LOWER STROBE ON XMIT 5
        CALL    RAISE           ;RAISE STROBE ON XMIT 6
        CALL    LOWER           ;LOWER STROBE ON XMIT 7
        CALL    RAISE           ;RAISE STROBE ON XMIT 8
        RET
;**************************************************************
TSTCNT: OUT     (C),A           ;O/P 1 BIT TO PORT B
        INC     C               ;PORT C HANDSHAKE
OUTLOP: IN      A,(C)           ;READ
        BIT     1,A             ;O/P BUFFER FULL?
        JR      Z,OUTLOP        ;BUSY, GO AWAY

OUTLO2: IN      A,(C)           ;READ
        BIT     1,A             ;O/P BUFFER FULL?
        JR      Z,OUTLOP        ;BUSY, GO AWAY


        LD      BC,COUNT        ;DELAY
DELOP3: DEC     BC
        LD      A,B
        OR      C
        JR      NZ,DELOP3

        LD      BC,1            ;SET TO PORT B

        LD      A,E
        RRCA
        LD      E,A             ;SHIFT 1 BIT

        RET
;**************************************************************
LOWER:
        SET     1,A
        OUT     (C),A
        RES     1,A             ;LOWER STROBE ON XMIT 1,3,5,7
        CALL    TSTCNT          ;BLIND ON FIRST, TEST AFTER FACT...
        RET
;**************************************************************
RAISE:
        RES     1,A
        OUT     (C),A
        SET     1,A             ;RAISE STROBE ON XMIT 2,4,6,8
        CALL    TSTCNT
        RET
;**************************************************************
TSTKEY: LD      A,(KBDFLG)
        OR      A
        JR      Z,NOKEY

        LD      BC,HSHAKE       ;
        IN      A,(C)           ;READ HANDSHAKE
        BIT     3,A             ;IS I/P STROBE THERE?
        JR      Z,NOKEY
        LD      A,0x0FF          ;A=FF CHR READY
        RET
;**************************************************************
NOKEY:  XOR     A               ;A=00 NO CHR
        RET
;**************************************************************
GETKEY: LD      BC,INPORT       ;
        IN      A,(C)           ;
        CP      3
        JR      Z,CNTC
        PUSH    AF
        CALL    VIDEO           ;SEND TO PC
        POP     AF
        RET
;**************************************************************
CNTC:   PUSH    AF                      ;03 TO ^C KEYBOARD CONVERSION
        LD      A,'^
        CALL    CHROUT                  ;SEND TO PC
        LD      A,'C
        CALL    CHROUT                  ;SEND TO PC
        POP     AF
        RET
;**************************************************************
SENDFN: LD      HL,FCB+1
        LD      B,11
OPENLP: LD      A,(HL)
        INC     HL
        PUSH    BC
        CALL    OUTPUT                  ;SEND FILENAME.BGI (8+3 CHRS)
        POP     BC
        DJNZ    OPENLP
        RET
;**************************************************************
OPENFL:
        LD      A,(DISKFL)      ;TEST DISK ENABLED
        OR      A
        JP      Z,QSORRY
        LD      A,ESC           ;SEND ESCAPE
        CALL    OUTPUT          ;TO PC
        LD      A,OPEN          ;SEND CODE
        CALL    OUTPUT          ;TO PC
        CALL    SENDFN

TRYKEY: CALL    TSTKEY          ;WAIT FOR INPUT BUFFER FULL
        OR      A
        JR      Z,TRYKEY
        LD      BC,INPORT       ;
        IN      A,(C)           ;GET ERROR CODE RESPONCE
        RET
;**************************************************************
READ:   LD      HL,TXTBGN       ;SET BASIC POINTER
        LD      DE,3584         ;NUMBER OF BYTES
READLP: PUSH    DE              ;SAVE BYTE COUNT
TRYKY:  CALL    TSTKEY          ;TEST FOR BYTE
        OR      A
        JR      Z,TRYKY         ;WAIT FOR IT
        LD      BC,INPORT       ;
        IN      A,(C)           ;GET BYTE
        LD      (HL),A          ;SAVE TO TEXT BEGIN
        INC     HL              ;BUMP MEMORY POINTER
        POP     DE              ;GET COUNT
        DEC     DE              ;DECREMENT IT
        LD      A,D             ;TEST COUNT
        OR      E
        JR      NZ,READLP       ;TEST FOR 3584 BYTES
        RET
;**************************************************************
WRITE:  LD      A,1
        LD      (LAST),A

        LD      HL,TXTBGN       ;SET BASIC POINTER
        LD      DE,3584         ;NUMBER OF BYTES
WRTLP:  PUSH    DE              ;SAVE BYTE COUNT
        LD      A,(HL)          ;GET TEXT BEGIN

        OR      A
        JR      Z,NULL
WRLOOP: LD      (LAST),A

        CALL    OUTPUT          ;SEND TO PC

        INC     HL              ;BUMP MEMORY POINTER
        POP     DE              ;GET COUNT
        DEC     DE              ;DECREMENT IT
        LD      A,D             ;TEST COUNT
        OR      E
        JR      NZ,WRTLP        ;TEST FOR 3584 BYTES
        RET
;**************************************************************
NULL:   LD      A,(LAST)
        OR      A
        JR      Z,FINAL
        XOR     A
        JR      WRLOOP
FINAL:  CALL    OUTPUT
        POP     DE              ;DUMMY POP
        RET
;**************************************************************
DEBUG:  PUSH    AF
        LD      A,(FRONT)      ;NZ = FRONT PANEL
        OR      A
        JR      Z,NOFRONT

        POP     AF
        PUSH    AF

        PUSH    BC
        PUSH    DE
        PUSH    HL

        CALL    DISNUM

        POP     HL
        POP     DE
        POP     BC
NOFRONT:
        POP     AF
        RET
;**************************************************************
;DISPLAY CONTENTS OF A REG IN FPANEL LEDS
;BOTH DECIMAL POINTS ON
DISNUM: PUSH    AF              ;SAVE NUMBER
        AND     0x0F0            ;MASK BOTT NIBBLE
        RRC     A               ;RIGHT JUSTIFY 
        RRC     A               ;
        RRC     A               ;
        RRC     A               ;
        LD      C,A             ;SAVE MSB NUMBER
        POP     AF              ;GET NUMBER
        AND     0x0F             ;MASK TOP NIBBLE
;;;        SET     7,A             ;TURN ON DEC 1
        LD      B,A             ;STORE FOR LED ROUTINE
        LD      A,C             ;GET MSB FOR LEDS 
;;;        SET     7,A             ;TURN ON DEC 2
        CALL    SETLED          ;WRITE TO LEDS
        RET
;**************************************************************
;MSB CHR IN A, LSB IN B         ;1-NOV-87
SETLED: LD      HL,LED1
        LD      (HL),A
        INC     HL
        LD      (HL),B
        CALL    LEDSON
        RET
;**************************************************************
;SERIAL SEVEN SEGMENT LED DRIVER   2-OCT-87
; 
;LOAD MSB CHR TO BE DISPLAYED INTO (LED1)
;LOAD LSB CHR TO BE DISPLAYED INTO (LED2)
;D7 SET IF DECIMAL POINT TO BE DISPLAYED
LEDSON: LD      HL,LED1         ;SET UP FOR MSB
        CALL    SEG7            ;CONVERT CHR TO 7 SEG
        RLCA                    ;SHIFT LEFT TO BIT 1
        RLCA    
        LD      D,A             ;SAVE IN D
        LD      HL,LED2         ;SET UP FOR LSB
        CALL    SEG7            ;CONVERT CHR TO 7 SEG
LEDP:   RLCA
        LD      C,A             ;SAVE LSB IN BIT 0
        LD      B,8             ;SET TO 8 BITS
IN2LED: LD      A,D             ;GET MSB
        AND     02              ;LOOK AT BIT 1 ONLY
        LD      E,A             ;SAVE IN E
        LD      A,C             ;GET LSB
        AND     01              ;LOOK AT BIT 0 ONLY
        OR      E               ;GATHER BITS
        OUT     (0x60),A         ;WR TO LEDS
        RLC     D               ;SHIFT LEFT MSB
        RLC     C               ;SHIFT LEFT LSB
        DJNZ    IN2LED          ;DO IT 8 TIMES
        RET             
;**************************************************************
SEG7:   LD      E,0             ;DEFAULT NO DEC
        LD      A,(HL)          ;GET CHARACTER
        BIT     7,A             ;TEST DEC OFF/ON
        CALL    NZ,SETDEC       ;TESTED AS ON
        AND     0x7F             ;STRIP DEC PNT
        LD      B,0             ;CLR FOR MATHS
        LD      C,A             ;GET OFFSET
        LD      HL,LEDTAB       ;POINT TO SEG LAYOUT
        ADD     HL,BC           ;POINT INTO TABLE
        LD      A,(HL)          ;GET LAYOUT
        OR      E               ;MIX WITH DEC PNT OFF/ON
        RET     
;**************************************************************
SETDEC: LD      E,0x80          ;SET DEC ON     
        RET
;**************************************************************
;                                  OFFSET IN HEX        
;
LEDTAB: .BYTE    0x3F            ;0      0
        .BYTE    0x06            ;1      1
        .BYTE    0x5B            ;2      2
        .BYTE    0x4F            ;3      3
        .BYTE    0x66            ;4      4
        .BYTE    0x6D            ;5      5
        .BYTE    0x7D            ;6      6
        .BYTE    0x07            ;7      7
        .BYTE    0x7F            ;8      8
        .BYTE    0x67            ;9      9
        .BYTE    0x77            ;A      A
        .BYTE    0x7C             ;B      B
        .BYTE    0x39             ;C      C
        .BYTE    0x5E             ;D      D
        .BYTE    0x79             ;E      E
        .BYTE    0x71             ;F      F
        .BYTE    0x76             ;H      10
        .BYTE    0x38             ;L      11
        .BYTE    0x73             ;P      12
        .BYTE    0x3E             ;U      13
        .BYTE    0x09             ;=      14
        .BYTE    0x40             ;-      15
        .BYTE    0x00             ;BLANK  16
        .BYTE    0x70             ;T      17H
;**************************************************************
SHOWHL:
        PUSH    AF
        PUSH    HL
        LD      A,H
        CALL    HEXOUT
        LD      A,L
        CALL    HEXOUT
        LD      A,0x20
        CALL    CHROUT
        POP     HL
        POP     AF
        RET
;**************************************************************
;O/P BYTE IN A REG IN HEX FORMAT
;MSB,LSB
HEXOUT: PUSH    HL
        CALL    HEXC
        LD      L,A
        LD      A,H             ;
        CALL    CHROUT          ;SEND TO PC
        LD      A,L             ;
        CALL    CHROUT          ;SEND TO PC
        POP     HL
        RET
;**************************************************************
;VALUE IN A CONVERTED TO 2 ASCII HEX DIGITS
;IN A, OUT H=MSB, A=LSB   EG:-  00-->30  0F-->46
HEXC:   LD      L,A
        RRCA
        RRCA
        RRCA
        RRCA
        CALL    HEXC00
        LD      H,A
        LD      A,L
HEXC00: AND     0x0F
        ADD     A,0x90
        DAA
        ADC     A,0x40
        DAA
        RET
;**************************************************************
START:  IM	1
		LD      SP,STACK        ; SET STACK POINTER
		CALL	INIT16550
;        CALL    INIT_8255
        LD      A,0x0FF
        LD      BC,PITBASE+1
        OUT     (C),A           ;SET BITS HIGH
        LD      BC,0x70          ;5-OCT-87
		IN	A,(C)		;TST 4 FRONT PANEL 		
		CP	0x7F		;DEFAULT VALUE IF PRESENT
		JR	NZ,NOFP
		LD	(FRONT),A	;NZ = FRONT PANEL
VERSNU: LD      HL,VERS         ;2-NOV-87
        LD      A,(HL)          ;GET VERSION
        SET     7,A             ;SET DECIMAL POINT
        SUB     0x30             ;ASCII TO BASE
        LD      C,A
        INC     HL              ;BUMP TO LSB
        INC     HL
        LD      A,(HL)          ;GET LSB VERSION
        SUB     0x30
        LD      B,A
        LD      A,C
        CALL    SETLED          ;DISPLAY IT
NOFP:
        LD      A,(FLMOVE)
        CP      0X0FF
        CALL    Z,DMOVE
NOMOVE:
        LD      HL,TXTBGN               ;TXTBGN
        LD      (TXTUNF),HL             ;TXTUNF
        LD      HL,START
        LD      (RANPNT),HL
        CALL    ONE_ON          ;TURN ON LINE ONE SWITCHES
        CALL    AUTO            ;TEST FOR LINE 1
        XOR     A               ;CLEAR ACCUMULATOR
        LD      DE,FANFARE
        CALL    PRTSTR
        JP      RSTART          ;READY PROMPT
;**************************************************************
ONE_ON: LD      A,1
        LD      (VIDEOF),A      ;TURN ON VIDEO SWITCH
        LD      (KBDFLG),A      ;TURN ON KEYBOARD SWITCH
        LD      (DISKFL),A      ;TURN ON DISK I/O SWITCH
        XOR     A
        LD      (LPFLAG),A      ;PRINTER OFF, VIDEO ON
        RET
;**************************************************************
;SETFLAGS:
;        LD      A,(DISKFL)      ;LINE 1 DISK FLAG
;        LD      B,A             ;SAVE TO B
;        LD      A,(FLDISK)      ;ROM DISK FLAG
;        AND     B               ;TOTAL THEM
;        LD      (DISKGT),A      ;AND STORE
;        LD      A,(KBDFLG)      ;LINE 1 KEYBOARD FLAG
;        LD      B,A             ;SAVE TO B
;        LD      A,(FLKEYB)      ;ROM KEYBOARD FLAG
;        AND     B               ;TOTAL THEM
;        LD      (KBDFGT),A      ;AND STORE
;        LD      A,(VIDEOF)      ;LINE 1 VIDEO FLAG
;        LD      B,A             ;SAVE TO B
;        LD      A,(FLVIDE)      ;ROM VIDEO FLAG
;        AND     B               ;TOTAL THEM
;        LD      (VIDEGT),A      ;AND STORE
;        RET
;**************************************************************
;THIS CODE DELETED FOR ZLOAD VERSION
;IT IS A MEANS OF ADJUSTING MEMORY TO MAX USE FOR CPM
;
;        LD      A,(7)           ;GET FBASE FOR TOP
;        LD      (RSTART+2),A    ;MSB STACK
;        DEC     A               ;DECREMENT FOR OTHER POINTERS AND FIX THEM TOO
;        LD      (SS1A+2),A      ;MSB VARBGN 
;        LD      (TV1A+2),A      ;MSB VARBGN
;        LD      (ST3A+2),A      ;MSB BUFFER
;        LD      (ST4A+2),A      ;MSB TXTEND
;        LD      (IP3A+2),A      ;MSB BUFFER
;        LD      (SIZEA+2),A     ;MSB VARBGN
;        LD      (GETLN+5),A     ;MSB BUFFER
;        LD      (PUSHA+2),A     ;MSB STKLMT
;**************************************************************
AUTO:   LD      HL,0
	LD	(CURRNT),HL	; CURRNT->LINE # = 0
	LD	(LOPVAR),HL
	LD	(STKGOS),HL
        LD      HL,TXTBGN       ;AUTO RUN CODE ON LINE 1 ENTRY
        LD      A,(HL)          ;TEST FOR LINE 1
        CP      1               ;LINE #1
        RET     NZ              ;NO-AUTO
        INC     HL
        LD      A,(HL)
        CP      0               ;MSB LINE #
        RET     NZ              ;NO-AUTO
        CALL    SETEND          ;CALC EOF AND SETS POINTER
        POP     AF              ;DUMMY POP RET

        LD      DE,TXTBGN+2     ;IS LINE 1. TST FOR 'REM
        LD      A,(DE)
        CP      0X27             ;IS '?
        JR      NZ,RUNIT
        INC     DE

        LD      B,3             ;3 TIMES
        LD      HL,DISKFL       ;SET START OF FLAGS
FLGLOP: LD      A,(DE)          ;TST FIRST CHR
        CP      CR              ;LINE FINISHED?
        JR      Z,RUNIT
        SUB     0x30             ;CONV BASE BINARY
        LD      (HL),A          ;LOAD FLAGS...
        INC     HL              ;FROM 'REM ON LINE 1
        INC     DE              ;Disk, Keyboard, then Video
        DJNZ    FLGLOP
RUNIT:
;;;
;;;        CALL    SETFLAGS
        JP      RUNNER          ;RUNS LINE 1

;TSTVAR: RST     8*5             ;CALL IGNBLK FIRST; *** TSTV OR RST 7 ***
;        SUB     40H             ; TEST VARIABLES
;        RET     C               ; C:NOT A VARIABLE
;        JP      TSTV1

TSTV1:  JP      NZ,TV1          ; NOT "@" ARRAY
	INC	DE		; IT IS THE "@" ARRAY 
	CALL	PARN		; @ SHOULD BE FOLLOWED
	ADD	HL,HL		; BY (EXPR) AS ITS INDEX
	JP	C,QHOW		; IS INDEX TOO BIG? 
	PUSH	DE		; WILL IT OVERWRITE 
	EX	DE,HL		; TEXT? 
	CALL	SIZE		; FIND SIZE OF FREE 
        RST     0x18		;8*4             ;COMPAR          ; AND CHECK THAT
        JP      C,ASORRY        ; IF SO, SAY "SORRY"
SS1A:   LD      HL,VARBGN       ; IF NOT, GET ADDRESS
	CALL	SUBDE		; OF @(EXPR) AND PUT IT 
	POP	DE		; IN HL 
	RET			; C FLAG IS CLEARED 
;**************************************************************
TV1:    CP      27              ; NOT @, IS IT A TO Z?
        CCF                     ; IF NOT RETURN C FLAG
	RET	C
        INC     DE              ; IF A THROUGH Z
TV1A:   LD      HL,VARBGN       ; COMPUTE ADDRESS OF
	RLCA			; THAT VARIABLE 
	ADD	A,L		; AND RETURN IT IN HL 
	LD	L,A		; WITH C FLAG CLEARED 
	LD	A,0
	ADC	A,H
	LD	H,A
	RET
;**************************************************************
;TSTCHR: EX      (SP),HL         ;*** TSTC OR RST 1 ***
;        RST     8*5             ;IGNORE BLANKS AND
;        CP      (HL)            ; TEST CHARACTER
;        JP     TC1

TC1:    INC     HL              ; COMPARE THE BYTE THAT
	JP	Z,TC2		; FOLLOWS THE RST INST. 
	PUSH	BC		; WITH THE TEXT (DE->)
        LD      C,(HL)          ; IF NOT =, ADD THE 2ND
	LD	B,0		; BYTE THAT FOLLOWS THE 
	ADD	HL,BC		; RST TO THE OLD PC 
	POP	BC		; I.E., DO A RELATIVE 
        DEC     DE              ; JUMP IF NOT =
TC2:    INC     DE              ; IF =, SKIP THOSE BYTES
	INC	HL		; AND CONTINUE
	EX	(SP),HL
	RET
;**************************************************************
TSTNUM:	LD		HL,0		; *** TSTNUM ***
        LD      B,H			; TEST IF THE TEXT IS
        RST     0x28		; IGNBLK; A NUMBER
TN1:    CP      0x30		; IF NOT, RETURN 0 IN
		RET		C			; B AND HL
        CP      0x3A		;72Q ; IF NUMBERS, CONVERT
		RET		NC			; TO BINARY IN HL AND 
        LD      A,0x0F0		;360Q ; SET A TO # OF DIGITS
        AND     H			; IF H>255, THERE IS NO
		JP		NZ,QHOW		; ROOM FOR NEXT DIGIT 
		INC		B			; B COUNTS # OF DIGITS
		PUSH	BC
		LD		B,H			; HL=10;*HL+(NEW DIGIT)
		LD		C,L
	ADD	HL,HL		; WHERE 10;* IS DONE BY
	ADD	HL,HL		; SHIFT AND ADD 
	ADD	HL,BC
	ADD	HL,HL
	LD	A,(DE)		; AND (DIGIT) IS FROM 
	INC	DE		; STRIPPING THE ASCII 
        AND     0X0F             ; CODE
	ADD	A,L
	LD	L,A
	LD	A,0
	ADC	A,H
	LD	H,A
	POP	BC
	LD	A,(DE)		; DO THIS DIGIT AFTER 
	JP	P,TN1		; DIGIT. S SAYS OVERFLOW
QHOW:	PUSH	DE		; *** ERROR: "HOW?" *** 
AHOW:	LD	DE,HOW
	JP	ERROR
HOW:    .ASCII    "How?"
		.BYTE	CR
READY:  .ASCII    "PBUFF Ready when "
		.ASCII	 "you are Curly..."
		.BYTE	CR,LF
WHAT:   .ASCII    "What?"
		.BYTE	CR
SORRY:  .ASCII    "Sorry"
		.BYTE	CR
;**************************************************************
;* *** MAIN ***
;* 
;* THIS IS THE MAIN LOOP THAT COLLECTS THE TINY BASIC PROGRAM
;* AND STORES IT IN THE MEMORY.
;* 
;* AT START, IT PRINTS "(CR)READY(CR)", AND INITIALIZES THE
;* STACK AND SOME OTHER INTERNAL VARIABLES.  THEN IT PROMPTS 
;* ">" AND READS A LINE. IF THE LINE STARTS WITH A NON-ZERO
;* NUMBER, THIS NUMBER IS THE LINE NUMBER. THE LINE NUMBER
;* (IN 16 BIT BINARY) AND THE REST OF THE LINE (INCLUDING CR)
;* IS STORED IN THE MEMORY. IF A LINE WITH THE SAME LINE
;* NUMBER IS ALREADY THERE, IT IS REPLACED BY THE NEW ONE. IF
;* THE REST OF THE LINE CONSISTS OF A 0DHONLY, IT IS NOT STORED
;* AND ANY EXISTING LINE WITH THE SAME LINE NUMBER IS DELETED. 
;* 
;* AFTER A LINE IS INSERTED, REPLACED, OR DELETED, THE PROGRAM
;* LOOPS BACK AND ASK FOR ANOTHER LINE.  THIS LOOP WILL BE 
;* TERMINATED WHEN IT READS A LINE WITH ZERO OR NO LINE
;* NUMBER; AND CONTROL IS TRANSFERED TO "DIRCT".
;* 
;* TINY BASIC PROGRAM SAVE AREA STARTS AT THE MEMORY LOCATION
;* LABELED "TXTBGN" AND ENDED AT "TXTEND".  WE ALWAYS FILL THIS
;* AREA STARTING AT "TXTBGN", THE UNFILLED PORTION IS POINTED
;* BY THE CONTENT OF A MEMORY LOCATION LABELED "TXTUNF". 
;* 
;* THE MEMORY LOCATION "CURRNT" POINTS TO THE LINE NUMBER
;* THAT IS CURRENTLY BEING INTERPRETED.  WHILE WE ARE IN 
;* THIS LOOP OR WHILE WE ARE INTERPRETING A DIRECT COMMAND 
;* (SEE NEXT SECTION), "CURRNT" SHOULD POINT TO A 0. 
;* 
RSTART: LD      SP,STACK        ;SET STACK, READY PROMPT
        CALL    CRLF            ; AND JUMP TO HERE

;;;        LD      A,(VARBGN)
;;;        CALL    DISNUM

	LD      DE,READY        ; DE->STRING

	SUB     A               ; A=0
	CALL	PRTSTG		; PRINT STRING UNTIL 0DH
	CALL	CRLF

	LD      HL,ST2+1        ; LITERAL 0
	LD	(CURRNT),HL	; CURRNT->LINE # = 0
ST2:	LD	HL,0
	LD	(LOPVAR),HL
	LD	(STKGOS),HL
ST3:    LD      A,'>           ; PROMPT '>' AND
        CALL    GETLN           ; READ A LINE
        
;        JP	ST3		; TODO: temporary loop back
        
        PUSH    DE              ; DE->END OF LINE
ST3A:   LD      DE,BUFFER       ; DE->BEGINNING OF LINE
        CALL    TSTNUM          ; TEST IF IT IS A NUMBER
        RST     0x28;	8*5             ;IGNBLK
        LD      A,H             ; HL=VALUE OF THE # OR
        OR      L               ; 0 IF NO # WAS FOUND
	POP	BC		; BC->END OF LINE 
	JP	Z,DIRECT
	DEC	DE		; BACKUP DE AND SAVE
	LD	A,H		; VALUE OF LINE # THERE 
	LD	(DE),A
	DEC	DE
	LD	A,L
	LD	(DE),A
	PUSH	BC		; BC,DE->BEGIN, END 
	PUSH	DE
	LD	A,C
	SUB	E
	PUSH	AF		; A=# OF BYTES IN LINE
	CALL	FNDLN		; FIND THIS LINE IN SAVE
	PUSH	DE		; AREA, DE->SAVE AREA 
	JP	NZ,ST4		; NZ:NOT FOUND, INSERT
	PUSH	DE		; Z:FOUND, DELETE IT
	CALL	FNDNXT		; FIND NEXT LINE
;*                                       DE->NEXT LINE 
	POP	BC		; BC->LINE TO BE DELETED
	LD	HL,(TXTUNF)	; HL->UNFILLED SAVE AREA
	CALL	MVUP		; MOVE UP TO DELETE 
	LD	H,B		; TXTUNF->UNFILLED AREA 
	LD	L,C
	LD	(TXTUNF),HL	; UPDATE

ST4:    POP     BC              ; GET READY TO INSERT
        LD      HL,(TXTUNF)     ; BUT FIRST CHECK IF

        CALL    TERM

        POP     AF              ; THE LENGTH OF NEW LINE
	PUSH	HL		; IS 3 (LINE # AND CR)
	CP	3		; THEN DO NOT INSERT
	JP	Z,RSTART	; MUST CLEAR THE STACK
	ADD	A,L		; COMPUTE NEW TXTUNF
	LD	L,A
	LD	A,0
	ADC	A,H
	LD	H,A		; HL->NEW UNFILLED AREA 
ST4A:   LD      DE,TXTEND       ; CHECK TO SEE IF THERE
        RST     0x20;	8*4             ;COMPAR          ; IS ENOUGH SPACE
	JP	NC,QSORRY	; SORRY, NO ROOM FOR IT 
	LD	(TXTUNF),HL	; OK, UPDATE TXTUNF 
	POP	DE		; DE->OLD UNFILLED AREA 
        CALL    MVDOWN

        CALL    TERM

        POP     DE              ; DE->BEGIN, HL->END
	POP	HL
        CALL    MVUP            ; MOVE NEW LINE TO SAVE
        JP      ST3             ; AREA

TERM:   PUSH    HL
        LD      HL,(TXTUNF)
        LD      (HL),0          ;DELETE CHRS BUG
        INC     HL              ;MUST TERMINATE WITH...
        LD      (HL),0          ;WITH 2 ZEROS (NULL LINE)
        POP     HL
        RET
;**************************************************************
;* *** TABLES *** DIRECT *** & EXEC ***
;* 
;* THIS SECTION OF THE CODE TESTS A STRING AGAINST A TABLE.
;* WHEN A MATCH IS FOUND, CONTROL IS TRANSFERED TO THE SECTION 
;* OF CODE ACCORDING TO THE TABLE. 
;* 
;* AT 'EXEC', DE SHOULD POINT TO THE STRING AD HL SHOULD POINT
;* TO THE TABLE-1.  AT 'DIRECT', DE SHOULD POINT TO THE STRING,
;* HL WILL BE SET UP TO POINT TO TAB1-1, WHICH IS THE TABLE OF 
;* ALL DIRECT AND STATEMENT COMMANDS.
;* 
;* A '.' IN THE STRING WILL TERMINATE THE TEST AND THE PARTIAL 
;* MATCH WILL BE CONSIDERED AS A MATCH.  E.G., 'P.', 'PR.',
;* 'PRI.', 'PRIN.', OR 'PRINT' WILL ALL MATCH 'PRINT'. 
;* 
;* THE TABLE CONSISTS OF ANY NUMBER OF ITEMS.  EACH ITEM 
;* IS A STRING OF CHARACTERS WITH BIT 7 SET TO 0 AND 
;* A JUMP ADDRESS STORED HI-LOW WITH BIT 7 OF THE HIGH 
;* BYTE SET TO 1.
;* 
;* END OF TABLE IS AN ITEM WITH A JUMP ADDRESS ONLY.  IF THE
;* STRING DOES NOT MATCH ANY OF THE OTHER ITEMS, IT WILL 
;* MATCH THIS NULL ITEM AS DEFAULT.
;* 
TAB1    =     .                ; DIRECT COMMANDS
        .ascii    "LIST\0"
;        .BYTE    LIST + SEV_UP,LIST
		.WORD	LIST
		.ascii	"RUN\0"
;        .BYTE    RUN >> 8 + SEV_UP,RUN & 255
		.WORD	RUN
		.ascii	"NEW\0"
;        .BYTE    NEW >> 8 + SEV_UP,NEW & 255
		.WORD	NEW 
		.ascii	"LOAD\0"
;        .BYTE    DLOAD >> 8 + SEV_UP,DLOAD & 255
		.WORD	DLOAD 
		.ascii	"SAVE\0"
 ;       .BYTE    DSAVE >> 8 + SEV_UP,DSAVE & 255
		.WORD	DSAVE 
;        .BYTE    "BYE"
;        .BYTE    80H,0H    ; GO BACK TO CPM

TAB2    =     .               ; DIRECT/STATEMENT
        .ascii    "OUT\0"
;        .BYTE    OUTCMD >> 8 + SEV_UP,OUTCMD & 255
		.WORD	OUTCMD 
        .ascii    "WAIT\0"
;        .BYTE    WAITCM >> 8 + SEV_UP,WAITCM & 255
		.WORD	WAITCM 
        .ascii    "POKE\0"
;        .BYTE    POKE >> 8 + SEV_UP,POKE & 255
		.WORD	POKE 
        .ascii    "NEXT\0"
;        .BYTE    NEXT >> 8 + SEV_UP,NEXT & 255
		.WORD 	NEXT 
		.ascii	"LET\0"
;        .BYTE    LET >> 8 + SEV_UP,LET & 255
		.WORD	LET
		.ascii	"IF\0"
;        .BYTE    IFF >> 8 + SEV_UP,IFF & 255
		.WORD	IFF 
		.ascii	"GOTO\0"
;        .BYTE    GOTO >> 8 + SEV_UP,GOTO & 255
		.WORD	GOTO 
		.ascii	"GOSUB\0"
;        .BYTE    GOSUB >> 8 + SEV_UP,GOSUB & 255
		.WORD	GOSUB 
		.ascii	"RETURN\0"
;        .BYTE    RETURN >> 8 + SEV_UP,RETURN & 255
		.WORD	RETURN 
		.ASCII	"'\0"
;        .BYTE    0x27
;        .BYTE    REM >> 8 + SEV_UP,REM & 255
		.WORD	REM 
        .ascii    "REM\0"
;        .BYTE    REM >> 8 + SEV_UP,REM & 255
		.WORD	REM 
        .ascii    "FOR\0"
;        .BYTE    FOR >> 8 + SEV_UP,FOR & 255
		.WORD	FOR
		.ascii	"INPUT\0"
;        .BYTE    INPUT >> 8 + SEV_UP,INPUT & 255
		.WORD	INPUT 
        .ascii    "?\0"
;        .BYTE    PRINT >> 8 + SEV_UP,PRINT & 255
		.WORD	PRINT 
        .ascii    "PRINT\0"
;        .BYTE    PRINT >> 8 + SEV_UP,PRINT & 255
		.WORD	PRINT 
        .ascii    "STOP\0"
;        .BYTE    STOP >> 8 + SEV_UP,STOP & 255
		.WORD	STOP 
        .ascii    "START\0"
;        .BYTE    START >> 8 + SEV_UP,START & 255
		.WORD	START 
        .ascii    "OLD\0"
;        .BYTE    OLD >> 8 + SEV_UP,OLD & 255
		.WORD	OLD 
        .ascii    "MOVE\0"
;        .BYTE    MOVE >> 8 + SEV_UP,MOVE & 255
		.WORD	MOVE 
;        .BYTE    DEFLT >> 8 + SEV_UP,DEFLT & 255
		.WORD	DEFLT 
;        .BYTE    "YOU CAN ADD MORE" ; COMMANDS BUT
				; REMEMBER TO MOVE DEFAULT DOWN.

TAB4    =     .               ; FUNCTIONS 
        .ascii    "INP\0"
;        .BYTE    INP >> 8 + SEV_UP,INP & 255
		.WORD	INP 
        .ascii    "PEEK\0"
;        .BYTE    PEEK >> 8 + SEV_UP,PEEK & 255
		.WORD	PEEK 
        .ascii    "USR\0"
;        .BYTE    USR >> 8 + SEV_UP,USR & 255
		.WORD	USR 
        .ascii    "RND\0"
;        .BYTE    RND >> 8 + SEV_UP,RND & 255
		.WORD	RND 
		.ascii	"ABS\0"
;        .BYTE    ABS >> 8 + SEV_UP,ABS & 255
		.WORD	ABS 
        .ascii    "MEM\0"
;        .BYTE    SIZE >> 8 + SEV_UP,SIZE & 255
		.WORD	SIZE 

;        .BYTE    XP40 >> 8 + SEV_UP,XP40 & 255
		.WORD	XP40 
;        .BYTE    'YOU CAN ADD MORE' ; FUNCTIONS BUT REMEMBER
				; TO MOVE XP40 DOWN

TAB5    =     .               ; "TO" IN "FOR" 
		.ascii	"TO\0"
;        .BYTE    FR1 >> 8 + SEV_UP,FR1 & 255
		.WORD	FR1 
;        .BYTE    QWHAT >> 8 + SEV_UP,QWHAT & 255
		.WORD	QWHAT 
TAB6	=	.		; "STEP" IN "FOR" 
		.ascii	"STEP\0"
;        .BYTE    FR2 >> 8 + SEV_UP,FR2 & 255
		.WORD	FR2 
;        .BYTE    FR3 >> 8 + SEV_UP,FR3 & 255
		.WORD	FR3 

TAB8    =     .               ; RELATION OPERATORS

        .ascii    ">=\0"
;        .BYTE    GTE >> 8 + SEV_UP,GTE & 255
		.WORD	GTE 
        .ascii    "<>\0"
;        .BYTE    NOTEQU >> 8 + SEV_UP,NOTEQU & 255
		.WORD	NOTEQU 
		.ascii	">\0"
;        .BYTE    GREATER >> 8 + SEV_UP,GREATER & 255
		.WORD	GREATER 
		.ascii	"=\0"
;        .BYTE    EQUAL >> 8 + SEV_UP,EQUAL & 255
		.WORD	EQUAL 
		.ascii	"<=\0"
;        .BYTE    LESSEQ >> 8 + SEV_UP,LESSEQ & 255
		.WORD	LESSEQ 
		.ascii	"<\0"
;        .BYTE    LESS >> 8 + SEV_UP,LESS & 255
		.WORD	LESS 
;        .BYTE    (NOTREL >> 8) + SEV_UP,NOTREL & 255
        .WORD    NOTREL 
;* 
DIRECT:	LD		HL,TAB1-1	; *** DIRECT ***
;* 
EXEC	=	.		; *** EXEC ***
EX0:    RST     0x28	;8*5             ; IGNORE LEADING BLANKS
		PUSH	DE		; SAVE POINTER
EX1:	LD      A,(DE)	; IF FOUND '.' IN STRING
		INC		DE		; BEFORE ANY MISMATCH 
        CP      '. 		; WE DECLARE A MATCH
		JP		Z,EX3
		INC		HL		; HL->TABLE 
        CP      (HL)	; IF MATCH, TEST NEXT
		JP		Z,EX1
        LD      A,0x00	; ELSE, SEE IF BIT 7
		DEC		DE		; OF TABLEIS SET, WHICH
		CP		(HL)	; IS THE JUMP ADDR. (HI)
		JP		Z,EX5	; C:YES, MATCHED
EX2:	INC		HL		; NC:NO, FIND JUMP ADDR.
		CP		(HL)
		JP		NZ,EX2
		INC		HL		; BUMP TO NEXT TAB. ITEM
		INC		HL
		POP		DE		; RESTORE STRING POINTER
		JP		EX0		; TEST AGAINST NEXT ITEM
EX3:    LD      A,0x00	; PARTIAL MATCH, FIND
EX4:	INC		HL		; JUMP ADDR., WHICH IS
		CP		(HL)	; FLAGGED BY BIT 7
		JP		NZ,EX4
EX5:	
		INC		HL
		LD		A,(HL)	; LOAD HL WITH THE JUMP 
		INC		HL		; ADDRESS FROM THE TABLE
		LD		H,(HL)
;		AND     MASK_7;7FH             ; MASK OFF BIT 7
		LD      L,A
		POP		AF		; CLEAN UP THE GABAGE 
		JP		(HL)	; AND WE GO DO IT 
;**************************************************************
;* WHAT FOLLOWS IS THE CODE TO EXECUTE DIRECT AND STATEMENT
;* COMMANDS.  CONTROL IS TRANSFERED TO THESE POINTS VIA THE
;* COMMAND TABLE LOOKUP CODE OF 'DIRECT' AND 'EXEC' IN LAST
;* SECTION.  AFTER THE COMMAND IS EXECUTED, CONTROL IS 
;* TANSFERED TO OTHER SECTIONS AS FOLLOWS:
;* 
;* FOR 'LIST', 'NEW', AND 'STOP': GO BACK TO 'RSTART'
;* FOR 'RUN': GO EXECUTE THE FIRST STORED LINE IF ANY; ELSE
;* GO BACK TO 'RSTART'.
;* FOR 'GOTO' AND 'GOSUB': GO EXECUTE THE TARGET LINE. 
;* FOR 'RETURN' AND 'NEXT': GO BACK TO SAVED RETURN LINE.
;* FOR ALL OTHERS: IF 'CURRNT' -> 0, GO TO 'RSTART', ELSE
;* GO EXECUTE NEXT COMMAND.  (THIS IS DONE IN 'FINISH'.) 
;* 
;**************************************************************
;* 
;* *** NEW *** STOP *** RUN (& FRIENDS) *** & GOTO *** 
;* 
;* 'NEW(CR)' SETS 'TXTUNF' TO POINT TO 'TXTBGN'
;* 
;* 'STOP(CR)' GOES BACK TO 'RSTART'
;* 
;* 'RUN(CR)' FINDS THE FIRST STORED LINE, STORE ITS ADDRESS (IN
;* 'CURRNT'), AND START EXECUTE IT.  NOTE THAT ONLY THOSE
;* COMMANDS IN TAB2 ARE LEGAL FOR STORED PROGRAM.
;* 
;* THERE ARE 3 MORE ENTRIES IN 'RUN':
;* 'RUNNXL' FINDS NEXT LINE, STORES ITS ADDR. AND EXECUTES IT. 
;* 'RUNTSL' STORES THE ADDRESS OF THIS LINE AND EXECUTES IT. 
;* 'RUNSML' CONTINUES THE EXECUTION ON SAME LINE.;* 
;* 'GOTO EXPR(CR)' EVALUATES THE EXPRESSION, FIND THE TARGET 
;* LINE, AND JUMP TO 'RUNTSL' TO DO IT.
;* 'DLOAD' LOADS A NAMED PROGRAM FROM DISK.
;* 'DSAVE' SAVES A NAMED PROGRAM ON DISK.
;* 'FCBSET' SETS UP THE FILE CONTROL BLOCK FOR SUBS.EQUENT DISK I/O.
;* 
NEW:	CALL	ENDCHK		; *** NEW(CR) *** 
	LD	HL,TXTBGN
	LD	(TXTUNF),HL
;* 
STOP:	CALL	ENDCHK		; *** STOP(CR) ***
	JP	RSTART
;**************************************************************
RUN:	CALL	ENDCHK		; *** RUN(CR) *** 
RUNNER: LD      DE,TXTBGN       ; FIRST SAVED LINE
;* 
RUNNXL:
        LD      HL,0            ; *** RUNNXL ***
        CALL    FNDLNP          ; FIND WHATEVER LINE #
	JP	C,RSTART	; C:PASSED TXTUNF, QUIT 
;* 
RUNTSL:
        EX      DE,HL           ; *** RUNTSL ***
	LD	(CURRNT),HL	; SET 'CURRNT'->LINE #
	EX	DE,HL
	INC	DE		; BUMP PASS LINE #
	INC	DE
;* 
RUNSML:	CALL	CHKIO		; *** RUNSML ***
	LD	HL,TAB2-1	; FIND COMMAND IN TAB2
	JP	EXEC		; AND EXECUTE IT
;**************************************************************
GOTO:   RST     0x18	;8*3             ;EXP             ; *** GOTO EXPR ***
	PUSH	DE		; SAVE FOR ERROR ROUTINE
	CALL	ENDCHK		; MUST FIND A 0DH
	CALL	FNDLN		; FIND THE TARGET LINE
	JP	NZ,AHOW		; NO SUCH LINE #
	POP	AF		; CLEAR THE "PUSH DE" 
	JP	RUNTSL		; GO DO IT
;**************************************************************
DLOAD:  RST     0x28	;8*5             ;IGNORE BLANKS
        PUSH    HL              ;SAVE HL
        CALL    FCBSET          ;SET UP FILE CONTROL BLOCK
        PUSH    DE              ;SAVE THE REST
        PUSH    BC              ;
        LD      DE,FCB          ;GET FCB ADDRESS
        CALL    OPENFL          ;OPEN FILE
        CP      0x0FF            ;IS IT THERE?
        JP      Z,QHOW          ;NO, SEND ERROR
LOAD:   CALL    READ            ;GET TBI FILE FROM PC
        CALL    SETEND          ;SET EOF
        POP     BC              ;GET OLD REGISTERS BACK
        POP     DE              ;
        POP     HL              ;
        CALL    AUTO            ;TEST FOR AUTO-RUN
        RST     0x30	;8*6             ;FINISH
;**************************************************************
DSAVE:  LD      A,(DISKFL)
        OR      A
        JP      Z,QSORRY
        CCF                     ;BUG WOULD SAVE ON NEW
        PUSH    HL              ;WHEN NOTHING TO SAVE
        PUSH    DE              ;

        LD      DE,TXTBGN       ;GET START OF BASIC PROGRAM

;        PUSH    DE
;        EX      DE,HL
;        CALL    SHOWHL
;        POP     DE

        LD      HL,(TXTUNF)     ;GET END MARKER

;        PUSH    DE
;        PUSH    HL
;        EX      DE,HL
;        CALL    SHOWHL
;        POP     HL
;        POP     DE

        SBC     HL,DE           ;SUB THE TWO
        INC     HL              ;ADJ TO TXTUNF

        LD      A,H
        OR      L

        POP     DE              ;
        POP     HL              ;
        JR      NZ,SAVEOK       ;
        JP      QHOW            ;HOW ERROR

SAVEOK: RST     0x28	;8*5             ;IGNORE BLANKS
        PUSH    HL              ;SAVE HL
        CALL    FCBSET          ;SETUP FCB
        PUSH    DE              ;
        PUSH    BC              ;SAVE OTHERS
        LD      A,ESC           ;SEND ESCAPE
        CALL    OUTPUT          ;TO PC
        LD      A,DELETE        ;SEND CODE
        CALL    OUTPUT          ;TO PC
        CALL    SENDFN          ;AND FILENAME
        CALL    WRITE           ;SEND TBI FILE TO PC
        POP     BC              ;GET REGISTERS BACK
        POP     DE              ;
        POP     HL              ;
        RST     0x30	;8*6             ;FINISH
;**************************************************************
FCBSET: LD      HL,FCB          ; GET FILE CONTROL BLOCK ADDRESS
	LD	(HL),0		; CLEAR ENTRY TYPE
FNCLR:	INC	HL		; NEXT LOCATION
	LD	(HL),0x20	; CLEAR TO SPACE
	LD	A,FCB+8 & 255
	CP	L		; DONE?
	JP	NZ,FNCLR	; NO, DO IT AGAIN
	INC	HL		; NEXT
	LD	(HL),'T	; SET FILE TYPE TO 'TBI'
	INC	HL
	LD	(HL),'B
	INC	HL
	LD	(HL),'I
EXRC:	INC	HL		; CLEAR REST OF FCB
	LD	(HL),0
	LD	A,FCB+15 & 255
	CP	L		; DONE?
	JP	NZ,EXRC		; NO, CONTINUE
	LD	HL,FCB+1	; GET FILENAME START
FN:	LD	A,(DE)		; GET CHARACTER
        CP      CR              ; IS IT A 'CR'
	RET	Z		; YES, DONE
	CP	'!		; LEGAL CHARACTER?
	JP	C,QWHAT		; NO, SEND ERROR
	CP	'[		; AGAIN
	JP	NC,QWHAT	; DITTO
	LD	(HL),A		; SAVE IT IN FCB
	INC	HL		; NEXT
	INC	DE
	LD	A,FCB+9 & 255
	CP	L		; LAST?
	JP	NZ,FN		; NO, CONTINUE
	RET			; TRUNCATE AT 8 CHARACTERS
;**************************************************************
SETEND: LD      DE,TXTBGN+2     ;TEST AND SET FILE END.
FNDEND: LD      A,(DE)          ;
        CP      CR              ;
        INC     DE              ;
        JR      NZ,FNDEND       ;
        LD      A,(DE)          ;
        CP      0               ;
        JR      NZ,FNDEND       ;
        INC     DE
        LD      A,(DE)
        DEC     DE
        CP      0
        JR      NZ,FNDEND
        LD      (TXTUNF),DE     ;
        RET
;**************************************************************
OLD:    LD      SP,STACK        ;ZAP NEW
        CALL    SETEND
        JP      RSTART
;**************************************************************
MOVE:
        LD      A,(FLMOVE)
        CP      0x0FF
        JR      NZ,NOTMOVE
        CALL    DMOVE
        JR      OLD
;**************************************************************
DMOVE:  LD      HL,ROMBAS
        LD      DE,RAMBAS
        LD      BC,TXTEND-TXTBGN;1000H
        LDIR
        RET
;*************************************************************
NOTMOVE:
        JP      QSORRY
;*************************************************************
;* *** LIST *** & PRINT ***
;* 
;* LIST HAS TWO FORMS: 
;* 'LIST(CR)' LISTS ALL SAVED LINES
;* 'LIST #(CR)' START LIST AT THIS LINE #
;* YOU CAN STOP THE LISTING BY CONTROL C KEY 
;* 
;* PRINT COMMAND IS 'PRINT ....;' OR 'PRINT ....(CR)'
;* WHERE '....' IS A LIST OF EXPRESIONS, FORMATS, BACK-
;* ARROWS, AND STRINGS.  THESE ITEMS ARE SEPERATED BY COMMAS.
;* 
;* A FORMAT IS A POUND SIGN FOLLOWED BY A NUMBER.  IT CONTROLSs 
;* THE NUMBER OF SPACES THE VALUE OF A EXPRESION IS GOING TO 
;* BE PRINTED.  IT STAYS EFFECTIVE FOR THE REST OF THE PRINT 
;* COMMAND UNLESS CHANGED BY ANOTHER FORMAT.  IF NO FORMAT IS
;* SPECIFIED, 6 POSITIONS WILL BE USED.
;* 
;* A STRING IS QUOTED IN A PAIR OF SINGLE QUOTES OR A PAIR OF
;* DOUBLE QUOTES.
;* 
;* A BACK-ARROW MEANS GENERATE A (CR) WITHOUT (LF) 
;* 
;* A (CRLF) IS GENERATED AFTER THE ENTIRE LIST HAS BEEN
;* PRINTED OR IF THE LIST IS A NULL LIST.  HOWEVER IF THE LIST
;* ENDED WITH A COMMA, NO (CRL) IS GENERATED. 
;* 
LIST:   CALL    TSTNUM          ; TEST IF THERE IS A #
        CALL    ENDCHK          ; IF NO # WE GET A 0
	CALL	FNDLN		; FIND THIS OR NEXT LINE
LS1:	JP	C,RSTART	; C:PASSED TXTUNF 
	CALL	PRTLN		; PRINT THE LINE
        CALL    CHKIO           ; STOP IF HIT CONTROL-C
	CALL	FNDLNP		; FIND NEXT LINE
	JP	LS1		; AND LOOP BACK 
;**************************************************************
PRINT:  LD      C,13;8;6             ; C = # OF SPACES
        RST     0x08	;8*1             ;TSTCHR; IF NULL LIST & ";"
        .BYTE    0x36;
        .BYTE    PR2-1-.

        CALL    CRLF            ; GIVE CR-LF AND
	JP	RUNSML		; CONTINUE SAME LINE
;**************************************************************
PR2:    RST     0x08	;8*1             ;TSTCHR; IF NULL LIST (CR)
        .BYTE    CR
        .BYTE    PR0-1-.

        CALL    CRLF            ; ALSO GIVE CR-LF AND
	JP	RUNNXL		; GO TO NEXT LINE 
;**************************************************************
PR0:    RST     0x08	;8*1             ;TSTCHR; ELSE IS IT FORMAT?
	.BYTE	'#
        .BYTE    PR1-1-.

        RST     0x18	;8*3             ;YES, EVALUATE EXPR.
	LD	C,L		; AND SAVE IT IN C
	JP	PR3		; LOOK FOR MORE TO PRINT
;**************************************************************
PR1:    CALL    QTSTG           ; OR IS IT A STRING?
        JP      PR8             ; IF NOT, MUST BE EXPR.
;**************************************************************
PR3:    RST     0x08	;8*1             ;TSTCHR; IF ",", GO FIND NEXT

        .BYTE    ',
;;        .BYTE    ';'          ;PRINT FORMAT


;        .BYTE    6Q
;;        .BYTE    PR6-1-$
        .BYTE    PR3A-1-.
        XOR     A
        LD      (SEMICO),A
        CALL    FIN             ; IN THE LIST.
	JP	PR0		; LIST CONTINUES
;**************************************************************
PR3A:   RST     0x08	;8*1             ;TSTCHR;IF ";", GO FIND NEXT
        .BYTE    ';             ;PRINT FORMAT
        .BYTE    PR6-1-.
        LD      A,1
        LD      (SEMICO),A
        CALL    FIN             ; IN THE LIST.
        JP      PR0             ; LIST CONTINUES
;**************************************************************
PR6:    CALL    CRLF            ; LIST ENDS
        RST     0x30	;8*6             ;FINISH
;**************************************************************
PR8:    RST     0x18	;8*3             ;EVALUATE THE EXPR
	PUSH	BC
        CALL    PRTNUM          ; PRINT THE VALUE
	POP	BC
	JP	PR3		; MORE TO PRINT?
;**************************************************************
;* *** GOSUB *** & RETURN ***
;* 
;* 'GOSUB EXPR;' OR 'GOSUB EXPR (CR)' IS LIKE THE 'GOTO' 
;* COMMAND, EXCEPT THAT THE CURRENT TEXT POINTER, STACK POINTER
;* ETC. ARE SAVE SO THAT EXECUTION CAN BE CONTINUED AFTER THE
;* SUBROUTINE 'RETURN'.  IN ORDER THAT 'GOSUB' CAN BE NESTED 
;* (AND EVEN RECURSIVE), THE SAVE AREA MUST BE STACKED.
;* THE STACK POINTER IS SAVED IN 'STKGOS'. THE OLD 'STKGOS' IS 
;* SAVED IN THE STACK.  IF WE ARE IN THE MAIN ROUTINE, 'STKGOS'
;* IS ZERO (THIS WAS DONE BY THE "MAIN" SECTION OF THE CODE),
;* BUT WE STILL SAVE IT AS A FLAG FORr NO FURTHER 'RETURN'S.
;* 
;* 'RETURN(CR)' UNDOS EVERYHING THAT 'GOSUB' DID, AND THUS
;* RETURN THE EXCUTION TO THE COMMAND AFTER THE MOST RECENT
;* 'GOSUB'.  IF 'STKGOS' IS ZERO, IT INDICATES THAT WE
;* NEVER HAD A 'GOSUB' AND IS THUS AN ERROR. 
;* 
GOSUB:	CALL	PUSHA		; SAVE THE CURRENT "FOR"
        RST     0x18;	8*3             ;EXP             ; PARAMETERS
	PUSH	DE		; AND TEXT POINTER
	CALL	FNDLN		; FIND THE TARGET LINE
	JP	NZ,AHOW		; NOT THERE. SAY "HOW?" 
	LD	HL,(CURRNT)	; FOUND IT, SAVE OLD
	PUSH	HL		; 'CURRNT' OLD 'STKGOS' 
	LD	HL,(STKGOS)
	PUSH	HL
	LD	HL,0		; AND LOAD NEW ONES 
	LD	(LOPVAR),HL
	ADD	HL,SP
	LD	(STKGOS),HL
	JP	RUNTSL		; THEN RUN THAT LINE
;**************************************************************
RETURN: CALL    ENDCHK          ; THERE MUST BE A 0DH
	LD	HL,(STKGOS)	; OLD STACK POINTER 
	LD	A,H		; 0 MEANS NOT EXIST 
	OR	L
	JP	Z,QWHAT		; SO, WE SAY: "WHAT?" 
	LD	SP,HL		; ELSE, RESTORE IT
	POP	HL
	LD	(STKGOS),HL	; AND THE OLD 'STKGOS'
	POP	HL
	LD	(CURRNT),HL	; AND THE OLD 'CURRNT'
	POP	DE		; OLD TEXT POINTER
	CALL	POPA		; OLD "FOR" PARAMETERS
        RST     0x30	;8*6             ;FINISH; AND WE ARE BACK HOME
;**************************************************************
;* 
;* *** FOR *** & NEXT ***
;* 
;* 'FOR' HAS TWO FORMS:
;* 'FOR VAR=EXP1 TO EXP2 STEP EXP1' AND 'FOR VAR=EXP1 TO EXP2' 
;* THE SECOND FORM MEANS THE SAME THING AS THE FIRST FORM WITH 
;* EXP1=1.  (I.E., WITH A STEP OF +1.) 
;* TBI WILL FIND THE VARIABLE VAR. AND SET ITS VALUE TO THE
;* CURRENT VALUE OF EXP1.  IT ALSO EVALUATES EXPR2 AND EXP1
;* AND SAVE ALL THESE TOGETHER WITH THE TEXT POINTERr ETC. IN 
;* THE 'FOR' SAVE AREA, WHICH CONSISTS OF 'LOPVAR', 'LOPINC',
;* 'LOPLMT', 'LOPLN', AND 'LOPPT'.  IF THERE IS ALREADY SOME-
;* THING IN THE SAVE AREA (THIS IS INDICATED BY A NON-ZERO 
;* 'LOPVAR'), THEN THE OLD SAVE AREA IS SAVED IN THE STACK 
;* BEFORE THE NEW ONE OVERWRITES IT. 
;* TBI WILL THEN DIG IN THE STACK AND FIND OUT IF THIS SAME
;* VARIABLE WAS USED IN ANOTHER CURRENTLY ACTIVE 'FOR' LOOP. 
;* IF THAT IS THE CASE THEN THE OLD 'FOR' LOOP IS DEACTIVATED.
;* (PURGED FROM THE STACK..) 
;* 
;* 'NEXT VAR' SERVES AS THE LOGICAL (NOT NECESSARILLY PHYSICAL)
;* END OF THE 'FOR' LOOP.  THE CONTROL VARIABLE VAR. IS CHECKED
;* WITH THE 'LOPVAR'.  IF THEY ARE NOT THE SAME, TBI DIGS IN
;* THE STACK TO FIND THE RIGHTt ONE AND PURGES ALL THOSE THAT 
;* DID NOT MATCH.  EITHER WAY, TBI THEN ADDS THE 'STEP' TO 
;* THAT VARIABLE AND CHECK THE RESULT WITH THE LIMIT.  IF IT
;* IS WITHIN THE LIMIT, CONTROL LOOPS BACK TO THE COMMAND
;* FOLLOWING THE 'FOR'.  IF OUTSIDE THE LIMIT, THE SAVE ARER
;* IS PURGED AND EXECUTION CONTINUES.
;* 
FOR:	CALL	PUSHA		; SAVE THE OLD SAVE AREA
	CALL	SETVAL		; SET THE CONTROL VAR.
	DEC	HL		; HL IS ITS ADDRESS 
	LD	(LOPVAR),HL	; SAVE THAT 
	LD	HL,TAB5-1	; USE 'EXEC' TO LOOK
	JP	EXEC		; FOR THE WORD 'TO' 
;**************************************************************
FR1:    RST     0x18	;8*3             ;EXP             ; EVALUATE THE LIMIT
	LD	(LOPLMT),HL	; SAVE THAT 
	LD	HL,TAB6-1	; USE 'EXEC' TO LOOK
	JP	EXEC		; FOR THE WORD 'STEP'
;**************************************************************
FR2:    RST     0x18	;8*3             ;EXP             ; FOUND IT, GET STEP
	JP	FR4
;**************************************************************
FR3:    LD      HL,1;Q           ; NOT FOUND, SET TO 1
FR4:	LD	(LOPINC),HL	; SAVE THAT TOO 
FR5:	LD	HL,(CURRNT)	; SAVE CURRENT LINE # 
	LD	(LOPLN),HL
	EX	DE,HL		; AND TEXT POINTER
	LD	(LOPPT),HL
        LD      BC,0x0A;12Q          ; DIG INTO STACK TO
	LD	HL,(LOPVAR)	; FIND 'LOPVAR' 
	EX	DE,HL
	LD	H,B
	LD	L,B		; HL=0 NOW
	ADD	HL,SP		; HERE IS THE STACK 
        .BYTE    0x3E;76Q
FR7:	ADD	HL,BC		; EACH LEVEL IS 10 DEEP 
	LD	A,(HL)		; GET THAT OLD 'LOPVAR' 
	INC	HL
	OR	(HL)
	JP	Z,FR8		; 0 SAYS NO MORE IN IT
	LD	A,(HL)
	DEC	HL
	CP	D		; SAME AS THIS ONE? 
	JP	NZ,FR7
	LD	A,(HL)		; THE OTHER HALF? 
	CP	E
	JP	NZ,FR7
	EX	DE,HL		; YES, FOUND ONE
        LD      HL,0
	ADD	HL,SP		; TRY TO MOVE SP
	LD	B,H
	LD	C,L
        LD      HL,0x0A;12Q
	ADD	HL,DE
	CALL	MVDOWN		; AND PURGE 10 WORDS
	LD	SP,HL		; IN THE STACK
FR8:	LD	HL,(LOPPT)	; JOB DONE, RESTORE DE
	EX	DE,HL
        RST     0x30	;8*6             ;FINISH; AND CONTINUE
;**************************************************************
NEXT:   RST     0x10	;8*2             ;TSTVAR          ; GET ADDRESS OF VAR.
	JP	C,QWHAT		; NO VARIABLE, "WHAT?"
	LD	(VARNXT),HL	; YES, SAVE IT
NX0:	PUSH	DE		; SAVE TEXT POINTER 
	EX	DE,HL
	LD	HL,(LOPVAR)	; GET VAR. IN 'FOR' 
	LD	A,H
	OR	L		; 0 SAYS NEVER HAD ONE
	JP	Z,AWHAT		; SO WE ASK: "WHAT?"
        RST     0x18	;8*4             ;COMPAR          ; ELSE WE CHECK THEM
	JP	Z,NX3		; OK, THEY AGREE
	POP	DE		; NO, LET'S SEE 
	CALL	POPA		; PURGE CURRENT LOOP
	LD	HL,(VARNXT)	; AND POP ONE LEVEL 
	JP	NX0		; GO CHECK AGAIN
;**************************************************************
NX3:    LD      E,(HL)          ; COME HERE WHEN AGREED 
	INC	HL
	LD	D,(HL)		; DE=VALUE OF VAR.
	LD	HL,(LOPINC)
	PUSH	HL
	ADD	HL,DE		; ADD ONE STEP
	EX	DE,HL
	LD	HL,(LOPVAR)	; PUT IT BACK 
	LD	(HL),E
	INC	HL
	LD	(HL),D
	LD	HL,(LOPLMT)	; HL->LIMIT 
	POP	AF		; OLD HL
	OR	A
	JP	P,NX1		; STEP > 0
	EX	DE,HL
NX1:	CALL	CKHLDE		; COMPARE WITH LIMIT
	POP	DE		; RESTORE TEXT POINTER
	JP	C,NX2		; OUTSIDE LIMIT 
	LD	HL,(LOPLN)	; WITHIN LIMIT, GO
	LD	(CURRNT),HL	; BACK TO THE SAVED 
	LD	HL,(LOPPT)	; 'CURRNT' AND TEXT 
	EX	DE,HL		; POINTER 
        RST     0x30	;8*6             ;FINISH
;**************************************************************
NX2:    CALL    POPA            ; PURGE THIS LOOP
        RST     0x30	;8*6             ;FINISH
;**************************************************************
;* *** REM *** IF *** INPUT *** & LET (& DEFLT) ***
;* 
;* 'REM' CAN BE FOLLOWED BY ANYTHING AND IS IGNORED BY TBI.
;* TBI TREATS IT LIKE AN 'IF' WITH A FALSE CONDITION.
;* 
;* 'IF' IS FOLLOWED BY AN EXPR. AS A CONDITION AND ONE OR MORE 
;* COMMANDS (INCLUDING OUTHER 'IF'S) SEPERATED BY SEMI-COLONS. 
;* NOTE THAT THE WORD 'THEN' IS NOT USED.  TBI EVALUATES THE 
;* EXPR. IF IT IS NON-ZERO, EXECUTION CONTINUES.  IF THE
;* EXPR. IS ZERO, THE COMMANDS THAT FOLLOWS ARE IGNORED AND
;* EXECUTION CONTINUES AT THE NEXT LINE. 
;* 
;* 'IPUT' COMMAND IS LIKE THE 'PRINT' COMMAND, AND IS FOLLOWED
;* BY A LIST OF ITEMS.  IF THE ITEM IS A STRING IN SINGLE OR
;* DOUBLE QUOTES, OR IS A BACK-ARROW, IT HAS THE SAME EFFECT AS
;* IN 'PRINT'.  IF AN ITEM IS A VARIABLE, THIS VARIABLE NAME IS
;* PRINTED OUT FOLLOWED BY A COLON.  THEN TBI WAITS FOR AN 
;* EXPR. TO BE TYPED IN.  THE VARIABLE ISs THEN SET TO THE
;* VALUE OF THIS EXPR.  IF THE VARIABLE IS PROCEDED BY A STRING
;* (AGAIN IN SINGLE OR DOUBLE QUOTES), THE STRING WILL BE
;* PRINTED FOLLOWED BY A COLON.  TBI THEN WAITS FOR INPUT EXPR.
;* AND SET THE VARIABLE TO THE VALUE OF THE EXPR.
;* 
;* IF THE INPUT EXPR. IS INVALID, TBI WILL PRINT "WHAT?",
;* "HOW?" OR "SORRY" AND REPRINT THE PROMPT AND REDO THE INPUT.
;* THE EXECUTION WILL NOT TERMINATE UNLESS YOU TYPE CONTROL-C. 
;* THIS IS HANDLED IN 'INPERR'.
;* 
;* 'LET' IS FOLLOWED BY A LIST OF ITEMS SEPERATED BY COMMAS. 
;* EACH ITEM CONSISTS OF A VARIABLE, AN .EQUAL SIGN, AND AN EXPR. 
;* TBI EVALUATES THE EXPR. AND SET THE VARIBLE TO THAT VALUE.
;* TB WILL ALSO HANDLE 'LET' COMMAND WITHOUT THE WORD 'LET'.
;* THIS IS DONE BY 'DEFLT'.
;* 
REM:    LD      HL,0            ; *** REM ***
;        .BYTE    76Q            ;REM BUG
        JR      IFF1A           ;REM BUG

;* 
IFF:    RST     0x18	;8*3             ;EXP             ; *** IFF ***
IFF1A:  LD      A,H             ; IS THE EXPR.=0?
	OR	L
	JP	NZ,RUNSML	; NO, CONTINUE
	CALL	FNDSKP		; YES, SKIP REST OF LINE
	JP	NC,RUNTSL
	JP	RSTART
;**************************************************************
INPERR:	LD	HL,(STKINP)	; *** INPERR ***
	LD	SP,HL		; RESTORE OLD SP
	POP	HL		; AND OLD 'CURRNT'
	LD	(CURRNT),HL
	POP	DE		; AND OLD TEXT POINTER
	POP	DE		; REDO INPUT
;* 
INPUT	=	.		; *** INPUT *** 
IP1:	PUSH	DE		; SAVE IN CASE OF ERROR 
	CALL	QTSTG		; IS NEXT ITEM A STRING?
	JP	IP2		; NO
        RST     0x10	;8*2             ;TSTVAR          ; YES. BUT FOLLOWED BY A
	JP	C,IP4		; VARIABLE?   NO. 
	JP	IP3		; YES.  INPUT VARIABLE
IP2:	PUSH	DE		; SAVE FOR 'PRTSTG' 
        RST     0x10	;8*2             ;TSTVAR          ; MUST BE VARIABLE NOW
	JP	C,QWHAT		; "WHAT?" IT IS NOT?
	LD	A,(DE)		; GET READY FOR 'RTSTG'
	LD	C,A
	SUB	A
	LD	(DE),A
	POP	DE
	CALL	PRTSTG		; PRINT STRING AS PROMPT
	LD	A,C		; RESTORE TEXT
	DEC	DE
	LD	(DE),A
IP3:	PUSH	DE		; SAVE IN CASE OF ERROR 
	EX	DE,HL
	LD	HL,(CURRNT)	; ALSO SAVE 'CURRNT'
	PUSH	HL
	LD	HL,IP1		; A NEGATIVE NUMBER 
	LD	(CURRNT),HL	; AS A FLAG 
        LD      HL,0            ; SAVE SP TOO
	ADD	HL,SP
	LD	(STKINP),HL
	PUSH	DE		; OLD HL
        LD      A,0x3A;72Q           ; PRINT THIS TOO
	CALL	GETLN		; AND GET A LINE
IP3A:   LD      DE,BUFFER       ; POINTS TO BUFFER
        RST     0x18	;8*3             ;EXP             ; EVALUATE INPUT
;        NOP                     ; CAN BE 'CALL ENDCHK'
;        NOP
;        NOP
	POP	DE		; OK, GET OLD HL
	EX	DE,HL
	LD	(HL),E		; SAVE VALUE IN VAR.
	INC	HL
	LD	(HL),D
	POP	HL		; GET OLD 'CURRNT'
	LD	(CURRNT),HL
	POP	DE		; AND OLD TEXT POINTER
IP4:	POP	AF		; PURGE JUNK IN STACK 

        RST     0x08	;8*1             ;TSTCHR;IS NEXT CHR A COMMA?
        .BYTE    ',             ;COMMA?
        .BYTE    IP5-1-.

        JP      IP1             ; YES, MORE ITEMS.
IP5:    RST     0x30	;8*6             ;FINISH
;**************************************************************
DEFLT:	LD	A,(DE)		; *** DEFLT *** 
        CP      CR              ; EMPTY LINE IS OK
	JP	Z,LT1		; ELSE IT IS 'LET'
;* 
LET:	CALL	SETVAL		; *** LET *** 
        RST     0x08	;8*1             ;TSTCHR; SET VALUE TO VAR.
	.BYTE	',
        .BYTE    LT1-1-.

        JP      LET             ; ITEM BY ITEM
LT1:    RST     0x30	;8*6             ;FINISH
;**************************************************************
;* 
;* *** EXPR ***
;* 
;* 'EXPR' EVALUATES ARITHMETICAL OR LOGICAL EXPRESSIONS. 
;* <EXPR>::=<EXPR2>
;*          <EXPR2><REL.OP.><EXPR2>
;* WHERE <REL.OP.> IS ONE OF THE OPERATORS IN TAB8 AND THE
;* RESULT OF THESE OPERATIONS IS 1 IF TRUE AND 0 IF FALSE.
;* <EXPR2>::=(+ OR -)<EXPR3>(+ OR -<EXPR3>)(....)
;* WHERE () ARE OPTIONAL AND (....) ARE OPTIONAL REPEATS.
;* <EXPR3>::=<EXPR4>(<* OR /><EXPR4>)(....)
;* <EXPR4>::=<VARIABLE>
;*           <FUNCTION>
;*           (<EXPR>)
;* <EXPR> IS RECURSIVE SO THAT VARIABLE '@' CAN HAVE AN <EXPR> 
;* AS INDEX, FNCTIONS CAN HAVE AN <EXPR> AS ARGUMENTS, AND
;* <EXPR4> CAN BE AN <EXPR> IN PARANTHESE. 
;* 
;*                 EXPR   CALL EXPR2     THIS IS AT LOC. 18
;*                        PUSH HL        SAVE <EXPR2> VALUE
;**************************************************************
EXPR1:  LD      HL,TAB8-1       ; LOOKUP REL.OP.
	JP	EXEC		; GO DO IT
;**************************************************************
GTE:    CALL    COMREL          ;REL.OP.">="
        RET     C               ;NO, RETURN HL=0
        LD      L,A             ;YES, RETURN HL=1
	RET
;**************************************************************
NOTEQU: CALL    COMREL          ;REL.OP."<>"
        RET     Z               ;FALSE, RETURN HL=0
        LD      L,A             ;TRUE, RETURN HL=1
	RET
;**************************************************************
GREATER:CALL    COMREL          ;REL.OP.">"
        RET     Z               ;FALSE
        RET     C               ;ALSO FALSE, HL=0
        LD      L,A             ;TRUE, HL=1
	RET
;**************************************************************
LESSEQ: CALL    COMREL          ;REL.OP."<="
        LD      L,A             ;SET HL=1
        RET     Z               ;REL. TRUE, RETURN
	RET	C
        LD      L,H             ;ELSE SET HL=0
	RET
;**************************************************************
EQUAL:  CALL    COMREL          ;REL.OP."="
        RET     NZ              ;FALSE, RETRUN HL=0
        LD      L,A             ;ELSE SET HL=1
	RET
;**************************************************************
LESS:   CALL    COMREL          ;REL.OP."<"
        RET     NC              ;FALSE, RETURN HL=0
        LD      L,A             ;ELSE SET HL=1
	RET
;**************************************************************
NOTREL: POP     HL              ;NOT REL.OP.
        RET                     ;RETURN HL=<EXPR2>
;**************************************************************
COMREL: LD      A,C             ; SUBROUTINE FOR ALL
	POP	HL		; REL.OP.'S 
	POP	BC
	PUSH	HL		; REVERSE TOP OF STACK
	PUSH	BC
	LD	C,A
	CALL	EXPR2		; GET 2ND <EXPR2> 
	EX	DE,HL		; VALUE IN DE NOW 
	EX	(SP),HL		; 1ST <EXPR2> IN HL 
	CALL	CKHLDE		; COMPARE 1ST WITH 2ND
	POP	DE		; RESTORE TEXT POINTER
        LD      HL,0            ; SET HL=0, A=1
	LD	A,1
	RET
;**************************************************************
EXPR2:  RST     0x08	;8*1             ;TSTCHR; NEGATIVE SIGN?
	.BYTE	'-
        .BYTE    TRYPOS-1-.

        LD      HL,0            ; YES, FAKE '0-'
        JP      TISSUB          ; TREAT LIKE SUBTRACT
;**************************************************************
TRYPOS: RST     0x08	;8*1             ;TSTCHR; POSITIVE SIGN?  IGNORE
	.BYTE	'+
        .BYTE    NOTFAR-1-.

NOTFAR: CALL    EXPR3           ; 1ST <EXPR3>
XP23:   RST     0x08	;8*1             ;TSTCHR; ADD?
	.BYTE	'+
        .BYTE    TRYSUB-1-.

        PUSH    HL              ; YES, SAVE VALUE
	CALL	EXPR3		; GET 2ND<EXPR3> 
XP24:	EX	DE,HL		; 2ND IN DE 
	EX	(SP),HL		; 1ST IN HL 
	LD	A,H		; COMPARE SIGN
	XOR	D
	LD	A,D
	ADD	HL,DE
	POP	DE		; RESTORE TEXT POINTER
	JP	M,XP23		; 1ST 2ND SIGN DIFFER 
	XOR	H		; 1ST 2ND SIGN .EQUAL
        JP      P,XP23          ; SO IS RESULT
	JP	QHOW		; ELSE WE HAVE OVERFLOW 
;**************************************************************
TRYSUB: RST     0x08	;8*1             ;TSTCHR; SUBTRACT?
	.BYTE	'-
        .BYTE    XIT-1-.;;TRYAND-1-$;;;;XIT-1-$

TISSUB: PUSH    HL              ; YES, SAVE 1ST <EXPR3>
	CALL	EXPR3		; GET 2ND <EXPR3> 
	CALL	CHGSGN		; NEGATE
	JP	XP24		; AND ADD THEM
;**************************************************************
EXPR3:	CALL	EXPR4		; GET 1ST <EXPR4> 
XP31:   RST     0x08	;8*1             ;TSTCHR; MULTIPLY?
	.BYTE	'*
        .BYTE    TRYDIV-1-.

        PUSH    HL              ; YES, SAVE 1ST
	CALL	EXPR4		; AND GET 2ND <EXPR4> 
        LD      B,0             ; CLEAR B FOR SIGN
	CALL	CHKSGN		; CHECK SIGN
	EX	DE,HL		; 2ND IN DE NOW 
	EX	(SP),HL		; 1ST IN HL 
	CALL	CHKSGN		; CHECK SIGN OF 1ST 
	LD	A,H		; IS HL > 255 ? 
	OR	A
	JP	Z,XP32		; NO
	LD	A,D		; YES, HOW ABOUT DE 
	OR	D
	EX	DE,HL		; PUT SMALLER IN HL 
	JP	NZ,AHOW		; ALSO >, WILL OVERFLOW 
XP32:	LD	A,L		; THIS IS DUMB
        LD      HL,0            ; CLEAR RESULT
	OR	A		; ADD AND COUNT 
	JP	Z,XP35
XP33:	ADD	HL,DE
	JP	C,AHOW		; OVERFLOW
	DEC	A
	JP	NZ,XP33
	JP	XP35		; FINISHED
;**************************************************************
TRYDIV: RST     0x08	;8*1             ;TSTCHR; DIVIDE?
	.BYTE	'/
        .BYTE    TRYAND-1-.;;;XIT-1-$

        PUSH    HL              ; YES, SAVE 1ST <EXPR4>
	CALL	EXPR4		; AND GET 2ND ONE 
        LD      B,0             ; CLEAR B FOR SIGN
	CALL	CHKSGN		; CHECK SIGN OF 2ND 
	EX	DE,HL		; PUT 2ND IN DE 
	EX	(SP),HL		; GET 1ST IN HL 
        CALL    CHKSGN          ; CHECK SIGN OF 1ST
        LD      A,D             ; DIVIDE BY 0?
	OR	E
	JP	Z,AHOW		; SAY "HOW?"
	PUSH	BC		; ELSE SAVE SIGN
	CALL	DIVIDE		; USE SUBROUTINE
	LD	H,B		; RESULT IN HL NOW
	LD	L,C
	POP	BC		; GET SIGN BACK 
XP35:	POP	DE		; AND TEXT POINTER
	LD	A,H		; HL MUST BE +
	OR	A
	JP	M,QHOW		; ELSE IT IS OVERFLOW 
	LD	A,B
	OR	A
        CALL    M,CHGSGN        ; CHANGE SIGN IF NEEDED
	JP	XP31		; LOOK OR MORE TERMS 
;**************************************************************
TRYAND: RST     0x08	;8*1             ;TSTCHR;LOGICAL AND
        .BYTE    'a
        .BYTE    TRYOR-1-.;;;XIT-1-$

        PUSH    HL              ; YES, SAVE VALUE

;        INC     DE              ;BUMP OVER 'N'
;        LD      A,(DE)
;        CP      'N'
;        JP      Z,AHOW
;        INC     DE              ;BUMP OVER 'D'
;        LD      A,(DE)
;        CP      'D'
;        JP      Z,AHOW

        CALL    EXPR3           ; GET 2ND<EXPR3>
        EX      DE,HL           ; 2ND IN DE
        EX      (SP),HL         ; 1ST IN HL 

        LD      H,0             ;B BIT LOGIC OR FUNCTION
        LD      D,0
        LD      A,L
        AND     E

LOGXIT: LD      L,A
        POP     DE              ; RESTORE TEXT POINTER
        JP      XP23
;**************************************************************
TRYOR:  RST     0x08	;8*1             ;TSTCHR;LOGICAL OR
        .BYTE    'o
        .BYTE    TRYXOR-1-.

        PUSH    HL              ; YES, SAVE VALUE
        CALL    EXPR3           ; GET 2ND<EXPR3>
        EX      DE,HL           ; 2ND IN DE
        EX      (SP),HL         ; 1ST IN HL 

        LD      H,0             ;8 BIT OR FUNCTION
        LD      D,0
        LD      A,L
        OR      E

        JR      LOGXIT
;**************************************************************
TRYXOR: RST     0x08	;8*1             ;TSTCHR;LOGICAL XOR
        .BYTE    'x
        .BYTE    TRYLEF-1-.

        PUSH    HL              ; YES, SAVE VALUE
        CALL    EXPR3           ; GET 2ND<EXPR3>
        EX      DE,HL           ; 2ND IN DE
        EX      (SP),HL         ; 1ST IN HL 

        LD      H,0             ;8 BIT XOR FUNCTION
        LD      D,0
        LD      A,L
        XOR     E

        JR      LOGXIT
;**************************************************************
TRYLEF: RST     0x08	;8*1             ;TSTCHR;LOGICAL SHIFT LEFT
        .BYTE    'l
        .BYTE    TRYRT-1-.

        PUSH    HL              ; YES, SAVE VALUE
        CALL    EXPR3           ; GET 2ND<EXPR3>
        EX      DE,HL           ; 2ND IN DE
        EX      (SP),HL         ; 1ST IN HL 

;        LD      H,0             

        LD      D,0             ;8 BIT SHIFT LEFT FUNCTION
LEFTLP: LD      A,L
        RLCA
        DEC     H
        JR      NZ,LEFTLP

        JR      LOGXIT
;**************************************************************
TRYRT:  RST     0x08	;8*1             ;TSTCHR;LOGICAL SHIFT RIGHT
        .BYTE    'r
        .BYTE    XIT-1-.

        PUSH    HL              ; YES, SAVE VALUE
        CALL    EXPR3           ; GET 2ND<EXPR3>
        EX      DE,HL           ; 2ND IN DE
        EX      (SP),HL         ; 1ST IN HL 

;        LD      H,0             

        LD      D,0             ;8 BIT SHIFT RIGHT FUNCTION
RTLP:   LD      A,L
        RRCA
        DEC     H
        JR      NZ,RTLP

        JR      LOGXIT
;**************************************************************
EXPR4:  LD      HL,TAB4-1       ; FIND FUNCTION IN TAB4 
	JP	EXEC		; AND GO DO IT
;**************************************************************
XP40:   RST     0x10	;8*2             ;TSTVAR          ; NO, NOT A FUNCTION
	JP	C,XP41		; NOR A VARIABLE
	LD	A,(HL)		; VARIABLE
	INC	HL
	LD	H,(HL)		; VALUE IN HL 
	LD	L,A
	RET
;**************************************************************
XP41:   CALL    TSTNUM          ; OR IS IT A NUMBER 
	LD	A,B		; # OF DIGIT
	OR	A
	RET	NZ		; OK

PARN:   RST     0x08	;8*1             ;TSTCHR; NO DIGIT, MUST BE
	.BYTE	'(
        .BYTE    XQWHAT-1-.

        RST     0x18	;8*3             ;"(EXPR)"
        RST     0x08	;8*1             ;TSTCHR
        .BYTE    ')
        .BYTE    XQWHAT-1-.
XIT:    RET
;**************************************************************
XQWHAT: JP      QWHAT           ; ELSE SAY: "WHAT?"
;**************************************************************
RND:	CALL	PARN		; *** RND(EXPR) *** 
	LD	A,H		; EXPR MUST BE +
	OR	A
	JP	M,QHOW
	OR	L		; AND NON-ZERO
	JP	Z,QHOW
	PUSH	DE		; SAVE BOTH 
	PUSH	HL
	LD	HL,(RANPNT)	; GET MEMORY AS RANDOM
	LD	DE,LSTROM	; NUMBER
        RST     0x20	;8*4             ;COMPAR          ;
        JP      C,RA1           ; WRAP AROUND IF LAST
	LD	HL,START
RA1:	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	LD	(RANPNT),HL
	POP	HL
	EX	DE,HL
	PUSH	BC
	CALL	DIVIDE		; RND(N)=MOD(M,N)+1 
	POP	BC
	POP	DE
	INC	HL
	RET
;**************************************************************
ABS:	CALL	PARN		; *** ABS(EXPR) *** 
	CALL	CHKSGN		; CHECK SIGN
	LD	A,H		; NOTE THAT -32768
	OR	H		; CANNOT CHANGE SIGN
	JP	M,QHOW		; SO SAY: "HOW?"
	RET
;**************************************************************
SIZE:   LD      HL,(TXTUNF)     ; *** SIZE ***
	PUSH	DE		; GET THE NUMBER OF FREE
	EX	DE,HL		; BYTES BETWEEN 'TXTUNF'

;;SIZEA:  LD      HL,VARBGN       ; AND 'VARBGN'
SIZEA:  LD      HL,TXTEND       ;AND TXTEND

        CALL    SUBDE
	POP	DE
	RET
;*********************************************************
;*
;*   *** OUT *** INP *** WAIT *** POKE *** PEEK *** & USR
;*
;*  OUT I,J(,K,L)
;*
;*  OUTPUTS EXPRESSION 'J' TO PORT 'I', AND MAY BE REPEATED
;*  AS IN DATA 'L' TO PORT 'K' AS MANY TIMES AS NEEDED
;*
;*  INP (I)
;*
;*  THIS FUNCTION RETURNS DATA READ FROM INPUT PORT 'I' AS
;*  IT'S VALUE.
;*
;*  WAIT I,J,K
;*
;*  THIS COMMAND READS THE STATUS OF PORT 'I', EXCLUSIVE OR'S
;*  THE RESULT WITH 'K' IF THERE IS ONE, OR IF NOT WITH 0, 
;*  AND'S WITH 'J' AND RETURNS WHEN THE RESULT IS NONZERO.
;*
;*  POKE I,J(,K,L)
;*
;*  THIS COMMAND WORKS LIKE OUT EXCEPT THAT IT PUTS DATA 'J'
;*  INTO MEMORY LOCATION 'I'.
;*
;*  PEEK (I)
;*
;*  THIS FUNCTION WORKS LIKE INP EXCEPT IT GETS IT'S VALUE
;*  FROM MEMORY LOCATION 'I'.
;*
;*  USR (I(,J))
;*
;*  USR CALLS A MACHINE LANGUAGE SUBROUTINE AT LOCATION 'I'
;*  IF THE OPTIONAL PARAMETER 'J' IS USED ITS VALUE IS PASSED
;*  IN H&L.  THE VALUE OF THE FUNCTION SHOULD BE RETURNED IN H&L.
;*
;************************************************************
OUTCMD: RST     0x18	;8*3             ;EXP;RESULT TO L
        LD      H,0             ;FOR PBUFF
        PUSH    HL              ;SAVE PORT
        POP     IX              ;TO IX IN PBUFF FORMAT

        RST     0x08	;8*1             ;TSTCHR;IS NEXT CHR A COMMA?
        .BYTE    ',             ;COMMA?
        .BYTE    VQWHAT-1-.      ;NO COMMA WHAT!!

        RST     0x18	;8*3             ;EXP;VALUE TO OUT IN L

        PUSH    BC              ;SAVE OLD BC

        PUSH    IX              ;PORT TO STACK
        POP     BC              ;PORT TO BC
        OUT     (C),L           ;OP VALUE TO PORT

        POP     BC              ;REST OLD BC

        RST     0x08	;8*1             ;TSTCHR;IS NEXT CHR A COMMA?
        .BYTE    ',             ;COMMA?
        .BYTE    OUTXIT-1-.      ;NO, FINISH OFF
        JP      OUTCMD          ;YES, DO IT AGAIN
OUTXIT: RST     0x30	;8*6             ;FINISH          ;
;************************************************************
WAITCM: RST     0x18	;8*3             ;EXP;PORT TO L
        LD      H,0             ;CLEAR H
        PUSH    HL              ;STORE IT
        POP     IX              ;POP IT TO IX, CONTAINS PORT

        RST     0x08	;8*1             ;TSTCHR;IS NEXT CHR A COMMA?
        .BYTE    ',             ;COMMA?
        .BYTE    VQWHAT-1-.      ;NO COMMA WHAT!!

WT1:    RST     0x18	;8*3             ;EXP ;PORT TEST VALUE TO L

        LD      A,L
        EX      AF,AF'          ;CONTAINS TEST VALUE

        RST     0x08	;8*1             ;TSTCHR; A COMMA?
        .BYTE    ',             ;COMMA?
        .BYTE    WAIT1-1-.       ;NO SECOND COMMA, LAST =0

        RST     0x18	;8*3             ;EXP;XORED WAIT VALUE TO L
        LD      H,L             ;PORT TEST VALUE

        JR      WAITIO          ;
WAIT1:  LD      H,255           ;DEFAULT, CHECK ALL BITS

WAITIO:
        EX      AF,AF'          ;GET TEST VALUE
        LD      L,A             ;STORE VAL TO L

        LD      A,H
        AND     L
        LD      L,A             ;AND TEST VAL WITH MASK

        PUSH    BC              ;SAVE OLD BC

        PUSH    IX              ;PORT TO STACK
        POP     BC              ;PORT TO BC
WAITLP:
        IN      A,(C)           ;GET PORT DATA
        AND     H               ;MASK UN-NEEDED BITS
        CP      L               ;COMPARE MASK
        JR      NZ,WAITLP       ;NO GOOD, TRY AGAIN

        POP     BC              ;RESTORE OLD BC
        RST     0x30	;8*6             ;FINISH ,I'M OUTA HERE
;**************************************************************
INP:    CALL    PARN            ;GET PORT # TO L
        LD      H,0             ;CLR MSB, 8 BIT ONLY
        PUSH    BC              ;SAVE BC
        LD      B,0             ;CLEAR FOR PBUFF PORT
        LD      C,L             ;SET 8 BIT PORT
        IN      L,(C)           ;GET DATA FOR RETURN
        POP     BC              ;RESTORE BC
        RET
;**************************************************************
VQWHAT: JP      QWHAT
;**************************************************************
POKE:   RST     0x18	;8*3             ;EXP             ;
        PUSH    HL

        RST     0x08	;8*1             ;TSTCHR;IS NEXT CHR A COMMA?
        .BYTE    ',             ;COMMA?
        .BYTE    NEXT9-1-.

        RST     0x18	;8*3             ;EXP             ;
        LD      A,L
	POP	HL
	LD	(HL),A

        RST     0x08	;8*1             ;TSTCHR;IS NEXT CHR A COMMA?
        .BYTE    ',             ;COMMA?
        .BYTE    FINPOKE-1-.

        JP      POKE
FINPOKE:RST     0x30	;8*6             ;FINISH
;**************************************************************
PEEK:   CALL    PARN
	LD	L,(HL)
	LD	H,0
        RET
;**************************************************************
NEXT9:  JP      QWHAT
;**************************************************************
USR:    PUSH    BC
        RST     0x08	;8*1             ;TSTCHR
        .BYTE    '(
        .BYTE    NEXT10-1-.

        RST     0x18	;8*3             ;EXPR
        RST     0x08	;8*1             ;TSTCHR
        .BYTE    ')
        .BYTE    PASPRM-1-.

        PUSH    DE
	LD	DE,USRET
	PUSH	DE

;BUG
        LD      A,H
;        OR      A,USR_OFF               ;USR @ 8000H PLUS FOR ZLOAD
        LD      H,A

        PUSH    HL
	RET			; CALL USR ROUTINE
;**************************************************************
PASPRM: RST     0x08	;8*1             ;TSTCHR;IS NEXT CHR A COMMA?
        .BYTE    ',             ;COMMA?
        .BYTE    NEXT10-1-.

        PUSH    HL
        RST     0x18	;8*3             ;EXP             ;

        RST     0x08	;8*1             ;TSTCHR
        .BYTE    ')
        .BYTE    NEXT10-1-.

        POP     BC
	PUSH	DE
	LD	DE,USRET
	PUSH	DE

;BUG
        LD      A,B
;        OR      A,USR_OFF               ;USR @ 8000H PLUS FOR ZLOAD
        LD      B,A

        PUSH    BC
        RET                     ; CALL USR ROUTINE
;**************************************************************
USRET:  POP     DE
NEXT10: POP     BC
	RET
;**************************************************************
;;?????        JP      QWHAT
;**************************************************************
;* 
;* *** DIVIDE *** SUBDE *** CHKSGN *** CHGSGN *** & CKHLDE *** 
;* 
;* 'DIVIDE' DIVIDES HL BY DE, RESULT IN BC, REMAINDER IN HL
;* 
;* 'SUBDE' SUBTRACTS DE FROM HL
;* 
;* 'CHKSGN' CHECKS SIGN OF HL.  IF +, NO CHANGE.  IF -, CHANGE
;* SIGN AND FLIP SIGN OF B.
;* 
;* 'CHGSGN' CHNGES SIGN OF HL AND B UNCONDITIONALLY. 
;* 
;* 'CKHLDE' CHECKS SIGN OF HL AND DE.  IF DIFFERENT, HL AND DE
;* ARE INTERCHANGED.  IF SAME SIGN, NOT INTERCHANGED.  EITHER
;* CASE, HL DE ARE THEN COMPARED TO SET THE FLAGS. 
;* 
DIVIDE:	PUSH	HL		; *** DIVIDE ***
	LD	L,H		; DIVIDE H BY DE
	LD	H,0
	CALL	DV1
	LD	B,C		; SAVE RESULT IN B
	LD	A,L		; (REMAINDER+L)/DE
	POP	HL
	LD	H,A
DV1:    LD      C,0x0FF;377Q          ; RESULT IN C
DV2:	INC	C		; DUMB ROUTINE
	CALL	SUBDE		; DIVIDE BY SUBTRACT
	JP	NC,DV2		; AND COUNT 
	ADD	HL,DE
	RET
;**************************************************************
SUBDE:	LD	A,L		; *** SUBDE *** 
	SUB	E		; SUBTRACT DE FROM
	LD	L,A		; HL
	LD	A,H
	SBC	A,D
	LD	H,A
	RET
;**************************************************************
CHKSGN:	LD	A,H		; *** CHKSGN ***
	OR	A		; CHECK SIGN OF HL
        RET     P               ; IF -, CHANGE SIGN
;* 
CHGSGN:	LD	A,H		; *** CHGSGN ***
	CPL			; CHANGE SIGN OF HL 
	LD	H,A
	LD	A,L
	CPL
	LD	L,A
	INC	HL
	LD	A,B		; AND ALSO FLIP B 
        XOR     0x80;200Q
	LD	B,A
	RET
;**************************************************************
CKHLDE:	LD	A,H
	XOR	D		; SAME SIGN?
	JP	P,CK1		; YES, COMPARE
	EX	DE,HL		; NO, XCH AND COMP
CK1:    RST     0x20	;8*4             ;COMPAR
        RET                     ;*
;**************************************************************
;* 
;* *** SETVAL *** FIN *** ENDCHK *** & ERROR (& FRIENDS) *** 
;* 
;* "SETVAL" EXPECTS A VARIABLE, FOLLOWED BY AN .EQUAL SIGN AND
;* THEN AN EXPR.  IT EVALUATES THE EXPR. AND SET THE VARIABLE
;* TO THAT VALUE.
;* 
;* "FIN" CHECKS THE END OF A COMMAND.  IF IT ENDED WITH ";",
;* EXECUTION CONTINUES.  IF IT ENDED WITH A CR, IT FINDS THE
;* NEXT LINE AND CONTINUE FROM THERE.
;* 
;* "ENDCHK" CHECKS IF A COMMAND IS ENDED WITH CR.  THIS IS
;* R.EQUIRED IN CERTAIN COMMANDS. (GOTO, RETURN, AND STOP ETC.) 
;* 
;* "ERROR" PRINTS THE STRING POINTED BY DE (AND ENDS WITH CR). 
;* IT THEN PRINTS THE LINE POINTED BY 'CURRNT' WITH A "?"
;* INSERTED AT WHERE THE OLD TEXT POINTER (SHOULD BE ON TOP
;* O THE STACK) POINTS TO.  EXECUTION OF TB IS STOPPED
;* AND TBI IS RESTARTED.  HOWEVER, IF 'CURRNT' -> ZERO
;* (INDICATING A DIRECT COMMAND), THE DIRECT COMMAND IS NOT
;*  PRINTED.  AND IF 'CURRNT' -> NEGATIVE # (INDICATING 'INPUT'
;* COMMAND, THE INPUT LINE IS NOT PRINTED AND EXECUTION IS 
;* NOT TERMINATED BUT CONTINUED AT 'INPERR'. 
;* 
;* RELATED TO 'ERROR' ARE THE FOLLOWING: 
;* 'QWHAT' SAVES TEXT POINTER IN STACK AND GET MESSAGE "WHAT?" 
;* 'AWHAT' JUST GET MESSAGE "WHAT?" AND JUMP TO 'ERROR'. 
;* 'QSORRY' AND 'ASORRY' DO SAME KIND OF THING.
;* 'QHOW' AND 'AHOW' IN THE ZERO PAGE SECTION ALSO DO THIS 
;* 
SETVAL: RST     0x10	;8*2             ;TSTVAR          ; *** SETVAL ***
	JP	C,QWHAT		; "WHAT?" NO VARIABLE 
	PUSH	HL		; SAVE ADDRESS OF VAR.
        RST     0x08	;8*1             ;TSTCHR; PASS "=" SIGN
	.BYTE	'=

;08H        .BYTE    10Q
        .BYTE    SV1-1-.

        RST     0x18	;8*3             ;EVALUATE EXPR.
	LD	B,H		; VALUE IN BC NOW 
	LD	C,L
	POP	HL		; GET ADDRESS 
	LD	(HL),C		; SAVE VALUE
	INC	HL
	LD	(HL),B
	RET
;**************************************************************
SV1:    JP      QWHAT           ; NO "=" SIGN 
;* 
FIN:    RST     0x08	;8*1             ;TSTCHR; *** FIN ***

;;        .BYTE    73Q
        .BYTE    ':             ;MULTIPLE STATS ON ONE LINE

;        .BYTE    4Q
        .BYTE    FI1-1-.

        POP     AF              ; ";", PURGE RET ADDR.
	JP	RUNSML		; CONTINUE SAME LINE
FI1:    RST     0x08	;8*1             ;TSTCHR; NOT ";", IS IT CR?
        .BYTE    CR

;        .BYTE    4Q
        .BYTE    FI2-1-.

        POP     AF              ; YES, PURGE RET ADDR.
	JP	RUNNXL		; RUN NEXT LINE 
FI2:	RET			; ELSE RETURN TO CALLER 
;**************************************************************
ENDCHK: RST     0x28	;8*5             ;IGNBLK & *** ENDCHK ***
        CP      CR              ; END WITH CR?
	RET	Z		; OK, ELSE SAY: "WHAT?" 
;* 
QWHAT:	PUSH	DE		; *** QWHAT *** 
AWHAT:	LD	DE,WHAT		; *** AWHAT *** 
ERROR:	SUB	A		; *** ERROR *** 
	CALL	PRTSTG		; PRINT 'WHAT?', 'HOW?' 
	POP	DE		; OR 'SORRY'
	LD	A,(DE)		; SAVE THE CHARACTER
	PUSH	AF		; AT WHERE OLD DE ->
	SUB	A		; AND PUT A 0 THERE 
	LD	(DE),A
	LD	HL,(CURRNT)	; GET CURRENT LINE #
	PUSH	HL
	LD	A,(HL)		; CHECK THE VALUE 
	INC	HL
	OR	(HL)
	POP	DE
        JP      Z,RSTART        ; IF ZERO, JUST RERSTART
        LD      A,(HL)          ; IF NEGATIVE,
	OR	A
	JP	M,INPERR	; REDO INPUT
	CALL	PRTLN		; ELSE PRINT THE LINE 
	DEC	DE		; UPTO WHERE THE 0 IS 
	POP	AF		; RESTORE THE CHARACTER 
	LD	(DE),A
        LD      A,'?;3FH;77Q           ; PRINT A "?"
        CALL    CHROUT          ;
	SUB	A		; AND THE REST OF THE 
	CALL	PRTSTG		; LINE
	JP	RSTART
QSORRY:	PUSH	DE		; *** QSORRY ***
ASORRY:	LD	DE,SORRY	; *** ASORRY ***
	JP	ERROR
;* 
;**************************************************************
;* 
;* *** GETLN *** FNDLN (& FRIENDS) *** 
;* 
;* 'GETLN' READS A INPUT LINE INTO 'BUFFER'.  IT FIRST PROMPT
;* THE CHARACTER IN A (GIVEN BY THE CALLER), THEN IT FILLS THE 
;* THE BUFFER AND ECHOS.  IT IGNORES LF'S AND NULLS, BUT STILL 
;* ECHOS THEM BACK.  RUB-OUT IS USED TO CAUSE IT TO DELETE 
;* THE LAST CHARATER (IF THERE IS ONE), AND ALT-MOD IS USED TO
;* CAUSE IT TO DELETE THE WHOLE LINE AND START IT ALL OVER.
;* 0DH SIGNALS THE END OF A LINE, AND CAUE 'GETLN' TO RETURN.
;* 
;* 'FNDLN' FINDS A LINE WITH A GIVEN LINE # (IN HL) IN THE 
;* TEXT SAVE AREA.  DE IS USED AS THE TEXT POINTER.  IF THE
;* LINE IS FOUND, DE WILL POINT TO THE BEGINNING OF THAT LINE
;* (I.E., THE LOW BYTE OF THE LINE #), AND FLAGS ARE NC & Z. 
;* IF THAT LINE IS NOT THERE AND A LINE WITH A HIGHER LINE #
;* IS FOUND, DE POINTS TO THERE AND FLAGS ARE NC & NZ.  IF
;* WE REACHED THE END OF TEXT SAVE ARE AND CANNOT FIND THE 
;* LINE, FLAGS ARE C & NZ. 
;* 'FNDLN' WILL INITIALIZE DE TO THE BEGINNING OF THE TEXT SAVE
;* AREA TO START THE SEARCH.  SOME OTHER ENTRIES OF THIS 
;* ROUTINE WILL NOT INITIALIZE DE AND DO THE SEARCH. 
;* 'FNDLNP' WILL START WITH DE AND SEARCH FOR THE LINE #.
;* 'FNDNXT' WILL BUMP DE BY 2, FIND A 0DH AND THEN START SEARCH.
;* 'FNDSKP' USE DE TO FIND A CR, AND THEN STRART SEARCH. 
;* 
GETLN:
        CALL    CHROUT          ; *** GETLN ***
        LD      DE,BUFFER       ; PROMPT AND INIT    msb buff mod
GL1:	CALL	CHKIO			; CHECK KEYBOARD
		JP		NC,GL1			; NO INPUT, WAIT
;;        CP      177Q   ;127H    ; DELETE LST CHARACTER?
        CP      0x08
        JP      Z,DELCHR	; YES
        CP      0x0A		;12Q	; IGNORE LF
		JP		Z,GL1
		OR		A			; IGNORE NULL 
		JP		Z,GL1
        CP      0x5C		;134Q	; DELETE THE WHOLE LINE?
		JP		Z,GL4		; YES 
		LD		(DE),A		; ELSE, SAVE INPUT
		INC		DE			; AND BUMP POINTER
		CP      CR			;15Q		; WAS IT CR?
		JP		NZ,GL2		; NO
        LD      A,LF		;12Q	; YES, GET LINE FEED
        CALL    CHROUT      ; print a line feed
		RET					; WE'VE GOT A LINE
;**************************************************************
GL2:    LD      A,E             ; MORE FREE ROOM?
		CP		BUFEND & 0x0FF
		JP		NZ,GL1			; YES, GET NEXT INPUT 
DELCHR: LD      A,E             ; DELETE LAST CHARACTER
		CP		BUFFER & 0x0FF	; BUT DO WE HAVE ANY? 
		JP		Z,GL4			; NO, REDO WHOLE LINE 
		DEC		DE				; YES, BACKUP POINTER 

        LD      A,0x20          ; DELETE CHR ON LINE
        CALL    CHROUT
        LD      A,8
        CALL    CHROUT

        JP      GL1             ; GO GET NEXT INPUT
GL4:    CALL    CRLF            ; REDO ENTIRE LINE
        LD      A,0x5E;136Q          ; CR, LF AND UP-ARROW
		JP		GETLN
;* 
FNDLN:	LD	A,H		; *** FNDLN *** 
	OR	A		; CHECK SIGN OF HL
	JP	M,QHOW		; IT CANNT BE -
	LD	DE,TXTBGN	; INIT. TEXT POINTER
;* 
FNDLNP	=	.		; *** FNDLNP ***
FL1:	PUSH	HL		; SAVE LINE # 
        LD      HL,(TXTUNF)     ; CHECK IF WE PASSED END
	DEC	HL
        RST     0x20	;8*4             ;COMPAR
        POP     HL              ; GET LINE # BACK
	RET	C		; C,NZ PASSED END 
	LD	A,(DE)		; WE DID NOT, GET BYTE 1
	SUB	L		; IS THIS THE LINE? 
	LD	B,A		; COMPARE LOW ORDER 
	INC	DE
	LD	A,(DE)		; GET BYTE 2
	SBC	A,H		; COMPARE HIGH ORDER
	JP	C,FL2		; NO, NOT THERE YET 
	DEC	DE		; ELSE WE EITHER FOUND
	OR	B		; IT, OR IT IS NOT THERE
	RET			; NC,Z:FOUND; NC,NZ:NO
;**************************************************************

FNDNXT	=	.		; *** FNDNXT ***
	INC	DE		; FIND NEXT LINE
FL2:	INC	DE		; JUST PASSED BYTE 1 & 2
;* 
FNDSKP:	LD	A,(DE)		; *** FNDSKP ***
        CP      CR              ; TRY TO FIND 0DH
	JP	NZ,FL2		; KEEP LOOKING
	INC	DE		; FOUND CR, SKIP OVER 
        JP      FL1             ; CHECK IF END OF TEXT
;* 
;*************************************************************
;* 
;* *** PRTSTG *** QTSTG *** PRTNUM *** & PRTLN *** 
;* 
;* 'PRTSTG' PRINTS A STRING POINTED BY DE.  IT STOPS PRINTING
;* AND RETURNS TO CALLER WHEN EITHER A 0DH IS PRINTED OR WHEN
;* THE NEXT BYTE IS THE SAME AS WHAT WAS IN A (GIVEN BY THE
;* CALLER).  OLD A IS STORED IN B, OLD B IS LOST.
;* 
;* 'QTSTG' LOOKS FOR A BACK-ARROW, SINGLE QUOTE, OR DOUBLE 
;* QUOTE.  IF NONE OF THESE, RETURN TO CALLER.  IF BACK-ARROW,
;* OUTPUT A 0DH WITHOUT A LF.  IF SINGLE OR DOUBLE QUOTE, PRINT
;* THE STRING IN THE QUOTE AND DEMANDS A MATCHING UNQUOTE. 
;* AFTER THE PRINTING THE NEXT 3 BYTES OF THE CALLER IS SKIPPED
;* OVER (USUALLY A JUMP INSTRUCTION).
;* 
;* 'PRTNUM' PRINTS THE NUMBER IN HL.  LEADING BLANKS ARE ADDED 
;* IF NEEDED TO PAD THE NUMBER OF SPACES TO THE NUMBER IN C.
;* HOWEVER, IF THE NUMBER OF DIGITS IS LARGER THAN THE # IN
;* C, ALL DIGITS ARE PRINTED ANYWAY.  NEGATIVE SIGN IS ALSO
;* PRINTED AND COUNTED IN, POSITIVE SIGN IS NOT. 
;* 
;* 'PRTLN' PRINSrA SAVED TEXT LINE WITH LINE # AND ALL. 
;* 
PRTSTG:	LD	B,A		; *** PRTSTG ***
PS1:    LD      A,(DE)          ; GET A CHARACTER
	INC	DE		; BUMP POINTER
	CP	B		; SAME AS OLD A?
	RET	Z		; YES, RETURN 
        CALL    CHROUT          ; ELSE PRINT IT
        CP      CR              ; WAS IT A CR?
	JP	NZ,PS1		; NO, NEXT
	RET			; YES, RETURN 
;**************************************************************
PRTSTR: LD      A,(DE)          ;DE=STRING, 0 = TERM
        CP      0               ;ROUTINE ADDED
        RET     Z
        INC     DE
        CALL    CHROUT
        JR      PRTSTR
;*
QTSTG:  RST     0x08	;8*1             ;TSTCHR; *** QTSTG ***
	.BYTE	'"

;0FH        .BYTE    17Q
        .BYTE    QT3-1-.

        LD      A,0x22;42Q           ; IT IS A "
QT1:	CALL	PRTSTG		; PRINT UNTIL ANOTHER 
        CP      CR              ; WAS LAST ONE A CR?
	POP	HL		; RETURN ADDRESS
	JP	Z,RUNNXL	; WAS CR, RUN NEXT LINE 
QT2:	INC	HL		; SKIP 3 BYTES ON RETURN
	INC	HL
	INC	HL
	JP	(HL)		; RETURN
QT3:    RST     0x08	;8*1             ;TSTCHR; IS IT A ' ?
        .BYTE    0x27;47Q

;        .BYTE    5Q
        .BYTE    QT4-1-.

        LD      A,0x27;47Q           ; YES, DO SAME
	JP	QT1		; AS IN " 
QT4:    RST     0x08	;8*1             ;TSTCHR; IS IT BACK-ARROW?
        .BYTE    0x5F;137Q            ; 5FH

;08H        .BYTE    10Q
        .BYTE    QT5-1-.

        LD      A,0x0D;;;215Q          ;8DH  YES, 0DHWITHOUT LF!!
        CALL    CHROUT          ; DO IT TWICE
;;;        CALL    CHROUT          ; TTY ENOUGH TIME
	POP	HL		; RETURN ADDRESS
	JP	QT2
QT5:	RET			; NONE OF ABOVE 
;**************************************************************
;PRTNUM HL AS DECIMAL WITH SIGN
PRTNUM: PUSH    DE              ; *** PRTNUM ***
        LD      DE,0x0A	;12Q          ; DECIMAL
	PUSH	DE		; SAVE AS A FLAG
	LD	B,D		; B=SIGN
	DEC	C		; C=SPACES
	CALL	CHKSGN		; CHECK SIGN
	JP	P,PN1		; NO SIGN 
        LD      B,0x2D	;55Q           ; B=SIGN
	DEC	C		; '-' TAKES SPACE 
PN1:	PUSH	BC		; SAVE SIGN & SPACE 
PN2:	CALL	DIVIDE		; DEVIDE HL BY 10 
	LD	A,B		; RESULT 0? 
	OR	C
	JP	Z,PN3		; YES, WE GOT ALL 
	EX	(SP),HL		; NO, SAVE REMAINDER
	DEC	L		; AND COUNT SPACE 
	PUSH	HL		; HL IS OLD BC
	LD	H,B		; MOVE RESULT TO BC 
	LD	L,C
	JP	PN2		; AND DIVIDE BY 10
PN3:	POP	BC		; WE GOT ALL DIGITS IN
PN4:	DEC	C		; THE STACK 
	LD	A,C		; LOOK AT SPACE COUNT 
	OR	A
	JP	M,PN5		; NO LEADING BLANKS 
;ZZZ
;REDUCES BLANKS BETWEEN VALUES
        LD      A,(SEMICO)
        OR      A
        JR      NZ,PN5

        LD      A,0x20           ; LEADING BLANKS ON VALS
        CALL    CHROUT          ;
        JP      PN4             ; MORE?
PN5:
;;ZZZ
        LD      A,1
        LD      (SEMICO),A
        LD      A,B             ; PRINT SIGN
;;ZZZ
        OR      A               ;DEL LEADING BLANK ON LINE #
        JR      Z,PN5A          ;

;PRINTS LEADING '-' OR SPACE ON LINE # AND VALUES
        CALL    CHROUT          ; MAYBE - OR NULL
PN5A:   LD      E,L             ; LAST REMAINDER IN E
PN6:	LD	A,E		; CHECK DIGIT IN E
        CP      0x0A             ; 10 IS FLAG FOR NO MORE
	POP	DE
        RET     Z               ; IF SO, RETURN
        ADD     A,0x30           ; ELSE CONVERT TO ASCII
        CALL    CHROUT          ; AND PRINT THE DIGIT. LINE # AND VALS
	JP	PN6		; GO BACK FOR MORE

;*END PRTNUM HL AS DECIMAL WITH SIGN
;**************************************************************
;PRINT DE AS LINE NUMBER
PRTLN:  LD      A,(DE)          ; *** PRTLN ***
	LD	L,A		; LOW ORDER LINE #
	INC	DE
	LD	A,(DE)		; HIGH ORDER
	LD	H,A
	INC	DE

;ZZZ
;DELETES LEADING BLANKS ON LINE NUMBER.
;        LD      C,4Q            ; PRINT 4 DIGIT LINE #
        LD      C,0             ;

        CALL    PRTNUM
        LD      A,#0x20           ; FOLLOWED BY A BLANK
        CALL    CHROUT          ; THIS IS SPACE AFTER LINE NUMBER IN LIST
        SUB     A               ; AND THEN THE TEXT
	CALL	PRTSTG
	RET
;**************************************************************
;* 
;* *** MVUP *** MVDOWN *** POPA *** & PUSHA ***
;* 
;* 'MVUP' MOVES A BLOCK UP FROM HERE DE-> TO WHERE BC-> UNTIL 
;* DE = HL 
;* 
;* 'MVDOWN' MOVES A BLOCK DOWN FROM WHERE DE-> TO WHERE HL-> 
;* UNTIL DE = BC 
;* 
;* 'POPA' RESTORES THE 'FOR' LOOP VARIABLE SAVE AREA FROM THE
;* STACK 
;* 
;* 'PUSHA' STACKS THE 'FOR' LOOP VARIABLE SAVE AREA INTO THE 
;* STACK 
;* 
MVUP:   RST     0x20	;8*4             ;COMPAR          ; *** MVUP ***
	RET	Z		; DE = HL, RETURN 
	LD	A,(DE)		; GET ONE BYTE
	LD	(BC),A		; MOVE IT 
	INC	DE		; INCREASE BOTH POINTERS
	INC	BC
	JP	MVUP		; UNTIL DONE
;* 
MVDOWN:	LD	A,B		; *** MVDOWN ***
        SUB     D               ; TEST IF DE = BC
	JP	NZ,MD1		; NO, GO MOVE 
	LD	A,C		; MAYBE, OTHER BYTE?
	SUB	E
	RET	Z		; YES, RETURN 
MD1:	DEC	DE		; ELSE MOVE A BYTE
	DEC	HL		; BUT FIRST DECREASE
	LD	A,(DE)		; BOTH POINTERS AND 
	LD	(HL),A		; THEN DO IT
	JP	MVDOWN		; LOOP BACK 
;* 
POPA:	POP	BC		; BC = RETURN ADDR. 
	POP	HL		; RESTORE LOPVAR, BUT 
	LD	(LOPVAR),HL	; =0 MEANS NO MORE
	LD	A,H
	OR	L
	JP	Z,PP1		; YEP, GO RETURN
	POP	HL		; NOP, RESTORE OTHERS 
	LD	(LOPINC),HL
	POP	HL
	LD	(LOPLMT),HL
	POP	HL
	LD	(LOPLN),HL
	POP	HL
	LD	(LOPPT),HL
PP1:	PUSH	BC		; BC = RETURN ADDR. 
	RET
;**************************************************************
PUSHA:  LD      HL,STKLMT       ; *** PUSHA ***
	CALL	CHGSGN
	POP	BC		; BC=RETURN ADDRESS 
	ADD	HL,SP		; IS STACK NEAR THE TOP?
	JP	NC,QSORRY	; YES, SORRY FOR THAT.
	LD	HL,(LOPVAR)	; ELSE SAVE LOOP VAR.S
        LD      A,H             ; BUT IF LOPVAR IS 0
	OR	L		; THAT WILL BE ALL
	JP	Z,PU1
	LD	HL,(LOPPT)	; ELSE, MORE TO SAVE
	PUSH	HL
	LD	HL,(LOPLN)
	PUSH	HL
	LD	HL,(LOPLMT)
	PUSH	HL
	LD	HL,(LOPINC)
	PUSH	HL
	LD	HL,(LOPVAR)
PU1:	PUSH	HL
	PUSH	BC		; BC = RETURN ADDR. 
	RET
;**************************************************************
;FINISH: POP     AF              ; *** FINISH/RST 6 ***
;        CALL    FIN             ; CHECK END OF COMMAND
;        JP      QWHAT           ; PRINT "WHAT?" IF WRONG

;IGNBLK: LD      A,(DE)          ; *** IGNBLK/RST 5 ***
;        CP      ' '             ; IGNORE BLANKS
;        RET     NZ              ; IN TEXT (WHERE DE->)
;        INC     DE              ; AND RETURN THE FIRST
;        JP      IGNBLK          ; NON-BLANK CHAR. IN A

;COMPAR: LD      A,H             ; *** COMP OR OLD RST 4 ***
;        CP      D               ; COMPARE HL WITH DE
;        RET     NZ              ; RETURN CORRECT C AND
;        LD      A,L             ; Z FLAGS
;        CP      E               ; BUT OLD A IS LOST
;        RET
;**************************************************************
;EXP:    CALL    EXPR2           ; *** EXP OR OLD RST 3 ***
;        PUSH    HL              ; EVALUATE AN EXPRESION
;        JP      EXPR1           ; REST OF IT IS AT EXPR1
;**************************************************************
CRLF:   LD      A,CR            ; O/P CRLF ROUTINE
	CALL	CHROUT
	LD	A,LF
	JP	CHROUT

;CHROUT: PUSH    AF              ; *** OLD RST 2 ***
        LD      A,(VIDEOF)      ; PRINT CHARACTER ONLY
        OR      A               ; IF VIDEOF SWITCH IS ON
        JR      NZ,VIDON        ; IT IS ON
        POP     AF              ; IT IS OFF
        RET                     ; RESTORE AF AND RETURN
;**************************************************************
VIDON:  POP     AF              ; GET OLD A BACK
        PUSH    BC              ; SAVE B ON STACK
        PUSH    DE              ; AND D
        PUSH    HL              ; AND H TOO
        LD      (OUTCAR),A      ; SAVE CHARACTER
        CALL    OUTPUTM         ;SEND TO PC
        LD      A,(OUTCAR)      ; GET CHAR. BACK
        CP      CR              ; WAS IT A 'CR'?
        JP      NZ,DONE         ; NO, DONE
        LD      A,LF            ; GET LINEFEED
        CALL    OUTPUTM         ;SEND TO PC
DONE:   LD      A,(OUTCAR)      ; GET CHARACTER BACK
IDONE:  POP     HL              ; GET H BACK
        POP     DE              ; AND D
        POP     BC              ; AND B TOO
        RET                     ; DONE AT LAST

	.ifdef	sim
;------------------------------------------------------------------------------
; RXA - Receive a byte over SIO/0 Ch A
;------------------------------------------------------------------------------
RXA:
	CALL	CKSIOA		; Get the status word
	JR		NC,RXA		; Loop until a character arrives
	ld		a,0
	OUT		(0),A		; zero out the flag
	IN		A,(1)		; Get the character
	RET					; Char ready in A
;------------------------------------------------------------------------------
; TXA - Transmit a byte over SIO/0 Ch A
;------------------------------------------------------------------------------
CHROUT:
TXA:
	PUSH	AF		; Store character
	CALL	CKSIOA		; See if SIO channel A is finished transmitting
	JR	Z,TXA+1		; Loop until SIO flag signals ready
	POP	AF		; Retrieve character
	OUT	(1),A		; Output the character
	RET

;------------------------------------------------------------------------------
; Check SIO Channel A status flag, RX char ready=CY, TX buffer clear=NZ
;------------------------------------------------------------------------------
CKSIOA:
	IN		a,(0)	;Retrieve Status Word
	or		#0x80	; set Z=0 tx buffer clear
	RRCA			;RX status into CY flag
	LD		a,0
	RET
	.else
;------------------------------------------------------------------------------
; RXA - Receive a byte over SIO/0 Ch A
;------------------------------------------------------------------------------
RXA:
	CALL	CKSIOA		; Get the status word
	JR	NC,RXA		; Loop until a character arrives
	IN	A,(RBR)		; Get the character
	RET			; Char ready in A
;------------------------------------------------------------------------------
; TXA - Transmit a byte over SIO/0 Ch A
;------------------------------------------------------------------------------
CHROUT:
TXA:
	PUSH	AF		; Store character
	CALL	CKSIOA		; See if SIO channel A is finished transmitting
	JR	Z,TXA+1		; Loop until SIO flag signals ready
	POP	AF		; Retrieve character
	OUT	(THR),A		; Output the character
	RET

;------------------------------------------------------------------------------
; Check SIO Channel A status flag, RX char ready=CY, TX buffer clear=NZ
;------------------------------------------------------------------------------
CKSIOA:
	IN	a,(LSR)		;Retrieve Status Word
	RRCA			;RX status into CY flag
	BIT	4,A		;TX Buffer Empty into Z flag
	RET
	.endif

UART1_BASE	=	0x00
RBR	=	UART1_BASE + 0x00
THR	=	UART1_BASE + 0x00
IER	=	UART1_BASE + 0x01
FCR	=	UART1_BASE + 0x02
LCR	=	UART1_BASE + 0x03
MCR	=	UART1_BASE + 0x04
LSR	=	UART1_BASE + 0x05
MSR	=	UART1_BASE + 0x06
DLL	=	UART1_BASE + 0x00
DLM	=	UART1_BASE + 0x01

INIT16550:
	LD	A,0x80
	OUT	(LCR),A		; set baud divisor latch
	LD	A,0x00
	OUT	(DLM),A		; set baud dividor
	LD	A,0x51
	OUT	(DLL),A		; set baud dividor
	LD	A,0x03
	OUT	(LCR),A		; set divisor latch to 1 and 8 bit word length
	LD	A,0x00
	OUT	(MCR),A		; set baud dividor

	LD	A,0x30
	OUT	(THR),A		; set baud dividor
	ret
	
;**************************************************************
;* 
;* *** OUTC *** & CHKIO ***
;!
;* THESE ARE THE ONLY I/O ROUTINES IN TBI. 
;* 'OUTC' IS CONTROLLED BY A SOFTWARE SWITCH 'VIDEOF'. IF VIDEOF=0
;* 'OUTC' WILL JUST RETURN TO THE CALLER.  IF VIDEOF IS NOT 0,
;* IT WILL OUTPUT THE BYTE IN A.  IF THAT IS A CR, A LF IS ALSO
;* SEND OUT.  ONLY THE FLAGS MAY BE CHANGED AT RETURN, ALL REG.
;* ARE RESTORED. 
;* 
;* 'CHKIO' CHECKS THE INPUT.  IF NO INPUT, IT WILL RETURN TO
;* THE CALLER WITH THE Z FLAG SET.  IF THERE IS INPUT, Z FLAG
;* IS CLEARED AND THE INPUT BYTE IS IN A.  HOWEVER, IF THE
;* INPUT IS A CONTROL-O, THE 'VIDEOF' SWITCH IS COMPLIMENTED, AND
;* Z FLAG IS RETURNED.  IF A CONTROL-C IS READ, 'CHKIO' WILL
;* RESTART TBI AND NOT RETURN TO THE CALLER.
;* 
;**************************************************************


CHKIO:  
	CALL	CKSIOA			; check for character on serial port, return in A
	JP		NC,CHKIOXIT		; RXRDY is in the carry bit.  if no carry, exit
	CALL	RXA				; if character received, read it
	cp		3				; compare with control-c
	jp		nz,CHKIO1		; if not control-c, return the character
	JP		RSTART			; if it is control-c, restart
CHKIO1:
	CALL	CHROUT			; echo the character back
	SCF
CHKIOXIT:
	RET

;TODO: remove old code
	PUSH    BC              ; SAVE B ON STACK
        PUSH    DE              ; AND D
        PUSH    HL              ; THEN H
;        CALL    TSTKEY          ;TEST FOR KEY
	CALL	CKSIOA
;        OR      A               ; SET FLAGS
        JP      Z,GETIT        ; IF READY GET CHARACTER
        JP      IDONE           ; RESTORE AND RETURN
GETIT:  CALL    RXA          ;GET THE KEYBOARD DATA
        CP      3               ;IS IT ^C?
        JP      NZ,IDONE        ;RETURN AND RESTORE IF NOT
        JP      RSTART          ;YES, RESTART TBI
		
;TODO: end remove old code

;**************************************************************
LEDOFF: LD      BC,PITBASE+3
        LD      A,0x91
        OUT     (C),A
        LD      A,0x0A
        OUT     (C),A
        RET
;**************************************************************
LEDON:  LD      BC,PITBASE+3
        LD      A,0x91
        OUT     (C),A
        LD      A,0x0B
        OUT     (C),A
        RET
;**************************************************************
LSTROM  =     .

;END OF Z8TBASIC
;**************************************************************
ROMBAS  =     0x1000
;
;SAMPLE PROGRAM IN ROM
;
		.ORG     ROMBAS+OFFSET
;
		.WORD	10                     ;LINE NO# 10
		.ASCII	"A=INP(2):?A"          ;A=INP(2):?A
		.WORD	20                     ;LINE NO# 20
		.ASCII    "GOTO 10\r"              ;GOTO 10
		.BYTE    00,00                  ;ZERO BYTES TERM.
;
;**************************************************************
;TXTBGN  =     0x8000           ; TEXT SAVE AREA BEGINS
;TXTEND  =     0x9000           ; TEXT SAVE AREA ENDS
;RAM     =     0x8000
;RAMBAS  =     RAM
TXTBGN  =     0x3000           ; TEXT SAVE AREA BEGINS
TXTEND  =     0x3E00           ; TEXT SAVE AREA ENDS
RAMBAS  =     0x3000
;**************************************************************
FCB     =     0x3E01

VARBGN  =     0x3EAF           ;.BLOCK    2*27           ; VARIABLE @(0)
                                ;.BLOCK    1              ; EXTRA BYTE FOR BUFFER
BUFFER  =     2*27+1+VARBGN   ;.BLOCK    80             ; INPUT BUFFER
BUFEND  =     BUFFER+80       ;                       ; BUFFER ENDS
                                ;.BLOCK    40             ; EXTRA BYTES FOR STACK
OUTCAR  =     BUFEND+40       ;.BYTE    0              ; OUTPUT CHAR. STORAGE
CURRNT  =     OUTCAR+1        ;DEFW    0              ; POINTS TO CURRENT LINE
STKGOS  =     CURRNT+2        ;DEFW    0              ; SAVES SP IN 'GOSUB'
VARNXT  =     STKGOS+2        ;DEFW    0              ; TEMPORARY STORAGE
STKINP  =     VARNXT+2        ;DEFW    0              ; SAVES SP IN 'INPUT'
LOPVAR  =     STKINP+2        ;DEFW    0              ; 'FOR' LOOP SAVE AREA
LOPINC  =     LOPVAR+2        ;DEFW    0              ; INCREMENT
LOPLMT  =     LOPINC+2        ;DEFW    0              ; LIMIT
LOPLN   =     LOPLMT+2        ;DEFW    0              ; LINE NUMBER
LOPPT   =     LOPLN+2         ;DEFW    0              ; TEXT POINTER
RANPNT  =     LOPPT+2         ;DEFW    START          ; RANDOM NUMBER POINTER
TXTUNF  =     RANPNT+2

OUTPRT  =     TXTUNF+2        ;WHICH PORT FOR BASIC OUT
INPRT   =     OUTPRT+1        ;WHICH PORT FOR BASIC IN
WTPRT   =     INPRT+1         ;WHICH PORT FOR BASIC WAIT
SEMICO  =     WTPRT+1         ;PRINT SPECIFIER. ; OR ,

DISKFL  =     SEMICO+1        ;DISK I/O OFF/ON (LINE 1)
KBDFLG  =     DISKFL+1        ;KEYBOARD OFF/ON (LINE 1)
VIDEOF  =     KBDFLG+1        ;VIDEO OFF/ON    (LINE 1)

LPFLAG  =     VIDEOF+1        ;LINE PRINT?
;KBDFGT  =     DISKGT+1        ;KEYBOARD (LINE 1 ANDED WITH ROM BYTE)
;VIDEGT  =     KBDFGT+1        ;VIDEO    (LINE 1 ANDED WITH ROM BYTE)

LED1    =     LPFLAG+1
LED2    =     LED1+1
LAST    =     LED2+1
FRONT  =     LAST+1

;STKLMT  =     RAM+1F00H       ;TOP LIMIT FOR STACK
;STACK   =     RAM+1FFFH       ;STACK STARTS HERE

STKLMT  =     0x9F80           ;TOP LIMIT FOR STACK
;STACK   =     9FF0H           ;STACK STARTS HERE
STACK	=	0xFFD0
        .ORG     0x0FFF+OFFSET
        .BYTE    0x0FF

        .END

