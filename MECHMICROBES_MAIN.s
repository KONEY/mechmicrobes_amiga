;*** WWW.KONEY.ORG *******
;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/mechmicrobes_amiga/"
	SECTION	"Code",CODE
	INCLUDE	"PhotonsMiniWrapper1.04!.S"
	INCLUDE	"custom-registers.i"
	INCLUDE	"PT12_OPTIONS.i"
	INCLUDE	"P6112-Play-stripped.i"
;********** Constants **********
w=368		;screen width, height, depth
h=230
bpls=5		;handy values:
bpl=w/16*2	;byte-width of 1 bitplane line (46)
bwid=bpls*bpl	;byte-width of 1 pixel line (all bpls)
TrigShift=7
PXLSIDE=16
Z_Shift=PXLSIDE*5/2	; 5x5 obj
LOGOSIDE=16*7
LOGOBPL=LOGOSIDE/16*2
MARGINX=(w/2)
MARGINY=(LOGOSIDE/2)
TXT_FRMSKIP=3
;*************
MODSTART_POS=0		; start music at position # !! MUST BE EVEN FOR 16BIT
;*************

VarTimesTrig MACRO ;3 = 1 * 2, where 2 is cos(Angle)^(TrigShift*2) or sin(Angle)^(TrigShift*2)
	move.l \1,\3
	muls \2,\3

	asr.l #TrigShift,\3 ;left >>= TrigShift
	asr.l #TrigShift,\3
	ENDM

;********** Demo **********	; Demo-specific non-startup code below.
Demo:				; a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	MOVE.L	#VBint,$6C(A4)
	MOVE.W	#%1110000000100000,INTENA
	;** SOMETHING INSIDE HERE IS NEEDED TO MAKE MOD PLAY! **
	;move.w	#%1110000000000000,INTENA	; Master and lev6	; NO COPPER-IRQ!
	MOVE.W	#%1000011111100000,DMACON
	;*--- clear screens ---*
	LEA	SCREEN1,A1
	;BSR.W	ClearScreen
	LEA	SCREEN2,A1
	;BSR.W	ClearScreen
	BSR	WaitBlitter
	;*--- start copper ---*
	LEA	SCREEN1,A0
	MOVEQ	#bpl,D0
	LEA	COPPER\.BplPtrs+2,A1
	MOVEQ	#bpls-1,D1
	BSR.W	PokePtrs

	; #### Point LOGO sprites
	POINT_SPRITES:
	LEA	COPPER\.SpritePointers,A1	; Puntatori in copperlist
	MOVE.L	#0,D0		; sprite 0
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0	; sprite 1
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0	; sprite 2
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0	; sprite 3
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0	; sprite 4
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0	; sprite 5
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#LED_ON,D0	; sprite 6
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#LED_OFF,D0	; sprite 7
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	; ** POINTS TO COORDS **
	MOVEQ	#64-1,D1
	LEA	KONEY_OPT,A2
	.calcuCoords:
	MOVE.W	(A2),D0
	MULU.W	#PXLSIDE,D0
	MULU.W	#Z_Shift,D0	; PRECALCULATED ZSHIFT
	MOVE.W	D0,(A2)+
	DBRA	D1,.calcuCoords
	; ** POINTS TO COORDS **
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	;---  Call P61_Init  ---
	MOVEM.L	D0-A6,-(SP)
	LEA	MODULE,A0
	SUB.L	A1,A1
	SUB.L	A2,A2
	MOVE.W	#MODSTART_POS,P61_InitPos	; TRACK START OFFSET
	JSR	P61_Init
	MOVEM.L (SP)+,D0-A6

	MOVE.L	#COPPER,COP1LC	; ATTACH THE COPPER
;********************  main loop  ********************
MainLoop:
	move.w	#$12c,d0		;No buffering, so wait until raster
	bsr.w	WaitRaster	;is below the Display Window.
	;*--- swap buffers ---*
	movem.l	DrawBuffer(PC),a2-a3
	exg	a2,a3
	movem.l	a2-a3,DrawBuffer	;draw into a2, show a3
	;*--- show one... ---*
	move.l	a3,a0
	move.l	#bpl*h,d0
	lea	COPPER\.BplPtrs+2,a1
	moveq	#bpls-1,d1
	bsr.w	PokePtrs
	;*--- ...draw into the other(a2) ---*
	move.l	a2,a1

	BSR.W	ClearBlitterBuffer
	BSR.W	__SET_PT_VISUALS
	MOVE.L	#TR909,DrawBuffer
	; do stuff here :)

	BSR.W	__POPULATETXTBUFFER
	BSR.W	__SHIFTTEXT

	MOVE.W	AUDIOCHLEVEL2,Z_POS

	; ## SONG POS RESETS ##
	MOVE.W	P61_Pos,D6
	MOVE.W	P61_DUMMY_POS,D5
	CMP.W	D5,D6
	BEQ.S	.dontReset
	ADDQ.W	#1,P61_DUMMY_POS
	ADDQ.W	#1,P61_LAST_POS
	.dontReset:
	; ## SONG POS RESETS ##

	;CLR.W	$100		; DEBUG | w 0 100 2
	.block0:			; BEWARE THE SMC !!
	TST.W	P61_LAST_POS
	BEQ.B	.skipSequencer
	MOVE.W	#$F00,$DFF180	; show rastertime left down to $12c
	; mock a BRA.S .skipCMP
	MOVE.W	#$6000|(.call-(.block0+2)),.block0
	.call:	
	BSR.W	__SET_SEQUENCER_LEDS
	.skipSequencer:

	; **** JOYSTICK TEST ****
	MOVEM.W	$DFF00C,D0	; FROM EAB
	ANDI.W	#$0303,D0
	MOVE.W	D0,D1
	ADD.W	D1,D1
	ADDI.W	#$0101,D0
	ADD.W	D1,D0
	BTST	#9,D0		; 9 LEFT
	BEQ.S	.notLeft
	SUBI.W	#2,ANGLE
	.notLeft:
	BTST	#1,D0		; 1 RIGHT
	BEQ.S	.notRight
	ADDI.W	#2,ANGLE
	.notRight:
	BTST	#10,D0		; 10 UP
	BEQ.S	.notDown
	SUBI.W	#2,Z_POS
	.notDown:
	BTST	#2,D0		; 2 DOWN
	BEQ.S	.notUp
	ADDI.W	#2,Z_POS
	.notUp:
	; **** JOYSTICK TEST ****

	; **** ROTATION VALUES ****
	MOVE.W	ANGLE,D2
	CMPI.W	#0,D2
	BGE.S	.dontResetL
	MOVE.W	#358,D2
	.dontResetL:
	CMPI.W	#360,D2
	BLO.S	.dontResetR
	MOVEQ.L	#0,D2
	.dontResetR:
	MOVE.W	D2,ANGLE

	; **** ZOOM VALUES ****
	MOVE.W	Z_POS,D4
	MOVE.W	#Z_Shift,D7	; CENTER
	ADD.W	D7,D4
	MOVE.W	D4,Z_FACT
	MOVE.W	D7,D2		; OPTIMIZING mulu.w #40,d1
	LSL.W	#5,D7
	LSL.W	#3,D2
	ADD.W	D2,D7
	;MULU.W	D7,D7
	DIVU.W	D4,D7
	MOVE.W	D7,CENTER
	; **** END VALUES ****

	BSR.W	InitLine		; inizializza line-mode
	MOVE.W	#$FFFF,BLTBDAT	; BLTBDAT = pattern della linea!
	;MOVE.B	$DFF006,BLTBDAT

	MOVEQ	#16-1,D7
	MOVE.W	#0,XY_INIT
	LEA	KONEY_OPT,A2

	.fetchCoordz:
	MOVEM.L	D7,-(SP)

	MOVEQ.L	#0,D0		; QUICKEST RESET
	MOVE.L	D0,D1		; QUICKEST RESET
	MOVE.W	(A2)+,D0		; X1
	MOVE.W	(A2)+,D1		; Y1
	; **** ROTATING??? ****
	MOVE.W	ANGLE,D7
	LEA.L	SinTbl(pc),A0
	MOVE.W	(A0,D7),D3
	LEA.L	CosTbl(pc),A0
	MOVE.W	(A0,D7),D4

	; NO NEED TO OPT BECAUSE FIRST AND LAST = 0,0!!
	; **** OPTIMIZATION!! ****
	MOVEM.W	XY_INIT,D7
	CMPI.W	#0,D7
	BNE.S	.notFirstLoop
	MOVE.W	#1,XY_INIT
	; **** Z-POSITION ****
	DIVU.W	Z_FACT,D0
	DIVU.W	Z_FACT,D1
	SUB.W	CENTER,D0
	SUB.W	CENTER,D1
	MOVE.W	D0,X_TEMP
	MOVE.W	D1,Y_TEMP
	.notFirstLoop:
	; **** OPTIMIZATION!! ****

	MOVE.W	X_TEMP,D0
	MOVE.W	Y_TEMP,D1

	BSR.W	__ROTATE

	MOVEM.L	D0-D1,-(SP)
	MOVEQ.L	#0,D0		; QUICKEST RESET
	MOVE.L	D0,D1		; QUICKEST RESET
	MOVE.W	(A2)+,D0		; X2
	MOVE.W	(A2)+,D1		; Y2
	; **** Z-POSITION ****
	;MULU.W	#Z_Shift,D0	; PRECALCULATED
	;MULU.W	#Z_Shift,D1	; PRECALCULATED
	DIVU.W	Z_FACT,D0
	DIVU.W	Z_FACT,D1
	SUB.W	CENTER,D0
	SUB.W	CENTER,D1	
	MOVE.W	D0,X_TEMP
	MOVE.W	D1,Y_TEMP

	BSR.W	__ROTATE

	MOVE.W	D0,D2		; X2
	MOVE.W	D1,D3		; Y2

	MOVEM.L	(SP)+,D0-D1

	BSR.W	Drawline

	MOVEM.L	(SP)+,D7
	DBRA	D7,.fetchCoordz

	BSR.W	__BLIT_3D_IN_PLACE

	;ADDI.W	#2,ANGLE		; JUST ROTATE :)
	;*--- main loop end ---*

	ENDING_CODE:
	BTST	#6,$BFE001
	BNE.S	.DontShowRasterTime
	MOVE.W	#$0FF,$DFF196	; show rastertime left down to $12c
	.DontShowRasterTime:
	BTST	#2,$DFF016	; POTINP - RMB pressed?
	BNE.W	MainLoop		; then loop
	;*--- exit ---*
	;    ---  Call P61_End  ---
	MOVEM.L D0-A6,-(SP)
	JSR P61_End
	MOVEM.L (SP)+,D0-A6
	RTS

;********** Demo Routines **********
PokePtrs:				; Generic, poke ptrs into copper list
	.bpll:	
	move.l	a0,d2
	swap	d2
	move.w	d2,(a1)		;high word of address
	move.w	a0,4(a1)		;low word of address
	addq.w	#8,a1		;skip two copper instructions
	add.l	d0,a0		;next ptr
	dbf	d1,.bpll
	rts
ClearScreen:			; a1=screen destination address to clear
	bsr	WaitBlitter
	clr.w	BLTDMOD			; destination modulo
	move.l	#$01000000,BLTCON0		; set operation type in BLTCON0/1
	move.l	a1,BLTDPTH		; destination address
	move.l	#h*bpls*64+bpl/2,BLTSIZE	;blitter operation size
	rts
VBint:				; Blank template VERTB interrupt
	btst	#5,$DFF01F	; check if it's our vertb int.
	beq.s	.notvb
	move.w	#$20,$DFF09C	; poll irq bit
	move.w	#$20,$DFF09C	; KONEY REFACTOR
	.notvb:	
	rte

ClearBlitterBuffer:
	BSR	WaitBlitter
	MOVE.W	#$09F0,BLTCON0		; A**,Shift 0, A -> D
	MOVE.W	#0,BLTCON1		; Everything Normal
	MOVE.L	#0,BLTAMOD		; Init modulo Sou. A
	MOVE.W	#0,BLTDMOD		; Init modulo Dest D
	MOVE.L	#EMPTY,BLTAPTH		; Source
	MOVE.L	#BUFFER3D,BLTDPTH		; Dest
	MOVE.W	#w*64+LOGOSIDE/16,BLTSIZE	; Start Blitter (Blitsize)
	RTS
Drawline:
	LEA	BUFFER3D,A0
	ADDI.W	#MARGINX,D0
	ADD.W	TOP_MARGIN,D1
	ADDI.W	#MARGINX,D2
	ADD.W	TOP_MARGIN,D3

	sub.w	d1,d3	; D3=Y2-Y1
	beq.s	.skip	; per il fill non servono linee orizzontali 
	bgt.s	.y2gy1	; salta se positivo..
	exg	d0,d2	; ..altrimenti scambia i punti
	add.w	d3,d1	; mette in D1 la Y piu` piccola
	neg.w	d3	; D3=DY
	.y2gy1:
	mulu.w	#bpl,d1		; offset Y
	add.l	d1,a0
	moveq	#0,d1		; D1 indice nella tabella ottanti
	sub.w	d0,d2		; D2=X2-X1
	bge.s	.xdpos		; salta se positivo..
	addq.w	#2,d1		; ..altrimenti sposta l'indice
	neg.w	d2		; e rendi positiva la differenza
	.xdpos:
	moveq	#$f,d4		; maschera per i 4 bit bassi
	and.w	d0,d4		; selezionali in D4
	
				; solo se DL_Fill=1
	move.b	d4,d5		; calcola numero del bit da invertire
	not.b	d5		; (la BCHG numera i bit in modo inverso	

	lsr.w	#3,d0		; offset X:
				; Allinea a byte (serve per BCHG)
	add.w	d0,a0		; aggiunge all'indirizzo
				; nota che anche se l'indirizzo
				; e` dispari non fa nulla perche`
				; il blitter non tiene conto del
				; bit meno significativo di BLTxPT

	ror.w	#4,d4		; D4 = valore di shift A
	ori.w	#$0B4A,d4	; aggiunge l'opportuno
				; Minterm (OR o EOR)
	swap	d4		; valore di BLTCON0 nella word alta
		
	cmp.w	d2,d3		; confronta DiffX e DiffY
	bge.s	.dygdx		; salta se >=0..
	addq.w	#1,d1		; altrimenti setta il bit 0 del'indice
	exg	d2,d3		; e scambia le Diff
	.dygdx:
	add.w	d2,d2		; D2 = 2*DiffX
	move.w	d2,d0		; copia in D0
	sub.w	d3,d0		; D0 = 2*DiffX-DiffY
	addx.w	d1,d1		; moltiplica per 2 l'indice e
				; contemporaneamente aggiunge il flag
				; X che vale 1 se 2*DiffX-DiffY<0
				; (settato dalla sub.w)
	move.b	OKTS(PC,d1.w),d4	; legge l'ottante
	swap	d2		; valore BLTBMOD in word alta
	move.w	d0,d2		; word bassa D2=2*DiffX-DiffY
	sub.w	d3,d2		; word bassa D2=2*DiffX-2*DiffY
	moveq	#6,d1		; valore di shift e di test per
				; la wait blitter 

	lsl.w	d1,d3		; calcola il valore di BLTSIZE
	add.w	#$42,d3

	BSR	WaitBlitter

	bchg	d5,(a0)		; Inverte il primo bit della linea

	move.l	d4,BLTCON0	; BLTCON0/1
	move.l	d2,BLTBMOD	; BLTBMOD e BLTAMOD
	move.l	a0,BLTCPTH	; BLTCPT
	move.w	d0,BLTAPTL	; BLTAPTL
	move.l	a0,BLTDPTH	; BLTDPT - indirizzo schermo
	move.w	d3,BLTSIZE	; BLTSIZE
	.skip:
	rts

	OKTS:
	DC.B 3,3+$40
	DC.B 19,19+$40
	DC.B 11,11+$40
	DC.B 23,23+$40
InitLine:
	BSR	WaitBlitter
	moveq.l	#-1,d5
	move.l	d5,BLTAFWM	; BLTAFWM/BLTALWM = $FFFF
	move.w	#$8000,BLTADAT	; BLTADAT = $8000
	move.w	#bpl,BLTCMOD	; BLTCMOD = 40
	move.w	#bpl,BLTDMOD	; BLTDMOD = 40
	rts

__ROTATE:
	; Rotate around Z Axis:
	VarTimesTrig d0,d4,d5	;left = rotatedX * cos
	VarTimesTrig d1,d3,d6	;right = rotatedY * sin
	move.l	d5,d7		;tmp = left - right
	sub.l	d6,d7
	VarTimesTrig d0,d3,d5	;left = rotatedX * sin
	VarTimesTrig d1,d4,d6	;right = rotatedY * cos
	move.l	d5,d1		;rotatedY = left + right
	add.l	d6,d1
	move.l	d7,d0		;rotatedX = tmp
	RTS

__BLIT_3D_IN_PLACE:
	BSR	WaitBlitter
	MOVE.W	#$FFFF,BLTAFWM		; BLTAFWM lo spiegheremo dopo
	MOVE.W	#$FFFF,BLTALWM		; BLTALWM lo spiegheremo dopo
	MOVE.W	#%0000100111110000,BLTCON0	; BLTCON0 (usa A+D)
	;MOVE.W	#%0000000000001010,BLTCON1	; BLTCON1 lo spiegheremo dopo
	MOVE.W	#%0000000000010010,BLTCON1	; BLTCON1 lo spiegheremo dopo
	MOVE.W	#bpl-LOGOBPL,BLTAMOD	; BLTAMOD =0 perche` il rettangolo
	MOVE.W	#bpl-LOGOBPL,BLTDMOD	; Init modulo Dest D
	MOVE.L	#BUFFER3D,A4
	ADD.L	#bpl/2-LOGOBPL/2,A4
	ADD.L	#(LOGOSIDE+16)*40-2,A4	; FOR DESC MODE - WHY THIS VALUES??
	MOVE.L	A4,BLTAPTH		; BLTAPT  (fisso alla figura sorgente)
	MOVE.L	BITPLANE_PTR,A4
	ADD.L	#bpl/2-LOGOBPL/2,A4
	ADD.L	#(LOGOSIDE+16)*40-2,A4	; FOR DESC MODE - WHY THIS VALUES??
	MOVE.L	A4,BLTDPTH
	MOVE.W	#LOGOSIDE*64+LOGOSIDE/16,BLTSIZE	; Start Blitter (Blitsize)
	RTS

__SET_SEQUENCER_LEDS:
	; ## SEQUENCER LEDS ##
	MOVE	P61_SEQ_POS,D0
	MOVE.W	P61_rowpos,D6
	MOVE.W	P61_DUMMY_SEQPOS,D5
	CMP.W	D5,D6
	BEQ.S	.dontResetRowPos
	MOVE.W	D6,P61_DUMMY_SEQPOS
	ADDQ.W	#1,D0
	AND.W	#15,D0
	MOVE.W	D0,P61_SEQ_POS
	.dontResetRowPos:
	LEA	SEQ_POS_ON,A0
	MOVE.B	(A0,D0.W),LED_ON\.HPOS
	LEA	SEQ_POS_OFF,A0
	MOVE.B	(A0,D0.W),LED_OFF\.HPOS
	LEA	SEQ_POS_BIT,A0
	MOVE.B	(A0,D0.W),LED_ON\.CTRL
	MOVE.B	(A0,D0.W),LED_OFF\.CTRL
	; ## SEQUENCER LEDS ##
	RTS

__SET_PT_VISUALS:
	; ## MOD VISUALIZERS ##########
	; ## COMMANDS 80x TRIGGERED EVENTS ##
	;MOVE.W	P61_1F,D2		; 1Fx
	MOVE.W	P61_E8,D2		; 80x
	; ## COMMANDS 80x TRIGGERED EVENTS ##
	; KICK
	LEA	P61_visuctr2(PC),A0 ; which channel? 0-3
	MOVEQ	#15,D0		; maxvalue
	SUB.W	(A0),D0		; -#frames/irqs since instrument trigger
	BPL.S	.ok1		; below minvalue?
	MOVEQ	#0,D0		; then set to minvalue
	.ok1:
	DIVU.W	#2,D0
	MOVE.B	D0,AUDIOCHLEVEL2
	;MOVE.W	D0,LOGOCOL1	; poke WHITE color now
	_ok1:
	RTS
	; MOD VISUALIZERS *****

__POPULATETXTBUFFER:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	MOVE.W	FRAMESINDEX,D7
	CMPI.W	#3,D7
	BNE.W	.SKIP
	MOVE.L	BGPLANE0,A4
	LEA	FONT,A5
	LEA	TEXT,A6
	ADD.W	#bpl*(h-19)+1,A4	; POSITIONING
	ADD.W	TEXTINDEX,A6
	CMP.L	#_TEXT-1,A6	; Siamo arrivati all'ultima word della TAB?
	BNE.S	.PROCEED
	MOVE.W	#0,TEXTINDEX	; Riparti a puntare dalla prima word
	LEA	TEXT,A6		; FIX FOR GLITCH (I KNOW IT'S FUN... :)
	.PROCEED:
	MOVE.B	(A6),D2		; Prossimo carattere in d2
	SUBI.B	#$20,D2		; TOGLI 32 AL VALORE ASCII DEL CARATTERE, IN
	MULU.W	#8,D2		; MOLTIPLICA PER 8 IL NUMERO PRECEDENTE,
	ADD.W	D2,A5
	MOVEQ	#0,D6		; RESET D6
	MOVE.B	#8-1,D6
	.LOOP:
	ADD.W	#bpl-2,A4		; POSITIONING
	MOVE.B	(A5)+,(A4)+
	MOVE.B	#%00000000,(A4)+	; WRAPS MORE NICELY?
	DBRA	D6,.LOOP
	ADD.W	#bpl*2-2,A4	; POSITIONING
	MOVE.B	#%00000000,(A4)	; WRAPS MORE NICELY?
	.SKIP:
	SUBI.W	#1,D7
	CMPI.W	#0,D7
	BEQ.W	.RESET
	MOVE.W	D7,FRAMESINDEX
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS
	.RESET:
	ADDI.W	#1,TEXTINDEX
	MOVE.W	#3,D7
	MOVE.W	D7,FRAMESINDEX	; OTTIMIZZABILE
	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

__SHIFTTEXT:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	BSR.W	WaitBlitter
	MOVE.L	BGPLANE0,A4
	ADD.W	#bpl*(h-11),A4	; POSITIONING
	MOVE.W	#$FFFF,BLTAFWM	; BLTAFWM lo spiegheremo dopo
	MOVE.W	#$000F,BLTALWM	; BLTALWM lo spiegheremo dopo
	MOVE.W	#%0010100111110000,BLTCON0	; BLTCON0 (usa A+D); con shift di un pixel
	MOVE.W	#%0000000000000010,BLTCON1	; BLTCON1 BIT 12 DESC MODE
	MOVE.W	#3,BLTAMOD	; BLTAMOD =0 perche` il rettangolo
	MOVE.W	#3,BLTDMOD	; BLTDMOD 40-4=36 il rettangolo

	MOVE.L	A4,BLTAPTH	; BLTAPT  (fisso alla figura sorgente)
	MOVE.L	A4,BLTDPTH

	MOVE.W	#5*64+(w-10)/16,BLTSIZE	; BLTSIZE (via al blitter !)

	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

;********** Fastmem Data **********
	INCLUDE	"sincosin_table.i"	; VALUES

AUDIOCHLEVEL2:	DC.W 0
P61_LAST_POS:	DC.W MODSTART_POS
P61_DUMMY_POS:	DC.W 0
P61_FRAMECOUNT:	DC.W 0
P61_SEQ_POS:	DC.W 0
P61_DUMMY_SEQPOS:	DC.W 63
ANGLE:		DC.W 0
Z_POS:		DC.W 72	; 72 for min | -6 for big | 3 max 45 deg
Z_FACT:		DC.W 0
CENTER:		DC.W 0
TOP_MARGIN:	DC.W MARGINY-16
X_TEMP:		DC.W 0
Y_TEMP:		DC.W 0
XY_INIT:		DC.W 0
TEXTINDEX:	DC.W 0
FRAMESINDEX:	DC.W 3

KONEY_OPT:	; OPTIMIZED
	DC.W 0,0,1,0
	DC.W 1,0,1,2
	DC.W 1,2,4,2
	DC.W 4,2,4,0
	DC.W 4,0,5,0
	DC.W 5,0,5,1
	DC.W 5,1,3,1
	DC.W 3,1,3,4
	DC.W 3,4,5,4
	DC.W 5,4,5,5
	DC.W 5,5,4,5
	DC.W 4,5,4,3
	DC.W 4,3,2,3
	DC.W 2,3,2,5
	DC.W 2,5,0,5
	DC.W 0,5,0,0

SEQ_POS_ON:	DC.B $00,$51,$5C,$65,$00,$7A,$84,$8E,$00,$A3,$AD,$B8,$00,$CD,$D8,$E2
SEQ_POS_BIT:	DC.B $1,$1,$0,$1,$0,$0,$1,$1,$0,$0,$1,$0,$1,$0,$1,$1
SEQ_POS_OFF:	DC.B $47,$00,$00,$00,$70,$00,$00,$00,$99,$00,$00,$00,$C2,$00,$00,$00

BGPLANE0:		DC.L TR909
BGPLANE1:		DC.L TR909+bpl*h
BGPLANE2:		DC.L TR909+bpl*h*2
BGPLANE3:		DC.L TR909+bpl*h*3
BGPLANE4:	DC.L TR909+bpl*h*3
BITPLANE_PTR:	DC.L TR909+(bpl*h*4)	; +(bpl/2)-(LOGOBPL/2)
DrawBuffer:	DC.L SCREEN2		; pointers to buffers
ViewBuffer:	DC.L SCREEN1		; to be swapped

TEXT:	DC.B "!!WARNING!! - EPILEPSY DANGER AHEAD!!   SERIOUSLY... :)    "
	DC.B "WELCOME TO:   ### MECHMICROBES ###   KONEY'S FOURTH AMIGA HARDCORE RELEASE!   "
	DC.B "THIS TEXT IS DUMMY TEXT, IT SAYS NOTHING, IT'S JUST A PLACEHOLDER... "
	DC.B "ONLY FOR TESTING! - MAKE SURE TO VISIT WWW.KONEY.ORG FOR MORE INDUSTRIAL "
	DC.B "AMIGACORE!!            .EOF                                                              "
	EVEN
_TEXT:

FONT:		DC.L 0,0			; SPACE CHAR
		INCBIN "cosmicalien_font.raw",0
		EVEN

	SECTION	ChipData,DATA_C	;declared data that must be in chipmem

TR909:		INCBIN "TR-909_368x230x5.raw"
MODULE:		INCBIN "mechmicrobes.P61"	; code $B000

LED_ON:
	.VPOS:
	DC.B $90	; Posizione verticale di inizio sprite (da $2c a $f2)
	.HPOS:
	DC.B $47	; Posizione orizzontale di inizio sprite (da $40 a $d8)
	DC.B $00	; $50+13=$5d	; posizione verticale di fine sprite
	.CTRL:
	DC.B $00
	DC.W $0000,$0000,$0000,$0000,$0000,$0000
	DC.W $0000,$0000,$0000,$0000,$0000,$0000
	DC.W $0000,$0000,$0000,$0000,$0000,$0000
	DC.W $0000,$0000,$0000,$0000,$0000,$0000
	DC.W $0000,$0000,$0000,$0000,$0000,$0000
	DC.W 0,0	; 2 word azzerate definiscono la fine dello sprite.
LED_OFF:	
	.VPOS:
	DC.B $90	; Posizione verticale di inizio sprite (da $2c a $f2)
	.HPOS:
	DC.B $00	; Posizione orizzontale di inizio sprite (da $40 a $d8)
	DC.B $00	; $50+13=$5d	; posizione verticale di fine sprite
	.CTRL:
	DC.B $00
	DC.W $0000,$0000,$0000,$0000,$0000,$0000
	DC.W $0000,$0000,$0000,$0000,$0000,$0000
	DC.W $0000,$0000,$0000,$0000,$0000,$0000
	DC.W $0000,$0000,$0000,$0000,$0000,$0000
	DC.W $0000,$0000,$0000,$0000,$0000,$0000
	DC.W 0,0	; 2 word azzerate definiscono la fine dello sprite.

COLS_NEG	= 0

COPPER:
	DC.W $1FC,0		; Slow fetch mode, remove if AGA demo.
	DC.W $8E,$3061		; 238h display window top, left | DIWSTRT - 11.393
	DC.W $90,$16D1		; and bottom, right.	| DIWSTOP - 11.457
	DC.W $92,$28		; Standard bitplane dma fetch start
	DC.W $94,$D8		; and stop eab.abime.net/showthread.php?t=69926
	DC.W $106,$0C00		; (AGA compat. if any Dual Playf. mode)
	DC.W $108,0		; bwid-bpl	;modulos
	DC.W $10A,0		; bwid-bpl	;RISULTATO = 80 ?
	DC.W $102,0		; SCROLL REGISTER (AND PLAYFIELD PRI)
	DC.W $104,%0000000000100100	; BPLCON2
	DC.W $100,bpls*$1000+$200	; enable bitplanes

	.BplPtrs:
	DC.W $E0,0
	DC.W $E2,0
	DC.W $E4,0
	DC.W $E6,0
	DC.W $E8,0
	DC.W $EA,0
	DC.W $EC,0
	DC.W $EE,0
	DC.W $F0,0
	DC.W $F2,0
	DC.W $F4,0
	DC.W $F6,0	; full 6 ptrs, in case you increase bpls

	.SpritePointers:
	DC.W $120,0,$122,0 ; 0
	DC.W $124,0,$126,0 ; 1
	DC.W $128,0,$12A,0 ; 2
	DC.W $12C,0,$12E,0 ; 3
	DC.W $130,0,$132,0 ; 4
	DC.W $134,0,$136,0 ; 5
	DC.W $138,0,$13A,0 ; 6
	DC.W $13C,0,$13E,0 ; 7

	.Palette:
	IFEQ COLS_NEG
	DC.W $0180,$0000,$0182,$0853,$0184,$0BBB,$0186,$0D61
	DC.W $0188,$0D88,$018A,$0667,$018C,$0556,$018E,$0FFF
	DC.W $0190,$0EEE,$0192,$0DDD,$0194,$0CCD,$0196,$0CCC
	DC.W $0198,$0AAA,$019A,$0999,$019C,$0888,$019E,$0777
	DC.W $01A0,$0555,$01A2,$0444,$01A4,$0FF0,$01A6,$0EEF
	DC.W $01A8,$0BBC,$01AA,$099A,$01AC,$0F0F,$01AE,$00FF
	DC.W $01B0,$0CBA,$01B2,$0CA9,$01B4,$0778,$01B6,$000F
	DC.W $01B8,$00F0,$01BA,$0B00,$01BC,$0632,$01BE,$0F00
	ELSE
	DC.W $0180,$0000,$0182,$09CD,$0184,$0666,$0186,$04BD
	DC.W $0188,$0499,$018A,$0BBA,$018C,$0CCB,$018E,$0222
	DC.W $0190,$0333,$0192,$0444,$0194,$0554,$0196,$0555
	DC.W $0198,$0777,$019A,$0888,$019C,$0999,$019E,$0AAA
	DC.W $01A0,$0CCC,$01A2,$0DDD,$01A4,$022D,$01A6,$0332
	DC.W $01A8,$0665,$01AA,$0887,$01AC,$02D2,$01AE,$0D22
	DC.W $01B0,$0567,$01B2,$0578,$01B4,$0AA9,$01B6,$0DD2
	DC.W $01B8,$0D2D,$01BA,$06DD,$01BC,$0BDD,$01BE,$02DD
	ENDC

	; **** COPPERWAITS ****
	.LOGO_COLORS:
	DC.W $3001,$FF00		; ## START ##
	DC.W $01B6,$0FFF		; WHITE
	DC.W $01AC,$0BBC		; TRASP?
	DC.W $01A4,$0FFF
	DC.W $01BE,$0FFF		; TRASP?
	DC.W $01B8,$0DDE		; TRASP?

	DC.W $8001,$FF00		; ## SECOND PART TXT ##
	DC.W $01A2,$0DDD
	DC.W $0182,$0999		; TXT DARK
	DC.W $0186,$0AAA		; TXT LIGHT
	DC.W $01A6,$0DDD

	DC.W $8601,$FF00		; ## KNOBS ##
	DC.W $01AA,$0BBB
	DC.W $0182,$0853		; TXT RESTORE
	DC.W $0186,$0D61		; TXT RESTORE
	DC.W $01A2,$0CCD
	DC.W $01A6,$0EEF
	DC.W $0198,$0999

	DC.W $9401,$FF00		; ## TXT ##
	DC.W $0182,$0999		; TXT DARK
	DC.W $0186,$0AAA		; TXT LIGHT
	DC.W $01AA,$0BBB
	DC.W $01A6,$0DDD

	DC.W $9A01,$FF00		; ## KNOBS ##
	DC.W $0182,$0853		; TXT RESTORE
	DC.W $0186,$0D61		; TXT RESTORE
	DC.W $01A6,$0EEF

	DC.W $B001,$FF00		; ## RESTORE ##
	DC.W $01B6,$0000
	DC.W $01BE,$0F00
	DC.W $01A2,$0444
	DC.W $01A6,$0EEF
	DC.W $01AA,$099A
	DC.W $0198,$0AAA

	.SEQ_LED:
	DC.W $F801,$FF00		; horizontal position masked off
	DC.W $174,$FC00,$176,$FC00	; SPR6DATA
	DC.W $17C,$0000,$17E,$FC00	; SPR7DATA
	DC.W $FA01,$FF00
	DC.W $174,$0000,$176,$0000	; SPR6DATA
	DC.W $17C,$0000,$17E,$0000	; SPR7DATA

	DC.W $FF01,$FF00		; horizontal position masked off
	DC.W $018E,$0CCD		; SCROLLTEXT - $0D61

	DC.W $FFDF,$FFFE		; allow VPOS>$ff

	DC.W $FFFF,$FFFE		; magic value to end copperlist
_COPPER:

;*******************************************************************************
	SECTION	ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************

BUFFER3D:		DS.B LOGOSIDE*bpl	; bigger to hold zoom
EMPTY:		DS.B LOGOSIDE*bpl	; clear buffer
SCREEN1:		DS.B h*bwid	; Define storage for buffer 1
SCREEN2:		DS.B h*bwid	; two buffers

END
