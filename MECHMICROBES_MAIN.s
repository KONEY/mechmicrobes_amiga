;*** WWW.KONEY.ORG *******
;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/mechmicrobes_amiga/"
	SECTION	"Code",CODE
	INCLUDE	"custom-registers.i"
	INCLUDE	"PhotonsMiniWrapper1.04!.S"
	INCLUDE	"cli_output.s"
	INCLUDE	"PT12_OPTIONS.i"
	INCLUDE	"P6112-Play-stripped.i"
;********** Constants **********
wd		EQU 368		; screen width, height, depth
hg		EQU 230
bpls		EQU 5		; handy values:
bwpl		EQU wd/16*2	; byte-width of 1 bitplane line (46)
bwid		EQU bpls*bwpl	; byte-width of 1 pixel line (all bpls)
TrigShift		EQU 7
PXLSIDE		EQU 16
Z_Shift		EQU PXLSIDE*5/2	; 5x5 obj
LOGOSIDE		EQU 16*7
LOGOBPL		EQU LOGOSIDE/16*2
MARGINX		EQU wd/2
MARGINY		EQU LOGOSIDE/2
BLIT_POSITION	EQU (bwpl/2-LOGOBPL/2)+((LOGOSIDE+16)*40-2)
BPL_BLIT_OFFSET	EQU BLIT_POSITION+bwpl*hg*4		; we precalculate :)
TXT_FRMSKIP 	EQU 3
;*************
MODSTART_POS 	EQU 1-1		; start music at position # !! MUST BE EVEN FOR 16BIT
;*************

VarTimesTrig 	MACRO ; 3 = 1 * 2, where 2 is cos(Angle)^(TrigShift*2) or sin(Angle)^(TrigShift*2)
	move.l \1,\3
	muls \2,\3
	asr.l #TrigShift,\3 ;left >>= TrigShift
	asr.l #TrigShift,\3
		ENDM

;CLR.W	$100		; DEBUG | w 0 100 2
;********** Demo **********	; Demo-specific non-startup code below.
Demo:				; a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	MOVE.L	#VBint,$6C(A4)
	MOVE.W	#%1110000000100000,INTENA
	;** SOMETHING INSIDE HERE IS NEEDED TO MAKE MOD PLAY! **
	;move.w	#%1110000000000000,INTENA	; Master and lev6	; NO COPPER-IRQ!
	MOVE.W	#%1000011111100000,DMACON
	;*--- clear screens ---*
	;LEA	SCREEN1,A1
	;BSR.W	ClearScreen
	;LEA	SCREEN2,A1
	;BSR.W	ClearScreen
	;BSR	WaitBlitter
	;*--- start copper ---*
	LEA	SCREEN1,A0

	MOVEQ	#bwpl,D0
	LEA	COPPER1\.BplPtrs+2,A1
	MOVEQ	#bpls-1,D1
	BSR.W	PokePtrs

	MOVEQ	#bwpl,D0
	LEA	COPPER2\.BplPtrs+2,A1
	MOVEQ	#bpls-1,D1
	BSR.W	PokePtrs

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	BSR.W	__DUPLICATE_SCREEN
	BSR.W	__NEGATIVE_COLORS	; INVERT COLORS IN COP2
	MOVEQ	#64-1,D1		; ** POINTS TO COORDS **
	LEA	KONEY_OPT,A2
	.calcuCoords:
	MOVE.W	(A2),D0
	MULU.W	#PXLSIDE,D0
	MULU.W	#Z_Shift,D0	; PRECALCULATED ZSHIFT
	MOVE.W	D0,(A2)+
	DBRA	D1,.calcuCoords
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	BSR.W	__POINT_SPRITES	; #### Point sprites

	; ---  Call P61_Init  ---
	MOVEM.L	D0-A6,-(SP)
	LEA	MODULE,A0
	SUB.L	A1,A1
	SUB.L	A2,A2
	MOVE.W	#MODSTART_POS,P61_InitPos	; TRACK START OFFSET
	JSR	P61_Init
	MOVEM.L (SP)+,D0-A6

	BSR.W	__POINT_COPPERLISTS
;********************  main loop  ********************
MainLoop:
	move.w	#$12c,d0	;No buffering, so wait until raster
	bsr.w	WaitRaster	;is below the Display Window.

	BSR.W	__SET_PT_VISUALS

	;*--- swap buffers ---*
	movem.l	DrawBuffer(PC),a2-a3
	exg	a2,a3
	movem.l	a2-a3,DrawBuffer	;draw into a2, show a3
	;*--- show one... ---*
	move.l	a3,a0
	move.l	#bwpl*hg,d0
	lea	COPPER1\.BplPtrs+2,a1
	moveq	#bpls-1,d1
	bsr.w	PokePtrs

	move.l	a3,a0
	ADD.L	#bwpl,A0		; Oppure aggiungi la lunghezza di una linea
	move.l	#bwpl*hg,d0
	lea	COPPER2\.BplPtrs+2,a1
	moveq	#bpls-1,d1
	bsr.w	PokePtrs
	;*--- ...draw into the other(a2) ---*
	move.l	a2,a1

	BSR.W	ClearBlitterBuffer
	;MOVE.L	#TR909,DrawBuffer
	; do stuff here :)

	BSR.W	__FILLANDSCROLLTXT

	.block0:			; BEWARE THE SMC !!
	TST.W	P61_LAST_POS
	BEQ.B	.skipSequencer
	MOVE.W	#$6000|(.call-(.block0+2)),.block0	; mock a BRA.S .skipCMP
	.call:	
	BSR.W	__SET_SEQUENCER_LEDS
	.skipSequencer:

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
	; **** ROTATION VALUES ****

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

	;ADDI.W	#2,ANGLE		; JUST ROTATE FOREVER :)
	;*--- main loop end ---*

	ENDING_CODE:
	BTST	#6,$BFE001
	BNE.S	.DontShowRasterTime
	MOVE.W	#$0FF,$DFF196	; show rastertime left down to $12c
	.DontShowRasterTime:
	BTST	#2,$DFF016	; POTINP - RMB pressed?
	BNE.W	MainLoop		; then loop
	;*--- exit ---*
	BSR.W	__SONG_POS_2_ASCII
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
	move.w	d2,(a1)		; high word of address
	move.w	a0,4(a1)		; low word of address
	addq.w	#8,a1		; skip two copper instructions
	add.l	d0,a0		; next ptr
	dbf	d1,.bpll
	rts
ClearScreen:			; a1=screen destination address to clear
	bsr	WaitBlitter
	clr.w	BLTDMOD		; destination modulo
	move.l	#$01000000,BLTCON0	; set operation type in BLTCON0/1
	move.l	a1,BLTDPTH	; destination address
	move.l	#hg*bpls*64+bwpl/2,BLTSIZE	;blitter operation size
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
	MOVE.W	#wd*64+LOGOSIDE/16,BLTSIZE	; Start Blitter (Blitsize)
	RTS
Drawline:
	LEA	BUFFER3D,A0	; ROUTINE STOLEN FROM RAM_JAM
	ADDI.W	#MARGINX,D0
	ADD.W	TOP_MARGIN,D1
	ADDI.W	#MARGINX,D2
	ADD.W	TOP_MARGIN,D3

	sub.w	d1,d3		; D3=Y2-Y1
	beq.s	.skip		; per il fill non servono linee orizzontali 
	bgt.s	.y2gy1		; salta se positivo..
	exg	d0,d2		; ..altrimenti scambia i punti
	add.w	d3,d1		; mette in D1 la Y piu` piccola
	neg.w	d3		; D3=DY
	.y2gy1:
	mulu.w	#bwpl,d1		; offset Y
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
	move.w	#bwpl,BLTCMOD	; BLTCMOD = 40
	move.w	#bwpl,BLTDMOD	; BLTDMOD = 40
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
	BSR.W	WaitBlitter
	MOVE.W	#$FFFF,BLTAFWM		; BLTAFWM
	MOVE.W	#$FFFF,BLTALWM		; BLTALWM
	MOVE.W	#%0000100111110000,BLTCON0	; BLTCON0
	;MOVE.W	#%0000000000001010,BLTCON1	; BLTCON1
	MOVE.W	#%0000000000010010,BLTCON1	; BLTCON1
	MOVE.W	#bwpl-LOGOBPL,BLTAMOD	; BLTAMOD
	MOVE.W	#bwpl-LOGOBPL,BLTDMOD	; Init modulo Dest D
	LEA	BUFFER3D,A4
	ADD.L	#BLIT_POSITION,A4
	MOVE.L	A4,BLTAPTH		; BLTAPT  (fisso alla figura sorgente)
	MOVEM.L	DrawBuffer(PC),A5
	ADD.L	#BPL_BLIT_OFFSET,A5
	MOVE.L	A5,BLTDPTH
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
	RTS

__SET_PT_VISUALS:
	; ## SONG POS RESETS ##
	MOVE.W	P61_Pos,D6
	MOVE.W	P61_DUMMY_POS,D5
	CMP.W	D5,D6
	BEQ.S	.dontReset
	ADDQ.W	#1,P61_DUMMY_POS
	ADDQ.W	#1,P61_LAST_POS
	.dontReset:
	; ## SONG POS RESETS ##

	; ## COMMANDS 80x TRIGGERED EVENTS ##
	MOVE.W	P61_1F,D1		; 1Fx
	MOVE.W	P61_E8,D2		; 80x

	CMPI.W	#$F,D1		; $F STROBO ON
	BNE.S	.skip1FF
	BSR.W	__START_STROBO
	MOVE.W	#0,P61_1F		; RESET FX
	BRA.S	.skipAddAngle

	.skip1FF:
	CMPI.W	#1,D1		; IF 1F & 80 EQUALS
	BNE.S	.dontResetAngle
	CMPI.W	#1,D2		; IF 1F & 80 EQUALS
	BNE.S	.dontResetAngle
	MOVE.W	#0,ANGLE		; RESET LOGO
	;MOVE.W	#3,Z_POS		; MAX SIZE
	MOVE.W	#0,P61_1F		; RESET FX
	MOVE.W	#0,P61_E8	; RESET FX
	BRA.S	.skipSubAngle
	.dontResetAngle:

	ADD.W	D1,ANGLE		; WE OPTIMIZE :)
	ADD.W	D1,ANGLE
	.skipAddAngle:

	CMPI.W	#$F,D2		; $F STROBO OFF
	BNE.S	.skip80F
	BSR.W	__STOP_STROBO
	MOVE.W	#0,P61_E8	; RESET FX
	BRA.S	.skipSubAngle
	.skip80F:

	SUB.W	D2,ANGLE		; WE OPTIMIZE :)
	SUB.W	D2,ANGLE
	.skipSubAngle:
	; ## COMMANDS 80x TRIGGERED EVENTS ##

	; ## MOD VISUALIZERS ##########
	; ## KICK FX - SAMPLE#3 - KICK10.WAV ON CH1
	LEA	P61_visuctr1(PC),A0	; which channel? 0-3
	MOVEQ	#15,D0		; maxvalue
	SUB.W	(A0),D0		; -#frames/irqs since instrument trigger
	BPL.S	.ok1		; below minvalue?
	MOVEQ	#0,D0		; then set to minvalue
	.ok1:

	MOVE.W	P61_CH1_INS,D1	; NEW VALUES FROM P61
	CMPI.W	#3,D1		; SAMPLE # 3
	BLO.S	.skipKickFx
	LEA	ZOOM_VALUES,A0
	MOVE.B	(A0,D0.W),D1
	MOVE.W	D1,Z_POS

	MOVE.W	TOP_MARGIN,D1	; RELOCATE LOGO
	CMPI.W	#MARGINY,D1
	BEQ.S	.skipKickFx
	ADDI.W	#1,TOP_MARGIN
	.skipKickFx:
	; MOD VISUALIZERS *****

	MOVE.W	P61_LAST_POS,D1
	CMPI.W	#76,D1		; STOP AT END OF MUSIC
	BNE.S	.dontStopMusic
	MOVEM.L	D0-A6,-(SP)
	JSR	P61_End
	MOVEM.L	(SP)+,D0-A6
	MOVE.W	#0,P61_DUMMY_POS
	SUBI.W	#1,P61_LAST_POS
	.dontStopMusic:

	RTS

__FILLANDSCROLLTXT:
	MOVE.W	FRAMESINDEX,D7
	CMPI.W	#3,D7
	BNE.W	.SKIP
	MOVEM.L	ViewBuffer,A4	; Trick for double buffering ;)
	LEA	FONT,A5
	LEA	TEXT,A3
	ADD.W	#bwpl*(hg-19)+1,A4	; POSITIONING
	ADD.W	TEXTINDEX,A3
	CMP.L	#_TEXT-1,A3	; Siamo arrivati all'ultima word della TAB?
	BNE.S	.PROCEED
	MOVE.W	#0,TEXTINDEX	; Riparti a puntare dalla prima word
	LEA	TEXT,A3		; FIX FOR GLITCH (I KNOW IT'S FUN... :)
	.PROCEED:
	MOVE.B	(A3),D2		; Prossimo carattere in d2
	SUBI.B	#$20,D2		; TOGLI 32 AL VALORE ASCII DEL CARATTERE, IN
	MULU.W	#8,D2		; MOLTIPLICA PER 8 IL NUMERO PRECEDENTE,
	ADD.W	D2,A5
	MOVEQ	#0,D6		; RESET D6
	MOVE.B	#8-1,D6
	.LOOP:
	ADD.W	#bwpl-2,A4	; POSITIONING
	MOVE.B	(A5)+,(A4)+
	MOVE.B	#%00000000,(A4)+	; WRAPS MORE NICELY?
	DBRA	D6,.LOOP
	ADD.W	#bwpl*2-2,A2	; POSITIONING
	ADD.W	#bwpl*2-2,A4	; POSITIONING
	MOVE.B	#%00000000,(A4)	; WRAPS MORE NICELY?
	.SKIP:
	SUBI.W	#1,D7
	CMPI.W	#0,D7
	BEQ.W	.RESET
	MOVE.W	D7,FRAMESINDEX
	BRA.S	.SHIFTTEXT
	.RESET:
	ADDI.W	#1,TEXTINDEX
	MOVE.W	#3,D7
	MOVE.W	D7,FRAMESINDEX	; OTTIMIZZABILE

	.SHIFTTEXT:
	BSR.W	WaitBlitter
	MOVEM.L	ViewBuffer,A2	; DOUBLE
	MOVE.L	DrawBuffer,A4	; BUFFERING ;)
	ADD.W	#bwpl*(hg-11),A2	; POSITIONING
	ADD.W	#bwpl*(hg-11),A4	; POSITIONING
	MOVE.W	#$FFFF,BLTAFWM	; BLTAFWM lo spiegheremo dopo
	MOVE.W	#$000F,BLTALWM	; BLTALWM lo spiegheremo dopo
	MOVE.W	#%0010100111110000,BLTCON0	; BLTCON0 (usa A+D); con shift di un pixel
	MOVE.W	#%0000000000000010,BLTCON1	; BLTCON1 BIT 12 DESC MODE
	MOVE.W	#3,BLTAMOD	; BLTAMOD =0 perche` il rettangolo
	MOVE.W	#3,BLTDMOD	; BLTDMOD 40-4=36 il rettangolo
	MOVE.L	A2,BLTAPTH	; BLTAPT  (fisso alla figura sorgente)
	MOVE.L	A4,BLTDPTH
	MOVE.W	#5*64+(wd-10)/16,BLTSIZE	; BLTSIZE (via al blitter !)
	RTS

__POINT_SPRITES:			; #### Point LOGO sprites
	LEA	COPPER1\.SpritePointers,A1
	MOVE.L	#0,D0		; sprite 0
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0		; sprite 1
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0		; sprite 2
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0		; sprite 3
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0		; sprite 4
	MOVE.W	D0,6(A1)
	SWAP	D0
	MOVE.W	D0,2(A1)

	ADDQ.W	#8,A1
	MOVE.L	#0,D0		; sprite 5
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
	RTS

__NEGATIVE_COLORS:
	LEA	COPPER1\.Palette,A2
	LEA	COPPER2\.Palette,A3
	.loop:
	MOVE.L	(A2)+,D1
	CMP.L	#$FFFFFFFE,D1	; END OF COPPERLIST
	BEQ.S	.exit
	CMP.L	#$01800000,D1	; BG COLOR UNCHANGED
	BEQ.S	.skipWaits
	CMP.W	#$FF00,D1
	BEQ.S	.skipWaits
	CMP.W	#$FC00,D1
	BEQ.S	.skipWaits
	CMP.W	#$0000,D1
	BEQ.S	.skipWaits
	CMP.W	#$FFFE,D1		; DONT CHANGE INSTRUCTIONS
	BEQ.S	.skipWaits

	MOVE.W	#$0FFF,D3	; TO MAKE COLOR NEGATIVE

	MOVE.W	D1,D5	; R
	MOVE.W	D1,D6	; G
	MOVE.W	D1,D7	; B
	LSR.W	#8,D5	; THIS IS SHAMEFUL
	LSL.W	#8,D6	; BUT I'M IN HURRY :)
	LSR.W	#8,D6
	LSR.W	#4,D6
	ROR.W	#4,D7
	LSR.W	#8,D7
	LSR.W	#4,D7

	CMP.B	#3,D5
	BLT.S	.noRed
	SUB.B	#3,D5	; TO MAKE IT LIGHTER
	.noRed:
	CMP.B	#3,D6
	BLT.S	.noGreen
	SUB.B	#3,D6	; TO MAKE IT LIGHTER
	.noGreen:
	CMP.B	#3,D7
	BLT.S	.noBlue
	SUB.B	#3,D7	; TO MAKE IT LIGHTER
	.noBlue:

	ROL.W	#8,D5
	ROL.W	#4,D6
	OR.W	D5,D6
	OR.W	D6,D7

	MOVE.W	D7,D1

	SUB.W	D1,D3
	MOVE.W	D3,D1
	.skipWaits:
	MOVE.L	D1,(A3)+
	BRA.S	.loop
	.exit:
	RTS

__POINT_COPPERLISTS:
	MOVE.L	#COPPER1,D0
	LEA	COPPER1\.CopJumpL,A0
	MOVE.W	D0,(A0)
	LEA	COPPER1\.CopJumpH,A0
	SWAP	D0
	MOVE.W	D0,(A0)
	
	MOVE.L	#COPPER1,D0
	LEA	COPPER2\.CopJumpL,A0
	MOVE.W	D0,(A0)
	LEA	COPPER2\.CopJumpH,A0
	SWAP	D0
	MOVE.W	D0,(A0)

	SWAP	D0
	MOVE.W	#$8000,$DFF02A	; FROM EAB
	MOVE.L	D0,COP1LC		; COP1LCH
	RTS

__STOP_STROBO:
	MOVE.L	#COPPER1,D0
	LEA	COPPER1\.CopJumpL,A0
	MOVE.W	D0,(A0)
	LEA	COPPER1\.CopJumpH,A0
	SWAP	D0
	MOVE.W	D0,(A0)
	RTS

__START_STROBO:
	MOVE.L	#COPPER2,D0
	LEA	COPPER1\.CopJumpL,A0
	MOVE.W	D0,(A0)
	LEA	COPPER1\.CopJumpH,A0
	SWAP	D0
	MOVE.W	D0,(A0)
	RTS

__SONG_POS_2_ASCII:
	; ## TRANSFORM SONGPOS INTO ASCII TXT  ##
	MOVE.W	P61_LAST_POS,D5
	ADDQ.W	#1,D5	; DONT SHOW 0 TO USER
	CLR.L	D6
	MOVE.W	D5,D6
	DIVU.W	#$A,D6
	SWAP	D6
	MOVE.W	D6,D5
	SWAP	D6
	OR.B	#48,D6	; POINT TO CHAR 0
	LSL.W	#8,D6
	OR.B	#48,D5	; POINT TO CHAR 0
	OR.W	D6,D5
	MOVE.W	D5,TXT_POS
	RTS

__DUPLICATE_SCREEN:
	MOVEM.L	D0-A6,-(SP)	; SAVE TO STACK
	LEA	SCREEN1,A0
	LEA	SCREEN2,A1
	MOVE.L	#hg*bpls-1,D1	; LINES
	.outerLoop:
	MOVE.L	#wd/16-1,D0	; SIZE OF SOURCE IN WORDS
	.innerLoop:
	MOVE.W	(A0)+,(A1)+
	DBRA	D0,.innerLoop
	DBRA.W	D1,.outerLoop

	MOVEM.L	(SP)+,D0-A6	; FETCH FROM STACK
	RTS

;********** Fastmem Data **********
	INCLUDE	"sincosin_table.i"	; VALUES

AUDIOCHLEVEL1:	DC.W 0
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
END_TEXT_LEN:	DC.W 152

ZOOM_VALUES:	DC.B 70,67,65,62,48,44,40,36,32,28,24,20,16,12,8,5,3
		EVEN

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

DrawBuffer:	DC.L SCREEN2		; pointers to buffers
ViewBuffer:	DC.L SCREEN1		; to be swapped

FONT:		DC.L 0,0			; SPACE CHAR
		INCBIN "cosmicalien_font.raw",0
		EVEN

END_TEXT:	DC.B "THANKS FOR EXECUTING MECHMICROBES BY KONEY!",10
		DC.B "YOU REACHED BLOCK "
		TXT_POS: DC.B "XX"
		DC.B " FROM A SEQUENCE OF 76. ",10
		DC.B "VISIT WWW.KONEY.ORG FOR MORE TECHNO "
		DC.B "AND HARDCORE AMIGA STUFF!",10
		EVEN

TEXT:		INCLUDE "textscroller.i"

	SECTION "ChipData",DATA_C	;declared data that must be in chipmem

SCREEN1:		INCBIN "TR-909_368x230x5.raw"

MODULE:		INCBIN "mechmicrobes.P61"	; code $100B002

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

COPPER1:		INCLUDE "copperlist_common.i" _COPPER1:
COPPER2:		INCLUDE "copperlist_common.i" _COPPER2:

; *******************************************************************
	SECTION	ChipBuffers,BSS_C	;BSS doesn't count toward exe size
; *******************************************************************

BUFFER3D:		DS.B LOGOSIDE*bwpl	; bigger to hold zoom
EMPTY:		DS.B LOGOSIDE*bwpl	; clear buffer
;SCREEN1:		DS.B 0		; Define storage for buffer 1
SCREEN2:		DS.B bwid*hg	; two buffers

END
