;APS00000000000000000000000000000000000000000000000000000000000000000000000000000000
;*** WWW.KONEY.ORG *******
;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/mechmicrobes_amiga/"
	SECTION	"Code",CODE
	INCLUDE	"PhotonsMiniWrapper1.04!.S"
	INCLUDE	"custom-registers.i"	;use if you like ;)
	INCLUDE	"PT12_OPTIONS.i"
	INCLUDE	"P6112-Play-stripped.i"
;********** Constants **********
w=368		;screen width, height, depth
h=230
bpls=5		;handy values:
bpl=w/16*2	;byte-width of 1 bitplane line (40)
bwid=bpls*bpl	;byte-width of 1 pixel line (all bpls)
TrigShift=7
PXLSIDE=16
Z_Shift=PXLSIDE*5/2	; 5x5 obj
LOGOSIDE=16*7
LOGOBPL=LOGOSIDE/16*2
MARGINX=(w/2)
MARGINY=(LOGOSIDE/2)-9
;*************
MODSTART_POS=0		; start music at position # !! MUST BE EVEN FOR 16BIT
;*************

VarTimesTrig MACRO ;3 = 1 * 2, where 2 is cos(Angle)^(TrigShift*2) or sin(Angle)^(TrigShift*2)
	move.l \1,\3
	muls \2,\3

	asr.l #TrigShift,\3 ;left >>= TrigShift
	asr.l #TrigShift,\3
	ENDM

;********** Demo **********	;Demo-specific non-startup code below.
Demo:				;a4=VBR, a6=Custom Registers Base addr
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
	;LEA	MODULE,A0
	SUB.L	A1,A1
	SUB.L	A2,A2
	MOVE.W	#MODSTART_POS,P61_InitPos	; TRACK START OFFSET
	;JSR	P61_Init
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
	bsr	ClearBuffer2
	;MOVE.L	#BUFFER3D,DrawBuffer
	MOVE.L	#TR909,DrawBuffer

	; do stuff here :)

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
	.notFirstBlock:
	LEA	(PC),A2
	MOVE.W	P61_LAST_POS,D0
	CMPI	#0,D0
	BEQ.S	.dontStartSequencer
	MOVE.W	#$F00,$DFF180	; show rastertime left down to $12c
	LEA	.skipCMP,A1
	SUB.W	A2,A1
	MOVE.W	A1,D0
	EOR.W	#$6000,D0	; BRA.S	.skipCMP
	LEA	.notFirstBlock,A0
	MOVE.W	D0,(A0)
	.skipCMP:
	BSR.W	__SET_SEQUENCER_LEDS
	.dontStartSequencer:

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
	MOVE.W	#$0FF,$DFF180	; show rastertime left down to $12c
	.DontShowRasterTime:
	BTST	#2,$DFF016	; POTINP - RMB pressed?
	BNE.W	MainLoop		; then loop
	;*--- exit ---*
	;    ---  Call P61_End  ---
	MOVEM.L D0-A6,-(SP)
	;JSR P61_End
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
ClearBuffer:			; by KONEY
	bsr	WaitBlitter
	clr.w	BLTDMOD			; destination modulo
	move.l	#$01000000,BLTCON0		; set operation type in BLTCON0/1
	move.l	BITPLANE_PTR,BLTDPTH		; destination address
	MOVE.W	#LOGOSIDE*bpls*64+bpl/2,BLTSIZE	; Start Blitter (Blitsize)
	rts
ClearBuffer2:
	bsr	WaitBlitter
	MOVE.W	#$09f0,BLTCON0		; A**,Shift 0, A -> D
	MOVE.W	#0,BLTCON1		; Everything Normal
	MOVE.L	#0,BLTAMOD		; Init modulo Sou. A
	MOVE.W	#0,BLTDMOD		; Init modulo Dest D
	MOVE.L	#EMPTY,BLTAPTH		; Source
	MOVE.L	#BUFFER3D,BLTDPTH		; Dest
	MOVE.W	#w*64+LOGOSIDE/16,BLTSIZE	; Start Blitter (Blitsize)
	RTS

;******************************************************************************
; Questa routine effettua il disegno della linea. prende come parametri gli
; estremi della linea P1 e P2, e l'indirizzo del bitplane su cui disegnarla.
; D0 - X1 (coord. X di P1)
; D1 - Y1 (coord. Y di P1)
; D2 - X2 (coord. X di P2)
; D3 - Y2 (coord. Y di P2)
; A0 - indirizzo bitplane
;******************************************************************************

Drawline:
	LEA	BUFFER3D,A0
	ADDI.W	#MARGINX,D0
	ADDI.W	#MARGINY,D1
	ADDI.W	#MARGINX,D2
	ADDI.W	#MARGINY,D3

	cmp.w	d1,d3		; se D3>D1 i punti sono gia` nell'ordine giusto
	bge.w	Ordinati
	exg.l	d1,d3		; altrimenti scambiali
	exg.l	d0,d2
	Ordinati:
	sub.w	d1,d3		; D3=DiffY  (e` sicuramente positivo)

	; * scelta ottante
	sub.w	d0,d2		; D2=X2-X1
	bmi.s	DRAW4		; se negativo salta, altrimenti D2=DiffX
	cmp.w	d3,d2		; confronta DiffX e DiffY
	bmi.s	DRAW1		; se D2<D3 salta..
				; .. altrimenti D3=DY e D2=DX
	moveq	#$10,d5		; codice ottante
	bra.s	DRAWL

	DRAW1:
	exg.l	d2,d3		; scambia D2 e D3, in modo che D3=DY e D2=DX
	moveq	#$00,d5		; codice ottante
	bra.s	DRAWL

	DRAW4:
	neg.w	d2		; rende D2 positivo
	cmp.w	d3,d2		; confronta DiffX e DiffY
	bmi.s	DRAW5		; se D2<D3 salta..
				; .. altrimenti D3=DY e D2=DX
	moveq	#$14,d5		; codice ottante
	bra.s	DRAWL

	DRAW5:
	exg.l	d2,d3		; scambia D2 e D3, in modo che D3=DY e D2=DX
	moveq	#$08,d5		; codice ottante
	bra.w	DRAWL

	; Quando l'esecuzione raggiunge questo punto, abbiamo:
	; D2 = DX
	; D3 = DY
	; D5 = codice ottante

	DRAWL:
	mulu.w	#bpl,d1		; offset Y
	add.w	d1,a0		; aggiunge l'offset Y all'indirizzo

	move.w	d0,d1		; copia la coordinata X
	lsr.w	#4,d1		; cancella i 4 bit bassi della X
	add.w	d1,d1		; ottiene l'offset X in bytes
	add.w	d1,a0		; aggiunge l'offset X all'indirizzo

	andi.w	#$000F,d0	; seleziona i 4 bit piu` bassi della X..
	move.w	d0,d1		; .. li copia in D1..
	ror.w	#4,d0		; .. e li sposta nei bit da 12 a 15
	ori.w	#$0B4A,d0	; con un OR ottengo il valore da scrivere
				; in BLTCON0. Con questo valore di LF ($4A)
				; si disegnano linee in EOR con lo sfondo.
				; in BLTCON0. Con questo valore di LF ($4A)
	move.l	a0,a1		; copia l'indirizzo
	cmpi.w	#7,d1		; il numero del bit e` > 7 ?
	ble.s	NonCorreggi	; se no salta
	addq.w	#1,a1		; ..altrimenti punta al prossimo byte
	subq.w	#8,d1		; e rendi il numero del bit < 7
	NonCorreggi:
	not.b	d1		; inverti la numerazione dei bit
				; questa istruzione e` necessaria perche`
				; all'interno del byte i bit sono numerati 
				; da destra a sinistra, mentre le coordinate
				; dei pixel vanno da sinistra a destra
	CLR.W	$100		; DEBUG | w 0 100 2
	bchg	d1,(a1)		; inverti il primo pixel della linea

	move.w	d2,d1		; copia DX in D1
	addq.w	#1,d1		; D1=DX+1
	lsl.w	#$06,d1		; calcola in D1 il valore da mettere in BLTSIZE
	addq.w	#2,d1		; aggiunge la larghezza, pari a 2 words

	lsl.w	#$02,d3		; D3=4*DY
	add.w	d2,d2		; D2=2*DX

	BSR	WaitBlitter
	move.w	d3,BLTBMOD	; BLTBMOD=4*DY
	sub.w	d2,d3		; D3=4*DY-2*DX
	move.w	d3,BLTAPTL	; BLTAPTL=4*DY-2*DX prep val for BLTCON1
	ori.w	#$0003,d5	; setta bit 0 (attiva line-mode), e
	tst.w	d3
	bpl.s	OK1		; se 4*DY-2*DX>0 salta..
	ori.w	#$0040,d5	; altrimenti setta il bit SIGN
	OK1:
	move.w	d0,BLTCON0	; BLTCON0
	move.w	d5,BLTCON1	; BLTCON1
	sub.w	d2,d3		; D3=4*DY-4*DX
	move.w	d3,BLTAMOD	; BLTAMOD=4*DY-4*DX
	move.l	a0,BLTCPTH	; BLTCPT - indirizzo schermo
	move.l	a0,BLTDPTH	; BLTDPT - indirizzo schermo
	move.w	d1,BLTSIZE	; BLTSIZE
	rts

;******************************************************************************
; Questa routine setta i registri del blitter che non devono essere
; cambiati tra una line e l'altra
;******************************************************************************

InitLine:
	BSR	WaitBlitter
	moveq.l	#-1,d5
	move.l	d5,BLTAFWM	; BLTAFWM/BLTALWM = $FFFF
	move.w	#$8000,BLTADAT	; BLTADAT = $8000
	move.w	#bpl,BLTCMOD	; BLTCMOD = 40
	move.w	#bpl,BLTDMOD	; BLTDMOD = 40
	rts

;******************************************************************************
; D0-D1
;******************************************************************************
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

;********** Fastmem Data **********
	INCLUDE	"sincosin_table.i"	; VALUES

P61_LAST_POS:	DC.W MODSTART_POS
P61_DUMMY_POS:	DC.W 0
P61_FRAMECOUNT:	DC.W 0
P61_SEQ_POS:	DC.W 0
P61_DUMMY_SEQPOS:	DC.W 63
ANGLE:		DC.W 0
Z_POS:		DC.W 12
Z_FACT:		DC.W 0
CENTER:		DC.W 0
X_TEMP:		DC.W 0
Y_TEMP:		DC.W 0
XY_INIT:		DC.W 0

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

BITPLANE_PTR:	DC.L TR909+(bpl*h*4);	+(bpl/2)-(LOGOBPL/2)	; bitplane azzerato lowres
DrawBuffer:	DC.L SCREEN2		; pointers to buffers
ViewBuffer:	DC.L SCREEN1		; to be swapped

	SECTION	ChipData,DATA_C	;declared data that must be in chipmem

TR909:		INCBIN "testpic_368x230x5.raw"
;MODULE:		INCBIN "mechmicrobes.P61"	; code $B000

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

	.Palette:
	DC.W $0180,$0000,$0182,$0853,$0184,$0BBB,$0186,$0D61
	DC.W $0188,$0D88,$018A,$0667,$018C,$0556,$018E,$0FFF
	DC.W $0190,$0EEE,$0192,$0DDD,$0194,$0CCD,$0196,$0CCC
	DC.W $0198,$0AAA,$019A,$0999,$019C,$0888,$019E,$0777
	DC.W $01A0,$0555,$01A2,$0444,$01A4,$0FF0,$01A6,$0EEF
	DC.W $01A8,$0BBC,$01AA,$099A,$01AC,$0F0F,$01AE,$00FF
	DC.W $01B0,$0CBA,$01B2,$0CA9,$01B4,$0778,$01B6,$000F
	DC.W $01B8,$00F0,$01BA,$0B00,$01BC,$0632,$01BE,$0F00

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

	; **** COPPERWAITS ****

	.SEQ_LED:
	DC.W $F801,$FF00	; horizontal position masked off
	DC.W $174,$FC00,$176,$FC00	; SPR6DATA
	DC.W $17C,$0000,$17E,$FC00	; SPR7DATA
	DC.W $FA01,$FF00
	DC.W $174,$0000,$176,$0000	; SPR6DATA
	DC.W $17C,$0000,$17E,$0000	; SPR7DATA


	DC.W $FFDF,$FFFE	; allow VPOS>$ff

	DC.W $FFFF,$FFFE	;magic value to end copperlist
_COPPER:

;*******************************************************************************
	SECTION	ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************

BUFFER3D:		DS.B LOGOSIDE*bpl	; bigger to hold zoom
EMPTY:		DS.B LOGOSIDE*bpl	; clear buffer
SCREEN1:		DS.B h*bwid	; Define storage for buffer 1
SCREEN2:		DS.B h*bwid	; two buffers

END
