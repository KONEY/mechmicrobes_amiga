;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/mechmicrobes_amiga/"
	SECTION	"Code",CODE
	INCLUDE	"PhotonsMiniWrapper1.04!.S"
	INCLUDE	"custom-registers.i"	;use if you like ;)
	INCLUDE	"cli_output.s"
	INCLUDE	"PT12_OPTIONS.i"
	INCLUDE	"P6112-Play-stripped.i"
;********** Constants **********
w=368		;screen width, height, depth
h=230
bpls=5		;handy values:
bpl=w/16*2	;byte-width of 1 bitplane line (40)
bwid=bpls*bpl	;byte-width of 1 pixel line (all bpls)
MARGINX=(w/2)
MARGINY=(h/2)
TrigShift=7
PXLSIDE=16
Z_Shift=PXLSIDE*5/2	; 5x5 obj
;*************
MODSTART_POS=1		; start music at position # !! MUST BE EVEN FOR 16BIT
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
	BSR.W	ClearScreen
	LEA	SCREEN2,A1
	BSR.W	ClearScreen
	BSR	WaitBlitter
	;*--- start copper ---*
	move.l	DrawBuffer,A0
	MOVEQ	#bpl,D0
	LEA	BplPtrs+2,A1
	MOVEQ	#0,D1
	BSR.W	PokePtrs

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
	
	;MOVE.W	#Z_Shift,D1
	;MOVE.W	D1,D7
	;LSL.W	#5,D1
	;LSL.W	#3,D7
	;ADD.W	D7,D1

	MOVE.L	#COPPER,$DFF080	; ATTACH THE COPPER
;********************  main loop  ********************
MainLoop:
	move.w	#$12c,d0		;No buffering, so wait until raster
	bsr.w	WaitRaster	;is below the Display Window.
	;*--- swap buffers ---*
	;movem.l	DrawBuffer(PC),a2-a3
	;exg	a2,a3
	;movem.l	a2-a3,DrawBuffer	;draw into a2, show a3
	;*--- show one... ---*
	;move.l	a3,a0
	;move.l	#bpl*h,d0
	;lea	BplPtrs+2,a1
	;moveq	#bpls-1,d1
	;bsr.w	PokePtrs
	;*--- ...draw into the other(a2) ---*
	;move.l	DrawBuffer,a1
	bsr	ClearBuffer2
	;MOVE.L	BITPLANE_PTR,DrawBuffer
	;MOVE.L	#TR909,DrawBuffer

	; do stuff here :)

	; **** JOYSTICK TEST ****
	MOVEM.W	$DFF00C,D0	; FROM EAB
	ANDI.W	#$0303,D0
	MOVE.W	D0,D1
	ADD.W	D1,D1
	ADDI.W	#$0101,D0
	ADD.W	D1,D0
	BTST	#9,D0		; 9 LEFT
	BEQ.S	.notLeft
	SUBI.W	#8,ANGLE
	.notLeft:
	BTST	#1,D0		; 1 RIGHT
	BEQ.S	.notRight
	TST.W	JOYDIR_STATUS
	BNE.W	.SkipJoyActions
	MOVE.W	#1,JOYDIR_STATUS
	ADDI.L	#2,KONEY_PTR	; MESSES COORDZ
	.notRight:
	BTST	#10,D0		; 10 UP
	BEQ.S	.notDown
	SUBI.W	#2,Z_POS
	.notDown:
	BTST	#2,D0		; 2 DOWN
	BEQ.S	.notUp
	ADDI.W	#2,Z_POS
	.notUp:

	.SkipJoyActions:
	CMPI.W	#$00000101,D0
	BNE.S	.DontResetStatus
	MOVE.W	#0,JOYDIR_STATUS
	.DontResetStatus:
	; **** JOYSTICK TEST ****

	;CLR.W	$100		; DEBUG | w 0 100 2

	; **** ROTATION VALUES ****
	MOVE.W	ANGLE,D2
	ADDI.W	#6,D2
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

	bsr.w	InitLine		; inizializza line-mode
	MOVE.W	#$FFFF,BLTBDAT	; BLTBDAT = pattern della linea!
	;MOVE.B	$DFF006,BLTBDAT

	MOVEQ	#23,D7
	MOVE.W	#0,XY_INIT
	MOVE.L	KONEY_PTR,A2

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

	;LSR.L	#1,D0		; GLITCH
	ROL.B	#1,D3
	EXG.L	D0,D3
	ROL.B	#1,D1		; GLITCH
	LSL.L	#2,D2		; GLITCH
	;EXG.L	D1,D2

	BSR.W	Drawline

	MOVEM.L	(SP)+,D7
	DBRA	D7,.fetchCoordz

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
	move.l	DrawBuffer,BLTDPTH		; destination address
	MOVE.W	#(h*64)+(w/16),BLTSIZE	; Start Blitter (Blitsize)
	rts
ClearBuffer2:
	bsr	WaitBlitter
	MOVE.W	#$09f0,BLTCON0		; A**,Shift 0, A -> D
	MOVE.W	#0,BLTCON1		; Everything Normal
	MOVE.L	#0,BLTAMOD		; Init modulo Sou. A
	MOVE.W	#0,BLTDMOD		; Init modulo Dest D
	MOVE.L	#Empty,BLTAPTH		; Source
	MOVE.L	DrawBuffer,BLTDPTH		; Dest
	MOVE.W	#h*64+w/16,BLTSIZE		; Start Blitter (Blitsize)
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
	;LEA	BITPLANE,A0
	MOVE.L	DrawBuffer,A0
	ADDI.W	#MARGINX,D0
	ADDI.W	#MARGINY,D1
	ADDI.W	#MARGINX,D2
	ADDI.W	#MARGINY,D3

	; * scelta ottante
	sub.w	d0,d2		; D2=X2-X1
	bmi.s	.DRAW4		; se negativo salta, altrimenti D2=DiffX
	sub.w	d1,d3		; D3=Y2-Y1
	bmi.s	.DRAW2		; se negativo salta, altrimenti D3=DiffY
	cmp.w	d3,d2		; confronta DiffX e DiffY
	bmi.s	.DRAW1		; se D2<D3 salta..
				; .. altrimenti D3=DY e D2=DX
	moveq	#$10,d5		; codice ottante
	bra.s	.DRAWL
	.DRAW1:
	exg.l	d2,d3		; scambia D2 e D3, in modo che D3=DY e D2=DX
	moveq	#0,d5		; codice ottante
	bra.s	.DRAWL
	.DRAW2:
	neg.w	d3		; rende D3 positivo
	cmp.w	d3,d2		; confronta DiffX e DiffY
	bmi.s	.DRAW3		; se D2<D3 salta..
				; .. altrimenti D3=DY e D2=DX
	moveq	#$18,d5		; codice ottante
	bra.s	.DRAWL
	.DRAW3:
	exg.l	d2,d3		; scambia D2 e D3, in modo che D3=DY e D2=DX
	moveq	#$04,d5		; codice ottante
	bra.s	.DRAWL
	.DRAW4:
	neg.w	d2		; rende D2 positivo
	sub.w	d1,d3		; D3=Y2-Y1
	bmi.s	.DRAW6		; se negativo salta, altrimenti D3=DiffY
	cmp.w	d3,d2		; confronta DiffX e DiffY
	bmi.s	.DRAW5		; se D2<D3 salta..
				; .. altrimenti D3=DY e D2=DX
	moveq	#$14,d5		; codice ottante
	bra.s	.DRAWL
	.DRAW5:
	exg.l	d2,d3		; scambia D2 e D3, in modo che D3=DY e D2=DX
	moveq	#$08,d5		; codice ottante
	bra.s	.DRAWL
	.DRAW6:
	neg.w	d3		; rende D3 positivo
	cmp.w	d3,d2		; confronta DiffX e DiffY
	bmi.s	.DRAW7		; se D2<D3 salta..
				; .. altrimenti D3=DY e D2=DX
	moveq	#$1c,d5		; codice ottante
	bra.s	.DRAWL
	.DRAW7:
	exg.l	d2,d3		; scambia D2 e D3, in modo che D3=DY e D2=DX
	moveq	#$0c,d5		; codice ottante

	; Quando l'esecuzione raggiunge questo punto, abbiamo:
	; D2 = DX
	; D3 = DY
	; D5 = codice ottante

	.DRAWL:
	;MOVE.W	D1,D4		; OPTIMIZING mulu.w #40,d1
	;LSL.W	#5,D1
	;LSL.W	#3,D4
	;ADD.W	D4,D1
	mulu.w	#bpl,d1		; offset Y
	add.w	d1,a0		; aggiunge l'offset Y all'indirizzo

	move.w	d0,d1		; copia la coordinata X
	andi.w	#$000F,d0		; seleziona i 4 bit piu` bassi della X..
	ror.w	#4,d0		; .. e li sposta nei bit da 12 a 15
	ori.w	#$0BCA,d0		; con un OR ottengo il valore da scrivere
				; in BLTCON0. Con questo valore di LF ($4A)
				; si disegnano linee in EOR con lo sfondo.
				; #$0BCA

	lsr.w	#4,d1		; cancella i 4 bit bassi della X
	add.w	d1,d1		; ottiene l'offset X in bytes
	add.w	d1,a0		; aggiunge l'offset X all'indirizzo

	move.w	d2,d1		; copia DX in D1
	addq.w	#1,d1		; D1=DX+1
	lsl.w	#$06,d1		; calcola in D1 il valore da mettere in BLTSIZE
	addq.w	#$0002,d1		; aggiunge la larghezza, pari a 2 words

	lsl.w	#$02,d3		; D3=4*DY
	add.w	d2,d2		; D2=2*DX
	BSR	WaitBlitter
	move.w	d3,BLTBMOD	; BLTBMOD=4*DY
	sub.w	d2,d3		; D3=4*DY-2*DX
	move.w	d3,BLTAPTL	; BLTAPTL=4*DY-2*DX prep val for BLTCON1
	ori.w	#$0001,d5		; setta bit 0 (attiva line-mode)
	tst.w	d3
	bpl.s	OK1		; se 4*DY-2*DX>0 salta..
	ori.w	#$0040,d5		; altrimenti setta il bit SIGN
	OK1:
	move.w	d0,BLTCON0	; BLTCON0
	move.w	d5,BLTCON1	; BLTCON1
	sub.w	d2,d3		; D3=4*DY-4*DX
	move.w	d3,BLTAMOD	; BLTAMOD=4*DY-4*DX
	move.l	a0,BLTCPTH	; BLTCPT - indirizzo schermo
	move.l	a0,BLTDPTH	; BLTDPT - indirizzo schermo
	move.w	d1,BLTSIZE	; BLTSIZE
	rts

DrawlineFill:
	MOVE.L	DrawBuffer,A0	; ROUTINE STOLEN FROM RAM_JAM
	ADDI.W	#MARGINX,D0
	ADDI.W	#MARGINY,D1
	ADDI.W	#MARGINX,D2
	ADDI.W	#MARGINY,D3

	sub.w	d1,d3		; D3=Y2-Y1
	beq.s	.skip		; per il fill non servono linee orizzontali 
	bgt.s	.y2gy1		; salta se positivo..
	exg	d0,d2		; ..altrimenti scambia i punti
	add.w	d3,d1		; mette in D1 la Y piu` piccola
	neg.w	d3		; D3=DY
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
	ori.w	#$0B4A,d4		; aggiunge l'opportuno
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

;******************************************************************************
; Questa routine setta i registri del blitter che non devono essere
; cambiati tra una line e l'altra
;******************************************************************************

InitLine:
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

;********** Fastmem Data **********
	INCLUDE	"sincosin_table.i"	; VALUES

TOP_MARGIN:	DC.W MARGINY-16
ANGLE:		DC.W 0
Z_POS:		DC.W 32
Z_FACT:		DC.W Z_Shift
CENTER:		DC.W Z_Shift
X_TEMP:		DC.W 0
Y_TEMP:		DC.W 0
XY_INIT:		DC.W 0
END_TEXT_LEN:	DC.W 152
JOYDIR_STATUS:	DC.W 0

END_TEXT:		DC.B "THANKS FOR EXECUTING MECHMICROBES BY KONEY!",10
		DC.B "YOU REACHED BLOCK "
		TXT_POS: DC.B "XX"
		DC.B " FROM A SEQUENCE OF 76. ",10
		DC.B "VISIT WWW.KONEY.ORG FOR MORE TECHNO "
		DC.B "AND HARDCORE AMIGA STUFF!",10
		EVEN

KONEY:			; ROTATED 90 DEG
		DC.W 0,0,1,0
		DC.W 1,0,1,1
		DC.W 1,1,2,1
		DC.W 2,1,2,2
		DC.W 2,2,3,2
		DC.W 3,2,3,1
		DC.W 3,1,4,1
		DC.W 4,1,4,0
		DC.W 4,0,5,0
		DC.W 5,0,5,1
		DC.W 5,1,4,1
		DC.W 4,1,4,2
		DC.W 4,2,3,2
		DC.W 3,2,3,3
		DC.W 3,3,5,3
		DC.W 5,3,5,5
		DC.W 5,5,0,5
		DC.W 0,5,0,4
		DC.W 0,4,2,4
		DC.W 2,4,2,2
		DC.W 2,2,1,2
		DC.W 1,2,1,1
		DC.W 1,1,0,1
		DC.W 0,1,0,0

KONEY_PTR:	DC.L KONEY_OPT
KONEY_OPT:		; OPTIMIZED
		DC.W 0,0,0,5
		DC.W 0,5,5,5
		DC.W 5,5,4,0
		DC.W 4,0,0,1
		DC.W 0,1,1,4
		DC.W 1,4,4,4
		DC.W 4,4,4,2
		DC.W 4,2,2,2
		DC.W 2,2,2,3
		DC.W 2,3,3,3
		DC.W 3,3,3,2
		DC.W 3,5,4,1
		DC.W 4,1,5,0
		DC.W 5,0,5,1
		DC.W 5,1,1,1
		DC.W 1,1,1,2
		DC.W 1,2,5,2
		DC.W 5,2,5,3
		DC.W 5,3,1,3
		DC.W 1,3,5,4
		DC.W 5,1,5,2
		DC.W 5,3,2,5
		DC.W 5,5,1,5
		DC.W 1,5,3,0

		DC.W 2,0,5,3
		DC.W 5,3,4,1
		DC.W 4,1,3,4
		DC.W 3,4,3,0
		DC.W 2,4,2,2
		DC.W 1,1,2,1
		DC.W 4,1,4,0
		DC.W 5,5,0,5
		DC.W 0,5,2,1
		DC.W 3,1,4,1
		DC.W 2,1,2,2
		DC.W 1,0,0,3


BITPLANE_PTR:	DC.L BITPLANE	; bitplane azzerato lowres
DrawBuffer:	DC.L SCREEN1	; pointers to buffers
ViewBuffer:	DC.L BITPLANE	; to be swapped

	SECTION	ChipData,DATA_C	;declared data that must be in chipmem

TR909:	INCBIN "TR-909_368x230x5.raw"

COPPER:
	DC.W $1FC,0	; Slow fetch mode, remove if AGA demo.
	DC.W $8E,$3061	; 238h display window top, left | DIWSTRT - 11.393
	DC.W $90,$16D1	; and bottom, right.	| DIWSTOP - 11.457
	DC.W $92,$28	; Standard bitplane dma fetch start
	DC.W $94,$D8	; and stop eab.abime.net/showthread.php?t=69926

	DC.W $106,$0C00	; (AGA compat. if any Dual Playf. mode)
	DC.W $108,0	; bwid-bpl	;modulos
	DC.W $10A,0	; bwid-bpl	;RISULTATO = 80 ?

	DC.W $102,0	; SCROLL REGISTER (AND PLAYFIELD PRI)

	Palette:
	DC.W $0180,$0000,$0182,$0DDC,$0184,$0A98,$0186,$0ABA
	DC.W $0188,$0E93,$018A,$0211,$018C,$0D61,$018E,$0898
	DC.W $0190,$0EEE,$0192,$0FFE,$0194,$0CCC,$0196,$0EC8
	DC.W $0198,$0BB9,$019A,$0EEE,$019C,$0DDD,$019E,$0888
	DC.W $01A0,$0853,$01A2,$0556,$01A4,$0777,$01A6,$0632
	DC.W $01A8,$0DDD,$01AA,$0FFF,$01AC,$0CCB,$01AE,$0DDC
	DC.W $01B0,$0223,$01B2,$0BBB,$01B4,$0676,$01B6,$0999
	DC.W $01B8,$0AAA,$01BA,$0887,$01BC,$0B00,$01BE,$0F00

	BplPtrs:
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
	DC.W $F6,0		;full 6 ptrs, in case you increase bpls
	DC.W $100,bpls*$1000+$200	;enable bitplanes

	SpritePointers:
	DC.W $120,0,$122,0	; 0
	DC.W $124,0,$126,0	; 1
	DC.W $128,0,$12A,0	; 2
	DC.W $12C,0,$12E,0	; 3
	DC.W $130,0,$132,0	; 4
	DC.W $134,0,$136,0	; 5
	DC.W $138,0,$13A,0	; 6
	DC.W $13C,0,$13E,0	; 7

	COPPERWAITS:
	DC.W $FFDF,$FFFE	; allow VPOS>$ff

	DC.W $FFFF,$FFFE	;magic value to end copperlist
_COPPER:

;*******************************************************************************
	SECTION	ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************

BITPLANE:		DS.B h*bwid	; bitplane azzerato lowres
EMPTY:		DS.B h*bpl
SCREEN1:		DS.B h*bwid	; Define storage for buffer 1
SCREEN2:		DS.B h*bwid	; two buffers

END