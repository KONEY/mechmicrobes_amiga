;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/mechmicrobes_amiga/"
	SECTION	"Code",CODE
	INCLUDE	"PhotonsMiniWrapper1.04!.S"
	INCLUDE	"custom-registers.i"	;use if you like ;)
	INCLUDE	"PT12_OPTIONS.i"
	INCLUDE	"P6112-Play-stripped.i"
;********** Constants **********
w=	336		; screen width
h=	256		; screen height
bpls=	4		; depth
bpl=	w/16*2		; byte-width of 1 bitplane line (40bytes)
bwid=	bpls*bpl		; byte-width of 1 pixel line (all bpls)
MARGINX=(320/2)
MARGINY=(256/2)
TrigShift=7
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
	MOVE.W	#%1000011111100000,DMACON	; BIT10=BLIT NASTY
	MOVE.W	MODSTART_POS,D3
	CMP.W	#0,D3
	BEQ.S	.dontDisableBlitterNasty	; IF START > 0 DISABLE BLIT NASTY NOW
	MOVE.W	#%0000010000000000,DMACON	; BIT10=BLIT NASTY DISABLED
	.dontDisableBlitterNasty:
	;*--- clear screens ---*
	LEA	SCREEN1,A1
	BSR.W	ClearScreen
	LEA	SCREEN2,A1
	BSR.W	ClearScreen
	BSR	WaitBlitter
	;*--- start copper ---*
	LEA	SCREEN1,A0
	MOVEQ	#bpl,D0
	LEA	BplPtrs+2,A1
	MOVEQ	#bpls-1,D1
	BSR.W	PokePtrs

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	; ** POINTS TO COORDS **
	MOVE.W	#96-1,D1
	LEA	KONEY,A2
	.calcuCoords:
	MOVE.W	(A2),D0
	MOVE.W	PXLSIDE,D2
	MULU	D2,D0
	MULU	#5,D2
	DIVU	#2,D2
	SUB.W	D2,D0	
	;ADD.W	#MARGIN,D0
	MOVE.W	D0,(A2)+
	DBRA	D1,.calcuCoords
	; ** POINTS TO COORDS **
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	;---  Call P61_Init  ---

	MOVE.L	#COPPER,$80(a6)

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
	lea	BplPtrs+2,a1
	moveq	#bpls-1,d1
	bsr.w	PokePtrs
	;*--- ...draw into the other(a2) ---*
	move.l	a2,a1
	;bsr	ClearScreen

	MOVE.L	BITPLANE,DrawBuffer

	; do stuff here :)

	bsr.w	InitLine		; inizializza line-mode

	;move.w	#$ffff,d0		; linea continua
	;bsr.w	SetPattern	; definisce pattern
	MOVE.W	#$FFFF,BLTBDAT	; BLTBDAT = pattern della linea!

	MOVE.W	#24-1,D7
	LEA	KONEY,A2
	.fetchCoordz:
	MOVEM.L	D7,-(SP)

	MOVE.W	(A2)+,D0		; X1
	MOVE.W	(A2)+,D1		; Y1

	; **** ROTATING??? ****
	MOVE.W	ANGLE,D7
	LEA.L	SinTbl(pc),A0
	MOVE.W	(A0,D7),D3
	LEA.L	CosTbl(pc),A0
	MOVE.W	(A0,D7),D4

	BSR.W	__ROTATE

	MOVEM.L	D0-D1,-(SP)

	MOVE.W	(A2)+,D0		; X2
	MOVE.W	(A2)+,D1		; Y2

	BSR.W	__ROTATE

	MOVE.W	D0,D2		; X2
	MOVE.W	D1,D3		; Y2

	MOVEM.L	(SP)+,D0-D1

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
	;;    ---  Call P61_End  ---
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
	clr.w	$66(a6)		; destination modulo
	move.l	#$01000000,$40(a6)	; set operation type in BLTCON0/1
	move.l	a1,$54(a6)	; destination address
	move.l	#h*bpls*64+bpl/2,$58(a6)	;blitter operation size
	rts

VBint:				; Blank template VERTB interrupt
	movem.l	d0/a6,-(sp)	; Save used registers
	lea	$dff000,a6
	btst	#5,$1f(a6)	; check if it's our vertb int.
	beq.s	.notvb
	;*--- do stuff here ---*
	moveq	#$20,d0		; poll irq bit
	move.w	d0,$9c(a6)
	move.w	d0,$9c(a6)
	.notvb:	
	movem.l	(sp)+,d0/a6	; restore
	rte

;******************************************************************************
; Questa routine effettua il disegno della linea. prende come parametri gli
; estremi della linea P1 e P2, e l'indirizzo del bitplane su cui disegnarla.
; D0 - X1 (coord. X di P1)
; D1 - Y1 (coord. Y di P1)
; D2 - X2 (coord. X di P2)
; D3 - Y2 (coord. Y di P2)
; A0 - indirizzo bitplane
;******************************************************************************
;	    ("`-/")_.-'"``-._
;	     . . `; -._    )-;-,_`)
;	FL  (v_,)'  _  )`-.\  ``-'
;	   _.- _..-_/ / ((.'
;	 ((,.-'   ((,/

Drawline:
	LEA	BITPLANE,A0
	ADD.W	#MARGINX,D0
	ADD.W	#MARGINY,D1
	ADD.W	#MARGINX,D2
	ADD.W	#MARGINY,D3
	; * scelta ottante
	sub.w	d0,d2		; D2=X2-X1
	bmi.s	DRAW4		; se negativo salta, altrimenti D2=DiffX
	sub.w	d1,d3		; D3=Y2-Y1
	bmi.s	DRAW2		; se negativo salta, altrimenti D3=DiffY
	cmp.w	d3,d2		; confronta DiffX e DiffY
	bmi.s	DRAW1		; se D2<D3 salta..
				; .. altrimenti D3=DY e D2=DX
	moveq	#$10,d5		; codice ottante
	bra.s	DRAWL
DRAW1:
	exg.l	d2,d3		; scambia D2 e D3, in modo che D3=DY e D2=DX
	moveq	#0,d5		; codice ottante
	bra.s	DRAWL
DRAW2:
	neg.w	d3		; rende D3 positivo
	cmp.w	d3,d2		; confronta DiffX e DiffY
	bmi.s	DRAW3		; se D2<D3 salta..
				; .. altrimenti D3=DY e D2=DX
	moveq	#$18,d5		; codice ottante
	bra.s	DRAWL
DRAW3:
	exg.l	d2,d3		; scambia D2 e D3, in modo che D3=DY e D2=DX
	moveq	#$04,d5		; codice ottante
	bra.s	DRAWL
DRAW4:
	neg.w	d2		; rende D2 positivo
	sub.w	d1,d3		; D3=Y2-Y1
	bmi.s	DRAW6		; se negativo salta, altrimenti D3=DiffY
	cmp.w	d3,d2		; confronta DiffX e DiffY
	bmi.s	DRAW5		; se D2<D3 salta..
				; .. altrimenti D3=DY e D2=DX
	moveq	#$14,d5		; codice ottante
	bra.s	DRAWL
DRAW5:
	exg.l	d2,d3		; scambia D2 e D3, in modo che D3=DY e D2=DX
	moveq	#$08,d5		; codice ottante
	bra.s	DRAWL
DRAW6:
	neg.w	d3		; rende D3 positivo
	cmp.w	d3,d2		; confronta DiffX e DiffY
	bmi.s	DRAW7		; se D2<D3 salta..
				; .. altrimenti D3=DY e D2=DX
	moveq	#$1c,d5		; codice ottante
	bra.s	DRAWL
DRAW7:
	exg.l	d2,d3		; scambia D2 e D3, in modo che D3=DY e D2=DX
	moveq	#$0c,d5		; codice ottante

; Quando l'esecuzione raggiunge questo punto, abbiamo:
; D2 = DX
; D3 = DY
; D5 = codice ottante

DRAWL:
	mulu.w	#40,d1		; offset Y
	add.l	d1,a0		; aggiunge l'offset Y all'indirizzo

	move.w	d0,d1		; copia la coordinata X
	and.w	#$000F,d0	; seleziona i 4 bit piu` bassi della X..
	ror.w	#4,d0		; .. e li sposta nei bit da 12 a 15
	or.w	#$0BCA,d0	; con un OR ottengo il valore da scrivere
				; in BLTCON0. Con questo valore di LF ($4A)
				; si disegnano linee in EOR con lo sfondo.

	lsr.w	#4,d1		; cancella i 4 bit bassi della X
	add.w	d1,d1		; ottiene l'offset X in bytes
	add.w	d1,a0		; aggiunge l'offset X all'indirizzo

	move.w	d2,d1		; copia DX in D1
	addq.w	#1,d1		; D1=DX+1
	lsl.w	#$06,d1		; calcola in D1 il valore da mettere in BLTSIZE
	addq.w	#$0002,d1		; aggiunge la larghezza, pari a 2 words

	lsl.w	#$02,d3		; D3=4*DY
	add.w	d2,d2		; D2=2*DX

	move.w	d3,BLTBMOD	; BLTBMOD=4*DY
	sub.w	d2,d3		; D3=4*DY-2*DX
	move.w	d3,BLTAPTL	; BLTAPTL=4*DY-2*DX
				; prepara valore da scrivere in BLTCON1
	or.w	#$0001,d5		; setta bit 0 (attiva line-mode)
	tst.w	d3
	bpl.s	OK1		; se 4*DY-2*DX>0 salta..
	or.w	#$0040,d5	; altrimenti setta il bit SIGN
OK1:
	move.w	d0,$40(a5)	; BLTCON0
	move.w	d5,$42(a5)	; BLTCON1
	sub.w	d2,d3		; D3=4*DY-4*DX
	move.w	d3,$64(a5)	; BLTAMOD=4*DY-4*DX
	move.l	a0,$48(a5)	; BLTCPT - indirizzo schermo
	move.l	a0,$54(a5)	; BLTDPT - indirizzo schermo
	move.w	d1,$58(a5)	; BLTSIZE
	rts

;******************************************************************************
; Questa routine setta i registri del blitter che non devono essere
; cambiati tra una line e l'altra
;******************************************************************************

InitLine:
	moveq.l	#-1,d5
	move.l	d5,BLTAFWM	; BLTAFWM/BLTALWM = $FFFF
	move.w	#$8000,BLTADAT	; BLTADAT = $8000
	move.w	#40,BLTCMOD	; BLTCMOD = 40
	move.w	#40,BLTDMOD	; BLTDMOD = 40
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

;****************************************************************************
;********** Fastmem Data **********
	INCLUDE	"sincosin_table.i"	; VALUES

ANGLE:	DC.W 90
PXLSIDE:	DC.W 8

KONEY:	; ROTATED 90 DEG
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

DrawBuffer:	DC.L SCREEN2	; pointers to buffers
ViewBuffer:	DC.L SCREEN1	; to be swapped

	SECTION	ChipData,DATA_C	;declared data that must be in chipmem

COPPER:
	DC.W $1FC,0	; Slow fetch mode, remove if AGA demo.
	DC.W $8E,$2C81	; 238h display window top, left | DIWSTRT - 11.393
	DC.W $90,$2CC1	; and bottom, right.	| DIWSTOP - 11.457
	DC.W $92,$38	; Standard bitplane dma fetch start
	DC.W $94,$D0	; and stop for standard screen.

	DC.W $106,$0C00	; (AGA compat. if any Dual Playf. mode)
	
	DC.W $108	; BPL1MOD	 Bitplane modulo (odd planes)
	BPL1MOD:
	DC.W 2		; bwid-bpl	;modulos
	
	DC.W $10A	; BPL2MOD Bitplane modulo (even planes)
	BPL2MOD:
	DC.W 2		; bwid-bpl	;RISULTATO = 80 ?
	
	DC.W $102,0	; SCROLL REGISTER (AND PLAYFIELD PRI)

	Palette:
	DC.W $0180,$0000,$0182,$0334,$0184,$0445,$0186,$0556
	DC.W $0188,$0667,$018A,$0333,$018C,$0667,$018E,$0777
	DC.W $0190,$0888,$0192,$0888,$0194,$0999,$0196,$0AAA
	DC.W $0198,$0BBB,$019A,$0CCC,$019C,$0DDD,$019E,$0FFF

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

	DC.W $1A6
	LOGOCOL1:
	DC.W $000	; COLOR0-1
	DC.W $1AE
	LOGOCOL2:
	DC.W $000	; COLOR2-3
	DC.W $1B6
	LOGOCOL3:
	DC.W $000	; COLOR4-5

	COPPERWAITS:
	; HW DISPLACEMENT
	;DC.W $002D,$FFFE
	;DC.W $F102,$A68E
	;DC.W $FE07,$FFFE
	;DC.W $F102,$1F83

	;DC.W $FE07,$FFFE
	;DC.W $0180,$0FFF
	;DC.W $FF07,$FFFE
	;DC.W $0180,$0011	; SCROLLAREA BG COLOR
	;DC.W $0182,$0AAA	; SCROLLING TEXT WHITE ON

	DC.W $FFDF,$FFFE	; allow VPOS>$ff

	DC.W $2201,$FF00	; horizontal position masked off
	DC.W $0108,$0002	; BPL1MOD - Should fix the scroll
	DC.W $010A,$0002	; BPL2MOD - when glitch but dont work :)
	DC.W $0188,$0FFF	; BG COLOR
	
	DC.W $2301,$FF00	; horizontal position masked off
	DC.W $0188,$0DDD	; BG COLOR
	DC.W $0198,$0000	; TXT COLOR
	
	;DC.W $2401,$FF00	; horizontal position masked off
	;DC.W $0198,$0222	; TXT COLOR
	
	DC.W $2501,$FF00	; horizontal position masked off
	DC.W $0188,$0AAA	; BG COLOR
	;DC.W $0198,$0444	; TXT COLOR

	;DC.W $2601,$FF00	; horizontal position masked off
	;DC.W $0198,$0333	; TXT COLOR

	DC.W $2701,$FF00	; horizontal position masked off
	DC.W $0188,$0888	; BG COLOR
	;DC.W $0198,$0666	; TXT COLOR

	;DC.W $2801,$FF00	; horizontal position masked off
	;DC.W $0198,$0999	; TXT COLOR

	DC.W $2901,$FF00	; horizontal position masked off
	DC.W $0188,$0555	; BG COLOR
	;DC.W $0198,$0AAA	; TXT COLOR

	DC.W $2A01,$FF00	; horizontal position masked off
	DC.W $0188,$0333	; BG COLOR
	;DC.W $0198,$0EEE	; TXT COLOR

	DC.W $2B01,$FF00	; RESTORE BLACK
	DC.W $0188,$0000

	DC.W $FFFF,$FFFE	;magic value to end copperlist
_COPPER:

;*******************************************************************************
	SECTION	ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************

BITPLANE:		DS.B h*bwid	; bitplane azzerato lowres
SCREEN1:		DS.B h*bwid	; Define storage for buffer 1
SCREEN2:		DS.B h*bwid	; two buffers

END