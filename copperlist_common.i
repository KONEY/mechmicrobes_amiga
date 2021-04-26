	DC.W COP1LCH		; AREA TO POKE VALUES
	.CopJumpH:		; FOR THE TWO COPPERLISTS
	DC.W $FFFF
	DC.W COP1LCL
	.CopJumpL:
	DC.W $FFFF

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
	DC.W $0180,$0000,$0182,$0853,$0184,$0BBB,$0186,$0D61
	DC.W $0188,$0D88,$018A,$0667,$018C,$0556,$018E,$0FFF
	DC.W $0190,$0EEE,$0192,$0DDD,$0194,$0CCD,$0196,$0CCC
	DC.W $0198,$0AAA,$019A,$0999,$019C,$0888,$019E,$0777
	DC.W $01A0,$0555,$01A2,$0444,$01A4,$0FF0,$01A6,$0EEF
	DC.W $01A8,$0BBC,$01AA,$099A,$01AC,$0F0F,$01AE,$00FF
	DC.W $01B0,$0CBA,$01B2,$0CA9,$01B4,$0778,$01B6,$000F
	DC.W $01B8,$00F0,$01BA,$0B00,$01BC,$0632,$01BE,$0F00

	.Waits:
	; LOGO_COLORS
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

	; SEQ_LEDs
	DC.W $F801,$FF00			; horizontal position masked off
	DC.W $0174,$FC00,$0176,$FC00	; SPR6DATA
	DC.W $017C,$0000,$017E,$FC00	; SPR7DATA
	DC.W $FA01,$FF00
	DC.W $0174,$0000,$0176,$0000	; SPR6DATA
	DC.W $017C,$0000,$017E,$0000	; SPR7DATA

	DC.W $FF01,$FF00		; horizontal position masked off
	DC.W $018E,$0BBB		; SCROLLTEXT - $0D61

	DC.W $FFDF,$FFFE		; allow VPOS>$ff

	DC.W $FFFF,$FFFE		; magic value to end copperlist