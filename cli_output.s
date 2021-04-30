; * CLI output module
; * ----------------------------------------------------

;_SysBase	equ 4		; Base di Exec
;_LVOOpenLibrary	equ -552		; Apertura Library
;_LVOOutput	equ -60		; DOS: prelevamento Handle di Output
;_LVOWrite	equ -48		; Output

; *Apertura di DOS/Lib:
type_end_txt:

	move.l	#dosname,a1	; Nome della DOS-Lib
	moveq	#0,d0		; Versione indifferente
	move.l	4.w,a6		; Base di Exec
	jsr	-552(a6)		; Apertura di DOS-Lib
	tst.l	d0		; Errore?
	beq	fini		; Se errore, Fine
	move.l	d0,_DOSBase	; Annotare il puntatore

; * Determinazione dell'Handle di output:
	move.l	_DOSBase,a6	; Chiamata Funzione DOS
	jsr	-60(a6)		; Prelevamento Handle di Output
	move.l	d0,d4		; e sua annotazione in d4

; * ora Output di testo:
	move.l	d4,d1		; Handle di Output
	move.l	#END_TEXT,d2	; Indirizzo del Testo
	CLR.L	D3
	move.w	END_TEXT_LEN,d3		; Lunghezza del testo
	move.l	_DOSBase,a6	; DOS di base
	jsr	-48(a6)		; Funzione "Scrittura"

; * Al termine chiudere sempre le Lib!

	move.l	_DOSBase,a1	; Base della Lib
	move.l	4.w,a6		; Base di Exec
	jsr	-414(a6)		; Funzione "Chiusura"

fini:	rts			; Ritorno al CLI

; * Campo Dati:

_DOSBase:	dc.l 0

dosname:	dc.b "dos.library",0
	EVEN