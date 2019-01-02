COMMENT $

   ûirogen's VPCSCAN v293 Signature Extractor
   ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
   The source code should be pretty easy to read and understand, for
   future versions of VPCSCAN, change the offsets below to match the
   locations of the names&signatures.

   version 1 - 08-23-94 - original release

$
segment	cseg
assume	cs:	cseg, ds: cseg, es: cseg, ss: cseg

OFS_1   equ 24462h              ; offset of normal sigs
OFS_2   equ 31F38h              ; ..ending
OFS_3   equ 3CFD5h              ; offset of wildcard sigs
OFS_4   equ 3EAE5h              ; ..ending

org	100h
start:

        lea     dx,intro_msg    ; display intro..
	call	disp

	mov	ah,3dh		; open phile
	lea	dx,fname
	int	21h
	jnc	open_ok
	jmp	error_open	; if error abort..
open_ok:
        xchg    ax,bx
        mov     dx,(OFS_1 and 0FFFFh)
        mov     cx,2             ; assume high byte=2
        call    pfile            ; set fp to first set of sigs
        lea     dx,buffer        ; buffer is heap
        push    dx
        mov     cx,(OFS_2-OFS_1)+1
        call    rfile            ; read first set of sigz
        add     dx,ax            ; get offset buffer+bytes read
        push    dx
        mov     dx,(OFS_3 and 0FFFFh)
        mov     cx,3
        call    pfile            ; set fp to next set of sigs
        pop     dx
        mov     cx,(OFS_4-OFS_3)
        call    rfile            ; append 2nd sigs to 1st sigs in heap
        xchg    dx,si
        add     si,ax
        mov     word ptr [si],0FFFFh ; end marker
        mov     ah,3eh          ; close phile
	int	21h

        pop     si
mloop:
        lea     dx,pair         ; lf/cr
        call    disp
        cmp     v_s,0           ; processing virus name, or sig?
        jnz     mloop2
        lea     dx,v_stg        ; display string if virus
d_set:  call    disp
mloop2:
        mov     bh,byte ptr [si]
        cmp     v_s,0
        jz      no_hex          ; if virus name, then ascii display
        call    byte_disp       ; if sig display hex digits
        jmp     d_hex
no_hex: xchg    bh,dl
        call    disp_one        ; display char
d_hex:
        inc     si              ; increment ptr
        cmp     byte ptr [si],0 ; possible end of field?
        jnz     mloop2          ; if not then continue reading field
        cmp     word ptr [si+1],0FFFFh ; end of all sigs?
        jz      exit
no_end: cmp     v_s,0           ; if virus name, then no logic check
        jz      no_chk
        mov     cx,3            ; logic check uses 3 bytes
        mov     bh,30h          ; decrease range for first char only
        mov     di,si           ; use di for ptr
        inc     di
        cmp     byte ptr [si+3],0 ; if 2 byte virus name, then count=2
        jnz     ischar_loop
        dec     cx
ischar_loop:                    ; logic loop to determine if end of sig field
        mov     al,byte ptr [di] ; get byte
        cmp     ax,'u&'         ; fix #1  (ah=last byte loaded)
        jz      mloop2
        cmp     ax,'vk'         ; fix #2
        jz      mloop2
        cmp     ax,'7g'         ; fix #3
        jz      mloop2
        cmp     al,3Ah          ; if in this range, then next field !name
        jl      not_mid
        cmp     al,40h
        jle     mloop2
not_mid:                        ; if in this range, then next field !name
        cmp     al,5Bh
        jl      not_mid2
        cmp     al,5Eh
        jle     mloop2
not_mid2:
        cmp     al,20h          ; skip <space>
        jz      nxt_chk
        cmp     al,21h          ; skip '!'
        jz      nxt_chk
        cmp     al,25h          ; skip '%'
        jz      nxt_chk
        cmp     al,26h          ; skip '&'
        jz      nxt_chk
        cmp     al,2Dh          ; skip '-'
        jz      nxt_chk
        cmp     al,7Ah          ; if !alphanumeric then !name
        jg      mloop2
        cmp     al,bh           ; if !alphanumeric then !name
        jl      mloop2
        mov     bh,2Dh          ; extend range for last two chars
nxt_chk:inc     di
        mov     ah,al           ; ah=last byte loaded
        loop    ischar_loop
no_chk:
        not     v_s             ; if !0 then signature
        inc     si              ; increment pointer [skip null]
        cmp     word ptr [si],0
        jz      exit
not_end:
        jmp     mloop           ; process next field

exit:
        ret

error_open:                     ; error dude..
        lea     dx,error_msg
disp:
	mov	ah,9
	int	21h
	xor	ax,ax
	ret



byte_disp: 			; display hex->ascii byte, bh=byte
        cmp     last,'|'
        jnz     no_wcard
        cmp     bh,'?'
        jnz     no_wcard
        xchg    dl,bh
        call    disp_one
        jmp     _ret
no_wcard:
        cmp     bh,'|'
        jnz     no_wcard2
        cmp     byte ptr [si+1],'?'
        jz      _ret2
no_wcard2:
        dec     bh
        mov     ch,2            ; two ascii chars, two hex nibbles
bloop:
	mov	cl,4
	rol	bx,cl		; rotateL 4 bits, setup next nibble
	mov	dl,bl
	and	dl,0Fh		; kill other nibble
	add	dl,30h		; +30=3xh -> ascii numeric digit
	cmp	dl,3Ah		; numeric or alphabetic?
	jl	no_add		;
	add	dl,7		; add if alphabetic
no_add:
        call    disp_one
        dec     ch              ; decrement counter
	jnz	bloop
_ret:
        mov     dl,20h
        call    disp_one
_ret2:  mov     last,bh         ; bh ! char if ! '|'
        ret

disp_one:                       ; display one char
        mov     ah,2
        int     21h
        ret

rfile:                          ; read phile
        mov      ah,3fh
        int      21h
        ret

pfile:                          ; position fp
        mov     ax,4200h
        int     21h
        ret

last    db      0
fname   db      'VPCSCAN.EXE',0
error_msg db    0Dh,0Ah,'Error opening VPCSCAN.EXE!','$'
intro_msg db    0Dh,0Ah,"  ûirogen's VPCSCAN v293 Signature Extractor - Coded by ûirogen"
        db      0Dh,0Ah,'                       ş 8-23-94 v1.0 ş'
        db      0Dh,0Ah,'           The Adjacent Reality BBS [615].586.9515'
	db	0Dh,0Ah,'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ'
pair	db	0Dh,0Ah,'$'
v_stg   db      'ÀÄ ','$'
v_s     db      0FFh                  ; 0 if name, !0 if sig being processed
buffer:

cseg	ends
        end     start
