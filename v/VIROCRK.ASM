
;
; VIROCRK v2.0, (c)1995 ûirogen [NuKE].
; ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
; Sorry, didn't feel like commenting this. You see, if I go through
; the code and comment it then I'll no-doubt end up re-writing half
; of it also, and I just haven't got the time.
;
;

segment     cseg
            assume  cs: cseg, ds: cseg, es: cseg, ss: cseg

lf          equ     0ah
cr          equ     0dh

org         100h
start:
            lea     dx,intro
            call    disp

            cmp     byte ptr ds: [80h],0
            jnz     is_cmd_line
            lea     dx,no_cmd_line
            call    disp
            ret

is_cmd_line:
            mov     di,82h
            mov     al,' '
            mov     cx,0FFh
            repnz   scasb
            mov     si,di
            dec     di
            mov     byte ptr [di],0
            lea     di,plain_text
            mov     cx,0FFh
copy_loop:
            lodsb
            cmp     al,cr
            jz      quit_copy
            stosb
            loop    copy_loop
quit_copy:
            mov     ax,0ffh
            sub     ax,cx
            mov     plain_len,ax
            mov     dx,82h
            mov     ax,3d02h
            int     21h
            jnc     no_error
            jmp     error
no_error:

            xchg    ax,bx
            lea     dx,buffer
            mov     cx,0FFFEh-(offset buffer-offset start)-200h
            mov     ah,3fh
            int     21h
            mov     bytes_read,ax
            mov     ah,3eh
            int     21h

            lea     dx,byte_xor_msg
            call    disp
xor8:
            call    crack_byte_XOR
            jnc     cont0
            mov     cx,2
            lea     si,key8
            call    cvt_key
            call    xor_b_crypt_ct
            call    alright
            call    xor_b_crypt_ct
            inc     key8
            jnz     xor8
cont0:


            call    reset
            lea     dx,byte_add_msg
            call    disp
            mov     word ptr ext,'S8'
            mov     byte ptr ext+2,'B'
add8:
            call    crack_byte_add
            jnc     cont2
            mov     cx,2
            lea     si,key8
            call    cvt_key
            call    sub_b_crypt_ct
            call    alright
            call    add_b_crypt_ct
            inc     key8
            jnz     add8
cont2:

            call    reset
            lea     dx,byte_rot_msg
            call    disp
            mov     word ptr ext,'R8'
            mov     byte ptr ext+2,'T'
rot8:
            call    crack_byte_rot
            jnc     cont6
            mov     cx,2
            lea     si,key8
            call    cvt_key
            call    alright
            inc     key8
            cmp     key8,9
            jnz     rot8
cont6:

            call    reset
            lea     dx,word_xor_msg
            call    disp
            mov     word ptr ext,'61'
            mov     byte ptr ext+2,'X'
xor16:
            call    crack_word_XOR
            jnc     cont1
            mov     cx,4
            lea     si,key16
            call    cvt_key
            call    xor_w_crypt_ct
            call    alright
            call    xor_w_crypt_ct
            inc     key16
            jnz     xor16
cont1:

            call    reset
            lea     dx,word_add_msg
            call    disp
            mov     word ptr ext,'61'
            mov     byte ptr ext+2,'S'
add16:
            call    crack_word_add
            jnc     cont4
            mov     cx,4
            lea     si,key16
            call    cvt_key
            call    sub_w_crypt_ct
            call    alright
            call    add_w_crypt_ct
            inc     key16
            jnz     add16
cont4:

            call    reset
            lea     dx,word_rot_msg
            call    disp
            mov     word ptr ext,'61'
            mov     byte ptr ext+2,'R'
rot16:
            call    crack_word_rot
            jnc     cont7
            mov     cx,4
            lea     si,key16
            call    cvt_key
            call    alright
            inc     key16
            cmp     key16,17
            jnz     rot16
cont7:

            cmp     adr_ct,offset buffer
            jnz     did_adjust
            lea     dx,adjust_msg
            call    disp
            inc     adr_ct
            inc     adr_pt
            dec     plain_len
            jmp     cont1
did_adjust:



            lea     dx,done_msg
            call    disp
            ret

alright:
            lea     dx,fnd_msg
            call    disp
            lea     dx,fname
            mov     ah,3Ch
            xor     cx,cx
            int     21h
            xchg    ax,bx
            lea     dx,buffer
            mov     cx,bytes_read
            mov     ah,40h
            int     21h
            mov     ah,3eh
            int     21h

            ret

error:
            lea     dx,error_msg
            call    disp
            ret


setup_pt_ptr:
            mov     cx,plain_len
            mov     si,adr_pt
            mov     di,si
            ret

setup_ct_ptr:
            mov     cx,bytes_read
            mov     si,adr_ct
            mov     di,si
            ret



crack_byte_XOR:
            inc     count
            cmp     count,(0FFh/16)+1
            jnz     nodispbx
            mov     count,0
            mov     dl,'.'
            call    disp_char
nodispbx:
            call    xor_b_crypt_pt
            call    search_file
            pushf
            call    xor_b_crypt_pt
            popf
            jc      done_b_xor
            inc     key8
            jnz     crack_byte_xor
done_b_xor:
            ret

xor_b_crypt_pt:
            call    setup_pt_ptr
            jmp     xor_b_loop
xor_b_crypt_ct:
            call    setup_ct_ptr
xor_b_loop:
            lodsb
            xor     al,key8
            stosb
            loop    xor_b_loop
            ret


crack_word_XOR:
            inc     count
            cmp     count,0FFFFh/16
            jnz     nodispwx
            mov     count,0
            mov     dl,'.'
            call    disp_char
nodispwx:
            call    xor_w_crypt_pt
            call    search_file
            pushf
            call    xor_w_crypt_pt
            popf
            jc      done_w_xor
            inc     key16
            jnz     crack_word_XOR
done_w_xor:
            ret


xor_w_crypt_pt:
            call    setup_pt_ptr
            jmp     xor_w_loop
xor_w_crypt_ct:
            call    setup_ct_ptr
xor_w_loop:
            lodsw
            mov     bx,key16
            xchg    bl,bh
            xor     ax,bx
            stosw
            dec     cx
            jz      _ret
            loop    xor_w_loop
_ret:
            ret


crack_byte_ADD:
            inc     count
            cmp     count,(0FFh/16)+1
            jnz     nodispba
            mov     count,0
            mov     dl,'.'
            call    disp_char
nodispba:
            call    add_b_crypt_pt
            call    search_file
            pushf
            call    sub_b_crypt_pt
            popf
            jc      done_b_add
            inc     key8
            jnz     crack_byte_add
done_b_add:
            ret

add_b_crypt_pt:
            call    setup_pt_ptr
            jmp     add_b_loop
add_b_crypt_ct:
            call    setup_ct_ptr
add_b_loop:
            lodsb
            add     al,key8
            stosb
            loop    add_b_loop
            ret

sub_b_crypt_pt:
            call    setup_pt_ptr
            jmp     sub_b_loop
sub_b_crypt_ct:
            call    setup_ct_ptr
sub_b_loop:
            lodsb
            sub     al,key8
            stosb
            loop    sub_b_loop
            ret

crack_word_ADD:
            inc     count
            cmp     count,0FFFFh/16
            jnz     nodispwa
            mov     count,0
            mov     dl,'.'
            call    disp_char
nodispwa:
            call    add_w_crypt_pt
            call    search_file
            pushf
            call    sub_w_crypt_pt
            popf
            jc      done_w_add
            inc     key16
            jnz     crack_word_add
done_w_add:
            ret

add_w_crypt_pt:
            call    setup_pt_ptr
            jmp     add_w_loop
add_w_crypt_ct:
            call    setup_ct_ptr
add_w_loop:
            lodsw
            add     ax,key16
            stosw
            dec     cx
            jz      _retaw
            loop    add_w_loop
_retaw:
            ret

sub_w_crypt_pt:
            call    setup_pt_ptr
            jmp     sub_w_loop
sub_w_crypt_ct:
            call    setup_ct_ptr
sub_w_loop:
            lodsw
            sub     ax,key16
            stosw
            dec     cx
            jz      _retsw
            loop    sub_w_loop
_retsw:
            ret

crack_byte_rot:
            mov     dl,'.'
            call    disp_char
            call    ror_b_crypt_ct
            call    search_file
            jc      done_b_rot
            inc     key8
            cmp     key8,9
            jnz     crack_byte_rot
done_b_rot:
            pushf
            cmp     key8,8
            jnz     brot_ok
            popf
            clc
            ret
brot_ok:
            popf
            ret

            ret

ror_b_crypt_ct:
            call    setup_ct_ptr
ror_b_loop:
            lodsb
            ror     al,1
            stosb
            loop    ror_b_loop
            ret

crack_word_rot:
            mov     dl,'.'
            call    disp_char
            call    ror_w_crypt_ct
            call    search_file
            jc      done_w_rot
            inc     key16
            cmp     key16,17
            jnz     crack_word_rot
done_w_rot:
            pushf
            cmp     key16,16
            jnz     wrot_ok
            popf
            clc
            ret
wrot_ok:
            popf
            ret

ror_w_crypt_ct:
            call    setup_ct_ptr
ror_w_loop:
            lodsw
            ror     ax,1
            stosw
            dec     cx
            jz      ror_w_done
            loop    ror_w_loop
ror_w_done:
            ret




search_file:
            mov     cx,bytes_read
            mov     si,adr_ct
            dec     si
search_loop0:
            mov     di,adr_pt
search_loop:
            mov     ax,di
            sub     ax,adr_pt
            cmp     ax,plain_len
            jz      got_it
            inc     si
            mov     al,byte ptr [di]
            cmp     al, byte ptr [si]
            jz      got_one
            loop    search_loop0
            jmp     not_found
got_one:
            inc     di

            loop    search_loop
not_found:
            clc
            ret
got_it:
            stc
            ret


cvt_key:
            lea     di,key_txt+3
cvt_key_l0:
            call    cvt_byte
            sub     cx,2
            jnz     cvt_key_l0
            ret

cvt_byte:
            push    cx
            mov     al,byte ptr [si]
            inc     si
            push    ax
            mov     cx,4
            shl     al,cl
            shr     al,cl
            call    to_ascii
            mov     byte ptr [di],al
            dec     di
            pop     ax
            shr     al,cl
            call    to_ascii
            mov     byte ptr [di],al
            dec     di
            pop     cx
            ret

to_ascii:
            cmp     al,9
            jg      is_alpha
            add     al,'0'
            ret
is_alpha:
            add     al,'A'-10
            ret


reset:
            mov     key8,1
            mov     key16,1
            mov     count,0
            mov     word ptr key_txt,'00'
            mov     word ptr key_txt+2,'00'
            ret

disp_char:
            mov     ah,2
            int     21h
            mov     ah,0bh
            int     21h
            cmp     al,0
            jz      no_key
            lea     dx,user_abort
            call    disp
            mov     ax,4c01h
            int     21h
no_key:
            ret
disp:
            mov     ah,9
            int     21h
            ret

intro:
 db      cr,lf,'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
 db      cr,lf,"³ ûirogen's Super-Duper Encryption Cracker ³  [û] FDA Approved"
 db      cr,lf,'³ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ³  [û] FCC Class B Appliance'
 db      cr,lf,'³ version 2.0 ; (c)1995 ûirogen [NuKE]     ³  [û] Love Me, Fuck Me, Kill Me'
 db      cr,lf,'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ$'
byte_add_msg db     cr,lf,'  Attempting 8-bit ADD/SUB  .$'
byte_xor_msg db     cr,lf,'  Attempting 8-bit XOR      .$'
byte_rot_msg db     cr,lf,'  Attempting 8-bit ROR/ROL  .$'
word_xor_msg db     cr,lf,'  Attempting 16-bit XOR     .$'
word_add_msg db     cr,lf,'  Attempting 16-bit ADD/SUB .$'
word_rot_msg db     cr,lf,'  Attempting 16-bit ROR/ROL .$'
adjust_msg  db      cr,lf,'  Adjusting 16-bit Addressing$'
user_abort  db      cr,lf,cr,lf, ' Key Input Detected. Process Aborted. ',cr,lf,'$'
done_msg    db      cr,lf,cr,lf,'  All keys exhausted.',cr,lf,'$'
fnd_msg     db      cr,lf,'     Possible key found! Writing decrypted file: '
fname       db      'DEC-'
key_txt     db      '0000.'
ext         db      '8XR',0,'$'
error_msg   db      cr,lf,cr,lf,'Error opening file! Usage: VIROCRK [filename] [search text]',cr,lf,'$'
no_cmd_line db      cr,lf,cr,lf,'No command line. Usage: VIROCRK [filename] [search text]',cr,lf,'$'
plain_len   dw      0
plain_text  db      128 dup (0)
bytes_read  dw      0
key8        db      1
key16       dw      1
count       dw      0
adr_pt      dw      offset plain_text
adr_ct      dw      offset buffer
buffer:
cseg        ends
            end     start

