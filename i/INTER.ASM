; Interface to RS232INT.EXE module
CODESEG SEGMENT PARA PUBLIC 'CODE'
     PUBLIC    REFRESH
     PUBLIC    SETSENSE
     PUBLIC    SENDBREAK
     PUBLIC    BUFSIZE
     PUBLIC    RECEIVE
     PUBLIC    SEND
;
; TABLE OFF ADDRESSES TO BE FILLED FOR JUMPS
SZ   DW      0
FIRST   DW   ?
SB   DW     10H
     DW      ?
BF   DW     20H
     DW      ?
RC   DW     30H
     DW      ?
SN   DW     40H
     DW      ?
REFRESH PROC   FAR
     ASSUME CS:CODESEG
SEGAD   EQU    4F0H  ;ADDRESS IN INTRA AREA OF SEG ADDRESS
CHNAD   EQU    4F2H  ;ADDRESS IN INTRA AREA OF CHAN # OFFSET
     PUSH    AX
     PUSH    BX
     PUSH    DS
     MOV     AX,0   ;SET DS TO 0
     MOV     DS,AX
     MOV     AX,DS:[SEGAD]   ;GET SEG ADDRESS IN AX
     MOV     BX,0
LUPE: MOV     FIRST[BX],AX ;STORE IN ADDR TABLE
     ADD     BX,4
     CMP     BX,20
     JNE     LUPE     ;MODIFY 5 ADDRESSES
     MOV     BX,DS:[CHNAD]     ;GET CHAN # OFFSET IN BX
     MOV     DS,AX          ;NOW SET DATA SEG TO LOADED CODE
     MOV     BYTE PTR [BX],0         ;ZERO CHAN#
     POP     DS
     POP     BX
     POP     AX
     RET
REFRESH      ENDP
;
SETSENSE   PROC  FAR
     JMP   DWORD PTR SZ  ;GO TO LOADED CODE ENTRY POINT
SETSENSE   ENDP
;
SENDBREAK  PROC  FAR
     JMP   DWORD PTR SB   ;GO TO LOADED CODE ENTRY POINT
SENDBREAK  ENDP
;
BUFSIZE    PROC  FAR
     JMP   DWORD PTR BF   ;GO TO LOADED CODE ENTRY POINT
BUFSIZE    ENDP
;
RECEIVE    PROC  FAR
     JMP   DWORD PTR RC   ;GO TO LOADED CODE ENTRY POINT
RECEIVE    ENDP
;
SEND       PROC  FAR
     JMP   DWORD PTR SN   ;GO TO LOADED CODE ENTRY POINT
SEND       ENDP
;
CODESEG    ENDS
           END
                                                                                                        