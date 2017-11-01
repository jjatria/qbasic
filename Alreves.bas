10 CLS
INPUT "En que estas pensando"; palabra$
CLS
y = 1: x = 1
FOR b = LEN(palabra$) TO 1 STEP -1
  x = x + 1
  LOCATE y, x: PRINT MID$(palabra$, b, 1)
NEXT
INPUT "Quieres escribir otra palabra"; denuevo$
IF denuevo$ = "si" OR denuevo$ = "s" OR denuevo$ = "y" OR denuevo$ = "yes" OR denuevo$ = "S" OR denuevo$ = "SI" OR denuevo$ = "Si" OR denuevo$ = "Y" OR denuevo$ = "Yes" OR denuevo$ = "YES" THEN
GOTO 10
END IF
END

