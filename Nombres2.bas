CLS
PRINT "Programa generador de nombres"
PRINT "Presiona cualquier tecla para que se genere una lista aleatoria de nombres"
SLEEP
longitud = 8
50
LOCATE 23, 1
PRINT "                                                          "
lista = 0
ultima = 1
vocales$ = "aaaaaeeeeiiooouu"
consonantes$ = "bbbbccccddddfffggghhjjjkkllllmmmmnnnn¤¤ppppqqqrrrrssttttvvxxwyz"
FOR linea = 3 TO 22
        LOCATE linea, 4
        PRINT "                                                                                "
NEXT linea
linea = 4
100
FOR char = 4 TO longitud + 3
        vocal$ = MID$(vocales$, CINT((RND(1) * (LEN(vocales$) - 1)) + 1), 1)
120     consonante$ = MID$(consonantes$, CINT((RND(1) * (LEN(consonantes$) - 1)) + 1), 1)
        SELECT CASE ultima
                CASE -1
                LOCATE linea, char + lista
                IF char = 4 THEN PRINT UCASE$(vocal$) ELSE
                IF char <> 4 THEN PRINT vocal$
                CASE 1
                LOCATE linea, char
                IF char = longitud + 3 AND consonante$ = "¤" THEN GOTO 120
                IF char = 4 THEN PRINT UCASE$(consonante$) ELSE
                IF char <> 4 THEN PRINT consonante$
        END SELECT
        ultima = ultima * -1
NEXT char
linea = linea + 1
IF linea < 22 GOTO 100
200
LOCATE 23, 1
PRINT "                                                                               "
LOCATE 23, 1
INPUT "Quieres otra lista de nombres (s/n/x)"; denuevo$
IF denuevo$ = "s" THEN GOTO 50
IF denuevo$ = "n" THEN END
IF denuevo$ = "x" THEN
        LOCATE 23, 1
        longitudanterior = longitud
        INPUT "Cu ntos caracteres quieres que tenga el nombre (1-77)"; longitud
        IF longitud > 77 THEN
                longitud = longitudanterior
                LOCATE 23, 1
                PRINT "El nombre no puede exceder los 77 caracteres. Presiona cualquier tecla."
                SLEEP
                GOTO 200
        ELSE GOTO 50
        END IF
END IF
IF denuevo$ <> "n" AND denuevo$ <> "s" AND denuevo$ <> "x" THEN GOTO 200


