SCREEN 7, 0, 1, 0
COLOR , 2
RANDOMIZE TIMER
DIM personaje(100)
DIM enemigo(100)

CLS 'dibujo de los sprites
FOR lineaDibujoPersonaje = 1 TO 10
  FOR columnaDibujoPersonaje = 1 TO 10
    READ colorDibujoPersonaje
    PSET (columnaDibujoPersonaje, lineaDibujoPersonaje), colorDibujoPersonaje
  NEXT columnaDibujoPersonaje
NEXT lineaDibujoPersonaje
GET (1, 1)-(10, 10), personaje
CLS
FOR lineaDibujoEnemigo = 1 TO 10
  FOR columnaDibujoEnemigo = 1 TO 10
    READ colorDibujoEnemigo
    PSET (columnaDibujoEnemigo, lineaDibujoEnemigo), colorDibujoEnemigo
  NEXT columnaDibujoEnemigo
NEXT lineaDibujoEnemigo
GET (1, 1)-(10, 10), enemigo

xEnemigo = INT((309) * RND + 1)
yEnemigo = INT((189 - 10) * RND + 11)
xPersonaje = 25
yPersonaje = 25
CLS

'----------------------------
'-------Loop Principal-------
'----------------------------

DO
LINE (1, 1)-(30 - conteoDisparo, 7), 4, BF
LOCATE 2, 1: PRINT colisiones
movimientoEnemigo = movimientoEnemigo + 1
IF conteoDisparo <> 0 THEN conteoDisparo = conteoDisparo - 1
IF chequeoDisparo = 1 THEN
  chequeoDisparo = 0
  disparoActivo = 1
END IF

GOSUB lecturaDeTeclas
GOSUB movimientoEnemigoSub
GOSUB movimientoPersonajeSub
GOSUB choque
IF disparoActivo = 1 THEN GOSUB disparo

IF chequeoFantasma = 1 THEN conteoFantasma = conteoFantasma + 1
IF conteoFantasma >= 50 THEN
  conteoFantasma = 0
  chequeoFantasma = 0
END IF
PUT (xEnemigo, yEnemigo), enemigo
IF (conteoFantasma / 2) - CINT(conteoFantasma / 2) = 0 THEN PUT (xPersonaje, yPersonaje), personaje

PCOPY 1, 0
CLS
LOOP

'----------------------------
'---Fin del Loop Principal---
'----------------------------

lecturaDeTeclas:
SELECT CASE INKEY$
  CASE "4"
    xMovimientoPersonaje = -1
    yMovimientoPersonaje = 0
  CASE "6"
    xMovimientoPersonaje = 1
    yMovimientoPersonaje = 0
  CASE "8"
    xMovimientoPersonaje = 0
    yMovimientoPersonaje = -1
  CASE "2"
    xMovimientoPersonaje = 0
    yMovimientoPersonaje = 1
  CASE "7"
    xMovimientoPersonaje = -1
    yMovimientoPersonaje = -1
  CASE "1"
    xMovimientoPersonaje = -1
    yMovimientoPersonaje = 1
  CASE "9"
    xMovimientoPersonaje = 1
    yMovimientoPersonaje = -1
  CASE "3"
    xMovimientoPersonaje = 1
    yMovimientoPersonaje = 1
  CASE "5"
    xMovimientoPersonaje = 0
    yMovimientoPersonaje = 0
  CASE "q"
    END
  CASE CHR$(32)              'disparo
    IF conteoDisparo = 0 THEN
      SOUND 1000, 1
      chequeoDisparo = 1
      conteoDisparo = 30
    END IF
END SELECT
RETURN

movimientoPersonajeSub:
IF xPersonaje > 309 THEN
  xMovimientoPersonaje = -1 'evita que el personaje salga de la pantalla
  SOUND 350, 1
END IF
IF xPersonaje < 11 THEN
  xMovimientoPersonaje = 1
  SOUND 350, 1
END IF
IF yPersonaje > 189 THEN
  yMovimientoPersonaje = -1
  SOUND 350, 1
END IF
IF yPersonaje < 11 THEN
  yMovimientoPersonaje = 1
  SOUND 350, 1
END IF
xPersonaje = xPersonaje + xMovimientoPersonaje
yPersonaje = yPersonaje + yMovimientoPersonaje
RETURN

movimientoEnemigoSub:
IF (movimientoEnemigo / 30) - CINT(movimientoEnemigo / 30) = 0 THEN 'cambia la direccion del movimiento del enemigo si la variable "movimientoenemigo" es divisible por 30
  xMovimientoEnemigo = INT((3) * RND + -1)
  yMovimientoEnemigo = INT((3) * RND + -1)
  movimientoEnemigo = 0
  SOUND 500, 1
END IF
IF xEnemigo > 309 THEN       'evita que el enemigo salga de la pantalla
  xMovimientoEnemigo = -1
  SOUND 237, 1
END IF
IF xEnemigo < 11 THEN
  xMovimientoEnemigo = 1
  SOUND 237, 1
END IF
IF yEnemigo > 189 THEN
  yMovimientoEnemigo = -1
  SOUND 237, 1
END IF
IF yEnemigo < 11 THEN
  yMovimientoEnemigo = 1
  SOUND 237, 1
END IF
xEnemigo = xEnemigo + xMovimientoEnemigo
yEnemigo = yEnemigo + yMovimientoEnemigo
RETURN

choque:
IF chequeoFantasma <> 1 THEN
  IF ABS((xEnemigo) - (xPersonaje)) < 10 AND ABS((yEnemigo) - (yPersonaje)) < 10 THEN
    SOUND 349, 1
    yMovimientoEnemigo = yMovimientoEnemigo * -1
    xMovimientoEnemigo = xMovimientoEnemigo * -1
    yMovimientoPersonaje = 0
    xMovimientoPersonaje = 0
    colisiones = colisiones + 1
    chequeoFantasma = 1
    IF colisiones >= 10 THEN END
  END IF
END IF
RETURN

disparo:
LOCATE 1, 1
PRINT "bang!"
disparoActivo = 0
RETURN

DATA 00, 00, 00, 00, 14, 14, 00, 00, 00, 00
DATA 00, 00, 00, 14, 14, 14, 14, 00, 00, 00
DATA 00, 00, 00, 14, 14, 14, 14, 00, 00, 00
DATA 00, 14, 00, 00, 14, 14, 00, 00, 14, 00
DATA 14, 14, 01, 01, 01, 01, 01, 01, 14, 14
DATA 14, 14, 01, 01, 01, 01, 01, 01, 14, 14
DATA 00, 00, 00, 01, 01, 01, 01, 00, 00, 00
DATA 00, 00, 00, 06, 06, 06, 06, 00, 00, 00
DATA 00, 00, 06, 06, 00, 00, 06, 06, 00, 00
DATA 00, 06, 06, 06, 00, 00, 06, 06, 06, 00

DATA 00, 00, 10, 00, 00, 00, 00, 10, 00, 00
DATA 00, 00, 00, 10, 00, 00, 10, 00, 00, 00
DATA 00, 00, 00, 10, 00, 00, 10, 00, 00, 00
DATA 00, 00, 10, 10, 10, 10, 10, 10, 00, 00
DATA 00, 10, 10, 10, 10, 10, 10, 10, 10, 00
DATA 10, 10, 10, 04, 10, 10, 04, 10, 10, 10
DATA 00, 10, 10, 10, 10, 10, 10, 10, 10, 00
DATA 10, 10, 04, 07, 04, 04, 07, 04, 10, 10
DATA 00, 10, 04, 04, 04, 04, 04, 04, 10, 00
DATA 10, 00, 10, 10, 10, 10, 10, 10, 00, 10

SUB enemigosub
   









END SUB

