DECLARE SUB menuInicio ()
DECLARE SUB obstaculos ()
DECLARE SUB lecturaDeTeclas ()
DECLARE SUB movimientoPersonajeSub ()
DECLARE SUB movimientoEnemigoSub ()
DECLARE SUB choque ()
DECLARE SUB disparo ()
COMMON SHARED genero$, lineaDibujoPersonajeMasculino, lineaDibujoPersonajeFemenino, lineaDibujoEnemigo, columnaDibujoPersonajeMasculino, columnaDibujoPersonajeFemenino, columnaDibujoEnemigo, colorDibujoPersonajeMasculino,  _
colorDibujoPersonajeFemenino, colorDibujoEnemigo, xPersonaje, yPersonaje, xMovimientoPersonaje, yMovimientoPersonaje, xEnemigo, yEnemigo, movimientoEnemigo, yMovimientoEnemigo, xMovimientoEnemigo, xBala, yBala, xMovimientoBala, yMovimientoBala,  _
chequeoDisparo, conteoDisparo, chequeoFantasma, conteoFantasma, colisiones, disparoActivo, ultimaTecla$, cantidadDeEnemigos, aumentoDeEnemigos, xLinea, yLinea, longitudDelinea, consola$, activacionDeEscudo, velocidadPersonaje,  _
imprimirPosicionesEnemigos, dibujarParedes, cantidadDeVidas, vidas
RANDOMIZE TIMER
DIM SHARED personajeMasculino(100)
DIM SHARED personajeFemenino(100)
DIM SHARED enemigo(100)
DIM SHARED corazon(25)
DIM SHARED xEnemigo(10)
DIM SHARED yEnemigo(10)
DIM SHARED xMovimientoEnemigo(10)
DIM SHARED yMovimientoEnemigo(10)

'Inicializacion de variables importantes.
cantidadDeEnemigos = 0          'Determina la cantidad de enemigos que apareceran en la pantalla, adem s del primero.
aumentoDeEnemigos = 1           'Determina si la cantidad de enemigos aumenta (si se ha eliminado menos de 10) o disminuye (si se ha eliminado mas).
activacionDeEscudo = -1         'Determina la presencia o ausencia del escudo.
velocidadPersonaje = 1          'Determina la velocidad a la que se mueve el personaje. Esta aun no est  del todo habilitada y no funciona bien.
imprimirPosicionesEnemigos = -1 'Mas que nada para debug, determina si se imprime o no una lista con las posiciones de los enemigos.
dibujarParedes = 1              'Determina si las pardes han o no de dibujarse.
vidas = 2                       'Determina la cantidad de vidas del personaje.
genero$ = "masculino"

CALL menuInicio

SCREEN 7, 0, 1, 0
COLOR , 2

CLS 'Dibujo de los sprites.
FOR lineaDibujoPersonajeMasculino = 1 TO 10
  FOR columnaDibujoPersonajeMasculino = 1 TO 10
    READ colorDibujoPersonajeMasculino
    PSET (columnaDibujoPersonajeMasculino, lineaDibujoPersonajeMasculino), colorDibujoPersonajeMasculino
  NEXT columnaDibujoPersonajeMasculino
NEXT lineaDibujoPersonajeMasculino
GET (1, 1)-(10, 10), personajeMasculino
CLS
FOR lineaDibujoPersonajeFemenino = 1 TO 10
  FOR columnaDibujoPersonajeFemenino = 1 TO 10
    READ colorDibujoPersonajeFemenino
    PSET (columnaDibujoPersonajeFemenino, lineaDibujoPersonajeFemenino), colorDibujoPersonajeFemenino
  NEXT columnaDibujoPersonajeFemenino
NEXT lineaDibujoPersonajeFemenino
GET (1, 1)-(10, 10), personajeFemenino
CLS
FOR lineaDibujoEnemigo = 1 TO 10
  FOR columnaDibujoEnemigo = 1 TO 10
    READ colorDibujoEnemigo
    PSET (columnaDibujoEnemigo, lineaDibujoEnemigo), colorDibujoEnemigo
  NEXT columnaDibujoEnemigo
NEXT lineaDibujoEnemigo
GET (1, 1)-(10, 10), enemigo
CLS
FOR lineaDibujoCorazon = 1 TO 5
  FOR columnaDibujoCorazon = 1 TO 5
    READ colorDibujoCorazon
    PSET (columnaDibujoCorazon, lineaDibujoCorazon), colorDibujoCorazon
  NEXT columnaDibujoCorazon
NEXT lineaDibujoCorazon
GET (1, 1)-(5, 5), corazon

FOR inicializacionDelArrayDeEnemigos = 0 TO 10
  xEnemigo(inicializacionDelArrayDeEnemigos) = INT((281) * RND + 20)
  yEnemigo(inicializacionDelArrayDeEnemigos) = INT((151) * RND + 40)
NEXT inicializacionDelArrayDeEnemigos

xPersonaje = 25
yPersonaje = 25

CLS

'----------------------------
'-------Loop Principal-------
'----------------------------

DO

IF imprimirPosicionesEnemigos = 1 THEN
  PRINT
  PRINT
  FOR test = 0 TO 10
  PRINT yEnemigo(test)
  LOCATE test + 3, 5
  PRINT xEnemigo(test)
  NEXT test
  LOCATE 15, 3
  PRINT "Y   X"
END IF

IF cantidadDeEnemigos = 9 THEN
  aumentoDeEnemigos = -1
ELSEIF cantidadDeEnemigos < 0 THEN
  LOCATE 13, 16
  PRINT "YOU WIN!"
  cancionFinal$ = "o3 L8 E o2 B o3 D C L2 o2 A"
  PLAY "MB X" + VARPTR$(cancionFinal$)
  PCOPY 1, 0
  DO UNTIL INKEY$ <> ""
  LOOP
  EXIT DO
END IF

IF colisiones < 0 THEN colisiones = 0 'Evita situaiones engorrosas, ya que al matar enemigos se suman vidas.
LINE (1, 1)-(50 - conteoDisparo, 7), 4, BF 'Imprime la barra que indica cuando se puede disparar.

FOR cantidadDeVidas = 0 TO (vidas - colisiones) 'Imprime los corazones que indican las vidas del jugador.
  PUT (60 + (cantidadDeVidas * 10), 2), corazon
NEXT cantidadDeVidas

IF colisiones = vidas + 1 THEN 'Revisa si al jugador le quedan vidas.
  LOCATE 13, 16
  PRINT "GAME OVER"
  cancionFinal$ = "o3 L8 E o2 B o3 D C L2 o2 A"
  PLAY "MB X" + VARPTR$(cancionFinal$)
  PCOPY 1, 0
  DO UNTIL INKEY$ <> ""
  LOOP
  EXIT DO
END IF

movimientoEnemigo = movimientoEnemigo + 1 'Aumenta la variable que una vez cada treinta ciclos hace que el enemigo cambie de direccion.

IF conteoDisparo <> 0 THEN 'Plotea la bala siempre y cuando conteoDisparo indique que la bala sigue existiendo y no se puede disparar de nuevo.
  conteoDisparo = conteoDisparo - 1
  xBala = xBala + xMovimientoBala
  yBala = yBala + yMovimientoBala
  PSET (xBala, yBala), 14
ELSE
  xBala = xPersonaje + 5
  yBala = yPersonaje + 5
END IF

CALL lecturaDeTeclas
CALL obstaculos
CALL movimientoEnemigoSub
CALL movimientoPersonajeSub
CALL choque

IF chequeoDisparo = 1 THEN 'Le indica al programa que hay un disparo activo.
  chequeoDisparo = 0
  disparoActivo = 1
END IF

IF disparoActivo = 1 THEN CALL disparo 'Si hay un disparo activo, entonces se llama la rutina que asigna la direccion del tiro.
                       
IF chequeoFantasma = 1 THEN conteoFantasma = conteoFantasma + 1 'Aumenta la variable que le indica al programa por cuanto tiempo tiene que permanecer el personaje en estado de fantasma.

IF conteoFantasma >= 50 THEN 'Si esta variable llega a 50, el estado fantasma se termina.
  conteoFantasma = 0
  chequeoFantasma = 0
END IF

IF (conteoFantasma / 2) - CINT(conteoFantasma / 2) = 0 THEN
  SELECT CASE genero$
    CASE "masculino"
      PUT (xPersonaje, yPersonaje), personajeMasculino 'Por el contrario, mientras esta variable sea inferior a 50, el personaje solo se imprime una vez cada dos ciclos.
    CASE "femenino"
      PUT (xPersonaje, yPersonaje), personajeFemenino 'Por el contrario, mientras esta variable sea inferior a 50, el personaje solo se imprime una vez cada dos ciclos.
  END SELECT
END IF
PCOPY 1, 0 'Imprime lo hecho en la pagina activa
CLS 'Se asegura de que cada ciclo genere una pagina nueva, evitando la creacion de estelas.
LOOP

'----------------------------
'---Fin del Loop Principal---
'----------------------------

'Pixeles del dibujo del personaje masculino.

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

'Pixeles del dibujo del personaje femenino.

DATA 00, 00, 00, 00, 06, 06, 00, 00, 00, 00
DATA 00, 00, 00, 06, 14, 14, 06, 00, 00, 00
DATA 00, 00, 00, 06, 14, 14, 06, 00, 00, 00
DATA 00, 00, 00, 06, 14, 14, 06, 00, 00, 00
DATA 14, 04, 04, 04, 04, 04, 04, 04, 04, 14
DATA 14, 04, 04, 04, 04, 04, 04, 04, 04, 14
DATA 00, 00, 00, 04, 04, 04, 04, 00, 00, 00
DATA 00, 00, 01, 01, 01, 01, 01, 01, 00, 00
DATA 00, 01, 01, 01, 01, 01, 01, 01, 01, 00
DATA 00, 00, 04, 04, 00, 00, 04, 04, 00, 00

'Pixeles del dibujo del enemigo.

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

'Pixeles del dibujo del corazon.

DATA 00, 04, 00, 04, 00
DATA 04, 04, 04, 04, 04
DATA 04, 04, 04, 04, 04
DATA 00, 04, 04, 04, 00
DATA 00, 00, 04, 00, 00

SUB choque
  
  IF chequeoFantasma <> 1 THEN
    FOR numeroDeEnemigos = 0 TO cantidadDeEnemigos
      IF ABS((xEnemigo(numeroDeEnemigos)) - (xPersonaje)) < 10 AND ABS((yEnemigo(numeroDeEnemigos)) - (yPersonaje)) < 10 THEN
        SOUND 349, 1
        yMovimientoEnemigo(numeroDeEnemigos) = yMovimientoEnemigo(numeroDeEnemigos) * -1
        xMovimientoEnemigo(numeroDeEnemigos) = xMovimientoEnemigo(numeroDeEnemigos) * -1
        yMovimientoPersonaje = 0
        xMovimientoPersonaje = 0
        colisiones = colisiones + 1
        chequeoFantasma = 1
      END IF
    NEXT numeroDeEnemigos
  END IF

  FOR numeroDeEnemigos = 0 TO cantidadDeEnemigos
    IF chequeoFantasma <> 1 THEN
      IF xBala <= (xEnemigo(numeroDeEnemigos) + 9) AND xBala >= xEnemigo(numeroDeEnemigos) AND yBala <= (yEnemigo(numeroDeEnemigos) + 9) AND yBala >= yEnemigo(numeroDeEnemigos) THEN
        xEnemigo(numeroDeEnemigos) = INT((281) * RND + 20)
        yEnemigo(numeroDeEnemigos) = INT((151) * RND + 40)
        colisiones = colisiones - 1
        musicaDeVictoria$ = "o3 L8 E D+ E D+" 'Esta musica la saque del help de PLAY, es solo un ejemplo, luego hay que modificarla.
        PLAY "MB X" + VARPTR$(musicaDeVictoria$)
        cantidadDeEnemigos = cantidadDeEnemigos + aumentoDeEnemigos
        xBala = xPersonaje + 5
        yBala = yPersonaje + 5
        conteoDisparo = 0
      END IF
    END IF
  NEXT numeroDeEnemigos

END SUB

SUB disparo

  SELECT CASE ultimaTecla$
    CASE "1"
      xMovimientoBala = -2
      yMovimientoBala = 2
    CASE "2"
      xMovimientoBala = 0
      yMovimientoBala = 2
    CASE "3"
      xMovimientoBala = 2
      yMovimientoBala = 2
    CASE "4"
      xMovimientoBala = -2
      yMovimientoBala = 0
    CASE "6"
      xMovimientoBala = 2
      yMovimientoBala = 0
    CASE "7"
      xMovimientoBala = -2
      yMovimientoBala = -2
    CASE "8"
      xMovimientoBala = 0
      yMovimientoBala = -2
    CASE "9"
      xMovimientoBala = 2
      yMovimientoBala = -2
  END SELECT

  disparoActivo = 0 'Evita que la direccion de la bala cambie una vez que esta ya se ha disparado.

END SUB

SUB lecturaDeTeclas

  SELECT CASE UCASE$(INKEY$)
    CASE "1"                    'Diagonal izquierda-abajo
      xMovimientoPersonaje = -1
      yMovimientoPersonaje = 1
      ultimaTecla$ = "1"
    CASE "2"                    'Abajo
      xMovimientoPersonaje = 0
      yMovimientoPersonaje = 1
      ultimaTecla$ = "2"
    CASE "3"                    'Diagonal derecha-abajo
      xMovimientoPersonaje = 1
      yMovimientoPersonaje = 1
      ultimaTecla$ = "3"
    CASE "4"                    'Izquierda
      xMovimientoPersonaje = -1
      yMovimientoPersonaje = 0
      ultimaTecla$ = "4"
    CASE "5"                    'Detenerse
      xMovimientoPersonaje = 0
      yMovimientoPersonaje = 0
    CASE "6"                    'Derecha
      xMovimientoPersonaje = 1
      yMovimientoPersonaje = 0
      ultimaTecla$ = "6"
    CASE "7"                    'Diagonal izquierda-arriba
      xMovimientoPersonaje = -1
      yMovimientoPersonaje = -1
      ultimaTecla$ = "7"
    CASE "8"                    'Arriba
      xMovimientoPersonaje = 0
      yMovimientoPersonaje = -1
      ultimaTecla$ = "8"
    CASE "9"                    'Diagonal derecha-arriba
      xMovimientoPersonaje = 1
      yMovimientoPersonaje = -1
      ultimaTecla$ = "9"
    CASE CHR$(27)               'Salir con ESC
      END
    CASE "Q"                    'Salir con "q"
      END
    CASE CHR$(32)               'Disparar con la barra espaciadora
      IF conteoDisparo = 0 THEN
        SOUND 1000, 1
        chequeoDisparo = 1
        conteoDisparo = 50
      END IF
    CASE CHR$(0) + CHR$(59)     'Ayuda con F1
      CLS
      LOCATE 5, 1
      COLOR 2, 0
      PRINT "               Controles"
      PRINT "               ---------"
      PRINT
      PRINT "Q o ESC           -> Salir del juego"
      PRINT "Barra Espaciadora -> Disparar"
      PRINT "Teclado Numerico  -> Mover al personaje"
      PRINT "                     (activa el BloqNum)"
      PRINT
      PRINT "Presiona cualquier tecla para continuar."
      PCOPY 1, 0
      SLEEP
      COLOR , 2
    'CASE CHR$(124)
    CASE "X"
      INPUT consola$ 'Claves para debug
      IF LCASE$(RTRIM$(LTRIM$(consola$))) = "shield" THEN activacionDeEscudo = activacionDeEscudo * -1
      'IF LCASE$(RTRIM$(LTRIM$(LEFT$(consola$, 5)))) = "speed" THEN velocidadPersonaje = VAL(MID$(consola$, 6, LEN(consola$) - 5))
      IF LCASE$(RTRIM$(LTRIM$(consola$))) = "enemysighted" THEN imprimirPosicionesEnemigos = imprimirPosicionesEnemigos * -1
      IF LCASE$(RTRIM$(LTRIM$(consola$))) = "togglewalls" THEN dibujarParedes = dibujarParedes * -1
      IF LCASE$(RTRIM$(LTRIM$(consola$))) = "likeavirgin" THEN genero$ = "femenino"
      IF LCASE$(RTRIM$(LTRIM$(consola$))) = "newyorkcityboy" THEN genero$ = "masculino"
      IF LCASE$(RTRIM$(LTRIM$(consola$))) = "tooyoungtodie" THEN
        vidas = 9
        colisiones = 0
      END IF
      IF LCASE$(RTRIM$(LTRIM$(consola$))) = "piriposa" THEN
        LOCATE 13, 13
        PRINT "HOLA PIRIPOSA!"
        PCOPY 1, 0
        DO UNTIL INKEY$ <> ""
        LOOP
      END IF
      'Chequeos para el aumento artificial de la cantidad de enemigos.
      IF LCASE$(RTRIM$(LTRIM$(LEFT$(consola$, 9)))) = "isthatall" AND LEN(consola$) > 11 THEN consola$ = LEFT$(consola$, 11)
      IF VAL(MID$(consola$, 10, LEN(consola$))) > 10 THEN MID$(consola$, 10, LEN(consola$)) = "10"
      IF LCASE$(RTRIM$(LTRIM$(LEFT$(consola$, 9)))) = "isthatall" THEN cantidadDeEnemigos = VAL(MID$(consola$, 10, LEN(consola$) - 9)) - 1
    END SELECT

END SUB

SUB menuInicio
SCREEN 7
COLOR 1
INPUT "Eres hombre (1) o mujer (2)"; cambioDeGenero
IF cambioDeGenero < 1 THEN cambioDeGenero = 1
IF cambioDeGenero > 2 THEN cambioDeGenero = 2
IF cambioDeGenero = 1 THEN genero$ = "masculino"
IF cambioDeGenero = 2 THEN genero$ = "femenino"
END SUB

SUB movimientoEnemigoSub

  FOR numeroDeEnemigos = 0 TO cantidadDeEnemigos
    IF (movimientoEnemigo / 30) - CINT(movimientoEnemigo / 30) = 0 THEN 'cambia la direccion del movimiento del enemigo si la variable "movimientoenemigo" es divisible por 30
      xMovimientoEnemigo(numeroDeEnemigos) = INT((3) * RND - 1)
      yMovimientoEnemigo(numeroDeEnemigos) = INT((3) * RND - 1)
      movimientoEnemigo = 0
      'SOUND 500, 1
    END IF

    IF xEnemigo(numeroDeEnemigos) > 309 THEN       'evita que el enemigo salga de la pantalla
      xEnemigo(numeroDeEnemigos) = 309
      xMovimientoEnemigo(numeroDeEnemigos) = -1
      SOUND 237, 1
    END IF
    IF xEnemigo(numeroDeEnemigos) < 1 THEN
      xEnemigo(numeroDeEnemigos) = 1
      xMovimientoEnemigo(numeroDeEnemigos) = 1
      SOUND 237, 1
    END IF
    IF yEnemigo(numeroDeEnemigos) > 189 THEN
      yEnemigo(numeroDeEnemigos) = 189
      yMovimientoEnemigo(numeroDeEnemigos) = -1
      SOUND 237, 1
    END IF
    IF yEnemigo(numeroDeEnemigos) < 11 THEN
      yEnemigo(numeroDeEnemigos) = 11
      yMovimientoEnemigo(numeroDeEnemigos) = 1
      SOUND 237, 1
    END IF

    'Derecha
    IF xMovimientoEnemigo(numeroDeEnemigos) = 1 AND yMovimientoEnemigo(numeroDeEnemigos) = 0 THEN
      FOR revisionParedes = 0 TO 9
        IF POINT(xEnemigo(numeroDeEnemigos) + 10, yEnemigo(numeroDeEnemigos) + revisionParedes) = 7 THEN 'Pared derecha
          xMovimientoEnemigo(numeroDeEnemigos) = -1
          sonido = 1
        END IF
      NEXT revisionParedes
      IF sonido = 1 THEN SOUND 350, 1
      sonido = 0
    END IF
    'Izquierda
    IF xMovimientoEnemigo(numeroDeEnemigos) = -1 AND yMovimientoEnemigo(numeroDeEnemigos) = 0 THEN
      FOR revisionParedes = 0 TO 9
        IF POINT(xEnemigo(numeroDeEnemigos) - 1, yEnemigo(numeroDeEnemigos) + revisionParedes) = 7 THEN 'Pared izquierda
          xMovimientoEnemigo(numeroDeEnemigos) = 1
          sonido = 1
        END IF
      NEXT revisionParedes
      IF sonido = 1 THEN SOUND 350, 1
      sonido = 0
    END IF
    'Abajo
    IF yMovimientoEnemigo(numeroDeEnemigos) = 1 AND xMovimientoEnemigo(numeroDeEnemigos) = 0 THEN
      FOR revisionParedes = 0 TO 9
        IF POINT(xEnemigo(numeroDeEnemigos) + revisionParedes, yEnemigo(numeroDeEnemigos) + 10) = 7 THEN 'Pared de abajo
          yMovimientoEnemigo(numeroDeEnemigos) = -1
          sonido = 1
        END IF
      NEXT revisionParedes
      IF sonido = 1 THEN SOUND 350, 1
      sonido = 0
    END IF
    'Arriba
    IF yMovimientoEnemigo(numeroDeEnemigos) = -1 AND xMovimientoEnemigo(numeroDeEnemigos) = 0 THEN
      FOR revisionParedes = 0 TO 9
        IF POINT(xEnemigo(numeroDeEnemigos) + revisionParedes, yEnemigo(numeroDeEnemigos) - 1) = 7 THEN 'Pared de arriba
          yMovimientoEnemigo(numeroDeEnemigos) = 1
          sonido = 1
        END IF
      NEXT revisionParedes
      IF sonido = 1 THEN SOUND 350, 1
      sonido = 0
    END IF
    'Diagonal arriba-izquierda
    IF yMovimientoEnemigo(numeroDeEnemigos) = -1 AND xMovimientoEnemigo(numeroDeEnemigos) = -1 THEN
      FOR revisionParedes = 0 TO 9
        IF POINT(xEnemigo(numeroDeEnemigos) + revisionParedes, yEnemigo(numeroDeEnemigos) - 1) = 7 THEN 'Pared de arriba
          yMovimientoEnemigo(numeroDeEnemigos) = 1
          xMovimientoEnemigo(numeroDeEnemigos) = -1
          sonido = 1
        ELSEIF POINT(xEnemigo(numeroDeEnemigos) - 1, yEnemigo(numeroDeEnemigos) + revisionParedes) = 7 THEN 'Pared izquierda
          yMovimientoEnemigo(numeroDeEnemigos) = -1
          xMovimientoEnemigo(numeroDeEnemigos) = 1
          sonido = 1
        END IF
      NEXT revisionParedes
      IF sonido = 1 THEN SOUND 350, 1
      sonido = 0
    END IF
    'Diagonal arriba-derecha
    IF yMovimientoEnemigo(numeroDeEnemigos) = -1 AND xMovimientoEnemigo(numeroDeEnemigos) = 1 THEN
      FOR revisionParedes = 0 TO 9
        IF POINT(xEnemigo(numeroDeEnemigos) + revisionParedes, yEnemigo(numeroDeEnemigos) - 1) = 7 THEN 'Pared de arriba
          yMovimientoEnemigo(numeroDeEnemigos) = 1
          xMovimientoEnemigo(numeroDeEnemigos) = 1
          sonido = 1
        ELSEIF POINT(xEnemigo(numeroDeEnemigos) + 10, yEnemigo(numeroDeEnemigos) + revisionParedes) = 7 THEN 'Pared derecha
          yMovimientoEnemigo(numeroDeEnemigos) = -1
          xMovimientoEnemigo(numeroDeEnemigos) = -1
          sonido = 1
        END IF
      NEXT revisionParedes
      IF sonido = 1 THEN SOUND 350, 1
      sonido = 0
    END IF
    'Diagonal abajo-izquierda
    IF yMovimientoEnemigo(numeroDeEnemigos) = 1 AND xMovimientoEnemigo(numeroDeEnemigos) = -1 THEN
      FOR revisionParedes = 0 TO 9
        IF POINT(xEnemigo(numeroDeEnemigos) + revisionParedes, yEnemigo(numeroDeEnemigos) + 10) = 7 THEN 'Pared de abajo
          yMovimientoEnemigo(numeroDeEnemigos) = -1
          xMovimientoEnemigo(numeroDeEnemigos) = -1
          sonido = 1
        ELSEIF POINT(xEnemigo(numeroDeEnemigos) - 1, yEnemigo(numeroDeEnemigos) + revisionParedes) = 7 THEN 'Pared izquierda
          yMovimientoEnemigo(numeroDeEnemigos) = 1
          xMovimientoEnemigo(numeroDeEnemigos) = 1
          sonido = 1
        END IF
      NEXT revisionParedes
      IF sonido = 1 THEN SOUND 350, 1
      sonido = 0
    END IF
    'Diagonal abajo-derecha
    IF yMovimientoEnemigo(numeroDeEnemigos) = 1 AND xMovimientoEnemigo(numeroDeEnemigos) = 1 THEN
      FOR revisionParedes = 0 TO 9
        IF POINT(xEnemigo(numeroDeEnemigos) + revisionParedes, yEnemigo(numeroDeEnemigos) + 10) = 7 THEN 'Pared de abajo
          yMovimientoEnemigo(numeroDeEnemigos) = -1
          xMovimientoEnemigo(numeroDeEnemigos) = 1
          sonido = 1
        ELSEIF POINT(xEnemigo(numeroDeEnemigos) + 10, yEnemigo(numeroDeEnemigos) + revisionParedes) = 7 THEN 'Pared derecha
          yMovimientoEnemigo(numeroDeEnemigos) = 1
          xMovimientoEnemigo(numeroDeEnemigos) = -1
          sonido = 1
        END IF
      NEXT revisionParedes
      IF sonido = 1 THEN SOUND 350, 1
      sonido = 0
    END IF
 
    xEnemigo(numeroDeEnemigos) = xEnemigo(numeroDeEnemigos) + xMovimientoEnemigo(numeroDeEnemigos)
    yEnemigo(numeroDeEnemigos) = yEnemigo(numeroDeEnemigos) + yMovimientoEnemigo(numeroDeEnemigos)
    PUT (xEnemigo(numeroDeEnemigos), yEnemigo(numeroDeEnemigos)), enemigo 'pone al enemigo en la pantalla
  NEXT numeroDeEnemigos

END SUB

SUB movimientoPersonajeSub
  IF xPersonaje > 309 THEN 'Evita que el personaje salga de la pantalla.
    xMovimientoPersonaje = -1 * velocidadPersonaje
    SOUND 350, 1
  END IF
  IF xPersonaje < 1 THEN
    xMovimientoPersonaje = 1 * velocidadPersonaje
    SOUND 350, 1
  END IF
  IF yPersonaje > 189 THEN
    yMovimientoPersonaje = -1 * velocidadPersonaje
    SOUND 350, 1
  END IF
  IF yPersonaje < 11 THEN
    yMovimientoPersonaje = 1 * velocidadPersonaje
    SOUND 350, 1
  END IF

  'Derecha
  IF xMovimientoPersonaje = 1 AND yMovimientoPersonaje = 0 THEN
    FOR revisionParedes = 0 TO 9
      IF POINT(xPersonaje + 10, yPersonaje + revisionParedes) = 7 THEN 'Pared derecha
        xMovimientoPersonaje = -1
        sonido = 1
      END IF
    NEXT revisionParedes
    IF sonido = 1 THEN SOUND 350, 1
    sonido = 0
  END IF
  'Izquierda
  IF xMovimientoPersonaje = -1 AND yMovimientoPersonaje = 0 THEN
    FOR revisionParedes = 0 TO 9
      IF POINT(xPersonaje - 1, yPersonaje + revisionParedes) = 7 THEN 'Pared izquierda
        xMovimientoPersonaje = 1
        sonido = 1
      END IF
    NEXT revisionParedes
    IF sonido = 1 THEN SOUND 350, 1
    sonido = 0
  END IF
  'Abajo
  IF yMovimientoPersonaje = 1 AND xMovimientoPersonaje = 0 THEN
    FOR revisionParedes = 0 TO 9
      IF POINT(xPersonaje + revisionParedes, yPersonaje + 10) = 7 THEN 'Pared de abajo
        yMovimientoPersonaje = -1
        sonido = 1
      END IF
    NEXT revisionParedes
    IF sonido = 1 THEN SOUND 350, 1
    sonido = 0
  END IF
  'Arriba
  IF yMovimientoPersonaje = -1 AND xMovimientoPersonaje = 0 THEN
    FOR revisionParedes = 0 TO 9
      IF POINT(xPersonaje + revisionParedes, yPersonaje - 1) = 7 THEN 'Pared de arriba
        yMovimientoPersonaje = 1
        sonido = 1
      END IF
    NEXT revisionParedes
    IF sonido = 1 THEN SOUND 350, 1
    sonido = 0
  END IF
  'Diagonal arriba-izquierda
  IF yMovimientoPersonaje = -1 AND xMovimientoPersonaje = -1 THEN
    FOR revisionParedes = 0 TO 9
      IF POINT(xPersonaje + revisionParedes, yPersonaje - 1) = 7 THEN 'Pared de arriba
        yMovimientoPersonaje = 1
        xMovimientoPersonaje = -1
        sonido = 1
      ELSEIF POINT(xPersonaje - 1, yPersonaje + revisionParedes) = 7 THEN 'Pared izquierda
        yMovimientoPersonaje = -1
        xMovimientoPersonaje = 1
        sonido = 1
      END IF
    NEXT revisionParedes
    IF sonido = 1 THEN SOUND 350, 1
    sonido = 0
  END IF
  'Diagonal arriba-derecha
  IF yMovimientoPersonaje = -1 AND xMovimientoPersonaje = 1 THEN
    FOR revisionParedes = 0 TO 9
      IF POINT(xPersonaje + revisionParedes, yPersonaje - 1) = 7 THEN 'Pared de arriba
        yMovimientoPersonaje = 1
        xMovimientoPersonaje = 1
        sonido = 1
      ELSEIF POINT(xPersonaje + 10, yPersonaje + revisionParedes) = 7 THEN 'Pared derecha
        yMovimientoPersonaje = -1
        xMovimientoPersonaje = -1
        sonido = 1
      END IF
    NEXT revisionParedes
    IF sonido = 1 THEN SOUND 350, 1
    sonido = 0
  END IF
  'Diagonal abajo-izquierda
  IF yMovimientoPersonaje = 1 AND xMovimientoPersonaje = -1 THEN
    FOR revisionParedes = 0 TO 9
      IF POINT(xPersonaje + revisionParedes, yPersonaje + 10) = 7 THEN 'Pared de abajo
        yMovimientoPersonaje = -1
        xMovimientoPersonaje = -1
        sonido = 1
      ELSEIF POINT(xPersonaje - 1, yPersonaje + revisionParedes) = 7 THEN 'Pared izquierda
        yMovimientoPersonaje = 1
        xMovimientoPersonaje = 1
        sonido = 1
      END IF
    NEXT revisionParedes
    IF sonido = 1 THEN SOUND 350, 1
    sonido = 0
  END IF
  'Diagonal abajo-derecha
  IF yMovimientoPersonaje = 1 AND xMovimientoPersonaje = 1 THEN
    FOR revisionParedes = 0 TO 9
      IF POINT(xPersonaje + revisionParedes, yPersonaje + 10) = 7 THEN 'Pared de abajo
        yMovimientoPersonaje = -1
        xMovimientoPersonaje = 1
        sonido = 1
      ELSEIF POINT(xPersonaje + 10, yPersonaje + revisionParedes) = 7 THEN 'Pared derecha
        yMovimientoPersonaje = 1
        xMovimientoPersonaje = -1
        sonido = 1
      END IF
    NEXT revisionParedes
    IF sonido = 1 THEN SOUND 350, 1
    sonido = 0
  END IF

  xPersonaje = xPersonaje + xMovimientoPersonaje
  yPersonaje = yPersonaje + yMovimientoPersonaje

END SUB

SUB obstaculos

  'xLinea = INT((305) * RND + 1)
  'yLinea = INT((171) * RND + 15)
  'longitudDeLinea = INT((81) * RND + 20)

  LINE (0, 9)-(320, 9), 1
  IF dibujarParedes = 1 THEN
    LINE (80, 50)-(80, 150), 7
    LINE (240, 50)-(240, 150), 7
    LINE (110, 100)-(210, 100), 7
  END IF
  IF activacionDeEscudo = 1 THEN
    CIRCLE (xPersonaje + 5, yPersonaje + 5), 12, 7
    CIRCLE (xPersonaje + 5, yPersonaje + 5), 13, 7
  END IF

END SUB

