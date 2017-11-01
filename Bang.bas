'Juego de tipo BangBang escrito en BASIC.
'Creado por Jose Joaquin Atria, en Santiago, Chile (baboso@mail.com)
'
'              Historial de versiones
'
'Hasta la v.0.5, hecha en algun momento de 2002 no se contabilizaron
'los cambios de version, por lo que los datos son posteriores a esta.
'v.0.5 - 2002: se agregan SUBs para hacer mas facil de entender el programa
'              y para que sea mas hermoso (...).
'v.0.6 - 2004: 12/12/2004 - he arreglado algunos problemas menores que
'              ten¡an que ver con disposiciones gr ficas (i.e. el cursor, el
'              cero del 100 al pasar a 0 y el tanque al hacer un nuevo
'              disparo). Sin embargo, queda por arreglar todo el soporte para
'              dos jugadores y al asunto de las l¡neas 900-1000, que a£n no
'              le permiten al programa enterarse de cu ndo se ha hecho un
'              acierto y cu ndo no. Por el momento, ignoro cu l es el error
'              en el algoritmo de la l¡nea 1000, que hoy modifiqu‚: al usar
'              la variable alty para comparar, la calculaba como (400 - alty),
'              aunque no s‚ por qu‚ hice eso.
'              14/12/2004 - entre ayer y hoy solucion‚ finalmente el problema
'              del algoritmo para los aciertos replanteando el asunto. Pens‚
'              que lo mejor era una acercamiento radicalmente distinto, pues
'              encontr‚ que el algoritmo anterior ten¡a demasiados agujeros.
'              Decid¡ por lo mismo re-escribirlo en t‚rminos de comparaciones
'              de la diferencia entre CINT(x) y CINT(altx), y CINT(y) y
'              CINT(alty). Esto hace adem s que el c¢digo sea m s hermoso
'              (pues todos saben que largas condicionales IF..THEN son infini-
'              tamente m s feas que una parsimoniosa comparaci¢n de diferen-
'              cias). Falta sin embargo escribir qu‚ es lo que pasa despu‚s.
'              Reduje adem s el STEP del c lculo de las coordenadas de la bala
'              de .006 a .004 para evitar situaciones embarazosas como las de
'              h = 78, r= 46; h = 81, r = 61; y h = 81, r = 63. Estas ya no
'              atraviesan el piso, pero ese tipo de errores me hacen pregun-
'              tarme si no deber¡a tambi‚n replantearme el c lculo del suelo,
'              quiz s en los mismos t‚rminos de comparaci¢n de diferencias.
'              15 - 16/12/2004 - efectivamente, un replanteamiento de la l¡nea
'              que calculaba los momentos en los que la bala chocaba contra el
'              piso logr¢ que la bala nunca m s pudiera atravesarlo, sin im-
'              portar el STEP que le pusiera. La aparici¢n de cada vez m s ca-
'              sos en los que la bala atravesaba el piso me llev¢ a disminuir
'              el STEP del ploteo hasta 0.003, velocidad en la que m s que
'              bala parec¡a algo muy lento, porque lo era. La soluci¢n fue tal
'              y como la esperaba: la l¡nea 760 tiene entonces ahora el
'              algoritmo IF ABS(yterrain(CINT(x))-CINT(y)) < 1.5 THEN GOTO 900
'              y funciona a la perfecci¢n. El uso de ABS() tambi‚n es nuevo y
'              surgi¢ como la medida obvia para solucionar un par de problemas
'              del algoritmo creado para la verificaci¢n de un impacto. El
'              £nico otro cambio digno de notar es la inclusi¢n del comando
'              RANDOMIZE TIMER para lograr posiciones realmente aleatorias del
'              enemigo/objetivo [ln. 185]. Para motivos de debug, esta l¡nea
'              puede omitirse y se generar  un e/o en las coordenadas y = -9,
'              x = 622.5726. Se puede impactar estas coordenadas con h = 78,
'              r = 45; h = 88, r = 64; y h = 100, r = 71; y por supuesto, un
'              n£mero infinito de otras combinaciones que a£n ignoro.
'              17/07/2005 - incluyo una linea (dibujada en la ln. 243) que
'              marca gr ficamente la separaci¢n entre el  rea en donde se im-
'              primen los datos y el  rea en donde se plotea la ruta del tiro.
'              Adem s, agregu‚ en el SUB definirgravedad la l¡nea RANDOMIZE
'              TIMER, que permite al programa generar terrenos realmente alea-
'              torios. Por alguna raz¢n, esta l¡nea no se hab¡a incluido antes
'              A pesar de que el programa funciona a la perfecci¢n con ella
'              (i.e. no pone al e/o bajo la l¡nea de terreno, y determina tam-
'              bi‚n una posicion aleatoria para este a lo largo de la misma).
'              Comenc‚ adem s la ardua labor de hacer mi c¢digo entendible,
'              pues me est  tomando demasiado tiempo precioso el entender lo
'              que hice en sesiones pasadas cuando vuelvo a taclear los pro-
'              blemas de BANG.BAS. La variable "g" ahora se llama exitosa-
'              mente "gravedad" y el SUB "gravedad" ahora se llama "definir-
'              gravedad" (debido a una duplicaci¢n de etiquetas). Debo termi-
'              nar esto lo antes posible.


DECLARE SUB terrain ()
DECLARE SUB instrucciones ()
DECLARE SUB definirgravedad ()
DECLARE SUB victoria ()
DECLARE SUB menu ()
DECLARE SUB seguro ()
DECLARE SUB fin ()
DECLARE SUB teclas ()
DIM SHARED a AS DOUBLE
DIM SHARED b AS DOUBLE
DIM SHARED h AS DOUBLE
DIM SHARED x AS DOUBLE
DIM SHARED y AS DOUBLE
DIM SHARED haches(20)
DIM SHARED erres(20)
DIM SHARED yterrain(640) 'que ser  el array en el que se guardar n las
                         'coordenadas del terreno para evitar que la bala
                         'lo atraviese.
COMMON SHARED gravedad, var, k, counter, sure$, sure3$, tec$, r, player, altx, alty
player = 1 'valor por defecto: un s¢lo jugador.
gravedad = 4.9
alter = 1

'BangBang!
CALL menu 'Primera subrutina que crea el menu en la pantalla y lee opciones.
20 counter = 1 'Counter es la variable que indica el numero de tiro.
30 SCREEN 12
40 var = 0 'var es una variable generica usada practicamente para todas las variables que el usuario ingrese. Principalmente, es la variable que indica tanto el angulo como la velocidad.
50 k = (3.141593 / 180) 'Constante para transformar los senos y cosenos de radianes a grados.
60 LOCATE 1, 1: PRINT "Velocidad =": LOCATE 1, 13: PRINT " 0   "

70 IF player = 1 THEN 'Se dibuja la pantalla de juego, diferente si hay uno o dos jugadores (indicado por la variable "player").
80      LOCATE 1, 40: PRINT "Este es tu tiro numero "; counter
90 ELSE
100     LOCATE 1, 40: PRINT "Velocidad:"
110     LOCATE 2, 40: PRINT "Angulo de Elevacion:"
120 END IF

130 LOCATE 2, 1: PRINT "Angulo de Elevacion =": LOCATE 2, 23: PRINT " 0   "
140 LOCATE 1, 75: PRINT "     "
150 LOCATE 2, 75: PRINT "     "
160 LOCATE 3, 1: PRINT "                                     "

170 'Con estas lineas se establece el interfaz de juego y la posicion inicial del cursor.
180 IF counter = 1 THEN terrain 'Rutina de generacion aleatoria del terreno.
185 'RANDOMIZE TIMER
190 altx = (RND * 400) + 231 'generaci¢n de la posici¢n del objetivo/enemigo.
200 alty = yterrain(altx)
210 CIRCLE (altx, 400 - alty), 50, 5 'dibujo de un c¡rculo alrededor del o/e para facilitar su ubicaci¢n.
220 LINE (altx, 395 - alty)-(altx + 11, 400 - alty), 4, BF: LINE (altx + 11, 395 - alty)-(altx + 11, 384 - alty), 0, BF: LINE (1, 395)-(11, 395), 13 'dibujo del o/e en la posici¢n determinada previamente.
230 LINE (1, 395)-(11, 400), 13, BF: LINE (1, 395)-(11, 386), 0, BF: LINE (1, 395)-(11, 395), 13 'dibujo del tanque del jugador.
240 var = 0: LOCATE 1, 35: PRINT "®": LOCATE 3, 1: PRINT "               ": LOCATE 2, 35: PRINT " "
241 IF player = 1 THEN LOCATE 1, 40: PRINT "Este es tu tiro numero "; counter
242 LINE (1, 395)-(1 + COS(r2 * (k)) * 10, 1 + 394 - SIN(r2 * (k)) * 10), 0
243 LINE (1, 49)-(640, 49), 15

245 DO 'Primer Loop. Seleccion de la Velocidad.           
250   teclas 'Lee Teclas
260   IF var > 100 THEN var = 0 ELSE IF var < 0 THEN var = 100
265   IF var = 0 THEN LOCATE 1, 16: PRINT " " 'soluciona un problema con el
                                              'ultimo caracter de 100 al
                                              'volver a 0.
270   h = var
280   IF tec$ = "8" OR tec$ = "2" THEN
LOCATE 1, 13: PRINT h
LOCATE 10, 20: PRINT "         "
END IF
290   IF tec$ = "5" THEN IF var = 0 THEN 240 ELSE GOTO 310
300 LOOP
310 var = 0: LOCATE 1, 35: PRINT " " 'Cambio de la posicion del cursor.
320 LOCATE 2, 35: PRINT "®"
330 DO 'Segundo Loop. Seleccion del Angulo.
340   teclas 'Lee Teclas
345   IF player = 2 THEN alter = alter * -1
350   IF var > 90 THEN var = 0 ELSE IF var < 0 THEN var = 90
360   IF tec$ = "8" OR tec$ = "2" THEN LINE (1, 395)-(1 + COS(r2 * (k)) * 10, 1 + 394 - SIN(r2 * (k)) * 10), 0
370   r = 90 - var: r2 = var
380   IF tec$ = "8" OR tec$ = "2" THEN LOCATE 2, 23: PRINT var
390   IF tec$ = "5" THEN IF var = 0 THEN 330 ELSE GOTO 670
400   LINE (1, 395)-(1 + COS(r2 * (k)) * 10, 1 + 394 - SIN(r2 * (k)) * 10), 13
410 LOOP
415 LOCATE 2, 35: PRINT " "
417 IF player = 2 THEN
420 LOCATE 3, 1: PRINT "               ": LOCATE 2, 35: PRINT " ": LOCATE 1, 35: PRINT "¯"
430 var = 0
435    DO 'Primer Loop del segundo jugador. Seleccion de la Velocidad.
440    teclas 'Lee Teclas
450      IF var > 100 THEN var = 0 ELSE IF var < 0 THEN var = 100
455      IF var = 0 THEN LOCATE 1, 53: PRINT " "
460       h = var
470       IF tec$ = "8" OR tec$ = "2" THEN LOCATE 1, 50: PRINT h
480       IF tec$ = "5" THEN IF var = 0 THEN 435 ELSE GOTO 500
490    LOOP
500    var = 0: LOCATE 1, 35: PRINT " " 'Cambio de la posicion del cursor.
510    LOCATE 2, 35: PRINT "¯"
560    DO 'Segundo Loop del segundo jugador. Seleccion del Angulo.
570      teclas 'Lee Teclas
580      IF var > 90 THEN var = 0 ELSE IF var < 0 THEN var = 90
590      IF tec$ = "8" OR tec$ = "2" THEN LINE (1, 395)-(1 + COS(r2 * (k)) * 10, 1 + 394 - SIN(r2 * (k)) * 10), 0
600      r = 90 - var: r2 = var
610      IF tec$ = "8" OR tec$ = "2" THEN LOCATE 2, 60: PRINT var
620      IF tec$ = "5" THEN IF var = 0 THEN 560 ELSE alter = alter * -1: GOTO 650
630      LINE (1, 395)-(1 + COS(r2 * (k)) * 10, 1 + 394 - SIN(r2 * (k)) * 10), 13
640    LOOP
650 LOCATE 2, 35: PRINT " "
660 END IF

670 INPUT "Estas seguro"; sure$ 'Rutina de ploteo del tiro. "h" es la velocidad
                                'y "r" es la elevaci¢n en grados.
680 IF sure$ = "s" THEN GOTO 690 ELSE IF sure$ = "n" THEN GOTO 60
haches(counter) = h
erres(counter) = r
685 counter = counter + 1
690 a = h * SIN(r * (k))
700 b = h * COS(r * (k))
710     FOR t = 1 TO 300 STEP .005
715     IF y < 350 THEN 'este bloque if y el de 765 - 775 impiden que se plotee
                        'la ruta de la bala sobre los datos ya impresos en la
                        'pantalla
720             PSET (x, 400 - y), counter
725     END IF
730     x = a * t
740     y = b * t - gravedad * t ^ 2
750     IF CINT(x) <= 640 THEN
760             IF ABS(yterrain(CINT(x)) - CINT(y)) < 1.5 THEN GOTO 900
765             IF y < 350 THEN
770                     PSET (x, 400 - y), 7
775             END IF
780     ELSE LOCATE player + 1, 40: PRINT "Fuera del rango!": GOTO 900
790     END IF
800     LOCATE 1, 69 'Se imprimen las coordenadas (en enteros aproximados) en
                     'la esquina superior derecha de la pantalla.
810     PRINT "x ="
820     LOCATE 1, 75
830     PRINT CINT(x)
840     LOCATE 2, 69
850     PRINT "y ="
860     LOCATE 2, 75
870     PRINT CINT(y)
880     'SOUND CINT(y) + 500, .05
885     IF ABS(alty - CINT(y)) < 10 AND ABS(altx - CINT(x)) < 10 THEN LOCATE 10, 20: PRINT " You Win!": CALL victoria: GOTO 185: 'boom = 2
890   NEXT t
900
1000  IF alter = -1 THEN GOTO 417 ELSE GOTO 240: 'stp = 1
      


1010 'DO UNTIL boom < 2
1020 'boom = boom + stp
1030 'CIRCLE (CINT(x), 400 - CINT(y)), boom, 12
1040 'IF boom > 20 THEN stp = stp * -1
1050 'IF stp = -1 THEN CIRCLE (CINT(x), 400 - CINT(y)), boom, 0
1060 'FOR i = 12 TO 640
1070 'PSET (x, yterrain(i)), 13
1080 'NEXT
1090 'LOOP

1100 counter = counter + 1 'counter es la cantidad de veces que se ha
                           'disparado.
1110 IF player = 1 THEN
1120    LOCATE 3, 1: INPUT "Quieres hacer otro tiro"; again$
1130    IF again$ = "s" OR again$ = "si" OR again$ = "S" OR again$ = "Si" OR again$ = "SI" THEN CIRCLE (1, 400), 30, 0: GOTO 30 ELSE fin
1140    ELSE GOTO 410
1150 END IF

SUB definirgravedad
CLS 'Seleccion de la gravedad.
PRINT "Que fuerza de gravedad quieres que haya en el mundo?"
INPUT "En la Tierra es 9.8. En este juego, el valor por defecto es 4.9."; gravedad
END SUB

SUB fin
CLS 'Fin del programa.
COLOR 1
LOCATE 10, 38: PRINT "Adios!"
END
END SUB

SUB instrucciones
CLS 'Instrucciones
PRINT
PRINT "     Al iniciar el juego ingresa con los numeros 8 y 2 del teclado numerico los"
PRINT "valores correspondientes a la velocidad y al angulo que le quieres dar a tu tiro"
PRINT "(fijate en tu cursor (®) ). Si presionas la tecla 'q' en cualquier momento, te "
PRINT "saldras del juego."
PRINT
PRINT "     La opcion '(3) Seleccionar dificultad' se refiere a la eficiencia de la in-"
PRINT "teligencia artificial del oponente manejado por el computador en el juego de un "
PRINT "jugador. A mayor dificultad, mas rapidamente el oponente te destruira. "
PRINT
PRINT "     En este momento, como es solo una version parcial (v.0.6), no se ha comple-"
PRINT "tado el soporte para multijugador. Se hace lo que se puede..."
PRINT
PRINT "                                        Presiona cualquier tecla para continuar"
SLEEP
'GOTO 10
END SUB

SUB menu
CLS : SCREEN 0
COLOR 1
PRINT "                                                                 "
PRINT "               °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° "
PRINT "               °°±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±°° "
PRINT "               °°±±²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²±±°° "
PRINT "               °°±±²²": COLOR 6: LOCATE 5, 35: PRINT "BANG BANG!": COLOR 1: LOCATE 5, 59: PRINT "²²±±°° "
PRINT "               °°±±²²                                     ²²±±°° "
PRINT "               °°±±²²": COLOR 7: LOCATE 7, 23: PRINT "(1) Jugar": COLOR 1: LOCATE 7, 59: PRINT "²²±±°° "
PRINT "               °°±±²²": COLOR 7: LOCATE 8, 23: PRINT "(2) Seleccionar gravedad": COLOR 1: LOCATE 8, 59: PRINT "²²±±°° "
PRINT "               °°±±²²": COLOR 7: LOCATE 9, 23: PRINT "(3) Selecionar dificultad": COLOR 1: LOCATE 9, 59: PRINT "²²±±°° "
PRINT "               °°±±²²": COLOR 7: LOCATE 10, 23: PRINT "(4) 2 jugadores": COLOR 1: LOCATE 10, 59: PRINT "²²±±°° "
PRINT "               °°±±²²": COLOR 7: LOCATE 11, 23: PRINT "(5) Instrucciones": COLOR 1: LOCATE 11, 59: PRINT "²²±±°° "
PRINT "               °°±±²²": COLOR 7: LOCATE 12, 23: PRINT "(6) Salir": COLOR 1: LOCATE 12, 59: PRINT "²²±±°° "
PRINT "               °°±±²²                                     ²²±±°° "
PRINT "               °°±±²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²±±°° "
PRINT "               °°±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±°° "
PRINT "               °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°° "
COLOR 7: PRINT "                   Asegurate de que el BloqNum este activado"
LOCATE 23, 50: PRINT "(c) 2002 Jose J. Atria"
LOCATE 24, 67: PRINT "v.0.6"
COLOR 7
DO
  men$ = INKEY$
  SELECT CASE men$
    CASE "1": EXIT SUB
    CASE "2": definirgravedad: menu
    CASE "3": EXIT SUB
    CASE "4": player = 2: EXIT SUB
    CASE "5": instrucciones: menu
    CASE "6": seguro
  END SELECT
LOOP
END SUB

SUB seguro
CLS : COLOR 1
LOCATE 10, 30: PRINT "ESTAS SEGURO? S/N"
DO
sure3$ = INKEY$
IF sure3$ = "S" OR sure3$ = "s" THEN fin
IF sure3$ = "n" OR sure3$ = "N" THEN menu
LOOP
END SUB

SUB teclas
tec$ = INKEY$: 'Lectura de Teclas y modificacion de variable
IF tec$ = "8" THEN var = var + 1
IF tec$ = "2" THEN var = var - 1
IF tec$ = "q" OR tec$ = "Q" THEN fin
IF tec$ = "M" OR tec$ = "m" THEN menu
END SUB

SUB terrain
'Rutina de generacion aleatoria del terreno.
LINE (1, 300)-(640, 500), 0, BF ' Limpia el terreno
x = 1
yt = 400
RANDOMIZE TIMER
FOR t = 1 TO 640
  a = INT(RND * 5)
  'IF a = 0 THEN yt = yt + 2: PSET (x, yt - 1)
  IF a = 0 THEN yt = yt + 1
  IF a = 1 THEN yt = yt - 1
  'IF a = 3 THEN yt = yt - 2: PSET (x, yt + 1)
  'IF a = 4 THEN x = x + 1: PSET (x - 1, yt)
  'IF a = 5 THEN yt = yt + 2: PSET (x, yt - 1)
  IF a = 2 THEN yt = yt + 1
  IF a = 3 THEN yt = yt - 1
  'IF a = 8 THEN yt = yt - 2: PSET (x, yt + 1)
  'IF a = 9 THEN x = x + 1: PSET (x - 1, yt)
  'IF a = 10 THEN x = x + 2: PSET (x - 2, yt): PSET (x - 1, yt)
  IF a = 4 THEN yt = yt
  PSET (x, yt)
  x = x + 1
  yterrain(t) = 400 - yt
NEXT
END SUB

SUB victoria
   
FOR tiros = 1 TO counter - 1
        a = haches(tiros) * SIN(erres(tiros) * (k))
        b = haches(tiros) * COS(erres(tiros) * (k))
        FOR t = 1 TO 300 STEP .005
                IF y < 350 THEN 'este bloque if y el de 765 - 775 impiden que se plotee
                                'la ruta de la bala sobre los datos ya impresos en la
                                'pantalla
                        PSET (x, 400 - y), 0
                END IF
                x = a * t
                y = b * t - gravedad * t ^ 2
        NEXT t
NEXT tiros

CIRCLE (altx, 400 - alty), 50, 0 'dibujo de un c¡rculo alrededor del o/e para facilitar su ubicaci¢n.
LINE (altx, 395 - alty)-(altx + 11, 400 - alty), 0, BF: LINE (altx + 11, 395 - alty)-(altx + 11, 384 - alty), 0, BF: LINE (1, 395)-(11, 395), 13 'dibujo del o/e en la posici¢n determinada previamente.

FOR x = 1 TO 640
        PSET (x, 400 - yterrain(x))
NEXT
  
END SUB

