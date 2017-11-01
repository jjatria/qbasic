'En este juego el protagonista () intenta comerse los  antes de que
'su hermano gemelo malvado () lo atrape. El protagonista es controlado
'mediante las teclas del numpad (siempre y cuando el BloqNum este activado.
'Para el movimiento de  existen las variables "xperseguidor" y "ypersegui-
'dor ", que" corresponden a las coordenadas "xpersonaje" e "ypersonaje" del
'"perseguidor", y que tienden a acercarse SIEMPRE a las variables "xpersonaje" e "ypersonaje"
'(las coordenadas de ).
'Al inicio del programa se le pide al usuario que ingrese el nivel de
'dificultad (1 o 2) [ln.100]. Este valor tiene importancia a la hora de
'calcular la cantidad de vidas iniciales [ln.110], la velocidad del
'movimiento de  [ln.280] y la cantidad de movidas de  iniciales (variable
'qw) [ln460].
'Existen tres variables esenciales para el juego. Estas son: "counter", que
'le dice al programa la cantidad de veces que  se ha movido; "counter2",
'que cuenta los treboles comidos; y "countercheck", que se encarga de
'decirle al programa que el jugador se ha comido un trebol y es hora de
'poner otro.
'Las paredes son otro tema. Mi objetivo es crear una serie de modulos a partir
'de los cuales el programa, con alguna funcion RANDOMIZE, pueda crear un mapa
'"original" cada vez que se juege (claro que a la octava vez que se juege ya
'conoceran todas las posibles combinaciones de modulos). La verdad, es que aun
'no he logrado tener exito en ese proyecto (pero bueno, por eso mismo no
'considero que este programa este listo). Tambien falta hacer una portada y un
'menu, en donde eventualmente se puedan configurar controles, prender o apagar
'el sonido (que a veces es un tanto irritante, de hecho, debo hacer un mejor
'uso de el, o sacarlo), leer un readme con instrucciones (que probablemente
'se parezca notablemente  este texto) y algo con creditos, ademas de la opcion
'de salirse.

10 CLS : COLOR 7
20 lives = 6
30 xpersonaje = 1
40 ypersonaje = 1
50 yperseguidor = 22
60 xperseguidor = 80
70 counter = 0
80 counter2 = 0
90 countercheck = 0
100 INPUT "Selecciona la dificultad del juego (1-2): ", dificultad: IF dificultad > 2 THEN 100 ELSE IF dificultad < 1 THEN 100
110 lives = lives - (dificultad * 2)
120 CLS

130 DO 'Inicia el LOOP principal
140 IF lives = 0 THEN 890
150 IF counter2 = countercheck THEN GOSUB 770 ELSE 160: 'Frutas!
160 IF xpersonaje < 1 THEN xpersonaje = 80 ELSE IF xpersonaje > 80 THEN xpersonaje = 1
170 IF ypersonaje < 1 THEN ypersonaje = 21 ELSE IF ypersonaje > 21 THEN ypersonaje = 1
180 IF xpersonaje = 40 AND ypersonaje < 18 AND ypersonaje > 10 THEN xpersonaje = 39
190 IF xpersonaje = 41 AND ypersonaje < 18 AND ypersonaje > 10 THEN xpersonaje = 42
200 IF xpersonaje > 9 AND xpersonaje < 40 AND ypersonaje = 17 THEN ypersonaje = 16
210 IF ypersonaje = 18 AND xpersonaje < 47 AND xpersonaje > 9 THEN ypersonaje = 19
220 IF ypersonaje = 17 AND xpersonaje < 47 AND xpersonaje > 41 THEN ypersonaje = 16
230 IF xpersonaje = rndx AND ypersonaje = rndy THEN counter2 = counter2 + 1
240 GOSUB 550: 'Posicion del personaje
250 GOSUB 990: 'Posicion del perseguidor
260 GOSUB 590: 'Paredes 
270 GOSUB 530: 'Lee teclas
280 IF dificultad = 1 THEN dif = .5 ELSE IF dificultad = 2 THEN dif = .75
290 IF teclas$ = "1" OR teclas$ = "2" OR teclas$ = "3" OR teclas$ = "4" OR teclas$ = "5" OR teclas$ = "6" OR teclas$ = "7" OR teclas$ = "8" OR teclas$ = "9" THEN IF xpersonaje > xperseguidor THEN xperseguidor = xperseguidor + dif: LOCATE  _
yperseguidor, xperseguidor - dif: PRINT " "
300 IF teclas$ = "1" OR teclas$ = "2" OR teclas$ = "3" OR teclas$ = "4" OR teclas$ = "5" OR teclas$ = "6" OR teclas$ = "7" OR teclas$ = "8" OR teclas$ = "9" THEN IF xpersonaje < xperseguidor THEN xperseguidor = xperseguidor - dif: LOCATE  _
yperseguidor, xperseguidor + dif: PRINT " "
310 IF teclas$ = "1" OR teclas$ = "2" OR teclas$ = "3" OR teclas$ = "4" OR teclas$ = "5" OR teclas$ = "6" OR teclas$ = "7" OR teclas$ = "8" OR teclas$ = "9" THEN IF ypersonaje > yperseguidor THEN yperseguidor = yperseguidor + dif: LOCATE  _
yperseguidor - dif, xperseguidor: PRINT " "
320 IF teclas$ = "1" OR teclas$ = "2" OR teclas$ = "3" OR teclas$ = "4" OR teclas$ = "5" OR teclas$ = "6" OR teclas$ = "7" OR teclas$ = "8" OR teclas$ = "9" THEN IF ypersonaje < yperseguidor THEN yperseguidor = yperseguidor - dif: LOCATE  _
yperseguidor + dif, xperseguidor: PRINT " "
330 IF teclas$ = "1" OR teclas$ = "2" OR teclas$ = "3" OR teclas$ = "4" OR teclas$ = "5" OR teclas$ = "6" OR teclas$ = "7" OR teclas$ = "8" OR teclas$ = "9" THEN IF xpersonaje = CINT(xperseguidor) AND ypersonaje = CINT(yperseguidor) THEN counter2 =  _
counter2 - 1: lives = lives - 1
340 IF CINT(yperseguidor) = rndy AND CINT(xperseguidor) = rndx THEN counter2 = counter2 - 1: countercheck = countercheck - 2
350 IF teclas$ = "8" THEN ypersonaje = ypersonaje - 1: LOCATE ypersonaje + 1, xpersonaje: PRINT " ": counter = counter + 1
360 IF teclas$ = "7" THEN ypersonaje = ypersonaje - 1: xpersonaje = xpersonaje - 1: LOCATE ypersonaje + 1, xpersonaje + 1: PRINT " ": counter = counter + 1
370 IF teclas$ = "9" THEN ypersonaje = ypersonaje - 1: xpersonaje = xpersonaje + 1: LOCATE ypersonaje + 1, xpersonaje - 1: PRINT " ": counter = counter + 1
380 IF teclas$ = "2" THEN ypersonaje = ypersonaje + 1: LOCATE ypersonaje - 1, xpersonaje: PRINT " ": counter = counter + 1
390 IF teclas$ = "4" THEN xpersonaje = xpersonaje - 1: LOCATE ypersonaje, xpersonaje + 1: PRINT " ": counter = counter + 1
400 IF teclas$ = "3" THEN xpersonaje = xpersonaje + 1: ypersonaje = ypersonaje + 1: LOCATE ypersonaje - 1, xpersonaje - 1: PRINT " ": counter = counter + 1
410 IF teclas$ = "6" THEN xpersonaje = xpersonaje + 1: LOCATE ypersonaje, xpersonaje - 1: PRINT " ": counter = counter + 1
420 IF teclas$ = "1" THEN xpersonaje = xpersonaje - 1: ypersonaje = ypersonaje + 1: LOCATE ypersonaje - 1, xpersonaje + 1: PRINT " ": counter = counter + 1
430 IF teclas$ = "r" OR teclas$ = "R" THEN 10
440 IF teclas$ = "q" OR teclas$ = "Q" THEN 890
450 IF teclas$ = "1" OR teclas$ = "2" OR teclas$ = "3" OR teclas$ = "4" OR teclas$ = "5" OR teclas$ = "6" OR teclas$ = "7" OR teclas$ = "8" OR teclas$ = "9" THEN SOUND 132 + (2 * qw), .5
460 qw = (1000 * (dificultad * .5)) - counter
470 LOCATE 22, 1: PRINT qw; "Apurate! Come treboles! ---> "; counter2; CHR$(5); "   ": LOCATE 22, 50: PRINT CHR$(5); " : x="; rndx; "y="; rndy
480 IF counter2 = 15 THEN counter = -100 ELSE IF counter2 = 30 THEN counter = -50
490 LOCATE 23, 2: PRINT "Te queda(n)"; lives; "vida(s)."
500 LOCATE 23, 38: PRINT "x= "; xpersonaje; ". y= "; ypersonaje; "/ x2= "; chsrx; "  ": LOCATE 23, 65: PRINT "y2= "; chsry; "  "
510 IF qw = 0 THEN 890
520 LOOP

530 'Lee teclas
540 teclas$ = INKEY$: RETURN

550 'Posicion del personaje
560 LOCATE ypersonaje, xpersonaje
570 COLOR 4: PRINT CHR$(1): COLOR 7
580 RETURN
590 'Paredes
600 FOR t = 1 TO 5
610 FOR p = 1 TO 2
620 py = 11 + t: px = 39 + p
630 LOCATE py, px
640 COLOR 2: PRINT CHR$(179)
650 LOCATE 11, 40: PRINT CHR$(218); CHR$(191)
660 LOCATE 17, 40: PRINT CHR$(217); CHR$(192)
670 NEXT p: NEXT t
680 FOR px2 = 1 TO 29: FOR py2 = 1 TO 2
690 LOCATE 16 + py2, 10 + px2: PRINT CHR$(196): NEXT py2: NEXT px2
700 LOCATE 18, 10: PRINT CHR$(192): LOCATE 17, 10: PRINT CHR$(218)
710 FOR px3 = 1 TO 4
720 LOCATE 17, 41 + px3: PRINT CHR$(196): NEXT px3
730 FOR px4 = 1 TO 6
740 LOCATE 18, 39 + px4: PRINT CHR$(196): NEXT px4
750 LOCATE 18, 46: PRINT CHR$(217): LOCATE 17, 46: PRINT CHR$(191)
760 COLOR 7: RETURN
770 'Frutas!
780 RANDOMIZE INT(RND * 6) + counter
790 rndy = INT(RND * 20) + 1
800 rndx = INT(RND * 79) + 1
810 IF rndx = 40 AND rndy < 18 AND rndy > 10 THEN 780
820 IF rndx = 41 AND rndy < 18 AND rndy > 10 THEN 780
830 IF rndx > 9 AND rndx < 40 AND rndy = 17 THEN 780
840 IF rndy = 18 AND rndx < 47 AND rndx > 9 THEN 780
850 IF rndy = 17 AND rndx < 47 AND rndx > 41 THEN 780
860 LOCATE rndy, rndx: PRINT CHR$(5)
870 countercheck = countercheck + 1
880 RETURN

890 'Termino (Los mensajes de abajo son la unica wea que no he arreglado y me da paja hacerlo. No es muy dificil, en todo caso y no me extranaria si alguien mas lo hiciera)
900 CLS
910 IF counter2 < 0 THEN PRINT "Tu malvado hermano gemelo te quito"; -(counter2); "preciado(s) trebol(es)." ELSE PRINT "Te comiste "; counter2; "treboles."
920 IF counter2 > 5 THEN PRINT "Nunca pense que alguien leyera esto. Estas seguro que no estas en coma?"
930 IF counter2 > 10 THEN PRINT "VAMOS! Cualquiera lo puede hacer mejor que tu!"
940 IF counter2 > 15 THEN PRINT "Bien hecho. Te mereces saber que el tiempo que jugaste fue tiempo que perdiste"
950 IF counter2 > 20 THEN PRINT "Estas hecho todo un suertudo (porque de talento en este jueguito no hay nada)."
960 IF counter2 >= 40 THEN PRINT "Bienvenido al Salon de la Fama de la Asociacion de Jugadores de Juegos Imbeciles": LOCATE 22, 1: PRINT "Y recuerda: 'Estar en el salon de la fama de la A.J.J.I es TODO UN HONOR!'"
970 IF counter2 > 50 THEN PRINT "Me quito el sombrero ante ti. Nunca has pensado en jugar poker?"
980 SLEEP: END


990 'Posicion del perseguidor
1000 IF yperseguidor < 1 THEN yperseguidor = 1 ELSE IF yperseguidor > 21 THEN yperseguidor = 21
1010 IF xperseguidor < 1 THEN xperseguidor = 1 ELSE IF xperseguidor > 80 THEN xperseguidor = 80
1020 IF xperseguidor = 40 AND yperseguidor < 18 AND yperseguidor > 10 THEN xperseguidor = 39
1030 IF xperseguidor = 41 AND yperseguidor < 18 AND yperseguidor > 10 THEN xperseguidor = 42
1040 IF xperseguidor > 9 AND xperseguidor < 40 AND yperseguidor = 17 THEN yperseguidor = 16
1050 IF yperseguidor = 18 AND xperseguidor < 47 AND xperseguidor > 9 THEN yperseguidor = 19
1060 IF yperseguidor = 17 AND xperseguidor < 47 AND xperseguidor > 41 THEN yperseguidor = 16
1070 COLOR 4: LOCATE CINT(yperseguidor), CINT(xperseguidor): PRINT CHR$(2): COLOR 7
1080 RETURN


