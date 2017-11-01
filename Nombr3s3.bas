' Version History
' 13 / 07 / 2005: Comienzo a escribir este programa debido a un renacido
'                 interes en un programa generador de nombres mucho mas simple
'                 escrito hace un par de a¤os. Aquel estaba construido en base
'                 a letras (alternaba vocales y consonantes), y aunque tenia
'                 resultados satisfactorios, no era todo lo que en un momento
'                 espere de el. Por eso, decidi retomar una idea que habia
'                 tenido de hacer un programa similar pero que funcionara
'                 alternando silabas incluidas en una base de datos mediante
'                 el empleo de la secuencia READ...DATA. Hoy se incluyen todas
'                 las silabas de dos letras (trabadas y no) y las de una, y la
'                 primera tanda de silabas de tres letras (las con la liquida
'                 <l>). Espero que en el futuro pueda ajustar correctamente la
'                 frecuencia de cada silaba. El problema de los espacios quedo
'                 arreglado y sera facil mantenerlo funcionando, pues solo
'                 requiere de un correcto conteo y orden de las silabas en los
'                 DATA. Seria optimo, sin embargo, encontrar alguna manera de
'                 hacerlo funcionar sin los bloques if que tanto detesto.
'                 Trate de hacerlo con SELECT CASE...END SELECT, pero por
'                 alguna razon, no me funciono.
' 15 / 07 / 2005: Finalmente logre solucionar el problema del INPUT: por algu-
'                 na razon, el programa leia el input sin imprimir el prompt
'                 por lo que uno nunca se enteraba de que tenia que escribir.
'                 Como pedia ademas presionar una tecla al empezar, y general-
'                 mente uno apretaba <ENTER>, el INPUT guardaba <ENTER> y se
'                 lo saltaba. Solucione ese problema encerrando el SLEEP en un
'                 LOOP que se repite mientras no se presione ninguna tecla
'                 (INKEY$ = ""). Ahora, el programa pregunta si se repite y lo
'                 hace sin problemas. Falta solucionar el problema de los dip-
'                 tongos y de los triptongos, asi como el de los nombres poco
'                 hermosos. Necesito hacer algun algoritmo que evalue los nom-
'                 bres.


repetir = 1
50
CLS
PRINT "Programa generador de nombres v2.0"
PRINT "por Jos‚ Joaqu¡n Atria"
PRINT "Presiona cualquier tecla para continuar"

IF repetir = 1 THEN
  DO WHILE INKEY$ = ""
  LOOP
END IF

espacio = 1
linea = 5
veces = 18
FOR linea = 5 TO 18 + 5
  PRINT "                                                                              "
NEXT
linea = 5
FOR cantidad = 1 TO veces
  FOR longitud = 1 TO 3
    RANDOMIZE TIMER
    RESTORE 100
    char = INT((449 - 1 + 1) * RND + 1)
    FOR silaba = 1 TO char
      READ silaba$
    NEXT
    LOCATE linea, espacio
    PRINT silaba$
    IF espacio = 1 THEN
      LOCATE linea, espacio
      PRINT UCASE$(LEFT$(silaba$, 1))
    END IF
    SELECT CASE char
      CASE IS > 488
        espacio = espacio + 3
      CASE IS < 489 AND char > 18
        espacio = espacio + 2
      CASE IS < 19
        espacio = espacio + 1
    END SELECT
  NEXT
  espacio = 1
  linea = linea + 1
NEXT

INPUT again$
IF again$ = "s" THEN
  repetir = 2
  GOTO 50
END IF

100
DATA a, a, a, a, a, a, e, e, e, e, i, i, o, o, o, o, u, u                   : 'estas tienen 18 silabas cada una
DATA ae, ai, ao, au, ea, ei, eo, eu, ia, ie, io, iu, oa, oe, oi, ou, ua, ue, ui, uo
DATA ba, ba, ba, ba, ba, ba, be, be, be, be, bi, bi, bo, bo, bo, bo, bu, bu
DATA ca, ca, ca, ca, ca, ca, ce, ce, ce, ce, ci, ci, co, co, co, co, cu, cu
DATA da, da, da, da, da, da, de, de, de, de, di, di, do, do, do, do, du, du
DATA fa, fa, fa, fa, fa, fa, fe, fe, fe, fe, fi, fi, fo, fo, fo, fo, fu, fu
DATA ga, ga, ga, ga, ga, ga, ge, ge, ge, ge, gi, gi, go, go, go, go, gu, gu
DATA ja, ja, ja, ja, ja, ja, je, je, je, je, ji, ji, jo, jo, jo, jo, ju, ju
DATA ka, ka, ka, ka, ka, ka, ke, ke, ke, ke, ki, ki, ko, ko, ko, ko, ku, ku
DATA la, la, la, la, la, la, le, le, le, le, li, li, lo, lo, lo, lo, lu, lu
DATA ma, ma, ma, ma, ma, ma, me, me, me, me, mi, mi, mo, mo, mo, mo, mu, mu
DATA na, na, na, na, na, na, ne, ne, ne, ne, ni, ni, no, no, no, no, nu, nu
DATA pa, pa, pa, pa, pa, pa, pe, pe, pe, pe, pi, pi, po, po, po, po, pu, pu
DATA ra, ra, ra, ra, ra, ra, re, re, re, re, ri, ri, ro, ro, ro, ro, ru, ru
DATA sa, sa, sa, sa, sa, sa, se, se, se, se, si, si, so, so, so, so, su, su
DATA ta, ta, ta, ta, ta, ta, te, te, te, te, ti, ti, to, to, to, to, tu, tu
DATA va, va, va, va, va, va, ve, ve, ve, ve, vi, vi, vo, vo, vo, vo, vu, vu : 'hasta aqui van 308 silabas
DATA wa, wa, we, we, wi, wi, wo, wo, wu, wu
DATA xa, xe, xi, xo, xu
DATA ya, ye, yi, yo, yu
DATA za, ze, zi, zo, zu
DATA ab, ab, ab, eb, eb, ib, ob, ob, ub
DATA ac, ac, ac, ec, ec, ic, oc, oc, uc
DATA ad, ad, ad, ed, ed, id, od, od, ud
DATA af, af, af, ef, ef, if, of, of, uf
DATA ag, ag, ag, eg, eg, ig, og, og, ug
DATA aj, aj, aj, ej, ej, ij, oj, oj, uj
DATA ak, ak, ak, ek, ek, ik, ok, ok, uk
DATA al, al, al, el, el, il, ol, ol, ul
DATA am, am, am, em, em, im, om, om, um
DATA an, an, an, en, en, in, on, on, un
DATA ap, ap, ap, ep, ep, ip, op, op, up
DATA ar, ar, ar, er, er, ir, or, or, ur
DATA as, as, as, es, es, is, os, os, us
DATA at, at, at, et, et, it, ot, ot, ut
DATA av, av, av, ev, ev, iv, ov, ov, uv
DATA aw, ew, iw, ow, uw
DATA ax, ex, ix, ox, ux
DATA ay, ey, iy, oy, uy
DATA az, ez, iz, oz, uz                                                     : 'hasta aqui van 488 silabas (308 + 180)
DATA cha, cha, cha, che, che, chi, cho, cho, chu
DATA bla, bla, bla, ble, ble, bli, blo, blo, blu
DATA cla, cla, cla, cle, cle, cli, clo, clo, clu
DATA dla, dle, dli, dlo, dlu
DATA fla, fla, fla, fle, fle, fli, flo, flo, flu
DATA gla, gla, gla, gle, gle, gli, glo, glo, glu
DATA kla, kla, kla, kle, kle, kli, klo, klo, klu
DATA pla, pla, pla, ple, ple, pli, plo, plo, plu
DATA tla, tla, tla, tle, tle, tli, tlo, tlo, tlu
DATA vla, vla, vla, vle, vle, vli, vlo, vlo, vlu
DATA zla, zle, zli, zlo, zlo                                                : REM hasta aqui van 579 (488 + 91)

