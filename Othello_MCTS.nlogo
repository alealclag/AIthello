__includes ["lib/MCTS.nls" "lib/validaDirecciones.nls"]
extensions [matrix]

breed[negras negra]
breed[blancas blanca]
breed[vaciasBlanca vaciaBlanca]
breed[vaciasNegra vaciaNegra]

globals [turno played noMov1 noMov2 tablero]

negras-own [
  id ; will mantain the type of the piece (it identifies the piece)
]
blancas-own [
  id; will mantain the type of the piece (it identifies the piece)
]
patches-own [
  value ; will mantain the id of the piece in it
]

;Aclaración: debido a que en los patches las coordenadas de "y" hacia abajo son negativas y en la matriz son positivas y que las coordenadas en la matriz están al revés,
;en varios partes del código le cambiamos el signo y/u orden a las coordenadas.

;----------------------------------------------------
;MCTS
;Estas son las funciones principales para el uso de MCTS
;----------------------------------------------------



to-report MCTS:get-content [s]
  report first s
end

; Get the player that generates the state
to-report MCTS:get-playerJustMoved [s]
  ifelse (last s) = 1[report 2][
    ifelse (last s) = 2 [report 1][report 0]]
end

; Create a state from the content and player
to-report MCTS:create-state [c p]
  report (list c p)
end

; Get the rules applicable to the state
to-report MCTS:get-rules [s]
  let c MCTS:get-content s
  let transitions []
  let lista (range 8)
  foreach lista [ i ->
    foreach lista [ j ->
      let vecinos get-vecinos c i j
      foreach vecinos [ v ->
      if MCTS:valid-transition? v s [set transitions lput v transitions];vamos casilla a casilla comprobando si es un movimiento válido
      ]
    ]
  ]
  set transitions remove-duplicates transitions
  report transitions
end

; Apply the rule r to the state s
to-report MCTS:apply [r s]
  let board MCTS:get-content s
  let p MCTS:get-playerJustMoved s
  let player 0
  ifelse p = 1[set player 2][set player 1]
  let i (item 0 r)
  let j (item 1 r)
  let cboard matrix:copy board
  let new-player p

  matrix:set cboard j i player
  let posConv []
  ;check i=i+0 y j=j-1
  set posConv MCTS:checkCasillasVecinas cboard j i -1 0 posConv player
 ;print posConv
  ;check i=i+1 y j=j-1
  set posConv MCTS:checkCasillasVecinas cboard j i -1 1 posConv player
; print posConv
  ;check i=i+1 y j=j+0
  set posConv MCTS:checkCasillasVecinas cboard j i  0 1 posConv player
; print posConv
  ;check i=i+1 y j=j+1
  set posConv MCTS:checkCasillasVecinas cboard j i  1 1 posConv player
; print posConv
  ;check i=i + 0 y j=j+1
  set posConv MCTS:checkCasillasVecinas cboard j i  1 0 posConv player
; print posConv
  ;check i= i-1 + 0 y j=j+1
  set posConv MCTS:checkCasillasVecinas cboard j i  1 -1 posConv player
; print posConv
  ;check i= i-1 + 0 y j=j+0
  set posConv MCTS:checkCasillasVecinas cboard j i  0 -1 posConv player
; print posConv
  ;check i= i-1 + 0 y j=j-1
  set posConv MCTS:checkCasillasVecinas cboard j i -1 -1 posConv player

  ;realizamos los cambios en la matriz
  foreach posConv[ x -> matrix:set cboard (first x) (last x) player]

  report MCTS:create-state cboard new-player
end

; Move the result from the last state to the current one
to-report MCTS:get-result [s p]
  let c MCTS:get-content s
  let numB 0
  let numN 0
  let board matrix:to-row-list c
  foreach board[ fila ->
    foreach fila[ casilla ->
      if casilla = 1 [set numN numN + 1]
      if casilla = 2 [set numB numB + 1]
    ]
  ]
  if numB > numN[
    report 1
  ]
  if numB < numN[
   report 0
  ]
  if numB = numN[report 0.5]
end



;-----------------------------------------
;Funciones auxiliares para MCTS
;-----------------------------------------


;Obtenemos una lista con los vecinos en la matriz de la coordenada x y
to-report get-vecinos [s x y]
  let vecinos []
  let vecino []
  if (get_color s (x - 1) (y - 1)) = 0[
    set vecino lput (x - 1) vecino
    set vecino lput (y - 1) vecino
    set vecinos lput vecino vecinos
    set vecino []
  ]
  if (get_color s x (y - 1)) = 0[
    set vecino lput x vecino
    set vecino lput (y - 1) vecino
    set vecino lput vecino vecinos
    set vecino []
  ]
  if (get_color s (x + 1) (y - 1)) = 0[
    set vecino lput (x + 1) vecino
    set vecino lput (y - 1) vecino
    set vecino lput vecino vecinos
    set vecino []
  ]
  if (get_color s (x - 1) y) = 0[
    set vecino lput (x - 1) vecino
    set vecino lput y vecino
    set vecinos lput vecino vecinos
    set vecino []
  ]
  if (get_color s (x + 1) y) = 0[
    set vecino lput (x + 1) vecino
    set vecino lput y vecino
    set vecinos lput vecino vecinos
    set vecino []
  ]
  if (get_color s (x - 1) (y + 1)) = 0[
    set vecino lput (x - 1) vecino
    set vecino lput (y + 1) vecino
    set vecinos lput vecino vecinos
    set vecino []
  ]
  if (get_color s x (y + 1)) = 0[
    set vecino lput x vecino
    set vecino lput (y + 1) vecino
    set vecinos lput vecino vecinos
    set vecino []
  ]
  if (get_color s (x + 1) (y + 1)) = 0[
    set vecino lput (x + 1) vecino
    set vecino lput (y + 1) vecino
    set vecinos lput vecino vecinos
    set vecino []
  ]
  report vecinos
end

;get_color devuelve el color de la casilla cuyas coordenadas son x e y en el tablero s
to-report get_color [s x y]
  let colour 3
  ifelse (x < 0) or (y < 0) or (x >= 8) or (y >= 8)[set colour 3][
    set colour (matrix:get s y x)
  ]
  report colour
end

;reporta verdadero si la transición t es válida en el estado s
to-report MCTS:valid-transition? [t s]
  let valid false
  let board MCTS:get-content s
  let x (item 0 t)
  let y (item 1 t)
  let fila (matrix:get-row board y)
  let casilla (item x fila)
  ifelse casilla != 0 [report false][;en caso de existir ya una ficha en la posición dada devolveos false

    let colour (item 1 s);obtenemos el color que toca colocar
    ;realizamos la comprobación para las 8 direcciones posibles
    set valid valid_color board colour x y valid (x - 1) (y - 1) -1 -1
    set valid valid_color board colour x y valid x (y - 1) 0 -1
    set valid valid_color board colour x y valid (x + 1) (y - 1) 1 -1
    set valid valid_color board colour x y valid (x - 1) (y ) -1 0
    set valid valid_color board colour x y valid (x + 1) (y ) 1 0
    set valid valid_color board colour x y valid (x - 1) (y + 1) -1 1
    set valid valid_color board colour x y valid x (y + 1) 0 1
    set valid valid_color board colour x y valid (x + 1) (y + 1) 1 1

    report valid]
end

;valid_color es una función recursiva que realiza la comprobación de que se cumpla la secuencia negro,blanco,...,blanco,negro y viceversa
to-report valid_color [s colour x y valido x_vecino y_vecino dirx diry]
  ifelse valido = true [report true][;si en alguno de los casos anteriores se ha devuelto true no es necesario realizar ninguna operación más
    ifelse (x_vecino < 0) or (y_vecino < 0) or (x_vecino >= 8) or (y_vecino >= 8)[report false][;comprobamos que esté entre los límites del tablero
      let color_vecino get_color s x_vecino y_vecino ;obtenemos el color de la ficha en la dirección dada
      ifelse (colour != color_vecino) and (color_vecino != 0) [;si es del color contrario hacemos una llamada recursiva, siendo esta vez el "vecino" la siguiente ficha en la dirección dada
        report valid_color s colour x y valido (x_vecino + dirx) (y_vecino + diry) dirx diry][
        ifelse (colour = color_vecino) and (no_vecinos x_vecino y_vecino x y)[;si es del mismo color pueden darse dos casos: que entre esta y la que vayamos a colocar no haya ninguna otra ficha del otro color (son vecinos) o que sí las haya (no son vecinos)
          report true][
          report false]
  ]]]
end

;MCTS:checkCasillasVecinas recibe la posición de la ficha recién colocada y unos valores, si y sj, que se utilizarán para acceder a la casilla contigua que se encuentra en el
;en el sentido requerido, para ello sumará estos valores a las coordenadas de la posición dada. Aparte recibe una lista donde añadirá o no las fichas contrarias a convertir,
;y el color del jugador. Devuelve dicha lista si la casilla contigua en la dirección deseada que comprobamos está fuera del tablero, devolverá posConv sin modificar.
;De la misma forma lo hará si dicha casilla contiene una ficha del mismo color que el del jugador.
;Solo si no se cumplen estas condiciones realizará la llamada a MCTS:funRec, es decir, siempre lo hace si encuentra el inicio de una posible lista de fichas del rival a convertir
to-report MCTS:checkCasillasVecinas [board j i sj si  posConv player]
  let res []
  let posi i + si
  let posj j + sj
  ifelse (posi < 0) or (posj < 0) or (posi >= 8) or (posj >= 8)[report posConv][
    let ficha matrix:get board posj posi
    ifelse (ficha = 0) or (ficha = player)[report posConv][
     let acum []
     let pos MCTS:funRec board posj posi  sj si acum player
     ifelse(empty? pos)[report posConv][
         set res (combinaListas pos posConv)
         report res
     ]
    ]
  ]
end

;MCTS:funRec es una función recursiva que recibe la posición inicial de la posible fila de fichas contrarias en el sentido deseado a convertir. Y se hará sucesivas llamadas
;siempre que encuentre una ficha del jugador rival, añadiendo en cada llamada la posición de ésta a "listapos", y solo se detendrá si encuentra una ficha del color del jugador
;actual, una casilla vacía o llegue al límite del tablero. En el primero caso, devolverá "listapos", el en segundo caso una lista vacía, y en el tercero, dependerá si al
;final del tablero hay una ficha del jugador actual, devolviendo "listapos", o no, devolviendo una lista vacía.
to-report MCTS:funRec [board j i sj si listapos player]
  let posi i + si
  let posj j + sj
  ifelse ((matrix:get board j i) = player)[report listapos][
    ifelse (((matrix:get board j i) = 0) or (posi < 0) or (posj < 0) or (posi >= 8) or (posj >= 8))[
      report []
    ][
      let pos (list j i)
      set listapos fput pos listapos
      report MCTS:funRec board posj posi sj si listapos player
  ]]
end

to-report combinaListas [l1 l2]
  let i 0
  while [i < length l1] [
  set l2 lput (item i l1) l2
  set i i + 1
  ]
  report l2
end



;----------------------------------------------------
;Graphic Interface
;Estas funciones son usadas para la interfaz gráfica del juego
;----------------------------------------------------


;convierte_fichas recibe como parámetros la posición de la ficha colocada y el color del jugador actual. Como, según las reglas del juego, solo podemos colocar una ficha de forma que
;convierta alguna/s del rival, en alguna/s dirección de la posicón de la ficha colocada habrá una fila de fichas del rival a convertir que termina con una ficha del color
;de la recíen colocada. Para obtener una lista válida de fichas del rival a convertir creamos una lista vacía PosConv que va a acumular éstas.
;Por eso, tenemos una llamada por cada dirección al método checkCasillas
;Después de obtner las posiciones de las fichas del rival a convertir realizamos las transformaciones de estas en el tablero y en la matrix.
to convierte_fichas [x y player]
  let posConv []
  ;check i=i+0 y j=j-1
  set posConv checkCasillasVecinas  y x -1 0 posConv player

  ;check i=i+1 y j=j-1
  set posConv checkCasillasVecinas  y x -1 1 posConv player

  ;check i=i+1 y j=j+0
  set posConv checkCasillasVecinas  y x  0 1 posConv player

  ;check i=i+1 y j=j+1
  set posConv checkCasillasVecinas  y x  1 1 posConv player

  ;check i=i + 0 y j=j+1
  set posConv checkCasillasVecinas  y x  1 0 posConv player

  ;check i= i-1 + 0 y j=j+1
  set posConv checkCasillasVecinas  y x  1 -1 posConv player

  ;check i= i-1 + 0 y j=j+0
  set posConv checkCasillasVecinas  y x  0 -1 posConv player

  ;check i= i-1 + 0 y j=j-1
  set posConv checkCasillasVecinas  y x -1 -1 posConv player

  let i 0
  ;Primero nos deshaemos de las fichas
  while [i < length posConv] [
  let pos item i posConv
  let p one-of turtles-on patch first pos last pos
    ask p [
      die
      ]
   ask patch first pos last pos [
     set value 0
    ]

  set i i + 1
  ]

  set i 0
  ;Luego colocamos las fichas nuevas
  while [i < length posConv] [
  let pos item i posConv
  if(player = 1) [
    create-negras 1[set  xcor first pos set ycor last pos set id player]
    matrix:set tablero ((last pos) * (-1)) first pos 1
    ask patch first pos last pos [
     set value player
    ]
  ]

  if(player = 2)[
    create-blancas 1[set  xcor first pos set ycor last pos set id player]
    matrix:set tablero ((last pos) * (-1)) first pos 2
    ask patch first pos last pos [
     set value player
    ]
  ]
    set i i + 1
  ]
end

;checkCasillasVecinas recibe la posición de la ficha recién colocada y unos valores, si y sj, que se utilizarán para acceder a la casilla contigua que se encuentra en el
;en el sentido requerido, para ello sumará estos valores a las coordenadas de la posición dada, aunque para las filas del tablero(j) restará ya que nuestro mundo va
;de 0 a -7 en el eje y. Aparte recibe una lista donde añadirá o no las fichas contrarias a convertir, y el color del jugador. Devuelve dicha lista
;Si la casilla contigua en la dirección deseada que comprobamos está fuera del tablero, devolverá posConv sin modificar. De la misma forma lo hará si dicha casilla contiene
;una ficha del mismo color que el del jugador.
;Solo si no se cumplen estas condiciones realizará la llamada a funRec, es decir, siempre lo hace si encuentra el inicio de una posible lista de fichas del rival a convertir
to-report checkCasillasVecinas [j i sj si posConv player]
  let res []
  if-else(j - sj <= 0 and j - sj >= -7 and i + si >= 0 and i + si <= 7)[
  let ficha [value] of patch (i + si) (j - sj)
  ifelse(ficha = 0 or ficha = player)[

      report posConv]
      [
        let acum[]
        let pos funRec  (j - sj) (i + si)  sj si acum player
        ifelse(empty? pos)[
        report posConv

      ][
         set res (combinaListas pos posConv)
         report res
     ]

      ]
  ][
    report posConv
  ]
end

;funRec es una función recursiva que recibe la posición inicial de la posible fila de fichas contrarias en el sentido deseado a convertir. Y se hará sucesivas llamadas
;siempre que encuentre una ficha del jugador rival, añadiendo en cada llamada la posición de ésta a "listapos", y solo se detendrá si encuentra una ficha del color del jugador
;actual, una casilla vacía o llegue al límite del tablero. En el primero caso, devolverá "listapos", el en segundo caso una lista vacía, y en el tercero, dependerá si al
;final del tablero hay una ficha del jugador actual, devolviendo "listapos", o no, devolviendo una lista vacía.
to-report funRec [ j i sj si listapos player]
  let ficha [value] of patch i j
  (ifelse(ficha = player)[
    report listapos
    ] (ficha = 0)[
    report []
     ]
    [
      ifelse((j - sj) <= 0 and (j - sj) >= -7 and (i + si) >= 0 and (i + si) <= 7)[
        let cpj j
        let cpi i
        let pos (list cpi cpj)
        report funRec  (j - sj) (i + si) sj si (fput pos listapos) player
      ]
      [report []]
    ]
   )

end

;comprueba que en el patch x y se pueda colocar la ficha
to-report valid_mov?[x y]
  ifelse (not any? negras-on patch x y) and (not any? blancas-on patch x y) and ((any? vaciasNegra-on patch x y) or (any? vaciasNegra-on patch x y))
  [report true][report false]
end

;comprueba que dos patches no sean vecinos
to-report no_vecinos [x_vecino y_vecino x y]
  ifelse (((abs(x_vecino - x)) = 1) or (x_vecino = x)) and (((abs(y_vecino - y)) = 1) or (y_vecino = y)) [report false][report true]
end

;coloca la ficha de del color indicado en la coordenada x y
to colocar-ficha [x y player]
  ;ficha negra
  if(player = 1) [
    ask vaciasNegra[die];eliminamos las marcas de posibles movimientos
    create-negras 1[set  xcor x set ycor y set id player];creamos la ficha
    matrix:set tablero (y * (-1)) x 1 ;añadimos la ficha a la matriz
    ask negras[set value id]
    convierte_fichas x y player ;realizamos los cambios producidos por el movimiento
    set turno 2
    set noMov1 false
    wait .3
  ]
  ;ficha blanca
  if(player = 2)[
    ask vaciasBlanca[die] ;eliminamos las marcas de posibles movimientos
    create-blancas 1[set xcor x set ycor y set id player]
    matrix:set tablero (y * (-1)) x 2 ;añadimos la ficha a la matriz
    ask blancas[set value id]
    convierte_fichas x y player ;realizamos los cambios producidos por el movimiento
    set turno 1
    set noMov2 false
    wait .3
  ]
end

;realizamos las comprobaciones para obtener los distintos movimientos posibles. Para dejar el código más "limpio", hemos guardado estas funciones en una librería aparte.
to validar-posiciones-turno-negras

  vertical-arriba-negras
  diagonal-arriba-derecha-negras
  horizontal-derecha-negras
  diagonal-abajo-derecha-negras
  vertical-abajo-negras
  diagonal-abajo-izquierda-negras
  horizontal-izquierda-negras
  diagonal-izquierda-arriba-negras

end

to validar-posiciones-turno-blancas

  vertical-arriba-blancas
  diagonal-arriba-derecha-blancas
  horizontal-derecha-blancas
  diagonal-abajo-derecha-blancas
  vertical-abajo-blancas
  diagonal-abajo-izquierda-blancas
  horizontal-izquierda-blancas
  diagonal-izquierda-arriba-blancas

end



;----------------------------------------------------
;main
;----------------------------------------------------



to setup
  clear-all
  set-patch-size 50
  resize-world  0 7 -7 0
  set-default-shape negras "fichaNegra"
  set-default-shape blancas "fichaBlanca"
  set-default-shape vaciasNegra "fichavacianegra"
  set-default-shape vaciasBlanca "fichavaciablanca"
  set played 1;Se usa para indicar si se ha jugado un turno o no, porque si no ejecutaríamos un turno varias veces hasta que se juegue
  set turno 1;Indica el turno
  set noMov1 false;Estas dos variables sirven para indicar cuando no hay movimientos posibles para un jugador
  set noMov2 false
  set iterations 50
  set tablero matrix:from-row-list [[0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 1 2 0 0 0] [0 0 0 2 1 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0] [0 0 0 0 0 0 0 0]]
  ask patches[
    set pcolor ifelse-value (pxcor + pycor) mod 2 = 0 [53][67]
  ]

  create-negras 1[set xcor 3 set ycor -3 set id 1]
  create-negras 1[set xcor 4 set ycor -4 set id 1]
  create-blancas 1[set xcor 3 set ycor -4 set id 2]
  create-blancas 1[set xcor 4 set ycor -3 set id 2]

  ask negras[set value id]
  ask blancas[set value id]

end

to play
  if noMov1 and noMov2 [;si no se ha podido mover ninguna ficha termina el juego
      if count blancas > count negras[user-message ("¡La victoria es para... jugador blanco!")]
      if count blancas = count negras[user-message ("¡Wow! ¡Empate!")]
      if count blancas < count negras[user-message ("¡La victoria es para... jugador negro!")]
      stop]
  ifelse turno = 1[
    if played = 1 [validar-posiciones-turno-negras];comprobamos las posiciones válidas y las creamos
    set played 2;cambiamos el valor de la variable para que no repitamos el turno varias veces
    ifelse count vaciasNegra != 0[;comprobamos si hay movimientos posibles
      if mouse-down? and valid_mov? round mouse-xcor round mouse-ycor[;comprobamos que se haya clickado en una casilla valida
        colocar-ficha round mouse-xcor round mouse-ycor 1 ;colocamos la ficha
        set noMov1 false;ponemos noMov1 a false porque hemos hecho un movimiento
      ]
    ][print "No hay movimientos posibles para el jugador negro"
    set turno 2
    set noMov1 true]
    ][
    ;turno de la IA
    if turno = 2 [
      if played = 2 [validar-posiciones-turno-blancas]
      set played 1
      ifelse count vaciasBlanca != 0[
        let m MCTS:UCT (list tablero 2) iterations;obtenemos el movimiento que nos indique el algoritmo de MCTS
        colocar-ficha (item 0 m) ((item 1 m) * (-1)) 2
        set noMov2 false
      ][print "No hay movimientos posibles para el jugador blanco"
      set turno 1
      set noMov2 true]
  ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
7
10
415
419
-1
-1
50.0
1
10
1
1
1
0
0
0
1
0
7
-7
0
0
0
1
ticks
30.0

BUTTON
106
435
171
468
NIL
play
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
16
434
79
467
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
16
495
94
540
MCTSnodes
count MCTSnodes
17
1
11

SLIDER
223
511
395
544
iterations
iterations
0
100
6.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fichablanca
true
0
Circle -1 true false 30 30 240

fichanegra
true
0
Circle -16777216 true false 22 22 256

fichavaciablanca
true
0
Circle -1 false false 29 29 242
Rectangle -7500403 false true 135 180 135 180

fichavacianegra
true
0
Circle -16777216 false false 29 29 242

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -2064490 true false 270 75 225 30 30 225 75 270
Polygon -2064490 true false 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
