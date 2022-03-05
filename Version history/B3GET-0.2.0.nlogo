extensions [sound]
;;sound:play-note "TRUMPET" random 60 random 64 random 2;
globals [grass cells]  ;; keep track of how much grass there is
breed [males male]
breed [females female]
turtles-own [age energy competitive-ability group-number generation-number home-base adult? genes]
patches-own [penergy fertile?]

to setup
  clear-all
  setup-patches;
  setup-groups;
  display-labels;
  set cells count patches;
  reset-ticks
end

;;;;;;;;;; setup ;;;;;;;;;;;;;;

to setup-patches
  ask patches [
    ifelse random 100 < patch-abundance * 100 [
      set fertile? true;
    ] [
      set fertile? false;
    ]
  ]

  let patchcount 0;
  while [patchcount < (patch-patchiness * 100)] [
    ask patches [
      if random 100 < patch-patchiness * 100 [
        let neighborsList [self] of neighbors;
        foreach neighborsList [ ?1 ->
          ifelse [fertile?] of ?1 [
            set fertile? true;
          ] [
            set fertile? false;
          ]
        ]
      ]
    ]
   set patchcount patchcount + 5;
  ]

  ask patches [
    ifelse fertile? [
      set penergy 1 + random patch-max-energy - 1;
    ] [
      set penergy 0;
    ]
    set-patch-color;
  ]
end

to setup-groups
  let groupCount 0
  while [groupCount < initial-group-count] [

    let xCoord random-xcor;
    let yCoord random-ycor;

    ;; ADD FEMALES TO GROUP
    create-males initial-number-males [
      set size 2.0  ;
      set label-color white;
      set energy random 50
      setxy xCoord yCoord
      set shape "triangle"
      set age random life-expectancy
      set group-number groupCount
      set home-base patch-here
      ifelse age > age-at-maturity [
        set adult? true;
        set color groupCount * 10 + 3 + 0 + 90;
      ] [
        set adult? false;
        set color groupCount * 10 + 3 + 2 + 90;
      ]
      set genes "00000000";
    ]

    ;; ADD MALES TO GROUP
    create-females initial-number-females [

      set size 1.5  ;
      set label-color white;
      set energy random 50
      setxy xCoord yCoord
      set shape "circle"
      set age random life-expectancy
      set group-number groupCount
      set home-base patch-here
      ;;set dying? false
      ;;set birthing? false
      ifelse age > age-at-maturity [
        set adult? true;
        set color groupCount * 10 + 3 + 0 + 90;
      ] [
        set adult? false;
        set color groupCount * 10 + 3 + 2 + 90;
      ]
      set genes "00000000";
    ]
    set groupCount groupCount + 1;
  ]

end

;;;;;;;;;;;; go ;;;;;;;;;;;;;;

to go

  display-labels

  patchify;

  ask patches [
    ;;maintain-patches;
    grow-grass;
    set-patch-color;
  ]

  set grass count patches with [penergy > 0];

  ask turtles [
    patch-calculation;
    ;;compete;
    eat-grass

    death
  ]

  ask females [
    reproduce;
  ]

  calculate-home-bases

  tick
end

;;;;;;;;;;;;;;;;;;;

to calculate-home-bases

end

;;;;;;;;;;;; patch ;;;;;;;;;;;;

to maintain-patches

  ;; maintian abundance level
  ifelse penergy > 0 [
    grow-grass;
  ][
    if grass / cells < patch-abundance [
      grow-grass;
      set grass count patches with [penergy > 0];
    ]
  ]
  ;; make-patchy;

end

to grow-grass
  ifelse fertile? [
    if random 100 < patch-growth-rate * 100 [
      if penergy < patch-max-energy [
        set penergy penergy + 1;
      ]
    ]
  ] [
    set penergy 0;
  ]
end

to make-patchy
  ifelse count neighbors with [penergy > 0] / 8 >= .5 [
     set penergy penergy + (1 * patch-patchiness);
  ] [
     set penergy penergy - (1 * patch-patchiness);
  ]
  if penergy > patch-max-energy [ set penergy patch-max-energy ]
end

to patchify
   ask patches [
      if random 100000 < patch-patchiness * 100 [
        let neighborsList [self] of neighbors;
        foreach neighborsList [ ?1 ->
          ifelse [fertile?] of ?1 [
            set fertile? true;
          ] [
            set fertile? false;
          ]
        ]
      ]
    ]
end

to set-patch-color
  if penergy < 0 [ set penergy 0 ]
  ;;if penergy = 0 [ set pcolor brown ]
  ;if penergy > 0 [ set pcolor green + 1 ]
 ; if penergy > patch-max-energy * 0.25  [ set pcolor green + 1 ]
  ;if penergy > patch-max-energy * 0.50 [ set pcolor green ]
  ;if penergy > patch-max-energy * 0.75 [ set pcolor green ]
  ;if penergy > patch-max-energy [ set penergy patch-max-energy ]

  set pcolor scale-color green penergy (patch-max-energy + 20) -10;

end

;;;;;;;;;; entities ;;;;;;;;;;;;;

to youth-patch-calculation
end

to patch-calculation

  let bestPatch patch-here;
  let bestPatchValue 1;
  let patchList [self] of patches with [distance myself < perception-range];
  let meTurtle self;

  foreach patchList [ ?1 ->

    ;; FOOD
    let foodValue [penergy] of ?1 * (1 - ([energy] of self / birth-cost));

    ;; CONSPECIFIC
    let conspecificList [self] of turtles-on ?1;
    let conspecificValue 0;
    foreach conspecificList [
      set conspecificValue conspecificValue + ( 1 - (distance meTurtle / perception-range))
    ]

    ;; HOME RANGE
    let homeValue (1 - ([distance [home-base] of meTurtle] of ?1 / 100)) * (distance [home-base] of meTurtle / 100)

    let groupValue 0;
    foreach conspecificList [
      ;;if [group-number
      set groupValue groupValue + ( 1 - (distance meTurtle / perception-range))
    ]

    ;; MATES
    let mateValue 0;
    foreach conspecificList [ ??1 ->
      if [breed] of ??1 = females and [breed] of meTurtle = males [
        set mateValue mateValue + ( 1 - (distance meTurtle / perception-range))
      ]
    ]

    ;; PREDATORS
    ;;if is-turtle? male [
    ;;  foreach conspecificList [
    ;;    set mateValue mateValue + ( 1 - ([distance self] of ? / perception-range))
    ;;  ]
    ;;]

    ;; TOTAL
    let patchValue homeValue + foodValue + mateValue - conspecificValue;

    if patchValue > bestPatchValue [
      set bestPatchValue patchValue;
      set bestPatch ?1;
    ]
  ]

  move-to-patch bestPatch;

end

to move-to-patch [to-patch]  ;; turtle procedure
    set energy energy - 5
    ifelse to-patch = nobody [
      rt random 50
      lt random 50
      fd 1
    ] [
     face to-patch
     rt random-float 20
     lt random-float 20
     fd 1
    ]
    update-age
end

to update-age
  set age age + 1;
  if age > age-at-maturity [
    if adult? = false [
      set adult? true;
      set color group-number * 10 + 3 + 0 + 90;
      check-transfer
    ]
  ]
  if age > life-expectancy [
    die
  ]
end

to check-transfer

  if breed = males [
    if male-transfer? [

      let initGrp group-number
      while [group-number = initGrp] [
        set group-number random initial-group-count;
      ]
      set home-base [home-base] of one-of turtles with [group-number = [group-number] of myself]
    ]
  ]

  if breed = females [
    if female-transfer? [

      let initGrp group-number
      while [group-number = initGrp] [
        set group-number random initial-group-count;
      ]
      set home-base [home-base] of one-of turtles with [group-number = [group-number] of myself]
    ]
  ]

end

to eat-grass
  set energy energy + food-eaten-per-step
  set penergy penergy - food-eaten-per-step
end

to reproduce
  if energy > birth-cost [
    set energy energy - birth-cost + 20

    ifelse random 100 < 50 [
      hatch-males 1 [
        set generation-number generation-number + 1
        set color group-number * 10 + 3 + 2 + 90;
        set size 2.0  ;
        set label-color white;
        set energy random 50
        setxy xcor ycor
        set shape "triangle"
        set group-number group-number
        set home-base patch-here
        set age 0
        set adult? false;
        set genes genes
        calculate-genes genes
      ]
    ] [
      hatch-females 1 [
        set generation-number generation-number + 1
        set color group-number * 10 + 3 + 2 + 90;
        set size 1.5  ;
        set label-color white;
        set energy random 50
        setxy xcor ycor
        set shape "circle"
        set group-number group-number
        set home-base patch-here
        set age 0
        set adult? false;
        set genes genes
        calculate-genes genes
      ]
    ]

  ]
end

to calculate-genes [parent-genes]

  let i 0;
  let changeCount 0;

  while [i < 8] [
    if random 100 < 50 and changeCount < 5 [
      set genes replace-item i genes one-of ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"]
    ]

    ifelse changeCount < 5 and i = 7 [
      set i 0
    ] [
      set i i + 1
    ]
  ]

end

to death  ;; turtle procedure
  ;; when energy dips below zero, die
  if energy < 0 [ die ]
end

to display-labels
  ask turtles [ set label "" ]
  if show-energy? [
    ask females [ set label round energy ]
    ask males [ set label round energy ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
400
32
862
495
-1
-1
4.5
1
14
1
1
1
0
1
1
1
-50
50
-50
50
1
1
1
ticks
30.0

SLIDER
7
140
182
173
initial-number-males
initial-number-males
0
250
1.0
1
1
NIL
HORIZONTAL

SLIDER
7
182
182
215
initial-number-females
initial-number-females
0
250
3.0
1
1
NIL
HORIZONTAL

BUTTON
8
28
77
61
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

BUTTON
90
28
157
61
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
886
322
1202
519
populations
time
 b
0.0
100.0
0.0
100.0
true
true
"" ""
PENS
"males" 1.0 0 -13791810 true "" "plot count males"
"females" 1.0 0 -5825686 true "" "plot count females"

MONITOR
888
260
959
305
# males
count males
3
1
11

MONITOR
963
260
1045
305
# females
count females
3
1
11

TEXTBOX
10
321
150
340
Entity settings
11
0.0
0

TEXTBOX
11
223
163
241
Patch Settings
11
0.0
0

SWITCH
167
28
303
61
show-energy?
show-energy?
1
1
-1000

SLIDER
193
240
365
273
patch-growth-rate
patch-growth-rate
0
1
0.99
.01
1
NIL
HORIZONTAL

SLIDER
13
241
185
274
patch-abundance
patch-abundance
0
1
0.59
.01
1
NIL
HORIZONTAL

SLIDER
13
280
185
313
patch-patchiness
patch-patchiness
0
1
0.22
0.01
1
NIL
HORIZONTAL

SLIDER
193
280
365
313
patch-max-energy
patch-max-energy
0
100
24.0
1
1
NIL
HORIZONTAL

INPUTBOX
14
343
169
403
perception-range
2.0
1
0
Number

SLIDER
8
98
182
131
initial-group-count
initial-group-count
0
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
180
384
352
417
birth-cost
birth-cost
0
1000
260.0
10
1
NIL
HORIZONTAL

SWITCH
13
411
172
444
female-transfer?
female-transfer?
1
1
-1000

SWITCH
13
450
172
483
male-transfer?
male-transfer?
1
1
-1000

SLIDER
180
343
352
376
max-energy
max-energy
0
1000
1000.0
10
1
NIL
HORIZONTAL

TEXTBOX
11
78
161
96
Initialization Settings
11
0.0
1

SLIDER
179
424
351
457
food-eaten-per-step
food-eaten-per-step
0
50
9.0
1
1
NIL
HORIZONTAL

INPUTBOX
196
81
351
141
age-at-maturity
75.0
1
0
Number

INPUTBOX
201
159
356
219
life-expectancy
400.0
1
0
Number

@#$#@#$#@
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
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
setup
set grass? true
repeat 75 [ go ]
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
