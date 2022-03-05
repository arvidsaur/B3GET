globals [
  sex_list
  shape_list
  color_list
  check_list
  masterGenes
  chromosomeLength
  input_list
  output_list
  intial_threshold_hunger ]

breed [organisms organism]

patches-own [ penergy ]
organisms-own [ sex energy threshold_hunger generation morph_chromosome act_chromosome sex_chromosome starting_act depth sired aggression dying? ]

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::: SETUP ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to setup
  clear-all
  initialize-lists
  setup-patches
  add-organisms
  reset-ticks
end

to initialize-lists
  set chromosomeLength 30
  set intial_threshold_hunger random birth-cost
  set sex_list ["male" "female"]
  set shape_list ["circle" "triangle" "square"]
  set color_list ["red" "orange" "green" "blue" "magenta"]
  set check_list [ "shape?" "color?" "sex?" "energy?" "guy?" "patch?" "hostile?" ]
  set input_list [ "male" "female" "circle" "triangle" "square" "red" "orange" "green" "blue" "magenta" "hungry" "satiated" "alone" "other" "food" "empty" "nice" "neutral" "mean"]
  set output_list [ "forward" "mate" "eat" "turn" "attack" "shape?" "color?" "sex?" "energy?" "guy?" "patch?" "hostile?" ]
  set masterGenes [ ]
end

to setup-patches
  ask patches [
    ifelse (random-float 1.0 < patch-growth-rate) [set penergy patch-max-energy] [set penergy 0]
    set-patch-color
  ]
end

to add-organisms
  create-organisms initial-organisms [
    set starting_act one-of check_list
    set threshold_hunger random birth-cost
    set morph_chromosome []
    set morph_chromosome lput one-of sex_list morph_chromosome
    set morph_chromosome lput one-of shape_list morph_chromosome
    set morph_chromosome lput one-of color_list morph_chromosome
    set act_chromosome generateGenes
    set sex_chromosome generateGenes
    setupOrganism
  ]
end

to setupOrganism
  set xcor (floor random-xcor )
  set ycor (floor random-ycor )
  set size 1
  set dying? false
  set aggression random-float 1.0
  set sex item 0 morph_chromosome
  set shape item 1 morph_chromosome
  set color read-from-string item 2 morph_chromosome
  set energy 50
  set label first sex
  set generation generation + 1
  set sired 0
  set heading one-of [ 0 90 180 270 ]
end

to-report generateGenes
  let report_list []
  let switch? true
  repeat 20 [
    ifelse switch? [
      set report_list lput one-of input_list report_list
      set switch? false
    ][
      set report_list lput one-of output_list report_list
      set switch? true
    ]
  ]
  report report_list
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::: GO :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to go
  ask patches [ grow ]
  ask organisms [
    act
    check-death]
  tick
  ask organisms with [dying?] [ die ]
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::: PATCH FUNCTIONS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to grow
  if (random-float 1.0 < patch-growth-rate) [
    if (penergy = 0) [set penergy patch-max-energy]]
  set-patch-color
end

to set-patch-color
  set pcolor ifelse-value (penergy = 0) [brown] [green + 1]
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::: organism FUNCTIONS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to act
  ;print (word self starting_act)
  set depth 0
  do starting_act
  turn
  update-energy (- 1)
  ;print "";
end

to input-output [ input ]
  set depth depth + 1
  let chromosome-list sentence sex_chromosome act_chromosome

  ifelse member? input chromosome-list and depth < 10 [
    if (position input chromosome-list + 1 < length chromosome-list) [
      ;print (word input " " (item (position input chromosome-list + 1) chromosome-list))
      let output item (position input chromosome-list + 1) chromosome-list
      do output
      ;input-output output
    ]] [ turn ]
end

to do [code]
  if depth > 10 [ stop ]
  if code = "shape?" [ check-shape ]
  if code = "color?" [ check-color ]
  if code = "sex?" [ check-sex ]
  if code = "energy?" [ check-energy ]
  if code = "guy?" [ check-guy ]
  if code = "patch?" [ check-patch ]
  if code = "forward" [ move ]
  if code = "turn" [ turn ]
  if code = "mate" [ mate ]
  if code = "eat" [ eat ]
  if code = "attack" [ attack ]
  if code = "hostile?" [ check-hostility ]
end

to check-shape
  if (count other (organisms-on (patch-ahead 1)) > 0) [
    let other-guy one-of other (organisms-on (patch-ahead 1))
    input-output ([shape] of other-guy)
  ]
end

to check-color
  if (count other (organisms-on (patch-ahead 1)) > 0) [
    let other-guy one-of other (organisms-on (patch-ahead 1))
    input-output (item 2 ([morph_chromosome] of other-guy))
  ]
end

to check-sex
  if (count other (organisms-on (patch-ahead 1)) > 0) [
    let other-guy one-of other (organisms-on (patch-ahead 1))
    input-output ([sex] of other-guy)
  ]
end

to check-energy
  let new-input ifelse-value ([energy] of self < threshold_hunger) [ "hungry" ] [ "satiated" ]
  input-output new-input
end

to check-guy
  let new-input ifelse-value (one-of organisms-on patch-ahead 1 = nobody) [ "alone" ] [ "other" ]
  input-output new-input
end

to check-patch
  let new-input ifelse-value ([penergy] of patch-ahead 1 > 0) [ "food" ] [ "empty" ]
  input-output new-input
end

to check-hostility
  if (count other (organisms-on (patch-ahead 1)) > 0) [
    let other-guy one-of other (organisms-on (patch-ahead 1))
    let agg "nice"
    if ([aggression] of other-guy > 0.33) [ set agg "neutral" ]
    if ([aggression] of other-guy > 0.66) [ set agg "mean" ]
    input-output agg
  ]
end

to move
  ifelse not any? other turtles-on patch-ahead 1 [ fd 1 ] [ turn ]
  update-energy (- move-cost)
end

to turn
  set depth depth + 1
  ifelse random 2 = 0 [ rt 90 ] [ lt 90 ]
  update-energy (- move-cost)
  move
end

to eat
  if ([penergy] of patch-ahead 1 > 0) [
    update-energy [penergy] of patch-ahead 1
    ask patch-ahead 1 [ set penergy 0 ] ]
end

to attack
  let guy one-of organisms-on patch-ahead 1
  if guy != nobody [
    if (guy != nobody) [ ask guy [ update-energy (- attack-cost * ( 1 + aggression )) ]]]
end

to mate
  let me self
  if ((count other (organisms-on (patch-ahead 1)) with [sex != [sex] of me]) > 0) [
    let mate-here one-of other (organisms-on (patch-ahead 1)) with [sex != [sex] of me]
    ifelse (sex = "female") [ reproduce mate-here ] [ ask mate-here [reproduce me ]]
  ]
end

to reproduce [partner]
  let me self
  if energy > 2 * birth-cost [
    hatch-organisms 1 [
      set energy energy + birth-cost
      set threshold_hunger (intial_threshold_hunger + [intial_threshold_hunger] of partner) / 2
      set morph_chromosome combineLists [morph_chromosome] of self [morph_chromosome] of partner
      setupOrganism
      set act_chromosome combineLists [act_chromosome] of self [act_chromosome] of partner
      ifelse ([sex] of me = sex) [
        set sex_chromosome [sex_chromosome] of self
        set aggression [aggression] of self
        ] [
        set sex_chromosome [sex_chromosome] of partner
        set aggression [aggression] of partner]
      mutate-genes
      set generation [generation] of me + 1
    ]
    set sired sired + 1
    ask partner [ set sired sired + 1 ]
    update-energy (- birth-cost)
  ]
end

to check-death
  if energy <= 0 [ set dying? true ]
end

to update-energy [ value ]
  set energy energy + value
  check-death
end

to mutate-genes
  if random-float 1.0 < mutation-rate [
    let value random 11
    if value = 1 [ set starting_act one-of check_list ]
    if value = 2 [ set threshold_hunger random birth-cost ]
    if value = 3 [ set morph_chromosome replace-item 0 morph_chromosome one-of sex_list ]
    if value = 4 [ set morph_chromosome replace-item 1 morph_chromosome one-of shape_list ]
    if value = 5 [ set morph_chromosome replace-item 2 morph_chromosome one-of color_list ]
    if value = 6 [ if length act_chromosome > 0 [set act_chromosome replace-item (random length act_chromosome) act_chromosome (one-of input_list) ]]
    if value = 7 [ if length act_chromosome > 0 [set act_chromosome replace-item (random length act_chromosome) act_chromosome (one-of output_list) ]]
    if value = 8 [ if length sex_chromosome > 0 [set sex_chromosome replace-item (random length sex_chromosome) sex_chromosome (one-of input_list) ]]
    if value = 9 [ if length sex_chromosome > 0 [set sex_chromosome replace-item (random length sex_chromosome) sex_chromosome (one-of output_list) ]]
    if value = 10 [ set aggression random-float 1.0 ]
    ;if value = 10 [ if length act_chromosome < 30 [ set sex_chromosome lput one-of input_list sex_chromosome ]]
    ;if value = 11 [ if length act_chromosome < 30 [ set sex_chromosome lput one-of output_list sex_chromosome ]]
    ;if value = 12 [ if length act_chromosome < 30 [ set act_chromosome lput one-of input_list act_chromosome ]]
    ;if value = 13 [ if length act_chromosome < 30 [ set act_chromosome lput one-of output_list act_chromosome ]]
    ;if value = 14 [ if length sex_chromosome > 0 [set sex_chromosome remove-item (random length sex_chromosome) sex_chromosome ]]
    ;if value = 15 [ if length sex_chromosome > 0 [set act_chromosome remove-item (random length act_chromosome) act_chromosome ]]
  ]
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::: ALGORITHMS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to-report combineLists [ l1 l2 ]
  let i 0
  let combinedList []
  let fromList []
  let maxIndex 0
  ifelse (length l1 > length l2) [
    set combinedList l1
    set fromList l2
    set maxIndex length l2
  ][
    set combinedList l2
    set fromList l1
    set maxIndex length l1
  ]
  while [i < maxIndex] [
    if random 2 = 0 [
      set combinedList replace-item i combinedList item i fromList
    ]
    set i i + 1
  ]
  report combinedList
end

to-report getListfromString [ string ]
  let lst []
  let i 0
  let wrd ""
  while [i < length string ][
    ifelse (length wrd > 0 and item i string = " ") [
      set lst lput wrd lst;
      set wrd "";
    ][
      set wrd word wrd item i string;
    ]
    set i i + 1
  ]
  set lst lput wrd lst;
  report lst;
end

to-report get-genes
  report sentence sex_chromosome act_chromosome
end

to-report strategy-present? [ in out ]
  let lst get-genes
  let rep false
  if member? in lst [
    if (position in lst + 1 < length lst) [
      let output item (position in lst + 1) lst
      if (out = output) [ set rep true ]
    ]
  ]
  report rep
end
@#$#@#$#@
GRAPHICS-WINDOW
220
10
724
515
-1
-1
9.73
1
14
1
1
1
0
1
1
1
0
50
0
50
1
1
1
ticks
30.0

BUTTON
39
15
108
48
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
113
15
180
48
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

TEXTBOX
71
63
174
81
Patch Settings
12
0.0
0

SLIDER
12
83
212
116
patch-growth-rate
patch-growth-rate
0
.1
0.025
.001
1
NIL
HORIZONTAL

SLIDER
12
123
212
156
patch-max-energy
patch-max-energy
0
9
6.0
1
1
NIL
HORIZONTAL

SLIDER
11
188
212
221
birth-cost
birth-cost
0
100
40.0
5
1
NIL
HORIZONTAL

SLIDER
11
228
212
261
move-cost
move-cost
0
9
1.0
.1
1
NIL
HORIZONTAL

SLIDER
11
267
211
300
attack-cost
attack-cost
0
9
9.0
1
1
NIL
HORIZONTAL

TEXTBOX
68
167
175
185
World Settings
12
0.0
1

MONITOR
46
470
164
515
NIL
count organisms
17
1
11

PLOT
1294
70
1575
284
Allele Frequencies
Time
Frequencies
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"red" 1.0 0 -2674135 true "" "plot (count organisms with [color = read-from-string \"red\"]) / (count organisms + 1)"
"orange" 1.0 0 -955883 true "" "plot (count organisms with [color = read-from-string \"orange\"]) / (count organisms + 1)"
"green" 1.0 0 -10899396 true "" "plot (count organisms with [color = read-from-string \"green\"]) / (count organisms + 1)"
"blue" 1.0 0 -13345367 true "" "plot (count organisms with [color = read-from-string \"blue\"]) / (count organisms + 1)"
"magenta" 1.0 0 -5825686 true "" "plot (count organisms with [color = read-from-string \"magenta\"]) / (count organisms + 1)"
"sex F:M" 1.0 0 -7500403 true "" ";plot (count organisms with [sex = \"female\"]) / (count organisms with [sex = \"male\"])"

INPUTBOX
11
392
114
452
initial-organisms
100.0
1
0
Number

BUTTON
120
392
211
452
NIL
add-organisms
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
46
525
158
570
mode generation
modes [generation] of organisms
17
1
11

SLIDER
11
331
211
364
mutation-rate
mutation-rate
0
1.0
0.2
.01
1
NIL
HORIZONTAL

PLOT
738
373
1000
542
Strategy: attack
Time
Frequency
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Male-Male" 1.0 0 -2674135 true "" "plot ( count organisms with [ sex = \"male\" and strategy-present? \"male\" \"attack\" ] / (count organisms + 0.00001))"
"Male-Female" 1.0 0 -10899396 true "" "plot ( count organisms with [ sex = \"male\" and strategy-present? \"female\" \"attack\" ] / (count organisms + 0.00001))"
"Female-Male" 1.0 0 -13791810 true "" "plot ( count organisms with [ sex = \"female\" and strategy-present? \"male\" \"attack\" ] / (count organisms + 0.00001))"
"Female-Female" 1.0 0 -5825686 true "" "plot ( count organisms with [ sex = \"female\" and strategy-present? \"male\" \"attack\" ] / (count organisms + 0.00001))"
"Male-Other" 1.0 0 -6459832 true "" "plot ( count organisms with [ sex = \"male\" and strategy-present? \"other\" \"attack\" ] / (count organisms + 0.00001))"
"Female-Other" 1.0 0 -13345367 true "" "plot ( count organisms with [ sex = \"female\" and strategy-present? \"other\" \"attack\" ] / (count organisms + 0.00001))"

TEXTBOX
57
311
160
329
Genetics Settings
12
0.0
1

PLOT
737
12
999
176
Strategy: eat when food
Time
Frequency
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"eat" 1.0 0 -10899396 true "" "plot ( count organisms with [strategy-present? \"food\" \"eat\"] / (count turtles + 0.000001) )"

PLOT
1008
184
1264
364
Strategy: mate
Time
Frequnecy
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"MO" 1.0 0 -6459832 true "" "plot ( count organisms with [ sex = \"male\" and strategy-present? \"other\" \"mate\" ] / (count turtles + 0.000001) )"
"MF" 1.0 0 -2674135 true "" "plot ( count organisms with [ sex = \"male\" and strategy-present? \"female\" \"mate\" ] / (count turtles + 0.000001) )"
"MM" 1.0 0 -14835848 true "" "plot ( count organisms with [ sex = \"male\" and strategy-present? \"male\" \"mate\" ] / (count turtles + 0.000001) )"
"FF" 1.0 0 -8630108 true "" "plot ( count organisms with [ sex = \"female\" and strategy-present? \"female\" \"mate\" ] / (count turtles + 0.000001) )"
"FM" 1.0 0 -5825686 true "" "plot ( count organisms with [ sex = \"female\" and strategy-present? \"male\" \"mate\" ] / (count turtles + 0.000001) )"
"FO" 1.0 0 -13345367 true "" "plot ( count organisms with [ sex = \"female\" and strategy-present? \"other\" \"mate\" ] / (count turtles + 0.000001) )"

PLOT
1010
373
1264
542
Strategy: aggression
Time
Mean
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"male" 1.0 0 -2674135 true "" "plot mean [aggression] of organisms with [ sex = \"male\" ]"
"female" 1.0 0 -5825686 true "" "plot mean [aggression] of organisms with [ sex = \"female\" ]"

PLOT
737
184
1000
364
Sex Ratio
Time
Female : Male
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (count organisms with [ sex = \"female\" ] / (count organisms with [ sex = \"male\"] + 0.0000001))"

PLOT
1007
12
1265
175
Mortality Rate
Time
Mortality
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles with [dying?] / (count turtles + 0.0000001)"

@#$#@#$#@
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

circle
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250
Polygon -7500403 true true 41 250 61 265 79 277 96 285 113 292 130 295 150 297 172 295 197 289 215 281 241 266 254 254 259 250 148 184

square
true
0
Polygon -7500403 true true 150 5 40 250 150 240 260 250
Rectangle -7500403 true true 90 225 210 285

triangle
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250
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
