globals [
  sex_list
  shape_list
  color_list
  check_list
  masterGenes
  chromosomeLength
  input_list
  output_list
  intial_threshold_hunger
  male_male
  male_female
  female_male
  female_female]

breed [organisms organism]

patches-own [ penergy ]
organisms-own [ sex energy threshold_hunger generation morph_chromosome act_chromosome sex_chromosome starting_act depth sired ]

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
  set check_list [ "shape?" "color?" "sex?" "energy?" "guy?" "patch?" ]
  set input_list [ "male" "female" "circle" "triangle" "square" "red" "orange" "green" "blue" "magenta" "hungry" "satiated" "alone" "other" "food" "empty"]
  set output_list [ "forward" "mate" "eat" "turn" "attack" "shape?" "color?" "sex?" "energy?" "guy?" "patch?" ]
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
  set male_male 0
  set female_male 0
  set male_female 0
  set female_female 0
  ask organisms [
    check-death
    act ]
  tick
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
  set depth 0
  do starting_act
  ;turn
  ;if (self = organism 53643) [ print " " ]
  update-energy (- move-cost)
end

to input-output [ input ]
  set depth depth + 1
  ;if (self = organism 53643) [ print input ]
  let chromosome-list sentence act_chromosome sex_chromosome
  ifelse member? input chromosome-list [
    let index (position input chromosome-list) + 1
    let still_output ( index < length chromosome-list)
    while [still_output] [
      do item index chromosome-list
      set index index + 1
      ifelse (index < length chromosome-list) [
        ifelse member? ( item index chromosome-list ) output_list [
          set still_output true
        ] [ set still_output false ]
      ] [ set still_output false ]
    ]
  ] [ turn ]
end

to do [code]
  ;if (self = organism 53643) [ print code ]
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

to move
  ifelse not any? other turtles-on patch-ahead 1 [ fd 1 ] [ turn ]
  update-energy (- move-cost)
end

to turn
  set depth depth + 1
  ifelse random 2 = 0 [ rt 90 ] [ lt 90 ]
  update-energy (- move-cost)
  ;move
end

to eat
  if ([penergy] of patch-ahead 1 > 0) [
    update-energy [penergy] of patch-ahead 1
    ask patch-ahead 1 [ set penergy 0 ] ]
end

to attack
  let guy one-of organisms-on patch-ahead 1
  if guy != nobody [
    if (sex = "male" and [sex] of guy = "male") [ set male_male male_male + 1]
    if (sex = "male" and [sex] of guy = "female") [ set male_female male_female + 1]
    if (sex = "female" and [sex] of guy = "male") [ set female_male female_male + 1]
    if (sex = "female" and [sex] of guy = "female") [ set female_female female_female + 1]
    if (guy != nobody) [ ask guy [ update-energy (- attack-cost)
        ;print (word self " "  guy )
        ]]]
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
      ifelse ([sex] of me = sex) [ set sex_chromosome [sex_chromosome] of self ] [ set sex_chromosome [sex_chromosome] of partner ]
      mutate-genes
      set generation [generation] of me + 1
    ]
    set sired sired + 1
    ask partner [ set sired sired + 1 ]
    update-energy (- birth-cost)
  ]
end

to check-death
  if energy <= 0 [ die ]
end

to update-energy [ value ]
  set energy energy + value
  check-death
end

to mutate-genes
  if random-float 1.0 < mutation-rate [
    let value random 10
    if value = 1 [ set starting_act one-of check_list ]
    if value = 2 [ set threshold_hunger random birth-cost ]
    if value = 3 [ set morph_chromosome replace-item 0 morph_chromosome one-of sex_list ]
    if value = 4 [ set morph_chromosome replace-item 1 morph_chromosome one-of shape_list ]
    if value = 5 [ set morph_chromosome replace-item 2 morph_chromosome one-of color_list ]
    if value = 6 [ if length act_chromosome > 0 [set act_chromosome replace-item (random length act_chromosome) act_chromosome (one-of input_list) ]]
    if value = 7 [ if length act_chromosome > 0 [set act_chromosome replace-item (random length act_chromosome) act_chromosome (one-of output_list) ]]
    if value = 8 [ if length sex_chromosome > 0 [set sex_chromosome replace-item (random length sex_chromosome) sex_chromosome (one-of input_list) ]]
    if value = 9 [ if length sex_chromosome > 0 [set sex_chromosome replace-item (random length sex_chromosome) sex_chromosome (one-of output_list) ]]
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
5.0
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
100.0
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
740
12
858
57
NIL
count organisms
17
1
11

PLOT
738
63
1024
291
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
"sex F:M" 1.0 0 -7500403 true "" "plot (count organisms with [sex = \"female\"]) / (count organisms with [sex = \"male\"])"

INPUTBOX
11
392
114
452
initial-organisms
10.0
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
865
12
977
57
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
0.1
.01
1
NIL
HORIZONTAL

PLOT
738
300
1024
537
Evolution of Aggression
Time
Attack Count
0.0
10.0
0.0
10.0
true
true
"" ";ask plot [ wait 50 [ clear-plot ]]"
PENS
"Male-Male" 1.0 0 -2674135 true "" "plot male_male"
"Male-Female" 1.0 0 -10899396 true "" "plot male_female"
"Female-Male" 1.0 0 -13791810 true "" "plot female_male"
"Female-Female" 1.0 0 -5825686 true "" "plot female_female"

TEXTBOX
57
311
160
329
Genetics Settings
12
0.0
1

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
