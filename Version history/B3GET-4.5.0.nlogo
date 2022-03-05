extensions [sound table]

breed [ baboons baboon ]
breed [ groups group ]
breed [ alleles allele ]
breed [ vectors vector ]

baboons-own [
  age
  sex
  life-history
  energy
  mother
  father
  chromosomeI
  chromosomeII
  generation
  my-group
  energy-stores
  levelup-chance
  mortality-chance
  conception-chance
  emigration-chance
  ovulation-chance
  signalX-chance
  signalY-chance
  signalZ-chance
]

groups-own [
  group-color
]

alleles-own [
  allele-ego
  allele-somebody
  allele-action
  allele-weight
]

vectors-own [
  vector-target
  vector-action
  vector-weight
]

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::: INITIAL PARAMETERS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

globals [
  initial-chromosome
  action-list
  life-history-list
  self-action-list
  other-action-list
  global-mutation-rate
  global-sex-ratio
  global-perception-range
  median-chrome
  character-list
]

to setup-parameters
  load-chromosome
  set character-list [ "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "?" "!" ]
  set life-history-list [ "fetus" "infant" "juvenile" "adult" "senescent" "dead" ]

  set action-list [ "attack" "mate" "groom" "maintain" "grow" "conception" "lactate" "toward" "away" "join" "leave" ]
  set self-action-list [ "maintain" "grow" "conception" "lactate" "leave" ]
  set other-action-list [ "attack" "mate" "groom" "toward" "away" "join" ]
  set global-mutation-rate 0.05
  set global-sex-ratio 0.5
  set global-perception-range 5
  set median-chrome []

end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::: SETUP ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to setup-world
  clear-all
  setup-parameters
  setup-patches
  setup-groups
  setup-baboons
  reset-ticks
end

to setup-patches
  ask patches [ set pcolor green + 2 ]
end

to setup-groups
  create-groups number-of-groups [
    set hidden? true
    set group-color one-of base-colors
    set size 1
    move-to one-of patches
  ]
end

to setup-baboons
  ask groups [
    hatch-baboons females-per-group * 2 [
      initialize-baboon
      set chromosomeI initial-chromosome
      set chromosomeII initial-chromosome
      set generation 0
      set hidden? false
      set age random 100
      set size 1
      ;set cycle-tick random cycle-length
      set my-group myself
      move-to one-of patches with [ distance [my-group] of myself < 3 ]
      set mother nobody
      set father nobody
      mutate-chromosomes 0.25
      set life-history "adult"
      update-fertility
      update-status
    ]]
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::: GO :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to go
  clear-links

  if not any? baboons [ stop ]
  let max-individuals count groups * females-per-group * 2
  if count baboons > max-individuals [ ask n-of ( count baboons - max-individuals ) baboons [ die ]]
  ask groups [ if count baboons with [ sex = "female" and my-group = myself ] > females-per-group [ ask n-of (count baboons with [ sex = "female" and my-group = myself ] - females-per-group) baboons with [ sex = "female" ] [ die ]]]
  ask groups [ if not any? baboons with [ my-group = myself ] [ die ]]
  if ceiling ( ticks / 5000 ) = (ticks / 5000) [
    set median-chrome get-median-chromosome
    ;export-world (word "BABOONS20171129 " world-print-name " G" median [generation] of baboons ".csv")
  ]

  ask baboons [ update-energy-and-metabolism ]
  ask baboons [ if energy > 0 [ make-decisions-and-do-actions ]]
  ask baboons [ update-life-history update-fertility update-status ]

  tick
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::: BABOONS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;:::: INITIALIZE BABOON :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to initialize-baboon-with-parents [ m f ]
  initialize-baboon
  set mother m
  set father f
  setup-chromosomes-from m f
  set generation [generation] of m + 1
  move-to m
  set color [color] of mother
  set my-group [my-group] of mother
end

to initialize-baboon
  set sex ifelse-value ( random-float 1.0 < global-sex-ratio ) ["male"] ["female"]
  set shape ifelse-value ( sex = "female" ) ["circle"] ["triangle"]
  set hidden? true
  set label-color grey
  set label ""
  set age 0
  set life-history "fetus"
  set conception-chance 0.01
  set mortality-chance 0.01
  set levelup-chance 0.01
  set emigration-chance 0.01
  set ovulation-chance 0.01
  set signalX-chance 0.01
  set signalY-chance 0.01
  set signalZ-chance 0.01
  set energy-stores 0
  set size 0.01
  set color blue
  set generation 0
  set my-group nobody
end

to setup-chromosomes-from [m f]
  set chromosomeI one-of (list [chromosomeI] of m [chromosomeII] of m )
  set chromosomeII one-of (list [chromosomeI] of f [chromosomeII] of f )
  let i 0
  while [i < length chromosomeI ] [ if random 100 < 50
    [ let itemI item i chromosomeI
      let itemII item i chromosomeII
      set chromosomeI replace-item i chromosomeI itemII
      set chromosomeII replace-item i chromosomeII itemI ]
  set i i + 1 ]
  mutate-chromosomes global-mutation-rate
end

to mutate-chromosomes [ rate ]
  repeat length chromosomeI * 2 [
    if random-float 1.0 < rate [
      let index random length chromosomeI
      ifelse random-float 1.0 < 0.5
        [ set chromosomeI replace-item index chromosomeI mutate-allele item index chromosomeI ]
        [ set chromosomeII replace-item index chromosomeII mutate-allele item index chromosomeII ]]]
end

to-report mutate-allele [ input ]
  let output input
  if is-number? input [ set output ( abs precision ( input + random-float input - random-float input ) 2 ) ]
  if member? input action-list [ set output one-of action-list ]
  report output
end

;:::: BABOON STATUS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to update-energy [update]
  set energy energy + update
end

to update-mortality-rate [update]
  if mortality-chance = 0 [ set mortality-chance 0.01 ]
  ifelse update < 0
    [ set mortality-chance ( mortality-chance ^ (1 + abs update) ) ]
    [ set mortality-chance ( mortality-chance ^ (1 / ( 1 + update) )) ]
end

to update-conception-chance [update]
  if conception-chance = 0 [ set conception-chance 0.01 ]
  ifelse update < 0
    [ set conception-chance ( conception-chance ^ (1 + abs update) ) ]
    [ set conception-chance ( conception-chance ^ (1 / ( 1 + update) )) ]
end

to-report phenotype-of [ target ]
  let target-self ifelse-value ( target = self ) [ "e" ] [ "o" ]
  let target-sex first [sex] of target
  let target-life-history first [life-history] of target
  let target-fertility first [fertility] of target
  let target-size ifelse-value ( [size] of target < size ) [ "r" ] [ "b" ]
  let target-health ifelse-value ( [mortality-chance] of target < mortality-chance ) [ "h" ] [ "k" ]
  let target-group ifelse-value ( [my-group] of target = my-group ) [ "n" ] [ "u" ]
  report (word target-self target-sex target-life-history target-fertility target-size target-health target-group )
end

to-report winning-likelihood-with [ target ]
  ifelse target = nobody
  [ report 1 ]
  [ report ( size / ( size + [size] of target + 0.0000001)) ]
end

to update-fertility
  if sex = "female" [
;    if fertility = "" and life-history = "adult" [ reset-fertility "cycling" ]
;    if fertility = "cycling" and cycle-tick > cycle-length [ reset-fertility "ovulating" ]
;    if fertility = "ovulating" and cycle-tick > 1 [ reset-fertility "cycling" ]
;    if fertility != "pregnant" and count baboons with [ mother = myself and life-history = "fetus" ] > 0 [ reset-fertility "pregnant" ]
;    if fertility = "pregnant" and count baboons with [ mother = myself and life-history = "fetus" ] = 0 [ reset-fertility "lactating" ]
;    if fertility = "lactating" and ( cycle-tick > lactation-length or count baboons with [ mother = myself and life-history = "juvenile" ] = 0 ) [ reset-fertility "cycling" ]
;    if life-history = "senescent" [ reset-fertility " " ]
  ]
end

to reset-fertility [ input ]
  set label first input
end

to-report fertility
  let fertile " "
  if label = "c" [ set fertile "cycling" ]
  if label = "o" [ set fertile "ovulating" ]
  if label = "p" [ set fertile "pregnant" ]
  if label = "l" [ set fertile "lactating" ]
  report fertile
end

to update-life-history
  set age age + 1
  let current-history life-history
  ;if ( age > sum alleles-at-row-columns "life_history" ( map [ [i] -> ( word "ego_" i ) ] phenotype-of self ) ) [ set life-history item ( position current-history life-history-list + 1 ) life-history-list ]
end

to update-status
  update-mortality-rate 1
  update-conception-chance (-1)
  if random-float 1.0 <= mortality-chance [ die ]
  if life-history = "fetus" or life-history = "infant" and mother != nobody  [
    move-to mother rt random 360 fd random-float 1.0 ]
  if life-history = "dead" [ die ]
  if life-history = "infant" [
    set hidden? false ]
  if life-history = "fetus" and mother = nobody [ die ]
;  ifelse my-group = nobody
;  [ set color black ]
;  [ set color scale-color [group-color] of my-group age longevity 0 ]
end

to get-energy-stores-of [ target ]
  if target = mother and mother != nobody [
    set energy [ energy-stores] of mother
    ask mother [ set energy-stores 0 ]
  ]
end

;:::: BABOON FUNCTIONS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to update-energy-and-metabolism
  ifelse ( life-history = "fetus" or life-history = "infant" )
  [ if mother != nobody [ get-energy-stores-of mother ]]
  [ update-energy energy-received-per-timestep ]
  let bmr-cost ( abs size ^ 0.762 ) * energy-cost-per-BMR
  update-energy ( - bmr-cost )
end

to make-decisions-and-do-actions
  let meself self
  let decision-vectors []

  ; DECISIONS
  ask min-n-of ifelse-value ( count baboons > global-perception-range ) [ global-perception-range ] [ count baboons ] baboons [distance meself] [
    let otherguy self
    foreach (sentence chromosomeI chromosomeII) [ a ->
      let ego [allele-ego] of a
      let somebody [allele-somebody] of a
      let action [allele-action] of a
      let weight [allele-weight] of a
      if first-string-is-part-of-last? ( ego ) ( phenotype-of meself ) [
        if first-string-is-part-of-last? ( somebody ) ( phenotype-of otherguy ) [
          let target nobody
          if ( member? action other-action-list and somebody != meself ) [ set target somebody ]
          if ( member? action self-action-list ) [ set target meself ]
          add-vector-to-list target action weight decision-vectors
    ]]]]

  ;print decision-vectors

  ; distance - ifelse-value ( distance t > 1 ) [ weight-value / ( distance t ^ 2) ] [ weight-value ]

  ; ACTIONS
  ;set total-weight sum ( table:values decision-table )
  let X-magnitude 0.00001 * one-of [ -1 1 ]
  let Y-magnitude 0.00001 * one-of [ -1 1 ]
  foreach decision-vectors [ x ->
    ;let angle atan ([ycor] of [target] of x - ycor + 0.00001 ) ([xcor] of [target] of x - xcor + 0.00001 )
    ;if [action] of x = "W" [ set angle angle - 180 ]
    ;set X-magnitude X-magnitude + (weight * sin angle)
    ;set Y-magnitude Y-magnitude + (weight * cos angle)
    ;if distance [target] of x < 1 [ do-action [action] of x [target] of x "weight" ]
  ]

  ; MOVEMENT
;  let dist sqrt ( Y-magnitude ^ 2 + X-magnitude ^ 2 )
;  let engy dist * energy-per-step-unit
;  move ( atan Y-magnitude X-magnitude ) engy
end

to move [ dirn value ]
  set heading dirn
  ;forward ( value / energy-per-step-unit ) * size
  update-energy ( - value )
end

;::::: BABOON ACTIONS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to do-action [ string target value ]
  ; make sure fetus can't do things
  if distance target <= 1.0 [
    if string = "attack" [ attack target value ]
    if string = "mate" [ mate target ]
    if string = "groom" [ groom target value ]
    if string = "maintain" [ maintain value ]
    if string = "grow" [ grow value ]
    if string = "conception" [ conception value ]
    if string = "lactate" [ lactate value ]
    if string = "join" [ join target value ]
    if string = "leave" [ leave value ]
    ;if string = "forage" [ forage value ]
    update-energy ( - value )
  ]
end

to attack [ t value ]
  ;if target != nobody
  ;[ ifelse ( random-float 1.0 < winning-likelihood-with target )
  ;  [ ask target [ update-mortality-rate ( value / energy-per-attack-unit-cost )]]
  ;  [ update-mortality-rate ( value / energy-per-attack-unit-cost )]]
end

to mate [ t ]
;  if target != nobody [
;    ifelse in-link-neighbor? target
;    [ if my-group = [my-group] of target
;      [ ifelse sex = "female"
;        [ copulate target ]
;        [ ask target [ copulate myself ]]]]
;    [ create-link-to target ]]
end

to copulate [ t ] ; FEMALE
;  if t != nobody
;  [ if fertility = "ovulating" and life-history = "adult" and [life-history] of target = "adult" [
;    if random-float 1.0 < mean (list conception-chance [conception-chance] of target) [
;      hatch-baboons 1 [ initialize-baboon-with-parents myself target  ]
;      reset-fertility "pregnant" ]]]
end

to groom [ t value ]
  ;ask t [ update-mortality-rate ( - value / energy-per-groom-unit-benefit ) ]
end

to maintain [ value ]
  ;update-mortality-rate ( - value / energy-per-maintenance-unit )
end

to grow [ value ]
  ;set body-size body-size + ( value / energy-per-growth-unit )
  ;set size ifelse-value (body-size > 1) [ body-size ^ (1 / 3) ] [ body-size ]
end

to conception [ value ]
  ;update-conception-chance ( value / energy-per-conception-unit )
end

to lactate [ value ]
 ; set energy-stores energy-stores + ( value / energy-per-lactation-unit )
end

to join [ t value ]
 ; if target != nobody and value > energy-threshold-for-join [ set my-group [my-group] of t ]
end

to leave [ value ]
  ;if value > energy-threshold-for-leave [ set my-group nobody ]
end

;to forage [ value ]
;  set desired-energy value * energy-to-forage-gains
;end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::: CHROMOSOMES :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to load-chromosome
  let file chromosome-to-load
  if ( file != false )
  [ set initial-chromosome []
    file-open file
    while [ not file-at-end? ]
    [ let eG file-read
      let sO file-read
      let aC file-read
      let wE file-read
      let new-allele nobody
      create-alleles 1 [
        set hidden? true
        set new-allele self
        set allele-ego eG
        set allele-somebody sO
        set allele-action aC
        set allele-weight wE ]
      set initial-chromosome (sentence initial-chromosome new-allele) ]
    file-close ]
end

to-report allele-at [ index ]
  let allele-to-report []
;  let alleleI item index chromosomeI
;  let alleleII item index chromosomeII
;  if [allele-dominant] of alleleI and [allele-dominant] of alleleII [ set allele-to-report one-of (sentence alleleI alleleII) ]
;  if [allele-dominant] of alleleI and not [allele-dominant] of alleleII [ set allele-to-report alleleI ]
;  if not [allele-dominant] of alleleI and [allele-dominant] of alleleII [ set allele-to-report alleleII ]
;  if not [allele-dominant] of alleleI and not [allele-dominant] of alleleII [ set allele-to-report one-of (sentence alleleI alleleII) ]
  report allele-to-report
end

to-report get-combined-chromosome
  let combined-chromosome []
  let index 0
  while [index < length chromosomeI] [
    set combined-chromosome lput ( allele-at index ) combined-chromosome
    set index index + 1 ]
  report combined-chromosome
end

to-report first-string-is-part-of-last? [ thing1 thing2 ]
  let first-is-part-of-last true
  let index 0
  while [index < length thing1] [
    if ( not member? item index thing1 thing2 ) and ( item index thing1 != " " ) [ set first-is-part-of-last false ]
    set index index + 1 ]
  report first-is-part-of-last
end

to add-vector-to-list [ t a w vList ] ; also must check if this combo exists already!!
 ;   create-vectors 1 [
;    set target t
;    set action a
;    set weight w
;  ]
;  foreach vList [ l ->
;    if ( [vector-target] of v = [vector-target] of l ) and ( [vector-action] of v = [vector-action] of l )
;    [ report position l vList ]]
end



; set initial-chromosome filter [ [i] -> i != "" ] initial-chromosome






to-report get-median-chromosome
  let median-chromosome-list []
  let chromosome-length length [chromosomeI] of one-of baboons
  let index 0
  while [ index < chromosome-length ] [
    let allele-list []
    ask baboons [ set allele-list lput allele-at index allele-list ]
    ifelse is-number? first allele-list
    [ set median-chromosome-list lput median allele-list median-chromosome-list ]
    [ set median-chromosome-list lput first modes allele-list median-chromosome-list ]
    set index index + 1 ]
  report median-chromosome-list
end
@#$#@#$#@
GRAPHICS-WINDOW
214
10
742
539
-1
-1
10.4
1
10
1
1
1
0
1
1
1
0
49
0
49
0
0
1
ticks
30.0

BUTTON
20
66
109
99
setup
setup-world
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
116
66
198
99
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
1

MONITOR
764
112
868
157
Generation
median [generation] of baboons
2
1
11

SLIDER
7
111
202
144
number-of-groups
number-of-groups
0
50
9.0
1
1
NIL
HORIZONTAL

SLIDER
6
150
201
183
females-per-group
females-per-group
0
50
9.0
1
1
NIL
HORIZONTAL

CHOOSER
31
11
206
56
chromosome-to-load
chromosome-to-load
"chromos0me.txt"
0

MONITOR
764
164
870
209
NIL
count baboons
0
1
11

TEXTBOX
803
10
924
28
FIXED PARAMETERS
12
0.0
1

SLIDER
764
71
977
104
energy-cost-per-BMR
energy-cost-per-BMR
0
100
0.0
1
1
NIL
HORIZONTAL

PLOT
763
218
1011
375
Generations
generation
count
0.0
100.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [generation] of baboons"

MONITOR
899
164
996
209
NIL
count groups
17
1
11

SLIDER
764
34
977
67
energy-received-per-timestep
energy-received-per-timestep
0
100
0.0
1
1
NIL
HORIZONTAL

INPUTBOX
35
197
178
257
world-print-name
0
1
0
String

@#$#@#$#@
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

circle
true
0
Circle -7500403 true true 0 0 300

circlex
true
0
Circle -7500403 true true 0 0 300
Circle -1184463 true false 103 13 92

circlexy
true
0
Circle -7500403 true true 0 0 300
Circle -1184463 true false 103 13 92
Polygon -2674135 true false 30 120 270 120 285 165 15 165 30 120

circlexyz
true
0
Circle -7500403 true true 0 0 300
Circle -1184463 true false 103 13 92
Polygon -14835848 true false 150 210 270 180 240 240 150 285 60 240 30 180
Polygon -2674135 true false 30 120 270 120 285 165 15 165 30 120

circlexz
true
0
Circle -7500403 true true 0 0 300
Circle -1184463 true false 103 13 92
Polygon -14835848 true false 150 210 270 180 240 240 150 285 60 240 30 180

circley
true
0
Circle -7500403 true true 0 0 300
Polygon -2674135 true false 30 120 270 120 285 165 15 165 30 120

circleyz
true
0
Circle -7500403 true true 0 0 300
Polygon -14835848 true false 150 210 270 180 240 240 150 285 60 240 30 180
Polygon -2674135 true false 30 120 270 120 285 165 15 165 30 120

circlez
true
0
Circle -7500403 true true 0 0 300
Polygon -14835848 true false 150 210 270 180 240 240 150 285 60 240 30 180

triangle
true
0
Polygon -7500403 true true 150 30 15 255 285 255

trianglex
true
0
Polygon -7500403 true true 150 30 15 255 285 255
Circle -1184463 true false 120 75 60

trianglexy
true
0
Polygon -7500403 true true 150 30 15 255 285 255
Circle -1184463 true false 120 75 60
Polygon -2674135 true false 90 150 210 150 225 180 75 180

trianglexyz
true
0
Polygon -7500403 true true 150 30 15 255 285 255
Circle -1184463 true false 120 75 60
Polygon -2674135 true false 90 150 210 150 225 180 75 180
Polygon -14835848 true false 75 195 225 195 255 240 45 240

trianglexz
true
0
Polygon -7500403 true true 150 30 15 255 285 255
Circle -1184463 true false 120 75 60
Polygon -14835848 true false 75 195 225 195 255 240 45 240

triangley
true
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -2674135 true false 90 150 210 150 225 180 75 180

triangleyz
true
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -2674135 true false 90 150 210 150 225 180 75 180
Polygon -14835848 true false 75 195 225 195 255 240 45 240

trianglez
true
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -14835848 true false 75 195 225 195 255 240 45 240
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
