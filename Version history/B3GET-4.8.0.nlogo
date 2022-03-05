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
  stomach-stores
  energy-stores
  mother
  father
  count-offspring
  chromosomeI
  chromosomeII
  combined-chromosome
  generation
  my-group
  life-history-chance
  fertility-chance
  mortality-chance
  conception-chance
  emigration-chance
]

groups-own [
  group-color
]

alleles-own [
  allele-ego
  allele-somebody
  allele-action
  allele-weight
  allele-mutate?
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
  chromosome-file-name
  initial-chromosome
  lower-character-list
  upper-character-list
  move-toward-list
  move-away-list
  life-history-list
  fertility-list
  global-mutation-rate
  global-sex-ratio
  global-perception-range
  median-chromosome
  global-energy-available
]

to setup-parameters
  load-chromosome
  set lower-character-list [ "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" ]
  set upper-character-list [  "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "?" "!" ]
  set move-toward-list [ "M" "T" "J" ]
  set move-away-list [ "W" "L" ]
  set life-history-list [ "gestator" "infant" "juvenile" "adult" "senescent" "dead" ]
  set fertility-list [ "cycling" "pregnant" "lactating" ]
  set global-mutation-rate 0.05
  set global-sex-ratio 0.5
  set global-perception-range 10
  set median-chromosome []
  set global-energy-available global-food-growth-rate
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
    initialize-group
  ]
end

to setup-baboons
  ask groups [
    hatch-baboons females-per-group * 2 [
      initialize-baboon
      set chromosomeI copy-chromosome initial-chromosome
      set chromosomeII copy-chromosome initial-chromosome
      mutate-chromosomes
      set combined-chromosome get-combined-chromosomes chromosomeI chromosomeII
      set generation 0
      set hidden? false
      set age random 1000
      set size 1
      set my-group myself
      move-to one-of patches with [ distance [my-group] of myself < 3 ]
      set mother nobody
      set father nobody
      set life-history "adult"
      set stomach-stores global-energy-available / count baboons
      update-fertility
      update-status

    ]]
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::: GO :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to go
  clear-links
  if ceiling ( ticks / 5000 ) = (ticks / 5000) [ export-world (word world-print-name " G" median [generation] of baboons ".csv")] ; record data
  set global-energy-available global-food-growth-rate ; grow food on world

  ask baboons [ update-energy-and-metabolism ]
  ;ask baboons [ check-fitness-function ]
  ask baboons [ update-status ]
  ask baboons [ if energy > 0 [ make-decisions-and-do-actions ]]

  ask groups [ if group-size = 0 [ die ]]
  ask groups [
    set xcor mean [xcor] of baboons with [ my-group = myself ]
    set ycor mean [ycor] of baboons with [ my-group = myself ]]
  ask vectors [ die ]
  tick
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::: GROUPS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to initialize-group
  set hidden? true
  set group-color one-of base-colors
  set size 1
  move-to one-of patches
end

to-report adult-group-size
  report count baboons with [ my-group = myself and life-history = "adult" ]
end

to-report group-size
  report count baboons with [ my-group = myself ]
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::: BABOONS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;:::: INITIALIZE BABOON :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to initialize-baboon-with-parents [ m f ]
  initialize-baboon
  set mother m
  set father f
  setup-chromosomes-from-parents m f
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
  set life-history "gestator"
  set conception-chance 0.01
  set mortality-chance 0
  set life-history-chance 0
  set emigration-chance 0
  set fertility-chance 0
  set energy-stores 0
  set stomach-stores 0
  set size 0.01
  set color black
  set generation 0
  set my-group nobody
  set energy 1
  set count-offspring 0
end

to setup-chromosomes-from-parents [m f]
  set chromosomeI copy-chromosome one-of (list [chromosomeI] of m [chromosomeII] of m ) ; one chromosome from mother
  set chromosomeII copy-chromosome one-of (list [chromosomeI] of f [chromosomeII] of f ) ; one chromosome from father
  let i 0
  while [i < length chromosomeI ] [ if random-float 1.0 < 0.5 ; half the time, alleles will switch places
    [ let itemI item i chromosomeI
      let itemII item i chromosomeII
      set chromosomeI replace-item i chromosomeI itemII
      set chromosomeII replace-item i chromosomeII itemI ]
  set i i + 1 ]
  mutate-chromosomes
  set combined-chromosome get-combined-chromosomes chromosomeI chromosomeII
end

to mutate-chromosomes
  repeat length chromosomeI * 2 [
    if random-float 1.0 < global-mutation-rate [
      let index random length chromosomeI
      ifelse random-float 1.0 < 0.5
        [ mutate-allele item index chromosomeI ]
        [ mutate-allele item index chromosomeII ]]]
end

to mutate-allele [ input-allele ]
  let index random 4
  ask input-allele [
    if allele-mutate? [
      let some-number random-float 1.0 - random-float 1.0
      set allele-weight get-updated-value allele-weight some-number
  ]]
end

;:::: BABOON STATUS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to update-energy [update]
  set energy energy + update
  if energy < 0 [ set energy 0 ]
end

to-report phenotype-of [ target ]
  let target-self ifelse-value ( target = self ) [ "e" ] [ "o" ]
  let target-alone ifelse-value ( any? baboons with [ life-history = "adult" and my-group = [my-group] of target and self != target ]) [ "w" ] [ "v" ]
  let target-sex first [sex] of target
  let target-life-history first [life-history] of target
  let target-fertility first [fertility] of target
  let target-size ifelse-value ( [size] of target < size ) [ "r" ] [ "b" ]
  let target-health ifelse-value ( [mortality-chance] of target > mortality-chance ) [ "d" ] [ "h" ]
  let target-kin "q"
  let target-group ifelse-value ( [my-group] of target = my-group ) [ "n" ] [ "u" ]
  report (word target-self target-alone target-sex target-life-history target-fertility target-size target-health target-kin target-group )
end

to-report fertility
  let fertile " "
  if label = "c" [ set fertile "cycling" ]
  if label = "p" [ set fertile "pregnant" ]
  if label = "l" [ set fertile "lactating" ]
  report fertile
end

to-report winning-likelihood-with [ target ]
  ifelse target = nobody
  [ report 1 ]
  [ report ( size / ( size + [size] of target + 0.0000001)) ]
end

to update-mortality-chance [update]
  set mortality-chance get-updated-value mortality-chance update
end

to update-conception-chance [update]
  set conception-chance get-updated-value conception-chance update
end

to update-life-history
  ifelse ( position life-history life-history-list + 1 < length life-history-list )
  [ set life-history item (( position life-history life-history-list ) + 1)  life-history-list ]
  [ set life-history first life-history-list ]
  set hidden? false
  if mother != nobody [ if [life-history] of mother = "pregnant" [ ask mother [ update-fertility ]]]
  update-fertility
end

to update-fertility
  let was-pregnant false
  ifelse member? fertility fertility-list
  [ if fertility = "pregnant" [ set was-pregnant true ]
    ifelse ( ((position fertility fertility-list ) + 1) < length fertility-list )
    [ reset-fertility item (( position fertility fertility-list ) + 1 ) fertility-list ]
    [ reset-fertility first fertility-list ]]
  [ reset-fertility first fertility-list ]
  if sex != "female" or life-history != "adult" [ reset-fertility " " ]
  if was-pregnant [ give-birth ]
end

to reset-fertility [ input ]
  set label first input
end

to update-status ; kill off alleles when you die
  set age age + 1
  ifelse my-group = nobody
  [ set color black ]
  [ set color [group-color] of my-group ]
  update-mortality-chance 0.01
  update-conception-chance (-0.01)
  set emigration-chance 0
  if life-history = "gestator" or life-history = "infant" and mother != nobody [
    move-to mother rt random 360 fd random-float 1.0 ] ; stay with mother
  if random-float 1.0 <= mortality-chance or life-history = "dead" [
    print (word "DEAD "  self " " sex " " age " " count-offspring " " life-history " " mortality-chance " " energy )
    if (count baboons with [ my-group = [my-group] of myself ] = 1) [ ask my-group [ die ]]
    foreach chromosomeI [ die ]
    foreach chromosomeII [ die ]
    die ] ; protocols for death
end

to get-energy-stores-of [ target ]
  if target = mother and mother != nobody [
    set stomach-stores stomach-stores + [ energy-stores] of mother
    ask mother [ set energy-stores 0 ]
  ]
end

;:::: BABOON FUNCTIONS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to update-energy-and-metabolism
  ;if ( life-history = "adult" and [adult-group-size] of my-group = females-per-group * 2 ) [ update-energy 1 ]
  if ( life-history = "gestator" or life-history = "infant" or life-history = "juvenile" ) [ get-energy-stores-of mother ]
  update-energy stomach-stores
  set stomach-stores 0
  let bmr-cost ( abs size ^ 0.762 )
  update-energy ( - bmr-cost )
end

to check-fitness-function
;  if (life-history = "adult" ) [
;    ; update-energy -0.01 * (abs ( [adult-group-size] of my-group - females-per-group * 2 )) ; punish if not desired group size
;    update-energy ifelse-value ( distance my-group > distance min-one-of groups with [ self != [my-group] of myself ] [ distance myself ] ) [ -0.1 ] [ 0.1 ] ; reward / punish group proximity
;  ]
end

to make-decisions-and-do-actions
  let meself self
  let self-phenotype phenotype-of meself
  let decision-vectors []

  ; DECISIONS
  ask min-n-of ifelse-value ( count baboons > ( global-perception-range * 2) ) [ ( global-perception-range * 2 ) ] [ count baboons ] baboons [distance meself] [
    if random-float 1.0 < 0.5 [
      let otherguy self
      let other-phenotype [phenotype-of otherguy] of meself
      foreach combined-chromosome [ a ->
        let ego [allele-ego] of a
        let somebody [allele-somebody] of a
        let action [allele-action] of a
        let weight [allele-weight] of a
        if first-string-is-part-of-last? ( ego ) ( self-phenotype ) [
          if first-string-is-part-of-last? ( somebody ) ( other-phenotype ) [
            ;print (word "VECTORS " meself " " action " " ego " " phenotype-of meself " " somebody " " phenotype-of otherguy )
            let vector-doesnt-exist true
            foreach decision-vectors [ v ->
              if ( [vector-target] of v = somebody ) and ( [vector-action] of v = action ) [
                set vector-doesnt-exist false
                ask v [ set vector-weight vector-weight + weight ]]]
            if vector-doesnt-exist [
              hatch-vectors 1 [
                set hidden? true
                set vector-target otherguy
                set vector-action action
                set vector-weight ifelse-value (distance meself <= 1) [ weight ] [ weight / (( distance meself ) ^ 2 ) ]
                set decision-vectors lput self decision-vectors
  ]]]]]]]

  ; NORMLIZE WEIGHTS
  let sum-weight 0
  foreach decision-vectors [ v -> set sum-weight sum-weight + [vector-weight] of v ]
  foreach decision-vectors [ v -> ask v [ set vector-weight vector-weight / sum-weight ]]

  ; ACTIONS
  let X-magnitude one-of [ 0.1 -0.1 ]
  let Y-magnitude one-of [ 0.1 -0.1 ]
  foreach decision-vectors [ v ->
    ;print (word "DECISION " self " " ([vector-action] of v) " " ([vector-target] of v) " " ([vector-weight] of v))

    if ( member? ([vector-action] of v) move-toward-list or member? ([vector-action] of v) move-away-list ) [
      let ycor-difference ([ycor] of [vector-target] of v - ycor )
      let xcor-difference ([xcor] of [vector-target] of v - xcor )
      let angle ifelse-value ( ycor-difference = 0 or xcor-difference = 0 ) [ random 360 ] [ atan ycor-difference xcor-difference ]
      if ( member? ([vector-action] of v) move-away-list ) [ set angle angle - 180 ]
      set X-magnitude X-magnitude + ([vector-weight] of v * sin angle)
      set Y-magnitude Y-magnitude + ([vector-weight] of v * cos angle)
    ]
    if distance [vector-target] of v <= 1 [ do-action ([vector-action] of v) ([vector-target] of v) ([vector-weight] of v) ]
  ]


  ; MOVEMENT
  move ( atan Y-magnitude X-magnitude ) energy
end

to move [ dirn value ]
  set heading dirn
  forward get-updated-value size value
  update-energy ( - value )
end

;::::: BABOON ACTIONS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to do-action [ action-string target value ]
  if action-string = "A" [ attack target value ]
  if action-string = "M" [ mate target ]
  if action-string = "G" [ groom target value ]
  if action-string = "N" [ maintain value ]
  if action-string = "R" [ grow value ]
  if action-string = "C" [ conception value ]
  if action-string = "S" [ lactate value ]
  if action-string = "J" [ join target value ]
  if action-string = "L" [ leave value ]
  if action-string = "F" [ forage value ]
  if action-string = "U" [ level-up value ]
  if action-string = "B" [ level-up-fertility value ]
  update-energy ( - value )
end

to attack [ target value ]
  ifelse ( random-float 1.0 < winning-likelihood-with target )
  [ ask target [ update-mortality-chance value ]]
  [ update-mortality-chance value ]
end

to mate [ target ]
  if target != self [
    ifelse in-link-neighbor? target
    [ if my-group = [my-group] of target
      [ ifelse sex = "female"
        [ check-conception target ]
        [ ask target [ check-conception myself ]]]]
    [ create-link-to target [ set hidden? true ]]]
end

to check-conception [ target ] ; FEMALE PROCEDURE
  if fertility = "cycling" and life-history = "adult" and [life-history] of target = "adult" [
    if random-float 1.0 < mean (list conception-chance [conception-chance] of target) [
      hatch-baboons 1 [ initialize-baboon-with-parents myself target ]
      set count-offspring count-offspring + 1
      ask target [ set count-offspring count-offspring + 1 ]
      update-fertility ]]
end

to groom [ target value ]
  ask target [ update-mortality-chance ( - value ) ]
end

to join [ target value ]
 if random-float 1.0 < value [
    ;print (word "JOIN " self " " my-group " " target " " [my-group] of target " " value )
    set my-group [my-group] of target
  ]
end

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to maintain [ value ]
  update-mortality-chance ( - value )
end

to grow [ value ]
  set size get-updated-value size value
end

to conception [ value ]
  update-conception-chance value
end

to lactate [ value ]
 set energy-stores energy-stores + value
end

to leave [ value ]
  set emigration-chance get-updated-value emigration-chance value
  if random-float 1.0 < emigration-chance [
    hatch-groups 1 [
      initialize-group
      ask myself [ set my-group myself ]
  ]]
end

to forage [ value ]
  let energy-eaten e ^ value
  ifelse ( energy-eaten < global-energy-available )
  [ set stomach-stores stomach-stores + energy-eaten
    set global-energy-available global-energy-available - energy-eaten ]
  [ set stomach-stores stomach-stores + global-energy-available
    set global-energy-available 0 ]
end

to level-up [ value ]
  set life-history-chance get-updated-value life-history-chance value
  let new-vale random-float 1.0
  if new-vale < life-history-chance [
    update-life-history
    set life-history-chance 0 ]
end

to level-up-fertility [ value ]
  set fertility-chance get-updated-value fertility-chance value
  if random-float 1.0 < fertility-chance [
    update-fertility
    set fertility-chance 0 ]
end

to give-birth
  let baby one-of baboons with [ mother = myself and life-history = "gestator" ]
  if baby != nobody [ ask baby [
    update-life-history
    set hidden? false ]]
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::: FUNCTIONS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to load-chromosome
  let file ifelse-value ( chromosome-file-name = 0 ) [chromosome-to-load] [chromosome-file-name]
  if ( file != false )
  [ set initial-chromosome []
    file-open file
    while [ not file-at-end? ]
    [ let eG file-read
      let sO file-read
      let aC file-read
      let wE file-read
      let aM file-read
      let new-allele nobody
      create-alleles 1 [
        set hidden? true
        set new-allele self
        set allele-ego eG
        set allele-somebody sO
        set allele-action aC
        set allele-weight wE
        set allele-mutate? read-from-string aM
      ]
      set initial-chromosome (sentence initial-chromosome new-allele) ]
    file-close ]
end

to-report copy-chromosome [ input-chromosome ]
  let output-chromosome []
  foreach input-chromosome [ a ->
    set output-chromosome lput copy-allele a output-chromosome ]
  report output-chromosome
end

to-report copy-allele [ input-allele ]
  let output-allele nobody
  hatch-alleles 1 [
    set hidden? true
    set output-allele self
    set allele-ego [allele-ego] of input-allele
    set allele-somebody [allele-somebody] of input-allele
    set allele-action [allele-action] of input-allele
    set allele-weight [allele-weight] of input-allele
    set allele-mutate? [allele-mutate?] of input-allele ]
  report output-allele
end

to-report get-combined-chromosomes [ chromosome1 chromosome2 ]
  let new-chromosome copy-chromosome chromosome1
  foreach new-chromosome [ a ->
    let allele-weight-2 [allele-weight] of item ( position a new-chromosome ) chromosome2
    ask a [ set allele-weight (( allele-weight + allele-weight-2 ) / 2 )]
  ]
  report new-chromosome
end

to-report generate-phenotype-string
  let output-string ""
  repeat random 10 [ set output-string insert-item length output-string output-string one-of lower-character-list ]
  report output-string
end

to-report first-string-is-part-of-last? [ string1 string2 ]
  let first-is-part-of-last true
  let index 0
  while [index < length string1] [
    if ( not member? item index string1 string2 ) and ( item index string1 != " " ) [ set first-is-part-of-last false ]
    set index index + 1 ]
  report first-is-part-of-last
end

to-report get-updated-value [ current-value update-value ]
  let report-value ifelse-value ( current-value = 0 ) [ 0.0001 ] [ current-value ]
  ifelse update-value < 0
  [ set report-value ( report-value ^ (1 + abs update-value) ) ]
  [ set report-value ( report-value ^ (1 / ( 1 + update-value) )) ]
  report report-value
end

to-report printout-chromosome
  let filename (word world-print-name "_c" random 10000 )
  file-open filename
  foreach initial-chromosome [ a ->
    file-print (word
      "\"" [allele-ego] of a "\"" " "
      "\"" [allele-somebody] of a "\"" " "
      "\"" [allele-action] of a "\"" " "
      precision (mean [[allele-weight] of (item (position a initial-chromosome) chromosomei)] of baboons ) 10 " "
      "\"" [allele-mutate?] of a "\"" )]
  file-close
  report filename
end
@#$#@#$#@
GRAPHICS-WINDOW
214
10
720
517
-1
-1
9.96
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
739
169
813
214
Generation
median [generation] of baboons
2
1
11

SLIDER
20
111
197
144
number-of-groups
number-of-groups
1
50
6.0
1
1
NIL
HORIZONTAL

SLIDER
20
150
197
183
females-per-group
females-per-group
1
50
8.0
1
1
NIL
HORIZONTAL

CHOOSER
18
11
205
56
chromosome-to-load
chromosome-to-load
"chromos0me.txt"
0

MONITOR
891
168
969
213
# baboons
count baboons
0
1
11

PLOT
739
10
956
162
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
819
169
885
214
# groups
count groups
17
1
11

INPUTBOX
32
237
189
297
world-print-name
20180129
1
0
String

BUTTON
67
309
148
342
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
20
190
199
223
global-food-growth-rate
global-food-growth-rate
0
995
955.0
5
1
NIL
HORIZONTAL

MONITOR
975
168
1039
213
# females
count baboons with [ sex = \"female\" ]
0
1
11

MONITOR
1044
168
1099
213
# males
count baboons with [ sex = \"male\" ]
17
1
11

PLOT
964
11
1177
163
Age
age
count
0.0
1500.0
0.0
5.0
true
false
"" ""
PENS
"default" 100.0 1 -16777216 true "" "histogram [age] of baboons"

PLOT
739
220
951
364
Female Reproductive Skew
# offspring
count
0.0
20.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [count-offspring] of baboons with [ sex = \"female\" and life-history = \"adult\" ]"

PLOT
957
220
1182
364
Male Reproductive Skew
# offspring
count
0.0
20.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [count-offspring] of baboons with [ sex = \"male\" and life-history = \"adult\"]"

MONITOR
1104
168
1187
213
M:F size ratio
median [size] of baboons with [ sex = \"male\" ] / median [size] of baboons with [ sex = \"female\" ]
2
1
11

PLOT
739
369
953
519
Group Size
group size
count
0.0
100.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [adult-group-size] of groups"

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
