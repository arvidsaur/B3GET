extensions [sound table]

breed [ baboons baboon ]
breed [ groups group ]

baboons-own [
  age
  sex
  life-history
  body-size
  previous-energy
  desired-energy
  energy
  cycle-tick
  mother
  father
  chromosomeI
  chromosomeII
  generation
  my-group
  mortality-rate
  conception-rate
  lactation-stores
  perception-range
]

groups-own [
  group-color
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
  world-resources
]

to setup-parameters
  load-chromosome
  set life-history-list [ "fetus" "infant" "juvenile" "adult" "senescent" "dead" ]
  set action-list [ "attack" "mate" "groom" "maintain" "grow" "conception" "lactate" "toward" "away" "join" "leave" ]
  set self-action-list [ "maintain" "grow" "conception" "lactate" "leave" ]
  set other-action-list [ "attack" "mate" "groom" "toward" "away" "join" ]
  set global-mutation-rate 0.05
  set global-sex-ratio 0.5
  set global-perception-range 5
  set world-resources resource-growth-rate
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
  ask groups [ update-group ]
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
    hatch-baboons individuals-per-group [
      initialize-baboon
      set chromosomeI initial-chromosome
      set chromosomeII initial-chromosome
      move-to one-of patches
      set generation 0
      set hidden? false
      set age gestation + infancy + adolescence + random 100
      set size 1
      set body-size size
      set cycle-tick random cycle-length
      set my-group myself
      set conception-rate 1
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
  if not any? baboons [ stop ]
  if count baboons > 100 [ ask n-of ( count baboons - 100 ) baboons [ die ]]
  ;if ceiling ( ticks / 10000 ) = (ticks / 10000) [ export-world (word "BABOONS20171013 G" median [generation] of baboons ".csv")]
  update-resources
  ;ask baboons [ print (word "energys "  self " " desired-energy " " energy )]
  ;print world-resources
  ask groups [ if not any? baboons with [ my-group = myself ] [ die ]]
  ;ask groups [ update-group ]
  ask baboons [ update-energy-and-metabolism ]
  ask baboons [ if energy > 0 [ make-decisions-and-do-actions ]]
  ask baboons [ update-life-history update-fertility update-status ]
  ;ask baboons [ print (word "energysII "  self " " desired-energy " " energy )]
  distribute-resources
  ;print " "
  tick
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::: GROUPS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to update-group
  setxy ( mean [xcor] of baboons with [ my-group = myself ] ) ( mean [ycor] of baboons with [ my-group = myself ] )
  ;lt random 360
  ;forward random-float 1.0
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::: RESOURCES :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to update-resources
  set world-resources world-resources + resource-growth-rate
end

to distribute-resources
  let total-desired-energy sum [desired-energy] of baboons
  ;print [desired-energy] of baboons
  if total-desired-energy > 0 [
    ask baboons [
      update-energy ( desired-energy / total-desired-energy ) * world-resources
      set previous-energy energy
      set desired-energy 0
  ]]
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
  set desired-energy 100
  set life-history "fetus"
  set body-size 0
  set conception-rate 0
  set lactation-stores 0
  set size 0
  set color blue
  set generation 0
  set my-group nobody
  set mortality-rate 0.00
  set perception-range 2
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
  if is-number? input [ set output ( abs precision ( input + random-float 1.0 - random-float 1.0 ) 2 ) ]
  if member? input action-list [ set output one-of action-list ]
  report output
end

;:::: BABOON STATUS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to-report gestation report allele-at-row-column "life_history" "ego_fetus" end
to-report infancy report allele-at-row-column "life_history" "ego_infant" end
to-report adolescence report allele-at-row-column "life_history" "ego_juvenile" end
to-report adulthood report allele-at-row-column "life_history" "ego_adult" end
to-report longevity report gestation + infancy + adolescence + adulthood + allele-at-row-column "life_history" "ego_senescent" end
to-report cycle-length report allele-at-row-column "life_history" "ego_cycling" end
to-report ovulation-length report allele-at-row-column "life_history" "ego_ovulating" end
to-report pregnancy-length report allele-at-row-column "life_history" "ego_pregnant" end
to-report lactation-length report allele-at-row-column "life_history" "ego_lactating" end

to update-energy [update]
  set energy energy + update
end

to update-mortality-rate [update]
  ;print (word "update mortality " self " " update)
  set mortality-rate mortality-rate + update
  if mortality-rate < 0 [ set mortality-rate 0 ]
end

to-report phenotype-of [ target ]
  let target-self ifelse-value ( target = self ) [ "self" ] [ "other" ]
  let target-sex [sex] of target
  let target-life-history [life-history] of target
  let target-fertility [fertility] of target
  let target-size ifelse-value ( [body-size] of target < body-size ) [ "smaller" ] [ "larger" ]
  let target-health ifelse-value ( [mortality-rate] of target < mortality-rate ) [ "healthier" ] [ "sicker" ]
  let target-group ifelse-value ( [my-group] of target = my-group ) [ "ingroup" ] [ "outgroup" ]
  report (sentence target-self target-sex target-life-history target-fertility target-size target-health target-group )
end

to-report winning-likelihood-with [ target ]
  ifelse target = nobody
  [ report 1 ]
  [ report ( body-size / ( body-size + [body-size] of target + 0.0000001)) ]
end

to update-fertility
  if sex = "female" [
    set cycle-tick cycle-tick + 1
    if fertility = "" and life-history = "adult" [ reset-fertility "cycling" ]
    if fertility = "cycling" and cycle-tick > cycle-length [ reset-fertility "ovulating" ]
    if fertility = "ovulating" and cycle-tick > ovulation-length [ reset-fertility "cycling" ]
    if fertility != "pregnant" and count baboons with [ mother = myself and life-history = "fetus" ] > 0 [ reset-fertility "pregnant" ]
    if fertility = "pregnant" and count baboons with [ mother = myself and life-history = "fetus" ] = 0 [ reset-fertility "lactating" ]
    if fertility = "lactating" and ( cycle-tick > lactation-length or count baboons with [ mother = myself and life-history = "juvenile" ] = 0 ) [ reset-fertility "cycling" ]
    if life-history = "senescent" [ reset-fertility " " ]
  ]
end

to reset-fertility [ input ]
  set label first input
  set cycle-tick 0
end

to-report fertility
  let fertile ""
  if label = "c" [ set fertile "cycling" ]
  if label = "o" [ set fertile "ovulating" ]
  if label = "p" [ set fertile "pregnant" ]
  if label = "l" [ set fertile "lactating" ]
  report fertile
end

to update-life-history
  set age age + 1
  ;set mortality-rate mortality-rate + 0.0001
  let current-history life-history
  if ( age > sum alleles-at-row-columns "life_history" ( map [ [i] -> ( word "ego_" i ) ] phenotype-of self ) ) [ set life-history item ( position current-history life-history-list + 1 ) life-history-list ]
end

to update-status
  if random-float 1.0 <= mortality-rate [ die ]
  if life-history = "fetus" or life-history = "infant" and mother != nobody  [
    move-to mother rt random 360 fd random-float 1.0 ]
  if life-history = "dead" [ print "dead" die ]
  if life-history = "infant" [
    set hidden? false
    set perception-range global-perception-range ]
  if life-history = "fetus" and mother = nobody [ die ]
  ifelse my-group = nobody
  [ set color black ]
  [ set color scale-color [group-color] of my-group age longevity 0 ]
end

to get-lactation-stores-of [ target ]
  if target = mother and mother != nobody [
    ;print (word "get stores " self " " mother " " [lactation-stores] of mother )
    set energy [ lactation-stores] of mother
    ask mother [ set lactation-stores 0 ]
  ]
end

;:::: BABOON FUNCTIONS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to update-energy-and-metabolism
  update-energy energy-received-per-timestep ; ;desired-energy ;
  ;set desired-energy 0
  if ( life-history = "fetus" or life-history = "infant" ) and mother != nobody [ get-lactation-stores-of mother ]
  let bmr-cost ( abs body-size ^ 0.762 ) * energy-cost-per-BMR
  ; infant-carrying?
  update-energy ( - bmr-cost )
  ;if life-history = "fetus" [ print (word "energy " self " " energy ) ]
end

to make-decisions-and-do-actions

  ; OBSERVATIONS
  let close-baboons []
  let decision-table table:make
  ask min-n-of ifelse-value ( count baboons > global-perception-range ) [ global-perception-range ] [ count baboons ] baboons [ distance myself ] [ set close-baboons lput self close-baboons ]
  foreach close-baboons [ [t] ->
    ; consults with chromosomes to get a list of decisions and weights for each target
    let decision-list alleles-at-rows-columns ( map [ [i] -> (word i "_decision" )] phenotype-of t ) ( map [ [i] -> ( word "ego_" i ) ] phenotype-of self )
    let weight-list alleles-at-rows-columns ( map [ [i] -> (word i "_weight" )] phenotype-of t ) ( map [ [i] -> ( word "ego_" i ) ] phenotype-of self )
    foreach decision-list [ [d] ->
      ; checks to make sure adding appropriate actions to table
      let add-to-table member? d action-list
      set add-to-table (( member? d other-action-list ) and ( t != self ) and life-history != "fetus") or ( member? d self-action-list )
      if add-to-table [
        let weight-value abs ( item ( position d decision-list ) weight-list )
        let weight ifelse-value ( distance t > 1 ) [ weight-value / ( distance t ^ 2) ] [ weight-value ]
        ; all self-directed actions use self as target
        let target ifelse-value ( member? d self-action-list ) [ self ] [ t ]
        ; actions on individuals too far away are transformed into "toward" actions
        let decision ifelse-value ( member? d other-action-list and distance target > 1 ) [ "toward" ] [ d ]
        let target-decision-key ( word target "_" decision )
        ; add or update table based on target-decision-weight combinations
        ifelse table:has-key? decision-table target-decision-key
        [ let old-weight table:get decision-table target-decision-key
          table:put decision-table target-decision-key ( old-weight + weight ) ]
        [ table:put decision-table target-decision-key weight ]]]]

  let total-weight 0
  if table:length decision-table > 0 [
    ;print decision-table
    ;if life-history = "fetus" [ print table:to-list decision-table ]
    set total-weight sum ( table:values decision-table ) ]
  let decision-list table:to-list decision-table
  let X-magnitude 0.00001 * one-of [ -1 1 ]
  let Y-magnitude 0.00001 * one-of [ -1 1 ]


  ; ACTIONS
  foreach decision-list [ [x] ->
    let action substring ( first x ) ( position "_" first x + 1) ( length first x )
    let baboon-number read-from-string ( substring ( first x ) 8 ( position ")" first x ))
    let target one-of baboons with [ who = baboon-number ]
    let weight last x
    ;print weight
    ;print (word "number " baboon-number)
    ;print (word self " action: " action " target: " target " weight: " weight )
    ifelse action = "toward" or action = "away"
    ; calculate trajectory based on weights of toward/away from targets
    [ let angle atan ([ycor] of target - ycor + 0.00001 ) ([xcor] of target - xcor + 0.00001 )
      if action = "away" [ set angle angle - 180 ]
      set X-magnitude X-magnitude + (weight * sin angle)
      set Y-magnitude Y-magnitude + (weight * cos angle) ]
    ; otherwise, do action to target with an energy calculated by weight
    [ do-action action target ( energy * weight / total-weight ) ]]

  ; MOVEMENT
  ; all energy that remains can go towards moving
  let dist sqrt ( Y-magnitude ^ 2 + X-magnitude ^ 2 )
  let engy dist * energy-per-step-unit
  ifelse ( sex  = "male" )
  [ move ( atan Y-magnitude X-magnitude ) engy ]
  [ move-female ( atan Y-magnitude X-magnitude ) engy ]
end

to move [ dirn value ]
  ;print (word self " move " dirn " " value )
  set heading dirn
  forward ( value / energy-per-step-unit ) * size
  ;repeat steps * 2 [ fd 0.5 ]
  update-energy ( - value )
end

to move-female [ dirn value ]
  let ptch one-of patches with [ distance [my-group] of myself < 3 and distance myself < 2 ]
  ifelse ptch = nobody [ face my-group ] [ face ptch ]
  forward 1
end

;::::: BABOON ACTIONS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to do-action [ string target value ]
  if distance target <= 1.0 [
    ;print ( word "action " self " "  string " " target " " value )
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

to attack [ target value ]
  ;print (word "attack " self " " target " " value )
  if target != nobody
  [ ifelse ( random-float 1.0 < winning-likelihood-with target )
    [ ask target [ update-mortality-rate ( value / energy-per-attack-unit-cost )]]
    [ update-mortality-rate ( value / energy-per-attack-unit-cost )]]
end

to mate [ target ]
  if target != nobody [ ifelse sex = "female" [ copulate target ] [ ask target [ copulate myself ]]]
end

to copulate [ target ] ; FEMALE
  ;print (word "mate " self " " target)
  if target != nobody
  [ if fertility = "ovulating" [
    if random-float 1.0 < mean (list conception-rate [conception-rate] of target) [
      hatch-baboons 1 [ initialize-baboon-with-parents myself target  ]
      reset-fertility "pregnant" ]]]
end

to groom [ target value ]
  ;print (word "groom " self " " target " " value )
  ask target [ update-mortality-rate ( - value / energy-per-groom-unit-benefit ) ]
end

to maintain [ value ]
  ;print (word "maintain " self " " value )
  update-mortality-rate ( - value / energy-per-maintenance-unit )
end

to grow [ value ]
  ;if life-history = "fetus" [ print (word "grow" " " self " " value ) ]
  set body-size body-size + ( value / energy-per-growth-unit )
  set size ifelse-value (body-size > 1) [ body-size ^ (1 / 3) ] [ body-size ]
end

to conception [ value ]
  set conception-rate conception-rate + ( value / energy-per-conception-unit )
end

to lactate [ value ]
  ;print (word "lactate " self " " value)
  set lactation-stores lactation-stores + ( value / energy-per-lactation-unit )
end

to join [ target value ]
  if target != nobody and value > energy-threshold-for-join [ set my-group [my-group] of target ]
end

to leave [ value ]
  if value > energy-threshold-for-leave [
    hatch-groups 1 [
      set hidden? true
      set group-color one-of base-colors
      set size 1
      move-to myself
      let me self
      ask myself [ set my-group me ]
  ]]
end

to forage [ value ]
  set desired-energy value * energy-to-forage-gains
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::: CHROMOSOMES :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to load-chromosome
  let file chromosome-to-load ;user-file
  if ( file != false )
  [ set initial-chromosome []
    file-open file
    while [ not file-at-end? ] [ set initial-chromosome (sentence initial-chromosome file-read) ]
    set initial-chromosome filter [ [i] -> i != "" ] initial-chromosome
    ;user-message "File loading complete!"
    file-close ]
end

to-report allele-at [ index ]
  let alleleI item index chromosomeI
  let alleleII item index chromosomeII
  let reporter ifelse-value ( index = 0 ) [ 0 ] [ phenotype-from-genotype alleleI alleleII ]
  report reporter
end

to-report phenotype-from-genotype [ allele1 allele2 ]
  let phenotype-report 0
  if is-number? allele1 and is-number? allele2 [ set phenotype-report mean ( list allele1 allele2 ) ]
  if is-string? allele1 and is-string? allele2 [ set phenotype-report first sort-by < (list allele1 allele2) ]
  report phenotype-report
end

to-report allele-at-row-column [ row column ]
  let R position row chromosomeI
  let C position column chromosomeI
  let index ifelse-value ( is-number? R and is-number? C ) [ R + C ] [ 0 ]
  report allele-at index
end

to-report alleles-at-row-columns [ row columns ]
  let reporter []
  foreach columns [ [c] -> set reporter lput allele-at-row-column row c reporter ]
  report reporter
end

to-report alleles-at-rows-columns [ rows columns ]
  let reporter []
  foreach rows [ [r] -> set reporter (sentence reporter alleles-at-row-columns r columns ) ]
  report reporter
end
@#$#@#$#@
GRAPHICS-WINDOW
213
10
755
553
-1
-1
26.7
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
19
0
19
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
994
10
1098
55
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
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
6
150
201
183
individuals-per-group
individuals-per-group
0
50
18.0
1
1
NIL
HORIZONTAL

CHOOSER
31
11
190
56
chromosome-to-load
chromosome-to-load
"chromos0me.txt"
0

MONITOR
994
62
1100
107
NIL
count baboons
0
1
11

TEXTBOX
832
10
953
28
FIXED PARAMETERS
12
0.0
1

SLIDER
766
182
979
215
energy-per-lactation-unit
energy-per-lactation-unit
0
100
5.0
1
1
NIL
HORIZONTAL

SLIDER
766
219
979
252
energy-per-conception-unit
energy-per-conception-unit
0
100
20.0
1
1
NIL
HORIZONTAL

SLIDER
767
144
979
177
energy-per-step-unit
energy-per-step-unit
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
766
256
979
289
energy-per-growth-unit
energy-per-growth-unit
0
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
766
293
979
326
energy-per-maintenance-unit
energy-per-maintenance-unit
0
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
766
329
980
362
energy-per-attack-unit-cost
energy-per-attack-unit-cost
0
1000
1000.0
1
1
NIL
HORIZONTAL

SLIDER
766
365
980
398
energy-per-groom-unit-benefit
energy-per-groom-unit-benefit
0
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
767
106
980
139
energy-cost-per-BMR
energy-cost-per-BMR
0
100
2.0
1
1
NIL
HORIZONTAL

PLOT
993
116
1241
273
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
1129
62
1226
107
NIL
count groups
17
1
11

SLIDER
996
288
1222
321
energy-threshold-for-leave
energy-threshold-for-leave
0
100
5.0
1
1
NIL
HORIZONTAL

SLIDER
996
329
1222
362
energy-threshold-for-join
energy-threshold-for-join
0
100
5.0
1
1
NIL
HORIZONTAL

SLIDER
767
68
982
101
energy-to-forage-gains
energy-to-forage-gains
0
100
6.0
1
1
NIL
HORIZONTAL

SLIDER
766
29
981
62
resource-growth-rate
resource-growth-rate
0
10000
650.0
1
1
NIL
HORIZONTAL

PLOT
1004
375
1204
525
plot 1
NIL
NIL
0.0
200.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [previous-energy] of baboons"

SLIDER
768
436
987
469
energy-received-per-timestep
energy-received-per-timestep
0
100
50.0
1
1
NIL
HORIZONTAL

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

cylinder
false
0
Circle -7500403 true true 0 0 300

flower
true
0
Circle -7500403 true true 117 12 66
Circle -7500403 true true 116 221 67
Circle -7500403 true true 41 41 67
Circle -7500403 true true 11 116 67
Circle -7500403 true true 41 191 67
Circle -7500403 true true 191 191 67
Circle -7500403 true true 221 116 67
Circle -7500403 true true 191 41 67
Circle -7500403 true true 60 60 180

food
false
0
Circle -7500403 true true 96 76 108
Circle -7500403 true true 72 104 156
Polygon -7500403 true true 221 149 195 101 106 99 80 148

fruit
true
0
Polygon -7500403 true true 33 58 0 150 30 240 105 285 135 285 150 270 165 285 195 285 255 255 300 150 268 62 226 43 194 36 148 32 105 35
Polygon -16777216 true false 120 60 180 60 150 75 120 60

herb
false
0
Rectangle -7500403 true true 135 240 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 240 120 195 150 165 180 195 165 240

leaf
true
0
Rectangle -7500403 true true 159 203 171 283
Polygon -7500403 true true 165 263 148 276 117 276 60 240 45 180 48 139 58 114 69 123 77 87 90 53 109 30 119 39 150 0 180 30 210 75 210 60 231 85 250 125 255 173 265 165 263 205 240 247 215 271 191 275

shrub
false
0
Rectangle -7500403 true true 135 165 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 165 120 120 150 90 180 120 165 165

tree
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

triangle
true
0
Polygon -7500403 true true 150 30 15 255 285 255
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
