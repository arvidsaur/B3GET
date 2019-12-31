;-----------------------------------------------------------------------------------
;
;   888888ba  d8888b.  .88888.   88888888b d888888P
;   88    `8b     `88 d8'   `88  88           88
;  a88aaaa8P'  aaad8' 88        a88aaaa       88
;   88   `8b.     `88 88   YP88  88           88
;   88    .88     .88 Y8.   .88  88           88
;   88888888P d88888P  `88888'   88888888P    dP
;
;-----------------------------------------------------------------------------------

extensions [ csv profiler table ]

__includes [
  "commands.nls"
  "data.nls"
  "files.nls"
  "g3notype.nls"
  "import-export.nls"
  "selection.nls"
  "sta7us.nls"
  "verification.nls"
]

breed [ plants plant ]
breed [ groups group ]
breed [ anima1s anima1 ]

turtles-own [ meta-id age energy.supply ]

anima1s-own [

  ; PHENOTYPE VARIABLES
  ; visible
  biological.sex
  life.history
  female.fertility
  group.identity
  is.dead
  alpha.signal
  beta.signal
  gamma.signal
  living.chance
  body.size
  body.shade

  identity.I
  identity.II
  carried.items

  ; hidden
  stomach.capacity
  mutation.chance
  sex.ratio
  litter.size
  conception.chance
  day.perception.angle
  night.perception.angle
  audio.perception.angle
  day.perception.range
  night.perception.range
  audio.perception.range
  vocal.range
  alpha.chance
  beta.chance
  gamma.chance
  birthing.chance
  weaning.chance
  infancy.chance
  juvenility.chance
  adulthood.chance
  senescency.chance
  x.magnitude
  y.magnitude

  chromosome.I
  chromosome.II
  decision.vectors
  memory.storage

  ; TRACKING VARIABLES
  mother-identity
  father-identity
  generation-number
  previous-group-id
  natal-group-id
  natal-group-size
  ticks-at-conception
  ticks-at-birth
  ticks-at-weaning
  ticks-at-sexual-maturity
  ticks-at-senescence
  ticks-at-death
  adult-living-chance
  adult-body-size
  adult-body-shade
  adult-mutation-chance
  adult-sex-ratio
  adult-litter-size
  adult-stomach-capacity
  adult-conception-chance
  adult-day-perception-angle
  adult-night-perception-angle
  adult-audio-perception-angle
  adult-day-perception-range
  adult-night-perception-range
  adult-audio-perception-range
  adult-vocal-range
  adult-alpha-chance
  adult-beta-chance
  adult-gamma-chance
  mother-initiated-birth
  mother-initiated-weaning
  distance-traveled
  foraging-gains
  foraging-losses
  helping-benefit
  helping-cost
  female-female-ingroup-helping
  male-male-ingroup-helping
  female-male-ingroup-helping
  matings-list
  conceptions-list
  group-transfers-list
  cells-occupied
  infanticide-list
  cause-of-death

]

groups-own [

  ; TRACKING VARIABLES
  my-creator
  distance-traveled
  total-birth-count
  total-energy-gained
  gestatee-female-energy-gained
  gestatee-male-energy-gained
  infant-female-energy-gained
  infant-male-energy-gained
  juvenile-female-energy-gained
  juvenile-male-energy-gained
  adult-cycling-energy-gained
  adult-pregnant-energy-gained
  adult-lactating-energy-gained
  adult-male-energy-gained
  senescent-female-energy-gained
  senescent-male-energy-gained

]

globals [
  model-version
  simulation-id
  sun-status
  deterioration-rate
  done-decisions
  selection-rate
]

;--------------------------------------------------------------------------------------------------------------------
;
;                      dP
;                      88
;  .d8888b. .d8888b. d8888P dP    dP 88d888b.
;  Y8ooooo. 88ooood8   88   88    88 88'  `88
;        88 88.  ...   88   88.  .88 88.  .88
;  `88888P' `88888P'   dP   `88888P' 88Y888P'
;                                    88
;                                    dP
;--------------------------------------------------------------------------------------------------------------------

to setup-parameters
  set model-version "1.0.2+"
  set done-decisions []
  set deterioration-rate -0.01
  set selection-rate 0.0001
end

to setup
  ;if ( simulation-id != 0 ) [ save-world ]
  clear-all
  reset-ticks
  setup-parameters
  setup-plants
  set simulation-id 0
  update-simulation
  import-population (word "../data/" path-to-experiment ifelse-value ( length path-to-experiment > 0 ) [ "/" ] [ "" ] population ".csv")
  import-genotype (word "../data/" path-to-experiment ifelse-value ( length path-to-experiment > 0 ) [ "/" ] [ "" ] genotype ".txt")
  clear-output
end

;--------------------------------------------------------------------------------------------------------------------
;
;  .d8888b. .d8888b.
;  88'  `88 88'  `88
;  88.  .88 88.  .88
;  `8888P88 `88888P'
;       .88
;   d8888P
;
;--------------------------------------------------------------------------------------------------------------------

to go

  ; ENVIRONMENT CONTRAINTS
  update-plants
  ask turtles [ set age age + 1 ]
  ask anima1s with [ life.history = "gestatee" and mother = nobody ] [ set is.dead true ]
  ask groups with [ count group-members = 0 ] [ die ]
  ask anima1s with [ length carried.items > 0 ] [ set carried.items remove nobody carried.items ]
  set done-decisions []
  ask anima1s [
    check-mortality
    deteriorate
    update-appearance ]

  ; AGENT ACTION
  ask anima1s [
    if ( not is.dead )
    [ make-decisions
      act ]]

  ; META-ANALYSIS
  if ( ticks > 0 and ceiling (ticks / 100) = (ticks / 100) and any? anima1s ) [
    print (word "Simulation " simulation-id " is now at " precision (ticks / plant-annual-cycle) 3 " years, "
      precision sum [energy.supply] of plants 3 " plant units, "
      precision mean [generation-number] of anima1s 3 " generations, and contains "
      count anima1s with [ not is.dead ] " living organisms.") ]

  if collect-data? [ collect-data ]
  if selection-on? [ artificial-selection ]

  tick
end

to make-decisions
  set decision.vectors []
  let environment no-turtles

  if ( sun-status = "DAY" or sun-status = "NIGHT" ) [
    set environment turtles with [ not is-group? self and not hidden? ] in-cone ( 5 * day.perception.range ) ( 300 * day.perception.angle )
    if (female.fertility = "pregnant") [ set environment turtles with [ not is-group? self and (not hidden? or member? self [my-offspring] of myself ) ] in-cone ( 5 * day.perception.range ) ( 300 * day.perception.angle ) ] ] ; mothers can see gestatees

;  if ( sun-status = "NIGHT" ) [
;    set environment turtles with [ not is-group? self and not hidden? ] in-cone ( 5 * day.perception.range ) ( 300 * night.perception.angle )
;    if (female.fertility = "pregnant") [ set environment turtles with [ not is-group? self and (not hidden? or member? self [my-offspring] of myself ) ] in-cone ( 5 * day.perception.range ) ( 300 * night.perception.angle ) ] ] ; mothers can see gestatees

  if (life.history = "gestatee" ) [ set environment turtles with [ [meta-id] of self = [meta-id] of myself or meta-id = [mother-identity] of myself ]] ; gestatees can only see themselves, and their mothers
  generate-decisions environment
end

to update-plants
  let season ( cos (( 360 / plant-annual-cycle ) * ticks))
  let daytime ( cos (( 360 / plant-daily-cycle ) * ticks))
  set sun-status ifelse-value ( daytime > 0 ) [ "DAY" ] [ "NIGHT" ]

  let energy-update get-updated-value ( plant-seasonality * ( 1 / 10000 ) * season ) ( 10 * daytime )

  ask plants [
    set energy.supply ( get-updated-value energy.supply energy-update )
    update-plant-color ]

  ask n-of 1 patches [ sprout-plants 1 [ initialize-plant ]]

  ; only a small proportion of plants change each TIMESTEP
  ask n-of (ceiling (0.01 * count patches)) patches [

    ; calculate the total penergy of surrounding PLANT neighbors
    let total-neighbor-penergy 0
    ask neighbors [
      if any? plants-here [
        ask plants-here [ set total-neighbor-penergy total-neighbor-penergy + energy.supply ]]]

    ; calculate a random value along a normal distribution of mean TOTAL-NEIGHBOR-PENERGY and standard deviation LIFE-SD
    let normal-value ( random-normal total-neighbor-penergy 1.0 )

    ; life occurs when the random value is within the MIN and MAX values
    ifelse ( normal-value >= plant-minimum-neighbors and normal-value <= plant-maximum-neighbors )
    [ if ( not any? plants-here ) [ sprout-plants 1 [ initialize-plant ]] ]
    [ if ( any? plants-here ) [ ask plants-here [ die ]]] ]

end

to initialize-plant
  set meta-id random 9999999
  set energy.supply 0
  set hidden? false
  set size 1
  set shape "square"
  update-plant-color
end

to update-plant-color
  set color scale-color green energy.supply 1.5 -0.25
end

to deteriorate
  ; the deterioration-rate is a negative number ( i.e. -0.001 )
  set living.chance get-updated-value living.chance deterioration-rate
  set body.size get-updated-value body.size deterioration-rate
  set body.shade get-updated-value body.shade deterioration-rate
end

to check-mortality
  if ( random-float 1.0 > living.chance ) [
    (ifelse

      ; SECOND DEATH: remove agent from the simulation
      ( is.dead = true ) [ die ]

      ; FIRST DEATH: set anima1 to is.dead true
      [ set is.dead true set ticks-at-death ticks ])
  ]
end

to update-appearance
  set size body.size
  set label " "
  set color (( [color] of one-of groups with [ meta-id = [group.identity] of myself ] ) + 5 - ( 10 ^ body.shade ))
  set shape get-shape
end

to update-energy [ update ]
  set energy.supply energy.supply + update
end

;--------------------------------------------------------------------------------------------------------------------
; REPORTERS
;--------------------------------------------------------------------------------------------------------------------

; GLOBAL REPORTERS

to-report get-time
  report ifelse-value ( ( cos (( 360 / plant-daily-cycle ) * ticks)) > 0 ) [ "DAY" ] [ "NIGHT" ]
end

to-report generate-alphabet-string [ number-length ]
  let lower-character-list [ "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" ]
  let output-string ""
  repeat number-length [ set output-string insert-item length output-string output-string one-of lower-character-list ]
  report output-string
end

to-report get-updated-value [ current-value update-value ]
  let report-value ifelse-value ( current-value < 0.00001 ) [ 0.00001 ] [ ifelse-value ( current-value > 0.99999 ) [ 0.99999 ] [ current-value ] ]
  ifelse update-value < 0
  [ set report-value ( report-value ^ (1 + abs update-value) ) ]
  [ set report-value ( report-value ^ (1 / ( 1 + update-value) )) ]
  report report-value
end

; GROUP REPORTERS

to-report group-size
  report count anima1s with [ group.identity = [meta-id] of myself ]
end

; ANIMA1 REPORTERS

to-report get-shape
  let base_shape ifelse-value ( biological.sex = "male" ) [ "triangle" ] [ "circle" ]
  let a_on ifelse-value alpha.signal [ "a" ] [ "" ]
  let b_on ifelse-value beta.signal [ "b" ] [ "" ]
  let c_on ifelse-value gamma.signal [ "c" ] [ "" ]
  report ( word base_shape a_on b_on c_on )
end

to-report relatedness-with [ target ]
  let matching 0
  let not-matching 0
  let i 0
  while [i < 10] [

    let allele-here-I item i identity.I
    let allele-here-II item i identity.II
    let target-allele-I item i [identity.I] of target
    let target-allele-II item i [identity.II] of target

    ifelse (( allele-here-I = target-allele-I ) or ( allele-here-I = target-allele-II ))
    [ set matching matching + 1 ] [ set not-matching not-matching + 1 ]

    ifelse (( allele-here-II = target-allele-I ) or ( allele-here-II = target-allele-II ))
    [ set matching matching + 1 ] [ set not-matching not-matching + 1 ]

    set i i + 1 ]

  report matching / ( matching + not-matching )
end

to-report my-offspring
  report ifelse-value ( biological.sex = "female" )
  [ anima1s with [ mother-identity = [meta-id] of myself ]]
  [ anima1s with [ father-identity = [meta-id] of myself ]]
end

to-report current-group
  report ifelse-value (any? groups with [ meta-id = [group.identity] of myself ] and group.identity != 0 )
  [ one-of groups with [ meta-id = [group.identity] of myself ] ]
  [ nobody ]
end

to-report previous-group
  report ifelse-value (any? groups with [ meta-id = [previous-group-id] of myself ])
  [ one-of groups with [ meta-id = [previous-group-id] of myself ]]
  [ nobody ]
end

to-report natal-group
  report ifelse-value (any? groups with [ meta-id = [natal-group-id] of myself ])
  [ one-of groups with [ meta-id = [natal-group-id] of myself ]]
  [ nobody ]
end

to-report mother
  report ifelse-value (any? turtles with [ meta-id = [mother-identity] of myself ])
  [ one-of turtles with [ meta-id = [mother-identity] of myself ]]
  [ nobody ]
end

to-report father
  report ifelse-value (any? turtles with [ meta-id = [father-identity] of myself ])
  [ one-of turtles with [ meta-id = [father-identity] of myself ]]
  [ nobody ]
end

;--------------------------------------------------------------------------------------------------------------------
;
;                      dP   oo
;                      88
;  .d8888b. .d8888b. d8888P dP .d8888b. 88d888b. .d8888b.
;  88'  `88 88'  `""   88   88 88'  `88 88'  `88 Y8ooooo.
;  88.  .88 88.  ...   88   88 88.  .88 88    88       88
;  `88888P8 `88888P'   dP   dP `88888P' dP    dP `88888P'
;
;--------------------------------------------------------------------------------------------------------------------

to act

  foreach decision.vectors [ vector ->

    let decision-id item 0 vector
    let raw-value item 4 vector
    let energy-cost abs raw-value

    if ( not member? decision-id done-decisions and energy.supply > energy-cost ) [
      let target item 2 vector
      let action item 3 vector
      update-energy ( - energy-cost )

      ifelse ifelse-value ( target = nobody ) [ false ] [ distance target < ( size / 2 + [size] of target / 2 ) ]
      [ do-action action target raw-value ]
      [ if (target != nobody ) [ move-toward target raw-value ]]]]

end

;----------------------------------------------------------------------------------------------
; DO-ACTION
;
; Operation to do a given ACTION directed to a given TARGET with a given VALUE.
;
; CALLER: anima1
; ACTION: the action code representing the action to perform.
; TARGET: the receiver of an inter-action.
; VALUE: the amount of energy allotted to the action.
;----------------------------------------------------------------------------------------------

to do-action [ action-code target value ]

  carefully [
    if action-code = "maintain-body" [ maintain-body value ]
    if action-code = "body-size" [ body-size value ]
    if action-code = "body-shade" [ body-shade value ]
    if action-code = "day-perception" [ day-perception value ]
    if action-code = "night-perception" [ night-perception value ]
    if action-code = "audio-perception" [ audio-perception value ]
    if action-code = "day-perception-angle" [ day-perception-angle value ]
    if action-code = "night-perception-angle" [ night-perception-angle value ]
    if action-code = "audio-perception-angle" [ audio-perception-angle value ]
    if action-code = "vocal-range" [ vocal-range value ]
    if action-code = "conception-chance" [ conception-chance value ]
    if action-code = "stomach-capacity" [ stomach-capacity value ]
    if action-code = "mutation-rate" [ mutation-rate value ]
    if action-code = "sex-ratio" [ sex-ratio value ]
    if action-code = "litter-size" [ litter-size value ]
    if action-code = "move-toward" [ move-toward target value ]
    if action-code = "turn-right" [ turn-right value ]
    if action-code = "turn-left" [ turn-left value ]
    if action-code = "go-forward" [ go-forward value ]
    if action-code = "signal-a-on" [ signal-a-on value ]
    if action-code = "signal-b-on" [ signal-b-on value ]
    if action-code = "signal-b-on" [ signal-c-on value ]
    if action-code = "check-infancy" [ check-infancy value ]
    if action-code = "check-birth" [ check-birth value ]
    if action-code = "check-juvenility" [ check-juvenility value ]
    if action-code = "check-weaning" [ check-weaning value ]
    if action-code = "check-adulthood" [ check-adulthood value ]
    if action-code = "check-senescence" [ check-senescence value ]
    if action-code = "supply-to" [ supply-to target value ]
    if action-code = "demand-from" [ demand-from target value ]
    if action-code = "eat" [ eat target value ]
    if action-code = "pick-up" [ pick-up target value ]
    if action-code = "squirm-from" [ squirm-from target value ]
    if action-code = "put-down" [ put-down target value ]
    if action-code = "cling-to" [ cling-to target value ]
    if action-code = "attack" [ attack target value ]
    if action-code = "help" [ help target value ]
    if action-code = "join-group-of" [ join-group-of target value ]
    if action-code = "leave-group" [ leave-group-of target value ]
    if action-code = "mate-with" [ mate-with target value ]

  ][  ]

end

;--------------------------------------------------------------------------------------------------------------------
; ACTIONS
;
; CALLER: the anima1 doing the action
; VALUE: the energy to be used to do the action
; TARGET: the receiver of the action ( INTER-ACTIONS have a target and INTRA-ACTIONS do not )
;
; The 'Info' tab contains a summary of possible actions.
;
;--------------------------------------------------------------------------------------------------------------------

to maintain-body [ value ] set living.chance get-updated-value living.chance value end

to body-size [ value ] set body.size get-updated-value body.size value end

to body-shade [ value ] set body.shade get-updated-value body.shade value end

to day-perception [ value ] set day.perception.range get-updated-value day.perception.range value end

to night-perception [ value ] set night.perception.range get-updated-value night.perception.range value end

to audio-perception [ value ] set audio.perception.range get-updated-value audio.perception.range value end

to day-perception-angle [ value ] set day.perception.angle get-updated-value day.perception.angle value end

to night-perception-angle [ value ] set night.perception.angle get-updated-value night.perception.angle value end

to audio-perception-angle [ value ] set audio.perception.angle get-updated-value audio.perception.angle value end

to vocal-range [ value ] set vocal.range get-updated-value vocal.range value end

to conception-chance [ value ] set conception.chance get-updated-value conception.chance value end

to mutation-rate [ value ] set mutation.chance get-updated-value mutation.chance value end

to sex-ratio [ value ] set sex.ratio get-updated-value sex.ratio value end

to litter-size [ value ] set litter.size get-updated-value litter.size value end

to stomach-capacity [ value ] set stomach.capacity get-updated-value stomach.capacity value end

;--------------------------------------------------------------------------------------------------------------------
; MOVEMENT
;--------------------------------------------------------------------------------------------------------------------

to move-toward [ target value ]
  if (target != nobody ) [
    let ycor-difference ([ycor] of target - [ycor] of self )
    let xcor-difference ([xcor] of target - [xcor] of self )
    let angle ifelse-value ( ycor-difference = 0 or xcor-difference = 0 ) [ random 360 ] [ atan ycor-difference xcor-difference ]
    if ( value < 0 ) [ set angle angle - 180 ]
    set x.magnitude x.magnitude + (abs value * sin angle)
    set y.magnitude y.magnitude + (abs value * cos angle)
  ]
end

to turn-right [ value ]
  ifelse ( value > 0 )
  [ right ( 360 * value ) ]
  [ left ( 360 * value ) ]
end

to turn-left [ value ]
  ifelse ( value > 0 )
  [ left ( 360 * value ) ]
  [ right ( 360 * value ) ]
end

to go-forward [ value ]

  if ( value < 0 ) [ right 180 ]

  let sum-weight size
  foreach carried.items [ object ->
    set sum-weight sum-weight + [size] of object ]
  set heading ( atan y.magnitude x.magnitude )
  set x.magnitude one-of [ 0.001 -0.001 ]
  set y.magnitude one-of [ 0.001 -0.001 ]
  let travel-distance ( size * (sqrt (( 2 * abs value ) / sum-weight )) )
  forward travel-distance
  foreach carried.items [ object ->
    if (object != nobody) [ ask object [ move-to myself ] ]]
  if collect-data? [
    set distance-traveled distance-traveled + travel-distance
    if not member? patch-here cells-occupied [ set cells-occupied lput patch-here cells-occupied ]]
end

;--------------------------------------------------------------------------------------------------------------------
; SIGNALING
;--------------------------------------------------------------------------------------------------------------------

to signal-a-on [ value ]
  set alpha.chance get-updated-value alpha.chance value
  ifelse ( random-float 1.0 < alpha.chance ) [
    set alpha.signal true
  ][
    set alpha.signal false
  ]
end

to signal-b-on [ value ]
  set beta.chance get-updated-value beta.chance value
  ifelse ( random-float 1.0 < beta.chance ) [
    set beta.signal true
  ][
    set beta.signal false
  ]
end

to signal-c-on [ value ]
  set gamma.chance get-updated-value gamma.chance value
  ifelse ( random-float 1.0 < gamma.chance ) [
    set gamma.signal true
  ][
    set gamma.signal false
  ]
end

;--------------------------------------------------------------------------------------------------------------------
; LIFE HISTORY
;--------------------------------------------------------------------------------------------------------------------

to check-infancy [ value ]
  set infancy.chance get-updated-value infancy.chance value
  if ( life.history = "gestatee" and random-float 1.0 < infancy.chance ) [
    set mother-initiated-birth false
    ifelse ( mother = nobody )
    [ set is.dead true ]
    [ ask mother [ give-birth ]]
  ]
end

to check-birth [ value ]
  set birthing.chance get-updated-value birthing.chance value
  if ( female.fertility = "pregnant" and random-float 1.0 < birthing.chance ) [
    give-birth
  ]
end

to give-birth
  if ( female.fertility = "pregnant" ) [
    set female.fertility "lactating"
    ask my-offspring with [ life.history = "gestatee" ] [ update-to-infant ]
    set birthing.chance 0
  ]
end

to update-to-infant
  set life.history "infant"
  set female.fertility " "
  set hidden? false
  set ticks-at-birth ticks
  if (collect-data?) [ ask current-group [ set total-birth-count total-birth-count + 1 ]]
end

to check-juvenility [ value ]
  set juvenility.chance get-updated-value juvenility.chance value
  if ( life.history = "infant" and random-float 1.0 < juvenility.chance ) [
    set mother-initiated-weaning false
    ifelse ( mother = nobody )
    [ update-to-juvenile ]
    [ ask mother [ wean-offspring ]]
  ]
end

to check-weaning [ value ]
  set weaning.chance get-updated-value weaning.chance value
  if ( female.fertility = "lactating" and random-float 1.0 < weaning.chance ) [
    wean-offspring
  ]
end

to wean-offspring
  if ( female.fertility = "lactating" ) [
    set female.fertility "cycling"
    ask my-offspring with [ life.history = "infant" ] [ update-to-juvenile ]
    set weaning.chance 0
  ]
end

to update-to-juvenile
  set life.history "juvenile"
  set female.fertility " "
  set ticks-at-weaning ticks
end

to check-adulthood [ value ]
  set adulthood.chance get-updated-value adulthood.chance value
  if ( life.history = "juvenile" and random-float 1.0 < adulthood.chance ) [
    update-to-adult
  ]
end

to update-to-adult
  set life.history "adult"
  set female.fertility ifelse-value ( biological.sex = "male" ) [ " " ] [ "cycling" ]
  set ticks-at-sexual-maturity ticks
  set adult-living-chance living.chance
  set adult-body-size body.size
  set adult-body-shade body.shade
  set adult-mutation-chance mutation.chance
  set adult-sex-ratio sex.ratio
  set adult-litter-size litter.size
  set adult-conception-chance conception.chance
  set adult-day-perception-angle day.perception.angle
  set adult-night-perception-angle night.perception.angle
  set adult-audio-perception-angle audio.perception.angle
  set adult-day-perception-range day.perception.range
  set adult-night-perception-range night.perception.range
  set adult-audio-perception-range audio.perception.range
  set adult-vocal-range vocal.range
  set adult-alpha-chance alpha.chance
  set adult-beta-chance beta.chance
  set adult-gamma-chance gamma.chance
end

to check-senescence [ value ]
  set senescency.chance get-updated-value senescency.chance value
  if ( life.history = "adult" and random-float 1.0 < senescency.chance ) [
    update-to-senescent
  ]
end

to update-to-senescent
  set life.history "senescent"
  set female.fertility " "
  ask my-offspring with [ life.history = "gestatee" ] [ update-to-infant ]
  ask my-offspring with [ life.history = "infant" ] [ update-to-juvenile ]
  set ticks-at-senescence ticks
end

;--------------------------------------------------------------------------------------------------------------------
; ENERGY
;--------------------------------------------------------------------------------------------------------------------

to supply-to [ target value ] ;;
  if ( target != self and is-anima1? target and female.fertility = "lactating" or female.fertility = "pregnant" ) [
    let demand-decisions get-decisions target self "demand-from"
    if ( not empty? demand-decisions ) [
      let one-demand one-of demand-decisions
      ask target [
        receive-from myself value
        decisions-done (list one-demand)
      ]
    ]
  ]
end

to demand-from [ target value ] ;;
  if ( target != self and is-anima1? target ) [
    let supply-decisions get-decisions target self "supply-to"
    if ( not empty? supply-decisions ) [
      let one-supply one-of supply-decisions
      receive-from myself value
      ask target [ decisions-done (list one-supply) ]
  ]]
end

to receive-from [ target value ] ;; value here should be lesser of supplier or demander value
  let energy-wanted 10 ^ value
  let energy-received ifelse-value ( energy-wanted < [ energy.supply ] of target ) [ energy-wanted ] [ [ energy.supply ] of target ]
  update-energy energy-received
  ask target [ set energy.supply ( energy.supply - energy-received ) ]
end

to eat [ target value ]
  let energy-wanted get-updated-value stomach.capacity value
  let energy-eaten ifelse-value ( energy-wanted < [energy.supply] of target) [energy-wanted] [[energy.supply] of target]
  update-energy energy-eaten
  ask target [ update-energy ( - energy-eaten ) ]

  if (collect-data?) [

    set foraging-gains foraging-gains + energy-eaten
    set foraging-losses foraging-losses + value

    ; group is tracking member foraging performance
    ask current-group [
      set total-energy-gained total-energy-gained + energy-eaten
      if ( [life.history] of myself = "gestatee" and [biological.sex] of myself = "female" ) [ set gestatee-female-energy-gained gestatee-female-energy-gained + energy-eaten ]
      if ( [life.history] of myself = "gestatee" and [biological.sex] of myself = "male" ) [ set gestatee-male-energy-gained gestatee-male-energy-gained + energy-eaten ]
      if ( [life.history] of myself = "infant" and [biological.sex] of myself = "female" ) [ set infant-female-energy-gained infant-female-energy-gained + energy-eaten ]
      if ( [life.history] of myself = "infant" and [biological.sex] of myself = "male" ) [ set infant-male-energy-gained infant-male-energy-gained + energy-eaten ]
      if ( [life.history] of myself = "juvenile" and [biological.sex] of myself = "female" ) [ set juvenile-female-energy-gained juvenile-female-energy-gained + energy-eaten ]
      if ( [life.history] of myself = "juvenile" and [biological.sex] of myself = "male" ) [ set juvenile-male-energy-gained juvenile-male-energy-gained + energy-eaten ]
      if ( [life.history] of myself = "adult" and [biological.sex] of myself = "female" and [female.fertility] of myself = "cycling" ) [ set adult-cycling-energy-gained adult-cycling-energy-gained + energy-eaten ]
      if ( [life.history] of myself = "adult" and [biological.sex] of myself = "female" and [female.fertility] of myself = "pregnant" ) [ set adult-pregnant-energy-gained adult-pregnant-energy-gained + energy-eaten ]
      if ( [life.history] of myself = "adult" and [biological.sex] of myself = "female" and [female.fertility] of myself = "lactating" ) [ set adult-lactating-energy-gained adult-lactating-energy-gained + energy-eaten ]
      if ( [life.history] of myself = "adult" and [biological.sex] of myself = "male" ) [ set adult-male-energy-gained adult-male-energy-gained + energy-eaten ]
      if ( [life.history] of myself = "senescent" and [biological.sex] of myself = "female" ) [ set senescent-female-energy-gained senescent-female-energy-gained + energy-eaten ]
      if ( [life.history] of myself = "senescent" and [biological.sex] of myself = "male" ) [ set senescent-male-energy-gained senescent-male-energy-gained + energy-eaten ]
    ]
  ]

end

;--------------------------------------------------------------------------------------------------------------------
; INTERACTIONS
;--------------------------------------------------------------------------------------------------------------------

to join-group-of [ target value ] ;;
  if ( is-anima1? target and random-float 1.0 < value ) [
    set previous-group-id group.identity
    set group.identity [group.identity] of target
    if collect-data? [ set group-transfers-list lput [meta-id] of current-group group-transfers-list ]
  ]
end

to leave-group-of [ target value ] ;;
  if ( random-float 1.0 < abs value ) [
    set previous-group-id group.identity
    hatch-groups 1 [
      initialize-group
      set my-creator [meta-id] of myself
      ask myself [ set group.identity [meta-id] of myself ]]
    ask previous-group [ if not any? group-members [ die ]]
  ]
end

to initialize-group
  set hidden? true
  set age 0
  set color one-of base-colors
  set meta-id random 99999999
  move-to one-of patches
end

to pick-up [ target value ] ;;

;  if ( target != self and is-anima1? target ) [
;
;    let squirm-decisions get-decisions target self "squirm-from"
;    let squirm-cost get-decisions-cost squirm-decisions
;
;    let pickup-decisions get-decisions self target "pick-up"
;    let pickup-cost get-decisions-cost pickup-decisions
;
;    ifelse ( squirm-cost > pickup-cost )
;    [ if ( member? target carried.items) [set carried.items remove target carried.items ]]
;    [ if ( not member? target carried.items) [set carried.items lput target carried.items ]]
;
;    ask target [ decisions-done squirm-decisions ]
;    decisions-done pickup-decisions
;
;  ]
end

to squirm-from [ target value ] ;;

;  if ( target != self and is-anima1? target ) [
;
;    let pickup-decisions get-decisions target self "pick-up"
;    let pickup-cost get-decisions-cost pickup-decisions
;
;    let squirm-decisions get-decisions self target "squirm-from"
;    let squirm-cost get-decisions-cost squirm-decisions
;
;    ifelse ( squirm-cost > pickup-cost )
;    [ ask target [ set carried.items remove myself carried.items ]]
;    [ ask target [ set carried.items lput target carried.items ]]
;
;    decisions-done squirm-decisions
;    ask target [ decisions-done pickup-decisions ]
;
;  ]
end

to put-down [ target value ] ;;

  if ( target != self and is-anima1? target ) [

    let cling-decisions get-decisions target self "cling-to"
    let cling-cost get-decisions-cost cling-decisions
    let putdown-decisions get-decisions self target "put-down"
    let putdown-cost get-decisions-cost putdown-decisions

    ifelse ( putdown-cost >= cling-cost )
    [ if ( member? target carried.items) [ set carried.items remove target carried.items ]]
    [ if ( not member? target carried.items ) [ set carried.items lput target carried.items ]]

    decisions-done putdown-decisions
    ask target [ decisions-done cling-decisions ]

  ]
end

to cling-to [ target value ] ;; squirm from

;  if ( target != self and is-anima1? target ) [
;
;    let putdown-decisions get-decisions target self "put-down"
;    let putdown-cost get-decisions-cost putdown-decisions
;
;    let cling-decisions get-decisions self target "cling-to"
;    let cling-cost get-decisions-cost cling-decisions
;
;    ifelse ( putdown-cost > cling-cost )
;    [ ask target [ set carried.items remove myself carried.items ]]
;    [ ask target [ set carried.items lput target carried.items ]]
;
;    decisions-done cling-decisions
;    ask target [ decisions-done putdown-decisions ]
;
;  ]
end

to attack [ target value ] ;;

  if ( is-anima1? target ) [
    let attack-decisions get-decisions target self "attack"
    ifelse ( not empty? attack-decisions ) [
      let one-attack one-of attack-decisions
      let target-attack-cost get-decisions-cost (list one-attack)

      ifelse ( random-float 1.0 < winning-likelihood-with target )
      [ ask target [ set living.chance get-updated-value living.chance ( - value ) ]]
      [ set living.chance get-updated-value living.chance ( - value ) ]

      ask target [ decisions-done (list one-attack) ]

    ][
      ask target [ set living.chance get-updated-value living.chance ( - value ) ]
    ]
  ]
end

to-report winning-likelihood-with [ target ]
  ifelse target = nobody
  [ report 1 ]
  [ report ( size / ( size + [size] of target + 0.0000000001)) ]
end

to help [ target value ]  ;; attack
  ifelse ( value < 0 ) [
    attack target abs value
  ] [

    if ( is-anima1? target ) [

      if (collect-data? and target != self ) [ ; Hamilton's Rule
        let r relatedness-with target
        ;print (word " me: " self " living.chance: " living.chance "  target: " target "   relatedness " relatedness-with target " ([living.chance] of target) " ([living.chance] of target)  "   value:"  value " update  " (get-updated-value [living.chance] of target ( value )) "  total: " (( get-updated-value [living.chance] of target ( value ) ) - [living.chance] of target ))
        let B ( get-updated-value [living.chance] of target ( value ) ) - [living.chance] of target ; how much target benefitted
        let C living.chance - ( get-updated-value living.chance value ) ; how much you *could have* benefitted by investing in yourself
        set helping-benefit helping-benefit + r * B ; benefit to yourself based on relatedness to target
        set helping-cost helping-cost + C ; cost of not helping yourself

        if ( biological.sex = "female" and [biological.sex] of target = "female" and group.identity = [group.identity] of target ) [
          set female-female-ingroup-helping female-female-ingroup-helping + value
        ]
        if ( biological.sex = "male" and [biological.sex] of target = "male" and group.identity = [group.identity] of target ) [
          set male-male-ingroup-helping male-male-ingroup-helping + value
        ]
        if ( (( biological.sex = "female" and [biological.sex] of target = "male" ) or ( biological.sex = "male" and [biological.sex] of target = "female" )) and group.identity = [group.identity] of target ) [
          set female-male-ingroup-helping female-male-ingroup-helping + value
        ]
      ]

      ask target [ set living.chance get-updated-value living.chance ( value ) ]
    ]
  ]

end

to mate-with [ target value ] ;;
  let mate-decisions get-decisions target self "mate-with"
  if ( not empty? mate-decisions ) [
    let one-mate one-of mate-decisions
    ifelse ( biological.sex = "female" )
    [ conceive-with target ]
    [ ask target [ conceive-with myself ]]
    ask target [ decisions-done (list one-mate) ]

    ; DATA
    if collect-data? [
      set matings-list lput [meta-id] of target matings-list
      ask target [ set matings-list lput [meta-id] of myself matings-list ]]]
end

to conceive-with [ target ] ; FEMALE PROCEDURE
  if biological.sex = "female" and female.fertility = "cycling" and life.history = "adult" and [life.history] of target = "adult" and [biological.sex] of target = "male" [
    if random-float 1.0 < (mean (list conception.chance [conception.chance] of target)) [
      set label "!"
      let preferred-litter 10 ^ ( mean (list litter.size [litter.size] of target))
      let floor-litter floor preferred-litter
      let percent-litter preferred-litter - floor-litter
      let my-litter-size ifelse-value ( random-float 1.0 < percent-litter ) [ floor-litter + 1 ] [ floor-litter ]
      hatch-anima1s my-litter-size [ initialize-from-parents myself target ]
      set female.fertility "pregnant"

      ; DATA
      if collect-data? [
        set conceptions-list lput [meta-id] of target conceptions-list
        ask target [ set conceptions-list lput [meta-id] of myself conceptions-list ]]]]
end

to initialize-from-parents [ m f ]
  set meta-id random 9999999
  set hidden? true
  set is.dead false
  ask m [ pick-up myself 1.0 ]
  set biological.sex ifelse-value ( random-float 1.0 < 0.5 ) ["male"] ["female"] ;mean (list [sex.ratio] of m [sex.ratio] of f) ) ["male"] ["female"]
  set shape ifelse-value ( biological.sex = "female" ) ["circle"] ["triangle"]
  set generation-number [generation-number] of m + 1
  setup-chromosomes-from m f
  set group.identity [group.identity] of m
  set natal-group-id group.identity
  set mother-identity [meta-id] of m
  set father-identity [meta-id] of f
  set previous-group-id 0
  set female.fertility " "
  set life.history "gestatee"
  set label-color black
  set age 0
  set energy.supply 1
  set body.size 0.01
  set body.shade 0
  set day.perception.range 0
  set night.perception.range 0
  set audio.perception.range 0
  set mutation.chance 0
  set sex.ratio 0.5
  set litter.size 0
  set stomach.capacity 0
  set conception.chance 0
  set birthing.chance 0
  set weaning.chance 0
  set infancy.chance 0
  set juvenility.chance 0
  set adulthood.chance 0
  set senescency.chance 0
  set living.chance 1
  set alpha.chance 0
  set beta.chance 0
  set gamma.chance 0
  set alpha.signal false
  set beta.signal false
  set gamma.signal false
  set day.perception.range 0
  set night.perception.range 0
  set audio.perception.range 0
  set day.perception.angle 0
  set night.perception.angle 0
  set audio.perception.angle 0
  set vocal.range 0
  set ticks-at-conception ticks
  set ticks-at-birth 0
  set ticks-at-weaning 0
  set ticks-at-sexual-maturity 0
  set ticks-at-senescence 0
  set ticks-at-death 0
  set adult-living-chance 0
  set adult-body-size 0
  set adult-body-shade 0
  set adult-mutation-chance 0
  set adult-sex-ratio 0
  set adult-litter-size 0
  set adult-stomach-capacity 0
  set adult-conception-chance 0
  set adult-day-perception-angle 0
  set adult-night-perception-angle 0
  set adult-audio-perception-angle 0
  set adult-day-perception-range 0
  set adult-night-perception-range 0
  set adult-audio-perception-range 0
  set adult-vocal-range 0
  set adult-alpha-chance 0
  set adult-beta-chance 0
  set adult-gamma-chance 0
  set mother-initiated-birth true
  set mother-initiated-weaning true
  set distance-traveled 0
  set foraging-gains 0
  set foraging-losses 0
  set helping-benefit 0
  set helping-cost 0
  set carried.items []
  set group-transfers-list []
  set matings-list []
  set conceptions-list []
  set cells-occupied []
  set infanticide-list []
  set decision.vectors []
  set x.magnitude one-of [ 0.001 -0.001 ]
  set y.magnitude one-of [ 0.001 -0.001 ]
  set natal-group-size [group-size] of current-group
  receive-from mother 1.0 ; delete
end

to setup-chromosomes-from [m f]

  ; CHROMOSOME I
  set chromosome.I []
  let i 0
  let chrome-i-length max (list length [chromosome.I] of m length [chromosome.II] of m)
  while [i < chrome-i-length ] [
    ifelse random-float 1.0 < 0.5 ; half the time, alleles will switch places
    [ ifelse ( i < length [chromosome.I] of m ) [ set chromosome.I lput ( item i [chromosome.I] of m) chromosome.I] [ set chromosome.I lput ( item i [chromosome.II] of m) chromosome.I ]  ]
    [ ifelse ( i < length [chromosome.II] of m ) [ set chromosome.I lput ( item i [chromosome.II] of m) chromosome.I] [ set chromosome.I lput ( item i [chromosome.I] of m) chromosome.I ]  ]
    set i i + 1 ]

  ; CHROMOSOME II
  set chromosome.II []
  set i 0
  let chrome-ii-length max (list length [chromosome.I] of f length [chromosome.II] of f)
  while [i < chrome-ii-length ] [
    ifelse random-float 1.0 < 0.5 ; half the time, alleles will switch places
    [ ifelse ( i < length [chromosome.I] of f ) [ set chromosome.II lput (item i [chromosome.I] of f) chromosome.II] [ set chromosome.II lput (item i [chromosome.II] of f) chromosome.II ]  ]
    [ ifelse ( i < length [chromosome.II] of f ) [ set chromosome.II lput (item i [chromosome.II] of f) chromosome.II] [ set chromosome.II lput (item i [chromosome.I] of f) chromosome.II ]  ]
    set i i + 1 ]

  if random-float 1.0 < 0.5 [
    let chromosome-holder chromosome.I
    set chromosome.I chromosome.II
    set chromosome.II chromosome-holder ]

  ; IDENTITY CHROMOSOMES
  set identity.I one-of (list [identity.I] of m [identity.II] of m )
  set identity.II one-of (list [identity.I] of f [identity.II] of f )
  set i 0
  while [i < length identity.I ] [ if random-float 1.0 < 0.5 ; half the time, alleles will switch places
    [ let itemIII item i identity.I
      let itemIV item i identity.II
      set identity.I replace-item i identity.I itemIV
      set identity.II replace-item i identity.II itemIII ]
    set i i + 1 ]

  let rate-of-mutation 0.15 ; mean (list [mutation.chance] of m [mutation.chance] of f)
  mutate-chromosomes rate-of-mutation
end

to mutate-chromosomes [ rate-of-mutation ]
  set chromosome.I mutate-chromosome chromosome.I rate-of-mutation
  set chromosome.II mutate-chromosome chromosome.II rate-of-mutation
end

to-report mutate-chromosome [ chromosome-list _mutation.chance ]
  let updated-chromosome []

  foreach chromosome-list [ allele ->
    let updated-alleles (list allele)

    if ( random-float 1.0 < _mutation.chance ) [

      let choice random 6

      (ifelse

        ( first allele = true and choice = 0 ) [ ; delete allele
          set updated-alleles [] ]

        ( first allele = true and choice = 1 ) [] ; delete codon

        ( first allele = true and choice = 2 ) [ ; duplicate allele
          set updated-alleles (list allele allele) ]

        ( first allele = true and choice = 3 ) [] ; duplicate codon

        ( choice = 4 ) [ ; update number
          foreach updated-alleles [ x ->
            let allele-position position x updated-alleles
            foreach x [ codon ->
              if ( is-number? codon ) [
                let codon-position position codon x
                let updated-allele replace-item codon-position x ( precision (one-of [ -1 1 ] * get-updated-value codon ( 0.1 * one-of [ -1 1 ] )) 6 )
                set updated-alleles replace-item allele-position updated-alleles updated-allele  ]]]]

        ( first allele = true and choice = 5 ) [ ; update codon
          foreach updated-alleles [ x ->
            let allele-position position x updated-alleles
            foreach x [ codon ->
              if ( is-string? codon ) [
                let codon-position position codon x
                let updated-allele replace-item codon-position x get-mutation
                set updated-alleles replace-item allele-position updated-alleles updated-allele  ]]]]

        ( choice = 6 ) [ ; new allele
          set updated-alleles (list (list get-mutation )) ]

        [])
    ]

    foreach updated-alleles [ allele-update ->
      set updated-chromosome ifelse-value ( allele-update != [] ) [ lput allele-update updated-chromosome ] [ updated-chromosome ]]

  ]

  report updated-chromosome
end
@#$#@#$#@
GRAPHICS-WINDOW
6
85
710
790
-1
-1
6.96
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
99
0
99
1
1
1
timesteps
30.0

BUTTON
270
10
336
79
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
341
10
408
79
go
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

INPUTBOX
7
10
263
79
path-to-experiment
NIL
1
0
String

BUTTON
414
10
489
79
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

PLOT
719
133
966
309
plot
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" "if any? anima1s [\n\nif (useful-commands = \"age-histogram\") [\n  clear-plot\n  set-plot-x-range 0 max [age] of anima1s with [ is.dead = false ] + 1000\n  set-plot-y-range 0 10 ]\n  \nif (useful-commands = \"lotka-volterra\") [\n  let plts ( plant-quality * count plants / 100 )\n  let anls count anima1s\n  let max-value max (list plts anls )\n  set-plot-x-range 0 10\n  set-plot-y-range 0 ( max-value + 10 ) ]\n ]\n  \n"
PENS
"age" 1000.0 1 -16777216 true "" "if (useful-commands = \"age-histogram\") [ histogram [age] of anima1s with [ is.dead = false] ]"
"plants" 1.0 0 -13840069 true "" "if (useful-commands != \"age-histogram\") [ plot ((plant-quality * (sum [energy.supply] of plants ))/ 100 ) ]"
"organisms" 1.0 0 -6459832 true "" "if (useful-commands != \"age-histogram\") [ plot ( count anima1s ) ]"

INPUTBOX
719
10
966
127
documentation-notes
NIL
1
0
String

MONITOR
18
102
95
147
simulation
(word simulation-id )
17
1
11

BUTTON
637
10
711
80
save
save
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
719
432
1221
790
11

SLIDER
973
204
1216
237
plant-minimum-neighbors
plant-minimum-neighbors
0
8
1.0
.1
1
NIL
HORIZONTAL

SLIDER
973
240
1216
273
plant-maximum-neighbors
plant-maximum-neighbors
0
8
8.0
.1
1
NIL
HORIZONTAL

MONITOR
538
98
613
143
season
( cos (( 360 / plant-annual-cycle ) * ticks))
3
1
11

SLIDER
973
131
1216
164
plant-seasonality
plant-seasonality
0
100
50.0
5
1
%
HORIZONTAL

SLIDER
973
59
1216
92
plant-annual-cycle
plant-annual-cycle
10
10000
1000.0
10
1
ticks
HORIZONTAL

SLIDER
973
95
1216
128
plant-daily-cycle
plant-daily-cycle
1
100
10.0
1
1
ticks
HORIZONTAL

MONITOR
619
98
698
143
time
sun-status
17
1
11

INPUTBOX
973
279
1096
348
population
population
1
0
String

INPUTBOX
973
353
1096
422
genotype
NIL
1
0
String

BUTTON
1101
279
1156
348
⟳
reset-population
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
1161
279
1216
312
⤒
export-population
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
1161
315
1216
348
⤓
seed-population
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
719
377
907
422
useful-commands
useful-commands
"help-me" "--------" "lotka-volterra" "age-histogram" "metafile-report" "verify-code" "check-runtime" "simulation-report" "clear-plants" "setup-plants" "clear-population" "view-genotype" "view-decisions" "add-allele" "delete-allele" "population-report"
8

BUTTON
911
377
966
422
▷
command
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
1101
353
1156
422
⟳
reset-genotype
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
1161
389
1216
422
⤓
seed-genotype
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
1161
353
1216
386
⤒
export-genotype
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
973
168
1216
201
plant-quality
plant-quality
.01
1
1.0
.01
1
NIL
HORIZONTAL

SWITCH
495
10
631
43
collect-data?
collect-data?
0
1
-1000

SWITCH
495
46
631
79
selection-on?
selection-on?
0
1
-1000

INPUTBOX
719
313
966
373
command-input
9295044
1
0
String (commands)

CHOOSER
973
10
1216
55
model-structure
model-structure
"baseline" "aspatial" "reaper" "sower" "freelunch" "idealform" "noevolution" "nomutation"
0

@#$#@#$#@
# B3GET 1.0.2 INFORMATION

Compatible with NetLogo 6.1.1

## WHAT IS IT?

B3GET is designed to test hypotheses in biology by simulating populations of virtual organisms evolving over generations, whose evolutionary outcomes reflect the selection pressures of their environment. Users input populuation files to seed the initial population and run simulations to evolve these populations - and their genotypes - over generations. Behavioral strategies that are beneficial for their environmental context are expected to emerge.

B3GET helps answer fundamental questions in evolutionary biology by offering users a virtual field site to precisely track the evolution of organismal populations. Researchers can use B3GET to: (1) investigate how populations vary in response to ecological pressures; (2) trace evolutionary histories over indefinite time scales and generations; (3) track an individual for every moment of their life from conception to post-mortem decay; and (4) create virtual analogues of living species, including primates like baboons and chimpanzees, to answer species-specific questions. Users are able to save, edit, and import population and genotype files, offering an array of possibilities for creating controlled biological experiments.

## HOW IT WORKS

B3GET simulates several factors considered important in biology, including life history trade-offs, investment in body size, variation in aggression, sperm competition, infanticide, and competition over access to food and mates. B3GET calculates each agent’s decision-vectors from its diploid chromosomes and current environmental context. These decision-vectors dictate movement, body growth, desire to mate and eat, and other agent actions. Chromosomes are modified during recombination and mutation, resulting in behavioral strategies that evolve over generations.

## HOW TO USE IT

### STARTING UP

B3GET should come with the following file and [folder] structure. These extensions are written to maintain a modular code structure, which promotes: (1) ease in testing and maintenance, (2) ensuring that future innovations will not interfere with other parts of the program.

> [B3GET]
--- [code]
------ B3GET.nlogo
------ commands.nls
------ data.nls
------ files.nls
------ sta7us.nls
------ import-export.nls
------ selection.nls
------ verification.nls
--- [data]
------ genotype.txt
------ population.csv
--- [docs]
------ B3GET-ODD-protocol.pdf

B3GET starts with PATH-TO-EXPERIMENT set to [../data/], which means that any data or files generated during simulation will be saved in the [data] folder. Initially, POPULATION is set to [population] and GENOTYPE is set to [genotype], which are files included during download. With these settings, you can just click SETUP and GO to start your first simulation! Please refer to the 'control' descriptions below to perform more complex tasks.

### PRIMARY CONTROLS

PATH-TO-EXPERIMENT: the path indicating where to store data for the current experiment.
SETUP: returns the model to the starting state.
GO: runs the simulation.
GO ONCE: runs exactly one tick, or timestep, of the simulation.
COLLECT-DATA?: 'ON' collects data on the animals (see "data" extension).
SELECTION-ON?: 'ON' artificially culls the animal population (see "selection" extension).
SAVE: records the current simulation state and documentation-notes in an external file.
DOCUMENTATION-NOTES: write notes to yourself here and click 'save' to save them.

### VIEW INFORMATION

The main view screen allows us to visually see emergent behaviors. 

SIMULATION: the unique identification code of the current simulation.
SEASON: cycles between 1.0 (summer) and -1.0 (winter) according to the plant-annual-cycle.
TIME: varies from DAY to NIGHT according to plant-daily-cycle.

### ENVIRONMENTAL CONTROLS

Plants and plant life and growth are modeled after Conway's Game of Life, a cellular automaton model that contained rules for when a cell could be 'alive' or 'dead', thus simulating a living ecosystem. In B3GET, 'alive' cells contain plant agents, depicated as a green squares in the model.

PLANT-ANNUAL-CYCLE: the length of a year in timesteps.
PLANT-SEASONALITY: the degree of difference in plant abundance from summer to winter.
PLANT-MINIMUM-NEIGHBORS: preferred minimum number of plant neighbors for each plant.
PLANT-MAXIMUM-NEIGHBORS: preferred maximum number of neighbors for each plant.
PLANT-DAILY-CYCLE: the length of a day in timesteps.
PLANT-QUALITY: the maximum energy that any plant can contain.

### IMPORT & EXPORT CONTROLS

B3GET includes the ability to import and export files related to populations and organism genotypes. This allows the user access and control over population composition and which behaviors are included in their genotype files. By default, POPULATION is set to 'population' and GENOTYPE is set to 'genotype'.

#### POPULATION FILES

To first begin a simulation, you must import a 'seed' population, the ancestral population from which your new population will evolve. Upon clicking SETUP, B3GET will automatically import the population name indicated in the POPULATION input box, if it exists. POPULATION import and export is accessed by the following buttons:

⟳: generates a random POPULATION name.
⤒: saves all organisms to the file matching the POPULATION name.
⤓: imports all organisms from the file matching the POPULATION name.

#### GENOTYPE FILES

Each organism imported from a population file includes its own genotype, but you can override this and give each member of the population a new genotype. As with the population files, B3GET will automatically import the genotype name indicated in the GENOTYPE input box, if it exists. The GENOTYPE input is accessed by the following buttons:

⟳: creates a GENOTYPE name after an arbitrarily selected organism.
⤒: saves the genotype of organism matching the GENOTYPE name to same-named file.
⤓: gives all organisms the genotype file imported from the GENOTYPE name.

### MONITORING CONTROLS

While you can use the BehaviorSpace functionality to 'grow' many populations at once, sometimes it is useful to visually observe and influence a single simulation. The mintoring controls allow for some direct observation and influence.

USEFUL-COMMANDS: select from a list of premade functions.
▷: hit this button to run the command selected by USEFUL-COMMANDS
PLOT: outputs a range of plots, which are accessed in useful-commands.
OUPUT: many controls output some text, which shows up in this window.

### EXTENSIONS

Detailed information on what each extension does can be found in those files. Here is a brief list of all extensions and briefly what they do:

COMMANDS: controls the extra commands to use during experimentation.
DATA: controls how data is collected during simulation runs.
FILES: controls how files are created and data is stored within them.
IMPORT-EXPORT: controls for importing and exporting populations of agents.
SELECTION: controls for artificial selection of agents during simulation.
STA7US: a simple genotype file reader.
G3NOTYPE: a more complex genotype file reader.
VERIFICATION: the verification code for this model.

### NEW EXPERIMENT

If you want to start a new experiment and store information in a separate place, simply create a new folder in [data] and make sure to update the PATH-TO-EXPERIMENT with this new folder path. You must add a poulation file to this new folder (the default file popu1ation would be a fine choice), and update the POPULATION input to this population file name. You can also add a genotype file and update the GENOTYPE input, but this is not neccessary because the population file also contains the genotypes for every agent.

## THINGS TO NOTICE

This model is designed to explore the behavior space of virtual organisms who are subject to the same kinds of ecological constraints we observe in nature. In your simulation, what behaviors emerge in your population and why might these behaviors be part of a successful strategy? Additionally, you can observe population level dynamics like the lokta-volterra cycles of the organism and plant populations.

## THINGS TO TRY

The file system of B3GET allows the user to directly modify genotype and population files to many different configurations. The number of alleles of each chromosome can also be modified but beware that lengthy genotypes may cause lag in runtime. Additionally, you can explore how the environmental controls influence behavioral strategies over generations.

## HOW TO CITE

Cite this model:

Crouse, Kristin (2019, November 18). “B3GET” (Version 1.0.2). CoMSES Computational Model Library. Retrieved from: https://www.comses.net/codebases/6b10f629-7958-4b31-b489-d51c17d0f5b8/releases/1.0.2/

Peer-reviewed paper:

Crouse, K. N., Miller, C. M., & Wilson, M. L. (2019). New approaches to modeling primate socioecology: Does small female group size BEGET loyal males?. Journal of human evolution, 137, 102671.

## COPYRIGHT AND LICENSE

© 2019 K N Crouse

The model may be freely used, modified and redistributed provided this copyright is included and the resulting models are not used for profit.

Contact K N Crouse at crou0048@umn.edu if you have questions about its use.
.

# REFERENCE GUIDE

## Actions

Agent actions are either intra-actions, which can only modify the state of the agent performing the action, or inter-actions, which can modify the state of other 'target' agents adjacent to the performing agent.

### INTRA-ACTIONS

MAINTENANCE: agents must maintain their body to continue living.
GROWTH: agents are conceived with a very small body size and must grow to get bigger.
APPEARANCE: agents can alter their color, shade and which colored stripes are visible.
PERCEPTION: agents must invest in their perceptive abilities in order to 'see'.
VOCALS: agents must invest in their vocal range to project auditory communication.
REPRODUCTION: agents must invest in reproduction in order to successfully conceive.
OFFSPRING: agents can influence the litter size and biological sex of their offspring.
MOVEMENT: agents can invest energy in turning and moving forward.
LIFE HISTORY: agents control the timing of development and fertilty.

### INTER-ACTIONS

NURSING: mother agents can supply energy to their offspring.
EATING: agents can eat both plants and other dead agents.
CARRYING: agents can pick up and put down other agents, and in turn agents can cling to or squirm away from other agents.
FIGHTING: agents can attack each other, which increases the receiver's mortality risk.
HELPING: agents can also help each other, decreasing the receiver's mortality risk.
TRANSFERS: agents can join or leave the group of another individual.
MATING: agents mate with each other to conceive offspring, gestated by the mother.

## Genotypes

The actions listed above are athe range of possible actions that an agent can take. However, whether an agent performs these actions, how much effort they put into doing so, and who they target is up to their genotype. An indefinite number of genotype file configurations are possible, as long as they include the following: (1) each row represents one allele, (2) these alleles represent self-contained procedures that generate decision-vectors from considering the environment as input, and (3) each row contains a list of codons that can be altered during recombination and mutation. This version of B3GET comes with two genotype file extensions: sta7us and g3notype (beta version). Specific information about each file type can be found within those extension files.

## Phenotypes

When agents perform actions, for the most part this results in changes in the state variables of themselves or others. These states can be thought of as an organism's phenotype, the set of virtual 'organs' which emerges from a combination of an organism's genotype and its life experiences interacting with its environment.

### VISIBLE 'ORGANS'

These organs are visible to others.

SIZE: the overall body size or mass of an individual.
COLOR: color represents group identity and shade correlates with age.
SEX: either "male" or "female".
LIFE HISTORY: "gestatee", "infant", "juvenile", "adult", or "senescent".
FERILITY: is "cycling", "pregnant" or "lactating" for adult females.
HEALTH: liklihood of an agent living to the next timestep.
ENERGY: agents can see the energy supply of another agent.
ALIVE: agents can see whether an agent is dead or alive.
SIGNALS: agents can display phenotypic signals, which appear as colored stripes.
GROUP: agents can tell whether or not they have the same group identity.
KINSHIP: check the chromosomes to deteremine degree of relatedness to that individual.
INVENTORY: a list of other agents that are being carried.

### LIFE HISTORY 'ORGAN' CONSTRAINTS

Like real organs, virtual organisms also have constraints to their biology that are tied to their life history and reproductive states:

GESTATEE: hidden from everyone but mother, and capable of nursing
INFANT: visible to everyone, unable to eat but capable of nursing
JUVENILE: able to eat, but not able to conceive offspring
ADULT: able to conceive offspring
CYCLING: females are able to conceive
PREGNANT: females enter this state upon conception, now able to nurse
LACTATING: females enter this state upon weaning, and are able to nurse
SENESCENT: unable to conceive or nurse

### HIDDEN 'ORGANS'

These organs are hidden from others.

REPRODUCTION: determines the ability to conceive and create offspring.
PERCEPTION: determines the ability to perceive the enivornment.
VOCALS: determines the ability to project sound.
REGULATORS: determines the chance of changing current state of signal or trait.
DIRECTION: tracks the overall preferred direction to go.
CHROMOSOMES: diploid chromosomes regulate the innate actions of the agent.
DECISION CENTER: where decisions live in the mind before becoming actionable.
MEMORY STORAGE: storage for learned actions and agents, places, things.

.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

circle
true
0
Circle -7500403 true true 0 0 300

circlea
true
0
Circle -7500403 true true 0 0 300
Circle -1184463 true false 103 13 92

circleab
true
0
Circle -7500403 true true 0 0 300
Circle -1184463 true false 103 13 92
Polygon -2674135 true false 30 120 270 120 285 165 15 165 30 120

circleabc
true
0
Circle -7500403 true true 0 0 300
Circle -1184463 true false 103 13 92
Polygon -14835848 true false 150 210 270 180 240 240 150 285 60 240 30 180
Polygon -2674135 true false 30 120 270 120 285 165 15 165 30 120

circleac
true
0
Circle -7500403 true true 0 0 300
Circle -1184463 true false 103 13 92
Polygon -14835848 true false 150 210 270 180 240 240 150 285 60 240 30 180

circleb
true
0
Circle -7500403 true true 0 0 300
Polygon -2674135 true false 30 120 270 120 285 165 15 165 30 120

circlebc
true
0
Circle -7500403 true true 0 0 300
Polygon -14835848 true false 150 210 270 180 240 240 150 285 60 240 30 180
Polygon -2674135 true false 30 120 270 120 285 165 15 165 30 120

circlec
true
0
Circle -7500403 true true 0 0 300
Polygon -14835848 true false 150 210 270 180 240 240 150 285 60 240 30 180

line
true
0
Line -7500403 true 150 0 150 300

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

square
false
0
Rectangle -7500403 true true 30 30 270 270

triangle
true
0
Polygon -7500403 true true 150 30 15 255 285 255

trianglea
true
0
Polygon -7500403 true true 150 30 15 255 285 255
Circle -1184463 true false 120 75 60

triangleab
true
0
Polygon -7500403 true true 150 30 15 255 285 255
Circle -1184463 true false 120 75 60
Polygon -2674135 true false 90 150 210 150 225 180 75 180

triangleabc
true
0
Polygon -7500403 true true 150 30 15 255 285 255
Circle -1184463 true false 120 75 60
Polygon -2674135 true false 90 150 210 150 225 180 75 180
Polygon -14835848 true false 75 195 225 195 255 240 45 240

triangleac
true
0
Polygon -7500403 true true 150 30 15 255 285 255
Circle -1184463 true false 120 75 60
Polygon -14835848 true false 75 195 225 195 255 240 45 240

triangleb
true
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -2674135 true false 90 150 210 150 225 180 75 180

trianglebc
true
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -2674135 true false 90 150 210 150 225 180 75 180
Polygon -14835848 true false 75 195 225 195 255 240 45 240

trianglec
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
<experiments>
  <experiment name="EcologySpace" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>plant-minimum-neighbors &gt;= plant-maximum-neighbors or not any? anima1s or median [generation-number] of anima1s &gt; 100</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../output/original/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="collect-data?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-structure">
      <value value="&quot;original&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterioration-rate">
      <value value="-0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-annual-cycle">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-daily-cycle">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-seasonality">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-quality">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-minimum-neighbors">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-maximum-neighbors">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="&quot;population&quot;"/>
      <value value="&quot;population&quot;"/>
      <value value="&quot;population&quot;"/>
      <value value="&quot;population&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
