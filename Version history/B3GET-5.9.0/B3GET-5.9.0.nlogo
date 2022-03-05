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
  "B3GET-data.nls"
  "B3GET-files.nls"
  "B3GET-g3notype.nls"
  "B3GET-import-export.nls"
  "B3GET-selection.nls"
  "B3GET-sta7us.nls"
  "B3GET-verification.nls"
]

breed [ plants plant ]
breed [ groups group ]
breed [ anima1s anima1 ]

turtles-own [ meta-id age ]

anima1s-own [

  ; PHENOTYPE VARIABLES
  ; visible
  biological.sex
  life.history
  female.fertility
  energy.supply
  group.identity
  is.dead
  alpha.signal
  beta.signal
  gamma.signal
  living.chance
  body.size
  body.shade

  chromosome.I
  chromosome.II
  carried.items

  ; hidden
  mutation.chance ; not done
  sex.ratio ; not done
  litter.size ; not done
  conception.chance
  day.perception.angle ; not done
  night.perception.angle ; not done
  audio.perception.angle ; not done
  day.perception.range ; not done
  night.perception.range ; not done
  audio.perception.range ; not done
  vocal.range ; not done
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
  adult-body-size
  adult-body-shade
  adult-perception-range
  adult-mutation-chance
  adult-sex-ratio
  adult-litter-size
  adult-conception-chance
  mother-initiated-birth
  mother-initiated-weaning
  distance-traveled
  foraging-gains
  foraging-losses
  helping-benefit
  helping-cost
  matings-list
  conceptions-list
  group-transfers-list
  cells-occupied
  infanticide-list                 ; list of infants attacked, which periodically gets deleted if check and infant still alive
  cause-of-death

]

plants-own [ penergy ]

groups-own [

  ; TRACKING VARIABLES
  distance-traveled
  total-energy-gained
  total-birth-count
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
  global-deterioration-rate
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
;
;--------------------------------------------------------------------------------------------------------------------

to setup-parameters
  set model-version "5.9.0"
  set global-deterioration-rate -0.0001
end

to setup-simulation
  clear-all
  reset-ticks
  setup-parameters
  setup-plants
  set simulation-id 0
  set documentation-notes ( word "Started a new simulation. " documentation-notes )
  update-simulation
  ifelse population-file-exists? [ import-population (word path-to-experiment "/" population ".csv") ] [ set population "" ]
  ifelse genotype-file-exists? [ import-genotype (word path-to-experiment "/" genotype ".txt") ] [ set genotype "" ]
end

to clear-plants
  clear-patches
  ask plants [ die ]
  set documentation-notes (word "Cleared all plants in simulation " simulation-id "." documentation-notes )
  update-simulation
end

to clear-population
  ask turtles with [ shape != "square" ][ die ]
end

to setup-plants
  ask plants [ die ]
  ask patches [
    set pcolor 36
    sprout-plants 1 [ initialize-plant ]]
end

to initialize-plant
  set meta-id random 9999999
  set penergy 0
  set hidden? false
  set size 1
  set shape "square"
  update-plant-color
end

to update-plant-color
  set color scale-color green penergy (1.5 * plant-quality) (-0.25 * plant-quality)
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

  ;ENVIRONMENTAL CONSTRAINTS
  update-plants
  ask turtles [ set age age + 1 ]
  ask groups [
    let new-xcor mean [xcor] of group-members
    let new-ycor mean [ycor] of group-members
    set distance-traveled distance-traveled + sqrt ( ( new-xcor - xcor ) ^ 2 + ( new-ycor - ycor ) ^ 2 )
    set xcor new-xcor
    set ycor new-ycor
  ]
  ask anima1s [
    deteriorate
    check-mortality
    update-appearance
    remove-done-decisions
    set decision.vectors []
    make-decisions 1
  ]

  ;ANIMA1 AGENCY
  ask anima1s [ if ( not is.dead ) [ act ]]

  if ( ticks > 0 and ceiling (ticks / 100) = (ticks / 100) and any? anima1s ) [
    print (word "Simulation " simulation-id " is now at " ticks " ticks, "
      precision mean [generation-number] of anima1s 3 " generations, and contains "
      count anima1s with [ not is.dead ] " individuals.") ]

  ;DATA COLLECTION
  if collect-data? [ collect-data ]

  ;ARTIFICIAL SELECTION
  if selection-on? [ artificial-selection ]

  tick
end

to make-decisions [ value ] ; value should be percent of decision-vectors that survive to be used
  let environment up-to-n-of 100 turtles with [ not is-group? self and ( not hidden? or member? self [my-offspring] of myself ) ] in-cone ( 4 ) 360
  if (life.history = "gestatee" ) [ set environment turtles with [ [meta-id] of self = [meta-id] of myself or meta-id = [mother-identity] of myself ]] ; gestatees can only see themselves, and their mothers
  generate-decisions environment
end

to update-plants
  let season ( cos (( 360 / plant-annual-cycle ) * ticks))
  let daytime ( cos (( 360 / plant-daily-cycle ) * ticks))

  let energy-update get-updated-value ( plant-seasonality * ( 1 / 100 ) * season ) ( 100 * daytime )

  ask plants [
    set penergy plant-quality * ( get-updated-value ( penergy / plant-quality ) energy-update )
    update-plant-color ]

  ; only a small proportion of plants change each TIMESTEP
  ask n-of (ceiling (0.01 * count patches)) patches [

    ; calculate the total penergy of surrounding PLANT neighbors
    let total-neighbor-penergy 0
    ask neighbors [
      if any? plants-here [
        ask plants-here [ set total-neighbor-penergy total-neighbor-penergy + penergy ]]]

    ; calculate a random value along a normal distribution of mean TOTAL-NEIGHBOR-PENERGY and standard deviation LIFE-SD
    let normal-value ( random-normal total-neighbor-penergy 1.0 ) / plant-quality

    ; life occurs when the random value is within the MIN and MAX values
    ifelse ( normal-value >= plant-minimum-neighbors and normal-value <= plant-maximum-neighbors )
    [ if ( not any? plants-here ) [ sprout-plants 1 [ initialize-plant ]] ]
    [ if ( any? plants-here ) [ ask plants-here [ die ]]] ]

end

to deteriorate
  ; the global-deterioration-rate is a negative number ( i.e. -0.001 )
  set living.chance get-updated-value living.chance global-deterioration-rate
  set body.size get-updated-value body.size global-deterioration-rate
  set body.shade get-updated-value body.shade global-deterioration-rate
  set day.perception.range get-updated-value day.perception.range global-deterioration-rate
  set night.perception.range get-updated-value night.perception.range global-deterioration-rate
  set audio.perception.range get-updated-value audio.perception.range global-deterioration-rate
  set mutation.chance get-updated-value mutation.chance global-deterioration-rate
  set sex.ratio get-updated-value sex.ratio global-deterioration-rate
  set litter.size get-updated-value litter.size global-deterioration-rate
  set conception.chance get-updated-value conception.chance  global-deterioration-rate
  set birthing.chance get-updated-value birthing.chance global-deterioration-rate
  set weaning.chance get-updated-value weaning.chance global-deterioration-rate
  set infancy.chance get-updated-value infancy.chance global-deterioration-rate
  set juvenility.chance get-updated-value juvenility.chance global-deterioration-rate
  set adulthood.chance get-updated-value adulthood.chance global-deterioration-rate
  set senescency.chance get-updated-value senescency.chance global-deterioration-rate
end

to check-mortality
  if ( random-float 1.0 > living.chance ) [
    (ifelse

      ; THIRD TIME: remove anima1 from the simulation
      ( is.dead = true and energy.supply = 0 ) [ die ]

      ; SECOND TIME: transfer all bodily energy
      ( is.dead = true and energy.supply > 0 ) [ set energy.supply 0 ]

      ; FIRST TIME: set anima1 to is.dead true
      [ set is.dead true set ticks-at-death ticks ])
  ]
end

to update-appearance
  set size body.size
  set label " "
  set color ( [color] of one-of groups with [ meta-id = [group.identity] of myself] ) + 5 - ( 10 ^ body.shade )
  set shape get-shape
end

to update-energy [ update ]
  set energy.supply energy.supply + update
end

;--------------------------------------------------------------------------------------------------------------------
; REPORTERS
;--------------------------------------------------------------------------------------------------------------------

to-report get-energy-feedback [ value ]
  report 100 ^ value ;param
end

to-report get-shape
  let base_shape ifelse-value ( biological.sex = "male" ) [ "triangle" ] [ "circle" ]
  let a_on ifelse-value alpha.signal [ "a" ] [ "" ]
  let b_on ifelse-value beta.signal [ "b" ] [ "" ]
  let c_on ifelse-value gamma.signal [ "c" ] [ "" ]
  report ( word base_shape a_on b_on c_on )
end

to-report group-size
  report count anima1s with [ group.identity = [meta-id] of myself ]
end

to-report get-updated-value [ current-value update-value ]
  let report-value ifelse-value ( current-value < 0.0000000001 ) [ 0.0000000001 ] [ ifelse-value ( current-value > 0.9999999999 ) [ 0.9999999999 ] [ current-value ] ]
  ifelse update-value < 0
  [ set report-value ( report-value ^ (1 + abs update-value) ) ]
  [ set report-value ( report-value ^ (1 / ( 1 + update-value) )) ]
  report precision report-value 10
end

to-report relatedness-with [ target ]
  let matching 0
  let not-matching 0
  let length-i length chromosome.I
  let length-ii length chromosome.II
  repeat 10
  [
    let chromosome-index one-of [ 0 1 ]
    let gene-index random ifelse-value ( chromosome-index = 0 ) [ length-i ] [ length-ii ]
    let my-allele item gene-index ifelse-value ( chromosome-index = 0 ) [ chromosome.I ] [ chromosome.II ]
    let allele-part-index 1 + random (length my-allele - 1)
    let my-allele-part item allele-part-index my-allele
    let target-chromosome ifelse-value ( chromosome-index = 0 ) [ [chromosome.I] of target ] [ [chromosome.II] of target ]
    let target-allele ifelse-value ( gene-index >= length target-chromosome ) [ "" ] [ item gene-index target-chromosome ]
    let target-allele-part ifelse-value ( allele-part-index >= length target-allele ) [ "" ] [ item allele-part-index target-allele ]
    ifelse ( my-allele-part = target-allele-part ) [ set matching matching + 1 ] [ set not-matching not-matching + 1 ]
  ]
  report matching / ( matching + not-matching + 0.0000000001 )
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

to leave-group [ value ]
  if ( random-float 1.0 < value ) [
    set previous-group-id group.identity
    hatch-groups 1 [
      initialize-group
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

;--------------------------------------------------------------------------------------------------------------------
; MOVEMENT
;--------------------------------------------------------------------------------------------------------------------

to set-heading [ value ] ; value not used
  set heading ( atan y.magnitude x.magnitude )
  set x.magnitude one-of [ 0.01 -0.01 ]
  set y.magnitude one-of [ 0.01 -0.01 ]
end

to turn-right [ value ] right ( 360 * value ) end

to turn-left [ value ] left ( 360 * value ) end

to go-forward [ value ]
;  let carry-weight size + sum map [ c -> [size] of c ] carried.items
;  let carry-energy-cost carry-weight * length carried.items
;
;  if (carry-energy-cost <= energy.supply) [
;    update-energy ( - carry-energy-cost )
;    forward value
;    set distance-traveled distance-traveled + value
;    foreach carried.items [ object ->
;      if (object != nobody) [ ask object [ move-to myself ] ]]]

  let travel-distance get-updated-value body.size value
  forward travel-distance
  if collect-data? [ set distance-traveled distance-traveled + travel-distance ]
  foreach carried.items [ object ->
    if (object != nobody) [ ask object [ move-to myself ] ]]
  if collect-data? [ if not member? patch-here cells-occupied [ set cells-occupied lput patch-here cells-occupied ]]
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
  if ( random-float 1.0 < infancy.chance ) [
    set mother-initiated-birth false
    update-to-infant
  ]
end

to update-to-infant
  set life.history "infant"
  set female.fertility " "
  set hidden? false
  set ticks-at-birth ticks
  if (collect-data?) [ ask current-group [ set total-birth-count total-birth-count + 1 ]]
end

to check-birth [ value ] ; want to code in check if gestatee is alive but also want anima1s to evolve this
  set birthing.chance get-updated-value birthing.chance value
  if ( random-float 1.0 < birthing.chance ) [
    set female.fertility "lactating"
    ask my-offspring with [ life.history = "gestatee" ] [ update-to-infant ]
    set birthing.chance 0
  ]
end

to check-juvenility [ value ]
  set juvenility.chance get-updated-value juvenility.chance value
  if ( random-float 1.0 < juvenility.chance ) [
    set mother-initiated-weaning false
    update-to-juvenile
  ]
end

to update-to-juvenile
  set life.history "juvenile"
  set female.fertility " "
  set ticks-at-weaning ticks
end

to check-weaning [ value ]
  set weaning.chance get-updated-value weaning.chance value
  if ( random-float 1.0 < weaning.chance ) [
    set female.fertility "cycling"
    ask my-offspring with [ life.history = "infant" ] [ update-to-juvenile ]
    set weaning.chance 0
  ]
end

to check-adulthood [ value ]
  set adulthood.chance get-updated-value adulthood.chance value
  if ( random-float 1.0 < adulthood.chance ) [
    update-to-adult
  ]
end

to update-to-adult
  set life.history "adult"
  set female.fertility ifelse-value ( biological.sex = "male" ) [ " " ] [ "cycling" ]
  set adult-body-size body.size
  set adult-body-shade body.shade
  set adult-perception-range day.perception.range
  set adult-mutation-chance mutation.chance
  set adult-sex-ratio sex.ratio
  set adult-litter-size litter.size
  set adult-conception-chance conception.chance
  set ticks-at-sexual-maturity ticks
end

to check-senescence [ value ]
  set senescency.chance get-updated-value senescency.chance value
  if ( random-float 1.0 < senescency.chance ) [
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

to supply-to [ target value ]
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

to demand-from [ target value ]
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
  let energy-wanted get-energy-feedback value
  let energy-eaten 0

  (ifelse
    ( is-plant? target ) [
      set energy-eaten ifelse-value ( energy-wanted < [penergy] of target) [energy-eaten] [[penergy] of target]
      ;set _stomach _stomach + energy-eaten
      update-energy energy-eaten
      ask target [
        set penergy (penergy - energy-eaten)
        if ( penergy <= 0 ) [ set penergy 0 ]]]

    ( is-anima1? target ) [
      set energy-eaten ifelse-value ( energy-wanted < [energy.supply] of target) [energy-eaten] [[energy.supply] of target]
      ;set _stomach _stomach + energy-eaten
      update-energy energy-eaten
      ask target [
        ;set _reserves (_reserves - energy-eaten)
        ;if ( _reserves <= 0 ) [ set _reserves 0 ]
        update-energy ( - energy-eaten )
      ]
    ]

    [])

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

to pick-up [ target value ]

  if ( target != self and is-anima1? target ) [

    let squirm-decisions get-decisions target self "squirm-from"
    let squirm-cost get-decisions-cost squirm-decisions

    let pickup-decisions get-decisions self target "pick-up"
    let pickup-cost get-decisions-cost pickup-decisions

    ifelse ( squirm-cost > pickup-cost )
    [ set carried.items remove target carried.items ]
    [ set carried.items lput target carried.items ]

    ask target [ decisions-done squirm-decisions ]
    decisions-done pickup-decisions

  ]
end

to squirm-from [ target value ]

  if ( target != self and is-anima1? target ) [

    let pickup-decisions get-decisions target self "pick-up"
    let pickup-cost get-decisions-cost pickup-decisions

    let squirm-decisions get-decisions self target "squirm-from"
    let squirm-cost get-decisions-cost squirm-decisions

    ifelse ( squirm-cost > pickup-cost )
    [ ask target [ set carried.items remove myself carried.items ]]
    [ ask target [ set carried.items lput target carried.items ]]

    decisions-done squirm-decisions
    ask target [ decisions-done pickup-decisions ]

  ]
end

to put-down [ target value ]

  if ( target != self and is-anima1? target ) [

    let cling-decisions get-decisions target self "cling-to"
    let cling-cost get-decisions-cost cling-decisions
    let putdown-decisions get-decisions self target "put-down"
    let putdown-cost get-decisions-cost putdown-decisions

    ifelse ( putdown-cost >= cling-cost )
    [ set carried.items remove target carried.items ]
    [ set carried.items lput target carried.items ]

    decisions-done putdown-decisions
    ask target [ decisions-done cling-decisions ]

  ]
end

to cling-to [ target value ]

  if ( target != self and is-anima1? target ) [

    let putdown-decisions get-decisions target self "put-down"
    let putdown-cost get-decisions-cost putdown-decisions

    let cling-decisions get-decisions self target "cling-to"
    let cling-cost get-decisions-cost cling-decisions

    ifelse ( putdown-cost > cling-cost )
    [ ask target [ set carried.items remove myself carried.items ]]
    [ ask target [ set carried.items lput target carried.items ]]

    decisions-done cling-decisions
    ask target [ decisions-done putdown-decisions ]

  ]
end

to attack [ target value ]

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

to help [ target value ]
  if ( is-anima1? target ) [

    if (collect-data?) [ ; Hamilton's Rule
      let r relatedness-with target
      let B get-updated-value ([living.chance] of target) ( value ) - [living.chance] of target ; how much target benefitted
      let C living.chance - get-updated-value living.chance ( value ) ; how much you *could have* benefitted by investing in yourself
      set helping-benefit helping-benefit + r * B ; benefit to yourself based on relatedness to target
      set helping-cost helping-cost + C ; cost of not helping yourself
    ]

    ask target [ set living.chance get-updated-value living.chance ( value ) ]
  ]
end

to join-group-of [ target value ]
  if ( is-anima1? target and random-float 1.0 < value ) [
    set previous-group-id group.identity
    set group.identity [group.identity] of target
    if collect-data? [ set group-transfers-list lput [meta-id] of current-group group-transfers-list ]
  ]
end

to mate-with [ target value ]
  let mate-decisions get-decisions target self "mate-with"
  if ( not empty? mate-decisions ) [
    let one-mate one-of mate-decisions
    ifelse ( biological.sex = "female" )
    [ conceive-with target ]
    [ ask target [ conceive-with myself ]]
    set matings-list lput [meta-id] of target matings-list
    ask target [
      decisions-done (list one-mate)
      set matings-list lput [meta-id] of myself matings-list ]]
end

to conceive-with [ target ] ; FEMALE PROCEDURE
  if biological.sex = "female" and female.fertility = "cycling" and life.history = "adult" and [life.history] of target = "adult" and [biological.sex] of target = "male" [
    if random-float 1.0 < (mean (list conception.chance [conception.chance] of target)) [
      set label "!"
      set conceptions-list lput [meta-id] of target conceptions-list
      ask target [ set conceptions-list lput [meta-id] of myself conceptions-list ]
;      let preferred-litter ;100 ^ ( mean (list litter.size [litter.size] of target)) ; param
;      let floor-litter floor preferred-litter
;      let percent-litter preferred-litter - floor-litter
;      let my-litter-size ifelse-value ( random-float 1.0 < percent-litter ) [ floor-litter + 1 ] [ floor-litter ]
      hatch-anima1s 1 [ initialize-from-parents myself target ]
      set female.fertility "pregnant"
  ]]
end

to initialize-from-parents [ m f ]
  set meta-id random 9999999
  set hidden? true
  set is.dead false
  ask m [ pick-up myself 1.0 ]
  set biological.sex ifelse-value ( random-float 1.0 < 0.5 ) ["male"] ["female"] ; mean (list [sex.ratio] of m [sex.ratio] of f) ) ["male"] ["female"]
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
  set label-color white
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
  set adult-body-size 0
  set adult-body-shade 0
  set adult-perception-range 0
  set adult-mutation-chance 0
  set adult-sex-ratio 0
  set adult-litter-size 0
  set adult-conception-chance 0
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

  let rate-of-mutation 0.05 ; mean (list [mutation.chance] of m [mutation.chance] of f)
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

        ( choice = 0 ) [ ; delete allele
          set updated-alleles [] ]

        ( choice = 1 ) [] ; delete codon

        ( choice = 2 ) [ ; duplicate allele
          set updated-alleles (list allele allele) ]

        ( choice = 3 ) [] ; duplicate codon

        ( choice = 4 ) [ ; update number
          foreach updated-alleles [ x ->
            let allele-position position x updated-alleles
            foreach x [ codon ->
              if ( is-number? codon ) [
                let codon-position position codon x
                let updated-allele replace-item codon-position x ( precision (one-of [ -1 1 ] * get-updated-value codon ( 0.1 * one-of [ -1 1 ] )) 6 )
                set updated-alleles replace-item allele-position updated-alleles updated-allele  ]]]]

        ( choice = 5 ) [ ; update codon
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
      set updated-chromosome ifelse-value ( allele-update != [] ) [ lput allele-update updated-chromosome ] [ updated-chromosome ]
    ]
  ]

  report updated-chromosome
end
@#$#@#$#@
GRAPHICS-WINDOW
7
87
711
792
-1
-1
23.2
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
29
0
29
1
1
1
ticks
30.0

BUTTON
270
10
336
79
setup
setup-simulation
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
experiments/EcologyProject/
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
975
231
1217
424
plot
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"age" 100.0 1 -16777216 true "" "if (useful-commands = \"age\") [ histogram [age] of anima1s ]"

INPUTBOX
720
10
967
223
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
79
save
update-simulation
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
720
433
1217
789
11

SLIDER
974
82
1217
115
plant-minimum-neighbors
plant-minimum-neighbors
0
8
0.0
.5
1
NIL
HORIZONTAL

SLIDER
974
118
1217
151
plant-maximum-neighbors
plant-maximum-neighbors
0
8
8.0
.5
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
974
46
1217
79
plant-seasonality
plant-seasonality
0
100
0.0
5
1
%
HORIZONTAL

SLIDER
974
10
1217
43
plant-annual-cycle
plant-annual-cycle
10
10000
2000.0
10
1
ticks
HORIZONTAL

SLIDER
974
154
1217
187
plant-daily-cycle
plant-daily-cycle
1
100
1.0
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
ifelse-value ( ( cos (( 360 / plant-daily-cycle ) * ticks)) > 0 ) [ \"DAY\" ] [ \"NIGHT\" ]
17
1
11

INPUTBOX
720
230
846
299
population
popu1ation
1
0
String

INPUTBOX
720
303
846
373
genotype
NIL
1
0
String

BUTTON
851
230
906
299
⟳
ifelse ( population = \" \" )\n[ set population generate-population-id ]\n[ set population \" \" ]
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
911
230
966
263
⤒
save-population ( word path-to-experiment \"/\" population \".csv\")
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
911
266
966
299
⤓
import-population (word path-to-experiment \"/\" population \".csv\")
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
720
379
907
424
useful-commands
useful-commands
"output-runtime" "output-all-verification" "output-true-verification" "output-false-verification" "output-decisions" "clear-plants" "clear-population" "reset-chromosomes" "setup-plants" "plot-plant-abundance" "plot-age" "plot-generation" "plot-group-size" "plot-group-transfers" "plot-reproductive-skew"
13

BUTTON
911
379
966
424
▷
clear-output\nrun useful-commands\n
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
851
303
906
372
⟳
ifelse ( genotype = \" \" or genotype = \"\" )\n[ set genotype generate-genotype-id ]\n[ set genotype \" \" ]
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
911
339
966
372
⤓
import-genotype (word path-to-experiment \"/\" genotype \".txt\") 
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
911
303
966
336
⤒
ask one-of anima1s [ save-genotype ( word path-to-experiment \"/\" genotype \".txt\") ]
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
974
190
1217
223
plant-quality
plant-quality
.1
1
1.0
.1
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

@#$#@#$#@
# B3GET 5.9.0 INFORMATION

Compatible with NetLogo 6.1.1

## WHAT IS IT?

This model simulates the behavior of evolving animal populations to test hypotheses in behavioral ecology. Users input populuation files to seed the initial population and run simulations to evolve these populations - and their genotype files - over generations. Behavioral strategies that are beneficial for their environmental context are expected to emerge.

## HOW IT WORKS

Primate agents possess two chromosomes that dictate the weighted behavioral preference for various "actions" for given environmental conditions. These weighted preferences (and many other aspects of assessing the environment and deciding actions) can mutate and so new behavioral strategies can emerge over generations.

## HOW TO USE IT

### STARTING UP

B3GET should come with the following file structure. These extensions are written as such to maintain a modular code structure, which promotes: (1) ease in testing and maintenance, (2) future innovations will not interfere with other parts of the program.

> [B3GET]
--- B3GET.nlogo
--- B3GET-data.nls
--- B3GET-files.nls
--- B3GET-g3notype.nls
--- B3GET-import-export.nls
--- B3GET-selection.nls
--- B3GET-verification.nls
--- [experiments]
------ [experiment1]
---------- metafile.csv
---------- g3notype.txt
---------- sta7us.txt
---------- popula7ion.csv
---------- s33d.csv

B3GET should start with PATH-TO-EXPERIMENT set to [experiments/experiment1], which means that any data or files generated during simulation will be saved in the [experiment1] folder. Initially, POPULATION is set to [popula7ion] and GENOTYPE is set to [sta7us], which are files included above. With these settings, you can just click SETUP and GO to start your first simulation! Please refer to the control descriptions below to perform more complex tasks.

### PRIMARY CONTROLS

PATH-TO-EXPERIMENT: the path indicating where to store data for the current experiment.
SETUP: returns the model to the starting state.
GO: runs the simulation.
GO ONCE: runs exactly one tick, or timestep, of the simulation
COLLECT-DATA?: "ON" collects data on the animals (see "data" extension).
SELECTION-ON?: "ON" artificially culls the animal population (see "selection" extension).
SAVE: 
DOCUMENTATION-NOTES:

### VIEW INFORMATION

The main view screen allows us to visually see emergent behaviors. 

SIMULATION: the unique identification code of the current simulation.
SEASON: cycles between 1.0 (summer solstice) and -1.0 (winter solstice) according to the plant-annual-cycle.
TIME: varies from DAY to NIGHT according to plant-daily-cycle.

### ENVIRONMENTAL CONTROLS

Plants and plant life and growth are modeled after Conway's Game of Life, a cellular automaton model that contained rules for when a cell could be 'alive' or 'dead', thus simulating a living ecosystem.

PLANT-ANNUAL-CYCLE
PLANT-SEASONALITY
PLANT-MINIMUM-NEIGHBORS
PLANT-MAXIMUM-NEIGHBORS
PLANT-DAILY-CYCLE
PLANT-QUALITY

### IMPORT & EXPORT CONTROLS

To first begin a simulation, you must import a 'seed' population, the ancestral population from which a new population will evolve. Upon clicking SETUP, B3GET will automatically import the population name indicated in the POPULATION input box, if it exists. 

#### POPULATION

The POPULATION input stores the population name used with the following buttons:

⟳: alternates between generating a random POPULATION name or clearing POPULATION.
<: prints POPULATION file information in the output (work in progress).
⤒: saves all animals to the file matching the POPULATION name.
⤓: imports all animals from the file matching the POPULATION name.

#### GENOTYPE

Each animal imported from a population file includes its own genotype, but you can override this and give each member of the population a new genotype. As with the population files, B3GET will automatically import the genotype name indicated in the GENOTYPE input box, if it exists. The GENOTYPE input stores the genotype name used with the following buttons:

⟳: alternates between naming GENOTYPE after a random animal or clearing GENOTYPE.
<: prints GENOTYPE file information in the output (work in progress).
⤒: saves the genotype of animal matching the GENOTYPE name to same-named file.
⤓: gives all animals the genotype file imported from the GENOTYPE name.

By default, POPULATION is set to popu7ation and GENOTYPE is set to sta7us.

### MONITORING CONTROLS

While you can use the BehaviorSpace functionality to grow many populations at once, sometimes it is useful to directly observe and influence a single simulation. The mintoring controls allow for some direct observation and influence.

USEFUL-COMMANDS: selection from a list of premade functions
▷: hit this button to run the command selected by USEFUL-COMMANDS
PLOT:
OUPUT: 

### EXTENSIONS

Detailed information on what each extension does can be found in those files. Here is a brief list of all extensions and briefly what they do:

DATA: controls how data is collected during simulation runs.
FILES: controls how files are created and data stored within them.
IMPORT-EXPORT: controls for importing and exporting populations of agents.
SELECTION: controls for artificial selection of agents during simulation.
STA7US: a simple genotype file reader.
G3NOTYPE: a more complex genotype file reader.
VERIFICATION: verification code for this model.

### NEW EXPERIMENT

If you want to start a new experiment and store information in a separate place, simply create a new folder in [experiments] and make sure to update the PATH-TO-EXPERIMENT with the new folder name. A new metadata file will be automatically generated. Any genotype files that you want the model to access must be stored here. 

## THINGS TO NOTICE

The individuals in the initial seeded population have the same genotype(s). However, stochastic occurances and fluxuating environmental conditions (based on the social structure of the population) cause unique individual behaviors to emerge in an unexpected and idiosynchractic way. Over time, as the number of accumulated mutations increases, more individuals may appear to have unique behavioral strategies and life histories. 

## THINGS TO TRY

You can modify genotype files to many different configurations. The number of genes can also be modified but beware that lengthy genotypes may cause lag in runtime. Explore how modifying the environmental settings may influence behavioral strategies. However, genotype configuration also influence female, and male, strategies.

## HOW TO CITE

The first peer-reviewed paper on B3GET can be found here:

Crouse, K. N., Miller, C. M., and Wilson, M. L. (2019). New approaches in primate socioecology: Does small female group size BEGET loyal males? Journal of Human Evolution, xxx, xx-xx.

## COPYRIGHT AND LICENSE

Copyright 2019 K N Crouse

The model may be freely used, modified and redistributed provided this copyright is included and the resulting models are not used for profit.

Contact K N Crouse at crou0048@umn.edu if you have questions about its use.

#####

# REFERENCE GUIDE

## Actions

### Main intra-actions

MAINTAIN-BODY [ value ] VALUE correlates with reduction in mortality chance.
BODY-SIZE [ value ] Add VALUE to grow body size.
BODY-SHADE [ value ]darken body color
DAY-PERCEPTION [ value ]widen visual perception during the day
NIGHT-PERCEPTION [ value ]widen visual perception during the night
AUDIO-PERCEPTION [ value ]widen audio perception range
CONCEPTION-CHANCE[ value ]increase the chance of conceiving during mating
MUTATION-RATE [ value ]change the amount of mutation that occurs during chromosomal recombination
SEX-RATIO [ value ]modify the liklihood of producing a female or male offspring (female)
LITTER-SIZE [ value ](female only) modify the amount of offspring produced in each litter
LEAVE-GROUP [ value ]attempt to leave current group in favor of being alone ---> 

### Movement

TURN-RIGHT [ value ] turn right by some angle
TURN-LEFT [ value ]turn left by some angle
GO-FORWARD [ value ] move directly ahead by about a body size
set-heading [ value ]set heading to preference and move forward

### Signaling

SIGNAL-A-ON [ value ] modify the chance of signal A turning on Turn signal A on.
SIGNAL-B-ON [ value ] Modify the chance of signal B turning on.
SIGNAL-C-ON [ value ] Modify the chance of signal C turning on.

### Life history

CHECK-INFANCY [ value ] check-infancy ---> UPDATE-TO-INFANT
CHECK-BIRTHING [ value ] check-birth [ value ] ---> UPDATE-TO-INFANT
CHECK-JUVENILITY [ value ] check-juvenility [ value ] ---> UPDATE-TO-JUVENILE
CHECK-WEANING [ value ] check-weaning [ value ] ---> UPDATE-TO-JUVENILE
CHECK-ADULTHOOD [ value ] check-adulthood [ value ] ---> UPDATE-TO-ADULT
CHECK-SENESCENCE [ value ] check-senescence [ value ] ---> UPDATE-TO-SENESCENT

### Energy

SUPPLY-TO [ target value ]
DEMAND-FROM [ target value ] 
---> RECEIVE-FROM
EAT [ target value ]

### Other inter-actions

PICK-UP [ target value ]
PUT-DOWN [ target value ]
GIVE-TO (not done)
RECEIVE-FROM (not done)
ATTACK [ target value ]
HELP [ target value ]
JOIN-GROUP-OF [ target value ]
MATE-WITH [ target value ] ---> CONCEIVE-WITH

## Genotypes

The actions listed above show the range of possible actions that an agent can take. However, the degree to which an agent performs these actions, how much effort they put into doing so, and who they target is up to the genotype. Many different genotype file configurations are possible, as long as they include (1) each row represents one allele, (2) these alelels here represented as a function that takes the environment as input and outputs the desired action, and (3) each row contains a list of codons that can be altered during reproduction. This version of B3GET comes with two genotype file extensions: sta7us and g3notype. Specific information about each file type can be found there.

## Phenotypes

When agents perform actions, for the most part this means that they are changing the state variables of themselves or others. These state variables can be thought of as an organisms phenotype, which emerges from a combination of an organism's genotype and life experiences interacting with its environment. What emerges are the 'organs' that the virtual organism has.

### Visible 'organs'

SIZE: determines the overall body size or mass of an individual.
COLOR: color is determined by group.identity and the sade of that color by body.shade.
SEX: either "male" or "female".
LIFE HISTORY: "gestatee", "infant", "juvenile", "adult", or "senescent".
FERILITY: is "cycling", "pregnant" or "lactating" for adult females.
HEALTH: liklihood of an agent living to the next timestep.
ENERGY: agents can see the energy supply of another agent.
ALIVE: agents can see whether or not an agent is dead or alive.
SIGNALS: determines whether or not that signal is visibly on.
GROUP: agents can tell whether or not they have the same group identity.
KINSHIP: check the chromosomes to deteremine degree of relatedness to that individual.

### Hidden 'organs'

REPRODUCTIVE: ability to conceive and create offspring.
PERCEPTION: ability to perceive the enivornment.
VOCALS: ability to project sound.
REGULATORS: chance of changing current state of signal or trait.
DIRECTION: tracks the overall preferred direction to go.

### Complex 'organs'

CHROMOSOMES: two chromosomes regulate the innate actions of the agent.
DECISION CENTER: where decisions live in the mind before becoming actionable.
MEMORY STORAGE: storage for learned actions and people, places, things.
INVENTORY: a list of other agents that are being carried.

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
  <experiment name="thesis_experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup-simulation</setup>
    <go>go</go>
    <exitCondition>not any? anima1s or median [generation] of anima1s &gt; 100</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;experiments/thesis/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-version">
      <value value="&quot;20190912&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="global-deterioration-rate">
      <value value="-1.0E-4"/>
      <value value="-0.001"/>
      <value value="-0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="collect-data?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-annual-cycle">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-seasonality">
      <value value="0"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="plant-minimum-neighbors" first="0" step="0.5" last="8"/>
    <steppedValueSet variable="plant-maximum-neighbors" first="0" step="0.5" last="8"/>
    <enumeratedValueSet variable="population">
      <value value="&quot;popula7ion&quot;"/>
      <value value="&quot;c0mmunity&quot;"/>
      <value value="&quot;s33d&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype">
      <value value="&quot;g3notype&quot;"/>
      <value value="&quot;sta7us&quot;"/>
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
