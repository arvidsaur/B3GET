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
  "import-export.nls"
  "selection.nls"
  "verification.nls"
  "sta7us.nls"
  "g8tes.nls"
  "results.nls"
]

breed [ plants plant ]
breed [ groups group ]
breed [ anima1s anima1 ]

turtles-own [ meta-id age energy.supply ]

patches-own [ penergy.supply ]

anima1s-own [

  ; PHENOTYPE VARIABLES
  ; visible
  biological.sex
  life.history
  female.fertility
  group.identity
  is.alive
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
  stomach.size
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

  mother
  x.magnitude
  y.magnitude
  chromosome.I
  chromosome.II

  my.environment
  decision.vectors
  energy.allocated
  completed.actions

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
  adult-stomach-size
  adult-mutation-chance
  adult-sex-ratio
  adult-litter-size
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
  foraging-cost
  ingroup-female-female-help-cost
  ingroup-male-male-help-cost
  ingroup-male-female-help-cost
  whole-related-help-cost
  half-related-help-cost
  fourth-related-help-cost
  eighth-related-help-cost
  matings-list
  conceptions-list
  group-transfers-list
  cells-occupied
  infanticide-list ;; to do
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
  model-structure
  genotype-reader
  simulation-id
  deterioration-rate
  selection-rate

  ; TRACKING VARIABLS
  population-decisions
  population-allocations
  population-actions
]

;--------------------------------------------------------------------------------------------------------------------
;
;   oo            dP                     .8888b
;                 88                     88   "
;   dP 88d888b. d8888P .d8888b. 88d888b. 88aaa  .d8888b. .d8888b. .d8888b.
;   88 88'  `88   88   88ooood8 88'  `88 88     88'  `88 88'  `"" 88ooood8
;   88 88    88   88   88.  ... 88       88     88.  .88 88.  ... 88.  ...
;   dP dP    dP   dP   `88888P' dP       dP     `88888P8 `88888P' `88888P'
;
;--------------------------------------------------------------------------------------------------------------------

to setup-button
  setup generate-simulation-id
end

to go-button
  go
end

to save-button
  ifelse ( simulation-id = 0 )
  [ set simulation-id generate-simulation-id ]
  [ set documentation-notes (word "Simulation " simulation-id " saved. " documentation-notes )
    update-metafile "simulation" simulation-id ]
end

to reset-population-button
  set population generate-population-id
end

to export-population-button
  save-population
end

to import-population-button
  import-population
end

to reset-genotype-button
  set genotype generate-genotype-id
end

to export-genotype-button
  ask anima1s with [ read-from-string but-first genotype = meta-id ] [ save-genotype ]
end

to import-genotype-button
  import-genotype
end

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
  set model-version "~1.1.0"
  set deterioration-rate -0.001
  set selection-rate 0.0001
end

to setup-plants
  ask plants [ die ]
  ask patches [ set pcolor 36 ]
  ask patches [ sprout-plants 1 [ initialize-plant ]]
end

to setup-patches
  ask patches [ initialize-patch ]
end

to setup [ new-simulation-id ]
  ; save world file of old simulation before starting new simulation under following conditions
  if ( simulation-id != 0 and behaviorspace-run-number = 0 and output-results? = true ) [ record-world ]
  clear-all
  reset-ticks
  set simulation-id new-simulation-id
  setup-parameters
  import-population ; must go first
  import-genotype
  ;setup-plants
  setup-patches
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

  ; THE ENVIRONMENT
  ifelse ( model-structure = "no-plants" )
  [ ask plants [ die ] ]
  [
    ;update-plants
    update-patches
  ]

  ; AGENT MORTALITY
  ifelse ( model-structure = "reaper" )
  [ if (( count anima1s - 100 ) > 0 ) [ ask n-of ( count anima1s - 100 ) anima1s [ remove-from-simulation ]]]
  [ ask anima1s [check-mortality] ]

  ; AGENT REPRODUCTION
  if ( model-structure = "sower" and count anima1s < 100 ) ; random mating
  [ repeat ( 100 - count anima1s ) [
    if ( ( count anima1s with [ biological.sex = "male" and life.history = "adult" ] > 0 ) and ( count anima1s with [ biological.sex = "female" and life.history = "adult" and female.fertility = "cycling" ] > 0 ) )
    [ ask one-of anima1s with [ biological.sex = "female" and life.history = "adult" and female.fertility = "cycling" ] [ conceive-with ( one-of anima1s with [ biological.sex = "male" and life.history = "adult" ] ) 999999 ]]]]

  ; AGENT UPDATES
  ask turtles [ set age age + 1 ] ; all turtles age each timestep
  ask anima1s [ deteriorate update-appearance ]

  ; AGENT AGENCY
  ask anima1s with [ is.alive ] [ consider-environment ]
  ask anima1s with [ is.alive ] [ make-decisions ]
  ask anima1s with [ is.alive ] [ allocate-energy ]
  ask anima1s with [ is.alive ] [ do-actions ]

  ; ARTIFICAL SELECTION
  if selection-on? [ artificial-selection ]

  ; SIMULATION OUTPUT
  if output-results? [ output-results ]
  ; prints out current status of the simulation every 100 timesteps
  if ( ticks > 0 and ceiling (ticks / 100) = (ticks / 100) and any? anima1s ) [
    let print-text (word "Simulation " simulation-id " is now at " precision (ticks / plant-annual-cycle) 3 " years, "
      precision sum [energy.supply] of plants 3 " plant units, "
      precision mean [generation-number] of anima1s 3 " generations, and contains "
      count anima1s with [ is.alive ] " living organisms.")
    print print-text
    if ( behaviorspace-run-number > 0 ) [ output-print print-text ] ]

  tick
end

;--------------------------------------------------------------------------------------------------------------------
; GLOBAL
;--------------------------------------------------------------------------------------------------------------------

to-report get-solar-status report ifelse-value ( ( cos (( 360 / plant-daily-cycle ) * ticks)) > 0 ) [ "DAY" ] [ "NIGHT" ] end

to-report get-updated-value [ current-value update-value ]
  let report-value ifelse-value ( current-value < 0.00001 ) [ 0.00001 ] [ ifelse-value ( current-value > 0.99999 ) [ 0.99999 ] [ current-value ] ]
  ifelse update-value < 0
  [ set report-value ( report-value ^ (1 + abs update-value) ) ]
  [ set report-value ( report-value ^ (1 / ( 1 + update-value) )) ]
  report report-value
end

to-report generate-simulation-id
  let alphabet [ "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" ]
  report ( word "s" random 99 one-of alphabet one-of alphabet one-of alphabet )
end

;--------------------------------------------------------------------------------------------------------------------
; PLANTS
;--------------------------------------------------------------------------------------------------------------------

to update-plants
  let season ( cos (( 360 / plant-annual-cycle ) * ticks))
  let daytime ( cos (( 360 / plant-daily-cycle ) * ticks))

  ask plants [

    let energy-update ( random-float plant-seasonality * ( abs season ) * ( 1 / 10000 ) )
    let random-change random-float 0.001

    let value 0
    ask neighbors [
      if any? plants-here [
        ask plants-here [ set value value + energy.supply ]]]

    set energy.supply ifelse-value ( season > 0 )
    [ energy.supply + random-change + ifelse-value ( value >= plant-minimum-neighbors and value <= plant-maximum-neighbors ) [ energy-update ] [  0 ] ]
    [ energy.supply - random-change ]

    if energy.supply >= plant-quality [ set energy.supply plant-quality ]
    if energy.supply <= 0 [ set energy.supply 0 ]

    ;set label precision value 1
    update-plant-color
  ]
end

to update-patches
  let season ( cos (( 360 / plant-annual-cycle ) * ticks))

;  ask patches [
;
;    let a 1
;    let b ( plant-minimum-neighbors + plant-maximum-neighbors ) / 2
;    let c ( b - plant-minimum-neighbors )
;    let x sum [penergy.supply] of neighbors
;
;    let probability-value ( a * e ^ ( - (( x - b ) ^ 2 ) / ( 2 * ( c ^ 2 ) )) ) -  0.5 + ( 0.5 * season * plant-seasonality / 100 )
;
;    let boolean-result random-float 1.0 <= ( abs probability-value )
;
;    let change-in-energy plant-daily-cycle / plant-annual-cycle
;
;    if ( probability-value > 0 and boolean-result = true ) [ set penergy.supply penergy.supply + change-in-energy ]
;    if ( probability-value <= 0 and boolean-result = true ) [ set penergy.supply penergy.supply - change-in-energy ]
;
;    if penergy.supply >= plant-quality [ set penergy.supply plant-quality ]
  ;    if penergy.supply <= 0 [ set penergy.supply 0 ]
  ;
  ;    ;set plabel precision x 1
  ;    update-patch-color ]

  ask patches [

    let a 1 ;( season * plant-seasonality / 100 )
    let b ( plant-minimum-neighbors + plant-maximum-neighbors ) / 2
    let c ( b - plant-minimum-neighbors )
    let x sum [penergy.supply] of neighbors

    let up ( a * e ^ ( - (( x - b ) ^ 2 ) / ( 2 * ( c ^ 2 ) )) ) ; - 0.5 + ( 0.5 * season * plant-seasonality / 100 )

    let boolean-result random-float 1.0 <= ( abs probability-value )

    let change-in-energy 0.001 ; plant-daily-cycle / plant-annual-cycle

;    ifelse ( probability-value > 0 )
;    [ set penergy.supply penergy.supply + change-in-energy ]
;    [ set penergy.supply penergy.supply - change-in-energy ]

    ;if ( probability-value <= 0 and boolean-result = true ) [ set penergy.supply penergy.supply - change-in-energy ]

    ;set penergy.supply penergy.supply + ifelse-value ( season > 0 ) [ 0.0001 ] [ ( - 0.0001 ) ]
    set penergy.supply penergy.supply + probability-value * change-in-energy
    if penergy.supply >= plant-quality [ set penergy.supply plant-quality ]
    if penergy.supply <= 0 [ set penergy.supply 0 ]

    set plabel precision x 1
    update-patch-color ]

 ; diffuse penergy.supply 0.001

end

to initialize-plant
  set meta-id random 9999999
  set energy.supply random-float plant-quality
  set hidden? false
  set size 1
  set shape "square"
  update-plant-color
end

to initialize-patch
  set penergy.supply random-float plant-quality
  update-patch-color
end

to update-plant-color
  set color scale-color green energy.supply 1.5 -0.25
end

to update-patch-color
  set pcolor scale-color green penergy.supply 1.5 -0.25
end

;--------------------------------------------------------------------------------------------------------------------
; AGENTS
;--------------------------------------------------------------------------------------------------------------------

to deteriorate
  ; the deterioration-rate is a negative number ( i.e. -0.001 )
  set living.chance get-updated-value living.chance deterioration-rate
end

to check-mortality
  if ( random-float 1.0 > living.chance ) [
    ifelse ( is.alive = false )

    ; SECOND DEATH: remove agent from the simulation
    [ remove-from-simulation ]

    ; FIRST DEATH: set anima1 to is.dead true
    [ set is.alive false set ticks-at-death ticks ]]
end

to remove-from-simulation
  let me-the-dying-agent self
  if ( ticks-at-death = 0 ) [ set ticks-at-death ticks ]
  ask anima1s with [ member? myself carried.items ] [ set carried.items remove myself remove-duplicates carried.items ]
  ask current-group [ if ( not any? group-members with [ self != me-the-dying-agent ] ) [ die ] ]
  die
end

to update-appearance
  set size body.size
  set label " "
  set color (( [color] of one-of groups with [ meta-id = [group.identity] of myself ] ) + 5 - ( 10 ^ body.shade ))
  set shape get-shape
end

to-report get-shape
  let base_shape ifelse-value ( biological.sex = "male" ) [ "triangle" ] [ "circle" ]
  let a_on ifelse-value alpha.signal [ "a" ] [ "" ]
  let b_on ifelse-value beta.signal [ "b" ] [ "" ]
  let c_on ifelse-value gamma.signal [ "c" ] [ "" ]
  report ( word base_shape a_on b_on c_on )
end

to update-energy [ update ]
  set energy.supply energy.supply + update
end

;--------------------------------------------------------------------------------------------------------------------
;                             oo
;
;  .d8888b. 88d888b. .d8888b. dP 88d888b. .d8888b.
;  88ooood8 88'  `88 88'  `88 88 88'  `88 88ooood8
;  88.  ... 88    88 88.  .88 88 88    88 88.  ...
;  `88888P' dP    dP `8888P88 dP dP    dP `88888P'
;                         .88
;                     d8888P
;
; consider-environment > make-decisions > allocate-energy > do-actions
;--------------------------------------------------------------------------------------------------------------------

;----------------------------------------------------------------------------------------------
;
; FILTER ENVIRONMENT TO BE CONSIDERED TO SUROUNDING CONIC SECTION
;
; This subroutine ...
;
; ENTRY:
;
; EXIT:
;
;----------------------------------------------------------------------------------------------

to consider-environment
  set my.environment no-turtles
  let sun-status get-solar-status

  ; ASPATIAL WORLD
  ifelse ( model-structure = "aspatial" ) [
    set my.environment up-to-n-of 100 turtles

  ][ ; SPATIAL WORLD
    if ( sun-status = "DAY" ) [
      set my.environment turtles with [ not is-group? self and not hidden? ] in-cone ( 5 * day.perception.range ) ( 360 * day.perception.angle )
      if (female.fertility = "pregnant") [ set my.environment turtles with [ not is-group? self and (not hidden? or member? self [my-offspring] of myself ) ] in-cone ( 5 * day.perception.range ) ( 300 * day.perception.angle ) ] ] ; mothers can see gestatees

    if ( sun-status = "NIGHT" ) [
      set my.environment turtles with [ not is-group? self and not hidden? ] in-cone ( 5 * day.perception.range ) ( 360 * day.perception.angle ) ; set to night.perception.angle once genotype includes this
      if (female.fertility = "pregnant") [ set my.environment turtles with [ not is-group? self and (not hidden? or member? self [my-offspring] of myself ) ] in-cone ( 5 * day.perception.range ) ( 300 * day.perception.angle ) ] ] ; mothers can see gestatees

    if (life.history = "gestatee" ) [ set my.environment turtles with [ [meta-id] of self = [meta-id] of myself or meta-id = [mother-identity] of myself ]] ; gestatees can only see themselves, and their mothers
  ]
end

;----------------------------------------------------------------------------------------------
;
; MAKE-DECISIONS
;
; This subroutine ...
;
; ENTRY:  'input-chromosome' is the chromosome to be copied.
;         'mutation-chance-per-locus' defines the chance that a mutation will
;         occur at each allele loci.
;
; EXIT:   'mutate-chromosome' returns a copy of the chromosome with
;         modifications to a subset of the alleles.
;
;----------------------------------------------------------------------------------------------

to make-decisions
  set decision.vectors ( ifelse-value
    ( genotype-reader = "sta7us" ) [ sta7us-get-decisions my.environment ]
    ( genotype-reader = "g8tes" ) [ g8tes-get-decisions my.environment ]
    [ sta7us-get-decisions my.environment ] ) ; default
end

;----------------------------------------------------------------------------------------------
;
; ALLOCATE-ENERGY
;
; This subroutine ...
;
; ENTRY:  'input-chromosome' is the chromosome to be copied.
;         'mutation-chance-per-locus' defines the chance that a mutation will
;         occur at each allele loci.
;
; EXIT:   'mutate-chromosome' returns a copy of the chromosome with
;         modifications to a subset of the alleles.
;
;----------------------------------------------------------------------------------------------

to allocate-energy

  set energy.allocated []
  foreach decision.vectors [ vector ->

    let raw-value item 3 vector
    let energy-cost abs raw-value

    let passes-energy-check ifelse-value ( model-structure = "free-lunch" ) [ true ] [ energy.supply > energy-cost ] ; FREE LUNCH always passes energy check

    if ( not member? vector energy.allocated and passes-energy-check ) [
      set energy.allocated lput vector energy.allocated
      update-energy ( - energy-cost ) ]]

end

;----------------------------------------------------------------------------------------------
;
; COMPLETE LIST OF ACTIONS THAT HAVE BEEN ALLOCATED ENERGY
;
; This routine allows the caller to complete all actions that have been included in the
; 'alocated.energy list.
;
; ENTRY:
;
; EXIT:
;
;----------------------------------------------------------------------------------------------

to do-actions

  set completed.actions []
  foreach filter [ vector -> not member? vector completed.actions ] energy.allocated [ vector ->
    let target item 1 vector
    let action item 2 vector
    let cost item 3 vector

    ifelse ( ifelse-value ( target = nobody ) [ false ] [ distance target < ( size / 2 + [size] of target / 2 ) ] )

    [( ifelse

      action = "maintain-body" [ maintain-body cost ]
      action = "body-size" [ body-size cost ]
      action = "body-shade" [ body-shade cost ]
      action = "day-perception-range" [ day-perception-range cost ]
      action = "night-perception-range" [ night-perception-range cost ]
      action = "audio-perception-range" [ audio-perception-range cost ]
      action = "day-perception-angle" [ day-perception-angle cost ]
      action = "night-perception-angle" [ night-perception-angle cost ]
      action = "audio-perception-angle" [ audio-perception-angle cost ]
      action = "vocal-range" [ vocal-range cost ]
      action = "conception-chance" [ conception-chance cost ]
      action = "stomach-size" [ stomach-size cost ]
      action = "mutation-chance" [ mutation-chance cost ]
      action = "sex-ratio" [ sex-ratio cost ]
      action = "litter-size" [ litter-size cost ]
      action = "move-toward" [ move-toward target cost ]
      action = "move-away-from" [ move-toward target ( - cost ) ]
      action = "turn-right" [ turn-right cost ]
      action = "turn-left" [ turn-right ( - cost ) ]
      action = "go-forward" [ go-forward cost ]
      action = "set-heading" [ set-heading cost ]
      action = "hide" [ hide cost ]
      action = "signal-alpha-on" [ signal-alpha-on cost ]
      action = "signal-beta-on" [ signal-beta-on cost ]
      action = "signal-gamma-on" [ signal-gamma-on cost ]
      action = "check-infancy" [ check-infancy cost ]
      action = "check-birth" [ check-birth cost ]
      action = "check-juvenility" [ check-juvenility cost ]
      action = "check-weaning" [ check-weaning cost ]
      action = "check-adulthood" [ check-adulthood cost ]
      action = "check-senescence" [ check-senescence cost ]
      action = "supply-to" [ supply-to target cost ]
      action = "demand-from" [ demand-from target cost ]
      action = "eat" [ eat target cost ]
      action = "join" [ join target cost ]
      action = "leave" [ leave target cost ]
      action = "recruit" [ recruit target cost ]
      action = "kick-out" [ kick-out target cost ]
      action = "pick-up" [ pick-up target cost ]
      action = "put-down" [ put-down target cost  ]
      action = "cling-to" [ cling-to target cost ]
      action = "squirm-from" [ squirm-from target cost ]
      action = "help" [ help target cost ]
      action = "attack" [ attack target cost ]
      action = "mate-with" [ mate-with target cost ]

      [ ])]

    [ if ( target != nobody ) [ move-toward target ( abs cost ) ]]] ; do I want it absolute?

end

to-report get-action-cost-of [ target action-name ]
  let allocated-actions filter [ vector -> item 1 vector = target and item 2 vector = action-name and not member? vector completed.actions ] energy.allocated
  foreach allocated-actions [ vector -> complete-action ( item 1 vector ) ( item 2 vector ) ( item 3 vector ) "get-action-cost-of" ]
  report sum map [ vector -> item 3 vector ] filter [ vector -> item 1 vector = target and item 2 vector = action-name ] completed.actions
end

to complete-action [ target action cost outcome ]
  if ( not is-list? completed.actions ) [ set completed.actions [] ]
  set completed.actions lput ( list self target action cost outcome ) completed.actions
  if ( not is-list? population-actions ) [ set population-actions [] ]
  set population-actions lput ( list ticks self target action cost outcome ) population-actions
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
;
; CALLER: the anima1 doing the action
; COST: the energy to be used to do the action, can be negative
; TARGET: the receiver of the action ( INTER-ACTIONS have a target and INTRA-ACTIONS do not )
;
; The 'Info' tab contains a summary of possible actions.
;
;--------------------------------------------------------------------------------------------------------------------

to maintain-body [ cost ]
  complete-action self "maintain-body" cost 0
  living-chance cost
end

to living-chance [ cost ]
  let before-living-chance living.chance
  set living.chance get-updated-value living.chance cost
  complete-action self "living-chance" cost ( living.chance - before-living-chance )
end

to body-size [ cost ]
  let before-body-size body.size
  set body.size get-updated-value body.size cost
  complete-action self "body-size" cost ( body.size - before-body-size )
end

to body-shade [ cost ]
  let before-body-shade body.shade
  set body.shade get-updated-value body.shade cost
  complete-action self "body-shade" cost ( body.shade - before-body-shade )
end

to day-perception-range [ cost ]
  let before-day-perception-range day.perception.range
  set day.perception.range get-updated-value day.perception.range cost
  complete-action self "day-perception-range" cost ( day.perception.range - before-day-perception-range )
end

to night-perception-range [ cost ]
  let before-night-perception-range night.perception.range
  set night.perception.range get-updated-value night.perception.range cost
  complete-action self "night-perception-range" cost ( night.perception.range - before-night-perception-range )
end

to audio-perception-range [ cost ]
  let before-audio-perception-range audio.perception.range
  set audio.perception.range get-updated-value audio.perception.range cost
  complete-action self "audio-perception-range" cost ( audio.perception.range - before-audio-perception-range )
end

to day-perception-angle [ cost ]
  let before-day-perception-angle day.perception.angle
  set day.perception.angle get-updated-value day.perception.angle cost
  complete-action self "day-perception-angle" cost ( day.perception.angle - before-day-perception-angle )
end

to night-perception-angle [ cost ]
  let before-night-perception-angle night.perception.angle
  set night.perception.angle get-updated-value night.perception.angle cost
  complete-action self "night-perception-angle" cost ( night.perception.angle - before-night-perception-angle )
end

to audio-perception-angle [ cost ]
  let before-audio-perception-angle audio.perception.angle
  set audio.perception.angle get-updated-value audio.perception.angle cost
  complete-action self "audio-perception-angle" cost ( audio.perception.angle - before-audio-perception-angle )
end

to vocal-range [ cost ]
  let before-vocal-range vocal.range
  set vocal.range get-updated-value vocal.range cost
  complete-action self "vocal-range" cost ( vocal.range - before-vocal-range )
end

to conception-chance [ cost ]
  let before-conception-chance conception.chance
  set conception.chance get-updated-value conception.chance cost
  complete-action self "conception-chance" cost ( conception.chance - before-conception-chance )
end

to stomach-size [ cost ]
  let before-stomach-size stomach.size
  set stomach.size get-updated-value stomach.size cost
  complete-action self "stomach-size" cost ( stomach.size - before-stomach-size )
end

to mutation-chance [ cost ]
  let before-mutation-chance mutation.chance
  set mutation.chance get-updated-value mutation.chance cost
  complete-action self "mutation-chance" cost ( mutation.chance - before-mutation-chance )
end

to sex-ratio [ cost ]
  let before-sex-ratio sex.ratio
  set sex.ratio get-updated-value sex.ratio cost
  complete-action self "sex-ratio" cost ( sex.ratio - before-sex-ratio )
end

to litter-size [ cost ]
  let before-litter-size litter.size
  set litter.size get-updated-value litter.size cost
  complete-action self "litter-size" cost ( litter.size - before-litter-size )
end

;--------------------------------------------------------------------------------------------------------------------
; MOVEMENT
;--------------------------------------------------------------------------------------------------------------------

to move-toward [ target cost ]
  if (target != nobody ) [

    let ycor-difference ([ycor] of target - [ycor] of self )
    let xcor-difference ([xcor] of target - [xcor] of self )
    if ( not ( ycor-difference = 0 and xcor-difference = 0 ) ) [

      let angle atan ycor-difference xcor-difference
      if ( cost < 0 ) [ set angle angle - 180 ]
      set x.magnitude x.magnitude + (abs cost * sin angle)
      set y.magnitude y.magnitude + (abs cost * cos angle) ]

    set heading ifelse-value ( x.magnitude = 0 and y.magnitude = 0 ) [ heading ] [ ( atan y.magnitude x.magnitude ) ]
    complete-action target "move-toward" cost heading
  ]
end

to turn-right [ cost ]
  ifelse ( cost > 0 )
  [ right ( 360 * cost ) complete-action self "turn-right" cost heading ]
  [ left ( 360 * abs cost ) complete-action self "turn-left" cost heading ]
end

to go-forward [ cost ]
  if ( life.history != "gestatee" ) [

    set x.magnitude 0
    set y.magnitude 0

    if ( cost < 0 ) [ right 180 ]
    let sum-weight size
    foreach carried.items [ object -> set sum-weight sum-weight + [size] of object ]
    let travel-distance (size * (sqrt (( 2 * abs cost ) / sum-weight )) )
    forward travel-distance
    foreach carried.items [ object -> ifelse (object = nobody) [ set carried.items remove-item nobody carried.items ] [ ask object [ move-to myself ] ]]
    complete-action self "go-forward" cost heading
    ; track travel
    set distance-traveled distance-traveled + travel-distance
    if not member? patch-here cells-occupied [ set cells-occupied lput patch-here cells-occupied ]
  ]
end

to set-heading [ cost ]
  set heading cost * 360
  complete-action self "set-heading" cost heading
end

to hide [ cost ]
  if ( random-float 1.0 <= abs cost ) [
    ifelse ( cost > 0 )
    [ set hidden? true ]
    [ set hidden? false ]
    complete-action self "hide" cost hidden? ]
end

;--------------------------------------------------------------------------------------------------------------------
; SIGNALING
;--------------------------------------------------------------------------------------------------------------------

to signal-alpha-on [ cost ]
  set alpha.chance get-updated-value alpha.chance cost
  ifelse ( random-float 1.0 < alpha.chance ) [
    set alpha.signal true
    complete-action self "alpha-on" cost alpha.chance
  ][
    set alpha.signal false
    complete-action self "alpha-off" cost alpha.chance
  ]
end

to signal-beta-on [ cost ]
  set beta.chance get-updated-value beta.chance cost
  ifelse ( random-float 1.0 < beta.chance ) [
    set beta.signal true
    complete-action self "beta-on" cost beta.chance
  ][
    set beta.signal false
    complete-action self "beta-off" cost beta.chance
  ]
end

to signal-gamma-on [ cost ]
  set gamma.chance get-updated-value gamma.chance cost
  ifelse ( random-float 1.0 < gamma.chance ) [
    set gamma.signal true
    complete-action self "gamma-on" cost gamma.chance
  ][
    set gamma.signal false
    complete-action self "gamma-off" cost gamma.chance
  ]
end

;--------------------------------------------------------------------------------------------------------------------
; LIFE HISTORY
;--------------------------------------------------------------------------------------------------------------------

to check-infancy [ cost ]
  ifelse ( mother = nobody )
  [ set is.alive false ] ; gestatees die if mother is dead
  [ set infancy.chance get-updated-value infancy.chance cost
    if ( life.history = "gestatee" and random-float 1.0 < infancy.chance ) [
      set mother-initiated-birth false
      complete-action self "check-infancy" cost infancy.chance
      ask mother [ give-birth ]
    ]
  ]
end

to check-birth [ cost ]
  set birthing.chance get-updated-value birthing.chance cost
  if ( female.fertility = "pregnant" and random-float 1.0 < birthing.chance ) [
    complete-action self "check-birth" cost birthing.chance
    give-birth
  ]
end

to give-birth
  if ( female.fertility = "pregnant" ) [
    set female.fertility "lactating"
    complete-action self "give-birth" 0 0
    ask my-offspring with [ life.history = "gestatee" ] [ update-to-infant ]
    set birthing.chance 0
  ]
end

to update-to-infant
  set life.history "infant"
  set female.fertility " "
  set hidden? false
  set ticks-at-birth ticks
  complete-action self "update-to-infant" 0 0
  ask current-group [ set total-birth-count total-birth-count + 1 ]
end

to check-juvenility [ cost ]
  set juvenility.chance get-updated-value juvenility.chance cost
  if ( life.history = "infant" and random-float 1.0 < juvenility.chance ) [
    set mother-initiated-weaning false
    complete-action self "check-juvenility" cost juvenility.chance
    ifelse ( mother = nobody )
    [ update-to-juvenile ]
    [ ask mother [ wean-offspring ]]
  ]
end

to check-weaning [ cost ]
  set weaning.chance get-updated-value weaning.chance cost
  if ( female.fertility = "lactating" and random-float 1.0 < weaning.chance ) [
    complete-action self "check-weaning" cost weaning.chance
    wean-offspring
  ]
end

to wean-offspring
  if ( female.fertility = "lactating" ) [
    set female.fertility "cycling"
    complete-action self "wean-offspring" 0 0
    ask my-offspring with [ life.history = "infant" ] [ update-to-juvenile ]
    set weaning.chance 0
  ]
end

to update-to-juvenile
  set life.history "juvenile"
  set female.fertility " "
  set ticks-at-weaning ticks
  complete-action self "update-to-juvenile" 0 0
end

to check-adulthood [ cost ]
  set adulthood.chance get-updated-value adulthood.chance cost
  if ( life.history = "juvenile" and random-float 1.0 < adulthood.chance ) [
    complete-action self "check-adulthood" cost adulthood.chance
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
  set adult-stomach-size stomach.size
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
  complete-action self "update-to-adult" 0 0
end

to check-senescence [ cost ]
  set senescency.chance get-updated-value senescency.chance cost
  if ( life.history = "adult" and random-float 1.0 < senescency.chance ) [
    complete-action self "check-senescence" cost senescency.chance
    update-to-senescent
  ]
end

to update-to-senescent
  set life.history "senescent"
  set female.fertility " "
  ; all dependent offspring are made independent by this process
  ask my-offspring with [ life.history = "gestatee" ] [ update-to-infant ]
  ask my-offspring with [ life.history = "infant" ] [ update-to-juvenile ]
  set ticks-at-senescence ticks
  complete-action self "update-to-senescent" 0 0
end

;--------------------------------------------------------------------------------------------------------------------
; ENERGY
;--------------------------------------------------------------------------------------------------------------------

to supply-to [ target cost ]
  if ( target != self and is-anima1? target and ( female.fertility = "lactating" or female.fertility = "pregnant" ) ) [
    let target-cost get-action-cost-of target "demand-from"
    let net-cost ( cost + target-cost )
    ifelse ( net-cost > 0 )
    [ complete-action target "supply-to" cost net-cost
      ask target [ receive-from myself net-cost ] ]
    [ complete-action target "supply-to" cost 0 ]
  ]
end

to demand-from [ target cost ]
  if ( target != self and is-anima1? target and ( life.history = "gestatee" or life.history = "infant" ) ) [
    let target-supply-cost get-action-cost-of target "supply-to"
    let net-cost ( cost + target-supply-cost )
    ifelse ( net-cost > 0 )
    [ complete-action target "demand-from" cost net-cost
      receive-from target net-cost ]
    [ complete-action target "demand-from" cost 0 ]
  ]
end

to eat [ target cost ]
  if ( life.history = "juvenile" or life.history = "adult" or life.history = "senescent" and ( is-plant? target or is-anima1? target )) [
    complete-action target "eat" cost [breed] of target
    receive-from target cost
  ]
end

to receive-from [ target cost ]
  if ( cost > 0 and is-anima1? target or is-plant? target ) [
    let energy-wanted get-updated-value stomach.size cost
    let energy-received ifelse-value ( energy-wanted < [ energy.supply ] of target ) [ energy-wanted ] [ [ energy.supply ] of target ]
    update-energy energy-received
    ask target [ update-energy ( - energy-received ) ]
    complete-action target "receive-from" cost energy-received
  ]
end

;--------------------------------------------------------------------------------------------------------------------
; INTERACTIONS
;--------------------------------------------------------------------------------------------------------------------

to join [ target cost ]
  if ( is-anima1? target ) [
    complete-action target "join" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "join"
      let probability ( cost + target-cost ) / cost
      if ( random-float 1.0 <= probability ) [ join-group ([group.identity] of target) ]]]
end

to leave [ target cost ]
  if ( is-anima1? target ) [
    complete-action target "leave" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "leave"
      let probability ( cost + target-cost ) / cost
      if ( random-float 1.0 <= probability ) [ leave-group ]]]
end

to recruit [ target cost ]
  if ( is-anima1? target ) [
    complete-action target "recruit" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "recruit"
      let probability ( cost + target-cost ) / cost
      if ( random-float 1.0 <= probability ) [ ask target [ join-group [group.identity] of myself ]]]]
end

to kick-out [ target cost ]
  if ( is-anima1? target ) [
    complete-action target "kick-out" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "kick-out"
      let probability ( cost + target-cost ) / cost
      if ( random-float 1.0 <= probability ) [ ask target [ leave-group ]]]]
end

to join-group [ group-id ]
  if ( group.identity != group-id ) [
    set previous-group-id group.identity
    set group.identity group-id
    set group-transfers-list lput [meta-id] of current-group group-transfers-list
    complete-action self "join-group" 0 group-id ]
end

to leave-group
  set previous-group-id group.identity
  hatch-groups 1 [
    initialize-group
    set my-creator [meta-id] of myself
    ask myself [ set group.identity [meta-id] of myself ]]
  ask previous-group [ if not any? group-members [ die ]]
  complete-action self "leave-group" 0 0
end

to-report previous-group
  report ifelse-value (any? groups with [ meta-id = [previous-group-id] of myself ])
  [ one-of groups with [ meta-id = [previous-group-id] of myself ]]
  [ nobody ]
end

to initialize-group
  set hidden? true
  set age 0
  set color one-of base-colors
  set meta-id random 99999999
  move-to one-of patches
end

to pick-up [ target cost ]
  if ( is-anima1? target ) [
    complete-action target "pick-up" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "pick-up"
      let probability ( cost + target-cost ) / cost
      if ( random-float 1.0 <= probability ) [ carry target ]]]
end

to put-down [ target cost ]
  if ( is-anima1? target ) [
    complete-action target "put-down" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "put-down"
      let probability ( cost + target-cost ) / cost
      if ( random-float 1.0 <= probability ) [ drop target ]]]
end

to cling-to [ target cost ]
  if ( is-anima1? target ) [
    complete-action target "cling-to" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "cling-to"
      let probability ( cost + target-cost ) / cost
      if ( random-float 1.0 <= probability ) [ ask target [ carry myself ] ]]]
end

to squirm-from [ target cost ]
  if ( is-anima1? target ) [
    complete-action target "squirm-from" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "squirm-from"
      let probability ( cost + target-cost ) / cost
      if ( random-float 1.0 <= probability ) [ ask target [ drop myself ] ]]]
end

to carry [ target ]
  if ( not member? target [carried.items] of anima1s ) [
    ask anima1s with [ member? target carried.items ] [ set carried.items remove-item ( position target remove-duplicates carried.items ) remove-duplicates carried.items ]
    set carried.items lput target carried.items
    ask target [ move-to myself ]
    complete-action target "carry" 0 0 ]
end

to drop [ target ]
  if ( member? target carried.items ) [ set carried.items remove target remove-duplicates carried.items ]
  complete-action target "drop" 0 0
end

to help [ target cost ]
  if ( is-anima1? target ) [
    complete-action target "help" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "help"
      let cost-probability ( cost + target-cost ) / cost
      let size-probability ( size / ( size + [size] of target ) )
      let net-cost cost + target-cost
      if ( random-float 1.0 <= ( cost-probability * size-probability ) ) [ aid target net-cost ]]]
end

to attack [ target cost ]
  if ( is-anima1? target ) [
    complete-action target "attack" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "attack"
      let cost-probability ( cost + target-cost ) / cost
      let size-probability ( size / ( size + [size] of target ) )
      let net-cost cost + target-cost
      if ( random-float 1.0 <= ( cost-probability * size-probability ) ) [ harm target net-cost ]]]
end

to harm [ target cost ]
  ask target [ living-chance ( - cost ) ]
  complete-action target "harm" cost 0
end

to aid [ target cost ]
  ask target [ living-chance cost ]
  complete-action target "aid" cost 0
end

to mate-with [ target my-mate-cost ]
  if ( is-anima1? target and life.history = "adult" and ( biological.sex = "male" or ( biological.sex = "female" and female.fertility = "cycling" ))) [
    let target-mate-cost get-action-cost-of target "mate-with"
    ifelse ( target-mate-cost > 0 ) ; target has already paid cost to mate-with self
    [ let net-mate-cost ( my-mate-cost + target-mate-cost )
      if ( net-mate-cost > 0 and model-structure != "sower" ) [ ; SOWER calls conceive-with from an alternative source
        complete-action target "mate-with" my-mate-cost net-mate-cost
        set label "!" ; signals successful mating event
        ifelse ( biological.sex = "female" )
        [ conceive-with target net-mate-cost ]
        [ ask target [ conceive-with myself net-mate-cost ]]]]
    [ complete-action target "mate-with" my-mate-cost "" ]
  ]
end

to conceive-with [ target net-mate-cost ] ; FEMALE PROCEDURE
  if ( biological.sex = "female" and female.fertility = "cycling" and life.history = "adult" and [life.history] of target = "adult" and [biological.sex] of target = "male" ) [
    if random-float 1.0 < ( get-updated-value (mean (list conception.chance [conception.chance] of target)) net-mate-cost ) [
      let preferred-litter 10 ^ ( mean (list litter.size [litter.size] of target))
      let floor-litter floor preferred-litter
      let percent-litter preferred-litter - floor-litter
      let my-litter-size ifelse-value ( random-float 1.0 < percent-litter ) [ floor-litter + 1 ] [ floor-litter ]
      hatch-anima1s my-litter-size [ initialize-from-parents myself target ]
      set female.fertility "pregnant"
      complete-action target "conceive-with" net-mate-cost my-litter-size
    ]
  ]
end

to initialize-from-parents [ m f ]
  set meta-id random 9999999
  set hidden? true
  set is.alive true
  ask m [ set carried.items lput myself carried.items ]
  set biological.sex ifelse-value ( random-float 1.0 < mean (list [sex.ratio] of m [sex.ratio] of f) ) ["male"] ["female"]
  set shape ifelse-value ( biological.sex = "female" ) ["circle"] ["triangle"]
  set generation-number [generation-number] of m + 1
  set mother m
  ifelse ( model-structure = "no-evolution" )
  [ set chromosome.I [chromosome.I] of m
    set chromosome.II [chromosome.II] of m ]
  [ setup-chromosomes-from m f ]
  set group.identity [group.identity] of m
  set natal-group-id group.identity
  set mother-identity [meta-id] of m
  set father-identity [meta-id] of f
  set energy.supply 1
  set life.history "gestatee"
  set female.fertility " "
  set label-color black
  set age 0
  set body.size 0.01
  set body.shade 0
  set stomach.size 0.1
  set mutation.chance 0.1
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
  set day.perception.range 0.1
  set night.perception.range 0.1
  set audio.perception.range 0.1
  set day.perception.angle 0.1
  set night.perception.angle 0.1
  set audio.perception.angle 0.1
  set vocal.range 0.1
  set carried.items []
  set decision.vectors []
  set previous-group-id 0
  set ticks-at-conception ticks
  set ticks-at-birth 0
  set ticks-at-weaning 0
  set ticks-at-sexual-maturity 0
  set ticks-at-senescence 0
  set ticks-at-death 0
  set adult-living-chance 0
  set adult-body-size 0
  set adult-body-shade 0
  set adult-day-perception-angle 0
  set adult-night-perception-angle 0
  set adult-audio-perception-angle 0
  set adult-day-perception-range 0
  set adult-night-perception-range 0
  set adult-audio-perception-range 0
  set adult-vocal-range 0
  set adult-conception-chance 0
  set adult-stomach-size 0
  set adult-mutation-chance 0
  set adult-sex-ratio 0
  set adult-litter-size 0
  set adult-alpha-chance 0
  set adult-beta-chance 0
  set adult-gamma-chance 0
  set mother-initiated-birth true
  set mother-initiated-weaning true
  set my.environment no-turtles
  set distance-traveled 0
  set foraging-gains 0
  set foraging-cost 0
  set natal-group-size [group-size] of current-group
  set group-transfers-list []
  set matings-list []
  set conceptions-list []
  set cells-occupied []
  set infanticide-list []
  set previous-group-id 0
  if ( model-structure = "ideal-form" ) [ set-phenotype-to-ideal-form ]
end

to set-phenotype-to-ideal-form
  set body.size mean [body.size] of anima1s
  set body.shade mean [body.shade] of anima1s
  set day.perception.range mean [day.perception.range] of anima1s
  set night.perception.range mean [night.perception.range] of anima1s
  set audio.perception.range mean [audio.perception.range] of anima1s
  set stomach.size mean [stomach.size] of anima1s
  set mutation.chance mean [mutation.chance] of anima1s
  set sex.ratio mean [sex.ratio] of anima1s
  set litter.size mean [litter.size] of anima1s
  set conception.chance mean [conception.chance] of anima1s
  set birthing.chance mean [birthing.chance] of anima1s
  set weaning.chance mean [weaning.chance] of anima1s
  set infancy.chance mean [infancy.chance] of anima1s
  set juvenility.chance mean [juvenility.chance] of anima1s
  set adulthood.chance mean [adulthood.chance] of anima1s
  set senescency.chance mean [senescency.chance] of anima1s
  set living.chance mean [living.chance] of anima1s
  set alpha.chance mean [alpha.chance] of anima1s
  set beta.chance mean [beta.chance] of anima1s
  set gamma.chance mean [gamma.chance] of anima1s
  set day.perception.range mean [day.perception.range] of anima1s
  set night.perception.range mean [night.perception.range] of anima1s
  set audio.perception.range mean [audio.perception.range] of anima1s
  set day.perception.angle mean [day.perception.angle] of anima1s
  set night.perception.angle mean [night.perception.angle] of anima1s
  set audio.perception.angle mean [audio.perception.angle] of anima1s
  set vocal.range mean [vocal.range] of anima1s
  update-to-adult
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

  let rate-of-mutation mean (list [mutation.chance] of m [mutation.chance] of f)
  mutate-chromosomes 0.15 ; rate-of-mutation
end

to mutate-chromosomes [ rate-of-mutation ]
  set chromosome.I mutate-chromosome chromosome.I rate-of-mutation
  set chromosome.II mutate-chromosome chromosome.II rate-of-mutation
end

; --------------------------------------------------------------------------- ;
;
; MODIFY CHROMOSOME WITH NOVEL MUTATIONS AT RANDOM ALLELE LOCI
;
; This subroutine creates a copy of a given chromosome with a subset
; of its alleles modified according to the probability of a mutation occuring at
; each locus.
;
; ENTRY:  'input-chromosome' is the chromosome to be copied.
;         'mutation-chance-per-locus' defines the chance that a mutation will
;         occur at each allele loci.
;
; EXIT:   'mutate-chromosome' returns a copy of the chromosome with
;         modifications to a subset of the alleles.
;
; --------------------------------------------------------------------------- ;

to-report mutate-chromosome [ input-chromosome  mutation-chance-per-locus ]

  let ouput-chromosome []
  foreach input-chromosome [ allele ->
    let updated-alleles (list allele)

    if ( first allele = true and random-float 1.0 < mutation-chance-per-locus ) [

      let choice random 5

      (ifelse

        ; delete allele
        ( choice = 0 ) [
          set updated-alleles [] ] ; redundant but complete

        ; duplicate allele
        ( choice = 1 ) [
          set updated-alleles (list allele allele) ]

        ; mutate allele
        ( choice >= 2 ) [
          let new-allele []
          let random-index random length allele
          let index 0
          foreach allele [ codon ->
            ( ifelse
              ( choice = 2 and random-index = index and model-structure != "uninvadable" ) [ set new-allele lput get-mutation codon new-allele ] ; mutate codon
              ( choice = 3 and random-index = index ) [ repeat 2 [ set new-allele lput codon new-allele ] ] ; duplicate codon
              ( choice = 4 and random-index = index ) [  ] ; delete codon
              [ set new-allele lput codon new-allele ])
            set index index + 1 ]
          set updated-alleles ( list new-allele ) ]

        [])]

    foreach updated-alleles [ allele-update ->
      set ouput-chromosome ifelse-value ( allele-update != [] ) [ lput allele-update ouput-chromosome ] [ ouput-chromosome ]]
  ]
  report ouput-chromosome
end

to-report get-mutation [ unmutated-codon ]
  report (ifelse-value
    ( is-number? unmutated-codon ) [ get-updated-value unmutated-codon ( one-of [ 1 -1 ] ) ] ; number mutations always result in slight increase or decrease of origional value
    [( ifelse-value
      ( genotype-reader = "sta7us" ) [ sta7us-get-mutation unmutated-codon ]
      ( genotype-reader = "g8tes" ) [ g8tes-get-mutation unmutated-codon ]
      [ sta7us-get-mutation unmutated-codon ] ) ]) ; default
end
@#$#@#$#@
GRAPHICS-WINDOW
8
85
711
789
-1
-1
34.75
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
setup-button
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
go-button
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
../results/
1
0
String

BUTTON
414
10
489
79
go once
go-button
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
720
106
967
260
plot
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" "if any? anima1s [\n\nif (useful-commands = \"age-histogram\") [\n  clear-plot\n  set-plot-x-range 0 max [age] of anima1s with [ is.alive ] + 1000\n  set-plot-y-range 0 10 ]\n  \nif (useful-commands = \"lotka-volterra\") [\n  let plts ( plant-quality * count plants / 100 )\n  let anls count anima1s\n  let max-value max (list plts anls )\n  set-plot-x-range 0 10\n  set-plot-y-range 0 ( max-value + 10 ) ]\n ]\n  \n"
PENS
"age" 1000.0 1 -16777216 true "" "if (useful-commands = \"age-histogram\") [ histogram [age] of anima1s with [ is.alive ] ]"
"plants" 1.0 0 -13840069 true "" "if (useful-commands != \"age-histogram\") [ plot ( mean [energy.supply] of plants ) ]"
"organisms" 1.0 0 -6459832 true "" "if (useful-commands != \"age-histogram\") [ plot ( count anima1s ) ]"

INPUTBOX
720
10
967
102
documentation-notes
NIL
1
0
String

MONITOR
20
98
97
143
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
save-button
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
155
1217
188
plant-minimum-neighbors
plant-minimum-neighbors
0
8
6.1
.1
1
NIL
HORIZONTAL

SLIDER
974
191
1217
224
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
974
82
1217
115
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
1000.0
10
1
ticks
HORIZONTAL

SLIDER
974
46
1217
79
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
get-solar-status
17
1
11

INPUTBOX
974
230
1097
299
population
NIL
1
0
String

INPUTBOX
974
304
1097
373
genotype
NIL
1
0
String

BUTTON
1102
230
1157
299
⟳
reset-population-button
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
1162
230
1217
263
⤒
export-population-button
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
1162
266
1217
299
⤓
import-population-button
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
328
908
373
useful-commands
useful-commands
"help-me" "--------" "lotka-volterra" "age-histogram" "metafile-report" "verify-code" "check-runtime" "simulation-report" "genotype-reader" "model-structure" "clear-plants" "setup-plants" "clear-population" "view-genotype" "view-decisions" "view-allocation" "view-actions" "add-allele" "delete-allele" "population-report"
12

BUTTON
912
328
967
373
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
1102
304
1157
373
⟳
reset-genotype-button
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
1162
340
1217
373
⤓
import-genotype-button
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
1162
304
1217
337
⤒
export-genotype-button
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
118
1217
151
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
output-results?
output-results?
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
1
1
-1000

INPUTBOX
720
264
967
324
command-input
no-plants
1
0
String (commands)

OUTPUT
720
380
1218
788
11

MONITOR
96
805
203
850
NIL
plant-patchiness
1
1
11

MONITOR
211
805
327
850
platn abundance
sum [ penergy.supply] of patches
1
1
11

@#$#@#$#@
# B3GET 1.1.0 INFORMATION

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
------ g8tes.nls
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
G8TES: a more complex genotype file reader.
VERIFICATION: the verification code for this model.

### NEW EXPERIMENT

If you want to start a new experiment and store information in a separate place, simply create a new folder in [data] and make sure to update the PATH-TO-EXPERIMENT with this new folder path. You must add a poulation file to this new folder (the default file popu1ation would be a fine choice), and update the POPULATION input to this population file name. You can also add a genotype file and update the GENOTYPE input, but this is not neccessary because the population file also contains the genotypes for every agent.

## THINGS TO NOTICE

This model is designed to explore the behavior space of virtual organisms who are subject to the same kinds of ecological constraints we observe in nature. In your simulation, what behaviors emerge in your population and why might these behaviors be part of a successful strategy? Additionally, you can observe population level dynamics like the lokta-volterra cycles of the organism and plant populations.

## THINGS TO TRY

The file system of B3GET allows the user to directly modify genotype and population files to many different configurations. The number of alleles of each chromosome can also be modified but beware that lengthy genotypes may cause lag in runtime. Additionally, you can explore how the environmental controls influence behavioral strategies over generations.

## HOW TO CITE

Cite this model:

Crouse, Kristin (2020). “B3GET” (Version 1.1.0). CoMSES Computational Model Library. Retrieved from: https://www.comses.net/codebases/6b10f629-7958-4b31-b489-d51c17d0f5b8/releases/1.1.0/

Peer-reviewed paper:

Crouse, K. N., Miller, C. M., & Wilson, M. L. (2019). New approaches to modeling primate socioecology: Does small female group size BEGET loyal males?. Journal of human evolution, 137, 102671.

## COPYRIGHT AND LICENSE

© 2020 K N Crouse

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

The actions listed above are athe range of possible actions that an agent can take. However, whether an agent performs these actions, how much effort they put into doing so, and who they target is up to their genotype. An indefinite number of genotype file configurations are possible, as long as they include the following: (1) each row represents one allele, (2) these alleles represent self-contained procedures that generate decision-vectors from considering the environment as input, and (3) each row contains a list of codons that can be altered during recombination and mutation. This version of B3GET comes with two genotype file extensions: sta7us and g8tes (beta version). Specific information about each file type can be found within those extension files.

## Phenotypes

When agents perform actions, for the most part this results in changes in the state variables of themselves or others. These states can be thought of as an organism's phenotype, the set of virtual 'organs' which emerges from a combination of an organism's genotype and its life experiences interacting with its environment.

### VISIBLE 'ORGANS'

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
  <experiment name="WORLD-A" repetitions="1" runMetricsEveryStep="true">
    <setup>setup

; give simulation-id specific configuration: sDOB17 means simulation of WORLD-D, Baboons seed population, run B (instead of A), plant-minimum-neighbors = 1 and plant-maximum-neighbors = 7
ifelse ( plant-minimum-neighbors &lt; plant-maximum-neighbors ) [
  set simulation-id ( word "s" (last behaviorspace-experiment-name) (first population) "A" plant-minimum-neighbors plant-maximum-neighbors )
][
  let min-holder plant-minimum-neighbors
  let max-holder plant-maximum-neighbors
  set plant-minimum-neighbors max-holder - 1
  set plant-maximum-neighbors min-holder
  set simulation-id ( word "s" (last behaviorspace-experiment-name) (first population) "B" plant-minimum-neighbors plant-maximum-neighbors )
]</setup>
    <go>go</go>
    <timeLimit steps="101"/>
    <exitCondition>not any? anima1s or median [generation-number] of anima1s &gt; 100</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-structure">
      <value value="&quot;baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;status&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterioration-rate">
      <value value="-0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-annual-cycle">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-daily-cycle">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-seasonality">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-quality">
      <value value="0.5"/>
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
      <value value="8"/>
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
      <value value="&quot;Chimpanzees&quot;"/>
      <value value="&quot;Geladas&quot;"/>
      <value value="&quot;Olives&quot;"/>
      <value value="&quot;Hamadryas&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="WORLD-TEST" repetitions="1" runMetricsEveryStep="true">
    <setup>clear-all

; give simulation-id specific configuration: sDOB17 means simulation of WORLD-D, Baboons seed population, run B (instead of A), plant-minimum-neighbors = 1 and plant-maximum-neighbors = 7
ifelse ( plant-minimum-neighbors &lt; plant-maximum-neighbors ) [
  set simulation-id ( word "s" (last behaviorspace-experiment-name) (first population) "A" plant-minimum-neighbors plant-maximum-neighbors )
][
  let min-holder plant-minimum-neighbors
  let max-holder plant-maximum-neighbors
  set plant-minimum-neighbors max-holder - 1
  set plant-maximum-neighbors min-holder
  set simulation-id ( word "s" (last behaviorspace-experiment-name) (first population) "B" plant-minimum-neighbors plant-maximum-neighbors )
]

setup simulation-id</setup>
    <go>go</go>
    <final>crt 1 [ record-simulation die ]</final>
    <timeLimit steps="1001"/>
    <exitCondition>not any? anima1s</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-structure">
      <value value="&quot;baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;status&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterioration-rate">
      <value value="-0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-annual-cycle">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-daily-cycle">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-seasonality">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-quality">
      <value value="0.5"/>
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
      <value value="8"/>
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
      <value value="&quot;Chimpanzees&quot;"/>
      <value value="&quot;Geladas&quot;"/>
      <value value="&quot;Olives&quot;"/>
      <value value="&quot;Hamadryas&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="WORLD-TEST1" repetitions="1" runMetricsEveryStep="true">
    <setup>clear-all

; give simulation-id specific configuration: sDOB17 means simulation of WORLD-D, Baboons seed population, run B (instead of A), plant-minimum-neighbors = 1 and plant-maximum-neighbors = 7
ifelse ( plant-minimum-neighbors &lt; plant-maximum-neighbors ) [
  set simulation-id ( word "s" (last behaviorspace-experiment-name) (first population) "A" plant-minimum-neighbors plant-maximum-neighbors )
][
  let min-holder plant-minimum-neighbors
  let max-holder plant-maximum-neighbors
  set plant-minimum-neighbors max-holder - 1
  set plant-maximum-neighbors min-holder
  set simulation-id ( word "s" (last behaviorspace-experiment-name) (first population) "B" plant-minimum-neighbors plant-maximum-neighbors )
]

setup simulation-id</setup>
    <go>go</go>
    <timeLimit steps="1001"/>
    <exitCondition>not any? anima1s</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-structure">
      <value value="&quot;baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;status&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterioration-rate">
      <value value="-0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-annual-cycle">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-daily-cycle">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-seasonality">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-quality">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-minimum-neighbors">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-maximum-neighbors">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="&quot;Chimpanzees&quot;"/>
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
