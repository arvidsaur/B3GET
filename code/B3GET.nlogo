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
  "extensions/commands.nls"
  "extensions/data.nls"
  "extensions/import-export.nls"
  "extensions/selection.nls"
  "extensions/verification.nls"
  "extensions/sta7us.nls"
  "extensions/results.nls"
]

breed [ anima1s anima1 ]

anima1s-own [

  meta.id

  ;PHENOTYPE VARIABLES: visible to all agents
  biological.sex
  life.history
  female.fertility
  group.identity
  is.alive
  yellow.signal
  red.signal
  blue.signal
  body.size
  body.shade
  is.resting

  identity.I
  identity.II
  carried.items

  ;PHENOTYPE VARIABLES: hidden to all agents but self
  living.chance
  energy.supply
  bite.capacity
  mutation.chance
  sex.ratio
  litter.size
  conception.chance
  visual.angle
  visual.range
  day.perception
  night.perception
  yellow.chance
  red.chance
  blue.chance
  birthing.chance
  weaning.chance
  infancy.chance
  juvenility.chance
  adulthood.chance
  x.magnitude
  y.magnitude

  chromosome.I
  chromosome.II

  my.environment
  decision.vectors
  actions.completed

  ;TRACKING VARIABLES: hidden to all agents
  age.in.ticks
  generation.number
  my.mother
  mother.identity
  father.identity
  natal.group.id
  natal.group.size
  ticks.at.conception
  ticks.at.birth
  ticks.at.weaning
  ticks.at.sexual.maturity
  ticks.at.death
  adult.living.chance
  adult.body.size
  adult.body.shade
  adult.energy.supply
  adult.bite.capacity
  adult.mutation.chance
  adult.sex.ratio
  adult.litter.size
  adult.conception.chance
  adult.visual.angle
  adult.visual.range
  adult.day.perception
  adult.night.perception
  adult.yellow.chance
  adult.red.chance
  adult.blue.chance
  distance.traveled
  cells.occupied
  mother.initiated.birth
  mother.initiated.weaning
  whole.related.help.cost
  half.related.help.cost
  fourth.related.help.cost
  eighth.related.help.cost
  foraging.gains
  total.energy.gains
  total.energy.cost
  matings.list
  conceptions.list
  group.transfers.list
  infanticide.list
]

patches-own [
  pmeta.id
  pterminal.energy
  penergy.supply ]

globals [
  model-version
  model-structure
  genotype-reader
  simulation-id
  deterioration-rate
  maximum-visual-range
  base-litter-size

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
  if ( simulation-id != 0 ) [ record-world ]
  setup
end

to go-button
  go
end

to save-button
  ifelse ( simulation-id = 0 )
  [ set simulation-id generate-simulation-id ]
  [ set observation-notes (word simulation-id ": " observation-notes )
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
  ask anima1s with [ read-from-string but-first genotype = meta.id ] [ save-genotype ]
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
  set deterioration-rate -0.01
  set maximum-visual-range 5
  set base-litter-size 10
  set model-structure []
end

to setup-patches
  ask patches [
    set pterminal.energy random-float plant-quality
    set pmeta.id random 9999999
    update-patch-color ]
  ;repeat 100 [ update-patches ask patches [ set penergy.supply penergy.supply + 10 / plant-annual-cycle if penergy.supply > pterminal.energy [ set penergy.supply pterminal.energy ] update-patch-color ] ]

end

to setup ; [ new-simulation-id ]
  ; save world file of old simulation before starting new simulation under following conditions
  ;if ( simulation-id != 0 and behaviorspace-run-number = 0 and output-results? = true ) [ record-world ]
  clear-all
  reset-ticks
  set simulation-id generate-simulation-id ; new-simulation-id
  setup-parameters
  import-population
  import-genotype
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
  ifelse ( member? "no-plants" model-structure )
  [ ask patches [ set pcolor brown + 1 ] ]
  [ update-patches ]

  ask patches [ set penergy.supply penergy.supply + 1 / plant-annual-cycle ]
  ask patches [ if penergy.supply > pterminal.energy [ set penergy.supply pterminal.energy ]]

  ; AGENT UPDATES
  ask anima1s [ set age.in.ticks age.in.ticks + 1 ] ; all individuals age at each time step
  ask anima1s [ deteriorate ] ; all individuals decay at every time step
  ask anima1s with [ is.alive ] [ update-appearance ]
  ask anima1s with [ not empty? carried.items ] [ foreach carried.items [ itm -> ask itm [ move-to myself ]]] ; update carried items to be with carrier

  ; AGENT MORTALITY
  ifelse ( member? "reaper" model-structure  )
  [ if (( count anima1s with [ is.alive ] - 100 ) > 0 ) [ ask n-of ( count anima1s with [ is.alive ] - 100 ) anima1s [ set-to-dead ]]
    ask anima1s with [ is.alive = false ] [ check-mortality ]] ; this organization ensures that 100 individuals stay alive at all times
  [ ask anima1s [ check-mortality ] ]

  ; AGENT REPRODUCTION
  if ( member? "stork" model-structure and count anima1s with [ is.alive ] < 100 ) ; random mating
  [ repeat ( 100 - count anima1s with [ is.alive ]) [
    if ( ( count anima1s with [ biological.sex = "male" and life.history = "adult" and is.alive ] > 0 )                                        ; if there is at least one adult male
      and ( count anima1s with [ biological.sex = "female" and life.history = "adult" and female.fertility = "cycling" and is.alive ] > 0 ) )  ; and one adult cycling female left in the simulation,
    [ ask one-of anima1s with [ biological.sex = "female" and life.history = "adult" and female.fertility = "cycling" and is.alive ]           ; randomly select one adult male and one cycling female,
      [ conceive-with ( one-of anima1s with [ biological.sex = "male" and life.history = "adult" and is.alive ] ) 999999 ]]]]                  ; and the female spontaneously conceives a new offspring from their alleles

  ; AGENT AGENCY
  ask anima1s with [ is.alive ] [ consider-environment ]
  ask anima1s with [ is.alive ] [ make-decisions ]
  ask anima1s with [ is.alive ] [ do-actions ]

  ; ARTIFICAL SELECTION
  if selection-on? [ artificial-selection ]

  ; SIMULATION OUTPUT
  if output-results? [ output-results ]
  ; prints out current status of the simulation every 100 timesteps
  if ( ticks > 0 and ceiling (ticks / 100) = (ticks / 100) and any? anima1s ) [
    let print-text (word "Simulation " simulation-id " is now at " precision (ticks / plant-annual-cycle) 3 " years, "
      precision sum [penergy.supply] of patches 3 " plant units, "
      precision mean [generation.number] of anima1s 3 " generations, and contains "
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
  let alphabet get-alphabet
  report ( word "s" random 99 one-of alphabet one-of alphabet one-of alphabet )
end

to-report get-alphabet
  report [ "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" ]
end

;--------------------------------------------------------------------------------------------------------------------
; PATCHES
;--------------------------------------------------------------------------------------------------------------------

to update-patches

  let season ( cos (( 360 / plant-annual-cycle ) * ticks))
  let density ( sum [penergy.supply] of patches ) / count patches

  ask patches [
    set pterminal.energy updated-plant-energy pterminal.energy ( sum [penergy.supply] of neighbors ) season density
    update-patch-color ]

end

to-report updated-plant-energy [ old-energy neighbor-energy season plant-density ]

  let energy-to-report old-energy

  let seasonal ( ( plant-seasonality * season + 1 ) / 2 )
  let a seasonal
  let b ( plant-minimum-neighbors + plant-maximum-neighbors ) / 2
  let c ( b - plant-minimum-neighbors )
  let x ( neighbor-energy / plant-quality )
  let probability-up ifelse-value ( c = 0 ) [ 0 ] [ ( a * e ^ ( - (( x - b ) ^ 2 ) / ( 2 * ( c ^ 2 ) )) ) ]
  let y ( plant-daily-cycle / plant-annual-cycle ) * ( ( plant-density ) * ( 2 * probability-up - 1 ) + ( seasonal - plant-density ) )
  set energy-to-report ( ( energy-to-report / plant-quality ) + ( random-float y ) * plant-quality ) * plant-quality

  if energy-to-report >= plant-quality [ set energy-to-report plant-quality ]
  if energy-to-report <= 0.000 [ set energy-to-report 0.000 ]

  report energy-to-report
end

to update-patch-color
  set pcolor scale-color green ( ( pterminal.energy + penergy.supply ) / 2 ) 1.5 -0.25
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

    ; FIRST DEATH: set anima1 to is.alive false
    [ set-to-dead ]]
end

to set-to-dead
  set is.alive false
  set ticks.at.death ticks
  set label "x"
end

to remove-from-simulation
  if ( ticks.at.death = 0 ) [ set ticks.at.death ticks ]
  ask anima1s with [ member? myself carried.items ] [ set carried.items remove myself remove-duplicates carried.items ]
  die
end

to update-appearance
  set size body.size
  set label " "
  set color round ( wrap-color group.identity + 5 - ( 10 ^ body.shade ))
  set shape get-shape
end

to-report get-shape
  let base_shape ifelse-value ( biological.sex = "male" ) [ "triangle" ] [ "circle" ]
  let eye_size ( ifelse-value ( visual.range < ( 1 / 3 ) ) [ "1" ] ( visual.range < ( 2 / 3 ) ) [ "2" ] [ "3" ] )
  let eye_spacing ( ifelse-value ( visual.angle < ( 1 / 3 ) ) [ "1" ] ( visual.angle < ( 2 / 3 ) ) [ "2" ] [ "3" ] )
  let current-perception ifelse-value ( get-solar-status = "DAY" ) [ day.perception ] [ night.perception ]
  let eye_acuity ( ifelse-value ( is.resting ) [ "1" ] ( current-perception > 0.5 ) [ "3" ] ( current-perception > 0 ) [ "2" ] [ "1" ])
  let a_on ifelse-value yellow.signal [ "a" ] [ "" ]
  let b_on ifelse-value red.signal [ "b" ] [ "" ]
  let c_on ifelse-value blue.signal [ "c" ] [ "" ]
  report ( word base_shape eye_size eye_spacing eye_acuity a_on b_on c_on )
end

to update-energy [ update ]
  set energy.supply energy.supply + update
  ifelse ( update > 0 )
  [ set total.energy.gains total.energy.gains + update ]
  [ set total.energy.cost total.energy.cost + abs update ]
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

to consider-environment
  set my.environment []
  let sun-status get-solar-status

  ; ASPATIAL WORLD
  ifelse ( member? "aspatial" model-structure )
  [ ask n-of 10 patches [ ask myself [ set my.environment lput myself my.environment ] ]]

  ; SPATIAL WORLD
  [ ask patches in-cone ( maximum-visual-range * visual.range ) ( 360 * visual.angle ) [ ask myself [ set my.environment lput myself my.environment ] ]]

  foreach my.environment [ p -> ask [anima1s-here] of p [ ; look at each patch in environment taken from above
    if ( is-anima1? self and [life.history] of myself != "gestatee" )
    ;[ if ( self = myself or self = my.mother) [ ask myself [ set my.environment lput myself my.environment ] ] ] ; gestatees can only see themselves and their mothers
    [ if ( not hidden? or self = myself ) [ ask myself [ set my.environment lput myself my.environment ] ]]]] ; non-gestatees can see self and all non-hidden others in environment

  if ( not member? "aspatial" model-structure ) [
    let complete-environment remove-duplicates my.environment
    let complete-count length complete-environment
    set my.environment up-to-n-of ( ( ifelse-value ( sun-status = "DAY" ) [ day.perception ] [ night.perception ] ) * complete-count ) complete-environment ]

  foreach carried.items [ c -> if ( [my.mother] of c = self ) [
    set my.environment lput c my.environment ; mothers can see their gestatees, infants and juveniles
    ask c [ set my.environment lput myself my.environment ] ]]  ; gestatees, infants and juveniles can see their mothers

  set my.environment remove-duplicates lput self my.environment ; self is always in environment
  set actions.completed []

end

to make-decisions

  ; Get decisions from selected genotype reader
  set decision.vectors ( ifelse-value
    ( genotype-reader = "sta7us" ) [ sta7us-get-decisions my.environment ]
    [ sta7us-get-decisions my.environment ] ) ; default

  ;REDUCE DECISIONS TO ONE PER TARGET-ACTION COMBO
  let reduced-decisions []

  foreach decision.vectors [ original-vector ->
    let original-target item 1 original-vector
    let original-action item 2 original-vector
    let original-weight item 3 original-vector
    let vector-doesnt-exist true

    let index 0
    foreach reduced-decisions [ reduced-vector ->
      let reduced-target item 1 reduced-vector
      let reduced-action item 2 reduced-vector
      let reduced-weight item 3 reduced-vector

      if ( reduced-target = original-target ) and ( reduced-action = original-action ) [
        set vector-doesnt-exist false
        let new-vector ( list self reduced-target reduced-action ( reduced-weight + original-weight ) false )
        set reduced-decisions remove-item index reduced-decisions
        set reduced-decisions lput new-vector reduced-decisions ]
      set index index + 1
    ]
    if vector-doesnt-exist [ set reduced-decisions lput ( list self original-target original-action original-weight false ) reduced-decisions ]
  ]

  set decision.vectors reduced-decisions

end

to do-actions

  foreach decision.vectors [ vector ->
    let done item 4 vector

    if ( not done and check-energy vector ) [
      let target item 1 vector
      let action item 2 vector
      let cost item 3 vector ; must put check here to only call if cost not 0

      if ( is.resting = false )

      [ ( ifelse  ; these actions can only be performed when not resting

        action = "move-toward" [ move-toward target cost ]
        action = "move-away-from" [ move-toward target ( - cost ) ]
        action = "turn-right" [ turn-right cost ]
        action = "turn-left" [ turn-right ( - cost ) ]
        action = "go-forward" [ go-forward cost ]
        action = "set-heading" [ set-heading cost ]
        action = "set-heading-random" [ set-heading-random cost ]
        action = "hide" [ hide cost ]
        action = "supply-to" [ if ( check-distance target ) [ supply-to target cost ]]
        action = "demand-from" [ if ( check-distance target ) [ demand-from target cost ]]
        action = "eat" [ if ( check-distance target ) [ eat target cost ]]
        action = "join" [ join target cost ]
        action = "leave" [ leave target cost ]
        action = "recruit" [ recruit target cost ]
        action = "expel" [ expel target cost ]
        action = "pick-up" [ if ( check-distance target ) [ pick-up target cost ]]
        action = "put-down" [ if ( check-distance target ) [ put-down target cost  ]]
        action = "cling-to" [ if ( check-distance target ) [ cling-to target cost ]]
        action = "squirm-from" [ if ( check-distance target ) [ squirm-from target cost ]]
        action = "help" [ if ( check-distance target ) [ help target cost ]]
        action = "hurt" [ if ( check-distance target ) [ hurt target cost ]]
        action = "mate-with" [ if ( check-distance target ) [ mate-with target cost ]]
        [])]

      ( ifelse ; these actions can be performed resting or not resting
        action = "rest" [ rest cost ]
        action = "living-chance" [ living-chance cost ] ; should these have distance checkers too?
        action = "body-size" [ body-size cost ]
        action = "body-shade" [ body-shade cost ]
        action = "visual-range" [ visual-range cost ]
        action = "visual-angle" [ visual-angle cost ]
        action = "day-perception" [ day-perception cost ]
        action = "night-perception" [ night-perception cost ]
        action = "conception-chance" [ conception-chance cost ]
        action = "bite-capacity" [ bite-capacity cost ]
        action = "mutation-chance" [ mutation-chance cost ]
        action = "sex-ratio" [ sex-ratio cost ]
        action = "litter-size" [ litter-size cost ]
        action = "yellow-signal-on" [ yellow-signal-on cost ]
        action = "red-signal-on" [ red-signal-on cost ]
        action = "blue-signal-on" [ blue-signal-on cost ]
        action = "check-infancy" [ check-infancy cost ]
        action = "check-birth" [ check-birth cost ]
        action = "check-juvenility" [ check-juvenility cost ]
        action = "check-weaning" [ check-weaning cost ]
        action = "check-adulthood" [ check-adulthood cost ]
        [])
    ]
  ]
end

to-report check-distance [ target ]
  report ( ifelse-value
    ( member? "aspatial" model-structure ) [ true ]
    ( target = nobody ) [ false ]
    [ distance target < ( size / 2 + ( ifelse-value ( is-patch? target ) [ 1 ] [[size] of target / 2 ])) ])
end

to-report check-energy [ vector ]
  let cost item 3 vector
  let passes-energy-check ifelse-value ( member? "free-lunch" model-structure ) [ true ] [ energy.supply > abs cost ] ; FREE LUNCH always passes energy check

  if ( passes-energy-check ) [
    update-energy ( - abs cost )
    let new-vector lput true but-last vector
    let vector-index position vector decision.vectors
    if ( is-number? vector-index ) [
      set decision.vectors remove-item vector-index decision.vectors
      set decision.vectors lput new-vector decision.vectors ]]

  report passes-energy-check
end

to-report get-action-cost-of [ target action-name ]

  ; ask target to complete actions from incomplete related decisions
  let not-done-decisions filter [ vector -> item 1 vector = self and item 2 vector = action-name and item 4 vector = false ] [decision.vectors] of target
  ask target [
    foreach not-done-decisions [ vector ->
      if ( check-energy vector ) [
        complete-action ( item 1 vector ) ( item 2 vector ) ( item 3 vector ) "get-action-cost-of" ]]]

  ; get summation of target related cost and report
  let action-cost sum map [ vector -> item 3 vector ] filter [ vector -> item 1 vector = self and item 2 vector = action-name ] [actions.completed] of target
  report action-cost

end

to complete-action [ target action cost outcome ]
  set actions.completed lput ( list self target action cost outcome ) actions.completed
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

to visual-range [ cost ]
  let before-visual-range visual.range
  set visual.range get-updated-value visual.range cost
  complete-action self "visual-range" cost ( visual.range - before-visual-range )
end

to visual-angle [ cost ]
  let before-visual-angle visual.angle
  set visual.angle get-updated-value visual.angle cost
  complete-action self "visual-angle" cost ( visual.angle - before-visual-angle )
end

to day-perception [ cost ]
  let before-day-perception day.perception
  set day.perception get-updated-value day.perception cost
  complete-action self "day-perception" cost ( day.perception - before-day-perception )
end

to night-perception [ cost ]
  let before-night-perception night.perception
  set night.perception get-updated-value night.perception cost
  complete-action self "night-perception" cost ( night.perception - before-night-perception )
end

to conception-chance [ cost ]
  let before-conception-chance conception.chance
  set conception.chance get-updated-value conception.chance cost
  complete-action self "conception-chance" cost ( conception.chance - before-conception-chance )
end

to bite-capacity [ cost ]
  let before-bite-capacity bite.capacity
  set bite.capacity get-updated-value bite.capacity cost
  complete-action self "bite-capacity" cost ( bite.capacity - before-bite-capacity )
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

    ; deteremine x and y coordinate difference to target
    let ycor-difference ( ( ifelse-value ( is-patch? target ) [ [pycor] of target ] [ [ycor] of target ] ) - [ycor] of self )
    let xcor-difference ( ( ifelse-value ( is-patch? target ) [ [pxcor] of target ] [ [xcor] of target ] ) - [xcor] of self )

    if ( not ( ycor-difference = 0 and xcor-difference = 0 ) ) [

      ; calculate angle to target
      let angle atan xcor-difference ycor-difference
      if ( cost < 0 ) [ set angle angle - 180 ]

      ; set magnitudes based on cost
      set x.magnitude x.magnitude + (abs cost * sin angle)
      set y.magnitude y.magnitude + (abs cost * cos angle) ]

    ; set heading based on magnitudes
    set heading ifelse-value ( x.magnitude = 0 and y.magnitude = 0 ) [ heading ] [ ( atan x.magnitude y.magnitude ) ]
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

    if ( cost < 0 ) [ right 180 ]
    let sum-weight size
    foreach carried.items [ object -> set sum-weight sum-weight + [size] of object ]
    let travel-distance (size * (sqrt (( 2 * abs cost ) / sum-weight )) )
    forward travel-distance
    complete-action self "go-forward" cost heading

    set x.magnitude 0
    set y.magnitude 0

    ; track travel
    set distance.traveled distance.traveled + travel-distance
    if not member? patch-here cells.occupied [ set cells.occupied lput patch-here cells.occupied ]
  ]
end

to set-heading [ cost ]
  set heading cost * 360
  complete-action self "set-heading" cost heading
end

to set-heading-random [ cost ]
  set heading heading + cost * ( random 360 ) - cost * ( random 360 )
end

to hide [ cost ]
  if ( random-float 1.0 <= abs cost ) [
    ifelse ( cost > 0 )
    [ set hidden? true ]
    [ set hidden? false ]
    complete-action self "hide" cost hidden? ]
end

to rest [ cost ]
  if ( cost > 0 ) [ set is.resting true ]
  if ( cost < 0 ) [ set is.resting false ]
end

;--------------------------------------------------------------------------------------------------------------------
; SIGNALING
;--------------------------------------------------------------------------------------------------------------------

to yellow-signal-on [ cost ]
  set yellow.chance get-updated-value yellow.chance cost
  ifelse ( random-float 1.0 < yellow.chance ) [
    set yellow.signal true
    complete-action self "yellow-signal-on" cost yellow.chance
  ][
    set yellow.signal false
    complete-action self "yellow-signal-off" cost yellow.chance
  ]
end

to red-signal-on [ cost ]
  set red.chance get-updated-value red.chance cost
  ifelse ( random-float 1.0 < red.chance ) [
    set red.signal true
    complete-action self "red-signal-on" cost red.chance
  ][
    set red.signal false
    complete-action self "red-signal-off" cost red.chance
  ]
end

to blue-signal-on [ cost ]
  set blue.chance get-updated-value blue.chance cost
  ifelse ( random-float 1.0 < blue.chance ) [
    set blue.signal true
    complete-action self "blue-signal-on" cost blue.chance
  ][
    set blue.signal false
    complete-action self "blue-signal-off" cost blue.chance
  ]
end

;--------------------------------------------------------------------------------------------------------------------
; LIFE HISTORY
;--------------------------------------------------------------------------------------------------------------------

to check-infancy [ cost ]
  ifelse ( my.mother = nobody )
  [ set is.alive false ] ; gestatees die if mother is dead
  [ set infancy.chance get-updated-value infancy.chance cost
    if ( life.history = "gestatee" and random-float 1.0 < infancy.chance ) [
      set mother.initiated.birth false
      complete-action self "check-infancy" cost infancy.chance
      ask my.mother [ give-birth ]
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
  set ticks.at.birth ticks
  set label "i"
  complete-action self "update-to-infant" 0 0
end

to check-juvenility [ cost ]
  set juvenility.chance get-updated-value juvenility.chance cost
  if ( life.history = "infant" and random-float 1.0 < juvenility.chance ) [
    set mother.initiated.weaning false
    complete-action self "check-juvenility" cost juvenility.chance
    ifelse ( my.mother = nobody )
    [ update-to-juvenile ]
    [ ask my.mother [ wean-offspring ]]
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
  set ticks.at.weaning ticks
  complete-action self "update-to-juvenile" 0 0
  let my-meta-id meta.id
  set label "j"
  ask anima1s with [ member? my-meta-id infanticide.list ] [ set infanticide.list remove my-meta-id remove-duplicates infanticide.list ]
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
  set ticks.at.sexual.maturity ticks
  set adult.living.chance living.chance
  set adult.body.size body.size
  set adult.body.shade body.shade
  set adult.energy.supply energy.supply
  set adult.bite.capacity bite.capacity
  set adult.mutation.chance mutation.chance
  set adult.sex.ratio sex.ratio
  set adult.litter.size litter.size
  set adult.conception.chance conception.chance
  set adult.visual.angle visual.angle
  set adult.visual.range visual.range
  set adult.day.perception day.perception
  set adult.night.perception night.perception
  set adult.yellow.chance yellow.chance
  set adult.red.chance red.chance
  set adult.blue.chance blue.chance
  set label "a"
  complete-action self "update-to-adult" 0 0
end

;--------------------------------------------------------------------------------------------------------------------
; ENERGY
;--------------------------------------------------------------------------------------------------------------------

to supply-to [ target cost ]
  if ( target != self and is-anima1? target and [ is.alive ] of target = true and ( female.fertility = "lactating" or female.fertility = "pregnant" ) ) [
    let target-cost get-action-cost-of target "demand-from"
    let net-cost ( cost + target-cost )
    ifelse ( net-cost > 0 )
    [ complete-action target "supply-to" cost net-cost
      ask target [ receive-from myself net-cost ] ]
    [ complete-action target "supply-to" cost 0 ]
  ]
end

to demand-from [ target cost ]
  if ( target != self and is-anima1? target and [ is.alive ] of target = true and ( life.history = "gestatee" or life.history = "infant" ) ) [
    let target-supply-cost get-action-cost-of target "supply-to"
    let net-cost ( cost + target-supply-cost )
    ifelse ( net-cost > 0 )
    [ complete-action target "demand-from" cost net-cost
      receive-from target net-cost ]
    [ complete-action target "demand-from" cost 0 ]
  ]
end

to eat [ target cost ]
  if ( life.history = "juvenile" or life.history = "adult" or life.history = "senescent" and ( is-patch? target or is-anima1? target )) [
    complete-action target "eat" cost ""
    receive-from target cost
  ]
end

to receive-from [ target cost ]
  if ( cost > 0 and is-anima1? target or is-patch? target ) [
    let energy-wanted get-updated-value bite.capacity cost
    let energy-supply ifelse-value ( is-patch? target ) [ [ penergy.supply ] of target ] [ [ energy.supply ] of target ]
    let energy-received ifelse-value ( energy-wanted < energy-supply ) [ energy-wanted ] [ energy-supply ]
    update-energy energy-received
    ifelse ( is-patch? target )
    [ ask target [ set penergy.supply penergy.supply - energy-received ]]
    [ ask target [ update-energy ( - energy-received ) ]]
    complete-action target "receive-from" cost energy-received
    if ( life.history = "juvenile" or life.history = "adult" or life.history = "senescent" ) [ set foraging.gains foraging.gains + energy-received ]
  ]
end

;--------------------------------------------------------------------------------------------------------------------
; INTERACTIONS
;--------------------------------------------------------------------------------------------------------------------

to join [ target cost ]
  if ( is-anima1? target and [ is.alive ] of target = true ) [
    complete-action target "join" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "join"
      let probability ( cost + target-cost ) / cost
      if ( random-float 1.0 <= probability ) [ join-group ([group.identity] of target) ]]]
end

to leave [ target cost ]
  if ( is-anima1? target and [ is.alive ] of target = true ) [
    complete-action target "leave" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "leave"
      let probability ( cost + target-cost ) / cost
      if ( random-float 1.0 <= probability ) [ leave-group ]]]
end

to recruit [ target cost ]
  if ( is-anima1? target and [ is.alive ] of target = true ) [
    complete-action target "recruit" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "recruit"
      let probability ( cost + target-cost ) / cost
      if ( random-float 1.0 <= probability ) [ ask target [ join-group [group.identity] of myself ]]]]
end

to expel [ target cost ]
  if ( is-anima1? target and [ is.alive ] of target = true ) [
    complete-action target "expel" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "expel"
      let probability ( cost + target-cost ) / cost
      if ( random-float 1.0 <= probability ) [ ask target [ leave-group ]]]]
end

to join-group [ group-id ]
  if ( group.identity != group-id ) [
    set group.identity group-id
    set label "="
    set group.transfers.list lput group.identity group.transfers.list
    complete-action self "join-group" 0 group-id ]
end

to leave-group
  set group.identity ( random 10000 * 140 + one-of base-colors )
  set label "~"
  complete-action self "leave-group" 0 0
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
    set label "^"
    complete-action target "carry" 0 0 ]
end

to drop [ target ]
  if ( member? target carried.items ) [
    set carried.items remove target remove-duplicates carried.items
    set label "*"
    complete-action target "drop" 0 0 ]
end

to help [ target cost ]
  if ( is-anima1? target and [ is.alive ] of target = true ) [
    complete-action target "help" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "help"
      let cost-probability ( cost + target-cost ) / cost
      let size-probability ( size / ( size + [size] of target ) )
      let net-cost cost + target-cost
      if ( random-float 1.0 <= ( cost-probability * size-probability ) ) [ aid target net-cost ]]]
end

to aid [ target cost ]
  if ( is-anima1? target and [ is.alive ] of target = true ) [
    complete-action target "aid" cost 0
    ask target [ living-chance cost ]
    set label "+"
    ; recording kin selection
    let relatedness-with-target relatedness-with target
    if ( relatedness-with-target > 0.75 ) [ set whole.related.help.cost whole.related.help.cost + cost ]
    if ( relatedness-with-target <= 0.75 and relatedness-with-target > 0.375 ) [ set half.related.help.cost half.related.help.cost + cost ]
    if ( relatedness-with-target <= 0.375 and relatedness-with-target > 0.1875 ) [ set fourth.related.help.cost fourth.related.help.cost + cost ]
    if ( relatedness-with-target <= 0.1875  ) [ set eighth.related.help.cost eighth.related.help.cost + cost ]
  ]
end

to hurt [ target cost ]
  if ( is-anima1? target and [ is.alive ] of target = true ) [
    complete-action target "hurt" cost 0
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "hurt"
      let cost-probability ( cost + target-cost ) / cost
      let size-probability ( size / ( size + [size] of target ) )
      let net-cost cost + target-cost
      if ( random-float 1.0 <= ( cost-probability * size-probability ) ) [ harm target net-cost ]]]
end

to harm [ target cost ]
  if ( is-anima1? target and [ is.alive ] of target = true ) [
    complete-action target "harm" cost 0
    ask target [ living-chance ( - cost ) ]
    set label "-"
    if ( [ life.history ] of target = "infant" ) [ set infanticide.list lput [meta.id] of target infanticide.list ]
  ]
end

to mate-with [ target cost ]
  if ( is-anima1? target and [ is.alive ] of target = true and life.history = "adult" and ( biological.sex = "male" or ( biological.sex = "female" and female.fertility = "cycling" ))) [
    let target-cost get-action-cost-of target "mate-with"
    ifelse ( target-cost >= 0 ) ; target has already paid cost to mate-with self
    [ let net-cost ( cost + target-cost )
      if ( net-cost > 0 and not member? "stork" model-structure ) [ ; STORK calls conceive-with from an alternative source
        complete-action target "mate-with" cost net-cost
        set matings.list lput [meta.id] of target matings.list
        ask target [ set matings.list lput [meta.id] of myself matings.list ]
        ifelse ( biological.sex = "female" )
        [ ask target [ set label "!" ]
          conceive-with target net-cost ]
        [ set label "!" ; signals successful mating event
          ask target [ conceive-with myself net-cost ]]]]
    [ complete-action target "mate-with" cost "" ]
  ]
end

to conceive-with [ target net-mate-cost ] ; FEMALE PROCEDURE
  if ( biological.sex = "female" and female.fertility = "cycling" and life.history = "adult" and [life.history] of target = "adult" and [biological.sex] of target = "male" and [ is.alive ] of target = true ) [
    if random-float 1.0 < ( get-updated-value (mean (list conception.chance [conception.chance] of target)) net-mate-cost ) [
      let preferred-litter base-litter-size ^ ( mean (list litter.size [litter.size] of target))
      let floor-litter floor preferred-litter
      let percent-litter preferred-litter - floor-litter
      let my-litter-size ifelse-value ( random-float 1.0 < percent-litter ) [ floor-litter + 1 ] [ floor-litter ]
      hatch-anima1s my-litter-size [ initialize-from-parents myself target ]
      set female.fertility "pregnant"
      complete-action target "conceive-with" net-mate-cost my-litter-size
      set conceptions.list lput [meta.id] of target conceptions.list
      ask target [ set conceptions.list lput [meta.id] of myself conceptions.list ]
    ]
  ]
end

to initialize-from-parents [ m f ]
  set meta.id random 9999999
  set is.alive true
  set biological.sex ifelse-value ( random-float 1.0 < mean (list [sex.ratio] of m [sex.ratio] of f) ) ["male"] ["female"]
  set generation.number ( max (list [generation.number] of m [generation.number] of f ) + 1 )
  set my.mother m
  ifelse ( member? "no-evolution" model-structure )
  [ set chromosome.I [chromosome.I] of m
    set chromosome.II [chromosome.II] of m ]
  [ setup-chromosomes-from m f ]
  set group.identity [group.identity] of m
  set natal.group.id group.identity
  set mother.identity [meta.id] of m
  set father.identity [meta.id] of f
  set energy.supply 0
  set life.history "gestatee"
  set female.fertility " "
  set label-color white
  set age.in.ticks 0
  set carried.items []
  set my.environment []
  set decision.vectors []
  set actions.completed []
  set ticks.at.conception ticks
  set ticks.at.birth 0
  set ticks.at.weaning 0
  set ticks.at.sexual.maturity 0
  set ticks.at.death 0
  set adult.living.chance 0
  set adult.body.size 0
  set adult.body.shade 0
  set adult.energy.supply 0
  set adult.bite.capacity 0
  set adult.mutation.chance 0
  set adult.sex.ratio 0
  set adult.litter.size 0
  set adult.conception.chance 0
  set adult.visual.angle 0
  set adult.visual.range 0
  set adult.day.perception 0
  set adult.night.perception 0
  set adult.yellow.chance 0
  set adult.red.chance 0
  set adult.blue.chance 0
  set mother.initiated.birth true
  set mother.initiated.weaning true
  set whole.related.help.cost 0
  set half.related.help.cost 0
  set fourth.related.help.cost 0
  set eighth.related.help.cost 0
  set foraging.gains 0
  set total.energy.gains 0
  set total.energy.cost 0
  set distance.traveled 0
  set cells.occupied []
  set natal.group.size count anima1s with [ group.identity = [group.identity] of myself ]
  set matings.list []
  set conceptions.list []
  set group.transfers.list []
  set infanticide.list []
  ifelse ( member? "ideal-form" model-structure ) [ set-phenotype-to-ideal-form ] [ set-phenotype-to-initialized-form ]
end

to set-phenotype-to-initialized-form
  ask my.mother [ set carried.items lput myself carried.items ]
  set hidden? true
  set body.size 0.01
  set body.shade 0
  set bite.capacity 0.1
  set mutation.chance 0.1
  set sex.ratio 0.5
  set litter.size 0
  set conception.chance 0
  set birthing.chance 0
  set weaning.chance 0
  set infancy.chance 0
  set juvenility.chance 0
  set adulthood.chance 0
  set living.chance 1
  set yellow.chance 0
  set red.chance 0
  set blue.chance 0
  set yellow.signal false
  set red.signal false
  set blue.signal false
  set visual.angle 0.1
  set visual.range 0.1
  set day.perception 0.1
  set night.perception 0.1
end

to set-phenotype-to-ideal-form
  ask my.mother [
    let new-energy-supply ( energy.supply / 2 )
    set energy.supply new-energy-supply
    ask myself [ set energy.supply new-energy-supply ]]
  set hidden? false
  set body.size mean [body.size] of anima1s
  set body.shade mean [body.shade] of anima1s
  set bite.capacity mean [bite.capacity] of anima1s
  set mutation.chance mean [mutation.chance] of anima1s
  set sex.ratio mean [sex.ratio] of anima1s
  set litter.size mean [litter.size] of anima1s
  set conception.chance mean [conception.chance] of anima1s
  set birthing.chance mean [birthing.chance] of anima1s
  set weaning.chance mean [weaning.chance] of anima1s
  set infancy.chance mean [infancy.chance] of anima1s
  set juvenility.chance mean [juvenility.chance] of anima1s
  set adulthood.chance mean [adulthood.chance] of anima1s
  set living.chance mean [living.chance] of anima1s
  set yellow.chance mean [yellow.chance] of anima1s
  set red.chance mean [red.chance] of anima1s
  set blue.chance mean [blue.chance] of anima1s
  set yellow.signal one-of modes [yellow.signal] of anima1s
  set red.signal one-of modes [red.signal] of anima1s
  set blue.signal one-of modes [blue.signal] of anima1s
  set visual.angle mean [ visual.angle ] of anima1s
  set visual.range mean [ visual.range ] of anima1s
  set day.perception mean [ day.perception ] of anima1s
  set night.perception mean [ night.perception ] of anima1s
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
  mutate-chromosomes rate-of-mutation

end

to mutate-chromosomes [ rate-of-mutation ]
  set chromosome.I mutate-chromosome chromosome.I rate-of-mutation
  set chromosome.II mutate-chromosome chromosome.II rate-of-mutation
end

to-report mutate-chromosome [ input-chromosome mutation-chance-per-locus ]

  let ouput-chromosome []
  foreach input-chromosome [ allele ->
    let updated-alleles (list allele)

    if ( first allele = true and random-float 1.0 < mutation-chance-per-locus ) [

      let choice random 5

      (ifelse

        ; delete allele
        ( choice = 0 ) [
          set updated-alleles [] ]

        ; duplicate allele
        ( choice = 1 ) [
          set updated-alleles (list allele allele) ]

        ; mutate allele
        ( choice >= 2 ) [
          let new-allele []
          let random-index random ( length allele - 1 ) + 1 ; excludes first codon from mutation
          let index 0
          foreach allele [ codon ->
            ( ifelse
              ( choice = 2 and random-index = index and not member? "uninvadable" model-structure ) [ set new-allele lput get-mutation codon new-allele ] ; mutate codon
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
      [ sta7us-get-mutation unmutated-codon ] ) ]) ; default
end
@#$#@#$#@
GRAPHICS-WINDOW
6
86
836
917
-1
-1
8.22
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
99
0
99
1
1
1
timesteps
30.0

BUTTON
396
10
462
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
467
10
534
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
387
79
path-to-experiment
../results/test2/
1
0
String

BUTTON
540
10
615
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

INPUTBOX
849
10
1092
254
observation-notes
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
763
10
837
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
1103
155
1346
188
plant-minimum-neighbors
plant-minimum-neighbors
0
8
6.0
.1
1
NIL
HORIZONTAL

SLIDER
1103
191
1346
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
665
97
740
142
season
( cos (( 360 / plant-annual-cycle ) * ticks))
3
1
11

SLIDER
1103
82
1346
115
plant-seasonality
plant-seasonality
0
1
1.0
.05
1
NIL
HORIZONTAL

SLIDER
1102
10
1346
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
1102
46
1346
79
plant-daily-cycle
plant-daily-cycle
1
100
20.0
1
1
ticks
HORIZONTAL

MONITOR
746
97
825
142
time
get-solar-status
17
1
11

INPUTBOX
1103
230
1226
299
population
population
1
0
String

INPUTBOX
1103
304
1226
373
genotype
genotype
1
0
String

BUTTON
1231
230
1286
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
1291
230
1346
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
1291
266
1346
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
849
328
1033
373
useful-commands
useful-commands
"help-me" "--------" "meta-report" "verify-code" "check-runtime" "simulation-report" "genotype-reader" "model-structure" "reset-plants" "import-world" "clear-population" "view-genotype" "view-decisions" "view-actions" "add-allele" "delete-allele"
14

BUTTON
1037
328
1092
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
1231
304
1286
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
1291
340
1346
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
1291
304
1346
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
1103
118
1346
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
621
10
757
43
output-results?
output-results?
1
1
-1000

SWITCH
621
46
757
79
selection-on?
selection-on?
0
1
-1000

INPUTBOX
849
262
1092
322
command-input
true
1
0
String (commands)

OUTPUT
849
385
1348
918
10

@#$#@#$#@
# B3GET 1.1.0 INFORMATION

Compatible with NetLogo 6.1.1

## WHAT IS IT?

B3GET is designed to test hypotheses in biology by simulating populations of virtual organisms evolving over generations, whose evolutionary outcomes reflect the selection pressures of their environment. Users input populuation files to seed the initial population and run simulations to evolve these populations - and their genotypes - over generations. Behavioral strategies that are beneficial for their ecological context are expected to emerge.

B3GET helps answer fundamental questions in evolutionary biology by offering users a virtual field site to precisely track the evolution of organismal populations. Researchers can use B3GET to: (1) investigate how populations vary in response to ecological pressures; (2) trace evolutionary histories over indefinite time scales and generations; (3) track an individual for every moment of their life from conception to post-mortem decay; (4) create virtual analogues of living species, including primates like baboons and chimpanzees, to answer species-specific questions; and (5) determine the plausible evolutionary pathways of optimal strategies in response to ecological pressures. Users are able to save, edit, and import population and genotype files, offering an array of possibilities for creating controlled biological experiments.

## HOW IT WORKS

B3GET simulates several factors considered important in biology, including life history trade-offs, investment in body size, variation in aggression, sperm competition, infanticide, and competition over access to food and mates. B3GET calculates each agent’s decision-vectors from its diploid chromosomes and current environmental context. These decision-vectors dictate movement, body growth, desire to mate and eat, and other agent actions. Chromosomes are modified during recombination and mutation, resulting in behavioral strategies that evolve over generations.

## HOW TO USE IT

### STARTING UP

B3GET should come with the following file and [folder] structure. These extensions are written to maintain a modular code structure, which promotes: (1) ease in testing and maintenance, (2) ensuring that future innovations will not interfere with other parts of the program.

> [B3GET]
--- [code]
------ B3GET.nlogo
------ [ extensions ]
--------- commands.nls
--------- data.nls
--------- files.nls
--------- sta7us.nls
--------- import-export.nls
--------- selection.nls
--------- verification.nls
--------- g8tes.nls
--- [data]
------ genotype.txt
------ population.csv
--- [docs]
------ B3GET-ODD-protocol.pdf

B3GET starts with PATH-TO-EXPERIMENT set to [../data/], which means that any data or files generated during simulation will be saved in the [data] folder. Initially, POPULATION is set to [population] and GENOTYPE is set to [genotype], which are files included during download. With these settings, you can just click SETUP and GO to start your first simulation! Please refer to the descriptions of the controls below to perform more complex tasks.

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

Plants, plant life and growth are inspired by Conway's Game of Life, a cellular automaton model that contained rules for when a cell could be 'alive' or 'dead', thus simulating a living ecosystem. In B3GET, 'alive' cells contain plant agents, depicated as a green squares in the model.

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

Peer-reviewed publication on an earlier version of this model:

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

circle111
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30

circle111a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30

circle111ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle111abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle111ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30

circle111b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle111bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle111c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30

circle112
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 101 24 6
Circle -16777216 true false 192 24 6

circle112a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 101 24 6
Circle -16777216 true false 192 24 6

circle112ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 101 24 6
Circle -16777216 true false 192 24 6

circle112abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 101 24 6
Circle -16777216 true false 192 24 6

circle112ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 101 24 6
Circle -16777216 true false 192 24 6

circle112b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 101 24 6
Circle -16777216 true false 192 24 6

circle112bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 101 24 6
Circle -16777216 true false 192 24 6

circle112c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 101 24 6
Circle -16777216 true false 192 24 6

circle113
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 99 22 10
Circle -16777216 true false 190 22 10

circle113a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 99 22 10
Circle -16777216 true false 190 22 10

circle113ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 99 22 10
Circle -16777216 true false 190 22 10

circle113abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 99 22 10
Circle -16777216 true false 190 22 10

circle113ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 99 22 10
Circle -16777216 true false 190 22 10

circle113b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 99 22 10
Circle -16777216 true false 190 22 10

circle113bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 99 22 10
Circle -16777216 true false 190 22 10

circle113c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 90 15 30
Circle -7500403 true true 180 15 30
Circle -1 true false 96 21 18
Circle -1 true false 186 21 18
Circle -16777216 true false 99 22 10
Circle -16777216 true false 190 22 10

circle121
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30

circle121a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30

circle121ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle121abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle121ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30

circle121b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle121bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle121c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30

circle122
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 56 56 4
Circle -16777216 true false 239 56 4

circle122a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 56 56 4
Circle -16777216 true false 239 56 4

circle122ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 56 56 4
Circle -16777216 true false 239 56 4

circle122abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 56 56 4
Circle -16777216 true false 239 56 4

circle122ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 56 56 4
Circle -16777216 true false 239 56 4

circle122b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 56 56 4
Circle -16777216 true false 239 56 4

circle122bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 56 56 4
Circle -16777216 true false 239 56 4

circle122c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 56 56 4
Circle -16777216 true false 239 56 4

circle123
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 53 53 10
Circle -16777216 true false 236 53 10

circle123a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 53 53 10
Circle -16777216 true false 236 53 10

circle123ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 53 53 10
Circle -16777216 true false 236 53 10

circle123abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 53 53 10
Circle -16777216 true false 236 53 10

circle123ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 53 53 10
Circle -16777216 true false 236 53 10

circle123b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 53 53 10
Circle -16777216 true false 236 53 10

circle123bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 53 53 10
Circle -16777216 true false 236 53 10

circle123c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 45 30
Circle -7500403 true true 225 45 30
Circle -1 true false 51 51 18
Circle -1 true false 231 51 18
Circle -16777216 true false 53 53 10
Circle -16777216 true false 236 53 10

circle131
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30

circle131a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30

circle131ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle131abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle131ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30

circle131b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle131bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle131c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30

circle132
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 28 86 6
Circle -16777216 true false 266 86 6

circle132a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 28 86 6
Circle -16777216 true false 266 86 6

circle132ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 28 86 6
Circle -16777216 true false 266 86 6

circle132abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 28 86 6
Circle -16777216 true false 266 86 6

circle132ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 28 86 6
Circle -16777216 true false 266 86 6

circle132b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 28 86 6
Circle -16777216 true false 266 86 6

circle132bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 28 86 6
Circle -16777216 true false 266 86 6

circle132c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 28 86 6
Circle -16777216 true false 266 86 6

circle133
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 26 84 10
Circle -16777216 true false 264 84 10

circle133a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 26 84 10
Circle -16777216 true false 264 84 10

circle133ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 26 84 10
Circle -16777216 true false 264 84 10

circle133abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 26 84 10
Circle -16777216 true false 264 84 10

circle133ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 26 84 10
Circle -16777216 true false 264 84 10

circle133b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 26 84 10
Circle -16777216 true false 264 84 10

circle133bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 26 84 10
Circle -16777216 true false 264 84 10

circle133c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 18 75 30
Circle -7500403 true true 252 75 30
Circle -1 true false 24 81 18
Circle -1 true false 258 81 18
Circle -16777216 true false 26 84 10
Circle -16777216 true false 264 84 10

circle211
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42

circle211a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42

circle211ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle211abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle211ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42

circle211b
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle211bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle211c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42

circle212
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Circle -1 true false 93 11 24
Circle -16777216 true false 100 15 8
Circle -1 true false 182 11 24
Circle -16777216 true false 189 15 8

circle212a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Circle -1 true false 93 11 24
Circle -16777216 true false 100 15 8
Circle -1 true false 182 11 24
Circle -16777216 true false 189 15 8

circle212ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 93 11 24
Circle -16777216 true false 100 15 8
Circle -1 true false 182 11 24
Circle -16777216 true false 189 15 8

circle212abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 93 11 24
Circle -16777216 true false 100 15 8
Circle -1 true false 182 11 24
Circle -16777216 true false 189 15 8

circle212ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Circle -1 true false 93 11 24
Circle -16777216 true false 100 15 8
Circle -1 true false 182 11 24
Circle -16777216 true false 189 15 8

circle212b
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 93 11 24
Circle -16777216 true false 100 15 8
Circle -1 true false 182 11 24
Circle -16777216 true false 189 15 8

circle212bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 93 11 24
Circle -16777216 true false 100 15 8
Circle -1 true false 182 11 24
Circle -16777216 true false 189 15 8

circle212c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Circle -1 true false 93 11 24
Circle -16777216 true false 100 15 8
Circle -1 true false 182 11 24
Circle -16777216 true false 189 15 8

circle213
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Circle -1 true false 93 11 24
Circle -16777216 true false 98 13 13
Circle -1 true false 182 11 24
Circle -16777216 true false 187 13 13

circle213a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Circle -1 true false 93 11 24
Circle -16777216 true false 98 13 13
Circle -1 true false 182 11 24
Circle -16777216 true false 187 13 13

circle213ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 93 11 24
Circle -16777216 true false 98 13 13
Circle -1 true false 182 11 24
Circle -16777216 true false 187 13 13

circle213abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 93 11 24
Circle -16777216 true false 98 13 13
Circle -1 true false 182 11 24
Circle -16777216 true false 187 13 13

circle213ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Circle -1 true false 93 11 24
Circle -16777216 true false 98 13 13
Circle -1 true false 182 11 24
Circle -16777216 true false 187 13 13

circle213b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 93 11 24
Circle -16777216 true false 98 13 13
Circle -1 true false 182 11 24
Circle -16777216 true false 187 13 13

circle213bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 93 11 24
Circle -16777216 true false 98 13 13
Circle -1 true false 182 11 24
Circle -16777216 true false 187 13 13

circle213c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 84 2 42
Circle -7500403 true true 173 2 42
Circle -1 true false 93 11 24
Circle -16777216 true false 98 13 13
Circle -1 true false 182 11 24
Circle -16777216 true false 187 13 13

circle221
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42

circle221a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42

circle221ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle221abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle221ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42

circle221b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle221bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle221c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42

circle222
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Circle -1 true false 48 33 24
Circle -16777216 true false 53 38 8
Circle -1 true false 228 32 24
Circle -16777216 true false 238 37 8

circle222a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Circle -1 true false 48 33 24
Circle -16777216 true false 53 38 8
Circle -1 true false 228 32 24
Circle -16777216 true false 238 37 8

circle222ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 33 24
Circle -16777216 true false 53 38 8
Circle -1 true false 228 32 24
Circle -16777216 true false 238 37 8

circle222abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 33 24
Circle -16777216 true false 53 38 8
Circle -1 true false 228 32 24
Circle -16777216 true false 238 37 8

circle222ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Circle -1 true false 48 33 24
Circle -16777216 true false 53 38 8
Circle -1 true false 228 32 24
Circle -16777216 true false 238 37 8

circle222b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 33 24
Circle -16777216 true false 53 38 8
Circle -1 true false 228 32 24
Circle -16777216 true false 238 37 8

circle222bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 33 24
Circle -16777216 true false 53 38 8
Circle -1 true false 228 32 24
Circle -16777216 true false 238 37 8

circle222c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Circle -1 true false 48 33 24
Circle -16777216 true false 53 38 8
Circle -1 true false 228 32 24
Circle -16777216 true false 238 37 8

circle223
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Circle -1 true false 48 33 24
Circle -16777216 true false 51 36 13
Circle -1 true false 228 32 24
Circle -16777216 true false 236 35 13

circle223a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Circle -1 true false 48 33 24
Circle -16777216 true false 51 36 13
Circle -1 true false 228 32 24
Circle -16777216 true false 236 35 13

circle223ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 33 24
Circle -16777216 true false 51 36 13
Circle -1 true false 228 32 24
Circle -16777216 true false 236 35 13

circle223abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 33 24
Circle -16777216 true false 51 36 13
Circle -1 true false 228 32 24
Circle -16777216 true false 236 35 13

circle223ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Circle -1 true false 48 33 24
Circle -16777216 true false 51 36 13
Circle -1 true false 228 32 24
Circle -16777216 true false 236 35 13

circle223b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 33 24
Circle -16777216 true false 51 36 13
Circle -1 true false 228 32 24
Circle -16777216 true false 236 35 13

circle223bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 33 24
Circle -16777216 true false 51 36 13
Circle -1 true false 228 32 24
Circle -16777216 true false 236 35 13

circle223c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 24 42
Circle -7500403 true true 219 24 42
Circle -1 true false 48 33 24
Circle -16777216 true false 51 36 13
Circle -1 true false 228 32 24
Circle -16777216 true false 236 35 13

circle231
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42

circle231a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42

circle231ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle231abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle231ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42

circle231b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle231bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

circle231c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42

circle232
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Circle -1 true false 18 78 24
Circle -16777216 true false 22 84 8
Circle -1 true false 258 77 24
Circle -16777216 true false 269 83 8

circle232a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Circle -1 true false 18 78 24
Circle -16777216 true false 22 84 8
Circle -1 true false 258 77 24
Circle -16777216 true false 269 83 8

circle232ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 18 78 24
Circle -16777216 true false 22 84 8
Circle -1 true false 258 77 24
Circle -16777216 true false 269 83 8

circle232abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 18 78 24
Circle -16777216 true false 22 84 8
Circle -1 true false 258 77 24
Circle -16777216 true false 269 83 8

circle232ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Circle -1 true false 18 78 24
Circle -16777216 true false 22 84 8
Circle -1 true false 258 77 24
Circle -16777216 true false 269 83 8

circle232b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 18 78 24
Circle -16777216 true false 22 84 8
Circle -1 true false 258 77 24
Circle -16777216 true false 269 83 8

circle232bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 18 78 24
Circle -16777216 true false 22 84 8
Circle -1 true false 258 77 24
Circle -16777216 true false 269 83 8

circle232c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Circle -1 true false 18 78 24
Circle -16777216 true false 22 84 8
Circle -1 true false 258 77 24
Circle -16777216 true false 269 83 8

circle233
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Circle -1 true false 18 78 24
Circle -16777216 true false 20 82 13
Circle -1 true false 258 77 24
Circle -16777216 true false 267 81 13

circle233a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Circle -1 true false 18 78 24
Circle -16777216 true false 20 82 13
Circle -1 true false 258 77 24
Circle -16777216 true false 267 81 13

circle233ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 18 78 24
Circle -16777216 true false 20 82 13
Circle -1 true false 258 77 24
Circle -16777216 true false 267 81 13

circle233abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 18 78 24
Circle -16777216 true false 20 82 13
Circle -1 true false 258 77 24
Circle -16777216 true false 267 81 13

circle233ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Circle -1 true false 18 78 24
Circle -16777216 true false 20 82 13
Circle -1 true false 258 77 24
Circle -16777216 true false 267 81 13

circle233b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 18 78 24
Circle -16777216 true false 20 82 13
Circle -1 true false 258 77 24
Circle -16777216 true false 267 81 13

circle233bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 18 78 24
Circle -16777216 true false 20 82 13
Circle -1 true false 258 77 24
Circle -16777216 true false 267 81 13

circle233c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 9 69 42
Circle -7500403 true true 249 68 42
Circle -1 true false 18 78 24
Circle -16777216 true false 20 82 13
Circle -1 true false 258 77 24
Circle -16777216 true false 267 81 13

circle311
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 75 -22 60
Circle -7500403 true true 165 -22 60

circle311a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 75 -22 60
Circle -7500403 true true 165 -22 60

circle311ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 75 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 165 -22 60

circle311abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 165 -22 60

circle311ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 -22 60
Circle -7500403 true true 165 -22 60

circle311b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 75 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 165 -22 60

circle311bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 165 -22 60

circle311c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 -22 60
Circle -7500403 true true 165 -22 60

circle312
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 75 -22 60
Circle -1 true false 83 -14 44
Circle -16777216 true false 99 -4 12
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 189 -4 12

circle312a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 75 -22 60
Circle -1 true false 83 -14 44
Circle -16777216 true false 99 -4 12
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 189 -4 12

circle312ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 75 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 83 -14 44
Circle -16777216 true false 99 -4 12
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 189 -4 12

circle312abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 83 -14 44
Circle -16777216 true false 99 -4 12
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 189 -4 12

circle312ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 -22 60
Circle -1 true false 83 -14 44
Circle -16777216 true false 99 -4 12
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 189 -4 12

circle312b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 75 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 83 -14 44
Circle -16777216 true false 99 -4 12
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 189 -4 12

circle312bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 83 -14 44
Circle -16777216 true false 99 -4 12
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 189 -4 12

circle312c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 -22 60
Circle -1 true false 83 -14 44
Circle -16777216 true false 99 -4 12
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 189 -4 12

circle313
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 75 -22 60
Circle -1 true false 83 -14 44
Circle -16777216 true false 92 -11 26
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 182 -11 26

circle313a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 75 -22 60
Circle -1 true false 83 -14 44
Circle -16777216 true false 92 -11 26
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 182 -11 26

circle313ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 75 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 83 -14 44
Circle -16777216 true false 92 -11 26
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 182 -11 26

circle313abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 83 -14 44
Circle -16777216 true false 92 -11 26
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 182 -11 26

circle313ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 83 -14 44
Circle -16777216 true false 92 -11 26
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 182 -11 26

circle313b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 75 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 83 -14 44
Circle -16777216 true false 92 -11 26
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 182 -11 26

circle313bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 83 -14 44
Circle -16777216 true false 92 -11 26
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 182 -11 26

circle313c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 -22 60
Circle -1 true false 83 -14 44
Circle -16777216 true false 92 -11 26
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 182 -11 26

circle321
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 30 8 60
Circle -7500403 true true 210 8 60

circle321a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 30 8 60
Circle -7500403 true true 210 8 60

circle321ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 30 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 210 8 60

circle321abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 210 8 60

circle321ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 8 60
Circle -7500403 true true 210 8 60

circle321b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 30 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 210 8 60

circle321bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 210 8 60

circle321c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 8 60
Circle -7500403 true true 210 8 60

circle322
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 30 8 60
Circle -1 true false 38 16 44
Circle -16777216 true false 49 28 12
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 239 27 12

circle322a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 30 8 60
Circle -1 true false 38 16 44
Circle -16777216 true false 49 28 12
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 239 27 12

circle322ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 30 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 16 44
Circle -16777216 true false 49 28 12
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 239 27 12

circle322abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 16 44
Circle -16777216 true false 49 28 12
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 239 27 12

circle322ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 8 60
Circle -1 true false 38 16 44
Circle -16777216 true false 49 28 12
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 239 27 12

circle322b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 30 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 16 44
Circle -16777216 true false 49 28 12
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 239 27 12

circle322bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 16 44
Circle -16777216 true false 49 28 12
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 239 27 12

circle322c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 8 60
Circle -1 true false 38 16 44
Circle -16777216 true false 49 28 12
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 239 27 12

circle323
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 30 8 60
Circle -1 true false 38 16 44
Circle -16777216 true false 42 21 26
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 232 20 26

circle323a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 30 8 60
Circle -1 true false 38 16 44
Circle -16777216 true false 42 21 26
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 232 20 26

circle323ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 30 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 16 44
Circle -16777216 true false 42 21 26
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 232 20 26

circle323abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 16 44
Circle -16777216 true false 42 21 26
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 232 20 26

circle323ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 8 60
Circle -1 true false 38 16 44
Circle -16777216 true false 42 21 26
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 232 20 26

circle323b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 30 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 16 44
Circle -16777216 true false 42 21 26
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 232 20 26

circle323bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 16 44
Circle -16777216 true false 42 21 26
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 232 20 26

circle323c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 8 60
Circle -1 true false 38 16 44
Circle -16777216 true false 42 21 26
Circle -7500403 true true 210 8 60
Circle -1 true false 218 16 44
Circle -16777216 true false 232 20 26

circle331
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 0 38 60
Circle -7500403 true true 240 38 60

circle331a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 0 38 60
Circle -7500403 true true 240 38 60

circle331ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 0 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 240 38 60

circle331abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 0 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 240 38 60

circle331ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 0 38 60
Circle -7500403 true true 240 38 60

circle331b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 0 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 240 38 60

circle331bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 0 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 240 38 60

circle331c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 0 38 60
Circle -7500403 true true 240 38 60

circle332
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 0 38 60
Circle -1 true false 8 46 44
Circle -16777216 true false 18 60 12
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 270 60 12

circle332a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 0 38 60
Circle -1 true false 8 46 44
Circle -16777216 true false 18 60 12
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 270 60 12

circle332ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 0 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 8 46 44
Circle -16777216 true false 18 60 12
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 270 60 12

circle332abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 0 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 8 46 44
Circle -16777216 true false 18 60 12
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 270 60 12

circle332ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 0 38 60
Circle -1 true false 8 46 44
Circle -16777216 true false 18 60 12
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 270 60 12

circle332b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 0 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 8 46 44
Circle -16777216 true false 18 60 12
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 270 60 12

circle332bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 0 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 8 46 44
Circle -16777216 true false 18 60 12
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 270 60 12

circle332c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 0 38 60
Circle -1 true false 8 46 44
Circle -16777216 true false 18 60 12
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 270 60 12

circle333
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 0 38 60
Circle -1 true false 8 46 44
Circle -16777216 true false 11 53 26
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 263 53 26

circle333a
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 0 38 60
Circle -1 true false 8 46 44
Circle -16777216 true false 11 53 26
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 263 53 26

circle333ab
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Circle -7500403 true true 0 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 8 46 44
Circle -16777216 true false 11 53 26
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 263 53 26

circle333abc
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 0 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 8 46 44
Circle -16777216 true false 11 53 26
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 263 53 26

circle333ac
true
0
Circle -7500403 true true 45 45 210
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 0 38 60
Circle -1 true false 8 46 44
Circle -16777216 true false 11 53 26
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 263 53 26

circle333b
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 0 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 8 46 44
Circle -16777216 true false 11 53 26
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 263 53 26

circle333bc
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 0 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 8 46 44
Circle -16777216 true false 11 53 26
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 263 53 26

circle333c
true
0
Circle -7500403 true true 45 45 210
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 0 38 60
Circle -1 true false 8 46 44
Circle -16777216 true false 11 53 26
Circle -7500403 true true 240 38 60
Circle -1 true false 248 46 44
Circle -16777216 true false 263 53 26

test
true
0
Circle -7500403 true true 45 45 210
Circle -7500403 true true 75 -22 60
Circle -1 true false 83 -14 44
Circle -16777216 true false 99 -4 12
Circle -7500403 true true 165 -22 60
Circle -1 true false 173 -14 44
Circle -16777216 true false 189 -4 12
Polygon -7500403 true true 150 60 135 30 150 45 165 30
Polygon -7500403 true true 120 240 120 270 135 255 150 270 165 255 180 270 180 240
Polygon -7500403 true true 75 105 45 75 30 60 45 60 45 45 60 75 75 90
Polygon -7500403 true true 225 240 240 240 255 240 270 225 240 225 225 210 210 225
Polygon -7500403 true true 225 105 255 75 270 60 255 60 255 45 240 75 225 90
Polygon -7500403 true true 75 240 60 240 45 240 30 225 60 225 75 210 90 225
Rectangle -13791810 true false 75 105 225 135
Rectangle -1184463 true false 105 150 195 180
Rectangle -2674135 true false 135 195 165 225

triangle111
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30

triangle111a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30

triangle111ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle111abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle111ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30

triangle111b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle111bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle111c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30

triangle112
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 86 24 6
Circle -16777216 true false 207 24 6

triangle112a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 86 24 6
Circle -16777216 true false 207 24 6

triangle112ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 86 24 6
Circle -16777216 true false 207 24 6

triangle112abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 86 24 6
Circle -16777216 true false 207 24 6

triangle112ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 86 24 6
Circle -16777216 true false 207 24 6

triangle112b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 86 24 6
Circle -16777216 true false 207 24 6

triangle112bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 86 24 6
Circle -16777216 true false 207 24 6

triangle112c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 86 24 6
Circle -16777216 true false 207 24 6

triangle113
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 84 22 10
Circle -16777216 true false 205 22 10

triangle113a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 84 22 10
Circle -16777216 true false 205 22 10

triangle113ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 84 22 10
Circle -16777216 true false 205 22 10

triangle113abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 84 22 10
Circle -16777216 true false 205 22 10

triangle113ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 84 22 10
Circle -16777216 true false 205 22 10

triangle113b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 84 22 10
Circle -16777216 true false 205 22 10

triangle113bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 84 22 10
Circle -16777216 true false 205 22 10

triangle113c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 75 15 30
Circle -7500403 true true 195 15 30
Circle -1 true false 81 21 18
Circle -1 true false 201 21 18
Circle -16777216 true false 84 22 10
Circle -16777216 true false 205 22 10

triangle121
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30

triangle121a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30

triangle121ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle121abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle121ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30

triangle121b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle121bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle121c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30

triangle122
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 70 55 6
Circle -16777216 true false 223 55 6

triangle122a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 70 55 6
Circle -16777216 true false 223 55 6

triangle122ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 70 55 6
Circle -16777216 true false 223 55 6

triangle122abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 70 55 6
Circle -16777216 true false 223 55 6

triangle122ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 70 55 6
Circle -16777216 true false 223 55 6

triangle122b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 70 55 6
Circle -16777216 true false 223 55 6

triangle122bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 70 55 6
Circle -16777216 true false 223 55 6

triangle122c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 70 55 6
Circle -16777216 true false 223 55 6

triangle123
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 68 53 10
Circle -16777216 true false 221 53 10

triangle123a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 68 53 10
Circle -16777216 true false 221 53 10

triangle123ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 68 53 10
Circle -16777216 true false 221 53 10

triangle123abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 68 53 10
Circle -16777216 true false 221 53 10

triangle123ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 68 53 10
Circle -16777216 true false 221 53 10

triangle123b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 68 53 10
Circle -16777216 true false 221 53 10

triangle123bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 68 53 10
Circle -16777216 true false 221 53 10

triangle123c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 45 30
Circle -7500403 true true 210 45 30
Circle -1 true false 66 51 18
Circle -1 true false 216 51 18
Circle -16777216 true false 68 53 10
Circle -16777216 true false 221 53 10

triangle131
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30

triangle131a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30

triangle131ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle131abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle131ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30

triangle131b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle131bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle131c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30

triangle132
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 58 86 6
Circle -16777216 true false 236 86 6

triangle132a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 58 86 6
Circle -16777216 true false 236 86 6

triangle132ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 58 86 6
Circle -16777216 true false 236 86 6

triangle132abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 58 86 6
Circle -16777216 true false 236 86 6

triangle132ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 58 86 6
Circle -16777216 true false 236 86 6

triangle132b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 58 86 6
Circle -16777216 true false 236 86 6

triangle132bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 58 86 6
Circle -16777216 true false 236 86 6

triangle132c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 58 86 6
Circle -16777216 true false 236 86 6

triangle133
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 56 84 10
Circle -16777216 true false 234 84 10

triangle133a
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 56 84 10
Circle -16777216 true false 234 84 10
Circle -1184463 true false 120 60 60

triangle133ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 56 84 10
Circle -16777216 true false 234 84 10
Circle -1184463 true false 120 60 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle133abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 56 84 10
Circle -16777216 true false 234 84 10
Circle -1184463 true false 120 60 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Polygon -13791810 true false 75 195 225 195 180 225 120 225 75 195

triangle133ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 56 84 10
Circle -16777216 true false 234 84 10
Circle -1184463 true false 120 60 60
Polygon -13791810 true false 75 195 225 195 180 225 120 225 75 195

triangle133b
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 56 84 10
Circle -16777216 true false 234 84 10
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle133bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 56 84 10
Circle -16777216 true false 234 84 10
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Polygon -13791810 true false 75 195 225 195 180 225 120 225 75 195

triangle133c
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -7500403 true true 48 75 30
Circle -7500403 true true 222 75 30
Circle -1 true false 54 81 18
Circle -1 true false 228 81 18
Circle -16777216 true false 56 84 10
Circle -16777216 true false 234 84 10
Polygon -13791810 true false 75 195 225 195 180 225 120 225 75 195

triangle211
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42

triangle211a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42

triangle211ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle211abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle211ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42

triangle211b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle211bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle211c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42

triangle212
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Circle -1 true false 78 11 24
Circle -16777216 true false 85 15 8
Circle -1 true false 197 11 24
Circle -16777216 true false 205 15 8

triangle212a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Circle -1 true false 78 11 24
Circle -16777216 true false 85 15 8
Circle -1 true false 197 11 24
Circle -16777216 true false 205 15 8

triangle212ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 78 11 24
Circle -16777216 true false 85 15 8
Circle -1 true false 197 11 24
Circle -16777216 true false 205 15 8

triangle212abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 78 11 24
Circle -16777216 true false 85 15 8
Circle -1 true false 197 11 24
Circle -16777216 true false 205 15 8

triangle212ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Circle -1 true false 78 11 24
Circle -16777216 true false 85 15 8
Circle -1 true false 197 11 24
Circle -16777216 true false 205 15 8

triangle212b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 78 11 24
Circle -16777216 true false 85 15 8
Circle -1 true false 197 11 24
Circle -16777216 true false 205 15 8

triangle212bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 78 11 24
Circle -16777216 true false 85 15 8
Circle -1 true false 197 11 24
Circle -16777216 true false 205 15 8

triangle212c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Circle -1 true false 78 11 24
Circle -16777216 true false 85 15 8
Circle -1 true false 197 11 24
Circle -16777216 true false 205 15 8

triangle213
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Circle -1 true false 78 11 24
Circle -16777216 true false 83 13 13
Circle -1 true false 197 11 24
Circle -16777216 true false 202 13 13

triangle213a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Circle -1 true false 78 11 24
Circle -16777216 true false 83 13 13
Circle -1 true false 197 11 24
Circle -16777216 true false 202 13 13

triangle213ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 78 11 24
Circle -16777216 true false 83 13 13
Circle -1 true false 197 11 24
Circle -16777216 true false 202 13 13

triangle213abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 78 11 24
Circle -16777216 true false 83 13 13
Circle -1 true false 197 11 24
Circle -16777216 true false 202 13 13

triangle213ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Circle -1 true false 78 11 24
Circle -16777216 true false 83 13 13
Circle -1 true false 197 11 24
Circle -16777216 true false 202 13 13

triangle213b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 78 11 24
Circle -16777216 true false 83 13 13
Circle -1 true false 197 11 24
Circle -16777216 true false 202 13 13

triangle213bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 78 11 24
Circle -16777216 true false 83 13 13
Circle -1 true false 197 11 24
Circle -16777216 true false 202 13 13

triangle213c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 69 2 42
Circle -7500403 true true 188 2 42
Circle -1 true false 78 11 24
Circle -16777216 true false 83 13 13
Circle -1 true false 197 11 24
Circle -16777216 true false 202 13 13

triangle221
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42

triangle221a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42

triangle221ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle221abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle221ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42

triangle221b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle221bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle221c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42

triangle222
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Circle -1 true false 63 33 24
Circle -16777216 true false 68 38 8
Circle -1 true false 213 32 24
Circle -16777216 true false 223 37 8

triangle222a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Circle -1 true false 63 33 24
Circle -16777216 true false 68 38 8
Circle -1 true false 213 32 24
Circle -16777216 true false 223 37 8

triangle222ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 63 33 24
Circle -16777216 true false 68 38 8
Circle -1 true false 213 32 24
Circle -16777216 true false 223 37 8

triangle222abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 63 33 24
Circle -16777216 true false 68 38 8
Circle -1 true false 213 32 24
Circle -16777216 true false 223 37 8

triangle222ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Circle -1 true false 63 33 24
Circle -16777216 true false 68 38 8
Circle -1 true false 213 32 24
Circle -16777216 true false 223 37 8

triangle222b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 63 33 24
Circle -16777216 true false 68 38 8
Circle -1 true false 213 32 24
Circle -16777216 true false 223 37 8

triangle222bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 63 33 24
Circle -16777216 true false 68 38 8
Circle -1 true false 213 32 24
Circle -16777216 true false 223 37 8

triangle222c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Circle -1 true false 63 33 24
Circle -16777216 true false 68 38 8
Circle -1 true false 213 32 24
Circle -16777216 true false 223 37 8

triangle223
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Circle -1 true false 63 33 24
Circle -16777216 true false 66 36 13
Circle -1 true false 213 32 24
Circle -16777216 true false 221 35 13

triangle223a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Circle -1 true false 63 33 24
Circle -16777216 true false 66 36 13
Circle -1 true false 213 32 24
Circle -16777216 true false 221 35 13

triangle223ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 63 33 24
Circle -16777216 true false 66 36 13
Circle -1 true false 213 32 24
Circle -16777216 true false 221 35 13

triangle223abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 63 33 24
Circle -16777216 true false 66 36 13
Circle -1 true false 213 32 24
Circle -16777216 true false 221 35 13

triangle223ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Circle -1 true false 63 33 24
Circle -16777216 true false 66 36 13
Circle -1 true false 213 32 24
Circle -16777216 true false 221 35 13

triangle223b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 63 33 24
Circle -16777216 true false 66 36 13
Circle -1 true false 213 32 24
Circle -16777216 true false 221 35 13

triangle223bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 63 33 24
Circle -16777216 true false 66 36 13
Circle -1 true false 213 32 24
Circle -16777216 true false 221 35 13

triangle223c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 54 24 42
Circle -7500403 true true 204 24 42
Circle -1 true false 63 33 24
Circle -16777216 true false 66 36 13
Circle -1 true false 213 32 24
Circle -16777216 true false 221 35 13

triangle231
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42

triangle231a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42

triangle231ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle231abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle231ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42

triangle231b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle231bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135

triangle231c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42

triangle232
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Circle -1 true false 48 78 24
Circle -16777216 true false 52 84 8
Circle -1 true false 228 77 24
Circle -16777216 true false 239 83 8

triangle232a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Circle -1 true false 48 78 24
Circle -16777216 true false 52 84 8
Circle -1 true false 228 77 24
Circle -16777216 true false 239 83 8

triangle232ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 78 24
Circle -16777216 true false 52 84 8
Circle -1 true false 228 77 24
Circle -16777216 true false 239 83 8

triangle232abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 78 24
Circle -16777216 true false 52 84 8
Circle -1 true false 228 77 24
Circle -16777216 true false 239 83 8

triangle232ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Circle -1 true false 48 78 24
Circle -16777216 true false 52 84 8
Circle -1 true false 228 77 24
Circle -16777216 true false 239 83 8

triangle232b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 78 24
Circle -16777216 true false 52 84 8
Circle -1 true false 228 77 24
Circle -16777216 true false 239 83 8

triangle232bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 78 24
Circle -16777216 true false 52 84 8
Circle -1 true false 228 77 24
Circle -16777216 true false 239 83 8

triangle232c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Circle -1 true false 48 78 24
Circle -16777216 true false 52 84 8
Circle -1 true false 228 77 24
Circle -16777216 true false 239 83 8

triangle233
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Circle -1 true false 48 78 24
Circle -16777216 true false 50 82 13
Circle -1 true false 228 77 24
Circle -16777216 true false 237 81 13

triangle233a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Circle -1 true false 48 78 24
Circle -16777216 true false 50 82 13
Circle -1 true false 228 77 24
Circle -16777216 true false 237 81 13

triangle233ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 78 24
Circle -16777216 true false 50 82 13
Circle -1 true false 228 77 24
Circle -16777216 true false 237 81 13

triangle233abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 78 24
Circle -16777216 true false 50 82 13
Circle -1 true false 228 77 24
Circle -16777216 true false 237 81 13

triangle233ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Circle -1 true false 48 78 24
Circle -16777216 true false 50 82 13
Circle -1 true false 228 77 24
Circle -16777216 true false 237 81 13

triangle233b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 78 24
Circle -16777216 true false 50 82 13
Circle -1 true false 228 77 24
Circle -16777216 true false 237 81 13

triangle233bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 48 78 24
Circle -16777216 true false 50 82 13
Circle -1 true false 228 77 24
Circle -16777216 true false 237 81 13

triangle233c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 39 69 42
Circle -7500403 true true 219 68 42
Circle -1 true false 48 78 24
Circle -16777216 true false 50 82 13
Circle -1 true false 228 77 24
Circle -16777216 true false 237 81 13

triangle311
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 60 -22 60
Circle -7500403 true true 180 -22 60

triangle311a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 60 -22 60
Circle -7500403 true true 180 -22 60

triangle311ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 60 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 180 -22 60

triangle311abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 180 -22 60

triangle311ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 -22 60
Circle -7500403 true true 180 -22 60

triangle311b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 60 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 180 -22 60

triangle311bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 180 -22 60

triangle311c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 -22 60
Circle -7500403 true true 180 -22 60

triangle312
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 60 -22 60
Circle -1 true false 68 -14 44
Circle -16777216 true false 84 -4 12
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 204 -4 12

triangle312a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 60 -22 60
Circle -1 true false 68 -14 44
Circle -16777216 true false 84 -4 12
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 204 -4 12

triangle312ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 60 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 68 -14 44
Circle -16777216 true false 84 -4 12
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 204 -4 12

triangle312abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 68 -14 44
Circle -16777216 true false 84 -4 12
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 204 -4 12

triangle312ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 -22 60
Circle -1 true false 68 -14 44
Circle -16777216 true false 84 -4 12
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 204 -4 12

triangle312b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 60 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 68 -14 44
Circle -16777216 true false 84 -4 12
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 204 -4 12

triangle312bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 68 -14 44
Circle -16777216 true false 84 -4 12
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 204 -4 12

triangle312c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 -22 60
Circle -1 true false 68 -14 44
Circle -16777216 true false 84 -4 12
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 204 -4 12

triangle313
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 60 -22 60
Circle -1 true false 68 -14 44
Circle -16777216 true false 77 -11 26
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 197 -11 26

triangle313a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 60 -22 60
Circle -1 true false 68 -14 44
Circle -16777216 true false 77 -11 26
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 197 -11 26

triangle313ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 60 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 68 -14 44
Circle -16777216 true false 77 -11 26
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 197 -11 26

triangle313abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 68 -14 44
Circle -16777216 true false 77 -11 26
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 197 -11 26

triangle313ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 -22 60
Circle -1 true false 68 -14 44
Circle -16777216 true false 77 -11 26
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 197 -11 26

triangle313b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 60 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 68 -14 44
Circle -16777216 true false 77 -11 26
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 197 -11 26

triangle313bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 -22 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 68 -14 44
Circle -16777216 true false 77 -11 26
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 197 -11 26

triangle313c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 60 -22 60
Circle -1 true false 68 -14 44
Circle -16777216 true false 77 -11 26
Circle -7500403 true true 180 -22 60
Circle -1 true false 188 -14 44
Circle -16777216 true false 197 -11 26

triangle321
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -7500403 true true 45 8 60
Circle -7500403 true true 195 8 60

triangle321a
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -1184463 true false 120 60 58
Circle -7500403 true true 45 8 60
Circle -7500403 true true 195 8 60

triangle321ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -1184463 true false 120 60 58
Circle -7500403 true true 45 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 195 8 60

triangle321abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 195 8 60

triangle321ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 8 60
Circle -7500403 true true 195 8 60

triangle321b
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Circle -7500403 true true 45 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 195 8 60

triangle321bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 195 8 60

triangle321c
true
0
Polygon -7500403 true true 150 0 0 240 300 240 150 0
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 8 60
Circle -7500403 true true 195 8 60

triangle322
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 45 8 60
Circle -1 true false 53 16 44
Circle -16777216 true false 64 28 12
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 224 27 12

triangle322a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 45 8 60
Circle -1 true false 53 16 44
Circle -16777216 true false 64 28 12
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 224 27 12

triangle322ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 45 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 53 16 44
Circle -16777216 true false 64 28 12
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 224 27 12

triangle322abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 53 16 44
Circle -16777216 true false 64 28 12
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 224 27 12

triangle322ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 8 60
Circle -1 true false 53 16 44
Circle -16777216 true false 64 28 12
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 224 27 12

triangle322b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 45 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 53 16 44
Circle -16777216 true false 64 28 12
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 224 27 12

triangle322bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 53 16 44
Circle -16777216 true false 64 28 12
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 224 27 12

triangle322c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 8 60
Circle -1 true false 53 16 44
Circle -16777216 true false 64 28 12
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 224 27 12

triangle323
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 45 8 60
Circle -1 true false 53 16 44
Circle -16777216 true false 57 21 26
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 217 20 26

triangle323a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 45 8 60
Circle -1 true false 53 16 44
Circle -16777216 true false 57 21 26
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 217 20 26

triangle323ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 45 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 53 16 44
Circle -16777216 true false 57 21 26
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 217 20 26

triangle323abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 53 16 44
Circle -16777216 true false 57 21 26
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 217 20 26

triangle323ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 8 60
Circle -1 true false 53 16 44
Circle -16777216 true false 57 21 26
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 217 20 26

triangle323b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 45 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 53 16 44
Circle -16777216 true false 57 21 26
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 217 20 26

triangle323bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 8 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 53 16 44
Circle -16777216 true false 57 21 26
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 217 20 26

triangle323c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 45 8 60
Circle -1 true false 53 16 44
Circle -16777216 true false 57 21 26
Circle -7500403 true true 195 8 60
Circle -1 true false 203 16 44
Circle -16777216 true false 217 20 26

triangle331
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 30 38 60
Circle -7500403 true true 210 38 60

triangle331a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 30 38 60
Circle -7500403 true true 210 38 60

triangle331ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 30 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 210 38 60

triangle331abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 210 38 60

triangle331ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 38 60
Circle -7500403 true true 210 38 60

triangle331b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 30 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 210 38 60

triangle331bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -7500403 true true 210 38 60

triangle331c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 38 60
Circle -7500403 true true 210 38 60

triangle332
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 30 38 60
Circle -1 true false 38 46 44
Circle -16777216 true false 48 60 12
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 240 60 12

triangle332a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 30 38 60
Circle -1 true false 38 46 44
Circle -16777216 true false 48 60 12
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 240 60 12

triangle332ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 30 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 46 44
Circle -16777216 true false 48 60 12
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 240 60 12

triangle332abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 46 44
Circle -16777216 true false 48 60 12
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 240 60 12

triangle332ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 38 60
Circle -1 true false 38 46 44
Circle -16777216 true false 48 60 12
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 240 60 12

triangle332b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 30 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 46 44
Circle -16777216 true false 48 60 12
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 240 60 12

triangle332bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 46 44
Circle -16777216 true false 48 60 12
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 240 60 12

triangle332c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 38 60
Circle -1 true false 38 46 44
Circle -16777216 true false 48 60 12
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 240 60 12

triangle333
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 30 38 60
Circle -1 true false 38 46 44
Circle -16777216 true false 41 53 26
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 233 53 26

triangle333a
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 30 38 60
Circle -1 true false 38 46 44
Circle -16777216 true false 41 53 26
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 233 53 26

triangle333ab
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Circle -7500403 true true 30 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 46 44
Circle -16777216 true false 41 53 26
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 233 53 26

triangle333abc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 46 44
Circle -16777216 true false 41 53 26
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 233 53 26

triangle333ac
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -1184463 true false 120 60 58
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 38 60
Circle -1 true false 38 46 44
Circle -16777216 true false 41 53 26
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 233 53 26

triangle333b
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Circle -7500403 true true 30 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 46 44
Circle -16777216 true false 41 53 26
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 233 53 26

triangle333bc
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 38 60
Polygon -2674135 true false 90 135 60 180 240 180 210 135
Circle -1 true false 38 46 44
Circle -16777216 true false 41 53 26
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 233 53 26

triangle333c
true
0
Polygon -7500403 true true 150 0 0 240 300 240
Polygon -13791810 true false 150 195 225 195 180 225 150 225 120 225 75 195
Circle -7500403 true true 30 38 60
Circle -1 true false 38 46 44
Circle -16777216 true false 41 53 26
Circle -7500403 true true 210 38 60
Circle -1 true false 218 46 44
Circle -16777216 true false 233 53 26
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
    <final>crt 1 [ record-simulation die ]</final>
    <timeLimit steps="101"/>
    <exitCondition>not any? anima1s or median [generation-number] of anima1s &gt; 100</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-structure">
      <value value="&quot;baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta7us&quot;"/>
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
      <value value="0.5"/>
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
    <go>go</go>
    <final>crt 1 [ record-simulation die ]</final>
    <timeLimit steps="100"/>
    <exitCondition>not any? anima1s</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-structure">
      <value value="&quot;baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta7us&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterioration-rate">
      <value value="-0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="false"/>
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
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-quality">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-minimum-neighbors">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-maximum-neighbors">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="&quot;test&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype">
      <value value="&quot;test&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="WORLD-X" repetitions="1" runMetricsEveryStep="true">
    <setup>; give simulation-id specific configuration: sDOB17 means simulation of WORLD-D, Baboons seed population, run B (instead of A), plant-minimum-neighbors = 1 and plant-maximum-neighbors = 7
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
    <timeLimit steps="101"/>
    <exitCondition>not any? anima1s or median [generation.number] of anima1s &gt; 100</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-structure">
      <value value="&quot;baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta7us&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterioration-rate">
      <value value="-0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="false"/>
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
      <value value="0.5"/>
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
      <value value="&quot;adam-and-eve&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="WORLD-TEST-2" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
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
    <final>print behaviorspace-run-number
save-verification-to "verification.csv"
crt 1 [ record-simulation die ]</final>
    <timeLimit steps="15"/>
    <exitCondition>not any? anima1s</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-structure">
      <value value="&quot;baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta7us&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterioration-rate">
      <value value="-0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="false"/>
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
      <value value="0.5"/>
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
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-maximum-neighbors">
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="&quot;adam-and-eve&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype">
      <value value="&quot;adam-and-eve&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SIMULTATION-EXPERIMENT" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>not any? anima1s or median [generation-number] of anima1s &gt;= 10</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="model-structure">
      <value value="&quot;baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta7us&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterioration-rate">
      <value value="-0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-annual-cycle">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-daily-cycle">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-seasonality">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-quality">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-minimum-neighbors">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-maximum-neighbors">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="&quot;Chimpanzees&quot;"/>
      <value value="&quot;Geladas&quot;"/>
      <value value="&quot;Olives&quot;"/>
      <value value="&quot;Hamadryas&quot;"/>
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
