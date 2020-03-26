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
  "results.nls"
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
  x.magnitude
  y.magnitude

  chromosome.I
  chromosome.II
  decision.vectors
  completed.actions
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
  completed-actions

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
  sun-status
  deterioration-rate
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
  set model-version "1.1.0"
  set deterioration-rate -0.001
  set selection-rate 0.0001
end

to setup [ new-simulation-id ]
  ;if ( simulation-id != 0 and output-results? = true ) [ record-world ]
  clear-all
  reset-ticks
  setup-parameters
  setup-plants
  set simulation-id new-simulation-id
  import-population
  import-genotype
  clear-output
end

to setup-plants
  ask plants [ die ]
  ask patches [
    set pcolor 36
    sprout-plants 1 [ initialize-plant ]]
  repeat 100 [ update-plants ]
end

to save
  ifelse ( simulation-id = 0 )
  [ set simulation-id generate-simulation-id ]
  [ set documentation-notes (word "Simulation " simulation-id " saved. " documentation-notes )
    update-metafile "simulation" simulation-id ]
end

to reset-population
  set population generate-population-id
end

to export-population
  save-population
end

to seed-population
  import-population
end

to reset-genotype
  set genotype generate-genotype-id
end

to export-genotype
  ask anima1s with [ read-from-string but-first genotype = meta-id ] [ save-genotype ]
end

to seed-genotype
  import-genotype
end

to-report generate-simulation-id
  let alphabet [ "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" ]
  report ( word "s" random 99 one-of alphabet one-of alphabet one-of alphabet )
end

to-report get-decisions-from-genoreader [ my-environment ]
  report ( ifelse-value
    ( genotype-reader = "sta7us" ) [ sta7us-make-decisions my-environment ]
    ( genotype-reader = "g3notype" ) [ g3notype-make-decisions my-environment ]
    [ sta7us-make-decisions my-environment ] )
end

to-report get-mutation-from-genoreader [ unmutated-codon ]
  report (ifelse-value
    ( is-number? unmutated-codon ) [ get-updated-value unmutated-codon ( one-of [ 1 -1 ] ) ]
    [( ifelse-value
      ( genotype-reader = "sta7us" ) [ sta7us-get-mutation unmutated-codon ]
      ( genotype-reader = "g3notype" ) [ g3notype-get-mutation unmutated-codon ]
      [ sta7us-get-mutation unmutated-codon ] ) ])
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

  ; code to be deleted
  ask anima1s with [ length carried.items > 0 ] [ set carried.items remove nobody carried.items ]
  ask groups with [ count group-members = 0 ] [ die ]

  ; ENVIRONMENTAL CONTRAINTS
  update-plants
  ask turtles [ set age age + 1 ]
  if selection-on? [ artificial-selection ]
  ifelse ( model-structure = "reaper" )
  [ if (( count anima1s - 100 ) > 0 ) [ ask n-of ( count anima1s - 100 ) anima1s [ set ticks-at-death ticks die ]]]
  [ ask anima1s [check-mortality] ]
  ask anima1s [ deteriorate update-appearance ]
  if ( model-structure = "sower" and count anima1s with [ biological.sex = "male" and life.history = "adult" ] > 0 and count anima1s with [ biological.sex = "female" and life.history = "adult" ] > 0 and (count anima1s < 100) ) [ repeat ( 100 - count anima1s ) [ ask one-of anima1s with [ biological.sex = "female" and life.history = "adult" ] [ conceive-with one-of anima1s with [ biological.sex = "male" and life.history = "adult" ] ] ] ] ; random mating

  ; AGENT AGENCY
  ask anima1s [
    if ( not is.dead )
    [ make-decisions
      act ]]

  ; SIMULATION OUTPUT
  if output-results? [ output-results ]

  ; prints out current status of the simulation every 100 timesteps
  if ( ticks > 0 and ceiling (ticks / 100) = (ticks / 100) and any? anima1s ) [
    let print-text (word "Simulation " simulation-id " is now at " precision (ticks / plant-annual-cycle) 3 " years, "
      precision sum [energy.supply] of plants 3 " plant units, "
      precision mean [generation-number] of anima1s 3 " generations, and contains "
      count anima1s with [ not is.dead ] " living organisms.")
    print print-text
    if ( behaviorspace-run-number > 0 ) [ output-print print-text ] ]

  tick
end

to make-decisions
  let my-environment no-turtles

  ; ASPATIAL WORLD
  ifelse ( model-structure = "aspatial" ) [
    set my-environment up-to-n-of 100 turtles

  ][ ; SPATIAL WORLD
    if ( sun-status = "DAY" ) [
      set my-environment turtles with [ not is-group? self and not hidden? ] in-cone ( 5 * day.perception.range ) ( 300 * day.perception.angle )
      if (female.fertility = "pregnant") [ set my-environment turtles with [ not is-group? self and (not hidden? or member? self [my-offspring] of myself ) ] in-cone ( 5 * day.perception.range ) ( 300 * day.perception.angle ) ] ] ; mothers can see gestatees

    if ( sun-status = "NIGHT" ) [
      set my-environment turtles with [ not is-group? self and not hidden? ] in-cone ( 5 * day.perception.range ) ( 300 * day.perception.angle ) ; set to night.perception.angle once genotype includes this
      if (female.fertility = "pregnant") [ set my-environment turtles with [ not is-group? self and (not hidden? or member? self [my-offspring] of myself ) ] in-cone ( 5 * day.perception.range ) ( 300 * day.perception.angle ) ] ] ; mothers can see gestatees

    if (life.history = "gestatee" ) [ set my-environment turtles with [ [meta-id] of self = [meta-id] of myself or meta-id = [mother-identity] of myself ]] ; gestatees can only see themselves, and their mothers
  ]

  ; GENOTYPE READER
  set decision.vectors get-decisions-from-genoreader my-environment
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

to-report how-many-ticks? report 10 end

to-report world-patchiness
  let patch-diameter 5
  let patch-list []

  let i 0
  let j 0
  let i-next patch-diameter
  let j-next patch-diameter

  while [ j < world-height ] [
    while [ i < world-width ] [

      set patch-list lput count plants with [ xcor > i and xcor <= i-next and ycor > j and ycor <= j-next ] patch-list
      set i i-next
      set i-next ifelse-value (( i + patch-diameter ) > world-width ) [ world-width ] [ i + patch-diameter ] ]

    set i 0
    set i-next patch-diameter
    set j j-next
    set j-next ifelse-value (( j + patch-diameter ) > world-height ) [ world-height ] [ j + patch-diameter ] ]

  report ( variance patch-list / mean patch-list )
end

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


; --------------------------------------------------------------------------- ;
;
; DETERMINE GENETIC RELATEDNESS BETWEEN TWO INDIVIDUALS
;
; This routine determines the genetic relatedness between the caller and a
; specified individual in the population. For example, consider these four
; chromosomes.
;
; Locus         0 1 2 3 4 5 6 7 8 9
;
; Caller:   I:  y h m u n f q a x r
;          II:  c j s w v i l f a p
;
; Target:   I:  j l h y m d u t v c
;          II:  y u l x i s f p w a
;
; The first locus has one match, Caller I and Target II, and three non-matches,
; Caller I and Target I (y and J), Caller II and Target I (c and j), and
; Caller II to Target II (c y). This is considered a relatedness of
; <m>1/(1+3) = 0.25</m>, at the first locus. This is repeated across all loci
; and the average is returned.
;
; ENTRY:  'target' defines an individual to be compared the calling individual.
;         'identity.I' and 'identity.II' carry the genetic information to be
;          compared between caller and target.
;         'identity.I' and 'identity.II' both contain exactly 10 loci.
;
; EXIT:   'relatedness-with' returns an estimate of the degree of relatedness, as
;          fraction between 0 and 1.
;
; --------------------------------------------------------------------------- ;

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

  set completed.actions []
  foreach decision.vectors [ vector ->

    let decision-id item 0 vector
    let raw-value item 4 vector
    let energy-cost abs raw-value

    let energy-check ifelse-value ( model-structure = "freelunch" ) [ true ] [ energy.supply > energy-cost ] ; FREE LUNCH always passes energy check

    if ( not member? decision-id completed.actions and energy-check ) [
      set completed.actions lput decision-id completed.actions
      let target item 2 vector
      let action item 3 vector
      update-energy ( - energy-cost )

      ifelse ifelse-value ( target = nobody ) [ false ] [ distance target < ( size / 2 + [size] of target / 2 ) ]
      [ do-action action target raw-value ]
      [ if (target != nobody ) [ move-toward target raw-value ]]]]

end

to-report get-cost-and-process-decisions [ target action-name ]
  let decisions filter [ vector -> item 2 vector = target and item 3 vector = action-name and not member? (item 0 vector) completed.actions ] decision.vectors ; list of decisions filtered by target and action and not completed
  let ids map [ vector -> item 0 vector ] decisions ; list of ids from decisions
  set completed.actions sentence ids completed.actions ; add ids to list of completed actions
  let cost sum map [ vector -> item 4 vector ] decisions ; summation of costs for all decisions in the list
  report cost ; return this cost summation
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

to do-action [ action-code target cost ]

  carefully [
    if action-code = "maintain-body" [ maintain-body cost ]
    if action-code = "body-size" [ body-size cost ]
    if action-code = "body-shade" [ body-shade cost ]
    if action-code = "day-perception" [ day-perception cost ]
    if action-code = "night-perception" [ night-perception cost ]
    if action-code = "audio-perception" [ audio-perception cost ]
    if action-code = "day-perception-angle" [ day-perception-angle cost ]
    if action-code = "night-perception-angle" [ night-perception-angle cost ]
    if action-code = "audio-perception-angle" [ audio-perception-angle cost ]
    if action-code = "vocal-range" [ vocal-range cost ]
    if action-code = "conception-chance" [ conception-chance cost ]
    if action-code = "stomach-size" [ stomach-size cost ]
    if action-code = "mutation-rate" [ mutation-rate cost ]
    if action-code = "sex-ratio" [ sex-ratio cost ]
    if action-code = "litter-size" [ litter-size cost ]
    if action-code = "move-toward" [ move-toward target cost ]
    if action-code = "turn-right" [ turn-right cost ]
    if action-code = "turn-left" [ turn-right ( - cost ) ]
    if action-code = "go-forward" [ go-forward cost ]
    if action-code = "signal-a-on" [ signal-a-on cost ]
    if action-code = "signal-b-on" [ signal-b-on cost ]
    if action-code = "signal-b-on" [ signal-c-on cost ]
    if action-code = "check-infancy" [ check-infancy cost ]
    if action-code = "check-birth" [ check-birth cost ]
    if action-code = "check-juvenility" [ check-juvenility cost ]
    if action-code = "check-weaning" [ check-weaning cost ]
    if action-code = "check-adulthood" [ check-adulthood cost ]
    if action-code = "check-senescence" [ check-senescence cost ]
    if action-code = "supply-to" [ supply-to target cost ]
    if action-code = "demand-from" [ demand-from target cost ]
    if action-code = "eat" [ receive-from target cost ]
    if action-code = "join-group-of" [ join-group-of target cost ]
    if action-code = "leave-group" [ join-group-of target ( - cost ) ]
    if action-code = "pick-up" [ pick-up target cost ]
    if action-code = "put-down" [ pick-up target ( - cost ) ]
    if action-code = "cling-to" [ cling-to target cost ]
    if action-code = "squirm-from" [ cling-to target ( - cost ) ]
    if action-code = "help" [ help target cost ]
    if action-code = "attack" [ help target ( - cost ) ]
    if action-code = "mate-with" [ mate-with target cost ]

  ][  ]

end

;--------------------------------------------------------------------------------------------------------------------
; ACTIONS
;
; CALLER: the anima1 doing the action
; COST: the energy to be used to do the action, can be negative
; TARGET: the receiver of the action ( INTER-ACTIONS have a target and INTRA-ACTIONS do not )
;
; The 'Info' tab contains a summary of possible actions.
;
;--------------------------------------------------------------------------------------------------------------------

to maintain-body [ cost ] set living.chance get-updated-value living.chance cost end

to body-size [ cost ] set body.size get-updated-value body.size cost end

to body-shade [ cost ] set body.shade get-updated-value body.shade cost end

to day-perception [ cost ] set day.perception.range get-updated-value day.perception.range cost end

to night-perception [ cost ] set night.perception.range get-updated-value night.perception.range cost end

to audio-perception [ cost ] set audio.perception.range get-updated-value audio.perception.range cost end

to day-perception-angle [ cost ] set day.perception.angle get-updated-value day.perception.angle cost end

to night-perception-angle [ cost ] set night.perception.angle get-updated-value night.perception.angle cost end

to audio-perception-angle [ cost ] set audio.perception.angle get-updated-value audio.perception.angle cost end

to vocal-range [ cost ] set vocal.range get-updated-value vocal.range cost end

to conception-chance [ cost ] set conception.chance get-updated-value conception.chance cost end

to stomach-size [ cost ] set stomach.size get-updated-value stomach.size cost end

to mutation-rate [ cost ] set mutation.chance get-updated-value mutation.chance cost end

to sex-ratio [ cost ] set sex.ratio get-updated-value sex.ratio cost end

to litter-size [ cost ] set litter.size get-updated-value litter.size cost end

  ;--------------------------------------------------------------------------------------------------------------------
  ; MOVEMENT
  ;--------------------------------------------------------------------------------------------------------------------

to move-toward [ target cost ]
  if (target != nobody ) [
    let ycor-difference ([ycor] of target - [ycor] of self )
    let xcor-difference ([xcor] of target - [xcor] of self )
    let angle ifelse-value ( ycor-difference = 0 or xcor-difference = 0 ) [ random 360 ] [ atan ycor-difference xcor-difference ]
    if ( cost < 0 ) [ set angle angle - 180 ]
    set x.magnitude x.magnitude + (abs cost * sin angle)
    set y.magnitude y.magnitude + (abs cost * cos angle)
  ]
end

to turn-right [ cost ]
  ifelse ( cost > 0 )
  [ right ( 360 * cost ) ]
  [ left ( 360 * abs cost ) ]
end

to go-forward [ cost ]

  if ( cost < 0 ) [ right 180 ]

  let sum-weight size
  foreach carried.items [ object ->
    set sum-weight sum-weight + [size] of object ]
  set heading ( atan y.magnitude x.magnitude )
  set x.magnitude one-of [ 0.001 -0.001 ]
  set y.magnitude one-of [ 0.001 -0.001 ]
  let travel-distance (size * (sqrt (( 2 * abs cost ) / sum-weight )) )
  forward travel-distance
  foreach carried.items [ object ->
    if (object != nobody) [ ask object [ move-to myself ] ]]
  if output-results? [
    set distance-traveled distance-traveled + travel-distance
    if not member? patch-here cells-occupied [ set cells-occupied lput patch-here cells-occupied ]]
end

;--------------------------------------------------------------------------------------------------------------------
; SIGNALING
;--------------------------------------------------------------------------------------------------------------------

to signal-a-on [ cost ]
  set alpha.chance get-updated-value alpha.chance cost
  ifelse ( random-float 1.0 < alpha.chance ) [
    set alpha.signal true
  ][
    set alpha.signal false
  ]
end

to signal-b-on [ cost ]
  set beta.chance get-updated-value beta.chance cost
  ifelse ( random-float 1.0 < beta.chance ) [
    set beta.signal true
  ][
    set beta.signal false
  ]
end

to signal-c-on [ cost ]
  set gamma.chance get-updated-value gamma.chance cost
  ifelse ( random-float 1.0 < gamma.chance ) [
    set gamma.signal true
  ][
    set gamma.signal false
  ]
end

;--------------------------------------------------------------------------------------------------------------------
; LIFE HISTORY
;--------------------------------------------------------------------------------------------------------------------

to check-infancy [ cost ]
  set infancy.chance get-updated-value infancy.chance cost
  if ( life.history = "gestatee" and random-float 1.0 < infancy.chance ) [
    set mother-initiated-birth false
    ifelse ( mother = nobody )
    [ set is.dead true ]
    [ ask mother [ give-birth ]]
  ]
end

to check-birth [ cost ]
  set birthing.chance get-updated-value birthing.chance cost
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
  if (output-results?) [ ask current-group [ set total-birth-count total-birth-count + 1 ]]
end

to check-juvenility [ cost ]
  set juvenility.chance get-updated-value juvenility.chance cost
  if ( life.history = "infant" and random-float 1.0 < juvenility.chance ) [
    set mother-initiated-weaning false
    ifelse ( mother = nobody )
    [ update-to-juvenile ]
    [ ask mother [ wean-offspring ]]
  ]
end

to check-weaning [ cost ]
  set weaning.chance get-updated-value weaning.chance cost
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

to check-adulthood [ cost ]
  set adulthood.chance get-updated-value adulthood.chance cost
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
end

to check-senescence [ cost ]
  set senescency.chance get-updated-value senescency.chance cost
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

to supply-to [ target cost ]
  if ( target != self and is-anima1? target and female.fertility = "lactating" or female.fertility = "pregnant" ) [
    let other-supply-cost get-cost-and-process-decisions target "supply-to"
    let target-demand-cost [ get-cost-and-process-decisions myself "demand-from" ] of target
    let net-cost ( cost + other-supply-cost + target-demand-cost )
    if ( net-cost > 0 ) [ ask target [ receive-from myself net-cost ] ]
  ]
end

to demand-from [ target cost ]
  if ( target != self and is-anima1? target ) [
    let other-demand-cost get-cost-and-process-decisions target "demand-from"
    let target-supply-cost [ get-cost-and-process-decisions myself "supply-to" ] of target
    let net-cost ( cost + other-demand-cost + target-supply-cost )
    if ( net-cost > 0 ) [ receive-from target net-cost ]
  ]
end

to receive-from [ target cost ]
  if ( cost > 0 and is-anima1? target or is-plant? target ) [
    let energy-wanted get-updated-value stomach.size cost
    let energy-received ifelse-value ( energy-wanted < [ energy.supply ] of target ) [ energy-wanted ] [ [ energy.supply ] of target ]
    update-energy energy-received
    ask target [ update-energy ( - energy-received ) ]
    if ( is-plant? target ) [ collect-eating-data energy-received cost ]
  ]
end

to collect-eating-data [ energy-eaten cost-of-eating ]
  if (output-results?) [

    set foraging-gains foraging-gains + energy-eaten
    set foraging-losses foraging-losses + cost-of-eating

    ; group is tracking foraging performance of its members
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

to join-group-of [ target cost ]
  if ( is-anima1? target ) [
    let other-join-cost get-cost-and-process-decisions target "join-group-of"
    let target-join-cost [ get-cost-and-process-decisions myself "join-group-of" ] of target
    let net-cost ( cost + other-join-cost + target-join-cost )
    ifelse ( net-cost > 0 )
    [ join-group ([group.identity] of target) net-cost ]
    [ leave-group ([group.identity] of target) abs net-cost ]
  ]
end

to join-group [ group-id cost ]
  if ( random-float 1.0 < cost and group.identity != group-id ) [
    set previous-group-id group.identity
    set group.identity group-id
    if output-results? [ set group-transfers-list lput [meta-id] of current-group group-transfers-list ]
  ]
end

to leave-group [ group-id cost ]
  if ( random-float 1.0 < cost and group.identity != group-id ) [
    set previous-group-id group.identity
    hatch-groups 1 [
      initialize-group
      set my-creator [meta-id] of myself
      ask myself [ set group.identity [meta-id] of myself ]]
    ask previous-group [ if not any? group-members [ die ]]
  ]
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
  if ( target != self and is-anima1? target ) [
    let other-pickup-cost get-cost-and-process-decisions target "pick-up"
    let target-clingto-cost [ get-cost-and-process-decisions myself "cling-to" ] of target
    let net-cost ( cost + other-pickup-cost + target-clingto-cost )
    ifelse ( net-cost > 0 )
    [ if ( not member? target carried.items ) [ set carried.items lput target carried.items ] ]
    [ if ( member? target carried.items ) [ set carried.items remove target carried.items ] ]
  ]
end

to cling-to [ target cost ]
  if ( target != self and is-anima1? target ) [
    let other-clingto-cost get-cost-and-process-decisions target "cling-to"
    let target-pickup-cost [ get-cost-and-process-decisions myself "pick-up" ] of target
    let net-cost ( cost + other-clingto-cost + target-pickup-cost )
    ifelse ( net-cost > 0 )
    [ ask target [ if ( not member? myself carried.items ) [ set carried.items lput myself carried.items ] ]]
    [ ask target [ if ( member? myself carried.items ) [ set carried.items remove myself carried.items ] ]]
  ]
end

to attack [ target cost ]
  if ( is-anima1? target ) [
    let other-attack-cost get-cost-and-process-decisions target "attack"
    let my-attack-value size * ( cost + other-attack-cost )
    let target-help-cost [ get-cost-and-process-decisions myself "help" ] of target
    let target-help-value [size] of target * target-help-cost
    ( ifelse
      ( my-attack-value > 0 and target-help-value > 0 ) [ ask target [ maintain-body ( - my-attack-value ) ] ]
      ( my-attack-value > 0 and target-help-value <= 0 ) [ ask target [ maintain-body ( - my-attack-value - target-help-value ) ]]
      ( my-attack-value <= 0 and target-help-value > 0 ) [ maintain-body ( target-help-value + my-attack-value ) ]
      [])
  ]
end

to help [ target cost ]
  if ( is-anima1? target ) [
    let other-help-cost get-cost-and-process-decisions target "help"
    let my-help-value size * ( cost + other-help-cost )
    let target-attack-cost [ get-cost-and-process-decisions myself "attack" ] of target
    let target-attack-value [size] of target * target-attack-cost
    ( ifelse
      ( my-help-value > 0 and target-attack-value > 0 ) [ maintain-body ( - target-attack-value ) ]
      ( my-help-value > 0 and target-attack-value <= 0 ) [ ask target [ maintain-body ( my-help-value + target-attack-value ) ]]
      ( my-help-value <= 0 and target-attack-value > 0 ) [ maintain-body ( - target-attack-value + my-help-value ) ]
      [])
  ]
end

to collect-helping-data [ target cost ]
  ;  if ( output-results? and target != self ) [ ; Hamilton's Rule
  ;    let r relatedness-with target
  ;    ;print (word " me: " self " living.chance: " living.chance "  target: " target "   relatedness " relatedness-with target " ([living.chance] of target) " ([living.chance] of target)  "   value:"  value " update  " (get-updated-value [living.chance] of target ( value )) "  total: " (( get-updated-value [living.chance] of target ( value ) ) - [living.chance] of target ))
  ;    let B ( get-updated-value [living.chance] of target ( cost ) ) - [living.chance] of target ; how much target benefitted
  ;    let C living.chance - ( get-updated-value living.chance cost ) ; how much you *could have* benefitted by investing in yourself
  ;    set helping-benefit helping-benefit + r * B ; benefit to yourself based on relatedness to target
  ;    set helping-cost helping-cost + C ; cost of not helping yourself
  ;
  ;    if ( biological.sex = "female" and [biological.sex] of target = "female" and group.identity = [group.identity] of target ) [
  ;      set female-female-ingroup-helping female-female-ingroup-helping + cost
  ;    ]
  ;    if ( biological.sex = "male" and [biological.sex] of target = "male" and group.identity = [group.identity] of target ) [
  ;      set male-male-ingroup-helping male-male-ingroup-helping + cost
  ;    ]
  ;    if ( (( biological.sex = "female" and [biological.sex] of target = "male" ) or ( biological.sex = "male" and [biological.sex] of target = "female" )) and group.identity = [group.identity] of target ) [
  ;      set female-male-ingroup-helping female-male-ingroup-helping + cost
  ;    ]
  ;  ]
end

to mate-with [ target cost ]
  if ( is-anima1? target ) [
    let other-mate-cost get-cost-and-process-decisions target "mate-with"
    let target-mate-cost [ get-cost-and-process-decisions myself "mate-with" ] of target
    let net-cost ( cost + other-mate-cost + target-mate-cost )
    if ( net-cost > 0 and model-structure != "sower" ) [ ; SOWER calls conceive-with from an alternative source
      ifelse ( biological.sex = "female" )
      [ conceive-with target ]
      [ ask target [ conceive-with myself ]]
      collect-mating-data target
    ]
  ]
end

to collect-mating-data [ target ]
  if output-results? [
    set matings-list lput [meta-id] of target matings-list
    ask target [ set matings-list lput [meta-id] of myself matings-list ]]
end

to conceive-with [ target ] ; FEMALE PROCEDURE
  if biological.sex = "female" and female.fertility = "cycling" and life.history = "adult" and [life.history] of target = "adult" and [biological.sex] of target = "male" [
    set label "!"
    if random-float 1.0 < (mean (list conception.chance [conception.chance] of target)) [
      let preferred-litter 10 ^ ( mean (list litter.size [litter.size] of target))
      let floor-litter floor preferred-litter
      let percent-litter preferred-litter - floor-litter
      let my-litter-size ifelse-value ( random-float 1.0 < percent-litter ) [ floor-litter + 1 ] [ floor-litter ]
      hatch-anima1s my-litter-size [ initialize-from-parents myself target ]
      set female.fertility "pregnant"
      collect-conception-data target
  ]]
end

to collect-conception-data [ target ]
  if output-results? [
    set conceptions-list lput [meta-id] of target conceptions-list
    ask target [ set conceptions-list lput [meta-id] of myself conceptions-list ]]
end

to initialize-from-parents [ m f ]
  set meta-id random 9999999
  set hidden? true
  set is.dead false
  ask m [ pick-up myself 1.0 ]
  set biological.sex ifelse-value ( random-float 1.0 < mean (list [sex.ratio] of m [sex.ratio] of f) ) ["male"] ["female"]
  set shape ifelse-value ( biological.sex = "female" ) ["circle"] ["triangle"]
  set generation-number [generation-number] of m + 1
  ifelse ( model-structure = "noevolution" )
  [ set chromosome.I [chromosome.I] of m
    set chromosome.II [chromosome.II] of m ]
  [ setup-chromosomes-from m f ]
  set group.identity [group.identity] of m
  set natal-group-id group.identity
  set mother-identity [meta-id] of m
  set father-identity [meta-id] of f
  set energy.supply 1
  receive-from mother 1.0 ; delete
  set life.history "gestatee"
  set female.fertility " "
  set label-color black
  set age 0
  set body.size 0.01
  set body.shade 0
  set day.perception.range 0
  set night.perception.range 0
  set audio.perception.range 0
  set stomach.size 0.1
  set mutation.chance 0.10
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
  set carried.items []
  set decision.vectors []
  set x.magnitude one-of [ 0.001 -0.001 ]
  set y.magnitude one-of [ 0.001 -0.001 ]
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
  set adult-mutation-chance 0
  set adult-sex-ratio 0
  set adult-litter-size 0
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
  set natal-group-size [group-size] of current-group
  set group-transfers-list []
  set matings-list []
  set conceptions-list []
  set cells-occupied []
  set infanticide-list []
  set previous-group-id 0
  if ( model-structure = "idealform" ) [ set-phenotype-to-ideal-form ]
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
              ( choice = 2 and random-index = index and model-structure != "uninvadable" ) [ set new-allele lput get-mutation-from-genoreader codon new-allele ] ; mutate codon
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
@#$#@#$#@
GRAPHICS-WINDOW
6
85
711
791
-1
-1
13.94
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
setup generate-simulation-id
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
100.0
true
false
"" "if any? anima1s [\n\nif (useful-commands = \"age-histogram\") [\n  clear-plot\n  set-plot-x-range 0 max [age] of anima1s with [ is.dead = false ] + 1000\n  set-plot-y-range 0 10 ]\n  \nif (useful-commands = \"lotka-volterra\") [\n  let plts ( plant-quality * count plants / 100 )\n  let anls count anima1s\n  let max-value max (list plts anls )\n  set-plot-x-range 0 10\n  set-plot-y-range 0 ( max-value + 10 ) ]\n ]\n  \n"
PENS
"age" 1000.0 1 -16777216 true "" "if (useful-commands = \"age-histogram\") [ histogram [age] of anima1s with [ is.dead = false] ]"
"plants" 1.0 0 -13840069 true "" "if (useful-commands != \"age-histogram\") [ plot ((plant-quality * (sum [energy.supply] of plants ))/ 100 ) ]"
"organisms" 1.0 0 -6459832 true "" "if (useful-commands != \"age-histogram\") [ plot ( count anima1s ) ]"

INPUTBOX
720
10
967
102
documentation-notes
Population file p-20-03-25-01 imported. Population file p-20-03-25-01 imported. New population p-20-03-25-01 saved. Population file p-20-03-25-01 imported. Population file p-20-03-25-01 imported. New population p-20-03-25-01 saved. New population p-20-03-25-01 saved. New population p-20-03-25-01 saved. New population p-2020-03-25-20 saved. Population file fastbreeders6 imported. Population file fastbreeders6 imported. Population file fastbreeders6 imported. 
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
720
383
1217
791
11

SLIDER
974
155
1217
188
plant-minimum-neighbors
plant-minimum-neighbors
0
8
4.0
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
sun-status
17
1
11

INPUTBOX
974
230
1097
299
population
p-20-03-25-01
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
1162
230
1217
263
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
1162
266
1217
299
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
720
328
908
373
useful-commands
useful-commands
"help-me" "--------" "lotka-volterra" "age-histogram" "metafile-report" "verify-code" "check-runtime" "simulation-report" "clear-plants" "setup-plants" "clear-population" "view-genotype" "view-decisions" "add-allele" "delete-allele" "population-report"
6

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
1162
340
1217
373
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
1162
304
1217
337
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
974
118
1217
151
plant-quality
plant-quality
.01
1
0.5
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
1
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
5605583
1
0
String (commands)

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

The actions listed above are athe range of possible actions that an agent can take. However, whether an agent performs these actions, how much effort they put into doing so, and who they target is up to their genotype. An indefinite number of genotype file configurations are possible, as long as they include the following: (1) each row represents one allele, (2) these alleles represent self-contained procedures that generate decision-vectors from considering the environment as input, and (3) each row contains a list of codons that can be altered during recombination and mutation. This version of B3GET comes with two genotype file extensions: sta7us and g3notype (beta version). Specific information about each file type can be found within those extension files.

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
