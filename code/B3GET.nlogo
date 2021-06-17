; --------------------------------------------------------------------------------------------------------- ;
;
;   888888ba  d8888b.  .88888.   88888888b d888888P
;   88    `8b     `88 d8'   `88  88           88
;  a88aaaa8P'  aaad8' 88        a88aaaa       88
;   88   `8b.     `88 88   YP88  88           88
;   88    .88     .88 Y8.   .88  88           88
;   88888888P d88888P  `88888'   88888888P    dP
;
; --------------------------------------------------------------------------------------------------------- ;
;
; This program is to simulate populations of virtual organisms evolving over generations, whose evolutionary
; outcomes reflect the selection pressures of their environment. The program is divided into four main
; sections:
;
;  1. Setup. Prepares for operation of the program.
;
;  2. Go. Advance one timestep across the enviroment.
;
;  3. Engine. A collection of subroutines allowing living agents assess their
;       environments and advance on time step accordingly.
;
;  4. Actions. A collection of subroutines to carry out necessary steps, as
;       called for in 'engine' subroutines.
;
;  5. Genotype reader. Converts environmental and genotype information into a
;       set of decision vectors -- who is targeted, action to be taken, energy
;       for such action, and so forth (sta2us and gat3s).
;
;  6. Service. Includes necessary processing for the interface -- button clicks,
;       output, internal operation checking, and so forth. (analysis, data, interface,
;       commands, plots, results, selection, verification)
;
; --------------------------------------------------------------------------------------------------------- ;

extensions [ csv profiler table time ]

__includes [
  "extensions/analysis.nls"
  "extensions/data.nls"
  "extensions/gat3s.nls"
  "extensions/interface.nls"
  "extensions/results.nls"
  "extensions/selection.nls"
  "extensions/sta2us.nls"
  "extensions/verification.nls"
]

; --------------------------------------------------------------------------------------------------------- ;
;
; CHARACTERISTICS OF MOVABLE AGENTS IN THE PROGRAM
;
; In this program, plants are part of the patch structure of the system and characteristics animals are
; defined here. The term "anima1" is used with the digit "1" instead of an "l" to mark these
; as virtual animals.
;
; --------------------------------------------------------------------------------------------------------- ;

breed [ anima1s anima1 ]

anima1s-own [

  my.identity                      ; Unique identification number for each individual.

                                   ; PHENOTYPE VARIABLES THAT ARE VISIBLE TO ALL INDIVIUDALS

  biological.sex                   ; Either male or female
  life.history                     ; Either gestatee, infant, juvenile or adult
  fertility.status                 ; Either cycling, pregnant or lactating
  group.identity                   ; Number that specifies both group affiliation and color
  is.alive                         ; Either true or false
  yellow.signal                    ; When true, yellow signal is visible
  red.signal                       ; When true, red signal is visible
  blue.signal                      ; When true, blue signal is visible
  body.size                        ; Current size of individual
  body.shade                       ; Current shade of color showing (base color is determined by group.identity)
  is.resting                       ; When true, cannot perform "active" actions

  identity.I                       ; The identity chromosomes are inherited from parents and used
  identity.II                      ; to calculate genetic relatedness between two individuals
  carried.items                    ; List of objects carried (eg gestatee, infants, parasites, etc)

                                   ; PHENOTYPE VARIABLES THAT ARE HIDDEN FROM ALL INDIVIDUALS BUT SELF

  hidden.chance                    ; Probability that an animal will become hidden from view
  fully.decayed                    ; When true, individual is no longer available as food
  survival.chance                  ; Probability of surviving to the next timestep
  energy.supply                    ; Current amount of energy units available for performing actions
  bite.capacity                    ; Amount of energy units that can be consumed from a plant per timestep
  mutation.chance                  ; Probability of offspring having a mutation for each gene locus
  sex.ratio                        ;
  litter.size                      ;
  conception.chance                ;
  visual.angle                     ;
  visual.range                     ;
  day.perception                   ;
  night.perception                 ;
  yellow.chance                    ;
  red.chance                       ;
  blue.chance                      ;
  birthing.chance                  ;
  weaning.chance                   ;
  infancy.chance                   ;
  juvenility.chance                ;
  adulthood.chance                 ;
  x.magnitude                      ;
  y.magnitude                      ;

  chromosome.I                     ;
  chromosome.II                    ;

  my.environment                   ;
  decision.vectors                 ;
  actions.completed                ;

                                   ; TRACKING VARIABLES FOR USER USE ONLY AND ARE HIDDEN FROM ALL INDIVIDUALS

  age.in.ticks                     ;
  generation.number                ;
  my.mother                        ;
  mother.identity                  ;
  father.identity                  ;
  natal.group.identity             ;
  natal.group.size                 ;
  death.group.identity             ;
  death.group.size                 ;
  ticks.at.conception              ;
  ticks.at.birth                   ;
  ticks.at.weaning                 ;
  ticks.at.sexual.maturity         ;
  ticks.at.death                   ;
  adult.hidden.chance              ;
  adult.survival.chance            ;
  adult.body.size                  ;
  adult.body.shade                 ;
  adult.energy.supply              ;
  adult.bite.capacity              ;
  adult.mutation.chance            ;
  adult.sex.ratio                  ;
  adult.litter.size                ;
  adult.conception.chance          ;
  adult.visual.angle               ;
  adult.visual.range               ;
  adult.day.perception             ;
  adult.night.perception           ;
  adult.yellow.chance              ;
  adult.red.chance                 ;
  adult.blue.chance                ;
  distance.traveled                ;
  mother.initiated.birth           ;
  mother.initiated.weaning         ;
  whole.related.help.cost          ;
  half.related.help.cost           ;
  fourth.related.help.cost         ;
  eighth.related.help.cost         ;
  not.related.help.cost            ;
  whole.related.attack.cost        ;
  half.related.attack.cost         ;
  fourth.related.attack.cost       ;
  eighth.related.attack.cost       ;
  not.related.attack.cost          ;
  foraging.gains                   ;
  total.energy.gains               ;
  total.energy.cost                ;
  help.from.history                ;
  attack.from.history              ;
  copulations.history              ;
  conceptions.history              ;
  group.transfers.history          ;
  infanticide.history              ;
  cause.of.death ]                 ;

; --------------------------------------------------------------------------------------------------------- ;
;
; CHARACTERISTICS OF STATIONARY AGENTS IN THE PROGRAM
;
; In this program, plants are part of the patch structure of the system,
; defined here, in contrast with animals, which are defined above.
;
; --------------------------------------------------------------------------------------------------------- ;

patches-own [
  pmy.identity                     ;
  pterminal.energy                 ;
  penergy.supply                   ;
  pgroup.identity                  ;
  pgroups.here ]                   ;

; --------------------------------------------------------------------------------------------------------- ;
;
; VARIABLES AVAILABLE TO THE ENTIRE PROGRAM
;
; --------------------------------------------------------------------------------------------------------- ;

globals [
  model-version                    ;
  model-structure                  ;
  simulation-id                    ;

  ; WORLD SETTINGS
  deterioration-rate               ;
  maximum-visual-range             ;
  base-litter-size                 ;

  ; TRACKING VARIABLES
  start-date-and-time              ;
  verification-results             ;

  timestep-interval
  plant-abundance-record           ;
  plant-patchiness-record          ;
  population-size-record           ;

  solar-status                     ;
  current-season                   ;

  selected-display
]

; ========================================================================================================= ;
;
;                      dP
;                      88
;  .d8888b. .d8888b. d8888P dP    dP 88d888b.
;  Y8ooooo. 88ooood8   88   88    88 88'  `88
;        88 88.  ...   88   88.  .88 88.  .88
;  `88888P' `88888P'   dP   `88888P' 88Y888P'
;                                    88
;                                    dP
; ========================================================================================================= ;

; --------------------------------------------------------------------------------------------------------- ;
;
; SETUP GLOBAL PARAMETERS AT THE START OF A NEW SIMULATION
;
;
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to setup-parameters
  set model-version "1.2.0-Beta2"                              ;
  set deterioration-rate -0.01                                 ;
  set maximum-visual-range 5                                   ;
  set base-litter-size 10                                      ;
  set model-structure [ "baseline" ]                           ;
  set verification-results []                                  ;
  set timestep-interval 100                                 ;
  set plant-abundance-record []                                ;
  set plant-patchiness-record []                               ;
  set population-size-record []                                ;
  set start-date-and-time date-and-time                        ;
  set selected-display "groups"                                ;
  reset-timer                                                  ;
end

; --------------------------------------------------------------------------------------------------------- ;
;
; SETUP PLANTS AT THE START OF A NEW SIMULATION
;
;
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to setup-patches
  let initial-group-list []
  repeat 100 [ set initial-group-list lput 0 initial-group-list ]
  ask patches [
    set pterminal.energy random-float plant-quality
    set penergy.supply pterminal.energy
    set pmy.identity random 9999999
    set pgroups.here [ ]
    set pgroups.here initial-group-list
    update-patch-color ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
;
;
;
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to setup ; [ new-simulation-id ]
         ; save world file of old simulation before starting new simulation under following conditions
         ;if ( simulation-id != 0 and behaviorspace-run-number = 0 and output-results? = true ) [ record-world ]

  clear-all
  reset-ticks
  set simulation-id generate-simulation-id
  setup-parameters
  setup-patches
  if ( path-to-experiment = "" ) [ set path-to-experiment "../results/" ]
  import-population ; test
  import-genotype
  output-print (word " Simulation " simulation-id " " behaviorspace-run-number " end setup at "  date-and-time )
end

; ========================================================================================================= ;
;
;  .d8888b. .d8888b.
;  88'  `88 88'  `88
;  88.  .88 88.  .88
;  `8888P88 `88888P'
;       .88
;   d8888P
;
; ========================================================================================================= ;

; --------------------------------------------------------------------------------------------------------- ;
;
; MAIN SUBROUTINE CALLED ONCE EACH TIMESTEP
;
; This is the main repetitive entry for the program. Time in this program is measured in
; abstract years and divided into abstract days, then further divided into
; abstract hours. For decimal simplicity, there are 100 days per year and 10
; hours per day. These divisions are arbitrary and can be modified as
; parameters to the program. Space in this program is divided into fixed-size
; square patches arranged in a rectangular grid, as is standard in the NetLogo
; programming language.<https://en.wikipedia.org/wiki/NetLogo/>
;
; The program steps forward hour by hour, and this routine is called once per
; step. In each step, the program works across the spatial structure to
; determine the dynamics during the present time step. This program, therefore,
; essentially implements an Euler method of
; simulation<https://en.wikipedia.org/wiki/Euler_method/> across a spatial
; structure, making it, in the most basic case, essentially a
; partial-differential equation solver. Considerations such as the
; Courant condition<https://en.wikipedia.org/wiki/Courant%E2%80%93Friedrichs%E2%80%93Lewy_condition/>
; must therefore be observed.
;
; ENTRY: 'model-structure' defines the overall conditions for the simulation.
;          'no-plants' means plant growth is inhibited.
;          'reaper' means individuals are randomly selected to die when the
;            total number of individuals across the entire spatial structure
;            exceeds 'n' individuals, where 'n' is 100.
;          'sower' means individuals are randomly selected to reproduce
;            until the total number of individuals across the entire spatial
;            structure exceeds 'n' individuals, where 'n' is 100.
;          'aspatial' means that interactions among individuals are global, not
;            restricted to neighboring spatial patches.
;          'free-lunch' means that individuals may perform behaviors even if
;            depleted of energy.
;          'no-evolution' means the evolutionary mechanisms are disabled.
;          'ideal-form' means simulated individuals conceived in the model
;            acquire traits representing the average of the entire population
;            at the time.
;          'uninvadable' means evolution occurs without mutation, only with
;            recombination and other simulated genetic mechanisms.
;        'anima1s-own' is initialized and prepared for use. In particular,
;          'fully.decayed', 'carried.items', ... are used. [FILL IN THE ...'S]
;        'recent-decisions-made' contains the recent history of the decision
;          vectors created by all agents.
;        'recent-actions-completed' contains the recent history of actions
;          completed by all agents.
;        'how-many-ticks?' contains the number ticks to be tracked variables
;          carrying recent history.
;
; EXIT:
;        'recent-decisions-made' and 'recent-actions-completed' are updated
;          with the most recent events, up to a maximum of 'how-many-ticks?'
;
; --------------------------------------------------------------------------------------------------------- ;

to go

  if ( ticks = 0 ) [                                           ; Update metafile when a
    update-metafile                                            ; new simulation starts.
    "simulation"
    simulation-id
    "SIMULATION STARTED" ]

  global-update                                                ; Update the current state of the
  plants-update                                                ; plants and animals in the
  animals-update                                               ; simulated world.

  ask anima1s with [ is.alive ] [ consider-environment ]       ; Allow living animals to view their environment,
  ask anima1s with [ is.alive ] [ make-decisions ]             ; make decisions according to their genotype, and
  ask anima1s with [ is.alive ] [ do-actions ]                 ; perform those actions if they have enough energy.

  if selection-on? [ artificial-selection ]                    ; Impose artificial selection on animals.
  if output-results? [ output-results ]                        ; Generate data from current simulation state.
  display-simulation-status                                    ; Display simulation status.

  tick                                                         ; Move forward one simulated hour.

end

; --------------------------------------------------------------------------------------------------------- ;
;
; DISPLAY THE CURRENT SIMULATION STATUS FOR USER
;
; This subroutine periodically displays the current status of the simulation. Typically it is called at
; least once per timestep, and it watches for transitions to the next period, the length of which is
; defined in an external variable.
;
; ENTRY:  The simulation is currently running with living individuals present.
;
;         'timestep-interval' is set to the number of ticks between displays. Typically this is
;            set to 100.
;
;         'ticks' is the number of current time step. On the first call, its value is 0.
;
; EXIT:   The simulation status is displayed. In particular, the number of simulated years thus far, the
;           number of units of plant energy, the number of animals living, and the number of generations
;           that have transpired are displayed.
;
; --------------------------------------------------------------------------------------------------------- ;

to display-simulation-status
  if ( ticks-on-interval? timestep-interval ) [             ; Determine if periodic condition is met.

    print ( word                                               ; Diplay the following information:
      "Simulation " simulation-id                              ; Current simulation identification.
      " is now at "
      precision (ticks / plant-annual-cycle) 3                 ; The number of simulated years thus far.
      " years, "
      precision sum [penergy.supply] of patches 3              ; The number of units of plant energy.
      " plant units, "
      precision mean ( sentence
        [generation.number] of anima1s with [ is.alive ]       ; The number of generations that have transpired.
        0 ) 3
      " generations, and contains "
      count anima1s with [ is.alive ] " living organisms." )   ; The number of living individuals.

  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; DETERMINE IF THE CURRENT TIMESTEP LANDS ON A GIVEN INTERVAL
;
; This subroutine is used to determine when periodic events should occur. Specifically, it calculates
; if the current timestep (ticks) is a multiple of a given interval value.
;
; ENTRY:  period | The number of ticks between intervals.
;
; EXIT:   This subroutine returns true if the current timestep is on the interval and false otherwise.
;
; --------------------------------------------------------------------------------------------------------- ;

to-report ticks-on-interval? [ period ]
  report remainder ticks period = 0  ; ceiling ( ticks / period ) = ( ticks / period )
end

; --------------------------------------------------------------------------------------------------------- ;
;          _           _               _
;   __ _  | |   ___   | |__     __ _  | |
;  / _` | | |  / _ \  | '_ \   / _` | | |
; | (_| | | | | (_) | | |_) | | (_| | | |
;  \__, | |_|  \___/  |_.__/   \__,_| |_|
;  |___/
; --------------------------------------------------------------------------------------------------------- ;

; --------------------------------------------------------------------------------------------------------- ;
;
; PERFORM ALL GLOBAL UPDATES TO THE CURRENT SIMULATION
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to global-update

  carefully [

    set solar-status ifelse-value                               ; Identify whether it is day or night
    ( ( cos (( 360 / plant-daily-cycle ) * ticks)) > 0 )        ; based on the current timestep and
    [ "DAY" ]                                                   ; user setting for day length.
    [ "NIGHT" ]

    set current-season ( cos (( 360 / plant-annual-cycle ) * ticks )) ; Calculate the current season
                                                                ; which oscillates sinusoidally with time.

    if ( ticks-on-interval? timestep-interval ) [            ; Periodically record data on the following:

      set plant-abundance-record lput                           ;
      sum [penergy.supply] of patches
      plant-abundance-record

      set plant-patchiness-record lput
      plant-patchiness
      plant-patchiness-record

      set population-size-record lput
      count anima1s with [ is.alive ]
      population-size-record
    ]

  ] [ print ( word "GLOBAL UPDATE ERROR: " error-message ) ]    ; If error occurs, print out error message.

end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALCULATE A NEW VALUE ....
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to-report get-updated-value [ current-value update-value ]
  let report-value ifelse-value ( current-value < 0.00001 ) [ 0.00001 ] [ ifelse-value ( current-value > 0.99999 ) [ 0.99999 ] [ current-value ] ]
  ifelse update-value < 0
  [ set report-value ( report-value ^ (1 + abs update-value) ) ]
  [ set report-value ( report-value ^ (1 / ( 1 + update-value) )) ]
  report report-value
end

to-report generate-simulation-id report ( word "s" generate-timestamp ) end

to-report generate-timestamp
  let string-to-report ""
  let time-difference time:difference-between (time:create "1970-01-01 00:00:00.0") (time:create "") "seconds"
  set time-difference time-difference + random 1000 - random 1000
  let hex-list [ "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" ]

  while [ time-difference > 0 ] [
    let unix-remainder floor remainder time-difference 36
    set string-to-report ( word item unix-remainder hex-list string-to-report )
    set time-difference floor ( time-difference / 36 ) ]

  report string-to-report
end

; --------------------------------------------------------------------------------------------------------- ;
;          _                   _
;  _ __   | |   __ _   _ __   | |_   ___
; | '_ \  | |  / _` | | '_ \  | __| / __|
; | |_) | | | | (_| | | | | | | |_  \__ \
; | .__/  |_|  \__,_| |_| |_|  \__| |___/
; |_|
; --------------------------------------------------------------------------------------------------------- ;
; --------------------------------------------------------------------------------------------------------- ;
;
; PRINT OUT THE CURRENT SIMULATION STATUS FOR USER
;
; Prints out current status of the simulation every 100 timesteps.
;
; ENTRY: Simulation must be currently running.
;
; EXIT: B3GET prints out simulation information.
;
; --------------------------------------------------------------------------------------------------------- ;

to plants-update

  carefully [

    ifelse ( member? "no-plants" model-structure )   ; If the model-structure setting contains "no-plants"
    [
      ask patches with [ pcolor != brown + 1 ]       ; Then for patches that are not currently brown
      [ set pcolor brown + 1 ]                       ; Set their color to brown
    ][
      update-patches                                 ; Otherwise update plant growth in all patches
    ]

  ] [ print ( word "PLANTS UPDATE ERROR: " error-message ) ]    ; If error occurs, print out error message.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; UPDATE THE ATTRIBUTES OF ALL PLANTS FOR THIS TIMESTEP
;
; This routine is called once per time step to update all individual cells in
; the environment. This update only involves plants.
;
; Each plant has a maximum energy that it can accumulate at any moment, and that
; maximum energy varies with season and other parameters. In this model the
; present energy of a plant increases by a fixed amount each time step as part of
; plant growth, and decreases by plants being consumed by animals, and also can
; decrease by reductions in the maximum energy allowed.
;
;
; ENTRY: 'ticks' defines the time steps thus far (standard in NetLogo).
;        'plant-annual-cycle' defines the number of time steps in an abstract
;          year (typically 1000).
;        'penergy.supply' defines the amount of energy in each patch that is
;          available for food. This accumulates in each cell at the rate of
;          1 unit per abstract year, with an upper bound.
;        'pterminal.energy' is the upper bound.
;
; EXIT:  'season' defines the abstract season at the current value of 'tick'
;          on a cosine scale.
;         -1 to  0 represents spring (-90 degrees to   0 degrees)
;          0 to  1 represents summer (  0 degrees to  90 degrees)
;          1 to  0 represents fall   ( 90 degrees to 180 degrees)
;          0 to -1 represents winter (180 degrees to -90 degrees)
;
; --------------------------------------------------------------------------------------------------------- ;

to update-patches

  ifelse ( solar-status = "DAY" ) [
    let season current-season
    let density ( sum [penergy.supply] of patches ) / ( count patches * plant-quality )
    ask patches [
      update-terminal-energy season density
      set penergy.supply penergy.supply + plant-quality / plant-annual-cycle
      if penergy.supply > pterminal.energy [ set penergy.supply pterminal.energy ]
      update-patch-color ]
  ][
    ask patches [ update-patch-color ]
  ]

end

; --------------------------------------------------------------------------------------------------------- ;
;
; INCREASE OR DECREASE A PLANT'S TERMINAL ENERGY VALUE
;
; This subroutine is called once per time step per plant.
; As plants grow, their energy content increases to a maximum, their "terminal
; energy," and during winter that energy can decrease. Also, as they are eaten,
; their energy content decreases, but not their terminal energy.
; This subroutine calculates the change in energy content, but also updates the
; plant terminal energy, which can NOT QUITE RIGHT -- FIX...
;
; ENTRY: 'plant-season' specifies the abstract season on a cosine scale, as
;          defined above.
;        'plant-density' specifies the average energy per plant, as a proportion
;          of the maximum plant quality.
;
; --------------------------------------------------------------------------------------------------------- ;

to update-terminal-energy [ plant-season plant-density ]

  let seasonal-factor ( ( plant-seasonality * plant-season + 1 ) / 2 )
  let optimal-neighbor-energy ( plant-minimum-neighbors + plant-maximum-neighbors ) / 2
  let neighbor-energy-sd ( optimal-neighbor-energy - plant-minimum-neighbors )
  let neighbor-energy ( mean (list (sum [penergy.supply] of neighbors) (sum [pterminal.energy] of neighbors) ) / plant-quality ) ; fix language in ch 2?

  let probability-up ifelse-value ( neighbor-energy-sd = 0 ) [ 0 ] [ ( 1 * e ^ ( - (( neighbor-energy - optimal-neighbor-energy ) ^ 2 ) / ( 2 * ( neighbor-energy-sd ^ 2 ) )) ) ]
  let y ( ( plant-daily-cycle * plant-quality ) / plant-annual-cycle ) * ( plant-density * ( 2 * probability-up - 1 ) + seasonal-factor - plant-density )

  set pterminal.energy ( pterminal.energy + random-float y )
  if pterminal.energy >= plant-quality [ set pterminal.energy plant-quality ]
  if pterminal.energy <= 0.000 [ set pterminal.energy 0.000 ]

end

; --------------------------------------------------------------------------------------------------------- ;
;
; UPDATE COLOR OF A PLANT BASED ON ITS CURRENT ENERGY SUPPLY AND TERMINAL ENERGY
;
; This soubroutine is called by each plant once per timestep and updates the
; color of the plant, which can be viewed in the interface.
;
; --------------------------------------------------------------------------------------------------------- ;

to update-patch-color
  set pcolor scale-color green (( ( pterminal.energy + penergy.supply ) / 2 ) / plant-quality ) 1.5 -0.25
end

; --------------------------------------------------------------------------------------------------------- ;;
;  _               _   _           _       _                   _
; (_)  _ __     __| | (_) __   __ (_)   __| |  _   _    __ _  | |  ___
; | | | '_ \   / _` | | | \ \ / / | |  / _` | | | | |  / _` | | | / __|
; | | | | | | | (_| | | |  \ V /  | | | (_| | | |_| | | (_| | | | \__ \
; |_| |_| |_|  \__,_| |_|   \_/   |_|  \__,_|  \__,_|  \__,_| |_| |___/
;
; --------------------------------------------------------------------------------------------------------- ;

; --------------------------------------------------------------------------------------------------------- ;
;
; PERFORM ALL GLOBAL UPDATES FOR ANIMA1S
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to animals-update

  carefully [

    ifelse ( member? "reaper" model-structure ) [
      if (( count anima1s with [ is.alive ] - 100 ) > 0 ) [ ask n-of ( count anima1s with [ is.alive ] - 100 ) anima1s [ set-to-dead ]]
      ask anima1s with [ not fully.decayed and is.alive = false ] [ check-mortality ]
    ]
    [ ask anima1s with [ not fully.decayed ] [ check-mortality ] ]


    ; this organization ensures that 100 individuals stay alive at all times

    if ( member? "stork" model-structure ) [

      ; UPDATE AGENTS : reproduction ( when model-structure = "stork" )
      if (count anima1s with [ is.alive ] < 100 ) ; random mating
      [ repeat ( 100 - count anima1s with [ is.alive ]) [
        if ( ( count anima1s with [ biological.sex = "male" and life.history = "adult" and is.alive ] > 0 )                                        ; if there is at least one adult male
          and ( count anima1s with [ biological.sex = "female" and life.history = "adult" and fertility.status = "cycling" and is.alive ] > 0 ) )  ; and one adult cycling female left in the simulation,
        [ ask one-of anima1s with [ biological.sex = "female" and life.history = "adult" and fertility.status = "cycling" and is.alive ]           ; randomly select one adult male and one cycling female,
          [ conceive-with ( one-of anima1s with [ biological.sex = "male" and life.history = "adult" and is.alive ] ) ]]]]                  ; and the female spontaneously conceives a new offspring from their alleles
    ]

    ; UPDATE AGENTS
    ask anima1s with [ not fully.decayed ] [
      set my.environment [] set decision.vectors [] set actions.completed []                         ; Clear memory from the previous timestep.
      set age.in.ticks age.in.ticks + 1                                                              ; Advance the individual's age by one timestep.
      deteriorate                                                                                    ; Advance the individual's decay for this time step.
                                                                                                     ; NEXT: DO THE RIGHT THING FOR SUBROUTINE "deteriorate" <------------------------------------
      update-appearance                                                                              ; Update the visual appearance to match.

      if ( not empty? carried.items ) [             ; If the carrier has move, bring all the carried items along.
        foreach carried.items [ itm ->
          ask itm [ move-to myself ]
        ]
      ]

      ;    if ( [pgroup.identity] of patch-here != group.identity and count anima1s with [ group.identity = [group.identity] of myself ] > 1 ) [ ; is this code costly?
      ;      ask patch-here [ set pgroup.identity [group.identity] of myself ]] ; update territories of groups

      ask patch-here [
        set pgroups.here but-first pgroups.here
        set pgroups.here lput [group.identity] of myself pgroups.here
        set pgroup.identity one-of modes pgroups.here
      ]

    ]

  ] [ print ( word "INDVIDUALS UPDATE ERROR: " error-message ) ]    ; If error occurs, print out error message.

end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER'S HEALTH DECREASES FROM GLOBAL EFFECTS
;
; The caller's body decays to a degree set by the global variable deterioration-rate. This reduces
; the caller's chance of survival. The deterioration-rate is a negative number ( i.e. -0.001 )
;
; ENTRY:  survival.chance | The caller's unique value for their chance of survival to the next timestep.
;         deterioration-rate | A global variable set by the user to drive the rate of decay in environment.
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to deteriorate
  set survival.chance get-updated-value survival.chance deterioration-rate
end

; --------------------------------------------------------------------------------------------------------- ;
;
; DETERMINE WETHER THE BODY HAS DETERIORATED TO THE POINT OF DEATH OR COMPLETE DECAY
;
; The caller determines whether they will survive to the next timestep. If the caller is still alive then
; the bodily decay process begins. If the decay process is already underway, the caller's body is now marked
; as fully decayed and is not longer present in the environment.
;
; ENTRY:  survival.chance
;         is.alive
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to check-mortality
  if ( random-float 1.0 > survival.chance ) [                  ; Determine if they survive this timestep.
    ifelse ( is.alive = true )                                 ; If this occurs when still alive,
    [ set-to-dead ]                                            ; mark individual as dead.
    [ remove-from-environment ]]                               ; If already dead, fully decay body.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; UPDATE CALLER TO BE DEAD
;
; The caller's status is updated to dead, which triggers some tracking records and visual changes. The
; caller's state is perminantly set todead.
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to set-to-dead
  set is.alive false
  set ticks.at.death ticks
  set death.group.identity group.identity
  set death.group.size (count anima1s with [ is.alive and group.identity = [group.identity] of myself ])
  set label "x"
  set my.environment []
  set decision.vectors []
  set actions.completed []
end

; --------------------------------------------------------------------------------------------------------- ;
;
; U
;
; The
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to remove-from-environment
  if ( ticks.at.death = 0 ) [ set ticks.at.death ticks ]
  ask anima1s with [ member? myself carried.items ] [ set carried.items remove myself remove-duplicates carried.items ] ; remove me from other agent's carried.items
  set carried.items [] ; remove others from my carried items
  set hidden? true
  set fully.decayed true
  set label " "
end

; --------------------------------------------------------------------------------------------------------- ;
;
; U
;
; The
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to update-appearance
  set size body.size
  set label ifelse-value ( is.alive ) [ " " ] [ "x" ]
  set color round ( wrap-color group.identity + 5 - ( 10 ^ body.shade ))
  set shape get-shape
end

; --------------------------------------------------------------------------------------------------------- ;
;
; U
;
; The
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to-report get-shape
  let base_shape ifelse-value
  ( biological.sex = "male" ) [ "triangle" ] [ "circle" ]
  let eye_size ( ifelse-value
    ( visual.range < ( 1 / 3 ) )
    [ "1" ]
    ( visual.range < ( 2 / 3 ) ) [ "2" ] [ "3" ] )
  let eye_spacing ( ifelse-value ( visual.angle < ( 1 / 3 ) ) [ "1" ] ( visual.angle < ( 2 / 3 ) ) [ "2" ] [ "3" ] )
  let current-perception ifelse-value ( solar-status = "DAY" ) [ day.perception ] [ night.perception ]
  let eye_acuity ( ifelse-value ( is.resting or not is.alive ) [ "1" ] ( current-perception > 0.5 ) [ "3" ] ( current-perception > 0 ) [ "2" ] [ "1" ])
  let a_on ifelse-value yellow.signal [ "a" ] [ "" ]
  let b_on ifelse-value red.signal [ "b" ] [ "" ]
  let c_on ifelse-value blue.signal [ "c" ] [ "" ]
  report ( word base_shape eye_size eye_spacing eye_acuity a_on b_on c_on )
end

; --------------------------------------------------------------------------------------------------------- ;
;
; UPDATE CALLER'S ENERGY SUPPLY ACCORDING TO INPUT VALUE
;
; This subroutine regulates the input and output flow of the caller's energy as they live their lives. The
; input value can be positive, which means that the caller has gained energy, such as through eating or
; nursing. An input value that is negative occurs when the caller has lost energy, such as through performing
; behaviors. This inflow and outflow of energy is recorded for each individual's net cost and gain of energy.
;
; ENTRY:  update | The positive or negative input value.
;
; EXIT:  The caller's energy supply is increased or decreased by a given ammount.
;        This net gain or loss is recorded.
;
; --------------------------------------------------------------------------------------------------------- ;

to update-energy [ update ]
  set energy.supply energy.supply + update                      ; Update caller's energy with input value,
  ifelse ( update > 0 )                                         ; which can be positive or negative.
  [ set total.energy.gains total.energy.gains + update ]        ; Record positive values as energy gains.
  [ set total.energy.cost total.energy.cost + abs update ]      ; Record negative values as energy costs.
end

; ========================================================================================================= ;
; ========================================================================================================= ;
;
;                             oo
;
;  .d8888b. 88d888b. .d8888b. dP 88d888b. .d8888b.
;  88ooood8 88'  `88 88'  `88 88 88'  `88 88ooood8
;  88.  ... 88    88 88.  .88 88 88    88 88.  ...
;  `88888P' dP    dP `8888P88 dP dP    dP `88888P'
;                         .88
;                     d8888P
;
; This is the "engine" of the running simulation, where all living agents assess their environments and then make
; decisions based upon this assessment and also upon their genotypes. If they have enough energy they perform
; actions that correspond to the decisions that they made. The main subroutines used in each
; timestep for each individual are:
;
; consider-environment > make-decisions > check-energy > do-actions
;
; ========================================================================================================= ;
; ========================================================================================================= ;

; --------------------------------------------------------------------------------------------------------- ;
;
; SET CURRENT ENVIRONMENT OF ANIMALS AND PLANTS FROM CURRENT FIELD OF VIEW
;
; This routine runs once per time step for each animal. A specific case includes mothers and gestatees,
; who always include each other in their sets of environmental objects.
;
; ENTRY: The collection of patches defines the status of all individuals at the end of the previous time step.
;
;        'model-structure' defines the overall conditions for the simulation, as documented in subroutine 'go'.
;
; EXIT:  Each agent has a new set of patches and agents, including itself, that make up its environment.
;
;
; --------------------------------------------------------------------------------------------------------- ;

to consider-environment

  carefully [
                                                                 ; ASPATIAL WORLD

    ifelse ( member? "aspatial" model-structure )                ; If this world is aspatial,
    [ let patch-count count patches                              ; count the number of patches
      in-cone                                                    ; that would be in the field of
      ( ceiling ( maximum-visual-range * visual.range ) )        ; view were the world spatial,
      ( 360 * visual.angle )                                     ; then randomly select a set of
      ask n-of patch-count patches                               ; patches to match the size of
      [ ask myself                                               ; that field of view.
        [ set my.environment lput myself my.environment ] ]

      if ( life.history != "gestatee" ) [                        ; If the individual is not a gestatee,
        ask other anima1s with                                   ; put all the visible individuals in
        [ member? patch-here [my.environment] of myself ]        ; the randomly selected patches into
        [ if ( not hidden? )                                     ; the current environment.
          [ ask myself
            [ set my.environment
              lput myself my.environment ] ] ] ] ]
                                                                 ; SPATIAL WORLD

    [ ask patches in-cone                                        ; If this world is spatial,
      ( ceiling ( maximum-visual-range * visual.range ) )        ; select all the patches that
      ( 360 * visual.angle )                                     ; actually are in the field of
      [ ask myself                                               ; view.
        [ set my.environment lput myself my.environment ] ]

      if ( life.history != "gestatee" ) [                        ; If the individual is not a gestatee,
        ask other anima1s in-cone                                ; put all the visible individuals in
        ( maximum-visual-range * visual.range )                  ; the field of view into the current
        ( 360 * visual.angle )                                   ; environment.
        [ if ( not hidden? )
          [ ask myself
            [ set my.environment
              lput myself my.environment ] ] ] ] ]

    let current-perception ifelse-value ( solar-status = "DAY" ) ; Current perceptive abilities are based own
    [ day.perception ] [ night.perception ]                      ; visual attributes and on whether it is day or night.
    set my.environment up-to-n-of                                ; If current perceptive abilities are
    ceiling ( current-perception * length my.environment )       ; less than 100%, randomly remove some objects
    my.environment                                               ; from environment to correspond with limited perceptive abilities.

    if ( life.history = "gestatee" )                             ; If the individual is a gestatee,
    [ if ( my.mother != nobody )                                 ; put the gestatee in the mother's
      [ if ( not member? self [my.environment] of my.mother ) [  ; current environment.
        ask my.mother
        [ set my.environment
          lput myself my.environment ]]
        set my.environment                                       ; Also in that case put the mother
        lput my.mother my.environment ] ]                        ; in the gestatee's environment.

    foreach carried.items [ itm ->                               ; All objects that the individual is carrying
      set my.environment lput itm my.environment                 ; are also added to the current
      ask itm [                                                  ; environment.
        if ( is.alive and not member? myself my.environment )
        [ set my.environment lput myself my.environment ] ]]

    set my.environment lput self my.environment                  ; Individual also adds itself to current environment.
    set my.environment remove-duplicates my.environment          ; Remove any duplicate objects from current environment.

  ] [ print ( word "CONSIDER ENVIRONMENT ERROR - " my.identity " : " error-message ) ] ; If error occurs, print out error message.

end

; --------------------------------------------------------------------------------------------------------- ;
;
; GENERATE LIST OF DECISIONS FROM CURRENT ENVIRONMENT AND GENOTYPE
;
; The main purpose of this subroutine is to access the appropriate subroutines to translate a genotype into
; decision vectors, based on the current environment. There are two languages that genotypes can be written
; in: sta2us and gat3s. The appropriate subrotuines in B3GET vary according to which genotype language is
; used. The user can manually set which genotype reader to use by selecting the genotype-reader in the interface,
; otherwise B3GET will use the sta2us genotype reader by default.
;
; ENTRY: genotype-reader | Either "sta2us" or "gat3s" based on genotype language used.
;
;        my.enivornment | The current environment of the caller.
;
;
; EXIT: decision.vectors | This list includes all decisions that the caller made from its genotype and environment.
;
;       recent-decisions-made | The global record of decisions is updated with caller's decisions.
;
; --------------------------------------------------------------------------------------------------------- ;

to make-decisions

  carefully [

    set decision.vectors ( ifelse-value                          ; Get decisions from selected genotype reader.
      ( genotype-reader = "sta2us" )                             ; If genotype reader is sta2us,
      [ sta7us-get-decisions my.environment ]                    ; get decisions based on a sta2us genotype.
      ( genotype-reader = "gat3s" )                              ; If gentoype reader is gat3s,
      [ gat3s-get-decisions my.environment ]                     ; get decisions based on a gat3s genotype.
      [ sta7us-get-decisions my.environment ] )                  ; If not specified, assume current genotype reader is sta2us.

    ;reduce-to-unique-vectors                                     ; Reduce to unique set of decision vectors

  ] [ print ( word "MAKE DECISIONS ERROR - " my.identity " : " error-message ) ]    ; If error occurs, print out error message.

end

; --------------------------------------------------------------------------------------------------------- ;
;
; HELPER SUBROUTINE TO REDUCE LIST OF DECISIONS TO UNIQUE COMBINATIONS OF TARGET AND ACTION
;
; This subroutine reduces the current list of decision vectors into a list of unique vectors such that
; there is only one vector for each combination of target and action. During this reduction processes, the
; decision vector weights are added together resulting in a decision vector with the net weight.
;
; ENTRY: Initial list of decision vectors before reduction process.
;
; EXIT:  Final list of decision vectors after the reduction process.
;
; --------------------------------------------------------------------------------------------------------- ;

to reduce-to-unique-vectors

  let initial-decisions decision.vectors                         ; Initially, there is a full list of decisions
  let reduced-decisions []                                       ; and an empty list.

  foreach initial-decisions [ original-vector ->                 ; Looping through the full list of decisions,
    let original-ego item 0 original-vector                      ; identify the caller,
    let original-target item 1 original-vector                   ; identity the target,
    let original-action item 2 original-vector                   ; identity the action,
    let original-weight item 3 original-vector                   ; identify the cost.

    let vector-doesnt-exist true                                 ;

    let index 0                                                  ;
    foreach reduced-decisions [ reduced-vector ->                ;
      let reduced-ego item 0 reduced-vector                      ;
      let reduced-target item 1 reduced-vector                   ;
      let reduced-action item 2 reduced-vector                   ;
      let reduced-weight item 3 reduced-vector                   ;

      if ( reduced-ego = original-ego )                          ;
      and ( reduced-target = original-target )                   ;
      and ( reduced-action = original-action ) [                 ;

        set vector-doesnt-exist false                            ;

        set reduced-weight ifelse-value                          ;
        ( is-number? reduced-weight )                            ;
        [ reduced-weight ]                                       ;
        [ 0 ]                                                    ;

        set original-weight ifelse-value                         ;
        ( is-number? original-weight )                           ;
        [ original-weight ]                                      ;
        [ 0 ]                                                    ;

        let new-vector ( list                                    ;
          self                                                   ;
          reduced-target                                         ;
          reduced-action                                         ;
          ( reduced-weight + original-weight )                   ;
          false )                                                ;

        set reduced-decisions                                    ;
        remove-item index reduced-decisions                      ;

        set reduced-decisions                                    ;
        lput new-vector reduced-decisions ]                      ;

      set index index + 1                                        ;
    ]

    if vector-doesnt-exist [                                     ;
      set reduced-decisions lput                                 ;
      ( list                                                     ;
        self                                                     ;
        original-target                                          ;
        original-action                                          ;
        original-weight                                          ;
        false )                                                  ;
      reduced-decisions ]                                        ;
  ]
  set decision.vectors reduced-decisions                         ;
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER PERFORMS EACH ACTION LISTED IN ITS DECISION VECTORS IF IT HAS SUFFICIENT ENERGY
;
; The caller performs each that action that corresponds to each decision in its current list of decision
; vectors. The caller will pay an energy cost to perform each action based on the value listed in the
; corresponding decision. However, the caller will not be able to perform the action if it does not have
; sufficient energy to do so. If the caller does have enough energy, this subroutine triggers the appropriate
; subroutine that corresponds to the action. For clarity, the names of each action exactly match the corresponding
; name for each subroutine. The caller is also prohibited from performing certain actions if they are currently
; resting.
;
; ENTRY: List of decisions vectors for the caller.
;
;        Current energy.supply of the caller, which is how much energy they have to perform actions.
;
; EXIT: The actions that the caller performs may alter the simulation environment in various ways. See the
;       descriptions for each action subroutine below for more details.
;
;       The actions.completed list is updated with the actions that the caller performed during this
;       subroutine.
;
; --------------------------------------------------------------------------------------------------------- ;

to do-actions

  carefully [

    foreach decision.vectors [ vector ->                         ; For each decision vector
      let done item 4 vector                                     ; first determine if this decision has already
                                                                 ; been acted upon.

      if ( not done and                                          ; If the decision has not already been acted upon
        check-energy vector ) [                                  ; and the caller has sufficient energy to perform
                                                                 ; the action, then the action can be performed.

        let target item 1 vector                                 ; Identify the target of the decision.
        let action item 2 vector                                 ; Identify which action the caller has decided to take.
        let cost item 3 vector                                   ; Identify how much the action will cost.

        if ( is.resting = false )                                ; These actions can only be performed if the caller
                                                                 ; is not resting.

        [ ( ifelse                                               ; Call the subroutine that corresponds with the action name.

          action = "move-toward" [ move-toward target cost ]
          action = "move-away-from" [ move-toward target ( - cost ) ]
          action = "turn-right" [ turn-right cost ]
          action = "turn-left" [ turn-right ( - cost ) ]
          action = "go-forward" [ go-forward cost ]
          action = "set-heading" [ set-heading cost ]
          action = "set-heading-random" [ set-heading-random cost ]
          action = "match-heading" [ match-heading target cost ]
          action = "eat" [ if ( check-distance target ) [ eat target cost ] ]
          action = "join" [ join target cost ]
          action = "leave" [ leave target cost ]
          action = "recruit" [ recruit target cost ]
          action = "expel" [ expel target cost ]
          action = "pick-up" [ if ( check-distance target ) [ pick-up target cost ] ]
          action = "put-down" [ if ( check-distance target ) [ put-down target cost  ] ]
          action = "cling-to" [ if ( check-distance target ) [ cling-to target cost ] ]
          action = "squirm-from" [ if ( check-distance target ) [ squirm-from target cost ] ]
          action = "help" [ if ( check-distance target ) [ help target cost ] ]
          action = "attack" [ if ( check-distance target ) [ attack target cost ] ]
          action = "mate-with" [ if ( check-distance target ) [ mate-with target cost ] ]
          [])]

        ( ifelse                                                 ; These actions can be performed resting or not resting.

          action = "hide" [ hide cost ]
          action = "rest" [ rest cost ]
          action = "survival-chance" [ survival-chance cost ]
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
          action = "yellow-signal" [ yellow-signal cost ]
          action = "red-signal" [ red-signal cost ]
          action = "blue-signal" [ blue-signal cost ]
          action = "check-infancy" [ check-infancy cost ]
          action = "check-birth" [ check-birth cost ]
          action = "check-juvenility" [ check-juvenility cost ]
          action = "check-weaning" [ check-weaning cost ]
          action = "check-adulthood" [ check-adulthood cost ]
          action = "supply-to" [ if ( check-distance target ) [ supply-to target cost ]]
          action = "demand-from" [ if ( check-distance target ) [ demand-from target cost ]]
          [])
      ]
    ]

  ] [ print ( word "DO ACTIONS ERROR - " my.identity " : " error-message ) ]      ; If error occurs, print out error message.

end

; --------------------------------------------------------------------------------------------------------- ;
;
; CHECK THAT DISTANCE BETWEEN CALLER AND TARGET IS CLOSE ENOUGH TO INTERACT
;
; Individuals can only perform interactions that target another individual if they are close enough. This
; subroutine checks that the caller and target are close enough to perform the interaction. This subroutine
; returns TRUE, meaning they are close enough, if the distance between them is less than the maximum possible
; distance defined by the sum of the radius of both individuals. If the model-structure is currently "aspatial"
; then two individuals are always within proximity.
;
; ENTRY: model-structure | If currently aspatial then always returns true
;
;        Current spatial coordinates of the caller.
;
;        Current spatial coordinates of the target.
;
;
; EXIT:  Returns true or false depending on if distance between caller and target is within bounds.
;
; --------------------------------------------------------------------------------------------------------- ;

to-report check-distance [ target ]
  let target-radius ifelse-value ( is-patch? target )          ; The radius of the target
  [ 0.5 ]                                                      ; is 1/2 if target is a plant
  [[size] of target / 2 ]                                      ; and half its size if target is an anima1.

  let my-radius ( size / 2 )                                   ; The radius of the caller is half its size.

  report ( ifelse-value
    ( member? "aspatial" model-structure )                     ; If current simulation is "aspatial" then
    [ true ]                                                   ; distance to target is always within bounds.

    ( target = nobody )                                        ; If there is no target then distance
    [ false ]                                                  ; to target is not within bounds.

    [ distance target < target-radius + my-radius ])           ; In all other cases, the distance to
                                                               ; target is within bounds if it is less
                                                               ; than the sum of caller and target radii.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CHECK THAT CALLER HAS SUFFICIENT ENERGY TO ACT UPON DECISION
;
; The caller can only perform an action if it has sufficient energy allocated for that purpose. In this
; subroutine, the caller looks at a single decision vector
;
; ENTRY: model-structure | If currently aspatial then always returns true
;
;        Current spatial coordinates of the caller.
;
;        Current spatial coordinates of the target.
;
;
; EXIT:  Returns true or false depending on if distance between caller and target is within bounds.
;
; --------------------------------------------------------------------------------------------------------- ;

to-report check-energy [ vector ]
  let cost item 3 vector                                       ; Identify the cost of the decision.
  let passes-energy-check ifelse-value                         ; Confirm if caller's energy is sufficient for cost.
  ( member? "free-lunch" model-structure )                     ; If B3GET is set to "free lunch"
  [ true ]                                                     ; then any amount of energy is sufficient.
  [ energy.supply > abs cost and abs cost > 0 ]                ; Otherwise, the energy is sufficient if
                                                               ; it is greater than the cost, and the cost
                                                               ; is greater than 0.

  if ( passes-energy-check ) [                                 ; If the energy is sufficient,
    update-energy ( - abs cost )                               ; reduce the caller's energy supply by the cost.

    let new-vector lput true but-last vector                   ; Update this decision in the caller's
    let vector-index position vector decision.vectors          ; decision.vectors to be marked as
    if ( is-number? vector-index ) [                           ; having been acted upon.
      set decision.vectors                                     ; This ensures that the same decision
      remove-item vector-index decision.vectors                ; will not be acted upon twice.
      set decision.vectors
      lput new-vector decision.vectors ]]

  report passes-energy-check                                   ; Report whether the energy check passed,
                                                               ; therby allowing the decision to be acted upon.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; DETERMINE IF TARGET HAS COUNTERACTED AN ACTION OF CALLER
;
; This can only be called for certain actions, as specified within the code below, and include actions
; where no further action beyond paying for it is needed. This subroutine is needed to ensure that
; actions occur in the appriopriate order. ask target to complete actions from incomplete related decisions
; Note that the cost reported will also include any costs that have been paid previously in this timestep,
; which simulates all actions occurring simultaneously rather than in a particular order.
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to-report get-action-cost-of [ target action-name ]

  let not-done-decisions filter [ vector ->                    ; Identify any decisions that the target
    item 1 vector = self and                                   ; has that is targeting the caller,
    item 2 vector = action-name and                            ; same name as the input value,
    item 4 vector = false ]                                    ; and is unfinished.
  [decision.vectors] of target

  ask target [                                                 ; The target updates these decisions to
    foreach not-done-decisions [ vector ->                     ; completed actions if they have sufficient
      if ( check-energy vector ) [                             ; energy.
        complete-action
        ( item 1 vector )
        ( item 2 vector )
        ( item 3 vector ) ]]]

  let action-cost sum
  map [ vector -> item 3 vector ]                              ; Get total cost from target for all
  filter [ vector ->                                           ; of its completed actions that
    item 1 vector = self and                                   ; both target the caller
    item 2 vector = action-name ]                              ; and share the same name as the input value.
  [ actions.completed ] of target

  report action-cost                                           ; Report this cost.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER KEEPS A RECORD FOR EACH ACTION THAT THEY COMPLETE
;
; Each action subroutine calls this subroutine to keep a record of each action that has been taken
; for each individual.
;
; ENTRY: target | Whoever the caller has targeted when performing action.
;
;        action | The type of action that the caller has completed.
;
;        cost | The amount
;
; EXIT:  The action is added to the caller's completed actions.
;
;        The action is also added to the global record of completed actions.
;
; --------------------------------------------------------------------------------------------------------- ;

to complete-action [ target action cost ]
  let completed-action ( list                                  ; Create an action item that
    self                                                       ; includes the caller,
    target                                                     ; the target of the caller,
    action                                                     ; the action performed by the caller,
    precision cost 10 )                                        ; and the energy cost of that action.

  set actions.completed                                        ; Record this action item
  lput completed-action                                        ; in caller's list of recently completed
  actions.completed                                            ; actions.
end

; ========================================================================================================= ;
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
; ========================================================================================================= ;

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES ITS SURVIVAL CHANCE ATTRIBUTE
;
; ENTRY: cost | The amount of energy spent to update this attribute.
;
; EXIT: The survival.chance value increases if cost is positive or decreases if cost is negative based
;       on the get-updated-value subroutine.
;
;       This subroutine also calls the complete-action subroutine to keep a record of this action.
;
; --------------------------------------------------------------------------------------------------------- ;

to survival-chance [ cost ]
  complete-action self "survival-chance" cost
  set survival.chance get-updated-value survival.chance cost
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES ITS BODY SIZE ATTRIBUTE
;
; ENTRY: cost | The amount of energy spent to update this attribute.
;
; EXIT: The body.size value increases if cost is positive or decreases if cost is negative based
;       on the get-updated-value subroutine.
;
;       This subroutine also calls the complete-action subroutine to keep a record of this action.
;
; --------------------------------------------------------------------------------------------------------- ;

to body-size [ cost ]
  complete-action self "body-size" cost
  set body.size get-updated-value body.size cost
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES ITS BODY SHADE ATTRIBUTE
;
; ENTRY: cost | The amount of energy spent to update this attribute.
;
; EXIT: The body.shade value increases if cost is positive or decreases if cost is negative based
;       on the get-updated-value subroutine.
;
;       This subroutine also calls the complete-action subroutine to keep a record of this action.
;
; --------------------------------------------------------------------------------------------------------- ;

to body-shade [ cost ]
  complete-action self "body-shade" cost
  set body.shade get-updated-value body.shade cost
end

to visual-range [ cost ]
  complete-action self "visual-range" cost
  set visual.range get-updated-value visual.range cost
end

to visual-angle [ cost ]
  complete-action self "visual-angle" cost
  set visual.angle get-updated-value visual.angle cost
end

to day-perception [ cost ]
  complete-action self "day-perception" cost
  set day.perception get-updated-value day.perception cost
end

to night-perception [ cost ]
  complete-action self "night-perception" cost
  set night.perception get-updated-value night.perception cost
end

to conception-chance [ cost ]
  complete-action self "conception-chance" cost
  set conception.chance get-updated-value conception.chance cost
end

to bite-capacity [ cost ]
  complete-action self "bite-capacity" cost
  set bite.capacity get-updated-value bite.capacity cost
end

to mutation-chance [ cost ]
  complete-action self "mutation-chance" cost
  set mutation.chance get-updated-value mutation.chance cost
end

to sex-ratio [ cost ]
  complete-action self "sex-ratio" cost
  set sex.ratio get-updated-value sex.ratio cost
end

to litter-size [ cost ]
  complete-action self "litter-size" cost
  set litter.size get-updated-value litter.size cost
end

; --------------------------------------------------------------------------------------------------------- ;
;                                                                _
;  _ __ ___     ___   __   __   ___   _ __ ___     ___   _ __   | |_
; | '_ ` _ \   / _ \  \ \ / /  / _ \ | '_ ` _ \   / _ \ | '_ \  | __|
; | | | | | | | (_) |  \ V /  |  __/ | | | | | | |  __/ | | | | | |_
; |_| |_| |_|  \___/    \_/    \___| |_| |_| |_|  \___| |_| |_|  \__|
;
; --------------------------------------------------------------------------------------------------------- ;

to move-toward [ target cost ]
  complete-action target "move-toward" cost

  if (target != nobody ) [

    ; deteremine x and y coordinate difference to target
    let ycor-difference ( ( ifelse-value ( is-patch? target ) [ [pycor] of target ] [ [ycor] of target ] ) - [ycor] of self )
    let xcor-difference ( ( ifelse-value ( is-patch? target ) [ [pxcor] of target ] [ [xcor] of target ] ) - [xcor] of self )

    ; these lines check if agent is looking at target across one of the edges of the world, which would not calculate the angle correctly
    if ( ycor-difference > maximum-visual-range and ycor-difference > 0 ) [ set ycor-difference ycor-difference - 100 ]
    if ( xcor-difference > maximum-visual-range and xcor-difference > 0 ) [ set xcor-difference xcor-difference - 100 ]
    if ( abs ycor-difference > maximum-visual-range and ycor-difference < 0 ) [ set ycor-difference ycor-difference + 100 ]
    if ( abs xcor-difference > maximum-visual-range and xcor-difference < 0 ) [ set xcor-difference xcor-difference + 100 ]

    if ( not ( ycor-difference = 0 and xcor-difference = 0 ) ) [ ; If the target isn't in the exact same location as self

      ; calculate angle to target
      let angle atan xcor-difference ycor-difference
      if ( cost < 0 ) [ set angle angle - 180 ]

      ; set magnitudes based on cost
      set x.magnitude x.magnitude + (abs cost * sin angle)
      set y.magnitude y.magnitude + (abs cost * cos angle) ]

    ; set heading based on magnitudes
    set heading ifelse-value ( x.magnitude = 0 and y.magnitude = 0 ) [ heading ] [ ( atan x.magnitude y.magnitude ) ]
  ]
end

to match-heading [ target cost ]
  complete-action target "match-heading" cost
;  ;print heading
;  ;print [heading] of target
  ;  let total-cost 10 * cost / ( abs deterioration-rate ) ; scaled with deterioration rate
  ;  let heading-difference subtract-headings heading [heading] of target
  ;  ;print heading-difference
  ;  let cost-of-heading abs heading-difference / 360
  ;  ;print cost-of-heading
  ;
  ;  ifelse ( total-cost >= cost-of-heading ) [
  ;    set heading [heading] of target
  ;  ][
  ;    set heading heading + 360 * total-cost * -1 * ( heading-difference / abs heading-difference )
  ;  ]
  ;print heading
  ;print ""


  if (target != nobody ) [

    ; deteremine x and y coordinate difference to target
    let ycor-difference [ y.magnitude ] of target
    let xcor-difference [ x.magnitude ] of target

    if ( not ( ycor-difference = 0 and xcor-difference = 0 ) ) [ ; If the target isn't in the exact same location as self

      ; calculate angle to target
      let angle atan xcor-difference ycor-difference
      if ( cost < 0 ) [ set angle angle - 180 ]

      ; set magnitudes based on cost
      set x.magnitude x.magnitude + (abs cost * sin angle)
      set y.magnitude y.magnitude + (abs cost * cos angle) ]

    ; set heading based on magnitudes
    set heading ifelse-value ( x.magnitude = 0 and y.magnitude = 0 ) [ heading ] [ ( atan x.magnitude y.magnitude ) ]
  ]

end

to turn-right [ cost ]
  complete-action self "turn" cost
  if ( cost > 0 ) [ right ( 360 * cost )
    complete-action self "turn-right" 0 ]
  if ( cost < 0 ) [ left ( 360 * abs cost )
    complete-action self "turn-left" 0 ]
end

to go-forward [ cost ]
  complete-action self "go-forward" cost
  if ( life.history != "gestatee" ) [

    ifelse ( cost < 0 ) [ right 180 complete-action self "went-backward" 0] [ complete-action self "went-forward" 0 ]
    let sum-weight size
    foreach carried.items [ object -> set sum-weight sum-weight + [size] of object ]
    let travel-distance ifelse-value ( sum-weight > 0 ) [ (size * (sqrt (( 2 * abs cost ) / sum-weight )) ) ] [ 0 ]
    forward travel-distance

    set x.magnitude 0
    set y.magnitude 0

    ; track travel
    set distance.traveled distance.traveled + travel-distance

  ]
end

to set-heading [ cost ]
  complete-action self "set-heading" cost
  set heading cost * 360
end

to set-heading-random [ cost ]
  complete-action self "set-heading-random" cost
  let x-difference 2 * x.magnitude * 100000 * cost
  if ( x-difference = 0 ) [ set x-difference 100000 * cost ]
  if ( x-difference > 1E10 ) [ set x-difference 1E10 ]
  set x.magnitude x.magnitude + random-float x-difference - random-float x-difference
  let y-difference 2 * y.magnitude * 100000 * cost
  if ( y-difference = 0 ) [ set y-difference 100000 * cost ]
  if ( y-difference > 1E10 ) [ set y-difference 1E10 ]
  set y.magnitude y.magnitude + random-float y-difference - random-float y-difference
  set heading ifelse-value ( x.magnitude = 0 and y.magnitude = 0 ) [ heading ] [ ( atan x.magnitude y.magnitude ) ]
end

to hide [ cost ]
  complete-action self "hide" cost
  set hidden.chance get-updated-value hidden.chance cost
  if ( life.history != "gestatee" ) [
    ifelse ( random-float 1.0 < hidden.chance ) [
      set hidden? true
      complete-action self "is-hidden" 0
    ][
      set hidden? false
      complete-action self "not-hidden" 0
  ]]
end

to rest [ cost ]
  complete-action self "rest" cost
  if ( cost > 0 ) [ set is.resting true complete-action self "is-resting" 0 ]
  if ( cost < 0 ) [ set is.resting false complete-action self "not-resting" 0 ]
end

; --------------------------------------------------------------------------------------------------------- ;
;        _                           _   _
;  ___  (_)   __ _   _ __     __ _  | | (_)  _ __     __ _
; / __| | |  / _` | | '_ \   / _` | | | | | | '_ \   / _` |
; \__ \ | | | (_| | | | | | | (_| | | | | | | | | | | (_| |
; |___/ |_|  \__, | |_| |_|  \__,_| |_| |_| |_| |_|  \__, |
;            |___/                                   |___/
; --------------------------------------------------------------------------------------------------------- ;

to yellow-signal [ cost ]
  complete-action self "yellow-signal" cost
  set yellow.chance get-updated-value yellow.chance cost
  ifelse ( random-float 1.0 < yellow.chance ) [
    set yellow.signal true
    complete-action self "yellow-signal-on" 0
  ][
    set yellow.signal false
    complete-action self "yellow-signal-off" 0
  ]
end

to red-signal [ cost ]
  complete-action self "red-signal" cost
  set red.chance get-updated-value red.chance cost
  ifelse ( random-float 1.0 < red.chance ) [
    set red.signal true
    complete-action self "red-signal-on" 0
  ][
    set red.signal false
    complete-action self "red-signal-off" 0
  ]
end

to blue-signal [ cost ]
  complete-action self "blue-signal" cost
  set blue.chance get-updated-value blue.chance cost
  ifelse ( random-float 1.0 < blue.chance ) [
    set blue.signal true
    complete-action self "blue-signal-on" 0
  ][
    set blue.signal false
    complete-action self "blue-signal-off" 0
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;  _   _    __            _       _         _
; | | (_)  / _|   ___    | |__   (_)  ___  | |_    ___    _ __   _   _
; | | | | | |_   / _ \   | '_ \  | | / __| | __|  / _ \  | '__| | | | |
; | | | | |  _| |  __/   | | | | | | \__ \ | |_  | (_) | | |    | |_| |
; |_| |_| |_|    \___|   |_| |_| |_| |___/  \__|  \___/  |_|     \__, |
;                                                                |___/
; --------------------------------------------------------------------------------------------------------- ;

to check-infancy [ cost ]
  complete-action self "check-infancy" cost
  ifelse ( my.mother = nobody )
  [ set-to-dead ] ; gestatees die if mother is dead
  [ set infancy.chance get-updated-value infancy.chance cost
    complete-action self "update-infancy-chance" 0
    if ( life.history = "gestatee" and random-float 1.0 < infancy.chance ) [
      ask my.mother [ give-birth ]
    ]
  ]
end

to check-birth [ cost ]
  complete-action self "check-birth" cost
  set birthing.chance get-updated-value birthing.chance cost
  complete-action self "update-birthing-chance" 0
  if ( fertility.status = "pregnant" and random-float 1.0 < birthing.chance ) [
    ask my-offspring with [ life.history = "gestatee" ] [ set mother.initiated.birth true ]
    give-birth
  ]
end

to give-birth
  complete-action self "give-birth" 0
  if ( fertility.status = "pregnant" ) [
    set fertility.status "lactating"
    ask my-offspring with [ life.history = "gestatee" ] [ update-to-infant ]
    set birthing.chance 0
    complete-action self "gave-birth" 0
  ]
end

to-report my-offspring
  report ifelse-value ( biological.sex = "female" )
  [ anima1s with [ my.mother = myself ]]
  [ anima1s with [ father.identity = [my.identity] of myself ]]
end

to update-to-infant
  if ( is.alive ) [
    complete-action self "update-to-infant" 0
    set life.history "infant"
    set fertility.status " "
    set hidden? false
    set is.resting false
    set ticks.at.birth ticks
    set label "i"
  ]
end

to check-juvenility [ cost ]
  complete-action self "check-juvenility" cost
  set juvenility.chance get-updated-value juvenility.chance cost
  complete-action self "update-juvenility-chance" 0
  if ( life.history = "infant" and random-float 1.0 < juvenility.chance ) [
    ifelse ( my.mother = nobody )
    [ update-to-juvenile ]
    [ ask my.mother [ wean-offspring ]]
  ]
end

to check-weaning [ cost ]
  complete-action self "check-weaning" cost
  set weaning.chance get-updated-value weaning.chance cost
  complete-action self "update-weaning-chance" 0
  if ( fertility.status = "lactating" and random-float 1.0 < weaning.chance ) [
    ask my-offspring with [ life.history = "infant" ] [ set mother.initiated.weaning true ]
    wean-offspring
  ]
end

to wean-offspring
  complete-action self "wean-offspring" 0
  if ( fertility.status = "lactating" ) [
    set fertility.status "cycling"
    ask my-offspring with [ life.history = "infant" ] [ update-to-juvenile ]
    set weaning.chance 0
    complete-action self "weaned-offspring" 0
  ]
end

to update-to-juvenile
  if ( is.alive ) [
    complete-action self "update-to-juvenile" 0
    set life.history "juvenile"
    set fertility.status " "
    set ticks.at.weaning ticks
    let my-meta-id my.identity
    set label "j"
    ask anima1s with [ member? my-meta-id infanticide.history ] [ set infanticide.history remove my-meta-id remove-duplicates infanticide.history ]
  ]
end

to check-adulthood [ cost ]
  complete-action self "check-adulthood" cost
  set adulthood.chance get-updated-value adulthood.chance cost
  complete-action self "update-adulthood-chance" 0
  if ( life.history = "juvenile" and random-float 1.0 < adulthood.chance ) [
    update-to-adult
  ]
end

to update-to-adult
  if ( is.alive ) [
    complete-action self "update-to-adult" 0
    set life.history "adult"
    set fertility.status ifelse-value ( biological.sex = "male" ) [ " " ] [ "cycling" ]
    set ticks.at.sexual.maturity ticks
    set adult.hidden.chance hidden.chance
    set adult.survival.chance survival.chance
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
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
;   ___   _ __     ___   _ __    __ _   _   _
;  / _ \ | '_ \   / _ \ | '__|  / _` | | | | |
; |  __/ | | | | |  __/ | |    | (_| | | |_| |
;  \___| |_| |_|  \___| |_|     \__, |  \__, |
;                               |___/   |___/
; --------------------------------------------------------------------------------------------------------- ;

to supply-to [ target cost ]
  complete-action target "supply-to" cost
  if ( target != self and is-anima1? target and [ is.alive ] of target = true and ( fertility.status = "lactating" or fertility.status = "pregnant" ) ) [
    let target-cost get-action-cost-of target "demand-from"
    let net-cost ( cost + target-cost )
    if ( net-cost > 0 ) [ ask target [ receive-from myself net-cost ] ]
  ]
end

to demand-from [ target cost ]
  complete-action target "demand-from" cost
  if ( target != self and is-anima1? target and [ is.alive ] of target = true and ( life.history = "gestatee" or life.history = "infant" ) ) [
    let target-supply-cost get-action-cost-of target "supply-to"
    let net-cost ( cost + target-supply-cost )
    if ( net-cost > 0 ) [ receive-from target net-cost ]
  ]
end

to eat [ target cost ]
  complete-action target "eat" cost
  if ( life.history = "juvenile" or life.history = "adult" or life.history = "senescent" and ( is-patch? target or is-anima1? target )) [
    receive-from target cost
  ]
end

to receive-from [ target cost ]
  complete-action target "receive-from" cost
  if ( cost > 0 and is-anima1? target or is-patch? target ) [
    let energy-wanted get-updated-value bite.capacity cost
    let energy-supply ifelse-value ( is-patch? target ) [ [ penergy.supply ] of target ] [ [ energy.supply ] of target ]
    let energy-received ifelse-value ( energy-wanted < energy-supply ) [ energy-wanted ] [ energy-supply ]
    if ( energy-received > 0 ) [
      complete-action target "update-energy" 0
      update-energy energy-received
      ifelse ( is-patch? target )
      [ ask target [ set penergy.supply penergy.supply - energy-received ]]
      [ ask target [ update-energy ( - energy-received ) ]]
      if ( life.history = "juvenile" or life.history = "adult" ) [ set foraging.gains foraging.gains + energy-received ] ; only juveniles and adults can eat plants
  ]]
end

; --------------------------------------------------------------------------------------------------------- ;
;  _           _                                  _     _
; (_)  _ __   | |_    ___   _ __    __ _    ___  | |_  (_)   ___    _ __    ___
; | | | '_ \  | __|  / _ \ | '__|  / _` |  / __| | __| | |  / _ \  | '_ \  / __|
; | | | | | | | |_  |  __/ | |    | (_| | | (__  | |_  | | | (_) | | | | | \__ \
; |_| |_| |_|  \__|  \___| |_|     \__,_|  \___|  \__| |_|  \___/  |_| |_| |___/
;
; --------------------------------------------------------------------------------------------------------- ;

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER APPLIES TO JOIN THE SAME GROUP AS THE TARGET
;
; This routine
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;        'cost' defines the amount of energy that the caller paid for this interaction.
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to join [ target cost ]
  complete-action target "join" cost                            ; Record that this action has been completed.
  if ( is-anima1? target and [ is.alive ] of target = true ) [  ; Check that the target is able to interact.
    let my-cost 0                                               ; Establish placeholder values to track caller
    let target-cost 0                                           ; and target decisions about caller joining the group.
    ask anima1s with                                            ; Identify all individuals
    [ group.identity = [group.identity] of target ] [           ; in the same group as the target.
      set my-cost my-cost + get-action-cost-of myself "join"    ; Record the caller's total energy cost to join group.
      set target-cost target-cost +                             ; Record any decisions that group members made
      [ get-action-cost-of myself "recruit" ] of myself         ; about caller joining the group.
    ]
    let probability ( my-cost + target-cost )
    / ( abs my-cost + abs target-cost + 0.0000000001 )          ; The probability of caller joining group depends on
    if ( random-float 1.0 <= probability )                      ; the relative costs paid by caller and group members.
    [ join-group ([group.identity] of target) ]                 ; If the probability is high enough, caller joins group.
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER APPLIES TO LEAVE CURRENT GROUP THAT IS SHARED BY TARGET
;
; This routine only works if target and caller are part of the same group.
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;        'cost' defines the amount of energy that the caller paid for this interaction.
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to leave [ target cost ]
  complete-action target "leave" cost                           ; Record that this action has been completed.
  if ( is-anima1? target and [ is.alive ] of target = true ) [  ; Check that the target is able to interact.
    let my-cost 0                                               ; Establish placeholder values to track caller
    let target-cost 0                                           ; and target decisions about caller leaving the group.
    ask anima1s with                                            ; Identify all individuals
    [ group.identity = [group.identity] of target ] [           ; in the same group as the target.
      set my-cost my-cost + get-action-cost-of myself "leave"   ; Record the caller's total energy cost to leave group.
      set target-cost target-cost +                             ; Record any decisions that group members made
      [ get-action-cost-of myself "expel" ] of myself           ; about caller leaving the group.
    ]
    let probability ( my-cost + target-cost )
    / ( abs my-cost + abs target-cost + 0.0000000001 )          ; The probability of caller joining group depends on
    if ( random-float 1.0 <= probability                        ; the relative costs paid by caller and group members.
      or ( probability = 0 and cost = 0 ))
    [ leave-group ]                                             ; If the probability is high enough, caller leaves group.
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER APPLIES TO RECRUIT THE TARGET TO ITS CURRENT GROUP
;
; This routine
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;        'cost' defines the amount of energy that the caller paid for this interaction.
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to recruit [ target cost ]
  complete-action target "recruit" cost                           ; Record that this action has been completed.
  if ( is-anima1? target and [ is.alive ] of target = true ) [    ; Check that the target is able to interact.
    let our-cost 0                                                ; Establish placeholder values to track group members
    let target-cost 0                                             ; and target decisions about recruiting the target.
    ask anima1s with                                              ; Identify all individuals
    [ group.identity = [group.identity] of myself ] [             ; in the same group as the caller.
      set our-cost our-cost +                                     ; Record any decisions that group members made
      [ get-action-cost-of myself "recruit" ] of target           ; about caller being recruited to the group.
      set target-cost target-cost +                               ; Record the target's total energy cost to join group.
      get-action-cost-of target "join"
    ]
    let probability ( our-cost + target-cost )
    / ( abs our-cost + abs target-cost + 0.0000000001 )           ; The probability of caller joining group depends on
    if ( random-float 1.0 <= probability )                        ; the relative costs paid by target and group members.
    [ ask target [ join-group [group.identity] of myself ]]       ; If the probability is high enough, target joins group.
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER APPLIES TO EXPEL THE TARGET FROM ITS CURRENT GROUP
;
; This routine...strangers cannot do this to non-group members to get them to leave their group (Change)
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;        'cost' defines the amount of energy that the caller paid for this interaction.
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to expel [ target cost ]
  complete-action target "expel" cost                             ; Record that this action has been completed.
  if ( is-anima1? target and [ is.alive ] of target = true ) [    ; Check that the target is able to interact.
    let our-cost 0                                                ; Establish placeholder values to track group members
    let target-cost 0                                             ; and target decisions about expelling the target.
    ask anima1s with                                              ; Identify all individuals
    [ group.identity = [group.identity] of myself ] [             ; in the same group as the caller.
      set our-cost our-cost +                                     ; Record any decisions that group members made
      [ get-action-cost-of myself "expel" ] of target             ; about caller being expelled by the group.
      set target-cost target-cost +                               ; Record the target's total energy cost to leave group.
      get-action-cost-of target "leave"
    ]
    let probability ( our-cost + target-cost )
    / ( abs our-cost + abs target-cost + 0.0000000001 )           ; The probability of caller joining group depends on
    if ( random-float 1.0 <= probability )                        ; the relative costs paid by target and group members.
    [ ask target [ leave-group ]]                                 ; If the probability is high enough, target leaves group.
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER JOINS THE DESIGNATED GROUP
;
; This subroutine is called when the caller decides to join an established group. When this happens, a
; record is made of this action, the caller updates its group.identity with the identity of the designated
; group, and the caller's label is '='. These effects only occur if the caller is not already in the
; desingated group.
;
; ENTRY:  group-id | The identity of the designated group that the caller wants to join.
;
; EXIT:   Caller is not part of the designated group.
;
; --------------------------------------------------------------------------------------------------------- ;

to join-group [ group-id ]
  complete-action self "join-group" 0                          ; Record that this action is completed.
  ifelse ( group.identity != group-id ) [                      ; If the caller is not already parf of group:

    leave self 0                                               ; Determine if the caller leaves its current group.
    if ( is-solitary? ) [                                      ; If the caller has left its group:

      set group.identity group-id                              ; Establish the input value as the current group.
      set label "="                                            ; Visually indicate that caller has joined a group.
      set group.transfers.history                              ; Caller keeps a record of transferring to this group.
      lput group.identity group.transfers.history
      complete-action self "joined-group" 0  ]                 ; Record that the individual has now joined the group.

  ][
    complete-action self "already-in-group" 0                  ; Record if individual is already in this group.
  ]
end

to-report is-solitary?
  report count anima1s with [ group.identity = [group.identity] of myself ] = 1
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER LEAVES ITS CURRENT GROUP
;
; This subroutine is called when the caller decides to leave its current group. When this happens,
; the caller updates its group.identity with a new random identity, which identifies the caller
; as being the only member of a newly created group. The caller's label is set to '~' to visually indicate
; that the caller has left its group.
;
; ENTRY:
;
; EXIT:   Caller is now the singular member of a newly created group.
;
; --------------------------------------------------------------------------------------------------------- ;

to leave-group
  complete-action self "leave-group" 0                         ; Record that this action is completed.
  set group.identity ( random 10000 *                          ; Caller is established as singular member
    140 + one-of base-colors )                                 ; of new group.
  set label "~"                                                ; Visually indicate that the caller left old group.
  complete-action self "left-group" 0
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER ATTEMPTS TO PICK UP THE TARGET
;
; This subroutine
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;        'cost' defines the amount of energy that the caller paid for this interaction.
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to pick-up [ target cost ]
  complete-action target "pick-up" cost
  if ( is-anima1? target ) [
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "cling-to"
      let probability ( cost + target-cost )
      / ( abs cost + abs target-cost + 0.0000000001 )             ; The probability of caller
      if ( random-float 1.0 <= probability )
      [ carry target ]]]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER ATTEMPTS TO PUT DOWN TARGET
;
; This subroutine
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;        'cost' defines the amount of energy that the caller paid for this interaction.
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to put-down [ target cost ]
  complete-action target "put-down" cost
  if ( is-anima1? target ) [
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "squirm-from"
      let probability ( cost + target-cost )
      / ( abs cost + abs target-cost + 0.0000000001 )             ; The probability of caller
      if ( random-float 1.0 <= probability )
      [ drop target ]]]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER ATTEMPTS TO CLING TO TARGET
;
; This subroutine
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;        'cost' defines the amount of energy that the caller paid for this interaction.
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to cling-to [ target cost ]
  complete-action target "cling-to" cost
  if ( is-anima1? target ) [
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "pick-up"
      let probability ( cost + target-cost )
      / ( abs cost + abs target-cost + 0.0000000001 )             ; The probability of caller
      if ( random-float 1.0 <= probability )
      [ ask target [ carry myself ] ]]]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER ATTEMPTS TO SQUIRM FROM TARGET
;
; This subroutine
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;        'cost' defines the amount of energy that the caller paid for this interaction.
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to squirm-from [ target cost ]
  complete-action target "squirm-from" cost
  if ( is-anima1? target ) [
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "put-down"
      let probability ( cost + target-cost )
      / ( abs cost + abs target-cost + 0.0000000001 )             ; The probability of caller
      if ( random-float 1.0 <= probability )
      [ ask target [ drop myself ] ]]]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER ATTEMPTS TO CARRY TARGET
;
; This subroutine
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to carry [ target ]
  complete-action target "carry" 0
  if ( not member? target [carried.items] of anima1s ) [
    ask anima1s with [ member? target carried.items ] [ set carried.items remove-item ( position target remove-duplicates carried.items ) remove-duplicates carried.items ]
    set carried.items lput target carried.items
    ask target [ move-to myself ]
    set label "^"
    complete-action target "carry-complete" 0
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER ATTEMPTS TO DROP TARGET
;
; This subroutine
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to drop [ target ]
  complete-action target "drop" 0
  if ( member? target carried.items ) [
    set carried.items remove target remove-duplicates carried.items
    set label "*"
    complete-action target "drop-complete" 0
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER ATTEMPTS TO HELP TARGET
;
; This subroutine
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;        'cost' defines the amount of energy that the caller paid for this interaction.
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to help [ target cost ]
  complete-action target "help" cost
  if ( is-anima1? target and [ is.alive ] of target = true ) [
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "help"
      let cost-probability ( cost + target-cost ) / cost
      let size-probability ( size / ( size + [size] of target ) )
      let net-cost cost + target-cost
      if ( random-float 1.0 <= ( cost-probability * size-probability ) ) [ aid target net-cost ]]]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER AIDS THE TARGET
;
; This subroutine
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;        'cost' defines the amount of energy that the caller paid for this interaction.
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to aid [ target cost ]
  complete-action target "aid" 0

  ask target [
    survival-chance cost

    set help.from.history lput
    [my.identity] of myself
    help.from.history ]

  set label "+"

  let relatedness-with-target relatedness-with target
  if ( relatedness-with-target > 0.90 )
  [ set whole.related.help.cost whole.related.help.cost + cost ]
  if ( relatedness-with-target <= 0.90 and
    relatedness-with-target > 0.40 )
  [ set half.related.help.cost half.related.help.cost + cost ]
  if ( relatedness-with-target <= 0.40 and
    relatedness-with-target > 0.15 )
  [ set fourth.related.help.cost fourth.related.help.cost + cost ]
  if ( relatedness-with-target <= 0.15 and
    relatedness-with-target > 0.05 )
  [ set eighth.related.help.cost eighth.related.help.cost + cost ]
  if ( relatedness-with-target <= 0.05 )
  [ set not.related.help.cost not.related.help.cost + cost ]

  complete-action target "aid-complete" 0
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER ATTEMPTS TO ATTACK THE TARGET
;
; This subroutine
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;        'cost' defines the amount of energy that the caller paid for this interaction.
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to attack [ target cost ]
  complete-action target "attack" cost
  if ( is-anima1? target and [ is.alive ] of target = true ) [
    if ( cost > 0 )
    [ let target-cost get-action-cost-of target "attack"
      let cost-probability ( cost + target-cost ) / ( abs cost + abs target-cost )
      let size-probability ( size / ( size + [size] of target ) )
      let net-cost cost + target-cost
      if ( random-float 1.0 <= ( cost-probability * size-probability ) ) [ harm target net-cost ]]]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER HARMS THE TARGET
;
; This subroutine
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;        'cost' defines the amount of energy that the caller paid for this interaction.
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to harm [ target cost ]
  complete-action target "harm" 0
  if ( is-anima1? target and [ is.alive ] of target = true ) [

    ask target [
      survival-chance ( - cost )
      set attack.from.history lput
      [my.identity] of myself
      attack.from.history ]

    set label "-"

    let relatedness-with-target relatedness-with target               ; recording kin selection
    if ( relatedness-with-target > 0.90 )
    [ set whole.related.attack.cost whole.related.attack.cost + cost ]
    if ( relatedness-with-target <= 0.90 and
      relatedness-with-target > 0.40 )
    [ set half.related.attack.cost half.related.attack.cost + cost ]
    if ( relatedness-with-target <= 0.40 and
      relatedness-with-target > 0.15 )
    [ set fourth.related.attack.cost fourth.related.attack.cost + cost ]
    if ( relatedness-with-target <= 0.15 and
      relatedness-with-target > 0.05 )
    [ set eighth.related.attack.cost eighth.related.attack.cost + cost ]
    if ( relatedness-with-target <= 0.05 )
    [ set not.related.attack.cost not.related.attack.cost + cost ]

    if ( [ life.history ] of target = "infant" and                     ; infanticide
      target != self and
      not member? target infanticide.history )
    [ set infanticide.history lput [my.identity] of target infanticide.history ]

    complete-action target "harm-complete" 0
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; DETERMINE GENETIC RELATEDNESS BETWEEN CALLER AND TARGET
;
; This routine determines the genetic relatedness between the caller and a specified individual in the
; population. For example, consider these four chromosomes.
;
; Locus         0 1 2 3 4 5 6 7 8 9
;
; Caller:   I:  y h m u n f q a x r
;          II:  c j s w v i l f a p
;
; Target:   I:  j l h y m d u t v c
;          II:  y u l x i s f p w a
;
; The first locus has one match, Caller I and Target II, and three non-matches, Caller I and Target I (y and J),
; Caller II and Target I (c and j), and Caller II to Target II (c y). This is considered a relatedness of
; <m>1/(1+3) = 0.25</m>, at the first locus. This is repeated across all loci and the average is returned.
;
; ENTRY:  'target' defines an individual to be compared the calling individual.
;         'identity.I' and 'identity.II' carry the genetic information to be compared between caller and target.
;         'identity.I' and 'identity.II' both contain exactly 10 loci.
;
; EXIT:   'relatedness-with' returns an estimate of the degree of relatedness, as fraction between 0 and 1.
;
; --------------------------------------------------------------------------------------------------------- ;

to-report relatedness-with [ target ]
  let matching 0                                               ; Set up variables to track the number
  let not-matching 0                                           ; of matching and non-matching alleles
  let i 0                                                      ;
  while [i < 10] [                                             ; Iterate through the identity chromosomes
                                                               ; of caller and target, considering each
    let allele-here-I item i identity.I                        ; allele at the same locus for both.
    let allele-here-II item i identity.II                      ;
    let target-allele-I item i [identity.I] of target          ; Identify the alleles from both loci for
    let target-allele-II item i [identity.II] of target        ; both individuals.

    ifelse (( allele-here-I = target-allele-I ) or             ; Check if either allele from the target
      ( allele-here-I = target-allele-II ))                    ; matches the caller's allele from its
    [ set matching matching + 1 ]                              ; first identity chromosome.
    [ set not-matching not-matching + 1 ]                      ; Record if they match or do not match.

    ifelse (( allele-here-II = target-allele-I ) or            ; Check if either allele from the target
      ( allele-here-II = target-allele-II ))                   ; matches the caller's allele from its
    [ set matching matching + 1 ]                              ; second identity chromosome.
    [ set not-matching not-matching + 1 ]                      ; Record if they match or do not match.

    set i i + 1 ]                                              ; Progress through the loop.

  report matching / ( matching + not-matching )                ; Return the percent of matching alleles.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER INITIATES MATING WITH TARGET
;
; This inter-action subroutine ititiates mating between the caller and the target. The ultimate goal of
; mating is to conceive offspring, but this is only accomplished if certain criteria are met. The
; caller and target must not be of the same biological sex and they must both be adults. The female in
; this interaction must also be currently cycling (rather than pregnant or lactating).
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;        'cost' defines the amount of energy that the caller paid for this interaction.
;
;        conception.chance | is defined in the anima1 state variables above.
;
; EXIT:  If mating occurs, there is a chance that caller and target will conceive.
;
;        copulations.history | A record of all mating partners and frequency of matings.
;
; --------------------------------------------------------------------------------------------------------- ;

to mate-with [ target cost ]
  complete-action target "mate-with" cost                      ; Record that this action occurred.

  if ( cost > 0                                                ; Perform a check to make sure that
    and is-anima1? target                                      ; copulation only occurs when certain
    and [ is.alive ] of target = true                          ; criteria met, including that the
    and life.history = "adult"                                 ; cost of mating must be greater than 0,
    and [life.history] of target = "adult"                     ; caller and target must be alive and
    and ( biological.sex = "male"                              ; be adults, be opposite sexes, the
      or ( biological.sex = "female"                           ; female must be cycling, and they must
        and fertility.status = "cycling" ))                    ; both have a chance to conceive greater
    and ( [biological.sex] of target = "male"                  ; than 0.
      or ( [biological.sex] of target = "female"               ;
        and [fertility.status] of target = "cycling" ))        ;
    and ( biological.sex != [biological.sex] of target )       ;
    and conception.chance > 0                                  ;
    and [conception.chance] of target > 0 ) [                  ; If all of these criteria are met...

    let my-cost [ get-action-cost-of myself "mate-with" ] of target
    let target-cost get-action-cost-of target "mate-with"      ; Calculate the probability of copulation,
    let net-cost ( my-cost + target-cost )                        ; which could be 100% if both parties cooperate
    let copulation-prob net-cost / my-cost                        ; or a smaller probability if either party resists.
    if ( random-float 1.0 <= copulation-prob ) [               ; If passes this probability check...

      set label "!"                                            ; Signal that a successful copulation event occurred.
      set copulations.history lput                             ; Create a record of this copulation event in
      [my.identity] of target                                  ; both the caller and target's history.
      copulations.history
      ask target [
        set copulations.history lput
        [my.identity] of myself
        copulations.history ]

      let mean-conception (mean (list                          ; Calculate the probability that these
        conception.chance                                      ; individuals conceive during copulation,
        [conception.chance] of target))                        ; which is based on their conception.chance
                                                               ; attributes and net energy towards mating.

      let conception-prob get-updated-value mean-conception net-cost

      if random-float 1.0 < conception-prob [                  ; If the conception probability passes...
        ifelse ( biological.sex = "female" )                   ; Identify the female participant and
        [ conceive-with target ]                               ; she conceives new offspring.
        [ ask target [ conceive-with myself ]]

      ]
    ]
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; FEMALE CALLER CONCEIVES WITH TARGET
;
; This subroutine can only be performed by a female caller.
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to conceive-with [ target ]
  complete-action target "conceive-with" 0
  if ( biological.sex = "female"                               ; Perform a final check to make sure that
    and fertility.status = "cycling"                           ; conception only occurs if the caller and
    and life.history = "adult"                                 ; target fit a set of criteria, including
    and [life.history] of target = "adult"                     ; that the caller is an adult cycling female
    and [biological.sex] of target = "male"                    ; and the target is an adult male and they
    and conception.chance > 0                                  ; are both alive and capable of conception.
    and [conception.chance] of target > 0
    and is.alive = true
    and [ is.alive ] of target = true )
  [                                                            ; If all of these criteria are met...
    let preferred-litter base-litter-size ^ litter.size        ; Determine how many offspring to produce
    let floor-litter floor preferred-litter                    ; based on caller's litter.size attribute,
    let percent-litter preferred-litter - floor-litter         ; which results in a number of offspring
    let my-litter-size ifelse-value                            ; between 1 and 10.
    ( random-float 1.0 < percent-litter )                      ; The final number of offspring produced
    [ floor-litter + 1 ]                                       ; is based on which integer value is closest
    [ floor-litter ]                                           ; to this calculation.

    hatch-anima1s my-litter-size [                             ; Create a number of offspring based on the
      initialize-from-parents myself target ]                  ; litter calculation above and initialize these
                                                               ; new individuals. The new mother updates her
    set fertility.status "pregnant"                            ; fertility status to pregnant.

    repeat my-litter-size [ set conceptions.history lput       ; Create a record of this conception with
      [my.identity] of target                                  ; the target.
      conceptions.history ]

    ask target [                                               ; The target also creates a record of this
      set conceptions.history lput                             ; conception with the caller.
      [my.identity] of myself
      conceptions.history ]
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;  _           _   _     _           _   _                 _     _
; (_)  _ __   (_) | |_  (_)   __ _  | | (_)  ____   __ _  | |_  (_)   ___    _ __
; | | | '_ \  | | | __| | |  / _` | | | | | |_  /  / _` | | __| | |  / _ \  | '_ \
; | | | | | | | | | |_  | | | (_| | | | | |  / /  | (_| | | |_  | | | (_) | | | | |
; |_| |_| |_| |_|  \__| |_|  \__,_| |_| |_| /___|  \__,_|  \__| |_|  \___/  |_| |_|
;
; --------------------------------------------------------------------------------------------------------- ;

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER IS INITIALIZED
;
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to initialize-from-parents [ m f ]
  set label-color white                                        ;
  set my.identity random 9999999                               ; Set identity to a random number.
  let mean-sex-ratio mean (list                                ;
    [sex.ratio] of m                                           ;
    [sex.ratio] of f)                                          ;
  set biological.sex ifelse-value                              ;
  ( random-float 1.0 < mean-sex-ratio )                        ;
  ["male"] ["female"]                                          ;
  set life.history "gestatee"                                  ;
  set fertility.status " "                                     ;
  set group.identity [group.identity] of m                     ;
  set is.alive true                                            ;
  set carried.items []                                         ;
  set fully.decayed false                                      ;
  set energy.supply 0                                          ;
  set x.magnitude 0                                            ;
  set y.magnitude 0                                            ;
  ifelse ( member? "no-evolution" model-structure )            ;
  [ set chromosome.I [chromosome.I] of m                       ;
    set chromosome.II [chromosome.II] of m ]                   ;
  [ setup-chromosomes-from m f ]
  set my.environment []
  set decision.vectors []
  set actions.completed []
  set age.in.ticks 0                                           ;
  set generation.number ( [generation.number] of m + 1 )       ;
  set my.mother m
  set mother.identity [my.identity] of m                       ;
  set father.identity [my.identity] of f                       ;
  set natal.group.identity group.identity
  set natal.group.size count anima1s with [
    group.identity = [group.identity] of myself
    and is.alive ]
  set death.group.identity 0
  set death.group.size 0
  set ticks.at.conception ticks
  set ticks.at.birth 0
  set ticks.at.weaning 0
  set ticks.at.sexual.maturity 0
  set ticks.at.death 0
  set adult.hidden.chance 0
  set adult.survival.chance 0
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
  set distance.traveled 0
  set mother.initiated.birth false
  set mother.initiated.weaning false
  set whole.related.help.cost 0
  set half.related.help.cost 0
  set fourth.related.help.cost 0
  set eighth.related.help.cost 0
  set not.related.help.cost 0
  set whole.related.attack.cost 0
  set half.related.attack.cost 0
  set fourth.related.attack.cost 0
  set eighth.related.attack.cost 0
  set not.related.attack.cost 0
  set foraging.gains 0
  set total.energy.gains 0
  set total.energy.cost 0
  set help.from.history []
  set attack.from.history []
  set copulations.history []
  set conceptions.history []
  set group.transfers.history []
  set infanticide.history []
  set cause.of.death ""
  ifelse ( member? "ideal-form" model-structure )
  [ set-phenotype-to-ideal-form ]
  [ set-phenotype-to-initialized-form ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; PHENOTYPE OF CALLER IS INITIALIZED
;
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to set-phenotype-to-initialized-form
  ask my.mother [ set carried.items                             ; Mother has just conceived the caller
    lput myself carried.items ]                                 ; so caller is initially placed in her 'womb'
  set hidden? true                                              ; Caller is a gestatee and so initially hidden.
  set is.resting true                                           ; Caller is initially resting.
  set body.size 0                                               ; Caller is mainly given starting attributes
  set body.shade 0                                              ; with false or 0 value.
  set bite.capacity 0.01                                        ; The exceptions to this is that the caller
  set mutation.chance 0.05                                      ; initially has a non-zero mouth size,
  set sex.ratio 0.5                                             ; a preferred offspring sex ratio of 50 : 50,
  set survival.chance 1                                         ; and starting with a 100% chance of curvival.
  set litter.size 0
  set conception.chance 0
  set hidden.chance 0
  set birthing.chance 0
  set weaning.chance 0
  set infancy.chance 0
  set juvenility.chance 0
  set adulthood.chance 0
  set yellow.chance 0
  set red.chance 0
  set blue.chance 0
  set yellow.signal false
  set red.signal false
  set blue.signal false
  set visual.angle 0
  set visual.range 0
  set day.perception 0
  set night.perception 0
end

; --------------------------------------------------------------------------------------------------------- ;
;
; PHENOTYPE OF CALLER IS INITIALIZED IF MODEL-STRUCTURE IS IDEAL-FORM
;
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to set-phenotype-to-ideal-form
  ask my.mother [
    let new-energy-supply ( energy.supply / 2 )
    set energy.supply new-energy-supply
    ask myself [ set energy.supply new-energy-supply ]]
  set hidden? false
  set is.resting false
  set body.size mean [body.size] of anima1s with [ is.alive ]
  set body.shade mean [body.shade] of anima1s with [ is.alive ]
  set bite.capacity mean [bite.capacity] of anima1s with [ is.alive ]
  set mutation.chance mean [mutation.chance] of anima1s with [ is.alive ]
  set sex.ratio mean [sex.ratio] of anima1s with [ is.alive ]
  set litter.size mean [litter.size] of anima1s with [ is.alive ]
  set conception.chance mean [conception.chance] of anima1s with [ is.alive ]
  set hidden.chance mean [hidden.chance] of anima1s with [ is.alive ]
  set birthing.chance mean [birthing.chance] of anima1s with [ is.alive ]
  set weaning.chance mean [weaning.chance] of anima1s with [ is.alive ]
  set infancy.chance mean [infancy.chance] of anima1s with [ is.alive ]
  set juvenility.chance mean [juvenility.chance] of anima1s with [ is.alive ]
  set adulthood.chance mean [adulthood.chance] of anima1s with [ is.alive ]
  set survival.chance mean [survival.chance] of anima1s with [ is.alive ]
  set yellow.chance mean [yellow.chance] of anima1s with [ is.alive ]
  set red.chance mean [red.chance] of anima1s with [ is.alive ]
  set blue.chance mean [blue.chance] of anima1s with [ is.alive ]
  set yellow.signal one-of modes [yellow.signal] of anima1s with [ is.alive ]
  set red.signal one-of modes [red.signal] of anima1s with [ is.alive ]
  set blue.signal one-of modes [blue.signal] of anima1s with [ is.alive ]
  set visual.angle mean [ visual.angle ] of anima1s with [ is.alive ]
  set visual.range mean [ visual.range ] of anima1s with [ is.alive ]
  set day.perception mean [ day.perception ] of anima1s with [ is.alive ]
  set night.perception mean [ night.perception ] of anima1s with [ is.alive ]
  update-to-adult                                               ; Update caller to adult status.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; MODIFY CHROMOSOME WITH NOVEL MUTATIONS AT RANDOM ALLELE LOCI
;
; This subroutine is called by indivifual anima1s and results in potentially modified versions of the current
; organization for the individual's chromosome.I and chromosome.II.
;
; ENTRY:  mutation-chance-per-locus | defines the chance that a mutation will
;           occur at each allele locus.
;
;         Caller has complete knowledge of current configuration of chromosome.I and chromosome.II
;
; EXIT:   When this subroutine is complete, both chromosome.I and chromosome.II have been updated with any modifications.
;
; --------------------------------------------------------------------------------------------------------- ;

to setup-chromosomes-from [ m f ]
                                                               ; BEHAVIOR CHROMOSOMES

  set chromosome.I []                                          ; Set up the caller's first chromosome.
  let i 0                                                      ; Iterate through the mother's chromosomes
  let chrome-i-length max ( list                               ; and select alleles to place in caller's
    length [chromosome.I] of m                                 ; chromosome.
    length [chromosome.II] of m )                              ;
  while [i < chrome-i-length ] [                               ; There is a 50 : 50 chance that the allele
    ifelse random-float 1.0 < 0.5                              ; will originate from mother's chromosome.I or II.
    [ ifelse ( i < length [chromosome.I] of m )
      [ set chromosome.I lput ( item i [chromosome.I] of m) chromosome.I ]
      [ set chromosome.I lput ( item i [chromosome.II] of m) chromosome.I ]]
    [ ifelse ( i < length [chromosome.II] of m )
      [ set chromosome.I lput ( item i [chromosome.II] of m) chromosome.I ]
      [ set chromosome.I lput ( item i [chromosome.I] of m) chromosome.I ]]
    set i i + 1 ]

  set chromosome.II []                                         ; Set up the caller's second chromosome.
  set i 0                                                      ; Iterate through the father's chromosomes
  let chrome-ii-length max ( list                              ; and select alleles to place in caller's
    length [chromosome.I] of f                                 ; chromosome.
    length [chromosome.II] of f )                              ;
  while [i < chrome-ii-length ] [                              ; There is a 50 : 50 chance that the allele
    ifelse random-float 1.0 < 0.5                              ; will originate from father's chromosome.I or II.
    [ ifelse ( i < length [chromosome.I] of f )
      [ set chromosome.II lput (item i [chromosome.I] of f) chromosome.II ]
      [ set chromosome.II lput (item i [chromosome.II] of f) chromosome.II ]]
    [ ifelse ( i < length [chromosome.II] of f )
      [ set chromosome.II lput (item i [chromosome.II] of f) chromosome.II ]
      [ set chromosome.II lput (item i [chromosome.I] of f) chromosome.II ]]
    set i i + 1 ]

  if random-float 1.0 < 0.5 [                                  ; There is a 50 : 50 chance that the caller's
    let chromosome-holder chromosome.I                         ; first and second chromsomes will switch places.
    set chromosome.I chromosome.II
    set chromosome.II chromosome-holder ]
                                                               ; IDENTITY CHROMOSOMES

  let identity.A [identity.I] of m                             ; Create placeholder identity chromosomes
  let identity.B [identity.II] of m                            ; that are identical to mother's.

  set i 0                                                      ; Iterate through the placeholder chromosomes.
  while [i < length identity.A ] [ if random-float 1.0 < 0.5   ; Half of the time, the alleles will switch
    [ let item.A item i identity.A                             ; places.
      let item.B item i identity.B
      set identity.A replace-item i identity.A item.B
      set identity.B replace-item i identity.B item.A ]
    set i i + 1 ]

  let chosen-m-identity one-of ( list identity.A identity.B )  ; Identify the placeholder chromosome to
                                                               ; be inherited.

  let identity.C [identity.I] of f                             ; Create placeholder identity chromosomes
  let identity.D [identity.II] of f                            ; that are identical to father's.

  set i 0                                                      ; Iterate through the placeholder chromosomes.
  while [i < length identity.C ] [ if random-float 1.0 < 0.5   ; Half of the time, the alleles will switch
    [ let item.C item i identity.C                             ; places.
      let item.D item i identity.D
      set identity.C replace-item i identity.C item.D
      set identity.D replace-item i identity.D item.C ]
    set i i + 1 ]

  let chosen-f-identity one-of ( list identity.C identity.D )  ; Identify the placeholder chromosome to
                                                               ; be inherited.

  ifelse ( one-of [ true false ] ) [                           ; There is a 50 : 50 chance that the
    set identity.I chosen-m-identity                           ; identity chromosomes will switch
    set identity.II chosen-f-identity                          ; places.
  ][
    set identity.I chosen-f-identity
    set identity.II chosen-m-identity
  ]

  let rate-of-mutation mean ( list                             ; Calculate the chance of mutation at
    [mutation.chance] of m                                     ; each locus based on mother and father's
    [mutation.chance] of f )                                   ; mutation.chance attributes and apply
  mutate-chromosomes rate-of-mutation                          ; to caller's chromosomes.

end

; --------------------------------------------------------------------------------------------------------- ;
;
; MODIFY CHROMOSOME WITH NOVEL MUTATIONS AT RANDOM ALLELE LOCI
;
; This subroutine is called by indivifual anima1s and results in potentially modified versions of the current
; organization for the individual's chromosome.I and chromosome.II.
;
; ENTRY:  mutation-chance-per-locus | defines the chance that a mutation will
;           occur at each allele locus.
;
;         Caller has complete knowledge of current configuration of chromosome.I and chromosome.II
;
; EXIT:   When this subroutine is complete, both chromosome.I and chromosome.II have been updated with any modifications.
;
; --------------------------------------------------------------------------------------------------------- ;

to mutate-chromosomes [ mutation-chance-per-locus ]
  set chromosome.I mutate-chromosome                           ; Apply mutations to the alleles in
  chromosome.I mutation-chance-per-locus                       ; both behavior chromosomes.
  set chromosome.II mutate-chromosome
  chromosome.II mutation-chance-per-locus

  let letter-list [                                            ; Create list of all upper case alphabet letters.
    "A" "B" "C" "D" "E"
    "F" "G" "H" "I" "J"
    "K" "L" "M" "N" "O"
    "P" "Q" "R" "S" "T"
    "U" "V" "W" "X" "Y"
    "Z" ]

  let i 0                                                      ; Iterate through the first identity
  while [ i < length identity.I ] [                            ; chromosome and apply mutation at
    if random-float 1.0 < mutation-chance-per-locus            ; the rate of input value.
    [ let new-letter one-of letter-list
      set identity.I replace-item i identity.I new-letter ]
    set i i + 1 ]

  set i 0                                                      ; Iterate through the second identity
  while [ i < length identity.I ] [                            ; chromosome and apply mutation at
    if random-float 1.0 < mutation-chance-per-locus            ; the rate of input value.
    [ let new-letter one-of letter-list
      set identity.II replace-item i identity.II new-letter ]
    set i i + 1 ]

end

; --------------------------------------------------------------------------------------------------------- ;
;
; MODIFY CHROMOSOME WITH NOVEL MUTATIONS AT RANDOM ALLELE LOCI
;
; This subroutine creates a copy of a given chromosome with a subset
; of its alleles modified according to the probability of a mutation occuring at
; each locus. See the overall description of the program for the structure of
; chromosomes, alleles, and codons.
;
; ENTRY:  input-chromosome | is the chromosome to be copied.
;
;         mutation-chance-per-locus | defines the chance that a mutation will
;           occur at each allele locus.
;
;         model-structure | defines the type of code used. In particular, if
;           it is set to "uninvadable," then no new codons may be created.
;
;
; EXIT:   This subroutine returns a copy of the chromosome with
;           modifications to a subset of the alleles.
;
; --------------------------------------------------------------------------------------------------------- ;

to-report mutate-chromosome [ input-chromosome mutation-chance-per-locus ]

  let ouput-chromosome []                                      ; Establish an empty chromosome.

  foreach input-chromosome [ allele ->                         ; Begin a loop through each allele
    let updated-alleles (list allele)                          ; in chromosome supplied on entry.

    let first-allele first allele

    if ( ( first-allele != "0|"                                ; The allele is able to mutate if the first codon is not [0] or false
      or first-allele = true )                                 ; Check whether the allele is mutable and if ; this line is for capatibility with older genotype files that have a boolean in first position
      and random-float 1.0 < mutation-chance-per-locus ) [     ; probability dictates that it should be changed.

      let choice ( ifelse-value                                ; Check first-allele for range of mutation choices allowed
        ( first-allele = "1|" ) [ 1 ]
        ( first-allele = "2|" ) [ one-of [ 1 2 ] ]
        ( first-allele = "3|" ) [ one-of [ 1 2 3 ] ]
        ( first-allele = "4|" ) [ one-of [ 1 2 3 4 5 ] ]
        ( first-allele = "5|" ) [ one-of [ 1 2 3 4 5 6 7 ] ]
        [ one-of [ 1 2 3 4 5 6 7 ] ] )                         ; Or return any choice when first-allele is true

      (ifelse                                                  ; Select the proper case.

                                                               ; Prepare to mutate the allele
        ( choice < 6 ) [                                       ; Generate a random number between 0 and 4.
          let new-allele []
          let random-index random ( length allele - 1 ) + 1    ; excludes first codon from mutation
          let index 0
          foreach allele [ codon ->                            ; Proceed to examine each codon in the allele.
            ( ifelse

              ( choice = 1                                     ; CHOICE 1: mutate codon if it is a number ; unless the structure is uninvadable.
                and random-index = index
                and not member? "uninvadable" model-structure )
              [ set new-allele
                lput get-mutation codon "numbers" new-allele ]

              ( choice = 2                                     ; CHOICE 2: mutate codon, letters only unless the structure is uninvadable.
                and random-index = index
                and not member? "uninvadable" model-structure )
              [ set new-allele
                lput get-mutation codon "letters" new-allele ]

              ( choice = 3                                     ; CHOICE 3: mutate codon, both numbers and letters unless the structure is uninvadable.
                and random-index = index
                and not member? "uninvadable" model-structure )
              [ set new-allele
                lput get-mutation codon "both" new-allele ]

              ( choice = 4                                     ; CHOICE 4: duplicate codon
                and random-index = index )
              [ repeat 2
                [ set new-allele lput codon new-allele ] ]

              ( choice = 5                                     ; CHOICE 5: delete codon
                and random-index = index ) [  ]

              [ set new-allele lput codon new-allele ])        ; Copy untouched codons into the new allele.
            set index index + 1 ]                              ; Advance and repeat for all codons,
          set updated-alleles ( list new-allele ) ]            ; then update the allele.

        ( choice = 6 ) [                                       ; CHOICE 6: duplicate allele making an exact copy
          set updated-alleles (list allele allele) ]

        ( choice = 7 ) [                                       ; CHOICE 7: delete allele completely
          set updated-alleles [] ]

        [])]

    foreach updated-alleles [ allele-update ->                 ; Finally, if modification occurred
      set ouput-chromosome ifelse-value                        ; in any allele, then update the
      ( allele-update != [] )                                  ; chromosome with those modified
      [ lput allele-update ouput-chromosome ]                  ; alleles.
      [ ouput-chromosome ]]
  ]
  report ouput-chromosome                                      ; Return the results.
end


; --------------------------------------------------------------------------------------------------------- ;
;
; GET A MODIFIED VERSION OF A GIVEN CODON
;
; This subroutine takes a codon and returns a mutated version of that codon.
;
; ENTRY:  unmutated-codon | is the initial codon.
;
;         type-of-mutation | defines the what type of mutation can occur "numbers," "letters," or "both"
;
;
; EXIT:   THis subroutine returns a mutant verion of the unmutated-codon
;
; --------------------------------------------------------------------------------------------------------- ;

to-report get-mutation [ unmutated-codon type-of-mutation ]

  report ( ifelse-value                                        ; Return the mutation based on:

    ( genotype-reader = "sta2us" )                             ; If genotype-reader is sta2us..
    [ sta7us-get-mutation unmutated-codon type-of-mutation]    ; then get mutation from sta2us extension

    ( genotype-reader = "gat3s" )                              ; If genotype-reader is gat3s..
    [ g8tes-get-mutation unmutated-codon type-of-mutation]     ; then get mutation from gat3s extension

    [ sta7us-get-mutation unmutated-codon type-of-mutation ] ) ; default mutation if gentoype-reader not indicated
end
@#$#@#$#@
GRAPHICS-WINDOW
7
86
838
918
-1
-1
8.23
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
0
0
1
timesteps
30.0

BUTTON
361
10
427
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
432
10
499
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
354
79
path-to-experiment
../results/thesis-geladas-test/
1
0
String

BUTTON
505
10
580
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
1118
117
observation-notes
NIL
1
0
String

MONITOR
20
98
93
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
849
466
1118
499
plant-minimum-neighbors
plant-minimum-neighbors
0
8
0.0
.1
1
NIL
HORIZONTAL

SLIDER
849
503
1118
536
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
849
393
1118
426
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
849
321
1117
354
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
849
357
1118
390
plant-daily-cycle
plant-daily-cycle
1
100
11.0
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
solar-status
17
1
11

INPUTBOX
849
122
995
191
population
four-groups-plus-males
1
0
String

INPUTBOX
849
197
995
265
genotype
geladas
1
0
String

BUTTON
1001
122
1056
191
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
1062
122
1117
155
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
1062
158
1117
191
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
543
1057
588
useful-commands
useful-commands
"help-me" "meta-report" "show-territories" "---------------------" " > OPERATIONS   " "---------------------" "parameter-settings" "default-settings" "model-structure" "-- aspatial" "-- free-lunch" "-- ideal-form" "-- no-evolution" "-- no-plants" "-- reaper" "-- signal-fertility" "-- stork" "-- uninvadable" "clear-population" "new-population" "reset-plants" "save-world" "import-world" "save-simulation" "output-results" "---------------------" " > VERIFICATION " "---------------------" "dynamic-check" "-- true" "-- false" "runtime-check" "visual-check" "-- attack-pattern" "-- dine-and-dash" "-- life-history-channel" "-- musical-pairs" "-- night-and-day" "-- popularity-context" "-- speed-mating" "-- square-dance" "-- supply-and-demand" "---------------------" " > DISPLAY RESULTS   " "---------------------" "age" "generations" "life-history" "genotype" "phenotype" "-- survival-chance" "-- body-size" "-- body-shade" "-- fertility-status" "-- hidden-chance" "-- bite-capacity" "-- mutation-chance" "-- sex-ratio" "-- litter-size" "-- conception-chance" "-- visual-angle" "-- visual-range" "-- day-perception" "-- night-perception" "-- yellow-chance" "-- red-chance" "-- blue-chance" "-- birthing-chance" "-- weaning-chance" "-- infancy-chance" "-- juvenility-chance" "-- adulthood-chance" "carried-items" "energy-supply" "behaviors" "-- environment" "-- decisions" "-- actions" "-- birthing" "-- weaning" "-- matings" "-- mating-partners" "-- conceptions" "-- infanticide" "-- group-transfers" "-- travel-distance" "-- foraging-gains" "-- total-energy-gains" "-- total-energy-cost" "-- receiving-history" "-- carried-history" "-- aid-history" "-- harm-history"
75

BUTTON
1063
543
1118
588
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
1002
196
1057
265
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
1062
232
1117
265
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
1062
196
1117
229
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
849
429
1118
462
plant-quality
plant-quality
.1
100
10.0
.1
1
NIL
HORIZONTAL

SWITCH
588
46
755
79
output-results?
output-results?
1
1
-1000

SWITCH
588
10
755
43
selection-on?
selection-on?
1
1
-1000

PLOT
849
646
1696
878
plot
x
y
0.0
8.0
0.0
1.0
true
true
"" ""
PENS
"cycling female" 0.1 1 -2064490 true "" ""
"pregnant female" 1.0 0 -5825686 true "" ""
"lactating female" 1.0 0 -8630108 true "" ""
"adult male" 1.0 0 -13791810 true "" ""
"juvenile female" 1.0 0 -13840069 true "" ""
"juvenile male" 1.0 0 -10899396 true "" ""
"infant female" 1.0 0 -1184463 true "" ""
"infant male" 1.0 0 -6459832 true "" ""
"gestatee female" 1.0 0 -955883 true "" ""
"gestatee male" 1.0 0 -2674135 true "" ""

CHOOSER
849
271
1118
316
genotype-reader
genotype-reader
"sta2us" "gat3s"
0

SWITCH
1274
885
1409
918
adults
adults
1
1
-1000

SWITCH
1131
885
1269
918
juveniles
juveniles
1
1
-1000

SWITCH
990
885
1126
918
infants
infants
1
1
-1000

SWITCH
849
885
985
918
gestatees
gestatees
1
1
-1000

SWITCH
1415
885
1553
918
males
males
1
1
-1000

SWITCH
1558
885
1696
918
females
females
1
1
-1000

CHOOSER
849
594
1118
639
plot-type
plot-type
"individuals" "groups" "population" "generations"
0

MONITOR
1130
594
1696
639
plot-information
\"Individual Plot of Body Size for Adult Females\"
17
1
11

SLIDER
1287
123
1546
156
simulation-summary-ticks
simulation-summary-ticks
0
10000
0.0
500
1
NIL
HORIZONTAL

SLIDER
1287
428
1546
461
verification-rate
verification-rate
0
1
0.0
0.001
1
NIL
HORIZONTAL

SWITCH
1287
237
1546
270
record-individuals-on
record-individuals-on
1
1
-1000

SLIDER
1287
161
1546
194
simulation-scan-ticks
simulation-scan-ticks
0
10000
0.0
250
1
NIL
HORIZONTAL

SLIDER
1287
199
1546
232
group-scan-ticks
group-scan-ticks
0
10000
0.0
500
1
NIL
HORIZONTAL

SLIDER
1287
275
1546
308
individual-scan-ticks
individual-scan-ticks
0
10000
0.0
500
1
NIL
HORIZONTAL

SLIDER
1287
389
1546
422
view-scan-ticks
view-scan-ticks
0
10000
10000.0
500
1
NIL
HORIZONTAL

SLIDER
1287
351
1546
384
genotype-scan-ticks
genotype-scan-ticks
0
10000
0.0
500
1
NIL
HORIZONTAL

SLIDER
1287
313
1546
346
focal-follow-rate
focal-follow-rate
0
1
0.0
0.0001
1
NIL
HORIZONTAL

OUTPUT
1132
10
1698
587
12

@#$#@#$#@
# B3GET 1.2.0 INFORMATION

## WHAT IS IT?

B3GET is designed to test hypotheses in biology by simulating populations of virtual organisms evolving over generations, whose evolutionary outcomes reflect the selection pressures of their environment. Users input population files to seed the initial population and run simulations to evolve these populations - and their genotypes - over generations. Behavioral strategies that are beneficial for their ecological context are expected to emerge.

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
--------- sta7us.nls
--------- import-export.nls
--------- selection.nls
--------- verification.nls
--- [data]
------ genotype.txt
------ population.csv
--- [results]
--- [docs]
------ B3GET-ODD-protocol.pdf

B3GET starts with PATH-TO-EXPERIMENT set to [../results/], which means that any files generated during simulation will be saved in the [results] folder. Initially, POPULATION is set to [population] and GENOTYPE is set to [genotype], which are files included during download. With these settings, you can just click SETUP and GO to start your first simulation! Please refer to the descriptions of the controls below to perform more complex tasks.

### PRIMARY CONTROLS

PATH-TO-EXPERIMENT: the path indicating where to store data for the current experiment.
SETUP: returns the model to the starting state.
GO: runs the simulation.
GO ONCE: runs exactly one tick, or timestep, of the simulation.
COLLECT-DATA?: 'ON' collects data on the animals (see "data" extension).
SELECTION-ON?: 'ON' artificially culls the animal population (see "selection" extension).
SAVE: records the current simulation state and documentation-notes in an external file.
OBSERVATION-NOTES: write notes to yourself here and click 'save' to save them.

### VIEW INFORMATION

The main view screen allows us to visually see emergent behaviors.

SIMULATION: the unique identification code of the current simulation.
SEASON: cycles between 1.0 (summer) and -1.0 (winter) according to the plant-annual-cycle.
TIME: varies from DAY to NIGHT according to plant-daily-cycle.

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

### ENVIRONMENTAL CONTROLS

Plants, plant life and growth are inspired by Conway's Game of Life, a cellular automaton model that contained rules for when a cell could be 'alive' or 'dead', thus simulating a living ecosystem. In B3GET, 'alive' cells contain plant agents, depicated as a green squares in the model.

PLANT-ANNUAL-CYCLE: the length of a year in timesteps.
PLANT-SEASONALITY: the degree of difference in plant abundance from summer to winter.
PLANT-MINIMUM-NEIGHBORS: preferred minimum number of plant neighbors for each plant.
PLANT-MAXIMUM-NEIGHBORS: preferred maximum number of neighbors for each plant.
PLANT-DAILY-CYCLE: the length of a day in timesteps.
PLANT-QUALITY: the maximum energy that any plant can contain.

### MONITORING CONTROLS

While you can use the BehaviorSpace functionality to 'grow' many populations at once, sometimes it is useful to visually observe and influence a single simulation. The monitoring controls allow for some direct observation and influence.

USEFUL-COMMANDS: select from a list of premade functions.
▷: hit this button to run the command selected by USEFUL-COMMANDS
PLOT: outputs a range of plots, which are accessed in useful-commands.
OUPUT: many controls output some text, which shows up in this window.

### EXTENSIONS

Detailed information on what each extension does can be found in those files. Here is a brief list of all extensions and briefly what they do:

DATA: controls how files are created and data is stored within them.
INTERFACE: controls the extra commands to use during experimentation.
GAT3S: a more complex genotype reader.
RESULTS: controls for generating data.
STA2US: a simple genotype file reader.
SELECTION: controls for artificial selection of agents during simulation.
VERIFICATION: the verification code for this model.

### NEW EXPERIMENT

If you want to start a new experiment and store information in a separate place, simply create a new folder in [data] and make sure to update the PATH-TO-EXPERIMENT with this new folder path. You must add a poulation file to this new folder (the default file popu1ation would be a fine choice), and update the POPULATION input to this population file name. You can also add a genotype file and update the GENOTYPE input, but this is not neccessary because the population file also contains the genotypes for every agent.

## THINGS TO NOTICE

This model is designed to explore the behavior space of virtual organisms who are subject to the same kinds of ecological constraints we observe in nature. In your simulation, what behaviors emerge in your population and why might these behaviors be part of a successful strategy? Additionally, you can observe population level dynamics like the lotka-volterra cycles of the organism and plant populations.

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

The actions listed above are the range of possible actions that an agent can take. However, whether an agent performs these actions, how much effort they put into doing so, and who they target is up to their genotype. An indefinite number of genotype file configurations are possible, as long as they include the following: (1) each row represents one allele, (2) these alleles represent self-contained procedures that generate decision-vectors from considering the environment as input, and (3) each row contains a list of codons that can be altered during recombination and mutation. This version of B3GET comes with two genotype file extensions: sta7us and g8tes (beta version). Specific information about each file type can be found within those extension files.

## Phenotypes

When agents perform actions, for the most part this results in changes in the state variables of themselves or others. These states can be thought of as an organism's phenotype, the set of virtual 'organs' which emerges from a combination of an organism's genotype and its life experiences interacting with its environment.

### VISIBLE 'ORGANS'

SIZE: the overall body size or mass of an individual.
COLOR: color represents group identity and shade correlates with age.
SEX: either "male" or "female".
LIFE HISTORY: "gestatee", "infant", "juvenile", or "adult".
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
NetLogo 6.2.0
@#$#@#$#@
setup
set grass? true
repeat 75 [ go ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="TEST-QJZWXF" repetitions="1" runMetricsEveryStep="false">
    <setup>setup

; give simulation-id specific configuration: sDOB17 means
; simulation of WORLD-D, Baboons seed population,
; run B (instead of A), plant-minimum-neighbors = 1 and
; plant-maximum-neighbors = 7
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
    <final>;record-simulation
;record-world
output-print (word "simulation " behaviorspace-run-number " ends at " ticks " ticks." )</final>
    <timeLimit steps="10"/>
    <exitCondition>not any? anima1s with [ is.alive ]</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-annual-cycle">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-daily-cycle">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-seasonality">
      <value value="0.3"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-quality">
      <value value="3"/>
      <value value="6"/>
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
      <value value="&quot;pQJZWXF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="TEST-QKSOH9" repetitions="1" runMetricsEveryStep="false">
    <setup>setup

; give simulation-id specific configuration: sDOB17 means
; simulation of WORLD-D, Baboons seed population,
; run B (instead of A), plant-minimum-neighbors = 1 and
; plant-maximum-neighbors = 7
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
    <final>record-simulation
record-world</final>
    <timeLimit steps="101"/>
    <exitCondition>not any? anima1s with [ is.alive ]</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/test-QKSOH9/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-annual-cycle">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-daily-cycle">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-seasonality">
      <value value="0.3"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-quality">
      <value value="3"/>
      <value value="6"/>
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
      <value value="&quot;pQJZWXF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>carefully [ collect-data "../results/output.csv" ] [ output-print error-message ]</final>
    <timeLimit steps="10"/>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-annual-cycle">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-daily-cycle">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-seasonality">
      <value value="0.3"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="&quot;pQJZWXF&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-6" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>carefully [ collect-data "../results/output.csv" ] [ output-print error-message ]</final>
    <timeLimit steps="10"/>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
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
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-minimum-neighbors">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-maximum-neighbors">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="&quot;pQJZWXF&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-24" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>carefully [ collect-data "../results/output.csv" ] [ output-print error-message ]</final>
    <timeLimit steps="10"/>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
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
      <value value="5"/>
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
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="&quot;pQJZWXF&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-72" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <final>carefully [ collect-data "../results/output.csv" ] [ output-print error-message ]</final>
    <timeLimit steps="1000"/>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
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
      <value value="5"/>
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
      <value value="&quot;pQJZWXF&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sAO1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
set-simulation-id</setup>
    <go>go</go>
    <final>simulation-summary
record-world</final>
    <timeLimit steps="100000"/>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/thesis/&quot;"/>
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
    <enumeratedValueSet variable="population">
      <value value="&quot;Olives&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype">
      <value value="&quot;olives&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
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
      <value value="5"/>
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
    </enumeratedValueSet>
  </experiment>
  <experiment name="selection-experiment" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
set genotype generate-genotype-id
set population generate-population-id
save-population</setup>
    <go>go</go>
    <final>record-world</final>
    <timeLimit steps="50000"/>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/selection-experiment/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterioration-rate">
      <value value="-0.01"/>
      <value value="-0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="&quot;seed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
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
      <value value="5"/>
      <value value="10"/>
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
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="selection" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
set genotype generate-genotype-id
set population generate-population-id
save-population</setup>
    <go>go</go>
    <final>record-world</final>
    <exitCondition>any? anima1s with [ generation.number = 10 ]</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/selection/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterioration-rate">
      <value value="-0.01"/>
      <value value="-0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="&quot;seed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype">
      <value value="&quot;seed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
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
      <value value="5"/>
      <value value="10"/>
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
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="selection-alpha" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
set genotype generate-genotype-id
set population generate-population-id
save-population</setup>
    <go>go</go>
    <final>record-world</final>
    <exitCondition>any? anima1s with [ generation.number = 5 ]</exitCondition>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/seed-alpha/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterioration-rate">
      <value value="-0.01"/>
      <value value="-0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="&quot;seed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype">
      <value value="&quot;seed&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
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
      <value value="5"/>
      <value value="10"/>
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
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sAC1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
set-simulation-id</setup>
    <go>go</go>
    <final>simulation-summary
record-world</final>
    <timeLimit steps="500000"/>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/thesis/&quot;"/>
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
    <enumeratedValueSet variable="population">
      <value value="&quot;Chimpanzees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype">
      <value value="&quot;chimpanzees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
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
      <value value="20"/>
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
    </enumeratedValueSet>
    <enumeratedValueSet variable="verification-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="verification-rate">
      <value value="1.0E-4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-summary-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-summary-ticks">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="record-individuals-on">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scan-interval">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-scans-on">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-scans-on">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individual-scans-on">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="focal-follows-on">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="focal-follow-rate">
      <value value="1.0E-4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="AC1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
; give simulation-id specific configuration: sDO17B means
; simulation of WORLD-D, Baboons seed population,
; run B (instead of A), plant-minimum-neighbors = 1 and
; plant-maximum-neighbors = 7
ifelse ( plant-minimum-neighbors &lt; plant-maximum-neighbors ) [
  set simulation-id ( word "s" but-last behaviorspace-experiment-name plant-minimum-neighbors plant-maximum-neighbors "A" )
][
  let min-holder plant-minimum-neighbors
  let max-holder plant-maximum-neighbors
  set plant-minimum-neighbors max-holder - 1
  set plant-maximum-neighbors min-holder
  set simulation-id ( word "s" but-last behaviorspace-experiment-name plant-minimum-neighbors plant-maximum-neighbors "B" )
]</setup>
    <go>go</go>
    <final>record-world
simulation-summary</final>
    <timeLimit steps="500000"/>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/thesis/&quot;"/>
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
    <enumeratedValueSet variable="population">
      <value value="&quot;Chimpanzees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype">
      <value value="&quot;chimpanzees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
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
      <value value="5"/>
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
    </enumeratedValueSet>
    <enumeratedValueSet variable="verification-rate">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-summary-ticks">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-scan-ticks">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-scan-ticks">
      <value value="100000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individual-scan-ticks">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="view-scan-ticks">
      <value value="50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-scan-ticks">
      <value value="100000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="focal-follow-rate">
      <value value="1.0E-4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="record-individuals-on">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="observation-notes">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-quality">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="males">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infants">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="&quot;Chimpanzees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-seasonality">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="juveniles">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="selection-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gestatees">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="useful-commands">
      <value value="&quot;age&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="females">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-annual-cycle">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-minimum-neighbors">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-daily-cycle">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot-type">
      <value value="&quot;individuals&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adults">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-maximum-neighbors">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype">
      <value value="&quot;chimpanzees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/thesis/&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ACX" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
; give simulation-id specific configuration: sDO17B means
; simulation of WORLD-D, Baboons seed population,
; run B (instead of A), plant-minimum-neighbors = 1 and
; plant-maximum-neighbors = 7
ifelse ( plant-minimum-neighbors &lt; plant-maximum-neighbors ) [
  set simulation-id ( word "s" but-last behaviorspace-experiment-name plant-minimum-neighbors plant-maximum-neighbors "A" )
][
  let min-holder plant-minimum-neighbors
  let max-holder plant-maximum-neighbors
  set plant-minimum-neighbors max-holder - 1
  set plant-maximum-neighbors min-holder
  set simulation-id ( word "s" but-last behaviorspace-experiment-name plant-minimum-neighbors plant-maximum-neighbors "B" )
]</setup>
    <go>go</go>
    <final>record-world
simulation-summary</final>
    <timeLimit steps="5000"/>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/thesis/&quot;"/>
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
    <enumeratedValueSet variable="population">
      <value value="&quot;Chimpanzees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype">
      <value value="&quot;chimpanzees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
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
      <value value="5"/>
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
    </enumeratedValueSet>
    <enumeratedValueSet variable="verification-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="verification-rate">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-summary-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-summary-ticks">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-scans-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-scan-ticks">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-scans-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-scan-ticks">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individual-scans-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individual-scan-ticks">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="view-scans-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="view-scan-ticks">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-scans-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-scan-ticks">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="focal-follows-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="focal-follow-rate">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="record-individuals-on">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ACY" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
; give simulation-id specific configuration: sDO17B means
; simulation of WORLD-D, Baboons seed population,
; run B (instead of A), plant-minimum-neighbors = 1 and
; plant-maximum-neighbors = 7
ifelse ( plant-minimum-neighbors &lt; plant-maximum-neighbors ) [
  set simulation-id ( word "s" but-last behaviorspace-experiment-name plant-minimum-neighbors plant-maximum-neighbors "A" )
][
  let min-holder plant-minimum-neighbors
  let max-holder plant-maximum-neighbors
  set plant-minimum-neighbors max-holder - 1
  set plant-maximum-neighbors min-holder
  set simulation-id ( word "s" but-last behaviorspace-experiment-name plant-minimum-neighbors plant-maximum-neighbors "B" )
]</setup>
    <go>go</go>
    <final>record-world
simulation-summary</final>
    <timeLimit steps="5000"/>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../results/thesis/&quot;"/>
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
    <enumeratedValueSet variable="population">
      <value value="&quot;Chimpanzees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype">
      <value value="&quot;chimpanzees&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-reader">
      <value value="&quot;sta2us&quot;"/>
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
      <value value="5"/>
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
    </enumeratedValueSet>
    <enumeratedValueSet variable="verification-on">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="verification-rate">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-summary-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-summary-ticks">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-scans-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-scan-ticks">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-scans-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-scan-ticks">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individual-scans-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="individual-scan-ticks">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="view-scans-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="view-scan-ticks">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-scans-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype-scan-ticks">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="focal-follows-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="focal-follow-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="record-individuals-on">
      <value value="false"/>
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
