; ========================================================================================================= ;
;
;   888888ba  d8888b.  .88888.   88888888b d888888P
;   88    `8b     `88 d8'   `88  88           88
;  a88aaaa8P'  aaad8' 88        a88aaaa       88
;   88   `8b.     `88 88   YP88  88           88
;   88    .88     .88 Y8.   .88  88           88
;   88888888P d88888P  `88888'   88888888P    dP
;
; ========================================================================================================= ;
;
; This program is to simulate populations of virtual organisms evolving over generations, whose evolutionary
; outcomes reflect the selection pressures of their environment. The program is divided into the following
; sections:
;
; * SETUP. Prepares for operation of the program.
;
; * GO. Advances the simulation by one timestep.
;    - Global. Subroutines that are called globally or affect global variables.
;    - Plants. Subroutines that relate to the seasonal and daily growth of plants.
;    - Individuals. Subroutines that handle individual appearance and mortality.
;
; * ENGINE. Allows anima1s to assess their environments and enact behaviors.
;    - Environment. Anima1s identify the objects in their environment.
;    - Decisions. Anima1s make decisions about how to proceed.
;    - Actions. Anima1s enact their decisions if they have sufficient energy.
;
; * ACTIONS. A collection of subroutines to carry out the anima1 actions.
;    - Intraactions. Actions that occuring internally.
;    - Movement. Actions that affect changing spatial location or direction.
;    - Signaling. Actions that affect phenotypic displays.
;    - Development. Actions that dictate anima1 growth and development.
;    - Energy. Actions that modify the current energy supply of anima1s.
;    - Interactions. Actions that occur between two anima1s.
;    - Initialization. Subroutines for creating a new anima1.
;
; * ANALYSIS. Work in progress and user analysis subroutines.
;
; * DATA. Handles importing data and saving new dat for later import.
;    - Populations. Import and export population information.
;    - Genotypes. Import and export genotype information.
;
; * GENOTYPE READER. Converts environmental and genotype information into a set of decision vectors.
;    - Sta2us. A more limited but more efficient genotype reader.
;    - Gat3s. A less limited but less efficient genotype reader.
;
; * INTERFACE. Includes necessary processing for the interface.
;    - Buttons. Handles button click operations.
;    - Commands. Handles the operations for usesul-commands.
;    - Plots. Handles plot displays.
;
; * RESULTS. Records information on the current state of a simulation.
;    - Verification. Assessment of the internal code operations.
;    - Summary. Summary information on the simulation or specific individuals.
;    - Scans. Crossectional information from the simulation.
;    - Focal. Fine-grained longitudinal information on a specific individual.
;
; * SELECTION. Imposes artificial selection on populations (work in progress).
;
; * VERIFICATION. Checks the internal code operations.
;
; A note on terminology. This program is documented following the original terminology for computer programs
; introduced by John von Neumann and others, where there are "routines" and "subroutines." Later refinements
; added terms like "functions," "methods," and so forth, but this program keeps with the simpler and
; sufficient terminology.
;
; --------------------------------------------------------------------------------------------------------- ;

extensions [ csv profiler table time ]

__includes [ ; For more information on local extensions, see the corresponding files.

  "extensions/analysis.nls"
  "extensions/data.nls"
  "extensions/gat3s.nls"
  "extensions/interface.nls"
  "extensions/results.nls"
  "extensions/selection.nls"
  "extensions/sta2us.nls"
  "extensions/verification.nls" ]

; ========================================================================================================= ;
;
;  dP     dP  .d888888   888888ba  dP  .d888888   888888ba  dP         88888888b .d88888b
;  88     88 d8'    88   88    `8b 88 d8'    88   88    `8b 88         88        88.    "'
;  88    .8P 88aaaaa88a a88aaaa8P' 88 88aaaaa88a a88aaaa8P' 88        a88aaaa    `Y88888b.
;  88    d8' 88     88   88   `8b. 88 88     88   88   `8b. 88         88              `8b
;  88  .d8P  88     88   88     88 88 88     88   88    .88 88         88        d8'   .8P
;  888888'   88     88   dP     dP dP 88     88   88888888P 88888888P  88888888P  Y88888P
;
; ========================================================================================================= ;

; --------------------------------------------------------------------------------------------------------- ;
;
; CHARACTERISTICS OF ANIMA1S
;
; The term "anima1" is used with the digit "1" instead of an "l" to mark these as virtual animals.
;
; --------------------------------------------------------------------------------------------------------- ;

breed [ anima1s anima1 ]

anima1s-own [

  my.identity                      ; Unique identification number for each individual

  ; --------------------------------------------------------------------------------------------------------
  ; ATTRIBUTES THAT ARE VISIBLE TO ALL INDIVIUDALS
  ; --------------------------------------------------------------------------------------------------------

  biological.sex                   ; Either male or female
  life.history                     ; Either gestatee, infant, juvenile or adult
  fertility.status                 ; Either cycling, pregnant or lactating
  group.identity                   ; Number that specifies both group affiliation and color
  is.alive                         ; Either true or false
  yellow.signal                    ; When true, yellow is visible on body
  red.signal                       ; When true, red is visible on body
  blue.signal                      ; When true, blue is visible on body
  body.size                        ; Current size of individual
  body.shade                       ; Current shade of color showing (base color determined by group.identity)
  is.resting                       ; When true, individual cannot perform "active" actions

  identity.I                       ; The identity chromosomes are inherited from parents and used
  identity.II                      ; to calculate genetic relatedness between two individuals
  carried.items                    ; Objects carried by the individual (eg gestatee, infants, parasites, etc)

  ; --------------------------------------------------------------------------------------------------------
  ; ATTRIBUTES THAT ARE HIDDEN FROM ALL INDIVIDUALS BUT ONESELF
  ; --------------------------------------------------------------------------------------------------------

  hidden.chance                    ; Probability that an animal will become hidden from view
  fully.decayed                    ; When true, individual is no longer available as food
  survival.chance                  ; Probability of surviving to the next timestep
  energy.supply                    ; Current amount of energy units available for performing actions
  bite.capacity                    ; Amount of energy units that can be consumed from a plant per timestep
  mutation.chance                  ; Probability of offspring having a mutation at each gene locus
  sex.ratio                        ; Probability of female conceiving a male as opposed to a female
  litter.size                      ; Preferred litter size of female
  conception.chance                ; Probability of conceiving during a mating event
  visual.angle                     ; Visual variables define the cone of perception
  visual.range                     ; within which the anima1 can see objects in its environment
  day.perception                   ; Probability of seeing an object in environment during the day
  night.perception                 ; Probability of seeing an object in environment at night
  yellow.chance                    ; Probability of signaling yellow
  red.chance                       ; Probability of signaling red
  blue.chance                      ; Probability of signaling blue
  birthing.chance                  ; Probability of giving birth
  weaning.chance                   ; Probability of weaning offspring
  infancy.chance                   ; Probability of developing into an infant
  juvenility.chance                ; Probability of developing into a juvenile
  adulthood.chance                 ; Probability of developing into an adult
  x.magnitude                      ; Current level of desire to move along the horizontal plane
  y.magnitude                      ; Current level of desire to move along the vertical plane

  chromosome.I                     ; These two chromosomes comprise the genotype of an individual
  chromosome.II                    ; Each chromosome contains one or more alleles that dictate behaviors

  my.environment                   ; Collection of current objects in individual's environment
  decision.vectors                 ; List of current decisions that the individual has made
  actions.completed                ; Record of current actions that the individual has taken

  ; --------------------------------------------------------------------------------------------------------
  ; TRACKING VARIABLES FOR USERS ONLY - HIDDEN FROM ALL INDIVIDUALS
  ; --------------------------------------------------------------------------------------------------------

  solitary?                        ; When true, the anima1 is in a group by itself
  age.in.ticks                     ; The number of timesteps since the anima1 was conceived
  generation.number                ; Generation of the individual, which is one more than its mother
  my.mother                        ; Anima1's mother
  mother.identity                  ; Identity of the anima1's mother
  father.identity                  ; Identity of the anima1's father
  natal.group.identity             ; Identity of the group to which the individual was born
  natal.group.size                 ; Size of the group when the indivdual was born
  death.group.identity             ; Identity of the group at the time of the individual's death
  death.group.size                 ; Size of the group when the indivdual died
  ticks.at.conception              ; Timesteps when the individual was conceived
  ticks.at.birth                   ; Timesteps when the individual was born
  ticks.at.weaning                 ; Timesteps when the individual was weaned from its mother
  ticks.at.sexual.maturity         ; Timesteps when the individual became an adult
  ticks.at.death                   ; Timesteps when the individual died
  adult.hidden.chance              ; Record of hidden chance value upon reaching adulthood
  adult.survival.chance            ; Record of survival chance value upon reaching adulthood
  adult.body.size                  ; Record of body size value upon reaching adulthood
  adult.body.shade                 ; Record of body shade value upon reaching adulthood
  adult.energy.supply              ; Record of energy supply value upon reaching adulthood
  adult.bite.capacity              ; Record of bite capacity value upon reaching adulthood
  adult.mutation.chance            ; Record of mutation chance value upon reaching adulthood
  adult.sex.ratio                  ; Record of sex ratio value upon reaching adulthood
  adult.litter.size                ; Record of litter size value upon reaching adulthood
  adult.conception.chance          ; Record of conception chance value upon reaching adulthood
  adult.visual.angle               ; Record of visual angle value upon reaching adulthood
  adult.visual.range               ; Record of visual range value upon reaching adulthood
  adult.day.perception             ; Record of day perception value upon reaching adulthood
  adult.night.perception           ; Record of night perception value upon reaching adulthood
  adult.yellow.chance              ; Record of yellow chance value upon reaching adulthood
  adult.red.chance                 ; Record of red chance value upon reaching adulthood
  adult.blue.chance                ; Record of blue chance value upon reaching adulthood
  distance.traveled                ; Total amount of spatial distance traveled throughout lifetime
  mother.initiated.birth           ; Record of whether or not mother initiated its birth
  mother.initiated.weaning         ; Record of whether or not mother initiated the weaning process
  whole.related.help.cost          ; Total amount of energy spent to help whole related individuals
  half.related.help.cost           ; Total amount of energy spent to help half related individuals
  fourth.related.help.cost         ; Total amount of energy spent to help fourth related individuals
  eighth.related.help.cost         ; Total amount of energy spent to help eighth related individuals
  not.related.help.cost            ; Total amount of energy spent to help non-related individuals
  whole.related.attack.cost        ; Total amount of energy spent to attack whole related individuals
  half.related.attack.cost         ; Total amount of energy spent to attack half related individuals
  fourth.related.attack.cost       ; Total amount of energy spent to attack fourth related individuals
  eighth.related.attack.cost       ; Total amount of energy spent to attack eighth related individuals
  not.related.attack.cost          ; Total amount of energy spent to attack non-related individuals
  foraging.gains                   ; Total amount of energy gained through foraging
  total.energy.gains               ; Total amount of energy gained during lifetime
  total.energy.cost                ; Total amount of energy spent during lifetime
  foraging.gains.this.timestep     ; Amount of energy gained from foraging in current timestep
  energy.gains.this.timestep       ; Amount of energy gained in current timestep
  energy.cost.this.timestep        ; Amount of energy spent in current timestep
  help.from.history                ; Record of individuals who have helped this anima1
  attack.from.history              ; Record of individuals who have attacked this anima1
  copulations.history              ; Record of individuals who have mated with this anima1
  conceptions.history              ; Record of individuals who conceived with this anima1
  group.transfers.history          ; Record of groups to which this anima1 has joined
  infanticide.history              ; Record of infants who have been killed by this anima1
  cause.of.death                   ; A description of the most likely reason why the individual died

  from.ingroup.male.attack.count
  from.nongroup.male.attack.count
  from.ingroup.female.attack.count
  from.nongroup.female.attack.count

  from.ingroup.male.attack.energy.spent
  from.nongroup.male.attack.energy.spent
  from.ingroup.female.attack.energy.spent
  from.nongroup.female.attack.energy.spent

  to.ingroup.male.attack.count
  to.nongroup.male.attack.count
  to.ingroup.female.attack.count
  to.nongroup.female.attack.count

  to.ingroup.male.attack.energy.spent
  to.nongroup.male.attack.energy.spent
  to.ingroup.female.attack.energy.spent
  to.nongroup.female.attack.energy.spent

  from.ingroup.male.help.count
  from.nongroup.male.help.count
  from.ingroup.female.help.count
  from.nongroup.female.help.count

  from.ingroup.male.help.energy.spent
  from.nongroup.male.help.energy.spent
  from.ingroup.female.help.energy.spent
  from.nongroup.female.help.energy.spent

  to.ingroup.male.help.count
  to.nongroup.male.help.count
  to.ingroup.female.help.count
  to.nongroup.female.help.count

  to.ingroup.male.help.energy.spent
  to.nongroup.male.help.energy.spent
  to.ingroup.female.help.energy.spent
  to.nongroup.female.help.energy.spent

  to.ingroup.male.juvenile.attack.count
  to.nongroup.male.juvenile.attack.count
  to.ingroup.female.juvenile.attack.count
  to.nongroup.female.juvenile.attack.count

  to.ingroup.male.juvenile.attack.energy.spent
  to.nongroup.male.juvenile.attack.energy.spent
  to.ingroup.female.juvenile.attack.energy.spent
  to.nongroup.female.juvenile.attack.energy.spent

  to.ingroup.male.infant.attack.count
  to.nongroup.male.infant.attack.count
  to.ingroup.female.infant.attack.count
  to.nongroup.female.infant.attack.count

  to.ingroup.male.infant.attack.energy.spent
  to.nongroup.male.infant.attack.energy.spent
  to.ingroup.female.infant.attack.energy.spent
  to.nongroup.female.infant.attack.energy.spent

  to.ingroup.male.juvenile.help.count
  to.nongroup.male.juvenile.help.count
  to.ingroup.female.juvenile.help.count
  to.nongroup.female.juvenile.help.count

  to.ingroup.male.juvenile.help.energy.spent
  to.nongroup.male.juvenile.help.energy.spent
  to.ingroup.female.juvenile.help.energy.spent
  to.nongroup.female.juvenile.help.energy.spent

  to.ingroup.male.infant.help.count
  to.nongroup.male.infant.help.count
  to.ingroup.female.infant.help.count
  to.nongroup.female.infant.help.count

  to.ingroup.male.infant.help.energy.spent
  to.nongroup.male.infant.help.energy.spent
  to.ingroup.female.infant.help.energy.spent
  to.nongroup.female.infant.help.energy.spent

  timesteps.cycling.with.red.signal
  timesteps.cycling.without.red.signal
  timesteps.pregnant.with.red.signal
  timesteps.pregnant.without.red.signal
  timesteps.lactating.with.red.signal
  timesteps.lactating.without.red.signal
  timesteps.noncycling.with.red.signal
  timesteps.noncycling.without.red.signal

]

; --------------------------------------------------------------------------------------------------------- ;
;
; CHARACTERISTICS OF PLANTS
;
; Plants are part of the "patch" structure of the NetLogo system and each patch represents a single plant.
;
; --------------------------------------------------------------------------------------------------------- ;

patches-own [
  pmy.identity                     ; Unique identification number of a plant
  pterminal.energy                 ; The maximum energy that the plant can have
  penergy.supply                   ; The current energy level of the plant
  pgroup.identity                  ; The current group affiliation of the plant
  pgroups.here ]                   ; A record of the most recent groups to visit the plant

; --------------------------------------------------------------------------------------------------------- ;
;
; GLOBAL VARIABLES
;
; These variables can be accessed by anywhere in the program and include some program and simulation
; information, user settings, and information stored during a simulation.
;
; 'model-structure' defines the overall conditions for the simulation.
;  - 'no-plants' means plant growth is inhibited.
;  - 'reaper' means individuals are randomly selected to die when the total number of individuals across
;     the entire spatial structure exceeds 'n' individuals, where 'n' is minimum-population-size.
;  - 'stork' means individuals are randomly selected to reproduce until the total number of individuals
;     across the entire spatial structure exceeds 'n' individuals, where 'n' is maximum-population-size.
;  - 'aspatial' means that interactions among individuals are global, not restricted to neighboring spatial
;     patches.
;  - 'free-lunch' means that individuals may perform behaviors even if depleted of energy.
;  - 'no-evolution' means the evolutionary mechanisms are disabled.
;  - 'ideal-form' means simulated individuals conceived in the model acquire traits representing the average
;     of the entire population at the time.
;  - 'uninvadable' means evolution occurs with recombination and other simulated genetic mechanisms but
;     without mutation.
;
; --------------------------------------------------------------------------------------------------------- ;

globals [

  ; --------------------------------------------------------------------------------------------------------
  ; SIMULATION INFORMATION
  ; --------------------------------------------------------------------------------------------------------

  model-name
  model-version                    ; Current version of B3GET
  model-structure                  ; User setting for including specialized subroutines
  genotype-reader
  simulation-id                    ; Randomly generated simulation identity
  simulation-stop-at               ; User setting to define when to end the simulation

  ; --------------------------------------------------------------------------------------------------------
  ; ENVIRONMENT SETTINGS
  ; --------------------------------------------------------------------------------------------------------

  deterioration-rate               ; Rate at which anima1s experience a decrease in survival chance
  maximum-visual-range             ; Maximum possible range for an anima1's cone of perception
  base-litter-size                 ; Maximum possible litter size of an anima1
  maximum-population-size          ; The maximum population size when the "reaper" code is activated.
  minimum-population-size          ; The minimum population size when the "stork" code is activated.

  solar-status                     ; Whether it is currently day or night in the simulation
  current-season                   ; Current time of year in the simulation

  ; --------------------------------------------------------------------------------------------------------
  ; SIMULATION RESULTS
  ; --------------------------------------------------------------------------------------------------------

  selection-on?
  output-results?
  timestep-interval                ; Period between general simulation records
  simulation-summary-ticks         ; Timestep when summary information about the simulation is recorded
  simulation-scan-ticks            ; Period between scans of the current simulation
  group-scan-ticks                 ; Period between scans of all groups in the current simulation
  individual-scan-ticks            ; Period between scans of all living individuals in current simulation
  view-scan-ticks                  ; Period between scans of the simulation view display
  genotype-scan-ticks              ; Period between scans of the population average genotype
  focal-follow-rate                ; Rate at which an individual is chosen to follow
  record-individuals               ; When true, records information on all anima1s who die
  verification-rate                ; Rate at which the simulation is assessed
  record-world-ticks               ; Timestep when the NetLogo world is exported

  start-date-and-time              ; The date and time when the simulation was first setup
  plant-abundance-record           ; A periodic record of the plant abundance in the simulation
  plant-patchiness-record          ; A periodic record of the plant patchiness in the simulation
  population-size-record           ; A periodic record of the population size in the simulation
  decisions-made-this-timestep     ; Complete list of decisions made by all anima1s this timestep
  actions-completed-this-timestep  ; Complete list of actions completed by all anima1s this timestep
  verification-results             ; Record of verification assessments during this simulation

]

; ========================================================================================================= ;
;
;   .d88888b   88888888b d888888P dP     dP  888888ba
;   88.    "'  88           88    88     88  88    `8b
;   `Y88888b. a88aaaa       88    88     88 a88aaaa8P'
;         `8b  88           88    88     88  88
;   d8'   .8P  88           88    Y8.   .8P  88
;    Y88888P   88888888P    dP    `Y88888P'  dP
;
; ========================================================================================================= ;

; --------------------------------------------------------------------------------------------------------- ;
;
; SETUP THE ENVIRONMENT OF A NEW SIMULATION
;
; This is the main entry of the program and must be called before any other subroutine can proceed. This
; subroutine can also be called when a simulation is currently running, which will delete the current
; simulation and set up a new simulation. When this occurs, if at least 50,000 timesteps have transpired,
; the current state of the simulation is saved before it is deleted. Once the simulation environment is
; deleted, a new simulation is given a randomly generated identity. This subroutine relies on additional
; operations to set initial parameters, to initialize plants, and to import a starting population and
; possibly genotype. Once these processes are complete, the new simulation state is displayed. Subsequently,
; to run the simulation, the subroutine 'go' is called.
;
; ENTRY: This subroutine is the main entry, called whenever the user presses the setup button on the screen
;        for interactive operations, or when invoked by the behavior-space commands in non-interactive
;        operations.
;
; EXIT: setup-parameters
;
;       setup-patches
;
;       import-population
;
;       import-genotype
;
; --------------------------------------------------------------------------------------------------------- ;

to setup
  if ( simulation-id != 0 and                                ; If a simulation is currently running
    behaviorspace-run-number = 0 and                         ; and at least 50000 timesteps have transpired
    ticks > 50000 and                                        ; and user is currently recording data then
    output-results? = true ) [ record-world ]                ; completely record the current simulation
                                                             ; state.

  clear-all                                                  ; Delete all current settings in the simulation.
  reset-ticks                                                ; Reset the timesteps to zero.
  if ( simulation-id = 0 )
  [ set simulation-id generate-simulation-id  ]              ; Generate a new simulation identification.

  setup-parameters                                           ; Setup the global parameter settings.
  setup-patches                                              ; Initialize the plants for a new simulation.
  import-population                                          ; Create an initial population of indiviudals
  import-genotype                                            ; and their genotypes from user files.
  output-print (word                                         ; Once setup is complete, display the current
    " Simulation " simulation-id " "                         ; state of the new simulation.
    " was setup at " date-and-time )
end

; --------------------------------------------------------------------------------------------------------- ;
;
; SETUP GLOBAL PARAMETERS AT THE START OF A NEW SIMULATION
;
; This subroutine sets or initializes the global variables of the program. Many variables have default
; settings, which are set unless these variables have been set elsewhere.
;
; ENTRY:  The setup process for a new simulation has started.
;
; EXIT:   Global variables have been set or initialized.
;
; --------------------------------------------------------------------------------------------------------- ;

to setup-parameters
  set model-name "B3GET"
  set model-version "1.3.0.1"                                  ; Model version of B3GET.
  if ( model-structure = 0 or model-structure = [] ) [
    set model-structure [ "baseline" ] ]                       ; A simulation has a baseline model structure.
  set start-date-and-time date-and-time                        ; by default.

  if ( path-to-experiment = "" )                               ; By default, the path-to-experiment is
  [ set path-to-experiment "../results/" ]                     ; set to point to the results folder.

  if ( starting-seed = 0 )                                      ; By default, use new-seed to generate a
  [ set starting-seed new-seed ]                                ; random-seed if one is not provided.
  random-seed starting-seed                                     ; Then set random-seed to this seed.
  record-starting-seed

  if ( timestep-interval = 0)
  [ set timestep-interval 100 ]                                ; Default period between records.

  if ( deterioration-rate = 0 )
  [ set deterioration-rate -0.01 ]                             ; Default deterioration rate.

  if ( maximum-visual-range = 0 )                              ; This value must be set to a nonzero positive
  [ set maximum-visual-range 5 ]                               ; integer in order for an anima1 to see.

  if ( base-litter-size = 0 )                                  ; This value must be set to a nonzero positive
  [ set base-litter-size 10 ]                                  ; integer in order for conception to occur.

  if ( maximum-population-size = 0 )                           ; Only used for "reaper" code
  [ set maximum-population-size 150 ]

  if ( minimum-population-size = 0 )                           ; Only used for "stork" code
  [ set minimum-population-size 150 ]

  set verification-results []                                  ; These global variables are lists that need
  set plant-abundance-record []                                ; to be initialized here so that they can be
  set plant-patchiness-record []                               ; referenced in the program.
  set population-size-record []
  set actions-completed-this-timestep []

  if ( simulation-summary-ticks = 0 )                          ; Initialize the simulation with default
  [ set simulation-summary-ticks 25000 ]                       ; values for results output settings.

  if ( simulation-scan-ticks = 0 )
  [ set simulation-scan-ticks 250 ]

  if ( group-scan-ticks = 0 )
  [ set group-scan-ticks 5000 ]

  if ( individual-scan-ticks = 0 )
  [ set individual-scan-ticks 0 ]

  if ( view-scan-ticks = 0 )
  [ set view-scan-ticks 5000 ]

  if ( genotype-scan-ticks = 0 )
  [ set genotype-scan-ticks 5000 ]

  if ( focal-follow-rate = 0 )
  [ set focal-follow-rate 0.00001 ]

  if ( record-individuals = 0 )
  [ set record-individuals true ]

  if ( verification-rate = 0 )
  [ set verification-rate 1E-6 ]

  if ( record-world-ticks = 0 )
  [ set record-world-ticks 25000 ]

  if ( simulation-stop-at = 0 )
  [ set simulation-stop-at 999999999 ]

  if ( selection-on? = 0 )
  [ set selection-on? false ]

  if ( output-results? = 0 )
  [ set output-results? false ]

  set genotype-reader "sta2us"

  reset-timer                                                  ; Start the simulation timer
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CREATE A UNIQUE IDENTITY
;
; These subroutines return the appropriate codename for the designated item type: simulation, population
; or genotype. These codes are used to assign unique names to these items.
;
; ENTRY: NA
;
; EXIT: generate-timestamp
;
; --------------------------------------------------------------------------------------------------------- ;

to-report generate-simulation-id
  report ( word "s" generate-timestamp )
end

to-report generate-genotype-id
  report ( word "g" generate-timestamp )
end

to-report generate-population-id
  report ( word "p" generate-timestamp )
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CREATE A NEW STRING IN HEXADECIMAL CODE
;
; This subroutine creates a hexadecimal code that is based on the current time. This subroutine is mainly
; used to create random but informative file names for the simulations, populations and genotypes.
;
; ENTRY: NA
;
; EXIT: NA
;
; --------------------------------------------------------------------------------------------------------- ;

to-report generate-timestamp

  let string-to-report ""
  let time-difference time:difference-between                  ; Calculate the number of seconds since
  (time:create "1970-01-01 00:00:00.0")                        ; January 1st 1970 at the stroke of midnight
  (time:create "")                                             ; on New Year's Eve.
  "seconds"

  set time-difference time-difference                          ; Create a little randomness to this time
  + random 1000 - random 1000                                  ; difference so that mutiple files may be
  let hex-list [ "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"       ; created withinthe same second and not usually
    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"        ; cause file write collisions.
    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" ]      ; Create a list of numbers and letters.

  while [ time-difference > 0 ] [                              ; Generate a hexadecimal code based on
    let unix-remainder floor remainder time-difference 36      ; the randomized time difference.
    set string-to-report ( word
      item unix-remainder hex-list
      string-to-report )
    set time-difference floor ( time-difference / 36 ) ]

  report string-to-report                                      ; Return this hexadecimal code.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; SETUP PLANTS AT THE START OF A NEW SIMULATION [CL]
;
; The plant environment must be set up at the start of each simulation. During this process, the NetLogo
; patches, or cells, include a few starting conditions. First, they are given a random amount of potential
; energy between 0 and the 'plant-quality' global variable, and have this much starting energy. They are
; given a random id number so that other individuals can uniquely identity them. They are also given initial
; records for the group whose territory is the patch, which is initially set to no group. And finally, the
; color of the patch is updated to reflect its current energy contents, with darker colors of green
; representing more energy contained within that plant.
;
; ENTRY: plant-quality
;
; EXIT:  plants are updated with starting conditions for a simulation environment
;
;        update-patch-color
;
; --------------------------------------------------------------------------------------------------------- ;

to setup-patches
  let initial-group-list []                                ; Set up a generic list that contains one
  repeat 100 [                                             ; hundered zeros.
    set initial-group-list lput 0 initial-group-list ]
  ask patches [                                            ; Update the settings for each patch.
    set pterminal.energy random-float plant-quality        ; Give each plant a random starting maximum
    set penergy.supply pterminal.energy                    ; energy capacity and set the energy to this amount.
    set pmy.identity random 9999999                        ; Plants get a randomly generated id number.
    set pgroups.here initial-group-list                    ; The record of groups to occupy space, initially
                                                           ; includes no groups.
    update-patch-color ]                                   ; Update plant color to match its amount of energy.
end

; ========================================================================================================= ;
;
;   .88888.   .88888.
;  d8'   `88 d8'   `8b
;  88        88     88
;  88   YP88 88     88
;  Y8.   .88 Y8.   .8P
;   `88888'   `8888P'
;
; ========================================================================================================= ;

; --------------------------------------------------------------------------------------------------------- ;
;
; MAIN SUBROUTINE CALLED ONCE EACH TIMESTEP
;
; This is the secondary entry to the program to start a simulation running, under conditions established by
; subroutine 'setup'. The present subroutine is invoked when the user presses 'go' in interactive operations
; or when it is called by the BehaviorSpace commands in non-interactive operations.
;
; Time in this program is measured in abstract years and divided into abstract days, then further divided into
; abstract hours. For decimal simplicity, there are 100 days per year and 10 hours per day in the default
; settings. These divisions are arbitrary and can be modified in the interface as parameters to the program.
; Physical space in this program is divided into fixed-size square patches (cells) arranged in a rectangular
; grid, as is standard in the NetLogo programming language.<https://en.wikipedia.org/wiki/NetLogo/>
;
; The program steps forward hour by hour, and this routine is called once per step. In each step, the program
; works across the spatial structure to determine the dynamics during the present time step. This program,
; therefore, essentially implements an Euler method of simulation across a spatial structure, making it, in
; the most basic case, essentially a partial-differential equation solver. Considerations such as the Courant
; condition must therefore be observed.
;
; In each timestep, the program updates global settings, plant settings and anima1 general settings. Then,
; anima1s look at their environment, make decisions about how to proceed, and then act on those decisions.
; The simulation then imposes artificial selection on the anima1 population if this setting is on. The
; simulation then generates data from the current simulation, depending on the settings. Periodically,
; information about the current state of the simulation is displayed.
;
; ENTRY: The 'setup' subroutine has been called to establish conditions for the simulation, including plants
;        with starting levels of energy and a population of anima1s with attributes that include starting
;        values.
;
;
; EXIT:  globals-update
;
;        plants-update
;
;        anima1s-update
;
;        consider-environment
;
;        make-decisions
;
;        do-actions
;
;        artificial-selection
;
;        output-results
;
;        display-simulation-status
;
; --------------------------------------------------------------------------------------------------------- ;

to profiler-setup
  protopan-setup
  profiler:start
end

to go
  let to-go-entered timer
  ;output-print ( word "tick " ticks " to go entered " to-go-entered )
  if ( ticks = 0 ) [                                         ; Update metafile when a
    update-metafile                                          ; new simulation starts.
    "simulation"
    simulation-id
    "SIMULATION STARTED" ]

  if ( ticks = simulation-stop-at + 1 ) [             ; Stop at the predetermined timestep. 
  let _fname "profiler.txt"
  carefully [file-delete _fname] []
  file-open _fname
  file-print profiler:report
  file-close
  stop
  ]

  global-update                                              ; Update the current state of the
  let global-update-done timer
  plants-update                                              ; plants and animals in the
  let plants-update-done timer
  animals-update                                             ; simulated world.
  let animals-update-done timer

  let anima1s-alive anima1s with [ is.alive ]

  ask anima1s-alive [ consider-environment ]     ; Allow living animals to view their environment,
  let consider-environment-done timer
  ask anima1s-alive [ make-decisions ]           ; make decisions according to their genotype, and
  let make-decisions-done timer
  ask anima1s-alive [ do-actions ]               ; perform those actions if they have enough energy.
  let do-actions-done timer

  if selection-on? [ artificial-selection ]                  ; Impose artificial selection on animals.
  if output-results? [ output-results ]                      ; Generate data from current simulation state.
  display-simulation-status                                  ; Display simulation status.
  let end-of-go timer

  tick                                                       ; Move forward one simulated hour.

  let number-alive count anima1s-alive
  let number-total count anima1s

  update-timings simulation-id number-total number-alive to-go-entered global-update-done plants-update-done animals-update-done consider-environment-done make-decisions-done do-actions-done end-of-go
end

; --------------------------------------------------------------------------------------------------------- ;
;
; DISPLAY THE CURRENT SIMULATION STATUS FOR USER
;
; This subroutine periodically displays the current status of the simulation. Typically it is called at
; least once per timestep, and it watches for transitions to the next period, the length of which is
; defined in an external variable.
;
; ENTRY:  The simulation is currently running with living anima1s present.
;
;         The 'timestep-interval' global setting is set to the number of ticks between displays. By default
;         this is set to 100.
;
;         'ticks' is the number of current time steps. On the first call, its value is 0.
;
; EXIT:   The simulation status is displayed. In particular, the number of simulated years thus far, the
;         number of units of plant energy, the number of animals living, and the number of generations
;         that have transpired are displayed.
;
; --------------------------------------------------------------------------------------------------------- ;

to display-simulation-status
  if ( ticks-on-interval? timestep-interval ) [                ; Determine if periodic condition is met.

    print ( word                                               ; Diplay the following information:
      "Simulation " simulation-id                              ; Current simulation identification.
      " is now at "
      precision (ticks / plant-annual-cycle) 3                 ; The number of simulated years thus far.
      " years, "
      precision sum [penergy.supply] of patches 3              ; The number of units of plant energy.
      " plant units, "
      precision mean ( sentence
        [generation.number] of anima1s with [ is.alive ]       ; The number of generations have transpired.
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
; ENTRY:  The input value 'period' is the number of ticks between intervals.
;
; EXIT:   This subroutine returns true if the current timestep is on the interval and false otherwise.
;
; --------------------------------------------------------------------------------------------------------- ;

to-report ticks-on-interval? [ period ]
  report remainder ticks period = 0
end

;--------------------------------------------------------------------------------------------------------- ;
;
;           dP          dP                dP
;           88          88                88
;  .d8888b. 88 .d8888b. 88d888b. .d8888b. 88
;  88'  `88 88 88'  `88 88'  `88 88'  `88 88
;  88.  .88 88 88.  .88 88.  .88 88.  .88 88
;  `8888P88 dP `88888P' 88Y8888' `88888P8 dP
;       .88
;   d8888P
;
;--------------------------------------------------------------------------------------------------------- ;

;--------------------------------------------------------------------------------------------------------- ;
;
; PERFORM ALL GLOBAL UPDATES TO THE CURRENT SIMULATION
;
; This subroutine updates all global settings for the current timestep. The program sets the current time of
; year and day, depending on the current timesteps and user settings. If the current timestep lands on the
; designated value, the program records the current values for plant abundance, plant patchiness, and anima1
; population size. The record-keeping variables for anima1 decisions and actions is cleared for this timestep.
; To ensure that simulations continue running, this subroutine catches and displays all errors instead of
; halting the simulation.
;
; ENTRY: current timesteps
;
;        plant-daily-cycle
;
;        plant-annual-cycle is defined above.
;
; EXIT: The variables solar-status and current-season, and tracking variables plant-abundance-record
;       plant-patchiness-record and population-size record
;
;      'decisions-made-this-timestep' and 'actions-completed-this-timestep' are updated
;       with the most recent events, up to a maximum of 'how-many-ticks?'
;
;--------------------------------------------------------------------------------------------------------- ;

to global-update

  carefully [

    set solar-status ifelse-value                              ; Identify whether it is day or night
    ( ( cos (( 360 / plant-daily-cycle ) * ticks)) > 0 )       ; based on the current timestep and
    [ "DAY" ]                                                  ; user setting for day length.
    [ "NIGHT" ]

    set current-season                                         ; Calculate the current season
    ( cos (( 360 / plant-annual-cycle ) * ticks ))             ; which oscillates sinusoidally with time.

    if ( ticks-on-interval? timestep-interval and
      any? anima1s with [is.alive] ) [

      set plant-abundance-record lput                          ; Periodically record the total amount
      sum [penergy.supply] of patches                          ; of energy available in all plants.
      plant-abundance-record

      set plant-patchiness-record lput                         ; Periodically record the current level
      plant-patchiness                                         ; of patchiness in plant distribution.
      plant-patchiness-record

      set population-size-record lput                          ; Periodically record the current number
      count anima1s with [ is.alive ]                          ; of living individuals.
      population-size-record
    ]

    set actions-completed-this-timestep []                     ; Reset the variables for decisions and
    set decisions-made-this-timestep []                        ; actions from the previous timestep.

  ] [ print ( word "GLOBAL UPDATE ERROR: " error-message ) ]   ; If error occurs, print out error message.

end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALCULATE A NEW VALUE BETWEEN ZERO AND ONE
;
; This subroutine takes in two values, the current value, which is assumed to be between 0 and 1, and the
; update value, which can be any number from negative infinity to positive infinity. If the update value
; is positive, then this subroutine returns a number that is closer to one than its current value, and the
; degree to which is approaches one is related to the magnitude of the update value. Likewise, if the update
; value is a negative number, this subroutine returns a number that is closer to zero than the current value.
;
; ENTRY:  current-value
;
;         update-value
;
; EXIT:   NA
;
; --------------------------------------------------------------------------------------------------------- ;

to-report get-updated-value [ current-value update-value ]
  let report-value ifelse-value ( current-value < 0.00001 )           ; Keep the current value within
  [ 0.00001 ]                                                         ; bounds so that it is neither too
  [ ifelse-value ( current-value > 0.99999 )                          ; close to zero or one.
    [ 0.99999 ]
    [ current-value ] ]
  ifelse update-value < 0                                             ; Apply a different calculation depending
  [ set report-value ( report-value ^ (1 + abs update-value) ) ]      ; on if the update value is positive or
  [ set report-value ( report-value ^ (1 / ( 1 + update-value) )) ]   ; negative and return this value.
  report report-value
end

; --------------------------------------------------------------------------------------------------------- ;
;
;           dP                     dP
;           88                     88
;  88d888b. 88 .d8888b. 88d888b. d8888P .d8888b.
;  88'  `88 88 88'  `88 88'  `88   88   Y8ooooo.
;  88.  .88 88 88.  .88 88    88   88         88
;  88Y888P' dP `88888P8 dP    dP   dP   `88888P'
;  88
;  dP
;
; --------------------------------------------------------------------------------------------------------- ;

; --------------------------------------------------------------------------------------------------------- ;
;
; UPDATE THE ATTRIBUTES OF ALL PLANTS FOR THIS TIMESTEP
;
; This subroutine is called once per time step to update all individual plants in the environment. Each
; plant has a maximum energy that it can accumulate at any moment, and that maximum energy varies with
; season and other parameters. In this model the present energy of a plant increases by a fixed amount each
; time step as part of plant growth, and decreases by plants being consumed by animals, and also can decrease
; by reductions in the maximum energy allowed. Plant growth does not occur if the model-structure includes
; "no-plants."
;
; ENTRY: model-structure is set to "no-plants"
;
;        'ticks' defines the time steps thus far (standard in NetLogo).
;
;        'plant-annual-cycle' defines the number of time steps in an abstract
;          year (typically 1000).
;
;        'penergy.supply' defines the amount of energy in each patch that is
;          available for food. This accumulates in each cell at the rate of
;          1 unit per abstract year, with an upper bound.
;
;        'pterminal.energy' is the upper bound.
;
; EXIT:  update-patches
;
;        update-terminal-energy
;
;         'season' defines the abstract season at the current value of 'tick'
;          on a cosine scale.
;
;         -1 to  0 represents spring (-90 degrees to   0 degrees)
;          0 to  1 represents summer (  0 degrees to  90 degrees)
;          1 to  0 represents fall   ( 90 degrees to 180 degrees)
;          0 to -1 represents winter (180 degrees to -90 degrees)
;
; --------------------------------------------------------------------------------------------------------- ;

to plants-update

  carefully [

    ifelse ( member? "no-plants" model-structure )             ; If the model-structure setting contains
    [ ask patches with [ pcolor != brown + 1 ]                 ; "no-plants" then for patches that are not
      [ set pcolor brown + 1 ]]                                ; currently brown set their color to brown.

    [ if ( solar-status = "DAY" ) [                            ; If it is currently daytime, perform the
        let season current-season                              ; following. First, calculate the current season
        let density ( sum [penergy.supply] of patches )        ; and the current population density.
        / ( count patches * plant-quality )

        ask patches [                                          ; Then for each plant, adjust its terminal energy
          update-terminal-energy season density                ; based on seasonal factors and local neighbors.

          set penergy.supply penergy.supply                    ; Each plant also grows in energy based on
          + plant-quality / plant-annual-cycle                 ; user set year length and max plant energy allowed.

          if penergy.supply > pterminal.energy                 ; The program makes sure to keep the amount
          [ set penergy.supply pterminal.energy ]]]            ; of energy it possesses lower or equal to its
                                                               ; terminal energy setting.

      ask patches [ update-patch-color ] ]                     ; Both day and night, update the visuals of plants.

  ] [ print ( word "PLANTS UPDATE ERROR: " error-message ) ]   ; If error occurs, print out error message.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; INCREASE OR DECREASE A PLANT'S TERMINAL ENERGY VALUE
;
; This subroutine is called once per timestep per plant. As plants grow, their energy content increases
; to a maximum, called their "terminal energy." This subroutine calculates the change in this terminal
; energy value based on both seasonal factors and local environmenal conditions. For example, summer
; usually results in an increased terminal energy and winter usually results in a decreased terminal energy.
; Additionally, depending on user settings, the plants may experience overcrowding from too many plant
; neighbors, which results in decreased terminal energy. Likewise, the terminal energy can decrease from
; undercrowding. However, if the plant is currently in optimal conditions with respect to its local
; plant neighbors, then its terminal energy may increase.
;
; ENTRY: 'plant-season' specifies the abstract season on a cosine scale, as
;          defined above.
;
;        'plant-density' specifies the average energy per plant, as a proportion
;          of the maximum plant quality.
;
;         plant-seasonality  plant-minimum-neighbors plant-maximum-neighbors
;         plant-daily-cycle  plant-quality plant-annual-cycle
;
;         information on the plants surrounding the current plant
;
; EXIT: the plant's terminal energy is adjusted up or down within bounds.
;
; --------------------------------------------------------------------------------------------------------- ;

to update-terminal-energy [ plant-season plant-density ]

  let seasonal-factor (                                        ; Calculate the degree of impact
    ( plant-seasonality * plant-season + 1 ) / 2 )             ; that the season has on the plants.

  let optimal-neighbor-energy (                                ; Calculate the optimal number of neighbors
    plant-minimum-neighbors + plant-maximum-neighbors ) / 2    ; that a plant should currently desire.

  let neighbor-energy-sd (                                     ; Calculate the range around the optimal
    optimal-neighbor-energy                                    ; desired neighbors that the plant will
    - plant-minimum-neighbors )                                ; also find satisfying conditions.

  let neighbor-energy (                                        ; Calculate the number of neighbors surrounding
    mean ( list                                                ; a plant based on their energy settings
      (sum [penergy.supply] of neighbors)                      ; with respect to the user specified amount of
      (sum [pterminal.energy] of neighbors) )                  ; energy allowed.
    / plant-quality )

  let probability-up ifelse-value                              ; Calculate the degree to which the plant
  ( neighbor-energy-sd = 0 ) [ 0 ] [                           ; finds itself in ideal neighbor conditions.
    ( 1 * e ^ (
      - (( neighbor-energy - optimal-neighbor-energy ) ^ 2 )
      / ( 2 * ( neighbor-energy-sd ^ 2 ) )) ) ]

  let y (                                                      ; Calculate the adjustment in
    ( plant-daily-cycle * plant-quality )                      ; the plant's maximum amount
    / plant-annual-cycle ) * (                                 ; of energy allowed based on the amount
    plant-density *                                            ; of neighbors and other general seasonal
    ( 2 * probability-up - 1 ) +                               ; factors using the above calculations.
    seasonal-factor -
    plant-density )

  set pterminal.energy ( pterminal.energy + random-float y )   ; Update the plants terminal energy setting,
  if pterminal.energy >= plant-quality                         ; which sets its mamimum allowed energy and
  [ set pterminal.energy plant-quality ]                       ; also keep this value within world bounds.
  if pterminal.energy <= 0.000 [ set pterminal.energy 0.000 ]

end

; --------------------------------------------------------------------------------------------------------- ;
;
; UPDATE COLOR OF A PLANT BASED ON ITS CURRENT ENERGY SUPPLY AND TERMINAL ENERGY
;
; This soubroutine is called by each plant once per timestep and updates the color of the plant, which can
; be viewed in the interface. A plant with a high amount of energy will appear darker than a plant with a
; low amount of energy.
;
; ENTRY:  pterminal.energy penergy.supply plant-quality ?
;
; EXIT:   The plant's color is updated
;
; --------------------------------------------------------------------------------------------------------- ;

to update-patch-color
  set pcolor scale-color green (( ( pterminal.energy + penergy.supply ) / 2 ) / plant-quality ) 1.5 -0.25
end

; --------------------------------------------------------------------------------------------------------- ;
;
;  oo                dP oo          oo       dP                   dP
;                    88                      88                   88
;  dP 88d888b. .d888b88 dP dP   .dP dP .d888b88 dP    dP .d8888b. 88 .d8888b.
;  88 88'  `88 88'  `88 88 88   d8' 88 88'  `88 88    88 88'  `88 88 Y8ooooo.
;  88 88    88 88.  .88 88 88 .88'  88 88.  .88 88.  .88 88.  .88 88       88
;  dP dP    dP `88888P8 dP 8888P'   dP `88888P8 `88888P' `88888P8 dP `88888P'
;
; --------------------------------------------------------------------------------------------------------- ;

; --------------------------------------------------------------------------------------------------------- ;
;
; PERFORM ALL GLOBAL UPDATES FOR ANIMA1S
;
; This subroutine handles all anima1 operations that are not handled by the main environment-decision-action
; operations. In other words, this subroutine includes all operations that the anima1s themselves do
; not pay energy for, but instead the environment imposes on the individuals. First, the environment
; imposes mortality on the anima1s such that indivudals who have a lower survival chance are more likely
; to die. The environment also imposes that each individual's survival chance dcreases every timestep.
;
; Other operations do not affect the anima1s directly but instead are related to tracking variables
; for user purposes only. These include variables related to instantaneous energy gains and losses,
; and the repositories anima1s use for viewing their environment and making decisions. This subroutine
; also handles the appearance of individuals in the simulation view.
;
; This subroutine is also associated with two model-structure settings: "reaper" and "stork"
; This organization ensures that 100 individuals stay alive at all times.... essentially ; random mating
; In particular, this subroutine handles death, and reporduction if model-structure includes "stork"
;
;
; ENTRY: The simulation is running and a population of anima1s exists
;
; EXIT: Anima1s updated attributes
;
;       It is possible that new anima1s were created if "stork"
;
; --------------------------------------------------------------------------------------------------------- ;

to animals-update

  carefully [

    ifelse ( member? "reaper" model-structure ) [                   ; If the current model structure includes
      if ( count anima1s with [ is.alive ] >                        ; "reaper" then randomly select individuals
        maximum-population-size ) [                                 ; to die until the population size is below
        ask n-of ( count anima1s with [ is.alive ] -                ; set number of indiviudals.
          maximum-population-size ) anima1s
        [ set-to-dead ]]
      ask anima1s with [ not fully.decayed and is.alive = false ]   ; Check individuals who have died if they
      [ check-mortality ] ]                                         ; should fully decay.

    [ ask anima1s with [ not fully.decayed ]                        ; When "reaper" is not included, the anima1s
      [ check-mortality ] ]                                         ; undergo regular death processes.


    if ( member? "stork" model-structure ) [                        ; If the current model structure includes
      if (count anima1s with [ is.alive ] <                         ; "stork" then randomly select individuals
        minimum-population-size ) [                                 ; to reproduce until the population size
        repeat ( minimum-population-size -                          ; is above a set number of individuals.
          count anima1s with [ is.alive ]) [
          if ( ( count anima1s with [
            biological.sex = "male" and
            life.history = "adult" and
            is.alive ] > 0 )                                        ; There must be at least one adult male
            and ( count anima1s with [                              ; and one adult cycling female left in order
              biological.sex = "female" and                         ; for conception to occur.
              life.history = "adult" and
              fertility.status = "cycling" and
              is.alive ] > 0 ) )
          [ ask one-of anima1s with [
            biological.sex = "female" and
            life.history = "adult" and
            fertility.status = "cycling" and
            is.alive ]
            [ conceive-with ( one-of anima1s with [
              biological.sex = "male" and
              life.history = "adult" and
              is.alive ] ) ]]]]]

    ask anima1s with [ not fully.decayed ] [

      (ifelse                                                       ; Record information on fertility and red signal

        ( fertility.status = "cycling" and red.signal = TRUE ) [ set timesteps.cycling.with.red.signal timesteps.cycling.with.red.signal + 1 ]
        ( fertility.status = "cycling" and red.signal = FALSE ) [ set timesteps.cycling.without.red.signal timesteps.cycling.without.red.signal + 1 ]
        ( fertility.status = "pregnant" and red.signal = TRUE ) [ set timesteps.pregnant.with.red.signal timesteps.pregnant.with.red.signal + 1 ]
        ( fertility.status = "pregnant" and red.signal = FALSE ) [ set timesteps.pregnant.without.red.signal timesteps.pregnant.without.red.signal + 1 ]
        ( fertility.status = "lactating" and red.signal = TRUE ) [ set timesteps.lactating.with.red.signal timesteps.lactating.with.red.signal + 1 ]
        ( fertility.status = "lactating" and red.signal = FALSE ) [ set timesteps.lactating.without.red.signal timesteps.lactating.without.red.signal + 1 ]
        ( fertility.status = "" or fertility.status = " " and red.signal = TRUE ) [ set timesteps.noncycling.with.red.signal timesteps.noncycling.with.red.signal + 1 ]
        ( fertility.status = "" or fertility.status = " " and red.signal = FALSE ) [ set timesteps.noncycling.without.red.signal timesteps.noncycling.without.red.signal + 1 ]
        [])

      set foraging.gains.this.timestep 0                            ; Some variables are cleared each timestep
      set energy.gains.this.timestep 0                              ; to be used again in the subsequent timestep.
      set energy.cost.this.timestep 0
      set my.environment []
      set decision.vectors []
      set actions.completed []                                      ; Clear memory from the previous timestep.
      set age.in.ticks age.in.ticks + 1                             ; Advance the individual's age by one timestep.

      deteriorate                                                   ; Advance the individual's decay for this time
      update-appearance                                             ; step. Update the visual appearance to match.

      if ( not empty? carried.items ) [                             ; If the carrier has moved, bring all the
        foreach carried.items [ itm ->                              ; carried items along.
          ask itm [ move-to myself ]]]

      ask patch-here [                                              ; Update territory ranges based on each
        set pgroups.here but-first pgroups.here                     ; anima1's present location.
        set pgroups.here lput
        [group.identity] of myself pgroups.here
        set pgroup.identity one-of modes pgroups.here ]

      set cause.of.death ""
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
;          Typically this is a number will above 0.99 to support survival across multiple time steps, but
;          can be any value from 0 to 1.
;
;         deterioration-rate | A global variable set by the user to drive the rate of decay in the environment.
;
; EXIT:   NA
;
; --------------------------------------------------------------------------------------------------------- ;

to deteriorate
  set survival.chance get-updated-value survival.chance deterioration-rate
end

; --------------------------------------------------------------------------------------------------------- ;
;
; DETERMINE WHETHER THE BODY HAS DETERIORATED TO THE POINT OF DEATH OR COMPLETE DECAY
;
; The caller determines whether they will survive to the next timestep. If the caller is still alive then
; the bodily decay process begins. If the decay process is already underway, the caller's body is now marked
; as fully decayed and is not longer present in the environment.
;
; ENTRY:  survival.chance
;
;         is.alive
;
; EXIT: set-to-dead
;
;       remove-from-environment
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
; caller's state is perminantly set to dead and it can no longer be seen by other individuals.
;
; ENTRY: is.alive group.identity, anima1s in group
;
; EXIT:  ticks.at.death death.group.identity
;
; --------------------------------------------------------------------------------------------------------- ;

to set-to-dead
  set is.alive false                                           ; Update the anima1 to be dead.
  set ticks.at.death ticks                                     ; Record information about the anima1
  set death.group.identity group.identity                      ; at the time of death, including the current
  set death.group.size ( count anima1s with [                  ; time, and information about their current
    is.alive and                                               ; group.
    group.identity = [group.identity] of myself ] )
  set label "x"                                                ; Visually display that anima1 has died.
  set my.environment []                                        ; Clear all active records.
  set decision.vectors []
  set actions.completed []
  ask anima1s with
  [ my.mother = [my.identity] of myself and is.alive ] [       ; Update probable future cause of death of
    set cause.of.death ( word "Mother has died." ) ]           ; a mother's offspring.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; UPDATE CALLER TO BE FULLY DECAYED
;
; An individual who dies is still available in the environment for living individuals to interact with. For
; exmaple, a living individual may want to eat a dead individual. However, eventually, a dead anima1's body
; is fully decayed such that another individual cannot interact with it. This subroutine handles this
; process to completely remove an anima1 from view in the environment. However, the anima1 still exists
; to be examined by the user.
;
; ENTRY: ticks.at.death carried.items of others and myself
;
; EXIT:  Anima1 is completely removed from view.
;
; --------------------------------------------------------------------------------------------------------- ;

to remove-from-environment
  if ( ticks.at.death = 0 ) [ set ticks.at.death ticks ]       ; Record when the anima1 died ("reaper" only).
  ask anima1s with [ member? myself carried.items ]
  [ set carried.items                                          ; Remove this anima1 from other
    remove myself remove-duplicates                            ; anima1's carried items.
    carried.items ]
  set carried.items []                                         ; Remove others from my carried items
  set hidden? true                                             ; Remove anima1 from view.
  set fully.decayed true                                       ; Update the anima1 to fully decayed
  set label " "                                                ; Remove the label that indicated it was dead.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; UPDATE VISUAL DISPLAY TO MATCH CURRENT ATTRIBUTES
;
; This subroutine updates the size and overall appearance of an anima1 at each timestep. During the course
; of an anima1's life, they can grow larger, change in coloration, and signals. This subroutine ensures that
; the appearance of the anima1 accurately reflects these changes.
;
; ENTRY: The anima1's body.size, group.identity, is.alive are known
;
; EXIT: The anima1's size and shape are updated.
;
; --------------------------------------------------------------------------------------------------------- ;

to update-appearance
  set size body.size                                           ; Update the size.
  set label ifelse-value
  ( is.alive ) [ " " ] [ "x" ]                                 ; If the anima1 has died, display an 'X'
  set color round (                                            ; Update the color.
    wrap-color group.identity + 5 - ( 10 ^ body.shade ))
  set shape get-shape                                          ; Udate the shape.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; DETERMINE THE CURRENT SHAPE OF CALLER
;
; This subroutine determines the appropriate shape to display depending on many factors of the current
; state of the caller. The base shape is either a triangle for males or a circle for females. The eye size and spacing
; is calculated from the anima1's visual attributes and depends on the current time of day. The shape may
; also include colored signals and other features ...
;
; ENTRY: The attributes of the anima1 accurately represent its current state: 'biological.sex,'
;        'visual.range,' 'visual.angle,' 'day.perception,' 'night.perception,' 'is.resting,' 'is.alive,'
;        'yellow.signal,' 'red.signal,' and 'blue.signal' are all defined above.
;
;        The current 'solar-status' is either DAY or NIGHT.
;
; EXIT:  'get-shape' returns a string representing the shape, as processed by NetLogo and as defined in the
;          Turtle Shapes Editor.  For example, "triangle123" defines a male with ...
;
; --------------------------------------------------------------------------------------------------------- ;

to-report get-shape
  let base_shape ifelse-value                                  ; Determine whether the base shape should be
  ( biological.sex = "male" ) [ "triangle" ] [ "circle" ]      ; triangle or circle depending on the sex.
  let eye_size ( ifelse-value

    ( visual.range < ( 1 / 3 ) ) [ "1" ]                       ; Determine the degree of eye size and spacing
    ( visual.range < ( 2 / 3 ) ) [ "2" ] [ "3" ] )             ; from anima1 visual attributes.
  let eye_spacing ( ifelse-value
    ( visual.angle < ( 1 / 3 ) ) [ "1" ]
    ( visual.angle < ( 2 / 3 ) ) [ "2" ] [ "3" ] )

  let current-perception ifelse-value                          ; Determine the pupil shape based on the
  ( solar-status = "DAY" )                                     ; time of day and the anima1's ability
  [ day.perception ] [ night.perception ]                      ; to see during that time.
  let eye_acuity ( ifelse-value
    ( is.resting or not is.alive ) [ "1" ]
    ( current-perception > 0.5 )   [ "3" ]
    ( current-perception > 0 )     [ "2" ] [ "1" ] )

  let a_on ifelse-value yellow.signal [ "a" ] [ "" ]           ; Determine whether the color
  let b_on ifelse-value red.signal    [ "b" ] [ "" ]           ; signals are on or off.
  let c_on ifelse-value blue.signal   [ "c" ] [ "" ]

  report ( word base_shape eye_size eye_spacing                ; Report the appropriate shape
    eye_acuity a_on b_on c_on )                                ; based on the above information.
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
  set energy.supply energy.supply + update                     ; Update caller's energy with input value,
  ifelse ( update > 0 )                                        ; which can be positive or negative.
  [ set total.energy.gains total.energy.gains + update
    set energy.gains.this.timestep
    energy.gains.this.timestep + update ]                      ; Record positive values as energy gains.
  [ set total.energy.cost total.energy.cost + abs update
    set energy.cost.this.timestep
    energy.cost.this.timestep + abs update ]                   ; Record negative values as energy costs.

  if ( energy.supply = 0 ) [ set cause.of.death ( word "Not enough energy." ) ]
end

; ========================================================================================================= ;
;
;   88888888b 888888ba   .88888.  dP 888888ba   88888888b
;   88        88    `8b d8'   `88 88 88    `8b  88
;  a88aaaa    88     88 88        88 88     88 a88aaaa
;   88        88     88 88   YP88 88 88     88  88
;   88        88     88 Y8.   .88 88 88     88  88
;   88888888P dP     dP  `88888'  dP dP     dP  88888888P
;
;
; This is the "engine" of the running simulation, where all living agents assess their environments and then
; make decisions based upon this assessment and also upon their genotypes. If they have enough energy they
; perform actions that correspond to the decisions that they made. The main subroutines used in each
; timestep for each individual are:
;
; ========================================================================================================= ;

; --------------------------------------------------------------------------------------------------------- ;
;
;                             oo                                                           dP
;                                                                                          88
;  .d8888b. 88d888b. dP   .dP dP 88d888b. .d8888b. 88d888b. 88d8b.d8b. .d8888b. 88d888b. d8888P
;  88ooood8 88'  `88 88   d8' 88 88'  `88 88'  `88 88'  `88 88'`88'`88 88ooood8 88'  `88   88
;  88.  ... 88    88 88 .88'  88 88       88.  .88 88    88 88  88  88 88.  ... 88    88   88
;  `88888P' dP    dP 8888P'   dP dP       `88888P' dP    dP dP  dP  dP `88888P' dP    dP   dP
;
; --------------------------------------------------------------------------------------------------------- ;

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
    [ day.perception ] [ night.perception ]                      ; visual attributes and on whether it is day
    set my.environment up-to-n-of                                ; or night. If current perceptive abilities are
    ceiling ( current-perception * length my.environment )       ; less than 100%, randomly remove some objects
    my.environment                                               ; from environment to correspond with limited
                                                                 ; perceptive abilities.

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

    set my.environment lput self my.environment                  ; Individual is also in current environment.
    set my.environment remove-duplicates my.environment          ; Duplicate objects are not considered.

  ] [ print ( word "CONSIDER ENVIRONMENT ERROR - " my.identity " : " error-message ) ] ; If error occurs
                                                                                       ; print out error message.
end

; --------------------------------------------------------------------------------------------------------- ;
;
;        dP                   oo          oo
;        88
;  .d888b88 .d8888b. .d8888b. dP .d8888b. dP .d8888b. 88d888b. .d8888b.
;  88'  `88 88ooood8 88'  `"" 88 Y8ooooo. 88 88'  `88 88'  `88 Y8ooooo.
;  88.  .88 88.  ... 88.  ... 88       88 88 88.  .88 88    88       88
;  `88888P8 `88888P' `88888P' dP `88888P' dP `88888P' dP    dP `88888P'
;
;
; --------------------------------------------------------------------------------------------------------- ;

; --------------------------------------------------------------------------------------------------------- ;
;
; GENERATE LIST OF CALLER DECISIONS FROM CURRENT ENVIRONMENT AND GENOTYPE
;
; The main purpose of this subroutine is to access the appropriate subroutines to translate a genotype into
; decision vectors, based on the current environment. There are two languages that genotypes can be written
; in: sta2us and gat3s. The appropriate subrotuines in B3GET vary according to which genotype language is
; used. The user can manually set which genotype reader to use by selecting the genotype-reader in the
; interface, otherwise B3GET will use the sta2us genotype reader by default.
;
; ENTRY: genotype-reader contains either "sta2us" or "gat3s", based on genotype language used.
;
;        my.environment, as described above, defines the current environment of the caller.
;
;
; EXIT: decision.vectors, as described above, includes all decisions that the caller made from its genotype and
;       environment.
;
;       decisions-made-this-timestep, as described above, defines the global record of decisions is updated with
;       caller's decisions.
;
; --------------------------------------------------------------------------------------------------------- ;

to make-decisions

  carefully [

    set decision.vectors ( ifelse-value                          ; Get decisions from selected genotype reader.
      ( genotype-reader = "sta2us" )                             ; If genotype reader is sta2us,
      [ sta2us-get-decisions my.environment ]                    ; get decisions based on a sta2us genotype.
      ( genotype-reader = "gat3s" )                              ; If gentoype reader is gat3s,
      [ gat3s-get-decisions my.environment ]                     ; get decisions based on a gat3s genotype.
      [ sta2us-get-decisions my.environment ] )                  ; If not specified, assume current genotype
                                                                 ; reader is sta2us.
    foreach decision.vectors [ d ->                              ; Add this individual's decision to the global

      set decisions-made-this-timestep lput d decisions-made-this-timestep ]        ; pool of decisions.

  ] [ print ( word "MAKE DECISIONS ERROR - " my.identity " : " error-message ) ]    ; If error occurs, print
                                                                                    ; out error message.
end

; --------------------------------------------------------------------------------------------------------- ;
;
;                      dP   oo
;                      88
;  .d8888b. .d8888b. d8888P dP .d8888b. 88d888b. .d8888b.
;  88'  `88 88'  `""   88   88 88'  `88 88'  `88 Y8ooooo.
;  88.  .88 88.  ...   88   88 88.  .88 88    88       88
;  `88888P8 `88888P'   dP   dP `88888P' dP    dP `88888P'
;
;
; --------------------------------------------------------------------------------------------------------- ;

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER PERFORMS EACH ACTION LISTED IN ITS DECISION VECTORS IF IT HAS SUFFICIENT ENERGY
;
; The caller performs each that action that corresponds to each decision in its current list of decision
; vectors. The caller will pay an energy cost to perform each action based on the value listed in the
; corresponding decision. However, the caller will not be able to perform the action if it does not have
; sufficient energy to do so. If the caller does have enough energy, this subroutine triggers the appropriate
; subroutine that corresponds to the action. For clarity, the names of each action exactly match the
; corresponding name for each subroutine. The caller is also prohibited from performing certain actions if
; they are currently resting.
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

    foreach decision.vectors [ v ->
      let vector v                                               ; For each decision vector
      let done item 4 vector                                     ; first determine if this decision has already
                                                                 ; been acted upon.

      if ( not done and                                          ; If the decision has not already been acted upon
        check-energy vector ) [                                  ; and the caller has sufficient energy to perform
                                                                 ; the action, then the action can be performed.

        let target item 1 vector                                 ; Identify the target of the decision.
        let action item 2 vector                                 ; Identify which action the caller has decided
        let cost item 3 vector                                   ; to take. Identify how much the action will cost.

        if ( is.resting = false )                                ; These actions can only be performed if the caller
                                                                 ; is not resting.

        [ ( ifelse                                               ; Call the subroutine that corresponds with
                                                                 ; the action name.
          action = "move-toward" [ move-toward target cost ]
          action = "move-away-from" [ move-toward target ( - cost ) ]
          action = "turn-right" [ turn-right cost ]
          action = "turn-left" [ turn-right ( - cost ) ]
          action = "go-forward" [ go-forward cost ]
          action = "set-heading" [ set-heading cost ]
          action = "set-heading-random" [ set-heading-random cost ]
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

        ( ifelse                                                 ; These actions can be performed resting
                                                                 ; or not resting.

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

  ] [ print ( word "DO ACTIONS ERROR - " my.identity " : " error-message ) ] ; If error occurs,
                                                                             ; print out error message.

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
  let abs-cost abs cost
  let passes-energy-check ifelse-value                         ; Confirm if caller's energy is sufficient.
  ( member? "free-lunch" model-structure )                     ; If B3GET is set to "free lunch"
  [ true ]                                                     ; then any amount of energy is sufficient.
  [ energy.supply > abs-cost and abs-cost > 0 ]                ; Otherwise, the energy is sufficient if
                                                               ; it is greater than the cost, and the cost
                                                               ; is greater than 0.
  if ( passes-energy-check ) [                                 ; If the energy is sufficient,
    update-energy ( - abs-cost )                               ; reduce the caller's energy supply by the cost.
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
; ENTRY:  target
;
;         action-name
;
;         decision.vectors
;
;         actions.completed
;
; EXIT:   Subroutine returns the cost that the target paid for all actions that were directed
;         at the caller
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

  set actions-completed-this-timestep                          ; Globally record this action item.
  lput completed-action
  actions-completed-this-timestep
end

; ========================================================================================================= ;
;
;   .d888888   a88888b. d888888P dP  .88888.  888888ba  .d88888b
;  d8'    88  d8'   `88    88    88 d8'   `8b 88    `8b 88.    "'
;  88aaaaa88a 88           88    88 88     88 88     88 `Y88888b.
;  88     88  88           88    88 88     88 88     88       `8b
;  88     88  Y8.   .88    88    88 Y8.   .8P 88     88 d8'   .8P
;  88     88   Y88888P'    dP    dP  `8888P'  dP     dP  Y88888P
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
; CALLER UPDATES HIDDEN STATUS
;
; This subroutine first updates the hidden chance attribute based on the cost. Then, if the caller
; is not a gestatee, it determines if it is hidden base on the probability set by hidden cost.
;
; ENTRY: cost
;
;       hidden chance
;
; EXIT: The callers hidden? variable is updated.
;
; --------------------------------------------------------------------------------------------------------- ;

to hide [ cost ]
  complete-action self "hide" cost                           ; Record that this action has started.
  set hidden.chance get-updated-value hidden.chance cost     ; Apply update procedure to hidden chance.
  if ( life.history != "gestatee" ) [                        ; Check that the caller is not a gestatee
    ifelse ( random-float 1.0 < hidden.chance ) [            ; and then calculate a probability of
      set hidden? true                                       ; being hidden.
      complete-action self "is-hidden" 0
    ][                                                       ; Update status to be hidden or not
      set hidden? false                                      ; hidden based on probability calculation.
      complete-action self "not-hidden" 0
  ]]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES RESTING STATUS
;
; This subroutine determines if the caller is resting based on the cost of the action. The caller is
; updated to hiding if the cost is a positive number and to not hiding if the cost is a negative
; number.
;
; ENTRY: cost
;
; EXIT: Update caller status for resting
;
; --------------------------------------------------------------------------------------------------------- ;

to rest [ cost ]
  complete-action self "rest" cost                          ; Record that this action has started.
  if ( cost > 0 ) [                                         ; Set the caller to resting if the cost is a
    set is.resting true                                     ; positive number.
    complete-action self "is-resting" 0 ]                   ; Record that this action has been completed.
  if ( cost < 0 ) [                                         ; Set the caller to not resting if the cost
    set is.resting false                                    ; is a negative number.
    complete-action self "not-resting" 0 ]                  ; Record that this action has been completed.
end

; --------------------------------------------------------------------------------------------------------- ;
;
;  oo            dP                                         dP   oo
;                88                                         88
;  dP 88d888b. d8888P 88d888b. .d8888b. .d8888b. .d8888b. d8888P dP .d8888b. 88d888b. .d8888b.
;  88 88'  `88   88   88'  `88 88'  `88 88'  `88 88'  `""   88   88 88'  `88 88'  `88 Y8ooooo.
;  88 88    88   88   88       88.  .88 88.  .88 88.  ...   88   88 88.  .88 88    88       88
;  dP dP    dP   dP   dP       `88888P8 `88888P8 `88888P'   dP   dP `88888P' dP    dP `88888P'
;
;
; --------------------------------------------------------------------------------------------------------- ;

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES SURVIVAL CHANCE [CL]
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
; CALLER UPDATES BODY SIZE
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
; CALLER UPDATES BODY SHADE
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

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES VISUAL RANGE
;
; ENTRY: cost | The amount of energy spent to update this attribute.
;
; EXIT: The visual.range value increases if cost is positive or decreases if cost is negative based
;       on the get-updated-value subroutine.
;
;       This subroutine also calls the complete-action subroutine to keep a record of this action.
;
; --------------------------------------------------------------------------------------------------------- ;

to visual-range [ cost ]
  complete-action self "visual-range" cost
  set visual.range get-updated-value visual.range cost
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES VISUAL ANGLE
;
; ENTRY: cost | The amount of energy spent to update this attribute.
;
; EXIT: The visual.angle value increases if cost is positive or decreases if cost is negative based
;       on the get-updated-value subroutine.
;
;       This subroutine also calls the complete-action subroutine to keep a record of this action.
;
; --------------------------------------------------------------------------------------------------------- ;

to visual-angle [ cost ]
  complete-action self "visual-angle" cost
  set visual.angle get-updated-value visual.angle cost
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES DAY PERCEPTION
;
; ENTRY: cost | The amount of energy spent to update this attribute.
;
; EXIT: The day.perception value increases if cost is positive or decreases if cost is negative based
;       on the get-updated-value subroutine.
;
;       This subroutine also calls the complete-action subroutine to keep a record of this action.
;
; --------------------------------------------------------------------------------------------------------- ;

to day-perception [ cost ]
  complete-action self "day-perception" cost
  set day.perception get-updated-value day.perception cost
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES NIGHT PERCEPTION
;
; ENTRY: cost | The amount of energy spent to update this attribute.
;
; EXIT: The night.perception value increases if cost is positive or decreases if cost is negative based
;       on the get-updated-value subroutine.
;
;       This subroutine also calls the complete-action subroutine to keep a record of this action.
;
; --------------------------------------------------------------------------------------------------------- ;

to night-perception [ cost ]
  complete-action self "night-perception" cost
  set night.perception get-updated-value night.perception cost
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES CONCEPTION CHANCE
;
; ENTRY: cost | The amount of energy spent to update this attribute.
;
; EXIT: The conception.chance value increases if cost is positive or decreases if cost is negative based
;       on the get-updated-value subroutine.
;
;       This subroutine also calls the complete-action subroutine to keep a record of this action.
;
; --------------------------------------------------------------------------------------------------------- ;

to conception-chance [ cost ]
  complete-action self "conception-chance" cost
  set conception.chance get-updated-value conception.chance cost
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES BITE CAPACITY
;
; ENTRY: cost | The amount of energy spent to update this attribute.
;
; EXIT: The bite.capacity value increases if cost is positive or decreases if cost is negative based
;       on the get-updated-value subroutine.
;
;       This subroutine also calls the complete-action subroutine to keep a record of this action.
;
; --------------------------------------------------------------------------------------------------------- ;

to bite-capacity [ cost ]
  complete-action self "bite-capacity" cost
  set bite.capacity get-updated-value bite.capacity cost
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES MUTATION CHANCE
;
; ENTRY: cost | The amount of energy spent to update this attribute.
;
; EXIT: The mutation.chance value increases if cost is positive or decreases if cost is negative based
;       on the get-updated-value subroutine.
;
;       This subroutine also calls the complete-action subroutine to keep a record of this action.
;
; --------------------------------------------------------------------------------------------------------- ;

to mutation-chance [ cost ]
  complete-action self "mutation-chance" cost
  set mutation.chance get-updated-value mutation.chance cost
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES SEX RATIO
;
; ENTRY: cost | The amount of energy spent to update this attribute.
;
; EXIT: The sex.ratio value increases if cost is positive or decreases if cost is negative based
;       on the get-updated-value subroutine.
;
;       This subroutine also calls the complete-action subroutine to keep a record of this action.
;
; --------------------------------------------------------------------------------------------------------- ;

to sex-ratio [ cost ]
  complete-action self "sex-ratio" cost
  set sex.ratio get-updated-value sex.ratio cost
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES LITTER SIZE
;
; ENTRY: cost | The amount of energy spent to update this attribute.
;
; EXIT: The litter.size value increases if cost is positive or decreases if cost is negative based
;       on the get-updated-value subroutine.
;
;       This subroutine also calls the complete-action subroutine to keep a record of this action.
;
; --------------------------------------------------------------------------------------------------------- ;

to litter-size [ cost ]
  complete-action self "litter-size" cost
  set litter.size get-updated-value litter.size cost
end

; --------------------------------------------------------------------------------------------------------- ;
;
;                                                                       dP
;                                                                       88
;  88d8b.d8b. .d8888b. dP   .dP .d8888b. 88d8b.d8b. .d8888b. 88d888b. d8888P
;  88'`88'`88 88'  `88 88   d8' 88ooood8 88'`88'`88 88ooood8 88'  `88   88
;  88  88  88 88.  .88 88 .88'  88.  ... 88  88  88 88.  ... 88    88   88
;  dP  dP  dP `88888P' 8888P'   `88888P' dP  dP  dP `88888P' dP    dP   dP
;
;
; --------------------------------------------------------------------------------------------------------- ;

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES HEADING WITH RESPECT TO TARGET
;
; This subroutine calculates a new heading for the caller with respect to the target. First, the position
; difference between the caller and target are calculated along the x and y axes. If this difference is
; non-zero, then a magnitude of difference if calculated based on the cost and the angle between the
; target and the caller. Finally, these magnitudes are used to determine the new heading.
;
; ENTRY: target
;
;        cost
;
;        Positions of the caller and target
;
; EXIT:  Caller magnitude preferrences and heading are updated
;
; --------------------------------------------------------------------------------------------------------- ;

to move-toward [ target cost ]
  complete-action target "move-toward" cost                    ; Record that this action has started.

  if (target != nobody ) [

    let ycor-difference ( ( ifelse-value                       ; Determine the distance from caller to
      ( is-patch? target )                                     ; target along cardinal directions.
      [ [pycor] of target ]
      [ [ycor] of target ] ) - [ycor] of self )
    let xcor-difference ( ( ifelse-value
      ( is-patch? target )
      [ [pxcor] of target ]
      [ [xcor] of target ] ) - [xcor] of self )

    if ( ycor-difference > maximum-visual-range and            ; Check if the anima1 is looking at target
      ycor-difference > 0 )                                    ; across one of the edges of the world
    [ set ycor-difference ycor-difference - 100 ]              ; so that the angles are calculated correctly.
    if ( xcor-difference > maximum-visual-range and
      xcor-difference > 0 )
    [ set xcor-difference xcor-difference - 100 ]
    if ( abs ycor-difference > maximum-visual-range and
      ycor-difference < 0 )
    [ set ycor-difference ycor-difference + 100 ]
    if ( abs xcor-difference > maximum-visual-range and
      xcor-difference < 0 )
    [ set xcor-difference xcor-difference + 100 ]

    if ( not ( ycor-difference = 0 and                         ; Make sure target isn't in
      xcor-difference = 0 ) ) [                                ; the exact same location as self.

      let angle atan xcor-difference ycor-difference           ; Calculate angle to the target.
      if ( cost < 0 ) [ set angle angle - 180 ]

      set x.magnitude x.magnitude + (abs cost * sin angle)     ; Set direction magnitudes based on energy cost.
      set y.magnitude y.magnitude + (abs cost * cos angle) ]

    set heading ifelse-value                                   ; Set current heading based on magnitudes.
    ( x.magnitude = 0 and y.magnitude = 0 )
    [ heading ]
    [ ( atan x.magnitude y.magnitude ) ]
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES HEADING BASED ON CURRENT HEADING
;
; This subroutine allows the caller wants to turn right or left by a specific amount. The cost
; of this subroutine is exponentially higher than most other actions. Thus, it is not expected to
; evolve in a population naturally. Instead, this subroutine is used for visual testing purposes.
;
; ENTRY: cost
;
; EXIT: right and left
;
; --------------------------------------------------------------------------------------------------------- ;

to turn-right [ cost ]
  complete-action self "turn" cost                             ; Record that this action has started.
  if ( cost > 0 ) [ right ( 360 * cost )                       ; Turn tight if cost is positive.
    complete-action self "turn-right" 0 ]                      ; Record that this action has been completed.
  if ( cost < 0 ) [ left ( 360 * abs cost )                    ; Turn left if cost is negative.
    complete-action self "turn-left" 0 ]                       ; Record that this action has been completed.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER MOVES FORWARD OR BACKWARD
;
; This subroutine allows the caller to move forward spatially. The amount that an individual can move
; forward is based on how much energy (cost) the caller put into this action, plus how large in size in
; the caller and any other objects that the caller is carrying. The higher the energy, the farther the
; caller will move forward. The higher the net size of the caller and the objects that it is carrying,
; the less distant the caller will move forward. If the caller pays a negative cost, then the caller moves
; backward instead of forward.
;
; ENTRY: cost
;
; EXIT: Caller moves forward or backward by the calculated amount.
;
;       Records for distance traveled are updated.
;
; --------------------------------------------------------------------------------------------------------- ;

to go-forward [ cost ]
  complete-action self "go-forward" cost                       ; Record that this action has started.
  if ( life.history != "gestatee" ) [                          ; Only non-gestatees can move forward.

    ifelse ( cost < 0 )                                        ; A negative cost means the caller
    [ right 180 complete-action self "went-backward" 0 ]       ; went backward rather than
    [ complete-action self "went-forward" 0 ]                  ; moved forward.
    let sum-weight size                                        ; Calculate the caller's weight plus the
    foreach carried.items [ object ->                          ; weight of all carried objects.
      set sum-weight sum-weight + [size] of object ]
    let travel-distance ifelse-value                           ; Calculate the distance traveled
    ( sum-weight > 0 )                                         ; based on the cost of this action
    [ (size * (sqrt (( 2 * abs cost ) / sum-weight )) ) ]      ; and total amount of weight that
    [ 0 ]                                                      ; must be moved.

    forward travel-distance                                    ; Move caller by the calculated distance.

    set x.magnitude 0                                          ; Reset the caller's preference for
    set y.magnitude 0                                          ; their heading.

    set distance.traveled distance.traveled + travel-distance  ; Track this travel event.

  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES HEADING TO SPECIFIED VALUE
;
; This subroutine allows the caller to orient to a specific heading. The amount that an individual rotates
; is based on how much energy (cost) the caller put into this action. Each 1/360th unit of energy pays for
; one degree of rotation. For example, if the caller pays 0.25 then they will rotate 90 degrees, and if the
; caller pays 1.0 then the caller will rotate 360 degrees.
;
; ENTRY: cost
;
; EXIT: Caller updates its heading according to the cost.
;
; --------------------------------------------------------------------------------------------------------- ;

to set-heading [ cost ]
  complete-action self "set-heading" cost                     ; Record that this action has started.
  set heading cost * 360
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES HEADING WITH A RANDOM DEVIATION
;
; This subroutine allows the caller to randomly re-orient themselves with respect to their present heading.
; The amount that an individual rotates is based on how much energy (cost) the caller put into this action.
;
; ENTRY: cost
;
;        Current heading and location are known.
;
; EXIT: Heading updated
;
; --------------------------------------------------------------------------------------------------------- ;

to set-heading-random [ cost ]
  complete-action self "set-heading-random" cost               ; Record that this action has started.
  let x-difference 2 * x.magnitude * 100000 * cost
  if ( x-difference = 0 )                                      ; Calculate a new preferred
  [ set x-difference 100000 * cost ]                           ; heading along the x axis.
  if ( x-difference > 1E10 )
  [ set x-difference 1E10 ]
  set x.magnitude x.magnitude +
  random-float x-difference - random-float x-difference
  let y-difference 2 * y.magnitude * 100000 * cost
  if ( y-difference = 0 ) [ set y-difference 100000 * cost ]   ; Calculate a new preferred heading
  if ( y-difference > 1E10 ) [ set y-difference 1E10 ]         ; along the y axis.
  set y.magnitude y.magnitude +
  random-float y-difference - random-float y-difference
  set heading ifelse-value
  ( x.magnitude = 0 and y.magnitude = 0 )
  [ heading ]                                                  ; Determine the new heading based
  [ ( atan x.magnitude y.magnitude ) ]                         ; on these calculations.
end

; --------------------------------------------------------------------------------------------------------- ;
;
;           oo                            dP oo
;                                         88
;  .d8888b. dP .d8888b. 88d888b. .d8888b. 88 dP 88d888b. .d8888b.
;  Y8ooooo. 88 88'  `88 88'  `88 88'  `88 88 88 88'  `88 88'  `88
;        88 88 88.  .88 88    88 88.  .88 88 88 88    88 88.  .88
;  `88888P' dP `8888P88 dP    dP `88888P8 dP dP dP    dP `8888P88
;                   .88                                       .88
;               d8888P                                    d8888P
;
; --------------------------------------------------------------------------------------------------------- ;

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES YELLOW SIGNAL STATUS
;
; This subroutine first updates the caller's yellow signal according to the cost. Then, it calculates
; the probability of the caller turning yellow, and updates the status of this caller's signal accordingly.
;
; ENTRY: cost
;
;        yellow.signal
;
; EXIT: Signal status is updated.
;
; --------------------------------------------------------------------------------------------------------- ;

to yellow-signal [ cost ]
  complete-action self "yellow-signal" cost                    ; Record that this action has started.
  set yellow.chance get-updated-value yellow.chance cost       ; Apply update procedure to yellow chance.
  ifelse ( random-float 1.0 < yellow.chance ) [                ; Check the probability of turning yellow
    set yellow.signal true                                     ; and do so if needed.
    complete-action self "yellow-signal-on" 0                  ; Record that this action has been completed.
  ][                                                           ;
    set yellow.signal false                                    ; Turn yellow signal off if needed.
    complete-action self "yellow-signal-off" 0                 ; Record that this action has been completed.
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES RED SIGNAL STATUS
;
; This subroutine first updates the caller's red signal according to the cost. Then, it calculates
; the probability of the caller turning red, and updates the status of this caller's signal accordingly.
;
; ENTRY: cost
;
;        red.signal
;
; EXIT: Signal status is updated.
;
; --------------------------------------------------------------------------------------------------------- ;

to red-signal [ cost ]
  complete-action self "red-signal" cost                       ; Record that this action has started.
  set red.chance get-updated-value red.chance cost             ; Apply update procedure to red chance.
  ifelse ( random-float 1.0 < red.chance ) [                   ; Check the probability of turning red
    set red.signal true                                        ; and do so if needed.
    complete-action self "red-signal-on" 0                     ; Record that this action has been completed.
  ][                                                           ;
    set red.signal false                                       ; Turn red signal off if needed.
    complete-action self "red-signal-off" 0                    ; Record that this action has been completed.
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER UPDATES BLUE SIGNAL STATUS
;
; This subroutine first updates the caller's blue signal according to the cost. Then, it calculates
; the probability of the caller turning blue, and updates the status of this caller's signal accordingly.
;
; ENTRY: cost
;
;        blue.signal
;
; EXIT: Signal status is updated.
;
; --------------------------------------------------------------------------------------------------------- ;

to blue-signal [ cost ]
  complete-action self "blue-signal" cost                      ; Record that this action has started.
  set blue.chance get-updated-value blue.chance cost           ; Apply update procedure to blue chance.
  ifelse ( random-float 1.0 < blue.chance ) [                  ; Check the probability of turning blue
    set blue.signal true                                       ; and do so if needed.
    complete-action self "blue-signal-on" 0                    ; Record that this action has been completed.
  ][                                                           ;
    set blue.signal false                                      ; Turn blue signal off if needed.
    complete-action self "blue-signal-off" 0                   ; Record that this action has been completed.
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
;        dP                            dP                                                  dP
;        88                            88                                                  88
;  .d888b88 .d8888b. dP   .dP .d8888b. 88 .d8888b. 88d888b. 88d8b.d8b. .d8888b. 88d888b. d8888P
;  88'  `88 88ooood8 88   d8' 88ooood8 88 88'  `88 88'  `88 88'`88'`88 88ooood8 88'  `88   88
;  88.  .88 88.  ... 88 .88'  88.  ... 88 88.  .88 88.  .88 88  88  88 88.  ... 88    88   88
;  `88888P8 `88888P' 8888P'   `88888P' dP `88888P' 88Y888P' dP  dP  dP `88888P' dP    dP   dP
;                                                  88
;                                                  dP
; --------------------------------------------------------------------------------------------------------- ;

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER DEVELOPS INTO INFANT
;
; This subroutine enables the caller to invest in their development towards becoming an infant. The degree to
; which an individual develops is based on how much energy (cost) the caller put into this action. If the
; caller is sufficiently developed, they become an infant.
;
; ENTRY: cost
;
;        infancy.chance
;
; EXIT: give-birth
;
; --------------------------------------------------------------------------------------------------------- ;

to check-infancy [ cost ]
  complete-action self "check-infancy" cost                    ; Record that this action has started.
  ifelse ( my.mother = nobody )                                ; Caller cannot survive if its mother is dead.
  [ set-to-dead ]
  [ set infancy.chance get-updated-value infancy.chance cost   ; Apply update procedure to infancy chance.
    complete-action self "update-infancy-chance" 0             ; Record that this action has been completed.
    if ( life.history = "gestatee" and                         ; Calculate the probability of becoming
      random-float 1.0 < infancy.chance ) [                    ; an infant and proceed if necessary.
      ask my.mother [ give-birth ]
    ]
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER DETERMINES IF THEY SHOULD GIVE BIRTH
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: exit 1...
;
; --------------------------------------------------------------------------------------------------------- ;

to check-birth [ cost ]
  complete-action self "check-birth" cost                      ; Record that this action has started.
  set birthing.chance get-updated-value birthing.chance cost   ; Apply update procedure to birthing chance.
  complete-action self "update-birthing-chance" 0              ; Record that this action has been completed.
  if ( fertility.status = "pregnant" and                       ; Calculate the probability of giving
    random-float 1.0 < birthing.chance ) [                     ; birth and proceed if necessary.
    ask my-offspring with [ life.history = "gestatee" ] [
      set mother.initiated.birth true ]
    give-birth
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER GIVES BIRTH
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: Caller updates status from pregnant to lactating.
;
; --------------------------------------------------------------------------------------------------------- ;

to give-birth
  complete-action self "give-birth" 0                          ; Record that this action has started.
  if ( fertility.status = "pregnant" ) [                       ; If the caller is pregnant,
    set fertility.status "lactating"                           ; update the caller to be lactating.
    ask my-offspring with [ life.history = "gestatee" ]        ; Trigger any current gestatees of the
    [ update-to-infant ]                                       ; caller to become infants.
    set birthing.chance 0                                      ; Reset pregnancy status.
    complete-action self "gave-birth" 0                        ; Record that this action has been completed.
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; GENERATE A LIST OF CALLER'S OFFSPRING
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: exit 1...
;
; --------------------------------------------------------------------------------------------------------- ;

to-report my-offspring
  report ifelse-value ( biological.sex = "female" )            ; Determine caller's offspring
  [ anima1s with [ my.mother = myself ]]                       ; by searching for individuals whose mother
  [ anima1s with [                                             ; or father is the caller.
    father.identity = [my.identity] of myself ]]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER BECOMES AN INFANT
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: exit 1...
;
; --------------------------------------------------------------------------------------------------------- ;

to update-to-infant
  complete-action self "update-to-infant" 0                    ; Record that this action has started.
  if ( is.alive ) [                                            ; If caller is alive,
    set life.history "infant"                                  ; update the caller to be an infant.
    set fertility.status " "                                   ; Caller is initialized with
    set hidden? false                                          ; infant settings and is no longer hidden
    set is.resting false                                       ; or resting.
    set ticks.at.birth ticks                                   ; Record when this development event occurred.
    set label "i"                                              ; Display that this individual became an infant.
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER DEVELOPS INTO JUVENILE
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: exit 1...
;
; --------------------------------------------------------------------------------------------------------- ;

to check-juvenility [ cost ]
  complete-action self "check-juvenility" cost                 ; Record that this action has started.
  set juvenility.chance
  get-updated-value juvenility.chance cost                     ; Apply update procedure to juvenility chance.
  complete-action self "update-juvenility-chance" 0            ; Record that this action has been completed.
  if ( life.history = "infant" and                             ; Calculate the probability of the caller
    random-float 1.0 < juvenility.chance ) [                   ; becoming a juvenile and proceed if
    ifelse ( my.mother = nobody )                              ; necessary.
    [ update-to-juvenile ]
    [ ask my.mother [ wean-offspring ]]
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER DETERMINES IF THEY SHOULD STOP LACTATING
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: exit 1...
;
; --------------------------------------------------------------------------------------------------------- ;

to check-weaning [ cost ]
  complete-action self "check-weaning" cost                    ; Record that this action has started.
  set weaning.chance get-updated-value weaning.chance cost     ; Apply update procedure to weaning chance.
  if ( fertility.status = "lactating" and
    random-float 1.0 < weaning.chance ) [                      ; Calculate the probability of the caller
    ask my-offspring with [ life.history = "infant" ] [        ; weaning her offspring and proceed if
      set mother.initiated.weaning true ]                      ; necessary.
    wean-offspring                                             ; Record that the mother initiated this process.
  ]
  complete-action self "update-weaning-chance" 0               ; Record that this action has been completed.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER WEANS OFFSPRING
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: exit 1...
;
; --------------------------------------------------------------------------------------------------------- ;

to wean-offspring
  complete-action self "wean-offspring" 0                      ; Record that this action has started.
  if ( fertility.status = "lactating" ) [                      ; If the caller is currently lactating,
    set fertility.status "cycling"                             ; update the caller's status to cycling.
    ask my-offspring with [ life.history = "infant" ] [        ; Trigger any current infants of the caller
      update-to-juvenile ]                                     ; to become juveniles.
    set weaning.chance 0                                       ; Reset lactating status.
    complete-action self "weaned-offspring" 0                  ; Record that this action has been completed.
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER BECOMES A JUVENILE
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: exit 1...
;
; --------------------------------------------------------------------------------------------------------- ;

to update-to-juvenile
  complete-action self "update-to-juvenile" 0                  ; Record that this action has started.
  if ( is.alive ) [                                            ; If the caller is alive,
    set life.history "juvenile"                                ; update the caller's status to juvenile.
    set fertility.status " "
    set label "j"                                              ; Display that the caller became a juvenile.
    set ticks.at.weaning ticks                                 ; Record when the caller made this
    let my-meta-id my.identity                                 ; transition and update the records of
    ask anima1s with                                           ; individuals who attacked the caller to
    [ member? my-meta-id infanticide.history ]                 ; indicate that they did not commit
    [ set infanticide.history remove my-meta-id                ; infanticide.
      remove-duplicates infanticide.history ]
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER DEVELOPS INTO ADULT
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: exit 1...
;
; --------------------------------------------------------------------------------------------------------- ;

to check-adulthood [ cost ]
  complete-action self "check-adulthood" cost                  ; Record that this action has started.
  set adulthood.chance
  get-updated-value adulthood.chance cost                      ; Apply update procedure to adulthood chance.
  if ( life.history = "juvenile" and                           ; Calculate the probability of the caller
    random-float 1.0 < adulthood.chance ) [                    ; becoming an adult and proceed if necessary.
    update-to-adult
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER BECOMES AN ADULT
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: exit 1...
;
; --------------------------------------------------------------------------------------------------------- ;

to update-to-adult
  complete-action self "update-to-adult" 0                     ; Record that this action has started.
  if ( is.alive ) [                                            ; If the caller is currently alive,
    set life.history "adult"                                   ; update the caller's status to adult.
    set fertility.status ifelse-value                          ; Update females to begin cycling.
    ( biological.sex = "male" ) [ " " ] [ "cycling" ]
    set label "a"                                              ; Display that the caller became an adult.
    set ticks.at.sexual.maturity ticks                         ; Record information about the current
    set adult.hidden.chance hidden.chance                      ; state of the caller upon reaching
    set adult.survival.chance survival.chance                  ; adulthood.
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
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
;
;  .d8888b. 88d888b. .d8888b. 88d888b. .d8888b. dP    dP
;  88ooood8 88'  `88 88ooood8 88'  `88 88'  `88 88    88
;  88.  ... 88    88 88.  ... 88       88.  .88 88.  .88
;  `88888P' dP    dP `88888P' dP       `8888P88 `8888P88
;                                           .88      .88
;                                       d8888P   d8888P
;
; --------------------------------------------------------------------------------------------------------- ;

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER INITIATES CONSUMPTION OF TARGET
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: exit 1...
;
; --------------------------------------------------------------------------------------------------------- ;

to eat [ target cost ]
  complete-action target "eat" cost                            ; Record that this action has started.
  if ( life.history = "juvenile" or                            ; Check that the caller and target are
    life.history = "adult" and                                 ; able to interact.
    ( is-patch? target or is-anima1? target )) [
    receive-from target cost                                   ; Caller receives energy from target.
  ]
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER TAKES ENERGY FROM TARGET
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: exit 1...
;
; --------------------------------------------------------------------------------------------------------- ;

to receive-from [ target cost ]
  complete-action target "receive-from" cost                   ; Record that this action has started.
  if ( cost > 0 and                                            ; Check that the target is able to interact.
    is-anima1? target or
    is-patch? target ) [

    let energy-wanted get-updated-value bite.capacity cost     ; Calculate how much energy that the caller
                                                               ; can receive.
    let energy-supply ifelse-value                             ; Determine how much energy the target has.
    ( is-patch? target )
    [ [ penergy.supply ] of target ]
    [ [ energy.supply ] of target ]

    let energy-received ifelse-value                           ; Determine the actual energy that caller
    ( energy-wanted < energy-supply )                          ; can receive based on the minimum value
    [ energy-wanted ]                                          ; from above energy values.
    [ energy-supply ]

    if ( energy-received > 0 ) [                               ; If the actual energy is a positive value:
      complete-action target "update-energy" 0                 ; Update the caller's energy and record
      update-energy energy-received                            ; that the transaction occurred.

      ifelse ( is-patch? target )                              ; Reduce the target's energy by the same
      [ ask target [                                           ; value.
        set penergy.supply penergy.supply -
        energy-received ]]
      [ ask target [ update-energy ( - energy-received ) ]]

      if ( life.history = "juvenile" or                        ; In cases where the target is foraging
        life.history = "adult" and                             ; on plants:
        is-patch? target ) [
        set foraging.gains.this.timestep                       ; Record data on this foraging interaction.
        foraging.gains.this.timestep + energy-received
        set foraging.gains
        foraging.gains + energy-received ]
  ]]
end

; --------------------------------------------------------------------------------------------------------- ;
;
;  oo            dP                                         dP   oo
;                88                                         88
;  dP 88d888b. d8888P .d8888b. 88d888b. .d8888b. .d8888b. d8888P dP .d8888b. 88d888b. .d8888b.
;  88 88'  `88   88   88ooood8 88'  `88 88'  `88 88'  `""   88   88 88'  `88 88'  `88 Y8ooooo.
;  88 88    88   88   88.  ... 88       88.  .88 88.  ...   88   88 88.  .88 88    88       88
;  dP dP    dP   dP   `88888P' dP       `88888P8 `88888P'   dP   dP `88888P' dP    dP `88888P'
;
; --------------------------------------------------------------------------------------------------------- ;


; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER PROVIDES ENERGY TO TARGET
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: exit 1...
;
; --------------------------------------------------------------------------------------------------------- ;

to supply-to [ target cost ]
  complete-action target "supply-to" cost                      ; Record that this action has started.
  if ( target != self and                                      ; Check that the target is able to interact.
    is-anima1? target and
    [ is.alive ] of target = true and                          ; Check that the caller is either pregnant
    ( fertility.status = "lactating"                           ; or lactating.
      or fertility.status = "pregnant" ) ) [

    let caller-cost [ get-action-cost-of
      myself "supply-to" ] of target
    let target-cost get-action-cost-of target "demand-from"    ; Determine the negotiation between caller
    let net-cost ( caller-cost + target-cost )                 ; and target.
    let probability ( caller-cost + target-cost )              ; The probability of caller supplying energy
    / ( abs caller-cost + abs target-cost + 0.0000000001 )     ; to target depends on the relative costs paid
    if ( random-float 1.0 <= probability )                     ; by caller and target. If the probability is
    [ ask target [ receive-from myself net-cost ] ]]           ; high enough, caller gives energy to target.

end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER REQUESTS ENERGY FROM TARGET
;
; This subroutine...
;
; ENTRY: entry 1...
;
; EXIT: exit 1...
;
; --------------------------------------------------------------------------------------------------------- ;

to demand-from [ target cost ]
  complete-action target "demand-from" cost                    ; Record that this action has started.
  if ( target != self and                                      ; Check that the target is able to interact.
    is-anima1? target and
    [ is.alive ] of target = true and                          ; Check that the caller is either an infant
    ( life.history = "gestatee" or                             ; or gestatee.
      life.history = "infant" ) ) [

    let caller-cost [ get-action-cost-of
      myself "demand-from" ] of target
    let target-cost                                            ; Determine the negotiation between caller
    get-action-cost-of target "supply-to"                      ; and target.
    let net-cost ( caller-cost + target-cost )
    let probability ( caller-cost + target-cost )              ; The probability of target supplying energy
    / ( abs caller-cost + abs target-cost + 0.0000000001 )     ; to caller depends on the relative costs paid
    if ( random-float 1.0 <= probability )                     ; by caller and target. If the probability is
    [ receive-from target net-cost ]]                          ; high enough, target gives energy to caller.

end

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
  complete-action target "join" cost                            ; Record that this action has started.
  if ( is-anima1? target and [ is.alive ] of target = true
    and [ group.identity ] of target != group.identity ) [      ; Check that the target is able to interact.
    let my-cost 0                                               ; Establish placeholder values to track caller
    let target-cost 0                                           ; and target decisions about caller joining
    ask anima1s with                                            ; the group. Identify all individuals
    [ group.identity = [group.identity] of target ] [           ; in the same group as the target.
      set my-cost my-cost + get-action-cost-of myself "join"    ; Record the caller's total energy cost to join
      set target-cost target-cost +                             ; group. Record any decisions that group members
      [ get-action-cost-of myself "recruit" ] of myself         ; made about caller joining the group.
    ]
    let probability ( my-cost + target-cost )
    / ( abs my-cost + abs target-cost + 0.0000000001 )          ; The probability of caller joining group depends
    if ( random-float 1.0 <= probability )                      ; on the relative costs paid by caller and group
    [ join-group ([group.identity] of target) ]                 ; members. If the probability is high enough,
  ]                                                             ; caller joins group.
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
  complete-action target "leave" cost                           ; Record that this action has started.
  if ( is-anima1? target and [ is.alive ] of target = true
    and [ group.identity ] of target = group.identity ) [       ; Check that the target is able to interact.
    let my-cost 0                                               ; Establish placeholder values to track caller
    let target-cost 0                                           ; and target decisions about caller leaving the
    ask anima1s with                                            ; group. Identify all individuals
    [ group.identity = [group.identity] of target ] [           ; in the same group as the target.
      set my-cost my-cost + get-action-cost-of myself "leave"   ; Record the caller's total energy cost to leave
      set target-cost target-cost +                             ; group. Record any decisions that group members
      [ get-action-cost-of myself "expel" ] of myself           ; made about caller leaving the group.
    ]
    let probability ( my-cost + target-cost )
    / ( abs my-cost + abs target-cost + 0.0000000001 )          ; The probability of caller joining group depends
    if ( random-float 1.0 <= probability                        ; on the relative costs paid by caller and group
      or ( probability = 0 and cost = 0 ))                      ; members.
    [ leave-group ]                                             ; If the probability is high enough, caller leaves
  ]                                                             ; group.
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
  complete-action target "recruit" cost                           ; Record that this action has started.
  if ( is-anima1? target and [ is.alive ] of target = true
    and [ group.identity ] of target != group.identity ) [        ; Check that the target is able to interact.
    let our-cost 0                                                ; Establish placeholder values to track group
    let target-cost 0                                             ; members and target decisions about recruiting
    ask anima1s with                                              ; the target. Identify all individuals
    [ group.identity = [group.identity] of myself ] [             ; in the same group as the caller.
      set our-cost our-cost +                                     ; Record any decisions that group members made
      [ get-action-cost-of myself "recruit" ] of target           ; about caller being recruited to the group.
      set target-cost target-cost +                               ; Record the target's total energy cost to join
      get-action-cost-of target "join"                            ; group.
    ]
    let probability ( our-cost + target-cost )
    / ( abs our-cost + abs target-cost + 0.0000000001 )           ; The probability of caller joining group depends
    if ( random-float 1.0 <= probability )                        ; on the relative costs paid by target and group
    [ ask target [ join-group [group.identity] of myself ]]       ; members. If the probability is high enough, target
  ]                                                               ; joins group.
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
  complete-action target "expel" cost                             ; Record that this action has started.
  if ( is-anima1? target and [ is.alive ] of target = true
    and [ group.identity ] of target = group.identity ) [         ; Check that the target is able to interact.
    let our-cost 0                                                ; Establish placeholder values to track group
    let target-cost 0                                             ; members and target decisions about expelling the
    ask anima1s with                                              ; target. Identify all individuals
    [ group.identity = [group.identity] of myself ] [             ; in the same group as the caller.
      set our-cost our-cost +                                     ; Record any decisions that group members made
      [ get-action-cost-of myself "expel" ] of target             ; about caller being expelled by the group.
      set target-cost target-cost +                               ; Record the target's total energy cost to leave
      get-action-cost-of target "leave"                           ; group.
    ]
    let probability ( our-cost + target-cost )
    / ( abs our-cost + abs target-cost + 0.0000000001 )           ; The probability of caller joining group depends
    if ( random-float 1.0 <= probability )                        ; on the relative costs paid by target and group
    [ ask target [ leave-group ]]                                 ; members. If the probability is high enough, target
  ]                                                               ; leaves group.
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
  complete-action self "join-group" 0                          ; Record that this action has started.
  ifelse ( group.identity != group-id ) [                      ; If the caller is not already parf of group:

    leave self 0                                               ; Determine if the caller leaves current group.
    ifelse ( solitary? ) [                                     ; If the caller has left its group

      set solitary? false
      set group.identity group-id                              ; Establish input value as the current group.
      set label "="                                            ; Visually indicate that caller has joined group.
      set group.transfers.history                              ; Caller keeps a record of transferring to group.
      lput group.identity group.transfers.history
      complete-action self "joined-group" 0  ]                 ; Record that the individual has now joined group.

    [ complete-action self "still-in-previous-group" 0 ]       ; Record that the individual remained in
                                                               ; previous group.
  ][
    complete-action self "already-in-group" 0                  ; Record if individual is already in this group.
  ]
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
  set label "~"                                                ; Visually indicate that the caller left old
  set solitary? true                                           ; group.
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
  complete-action target "pick-up" cost                        ; Record that this action has started.
  if ( is-anima1? target ) [                                   ; Check that the target is able to interact.
    let caller-cost [ get-action-cost-of myself "pick-up" ] of target
    let target-cost get-action-cost-of target "cling-to"
    let probability ( caller-cost + target-cost )              ; The probability of caller carrying target
    / ( abs caller-cost + abs target-cost + 0.0000000001 )     ; depends on the relative costs paid by caller
    if ( random-float 1.0 <= probability )                     ; and target. If the probability is high enough,
    [ carry target ]]                                          ; caller picks up target.
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
  complete-action target "put-down" cost                       ; Record that this action has started.
  if ( is-anima1? target ) [                                   ; Check that the target is able to interact.
    let caller-cost [ get-action-cost-of myself "put-down" ] of target
    let target-cost get-action-cost-of target "squirm-from"
    let probability ( caller-cost + target-cost )              ; The probability of caller dropping target
    / ( abs caller-cost + abs target-cost + 0.0000000001 )     ; depends on the relative costs paid by caller
    if ( random-float 1.0 <= probability                       ; and target. If the probability is high enough,
      or ( probability = 0 and cost = 0 ))                     ; caller puts down target.
    [ drop target ]]
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
  complete-action target "cling-to" cost                       ; Record that this action has started.
  if ( is-anima1? target ) [                                   ; Check that the target is able to interact.
    let caller-cost [ get-action-cost-of myself "cling-to" ] of target
    let target-cost get-action-cost-of target "pick-up"
    let probability ( caller-cost + target-cost )              ; The probability of caller clinging to target
    / ( abs caller-cost + abs target-cost + 0.0000000001 )     ; depends on the relative costs paid by caller
    if ( random-float 1.0 <= probability )                     ; and target. If the probability is high enough,
    [ ask target [ carry myself ] ]]                           ; target carries caller.
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
  complete-action target "squirm-from" cost                    ; Record that this action has started.
  if ( is-anima1? target ) [                                   ; Check that the target is able to interact.
    let caller-cost [ get-action-cost-of myself "squirm-from" ] of target
    let target-cost get-action-cost-of target "put-down"
    let probability ( caller-cost + target-cost )              ; The probability of caller squirming from to target
    / ( abs caller-cost + abs target-cost + 0.0000000001 )     ; depends on the relative costs paid by caller and
    if ( random-float 1.0 <= probability )                     ; target. If the probability is high enough, target
    [ ask target [ drop myself ] ]]                            ; drops caller.
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
  complete-action target "carry" 0                             ; Record that this action has started.
  if ( not member? target [carried.items] of anima1s ) [       ; Check that the target is able to interact.

    ask anima1s with [ member? target carried.items ] [        ; Prompt any individual currently carrying the
      put-down target 0 ]                                      ; target to put down the target.

    ifelse ( [ is-not-carried? ] of target ) [                 ; If the target is not being carried caller's
      set carried.items lput target carried.items              ; carried items updates to include target.
      ask target [ move-to myself ]                            ; The target's location is updated to match the
      set label "^"                                            ; caller. Visually indicate that caller has picked
      complete-action target "carry-complete" 0  ]             ; up target. Record that the individual has now
                                                               ; joined the group.
    [ complete-action target "still-carried-by-another" 0 ]    ; Record that the individual remained in previous
  ]                                                            ; group.
end

to-report is-not-carried? report not any? anima1s with [ member? myself carried.items ] end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER ATTEMPTS TO DROP TARGET
;
; This subroutine
;
; ENTRY: 'target' defines the individual that the caller is interacting with.
;
; EXIT: Target is not or no longer in the carried.items inventory of caller.
;
; --------------------------------------------------------------------------------------------------------- ;

to drop [ target ]
  complete-action target "drop" 0                              ; Record that this action has started.
  if ( member? target carried.items ) [                        ; Check that the target is able to interact.
    set carried.items remove target                            ; Target is removed from current inventory of
    remove-duplicates carried.items                            ; caller.
    set label "*"                                              ; Visually indicate that the caller has dropped target.
    complete-action target "drop-complete" 0                   ; Record that the target has been dropped by target.
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
  complete-action target "help" cost                           ; Record that this action has started.
  if ( is-anima1? target ) [                                   ; Check that the target is able to interact.
    let caller-cost [ get-action-cost-of                       ; Identify how much energy the caller and
      myself "help" ] of target                                ; target have paid for this help interaction.
    let target-cost get-action-cost-of target "help"
    let net-cost caller-cost + target-cost                     ; Identify the net amount of energy paid.

    if ( caller-cost > 0 ) [                                   ; If the caller paid a positive amount of
      aid target net-cost ]                                    ; energy then they aid target by the net energy.

    if ( target-cost > 0 ) [                                   ; If the target paid a positive amount of
      ask target [ aid myself net-cost ]]                      ; energy then they aid the caller by net energy.
  ]
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
  complete-action target "aid" 0                               ; Record that this action has started.
  if ( is-anima1? target and                                   ; Check that the target is able to interact
    [ is.alive ] of target = true and cost > 0) [              ; and if the cost is a positive value.

    ask target [                                               ; Increase the target's survival chance
      survival-chance cost                                     ; by the input cost.

      set help.from.history lput                               ; Record that the caller has aided target.
      [my.identity] of myself
      help.from.history ]

    set label "+"                                              ; Display the '+' symbol to indicate that caller
                                                               ; helped the target.

    let relatedness-with-target relatedness-with target        ; Record information on the degree of genetic
    if ( relatedness-with-target > 0.90 )                      ; relatedness between the caller and the target
    [ set whole.related.help.cost                              ; and how much energy went into this interaction.
      whole.related.help.cost + cost ]
    if ( relatedness-with-target <= 0.90 and
      relatedness-with-target > 0.40 )
    [ set half.related.help.cost
      half.related.help.cost + cost ]
    if ( relatedness-with-target <= 0.40 and
      relatedness-with-target > 0.15 )
    [ set fourth.related.help.cost
      fourth.related.help.cost + cost ]
    if ( relatedness-with-target <= 0.15 and
      relatedness-with-target > 0.05 )
    [ set eighth.related.help.cost
      eighth.related.help.cost + cost ]
    if ( relatedness-with-target <= 0.05 )
    [ set not.related.help.cost
      not.related.help.cost + cost ]

    if ( life.history = "adult" and [life.history] of target = "adult" and biological.sex = "male" and group.identity = [group.identity] of target ) [
      ask target [
        set from.ingroup.male.help.count from.ingroup.male.help.count + 1
        set from.ingroup.male.help.energy.spent from.ingroup.male.help.energy.spent + cost ]]
    if ( life.history = "adult" and [life.history] of target = "adult" and biological.sex = "male" and group.identity != [group.identity] of target ) [
      ask target [
        set from.nongroup.male.help.count from.nongroup.male.help.count + 1
        set from.nongroup.male.help.energy.spent from.nongroup.male.help.energy.spent + cost ]]
    if ( life.history = "adult" and [life.history] of target = "adult" and biological.sex = "female" and group.identity = [group.identity] of target ) [
      ask target [
        set from.ingroup.female.help.count from.ingroup.female.help.count + 1
        set from.ingroup.female.help.energy.spent from.ingroup.female.help.energy.spent + cost ]]
    if ( life.history = "adult" and [life.history] of target = "adult" and biological.sex = "female" and group.identity != [group.identity] of target ) [
      ask target [
        set from.nongroup.female.help.count from.nongroup.female.help.count + 1
        set from.nongroup.female.help.energy.spent from.nongroup.female.help.energy.spent + cost ]]

    if ( life.history = "adult" and [life.history] of target = "adult" and [biological.sex] of target = "male" and group.identity = [group.identity] of target ) [
      set to.ingroup.male.help.count to.ingroup.male.help.count + 1
      set to.ingroup.male.help.energy.spent to.ingroup.male.help.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "adult" and [biological.sex] of target = "male" and group.identity != [group.identity] of target ) [
      set to.nongroup.male.help.count to.nongroup.male.help.count + 1
      set to.nongroup.male.help.energy.spent to.nongroup.male.help.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "adult" and [biological.sex] of target = "female" and group.identity = [group.identity] of target ) [
      set to.ingroup.female.help.count to.ingroup.female.help.count + 1
      set to.ingroup.female.help.energy.spent to.ingroup.female.help.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "adult" and [biological.sex] of target = "female" and group.identity != [group.identity] of target ) [
      set to.nongroup.female.help.count to.nongroup.female.help.count + 1
      set to.nongroup.female.help.energy.spent to.nongroup.female.help.energy.spent + cost ]

    if ( life.history = "adult" and [life.history] of target = "juvenile" and [biological.sex] of target = "male" and group.identity = [group.identity] of target ) [
        set to.ingroup.male.juvenile.help.count to.ingroup.male.juvenile.help.count + 1
        set to.ingroup.male.juvenile.help.energy.spent to.ingroup.male.juvenile.help.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "juvenile" and [biological.sex] of target = "male" and group.identity != [group.identity] of target ) [
      set to.nongroup.male.juvenile.help.count to.nongroup.male.juvenile.help.count + 1
      set to.nongroup.male.juvenile.help.energy.spent to.nongroup.male.juvenile.help.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "juvenile" and [biological.sex] of target = "female" and group.identity = [group.identity] of target ) [
      set to.ingroup.female.juvenile.help.count to.ingroup.female.juvenile.help.count + 1
      set to.ingroup.female.juvenile.help.energy.spent to.ingroup.female.juvenile.help.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "juvenile" and [biological.sex] of target = "female" and group.identity != [group.identity] of target ) [
      set to.nongroup.female.juvenile.help.count to.nongroup.female.juvenile.help.count + 1
      set to.nongroup.female.juvenile.help.energy.spent to.nongroup.female.juvenile.help.energy.spent + cost ]

    if ( life.history = "adult" and [life.history] of target = "infant" and [biological.sex] of target = "male" and group.identity = [group.identity] of target ) [
      set to.ingroup.male.infant.help.count to.ingroup.male.infant.help.count + 1
      set to.ingroup.male.infant.help.energy.spent to.ingroup.male.infant.help.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "infant" and [biological.sex] of target = "male" and group.identity != [group.identity] of target ) [
      set to.nongroup.male.infant.help.count to.nongroup.male.infant.help.count + 1
      set to.nongroup.male.infant.help.energy.spent to.nongroup.male.infant.help.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "infant" and [biological.sex] of target = "female" and group.identity = [group.identity] of target ) [
      set to.ingroup.female.infant.help.count to.ingroup.female.infant.help.count + 1
      set to.ingroup.female.infant.help.energy.spent to.ingroup.female.infant.help.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "infant" and [biological.sex] of target = "female" and group.identity != [group.identity] of target ) [
      set to.nongroup.female.infant.help.count to.nongroup.female.infant.help.count + 1
      set to.nongroup.female.infant.help.energy.spent to.nongroup.female.infant.help.energy.spent + cost ]

    complete-action target "aid-complete" 0   ]                ; Record that this action has been completed.
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
  complete-action target "attack" cost                         ; Record that this action has started.
  if ( is-anima1? target ) [                                   ; Check that the target is able to interact.
    let caller-cost [ get-action-cost-of                       ; Identify how much energy the caller and
      myself "attack" ] of target                              ; target have paid for this help interaction.
    let target-cost get-action-cost-of target "attack"
    let net-cost caller-cost + target-cost                     ; Identify the net amount of energy paid.
    let size-probability ( size /                              ; Identify the relative size difference
      ( size + [size] of target + 0.0000000001 ) )             ; between the caller and target.

    if ( caller-cost > 0 ) [                                   ; If the caller paid a positive amount of
      if ( random-float 1.0 <= size-probability ) [            ; energy and can overpower the target
        harm target net-cost ]]                                ; then they harm the target by the net energy.

    if ( target-cost > 0 ) [                                   ; If the target paid a positive amount of
      if ( random-float 1.0 <=                                 ; energy and can overpower the caller
        ( 1 - size-probability ) ) [                           ; then they aid the caller by net energy.
        ask target [ harm myself net-cost ]]]
  ]
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
  complete-action target "harm" 0                              ; Record that this action has started.
  if ( is-anima1? target and                                   ; Check that the target is able to interact
    [ is.alive ] of target = true and cost > 0) [              ; and if the cost is a positive value.

    ask target [                                               ; Decrease the target's survival chance
      survival-chance ( - cost )                               ; by the input cost.
      set cause.of.death ( word "Attacked by "                 ; Temporarily record the likely cause of
        [my.identity] of myself "." )                          ; death of the target as being attacked by caller.
      set attack.from.history lput
      [my.identity] of myself                                  ; Record that the caller has harmed the target.
      attack.from.history ]

    set label "-"                                              ; Display the '-' symbol to indicate that caller
                                                               ; helped the target.

    let relatedness-with-target relatedness-with target        ; Record information on the degree of genetic
    if ( relatedness-with-target > 0.90 )                      ; relatedness between the caller and the target
    [ set whole.related.attack.cost                            ; and how much energy when into this interaction.
      whole.related.attack.cost + cost ]
    if ( relatedness-with-target <= 0.90 and
      relatedness-with-target > 0.40 )
    [ set half.related.attack.cost
      half.related.attack.cost + cost ]
    if ( relatedness-with-target <= 0.40 and
      relatedness-with-target > 0.15 )
    [ set fourth.related.attack.cost
      fourth.related.attack.cost + cost ]
    if ( relatedness-with-target <= 0.15 and
      relatedness-with-target > 0.05 )
    [ set eighth.related.attack.cost
      eighth.related.attack.cost + cost ]
    if ( relatedness-with-target <= 0.05 )
    [ set not.related.attack.cost
      not.related.attack.cost + cost ]

    if ( life.history = "adult" and [life.history] of target = "adult" and biological.sex = "male" and group.identity = [group.identity] of target ) [
      ask target [
        set from.ingroup.male.attack.count from.ingroup.male.attack.count + 1
        set from.ingroup.male.attack.energy.spent from.ingroup.male.attack.energy.spent + cost ]]
    if ( life.history = "adult" and [life.history] of target = "adult" and biological.sex = "male" and group.identity != [group.identity] of target ) [
      ask target [
        set from.nongroup.male.attack.count from.nongroup.male.attack.count + 1
        set from.nongroup.male.attack.energy.spent from.nongroup.male.attack.energy.spent + cost ]]
    if ( life.history = "adult" and [life.history] of target = "adult" and biological.sex = "female" and group.identity = [group.identity] of target ) [
      ask target [
        set from.ingroup.female.attack.count from.ingroup.female.attack.count + 1
        set from.ingroup.female.attack.energy.spent from.ingroup.female.attack.energy.spent + cost ]]
    if ( life.history = "adult" and [life.history] of target = "adult" and biological.sex = "female" and group.identity != [group.identity] of target ) [
      ask target [
        set from.nongroup.female.attack.count from.nongroup.female.attack.count + 1
        set from.nongroup.female.attack.energy.spent from.nongroup.female.attack.energy.spent + cost ]]

    if ( life.history = "adult" and [life.history] of target = "adult" and [biological.sex] of target = "male" and group.identity = [group.identity] of target ) [
        set to.ingroup.male.attack.count to.ingroup.male.attack.count + 1
        set to.ingroup.male.attack.energy.spent to.ingroup.male.attack.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "adult" and [biological.sex] of target = "male" and group.identity != [group.identity] of target ) [
      set to.nongroup.male.attack.count to.nongroup.male.attack.count + 1
      set to.nongroup.male.attack.energy.spent to.nongroup.male.attack.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "adult" and [biological.sex] of target = "female" and group.identity = [group.identity] of target ) [
      set to.ingroup.female.attack.count to.ingroup.female.attack.count + 1
      set to.ingroup.female.attack.energy.spent to.ingroup.female.attack.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "adult" and [biological.sex] of target = "female" and group.identity != [group.identity] of target ) [
      set to.nongroup.female.attack.count to.nongroup.female.attack.count + 1
      set to.nongroup.female.attack.energy.spent to.nongroup.female.attack.energy.spent + cost ]

    if ( life.history = "adult" and [life.history] of target = "juvenile" and [biological.sex] of target = "male" and group.identity = [group.identity] of target ) [
        set to.ingroup.male.juvenile.attack.count to.ingroup.male.juvenile.attack.count + 1
        set to.ingroup.male.juvenile.attack.energy.spent to.ingroup.male.juvenile.attack.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "juvenile" and [biological.sex] of target = "male" and group.identity != [group.identity] of target ) [
      set to.nongroup.male.juvenile.attack.count to.nongroup.male.juvenile.attack.count + 1
      set to.nongroup.male.juvenile.attack.energy.spent to.nongroup.male.juvenile.attack.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "juvenile" and [biological.sex] of target = "female" and group.identity = [group.identity] of target ) [
      set to.ingroup.female.juvenile.attack.count to.ingroup.female.juvenile.attack.count + 1
      set to.ingroup.female.juvenile.attack.energy.spent to.ingroup.female.juvenile.attack.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "juvenile" and [biological.sex] of target = "female" and group.identity != [group.identity] of target ) [
      set to.nongroup.female.juvenile.attack.count to.nongroup.female.juvenile.attack.count + 1
      set to.nongroup.female.juvenile.attack.energy.spent to.nongroup.female.juvenile.attack.energy.spent + cost ]

    if ( life.history = "adult" and [life.history] of target = "infant" and [biological.sex] of target = "male" and group.identity = [group.identity] of target ) [
      set to.ingroup.male.infant.attack.count to.ingroup.male.infant.attack.count + 1
      set to.ingroup.male.infant.attack.energy.spent to.ingroup.male.infant.attack.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "infant" and [biological.sex] of target = "male" and group.identity != [group.identity] of target ) [
      set to.nongroup.male.infant.attack.count to.nongroup.male.infant.attack.count + 1
      set to.nongroup.male.infant.attack.energy.spent to.nongroup.male.infant.attack.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "infant" and [biological.sex] of target = "female" and group.identity = [group.identity] of target ) [
      set to.ingroup.female.infant.attack.count to.ingroup.female.infant.attack.count + 1
      set to.ingroup.female.infant.attack.energy.spent to.ingroup.female.infant.attack.energy.spent + cost ]
    if ( life.history = "adult" and [life.history] of target = "infant" and [biological.sex] of target = "female" and group.identity != [group.identity] of target ) [
      set to.nongroup.female.infant.attack.count to.nongroup.female.infant.attack.count + 1
      set to.nongroup.female.infant.attack.energy.spent to.nongroup.female.infant.attack.energy.spent + cost ]

    if ( [ life.history ] of target = "infant" and             ; If target is an infant and the caller
      target != self and                                       ; has not previously harmed the target,
      not member? [my.identity] of target                      ; temporarily record that the caller has
      infanticide.history )                                    ; committed infanticide on the target.
    [ set infanticide.history lput                             ; However, this record is removed if the
      [my.identity] of target infanticide.history ]            ; target lives to be a juvenile.

    complete-action target "harm-complete" 0                   ; Record that this action has been completed.
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
  complete-action target "mate-with" cost                      ; Record that this action has started.

  if ( not member? "stork" model-structure
    and cost > 0                                               ; Perform a check to make sure that
    and is-anima1? target                                      ; copulation only occurs when certain
    and [ is.alive ] of target = true                          ; criteria met, including that the
    and life.history = "adult"                                 ; cost of mating must be greater than 0,
    and [life.history] of target = "adult"                     ; caller and target must be alive and
    and ( biological.sex = "male"                              ; be adults, be opposite sexes, the
      or ( biological.sex = "female"                           ; female must be cycling, and they must
        and fertility.status = "cycling" ))                    ; both have a chance to conceive greater
    and ( [biological.sex] of target = "male"                  ; than 0.
      or ( [biological.sex] of target = "female"
        and [fertility.status] of target = "cycling" ))
    and ( biological.sex != [biological.sex] of target )
    and conception.chance > 0
    and [conception.chance] of target > 0 ) [                  ; If all of these criteria are met...

    let caller-cost [ get-action-cost-of myself "mate-with" ] of target
    let target-cost get-action-cost-of target "mate-with"

    let net-cost ( caller-cost + target-cost )
    let probability ( caller-cost + target-cost )              ; The probability of caller mating with
    / ( abs caller-cost + abs target-cost + 0.0000000001 )     ; the target depends on the relative costs paid
    if ( random-float 1.0 <= probability ) [                   ; by caller and target. If the probability is
                                                               ; high enough, caller mates with the target.

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
  complete-action target "conceive-with" 0                     ; Record that this action has started.
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
;
;  oo          oo   dP   oo          dP oo                     dP   oo
;                   88               88                        88
;  dP 88d888b. dP d8888P dP .d8888b. 88 dP d888888b .d8888b. d8888P dP .d8888b. 88d888b.
;  88 88'  `88 88   88   88 88'  `88 88 88    .d8P' 88'  `88   88   88 88'  `88 88'  `88
;  88 88    88 88   88   88 88.  .88 88 88  .Y8P    88.  .88   88   88 88.  .88 88    88
;  dP dP    dP dP   dP   dP `88888P8 dP dP d888888P `88888P8   dP   dP `88888P' dP    dP
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
  set label-color white                                        ; The label color is white to be more visible.
  set my.identity random 9999999                               ; Set identity to a random number.
  let mean-sex-ratio mean (list
    [sex.ratio] of m
    [sex.ratio] of f)
  set biological.sex ifelse-value                              ; The probability of becoming male or female
  ( random-float 1.0 < mean-sex-ratio )                        ; depends on the preferred sex ratio of parents.
  ["male"] ["female"]
  set life.history "gestatee"                                  ; New individuals start out as gestatees.
  set fertility.status " "                                     ; New individuals do not have a fertility status.
  set group.identity [group.identity] of m                     ; New individuals are given same group identity
                                                               ; a their mother.
  set is.alive true
  set carried.items []                                         ; New individuals start out with their attributes
  set fully.decayed false                                      ; empty, false, or set to zero.
  set energy.supply 0
  set x.magnitude 0
  set y.magnitude 0
  ifelse ( member? "no-evolution" model-structure )            ; The "no-evolution" model structure results
  [ set chromosome.I [chromosome.I] of m                       ; in new offspring inheriting their gentoype
    set chromosome.II [chromosome.II] of m ]                   ; directly from their mother.
  [ setup-chromosomes-from m f ]                               ; By default, offspring inherit their genes from
                                                               ; both parents.
  set my.environment []
  set decision.vectors []
  set actions.completed []
  set age.in.ticks 0
  set generation.number ( [generation.number] of m + 1 )       ; New individuals have a generation one more
                                                               ; than their mother.
  set my.mother m
  set mother.identity [my.identity] of m                       ; A new individual's identity is inherited from
  set father.identity [my.identity] of f                       ; both parents.
  set natal.group.identity group.identity                      ; Record information about the group to
  set natal.group.size count anima1s with [                    ; which the new individual was born.
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
  set foraging.gains.this.timestep 0
  set energy.gains.this.timestep 0
  set energy.cost.this.timestep 0
  set help.from.history []
  set attack.from.history []
  set copulations.history []
  set conceptions.history []
  set group.transfers.history []
  set infanticide.history []                                   ; A premature cause of death is assumed to
  set cause.of.death "Suspected genetic abnormality."          ; be genetic by default.
  ifelse ( member? "ideal-form" model-structure )              ; The "ideal-form" model-structure sets new
  [ set-phenotype-to-ideal-form ]                              ; individuals to have population average
  [ set-phenotype-to-initialized-form ]                        ; attributes. By default, individuals are
                                                               ; initialized with default attributes.
end

; --------------------------------------------------------------------------------------------------------- ;
;
; CALLER'S PHENOTYPE IS SET TO STARTING CONDITIONS
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
; CALLER'S PHENOTYPE IS INITIALIZED FOR 'IDEAL-FORM' MODEL-STRUCTURE
;
;
; ENTRY:
;
; EXIT:
;
; --------------------------------------------------------------------------------------------------------- ;

to set-phenotype-to-ideal-form
  ask my.mother [                                              ; Set the mother's energy to half its
    let new-energy-supply ( energy.supply / 2 )                ; original value and then give the remaining
    set energy.supply new-energy-supply                        ; half to the new offspring.
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
; VIRTUAL CHROMOSOMAL RECOMBINATION
;
; This subroutine is called by indivifual anima1s and results in potentially modified versions of the current
; organization for the individual's chromosome.I and chromosome.II.
;
; ENTRY:  mutation-chance-per-locus | defines the chance that a mutation will
;           occur at each allele locus.
;
;         Caller has complete knowledge of current configuration of chromosome.I and chromosome.II
;
; EXIT:   When this subroutine is complete, both chromosome.I and chromosome.II have been updated with
;         any modifications.
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
; VIRTUAL CHROMOSOMAL MUTATION
;
; This subroutine is called by indivifual anima1s and results in potentially modified versions of the current
; organization for the individual's chromosome.I and chromosome.II.
;
; ENTRY:  mutation-chance-per-locus | defines the chance that a mutation will
;           occur at each allele locus.
;
;         Caller has complete knowledge of current configuration of chromosome.I and chromosome.II
;
; EXIT:   When this subroutine is complete, both chromosome.I and chromosome.II have been updated with
;         any modifications.
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

  let ouput-chromosome []                                        ; Establish an empty chromosome.

  foreach input-chromosome [ allele ->                           ; Begin a loop through each allele
    let updated-alleles (list allele)                            ; in chromosome supplied on entry.

    let first-allele first allele
                                                                 ; The allele is able to mutate if
    if ( ( first-allele != "0|"                                  ; the first codon is not '0' or false
      or first-allele = true )                                   ; Check whether the allele is able to mutate.
      and random-float 1.0 < mutation-chance-per-locus ) [       ; Check the probability of mutation.

      let choice ( ifelse-value                                  ; Check for range of mutation
        ( first-allele = "1|" ) [ 1 ]                            ; choices allowed.
        ( first-allele = "2|" ) [ one-of [ 1 2 ] ]
        ( first-allele = "3|" ) [ one-of [ 1 2 3 ] ]
        ( first-allele = "4|" ) [ one-of [ 1 2 3 4 5 ] ]
        ( first-allele = "5|" ) [ one-of [ 1 2 3 4 5 6 7 ] ]
        [ one-of [ 1 2 3 4 5 6 7 ] ] )                           ; Or any choice is allowed if not specified.

      (ifelse                                                    ; Select the proper case.

                                                                 ; Prepare to mutate the allele
        ( choice < 6 ) [
          let new-allele []
          let random-index random ( length allele - 1 ) + 1      ; Exclude the first codon from mutation.
          let index 0
          foreach allele [ codon ->                              ; Proceed to examine each codon in the allele.
            ( ifelse

              ( choice = 1
                and random-index = index                         ; CHOICE 1: mutate codon if it is a number
                and not member? "uninvadable" model-structure )  ; unless the structure is uninvadable.
              [ set new-allele
                lput get-mutation codon "numbers" new-allele ]

              ( choice = 2                                       ; CHOICE 2: mutate codon, letters only
                and random-index = index                         ; unless the structure is uninvadable.
                and not member? "uninvadable" model-structure )
              [ set new-allele
                lput get-mutation codon "letters" new-allele ]

              ( choice = 3                                       ; CHOICE 3: mutate codon, both numbers and
                and random-index = index                         ; letters unless the structure is uninvadable.
                and not member? "uninvadable" model-structure )
              [ set new-allele
                lput get-mutation codon "both" new-allele ]

              ( choice = 4                                       ; CHOICE 4: duplicate codon
                and random-index = index )
              [ repeat 2
                [ set new-allele lput codon new-allele ] ]

              ( choice = 5                                       ; CHOICE 5: delete codon
                and random-index = index ) [  ]

              [ set new-allele lput codon new-allele ])          ; Copy untouched codons into the new allele.
            set index index + 1 ]                                ; Advance and repeat for all codons,
          set updated-alleles ( list new-allele ) ]              ; then update the allele.

        ( choice = 6 ) [                                         ; CHOICE 6: duplicate allele making an exact copy
          set updated-alleles (list allele allele) ]

        ( choice = 7 ) [                                         ; CHOICE 7: delete allele completely
          set updated-alleles [] ]

        [])]

    foreach updated-alleles [ allele-update ->                   ; Finally, if modification occurred
      set ouput-chromosome ifelse-value                          ; in any allele, then update the
      ( allele-update != [] )                                    ; chromosome with those modified
      [ lput allele-update ouput-chromosome ]                    ; alleles.
      [ ouput-chromosome ]]
  ]
  report ouput-chromosome                                        ; Return the results.
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
    [ sta2us-get-mutation unmutated-codon type-of-mutation]    ; then get mutation from sta2us extension

    ( genotype-reader = "gat3s" )                              ; If genotype-reader is gat3s..
    [ g8tes-get-mutation unmutated-codon type-of-mutation]     ; then get mutation from gat3s extension

    [ sta2us-get-mutation unmutated-codon type-of-mutation ] ) ; default mutation if gentoype-reader not indicated
end
@#$#@#$#@
GRAPHICS-WINDOW
7
86
837
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
617
10
683
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
688
10
755
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
81
10
609
79
path-to-experiment
../results/
1
0
String

BUTTON
761
10
836
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
847
415
1116
535
simulation-notes
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

SLIDER
848
173
1117
206
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
848
210
1117
243
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
848
100
1117
133
plant-seasonality
plant-seasonality
0
1
0.5
.05
1
NIL
HORIZONTAL

SLIDER
848
28
1116
61
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
848
64
1117
97
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
910
248
1056
317
population
population
1
0
String

INPUTBOX
910
323
1056
391
genotype
NIL
1
0
String

BUTTON
848
248
903
317
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
248
1117
281
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
284
1117
317
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
847
542
1055
587
useful-commands
useful-commands
"help-me" "meta-report" "---------------------" " > OPERATIONS   " "---------------------" "parameter-settings" "default-settings" "model-structure" "-- aspatial" "-- free-lunch" "-- ideal-form" "-- no-evolution" "-- no-plants" "-- reaper" "-- stork" "-- uninvadable" "clear-population" "reset-plants" "save-notes" "save-world" "import-world" "output-results" "---------------------" " > VERIFICATION " "---------------------" "dynamic-check" "-- true" "-- false" "runtime-check" "visual-check" "-- attack-pattern" "-- dine-and-dash" "-- life-history-channel" "-- musical-pairs" "-- night-and-day" "-- popularity-context" "-- speed-mating" "-- square-dance" "-- supply-and-demand" "---------------------" " > DISPLAY RESULTS   " "---------------------" "age" "generations" "genotype" "phenotype" "-- survival-chance" "-- body-size" "-- body-shade" "-- fertility-status" "-- hidden-chance" "-- bite-capacity" "-- mutation-chance" "-- sex-ratio" "-- litter-size" "-- conception-chance" "-- visual-angle" "-- visual-range" "-- day-perception" "-- night-perception" "carried-items" "energy-supply" "behaviors" "-- environment" "-- decisions" "-- actions" "-- matings" "-- mating-partners" "-- conceptions" "-- infanticide" "-- group-transfers" "-- travel-distance" "-- foraging-gains" "-- total-energy-gains" "-- total-energy-cost" "show-territories"
45

BUTTON
1061
542
1116
587
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
849
322
904
391
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
358
1117
391
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
322
1117
355
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
848
136
1117
169
plant-quality
plant-quality
.1
100
5.0
.1
1
NIL
HORIZONTAL

PLOT
849
663
1707
917
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

SWITCH
1429
623
1519
656
adults
adults
1
1
-1000

SWITCH
1326
623
1424
656
juveniles
juveniles
1
1
-1000

SWITCH
1232
623
1322
656
infants
infants
1
1
-1000

SWITCH
1126
623
1227
656
gestatees
gestatees
1
1
-1000

SWITCH
1523
623
1613
656
males
males
1
1
-1000

SWITCH
1617
623
1707
656
females
females
1
1
-1000

CHOOSER
849
611
1118
656
plot-type
plot-type
"individuals" "groups" "generations"
0

OUTPUT
1126
10
1705
587
12

BUTTON
7
10
74
79
path...
path-button
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
849
592
1715
610
-- plot ----------------------------------------------------------------------------------------------------------------------------------------
11
0.0
1

TEXTBOX
850
10
1130
52
-- parameters -------------------------------
11
0.0
1

TEXTBOX
848
397
1132
415
-- commands --------------------------------
11
0.0
1

BUTTON
1626
20
1681
53
⬆︎
NIL
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
1625
544
1680
577
⬇︎
NIL
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
1785
244
1940
304
starting-seed
0.0
1
0
Number

@#$#@#$#@
# B3GET 1.2.0 INFORMATION

## WHAT IS IT?

B3GET is designed to test hypotheses in biology by simulating populations of virtual organisms evolving over generations, whose evolutionary outcomes reflect the selection pressures of their environment. Users import population files to seed the initial population and run simulations to evolve these populations - and their genotypes - over generations. Behavioral strategies that are beneficial for their ecological context are expected to emerge.

B3GET helps answer fundamental questions in evolutionary biology by offering users a virtual field site to precisely track the evolution of organismal populations. Researchers can use B3GET to: (1) investigate how populations vary in response to ecological pressures; (2) trace evolutionary histories over indefinite time scales and generations; (3) track an individual for every moment of their life from conception to post-mortem decay; (4) create virtual analogues of living species, including primates like baboons and chimpanzees, to answer species-specific questions; and (5) determine the plausible evolutionary pathways of optimal strategies in response to ecological pressures. Users are able to save, edit, and import population and genotype files, offering an array of possibilities for creating controlled biological experiments.

## HOW IT WORKS

B3GET simulates several factors considered important in biology, including life history trade-offs, investment in body size, variation in aggression, sperm competition, infanticide, and competition over access to food and mates. B3GET calculates each agent’s decision-vectors from its diploid chromosomes and current environmental context. These decision-vectors dictate movement, body growth, desire to mate and eat, and other agent actions. Chromosomes are modified during recombination and mutation, resulting in behavioral strategies that evolve over generations.

## HOW TO USE IT

### STARTING UP

B3GET should come with the following file and [folder] structure:

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

Crouse, Kristin (2021). “B3GET” (Version 1.2.0). CoMSES Computational Model Library. Retrieved from: https://www.comses.net/codebases/6b10f629-7958-4b31-b489-d51c17d0f5b8/releases/1.2.0/

Peer-reviewed publication on an earlier version of this model:

Crouse, K. N., Miller, C. M., & Wilson, M. L. (2019). New approaches to modeling primate socioecology: Does small female group size BEGET loyal males?. Journal of human evolution, 137, 102671.

## COPYRIGHT AND LICENSE

© 2021 K N Crouse

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

### HIDDEN 'ORGANS'

REPRODUCTION: determines the ability to conceive and create offspring.
PERCEPTION: determines the ability to perceive the environment.
REGULATORS: determines the chance of changing current state of signal or trait.
DIRECTION: tracks the overall preferred direction to go.
CHROMOSOMES: diploid chromosomes regulate the innate actions of the agent.
DECISION CENTER: where decisions live in the mind before becoming actionable.

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
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="observation-notes">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-quality">
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="males">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infants">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="&quot;ProtoPan&quot;"/>
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
      <value value="&quot;show-territories&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="females">
      <value value="true"/>
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
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plant-maximum-neighbors">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="output-results?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="path-to-experiment">
      <value value="&quot;../data/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype">
      <value value="&quot;protopan&quot;"/>
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
