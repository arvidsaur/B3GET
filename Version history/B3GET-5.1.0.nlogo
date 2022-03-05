extensions [sound table]

breed [ plants plant ]
breed [ primates primate ]
breed [ groups group ]
breed [ vectors vector ]

primates-own [
  ; STATIC
  sex
  generation
  mother
  father
  lineage-identity
  allele-identity
  behavior-chromosome-I
  behavior-chromosome-II
  combined-chromosome
  identity-chromosome-I
  identity-chromosome-II
  ; STATUS
  age
  life-history
  ; ENERGY
  action-energy
  stomach-energy
  maternal-energy
  ; CHANCE
  life-history-chance
  fertility-chance
  mortality-chance
  conception-chance
  ; TRACKING
  count-conceptions
  current-group
  natal-group
  count-group-transfers
  group-transfers-list
  age-at-last-birth
  interbirth-interval-list
  interbirth-interval
  alpha-infanticide-attempted?
  beta-infanticide-attempted?
]

groups-own [
  group-color
]

vectors-own [
  vector-target
  vector-action
  vector-weight
]

patches-own [
  fertile?
]

plants-own [
  penergy
]

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::: INITIAL PARAMETERS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

globals [
  ; WORLD
  world-energy-pool
  ; CHROMOSOMES
  ego-chromosome-list
  other-chromosome-list
  action-chromosome-list
  mutation-chromosome-list
  ; INITIAL
  alpha-chromosome-I
  alpha-chromosome-II
  beta-chromosome-I
  beta-chromosome-II
  alpha-male-conception-chance
  beta-male-conception-chance
  alpha-female-conception-chance
  beta-female-conception-chance
  alpha-male-mortality-chance
  beta-male-mortality-chance
  alpha-female-mortality-chance
  beta-female-mortality-chance
  alpha-male-body-size
  beta-male-body-size
  alpha-female-body-size
  beta-female-body-size
  ; LISTS
  lower-character-list
  upper-character-list
  move-toward-list
  move-away-list
  life-history-list
  fertility-list
  ; GLOBAL VALUES
  global-mutation-rate
  global-sex-ratio
  global-perception-range
  ; LIFE HISTORY
  gestatee-age-list
  infant-age-list
  juvenile-age-list
  adult-age-list
  senescent-age-list
  count-alpha-infanticides
  count-beta-infanticides
]

to setup-parameters
  load-chromosomes
  set world-energy-pool world-energy-rate
  set lower-character-list [ "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" ]
  set upper-character-list [ "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "?" "!" ]
  set move-toward-list [ "M" "T" "J" "A" "G" ]
  set move-away-list [ "W" "L" ]
  set life-history-list [ "gestatee" "infant" "juvenile" "adult" "senescent" "dead" ]
  set fertility-list [ "cycling" "pregnant" "lactating" ]
  set global-mutation-rate 0.10
  set global-sex-ratio 0.5
  set global-perception-range 10
  set gestatee-age-list []
  set infant-age-list []
  set juvenile-age-list []
  set adult-age-list []
  set senescent-age-list []
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::: SETUP ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to setup-world
  clear-all
  setup-parameters
  setup-patches
  setup-plants
  setup-groups
  setup-primates
  reset-ticks
end

to setup-patches
  ask patches [
    set pcolor green + 2
    set fertile? false
  ]
  set world-energy-rate 10 + 5 * initial-number-of-groups * females-per-group
  ;print (( count patches * patch-density ) / ( count patches * pi * patch-radius ^ 2 ))
  ask n-of (( count patches * patch-density ) / ( count patches * pi * patch-radius ^ 2 )) patches [ set fertile? true ]
end

to setup-plants
  ask patches with [ fertile? = true ] [
    sprout-plants 1 [
      set penergy world-energy-rate / ( count patches with [ fertile? ] )
      initialize-plant
    ]
  ]
end

to setup-groups
  create-groups initial-number-of-groups [ initialize-group ]
end

to setup-primates
  ask groups [
    let cI generate-alphabet-string 10
    let cII generate-alphabet-string 10
    ; MALE
    hatch-primates ceiling females-per-group [
      initialize-primate
      set sex "male"
      set shape "triangle"
      set current-group myself
      set identity-chromosome-I cI
      set identity-chromosome-II cII
      ifelse [ who ] of current-group / 2 = floor ([ who ] of current-group / 2) [
        set lineage-identity "alpha"
        set allele-identity "alpha"
        set behavior-chromosome-I alpha-chromosome-I
        set behavior-chromosome-II alpha-chromosome-II
        set conception-chance alpha-male-conception-chance
        set mortality-chance alpha-male-mortality-chance
        set size alpha-male-body-size
      ][
        set lineage-identity "beta"
        set allele-identity "beta"
        set behavior-chromosome-I beta-chromosome-I
        set behavior-chromosome-II beta-chromosome-II
        set conception-chance beta-male-conception-chance
        set mortality-chance beta-male-mortality-chance
        set size beta-male-body-size
      ]
      initialize-setup-primates
    ]
    ; FEMALES
    hatch-primates females-per-group [
      initialize-primate
      set sex "female"
      set shape "circle"
      set current-group myself
      set identity-chromosome-I cI
      set identity-chromosome-II cII
      ifelse [ who ] of current-group / 2 = floor ([ who ] of current-group / 2) [
        set lineage-identity "alpha"
        set allele-identity "alpha"
        set behavior-chromosome-I alpha-chromosome-I
        set behavior-chromosome-II alpha-chromosome-II
        set conception-chance alpha-female-conception-chance
        set mortality-chance alpha-female-mortality-chance
        set size alpha-female-body-size
      ][
        set lineage-identity "beta"
        set allele-identity "beta"
        set behavior-chromosome-I beta-chromosome-I
        set behavior-chromosome-II beta-chromosome-II
        set conception-chance beta-female-conception-chance
        set mortality-chance beta-female-mortality-chance
        set size beta-female-body-size
      ]
      initialize-setup-primates ]]
end

to initialize-setup-primates
  mutate-chromosomes
  set combined-chromosome get-combined-chromosomes behavior-chromosome-I behavior-chromosome-II
  set generation 0
  set hidden? false
  set age random 1000
  move-to one-of patches with [ distance [current-group] of myself < 3 ]
  set mother nobody
  set father nobody
  set life-history "adult"
  set stomach-energy 1
  set natal-group current-group
  update-fertility
  update-status
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::: GO :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to go

  if (selection-part != "none" and stop-simulation-at > 0 and genotype-file-number > stop-simulation-at ) [ stop ]

  ; CLEAN UP WORLD
  clear-links
  ask vectors [ die ]
  ask primates with [ action-energy = 0 and stomach-energy = 0 and (life-history = "juvenile" or life-history = "adult" or life-history = "senescent")] [ set life-history "dead" ]
  ask groups [ if group-size = 0 [ die ]]
  ask groups [
    set xcor mean [xcor] of primates with [ current-group = myself]
    set ycor mean [ycor] of primates with [ current-group = myself]]

  ; PLANT PROCEDURES
  ask patches with [ fertile? and not any? plants-here ] [
    sprout-plants 1 [
      initialize-plant
    ]
  ]
  ask plants [ set penergy get-updated-value penergy ( world-energy-rate / ( count patches with [ fertile? ] ))
  set color scale-color green penergy 2 -1 ]

  ; PRIMATE PROCEDURES
  ;set world-energy-pool world-energy-rate
  ask primates [ update-energy-and-metabolism ]
  ask primates [ update-status ]
  ask primates [ if action-energy > 0 [ make-decisions-and-do-actions ]]

  ; SELECTION EXPERIMENTS
  if selection-part = "Survival Selection" [ selection-1 ]
  if selection-part = "Life History Selection" [ selection-2 ]
  if selection-part = "Male Strategy Selection" [ selection-3 ]

  tick
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::: PLANTS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to initialize-plant
  set hidden? false
  set color scale-color green penergy 2 -1
  set size 1
  set shape "circle"
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

to-report group-size report count primates with [ current-group = myself ] end
to-report female-group-size report count primates with [ current-group = myself and life-history = "adult" and sex = "female" ] end
to-report male-group-size report count primates with [ current-group = myself and life-history = "adult" and sex = "male" ] end
to-report group-radius report mean [distance current-group] of primates with [ current-group = myself ] end
to-report cycling-females report count primates with [ current-group = myself and life-history = "adult" and sex = "female" and fertility = "cycling" ] end

to group-die
  ask primates with [ current-group = myself ] [ die ]
  die
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::: PRIMATES :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;:::: INITIALIZE PRIMATE :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to initialize-primate-with-parents [ m f ]
  initialize-primate
  set mother m
  set father f
  set lineage-identity [lineage-identity] of f
  setup-chromosomes-from-parents m f
  set generation [generation] of m + 1
  move-to m
  set color [color] of mother
  set current-group [current-group] of mother
  set natal-group current-group
end

to initialize-primate
  set sex ifelse-value ( random-float 1.0 < global-sex-ratio ) ["male"] ["female"]
  set shape ifelse-value ( sex = "female" ) ["circle"] ["triangle"]
  set hidden? true
  set label-color grey
  set label ""
  set age 0
  set life-history "gestatee"
  set conception-chance 0.5
  set mortality-chance 0
  set life-history-chance 0
  set fertility-chance 0
  set maternal-energy 0
  set stomach-energy 0
  set action-energy 0
  set size 0.01
  set color black
  set generation 0
  set current-group nobody
  set count-conceptions 0
  set natal-group nobody
  set count-group-transfers 0
  set group-transfers-list []
  set age-at-last-birth 0
  set interbirth-interval-list []
  set interbirth-interval 0
  set alpha-infanticide-attempted? false
  set beta-infanticide-attempted? false
end

to setup-chromosomes-from-parents [m f]
  ; CHROMOSOME I
  set behavior-chromosome-I []
  let i 0
  while [i < length [behavior-chromosome-I] of m ] [
    ifelse random-float 1.0 < 0.5 ; half the time, alleles will switch places
    [ set behavior-chromosome-I lput (item i [behavior-chromosome-I] of m) behavior-chromosome-I ]
    [ set behavior-chromosome-I lput (item i [behavior-chromosome-II] of m) behavior-chromosome-I ]
    set i i + 1 ]

  ; CHROMOSOME II
  set behavior-chromosome-II []
  set i 0
  while [i < length [behavior-chromosome-I] of f ] [
    ifelse random-float 1.0 < 0.5 ; half the time, alleles will switch places
    [ set behavior-chromosome-II lput (item i [behavior-chromosome-I] of f) behavior-chromosome-II ]
    [ set behavior-chromosome-II lput (item i [behavior-chromosome-II] of f) behavior-chromosome-II ]
    set i i + 1 ]

  if random-float 1.0 < 0.5 [
    let chromosome-holder behavior-chromosome-I
    set behavior-chromosome-I behavior-chromosome-II
    set behavior-chromosome-II chromosome-holder
  ]

  ; CHROMOSOME III & IV
  set identity-chromosome-I one-of (list [identity-chromosome-I] of m [identity-chromosome-II] of m )
  set identity-chromosome-II one-of (list [identity-chromosome-I] of f [identity-chromosome-II] of f )
  set i 0
  while [i < length identity-chromosome-I ] [ if random-float 1.0 < 0.5 ; half the time, alleles will switch places
    [ let itemIII item i identity-chromosome-I
      let itemIV item i identity-chromosome-II
      set identity-chromosome-I replace-item i identity-chromosome-I itemIV
      set identity-chromosome-II replace-item i identity-chromosome-II itemIII ]
    set i i + 1 ]
  mutate-chromosomes
  set combined-chromosome get-combined-chromosomes behavior-chromosome-I behavior-chromosome-II
end

to mutate-chromosomes
  ; CHROMOSOME I & II
  foreach behavior-chromosome-I [ a ->
    let index position a behavior-chromosome-I
    if ( item index mutation-chromosome-list = true ) [ if (random-float 1.0 < global-mutation-rate) [ set behavior-chromosome-I replace-item index behavior-chromosome-I (get-updated-value (item index behavior-chromosome-I) (random-float 1.0 - random-float 1.0)) ]]]
  foreach behavior-chromosome-II [ a ->
    let index position a behavior-chromosome-II
    if ( item index mutation-chromosome-list = true ) [ if (random-float 1.0 < global-mutation-rate)  [ set behavior-chromosome-II replace-item index behavior-chromosome-II (get-updated-value (item index behavior-chromosome-II) (random-float 1.0 - random-float 1.0)) ]]]

  ; CHROMOSOME III & IV
  repeat length identity-chromosome-I * 2 [
    if random-float 1.0 < global-mutation-rate [
      let index random length identity-chromosome-I
      ifelse random-float 1.0 < 0.5
        [ set identity-chromosome-I replace-item index identity-chromosome-I one-of lower-character-list ]
        [ set identity-chromosome-II replace-item index identity-chromosome-II one-of lower-character-list ]]]
end

;:::: PRIMATE STATUS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to update-energy [update]
  set action-energy action-energy + update
  if action-energy < 0 [ set action-energy 0 ]
end

to-report phenotype-of [ target ]
  let target-self ifelse-value ( target = self ) [ "e" ] [ "o" ]
  let target-sex first [sex] of target
  let target-life-history first [life-history] of target
  let target-fertility first [fertility] of target
  let target-size ifelse-value ( [size] of target < size ) [ "r" ] [ "b" ]
  let target-health ifelse-value ( [mortality-chance] of target > mortality-chance ) [ "d" ] [ "h" ]
  let target-kin ifelse-value ( get-degree-relatedness-with target >= 0.25 ) [ "k" ] [ "q" ]
  let target-group ifelse-value ( [current-group] of target = current-group ) [ "n" ] [ "u" ]
  report (word target-self target-sex target-life-history target-fertility target-size target-health target-kin target-group )
end

to-report get-degree-relatedness-with [ target ]
  let matching 0
  let not-matching 0
  let i 0
  while [i < length identity-chromosome-I] [
    let allele-here item i identity-chromosome-I
    ifelse (( allele-here = item i [identity-chromosome-I] of target ) or ( allele-here = item i [identity-chromosome-II] of target ))
    [ set matching matching + 1 ] [ set not-matching not-matching + 1 ]
    set i i + 1 ]
  while [i < length identity-chromosome-II] [
    let allele-here item i identity-chromosome-II
    ifelse (( allele-here = item i [identity-chromosome-I] of target ) or ( allele-here = item i [identity-chromosome-II] of target ))
    [ set matching matching + 1 ] [ set not-matching not-matching + 1 ]
    set i i + 1 ]
  report matching / ( matching + not-matching )
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
  record-life-history
  ifelse ( position life-history life-history-list + 1 < length life-history-list )
  [ set life-history item (( position life-history life-history-list ) + 1)  life-history-list ]
  [ set life-history first life-history-list ]
  set hidden? false
  if mother != nobody [ if [life-history] of mother = "pregnant" [ ask mother [ update-fertility ]]]
  update-fertility
end

to record-life-history
  if (life-history = "gestatee" ) [ set gestatee-age-list lput age gestatee-age-list ]
  if (life-history = "infant" ) [ set infant-age-list lput age infant-age-list ]
  if (life-history = "juvenile" ) [ set juvenile-age-list lput age juvenile-age-list ]
  if (life-history = "adult" ) [ set adult-age-list lput age adult-age-list ]
  if (life-history = "senescent" ) [ set senescent-age-list lput age senescent-age-list ]
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

; 6. UPDATES
to update-status
  ; UPDATES
  set age age + 1
  update-mortality-chance 0.001
  update-conception-chance -0.001
  ifelse current-group = nobody [ set color black ][ set color [group-color] of current-group ]
  if length interbirth-interval-list > 0 [ set interbirth-interval ( median interbirth-interval-list) ]
  ; STAY WITH MOTHER
  if (life-history = "gestatee" or life-history = "infant") and mother != nobody [
    move-to mother rt random 360 fd random-float 1.0 ]
  ; CHANCE OF THINGS HAPPENING
  if random-float 1.0 < life-history-chance [ update-life-history set life-history-chance 0 ]
  if random-float 1.0 < fertility-chance [ update-fertility set fertility-chance 0 ]
  if random-float 1.0 <= mortality-chance or life-history = "dead" [
    if life-history = "infant" and alpha-infanticide-attempted? [ set count-alpha-infanticides count-alpha-infanticides + 1 ]
    if life-history = "infant" and beta-infanticide-attempted? [ set count-beta-infanticides count-beta-infanticides + 1 ]
    if (count primates with [ current-group = [current-group] of myself ] = 1) [ ask current-group [ die ]]
    foreach behavior-chromosome-I [ die ]
    foreach behavior-chromosome-II [ die ]
    die ] ; protocols for death
end

;:::: PRIMATE FUNCTIONS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; 1. ENERGY
to update-energy-and-metabolism
  if ( life-history = "gestatee" or life-history = "infant" or life-history = "juvenile" ) [ get-maternal-energy-of mother ]
  update-energy stomach-energy
  set stomach-energy 0
  let bmr-cost ( abs size ^ 0.762 )
  update-energy ( - bmr-cost )
end

to get-maternal-energy-of [ target ]
  if target = mother and mother != nobody [
    set stomach-energy stomach-energy + [ maternal-energy] of mother
    ask mother [ set maternal-energy 0 ]
  ]
end

to make-decisions-and-do-actions
  let meself self
  let self-phenotype phenotype-of meself
  let decision-vectors []

  ; 2. ENVIRONMENT
  let local-primates primates with [ distance myself < global-perception-range ]
  ask n-of ( ceiling 0.1 * count local-primates ) local-primates [
    let otherguy self
    let other-phenotype [phenotype-of otherguy] of meself
    let i 0
    ; 3. DECISIONS
    foreach ego-chromosome-list [ ego ->
      if first-string-is-part-of-last? ( ego ) ( self-phenotype ) [
        let other-item item i other-chromosome-list
        if first-string-is-part-of-last? ( other-item ) ( other-phenotype ) [
          let action item i action-chromosome-list
          let weight item i [combined-chromosome] of meself
          let vector-doesnt-exist true
          foreach decision-vectors [ v ->
            if ( [vector-target] of v = other-item ) and ( [vector-action] of v = action ) [
              set vector-doesnt-exist false
              ask v [ set vector-weight vector-weight + weight ]]]
          if vector-doesnt-exist [
            hatch-vectors 1 [
              set hidden? true
              set vector-target otherguy
              set vector-action action
              set vector-weight ifelse-value (distance meself <= 1) [ weight ] [ weight / (( distance meself ) ^ 2 ) ]
              set decision-vectors lput self decision-vectors]]
      ]]
      set i i + 1
  ]]

  ; NORMLIZE WEIGHTS
  let sum-weight 0
  foreach decision-vectors [ v -> set sum-weight sum-weight + [vector-weight] of v ]
  foreach decision-vectors [ v -> ask v [ set vector-weight vector-weight / sum-weight ]]

  ; 4. ACTIONS
  let X-magnitude one-of [ 0.1 -0.1 ]
  let Y-magnitude one-of [ 0.1 -0.1 ]
  foreach decision-vectors [ v ->
    if ( member? ([vector-action] of v) move-toward-list or member? ([vector-action] of v) move-away-list ) [
      let ycor-difference ([ycor] of [vector-target] of v - ycor )
      let xcor-difference ([xcor] of [vector-target] of v - xcor )
      let angle ifelse-value ( ycor-difference = 0 or xcor-difference = 0 ) [ random 360 ] [ atan ycor-difference xcor-difference ]
      if ( member? ([vector-action] of v) move-away-list ) [ set angle angle - 180 ]
      set X-magnitude X-magnitude + ([vector-weight] of v * sin angle)
      set Y-magnitude Y-magnitude + ([vector-weight] of v * cos angle)
    ]
    if distance [vector-target] of v <= 1 [ do-action ([vector-action] of v) ([vector-target] of v) ( action-energy * [vector-weight] of v) ]
  ]

  ; 5. MOVEMENT
  move ( atan Y-magnitude X-magnitude ) action-energy
end

to move [ dirn value ]
  set heading dirn
  forward get-updated-value size value
  update-energy ( - value )
end

;::::: primate ACTIONS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

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
  if (infanticide-on? and first-string-is-part-of-last? "i" phenotype-of target) or ( not first-string-is-part-of-last? "i" phenotype-of target) [
    ifelse ( random-float 1.0 < winning-likelihood-with target )
    [ ask target [
      if life-history = "infant" and lineage-identity = "alpha" [ set alpha-infanticide-attempted? true ]
      if life-history = "infant" and lineage-identity = "beta" [ set beta-infanticide-attempted? true ]
      update-mortality-chance value ]]
    [ if life-history = "infant" and lineage-identity = "alpha" [ set alpha-infanticide-attempted? true ]
      if life-history = "infant" and lineage-identity = "beta" [ set beta-infanticide-attempted? true ]
      update-mortality-chance value ]]
end

to mate [ target ]
  if target != self [
    ifelse in-link-neighbor? target
    [ if current-group = [current-group] of target
      [ ifelse sex = "female"
        [ check-conception target ]
        [ ask target [ check-conception myself ]]]]
    [ create-link-to target [ set hidden? true ]]]
end

to check-conception [ target ] ; FEMALE PROCEDURE
  if fertility = "cycling" and life-history = "adult" and [life-history] of target = "adult" [
    if random-float 1.0 < mean (list conception-chance [conception-chance] of target) [
      hatch-primates 1 [ initialize-primate-with-parents myself target ]
      let previous-birth-age age-at-last-birth
      set age-at-last-birth age
      if previous-birth-age != 0 [ set interbirth-interval-list lput ( age - previous-birth-age ) interbirth-interval-list ]
      set count-conceptions count-conceptions + 1
      ask target [ set count-conceptions count-conceptions + 1 ]
      update-fertility ]]
end

to groom [ target value ]
  ask target [ update-mortality-chance ( - value ) ]
end

to join [ target value ]
 if random-float 1.0 < value [
    set current-group [current-group] of target
    if (current-group != natal-group and [group-size] of current-group > 1) [
      ifelse (member? [who] of current-group group-transfers-list)
      [ if ([who] of current-group != last group-transfers-list) [
        set count-group-transfers count-group-transfers + 1
        set group-transfers-list lput [who] of current-group group-transfers-list ]]
      [ set group-transfers-list lput [who] of current-group group-transfers-list
        set count-group-transfers count-group-transfers + 1]]
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
  set maternal-energy maternal-energy + value
end

to leave [ value ]
  if (female-dispersal-on? and sex = "female") or sex = "male" [
    if ( random-float 1.0 < value ) [
      hatch-groups 1 [
        initialize-group
        ask myself [ set current-group myself ]
  ]]]
end

to forage [ value ]
;  let energy-eaten e ^ ( get-updated-value 0.5 value )
;  ifelse ( energy-eaten < world-energy-pool )
;  [ set stomach-energy stomach-energy + energy-eaten
;    set world-energy-pool world-energy-pool - energy-eaten ]
;  [ set stomach-energy stomach-energy + world-energy-pool
;    set world-energy-pool 0 ]

  let energy-eaten e ^ ( get-updated-value (sum [penergy] of plants-here) value )
  set stomach-energy ( stomach-energy + energy-eaten )
  ask plants-here [ die ]

end

to level-up [ value ]
  set life-history-chance get-updated-value life-history-chance value
end

to level-up-fertility [ value ]
  set fertility-chance get-updated-value fertility-chance value
end

to give-birth
  let baby one-of primates with [ mother = myself and life-history = "gestatee" ]
  if baby != nobody [ ask baby [
    update-life-history
    set hidden? false ]]
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::: FUNCTIONS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to-report load-chromosome [ file-name ]
  let first-chromosome []
  let second-chromosome []
  let initial-male-conception-chance 0.5
  let initial-male-mortality-chance 0.001
  let initial-male-body-size 0.9
  let initial-female-conception-chance 0.5
  let initial-female-mortality-chance 0.001
  let initial-female-body-size 0.9
  if ( file-name != false )
  [ set ego-chromosome-list []
    set other-chromosome-list []
    set action-chromosome-list []
    set mutation-chromosome-list []
    file-open file-name
    while [ not file-at-end? ]
    [ let file-item file-read
      ifelse file-item = "male-conception-chance" [ set initial-male-conception-chance file-read ][
        ifelse file-item = "female-conception-chance" [ set initial-female-conception-chance file-read ][
          ifelse file-item = "male-mortality-chance" [ set initial-male-mortality-chance file-read ][
            ifelse file-item = "female-mortality-chance" [ set initial-female-mortality-chance file-read ][
              ifelse file-item = "male-body-size" [ set initial-male-body-size file-read ][
                ifelse file-item = "female-body-size" [ set initial-female-body-size file-read ][
                  ifelse is-number? file-item
                  [ ifelse length first-chromosome > length second-chromosome
                    [ set second-chromosome lput file-item second-chromosome ]
                    [ set first-chromosome lput file-item first-chromosome ]]
                  [ set ego-chromosome-list lput file-item ego-chromosome-list
                    set other-chromosome-list lput file-read other-chromosome-list
                    set action-chromosome-list lput file-read action-chromosome-list
                    set mutation-chromosome-list lput read-from-string file-read mutation-chromosome-list ]]]]]]]]
    file-close ]
  report (list
    first-chromosome
    second-chromosome
    initial-male-conception-chance
    initial-male-mortality-chance
    initial-male-body-size
    initial-female-conception-chance
    initial-female-mortality-chance
    initial-female-body-size )
end

to load-chromosomes
  let loaded-chromosome load-chromosome get-file-string ifelse-value (selection-part = "none") [ genotype1-file ] [ genotype-file-number ]
  set alpha-chromosome-I item 0 loaded-chromosome
  set alpha-chromosome-II item 1 loaded-chromosome
  set alpha-male-conception-chance item 2 loaded-chromosome
  set alpha-male-mortality-chance item 3 loaded-chromosome
  set alpha-male-body-size item 4 loaded-chromosome
  set alpha-female-conception-chance item 5 loaded-chromosome
  set alpha-female-mortality-chance item 6 loaded-chromosome
  set alpha-female-body-size item 7 loaded-chromosome
  if genotype2-file != "" and selection-part = "none" [ set loaded-chromosome load-chromosome ( get-file-string genotype2-file )]
  set beta-chromosome-I item 0 loaded-chromosome
  set beta-chromosome-II item 1 loaded-chromosome
  set beta-male-conception-chance item 2 loaded-chromosome
  set beta-male-mortality-chance item 3 loaded-chromosome
  set beta-male-body-size item 4 loaded-chromosome
  set beta-female-conception-chance item 5 loaded-chromosome
  set beta-female-mortality-chance item 6 loaded-chromosome
  set beta-female-body-size item 7 loaded-chromosome
end

to-report get-combined-chromosomes [ chromosome1 chromosome2 ]
  let new-chromosome chromosome1
  foreach chromosome2 [ a ->
    let index position a chromosome2
    let old-value item index new-chromosome
    set new-chromosome replace-item index new-chromosome (( old-value + a ) / 2)  ]
  report new-chromosome
end

to-report generate-alphabet-string [ number-length ]
  let output-string ""
  repeat number-length [ set output-string insert-item length output-string output-string one-of lower-character-list ]
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

to printout-genotype-of [ input-agent-set name-of-file ]
  file-open name-of-file
  let i 0
  while [ i < length ego-chromosome-list ] [
    file-print (word
      "\"" item i ego-chromosome-list "\"" " "
      "\"" item i other-chromosome-list "\"" " "
      "\"" item i action-chromosome-list "\"" " "
      "\"" item i mutation-chromosome-list "\"" " "
      precision (mean [ item i behavior-chromosome-I] of input-agent-set) 10 " "
      precision (mean [ item i behavior-chromosome-Ii] of input-agent-set) 10)
    set i i + 1 ]
  file-print (word "\"" "conception-chance" "\"" " " precision (mean [conception-chance] of input-agent-set) 10 " ")
  file-print (word "\"" "mortality-chance" "\"" " " precision (mean [mortality-chance] of input-agent-set) 30 " ")
  file-print (word "\"" "body-size" "\"" " " precision (mean [size] of input-agent-set) 10 " ")
  file-close
end

to-report get-file-string [ file-name ]
  report (word folder-name "/" file-name)
end

to-report get-next-available-file-name [ file-name file-designation ]
  let new-number 1
  while [file-exists? (word file-name "_" new-number file-designation ) ][ set new-number new-number + 1 ]
  report (word file-name "_" new-number file-designation )
end


;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::: SELECTION :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to output-successful-population
  let file-name get-next-available-file-name (word (get-file-string genotype-file-number) "_o" ) ".csv"
  export-world file-name
  printout-genotype-of ( n-of 1 primates ) file-name
end

to restart-unsuccessful-population
  let file-name get-next-available-file-name (word (get-file-string genotype-file-number) "_x" ) ".csv"
  export-world file-name
  setup-world
end

to restart-simulation [ restart-primates ]
  ifelse not any? restart-primates
  [ restart-unsuccessful-population ]
  [ export-world (word (get-file-string genotype-file-number) ".csv")
    set genotype-file-number genotype-file-number + 1
    printout-genotype-of (n-of 1 restart-primates) (get-file-string genotype-file-number)
    setup-world
  ]
end

; SURVIVAL SELECTION
to selection-1
  if ( count primates < ( females-per-group * initial-number-of-groups )) [
    let selected-primates primates with [ generation >= selection-number ]
    let the-selected-primate max-n-of 1 selected-primates [generation]
    let max-generation-of-group max [generation] of the-selected-primate
    if max-generation-of-group > selection-number [ set selection-number selection-number + 1 ]
    restart-simulation the-selected-primate ]
end

;; LIFE HISTORY SELECTION: GROUP FIDELITY, LARGE BODY SIZE, SHORT LIFE HISTORY
to selection-2
  ask primates with [ age > maximum-age ] [ die ]
  ask primates with [ distance current-group > 5 ] [ die ]
  ask primates with [ life-history = "adult" and size < 0.85 and size > 0.95 ] [ die ]
  ask primates with [ min-one-of groups [ distance myself ] != current-group ] [die]
  selection-1
end

;; MALE STRATEGY SELECTION
to selection-3
  if (ticks > 0 and ticks = select-every ) [
    ifelse count primates with [ sex = "male" and count-conceptions > 0 and generation > 0 ] = 0
    [ restart-unsuccessful-population ]
    [ if ( genotype-file-number = stop-simulation-at ) [ printout-genotype-info ]
      let male-primates max-n-of 1 primates with [ sex = "male" and generation > 0 ] [count-conceptions]
      restart-simulation male-primates ]
  ]
end

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::: DATA ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;::::: GENOTYPE INFO :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to printout-genotype-info
  file-open (get-file-string output-file)
  file-print (word

    folder-name " "
    genotype-file-number " "
    ifelse-value ( count primates <  2 * initial-number-of-groups * females-per-group ) [ "YES" ] [ "NO" ] " "
    (count-alpha-infanticides / length gestatee-age-list) " "
    (count-beta-infanticides / length gestatee-age-list) " "
    initial-number-of-groups " "
    females-per-group " "
    world-energy-rate " "

    ; population
    count primates " "
    median [generation] of primates " "
    mean [generation] of primates " "

    ; groups
    ifelse-value ( any? groups with [ group-size > 1 ]) [count groups with [ group-size > 1 ]] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0]) [ min [female-group-size] of groups with [ female-group-size > 0 ]] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0]) [ median [female-group-size] of groups with [ female-group-size > 0 ]] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0]) [ mean [female-group-size] of groups with [ female-group-size > 0 ]] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0]) [ max [female-group-size] of groups with [ female-group-size > 0 ]] [0] " "

    ; males per group
    ifelse-value ( any? groups with [female-group-size > 0]) [ median [male-group-size] of groups with [ female-group-size > 0 ]] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0]) [ mean [male-group-size] of groups with [ female-group-size > 0 ] ] [0] " "
    ifelse-value ( count groups with [female-group-size > 0] > 1) [ variance [male-group-size] of groups with [ female-group-size > 0 ]] [0] " "

    ; male transfers
    ifelse-value ( any? primates with [ sex = "male" and count-group-transfers > 0 ]) [ median [count-group-transfers] of primates with [ sex = "male" and count-group-transfers > 0 ]] [0] " "
    ifelse-value ( any? primates with [ sex = "male" and count-group-transfers > 0 ]) [ mean [count-group-transfers] of primates with [ sex = "male" and count-group-transfers > 0 ] ] [0] " "
    ifelse-value ( any? primates with [ sex = "male" and count-group-transfers > 0 ]) [ max [count-group-transfers] of primates with [ sex = "male" and count-group-transfers > 0 ]] [0] " "

    ; male RS
    ifelse-value ( any? primates with [ sex = "male" and count-group-transfers > 0 ]) [ median [count-conceptions] of primates with [ sex = "male" and count-group-transfers > 0 ]] [0] " "
    ifelse-value ( any? primates with [ sex = "male" and count-group-transfers > 0 ]) [ mean [count-conceptions] of primates with [ sex = "male" and count-group-transfers > 0 ] ] [0] " "
    ifelse-value ( count primates with [ sex = "male" and count-group-transfers > 0 ] > 1) [ variance [count-conceptions] of primates with [ sex = "male" and count-group-transfers > 0 ] ] [0] " "
    ifelse-value ( any? primates with [ sex = "male" and count-group-transfers > 0 ]) [ max [count-conceptions] of primates with [ sex = "male" and count-group-transfers > 0 ]] [0] " "

    ; male body size
    ifelse-value ( any? primates with [ sex = "male" and life-history = "adult" ]) [median [size] of primates with [ sex = "male" and life-history = "adult" ]] [0] " "
    ifelse-value ( any? primates with [ sex = "male" and life-history = "adult" ]) [ mean [size] of primates with [ sex = "male" and life-history = "adult" ] ] [0]" "
    ifelse-value ( any? primates with [ sex = "male" and life-history = "adult" ]) [max [size] of primates with [ sex = "male" and life-history = "adult" ]] [0] " "

    ; male conception chance
    ifelse-value ( any? primates with [ sex = "male" and life-history = "adult" ]) [median [conception-chance] of primates with [ sex = "male" and life-history = "adult" ]] [0] " "
    ifelse-value ( any? primates with [ sex = "male" and life-history = "adult" ]) [ mean [conception-chance] of primates with [ sex = "male" and life-history = "adult" ]  ] [0] " "
    ifelse-value ( any? primates with [ sex = "male" and life-history = "adult" ]) [max [conception-chance] of primates with [ sex = "male" and life-history = "adult" ]] [0] " "

    ; group radius
    ifelse-value ( any? groups with [female-group-size > 0]) [ min [group-radius] of groups with [ female-group-size > 0 ]] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0]) [ median [group-radius] of groups with [ female-group-size > 0 ]] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0]) [ mean [group-radius] of groups with [ female-group-size > 0 ]] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0]) [ max [group-radius] of groups with [ female-group-size > 0 ]] [0] " "

    ; female transfers
    ifelse-value ( any? primates with [ current-group != natal-group and sex = "female" ]) [ count primates with [ current-group != natal-group and sex = "female" ]] [0] " "
    ifelse-value ( any? primates with [ sex = "female" and count-group-transfers > 0 ]) [ count primates with [ count-group-transfers > 0 and sex = "female" ] ] [0] " "

    ; female RS
    ifelse-value ( any? primates with [ sex = "female" and life-history = "adult" ]) [ median [count-conceptions] of primates with [ sex = "female" and life-history = "adult" ]] [0] " "
    ifelse-value ( any? primates with [ sex = "female" and life-history = "adult" ]) [ mean [count-conceptions] of primates with [ sex = "female" and life-history = "adult" ] ] [0] " "
    ifelse-value ( any? primates with [ sex = "female" and life-history = "adult" ]) [ max [count-conceptions] of primates with [ sex = "female" and life-history = "adult" ]] [0] " "

    ; female body size
    ifelse-value ( any? primates with [ sex = "female" and life-history = "adult" ]) [median [size] of primates with [ sex = "female" and life-history = "adult" ]] [0] " "
    ifelse-value ( any? primates with [ sex = "female" and life-history = "adult" ]) [mean [size] of primates with [ sex = "female" and life-history = "adult" ]  ] [0]" "
    ifelse-value ( any? primates with [ sex = "female" and life-history = "adult" ]) [max [size] of primates with [ sex = "female" and life-history = "adult" ]] [0] " "

    ; female conception chance
    ifelse-value ( any? primates with [ sex = "female" and life-history = "adult" ]) [median [conception-chance] of primates with [ sex = "female" and life-history = "adult" ]] [0] " "
    ifelse-value ( any? primates with [ sex = "female" and life-history = "adult" ]) [ mean [conception-chance] of primates with [ sex = "female" and life-history = "adult" ]] [0] " "
    ifelse-value ( any? primates with [ sex = "female" and life-history = "adult" ]) [max [conception-chance] of primates with [ sex = "female" and life-history = "adult" ]] [0] " "

    ; fertile females
    ifelse-value ( any? groups with [female-group-size > 0] ) [ median [cycling-females] of groups with [female-group-size > 0] ] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0] ) [ mean [cycling-females] of groups with [female-group-size > 0] ] [0] " "
    ifelse-value ( count groups with [female-group-size > 0] > 1) [ variance [cycling-females] of groups with [female-group-size > 0] ] [0] " "

    ; life history
    ifelse-value ( length gestatee-age-list > 0) [min gestatee-age-list] [0] " "
    ifelse-value ( length gestatee-age-list > 0) [median gestatee-age-list] [0] " "
    ifelse-value ( length gestatee-age-list > 0) [mean gestatee-age-list] [0] " "
    ifelse-value ( length gestatee-age-list > 0) [max gestatee-age-list] [0] " "
    ifelse-value ( length infant-age-list > 0) [min infant-age-list] [0] " "
    ifelse-value ( length infant-age-list > 0) [median infant-age-list] [0] " "
    ifelse-value ( length infant-age-list > 0) [mean infant-age-list] [0] " "
    ifelse-value ( length infant-age-list > 0) [max infant-age-list] [0] " "
    ifelse-value ( length juvenile-age-list > 0) [min juvenile-age-list] [0] " "
    ifelse-value ( length juvenile-age-list > 0) [median juvenile-age-list] [0] " "
    ifelse-value ( length juvenile-age-list > 0) [mean juvenile-age-list] [0] " "
    ifelse-value ( length juvenile-age-list > 0) [max juvenile-age-list] [0] " "
    ifelse-value ( length adult-age-list > 0) [min adult-age-list] [0] " "
    ifelse-value ( length adult-age-list > 0) [median adult-age-list] [0] " "
    ifelse-value ( length adult-age-list > 0) [mean adult-age-list] [0] " "
    ifelse-value ( length adult-age-list > 0) [max adult-age-list] [0] " "
    ifelse-value ( length senescent-age-list > 0) [min senescent-age-list] [0] " "
    ifelse-value ( length senescent-age-list > 0) [median senescent-age-list] [0] " "
    ifelse-value ( length senescent-age-list > 0) [mean senescent-age-list] [0] " "
    ifelse-value ( length senescent-age-list > 0) [max senescent-age-list] [0] " "
    ifelse-value ( any? primates with [ sex = "female" and life-history = "adult" and interbirth-interval > 0 ] ) [min [ interbirth-interval ] of primates with [ sex = "female" and life-history = "adult" and interbirth-interval > 0 ]] [0] " "
    ifelse-value ( any? primates with [ sex = "female" and life-history = "adult" and interbirth-interval > 0 ] ) [median [ interbirth-interval ] of primates with [ sex = "female" and life-history = "adult" and interbirth-interval > 0 ]] [0] " "
    ifelse-value ( any? primates with [ sex = "female" and life-history = "adult" and interbirth-interval > 0 ] ) [mean [ interbirth-interval ] of primates with [ sex = "female" and life-history = "adult" and interbirth-interval > 0 ]] [0] " "
    ifelse-value ( any? primates with [ sex = "female" and life-history = "adult" and interbirth-interval > 0 ] ) [max [ interbirth-interval ] of primates with [ sex = "female" and life-history = "adult" and interbirth-interval > 0 ]] [0] " "

  )
  file-close
end

;::::: CARRYING CAPACITY ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to printout-energy
  if any? groups with [female-group-size > 1 ] [
    file-open (get-file-string output-file)
    file-print (word
      folder-name " "
      genotype-file-number " "
      initial-number-of-groups " "
      females-per-group " "
      world-energy-rate " "
      ifelse-value ( any? primates ) [count primates] [0] " "
      ifelse-value ( any? primates with [ sex = "female"]) [count primates with [ sex = "female"]] [0] " "
      ifelse-value ( any? groups with [ group-size > 1 ]) [ count groups with [ group-size > 1 ]] [0] " "
      ifelse-value ( any? groups with [female-group-size > 0]) [ min [female-group-size] of groups with [ female-group-size > 0 ]] [0] " "
      ifelse-value ( any? groups with [female-group-size > 0]) [ median [female-group-size] of groups with [ female-group-size > 0 ]] [0] " "
      ifelse-value ( any? groups with [female-group-size > 0]) [ mean [female-group-size] of groups with [ female-group-size > 0 ] ] [0] " "
      ifelse-value ( any? groups with [female-group-size > 0]) [ max [female-group-size] of groups with [ female-group-size > 0 ]] [0] " "
      ifelse-value ( any? groups with [female-group-size > 0]) [ min [group-radius] of groups with [ female-group-size > 0 ]] [0] " "
      ifelse-value ( any? groups with [female-group-size > 0]) [ median [group-radius] of groups with [ female-group-size > 0 ]] [0] " "
      ifelse-value ( any? groups with [female-group-size > 0]) [ mean [group-radius] of groups with [ female-group-size > 0 ]] [0] " "
      ifelse-value ( any? groups with [female-group-size > 0]) [ max [group-radius] of groups with [ female-group-size > 0 ]] [0] " "
    )
    file-close
  ]
end

;::::: TOURNAMENT RESULTS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

to printout-results
  file-open (get-file-string output-file)
  file-print (word
    get-file-string output-file " "
    infanticide-on? " "
    ticks " "
    initial-number-of-groups " "
    females-per-group " "
    world-energy-rate " "
    ;female-genotype-file " "
    genotype1-file " "
    genotype2-file " "
    (count-alpha-infanticides / (length gestatee-age-list + 0.00000000001 )) " "
    (count-beta-infanticides / (length gestatee-age-list + 0.00000000001 )) " "
    count primates " "
    ifelse-value ( any? groups with [ group-size > 1] ) [count groups with [ group-size > 1 ]] [0] " "
    ifelse-value ( any? groups with [ female-group-size > 0] ) [count groups with [ female-group-size > 0]] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0] ) [median [female-group-size] of groups with [female-group-size > 0]] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0] ) [mean [female-group-size] of groups with [female-group-size > 0]] [0] " "
    ifelse-value ( any? primates with [ sex = "male" and ( life-history = "adult" or life-history = "senescent") ]) [median [count-conceptions] of primates with [ sex = "male" and ( life-history = "adult" or life-history = "senescent") ]] [0] " "
    ifelse-value ( any? primates with [ sex = "male" and ( life-history = "adult" or life-history = "senescent") ]) [mean [count-conceptions] of primates with [ sex = "male" and (life-history = "adult" or life-history = "senescent") ]] [0] " "
    ifelse-value ( count primates with [ sex = "male" and ( life-history = "adult" or life-history = "senescent") ] > 1) [variance [count-conceptions] of primates with [ sex = "male" and (life-history = "adult" or life-history = "senescent") ]] [0] " "
    ifelse-value ( any? primates with [ sex = "male" and ( life-history = "adult" or life-history = "senescent") ] ) [max [count-conceptions] of primates with [ sex = "male" and (life-history = "adult" or life-history = "senescent" )]] [0] " "
    get-lineage-representations " "
    get-lineage-median-RS " "
    get-lineage-mean-RS " "
    get-lineage-variance-RS " "
    get-lineage-max-RS " "
    get-allele-frequencies " "
    get-allele-median-RS " "
    get-allele-mean-RS " "
    get-allele-variance-RS " "
    get-allele-max-RS " "

    ifelse-value ( any? groups with [female-group-size > 0]) [ min [male-group-size] of groups with [ female-group-size > 0 ]] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0]) [ median [male-group-size] of groups with [ female-group-size > 0 ]] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0]) [ mean [male-group-size] of groups with [ female-group-size > 0 ] ] [0] " "
    ifelse-value ( count groups with [female-group-size > 0] > 1) [ variance [male-group-size] of groups with [ female-group-size > 0 ]] [0] " "
    ifelse-value ( any? groups with [female-group-size > 0]) [ max [male-group-size] of groups with [ female-group-size > 0 ]] [0] " "

    get-sib-report " "
    count primates with [mother != nobody and father != nobody]

  )
  file-close
  ;export-world get-next-available-file-name (word (get-file-string "" "") females-per-group "-" female-genotype-file "-" male1-genotype-file "-" male2-genotype-file ) (".csv")
end

to-report get-lineage-representations
  let alpha-primates primates with [ lineage-identity = "alpha" and generation > 0 ]
  let alpha-lineage-identity-representation ifelse-value ( any? alpha-primates ) [ count alpha-primates ] [ 0 ]
  let beta-primates primates with [ lineage-identity = "beta" and generation > 0 ]
  let beta-lineage-identity-representation ifelse-value ( any? beta-primates ) [ count beta-primates ] [ 0 ]
  report (word alpha-lineage-identity-representation " " beta-lineage-identity-representation)
end

to-report get-lineage-median-RS
  let alpha-primates primates with [ lineage-identity = "alpha" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let alpha-reproductive-success ifelse-value ( any? alpha-primates ) [ median [count-conceptions] of alpha-primates ] [ 0 ]
  let beta-primates primates with [ lineage-identity = "beta" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let beta-reproductive-success ifelse-value ( any? beta-primates ) [ median [count-conceptions] of beta-primates ] [ 0 ]
  report (word alpha-reproductive-success " " beta-reproductive-success )
end

to-report get-lineage-mean-RS
  let alpha-primates primates with [ lineage-identity = "alpha" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let alpha-reproductive-success ifelse-value ( any? alpha-primates ) [ mean [count-conceptions] of alpha-primates ] [ 0 ]
  let beta-primates primates with [ lineage-identity = "beta" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let beta-reproductive-success ifelse-value ( any? beta-primates ) [ mean [count-conceptions] of beta-primates ] [ 0 ]
  report (word alpha-reproductive-success " " beta-reproductive-success )
end

to-report get-lineage-variance-RS
  let alpha-primates primates with [ lineage-identity = "alpha" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let alpha-reproductive-success ifelse-value ( count alpha-primates > 1 ) [ variance [count-conceptions] of alpha-primates ] [ 0 ]
  let beta-primates primates with [ lineage-identity = "beta" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let beta-reproductive-success ifelse-value ( count beta-primates > 1) [ variance [count-conceptions] of beta-primates ] [ 0 ]
  report (word alpha-reproductive-success " " beta-reproductive-success )
end

to-report get-lineage-max-RS
  let alpha-primates primates with [ lineage-identity = "alpha" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let alpha-reproductive-success ifelse-value ( any? alpha-primates ) [ max [count-conceptions] of alpha-primates ] [ 0 ]
  let beta-primates primates with [ lineage-identity = "beta" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let beta-reproductive-success ifelse-value ( any? beta-primates ) [ max [count-conceptions] of beta-primates ] [ 0 ]
  report (word alpha-reproductive-success " " beta-reproductive-success )
end

to-report get-allele-frequencies
  let alpha-allele-count 0
  let beta-allele-count 0
  let alpha-still-alpha-count 0
  let beta-still-beta-count 0
  let alpha-now-beta-count 0
  let beta-now-alpha-count 0
  ask primates with [ generation > 0 ] [
    let my-alpha-count 0
    let my-beta-count 0
    foreach behavior-chromosome-I [ c ->
      if ( member? c alpha-chromosome-I or member? c alpha-chromosome-II) and not (member? c beta-chromosome-I or member? c beta-chromosome-II) [ set my-alpha-count my-alpha-count + 1 ]
      if ( member? c beta-chromosome-I or member? c beta-chromosome-II) and not (member? c alpha-chromosome-I or member? c alpha-chromosome-II) [ set my-beta-count my-beta-count + 1 ]]
    foreach behavior-chromosome-II [ c ->
      if ( member? c alpha-chromosome-I or member? c alpha-chromosome-II) and not (member? c beta-chromosome-I or member? c beta-chromosome-II) [ set my-alpha-count my-alpha-count + 1 ]
      if ( member? c beta-chromosome-I or member? c beta-chromosome-II) and not (member? c alpha-chromosome-I or member? c alpha-chromosome-II) [ set my-beta-count my-beta-count + 1 ]]
    if lineage-identity = "alpha" and my-alpha-count > my-beta-count [
      set allele-identity "alpha"
      set alpha-still-alpha-count alpha-still-alpha-count + 1 ]
    if lineage-identity = "alpha" and my-beta-count >= my-alpha-count [
      set allele-identity "beta"
      set alpha-now-beta-count alpha-now-beta-count + 1]
    if lineage-identity = "beta" and my-beta-count > my-alpha-count [
      set allele-identity "beta"
      set beta-still-beta-count beta-still-beta-count + 1]
    if lineage-identity = "beta" and my-alpha-count >= my-beta-count [
      set allele-identity "alpha"
      set beta-now-alpha-count beta-now-alpha-count + 1]
    set alpha-allele-count alpha-allele-count + my-alpha-count
    set beta-allele-count beta-allele-count + my-beta-count
  ]
  let total ( alpha-allele-count + beta-allele-count )
  report (word alpha-allele-count " " beta-allele-count " "  alpha-still-alpha-count " " beta-still-beta-count " " alpha-now-beta-count " " beta-now-alpha-count)
end

to-report get-allele-median-RS
  let alpha-primates primates with [ allele-identity = "alpha" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let alpha-reproductive-success ifelse-value ( any? alpha-primates ) [ median [count-conceptions] of alpha-primates ] [ 0 ]
  let beta-primates primates with [ allele-identity = "beta" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let beta-reproductive-success ifelse-value ( any? beta-primates ) [ median [count-conceptions] of beta-primates ] [ 0 ]
  report (word alpha-reproductive-success " " beta-reproductive-success )
end

to-report get-allele-mean-RS
  let alpha-primates primates with [ allele-identity = "alpha" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let alpha-reproductive-success ifelse-value ( any? alpha-primates ) [ mean [count-conceptions] of alpha-primates ] [ 0 ]
  let beta-primates primates with [ allele-identity = "beta" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let beta-reproductive-success ifelse-value ( any? beta-primates ) [ mean [count-conceptions] of beta-primates ] [ 0 ]
  report (word alpha-reproductive-success " " beta-reproductive-success )
end

to-report get-allele-variance-RS
  let alpha-primates primates with [ allele-identity = "alpha" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let alpha-reproductive-success ifelse-value ( count alpha-primates > 1 ) [ variance [count-conceptions] of alpha-primates ] [ 0 ]
  let beta-primates primates with [ allele-identity = "beta" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let beta-reproductive-success ifelse-value ( count beta-primates > 1) [ variance [count-conceptions] of beta-primates ] [ 0 ]
  report (word alpha-reproductive-success " " beta-reproductive-success )
end

to-report get-allele-max-RS
  let alpha-primates primates with [ allele-identity = "alpha" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let alpha-reproductive-success ifelse-value ( any? alpha-primates ) [ max [count-conceptions] of alpha-primates ] [ 0 ]
  let beta-primates primates with [ allele-identity = "beta" and sex = "male" and (life-history = "adult" or life-history = "senescent") ]
  let beta-reproductive-success ifelse-value ( any? beta-primates ) [ max [count-conceptions] of beta-primates ] [ 0 ]
  report (word alpha-reproductive-success " " beta-reproductive-success )
end

to-report get-sib-report
  let count-full-sibs 0
  let count-maternal-half-sibs 0
  let count-paternal-half-sibs 0
  let count-no-sibs 0
  ask primates [
    let me-a self
    ask primates [
      let me-b self
      if (me-a != me-b and [father] of me-a = [father] of me-b and [mother] of me-a = [mother] of me-b) [ set count-full-sibs count-full-sibs + 1 ]
      if (me-a != me-b and [father] of me-a != [father] of me-b and [mother] of me-a = [mother] of me-b) [ set count-maternal-half-sibs count-maternal-half-sibs + 1 ]
      if (me-a != me-b and [father] of me-a = [father] of me-b and [mother] of me-a != [mother] of me-b) [ set count-paternal-half-sibs count-paternal-half-sibs + 1 ]
      if (me-a != me-b and [father] of me-a != [father] of me-b and [mother] of me-a != [mother] of me-b) [ set count-no-sibs count-no-sibs + 1 ]
  ]]
  report (word count-full-sibs " " count-paternal-half-sibs " " count-maternal-half-sibs " " count-no-sibs)
end
@#$#@#$#@
GRAPHICS-WINDOW
232
93
676
538
-1
-1
8.72
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
7
12
70
45
setup
setup-world\n
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
76
12
137
45
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
684
186
758
231
Generation
median [generation] of primates
2
1
11

SLIDER
466
10
669
43
initial-number-of-groups
initial-number-of-groups
0
50
0.0
1
1
NIL
HORIZONTAL

SLIDER
466
49
669
82
females-per-group
females-per-group
1
50
1.0
1
1
NIL
HORIZONTAL

MONITOR
683
10
758
55
# primates
count primates
0
1
11

PLOT
684
59
906
179
Generations
generation
count
0.0
100.0
0.0
10.0
true
false
"" "clear-plot"
PENS
"default" 1.0 1 -16777216 true "" "histogram [generation] of primates"

MONITOR
763
186
834
231
# groups
count groups with [ group-size > 1 ]
17
1
11

INPUTBOX
233
12
460
82
folder-name
Version 5/SIMULATIONS/20180324
1
0
String

BUTTON
146
12
218
45
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
237
690
451
723
world-energy-rate
world-energy-rate
0
10000
10.0
5
1
/ timestep
HORIZONTAL

MONITOR
763
10
839
55
# females
count primates with [ sex = \"female\" ]
0
1
11

MONITOR
845
10
906
55
# males
count primates with [ sex = \"male\" ]
17
1
11

PLOT
1044
10
1278
255
Age
age
count
0.0
3000.0
0.0
5.0
true
false
"" "clear-plot"
PENS
"default" 100.0 1 -16777216 true "" "histogram [age] of primates"

PLOT
1101
452
1283
578
Female Reproductive Skew
# offspring
count
0.0
20.0
0.0
5.0
true
false
"" "clear-plot"
PENS
"default" 1.0 1 -16777216 true "" "histogram [count-conceptions] of primates with [ sex = \"female\" and life-history = \"adult\" ]"

PLOT
920
452
1093
577
Male Reproductive Skew
# offspring
count
0.0
20.0
0.0
5.0
true
false
"" "clear-plot"
PENS
"default" 1.0 1 -16777216 true "" "histogram [count-conceptions] of primates with [ sex = \"male\" and life-history = \"adult\" ]"

PLOT
685
237
908
369
Female Group Size
group size
count
0.0
30.0
0.0
10.0
true
false
"" "clear-plot"
PENS
"default" 1.0 1 -16777216 true "" "histogram [female-group-size] of groups with [female-group-size > 0]"

MONITOR
687
378
744
423
F size
median [size] of primates with [ sex = \"female\" and life-history = \"adult\" and generation > 0]
3
1
11

MONITOR
751
378
808
423
M size
median [size] of primates with [ sex = \"male\" and life-history = \"adult\" and generation > 0]
3
1
11

SLIDER
7
766
214
799
selection-number
selection-number
1
10
1.0
1
1
NIL
HORIZONTAL

MONITOR
1003
265
1090
310
# M transfer
count primates with [ count-group-transfers > 0 and sex = \"male\" ]
17
1
11

MONITOR
1184
265
1277
310
# F transfer
count primates with [ count-group-transfers > 0 and sex = \"female\" ]
17
1
11

MONITOR
841
186
911
231
# single
count groups with [ group-size = 1]
17
1
11

PLOT
920
318
1092
445
Male Dispersal
# group transfers
count
0.0
20.0
0.0
5.0
true
false
"" "clear-plot"
PENS
"default" 1.0 1 -16777216 true "" "histogram [count-group-transfers] of primates with [ sex = \"male\" and life-history = \"adult\" ]"

PLOT
1102
318
1281
445
Female Dispersal
# group transfers
count
0.0
20.0
0.0
5.0
true
false
"" "clear-plot"
PENS
"default" 1.0 1 -16777216 true "" "histogram [count-group-transfers] of primates with [ sex = \"female\" and life-history = \"adult\" ]"

PLOT
688
429
907
573
Body Size Dimorphism
size
count
0.5
1.01
0.0
1.0
true
true
"" "clear-plot"
PENS
"male" 0.01 1 -11053225 true "" "histogram [size] of primates with [ life-history = \"adult\" and sex = \"male\" and generation > 0]"
"female" 0.01 1 -4539718 true "" "histogram [size] of primates with [ life-history = \"adult\" and sex = \"female\" and generation > 0]"

MONITOR
920
10
1033
55
gestation length
median gestatee-age-list
1
1
11

MONITOR
920
61
1033
106
infant length
median infant-age-list - median gestatee-age-list
1
1
11

MONITOR
921
111
1034
156
juvenile length
median juvenile-age-list - median gestatee-age-list - median infant-age-list
1
1
11

MONITOR
922
161
1034
206
adult length
median adult-age-list - median juvenile-age-list - median gestatee-age-list - median infant-age-list
1
1
11

MONITOR
920
265
997
310
# M leave
count primates with [ current-group != natal-group and sex = \"male\" ]
17
1
11

MONITOR
1099
265
1177
310
# F leave
count primates with [ current-group != natal-group and sex = \"female\" ]
17
1
11

MONITOR
815
378
909
423
Group size
mean [female-group-size] of groups with [female-group-size > 0]
1
1
11

TEXTBOX
11
704
161
722
SELECTION SETTINGS
12
0.0
1

CHOOSER
8
649
213
694
selection-part
selection-part
"none" "Survival Selection" "Life History Selection" "Male Strategy Selection"
0

SLIDER
7
842
215
875
select-every
select-every
0
10000
1000.0
100
1
ticks
HORIZONTAL

TEXTBOX
12
925
162
943
SIMULATION SETTINGS
12
0.0
1

INPUTBOX
7
1081
214
1141
output-file
top_20.txt
1
0
String

SWITCH
238
647
448
680
infanticide-on?
infanticide-on?
0
1
-1000

SLIDER
8
727
215
760
genotype-file-number
genotype-file-number
0
100
1.0
1
1
NIL
HORIZONTAL

MONITOR
922
212
1035
257
interbirth-interval
median [ interbirth-interval ] of primates with [ sex = \"female\" and life-history = \"adult\" and interbirth-interval > 0\n]
2
1
11

SLIDER
7
881
216
914
stop-simulation-at
stop-simulation-at
0
50
4.0
1
1
genotype
HORIZONTAL

INPUTBOX
8
948
217
1008
genotype1-file
G31
1
0
String

INPUTBOX
7
1014
216
1074
genotype2-file
G31
1
0
String

SLIDER
8
805
215
838
maximum-age
maximum-age
0
10000
1800.0
100
1
ticks
HORIZONTAL

SWITCH
458
647
676
680
female-dispersal-on?
female-dispersal-on?
0
1
-1000

SLIDER
6
54
223
87
patch-density
patch-density
0
1
0.22
0.01
1
% covered
HORIZONTAL

SLIDER
6
92
223
125
patch-radius
patch-radius
0
1
0.09
0.01
1
% of world
HORIZONTAL

SLIDER
6
131
223
164
patch-quality
patch-quality
0
1
0.69
0.01
1
energy max
HORIZONTAL

SLIDER
6
170
223
203
patch-variability
patch-variability
0
1
0.38
0.01
1
% variable
HORIZONTAL

SLIDER
6
208
223
241
patch-seasonality
patch-seasonality
0
1
0.61
0.01
1
% seasonal
HORIZONTAL

SLIDER
6
247
223
280
patch-growth-rate
patch-growth-rate
0
1
0.92
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
Compatible with NetLogo 6.0.2

## WHAT IS IT?

This model simulates the behavior of evolved primate agents to test hypotheses in behavioral ecology. Users imput genotype files to seed the initial population and run simulations to evolve these populations, and their genotype files, over generation. Strategies that are most beneficial for a given environmental context are expected to emerge.

## HOW IT WORKS

Primate agents possess two chromosomes that dictate the weighted behavioral preference for given environmental conditions. These weighted preferences can mutate and so new behavioral strategies can emerge over generations.

## HOW TO USE IT

### UPLOADING GENOTYPE FILES

Create a folder in the same directory as the model and set FOLDER-NAME to the name you give this folder. Any genotype files that you want the model to access must be stored here. 

### GENERAL

SETUP: returns the model to the starting state
GO: runs the simulation
ONCE: runs exactly one tick, or time step, of the simulation
INITIAL-NUMBER-OF-GROUPS: The number of groups present at the start of a simulation
FEMALES-PER-GROUP: The initial number of females (and males) in each group
WORLD-ENERGY-RATE: The global available energy pool, which replenishes at each time step
INFANTICIDE-ON?: When turned on, allows males to attack infants
FEMALE-DISPERSAL-ON?: When turned on, allows females to leave their natal group
FOLDER-NAME: The reference folder where genotype files are stored
- This is just the name of the reference folder if its in the same directory as the model
- You can also use '/' if the folder with is nested within other folders
SELECTION-PART: This is "none" for normal simulations, otherwise see "SELECTION SETTINGS"

### SELECTION SETTINGS

* Survival Selection is used to select better surviving populations if your genotype file causes crashes
* Life History Selection is used to select populations with shorter maximum lifespans
* Male Genotype Selection is used to select for good reproductive strategy performance

GENOTYPE-FILE-NUMBER: The genotype file number to upload
- During selection, genotype file names must be integer numbers only with no extension!
SELECTION-NUMBER: This is not set by the user, but instead updates during the selection process
MAXIMUM-AGE: This is the maximum allowed age during the Life History Selection process
SELECT-EVERY: When ticks reaches this mark, the model updates the current selection process with a new genotype
STOP-SIMULATION-AT: During the selection process, new genotype files are produced as new numbers with an increment of 1. This setting stops further selection processes if this genotype number is reached.

### SIMULATION SETTINGS

General simulations should be run using the Simulation Settings. Make sure SELECTION-PART is set to "none." These simulations can seed worlds with one genotype for females and up to two genotypes for males. If you want all initial primates to have the same genotype, set all inputs to access the same genotype file. These genotype files do not need to be numerical names but can be any regular file name.

FEMALE-GENOTYPE-FILE: The name of genotype file to seed initial females
MALE1-GENOTYPE-FILE: The name of genotype file to seed half of initial males
MALE2-GENOTYPE-FILE: The name of genotype file to seed the other half of initial males
- Males from each genotype are distriuted evenly across groups
OUTPUT-FILE: This sets the name of the data file that you output from the model

### MONITORS

'#' PRIMATES: Total population size of primates
'#' MALES: Total number of primate males of any age
'#' FEMALES: Total number of primate females of any age
GESTATION LENGTH: Median gestation length for the population
INFANT LENGTH: Median infant length (lactation length) for the population
JUVENILE LENGTH: Median juvenile length for the population
ADULT LENGTH: Median adult length for the population
INTERBIRTH-INTERVAL: Median interbirth-interval for the current population
GENERATION: Median generation number for the current population
'#' GROUPS: Number of groups with more than one individual present (a social group)
'#' SINGLE: Number of groups with exactly one individual present (a solitary group)
'#' M LEAVE: Number of males who have left their natal group
'#' M TRANSFER: Number of males who have transfered into another group
'#' F LEAVE: Number of females who have left their natal group
'#' F TRANSFER: Number of females who have transfered into another group
F SIZE: Median body-size of adult females with generation number greater than 0
M SIZE: Median body-size of adult males with generation number greater than 0
GROUP SIZE: Median number of females per group

### PLOTS

GENERATION: Histogram of current generation number for the primate population
AGE: Histogram of current age for the primate population
FEMALE GROUP SIZE: Histogram of number of females per primate group
BODY SIZE DIMORPHISM: Histogram of adult primate body-size separated by sex
MALE DISPERSAL: Histogram of number of group transfers per male
FEMALE DISPERSAL: Histogram of number of group transfers per female
MALE REPRODUCTIVE SKEW: Histogram of male reproductive success (count-conceptions)
FEMALE REPRODUCTIVE SKEW: Histogram of female reproductive success (count-conceptions)

## THINGS TO NOTICE

The individuals in the initial seeded population have the same genotype(s). However, stochastic occurances and fluxuating environmental conditions (based on the social structure of the population) cause unique individual behaviors to emerge in an unexpected and idiosynchractic way. Over time, as the number of accumulated mutations increases, more individuals may appear to have unique behavioral strategies and life histories. 

## THINGS TO TRY

You can modify genotype files to many different configurations. The number of genes can also be modified but beware that lengthy genotypes may cause lag in runtime. Explore how INITIAL-NUMBER-OF-GROUPS and FEMALES-PER-GROUP setings may influence behavioral strategies. However, genotype configuration also influence female, and male, strategies.

## COPYRIGHT AND LICENSE

Copyright 2018 K N Crouse

The model may be freely used, modified and redistributed provided this copyright is included and the resulting models are not used for profit.

Contact K N Crouse at crou0048@umn.edu if you have questions about its use.
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
<experiments>
  <experiment name="carrying-capacity" repetitions="1" runMetricsEveryStep="true">
    <setup>setup-world</setup>
    <go>go</go>
    <final>printout-group-energy-data</final>
    <timeLimit steps="1000"/>
    <enumeratedValueSet variable="infanticide-on?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-dispersal-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-of-groups">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="females-per-group">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="world-energy-rate" first="10" step="10" last="300"/>
    <enumeratedValueSet variable="genotype1-file">
      <value value="&quot;G2&quot;"/>
      <value value="&quot;G18&quot;"/>
      <value value="&quot;G41&quot;"/>
      <value value="&quot;G48&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="male2-genotype-file">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="top-genotypes-tournament" repetitions="10" runMetricsEveryStep="true">
    <setup>setup-world</setup>
    <go>go</go>
    <final>printout-results</final>
    <timeLimit steps="100"/>
    <enumeratedValueSet variable="infanticide-on?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-dispersal-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="females-per-group">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-of-groups">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype1-file">
      <value value="&quot;G8&quot;"/>
      <value value="&quot;G18&quot;"/>
      <value value="&quot;G19&quot;"/>
      <value value="&quot;G21&quot;"/>
      <value value="&quot;G22&quot;"/>
      <value value="&quot;G31&quot;"/>
      <value value="&quot;G42&quot;"/>
      <value value="&quot;G46&quot;"/>
      <value value="&quot;G47&quot;"/>
      <value value="&quot;G48&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype2-file">
      <value value="&quot;G8&quot;"/>
      <value value="&quot;G18&quot;"/>
      <value value="&quot;G19&quot;"/>
      <value value="&quot;G21&quot;"/>
      <value value="&quot;G22&quot;"/>
      <value value="&quot;G31&quot;"/>
      <value value="&quot;G42&quot;"/>
      <value value="&quot;G46&quot;"/>
      <value value="&quot;G47&quot;"/>
      <value value="&quot;G48&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="genotype-performance" repetitions="40" runMetricsEveryStep="true">
    <setup>setup-world</setup>
    <go>go</go>
    <final>printout-genotype-info</final>
    <timeLimit steps="1000"/>
    <enumeratedValueSet variable="infanticide-on?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-dispersal-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-of-groups">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="females-per-group">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-energy-rate">
      <value value="170"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype1-file">
      <value value="&quot;M1&quot;"/>
      <value value="&quot;M2&quot;"/>
      <value value="&quot;M3&quot;"/>
      <value value="&quot;M4&quot;"/>
      <value value="&quot;M5&quot;"/>
      <value value="&quot;M6&quot;"/>
      <value value="&quot;M7&quot;"/>
      <value value="&quot;M8&quot;"/>
      <value value="&quot;M9&quot;"/>
      <value value="&quot;M10&quot;"/>
      <value value="&quot;M11&quot;"/>
      <value value="&quot;M12&quot;"/>
      <value value="&quot;M13&quot;"/>
      <value value="&quot;M14&quot;"/>
      <value value="&quot;M15&quot;"/>
      <value value="&quot;M16&quot;"/>
      <value value="&quot;M17&quot;"/>
      <value value="&quot;M18&quot;"/>
      <value value="&quot;M19&quot;"/>
      <value value="&quot;M20&quot;"/>
      <value value="&quot;M21&quot;"/>
      <value value="&quot;M22&quot;"/>
      <value value="&quot;M23&quot;"/>
      <value value="&quot;M24&quot;"/>
      <value value="&quot;M25&quot;"/>
      <value value="&quot;M26&quot;"/>
      <value value="&quot;M27&quot;"/>
      <value value="&quot;M28&quot;"/>
      <value value="&quot;M29&quot;"/>
      <value value="&quot;M30&quot;"/>
      <value value="&quot;M31&quot;"/>
      <value value="&quot;M32&quot;"/>
      <value value="&quot;M33&quot;"/>
      <value value="&quot;M34&quot;"/>
      <value value="&quot;M35&quot;"/>
      <value value="&quot;M36&quot;"/>
      <value value="&quot;M37&quot;"/>
      <value value="&quot;M38&quot;"/>
      <value value="&quot;M39&quot;"/>
      <value value="&quot;M40&quot;"/>
      <value value="&quot;M41&quot;"/>
      <value value="&quot;M42&quot;"/>
      <value value="&quot;M43&quot;"/>
      <value value="&quot;M44&quot;"/>
      <value value="&quot;M45&quot;"/>
      <value value="&quot;M46&quot;"/>
      <value value="&quot;M47&quot;"/>
      <value value="&quot;M48&quot;"/>
      <value value="&quot;M49&quot;"/>
      <value value="&quot;M50&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype2-file">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="all-genotypes-tournament" repetitions="5" runMetricsEveryStep="true">
    <setup>setup-world</setup>
    <go>go</go>
    <final>printout-results</final>
    <timeLimit steps="100"/>
    <enumeratedValueSet variable="infanticide-on?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-dispersal-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="females-per-group">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-of-groups">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype1-file">
      <value value="&quot;M40&quot;"/>
      <value value="&quot;M30&quot;"/>
      <value value="&quot;M26&quot;"/>
      <value value="&quot;M38&quot;"/>
      <value value="&quot;M34&quot;"/>
      <value value="&quot;M41&quot;"/>
      <value value="&quot;M45&quot;"/>
      <value value="&quot;M42&quot;"/>
      <value value="&quot;M21&quot;"/>
      <value value="&quot;M19&quot;"/>
      <value value="&quot;M37&quot;"/>
      <value value="&quot;M33&quot;"/>
      <value value="&quot;M49&quot;"/>
      <value value="&quot;M46&quot;"/>
      <value value="&quot;M16&quot;"/>
      <value value="&quot;M24&quot;"/>
      <value value="&quot;M22&quot;"/>
      <value value="&quot;M35&quot;"/>
      <value value="&quot;M6&quot;"/>
      <value value="&quot;M18&quot;"/>
      <value value="&quot;M14&quot;"/>
      <value value="&quot;M44&quot;"/>
      <value value="&quot;M48&quot;"/>
      <value value="&quot;M39&quot;"/>
      <value value="&quot;M9&quot;"/>
      <value value="&quot;M50&quot;"/>
      <value value="&quot;M29&quot;"/>
      <value value="&quot;M2&quot;"/>
      <value value="&quot;M47&quot;"/>
      <value value="&quot;M36&quot;"/>
      <value value="&quot;M15&quot;"/>
      <value value="&quot;M20&quot;"/>
      <value value="&quot;M43&quot;"/>
      <value value="&quot;M11&quot;"/>
      <value value="&quot;M8&quot;"/>
      <value value="&quot;M25&quot;"/>
      <value value="&quot;M1&quot;"/>
      <value value="&quot;M31&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype2-file">
      <value value="&quot;M40&quot;"/>
      <value value="&quot;M30&quot;"/>
      <value value="&quot;M26&quot;"/>
      <value value="&quot;M38&quot;"/>
      <value value="&quot;M34&quot;"/>
      <value value="&quot;M41&quot;"/>
      <value value="&quot;M45&quot;"/>
      <value value="&quot;M42&quot;"/>
      <value value="&quot;M21&quot;"/>
      <value value="&quot;M19&quot;"/>
      <value value="&quot;M37&quot;"/>
      <value value="&quot;M33&quot;"/>
      <value value="&quot;M49&quot;"/>
      <value value="&quot;M46&quot;"/>
      <value value="&quot;M16&quot;"/>
      <value value="&quot;M24&quot;"/>
      <value value="&quot;M22&quot;"/>
      <value value="&quot;M35&quot;"/>
      <value value="&quot;M6&quot;"/>
      <value value="&quot;M18&quot;"/>
      <value value="&quot;M14&quot;"/>
      <value value="&quot;M44&quot;"/>
      <value value="&quot;M48&quot;"/>
      <value value="&quot;M39&quot;"/>
      <value value="&quot;M9&quot;"/>
      <value value="&quot;M50&quot;"/>
      <value value="&quot;M29&quot;"/>
      <value value="&quot;M2&quot;"/>
      <value value="&quot;M47&quot;"/>
      <value value="&quot;M36&quot;"/>
      <value value="&quot;M15&quot;"/>
      <value value="&quot;M20&quot;"/>
      <value value="&quot;M43&quot;"/>
      <value value="&quot;M11&quot;"/>
      <value value="&quot;M8&quot;"/>
      <value value="&quot;M25&quot;"/>
      <value value="&quot;M1&quot;"/>
      <value value="&quot;M31&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="infanticide-tournament" repetitions="10" runMetricsEveryStep="true">
    <setup>setup-world</setup>
    <go>go</go>
    <final>printout-results</final>
    <timeLimit steps="1000"/>
    <enumeratedValueSet variable="infanticide-on?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="female-dispersal-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="females-per-group">
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-of-groups">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype1-file">
      <value value="&quot;G8&quot;"/>
      <value value="&quot;G18&quot;"/>
      <value value="&quot;G19&quot;"/>
      <value value="&quot;G21&quot;"/>
      <value value="&quot;G22&quot;"/>
      <value value="&quot;G31&quot;"/>
      <value value="&quot;G42&quot;"/>
      <value value="&quot;G46&quot;"/>
      <value value="&quot;G47&quot;"/>
      <value value="&quot;G48&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="genotype2-file">
      <value value="&quot;G8&quot;"/>
      <value value="&quot;G18&quot;"/>
      <value value="&quot;G19&quot;"/>
      <value value="&quot;G21&quot;"/>
      <value value="&quot;G22&quot;"/>
      <value value="&quot;G31&quot;"/>
      <value value="&quot;G42&quot;"/>
      <value value="&quot;G46&quot;"/>
      <value value="&quot;G47&quot;"/>
      <value value="&quot;G48&quot;"/>
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
