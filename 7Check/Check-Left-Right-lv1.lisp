;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Fabrizio Chavez
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : Left-to-right-no-vbp
;;; Version     : 2
;;; 
;;; Description : Reverse checking Macronavigation strategy  
;;;                        * This file adopts the reverse search order when checking (right to left bottom to top, in this case)
;;;			  
;;;			
;;;
;;; Bugs        : * Stalling productions when a candidate is found and voted for (Look at oval,find current, attend current)  
;;;                   is firing for the second race, even though nothing is voted for.
;;;
;;;
;;;
;;;          *Changes:
;;;                 
;;;                 - notes were added for "oval-in-vote" read them, changes to all other models may be needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(P find-first-box
    =goal>
      state  start-voting
==>
    +visual-location> 
      kind image
      screen-x lowest
      screen-y lowest
    =goal>
       state attend-first-box
)


(P attend-first-box
     =goal>
       state   attend-first-box
     =visual-location>
        kind  image
     ?visual>
       state   free
==>
     =visual-location>
     +visual>
        cmd  move-attention
        screen-pos  =visual-location
     =visual-location>
     =goal>
        state find-first-race-title
)


(P find-first-race-title
   =goal>
     state find-first-race-title
==>
   +visual-location>
      kind  text
      color red
     :nearest  current
  =goal>
     state  attend-first-race-title
)



(P attend-first-race-title
   =goal>
     state  attend-first-race-title
   =visual-location>
      kind text
   ?visual>
     state free
   ?imaginal>
     state  free
==>
   +visual>
     cmd  move-attention
     screen-pos =visual-location 
   +imaginal>
     race-group  none
     candidate-group  none
     party-group     none
     checking        none
   =visual-location>
   =goal>
     state  encode-race
)


;**************************************************************************************************************
;Visual request for attending oval to be voted and then looking at current box
; 
; I added this to make a delay because the model would move vis-loc to next box THEN vote
; on the previous race THEN move visual to the next box
;
;This was causing problems for the last row because the late vote would put back vis-loc on the bubble so the 
; vis-loc failure wouldn't proc the "last-row" set of productions (since the yrequire a vis-loc failure)
;*****************************************************************************************************************

(P attend-oval-in-vote        ;Even if candidate retrieval fails, this will end up attending the current box
      =goal>                  ; because the production "abstain-nothing-found" which triggers this one
        state  find-next-race ; has a vis-loc request for the current racebox
      =imaginal>  
      =visual-location>
      ?visual>
       state  free
==>
     =visual-location>
     =imaginal>
     +visual>
       cmd move-attention
       screen-pos =visual-location
     =goal>
        state  find-current-race-box
)

(P Find-current-race-box
      =goal>
        state  Find-current-race-box
      =imaginal> 
      =visual-location>
          screen-top  =current-top
==>
     =imaginal>
     +visual-location>
        kind  image
        :nearest  current
     =goal>
        state  attend
)


(P attend-current-race-box
      =goal>
        state  attend
      =visual-location>
      ?visual>
         state  free
==>
      =visual-location>
      +visual>
        cmd  move-attention
        screen-pos  =visual-location
     =goal>
       state lego
)



    
;************************************************************
;Move to the next box to the bottom
;We come here after finding our current box (Production above)
;**************************************************************

(P find-next-race
    =goal>
      state lego
    =visual-location>
       screen-bottom  =current-bottom
       screen-right   =current-right
       screen-left    =current-left
==>
   +visual-location>
     kind  image
    > screen-top =current-bottom
      screen-right =current-right
      screen-left  =current-left
      :nearest     current 
   =goal>
     state  attend-race
)



(P attend-race                 ; LEFT OFF HERE
      =goal>
        state  attend-race
      =visual-location>
         kind image
      ?visual>
        state  free
==>
    =visual-location>
    +visual>
      cmd move-attention
      screen-pos =visual-location
    =goal>
        state  find-race-title
)



(P find-race-title
      =goal>
        state  find-race-title
      =visual-location>
         kind image
         screen-top  =current-top
         screen-bottom  =current-bottom       
==>
    +visual-location>
      kind text
      >= screen-top  =current-top
      <= screen-bottom  =current-bottom
      :nearest  current
       color red
    =goal>
      state  attend-race-title
)




(p attend-race-title
    =goal>
      state  attend-race-title
    =visual-location>
        kind  text
        color  red
    ?visual>
      state  free
    =imaginal>
==>
    =visual-location>
    +visual>
      cmd  move-attention
      screen-pos  =visual-location  
    =imaginal>                          ;Change should this be a "="
      race-group  none
      candidate-group  none
      party-group  none
      
    =goal>
       state  encode-race
)
     

; REMEMBER: you must call the find next race box "find-next-race" because thats how you have it on the
; micronavigation strategies when the model is to abstain.



;--------------------------------------------------------------------------------------------------------------------------------------
;                               This section uses Visual location failure to move down rows
;--------------------------------------------------------------------------------------------------------------------------------------
;First it finds the top-most box of the current column so it has a point of reference to go down due to visual-loc failure
; clearing your buffer contents
;--------------------------------------------------------------------------------------------------------------------------------------
(P find-current-topmost        ;YOU MIGHT NEED TO ADD A "DID I VOTE" CHECK HERE!!!!!!!!!!!!!!
   =goal>
     state  attend-race
   ?visual-location>
     buffer  failure 
==>
  +visual-location>
    kind  image
    screen-top lowest
    screen-right current
    :nearest current-x
   =goal>
     state  attend-current-topmost
)


(P attend-current-topmost                 
   =goal>
    state  attend-current-topmost
   =visual-location> 
   ?visual>
     state free 
==>
  =visual-location>
  +visual>
    cmd  move-attention
    screen-pos   =visual-location
  =goal>
    state  find-next-row-topmost
)




(P find-next-row-topmost
  =goal>
    state  find-next-row-topmost  
  =visual-location>
    ISA  visual-location        ;Is there instances where "ISA" can cause a production to not fire?
    kind  image
    screen-right  =current-right  ;This production would not fire until I added this line.. why? I never specify the variable on the LHS
==>
  +visual-location>              
    ISA  visual-location
    kind  image
    > screen-left  =current-right
      screen-x     lowest
      screen-y     lowest
      :nearest     current-x
  =goal>
    state attend-next-row-topmost
)



(P attend-next-row-topmost
   =goal>
     state  attend-next-row-topmost
  =visual-location>
  ?visual>
    state  free
==>
  +visual>
    cmd move-attention
    screen-pos  =visual-location
  =visual-location>
  =goal>
    state  find-race-title
)

;******************************************************************************************************************s
;This last production sends us back to the "find-race-title" which is the process necessary to remake the imaginal
;buffer contents empty. This then goes on to folder "encoding groups"
;********************************************************************************************************************


;--------------------------------------------------------------------------------------------------------------
;;                   Productions that trigger after the last race is reached
;--------------------------------------------------------------------------------------------------------------

;*********************************
;The model will try to move to the next column and of course it fails because there are no more rows.
; This production places the model at the first race (or middle race, depending on the checking strategy)
;******************************************

(P last-loc-failure                  ;This has model find at leftmost box for last row
   =goal> 
     state  attend-next-row-leftmost
   ?visual-location>
     buffer  failure
   =visual>               
==>
    +visual-location>
      ISA  visual-location
      kind  image
      :nearest  current-y
       screen-x lowest
   =goal>
      state  attend-last-loc-failure
)


(p attend-last-loc-failure
      =goal>
        state  attend-last-loc-failure
     =visual-location>
     ?visual>
        state  free
==>
    =visual-location>
    +visual>
      cmd  move-attention
      screen-pos   =visual-location
    =goal>
      state last-move-down
)


(P last-move-down
     =goal>
       state   last-move-down
    =visual-location>
       screen-top  =current-top
       screen-bottom  =current-bottom
       screen-left    =current-left
       screen-right  =current-right
==>
    +visual-location>
      kind  image
      > screen-left  =current-right
      > screen-bottom  =current-bottom
      > screen-top  =current-top
      :nearest  current-x
    =goal>
      state  attend-last-move-down
)



(P attend-last-move-down
    =goal>
      state  attend-last-move-down
    =visual-location>
     ISA visual-location
   ?visual>
     state  free
==>
    =visual-location>
    +visual>
     cmd  move-attention
     screen-pos  =visual-location
    =goal>
      state  find-second-last-race-title
)


(P find-second-last-race-title
      =goal>
        state  find-second-last-race-title
      =visual-location>
         kind image
         screen-top  =current-top
         screen-bottom  =current-bottom       
==>
    +visual-location>
      kind text
      >= screen-top  =current-top
      <= screen-bottom  =current-bottom
      :nearest  current
       color red
    =goal>
      state  attend-second-last-race-title
)




(p attend-second-last-race-title
    =goal>
      state  attend-second-last-race-title
    =visual-location>
        kind  text
        color  red
    ?visual>
      state  free
    =imaginal>
==>
    =visual-location>
    +visual>
      cmd  move-attention
      screen-pos  =visual-location  
    =imaginal>
      race-group  none
      candidate-group  none
      party-group  none
      last-row     true

     =goal>
       state  encode-race
)




(p find-last-race
     =goal>
       state lego
     =visual-location>
       screen-right =current-right
     =imaginal>
       last-row  true
==>
   +visual-location>
       kind  image
    > screen-left =current-right
      screen-left   lowest
       :nearest     current-y
   =imaginal>
   =goal>
     state attend-final-race
)

(P attend-final-race
      =goal>
        state  attend-final-race
      =visual-location>
         kind image
      =imaginal>
        last-row true
      ?visual>
        state  free
==>
    =imaginal>
    =visual-location>
    +visual>
      cmd move-attention
      screen-pos =visual-location
    =goal>
        state  find-final-race-title
)


(P find-final-race-title
      =goal>
        state  find-final-race-title
      =visual-location>
         kind image
         screen-top  =current-top
         screen-bottom  =current-bottom       
==>
    +visual-location>
      kind text
      >= screen-top  =current-top
      <= screen-bottom  =current-bottom
      :nearest  current
       color red
    =goal>
      state  attend-final-race-title
)




(p attend-final-race-title    ;Change - this is different as well
    =goal>
      state  attend-final-race-title
    =visual-location>
        kind  text
        color  red
    ?visual>
      state  free
    ?imaginal>
       state  free
==>
    =visual-location>
    +visual>
      cmd  move-attention
      screen-pos  =visual-location     
    =goal>
       state  encode-race
)



;;-----------------------------------------------MACRO STRAT FOR CHECKING BEGINS HERE-----------------------------------------------
;---------------------------------------------------------------------------------------------------------
;; Section 1
;Macro search strategy for fing all races in order
; This production switches us to checking
;----------------------------------------------------------------------------------------------------------

(p begin-checking
   =goal>
    state  attend-next-row-topmost
   ?visual-location>
     buffer failure
   =imaginal>
     checking none
==>
   =imaginal>
     checking  true    
   =goal>
     state c-top-most-left-most
)



;---------------------------------------------------------------------------------------------------------
;; Section 2
;Macro search strategy for checking all races in top-to-bottom-left-to-right order
; Here we just repeat the same search strategy as used for voting. 
; These 2 productions attend the very first box
;----------------------------------------------------------------------------------------------------------

(P c-top-most-left-most
    =goal>
      state  c-top-most-left-most
    =imaginal> 
      checking  true
==>
     =imaginal>
     +visual-location>
      kind  image
      screen-x lowest
      screen-y lowest
     =goal>
      state c-attend-topmost-leftmost
)


(p c-attend-topmost-leftmost
     =goal>
       state c-attend-topmost-leftmost
     =visual-location>
     ?visual>
       state free
==>
    =visual-location>
    +visual>
      cmd  move-attention
      screen-pos  =visual-location
    =goal>
      state check-if-filled
)


;---------------------------------------------------------------------------------------------------------
;; Section 3
;These move us left-to-right-top-to-bottom
;----------------------------------------------------------------------------------------------------------

(P c-global-find-current-race
   =goal>
     state no-fix-needed
   =imaginal>
     checking true
==>
   =imaginal>
   +visual-location>
     kind image
     :nearest current
   =goal>
     state c-global-attend-current-race
)


(P c-global-attend-current-race
   =goal>
     state c-global-attend-current-race
   =imaginal>
     checking true
   =visual-location>
==>
   =visual-location>
   =imaginal>
   +visual>
     cmd move-attention
     screen-pos =visual-location
   =goal>
     state c-move-to-next-race
)


(P c-move-to-next-race
    =goal>
      state c-move-to-next-race
    =visual-location>
       screen-bottom  =current-bottom
       screen-right   =current-right
       screen-left    =current-left
==>
   +visual-location>
      kind  image
    > screen-top =current-bottom
      screen-right =current-right
      screen-left  =current-left
      :nearest     current 
   =goal>
     state  c-attend-next
)

 


(P c-attend-next
      =goal>
        state  c-attend-next
      =visual-location>
         kind image
      ?visual>
        state  free
==>
    =visual-location>
    +visual>
      cmd move-attention
      screen-pos =visual-location
    =goal>
        state  check-if-filled
)


;----------------------------------------------------------------------------------------------------------------------------------------------
;; Section 4
; When the model finds something to fix it repeats the regular procedure of -> making groups -> reading title to find
; the candidate to vote for -> clicking on that candidate's bubble -> stalling for time for the click production to fire
; by looking at the oval, finding current box and attending it.
;
; Because this sequence is on the regular, non-checking productions path, it sends us to "find-next-race" instead of "c-move-to-next-race". 
; The production below grabs the process right when the stall ends and sends it to "c-move-to-next-race" insted of the regular "find-next-race"
;------------------------------------------------------------------------------------------------------------------------------------------------

(P switch-from-stalling-to-checking-behavior
   =goal>
     state lego
   =imaginal>
     checking true
==>
   =imaginal>
   =goal>
     state no-fix-needed
)
;-------------------------------------------------------------------------------------------------------------------------
;; Section 5
;If the check finds no name to retrieve and no VBP, this production fires to change back to "checking" mode
; instead of repeating all the voting productions
;------------------------------------------------------------------------------------------------------------------------
(p state-change-to-checking   ;Havent see this production fire, is it necessary?
   =goal>
     state  attend-race
   =imaginal>
     checking true
==>
   =imaginal>
   =goal>
     state no-fix-needed
)



;---------------------------------------------------------------------------------------------------------
;; Section 6
;Productions dealing with reaching the end of the column
;These productions fire when we reach the end of a column
;----------------------------------------------------------------------------------------------------------



(P c-find-current-topmost        ;Do i need to apply this change to every production?
   =goal>
     state  c-attend-next
   ?visual-location>
     buffer  failure 
   =imaginal>
     checking true
==>
  =imaginal>
  +visual-location>
    kind  image
    screen-top lowest
    screen-right current
    :nearest current-x
   =goal>
     state  c-attend-current-topmost
)


(P c-attend-current-row-topmost                 
   =goal>
    state  c-attend-current-topmost
   =visual-location> 
   ?visual>
     state free 
   =imaginal>
     checking true
==>
  =imaginal>
  =visual-location>
  +visual>
    cmd  move-attention
    screen-pos   =visual-location
  =goal>
    state  c-find-next-row-topmost
)




(P c-find-next-row-topmost
  =goal>
    state  c-find-next-row-topmost  
  =visual-location>
    ISA  visual-location        
    kind  image
    screen-right  =current-right  
 =imaginal>
     checking true
==>
  =imaginal>
  +visual-location>              
    ISA  visual-location
    kind  image
    > screen-left  =current-right
      screen-x     lowest
      screen-y     lowest
      :nearest     current-x
  =goal>
    state c-attend-next-row-topmost
)



(P c-attend-next-row-topmost
   =goal>
     state  c-attend-next-row-topmost
  =visual-location>
  ?visual>
    state  free
 =imaginal>
     checking true
==>
  =imaginal>
  +visual>
    cmd move-attention
    screen-pos  =visual-location
  =visual-location>
  =goal>
    state  check-if-filled
)












      