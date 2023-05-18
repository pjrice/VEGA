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















      