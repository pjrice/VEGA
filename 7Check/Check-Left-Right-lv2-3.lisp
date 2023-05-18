






;;-----------------------------------------------MACRO STRAT FOR CHECKING BEGINS HERE-----------------------------------------------
;---------------------------------------------------------------------------------------------------------
;; Section 1
;Macro search strategy for fing all races in order
; This production switches us to checking
;----------------------------------------------------------------------------------------------------------

(p begin-checking                    ;moving to the next column fails because there are no columns left so we go to checking
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
      state c-find-first-race-title
)


(P c-find-first-race-title
   =goal>
     state c-find-first-race-title
==>
   +visual-location>
      kind  text
      color red
     :nearest  current
  =goal>
     state  c-attend-first-race-title
)

(P c-attend-first-race-title
   =goal>
     state  c-attend-first-race-title
   =visual-location>
      kind text
   ?visual>
     state free
   =imaginal>
     checking true
==>
   +visual>
     cmd  move-attention
     screen-pos =visual-location 
   =imaginal>
     race-group  none
     candidate-group  none
     party-group     none 
   =visual-location>
   =goal>
     state  encode-race
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
        state  c-find-race-title
)

(P c-find-race-title
      =goal>
        state  c-find-race-title
      =visual-location>
         kind image
         screen-top  =current-top
         screen-bottom  =current-bottom    
      =imaginal>
        checking true   
==>
    =imaginal>
    +visual-location>
      kind text
      >= screen-top  =current-top
      <= screen-bottom  =current-bottom
      :nearest  current
       color red
    =goal>
      state  c-attend-race-title
)



(p c-attend-race-title
    =goal>
      state  c-attend-race-title
    =visual-location>
        kind  text
        color  red
    ?visual>
      state  free
    =imaginal>
      checking true
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




;******************************************************************************
;; Section 4
;Visual request for attending oval to be voted and then looking at current box while unclicking the current bubble
; during checking behavior
; 
; I added this to make a delay because the model would move vis-loc to next candidate -> attend that candidate 
;  THEN unclick the box it was trying to unclick
;******************************************************************************


(P c-attend-wrong-oval-in-vote
      =goal>
        state  c-attend-wrong-oval-in-vote
      =imaginal>  
        checking true
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
        state  c-stall-find-candidate-next-to-bubble
)

(P c-stall-find-candidate-next-to-bubble
   =goal>
     state c-stall-find-candidate-next-to-bubble
   =visual-location>
     screen-top =current-top
     screen-bottom =current-bottom
   =imaginal>
    candidate-group =grp2
    checking true
==>
    =imaginal>
    +visual-location>
      group  =grp2
      screen-top =current-top
      screen-bottom =current-bottom
      :nearest current
     =goal>
        state c-stall-attend-candidate-next-to-bubble
)
 


(P c-stall-attend-candidate-next-to-bubble
   =goal>
     state c-stall-attend-candidate-next-to-bubble
   =visual-location>
   ?visual>
      state  free
   =imaginal>
     checking true
==>
   =imaginal>
   =visual-location>
   +visual>
      cmd  move-attention
      screen-pos  =visual-location
   =goal>
     state c-search-for-candidate
)



;---------------------------------------------------------------------------------------------------------
;; Section 5
; When the model finds something to fix it repeats the regular procedure of -> making groups -> reading title to find
; the candidate to vote for -> clicking on that candidate's bubble -> stalling for time for the click production to fire
; by looking at the oval, finding current box and attending it. Because this sequence is on the regular, non-checking
; productions path, it sends us to "find-next-race" instead of "c-move-to-next-race". 
;
; The production below grabs the process right when the stall ends and sends us to "c-move-to-next-race"
;----------------------------------------------------------------------------------------------------------

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
;; Section 6
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
;; Section 7
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
    state  c-find-race-title
)












      