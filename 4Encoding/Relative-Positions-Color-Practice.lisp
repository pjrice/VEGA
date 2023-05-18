
; This version doesn't have a 'check if already voted' production
; You also don't have "clear all finsts" for your last production. Normally attend-party then another production for clearing all finsts
; You also removed the action of looking at the bubbles and encoding that into bubble group

(p encode-race
   =goal>
     state encode-race
  =visual-location>
     group =group1
  =imaginal>
==>
   =visual-location>
   =imaginal>
    race-group =group1
   =goal>
     state find-candidate-for-grouping
)



(p find-candidate-for-grouping
     =goal>
       state  find-candidate-for-grouping
     =visual-location>
       kind text
==>
    +visual-location>
      kind text
      color  purple
      :nearest current
       > screen-y  current
    =goal>
      state  attend-encode-candidate-grp
)


(p attend-encode-candidate-grp
    =goal>
      state  attend-encode-candidate-grp
    =visual-location>
      group =group2
    ?visual>
       state free
    =imaginal>
==>
    =visual-location>
    +visual>
      cmd move-attention
      screen-pos =visual-location
    =imaginal>
      candidate-group =group2
    =goal>
      state find-party-for-encoding
)
       
       
(p find-party-for-encoding
    =goal>
      state find-party-for-encoding
    =visual-location>
        kind  text
==>
     +visual-location>
       kind text
       color blue
       > screen-x current
       :nearest current
     =goal>
       state  encode-attend-party-group
)



(p encode-attend-party-group
   =goal>
      state  encode-attend-party-group
   =visual-location>
     group =group3
   =imaginal>
  ?visual>
    state free
==>
    +visual>
      cmd  move-attention
      screen-pos =visual-location
    =imaginal>
     party-group =group3
    =visual-location>
    =goal>
       state  found-contest-description
)
         
 ;(p clear-all-finsts
;   =goal> 
;      state clear-all-finsts
;==>
;   +visual>
;     ISA clear-all-finsts
;   =goal>
;    state found-contest-description
;)

   
