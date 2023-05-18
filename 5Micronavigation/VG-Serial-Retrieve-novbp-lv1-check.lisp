;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : VG-Serial-Retrieve-Party-NOVBP
;;; Version     : r2
;;; 
;;; Description : Micronavigation Strategy - to be used with the "left-to-right-no-vpb-check" macronavigation
;;;             : * Doesn't have a vote by part procedure in order to cause errors
;;;             : * If initial retrieval fails then it will move on to the next race and leave the bubble blank
;;; 
;;; Bugs        : * None known
;;;
;;; To do       : * Check for useless productions 
;;;             : Look at left-to-right-no-vbp and consider a work around the whole stalling productions still firing
;;;                 even when there was a retrival failure during regular voting and the fact that no stalling happens
;;;                 during retrival failure in checking behavior which can then lead to the wrong race being looked at after
;;;                 when at the end of a row because of the "vis-loc" failure which leads the model to find the leftmost race
;;;                 in the same column but ends up looking at the wrong one because the "visual" contents are not on the current box
;;;                 but rather something else (probably the race title since its the last thing attended before the vis-loc failure)
;;;                 This caused problems when checking race #6 in the Wisconsin ballot because the model would try to find the leftmost
;;;                 race of the same row after a vis-loc failure of findin a race to the right, so its left hand side used :nearest current
;;;                 which uses "visual" contents and because the current race wasnt looked at, the visual contents were of the race title
;;;                 (because retrieval failure is the last thing that was attended prior to the vis-loc failure).        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;**************************************************************************************
;We now use the groups created in "encoding" folder to guide our "visual-location" 
;**************************************************************************************
(p minav_find-contest-description
     =goal>
       state found-contest-description
    =imaginal>
       race-group =val1
==>
   =imaginal>
   +visual-location>
      kind text
      group =val1
  =goal>
     state attend-contest-description
)


(P minav_attend-contest-description
   =goal>
      state attend-contest-description
   =visual-location>
   ?visual>
     state  free
==>
   =visual-location>
   +visual>
     cmd  move-attention
     screen-pos  =visual-location
   =goal>
     state do-i-abstain
)

;******************************************************************************************
; Here the model checks if it should abstain from this race by checking declarative memory
; If it does it will just go to the next box
;******************************************************************************************

(P do-i-abstain             ;THIS ALWAYS FAILS FOR THIS MODEL CUZ YOU ARE MEANT TO NEVER ABSTAIN FOR THIS MODEL
  =goal>
    state do-i-abstain
  ?retrieval>
    state  free
  =visual>
    value =textval
  =imaginal>
==>
  +retrieval>
     contest =textval
  =imaginal>
     race  =textval
  =visual>               ;last thing u added
  =goal>
    state  checking-contest

)


(p abstain
   =goal>
     state  checking-contest 
   =imaginal>    ;If I don't have this check the retrieval failure from above stops happening and this production fires.. why?
     contest =this       
     ;these are 2 seperate productions so why does it stop failing when this is introduced
==>                  ;the production below DEPENDS on this failing so it stops it from happening
    =goal>
       state find-next-race
)


;**************************************************************************************
; If the model doesn't need to abstain we continue with the following productions
; These look at the race title and see if they know a candidate they want to vote for based on race title
; The model knows this by retrieving information from declarative memory (the "memory" folder)
;
;; KEEP IN MIND: The model can fail to retrieve candidate name cuz of memory strategy (roll-off, all-perf)
;; There is a chance that the model doesn't retrieve candidate name even if it exists
;; The productions that deal with such failure are after this section
;**************************************************************************************

(P minav-retrieve-info-from-declarative
    =goal>
      state checking-contest
    ?retrieval>
      buffer failure
    =imaginal>
      race  =racetitle
==>
    =imaginal>
    +retrieval>
       race =racetitle
    =goal>
      state  move-info-to-imaginal
)
    


(P minav-info-to-imaginal
     =goal> 
       state move-info-to-imaginal
     =retrieval>
       name =n
       party =p
    =imaginal>
==>
    =imaginal>
       name =n
       party =p
    =goal>
      state  search-for-candidate
)


    
;*******************************************************************************************************
;If no candidate is retrieved after reading the race then we abstain.
; This is signified by a retrieval failure from "MINAV-RETRIEVE-INFO-FROM-DECLARATIVE" - which is above
;*******************************************************************************************************
(p no-candidate-retrieved
     =goal> 
       state  move-info-to-imaginal
     ?retrieval>
       buffer  failure
==> 
    =goal>
      state  move-to-abstain
)


(p abstain-nothing-found
    =goal>
      state move-to-abstain
==>
   +visual-location>
       kind image
      :nearest  current
   =goal> 
     state  find-next-race
)

       

(p no-candidate-found      ;Is this even needed
   =goal>
     state  abstain
==>
   =goal>
     state find-next-race
)
     

;**************************************************************************************
; Now that the model knows the candidate its gonna vote for on this race we begin to search for him/her
;**************************************************************************************

(P mnav-search-for-candidate
   =goal>
     state search-for-candidate
   =imaginal>
    candidate-group =grp2
==>
    =imaginal>
    +visual-location>
      group  =grp2
;kind  text
     > screen-y  current
       screen-y  lowest
;screen-x  lowest
     =goal>
        state attend-candidate-found
)



(P mnav_attend-candidate-found
   =goal>
     state  attend-candidate-found
   =visual-location>
   ?visual>
      state  free
==>
   =visual-location>
   +visual>
      cmd  move-attention
      screen-pos  =visual-location
   =goal>
     state check-if-intended-candidate
)
         
    
;**************************************************************************************
;We found a candidate 
;Now we check if this bubble represents the candidate we wantto vote for
;**************************************************************************************

(P check-if-intended-candidate
    =goal>
      state  check-if-intended-candidate
    =visual>
      value =name
      screen-pos  =pos
    =imaginal>
      name  =name
==>
   =visual>
   =imaginal>
   +manual>      ;do i even need this here
    loc =pos
   =goal>
     state  begin-click-productions
)
   

;**************************************************************************************
;If the currently looked at candidate is not the intended one we go back to searching
;for him/her
;**************************************************************************************

(P not-intended-candidate
    =goal>
     state  check-if-intended-candidate
    =visual>
      value  =name
    =imaginal>
     - name  =name
==>
    =imaginal>
    =visual>
    =goal>
     state  search-for-candidate
)


(p no-candidate-recognized-move-on
     =goal>
       state   attend-candidate-found
     ?visual-location>
       buffer failure
==>
    =goal>
      state  move-to-abstain
)
                         


;; ---------------------------------- Productions that check if we have bubbled in or not----------------------------------
;**********************************************************************************************************************
;                                      CHECKING MICRO STRAT BEGINS HERE
;************************************************************************************************************************
;; ---------------------------------- Productions that check if we have bubbled in or not----------------------------------
(p check-if-bubble
    =goal>
      state  check-if-filled
    =visual-location>
      screen-right =current-right
      screen-left  =current-left
      screen-bottom  =current-bottom
      screen-top     =current-top
      
==>
    +visual-location>
    > screen-right =current-left
    < screen-left  =current-right
     < screen-top   =current-bottom
     > screen-bottom  =current-top
   kind oval
   color  black
    =goal>
     state c-attend-bubbled-in
)

(p c-attend-bubbled-in
     =goal>
       state c-attend-bubbled-in
     =visual-location>
     ?visual>
       state free
==>
    =visual-location>
    +visual>
      cmd  move-attention
      screen-pos  =visual-location
    =goal>
      state no-fix-needed
)


;**********************************************************************************************************************
;This deals with no black bubble being found. 
; - checks to see if it should have voted for someone off memory
; - This first part remakes the necessary groups
;***********************************************************************************************************************

(P c-look-at-currentbox-to-get-info
   =goal> 
     state  c-attend-bubbled-in
   ?visual-location>
     buffer failure
==>
   +visual-location>
     kind image
     :nearest current
   =goal>
     state c-race-title
)
      
(P c-race-title
      =goal>
        state  c-race-title
      =visual-location>
        kind image
        screen-top =current-top
        screen-bottom =current-bottom
==>
    +visual-location>
        kind text
       >= screen-top  =current-top
       <= screen-bottom  =current-bottom
        :nearest  current
         color    red
     =goal>
      state  c-attend-race-title
)



(p c-attend-race-title
    =goal>
      state  c-attend-race-title
    =visual-location>
    ?visual>
      state  free
==>
    =visual-location>
    +visual>
      cmd  move-attention
      screen-pos  =visual-location  
     =goal>
       state  encode-race
)
   
;**************************************************************************************************************************************************
; These productions deal with:
;  - No bubble was found so we went to regular voting productions (encode groups, read race title, retrieve who to vote for)
;     but model failed to retrieve a candidate.

;We use this production, as opposed to the regular one, because it sends us to the checking
;     behavior productions as opposed to continuing the regular ones. 
;
;It also has the model attend the current race after the retrieval failure so the information in the visual buffer is 
;    the current box as opposed to the race title. We need this because when the model goes on to look at the new 
;    column those productions use the coordinates of the current box. If the visual buffer as info from the race title 
;    some errors will occur (such as when race #6 in Wisconsin ballot has a retrieval failure during checking and so it
;    goes to the left most race of the row above instead of the current row. 
;*************************************************************************************************************************************************

(p c-abstain-nothing-found      ;This is the abstain if nothing is retrieved when checking
    =goal>                      ; This looks at the current race for reasons explained below
      state move-to-abstain
    =imaginal>
      checking  true
==>
   =imaginal>
   +visual-location>
       kind image
      :nearest  current
   =goal> 
     state  no-fix-needed
)



;Production Parameters
(spp Select-Choice_Imaginal-Match-Stop :u 1000)
(spp Select-Choice_Search-Screen-Ordered :u 8)
(spp check-contest :u 4000)

