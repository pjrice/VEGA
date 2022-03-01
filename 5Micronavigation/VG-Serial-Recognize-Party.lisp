;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne [and others]
;;; Copyright   : (c) 2019 Mike Byrne
;;; Address     : Department of Psychology 
;;;             : Rice University
;;;             : Houston, TX 77005
;;;             : byrne@rice.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : VG-Serial-Recognize-Party.lisp
;;; Version     : r2
;;; 
;;; Description : Micronavigation Strategy
;;;             : * Uses a recognition-based memory strategy
;;;             : * Uses serial visual search strategy.
;;;             : * Retrieval fails randomly if name is under the activation threshold.
;;;             : * If retrieval fails, performs a serial search by party
;;; 
;;; Bugs        : * None known
;;;
;;; To do       : * 
;;; 
;;; ----- History -----
;;; 2019.4.22   Joshua Engels
;;;				: * Cut and transformed the micronavigation code (see conversion.txt) from the file of the same name as this one for use in constructing full models
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; See old voting box model file of the same name for further editing history
;;; 
;;; As noted above, this model searches for candidates by working down the list.
;;; Retrieval will fail randomly and if this happens, the model will select a candidate
;;; based on party affiliation.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;****************************************
;****************************************
;start to vote
;****************************************
;****************************************

;****************************************


;****************************************
;Put the visual location somewhere on the screen

(P Select-Choice_Locate-Contest-Description

=goal>
	ISA       MakeVote
	state     ready-to-make-choice
	

=imaginal>
	race-group 	 =val1
	
?visual-location>
	state     free
	
==>

=imaginal>

+visual-location>
	ISA       visual-location
	kind      text
	group     =val1

=goal>
	state    found-contest-description

!eval! (setf current-strat 'recognition)

)


;****************************************
;Attend some visual location
(P Select-Choice_Attend-Contest-Description

=goal>
	ISA       MakeVote
	state     found-contest-description
	

=visual-location>
	ISA       visual-location
	kind      text
	group     =val1

?visual>
	state     free

==>

+visual>
	ISA           move-attention
	screen-pos    =visual-location

=goal>
	state     attended-contest-description

)

;****************************************
(P encode-contest-description

=goal>
	ISA     MakeVote
	state   attended-contest-description
	

=visual>
	ISA  	text
	value   =textVal

==>

+retrieval>
	ISA       Abstain
	contest   =textVal

=goal>
	state	encoded-contest-description

!output! ("Contest is: ~s" =textVal)

)


;****************************************
; Production that fires only if contest is one to abstain from

(P check-contest

=goal>
	ISA 	MakeVote
	state	encoded-contest-description
	

=retrieval>
	ISA      Abstain
	contest  =race

==>

; sends to navigation production
=goal>
	ISA 	MakeVote
	state	find-next-race
	

	

)


;****************************************
;Read text in order with no specific name in mind
(p Select-Choice_Search-Screen-Ordered

=goal>
	ISA     MakeVote
	state   encoded-contest-description
	

?retrieval>
	buffer	failure
	
=imaginal> 
	candidate-group  =val2

==>

=imaginal> 

+visual-location>
	ISA          visual-location
	group        =val2
	kind         text
	> screen-y   current
	screen-y     lowest
	screen-x     lowest

=goal>
	state    something-found
	
)


;****************************************
;Attend that name
(P Select-Choice_Attend-Search

=goal>
	ISA       MakeVote
	state     something-found
	

=visual-location>
	ISA       visual-location
	kind      text
	group     =val2 

?visual>
	state     free

==>

+visual>
	ISA           move-attention
	screen-pos    =visual-location

=goal>
	state     attending-something-found

)

;****************************************
;Search for that name in memory
(P Select-Choice_Encode-Search

=goal>
	ISA       MakeVote
	state     attending-something-found
	

=visual>
	ISA     text
	value   =val

?visual-location>
	state     free

?retrieval>
	state     free

==>

=visual>

+retrieval>
	ISA       candidate 
	name      =val

=goal>
	state     encoded-contest

!output! ("Looking at Candidate: ~s" =val)

)

;****************************************
;See if the name in the visual location matches
;any name in memory
(P  Select-Choice_Match-Stop

=goal>
	ISA       MakeVote
	state     encoded-contest
	

=retrieval>
	ISA       candidate 
	name      =val

=visual> 
	ISA          text
	value        =val
	screen-pos   =pos

?visual-location>
	state      free

?manual>
	state      free

==>

=retrieval>

=visual>

+manual>
	ISA     move-cursor
	loc     =pos

=goal>
	state   moved-to-candidate
	
!output! ("Match! Voting for: ~s" =val)

)

;****************************************
;Deal with retrieval failure
;If the name in the visual location does not match
;anything in memory, read another name

(P  Select-Choice_No-Match

=goal>
	ISA       MakeVote
	state     encoded-contest
	

?visual-location>
	state     free

?retrieval>
	buffer      failure


==>

=retrieval>

=goal>
	state      encoded-contest-description

!output! ("Name does not match. Read another.")

)

;****************************************
;Deal with retrieval failure
;If the name in the visual location does not match
;anything in memory and it is at the bottom of the 
;name list, vote by party
;****************************************

(P  Select-Choice_No-Match-VoteByParty

=goal>
	ISA     MakeVote
	state   something-found
	

?visual-location>
	buffer    failure

==>

=visual-location>

=goal>
	state      read-by-party

!output! ("Bottom of list and nothing matches. Voting by party.")
!eval! (setf current-strat 'party)

)


;****************************************
; serially searches the screen to find a candidate's party


(P VBP-Select-Choice_Search-Screen-Ordered

=goal>
	ISA	    MakeVote
	state   read-by-party

?retrieval>
	state   free

?visual-location>
	state   free
	
?visual>
	state   free

=imaginal>
	party-group  =val3

==>

=imaginal>

+visual-location>
	ISA          visual-location
	group        =val3
	kind         text
	:attended    nil
	screen-y     lowest

=goal>
	state    vbp-attend-name
  
)
;****************************************
; something found, moves attention to that location

(P VBP-Select-Choice_Attend-Search

=goal>
	ISA     MakeVote
	state   vbp-attend-name
	

=visual-location>
	ISA     visual-location
	kind    text
	group   =val3

?visual>
	state	free

==> 

+visual>
	ISA          move-attention
	screen-pos   =visual-location

=goal>
	state	party-found

)

;****************************************
; a name was found and now we need to check if it matches with the default party

(P VBP-Encode-Search-Match

=goal>
	ISA         MakeVote
	state       party-found
	
	default     =party

=visual>
	ISA         text
	value       =party
	screen-pos  =pos

?manual>
	state	free

==>

=visual>

+manual>
	ISA     move-cursor
	loc     =pos

=goal>
	state   moved-to-candidate

!output! ("Matches party ~s" =party)

)




;****************************************
; if the name in the visual location doesn't match the default party from memory
; search again

(P Vote-by-Party_No-Match

=goal>
	ISA      MakeVote
	state    party-found
	
	default  =party

=visual>
	ISA      text
	- value  =party
	value    =notparty
	
==>

=goal>
	state	read-by-party

!output! ("Party ~s does not match default" =notparty)
!output! ("Default party is: ~s" =party)

)


;****************************************
; if the model has looked at the entire list and cannot recall default party 
; abstain

(P Vote-by-Party_No-Match-ABSTAIN

=goal>
	ISA      MakeVote
	state    vbp-attend-name
	
	
;already attended everything
?visual-location> 
	buffer	failure

==>

+visual-location>

=goal>
	state	find-next-race
	

	

!output! ("Reached the end of the search and nothing matches-- Abstain")

)

;Production Parameters
(spp Select-Choice_Search-Screen-Ordered :u 1000)
(spp check-contest :u 1000)


