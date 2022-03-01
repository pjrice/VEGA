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
;;; Filename    : VG-Serial-Retreive-Party.lisp
;;; Version     : r2
;;; 
;;; Description : Micronavigation Stragety
;;;             : * Uses a retrieval-based serial strategy to search for candidates by name.
;;;             : * Model abstains if it reaches the bottom and no name has matched
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
;;; The model does a single retrieveal from DM for each race and then does a random 
;;; search for the string that was retrieved from memory. Votes by party if retrieval fails.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;****************************************

(P Select-Choice_Locate-Contest-Description

=goal>
	ISA       MakeVote
	state     ready-to-make-choice
	

=imaginal>
	race-group   =val1

==>

=imaginal>

+visual-location>
	ISA       visual-location
	kind      text
	group     =val1

=goal>
	state     found-contest-description
	
!eval! (setf current-strat 'retrieval)

)

;****************************************

(P Select-Choice_Attend-Contest-Description

=goal>
	ISA       MakeVote
	state     found-contest-description

=imaginal>	

=visual-location>
	ISA       visual-location
	kind      =text
	group     =val1

?visual>
	state     free

==>

=imaginal>

+visual>
	ISA          move-attention
	screen-pos   =visual-location

=goal>
	state        attended-contest-description

)

;****************************************
; the following two productions check if model should abstain from contest
(P check-contest

=goal>
	ISA      MakeVote
	state    attended-contest-description

=visual>
	ISA      text
	value    =textVal

?retrieval>
	state    free

=imaginal>

==>

+retrieval>
	ISA      Abstain
	contest  =textVal

=imaginal> ; encoding contest in imaginal buffer for a later check if it goes past end state
	ISA      MakeVote
	race     =textVal 

=goal>
	state    checking-contest

)

;****************************************

(P abstain

=goal>
	ISA      MakeVote
	state	 checking-contest
	

=retrieval>
	ISA      Abstain
	contest  =this

==>

; sends to navigation production
=goal>
	ISA 	MakeVote
	state	find-next-race
	

	

)

;****************************************

(P Select-Choice_Encode-Contest-Description

=goal>
	ISA     MakeVote
	state   checking-contest
	

?retrieval>
	buffer  failure

=imaginal>
	ISA     MakeVote
	race    =textVal
       
==>

+retrieval>
	ISA      Candidate
	race     =textval 

=imaginal>

=goal>
	state    encoded-contest-description

!output! ("Contest is: ~s" =textVal)

)


;****************************************
; Successful retrieval of candidate to vote for

(P Retrieval-Success

=goal> 
	ISA     MakeVote
	state   encoded-contest-description
	

=retrieval>
	ISA      Candidate
	race     =r
	name     =n
	
=imaginal>

==>

;=retrieval>

=imaginal> ;moving the info in retrieval to imaginal buffer for later
	ISA         MakeVote
	race        =r
	candidate   =n

=goal>
	state	    search-screen

!output! ("I'm voting for: ~s" =n)

)

;****************************************
(P Select-Choice_Search-Screen-Fastest

=goal>
	ISA       MakeVote
	state     search-screen
	

?visual-location>
	state     free
 
=imaginal> 
	candidate-group  =val2

==>

=imaginal> 
 
+visual-location>
	ISA           visual-location
	group         =val2
	kind          text
	:attended     nil
	
=goal>
	state  	  something-found

)

;****************************************

(P Select-Choice_Attend-Search

=goal>
	ISA      MakeVote
	state    something-found
	

=visual-location>
	ISA       visual-location
	kind      text
	group     =val2

?visual>
	state     free

==>

+visual>
	ISA         move-attention
	screen-pos  =visual-location

=goal>
	state       attending-something-found

)

;****************************************
;Search for that name in memory

(P Select-Choice_Encode-Search

=goal>
	ISA       MakeVote
	state     attending-something-found
	

=visual>
	ISA          text
	value        =val
	screen-pos   =pos

?visual-location>
	state        free

==>

=visual>

=goal>
	state     encoded-search

!output! ("Looking at Candidate: ~s" =val)

)


;****************************************
;See if the name in the visual location matches
;any name in memory

(P Select-Choice_Imaginal-Match-Stop

=goal>
	ISA       MakeVote
	state     encoded-search
	

=imaginal>
	ISA         MakeVote
	candidate   =val

=visual> 
	ISA           text
	value         =val
	screen-pos    =pos

?visual-location>
	state     free

?manual>
	state     free

==>

=imaginal>

=visual>

+manual>
	ISA     move-cursor
	loc     =pos

=goal>
	state   moved-to-candidate

;!STOP!

)

;****************************************
; works now

(P Select-Choice-No-Match

=goal>
	ISA       MakeVote
	state     encoded-search
	

?visual-location>
	state     free

==>

=goal>
	state     search-screen

)


;****************************************

; Productions that handle retrieval failure (i.e. when retrieval of candidate fails)
; switches to random voting by party
; VBP = Vote By Party

;****************************************
;; Model has read contest description but retrieval of candidate has failed

(P VBP-Retrieval-Fails

=goal>
	ISA 	 MakeVote
	state 	 encoded-contest-description
	

?retrieval>
	buffer	 failure

==>

=goal>
	state	search-by-party

!output! ("Initial retrieval failure, voting by party")
!eval! (setf current-strat 'party)

)

;****************************************
;OR
; Initial retrieval worked but model looked at everything and retrieval match failed

(P VBP-Retrieval-Fails-after-searching

=goal>
	ISA 	MakeVote
	state 	something-found
	

?visual-location>
	buffer	failure 

==> 

=goal>
	state	search-by-party

!output! ("Looked at everything and nothing retrieved-- voting by party")
!eval! (setf current-strat 'party)

)

;****************************************
;; performs another random search, looking for party affiliation

(P VBP_Search-Screen-Random

=goal>
	ISA     MakeVote
	state   search-by-party
	

?retrieval>
	state	free

?visual-location>
	state	free

=imaginal> 
	party-group  =val3

==>

=imaginal> 

+visual-location>
	ISA          visual-location
	kind         text
	group        =val3
	:attended    nil

=goal>
	state     vbp-attend-name

)

;****************************************
; something found, moves attention to that location

(P Vote-by-Party_Attend-Location

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
	ISA         move-attention
	screen-pos  =visual-location

=goal>
	state	party-found

)

;****************************************
; a name was found and matches with the default party

(P Vote-by-Party_Match

=goal>
	ISA        MakeVote
	state      party-found
	
	default	   =party

=visual>
	ISA         text
	value       =party
	screen-pos  =pos

?manual>
	state	    free

==> 

=visual>

+manual>
	ISA     move-cursor
	loc     =pos

=goal>
	state	moved-to-candidate

!output! ("Party found and matches: ~s" =party)

)

;****************************************
; production fires if default party does not match
; returns goal state to search again

(P Vote-by-Party_No-Match

=goal>
	ISA      MakeVote
	state	 party-found
	
	default	 =p

=visual>
	ISA      text
	- value  =p
	value    =q

==>

=goal>
	state	search-by-party

!output! ("Party ~s does not match default. Search again." =q)

)




;****************************************
; if the model has looked at the entire list and cannot recall default party 
; abstain

(P Vote-by-Party_No-Match-ABSTAIN

=goal>
	ISA     MakeVote
	state   vbp-attend-name
	

?visual-location>
	buffer	failure

==>

+visual-location>

=goal>
	state	find-next-race
	

	

!output! ("Looked at all parties and nothing matches-- Abstain.")

)

;Production Parameters
(spp Select-Choice_Imaginal-Match-Stop :u 1000)
(spp Select-Choice_Search-Screen-Fastest :u 8)
(spp check-contest :u 4000)

