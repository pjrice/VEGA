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
;;; Filename    : VG-Serial-Retreive-Recognize-Party.lisp
;;; Version     : r2
;;; 
;;; Description : Micronavigation Strategy
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
;;; Previously named SearchWithRace_InOrder_VBModel.lisp 
;;;
;;; This model does a serial search down the list of candidates until it finds the
;;; one that matches a name in memory.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(P Select-Choice_Locate-Contest-Description

=goal>
	ISA       MakeVote
	state     ready-to-make-choice
	

=imaginal>
	race-group  =val1
	
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
	
!eval! (setf current-strat 'retrieval)

)

;****************************************

(P Select-Choice_Attend-Contest-Description

=goal>
	ISA       MakeVote
	state     found-contest-description
	

=visual-location>
	ISA       visual-location
	kind      =text
	group     =val1

?visual>
	state     free

==>

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
	ISA       MakeVote
	state     attended-contest-description
	

=visual>
	ISA       text
	value     =textVal

?retrieval>
	state     free

=imaginal>

==>

+retrieval>
	ISA       Abstain
	contest   =textVal

=imaginal> ;encoding contest in imaginal buffer for a later check if it goes past end state
	ISA       MakeVote
	race      =textVal 

=goal>
	state     checking-contest

)

;****************************************

(P abstain

=goal>
	ISA      MakeVote
	state 	 checking-contest
	

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
	ISA       MakeVote
	state     checking-contest
	

?retrieval>
	buffer    failure

=imaginal>
	ISA       MakeVote
	race      =textVal

==>

+retrieval>
	ISA       Candidate
	race      =textval  

=imaginal>

=goal>
	state    encoded-contest-description

!output! ("Contest is: ~s" =textVal)
)



;****************************************
; Successful retrieval of candidate to vote for

(P Initial-Retrieval-Success

=goal> 
	ISA     MakeVote
	state   encoded-contest-description
	

=retrieval>
	ISA     Candidate
	race    =r
	name    =n

=imaginal>

==>

=imaginal> ;moving the info in retrieval to imaginal buffer for later
	ISA         MakeVote
	race        =r
	candidate   =n

=goal>
	state	   search-screen-retrieval

!output! ("I'm voting for: ~s" =n)

)

;****************************************

(p Select-Choice_Search-Screen-Ordered_Retrieval

=goal>
	ISA     MakeVote
	state   search-screen-retrieval
	

?visual-location>
	state     free

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
	state     something-found-retrieval
  
)

;****************************************

(P Select-Choice_Attend-Search_Retrieval

=goal>
	ISA       MakeVote
	state     something-found-retrieval
	

=visual-location>
	ISA       visual-location
	kind      text
	group     =val2

?visual>
	state     free

==>

+visual>
	ISA          move-attention
	screen-pos   =visual-location


=goal>
	state     attending-something-found-retrieval

)

;****************************************

(P Select-Choice_Encode-Search_Retrieval

=goal>
	ISA     MakeVote
	state   attending-something-found-retrieval
	

=visual>
	ISA          text
	value        =val
	screen-pos   =pos

?visual-location>
	state        free

==>

=visual>

=goal>
	state	 encoded-search-retrieval

!output! ("Looking at candidate: ~s" =val)

)

;****************************************

(P  Select-Choice_Imaginal-Match-Stop_Retrieval

=goal>
	ISA         MakeVote
	state       encoded-search-retrieval
	

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
	state    moved-to-candidate

)

;****************************************
;If the name in the visual location does not match
; candidate saved in imaginal, look for another name 

(P  Select-Choice_No-Match_Retrieval

=goal>
	ISA       MakeVote
	state     encoded-search-retrieval
	

?visual-location>
	state	  free

==>

=goal>
	state    search-screen-retrieval

)

;****************************************
;
;Deal with retrieval failure, 
;initiates recognition strategy
;
;****************************************

;****************************************
;Model has read contest description but retrieval of candidate has failed 

(P Retrieval-Fails

=goal>
	ISA 	MakeVote
	state 	encoded-contest-description
	

?retrieval>
	buffer	failure

==>

=goal>
	state	search-screen-recognition

!output! ("Initial retrieval fails, switch to recog strategy.")
!eval! (setf current-strat 'recognition)

)

;****************************************
; Model has read contest description but retrieval of candidate has failed

(P Retrieval-Fails-after-searching

=goal>
	ISA     MakeVote
	state   something-found-retrieval
	

?visual-location>
	buffer	failure

==>

=goal>
	state  search-screen-recognition

!output! ("Looked at everything and nothing retrieved--switch to recognition")
!eval! (setf current-strat 'recognition)

)

;****************************************
;Read text in order with no specific name in mind

(p Select-Choice_Search-Screen-Ordered_Recognition

=goal>
	ISA     MakeVote
	state   search-screen-recognition
	

?retrieval>
	state   free

?visual-location>
	state   free
	
?visual>
	state   free	
	
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
	state    something-found-recognition
	
)

;****************************************
;Attend that name

(P Select-Choice_Attend-Search_Recognition

=goal>
	ISA       MakeVote
	state     something-found-recognition
	

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
	state     attending-something-found-recognition

)

;****************************************
;Search for that name in memory

(P Select-Choice_Encode-Search_Recognition

=goal>
	ISA       MakeVote
	state     attending-something-found-recognition
	

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
	state     encoded-search-recognition

!output! ("Looking at Candidate: ~s" =val)

)

;****************************************
;See if the name in the visual location matches
;the name in memory

(P  Select-Choice_Match-Stop_Recognition

=goal>
	ISA       MakeVote
	state     encoded-search-recognition
	

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

(P  Select-Choice_No-Match_Recognition

=goal>
	ISA       MakeVote
	state     encoded-search-recognition
	

?visual-location>
	state     free

?retrieval>
	buffer      failure

==>

=retrieval>

=goal>
	state      search-screen-recognition

!output! ("Name does not match. Read another.")

)

;****************************************

; Productions that handle recognition failure 
; Switches to voting by party
; VBP = Vote By Party

;****************************************
;; Model has read contest description but recognition of candidate has failed as well
; initiates vote-by-party strategy

(P Recognition-Fails

=goal>
	ISA 	MakeVote
	state 	something-found-recognition
	

?visual-location>
	buffer	failure

==>

=goal>
	state	search-by-party

!output! ("Looked at everything and nothing recognized-- voting by party")
!eval! (setf current-strat 'party)

)

;****************************************
;; current visual location is at the top of the list (Production fires if initial retrieval fails)
;; looking at party

(P VBP-Search-Screen-Top-Down

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
	ISA         visual-location
	kind        text
	group       =val3
	:attended    nil
	screen-y     lowest
  
=goal>
	state    vbp-something-found
  
)

; same production that fires if current visual location is at the bottom of the list
; this happens if it initial retrieval succeeds, but fails during search

(P VBP-Search-Screen-Bottom-Up

=goal>
	ISA     MakeVote
	state   search-by-party-at-bottom
	

?retrieval>
	state	free

?visual-location>
	state	free

=imaginal>
	party-group  =val3

==>

=imaginal>

+visual-location>
	ISA         visual-location
	kind        text
	group       =val3
	< screen-y  current
 	screen-y    highest
 	screen-x    lowest

=goal>
	state    vbp-something-found
  
)

;****************************************
; looks at party
(P VBP-Select-Choice_Attend-Search

=goal>
	ISA       MakeVote
	state     vbp-something-found
	

=visual-location>
	ISA       visual-location
	kind      text
	group     =val3

?visual>
	state     free

==>

+visual>
	ISA          move-attention
	screen-pos   =visual-location

=goal>
	state     vbp-attending-something-found

)

;****************************************
(P VBP-Encode-Search-Match

=goal>
	ISA      MakeVote
	state    vbp-attending-something-found
	
	default  =party

=visual> 
	ISA         text
	value       =party
	screen-pos  =pos

?manual>
	state	 free

==>

+manual>
	ISA     move-cursor
	loc     =pos

=goal>
	state   moved-to-candidate

!output! ("Party matches: ~s" =party)

)

;****************************************
;; if visual item doesn't match what we're looking at, then search again
(P VBP-No-Match-Next-Choice

=goal>
	ISA      MakeVote
	state    vbp-attending-something-found
	
	default  =party

=visual>
	ISA      text
	- value  =party
	value    =notparty

==>

=goal>
	state	search-by-party

!output! ("Party ~s does not match, search again." =notparty)

)

;****************************************
; if we reach bottom and nothing has been retrieved then vote by party

(P VBP-bottom-of-list-fail

=goal>
	ISA     MakeVote
	state   vbp-something-found
	

?visual-location>
	buffer	failure

==>

+visual-location>

=goal>
	state	find-next-race
	

)

;Production Parameters
(spp Select-Choice_Imaginal-Match-Stop_Retrieval :u 1000)
(spp Select-Choice_Search-Screen-Ordered_Retrieval :u 8)
(spp Select-Choice_Search-Screen-Ordered_Recognition :u 8)
(spp check-contest :u 4000)
