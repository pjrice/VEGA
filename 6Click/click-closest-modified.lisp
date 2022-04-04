;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Joshua Engels
;;; Copyright   : (c) 2019 Joshua Engels
;;; Address     : Lovett College 
;;;             : Rice University
;;;             : Houston, TX 77005
;;;             : jae4@rice.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : click-closest.lisp
;;; Version     : 1
;;; 
;;; Description : Click Strategy
;;;				: * Clicks the button in this race group that is closest to the currently attended position
;;;
;;; Bugs        : 
;;;
;;; To do       : 
;;;
;;; ----- History -----
;;; 2019.4.28   Joshua Engels
;;;				: * Documented the file
;;; 2022.4.4	Patrick Rice
;;;				: * Modified move-mouse-to-bubble so that the visual-location buffer is not harvested (it is
;;;					needed for find-race-title-box)
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;   
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Finds the bubble closest to the currently attended location
(P find-bubble

=goal>
	state		moved-to-candidate
	
==>

+visual-location>
	ISA			visual-location
	kind		oval
	< screen-x	current
	:nearest	current

=goal>
	state		attending-bubble

)


; Moves the mouse to the bubble
(P move-mouse-to-bubble

=goal>
	state		attending-bubble
	
?manual>
	state		free
	
=visual-location>
	ISA			visual-location
	kind		oval
	
==>

=visual-location> ; added 4/4/22 by PJR because find-race-title-box was not firing after click-bubble had fired, due to empty visual-location buffer

+manual>
	ISA     move-cursor
	loc     =visual-location


=goal>
	state	moving-mouse-to-bubble
	

)

; Clicks the bubble and sets the state to find-next-race
(P click-bubble

=goal>
	state     moving-mouse-to-bubble

?manual>
	state     free

==>

+manual>
	ISA     click-mouse

=goal>
	state 	find-next-race

)
