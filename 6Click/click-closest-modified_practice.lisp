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
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;   
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p begin-click-productions
      =goal>
         state  begin-click-productions
==>
    +visual-location>
       kind oval
       < screen-x   current
       :nearest current
     =goal>
      state  move-mouse
)


(p move-mouse-to-bubble
    =goal>
      state  move-mouse
    =visual-location> 
       kind oval
    ?manual>
      state free
==>
    =visual-location>
    +manual>
     ISA  move-cursor
     loc  =visual-location
    =goal>
      state click-bubble
)


(P click-bubble
    =goal>
      state  click-bubble
    ?manual>
      state free
==>  
    +manual>
      ISA click-mouse
    =goal>
      state  find-next-race
)
    

   