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
;;; Filename    : DownEachColumn-Box-Instructions-1.lisp
;;; Version     : 1
;;; 
;;; Description : Macronavigation strategy
;;;				: * This strategy will handle the races from top to bottom for all columns. (start from left column, then middle, then right)
;;;				: * If starting, first finds the header. Then find the instructions in the top left corner (below the header). Then footer.
;;;				: * Then the model finds the race below the instructions.
;;;				: * Otherwise tries to find the next race in the column, or if there is 
;;;				: * no such race the top race in the next column, or if there is no such next column ends the model run.
;;;
;;; Bugs        : * None known
;;;
;;; To do       : * There are a few places where I have had to cheat to make the model work (because we do not have access to relative group 
;;;				: * positios or super and sub groups). These places are documented, but eventually it would be better if they were removed. The
;;;				: * reason these cheats are neccesary is mostly because of navigating a ballot with noise. If one is not using a ballot with noise,
;;;				: * and these cheats are causing problems, they can be removed.
;;;
;;; ----- History -----
;;; 2022.01.16   Xianni Wang
;;;				: * Add productions to accommodate 4 ballot layouts (ballot_instructions_1,2,3,4; both top-instr & left-instr)
;;; 2021.11.04   Xianni Wang
;;;				: * Modify productions to do random visual search in header, footer & instructions boxes
;;;				: * Added productions to handle bubbles in the instructions
;;; 2021.10.27   Xianni Wang
;;;				: * Modify productions to do random visual search in headers & instructions boxes
;;; 2021.10.04   Xianni Wang
;;;				: * Created the file; modified from "top-to-bottom-left-to-right-box.lisp"
;;;				: * Added productions to handle headers & instructions
;;;				: * Modify visual location buffers to make the model navigate among races & columns
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; This model can handle non race content (header, footer, top-instructions, left-instructions).
;;; This model uses the primary macronavigation strategy observed in subjects (at least when they are given an in order ballot): top to bottom and 
;;; then left to right. It only finds the piece of text in the next expected spot; it does not check if that piece of text is a race title.
;;;   
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Assumes verticles are exactly lines up (instructions and first-row-races)

; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 
; Makes a visual location request for the header
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

(P Find-Header

=goal>
	state		start-voting

?imaginal>
	state		free

==>

+visual-location>
	ISA	        visual-location
	kind		image
	screen-top      lowest
	screen-y	lowest
	
=goal>
	state		attending-header-box
)

;attend box in the left-top corner
(P Attend-Header

=goal>
	state		attending-header-box
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

==>

+visual>
	ISA	        move-attention
	screen-pos	=visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		finding-text	
	
)

; find instructions & locate box boundaries
(P Find-Header-First-Line-1

=goal>
        state   	finding-text

=visual>

=visual-location>
	ISA		visual-location	
	kind		image
	screen-top	=current-top
        screen-bottom   =current-bottom

?imaginal>
	state		free	
==>

+visual-location>
	ISA		 visual-location	
	kind		 text
        >= screen-top	 =current-top
        <= screen-bottom =current-bottom       
        :attended        nil

+imaginal>
	header-bottom           =current-bottom
        header-top              =current-top
        instr-top               none
        instr-bottom            none
        instr-left              none
        instr-right             none
        footer-top              none
        footer-bottom           none
	race-group		none
	candidate-group  	none
	party-group		none

=goal>
	state		attending-header

)


(P Attending-Header-First-Line

=goal>
	state		attending-header

=visual-location>
	ISA             visual-location
	kind            text

?visual>
	state	        free

==>

+visual>
	ISA            move-attention
	screen-pos     =visual-location

=goal>
	state          retrieving-header
)

;Search for non-race text in memory
(P Retrieving-Header-First-Line

=goal>
	state	      retrieving-header

=visual>
	ISA           text
        value	      =text
	screen-pos    =pos

?retrieval>
	state         free

==>

=visual>

+retrieval>
	ISA        instruction 
	keyword    =text

=goal>
	state          checking-header
!output! ("looking at: ~s" =text)
)

;****************************************
;text was found and matches with the instruction keywords
(P Checking-Header

=goal>
        state       checking-header

=retrieval>
	ISA        instruction 
	keyword    =text

=visual>
        value	    =text
	screen-pos  =pos

==>

=retrieval>

=visual>

=goal>
	state	    finding-instructions

!output! ("Header found: ~s" =text)

)

;****************************************
;If the name in the visual location does not match
;anything in memory, find another name
(P Checking-Header_No-Match

=goal>
	state     checking-header
	
?retrieval>
	buffer    failure

==>

=goal>
	state     finding-header-redo

!output! ("Redo search")

)

;find text randomly (within header box)
(P Find-Header-Box-Redo

=goal>
        state   	finding-header-redo

=imaginal>
	header-bottom        =bottom
        header-top           =top
==>

=imaginal>

+visual-location>
	ISA		 visual-location	
	kind		 text
        >= screen-top	 =top
        <= screen-bottom =bottom       
        :attended        nil

=goal>
	state		attending-header

)

; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 
; Makes a visual location request for the instruction box
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

;**********************************************************
; find top instructions (width, screen-y hardcoded)
; the width should be defined using column width, once the visual grouping algorithm is updated

(P Find-Top-Instr-Box

=goal>
	state		finding-instructions

=imaginal>
	header-bottom   =val

==>

=imaginal>

+visual-location>
	ISA	        visual-location
	kind		image
        > width         250  
        < screen-top    200 
        > screen-top	=val
	;screen-top     lowest
	;screen-y	lowest
      
	
=goal>
	state		attending-instr-box
)


; if instructions is in the left-column (no top-instructions found)

(P Find-Top-Instr-Box-No-Match

=goal>
	state		attending-instr-box

?visual-location>
	buffer		failure

=imaginal>
	header-bottom   =val

==>

=imaginal>

+visual-location>
	ISA	        visual-location
	kind		image
        > screen-top	=val
	screen-left     lowest
	screen-y	lowest
	
=goal>
	state		attending-instr-box2
)

;attend instruction-box
(P Attend-Instr-Box2

=goal>
	state		attending-instr-box2
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		attending-first-box-2	
	
)

(P Attend-Instr-Box

=goal>
	state		attending-instr-box
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		attending-first-box-2	
	
)

; modify imaginal buffer
(P Attend-Instr-Box-2

=goal>
	state		attending-first-box-2

=visual-location>
	ISA		visual-location	
	kind		image
	screen-top	=current-top
        screen-bottom   =current-bottom
        screen-left     =current-left
        screen-right    =current-right

=imaginal>
	instr-top      none
        instr-bottom   none
        instr-left     none
        instr-right    none

==>
	
=visual-location>

=imaginal>
	instr-top      =current-top
        instr-bottom   =current-bottom
        instr-left     =current-left
        instr-right    =current-right
	
=goal>
	state		finding-instr	
	
)

;**********************************************************
; find instructions & locate box boundaries
(P Find-Instr

=goal>
        state   	finding-instr

=visual>

=visual-location>
	ISA		visual-location	
	kind		image

=imaginal>
	instr-top      =current-top
        instr-bottom   =current-bottom
        instr-left     =current-left
        instr-right    =current-right
==>

=imaginal>

+visual-location>
	ISA		  visual-location	
	kind		  text
        >= screen-top	  =current-top
        <= screen-bottom  =current-bottom      
        >= screen-left    =current-left
        <= screen-right   =current-right 
        :attended         nil

=goal>
	state		attending-instr
)

(P Attending-Instr

=goal>
	state		attending-instr

=visual-location>
	ISA             visual-location
	kind            text

?visual>
	state	        free
==>

=visual-location>

+visual>
	ISA            move-attention
	screen-pos     =visual-location

=goal>
	state          retrieving-instr

)

;Search for non-race text in memory
(P Retrieving-Instr

=goal>
	state	      retrieving-instr

=visual>
	ISA           text
        value	      =text
	screen-pos    =pos

?retrieval>
	state         free

==>

=visual>

+retrieval>
	ISA        instruction
	keyword    =text

=goal>
	state       checking-instr

!output! ("looking at: ~s" =text)
)


;a text was found and matches with the instruction keywords
(P Checking-Instr-Box

=goal>
        state       checking-instr

=visual>
        value	    =text
	screen-pos  =pos

=retrieval>
	ISA        instruction 
	keyword    =text

==>

=visual>

=goal>
	state	    find-bubbles-in-instr

!output! ("Instructions found: ~s" =text)

)

;If the name in the visual location does not match
;anything in memory, find another name
(P Checking-Instr_No-Match

=goal>
	state     checking-instr
	
?retrieval>
	buffer    failure

==>

=goal>
	state     finding-instr-redo

!output! ("Redo search")

)

;find text randomly (within header box)
(P Find-Instr-Box-Redo

=goal>
        state   	finding-instr-redo

=imaginal>
	instr-top      =top
        instr-bottom   =bottom
        instr-left     =left
        instr-right    =right
==>

=imaginal>

+visual-location>
	ISA		 visual-location	
	kind		 text
        >= screen-top	 =top
        <= screen-bottom  =bottom      
        >= screen-left    =left
        <= screen-right   =right 
        :attended        nil

=goal>
	state		attending-instr

)

;**********************************************************
; find if there is any bubbles in the instructions
; if so, attend, and then find races, if not, find races directly
(P Finding-Bubbles-In-Instr

=goal>
        state      find-bubbles-in-instr 

=visual>

=imaginal>
	instr-top      =current-top
        instr-bottom   =current-bottom
        instr-left     =current-left
        instr-right    =current-right

==>

=imaginal>

+visual-location>
	ISA		  visual-location	
	kind		  oval
        >= screen-top	  =current-top
        <= screen-bottom  =current-bottom      
        >= screen-left    =current-left
        <= screen-right   =current-right

=goal>
	state	    attend-bubbles-in-instr

)

(P Attending-Bubbles-in-Instr

=goal>
	state		attend-bubbles-in-instr

=visual-location>
	ISA             visual-location
	kind            oval

?visual>
	state	        free
==>

=visual-location>

+visual>
	ISA            move-attention
	screen-pos     =visual-location

=goal>
	state          find-instr-box

)

; ; if there is no bubbles & images present within instructions-box, find footer
(P Attending-Bubbles-in-Instr-No-Match

=goal>
	state		attend-bubbles-in-instr

?visual-location>
        buffer          failure

==>

+visual-location>
	ISA		 visual-location
        kind             image
       > screen-y        current
        screen-y	 highest

=goal>
	state          attending-footer-box

)
; ; 

; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 
; Makes a visual location request for the footer box
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

;find footer
(P Find-Instr-Box-Second-Time

=goal>
	state		find-instr-box

=visual-location>

=imaginal>
	header-bottom   =val
        instr-right     =right
        
==>

=imaginal>

+visual-location>
	ISA		visual-location
        kind            image
        >= screen-top    =val
        screen-right     =right
     ; <= screen-right   =current-right
        screen-y         lowest

=goal>
	state          attend-race-box-second-time

)

; Attend the current box
(P Attend-Current-Box-Instr

=goal>
	state		attend-race-box-second-time
	
=visual-location>
	ISA	        visual-location	
	kind            image
        
?visual>
	state		free

==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		finding-footer-box
	
)

; find the footer box
(P Find-Footer-Box

=goal>
	state		finding-footer-box

=visual>

=visual-location>
        kind            image
       
==>

+visual-location>
	ISA		        visual-location
        kind                    image
       > screen-y               current
        screen-y	        highest

=goal>
	state		attending-footer-box

)

; Attend footer box
(P Attend-Footer-Box

=goal>
	state		attending-footer-box
	
=visual-location>
	ISA	        visual-location	
	kind            image
        screen-top	=current-top
        screen-bottom   =current-bottom
?visual>
	state		free

=imaginal>
	footer-top      none
        footer-bottom   none

==>

=imaginal>
	footer-top      =current-top
        footer-bottom   =current-bottom

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		finding-footer-text
	
)

(P Find-Footer-Text

=goal>
        state   	finding-footer-text

=visual>

=visual-location>
	ISA		visual-location	
	kind		image
	screen-top	=current-top
        screen-bottom   =current-bottom
	
==>

+visual-location>
	ISA		 visual-location	
	kind		 text
        >= screen-top	 =current-top
        <= screen-bottom =current-bottom       
        :attended        nil

=goal>
	state		attending-footer-text

)

(P Attending-Footer-Text

=goal>
	state		attending-footer-text

=visual-location>
	ISA             visual-location
	kind            text

?visual>
	state	        free

==>

=visual-location>

+visual>
	ISA            move-attention
	screen-pos     =visual-location

=goal>
	state          retrieving-footer
)


;Search for non-race text in memory
(P Retrieving-Footer

=goal>
	state	      retrieving-footer

=visual>
	ISA           text
        value	      =text
	screen-pos    =pos

?retrieval>
	state         free

==>

=visual>

+retrieval>
	ISA        instruction
	keyword    =text

=goal>
	state       checking-footer

!output! ("looking at: ~s" =text)
)


;a text was found and matches with the instruction keywords
(P Checking-Footer-Box

=goal>
        state       checking-footer

=visual>
        value	    =text
	screen-pos  =pos

=retrieval>
	ISA        instruction 
	keyword    =text

==>

=visual>

=goal>
	state	    find-first-race

!output! ("Footer found: ~s" =text)

)

;If the name in the visual location does not match
;anything in memory, find another name
(P Checking-Footer_No-Match

=goal>
	state     checking-footer
	
?retrieval>
	buffer    failure

==>

=goal>
	state     finding-footer-redo

!output! ("Redo search")

)

;find text randomly (within footer box)
(P Find-Footer-Box-Redo

=goal>
        state   	finding-footer-redo

=imaginal>
	footer-top      =top
        footer-bottom   =bottom

==>

=imaginal>

+visual-location>
	ISA		 visual-location	
	kind		 text
        >= screen-top	 =top
        <= screen-bottom =bottom       
        :attended        nil

=goal>
	state		attending-footer-text
)


;**********************************************************
;**********************************************************
; attend current footer box, and then move to races

; Makes a visual location request for the current footer box
(P Find-Current-Box-Footer

=goal>
	state		find-first-race

=visual-location>

=imaginal>
	header-bottom   =val
        
==>

=imaginal>

+visual-location>
	ISA		visual-location
        kind            image
      >= screen-top     =val
     ; <= screen-right   =current-right
        :nearest        current

=goal>
	state		attending-current-box-footer

)

; Attend the current box
(P Attend-Current-Box-Footer

=goal>
	state		attending-current-box-footer
	
=visual-location>
	ISA	        visual-location	
	kind            image
        
?visual>
	state		free

==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		finding-racebox-first-column
	
)

; find the racebox in first column
(P Find-RaceBox-First-Column

=goal>
	state		finding-racebox-first-column

=visual>

=visual-location>
        kind            image
;	screen-top	=current-top
     
=imaginal>
        instr-bottom    =bottom
  
==>

=imaginal>

+visual-location>
	ISA		        visual-location
        kind                    image
        > screen-top	        =bottom
        screen-x                lowest
        screen-y                lowest

=goal>
	state		attending-racebox-first-column

)

(P Attend-RaceBox-First-Column

=goal>
	state		attending-racebox-first-column
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

==>

+visual>
	ISA		  move-attention
	screen-pos	  =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		finding-race	
	
)



; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 
; ------------------- These next productions find the race box and request a race box in the same column
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

; Makes a visual location request for the current box

(P Find-Current-Box

=goal>
	state		find-next-race

=visual-location>

=imaginal>
	header-bottom   =val
        
==>

=imaginal>

+visual-location>
	ISA		visual-location
        kind            image
      >= screen-top     =val
     ; <= screen-right   =current-right
        :nearest        current

=goal>
	state		attending-race-title

)

; Attend the current box
(P Attend-Current-Box

=goal>
	state		attending-race-title
	
=visual-location>
	ISA	        visual-location	
	kind            image
        
?visual>
	state		free

==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		finding-racebox-same-column
	
)

; find the underneath racebox
(P Find-RaceBox-Same-Column

=goal>
	state		finding-racebox-same-column

=visual>

=visual-location>
        kind            image
	screen-right	=current-right
	screen-bottom	=current-bottom
;        screen-top      =current-top
       
==>

+visual-location>
	ISA		        visual-location
        kind                    image
        > screen-top	        =current-bottom
        < screen-left           =current-right
        screen-right            =current-right
        :nearest                current

=goal>
	state		attending-racebox-same-column

; !output! ("Last race outline top ~s bottom ~s left ~s right~s" =top-bound =bottom-bound =left =right)

)

; ;
(P Attend-RaceBox-Same-Column

=goal>
	state		attending-racebox-same-column
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

==>

+visual>
	ISA		  move-attention
	screen-pos	  =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		finding-race	
	
)


(P Find-Race-Title

=goal>
	state   	finding-race

=visual>
	
=visual-location>
	ISA		visual-location	
	kind		image
	screen-top	=current-top
        screen-bottom   =current-bottom
        screen-left     =current-left
        screen-right    =current-right

==>

+visual-location>
	ISA		visual-location	
	kind		text
        >= screen-top	 =current-top
        <= screen-bottom =current-bottom
        >= screen-left   =current-left
        <= screen-right  =current-right
        screen-y         lowest
        :nearest         current
        ;color            red

=goal>
	state		attending-race

)


; We have found the race and so we pass control to encoding process (call relative positions file)
; ; attending race. works either same row or next row
(P Attend-Race

=goal>
	state		attending-race
			
=imaginal>

?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		text

==>

=imaginal>
	race-group		none
	candidate-group  	none
	party-group		none

+visual>
	ISA			move-attention
	screen-pos	        =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		text

=goal>
	state		storing-race-group

)

; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 
;-------------------------------- These next productions either pass control to encoding if a race is found or move to the next column
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

; ; This production being called means that we have reached the end of a row, so we begin the process of finding the top race in the next row

(P Find-RaceBox-Same-Column-No-Match

=goal>
	state		attending-racebox-same-column	

?visual-location>
	buffer		failure
	
==>	

=goal>
	state		find-top-race

)

;****************************************
; This is the next of the productions that find the next race if it is in a different column (after find-race-same-column-no-match)
; It finds the top race in this column to prepare for the switch to the next column

; define the width of the race-box (otherwise the model won't work if an image present within top-instr) 
(P Find-Top-Race

=goal>
	state		find-top-race

=imaginal>
       header-bottom   =val2
       
==>

=imaginal>

+visual-location>
	ISA		visual-location
        kind            image
        < width         300
        > width         200
        >= screen-top   =val2
        screen-top      lowest
        :nearest	current-x

=goal>
	state		attending-top-race

)


;****************************************
; This production attends the top race after we have found its location in preperation for the move to the next column.
(P Attend-Top-Race

=goal>
	state		attending-top-race
	
=visual-location>
	ISA		visual-location	
	kind		image
	
?visual>
	state		free

==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location

=visual-location>
	
=goal>
	state		find-racebox-next-column
	
)


;****************************************
; This production makes the visual location request for the top race in the next column over
; Another big cheat is here: we make a guess (called right-guess) for an x location that is to the right of any text in this column
; and hopefully to the left of any text in the next column
(P Find-RaceBox-Next-Column

=goal>
	state 		find-racebox-next-column
	
=visual>

=visual-location>
	screen-right	=current-right

==>

+visual-location>
	ISA			visual-location
	kind			image
	>= screen-left	        =current-right
        screen-x                lowest
        screen-y                lowest       
       ; :nearest		current-y

=goal>
	state		attending-racebox-next-column

)


(P Attend-RaceBox-Next-Column

=goal>
	state		attending-racebox-next-column
		
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

==>

+visual>
	ISA			move-attention
	screen-pos	        =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		finding-race
	
)


; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 
; ; If there is nothing found when looking for a new col, we are at the top right corner of the ballet and there are no more race boxes, 
; ; so we can end the model
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

(P Find-RaceBox-Next-Col-No-Match

=goal>
	state		attending-racebox-next-column

?visual-location>
	buffer		failure
	
==>

=goal>
	state		end

)

