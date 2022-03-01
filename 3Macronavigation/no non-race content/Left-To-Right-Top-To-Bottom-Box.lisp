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
;;; Filename    : Left-To-Right-Top-To-Bottom-Box.lisp
;;; Version     : 1
;;; 
;;; Description : Macronavigation strategy
;;;				: * If starting, finds the race in the top left corner. Otherwise tries to find the next race in the same row
;;;
;;; Bugs        : * None known
;;;
;;; To do       :
;;;
;;; ----- History -----
;;; 2019.10.9   Xianni Wang
;;;				: * Created the file; modified from "left-to-right-top-to-bottom.lisp"
;;; 2020.10.08   Xianni Wang
;;;				: * Add productions Find-RaceBox-Same-Row-Right-2 and Find-RaceBox-Same-Row-Right-3
;;;				: * updated productions that deal with "Already-voted" situation
;;; 2020.10.28   Xianni Wang
;;;                             : * Add productions (from Attend-RaceBox-Last-Row to Check-Corner)
;;;                                to solve the miss votes in the bottom-right corner
;;;                             : * Add productions (Find-RaceBox-Next-Row-Check) to check if the model reaches the bottom row
;;; 2021.10.08   Xianni Wang
;;;                             : * Updated imaginal buffers (RHS)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; This model uses the macronavigation strategy: left to right then top to bottom.
;;; This model will check the last row two times, in order to reduce the missvote in the bottom right corner.
;;;   
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Assumes verticles are exactly lines up

; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 
; Makes a visual location request for the first race (background box)
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

(P Find-First-Box

=goal>
	state		start-voting

?imaginal>
	state		free

==>

+visual-location>
	ISA	        visual-location
	kind		image
	screen-left     lowest
	screen-y	lowest
	
=goal>
	state		attending-first-box
)

;attend box in the left-top corner
(P Attend-First-Box

=goal>
	state		attending-first-box
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

==>

+visual>
	ISA		       move-attention
	screen-pos	       =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		finding-first-race	
	
)


; Find race title
(P Find-First-Race-Title

=goal>
	state   	finding-first-race
	
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
        :nearest         current
        color            red
;        < screen-y      current
;        :nearest	current


=goal>
	state		attending-race1

)

(P Attend-First-Race-Title

=goal>
	state		attending-race1
			
?imaginal>
        state           free

?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		text

==>

+imaginal>
	race-group		none
	candidate-group  	none
	party-group		none
	first-race-col	        nil
        instr-race-col          nil
        last-row	        nil
        last-row-second         nil

+visual>
	ISA			move-attention
	screen-pos	        =visual-location
	
=visual-location>

=goal>
	state		storing-race-group
;        anchor		=visual-location

)

; -----------------------reuse these two productions for the rest of the races------
; Find race title
(P Find-Race-Title

=goal>
	state   	finding-race

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
        :nearest         current
        color            red
;        < screen-y      current
;        :nearest	current


=goal>
	state		attending-race

)

; We have found the race and so we pass control to encoding process (call relative positions file)
; ; attending race. works either same row or next row
(P Attend-Race-Title

=goal>
	state		attending-race
			
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		text

=imaginal>

==>

=imaginal>
	race-group		none
	candidate-group  	none
	party-group		none

+visual>
	ISA			move-attention
	screen-pos	        =visual-location
	
=visual-location>

=goal>
	state		storing-race-group
;        anchor		=visual-location

)


; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 
; ------------------- These next productions find the race box and request a race box in the same row
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

; Deals with if we've already seen this race
(P Deal-With-Already-Voted-First-Col

=goal> 
	state			already-voted
	
=imaginal>
	first-race-col	        true
	
==>

=goal> 
	state			find-left-race

)

; Deals with if we've already seen this race
(P Deal-With-Already-Voted-Not-First-Col

=goal> 
	state			already-voted
	
=imaginal>
	first-race-col	        nil
        last-row	        nil
	
==>

=goal> 
	state			find-next-race1
	
=imaginal>

)

(P Find-Current-Race-Title-Box1

=goal>
	state		find-next-race1
	
==>

+visual-location>
	ISA		visual-location
        kind            image
        :nearest        current

	
=goal>
	state		attending-race-title

)


; ---------------------------------------------------------
; Makes a visual location request for the current race title (Box)
(P Find-Current-Race-Title-Box

=goal>
	state		find-next-race

=imaginal>		
        last-row	        nil

=visual-location>
	screen-top	=current-top
	
==>

=imaginal>

+visual-location>
	ISA		visual-location
        kind            image
      <= screen-top    =current-top
        :nearest        current

	
=goal>
	state		attending-race-title

)


; Attend the race title (Box)
(P Attend-Current-Race-Title-Box

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
	state		finding-racebox-same-row
	
)

; Makes a visual location request for the box that is to the right, and nearest to this current box
; check if there is a racebox at the lower right corner. if so, navigate back and ask for ":nearest  current-y"

(P Find-RaceBox-Same-Row

=goal>
	state		finding-racebox-same-row
	
=visual>

=visual-location>
        kind            image
	screen-right	=current-right
        screen-left     =current-left

==>

=visual>

+visual-location>
        ISA		visual-location
        kind            image
        > screen-left	=current-right
        >= screen-y     current
        screen-left	lowest
        :nearest        current-y
	
=goal>
	state		finding-racebox-same-row2

; !output! ("Last race outline top ~s bottom ~s left ~s right~s" =top-bound =bottom-bound =left =right)

)


(P Find-RaceBox-Same-Row-2

=goal>
	state		finding-racebox-same-row2

=visual>

=visual-location>
        kind            image
        screen-right	=current-right
        screen-left     =current-left
==>

=visual>

+visual-location>
        ISA		visual-location
        kind            image
	< screen-right	        =current-left
;        >= screen-top           =current-top
	screen-left		highest
        :nearest                current-y      

=goal>
	state		finding-racebox-same-row3

)


(P Find-RaceBox-Same-Row-3

=goal>
	state		finding-racebox-same-row3
	direction	rightwards
;	anchor		=anchor
	
=visual>

=visual-location>
        kind            image
	screen-right	=current-right
;	screen-top	=current-top

==>
	
+visual-location>
	ISA		        visual-location
        kind                    image
        > screen-left	        =current-right
        ;>= screen-top          =current-top
 ;       >= screen-y             current
        screen-left		lowest
        :nearest                current-y

=goal>
	state		attending-racebox-same-row

)



; ;
(P Attend-RaceBox-Same-Row

=goal>
	state		attending-racebox-same-row
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

=imaginal>

==>

=imaginal>
	race-group		none
	candidate-group  	none
	party-group		none
        button-group            none
	first-race-col	        nil
        last-row	        nil

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
;-------------------------------- These next productions either pass control to encoding if a race is found or move to the next row
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

; ; This production being called means that we have reached the end of a row, so we begin the process of finding the top race in the next row
(P Find-RaceBox-Same-Row-No-Match

=goal>
	; state		attending-racebox-same-row	
        state           finding-racebox-same-row2

?visual-location>
	buffer		failure

	
==>


=goal>
	state		find-left-race

)

; ; Make the visual location request for the leftmost race in the next row down
(P Find-Left-Race

=goal>
	state		find-left-race

;=visual-location>
;	screen-bottom   =current-bottom
;        screen-top      =current-top

==>

+visual-location>
	ISA		visual-location
        screen-left	lowest	
        kind            image
;        <= screen-y     current-y
       :nearest	       current-y 
;        screen-y        lowest
;	:nearest	current-y
;      <= screen-bottom	=current-bottom
;      >= screen-top     =current-top

=goal>
	state		attending-left-race

)

(P Attend-Left-Race

=goal>
	state		attending-left-race
	
=visual-location>
	ISA		visual-location	
	kind            image

?visual>
	state		free

==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location

=visual-location>
	
=goal>
	state		find-racebox-next-row
	
	
)

; ; Make the visual location request for the race below the attended leftmost race
(P Find-RaceBox-Next-Row

=goal>
	state 		find-racebox-next-row
	
;=visual>

=visual-location>
	ISA		visual-location
        kind            image
	screen-left	=current-left
;        screen-top      =screen-top
==>

;=visual>

; Order is very important here
+visual-location>
	ISA		visual-location
	kind		image
	= screen-left	=current-left
	> screen-y	current
	screen-y	lowest
;        screen-left     lowest
;	screen-y	lowest
	
=goal>
       state	 attending-racebox-next-row

)
	
; ; We have found a race in the next row, so attend it and start encoding
(P Attend-RaceBox-Next-Row

=goal>
	state		attending-racebox-next-row
		
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
	state		finding-racebox-next-row-check
	
)


; ; -------------- check if the model reaches the bottom row
; ; if reach, go to "Attend-RaceBox-Last-Row" production series; if not, go back to the current row & keep voting
(P Find-RaceBox-Next-Row-Check

=goal>
	state 		finding-racebox-next-row-check
	
=visual>

=visual-location>
	ISA		visual-location
        kind            image
	screen-left	=current-left
;        screen-top      =screen-top
==>

=visual>

; Order is very important here
+visual-location>
	ISA		visual-location
	kind		image
	= screen-left	=current-left
	> screen-y	current
	screen-y	lowest
;        screen-left     lowest
;	screen-y	lowest
	
=goal>
       state	 finding-racebox-next-row-check-2

)

(P Find-RaceBox-Next-Row-Check-Back

=goal>
	state 		finding-racebox-next-row-check-2
	
=visual>

=visual-location>
	ISA		visual-location
        kind            image
	screen-left	=current-left
        screen-top      =current-top

==>

=visual>

; Order is very important here
+visual-location>
	ISA		visual-location
	kind		image
	= screen-left	=current-left
        < screen-top    =current-top
	;< screen-y	current
	screen-top	highest
;        screen-left     lowest
;	screen-y	lowest
	
=goal>
       state	 attending-racebox-next-row2

)

(P Attend-RaceBox-Next-Row2

=goal>
	state		attending-racebox-next-row2
		
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

=imaginal>

==>

=imaginal>
	first-race-col         	true
        last-row	        nil

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
; ; If there is nothing found when looking for a new row, we are at the bottom right corner of the ballet and there are no more race boxes, 
; ; so we can end the model.
; ; But before we end the model, we check the last row two times:
; ; for the first time we navigate to the nearest box to the right (:nearest current-y)
; ; for the second time, we navigate to the nearest box to the bottom right (screen-y >= current  &   :nearest current-y)
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

; ; fail to find racebox in next row (reach the last row)

(P Find-RaceBox-Next-Row-No-Match

=goal>
	state		attending-racebox-next-row

=visual>

?visual-location>
	buffer		failure

=imaginal>

==>

=imaginal>
	first-race-col         	true
        last-row	        true

+visual-location>
        ISA		visual-location
        kind            image
        :nearest        current
 ;       screen-bottom	highest

=goal>
	state		attending-racebox-last-row

)

(P Find-RaceBox-Next-Row-Check-No-Match

=goal>
	state		finding-racebox-next-row-check-2

=visual>

?visual-location>
	buffer		failure

=imaginal>

==>

+visual-location>
        ISA		visual-location
        kind            image
        :nearest        current
 ;       screen-bottom	highest


=imaginal>
	first-race-col         	true
        last-row	        true

=goal>
	state		attending-racebox-last-row

)

; ; attend current racebox in the last row 
; ; these two productions check if the race in the bottom-left corner is voted

(P Attend-RaceBox-Last-Row

=goal>
	state		attending-racebox-last-row
	
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

(P Find-Current-Race-Title-Box-Last-Row

=goal>
	state		find-next-race

=imaginal>	
        last-row	true

=visual-location>
	screen-top	=current-top
	
==>

=imaginal>
	first-race-col    	nil
        last-row         	true

+visual-location>
	ISA		visual-location
        kind            image
      <= screen-top    =current-top
        :nearest        current

	
=goal>
	state		attending-race-title-last-row

)

; Attend the race title (Box) last row
(P Attend-Current-Race-Title-Box-Last-Row

=goal>
	state		attending-race-title-last-row
	
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
	state		finding-racebox-same-row-last-row
	
)

; Makes a visual location request for the box that is to the right, and nearest to this current box
; Need greater than y clause
(P Find-RaceBox-Same-Row-Last-Row

=goal>
	state		finding-racebox-same-row-last-row
	
=visual>

=visual-location>
        kind            image
	screen-right	=current-right
        screen-left     =current-left

==>

=visual>

+visual-location>
	ISA		        visual-location
        kind                    image
        > screen-left	        =current-right
        ;>= screen-top          =current-top
;        >= screen-y             current
        screen-left		lowest
        :nearest                current-y
	
=goal>
	state		attending-racebox-last-row-2

)

(P Attend-RaceBox-Last-Row-2

=goal>
	state		attending-racebox-last-row-2
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

=imaginal>

==>

=imaginal>
	first-race-col         	nil
        last-row	        true

+visual>
	ISA			move-attention
	screen-pos	        =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		finding-race	
	
)


(P Already-Voted-Last-Row

=goal>
	state		       already-voted

=imaginal>
        last-row                true
;        first-race-col	        nil

=visual>

=visual-location>
	screen-top	=current-top

==>

=visual>

=imaginal>

+visual-location>
        ISA		visual-location
        kind            image
        <= screen-top   =current-top
        :nearest        current

=goal> 
	state		finding-racebox-same-row-last-row	

) 

; ; --------------------
; if the last row is finished, go back to the bottom left corner and double check

(P Find-RaceBox-Same-Row-No-Match-Last-Row

=goal>
	
        state           attending-racebox-last-row-2

?visual-location>
	buffer		failure

=visual>

=imaginal>	

==>

=imaginal>
        first-race-col   true

+visual-location>
	ISA		     visual-location
        kind                 image
        screen-x             lowest
        screen-y             highest

=goal>
	state		    attending-racebox-last-row-second-time

)

(P Attend-RaceBox-Last-Row-Second-Time

=goal>
	state		attending-racebox-last-row-second-time

=imaginal>
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

==>

=imaginal>
;	first-race-col	        true
        last-row	        true

+visual>
	ISA			move-attention
	screen-pos	        =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		finding-race	
	
)



; ; ---------------
; ; The race in the bottom-left corner has already been voted, process to double check the last row

(P Deal-With-Already-Voted-Last-Row-BottomLeft

=goal> 
	state			already-voted
	
=imaginal>
	last-row	        true
	first-race-col	        true

=visual>

=visual-location>
	screen-top	=current-top

==>

=visual>

=imaginal>

+visual-location>
        ISA		visual-location
        <= screen-top    =current-top
        kind            image
        :nearest        current

=goal> 
	state			attending-racebox-last-row-bottomleft
	

)


(P Attend-RaceBox-Last-Row-BottomLeft

=goal>
	state		attending-racebox-last-row-bottomleft
	
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
	state		finding-racebox-same-row-last-row-corner	
	
)

(P Find-RaceBox-Same-Row-Last-Row-Corner

=goal>
	state		finding-racebox-same-row-last-row-corner
	
=visual>

=visual-location>
        kind            image
	screen-right	=current-right
        screen-left     =current-left

==>

=visual>

+visual-location>
	ISA		        visual-location
        kind                    image 
        > screen-left	        =current-right
        >= screen-y             current
        screen-left	        lowest
        :nearest                current-y
		
=goal>
	state		attending-racebox-last-row-corner

)


(P Attend-RaceBox-Last-Row-Corner

=goal>
	state		attending-racebox-last-row-corner

=imaginal>
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

==>

=imaginal>
	first-race-col	        nil
        last-row	        true
        last-row-second         true

+visual>
	ISA			move-attention
	screen-pos	        =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		finding-race	
	
)


(P Already-Voted-Last-Row-Check-Corner

=goal>
	state		       already-voted

=imaginal>
        last-row-second         true


=visual-location>
	screen-top	=current-top

==>


=imaginal>

+visual-location>
        ISA		visual-location
        kind            image
        <= screen-top   =current-top
        :nearest        current

=goal> 
	state		finding-racebox-same-row-last-row-corner	

) 

(P Check-Corner

=goal>
	state		       attending-racebox-last-row-corner

?visual-location>
        buffer                 failure

==>

=goal> 
	state			end	

) 

