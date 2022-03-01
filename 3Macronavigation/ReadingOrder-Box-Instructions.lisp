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
;;; Filename    : ReadingOrder-Box-Instructions.lisp
;;; Version     : 1
;;; 
;;; Description : Macronavigation strategy
;;;				: * If starting, first finds the header. Then find the instructions in the top left corner (below the header). Then footer.
;;;				: * Then, finds the race in the top left corner (middle column). Otherwise tries to find the next race in the same row.
;;;
;;; Bugs        : * None known
;;;
;;; To do       : * There are a few places where I have had to cheat to make the model work (because we do not have access to relative group 
;;;				: * positios or super and sub groups).
;;;				: * 
;;;				: * this strategy cannot miss any races listed under left-instructions (vg algorithm needs to be updated)
;;;
;;; ----- History -----
;;; 2022.01.16   Xianni Wang
;;;				: * Add productions to accommodate 4 ballot layouts (ballot_instructions_1,2,3,4; both top-instr & left-instr)
;;; 2021.11.10   Xianni Wang
;;;				: * Added productions to do random visual search in header, footer & instructions boxes
;;; 2021.10.06   Xianni Wang
;;;				: * Created the file; modified from "left-to-right-top-to-bottom-Box.lisp"
;;;				: * Added productions to handle headers (copied from "downeachcolumn-box-instructions.lisp")
;;;				: * Modify visual location buffers to make the model navigate among races & columns
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; This model can handle header, footer, and the instructions in the top left corner.
;;; This model uses the macronavigation strategy: left-to-right, then top-to-bottom.
;;;    for top-instr ballots, the model will start voting from the left column. 
;;;    for left-instr ballots, the model will start from the middle column.
;;; This model will check the last row two times, in order to reduce missvotes in the bottom right corner.
;;;   
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Assumes verticles are exactly lines up

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
	first-race-col	        nil
        left-race-col           nil
        last-row	        nil
        last-row-second         nil

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
; Makes a visual location request for the first box (instructions)
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

;attend box in the left-top corner

(P Attend-Instr-Box-2

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
(P Attend-Instr-Box2

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


;****************************************
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
	state		finding-instructions-2
	
)

;**********************************************************
;**********************************************************
; find top instructions again (width, screen-y hardcoded) before moving to races
; if top-instructions were found, then start from the left column with reading-order strategy
; if top-instructions were not found, then start from the middle column

(P Find-Top-Instr-Box-2

=goal>
	state		finding-instructions-2

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
	state		attending-current-top-instr-box
)

; Attend the top-instr box
(P Attend-Current-Instructions-Box

=goal>
	state		attending-current-top-instr-box
	
=visual-location>
	ISA	        visual-location	
	kind            image
        
?visual>
	state		free

=imaginal>

==>

=imaginal>
        left-race-col	true

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		finding-racebox-left-column
	
)

; if instructions is in the left-column (no top-instructions found), find the left-instructions box
(P Find-Top-Instr-Box-No-Match-2

=goal>
	state		attending-current-top-instr-box

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
       ; > width         200 
	screen-left     lowest
	screen-y	lowest
	
=goal>
	state		attending-current-left-instr-box2
)

; Attend the left-instr box
(P Attend-Current-Instructions-Box2

=goal>
	state		attending-current-left-instr-box2
	
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
	state		finding-racebox-middle-column
	
)

; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 
; Makes a visual location request for the first race box which right next to the left-instructions (middle col, first row)
; for top-instr ballot, find the upper leftmost race (left col) on the ballot 
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

; find the racebox in the left column (works for top-instr ballots)
(P Find-FirstRaceBox-Left-Column

=goal>
	state		finding-racebox-left-column

=visual>

=visual-location>
        kind            image
;	screen-top	=current-top
     
=imaginal>
        instr-bottom   =bottom
  
==>

=imaginal>

+visual-location>
	ISA		        visual-location
        kind                    image
        > screen-top	        =bottom
        screen-x                lowest
        screen-y                lowest

=goal>
	state		attending-current-racebox

)

;**********************************************
; find the top racebox in the middle column (works for left-instr ballots)
(P Find-RaceBox-Middle-Column

=goal>
	state		finding-racebox-middle-column

=visual>

=visual-location>
        kind            image
     
=imaginal>
        header-bottom   =headerbottom
        instr-bottom    =instrbottom
        instr-right     =right
  
==>

=imaginal>

+visual-location>
	ISA		        visual-location
        kind                    image
        > screen-top	        =headerbottom
        <= screen-top           =instrbottom
        > screen-left           =right
        screen-x                lowest
        screen-y                lowest

=goal>
	state		attending-current-racebox

)

(P Attend-Current-RaceBox

=goal>
	state		attending-current-racebox
	
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

; Deals with if we've already seen this race (middle column; first race column for left-instr ballots)
(P Deal-With-Already-Voted-First-Col

=goal> 
	state			already-voted
	
=imaginal>
	first-race-col	        true
	
==>

=imaginal>

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
        last-row	nil

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

; ;three productions for checking

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

==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
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

; ***************************************************
; ; Make the visual location request for the leftmost race in the next row down 

; works for left-instr ballots (races between instr-top and instr-bottom)
(P Find-Left-Race-MiddleCol

=goal>
	state	       find-left-race

=imaginal>
        instr-right    =instrright
        header-bottom  =headerbottom
        instr-bottom   =instrbottom
        left-race-col  nil

==>

=imaginal>

+visual-location>
	ISA		visual-location
     <= screen-top      =instrbottom
     >= screen-bottom   =headerbottom
     >= screen-left     =instrright
   ;  <= screen-bottom  =instrbottom
       ;screen-left	lowest	
        screen-x        lowest
        kind            image
       :nearest	        current-y
       ;screen-x        lowest


=goal>
	state		attending-left-race

)

; works for top-instr ballots (&left-instr ballots; for races lower than instructions)
(P Find-Left-Race-MiddleCol-Failure

=goal>
	state		find-left-race
	
;?visual-location>
;        buffer          failure

=imaginal>
        left-race-col	true

==>

=imaginal>

+visual-location>
	ISA		visual-location
        screen-left	lowest	
        kind            image
       :nearest	        current-y 
	
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





;*********************************
; ; Make the visual location request for the race below the attended leftmost race

;* the following two productions are for left-instr ballots (races between instr-top and instr-bottom)
(P Find-RaceBox-Next-Row

=goal>
	state 		find-racebox-next-row
	
=visual>

=visual-location>
	ISA		visual-location
        kind            image
	screen-left	=current-left
;        screen-top      =screen-top

=imaginal>
        footer-top     =val
        instr-bottom   =instrbottom
        header-bottom  =headerbottom
        instr-right    =instrright

==>

=imaginal>
=visual>

; Order is very important here
+visual-location>
	ISA		   visual-location
	kind		   image
        > screen-left      =instrright
        < screen-bottom    =val 
        >= screen-bottom   =headerbottom
        <= screen-top    =instrbottom
	= screen-left	 =current-left
	> screen-y	 current
	screen-x	 lowest
        screen-y         lowest
;        screen-left     lowest
;	screen-y	 lowest
	
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

;* the following two productions are for top-instr ballots (&left-instr ballots, for races lower than instructions)
; !!!!!!! might need to change later
; !!!!!!! (the left-race-col might not be changed sometimes, once the visual grouping algorithm is updated)

(P Find-RaceBox-Next-Row-Left-Col

=goal>
	state 	       attending-racebox-next-row
	
=visual>
        value	    =text
	screen-pos  =pos

?visual-location>
        buffer         failure

=imaginal>
        footer-top     =val
        instr-bottom   =instrbottom

==>

=visual>

=imaginal>
        left-race-col   true

+visual-location>
	ISA		    visual-location
	kind		    image
        <= screen-bottom    =val 
        >= screen-top       =instrbottom
        screen-x            lowest
        > screen-y          current
        screen-y            lowest

	;= screen-left	=current-left    
	;> screen-y	current
	;screen-y	lowest
	
=goal>
       state	 attending-racebox-next-row-2

)

(P Attend-RaceBox-Next-Row-2

=goal>
	state		attending-racebox-next-row-2
		
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

=imaginal>
        footer-top      =footertop
==>

=imaginal>

=visual>

; Order is very important here
+visual-location>
	ISA		 visual-location
	kind		 image
        <= screen-bottom =footertop
	= screen-left	 =current-left
	> screen-y	 current
	screen-y	 lowest
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
;	state		attending-racebox-next-row
        state           attending-racebox-next-row-2

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
;	state		free

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

=imaginal>
        footer-top      =val

==>

=imaginal>

=visual>

+visual-location>
	ISA		        visual-location
        kind                    image
        < screen-bottom         =val  
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
       ; first-race-col	        nil

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
; if the last row is finished, go back to the bottom left corner (left col) and double check

(P Find-RaceBox-Same-Row-No-Match-Last-Row

=goal>
	
        state           attending-racebox-last-row-2

?visual-location>
	buffer		failure

=visual>

=imaginal>
        instr-bottom    =bottom
         footer-top      =top
	
==>

=imaginal>
        first-race-col   true

+visual-location>
	ISA		     visual-location
        kind                 image
       <= screen-bottom      =top
       >= screen-top         =bottom
;       screen-x             lowest
;        screen-y            lowest
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
	left-race-col	        true
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
        first-race-col          true
        footer-top              =footertop

=visual>

=visual-location>
	screen-top	=current-top

==>

=visual>

=imaginal>

+visual-location>
        ISA		visual-location
        <= screen-top    =current-top
        <= screen-bottom =footertop
        kind             image
        :nearest         current

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

=imaginal>
        footer-top      =footertop

==>

=imaginal>

=visual>

+visual-location>
	ISA		        visual-location
        kind                    image 
        <= screen-bottom        =footertop
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

