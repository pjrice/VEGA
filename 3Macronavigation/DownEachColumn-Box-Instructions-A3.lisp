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
;;; Filename    : DownEachColumn-Box-Instructions-A3.lisp
;;; Version     : 1
;;; 
;;; Description : Macronavigation strategy (version A: cursory strategy)
;;;				: * This strategy will handle the races from top to bottom for all columns. (start from the middle column, then right, then left)
;;;				: * If starting, first finds the header. Then find the instructions in the top left corner (below the header). Then footer.
;;;				: * Then middle column and right column, next instr-column (left column)
;;;				: * the model will miss the races under instructions if the instructions-box is too long (>= screen-top  400)
;;;				: * This model can also handle non-race content in columns (during the voting process).
;;;
;;; Bugs        : * None known
;;;
;;; To do       : * There are a few places where I have had to cheat to make the model work (because we do not have access to relative group 
;;;				: * positios or super and sub groups).
;;;
;;;
;;; ----- History -----
;;; 2022.03.01   Xianni Wang
;;;				: * Adjusted productions to skip left-instructions
;;; 2022.01.23   Xianni Wang
;;;				: * Added productions to read and skip left-intructions (if text's height is over 300)
;;;				: * Added productions to handle/skip non-race content in columns (between races & before/after races).
;;; 2022.01.20   Xianni Wang
;;;				: * Added productions to handle ballots without footer.
;;; 2022.01.17   Xianni Wang
;;;				: * Added productions to accommodate 2 left-instructions ballot layouts (ballot instructions 1-2)
;;; 2021.11.10   Xianni Wang
;;;				: * Created the file; modified from "DownEachColumn-Box-Instructions-1.lisp"
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; This model can handle non-race content (header, footer, left-instructions).
;;; This model uses the primary macronavigation strategy observed in subjects (at least when they are given an in order ballot): top to bottom and 
;;; then left to right (start from the middle column, then right, then left). It only finds the piece of text in the next expected spot; it does not check if that piece of text is a race title.
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
        instr-race-col          nil

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

(P Find-Instr-Box

=goal>
	state		finding-instructions

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
	state		attending-instr-box
)

;attend box in the left-top corner
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

;****************************************
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

;****************************************
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
	state          attend-instr-box-second-time

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
	state          attend-instr-box-second-time

)

; Attend the current box
(P Attend-Current-Box-Instr

=goal>
	state		attend-instr-box-second-time
	
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
       > width                  300
        screen-y	        highest

=goal>
	state		attending-footer-box

)

;the following two productions are for: 
;if no footer on the ballot, go to next step (start voting)
(P Finding-Footer-No-Match

=goal>
	state     attending-footer-box
	
?visual-location>
	buffer    failure

==>

=goal>
	state     finding-racebox-middle-column-directly

!output! ("No footer found")

)

(P Find-RaceBox-Middle-Column-Directly

=goal>
	state		finding-racebox-middle-column-directly
     
=imaginal>
        header-bottom   =bottom
        instr-right     =right
  
==>

=imaginal>

+visual-location>
	ISA		      visual-location
        kind                  image
        > screen-top          =bottom
        > screen-left         =right
        screen-x              lowest
        screen-y              lowest
=goal>
	state		attending-racebox-middle-column

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
	state		finding-racebox-middle-column
	
)

; find the racebox in the middle column
(P Find-RaceBox-Middle-Column

=goal>
	state		finding-racebox-middle-column

=visual>

=visual-location>
        kind            image
;	screen-top	=current-top
     
=imaginal>
        header-bottom   =bottom
        instr-right     =right
  
==>

=imaginal>

+visual-location>
	ISA		      visual-location
        kind                  image
        > screen-top          =bottom
        > screen-left         =right
        screen-x              lowest
        screen-y              lowest

=goal>
	state		attending-racebox-middle-column

)

(P Attend-RaceBox-Middle-Column

=goal>
	state		attending-racebox-middle-column
	
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
	state		finding-oval-current-racebox	
	
)

;check if current box is a racebox
(P Find-Buttons-Current-Race-Box

=goal>
	state		finding-oval-current-racebox

=visual-location>
	ISA		visual-location
        kind            image
	screen-right	=current-right
	screen-bottom	=current-bottom
	screen-left	=current-left
	screen-top	=current-top
     
==>

+visual-location>
	ISA		  visual-location	
	kind		  oval
	<= screen-x	  =current-right
        <= screen-y       =current-bottom
        >= screen-x	  =current-left     
        >= screen-y       =current-top
        screen-y          lowest

=goal>
	state		finding-current-racebox-second-time

)

;buttons found, move back to racebox and attend it
(P Find-Current-Racebox-Again

=goal>
	state		finding-current-racebox-second-time

=visual-location>
	ISA		visual-location
	kind		oval
	screen-right	=current-right

?visual>
	state		free
	
==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
+visual-location>
	ISA		  visual-location	
	kind		  image
	>= screen-x	  =current-right
        :nearest	  current
	
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

;if no buttons found in the current racebox, then navigate to the next race
(P Finding-Race-Content-No-Match

=goal>
	state     finding-current-racebox-second-time
	
?visual-location>
	buffer    failure

=imaginal>
	instr-bottom   =val
        
==>

=imaginal>

+visual-location>
	ISA		visual-location
        kind            image
        >= screen-top   =val
     ; <= screen-right   =current-right
        :nearest        current

=goal>
	state		attending-race-title

!output! ("Non-race content")

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

;=visual>

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
	state		finding-oval-current-racebox2	
	
)

;check if current box is a racebox
(P Find-Buttons-Current-Race-Box2

=goal>
	state		finding-oval-current-racebox2

=visual-location>
	ISA		visual-location
        kind            image
	screen-right	=current-right
	screen-bottom	=current-bottom
	screen-left	=current-left
	screen-top	=current-top
     
==>

+visual-location>
	ISA		  visual-location	
	kind		  oval
	<= screen-x	  =current-right
        <= screen-y       =current-bottom
        >= screen-x	  =current-left     
        >= screen-y       =current-top
        screen-y          lowest

=goal>
	state		finding-current-racebox-second-time2

)

;buttons found, move back to racebox and attend it
(P Find-Current-Racebox-Again2

=goal>
	state		finding-current-racebox-second-time2

=visual-location>
	ISA		visual-location
	kind		oval
	screen-right	=current-right

?visual>
	state		free
	
==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
+visual-location>
	ISA		  visual-location	
	kind		  image
	>= screen-x	  =current-right
        :nearest	  current
	
=goal>
	state		attending-racebox-first-column2
)

(P Attend-RaceBox-First-Column2

=goal>
	state		attending-racebox-first-column2
	
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

;if no buttons found in the current racebox, then navigate to the next race
(P Finding-Race-Content-No-Match2

=goal>
	state     finding-current-racebox-second-time2
	
?visual-location>
	buffer    failure

=imaginal>
	instr-bottom   =val
        
==>

=imaginal>

+visual-location>
	ISA		visual-location
        kind            image
        >= screen-top   =val
     ; <= screen-right   =current-right
        :nearest        current

=goal>
	state		attending-race-title

!output! ("Non-race content")

)

; reuse the following two productions
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

; ; This production being called means that we have reached the end of a column, so we begin the process of finding the top race in the next row

(P Find-RaceBox-Same-Column-No-Match

=goal>
	state		attending-racebox-same-column	

?visual-location>
	buffer		failure

=imaginal>		
        instr-race-col  nil
	
==>

=imaginal>	

=goal>
	state		find-top-race

)

;****************************************
; This is the next of the productions that find the next race if it is in a different column (after find-race-same-column-no-match)
; It finds the top race in this column to prepare for the switch to the next column
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
	state		finding-oval-current-racebox3
	
)

(P Find-Buttons-Current-Race-Box-3

=goal>
	state		finding-oval-current-racebox3

=visual-location>
	ISA		visual-location
        kind            image
	screen-right	=current-right
	screen-bottom	=current-bottom
	screen-left	=current-left
	screen-top	=current-top
     
==>

+visual-location>
	ISA		  visual-location	
	kind		  oval
	<= screen-x	  =current-right
        <= screen-y       =current-bottom
        >= screen-x	  =current-left     
        >= screen-y       =current-top
        screen-y          lowest

=goal>
	state		finding-current-racebox-second-time3

)

;buttons found, move back to racebox and attend it
(P Find-Current-Racebox-Again-3

=goal>
	state		finding-current-racebox-second-time3

=visual-location>
	ISA		visual-location
	kind		oval
	screen-right	=current-right

?visual>
	state		free
	
==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
+visual-location>
	ISA		  visual-location	
	kind		  image
	>= screen-x	  =current-right
        :nearest	  current
	
=goal>
	state		finding-race
)

;if no buttons found in current racebox (non-race content), find current box, then navigate to the next race
(P Finding-Race-Content-No-Match-3

=goal>
	state     finding-current-racebox-second-time3
	
?visual-location>
	buffer    failure

=imaginal>
	instr-bottom   =val
  
==>

=imaginal>

+visual-location>
	ISA		visual-location
        kind            image
        >= screen-top   =val
     ; <= screen-right   =current-right
        :nearest        current

=goal>
	state		attending-race-title

!output! ("Non-race content!")

)



; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 
; ; If there is nothing found when looking for a new row, we are at the bottom right corner of the ballot and there are no more race boxes, 
; ; so we can check if there is any race listed below instructions
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

;  navigate to the left-instr
(P Find-RaceBox-Next-Col-No-Match

=goal>
	state		attending-racebox-next-column

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
	state		 attending-current-left-instr-box2
;	state		end
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
	state		end
	
)

