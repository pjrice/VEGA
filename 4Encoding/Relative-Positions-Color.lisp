
;; Encodes the race and makes a request for a button associated with this race
;; No attend production because productions that lead into this have one
(P Encode-Race

=goal>
	state   	storing-race-group

=visual>
	value		=text
	
=visual-location>
	ISA			visual-location	
	kind			text
	group			=group1
	screen-left		=current-left
	screen-right	        =current-right
        screen-top	        =current-top
        screen-bottom           =current-bottom

=imaginal>
	race-group	none

==>

+visual-location>
	ISA			visual-location
        ;> screen-top            =current-top
        ;< screen-bottom         =current-bottom
	;>= screen-y		current

	;<= screen-x	        =current-left
	<= screen-x	        =current-right
        :nearest		current
	kind			oval
	
=imaginal>
	race-group  =group1
 
=goal>
	state		find-button-group	
	
!output! ("Example of race is: ~s" =text)


)

;; Attends a button for this race
(P Attend-Button

=goal>
	state		find-button-group

=visual-location>
	ISA			visual-location	
	kind		oval

?visual>
	state		free
	
==>

+visual>
	ISA			move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		storing-button-group

)

;; Makes a location request for a clicked button
(P Check-Button

 =goal>
	state   	storing-button-group

=visual-location>
	group		=button-group

==>

+visual-location>
	ISA		visual-location
	kind		oval
	group		=button-group
	color		black
 
=goal>
	state		find-clicked-button
	
	
)

;; We have already voted, send back to macronavigation
(P Already-Voted

=goal>
	state		find-clicked-button

=visual-location>
	ISA		visual-location	
	kind		oval
	
=visual>

;?visual>
;	state		free
	
==>
=visual>

=visual-location>


=goal>
	state		already-voted

)

;; We have already voted, send back to micronavigation
(P Not-Already-Voted

=goal>
	state		find-clicked-button


?visual-location>
	buffer			failure

?visual>
	state		free
	
==>


+visual-location>
	ISA		visual-location
	color		purple
	:nearest	current
	= screen-y	current
	
=goal>
	state		find-candidate-group

)

;; Attends a candidate for this race
(P Attend-Candidate

=goal>
	state		find-candidate-group

=visual-location>
	ISA		visual-location	
	kind		text

?visual>
	state		free
	
==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		storing-candidate-group

)

;; Encodes the candidate group and makes a visual request for the parties
(P Encode-Candidate

 =goal>
	state   	storing-candidate-group
	
=visual>
	value		=text

=visual-location>
	ISA		visual-location	
	kind		text
	group		=group2

=imaginal>
	- race-group	none
	candidate-group	none
	race-group	=race-group

==>

+visual-location>
	ISA		visual-location
	kind		text
	:nearest	current
	= screen-y	current
	color		blue

=imaginal>
	candidate-group  =group2
 
=goal>
	state		find-party-group
	
!output! ("Example of candidate is: ~s" =text)

	
)

; Attends the party
(P Attend-Party

=goal>
	state		find-party-group

=visual-location>
	ISA		visual-location	
	kind		text
	; width		=width
	; screen-x	=middle-x

?visual>
	state		free
	
==>

+visual>
	ISA			move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		storing-party-group

)

; Encodes the party group and sends us into voting
(P Encode-Party

 =goal>
	state   	storing-party-group

=visual>
	value		=text

?visual>
	state		free
	
=visual-location>
	ISA		visual-location	
	kind		text
	group		=group3

=imaginal>
	- race-group		none
	- candidate-group	none
	party-group		none
	

==>

=imaginal>
	party-group  =group3
 
=goal>
	state	     ready-to-make-choice
	
+visual>
	ISA     clear-all-finsts
	
!output! ("Example of party is: ~s" =text)
	
)

;for example race without party group
(P Find-Party-Failure

 =goal>
	state   	find-party-group

?visual-location>
	buffer		failure	

==>
 
=goal>
	state	     ready-to-make-choice
   
)
