(define-model testModel

(sgp    :v t
        :trace-detail high)

(chunk-type read-letters state)

(add-dm 
 (start isa chunk) 
 (attend isa chunk)
 (respond isa chunk) 
 (done isa chunk)
 (goal isa read-letters state start))

(P find-unattended-letter
   =goal>
      ISA         read-letters
      state       start
 ==>
   +visual-location>
      :attended    nil
   =goal>
      state       find-location
)

(P attend-letter
   =goal>
      ISA         read-letters
      state       find-location
   =visual-location>
   ?visual>
      state       free
==>
   +visual>
      cmd         move-attention
      screen-pos  =visual-location
   =goal>
      state       attend
)
 
(goal-focus goal)
 
)
