


(defun button-test ()

    (let ((test-window (open-exp-window "test" :visible t :width 500 :height 500)))
    
        ; try a lambda function - this results in:
        ;"#|Warning: Error while calling "agi-win-click-mouse-monitor461" as a monitor for "click-mouse": Error #<TOO-FEW-ARGUMENTS Too few arguments in call to #<Compiled-function (:INTERNAL BUTTON-TEST) (Non-Global)  #x302001AD5E3F>: 0 arguments provided, at least 1 required. > occurred while trying to evaluate command "agi-win-click-mouse-monitor461" with parameters (NIL (558 557) INDEX) |#"
        (setf test-button1 (add-button-to-exp-window test-window :color 'white :text "" :x 200 :y 250 :width 20 :height 10 :action (lambda (button) (modify-button-for-exp-window button :color 'black))))
        
        ; try modifying the button with an action after it is initialized - this results in:
        ; "#|Warning: Error while calling "agi-win-click-mouse-monitor473" as a monitor for "click-mouse": Error #<TOO-FEW-ARGUMENTS Too few arguments in call to #<Compiled-function BUTTON-PRESS #x3020018E59DF>: 0 arguments provided, at least 1 required. > occurred while trying to evaluate command "agi-win-click-mouse-monitor473" with parameters (NIL (557 557) INDEX) |#"
        ; effectively the same as the lambda function
        (setf test-button2 (add-button-to-exp-window test-window :color 'white :text "" :x 250 :y 250 :width 20 :height 10 :action nil))
        (modify-button-for-exp-window test-button2 :action (list 'button-press test-button2))
        ;(modify-button-for-exp-window test-button2 :action (list "test-button-pressed" test-button2)) ; using a command results in the same as using the function
        
    )

)

(defun button-press (button)
    (modify-button-for-exp-window button :color 'black)
)

(add-act-r-command "test-button-pressed" 'button-press)


