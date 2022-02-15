
(defvar *window* nil)
(defvar *current-button* nil)

(defparameter button-state (make-hash-table))


(defun button-test ()

    (setf *window* (open-exp-window "test" :visible t :width 500 :height 500))
    
    (setf *current-button* (add-button-to-exp-window *window* :color 'white :text "" :x 250 :y 250 :width 20 :height 10 :action "test-button-pressed"))
    (setf (gethash *current-button* button-state) 0)


)

(defun button-press ()
    
    (let ((button *current-button*))
        (if (= (gethash button button-state) 0)
            (progn
                (modify-button-for-exp-window button :color 'black)
                (setf (gethash button button-state) 1))
            (progn
                (modify-button-for-exp-window button :color 'white)
                (setf (gethash button button-state) 0))
        )
    )
    
    
)

(add-act-r-command "test-button-pressed" 'button-press)
