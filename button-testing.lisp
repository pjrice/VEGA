(defvar *window* nil)
(defparameter button-state (make-hash-table))

(defun on-button-press (button)
    (if (= (gethash button button-state) 0)
        (progn
            (modify-button-for-exp-window button :color 'black)
            (setf (gethash button button-state) 1))
        (progn
            (modify-button-for-exp-window button :color 'white)
            (setf (gethash button button-state) 0))
    )
)

(defun button-test ()

    (setf *window* (open-exp-window "test" :visible t :width 500 :height 500))
    
    (let* (
        (xCoord 200))
        
        (loop 
            (let* (
                (b (add-button-to-exp-window *window* :color 'white :text "" :x xCoord :y 250 :width 20 :height 10 :action nil)))
                
                (setf (gethash b button-state) 0)
                (modify-button-for-exp-window b :action (list 'on-button-press b)))
                
            (setq xCoord (+ xCoord 50))
            (when (> xCoord 250) (return xCoord))
        )
    )
)
