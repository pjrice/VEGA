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

    (setf *window* (open-exp-window "test" :visible t :width 1000 :height 1000))
    
    (add-image-to-exp-window *window* "box" "lb.gif" :x 150 :y 150 :width 678 :height 574 :clickable nil)
    
    (let* (
        (xCoord 50)
        (xCoordMod 50)
        (bWidth 20)
        (bHeight 10)
        )
        
        (loop 
            (let* (
                (b (add-button-to-exp-window *window* :color 'white :text "" :x xCoord :y 250 :width bWidth :height bHeight :action nil)))
                
                (setf (gethash b button-state) 0)
                (modify-button-for-exp-window b :action (list 'on-button-press b)))
                
            (setf xCoord (+ xCoord xCoordMod))
            (setf xCoordMod (floor (* xCoordMod 1.5)))
            (when (> xCoord 200) (setf bWidth (floor (* bWidth 2.25))))
            (when (> xCoord 200) (setf bHeight (floor (* bHeight 2.25))))
            (when (> xCoord 750) (return xCoord))
        )
    )
)
