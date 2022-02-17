
(defvar *window* nil)

(defvar *width* 50)
(defvar *height* 50)


(defun find-top-left-x (desired-center-x width)
    (- desired-center-x (/ width 2))
)



(defun find-top-left-y (desired-center-y height)
    (- desired-center-y (/ height 2))
)



(setf *window* (open-exp-window "test" :visible t :width 500 :height 500))

(add-button-to-exp-window *window* :x (find-top-left-x 150 *width*)  :y (find-top-left-y 100 *height*) :width *width* :height *height*)
(add-button-to-exp-window *window* :x (find-top-left-x 250 *width*)  :y (find-top-left-y 100 *height*) :width *width* :height *height*)
(add-button-to-exp-window *window* :x (find-top-left-x 350 *width*)  :y (find-top-left-y 100 *height*) :width *width* :height *height*)

(add-button-to-exp-window *window* :x (find-top-left-x 150 *width*)  :y (find-top-left-y 250 *height*) :width *width* :height *height*)
(add-button-to-exp-window *window* :x (find-top-left-x 250 *width*)  :y (find-top-left-y 250 *height*) :width *width* :height *height*)
(add-button-to-exp-window *window* :x (find-top-left-x 350 *width*)  :y (find-top-left-y 250 *height*) :width *width* :height *height*)

(add-button-to-exp-window *window* :x (find-top-left-x 150 *width*)  :y (find-top-left-y 400 *height*) :width *width* :height *height*)
(add-button-to-exp-window *window* :x (find-top-left-x 250 *width*)  :y (find-top-left-y 400 *height*) :width *width* :height *height*)
(add-button-to-exp-window *window* :x (find-top-left-x 350 *width*)  :y (find-top-left-y 400 *height*) :width *width* :height *height*)

(install-device *window*)

