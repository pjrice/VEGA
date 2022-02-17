(load "/home/ausmanpa/actr7.x/load-act-r.lisp")

(defun gen-n-syms (n)
    (loop for i from 1 to n
        append (list (gensym)) into gps
        finally (return gps)
    )
)

(add-act-r-command "gen-n-syms" 'gen-n-syms)

(run-environment)

(define-model dummyModel (sgp :force-visual-commands t))

; start act-r connection in python here

(load "/home/ausmanpa/gp/VEGA/experiments/E01/stimuli/ACT-R/stim01.lisp")
