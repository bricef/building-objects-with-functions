
; This is a comment

; Setting a variable in the environment
(setq toes 10)

; Defining a function
(defun square (x)
    "This is a docstring"
    (* x x))

; which is syntactic sugar for 
(setq square 
    (lambda (x) 
        "This is a docstring" 
        (* x x)))