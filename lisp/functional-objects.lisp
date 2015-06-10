
;
; Required:
;  	first / car
;	rest / cdr
;	list / `()
;	equal
;	cond
;   cons
;	not
;	!! (to deal with Javascript's weirdness)
;	or, and, not
;	setq
;	


;
; ASSOCIATIVE ARRAYS
;


(defun Hash () `())

(defun second (l)
  (first (rest l)))

(defun third (l)
	(first (rest (rest l))))

(defun get-key (kv)
  (first kv))

(defun get-val (kv)
  (second kv))

(defun empty? (xs)
    (equal `() xs))

(defun get-in (m k)
  (cond 
    ((empty? m) nil)
    ((equal (get-key (first m)) k) (get-val (first m)))
    (:else (get-in (rest m) k))))

(defun concat (l1 l2)
	"Concatenates two lists together"
	(cond 
	    ((empty? l1) l2)
	    (l1 (cons (first l1) (concat (rest l1) l2)))))

(defun assoc (m k v)
  "Associates value 'v' to key 'k' in map 'm'"
  (cond 
    ((or (is-null m) (empty? m)) (list (list k v)))
    ((equal (get-key (first m)) k) (cons (list k v) (rest m)))
    (:else (cons (first m) (assoc (rest m) k v)))))


(defun dissoc (m k)
  (cond
    ((or (is-null m) (empty? m)) nil)
    ((equal (get-key (first m)) k) (rest m))
    (:else (cons (first m) (dissoc (rest m) k)))))

(setq test (assoc (assoc (Hash) "a" 1) "b" 2))
     

;
; The basis of methods
;

; We can now associate attributes to objects!
(setq shape (Hash))
(setq shape (assoc shape :height 4))
(setq shape (assoc shape :width 3))

; We can also add functions as values into our objects, because 
; we have first order functions
(setq shape 
	(assoc shape :area 
		(lambda (this) (* (get-in this :height) (get-in this :width)))))

((get-in shape :area) shape) ;-> 12

; we can warp this in syntactic sugar
(defun tell (obj message &opt args)
	((get-in obj message) obj args))

(tell shape :area)

; we can also use a macro to make property assignment nicer
(defmacro defprop (class name function)
	`(setq ,class (assoc ,class ,(to-keyword name) ,function)))



; We're now missing inheritance.
; We can create it by adding a parent attribute to the class
; and redefining a recursive get-in function.

(defun get-in (m k)
  (cond 
    ((empty? m) nil)
    ((equal (get-key (first m)) k) (get-val (first m)))
    (:else (get-in (rest m) k))))

(defun rget-in (m k)
	(let 
		((parent (get-in m :parent))
		 (v?	 (get-in m k)))
	(cond 
		((not (is-null v?)) v?)
		((not (is-null parent)) (rget-in parent k))
		(:else nil))))

; For example

(setq Mammal (Hash))
(setq Mammal 
	(assoc Mammal :pet 
		(lambda (this) (tell this :speak)) ))

(setq Dog (Hash))
(setq Dog (assoc Dog :speak (lambda (this) "Woof")))
(setq Dog (assoc Dog :parent Mammal))

(setq Cat (Hash))
(setq Cat (assoc Cat :speak (lambda (this) "Meow")))
(setq Cat (assoc Cat :parent Mammal))

; Of course, for this to work, we'll have to modify tell to use recursive get:
(defun tell (obj message &opt args)
	((rget-in obj message) obj args))

(tell Cat :pet) ;-> "Meow"
(tell Dog :pet) ;-> "Woof"

; We now have behavioural inheritence!

; In the On Lisp book, we'd now go into multiple inheritence,
; if I did this I might get lynched n stage, so might not be the best idea.

; What we have to realise is that at this point we have an OO language. 
; We're missing a lot of syntactic sugar though, so let's add some.

; (defun method (name methdef)
; 	(let ((fname (to-keyword (first methdef)))
; 		  (fargs (cons 'this (second methdef)))
; 		  (fbody (third methdef)))
; 		`(setq ,name (assoc ,name ,fname (lambda ,fargs ,fbody)))
; 	))


; (defmacro defclass (name par & methods)
; 	(progn
; 		(print name)
; 	(cons `progn 
; 		(cons `(setq ,name (Hash)) 
; 		    (map (lambda (meth) (makemethod name meth)) methods)))))
	

; (progn 
; 	(setq name (Hash))
; 	(setq name (assoc name :parent par))
; 	(setq name (assoc name :fname (lambda (this fargs) body)))
; )

; (defun new (class &opt parent)
; 	(cond 
; 		((is-undefined parent) (Hash))
; 		(:else (assoc )))

; (defclass Foo ()
;     (constructor (this) (set-props (:b 123)))
;     (foo (this) (+ 2 3))
;     (bar (this x) (* x (get-in this :b))))



; (defclass MyClass
; 	; an example method
; 	(area () 
; 		(* (this :width) (this :height)))

; 	; The constructor is just another method
; 	(constructor (w h)
; 		(set-props this ; but we've defined 'set-props' to make our life easier.
; 			(:width w)
; 			(:height h))))


