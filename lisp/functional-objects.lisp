(defun cons (x xs)
	(array-concat (list x) xs))
;
; Required:
;  	first
;	rest
;	list / `()
;	equal
;	cond
;	cons
;

(defun Hash () `())

(defun second (l)
  (first (rest l)))

(defun get-key (kv)
  (first kv))

(defun get-val (kv)
  (second kv))

(defun empty? (xs)
    (equal `() xs))

(defun get-in (m k)
  (cond 
    ((empty? m) m)
    ((equal (first (first m)) k) (get-val (first m)))
    (:else (get-in (rest m) k))))

(defun concat (l1 l2)
	"Concatenates two lists together"
	(cond 
	    ((empty? l1) l2)
	    (l1 (cons (first l1) (concat (rest l1) l2)))))

(defun assoc (m k v)
  "Associates value 'v' to key 'k' in map 'm'"
  (cond 
    ((empty? m) (list (list k v)))
    ((equal (get-key (first m)) k) (cons (list k v) (rest m)))
    (:else (cons (first m) (assoc (rest m) k v)))))

(defun dissoc (m k)
  (cond
    ((empty? m) m)
    ((equal (get-key (first m)) k) (rest m))
    (:else (cons (first m) (dissoc (rest m) k)))))

(setq test (assoc (assoc (Hash) "a" 1) "b" 2))
     
     