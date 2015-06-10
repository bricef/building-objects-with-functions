


(console.log "Setup OK!")

(defun source (name) 
	"Shows the source code of a function or macro is available."
	(cond 
		((== undefined name) (format nil "Symbol not defined!"))
		((== undefined name.source) "Source not available")
		(:else name.source)))


(defun cons (x xs)
	"Adds 'x' to the beginning of 'xs'"
	(array-concat (list x) xs))