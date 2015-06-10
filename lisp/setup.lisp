


(console.log "Setup OK!")

(defun source (name) 
	"Shows the source code of a function or macro is available."
	(cond 
		((== undefined name) (format nil "Symbol not defined!"))
		((== undefined name.source) "Source not available")
		(:else name.source)))

