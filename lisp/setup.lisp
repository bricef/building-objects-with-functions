


(console.log "Setup OK!")

(defun source (x) 
	"Shows the source code of a function or macro is available."
	(if (== undefined x.source) "not available"
		x.source))