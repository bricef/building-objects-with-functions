(defun MakeTemplate (template-name)
	(let ((template (Handlebars.compile
				(funcall 
					($ (format nil "#template-%s" template-name))
					html))))
	(lambda (& props)
		(funcall ($ "#main-lesson-flow") append 
			(template (apply #'object props))))))

(setq Heading (MakeTemplate "heading"))
(setq Slide (MakeTemplate "slide"))
(setq Code (MakeTemplate "code"))
(setq Quote (MakeTemplate "quote"))

(Heading 
	:title "Welcome"
	:desc "To <i>Building Objects with functions</i>.")

(Heading
	:title "Lisp Primer"
	:desc "Getting started with Lisp")

(Heading
	:title "Assumptions"
	:desc "The base of our pyramid")

(Heading
	:title "Destination"
	:desc "Beginning with the end in mind")

(Slide 
	:title "Minimum viable Object Orientation (MVOOL?)"
	:content "
	<ol>
		<li>Objects have properties</li>
		<li>Objects respond to messages</li>
		<li>Objects Inherit properties from their parents</li>
	</ol>
	")

(Heading
	:title "Departing"
	:desc "Building a purely functional associative array")

(Heading
	:title "Arriving")

(Quote
	:said "The truth is of course is that there is no journey. We are arriving and departing all at the same time."
	:by "David Bowie")

(Heading
	:title "What this is really about")

(Slide
	:title "Not really about Objects"
	:content "
	<p><b>No</b>, this is not a sales pitch. <em>Honest!</em></p>
	<p>This is about <b>and alternative way of developing software</b></p>
	<p>What we've done here differs from conventional development in two major ways</p>
	<p>Firstly, we have a radically <b>different result</b> and outcome by using a Domain Specific Language to represent Object Orientation<p>
	<p>Secondly, we used a <b>different process</b> for getting to our result<p>
	")

(Quote 
	:said "Lisp isn't a language, it's a building material."
	:by "Alan Kay")

(Slide
	:title "A different result by using a DSL"
	:content "
	<p>DSLs are <em>Domain Specific Languages</em></p>
	<p>They are a <b>scalable</b> way of <b>abstracting complexity</b>. Functional languages are well suited for building DSLs, and <b>Lisp is particularly well suited</b> to this way of develping.</p>
	<p>What we've done so far is build a DSL for manipulating objects and recreate an OO language.<p>
	<p>You could <b>build a DSL for logical, declarative programming</b> too. Example of exactly this can be found in <a href=\"http://minikanren.org/\">Minikanren</a> or <a href=\"https://github.com/clojure/core.logic\">core.logic</a>. <a href=\"https://mitpress.mit.edu/sicp/\">SICP</a> also shows how to do this in details.</p> 
	<p>DSL crop up everywhere.</p>
	<p>For example, these slides are build using a DSL:</p>
	")

(Code 
	:disableEditor t
	:code "(Heading
	:title \"Lisp Primer\"
	:desc \"Getting started with Lisp\")")

(Slide 
	:content "
	<p>In practice, you will rarely want to recreate an OO language inside a functional one</p>
	<p>The construct you have available in Lisp tend to be more expressive anyway.</p>
	<p>We only used this as a usecase because everyone is very familiar with the domain.</p>
	")

(Slide
	:title "A different process by using the REPL"
	:content "
	<p>
	")

(Heading :title "Thank you for your time")

(Slide 
	:title "Stay in touch"
	:content "
	<p>I hope this will be the beginning of a bigger journey</p>
	<p/>Please <b>stay in touch</b> by tweeting to <a href=\"https://twitter.com/fractallambda\">@fractallambda</a> or emailing <a href=\"mailto:brice@fractallambda.com\">brice@fractallambda.com</a></p> 
	<p>And <b>share</b> where you take these ideas</p>
	")

(Slide
	:title "References"
	:content "
	<ul>
		<li><a href=\"https://www.youtube.com/watch?v=_ahvzDzKdB0\">Growing a Language (Guy Steele Jr.)</a></li>
		<li><a href=\"http://www.paulgraham.com/onlisp.html\">On Lisp (Paul Graham)</a></li>
		<li><a href=\"http://www.paulgraham.com/acl.html\">ANSI Common Lisp (Paul Graham)</a></li>
		<li><a href=\"http://minikanren.org/\">Minikanren</a></li>
		<li><a href=\"https://mitpress.mit.edu/sicp/\">Structure and Interpretation of Computer Programs (Harold Abelson, Gerald Jay Sussman and Julie Sussman)</a></li>
		<li><a href=\"https://github.com/clojure/core.logic\">Clojure's core.logic</a></li>
	</ul>")

(Slide 
	:title "Interesting links"
	:content "
	<ul>
		<li><a href=\"http://cstheory.stackexchange.com/questions/21705/what-is-the-contribution-of-lambda-calculus-to-the-field-of-theory-of-computatio\">What is the contribution of Lambda calculus to the field of theory of computation</a></li>
		<li><a href=\"http://www.pps.univ-paris-diderot.fr/~saurin/Enseignement/LMFI/articles/HindleyCardone06.pdf\">History of Lambda-calculus and Combinatory Logic</a></li>
		<li><a href=\"http://cstheory.stackexchange.com/questions/1117/realizability-theory-difference-in-power-between-lambda-calculus-and-turing-mac\">Differences between Lambda calculus and Turing Machine</a></li>
		<li><a href=\"http://www.cs.unc.edu/~stotts/723/Lambda/overview.html\"> Introduction to the Lambda Calculus</a></li>
		<li><a href=\"http://blog.sigfpe.com/2007/02/monads-in-c-pt-ii.html\">Monads in C</a></li>
		<li><a href=\"http://cratylus.freewebspace.com/monads-in-javascript.htm\">Monads in Javascript</a></li>
		<li><a href=\"https://common-lisp.net/project/cl-monad-macros/monad-macros.htm\">Common Lisp monad macro</a></li>
		<li><a href=\"https://github.com/clojure/algo.monads\">Clojure monads</a></li>
		<li><a href=\"http://code.activestate.com/recipes/439361/\">Python Monads (very pretty)</a></li>
		<li><a href=\"http://okmij.org/ftp/Scheme/monad-in-Scheme.html\">Monads in Scheme</a></li>
		<li><a href=\"http://okmij.org/ftp/Computation/monadic-shell.html\">Monadic Bash?</a></li>
		<li><a href=\"https://web.media.mit.edu/~minsky/papers/TuringLecture/TuringLecture.html\">Form and Content in Computer Science (Marvin Minsky)</a></li>
	</ul>
	")



(funcall ($ ".slide-editor") each 
	(lambda (index)
		(let (
				(editor window.editor)
				(snippet (ace.edit this))
				(button (funcall (funcall ($ this) parent) find "button"))
			 )
			(button.click (lambda (e) 
				(progn 
					(console.log editor)
					(editor.navigateFileEnd)
					(editor.insert (+ "\n" (snippet.getValue))))))
			(snippet.renderer.setPadding 20)
			(snippet.setOptions 
				(object 
					:maxLines 10000
					:readOnly t
					:highlightActiveLine f
					:padding 20
					:showGutter f
					:fontSize 20
					:mode "ace/mode/lisp"
					:theme "ace/theme/tomorrow_night_blue")))))

