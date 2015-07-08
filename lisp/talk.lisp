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
(setq Image (MakeTemplate "img"))

(Heading 
	:title "Welcome"
	:desc "To <i>Building Objects with functions</i>.")

(Image :src "img/gandalf.jpg")

(Slide
	:title "Why I'm giving this talk"
	:content "
	<p>What I'm about to present is the exact approach that made 
	me learn functional programming.</p>
	<p>I first encountered it in the last chapter of <em>On Lisp</em> by 
	Paul Graham. You can find the <b>full text online for free</b>.</p>
	<p>Reading that chapter was the moment that made me learn Lisp.</p>
	")
; # Why I'm giving this talk
;  * My personal "Oh Shit" moment
;  * What made me learn lisp

(Slide
	:title "The purpose of this talk"
	:content "
	<p>This talk has two main aims</p>
	<ul>
		<li>To inspire you to learn more</li>
		<li>To transform some of you the way I was</li>
		<li>To explore the basic concepts of OOP from a differnet angle</li>
	</ul>")

;  * The Wizard Club
;      - Lisp's reputation
;      - The Neckbeard Hacker
;      - Image of gandalf macbook "Ah... A Lisp Hacker"

(Slide
	:title "What this talk is not about"
	:content "
	<p>This is not about learning Lisp.</p>
	<p>This is not about re-writing existing object systems</p>
	")

(Slide
	:title "How to watch this talk"
	:content "
	<p>The talk (in the form you see now) will be placed online and a link will go on the meetup page</p>
	<ul>
		<li>Don't try to follow every step. Especially if you don't know Lisp.</p>
		<li>Details don't matter that much right now.</p>
		<li>Try and walk away with an appreciation for power and expressiveness</li>
	</ul>
	")

; # How to watch this talk
;  * Health Warning. If you're new to Lisp, this is a firehose. Don't fight it. 
;  * You're not expected to remember the details
;  * You should walk away with an appreciation for expressive power
;  * Objects with functions is just an example.
;  * It's the one I picked because it's the one that transformed me
;  * I will Always refer back to the higher level picture 
;  * All available online for you to go through on your own term.
;  * You can get in touch with me directly through the website (in-context) or by email (brice@fractallambda.com)
;  * This is a Nothing up my sleeve talk. Ie, everything is visble. That doesn't mean it's the best. Efficiency is ~~sometimes~~ always ignored for the sake of simplicity.
;  * This was not built in 1h!

(Heading
	:title "Lisp Primer"
	:desc "Getting started with Lisp")

(Slide 
	:title "What's with these parentheses..."
	:content "
	<p>The first thing anyone notices is the parentheses.</p>
	<p>Newcomers from <b>curly braces languages</b> like C, Java,
	C#, etc... will be familiar with the following:</p>")

; ## The parentheses
; Curly braces language 

;     

(Code :code "
/*
 * <object>.<verb>(<subject>) 
 */
man.eat(dog)")



; Lisp 

;     (<verb> <object> <subject>) ; (eat man dog)

(Slide :content "<p>Lisp just places the parentheses in different places:")

(Code :code "
;
; (<verb> <object> <subject>)
;
(eat man dog)")

(Slide :content "
	<p>Although the difference between subject and objects isn't as clear cut as in
	curly braces languages, and you're really better off writing:</p>")

(Code :code "
;
; (<verb> <subject> <subject> ...)
;
(eat dog cat spam eggs)")


; A better representation would be

;     (<verb> <subject> <subject> ...) ; (eat dog cat spam eggs)

; This is a **good** thing. So little syntax makes reading code much easier.

(Slide :content "
<p>This syntax is a <b>good thing</b>!</p>
<p>We'll see why that is at the end when we write our first macro</p>
")

(Slide :title "First Class Functions" :content "
<p>In Lisp, functions can be values too. </p>
<p>For example, in curly braces languages you might have:</p>
")

(Code :code "
o.f(g); //-> h")

(Slide :content "
<p>In this case, only <tt>f</tt> is a function.</p> 
<p>In Lisp, where you might have:</p>")
; ## First class functions
; Functions can be values.

; For example, in curly braces, if you have a

;     o.f(g)-> h

; Only `f` is code. `o`, is an object (namespace for code and holder of data), `g` and `h` are data.

; In Lisp

;     (f o g) -> h

(Code :code "
(f o g) ;-> h")

(Slide :content "
<p>All of <tt>f</tt>, <tt>o</tt>, <tt>g</tt> and <tt>h</tt> can be functions!</p>
<p>Let's take a look at an example.</p>")

; All `f`, `o`, `g` and `h` can be functions. 

(Code :code "
(defun say-something () 
	\"Something\") ; Note how we don't need 'return'

(defun say-nothing () 
	\"Nothing\")

(defun say-maybe (sayingA sayingB)
	(lambda (dosay?)
		(if dosay? (sayingB)
		(sayingA))))
")

(Slide :content "
<p>And the result of <tt>say-maybe</tt> can be used as a function :)</p>
<p>Now you basically know lisp! Let's go for Pizza.</p>")



; Trippy. Or is that foggy?

; ## Quoting and unquoting

; The forms

;     `a

; and 

;     (quote a)

; are equivalent. They mean "The quoted thing as is".

; You know lisp now. My work here is done, let's grab some beers.

(Heading
	:title "Assumptions"
	:desc "The base of our pyramid")

(Quote 
	:said "Entities must not be multiplied beyond necessity"
	:by "William of Ockham")

; > "When you assume, you're making an ass out of U and me." - Anonymous


(Slide :title "What can we rely on"
	:content "
<p>It turns out, very little.</p>
<p>Here is the list of all primitives we'll be using in the talk:</p>
<ul>
<li><tt>first</tt> or <tt>car</tt></li>
<li><tt>rest</tt> or <tt>cdr</tt></li>
<li><tt>list</tt></li>
<li><tt>equal</tt></li>
<li><tt>cond</tt></li>
<li><tt>cons</tt></li>
<li><tt>or</tt></li>
<li><tt>and</tt></li>
<li><tt>not</tt></li>
<li><tt>setq</tt></li>
<li><tt>defun</tt></li>
<li><tt>defmacro</tt></li>
</ul>
")
; Lists, first, rest, cond, eq?, empty? (in scheme) 


(Heading
	:title "Destination"
	:desc "Beginning with the end in mind")


; ## The Core
; The concept of "objects", 
; Objects contain data (fields / attributes)
; Objects contain code code (procedures / methods)
; Methods can access and modify the data of the object with which they are associated ("this"). 
; Objects interact with one another
; Popular languages are class-based
; Objects are instances of classes
; Class == Type
; Instances are individual objects
; Instances choose what code and data to use when called
; Late binding 'this' 

; ## Nice to have
; Class-level data (class variables)
; Class-level methods
; Encapsulation (private members)
; Inheritance? -> Composition better in my book, but ok.

; ## Nuttier than squirrel poop
; Garbage collection
; Compile time type checking -> Issue with Compilation! Let's make a type checker.



(Slide 
	:title "Minimum viable Object Orientation (MVOOL?)"
	:content "
	<ol>
		<li>Objects have properties</li>
		<li>Objects respond to messages</li>
		<li>Objects Inherit properties from their parents</li>
	</ol>
	<p>Ugliness is permitted for the sake of time if we know how we could improve it in the future.</p>
	")

(Heading
	:title "Departing"
	:desc "Building a purely functional associative array")

; # The path we must take (Journey)
; > "sooner or later you're going to realize just as I did that there's a difference between knowing the path and walking the path." - Morpheus

; Associative arrays?
; Garbage collection?


(Slide 
	:title "Picking a place to start"
	:content "
<p>We'll build our objects from associative lists.</p>
<p>Most languages have a similar datastructure. You might know it by the following names:<p>
<ul>
<li>Maps</li>
<li>Dictonaries</li>
<li>Hashtables</li>
<li>Associative Arrays</p>
</ul>
<p>Naturally, because we're building things from scratch, we don't have this.</p>
<p>All modern lisps have such a datastructure, for what it's worth.</p>
<p>We're building it here because it's instructive</p>
<p>This isn't going to be efficient!</p>")

(Slide
	:title "A Basic API"
	:content "
<p>Let's take a look at what our API might look like</p>")

(Code :code "
; Creation
(Hash)

; Associate a value to a key
(assoc Hash :key value) ;-> new hash

; Getting a value out
(get-in myHash :key) ;-> value

; Removing a value
(dissoc myHash :key) ;-> New Hash
")

(Slide
	:title "Our implementation" 
	:content "
<p>We'll use a list of key-value pairs.</p>
<p>The final structure will look something like this:</p>")

(Code :code "
(
	(:a 1)
	(:b 2)
)")

(Slide :content "
<p>Let's get started with some simple utility functions</p>")

(Code :code "
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
")

(Slide 
	:title "Implementing the API" 
	:content "
<p>Let's start Implementing the API</p>
<p>We'll begin by accessing the values:</p>")

(Code :code "
(defun get-in (m k)
  (cond 
    ((empty? m) 
    	nil)
    ((equal (get-key (first m)) k) 
    	(get-val (first m)))
    (:else 
    	(get-in (rest m) k))))")

(Slide :content "<p>We can continue by adding a new element</p>")

(Code :code "
(defun assoc (m k v)
  \"Associates value 'v' to key 'k' in map 'm'\"
  (cond 
    ((or (is-null m) (empty? m)) 
    	(list (list k v)))
    ((equal (get-key (first m)) k) 
    	(cons (list k v) (rest m)))
    (:else 
    	(cons (first m) (assoc (rest m) k v)))))")

(Slide :content "<p>And finally by disassociating the values</p>")

(Code :code "
(defun dissoc (m k)
  (cond
    ((or (is-null m) (empty? m)) nil)
    ((equal (get-key (first m)) k) (rest m))
    (:else (cons (first m) (dissoc (rest m) k)))))")

(Slide :title "Let's test!"
	:content "
<p>Well, that wasn't so bad!</p>
<p>Now we have our associative lists what does that give us?</p>")

(Heading :title "Moving to objects")

(Slide 
	:title "How far can we go with just the Hash"
	:content "
<p>It turns out, some way!</p>
<p>We can use the hash and first class function to build a good approximation</p>")

(Code :code "
; We can now associate attributes to objects!
(setq shape (Hash))
(setq shape (assoc shape :height 4))
(setq shape (assoc shape :width 3))

; We can also add functions as values into 
; our objects, because we have first 
; class functions
(setq shape 
	(assoc shape :area 
		(lambda (this) 
			(* (get-in this :height) 
			   (get-in this :width)))))

; And use these functions by extracting 
; them manually
((get-in shape :area) shape) ;-> 12
")

(Slide :title "So how far to go?"
	:content "
<p>Well, we could start by improving the syntax of calling methods</p>")

(Code :code "
(defun tell (obj message &opt args)
	((get-in obj message) obj args))")

(Slide :content "
<p>And for defining properties:</p>")

(Code :code "
; we can also use a macro to make 
; property assignment nicer
(defmacro defprop (class name function)
	`(setq ,class 
		(assoc ,class 
			,(to-keyword name) 
			,function)))")

(Slide :title "Now, for Inheritance"
	:content "
	<p>Now we've satisfied our first two points. Let's move on to Inheritance.</p>
	<p>We get this by adding a parent attribute and looking recursively up the parent 
	hierarchy for a definition.</p>
	<p>Let's start with our old definition of get-in...</p>
")

(Code :code "
; This is the definition of get-in we used.
(defun get-in (m k)
  (cond 
    ((empty? m) nil)
    ((equal (get-key (first m)) k) (get-val (first m)))
    (:else (get-in (rest m) k))))
")

(Slide :content "<p>And we can use it in our recursive definition</p>")

(Code :code "
(defun rget-in (m k)
	(let 
		((parent (get-in m :parent))
		 (v?	 (get-in m k)))
	(cond 
		((not (is-null v?)) v?)
		((not (is-null parent)) (rget-in parent k))
		(:else nil))))")

(Slide :content "<p> We'll need to redefine <tt>tell</tt> to use our new function</p>")

(Code :code "
(defun tell (obj message &opt args)
  ((rget-in obj message) obj args))")

(Slide :title "What does this allow us to do?"
	:content "<p>Let's take a look</p>")

(Code :code "
(setq Mammal (Hash))
(setq Mammal 
	(assoc Mammal :pet 
		(lambda (this) (tell this :speak)) ))

(setq Dog (Hash))
(setq Dog (assoc Dog :speak (lambda (this) \"Woof\")))
(setq Dog (assoc Dog :parent Mammal))

(setq Cat (Hash))
(setq Cat (assoc Cat :speak (lambda (this) \"Meow\")))
(setq Cat (assoc Cat :parent Mammal))



(tell Cat :pet) ;->\"Meow\"
(tell Dog :pet) ;-> \"Woof\"")


(Heading
	:title "Arriving")

(Slide :title "Taking a step back"
	:content "<p>Let's review how far we've got so far.</p>")

(Quote
	:said "The truth is of course is that there is no journey. We are arriving and departing all at the same time."
	:by "David Bowie")

(Heading :title "Why do we care?")

(Image :src "img/hoc.jpg")
; # Why does this matter? why do we care?
; Power! I'm a power hungry maniac. 
; Expressive power in particular. 
; Let's just review:
; we built an object system (the foundational paradigm for most people in this room) in a single talk. 
; It's clear, concise, elegant, and readable code.

; # Review of the entire code.
; Image of final environment with breakdown of section in colour coded


(Slide :content "
	<p>Note that much is missing here! We've got quite a long way to get to the complete object system used by other languages</p>
	<p>Hopefully you can see the bones of the system starting to come out.</p>")

;     -----
;     [---]-> State handling
;     [---]-> Blah blah
;     [---]-> Table implementation
;     [---]-> GC
;     -----

; That's it. You've seen every line.

; # Pedantic corner. "Ummm actually."
; State management as pure fn isn't shown.
; Yes, that's right. There's a lack of purity! OMG. 
; Two options:
;     - Manual state management (let form on the state data structure)
;     - Monads come in - that's the topic for another talk.

; There are more exotic options out there.

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
	:title "A different process by using interactive development"
	:content "
	<p>We haven't fully explored this aspect. Development can become 
	even more interactive. For example, because code is data, the environment 
	can be serialised to disk as lisp code any time.</p>
	<p>There are some much more sophisticated tools to do this.</p>
	<p>For example, <a href=\"http://lighttable.com/\">Light Table</a> allows you to reload code 
	in the browser at the function level, to replace individual functions while
	they're being called in an update loop.</p>
	")

(Slide
	:title "Artefact vs Side Effect"
	:content "
	<p>This way of develping software is different to the approach you usually take.</p>
	<p>Traditionally, the program is <em>an artefact that you create</em>.</p>
	<p>We perform discovery to figure out what the problem is and what the solution could be.</p>
	<p>By using the repl to explore the problem, we're working in a different paradigm.<p>
	<p>The program is the result of the conversation we've had with the system.</p>
	<p>It becomes <em>a side effect of the discovery</em>. The feedback loop is much shorter.</p>
	")

(Quote
	:said "That language is an instrument of human reason, and not merely a medium for the expression of thought, is a truth generally admitted."
	:by "George Boole")
(Slide
	:content "<p>The difference is qualitative, and experiencing the second mode
	of development will spoil you.</p>
	<p>You'll never want to go back to <em>Program as Artefact</em>.</p>")

(Heading :title "Thank you for your time")

(Slide 
	:title "Please stay in touch"
	:content "
	<p>I hope this will be the beginning of a bigger journey</p>
	<p>There's still much to improve on on what we've done.</p>
	<p/>Please <b>stay in touch</b> by tweeting to <a href=\"https://twitter.com/fractallambda\">@fractallambda</a> 
	or emailing <a href=\"mailto:brice@fractallambda.com\">brice@fractallambda.com</a> if you take this any further or take any actions as a reuslt of this evening</p> 
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
					:showGutter f
					:fontSize 20
					:mode "ace/mode/lisp"
					:theme "ace/theme/tomorrow_night_blue")))))

