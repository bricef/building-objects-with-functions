
# Purpose of this talk
 * Inspire
 * Transform
 * The Wizard Club
     - Lisp's reputation
     - The Neckbeard Hacker
     - Image of gandalf macbook "Ah... A Lisp Hacker"
 
# Why I'm giving this talk
 * My personal "Oh Shit" moment
 * What made me learn lisp

# How to watch this talk
 * Health Warning. If you're new to Lisp, this is a firehose. Don't fight it. 
 * You're not expected to remember the details
 * You should walk away with an appreciation for expressive power
 * Objects with functions is just an example.
 * It's the one I picked because it's the one that transformed me
 * I will Always refer back to the higher level picture 
 * All available online for you to go through on your own term.
 * You can get in touch with me directly through the website (in-context) or by email (brice@fractallambda.com)
 * This is a Nothing up my sleeve talk. Ie, everything is visble. That doesn't mean it's the best. Efficiency is ~~sometimes~~ always ignored for the sake of simplicity.
 * This was not built in 1h!

# Lisp primer
## The parentheses
Curly braces language 

    <object>.<verb>(<subject>) // man.eat(dog)

Lisp 

    (<verb> <object> <subject>) ; (eat man dog)

A better representation would be

    (<verb> <subject> <subject> ...) ; (eat dog cat spam eggs)

This is a **good** thing. So little syntax makes reading code much easier.


## First order functions
Functions can be values.

For example, in curly braces, if you have a

    o.f(g)-> h

Only `f` is code. `o`, is an object (namespace for code and holder of data), `g` and `h` are data.

In Lisp

    (f o g) -> h

All `f`, `o`, `g` and `h` can be functions. 

Trippy. Or is that foggy?

## Quoting and unquoting

The forms

    `a

and 

    (quote a)

are equivalent. They mean "The quoted thing as is".

You know lisp now. My work here is done, let's grab some beers.

# Programming Assumptions
> "Entities must not be multiplied beyond necessity" William of Ockham

> "When you assume, you're making an ass out of U and me." - Anonymous

Lists, first, rest, cond, eq?, empty? (in scheme) 

# Begin with the end in Mind

## The Core
The concept of "objects", 
Objects contain data (fields / attributes)
Objects contain code code (procedures / methods)
Methods can access and modify the data of the object with which they are associated ("this"). 
Objects interact with one another
Popular languages are class-based
Objects are instances of classes
Class == Type
Instances are individual objects
Instances choose what code and data to use when called
Late binding 'this' 

## Nice to have
Class-level data (class variables)
Class-level methods
Encapsulation (private members)
Inheritance? -> Composition better in my book, but ok.

## Nuttier than squirrel poop
Garbage collection
Compile time type checking -> Issue with Compilation! Let's make a type checker.


# The path we must take (Journey)
> "sooner or later you're going to realize just as I did that there's a difference between knowing the path and walking the path." - Morpheus

Associative arrays?
Garbage collection?



# Why does this matter? why do we care?
Power! I'm a power hungry maniac. 
Expressive power in particular. 
Let's just review:
we built an object system (the foundational paradigm for most people in this room) in a single talk. 
It's clear, concise, elegant, and readable code.

# Review of the entire code.
Image of final environment with breakdown of section in colour coded

    -----
    [---]-> State handling
    [---]-> Blah blah
    [---]-> Table implementation
    [---]-> GC
    -----

That's it. You've seen every line.

# Pedantic corner. "Ummm actually."
State management as pure fn isn't shown.
Yes, that's right. There's a lack of purity! OMG. 
Two options:
    - Manual state management (let form on the state data structure)
    - Monads come in - that's the topic for another talk.

There are more exotic options out there.

# What this is really about

 * Yes, this **is** a lisp sales pitch. If you run a lisp company, I'm happy to talk about comissions
 * But seriously,
 * This talk is not really about objects
 * What I've done is shown you an alternative way of developing software both in the result and the process.

# This is different(result): DSL
* DSL are Domain Specific Language
* A nice way of building complexity in functional languages
* What we've done is create a DSL for manipulating object.
* Now you know what a lisp hacker means when he says "OOP? Bah, that's a 100 line DSL"
* This approach was taken for OO language.
* You can use it for logical, declarative programming too. [SICP], [Minikanren], [core.logic]

> "Lisp isn't a language, it's a building material." - Alan Kay

* In practice, you'd never want to recreate an OO DSL, because the LISP construct are more powerful anyway.
* The reason we used this usecase is because it's familiar to most people in the room.

# This is different(process): SW vs HW
* This is in personal rant territory, so bear with me, we're nearly there.
* What most people build is hardware. They define things, then run the program once compiled. Even tests are just HW testing. It just happens so fast that most people don't realise that's what they're doing.
* This is **NOT** software. Most people's systems can be turned completely into hardware. We design the _Program as an artefact_
* What we've done is very different. We've interacted with a SW system. The program isn't an artefact we iteratively designed. The program is the recorded conversation we had with the system. This is _Program side effect of reasoning_

> "That language is an instrument of human reason, and not merely a medium for the expression of thought, is a truth generally admitted." - George Boole, quoted in Iverson's Turing Award Lecture

* The difference is qualitative, but when you've experienced the second, you'll never go back to the first willingly. We're programming at the speed of thought.



# Thank you for your time
I'd love to hear back from you about your journey from here on.
contact details.

Keep _Thinking Impossible Thoughts_ (- Edsger Dijkstra, CACM, 15:10)


# References

Guy Steele Growing A Language
On Lisp
Ansi Common Lisp
SICP
Minikanren
Core.logic
https://web.media.mit.edu/~minsky/papers/TuringLecture/TuringLecture.html

# Interest
http://cstheory.stackexchange.com/questions/21705/what-is-the-contribution-of-lambda-calculus-to-the-field-of-theory-of-computatio
http://www.pps.univ-paris-diderot.fr/~saurin/Enseignement/LMFI/articles/HindleyCardone06.pdf
http://cstheory.stackexchange.com/questions/1117/realizability-theory-difference-in-power-between-lambda-calculus-and-turing-mac
http://www.cs.unc.edu/~stotts/723/Lambda/overview.html
http://blog.sigfpe.com/2007/02/monads-in-c-pt-ii.html
http://cratylus.freewebspace.com/monads-in-javascript.htm
https://common-lisp.net/project/cl-monad-macros/monad-macros.htm
https://github.com/clojure/algo.monads
(very pretty) http://code.activestate.com/recipes/439361/
http://okmij.org/ftp/Scheme/monad-in-Scheme.html
http://okmij.org/ftp/Computation/monadic-shell.html
http://en.wikipedia.org/wiki/Functional_programming#Simulating_state
http://clojure.org/state?responseToken=a0f40cb78e2358b131a0234b0de0e22f
http://en.wikipedia.org/wiki/Declarative_programming
https://web.media.mit.edu/~minsky/papers/TuringLecture/TuringLecture.html







