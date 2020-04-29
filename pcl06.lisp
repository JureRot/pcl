; there are two types of variables in lisp
; lexical (local) and dynamic (global)

; all variables are references to objects

; BASICS
; variables in lisp are loosely/dynamically typed (when ti comes to declaring and detecting errors)
; but common lisp behaves as strictly/strongly typed language in a sence that all type errors will be detected
; there is no way to treat an object as an instance of a class that it is not

; all values in lisp are in concept references to objects
; thus assigning new value to variable changes what object the variable refers to but has not effect on the previously referenced object
; but if the variable holds the reference to a mutable object, you can use that reference to modify the object itself

; defining a function with DEFUN defines variables to hold the argumetns that are passed to it when its called
; these variables are called bindings (which are just a runtime manifestation of a variable)
(defun foo (x y z)
  (+ x y z))
; assigning new value to a function parameter will not change the original value of the variable passed to the function
; but again, if the object referenced is mutable, we can change it within the function, and that change will be seen to the caller (becaues the function is referencing the same object)

; LET special operator also introduces new variables
(let (variable*)
  body-form*)
; each initialization form is either a list of variable name and initial value or just the name (which will be initalizied to NIL)
(let ((x 10) (y 20) z)
  ...)
; initalizes x=10, y=20, z=NIL
; when let is evaluated, inital values are evaluated first, than the bindings are created and set to the value, than the body is executed
; again, these are bindings, only referenced within the let body (not global scope)
; they can be referenced whithin the form that introduced the varaible (binding form)

(defun foo (x)
  (format t "Parameter: ~a~%" x)       ; |<------ x is argument
  (let ((x 2))                         ; |
	(format t "Outer LET: ~a~%" x)     ; | |<---- x is 2
	(let ((x 3))                       ; | |
	  (format t "Inner LET: ~a~%" x))  ; | | |<-- x is 3
	(format t "Outer LET: ~a~%" x))    ; | |
  (format t "Parameter: ~a~%" x))      ; |

(foo 1)

; LET* variation of LET lets us reference the variables already initalized in the initalize list (not just in the body as with LET)
; so this is incorrect:
(let ((x 10)
	  (y (+ x 10)))
  (list x y))
; but this isnt:
(let* ((x 10)
	   (y (+ x 10)))
  (list x y))

; but we could use nested lets:
(let ((x 10))
  (let ((y (+ x 10)))
	(list x y)))

; DOTIMES is another binding form
(dotimes (x 10)
  (format t "~d" x))


; LEXICAL VARIABLES AND CLOSURES
; by default binding forms introduce lexically scoped variables
; lexically scopped variables can only be referenced by code that is textually within the same binding form
; this is similar to local variables in other programming languages (, but with a twist)

(let ((count 0))
  #'(lambda () (setf count (1+ count))))
; ther reference to count within lambda form is lexiacally legal
; but this annonymous function will be returned as the value of the let form and could be invoked (via funcall) from outside the scope of the let
; AS IT HEPPENS:
; the binding will stick around for as long as it is needed
; (in this case; for as long as someone holds onto the reference of the function object returned by let, the binding of count will still exist)
; in this case the annonymous function is called a closure (because it closes over the binding created by let)

; the thing about closures is, that it's the binding (not the value of the variable) that's captured
; thus we can assign new values to the closure, that will persist between calls to the closure

(defparameter *fn* 
  (let ((count 0))
	#'(lambda ()
		(setf count (1+ count)))))
; we capture the closure above in a global variable
(funcall *fn*)
; each time we invoke it, the count will increase

; one closure can close over multiple bindings simply by refering to them
; single binding can be captured by multiple closures

(let ((count 0))
  (list 
	#'(lambda () (incf count))
	#'(lambda () (decf count))
	#'(lambda () count)))
; this returns a list of 3 closures of the same binding


; DYNAMIC / SPECIAL VARIABLES
; dynamic variables are like global variables in other programming languages
; they can be referenced to from anywere in the program
; in other programming languages use of global variables is bad practice, but in lisp they are more useful and more manageable

; global variables can be defined with DEFVAR or DEFPARAMETER
; they both take a variable name, initial value, and an optional documentation string
; naming convention states, that global variables start and end with *
(defvar *count* 0 "Count of widgets made so far.")
(defparameter *gap-tolerance* 0.001 "Tolerance to be allowed in widget gaps.")
; defvar will assign new value only if the variable is undefined.
; also defvar can be passed without initial value and will create unbound variable (withou value)

(defun increment-widget-count ()
  (incf *count*))

; standard input and ouput are also global variables
;but for instance, if we want to write something instead of to std-out to a file, we can change value of std-out global variable to a file stream.
; this would work, but we need to remember to change it back when we dont need that functionality any more, or all the rest of the program will instead of the std-out write to file.

; here is where we can use dynamic variables
; like lexical variables, they have a scope
; lexical variables can be referenced by code within the same binding form
; dynamic variables can be referenced by code invoked during the execution of same binding form

; all global variables are dynamic

; so that std-out example:
(let ((*standard-out* *some-other-stream*))
  ...)
; temporary redefine *standard-out*
; all the code rune within let block will use redefined *standard-out* value
; after that the value will return to whatever it was before the let block

(defvar *x* 10)
(defun foo ()
  (format t "X: ~d~%" *x*))

(foo)
; will dynamically look for *x*, so at the top level it will print 10

(let ((*x* 20))
  (foo))
; use let to temporary shadow the global binding
; foo called within this block will print 20

(foo)
; if we run foo again outside let, the *x* will be 10 again (back to normal)

(defun bar ()
  (foo)
  (let ((*x* 20))
	(foo))
  (foo))

(bar)
; here is that better demonstrated
; the variable *x* is not passed, but it still changes value inside let block

; as with lexical bindings, assigning new value only effects the current binding
(defun foo ()
  (format t "Before assignment~18tX: ~d~%" *x*)
  (setf *x* (+ 1 *x*))
  (format t "After assignment~18tX: ~d~%" *x*))

(bar)
; the middle call doesnt see the global binding, just the one let assigned
; and again, after let finishes, the value goes to what was before

; lisp defines all variables created with defvar and defparameter as globally special
; so when it's called within let or as function parameter or any construct that creates new variable bindings, those bindings will be dynamic
; that's why naming global variables with * is important (so you know confuse lexical variables with dynamic)

; it is possible to decalre name locally special (and the binding created for that variable will be dynamic and not lexical)
; other code can locally declare name special to refer to the dynamic binding
; (but this isnt commonly done)

; use dynamic varaibles only when you need to take advantege of changeing the behavior of downstream code or the posibility that downstream code changes the value of binding established before


; CONSTANTS
; all constants are global and are defined with DEFCONSTANT (same form as defparameter)
(defconstant name inital-value-form [ documentation-string ])
; after defining, the name can be used only to refer to the constant
; it cant be used as a function parameter or rebound with any binding form
; naming convention is that constant names start and end with +

; it is posible to redefine a constant by reevaluating defconstant with a different inital-value-form value (but many implementations will require you to reevaluate any code that refers to that constant)
; so use it for true constants (else use defparameter)


; ASSIGNMENT
; to get the value of the binding just refer to the varaible (because symbol evalueates to the value of the varaible it names)
; to assign new value to the binding use SETF macro
(setf place value)
; setf is a macro and first examins place form
; when place is a variable, it expands into a call of SETQ special operator
(setf x 10)

; again, assigning a new value to a binding has no effect on any other bindings of that variable
; it also doesn't have any effect on the value that stored in the binding before assigning
; (changing a variable only effets it inside its scope)
(defun foo (x)
  (setf x 10))
; setf will have no effect on any value outside foo

(let ((y 20))
  (foo y)
  (print y))
; will print 20 and not 10

;setf can assign multiple places in sequence
; instead of this:
(setf x 1)
(setf y 2)
; we can do this:
(setf x 1 y 2)

; because setf returns the newly assigned value you can nest them
(setf x (setf y (random 10)))
; will set x and y to the same random value


; GENERAL ASSIGNMENT
; data can be held in composite data structures not just variables
; these are arrays, hash tables, list and user-defined structures
; these will be covered later, but the gist is that setf can usually set values to all those places

; setf is simillar as = assignment operator in c-based/algol-derived languages
;                             PYTHON  |  LISP
;         variable:           x = 10  |  (setf x 10)
;    array element:        a[0] = 10  |  (setf (aref a 0) 10)
; hash table entry: hash['key'] = 10  |  (setf (gethash 'key hash) 10)
;  field in object:     o.field = 10  |  (setf (field o) 10)

; note: aref is array acces function, gethash does a hast tabel lookup, and field is a function that accesses a slot named field in user-defined object

; so just like = behaves, setf is used the same on a variable as it is on the bigger object
; and the place is modified without effecting previously stored object or the rest of the data structure


; OTHER WAYS OF MODIFY PLACES
; some assignment patterns are sufficiently common to have their own operators

; INCF and DECF
(setf x (+ x 1))
; and
(setf x (- x 1))
;can be done with incf and decf modify macros:
(incf x) ; == (setf x (+ x 1))
; and
(decf x) ; == (setf x (- x 1))

; incf and decf default to 1, but value can be passed
(incf x 10) ; == (setf x (+ x 10))

; modify macros are more concise to use than setf
; on top of that, they are written in a way that place expression is only evaluated once

(incf (aref *array* (random (length *array*))))
; will increment value of a random element in array
; doing this with setf is not soo eays
(setf (aref *array* (random (length *array*)))
	  (1+ (aref *array* (random (length *array*)))))
; this will not work, because we get two different elements (because random is called twice)
; the way incf evalueates to something more like this:
(let ((tmp (random (length *array*))))
  (setf (aref *array* tmp) (1+ (aref *array* tmp))))
; so just use incf and decf

; PUSH, POP, and PUSHNEW
; will be covered later

; ROTATEF and SHIFTF

; rotatef swaps the values of two variables and returns nil
(rotatef a b)
; if a and b are variables this is equivalent to:
(let ((tmp a)) (setf a b b tmp) nil)
; for other types of places, this call would be more complex, but rotate knows how to handle it

;shiftf shifts the values
; the value of the last argument is moved to the second-to-last one
; the value of the second-to-last argument is moved to the third-to-last one
; ...
; and returns the original value of the first argument
(shiftf a b 10)
; for variables this is equivalent to:
(let ((tmp a)) (setf a b b 10) tmp)

; rotatef and shiftf cant take any number of argments and will (like all modify macros) evaluate them once from left to right
