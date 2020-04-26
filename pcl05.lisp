; basic skeleton of a function

 (defun name (parameters*)
   "Optional documentation string."
   body-form*)

; naming convetion
; usually alpha and hyphens (dont use _ and camelCase)
; converter function -> (string->widget)

; if not parameters, the list is empty -> ()

; string literal directly after parameter list is a documentation string
; it should describe purpose of function
; can be fethced by (documentation 'name 'function)

; the body consists of any number of lisp expressions
; they are evaluated in order
; by default the value of the last expression is returned as the value of the function
; return-from special operator can be used instead

(defun hello-world ()
  (format t "hello, world"))

(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))
; value of (+ x y) becomes the return value


; REQUIRED PARAMETERS
; if parameters with simple names (like in verbose-sum) are required parameters (function call with too few or too many will signal an error)

; OPTIONAL PARAMETERS
; symbols that folow &optional are optional parameters
(defun foo (a b &optional c d)
  (list a b c d))
(foo 1 2) ; => (1 2 NIL NIL)
(foo 1 2 3) ; => (1 2 3 NIL)
(foo 1 2 3 4) ; => (1 2 3 4)
; a and b are reqired, c and d are optional
; less than 2 or more than 4 parameters will still signal an error

; to set default to something other than NIL you can pass a list of name and an expression
; this expression will be evaluated only if not enought arguments are passed
; usually this expression is just a default value
(defun foo (a &optional (b 10))
  (list a b))
(foo 1 2) ; => (1 2)
(foo 1) ; => (1 10)

; these expressions can refer to the parameters before them (can be dynamically set)
(defun make-rectangle (width &optional (height width))
  (format t "making rectangle w: ~d, h: ~d.~%" width height))
(make-rectangle 10)
(make-rectangle 10 20)
; here -> if height not set, its set to to the same value as width

; to make sure if the caller supplied optional parameter (even if same as default) we can add another symbol into list for parameter, a supplied-p variable (naming convention)
; this variable will be set true if caller set the optional parameter and nil if not
(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))
(foo 1 2) ; => (1 2 3 NIL)
(foo 1 2 3) ; => (1 2 3 T)
(foo 1 2 4) ; => (1 2 4 T)

; REST PARAMETERS
; some functions and special operators may need vairable number of parameters

;these are all legal calls
(format t "hello, world")
(format t "hello, ~a" name)
(format t "x: ~d y: ~d" x y)
(+)
(+ 1)
(+ 1 2)
(+ 1 2 3)

; instead of writing many optional parameters we can use rest parameters
; after &rest symbol, all remaining parameters will be grouped into one single list

; so format and + probably look soemthing like this
	; (defun format (stream string &rest values) ... )
	; (defun + (&rest numbers) ... )

; KEYWORD PARAMETERS
; sometimes in a function call we want to set the third optional parameter without setting the first two
; we can do this with keyword parameters (which are not positional)
; keyword parameters define which value goes with which parameters
; keyword parameters are defined similarly to optional parameters but with &key symbol
(defun foo (&key a b c)
  (list a b c))
; to assign a keyword parameter in function call you write its name starting with : and than its value
(foo) ; => (NIL NIL NIL)
(foo :a 1) ; => (1 NIL NIL)
(foo :b 1) ; => (NIL 1 NIL)
(foo :c 1) ; => (NIL NIL 1)
(foo :a 1 :c 3) ; => (1 NIL 3)
(foo :a 1 :b 2 :c 3) ; => (1 2 3)
(foo :a 1 :c 3 :b 2) ; => (1 2 3)
; if keyword parameter is not passed, it will be set to its default value
; as with optional parameters, you can specify the default and supplied-p as well as you can reference parameters earlier in the parameter list
(defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))
(foo :a 1) ; => (1 0 1 NIL)
(foo :b 1) ; => (0 1 1 T)
(foo :b 1 :c 4) ; => (0 1 4 T)
(foo :a 2 :b 1 :c 4) ; => (2 1 4 T)

; you can also make it so that calling keyword parameter has different name as actual parameter (different name in function call as in function itself)
; instead of the name of paramter, make a list with keyword for calling the function and the name for the paramter
(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))
(foo :apple 10 :box 20 :charlie 30) ; => (10 20 30 T)
; this can be useful for api building where you want descriptive (long) function call parameters but want to use short names internally

;dont mix optional and key (use just key)
(defun foo (x &optional y &key z)
  (list x y z))
(foo 1 2 :z 3) ; => (1 2 3)
(foo 1) ; => (1 NIL NIL)
(foo 1 :z 3) ; => ERROR
; because :z is taken in as y optional parameter

; when mixing rest and key both things happen
; all remaining parameters (including keywords) are asigned to a single list and all keyword paramters are asigned their value
(defun foo (&rest rest &key a b c)
  (list rest a b c))
(foo :a 1 :b 2 :c 3); => ((:A 1 :B 2 :C 3) 1 2 3)


; function return values
; by default the value of the last expression in the function is returned as the value of the function
; this is also most commonly used
; to manually return any value from any part of the function we can use return-from special operator
; (return-from isnt actually tied to function but to blocks of code (but defun creates a block around body of the function so it works here))

; first "argument" of the return-from special operator is the name of the function/block
; this name isn't evalueate and thus isn't quoted

(defun foo (n)
  (dotimes (i 10)
	(dotimes (j 10)
	  (when (> (* i j) n)
		(return-from foo (list i j))))))
; returns the first pair of numbers lower than 10 whose product is greater than the argument
(foo 10)

; yeah, naming the function you want to return is a bit of a hastle
; but because all expressions (including loops and conditionals) in lisp evaluate to a value, the return-from operator isn't ass common as return operator in c-like languages


; functions in lisp are just another kind of object
; so we can treat them as data (similar comcept as functino pointers in c)

(defun foo (x)
  (* 2 x))
(function foo)
; 'function' special operator, when given a name, returns the function object
; #'foo ; (works in repl)
; #'x is equivalent to (function x) (just like 'x is same as (quote x))

; when we haethe function object, we can invoke it

; if we know the number of arguments, we can use FUNCALL
(funcall #'foo 1 2 3) ; this is equivlent to (foo 1 2 3) so a little pointless if we know the name of the function
; first argument of funcall is the function object and the rest are to argument to be passed to it

; a bit more sensible use of funcall
(defun plot (fn min max step)
  (loop for i from min to max by step do
		(loop repeat (funcall fn i) do
			  (format t "*"))
		(format t "~%")))
(plot #'exp 0 4 1/2)
; plot takes function object as an argument
; it plots a simple ascii-art histogram of the values returnd by the argument function when it's invoked on the values from min to max, stepping by step.
; (we go from min to max using step of step, and call the argument function with the current value, and print the number of * as is the return value of this current argument fucntion call)
;NOTE: we dont use #' inside of a function because we want it to be interpreted as a variable (because the variable value is the function object) and we rely on the caller of plot to provide us with the function object

; but if we have, (for instance) values to pass to plot function in a single list named plot-data, the call would look like this:
(plot (first plot-data) (second plot-data) (third plot-data) (fourth plot-data))
; which is functional but less than ideal

; in this case we can use APPLY
; instead of individual arguments, apply expects a list
; so the call above can be written like this:
(apply #'plot plot-data)
; additionally, apply can take "loose" arguments, as long as the last one is a list
; so if plot-data only contained min max and step values, you would call plot like this:
(apply #'plot #'exp plot-data)
; apply doesn't care if function takes optional, rest or key paramters, as long as the loose arguments + argument list create a legal call


; ANONYMOUS FUNCTIONS
; when using functions as arguments of other funtions it's annoying to define and name a whole function that is only used once
; that's why we can create anonymous function using lambda
(lambda (parameters) body)
; lambda is basically a special kind of function name, where the name itself directly describes what the function does
; thats why we can use lambda expression in place of function name with #'
(funcall #'(lambda (x y)(+ x y)) 2 3)

; we can even use lambda expression as the name of a function in a function call expression
((lambda (x y) (+ x y)) 2 3) ; (defun foo (x y) (+ x y)) + (foo 2 3)
; but this is rarely done

; anonymous functions are useful when you need to pass a function as an argument to another function, and the function you need to pass is simple enough to be expressed inline

; so instead of:
(defun simple-double (x)
  (* 2 x))
(plot #'simple-double 0 10 1)
; we can easily write:
(plot #'(lambda (x) (* 2 x)) 0 10 1)

; another important use is for creating closures (functions that capture part of the environment where they are created) (but more about that in the future)
