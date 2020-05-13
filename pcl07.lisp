; lisp supports both functions and classes to extend the language

; it also supports a type called marcros
; NOTE: macros in lisp are not like macros in other languages (like c)

; each macro defines its own syintax to convert passed s-expressions into lisp forms
; so control structures (when, dolist, loop) and definitional forms (defun, defparameter) can be part of the "standard library" and not the core of the language (defun is a macro, while def in python is hardwired part of the core of python)
; but when you use them (call them) you dont have to care if they are macros or (in other languages part of the language core)

; in this lesson, some of control-construct macros will be introduced as an example of some of the things that can be done with macros
; next lesson will focus on creating out own macros


; if special operator is the most basic form of conditional exectuion

;(if condition then-form [else-form])

(if (> 2 3) "Yes" "No") ; => "No"
(if (> 2 3) "Yes") ; => NIL
(if (> 3 2) "Yes" "No") ; => "Yes"
; but if isnt the best syntactic construct because then and else form are restricted to bein single lisp forms (so sequences of actions must be wrapped in another form)
; one of these wrapping forms is PROGN (executes any number of forms in order and returns the value of the last form)
(if (spam-p current-message) 
    (progn 
      (file-in-spam-folder current-message)
      (update-spam-database current-message)))
; this is ok, but not ideal
; but because this is quite common action, we would like to abstract if + progn together
; this is exactlly what standart common lisp macro WHEN does

; WHEN
(when (spam-p current-message) 
  (file-in-spam-folder current-message)
  (update-spam-database current-message))
; when the first arugument/form is true (not-nil) evaluate the rest of the arguments/forms

;when macro is part of the standard library, but if it wasnt, we cluld define it like this:
(defmacro when (condition &rest body) 
  `(if ,condition (progn ,@body)))
; backquote notation (`): only symbols after comma (,) will be evaluated
; @ symbol means that the result (in form of a list) (in our case list of forms/action we want to execute) is spliced into an existing list
; (1 2 (list 3 4)) => (1 2 (3 4))
; (1 2 @(list 3 4)) => (1 2 3 4)


; UNLESS
; unless is a counterpart to when
; evalueates the body if condition is false
(defmacro unless (condition &rest body) 
  `(if (not ,condition) (progn ,@body)))


; COND
; writing long if, else-if, else condition is not hard with just if operator, but it it ugly
(if a
    (do-x)
    (if b 
        (do-y)
        (do-z)))
; if a do x, else if b do y, else do z

; so comon lisp has a macro for expressing multibranch conditionals called COND
(cond 
  (test-1 form*)
		.
		.
		.
  (test-N form*))

; the previous expression can be written like this:
(cond 
  (a (do-x))
  (b (do-y))
  (t (do-z)))
; will evaluate conditions of each branch in order (a, b c) until one is true
; will evaluate all forms in the body of that branch
; will return the value of the last evaluated form in that body (of the value of the condition if the body is empty)
; convention is to write the last (else) branch with t (true) condition

; this is basically a switch statement


; AND, OR, and NOT
; AND, OR and NOT are boolean logic operators that come handy when using IF, WHEN, UNLESS and COND
; not is strictly speaking a function (not a macro) but will be grouped here for its semantic relevance

; NOT takes one argument and inverts its truth value (returning T if argument is NIL and NIL otherwise)

; AND takes any number of subforms and returns NIL if any of them evaluate to NIL or it returns the value of the last subform if none of them are NIL

; OR again takes any number of subforms and returns the value of the first NON-NIL subform or it returns NIL if all of them are NIL

;AND and OR stop at the first subform that determines the overall truth value

(not nil)				; => T
(not (= 1 1))			; => NIL
(and (= 1 2) (= 3 3))	; => NIL
(or  (= 1 2) (= 3 3))	; => T


; LOOPING
; all lisp's looping control constructs are macros (abstractions) build on top of two special operators (tagbody and go)
; the most basic looping construst is DO, and two easier to use but less general are DOLIST and DOTIMES
; another (sometimes polorizing) construct is LOOP


; DOLIST
; loops over items of a list, executing the body form with the variable holding the current item of the list
(dolist (var list-form)
  body-form*)
; first the list-form is evalueated to produce a list
; then for each item in list, the body of the loop is evalueated with variable var that is holding the current itmem in the loop

(dolist (x '(1 2 3))
  (print x))
; used this way dolist evalueates to NIL

; if we want to break out of dolist, we can use RETURN
(dolist (x '(1 2 3))
  (print x)
  (if (evenp x) (return)))


; DOTIMES
; dotimes allows us to count the loops
; it works similarlly as dolist, but instead of a executing the body once for each element in list, the body is executed the number of times specified
(dotimes (var count-form)
  body-form*)
; count-form must evaluate to an integer
; var will hold successive integers from 0 to n<count-form

(dotimes (i 4)
  (print i))

; again, we can break with RETURN

; both loops can also be nested
(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))
; (1+ x) and (1+ y) are used because dotimes list start counting with 0


; DO
; dolist and dotimes are convenient and easy, but not flexible enough for all usecases (stepping over multiple variables, using arbitrary expression for ending the loop, ...)
; DO lets us bind any number of variables and gives us control on how they chane with each step of the loop
; it also lets us define the test that determines when to end the loop and the form to evalueate at the end to use as a return/evalueated value of the do form itself

(do (variable-definition*)
    (end-test-form result-form*)
    statements*)

; variable-definition is a lists containing (var init-form step-form)
; init-form will be evaluated at the beginning of the loop and the resulting value will be bound to variable var
; before each iteration of the loop the step-form will be evaluated and new value will be assigned to var
; step-form is optional. if left out var will keep its value between iterations if not explicitly assigned new value within the body of the loop
; if init-form is left out, the variable will be set to NIL
; at the end of each iteration the end-test-form is evaluated. if evaluated to NIL, the next iteration begins
; when end-test-form evaluates to true, the result-forms are evaluated and the value of the last one is returned as the value of the do expression

; step-forms for all the variables are evaluated together, and than are all the new values assigned, so we can refer to other loop variables in step-form
; (note: DO* does not behave like this but assigns new value imediately after evaluation)
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))
; (1+ n), next and (+cur next) are evaluated using the old values
; only after all of them have been evaluated are the new values assigned

; because in do, we can step multiple variables and have a result-forms, we offen dont need the body
; othertimes, when we use do as control construct, we dont need the result-form
; so sometimes can be a bit cryptic

; an easy way to orient yourself is by using the parentheses.
; there will always be 3 main pairs
; one to encloses variable declaration
; one that encloses end-test and result forms
; and one that encloses the whole do expression

; example one:
(do ((i 0 (1+ i)))
    ((>= i 4))
    (print i))
; we dont have any result form
; this would be the same as:
(dotimes (i 4)
  (print i))

; example two:
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))
; this is bodyless fibonacci generator

; example three:
(defvar *some-future-date*)
(setf *some-future-date* (+ 20 (get-universal-time)))
(do ()
    ((> (get-universal-time) *some-future-date*))
    (format t "Waiting~%")
    (sleep 5))
; this do loop binds no variables, but we still require an empty list
; this loop compares current time (get-universal-time) with some global variable (in this case set to 20 sec in the future) and prints every 5 seconds


; the mighty LOOP
; for looping over data structures, accumulating values while looping, collecting, counting, summing, minimizging, maximizing, ... DO, DOLIST and DOTIMES arent the best
; in this case using LOOP will be easier

; loop macros has two versions. simple and extended

; simple LOOP
(loop 
  body-form*)
; infinite loop that doesnt bind any variables
; to end we need to use RETURN

; example three from before:
(setf *some-future-date* (+ 20 (get-universal-time)))
(loop 
  (when (> (get-universal-time) *some-future-date*)
    (return))
  (format t "Waiting~%")
  (sleep 5))

; extended LOOP
; extended loop implements certain loop keywords (sortof like its own language)
; this language is veri unlike-lisp, but can be of much use

(do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
    (push i nums))
; this collects the numbers from 1 to 10 into a list
; create 2 vars: nums (empty list) and i (counter that increases)
; until i > 10, we push i to nums
; when i >= 10, we ouput reversed nums (nreverse)

; same can be done with loop as:
(loop for i from 1 to 10 collecting i)

; example one:
(loop for x from 1 to 10 summing (expt x 2))
; sum of first 10 squares
; (EXPT is exponent function/macro)

; example two:
(loop for x across "the quick brown fox jumps over the lazy dog" 
      counting (find x "aeiou"))
; counts the number of vowels in the sentance

; example two:
(loop for i below 10 
      and a = 0 then b
      and b = 1 then (+ b a)
      finally (return a))
; this returns the eleventh fibonacci number (like example two for DO)

; across, and, below, collecting, counting, finally, for, from, summing, then, and to are extended LOOP keywords
; but the rest of the code is regular lisp code

; just a reminder: LOOP is still just a macro
