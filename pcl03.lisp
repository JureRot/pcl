(list 1 2 3)
; simple list

(list :a 1 :b 2 :c 3)
; plist (property list)
; every other element (starting with the first) is a symbol describing the element that follows
; symbols that start with : are called keywords (basically a name)
; (function for list and plist is same, the content is different)

(getf (list :a 1 :b 2 :c 3) :a)
(getf (list :a 1 :b 2 :c 3) :c)
; getf takes a plist and symbol and returns value in plist after that symbol
; (acts like a poor man's hash table)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))
; define function make-cd that takes four arguments and puts them in a plist

(make-cd "Roses" "Kathy Mattea" 7 t)
; example of calling make-cd (creating one record)

(defvar *db* nil)
; define global variable *db* (and init it to nil)
; asterisks (*) are lisp convention for naming global variables

(defun add-record (cd)
  (push cd *db*))
; (abstracting the push macro by defining a function) (good practice)
; (push returns the new value of the var its modifying)

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

*db*
; calling the var itself also returns its contents

(defun dump-db ()
  (dolist (cd *db*)
	(format t "~{~a:~10t~a~%~}~%" cd)))
; dolist loops over all elements of *db* and assigns them to variable cd
; for each value of cd we call format funtion to print it out

; format directives
; ~a : aestetic directive (consumes one argument and outputs it in a human readable form (no "" and :))
(format t "~a" "Dixie Chicks")
(format t "~a" :title)
; ~t : tabulatin directive (doesn't consume any arguments, just makes enough space) (~10t makes enough space to move to the 10 column before processing next directive)
(format t "~a:~10t~a" :artist "Dixie Chicks")
; ~{, ~} : list directive (next argument must be a list) (loops over the list, processing the directive between ~{ and ~}, consuming as many elements of the list as needed)
; ~% : new-line directive (doesn't consume any arguments, emits a new line) (similar to \n)

; using list directive we could omit dolist from out dump-db function
(defun dump-db2 ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))
; output a prompt to global var *query-io* connected with io stream to the terminal
; force-output is neccessary in some implementations to print the prompt without new line
; we read the line of *query-io*
;prompt-read func will return the return value of read-line (which is the string it read)

(defun prompt-for-cd_basic ()
  (make-cd 
	(prompt-read "Title")
	(prompt-read "Artist")
	(prompt-read "Rating")
	(prompt-read "Ripped [y/n]")))
; prompt-read returns sting (not good for rating and ripped)

(defun prompt-for-cd ()
  (make-cd 
	(prompt-read "Title")
	(prompt-read "Artist")
	(or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
	(y-or-n-p "Ripped [y/n]: ")))
; parse-integer parses integer from string
; :junk-allowed t is for parse-integer to not throw an error if it cant parse an int and is more leniant
; if parse-integer cant parse an int it returns NIL
; or takes a series of expresions and returns the first non-nil one (we use it to set default value to 0)
; y-or-n-p is a robust function for y/n prompt that reprompts if input incorect and returns boolean

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
		(if (not (y-or-n-p "Another? [y/n]. ")) (return))))
; loop/repead adding record using add-record+prompt-for-cd until prompted to stop (another? -> n)
; when y/n prompt is n, we return/break

;(add-cds) ; commented to not trigger when loading

(defun save-db (filename)
  (with-open-file (out filename
					   :direction :output
					   :if-exists :supersede)
	(with-standard-io-syntax
	  (print *db* out))))
; with-open-file opens a file, binds the stream to a variable, executes a set of expresions and closes the file
; with-open-file takes a few parameters:
	; out -> name of variable of stream
	; filename -> name of file
	; and options:
		; :direction :ouput -> opening a file for writing
		; :if-exists :supersede -> overwrite if file exists
; expresion inside with-open-file that we execute is print
; we use print instead of format because output of print can be read by lisp reader
; we put print inside with-standard-io-syntax to set certain vars that affect the behavior of print to their standard values (we will do the same when reading)

;(save-db "~/Documents/git/other/pcl/pcl03.db") ; commented to not trigger when loading

(defun load-db (filename)
  (with-open-file (in filename)
	(with-standard-io-syntax
	  (setf *db* (read in)))))
; no need to specify the :direction because default is :input
; setf is assignment operator (sets first arg to result of evaluating the second arg)
; read is the same reader used by the REPL
; warning: *db* variable will be overwriten

(load-db "~/Documents/git/other/pcl/pcl03.db")

(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
; remove-if-not creates a new list with all elements that match the predicate removed
; evenp function returns true if its argument is even
; #' notation is so, that list know this is function name (else it would search for evenp variable)

(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))
; we can create function inside remove-if-not (anonymous function / lambda) (this does the same as above)
; (lambda (x) (= 0 (mod x 2))) -> checks that its argument modulo 2 is equal to 0
; for odd it would be (lambda (x) (= 1 (mod x 2)))

(remove-if-not
  #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")) *db*)
; getf gets plist value from variable
; equal compares two string

(defun select-by-artist (artist)
  (remove-if-not
	#'(lambda (cd) (equal (getf cd :artist) artist))
	*db*))
; and we can wrap it in a function
; note: lambda function has reference to artist variable

(defun select (select-fn)
  (remove-if-not select-fn *db*))
; we can pass function as the attribute to the function
; here we want lisp to use the select-fn variable, so we don't use #'
(select #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")))
; we pass lambda function directly as atribute (and here we need #')

(defun artist-selecor (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))
; this is a function that returns a function
; note: this works as expected, even if artist reference doesn't exist after artist-selecor returns

(select (artist-selecor "Dixie Chicks"))


(defun foo (a b c) (list a b c))
; normal function declaration (needs three arguments)
(foo 1 2 3) ; => (1 2 3)

(defun bar (&key a b c) (list a b c))
; and this is using keyword parameters
(bar :a 1 :b 2 :c 3) ; => (1 2 3)
(bar :c 3 :b 2 :a 1) ; => (1 2 3)
(bar :a 1 :c 3) ; => (1 NIL 3)
(bar) ; => (NIL NIL NIL)
; we can pass them in any order and can omit any of them (in this case set to NIL)

; sometimes we want to diferentiate between NIL that is set if parameter not passed and normal NIL value
; for keyword parameter, instead of simple name, we can pass a set of name, default value and supplied-p parameter
; supplied-p parameter will be true or false depending if the parameter was passed in function call
(defun foo-bar (&key a (b 20) (c 30 c-p)) (list a b c c-p))
; here we demonstrate this functionality
(foo-bar :a 1 :b 2 :c 3) ; => (1 2 3 T)
(foo-bar :c 3 :b 2 :a 1) ; => (1 2 3 T)
(foo-bar :a 1 :c 3) ; => (1 20 3 T)
(foo-bar) ; => (NIL 20 30 NIL)


; name where because sql
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
	  (and
		(if title (equal (getf cd :title) title) t)
		(if artist (equal (getf cd :artist) artist) t)
		(if rating (equal (getf cd :rating) rating) t)
		(if ripped-p (equal (getf cd :ripped) ripped) t))))
; if title parameter is passed
	; get title from cd
	; compare it to title parameter
; if not passed
	; return true (so it matches all)

; we do this for all paramters and join them in logical and
; this and is the body of the anonymous function we return (this is our general selector-function generator)

; for ripped we need supplied-p parameter because we need to diferentiate between passed NIL, meaning non-ripped cds, and NIL meaning that :ripped wasnt passed

(select (where :artist "Dixie Chicks"))
(select (where :rating 10 :ripped nil))


(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
		(mapcar
		  #'(lambda (row)
			  (when (funcall selector-fn row) ; gets items/cds that match the select-fn
				(if title (setf (getf row :title) title)) ; if title passed, sets valuse of :title in item to new title
				(if artist (setf (getf row :artist) artist))
				(if rating (setf (getf row :rating) rating))
				(if ripped-p (setf (getf row :ripped) ripped)))
			  row) *db*)))
; mapcar maps a function to each element in list (returns a new list with results of function for each element)
; setf is a general assignment operator

(update (where :artist "Dixie Chicks") :rating 11)
(select (where :artist "Dixie Chicks"))


(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
; remove-if is complement to remove-if-not (returns a new list with all maatched elements removed)
; it normaly doesnt affect the original list, but because we save it back to *db*, we actually change the db


; there is some code duplication in where
; instead of:
(select (where :title "Give Us a Break" :ripped t))
; we use:
(select
  #'(lambda (cd)
	  (and (equal (getf cd :title) "Give Us a Break")
		   (equal (getf cd :ripped) t))))
; this is faster because there is no runtime computation (no if clauses)
; but instead of doing this by hand we can use macros
; macros, like in c, run before runtime, but unlike in c here they are actually powerful and not just textual substitution and in general totally different

(defmacro backwards (expr) (reverse expr))
; this is how we define a macro
; reverse returns a new, reversed list

(backwards ("hello, world" t format))
; arguments are not evaluated before they are sent into the macro code (like in functions)
; and this is fast, because the code is generated in compile time, not runtime (thus code of this is identical to (format t "hello, world"))

; now on to improve where with macros

; we need to replace (equal (getf cd field) value) with a macro

(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))
; lisp will try to evaluate every simple name that is not first in list
; this is what we want for field and value, but not the other ones
; the ' before names tells lisp not to evaluate them (so they remaine as simple names and will still work as functions / context  variables)

(make-comparison-expr :rating 10)
(make-comparison-expr :title "Give Us a Break")

; ` works similarly to ' but has some more functionality
	; '(1 2 3) => (1 2 3)
	; `(1 2 3) => (1 2 3)
; subexpressions within a backquoted expression that are preceded with , will be evaluated
	; `(1 2 (+ 1 2)) => (1 2 (+ 1 2))
	; `(1 2 ,(+ 1 2)) => (1 2 3)
; with this we can improve make-comparison-expr

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))
; here only field and value are evaluated

(make-comparison-expr :rating 10)
(make-comparison-expr :title "Give Us a Break")

(defun make-comparison-list (fields)
  (loop while fields
		collecting (make-comparison-expr (pop fields) (pop fields))))
; because the arguments are passed as single list of field value pairs, we use loop to get those pairs
; pop is the opposite of push, and loop will run until no more elements left in fields
; collecting collects all retuurn values in a list
; (if we wrap this list within and and anonymous function we have something that works exactly like where function above)

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))
; ,@ splices the result (in form of a list) of evaluating the expression into existing list
	; `(and ,(list 1 2 3)) => (and (1 2 3))
	; `(and ,@(list 1 2 3)) => (and 1 2 3)
; can also be put in the middle of a list
	; `(and ,@(list 1 2 3) 4) => (and 1 2 3 4)
; &rest modifies how parameters are parsed
; with &rest, function or macro can take arbitrary number of arguments, which are collected into a single list (our case: clauses)

(macroexpand-1 '(where :title "Give Us a Break" :ripped t))
; macroexpand-1 returns code the macro will generate

(select (where :title "Give Us a Break" :ripped t))
; and it works
; and this macro where is shorter, faster and more general (no longer tied to specific field in our cd records (no longer hard coded, call of macro specifies all))

; THIS MACRO THINGS ARE INTERESTING. TRY TO UNDERSTAND AND USE THEM
