; text is translated (by the reader) into s-expressions
; s-expressions are evaluated (by the evalueator) into forms (lisp code)

; s-expressions are either lists or atoms
; lists are inside () and contain any number of space-separated elements
; these elements (atoms or nested lists) are themselves s-expressions

;atoms

;numbers
123       ; the integer one hundred twenty-three
3/7       ; the ratio three-sevenths
1.0       ; the floating-point number one in default precision
1.0e0     ; another way to write the same floating-point number
1.0d0     ; the floating-point number one in "double" precision
1.0e-4    ; the floating-point equivalent to one-ten-thousandth
+42       ; the integer forty-two
-42       ; the integer negative forty-two
-1/4      ; the ratio negative one-quarter
-2/8      ; another way to write negative one-quarter
246/2     ; another way to write the integer one hundred twenty-three

;string literals
"foo"     ; the string containing the characters f, o, and o.
"fo\o"    ; the same string
"fo\\o"   ; the string containing the characters f, o, \, and o.
"fo\"o"   ; the string containing the characters f, o, ", and o."
; \ and " need to be escaped with preceding \

; names (variable and function) are transalted (by the reader) into symbol objects
; shouldn't contain ( ) " ' ` , : ; \ or |
; names are case insensitive (for unescaped characters)
	; foo == Foo == FOO => FOO
	; \f\o\o != FOO
; use of hyphenated names (hello-world)
; global variables start and end with *
; contstants start and end with +
; low-level functions start with % or %%

x             ; the symbol X
()            ; the empty list
(1 2 3)       ; a list of three numbers
("foo" "bar") ; a list of two strings
(x y z)       ; a list of three symbols
(x 1 "foo")   ; a list of a symbol, a number, and a string
(+ (* 2 3) 4) ; a list of a symbol, a list, and a number.

(defun hello-world ()
  (format t "hello, world"))
; this is also a list
; 4 item list, containing two symbols, an empty list and another list (itself containing two symbols and a string)


; not every s-expression can be evaluated
; any atom is legal lisp form
; and list with symbol as the first element is legal lisp form

; symbol evaluated as a form is considered the name of a variable and evaluates to the current value of the variable
; numbers and strings are self-evaluating objects (evaluate to the itself)
; symbols can also be self-evaluating (variables they name can be assigned to the value of the symbol itself) (T, NIL)
; keyword-symbols (symbols with names starting with :) are also self-evaluating


; list forms are evaluated in 3 different ways depending on the symbol they start with (function, macro or special operator)
; if the symbol is not defined it's considered a function name

; FUNCTION CALLS
; evalueates the remaining elements of the function call (which must be legal lisp forms) and pass their values as arguments to the function
; function calls are evaluated from inside out
	; (+ 1 2) -> 1 and 2 are evalueated and than passed to + function which returns 3
	; (* (+ 1 2) (- 3 4)) -> 1 and 2 are evalueated and passed to + function which is evalueaed to 3; 3 and 4 are evalueated and passed to - function wich is evalueated to -1; 3 and -1 are passed to * function which is evalueated to -3

; SPECIAL OPERATORS
; when the first element of a list is symbol naming speical operator, the rest of the expressions are evalueated according to the rule of that operator
; IF -> (if test-form then-form [else-form])
	; evaluate test-form
	; if not-nil evaluate and return the value of then-form (not evalueated if false)
	; else evaluate and return the value of else-form (or NIL if omitted) (not evalueated if true)
; QUOTE -> (quote arg)
	;returns its argument (singular) unevaluated
	; (quote (+ 1 2)) -> (+ 1 2)
	; '(+ 1 2) is a shorthand for the expression above ('arg is shorthand for (quote arg))
; LET -> (let args)
	(let ((x 10)) x) ; -> 10
	; the second x is evaluated in an environment where it's the name of a variable established by the LET with the value 10


; MACROS
; are not defined by the language standard (gives users the ability to extend the langauge syntax)
; a macro is a function that takes s-expressions as arguments and returns a lisp form that's then evaluated in place of the macro form.
; this is done in 2 phases
; elements of the macro are passed, unevaluated, to the macro function (and thus dont have to be legal lisp forms)
; form returned by the macro (its exapnsion) is than evaluated by the normal evaluation rules
	; in REPL this happens one after another, but when compiling the program, the first happens in compile time and the second in runtime


; symbol NIL is the only false value, everything else is true
; symbol T is the canonical true value (used when need to return non-nil value and dont have anything other)
; NIL is used to represent an empty list (reader replaces () with NIL), thus NIL is both an atom and a list
; nil == () == 'nil == '()
	; in the quoted forms the QUOTE special operator evaluates to the symbol directly. For the same reason, both t and 't will evaluate to the same thing: the symbol T.


; EQUALITY (from most to least discriminating)
; EQ -> object identity
	; two objects are EQ if they are idential.
	; for numbers and characters this depends of particular lisp implementation
		; (eq 1 1) can be true or false
		; (eq x x) can be true or false (if value of x is number or character)
; EQL
	; like EQ, but objects of the same class representing the same numeric or character value are equal (the flaw from EQ above)
; EQUAL
	; true for lists if they have the same structure and content (recurively, acording to EQUAL)
	; true for strings if they contain the same characters
	; looser definition for bit vectors and pathnames
; EQUALP
	; character and string comparison is case insensitive
	; numbers are equivalent if they represent same mathematical value (even if different class) (thus (equalp 1 1.0) is true)
	; lists with EQUALP elements are EQUALP; likewise, arrays with EQUALP elements are EQUALP ???


; FORMATING
; each level of nesting gets indented a little more
; items at the same level of nesting are lined up
; body elements of macros and special forms are indented 2 spaces relative to the opening parenthesis
; dont use () as {} and spread them after the form (last line will have )))... at the end and not multiple lines with single ) a little less indented)
	; wrong:
	(defun foo ()
	  (dotimes (i 10)
		(format t "~d. hello~%" i)
	  )
	)
	; right:
	(defun foo ()
	  (dotimes (i 10)
		(format t "~d. hello~%" i)))


; COMMENTING
;;;; Four semicolons are used for a file header comment.

;;; A comment with three semicolons will usually be a paragraph
;;; comment that applies to a large section of code that follows,

(defun foo (x)
  (dotimes (i x)
	;; Two semicolons indicate this comment applies to the code
	;; that follows. Note that this comment is indented the same
	;; as the code that follows.
	(some-function-call)
	(another i)              ; this comment applies to this line only
	(and-another)            ; and this is for this line
	(baz)))
