10

(+ 2 3)

; "hello, world" should self evaluate to string 'hello, world' but only works in sbcl server (sbcl in terminal)
(print "hello, world") ; could use write-line instead

(format t "hello, world") ; t means to print to standard output

(defun hello-world () (format t "hello, world"))
(hello-world)

(defun hello-world2 ()
  (format t "Hello, world!"))
