(in-package :parsing-assembler)

(defmethod transpile ((p parser) pasm-dsl-spec-string dsl-string start-function)
  ;; input == two strings (a) a spec for the DSL written in PASM and (b) a program written in the DSL
  ;;  a function symbol which is called to run the transpiled DSL
  ;; output == string, code (Lisp) that is the DSL program rewritten in Lisp
    (let ((scanned-pasm (scanner:scanner pasm-dsl-spec-string)))
      (initially p scanned-pasm)
      (transpile-from-tokens p dsl-string start-function)))

(defmethod transpile-from-tokens ((p parser) dsl-string start-function)
  (let ((*pasm-tracing* nil)
	(*pasm-accept-tracing* nil))
    (pasm::<pasm> p))
  (format *standard-output* "~&**** PASM (pasm-tracing ~a)~%" pasm:*pasm-tracing*)
  (format *standard-output* "~&**** PASM (pasm-accept-tracing ~a)~%" pasm:*pasm-accept-tracing*)
  (let ((dsl-lisp-string (get-output-stream-string (pasm:output-string-stream p))))
    (let ((pkg (package-name (find-package (symbol-package start-function)))))
      (compile-string-as-file dsl-lisp-string pkg) 
      (let ((scanned-dsl (scanner:scanner dsl-string)))
	(initially p scanned-dsl)
	(funcall start-function p)
	(get-output-stream-string (pasm:output-string-stream p))))))

(defun compile-string-as-file (str pkg)
  ;; str reprents the contents of a generated .lisp file
  ;; LOAD it...
  ;; (there must be a better way)
  (with-open-file (f "/tmp/temp.lisp" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format f "(in-package ~s)~%~%" pkg)
    (write-string str f))
  (load "/tmp/temp.lisp"))



(defun create-and-load-parser (dsl-as-string)
  (let ((dsl-parser (create-parser dsl-as-string)))
    (load-parser dsl-parser)))

(defun create-parser (dsl-as-string)
  (let ((*pasm-tracing* nil)
	(*pasm-accept-tracing* nil)
	(p (make-instance 'parsing-assembler::parser)))
    (let ((tokens (scanner:scanner dsl-as-string)))
      (initially p dsl-as-string)
      (pasm::<pasm> p)
      (get-output-string-stream p))))

(defun load-parser (parser-as-string)
  ;; see comments in compile-file-as-string
  (with-open-file (f "/tmp/temp.lisp" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string parser-as-string f))
  (load "/tmp/temp.lisp"))
