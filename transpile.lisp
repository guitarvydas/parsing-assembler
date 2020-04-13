(in-package :parsing-assembler)

(defmethod transpile ((p parser) pasm-dsl-spec-string dsl-string start-function)
  ;; input == two strings (a) a spec for the DSL written in PASM and (b) a program written in the DSL
  ;;  a function symbol which is called to run the transpiled DSL
  ;; output == string, code (Lisp) that is the DSL program rewritten in Lisp
  (let ((scanned-pasm (scanner:scanner pasm-dsl-spec-string)))
    (initially p scanned-pasm)
    (pasm::<pasm> p)
    (let ((dsl-lisp-string (get-output-stream-string (pasm:output-string-stream p))))
      (let ((pkg (package-name (find-package (symbol-package start-function)))))
	(compile-string-as-file dsl-lisp-string pkg) 
	(let ((scanned-dsl (scanner:scanner dsl-string)))
	  (initially p scanned-dsl)
	  (funcall start-function p)
	  (get-output-stream-string (pasm:output-string-stream p)))))))

(defun compile-string-as-file (str pkg)
  ;; str reprents the contents of a generated .lisp file
  ;; LOAD it...
  ;; (there must be a better way)
  (with-open-file (f "/tmp/temp.lisp" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format f "(in-package ~s)~%~%" pkg)
    (write-string str f))
  (load "/tmp/temp.lisp"))

		    
