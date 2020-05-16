(in-package :parsing-assembler)

(defun pasm-to-file (package-name infilename outfilename)
  (let ((scanned-pasm (scanner:scanner (alexandria:read-file-into-string infilename))))
    (let ((p (make-instance 'parser)))
      (initially p scanned-pasm)
      (<pasm> p)
      (with-open-file (outf outfilename :direction :output :if-exists :supersede :if-does-not-exist :create)
	(let ((str (get-output-stream-string (pasm:output-string-stream p))))
	  (format outf "(in-package ~s)~%~%" package-name)
	  (write-string str outf)
	  #+nil(with-input-from-string (ins str)
		 (let ((sexp (read ins nil :eof)))
		   (@:loop
		     (@:exit-when (eq :eof sexp))
		     (pprint sexp outf)
		     (setf sexp (read ins nil :eof))))))))))


(defun pasm-to-string (package-name infilename)
  (let ((scanned-pasm (scanner:scanner (alexandria:read-file-into-string infilename))))
    (let ((p (make-instance 'parser)))
      (initially p scanned-pasm)
      (<pasm> p)
      (let ((str (get-output-stream-string (pasm:output-string-stream p))))
	(concatente 'string (format nil "(in-package ~s)~%~%" package-name)
		    str)))))


