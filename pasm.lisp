(in-package :parsing-assembler)

(defun pasm-to-file (package-name infilename outfilename &optional (suffix ""))
  (let ((scanned-pasm (scanner:scanner (alexandria:read-file-into-string infilename))))
    (let ((p (make-instance 'parser)))
      (initially p scanned-pasm)
      (<pasm> p suffix)
      (with-open-file (outf outfilename :direction :output :if-exists :supersede :if-does-not-exist :create)
	(let ((str (get-output-stream-string (pasm:output-string-stream p))))
	  (format outf "(in-package ~s)~%~%" package-name)
	  (write-string str outf))))))


(defun pasm-to-string (package-name infilename &optional (suffix ""))
  (let ((scanned-pasm (scanner:scanner (alexandria:read-file-into-string infilename))))
    (let ((p (make-instance 'parser)))
      (initially p scanned-pasm)
      (<pasm> p suffix)
      (let ((str (get-output-stream-string (pasm:output-string-stream p))))
	(concatenate 'string (format nil "(in-package ~s)~%~%" package-name)
		    str)))))


