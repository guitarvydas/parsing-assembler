(in-package :parsing-assembler)

(defmethod initially ((self parser) token-list)
  (let ((empty-token (scanner:make-token :kind :empty :text "" :line 0 :position 0)))
    (setf (next-token self) (pop token-list))
    (setf (accepted-token self) empty-token)
    (setf (token-stream self) token-list)
    (setf (output-string-stream self) (make-string-output-stream))))

(defmethod advance-next-token ((self parser))
  (if (null (token-stream self))
      (setf (next-token self) (scanner:make-token :kind :eof :text "generated eof" :line 0 :position 0))
      (setf (next-token self) (pop (token-stream self)))))
  
(defun format-token (tok)
  (if (characterp (token-text tok))
      (format nil "[~s line=~a position=~a (~a)~a]" (token-kind tok) (token-line tok) (token-position tok)
	      (char-code (token-text tok)) (token-text tok))
      (format nil "[~s line=~a position=~a ~a]" (token-kind tok) (token-line tok) (token-position tok)
	      (token-text tok))))

(defmethod pasm-parse-error ((self parser) message)
  (let ((final-message (format nil "~s, but got ~s" message (format-token (next-token self)))))
    (error final-message)))

(defmethod accept ((self parser))
  (setf (accepted-token self) (next-token self))
  (unless (eq :EOF (accepted-token self))
    (advance-next-token self))
  (when *pasm-accept-tracing*
    (if (and (numberp *pasm-accept-tracing*) (< *pasm-accept-tracing* 0))
	(format *error-output* "~a" (token-text (accepted-token self)))
	(progn
	  (format *error-output* "~&accepted ~a ~s" (token-text (accepted-token self)) (format-token (accepted-token self)))
	  (format *error-output* " next ~s~%" (format-token (next-token self)))))))

(defmethod lookahead-char? ((self parser) c)
  (let ((tok (next-token self)))
    (if (or
	 (and (eq :character (token-kind tok))
	      (char= c (token-text tok)))
	 (and (eq :symbol (token-kind tok))
	      (= 1 (length (token-text tok)))
	      (char= (char (token-text tok) 0) c)))
       +succeed+
      +fail+)))

(defmethod lookahead-symbol? ((self parser) str)
  (let ((tok (next-token self)))
    (if (and (eq :symbol (token-kind tok))
	      (string= str (token-text tok)))
       +succeed+
      +fail+)))

(defmethod lookahead? ((self parser) kind)
  (let ((tok (next-token self)))
    (if (eq kind (token-kind tok))
       +succeed+
      +fail+)))
       
(defmethod input-char ((self parser) c)
  (if (eq +succeed+ (lookahead-char? self c))
      (accept self)
     (pasm-parse-error 
      self 
      (format nil "expected character ~s (~a)" c (char-code c)))))

(defmethod input-symbol ((self parser) str)
  (flet ((nope () (pasm-parse-error self (format nil "expected :symbol with text ~a" str))))
    (if (lookahead? self :symbol)
	(if (string= str (scanner:token-text (next-token self)))
	   (accept self)
	  (nope))
	(nope))))

(defmethod input ((self parser) kind)
  (let ((tok (next-token self)))
    (if (eq kind (token-kind tok))
       (accept self)
      (pasm-parse-error self (format nil "expected token ~s" kind)))))
	
(defmethod emit-string ((self parser) fmtstr &rest args)
  (let ((out (output-string-stream self)))
    ;(apply 'cl:format *error-output* fmtstr args)
    (cond ((characterp fmtstr) 
	   (cl:format out "~c" fmtstr))
	  (t (apply 'cl:format out fmtstr args)))))


(defmethod call-rule ((self parser) func)
  (if (functionp func)
      (funcall func self)
      (error "not a function")))

(defmethod call-predicate ((self parser) func)
  (if (functionp func)
      (let ((%result (funcall func self)))
	%result)
      (error "not a function")))

(defmethod call-external ((self parser) func)
  (if (functionp func)
      (funcall func self)
      (error "not a function")))

(defmethod accept-and-return-token ((p parser))
  (accept p)
  (accepted-token p))

(defmethod pasm-filter-stream ((p parser) rule-name)
  (let ((*pasm-accept-tracing* nil)
	(*pasm-tracing* nil))
    (let ((%new-list nil))
      (loop (when (eq :EOF (token-kind (accepted-token p)))
	      (return))
	 (token-kind (accepted-token p)) (token-text (accepted-token p))
	 (token-kind (next-token p)) (token-text (next-token p))
	 (let ((%a (accepted-token p))
	       (%n (next-token p)))
	   (funcall rule-name p)
	   (if (and (eq %a (accepted-token p)) (eq %n (next-token p)))
	       (accept p)
	       (push (accepted-token p) %new-list))))
      (initially p (reverse %new-list)))))

(defmethod ptrace ((self parser))
  ; babel mode in emacs wants *error-output*
  ;(format *error-output* "~&trace in ~a next=(~a ~s ~a ~a) accepted=(~a ~s ~a ~a)~%"
  (format *error-output* "~&trace in ~a accepted=(~a ~s ~a ~a) next=(~a ~s ~a ~a)~%"
	  (current-rule self)
	  (scanner:token-kind (accepted-token self)) (scanner:token-text (accepted-token self)) 
	  (scanner:token-line (accepted-token self)) (scanner:token-position (accepted-token self))
	  (scanner:token-kind (next-token self)) (scanner:token-text (next-token self)) 
	  (scanner:token-line (next-token self)) (scanner:token-position (next-token self))))


(defmethod p-into-trace ((self parser))
  (when *pasm-tracing*
    (incf (depth self))
    (spaces *error-output* (depth self))
					; babel mode in emacs wants *error-output*
    (format *error-output* "into   ~a accepted=(~a ~s ~a ~a) next=(~a ~s ~a ~a)~%"
	    (current-rule self)
	    (scanner:token-kind (accepted-token self)) (scanner:token-text (accepted-token self)) 
	    (scanner:token-line (accepted-token self)) (scanner:token-position (accepted-token self))
	    (scanner:token-kind (next-token self)) (scanner:token-text (next-token self)) 
	    (scanner:token-line (next-token self)) (scanner:token-position (next-token self)))))
  
(defmethod p-return-trace ((self parser))
  (when *pasm-tracing*
    (decf (depth self))
    (spaces *error-output* (depth self))
					; babel mode in emacs wants *error-output*
    (format *error-output* "back into ~a accepted=(~a ~s ~a ~a) next=(~a ~s ~a ~a)~%"
	    (current-rule self)
	    (scanner:token-kind (accepted-token self)) (scanner:token-text (accepted-token self)) 
	    (scanner:token-line (accepted-token self)) (scanner:token-position (accepted-token self))
	    (scanner:token-kind (next-token self)) (scanner:token-text (next-token self)) 
	    (scanner:token-line (next-token self)) (scanner:token-position (next-token self)))))

(defun spaces (strm n)
  (format strm "~&")
  (@:loop
    (@:exit-when (<= n 0))
    (format strm "  ")
    (decf n)))
