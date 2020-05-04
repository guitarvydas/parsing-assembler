(in-package :parsing-assembler)

(defmethod skip-spaces-and-comments ((p parser))
  (cond ((eq :SPACE (token-kind (next-token p))))
	((eq :COMMENT (token-kind (next-token p))))
	(t (accept p))))
	 
(defmethod <pasm> ((p parser))
  (pasm-filter-stream p #'skip-spaces-and-comments)
  (@:loop
    (if (pasm:parser-success? (pasm:lookahead-char? p #\=))
	(<parse-rule> p)
	(if (pasm:parser-success? (pasm:lookahead-char? p #\-))
	    (<parse-predicate> p)
	    (@:exit-when t)))) ;; else exit
  (pasm:input p :EOF)
  )

;; !symbol or !"string" or !([string|symbol]+) !() nests
(defmethod <parse-hook> ((p parser))
  (pasm:input-char p #\!)
  (if (pasm:parser-success? (lookahead-char? p #\())
      (progn
	(emit-string p "(hook-list p ")
	(<parse-list-hook> p)
	(emit-string p ")~%"))
      (if (pasm:parser-success? (lookahead? p :string))
	  (<parse-string-hook> p )
	  (<parse-symbol-hook> p))))

(defmethod <parse-symbol-hook> ((p parser))
  (pasm:input p :symbol)
  (emit-string p "(hook-symbol '~a)" (token-text (accepted-token p))))

(defmethod <parse-string-hook> ((p parser))
  (pasm:input :symbol)
  (emit-string p "(hook-string \"~s\")" (token-text (accepted-token p))))

(defmethod <parse-list-hook> ((p parser))
  (input-char p #\( )
  (@:loop
    (if (pasm:parser-success? (lookahead? p :symbol))
	(progn (input p :symbol) (emit-string p "'~a " (token-text (accepted-token p))))
	(if (pasm:parser-success? (lookahead? p :string))
	    (progn (input p :string) (emit-string p "\"~a\" " (token-text (accepted-token p))))
	    (if (pasm:parser-success? (lookahead-char? p #\())
		(progn (input-char p #\() (<parse-list-hook> p))
		(@:exit-when t))))
    )
  (input-char p #\) )
  )

			    
(defmethod <filter-stream> ((p parser))
  (let ((*pasm-accept-trace* nil)
	(*pasm-tracing* nil))
    (pasm:input-char p #\~)
    (pasm:input p :symbol)
    (pasm:emit-string p "(pasm::pasm-filter-stream p #'~a)" (token-text (accepted-token p)))))

(defmethod <parse-token-expr> ((p parser))
  (cond ((pasm:parser-success? (pasm:lookahead-char? p #\'))
	 (pasm:input-char p #\')
	 (input-any-character p)
             (pasm:emit-string p "(pasm:input-char p #\\~a)" (scanner:token-text (pasm:accepted-token p)))
         (pasm:input-char p #\')
	 pasm:+succeed+)
	((pasm:parser-success? (parsing-assembler::look-upcase-symbol? p))
	 (input-upcase-symbol p)
	 (if (pasm:parser-success? (pasm:lookahead-char? p #\/))
	     ;; qualified token NAME/qual
             (progn
               (pasm:input-char p #\/)
	       ;; qual can be a SYMBOl or a single char
	       (if (pasm:parser-success? (lookahead? p :character))
		   (progn
		     (input-any-character p)
		     (pasm:emit-string p "(pasm:input-char p #\\~a)" (scanner:token-text (pasm:accepted-token p))))
		  (progn
		    (pasm:input p :symbol)
		    (pasm:emit-string p "(pasm:input-symbol p ~s)" (scanner:token-text (pasm:accepted-token p)))))
	       )
	   ;; unqualified token NAME
           (pasm:emit-string p "(pasm:input p :~a)" (scanner:token-text (pasm:accepted-token p))))
	 pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-noop> ((p parser))
  (cond ((pasm:parser-success? (pasm:lookahead-char? p #\*))
	 (pasm:input-char p #\*)
             (pasm:emit-string p " t ")
	 pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-dot> ((p parser))
  (cond ((pasm:parser-success? (pasm:lookahead-char? p #\.))
	 (pasm:input-char p #\.)
             (pasm:emit-string p " (pasm:accept p) ")
	 pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-expr> ((p parser))
  (cond ((pasm:parser-success? (<parse-token-expr> p)) pasm:+succeed+)
	((pasm:parser-success? (<parse-noop> p))       pasm:+succeed+)
	((pasm:parser-success? (<parse-dot> p))        pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-lookahead-token-expr> ((p parser))
  (cond ((pasm:parser-success? (pasm:lookahead-char? p #\'))
	 (pasm:input-char p #\')
	 (input-any-character p)
             (pasm:emit-string p "(pasm:parser-success? (pasm:lookahead-char? p #\\~a))" (scanner:token-text (pasm:accepted-token p)))
         (pasm:input-char p #\')
	 pasm:+succeed+)
	((pasm:parser-success? (parsing-assembler::look-upcase-symbol? p))
	 (parsing-assembler::input-upcase-symbol p)
	 (cond ((pasm:parser-success? (pasm:lookahead-char? p #\/))
                 ;; same as above (<parse-token-expr>) TOKEN/qual where qual can be a symbol or a character
                 ;; TOKEN/q where q is a :symbol or :character
                 (pasm:input-char p #\/)
                 (cond ((pasm:parser-success? (pasm:lookahead? p :character))
                        ;; q is :character
                        (input-any-character p)
			(pasm:emit-string p "(pasm:parser-success? (pasm:lookahead-char? p #\\~a))" (scanner:token-text (pasm:accepted-token p))))
                       ;; else q is :symbol
                       (t
                        (pasm:input p :symbol)
                        (pasm:emit-string p "(pasm:parser-success? (pasm:lookahead-symbol? p ~s))" (scanner:token-text (pasm:accepted-token p))))))
               (t (pasm:emit-string p "(pasm:parser-success? (pasm:lookahead? p :~a))" (scanner:token-text (pasm:accepted-token p)))))
         pasm:+succeed+)
        (t pasm:+fail+)))

(defmethod <parse-lookahead-noop> ((p parser))
  (cond ((pasm:parser-success? (pasm:lookahead-char? p #\*))
	 (pasm:input-char p #\*)
             (pasm:emit-string p " t ")
	 pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-lookahead-expr> ((p parser))
  (cond ((pasm:parser-success? (<parse-lookahead-token-expr> p)) pasm:+succeed+)
	((pasm:parser-success? (<parse-lookahead-noop> p))       pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-statement> ((p parser))
  (cond ((pasm:parser-success? (<parse-cycle> p)) pasm:+succeed+)
	((pasm:parser-success? (pasm:lookahead-char? p #\!)) (<parse-hook> p)    pasm:+succeed+)
	((pasm:parser-success? (<parse-choice> p)) pasm:+succeed+)
	((pasm:parser-success? (<parse-return> p)) pasm:+succeed+)
	((pasm:parser-success? (<parse-cycle-exit> p)) pasm:+succeed+)
	((pasm:parser-success? (<parse-input-token> p)) pasm:+succeed+)
	((pasm:parser-success? (<parse-lookahead> p)) pasm:+succeed+)
	((pasm:parser-success? (<parse-noop> p)) pasm:+succeed+)
	((pasm:parser-success? (<parse-dot> p)) pasm:+succeed+)
	((pasm:parser-success? (<parse-rule-call> p)) pasm:+succeed+)
	((pasm:parser-success? (<parse-predicate-call> p)) pasm:+succeed+)
	((pasm:parser-success? (<parse-external-call> p)) pasm:+succeed+)
	((pasm:parser-success? (pasm:lookahead-char? p #\~)) (<filter-stream> p) pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-statements> ((p parser))
  (let ((result pasm:+fail+))
  (@:loop
   (@:exit-when (not (pasm:parser-success? (<parse-statement> p))))
   (pasm:emit-string p "~%")
   (setf result pasm:+succeed+))
  result))

(defmethod <parse-choice-start> ((p parser))
  (cond ((parser-success? (lookahead-char? p #\*)) (<parse-noop> p) +succeed+)
	((parser-success? (lookahead-char? p #\&)) (<parse-predicate-call> p) +succeed+)
	((parser-success? (lookahead-char? p #\?)) (<parse-lookahead> p) +succeed+)
	(t +fail+)))

(defmethod <parse-cycle> ((p parser))
  (cond ((pasm:parser-success? (pasm:lookahead-char? p #\{))
	 (pasm:input-char p #\{)
                 (pasm:emit-string p "~&(loop~%")
	 (<parse-statements> p)
	 (pasm:input-char p #\})
                 (pasm:emit-string p ")~%")
	 pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-choice> ((p parser))
  (cond ((pasm:parser-success? (pasm:lookahead-char? p #\[))
	 (pasm:input-char p #\[)
             (pasm:emit-string p "~&(cond~%")
             (pasm:emit-string p "(")
	 (<parse-choice-start> p)
	 (<parse-statements> p)
             (pasm:emit-string p ")~%")
         (@:loop
           (@:exit-when (not (pasm:parser-success? (pasm:lookahead-char? p #\|))))
           (pasm:input-char p #\|)
               (pasm:emit-string p "(")
	   (<parse-choice-start> p)
           (<parse-statements> p)
               (pasm:emit-string p ")~%"))
	 (pasm:input-char p #\])
            (pasm:emit-string p ")~%")
	 pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-choice-alternate> ((p parser))
  (cond ((pasm:parser-success? (pasm:lookahead-char? p #\|))
	 (pasm:input-char p #\|)
             (pasm:emit-string p "(")
	 (<parse-statements> p)
             (pasm:emit-string p ")~%")
	 pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-input-token> ((p parser))
  (<parse-expr> p))

(defmethod error-if-not-success ((p parser) msg value)
  (if (eq pasm:+succeed+ value)
      pasm:+succeed+
      (pasm-parse-error p msg)))

(defmethod <parse-lookahead> ((p parser))
  (cond ((pasm:parser-success? (pasm:lookahead-char? p #\?))
	 (pasm:input-char p #\?)
	 (<parse-lookahead-expr> p))
	(t pasm:+fail+)))
  
(defmethod <parse-rule-call> ((p parser))
  (cond ((pasm:parser-success? (pasm:lookahead-char? p #\@))
	 (pasm:input-char p #\@)
	 (pasm:input p :symbol)
            (pasm:emit-string p "(pasm:call-rule p #'~a)" (scanner:token-text (pasm:accepted-token p)))
	 pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-predicate-call> ((p parser))
  (cond ((pasm:parser-success? (pasm:lookahead-char? p #\&))
	 (pasm:input-char p #\&)
	 (pasm:input p :symbol)
            (pasm:emit-string p "(pasm:parser-success? (pasm:call-predicate p #'~a))" (scanner:token-text (pasm:accepted-token p)))
	 pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-external-call> ((p parser))
  (cond ((pasm:parser-success? (lookahead? p :symbol))
	 (pasm:input p :symbol)
               (pasm:emit-string p "(pasm:call-external p #'~a)" (scanner:token-text (pasm:accepted-token p)))
	 pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-cycle-exit> ((p parser))
  (cond ((pasm:parser-success? (pasm:lookahead-char? p #\>))
	 (pasm:input-char p #\>)
	 (pasm:emit-string p "(return)")
	 pasm:+succeed+)
	(t pasm:+fail+)))

(defmethod <parse-return> ((p parser))
  (cond ((pasm:parser-success? (pasm:lookahead-char? p #\^))
	 (pasm:input-char p #\^)
	 (pasm:input p :symbol)
	 (error-if-not-success p "expected ^ok or ^fail"
	  (if (string= "fail" (scanner:token-text (pasm:accepted-token p)))
	      (progn
                      (pasm:emit-string p "(setf (pasm:current-rule p) prev-rule) (pasm::p-into-trace p)")
                      (pasm:emit-string p "(return-from ~a pasm:+fail+)" (pasm::current-rule p))
                pasm:+succeed+)
	      (if (string= "ok" (scanner:token-text (pasm:accepted-token p)))
		  (progn
                      (pasm:emit-string p "(setf (pasm:current-rule p) prev-rule) (pasm::p-return-trace p)")
                      (pasm:emit-string p "(return-from ~a pasm:+succeed+)" (pasm::current-rule p))
                    pasm:+succeed+)
		  pasm:+fail+)))
	 pasm:+succeed+)
	(t pasm:+fail+)))
  
(defmethod <parse-rule> ((p parser))
  (pasm:input-char p #\=)
  (pasm:input p :symbol)
  (setf (pasm::current-rule p) (scanner:token-text (pasm:accepted-token p)))
             (pasm:emit-string p "(defmethod ~a ((p pasm:parser))~%" (pasm::current-rule p))
             (pasm:emit-string p "  (let ((prev-rule (pasm:current-rule p)))")
	     (pasm:emit-string p "     (setf (pasm:current-rule p) \"~a\") (pasm::p-into-trace p)~%" (pasm::current-rule p))
  (<parse-statements> p)
             (pasm:emit-string p "(setf (pasm:current-rule p) prev-rule) (pasm::p-return-trace p)")
             (pasm:emit-string p "))~%~%")
  pasm:+succeed+
  )

(defmethod <parse-predicate> ((p parser))
  (pasm:input-char p #\-)
  (pasm:input p :symbol)
  (setf (pasm::current-rule p) (scanner:token-text (pasm:accepted-token p)))
             (pasm:emit-string p "(defmethod ~a ((p pasm:parser)) ;; predicate~%" (pasm::current-rule p))
             (pasm:emit-string p "  (let ((prev-rule (pasm:current-rule p)))")
	     (pasm:emit-string p "     (setf (pasm:current-rule p) \"~a\") (pasm::p-into-trace p)~%" (pasm::current-rule p))
  (<parse-statements> p)
             (pasm:emit-string p "(setf (pasm:current-rule p) prev-rule) (pasm::p-return-trace p)")
             (pasm:emit-string p "))~%~%")
  pasm:+succeed+
  )

