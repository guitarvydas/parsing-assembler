(in-package :parsing-assembler)

(defmethod input-upcase-symbol ((p parser))
  (let ((tok (next-token p)))
    (if (and (eq :symbol (token-kind tok))
	     (all-upcase? (token-text tok)))
       (accept p)
      (pasm-parse-error p "expected upcase symbol"))))

(defmethod look-upcase-symbol? ((p parser))
  (let ((tok (next-token p)))
    (if (and (eq :symbol (token-kind tok))
	     (all-upcase? (token-text tok)))
	pasm:+succeed+
      pasm:+fail+)))

(defun all-upcase? (str)
  (string= str (string-upcase str)))

(defmethod lookahead-any-char? ((self parser))
  (let ((tok (pasm:next-token self)))
    (if (or
	 (eq :character (scanner:token-kind tok))
	 (and (eq :symbol (scanner:token-kind tok))
	      (= 1 (length (scanner:token-text tok)))))
       pasm:+succeed+
      pasm:+fail+)))

(defmethod input-any-character ((p parser))
  (if (eq pasm:+succeed+ (lookahead-any-char? p))
      (accept p)
     (pasm-parse-error p (format nil "expected any character (or a symbol of length 1)"))))
