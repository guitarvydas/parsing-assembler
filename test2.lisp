(in-package :parsing-assembler)

(defparameter *test-pasm-spec-2a*
  "
= rmSpaces
  [ ?SPACE | ?COMMENT | * . ]

= sym
  SYMBOL symbolPush

= num
  INTEGER integerPush

= calculator
  ~rmSpaces
  @num '+' @num
  emitAtom1
  emitPlus
  emitAtom2
  stackPop
  stackPop

")

(defparameter *test-pasm-spec-2b*
  "
= rmSpaces
  [ ?SPACE | ?COMMENT | * . ]

= sym
  SYMBOL symbolPush

= num
  INTEGER integerPush

= calculator
  ~rmSpaces
  emitOpen
  @num
  emitCurriedPlus
  stackPop
  '+'
  emitSpace
  @num
  emitTop
  emitClose
  stackPop

")

(defparameter *test-dsl-code2*
  "
17 + 25
")


(defclass test-parser2 (parser)
  ((atom-stack :accessor atom-stack)))

(defmethod initially ((self test-parser2) token-list)
  (setf (atom-stack self) nil)
  (call-next-method))

;; test mechanisms
(defmethod emitOpen ((self test-parser2)) (emit-string self "("))
(defmethod emitClose ((self test-parser2)) (emit-string self ")"))
(defmethod emitOpenBrace ((self test-parser2)) (emit-string self "{"))
(defmethod emitCloseBrace ((self test-parser2)) (emit-string self "}"))
(defmethod emitSpace ((self test-parser2)) (emit-string self " "))
(defmethod emitPlus ((self test-parser2)) (emit-string self "+"))
(defmethod symbolPush ((self test-parser2)) (push (accepted-token self) (atom-stack self)))
(defmethod integerPush ((self test-parser2)) (push (accepted-token self) (atom-stack self)))
(defmethod stackPop ((self test-parser2)) (pop (atom-stack self)))
(defmethod emitAtom1 ((self test-parser2)) (emit-string self "~a" (token-text (second (atom-stack self)))))
(defmethod emitAtom2 ((self test-parser2)) (emit-string self "~a" (token-text (first (atom-stack self)))))
(defmethod emitTop ((self test-parser2)) (emit-string self "~a" (token-text (first (atom-stack self)))))
(defmethod emitCurriedPlus ((self test-parser2)) (emit-string self "(lambda(x)(+ x ~a))" (token-text (first (atom-stack self)))))
;; end mechanisms
  
(defun test2 ()
  (let ((p (make-instance 'test-parser2)))
    (let ((r (transpile p *test-pasm-spec-2a* *test-dsl-code2* 'parsing-assembler::calculator)))
      (format *standard-output* "~&      result=~a~%" r)
      (format *standard-output* "~&       final=~a~%" 
	      (transpile p *test-pasm-spec-2b* r 'parsing-assembler::calculator)))))
