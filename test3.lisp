(in-package :parsing-assembler)

(defparameter *test-pasm-spec-3b*
  "
%% some mechanisms are defined above

= operand
  [ ?INTEGER INTEGER operandPushInteger
  | * SYMBOL operandPushSymbol
  ]

= rmSpaces
  [ ?SPACE | ?COMMENT | * . ]

= tail
  {[ ?'+'
      '+'
      operatorPush
      @operand
      operandReplaceWithCurriedOperator
   | ?'-'
      '-'
      operatorPush
      @operand
      operandReplaceWithCurriedOperator
   | * >
  ]}

= EmitClosingParens
  {[ &closingStackEmpty > 
   | * emitClose closingPop
  ]}

= emitOperations
  {[ &operandStack1Deep  
     operandEmit 
     operandPop 
     @EmitClosingParens
     >
   | * 
      emitOpen
      closingPush
      operandEmit
      operandPop
      emitSpace
  ]}

= operand
  [ ?SYMBOL SYMBOL operandPushSymbol
  | ?INTEGER INTEGER operandPushInteger
  | *
  ]

= calculator
  ~rmSpaces
  @operand
  @tail
  @emitOperations
")

(defclass test-parser3 (parser)
  ((operand-stack :accessor operand-stack)
   (operator-stack :accessor operator-stack)
   (closing-stack :accessor closing-stack)))


(defmethod initially ((self test-parser3) token-list)
  (setf (operand-stack self) nil)
  (setf (operator-stack self) nil)
  (setf (closing-stack self) nil)
  (call-next-method))


;; mechanisms
(defun volatile(x) (print x))
(defmethod opBreak ((self test-parser3)) (volatile self) (break))
(defmethod emitOpen ((self test-parser3)) (emit-string self "("))
(defmethod emitClose ((self test-parser3)) (emit-string self ")"))
(defmethod emitSpace ((self test-parser3)) (emit-string self " "))
(defmethod emitPlus ((self test-parser3)) (emit-string self "+"))
(defmethod operandStackEmpty ((self test-parser3)) (if (null (operand-stack self)) +succeed+ +fail+))
(defmethod operandPushSymbol ((self test-parser3)) (push (token-text (accepted-token self)) (operand-stack self)))
(defmethod operandPushInteger ((self test-parser3)) (push (token-text (accepted-token self)) (operand-stack self)))
(defmethod operandStack1Deep ((self test-parser3)) (if (>= 1 (length (operand-stack self))) +succeed+ +fail+))
(defmethod operandPop ((self test-parser3)) (pop (operand-stack self)))
(defmethod operandEmit ((self test-parser3)) (emit-string self "~a" (first (operand-stack self))))
(defmethod operatorPush ((self test-parser3)) (push (token-text (accepted-token self)) (operator-stack self)))
(defmethod operatorPop  ((self test-parser3)) (pop (operator-stack self)))
(defmethod operatorStackEmpty ((self test-parser3)) (if (null (operator-stack self)) +succeed+ +fail+))
(defmethod operandReplaceWithCurriedOperator ((self test-parser3))
    (let ((operand-R (pop (operand-stack self))))
      (let ((op (pop (operator-stack self))))
	(push (format nil "(lambda(x) (~a x ~a))" op operand-R)
	      (operand-stack self)))))
(defmethod closingStackEmpty ((self test-parser3)) (if (null (closing-stack self)) +succeed+ +fail+))
(defmethod closingPush ((self test-parser3)) (push ")" (closing-stack self)))
(defmethod closingPop  ((self test-parser3)) (pop (closing-stack self)))
;; end mechanisms
  
(defparameter *test-dsl-code3* "3 - 2 - 1")
(defparameter *test-dsl-code3b* "7 + 9 + 11 + 15")

;; this converts infix to curried prefix lambdas
;; I have chosen to be very explicit / clunky.  It would certainly be possible to optimize this implementation.
(defun test3 ()
  (let ((p (make-instance 'test-parser3)))
    (format *standard-output* "final = ~a" (transpile p *test-pasm-spec-3b* *test-dsl-code3b* 'parsing-assembler::calculator))))
