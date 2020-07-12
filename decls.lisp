(in-package :parsing-assembler)

(defconstant +succeed+ :success)
(defconstant +fail+ nil)

(defun parser-success? (x)
  (eq x +succeed+))

(defclass parser () 
  ((token-stream :accessor token-stream :initarg :token-stream :initform nil) ;; actually, just a list
   (output-string-stream :accessor output-string-stream :initarg :output-stream 
			 :initform (make-string-output-stream))
   (output-token-stream :accessor %output-token-stream :initform nil)
   (next-token :accessor next-token :initform nil)
   (accepted-token :accessor accepted-token :initform nil)
   (state :accessor state :initform :idle)
   (current-rule :accessor current-rule :initform nil)
   (depth :accessor depth :initform 0)
   ))

;; a parser must support:

(defgeneric initially (self token-stream))

(defgeneric accept (self))
(defgeneric pasm-error (self message))

(defgeneric input-char (self c))

(defgeneric emit-string (self str &rest args))

(defmethod append-token ((self parser) token)
  (push token (%output-token-stream self)))

(defmethod output-token-stream ((self parser))
  (reverse (%output-token-stream self)))
