(defpackage :parsing-assembler
  (:use :cl :scanner)
  (:nicknames "PASM")
  (:export
   #:transpile
   #:parser
   #:output-string-stream

   #:initially
   #:lookahead-char?
   #:lookahead-symbol?
   #:lookahead?
   #:input-char
   #:input-symbol
   #:input
   #:emit-string
   #:parser-success?
   #:+succeed+
   #:+fail+
   #:next-token
   #:accepted-token
   #:call-rule
   #:call-predicate
   #:call-external
   #:accept

   #:token-stream
   #:output-string-stream
   #:next-token
   #:accepted-token

   #:ptrace
   #:current-rule)) ;; for debug
   
