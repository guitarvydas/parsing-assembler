(defpackage :parsing-assembler
  (:use :cl :scanner)
  (:nicknames "PASM")
  (:export
   #:transpile
   #:pasm-to-file

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
   #:pasm-parse-error

   #:token-stream
   #:output-string-stream
   #:next-token
   #:accepted-token

   #:ptrace
   #:*pasm-tracing*
   #:*pasm-accept-tracing*
   #:current-rule)) ;; for debug
   
