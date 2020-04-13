(defsystem :parsing-assembler
  :depends-on (:loops :alexandria :scanner)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")
                                     (:file "decls" :depends-on ("package"))
				     (:file "mechanisms" :depends-on ("decls"))
				     (:file "unexported-mechanisms" :depends-on ("decls"))
				     (:file "parser" :depends-on ("mechanisms" "unexported-mechanisms"))
				     (:file "transpile" :depends-on ("parser"))))))


(defsystem :parsing-assembler/test
  :depends-on (:parsing-assembler)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "test")
				     (:file "test2")
				     (:file "test3")))))


