(defsystem :parsing-assembler
  :depends-on (:loops :alexandria :scanner/use)
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")))))

(defsystem :parsing-assembler/use  ;; create a usable PASM parser
  :depends-on (:parsing-assembler)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "decls")
				     (:file "mechanisms" :depends-on ("decls"))
				     (:file "unexported-mechanisms" :depends-on ("decls"))
				     (:file "parser" :depends-on ("mechanisms" "unexported-mechanisms"))
				     (:file "pasm" :depends-on ("parser"))
				     (:file "transpile" :depends-on ("parser"))))))


(defsystem :parsing-assembler/test
  :depends-on (:parsing-assembler/use)
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


