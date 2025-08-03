;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-eglot--guess-contact-advice"
  :var (mock-result
        mock-fn)

  (before-each
    (spy-on 'pet-lookup-eglot-server-initialization-options :and-return-value '(:python (:pythonPath "/home/user/project/.venv/bin/python")))

    (setq mock-fn (lambda (&rest args)
                    mock-result)))

  (describe "when no initialization options exist"
    (it "should add pet initialization options to the contact"
      (setq mock-result '("mode" "managed-major-mode" "project-instance" ("pylsp") ("python")))

      (let ((result (pet-eglot--guess-contact-advice mock-fn)))
        (expect (nth 3 result)
                :to-equal '("pylsp" :initializationOptions (:python (:pythonPath "/home/user/project/.venv/bin/python"))))
        (expect (seq-subseq result 0 3) :to-equal '("mode" "managed-major-mode" "project-instance"))
        (expect (seq-subseq result 4) :to-equal '(("python"))))))

  (describe "when initialization options already exist"
    (it "should merge pet options with existing initialization options"
      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          ("pylsp" :initializationOptions (:existing-option t))
                          ("python")))

      (let ((result (pet-eglot--guess-contact-advice mock-fn)))
        (expect (nth 3 result)
                :to-equal '("pylsp" :initializationOptions
                            (:existing-option t :python (:pythonPath "/home/user/project/.venv/bin/python"))))
        (expect (seq-subseq result 4) :to-equal '(("python"))))))

  (describe "when pet has no configuration for the server"
    (it "should preserve existing options without modification"
      (spy-on 'pet-lookup-eglot-server-initialization-options)

      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          ("unknown-server" :initializationOptions (:existing-option t))
                          ("python")))

      (let ((result (pet-eglot--guess-contact-advice mock-fn)))
        (expect result :to-equal mock-result))))

  (describe "when no initialization options exist and pet has no config"
    (it "should return original result unchanged"
      (spy-on 'pet-lookup-eglot-server-initialization-options)

      (setq mock-result '("mode" "managed-major-mode" "project-instance" ("unknown-server") ("python")))
      (expect (pet-eglot--guess-contact-advice mock-fn) :to-equal mock-result)))

  (describe "functional contact handling"
    (it "should handle functional contacts by calling them first"
      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          (lambda () '("pylsp" :initializationOptions (:dynamic t)))
                          ("python")))

      (let ((result (pet-eglot--guess-contact-advice mock-fn)))
        (expect (nth 3 result)
                :to-equal '("pylsp" :initializationOptions
                            (:dynamic t :python (:pythonPath "/home/user/project/.venv/bin/python"))))
        (expect (seq-subseq result 4) :to-equal '(("python"))))))

  (describe "functional initialization options handling"
    (it "should wrap functional init-opts in a lambda that merges with pet config"
      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          ("pylsp" :initializationOptions (lambda (server) '(:dynamic t)))
                          ("python")))

      (let ((result (pet-eglot--guess-contact-advice mock-fn)))
        (let* ((contact (nth 3 result))
               (probe (seq-position contact :initializationOptions))
               (init-opts (plist-get (seq-subseq contact (or probe 0)) :initializationOptions)))
          (expect (seq-subseq contact 0 1) :to-equal '("pylsp"))
          (expect init-opts :to-be-truthy)
          (expect (functionp init-opts) :to-be-truthy)
          (expect (funcall init-opts nil)
                  :to-equal '(:dynamic t :python (:pythonPath "/home/user/project/.venv/bin/python"))))
        (expect (seq-subseq result 4) :to-equal '(("python"))))))

  (describe "with complex contact structure"
    (it "should handle contacts with multiple arguments and preserve structure"
      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          ("pylsp" "--verbose" :initializationOptions (:existing t) :other-option "value")
                          ("python")))

      (let ((result (pet-eglot--guess-contact-advice mock-fn)))
        (expect (seq-subseq result 0 3) :to-equal '("mode" "managed-major-mode" "project-instance"))
        (expect (seq-subseq result 4) :to-equal '(("python")))
        (let* ((contact (nth 3 result))
               (probe (seq-position contact :initializationOptions))
               (init-opts (seq-subseq contact probe)))
          (expect (seq-subseq contact 0 2) :to-equal '("pylsp" "--verbose"))
          (expect (plist-get init-opts :initializationOptions)
                  :to-equal '(:existing t :python (:pythonPath "/home/user/project/.venv/bin/python")))
          (expect (plist-get init-opts :other-option) :to-equal "value"))))

    (describe "program-with-args extraction"
      (it "should correctly identify program and arguments before initializationOptions"
        (setq mock-result '("mode" "managed-major-mode" "project-instance"
                            ("pylsp" "arg1" "arg2" :initializationOptions (:existing t))
                            ("python")))

        (let ((result (pet-eglot--guess-contact-advice mock-fn)))
          (expect 'pet-lookup-eglot-server-initialization-options
                  :to-have-been-called-with '("pylsp" "arg1" "arg2"))
          (expect (seq-subseq result 4) :to-equal '(("python")))))))

  (describe "executable resolution"
    (it "should resolve known Python server executables to their full paths"
      (spy-on 'pet-executable-find :and-call-fake 
              (lambda (cmd) 
                (cond ((equal cmd "pylsp") "/venv/bin/pylsp")
                      ((equal cmd "pyright-langserver") "/venv/bin/pyright-langserver")
                      (t nil))))
      
      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          ("pylsp" "--verbose")
                          ("python")))

      (let ((result (pet-eglot--guess-contact-advice mock-fn)))
        (expect (seq-subseq (nth 3 result) 0 2) :to-equal '("/venv/bin/pylsp" "--verbose"))
        (expect 'pet-executable-find :to-have-been-called-with "pylsp")))

    (it "should leave unknown executables unchanged"
      (spy-on 'pet-executable-find)
      
      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          ("unknown-server" "--flag")
                          ("python")))

      (let ((result (pet-eglot--guess-contact-advice mock-fn)))
        (expect (seq-subseq (nth 3 result) 0 2) :to-equal '("unknown-server" "--flag"))
        (expect 'pet-executable-find :not :to-have-been-called)))

    (it "should fall back to original name if pet-executable-find returns nil"
      (spy-on 'pet-executable-find)
      
      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          ("ruff" "server")
                          ("python")))

      (let ((result (pet-eglot--guess-contact-advice mock-fn)))
        (expect (seq-subseq (nth 3 result) 0 2) :to-equal '("ruff" "server"))
        (expect 'pet-executable-find :to-have-been-called-with "ruff"))))

  (describe "edge cases"
    (it "should handle empty contact gracefully"
      (setq mock-result '("mode" "managed-major-mode" "project-instance" () ("python")))
      (expect (pet-eglot--guess-contact-advice mock-fn) :to-equal mock-result))

    (it "should skip TCP network connections unchanged"
      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          ("localhost" 6008)
                          ("python")))

      (expect (pet-eglot--guess-contact-advice mock-fn) :to-equal mock-result)
      (expect 'pet-lookup-eglot-server-initialization-options :not :to-have-been-called))

    (it "should skip process initargs contacts unchanged"
      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          (:process (lambda () "custom-process") :other-option "value")
                          ("python")))

      (expect (pet-eglot--guess-contact-advice mock-fn) :to-equal mock-result)
      (expect 'pet-lookup-eglot-server-initialization-options :not :to-have-been-called))

    (it "should skip autoport contacts unchanged"
      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          ("solargraph" "socket" "--port" :autoport)
                          ("python")))

      (expect (pet-eglot--guess-contact-advice mock-fn) :to-equal mock-result)
      (expect 'pet-lookup-eglot-server-initialization-options :not :to-have-been-called)))

  (describe "cl-letf behavior"
    :var ((eglot-result '((python-mode) "/home/user/project/" eglot-lsp-server ("pylsp") ("python"))))

    (it "should temporarily rebind executable-find during eglot--guess-contact execution"
      (let ((original-executable-find (symbol-function #'executable-find))
            captured-function)
        (spy-on 'eglot--guess-contact :and-call-fake
                (lambda (&rest args)
                  (setq captured-function (symbol-function #'executable-find))
                  eglot-result))

        (pet-eglot--guess-contact-advice #'eglot--guess-contact)

        (expect captured-function :to-equal (symbol-function #'pet-eglot--executable-find-advice))
        (expect (symbol-function #'executable-find) :to-equal original-executable-find)))

    (it "should restore executable-find even when eglot--guess-contact throws an error"
      (let ((original-executable-find (symbol-function #'executable-find)))
        (spy-on 'eglot--guess-contact :and-throw-error 'error)
        (spy-on 'pet--process-guess-contact-result)

        (expect (pet-eglot--guess-contact-advice #'eglot--guess-contact 'python-mode)
                :to-throw 'error)

        (expect (symbol-function #'executable-find) :to-equal original-executable-find)
        (expect 'pet--process-guess-contact-result :not :to-have-been-called)))

    (it "should delegate executable-find calls to pet-executable-find within cl-letf scope"
      (let (executable-find-calls)
        (spy-on 'eglot--guess-contact :and-call-fake
                (lambda (&rest args)
                  (push (executable-find "python") executable-find-calls)
                  (push (executable-find "nonexistent") executable-find-calls)
                  eglot-result))
        (spy-on 'pet-executable-find :and-call-fake
                (lambda (cmd) (when (equal cmd "python") "/venv/bin/python")))

        (pet-eglot--guess-contact-advice #'eglot--guess-contact)

        (expect executable-find-calls :to-equal '(nil "/venv/bin/python"))
        (expect 'pet-executable-find :to-have-been-called-with "python")
        (expect 'pet-executable-find :to-have-been-called-with "nonexistent")))

    (it "should fall back to original executable-find when pet-executable-find returns nil"
      (let (executable-find-result
            (pet--orig-executable-find (lambda (cmd &optional remote)
                                         (when (equal cmd "system-tool") "/usr/bin/tool"))))
        (spy-on 'eglot--guess-contact :and-call-fake
                (lambda (&rest args)
                  (setq executable-find-result (executable-find "system-tool"))
                  eglot-result))
        (spy-on 'pet-executable-find)

        (pet-eglot--guess-contact-advice #'eglot--guess-contact)

        (expect executable-find-result :to-equal "/usr/bin/tool")))

    (it "should handle remote parameter correctly in fallback scenarios"
      (let (results
            (pet--orig-executable-find (lambda (cmd &optional remote)
                                         (if remote "remote-result" "local-result"))))
        (spy-on 'eglot--guess-contact :and-call-fake
                (lambda (&rest args)
                  (push (executable-find "tool") results)
                  (push (executable-find "tool" t) results)
                  eglot-result))
        (spy-on 'pet-executable-find)

        (pet-eglot--guess-contact-advice #'eglot--guess-contact)

        (expect results :to-equal '("remote-result" "local-result"))))

    (it "should process results through pet--process-guess-contact-result"
      (let ((processed-result (append eglot-result '(:processed t))))
        (spy-on 'eglot--guess-contact :and-return-value eglot-result)
        (spy-on 'pet--process-guess-contact-result :and-return-value processed-result)

        (expect (pet-eglot--guess-contact-advice #'eglot--guess-contact t)
                :to-equal processed-result)

        (expect 'eglot--guess-contact :to-have-been-called-with t)
        (expect 'pet--process-guess-contact-result :to-have-been-called-with eglot-result)))))

;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
