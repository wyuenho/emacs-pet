;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-eglot--guess-contact-advice"
  :var ((mock-result nil)
        (mock-fn nil))

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

      (let ((result (pet-eglot--guess-contact-advice mock-fn)))
        (expect result :to-equal mock-result))))

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

  (describe "edge cases"
    (it "should handle empty contact gracefully"
      (setq mock-result '("mode" "managed-major-mode" "project-instance" () ("python")))
      (expect (pet-eglot--guess-contact-advice mock-fn) :to-equal mock-result))

    (it "should skip TCP network connections unchanged"
      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          ("localhost" 6008)
                          ("python")))

      (let ((result (pet-eglot--guess-contact-advice mock-fn)))
        (expect result :to-equal mock-result)
        (expect 'pet-lookup-eglot-server-initialization-options
                :not :to-have-been-called)))

    (it "should skip process initargs contacts unchanged"
      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          (:process (lambda () "custom-process") :other-option "value")
                          ("python")))

      (let ((result (pet-eglot--guess-contact-advice mock-fn)))
        (expect result :to-equal mock-result)
        (expect 'pet-lookup-eglot-server-initialization-options
                :not :to-have-been-called)))

    (it "should skip autoport contacts unchanged"
      (setq mock-result '("mode" "managed-major-mode" "project-instance"
                          ("solargraph" "socket" "--port" :autoport)
                          ("python")))

      (let ((result (pet-eglot--guess-contact-advice mock-fn)))
        (expect result :to-equal mock-result)
        (expect 'pet-lookup-eglot-server-initialization-options
                :not :to-have-been-called)))))

;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
