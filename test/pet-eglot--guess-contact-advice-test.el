;; -*- lexical-binding: t; -*-

(require 'pet)

(setq python-indent-guess-indent-offset nil)
(describe "pet-eglot--guess-contact-advice"
  (before-each
    (spy-on 'pet-executable-find :and-return-value "/home/user/project/.venv/bin/pylsp")
    (spy-on 'project-current :and-return-value (if (< emacs-major-version 29) (cons 'vc "/home/user/project/") '(vc Git "/home/user/project/")))
    (advice-add 'eglot--executable-find :around #'pet-eglot--executable-find-advice)
    (advice-add 'eglot--guess-contact :around #'pet-eglot--guess-contact-advice))

  (after-each
    (advice-remove 'eglot--executable-find #'pet-eglot--executable-find-advice)
    (advice-remove 'eglot--guess-contact #'pet-eglot--guess-contact-advice))

  (it "should use pet-executable-find path for Python LSP servers"
    (assume (and (require 'eglot nil t)
                 (fboundp 'eglot--executable-find))
            "Unsupported `eglot' version")

    (let ((eglot-server-programs `((python-mode . ,(eglot-alternatives '("pylsp"))))))
      (spy-on 'pet-lookup-eglot-server-initialization-options)
      (with-temp-buffer
        (setq default-directory "/home/user/project")
        (setq buffer-file-name "/home/user/project/file.py")
        (python-mode)
        (let* ((result (eglot--guess-contact))
               (contact (nth 3 result))
               (program (car contact)))
          (expect program :to-equal "/home/user/project/.venv/bin/pylsp")))))

  (it "should add pet initialization options when none exist"
    (assume (and (require 'eglot nil t)
                 (fboundp 'eglot--executable-find))
            "Unsupported `eglot' version")

    (let ((eglot-server-programs `((python-mode . ,(eglot-alternatives '("pylsp"))))))
      (spy-on 'pet-lookup-eglot-server-initialization-options :and-return-value '(:test-option t))
      (with-temp-buffer
        (setq default-directory "/home/user/project")
        (setq buffer-file-name "/home/user/project/file.py")
        (python-mode)
        (let* ((result (eglot--guess-contact))
               (contact (nth 3 result))
               (probe (cl-position-if #'keywordp contact))
               (init-opts (and probe (cl-subseq contact probe))))
          (expect (plist-get init-opts :initializationOptions) :to-equal '(:test-option t))))))

  (it "should merge pet initialization options with existing ones"
    (assume (and (require 'eglot nil t)
                 (fboundp 'eglot--executable-find))
            "Unsupported `eglot' version")

    (let ((eglot-server-programs `((python-mode . ,(eglot-alternatives '(("pylsp" :initializationOptions (:existing-option t))))))))
      (spy-on 'pet-lookup-eglot-server-initialization-options :and-return-value '(:pet-option t))
      (with-temp-buffer
        (setq default-directory "/home/user/project")
        (setq buffer-file-name "/home/user/project/file.py")
        (python-mode)
        (let* ((result (eglot--guess-contact))
               (contact (nth 3 result))
               (probe (cl-position-if #'keywordp contact))
               (init-opts (and probe (cl-subseq contact probe))))
          (expect (plist-get init-opts :initializationOptions) :to-equal '(:existing-option t :pet-option t))))))

  (it "should preserve existing options when pet has no initialization options"
    (assume (and (require 'eglot nil t)
                 (fboundp 'eglot--executable-find))
            "Unsupported `eglot' version")

    (let ((eglot-server-programs `((python-mode . ,(eglot-alternatives '(("pylsp" :initializationOptions (:existing-option t))))))))
      (spy-on 'pet-lookup-eglot-server-initialization-options)
      (with-temp-buffer
        (setq default-directory "/home/user/project")
        (setq buffer-file-name "/home/user/project/file.py")
        (python-mode)
        (let* ((result (eglot--guess-contact))
               (contact (nth 3 result))
               (probe (cl-position-if #'keywordp contact))
               (init-opts (and probe (cl-subseq contact probe))))
          (expect (plist-get init-opts :initializationOptions) :to-equal '(:existing-option t)))))))

