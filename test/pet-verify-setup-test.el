;; -*- lexical-binding: t; -*-

(require 'pet)

(setq python-indent-guess-indent-offset nil)

(describe "pet-verify-setup"
  :var ((old-default-directory default-directory)
        (home (getenv "HOME"))
        (orig-getenv (symbol-function 'getenv))
        (process-environment (copy-sequence process-environment)))

  (before-each
    (setenv "HOME" "/home/user/")
    (setq-local default-directory "~/project/")
    (spy-on 'pet-project-root :and-return-value "/home/user/project/")
    (spy-on 'pet-virtualenv-root :and-return-value "/home/user/project/.venv/")
    (spy-on 'eglot--guess-contact
            :and-return-value
            '((python-mode python-ts-mode)
              (projectile . "/home/user/project/")
              eglot-lsp-server
              ("/home/user/project/.venv/bin/pyright-langserver" "--stdio"
               :initializationOptions
               (:python
                (:pythonPath
                 "/home/user/project/.venv/bin/python"
                 :venvPath
                 "/home/user/project/.venv/")))
              ("python" "python"))))

  (after-each
    (setenv "HOME" home)
    (setq-local default-directory old-default-directory))

  (it "should error when not in python mode"
    (expect (pet-verify-setup) :to-throw 'user-error))

  (it "should display unbound values"
    (with-temp-buffer
      (setq buffer-file-name "test.py")
      (python-mode)
      (pet-verify-setup)
      (expect
       (with-current-buffer "*pet info*"
         (re-search-forward "lsp-jedi-executable-command:\s+\\(.+\\)")
         (match-string 1))
       :to-equal "unbound")))

  (it "should display bound values"
    (with-temp-buffer
      (setq buffer-file-name "test.py")
      (python-mode)
      (pet-verify-setup)
      (expect
       (with-current-buffer "*pet info*"
         (re-search-forward "python-shell-interpreter:\s+\\(.+\\)")
         (match-string 1))
       :to-equal (if (or (< emacs-major-version 28)
                         (>= emacs-major-version 31))
                     "python"
                   "python3"))))

  (it "should display list as comma-separated values"
    (spy-on 'pet-flycheck-python-pylint-find-pylintrc)
    (spy-on 'pet-executable-find)
    (spy-on 'getenv :and-call-fake (lambda (name)
                                     (unless (equal name "XDG_CONFIG_HOME")
                                       (funcall orig-getenv name))))
    (with-temp-buffer
      (setq buffer-file-name "test.py")
      (python-mode)
      (setq-local flycheck-mode t)
      (pet-flycheck-toggle-local-vars)
      (pet-verify-setup)
      (expect
       (split-string (with-current-buffer "*pet info*"
                       (re-search-forward "flycheck-python-mypy-config:\s+\\(.+\\)")
                       (match-string 1))
                     "," t split-string-default-separators)
       :to-have-same-items-as '("mypy.ini" ".mypy.ini" "pyproject.toml" "setup.cfg" "/home/user/.config/mypy/config" "/home/user/.mypy.ini")))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
