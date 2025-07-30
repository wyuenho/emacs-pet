;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-lookup-eglot-server-initialization-options"
  (before-each
    (spy-on 'pet-virtualenv-root :and-return-value "/home/user/project/")
    (spy-on 'pet-executable-find :and-call-fake
            (lambda (command)
              (assoc-default command
                             '(("flake8"               . "/usr/bin/flake8")
                               ("pylint"               . "/usr/bin/pylint")
                               ("python"               . "/usr/bin/python")
                               ("jedi-language-server" . "/home/user/.local/bin/jedi-language-server")
                               ("ruff"                 . "/usr/bin/ruff"))))))

  (it "should return eglot initialization options for pylsp"
    (expect (pet-lookup-eglot-server-initialization-options "/home/user/.local/bin/pylsp") :to-equal
            '(:pylsp
              (:plugins
               (:jedi
                (:environment
                 "/home/user/project/")
                :ruff
                (:executable
                 "/usr/bin/ruff")
                :pylsp_mypy
                (:overrides
                 ["--python-executable" "/usr/bin/python" t])
                :flake8
                (:executable
                 "/usr/bin/flake8")
                :pylint
                (:executable
                 "/usr/bin/pylint"))))))

  (it "should return eglot initialization options for pyls"
    (expect (pet-lookup-eglot-server-initialization-options "/home/user/.local/bin/pyls") :to-equal
            '(:pyls
              (:plugins
               (:jedi
                (:environment
                 "/home/user/project/")
                :pylint
                (:executable
                 "/usr/bin/pylint"))))))

  (it "should return eglot initialization options for based-pyright"
    (expect (pet-lookup-eglot-server-initialization-options "/home/user/.local/bin/basedpyright-langserver") :to-equal
            `(:python
              (:pythonPath
               "/usr/bin/python"
               :venvPath
               "/home/user/project/"))))

  (it "should return eglot initialization options for pyright"
    (expect (pet-lookup-eglot-server-initialization-options "/home/user/.local/bin/pyright-langserver") :to-equal
            `(:python
              (:pythonPath
               "/usr/bin/python"
               :venvPath
               "/home/user/project/"))))

  (it "should return eglot initialization options for jedi-language-server"
    (expect (pet-lookup-eglot-server-initialization-options "jedi-language-server") :to-equal
            '(:jedi
              (:executable
               (:command
                "/home/user/.local/bin/jedi-language-server")
               :workspace
               (:environmentPath
                "/usr/bin/python")))))

  (it "should return eglot initialization options for ruff"
    (expect (pet-lookup-eglot-server-initialization-options "ruff") :to-equal
            '(:settings
              (:interpreter
               "/usr/bin/python"
               :path
               "/usr/bin/ruff"))))

  (it "should return eglot initialization options for ruff-lsp"
    (expect (pet-lookup-eglot-server-initialization-options "ruff-lsp") :to-equal
            '(:settings
              (:interpreter
               "/usr/bin/python"
               :path
               "/usr/bin/ruff"))))

  (it "should return eglot initialization options for pylsp when given as command list"
    (expect (pet-lookup-eglot-server-initialization-options '("/home/user/.local/bin/pylsp" "--arg1" "--arg2")) :to-equal
            '(:pylsp
              (:plugins
               (:jedi
                (:environment
                 "/home/user/project/")
                :ruff
                (:executable
                 "/usr/bin/ruff")
                :pylsp_mypy
                (:overrides
                 ["--python-executable" "/usr/bin/python" t])
                :flake8
                (:executable
                 "/usr/bin/flake8")
                :pylint
                (:executable
                 "/usr/bin/pylint"))))))

  (it "should return first matching server when command list has multiple servers"
    (expect (pet-lookup-eglot-server-initialization-options '("/usr/bin/unknown-server" "/home/user/.local/bin/pyright-langserver")) :to-equal
            '(:python
              (:pythonPath
               "/usr/bin/python"
               :venvPath
               "/home/user/project/"))))

  (it "should return nil when command list has no recognized servers"
    (expect (pet-lookup-eglot-server-initialization-options '("/usr/bin/unknown-server" "/usr/bin/another-unknown")) :to-be nil))

  (it "should handle empty command list"
    (expect (pet-lookup-eglot-server-initialization-options '()) :to-be nil)))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
