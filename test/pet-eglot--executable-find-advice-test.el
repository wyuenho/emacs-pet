;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-eglot--executable-find-advice"
  (it "should delegate to `pet-executable-find' for Python LSP servers"
    (spy-on 'eglot--executable-find :and-call-fake (lambda (&rest args) (string-join args " ")))
    (spy-on 'pet-executable-find :and-call-fake 'identity)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "pylsp") :to-equal "pylsp")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "pylsp")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "pyls") :to-equal "pyls")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "pyls")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "basedpyright-langserver") :to-equal "basedpyright-langserver")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "basedpyright-langserver")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "pyright-langserver") :to-equal "pyright-langserver")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "pyright-langserver")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "jedi-language-server") :to-equal "jedi-language-server")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "jedi-language-server")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "ruff") :to-equal "ruff")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "ruff")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "ruff-lsp") :to-equal "ruff-lsp")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "ruff-lsp")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "sh" "-c") :to-equal "sh -c")
    (expect 'eglot--executable-find :to-have-been-called-with "sh" "-c")))

