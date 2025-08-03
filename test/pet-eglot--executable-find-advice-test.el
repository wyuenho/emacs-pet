;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-eglot--executable-find-advice"
  (it "should delegate to `pet-executable-find' for all executables, then fall back to original"
    (spy-on 'pet-executable-find :and-call-fake 'identity)
    (spy-on 'pet--orig-executable-find)

    (expect (pet-eglot--executable-find-advice "pylsp") :to-equal "pylsp")
    (expect 'pet--orig-executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice "pyls") :to-equal "pyls")
    (expect 'pet--orig-executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice "basedpyright-langserver") :to-equal "basedpyright-langserver")
    (expect 'pet--orig-executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice "pyright-langserver") :to-equal "pyright-langserver")
    (expect 'pet--orig-executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice "jedi-language-server") :to-equal "jedi-language-server")
    (expect 'pet--orig-executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice "ruff") :to-equal "ruff")
    (expect 'pet--orig-executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice "ruff-lsp") :to-equal "ruff-lsp")
    (expect 'pet--orig-executable-find :not :to-have-been-called))

  (it "should fall back to original executable-find when pet-executable-find returns nil"
    (let ((pet--orig-executable-find (lambda (cmd &optional remote)
                                       (if remote "remote-result" "local-result"))))
      (spy-on 'pet-executable-find)

      (expect (pet-eglot--executable-find-advice "nonexistent") :to-equal "local-result")
      (expect (pet-eglot--executable-find-advice "nonexistent" t) :to-equal "remote-result"))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
