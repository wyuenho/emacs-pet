;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "Pyright server selection"
  (before-each
    (spy-on 'pet-executable-find :and-return-value "/venv/bin/tool")
    (spy-on 'pet-virtualenv-root :and-return-value "/venv/")
    (spy-on 'pet-flycheck-setup)
    (spy-on 'pet-eglot-setup)
    (spy-on 'pet-dape-setup)
    (spy-on 'pet-flycheck-teardown)
    (spy-on 'pet-eglot-teardown)
    (spy-on 'pet-dape-teardown))

  (it "preserves inherited Pyright and Basedpyright selections"
    (dolist (server '("pyright" "basedpyright"))
      (let ((lsp-pyright-langserver-command server))
        (with-temp-buffer
          (pet-buffer-local-vars-setup)
          (expect lsp-pyright-langserver-command :to-equal server)
          (expect (local-variable-p 'lsp-pyright-langserver-command)
                  :not :to-be-truthy)))))

  (it "preserves a local selection through repeated setup and teardown"
    (with-temp-buffer
      (setq-local lsp-pyright-langserver-command "basedpyright")
      (pet-buffer-local-vars-setup)
      (pet-buffer-local-vars-setup)
      (pet-buffer-local-vars-teardown)
      (expect lsp-pyright-langserver-command :to-equal "basedpyright")
      (expect (local-variable-p 'lsp-pyright-langserver-command) :to-be-truthy)))

  (it "still configures the Python environment for the selected server"
    (with-temp-buffer
      (setq-local lsp-pyright-langserver-command "basedpyright")
      (pet-buffer-local-vars-setup)
      (expect lsp-pyright-python-executable-cmd :to-equal "/venv/bin/tool")
      (expect lsp-pyright-venv-path :to-equal "/venv/")
      (pet-buffer-local-vars-teardown)
      (expect (local-variable-p 'lsp-pyright-python-executable-cmd)
              :not :to-be-truthy)
      (expect (local-variable-p 'lsp-pyright-venv-path) :not :to-be-truthy))))
