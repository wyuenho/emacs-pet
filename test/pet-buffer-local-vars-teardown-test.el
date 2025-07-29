;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-buffer-local-vars-teardown"
  (before-each
    (spy-on 'pet-flycheck-teardown)
    (spy-on 'pet-eglot-teardown)
    (spy-on 'pet-dape-teardown))

  (after-each
    (kill-local-variable 'python-shell-interpreter)
    (kill-local-variable 'python-shell-virtualenv-root)
    (kill-local-variable 'lsp-jedi-executable-command)
    (kill-local-variable 'lsp-pyls-plugins-jedi-environment)
    (kill-local-variable 'lsp-pylsp-plugins-jedi-environment)
    (kill-local-variable 'lsp-pyright-langserver-command)
    (kill-local-variable 'lsp-pyright-venv-path)
    (kill-local-variable 'lsp-pyright-python-executable-cmd)
    (kill-local-variable 'lsp-ruff-server-command)
    (kill-local-variable 'lsp-ruff-python-path)
    (kill-local-variable 'dap-python-executable)
    (kill-local-variable 'dap-variables-project-root-function)
    (kill-local-variable 'python-pytest-executable)
    (kill-local-variable 'pytest-global-name)
    (kill-local-variable 'python-black-command)
    (kill-local-variable 'python-isort-command)
    (kill-local-variable 'blacken-executable)
    (kill-local-variable 'yapfify-executable)
    (kill-local-variable 'ruff-format-command)
    (kill-local-variable 'py-autopep8-command)
    (kill-local-variable 'format-all--executable-table)
    (kill-local-variable 'apheleia-formatters))

  (it "should run the hook `pet-before-buffer-local-vars-teardown'"
    (let* ((calls 0)
           (test-func (lambda () (cl-incf calls (1+ calls)))))
      (add-hook 'pet-before-buffer-local-vars-teardown test-func)
      (pet-buffer-local-vars-teardown)
      (expect calls :to-equal 1)
      (remove-hook 'pet-before-buffer-local-vars-teardown test-func)))

  (it "should reset all buffer local variables for supported packages to default"
    (pet-buffer-local-vars-setup)
    (pet-buffer-local-vars-teardown)

    (expect 'pet-flycheck-teardown :to-have-been-called)
    (expect 'pet-eglot-teardown :to-have-been-called)
    (expect 'pet-dape-teardown :to-have-been-called)

    (expect (local-variable-p 'python-shell-interpreter) :not :to-be-truthy)
    (expect (local-variable-p 'python-shell-virtualenv-root) :not :to-be-truthy)
    (expect (local-variable-p 'lsp-jedi-executable-command) :not :to-be-truthy)
    (expect (local-variable-p 'lsp-pyls-plugins-jedi-environment) :not :to-be-truthy)
    (expect (local-variable-p 'lsp-pylsp-plugins-jedi-environment) :not :to-be-truthy)
    (expect (local-variable-p 'lsp-pyright-langserver-command) :not :to-be-truthy)
    (expect (local-variable-p 'lsp-pyright-venv-path) :not :to-be-truthy)
    (expect (local-variable-p 'lsp-pyright-python-executable-cmd) :not :to-be-truthy)
    (expect (local-variable-p 'lsp-ruff-server-command) :not :to-be-truthy)
    (expect (local-variable-p 'lsp-ruff-python-path) :not :to-be-truthy)
    (expect (local-variable-p 'dap-python-executable) :not :to-be-truthy)
    (expect (local-variable-p 'dap-variables-project-root-function) :not :to-be-truthy)
    (expect (local-variable-p 'python-pytest-executable) :not :to-be-truthy)
    (expect (local-variable-p 'pytest-global-name) :not :to-be-truthy)
    (expect (local-variable-p 'python-black-command) :not :to-be-truthy)
    (expect (local-variable-p 'python-isort-command) :not :to-be-truthy)
    (expect (local-variable-p 'blacken-executable) :not :to-be-truthy)
    (expect (local-variable-p 'yapfify-executable) :not :to-be-truthy)
    (expect (local-variable-p 'ruff-format-command) :not :to-be-truthy)
    (expect (local-variable-p 'py-autopep8-command) :not :to-be-truthy)
    (expect (local-variable-p 'format-all--executable-table) :not :to-be-truthy)
    (expect (local-variable-p 'apheleia-formatters) :not :to-be-truthy)))

