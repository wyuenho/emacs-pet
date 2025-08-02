;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-buffer-local-vars-teardown"
  (before-each
    (defvar format-all--executable-table
      (let ((tab (make-hash-table)))
        (puthash 'black "black" tab)
        (puthash 'isort "isort" tab)
        (puthash 'ruff "ruff" tab)
        (puthash 'yapf "yapf" tab)
        tab))
    (defvar apheleia-formatters
      '((black      . ("black"))
        (isort      . ("isort"))
        (ruff       . ("ruff"))
        (ruff-isort . ("ruff"))
        (yapf       . ("yapf"))))
    (spy-on 'pet-executable-find)
    (spy-on 'pet-virtualenv-root)
    (spy-on 'pet-flycheck-setup)
    (spy-on 'pet-eglot-setup)
    (spy-on 'pet-dape-setup)
    (spy-on 'pet-flycheck-teardown)
    (spy-on 'pet-eglot-teardown)
    (spy-on 'pet-dape-teardown))

  (after-each
    (makunbound 'format-all--executable-table)
    (unintern 'format-all--executable-table obarray)
    (makunbound 'apheleia-formatters)
    (unintern 'apheleia-formatters obarray))

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
    (expect (local-variable-p 'apheleia-formatters) :not :to-be-truthy)
    (expect (local-variable-p 'pytest-global-name) :not :to-be-truthy)))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
