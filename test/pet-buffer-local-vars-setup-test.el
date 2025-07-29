;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-buffer-local-vars-setup"
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
    (spy-on 'pet-flycheck-setup)
    (spy-on 'pet-eglot-setup)
    (spy-on 'pet-dape-setup))

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
    (kill-local-variable 'apheleia-formatters)

    (makunbound 'format-all--executable-table)
    (unintern 'format-all--executable-table obarray)
    (makunbound 'apheleia-formatters)
    (unintern 'apheleia-formatters obarray))

  (it "should set up all buffer local variables for supported packages"
    (spy-on 'pet-executable-find :and-call-fake
            (lambda (exec)
              (pcase exec
                ("python"
                 "/usr/bin/python")
                ("python3"
                 "/usr/bin/python3")
                ("jedi-language-server"
                 "/usr/bin/jedi-language-server")
                ("pytest"
                 "/usr/bin/pytest")
                ("black"
                 "/usr/bin/black")
                ("isort"
                 "/usr/bin/isort")
                ("yapf"
                 "/usr/bin/yapf")
                ("ruff"
                 "/usr/bin/ruff")
                ("autopep8"
                 "/usr/bin/autopep8")
                ("pyls"
                 "/usr/bin/pyls")
                ("pylsp"
                 "/usr/bin/pylsp")
                ("pyright"
                 "/usr/bin/pyright")
                ("ty"
                 "/usr/bin/ty"))))
    (spy-on 'pet-virtualenv-root :and-return-value "/home/user/project/.venv/")
    (pet-buffer-local-vars-setup)

    (expect 'pet-flycheck-setup :to-have-been-called)
    (expect 'pet-eglot-setup :to-have-been-called)
    (expect 'pet-dape-setup :to-have-been-called)

    (expect (local-variable-p 'python-shell-interpreter) :to-be-truthy)
    (expect (local-variable-p 'python-shell-virtualenv-root) :to-be-truthy)
    (expect (local-variable-p 'lsp-jedi-executable-command) :to-be-truthy)
    (expect (local-variable-p 'lsp-pyls-plugins-jedi-environment) :to-be-truthy)
    (expect (local-variable-p 'lsp-pylsp-plugins-jedi-environment) :to-be-truthy)
    (expect (local-variable-p 'lsp-pyls-server-command) :to-be-truthy)
    (expect (local-variable-p 'lsp-pylsp-server-command) :to-be-truthy)
    (expect (local-variable-p 'lsp-python-ty-clients-server-command) :to-be-truthy)
    (expect (local-variable-p 'lsp-pyright-langserver-command) :to-be-truthy)
    (expect (local-variable-p 'lsp-pyright-venv-path) :to-be-truthy)
    (expect (local-variable-p 'lsp-pyright-python-executable-cmd) :to-be-truthy)
    (expect (local-variable-p 'lsp-ruff-server-command) :to-be-truthy)
    (expect (local-variable-p 'lsp-ruff-python-path) :to-be-truthy)
    (expect (local-variable-p 'dap-python-executable) :to-be-truthy)
    (expect (local-variable-p 'dap-variables-project-root-function) :to-be-truthy)
    (expect (local-variable-p 'python-pytest-executable) :to-be-truthy)
    (expect (local-variable-p 'pytest-global-name) :to-be-truthy)
    (expect (local-variable-p 'python-black-command) :to-be-truthy)
    (expect (local-variable-p 'python-isort-command) :to-be-truthy)
    (expect (local-variable-p 'blacken-executable) :to-be-truthy)
    (expect (local-variable-p 'yapfify-executable) :to-be-truthy)
    (expect (local-variable-p 'ruff-format-command) :to-be-truthy)
    (expect (local-variable-p 'py-autopep8-command) :to-be-truthy)
    (expect (local-variable-p 'format-all--executable-table) :to-be-truthy)
    (expect (local-variable-p 'apheleia-formatters) :to-be-truthy)

    (expect python-shell-interpreter :to-equal (pcase (default-value 'python-shell-interpreter)
                                                 ("python3" "/usr/bin/python3")
                                                 ("python" "/usr/bin/python")))
    (expect python-shell-virtualenv-root :to-equal "/home/user/project/.venv/")
    (expect lsp-jedi-executable-command :to-equal "/usr/bin/jedi-language-server")
    (expect lsp-pyls-plugins-jedi-environment :to-equal "/home/user/project/.venv/")
    (expect lsp-pylsp-plugins-jedi-environment :to-equal "/home/user/project/.venv/")
    (expect lsp-pyls-server-command :to-equal '("/usr/bin/pyls"))
    (expect lsp-pylsp-server-command :to-equal '("/usr/bin/pylsp"))
    (expect lsp-python-ty-clients-server-command :to-equal '("/usr/bin/ty" "server"))
    (expect lsp-pyright-langserver-command :to-equal "/usr/bin/pyright")
    (expect lsp-pyright-venv-path :to-equal "/home/user/project/.venv/")
    (expect lsp-pyright-python-executable-cmd :to-equal "/usr/bin/python")
    (expect lsp-ruff-server-command :to-equal '("/usr/bin/ruff" "server"))
    (expect lsp-ruff-python-path :to-equal "/usr/bin/python")
    (expect dap-python-executable :to-equal "/usr/bin/python")
    (expect dap-variables-project-root-function :to-equal #'pet-project-root)
    (expect python-pytest-executable :to-equal "/usr/bin/pytest")
    (expect pytest-global-name :to-equal "/usr/bin/pytest")
    (expect python-black-command :to-equal "/usr/bin/black")
    (expect python-isort-command :to-equal "/usr/bin/isort")
    (expect blacken-executable :to-equal "/usr/bin/black")
    (expect yapfify-executable :to-equal "/usr/bin/yapf")
    (expect ruff-format-command :to-equal "/usr/bin/ruff")
    (expect yapfify-executable :to-equal "/usr/bin/yapf")
    (expect py-autopep8-command :to-equal "/usr/bin/autopep8")
    (expect (gethash 'black format-all--executable-table) :to-equal "/usr/bin/black")
    (expect (gethash 'isort format-all--executable-table) :to-equal "/usr/bin/isort")
    (expect (gethash 'ruff format-all--executable-table) :to-equal "/usr/bin/ruff")
    (expect (gethash 'yapf format-all--executable-table) :to-equal "/usr/bin/yapf")
    (expect (car (alist-get 'black apheleia-formatters)) :to-equal "/usr/bin/black")
    (expect (car (alist-get 'isort apheleia-formatters)) :to-equal "/usr/bin/isort")
    (expect (car (alist-get 'ruff apheleia-formatters)) :to-equal "/usr/bin/ruff")
    (expect (car (alist-get 'ruff-isort apheleia-formatters)) :to-equal "/usr/bin/ruff")
    (expect (car (alist-get 'yapf apheleia-formatters)) :to-equal "/usr/bin/yapf"))

  (it "should run the hook `pet-after-buffer-local-vars-setup'"
    (spy-on 'pet-executable-find)
    (spy-on 'pet-virtualenv-root)

    (let* ((calls 0)
           (test-func (lambda () (cl-incf calls (1+ calls)))))
      (add-hook 'pet-after-buffer-local-vars-setup test-func)
      (pet-buffer-local-vars-setup)
      (expect calls :to-equal 1)
      (remove-hook 'pet-after-buffer-local-vars-setup test-func))))

