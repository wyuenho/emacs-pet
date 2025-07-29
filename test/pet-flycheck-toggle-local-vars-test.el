;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-flycheck-toggle-local-vars"
  :var ((old-default-directory default-directory)
        (home (getenv "HOME"))
        (orig-getenv (symbol-function 'getenv))
        (process-environment (copy-sequence process-environment)))

  (before-each
    (setenv "HOME" "/home/user/")
    (setq-local default-directory "/home/user/")
    (defvar flycheck-mode t)
    (spy-on 'getenv :and-call-fake
            (lambda (name)
              (unless (member name '("XDG_CONFIG_HOME"))
                (funcall orig-getenv name)))))

  (after-each
    (setenv "HOME" home)
    (setq-local default-directory old-default-directory)
    (makunbound 'flycheck-mode)
    (unintern 'flycheck-mode obarray))

  (it "should set `flycheck' Python checkers variables to buffer-local when `flycheck-mode' is t"
    (spy-on 'pet-flycheck-python-pylint-find-pylintrc :and-return-value "/etc/pylintrc")
    (spy-on 'pet-executable-find :and-call-fake (lambda (name)
                                                  (pcase name
                                                    ("flake8" "/home/user/project/.venv/bin/flake8")
                                                    ("pylint" "/home/user/project/.venv/bin/pylint")
                                                    ("mypy" "/home/user/project/.venv/bin/mypy")
                                                    ("python" "/home/user/project/.venv/bin/python")
                                                    ("pyright" "/home/user/project/.venv/bin/pyright")
                                                    ("ruff" "/home/user/project/.venv/bin/ruff"))))
    (spy-on 'derived-mode-p :and-return-value t)
    (pet-flycheck-toggle-local-vars)
    (expect flycheck-python-mypy-config :to-equal `("mypy.ini" ".mypy.ini" "pyproject.toml" "setup.cfg" "/home/user/.config/mypy/config" "/home/user/.mypy.ini"))
    (expect flycheck-pylintrc :to-equal "/etc/pylintrc")
    (expect flycheck-python-flake8-executable :to-equal "/home/user/project/.venv/bin/flake8")
    (expect flycheck-python-pylint-executable :to-equal "/home/user/project/.venv/bin/pylint")
    (expect flycheck-python-mypy-executable :to-equal "/home/user/project/.venv/bin/mypy")
    (expect flycheck-python-mypy-python-executable :to-equal "/home/user/project/.venv/bin/python")
    (expect flycheck-python-pyright-executable :to-equal "/home/user/project/.venv/bin/pyright")
    (expect flycheck-python-pycompile-executable :to-equal flycheck-python-mypy-python-executable)
    (expect flycheck-python-ruff-executable :to-equal "/home/user/project/.venv/bin/ruff"))

  (it "should reset `flycheck' Python checkers variables to default when `flycheck-mode' is nil"
    (spy-on 'pet-flycheck-python-pylint-find-pylintrc :and-return-value "/etc/pylintrc")
    (spy-on 'pet-executable-find :and-call-fake (lambda (name)
                                                  (pcase name
                                                    ("flake8" "/home/user/project/.venv/bin/flake8")
                                                    ("pylint" "/home/user/project/.venv/bin/pylint")
                                                    ("mypy" "/home/user/project/.venv/bin/mypy")
                                                    ("python" "/home/user/project/.venv/bin/python")
                                                    ("pyright" "/home/user/project/.venv/bin/pyright")
                                                    ("ruff" "/home/user/project/.venv/bin/ruff"))))
    (spy-on 'derived-mode-p :and-return-value t)
    (pet-flycheck-toggle-local-vars)
    (setq-local flycheck-mode nil)

    (pet-flycheck-toggle-local-vars)
    (expect (local-variable-p 'flycheck-pylintrc) :to-be nil)
    (expect (local-variable-p 'flycheck-python-flake8-executable) :to-be nil)
    (expect (local-variable-p 'flycheck-python-pylint-executable) :to-be nil)
    (expect (local-variable-p 'flycheck-python-mypy-executable) :to-be nil)
    (expect (local-variable-p 'flycheck-python-mypy-python-executable) :to-be nil)
    (expect (local-variable-p 'flycheck-python-pyright-executable) :to-be nil)
    (expect (local-variable-p 'flycheck-python-pycompile-executable) :to-be nil)
    (expect (local-variable-p 'flycheck-python-ruff-executable) :to-be nil)

    (kill-local-variable 'flycheck-mode)))

