;; -*- lisp-indent-offset: 2; lexical-binding: t; -*-

(require 'dap-python)
(require 'flycheck)
(require 'lsp-jedi)
(require 'lsp-pyright)
(require 'python)
(require 'python-black)
(require 'python-isort)
(require 'python-pytest)
(require 'projectile)

(require 'pet)

(describe "pet-system-bin-dir"
  (describe "when called on Windows"
    (before-each
      (setq-local system-type 'windows-nt))
    (after-each
      (kill-local-variable 'system-type))

    (it "should return Scripts"
      (expect (pet-system-bin-dir) :to-equal "Scripts")))

  (describe "when called on non-Windows"
    (before-each
      (setq-local system-type 'gnu/linux))
    (after-each
      (kill-local-variable 'system-type))

    (it "should return bin"
      (expect (pet-system-bin-dir) :to-equal "bin"))))

(describe "pet-project-root"
  (it "should find project root"))

(describe "pet-find-file-from-project-root"
  (it "should find file from project root"))

(describe "pet-parse-json"
  (it "should parse a JSON string to alist"))

(describe "pet-parse-config-file"
  (it "should parse config file content to alist"))

(describe "pet-watch-config-file"
  (it "should watch for changes in config file and update cache variable"))

(describe "pet-def-config-accessor"
  (it "should create cache variable")
  (it "should create cache access function"))

(describe "pet-use-pre-commit-p"
  (it "should return t if project uses `pre-commit'")
  (it "should return nil if project does not use `pre-commit'"))

(describe "pet-use-poetry-p"
  (it "should return t if project uses `poetry'")
  (it "should return nil if project does not use `poetry'"))

(describe "pet-use-pyenv-p"
  (it "should return t if project uses `pyenv'")
  (it "should return nil if project does not use `pyenv'"))

(describe "pet-use-pipenv-p"
  (it "should return t if project uses `pipenv'")
  (it "should return nil if project does not use `pipenv'"))

(describe "pet-pre-commit-config-has-hook-p"
  (it "should return t if `.pre-commit-config.yaml' has hook declared")
  (it "should return nil if `.pre-commit-config.yaml' does not have hook declared"))

(describe "pet-parse-pre-commit-db"
  (it "should parse `pre-commit' database to alist"))

(describe "pet-pre-commit-virtualenv-path"
  (it "should return absolute path to the virtualenv of a `pre-commit' hook defined in a project"))

(describe "pet-executable-find"
  (it "should return the absolute path to the executable for a project using `pre-commit'")
  (it "should return the absolute path the executable for a project if its virtualenv is found")
  (it "should return the absolute path the executable for a project from `exec-path'"))

(describe "pet-virtualenv-root"
  (it "should return the absolute path of the virtualenv for a project using `poetry'")
  (it "should return the absolute path of the virtualenv for a project using `pipenv'")
  (it "should return the absolute path of the virtualenv for a project from `VIRTUAL_ENV'")
  (it "should return the absolute path of the `.venv' or `venv' directory in a project")
  (it "should return the absolute path of the virtualenv for a project using `pyenv'"))

(describe "pet-flycheck-python-pylint-find-pylintrc"
  (it "should return the absolute path to `pylintrc' from the project root")
  (it "should return the absolute path to `pylintrc' from `default-directory'")
  (it "should return the absolute path to `pylintrc' from a python package directory hierarchy")
  (it "should return the absolute path to `pylintrc' from `PYLINTRC'")
  (it "should return the absolute path to `pylintrc' from `XDG_CONFIG_HOME'")
  (it "should return the absolute path to `pylintrc' from `HOME'")
  (it "should return the absolute path to `pylintrc' from `/etc'"))

(describe "pet-flycheck-checker-get-advice"
  (it "should delegate `python-mypy' checker property to `pet-flycheck-checker-props'"))

(describe "pet-flycheck-toggle-local-vars"
  (it "should set `flycheck' Python checkers variables to buffer-local when `flycheck-mode-on' is t")
  (it "should reset `flycheck' Python checkers variables to default when `flycheck-mode-on' is nil"))

(describe "pet-flycheck-setup"
  (it "should set up `flycheck' python checker configuration file names")
  (it "should advice `flycheck-checker-get' with `pet-flycheck-checker-get-advice'")
  (it "should add `pet-flycheck-toggle-local-vars' to `flycheck-mode-hook'"))

(describe "pet-flycheck-teardown"
  (it "should remove advice `pet-flycheck-checker-get-advice' from `flycheck-checker-get'")
  (it "should remove `pet-flycheck-toggle-local-vars' from `flycheck-mode-hook'")
  (it "should reset `flycheck' Python checkers variables to default"))

(describe "pet-buffer-local-vars-setup"
  (it "should set up all buffer local variables for supported packages"))

(describe "pet-buffer-local-vars-teardown"
  (it "should reset all buffer local variables for supported packages to default"))

(describe "pet-minor-mode"
  (it "should set up all buffer local variables for supported packages if `pet-minor-mode' is t")
  (it "should reset all buffer local variables for supported packages to default if `pet-minor-mode' is nil"))

(describe "global-pet-minor-mode"
  (it "should turn on `pet-minor-mode' if current buffer major mode is derived from `python-mode'")
  (it "should turn of `pet-minor-mode' if current buffer major mode is not derived from `python-mode'"))

(describe "pet-cleanup-watchers-and-caches"
  (describe "when the last Python buffer for a project is killed"
    (it "should clear all watched files")
    (it "should clear all config file caches")
    (it "should clean `pet-project-virtualenv-cache'")))
