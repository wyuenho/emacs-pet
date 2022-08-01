;; -*- lisp-indent-offset: 2; -*-

(describe "python-exec-find-project-root"
  (it "should find project root"))

(describe "python-exec-find-find-file-from-project-root"
  (it "should find file from project root"))

(describe "python-exec-find-parse-json"
  (it "should parse a JSON string to alist"))

(describe "python-exec-find-parse-config-file"
  (it "should parse config file content to alist"))

(describe "python-exec-find-watch-config-file"
  (it "should watch for changes in config file and update cache variable"))

(describe "python-exec-find-def-config-accessor"
  (it "should create cache variable")
  (it "should create cache access function"))

(describe "python-exec-find-use-pre-commit-p"
  (it "should return t if project uses `pre-commit'")
  (it "should return nil if project does not use `pre-commit'"))

(describe "python-exec-find-use-poetry-p"
  (it "should return t if project uses `poetry'")
  (it "should return nil if project does not use `poetry'"))

(describe "python-exec-find-use-pyenv-p"
  (it "should return t if project uses `pyenv'")
  (it "should return nil if project does not use `pyenv'"))

(describe "python-exec-find-pre-commit-config-has-hook-p"
  (it "should return t if `.pre-commit-config.yaml' has hook declared")
  (it "should return nil if `.pre-commit-config.yaml' does not have hook declared"))

(describe "python-exec-find-parse-pre-commit-db"
  (it "should parse `pre-commit' database to alist"))

(describe "python-exec-find-pre-commit-virtualenv-path"
  (it "should return absolute path to the virtualenv of a `pre-commit' hook defined in a project"))

(describe "python-exec-find-executable-find"
  (it "should return the absolute path to the executable for a project using `pre-commit'")
  (it "should return the absolute path the executable for a project using `poetry'")
  (it "should return the absolute path the executable for a project using `pyenv'")
  (it "should return the absolute path the executable for a project from `exec-path'"))

(describe "python-exec-find-virtualenv-root"
  (it "should return the absolute path the virtualenv for a project using `poetry'")
  (it "should return the absolute path the virtualenv for a project using `pyenv'")
  (it "should return the absolute path the virtualenv for a project from `VIRTUAL_ENV'"))

(describe "python-exec-find-flycheck-python-pylint-find-pylintrc"
  (it "should return the absolute path to `pylintrc' from the project root")
  (it "should return the absolute path to `pylintrc' from `default-directory'")
  (it "should return the absolute path to `pylintrc' from a python package directory hierarchy")
  (it "should return the absolute path to `pylintrc' from `PYLINTRC'")
  (it "should return the absolute path to `pylintrc' from `XDG_CONFIG_HOME'")
  (it "should return the absolute path to `pylintrc' from `HOME'")
  (it "should return the absolute path to `pylintrc' from `/etc'"))

(describe "python-exec-find-flycheck-checker-get-advice"
  (it "should delegate `python-mypy' checker property to `python-exec-find-flycheck-checker-props'"))

(describe "python-exec-find-flycheck-toggle-local-vars"
  (it "should set `flycheck' Python checkers variables to buffer-local when `flycheck-mode-on' is t")
  (it "should reset `flycheck' Python checkers variables to default when `flycheck-mode-on' is nil"))

(describe "python-exec-find-flycheck-setup"
  (it "should set up `flycheck' python checker configuration file names")
  (it "should advice `flycheck-checker-get' with `python-exec-find-flycheck-checker-get-advice'")
  (it "should add `python-exec-find-flycheck-toggle-local-vars' to `flycheck-mode-hook'"))

(describe "python-exec-find-flycheck-teardown"
  (it "should remove advice `python-exec-find-flycheck-checker-get-advice' from `flycheck-checker-get'")
  (it "should remove `python-exec-find-flycheck-toggle-local-vars' from `flycheck-mode-hook'")
  (it "should reset `flycheck' Python checkers variables to default"))

(describe "python-exec-find-buffer-local-vars-setup"
  (it "should set up all buffer local variables for supported packages"))

(describe "python-exec-find-buffer-local-vars-teardown"
  (it "should reset all buffer local variables for supported packages to default"))

(describe "python-exec-find-minor-mode"
  (it "should set up all buffer local variables for supported packages if `python-exec-find-minor-mode' is t")
  (it "should reset all buffer local variables for supported packages to default if `python-exec-find-minor-mode' is nil"))

(describe "global-python-exec-find-minor-mode"
  (it "should turn on `python-exec-find-minor-mode' if current buffer major mode is derived from `python-mode'")
  (it "should turn of `python-exec-find-minor-mode' if current buffer major mode is not derived from `python-mode'"))

(describe "python-exec-find-cleanup-watchers-and-caches"
  (describe "when the last Python buffer for a project is killed"
    (it "should clear all watched files")
    (it "should clear all config file caches")
    (it "should clean `python-exec-find-project-virtualenv-cache'")))
