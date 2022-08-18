;; -*- lisp-indent-offset: 2; lexical-binding: t; -*-

(require 'blacken)
(require 'dap-python)
(require 'flycheck)
(require 'lsp-jedi)
(require 'lsp-pyright)
(require 'project)
(require 'projectile)
(require 'python)
(require 'python-black)
(require 'python-isort)
(require 'python-pytest)
(require 'yapfify)

(require 'pet)

;; (setq pet-debug t)

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

(describe "pet-report-error"
  (describe "when `pet-debug' is t"
    (before-each
      (setq-local pet-debug t))

    (after-each
      (kill-local-variable 'pet-debug))

    (it "should call minibuffer-message"
      (buttercup-suppress-warning-capture
        (spy-on 'minibuffer-message :and-call-fake 'ignore))
      (pet-report-error '(error . ("error")))
      (expect 'minibuffer-message :to-have-been-called-with "error")))

  (it "should not call minibuffer-message when `pet-debug' is nil"
    (pet-report-error '(error . ("error")))
    (expect 'minibuffer-message :not :to-have-been-called)))

(describe "pet-project-root"
  (it "should find project root with `projectile'"
    (spy-on 'projectile-project-root :and-return-value "/")
    (expect (pet-project-root) :to-equal "/"))

  (it "should find project root with `project.el'"
    (spy-on 'projectile-project-root)
    (spy-on 'project-current :and-return-value (if (< emacs-major-version 29) '(vc . "/") '(vc Git "/")))
    (expect (pet-project-root) :to-equal "/"))

  (it "should return nil when Python file does not appear to be in a project"
    (spy-on 'projectile-project-root)
    (spy-on 'project-current)
    (expect (pet-project-root) :to-be nil)))

(describe "pet-find-file-from-project-root"
  (it "should find file from project root"
    (spy-on 'pet-project-root :and-return-value "/etc")
    (expect (pet-find-file-from-project-root "\\`passwd\\'") :to-equal "/etc/passwd"))

  (it "should return nil when file not found from project root"
    (spy-on 'pet-project-root :and-return-value "/etc")
    (expect (pet-find-file-from-project-root "idontexist") :to-be nil))

  (it "should return nil when not under a project"
    (spy-on 'pet-project-root)
    (expect (pet-find-file-from-project-root "foo") :to-be nil)))

(describe "pet-parse-json"
  (it "should parse a JSON string to an alist"
    (expect (pet-parse-json "{\"foo\":\"bar\",\"baz\":[\"buz\",1]}") :to-equal '((foo . "bar") (baz "buz" 1)))))

(describe "pet-parse-config-file"
  :var* ((yaml-content "foo: bar\nbaz:\n  - buz\n  - 1\n")
          (toml-content "foo = \"bar\"\nbaz = [\"buz\", 1]\n")
          (json-content "{\"foo\":\"bar\",\"baz\":[\"buz\",1]}")
          (yaml-file (make-temp-file "pet-test" nil ".yaml" yaml-content))
          (toml-file (make-temp-file "pet-test" nil ".toml" toml-content))
          (json-file (make-temp-file "pet-test" nil ".json" json-content)))

  (after-all
    (delete-file yaml-file)
    (delete-file toml-file)
    (delete-file json-file))

  (before-each
    (setq-local pet-toml-to-json-program "tomljson")
    (setq-local pet-toml-to-json-program-arguments nil)
    (setq-local pet-yaml-to-json-program "yq")
    (setq-local pet-yaml-to-json-program-arguments '("--output-format" "json")))

  (after-each
    (kill-local-variable 'pet-toml-to-json-program)
    (kill-local-variable 'pet-toml-to-json-program-arguments)
    (kill-local-variable 'pet-yaml-to-json-program)
    (kill-local-variable 'pet-yaml-to-json-program-arguments))

  (it "should parse a YAML file content to alist"
    (expect (pet-parse-config-file yaml-file) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a TOML file content to alist"
    (expect (pet-parse-config-file toml-file) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a JSON file content to alist"
    (expect (pet-parse-config-file json-file) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil)))

(describe "pet-make-config-file-change-callback"
  (it "should return a function"
    (expect (functionp (pet-make-config-file-change-callback 'cache 'parser)) :to-be-truthy))

  (describe "when received deleted event"
    :var* ((descriptor 1)
            (file "/home/user/project/tox.ini")
            (event `((,file . ,descriptor))))

    (before-each
      (spy-on 'file-notify-rm-watch)
      (setq-local pet-watched-config-files event)
      (defvar cache `((,file . "content")))
      (defvar callback (pet-make-config-file-change-callback 'cache nil))
      (funcall callback `(,descriptor deleted ,file)))

    (after-each
      (kill-local-variable 'pet-watched-config-files)
      (makunbound 'cache)
      (unintern 'cache)
      (makunbound 'callback)
      (unintern 'callback))

    (it "should remove file watcher"
      (expect 'file-notify-rm-watch :to-have-been-called-with descriptor))

    (it "should remove entry from cache"
      (expect (assoc-default file cache) :not :to-be-truthy))

    (it "should remove entry from `pet-watched-config-files'"
      (expect (assoc-default file pet-watched-config-files) :not :to-be-truthy)))

  (describe "when received changed event"
    :var ((file "/home/user/project/tox.ini"))

    (before-each
      (defvar cache nil)
      (defun parser (file)
        "content")

      (spy-on 'parser :and-call-through)

      (defvar callback (pet-make-config-file-change-callback 'cache 'parser))
      (funcall callback `(1 changed ,file)))

    (after-each
      (makunbound 'cache)
      (unintern 'cache)
      (fmakunbound 'parser)
      (unintern 'parser)
      (makunbound 'callback)
      (unintern 'callback))

    (it "parse the file again"
      (expect 'parser :to-have-been-called-with file)
      (expect (spy-context-return-value (spy-calls-most-recent 'parser)) :to-equal "content"))

    (it "should set parsed value to cache"
      (expect (assoc-default file cache) :to-equal "content"))))

(describe "pet-watch-config-file"
  :var ((file "/home/user/project/tox.ini"))

  (describe "when the file is being watched"
    (before-each
      (spy-on 'file-notify-add-watch)
      (setq-local pet-watched-config-files `((,file . 1))))

    (after-each
      (kill-local-variable 'pet-watched-config-files))

    (it "should do nothing"
      (expect (pet-watch-config-file file nil nil) :to-be nil)
      (expect 'file-notify-add-watch :not :to-have-been-called)))

  (describe "when the file isn't being watched"
    :var ((callback (lambda ())))

    (before-each
      (spy-on 'file-notify-add-watch :and-return-value 1)
      (spy-on 'pet-make-config-file-change-callback :and-return-value callback)
      (defvar pet-tox-ini-cache nil)
      (defun parser (file) "content"))

    (after-each
      (makunbound 'pet-tox-ini-cache)
      (unintern 'pet-tox-ini-cache)
      (fmakunbound 'parser)
      (unintern 'parser))

    (it "should add an entry to the watched files cache"
      (pet-watch-config-file file 'pet-tox-ini-cache 'parser)
      (expect 'file-notify-add-watch :to-have-been-called-with file '(change) callback)
      (expect 'pet-make-config-file-change-callback :to-have-been-called-with 'pet-tox-ini-cache 'parser)
      (expect (assoc-default file pet-watched-config-files) :to-equal 1))))

(describe "pet-def-config-accessor"
  (before-each
    (defun parser (file) "content"))

  (after-all
    (fmakunbound 'parser)
    (unintern 'parser))

  (before-each
    (pet-def-config-accessor tox-ini :file-name "tox.ini" :parser parser))

  (after-each
    (fmakunbound 'pet-tox-ini)
    (unintern 'pet-tox-ini)
    (makunbound 'pet-tox-ini-cache)
    (unintern 'pet-tox-ini-cache))

  (it "should create cache variable"
    (expect (boundp 'pet-tox-ini-cache) :to-be t))

  (it "should create cache access function"
    (expect (fboundp 'pet-tox-ini) :to-be t))

  (describe "the cache access function"
    (before-each
      (spy-on 'pet-find-file-from-project-root :and-return-value "/home/user/project/tox.ini")
      (buttercup-suppress-warning-capture
        (spy-on 'pet-watch-config-file :and-call-fake 'ignore))
      (spy-on 'parser :and-call-through))

    (after-each
      (setq pet-tox-ini-cache nil))

    (it "should return cached value if it exists"
      (push (cons "/home/user/project/tox.ini" "cached content") pet-tox-ini-cache)
      (expect (pet-tox-ini) :to-equal "cached content")
      (expect 'pet-watch-config-file :not :to-have-been-called)
      (expect 'parser :not :to-have-been-called))

    (describe "when the config file content has not been cached"
      (it "should return parsed file content"
        (expect (pet-tox-ini) :to-equal "content"))

      (it "should watch file"
        (pet-tox-ini)
        (expect 'pet-watch-config-file :to-have-been-called-with "/home/user/project/tox.ini" 'pet-tox-ini-cache 'parser))

      (it "should cache config file content"
        (pet-tox-ini)
        (expect (alist-get "/home/user/project/tox.ini" pet-tox-ini-cache nil nil 'equal) :to-equal "content")))))

(describe "pet-use-pre-commit-p"
  (describe "when the project has a `.pre-commit-config.yaml' file"
    (before-each
      (spy-on 'pet-pre-commit-config :and-return-value t))

    (it "should return `pre-commit' path if `pre-commit' is found"
      (spy-on 'executable-find :and-return-value "/usr/bin/pre-commit")
      (expect (pet-use-pre-commit-p) :to-equal "/usr/bin/pre-commit")

      (spy-on 'pet-virtualenv-root :and-return-value "/home/user/venv/test")
      (let ((call-count 0))
        (spy-on 'executable-find :and-call-fake (lambda (&rest _)
                                                  (setq call-count (1+ call-count))
                                                  (when (= call-count 2)
                                                    "/home/user/venv/test/bin/pre-commit"))))

      (expect (pet-use-pre-commit-p) :to-equal "/home/user/venv/test/bin/pre-commit"))

    (it "should return nil if `pre-commit' is not found"
      (spy-on 'executable-find :and-return-value "/usr/bin/pre-commit")
      (expect (pet-use-pre-commit-p) :to-equal "/usr/bin/pre-commit")))

  (describe "when the project does not have a `.pre-commit-config.yaml' file"
    (before-each
      (spy-on 'pet-pre-commit-config))

    (it "should return nil if `pre-commit' is found"
      (spy-on 'executable-find :and-return-value "/usr/bin/pre-commit")
      (expect (pet-use-pre-commit-p) :to-be nil))

    (it "should return nil if `pre-commit' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pre-commit-p) :to-be nil))))

(describe "pet-use-conda-p"
  (describe "when the project has an `environment[a-zA-Z0-9-_].yaml' file"
    (before-each
      (spy-on 'pet-environment :and-return-value t))

    (it "should return `conda' path if `conda' is found"
      (spy-on 'executable-find :and-call-fake (lambda (_) (when (equal _ "conda") "/usr/bin/conda")))
      (expect (pet-use-conda-p) :to-equal "/usr/bin/conda"))

    (it "should return `mamba' path if `mamba' is found"
      (spy-on 'executable-find :and-call-fake (lambda (_) (when (equal _ "mamba") "/usr/bin/mamba")))
      (expect (pet-use-conda-p) :to-equal "/usr/bin/mamba"))

    (it "should return `micromamba' path if `micromamba' is found"
      (spy-on 'executable-find :and-call-fake (lambda (_) (when (equal _ "micromamba") "/usr/bin/micromamba")))
      (expect (pet-use-conda-p) :to-equal "/usr/bin/micromamba"))

    (it "should return nil if none of `conda' or `mamba' or `micromamba' is found"
      (spy-on 'executable-find)
      (expect (pet-use-conda-p) :to-be nil)))

  (describe "when the project does not have a `environment[a-zA-Z0-9-_].yaml' file"
    (before-each
      (spy-on 'pet-environment))

    (it "should return nil if `conda' is found"
      (spy-on 'executable-find :and-call-fake (lambda (_) (when (equal _ "conda") "/usr/bin/conda")))
      (expect (pet-use-conda-p) :to-be nil))

    (it "should return nil if `mamba' is found"
      (spy-on 'executable-find :and-call-fake (lambda (_) (when (equal _ "mamba") "/usr/bin/mamba")))
      (expect (pet-use-conda-p) :to-be nil))

    (it "should return nil if `microconda' is found"
      (spy-on 'executable-find :and-call-fake (lambda (_) (when (equal _ "microconda") "/usr/bin/microconda")))
      (expect (pet-use-conda-p) :to-be nil))

    (it "should return nil if none of `conda' or `mamba' or `micromamba' is found"
      (spy-on 'executable-find)
      (expect (pet-use-conda-p) :to-be nil))))

(describe "pet-use-poetry-p"
  (describe "when the `pyproject.toml' file in the project declares `poetry' as the build system"
    (before-each
      (spy-on 'pet-pyproject :and-return-value '((build-system (build-backend . "poetry.core.masonry.api")))))

    (it "should return `poetry' path if `poetry' is found"
      (spy-on 'executable-find :and-return-value "/usr/bin/poetry")
      (expect (pet-use-poetry-p) :to-equal "/usr/bin/poetry"))

    (it "should return nil if `poetry' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-poetry-p) :to-be nil)))

  (describe "when the `pyproject.toml' file in the project does not declare `poetry' as the build system"
    (before-each
      (spy-on 'pet-pyproject :and-return-value '((build-system (build-backend . "pdm")))))

    (it "should return nil if `poetry' is found"
      (expect (pet-use-poetry-p) :to-be nil))

    (it "should return nil if `poetry' is not found"
      (spy-on 'executable-find :and-return-value "/usr/bin/poetry")
      (expect (pet-use-poetry-p) :to-be nil)))

  (describe "when the project does not have a `pyproject.toml' file"
    (before-each
      (spy-on 'pet-pyproject))

    (it "should return nil if `poetry' is found"
      (expect (pet-use-poetry-p) :to-be nil))

    (it "should return nil if `poetry' is not found"
      (spy-on 'executable-find :and-return-value "/usr/bin/poetry")
      (expect (pet-use-poetry-p) :to-be nil))))

(describe "pet-use-pyenv-p"
  (describe "when the project has a `.python-version' file"
    (before-each
      (spy-on 'pet-python-version :and-return-value t))

    (it "should return `pyenv' path if `pyenv' is found"
      (spy-on 'executable-find :and-return-value "/usr/bin/pyenv")
      (expect (pet-use-pyenv-p) :to-equal "/usr/bin/pyenv"))

    (it "should return `pyenv' path if `pyenv' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pyenv-p) :to-be nil)))

  (describe "when the project does not have a `.python-version' file"
    (before-each
      (spy-on 'pet-python-version))

    (it "should return nil if `pyenv' is found"
      (spy-on 'executable-find :and-return-value "/usr/bin/pyenv")
      (expect (pet-use-pyenv-p) :to-be nil))

    (it "should return nil if `pyenv' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pyenv-p) :to-be nil))))

(describe "pet-use-pipenv-p"
  (describe "when the project has a `Pipfile' file"
    (before-each
      (spy-on 'pet-pipfile :and-return-value t))

    (it "should return `pipenv' path if `pipenv' is found"
      (spy-on 'executable-find :and-return-value "/usr/bin/pipenv")
      (expect (pet-use-pipenv-p) :to-equal "/usr/bin/pipenv"))

    (it "should return nil path if `pipenv' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pipenv-p) :to-be nil)))

  (describe "when the project does not have a `Pipfile' file"
    (before-each
      (spy-on 'pet-pipfile))

    (it "should return nil path if `pipenv' is found"
      (spy-on 'executable-find :and-return-value "/usr/bin/pipenv")
      (expect (pet-use-pipenv-p) :to-be nil))

    (it "should return nil path if `pipenv' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pipenv-p) :to-be nil))))

(describe "pet-pre-commit-config-has-hook-p"
  :var ((pre-commit-config-content
          '((repos
              ((hooks ((id . "black"))) (repo . "https://github.com/psf/black") (rev . "22.6.0"))))))

  (before-each
    (spy-on 'pet-pre-commit-config :and-return-value pre-commit-config-content))

  (it "should return t if `.pre-commit-config.yaml' has hook declared"
    (expect (pet-pre-commit-config-has-hook-p "black") :to-be-truthy))

  (it "should return nil if `.pre-commit-config.yaml' does not have hook declared"
    (expect (pet-pre-commit-config-has-hook-p "isort") :not :to-be-truthy)))

(describe "pet-parse-pre-commit-db"
  (it "should parse `pre-commit' database to alist"
    (spy-on 'call-process :and-call-fake (lambda (&rest _) (insert "{\"foo\": 1}")))
    (expect (pet-parse-pre-commit-db "some.db") :to-equal '((foo . 1)))
    (expect 'call-process :to-have-been-called-with "sqlite3" nil t nil "-json" "some.db" "select * from repos")))

(describe "pet-pre-commit-virtualenv-path"
  (it "should return absolute path to the virtualenv of a `pre-commit' hook defined in a project"))

(describe "pet-executable-find"
  (it "should return the absolute path to the executable for a project using `pre-commit'")
  (it "should return the absolute path the executable for a project if its virtualenv is found")
  (it "should return the absolute path the executable for a project from `exec-path'"))

(describe "pet-virtualenv-root"
  :var ((project-root "/home/users/project/")
         (conda-path "/usr/bin/conda")
         (conda-virtualenv "/home/users/.conda/envs/project/")
         (poetry-path "/usr/bin/poetry")
         (poetry-virtualenv "/home/users/.cache/pypoetry/virtualenvs/project/")
         (pipenv-path "/usr/bin/pipenv")
         (pipenv-virtualenv "/home/users/.local/share/virtualenvs/project/")
         (venv-virtualenv "/home/users/project/.venv/")
         (pyenv-path "/usr/bin/pyenv")
         (pyenv-virtualenv "/home/users/.pyenv/versions/project/")
         (pyenv-virtualenv-truename "/home/users/.pyenv/versions/3.8/envs/project/"))

  (before-each
    (spy-on 'pet-project-root :and-return-value project-root)
    (setq pet-project-virtualenv-cache nil))

  (after-each
    (setq pet-project-virtualenv-cache nil))

  (it "should return the absolute path of the virtualenv for a project from `VIRTUAL_ENV'"
    (spy-on 'getenv :and-call-fake (lambda (name) (when (equal name "VIRTUAL_ENV") "/home/users/.venvs/project")))
    (expect (pet-virtualenv-root) :to-equal "/home/users/.venvs/project"))

  (it "should return the absolute path of the virtualenv for a project using `conda'"
    (spy-on 'pet-use-conda-p :and-return-value conda-path)
    (spy-on 'call-process :and-call-fake (lambda (&rest _) (insert (format "{\"active_prefix\": \"%s\"}" conda-virtualenv)) 0))
    (expect (pet-virtualenv-root) :to-equal conda-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal conda-virtualenv)
    (expect 'call-process :to-have-been-called-with conda-path nil t nil "info" "--envs" "--json"))

  (it "should return the absolute path of the virtualenv for a project using `poetry'"
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-poetry-p :and-return-value poetry-path)
    (spy-on 'call-process :and-call-fake (lambda (&rest _) (insert poetry-virtualenv) 0))
    (expect (pet-virtualenv-root) :to-equal poetry-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal poetry-virtualenv)
    (expect 'call-process :to-have-been-called-with poetry-path nil t nil "env" "info" "--no-ansi" "--path"))

  (it "should return the absolute path of the virtualenv for a project using `pipenv'"
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-poetry-p :and-return-value nil)
    (spy-on 'pet-use-pipenv-p :and-return-value pipenv-path)
    (spy-on 'call-process :and-call-fake (lambda (&rest _) (insert pipenv-virtualenv) 0))
    (expect (pet-virtualenv-root) :to-equal pipenv-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal pipenv-virtualenv)
    (expect 'call-process :to-have-been-called-with pipenv-path nil t nil "--venv"))

  (it "should return the absolute path of the `.venv' or `venv' directory in a project"
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-poetry-p :and-return-value nil)
    (spy-on 'pet-use-pipenv-p :and-return-value nil)
    (spy-on 'file-exists-p :and-call-fake (lambda (path) (equal path "/home/users/project/.venv")))
    (expect (pet-virtualenv-root) :to-equal venv-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal venv-virtualenv))

  (it "should return the absolute path of the virtualenv for a project using `pyenv'"
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-poetry-p :and-return-value nil)
    (spy-on 'pet-use-pipenv-p :and-return-value nil)
    (spy-on 'file-exists-p :and-return-value nil)
    (spy-on 'pet-use-pyenv-p :and-return-value pyenv-path)
    (spy-on 'call-process :and-call-fake (lambda (&rest _) (insert pyenv-virtualenv) 0))
    (spy-on 'file-truename :and-call-fake (lambda (name) (when (equal name pyenv-virtualenv) pyenv-virtualenv-truename)))
    (expect (pet-virtualenv-root) :to-equal pyenv-virtualenv-truename)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal pyenv-virtualenv-truename)
    (expect 'call-process :to-have-been-called-with pyenv-path nil t nil "prefix"))

  (it "should return the absolute path of the virtualenv for a project if the root is found in cache"
    (setq pet-project-virtualenv-cache `((,project-root . "/home/users/.venvs/env/")))
    (expect (pet-virtualenv-root) :to-equal "/home/users/.venvs/env/")))

(describe "pet-flycheck-python-pylint-find-pylintrc"
  :var ((old-default-directory default-directory)
         (home (getenv "HOME")))

  (before-all
    (setq-local default-directory "~/project/src/")
    (setenv "HOME" "/home/user/"))

  (after-all
    (setq-local default-directory old-default-directory)
    (setenv "HOME" home))

  (it "should not error when run inside a non-file buffer"
    (expect (with-temp-buffer (pet-flycheck-python-pylint-find-pylintrc)) :not :to-throw))

  (it "should return the absolute path to `pylintrc' from `default-directory'"
    (spy-on 'file-exists-p :and-call-fake (lambda (path) (equal path "~/project/src/.pylintrc")))
    (expect (pet-flycheck-python-pylint-find-pylintrc) :to-equal "/home/user/project/src/.pylintrc"))

  (it "should return the absolute path to `pylintrc' from a python package directory hierarchy"
    (spy-on 'buffer-file-name :and-return-value "/home/user/project/src/foo.py")
    (spy-on 'file-exists-p :and-call-fake (lambda (path) (equal path "/home/user/project/src/__init__.py")))
    (spy-on 'locate-dominating-file :and-call-fake (lambda (file name)
                                                     (when (equal name ".pylintrc")
                                                       "/home/user/project/src/")))
    (expect (pet-flycheck-python-pylint-find-pylintrc) :to-equal "/home/user/project/src/.pylintrc"))

  (it "should return the absolute path to `pylintrc' from `PYLINTRC'"
    (spy-on 'buffer-file-name :and-return-value "/home/user/project/src/foo.py")
    (spy-on 'file-exists-p :and-call-fake (lambda (path) (equal path "/home/user/project/pyproject.toml")))
    (spy-on 'getenv :and-call-fake (lambda (name)
                                     (when (equal name "PYLINTRC")
                                       "/home/user/project/pyproject.toml")))

    (expect (pet-flycheck-python-pylint-find-pylintrc) :to-equal "/home/user/project/pyproject.toml"))

  (it "should return the absolute path to `pylintrc' from `XDG_CONFIG_HOME'"
    (spy-on 'buffer-file-name :and-return-value "/home/user/project/src/foo.py")
    (spy-on 'file-exists-p :and-call-fake (lambda (path) (equal path "/home/user/.config/pylintrc")))
    (spy-on 'getenv :and-call-fake (lambda (name)
                                     (when (equal name "XDG_CONFIG_HOME")
                                       "/home/user/.config")))

    (expect (pet-flycheck-python-pylint-find-pylintrc) :to-equal "/home/user/.config/pylintrc"))

  (it "should return the absolute path to `pylintrc' from `HOME'"
    (spy-on 'file-exists-p :and-call-fake (lambda (path) (equal path "/home/user/.pylintrc")))
    (expect (pet-flycheck-python-pylint-find-pylintrc) :to-equal "/home/user/.pylintrc"))

  (it "should return the absolute path to `pylintrc' from `/etc'"
    (spy-on 'file-exists-p :and-return-value nil)
    (expect (pet-flycheck-python-pylint-find-pylintrc) :to-equal "/etc/pylintrc")))

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

(describe "pet-mode"
  (it "should set up all buffer local variables for supported packages if `pet-mode' is t")
  (it "should reset all buffer local variables for supported packages to default if `pet-mode' is nil"))

(describe "pet-cleanup-watchers-and-caches"
  (describe "when the last Python buffer for a project is killed"
    (it "should clear all watched files")
    (it "should clear all config file caches")
    (it "should clean `pet-project-virtualenv-cache'")))

;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
