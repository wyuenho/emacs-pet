;; -*- lisp-indent-offset: 2; lexical-binding: t; -*-

(unless (< emacs-major-version 27)
  (load-file "test/undercover-init.el"))

(require 'pet)

;; (setq pet-debug t)

(setq python-indent-guess-indent-offset nil)

(describe "pet-system-bin-dir"
  (describe "when called on Windows"
    (before-each
      (setq-local system-type 'windows-nt)
      ;; Github actions triggers some weird Emacs 26 code path that calls
      ;; `file-truename' after setting `system-type' and when `flycheck' is not
      ;; in the `load-path'. WTF indeed.
      (spy-on 'w32-long-file-name))

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
    (spy-on 'project-current :and-return-value (if (< emacs-major-version 29) (cons 'vc "/") '(vc Git "/")))
    (expect (pet-project-root) :to-equal "/"))

  (it "should return nil when Python file does not appear to be in a project"
    (spy-on 'projectile-project-root)
    (spy-on 'project-current)
    (expect (pet-project-root) :to-be nil)))

(describe "pet-find-file-from-project-root"
  (it "should find file from project root"
    (spy-on 'pet-project-root :and-return-value "/etc")
    (expect (pet-find-file-from-project-root "passwd") :to-equal "/etc/passwd"))

  (it "should return nil when file not found from project root"
    (spy-on 'pet-project-root :and-return-value "/etc")
    (expect (pet-find-file-from-project-root "idontexist") :to-be nil))

  (it "should return nil when not under a project"
    (spy-on 'pet-project-root)
    (expect (pet-find-file-from-project-root "foo") :to-be nil)))

(describe "pet-locate-dominating-file"
  :var ((old-default-directory default-directory)
         (home (getenv "HOME"))
         (process-environment (copy-sequence process-environment)))

  (before-each
    (setenv "HOME" "/home/user/")
    (setq-local default-directory "~/project/src/")
    (spy-on 'pet-project-root :and-return-value "/home/user/project/"))

  (after-each
    (setenv "HOME" home)
    (setq-local default-directory old-default-directory))

  (it "should find file from `default-directory'"
    (spy-on 'locate-dominating-file :and-return-value "~/project/src/package")
    (spy-on 'file-expand-wildcards :and-call-fake
      (lambda (pattern &optional full)
        (cond
          ((and full (equal pattern "/home/user/project/src/package/.pylintrc"))
            (list (expand-file-name pattern)))
          ((and (not full) (equal pattern "~/project/src/package/.pylintrc"))
            (list pattern)))))
    (expect (pet-locate-dominating-file ".pylintrc") :to-equal "/home/user/project/src/package/.pylintrc"))

  (it "should find file from parent directory"
    (spy-on 'locate-dominating-file :and-return-value "~/project/src/")
    (spy-on 'file-expand-wildcards :and-call-fake
      (lambda (pattern &optional full)
        (cond
          ((and full (equal pattern "/home/user/project/src/.pylintrc"))
            (list (expand-file-name pattern)))
          ((and (not full) (equal pattern "~/project/src/.pylintrc"))
            (list pattern)))))
    (expect (pet-locate-dominating-file ".pylintrc") :to-equal "/home/user/project/src/.pylintrc"))

  (it "should return `nil' if found file is outside of project root"
    (spy-on 'file-expand-wildcards :and-call-fake
      (lambda (dir &optional _)
        (when (equal dir "~/")
          (list "~/"))))
    (expect (pet-locate-dominating-file ".pylintrc") :to-be nil)))

(describe "pet-find-file-from-project-root-recursively"
  :var ((old-default-directory default-directory)
         (home (getenv "HOME"))
         (process-environment (copy-sequence process-environment)))

  (before-each
    (setenv "HOME" "/home/user/")
    (setq-local default-directory "~/project/src/")
    (spy-on 'pet-project-root :and-return-value "/home/user/project/"))

  (after-each
    (setenv "HOME" home)
    (setq-local default-directory old-default-directory))

  (describe "when using projectile"
    (it "should return the absolute path of the first file in a project that matches the file wildcard"
      (spy-on 'projectile-dir-files :and-return-value '("environment-dev.yaml"))
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-equal "/home/user/project/environment-dev.yaml"))

    (it "should return `nil' if the project is empty"
      (spy-on 'projectile-dir-files :and-return-value nil)
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-be nil))

    (it "should return `nil' if no file in the project matches the file wildcard"
      (spy-on 'projectile-dir-files :and-return-value '("setup.py"))
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-be nil)))

  (describe "when using project.el"
    :var ((projectile-dir-files-def (symbol-function 'projectile-dir-files)))

    (before-each
      (fset 'projectile-dir-files nil))

    (after-each
      (fset 'projectile-dir-files projectile-dir-files-def))

    (it "should return the absolute path of the first file in a project that matches the file wildcard"
      (spy-on 'project-files :and-return-value '("/home/user/project/environment-dev.yaml"))
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-equal "/home/user/project/environment-dev.yaml"))

    (it "should return `nil' if the project is empty"
      (spy-on 'project-files :and-return-value nil)
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-be nil))

    (it "should return `nil' if no file in the project matches the file wildcard"
      (spy-on 'project-files :and-return-value '("setup.py"))
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-be nil)))

  (describe "when using `directory-files-recursively'"
    :var ((projectile-dir-files-def (symbol-function 'projectile-dir-files))
           (project-files-def (symbol-function 'project-files)))

    (before-each
      (fset 'projectile-dir-files nil)
      (fset 'project-files nil))

    (after-each
      (fset 'projectile-dir-files projectile-dir-files-def)
      (fset 'project-files project-files-def))

    (it "should return the absolute path of the first file in a project that matches the file wildcard"
      (spy-on 'directory-files-recursively :and-return-value '("/home/user/project/environment-dev.yaml"))
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-equal "/home/user/project/environment-dev.yaml"))

    (it "should return `nil' if the project is empty"
      (spy-on 'directory-files-recursively :and-return-value nil)
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-be nil))

    (it "should return `nil' if no file in the project matches the file wildcard"
      (spy-on 'directory-files-recursively :and-return-value '("setup.py"))
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-be nil))))

(describe "pet-parse-json"
  (it "should parse a JSON string to an alist"
    (expect (pet-parse-json "{\"foo\":\"bar\",\"baz\":[\"buz\",1]}") :to-equal '((foo . "bar") (baz "buz" 1)))))

(describe "pet-parse-config-file"
  :var* ((yaml-content "foo: bar\nbaz:\n  - buz\n  - 1\n")
          (toml-content "foo = \"bar\"\nbaz = [\"buz\", 1]\n")
          (json-content "{\"foo\":\"bar\",\"baz\":[\"buz\",1]}")
          (yaml-file (make-temp-file "pet-test" nil ".yaml" yaml-content))
          (toml-file (make-temp-file "pet-test" nil ".toml" toml-content))
          (json-file (make-temp-file "pet-test" nil ".json" json-content))
          (toml-file-sans-ext (make-temp-file "pet-test" nil nil toml-content))
          (yaml-file-sans-ext (make-temp-file "pet-test" nil nil yaml-content))
          (json-file-sans-ext (make-temp-file "pet-test" nil nil json-content))
          (jsonian-file-sans-ext (make-temp-file "pet-test" nil nil json-content)))

  (after-all
    (delete-file yaml-file)
    (delete-file toml-file)
    (delete-file json-file)
    (delete-file toml-file-sans-ext)
    (delete-file yaml-file-san-ext)
    (delete-file json-file-san-ext))

  (before-each
    (setq-local pet-toml-to-json-program "tomljson")
    (setq-local pet-toml-to-json-program-arguments nil)
    (setq-local pet-yaml-to-json-program "yq")
    (setq-local pet-yaml-to-json-program-arguments '("--output-format" "json"))
    (make-local-variable 'auto-mode-alist)
    (add-to-list 'auto-mode-alist (cons (concat (file-name-nondirectory toml-file-sans-ext) "\\'") 'conf-toml-mode))
    (add-to-list 'auto-mode-alist (cons (concat (file-name-nondirectory yaml-file-sans-ext) "\\'") 'yaml-mode))
    (add-to-list 'auto-mode-alist (cons (concat (file-name-nondirectory json-file-sans-ext) "\\'") 'json-mode))
    (add-to-list 'auto-mode-alist (cons (concat (file-name-nondirectory jsonian-file-sans-ext) "\\'") 'jsonian-mode)))

  (after-each
    (kill-local-variable 'pet-toml-to-json-program)
    (kill-local-variable 'pet-toml-to-json-program-arguments)
    (kill-local-variable 'pet-yaml-to-json-program)
    (kill-local-variable 'pet-yaml-to-json-program-arguments)
    (kill-local-variable 'auto-mode-alist))

  (it "should parse a YAML file content to alist"
    (expect (pet-parse-config-file yaml-file) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a TOML file content to alist if the file name matches a key in `auto-mode-alist' and the value is `yaml-mode'"
    (expect (pet-parse-config-file yaml-file-sans-ext) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a TOML file content to alist"
    (expect (pet-parse-config-file toml-file) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a TOML file content to alist if the file name matches a key in `auto-mode-alist' and the value is `conf-toml-mode'"
    (expect (pet-parse-config-file toml-file-sans-ext) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a JSON file content to alist"
    (expect (pet-parse-config-file json-file) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a JSON file content to alist if the file name matches a key in `auto-mode-alist' and the value is `json-mode'"
    (expect (pet-parse-config-file json-file-sans-ext) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a JSON file content to alist if the file name matches a key in `auto-mode-alist' and the value is `jsonian-mode'"
    (expect (pet-parse-config-file jsonian-file-sans-ext) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
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
      (spy-on 'pet-find-file-from-project :and-return-value "/home/user/project/tox.ini")
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
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "conda") "/usr/bin/conda")))
      (expect (pet-use-conda-p) :to-equal "/usr/bin/conda"))

    (it "should return `mamba' path if `mamba' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "mamba") "/usr/bin/mamba")))
      (expect (pet-use-conda-p) :to-equal "/usr/bin/mamba"))

    (it "should return `micromamba' path if `micromamba' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "micromamba") "/usr/bin/micromamba")))
      (expect (pet-use-conda-p) :to-equal "/usr/bin/micromamba"))

    (it "should return nil if none of `conda' or `mamba' or `micromamba' is found"
      (spy-on 'executable-find)
      (expect (pet-use-conda-p) :to-be nil)))

  (describe "when the project does not have a `environment[a-zA-Z0-9-_].yaml' file"
    (before-each
      (spy-on 'pet-environment))

    (it "should return nil if `conda' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "conda") "/usr/bin/conda")))
      (expect (pet-use-conda-p) :to-be nil))

    (it "should return nil if `mamba' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "mamba") "/usr/bin/mamba")))
      (expect (pet-use-conda-p) :to-be nil))

    (it "should return nil if `microconda' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "microconda") "/usr/bin/microconda")))
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
  (before-all
    (call-process "sqlite3" nil nil nil "some.db" "CREATE TABLE repos (repo TEXT NOT NULL, ref TEXT NOT NULL, path TEXT NOT NULL, PRIMARY KEY (repo, ref)); INSERT INTO repos VALUES('https://github.com/pycqa/flake8','5.0.0','/home/user/project/flake8');"))

  (after-all
    (call-process "rm" nil nil nil "some.db"))

  (it "should parse `pre-commit' database to alist"
    (expect (pet-parse-pre-commit-db "some.db") :to-equal '(((repo . "https://github.com/pycqa/flake8")
                                                              (ref . "5.0.0")
                                                              (path . "/home/user/project/flake8"))))))

(describe "pet-pre-commit-virtualenv-path"
  :var ((pre-commit-db-content '(((repo . "https://github.com/pycqa/flake8:flake8-comprehensions==3.10.0,flake8-no-implicit-concat==0.3.3")
                                   (ref . "bd1656c")
                                   (path . "/home/user/.cache/pre-commit/repofurqd1rq"))
                                  ((repo . "https://github.com/psf/black")
                                    (ref . "22.6.0")
                                    (path . "/home/user/.cache/pre-commit/repo85no_p81"))))
         (pre-commit-config-content '((repos
                                        ((hooks
                                           ((additional_dependencies "flake8-no-implicit-concat==0.3.3" "flake8-comprehensions==3.10.0")
                                             (id . "flake8")))
                                          (repo . "https://github.com/pycqa/flake8")
                                          (rev . "bd1656c"))
                                        ((hooks
                                           ((id . "black")))
                                          (repo . "https://github.com/psf/black")
                                          (rev . "22.6.0")))))
         (old-default-directory default-directory)
         (home (getenv "HOME"))
         (orig-getenv (symbol-function 'getenv))
         (process-environment (copy-sequence process-environment)))

  (before-each
    (setenv "HOME" "/home/user/")
    (setq-local default-directory "~/project/src/")
    (spy-on 'getenv :and-call-fake
      (lambda (name)
        (unless (member name '("PRE_COMMIT_HOME" "XDG_CACHE_HOME"))
          (funcall orig-getenv name))))
    (spy-on 'pet-pre-commit-config :and-return-value pre-commit-config-content))

  (after-each
    (setenv "HOME" home)
    (setq-local default-directory old-default-directory))

  (describe "when `pre-commit' database content is not cached"
    (before-each
      (spy-on 'file-exists-p :and-call-fake (lambda (name) (equal name "/home/user/.cache/pre-commit/db.db")))
      (spy-on 'pet-watch-config-file)
      (spy-on 'pet-parse-pre-commit-db :and-return-value pre-commit-db-content))

    (it "should return absolute path to the virtualenv of a `pre-commit' hook with additional dependencies"
      (spy-on 'file-expand-wildcards :and-return-value '("/home/user/.cache/pre-commit/repofurqd1rq/py_env-python3.9"
                                                          "/home/user/.cache/pre-commit/repofurqd1rq/py_env-python3.10"))
      (expect (pet-pre-commit-virtualenv-path "flake8") :to-equal "/home/user/.cache/pre-commit/repofurqd1rq/py_env-python3.10"))

    (it "should return absolute path to the virtualenv of a `pre-commit' hook with no additional dependencies"
      (spy-on 'file-expand-wildcards :and-return-value '("/home/user/.cache/pre-commit/repo85no_p81/py_env-python3.9"))
      (expect (pet-pre-commit-virtualenv-path "black") :to-equal "/home/user/.cache/pre-commit/repo85no_p81/py_env-python3.9")))

  (describe "when `pre-commit' database content is cached"
    (before-each
      (setq-local pet-pre-commit-database-cache `(("/home/user/.cache/pre-commit/db.db" ,@pre-commit-db-content))))

    (after-each
      (kill-local-variable 'pet-pre-commit-database-cache))

    (it "should return absolute path to the virtualenv of a `pre-commit' hook with additional dependencies"
      (spy-on 'file-expand-wildcards :and-return-value '("/home/user/.cache/pre-commit/repofurqd1rq/py_env-python3.9"
                                                          "/home/user/.cache/pre-commit/repofurqd1rq/py_env-python3.10"))
      (expect (pet-pre-commit-virtualenv-path "flake8") :to-equal "/home/user/.cache/pre-commit/repofurqd1rq/py_env-python3.10"))

    (it "should return absolute path to the virtualenv of a `pre-commit' hook with no additional dependencies"
      (spy-on 'file-expand-wildcards :and-return-value '("/home/user/.cache/pre-commit/repo85no_p81/py_env-python3.9"))
      (expect (pet-pre-commit-virtualenv-path "black") :to-equal "/home/user/.cache/pre-commit/repo85no_p81/py_env-python3.9"))))

(describe "pet-executable-find"

  (describe "when using `pre-commit'"
    (before-each
      (spy-on 'pet-use-pre-commit-p :and-return-value "/usr/bin/pre-commit")
      (spy-on 'pet--executable-find :and-return-value nil))

    (it "should return the absolute path to the executable if hook and hook repo are found and the executable is found in hook repo"
      (spy-on 'pet-pre-commit-config-has-hook-p :and-return-value t)
      (spy-on 'pet-pre-commit-virtualenv-path :and-return-value "/home/user/.cache/pre-commit/repoblack")
      (spy-on 'file-exists-p :and-call-fake (lambda (path) (equal path "/home/user/.cache/pre-commit/repoblack/bin/black")))
      (expect (pet-executable-find "black") :to-equal "/home/user/.cache/pre-commit/repoblack/bin/black"))

    (it "should return nil if the hook is not found in config"
      (spy-on 'pet-pre-commit-config-has-hook-p :and-return-value nil)
      (expect (pet-executable-find "black") :to-be nil))

    (it "should return nil if the hook repo is not found"
      (spy-on 'pet-pre-commit-config-has-hook-p :and-return-value t)
      (spy-on 'pet-pre-commit-virtualenv-path :and-return-value nil)
      (expect (pet-executable-find "black") :to-be nil))

    (it "should return nil if the executable is not found in hook repo"
      (spy-on 'pet-pre-commit-config-has-hook-p :and-return-value t)
      (spy-on 'pet-pre-commit-virtualenv-path :and-return-value "/home/user/.cache/pre-commit/repoblack")
      (spy-on 'file-exists-p :and-call-fake (lambda (path) (not (equal path "/home/user/.cache/pre-commit/repoblack/bin/black"))))
      (expect (pet-executable-find "black") :to-be nil)))

  (it "should return the absolute path the executable for a project if its virtualenv is found"
    (spy-on 'pet-use-pre-commit-p :and-return-value nil)
    (spy-on 'pet-virtualenv-root :and-return-value "/home/user/project/.venv/")
    (spy-on 'executable-find :and-return-value "/home/user/project/.venv/bin/python")
    (expect (pet-executable-find "python") :to-equal "/home/user/project/.venv/bin/python"))

  (it "should return the absolute path of the result of `pyenv which EXECUTABLE' if no virtualenv is found but `pyenv' is in `exec-path'"
    (spy-on 'pet-use-pre-commit-p :and-return-value nil)
    (spy-on 'pet-virtualenv-root :and-return-value nil)
    (spy-on 'executable-find :and-call-fake (lambda (executable &optional _)
                                              (when (equal executable "pyenv")
                                                "/usr/bin/pyenv")))
    (spy-on 'process-lines :and-return-value '("/home/user/.pyenv/versions/3.10.5/bin/python"))
    (expect (pet-executable-find "python") :to-equal "/home/user/.pyenv/versions/3.10.5/bin/python")
    (expect 'process-lines :to-have-been-called-with "pyenv" "which" "python"))

  (it "should return the absolute path the executable for a project from `exec-path'"
    (spy-on 'pet-use-pre-commit-p :and-return-value nil)
    (spy-on 'pet-virtualenv-root :and-return-value nil)
    (spy-on 'executable-find :and-call-fake (lambda (executable &optional _)
                                              (when (equal executable "black")
                                                "/home/user/project/.venv/bin/black")))
    (expect (pet-executable-find "black") :to-equal "/home/user/project/.venv/bin/black")))

(describe "pet-virtualenv-root"
  :var ((project-root "/home/user/project/")
         (conda-path "/usr/bin/conda")
         (conda-env-dirs '("/home/user/miniforge3/envs" "/home/user/.conda/envs"))
         (conda-virtualenv "/home/user/miniforge3/envs/project/")
         (poetry-path "/usr/bin/poetry")
         (poetry-virtualenv "/home/user/.cache/pypoetry/virtualenvs/project/")
         (pipenv-path "/usr/bin/pipenv")
         (pipenv-virtualenv "/home/user/.local/share/virtualenvs/project/")
         (venv-virtualenv "/home/user/project/.venv/")
         (pyenv-path "/usr/bin/pyenv")
         (pyenv-virtualenv "/home/user/.pyenv/versions/project/")
         (pyenv-virtualenv-truename "/home/user/.pyenv/versions/3.8/envs/project/")
         (old-default-directory default-directory)
         (home (getenv "HOME"))
         (process-environment (copy-sequence process-environment)))

  (before-each
    (setenv "HOME" "/home/user/")
    (setq-local default-directory "/home/user/project")
    (spy-on 'pet-project-root :and-return-value project-root)
    (setq pet-project-virtualenv-cache nil))

  (after-each
    (setenv "HOME" home)
    (setq-local default-directory old-default-directory)
    (setq pet-project-virtualenv-cache nil))

  (it "should not cache nil when not in a project"
    (spy-on 'pet-project-root :and-return-value nil)
    (expect (pet-virtualenv-root) :to-be nil)
    (expect pet-project-virtualenv-cache :to-be nil))

  (it "should return the absolute path of the virtualenv for a project from `VIRTUAL_ENV'"
    (spy-on 'getenv :and-call-fake (lambda (name) (when (equal name "VIRTUAL_ENV") "/home/user/.venvs/project")))
    (expect (pet-virtualenv-root) :to-equal "/home/user/.venvs/project"))

  (it "should return the absolute path of the virtualenv for a project using `conda'"
    (spy-on 'pet-use-conda-p :and-return-value conda-path)
    (spy-on 'pet-environment-path :and-return-value "/home/user/project/environment.yml")
    (spy-on 'call-process :and-call-fake
      (lambda (&rest _)
        (insert
          (format "{\"envs_dirs\": [%s]}"
            (string-join
              (seq-map
                (lambda (dir)
                  (format "\"%s\"" dir))
                conda-env-dirs)
              ",")))
        0))
    (spy-on 'pet-environment :and-return-value `((name . "project")))
    (spy-on 'file-directory-p :and-call-fake (lambda (filename) (equal filename conda-virtualenv)))
    (expect (pet-virtualenv-root) :to-equal conda-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal conda-virtualenv)
    (expect 'call-process :to-have-been-called-with conda-path nil t nil "info" "--json"))

  (it "should return the absolute path of the virtualenv for a project using `poetry'"
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-poetry-p :and-return-value poetry-path)
    (spy-on 'pet-pyproject-path :and-return-value "/home/user/project/pyproject.toml")
    (spy-on 'call-process :and-call-fake (lambda (&rest _) (insert poetry-virtualenv) 0))
    (expect (pet-virtualenv-root) :to-equal poetry-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal poetry-virtualenv)
    (expect 'call-process :to-have-been-called-with poetry-path nil t nil "env" "info" "--no-ansi" "--path"))

  (it "should return the absolute path of the virtualenv for a project using `pipenv'"
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-poetry-p :and-return-value nil)
    (spy-on 'pet-use-pipenv-p :and-return-value pipenv-path)
    (spy-on 'pet-pipfile-path :and-return-value "/home/user/project/Pipfile")
    (spy-on 'call-process :and-call-fake (lambda (&rest _) (insert pipenv-virtualenv) 0))
    (expect (pet-virtualenv-root) :to-equal pipenv-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal pipenv-virtualenv)
    (expect 'call-process :to-have-been-called-with pipenv-path nil '(t nil) nil "--quiet" "--venv"))

  (it "should return the absolute path of the `.venv' or `venv' directory in a project"
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-poetry-p :and-return-value nil)
    (spy-on 'pet-use-pipenv-p :and-return-value nil)
    (spy-on 'locate-dominating-file :and-return-value project-root)
    (expect (pet-virtualenv-root) :to-equal venv-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal venv-virtualenv))

  (it "should return the absolute path of the virtualenv for a project using `pyenv'"
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-poetry-p :and-return-value nil)
    (spy-on 'pet-use-pipenv-p :and-return-value nil)
    (spy-on 'locate-dominating-file :and-return-value nil)
    (spy-on 'pet-use-pyenv-p :and-return-value pyenv-path)
    (spy-on 'pet-python-version-path :and-return-value "/home/user/project/.python-version")
    (spy-on 'call-process :and-call-fake (lambda (&rest _) (insert pyenv-virtualenv) 0))
    (spy-on 'file-truename :and-call-fake (lambda (name) (when (equal name pyenv-virtualenv) pyenv-virtualenv-truename)))
    (expect (pet-virtualenv-root) :to-equal pyenv-virtualenv-truename)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal pyenv-virtualenv-truename)
    (expect 'call-process :to-have-been-called-with pyenv-path nil t nil "prefix"))

  (it "should return the absolute path of the virtualenv for a project if the root is found in cache"
    (setq pet-project-virtualenv-cache `((,project-root . "/home/user/.venvs/env/")))
    (expect (pet-virtualenv-root) :to-equal "/home/user/.venvs/env/")))

(describe "pet-flycheck-python-pylint-find-pylintrc"
  :var ((old-default-directory default-directory)
         (home (getenv "HOME"))
         (orig-getenv (symbol-function 'getenv))
         (process-environment (copy-sequence process-environment)))

  (before-each
    (setenv "HOME" "/home/user/")
    (setq-local default-directory "~/project/src/"))

  (after-each
    (setenv "HOME" home)
    (setq-local default-directory old-default-directory))

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
                                     (if (equal name "PYLINTRC")
                                       "/home/user/project/pyproject.toml"
                                       (funcall orig-getenv name))))

    (expect (pet-flycheck-python-pylint-find-pylintrc) :to-equal "/home/user/project/pyproject.toml"))

  (it "should return the absolute path to `pylintrc' from `XDG_CONFIG_HOME'"
    (spy-on 'buffer-file-name :and-return-value "/home/user/project/src/foo.py")
    (spy-on 'file-exists-p :and-call-fake (lambda (path) (equal path "/home/user/.config/pylintrc")))
    (spy-on 'getenv :and-call-fake (lambda (name)
                                     (if (equal name "XDG_CONFIG_HOME")
                                       "/home/user/.config"
                                       (funcall orig-getenv name))))

    (expect (pet-flycheck-python-pylint-find-pylintrc) :to-equal "/home/user/.config/pylintrc"))

  (it "should return the absolute path to `pylintrc' from `HOME'"
    (spy-on 'file-exists-p :and-call-fake (lambda (path) (equal path "/home/user/.pylintrc")))
    (expect (pet-flycheck-python-pylint-find-pylintrc) :to-equal "/home/user/.pylintrc"))

  (it "should return the absolute path to `pylintrc' from `/etc'"
    (spy-on 'file-exists-p :and-return-value nil)
    (expect (pet-flycheck-python-pylint-find-pylintrc) :to-equal "/etc/pylintrc")))

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
    (unintern 'flycheck-mode))

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
    (expect flycheck-python-pycompile-executable :to-equal python-shell-interpreter)
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

(describe "pet-flycheck-setup"
  :var ((old-default-directory default-directory)
         (home (getenv "HOME"))
         (process-environment (copy-sequence process-environment)))

  (before-each
    (setenv "HOME" "/home/user/")
    (setq-local default-directory "/home/user/")
    (defun flycheck-checker-get (checker property)))

  (after-each
    (setenv "HOME" home)
    (setq-local default-directory old-default-directory)
    (fmakunbound 'flycheck-checker-get)
    (unintern 'flycheck-checker-get))

  (it "should add `pet-flycheck-toggle-local-vars' to `flycheck-mode-hook'"
    (pet-flycheck-setup)
    (expect (member 'pet-flycheck-toggle-local-vars flycheck-mode-hook) :to-be-truthy))

  (it "should advice `flycheck-python-find-project-root'"
    (pet-flycheck-setup)
    (expect
      (advice-member-p 'pet-flycheck-python-find-project-root-advice 'flycheck-python-find-project-root)
      :to-be-truthy)))

(describe "pet-flycheck-teardown"
  (before-each
    (pet-flycheck-setup)
    (pet-flycheck-teardown)
    (defun flycheck-checker-get (checker property)))

  (after-each
    (fmakunbound 'flycheck-checker-get)
    (unintern 'flycheck-checker-get))

  (it "should remove advice on `flycheck-python-find-project-root'"
    (expect
      (advice-member-p 'pet-flycheck-python-find-project-root-advice 'flycheck-python-find-project-root)
      :not :to-be-truthy))

  (it "should remove `pet-flycheck-toggle-local-vars' from `flycheck-mode-hook'"
    (expect (member 'pet-flycheck-toggle-local-vars flycheck-mode-hook) :not :to-be-truthy))

  (it "should reset `flycheck' Python checkers variables to default"
    (expect (local-variable-p 'flycheck-pylintrc) :not :to-be-truthy)
    (expect (local-variable-p 'flycheck-python-flake8-executable) :not :to-be-truthy)
    (expect (local-variable-p 'flycheck-python-pylint-executable) :not :to-be-truthy)
    (expect (local-variable-p 'flycheck-python-mypy-executable) :not :to-be-truthy)
    (expect (local-variable-p 'flycheck-python-pyright-executable) :not :to-be-truthy)
    (expect (local-variable-p 'flycheck-python-pycompile-executable) :not :to-be-truthy)
    (expect (local-variable-p 'flycheck-python-ruff-executable) :not :to-be-truthy)))

(describe "pet-eglot--executable-find"
  (it "should delegate to `pet-executable-find' for Python LSP servers"
    (spy-on 'executable-find :and-call-fake (lambda (&rest args) args))
    (spy-on 'pet-executable-find :and-call-fake 'identity)

    (expect (pet-eglot--executable-find "pylsp") :to-equal "pylsp")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "pylsp")
    (expect 'executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find "pyls") :to-equal "pyls")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "pyls")
    (expect 'executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find "pyright-langserver") :to-equal "pyright-langserver")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "pyright-langserver")
    (expect 'executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find "jedi-language-server") :to-equal "jedi-language-server")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "jedi-language-server")
    (expect 'executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find "ruff-lsp") :to-equal "ruff-lsp")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "ruff-lsp")
    (expect 'executable-find :not :to-have-been-called)

    (if (>= emacs-major-version 27)
      (progn
        (expect (pet-eglot--executable-find "sh") :to-equal '("sh" nil))
        (expect 'executable-find :to-have-been-called-with "sh" nil))
      (expect (pet-eglot--executable-find "sh") :to-equal '("sh"))
      (expect 'executable-find :to-have-been-called-with "sh"))))

(describe "pet-eglot--workspace-configuration-plist-advice"
  (before-each
    (spy-on 'jsonrpc--process))

  (it "should pass canonicalized PATH to FN if it's a directory"
    (spy-on 'mock-eglot--workspace-configuration-plist)
    (spy-on 'process-command :and-return-value '("/usr/bin/jedi-language-server"))
    (spy-on 'file-directory-p :and-return-value t)

    (pet-eglot--workspace-configuration-plist-advice
      'mock-eglot--workspace-configuration-plist
      "server" "/home/users/project")

    (expect 'mock-eglot--workspace-configuration-plist :to-have-been-called-with "server" "/home/users/project/"))

  (it "should pass PATH to FN directly if it's a not directory"
    (spy-on 'mock-eglot--workspace-configuration-plist)
    (spy-on 'process-command :and-return-value '("/usr/bin/jedi-language-server"))
    (spy-on 'file-directory-p :and-return-value nil)

    (pet-eglot--workspace-configuration-plist-advice
      'mock-eglot--workspace-configuration-plist
      "server" "/home/users/project/file")

    (expect 'mock-eglot--workspace-configuration-plist :to-have-been-called-with "server" "/home/users/project/file"))

  (it "should return `nil' when no dir local variables and pet server initialization options"
    (spy-on 'mock-eglot--workspace-configuration-plist)
    (spy-on 'process-command :and-return-value '("/usr/bin/some-lsp-server"))

    (expect (pet-eglot--workspace-configuration-plist-advice
              'mock-eglot--workspace-configuration-plist
              "server")
      :not :to-be-truthy)

    (expect 'mock-eglot--workspace-configuration-plist :to-have-been-called))

  (it "should return pet server initialization options when no dir local variables"
    (spy-on 'mock-eglot--workspace-configuration-plist)
    (spy-on 'process-command :and-return-value '("/usr/bin/pyright-langserver"))
    (spy-on 'pet-lookup-eglot-server-initialization-options
      :and-return-value
      '(:python
         (:pythonPath
           "/usr/bin/python"
           :venvPath
           "/home/user/project/")))

    (expect (pet-eglot--workspace-configuration-plist-advice
              'mock-eglot--workspace-configuration-plist
              "server")
      :to-equal '(:python
                   (:pythonPath
                     "/usr/bin/python"
                     :venvPath
                     "/home/user/project/")))

    (expect 'mock-eglot--workspace-configuration-plist :to-have-been-called))

  (it "should return dir local variables when pet server initialization options"
    (spy-on 'mock-eglot--workspace-configuration-plist
      :and-return-value
      '(:python
         (:pythonPath
           "/usr/bin/python"
           :venvPath
           "/home/user/project/")))
    (spy-on 'process-command :and-return-value '("/usr/bin/pyright-langserver"))
    (spy-on 'pet-lookup-eglot-server-initialization-options)

    (expect (pet-eglot--workspace-configuration-plist-advice
              'mock-eglot--workspace-configuration-plist
              "server")
      :to-equal '(:python
                   (:pythonPath
                     "/usr/bin/python"
                     :venvPath
                     "/home/user/project/")))

    (expect 'mock-eglot--workspace-configuration-plist :to-have-been-called))

  (it "should return dir local variables and pet server initialization options when both available"
    (spy-on 'mock-eglot--workspace-configuration-plist
      :and-return-value
      '(:python
         (:pythonPath
           "/usr/bin/python")))
    (spy-on 'process-command :and-return-value '("/usr/bin/pyright-langserver"))
    (spy-on 'pet-lookup-eglot-server-initialization-options
      :and-return-value
      '(:python
         (:venvPath
           "/home/user/project/")))

    (expect (pet-eglot--workspace-configuration-plist-advice
              'mock-eglot--workspace-configuration-plist
              "server")
      :to-equal '(:python
                   (:pythonPath
                     "/usr/bin/python"
                     :venvPath
                     "/home/user/project/")))

    (expect 'mock-eglot--workspace-configuration-plist :to-have-been-called)))

(describe "pet-eglot--guess-contact-advice"
  (it "should return output unchanged when there's no pet server initialization"
    (spy-on 'eglot--guess-contact :and-return-value
      '((python-mode python-ts-mode)
         "project"
         'eglot-lsp-server
         ("pyright-langserver" "--stdio")
         "python-ts"))

    (spy-on 'pet-lookup-eglot-server-initialization-options)

    (expect
      (pet-eglot--guess-contact-advice 'eglot--guess-contact)
      :to-equal
      '((python-mode python-ts-mode)
         "project"
         'eglot-lsp-server
         ("pyright-langserver" "--stdio")
         "python-ts")))

  (it "should return contact with default server initialization when there's no pet server initialization"
    (spy-on 'eglot--guess-contact :and-return-value
      '((python-mode python-ts-mode)
         "project"
         'eglot-lsp-server
         ("jedi-language-server"
           :initializationOptions
           (:jedi
             (:executable
               (:args ["--ws"]))))
         "python-ts"))

    (spy-on 'pet-lookup-eglot-server-initialization-options :and-return-value
      '(:jedi
         (:executable
           (:command
             "/home/user/project/env/bin/jedi-language-server")
           :workspace
           (:environmentPath
             "/home/user/project/env/bin/python"))))

    (expect
      (pet-eglot--guess-contact-advice 'eglot--guess-contact)
      :to-equal
      '((python-mode python-ts-mode)
         "project"
         'eglot-lsp-server
         ("jedi-language-server"
           :initializationOptions
           (:jedi
             (:executable
               (:args
                 ["--ws"]
                 :command
                 "/home/user/project/env/bin/jedi-language-server")
               :workspace
               (:environmentPath
                 "/home/user/project/env/bin/python"))))
         "python-ts"))))

(describe "pet-lookup-eglot-server-initialization-options"
  (before-each
    (spy-on 'pet-virtualenv-root :and-return-value "/home/user/project/")
    (spy-on 'pet-executable-find :and-call-fake
      (lambda (command)
        (assoc-default command
          '(("flake8"                . "/usr/bin/flake8")
             ("pylint"               . "/usr/bin/pylint")
             ("python"               . "/usr/bin/python")
             ("jedi-language-server" . "/home/user/.local/bin/jedi-language-server")
             ("ruff"                 . "/usr/bin/ruff"))))))

  (it "should return eglot initialization options for pylsp"
    (expect (pet-lookup-eglot-server-initialization-options "/home/user/.local/bin/pylsp") :to-equal
      '(:pylsp
         (:plugins
           (:jedi
             (:environment
               "/home/user/project/")
             :ruff
             (:executable
               "/usr/bin/ruff")
             :pylsp_mypy
             (:overrides
               ["--python-executable" "/usr/bin/python" t])
             :flake8
             (:executable
               "/usr/bin/flake8")
             :pylint
             (:executable
               "/usr/bin/pylint"))))))

  (it "should return eglot initialization options for pyls"
    (expect (pet-lookup-eglot-server-initialization-options "/home/user/.local/bin/pyls") :to-equal
      '(:pyls
         (:plugins
           (:jedi
             (:environment
               "/home/user/project/")
             :pylint
             (:executable
               "/usr/bin/pylint"))))))

  (it "should return eglot initialization options for pyright"
    (expect (pet-lookup-eglot-server-initialization-options "/home/user/.local/bin/pyright-langserver") :to-equal
      `(:python
         (:pythonPath
           "/usr/bin/python"
           :venvPath
           "/home/user/project/"))))

  (it "should return eglot initialization options for jedi-language-server"
    (expect (pet-lookup-eglot-server-initialization-options "jedi-language-server") :to-equal
      '(:jedi
         (:executable
           (:command
             "/home/user/.local/bin/jedi-language-server")
           :workspace
           (:environmentPath
             "/usr/bin/python")))))

  (it "should return eglot initialization options for ruff-lsp"
    (expect (pet-lookup-eglot-server-initialization-options "ruff-lsp") :to-equal
      '(:settings
         (:interpreter
           "/usr/bin/python"
           :path
           "/usr/bin/ruff")))))

(describe "pet-merge-eglot-initialization-options"
  (it "should deeply merge 2 plists"
    (expect
      (pet-merge-eglot-initialization-options
        '(:a (:b [1 2] :c 0 :d "hello" :f :json-null))
        '(:a (:b [3 4] :c 9 :e "world" :g :json-false)))
      :to-equal
      '(:a (:b [1 2 3 4] :c 9 :d "hello" :f :json-null :e "world" :g :json-false)))))

(describe "pet-eglot-setup"
  (before-each
    (pet-eglot-setup))

  (after-each
    (pet-eglot-teardown))

  (it "should advice eglot functions"
    (pet-eglot-setup)
    (expect (advice-member-p 'pet-eglot--workspace-configuration-plist-advice 'eglot--workspace-configuration-plist) :to-be-truthy)
    (expect (advice-member-p 'pet-eglot-alternatives-advice 'eglot-alternatives) :to-be-truthy)
    (expect (advice-member-p 'pet-eglot--guess-contact-advice 'eglot--guess-contact) :to-be-truthy)))

(describe "pet-eglot-teardown"
  (it "should remove `pet' advices from eglot functions"
    (pet-eglot-setup)
    (pet-eglot-teardown)
    (expect (advice-member-p 'pet-eglot--workspace-configuration-plist-advice 'eglot--workspace-configuration-plist) :to-be nil)
    (expect (advice-member-p 'pet-eglot-alternatives-advice 'eglot-alternatives) :to-be nil)
    (expect (advice-member-p 'pet-eglot--guess-contact-advice 'eglot--guess-contact) :to-be nil)))

(describe "pet-dape-setup"
  (before-each
    (spy-on 'pet-find-file-from-project-root-recursively)
    (spy-on 'pet-executable-find :and-return-value "/usr/bin/python"))

  (it "should set up buffer local variable dape-command when no __main__.py is found"
    (pet-dape-setup)
    (expect (local-variable-p 'dape-command) :to-be-truthy)
    (expect dape-command :to-equal '(debugpy command "/usr/bin/python")))

  (it "should set up buffer local variable dape-command when a __main__.py is found"
    (spy-on 'pet-find-file-from-project-root-recursively :and-return-value "/home/user/project/src/foo/bar/__main__.py")
    (spy-on 'file-exists-p :and-call-fake
      (lambda (path)
        (member path
          '("/home/user/project/src/foo/bar/__init__.py" "/home/user/project/src/foo/__init__.py"))))
    (pet-dape-setup)
    (expect (local-variable-p 'dape-command) :to-be-truthy)
    (expect dape-command :to-equal '(debugpy-module command "/usr/bin/python" :module "foo.bar")))

  (it "should set up buffer local variable dape-cwd-function"
    (pet-dape-setup)
    (expect (local-variable-p 'dape-cwd-function) :to-be-truthy)
    (expect dape-cwd-function :to-equal 'pet-project-root)))

(describe "pet-dape-teardown"
  (it "should tear down bufer local variables for dape"
    (spy-on 'pet-find-file-from-project-root-recursively)
    (spy-on 'pet-executable-find :and-return-value "/usr/bin/python")
    (pet-dape-setup)
    (pet-dape-teardown)
    (expect (local-variable-p 'dape-command) :not :to-be-truthy)
    (expect (local-variable-p 'dape-cwd-function) :not :to-be-truthy)))

(describe "pet-buffer-local-vars-setup"
  (after-each
    (kill-local-variable 'python-shell-interpreter)
    (kill-local-variable 'python-shell-virtualenv-root)
    (kill-local-variable 'lsp-jedi-executable-command)
    (kill-local-variable 'lsp-pyls-plugins-jedi-environment)
    (kill-local-variable 'lsp-pylsp-plugins-jedi-environment)
    (kill-local-variable 'lsp-pyright-venv-path)
    (kill-local-variable 'lsp-pyright-python-executable-cmd)
    (kill-local-variable 'lsp-ruff-server-command)
    (kill-local-variable 'lsp-ruff-python-path)
    (kill-local-variable 'dap-python-executable)
    (kill-local-variable 'dap-variables-project-root-function)
    (kill-local-variable 'python-pytest-executable)
    (kill-local-variable 'python-black-command)
    (kill-local-variable 'python-isort-command)
    (kill-local-variable 'blacken-executable)
    (kill-local-variable 'yapfify-executable)
    (kill-local-variable 'ruff-format-command)
    (kill-local-variable 'py-autopep8-command))

  (it "should set up all buffer local variables for supported packages"
    (spy-on 'pet-executable-find :and-call-fake
      (lambda (exec)
        (pcase exec
          ("python"
            "/usr/bin/python")
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
            "/usr/bin/autopep8"))))
    (spy-on 'pet-virtualenv-root :and-return-value "/home/user/project/.venv/")
    (spy-on 'pet-flycheck-setup)
    (spy-on 'pet-eglot-setup)
    (spy-on 'pet-dape-setup)

    (pet-buffer-local-vars-setup)

    (expect 'pet-flycheck-setup :to-have-been-called)
    (expect 'pet-eglot-setup :to-have-been-called)
    (expect 'pet-dape-setup :to-have-been-called)

    (expect (local-variable-p 'python-shell-interpreter) :to-be-truthy)
    (expect (local-variable-p 'python-shell-virtualenv-root) :to-be-truthy)
    (expect (local-variable-p 'lsp-jedi-executable-command) :to-be-truthy)
    (expect (local-variable-p 'lsp-pyls-plugins-jedi-environment) :to-be-truthy)
    (expect (local-variable-p 'lsp-pylsp-plugins-jedi-environment) :to-be-truthy)
    (expect (local-variable-p 'lsp-pyright-venv-path) :to-be-truthy)
    (expect (local-variable-p 'lsp-pyright-python-executable-cmd) :to-be-truthy)
    (expect (local-variable-p 'lsp-ruff-server-command) :to-be-truthy)
    (expect (local-variable-p 'lsp-ruff-python-path) :to-be-truthy)
    (expect (local-variable-p 'dap-python-executable) :to-be-truthy)
    (expect (local-variable-p 'dap-variables-project-root-function) :to-be-truthy)
    (expect (local-variable-p 'python-pytest-executable) :to-be-truthy)
    (expect (local-variable-p 'python-black-command) :to-be-truthy)
    (expect (local-variable-p 'python-isort-command) :to-be-truthy)
    (expect (local-variable-p 'blacken-executable) :to-be-truthy)
    (expect (local-variable-p 'yapfify-executable) :to-be-truthy)
    (expect (local-variable-p 'ruff-format-command) :to-be-truthy)
    (expect (local-variable-p 'py-autopep8-command) :to-be-truthy)

    (expect python-shell-interpreter :to-equal "/usr/bin/python")
    (expect python-shell-virtualenv-root :to-equal "/home/user/project/.venv/")
    (expect lsp-jedi-executable-command :to-equal "/usr/bin/jedi-language-server")
    (expect lsp-pyls-plugins-jedi-environment :to-equal "/home/user/project/.venv/")
    (expect lsp-pylsp-plugins-jedi-environment :to-equal "/home/user/project/.venv/")
    (expect lsp-pyright-venv-path :to-equal "/home/user/project/.venv/")
    (expect lsp-pyright-python-executable-cmd :to-equal "/usr/bin/python")
    (expect lsp-ruff-server-command :to-equal '("/usr/bin/ruff" "server"))
    (expect lsp-ruff-python-path :to-equal "/usr/bin/python")
    (expect dap-python-executable :to-equal "/usr/bin/python")
    (expect dap-variables-project-root-function :to-equal #'pet-project-root)
    (expect python-pytest-executable :to-equal "/usr/bin/pytest")
    (expect python-black-command :to-equal "/usr/bin/black")
    (expect python-isort-command :to-equal "/usr/bin/isort")
    (expect blacken-executable :to-equal "/usr/bin/black")
    (expect yapfify-executable :to-equal "/usr/bin/yapf")
    (expect ruff-format-command :to-equal "/usr/bin/ruff")
    (expect yapfify-executable :to-equal "/usr/bin/yapf")
    (expect py-autopep8-command :to-equal "/usr/bin/autopep8")))

(describe "pet-buffer-local-vars-teardown"
  (after-each
    (kill-local-variable 'python-shell-interpreter)
    (kill-local-variable 'python-shell-virtualenv-root)
    (kill-local-variable 'lsp-jedi-executable-command)
    (kill-local-variable 'lsp-pyls-plugins-jedi-environment)
    (kill-local-variable 'lsp-pylsp-plugins-jedi-environment)
    (kill-local-variable 'lsp-pyright-venv-path)
    (kill-local-variable 'lsp-pyright-python-executable-cmd)
    (kill-local-variable 'lsp-ruff-server-command)
    (kill-local-variable 'lsp-ruff-python-path)
    (kill-local-variable 'dap-python-executable)
    (kill-local-variable 'dap-variables-project-root-function)
    (kill-local-variable 'python-pytest-executable)
    (kill-local-variable 'python-black-command)
    (kill-local-variable 'python-isort-command)
    (kill-local-variable 'blacken-executable)
    (kill-local-variable 'yapfify-executable)
    (kill-local-variable 'ruff-format-command)
    (kill-local-variable 'py-autopep8-command))

  (it "should reset all buffer local variables for supported packages to default"
    (spy-on 'pet-flycheck-teardown)
    (spy-on 'pet-eglot-teardown)
    (spy-on 'pet-dape-teardown)

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
    (expect (local-variable-p 'lsp-pyright-venv-path) :not :to-be-truthy)
    (expect (local-variable-p 'lsp-pyright-python-executable-cmd) :not :to-be-truthy)
    (expect (local-variable-p 'lsp-ruff-server-command) :not :to-be-truthy)
    (expect (local-variable-p 'lsp-ruff-python-path) :not :to-be-truthy)
    (expect (local-variable-p 'dap-python-executable) :not :to-be-truthy)
    (expect (local-variable-p 'dap-variables-project-root-function) :not :to-be-truthy)
    (expect (local-variable-p 'python-pytest-executable) :not :to-be-truthy)
    (expect (local-variable-p 'python-black-command) :not :to-be-truthy)
    (expect (local-variable-p 'python-isort-command) :not :to-be-truthy)
    (expect (local-variable-p 'blacken-executable) :not :to-be-truthy)
    (expect (local-variable-p 'yapfify-executable) :not :to-be-truthy)
    (expect (local-variable-p 'ruff-format-command) :not :to-be-truthy)
    (expect (local-variable-p 'py-autopep8-command) :not :to-be-truthy)))

(describe "pet-verify-setup"
  :var ((old-default-directory default-directory)
         (home (getenv "HOME"))
         (orig-getenv (symbol-function 'getenv))
         (process-environment (copy-sequence process-environment)))

  (before-each
    (setenv "HOME" "/home/user/")
    (setq-local default-directory "~/project/"))

  (after-each
    (setenv "HOME" home)
    (setq-local default-directory old-default-directory))

  (it "should error when not in python mode"
    (expect (pet-verify-setup) :to-throw 'user-error))

  (it "should display unbound values"
    (with-temp-buffer
      (python-mode)
      (pet-verify-setup)
      (expect
        (with-current-buffer "*pet info*"
          (re-search-forward "lsp-jedi-executable-command:\s+\\(.+\\)")
          (match-string 1))
        :to-equal "unbound")))

  (it "should display bound values"
    (with-temp-buffer
      (python-mode)
      (pet-verify-setup)
      (expect
        (with-current-buffer "*pet info*"
          (re-search-forward "python-shell-interpreter:\s+\\(.+\\)")
          (match-string 1))
        :to-equal (if (< emacs-major-version 28) "python" "python3"))))

  (it "should display list as comma-separated values"
    (spy-on 'pet-flycheck-python-pylint-find-pylintrc)
    (spy-on 'pet-executable-find)
    (spy-on 'getenv :and-call-fake (lambda (name)
                                     (unless (equal name "XDG_CONFIG_HOME")
                                       (funcall orig-getenv name))))
    (with-temp-buffer
      (python-mode)
      (setq-local flycheck-mode t)
      (pet-flycheck-toggle-local-vars)
      (pet-verify-setup)
      (expect
        (split-string (with-current-buffer "*pet info*"
                        (re-search-forward "flycheck-python-mypy-config:\s+\\(.+\\)")
                        (match-string 1))
          "," t split-string-default-separators)
        :to-have-same-items-as '("mypy.ini" ".mypy.ini" "pyproject.toml" "setup.cfg" "/home/user/.config/mypy/config" "/home/user/.mypy.ini")))))

(describe "pet-mode"
  (it "should set up all buffer local variables for supported packages if `pet-mode' is t"
    (spy-on 'pet-buffer-local-vars-setup)
    (pet-mode 1)
    (expect 'pet-buffer-local-vars-setup :to-have-been-called))

  (it "should reset all buffer local variables for supported packages to default if `pet-mode' is nil"
    (spy-on 'pet-buffer-local-vars-teardown)
    (pet-mode -1)
    (expect 'pet-buffer-local-vars-teardown :to-have-been-called)))

(describe "pet-cleanup-watchers-and-caches"
  :var ((last-project-buf-file-name "/home/user/project/src/__init__.py")
         (buffers (mapcar
                    (lambda (file)
                      (with-current-buffer (create-file-buffer file)
                        (set-visited-file-name file)
                        (python-mode)
                        (set-buffer-modified-p nil)
                        (current-buffer)))
                    '("/home/user/foo.py" "/home/user/project2/src/bar.py"))))

  (after-all
    (dolist (buf buffers)
      (kill-buffer buf)))

  (before-each
    (setq pet-project-virtualenv-cache '(("/home/user/project/" . "/home/user/project/.venv/")))
    (setq pet-pre-commit-config-cache '(("/home/user/project/.pre-commit-config.yaml" . "whatever")))
    (setq pet-pyproject-cache '(("/home/user/project/pyproject.toml" . "whatever")))
    (setq pet-pipfile-cache '(("/home/user/project/Pipfile" . "whatever")))
    (setq pet-python-version-cache '(("/home/user/project/.python-verion" . "whatever")))
    (setq pet-environment-cache '(("/home/user/project/environment.yml" . "whatever")))
    (setq pet-watched-config-files '(("/home/user/.cache/pre-commit/db.db" . 1)
                                      ("/home/user/project/.pre-commit-config.yaml" . 2)
                                      ("/home/user/project/pyproject.toml" . 3)
                                      ("/home/user/project/Pipfile" . 4)
                                      ("/home/user/project/.python-verion" . 5)
                                      ("/home/user/project/environment.yml" . 6)))

    (spy-on 'pet-project-root :and-return-value "/home/user/project/")
    (spy-on 'buffer-list :and-return-value buffers)
    (spy-on 'file-notify-rm-watch))

  (describe "when a buffer is killed"
    (it "should do nothing if it's not a file visiting buffer"
      (kill-buffer (with-current-buffer (get-buffer-create "*pet test*")
                     (python-mode)
                     (current-buffer)))

      (expect pet-project-virtualenv-cache :to-equal '(("/home/user/project/" . "/home/user/project/.venv/")))
      (expect pet-pre-commit-config-cache :to-equal '(("/home/user/project/.pre-commit-config.yaml" . "whatever")))
      (expect pet-pyproject-cache :to-equal '(("/home/user/project/pyproject.toml" . "whatever")))
      (expect pet-pipfile-cache :to-equal '(("/home/user/project/Pipfile" . "whatever")))
      (expect pet-python-version-cache :to-equal '(("/home/user/project/.python-verion" . "whatever")))
      (expect pet-environment-cache :to-equal '(("/home/user/project/environment.yml" . "whatever")))
      (expect pet-watched-config-files :to-equal '(("/home/user/.cache/pre-commit/db.db" . 1)
                                                    ("/home/user/project/.pre-commit-config.yaml" . 2)
                                                    ("/home/user/project/pyproject.toml" . 3)
                                                    ("/home/user/project/Pipfile" . 4)
                                                    ("/home/user/project/.python-verion" . 5)
                                                    ("/home/user/project/environment.yml" . 6))))

    (it "should do nothing if it's not a python mode buffer"
      (kill-buffer (with-current-buffer (create-file-buffer "/home/user/.emacs")
                     (emacs-lisp-mode)
                     (set-buffer-modified-p nil)
                     (current-buffer)))

      (expect pet-project-virtualenv-cache :to-equal '(("/home/user/project/" . "/home/user/project/.venv/")))
      (expect pet-pre-commit-config-cache :to-equal '(("/home/user/project/.pre-commit-config.yaml" . "whatever")))
      (expect pet-pyproject-cache :to-equal '(("/home/user/project/pyproject.toml" . "whatever")))
      (expect pet-pipfile-cache :to-equal '(("/home/user/project/Pipfile" . "whatever")))
      (expect pet-python-version-cache :to-equal '(("/home/user/project/.python-verion" . "whatever")))
      (expect pet-environment-cache :to-equal '(("/home/user/project/environment.yml" . "whatever")))
      (expect pet-watched-config-files :to-equal '(("/home/user/.cache/pre-commit/db.db" . 1)
                                                    ("/home/user/project/.pre-commit-config.yaml" . 2)
                                                    ("/home/user/project/pyproject.toml" . 3)
                                                    ("/home/user/project/Pipfile" . 4)
                                                    ("/home/user/project/.python-verion" . 5)
                                                    ("/home/user/project/environment.yml" . 6))))

    (it "should do nothing if it does not belong in a project"
      (spy-calls-reset 'pet-project-root)
      (spy-on 'pet-project-root)

      (kill-buffer (with-current-buffer (create-file-buffer "/home/user/baz.py")
                     (set-visited-file-name "/home/user/baz.py")
                     (python-mode)
                     (set-buffer-modified-p nil)
                     (current-buffer)))

      (expect pet-project-virtualenv-cache :to-equal '(("/home/user/project/" . "/home/user/project/.venv/")))
      (expect pet-pre-commit-config-cache :to-equal '(("/home/user/project/.pre-commit-config.yaml" . "whatever")))
      (expect pet-pyproject-cache :to-equal '(("/home/user/project/pyproject.toml" . "whatever")))
      (expect pet-pipfile-cache :to-equal '(("/home/user/project/Pipfile" . "whatever")))
      (expect pet-python-version-cache :to-equal '(("/home/user/project/.python-verion" . "whatever")))
      (expect pet-environment-cache :to-equal '(("/home/user/project/environment.yml" . "whatever")))
      (expect pet-watched-config-files :to-equal '(("/home/user/.cache/pre-commit/db.db" . 1)
                                                    ("/home/user/project/.pre-commit-config.yaml" . 2)
                                                    ("/home/user/project/pyproject.toml" . 3)
                                                    ("/home/user/project/Pipfile" . 4)
                                                    ("/home/user/project/.python-verion" . 5)
                                                    ("/home/user/project/environment.yml" . 6)))))

  (describe "when the last Python buffer for a project is killed"
    (before-each
      (kill-buffer (with-current-buffer (create-file-buffer last-project-buf-file-name)
                     (set-visited-file-name last-project-buf-file-name)
                     (python-mode)
                     (set-buffer-modified-p nil)
                     (current-buffer))))

    (it "should clean `pet-project-virtualenv-cache'"
      (expect (assoc-default "/home/user/project/" pet-project-virtualenv-cache) :to-be nil))

    (it "should clear all watched files except `pre-commit' db"
      (expect pet-watched-config-files :to-equal '(("/home/user/.cache/pre-commit/db.db" . 1))))

    (it "should clear all config file caches"
      (expect pet-project-virtualenv-cache :to-be nil)
      (expect pet-pre-commit-config-cache :to-be nil)
      (expect pet-pyproject-cache :to-be nil)
      (expect pet-pipfile-cache :to-be nil)
      (expect pet-python-version-cache :to-be nil)
      (expect pet-environment-cache :to-be nil))))

;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
