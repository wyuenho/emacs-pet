;; -*- lexical-binding: t; -*-

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
        (spy-on 'minibuffer-message))
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

(describe "pet-find-file-from-project-root-natively"
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

  (it "should find file using fd command"
    (spy-on 'executable-find :and-return-value "/usr/bin/fd")
    (spy-on 'process-lines :and-return-value '("/home/user/project/environment-dev.yaml"))
    (expect (pet-find-file-from-project-root-natively "environment*.yaml")
            :to-equal "/home/user/project/environment-dev.yaml"))

  (it "should call fd with correct arguments"
    (spy-on 'executable-find :and-return-value "/usr/bin/fd")
    (spy-on 'process-lines :and-return-value '("/home/user/project/environment-dev.yaml"))
    (pet-find-file-from-project-root-natively "environment*.yaml")
    (expect 'process-lines :to-have-been-called-with
            "/usr/bin/fd"
            '("-tf" "-cnever" "-H" "-a" "-g" "environment*.yaml" "/home/user/project/")))

  (it "should use custom fd command and arguments when configured"
    (let ((pet-fd-command "fdfind")
          (pet-fd-command-args '("-t" "f" "--hidden")))
      (spy-on 'executable-find :and-return-value "/usr/bin/fdfind")
      (spy-on 'process-lines :and-return-value '("/home/user/project/test.txt"))
      (pet-find-file-from-project-root-natively "test.txt")
      (expect 'process-lines :to-have-been-called-with
              "/usr/bin/fdfind"
              '("-t" "f" "--hidden" "test.txt" "/home/user/project/"))))

  (it "should return nil when fd is not available"
    (spy-on 'executable-find :and-return-value nil)
    (expect (pet-find-file-from-project-root-natively "file.txt") :to-be nil))

  (it "should return nil when fd finds no matches"
    (spy-on 'executable-find :and-return-value "/usr/bin/fd")
    (spy-on 'process-lines :and-return-value nil)
    (expect (pet-find-file-from-project-root-natively "nonexistent.txt") :to-be nil))

  (it "should handle fd command errors gracefully"
    (spy-on 'executable-find :and-return-value "/usr/bin/fd")
    (spy-on 'process-lines :and-throw-error '(error "fd failed"))
    (spy-on 'pet-report-error)
    (expect (pet-find-file-from-project-root-natively "file.txt") :to-be nil)
    (expect 'pet-report-error :to-have-been-called))

  (it "should return nil when not in a project"
    (spy-on 'pet-project-root :and-return-value nil)
    (expect (pet-find-file-from-project-root-natively "file.txt") :to-be nil)))

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
      (unintern 'cache obarray)
      (makunbound 'callback)
      (unintern 'callback obarray))

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
      (unintern 'cache obarray)
      (fmakunbound 'parser)
      (unintern 'parser obarray)
      (makunbound 'callback)
      (unintern 'callback obarray))

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
      (unintern 'pet-tox-ini-cache obarray)
      (fmakunbound 'parser)
      (unintern 'parser obarray))

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
    (unintern 'parser obarray))

  (before-each
    (pet-def-config-accessor tox-ini :file-name "tox.ini" :parser parser))

  (after-each
    (fmakunbound 'pet-tox-ini)
    (unintern 'pet-tox-ini obarray)
    (makunbound 'pet-tox-ini-cache)
    (unintern 'pet-tox-ini-cache obarray))

  (it "should create cache variable"
    (expect (boundp 'pet-tox-ini-cache) :to-be t))

  (it "should create cache access function"
    (expect (fboundp 'pet-tox-ini) :to-be t))

  (describe "the cache access function"
    (before-each
      (spy-on 'pet-find-file-from-project :and-return-value "/home/user/project/tox.ini")
      (spy-on 'pet-watch-config-file :and-call-fake (lambda (_ __ ___)))
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
        (expect (assoc-default "/home/user/project/tox.ini" pet-tox-ini-cache) :to-equal "content")))))

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

    (it "should return nil if `conda' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-conda-p) :to-be nil)))

  (describe "when the project does not have a `environment[a-zA-Z0-9-_].yaml' file"
    (before-each
      (spy-on 'pet-environment))

    (it "should return nil if `conda' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "conda") "/usr/bin/conda")))
      (expect (pet-use-conda-p) :to-be nil))

    (it "should return nil if `conda' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-conda-p) :to-be nil))))

(describe "pet-use-mamba-p"
  (describe "when the project has an `environment[a-zA-Z0-9-_].yaml' file"
    (before-each
      (spy-on 'pet-environment :and-return-value t))

    (it "should return `mamba' path if `mamba' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "mamba") "/usr/bin/mamba")))
      (expect (pet-use-mamba-p) :to-equal "/usr/bin/mamba"))

    (it "should return `micromamba' path if `micromamba' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "micromamba") "/usr/bin/micromamba")))
      (expect (pet-use-mamba-p) :to-equal "/usr/bin/micromamba"))

    (it "should return nil if none of `mamba' or `micromamba' is found"
      (spy-on 'executable-find)
      (expect (pet-use-mamba-p) :to-be nil)))

  (describe "when the project does not have a `environment[a-zA-Z0-9-_].yaml' file"
    (before-each
      (spy-on 'pet-environment))

    (it "should return nil if `mamba' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "mamba") "/usr/bin/mamba")))
      (expect (pet-use-mamba-p) :to-be nil))

    (it "should return nil if `microconda' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "microconda") "/usr/bin/microconda")))
      (expect (pet-use-mamba-p) :to-be nil))

    (it "should return nil if none of `mamba' or `micromamba' is found"
      (spy-on 'executable-find)
      (expect (pet-use-mamba-p) :to-be nil))))

(describe "pet-use-pixi-p"
  (describe "when the project has a `pixi.toml' file"
    (before-each
      (spy-on 'pet-pyproject)
      (spy-on 'pet-pixi :and-return-value '((workspace (name . "test-project")))))

    (it "should return `pixi' path if `pixi' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "pixi") "/usr/bin/pixi")))
      (expect (pet-use-pixi-p) :to-equal "/usr/bin/pixi"))

    (it "should return nil if `pixi' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pixi-p) :to-be nil)))

  (describe "when the project has `tool.pixi' section in `pyproject.toml'"
    (before-each
      (spy-on 'pet-pixi)
      (spy-on 'pet-pyproject :and-return-value '((tool (pixi (environments (test . ("test"))))))))

    (it "should return `pixi' path if `pixi' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "pixi") "/usr/bin/pixi")))
      (expect (pet-use-pixi-p) :to-equal "/usr/bin/pixi"))

    (it "should return nil if `pixi' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pixi-p) :to-be nil)))

  (describe "when the project has both `pixi.toml' and `tool.pixi' in `pyproject.toml'"
    (before-each
      (spy-on 'pet-pixi :and-return-value '((workspace (name . "test-project"))))
      (spy-on 'pet-pyproject :and-return-value '((tool (pixi (environments (test . ("test"))))))))

    (it "should return `pixi' path if `pixi' is found (pixi.toml takes precedence)"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "pixi") "/usr/bin/pixi")))
      (expect (pet-use-pixi-p) :to-equal "/usr/bin/pixi"))

    (it "should return nil if `pixi' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pixi-p) :to-be nil)))

  (describe "when the project does not have `pixi.toml' or `tool.pixi' section"
    (before-each
      (spy-on 'pet-pixi)
      (spy-on 'pet-pyproject))

    (it "should return nil if `pixi' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "pixi") "/usr/bin/pixi")))
      (expect (pet-use-pixi-p) :to-be nil))

    (it "should return nil if `pixi' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pixi-p) :to-be nil)))

  (describe "when `pyproject.toml' has empty `tool.pixi' section"
    (before-each
      (spy-on 'pet-pixi)
      (spy-on 'pet-pyproject :and-return-value '((tool (pixi)))))

    (it "should return nil if `pixi' is found (empty tool.pixi is not valid)"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "pixi") "/usr/bin/pixi")))
      (expect (pet-use-pixi-p) :to-be nil))

    (it "should return nil if `pixi' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pixi-p) :to-be nil))))

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

  (describe "when given an absolute path"
    (it "should return the absolute path if the file exists and is executable"
      (spy-on 'file-remote-p :and-return-value nil)
      (spy-on 'file-name-absolute-p :and-return-value t)
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/python")
      (expect (pet-executable-find "/usr/bin/python") :to-equal "/usr/bin/python"))

    (it "should return nil if the absolute path file does not exist or is not executable"
      (spy-on 'file-remote-p :and-return-value nil)
      (spy-on 'file-name-absolute-p :and-return-value t)
      (spy-on 'pet--executable-find :and-return-value nil)
      (spy-on 'pet-use-pre-commit-p :and-return-value nil)
      (spy-on 'pet-virtualenv-root :and-return-value nil)
      (expect (pet-executable-find "/nonexistent/path") :to-be nil))

    (it "should not use absolute path optimization for remote files"
      (spy-on 'file-remote-p :and-return-value "/ssh:user@host:")
      (spy-on 'file-name-absolute-p :and-return-value t)
      (spy-on 'pet-use-pre-commit-p :and-return-value nil)
      (spy-on 'pet-virtualenv-root :and-return-value nil)
      (spy-on 'pet--executable-find :and-return-value nil)
      (expect (pet-executable-find "/ssh:user@host:/usr/bin/python") :to-be nil))

    (it "should not use absolute path optimization for relative paths"
      (spy-on 'file-remote-p :and-return-value nil)
      (spy-on 'file-name-absolute-p :and-return-value nil)
      (spy-on 'pet-use-pre-commit-p :and-return-value nil)
      (spy-on 'pet-virtualenv-root :and-return-value nil)
      (spy-on 'pet--executable-find :and-return-value nil)
      (expect (pet-executable-find "python") :to-be nil)))

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

  (describe "when on *nix"
    (it "should return the absolute path to the python executable for a project if its virtualenv is found"
      (spy-on 'pet-use-pre-commit-p :and-return-value nil)
      (spy-on 'pet-virtualenv-root :and-return-value "/home/user/project/.venv/")
      (spy-on 'pet-use-conda-p :and-return-value nil)
      (spy-on 'pet-system-bin-dir)
      (spy-on 'executable-find :and-return-value "/home/user/project/.venv/bin/python")
      (expect (pet-executable-find "python") :to-equal "/home/user/project/.venv/bin/python")
      (expect 'pet-system-bin-dir :to-have-been-called-times 1))

    (it "should return the absolute path to the python executable for a conda project if its virtualenv is found"
      (spy-on 'pet-use-pre-commit-p :and-return-value nil)
      (spy-on 'pet-virtualenv-root :and-return-value "/home/user/anaconda/envs/project/")
      (spy-on 'pet-use-conda-p :and-return-value t)
      (spy-on 'pet-system-bin-dir)
      (spy-on 'executable-find :and-return-value "/home/user/anaconda/envs/project/bin/python")
      (expect (pet-executable-find "python") :to-equal "/home/user/anaconda/envs/project/bin/python")
      (expect 'pet-system-bin-dir :to-have-been-called-times 1)))

  (describe "when on windows"
    (before-each
      (setq-local system-type 'windows-nt))

    (after-each
      (kill-local-variable 'system-type))

    (it "should return the absolute path to the python executable for a project if its virtualenv is found"
      (spy-on 'pet-use-pre-commit-p :and-return-value nil)
      (spy-on 'pet-virtualenv-root :and-return-value "C:/Users/user/project/.venv/")
      (spy-on 'pet-use-conda-p :and-return-value nil)
      (spy-on 'pet-system-bin-dir)
      (spy-on 'executable-find :and-return-value "C:/Users/user/project/.venv/bin/python")
      (expect (pet-executable-find "python") :to-equal "C:/Users/user/project/.venv/bin/python")
      (expect 'pet-system-bin-dir :to-have-been-called-times 1))

    (it "should return the absolute path to the python executable for a conda project if its virtualenv is found"
      (spy-on 'pet-use-pre-commit-p :and-return-value nil)
      (spy-on 'pet-virtualenv-root :and-return-value "C:/Users/user/Anaconda3/envs/project/")
      (spy-on 'pet-use-conda-p :and-return-value t)
      (spy-on 'pet-system-bin-dir)
      (spy-on 'executable-find :and-return-value "C:/Users/user/Anaconda3/envs/project/python")
      (expect (pet-executable-find "python") :to-equal "C:/Users/user/Anaconda3/envs/project/python")
      (expect 'pet-system-bin-dir :not :to-have-been-called)))

  (describe "when `pet-search-globally' is t"
    (it "should return the absolute path of the result of `pyenv which EXECUTABLE' if no virtualenv is found but `pyenv' is in `exec-path'"
      (spy-on 'pet-use-pre-commit-p :and-return-value nil)
      (spy-on 'pet-virtualenv-root :and-return-value nil)
      (spy-on 'pet--executable-find :and-call-fake (lambda (executable &optional _)
                                                     (when (equal executable "pyenv")
                                                       "/usr/bin/pyenv")))
      (spy-on 'process-lines :and-return-value '("/home/user/.pyenv/versions/3.10.5/bin/python"))
      (expect (pet-executable-find "python" t) :to-equal "/home/user/.pyenv/versions/3.10.5/bin/python")
      (expect 'process-lines :to-have-been-called-with "pyenv" "which" "python")
      (expect 'pet--executable-find :to-have-been-called-times 1))

    (it "should return the absolute path the executable for a project from `exec-path'"
      (spy-on 'pet-use-pre-commit-p :and-return-value nil)
      (spy-on 'pet-virtualenv-root :and-return-value nil)
      (spy-on 'pet--executable-find :and-call-fake (lambda (executable &optional _)
                                                     (when (equal executable "black")
                                                       "/home/user/project/.venv/bin/black")))
      (expect (pet-executable-find "black" t) :to-equal "/home/user/project/.venv/bin/black")
      (expect 'pet--executable-find :to-have-been-called-times 2)))

  (describe "when `pet-search-globally' is nil"
    (before-each
      (setq-local pet-search-globally nil))

    (after-each
      (kill-local-variable 'pet-search-globally))

    (it "should not return the absolute path of the result of `pyenv which EXECUTABLE' if no virtualenv is found but `pyenv' is in `exec-path'"
      (spy-on 'pet-use-pre-commit-p :and-return-value nil)
      (spy-on 'pet-virtualenv-root :and-return-value nil)
      (spy-on 'pet--executable-find :and-call-fake (lambda (executable &optional _)
                                                     (when (equal executable "pyenv")
                                                       "/usr/bin/pyenv")))
      (spy-on 'process-lines :and-return-value '("/home/user/.pyenv/versions/3.10.5/bin/python"))

      (expect (pet-executable-find "python" nil) :to-equal nil)
      (expect 'process-lines :not :to-have-been-called-with "pyenv" "which" "python")
      (expect 'pet--executable-find :to-have-been-called-times 0))

    (it "should not return the absolute path the executable for a project from `exec-path'"
      (spy-on 'pet-use-pre-commit-p :and-return-value nil)
      (spy-on 'pet-virtualenv-root :and-return-value nil)
      (spy-on 'pet--executable-find :and-call-fake (lambda (executable &optional _)
                                                     (when (equal executable "black")
                                                       "/home/user/project/.venv/bin/black")))

      (expect (pet-executable-find "black" nil) :to-equal nil)
      (expect 'pet--executable-find :to-have-been-called-times 0))))

(describe "pet-virtualenv-root"
  :var ((project-root "/home/user/project/")
        (conda-path "/usr/bin/conda")
        (conda-virtualenv "/home/user/miniforge3/envs/project/")
        (mamba-path "/usr/bin/micromamba")
        (mamba-virtualenv "/home/user/micromamba/envs/project/")
        (pixi-path "/usr/bin/pixi")
        (pixi-virtualenv "/home/user/project/.pixi/envs/default/")
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
    (spy-on 'pet-use-pixi-p :and-return-value nil)
    (spy-on 'pet-use-conda-p :and-return-value conda-path)
    (spy-on 'pet-use-mamba-p :and-return-value nil)
    (spy-on 'getenv :and-call-fake (lambda (name) (when (equal name "CONDA_PREFIX") conda-virtualenv)))
    (expect (pet-virtualenv-root) :to-equal conda-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal conda-virtualenv))

  (it "should return the absolute path of the virtualenv for a project using `mamba'"
    (spy-on 'pet-use-pixi-p :and-return-value nil)
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-mamba-p :and-return-value mamba-path)
    (spy-on 'getenv :and-call-fake (lambda (name) (when (equal name "CONDA_PREFIX") mamba-virtualenv)))
    (expect (pet-virtualenv-root) :to-equal mamba-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal mamba-virtualenv))

  (it "should return the absolute path of the virtualenv for a project using `pixi'"
    (spy-on 'pet-use-pixi-p :and-return-value pixi-path)
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-mamba-p :and-return-value nil)
    (spy-on 'getenv :and-call-fake (lambda (name) (when (equal name "CONDA_PREFIX") pixi-virtualenv)))
    (expect (pet-virtualenv-root) :to-equal pixi-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal pixi-virtualenv))

  (it "should return the absolute path of the virtualenv for a project using `poetry'"
    (spy-on 'pet-use-pixi-p :and-return-value nil)
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-mamba-p :and-return-value nil)
    (spy-on 'pet-use-poetry-p :and-return-value poetry-path)
    (spy-on 'pet-pyproject-path :and-return-value "/home/user/project/pyproject.toml")
    (spy-on 'call-process :and-call-fake (lambda (&rest _) (insert poetry-virtualenv) 0))
    (expect (pet-virtualenv-root) :to-equal poetry-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal poetry-virtualenv)
    (expect 'call-process :to-have-been-called-with poetry-path nil t nil "env" "info" "--no-ansi" "--path"))

  (it "should return the absolute path of the virtualenv for a project using `pipenv'"
    (spy-on 'pet-use-pixi-p :and-return-value nil)
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-mamba-p :and-return-value nil)
    (spy-on 'pet-use-poetry-p :and-return-value nil)
    (spy-on 'pet-use-pipenv-p :and-return-value pipenv-path)
    (spy-on 'pet-pipfile-path :and-return-value "/home/user/project/Pipfile")
    (spy-on 'call-process :and-call-fake (lambda (&rest _) (insert pipenv-virtualenv) 0))
    (expect (pet-virtualenv-root) :to-equal pipenv-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal pipenv-virtualenv)
    (expect 'call-process :to-have-been-called-with pipenv-path nil '(t nil) nil "--quiet" "--venv"))

  (it "should return the absolute path of the `.venv' or `venv' directory in a project"
    (spy-on 'pet-use-pixi-p :and-return-value nil)
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-mamba-p :and-return-value nil)
    (spy-on 'pet-use-poetry-p :and-return-value nil)
    (spy-on 'pet-use-pipenv-p :and-return-value nil)
    (spy-on 'locate-dominating-file :and-return-value project-root)
    (expect (pet-virtualenv-root) :to-equal venv-virtualenv)
    (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal venv-virtualenv))

  (it "should return the absolute path of the virtualenv for a project using `pyenv'"
    (spy-on 'pet-use-pixi-p :and-return-value nil)
    (spy-on 'pet-use-conda-p :and-return-value nil)
    (spy-on 'pet-use-mamba-p :and-return-value nil)
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

(describe "pet-pixi-environments"
  (describe "when pixi is available"
    (before-each
      (spy-on 'pet-use-pixi-p :and-return-value "/usr/bin/pixi"))

    (it "should return list of environment prefixes on success"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{\"environments_info\": [{\"prefix\": \"/home/user/project/.pixi/envs/default\"}, {\"prefix\": \"/home/user/project/.pixi/envs/test\"}]}")
                0))
      (expect (pet-pixi-environments) :to-equal '("/home/user/project/.pixi/envs/default" "/home/user/project/.pixi/envs/test")))

    (it "should return empty list when no environments exist"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{\"environments_info\": []}")
                0))
      (expect (pet-pixi-environments) :to-equal '()))

    (it "should return nil on command failure"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "pixi: command not found")
                1))
      (spy-on 'pet-report-error)
      (expect (pet-pixi-environments) :to-equal nil)
      (expect 'pet-report-error :to-have-been-called-with '(user-error "pixi: command not found")))

    (it "should handle malformed JSON gracefully"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{invalid json")
                0))
      (spy-on 'pet-report-error)
      (expect (pet-pixi-environments) :to-be nil)
      (expect 'pet-report-error :to-have-been-called)))

  (describe "when pixi is not available"
    (before-each
      (spy-on 'pet-use-pixi-p :and-return-value nil))

    (it "should return nil"
      (expect (pet-pixi-environments) :to-be nil))))

(describe "pet-conda-environments"
  (describe "when conda is available"
    (before-each
      (spy-on 'pet-use-conda-p :and-return-value "/usr/bin/conda"))

    (it "should return list of environment paths on success"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{\"envs\": [\"/home/user/miniforge3/envs/myenv\", \"/home/user/miniforge3/envs/test\"]}")
                0))
      (expect (pet-conda-environments) :to-equal '("/home/user/miniforge3/envs/myenv" "/home/user/miniforge3/envs/test")))

    (it "should return empty list when no environments exist"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{\"envs\": []}")
                0))
      (expect (pet-conda-environments) :to-equal '()))

    (it "should return nil on command failure"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "conda: command not found")
                1))
      (spy-on 'pet-report-error)
      (expect (pet-conda-environments) :to-equal nil)
      (expect 'pet-report-error :to-have-been-called-with '(user-error "conda: command not found")))

    (it "should handle malformed JSON gracefully"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{invalid json")
                0))
      (spy-on 'pet-report-error)
      (expect (pet-conda-environments) :to-be nil)
      (expect 'pet-report-error :to-have-been-called)))

  (describe "when conda is not available"
    (before-each
      (spy-on 'pet-use-conda-p :and-return-value nil))

    (it "should return nil"
      (expect (pet-conda-environments) :to-be nil))))

(describe "pet-mamba-environments"
  (describe "when mamba is available"
    (before-each
      (spy-on 'pet-use-mamba-p :and-return-value "/usr/bin/mamba"))

    (it "should return list of environment paths on success"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{\"envs\": [\"/home/user/micromamba/envs/myenv\", \"/home/user/micromamba/envs/test\"]}")
                0))
      (expect (pet-mamba-environments) :to-equal '("/home/user/micromamba/envs/myenv" "/home/user/micromamba/envs/test")))

    (it "should return empty list when no environments exist"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{\"envs\": []}")
                0))
      (expect (pet-mamba-environments) :to-equal '()))

    (it "should signal user-error on command failure"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "mamba: command not found")
                1))
      (spy-on 'pet-report-error)
      (expect (pet-mamba-environments) :to-equal nil)
      (expect 'pet-report-error :to-have-been-called-with '(user-error "mamba: command not found")))

    (it "should handle malformed JSON gracefully"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{invalid json")
                0))
      (spy-on 'pet-report-error)
      (expect (pet-mamba-environments) :to-be nil)
      (expect 'pet-report-error :to-have-been-called)))

  (describe "when mamba is not available"
    (before-each
      (spy-on 'pet-use-mamba-p :and-return-value nil))

    (it "should return nil"
      (expect (pet-mamba-environments) :to-be nil))))

(describe "pet-pixi-switch-environment"
  :var ((project-root "/home/user/project/")
        (env-path "/home/user/project/.pixi/envs/test/")
        (other-env-path "/home/user/project/.pixi/envs/default/")
        (buffer-a nil)
        (buffer-b nil)
        (other-project-buffer nil))

  (before-each
    (spy-on 'pet-project-root :and-return-value project-root)
    (spy-on 'pet-pixi-environments :and-return-value
            (list other-env-path env-path))

    ;; Create test buffers in the project
    (setq buffer-a (get-buffer-create "test-a.py"))
    (setq buffer-b (get-buffer-create "test-b.py"))
    (with-current-buffer buffer-a
      (setq buffer-file-name "/home/user/project/main.py")
      (python-mode)
      (setq-local process-environment '("PATH=/usr/bin" "CONDA_PREFIX=/old/env" "HOME=/home/user")))
    (with-current-buffer buffer-b
      (setq buffer-file-name "/home/user/project/utils.py")
      (python-mode)
      (setq-local process-environment '("PATH=/usr/bin" "CONDA_PREFIX=/old/env" "HOME=/home/user")))

    ;; Create buffer from different project
    (setq other-project-buffer (get-buffer-create "other.py"))
    (with-current-buffer other-project-buffer
      (setq buffer-file-name "/home/user/other-project/main.py")
      (python-mode)
      (setq-local process-environment '("CONDA_PREFIX=/other/old/env"))))

  (after-each
    (when buffer-a (kill-buffer buffer-a))
    (when buffer-b (kill-buffer buffer-b))
    (when other-project-buffer (kill-buffer other-project-buffer))
    (setq pet-project-virtualenv-cache nil))

  (describe "basic functionality"
    (it "should update virtualenv cache with selected environment"
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-pixi-switch-environment env-path)

      (expect (assoc-default project-root pet-project-virtualenv-cache)
              :to-equal env-path))

    (it "should call teardown and setup for all project Python buffers"
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)
      (spy-on 'buffer-list :and-return-value (list buffer-a buffer-b other-project-buffer))

      (pet-pixi-switch-environment env-path)

      ;; Should be called once for each project buffer only
      (expect 'pet-buffer-local-vars-teardown :to-have-been-called-times 2)
      (expect 'pet-buffer-local-vars-setup :to-have-been-called-times 2))

    (it "should display success message"
      (spy-on 'message)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-pixi-switch-environment env-path)

      (expect 'message :to-have-been-called-with
              "Switched to %s environment: %s" "pixi" env-path)))

  (describe "environment variable handling"
    (it "should set CONDA_PREFIX environment variable in project buffers"
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)
      (spy-on 'buffer-list :and-return-value (list buffer-a buffer-b))

      (pet-pixi-switch-environment env-path)

      (with-current-buffer buffer-a
        (expect (getenv "CONDA_PREFIX") :to-equal env-path))
      (with-current-buffer buffer-b
        (expect (getenv "CONDA_PREFIX") :to-equal env-path)))

    (it "should preserve other environment variables while updating CONDA_PREFIX"
      (spy-on 'buffer-list :and-return-value (list buffer-a))
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-pixi-switch-environment env-path)

      (with-current-buffer buffer-a
        (expect (getenv "PATH") :to-equal "/usr/bin")
        (expect (getenv "HOME") :to-equal "/home/user")
        (expect (getenv "CONDA_PREFIX") :to-equal env-path))))

  (describe "buffer isolation"
    (it "should not affect buffers from other projects"
      (spy-on 'buffer-list :and-return-value (list buffer-a other-project-buffer))
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-pixi-switch-environment env-path)

      (with-current-buffer buffer-a
        (expect (getenv "CONDA_PREFIX") :to-equal env-path))
      (with-current-buffer other-project-buffer
        (expect (getenv "CONDA_PREFIX") :to-equal "/other/old/env"))))

  (describe "interactive completion"
    (it "should prompt with available environments"
      (spy-on 'completing-read :and-return-value env-path)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (call-interactively #'pet-pixi-switch-environment)

      (expect 'completing-read :to-have-been-called-with
              "Please select a pixi environment: "
              (list other-env-path env-path)
              nil t))

    (it "should use the selected environment"
      (spy-on 'completing-read :and-return-value other-env-path)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (call-interactively #'pet-pixi-switch-environment)

      (expect (assoc-default project-root pet-project-virtualenv-cache)
              :to-equal other-env-path)))

  (describe "error handling"
    (it "should handle when no project root is found"
      (spy-on 'pet-project-root :and-return-value nil)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-pixi-switch-environment env-path)

      (expect 'pet-buffer-local-vars-teardown :not :to-have-been-called)
      (expect 'pet-buffer-local-vars-setup :not :to-have-been-called))

    (it "should handle when no Python buffers exist in project"
      (spy-on 'buffer-list :and-return-value '())
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-pixi-switch-environment env-path)

      (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal env-path)
      (expect 'pet-buffer-local-vars-teardown :not :to-have-been-called))))

(describe "pet-conda-switch-environment"
  :var ((project-root "/home/user/project/")
        (env-path "/home/user/miniforge3/envs/test/")
        (other-env-path "/home/user/miniforge3/envs/default/")
        (buffer-a nil)
        (buffer-b nil)
        (other-project-buffer nil))

  (before-each
    (spy-on 'pet-project-root :and-return-value project-root)
    (spy-on 'pet-conda-environments :and-return-value
            (list other-env-path env-path))

    ;; Create test buffers in the project
    (setq buffer-a (get-buffer-create "test-a.py"))
    (setq buffer-b (get-buffer-create "test-b.py"))
    (with-current-buffer buffer-a
      (setq buffer-file-name "/home/user/project/main.py")
      (python-mode)
      (setq-local process-environment '("PATH=/usr/bin" "CONDA_PREFIX=/old/env" "HOME=/home/user")))
    (with-current-buffer buffer-b
      (setq buffer-file-name "/home/user/project/utils.py")
      (python-mode)
      (setq-local process-environment '("PATH=/usr/bin" "CONDA_PREFIX=/old/env" "HOME=/home/user")))

    ;; Create buffer from different project
    (setq other-project-buffer (get-buffer-create "other.py"))
    (with-current-buffer other-project-buffer
      (setq buffer-file-name "/home/user/other-project/main.py")
      (python-mode)
      (setq-local process-environment '("CONDA_PREFIX=/other/old/env"))))

  (after-each
    (when buffer-a (kill-buffer buffer-a))
    (when buffer-b (kill-buffer buffer-b))
    (when other-project-buffer (kill-buffer other-project-buffer))
    (setq pet-project-virtualenv-cache nil))

  (describe "basic functionality"
    (it "should update virtualenv cache with selected environment"
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-conda-switch-environment env-path)

      (expect (assoc-default project-root pet-project-virtualenv-cache)
              :to-equal env-path))

    (it "should call teardown and setup for all project Python buffers"
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)
      (spy-on 'buffer-list :and-return-value (list buffer-a buffer-b other-project-buffer))

      (pet-conda-switch-environment env-path)

      (expect 'pet-buffer-local-vars-teardown :to-have-been-called-times 2)
      (expect 'pet-buffer-local-vars-setup :to-have-been-called-times 2))

    (it "should display success message"
      (spy-on 'message)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-conda-switch-environment env-path)

      (expect 'message :to-have-been-called-with
              "Switched to %s environment: %s" "conda" env-path)))

  (describe "environment variable handling"
    (it "should set CONDA_PREFIX environment variable in project buffers"
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)
      (spy-on 'buffer-list :and-return-value (list buffer-a buffer-b))

      (pet-conda-switch-environment env-path)

      (with-current-buffer buffer-a
        (expect (getenv "CONDA_PREFIX") :to-equal env-path))
      (with-current-buffer buffer-b
        (expect (getenv "CONDA_PREFIX") :to-equal env-path)))

    (it "should preserve other environment variables while updating CONDA_PREFIX"
      (spy-on 'buffer-list :and-return-value (list buffer-a))
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-conda-switch-environment env-path)

      (with-current-buffer buffer-a
        (expect (getenv "PATH") :to-equal "/usr/bin")
        (expect (getenv "HOME") :to-equal "/home/user")
        (expect (getenv "CONDA_PREFIX") :to-equal env-path))))

  (describe "buffer isolation"
    (it "should not affect buffers from other projects"
      (spy-on 'buffer-list :and-return-value (list buffer-a other-project-buffer))
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-conda-switch-environment env-path)

      (with-current-buffer buffer-a
        (expect (getenv "CONDA_PREFIX") :to-equal env-path))
      (with-current-buffer other-project-buffer
        (expect (getenv "CONDA_PREFIX") :to-equal "/other/old/env"))))

  (describe "interactive completion"
    (it "should prompt with available environments"
      (spy-on 'completing-read :and-return-value env-path)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (call-interactively #'pet-conda-switch-environment)

      (expect 'completing-read :to-have-been-called-with
              "Please select a conda environment: "
              (list other-env-path env-path)
              nil t))

    (it "should use the selected environment"
      (spy-on 'completing-read :and-return-value other-env-path)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (call-interactively #'pet-conda-switch-environment)

      (expect (assoc-default project-root pet-project-virtualenv-cache)
              :to-equal other-env-path)))

  (describe "error handling"
    (it "should handle when no project root is found"
      (spy-on 'pet-project-root :and-return-value nil)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-conda-switch-environment env-path)

      (expect 'pet-buffer-local-vars-teardown :not :to-have-been-called)
      (expect 'pet-buffer-local-vars-setup :not :to-have-been-called))

    (it "should handle when no Python buffers exist in project"
      (spy-on 'buffer-list :and-return-value '())
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-conda-switch-environment env-path)

      (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal env-path)
      (expect 'pet-buffer-local-vars-teardown :not :to-have-been-called))))

(describe "pet-mamba-switch-environment"
  :var ((project-root "/home/user/project/")
        (env-path "/home/user/micromamba/envs/test/")
        (other-env-path "/home/user/micromamba/envs/default/")
        (buffer-a nil)
        (buffer-b nil)
        (other-project-buffer nil))

  (before-each
    (spy-on 'pet-project-root :and-return-value project-root)
    (spy-on 'pet-mamba-environments :and-return-value
            (list other-env-path env-path))

    ;; Create test buffers in the project
    (setq buffer-a (get-buffer-create "test-a.py"))
    (setq buffer-b (get-buffer-create "test-b.py"))
    (with-current-buffer buffer-a
      (setq buffer-file-name "/home/user/project/main.py")
      (python-mode)
      (setq-local process-environment '("PATH=/usr/bin" "CONDA_PREFIX=/old/env" "HOME=/home/user")))
    (with-current-buffer buffer-b
      (setq buffer-file-name "/home/user/project/utils.py")
      (python-mode)
      (setq-local process-environment '("PATH=/usr/bin" "CONDA_PREFIX=/old/env" "HOME=/home/user")))

    ;; Create buffer from different project
    (setq other-project-buffer (get-buffer-create "other.py"))
    (with-current-buffer other-project-buffer
      (setq buffer-file-name "/home/user/other-project/main.py")
      (python-mode)
      (setq-local process-environment '("CONDA_PREFIX=/other/old/env"))))

  (after-each
    (when buffer-a (kill-buffer buffer-a))
    (when buffer-b (kill-buffer buffer-b))
    (when other-project-buffer (kill-buffer other-project-buffer))
    (setq pet-project-virtualenv-cache nil))

  (describe "basic functionality"
    (it "should update virtualenv cache with selected environment"
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-mamba-switch-environment env-path)

      (expect (assoc-default project-root pet-project-virtualenv-cache)
              :to-equal env-path))

    (it "should call teardown and setup for all project Python buffers"
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)
      (spy-on 'buffer-list :and-return-value (list buffer-a buffer-b other-project-buffer))

      (pet-mamba-switch-environment env-path)

      (expect 'pet-buffer-local-vars-teardown :to-have-been-called-times 2)
      (expect 'pet-buffer-local-vars-setup :to-have-been-called-times 2))

    (it "should display success message"
      (spy-on 'message)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-mamba-switch-environment env-path)

      (expect 'message :to-have-been-called-with
              "Switched to %s environment: %s" "mamba" env-path)))

  (describe "environment variable handling"
    (it "should set CONDA_PREFIX environment variable in project buffers"
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)
      (spy-on 'buffer-list :and-return-value (list buffer-a buffer-b))

      (pet-mamba-switch-environment env-path)

      (with-current-buffer buffer-a
        (expect (getenv "CONDA_PREFIX") :to-equal env-path))
      (with-current-buffer buffer-b
        (expect (getenv "CONDA_PREFIX") :to-equal env-path)))

    (it "should preserve other environment variables while updating CONDA_PREFIX"
      (spy-on 'buffer-list :and-return-value (list buffer-a))
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-mamba-switch-environment env-path)

      (with-current-buffer buffer-a
        (expect (getenv "PATH") :to-equal "/usr/bin")
        (expect (getenv "HOME") :to-equal "/home/user")
        (expect (getenv "CONDA_PREFIX") :to-equal env-path))))

  (describe "buffer isolation"
    (it "should not affect buffers from other projects"
      (spy-on 'buffer-list :and-return-value (list buffer-a other-project-buffer))
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-mamba-switch-environment env-path)

      (with-current-buffer buffer-a
        (expect (getenv "CONDA_PREFIX") :to-equal env-path))
      (with-current-buffer other-project-buffer
        (expect (getenv "CONDA_PREFIX") :to-equal "/other/old/env"))))

  (describe "interactive completion"
    (it "should prompt with available environments"
      (spy-on 'completing-read :and-return-value env-path)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (call-interactively #'pet-mamba-switch-environment)

      (expect 'completing-read :to-have-been-called-with
              "Please select a mamba environment: "
              (list other-env-path env-path)
              nil t))

    (it "should use the selected environment"
      (spy-on 'completing-read :and-return-value other-env-path)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (call-interactively #'pet-mamba-switch-environment)

      (expect (assoc-default project-root pet-project-virtualenv-cache)
              :to-equal other-env-path)))

  (describe "error handling"
    (it "should handle when no project root is found"
      (spy-on 'pet-project-root :and-return-value nil)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-mamba-switch-environment env-path)

      (expect 'pet-buffer-local-vars-teardown :not :to-have-been-called)
      (expect 'pet-buffer-local-vars-setup :not :to-have-been-called))

    (it "should handle when no Python buffers exist in project"
      (spy-on 'buffer-list :and-return-value '())
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-mamba-switch-environment env-path)

      (expect (assoc-default project-root pet-project-virtualenv-cache) :to-equal env-path)
      (expect 'pet-buffer-local-vars-teardown :not :to-have-been-called))))

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
    (unintern 'flycheck-checker-get obarray))

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
    (unintern 'flycheck-checker-get obarray))

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

(describe "pet-eglot--executable-find-advice"
  (it "should delegate to `pet-executable-find' for Python LSP servers"
    (spy-on 'eglot--executable-find :and-call-fake (lambda (&rest args) (string-join args " ")))
    (spy-on 'pet-executable-find :and-call-fake 'identity)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "pylsp") :to-equal "pylsp")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "pylsp")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "pyls") :to-equal "pyls")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "pyls")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "basedpyright-langserver") :to-equal "basedpyright-langserver")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "basedpyright-langserver")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "pyright-langserver") :to-equal "pyright-langserver")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "pyright-langserver")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "jedi-language-server") :to-equal "jedi-language-server")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "jedi-language-server")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "ruff") :to-equal "ruff")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "ruff")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "ruff-lsp") :to-equal "ruff-lsp")
    (expect (spy-context-return-value (spy-calls-most-recent 'pet-executable-find)) :to-equal "ruff-lsp")
    (expect 'eglot--executable-find :not :to-have-been-called)

    (expect (pet-eglot--executable-find-advice 'eglot--executable-find "sh" "-c") :to-equal "sh -c")
    (expect 'eglot--executable-find :to-have-been-called-with "sh" "-c")))

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
  (before-each
    (spy-on 'pet-executable-find :and-return-value "/home/user/project/.venv/bin/pylsp")
    (spy-on 'project-current :and-return-value (if (< emacs-major-version 29) (cons 'vc "/home/user/project/") '(vc Git "/home/user/project/")))
    (advice-add 'eglot--executable-find :around #'pet-eglot--executable-find-advice)
    (advice-add 'eglot--guess-contact :around #'pet-eglot--guess-contact-advice))

  (after-each
    (advice-remove 'eglot--executable-find #'pet-eglot--executable-find-advice)
    (advice-remove 'eglot--guess-contact #'pet-eglot--guess-contact-advice))

  (it "should use pet-executable-find path for Python LSP servers"
    (assume (and (require 'eglot nil t)
                 (fboundp 'eglot--executable-find))
            "Unsupported `eglot' version")

    (let ((eglot-server-programs `((python-mode . ,(eglot-alternatives '("pylsp"))))))
      (spy-on 'pet-lookup-eglot-server-initialization-options)
      (with-temp-buffer
        (setq default-directory "/home/user/project")
        (setq buffer-file-name "/home/user/project/file.py")
        (python-mode)
        (let* ((result (eglot--guess-contact))
               (contact (nth 3 result))
               (program (car contact)))
          (expect program :to-equal "/home/user/project/.venv/bin/pylsp")))))

  (it "should add pet initialization options when none exist"
    (assume (and (require 'eglot nil t)
                 (fboundp 'eglot--executable-find))
            "Unsupported `eglot' version")

    (let ((eglot-server-programs `((python-mode . ,(eglot-alternatives '("pylsp"))))))
      (spy-on 'pet-lookup-eglot-server-initialization-options :and-return-value '(:test-option t))
      (with-temp-buffer
        (setq default-directory "/home/user/project")
        (setq buffer-file-name "/home/user/project/file.py")
        (python-mode)
        (let* ((result (eglot--guess-contact))
               (contact (nth 3 result))
               (probe (cl-position-if #'keywordp contact))
               (init-opts (and probe (cl-subseq contact probe))))
          (expect (plist-get init-opts :initializationOptions) :to-equal '(:test-option t))))))

  (it "should merge pet initialization options with existing ones"
    (assume (and (require 'eglot nil t)
                 (fboundp 'eglot--executable-find))
            "Unsupported `eglot' version")

    (let ((eglot-server-programs `((python-mode . ,(eglot-alternatives '(("pylsp" :initializationOptions (:existing-option t))))))))
      (spy-on 'pet-lookup-eglot-server-initialization-options :and-return-value '(:pet-option t))
      (with-temp-buffer
        (setq default-directory "/home/user/project")
        (setq buffer-file-name "/home/user/project/file.py")
        (python-mode)
        (let* ((result (eglot--guess-contact))
               (contact (nth 3 result))
               (probe (cl-position-if #'keywordp contact))
               (init-opts (and probe (cl-subseq contact probe))))
          (expect (plist-get init-opts :initializationOptions) :to-equal '(:existing-option t :pet-option t))))))

  (it "should preserve existing options when pet has no initialization options"
    (assume (and (require 'eglot nil t)
                 (fboundp 'eglot--executable-find))
            "Unsupported `eglot' version")

    (let ((eglot-server-programs `((python-mode . ,(eglot-alternatives '(("pylsp" :initializationOptions (:existing-option t))))))))
      (spy-on 'pet-lookup-eglot-server-initialization-options)
      (with-temp-buffer
        (setq default-directory "/home/user/project")
        (setq buffer-file-name "/home/user/project/file.py")
        (python-mode)
        (let* ((result (eglot--guess-contact))
               (contact (nth 3 result))
               (probe (cl-position-if #'keywordp contact))
               (init-opts (and probe (cl-subseq contact probe))))
          (expect (plist-get init-opts :initializationOptions) :to-equal '(:existing-option t)))))))

(describe "pet-lookup-eglot-server-initialization-options"
  (before-each
    (spy-on 'pet-virtualenv-root :and-return-value "/home/user/project/")
    (spy-on 'pet-executable-find :and-call-fake
            (lambda (command)
              (assoc-default command
                             '(("flake8"               . "/usr/bin/flake8")
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

  (it "should return eglot initialization options for based-pyright"
    (expect (pet-lookup-eglot-server-initialization-options "/home/user/.local/bin/basedpyright-langserver") :to-equal
            `(:python
              (:pythonPath
               "/usr/bin/python"
               :venvPath
               "/home/user/project/"))))

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

  (it "should return eglot initialization options for ruff"
    (expect (pet-lookup-eglot-server-initialization-options "ruff") :to-equal
            '(:settings
              (:interpreter
               "/usr/bin/python"
               :path
               "/usr/bin/ruff"))))

  (it "should return eglot initialization options for ruff-lsp"
    (expect (pet-lookup-eglot-server-initialization-options "ruff-lsp") :to-equal
            '(:settings
              (:interpreter
               "/usr/bin/python"
               :path
               "/usr/bin/ruff"))))

  (it "should return eglot initialization options for pylsp when given as command list"
    (expect (pet-lookup-eglot-server-initialization-options '("/home/user/.local/bin/pylsp" "--arg1" "--arg2")) :to-equal
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

  (it "should return first matching server when command list has multiple servers"
    (expect (pet-lookup-eglot-server-initialization-options '("/usr/bin/unknown-server" "/home/user/.local/bin/pyright-langserver")) :to-equal
            '(:python
              (:pythonPath
               "/usr/bin/python"
               :venvPath
               "/home/user/project/"))))

  (it "should return nil when command list has no recognized servers"
    (expect (pet-lookup-eglot-server-initialization-options '("/usr/bin/unknown-server" "/usr/bin/another-unknown")) :to-be nil))

  (it "should handle empty command list"
    (expect (pet-lookup-eglot-server-initialization-options '()) :to-be nil)))

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
    (expect (advice-member-p 'pet-eglot--executable-find-advice 'eglot--executable-find) :to-be-truthy)
    (expect (advice-member-p 'pet-eglot--guess-contact-advice 'eglot--guess-contact) :to-be-truthy)))

(describe "pet-eglot-teardown"
  (it "should remove `pet' advices from eglot functions"
    (pet-eglot-setup)
    (pet-eglot-teardown)
    (expect (advice-member-p 'pet-eglot--workspace-configuration-plist-advice 'eglot--workspace-configuration-plist) :to-be nil)
    (expect (advice-member-p 'pet-eglot--executable-find-advice 'eglot--executable-find) :to-be nil)
    (expect (advice-member-p 'pet-eglot--guess-contact-advice 'eglot--guess-contact) :to-be nil)))

(describe "pet-dape-setup"
  (before-each
    (spy-on 'pet-find-file-from-project-root-natively)
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
                ("ty"
                 "/usr/bin/ty"))))
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
    (expect (local-variable-p 'lsp-pyls-server-command) :to-be-truthy)
    (expect (local-variable-p 'lsp-pylsp-server-command) :to-be-truthy)
    (expect (local-variable-p 'lsp-python-ty-clients-server-command) :to-be-truthy)
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
    (expect py-autopep8-command :to-equal "/usr/bin/autopep8"))

  (it "should run the hook `pet-after-buffer-local-vars-setup'"
    (spy-on 'pet-executable-find)
    (spy-on 'pet-virtualenv-root)
    (spy-on 'pet-flycheck-setup)
    (spy-on 'pet-eglot-setup)
    (spy-on 'pet-dape-setup)

    (let* ((calls 0)
           (test-func (lambda () (cl-incf calls (1+ calls)))))
      (add-hook 'pet-after-buffer-local-vars-setup test-func)
      (pet-buffer-local-vars-setup)
      (expect calls :to-equal 1)
      (remove-hook 'pet-after-buffer-local-vars-setup test-func))))

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

  (it "should run the hook `pet-before-buffer-local-vars-teardown'"
    (spy-on 'pet-flycheck-teardown)
    (spy-on 'pet-eglot-teardown)
    (spy-on 'pet-dape-teardown)

    (let* ((calls 0)
           (test-func (lambda () (cl-incf calls (1+ calls)))))
      (add-hook 'pet-before-buffer-local-vars-teardown test-func)
      (pet-buffer-local-vars-teardown)
      (expect calls :to-equal 1)
      (remove-hook 'pet-before-buffer-local-vars-teardown test-func)))

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
       :to-equal (if (or (< emacs-major-version 28)
                         (>= emacs-major-version 31))
                     "python"
                   "python3"))))

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
  (before-each
    (spy-on 'pet-project-root :and-return-value "/home/user/project/")
    (spy-on 'pet-buffer-local-vars-setup)
    (spy-on 'pet-buffer-local-vars-teardown)
    (spy-on 'add-hook)
    (spy-on 'remove-hook))

  (describe "when enabling pet-mode"
    (it "should set up all buffer local variables for supported packages if `pet-mode' is t"
      (spy-on 'assoc-default :and-return-value "/home/user/project/.venv/")
      (pet-mode 1)
      (expect 'pet-buffer-local-vars-setup :to-have-been-called)
      (expect 'add-hook :to-have-been-called-with 'kill-buffer-hook #'pet-cleanup-watchers-and-caches t))

    (it "should call pixi environment switch when pixi is detected and no cached virtualenv"
      (spy-on 'assoc-default :and-return-value nil)
      (spy-on 'pet-use-pixi-p :and-return-value "/usr/bin/pixi")
      (spy-on 'call-interactively)
      (pet-mode 1)
      (expect 'call-interactively :to-have-been-called-with #'pet-pixi-switch-environment)
      (expect 'pet-buffer-local-vars-setup :not :to-have-been-called))

    (it "should call conda environment switch when conda is detected and no cached virtualenv and pixi not available"
      (spy-on 'assoc-default :and-return-value nil)
      (spy-on 'pet-use-pixi-p :and-return-value nil)
      (spy-on 'pet-use-conda-p :and-return-value "/usr/bin/conda")
      (spy-on 'call-interactively)
      (pet-mode 1)
      (expect 'call-interactively :to-have-been-called-with #'pet-conda-switch-environment)
      (expect 'pet-buffer-local-vars-setup :not :to-have-been-called))

    (it "should call mamba environment switch when mamba is detected and no cached virtualenv and pixi/conda not available"
      (spy-on 'assoc-default :and-return-value nil)
      (spy-on 'pet-use-pixi-p :and-return-value nil)
      (spy-on 'pet-use-conda-p :and-return-value nil)
      (spy-on 'pet-use-mamba-p :and-return-value "/usr/bin/mamba")
      (spy-on 'call-interactively)
      (pet-mode 1)
      (expect 'call-interactively :to-have-been-called-with #'pet-mamba-switch-environment)
      (expect 'pet-buffer-local-vars-setup :not :to-have-been-called))

    (it "should set up buffer local vars when no environment managers are detected"
      (spy-on 'assoc-default :and-return-value nil)
      (spy-on 'pet-use-pixi-p :and-return-value nil)
      (spy-on 'pet-use-conda-p :and-return-value nil)
      (spy-on 'pet-use-mamba-p :and-return-value nil)
      (pet-mode 1)
      (expect 'pet-buffer-local-vars-setup :to-have-been-called)))

  (describe "when disabling pet-mode"
    (it "should reset all buffer local variables for supported packages to default if `pet-mode' is nil"
      (pet-mode -1)
      (expect 'pet-buffer-local-vars-teardown :to-have-been-called)
      (expect 'remove-hook :to-have-been-called-with 'kill-buffer-hook #'pet-cleanup-watchers-and-caches t))))

(describe "pet-cleanup-watchers-and-caches"
  (describe "when pre-conditions are not met"
    :var ((project-root-a "/home/user/project-a/")
          (config-file-a "/home/user/project-a/pyproject.toml")
          (mock-watcher-a 'mock-watcher-a))

    (before-each
      (setq pet-config-cache-vars '(pet-pyproject-cache pet-environment-cache))
      ;; Set up initial state that should remain unchanged
      (setq pet-project-virtualenv-cache `((,project-root-a . "/venv/a")))
      (setq pet-watched-config-files `((,config-file-a . ,mock-watcher-a)))
      (setq pet-pyproject-cache `((,config-file-a . test-data))))

    (after-each
      ;; Reset all caches
      (setq pet-project-virtualenv-cache nil)
      (setq pet-watched-config-files nil)
      (setq pet-pyproject-cache nil)
      (setq pet-environment-cache nil))

    (it "should not modify anything when buffer has no file name"
      (with-temp-buffer
        (pet-cleanup-watchers-and-caches)

        (expect (assoc-default project-root-a pet-project-virtualenv-cache) :to-equal "/venv/a")
        (expect (assoc-default config-file-a pet-watched-config-files) :to-equal mock-watcher-a)
        (expect (assoc-default config-file-a pet-pyproject-cache) :to-equal 'test-data)))

    (it "should not modify anything when buffer is not python-mode"
      (with-temp-buffer
        (setq buffer-file-name "/some/file.txt")
        (pet-cleanup-watchers-and-caches)

        (expect (assoc-default project-root-a pet-project-virtualenv-cache) :to-equal "/venv/a")
        (expect (assoc-default config-file-a pet-watched-config-files) :to-equal mock-watcher-a)
        (expect (assoc-default config-file-a pet-pyproject-cache) :to-equal 'test-data)))

    (it "should not modify anything when project root cannot be determined"
      (with-temp-buffer
        (setq buffer-file-name "/some/file.py")
        (python-mode)
        (spy-on 'pet-project-root :and-return-value nil)

        (pet-cleanup-watchers-and-caches)

        (expect (assoc-default project-root-a pet-project-virtualenv-cache) :to-equal "/venv/a")
        (expect (assoc-default config-file-a pet-watched-config-files) :to-equal mock-watcher-a)
        (expect (assoc-default config-file-a pet-pyproject-cache) :to-equal 'test-data))))

  (describe "when pre-conditions are met"
    :var ((project-root-a "/home/user/project-a/")
          (project-root-b "/home/user/project-b/")
          (config-file-a "/home/user/project-a/pyproject.toml")
          (config-file-b "/home/user/project-b/environment.yml")
          (mock-watcher-a 'mock-watcher-a)
          (mock-watcher-b 'mock-watcher-b)
          (other-buffer-a nil)
          (other-buffer-b nil))

    (before-each
      (setq pet-config-cache-vars '(pet-pyproject-cache pet-environment-cache))
      (spy-on 'file-notify-rm-watch))

    (after-each
      ;; Clean up test buffers
      (when (and other-buffer-a (buffer-live-p other-buffer-a)) (kill-buffer other-buffer-a))
      (when (and other-buffer-b (buffer-live-p other-buffer-b)) (kill-buffer other-buffer-b))
      ;; Reset all caches
      (setq pet-project-virtualenv-cache nil)
      (setq pet-watched-config-files nil)
      (setq pet-pyproject-cache nil)
      (setq pet-environment-cache nil))

    (describe "when other python buffers exist in project"
      (it "should not clean up anything"
        (with-temp-buffer
          (setq buffer-file-name "/home/user/project-a/main.py")
          (python-mode)
          (spy-on 'pet-project-root :and-return-value project-root-a)

          ;; Set up other buffer in same project
          (setq other-buffer-a (get-buffer-create "other-a"))
          (with-current-buffer other-buffer-a
            (setq buffer-file-name "/home/user/project-a/module.py")
            (python-mode))

          ;; Set up initial cache state
          (setq pet-project-virtualenv-cache `((,project-root-a . "/venv/a") (,project-root-b . "/venv/b")))
          (setq pet-watched-config-files `((,config-file-a . ,mock-watcher-a) (,config-file-b . ,mock-watcher-b)))
          (setq pet-pyproject-cache `((,config-file-a . test-data-a) (,config-file-b . test-data-b)))

          (pet-cleanup-watchers-and-caches)

          ;; Nothing should be cleaned up
          (expect (assoc-default project-root-a pet-project-virtualenv-cache) :to-equal "/venv/a")
          (expect (assoc-default project-root-b pet-project-virtualenv-cache) :to-equal "/venv/b")
          (expect (assoc-default config-file-a pet-watched-config-files) :to-equal mock-watcher-a)
          (expect (assoc-default config-file-b pet-watched-config-files) :to-equal mock-watcher-b)
          (expect (assoc-default config-file-a pet-pyproject-cache) :to-equal 'test-data-a)
          (expect (assoc-default config-file-b pet-pyproject-cache) :to-equal 'test-data-b)
          (expect 'file-notify-rm-watch :not :to-have-been-called))))

    (describe "when no other python buffers exist in project"
      (it "should clear virtualenv cache for project only"
        (with-temp-buffer
          (setq buffer-file-name "/home/user/project-a/main.py")
          (python-mode)
          (spy-on 'pet-project-root :and-return-value project-root-a)

          ;; Set up other buffer in different project
          (setq other-buffer-b (get-buffer-create "other-b"))
          (with-current-buffer other-buffer-b
            (setq buffer-file-name "/home/user/project-b/other.py")
            (python-mode))

          (setq pet-project-virtualenv-cache
                `((,project-root-a . "/venv/a") (,project-root-b . "/venv/b")))

          (pet-cleanup-watchers-and-caches)

          (expect (assoc-default project-root-a pet-project-virtualenv-cache) :to-be nil)
          (expect (assoc-default project-root-b pet-project-virtualenv-cache) :to-equal "/venv/b")))

      (it "should remove file watchers for project files only"
        (with-temp-buffer
          (setq buffer-file-name "/home/user/project-a/main.py")
          (python-mode)
          (spy-on 'pet-project-root :and-return-value project-root-a)

          ;; Set up other buffer in different project
          (setq other-buffer-b (get-buffer-create "other-b"))
          (with-current-buffer other-buffer-b
            (setq buffer-file-name "/home/user/project-b/other.py")
            (python-mode))

          (setq pet-watched-config-files
                `((,config-file-a . ,mock-watcher-a) (,config-file-b . ,mock-watcher-b)))

          (pet-cleanup-watchers-and-caches)

          (expect 'file-notify-rm-watch :to-have-been-called-with mock-watcher-a)
          (expect 'file-notify-rm-watch :not :to-have-been-called-with mock-watcher-b)
          (expect (assoc-default config-file-a pet-watched-config-files) :to-be nil)
          (expect (assoc-default config-file-b pet-watched-config-files) :to-equal mock-watcher-b)))

      (it "should clear all config caches for project only"
        (with-temp-buffer
          (setq buffer-file-name "/home/user/project-a/main.py")
          (python-mode)
          (spy-on 'pet-project-root :and-return-value project-root-a)

          ;; Set up other buffer in different project
          (setq other-buffer-b (get-buffer-create "other-b"))
          (with-current-buffer other-buffer-b
            (setq buffer-file-name "/home/user/project-b/other.py")
            (python-mode))

          (setq pet-pyproject-cache
                `((,config-file-a . test-data-a) (,config-file-b . test-data-b)))
          (setq pet-environment-cache
                `((,config-file-a . env-data-a) (,config-file-b . env-data-b)))

          (pet-cleanup-watchers-and-caches)

          (expect (assoc-default config-file-a pet-pyproject-cache) :to-be nil)
          (expect (assoc-default config-file-b pet-pyproject-cache) :to-equal 'test-data-b)
          (expect (assoc-default config-file-a pet-environment-cache) :to-be nil)
          (expect (assoc-default config-file-b pet-environment-cache) :to-equal 'env-data-b)))

      (it "should handle empty caches gracefully"
        (with-temp-buffer
          (setq buffer-file-name "/home/user/project-a/main.py")
          (python-mode)
          (spy-on 'pet-project-root :and-return-value project-root-a)

          ;; Ensure caches are empty
          (setq pet-project-virtualenv-cache nil)
          (setq pet-watched-config-files nil)
          (setq pet-pyproject-cache nil)
          (setq pet-environment-cache nil)

          (expect (pet-cleanup-watchers-and-caches) :not :to-throw)
          (expect 'file-notify-rm-watch :not :to-have-been-called))))))

;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
