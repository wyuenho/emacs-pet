;;; python-x.el --- python mode extensions -*- lexical-binding: t -*-

;; Author: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Maintainer: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/wyuenho/emacs-python-x/
;; Keywords: python


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; TODO:
;; Support yaml and toml parsers other than dasel
;; Support alternative sqlite parser such as sqlite3 (https://github.com/pekingduck/emacs-sqlite3-api)
;; Support setuptools
;; Support updating buffer local variables when config change
;; Output progress when initing virtualenv
;; Support pipenv ????
;; Support pep 621 (flit/pdm) ????

;;; Code:


(require 'cl-lib)
(require 'filenotify)
(require 'let-alist)
(require 'pcase)
(require 'rx)
(require 'seq)
(require 'subr-x)

(defgroup python-x nil
  ""
  :group 'python
  :prefix "python-x-")

(defcustom python-x-yaml-parser "dasel"
  ""
  :group 'python-x
  :type '(choice (function :tag "Elisp function" 'yaml-parse-string)
                 (const :tag "dasel" "dasel")
                 (string :tag "Executable")))

(defcustom python-x-toml-parser "dasel"
  ""
  :group 'python-x
  :type '(choice (function :tag "Elisp function" 'toml:read-from-string)
                 (const :tag "dasel" "dasel")
                 (string :tag "Executable")))

(defcustom python-x-auto-install-virtual-environment nil
  ""
  :group 'python-x
  :type 'boolean)

(defun python-x-find-file-from-project-root (file-name)
  (when-let ((dir (locate-dominating-file
                   (or (and (functionp 'projectile-project-root)
                            (projectile-project-root))
                       default-directory)
                   file-name)))
    (expand-file-name (concat dir file-name))))

(defun python-x-parse-config-file (file-path)
  (condition-case err
      (with-temp-buffer
        (call-process "dasel" nil t nil "select" "-f" file-path "-w" "json")
        (json-parse-string (buffer-string) :object-type 'alist :array-type 'list))
    (error (minibuffer-message (error-message-string err)) nil)))

(defun python-x-parse-python-version (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (string-trim (buffer-string))))

(defvar python-x-watched-config-files nil)
(defun python-x-watch-config-file (config-file cache-var parser)
  (unless (assoc-default config-file python-x-watched-config-files)
    (push (cons config-file
                (file-notify-add-watch
                 config-file
                 '(change)
                 (lambda (event)
                   (pcase-let ((`(,_ ,action ,file ,@_)
                                event))
                     (pcase action
                       ((or 'deleted 'renamed)
                        (file-notify-rm-watch (assoc-default file python-x-watched-config-files))
                        (setf (symbol-value cache-var)
                              (assoc-delete-all file (symbol-value cache-var)))
                        (setf python-x-watched-config-files
                              (assoc-delete-all file python-x-watched-config-files)))
                       ('changed
                        (setf (alist-get file (symbol-value cache-var) nil t 'equal)
                              (funcall parser file))))))))
          python-x-watched-config-files)))

(defun python-x-pre-commit-config-file-path ()
  (python-x-find-file-from-project-root ".pre-commit-config.yaml"))

(defun python-x-pyproject-file-path ()
  (python-x-find-file-from-project-root "pyproject.toml"))

(defun python-x-python-version-file-path ()
  (python-x-find-file-from-project-root ".python-version"))

(cl-defmacro python-x-config-file-content (name &key file-path cache-var parser)
  `(defun ,name ()
     (let* ((config-file ,file-path)
            (cache-kvp (and config-file
                            (file-exists-p config-file)
                            (assoc config-file ,cache-var))))
       (if cache-kvp
           (cdr cache-kvp)
         (when config-file
           (python-x-watch-config-file config-file (quote ,cache-var) (quote ,parser))
           (when-let ((content (funcall (quote ,parser) config-file)))
             (push (cons config-file content) ,cache-var)
             content))))))

(defvar python-x-pre-commit-config-cache nil)
(python-x-config-file-content python-x-pre-commit-config
                              :file-path (python-x-pre-commit-config-file-path)
                              :cache-var python-x-pre-commit-config-cache
                              :parser python-x-parse-config-file)

(defvar python-x-pyproject-cache nil)
(python-x-config-file-content python-x-pyproject
                              :file-path (python-x-pyproject-file-path)
                              :cache-var python-x-pyproject-cache
                              :parser python-x-parse-config-file)

(defvar python-x-python-version-cache nil)
(python-x-config-file-content python-x-python-version
                              :file-path (python-x-python-version-file-path)
                              :cache-var python-x-python-version-cache
                              :parser python-x-parse-python-version)

(defun python-x-pre-commit-p (requirements)
  (and (python-x-pre-commit-config-file-path)
       (or (executable-find "pre-commit")
           (member "pre-commit" requirements))))

(defun python-x-use-poetry-p ()
  (when-let ((pyproject-path (python-x-pyproject-file-path)))
    (and
     (with-temp-buffer
       (insert-file-contents pyproject-path)
       (goto-char (point-min))
       (re-search-forward "^\\[tool.poetry\\]$" nil t nil))
     (executable-find "poetry")
     (if python-x-auto-install-virtual-environment
         (condition-case err
             (with-temp-buffer
               (or (zerop (call-process "poetry" nil t nil "env" "info" "-p"))
                   (zerop (call-process "poetry" nil t nil "install"))))
           (error (minibuffer-message (error-message-string err))
                  nil))
       t))))

(defun python-x-use-pyenv-p ()
  (and (python-x-python-version-file-path)
       (executable-find "pyenv")))

(defun python-x-requirements-from-file (anchor-dir file-path)
  (let ((requirements-file
         (if (and (null file-path) anchor-dir)
             (concat (file-name-as-directory anchor-dir) file-path)
           (expand-file-name file-path (or anchor-dir default-directory)))))
    (when (file-exists-p requirements-file)
      (with-temp-buffer
        (insert-file-contents requirements-file)
        (goto-char (point-min))
        (seq-remove 'string-empty-p
                    (mapcar 'string-trim
                            (string-lines (buffer-string) t)))))))

(defun python-x-resolve-requirements (anchor-dir file-path)
  (let ((requirements (python-x-requirements-from-file anchor-dir file-path)))
    (save-match-data
      (flatten-list
       (cl-loop for req in requirements
                collect (if (not (null (string-match
                                        (rx (or "-r" "-c" "--requirement" "--constraint")
                                            (+ space)
                                            (group (+ nonl)))
                                        req)))
                            (let ((file-path (match-string 1 req)))
                              (python-x-resolve-requirements
                               (file-name-directory
                                (expand-file-name file-path anchor-dir))
                               (file-name-nondirectory file-path)))
                          (python-x-parse-requirement-spec req)))))))

;; https://pip.pypa.io/en/stable/cli/pip_install/#requirements-file-format
(defun python-x-parse-requirement-spec (req)
  (save-match-data
    (cond ((not (null (string-match (rx (or "http" "file" "git" "git+ssh") "://") req))) ;; URL
           ;; https://datatracker.ietf.org/doc/html/rfc3987#section-2.2
           (rx-let ((ucschar (in "\uA000-\uD7FF"
                                 "\uF900-\uFDCF"
                                 "\uFDF0-\uFFEF"
                                 "\u10000-\u1FFFD"
                                 "\u20000-\u2FFFD"
                                 "\u30000-\u3FFFD"
                                 "\u40000-\u4FFFD"
                                 "\u50000-\u5FFFD"
                                 "\u60000-\u6FFFD"
                                 "\u70000-\u7FFFD"
                                 "\u80000-\u8FFFD"
                                 "\u90000-\u9FFFD"
                                 "\uA0000-\uAFFFD"
                                 "\uB0000-\uBFFFD"
                                 "\uC0000-\uCFFFD"
                                 "\uD0000-\uDFFFD"
                                 "\uE0000-\uEFFFD"))
                    (iprivate (in "\uE000-\uF8FF"
                                  "\uF0000-\uFFFFD"
                                  "\u100000-\u10FFFD"))
                    (pct-encoded (seq "%" (repeat 0 2 hex)))
                    (gen-delims (in "#/:?@[]"))
                    (sub-delims (in "!$&-,;="))
                    (reserved (or gen-delims sub-delims))
                    (unreserved (or alnum (in "-._~") ucschar))
                    (scheme (seq (+ alpha) (* (in alnum "+-."))))
                    (userinfo (* (or unreserved pct-encoded sub-delims ":")))
                    (ipvfuture (seq "v" (+ hex) "." (+ (or unreserved sub-delims ":"))))
                    (dec-octet (or digit
                                   (seq (in "1-9") digit)
                                   (seq "1" digit digit)
                                   (seq "2" (in "0-5") (in "0-5"))))
                    (ipv4address (seq dec-octet "." dec-octet "." dec-octet "." dec-octet))
                    (h16 (repeat 1 4 hex))
                    (ls32 (or (group  h16 ":" h16) ipv4address))
                    (ipv6address (or (seq (repeat 6 (seq  h16 ":")) ls32)
                                     (seq "::" (repeat 5 (seq h16 ":")) ls32)
                                     (seq (? h16) "::" (repeat 4 (seq h16 ":")) ls32)
                                     (seq (? (repeat 0 1 h16)) "::" (repeat 3 (seq h16 ":")) ls32)
                                     (seq (? (repeat 0 2 h16)) "::" (repeat 2 (seq h16 ":")) ls32)
                                     (seq (? (repeat 0 3 h16)) "::" h16 ":" ls32)
                                     (seq (? (repeat 0 4 h16)) "::" ls32)
                                     (seq (? (repeat 0 5 h16)) "::" h16)
                                     (seq (? (repeat 0 6 h16)) "::")))
                    (ip-literal (seq "[" (or ipv6address ipvfuture) "]"))
                    (reg-name (* (or unreserved pct-encoded sub-delims)))
                    (host (or ip-literal ipv4address reg-name))
                    (port (* digit))
                    (authority (seq (? userinfo "@") host (? ":" port)))
                    (pchar (or unreserved pct-encoded sub-delims ":" "@"))
                    (segment (* pchar))
                    (segment-nz (+ pchar))
                    (segment-nz-nc (+ (or unreserved pct-encoded sub-delims "@")))
                    (path-rootless (seq segment-nz (* "/" segment)))
                    (path-noscheme (seq segment-nz-nc (* "/" segment)))
                    (path-absolute (seq "/" (? path-rootless)))
                    (path-abempty (* "/" segment))
                    (path (or path-abempty path-absolute path-noscheme path-rootless))
                    (query (* (or pchar "/" "?" iprivate)))
                    (fragment query)
                    (url (seq scheme authority (? path) (? "?" query) (? "#" fragment))))

             (and (not (null (string-match (rx url) req))) (match-string 0 req))))
          ;; Path
          ((not (null (string-match (rx line-start (in "." "/")) req)))
           (when-let ((requirement-spec (car (split-string req (rx (or ";" "@" "--")))))
                      (requirement-spec (string-trim requirement-spec)))
             (when (not (string-empty-p requirement-spec))
               requirement-spec)))
          ;; TODO: fetch the package and read the setup.cfg or setup.py files to
          ;; extract dependencies
          ((not (null (string-match (rx (or "-e" "--editable") (+ space) (group (+ nonl)) eol) req)))
           nil)
          ;; Options
          ((not (null (string-match (rx "--" (+ (in alnum "-" "_"))) req)))
           nil)
          ;; Requirement specifiers
          (t (let* ((requirement-spec (split-string req (rx (or ";" "@" "--"))))
                    (package-version-spec (string-trim (car requirement-spec)))
                    (package
                     (car
                      (split-string
                       package-version-spec
                       (rx (or "==" "!=" "<=" ">=" "~=" "===" "<" ">"))))))
               (when package
                 (string-trim-right package (rx "[" (+ (in alnum "-" "_")) "]"))))))))

(defvar python-x-project-requirements-cache nil)
(defun python-x-project-requirements ()
  (let* ((file-path (buffer-file-name))
         (cache-kvp (alist-get file-path python-x-project-requirements-cache nil nil 'equal))
         (requirements (cdr cache-kvp)))
    (unless (or cache-kvp requirements)
      (let ((pip-args '("list" "--format=freeze" "--disable-pip-version-check")))
        (setf requirements
              (mapcar 'python-x-parse-requirement-spec
                      (or (and (python-x-use-poetry-p)
                               (condition-case err
                                   (apply 'process-lines "poetry" (append '("run" "pip") pip-args))
                                 (error (minibuffer-message (error-message-string err)) nil)))
                          (and (python-x-use-pyenv-p)
                               (condition-case err
                                   (apply 'process-lines "pyenv" (append '("exec" "pip") pip-args))
                                 (error (minibuffer-message (error-message-string err)) nil)))
                          (when-let ((project-root-requirements-file (python-x-find-file-from-project-root "requirements.txt")))
                            (python-x-resolve-requirements
                             (file-name-directory project-root-requirements-file)
                             project-root-requirements-file))
                          (and (executable-find "pip")
                               (condition-case err
                                   (apply 'process-lines "pip" pip-args)
                                 (error (minibuffer-message (error-message-string err)) nil))))))
        (setf python-x-project-requirements-cache
              (assoc-delete-all file-path python-x-project-requirements-cache))
        (when requirements
          (push (cons file-path requirements) python-x-project-requirements-cache))))
    requirements))

(defun python-x-pre-commit-config-has-hook-p (id)
  (member id
          (flatten-list
           (cl-loop for repo in (let-alist (python-x-pre-commit-config) .repos)
                    collect (cl-loop for hook in (let-alist repo .hooks)
                                     collect (let-alist hook .id))))))

(defun python-x-parse-pre-commit-db (db-file)
  (condition-case err
      (with-temp-buffer
        (call-process "sqlite3" nil t nil "-json" db-file "select * from repos")
        (json-parse-string (buffer-string) :object-type 'alist :array-type 'list))
    (error (minibuffer-message (error-message-string err)) nil)))

(defvar python-x-pre-commit-database-cache nil)

(defun python-x-pre-commit-virtualenv-path (hook-id)
  (when-let* ((db-file
               (concat
                (expand-file-name
                 (file-name-as-directory
                  (or (getenv "PRE_COMMIT_HOME")
                      (getenv "XDG_CACHE_HOME")
                      "~/.cache/")))
                "pre-commit/db.db"))

              (db
               (or (assoc-default db-file python-x-pre-commit-database-cache)
                   (when (file-exists-p db-file)
                     (python-x-watch-config-file db-file 'python-x-pre-commit-database-cache 'python-x-parse-pre-commit-db)
                     (when-let ((content (python-x-parse-pre-commit-db db-file)))
                       (push (cons db-file content) python-x-pre-commit-database-cache)
                       content))))

              (repo-config
               (seq-find
                (lambda (repo)
                  (seq-find
                   (lambda (hook)
                     (equal (let-alist hook .id) hook-id))
                   (let-alist repo .hooks)))
                (let-alist (python-x-pre-commit-config) .repos)))

              (repo-url
               (let ((additional-deps
                      (let-alist repo-config
                        (let-alist (seq-find (lambda (hook) (let-alist hook (equal .id hook-id))) .hooks)
                          .additional_dependencies))))
                 (concat (let-alist repo-config .repo)
                         (if additional-deps
                             (concat ":" (string-join (sort additional-deps 'string<) ","))))))

              (repo-dir
               (let-alist (seq-find
                           (lambda (row)
                             (let-alist row
                               (and (equal .repo repo-url)
                                    (equal .ref (let-alist repo-config .rev)))))
                           db)
                 .path)))

    (car
     (last
      (file-expand-wildcards
       (concat (file-name-as-directory repo-dir) "py_env-*")
       t)))))

(defun python-x-pre-commit-ensure-virtualenv-path (hook-id)
  (let ((venv-path (python-x-pre-commit-virtualenv-path hook-id)))
    (when (and (not venv-path) python-x-auto-install-virtual-environment)
      (condition-case err
          (when (cond ((python-x-use-poetry-p)
                       (with-temp-buffer
                         (zerop (call-process "poetry" nil t nil "run" "pre-commit" "install" "--install-hooks"))))
                      ((executable-find "pre-commit")
                       (with-temp-buffer
                         (zerop (call-process "pre-commit" nil t nil "install" "--install-hooks"))))
                      (t (user-error "command pre-commit not found.")))
            (setf venv-path (python-x-pre-commit-virtualenv-path hook-id)))
        (error (minibuffer-message (error-message-string err)))))
    venv-path))

(defun python-x-cleanup-watchers-and-caches ()
  (when (and (buffer-file-name)
             (derived-mode-p 'python-mode))
    (let* ((buf-file (buffer-file-name))
           (root (and (functionp 'projectile-project-root)
                      (projectile-project-root (file-name-directory buf-file)))))

      (when (and root
                 (not (seq-some (lambda (buf)
                                  (and (not (equal buf (current-buffer)))
                                       (string-prefix-p root (buffer-file-name buf))))
                                (buffer-list))))

        (pcase-dolist (`(,config-file . ,watcher) python-x-watched-config-files)
          (when (string-prefix-p root config-file)
            (file-notify-rm-watch watcher)
            (setf python-x-watched-config-files
                  (assoc-delete-all config-file python-x-watched-config-files))))

        (dolist (cache '(python-x-pre-commit-config-cache
                         python-x-pyproject-cache
                         python-x-python-version-cache))
          (pcase-dolist (`(,config-file . ,_) (symbol-value cache))
            (when (string-prefix-p root config-file)
              (setf (symbol-value cache)
                    (assoc-delete-all config-file (symbol-value cache)))))))

      (setf python-x-project-requirements-cache
            (assoc-delete-all buf-file python-x-project-requirements-cache)))))

(add-hook 'kill-buffer-hook 'python-x-cleanup-watchers-and-caches)



;;;###autoload
(defun python-x-executable-find (executable)
  (let ((is-python (string-prefix-p "python" executable))
        (requirements (python-x-project-requirements)))
    (cond ((and (python-x-pre-commit-p requirements)
                (python-x-pre-commit-config-has-hook-p executable)
                (not is-python))
           (concat (python-x-pre-commit-ensure-virtualenv-path executable) "/bin/" executable))
          ((and (python-x-use-poetry-p)
                (or is-python (member executable requirements)))
           (condition-case err
               (with-temp-buffer
                 (call-process "poetry" nil t nil "run" "which" executable)
                 (string-trim (buffer-string)))
             (error (minibuffer-message (error-message-string err)) nil)))
          ((and (python-x-use-pyenv-p)
                (or is-python (member executable requirements)))
           (condition-case err
               (with-temp-buffer
                 (if (zerop (call-process "pyenv" nil t nil "which" executable))
                     (string-trim (buffer-string))
                   (with-temp-buffer
                     (call-process "pyenv" nil t nil "prefix")
                     (user-error "%s not found under %s" executable (string-trim (buffer-string))))))
             (error (minibuffer-message (error-message-string err)) nil)))
          (t (when-let ((path (executable-find executable)))
               (condition-case nil
                   (if (and (executable-find "pyenv")
                            (member path (process-lines "pyenv" "shims")))
                       nil
                     path)
                 (error nil)))))))

;;;###autoload
(defun python-x-virtualenv-root ()
  (condition-case err
      (with-temp-buffer
        (cond ((python-x-use-poetry-p)
               (call-process "poetry" nil t nil "env" "info" "--path"))
              ((python-x-use-pyenv-p)
               (call-process "pyenv" nil t nil "prefix")))
        (file-truename (string-trim (buffer-string))))
    (error (minibuffer-message (error-message-string err)))))



(defun python-x-flycheck-python-pylint-find-pylintrc ()
  (let ((pylintrc '("pylintrc" ".pylintrc" "pyproject.toml" "setup.cfg")))
    (or (when-let ((pylintrc (seq-find
                              (lambda (file) (file-exists-p (concat default-directory file)))
                              pylintrc)))
          (expand-file-name (concat default-directory pylintrc)))
        (and (file-exists-p (concat (file-name-directory (buffer-file-name)) "__init__.py"))
             (when-let ((pylintrc (seq-find
                                   (apply-partially 'locate-dominating-file default-directory)
                                   pylintrc)))
               (expand-file-name (concat (locate-dominating-file default-directory pylintrc) pylintrc))))
        (and (getenv "PYLINTRC")
             (expand-file-name (getenv "PYLINTRC")))
        (when-let ((config-dir
                    (or (and (getenv "XDG_CONFIG_HOME")
                             (file-name-as-directory (getenv "XDG_CONFIG_HOME")))
                        "~/.config/")))
          (expand-file-name (concat config-dir "pylintrc")))
        (let ((home-dir-pylintrc (expand-file-name "~/.pylintrc")))
          (and (file-exists-p home-dir-pylintrc) home-dir-pylintrc))
        (and (file-exists-p "/etc/pylintrc")
             "/etc/pylintrc"))))

(defvar flycheck-flake8rc)
(defvar flycheck-python-mypy-config)
(defvar python-shell-interpreter)

;;;###autoload
(defun python-x-flycheck-setup ()
  (setq flycheck-flake8rc `(".flake8" "setup.cfg" "tox.ini"))

  (setq flycheck-python-mypy-config `("mypy.ini" ".mypy.ini" "pyproject.toml" "setup.cfg"
                                      ,(concat (expand-file-name
                                                (or (and (getenv "XDG_CONFIG_HOME")
                                                         (file-name-as-directory (getenv "XDG_CONFIG_HOME")))
                                                    "~/.config/"))
                                               "mypy/config")))

  (add-hook 'flycheck-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-mode)
                (setq-local flycheck-pylintrc (python-x-flycheck-python-pylint-find-pylintrc)
                            flycheck-python-flake8-executable (python-x-executable-find "flake8")
                            flycheck-python-pylint-executable (python-x-executable-find "pylint")
                            flycheck-python-mypy-executable (python-x-executable-find "mypy")
                            flycheck-python-pyright-executable (python-x-executable-find "pyright")
                            flycheck-python-pycompile-executable python-shell-interpreter)))))

(defun python-x-flycheck-teardown ()
  (kill-local-variable 'flycheck-pylintrc)
  (kill-local-variable 'flycheck-python-flake8-executable)
  (kill-local-variable 'flycheck-python-pylint-executable)
  (kill-local-variable 'flycheck-python-mypy-executable)
  (kill-local-variable 'flycheck-python-pyright-executable)
  (kill-local-variable 'flycheck-python-pycompile-executable))



(defun python-x-python-mode-hook-function ()
  (setq-local python-shell-interpreter (python-x-executable-find "python")
              python-shell-virtualenv-root (python-x-virtualenv-root))

  (with-eval-after-load 'flycheck
    (python-x-flycheck-setup))

  (with-eval-after-load 'lsp-jedi
    (setq-local lsp-jedi-executable-command (python-x-executable-find "jedi-language-server")))

  (with-eval-after-load 'lsp-pyright
    (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter
                lsp-pyright-venv-path python-shell-virtualenv-root))

  (with-eval-after-load 'dap-python
    (setq-local dap-python-executable python-shell-interpreter))

  (with-eval-after-load 'python-pytest
    (setq-local python-pytest-executable (python-x-executable-find "pytest")))

  (with-eval-after-load 'python-black
    (setq-local python-black-command (python-x-executable-find "black")))

  (with-eval-after-load 'python-isort
    (setq-local python-isort-command (python-x-executable-find "isort"))))

(defun python-x-cleanup-buffer-local-vars ()
  (kill-local-variable 'python-shell-interpreter)
  (kill-local-variable 'python-shell-virtualenv-root)

  (with-eval-after-load 'flycheck
    (python-x-flycheck-teardown))

  (with-eval-after-load 'lsp-jedi
    (kill-local-variable 'lsp-jedi-executable-command))

  (with-eval-after-load 'lsp-pyright
    (kill-local-variable 'lsp-pyright-python-executable-cmd)
    (kill-local-variable 'lsp-pyright-venv-path))

  (with-eval-after-load 'dap-python
    (kill-local-variable 'dap-python-executable))

  (with-eval-after-load 'python-pytest
    (kill-local-variable 'python-pytest-executable))

  (with-eval-after-load 'python-black
    (kill-local-variable 'python-black-command))

  (with-eval-after-load 'python-isort
    (kill-local-variable 'python-isort-command)))

;;;###autoload
(define-minor-mode python-x-minor-mode
  ""
  :lighter "PyX"
  :group 'python-x
  (if python-x-minor-mode
      (add-hook 'python-mode-hook #'python-x-python-mode-hook-function)
    (remove-hook 'python-mode-hook #'python-x-python-mode-hook-function)
    (python-x-cleanup-watchers-and-caches)
    (python-x-cleanup-buffer-local-vars)))

(defun python-x-minor-mode-on ()
  (when (derived-mode-p 'python-mode)
    (python-x-minor-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-python-x-minor-mode python-x-minor-mode python-x-minor-mode-on
  :group 'python-x)



(add-to-list 'auto-mode-alist '("\\.pythonrc\\'"   . python-mode))
(add-to-list 'auto-mode-alist '("\\.pylintrc\\'"   . conf-mode))
(add-to-list 'auto-mode-alist '("\\.flake8\\'"     . conf-mode))
(add-to-list 'auto-mode-alist '("\\poetry.lock\\'" . conf-toml-mode))

(provide 'python-x)

;;; python-x.el ends here
