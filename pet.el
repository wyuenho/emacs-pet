;;; pet.el --- Executable and virtualenv tracker for python-mode -*- lexical-binding: t -*-

;; Author: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Maintainer: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (f "0.20.0"))
;; Homepage: https://github.com/wyuenho/emacs-pet/
;; Keywords: tools


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
;; Support updating buffer local variables when config change

;;; Code:


(require 'cl-lib)
(require 'f)
(require 'filenotify)
(require 'let-alist)
(require 'pcase)
(require 'project)
(require 'python)
(require 'seq)
(require 'subr-x)

(when (< emacs-major-version 27)
  (require 'json))

(defgroup pet nil
  "Customization group for `pet'."
  :group 'python
  :prefix "pet-")

(defcustom pet-debug nil
  "Whether to turn on debug messages."
  :group 'pet
  :type 'boolean)

(defcustom pet-toml-to-json-program "dasel"
  "Name of the program to convert TOML to JSON.

The program must accept input from STDIN and output a JSON to
STDOUT.

You can customize the arguments that will be passed to the
program by adjusting `pet-toml-to-json-program-arguments'"
  :group 'pet
  :type '(choice (const "dasel")
                 (const "tomljson")
                 (string :tag "Other")))

(defcustom pet-toml-to-json-program-arguments '("select" "-f" "-" "-p" "toml" "-w" "json")
  "Arguments for `pet-toml-to-json-program'."
  :group 'pet
  :type '(repeat string))

(defcustom pet-yaml-to-json-program "dasel"
  "Name of the program to convert YAML to JSON.

The program must accept input from STDIN and output a JSON to
STDOUT.

You can customize the arguments that will be passed to the
program by adjusting `pet-yaml-to-json-program-arguments'"
  :group 'pet
  :type '(choice (const "dasel")
                 (const "yq")
                 (string :tag "Other")))

(defcustom pet-yaml-to-json-program-arguments '("select" "-f" "-" "-p" "yaml" "-w" "json")
  "Arguments for `pet-yaml-to-json-program'."
  :group 'pet
  :type '(repeat string))



(defun pet-system-bin-dir ()
  "Determine the correct script directory based on `system-type'."
  (if (eq system-type 'windows-nt) "Scripts" "bin"))

(defun pet-report-error (err)
  "Report ERR to the minibuffer.

Only reports to the minibuffer if `pet-debug' is non-nil."
  (when pet-debug
    (minibuffer-message (error-message-string err)))
  nil)

(defun pet-project-root ()
  "Return the path of root of the project.

If `projectile' is available, the function
`projectile-project-root' is used to find the project root.
Otherwise, `project-root' is used."
  (or (and (functionp 'projectile-project-root)
           (projectile-project-root))
      (when-let ((project (project-current)))
        (or (and (functionp 'project-root)
                 (expand-file-name (project-root project)))
            (and (functionp 'project-roots)
                 (when-let ((root (car (project-roots project))))
                   (expand-file-name root)))))))

(defun pet-find-file-from-project-root (file-name)
  "Find FILE-NAME from project root.

Returns absolute path to FILE-NAME if found from in the root of
the project or its ancestor directories, nil otherwise."
  (when-let ((dir (locate-dominating-file
                   (or (pet-project-root)
                       default-directory)
                   file-name)))
    (expand-file-name (concat (file-name-as-directory dir) file-name))))

(defun pet-parse-json (str)
  "Parse JSON STR to an alist.  Arrays are converted to lists."
  (if (functionp 'json-parse-string)
      (json-parse-string str :object-type 'alist :array-type 'list)
    (let ((json-array-type 'list))
      (json-read-from-string str))))

(defun pet-parse-config-file (file-path)
  "Parse a configuration file at FILE-PATH into JSON."
  (condition-case err
      (let* ((ext (downcase (or (file-name-extension file-path) "")))
             (file-name (file-name-nondirectory file-path))
             (auto-mode-alist-matcher (lambda (a b) (not (null (string-match-p a b)))))
             (toml-p (or (equal ext "toml")
                         (eq 'conf-toml-mode
                             (alist-get
                              file-name
                              auto-mode-alist
                              nil
                              nil
                              auto-mode-alist-matcher))))
             (yaml-p (or (not (null (string-match-p "ya?ml" ext)))
                         (eq 'yaml-mode
                             (alist-get
                              file-name
                              auto-mode-alist
                              nil
                              nil
                              auto-mode-alist-matcher)))))
        (with-temp-buffer
          (insert-file-contents file-path)
          (apply 'call-process-region
                 (point-min)
                 (point-max)
                 (cond (toml-p pet-toml-to-json-program)
                       (yaml-p pet-yaml-to-json-program))
                 t
                 t
                 nil
                 (cond (toml-p pet-toml-to-json-program-arguments)
                       (yaml-p pet-yaml-to-json-program-arguments)))
          (pet-parse-json (buffer-string))))
    (error (pet-report-error err))))

(defvar pet-watched-config-files nil)
(defun pet-watch-config-file (config-file cache-var parser)
  "Keep cache fresh by watching for changes in the config file.

CONFIG-FILE is the path to the configuration file to watch for
changes.  CACHE-VAR is the symbol to the variable where the
parsed configuration file content is stored.  PARSER is the
symbol to a function that takes a file path and parses its
content into an alist."
  (unless (assoc-default config-file pet-watched-config-files)
    (push (cons config-file
                (file-notify-add-watch
                 config-file
                 '(change)
                 (lambda (event)
                   (pcase-let ((`(,_ ,action ,file ,@_)
                                event))
                     (pcase action
                       ((or 'deleted 'renamed)
                        (file-notify-rm-watch (assoc-default file pet-watched-config-files))
                        (setf (alist-get file (symbol-value cache-var) nil t 'equal) nil)
                        (setf (alist-get file pet-watched-config-files nil t 'equal) nil))
                       ('changed
                        (setf (alist-get file (symbol-value cache-var) nil t 'equal)
                              (funcall parser file))))))))
          pet-watched-config-files)))

(cl-defmacro pet-def-config-accessor (name &key file-name parser)
  "Create a function for reading the content of a config file.

NAME is the name of the memorized function that will be created
to return the content of the configuration file FILE-NAME.
FILE-NAME is the name of the configuration file that will be
searched in the project.  The content of the file will be parsed
by PARSER and then cached in a variable called NAME-cache.

Changes to the file will automatically update the cached content
See `pet-watch-config-file' for details."
  (let ((cache-var (intern (concat (symbol-name name) "-cache"))))
    `(progn
       (defvar ,cache-var nil)
       (defun ,name ()
         (let* ((config-file (pet-find-file-from-project-root ,file-name))
                (cache-kvp (and config-file
                                (assoc config-file ,cache-var))))
           (if cache-kvp
               (cdr cache-kvp)
             (when config-file
               (pet-watch-config-file config-file (quote ,cache-var) (quote ,parser))
               (when-let ((content (funcall (quote ,parser) config-file)))
                 (push (cons config-file content) ,cache-var)
                 content))))))))

(pet-def-config-accessor pet-pre-commit-config
                         :file-name ".pre-commit-config.yaml"
                         :parser pet-parse-config-file)

(pet-def-config-accessor pet-pyproject
                         :file-name "pyproject.toml"
                         :parser pet-parse-config-file)

(pet-def-config-accessor pet-python-version
                         :file-name ".python-version"
                         :parser f-read-text)

(pet-def-config-accessor pet-pipfile
                         :file-name "Pipfile"
                         :parser pet-parse-config-file)

(defun pet-use-pre-commit-p ()
  "Whether the current project is using `pre-commit'."
  (and (pet-pre-commit-config)
       (or (executable-find "pre-commit")
           (and (pet-use-poetry-p)
                (when-let* ((venv (pet-virtualenv-root))
                            (exec-path (list (concat (file-name-as-directory venv) (pet-system-bin-dir)))))
                  (executable-find "pre-commit"))))))

(defun pet-use-poetry-p ()
  "Whether the current project is using `poetry'."
  (and (executable-find "poetry")
       (not
        (null
         (string-match-p
          "poetry"
          (or (let-alist (pet-pyproject)
                .build-system.build-backend)
              ""))))))

(defun pet-use-pyenv-p ()
  "Whether the current project is using `pyenv'."
  (and (pet-python-version)
       (executable-find "pyenv")))

(defun pet-use-pipenv-p ()
  "Whether the current project is using `pipenv'."
  (and (pet-pipfile)
       (executable-find "pipenv")))

(defun pet-pre-commit-config-has-hook-p (id)
  "Determine if the `pre-commit' configuration has a hook.

Return non-nil if the `pre-commit' configuration for the current
project has hook ID set up."
  (member id (cl-loop for repo in (let-alist (pet-pre-commit-config) .repos)
                      append (cl-loop for hook in (let-alist repo .hooks)
                                      collect (let-alist hook .id)))))

(defun pet-parse-pre-commit-db (db-file)
  "Parse `pre-commit' database.

Parse the pre-commit SQLite database located at DB-FILE to JSON."
  (condition-case err
      (with-temp-buffer
        (call-process "sqlite3" nil t nil "-json" db-file "select * from repos")
        (pet-parse-json (buffer-string)))
    (error (pet-report-error err))))

(defvar pet-pre-commit-database-cache nil)

(defun pet-pre-commit-virtualenv-path (hook-id)
  "Find the virtualenv location from the `pre-commit' database.

If the `pre-commit' hook HOOK-ID is found in the current Python
project's `.pre-commit-config.yaml' file, the hook ID and its
additional dependencies are used to construct a key for looking
up a virtualenv for the hook from the pre-commit database.

In order to find the hook virtualenv, `pre-commit' and the hooks
must both be installed into the current project first."
  (when-let* ((db-file
               (concat
                (expand-file-name
                 (file-name-as-directory
                  (or (getenv "PRE_COMMIT_HOME")
                      (getenv "XDG_CACHE_HOME")
                      "~/.cache/")))
                "pre-commit/db.db"))

              (db
               (or (assoc-default db-file pet-pre-commit-database-cache)
                   (when (file-exists-p db-file)
                     (pet-watch-config-file db-file 'pet-pre-commit-database-cache 'pet-parse-pre-commit-db)
                     (when-let ((content (pet-parse-pre-commit-db db-file)))
                       (push (cons db-file content) pet-pre-commit-database-cache)
                       content))))

              (repo-config
               (seq-find
                (lambda (repo)
                  (seq-find
                   (lambda (hook)
                     (equal (let-alist hook .id) hook-id))
                   (let-alist repo .hooks)))
                (let-alist (pet-pre-commit-config) .repos)))

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



;;;###autoload
(defun pet-executable-find (executable)
  "Find the correct EXECUTABLE for the current Python project.

Search for EXECUTABLE first in the `pre-commit' virtualenv, then
`poetry', then finally from the variable `exec-path'.

The executable will only be searched in an environment created by
a Python virtualenv management tool if the project is setup to
use it."
  (cond ((and (pet-use-pre-commit-p)
              (not (string-prefix-p "python" executable))
              (condition-case err
                  (if (not (pet-pre-commit-config-has-hook-p executable))
                      (user-error "`pre-commit' does not have hook %s configured" executable)
                    (when-let* ((venv (pet-pre-commit-virtualenv-path executable))
                                (bin-path (concat (file-name-as-directory venv) (pet-system-bin-dir) "/" executable)))
                      (if (file-exists-p bin-path)
                          bin-path
                        (user-error "`pre-commit' is configured but the hook %s do not appear to be installed" executable))))
                (error (pet-report-error err)))))
        ((when-let* ((venv (pet-virtualenv-root))
                     (exec-path (list (concat (file-name-as-directory venv) (pet-system-bin-dir)))))
           (executable-find executable)))
        ((when-let ((path (executable-find executable)))
           (condition-case nil
               (if (and (executable-find "pyenv")
                        (member path (process-lines "pyenv" "shims")))
                   nil
                 path)
             (error nil))))))

(defvar pet-project-virtualenv-cache nil)

;;;###autoload
(defun pet-virtualenv-root ()
  "Find the path to the virtualenv for the current Python project.

Selects a virtualenv in the follow order:

1. If the current project is using `poetry', return the absolute path to the
   virtualenv directory `poetry' created.
2. Ditto for `pipenv'.
3. The value of the environment variable `VIRTUAL_ENV' if defined.
4. The `.venv' or `venv' directory in the project root if found.
5. If the current project is using `pyenv', return the path to the virtualenv
   directory by looking up the prefix from `.python-version'."
  (let ((root (pet-project-root)))
    (cond ((alist-get root pet-project-virtualenv-cache nil nil 'equal))
          ((getenv "VIRTUAL_ENV")
           (expand-file-name (getenv "VIRTUAL_ENV")))
          ((pet-use-poetry-p)
           (condition-case err
               (with-temp-buffer
                 (let ((exit-code (call-process "poetry" nil t nil "env" "info" "--no-ansi" "--path"))
                       (output (string-trim (buffer-string))))
                   (if (zerop exit-code)
                       (setf (alist-get root pet-project-virtualenv-cache nil nil 'equal) output)
                     (user-error (buffer-string)))))
             (error (pet-report-error err))))
          ((pet-use-pipenv-p)
           (condition-case err
               (with-temp-buffer
                 (let ((exit-code (call-process "pipenv" nil t nil "--venv"))
                       (output (string-trim (buffer-string))))
                   (if (zerop exit-code)
                       (setf (alist-get root pet-project-virtualenv-cache nil nil 'equal) (file-truename output))
                     (user-error (buffer-string)))))
             (error (pet-report-error err))))
          ((cl-loop for d in (mapcar (apply-partially 'concat root) '(".venv" "venv"))
                    if (file-exists-p d)
                    return d))
          ((pet-use-pyenv-p)
           (condition-case err
               (with-temp-buffer
                 (let ((exit-code (call-process "pyenv" nil t nil "prefix"))
                       (output (string-trim (buffer-string))))
                   (if (zerop exit-code)
                       (setf (alist-get root pet-project-virtualenv-cache nil nil 'equal) (file-truename output))
                     (user-error (buffer-string)))))
             (error (pet-report-error err)))))))



(defun pet-flycheck-python-pylint-find-pylintrc ()
  "Polyfill `flycheck-pylintrc'.

Find the correct `pylint' configuration file according to the
algorithm described at
`https://pylint.pycqa.org/en/latest/user_guide/usage/run.html'."
  (let ((pylintrc '("pylintrc" ".pylintrc" "pyproject.toml" "setup.cfg")))
    (cond ((cl-loop for f in pylintrc
                    with path = nil
                    do (setq path (concat default-directory f))
                    if (file-exists-p path)
                    return path))
          ((file-exists-p (concat (file-name-directory (buffer-file-name)) "__init__.py"))
           (when-let ((path (cl-loop for f in pylintrc
                                     with dir = nil
                                     do (setq dir (locate-dominating-file default-directory f))
                                     if dir
                                     return (concat dir f))))
             (expand-file-name path)))
          ((getenv "PYLINTRC")
           (expand-file-name (getenv "PYLINTRC")))
          ((when-let ((config-dir
                       (or (and (getenv "XDG_CONFIG_HOME")
                                (file-name-as-directory (getenv "XDG_CONFIG_HOME")))
                           "~/.config/")))
             (expand-file-name (concat config-dir "pylintrc"))))
          ((let ((home-dir-pylintrc (expand-file-name "~/.pylintrc")))
             (and (file-exists-p home-dir-pylintrc) home-dir-pylintrc)))
          ((file-exists-p "/etc/pylintrc")
           "/etc/pylintrc"))))

(defvar flycheck-mode)
(defvar flycheck-flake8rc)
(defvar flycheck-python-mypy-config)
(defvar flycheck-pylintrc)
(defvar flycheck-python-flake8-executable)
(defvar flycheck-python-pylint-executable)
(defvar flycheck-python-mypy-executable)
(defvar flycheck-python-mypy-python-executable)
(defvar flycheck-python-pyright-executable)
(defvar flycheck-python-pycompile-executable)
(declare-function flycheck-checker-get "ext:flycheck")
(declare-function flycheck-string-or-nil-p "ext:flycheck")
(declare-function flycheck-register-option-var "ext:flycheck")

;; https://github.com/flycheck/flycheck/pull/1955
(defvar pet-flycheck-checker-props
  '((python-mypy . ((command . ("mypy"
                                "--show-column-numbers"
                                (config-file "--config-file" flycheck-python-mypy-config)
                                (option "--python-executable" flycheck-python-mypy-python-executable)
                                (option "--cache-dir" flycheck-python-mypy-cache-dir)
                                source-original)))))
  "`flycheck' Python checker property overrides.")

(defun pet-flycheck-checker-get-advice (fn checker property)
  "Advice `flycheck-checker-get'.

This function installs an advice to `flycheck-checker-get' to
redirect supported `flycheck' Python checker property lookups to
those defined in `pet-flycheck-check-props'.

FN is `flycheck-check-get', CHECKER is a `flycheck' Python
checker symbol, and PROPERTY is the checker property.  See
`flycheck-define-generic-checker' for details."
  (or (alist-get property (alist-get checker pet-flycheck-checker-props))
      (funcall fn checker property)))

(defun pet-flycheck-toggle-local-vars ()
  "Toggle buffer local variables for `flycheck' Python checkers.

When `flycheck-mode' is non-nil, set up all supported Python
checker executable variables buffer-locally.  Reset them to
default otherwise."
  (if flycheck-mode
      (progn
        (when (derived-mode-p 'python-mode)
          (setq-local flycheck-pylintrc (pet-flycheck-python-pylint-find-pylintrc))
          (setq-local flycheck-python-flake8-executable (pet-executable-find "flake8"))
          (setq-local flycheck-python-pylint-executable (pet-executable-find "pylint"))
          (setq-local flycheck-python-mypy-executable (pet-executable-find "mypy"))
          (setq-local flycheck-python-mypy-python-executable (pet-executable-find "python"))
          (setq-local flycheck-python-pyright-executable (pet-executable-find "pyright"))
          (setq-local flycheck-python-pycompile-executable python-shell-interpreter)))
    (kill-local-variable 'flycheck-pylintrc)
    (kill-local-variable 'flycheck-python-flake8-executable)
    (kill-local-variable 'flycheck-python-pylint-executable)
    (kill-local-variable 'flycheck-python-mypy-executable)
    (kill-local-variable 'flycheck-python-pyright-executable)
    (kill-local-variable 'flycheck-python-pycompile-executable)))

;;;###autoload
(defun pet-flycheck-setup ()
  "Setup all `flycheck' Python checker configuration."

  ;; https://github.com/flycheck/flycheck/pull/1956
  (setq flycheck-flake8rc `(".flake8" "setup.cfg" "tox.ini"))

  (setq flycheck-python-mypy-config `("mypy.ini" ".mypy.ini" "pyproject.toml" "setup.cfg"
                                      ,(concat (expand-file-name
                                                (or (and (getenv "XDG_CONFIG_HOME")
                                                         (file-name-as-directory (getenv "XDG_CONFIG_HOME")))
                                                    "~/.config/"))
                                               "mypy/config")))

  ;; https://github.com/flycheck/flycheck/pull/1955
  (defcustom flycheck-python-mypy-python-executable nil
    "Python executable to find the installed PEP 561 packages.

This variable is an option for the following syntax checkers:

  - `python-mypy'"
    :group 'flycheck-options
    :type '(choice (const :tag "Same as mypy's" nil)
                   (string :tag "Path"))
    :safe 'flycheck-string-or-nil-p)

  (flycheck-register-option-var 'flycheck-python-mypy-python-executable 'python-mypy)

  (when (functionp 'flycheck-checker-get)
    (advice-add 'flycheck-checker-get :around #'pet-flycheck-checker-get-advice))

  (add-hook 'flycheck-mode-hook #'pet-flycheck-toggle-local-vars))

;;;###autoload
(defun pet-flycheck-teardown ()
  "Reset all `flycheck' Python checker configuration to default."

  (when (functionp 'flycheck-checker-get)
    (advice-remove 'flycheck-checker-get #'pet-flycheck-checker-get-advice))
  (remove-hook 'flycheck-mode-hook #'pet-flycheck-toggle-local-vars)
  (kill-local-variable 'flycheck-pylintrc)
  (kill-local-variable 'flycheck-python-flake8-executable)
  (kill-local-variable 'flycheck-python-pylint-executable)
  (kill-local-variable 'flycheck-python-mypy-executable)
  (kill-local-variable 'flycheck-python-pyright-executable)
  (kill-local-variable 'flycheck-python-pycompile-executable))



(defvar lsp-jedi-executable-command)
(defvar lsp-pyright-python-executable-cmd)
(defvar lsp-pyright-venv-path)
(defvar dap-python-executable)
(defvar python-pytest-executable)
(defvar python-black-command)
(defvar python-isort-command)
(defvar blacken-executable)
(defvar yapfify-executable)

(defun pet-buffer-local-vars-setup ()
  "Setup the buffer local variables for Python tools.

Assign all supported Python tooling executable variables to
buffer local values."
  (setq-local python-shell-interpreter (pet-executable-find "python"))
  (setq-local python-shell-virtualenv-root (pet-virtualenv-root))

  (pet-flycheck-setup)

  (setq-local lsp-jedi-executable-command
              (pet-executable-find "jedi-language-server"))
  (setq-local lsp-pyright-venv-path python-shell-virtualenv-root)
  (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter)
  (setq-local dap-python-executable python-shell-interpreter)
  (setq-local python-pytest-executable (pet-executable-find "pytest"))
  (setq-local python-black-command (pet-executable-find "black"))
  (setq-local python-isort-command (pet-executable-find "isort"))
  (setq-local blacken-executable python-black-command)
  (setq-local yapfify-executable (pet-executable-find "yapf")))

(defun pet-buffer-local-vars-teardown ()
  "Reset all supported buffer local variable values to default."

  (kill-local-variable 'python-shell-interpreter)
  (kill-local-variable 'python-shell-virtualenv-root)

  (pet-flycheck-teardown)

  (kill-local-variable 'lsp-jedi-executable-command)
  (kill-local-variable 'lsp-pyright-venv-path)
  (kill-local-variable 'lsp-pyright-python-executable-cmd)
  (kill-local-variable 'dap-python-executable)
  (kill-local-variable 'python-pytest-executable)
  (kill-local-variable 'python-black-command)
  (kill-local-variable 'python-isort-command)
  (kill-local-variable 'blacken-executable)
  (kill-local-variable 'yapfify-executable))

(defun pet-verify-setup ()
  "Verify the values of buffer local variables visually.

Print all of the buffer local variable values `pet-minor-mode'
has assigned to."
  (interactive)

  (unless (derived-mode-p 'python-mode)
    (user-error "You are not in python-mode!"))

  (with-output-to-temp-buffer "*pet*"
    (mapc (lambda (var)
            (princ (format "%-40s" (concat (symbol-name var) ":")))
            (prin1 (symbol-value var))
            (terpri))
          '(python-shell-interpreter
            python-shell-virtualenv-root
            flycheck-flake8rc
            flycheck-python-flake8-executable
            flycheck-pylintrc
            flycheck-python-pylint-executable
            flycheck-python-mypy-executable
            flycheck-python-mypy-config
            flycheck-python-mypy-python-executable
            flycheck-python-pyright-executable
            flycheck-python-pycompile-executable
            lsp-jedi-executable-command
            lsp-pyright-python-executable-cmd
            lsp-pyright-venv-path
            dap-python-executable
            python-pytest-executable
            python-black-command
            blacken-executable
            python-isort-command
            yapfify-executable)))

  (select-window (get-buffer-window "*pet*")))

;;;###autoload
(define-minor-mode pet-minor-mode
  "Minor mode to setup buffer local variables for Python tools."
  :lighter "Pet"
  :group 'pet
  (if pet-minor-mode
      (pet-buffer-local-vars-setup)
    (pet-buffer-local-vars-teardown)))

(defun pet-minor-mode-on ()
  "Whether `pet-minor-mode' should be enabled."
  (when (derived-mode-p 'python-mode)
    (pet-minor-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-pet-minor-mode pet-minor-mode pet-minor-mode-on
  :group 'pet)



(add-to-list 'auto-mode-alist '("\\.pythonrc\\'"    . python-mode))
(add-to-list 'auto-mode-alist '("\\.pylintrc\\'"    . conf-mode))
(add-to-list 'auto-mode-alist '("\\.flake8\\'"      . conf-mode))
(add-to-list 'auto-mode-alist '("\\poetry.lock\\'"  . conf-toml-mode))
(add-to-list 'auto-mode-alist '("\\Pipfile\\'"      . conf-toml-mode))
(add-to-list 'auto-mode-alist '("\\Pipfile.lock\\'" . conf-toml-mode))



(defun pet-cleanup-watchers-and-caches ()
  "Clean up configuration file caches and watchers.

Delete configuration file caches and watchers when all
`python-mode' buffers of a project have been closed."
  (when (and (buffer-file-name)
             (derived-mode-p 'python-mode))
    (let ((root (pet-project-root)))
      (when (null (cl-loop for buf in (buffer-list)
                           if (and (not (equal buf (current-buffer)))
                                   (string-prefix-p root (buffer-file-name buf)))
                           return buf))

        (setf (alist-get root pet-project-virtualenv-cache nil t 'equal) nil)

        (pcase-dolist (`(,config-file . ,watcher) pet-watched-config-files)
          (when (string-prefix-p root config-file)
            (file-notify-rm-watch watcher)
            (setf (alist-get config-file pet-watched-config-files nil t 'equal) nil)))

        (dolist (cache '(pet-pre-commit-config-cache
                         pet-pyproject-cache
                         pet-python-version-cache))
          (pcase-dolist (`(,config-file . ,_) (symbol-value cache))
            (when (string-prefix-p root config-file)
              (setf (alist-get config-file (symbol-value cache) nil t 'equal) nil))))))))

(add-hook 'kill-buffer-hook #'pet-cleanup-watchers-and-caches)

(provide 'pet)

;;; pet.el ends here
