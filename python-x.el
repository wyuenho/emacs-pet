;;; python-x.el --- python mode extensions -*- lexical-binding: t -*-

;; Author: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Maintainer: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
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
;; Support updating buffer local variables when config change
;; Support pipenv ????
;; Support flit/pdm ????

;;; Code:


(require 'cl-lib)
(require 'filenotify)
(require 'let-alist)
(require 'pcase)
(require 'project)
(require 'python)
(require 'seq)
(require 'subr-x)

(when (< emacs-major-version 27)
  (require 'json))

(defgroup python-x nil
  "python-x"
  :group 'python
  :prefix "python-x-")

(defcustom python-x-debug nil
  "Whether to turn on debug messages."
  :group 'python-x
  :type 'boolean)

(defun python-x-report-error (err)
  (when python-x-debug
    (minibuffer-message (error-message-string err)))
  nil)

(defun python-x-project-root ()
  (or (and (functionp 'projectile-project-root)
           (projectile-project-root))
      (when-let ((project (project-current)))
        (or (and (functionp 'project-root)
                 (expand-file-name (project-root project)))
            (and (functionp 'project-roots)
                 (when-let ((root (car (project-roots project))))
                   (expand-file-name root)))))))

(defun python-x-find-file-from-project-root (file-name)
  (when-let ((dir (locate-dominating-file
                   (or (python-x-project-root)
                       default-directory)
                   file-name)))
    (expand-file-name (concat (file-name-as-directory dir) file-name))))

(defun python-x-parse-json (str)
  (if (functionp 'json-parse-string)
      (json-parse-string str :object-type 'alist :array-type 'list)
    (let ((json-array-type 'list))
      (json-read-from-string str))))

(defun python-x-parse-config-file (file-path)
  (condition-case err
      (with-temp-buffer
        (call-process "dasel" nil t nil "select" "-f" file-path "-w" "json")
        (python-x-parse-json (buffer-string)))
    (error (python-x-report-error err))))

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
                        (setf (alist-get file (symbol-value cache-var) nil t 'equal) nil)
                        (setf (alist-get file python-x-watched-config-files nil t 'equal) nil))
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

(cl-defmacro python-x-config-file-content (name &key file-path parser)
  (let ((cache-var (intern (concat (symbol-name name) "-cache"))))
    `(progn
       (defvar ,cache-var nil)
       (defun ,name ()
         (let* ((config-file ,file-path)
                (cache-kvp (and config-file
                                (assoc config-file ,cache-var))))
           (if cache-kvp
               (cdr cache-kvp)
             (when config-file
               (python-x-watch-config-file config-file (quote ,cache-var) (quote ,parser))
               (when-let ((content (funcall (quote ,parser) config-file)))
                 (push (cons config-file content) ,cache-var)
                 content))))))))

(python-x-config-file-content python-x-pre-commit-config
                              :file-path (python-x-pre-commit-config-file-path)
                              :parser python-x-parse-config-file)

(python-x-config-file-content python-x-pyproject
                              :file-path (python-x-pyproject-file-path)
                              :parser python-x-parse-config-file)

(defun python-x-pre-commit-p ()
  (and (python-x-pre-commit-config-file-path)
       (or (executable-find "pre-commit")
           (and (python-x-use-poetry-p)
                (when-let* ((venv (python-x-virtualenv-root))
                            (exec-path (list (concat (file-name-as-directory venv) "bin"))))
                  (executable-find "pre-commit"))))))

(defun python-x-use-poetry-p ()
  (and (executable-find "poetry")
       (not
        (null
         (string-match-p
          "poetry"
          (or (let-alist (python-x-pyproject)
                .build-system.build-backend)
              ""))))))

(defun python-x-use-pyenv-p ()
  (and (python-x-python-version-file-path)
       (executable-find "pyenv")))

(defun python-x-pre-commit-config-has-hook-p (id)
  (member id (cl-loop for repo in (let-alist (python-x-pre-commit-config) .repos)
                      append (cl-loop for hook in (let-alist repo .hooks)
                                      collect (let-alist hook .id)))))

(defun python-x-parse-pre-commit-db (db-file)
  (condition-case err
      (with-temp-buffer
        (call-process "sqlite3" nil t nil "-json" db-file "select * from repos")
        (python-x-parse-json (buffer-string)))
    (error (python-x-report-error err))))

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



;;;###autoload
(defun python-x-executable-find (executable)
  (or (and (python-x-pre-commit-p)
           (not (string-prefix-p "python" executable))
           (condition-case err
               (if (not (python-x-pre-commit-config-has-hook-p executable))
                   (user-error "pre-commit does not have hook %s configured." executable)
                 (when-let* ((venv (python-x-pre-commit-virtualenv-path executable))
                             (bin-path (concat (file-name-as-directory venv) "bin" "/" executable)))
                   (if (file-exists-p bin-path)
                       bin-path
                     (user-error "pre-commit is configured but the hook %s do not appear to be installed." executable))))
             (error (python-x-report-error err))))
      (and (or (python-x-use-poetry-p)
               (python-x-use-pyenv-p))
           (when-let* ((venv (python-x-virtualenv-root))
                       (exec-path (list (concat (file-name-as-directory venv) "bin"))))
             (executable-find executable)))
      (when-let ((path (executable-find executable)))
        (condition-case nil
            (if (and (executable-find "pyenv")
                     (member path (process-lines "pyenv" "shims")))
                nil
              path)
          (error nil)))))

(defvar python-x-project-virtualenv-cache nil)

;;;###autoload
(defun python-x-virtualenv-root ()
  (let* ((root (python-x-project-root)))
    (or (alist-get root python-x-project-virtualenv-cache nil nil 'equal)
        (cond ((python-x-use-poetry-p)
               (condition-case err
                   (with-temp-buffer
                     (let ((exit-code (call-process "poetry" nil t nil "env" "info" "--path"))
                           (output (string-trim (buffer-string))))
                       (if (zerop exit-code)
                           (setf (alist-get root python-x-project-virtualenv-cache nil nil 'equal) output)
                         (user-error "poetry is configured but the virtualenv does not appear to be installed."))))
                 (error (python-x-report-error err))))
              ((python-x-use-pyenv-p)
               (condition-case err
                   (with-temp-buffer
                     (let ((exit-code (call-process "pyenv" nil t nil "prefix"))
                           (output (string-trim (buffer-string))))
                       (if (zerop exit-code)
                           (setf (alist-get root python-x-project-virtualenv-cache nil nil 'equal) (file-truename output))
                         (user-error "poetry is configured but the virtualenv does not appear to be installed."))))
                 (error (python-x-report-error err))))
              (t (getenv "VIRTUAL_ENV"))))))



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

(with-eval-after-load 'flycheck
  (defcustom flycheck-python-mypy-python-executable nil
    "Python executable to find the installed PEP 561 packages."
    :group 'flycheck-options
    :type '(choice (const :tag "Same as mypy's" nil)
                   (string :tag "Path"))
    :safe #'flycheck-string-or-nil-p)

  (flycheck-register-option-var 'flycheck-python-mypy-python-executable 'python-mypy))

(defvar python-x-flycheck-checker-props
  '((python-mypy . ((command . ("mypy"
                                "--show-column-numbers"
                                (config-file "--config-file" flycheck-python-mypy-config)
                                (option "--python-executable" flycheck-python-mypy-python-executable)
                                (option "--cache-dir" flycheck-python-mypy-cache-dir)
                                source-original))))))

(defun python-x-flycheck-checker-get-advice (fn checker property)
  (or (alist-get property (alist-get checker python-x-flycheck-checker-props))
      (funcall fn checker property)))

(defun python-x-flycheck-toggle-local-vars ()
  (if flycheck-mode
      (progn
        (when (derived-mode-p 'python-mode)
          (setq-local flycheck-pylintrc (python-x-flycheck-python-pylint-find-pylintrc))
          (setq-local flycheck-python-flake8-executable (python-x-executable-find "flake8"))
          (setq-local flycheck-python-pylint-executable (python-x-executable-find "pylint"))
          (setq-local flycheck-python-mypy-executable (python-x-executable-find "mypy"))
          (setq-local flycheck-python-mypy-python-executable (python-x-executable-find "python"))
          (setq-local flycheck-python-pyright-executable (python-x-executable-find "pyright"))
          (setq-local flycheck-python-pycompile-executable python-shell-interpreter)))
    (kill-local-variable 'flycheck-pylintrc)
    (kill-local-variable 'flycheck-python-flake8-executable)
    (kill-local-variable 'flycheck-python-pylint-executable)
    (kill-local-variable 'flycheck-python-mypy-executable)
    (kill-local-variable 'flycheck-python-pyright-executable)
    (kill-local-variable 'flycheck-python-pycompile-executable)))

;;;###autoload
(defun python-x-flycheck-setup ()
  (setq flycheck-flake8rc `(".flake8" "setup.cfg" "tox.ini"))

  (setq flycheck-python-mypy-config `("mypy.ini" ".mypy.ini" "pyproject.toml" "setup.cfg"
                                      ,(concat (expand-file-name
                                                (or (and (getenv "XDG_CONFIG_HOME")
                                                         (file-name-as-directory (getenv "XDG_CONFIG_HOME")))
                                                    "~/.config/"))
                                               "mypy/config")))

  (with-eval-after-load 'flycheck
    (advice-add 'flycheck-checker-get :around #'python-x-flycheck-checker-get-advice)
    (add-hook 'flycheck-mode-hook #'python-x-flycheck-toggle-local-vars)))

;;;###autoload
(defun python-x-flycheck-teardown ()
  (advice-remove 'flycheck-checker-get #'python-x-flycheck-checker-get-advice)
  (remove-hook 'flycheck-mode-hook #'python-x-flycheck-toggle-local-vars)
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

(defun python-x-buffer-local-vars-setup ()
  (setq-local python-shell-interpreter (python-x-executable-find "python"))
  (setq-local python-shell-virtualenv-root (python-x-virtualenv-root))

  (python-x-flycheck-setup)

  (setq-local lsp-jedi-executable-command
              (python-x-executable-find "jedi-language-server"))
  (setq-local lsp-pyright-venv-path python-shell-virtualenv-root)
  (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter)
  (setq-local dap-python-executable python-shell-interpreter)
  (setq-local python-pytest-executable (python-x-executable-find "pytest"))
  (setq-local python-black-command (python-x-executable-find "black"))
  (setq-local python-isort-command (python-x-executable-find "isort")))

(defun python-x-buffer-local-vars-teardown ()
  (kill-local-variable 'python-shell-interpreter)
  (kill-local-variable 'python-shell-virtualenv-root)

  (python-x-flycheck-teardown)

  (kill-local-variable 'lsp-jedi-executable-command)
  (kill-local-variable 'lsp-pyright-venv-path)
  (kill-local-variable 'lsp-pyright-python-executable-cmd)
  (kill-local-variable 'dap-python-executable)
  (kill-local-variable 'python-pytest-executable)
  (kill-local-variable 'python-black-command)
  (kill-local-variable 'python-isort-command))

(defun python-x-verify-setup ()
  (interactive)

  (unless (derived-mode-p 'python-mode)
    (user-error "You are not in python-mode!"))

  (with-output-to-temp-buffer "*python-x*"
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
            python-isort-command)))

  (select-window (get-buffer-window "*python-x*")))

;;;###autoload
(define-minor-mode python-x-minor-mode
  ""
  :lighter "PyX"
  :group 'python-x
  (if python-x-minor-mode
      (python-x-buffer-local-vars-setup)
    (python-x-buffer-local-vars-teardown)))

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

(defun python-x-cleanup-watchers-and-caches ()
  (when-let* ((buffer-file-name)
              (derived-mode-p 'python-mode)
              (root (python-x-project-root))
              (null (seq-some (lambda (buf)
                                (and (not (equal buf (current-buffer)))
                                     (string-prefix-p root (buffer-file-name buf))))
                              (buffer-list))))
    (setf (alist-get root python-x-project-virtualenv-cache nil t 'equal) nil)

    (pcase-dolist (`(,config-file . ,watcher) python-x-watched-config-files)
      (when (string-prefix-p root config-file)
        (file-notify-rm-watch watcher)
        (setf (alist-get config-file python-x-watched-config-files nil t 'equal) nil)))

    (dolist (cache '(python-x-pre-commit-config-cache
                     python-x-pyproject-cache))
      (pcase-dolist (`(,config-file . ,_) (symbol-value cache))
        (when (string-prefix-p root config-file)
          (setf (alist-get config-file (symbol-value cache) nil t 'equal) nil))))))

(add-hook 'kill-buffer-hook 'python-x-cleanup-watchers-and-caches)

(provide 'python-x)

;;; python-x.el ends here
