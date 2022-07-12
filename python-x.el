;;; python-x.el --- summary -*- lexical-binding: t -*-

;; Author: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Maintainer: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


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
;; 1. support pyenv
;; 2. support system pip
;; 3. support pipenv
;; 4. support setuptools

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
  :type '(choice (const :tag "Elisp function" 'yaml-parse-string)
                 (const :tag "dasel" "dasel")
                 (string :tag "Executable")))

(defcustom python-x-toml-parser "dasel"
  ""
  :group 'python-x
  :type '(choice (const :tag "Elisp function" 'toml:read-from-string)
                 (const :tag "dasel" "dasel")
                 (string :tag "Executable")))

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
        (funcall 'call-process "dasel" nil t nil "select" "-f" file-path "-w" "json")
        (json-parse-string (buffer-string) :object-type 'alist :array-type 'list))
    (error (minibuffer-message "%s" (error-message-string err)) nil)))

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
                              (assoc-delete-all file (symbol-value cache-var))
                              python-x-watched-config-files
                              (assoc-delete-all file python-x-watched-config-files)))
                       ('changed
                        (setf (assoc-default file (symbol-value cache-var))
                              (funcall parser file))))))))
          python-x-watched-config-files)))

(defun python-x-pre-commit-config-file-path ()
  (python-x-find-file-from-project-root ".pre-commit-config.yaml"))

(defun python-x-pyproject-file-path ()
  (python-x-find-file-from-project-root "pyproject.toml"))

(defvar python-x-pre-commit-config-cache nil)
(defvar python-x-pyproject-cache nil)

(cl-defmacro python-x-config-file-content (name &key file-path cache-var parser)
  `(defun ,name ()
     (let* ((config-file ,file-path)
            (cache-value (and config-file
                              (file-exists-p config-file)
                              (assoc-default config-file ,cache-var))))
       (if cache-value
           cache-value
         (when config-file
           (python-x-watch-config-file config-file (quote ,cache-var) (quote ,parser))
           (when-let ((content (funcall (quote ,parser) config-file)))
             (push (cons config-file content) ,cache-var)
             content))))))

(python-x-config-file-content python-x-pre-commit-config
                              :file-path (python-x-pre-commit-config-file-path)
                              :cache-var python-x-pre-commit-config-cache
                              :parser python-x-parse-config-file)

(python-x-config-file-content python-x-pyproject
                              :file-path (python-x-pyproject-file-path)
                              :cache-var python-x-pyproject-cache
                              :parser python-x-parse-config-file)

(defun python-x-use-poetry-p ()
  (when-let ((pyproject-path (python-x-pyproject-file-path)))
    (and
     (with-temp-buffer
       (insert-file-contents pyproject-path)
       (goto-char (point-min))
       (re-search-forward "^\\[tool.poetry\\]$" nil t nil))
     (executable-find "poetry")
     (condition-case err
         (with-temp-buffer
           (or (zerop (call-process "poetry" nil t nil "env" "info" "-p"))
               (zerop (call-process "poetry" nil t nil "install"))))
       (error (minibuffer-message (error-message-string err))
              nil)))))

(defun python-x-pre-commit-p (requirements)
  (and (python-x-pre-commit-config-file-path)
       (or (executable-find "pre-commit")
           (member "pre-commit" requirements))))

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
                collect (if (string-match
                             (rx (or "-r" "-c" "--requirement" "--constraint")
                                 (+ space)
                                 (group (+ nonl)))
                             req)
                            (let ((file-path (match-string 1 req)))
                              (python-x-resolve-requirements
                               (file-name-directory
                                (expand-file-name file-path anchor-dir))
                               (file-name-nondirectory file-path)))
                          (python-parse-requirement-spec req)))))))

;; https://pip.pypa.io/en/stable/cli/pip_install/#requirements-file-format
(defun python-x-parse-requirement-spec (req)
  (save-match-data
    (cond ((string-match (rx (or "http" "file" "git" "git+ssh") "://") req) ;; URL
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

             (and (string-match (rx url) req) (match-string 0 req))))
          ;; Path
          ((string-match (rx line-start (in "." "/")) req)
           (when-let ((requirement-spec (car (split-string req (rx (or ";" "@" "--")))))
                      (requirement-spec (string-trim requirement-spec)))
             (when (not (string-empty-p requirement-spec))
               requirement-spec)))
          ;; TODO: fetch the package and read the setup.cfg or setup.py files to
          ;; extract dependencies
          ((string-match (rx (or "-e" "--editable") (+ space) (group (+ nonl)) eol) req)
           nil)
          ;; Options
          ((string-match (rx "--" (+ (in alnum "-" "_"))) req)
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
         (requirements
          (assoc-default file-path python-x-project-requirements-cache)))
    (unless requirements
      (setf requirements
            (mapcar 'python-x-parse-requirement-spec
                    (or (and (python-x-use-poetry-p)
                             (condition-case err
                                 (process-lines "poetry" "run" "pip" "list" "--format=freeze" "--disable-pip-version-check")
                               (error (minibuffer-message "%s" (error-message-string err)) nil)))
                        ;; TODO:
                        ;; 1. search the project root for something that looks a
                        ;; virtualenv (or pyenv for now), go into the venv and
                        ;; call venv/bin/pip list --format=freeze in it
                        ;; 2. if requirements.txt is found but no virtualenv
                        ;; found, make one and install the requirements
                        (when-let ((project-root-requirements-file (python-x-find-file-from-project-root "requirements.txt")))
                          (python-x-resolve-requirements
                           (file-name-directory project-root-requirements-file)
                           project-root-requirements-file))
                        (condition-case err
                            (process-lines "pip" "list" "--format=freeze" "--disable-pip-version-check")
                          (error (minibuffer-message "%s" (error-message-string err)) nil)))))
      (setf python-x-project-requirements-cache
            (assoc-delete-all file-path python-x-project-requirements-cache))
      (when requirements
        (push (cons file-path requirements) python-x-project-requirements-cache)))
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
        (funcall 'call-process "sqlite3" nil t nil "-json" db-file "select * from repos")
        (json-parse-string (buffer-string) :object-type 'alist :array-type 'list))
    (error (error-message-string err) nil)))

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
                             (concat ":" (string-join additional-deps ":"))))))

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
    (unless venv-path
      (condition-case err
          (progn
            (cond ((python-x-use-poetry-p)
                   (with-temp-buffer
                     (call-process "poetry" nil t nil "run" "pre-commit" "install" "--install-hooks")))
                  ((executable-find "pre-commit")
                   (with-temp-buffer
                     (call-process "pre-commit" nil t nil "install" "--install-hooks")))
                  (t (user-error "No pre-commit executable found.")))
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

        (dolist (cache '(python-x-pre-commit-config-cache python-x-pyproject-cache))
          (pcase-dolist (`(,config-file . ,_) (symbol-value cache))
            (when (string-prefix-p root config-file)
              (setf (symbol-value cache)
                    (assoc-delete-all config-file (symbol-value cache)))))))

      (setf python-x-project-requirements-cache
            (assoc-delete-all buf-file python-x-project-requirements-cache)))))

(add-hook 'kill-buffer-hook 'python-x-cleanup-watchers-and-caches)

;;;###autoload
(defun python-x-executable-find (executable)
  (let ((requirements (python-x-project-requirements)))
    (cond ((and (python-x-pre-commit-p requirements)
                (python-x-pre-commit-config-has-hook-p executable))
           (concat (file-name-as-directory
                    (python-x-pre-commit-ensure-virtualenv-path executable))
                   "/bin/" executable))
          ((and (python-x-use-poetry-p)
                (member executable requirements))
           (concat "poetry" "run" executable))
          ((executable-find executable)
           executable))))

(provide 'python-x)

;;; python-x.el ends here
