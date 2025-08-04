;;; pet.el --- Executable and virtualenv tracker for python-mode -*- lexical-binding: t -*-

;; Author: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Maintainer: Jimmy Yuen Ho Wong <wyuenho@gmail.com>
;; Version: 4.0.0
;; Package-Requires: ((emacs "27.1") (f "0.6.0") (map "3.3.1") (seq "2.24"))
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

;; __P__ython __E__xecutable __T__racker.  Tracks downs the correct Python
;; executables from the various virtualenv management tools and assign them to
;; buffer local variables.  The package to end all Emacs virtualenv packages.

;;; Code:


(require 'cl-lib)
(require 'f)
(require 'filenotify)
(require 'let-alist)
(require 'map)
(require 'pcase)
(require 'project)
(require 'python)
(require 'seq)
(require 'subr-x)
(require 'tramp)

;; optional dependencies
(require 'tomlparse nil t)
(require 'yaml nil t)

(defgroup pet nil
  "Customization group for `pet'."
  :group 'python
  :prefix "pet-")

(defcustom pet-debug nil
  "Whether to turn on debug messages."
  :group 'pet
  :type 'boolean)

(defcustom pet-prefer-elisp-parsers nil
  "Whether to prefer Emacs Lisp parsers over external programs.

When non-nil, `pet' will use as Elisp-based TOML or YAML parser directly
instead of trying `pet-toml-to-json-program' or
`pet-yaml-to-json-program' first.

This is useful if you want to avoid external dependencies."
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

(defcustom pet-toml-to-json-program-arguments '("-f" "-" "-r" "toml" "-w" "json")
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

(defcustom pet-yaml-to-json-program-arguments '("-f" "-" "-r" "yaml" "-w" "json")
  "Arguments for `pet-yaml-to-json-program'."
  :group 'pet
  :type '(repeat string))

(defcustom pet-find-file-functions '(pet-find-file-from-project-root
                                     pet-locate-dominating-file
                                     pet-find-file-from-project-root-natively
                                     pet-find-file-from-project-root-recursively)
  "Order in which `pet-find-file-from-project' should search for a config file.

Each function should take a file name as its sole argument and
return an absolute path to the file found in the current project
and nil otherwise.

NOTE: this variable is not a hook."
  :group 'pet
  :options '(pet-find-file-from-project-root
             pet-locate-dominating-file
             pet-find-file-from-project-root-natively
             pet-find-file-from-project-root-recursively)
  :type '(repeat function))

(defcustom pet-venv-dir-names '(".venv" "venv" "env")
  "Directory names to search for when looking for a virtualenv at the project root."
  :group 'pet
  :type '(repeat string))

(defcustom pet-fd-command "fd"
  "The \"fd\" command in the system."
  :type 'string
  :group 'pet)

(defcustom pet-fd-command-args '("-tf" "-cnever" "-H" "-a" "-g")
  "The arguments to pass to the \"fd\" command."
  :type '(repeat string)
  :group 'pet)

(defcustom pet-search-globally t
  "Whether PET should search executables beyond the project's virtualenvs.

If you need to configure this value, it is likely that you'd want a
different value depending the project you are working on.  If so, you
can safely set this variable in a `.dir-locals.el' file at the root of
the project directory."
  :group 'pet
  :type 'boolean
  :safe t)

(defcustom pet-after-buffer-local-vars-setup nil
  "Functions to run after buffer local variable values are set up."
  :type 'hook
  :group 'pet)

(defcustom pet-before-buffer-local-vars-teardown nil
  "Functions to run before buffer local variables values are torn down."
  :type 'hook
  :group 'pet)

(defvar pet-pre-commit-database-cache nil
  "Cached pre-commit database content (system-wide).")

(defvar pet-pre-commit-database-watcher nil
  "File watcher for pre-commit database file (system-wide).")

(defvar pet--orig-executable-find (advice--cd*r (symbol-function #'executable-find))
  "Original `executable-find' function definition before any advice.")



(defun pet--executable-find (command &optional remote)
  "Internal wrapper for the original `executable-find' function.

COMMAND is the executable to find, REMOTE indicates if searching
on a remote system.

This function provides access to the original `executable-find'
implementation, bypassing any potential interference from PET's
own executable finding logic."
  (funcall pet--orig-executable-find command remote))

(defun pet-system-bin-dir ()
  "Determine the correct script directory based on `system-type'."
  (if (eq (if (file-remote-p default-directory)
              (tramp-get-connection-property
               (tramp-dissect-file-name default-directory)
               "uname"
               'windows-nt)
            system-type)
          'windows-nt)
      "Scripts" "bin"))

(defun pet-report-error (err)
  "Report ERR to the minibuffer.

Only reports to the minibuffer if `pet-debug' is non-nil."
  (when pet-debug
    (minibuffer-message (error-message-string err)))
  nil)

(defun pet-conda-venv-p (venv-path)
  "Return non-nil if VENV-PATH is a conda-style environment.

This includes conda, mamba, micromamba, and pixi environments.
All conda-style environments have a `conda-meta' directory containing
package metadata."
  (when venv-path
    (let ((venv-dir (file-name-as-directory venv-path)))
      (file-exists-p (concat venv-dir "conda-meta")))))



;; Remote-aware process running helpers

(defmacro pet-run-process (program args &rest body)
  "Run PROGRAM with ARGS in a temp buffer and execute BODY.

BODY has access to `exit-code' variable and the current buffer with
output.  Works with both local and remote paths via `process-file'."
  (declare (indent 2))
  `(with-temp-buffer
     (condition-case err
         (let ((exit-code (apply #'process-file ,program nil t nil ,args)))
           ,@body)
       (error (pet-report-error err)))))

(defun pet-run-process-get-output (program &rest args)
  "Run PROGRAM with ARGS and return the entire OUTPUT if exit code is 0.

OUTPUT is the full stdout/stderr as a string, trimmed.

If exit-code is non-zero and output is non-empty, reports error to user
and return nil."
  (pet-run-process program args
    (let ((output (string-trim (buffer-string))))
      (if (zerop exit-code)
          output
        (when (not (string-empty-p output))
          (pet-report-error (list 'user-error output)))))))

(defun pet-run-process-get-line (program &rest args)
  "Run PROGRAM with ARGS and return the FIRST-LINE if exit code is 0.

FIRST-LINE is the first line of output, or empty string if no output.

If exit-code is non-zero and output is non-empty, reports error to user
and return nil."
  (pet-run-process program args
    (if (zerop exit-code)
        (progn
          (goto-char (point-min))
          (unless (eobp)
            (string-trim (buffer-substring (point-min) (line-end-position)))))
      (let ((output (string-trim (buffer-string))))
        (when (not (string-empty-p output))
          (pet-report-error (list 'user-error output)))))))

(defun pet-run-process-get-lines (program &rest args)
  "Run PROGRAM with ARGS and return LINES if exit code is 0.

LINES is a list of output lines, or empty list if no output.

If exit-code is non-zero and output is non-empty, reports error to user
and return nil."
  (pet-run-process program args
    (let ((output (string-trim (buffer-string))))
      (if (zerop exit-code)
          (unless (string-empty-p output)
            (split-string output "\n" t))
        (when (not (string-empty-p output))
          (pet-report-error (list 'user-error output)))))))



;;; Unified Cache Infrastructure

(defvar pet-cache nil
  "Unified cache for project-scoped pet data.

Structure: nested alists accessible by path lists.  All paths start with project
root, followed by category and optional key.

Project-scoped categories:
- `:virtualenv' - direct value (project virtualenv path)
- `:files' - (file-pattern . resolved-path) pairs
- `:configs' - (config-file-path . parsed-content) pairs
- `:file-watchers' - (config-file-path . watcher-descriptor) pairs

Examples:
- (pet-cache-get \='(\"/project\" :virtualenv))
- (pet-cache-get \='(\"/project\" :configs \"pyproject.toml\"))
- (pet-cache-get \='(\"/project\" :files \"*.py\"))

Cache Invariant Rules:
1. Strong Invariant (bidirectional): `:configs' ↔ `:file-watchers'
   - If an entry exists in `:configs', it MUST have a corresponding entry in
     `:file-watchers'
   - If an entry exists in `:file-watchers', it MUST have a corresponding entry
     in `:configs'

2. Weak Invariant (unidirectional): `:configs' → `:files'
   - If an entry exists in `:configs', the file path MUST exist as a value in
     some `:files' entry
   - But `:files' entries can exist without corresponding `:configs' entries

3. No Invariant: :files entries are independent
   - `:files' can contain any discovered file paths, regardless of parsing needs

Note: System-wide data like pre-commit database uses dedicated variables,
not this cache.")

(defun pet-cache-get (path)
  "Get value at PATH in cache.  PATH is a list of keys."

  (let ((keys path)
        (subtree pet-cache))
    (while keys
      (setq subtree (alist-get (pop keys) subtree nil nil 'equal)))
    subtree))

(defun pet-cache-put (path value)
  "Put VALUE at PATH in cache, creating intermediate alists as needed."
  (unless path
    (error "Path cannot be nil"))

  (let ((section pet-cache)
        (keys path)
        key parent)
    ;; Navigate as far as possible through existing cache structure
    ;; Stop when we hit the end of the path or a key where its value is nil
    (while (and keys (alist-get (car keys) section nil nil 'equal))
      (setq key (pop keys)          ; Remember current key for parent updates
            parent section          ; Remember parent for later modification
            section (alist-get key section nil nil 'equal))) ; Move to next level

    ;; Case 1: Full path has non-nil value - just update the value at the end
    (if (and (not keys) section)
        ;; Direct update of existing path's value
        (setf (alist-get key parent nil nil 'equal) value)

      ;; Case 2: Need to create missing structure for remaining keys
      ;; Build nested structure from the inside out (reverse key order)
      (let ((keys (reverse keys))
            (subtree value))

        ;; Build nested alist structure: each key wraps the previous structure
        ;; Example: keys '(:virtualenv "/project") becomes
        ;; '("/project" . ((:virtualenv . value)))
        (while keys
          (setq subtree (list (cons (pop keys) subtree))))

        ;; Add the new structure to the existing cache
        ;; subtree is now '((outermost-key . nested-structure))
        (setf (alist-get (car (car subtree)) section nil nil 'equal) (cdr (car subtree)))

        ;; Update the cache hierarchy: either update parent or global cache
        (if parent
            ;; We navigated partway - update the parent to point to modified section
            (setf (alist-get key parent nil nil 'equal) section)
          ;; We're at top level - update the global cache variable
          (setq pet-cache section))))))

(defun pet-cache-rem (path)
  "Remove value at PATH in cache.

Removes the entry completely from the alist structure."
  (let* ((keys path)
         (section pet-cache)
         key parent)
    ;; Navigate to the parent of the target entry
    ;; Stop one level before the final key so we can modify the parent
    (while (cdr keys)
      (setq key (pop keys)          ; Remember current key for parent updates
            parent section          ; Remember parent for later modification
            section (alist-get key section nil nil 'equal))) ; Move to next level

    ;; Remove the target entry using setf with 't' flag for removal
    ;; This completely removes the key-value pair from the alist
    (setf (alist-get (car keys) section nil t 'equal) nil)

    ;; Update the cache hierarchy to reflect the removal
    (if parent
        ;; We navigated down - update the parent to point to modified section
        (setf (alist-get key parent nil nil 'equal) section)
      ;; We're at top level - update the global cache directly
      (setf (alist-get (car keys) pet-cache nil t 'equal) nil))))

;;; High-level Cache Management Functions

(defun pet-setup-config-cache-and-watcher (absolute-path parser)
  "Parse and cache config file content, set up file watcher.

ABSOLUTE-PATH is the absolute path to the config file.  PARSER is the
function to parse the file content.

This function maintains the strong cache invariant between `:configs' and
`:file-watchers':
1. Parsing and caching the content in `:configs' category
2. Setting up file watcher and caching handle in `:file-watchers' category

Note: File path should already be cached in `:files' category by
`pet-find-file-from-project'."
  (when-let ((root (pet-project-root)))
    (pet-cache-put (list root :configs absolute-path) (funcall parser absolute-path))

    (unless (pet-cache-get (list root :file-watchers absolute-path))
      (let ((watcher (file-notify-add-watch
                      absolute-path
                      '(change)
                      (pet-make-config-file-change-callback parser))))
        (pet-cache-put (list root :file-watchers absolute-path) watcher)))))

(defun pet-teardown-config-cache-and-watcher (absolute-path)
  "Remove config cache and file watcher.

ABSOLUTE-PATH is the absolute path to the config file to remove.

This function maintains the strong cache invariant between `:configs' and
`:file-watchers':
1. Removing file watcher and cleaning up handle from `:file-watchers' category
2. Removing parsed content from `:configs' category

Note: `:files' entries are left intact as they can exist independently
of config parsing."
  (when-let ((root (pet-project-root)))
    (when-let ((watcher (pet-cache-get (list root :file-watchers absolute-path))))
      (file-notify-rm-watch watcher)
      (pet-cache-rem (list root :file-watchers absolute-path)))

    (pet-cache-rem (list root :configs absolute-path))))

(defun pet-cache--clear-project-internal (root)
  "Clear cache for project at ROOT.

Removes all cached virtualenv detection results, configuration file
contents, and file discovery results for the project.  Also cleans up
file watchers before clearing cache data to prevent resource leaks."
  (when root
    (when-let* ((watchers-section (pet-cache-get (list root :file-watchers))))
      (pcase-dolist (`(,_path . ,watcher) watchers-section)
        (file-notify-rm-watch watcher)))
    (pet-cache-rem (list root))))

(defun pet-cache--clear-precommit-internal ()
  "Clear pre-commit database cache and watcher.

Removes the cached pre-commit database data and cleans up the associated
file watcher to prevent resource leaks."
  (setq pet-pre-commit-database-cache nil)
  (when pet-pre-commit-database-watcher
    (file-notify-rm-watch pet-pre-commit-database-watcher)
    (setq pet-pre-commit-database-watcher nil)))

(defun pet-cache-clear-project ()
  "Clear pet's cache for the current project."
  (interactive)
  (when-let* ((root (pet-project-root)))
    (pet-cache--clear-project-internal root)
    (message "Cleared pet cache for project: %s" root)))

(defun pet-cache-clear-all ()
  "Clear all pet caches across all projects."
  (interactive)
  (when pet-cache
    (pcase-dolist (`(,_root . ,project-data) pet-cache)
      (when-let* ((watchers-section (alist-get :file-watchers project-data)))
        (pcase-dolist (`(,_path . ,watcher) watchers-section)
          (file-notify-rm-watch watcher)))))
  (setq pet-cache nil)
  (pet-cache--clear-precommit-internal)
  (message "Cleared all pet caches"))



(defun pet-project-root ()
  "Return the path of root of the project.

If `projectile' is available, the function
`projectile-project-root' is used to find the project root.
Otherwise, `project-root' is used."
  (or (and (functionp 'projectile-project-root)
           (projectile-project-root))
      (when-let* ((project (project-current)))
        (or (and (functionp 'project-root)
                 (expand-file-name (project-root project)))
            (and (functionp 'project-roots)
                 (when-let* ((root (car (project-roots project))))
                   (expand-file-name root)))))))

(defun pet-find-file-from-project-root (file)
  "Find FILE from the current project's root.

FILE is a file name or a wildcard.

Return absolute path to FILE if found in the project root, nil
otherwise."
  (when-let* ((root (pet-project-root)))
    (car (file-expand-wildcards (concat (file-name-as-directory root) file) t))))

(defun pet-locate-dominating-file (file)
  "Find FILE by walking up `default-directory' until the current project's root.

FILE is a file name or a wildcard.

Return absolute path to FILE if found, nil otherwise."
  (when-let* ((root (pet-project-root))
              (dir (locate-dominating-file
                    default-directory
                    (lambda (dir)
                      (car
                       (file-expand-wildcards
                        (concat (file-name-as-directory dir) file))))))
              (dir (expand-file-name dir)))
    (when (string-prefix-p root dir)
      (car (file-expand-wildcards (concat (file-name-as-directory dir) file) t)))))

(defun pet-find-file-from-project-root-natively (file)
  "Find FILE natively by searching down from the current project's root.

FILE is a file name or a wildcard.

Return absolute path to FILE if found, nil otherwise.

The actual search is done via calling native programs in a subprocess.

Currently only `fd' is supported.  See `pet-fd-command' and
`pet-fd-command-args'."
  (when-let* ((root (pet-project-root))
              (fd (pet--executable-find pet-fd-command t)))
    (let ((default-directory root))
      (apply #'pet-run-process-get-line fd `(,@pet-fd-command-args ,file ,root)))))

(defun pet-find-file-from-project-root-recursively (file)
  "Find FILE by recursively searching down from the current project's root.

FILE is a file name or a wildcard.

Return absolute path to FILE if found, nil otherwise."
  (when-let* ((root (pet-project-root)))
    (when-let* ((fileset
                 (cond ((functionp 'projectile-dir-files)
                        (mapcar (apply-partially #'concat root)
                                (projectile-dir-files (pet-project-root))))
                       (t (project-files (project-current))))))
      (seq-find (lambda (f)
                  (string-match-p
                   (wildcard-to-regexp file)
                   (file-name-nondirectory f)))
                (sort fileset 'string<)))))

(defun pet-find-file-from-project (file)
  "Find FILE from the current project.

Try each function in `pet-find-file-functions' in order and
return the absolute path found by the first function, nil
otherwise."
  (when-let* ((root (pet-project-root)))
    (or (pet-cache-get (list root :files file))
        ;; we don't really want to call `pet-find-file-functions' as real hook
        ;; because then both the buffer local and global values will be run, but
        ;; we just want to run the effective find file functions currently in
        ;; scope
        (when-let* ((result (seq-some (lambda (fn) (funcall fn file)) pet-find-file-functions)))
          (pet-cache-put (list root :files file) result)
          result))))

(defun pet-parse-json (str)
  "Parse JSON STR to an alist.  Arrays are converted to lists."
  (json-parse-string str :object-type 'alist :array-type 'list))

(defun pet-parse-toml-with-elisp (file-path)
  "Parse TOML file at FILE-PATH with an Elisp parser.

Returns parsed content on success, throws error on parse failure,
returns `:parser-not-available' if the parser is not available."
  (if (functionp 'tomlparse-file)
      (tomlparse-file file-path :object-type 'alist)
    :parser-not-available))

(defun pet-parse-yaml-with-elisp (file-path)
  "Parse YAML file at FILE-PATH with an Elisp parser.

Returns parsed content on success, throws error on parse failure,
returns `:parser-not-available' if the parser is not available."
  (if (functionp 'yaml-parse-string)
      (with-temp-buffer
        (insert-file-contents file-path)
        (yaml-parse-string (buffer-string) :object-type 'alist :sequence-type 'list))
    :parser-not-available))

(defun pet-try-elisp-parser (parser-fn file-path)
  "Try parsing FILE-PATH with PARSER-FN.

Returns (success . result) where success is t/nil and result is the
parsed data."
  (condition-case err
      (let ((result (funcall parser-fn file-path)))
        (if (eq result :parser-not-available)
            (cons nil nil)  ; Parser not available
          (cons t result))) ; Parse succeeded (including nil for empty files)
    (user-error (pet-report-error err) (cons nil nil))  ; Parse failed
    (error (pet-report-error err) (cons nil nil))))     ; Parse failed

(defun pet-try-external-parser (file-path program arguments)
  "Try parsing FILE-PATH with external PROGRAM and ARGUMENTS.

Returns (success . result) where success is t/nil and result is the
parsed data.  Normalizes :null to nil for consistent empty file
handling."
  (if (pet--executable-find program)
      (let ((output (get-buffer-create " *pet parser output*")))
        (unwind-protect
            (let ((exit-code
                   (condition-case err
                       (apply #'process-file program file-path output nil arguments)
                     (error (pet-report-error err)))))
              (if (and (integerp exit-code) (zerop exit-code))
                  (with-current-buffer output
                    (let ((result (pet-parse-json (buffer-string))))
                      ;; Normalize :null to nil for consistent empty file handling
                      (cons t (if (eq result :null) nil result))))
                (cons nil nil)))  ; Parse failed
          (kill-buffer output)))
    (cons nil nil)))  ; Program not found

(defun pet-try-parser-with-fallback (file-path elisp-parser-fn external-program external-args error-message)
  "Try parsing FILE-PATH with preferred parser, fallback to alternative.

ELISP-PARSER-FN is the elisp parser function.
EXTERNAL-PROGRAM and EXTERNAL-ARGS are for the external parser.
ERROR-MESSAGE is shown when both parsers fail."
  (pcase-let ((`(,success . ,result)
               (if pet-prefer-elisp-parsers
                   (pet-try-elisp-parser elisp-parser-fn file-path)
                 (pet-try-external-parser file-path external-program external-args))))
    (if success
        result  ; First parser succeeded
      ;; First parser failed, try fallback
      (pcase-let ((`(,fallback-success . ,fallback-result)
                   (if pet-prefer-elisp-parsers
                       (pet-try-external-parser file-path external-program external-args)
                     (pet-try-elisp-parser elisp-parser-fn file-path))))
        (if fallback-success
            fallback-result  ; Fallback succeeded
          (error error-message))))))

(defun pet-parse-config-file (file-path)
  "Parse a configuration file at FILE-PATH into JSON alist."
  (condition-case err
      (let* ((ext (downcase (or (file-name-extension file-path) "")))
             (auto-mode-alist-matcher (lambda (entry)
                                        (pcase-let ((`(,pat . ,mode) entry))
                                          (when (string-match-p pat file-path)
                                            mode))))
             (mode (seq-some auto-mode-alist-matcher auto-mode-alist))
             (json-p (or (equal ext "json")
                         (eq 'json-mode mode)
                         (eq 'json-ts-mode mode)
                         (eq 'jsonian-mode mode)))
             (toml-p (or (equal ext "toml")
                         (eq 'conf-toml-mode mode)
                         (eq 'toml-ts-mode mode)))
             (yaml-p (or (string-match-p "ya?ml" ext)
                         (eq 'yaml-mode mode)
                         (eq 'yaml-ts-mode mode))))

        (cond
         (json-p
          (with-temp-buffer
            (insert-file-contents file-path)
            (pet-parse-json (buffer-string))))

         (toml-p
          (pet-try-parser-with-fallback
           file-path
           #'pet-parse-toml-with-elisp
           pet-toml-to-json-program
           pet-toml-to-json-program-arguments
           (format "No TOML parser available. Please install %s or tomlparse.el" pet-toml-to-json-program)))

         (yaml-p
          (pet-try-parser-with-fallback
           file-path
           #'pet-parse-yaml-with-elisp
           pet-yaml-to-json-program
           pet-yaml-to-json-program-arguments
           (format "No YAML parser available. Please install %s or yaml.el" pet-yaml-to-json-program)))

         (t (error "Unsupported configuration file type: %s" file-path))))

    (error (pet-report-error err))))


(defun pet-make-config-file-change-callback (parser)
  "Make callback for `file-notify-add-watch'.

Return a callback with PARSER captured in its environment.
PARSER is the symbol to the parser to parse the file.
The callback updates the unified cache system.

When invoked, the callback returned will parse the file with
PARSER and update the unified cache if the file was changed.
If the file was deleted or renamed, remove the file's watcher
and cache entries while maintaining the cache invariant."
  (lambda (event)
    (pcase-let ((`(,_ ,action ,file . ,_) event))
      (pcase action
        ((or 'deleted 'renamed)
         (pet-teardown-config-cache-and-watcher file))
        ('changed
         (pet-cache-put (list (pet-project-root) :configs file) (funcall parser file)))))))


(cl-defmacro pet-def-config-accessor (name &key file-name parser)
  "Create a function for reading the content of a config file.

NAME will be used to create a memorized funcion named `pet-NAME' to
return the content of the configuration file FILE-NAME.  FILE-NAME is
the name or glob pattern of the configuration file that will be searched
in the project.

The content of the file will be parsed by PARSER and then cached in the
unified cache system.  Changes to the file will automatically update the
cached content and maintain the cache invariant."
  (declare (indent defun))
  (let* ((accessor-name (concat "pet-" (symbol-name name)))
         (path-accessor-name (concat accessor-name "-path"))
         (accessor-docstring
          (format "Accessor for `%s' in the current Python project.

If the file is found in the current Python project, cache its content in
the unified cache system and return it.

If the file content changes, it is parsed again and the cache is
refreshed automatically while maintaining the cache invariant.  If it is
renamed or deleted, all related cache entries are cleaned up.
"
                  name))
         (path-accessor-docstring (format "Path of `%s' in the current Python project.

Return nil if the file is not found." file-name)))
    `(progn
       (defun ,(intern path-accessor-name) ()
         ,path-accessor-docstring
         (pet-find-file-from-project ,file-name))

       (defun ,(intern accessor-name) ()
         ,accessor-docstring
         (when-let* ((config-file (,(intern path-accessor-name)))
                     (root (pet-project-root)))
           (or (pet-cache-get (list root :configs config-file))
               (progn
                 (pet-setup-config-cache-and-watcher config-file #',parser)
                 (pet-cache-get (list root :configs config-file)))))))))

(pet-def-config-accessor pre-commit-config
  :file-name ".pre-commit-config.yaml"
  :parser pet-parse-config-file)

(pet-def-config-accessor pyproject
  :file-name "pyproject.toml"
  :parser pet-parse-config-file)

(pet-def-config-accessor python-version
  :file-name ".python-version"
  :parser f-read-text)

(pet-def-config-accessor pipfile
  :file-name "Pipfile"
  :parser pet-parse-config-file)

;; So `pet-parse-config-file' knows Pipfile can be parsed with `pet-toml-to-json-program'.
(add-to-list 'auto-mode-alist '("/Pipfile\\'" . conf-toml-mode))

(pet-def-config-accessor environment
  :file-name "environment*.y?ml"
  :parser pet-parse-config-file)

(pet-def-config-accessor pixi
  :file-name "pixi.toml"
  :parser pet-parse-config-file)

(pet-def-config-accessor hatch
  :file-name "hatch.toml"
  :parser pet-parse-config-file)

(defun pet-use-pre-commit-p ()
  "Whether the current project is using `pre-commit'.

Returns the path to the `pre-commit' executable."
  (and (pet-pre-commit-config)
       (or (pet--executable-find "pre-commit" t)
           (and (when-let* ((venv (pet-virtualenv-root))
                            (exec-path (list (concat (file-name-as-directory venv) (pet-system-bin-dir))))
                            (process-environment (copy-sequence process-environment)))
                  (setenv "PATH" (string-join exec-path path-separator))
                  (pet--executable-find "pre-commit" t))))))

(defun pet-use-conda-p ()
  "Whether the current project is using `conda'.

Returns the path to the `conda' executable found."
  (and (pet-environment)
       (pet--executable-find "conda" t)))

(defun pet-use-mamba-p ()
  "Whether the current project is using `mamba' or `micromamba'.

Returns the path to the `mamba' executable variant found."
  (and (pet-environment)
       (or (pet--executable-find "mamba" t)
           (pet--executable-find "micromamba" t))))

(defun pet-use-pixi-p ()
  "Whether the current project is using `pixi'.

Returns the path to the `pixi' executable."
  (and (or (pet-pixi)
           (let-alist (pet-pyproject)
             .tool.pixi))
       (pet--executable-find "pixi" t)))

(defun pet-use-poetry-p ()
  "Whether the current project is using `poetry'.

Returns the path to the `poetry' executable."
  (and (string-match-p
        "poetry"
        (or (let-alist (pet-pyproject)
              .build-system.build-backend)
            ""))
       (pet--executable-find "poetry" t)))

(defun pet-use-pyenv-p ()
  "Whether the current project is using `pyenv'.

Returns the path to the `pyenv' executable."
  (and (pet-python-version)
       (pet--executable-find "pyenv" t)))

(defun pet-use-pipenv-p ()
  "Whether the current project is using `pipenv'.

Returns the path to the `pipenv' executable."
  (and (pet-pipfile)
       (pet--executable-find "pipenv" t)))

(defun pet-use-hatch-p ()
  "Whether the current project is using `hatch'.

Returns the path to the `hatch' executable."
  (and (or (pet-hatch)
           (let-alist (pet-pyproject)
             .tool.hatch.envs))
       (pet--executable-find "hatch" t)))

(defun pet-pre-commit-config-has-hook-p (id)
  "Determine if the `pre-commit' configuration has a hook.

Return non-nil if the `pre-commit' configuration for the current
project has hook ID set up."
  (member id (cl-loop for repo in (let-alist (pet-pre-commit-config) .repos)
                      append (cl-loop for hook in (let-alist repo .hooks)
                                      collect (let-alist hook .id)))))

(defun pet-parse-pre-commit-db (db-file)
  "Parse `pre-commit' database.

Read the pre-commit SQLite database located at DB-FILE into an alist."
  (or (and (functionp 'sqlite-available-p)
           (sqlite-available-p)
           (when-let* ((db (sqlite-open db-file)))
             (unwind-protect
                 (let* ((result-set (sqlite-select db "select * from repos" nil 'set))
                        result
                        row)
                   (while (setq row (sqlite-next result-set))
                     (setq result (cons (seq-mapn (lambda (a b) (cons (intern a) b))
                                                  (sqlite-columns result-set)
                                                  row)
                                        result)))
                   (sqlite-finalize result-set)
                   result)
               (sqlite-close db))))
      (when-let* ((sqlite3 (pet--executable-find "sqlite3" t))
                  (json (pet-run-process-get-output sqlite3 "-json" db-file "select * from repos")))
        (pet-parse-json json))))

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
                (unless (getenv "PRE_COMMIT_HOME") "pre-commit/")
                "db.db"))
              (db
               (or pet-pre-commit-database-cache
                   (when (file-exists-p db-file)
                     (let ((content (pet-parse-pre-commit-db db-file)))
                       (setq pet-pre-commit-database-cache content)
                       ;; Set up file watcher for system database
                       (unless pet-pre-commit-database-watcher
                         (setq pet-pre-commit-database-watcher
                               (file-notify-add-watch
                                db-file
                                '(change)
                                (lambda (event)
                                  (pcase-let ((`(,_ ,action . ,_) event))
                                    (pcase action
                                      ((or 'deleted 'renamed)
                                       (setq pet-pre-commit-database-cache nil)
                                       (when pet-pre-commit-database-watcher
                                         (file-notify-rm-watch pet-pre-commit-database-watcher)
                                         (setq pet-pre-commit-database-watcher nil)))
                                      ('changed
                                       (setq pet-pre-commit-database-cache (pet-parse-pre-commit-db db-file)))))))))
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
               (let-alist repo-config .repo))

              (repo-dir
               (let* ((additional-deps
                       (let-alist repo-config
                         (let-alist (seq-find (lambda (hook) (let-alist hook (equal .id hook-id))) .hooks)
                           .additional_dependencies)))
                      (unsorted-repo-url (concat repo-url ":" (string-join additional-deps ",")))
                      (sorted-repo-url (concat repo-url ":" (string-join (sort (copy-sequence additional-deps) 'string<) ","))))
                 (let-alist (seq-find
                             (lambda (row)
                               (let-alist row
                                 (and (if additional-deps
                                          (or (equal .repo unsorted-repo-url)
                                              (equal .repo sorted-repo-url))
                                        (equal .repo repo-url))
                                      (equal .ref (let-alist repo-config .rev)))))
                             db)
                   .path))))

    (car
     (last
      (file-expand-wildcards
       (concat (file-name-as-directory repo-dir) "py_env-*")
       t)))))



(defun pet-deep-copy-hash-table (value)
  "Recursively deep copy a hash table and any nested hash tables.

VALUE can be any Lisp object. If VALUE is a hash table, returns a new
hash table with the same keys and deeply copied values. If any values
are themselves hash tables, they are also deep copied recursively.

If VALUE is not a hash table, returns VALUE unchanged.

This is needed because `copy-hash-table' only performs a shallow copy,
sharing references to the original values."
  (if (hash-table-p value)
      (let ((copy (copy-hash-table value)))
        (cl-loop for key in (hash-table-keys copy)
                 with val
                 do (setq val (gethash key copy))
                 if (hash-table-p val)
                 do (puthash key (pet-deep-copy-hash-table val) copy))
        copy)
    value))

;;;###autoload
(defun pet-executable-find (executable &optional search-globally)
  "Find the correct EXECUTABLE for the current Python project.

Search for EXECUTABLE first in the `pre-commit' virtualenv, then
whatever environment is found by `pet-virtualenv-root'.

If SEARCH-GLOBALLY or `pet-search-globally' is non-nil, the search
continues to look in `pyenv', then finally from the variable
`exec-path'."

  (catch 'done
    (cond ((and (not (file-remote-p executable))
                (file-name-absolute-p executable)
                (pet--executable-find executable))
           executable)
          ((and (pet-use-pre-commit-p)
                (not (string-prefix-p "python" executable))
                (pet-pre-commit-config-has-hook-p executable))
           (condition-case err
               (let* ((venv (or (pet-pre-commit-virtualenv-path executable)
                                (user-error "`pre-commit' is configured but the hook `%s' does not appear to be installed" executable)))
                      (bin-dir (concat (file-name-as-directory venv) (pet-system-bin-dir)))
                      (bin-path (concat bin-dir "/" executable)))
                 (if (file-exists-p bin-path)
                     bin-path
                   (user-error "`pre-commit' is configured but `%s' is not found in %s" executable bin-dir)))
             (error (pet-report-error err))))
          ((when-let* ((venv (pet-virtualenv-root))
                       (venv-bin (tramp-file-local-name
                                  (concat (file-name-as-directory venv)
                                          (unless (and (string-prefix-p "python" executable)
                                                       (pet-conda-venv-p venv)
                                                       (eq system-type 'windows-nt))
                                            (pet-system-bin-dir)))))
                       (exec-path (list venv-bin))
                       (process-environment (copy-sequence process-environment))
                       (tramp-cache-data (pet-deep-copy-hash-table tramp-cache-data))
                       (tramp-remote-path (cons venv-bin (copy-sequence tramp-remote-path))))
             (if (file-remote-p default-directory)
                 (tramp-flush-connection-property (tramp-dissect-file-name default-directory) "remote-path")
               (setenv "PATH" (string-join exec-path path-separator)))
             (pet--executable-find executable t)))
          ((if (or search-globally pet-search-globally)
               nil
             (throw 'done nil)))
          ((pet--executable-find "pyenv" t)
           (pet-run-process-get-line "pyenv" "which" executable))
          (t (or (pet--executable-find executable t)
                 (pet--executable-find (concat executable "3") t))))))


;;;###autoload
(defun pet-virtualenv-root ()
  "Find the path to the virtualenv for the current Python project.

Selects a virtualenv in the following order:

1. Cached virtualenv path (from previous detection or manual switching).
2. The value of the environment variable `VIRTUAL_ENV' if defined.
3. Poetry virtualenv from `pyproject.toml'.
4. Hatch virtualenv from `hatch.toml' or `pyproject.toml'.
5. Pipenv virtualenv from `Pipfile'.
6. A directory in `pet-venv-dir-names' in the project root if found.
7. Pyenv virtualenv from `.python-version'."
  (let ((root (pet-project-root)))
    (or (pet-cache-get (list root :virtualenv))
        (let ((venv-path
               (cond ((when-let* ((ev (getenv "VIRTUAL_ENV")))
                        (expand-file-name ev)))
                     ((when-let* ((program (pet-use-poetry-p))
                                  (default-directory (file-name-directory (pet-pyproject-path))))
                        (pet-run-process-get-output program "env" "info" "--no-ansi" "--path")))
                     ((when-let* ((program (pet-use-hatch-p))
                                  (config-file (or (pet-hatch-path) (pet-pyproject-path)))
                                  (default-directory (file-name-directory config-file)))
                        (pet-run-process-get-output program "env" "find" "default")))
                     ((when-let* ((program (pet-use-pipenv-p))
                                  (default-directory (file-name-directory (pet-pipfile-path))))
                        (pet-run-process-get-output program "--quiet" "--venv")))
                     ((when-let* ((dir (cl-loop for name in pet-venv-dir-names
                                                with dir = nil
                                                if (setq dir (locate-dominating-file default-directory name))
                                                return (file-name-as-directory (concat dir name)))))
                        (expand-file-name dir)))
                     ((when-let* ((program (pet-use-pyenv-p))
                                  (default-directory (file-name-directory (pet-python-version-path))))
                        (file-truename (pet-run-process-get-output program "prefix")))))))
          ;; root maybe nil when not in a project, this avoids caching a nil
          (when root
            (pet-cache-put (list root :virtualenv) venv-path))
          venv-path))))

(cl-defmacro pet-def-env-list (name &key args parse-output)
  "Define an environment listing function.

NAME is the environment manager's name.  It will be used to look up a
`pet-use-NAME-p' function that determines whether this environment
manager is applicable to the current project, and the path of the
environment manager program.

The other required keyboard arguments are:

ARGS is a list of command-line argument strings to the environment
manager program to display a listing of environments.

PARSE-OUTPUT is an expression that parses the output of the environment
manager program, made available as the variable `output', for a list of
environments.  This expression must return a list of strings."
  (declare (indent defun))
  (let ((env-list-fn (intern (format "pet-%s-environments" name)))
        (use-prog-fn (intern (format "pet-use-%s-p" name)))
        (docstring (format "The list of environments managed by `%s'." name)))
    `(defun ,env-list-fn ()
       ,docstring
       (when-let* ((program (,use-prog-fn))
                   (output (pet-run-process-get-output program ,@args)))
         (condition-case err
             ,parse-output
           (error (pet-report-error err)))))))

(pet-def-env-list pixi
  :args ("info" "--json")
  :parse-output
  (mapcar (lambda (env) (let-alist env .prefix))
          (let-alist (pet-parse-json output) .environments_info)))

(pet-def-env-list conda
  :args ("info" "--json")
  :parse-output
  (let-alist (pet-parse-json output) .envs))

(pet-def-env-list mamba
  :args ("info" "--envs" "--json")
  :parse-output
  (let-alist (pet-parse-json output) .envs))

(pet-def-env-list hatch
  :args ("env" "show" "--json")
  :parse-output
  (mapcar (lambda (env-name)
            (pet-run-process-get-output program "env" "find" env-name))
          (mapcar #'symbol-name (mapcar #'car (pet-parse-json output)))))

(cl-defmacro pet-def-env-switch (name &key env-list-fn prompt-text)
  "Define a environment switching function.

NAME is the environment manager name.

The following are the keyword arguments:

`ENV-LIST-FN' is the function name that returns available environments.
`PROMPT-TEXT' is the text to display in the completion prompt."
  (declare (indent defun))
  (let ((switch-fn (intern (format "pet-%s-switch-environment" name)))
        (name-str (symbol-name name))
        (docstring (format "Switch to a different %s environment and refresh buffer variables.

ENV should be the path to the %s environment directory as
returned by `%s'." name name env-list-fn)))
    `(defun ,switch-fn (env)
       ,docstring
       (interactive
        (list
         (completing-read ,prompt-text (,env-list-fn) nil t)))

       (when-let* ((root (pet-project-root)))
         (pet-cache-put (list root :virtualenv) env)

         (when-let* ((project-buffers (cl-loop for buffer in (buffer-list)
                                               when (with-current-buffer buffer
                                                      (and (buffer-file-name)
                                                           (string-prefix-p root (buffer-file-name))
                                                           (derived-mode-p 'python-base-mode 'python-mode)))
                                               collect buffer)))
           (dolist (buffer project-buffers)
             (with-current-buffer buffer
               (pet-buffer-local-vars-teardown)
               (pet-buffer-local-vars-setup)))

           (message "Switched to %s environment: %s" ,name-str env))))))

(pet-def-env-switch pixi
  :env-list-fn pet-pixi-environments
  :prompt-text "Please select a pixi environment: ")

(pet-def-env-switch conda
  :env-list-fn pet-conda-environments
  :prompt-text "Please select a conda environment: ")

(pet-def-env-switch mamba
  :env-list-fn pet-mamba-environments
  :prompt-text "Please select a mamba environment: ")

(pet-def-env-switch hatch
  :env-list-fn pet-hatch-environments
  :prompt-text "Please select a hatch environment: ")



(defvar flycheck-mode)
(defvar flycheck-python-mypy-config)
(defvar flycheck-pylintrc)
(defvar flycheck-python-flake8-executable)
(defvar flycheck-python-pylint-executable)
(defvar flycheck-python-mypy-executable)
(defvar flycheck-python-pyright-executable)
(defvar flycheck-python-pycompile-executable)
(defvar flycheck-python-ruff-executable)

(defun pet-flycheck-python-pylint-find-pylintrc ()
  "Polyfill `flycheck-pylintrc'.

Find the correct `pylint' configuration file according to the
algorithm described at
`https://pylint.pycqa.org/en/latest/user_guide/usage/run.html'."
  (let* ((pylintrc '("pylintrc" ".pylintrc" "pyproject.toml" "setup.cfg"))
         (found     (cond ((cl-loop for f in pylintrc
                                    with path = nil
                                    do (setq path (concat default-directory f))
                                    if (file-exists-p path)
                                    return (expand-file-name path)))
                          ((and (buffer-file-name)
                                (file-exists-p (concat (file-name-directory (buffer-file-name)) "__init__.py")))
                           (when-let* ((path (cl-loop for f in pylintrc
                                                      with dir = nil
                                                      do (setq dir (locate-dominating-file default-directory f))
                                                      if dir
                                                      return (concat dir f))))
                             (expand-file-name path))))))
    (if found
        found
      (cond ((when-let* ((ev (getenv "PYLINTRC"))
                         (path (expand-file-name ev)))
               (and (file-exists-p path) path)))
            ((let* ((ev (getenv "XDG_CONFIG_HOME"))
                    (config-dir
                     (or (and ev (file-name-as-directory ev))
                         "~/.config/"))
                    (xdg-file-path (expand-file-name (concat config-dir "pylintrc"))))
               (and (file-exists-p xdg-file-path) xdg-file-path)))
            ((let ((home-dir-pylintrc (expand-file-name "~/.pylintrc")))
               (and (file-exists-p home-dir-pylintrc) home-dir-pylintrc)))
            (t "/etc/pylintrc")))))

(defun pet-flycheck-toggle-local-vars ()
  "Toggle buffer local variables for `flycheck' Python checkers.

When `flycheck-mode' is non-nil, set up all supported Python
checker executable variables buffer-locally.  Reset them to
default otherwise."
  (if (bound-and-true-p flycheck-mode)
      (progn
        (when (derived-mode-p (if (functionp 'python-base-mode) 'python-base-mode 'python-mode))
          (setq-local flycheck-python-mypy-config `("mypy.ini" ".mypy.ini" "pyproject.toml" "setup.cfg"
                                                    ,(expand-file-name
                                                      (concat
                                                       (or (when-let* ((xdg-config-home (getenv "XDG_CONFIG_HOME")))
                                                             (file-name-as-directory xdg-config-home))
                                                           "~/.config/")
                                                       "mypy/config"))
                                                    ,(expand-file-name "~/.mypy.ini")))
          (setq-local flycheck-pylintrc (pet-flycheck-python-pylint-find-pylintrc))
          (setq-local flycheck-python-flake8-executable (pet-executable-find "flake8"))
          (setq-local flycheck-python-pylint-executable (pet-executable-find "pylint"))
          (setq-local flycheck-python-mypy-executable (pet-executable-find "mypy"))
          (setq-local flycheck-python-pyright-executable (pet-executable-find "pyright"))
          (setq-local flycheck-python-ruff-executable (pet-executable-find "ruff"))
          (let ((python-path (pet-executable-find "python")))
            (setq-local flycheck-python-mypy-python-executable python-path)
            (setq-local flycheck-python-pycompile-executable python-path))))
    (kill-local-variable 'flycheck-python-mypy-config)
    (kill-local-variable 'flycheck-pylintrc)
    (kill-local-variable 'flycheck-python-flake8-executable)
    (kill-local-variable 'flycheck-python-pylint-executable)
    (kill-local-variable 'flycheck-python-mypy-executable)
    (kill-local-variable 'flycheck-python-mypy-python-executable)
    (kill-local-variable 'flycheck-python-pyright-executable)
    (kill-local-variable 'flycheck-python-pycompile-executable)
    (kill-local-variable 'flycheck-python-ruff-executable)))

(defun pet-flycheck-python-find-project-root-advice (_)
  "Delegate `flycheck-python-find-project-root' to `pet-virtualenv-root'."
  (pet-virtualenv-root))

;;;###autoload
(defun pet-flycheck-setup ()
  "Set up all `flycheck' Python checker configuration."
  (advice-add 'flycheck-python-find-project-root :override #'pet-flycheck-python-find-project-root-advice)
  (add-hook 'flycheck-mode-hook #'pet-flycheck-toggle-local-vars))

;;;###autoload
(defun pet-flycheck-teardown ()
  "Reset all `flycheck' Python checker configuration to default."
  (advice-remove 'flycheck-python-find-project-root #'pet-flycheck-python-find-project-root-advice)
  (remove-hook 'flycheck-mode-hook #'pet-flycheck-toggle-local-vars)
  (kill-local-variable 'flycheck-python-mypy-config)
  (kill-local-variable 'flycheck-pylintrc)
  (kill-local-variable 'flycheck-python-flake8-executable)
  (kill-local-variable 'flycheck-python-pylint-executable)
  (kill-local-variable 'flycheck-python-mypy-executable)
  (kill-local-variable 'flycheck-python-mypy-python-executable)
  (kill-local-variable 'flycheck-python-pyright-executable)
  (kill-local-variable 'flycheck-python-pycompile-executable)
  (kill-local-variable 'flycheck-python-ruff-executable))



(defvar eglot-workspace-configuration)
(declare-function jsonrpc--process "ext:jsonrpc")
(declare-function eglot--executable-find "ext:eglot")
(declare-function eglot--workspace-configuration-plist "ext:eglot")
(declare-function eglot--guess-contact "ext:eglot")

(defun pet-eglot--executable-find-advice (command &optional remote)
  "Advice for `eglot--executable-find' in Eglot < 1.17.30 and > 1.18.

COMMAND is the executable command to find, REMOTE is the optional
remote flag.

Delegates to `pet-executable-find' first for all executables, then
falls back to the original `executable-find' implementation."
  (or (pet-executable-find command)
      (pet--executable-find command remote)))

(defalias 'pet--ensure-list 'ensure-list)
(eval-when-compile
  (unless (functionp 'ensure-list)
    (defun pet--ensure-list (object)
      (declare (side-effect-free error-free))
      (if (listp object)
          object
        (list object)))))

(defun pet-lookup-eglot-server-initialization-options (command)
  "Return LSP initializationOptions for Eglot.

COMMAND is the name of the Python language server command.

COMMAND can be a list of strings.  If this is the case, each string is
searched for a supported LSP server command."
  (seq-find
   'identity
   (mapcar
    (lambda (command)
      (cond
       ((not
         (stringp command))
        'nil)
       ((string-match "pylsp" command)
        (let nil
          `(:pylsp
            (:plugins
             (:jedi
              (:environment ,(pet-virtualenv-root))
              :ruff
              (:executable ,(pet-executable-find "ruff"))
              :pylsp_mypy
              (:overrides
               ["--python-executable"
                (\,
                 (pet-executable-find "python"))
                t])
              :flake8
              (:executable ,(pet-executable-find "flake8"))
              :pylint
              (:executable ,(pet-executable-find "pylint")))))))
       ((string-match "pyls" command)
        (let nil
          `(:pyls
            (:plugins
             (:jedi
              (:environment ,(pet-virtualenv-root))
              :pylint
              (:executable ,(pet-executable-find "pylint")))))))
       ((string-match "basedpyright-langserver" command)
        (let nil
          `(:python
            (:pythonPath
             ,(pet-executable-find "python")
             :venvPath
             ,(pet-virtualenv-root)))))
       ((string-match "pyright-langserver" command)
        (let nil
          `(:python
            (:pythonPath
             ,(pet-executable-find "python")
             :venvPath
             ,(pet-virtualenv-root)))))
       ((string-match "jedi-language-server" command)
        (let nil
          `(:jedi
            (:executable
             (:command ,(pet-executable-find "jedi-language-server"))
             :workspace
             (:environmentPath ,(pet-executable-find "python"))))))
       ((string-match "ruff" command)
        (let nil
          `(:settings
            (:interpreter
             ,(pet-executable-find "python")
             :path
             ,(pet-executable-find "ruff")))))
       ((string-match "ruff-lsp" command)
        (let nil
          `(:settings
            (:interpreter
             ,(pet-executable-find "python")
             :path
             ,(pet-executable-find "ruff")))))
       (t 'nil)))
    (pet--ensure-list command))))

(defun pet--plistp (object)
  "Non-nil if and only if OBJECT is a valid plist."
  (let ((len (proper-list-p object)))
    (and len
         (zerop (% len 2))
         (seq-every-p
          (lambda (kvp)
            (keywordp (car kvp)))
          (seq-split object 2)))))

(defun pet-merge-eglot-initialization-options (a b)
  "Deep merge plists A and B."
  (map-merge-with 'plist
                  (lambda (c d)
                    (cond ((and (pet--plistp c) (pet--plistp d))
                           (pet-merge-eglot-initialization-options c d))
                          ((and (vectorp c) (vectorp d))
                           (vconcat (seq-union c d)))
                          (t d)))
                  (copy-tree a t)
                  (copy-tree b t)))

(defun pet-eglot--workspace-configuration-plist-advice (fn &rest args)
  "Enrich `eglot-workspace-configuration' with paths found by `pet'.

FN is `eglot--workspace-configuration-plist', ARGS is the
arguments to `eglot--workspace-configuration-plist'."
  (let* ((path (cadr args))
         ;; Ensure directory paths have trailing slash. See: https://github.com/joaotavora/eglot/pull/1281
         (canonical-path (if (and path (file-directory-p path))
                             (file-name-as-directory path)
                           path))
         (server (car args))
         (command (process-command (jsonrpc--process server)))
         (pet-config (pet-lookup-eglot-server-initialization-options command))
         (user-config (apply fn server (and canonical-path (cons canonical-path (cddr args))))))
    (pet-merge-eglot-initialization-options user-config pet-config)))

(defvar pet-known-python-language-servers '("pylsp" "pyls" "basedpyright-langserver"
                                            "pyright-langserver" "jedi-language-server"
                                            "ruff" "ruff-lsp" "ty" "pyrefly"))

(defun pet-resolve-python-server-executables (program-with-args)
  "Replace known Python language server names with their resolved paths.

PROGRAM-WITH-ARGS is a list where the first element is typically the
server executable name, followed by command-line arguments.

Returns a new list where any known Python language server executable
names are replaced with their resolved paths from `pet-executable-find',
falling back to the original name if not found."
  (mapcar (lambda (arg)
            (or (and (stringp arg)
                     (member arg pet-known-python-language-servers)
                     (pet-executable-find arg))
                arg))
          program-with-args))

(defun pet--process-guess-contact-result (result)
  "Merge PET's initialization options to Eglot's contacts.

RESULT is the result of `eglot--guess-contact'.

Processes the contact information returned by `eglot--guess-contact' and
merges PET's server initialization options for Python language servers.
Handles various contact formats including program contacts, TCP
connections, and autoport configurations."
  (let* ((contact (nth 3 result))
         (contact (if (functionp contact) (funcall contact) contact)))
    (pcase contact
      ;; Skip processing for non-program contacts
      (`(,(pred keywordp) . ,_)                    ; Process initargs: (:process ...)
       result)
      (`(,_host ,(pred integerp) . ,_)             ; TCP connection: ("host" port ...)
       result)
      (`(,_prog . ,(and rest (guard (cl-find-if   ; Autoport: ("prog" :autoport ...)
                                     (lambda (x)
                                       (or (eq x :autoport)
                                           (eq (car-safe x) :autoport)))
                                     rest))))
       result)
      ;; Empty contact - return unchanged
      ('nil
       result)
      ;; Process program contacts
      ((pred listp)
       (let* ((probe (cl-position-if #'keywordp contact))
              (raw-program-with-args (seq-subseq contact 0 (or probe (length contact))))
              (program-with-args (pet-resolve-python-server-executables raw-program-with-args))
              (kwargs (copy-sequence (and probe (seq-subseq contact probe (length contact)))))
              (init-opts (plist-get kwargs :initializationOptions))
              (pet-config (pet-lookup-eglot-server-initialization-options program-with-args)))
         (if (or init-opts pet-config)
             (append (seq-subseq result 0 3)
                     (list
                      (append
                       program-with-args
                       (plist-put
                        kwargs
                        :initializationOptions
                        (if (functionp init-opts)
                            (lambda (server)
                              (pet-merge-eglot-initialization-options
                               (funcall init-opts server)
                               pet-config))
                          (pet-merge-eglot-initialization-options
                           init-opts
                           pet-config)))))
                     (seq-subseq result 4))
           result)))
      (_ result))))

(defun pet-eglot--guess-contact-advice (fn &rest args)
  "Advice for the `eglot--guess-contact' in Eglot.

FN is `eglot--guess-contact', ARGS is the arguments to
`eglot--guess-contact'.

Re-routes `executable-find' to `pet-executable-find'.

Returns the result of `eglot--guess-contact' with PET's server
initialization options merged to the contacts."
  (let ((result (cl-letf (((symbol-function #'executable-find)
                           (symbol-function #'pet-eglot--executable-find-advice)))
                  (apply fn args))))
    (pet--process-guess-contact-result result)))

(defun pet-eglot-setup ()
  "Set up Eglot to use server executables and virtualenvs found by PET."
  (advice-add 'eglot--workspace-configuration-plist :around #'pet-eglot--workspace-configuration-plist-advice)
  (advice-add 'eglot--guess-contact :around #'pet-eglot--guess-contact-advice)
  (when (fboundp 'eglot--executable-find)
    (advice-add 'eglot--executable-find :override #'pet-eglot--executable-find-advice)))

(defun pet-eglot-teardown ()
  "Tear down PET advices to Eglot."
  (advice-remove 'eglot--workspace-configuration-plist #'pet-eglot--workspace-configuration-plist-advice)
  (advice-remove 'eglot--guess-contact #'pet-eglot--guess-contact-advice)
  (when (fboundp 'eglot--executable-find)
    (advice-remove 'eglot--executable-find #'pet-eglot--executable-find-advice)))


(defvar dape-command)
(defvar dape-cwd-function)

(defun pet-dape-setup ()
  "Set up the buffer local variables for `dape'."
  (if-let* ((main (pet-find-file-from-project "__main__.py"))
            (module (let* ((dir (file-name-directory main))
                           (dir-file-name (directory-file-name dir))
                           (module))
                      (while (file-exists-p (concat dir "__init__.py"))
                        (push (file-name-nondirectory dir-file-name) module)
                        (setq dir (file-name-directory dir-file-name))
                        (setq dir-file-name (directory-file-name dir)))
                      (string-join module "."))))
      (setq-local dape-command `(debugpy-module command ,(pet-executable-find "python") :module ,module))
    (setq-local dape-command `(debugpy command ,(pet-executable-find "python"))))
  (setq-local dape-cwd-function #'pet-project-root))

(defun pet-dape-teardown ()
  "Tear down the buffer local variables for `dape'."
  (kill-local-variable 'dape-command)
  (kill-local-variable 'dape-cwd-function))



(defvar lsp-jedi-executable-command)
(defvar lsp-pyls-plugins-jedi-environment)
(defvar lsp-pyls-server-command)
(defvar lsp-pylsp-plugins-jedi-environment)
(defvar lsp-pylsp-server-command)
(defvar lsp-pyright-langserver-command)
(defvar lsp-pyright-python-executable-cmd)
(defvar lsp-pyright-venv-path)
(defvar lsp-python-ty-clients-server-command)
(defvar lsp-ruff-server-command)
(defvar lsp-ruff-python-path)
(defvar dap-python-executable)
(defvar dap-variables-project-root-function)
(defvar python-pytest-executable)
(defvar python-black-command)
(defvar python-isort-command)
(defvar ruff-format-command)
(defvar blacken-executable)
(defvar yapfify-executable)
(defvar py-autopep8-command)
(defvar format-all--executable-table)
(defvar apheleia-formatters)
(defvar pytest-global-name)

(defun pet-buffer-local-vars-setup ()
  "Set up the buffer local variables for Python tools.

Assign all supported Python tooling executable variables to
buffer local values."
  (setq-local python-shell-interpreter (pet-executable-find (default-value 'python-shell-interpreter)))
  (setq-local python-shell-virtualenv-root (pet-virtualenv-root))

  (pet-flycheck-setup)

  (let ((python (pet-executable-find "python"))
        (black (pet-executable-find "black"))
        (isort (pet-executable-find "isort"))
        (ruff (pet-executable-find "ruff"))
        (yapf (pet-executable-find "yapf"))
        (pytest (pet-executable-find "pytest")))

    (setq-local lsp-jedi-executable-command
                (pet-executable-find "jedi-language-server"))
    (setq-local lsp-pyls-plugins-jedi-environment python-shell-virtualenv-root)
    (setq-local lsp-pyls-server-command (list (pet-executable-find "pyls")))
    (setq-local lsp-pylsp-plugins-jedi-environment python-shell-virtualenv-root)
    (setq-local lsp-pylsp-server-command (list (pet-executable-find "pylsp")))
    (setq-local lsp-pyright-langserver-command (pet-executable-find "pyright"))
    (setq-local lsp-pyright-python-executable-cmd python)
    (setq-local lsp-pyright-venv-path python-shell-virtualenv-root)
    (setq-local lsp-python-ty-clients-server-command (list (pet-executable-find "ty") "server"))
    (setq-local lsp-ruff-server-command (list ruff "server"))
    (setq-local lsp-ruff-python-path python)
    (setq-local dap-python-executable python)
    (setq-local dap-variables-project-root-function #'pet-project-root)
    (setq-local python-pytest-executable pytest)
    (setq-local pytest-global-name pytest)
    (setq-local python-black-command black)
    (setq-local python-isort-command isort)
    (setq-local ruff-format-command ruff)
    (setq-local blacken-executable black)
    (setq-local yapfify-executable yapf)
    (setq-local py-autopep8-command (pet-executable-find "autopep8"))

    (when (boundp 'format-all--executable-table)
      (setq-local format-all--executable-table
                  (copy-hash-table format-all--executable-table))
      (puthash 'black black format-all--executable-table)
      (puthash 'isort isort format-all--executable-table)
      (puthash 'ruff ruff format-all--executable-table)
      (puthash 'yapf yapf format-all--executable-table))

    (when (boundp 'apheleia-formatters)
      (setq-local apheleia-formatters (copy-tree apheleia-formatters))
      (setcar (alist-get 'black apheleia-formatters) black)
      (setcar (alist-get 'isort apheleia-formatters) isort)
      (setcar (alist-get 'ruff apheleia-formatters) ruff)
      (setcar (alist-get 'ruff-isort apheleia-formatters) ruff)
      (setcar (alist-get 'yapf apheleia-formatters) yapf)))

  (pet-eglot-setup)
  (pet-dape-setup)

  (run-hooks 'pet-after-buffer-local-vars-setup))

(defun pet-buffer-local-vars-teardown ()
  "Reset all supported buffer local variable values to default."

  (run-hooks 'pet-before-buffer-local-vars-teardown)

  (kill-local-variable 'python-shell-interpreter)
  (kill-local-variable 'python-shell-virtualenv-root)

  (pet-flycheck-teardown)

  (kill-local-variable 'lsp-jedi-executable-command)
  (kill-local-variable 'lsp-pyls-plugins-jedi-environment)
  (kill-local-variable 'lsp-pyls-server-command)
  (kill-local-variable 'lsp-pylsp-plugins-jedi-environment)
  (kill-local-variable 'lsp-pylsp-server-command)
  (kill-local-variable 'lsp-pyright-langserver-command)
  (kill-local-variable 'lsp-pyright-python-executable-cmd)
  (kill-local-variable 'lsp-pyright-venv-path)
  (kill-local-variable 'lsp-python-ty-clients-server-command)
  (kill-local-variable 'lsp-ruff-python-path)
  (kill-local-variable 'lsp-ruff-server-command)
  (kill-local-variable 'dap-python-executable)
  (kill-local-variable 'dap-variables-project-root-function)
  (kill-local-variable 'python-pytest-executable)
  (kill-local-variable 'python-black-command)
  (kill-local-variable 'python-isort-command)
  (kill-local-variable 'ruff-format-command)
  (kill-local-variable 'blacken-executable)
  (kill-local-variable 'yapfify-executable)
  (kill-local-variable 'py-autopep8-command)
  (kill-local-variable 'format-all--executable-table)
  (kill-local-variable 'apheleia-formatters)
  (kill-local-variable 'pytest-global-name)

  (pet-eglot-teardown)
  (pet-dape-teardown))

(defun pet-verify-setup ()
  "Verify the values of buffer local variables visually.

Print all of the buffer local variable values `pet-mode'
has assigned to."
  (interactive)

  (unless (derived-mode-p 'python-base-mode 'python-mode)
    (user-error "You are not in python-mode!"))

  (let ((kvp (mapcar (lambda (sym)
                       (cons sym
                             (if (boundp sym)
                                 (let ((val (symbol-value sym)))
                                   (if (consp val)
                                       (apply #'string-join
                                              (mapcar (apply-partially #'abbreviate-file-name)
                                                      (mapcar (apply-partially #'format "%s") val))
                                              (list ", "))
                                     (abbreviate-file-name (format "%s" val))))
                               'unbound)))
                     '(python-shell-interpreter
                       python-shell-virtualenv-root
                       flycheck-python-flake8-executable
                       flycheck-pylintrc
                       flycheck-python-pylint-executable
                       flycheck-python-mypy-executable
                       flycheck-python-mypy-config
                       flycheck-python-mypy-python-executable
                       flycheck-python-pyright-executable
                       flycheck-python-pycompile-executable
                       flycheck-python-ruff-executable
                       lsp-jedi-executable-command
                       lsp-pyls-plugins-jedi-environment
                       lsp-pyls-server-command
                       lsp-pylsp-plugins-jedi-environment
                       lsp-pylsp-server-command
                       lsp-pyright-python-executable-cmd
                       lsp-pyright-venv-path
                       lsp-python-ty-clients-server-command
                       lsp-ruff-server-command
                       lsp-ruff-python-path
                       dap-python-executable
                       dap-variables-project-root-function
                       dape-command
                       dape-cwd-function
                       python-pytest-executable
                       pytest-global-name
                       python-black-command
                       blacken-executable
                       python-isort-command
                       ruff-format-command
                       yapfify-executable
                       py-autopep8-command)))
        (buf (current-buffer)))

    (with-current-buffer-window "*pet info*" nil nil
      (insert (propertize (format "%-40s" "(pet-project-root):") 'face 'font-lock-variable-name-face))
      (insert (concat (pet-project-root) "\n"))

      (insert (propertize (format "%-40s" "(pet-virtualenv-root):") 'face 'font-lock-variable-name-face))
      (insert (concat (pet-virtualenv-root) "\n"))

      (mapc (pcase-lambda (`(,key . ,value))
              (insert (propertize (format "%-40s" (concat (symbol-name key) ":")) 'face 'font-lock-variable-name-face))
              (insert (format "%s" value))
              (insert "\n"))
            kvp)

      (insert (propertize (format "%-40s" "apheleia-formatters:")
                          'face 'font-lock-variable-name-face))
      (apply 'insert
             (with-current-buffer buf
               (if (boundp 'apheleia-formatters)
                   (list "\n"
                         (pp-to-string
                          (mapcar (lambda (key) (alist-get key apheleia-formatters))
                                  '(black isort ruff ruff-isort yapf))))
                 (list (format "%s\n" 'unbound)))))

      (insert (propertize (format "%-40s" "format-all--executable-table:")
                          'face 'font-lock-variable-name-face))
      (apply 'insert
             (with-current-buffer buf
               (if (boundp 'format-all--executable-table)
                   (list "\n"
                         (pp-to-string
                          (mapcar (lambda (key) (alist-get key format-all--executable-table))
                                  '(black isort ruff yapf))))
                 (list (format "%s\n" 'unbound)))))

      (insert (propertize (format "%-40s" "(eglot--guess-contact):") 'face 'font-lock-variable-name-face))
      (apply 'insert
             (with-current-buffer buf
               (if (fboundp 'eglot--guess-contact)
                   (list "\n" (pp-to-string (eglot--guess-contact)))
                 (list (format "%s\n" 'unbound)))))

      (insert (propertize (format "%-40s"
                                  (concat (symbol-name (if (file-remote-p default-directory)
                                                           'tramp-remote-path
                                                         'exec-path))
                                          ":"))
                          'face 'font-lock-variable-name-face) "\n")
      (mapc (lambda (dir)
              (insert (abbreviate-file-name (format "%s" dir)) "\n"))
            (if (file-remote-p default-directory)
                tramp-remote-path
              exec-path))
      (special-mode))))

;;;###autoload
(define-minor-mode pet-mode
  "Minor mode to set up buffer local variables for Python tools."
  :lighter " Pet"
  :group 'pet
  (if pet-mode
      (progn
        (cl-block nil
          (unless (pet-cache-get (list (pet-project-root) :virtualenv))
            (cond ((pet-use-pixi-p)
                   (call-interactively #'pet-pixi-switch-environment)
                   (cl-return))
                  ((pet-use-conda-p)
                   (call-interactively #'pet-conda-switch-environment)
                   (cl-return))
                  ((pet-use-mamba-p)
                   (call-interactively #'pet-mamba-switch-environment)
                   (cl-return))))
          (pet-buffer-local-vars-setup))
        (add-hook 'kill-buffer-hook #'pet-cleanup-watchers-and-caches t))
    (pet-buffer-local-vars-teardown)
    (remove-hook 'kill-buffer-hook #'pet-cleanup-watchers-and-caches t)))

(defun pet-cleanup-watchers-and-caches ()
  "Clean up configuration file caches and watchers.

Delete configuration file caches and watchers when all
`python-mode' buffers of a project have been closed."
  (when (and (buffer-file-name)
             (derived-mode-p 'python-base-mode 'python-mode))
    (when-let* ((root (pet-project-root)))
      (unless (cl-loop for buf in (buffer-list)
                       if (and (not (equal buf (current-buffer)))
                               (buffer-file-name buf)
                               (string-prefix-p root (buffer-file-name buf))
                               (with-current-buffer buf
                                 (derived-mode-p 'python-base-mode 'python-mode)))
                       return buf)
        (pet-cache--clear-project-internal root)))

    (unless (cl-loop for buf in (buffer-list)
                     if (and (not (equal buf (current-buffer)))
                             (with-current-buffer buf
                               (derived-mode-p 'python-base-mode 'python-mode)))
                     return buf)
      (pet-cache--clear-precommit-internal))))

(provide 'pet)

;;; pet.el ends here
