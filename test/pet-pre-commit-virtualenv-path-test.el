;; -*- lexical-binding: t; -*-

(require 'pet)

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
      (spy-on 'file-notify-add-watch)
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
      (setq-local pet-pre-commit-database-cache pre-commit-db-content))

    (after-each
      (kill-local-variable 'pet-pre-commit-database-cache))

    (it "should return absolute path to the virtualenv of a `pre-commit' hook with additional dependencies"
      (spy-on 'file-expand-wildcards :and-return-value '("/home/user/.cache/pre-commit/repofurqd1rq/py_env-python3.9"
                                                         "/home/user/.cache/pre-commit/repofurqd1rq/py_env-python3.10"))
      (expect (pet-pre-commit-virtualenv-path "flake8") :to-equal "/home/user/.cache/pre-commit/repofurqd1rq/py_env-python3.10"))

    (it "should return absolute path to the virtualenv of a `pre-commit' hook with no additional dependencies"
      (spy-on 'file-expand-wildcards :and-return-value '("/home/user/.cache/pre-commit/repo85no_p81/py_env-python3.9"))
      (expect (pet-pre-commit-virtualenv-path "black") :to-equal "/home/user/.cache/pre-commit/repo85no_p81/py_env-python3.9"))))

