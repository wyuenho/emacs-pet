;; -*- lexical-binding: t; -*-

(require 'pet)

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
    (spy-on 'file-exists-p )
    (expect (pet-flycheck-python-pylint-find-pylintrc) :to-equal "/etc/pylintrc")))

