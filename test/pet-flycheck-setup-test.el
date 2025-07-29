;; -*- lexical-binding: t; -*-

(require 'pet)

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

