;; -*- lexical-binding: t; -*-

(require 'pet)

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


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
