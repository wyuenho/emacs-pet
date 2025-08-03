;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-use-pre-commit-p"
  (describe "when the project has a `.pre-commit-config.yaml' file"
    (before-each
      (spy-on 'pet-pre-commit-config :and-return-value t))

    (it "should return `pre-commit' path if `pre-commit' is found"
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/pre-commit")
      (expect (pet-use-pre-commit-p) :to-equal "/usr/bin/pre-commit")

      (spy-on 'pet-virtualenv-root :and-return-value "/home/user/venv/test")
      (let ((call-count 0))
        (spy-on 'pet--executable-find :and-call-fake (lambda (&rest _)
                                                  (setq call-count (1+ call-count))
                                                  (when (= call-count 2)
                                                    "/home/user/venv/test/bin/pre-commit"))))

      (expect (pet-use-pre-commit-p) :to-equal "/home/user/venv/test/bin/pre-commit"))

    (it "should return nil if `pre-commit' is not found"
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/pre-commit")
      (expect (pet-use-pre-commit-p) :to-equal "/usr/bin/pre-commit")))

  (describe "when the project does not have a `.pre-commit-config.yaml' file"
    (before-each
      (spy-on 'pet-pre-commit-config))

    (it "should return nil if `pre-commit' is found"
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/pre-commit")
      (expect (pet-use-pre-commit-p) :to-be nil))

    (it "should return nil if `pre-commit' is not found"
      (spy-on 'pet--executable-find)
      (expect (pet-use-pre-commit-p) :to-be nil))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
