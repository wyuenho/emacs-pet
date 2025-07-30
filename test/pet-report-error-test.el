;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-report-error"
  (describe "when `pet-debug' is t"
    (before-each
      (setq-local pet-debug t))

    (after-each
      (kill-local-variable 'pet-debug))

    (it "should call minibuffer-message"
      (buttercup-suppress-warning-capture
        (spy-on 'minibuffer-message))
      (pet-report-error '(error . ("error")))
      (expect 'minibuffer-message :to-have-been-called-with "error")))

  (it "should not call minibuffer-message when `pet-debug' is nil"
    (pet-report-error '(error . ("error")))
    (expect 'minibuffer-message :not :to-have-been-called)))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
