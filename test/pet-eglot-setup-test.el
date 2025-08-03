;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-eglot-setup"
  (before-each
    (pet-eglot-setup))

  (after-each
    (pet-eglot-teardown))

  (it "should advice eglot functions"
    (spy-on 'eglot--executable-find)
    (pet-eglot-setup)
    (expect (advice-member-p 'pet-eglot--workspace-configuration-plist-advice 'eglot--workspace-configuration-plist) :to-be-truthy)
    (expect (advice-member-p 'pet-eglot--executable-find-advice 'eglot--executable-find) :to-be-truthy)
    (expect (advice-member-p 'pet-eglot--guess-contact-advice 'eglot--guess-contact) :to-be-truthy)))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
