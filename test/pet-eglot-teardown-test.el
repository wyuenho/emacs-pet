;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-eglot-teardown"
  (it "should remove `pet' advices from eglot functions"
    (pet-eglot-setup)
    (pet-eglot-teardown)
    (expect (advice-member-p 'pet-eglot--workspace-configuration-plist-advice 'eglot--workspace-configuration-plist) :to-be nil)
    (expect (advice-member-p 'pet-eglot--executable-find-advice 'eglot--executable-find) :to-be nil)
    (expect (advice-member-p 'pet-eglot--guess-contact-advice 'eglot--guess-contact) :to-be nil)))

