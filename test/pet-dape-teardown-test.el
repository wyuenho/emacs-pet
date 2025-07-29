;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-dape-teardown"
  (it "should tear down bufer local variables for dape"
    (spy-on 'pet-find-file-from-project-root-recursively)
    (spy-on 'pet-executable-find :and-return-value "/usr/bin/python")
    (pet-dape-setup)
    (pet-dape-teardown)
    (expect (local-variable-p 'dape-command) :not :to-be-truthy)
    (expect (local-variable-p 'dape-cwd-function) :not :to-be-truthy)))

