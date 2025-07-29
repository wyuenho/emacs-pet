;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-use-pyenv-p"
  (describe "when the project has a `.python-version' file"
    (before-each
      (spy-on 'pet-python-version :and-return-value t))

    (it "should return `pyenv' path if `pyenv' is found"
      (spy-on 'executable-find :and-return-value "/usr/bin/pyenv")
      (expect (pet-use-pyenv-p) :to-equal "/usr/bin/pyenv"))

    (it "should return `pyenv' path if `pyenv' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pyenv-p) :to-be nil)))

  (describe "when the project does not have a `.python-version' file"
    (before-each
      (spy-on 'pet-python-version))

    (it "should return nil if `pyenv' is found"
      (spy-on 'executable-find :and-return-value "/usr/bin/pyenv")
      (expect (pet-use-pyenv-p) :to-be nil))

    (it "should return nil if `pyenv' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pyenv-p) :to-be nil))))

