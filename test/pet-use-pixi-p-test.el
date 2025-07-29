;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-use-pixi-p"
  (describe "when the project has a `pixi.toml' file"
    (before-each
      (spy-on 'pet-pyproject)
      (spy-on 'pet-pixi :and-return-value '((workspace (name . "test-project")))))

    (it "should return `pixi' path if `pixi' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "pixi") "/usr/bin/pixi")))
      (expect (pet-use-pixi-p) :to-equal "/usr/bin/pixi"))

    (it "should return nil if `pixi' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pixi-p) :to-be nil)))

  (describe "when the project has `tool.pixi' section in `pyproject.toml'"
    (before-each
      (spy-on 'pet-pixi)
      (spy-on 'pet-pyproject :and-return-value '((tool (pixi (environments (test . ("test"))))))))

    (it "should return `pixi' path if `pixi' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "pixi") "/usr/bin/pixi")))
      (expect (pet-use-pixi-p) :to-equal "/usr/bin/pixi"))

    (it "should return nil if `pixi' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pixi-p) :to-be nil)))

  (describe "when the project has both `pixi.toml' and `tool.pixi' in `pyproject.toml'"
    (before-each
      (spy-on 'pet-pixi :and-return-value '((workspace (name . "test-project"))))
      (spy-on 'pet-pyproject :and-return-value '((tool (pixi (environments (test . ("test"))))))))

    (it "should return `pixi' path if `pixi' is found (pixi.toml takes precedence)"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "pixi") "/usr/bin/pixi")))
      (expect (pet-use-pixi-p) :to-equal "/usr/bin/pixi"))

    (it "should return nil if `pixi' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pixi-p) :to-be nil)))

  (describe "when the project does not have `pixi.toml' or `tool.pixi' section"
    (before-each
      (spy-on 'pet-pixi)
      (spy-on 'pet-pyproject))

    (it "should return nil if `pixi' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "pixi") "/usr/bin/pixi")))
      (expect (pet-use-pixi-p) :to-be nil))

    (it "should return nil if `pixi' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pixi-p) :to-be nil)))

  (describe "when `pyproject.toml' has empty `tool.pixi' section"
    (before-each
      (spy-on 'pet-pixi)
      (spy-on 'pet-pyproject :and-return-value '((tool (pixi)))))

    (it "should return nil if `pixi' is found (empty tool.pixi is not valid)"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "pixi") "/usr/bin/pixi")))
      (expect (pet-use-pixi-p) :to-be nil))

    (it "should return nil if `pixi' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pixi-p) :to-be nil))))

