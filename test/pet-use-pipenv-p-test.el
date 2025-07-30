;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-use-pipenv-p"
  (describe "when the project has a `Pipfile' file"
    (before-each
      (spy-on 'pet-pipfile :and-return-value t))

    (it "should return `pipenv' path if `pipenv' is found"
      (spy-on 'executable-find :and-return-value "/usr/bin/pipenv")
      (expect (pet-use-pipenv-p) :to-equal "/usr/bin/pipenv"))

    (it "should return nil path if `pipenv' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pipenv-p) :to-be nil)))

  (describe "when the project does not have a `Pipfile' file"
    (before-each
      (spy-on 'pet-pipfile))

    (it "should return nil path if `pipenv' is found"
      (spy-on 'executable-find :and-return-value "/usr/bin/pipenv")
      (expect (pet-use-pipenv-p) :to-be nil))

    (it "should return nil path if `pipenv' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-pipenv-p) :to-be nil))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
