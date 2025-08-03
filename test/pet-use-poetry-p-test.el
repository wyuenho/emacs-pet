;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-use-poetry-p"
  (describe "when the `pyproject.toml' file in the project declares `poetry' as the build system"
    (before-each
      (spy-on 'pet-pyproject :and-return-value '((build-system (build-backend . "poetry.core.masonry.api")))))

    (it "should return `poetry' path if `poetry' is found"
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/poetry")
      (expect (pet-use-poetry-p) :to-equal "/usr/bin/poetry"))

    (it "should return nil if `poetry' is not found"
      (spy-on 'pet--executable-find)
      (expect (pet-use-poetry-p) :to-be nil)))

  (describe "when the `pyproject.toml' file in the project does not declare `poetry' as the build system"
    (before-each
      (spy-on 'pet-pyproject :and-return-value '((build-system (build-backend . "pdm")))))

    (it "should return nil if `poetry' is found"
      (expect (pet-use-poetry-p) :to-be nil))

    (it "should return nil if `poetry' is not found"
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/poetry")
      (expect (pet-use-poetry-p) :to-be nil)))

  (describe "when the project does not have a `pyproject.toml' file"
    (before-each
      (spy-on 'pet-pyproject))

    (it "should return nil if `poetry' is found"
      (expect (pet-use-poetry-p) :to-be nil))

    (it "should return nil if `poetry' is not found"
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/poetry")
      (expect (pet-use-poetry-p) :to-be nil))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
