;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-use-hatch-p"
  (describe "when the project has a `hatch.toml' file"
    (before-each
      (spy-on 'pet-pyproject)
      (spy-on 'pet-hatch :and-return-value '((envs (default (dependencies ("pytest")))))))

    (it "should return `hatch' path if `hatch' is found"
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/hatch")
      (expect (pet-use-hatch-p) :to-equal "/usr/bin/hatch"))

    (it "should return nil if `hatch' is not found"
      (spy-on 'pet--executable-find)
      (expect (pet-use-hatch-p) :to-be nil)))

  (describe "when the `pyproject.toml' file has `[tool.hatch.envs]` section"
    (before-each
      (spy-on 'pet-hatch)
      (spy-on 'pet-pyproject :and-return-value '((tool (hatch (envs (default (dependencies ("pytest")))))))))

    (it "should return `hatch' path if `hatch' is found"
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/hatch")
      (expect (pet-use-hatch-p) :to-equal "/usr/bin/hatch"))

    (it "should return nil if `hatch' is not found"
      (spy-on 'pet--executable-find)
      (expect (pet-use-hatch-p) :to-be nil)))

  (describe "when the project has neither `hatch.toml' nor `[tool.hatch.envs]` in `pyproject.toml'"
    (before-each
      (spy-on 'pet-hatch)
      (spy-on 'pet-pyproject :and-return-value '((build-system (build-backend . "setuptools")))))

    (it "should return nil if `hatch' is found"
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/hatch")
      (expect (pet-use-hatch-p) :to-be nil))

    (it "should return nil if `hatch' is not found"
      (spy-on 'pet--executable-find)
      (expect (pet-use-hatch-p) :to-be nil)))

  (describe "when the project has no configuration files"
    (before-each
      (spy-on 'pet-hatch)
      (spy-on 'pet-pyproject))

    (it "should return nil regardless of `hatch' availability"
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/hatch")
      (expect (pet-use-hatch-p) :to-be nil))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
