;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-use-mamba-p"
  (describe "when the project has an `environment[a-zA-Z0-9-_].yaml' file"
    (before-each
      (spy-on 'pet-environment :and-return-value t))

    (it "should return `mamba' path if `mamba' is found"
      (spy-on 'pet--executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "mamba") "/usr/bin/mamba")))
      (expect (pet-use-mamba-p) :to-equal "/usr/bin/mamba"))

    (it "should return `micromamba' path if `micromamba' is found"
      (spy-on 'pet--executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "micromamba") "/usr/bin/micromamba")))
      (expect (pet-use-mamba-p) :to-equal "/usr/bin/micromamba"))

    (it "should return nil if none of `mamba' or `micromamba' is found"
      (spy-on 'pet--executable-find)
      (expect (pet-use-mamba-p) :to-be nil)))

  (describe "when the project does not have a `environment[a-zA-Z0-9-_].yaml' file"
    (before-each
      (spy-on 'pet-environment))

    (it "should return nil if `mamba' is found"
      (spy-on 'pet--executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "mamba") "/usr/bin/mamba")))
      (expect (pet-use-mamba-p) :to-be nil))

    (it "should return nil if `microconda' is found"
      (spy-on 'pet--executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "microconda") "/usr/bin/microconda")))
      (expect (pet-use-mamba-p) :to-be nil))

    (it "should return nil if none of `mamba' or `micromamba' is found"
      (spy-on 'pet--executable-find)
      (expect (pet-use-mamba-p) :to-be nil))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
