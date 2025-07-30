;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-use-conda-p"
  (describe "when the project has an `environment[a-zA-Z0-9-_].yaml' file"
    (before-each
      (spy-on 'pet-environment :and-return-value t))

    (it "should return `conda' path if `conda' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "conda") "/usr/bin/conda")))
      (expect (pet-use-conda-p) :to-equal "/usr/bin/conda"))

    (it "should return nil if `conda' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-conda-p) :to-be nil)))

  (describe "when the project does not have a `environment[a-zA-Z0-9-_].yaml' file"
    (before-each
      (spy-on 'pet-environment))

    (it "should return nil if `conda' is found"
      (spy-on 'executable-find :and-call-fake (lambda (exe &optional _) (when (equal exe "conda") "/usr/bin/conda")))
      (expect (pet-use-conda-p) :to-be nil))

    (it "should return nil if `conda' is not found"
      (spy-on 'executable-find)
      (expect (pet-use-conda-p) :to-be nil))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
