;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-mamba-environments"
  (describe "when mamba is available"
    (before-each
      (spy-on 'pet-use-mamba-p :and-return-value "/usr/bin/mamba"))

    (it "should return list of environment paths on success"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{\"envs\": [\"/home/user/micromamba/envs/myenv\", \"/home/user/micromamba/envs/test\"]}")
                0))
      (expect (pet-mamba-environments) :to-equal '("/home/user/micromamba/envs/myenv" "/home/user/micromamba/envs/test")))

    (it "should return empty list when no environments exist"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{\"envs\": []}")
                0))
      (expect (pet-mamba-environments) :to-equal '()))

    (it "should signal user-error on command failure"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "mamba: command not found")
                1))
      (spy-on 'pet-report-error)
      (expect (pet-mamba-environments) :to-equal nil)
      (expect 'pet-report-error :to-have-been-called-with '(user-error "mamba: command not found")))

    (it "should handle malformed JSON gracefully"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{invalid json")
                0))
      (spy-on 'pet-report-error)
      (expect (pet-mamba-environments) :to-be nil)
      (expect 'pet-report-error :to-have-been-called)))

  (describe "when mamba is not available"
    (before-each
      (spy-on 'pet-use-mamba-p))

    (it "should return nil"
      (expect (pet-mamba-environments) :to-be nil))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
