;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-conda-environments"
  (describe "when conda is available"
    (before-each
      (spy-on 'pet-use-conda-p :and-return-value "/usr/bin/conda"))

    (it "should return list of environment paths on success"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{\"envs\": [\"/home/user/miniforge3/envs/myenv\", \"/home/user/miniforge3/envs/test\"]}")
                0))
      (expect (pet-conda-environments) :to-equal '("/home/user/miniforge3/envs/myenv" "/home/user/miniforge3/envs/test")))

    (it "should return empty list when no environments exist"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{\"envs\": []}")
                0))
      (expect (pet-conda-environments) :to-equal '()))

    (it "should return nil on command failure"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "conda: command not found")
                1))
      (spy-on 'pet-report-error)
      (expect (pet-conda-environments) :to-equal nil)
      (expect 'pet-report-error :to-have-been-called-with '(user-error "conda: command not found")))

    (it "should handle malformed JSON gracefully"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{invalid json")
                0))
      (spy-on 'pet-report-error)
      (expect (pet-conda-environments) :to-be nil)
      (expect 'pet-report-error :to-have-been-called)))

  (describe "when conda is not available"
    (before-each
      (spy-on 'pet-use-conda-p ))

    (it "should return nil"
      (expect (pet-conda-environments) :to-be nil))))

