;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-hatch-environments"
  (describe "when hatch is available"
    (before-each
      (spy-on 'pet-use-hatch-p :and-return-value "/usr/bin/hatch"))

    (it "should return list of environment paths on success"
      (spy-on 'process-file :and-call-fake
              (lambda (program &optional infile buffer display &rest args)
                (cond 
                 ;; Mock hatch env show --json
                 ((and (equal program "/usr/bin/hatch")
                       (equal args '("env" "show" "--json")))
                  (insert "{\"default\":{\"type\":\"virtual\"},\"test\":{\"type\":\"virtual\"}}")
                  0)
                 ;; Mock hatch env find for each environment
                 ((and (equal program "/usr/bin/hatch")
                       (equal (car args) "env")
                       (equal (cadr args) "find"))
                  (let ((env-name (caddr args)))
                    (cond
                     ((equal env-name "default")
                      (insert "/home/user/.hatch/envs/project/default"))
                     ((equal env-name "test")
                      (insert "/home/user/.hatch/envs/project/test")))
                    0))
                 (t 1))))
      (expect (pet-hatch-environments) :to-equal '("/home/user/.hatch/envs/project/default" "/home/user/.hatch/envs/project/test")))

    (it "should return empty list when no environments exist"
      (spy-on 'process-file :and-call-fake
              (lambda (program &optional infile buffer display &rest args)
                (when (and (equal program "/usr/bin/hatch")
                           (equal args '("env" "show" "--json")))
                  (insert "{}")
                  0)))
      (expect (pet-hatch-environments) :to-equal '()))

    (it "should return nil on command failure"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "hatch: command not found")
                1))
      (spy-on 'pet-report-error)
      (expect (pet-hatch-environments) :to-equal nil)
      (expect 'pet-report-error :to-have-been-called)))

  (describe "when hatch is not available"
    (before-each
      (spy-on 'pet-use-hatch-p))

    (it "should return nil"
      (expect (pet-hatch-environments) :to-equal nil))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End: