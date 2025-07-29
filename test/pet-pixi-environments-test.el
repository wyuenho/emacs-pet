;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-pixi-environments"
  (describe "when pixi is available"
    (before-each
      (spy-on 'pet-use-pixi-p :and-return-value "/usr/bin/pixi"))

    (it "should return list of environment prefixes on success"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{\"environments_info\": [{\"prefix\": \"/home/user/project/.pixi/envs/default\"}, {\"prefix\": \"/home/user/project/.pixi/envs/test\"}]}")
                0))
      (expect (pet-pixi-environments) :to-equal '("/home/user/project/.pixi/envs/default" "/home/user/project/.pixi/envs/test")))

    (it "should return empty list when no environments exist"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{\"environments_info\": []}")
                0))
      (expect (pet-pixi-environments) :to-equal '()))

    (it "should return nil on command failure"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "pixi: command not found")
                1))
      (spy-on 'pet-report-error)
      (expect (pet-pixi-environments) :to-equal nil)
      (expect 'pet-report-error :to-have-been-called-with '(user-error "pixi: command not found")))

    (it "should handle malformed JSON gracefully"
      (spy-on 'process-file :and-call-fake
              (lambda (&rest _)
                (insert "{invalid json")
                0))
      (spy-on 'pet-report-error)
      (expect (pet-pixi-environments) :to-be nil)
      (expect 'pet-report-error :to-have-been-called)))

  (describe "when pixi is not available"
    (before-each
      (spy-on 'pet-use-pixi-p ))

    (it "should return nil"
      (expect (pet-pixi-environments) :to-be nil))))

