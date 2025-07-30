;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-find-file-from-project-root"
  (it "should find file from project root"
    (spy-on 'pet-project-root :and-return-value "/etc")
    (expect (pet-find-file-from-project-root "passwd") :to-equal "/etc/passwd"))

  (it "should return nil when file not found from project root"
    (spy-on 'pet-project-root :and-return-value "/etc")
    (expect (pet-find-file-from-project-root "idontexist") :to-be nil))

  (it "should return nil when not under a project"
    (spy-on 'pet-project-root)
    (expect (pet-find-file-from-project-root "foo") :to-be nil)))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
