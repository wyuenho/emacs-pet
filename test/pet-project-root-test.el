;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-project-root"
  (it "should find project root with `projectile'"
    (spy-on 'projectile-project-root :and-return-value "/")
    (expect (pet-project-root) :to-equal "/"))

  (it "should find project root with `project.el'"
    (spy-on 'projectile-project-root)
    (spy-on 'project-current :and-return-value (if (< emacs-major-version 29) (cons 'vc "/") '(vc Git "/")))
    (expect (pet-project-root) :to-equal "/"))

  (it "should return nil when Python file does not appear to be in a project"
    (spy-on 'projectile-project-root)
    (spy-on 'project-current)
    (expect (pet-project-root) :to-be nil)))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
