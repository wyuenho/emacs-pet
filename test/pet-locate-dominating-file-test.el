;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-locate-dominating-file"
  :var ((old-default-directory default-directory)
        (home (getenv "HOME"))
        (process-environment (copy-sequence process-environment)))

  (before-each
    (setenv "HOME" "/home/user/")
    (setq-local default-directory "~/project/src/")
    (spy-on 'pet-project-root :and-return-value "/home/user/project/"))

  (after-each
    (setenv "HOME" home)
    (setq-local default-directory old-default-directory))

  (it "should find file from `default-directory'"
    (spy-on 'locate-dominating-file :and-return-value "~/project/src/package")
    (spy-on 'file-expand-wildcards :and-call-fake
            (lambda (pattern &optional full)
              (cond
               ((and full (equal pattern "/home/user/project/src/package/.pylintrc"))
                (list (expand-file-name pattern)))
               ((and (not full) (equal pattern "~/project/src/package/.pylintrc"))
                (list pattern)))))
    (expect (pet-locate-dominating-file ".pylintrc") :to-equal "/home/user/project/src/package/.pylintrc"))

  (it "should find file from parent directory"
    (spy-on 'locate-dominating-file :and-return-value "~/project/src/")
    (spy-on 'file-expand-wildcards :and-call-fake
            (lambda (pattern &optional full)
              (cond
               ((and full (equal pattern "/home/user/project/src/.pylintrc"))
                (list (expand-file-name pattern)))
               ((and (not full) (equal pattern "~/project/src/.pylintrc"))
                (list pattern)))))
    (expect (pet-locate-dominating-file ".pylintrc") :to-equal "/home/user/project/src/.pylintrc"))

  (it "should return `nil' if found file is outside of project root"
    (spy-on 'file-expand-wildcards :and-call-fake
            (lambda (dir &optional _)
              (when (equal dir "~/")
                (list "~/"))))
    (expect (pet-locate-dominating-file ".pylintrc") :to-be nil)))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
