;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-find-file-from-project-root-recursively"
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

  (describe "when using projectile"
    (it "should return the absolute path of the first file in a project that matches the file wildcard"
      (spy-on 'projectile-dir-files :and-return-value '("environment-dev.yaml"))
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-equal "/home/user/project/environment-dev.yaml"))

    (it "should return `nil' if the project is empty"
      (spy-on 'projectile-dir-files)
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-be nil))

    (it "should return `nil' if no file in the project matches the file wildcard"
      (spy-on 'projectile-dir-files :and-return-value '("setup.py"))
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-be nil)))

  (describe "when using project.el"
    :var ((projectile-dir-files-def (symbol-function 'projectile-dir-files)))

    (before-each
      (fset 'projectile-dir-files nil))

    (after-each
      (fset 'projectile-dir-files projectile-dir-files-def))

    (it "should return the absolute path of the first file in a project that matches the file wildcard"
      (spy-on 'project-files :and-return-value '("/home/user/project/environment-dev.yaml"))
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-equal "/home/user/project/environment-dev.yaml"))

    (it "should return `nil' if the project is empty"
      (spy-on 'project-files)
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-be nil))

    (it "should return `nil' if no file in the project matches the file wildcard"
      (spy-on 'project-files :and-return-value '("setup.py"))
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-be nil)))

  (describe "when using `directory-files-recursively'"
    :var ((projectile-dir-files-def (symbol-function 'projectile-dir-files))
          (project-files-def (symbol-function 'project-files)))

    (before-each
      (fset 'projectile-dir-files nil)
      (fset 'project-files nil))

    (after-each
      (fset 'projectile-dir-files projectile-dir-files-def)
      (fset 'project-files project-files-def))

    (it "should return the absolute path of the first file in a project that matches the file wildcard"
      (spy-on 'directory-files-recursively :and-return-value '("/home/user/project/environment-dev.yaml"))
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-equal "/home/user/project/environment-dev.yaml"))

    (it "should return `nil' if the project is empty"
      (spy-on 'directory-files-recursively)
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-be nil))

    (it "should return `nil' if no file in the project matches the file wildcard"
      (spy-on 'directory-files-recursively :and-return-value '("setup.py"))
      (expect (pet-find-file-from-project-root-recursively "environment*.y*ml") :to-be nil))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
