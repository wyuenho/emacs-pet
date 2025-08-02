;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-find-file-from-project-root-natively"
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

  (it "should find file using fd command"
    (spy-on 'executable-find :and-return-value "/usr/bin/fd")
    (spy-on 'process-lines :and-return-value '("/home/user/project/environment-dev.yaml"))
    (expect (pet-find-file-from-project-root-natively "environment*.yaml")
            :to-equal "/home/user/project/environment-dev.yaml"))

  (it "should call fd with correct arguments"
    (spy-on 'executable-find :and-return-value "/usr/bin/fd")
    (spy-on 'process-lines :and-return-value '("/home/user/project/environment-dev.yaml"))
    (pet-find-file-from-project-root-natively "environment*.yaml")
    (expect 'process-lines :to-have-been-called-with
            "/usr/bin/fd"
            '("-tf" "-cnever" "-H" "-a" "-g" "environment*.yaml" "/home/user/project/")))

  (it "should use custom fd command and arguments when configured"
    (let ((pet-fd-command "fdfind")
          (pet-fd-command-args '("-t" "f" "--hidden")))
      (spy-on 'executable-find :and-return-value "/usr/bin/fdfind")
      (spy-on 'process-lines :and-return-value '("/home/user/project/test.txt"))
      (pet-find-file-from-project-root-natively "test.txt")
      (expect 'process-lines :to-have-been-called-with
              "/usr/bin/fdfind"
              '("-t" "f" "--hidden" "test.txt" "/home/user/project/"))))

  (it "should return nil when fd is not available"
    (spy-on 'executable-find )
    (expect (pet-find-file-from-project-root-natively "file.txt") :to-be nil))

  (it "should return nil when fd finds no matches"
    (spy-on 'executable-find :and-return-value "/usr/bin/fd")
    (spy-on 'process-lines )
    (expect (pet-find-file-from-project-root-natively "nonexistent.txt") :to-be nil))

  (it "should handle fd command errors gracefully"
    (spy-on 'executable-find :and-return-value "/usr/bin/fd")
    (spy-on 'process-lines :and-throw-error '(error "fd failed"))
    (spy-on 'pet-report-error)
    (expect (pet-find-file-from-project-root-natively "file.txt") :to-be nil)
    (expect 'pet-report-error :to-have-been-called))

  (it "should return nil when not in a project"
    (spy-on 'pet-project-root)
    (expect (pet-find-file-from-project-root-natively "file.txt") :to-be nil)))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
