;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-find-file-from-project-root-natively"
  :var ((old-default-directory default-directory)
        (home (getenv "HOME")))

  (before-each
    (setq-local process-environment (copy-sequence process-environment))
    (setenv "HOME" "/home/user/")
    (setq-local default-directory "~/project/src/")
    (spy-on 'pet-project-root :and-return-value "/home/user/project/"))

  (after-each
    (kill-local-variable 'process-environment)
    (setq-local default-directory old-default-directory))

  (it "should find file using fd command"
    (spy-on 'pet--executable-find :and-return-value "/usr/bin/fd")
    (spy-on 'process-file :and-call-fake
            (lambda (program infile buffer display &rest args)
              (when (equal program "/usr/bin/fd")
                (insert "/home/user/project/environment-dev.yaml\n")
                0)))
    (expect (pet-find-file-from-project-root-natively "environment*.yaml")
            :to-equal "/home/user/project/environment-dev.yaml"))

  (it "should call fd with correct arguments"
    (spy-on 'pet--executable-find :and-return-value "/usr/bin/fd")
    (spy-on 'process-file :and-call-fake
            (lambda (program infile buffer display &rest args)
              (when (equal program "/usr/bin/fd")
                (insert "/home/user/project/environment-dev.yaml\n")
                0)))
    (pet-find-file-from-project-root-natively "environment*.yaml")
    (expect 'process-file :to-have-been-called-with
            "/usr/bin/fd" nil t nil
            "-tf" "-cnever" "-H" "-a" "-g" "environment*.yaml" "/home/user/project/"))

  (it "should use custom fd command and arguments when configured"
    (let ((pet-fd-command "fdfind")
          (pet-fd-command-args '("-t" "f" "--hidden")))
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/fdfind")
      (spy-on 'process-file :and-call-fake
              (lambda (program infile buffer display &rest args)
                (when (equal program "/usr/bin/fdfind")
                  (when (eq buffer t)
                    (insert "/home/user/project/test.txt\n"))
                  0)))
      (pet-find-file-from-project-root-natively "test.txt")
      (expect 'process-file :to-have-been-called-with
              "/usr/bin/fdfind" nil t nil
              "-t" "f" "--hidden" "test.txt" "/home/user/project/")))

  (it "should return nil when fd is not available"
    (spy-on 'pet--executable-find)
    (expect (pet-find-file-from-project-root-natively "file.txt") :to-be nil))

  (it "should return nil when fd finds no matches"
    (spy-on 'pet--executable-find :and-return-value "/usr/bin/fd")
    (spy-on 'process-file :and-return-value 1) ; non-zero exit code
    (expect (pet-find-file-from-project-root-natively "nonexistent.txt") :to-be nil))

  (it "should handle fd command errors gracefully"
    (spy-on 'pet--executable-find :and-return-value "/usr/bin/fd")
    (spy-on 'process-file :and-throw-error 'error)
    (spy-on 'pet-report-error)
    (expect (pet-find-file-from-project-root-natively "file.txt") :to-be nil)
    (expect 'pet-report-error :to-have-been-called))

  (it "should return nil when not in a project"
    (spy-on 'pet-project-root)
    (expect (pet-find-file-from-project-root-natively "file.txt") :to-be nil)))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
