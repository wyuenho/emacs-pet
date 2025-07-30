;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-system-bin-dir"
  (describe "when called on Windows"
    (before-each
      (setq-local system-type 'windows-nt)
      ;; Github actions triggers some weird Emacs 26 code path that calls
      ;; `file-truename' after setting `system-type' and when `flycheck' is not
      ;; in the `load-path'. WTF indeed.
      (spy-on 'w32-long-file-name))

    (after-each
      (kill-local-variable 'system-type))

    (it "should return Scripts"
      (expect (pet-system-bin-dir) :to-equal "Scripts")))

  (describe "when called on non-Windows"
    (before-each
      (setq-local system-type 'gnu/linux))

    (after-each
      (kill-local-variable 'system-type))

    (it "should return bin"
      (expect (pet-system-bin-dir) :to-equal "bin"))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
