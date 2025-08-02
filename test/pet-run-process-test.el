;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'pet)

(describe "pet-run-process macro"
  :var (mock-exit-code mock-output)

  (before-each
    (setq mock-exit-code 0
          mock-output "test output\nline 2\nline 3")
    (spy-on 'process-file :and-call-fake
            (lambda (program infile buffer display &rest args)
              (when (eq buffer t)
                (insert mock-output))
              mock-exit-code)))

  (it "should provide exit-code and buffer access to body"
    (let ((result (pet-run-process "test-program" '("arg1" "arg2")
                    (list exit-code (buffer-string)))))
      (expect (car result) :to-equal 0)
      (expect (cadr result) :to-equal mock-output)
      (expect 'process-file :to-have-been-called-with "test-program" nil t nil "arg1" "arg2")))

  (it "should work with non-zero exit codes"
    (setq mock-exit-code 1
          mock-output "error message")
    (let ((result (pet-run-process "failing-program" '()
                    (list exit-code (buffer-string)))))
      (expect (car result) :to-equal 1)
      (expect (cadr result) :to-equal mock-output)))

  (it "should handle empty output"
    (setq mock-output "")
    (let ((result (pet-run-process "quiet-program" '()
                    (buffer-string))))
      (expect result :to-equal ""))))

(describe "pet-run-process-get-output"
  :var (mock-exit-code mock-output)

  (before-each
    (setq mock-exit-code 0
          mock-output "  test output with whitespace  ")
    (spy-on 'process-file :and-call-fake
            (lambda (program infile buffer display &rest args)
              (when (eq buffer t)
                (insert mock-output))
              mock-exit-code)))

  (it "should return trimmed output on success"
    (let ((result (pet-run-process-get-output "test-program" "arg1" "arg2")))
      (expect result :to-equal "test output with whitespace")
      (expect 'process-file :to-have-been-called-with "test-program" nil t nil "arg1" "arg2")))

  (it "should return nil and report error on failure"
    (setq mock-exit-code 1
          mock-output "error message")
    (spy-on 'pet-report-error)

    (let ((result (pet-run-process-get-output "failing-program")))
      (expect result :to-be nil)
      (expect 'pet-report-error :to-have-been-called)))

  (it "should not report error when exit-code is non-zero but output is empty"
    (setq mock-exit-code 1
          mock-output "")
    (spy-on 'pet-report-error)

    (let ((result (pet-run-process-get-output "quiet-failing-program")))
      (expect result :to-be nil)
      (expect 'pet-report-error :not :to-have-been-called)))

  (it "should handle empty output on success"
    (setq mock-output "")
    (let ((result (pet-run-process-get-output "quiet-program")))
      (expect result :to-equal ""))))

(describe "pet-run-process-get-line"
  :var (mock-exit-code mock-output)

  (before-each
    (setq mock-exit-code 0)
    (spy-on 'process-file :and-call-fake
            (lambda (program infile buffer display &rest args)
              (when (eq buffer t)
                (insert mock-output))
              mock-exit-code)))

  (it "should return first line efficiently on success"
    (setq mock-output "  first line  \nsecond line\nthird line")
    (let ((result (pet-run-process-get-line "test-program" "arg1")))
      (expect result :to-equal "first line")
      (expect 'process-file :to-have-been-called-with "test-program" nil t nil "arg1")))

  (it "should return nil when no output on success"
    (setq mock-output "")
    (let ((result (pet-run-process-get-line "quiet-program")))
      (expect result :to-equal nil)))

  (it "should handle single line output without newline"
    (setq mock-output "single line")
    (let ((result (pet-run-process-get-line "single-line-program")))
      (expect result :to-equal "single line")))

  (it "should return nil and report error on failure"
    (setq mock-exit-code 1
          mock-output "error message")
    (spy-on 'pet-report-error)

    (let ((result (pet-run-process-get-line "failing-program")))
      (expect result :to-be nil)
      (expect 'pet-report-error :to-have-been-called)))

  (it "should not report error when failure has empty output"
    (setq mock-exit-code 1
          mock-output "")
    (spy-on 'pet-report-error)

    (let ((result (pet-run-process-get-line "quiet-failing-program")))
      (expect result :to-be nil)
      (expect 'pet-report-error :not :to-have-been-called))))

(describe "pet-run-process-get-lines"
  :var (mock-exit-code mock-output)

  (before-each
    (setq mock-exit-code 0)
    (spy-on 'process-file :and-call-fake
            (lambda (program infile buffer display &rest args)
              (when (eq buffer t)
                (insert mock-output))
              mock-exit-code)))

  (it "should return lines list on success"
    (setq mock-output "line 1\nline 2\nline 3\n")
    (let ((result (pet-run-process-get-lines "multi-line-program")))
      (expect result :to-equal '("line 1" "line 2" "line 3"))))

  (it "should handle empty output on success"
    (setq mock-output "")
    (let ((result (pet-run-process-get-lines "quiet-program")))
      (expect result :to-be nil)))

  (it "should handle single line without trailing newline"
    (setq mock-output "single line")
    (let ((result (pet-run-process-get-lines "single-line-program")))
      (expect result :to-equal '("single line"))))

  (it "should return nil and report error on failure"
    (setq mock-exit-code 1
          mock-output "error line 1\nerror line 2")
    (spy-on 'pet-report-error)

    (let ((result (pet-run-process-get-lines "failing-program")))
      (expect result :to-be nil)
      (expect 'pet-report-error :to-have-been-called)))

  (it "should filter empty lines from output"
    (setq mock-output "line 1\n\nline 3\n\nline 5")
    (let ((result (pet-run-process-get-lines "sparse-output-program")))
      (expect result :to-equal '("line 1" "line 3" "line 5")))))

;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
