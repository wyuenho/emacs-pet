;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-parse-pre-commit-db"
  (before-all
    (call-process "sqlite3" nil nil nil "some.db" "CREATE TABLE repos (repo TEXT NOT NULL, ref TEXT NOT NULL, path TEXT NOT NULL, PRIMARY KEY (repo, ref)); INSERT INTO repos VALUES('https://github.com/pycqa/flake8','5.0.0','/home/user/project/flake8');"))

  (after-all
    (call-process "rm" nil nil nil "some.db"))

  (describe "when SQLite is available and functional"
    (it "should parse `pre-commit' database to alist using builtin SQLite"
      (assume (and (functionp 'sqlite-available-p) (sqlite-available-p)))
      (expect (pet-parse-pre-commit-db "some.db") :to-equal '(((repo . "https://github.com/pycqa/flake8")
                                                               (ref . "5.0.0")
                                                               (path . "/home/user/project/flake8")))))

    (it "should return nil when sqlite-open fails and fallback to sqlite3 command"
      (assume (and (functionp 'sqlite-available-p) (sqlite-available-p)))
      (spy-on 'sqlite-open)
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/sqlite3")
      (spy-on 'pet-run-process-get-output :and-return-value "[{\"repo\":\"https://github.com/pycqa/flake8\",\"ref\":\"5.0.0\",\"path\":\"/home/user/project/flake8\"}]")

      (expect (pet-parse-pre-commit-db "some.db") :to-equal '(((repo . "https://github.com/pycqa/flake8") (ref . "5.0.0") (path . "/home/user/project/flake8"))))
      (expect 'sqlite-open :to-have-been-called-with "some.db")
      (expect 'pet--executable-find :to-have-been-called-with "sqlite3" t)
      (expect 'pet-run-process-get-output :to-have-been-called-with "/usr/bin/sqlite3" "-json" "some.db" "select * from repos"))

    (it "should return nil when database file doesn't exist"
      (assume (and (functionp 'sqlite-available-p) (sqlite-available-p)))
      (spy-on 'sqlite-open)
      (spy-on 'pet--executable-find)
      (expect (pet-parse-pre-commit-db "/nonexistent/path/db.sqlite") :to-be nil)))

  (describe "when SQLite is not available"
    (before-each
      (spy-on 'sqlite-available-p))

    (it "should fallback to sqlite3 command when available"
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/sqlite3")
      (spy-on 'pet-run-process-get-output :and-return-value "[{\"repo\":\"https://github.com/pycqa/flake8\",\"ref\":\"5.0.0\",\"path\":\"/home/user/project/flake8\"}]")

      (expect (pet-parse-pre-commit-db "some.db") :to-equal '(((repo . "https://github.com/pycqa/flake8") (ref . "5.0.0") (path . "/home/user/project/flake8"))))
      (expect 'pet--executable-find :to-have-been-called-with "sqlite3" t)
      (expect 'pet-run-process-get-output :to-have-been-called-with "/usr/bin/sqlite3" "-json" "some.db" "select * from repos"))

    (it "should return nil when sqlite3 command is not available"
      (spy-on 'pet--executable-find)
      (expect (pet-parse-pre-commit-db "some.db") :to-be nil))

    (it "should return nil when sqlite3 command fails"
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/sqlite3")
      (spy-on 'pet-run-process-get-output)
      (expect (pet-parse-pre-commit-db "some.db") :to-be nil)))

  (describe "when sqlite-available-p function doesn't exist"
    (before-each
      (let ((original-functionp (symbol-function 'functionp)))
        (spy-on 'functionp :and-call-fake
                (lambda (symbol)
                  (and (not (eq symbol 'sqlite-available-p))
                       (funcall original-functionp symbol))))))

    (it "should fallback to external sqlite3 command"
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/sqlite3")
      (spy-on 'pet-run-process-get-output :and-return-value "[{\"repo\":\"test\",\"ref\":\"1.0\",\"path\":\"/test\"}]")

      (expect (pet-parse-pre-commit-db "some.db") :to-equal '(((repo . "test") (ref . "1.0") (path . "/test"))))
      (expect 'pet--executable-find :to-have-been-called-with "sqlite3" t))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
