;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-parse-pre-commit-db"
  (before-all
    (call-process "sqlite3" nil nil nil "some.db" "CREATE TABLE repos (repo TEXT NOT NULL, ref TEXT NOT NULL, path TEXT NOT NULL, PRIMARY KEY (repo, ref)); INSERT INTO repos VALUES('https://github.com/pycqa/flake8','5.0.0','/home/user/project/flake8');"))

  (after-all
    (call-process "rm" nil nil nil "some.db"))

  (it "should parse `pre-commit' database to alist"
    (expect (pet-parse-pre-commit-db "some.db") :to-equal '(((repo . "https://github.com/pycqa/flake8")
                                                             (ref . "5.0.0")
                                                             (path . "/home/user/project/flake8"))))))

