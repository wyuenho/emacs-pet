;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-parse-json"
  (it "should parse a JSON string to an alist"
    (expect (pet-parse-json "{\"foo\":\"bar\",\"baz\":[\"buz\",1]}") :to-equal '((foo . "bar") (baz "buz" 1)))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
