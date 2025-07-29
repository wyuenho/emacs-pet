;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-parse-config-file"
  :var* ((yaml-content "foo: bar\nbaz:\n  - buz\n  - 1\n")
         (toml-content "foo = \"bar\"\nbaz = [\"buz\", 1]\n")
         (json-content "{\"foo\":\"bar\",\"baz\":[\"buz\",1]}")
         (yaml-file (make-temp-file "pet-test" nil ".yaml" yaml-content))
         (toml-file (make-temp-file "pet-test" nil ".toml" toml-content))
         (json-file (make-temp-file "pet-test" nil ".json" json-content))
         (toml-file-sans-ext (make-temp-file "pet-test" nil nil toml-content))
         (yaml-file-sans-ext (make-temp-file "pet-test" nil nil yaml-content))
         (json-file-sans-ext (make-temp-file "pet-test" nil nil json-content))
         (jsonian-file-sans-ext (make-temp-file "pet-test" nil nil json-content)))

  (after-all
    (delete-file yaml-file)
    (delete-file toml-file)
    (delete-file json-file)
    (delete-file toml-file-sans-ext)
    (delete-file yaml-file-san-ext)
    (delete-file json-file-san-ext))

  (before-each
    (setq-local pet-toml-to-json-program "tomljson")
    (setq-local pet-toml-to-json-program-arguments nil)
    (setq-local pet-yaml-to-json-program "yq")
    (setq-local pet-yaml-to-json-program-arguments '("--output-format" "json"))
    (make-local-variable 'auto-mode-alist)
    (add-to-list 'auto-mode-alist (cons (concat (file-name-nondirectory toml-file-sans-ext) "\\'") 'conf-toml-mode))
    (add-to-list 'auto-mode-alist (cons (concat (file-name-nondirectory yaml-file-sans-ext) "\\'") 'yaml-mode))
    (add-to-list 'auto-mode-alist (cons (concat (file-name-nondirectory json-file-sans-ext) "\\'") 'json-mode))
    (add-to-list 'auto-mode-alist (cons (concat (file-name-nondirectory jsonian-file-sans-ext) "\\'") 'jsonian-mode)))

  (after-each
    (kill-local-variable 'pet-toml-to-json-program)
    (kill-local-variable 'pet-toml-to-json-program-arguments)
    (kill-local-variable 'pet-yaml-to-json-program)
    (kill-local-variable 'pet-yaml-to-json-program-arguments)
    (kill-local-variable 'auto-mode-alist))

  (it "should parse a YAML file content to alist"
    (expect (pet-parse-config-file yaml-file) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a TOML file content to alist if the file name matches a key in `auto-mode-alist' and the value is `yaml-mode'"
    (expect (pet-parse-config-file yaml-file-sans-ext) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a TOML file content to alist"
    (expect (pet-parse-config-file toml-file) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a TOML file content to alist if the file name matches a key in `auto-mode-alist' and the value is `conf-toml-mode'"
    (expect (pet-parse-config-file toml-file-sans-ext) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a JSON file content to alist"
    (expect (pet-parse-config-file json-file) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a JSON file content to alist if the file name matches a key in `auto-mode-alist' and the value is `json-mode'"
    (expect (pet-parse-config-file json-file-sans-ext) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil))

  (it "should parse a JSON file content to alist if the file name matches a key in `auto-mode-alist' and the value is `jsonian-mode'"
    (expect (pet-parse-config-file jsonian-file-sans-ext) :to-have-same-items-as '((foo . "bar") (baz "buz" 1)))
    (expect (get-buffer " *pet parser output*") :to-be nil)))

