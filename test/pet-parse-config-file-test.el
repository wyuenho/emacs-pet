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
    (delete-file yaml-file-sans-ext)
    (delete-file json-file-sans-ext))

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
    (kill-local-variable 'pet-prefer-elisp-parsers)
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
    (expect (get-buffer " *pet parser output*") :to-be nil))

  ;; Tests for new fallback parser functionality
  (describe "with parser fallbacks"
    (before-each
      (setq-local pet-prefer-elisp-parsers nil))

    (describe "when pet-prefer-elisp-parsers is nil (default behavior)"
      (it "should use external program when it succeeds"
        (spy-on 'executable-find :and-return-value "/usr/bin/tomljson")
        (spy-on 'process-file :and-return-value 0)
        (spy-on 'buffer-string :and-return-value "{\"foo\":\"bar\"}")
        (spy-on 'pet-parse-toml-with-elisp)
        (expect (pet-parse-config-file toml-file) :to-have-same-items-as '((foo . "bar")))
        (expect 'pet-parse-toml-with-elisp :not :to-have-been-called))

      (it "should fallback to elisp parser when external program fails"
        (spy-on 'executable-find :and-return-value "/usr/bin/tomljson")
        (spy-on 'process-file :and-return-value 1)
        (spy-on 'pet-parse-toml-with-elisp :and-return-value '((foo . "bar")))
        (expect (pet-parse-config-file toml-file) :to-have-same-items-as '((foo . "bar"))))

      (it "should return nil when external program fails and elisp parser is not available"
        (spy-on 'executable-find :and-return-value "/usr/bin/tomljson")
        (spy-on 'process-file :and-return-value 1)
        (spy-on 'pet-parse-toml-with-elisp :and-return-value :parser-not-available)
        (expect (pet-parse-config-file toml-file) :to-be nil))

      (it "should return nil when external program fails and elisp parser fails"
        (spy-on 'executable-find :and-return-value "/usr/bin/tomljson")
        (spy-on 'process-file :and-return-value 1)
        (spy-on 'pet-parse-toml-with-elisp :and-call-fake
                (lambda (file) (signal 'user-error '("Parse error"))))
        (expect (pet-parse-config-file toml-file) :to-be nil)))

    (describe "when pet-prefer-elisp-parsers is t"
      (before-each
        (setq-local pet-prefer-elisp-parsers t))

      (it "should use elisp parser when it succeeds"
        (spy-on 'pet-parse-toml-with-elisp :and-return-value '((foo . "bar")))
        (spy-on 'executable-find)
        (expect (pet-parse-config-file toml-file) :to-have-same-items-as '((foo . "bar")))
        (expect 'executable-find :not :to-have-been-called))

      (it "should fallback to external program when elisp parser is not available"
        (spy-on 'pet-parse-toml-with-elisp :and-return-value :parser-not-available)
        (spy-on 'executable-find :and-return-value "/usr/bin/tomljson")
        (spy-on 'process-file :and-return-value 0)
        (spy-on 'buffer-string :and-return-value "{\"foo\":\"bar\"}")
        (expect (pet-parse-config-file toml-file) :to-have-same-items-as '((foo . "bar"))))

      (it "should return nil when elisp parser is not available and external program fails"
        (spy-on 'pet-parse-toml-with-elisp :and-return-value :parser-not-available)
        (spy-on 'executable-find :and-return-value "/usr/bin/tomljson")
        (spy-on 'process-file :and-return-value 1)
        (expect (pet-parse-config-file toml-file) :to-be nil))

      (it "should fallback to external program when elisp parser fails"
        (spy-on 'pet-parse-toml-with-elisp :and-call-fake
                (lambda (file) (signal 'user-error '("Parse error"))))
        (spy-on 'executable-find :and-return-value "/usr/bin/tomljson")
        (spy-on 'process-file :and-return-value 0)
        (spy-on 'buffer-string :and-return-value "{\"foo\":\"bar\"}")
        (expect (pet-parse-config-file toml-file) :to-have-same-items-as '((foo . "bar"))))

      (it "should return nil when elisp parser fails and external program fails"
        (spy-on 'pet-parse-toml-with-elisp :and-call-fake
                (lambda (file) (signal 'user-error '("Parse error"))))
        (spy-on 'executable-find :and-return-value "/usr/bin/tomljson")
        (spy-on 'process-file :and-return-value 1)
        (expect (pet-parse-config-file toml-file) :to-be nil)))

    (describe "empty file handling"
      (it "should not fallback when external program returns empty object"
        (spy-on 'executable-find :and-return-value "/usr/bin/tomljson")
        (spy-on 'process-file :and-return-value 0)
        (spy-on 'buffer-string :and-return-value "{}")
        (spy-on 'pet-parse-json :and-return-value nil)
        (spy-on 'pet-parse-toml-with-elisp)
        (expect (pet-parse-config-file toml-file) :to-be nil)
        (expect 'pet-parse-toml-with-elisp :not :to-have-been-called))

      (it "should not fallback when external program returns null"
        (spy-on 'executable-find :and-return-value "/usr/bin/tomljson")
        (spy-on 'process-file :and-return-value 0)
        (spy-on 'buffer-string :and-return-value "null")
        (spy-on 'pet-parse-json :and-return-value nil)
        (spy-on 'pet-parse-toml-with-elisp)
        (expect (pet-parse-config-file toml-file) :to-be nil)
        (expect 'pet-parse-toml-with-elisp :not :to-have-been-called))

      (it "should normalize :null to nil and not fallback"
        (spy-on 'executable-find :and-return-value "/usr/bin/yq")
        (spy-on 'process-file :and-return-value 0)
        (spy-on 'buffer-string :and-return-value "null")
        (spy-on 'pet-parse-json :and-return-value :null)
        (spy-on 'pet--parse-yaml-with-elisp)
        (expect (pet-parse-config-file yaml-file) :to-be nil)
        (expect 'pet--parse-yaml-with-elisp :not :to-have-been-called))

      (it "should not fallback when elisp parser returns nil for empty file"
        (setq-local pet-prefer-elisp-parsers t)
        (spy-on 'pet-parse-toml-with-elisp :and-return-value nil)
        (spy-on 'executable-find)
        (expect (pet-parse-config-file toml-file) :to-be nil)
        (expect 'executable-find :not :to-have-been-called)))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
