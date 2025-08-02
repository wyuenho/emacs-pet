;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-cache"
  (before-each
    (make-variable-buffer-local 'pet-cache))

  (after-each
    (kill-local-variable 'pet-cache))

  (describe "pet-cache-get"
    (it "should return nil for empty cache"
      (setq-local pet-cache nil)
      (expect (pet-cache-get '("/nonexistent")) :to-be nil))

    (it "should return nil for nested path in empty cache"
      (setq-local pet-cache nil)
      (expect (pet-cache-get '("/nonexistent" :virtualenv)) :to-be nil))

    (it "should get single-level values"
      (setq-local pet-cache '(("/project1" . "project-data")))
      (expect (pet-cache-get '("/project1")) :to-equal "project-data"))

    (it "should get two-level values"
      (setq-local pet-cache '(("/project1" . ((:virtualenv . "/path/to/venv")))))
      (expect (pet-cache-get '("/project1" :virtualenv)) :to-equal "/path/to/venv"))

    (it "should get three-level values"
      (setq-local pet-cache '(("/project1" . ((:files . (("pyproject.toml" . "/project1/pyproject.toml")))
                                              (:configs . (("/project1/pyproject.toml" . ((tool . ((poetry . t)))))))
                                              (:file-watchers . (("/project1/pyproject.toml" . 123)))))))
      (expect (pet-cache-get '("/project1" :configs "/project1/pyproject.toml")) :to-equal '((tool . ((poetry . t))))))

    (it "should get intermediate level data"
      (setq-local pet-cache '(("/project1" . ((:files . (("pyproject.toml" . "/project1/pyproject.toml")
                                                         ("setup.py" . "/project1/setup.py")))
                                              (:configs . (("/project1/pyproject.toml" . "content1")
                                                           ("/project1/setup.py" . "content2")))
                                              (:file-watchers . (("/project1/pyproject.toml" . 123)
                                                                 ("/project1/setup.py" . 456)))))))
      (let ((configs (pet-cache-get '("/project1" :configs))))
        (expect configs :to-be-truthy)
        (expect (alist-get "/project1/pyproject.toml" configs nil nil 'equal) :to-equal "content1")
        (expect (alist-get "/project1/setup.py" configs nil nil 'equal) :to-equal "content2"))))

  (describe "pet-cache-put"
    (it "should create single-level entries"
      (setq-local pet-cache nil)
      (pet-cache-put '("/project1") "data")
      (expect pet-cache :to-equal '(("/project1" . "data"))))

    (it "should create nested entries automatically"
      (setq-local pet-cache nil)
      (pet-cache-put '("/project1" :virtualenv) "/venv")
      (expect pet-cache :to-equal '(("/project1" . ((:virtualenv . "/venv"))))))

    (it "should create deep nested entries for config files"
      (setq-local pet-cache nil)
      (pet-cache-put '("/project1" :configs "/project1/pyproject.toml") "content")
      (expect pet-cache :to-equal '(("/project1" . ((:configs . (("/project1/pyproject.toml" . "content"))))))))

    (it "should overwrite existing values"
      (setq-local pet-cache '(("/project1" . ((:virtualenv . "/old/venv")))))
      (pet-cache-put '("/project1" :virtualenv) "/new/venv")
      (expect pet-cache :to-equal '(("/project1" . ((:virtualenv . "/new/venv"))))))

    (it "should preserve existing data when adding new nested entries"
      (setq-local pet-cache '(("/project1" . ((:virtualenv . "/venv")))))
      (pet-cache-put '("/project1" :files "setup.py") "/project1/setup.py")
      (expect pet-cache :to-equal '(("/project1" . ((:files . (("setup.py" . "/project1/setup.py")))
                                                    (:virtualenv . "/venv"))))))

    (it "should fail gracefully with empty path"
      (expect (pet-cache-put '() "value") :to-throw)))

  (describe "pet-cache-rem"
    (it "should remove specific config file entries"
      (setq-local pet-cache '(("/project1" . ((:virtualenv . "/venv")
                                              (:files . (("pyproject.toml" . "/project1/pyproject.toml")
                                                         ("setup.py" . "/project1/setup.py")))
                                              (:configs . (("/project1/pyproject.toml" . "config1")
                                                           ("/project1/setup.py" . "config2")))
                                              (:file-watchers . (("/project1/pyproject.toml" . 123)
                                                                 ("/project1/setup.py" . 456)))))))
      (pet-cache-rem '("/project1" :configs "/project1/pyproject.toml"))
      (expect pet-cache :to-equal '(("/project1" . ((:virtualenv . "/venv")
                                                    (:files . (("pyproject.toml" . "/project1/pyproject.toml")
                                                               ("setup.py" . "/project1/setup.py")))
                                                    (:configs . (("/project1/setup.py" . "config2")))
                                                    (:file-watchers . (("/project1/pyproject.toml" . 123)
                                                                       ("/project1/setup.py" . 456))))))))

    (it "should remove entire categories"
      (setq-local pet-cache '(("/project1" . ((:virtualenv . "/venv")
                                              (:files . (("setup.py" . "/project1/setup.py")))))))
      (pet-cache-rem '("/project1" :files))
      (expect pet-cache :to-equal '(("/project1" . ((:virtualenv . "/venv"))))))

    (it "should remove top-level project entries"
      (setq-local pet-cache '(("/project1" . ((:virtualenv . "/venv")))))
      (pet-cache-rem '("/project1"))
      (expect pet-cache :to-equal '()))

    (it "should remove top-level project entries from multi-project cache"
      (setq-local pet-cache '(("/project-a" . ((:virtualenv . "/venv-a")))
                              ("/project-b" . ((:virtualenv . "/venv-b")))))
      (pet-cache-rem '("/project-a"))
      (expect pet-cache :to-equal '(("/project-b" . ((:virtualenv . "/venv-b")))))
      (expect (pet-cache-get '("/project-a")) :to-be nil))

    (it "should handle removal of non-existent keys gracefully"
      (setq-local pet-cache '(("/project1" . ((:virtualenv . "/venv")))))
      (pet-cache-rem '("/project1" :non-existent))
      (expect pet-cache :to-equal '(("/project1" . ((:virtualenv . "/venv")))))))

  (describe "multiple projects isolation"
    (it "should isolate data between projects"
      (pet-cache-put '("/project1" :virtualenv) "/venv1")
      (pet-cache-put '("/project2" :virtualenv) "/venv2")
      (pet-cache-put '("/project1" :configs "/project1/pyproject.toml") "config1")
      (pet-cache-put '("/project2" :configs "/project2/setup.py") "config2")

      (expect (pet-cache-get '("/project1" :virtualenv)) :to-equal "/venv1")
      (expect (pet-cache-get '("/project2" :virtualenv)) :to-equal "/venv2")
      (expect (pet-cache-get '("/project1" :configs "/project1/pyproject.toml")) :to-equal "config1")
      (expect (pet-cache-get '("/project2" :configs "/project2/setup.py")) :to-equal "config2")

      (pet-cache-rem '("/project1" :virtualenv))
      (expect (pet-cache-get '("/project1" :virtualenv)) :to-be nil)
      (expect (pet-cache-get '("/project2" :virtualenv)) :to-equal "/venv2")))

  (describe "realistic usage patterns"
    (it "should handle typical pet.el workflow"
      (let ((project "/home/user/myproject"))
        (pet-cache-put (list project :virtualenv) "/home/user/.venv/myproject")

        (pet-cache-put (list project :files "pyproject.toml") "/home/user/myproject/pyproject.toml")
        (pet-cache-put (list project :files "*.yml") "/home/user/myproject/.pre-commit-config.yaml")

        (pet-cache-put (list project :configs "/home/user/myproject/pyproject.toml")
                       '((tool . ((poetry . ((name . "myproject")))))))
        (pet-cache-put (list project :configs "/home/user/myproject/.pre-commit-config.yaml")
                       '((repos . (((repo . "https://github.com/psf/black"))))))

        (pet-cache-put (list project :files "main.py") "/home/user/myproject/main.py")
        (pet-cache-put (list project :files "*.txt") "/home/user/myproject/requirements.txt")

        (pet-cache-put (list project :file-watchers "/home/user/myproject/pyproject.toml") 123)
        (pet-cache-put (list project :file-watchers "/home/user/myproject/.pre-commit-config.yaml") 456)

        (expect (pet-cache-get (list project :virtualenv)) :to-equal "/home/user/.venv/myproject")
        (expect (pet-cache-get (list project :files "pyproject.toml")) :to-equal "/home/user/myproject/pyproject.toml")
        (expect (pet-cache-get (list project :files "*.yml")) :to-equal "/home/user/myproject/.pre-commit-config.yaml")
        (expect (pet-cache-get (list project :configs "/home/user/myproject/pyproject.toml")) :to-equal '((tool . ((poetry . ((name . "myproject")))))))
        (expect (pet-cache-get (list project :configs "/home/user/myproject/.pre-commit-config.yaml")) :to-equal '((repos . (((repo . "https://github.com/psf/black"))))))
        (expect (pet-cache-get (list project :files "main.py")) :to-equal "/home/user/myproject/main.py")
        (expect (pet-cache-get (list project :files "*.txt")) :to-equal "/home/user/myproject/requirements.txt")
        (expect (pet-cache-get (list project :file-watchers "/home/user/myproject/pyproject.toml")) :to-equal 123)
        (expect (pet-cache-get (list project :file-watchers "/home/user/myproject/.pre-commit-config.yaml")) :to-equal 456)

        (let ((all-files (pet-cache-get (list project :files)))
              (all-configs (pet-cache-get (list project :configs)))
              (all-watchers (pet-cache-get (list project :file-watchers))))
          (expect (length all-files) :to-equal 4)
          (expect (length all-configs) :to-equal 2)
          (expect (length all-watchers) :to-equal 2))))))

(describe "pet-setup-config-cache-and-watcher"
  :var ((project-root "/home/user/project/")
        (config-file "/home/user/project/pyproject.toml")
        (parsed-content '((tool . ((poetry . t)))))
        mock-parser
        (mock-watcher 'mock-watcher-123))

  (before-each
    (setq-local pet-cache nil)
    (setq mock-parser (lambda (file) parsed-content))
    (spy-on 'pet-project-root :and-return-value project-root)
    (spy-on 'file-notify-add-watch :and-return-value mock-watcher))

  (after-each
    (kill-local-variable 'pet-cache))

  (it "should parse and cache file content in :configs category"
    (pet-setup-config-cache-and-watcher config-file mock-parser)
    (expect (pet-cache-get (list project-root :configs config-file)) :to-equal parsed-content))

  (it "should call the parser function with the config file path"
    (let ((parser-called nil)
          (parser-arg nil))
      (fset 'mock-parser (lambda (file)
                           (setq parser-called t
                                 parser-arg file)
                           parsed-content))
      (pet-setup-config-cache-and-watcher config-file 'mock-parser)
      (expect parser-called :to-be t)
      (expect parser-arg :to-equal config-file)))

  (it "should set up file watcher and cache handle in :file-watchers category"
    (pet-setup-config-cache-and-watcher config-file mock-parser)
    (expect 'file-notify-add-watch :to-have-been-called)
    (expect (pet-cache-get (list project-root :file-watchers config-file)) :to-equal mock-watcher))

  (it "should pass correct arguments to file-notify-add-watch"
    (pet-setup-config-cache-and-watcher config-file mock-parser)
    (let ((call-args (spy-calls-args-for 'file-notify-add-watch 0)))
      (expect (nth 0 call-args) :to-equal config-file)
      (expect (nth 1 call-args) :to-equal '(change))
      (expect (functionp (nth 2 call-args)) :to-be t)))

  (it "should maintain strong invariant between :configs and :file-watchers"
    (pet-setup-config-cache-and-watcher config-file mock-parser)

    (expect (pet-cache-get (list project-root :configs config-file)) :to-equal parsed-content)
    (expect (pet-cache-get (list project-root :file-watchers config-file)) :to-equal mock-watcher))

  (it "should not create duplicate watchers for the same file"
    (pet-setup-config-cache-and-watcher config-file mock-parser)
    (expect 'file-notify-add-watch :to-have-been-called-times 1)

    (pet-setup-config-cache-and-watcher config-file mock-parser)
    (expect 'file-notify-add-watch :to-have-been-called-times 1)
    (expect (pet-cache-get (list project-root :file-watchers config-file)) :to-equal mock-watcher))

  (it "should handle multiple different config files"
    (let ((other-file "/home/user/project/setup.cfg")
          (other-content '((metadata . ((name . "test"))))))
      (setq mock-parser (lambda (file)
                          (if (string= file config-file)
                              parsed-content
                            other-content)))

      (pet-setup-config-cache-and-watcher config-file mock-parser)
      (pet-setup-config-cache-and-watcher other-file mock-parser)

      (expect (pet-cache-get (list project-root :configs config-file)) :to-equal parsed-content)
      (expect (pet-cache-get (list project-root :configs other-file)) :to-equal other-content)
      (expect (pet-cache-get (list project-root :file-watchers config-file)) :to-equal mock-watcher)
      (expect (pet-cache-get (list project-root :file-watchers other-file)) :to-equal mock-watcher)

      (expect 'file-notify-add-watch :to-have-been-called-times 2)))

  (it "should return nil when not in a project"
    (spy-on 'pet-project-root)
    (pet-setup-config-cache-and-watcher config-file mock-parser)

    (expect pet-cache :to-be nil)
    (expect 'file-notify-add-watch :not :to-have-been-called)))

(describe "pet-teardown-config-cache-and-watcher"
  :var ((project-root "/home/user/project/")
        (config-file "/home/user/project/pyproject.toml")
        (other-file "/home/user/project/setup.cfg")
        (pattern1 "pyproject.toml")
        (pattern2 "*.toml")
        (pattern3 "setup.cfg")
        (mock-watcher-1 'mock-watcher-123)
        (mock-watcher-2 'mock-watcher-456))

  (before-each
    (setq-local pet-cache nil)
    (spy-on 'pet-project-root :and-return-value project-root)
    (spy-on 'file-notify-rm-watch))

  (after-each
    (kill-local-variable 'pet-cache))

  (it "should remove file watcher and clean up handle"
    (pet-cache-put (list project-root :file-watchers config-file) mock-watcher-1)
    (pet-cache-put (list project-root :configs config-file) '((content . "test")))

    (pet-teardown-config-cache-and-watcher config-file)

    (expect 'file-notify-rm-watch :to-have-been-called-with mock-watcher-1)
    (expect (pet-cache-get (list project-root :file-watchers config-file)) :to-be nil))

  (it "should remove parsed content from :configs category"
    (pet-cache-put (list project-root :configs config-file) '((tool . ((poetry . t)))))
    (pet-cache-put (list project-root :configs other-file) '((metadata . ((name . "test")))))

    (pet-teardown-config-cache-and-watcher config-file)

    (expect (pet-cache-get (list project-root :configs config-file)) :to-be nil)
    (expect (pet-cache-get (list project-root :configs other-file)) :to-equal '((metadata . ((name . "test"))))))

  (it "should only remove configs and file-watchers, leaving files intact"
    (pet-cache-put (list project-root :files pattern1) config-file)
    (pet-cache-put (list project-root :files pattern2) config-file)
    (pet-cache-put (list project-root :files pattern3) other-file)
    (pet-cache-put (list project-root :configs config-file) '((content . "test")))
    (pet-cache-put (list project-root :file-watchers config-file) mock-watcher-1)

    (pet-teardown-config-cache-and-watcher config-file)

    (expect (pet-cache-get (list project-root :files pattern1)) :to-equal config-file)
    (expect (pet-cache-get (list project-root :files pattern2)) :to-equal config-file)
    (expect (pet-cache-get (list project-root :files pattern3)) :to-equal other-file)
    (expect (pet-cache-get (list project-root :configs config-file)) :to-be nil)
    (expect (pet-cache-get (list project-root :file-watchers config-file)) :to-be nil))

  (it "should handle removal when file has no watcher"
    (pet-cache-put (list project-root :configs config-file) '((content . "test")))
    (pet-cache-put (list project-root :files pattern1) config-file)

    (pet-teardown-config-cache-and-watcher config-file)

    (expect 'file-notify-rm-watch :not :to-have-been-called)
    (expect (pet-cache-get (list project-root :configs config-file)) :to-be nil)
    (expect (pet-cache-get (list project-root :files pattern1)) :to-equal config-file))

  (it "should handle strong cache invariant operations (configs and watchers)"
    (pet-cache-put (list project-root :files pattern1) config-file)
    (pet-cache-put (list project-root :files pattern2) config-file)
    (pet-cache-put (list project-root :configs config-file) '((tool . ((poetry . t)))))
    (pet-cache-put (list project-root :file-watchers config-file) mock-watcher-1)

    (pet-cache-put (list project-root :files pattern3) other-file)
    (pet-cache-put (list project-root :configs other-file) '((metadata . ((name . "test")))))
    (pet-cache-put (list project-root :file-watchers other-file) mock-watcher-2)

    (pet-teardown-config-cache-and-watcher config-file)

    (expect 'file-notify-rm-watch :to-have-been-called-with mock-watcher-1)
    (expect (pet-cache-get (list project-root :file-watchers config-file)) :to-be nil)
    (expect (pet-cache-get (list project-root :configs config-file)) :to-be nil)

    (expect (pet-cache-get (list project-root :files pattern1)) :to-equal config-file)
    (expect (pet-cache-get (list project-root :files pattern2)) :to-equal config-file)

    (expect (pet-cache-get (list project-root :files pattern3)) :to-equal other-file)
    (expect (pet-cache-get (list project-root :configs other-file)) :to-equal '((metadata . ((name . "test")))))
    (expect (pet-cache-get (list project-root :file-watchers other-file)) :to-equal mock-watcher-2))

  (it "should handle empty cache gracefully"
    (pet-teardown-config-cache-and-watcher config-file)

    (expect 'file-notify-rm-watch :not :to-have-been-called)
    (expect pet-cache :to-be nil))

  (it "should return nil when not in a project"
    (pet-cache-put (list project-root :configs config-file) '((content . "test")))

    (spy-on 'pet-project-root)

    (pet-teardown-config-cache-and-watcher config-file)

    (expect 'file-notify-rm-watch :not :to-have-been-called)
    (expect (pet-cache-get (list project-root :configs config-file)) :to-equal '((content . "test")))))

(describe "pet-cache-clear-project"
  :var ((project-root "/home/user/project/")
        (other-project "/home/user/other/")
        (mock-watcher-1 'mock-watcher-123)
        (mock-watcher-2 'mock-watcher-456))

  (before-each
    (setq-local pet-cache nil)
    (spy-on 'pet-project-root :and-return-value project-root)
    (spy-on 'file-notify-rm-watch)
    (spy-on 'message))

  (after-each
    (kill-local-variable 'pet-cache))

  (it "should clear cache for current project only"
    (pet-cache-put (list project-root :virtualenv) "/venv1")
    (pet-cache-put (list project-root :files "pyproject.toml") "/home/user/project/pyproject.toml")
    (pet-cache-put (list other-project :virtualenv) "/venv2")
    (pet-cache-put (list other-project :files "setup.py") "/home/user/other/setup.py")

    (pet-cache-clear-project)

    (expect (pet-cache-get (list project-root)) :to-be nil)
    (expect (pet-cache-get (list other-project :virtualenv)) :to-equal "/venv2")
    (expect (pet-cache-get (list other-project :files "setup.py")) :to-equal "/home/user/other/setup.py"))

  (it "should clean up file watchers before clearing cache"
    (pet-cache-put (list project-root :virtualenv) "/venv")
    (pet-cache-put (list project-root :file-watchers "/home/user/project/pyproject.toml") mock-watcher-1)
    (pet-cache-put (list project-root :file-watchers "/home/user/project/setup.py") mock-watcher-2)
    (pet-cache-put (list other-project :file-watchers "/home/user/other/config.yaml") 'other-watcher)

    (pet-cache-clear-project)

    (expect 'file-notify-rm-watch :to-have-been-called-with mock-watcher-1)
    (expect 'file-notify-rm-watch :to-have-been-called-with mock-watcher-2)
    (expect 'file-notify-rm-watch :to-have-been-called-times 2)
    (expect (pet-cache-get (list project-root)) :to-be nil)
    (expect (pet-cache-get (list other-project :file-watchers "/home/user/other/config.yaml")) :to-equal 'other-watcher))

  (it "should handle empty file watchers section gracefully"
    (pet-cache-put (list project-root :virtualenv) "/venv")
    (pet-cache-put (list project-root :files "setup.py") "/home/user/project/setup.py")

    (pet-cache-clear-project)

    (expect 'file-notify-rm-watch :not :to-have-been-called)
    (expect (pet-cache-get (list project-root)) :to-be nil))

  (it "should handle no cache for current project gracefully"
    (pet-cache-put (list other-project :virtualenv) "/venv2")

    (pet-cache-clear-project)

    (expect 'file-notify-rm-watch :not :to-have-been-called)
    (expect (pet-cache-get (list other-project :virtualenv)) :to-equal "/venv2"))

  (it "should display success message with project path"
    (pet-cache-put (list project-root :virtualenv) "/venv")

    (pet-cache-clear-project)

    (expect 'message :to-have-been-called-with "Cleared pet cache for project: %s" project-root))

  (it "should do nothing when not in a project"
    (spy-on 'pet-project-root)
    (pet-cache-put (list project-root :virtualenv) "/venv")

    (pet-cache-clear-project)

    (expect 'file-notify-rm-watch :not :to-have-been-called)
    (expect 'message :not :to-have-been-called)
    (expect (pet-cache-get (list project-root :virtualenv)) :to-equal "/venv")))

(describe "pet-cache-clear-all"
  :var ((project-root "/home/user/project/")
        (other-project "/home/user/other/")
        (mock-watcher-1 'mock-watcher-123)
        (mock-watcher-2 'mock-watcher-456)
        (pre-commit-watcher 'pre-commit-watcher-789))

  (before-each
    (setq-local pet-cache nil)
    (setq pet-pre-commit-database-watcher nil)
    (setq pet-pre-commit-database-cache nil)
    (spy-on 'file-notify-rm-watch)
    (spy-on 'message))

  (after-each
    (kill-local-variable 'pet-cache))

  (it "should clear all project caches"
    (pet-cache-put (list project-root :virtualenv) "/venv1")
    (pet-cache-put (list project-root :files "pyproject.toml") "/home/user/project/pyproject.toml")
    (pet-cache-put (list other-project :virtualenv) "/venv2")
    (pet-cache-put (list other-project :files "setup.py") "/home/user/other/setup.py")

    (pet-cache-clear-all)

    (expect pet-cache :to-be nil)
    (expect (pet-cache-get (list project-root)) :to-be nil)
    (expect (pet-cache-get (list other-project)) :to-be nil))

  (it "should clean up all file watchers across all projects"
    (pet-cache-put (list project-root :virtualenv) "/venv1")
    (pet-cache-put (list project-root :file-watchers "/home/user/project/pyproject.toml") mock-watcher-1)
    (pet-cache-put (list other-project :virtualenv) "/venv2")
    (pet-cache-put (list other-project :file-watchers "/home/user/other/setup.py") mock-watcher-2)

    (pet-cache-clear-all)

    (expect 'file-notify-rm-watch :to-have-been-called-with mock-watcher-1)
    (expect 'file-notify-rm-watch :to-have-been-called-with mock-watcher-2)
    (expect 'file-notify-rm-watch :to-have-been-called-times 2)
    (expect pet-cache :to-be nil))

  (it "should clean up pre-commit database watcher"
    (setq pet-pre-commit-database-watcher pre-commit-watcher)
    (setq pet-pre-commit-database-cache '((some . data)))

    (pet-cache-clear-all)

    (expect 'file-notify-rm-watch :to-have-been-called-with pre-commit-watcher)
    (expect pet-pre-commit-database-watcher :to-be nil)
    (expect pet-pre-commit-database-cache :to-be nil))

  (it "should handle empty cache gracefully"
    (pet-cache-clear-all)

    (expect 'file-notify-rm-watch :not :to-have-been-called)
    (expect pet-cache :to-be nil)
    (expect pet-pre-commit-database-cache :to-be nil))

  (it "should handle missing file watchers sections gracefully"
    (pet-cache-put (list project-root :virtualenv) "/venv1")
    (pet-cache-put (list other-project :virtualenv) "/venv2")

    (pet-cache-clear-all)

    (expect 'file-notify-rm-watch :not :to-have-been-called)
    (expect pet-cache :to-be nil))

  (it "should handle no pre-commit watcher gracefully"
    (pet-cache-put (list project-root :virtualenv) "/venv")
    (setq pet-pre-commit-database-cache '((some . data)))

    (pet-cache-clear-all)

    (expect pet-cache :to-be nil)
    (expect pet-pre-commit-database-cache :to-be nil)
    (expect 'file-notify-rm-watch :not :to-have-been-called))

  (it "should display success message"
    (pet-cache-put (list project-root :virtualenv) "/venv")

    (pet-cache-clear-all)

    (expect 'message :to-have-been-called-with "Cleared all pet caches"))

  (it "should handle complex cache structure with multiple watchers per project"
    (pet-cache-put (list project-root :virtualenv) "/venv1")
    (pet-cache-put (list project-root :file-watchers "/home/user/project/pyproject.toml") mock-watcher-1)
    (pet-cache-put (list project-root :file-watchers "/home/user/project/setup.py") mock-watcher-2)
    (pet-cache-put (list other-project :virtualenv) "/venv2")
    (pet-cache-put (list other-project :file-watchers "/home/user/other/config.yaml") 'other-watcher)
    (setq pet-pre-commit-database-watcher pre-commit-watcher)
    (setq pet-pre-commit-database-cache '((repos . ())))

    (pet-cache-clear-all)

    (expect 'file-notify-rm-watch :to-have-been-called-with mock-watcher-1)
    (expect 'file-notify-rm-watch :to-have-been-called-with mock-watcher-2)
    (expect 'file-notify-rm-watch :to-have-been-called-with 'other-watcher)
    (expect 'file-notify-rm-watch :to-have-been-called-with pre-commit-watcher)
    (expect 'file-notify-rm-watch :to-have-been-called-times 4)
    (expect pet-cache :to-be nil)
    (expect pet-pre-commit-database-watcher :to-be nil)
    (expect pet-pre-commit-database-cache :to-be nil)))

;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
