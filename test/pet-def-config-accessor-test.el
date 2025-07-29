;; -*- lexical-binding: t; -*-

(unless (< emacs-major-version 27)
  (load-file "test/undercover-init.el"))

(require 'pet)

(xdescribe "pet-def-config-accessor"
  (before-each
    (defun parser (file) "content")
    (spy-on 'pet-project-root :and-return-value "/home/user/project/")
    (spy-on 'file-notify-add-watch)
    (pet-def-config-accessor tox-ini :file-name "tox.ini" :parser parser))

  (after-each
    (fmakunbound 'pet-tox-ini)
    (unintern 'pet-tox-ini obarray)
    (fmakunbound 'pet-tox-ini-path)
    (unintern 'pet-tox-ini-path obarray)
    (fmakunbound 'parser)
    (unintern 'parser obarray))

  (it "should create config accessor function"
    (expect (fboundp 'pet-tox-ini) :to-be t))

  (it "should create path accessor function"
    (expect (fboundp 'pet-tox-ini-path) :to-be t))

  (describe "the config accessor function"
    (before-each
      (setq-local pet-cache nil)
      (spy-on 'pet-find-file-from-project :and-return-value "/home/user/project/tox.ini")
      (spy-on 'file-notify-add-watch :and-return-value 'mock-watcher)
      (spy-on 'parser :and-call-through))

    (after-each
      (kill-local-variable 'pet-cache))

    (it "should return cached value if it exists"
      (pet-cache-put (list "/home/user/project/" :configs "/home/user/project/tox.ini") "cached content")
      (expect (pet-tox-ini) :to-equal "cached content")
      (expect 'pet-setup-config-cache-and-watcher :not :to-have-been-called)
      (expect 'parser :not :to-have-been-called))

    (describe "when the config file content has not been cached"
      (it "should return parsed file content"
        (expect (pet-tox-ini) :to-equal "content"))

      (it "should setup cache and watcher"
        (pet-tox-ini)
        (expect 'file-notify-add-watch :to-have-been-called)
        (expect 'parser :to-have-been-called-with "/home/user/project/tox.ini"))

      (it "should cache config file content in unified cache"
        (pet-tox-ini)
        (expect (pet-cache-get (list "/home/user/project/" :configs "/home/user/project/tox.ini")) :to-equal "content")))))

(describe "pet-def-config-accessor"
  :var ((project-a-root "/home/user/project-a/")
        (project-b-root "/home/user/project-b/")
        (pyproject-file-a "/home/user/project-a/pyproject.toml")
        (pyproject-file-b "/home/user/project-b/pyproject.toml")
        (pyproject-content-a '((tool . ((poetry . ((name . "project-a")))))))
        (pyproject-content-b '((tool . ((poetry . ((name . "project-b")))))))
        (test-parser nil)
        (call-count 0))

  (before-each
    (setq-local pet-cache nil)
    (setq call-count 0)

    ;; Define a named parser function so the macro can reference it
    (defun test-parser (file)
      (setq call-count (1+ call-count))
      (cond ((string= file pyproject-file-a) pyproject-content-a)
            ((string= file pyproject-file-b) pyproject-content-b)
            (t "default-content")))

    ;; Define test accessor
    (pet-def-config-accessor test-pyproject
      :file-name "pyproject.toml"
      :parser test-parser)

    ;; Mock file system and watchers
    (spy-on 'file-notify-add-watch :and-return-value 'mock-watcher)
    (spy-on 'file-notify-rm-watch))

  (after-each
    ;; Cleanup generated functions
    (when (fboundp 'pet-test-pyproject)
      (fmakunbound 'pet-test-pyproject)
      (unintern 'pet-test-pyproject obarray))
    (when (fboundp 'pet-test-pyproject-path)
      (fmakunbound 'pet-test-pyproject-path)
      (unintern 'pet-test-pyproject-path obarray))
    (when (fboundp 'test-parser)
      (fmakunbound 'test-parser)
      (unintern 'test-parser obarray))
    (kill-local-variable 'pet-cache))

  (describe "1. Happy Path - First Time Discovery"
    (it "should discover, parse, cache, and watch a config file on first access"
      (spy-on 'pet-project-root :and-return-value project-a-root)
      (spy-on 'pet-find-file-from-project :and-return-value pyproject-file-a)

      (expect (pet-test-pyproject) :to-equal pyproject-content-a)
      (expect call-count :to-equal 1)
      (expect 'file-notify-add-watch :to-have-been-called)
      (expect (pet-cache-get (list project-a-root :configs pyproject-file-a))
              :to-equal pyproject-content-a)
      (expect (pet-cache-get (list project-a-root :file-watchers pyproject-file-a))
              :to-equal 'mock-watcher)))

  (describe "2. Performance - Subsequent Cache Hits"
    (it "should return cached content instantly on repeated calls"
      (spy-on 'pet-project-root :and-return-value project-a-root)
      (spy-on 'pet-find-file-from-project :and-return-value pyproject-file-a)

      (expect (pet-test-pyproject) :to-equal pyproject-content-a)
      (expect call-count :to-equal 1)

      (expect (pet-test-pyproject) :to-equal pyproject-content-a)
      (expect (pet-test-pyproject) :to-equal pyproject-content-a)
      (expect (pet-test-pyproject) :to-equal pyproject-content-a)

      (expect call-count :to-equal 1)

      ;; File watcher should only be set up once
      (expect 'file-notify-add-watch :to-have-been-called-times 1)))

  (describe "3. Missing Files - Graceful Degradation"
    (it "should handle missing config files gracefully"
      (spy-on 'pet-project-root :and-return-value project-a-root)
      (spy-on 'pet-find-file-from-project)

      (expect (pet-test-pyproject) :to-be nil)
      (expect (pet-test-pyproject-path) :to-be nil)

      (expect 'file-notify-add-watch :not :to-have-been-called)

      (expect call-count :to-equal 0)

      (expect pet-cache :to-be nil)
      (expect pet-cache :to-be nil))

    (it "should return nil when not in a project"
      (spy-on 'pet-project-root)

      (expect (pet-test-pyproject) :to-be nil)
      (expect 'file-notify-add-watch :not :to-have-been-called)
      (expect call-count :to-equal 0)))

  (describe "4. Project Switching"
    (it "should isolate config data between different projects"
      (spy-on 'pet-project-root :and-return-value project-a-root)
      (spy-on 'pet-find-file-from-project :and-return-value pyproject-file-a)
      (expect (pet-test-pyproject) :to-equal pyproject-content-a)

      (spy-on 'pet-project-root :and-return-value project-b-root)
      (spy-on 'pet-find-file-from-project :and-return-value pyproject-file-b)
      (expect (pet-test-pyproject) :to-equal pyproject-content-b)

      (spy-on 'pet-project-root :and-return-value project-a-root)
      (spy-on 'pet-find-file-from-project :and-return-value pyproject-file-a)
      (expect (pet-test-pyproject) :to-equal pyproject-content-a)

      ;; Verify both projects have their own cache entries
      (expect (pet-cache-get (list project-a-root :configs pyproject-file-a))
              :to-equal pyproject-content-a)
      (expect (pet-cache-get (list project-b-root :configs pyproject-file-b))
              :to-equal pyproject-content-b)))

  (describe "5. Memory Management - Repeated Access"
    (it "should not leak memory with repeated calls"
      (spy-on 'pet-project-root :and-return-value project-a-root)
      (spy-on 'pet-find-file-from-project :and-return-value pyproject-file-a)

      (dotimes (i 100)
        (pet-test-pyproject))

      (expect call-count :to-equal 1)

      (let ((configs (pet-cache-get (list project-a-root :configs))))
        (expect (length configs) :to-equal 1))

      (let ((watchers (pet-cache-get (list project-a-root :file-watchers))))
        (expect (length watchers) :to-equal 1))

      ;; File watcher should only be set up once
      (expect 'file-notify-add-watch :to-have-been-called-times 1)))

  (describe "6. Duplicate Watcher Prevention"
    (it "should not create duplicate watchers on multiple calls"
      (spy-on 'pet-project-root :and-return-value project-a-root)
      (spy-on 'pet-find-file-from-project :and-return-value pyproject-file-a)

      (pet-test-pyproject)
      (pet-test-pyproject)
      (pet-test-pyproject)

      (expect 'file-notify-add-watch :to-have-been-called-times 1)

      ;; Cache should have exactly one watcher entry
      (let ((watchers (pet-cache-get (list project-a-root :file-watchers))))
        (expect (length watchers) :to-equal 1)
        (expect (alist-get pyproject-file-a watchers nil nil 'equal) :to-equal 'mock-watcher))))

  (describe "7. Cache Invariant Maintenance"
    (it "should maintain strong invariant between configs and file-watchers"
      (spy-on 'pet-project-root :and-return-value project-a-root)
      (spy-on 'pet-find-file-from-project :and-return-value pyproject-file-a)

      (pet-test-pyproject)

      (expect (pet-cache-get (list project-a-root :configs pyproject-file-a))
              :to-be-truthy)
      (expect (pet-cache-get (list project-a-root :file-watchers pyproject-file-a))
              :to-be-truthy)

      ;; The invariant: if config exists, watcher must exist
      (let ((configs (pet-cache-get (list project-a-root :configs)))
            (watchers (pet-cache-get (list project-a-root :file-watchers))))
        (dolist (config-entry configs)
          (let ((config-path (car config-entry)))
            (expect (alist-get config-path watchers nil nil 'equal) :to-be-truthy)))))))

;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
