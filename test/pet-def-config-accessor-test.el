;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-def-config-accessor"
  :var ((project-a-root "/home/user/project-a/")
        (project-b-root "/home/user/project-b/")
        (pyproject-file-a "/home/user/project-a/pyproject.toml")
        (pyproject-file-b "/home/user/project-b/pyproject.toml")
        (pyproject-content-a '((tool . ((poetry . ((name . "project-a")))))))
        (pyproject-content-b '((tool . ((poetry . ((name . "project-b")))))))
        test-parser
        (call-count 0))

  (before-each
    (setq-local pet-cache nil)
    (setq call-count 0)

    (defun test-parser (file)
      (setq call-count (1+ call-count))
      (cond ((string= file pyproject-file-a) pyproject-content-a)
            ((string= file pyproject-file-b) pyproject-content-b)
            (t "default-content")))

    (pet-def-config-accessor test-pyproject
      :file-name "pyproject.toml"
      :parser test-parser)

    (spy-on 'file-notify-add-watch :and-return-value 'mock-watcher)
    (spy-on 'file-notify-rm-watch))

  (after-each
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

      (expect (length (pet-cache-get (list project-a-root :configs))) :to-equal 1)

      (expect (length (pet-cache-get (list project-a-root :file-watchers))) :to-equal 1)

      (expect 'file-notify-add-watch :to-have-been-called-times 1)))

  (describe "6. Duplicate Watcher Prevention"
    (it "should not create duplicate watchers on multiple calls"
      (spy-on 'pet-project-root :and-return-value project-a-root)
      (spy-on 'pet-find-file-from-project :and-return-value pyproject-file-a)

      (pet-test-pyproject)
      (pet-test-pyproject)
      (pet-test-pyproject)

      (expect 'file-notify-add-watch :to-have-been-called-times 1)

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

      (let ((configs (pet-cache-get (list project-a-root :configs)))
            (watchers (pet-cache-get (list project-a-root :file-watchers))))
        (dolist (config-entry configs)
          (expect (alist-get (car config-entry) watchers nil nil 'equal) :to-be-truthy))))))

;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
