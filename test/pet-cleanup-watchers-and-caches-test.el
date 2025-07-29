;; -*- lexical-binding: t; -*-

(require 'pet)

(setq python-indent-guess-indent-offset nil)
(describe "pet-cleanup-watchers-and-caches"
  :var ((project-root "/home/user/project/")
        (pyproject-file "/home/user/project/pyproject.toml")
        (mock-watcher 'mock-watcher-123)
        (mock-precommit-watcher 'mock-precommit-watcher)
        (test-buffer nil)
        ;; Variables to save/restore global state
        (saved-pet-cache nil)
        (saved-pet-pre-commit-database-cache nil)
        (saved-pet-pre-commit-database-watcher nil))

  (before-each
    ;; Save current global state
    (setq saved-pet-cache pet-cache)
    (setq saved-pet-pre-commit-database-cache pet-pre-commit-database-cache)
    (setq saved-pet-pre-commit-database-watcher pet-pre-commit-database-watcher)

    ;; Create test buffer
    (setq test-buffer (get-buffer-create "test.py"))
    (with-current-buffer test-buffer
      (setq buffer-file-name "/home/user/project/test.py")
      (python-mode)))

  (after-each
    ;; Clean up test buffer
    (when (buffer-live-p test-buffer)
      (kill-buffer test-buffer))

    ;; Restore original global state
    (setq pet-cache saved-pet-cache)
    (setq pet-pre-commit-database-cache saved-pet-pre-commit-database-cache)
    (setq pet-pre-commit-database-watcher saved-pet-pre-commit-database-watcher))

  (describe "Preconditions"
    (it "should do nothing when buffer has no file name"
      (with-current-buffer test-buffer
        (let ((buffer-file-name nil))
          (spy-on 'file-notify-rm-watch)
          (pet-cleanup-watchers-and-caches)
          (expect 'file-notify-rm-watch :not :to-have-been-called))))

    (it "should do nothing when buffer is not in python-mode"
      (with-current-buffer (get-buffer-create "test.txt")
        (setq buffer-file-name "/home/user/project/test.txt")
        (text-mode)
        (spy-on 'file-notify-rm-watch)
        (pet-cleanup-watchers-and-caches)
        (expect 'file-notify-rm-watch :not :to-have-been-called)
        (kill-buffer (current-buffer))))

    (it "should do nothing when not in a project"
      (with-current-buffer test-buffer
        (spy-on 'pet-project-root)
        (spy-on 'file-notify-rm-watch)
        (pet-cleanup-watchers-and-caches)
        (expect 'file-notify-rm-watch :not :to-have-been-called))))

  (describe "System-level Cleanup"
    (it "should clean up system cache when this is the only Python buffer"
      (with-current-buffer test-buffer
        ;; Set up minimal cache state
        (setq pet-cache `((,project-root . ((:file-watchers . ((,pyproject-file . ,mock-watcher)))))))
        (setq pet-pre-commit-database-cache '(((repo . "https://github.com/test"))))
        (setq pet-pre-commit-database-watcher mock-precommit-watcher)

        (spy-on 'pet-project-root :and-return-value project-root)
        (spy-on 'buffer-list :and-return-value (list test-buffer))
        (spy-on 'file-notify-rm-watch)

        (pet-cleanup-watchers-and-caches)

        ;; System cleanup should happen
        (expect 'file-notify-rm-watch :to-have-been-called-with mock-precommit-watcher)
        (expect pet-pre-commit-database-cache :to-be nil)))

    (it "should preserve system cache when other Python buffers exist in different projects"
      (let ((other-buffer (get-buffer-create "other.py")))
        (with-current-buffer other-buffer
          (setq buffer-file-name "/home/user/other-project/other.py")
          (python-mode))

        (with-current-buffer test-buffer
          ;; Set up cache state
          (setq pet-cache `((,project-root . ((:file-watchers . ((,pyproject-file . ,mock-watcher)))))))
          (setq pet-pre-commit-database-cache '(((repo . "https://github.com/test"))))
          (setq pet-pre-commit-database-watcher mock-precommit-watcher)

          (spy-on 'pet-project-root :and-return-value project-root)
          (spy-on 'buffer-list :and-return-value (list test-buffer other-buffer))
          (spy-on 'file-notify-rm-watch)

          (pet-cleanup-watchers-and-caches)

          ;; Should NOT clean up pre-commit watcher since other Python buffer exists
          (expect 'file-notify-rm-watch :not :to-have-been-called-with mock-precommit-watcher)
          ;; Pre-commit should remain
          (expect pet-pre-commit-database-cache :to-equal '(((repo . "https://github.com/test")))))

        (kill-buffer other-buffer)))

    (it "should not clean up system cache when other Python buffers exist in same project"
      (let ((same-project-buffer (get-buffer-create "other.py")))
        (with-current-buffer same-project-buffer
          (setq buffer-file-name "/home/user/project/other.py")
          (python-mode))

        (with-current-buffer test-buffer
          ;; Set up cache state
          (setq pet-cache `((,project-root . ((:file-watchers . ((,pyproject-file . ,mock-watcher)))))))
          (setq pet-pre-commit-database-cache '(((repo . "https://github.com/test"))))
          (setq pet-pre-commit-database-watcher mock-precommit-watcher)

          (spy-on 'pet-project-root :and-return-value project-root)
          (spy-on 'buffer-list :and-return-value (list test-buffer same-project-buffer))
          (spy-on 'file-notify-rm-watch)

          (pet-cleanup-watchers-and-caches)

          ;; Should NOT clean up anything since other Python buffer exists
          (expect 'file-notify-rm-watch :not :to-have-been-called)
          ;; Caches should remain
          (expect pet-pre-commit-database-cache :to-equal '(((repo . "https://github.com/test")))))

        (kill-buffer same-project-buffer)))

    (it "should handle empty cache gracefully"
      (with-current-buffer test-buffer
        (setq pet-cache nil)
        (setq pet-pre-commit-database-cache nil)

        (spy-on 'pet-project-root :and-return-value project-root)
        (spy-on 'buffer-list :and-return-value (list test-buffer))
        (spy-on 'file-notify-rm-watch)

        (pet-cleanup-watchers-and-caches)

        ;; Should not error and not call file-notify-rm-watch
        (expect 'file-notify-rm-watch :not :to-have-been-called)))

    (it "should handle missing pre-commit watcher gracefully"
      (with-current-buffer test-buffer
        (setq pet-cache nil)
        (setq pet-pre-commit-database-cache '(((repo . "https://github.com/test"))))
        (setq pet-pre-commit-database-watcher nil)

        (spy-on 'pet-project-root :and-return-value project-root)
        (spy-on 'buffer-list :and-return-value (list test-buffer))
        (spy-on 'file-notify-rm-watch)

        (pet-cleanup-watchers-and-caches)

        ;; Should not try to remove nil pre-commit watcher but should clean cache
        (expect 'file-notify-rm-watch :not :to-have-been-called)
        (expect pet-pre-commit-database-cache :to-be nil))))

  (describe "Project-level Cleanup"
    :var ((project-a-root "/home/user/project-a/")
          (project-b-root "/home/user/project-b/")
          (pyproject-a "/home/user/project-a/pyproject.toml")
          (pyproject-b "/home/user/project-b/pyproject.toml")
          (mock-watcher-a 'watcher-a)
          (mock-watcher-b 'watcher-b)
          (buffer-a nil)
          (buffer-b nil)
          (buffer-a2 nil))

    (before-each
      ;; Create buffers for different projects
      (setq buffer-a (get-buffer-create "project-a.py"))
      (setq buffer-b (get-buffer-create "project-b.py"))
      (setq buffer-a2 (get-buffer-create "project-a2.py"))

      (with-current-buffer buffer-a
        (setq buffer-file-name "/home/user/project-a/main.py")
        (python-mode))
      (with-current-buffer buffer-b
        (setq buffer-file-name "/home/user/project-b/main.py")
        (python-mode))
      (with-current-buffer buffer-a2
        (setq buffer-file-name "/home/user/project-a/utils.py")
        (python-mode)))

    (after-each
      (when (buffer-live-p buffer-a) (kill-buffer buffer-a))
      (when (buffer-live-p buffer-b) (kill-buffer buffer-b))
      (when (buffer-live-p buffer-a2) (kill-buffer buffer-a2)))

    (it "should clean up project cache when last Python buffer for project is closed"
      ;; Kill buffer-a2 so buffer-a becomes the last buffer for project-a
      (kill-buffer buffer-a2)

      (with-current-buffer buffer-a
        ;; Set up cache for both projects INSIDE the buffer context
        (setq pet-cache `((,project-a-root . ((:file-watchers . ((,pyproject-a . ,mock-watcher-a)))
                                             (:configs . ((,pyproject-a . ((tool . ((poetry . t)))))))))
                         (,project-b-root . ((:file-watchers . ((,pyproject-b . ,mock-watcher-b)))
                                            (:configs . ((,pyproject-b . ((tool . ((setuptools . t)))))))))))

        (spy-on 'pet-project-root :and-return-value project-a-root)
        ;; Mock buffer-list to return buffer-a (current) and buffer-b (different project)
        (spy-on 'buffer-list :and-return-value (list buffer-a buffer-b))
        (spy-on 'file-notify-rm-watch)

        (pet-cleanup-watchers-and-caches)

        ;; Should clean up project A's watchers but not project B's
        (expect 'file-notify-rm-watch :to-have-been-called-with mock-watcher-a)
        (expect 'file-notify-rm-watch :not :to-have-been-called-with mock-watcher-b)

        ;; Project A cache should be removed, project B should remain
        (expect (pet-cache-get (list project-a-root)) :to-be nil)
        (expect (pet-cache-get (list project-b-root :configs pyproject-b)) :to-be-truthy)))

    (it "should not clean up project cache when other Python buffers exist in same project"
      (with-current-buffer buffer-a
        ;; Set up cache for project A INSIDE the buffer context
        (setq pet-cache `((,project-a-root . ((:file-watchers . ((,pyproject-a . ,mock-watcher-a)))
                                             (:configs . ((,pyproject-a . ((tool . ((poetry . t)))))))))))

        (spy-on 'pet-project-root :and-return-value project-a-root)
        ;; Mock buffer-list to include both buffer-a and buffer-a2 (same project)
        (spy-on 'buffer-list :and-return-value (list buffer-a buffer-a2 buffer-b))
        (spy-on 'file-notify-rm-watch)

        (pet-cleanup-watchers-and-caches)

        ;; Should NOT clean up project A since buffer-a2 still exists
        (expect 'file-notify-rm-watch :not :to-have-been-called)
        (expect (pet-cache-get (list project-a-root :configs pyproject-a)) :to-be-truthy)))

    (it "should handle project with multiple file watchers"
      (let ((setup-py "/home/user/project-a/setup.py")
            (mock-watcher-setup 'watcher-setup))
        ;; Kill buffer-a2 so buffer-a becomes the last buffer for project-a
        (kill-buffer buffer-a2)

        (with-current-buffer buffer-a
          ;; Set up project with multiple config files and watchers INSIDE the buffer context
          (setq pet-cache `((,project-a-root . ((:file-watchers . ((,pyproject-a . ,mock-watcher-a)
                                                                  (,setup-py . ,mock-watcher-setup)))
                                               (:configs . ((,pyproject-a . ((tool . ((poetry . t)))))
                                                          (,setup-py . ((name . "test-project")))))))))

          (spy-on 'pet-project-root :and-return-value project-a-root)
          (spy-on 'buffer-list :and-return-value (list buffer-a buffer-b))
          (spy-on 'file-notify-rm-watch)

          (pet-cleanup-watchers-and-caches)

          ;; Should clean up ALL project A watchers
          (expect 'file-notify-rm-watch :to-have-been-called-with mock-watcher-a)
          (expect 'file-notify-rm-watch :to-have-been-called-with mock-watcher-setup)
          ;; Project cache should be completely removed
          (expect (pet-cache-get (list project-a-root)) :to-be nil))))

    (it "should handle project cleanup without affecting system-level cache"
      ;; Kill buffer-a2 so buffer-a becomes the last buffer for project-a, but buffer-b still exists
      (kill-buffer buffer-a2)

      (with-current-buffer buffer-a
        ;; Set up both project and system-level caches INSIDE the buffer context
        (setq pet-cache `((,project-a-root . ((:file-watchers . ((,pyproject-a . ,mock-watcher-a)))))))
        (setq pet-pre-commit-database-cache '(((repo . "https://github.com/test"))))
        (setq pet-pre-commit-database-watcher mock-precommit-watcher)

        (spy-on 'pet-project-root :and-return-value project-a-root)
        (spy-on 'buffer-list :and-return-value (list buffer-a buffer-b))
        (spy-on 'file-notify-rm-watch)

        (pet-cleanup-watchers-and-caches)

        ;; Should clean up project A but NOT system-level pre-commit (since buffer-b exists)
        (expect 'file-notify-rm-watch :to-have-been-called-with mock-watcher-a)
        (expect 'file-notify-rm-watch :not :to-have-been-called-with mock-precommit-watcher)
        ;; Project cache cleaned, system cache preserved
        (expect (pet-cache-get (list project-a-root)) :to-be nil)
        (expect pet-pre-commit-database-cache :to-equal '(((repo . "https://github.com/test"))))))))

;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
