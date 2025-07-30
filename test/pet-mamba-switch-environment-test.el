;; -*- lexical-binding: t; -*-

(require 'pet)

(setq python-indent-guess-indent-offset nil)

(describe "pet-mamba-switch-environment"
  :var ((project-root "/home/user/project/")
        (env-path "/home/user/micromamba/envs/test/")
        (other-env-path "/home/user/micromamba/envs/default/")
        (buffer-a nil)
        (buffer-b nil)
        (other-project-buffer nil))

  (before-each
    (setq-local pet-cache nil)
    (spy-on 'pet-project-root :and-return-value project-root)
    (spy-on 'pet-mamba-environments :and-return-value
            (list other-env-path env-path))

    ;; Create test buffers in the project
    (setq buffer-a (get-buffer-create "test-a.py"))
    (setq buffer-b (get-buffer-create "test-b.py"))
    (with-current-buffer buffer-a
      (setq buffer-file-name "/home/user/project/main.py")
      (python-mode)
      (setq-local process-environment '("PATH=/usr/bin" "CONDA_PREFIX=/old/env" "HOME=/home/user")))
    (with-current-buffer buffer-b
      (setq buffer-file-name "/home/user/project/utils.py")
      (python-mode)
      (setq-local process-environment '("PATH=/usr/bin" "CONDA_PREFIX=/old/env" "HOME=/home/user")))

    ;; Create buffer from different project
    (setq other-project-buffer (get-buffer-create "other.py"))
    (with-current-buffer other-project-buffer
      (setq buffer-file-name "/home/user/other-project/main.py")
      (python-mode)
      (setq-local process-environment '("CONDA_PREFIX=/other/old/env"))))

  (after-each
    (when buffer-a (kill-buffer buffer-a))
    (when buffer-b (kill-buffer buffer-b))
    (when other-project-buffer (kill-buffer other-project-buffer))
    (kill-local-variable 'pet-cache))

  (describe "basic functionality"
    (it "should update virtualenv cache with selected environment"
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-mamba-switch-environment env-path)

      (expect (pet-cache-get (list project-root :virtualenv))
              :to-equal env-path))

    (it "should call teardown and setup for all project Python buffers"
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)
      (spy-on 'buffer-list :and-return-value (list buffer-a buffer-b other-project-buffer))

      (pet-mamba-switch-environment env-path)

      (expect 'pet-buffer-local-vars-teardown :to-have-been-called-times 2)
      (expect 'pet-buffer-local-vars-setup :to-have-been-called-times 2))

    (it "should display success message"
      (spy-on 'message)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-mamba-switch-environment env-path)

      (expect 'message :to-have-been-called-with
              "Switched to %s environment: %s" "mamba" env-path)))

  (describe "cache handling"
    (it "should cache the selected environment for the project"
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)
      (spy-on 'buffer-list :and-return-value (list buffer-a buffer-b))

      (pet-mamba-switch-environment env-path)

      (expect (pet-cache-get (list project-root :virtualenv)) :to-equal env-path)))

  (describe "buffer isolation"
    (it "should only refresh variables for buffers from the same project"
      (spy-on 'buffer-list :and-return-value (list buffer-a other-project-buffer))
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-mamba-switch-environment env-path)

      ;; Should only call teardown/setup once per project buffer (buffer-a only)
      ;; other-project-buffer should be ignored because it's from different project
      (expect 'pet-buffer-local-vars-teardown :to-have-been-called-times 1)
      (expect 'pet-buffer-local-vars-setup :to-have-been-called-times 1)))

  (describe "interactive completion"
    (it "should prompt with available environments"
      (spy-on 'completing-read :and-return-value env-path)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (call-interactively #'pet-mamba-switch-environment)

      (expect 'completing-read :to-have-been-called-with
              "Please select a mamba environment: "
              (list other-env-path env-path)
              nil t))

    (it "should use the selected environment"
      (spy-on 'completing-read :and-return-value other-env-path)
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (call-interactively #'pet-mamba-switch-environment)

      (expect (pet-cache-get (list project-root :virtualenv))
              :to-equal other-env-path)))

  (describe "error handling"
    (it "should handle when no project root is found"
      (spy-on 'pet-project-root )
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-mamba-switch-environment env-path)

      (expect 'pet-buffer-local-vars-teardown :not :to-have-been-called)
      (expect 'pet-buffer-local-vars-setup :not :to-have-been-called))

    (it "should handle when no Python buffers exist in project"
      (spy-on 'buffer-list :and-return-value '())
      (spy-on 'pet-buffer-local-vars-teardown)
      (spy-on 'pet-buffer-local-vars-setup)

      (pet-mamba-switch-environment env-path)

      (expect (pet-cache-get (list project-root :virtualenv)) :to-equal env-path)
      (expect 'pet-buffer-local-vars-teardown :not :to-have-been-called))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
