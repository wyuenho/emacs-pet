;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-mode"
  (before-each
    (spy-on 'pet-project-root :and-return-value "/home/user/project/")
    (spy-on 'pet-buffer-local-vars-setup)
    (spy-on 'pet-buffer-local-vars-teardown)
    (spy-on 'add-hook)
    (spy-on 'remove-hook))

  (describe "when enabling pet-mode"
    (it "should set up all buffer local variables for supported packages if `pet-mode' is t"
      (spy-on 'assoc-default :and-return-value "/home/user/project/.venv/")
      (pet-mode 1)
      (expect 'pet-buffer-local-vars-setup :to-have-been-called)
      (expect 'add-hook :to-have-been-called-with 'kill-buffer-hook #'pet-cleanup-watchers-and-caches t))

    (it "should call pixi environment switch when pixi is detected and no cached virtualenv"
      (spy-on 'assoc-default )
      (spy-on 'pet-use-pixi-p :and-return-value "/usr/bin/pixi")
      (spy-on 'call-interactively)
      (pet-mode 1)
      (expect 'call-interactively :to-have-been-called-with #'pet-pixi-switch-environment)
      (expect 'pet-buffer-local-vars-setup :not :to-have-been-called))

    (it "should call conda environment switch when conda is detected and no cached virtualenv and pixi not available"
      (spy-on 'assoc-default )
      (spy-on 'pet-use-pixi-p )
      (spy-on 'pet-use-conda-p :and-return-value "/usr/bin/conda")
      (spy-on 'call-interactively)
      (pet-mode 1)
      (expect 'call-interactively :to-have-been-called-with #'pet-conda-switch-environment)
      (expect 'pet-buffer-local-vars-setup :not :to-have-been-called))

    (it "should call mamba environment switch when mamba is detected and no cached virtualenv and pixi/conda not available"
      (spy-on 'assoc-default )
      (spy-on 'pet-use-pixi-p )
      (spy-on 'pet-use-conda-p )
      (spy-on 'pet-use-mamba-p :and-return-value "/usr/bin/mamba")
      (spy-on 'call-interactively)
      (pet-mode 1)
      (expect 'call-interactively :to-have-been-called-with #'pet-mamba-switch-environment)
      (expect 'pet-buffer-local-vars-setup :not :to-have-been-called))

    (it "should set up buffer local vars when no environment managers are detected"
      (spy-on 'assoc-default )
      (spy-on 'pet-use-pixi-p )
      (spy-on 'pet-use-conda-p )
      (spy-on 'pet-use-mamba-p )
      (pet-mode 1)
      (expect 'pet-buffer-local-vars-setup :to-have-been-called)))

  (describe "when disabling pet-mode"
    (it "should reset all buffer local variables for supported packages to default if `pet-mode' is nil"
      (pet-mode -1)
      (expect 'pet-buffer-local-vars-teardown :to-have-been-called)
      (expect 'remove-hook :to-have-been-called-with 'kill-buffer-hook #'pet-cleanup-watchers-and-caches t))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
