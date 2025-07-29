;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-executable-find"

  (describe "when given an absolute path"
    (it "should return the absolute path if the file exists and is executable"
      (spy-on 'file-remote-p )
      (spy-on 'file-name-absolute-p :and-return-value t)
      (spy-on 'pet--executable-find :and-return-value "/usr/bin/python")
      (expect (pet-executable-find "/usr/bin/python") :to-equal "/usr/bin/python"))

    (it "should return nil if the absolute path file does not exist or is not executable"
      (spy-on 'file-remote-p )
      (spy-on 'file-name-absolute-p :and-return-value t)
      (spy-on 'pet--executable-find )
      (spy-on 'pet-use-pre-commit-p )
      (spy-on 'pet-virtualenv-root )
      (expect (pet-executable-find "/nonexistent/path") :to-be nil))

    (it "should not use absolute path optimization for remote files"
      (spy-on 'file-remote-p :and-return-value "/ssh:user@host:")
      (spy-on 'file-name-absolute-p :and-return-value t)
      (spy-on 'pet-use-pre-commit-p )
      (spy-on 'pet-virtualenv-root )
      (spy-on 'pet--executable-find )
      (expect (pet-executable-find "/ssh:user@host:/usr/bin/python") :to-be nil))

    (it "should not use absolute path optimization for relative paths"
      (spy-on 'file-remote-p )
      (spy-on 'file-name-absolute-p )
      (spy-on 'pet-use-pre-commit-p )
      (spy-on 'pet-virtualenv-root )
      (spy-on 'pet--executable-find )
      (expect (pet-executable-find "python") :to-be nil)))

  (describe "when using `pre-commit'"
    (before-each
      (spy-on 'pet-use-pre-commit-p :and-return-value "/usr/bin/pre-commit")
      (spy-on 'pet--executable-find ))

    (it "should return the absolute path to the executable if hook and hook repo are found and the executable is found in hook repo"
      (spy-on 'pet-pre-commit-config-has-hook-p :and-return-value t)
      (spy-on 'pet-pre-commit-virtualenv-path :and-return-value "/home/user/.cache/pre-commit/repoblack")
      (spy-on 'file-exists-p :and-call-fake (lambda (path) (equal path "/home/user/.cache/pre-commit/repoblack/bin/black")))
      (expect (pet-executable-find "black") :to-equal "/home/user/.cache/pre-commit/repoblack/bin/black"))

    (it "should return nil if the hook is not found in config"
      (spy-on 'pet-pre-commit-config-has-hook-p )
      (expect (pet-executable-find "black") :to-be nil))

    (it "should return nil if the hook repo is not found"
      (spy-on 'pet-pre-commit-config-has-hook-p :and-return-value t)
      (spy-on 'pet-pre-commit-virtualenv-path )
      (expect (pet-executable-find "black") :to-be nil))

    (it "should return nil if the executable is not found in hook repo"
      (spy-on 'pet-pre-commit-config-has-hook-p :and-return-value t)
      (spy-on 'pet-pre-commit-virtualenv-path :and-return-value "/home/user/.cache/pre-commit/repoblack")
      (spy-on 'file-exists-p :and-call-fake (lambda (path) (not (equal path "/home/user/.cache/pre-commit/repoblack/bin/black"))))
      (expect (pet-executable-find "black") :to-be nil)))

  (describe "when on *nix"
    (it "should return the absolute path to the python executable for a project if its virtualenv is found"
      (spy-on 'pet-use-pre-commit-p )
      (spy-on 'pet-virtualenv-root :and-return-value "/home/user/project/.venv/")
      (spy-on 'pet-use-conda-p )
      (spy-on 'pet-system-bin-dir)
      (spy-on 'executable-find :and-return-value "/home/user/project/.venv/bin/python")
      (expect (pet-executable-find "python") :to-equal "/home/user/project/.venv/bin/python")
      (expect 'pet-system-bin-dir :to-have-been-called-times 1))

    (it "should return the absolute path to the python executable for a conda project if its virtualenv is found"
      (spy-on 'pet-use-pre-commit-p )
      (spy-on 'pet-virtualenv-root :and-return-value "/home/user/anaconda/envs/project/")
      (spy-on 'pet-use-conda-p :and-return-value t)
      (spy-on 'pet-system-bin-dir)
      (spy-on 'executable-find :and-return-value "/home/user/anaconda/envs/project/bin/python")
      (expect (pet-executable-find "python") :to-equal "/home/user/anaconda/envs/project/bin/python")
      (expect 'pet-system-bin-dir :to-have-been-called-times 1)))

  (describe "when on windows"
    (before-each
      (setq-local system-type 'windows-nt))

    (after-each
      (kill-local-variable 'system-type))

    (it "should return the absolute path to the python executable for a project if its virtualenv is found"
      (spy-on 'pet-use-pre-commit-p )
      (spy-on 'pet-virtualenv-root :and-return-value "C:/Users/user/project/.venv/")
      (spy-on 'pet-use-conda-p )
      (spy-on 'pet-system-bin-dir)
      (spy-on 'executable-find :and-return-value "C:/Users/user/project/.venv/bin/python")
      (expect (pet-executable-find "python") :to-equal "C:/Users/user/project/.venv/bin/python")
      (expect 'pet-system-bin-dir :to-have-been-called-times 1))

    (it "should return the absolute path to the python executable for a conda project if its virtualenv is found"
      (spy-on 'pet-use-pre-commit-p )
      (spy-on 'pet-virtualenv-root :and-return-value "C:/Users/user/Anaconda3/envs/project/")
      (spy-on 'pet-use-conda-p :and-return-value t)
      (spy-on 'pet-system-bin-dir)
      (spy-on 'executable-find :and-return-value "C:/Users/user/Anaconda3/envs/project/python")
      (expect (pet-executable-find "python") :to-equal "C:/Users/user/Anaconda3/envs/project/python")
      (expect 'pet-system-bin-dir :not :to-have-been-called)))

  (describe "when `pet-search-globally' is t"
    (it "should return the absolute path of the result of `pyenv which EXECUTABLE' if no virtualenv is found but `pyenv' is in `exec-path'"
      (spy-on 'pet-use-pre-commit-p )
      (spy-on 'pet-virtualenv-root )
      (spy-on 'pet--executable-find :and-call-fake (lambda (executable &optional _)
                                                     (when (equal executable "pyenv")
                                                       "/usr/bin/pyenv")))
      (spy-on 'process-lines :and-return-value '("/home/user/.pyenv/versions/3.10.5/bin/python"))
      (expect (pet-executable-find "python" t) :to-equal "/home/user/.pyenv/versions/3.10.5/bin/python")
      (expect 'process-lines :to-have-been-called-with "pyenv" "which" "python")
      (expect 'pet--executable-find :to-have-been-called-times 1))

    (it "should return the absolute path the executable for a project from `exec-path'"
      (spy-on 'pet-use-pre-commit-p )
      (spy-on 'pet-virtualenv-root )
      (spy-on 'pet--executable-find :and-call-fake (lambda (executable &optional _)
                                                     (when (equal executable "black")
                                                       "/home/user/project/.venv/bin/black")))
      (expect (pet-executable-find "black" t) :to-equal "/home/user/project/.venv/bin/black")
      (expect 'pet--executable-find :to-have-been-called-times 2)))

  (describe "when `pet-search-globally' is nil"
    (before-each
      (setq-local pet-search-globally nil))

    (after-each
      (kill-local-variable 'pet-search-globally))

    (it "should not return the absolute path of the result of `pyenv which EXECUTABLE' if no virtualenv is found but `pyenv' is in `exec-path'"
      (spy-on 'pet-use-pre-commit-p )
      (spy-on 'pet-virtualenv-root )
      (spy-on 'pet--executable-find :and-call-fake (lambda (executable &optional _)
                                                     (when (equal executable "pyenv")
                                                       "/usr/bin/pyenv")))
      (spy-on 'process-lines :and-return-value '("/home/user/.pyenv/versions/3.10.5/bin/python"))

      (expect (pet-executable-find "python" nil) :to-equal nil)
      (expect 'process-lines :not :to-have-been-called-with "pyenv" "which" "python")
      (expect 'pet--executable-find :to-have-been-called-times 0))

    (it "should not return the absolute path the executable for a project from `exec-path'"
      (spy-on 'pet-use-pre-commit-p )
      (spy-on 'pet-virtualenv-root )
      (spy-on 'pet--executable-find :and-call-fake (lambda (executable &optional _)
                                                     (when (equal executable "black")
                                                       "/home/user/project/.venv/bin/black")))

      (expect (pet-executable-find "black" nil) :to-equal nil)
      (expect 'pet--executable-find :to-have-been-called-times 0))))

