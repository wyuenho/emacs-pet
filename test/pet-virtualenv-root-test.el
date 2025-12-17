;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-virtualenv-root"
  :var ((project-root "/home/user/project/")
        (conda-path "/usr/bin/conda")
        (conda-virtualenv "/home/user/miniforge3/envs/project/")
        (mamba-path "/usr/bin/micromamba")
        (mamba-virtualenv "/home/user/micromamba/envs/project/")
        (pixi-path "/usr/bin/pixi")
        (pixi-virtualenv "/home/user/project/.pixi/envs/default/")
        (poetry-path "/usr/bin/poetry")
        (poetry-virtualenv "/home/user/.cache/pypoetry/virtualenvs/project/")
        (pipenv-path "/usr/bin/pipenv")
        (pipenv-virtualenv "/home/user/.local/share/virtualenvs/project/")
        (venv-virtualenv "/home/user/project/.venv/")
        (pyenv-path "/usr/bin/pyenv")
        (pyenv-virtualenv "/home/user/.pyenv/versions/project/")
        (pyenv-virtualenv-truename "/home/user/.pyenv/versions/3.8/envs/project/")
        (old-default-directory default-directory)
        (home (getenv "HOME")))

  (before-each
    (setq-local process-environment (copy-sequence process-environment))
    (setenv "HOME" "/home/user/")
    (setq-local default-directory "/home/user/project")
    (spy-on 'pet-project-root :and-return-value project-root)
    (setq-local pet-cache nil))

  (after-each
    (kill-local-variable 'process-environment)
    (setq-local default-directory old-default-directory)
    (kill-local-variable 'pet-cache))

  (it "should not cache nil when not in a project"
    (spy-on 'pet-project-root)
    (expect (pet-virtualenv-root) :to-be nil)
    (expect pet-cache :to-be nil))

  (it "should return the absolute path of the virtualenv for a project from `VIRTUAL_ENV'"
    (spy-on 'getenv :and-call-fake (lambda (name) (when (equal name "VIRTUAL_ENV") "/home/user/.venvs/project")))
    (expect (pet-virtualenv-root) :to-equal "/home/user/.venvs/project"))


  (it "should return the absolute path of the virtualenv for a project using `poetry'"
    (spy-on 'pet-use-pixi-p)
    (spy-on 'pet-use-conda-p)
    (spy-on 'pet-use-mamba-p)
    (spy-on 'pet-use-poetry-p :and-return-value poetry-path)
    (spy-on 'pet-pyproject-path :and-return-value "/home/user/project/pyproject.toml")
    (spy-on 'call-process :and-call-fake (lambda (&rest _) (insert poetry-virtualenv) 0))
    (expect (pet-virtualenv-root) :to-equal poetry-virtualenv)
    (expect (pet-cache-get (list project-root :virtualenv)) :to-equal poetry-virtualenv)
    (expect 'call-process :to-have-been-called-with poetry-path nil t nil "env" "info" "--no-ansi" "--path"))

  (it "should return the absolute path of the virtualenv for a project using `pipenv'"
    (spy-on 'pet-use-pixi-p)
    (spy-on 'pet-use-conda-p)
    (spy-on 'pet-use-mamba-p)
    (spy-on 'pet-use-hatch-p)
    (spy-on 'pet-use-poetry-p)
    (spy-on 'pet-use-pipenv-p :and-return-value pipenv-path)
    (spy-on 'pet-pipfile-path :and-return-value "/home/user/project/Pipfile")
    (spy-on 'process-file :and-call-fake (lambda (&rest _) (insert pipenv-virtualenv) 0))
    (expect (pet-virtualenv-root) :to-equal pipenv-virtualenv)
    (expect (pet-cache-get (list project-root :virtualenv)) :to-equal pipenv-virtualenv)
    (expect 'process-file :to-have-been-called-with pipenv-path nil t nil "--quiet" "--venv"))

  (it "should return the absolute path of the `.venv' or `venv' directory in a project"
    (spy-on 'pet-use-pixi-p)
    (spy-on 'pet-use-conda-p)
    (spy-on 'pet-use-mamba-p)
    (spy-on 'pet-use-hatch-p)
    (spy-on 'pet-use-poetry-p)
    (spy-on 'pet-use-pipenv-p)
    (spy-on 'locate-dominating-file :and-return-value project-root)
    (expect (pet-virtualenv-root) :to-equal venv-virtualenv)
    (expect (pet-cache-get (list project-root :virtualenv)) :to-equal venv-virtualenv))

  (it "should return the absolute path of the virtualenv for a project using `pyenv'"
    (spy-on 'pet-use-pixi-p)
    (spy-on 'pet-use-conda-p)
    (spy-on 'pet-use-mamba-p)
    (spy-on 'pet-use-hatch-p)
    (spy-on 'pet-use-poetry-p)
    (spy-on 'pet-use-pipenv-p)
    (spy-on 'locate-dominating-file)
    (spy-on 'pet-use-pyenv-p :and-return-value pyenv-path)
    (spy-on 'pet-python-version-path :and-return-value "/home/user/project/.python-version")
    (spy-on 'process-file :and-call-fake (lambda (&rest _) (insert pyenv-virtualenv) 0))
    (spy-on 'file-truename :and-call-fake (lambda (name) (when (equal name pyenv-virtualenv) pyenv-virtualenv-truename)))
    (expect (pet-virtualenv-root) :to-equal pyenv-virtualenv-truename)
    (expect (pet-cache-get (list project-root :virtualenv)) :to-equal pyenv-virtualenv-truename)
    (expect 'process-file :to-have-been-called-with pyenv-path nil t nil "prefix"))

  (it "should return the absolute path of the virtualenv for a project using `hatch' with hatch.toml"
    (spy-on 'pet-use-pixi-p)
    (spy-on 'pet-use-conda-p)
    (spy-on 'pet-use-mamba-p)
    (spy-on 'pet-use-poetry-p)
    (spy-on 'pet-use-pipenv-p)
    (spy-on 'locate-dominating-file)
    (spy-on 'pet-use-pyenv-p)
    (spy-on 'pet-use-hatch-p :and-return-value "/usr/bin/hatch")
    (spy-on 'pet-hatch-path :and-return-value "/home/user/project/hatch.toml")
    (spy-on 'pet-pyproject-path)
    (spy-on 'pet-run-process-get-output :and-return-value "/home/user/.hatch/envs/project/default")
    (expect (pet-virtualenv-root) :to-equal "/home/user/.hatch/envs/project/default")
    (expect (pet-cache-get (list project-root :virtualenv)) :to-equal "/home/user/.hatch/envs/project/default"))

  (it "should return the absolute path of the virtualenv for a project using `hatch' with pyproject.toml"
    (spy-on 'pet-use-pixi-p)
    (spy-on 'pet-use-conda-p)
    (spy-on 'pet-use-mamba-p)
    (spy-on 'pet-use-poetry-p)
    (spy-on 'pet-use-pipenv-p)
    (spy-on 'locate-dominating-file)
    (spy-on 'pet-use-pyenv-p)
    (spy-on 'pet-use-hatch-p :and-return-value "/usr/bin/hatch")
    (spy-on 'pet-hatch-path)
    (spy-on 'pet-pyproject-path :and-return-value "/home/user/project/pyproject.toml")
    (spy-on 'pet-run-process-get-output :and-return-value "/home/user/.hatch/envs/project/default")
    (expect (pet-virtualenv-root) :to-equal "/home/user/.hatch/envs/project/default")
    (expect (pet-cache-get (list project-root :virtualenv)) :to-equal "/home/user/.hatch/envs/project/default"))

  (it "should return the absolute path of the virtualenv for a project using `pixi'"
    (spy-on 'pet-use-conda-p)
    (spy-on 'pet-use-mamba-p)
    (spy-on 'pet-use-poetry-p)
    (spy-on 'pet-use-pipenv-p)
    (spy-on 'locate-dominating-file)
    (spy-on 'pet-use-pyenv-p)
    (spy-on 'pet-use-pixi-p :and-return-value pixi-path)
    (spy-on 'pet-run-process-get-output :and-call-fake
            (lambda (program &rest args)
              (when (and (equal program pixi-path)
                         (equal args '("info" "--json")))
                (format "{\"environments_info\":{\"default\":{\"prefix\":\"%s\"}}}" pixi-virtualenv))))
    (expect (pet-virtualenv-root) :to-equal pixi-virtualenv)
    (expect (pet-cache-get (list project-root :virtualenv)) :to-equal pixi-virtualenv))

  (it "should return the absolute path of the virtualenv for a project using `conda'"
    (spy-on 'pet-use-pixi-p)
    (spy-on 'pet-use-mamba-p)
    (spy-on 'pet-use-poetry-p)
    (spy-on 'pet-use-pipenv-p)
    (spy-on 'locate-dominating-file)
    (spy-on 'pet-use-pyenv-p)
    (spy-on 'pet-use-conda-p :and-return-value conda-path)
    (spy-on 'pet-environment :and-return-value `((prefix . ,conda-virtualenv)))
    (expect (pet-virtualenv-root) :to-equal conda-virtualenv)
    (expect (pet-cache-get (list project-root :virtualenv)) :to-equal conda-virtualenv))

  (it "should return the absolute path of the virtualenv for a project using `mamba'"
    (spy-on 'pet-use-pixi-p)
    (spy-on 'pet-use-conda-p)
    (spy-on 'pet-use-poetry-p)
    (spy-on 'pet-use-pipenv-p)
    (spy-on 'locate-dominating-file)
    (spy-on 'pet-use-pyenv-p)
    (spy-on 'pet-use-mamba-p :and-return-value mamba-path)
    (spy-on 'pet-environment :and-return-value `((prefix . ,mamba-virtualenv)))
    (expect (pet-virtualenv-root) :to-equal mamba-virtualenv)
    (expect (pet-cache-get (list project-root :virtualenv)) :to-equal mamba-virtualenv))

  (it "should return the absolute path of the virtualenv for a project if the root is found in cache"
    (pet-cache-put (list project-root :virtualenv) "/home/user/.venvs/env/")
    (expect (pet-virtualenv-root) :to-equal "/home/user/.venvs/env/"))

  (it "should cache 'none and skip detection on subsequent calls when no virtualenv found"
    (spy-on 'pet-use-pixi-p)
    (spy-on 'pet-use-conda-p)
    (spy-on 'pet-use-mamba-p)
    (spy-on 'pet-use-poetry-p)
    (spy-on 'pet-use-hatch-p)
    (spy-on 'pet-use-pipenv-p)
    (spy-on 'locate-dominating-file)
    (spy-on 'pet-use-pyenv-p)
    (expect (pet-virtualenv-root) :to-be nil)
    (expect (pet-cache-get (list project-root :virtualenv)) :to-equal 'none)
    (expect (pet-virtualenv-root) :to-be nil)
    (expect 'pet-use-poetry-p :to-have-been-called-times 1)))

;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
