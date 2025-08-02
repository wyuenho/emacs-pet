;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-conda-venv-p"
  (it "should return t for conda environments with conda-meta directory"
    (spy-on 'file-exists-p :and-call-fake
            (lambda (path)
              (equal path "/home/user/anaconda3/envs/myenv/conda-meta")))

    (expect (pet-conda-venv-p "/home/user/anaconda3/envs/myenv") :to-be-truthy))

  (it "should return t for mamba environments with conda-meta directory"
    (spy-on 'file-exists-p :and-call-fake
            (lambda (path)
              (equal path "/home/user/micromamba/envs/test/conda-meta")))

    (expect (pet-conda-venv-p "/home/user/micromamba/envs/test") :to-be-truthy))

  (it "should return t for pixi environments with conda-meta directory"
    (spy-on 'file-exists-p :and-call-fake
            (lambda (path)
              (equal path "/home/user/project/.pixi/envs/default/conda-meta")))

    (expect (pet-conda-venv-p "/home/user/project/.pixi/envs/default") :to-be-truthy))

  (it "should return nil for traditional virtualenvs without conda-meta directory"
    (spy-on 'file-exists-p)

    (expect (pet-conda-venv-p "/home/user/project/.venv") :to-be nil))

  (it "should return nil for nil input"
    (expect (pet-conda-venv-p nil) :to-be nil))

  (it "should handle paths with and without trailing slashes"
    (spy-on 'file-exists-p :and-call-fake
            (lambda (path)
              (equal path "/home/user/anaconda3/envs/myenv/conda-meta")))

    (expect (pet-conda-venv-p "/home/user/anaconda3/envs/myenv") :to-be-truthy)
    (expect (pet-conda-venv-p "/home/user/anaconda3/envs/myenv/") :to-be-truthy)))
;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
