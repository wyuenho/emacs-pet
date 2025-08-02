;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-find-file-from-project"
  :var ((project-a-root "/home/user/project-a/")
        (project-b-root "/home/user/project-b/")
        (test-file "pyproject.toml")
        (test-file-path "/home/user/project-a/pyproject.toml")
        (another-file "setup.cfg")
        (another-file-path "/home/user/project-a/setup.cfg")
        saved-pet-cache
        saved-pet-find-file-functions)

  (before-each
    (setq-local pet-cache nil))

  (after-each
    (kill-local-variable 'pet-cache)
    (kill-local-variable 'pet-find-file-functions))

  (describe "Core Functionality"
    (it "should return cached result without calling find functions"
      (spy-on 'pet-project-root :and-return-value project-a-root)

      (pet-cache-put (list project-a-root :files test-file) test-file-path)

      (spy-on 'pet-find-file-from-project-root)
      (spy-on 'pet-locate-dominating-file)

      (setq-local pet-find-file-functions '(pet-find-file-from-project-root
                                            pet-locate-dominating-file))

      (expect (pet-find-file-from-project test-file) :to-equal test-file-path)

      (expect 'pet-find-file-from-project-root :not :to-have-been-called)
      (expect 'pet-locate-dominating-file :not :to-have-been-called))

    (it "should call functions in order, cache first success, and stop early"
      (spy-on 'pet-project-root :and-return-value project-a-root)

      (spy-on 'pet-find-file-from-project-root)
      (spy-on 'pet-locate-dominating-file :and-return-value test-file-path)
      (spy-on 'pet-find-file-from-project-root-natively)

      (setq-local pet-find-file-functions '(pet-find-file-from-project-root
                                            pet-locate-dominating-file
                                            pet-find-file-from-project-root-natively))

      (expect (pet-cache-get (list project-a-root :files test-file)) :to-be nil)

      (expect (pet-find-file-from-project test-file) :to-equal test-file-path)

      (expect 'pet-find-file-from-project-root :to-have-been-called-with test-file)
      (expect 'pet-locate-dominating-file :to-have-been-called-with test-file)
      (expect 'pet-find-file-from-project-root-natively :not :to-have-been-called)

      (expect (pet-cache-get (list project-a-root :files test-file)) :to-equal test-file-path))

    (it "should return nil when not in a project"
      (spy-on 'pet-project-root)

      (spy-on 'pet-find-file-from-project-root)

      (setq-local pet-find-file-functions '(pet-find-file-from-project-root))

      (expect (pet-find-file-from-project test-file) :to-be nil)

      (expect 'pet-find-file-from-project-root :not :to-have-been-called)))

  (describe "Function Chain Behavior"
    (it "should try all functions, return nil, and not cache nil results"
      (spy-on 'pet-project-root :and-return-value project-a-root)

      (spy-on 'pet-find-file-from-project-root)
      (spy-on 'pet-locate-dominating-file)
      (spy-on 'pet-find-file-from-project-root-natively)

      (setq-local pet-find-file-functions '(pet-find-file-from-project-root
                                            pet-locate-dominating-file
                                            pet-find-file-from-project-root-natively))

      (expect (pet-find-file-from-project "nonexistent.txt") :to-be nil)

      (expect 'pet-find-file-from-project-root :to-have-been-called-with "nonexistent.txt")
      (expect 'pet-locate-dominating-file :to-have-been-called-with "nonexistent.txt")
      (expect 'pet-find-file-from-project-root-natively :to-have-been-called-with "nonexistent.txt")

      (expect (pet-cache-get (list project-a-root :files "nonexistent.txt")) :to-be nil)))

  (describe "Cache Isolation"
    (it "should cache different files independently in same project"
      (spy-on 'pet-project-root :and-return-value project-a-root)

      (spy-on 'pet-find-file-from-project-root :and-call-fake
              (lambda (file)
                (cond ((equal file test-file) test-file-path)
                      ((equal file another-file) another-file-path)
                      (t nil))))

      (setq-local pet-find-file-functions '(pet-find-file-from-project-root))

      (expect (pet-find-file-from-project test-file) :to-equal test-file-path)
      (expect (pet-find-file-from-project another-file) :to-equal another-file-path)

      (expect (pet-cache-get (list project-a-root :files test-file)) :to-equal test-file-path)
      (expect (pet-cache-get (list project-a-root :files another-file)) :to-equal another-file-path))

    (it "should cache same filename separately per project"
      (spy-on 'pet-project-root :and-return-value project-a-root)
      (spy-on 'pet-find-file-from-project-root :and-return-value test-file-path)

      (setq-local pet-find-file-functions '(pet-find-file-from-project-root))

      (expect (pet-find-file-from-project test-file) :to-equal test-file-path)
      (expect (pet-cache-get (list project-a-root :files test-file)) :to-equal test-file-path)

      (spy-on 'pet-project-root :and-return-value project-b-root)

      (expect (pet-cache-get (list project-b-root :files test-file)) :to-be nil)

      (spy-on 'pet-find-file-from-project-root :and-return-value "/home/user/project-b/pyproject.toml")

      (setq-local pet-find-file-functions '(pet-find-file-from-project-root))

      (expect (pet-find-file-from-project test-file) :to-equal "/home/user/project-b/pyproject.toml")
      (expect 'pet-find-file-from-project-root :to-have-been-called-with test-file)

      (expect (pet-cache-get (list project-a-root :files test-file)) :to-equal test-file-path)
      (expect (pet-cache-get (list project-b-root :files test-file)) :to-equal "/home/user/project-b/pyproject.toml"))))

;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
