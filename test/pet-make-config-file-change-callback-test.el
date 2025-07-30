;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-make-config-file-change-callback"
  (it "should return a function"
    (expect (functionp (pet-make-config-file-change-callback #'identity)) :to-be-truthy))

  (describe "when received deleted event"
    :var* ((descriptor 1)
           (file "/home/user/project/tox.ini")
           (event `((,file . ,descriptor))))

    (before-each
      (setq-local pet-cache nil)
      (spy-on 'pet-project-root :and-return-value "/home/user/project/")
      (spy-on 'pet-teardown-config-cache-and-watcher)
      (pet-cache-put (list "/home/user/project/" :configs file) "content")
      (pet-cache-put (list "/home/user/project/" :file-watchers file) descriptor)
      (defvar callback (pet-make-config-file-change-callback nil))
      (funcall callback `(,descriptor deleted ,file)))

    (after-each
      (kill-local-variable 'pet-cache)
      (makunbound 'callback)
      (unintern 'callback obarray))

    (it "should call teardown function to maintain cache invariant"
      (expect 'pet-teardown-config-cache-and-watcher :to-have-been-called-with file)))

  (describe "when received changed event"
    :var ((file "/home/user/project/tox.ini"))

    (before-each
      (setq-local pet-cache nil)
      (spy-on 'pet-project-root :and-return-value "/home/user/project/")
      (defun parser (file)
        "content")

      (spy-on 'parser :and-call-through)

      (defvar callback (pet-make-config-file-change-callback #'parser))
      (funcall callback `(1 changed ,file)))

    (after-each
      (kill-local-variable 'pet-cache)
      (fmakunbound 'parser)
      (unintern 'parser obarray)
      (makunbound 'callback)
      (unintern 'callback obarray))

    (it "should parse the file again and update unified cache"
      (expect 'parser :to-have-been-called-with file)
      (expect (pet-cache-get (list "/home/user/project/" :configs file)) :to-equal "content"))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
