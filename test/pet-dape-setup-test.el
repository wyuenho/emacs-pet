;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-dape-setup"
  (before-each
    (spy-on 'pet-find-file-from-project)
    (spy-on 'pet-executable-find :and-return-value "/usr/bin/python"))

  (it "should set up buffer local variable dape-command when no __main__.py is found"
    (pet-dape-setup)
    (expect (local-variable-p 'dape-command) :to-be-truthy)
    (expect dape-command :to-equal '(debugpy command "/usr/bin/python")))

  (it "should set up buffer local variable dape-command when a __main__.py is found"
    (spy-on 'pet-find-file-from-project :and-return-value "/home/user/project/src/foo/bar/__main__.py")
    (spy-on 'file-exists-p :and-call-fake
            (lambda (path)
              (member path
                      '("/home/user/project/src/foo/bar/__init__.py" "/home/user/project/src/foo/__init__.py"))))
    (pet-dape-setup)
    (expect (local-variable-p 'dape-command) :to-be-truthy)
    (expect dape-command :to-equal '(debugpy-module command "/usr/bin/python" :module "foo.bar")))

  (it "should set up buffer local variable dape-cwd-function"
    (pet-dape-setup)
    (expect (local-variable-p 'dape-cwd-function) :to-be-truthy)
    (expect dape-cwd-function :to-equal 'pet-project-root)))

