;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-eglot--workspace-configuration-plist-advice"
  (before-each
    (spy-on 'jsonrpc--process))

  (it "should pass canonicalized PATH to FN if it's a directory"
    (spy-on 'mock-eglot--workspace-configuration-plist)
    (spy-on 'process-command :and-return-value '("/usr/bin/jedi-language-server"))
    (spy-on 'file-directory-p :and-return-value t)

    (pet-eglot--workspace-configuration-plist-advice
     'mock-eglot--workspace-configuration-plist
     "server" "/home/users/project")

    (expect 'mock-eglot--workspace-configuration-plist :to-have-been-called-with "server" "/home/users/project/"))

  (it "should pass PATH to FN directly if it's a not directory"
    (spy-on 'mock-eglot--workspace-configuration-plist)
    (spy-on 'process-command :and-return-value '("/usr/bin/jedi-language-server"))
    (spy-on 'file-directory-p )

    (pet-eglot--workspace-configuration-plist-advice
     'mock-eglot--workspace-configuration-plist
     "server" "/home/users/project/file")

    (expect 'mock-eglot--workspace-configuration-plist :to-have-been-called-with "server" "/home/users/project/file"))

  (it "should return `nil' when no dir local variables and pet server initialization options"
    (spy-on 'mock-eglot--workspace-configuration-plist)
    (spy-on 'process-command :and-return-value '("/usr/bin/some-lsp-server"))

    (expect (pet-eglot--workspace-configuration-plist-advice
             'mock-eglot--workspace-configuration-plist
             "server")
            :not :to-be-truthy)

    (expect 'mock-eglot--workspace-configuration-plist :to-have-been-called))

  (it "should return pet server initialization options when no dir local variables"
    (spy-on 'mock-eglot--workspace-configuration-plist)
    (spy-on 'process-command :and-return-value '("/usr/bin/pyright-langserver"))
    (spy-on 'pet-lookup-eglot-server-initialization-options
            :and-return-value
            '(:python
              (:pythonPath
               "/usr/bin/python"
               :venvPath
               "/home/user/project/")))

    (expect (pet-eglot--workspace-configuration-plist-advice
             'mock-eglot--workspace-configuration-plist
             "server")
            :to-equal '(:python
                        (:pythonPath
                         "/usr/bin/python"
                         :venvPath
                         "/home/user/project/")))

    (expect 'mock-eglot--workspace-configuration-plist :to-have-been-called))

  (it "should return dir local variables when pet server initialization options"
    (spy-on 'mock-eglot--workspace-configuration-plist
            :and-return-value
            '(:python
              (:pythonPath
               "/usr/bin/python"
               :venvPath
               "/home/user/project/")))
    (spy-on 'process-command :and-return-value '("/usr/bin/pyright-langserver"))
    (spy-on 'pet-lookup-eglot-server-initialization-options)

    (expect (pet-eglot--workspace-configuration-plist-advice
             'mock-eglot--workspace-configuration-plist
             "server")
            :to-equal '(:python
                        (:pythonPath
                         "/usr/bin/python"
                         :venvPath
                         "/home/user/project/")))

    (expect 'mock-eglot--workspace-configuration-plist :to-have-been-called))

  (it "should return dir local variables and pet server initialization options when both available"
    (spy-on 'mock-eglot--workspace-configuration-plist
            :and-return-value
            '(:python
              (:pythonPath
               "/usr/bin/python")))
    (spy-on 'process-command :and-return-value '("/usr/bin/pyright-langserver"))
    (spy-on 'pet-lookup-eglot-server-initialization-options
            :and-return-value
            '(:python
              (:venvPath
               "/home/user/project/")))

    (expect (pet-eglot--workspace-configuration-plist-advice
             'mock-eglot--workspace-configuration-plist
             "server")
            :to-equal '(:python
                        (:pythonPath
                         "/usr/bin/python"
                         :venvPath
                         "/home/user/project/")))

    (expect 'mock-eglot--workspace-configuration-plist :to-have-been-called)))

