;; -*- lexical-binding: t; -*-

(require 'pet)

(describe "pet-eglot--workspace-configuration-plist-advice"
  (it "should merge user config and pet config correctly"
    (spy-on 'jsonrpc--process :and-return-value 'mock-process)
    (spy-on 'process-command :and-return-value '("/usr/bin/pyright"))
    (spy-on 'file-directory-p)
    (spy-on 'pet-lookup-eglot-server-initialization-options
            :and-return-value '(:python (:pythonPath "/usr/bin/python")))
    (spy-on 'mock-fn :and-return-value '(:python (:venvPath "/home/user/.venv")))

    (expect (pet-eglot--workspace-configuration-plist-advice 'mock-fn "server" "/path")
            :to-equal '(:python (
                                 :venvPath "/home/user/.venv"
                                 :pythonPath "/usr/bin/python"))))

  (it "should canonicalize directory paths"
    (spy-on 'jsonrpc--process :and-return-value 'mock-process)
    (spy-on 'process-command :and-return-value '("/usr/bin/pyright"))
    (spy-on 'file-directory-p :and-return-value t)
    (spy-on 'pet-lookup-eglot-server-initialization-options)
    (spy-on 'mock-fn)

    (pet-eglot--workspace-configuration-plist-advice 'mock-fn "server" "/home/user/project")

    (expect 'mock-fn :to-have-been-called-with "server" "/home/user/project/"))

  (it "should handle nil path"
    (spy-on 'jsonrpc--process :and-return-value 'mock-process)
    (spy-on 'process-command :and-return-value '("/usr/bin/pyright"))
    (spy-on 'pet-lookup-eglot-server-initialization-options)
    (spy-on 'mock-fn)

    (pet-eglot--workspace-configuration-plist-advice 'mock-fn "server")

    (expect 'file-directory-p :not :to-have-been-called)
    (expect 'mock-fn :to-have-been-called-with "server"))

  (it "should extract command from server and pass to pet lookup"
    (spy-on 'jsonrpc--process :and-return-value 'mock-process)
    (spy-on 'process-command :and-return-value '("/usr/bin/pyright" "--stdio"))
    (spy-on 'file-directory-p)
    (spy-on 'pet-lookup-eglot-server-initialization-options)
    (spy-on 'mock-fn)

    (pet-eglot--workspace-configuration-plist-advice 'mock-fn "server" "/path")

    (expect 'jsonrpc--process :to-have-been-called-with "server")
    (expect 'process-command :to-have-been-called-with 'mock-process)
    (expect 'pet-lookup-eglot-server-initialization-options
            :to-have-been-called-with '("/usr/bin/pyright" "--stdio"))))


;; Local Variables:
;; eval: (buttercup-minor-mode 1)
;; End:
