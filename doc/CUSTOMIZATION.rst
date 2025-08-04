#####################
 Customization Guide
#####################

``pet`` provides several customization variables that you can adjust to
fit your needs:

***************************
 File Search Configuration
***************************

.. code:: elisp

   ;; Control the order and methods used to search for configuration files
   (setq pet-find-file-functions '(pet-find-file-from-project-root
                                   pet-locate-dominating-file
                                   pet-find-file-from-project-root-natively
                                   pet-find-file-from-project-root-recursively))

   ;; Directory names to search for when looking for virtualenvs at project root
   (setq pet-venv-dir-names '(".venv" "venv" "env"))

*****************************
 External Tool Configuration
*****************************

.. code:: elisp

   ;; TOML to JSON converter (default: "dasel")
   (setq pet-toml-to-json-program "tomljson")  ; or "dasel"
   (setq pet-toml-to-json-program-arguments '("-"))

   ;; YAML to JSON converter (default: "dasel")
   (setq pet-yaml-to-json-program "yq")  ; or "dasel"
   (setq pet-yaml-to-json-program-arguments '("-o=json"))

   ;; fd command configuration for fast file searches
   (setq pet-fd-command "fd")
   (setq pet-fd-command-args '("-tf" "-cnever" "-H" "-a" "-g"))

******************
 Parser Selection
******************

.. code:: elisp

   ;; Prefer Emacs Lisp parsers over external programs
   ;; When t, Pet will use tomlparse.el and yaml.el first,
   ;; falling back to external programs only if needed
   (setq pet-prefer-elisp-parsers t)

*****************
 Search Behavior
*****************

.. code:: elisp

   ;; Whether pet-executable-find should search outside project virtualenvs
   ;; Set to nil to only search within detected virtualenvs
   (setq pet-search-globally t)

*******
 Hooks
*******

.. code:: elisp

   ;; Functions to run after buffer-local variables are set up
   (add-hook 'pet-after-buffer-local-vars-setup
             (lambda () (message "Pet setup complete")))

   ;; Functions to run before buffer-local variables are torn down
   (add-hook 'pet-before-buffer-local-vars-teardown
             (lambda () (lsp-shutdown-workspace)))

***************************
 Project-specific Settings
***************************

You can set any of these variables on a per-project basis using
``.dir-locals.el``:

.. code:: elisp

   ;; Example .dir-locals.el for a large project
   ((python-mode . ((pet-find-file-functions . (pet-find-file-from-project-root
                                                pet-locate-dominating-file))
                    (pet-search-globally . nil))))
