python-x.el
===========

Greatings fellow Pythonistas and Emacs users!

Have you ever worked on a project that uses one of the many Python package
managers and/or virtual environments, where all the linters, formatters and
commit hooks are setup meticulously, and then when you fire up Emacs, packages
like `flycheck <https://www.flycheck.org/en/latest/>`_ or `lsp-mode
<https://emacs-lsp.github.io/lsp-mode/>`_ are either unable to find the binary
in your virtualenv, or are using a wrong one?

Have you ever tried one of the 11+ Emacs virtualenv packages to help you fix
this problem, but are still at a lost at why your other favorite Emacs packages
still can't find the right binaries?

If you answer "yes" for any of these questions, you've come to the right place.


How does ``python-x`` work?
---------------------------

The first key insight is to recognize the executables that many of these linting
and formatting Emacs packages rely on are configurable.

The second key insight is Emacs allows you to setup a different value for the
exectuable path on a per buffer basis.

As long as you use one of the supported Python virtualenv tools, ``python-x``
will be able to find the virtualenv root and binary you ask for, with **zero
Emacs configuration** necessary.

While ``python-x`` has zero dependency on other Emacs packages, it works well
with popular source code project management packages such as `Projectile
<https://docs.projectile.mx/projectile/index.html>`_ and the built-in
``project.el``. The first time you call one the few ``python-x`` helper
functions, it will use Projectile or project.el to detect the root of your
project, search for configuration files for many supported Python virtualenv
tools, and then lookup the location of the virtualenv based on the content of
the configuration files. Once a virtualenv is found, all executables are found
by looking into its ``bin`` directory.


Python Virtual Environment Tooling Support
------------------------------------------

- `pre-commit <https://pre-commit.com/>`_
- `poetry <https://python-poetry.org/>`_
- `pyenv <https://github.com/pyenv/pyenv>`_
- `direnv <https://direnv.net/>`_
- Whatever is on your ``$PATH``


Usage
-----

To get the most out of ``python-x``, it is best paired with `exec-path-from-shell
<https://github.com/purcell/exec-path-from-shell>`_. Once you have your
``exec-path`` synced up to your shell's ``$PATH`` environment variable, you can the
following ``python-x`` functions to help you setup the rest of your Emacs packages
**properly**.

``python-x`` offers 2 autoloaded functions to help you find the correct path to
the executable and virtualenv directory you'll need:

- ``(python-x-executable-find)``
- ``(python-x-virtualenv-root)``

For example, to set up ``python-mode`` to use the correct interpreter for ``M-x
run-python``:

.. code-block:: elisp

   (add-hook 'python-mode-hook
             (lambda ()
               (setq-local python-shell-interpreter (python-x-executable-find "python")
                           python-shell-virtualenv-root (python-x-virtualenv-root))))


For ``flycheck``, due to its complexity, ``python-x`` also comes with another
autoloaded function to help you setup the ``flake8``, ``pylint`` and ``mypy``
checkers:

.. code-block:: elisp

   (add-hook 'python-mode-hook
             (lambda ()
               (with-eval-after-load 'flycheck
                 (python-x-flycheck-setup))))


Complete Example
++++++++++++++++

.. code-block:: elisp

   (use-package exec-path-from-shell
     :if (memq (window-system) '(mac ns))
     :config (exec-path-from-shell-initialize))

   (use-package flycheck)

   (use-package lsp)

   (use-package lsp-pyright
     :after lsp)

   (use-package dap-python)

   (use-package python-pytest)

   (use-package python-black)

   (use-package python-isort)

   (use-package python-x
     :quelpa (python-x :fetcher github :repo "wyuenho/emacs-python-x")
     :config
     (add-hook 'python-mode-hook
               (lambda ()
                 (setq-local python-shell-interpreter (python-x-executable-find "python")
                             python-shell-virtualenv-root (python-x-virtualenv-root))

                 (with-eval-after-load 'flycheck
                   (python-x-flycheck-setup))

                 (with-eval-after-load 'lsp-pyright
                   (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter
                               lsp-pyright-venv-path python-shell-virtualenv-root))

                 (with-eval-after-load 'dap-python
                   (setq-local dap-python-executable python-shell-interpreter))

                 (with-eval-after-load 'python-pytest
                   (setq-local python-pytest-executable (python-x-executable-find "pytest")))

                 (with-eval-after-load 'python-black
                   (when-let ((black-executable (python-x-executable-find "black")))
                     (setq-local python-black-command black-executable)
                     (python-black-on-save-mode 1)))

                 (with-eval-after-load 'python-isort
                   (when-let ((isort-executable (python-x-executable-find "isort")))
                     (setq-local python-isort-command isort-executable)
                     (python-isort-on-save-mode 1))))))


License
-------

`GPLv3 <./LICENSE>`_
