**P** ython **E** xecutable **T** racker
========================================

Greatings fellow Pythonistas and Emacs users!

Have you ever worked on a project that uses one of the many Python package
managers and/or virtual environments, where all the linters, formatters and
commit hooks are set up meticulously, and then when you fire up Emacs, packages
like `flycheck <https://www.flycheck.org/en/latest/>`_ or `lsp-mode
<https://emacs-lsp.github.io/lsp-mode/>`_ are either unable to find the binary
in your virtualenv, or are using a wrong one?

Have you ever tried one of the 11+ Emacs virtualenv packages to help you fix
this problem, but are still at a lost at why your other favorite Emacs packages
still can't find the right binaries, or they stop working when you switch to a
different project using a different flavor of virtualenv?

If you answer "yes" for any of these questions, you've come to the right place.


How does ``pet`` work?
----------------------

The first key insight is to recognize the paths to executables of many of these
linting and formatting Emacs packages rely on are configurable.

The second key insight is Emacs allows you to setup a different value for the
exectuable path on a per buffer basis, and that these packages work with these
buffer-local values.

The hardest problem is finding the correct executable, this is what ``pet``
tries to solve.

As long as you use one of the supported Python virtualenv tools, ``pet`` will be
able to find the virtualenv root and binary you ask for, with **zero Emacs
configuration** necessary.

``pet`` works well with popular source code project management packages such as
`Projectile <https://docs.projectile.mx/projectile/index.html>`_ and the
built-in ``project.el``. The first time you call one the few ``pet`` helper
functions, it will use Projectile or project.el to detect the root of your
project, search for the configuration files for the many supported Python
virtualenv tools, and then lookup the location of the virtualenv based on the
content of the configuration files. Once a virtualenv is found, all executables
are found by looking into its ``bin`` directory.


Supported Python Virtual Environment Tools
------------------------------------------

- `pre-commit <https://pre-commit.com/>`_
- `poetry <https://python-poetry.org/>`_
- `pipenv <https://pipenv.pypa.io/en/latest/>`_
- `direnv <https://direnv.net/>`_
- `venv <https://docs.python.org/3/library/venv.html>`_ or `virtualenv
  <https://virtualenv.pypa.io/en/latest/>`_
- `pdm <https://github.com/pdm-project/pdm>`_
- `pipx <https://pypa.github.io/pipx/>`_
- `pyenv <https://github.com/pyenv/pyenv>`_
- Whatever is on your ``VIRTUAL_ENV`` environment variable


Supported Emacs Packages
------------------------

- Built-in `project.el <https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html>`_
- `projectile <https://docs.projectile.mx/projectile/index.html>`_
- `direnv.el <https://github.com/wbolster/emacs-direnv>`_
- `eglot <https://github.com/joaotavora/eglot>`_ (to make it work, eglot must be activated after pet)
- `flycheck <https://www.flycheck.org/en/latest/>`_
- `lsp-jedi <https://github.com/fredcamps/lsp-jedi>`_
- `lsp-pyright <https://github.com/emacs-lsp/lsp-pyright>`_
- `dap-python <https://emacs-lsp.github.io/dap-mode/page/configuration/#python>`_
- `blacken <https://github.com/pythonic-emacs/blacken>`_
- `yapfify <https://github.com/JorisE/yapfify>`_
- `python-black <https://github.com/wbolster/emacs-python-black>`_
- `python-isort <https://github.com/wyuenho/emacs-python-isort>`_
- `python-pytest <https://github.com/wbolster/emacs-python-pytest>`_


System Requirements
-------------------

Currently ``pet`` requires a program to convert TOML to JSON, a program to
convert YAML to JSON, and if you are using Emacs < 29, the ``sqlite3`` command
to be installed on your system.

By default, both the TOML to JSON and YAML to JSON converters are configured to
use `dasel <https://github.com/TomWright/dasel>`_.  If you are on Linux, it may
be more convenient to use `tomljson
<https://github.com/pelletier/go-toml#tools>`_ and `yq
<https://github.com/mikefarah/yq>`_ since both of which are likely to be
available from the system package management system.

When a suitable Emacs Lisp YAML and TOML parser becomes available, ``dasel``
will be made optional.


Usage
-----

If you are using Emacs on macOS, to get the most out of ``pet``, it is best to
install `exec-path-from-shell
<https://github.com/purcell/exec-path-from-shell>`_ first to ensure all of the
`Supported Python Virtual Environment Tools`_ are available in your
``exec-path``. Once your ``exec-path`` is synced up to your shell's ``$PATH``
environment variable, you can use the following ways to help you setup the rest
of your Emacs packages **properly**.


Basic Setup
+++++++++++

Generally, the following snippet is all you'll need:

.. code-block:: elisp

   (require 'pet)
   (add-hook 'python-mode-hook 'pet-minor-mode)


Or, if you use `use-package <https://github.com/jwiegley/use-package>`_:

.. code-block:: elisp

   (use-package pet
     :hook (python-mode . pet-mode))


This will setup the buffer local variables for all of the `Supported Emacs
Packages`_.


Advanced Usage
++++++++++++++

If you need to configure a package that ``pet`` doesn't support, or only want to
configure a couple of packages instead of all the supported one, ``pet`` offers
2 autoloaded functions to help you find the correct path to the executable and
virtualenv directory:

- ``(pet-executable-find EXECUTABLE)``
- ``(pet-virtualenv-root)``

For example, to set up ``python-mode`` to use the correct interpreter when you
execute ``M-x run-python``:

.. code-block:: elisp

   (add-hook 'python-mode-hook
             (lambda ()
               (setq-local python-shell-interpreter (pet-executable-find "python")
                           python-shell-virtualenv-root (pet-virtualenv-root))))


For ``flycheck``, due to its complexity, ``pet`` also comes with another
autoloaded function to help you setup the ``flake8``, ``pylint`` and ``mypy``
checkers:

.. code-block:: elisp

   (add-hook 'python-mode-hook 'pet-flycheck-setup)


Complete Example
++++++++++++++++

.. code-block:: elisp

   (use-package exec-path-from-shell
     :if (memq (window-system) '(mac ns))
     :config (exec-path-from-shell-initialize))

   (use-package flycheck)

   (use-package lsp)

   (use-package lsp-jedi
     :after lsp)

   (use-package lsp-pyright
     :after lsp)

   (use-package dap-python
     :after lsp)

   (use-package python-pytest)

   (use-package python-black)

   (use-package python-isort)

   (use-package pet
     :ensure-system-package (dasel sqlite3)
     :config
     (add-hook 'python-mode-hook
               (lambda ()
                 (setq-local python-shell-interpreter (pet-executable-find "python")
                             python-shell-virtualenv-root (pet-virtualenv-root))

                 (pet-flycheck-setup)
                 (flycheck-mode 1)

                 (setq-local lsp-jedi-executable-command
                             (pet-executable-find "jedi-language-server"))

                 (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter
                             lsp-pyright-venv-path python-shell-virtualenv-root)

                 (lsp)

                 (setq-local dap-python-executable python-shell-interpreter)

                 (setq-local python-pytest-executable (pet-executable-find "pytest"))

                 (when-let ((black-executable (pet-executable-find "black")))
                   (setq-local python-black-command black-executable)
                   (python-black-on-save-mode 1))

                 (when-let ((isort-executable (pet-executable-find "isort")))
                   (setq-local python-isort-command isort-executable)
                   (python-isort-on-save-mode 1)))))


FAQ
---

How do I get ``pet`` to pick up the virtualenv created by ``direnv`` or similar tools?
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Try `direnv.el <https://github.com/wbolster/emacs-direnv>`_, specifically, `this
PR <https://github.com/wbolster/emacs-direnv/pull/80>`_.

Once you have set up ``exec-path`` in your ``python-mode`` buffer using
``direnv.el``, ``pet`` will automatically pick up the executables.


Why didn't ``pet`` set up the executable variables on a fresh Python project clone?
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

``Pet`` does not automatically create virtualenvs for you. If you have a fresh
clone, you must create the virtualenv and install your development dependencies
into it first. Once it is done, the next time you open a Python file buffer
``pet`` will automatically set up the executable variables for you.

To find out how to do it, please find the virtualenv tool in question from
`Supported Python Virtual Environment Tools`_, and visit its documentation for
details.


Why doesn't ``pet`` simply set a buffer-local ``exec-path``?
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The reason is mainly due to the fact that many Python projects use development
tools located in different virtualenvs. This means ``exec-path`` needs to be
prepended with all of the virtualenvs for all of the dev tools, and always kept
in the correct order. An example where this approach may cause issues is dealing
with projects that use ``pre-commit`` and ``direnv``. A typical ``pre-commit``
configuration may include many "hooks", where each of them is isolated in its
own virtualenv. While prepending many directories to ``exec-path`` is not
problematic in itself, playing well with other Emacs packages that mutate
``exec-path`` reliably is non-trivial. Providing an absolute path to executable
variables conveniently sidesteps this complexity, while being slightly more
performant.

In addition, there are Emacs packages, most prominantly ``flycheck`` that by
default require dev tools to be installed into the same virtualenv as the first
``python`` executable found on ``exec-path``. Changing this behavior requires
setting the corresponding ``flycheck`` checker executable variable to the
intended absolute path.


``pet`` can't find my virtualenvs, how do I debug it?
+++++++++++++++++++++++++++++++++++++++++++++++++++++

You can turn on ``pet-debug`` and watch what comes out in the ``*Messages*``
buffer.  In addition, you can use ``M-x pet-verify-setup`` in your Python
buffers to find out what was detected.


Do I still need any of the 11+ virtualenv Emacs packages?
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Nope. You can uninstall them all. This is the raison d'être of this package.


License
-------

`GPLv3 <./LICENSE>`_
