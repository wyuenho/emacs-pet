.. image:: https://github.com/wyuenho/emacs-pet/actions/workflows/ci.yml/badge.svg
   :target: https://github.com/wyuenho/emacs-pet/actions/workflows/ci.yml
   :alt: CI Status

**P** ython **E** xecutable **T** racker
========================================

Greatings fellow Pythonistas and Emacs users!

Have you ever worked on a project that uses one of the many Python package
managers and/or virtual environments, where all the linters, formatters and
commit hooks are set up meticulously, and then when you fire up Emacs, packages
like `flycheck <https://www.flycheck.org/en/latest/>`_ or `lsp-mode
<https://emacs-lsp.github.io/lsp-mode/>`_ are either unable to find the binary
in your virtualenv, or are using the wrong one?

Have you ever tried one of the 11+ Emacs virtualenv packages to help you fix
this problem, but are still at a lost at why your other favorite Emacs packages
still can't find the right binaries, or they stop working when you switch to a
different project using a different flavor of virtualenv?

If you answer "yes" for any of these questions, you've come to the right place.


How does ``pet`` work?
----------------------

The first key insight is to recognize the paths to the executables of many
Python linting and formatting Emacs packages are configurable.

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

- `pre-commit <https://pre-commit.com>`_
- `poetry <https://python-poetry.org>`_
- `pipenv <https://pipenv.pypa.io>`_
- `direnv <https://direnv.net>`_
- `venv <https://docs.python.org/3/library/venv.html>`_, `virtualenv
  <https://virtualenv.pypa.io>`_ or `virtualenvwrapper
  <https://virtualenvwrapper.readthedocs.io>`_ (`virtualenvwrapper caveats`_)
- `maturin <https://www.maturin.rs>`_
- `uv <https://github.com/astral-sh/uv>`_
- `pdm <https://pdm-project.org>`_
- `pipx <https://pipx.pypa.io>`_
- `pyenv <https://github.com/pyenv/pyenv>`_ (very poorly maintained, don't use
  it unless you are using Homebrew on macOS)
- `docker <https://hub.docker.com/_/python>`_
- `pixi <https://pixi.sh>`_
- `conda <https://docs.conda.io>`_ (no support for conda-project yet)
- `mamba
  <https://mamba.readthedocs.io/en/latest/installation/mamba-installation.html>`_
- `micromamba
  <https://mamba.readthedocs.io/en/latest/installation/micromamba-installation.html>`_
- Whatever is on your ``VIRTUAL_ENV`` environment variable
- Even when you aren't in a virtual environment


Supported Emacs Packages
------------------------

- Built-in `project.el <https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html>`_
- `projectile <https://docs.projectile.mx/projectile/index.html>`_
- `envrc <https://github.com/purcell/envrc>`_ (`direnv caveats`_)
- `eglot <https://github.com/joaotavora/eglot>`_ (`eglot versions supported`_)
- `dape <https://github.com/svaante/dape>`_
- `flycheck <https://www.flycheck.org/en/latest/>`_
- `lsp-jedi <https://github.com/fredcamps/lsp-jedi>`_
- `lsp-pyright <https://github.com/emacs-lsp/lsp-pyright>`_
- `dap-python <https://emacs-lsp.github.io/dap-mode/page/configuration/#python>`_
- `blacken <https://github.com/pythonic-emacs/blacken>`_
- `yapfify <https://github.com/JorisE/yapfify>`_
- `python-black <https://github.com/wbolster/emacs-python-black>`_
- `python-isort <https://github.com/wyuenho/emacs-python-isort>`_
- `python-pytest <https://github.com/wbolster/emacs-python-pytest>`_
- `ruff-format <https://melpa.org/#/ruff-format>`_
- `py-autopep8 <https://github.com/emacsmirror/py-autopep8>`_
- `auto-virtualenvwrapper <https://github.com/robert-zaremba/auto-virtualenvwrapper.el/>`_


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

   ;; Emacs < 26
   ;; You have to make sure this function is added to the hook last so it's
   ;; called first
   (add-hook 'python-mode-hook 'pet-mode)

   ;; Emacs 27+
   ;; The -10 tells `add-hook' to makes sure the function is called as early as
   ;; possible whenever it is added to the hook variable
   (add-hook 'python-mode-hook 'pet-mode -10)

   ;; Emacs 29+
   ;; This will turn on `pet-mode' on `python-mode' and `python-ts-mode'
   (add-hook 'python-base-mode-hook 'pet-mode -10)

Or, if you use `use-package <https://github.com/jwiegley/use-package>`_:

.. code-block:: elisp

   (use-package pet
     :config
     (add-hook 'python-base-mode-hook 'pet-mode -10))


This will setup the buffer local variables for all of the `Supported Emacs
Packages`_.


Environment Switching
+++++++++++++++++++++

For projects using conda, mamba, or pixi, ``pet`` now provides interactive
environment switching::

   M-x pet-conda-switch-environment
   M-x pet-mamba-switch-environment
   M-x pet-pixi-switch-environment

When you enable ``pet-mode`` on a fresh project using these tools, ``pet`` will
automatically prompt you to select an environment if none is currently active.


Advanced Usage
++++++++++++++

If you need to configure a package that ``pet`` doesn't support, or only want to
configure a couple of packages instead of all of the supported ones, ``pet``
offers 2 autoloaded functions to help you find the correct path to the
executable and virtualenv directory:

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

   (use-package eglot)

   (use-package python-pytest)

   (use-package python-black)

   (use-package python-isort)

   (use-package ruff-format)

   (use-package pet
     :ensure-system-package (dasel sqlite3)
     :config
     (add-hook 'python-mode-hook
               (lambda ()
                 (setq-local python-shell-interpreter (pet-executable-find "python")
                             python-shell-virtualenv-root (pet-virtualenv-root))

                 ;; (pet-eglot-setup)
                 ;; (eglot-ensure)

                 (pet-flycheck-setup)
                 (flycheck-mode)

                 (setq-local lsp-jedi-executable-command
                             (pet-executable-find "jedi-language-server"))

                 (setq-local lsp-pyright-python-executable-cmd python-shell-interpreter
                             lsp-pyright-venv-path python-shell-virtualenv-root)

                 (lsp)

                 (setq-local dap-python-executable python-shell-interpreter)

                 (setq-local python-pytest-executable (pet-executable-find "pytest"))

                 (when-let ((ruff-executable (pet-executable-find "ruff")))
                   (setq-local ruff-format-command ruff-executable)
                   (ruff-format-on-save-mode))

                 (when-let ((black-executable (pet-executable-find "black")))
                   (setq-local python-black-command black-executable)
                   (python-black-on-save-mode))

                 (when-let ((isort-executable (pet-executable-find "isort")))
                   (setq-local python-isort-command isort-executable)
                   (python-isort-on-save-mode)))))


FAQ
---

.. _direnv caveats:

How do I get ``pet`` to pick up the virtualenv or PATH created by ``direnv``?
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Short answer:

Use `envrc <https://github.com/purcell/envrc>`_.

.. code-block:: elisp

   (require 'envrc)
   (add-hook 'change-major-mode-after-body-hook 'envrc-mode)


Longer answer:

There are a number of packages similar to ``envrc`` such as ``direnv`` and
``buffer-env`` that claim to be able to configure ``direnv`` in Emacs. However,
they all suffer from various problems such as changing the environment and
``exec-path`` for the entire Emacs process, unable to activate early enough or
being too general to support direnv tightly.

Because ``pet`` needs to configure the buffer local variables **before** the
rest of the minor modes are activated, but **after** ``exec-path`` has been set
up by direnv, one must take care of choosing a minor mode package that allows
the user to customize when it takes effect. This requirement rules out
``direnv.el`` [1]_.

.. [1] Earlier versions of ``pet`` suggested ``direnv.el`` as a solution, it is
       no longer recommended due to this reason.


.. _eglot versions supported:

Which version of ``eglot`` is supported?
++++++++++++++++++++++++++++++++++++++++

The only version of ``eglot`` that doesn't work with ``pet`` is 1.17.*, which
unfortunately is the version that comes with Emacs 30.  There is no easy way to
support it without massively complicating the already complex advices required.
The easiest thing to do is to upgrade to 1.18, which has reverted the change the
broken change that prevents ``pet`` from working.


.. _virtualenvwrapper caveats:

My project uses ``virtualenvwrapper``, how do I get ``pet`` to pick up the virtualenv?
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

You can use ``envrc`` + `this direnv configuration
<https://github.com/direnv/direnv/wiki/Python#virtualenvwrapper>`_ to activate
your virtualenv or `auto-virtualenvwrapper
<https://github.com/robert-zaremba/auto-virtualenvwrapper.el/>`_. Note that in
any case, your virtualenv must be activated before turning on ``pet-mode`` in
order to make the environment variable ``VIRTUAL_ENV`` available to it. For
example:

.. code-block:: elisp

   (require 'auto-virtualenvwrapper)
   (require 'pet)

   (add-hook 'python-base-mode-hook
     (lambda ()
       (auto-virtualenvwrapper-activate)
       (pet-mode))
     -10)
   (add-hook 'window-configuration-change-hook #'auto-virtualenvwrapper-activate)
   (add-hook 'focus-in-hook #'auto-virtualenvwrapper-activate)


Why didn't ``pet`` set up the executable variables on a fresh Python project clone?
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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


My package didn't pick up the correct paths, how do I debug ``pet``?
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

You can turn on ``pet-debug`` and watch what comes out in the ``*Messages*``
buffer. In addition, you can use ``M-x pet-verify-setup`` in your Python buffers
to find out what was detected.

For ``lsp``, use ``lsp-describe-session``.

For ``eglot``, use ``eglot-show-workspace-configuration``.

For ``flycheck``, use ``flycheck-verify-setup``.


Do I still need any of the 11+ virtualenv Emacs packages?
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Nope. You can uninstall them all. This is the raison d'être of this package.


License
-------

`GPLv3 <./LICENSE>`_
