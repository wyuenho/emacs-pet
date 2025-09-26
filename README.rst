.. image:: https://github.com/wyuenho/emacs-pet/actions/workflows/ci.yml/badge.svg
   :target: https://github.com/wyuenho/emacs-pet/actions/workflows/ci.yml
   :alt: CI Status

##########################################
 **P** ython **E** xecutable **T** racker
##########################################

Greetings fellow Pythonistas and Emacs users!

Have you ever worked on a project that uses one of the many Python
package managers and/or virtual environments, where all the linters,
formatters and commit hooks are set up meticulously, and then when you
fire up Emacs, packages like `flycheck
<https://www.flycheck.org/en/latest/>`_ or `lsp-mode
<https://emacs-lsp.github.io/lsp-mode/>`_ are either unable to find the
binary in your virtualenv, or are using the wrong one?

Have you ever tried one of the 11+ Emacs virtualenv packages to help you
fix this problem, but are still at a loss as to why your other favorite
Emacs packages still can't find the right binaries, or they stop working
when you switch to a different project using a different flavor of
virtualenv?

If you answer "yes" for any of these questions, you've come to the right
place.

************************
 How does ``pet`` work?
************************

The first key insight is to recognize the paths to the executables of
many Python linting and formatting Emacs packages are configurable.

The second key insight is Emacs allows you to setup a different value
for the executable path on a per buffer basis, and that these packages
work with these buffer-local values.

The hardest problem is finding the correct executable, this is what
``pet`` tries to solve.

``pet`` solves this through automatic detection and configuration:

Detect your project
   Uses Projectile or built-in ``project.el`` to find your project root

Find your virtualenv
   Searches for virtualenvs using multiple methods: configuration files
   (``pyproject.toml``, ``Pipfile``, etc.), standard directories
   (``.venv``, ``venv``), environment variables (``VIRTUAL_ENV``), and
   tool-specific detection

Set executable paths
   Once a virtualenv is found, pet sets buffer-local variables so your
   Emacs packages (flycheck, lsp-mode, formatters, etc.) use the correct
   executables from that virtualenv's ``bin`` directory

Remote file support
   For Python files accessed via TRAMP, pet detects remote virtualenvs
   and configures executables with proper state isolation to prevent
   cross-project interference

This happens automatically with **zero Emacs configuration** necessary.
The detection runs once per project and results are cached for
performance.

********************************************
 Supported Python Virtual Environment Tools
********************************************

-  `poetry <https://python-poetry.org>`_, `pipenv
   <https://pipenv.pypa.io>`_

-  `conda <https://docs.conda.io>`_, `mamba
   <https://mamba.readthedocs.io>`_, `micromamba
   <https://mamba.readthedocs.io>`_

-  `uv <https://github.com/astral-sh/uv>`_, `pdm
   <https://pdm-project.org>`_

-  `venv <https://docs.python.org/3/library/venv.html>`_, `virtualenv
   <https://virtualenv.pypa.io>`_

-  `direnv <https://direnv.net>`_, `pixi <https://pixi.sh>`_

- `pre-commit <https://pre-commit.com>`_, `pipx <https://pipx.pypa.io>`_, `pyenv
  <https://github.com/pyenv/pyenv>`_, `docker <https://docs.docker.com/>`_,
  `maturin <https://www.maturin.rs>`_, `virtualenvwrapper
  <https://virtualenvwrapper.readthedocs.io>`_, and more

See `doc/SUPPORTED.rst <doc/SUPPORTED.rst>`_ for the complete list and integration notes.

**************************
 Supported Emacs Packages
**************************

-  `lsp-mode <https://emacs-lsp.github.io/lsp-mode/>`_, `eglot
   <https://github.com/joaotavora/eglot>`_, `flycheck
   <https://www.flycheck.org/en/latest/>`_

-  `lsp-jedi <https://github.com/fredcamps/lsp-jedi>`_, `lsp-pyright
   <https://github.com/emacs-lsp/lsp-pyright>`_

-  `ruff-format <https://melpa.org/#/ruff-format>`_, `python-black
   <https://github.com/wbolster/emacs-python-black>`_, `python-isort
   <https://github.com/wyuenho/emacs-python-isort>`_

-  `python-pytest <https://github.com/wbolster/emacs-python-pytest>`_,
   `pytest-el <https://github.com/ionrock/pytest-el>`_

-  `dape <https://github.com/svaante/dape>`_, `dap-python
   <https://emacs-lsp.github.io/dap-mode/page/configuration/#python>`_

-  Built-in `project.el
   <https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html>`_,
   `projectile <https://docs.projectile.mx/projectile/index.html>`_

-  Built-in `TRAMP
   <https://www.gnu.org/software/emacs/manual/html_node/tramp/>`_ for
   remote file editing

-  `envrc <https://github.com/purcell/envrc>`_, `apheleia
   <https://github.com/radian-software/apheleia>`_, `format-all
   <https://github.com/lassik/emacs-format-all-the-code>`_, and more

See `doc/SUPPORTED.rst <doc/SUPPORTED.rst>`_ for the complete list and integration caveats.

*********************
 System Requirements
*********************

``pet`` has minimal system requirements and requires Emacs 27.1+.

For TOML and YAML configuration file parsing, ``pet`` supports multiple
options:

External Programs (Optional)
============================

-  `dasel <https://github.com/TomWright/dasel>`_ (default for both TOML
   and YAML)
-  `tomljson <https://github.com/pelletier/go-toml#tools>`_ and `yq
   <https://github.com/mikefarah/yq>`_ for Linux users

Emacs Lisp Parsers (Optional)
=============================

-  `tomlparse.el <https://github.com/johannes-mueller/tomlparse.el>`_
   for TOML (requires Emacs 29+)
-  `yaml.el <https://github.com/zkry/yaml.el>`_ for YAML (works with
   Emacs 26.1+)

``pet`` will automatically detect and use the first available parser for
each format, preferring external programs for performance, then falling
back to pure Emacs Lisp implementations.

Additional Requirements
=======================

If you are using Emacs < 29, the ``sqlite3`` command is required for pre-commit
database parsing.

Installation Tips
=================

.. code:: bash

   # Install dasel (cross-platform)
   # macOS
   brew install dasel
   # Linux (various package managers)
   sudo apt install dasel        # Ubuntu/Debian
   sudo dnf install dasel        # Fedora
   sudo pacman -S dasel          # Arch Linux

Alternatively, install tomlparse.el and yaml.el via MELPA::

   M-x package-install RET tomlparse RET

Or::

   M-x package-install RET yaml RET

*******
 Usage
*******

If you are using Emacs on macOS, install `exec-path-from-shell
<https://github.com/purcell/exec-path-from-shell>`_ first to ensure
Python tools are available in your ``exec-path``.

Basic Setup
===========

.. code:: elisp

   (use-package pet
     :config
     (add-hook 'python-base-mode-hook 'pet-mode -10))

This automatically configures all supported packages for both
``python-mode`` and ``python-ts-mode``.

Environment Switching
=====================

For projects using conda, mamba, or pixi, you can now switch environments
interactively::

   M-x pet-conda-switch-environment
   M-x pet-mamba-switch-environment
   M-x pet-pixi-switch-environment

When you enable ``pet-mode`` on a fresh project using these tools,
``pet`` will automatically prompt you to select an environment if none
is currently active.

Manual Configuration
====================

For packages ``pet`` doesn't yet support, or when you need fine-grained control:

.. code:: elisp

   (add-hook 'python-mode-hook
             (lambda ()
               (setq-local python-shell-interpreter (pet-executable-find "python")
                           python-shell-virtualenv-root (pet-virtualenv-root))))

For flycheck setup: ``(add-hook 'python-mode-hook 'pet-flycheck-setup)``

See `doc/SUPPORTED.rst <doc/SUPPORTED.rst>`_ for a complete configuration example with
lsp-mode, flycheck, formatters, and testing tools.

*************
 Performance
*************

``pet`` caches virtualenv detection results and works efficiently on
most projects. For large projects or performance issues, see
`doc/PERFORMANCE.rst <doc/PERFORMANCE.rst>`_ for detailed optimization strategies.

***************
 Customization
***************

For configuration options including file search methods, external tool
settings, parser selection, and project-specific settings, see
`doc/CUSTOMIZATION.rst <doc/CUSTOMIZATION.rst>`_.

***********************
 Troubleshooting & FAQ
***********************

Pet doesn't activate an environment in Eshell/shell/comint
==========================================================

``pet`` is not a terminal emulator or a shell or a shell environment manager
like ``direnv`` or ``asdf``. If you want to activate your Python virtual
environment automatically when you direction in a shell, configure your
shell. For details, please consult the documentation of your shell and these
environment managers.

If you have already configured your environment manager in the shell, you may
want to look at how the shell was launched from Emacs, i.e. whether it was
launched as a login shell or interactively. If you've only configured your
environment manager to activate in non-login interactive sessions or only in
login sessions, you may want to check your shell's startup files or the shell
launch arguments in Emacs.


Pet didn't detect my virtualenv
===============================

``pet`` doesn't create virtualenvs - create your virtualenv and install
dependencies first, then pet will detect it automatically.

Wrong Python version or missing executables
===========================================

Enable debug mode with ``(setq pet-debug t)`` and watch the
``*Messages*`` buffer. Use ``M-x pet-verify-setup`` in your Python
buffers to see what was detected.

**Verify setup for specific packages:**

-  ``lsp-mode``: ``M-x lsp-describe-session``
-  ``eglot``: ``M-x eglot-show-workspace-configuration``
-  ``flycheck``: ``M-x flycheck-verify-setup``

Slow performance on large projects
==================================

Install ``fd`` for faster file searches: ``brew install fd`` (macOS) or
``sudo apt install fd-find`` (Ubuntu). See `doc/PERFORMANCE.rst <doc/PERFORMANCE.rst>`_ for
optimization strategies.

Direnv integration issues
=========================

Use `envrc <https://github.com/purcell/envrc>`_ instead of other direnv
packages. See `doc/SUPPORTED.rst <doc/SUPPORTED.rst>`_ for detailed integration notes.

Why doesn't ``pet`` use buffer-local exec-path?
===============================================

Many Python projects use development tools in different virtualenvs
(e.g., pre-commit hooks). Managing multiple virtualenv paths in
``exec-path`` reliably is complex. Using absolute executable paths is
simpler and more performant.

Do I still need other virtualenv packages?
==========================================

No, you can uninstall them all - this is the main purpose of pet.

*********
 License
*********

`GPLv3 <./LICENSE>`_
