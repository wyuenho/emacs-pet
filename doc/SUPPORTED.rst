##############################
 Supported Tools and Packages
##############################

This document provides comprehensive lists of all Python virtual
environment tools and Emacs packages that pet supports.

********************************************
 Supported Python Virtual Environment Tools
********************************************

-  `pre-commit <https://pre-commit.com>`_

-  `poetry <https://python-poetry.org>`_

-  `pipenv <https://pipenv.pypa.io>`_

-  `direnv <https://direnv.net>`_

-  `venv <https://docs.python.org/3/library/venv.html>`_, `virtualenv
   <https://virtualenv.pypa.io>`_ or `virtualenvwrapper
   <https://virtualenvwrapper.readthedocs.io>`_ (`virtualenvwrapper
   caveats`_)

-  `maturin <https://www.maturin.rs>`_

-  `uv <https://github.com/astral-sh/uv>`_

-  `pdm <https://pdm-project.org>`_

-  `pipx <https://pipx.pypa.io>`_

-  `pyenv <https://github.com/pyenv/pyenv>`_ (very poorly maintained,
   don't use it unless you are using Homebrew on macOS)

-  `docker <https://hub.docker.com/_/python>`_

-  `pixi <https://pixi.sh>`_

-  `conda <https://docs.conda.io>`_ (no support for conda-project yet)

-  `mamba
   <https://mamba.readthedocs.io/en/latest/installation/mamba-installation.html>`_

-  `micromamba
   <https://mamba.readthedocs.io/en/latest/installation/micromamba-installation.html>`_

-  Whatever is on your ``VIRTUAL_ENV`` environment variable

-  Even when you aren't in a virtual environment

**************************
 Supported Emacs Packages
**************************

Project Management
==================

-  Built-in `project.el
   <https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html>`_
-  `projectile <https://docs.projectile.mx/projectile/index.html>`_

Environment Management
======================

-  `envrc <https://github.com/purcell/envrc>`_ (`direnv caveats`_)
-  `auto-virtualenvwrapper
   <https://github.com/robert-zaremba/auto-virtualenvwrapper.el/>`_

Language Servers and Debugging
==============================

-  `lsp-mode <https://emacs-lsp.github.io/lsp-mode/>`_
-  `eglot <https://github.com/joaotavora/eglot>`_
-  `dape <https://github.com/svaante/dape>`_
-  `lsp-jedi <https://github.com/fredcamps/lsp-jedi>`_
-  `lsp-pyright <https://github.com/emacs-lsp/lsp-pyright>`_
-  `dap-python
   <https://emacs-lsp.github.io/dap-mode/page/configuration/#python>`_

Linting and Syntax Checking
===========================

-  `flycheck <https://www.flycheck.org/en/latest/>`_

Code Formatting
===============

-  `blacken <https://github.com/pythonic-emacs/blacken>`_
-  `yapfify <https://github.com/JorisE/yapfify>`_
-  `python-black <https://github.com/wbolster/emacs-python-black>`_
-  `python-isort <https://github.com/wyuenho/emacs-python-isort>`_
-  `ruff-format <https://melpa.org/#/ruff-format>`_
-  `py-autopep8 <https://github.com/emacsmirror/py-autopep8>`_
-  `format-all <https://github.com/lassik/emacs-format-all-the-code>`_
-  `apheleia <https://github.com/radian-software/apheleia>`_

Testing
=======

-  `python-pytest <https://github.com/wbolster/emacs-python-pytest>`_
-  `pytest-el <https://github.com/ionrock/pytest-el>`_

***************************************
 Integration Caveats and Special Notes
***************************************

.. _direnv caveats:

How do I get ``pet`` to pick up the virtualenv or PATH created by ``direnv``?
=============================================================================

Short answer:

Use `envrc <https://github.com/purcell/envrc>`_.

.. code:: elisp

   (require 'envrc)
   (add-hook 'change-major-mode-after-body-hook 'envrc-mode)

Longer answer:

There are a number of packages similar to ``envrc`` such as ``direnv``
and ``buffer-env`` that claim to be able to configure ``direnv`` in
Emacs. However, they all suffer from various problems such as changing
the environment and ``exec-path`` for the entire Emacs process, unable
to activate early enough or being too general to support direnv tightly.

Because ``pet`` needs to configure the buffer local variables **before**
the rest of the minor modes are activated, but **after** ``exec-path``
has been set up by direnv, one must take care of choosing a minor mode
package that allows the user to customize when it takes effect. This
requirement rules out ``direnv.el`` [1]_.

.. [1]

   Earlier versions of ``pet`` suggested ``direnv.el`` as a solution, it is
   no longer recommended due to this reason.

.. _virtualenvwrapper caveats:

My project uses ``virtualenvwrapper``, how do I get ``pet`` to pick up the virtualenv?
======================================================================================

You can use ``envrc`` + `this direnv configuration
<https://github.com/direnv/direnv/wiki/Python#virtualenvwrapper>`_ to
activate your virtualenv or `auto-virtualenvwrapper
<https://github.com/robert-zaremba/auto-virtualenvwrapper.el/>`_. Note
that in any case, your virtualenv must be activated before turning on
``pet-mode`` in order to make the environment variable ``VIRTUAL_ENV``
available to it. For example:

.. code:: elisp

   (require 'auto-virtualenvwrapper)
   (require 'pet)

   (add-hook 'python-base-mode-hook
     (lambda ()
       (auto-virtualenvwrapper-activate)
       (pet-mode))
     -10)
   (add-hook 'window-configuration-change-hook #'auto-virtualenvwrapper-activate)
   (add-hook 'focus-in-hook #'auto-virtualenvwrapper-activate)

################################
 Advanced Configuration Example
################################

For users who want fine-grained control over specific packages:

.. code:: elisp

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
     ;; Optional: ensure external tools are installed
     ;; :ensure-system-package ((dasel . "dasel")
     ;;                         (sqlite3 . "sqlite3"))
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

.. note::

   Most of this configuration is handled automatically by ``pet-mode``.
   This example is useful for understanding how pet works internally or
   when you need to customize specific package integrations.
