Performance Optimization Guide
==============================

This document provides detailed guidance on optimizing pet's performance for different
project sizes and configurations.

How Pet Works And How It Impacts Performance
--------------------------------------------

``pet`` performs most of the work when opening the first Python file in a
project.

If your Python project uses any supported virtual environment tools, and that
you have installed these virtual environments, it should only take a couple of
seconds for ``pet`` to set up all your Python-related Emacs packages when you
open the first Python file of the project in Emacs.

What ``pet`` does is it searches your project directory for config files in
order to detect what virtual environment tools the project is using. It then
searches your project's virtual environments to discover the absolute paths of
these executables. Once this is done, the discovered paths will be cached quite
aggressively for the entire project, so minimal repeated work is done to
discover these executables when you open a separate Python file in the same
project.

Understanding this behavior helps optimize performance:

Caching Behavior
~~~~~~~~~~~~~~~~

``pet`` uses a unified cache system to optimize performance across multiple areas:

- Virtualenv paths are cached by project roots and persist until Emacs restarts or when
  the last project buffer is killed
- File discovery results are cached to avoid repeated filesystem searches for
  configuration files (``pyproject.toml``, ``environment.yml``, etc.)
- File watchers track configuration files to keep cached content up to date
- First open: Full detection runs, subsequent opens: cached results used
- Cache cleanup: Automatic cleanup when switching projects or killing buffers

File Search Strategy
~~~~~~~~~~~~~~~~~~~~

``pet`` searches for configuration files in this order (configurable via
``pet-find-file-functions``):

1. Project root check - Instant for files at project root
2. Directory walking - Fast, walks up from ``default-directory``
3. Native ``fd`` search - Fast even for large projects if ``fd`` is installed
4. Recursive search - Can be slow on large projects

When Performance Issues Occur
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Large projects (Linux kernel scale) may take many seconds during recursive search
- Projects with deep directory nesting and/or config files in subdirectories of the
  project root
- Network filesystems or slow storage

Optimization Strategies
-----------------------

Optimize File Search Order
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: elisp

    ;; Skip slow recursive search for large projects
    (setq pet-find-file-functions '(pet-find-file-from-project-root
                                    pet-locate-dominating-file
                                    pet-find-file-from-project-root-natively))

    ;; Or use only the fastest methods
    (setq pet-find-file-functions '(pet-find-file-from-project-root
                                    pet-locate-dominating-file))

    ;; Or provide your own project-specific file search function
    (setq pet-find-file-functions '(pet-find-file-from-project-root
                                    pet-locate-dominating-file
                                    my-superfast-find-file))

Install Fd for Faster Searches
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

    # Install fd for much faster file searches in large projects
    # Pet automatically detects and uses fd if available
    brew install fd        # macOS
    sudo apt install fd-find  # Ubuntu/Debian

Project-specific Performance Tuning
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: elisp

    ;; In .dir-locals.el for projects with performance issues:
    ((python-mode . ((pet-find-file-functions . (pet-find-file-from-project-root
                                                 pet-locate-dominating-file)))))

Benchmarking and Profiling
--------------------------

.. code-block:: elisp

    ;; Time pet-mode activation
    (benchmark-run 1 (pet-mode))

Profile pet-mode:

::

    M-x eval-expression RET (progn (profiler-start 'cpu) (pet-mode) (profiler-stop) (profiler-report)) RET

Performance Configuration Options
---------------------------------

File Search Functions
~~~~~~~~~~~~~~~~~~~~~

.. code-block:: elisp

    ;; Control the order and methods used to search for configuration files
    (setq pet-find-file-functions '(pet-find-file-from-project-root
                                    pet-locate-dominating-file
                                    pet-find-file-from-project-root-natively
                                    pet-find-file-from-project-root-recursively))

External Tool Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: elisp

    ;; fd command configuration for fast file searches
    (setq pet-fd-command "fd")
    (setq pet-fd-command-args '("-tf" "-cnever" "-H" "-a" "-g"))

    ;; TOML to JSON converter (default: "dasel")
    (setq pet-toml-to-json-program "tomljson")  ; or "dasel"
    (setq pet-toml-to-json-program-arguments '("-"))

    ;; YAML to JSON converter (default: "dasel")
    (setq pet-yaml-to-json-program "yq")  ; or "dasel"
    (setq pet-yaml-to-json-program-arguments '("-o=json"))

Parser Selection
~~~~~~~~~~~~~~~~

.. code-block:: elisp

    ;; Prefer Emacs Lisp parsers over external programs
    ;; When t, Pet will use tomlparse.el and yaml.el first,
    ;; falling back to external programs only if needed
    (setq pet-prefer-elisp-parsers t)
