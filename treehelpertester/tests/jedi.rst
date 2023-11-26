###################################################################
Jedi - an awesome autocompletion/static analysis library for Python
###################################################################

.. image:: https://secure.travis-ci.org/davidhalter/jedi.png?branch=master

Installation
============

    pip install jedi

Feature Support and Caveats
===========================

Jedi really understands your Python code. For a comprehensive list what Jedi

API
---

You can find the documentation for the `API here <https://jedi.readthedocs.org/en/latest/docs/plugin-api.html>`_.

Autocompletion / Goto / Pydoc
-----------------------------

Please check the API for a good explanation. There are the following commands:

Autocompletion in your REPL (IPython, etc.)
-------------------------------------------

<https://jedi.readthedocs.org/en/latest/docs/usage.html#tab-completion-in-the-python-shell>`_.

Static Analysis / Linter
------------------------

To do all forms of static analysis, please try to use ``jedi.names``. It will

Refactoring
-----------

Jedi's parser would support refactoring, but there's no API to use it right

Development
===========

There's a pretty good and extensive `development documentation

Testing
=======

The test suite depends on ``tox`` and ``pytest``

Acknowledgements
================

- Takafumi Arakaki (@tkf) for creating a solid test environment and a lot of
  other things.
