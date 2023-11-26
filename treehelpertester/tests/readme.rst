****************************************
HTTPie: a CLI, cURL-like tool for humans
****************************************

HTTPie (pronounced *aitch-tee-tee-pie*) is a **command line HTTP client**.

=============
Main Features
=============

* Expressive and intuitive syntax

============
Installation
============

On **Mac OS X**, HTTPie can be installed via `Homebrew <http://brew.sh/>`_

-------------------
Development version
-------------------

The **latest development version** can be installed directly from GitHub:

=====
Usage
=====

Hello World:

--------
Examples
--------

Custom `HTTP method`_, `HTTP headers`_ and `JSON`_ data:

--------

===========
HTTP Method
===========

The name of the HTTP method comes right before the URL argument:

===========
Request URL
===========

The only information HTTPie needs to perform a request is a URL.

=============
Request Items
=============

There are a few different *request item* types that provide a

====
JSON
====

JSON is the *lingua franca* of modern web services and it is also the

=====
Forms
=====

Submitting forms is very similar to sending `JSON`_ requests. Often the only

-------------
Regular Forms
-------------

.. code-block:: bash

-----------------
File Upload Forms
-----------------

If one or more file fields is present, the serialization and content type is

============
HTTP Headers
============

To set custom headers you can use the ``Header:Value`` notation:

==============
Authentication
==============

The currently supported authentication schemes are Basic and Digest

------------
Auth Plugins
------------

* `httpie-oauth <https://github.com/jkbrzt/httpie-oauth>`_: OAuth

==============
HTTP Redirects
==============

By default, HTTP redirects are not followed and only the first

=======
Proxies
=======

You can specify proxies to be used through the ``--proxy`` argument for each

=====
HTTPS
=====

-----------------------------------
Server SSL certificate verification
-----------------------------------

``REQUESTS_CA_BUNDLE`` (picked up by the underlying python-requests library):

---------------------------
Client side SSL certificate
---------------------------
To use a **client side certificate** for the SSL communication, you can pass

-----------
SSL version
-----------

Use the ``--ssl=<PROTOCOL>`` to specify the desired protocol version to use.

----------------------------
SNI (Server Name Indication)
----------------------------

If you use HTTPie with Python < 2.7.9

==============
Output Options
==============

By default, HTTPie only outputs the final response and the whole response

---------------------------------------
Viewing Intermediary Requests/Responses
---------------------------------------

To see *all* the HTTP communication, i.e. the final request/response as

-------------------------
Conditional Body Download
-------------------------

As an optimization, the response body is downloaded from the server
