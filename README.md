        __--__-_
    ,-^'        ^%\
  .'      /|      #&    __-.--,_
         / |       ~?:^'        `:. ...
        /  |
       /_-=|^  L I S P
     -/'  '|   I N
    (/     |&  W O N D E R L A N D
                          ,..
            __,,-    ,-~^'   `
  ..-'^'-'^^     `'^'

# Overview

A Lisp in Wonderland (aka. ALIW) is a wiki software written in Common Lisp, to ease the collaboration on editable web pages.

Besides its features you will experience during your visit,

- Extensible markup language (via meta-sexp),
- Content caching to reduce markup transformation overhead,
- Open to further development via flexible interface of Hunchentoot web server,
- Plain file based - thus platform independent - version control system,
- Advanced editing, diff'ing (via cl-difflib), history viewing capabilities,
- Modular user interface structure through CSS,
- Full-text search availability (via Montezuma)

are some of the relatively major features supported by ALIW.

# Installation

Before proceding to the next steps, you first need to ASDF-install ALIW tarball.

  CL-USER> (asdf-install:install :aliw)

or use the full path of the tarball.

  CL-USER> (asdf-install:install "/path/to/aliw.tar.gz")

After installing ALIW tarball successfully, you need to configure some of runtime parameters that act specific to each installation. For this purpose, you need to edit `specials.lisp' file coming with the ALIW tarball.

Besides optional ones, the parameters that you _must_ configure are

  *CONTENT-DIRECTORY*
  *CONTENT-INDEX-DIRECTORY*
  *STATIC-FILES-DIRECTORY*
  *ACCOUNTS-DIRECTORY*

After that, you can edit `specials.lisp` as you wish through your concerns. (All of the parameters in the file are documented.)

# Starting/Stopping Server

To start/stop the web server that wiki will run on, you can use `START-SERVER`/`STOP-SERVER` functions coming with `ALIW` package. `START-SERVER` takes identical parameters with [`HUNCHENTOOT:START-SERVER`](http://weitz.de/hunchentoot/#servers).

Pay attention that, you must use `ALIW:START-SERVER` and `ALIW:STOP-SERVER` to start/stop the wiki server, not `HUNCHENTOOT:START-SERVER`. Otherwise, required initialization steps will be totally skipped.

`START-SERVER` parameters:

  (&key (port 80 port-provided-p)
        address
        dispatch-table
        (name (gensym))
        (mod-lisp-p nil)
        (use-apache-log-p mod-lisp-p)
        (input-chunking-p t)
        (read-timeout *default-read-timeout*)
        (write-timeout *default-write-timeout*)
        #+(and :unix (not :win32)) setuid
        #+(and :unix (not :win32)) setgid
        #-:hunchentoot-no-ssl ssl-certificate-file
        #-:hunchentoot-no-ssl (ssl-privatekey-file ssl-certificate-file)
        #-:hunchentoot-no-ssl ssl-privatekey-password)

To stop the server, just call `STOP-SERVER`. (`STOP-SERVER` takes no arguments.)
