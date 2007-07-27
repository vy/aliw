;;; Copyright (c) 2007, Volkan YAZICI <yazicivo@ttnet.net.tr>
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;; - Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;; - Redistributions in binary form must reproduce the above
;;;   copyright notice, this list of conditions and the following
;;;   disclaimer in the documentation and/or other materials provided
;;;   with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

(in-package :aliw)

(defvar *server* nil
  "HTTP server instance. \(See START/STOP-SERVER.)")

(defvar *wiki-title* "A Lisp in Wonderland!"
  "Title of this wiki.")

(defvar *wiki-slogan* "Did you see the hole of that new rabbit?"
  "Slogan that will be used under the wiki title.")

(defvar *entrance-path* "Main"
  "Content page that will welcome newcomers.")

(defvar *static-menu-paths*
  '("Main" "RecentChanges" "Documents" "Downloads" "Contribution")
  "Static pages which will appear in the menu.")

(defvar *manually-handled-paths*
  '(:page-content-not-found   "ContentNotFound"
    :page-operation-not-found "OperationNotFound"
    :page-restricted-access   "RestrictedAccess"
    :page-recent-changes      "RecentChanges"
    :page-search              "Search"
    :account-create           "account:Create"
    :account-sign-in          "account:SignIn"
    :account-sign-out         "account:SignOut"
    :account-preferences      "account:Preferences")
  "List of manually handled wiki paths.")

(defvar *max-uri-depth* 8
  "Maximum number of allowed path depth during chunking of request URI. \(See
WIKI-PATH-FROM of :URI type.)")

(defvar *max-page-history* 5
  "Maximum number of will be stored visited pages history of the client.")

(defvar *max-recent-changes-history* 100
  "Maximum number of will be stored history of recent changes.")

(defvar *recently-changed-contents* nil
  "List of (WIKI-PATH REVISION) pairs of recently changed contents.")

(defvar *recently-changed-contents-lock*
  (hunchentoot-mp:make-lock "Recently Changed Contents List Lock")
  "Lock for serializing access to *RECENTLY-CHANGED-CONTENTS*.")

(defvar *path-types* '(:account :page)
  "Available WIKI-PATH types.")

(defvar *content-path-types* '(:page)
  "Available WIKI-PATH types for contents.")

(defvar *filename-escapes* '(:escape-char #\,
                             :separator-char #\+
                             :type-separator-char #\#)
  "Separator characters that will be used during filename encoding/decoding.")

;; This is the default for the test keyword argument to ESCAPE-STRING
;; and ESCAPE-CHAR of CL-WHO.
(setq *escape-char-p* #'(lambda (char) (find char "<>&'\"")))

(defvar *content-directory*
  (pathname-as-directory "/home/vy/projects/aliw/contents/")
  "Directory where page contents will get stored.")

(defvar *content-index-directory*
  (pathname-as-directory "/home/vy/projects/aliw/index/")
  "Directory where FTS indexes will get stored.")

(defvar *static-files-directory*
  (pathname-as-directory "/home/vy/projects/aliw/static/")
  "Directory where statics files are located. \(Images, CSS files, etc.)")

(defvar *accounts-directory*
  (pathname-as-directory "/home/vy/projects/aliw/accounts/")
  "Directory where account files will get stored.")

(defvar *content-locks-table* (make-hash-table :test #'equal)
  "Table of locks which will be used on access to wiki content.")

(defvar *content-locks-table-lock*
  (hunchentoot-mp:make-lock "Content Locks Table Lock")
  "Lock for serializing concurrent access to *CONTENT-LOCKS-TABLE*.")

(defvar *account-locks-table* (make-hash-table :test #'equal)
  "Table of locks which will be used on access to account information.")

(defvar *account-locks-table-lock*
  (hunchentoot-mp:make-lock "Account Locks Table Lock")
  "Lock for serializing concurrent access to *ACCOUNT-LOCKS-TABLE*.")

(defvar *content-index* nil
  "Instance holding indexed contents. \(Required for Montezuma.)")

(defconstant +buffer-size+ 8192
  "Will be used buffer size while raw reading/writing from/to streams.")

;;; We need to define some constants using DEFPARAMETER/DEFVAR.
;;; Because ANSI says that doing DEFCONSTANT of the same symbol more
;;; than once is undefined unless the new value is EQL to the old
;;; value. And we have some variable values which doesn't return T on
;;; EQL comparison.

(defvar +form-field-bounds+
  '(:username (3 . 64)
    :password (6 . 64)
    :email (nil . 128)
    :uri (nil . 256)
    :search (3 . 512))
  "Property list of (MIN . MAX) bounds of the HTML form fields.")