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

(in-package :cl-user)

(defpackage :aliw-asd
  (:use :cl :asdf))

(in-package :aliw-asd)

(defconstant +aliw-version+ "0.1.2")

(export '+aliw-version+)

(asdf:defsystem :aliw
  :version +aliw-version+
  :depends-on (:cl-difflib
               :cl-fad
               :cl-ppcre
               :cl-who
               :flexi-streams
               :hunchentoot
               :md5
               :meta-sexp
               :montezuma)
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "specials")
                             (:file "util")
                             (:file "path")
                             (:file "account")
                             (:file "content")
                             (:file "markup")
                             (:file "html")
                             (:file "account-handler")
                             (:file "page-handler")
                             (:file "wiki")))))