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

(defpackage :aliw
  (:documentation "Yet another wiki based on hunchentoot web server.")
  (:use :cl
        :cl-fad
        :cl-who
        :meta-sexp)
  (:export :start-server
           :stop-server
           ;; HTTP server variables.
           :*server*
           :*hunchentoot-default-external-format*
           :*default-content-type*
           :*handle-http-errors-p*
           :*show-lisp-errors-p*
           :*show-lisp-backtraces-p*
           :*use-user-agent-for-sessions*
           :*use-remote-addr-for-sessions*
           :*dispatch-table*
           ;; Wiki variables.
           :*wiki-title* 
           :*wiki-slogan*
           :*entrance-path*
           :*static-menu-paths*
           :*manually-handled-paths*
           :*max-page-history*
           :*max-recent-changes-history*
           :*recent-changes-page*
           :*recently-changed-contents*
           :*recently-changed-contents-lock*
           :*max-uri-depth*
           :*path-types*
           :*content-path-types*
           :*filename-escapes*
           :*content-directory*
           :*content-index-directory*
           :*static-files-directory*
           :*accounts-directory*
           :*markup-dtd-file*
           :*content-locks-table*
           :*content-locks-table-lock*
           :*account-locks-table*
           :*account-locks-table-lock*
           :*content-transformation-rules*
           :*content-index*
           :*rss-properties*
           :+buffer-size+
           :+namespace-uri+
           :+form-field-bounds+))