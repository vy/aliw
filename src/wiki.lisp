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


;;; Configure web server properties.

(setq
 ;; *H-D-E-F* is quite important when comparing data stored on disk
 ;; and submited by user as POST data. If you're having troubles in
 ;; diff'ing, configure below :EOL-STYLE value appropriately.
 hunchentoot:*hunchentoot-default-external-format*
 (flex:make-external-format :utf8 :eol-style :crlf)
 hunchentoot:*default-content-type* "text/html; charset=utf-8"

 hunchentoot:*handle-http-errors-p* t
 hunchentoot:*show-lisp-errors-p* t
 hunchentoot:*show-lisp-backtraces-p* t
 
 hunchentoot:*use-user-agent-for-sessions* t
 hunchentoot:*use-remote-addr-for-sessions* t)


;;; Configure web server dispatch table.

(defun redirect-to-entrance-handler ()
  "Shortcut handler to redirect client to *ENTRANCE-PAGE*."
  (hunchentoot:redirect (wiki-path-to :uri (wiki-path-from :label *entrance-path*))))

(setq
 hunchentoot:*dispatch-table*
 (list
  ;; Different type of WIKI-PATH handlers. (See *PATH-TYPES*.)
  (hunchentoot:create-regex-dispatcher
   (path-type-to-prefix-regex :account)
   'account-path-handler)
  (hunchentoot:create-regex-dispatcher
   (path-type-to-prefix-regex :page)
   'page-path-handler)
  ;; Custom folder dispatcher for *STATIC-FILES-DIRECTORY*
  (hunchentoot:create-folder-dispatcher-and-handler
   "/static/" *static-files-directory*)
  ;; As final rescue, redirect client to entrance. 
  (hunchentoot:create-prefix-dispatcher "/" 'redirect-to-entrance-handler)))


;;; Server start/stop routines.

(defun start-server (&rest hunchentoot-start-server-keys)
  "Stand up HTTP server."
  ;; Load stored recently-changed-contents.
  (with-recently-changed-contents-lock
    (setq *recently-changed-contents*
          (load-recently-changed-contents)))
  ;; Initialize content index.
  (setq *content-index*
        (make-instance 'montezuma:index
                       :path *content-index-directory*))
  ;; Fire HTTP server up.
  (setq *server* (apply #'hunchentoot:start-server
                        hunchentoot-start-server-keys)))

(defun stop-server (&optional (server *server*))
  "Stop HTTP server."
  (setq *server* (hunchentoot:stop-server server)))