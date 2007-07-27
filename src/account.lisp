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

(defclass wiki-account ()
  ((username
    :initarg :username
    :accessor wiki-account-username)
   (password
    :initarg :password
    :accessor wiki-account-password
    :documentation "Hashed account password.")
   (email
    :initarg :email
    :accessor wiki-account-email))
  (:documentation "WIKI-ACCOUNT is a generic storage unit to store information
of a wiki account. Besides various usages accross different parts of the pages,
all kind of authentication related process will be done over WIKI-ACCOUNT
too."))

(defmethod obj-equalp ((x wiki-account) (y wiki-account) &rest args)
  "Check equality of supplied WIKI-ACCOUNT classes."
  (declare (ignore args))
  (and (string= (wiki-account-username x) (wiki-account-username y))
       (string= (wiki-account-password x) (wiki-account-password y))
       (string= (wiki-account-email x) (wiki-account-email y))))

(defun wiki-account-pathname (username)
  "Returns path of the related account information file."
  (pathname-as-file
   (merge-pathnames (encode-to-filename username) *accounts-directory*)))

;; We'll issue locking with respect to username, not a WIKI-ACCOUNT
;; class. Because, WITH-WIKI-ACCOUNT-LOCK will be used during
;; WIKI-ACCOUNT creation. Yet another chicken-egg problem.
(generate-scope-serializer-macro with-wiki-account-lock
  ((username (hunchentoot:session-value 'username))
   (wiki-account-pathname)
   (*account-locks-table* *account-locks-table-lock*))
  "Lock specified account for access.")


;;; WIKI-ACCOUNT creation methods.

(defgeneric wiki-account-from (from input)
  (:documentation "Generic WIKI-ACCOUNT creation methods."))

(defmethod wiki-account-from ((from (eql :username)) username)
  "Retrieve WIKI-ACCOUNT information of specified username."
  (with-wiki-account-lock (username)
    (with-open-file (in (wiki-account-pathname username) :if-does-not-exist nil)
      (if in
          (destructuring-bind (&key password email)
              (with-standard-io-syntax (read in))
            (make-instance 'wiki-account
                           :username username
                           :password password
                           :email email))))))

(defmethod wiki-account-from ((from (eql :plist)) plist)
  "Create a new WIKI-ACCOUNT using arguments in supplied property list."
  ;; Check integrity of plist first.
  (assert (member :username plist))
  (assert (member :password plist))
  (make-instance 'wiki-account
                 :username (getf plist :username)
                 :password (hash (getf plist :password))
                 :email (getf plist :email)))

(define-per-request-var wiki-account-signed-in-p ()
  "Checks whether current client is authenticated or not."
  ;; At the moment, we just check the equality of client (stored in
  ;; cookies) and server (stored in session variables) side AUTH-SIGN
  ;; values.
  (let ((client-sign (hunchentoot:cookie-in "auth-sign")))
    (if client-sign
        (multiple-value-bind (session-sign exists-p)
            (hunchentoot:session-value 'auth-sign)
          (and exists-p (string= session-sign client-sign))))))

(define-per-request-var current-wiki-account ()
  "Returns WIKI-ACCOUNT of current user."
  (if (wiki-account-signed-in-p)
      (wiki-account-from :username (hunchentoot:session-value 'username))))


;;; Account authenatication related routines.

(defun wiki-account-sign-in (username password)
  "Try to authenticate specified user using supplied password."
  (with-wiki-account-lock (username)
    (with-open-file (in (wiki-account-pathname username) :if-does-not-exist nil)
      (if (and in
               (string= (hash password)
                        (getf (with-standard-io-syntax (read in)) :password)))
          (let ((sign (hash (list username password))))
            (hunchentoot:set-cookie "auth-sign" :value sign :path "/")
            (setf (hunchentoot:session-value 'auth-sign) sign
                  (hunchentoot:session-value 'username) username)
            t)))))

(defun wiki-account-sign-out ()
  "Remove any previously set authentication data."
  (when (wiki-account-signed-in-p)
    ;; It's enough to remove just one of the AUTH-SIGN values.
    (hunchentoot:set-cookie "auth-sign" :value "0" :expires "0")
    (hunchentoot:delete-session-value 'auth-sign)
    t))

(defun wiki-account-check-password (account password)
  "Checks whether supplied password validates."
  (string= (wiki-account-password account) (hash password)))

(defun wiki-account-create (account)
  "Create related on-disk files of the supplied WIKI-ACCOUNT."
  (let ((username (wiki-account-username account)))
    (with-wiki-account-lock (username)
      (with-wiki-account-lock (username)
        (with-open-file (in (wiki-account-pathname username)
                            :direction :output
                            :if-does-not-exist :create)
          (with-standard-io-syntax
            (print
             (list :password (wiki-account-password account)
                   :email (wiki-account-email account))
             in)))))))

(defun wiki-account-update (from to)
  "Replace account preferences with supplied values."
  ;; Both WIKI-ACCOUNT must point to the same user.
  (assert (string= (wiki-account-username from)
                   (wiki-account-username to)))
  ;; If two objects are identical, no need to bother.
  (if (obj-equalp from to)
      t
      (let ((username (wiki-account-username from)))
        (with-wiki-account-lock (username)
          (with-open-file (in (wiki-account-pathname username)
                              :direction :output
                              :if-exists :supersede)
            (with-standard-io-syntax
              (print
               (list :password (wiki-account-password to)
                     :email (wiki-account-email to))
               in)))))))

(defun wiki-account-exists-p (account)
  "Checks whether supplied WIKI-ACCOUNT exists."
  ;; Don't need to bother with locking for such a small operation.
  (file-exists-p (wiki-account-pathname (wiki-account-username account))))