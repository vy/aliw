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

(defclass wiki-path ()
  ((type
    :initarg :type
    :accessor wiki-path-type
    :documentation "Path type. \(See available *PATH-TYPES*.)")
   (list
    :initarg :list
    :accessor wiki-path-list
    :documentation "List representation of the URI path.")
   (parameters
    :initarg :parameters
    :initform nil
    :accessor wiki-path-parameters
    :documentation "GET parameters, if there is any."))
  (:documentation
   "WIKI-PATH, is a common storage unit to store any kind of path related
information about the wiki pages. You can transform handled wiki page URIs,
type-list indicators, wiki page labels, filenames into/from WIKI-PATH
instances. \(See WIKI-PATH-FROM and WIKI-PATH-TO generic functions for further
information).

We will not process any GET parameters while creating a WIKI-PATH instance.
Because that work is already done by the web server. Parameters will be used
while creating a new URI from an existing WIKI-PATH with freshly introduced GET
parameters supplied by the programmer. (Namely, a convenient way to add GET
parameters to an URI, specifically to a WIKI-PATH URI.) By the way, WIKI-PATH
parameters are case-insensitive."))

(defmethod obj-equalp ((x wiki-path) (y wiki-path) &key compare-parameters)
  "Checks equality of given WIKI-PATH objects. \(Path parameters are not
compared by default.)"
  (with-slots (type list parameters) x
    (and (eql type (wiki-path-type y))
           (equalp list (wiki-path-list y))
           (if compare-parameters
               (equalp parameters (wiki-path-parameters y))
               t))))

(defmethod obj-duplicate ((path wiki-path) &key duplicate-parameters)
  "Duplicates a WIKI-PATH object. \(Path parameters aren't duplicated by
default.)"
  (with-slots (type list parameters) path
    (make-instance 'wiki-path
                   :type type
                   :list (copy-list list)
                   :parameters (if duplicate-parameters
                                   (copy-list parameters)))))


;;; Path list preparation routines.

(defgeneric normalize-path (path)
  (:documentation "Perform necessary \(character) conversions just before using
supplied path in a WIKI-PATH object."))

(defmethod normalize-path ((path string))
  "Normalize path of string type."
  (cl-ppcre:regex-replace-all " " path "_"))

(defmethod normalize-path ((path list))
  "Normalize list of paths."
  (mapcar #'normalize-path path))


;;; Filename encoding/decoding routines for WIKI-PATH class.

(defmethod encode-to-filename
    ((input wiki-path) &rest named-pairs
     &key (type-separator-char (getf *filename-escapes* :type-separator-char))
     &allow-other-keys)
  "Encodes supplied WIKI-PATH into its filename-safe string representation."
    (with-output-to-string (out)
        ;; Write WIKI-PATH type.
        (write-string
         (apply #'encode-to-filename
                (string-downcase (symbol-name (wiki-path-type input)))
                :allow-other-keys t
                named-pairs)
         out)
        (write-char type-separator-char out)
        ;; Traverse WIKI-PATH list.
        (loop for paths on (wiki-path-list input)
              do (write-string
                  (apply #'encode-to-filename
                         (first paths)
                         :allow-other-keys t
                         named-pairs)
                  out)
              unless (null (rest paths))
              do (write-char type-separator-char out))))

(defmethod decode-from-filename
    ((type wiki-path) (filename string) &rest named-pairs
     &key (type-separator-char (getf *filename-escapes* :type-separator-char))
     &allow-other-keys)
  "Decodes supplied filepath into its original WIKI-PATH class form."
  (loop with type-separator = (make-array 1 :initial-element type-separator-char)
        with filename-len = (length filename)
        for avail-type in *path-types*
        for avail-type-str = (string-downcase (symbol-name avail-type))
        for avail-type-prefix = (string-append
                                  avail-type-str
                                  type-separator)
        for len = (length avail-type-prefix)
        when (and (< len filename-len)
                  (string= avail-type-prefix (subseq filename 0 len)))
        do (return
             (make-instance
              'wiki-path
              :type avail-type
              :list (normalize-path
                     (apply #'decode-from-filename
                            :list (subseq filename len)
                            :allow-other-keys t
                            named-pairs))))
        finally (error "Unknown path type! (~a)" filename)))


;;; WIKI-PATH from/to conversion routines.

(defgeneric wiki-path-from (from args)
  (:documentation "Creates a fresh wiki-path instance from supplied input."))

(defgeneric wiki-path-to (to path)
  (:documentation "Transforms supplied wiki-path instance into specified
  form."))

(defmethod wiki-path-from ((from (eql :uri)) (uri string))
  "Returns a WIKI-PATH instance from the supplied URI. (GET parameters are
ignored.)"
  (loop with uri = (subseq uri 0 (or (position #\? uri) (length uri)))
        with paths = (mapcar #'hunchentoot:url-decode
                             ;; Remove empty paths.
                             (remove-if-not
                              #'(lambda (path)
                                  (< 0 (length path)))
                              (cl-ppcre:split "/" uri :limit *max-uri-depth*)))
        with list = (rest paths)
        with type = (string-upcase (first paths))
        for avail-type in *path-types*
        when (string= type (symbol-name avail-type))
        do (return (make-instance 'wiki-path
                                  :type avail-type
                                  :list (normalize-path list)))
        finally (error "Unknown path type!")))

(defun stringify-wiki-path-parameters (parameters)
  "Transforms supplied WIKI-PATH parameters into string form suitable to place
in a URI. \(Parameter processing is case-insensitive.)"
  (loop for (key value) on parameters by #'cddr
        nconc (list (string-downcase (symbol-name key))
                    (if (stringp value)
                        value
                        (format nil "~a" value)))))

(defmethod wiki-path-to ((to (eql :uri)) (path wiki-path))
  "Transforms supplied WIKI-PATH object into its URI form."
  (with-slots (type list parameters) path
    (format nil "/~a/~{~a~^/~}~:[~;~:*?~{~a=~a~^&~}~]"
            (string-downcase (symbol-name type))
            (mapcar #'hunchentoot:url-encode list)
            (mapcar #'hunchentoot:url-encode
                    (stringify-wiki-path-parameters parameters)))))

(defmethod wiki-path-from ((from (eql :filename)) (filename string))
  "Creates a fresh WIKI-PATH instance from the supplied FILENAME.

Beware that, FILENAME must be a namestring, not an absolute path. In such a
case, consider using FILE-NAMESTRING before passing FILENAME as an argument."
  (decode-from-filename :wiki-path filename))

(defmethod wiki-path-to ((to (eql :filename)) (path wiki-path))
  "Returns an encoded filename-safe string unique to supplied WIKI-PATH
instance."
  (encode-to-filename path))

;; Label is a convenient representation of wiki paths. It's in the
;; `type:/path/to/page' format. (See examples below.) When `type:' part is
;; omitted, it will be treated as `page' by default.

;; Label Representation Examples:
;; `account:Preferences'
;; `page:Foo/Bar/Baz' (identical to `Foo/Bar/Baz')
;; `image:Perfect/Scene'
;; `file:Releases/Latest'

(let ((type-separator-for-label ":"))
  (defmethod wiki-path-from ((from (eql :label)) (label string))
    "Creates a WIKI-PATH object by parsing supplied LABEL representation."
    (loop with type = :page
          with list-str = label
          with label-len = (length label)
          ;; Try to determine the path type.
          for avail-type in *path-types*
          for avail-type-str = (string-downcase (symbol-name avail-type))
          for avail-type-prefix = (string-append avail-type-str
                                                 type-separator-for-label)
          for len = (length avail-type-prefix)
          when (and (< len label-len)
                    (string= avail-type-prefix (subseq label 0 len)))
          do (progn
               (setq list-str (subseq label len)
                     type avail-type)
               (loop-finish))
          ;; Return new WIKI-PATH instance of determined path type.
          finally (return
                    (make-instance
                     'wiki-path
                     :type type
                     :list (normalize-path
                            (remove-if-not
                             #'(lambda (path)
                                 (< 0 (length path)))
                             (cl-ppcre:split
                              "/" list-str :limit *max-uri-depth*)))))))

  (defmethod wiki-path-to ((to (eql :label)) (path wiki-path))
    "Returns label representation of the passed WIKI-PATH object."
    (with-slots (type list) path
      (cond
        ((eql type :page) (format nil "~{~a~^/~}" list))
        (t (format nil "~(~a~)~a~{~a~^/~}"
                   (symbol-name type) type-separator-for-label list))))))
  
(defmethod wiki-path-to ((to (eql :pretty)) (path wiki-path))
  "Returns a human readable (pretty) representation of the supplied WIKI-PATH."
  (with-slots (type list) path
    (format nil "~{~a~^ » ~}~a"
            ;; Replace `_' characters with whitespace.
            (mapcar #'(lambda (item) (cl-ppcre:regex-replace-all "_" item " "))
                    list)
            ;; Don't print path types of paths of `:page' type.
            (if (eql type :page)
                "" (format nil " [~@(~a~)]" (symbol-name type))))))


;;; Auxiliary Utility Functions

(defun path-type-p (type)
  "Validate supplied wiki path TYPE."
  (member type *path-types*))

(defun path-type-to-prefix-regex (type)
  "Transforms supplied wiki path TYPE into suitable regular expression format to
be used in dispatch table of the web server."
  (check-type type (satisfies path-type-p))
  (string-append "^/" (string-downcase (symbol-name type)) "/.*"))

(define-per-request-var current-wiki-path ()
  "Returns the WIKI-PATH instance created from the current HTTP SCRIPT_NAME.

Beware that, function doesn't process REQUEST_URI, but SCRIPT_NAME. Therefore,
any GET parameters supplied will be ignored. \(This process is done by
Hunchentoot before. There is no need to invent the wheel from scratch. See
WITH-HTTP-PARAMETERS macro to access GET/POST parameters.)"
  (wiki-path-from :uri (hunchentoot:script-name)))

(define-per-request-fun update-path-history ()
  "Update visited paths history."
  (multiple-value-bind (visited-paths exists-p)
      (hunchentoot:session-value 'path-history)
    (let ((current-path (current-wiki-path)))
      ;; If there doesn't exist any previously visited path, initialize one.
      (if (or (not exists-p) (endp visited-paths))
        (setf (hunchentoot:session-value 'path-history) (list current-path)))
      ;; Push CURRENT-WIKI-PATH into the history list if it is not the
      ;; last path previously recorded in the history.
      (unless (obj-equalp current-path
                          (first (hunchentoot:session-value 'path-history)))
        (push current-path (hunchentoot:session-value 'path-history))
        ;; If length of the visited paths list exceeds
        ;; *MAX-PAGE-HISTORY* limit, crop it down.
        (if (> (length (hunchentoot:session-value 'path-history))
               *max-page-history*)
            (setf (hunchentoot:session-value 'path-history)
                  (subseq (hunchentoot:session-value 'path-history)
                          0 *max-page-history*)))))))

(defun wiki-path-from-to (from input to)
  "Shortcut to make cross transformation between different WIKI-PATH
representations."
  (wiki-path-to to (wiki-path-from from input)))

(defun parametrize-wiki-path (input &rest parameters)
  "Parametrize supplied WIKI-PATH by using specified PARAMETERS."
  (let ((obj (obj-duplicate input)))
    (setf (wiki-path-parameters obj) parameters)
    obj))

(defun last-visited-content-path
    (&optional (content-path-types *content-path-types*))
  "Returns WIKI-PATH of the last visited content. \(Not an operation path, only
content.)"
  (loop with avoided-paths = (mapcar
                                #'(lambda (label) (wiki-path-from :label label))
                                (list "ContentNotFound"
                                      "OperationNotFound"
                                      "RestrictedAccess"))
        for path in (hunchentoot:session-value 'path-history)
        when (and (member (wiki-path-type path) content-path-types)
                  (not (member path avoided-paths :test #'obj-equalp)))
        return path
        ;; If nothing found, return page:Main.
        finally (return (wiki-path-from :label *entrance-path*))))