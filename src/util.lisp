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


;;; Auxilary CL shortcuts.

#-:lispworks
(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; (Copied to ALIW from cl-ppcre project of Dr. Edmund Weitz.)
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar
          #'(lambda (binding)
              (check-type binding (or cons symbol))
              (if (consp binding)
                  (destructuring-bind (var x) binding
                    (check-type var symbol)
                    `(,var
                      (gensym
                       ,(etypecase x
                                   (symbol (symbol-name x))
                                   (character (string x))
                                   (string x)))))
                  `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
     ,@body))

(defmacro string-append (&rest rest)
  (assert (not (null rest)))
  `(concatenate 'string ,@rest))

(defun parse-body-parts (body)
  "Parses supplied BODY with respect to ([[declaration* | documentation]] form*)
pattern and returns DECLERATIONS, DOCUMENTATION and FORM as seperate values."
  (loop with declarations
        with documentation
        for forms on body
        for form = (first forms)
        while forms
        do (cond ((and (listp form) (eql (first form) 'declare))
                  (push form declarations))
                 ((and (stringp form)
                       (not documentation)
                       (not (endp (rest forms))))
                  (setq documentation form))
                 (t (loop-finish)))
        finally (return (values (nreverse declarations) documentation forms))))

(defmacro with-body-parts ((declerations docstring rest) target-body &body body)
  "Binds passed DECLERATIONS, DOCSTRING and REST to the related parts of the
TARGET-BODY. \(Shortcut to PARSE-BODY-PARTS.)"
  `(multiple-value-bind (,declerations ,docstring ,rest)
       (parse-body-parts ,target-body)
     ,@body))

(defmacro extended-case ((item &key (test #'eql) single-test-form) &body forms)
  "An extended CASE function. \(See CLHS page of CASE function for details.) If
SINGLE-TEST-FORM is turned on, any (composite or not) form found in the test
field will be treated as a value returning function/form."
  `(cond
     ,@(mapcar
        #'(lambda (form &aux (test-form (first form)))
            (cons
             (cond
               ((and (not single-test-form) (listp test-form))
                `(member ,item (list ,@test-form) :test ,test))
               ((or (eql 'otherwise test-form)
                    (eql t test-form))
                `t)
               (t `(funcall ,test ,item ,test-form)))
             (rest form)))
        forms)))

(defun file-to-sequence (pathname &key (element-type 'character))
  "Reads specified file into a sequence and returns SEQUENCE and SIZE."
  (with-open-file (in pathname :element-type element-type)
    (loop with out = (flexi-streams:make-in-memory-output-stream
                      :element-type element-type)
          with buf = (make-array +buffer-size+ :element-type element-type)
          for pos = (read-sequence buf in)
          until (zerop pos)
          do (write-sequence buf out :end pos)
          sum pos into size
          finally (return-from file-to-sequence
                    (values
                     (flexi-streams:get-output-stream-sequence out)
                     size)))))


;;; TO-LINE-LIST family.

(defgeneric to-line-list (from input)
  (:documentation "Generic function to split given input into a list of its
lines. \(Newline characters will get omitted.)"))

(defmethod to-line-list ((from (eql :stream)) stream)
  "Split given stream into list of its lines."
  (loop for line = (read-line stream nil nil)
        while line collect line))

(defmethod to-line-list ((from (eql :file)) filespec)
  "Split given file into list of its lines."
  (with-open-file (in filespec)
    (to-line-list :stream in)))

(defmethod to-line-list ((from (eql :string)) string)
  "Split given string into list of its lines."
  (with-input-from-string (in string)
    (to-line-list :stream in)))


;;; Hash generation routines.

(defgeneric hash (input)
  (:documentation "Generates a unique fixed size hash value for given input."))

(defmethod hash ((input string))
  "Returns a fixed size hash value for the supplied string."
  (with-output-to-string (out)
    (loop for code across
          (md5:md5sum-sequence
           ;; FLEXI-STREAMS:STRING-TO-OCTETS does not return a
           ;; specialized array. We need to coerce it first to be able
           ;; to pass it to MD5-SEQUENCE.
           (coerce (flexi-streams:string-to-octets
                    input
                    :external-format hunchentoot:*hunchentoot-default-external-format*)
                   '(simple-array flexi-streams:octet (*))))
          do (format out "~2,'0x" code))))

(defmethod hash ((input list))
  "Return a fixed size hash value by recursively hashing supplied list items."
  (reduce
   #'(lambda (acc input)
       (hash (string-append acc input)))
   input))


;;; Generic object operators. (Comparison, duplication, etc.)

(defgeneric obj-equalp (x y &rest args)
  (:documentation "A convenient generic function to check the equality of
  supplied objects of class introduced within the code."))

(defgeneric obj-duplicate (o &rest args)
  (:documentation "A convenient generic function to duplicate an object of class
  introduced within the code."))


;;; Per-request variable/function definition macros.

(defmacro define-per-request-var (name (&rest args) &body body)
  "Defines a function which will return the value calculated via supplied BODY
upon every _new_ request. \(Return value will be cached for repetitive calls of
the same request.)"
  (with-body-parts (decls docstring body) body
    (with-unique-names (value present-p)
      `(defun ,name ,args
         ,(if docstring
           (string-append docstring " (Cached per HTTP request.)"))
         ,decls
         (multiple-value-bind (,value ,present-p)
             (hunchentoot:aux-request-value ',name)
           (if ,present-p
               ,value
               (setf (hunchentoot:aux-request-value ',name)
                     (progn ,@body))))))))

(defmacro define-per-request-fun (name (&rest args) &body body)
  "Defines a function which will be executed _once_ for every request during
page creation."
  (with-body-parts (decls docstring body) body
    (with-unique-names (value present-p)    
      `(defun ,name ,args
         ,(if docstring
            (string-append docstring " (Called once per HTTP request.)"))
         ,decls
         (multiple-value-bind (,value ,present-p)
             (hunchentoot:aux-request-value ',name)
           (declare (ignore ,value))
           (unless ,present-p
             (setf (hunchentoot:aux-request-value ',name) t)
             ,@body))))))


;;; Shortcut functions to form field bounds.

(defmacro form-field-min-len (key)
  "Shortcut to required minimum length of a form field."
  (or (car (getf +form-field-bounds+ key)) 0))

(defmacro form-field-max-len (key)
  "Shortcut to allowed maximum length of a form field."
  (or (cdr (getf +form-field-bounds+ key))
      ;; Else, get the biggest MAX value in +FORM-FIELD-BOUNDS+
      (loop for (min . max) in (rest +form-field-bounds+) by #'cddr
            maximize max)))

(defmacro form-field-resize (component input &rest rest)
  "Shortcut to resize supplied INPUT with respect to specified COMPONENT of the
+FORM-FIELD-BOUNDS+."
  `(progn
     ,@(loop for (key input) on (nconc (list component input) rest) by #'cddr
             collect `(if (> (length ,input) (form-field-max-len ,key))
                          (setf ,input
                                (subseq ,input (form-field-max-len ,key)))))))


;;; Utility Function & Macros for Handlers

(defmacro handle-current-wiki-path
    ((&rest handlers) &body default)
  "Shortcut macro to make condition forms of manually handled wiki paths."
  `(extended-case ((current-wiki-path) :test #'obj-equalp :single-test-form t)
     ,@(mapcar
        #'(lambda (handler)
            `((wiki-path-from                   
               :label (getf *manually-handled-paths*
                            (intern
                             (string-upcase
                              (symbol-name ',handler))
                             :keyword)))
              (,handler)))
        handlers)
     ,(if default `(t ,@default))))

(defmacro handle-if-modified-since (&key is-static)
  "Handles the `If-Modified-Since' header of the current HTTP request. \(Wrapper
for HUNCHENTOOT:HANDLE-IF-MODIFIED-SINCE.)"
  (if is-static
      `(hunchentoot:handle-if-modified-since ,(get-universal-time))
      `(hunchentoot:handle-if-modified-since
        (getf (wiki-content-log (current-wiki-content)) :timestamp))))

(defmacro with-http-parameters
    ((&key (default-parameter-type ''string) (default-request-type :both))
     (&rest fields) &body body)
  "Process specified lambda list similar to DEFINE-EASY-HANDLER of Hunchentoot."
  `(let ,(mapcar
          #'(lambda (field)
              (hunchentoot::make-defun-parameter
               field default-parameter-type default-request-type))
          fields)
     ,@body))

(defmacro define-path-handler ((name) &body body)
  "Generic macro to create a wiki handler."
  (with-body-parts (decls docstring body) body
    `(defun ,name () ,decls
            ,(if docstring
                 (string-append docstring " (Wiki Handler)"))
            (hunchentoot:start-session)
            (unwind-protect (progn ,@body)
              (update-path-history)))))

(defmacro with-restricted-access (&body body)
  "Wrapper macro for access required paths."
  `(cond
     ((not (wiki-account-signed-in-p))
      (parametrized-redirect
       (wiki-path-from :label "RestrictedAccess")
       :path (wiki-path-to :label (current-wiki-path))
       :action (hunchentoot:get-parameter "action")))
     (t ,@body)))

(defmacro parametrized-redirect (to-path &rest params)
  "Shortcut for parametrized WIKI-PATH redirections."
  `(hunchentoot:redirect
    (wiki-path-to :uri (parametrize-wiki-path ,to-path ,@params))))

(defun content-not-found ()
  "Shortcut to ContentNotFound redirection."
  (parametrized-redirect
   (wiki-path-from :label "ContentNotFound")
   :path (wiki-path-to :label (current-wiki-path))))

(defun operation-not-found ()
  "Shortcut to OperationNotFound redirection."
  (parametrized-redirect
   (wiki-path-from :label "OperationNotFound")
   :path (wiki-path-to :label (current-wiki-path))))


;;; Time conversion related routines.

(defun universal-time-age (to &optional (from (get-universal-time)))
  "Represents age of the supplied universal time in a human-readable string
format."
  (let ((tics (- from to)))
    (cond
      ((< 0 tics 60)
       (format nil "~@(~d second~:p ago~)" tics))
      ((< 0 (floor tics 60) 60)
       (format nil "~@(~d minute~:p ago~)" (floor tics 60)))
      ((< 0 (floor tics 3600) 24)
       (format nil "~@(~d hour~:p ago~)" (floor tics 3600)))
      ((< 0 (floor tics 86400) 30)
       (format nil "~@(~d day~:p ago~)" (floor tics 86400)))
      ((< 0 (floor tics 2592000) 12)
       (format nil "~@(~d month~:p ago~)" (floor tics 2592000)))
      (t
       (format nil "~@(~d year~:p ago~)" (floor tics 31536000))))))

(defun universal-time-timestamp (&optional (time (get-universal-time)))
  "Returns a timestamp string representation of the supplied TIME."
  (multiple-value-bind
        (second minute hour date month year day daylight-p zone)
      (decode-universal-time time)
    (declare (ignore daylight-p day zone))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))


;;; Filename encoding/decoding routines.

;;; Below encoding/decoding routines will help us to transform any
;;; string to/from its filename representation. (Mostly dedicated to
;;; filename) To customize its output, you can configure ESCAPE-CHAR
;;; and PATH-SEPARATOR-CHAR variables.

;;; Pay attention that, while using non-default values for ESCAPE-CHAR
;;; and PATH-SEPARATOR-CHAR variables, keep in mind the XSS attacks.

(defgeneric encode-to-filename (input &rest args)
  (:documentation "Encode given INPUT into filename-safe string form."))

;; Actually, decoder functions won't be needed by ALIW during
;; runtime. These functions are just for debugging purposes for the
;; administrators. (For instance, to learn the decoded name of a
;; content filename to learn which page it belongs to.)
(defgeneric decode-from-filename (type filename &rest args)
  (:documentation "Decodes given FILENAME of specified TYPE into its original
  form."))

(defmethod encode-to-filename
    ((input string) &key (escape-char (getf *filename-escapes* :escape-char)))
  "Encodes supplied string into its filename-safe string form."
  (let ((al-lim-lo (char-code #\A))
        (al-lim-hi (char-code #\Z))
        (nu-lim-lo (char-code #\0))
        (nu-lim-hi (char-code #\9))
        (escaped-escape-char (format nil "~c~:*~c" escape-char))
        (escape-format (format nil "~c~~a~:*~c" escape-char)))
    (with-output-to-string (out)
      (loop for char across input
            for code = (char-code (char-upcase char))
            do (cond
                 ((char= escape-char char)
                  (write-string escaped-escape-char out))
                 ;; Preserve 0-9, A-B and #\_ characters.
                 ((or (<= al-lim-lo code al-lim-hi)
                      (<= nu-lim-lo code nu-lim-hi)
                      (char= #\_ char))
                  (write-char char out))
                 ;; Convert #\space to #\_.
                 ((char= #\space char)
                  (write-char #\_ out))
                 (t (format out escape-format code)))))))

(defmethod encode-to-filename
    ((input list) &rest named-pairs
     &key (separator-char (getf *filename-escapes* :separator-char))
     &allow-other-keys)
    "Encodes supplied list into its filename-safe string form."
    (with-output-to-string (out)
      (loop for items on input
            do (write-string
                (apply #'encode-to-filename
                       (first items)
                       :allow-other-keys t
                       named-pairs)
                out)
            unless (null (rest items))
            do (write-char separator-char out))))

(defmethod decode-from-filename
    ((type (eql :string)) (filename simple-string)
     &key (escape-char (getf *filename-escapes* :escape-char)))
  "Decodes supplied encoded filename into its original string form."
  (with-output-to-string (out)
    (with-input-from-string (in filename)
      (loop with in-unsafe-char
            with unsafe-char-code
            for pos from 1 to (length filename)
            for char = (read-char in nil nil)
            while char
            do (if in-unsafe-char
                   ;; In an unsafe character.
                   (cond
                     ((digit-char-p char)
                      (setq unsafe-char-code
                            (+ (* 10 unsafe-char-code)
                               (digit-char-p char))))
                     ((char= char escape-char)
                      (if (zerop unsafe-char-code)
                          (write-char escape-char out)
                          (progn
                            (write-char (code-char unsafe-char-code) out)
                            (setq unsafe-char-code 0)))
                      (setq in-unsafe-char nil))
                     (t (error "Expecting a numeric! (Char.: ~a, Pos.: ~a)"
                               char pos)))
                   ;; In a safe character.
                   (cond
                     ((char= char escape-char)
                      (setq in-unsafe-char t
                            unsafe-char-code 0))
                     ;; Convert #\_ to #\space.
                     ((char= char #\_)
                      (write-char #\space out))
                     (t
                      (write-char char out))))))))

(defmethod decode-from-filename
    ((type list) (filename string) &rest named-pairs
     &key (separator-char (getf *filename-escapes* :separator-char))
     &allow-other-keys)
  "Decodes supplied encoded filename into its original list form."
  (mapcar
   #'(lambda (seq)
       (apply #'decode-from-filename seq :allow-other-keys t named-pairs))
   (cl-ppcre:split
    (make-string 1 :initial-element separator-char)
    filename)))


;;; Montezuma Helper Functions

(defun neutralize-montezuma-input (input)
  "Neutralizes supplied INPUT to be safely-passed to montezuma functions. \(This
hack exists because of a bug related with extended character handling in
Montezuma.)"
  (with-output-to-string (out)
    (loop with octets =
          (flexi-streams:string-to-octets
           input :external-format hunchentoot:*hunchentoot-default-external-format*)
          with prev-was-alnum
          for octet across octets
          for char = (code-char octet)
          do (cond
               ((alphanumericp char)
                (write-char char out)
                (setq prev-was-alnum t))
               (t
                (if prev-was-alnum
                    (write-char #\space out))
                (setq prev-was-alnum nil))))))


;;; Unclassified Functions

(defmacro generate-scope-serializer-macro
    (name ((arg arg-default) (key-init-fun &rest key-init-fun-args) (table table-lock))
     &optional docstring)
  `(defmacro ,name ((&optional (,arg ,arg-default)) &body body)
     ,docstring
     (with-unique-names (key)
       `(let ((,key (,',key-init-fun ,,arg ,@',key-init-fun-args)))
          ;; Check whether any previously set lock exists. If not, then
          ;; create a new one.
          (hunchentoot-mp:with-lock (,',table-lock)
            (unless (gethash ,key ,',table)
              (setf (gethash ,key ,',table)
                    (hunchentoot-mp:make-lock (princ-to-string ,key)))))
          ;; Evaluate body within serialized/locked scope.
          (hunchentoot-mp:with-lock ((gethash ,key ,',table))
            ,@body)))))