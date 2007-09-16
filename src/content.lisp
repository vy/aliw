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

(defclass wiki-content ()
  ((path
    :initarg :path
    :accessor wiki-content-path
    :documentation "WIKI-PATH of the content. \(See WIKI-PATH class for
    details.)")
   (meta
    :initarg :meta
    :writer (setf wiki-content-meta)
    :documentation "Meta information of the content.")
   (referrers
    :initarg :referrers
    :writer (setf wiki-content-referrers)
    :documentation "Label of the wiki content paths refering to this content.")
   (revision
    :initarg :revision
    :writer (setf wiki-content-revision)
    :documentation "Revision number of the content.")
   (data
    :initarg :data
    :writer (setf wiki-content-data)
    :documentation "Data of the current WIKI-PATH with specified REVISION.")
   (cache
    :initarg :data
    :writer (setf wiki-content-cache)
    :documentation "Cached \(and probably markup transformed) content data.")
   (size
    :initarg :data
    :accessor wiki-content-size
    :documentation "Size of the data slot.")
   (log
    :initarg :log
    :writer (setf wiki-content-log)
    :documentation "Related log message of the current WIKI-PATH of specific
    REVISION.")
   (loaded-slots
    :initform nil
    :accessor wiki-content-loaded-slots
    :documentation "List of loaded WIKI-CONTENT slots. \(For further details,
    see special reader methods for REVISION, DATA and LOG slots.)"))
  (:documentation
   "WIKI-CONTENT is a generic storage unit to store the contents of the
specified revision of a WIKI-PATH instance.

For efficiency purposes META, REFERRERS, REVISION, DATA and LOG slots built on
top of special reader methods which will load required on-disk data on
demand. Hence, you *MUST* use supplied reader methods while reaching these
slots.

Also, instead of trying to create manual instances of WIKI-CONTENT class, we
recommend \(you should be) using WIKI-CONTENT-FROM methods.

Furthermore, in case of a need to learn the pathname of a file which holds the
contents of a specific WIKI-CONTENT slot (e.g., DATA, LOG.), use
WIKI-CONTENT-PATHNAME methods.

Lastly, while modifying the actual data of a slot, we strongly recommend you to
use one of the already installed macros. (WITH-OUTPUT-TO-WIKI-CONTENT and
UPDATE-WIKI-CONTENT-HEAD-REVISION.) Otherwise, a corruption in the version
control mechanism is quite possible to occur, unless you know what you're
doing."))

(defmethod obj-equalp
    ((x wiki-content) (y wiki-content)
     &key (data-reader #'wiki-content-data-string)
     (data-equal-p #'string=))
  "Checks equality of supplied WIKI-CONTENT instances."
  (and (obj-equalp (wiki-content-path x) (wiki-content-path y))
       (equalp (wiki-content-meta x) (wiki-content-meta y))
       (equal (wiki-content-revision x) (wiki-content-revision y))
       (funcall data-equal-p (funcall data-reader x) (funcall data-reader y))
       (equalp (wiki-content-log x) (wiki-content-log y))))

(defmethod obj-duplicate ((x wiki-content) &rest args)
  "Creates a duplicate of supplied WIKI-CONTENT instance."
  ;; It's enough to copy just PATH and REVISION slots. Other slots
  ;; will get loaded on demand.
  (declare (ignore args))
  (wiki-content-from :path (obj-duplicate (wiki-content-path x))
                     (wiki-content-revision x)))


;;; Revision validation routines.

(defgeneric validate-wiki-content-revision-input (revision)
  (:documentation "Generic functions to transform supplied revision data input
  into internal format."))

(defmethod validate-wiki-content-revision-input ((revision integer))
  "Transform supplied integer into its internal revision number representation."
  (if (typep revision 'unsigned-byte) revision))

(defmethod validate-wiki-content-revision-input ((revision string))
  "Transform supplied string into its internal revision number representation."
  (validate-wiki-content-revision-input
   (parse-integer revision :junk-allowed t)))

(defmethod validate-wiki-content-revision-input (revision)
  "If didn't hit any applicable methods yet, treat input as unrecognized."
  nil)


;;; Utility functions to create WIKI-CONTENT objects.

(defgeneric wiki-content-from (from input &optional revision)
  (:documentation "Generic functions to create a WIKI-CONTENT object from the
supplied INPUT of various type."))

(defmacro define-wiki-content-from-for-wiki-paths (type)
  "Shortcut to create WIKI-CONTENT-FROM methods for various WIKI-PATH types."
  `(defmethod wiki-content-from ((from (eql ,type)) input &optional revision)
     (make-instance
      'wiki-content
      :path (wiki-path-from from input)
      :revision (validate-wiki-content-revision-input revision))))

(define-wiki-content-from-for-wiki-paths :uri)
(define-wiki-content-from-for-wiki-paths :filename)
(define-wiki-content-from-for-wiki-paths :label)

(defmethod wiki-content-from ((from (eql :path)) (path wiki-path)
                              &optional revision)
  "Creates WIKI-CONTENT from supplied WIKI-PATH and \(optional) revision
number."
  (make-instance 'wiki-content
                 :path path
                 :revision (validate-wiki-content-revision-input revision)))


;;; Pathname routines for WIKI-CONTENT objects.

(defgeneric wiki-content-pathname (content component)
  (:documentation "Returns WIKI-CONTENT pathname of the specified component."))

(defmethod wiki-content-pathname
    ((content wiki-content) (component (eql :content-directory)))
  "Returns toplevel content directory of the specified WIKI-CONTENT."
  (pathname-as-directory
   (merge-pathnames (wiki-path-to :filename (wiki-content-path content))
                    *content-directory*)))

(defmethod wiki-content-pathname
    ((content wiki-content) (component (eql :meta)))
  "Returns filepath of the meta file for the specified WIKI-CONTENT."
  (pathname-as-file
   (merge-pathnames
    "meta" (wiki-content-pathname content :content-directory))))

(defmethod wiki-content-pathname
    ((content wiki-content) (component (eql :referrers)))
  "Returns filepath of the referrers file for the specified WIKI-CONTENT."
  (pathname-as-file
   (merge-pathnames
    "referrers" (wiki-content-pathname content :content-directory))))

(defmethod wiki-content-pathname
    ((content wiki-content) (component (eql :cache)))
  "Returns filepath of the cache file for the specified WIKI-CONTENT."
  (pathname-as-file
   (merge-pathnames
    "cache" (wiki-content-pathname content :content-directory))))

(defmethod wiki-content-pathname
    ((content wiki-content) (componenet (eql :revs-directory)))
  "Returns directory of revision files."
  (pathname-as-directory
   (merge-pathnames "revs" (wiki-content-pathname content :content-directory))))

(defmethod wiki-content-pathname
    ((content wiki-content) (component (eql :logs-directory)))
  "Returns directory of log files."
  (pathname-as-directory
   (merge-pathnames "logs" (wiki-content-pathname content :content-directory))))

(defmethod wiki-content-pathname
    ((content wiki-content) (component (eql :data)))
  "Returns data filename of the specified WIKI-CONTENT."
  (pathname-as-file
   (merge-pathnames (princ-to-string (wiki-content-revision content))
                    (wiki-content-pathname content :revs-directory))))

(defmethod wiki-content-pathname
    ((content wiki-content) (component (eql :log)))
  "Returns log filename of the specified WIKI-CONTENT."
  (pathname-as-file
   (merge-pathnames (princ-to-string (wiki-content-revision content))
                    (wiki-content-pathname content :logs-directory))))


;;; In WITH-WIKI-CONTENT-LOCK, we are assuming that a hash table with
;;; as much entries as the # of available wiki pages won't be a
;;; problem for the current CL implementation. Otherwise, you may need
;;; to implement your own garbage collector for unused locks in
;;; *CONTENT-LOCKS-TABLE*.

(generate-scope-serializer-macro with-wiki-content-lock
  ((content (current-wiki-content))
   (wiki-content-pathname :content-directory)
   (*content-locks-table* *content-locks-table-lock*))
  "Lock related content for access.")


;;; Reader methods for WIKI-CONTENT slots with on-demand initialization.

(defmacro define-wiki-content-on-demand-slot-reader (fn (content slot) &body body)
  "Convenient macro to create on-demand slots using LOADED-SLOTS slot."
  (with-unique-names (is-member)
    (with-body-parts (decls docstring body) body
      `(progn
         (defgeneric ,fn (,content &rest args)
           (:documentation ,docstring))
         (defmethod ,fn ((,content wiki-content) &key force)
           ,@decls
           (let ((,is-member (member ,slot (wiki-content-loaded-slots ,content))))
             (when (or force (not ,is-member))
               (if (not ,is-member)
                   (push ,slot (wiki-content-loaded-slots ,content)))
               ,@body))
           (slot-value ,content ',(intern (symbol-name slot))))))))

(defmacro define-wiki-content-on-demand-data/cache-reader (slot fn element-type)
  "Shortcut to define DATA/CACHE slot readers for different element types."
  (assert (member slot '(:cache :data)))
  `(define-wiki-content-on-demand-slot-reader ,fn (content ,slot)
     ,(format nil "(~a) Reader function for ~a slot of WIKI-CONTENT class."
              element-type slot)
     (with-wiki-content-lock (content)
       (multiple-value-bind (seq size)
           (file-to-sequence (wiki-content-pathname content ,slot)
                             :element-type ',element-type)
         (setf (,(intern (format nil "~:@(wiki-content-~a~)" slot)) content)
               seq
               (wiki-content-size content) size)))))

(defmacro define-wiki-content-on-demand-cache-reader (fn element-type)
  "Shortcut to define CACHE slot readers for different element types."
  `(define-wiki-content-on-demand-slot-reader ,fn (content :cache)
     ,(format nil "(~:(~a~)) Reader function for CACHE slot of WIKI-CONTENT class."
              (symbol-name element-type))
     (with-wiki-content-lock (content)
       (multiple-value-bind (seq size)
           (file-to-sequence (wiki-content-pathname content :cache)
                             :element-type ',element-type)
         (setf (wiki-content-cache content) seq
               (wiki-content-size content) size)))))


(define-wiki-content-on-demand-slot-reader wiki-content-meta (content :meta)
  "Reader function for the META slot of WIKI-CONTENT class."
  (with-wiki-content-lock (content)
    (with-open-file (in (wiki-content-pathname content :meta))
      (let ((meta (with-standard-io-syntax (read in))))
        ;; Avoid recursive (self-referencing) head revision.
        (assert (not (null (getf meta :latest-revision))))
        (setf (wiki-content-meta content) meta)))))

(define-wiki-content-on-demand-slot-reader wiki-content-referrers (content :referrers)
  "Reader function for the REFERRERS slot of the WIKI-CONTENT class."
  (with-wiki-content-lock (content)
    (with-open-file (in (wiki-content-pathname content :referrers))
      (setf (wiki-content-referrers content)
            (with-standard-io-syntax (read in))))))

(define-wiki-content-on-demand-slot-reader wiki-content-revision (content :revision)
  "Reader function for the REVISION slot of WIKI-CONTENT class."
  (let ((rev (slot-value content 'revision)))
    (if (or (null rev)
            ;; Supplied revision cannot be bigger than LATEST-REVISION.
            (> rev (getf (wiki-content-meta content) :latest-revision)))
        (setf (wiki-content-revision content)
              (getf (wiki-content-meta content) :latest-revision)))))

(define-wiki-content-on-demand-slot-reader wiki-content-log (content :log)
  "Reader function for the LOG slot of WIKI-CONTENT."
  (with-wiki-content-lock (content)
    (with-open-file (in (wiki-content-pathname content :log))
      (setf (wiki-content-log content) (read in)))))

(define-wiki-content-on-demand-data/cache-reader
    :data wiki-content-data-string character)

(define-wiki-content-on-demand-data/cache-reader
    :data wiki-content-data-octets flexi-streams:octet)

(define-wiki-content-on-demand-data/cache-reader
    :cache wiki-content-cache-string character)

(define-wiki-content-on-demand-data/cache-reader
    :cache wiki-content-cache-octets flexi-streams:octet)


;;; Utility routines to read/write data on disk of a WIKI-CONTENT.

(defun commit-wiki-content
    (&key (content (current-wiki-content)) log data type overwrite cache
     (writer #'write-sequence) (element-type 'character)
     dont-update-data dont-update-changes-history)
  "Generic function to commit content data to disk.

CONTENT will be committed to disk using supplied LOG, DATA and CACHE with
specified WRITER function. ELEMENT-TYPE is the element-type of the LOG, DATA and
CACHE variables will be used during writing with specified WRITER.

At every new commit, a new revision will be created, unless OVERWRITE is
specified.

If DONT-UPDATE-DATA is turned on, DATA of the specified CONTENT won't be
updated.

If DONT-UPDATE-CHANGES-HISTORY is supplied, changes history table won't get
informed from the commit."
  (with-wiki-content-lock (content)
    (flet ((commit-component (component data &key (writer #'print)
                              (element-type 'character))
             (with-open-file (out (wiki-content-pathname content component)
                              :element-type element-type
                              :direction :output
                              :if-exists :supersede)
               (with-standard-io-syntax (funcall writer data out)))))
      (cond
        ((wiki-content-exists-p content)
         ;; Looks like content already exists. Then just increment the
         ;; latest revision and use it, unless caller wants us to
         ;; overwrite the latest revision.
         (unless overwrite
           ;; Reset revision.
           (setf (wiki-content-revision content) nil)
           ;; Force WIKI-CONTENT-REVISION to parse the LATEST-REVISION.
           (wiki-content-revision content :force t)
           ;; Incremenet LATEST-REVISION. (Both in META and REVISION slot.)
           (setf (getf (wiki-content-meta content) :latest-revision)
                 (incf (wiki-content-revision content)))
           ;; Commit META.
           (commit-component :meta (wiki-content-meta content))))
        (t
         ;; This is a new guy. Make it feel like at home.
         (ensure-wiki-content-on-disk-layout content)
         (commit-component :meta (list :latest-revision 0))
         (setf (wiki-content-revision content) 0)))
      ;; Will we update content data?
      (if (not dont-update-data)
          (commit-component
           :data data
           :writer writer
           :element-type element-type))
      ;; If we're overwriting an existing content, there is no need to
      ;; update log.
      (if (not overwrite)
          (commit-component
           :log (list :timestamp (get-universal-time)
                      :account (wiki-account-username (current-wiki-account))
                      :message log
                      :type type
                      :client (format nil "<!--~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~]-->"
                                      (hunchentoot:remote-addr)
                                      (hunchentoot:header-in :x-forwarded-for)))))
      ;; If we received any cache data, write it down. Otherwise,
      ;; delete any on-disk cache file associated with this content.
      (if cache
          (commit-component :cache cache :writer writer :element-type element-type)
          (if (wiki-content-cache-exists-p content)
            (delete-file (wiki-content-pathname content :cache))))))
  ;; Will we update the changes history table?
  (if (not dont-update-changes-history)
    (update-recently-changed-contents content)))

(defun wiki-content-move (newcontent &optional (oldcontent (current-wiki-content)))
  "Move wiki content to a new path. \(Beware that, referrers won't get informed
about this change!)"
  (with-wiki-content-lock (oldcontent)
    (with-wiki-content-lock (newcontent)
      ;; Move file.
      (rename-file
       (pathname-as-file
        (wiki-content-pathname oldcontent :content-directory))
       (pathname-as-file
        (wiki-content-pathname newcontent :content-directory)))
      ;; Log operation.
      (commit-wiki-content
        :content newcontent
        :log (format nil "Rename `~a' to `~a'."
                     (wiki-path-to :label (wiki-content-path oldcontent))
                     (wiki-path-to :label (wiki-content-path newcontent)))
        :data (wiki-content-data-octets newcontent)
        :element-type 'flexi-streams:octet
        :type :rename))))

(defun wiki-content-referrers-update (content)
  "Update on-disk referrers of the supplied WIKI-CONTENT."
  ;; If REFERRERS slot of the supplied WIKI-CONTENT isn't read from
  ;; disk yet, there's no need to bother trying to commit REFERRERS
  ;; slot value to disk. Because, they will be identical. (Assuming,
  ;; programmer tries to reach REFERRERS through its on-demand slot
  ;; reader function.)
  (if (member :referrers (wiki-content-loaded-slots content))
      (with-wiki-content-lock (content)
        (with-open-file (out (wiki-content-pathname content :referrers)
                             :direction :output
                             :if-exists :supersede)
          (with-standard-io-syntax
            (print (wiki-content-referrers content) out))))))


;;; Utility functions to manage the recent changes history.

(defmacro with-recently-changed-contents-lock (&body body)
  "Shortcut to lock *RECENTLY-CHANGED-CONTENTS* during access."
  `(hunchentoot-mp:with-lock (*recently-changed-contents-lock*)
     ,@body))

(defun ensure-recently-changed-contents-size (contents)
  "Ensures supplied list of WIKI-CONTENTs is smaller than
*MAX-RECENT-CHANGES-HISTORY* limit. \(Assuming, supplied list is sorted with
respect to log timestamps.)"
  (let ((size (length contents)))
    (if (> size *max-recent-changes-history*)
        (subseq contents 0 *max-recent-changes-history*)
        contents)))

(defun load-recently-changed-contents ()
  "Loads stored WIKI-CONTENT list of recently changed contents."
  (let* ((content
          (wiki-content-from
           :label (getf *manually-handled-paths* :page-recent-changes)))
         (contents
          (sort
           (loop for (label . revision)
                 in (with-input-from-string
                        (in (wiki-content-data-string content))
                      (with-standard-io-syntax (read in nil nil)))
                 for content = (wiki-content-from :label label revision)
                 when (wiki-content-exists-p content)
                 collect content)
           #'>  ;predicate
           :key #'(lambda (content)
                    (getf (wiki-content-log content) :timestamp)))))
    (ensure-recently-changed-contents-size contents)))

(defun write-recently-changed-contents ()
  "Writes *RECENTLY-CHANGED-CONTENTS* to disk."
  (with-recently-changed-contents-lock
    (commit-wiki-content
        :content (wiki-content-from
                  :label (getf *manually-handled-paths* :page-recent-changes))
        :data (mapcar
               #'(lambda (content)
                   (cons (wiki-path-to :label (wiki-content-path content))
                         (wiki-content-revision content)))
               *recently-changed-contents*)
        :overwrite t
        :writer #'print
        :dont-update-changes-history t)))

(defun update-recently-changed-contents (content)
  "Prepends supplied WIKI-CONTENT to *RECENTLY-CHANGED-CONTENTS* list and
updates on-disk storage of the recently changed contents."
  (with-recently-changed-contents-lock
    ;; Make sure supplied WIKI-CONTENT has a non-NIL revision number.
    (wiki-content-revision content :force t)
    ;; Update *RECENTLY-CHANGED-CONTENTS*.
    (setq *recently-changed-contents*
          (ensure-recently-changed-contents-size
           (cons content *recently-changed-contents*)))
    ;; Write result to disk too.
    (write-recently-changed-contents)))


;;; Unclassified utility routines.

(defun ensure-wiki-content-on-disk-layout (content)
  "Ensures that the skeleton on-disk files of the supplied WIKI-CONTENT exists."
  ;; Create directories.
  (mapc
   #'(lambda (component)
       (ensure-directories-exist (wiki-content-pathname content component)))
   (list :content-directory :revs-directory :logs-directory))
  ;; Create META and REFERRERS files.
  (loop for component in (list :meta :referrers)
        for pathname = (wiki-content-pathname content component)
        unless (file-exists-p pathname)
        do (with-open-file (out pathname
                                :direction :output
                                :if-does-not-exist :create)
             (with-standard-io-syntax (print nil out)))))

(defun wiki-content-exists-p (&optional (content (current-wiki-content)))
  "Checks if any on-disk content exists for the specified content."
  (if (file-exists-p (wiki-content-pathname content :content-directory))
      (let ((meta
             (with-open-file (in (wiki-content-pathname content :meta)
                                 :if-does-not-exist nil)
               (with-standard-io-syntax (read in)))))
        (or (getf meta :latest-revision)
            (getf meta :phony)))))

(defun wiki-content-cache-exists-p (&optional (content (current-wiki-content)))
  "Checks if any on-disk cache file exists for the specified content."
  (file-exists-p (wiki-content-pathname content :cache)))

(defun wiki-content-referrers-exist-p
    (&optional (content (current-wiki-content)))
  "Checks if there exists any on-disk content referrers file exists for the
specified content."
  (file-exists-p (wiki-content-pathname content :referrers)))

(define-per-request-var current-wiki-content ()
  "Returns the WIKI-CONTENT instance created from the current WIKI-PATH and
`revision' GET parameter."
  (with-http-parameters ()
      ((rev :request-type :get :parameter-type 'integer))
    (wiki-content-from :path (current-wiki-path) rev)))

(define-per-request-var current-wiki-content-of-latest-revision ()
  "Returns a copy of CURRENT-WIKI-CONTENT with revision set to head."
  (let ((content (obj-duplicate (current-wiki-content))))
    (setf (wiki-content-revision content)
          (getf (wiki-content-meta content) :latest-revision))
    content))