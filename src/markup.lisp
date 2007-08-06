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


;;; CONTEXT-ATTACHMENT Class and Routines

(defclass context-attachment ()
  ((output
    :initarg :output
    :initform (make-string-output-stream)
    :accessor context-attachment-output
    :documentation "Stream to write produced output.")
   (flags
    :initarg :flags
    :initform nil
    :accessor context-attachment-flags
    :documentation "State variables.")
   (links
    :initform nil
    :accessor context-attachment-links
    :documentation "Collected links so far.")
   (headers
    :initform nil
    :accessor context-attachment-headers
    :documentation "Collected headers so far."))
  (:documentation "State storage unit between passes to parser rules and
  renderers."))

(defmacro with-clean-parser-context ((ctx input old-attachment) &body body)
  "Shortcut macro to create a new scope with a fresh PARSER-CONTEXT by
preserving LINKS slot of the old and new attachments."
  (with-unique-names (new-attachment)
    `(let* ((,new-attachment
             (make-instance
              'context-attachment
              :flags (context-attachment-flags ,old-attachment)))
            (,ctx (create-parser-context
                   ,input :attachment ,new-attachment))
            (ret (progn ,@body)))
       (setf (context-attachment-links ,old-attachment)
             (nconc (context-attachment-links ,old-attachment)
                    (context-attachment-links ,new-attachment)))
       ret)))


;;; Renderers

(defmacro define-html-renderer
    (fn (&rest args) (&optional (attachment (gensym))) &body body)
  `(defrenderer ,fn ,args (,attachment)
     (with-html-output (*standard-output*
                        (context-attachment-output ,attachment))
       ,@body)))

(define-html-renderer internal-link! (label text) (attachment)
  (let ((path (wiki-path-from :label label)))
    (htm
     (:a :class (if (wiki-content-exists-p (wiki-content-from :path path))
                    "internal"
                    "internal-not-existing")
         :href (wiki-path-to :uri path)
         (esc (if (empty-char-accum-p text)
                  (wiki-path-to :pretty path)
                  text))))
    (if (member 'collect-links (context-attachment-flags attachment))
        (push (wiki-path-to :label path)
              (context-attachment-links attachment)))))

(define-html-renderer external-link! (proto-prefix href text) (attachment)
  (let ((href (format nil "~a~a" proto-prefix href)))
    (htm
     (:a :class "external"
         :href (escape-string-minimal href)
         (esc (if (empty-char-accum-p text) href text))))))

(define-html-renderer bold-italic! (text) () (:b (:i (esc text))))
(define-html-renderer italic! (text) () (:i (esc text)))
(define-html-renderer bold! (text) () (:b (esc text)))
(define-html-renderer underline! (text) () (:u (esc text)))
(define-html-renderer monospace! (text) () (:tt (esc text)))
(define-html-renderer strike! (text) () (:s (esc text)))
(define-html-renderer superscript! (text) () (:sup (esc text)))
(define-html-renderer subscript! (text) () (:sub (esc text)))

(defmacro define-header-renderer (level)
  (let ((fn (intern (format nil "~:@(header~a!~)" level)))
        (tag (intern (format nil "~:@(h~a~)" level) :keyword)))
    `(define-html-renderer ,fn (header) (attachment)
       (with-clean-parser-context (ctx header attachment)
         (let ((text (inline-text? ctx)))
           (htm (,tag (:a :name (encode-to-filename text) (str text))))
           (if (member 'collect-headers (context-attachment-flags attachment))
               (push (cons ,level text) (context-attachment-headers attachment))))))))

(define-header-renderer 1)
(define-header-renderer 2)
(define-header-renderer 3)

(define-html-renderer code! (code) ()
  (:pre :class "code" (esc code)))

(define-html-renderer blockquote! (quote) (attachment)
  (with-clean-parser-context (ctx quote attachment)
    (htm (:div :class "blockquote" (str (inline-text? ctx))))))

(define-html-renderer item-list! (list) (attachment)
  (:ul
   (mapc
    #'(lambda (item)
        (with-clean-parser-context (ctx item attachment)
          (htm (:li (str (inline-text? ctx))))))
    list)))

(define-html-renderer enum-list! (list &optional (start 1)) (attachment)
  (:ol
   :start (or start 1)
   (mapc
    #'(lambda (item)
        (with-clean-parser-context (ctx item attachment)
          (htm (:li (str (inline-text? ctx))))))
    list)))

(define-html-renderer paragraph! (text) (attachment)
  (with-clean-parser-context (ctx text attachment)
    (htm (:p (str (inline-text? ctx))))))


;;; Inline Rules

(defrule internal-link? (&aux label text) ()
  "[["
  (:assign label (make-char-accum))
  (:+ (:not (:or "]]" (:type (or space? newline?))))
      (:char-push label))
  (:? (:* (:type (or white-space? newline?)))
      (:assign text (make-char-accum))
      (:+ (:not "]]")
          (:char-push text)))
  "]]"
  (:render internal-link! label text))

(defrule external-link? (&aux proto-prefix href text) ()
  "["
  (:assign proto-prefix (:or "http://" "https://" "ftp://"))
  (:assign href (make-char-accum))
  (:+ (:not (:or "]" (:type space? newline?)))
      (:char-push href))
  (:? (:* (:type (or white-space? newline?)))
      (:assign text (make-char-accum))
      (:+ (:not "]")
          (:char-push text)))
  "]"
  (:render external-link! proto-prefix href text))

(defmacro define-text-rule (fn element renderer)
  `(defrule ,fn (&aux text) ()
     ,element
     (:assign text (make-char-accum))
     (:+ (:not ,element)
         (:char-push text))
     ,element
     (:render ,renderer text)))

(define-text-rule bold-italic? "'''''" bold-italic!)
(define-text-rule italic? "'''" italic!)
(define-text-rule bold? "''" bold!)
(define-text-rule underline? "__" underline!)
(define-text-rule monospace? "`" monospace!)
(define-text-rule strike? "~~" strike!)
(define-text-rule superscript? "^" superscript!)
(define-text-rule subscript? ",," subscript!)

(defrule inline-element? () ()
  (:rule (or internal-link? external-link? bold-italic? italic? bold?
             underline? monospace? strike? superscript? subscript?)))

(defrule inline-text? (&aux c) (attachment)
  (:* (:or (:rule inline-element?)
           (:and (:assign c (:read-atom))
                 (write-string
                  (escape-string-minimal (make-string 1 :initial-element c))
                  (context-attachment-output attachment)))))
  (get-output-stream-string (context-attachment-output attachment)))


;;; Block Elements

(defmacro define-header-rule (level)
  (let* ((fn (intern (format nil "~:@(header~a?~)" level)))
         (renderer (intern (format nil "~:@(header~a!~)" level)))
         (chars (make-string level :initial-element #\=))
         (start (format nil "~a " chars))
         (end (format nil " ~a" chars)))
    `(defrule ,fn (&aux header) ()
       ,start
       (:assign header (make-char-accum))
       (:+ (:not (:or ,end (:type newline?)))
           (:char-push header))
       ,end
       (:type newline?)
       (:render ,renderer header))))

(define-header-rule 1)
(define-header-rule 2)
(define-header-rule 3)

(defrule code? (&aux code) ()
  "{{{"
  (:and (:assign code (make-char-accum))
        (:+ (:not "}}}")
            (:char-push code)))
  "}}}"
  (:or (:type newline?) (:eof))
  (:render code! code))

(defrule blockquote? (&aux text) ()
  "  "
  (:assign text (make-char-accum))
  (:+ (:not (:type newline?))
      (:char-push text))
  (:or (:type newline?) (:eof))
  (:render blockquote! text))

(defrule item-list? (&aux text list) ()
  (:+ " * "
      (:assign text (make-char-accum))
      (:+ (:not (:type newline?))
          (:char-push text))
      (:or (:type newline?) (:eof))
      (:list-push text list))
  (:render item-list! (nreverse list)))

(defrule positive-integer? (&aux d (n 0)) ()
  (:+ (:assign d (:type digit?))
      (:assign n (+ (* n 10) (- (char-code d) #.(char-code #\0)))))
  (:return n))

(defrule enum-list? (&aux text start enum prev list) ()
  (:+ " "
      (:assign enum (:rule positive-integer?))
      (:or (null prev) (= (1+ prev) enum))
      (:assign prev enum)
      (if (null start) (setq start enum) t)
      ". "
      (:assign text (make-char-accum))
      (:+ (:not (:type newline?))
          (:char-push text))
      (:or (:type newline?) (:eof))
      (:list-push text list))
  (:render enum-list! (nreverse list) start))

(defrule block-element? () ()
  (:rule (or header1? header2? header3? code? blockquote? item-list? enum-list?)))


;;; Main Rules

(defrule paragraph? (&aux (text (make-char-accum))) ()
  (:+ (:not (:checkpoint (:type newline?)
                         (:type newline?)))
      (:char-push text))
  (:? (:type newline?)
      (:type newline?))
  (:render paragraph! text))

(defrule document? () (attachment)
  (:* (:* (:checkpoint (:* (:type white-space?))
                       (:type newline?)))
      (:not (:eof))
      (:rule (or block-element? paragraph?))
      (write-string #.(format nil "~%~%")
                    (context-attachment-output attachment))))


;;; External Interface Routines

(defun write-cross-links (links)
  "Write on-disk data of the cross LINKS using specified REFERRER."
  (loop with referrer = (wiki-path-to :label (current-wiki-path))
        for referee in links
        do (let ((content (wiki-content-from :label referee)))
             ;; Ensure WIKI-CONTENT skeleton exists.
             (if (not (wiki-content-referrers-exist-p content))
               (ensure-wiki-content-on-disk-layout content))
             ;; Append cross link to referrers list.
             (unless (member referrer
                             ;; Remember that WIKI-CONTENT-REFERRERS
                             ;; doesn't need an existing content.
                             (wiki-content-referrers content)
                             :test #'string=)
               (push referrer (wiki-content-referrers content))
               (wiki-content-referrers-update content)))))

(defun remove-abolished-references (current-links)
  "Removes abolished references remained from the previous revision, if any."
  (let ((curr-content (current-wiki-content)))
    ;; If this an initial import, we don't to bother.
    (if (wiki-content-exists-p curr-content)
      (let* ((curr-revision (wiki-content-revision curr-content))
             (prev-content
              (wiki-content-from :path (wiki-content-path curr-content)
                                 (1- curr-revision)))
             (attachment (make-instance
                          'context-attachment
                          :output (make-broadcast-stream)
                          :flags (list 'collect-links)))
             (referrer (wiki-path-to :label (current-wiki-path))))
        ;; Parse file to collect links in the previous revision.
        (document?
         (create-parser-context
          (wiki-content-data-string prev-content)
          :attachment attachment))
        (mapc
         #'(lambda (referee)
             (let ((referee-content (wiki-content-from :label referee)))
               (when (and (wiki-content-referrers-exist-p referee-content)
                          (member referrer
                                  (wiki-content-referrers referee-content)
                                  :test #'string=))
                 (setf (wiki-content-referrers referee-content)
                       (remove referrer (wiki-content-referrers referee-content)
                               :test #'string=))
                 (wiki-content-referrers-update referee-content))))
         ;; Collect references that do not exist anymore.
         (set-difference (context-attachment-links attachment) current-links
                         :test #'string=))))))

(defgeneric markup-to-html (input &key process-cross-links)
  (:documentation "Transforms supplied input in wiki markup syntax to HTML."))

(defmethod markup-to-html ((input string) &key process-cross-links)
  "Transforms supplied INPUT of type markup string into HTML."
  (let ((attachment (make-instance
                     'context-attachment
                     :output (make-broadcast-stream)
                     :flags (list 'collect-headers))))
    ;; Do we need to collect links also?
    (if process-cross-links
        (push 'collect-links (context-attachment-flags attachment)))
    ;; First trip to collect headers and links.
    (document? (create-parser-context input :attachment attachment))
    ;; Do the last trip with collected information.
    (setf (context-attachment-flags attachment) nil
          (context-attachment-output attachment) (make-string-output-stream))
    ;; First of all, place TOC.
    (if (context-attachment-headers attachment)
        (with-html-output
            (*standard-output* (context-attachment-output attachment))
          (:div
           :class "toc"
           (:div :class "title" "Contents")
           (loop with enum = #(0 0 0)
                 for (level . text)
                 in (nreverse (context-attachment-headers attachment)) do
                 (ecase level
                   (1 (setq enum (vector (1+ (elt enum 0)) 0 0)))
                   (2 (setq enum (vector (elt enum 0) (1+ (elt enum 1)) 0)))
                   (3 (incf (elt enum 2))))
                 (htm
                  (:div
                   :class (format nil "l~a" level)
                   (:span :class "enum"
                          (str (format nil "~a." (elt enum (1- level)))))
                   (:a :href (string-append "#" (encode-to-filename text))
                       (str text))))))))
    ;; Make second trip.
    (document? (create-parser-context input :attachment attachment))
    (if process-cross-links
        (let ((links (delete-duplicates (context-attachment-links attachment)
                                        :test #'string=)))
          (write-cross-links links)
          (remove-abolished-references links)))
    (get-output-stream-string (context-attachment-output attachment))))

(defmethod markup-to-html ((input wiki-content) &key process-cross-links)
  "Transforms INPUT of type WIKI-CONTENT to its HTML representation."
  (markup-to-html (wiki-content-data-string input)
                  :process-cross-links process-cross-links))