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

(define-path-handler (page-path-handler)
  "`page' type wiki path handler."
  (with-http-parameters () (action)
    (handle-current-wiki-path
        (page-content-not-found
         page-operation-not-found
         page-restricted-access
         page-recent-changes
         page-search)
      ;; This doesn't look like a special page. Handle it as an
      ;; action.
      (if (wiki-content-exists-p)
          ;; Actions depending on an existing wiki page.
          (extended-case (action :test #'string=)
            ("plain" (page-plain))
            ("source" (page-source))
            ("edit" (page-edit))
            ("history" (page-history))
            ("diff" (page-diff))
            ("rename" (page-rename))
            (t (page-display)))
          ;; Else, this is a new page request or there's no such content.
          (extended-case (action :test #'string=)
            ("create" (page-create))
            (t (content-not-found)))))))


;;; Manual path handlers.

(defun page-content-not-found ()
  "`page:ContentNotFound' wiki path handler."
  (with-http-parameters (:default-request-type :get)
      ((create :parameter-type 'boolean)
       path)
    (if (and create path)
        ;; Evaluate page create request.
        (parametrized-redirect
         (wiki-path-from :label path)
         :action "create")
        ;; Display form.
        (with-html-template ()
          (:div
           :class "error-box"
           (:div :class "head" "Content Not Found!")
           (:div
            :class "body"
            (:p "The page you are trying to view cannot be found.")
            (if path
                (htm
                 (:p (:u "Error Context:"))
                 (:pre "Path: "
                       (str (wiki-path-from-to :label path :uri)))))))
          (if path
              (htm
               (:form
                :action (wiki-path-to :uri (current-wiki-path))
                :method "get"
                (:input :type "hidden" :name "create" :value t)
                "You can use below form to create this page."
                (:div
                 (:input :type "text"
                         :id "uri"
                         :name "path"
                         :value path
                         :style "width: 250px")
                 "&nbsp;" (:input :type "submit" :value "Create New Page")))))))))

(defun page-operation-not-found ()
  "`Page:OperationNotFound' wiki path handler."
  (with-http-parameters (:default-request-type :get)
      (path)
    (with-html-template () 
      (:div
       :class "error-box"
       (:div :class "head" "Operation Not Found!")
       (:div
        :class "body"
        (:p "Requested operation isn't supported by the related wiki path
        handlers.")
        (if path
            (htm
             (:p (:u "Error Context:"))
             (:pre "Path: " (str (wiki-path-from-to :label path :uri))))))))))

(defun page-restricted-access ()
  "`page:RestrictedAccess' wiki path handler."
  (with-http-parameters (:default-request-type :get)
      (path action)
    (with-html-template ()
      (:div
       :class "error-box"
       (:div :class "head" "Authorization Required!")
       (:div
        :class "body"
        (:p
         "Access to content you are trying to reach is restricted. "
         "You need to "
         (:a :class "internal"
             :href (wiki-path-from-to :label "account:SignIn" :uri)
             "sign in")
         " first to be able to view any restricted content.")
        (if path
            (htm
             (:p (:u "Error Context:"))
             (:pre "Path: " (str path)
                   (fmt "~%")
                   (if action (htm "Action: " (str action)))))))))))

(defun page-recent-changes ()
  "`page:RecentChanges' wiki path handler."
  (with-html-template ()
    (str
     (display-content-history
      (mapcar
       #'(lambda (content)
           (nconc
            (list :path (wiki-content-path content)
                  :revision (wiki-content-revision content))
            (wiki-content-log content)))
       *recently-changed-contents*)
      :has-paths t
      :has-indices t))))

(defun page-search ()
  "`page:Search' wiki path handler."
  (with-http-parameters (:default-request-type :get)
      (keyword)
    (with-html-template ()
      (if (zerop (length keyword))
          (htm "You can use top right search box to search for given keywords in
          the available content.")
          ;; We have a valid search request.
          (let* ((tokens
                  (delete-if
                   #'(lambda (token) (zerop (length token)))
                   (cl-ppcre:split " " keyword)))
                 (scanner
                  (cl-ppcre:create-scanner
                   (format nil "(~{~a~^|~})"
                           (mapcar
                            #'cl-ppcre:quote-meta-chars
                            tokens))
                   :multi-line-mode t))
                 results)
            ;; Search given keywords in the indexed content.
            (montezuma:search-each
             *content-index*
             (format nil "content:\"~{~a~^ ~}\""
                     (mapcar
                      #'(lambda (token)
                          (escape-string
                           token
                           :test #'(lambda (char) (find char "\""))))
                      tokens))
             #'(lambda (doc-num score)
                 (push (cons doc-num score) results)))
            ;; Display returned results.
            (mapc
             #'(lambda (result)
                 (let* ((doc-num (car result))
                        (score (cdr result))
                        (doc (montezuma:get-document *content-index* doc-num))
                        (title (montezuma:document-value doc "title"))
                        (content (montezuma:document-value doc "content"))
                        ranges)
                   (htm
                    (:div
                     :class "search-result"
                     (:span
                      (:a :class "internal"
                          :href (wiki-path-from-to :label title :uri)
                          (str title))
                      (fmt " (Relevance: ~a)" score))
                     ;; Look for keywords in the content text.
                     (cl-ppcre:do-matches (start end scanner content)
                       (push (cons start end) ranges))
                     (:table
                      (:tbody
                       (let ((parity 0))
                         (mapc
                          #'(lambda (range)
                              (setq parity (mod (1+ parity) 2))
                              (htm
                               (:tr
                                :class (if (oddp parity) "odd" "even")
                                (:td "..."
                                     (str
                                      (cl-ppcre:regex-replace-all
                                       scanner (subseq content (car range) (cdr range))
                                       "<b style='color: red'>\\1</b>"))
                                     "..."))))
                          (nreverse
                           ;; Merge close ranges.
                           (reduce
                            #'(lambda (acc curr)
                                (if (and acc (or (< (car curr) (caar acc) (cdr curr))
                                                 (< (- (caar acc) (cdr curr)) 32)))
                                    (cons (cons (car curr) (cdar acc))
                                          (rest acc))
                                    (push curr acc)))
                            ;; Expand ranges. (Already reversed RANGES
                            ;; list will be reversed again.)
                            (loop with len = (length content)
                                  for (start . end) in ranges
                                  collect (cons (max (- start 64) 0)
                                                (min (+ end 64) len)))
                            :initial-value nil))))))))))
             (sort results #'> :key #'cdr))
            (if (null results)
                (htm
                 (:div
                  :class "warning-box"
                  (:div :class "head" "No Results!")
                  (:div :class "body"
                        (fmt "Your search - <b>~a</b> - did not match any
                        documents." keyword))))))))))


;;; Action handlers.

(defun display-page-content (&optional plain)
  "Common function to display page content."
  (handle-if-modified-since)
  (let* ((current-rev (wiki-content-revision
                       (current-wiki-content)))
         (latest-rev (wiki-content-revision
                      (current-wiki-content-of-latest-revision)))
         (content-action (if (not (= current-rev latest-rev)) current-rev)))
    (with-html-template (:has-edit-bar t
                         :plain plain
                         :content-action content-action)
      (str (display-common-content-warnings))
      (str (if (wiki-content-cache-exists-p)
               ;; Display cached content.
               (wiki-content-cache-string (current-wiki-content))
               ;; Display raw data after markup transformation.
               (markup-to-html (current-wiki-content))))
      (if (and (not plain) (wiki-content-referrers-exist-p))
          (let ((referrers (wiki-content-referrers (current-wiki-content))))
            (if referrers
                (htm
                 (:div
                  :class "referrers"
                  "Referrers: "
                  (loop for refs on referrers
                        for path = (wiki-path-from :label (first refs))
                        do (htm (:a :class (if (wiki-content-exists-p
                                                (wiki-content-from :path path))
                                               "internal"
                                               "internal-not-existing")
                                    :href (wiki-path-to :uri path)
                                    (str (wiki-path-to :pretty path))))
                        unless (endp (rest refs))
                        do (htm ", "))))))))))

(defun page-plain ()
  "`plain' action handler for `page' type wiki paths."
  (display-page-content t))

(defun page-source ()
  "`source' action handler for `page' type wiki paths."
  (setf (hunchentoot:content-type) "text/plain; charset=utf-8")
  (wiki-content-data-string (current-wiki-content)))

(defun page-display ()
  "`display' action handler for `page' type wiki paths."
  (display-page-content))

(defun page-edit ()
  "`edit' action handler for `page' type wiki paths."
  (with-restricted-access
    (with-http-parameters (:default-request-type :post)
        ((minoredit :parameter-type 'boolean)
         (preserve :parameter-type 'boolean)
         (diff :parameter-type 'boolean)
         diffreq prevreq editreq data log)
      (with-html-template (:content-action "Edit")
        ;; Will we modify the page content?
        (when editreq
          (commit-wiki-content
           :data data
           :cache (markup-to-html data :process-cross-links t)
           :log log
           :type (if minoredit :minor))
          (let ((label (wiki-path-to :label (current-wiki-path)))
                doc-num)
            ;; Check if an index already exists for current page.
            (montezuma:search-each
             *content-index*
             (format nil "title:\"~a\"" label)
             #'(lambda (doc score)
                 (declare (ignore score))
                 (setq doc-num doc)))
            (if doc-num
                ;; If so, delete it.
                (montezuma:delete-document *content-index* doc-num))
            ;; Add new content to index.
            (montezuma:add-document-to-index
             *content-index*
             (list (cons "title" label)
                   (cons "content" data))))
          (parametrized-redirect (current-wiki-path) :edit-done t))
        ;; Evaluate any preview request.
        (if prevreq
            (htm (:b "Preview of the new content.")
                 (:div :id "edit-prev" (str (markup-to-html data)))))
        ;; Any diff request?
        (if (or (and prevreq diff) diffreq)
            (let* ((content (current-wiki-content-of-latest-revision))
                   (rev (wiki-content-revision content)))
              (with-input-from-string
                  (in
                   (with-output-to-string (out)
                     (difflib:unified-diff
                      out
                      (to-line-list :string (wiki-content-data-string content))
                      (to-line-list :string data)
                      :test-function #'equalp)))
                (cond
                  ((read-line in nil nil)
                   (file-position in 0) ; Undo the effect of READ-LINE.
                   (htm
                    (:b "Differences between the old and new content.")
                    (:div
                     :id "edit-diff"
                     (str (display-unified-diff in rev (1+ rev) :fancy)))))
                  (t
                   (htm
                    (:div
                     :class "warning-box"
                     (:div :class "head" "No Changes Made!")
                     (:div
                      :class "body"
                      "There isn't any difference between the old and new content
                    to display."))))))))
        ;; Edit form.
        (:form
         :action (wiki-path-to :uri (current-wiki-path))
         :method "post"
         (:input :type "hidden" :name "action" :value "edit")
         (:b "You can use below form to submit your modifications over page
         content.")
         (:textarea
          :id "edit-text"
          :name "data"
          :rows "24"
          (cond
            ((and preserve data) (esc data))    ; Preserve user modifications, if any.
            ((wiki-content-exists-p)            ; Otherwise, display content as is.
             (str (wiki-content-data-string
                   (current-wiki-content-of-latest-revision))))))
         (:div
          "(You may want to see "
          (:a :class "internal"
              :href (wiki-path-from-to :label "WikiMarkupSyntax" :uri)
              "WikiMarkupSyntax")
          " page for a detailed information about the wiki syntax.)")
         (:div
          :id "edit-log"
          :rows "4"
          (:span "A brief description of the changes you have made:")
          (:textarea :name "log" (if (and preserve log) (esc log))))
         (:table
          (:tr
           (:td
            :style "width: 50%"
            (:div (:input :type "checkbox" :name "minoredit"
                          :value t :checked minoredit)
                  "This is a minor edit.")
            (:div (:input :type "checkbox" :name "preserve"
                          :value t :checked t)
                  "Preserve my modifications in case of submit fails."))
           (:td
            :style "width: 50%"
            (:div (:input :type "submit" :name "prevreq" :value "Preview Changes")
                  "&nbsp;"
                  (:input :type "submit" :name "diffreq" :value "Display Changes"))
            (:div (:input :type "checkbox" :name "diff" :value t :checked t)
                  "While previewing changes, also display the differences."))))
         (:div
          (:input :type "reset" :name "reset" :value "Reset Form")
          "&nbsp;"
          (:input :type "submit" :name "editreq" :value "Submit Changes")))     
        (:div
         :style "margin-top: 32px"
         (:b "Plain view of the latest revision of the current wiki page.")
         (:div :id "edit-view"
               (str (if (wiki-content-cache-exists-p)
                        (wiki-content-cache-string (current-wiki-content))
                        (markup-to-html (current-wiki-content))))))))))

(defun page-history ()
  "`history' action handler for `page' type wiki paths."
  (with-html-template (:has-edit-bar t :content-action "History")
    (let ((changes
           (loop with path = (current-wiki-path)
                 for rev
                 downfrom (wiki-content-revision
                           (current-wiki-content-of-latest-revision))
                 to 0
                 collect (nconc (list :path path :revision rev)
                                (wiki-content-log
                                 (wiki-content-from :path path rev))))))
      (str (display-content-history changes :has-diff-form t)))))

(defun display-unified-diff (in revold revnew &optional (output-style :fancy))
  "Display formatted unified diff output."
  (read-line in nil nil)        ; Skip first two header lines.
  (read-line in nil nil)
  (ecase output-style
    (:raw
     (with-html ()
       (:pre
        :style "font-size: 8pt"
        (loop with buf = (make-string +buffer-size+)
              for pos = (read-sequence buf in)
              until (zerop pos)
              do (str (escape-string
                       ;; We need this ugly trick to avoid thousands
                       ;; of NULL characters at the end of
                       ;; not-completely-filled buffer outputs.
                       (if (< pos +buffer-size+)
                           (subseq buf 0 pos)
                           buf)))))))
    (:fancy
     ;; We will use a common (but not standardized yet) unified-diff
     ;; output syntax, to parse. (See the notes of Guido van Rossum
     ;; for more information.
     ;; http://www.artima.com/weblogs/viewpost.jsp?thread=164293)
     (with-html ()
       (:table
        :id "diff-table"
        (loop with old-pos
              with new-pos
              for line = (read-line in nil nil)
              while line do
              (extended-case ((elt line 0) :test #'char=)
                (#\@                    ; Diff header.
                 (cl-ppcre:register-groups-bind (o-p n-p)
                     ((string-append
                        "^@@"
                        " \\-([0-9]{1,})+,[0-9]{1,}"
                        " \\+([0-9]{1,})+,[0-9]{1,} "
                        "@@$") line)
                   ;; To cut off SBCL warnings, we need to (somehow)
                   ;; guarantee PARSE-INTEGER will take a string as an
                   ;; input.
                   (check-type o-p string)
                   (check-type n-p string)
                   (setq old-pos (parse-integer o-p)
                         new-pos (parse-integer n-p)))
                 (htm
                  (:tr (:th
                        :style "width: 50%"
                        :colspan "2" (fmt "r~a @ line ~a" revold old-pos))
                       (:th
                        :style "width: 50%"
                        :colspan "2" (fmt "r~a @ line ~a" revnew new-pos)))))
                (#\space                ; A cute shared line.
                 (let ((line (escape-string (subseq line 1))))
                   (htm
                    (:tr
                     :class "shared"
                     (:td :class "revold" (str (1- (incf old-pos))))
                     (:td :class "content" (str line))
                     (:td :class "revnew" (str (1- (incf new-pos))))
                     (:td :class "content" (str line))))))
                (#\-                    ; We have a miss.
                 (htm
                  (:tr
                   (:td :class "revold" (str (1- (incf old-pos))))
                   (:td :class "removed" (str (escape-string (subseq line 1))))
                   (:td :class "revnew" "-")
                   (:td))))
                (#\+                    ; Yo see the new guy?
                 (htm
                  (:tr
                   (:td :class "revold" "+")
                   (:td)
                   (:td :class "revnew" (str (1- (incf new-pos))))
                   (:td :class "added" (str (escape-string (subseq line 1)))))))
                (t             ; Opps! This is not something expected.
                 (htm
                  (:tr
                   :class "bogus"
                   (:td :rowspan "4" (str (escape-string line)))))))))))))

(defun page-diff ()
  "`diff' action handler for `page' type wiki paths."
  (with-http-parameters (:default-request-type :get :default-parameter-type 'integer)
      ((raw :parameter-type 'boolean)
       revold revnew)
    (with-html-template (:has-edit-bar t
                         :content-action (format nil "~a:~a" revold revnew))
      (let ((revhead (wiki-content-revision
                      (current-wiki-content-of-latest-revision))))
        (cond
          ;; Validate revision numbers.
          ((or (null revold) (null revnew)
               (not (<= 0 revold (1- revnew) revnew revhead)))
           (htm
            (:div
             :class "error-box"
             (:div :class "head" "Invalid Revision Number")
             (:div
              :class "body"
              "Supplied revision numbers for diffing seems like
             invalid. Please repeat again your submission."))))
          ;; Display diff.
          (t
           (with-input-from-string
               (in
                (with-output-to-string (out)
                  ;; Let's see how cl-difflib dances.
                  (let ((content-old
                         (wiki-content-from :path (current-wiki-path) revold))
                        (content-new
                         (wiki-content-from :path (current-wiki-path) revnew)))
                    (difflib:unified-diff
                     out
                     (to-line-list
                      :file (wiki-content-pathname content-old :data))
                     (to-line-list
                      :file (wiki-content-pathname content-new :data))
                     :test-function #'equal
                     :from-file (wiki-path-to
                                 :label (wiki-content-path content-old))
                     :to-file (wiki-path-to
                               :label (wiki-content-path content-new))
                     :from-file-date (universal-time-timestamp
                                      (getf
                                       (wiki-content-log content-old)
                                       :timestamp))
                     :to-file-date (universal-time-timestamp
                                    (getf
                                     (wiki-content-log content-new)
                                     :timestamp))))))
             (cond
               (raw
                ;; As requested, produce raw output.
                (htm
                 (str (display-unified-diff in revold revnew :raw))
                 (:form
                  :style "padding-left: 32px; padding-top: 8px"
                  :action (str (wiki-path-to :uri (current-wiki-path)))
                  :method "get"
                  (:input :type "submit"
                          :value "Switch to fancy formatted output")
                  (:input :type "hidden" :name "action" :value "diff")
                  (:input :type "hidden" :name "revold" :value (str revold))
                  (:input :type "hidden" :name "revnew" :value (str revnew)))))
               (t
                (htm
                 (str (display-unified-diff in revold revnew :fancy))
                 (:form
                  :style "padding-left: 32px; padding-top: 8px"
                  :action (str (wiki-path-to :uri (current-wiki-path)))
                  :method "get"
                  (:input :type "submit"
                          :value "Switch to raw output")
                  (:input :type "hidden" :name "action" :value "diff")
                  (:input :type "hidden" :name "raw" :value t)
                  (:input :type "hidden" :name "revold" :value (str revold))
                  (:input :type "hidden" :name "revnew" :value (str revnew)))))))))))))

(defun page-rename ()
  "`rename' action handler for `page' type wiki paths."
  (with-restricted-access
    (with-http-parameters (:default-request-type :get)
        (name opres oldpath)
      (let (page-already-exists)
        ;; If received a rename request, execute it.
        (if name
            (let ((newcontent (wiki-content-from
                               :path (wiki-path-from
                                      :label (string-append "page:" name)))))
              (cond
                ;; Check whether supplied path already exists?
                ((wiki-content-exists-p newcontent)
                 (setq page-already-exists t))
                (t
                 ;; Seems request is ok. Move content and redirect
                 ;; client to the new page.
                 (wiki-content-move newcontent)
                 ;; Update indexes as well.
                 (let (doc-num)
                   (montezuma:search-each
                    *content-index*
                    (format nil "title:\"~a\""
                            (wiki-path-to :label (current-wiki-path)))
                    #'(lambda (doc score)
                        (declare (ignore score))
                        (setq doc-num doc)))
                   (if doc-num
                       (montezuma:delete-document *content-index* doc-num)))
                 (montezuma:add-document-to-index
                  *content-index*
                  (list (cons "title" (wiki-path-to
                                       :label (wiki-content-path newcontent)))
                        (cons "content" (wiki-content-data-octets newcontent))))
                 (parametrized-redirect
                  (wiki-content-path newcontent)
                  :action "rename"
                  :opres "done"
                  :oldpath (wiki-path-to :label (current-wiki-path)))))))
        ;; Nothing special, display rename form.
        (with-html-template (:has-edit-bar t :content-action "Rename")
          (cond
            ;; Have we just renamed the page?
            ((string= opres "done")
             (htm
              (:div
               :class "info-box"
               (:div :class "head" "Page Renamed Successfully!")
               (:div
                :class "body"
                (fmt "`~a' renamed to `~a' successfully."
                     oldpath (wiki-path-to :label (current-wiki-path)))))))
            (t
             (htm
              (:p "When you rename a page, its content will be moved to a new page
by appending a related log message to the log records.")
              (:div
               :class "warning-box"
               (:div :class "head" "Cross-Link Corruption Possibility!")
               (:div
                :class "body"
                "Pages that are referenced by this page won't get updated about
                the current rename operation."))
              ;; Is this a failed rename attempt?
              (if page-already-exists
                  (htm
                   (:div
                    :class "error-box"
                    (:div :class "head" "Page Exists!")
                    (:div :class "body"
                          "Page you typed already exists. An existing page cannot be
                overwritten."))))
              (:form
               :action (wiki-path-to :uri (current-wiki-path))
               :method "get"
               (:input :type "hidden" :name "action" :value "rename")
               (:table
                :class "fancy"
                (:tr (:th "Rename Page"))
                (:tr
                 (:td
                  (:table
                   (:tr
                    (:td "Rename")
                    (:td (:input :type "text"
                                 :value (wiki-path-to :label (current-wiki-path))
                                 :disabled t))
                    (:td "to")
                    (:td (:input :type "text" :name "name")))
                   (:tr
                    (:td
                     :colspan "4"
                     :style "align: center"
                     (:input :type "submit" :value "Rename Page"))))))))))))))))

(defun page-create ()
  "`create' action handler for `page' type wiki paths."
  (with-restricted-access
    (with-http-parameters (:default-request-type :post)
        (data preserve prev save)
      (with-html-template (:content-action "Create")
        ;; Shall we save the new content?
        (when save
          (commit-wiki-content
           :data data
           :cache (markup-to-html data :process-cross-links t)
           :type :initial)
          (montezuma:add-document-to-index
           *content-index*
           (list (cons "title" (wiki-path-to :label (current-wiki-path)))
                 (cons "content" data)))
          (parametrized-redirect (current-wiki-path) :is-fresh t))
        ;; Evaluate any preview request.
        (if prev
            (htm
             "Preview of the new content."
             (:div :id "create-prev" (str (markup-to-html data)))))
        ;; New page submit form.
        "Use below form to submit content of the new page."
        (:form
         :method "post"
         :action (wiki-path-to :uri (current-wiki-path))
         (:input :type "hidden" :name "action" :value "create")
         (:textarea
          :id "create-data"
          :name "data"
          :rows "24"
          (if (and preserve data) (esc data)))
         (:div "(You may want to see "
               (:a :class "internal"
                   :href (wiki-path-from-to :label "WikiMarkupSyntax" :uri)
                   "WikiMarkupSyntax")
               " page for a detailed information about the wiki syntax.)")
         (:div (:input :type "checkbox" :name "preserve" :value t :checked t)
               "Preserve page content if submit fails.")
         (:div (:input :type "reset" :value "Reset Form") "&nbsp;"
               (:input :type "submit" :name "prev" :value "Preview Content") "&nbsp;"
               (:input :type "submit" :name "save" :value "Save Content")))))))