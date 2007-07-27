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


;;; Common page appearance components.

(defmacro with-html ((&optional standalone) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue ,standalone :indent nil)
     ,@body))

(defmacro with-html-template
    ((&key has-edit-bar plain (has-content-title t) content-title
           content-action)
     &body body)
  "Brings common HTML layout for the display components."
  `(with-html (t)
     (:html
      (str (html-header))
      (:body
       (unless ,plain
         (str (html-title-bar))
         (str (html-menu-bar))
         (str (html-search-bar)))
       (if ,has-content-title
           (htm
            (:div :id "content-title"
                  (if ,content-title
                      (str ,content-title)
                      (str (wiki-path-to :pretty (current-wiki-path))))
                  (if ,content-action
                      (htm (:span :id "content-action"
                                  (fmt "[~a]" ,content-action)))))))
       (:div :id "content" ,@body)
       (if (and ,has-edit-bar (not ,plain))
           (str (html-edit-bar)))))))

(defun html-header ()
  "HTML headers. \(<head> tag content.)"
  (with-html ()
    (:head
     (:meta :http-equiv "content-type" :content "text/html;charset=utf-8")
     (:style :type "text/css" :media "screen" "@import url(/static/layout.css);")
     (:title (str (wiki-path-to :pretty (current-wiki-path)))))))

(defun html-title-bar ()
  "Title bar. \(Wiki title and slogan goes here.)"
  (with-html ()
    (:div
     :id "top-bar"
     (:div :id "title" (str *wiki-title*))
     (:div :id "slogan" (str *wiki-slogan*)))))

(defun html-menu-bar ()
  "Menu bar. \(Static pages, visited paths, account operations.)"
  (with-html ()
    ;; Account Operations
    (:div
     :id "account-ops"
     (if (wiki-account-signed-in-p)
          (htm
           (:span
            (fmt "(Signed in as `~a'.)"
                 (wiki-account-username (current-wiki-account))))
           (:a :href (wiki-path-from-to :label "account:Preferences" :uri)
               "My Preferences")
           (:a :href (wiki-path-from-to :label "account:SignOut" :uri)
               "Sign Out"))
          (htm
           (:a :href (wiki-path-from-to :label "account:SignIn" :uri)
               "Sign In")
           (:a :href (wiki-path-from-to :label "account:Create" :uri)
               "New Account"))))
    ;; Visited Paths
    (:div
     :id "visited-paths"
     (loop for path in (hunchentoot:session-value 'path-history)
            do (htm (:a :href (wiki-path-to :uri path)
                        (str (wiki-path-to :pretty path))))))
    ;; Static Menu Paths
    (:div
     :id "static-menu-paths"
     (loop for path in (mapcar
                        #'(lambda (l) (wiki-path-from :label l))
                        *static-menu-paths*)
           do (htm (:a :href (wiki-path-to :uri path)
                       (str (wiki-path-to :pretty path))))))))

(defun html-search-bar ()
  "HTML search bar."
  (with-html ()
    (:script
     :type "text/javascript"
     "<!--
function checkSearchInput() {
    if (document.searchform.keyword.value.length < 1) {
        window.alert('Invalid search keywords!');
        return false;
    }

    return true;
}

var inputCleared = false;
function clearSearchInput() {
    if (!inputCleared) {
        document.searchform.keyword.value = '';
        inputCleared = true;
    }
}
-->")
    (:form
     :action (wiki-path-from-to :label "Search" :uri)
     :method "get"
     :name "searchform"
     :onsubmit "return checkSearchInput()"
     (:input :id "search-box"
             :type "text"
             :name "keyword"
             :value "Search"
             :onclick "clearSearchInput()"))))

(defun html-edit-bar ()
  "Edit bar for pages. \(Various page actions.)"
  (with-html ()
    (:table
     :id "edit-bar"
     (:tr
      (:td
       :class "info"
       (let* ((content (current-wiki-content))
              (log (wiki-content-log content)))
         (fmt "Revision ~d modified at ~a by ~a."
              (wiki-content-revision content)
              (universal-time-timestamp (getf log :timestamp))
              (getf log :account))))
      (:td
       :class "actions"
       (flet ((action-link (caption &rest params)
                (htm
                 (:a :class "action"
                     :href (wiki-path-to
                            :uri (apply #'parametrize-wiki-path
                                        (cons (current-wiki-path) params)))
                     (str caption)))))
         (htm
          "Available Actions:"
          (str (action-link "Plain Content" :action "plain"))
          (str (action-link "Source" :action "source"))
          (str (action-link "History" :action "history")))
         (if (wiki-account-signed-in-p)
             (htm
              (str (action-link "Edit" :action "edit"))
              (str (action-link "Rename" :action "rename"))))))))))


;;; Appearance related utility functions.

(defun display-common-content-warnings ()
  "Display common warning messages passed to content pages via HTTP GET
variables."
  (with-http-parameters (:default-request-type :get :default-parameter-type 'boolean)
      (is-fresh already-signed-in signed-in isnt-signed-in signed-out edit-done)
    (with-html ()
      ;; Are we about to handle a newly created page?
      (if is-fresh
          (htm
           (:div
            :class "info-box"
            (:div :class "head" "Content Saved Successfully")
            (:div
             :class "body"
             "New page content has been committed successfully."))))
      ;; An authenticated client attempted to sign in again?
      (if already-signed-in
          (htm
           (:div
            :class "warning-box"
            (:div :class "head" "Client Is Already Authenticated")
            (:div
             :class "body"
             "You have already signed in succesfully. You need to "
             (:a :class "internal"
                 :href (wiki-path-from-to :label "account:SignOut" :uri)
                 "sign out")
             " first, to be able to sign in again."))))
      ;; Are we just signed in?
      (if signed-in
          (htm
           (:div
            :class "info-box"
            (:div :class "head" "Sign In Request Is Approved")
            (:div
             :class "body"
             "Your sign in request is approved. Now you can safely access to
           authorization required content and operation pages."))))
      ;; Is client isn't authenticated yet to be able to sign out?
      (if isnt-signed-in
          (htm
           (:div
            :class "warning-box"
            (:div :class "head" "Client Isn't Authenticated")
            (:div
             :class "body"
             "Your client isn't authenticated. You need to "
             (:a :class "internal"
                 :href (wiki-path-from-to :label "account:SignIn" :uri)
                 "sign in")
             " first to be able to sign out."))))
      ;; Are we just signed out?
      (if signed-out
          (htm
           (:div
            :class "info-box"
            (:div :class "head" "Signed Out Successfully")
            (:div :class "body" "You have signed out successfully."))))
      ;; Have we just updated the content?
      (if edit-done
          (htm
           (:div
            :class "info-box"
            (:div :class "head" "Content Updated Succesfully!")
            (:div
             :class "body"
             (:p "New content has been committed succesfully."))))))))

(defun display-content-history
    (changes &key has-paths has-diff-form has-indices (start-indice 1))
  "Common utility function to display history of a content."
  (with-html ()
    (if changes
        (htm
         (:script
          :type "text/javascript"
          "<!--
function validateCheckedRevs()
{
    var idxnew = -1;
    var idxold = -1;

    for (i = 0; i < document.history.revold.length; i++)
        if (document.history.revold[i].checked)
        {
            idxold = i;
            break;
        }

    for (i = 0; i < document.history.revnew.length; i++)
        if (document.history.revnew[i].checked)
        {
            idxnew = i;
            break;
        }

    if (idxold < 0 || idxnew < 0 || idxold == idxnew)
    {
        window.alert('You must select two distinct revisions to compare.');
        return false;
    }

    return true;
}
-->")
         (:form
          :name "history"
          :method "get"
          :onsubmit (format nil "return ~a"
                            (if has-diff-form "validateCheckedRevs()" "true"))
          (:table
           :id "history-table"
           ;; Table of changes.
           (:thead
            (:tr
             (if has-indices (htm (:th "No.")))
             (if has-diff-form
                 (htm
                  (:th "Old")
                  (:th "New")))
             (:th "Rev.")
             (:th "Committed&nbsp;at")
             (:th "Type" (:sup "*"))
             (:th "Edit&nbsp;Summary")))
           (:tbody
            (mapc
             #'(lambda (change &aux (indice (1- (incf start-indice))))
                 (destructuring-bind
                       (&key path revision timestamp account type message) change
                   (htm
                    (:tr
                     :class (if (oddp indice) "odd" "even")
                     (if has-indices (htm (:td (str indice))))
                     (if has-diff-form
                         (htm
                          (:td :class "rev-radio"
                               (:input :type "radio" :name "revold" :value revision))
                          (:td :class "rev-radio"
                               (:input :type "radio" :name "revnew" :value revision))))
                     (:td :class "rev"
                          (:a :href (wiki-path-to
                                     :uri (parametrize-wiki-path path :rev revision))
                              (str revision)))
                     (:td :class "time"
                          (str (cl-ppcre:regex-replace-all
                                " " (universal-time-timestamp timestamp) "&nbsp;")))
                     (:td :style "text-align: center"
                          (case type
                            (:initial (htm (:span :class "commit-initial" "I")))
                            (:minor (htm (:span :class "commit-minor" "M")))
                            (:rename (htm (:span :class "commit-rename" "R")))
                            (t (htm (:span :class "commit-general" "G")))))
                     (:td
                      :width "100%"
                      (:div :class "details"
                            (if has-paths
                                (htm
                                 (:a :class "internal"
                                     :href (wiki-path-to :uri path)
                                     (str (wiki-path-to :label path)))
                                 (fmt " committed by ~a ~a."
                                      (escape-string account)
                                      (universal-time-age timestamp)))
                                (fmt "Committed by ~a ~a."
                                     (escape-string account)
                                     (universal-time-age timestamp))))
                      (:div :class "message"
                            (if (< (length message) 2)
                                (str "N/A")
                                (esc message))))))))
             changes)))
          ;; Explanation of commit type letters.
          (:div
           (:sup "*") "&nbsp;"
           (:span :class "commit-general" "G") ": General, "
           (:span :class "commit-initial" "I") ": Initial, "
           (:span :class "commit-minor" "M") ": Minor, "
           (:span :class "commit-rename" "R") ": Rename")
          ;; Submit buttons.
          (if has-diff-form
              (htm
               (:div :style "padding-left: 32px; padding-top: 8px"
                     (:input :type "checkbox" :name "raw")
                     "Produce raw diff output."
                     (:input :type "hidden" :name "action" :value "diff"))
               (:div :style "padding-left: 32px;"
                     (:input :type "submit"
                             :name "diffrevs"
                             :value "Compare Selected Revisions"))))))
        ;; If there is nothing to display.
        (htm
         (:div
          :class "info-box"
          (:div :class "head" "No Changes")
          (:div :class "body" "There isn't any changes yet to display."))))))