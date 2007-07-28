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

(define-path-handler (account-path-handler)
  "`account' type wiki path handler."
  (handle-current-wiki-path
      (account-create
       account-sign-in
       account-sign-out
       account-preferences)
    (operation-not-found)))

(defun account-sign-in ()
  "account:SignIn handler."
  ;; Don't bother if client is already signed in.
  (if (wiki-account-signed-in-p)
      (parametrized-redirect (last-visited-content-path) :already-signed-in t))
  (with-http-parameters (:default-request-type :post)
      ((account-created :parameter-type 'boolean :request-type :get)
       (authreq :parameter-type 'boolean)
       username password)
    (let (sign-in-failed)
      ;; Evaluate any authentication request.
      (when authreq
        ;; Crop down username and password.
        (form-field-resize :username username :password password)
        (if (wiki-account-sign-in username password)
            (parametrized-redirect (last-visited-content-path) :signed-in t)
            (setq sign-in-failed t)))
      (with-html-template ()
        ;; Welcome new user.
        (if account-created
            (htm
             (:div
              :class "info-box"
              (:div :class "head" "Account Created Successfully")
              (:div
               :class "body"
               "Your new account is created successfully. Now you can sign in with
             your new account username and password."))))
        ;; Have we just had a failed sign in attempt?
        (if sign-in-failed
            (htm
             (:div
              :class "error-box"
              (:div :class "head" "Sign In Request Isn't Approved!")
              (:div
               :class "body"
               "Your sign in request isn't approved. Please check your
<i>username</i> and <i>password</i> again. Also be sure your web browser accepts
cookies from wiki domain."))))
        ;; Whatever. Display the form.
        (:script
         :type "text/javascript"
         (fmt "<!--
var minuserlen = ~a;
var minpasslen = ~a;

function checkFormFields()
{
    if (document.signin.username.value.length < minuserlen) {
        window.alert('Account username cannot be smaller than '+
                     minuserlen+' characters.');
        return false;
    }

    if (document.signin.password.value.length < minpasslen) {
        window.alert('Account password cannot be smaller than '+
                     minpasslen+' characters.');
        return false;
    }

    return true;
}
-->"
              (form-field-min-len :username)
              (form-field-min-len :password)))
        (:form
         :name "signin"
         :onsubmit "return checkFormFields()"
         :method "post"
         :action (wiki-path-to :uri (current-wiki-path))
         "Please type your " (:i "username") " and " (:i "password") " using
below form and press to " (:i "Sign In") " button to continue."
         (:table
          :class "fancy"
          (:tr (:th :colspan "3" "Sign In Form"))
          (:tr
           (:td "Username" (:sup "1"))
           (:td ":")
           (:td (:input :type "text"
                        :name "username"
                        :maxlength (form-field-max-len :username)
                        :value (str username))))
          (:tr
           (:td "Password" (:sup "2"))
           (:td ":")
           (:td (:input :type "password"
                        :maxlength (form-field-max-len :password)
                        :name "password")))
          (:tr
           (:td
            :style "text-align: center"
            :colspan "3"
            (:input :type "submit" :name "authreq" :value "Sign In"))))
         (:p (:sup "1")
             (:span :class "comment"
                    (fmt " (Minimum ~a characters are required.)"
                         (form-field-min-len :username)))
             (:br)
             (:sup "2")
             (:span :class "comment"
                    (fmt " (Minimum ~a characters are required.)"
                         (form-field-min-len :password)))))))))

(defun account-sign-out ()
  "account:SignOut handler."
  (parametrized-redirect
   (last-visited-content-path)
   (if (wiki-account-signed-in-p)
       (progn
         (wiki-account-sign-out)
         :signed-out)
       :isnt-signed-in) t))

(defun account-preferences ()
  "account:Preferences handler"
  (with-restricted-access
    (with-http-parameters (:default-request-type :post)
        ((editreq :parameter-type 'boolean)
         (alterpass :parameter-type 'boolean)    
         passold passnew1 passnew2 email)
      (let (pref-update-failed pref-update-done)
        ;; If we have received an edit request, evaluate it.
        (when editreq
          ;; Resize collected form fields.
          (form-field-resize :email email :password passold :password passnew1)
          (let ((newacc
                 (wiki-account-from
                  :plist (list
                          :username (wiki-account-username (current-wiki-account))
                          :password passnew1
                          :email email))))
            (cond
              ;; Will we also need to alter the password?
              (alterpass
               (cond
                 ((< (length passnew1) (form-field-min-len :password))
                  (push :small-pass pref-update-failed))
                 ((not (string= passnew1 passnew2))
                  (push :newpass-mismatch pref-update-failed))
                 ((not (wiki-account-check-password
                        (current-wiki-account) passold))
                  (push :wrong-pass pref-update-failed))))
              (t
               ;; Seems like client didn't want to touch his/her old password.
               (setf (wiki-account-password newacc)
                     (wiki-account-password (current-wiki-account)))))
            (if (< (length email) (form-field-min-len :email))
                (push :small-mail pref-update-failed))
            (unless pref-update-failed
              ;; Everything looks ok, issue preference update.
              (wiki-account-update (current-wiki-account) newacc)
              (setq pref-update-done t))))
        (with-html-template ()
          ;; Process previously passed diagnostics.
          (if pref-update-failed
              (htm
               (:div
                :class "error-box"
                (:div :class "head" "Preferences Update Failed!")
                (:div
                 :class "body"
                 (:p "One or more form fields are missing or erronously
typed.")
                 (:p
                  (:u "Error Context:")
                  (:ul
                   (mapc
                    #'(lambda (err)
                        (htm
                         (:li
                          (case err
                            (:small-pass
                             (fmt "Account password must be bigger than ~a
                               characters."
                                  (form-field-min-len :password)))
                            (:newpass-mismatch
                             (str "Typed new password must be identical in
                               both inputs."))
                            (:wrong-pass
                             (str "Typed old password couldn't be validated."))
                            (:small-mail
                             (fmt "Typed e-mail address must be bigger than ~a
                               characters."
                                  (form-field-min-len :email)))))))
                    pref-update-failed)))))))
          (if pref-update-done
              (htm
               (:div :class "info-box"
                     (:div :class "head" "Edit Done")
                     (:div :class "body" "Account preferences has been
                   updated succesfully."))))
          ;; Nothing special, display preferences form.
          (:p "You can use below form to modify your account preferences.")
          (:script
           :type "text/javascript"
           (fmt "<!--
var minuserlen = ~a;
var minpasslen = ~a;
var minmaillen = ~a;

function checkFormFields() {
    if (document.prefs.alterpass.checked &&
        document.prefs.passnew1.value != document.prefs.passnew2.value) {
        window.alert('Typed new password must be identical in both inputs.');
        return false;
    }

    if (document.prefs.alterpass.checked &&
        document.prefs.passnew1.value.length < minpasslen) {
        window.alert('Account password must be bigger than '+
                     minpasslen+'characters.');
        return false;
    }

    if (document.prefs.email.value.length < minmaillen) {
        window.alert('Typed e-mail address must be bigger than '+
                     minmaillen+' characters.');
        return false;
    }

    return true;
}
-->"
                (form-field-min-len :username)
                (form-field-min-len :password)
                (form-field-min-len :email)))
          (:form
           :name "prefs"
           :onsubmit "return checkFormFields()"
           :action (wiki-path-to :uri (current-wiki-path))
           :method "post"
           (:table
            :class "fancy"
            (:tr (:th :colspan "3" "Account Preferences"))
            (:tr
             (:td "Username")
             (:td ":")
             (:td (str (wiki-account-username (current-wiki-account)))))
            (:tr
             (:td "Change Password")
             (:td ":")
             (:td
              (:script
               :type "text/javascript"
               "<!--
function enablePassForms() {
   visibility = !document.prefs.alterpass.checked;
   document.prefs.passold.disabled = visibility;
   document.prefs.passnew1.disabled = visibility;
   document.prefs.passnew2.disabled = visibility;
}
-->")
              (:input
               :type "checkbox" :name "alterpass"
               :onchange "enablePassForms()")))
            (:tr
             (:td "Old Password")
             (:td ":")
             (:td (:input :type "password" :name "passold"
                                           :maxlength (form-field-max-len :password)
                                           :disabled t)))
            (:tr
             (:td "New Password" (:sup "1"))
             (:td ":")
             (:td (:input :type "password" :name "passnew1"
                                           :maxlength (form-field-max-len :password)
                                           :disabled t)))
            (:tr
             (:td "New Password (Repeat)")
             (:td ":")
             (:td (:input :type "password" :name "passnew2"
                                           :maxlength (form-field-max-len :password)
                                           :disabled t)))
            (:tr
             (:td "E-Mail" (:sup "2"))
             (:td ":")
             (:td (:input
                   :type "text" :name "email"
                   :maxlength (form-field-max-len :email)
                   :value (wiki-account-email (current-wiki-account)))))
            (:tr
             (:td
              :colspan "3"
              :style "padding-left: 32px"
              (:input :type "submit" :name "editreq" :value "Apply Modifications")
              "&nbsp;"
              (:input :type "reset" :value "Reset Form"))))
           (:p (:sup "1")
               (:span :class "comment"
                      (fmt " Minimum ~a characters are required."
                           (form-field-min-len :password)))
               (:br)
               (:sup "2")
               (:span :class "comment"
                      (fmt " Minimum ~a characters are required."
                           (form-field-min-len :email))))))))))

(defun account-create ()
  "`create' action handler for `account' type wiki paths."
  (with-http-parameters (:default-request-type :post)
      ((createreq :parameter-type 'boolean)
       username password1 password2 email)
    ;; Resize form fields to their bounds.
    (form-field-resize :username username
                       :password password1
                       :email email)
    (let (failed-parts)
      ;; Evaluate create account request.
      (when createreq
        ;; Check form fields.
        (if (< (length username) (form-field-min-len :username))
            (push :small-username failed-parts))
        (if (string= password1 password2)
            (if (< (length password1) (form-field-min-len :password))
                (push :small-password failed-parts))
            (push :password-mismatch failed-parts))
        (if (< (length email) (form-field-min-len :email))
            (push :small-email failed-parts))
        ;; If form fields are ok, proceed account creation.
        (if (null failed-parts)
            (let ((account
                   (wiki-account-from
                    :plist (list :username username
                                 :password password1
                                 :email email))))
              (with-wiki-account-lock (username)
                (cond
                  ;; Check whether requested account already exists.
                  ((wiki-account-exists-p account)
                   (push :account-exists failed-parts))
                  (t
                   (wiki-account-create account)
                   (parametrized-redirect
                    (wiki-path-from :label "account:SignIn")
                    :account-created t)))))))
      (with-html-template ()
        ;; Did everything go ok during account creation?
        (if failed-parts
            (htm
             (:div
              :class "error-box"
              (:div :class "head" "Invalid Form Fields!")
              (:div
               :class "body"
               (:p "One or more supplied form fields are invalid. Please correct
them and try again.")
               (:p (:u "Error Context:"))
               (:ul
                (mapc
                 #'(lambda (part)
                     (htm
                      (:li
                       (case part
                         (:small-username
                          (fmt "Typed username (`~a') cannot be smaller than
                            ~a characters."
                               username
                               (form-field-min-len :username)))
                         (:small-password
                          (fmt "Typed password cannot be smaller than ~a
                            characters."
                               (form-field-min-len :password)))
                         (:password-mismatch
                          (str "Typed passwords must be identical."))
                         (:account-exists
                          (fmt "An account of typed username (`~a') already
                            exists."
                               username))))))
                 failed-parts))))))
        "You can use below form to create a new wiki account. With a wiki account,
you'll be able to create new content and modify existing contents."
        (:script
         :type "text/javascript"
         (fmt "<!--
var minuserlen = ~a;
var minpasslen = ~a;
var minmaillen = ~a;

function validateForm() {
    if (document.prefs.username.value.length < minuserlen) {
        window.alert('Typed username cannot be smaller than '+
                     minuserlen+' characters.');
        return false;
    }

    if (document.prefs.password1.value != document.prefs.password2.value) {
        window.alert('Typed passwords have to be identical.');
        return false;
    }

    if (document.prefs.password1.value.length < minpasslen) {
        window.alert('Typed password cannot be smaller than '+
                     minpasslen+' characters.');
        return false;
    }

    if (document.prefs.email.value.length < minmaillen) {
        window.alert('Typed e-mail address cannot be smaller than '+
                     minmaillen+' characters.');
        return false;
    }

    return true;
-->"
              (form-field-min-len :username)
              (form-field-min-len :password)
              (form-field-min-len :email)))     
        (:form
         :name "prefs"
         :method "post"
         :action (wiki-path-to :uri (current-wiki-path))
         :onsubmit "return validateForm()"
         (:table
          :class "fancy"
          (:tr (:th :colspan "3" "New Account Preferences"))
          (:tr
           (:td "Username")
           (:td ":")
           (:td (:input :type "text" :name "username"
                                     :maxlength (form-field-max-len :username)
                                     :value (str username))))
          (:tr
           (:td "Password")
           (:td ":")
           (:td (:input :type "password" :name "password1"
                                         :maxlength (form-field-max-len :password))))
          (:tr
           (:td "Password (Repeat)")
           (:td ":")
           (:td (:input :type "password" :name "password2"
                                         :maxlength (form-field-max-len :password))))
          (:tr
           (:td "E-Mail")
           (:td ":")
           (:td (:input :type "text" :name "email"
                                     :maxlength (form-field-max-len :email)
                                     :value (str email))))
          (:tr
           (:td
            :colspan "3"
            :style "padding-left: 32px"
            (:input :type "submit" :name "createreq" :value "Create Account")
            "&nbsp;"
            (:input :type "reset" :value "Reset Form")))))))))