(define-module (header)
  #:use-module (rde features mail)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  )

;; NOWEB HEADER
(define* (mail-lst id fqda urls)
  "Make a simple mailing-list."
  (mailing-list
   (id   id)
   (fqda fqda)
   (config (l2md-repo ;; XXX maildir issue might be here
            (name (symbol->string id))
            (urls urls)))))
;; NOWEB HEADER

(define-public my-hosts '()
  ;; ixy
  ;; live
  )


(define-public my-user "samuel")
(define-public my-timezone "Europe/Berlin")

(define-public my-feature-mail-settings
  (feature-mail-settings
   #:mail-accounts
   (list
    (mail-account
     (id   'personal)
     (fqda "samuel@samuelculpepper.com")
     (type 'bravehost))
    (mail-account
     (id   'work)
     (fqda "sculpepper@newstore.com")
     (type 'gmail-tls))
    (mail-account
     (id   'work-gcp)
     (fqda "sculpepper@newstore.net")
     (type 'gmail-tls))
    (mail-account
     (id   'yt)
     (fqda "imzaniiyt@gmail.com")
     (type 'gmail-tls)))
   #:mailing-lists
   (list
    ;; https://public-inbox.org/README.html
    (mail-lst 'public-inbox-meta "meta@public-inbox.org"
              '("https://public-inbox.org/meta"
                "nntps://news.public-inbox.org/inbox.comp.mail.public-inbox.meta"
                "imaps://news.public-inbox.org/inbox.comp.mail.public-inbox.meta.0"))

     ;;;; source: https://mail.python.org/archives/list/speed@python.org/latest
     ;;;;  -> mbox: https://mail.python.org/archives/list/speed@python.org/export/speed@python.org-2022-02.mbox.gz?start=1970-01-01&end=2022-02-21
     ;;;; (mail-lst 'python-speed "speed@python.org"
     ;;;;           '("https://mail.python.org/mailman/listinfo/speed"
     ;;;;             "https://mail.python.org/archives/list/speed@python.org/"))
    ;;
     ;;;; (mail-lst 'rde-announce "~acbdw/rde-announce@lists.sr.ht"
     ;;;;           '("https://lists.sr.ht/~abcdw/rde-announce/export"))
     ;;;; (mail-lst 'rde-discuss "~acbdw/rde-discuss@lists.sr.ht"
     ;;;;           '("https://lists.sr.ht/~abcdw/rde-discuss"))
     ;;;; (mail-lst 'rde-devel "~acbdw/rde-devel@lists.sr.ht"
     ;;;;           '("https://lists.sr.ht/~abcdw/rde-devel"))
     ;;;;; emacs
    ;;(mail-lst 'emacs-org-mode "emacs-orgmode@gnu.org"
    ;;          '("https://yhetil.org/orgmode"))
    ;;(mail-lst 'emacs-bugs "bug-gnu-emacs@gnu.org"
    ;;          '("https://yhetil.org/emacs-bugs"))
    ;;
    ;;
    ;;(mail-lst 'emacs-hyperbole "bug-hyperbole@gnu.org"
    ;;          '("https://lists.gnu.org/archive/mbox/bug-hyperbole"
    ;;            "https://lists.gnu.org/archive/html/bug-hyperbole"))
    ;;(mail-lst 'emacs-hyperbole-users "hyperbole-users@gnu.org"
    ;;          '("https://lists.gnu.org/archive/mbox/hyperbole-users"
    ;;            "https://lists.gnu.org/archive/html/hyperbole-users"))
    ;;
    ;;(mail-lst 'guix-bugs "guix-bugs@gnu.org"
    ;;          '("https://yhetil.org/guix-bugs/0"))
    ;;(mail-lst 'guix-devel "guix-devel@gnu.org"
    ;;          '("https://yhetil.org/guix-devel/0"))
    ;;(mail-lst 'guix-patches "guix-patches@gnu.org"
    ;;          '("https://yhetil.org/guix-patches/1"))
    )))

(define-public my-feature-user-info
  (feature-user-info
   #:emacs-advanced-user? #t
   #:user-name my-user
   #:full-name "Samuel Culpepper"
   #:email "samuel@samuelculpepper.com"
   #:user-groups '(;; basic groups
                   "lp" "wheel" "video"
                   ;;; qmk, zsa
                   ;; "plugdev" ;; FIXME ??? supplementary group [...] undefined
                   )
   ))

(define-public my-feature-gnupg
  (feature-gnupg
   #:gpg-primary-key "EE20E25391AAB9BB"))
