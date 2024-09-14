(define-module (configs)

  #:use-module (header)
  #:use-module (emacs)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module (gnu home services) ; nope
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services ssh)

  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages)

  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services ssh)
  #:use-module (gnu services xorg)
  #:use-module (gnu services nfs)

  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system shadow) ; account-service-type

  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix utils)

  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)

  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features bittorrent)
  #:use-module (rde features bluetooth)
  #:use-module (rde features clojure)
  #:use-module (rde features docker)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features fontutils)
  #:use-module (rde features gnupg)
  #:use-module (rde features keyboard)
  #:use-module (rde features linux)
  #:use-module (rde features mail)
  #:use-module (rde features markup)
  #:use-module (rde features matrix)
  #:use-module (rde features messaging)
  #:use-module (rde features networking)
  #:use-module (rde features password-utils)
  #:use-module (rde features presets)
  #:use-module (rde features python)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features ssh)
  #:use-module (rde features system)
  #:use-module (rde features terminals)
  #:use-module (rde features uml)
  #:use-module (rde features version-control)
  #:use-module (rde features video)
  #:use-module (rde features virtualization)
  #:use-module (rde features web-browsers)
  #:use-module (rde features wm)
  #:use-module (rde features xdg)
  #:use-module (rde features xdisorg)
  ;;#:use-module (rde features irc)

  #:use-module (rde gexp)

  #:use-module (rde home services i2p)
  #:use-module (rde home services emacs)

  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (rde packages)

  #:use-module (rde system services accounts)

  ;;#:use-module (gnu home-services shellutils)

  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu services nvidia)

  ;;#:use-module (rde features bluetooth) ;; TODO qzdl

  #:use-module (srfi srfi-1))
(use-modules (rde features mail))

(define bravehost-folder-mapping
  '(("inbox"  . "INBOX")
    ("accounts" . "INBOX/Accounts")
    ("cv" . "INBOX/CV")
    ("info" . "INBOX/info")
    ("sent"   . "Sent")
    ("drafts" . "Drafts")
    ("trash"  . "Deleted Items")
    ("spam"   . "Junk")))

;; https://wiki.bravenet.com/Using_your_Bravenet_e-mail_account
(define bravehost-isync-settings
  (generate-isync-serializer "mail.bravehost.com" bravehost-folder-mapping))

(define gmail-tls-folder-mapping
  '(("inbox"   . "INBOX")
    ("sent"    . "[Gmail]/Sent Mail")
    ("drafts"  . "[Gmail]/Drafts")
    ("archive" . "[Gmail]/All Mail")
    ("trash"   . "[Gmail]/Trash")
    ("spam"    . "[Gmail]/Spam")))

(define gmail-tls-isync-settings
  (generate-isync-serializer "imap.gmail.com" gmail-tls-folder-mapping))
(define %thinkpad-layout
  (keyboard-layout
   "us" "altgr-intl"
   #:model "thinkpad"
   #:options '("ctrl:nocaps")))
(define %extra-zshrc
  (list ;; XXX higher level category
   ;; something which evals equiv to following for each promptline "PS1=\"[$(date -u '+%Y-%m-%d | %H:%M')] $PS1\""
   "alias ns='cd $HOME/git/ns'"
   "alias om='ns && cd om'"
   "alias omom='om && cd om'"
   "alias rt='ns && cd routing'"
   "alias sys='cd $HOME/git/sys'"

   ;; TIL https://unix.stackexchange.com/questions/225943/except-the-1st-argument
   "rgw() { d=$1; p=$2; argv[1,2]=(); rg $p $d $@; }"
   "alias rgg='rgw $HOME/git/'"
   "alias rgr='rgw $HOME/git/sys/rde'"
   "alias rgns='rgw $HOME/git/ns'"
   "alias rgom='rgw $HOME/git/ns/om'"
   "alias rgrt='rgw $HOME/git/ns/routing'"
   "alias rgsys='rgw $HOME/git/sys'"

   "alias gp='ls $GUIX_EXTRA_PROFILES'"
   "_gP() { export GUIX_PROFILE=$1 ; }"
   "alias gP='_gP'"
   "_gsP() { . $GUIX_EXTRA_PROFILES/$1/$1 ; }"
   "gsP=_gsP"
   ))
(define my-org-directory "~/life")
(define my-notes-directory
  (string-append my-org-directory "/roam"))
(define gaming? #f)

;;; Generic features should be applicable for various hosts/users/etc

(define* (pkgs #:rest lst)
  (map specification->package+output lst))

(define* (pkgs-vanilla #:rest lst)
  "Packages from guix channel."
  (define channel-guix
    (list (channel
           (name 'guix)
           (url "https://git.savannah.gnu.org/git/guix.git")
           ;;;; [2023-07-13 Thu 09:46]
           (commit "2794caed7c813f2ec4249236de36eaccafee8361"))))

  (define inferior (inferior-for-channels channel-guix))
  (define (get-inferior-pkg pkg-name)
    (car (lookup-inferior-packages inferior pkg-name)))

  (map get-inferior-pkg lst))

(use-modules
 (gnu packages)
 (guix packages)
 (guix gexp)
 (guix utils)
 (guix download)
 (guix git-download)
 (gnu packages emacs)
 (gnu packages emacs-xyz)
 (guix build-system emacs)
 (guix build-system gnu)
 ((guix licenses) #:prefix license:))

(define-public emacs-sql-indent
  (package
   (name "emacs-sql-indent")
   (version "1.6")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://elpa.gnu.org/packages/sql-indent-"
           version
           ".tar"))
     (sha256
      (base32 "000pimlg0k4mrv2wpqq8w8l51wpr1lzlaq6ai8iaximm2a92ap5b"))))
   (build-system emacs-build-system)
   (home-page "https://github.com/alex-hhh/emacs-sql-indent")
   (synopsis "Support for indenting code in SQL files.")
   (description
    "`sqlind-minor-mode' is a minor mode that enables syntax-based indentation for
`sql-mode' buffers: the TAB key indents the current line based on the SQL code
on previous lines.  To setup syntax-based indentation for every SQL buffer, add
`sqlind-minor-mode' to `sql-mode-hook'.  Indentation rules are flexible and can
be customized to match your personal coding style.  For more information, see
the \"sql-indent.org\" file.

The package also defines align rules so that the `align' function works for SQL
statements, see `sqlind-align-rules'.")
   (license license:gpl3+)))
(define-public emacs-org-ml
  (package
   (name "emacs-org-ml")
   (version "20220711.1528")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/ndwarshuis/org-ml.git")
                  (commit "385e3bee497f858705144d7ab5e6570d31d3ffe8")))
            (sha256
             (base32
              "0j506lp3lgf9iz94ag041bpdcr837j5lmbazq7v3brblm43dvh9p"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-org emacs-dash emacs-s))
   (home-page "https://github.com/ndwarshuis/org-ml")
   (synopsis "Functional Org Mode API")
   (description
    "This is a functional API for org-mode primarily using the `org-element' library.
`org-element.el' provides the means for converting an org buffer to a parse-tree
data structure.  This library contains functions to modify this parse-tree in a
more-or-less 'purely' functional manner (with the exception of parsing from the
buffer and writing back to the buffer).  For the purpose of this package, the
resulting parse tree is composed of 'nodes'.

This library exposes the following types of functions: - builder: build new
nodes to be inserted into a parse tree - property functions: return either
property values (get) or nodes with   modified properties (set and map) -
children functions: return either children of nodes (get) or return a node
with modified children (set and map) - node predicates: return t if node meets a
condition - pattern matching: return nodes based on a pattern that matches the
parse   tree (and perform operations on those nodes depending on the function) -
parsers: parse a buffer (optionally at current point) and return a parse   tree
- writers: insert/update the contents of a buffer given a parse tree

For examples please see full documentation at:
https://github.com/ndwarshuis/org-ml")
   (license license:gpl3+)))
(define-public emacs-moldable-emacs
  (package
    (name "emacs-moldable-emacs")
    (version "20220825.0037")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ag91/moldable-emacs")
             (commit "53f8b3af4572ab12be9f1f96da848278507ef350")))
       (sha256
        (base32 "1jcac4hiyh98q8cvim6yjaw1xihsy3r5lnjhijr3p89z2bv481xl"))))
    (arguments
     (list
      ;; #:tests? #true
      ;; #:test-command
      ;; #~(list "emacs" "-Q" "--batch"
      ;;         "-L" "test"
      ;;         "--load" "test/testein-loader.el")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'install-molds
            (lambda _
              (for-each
               (lambda (f)
                 (install-file
                  f (string-append (elpa-directory #$output)
                                   "/molds")))
               (find-files "./molds" ".*"))
              (for-each
               (lambda (f)
                 (install-file
                  f (string-append (elpa-directory #$output)
                                   "/tutorials")))
               (find-files "./tutorials" ".*")))))))
    (build-system emacs-build-system)
   ;;; propagated (external)
    ;; (check these via the mold “WhatMoldsCanIUse?”)
    ;; graph-cli
    ;; graphviz
    ;; imgclip
    ;; emacs-csv-mode OPTIONAL (buffer size to bar chart)
    (propagated-inputs
     (list emacs-dash
           emacs-s
           emacs-async
           ;; emacs-thunk builtin
           emacs-esxml
           emacs-org-ql
           ;; emacs-tree-sitter
           ;; emacs-code-compass
           ))
    (home-page "https://github.com/ag91/moldable-emacs")

    (synopsis "TODO")
    (description
     "TODO")
    (license license:gpl3+)))

(define-public emacs-ob-go
  (package
   (name "emacs-ob-go")
   (version "20190201.214")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/pope/ob-go")
           (commit "2067ed55f4c1d33a43cb3f6948609d240a8915f5")))
     (sha256
      (base32 "069w9dymiv97cvlpzabf193nyw174r38lz5j11x23x956ladvpbw"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-org))
   (home-page "https://github.com/pope/ob-go")
   (synopsis "Org-Babel support for evaluating go code.")
   (description
    "@code{ob-go} enables @{Org-Babel} support for evaluating @code{go}
code. It was created based on the usage of @code{ob-C}. The @code{go}
code is compiled and run via the @code{go run} command. If a
@code{main} function isn’t present, by default the code is wrapped in
a simple @{main func}. If @code{:package} option isn’t set, and no
package is declared in the code, then the @code{main package} is
declared.")
   (license license:gpl3+)))
(define-public emacs-ox-jira
  (package
   (name "emacs-ox-jira")
   (version "20220423.1403")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/stig/ox-jira.el.git")
                  (commit "00184f8fdef02a3a359a253712e8769cbfbea3ba")))
            (sha256
             (base32
              "1zyq4d0fvyawvb3w6072zl4zgbnrpzmxlz2l731wqrgnwm0l80gy"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-org))
   (home-page "https://github.com/stig/ox-jira.el")
   (synopsis "JIRA Backend for Org Export Engine")
   (description
    "This module plugs into the regular Org Export Engine and transforms Org files to
JIRA markup for pasting into JIRA tickets & comments.

In an Org buffer, hit `C-c C-e j j' to bring up *Org Export Dispatcher* and
export it as a JIRA buffer.  I usually use `C-x h' to mark the whole buffer,
then `M-w' to save it to the kill ring (and global pasteboard) for pasting into
JIRA issues.")
   (license license:gpl3+)))

(define-public emacs-kubernetes
  (package
   (name "emacs-kubernetes")
   (version "20220715.1717")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/kubernetes-el/kubernetes-el.git")
                  (commit "8163fd38015cbde0485f6eaab41450132bf6e19d")))
            (sha256
             (base32
              "06p5qz4h5ar86vv4nzpw08x18fjvs2zg5brx55h80hjdgr89b771"))))
   (build-system emacs-build-system)
   (inputs (list emacs-magit
                 emacs-magit-popup
                 emacs-dash
                 emacs-with-editor
                 emacs-request
                 emacs-s
                 emacs-transient))
   (arguments
    '(#:include '("^[^/]+.el$" "^[^/]+.el.in$"
                  "^dir$"
                  "^[^/]+.info$"
                  "^[^/]+.texi$"
                  "^[^/]+.texinfo$"
                  "^doc/dir$"
                  "^doc/[^/]+.info$"
                  "^doc/[^/]+.texi$"
                  "^doc/[^/]+.texinfo$")
                #:exclude '("^.dir-locals.el$" "^test.el$" "^tests.el$" "^[^/]+-test.el$"
                            "^[^/]+-tests.el$" "^kubernetes-evil.el$")))
   (home-page "https://github.com/kubernetes-el/kubernetes-el")
   (synopsis "Magit-like porcelain for Kubernetes")
   (description
    "kubernetes-el is a text-based, interactive management interface for managing
Kubernetes clusters within Emacs.")
   (license license:gpl3+)))
(define-public emacs-ox-slack
  (package
   (name "emacs-ox-slack")
   (version "20200108.1546")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/titaniumbones/ox-slack.git")
                  (commit "bd797dcc58851d5051dc3516c317706967a44721")))
            (sha256
             (base32
              "1kh2v08fqmsmfj44ik8pljs3fz47fg9zf6q4mr99c0m5ccj5ck7w"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-org emacs-ox-gfm))
   (home-page "https://github.com/titaniumbones/ox-slack")
   (synopsis "Slack Exporter for org-mode")
   (description
    "This library implements a Slack backend for the Org exporter, based on the `md
and `gfm back-ends.")
   (license license:gpl3+)))
(define-public emacs-svg-clock
  (package
   (name "emacs-svg-clock")
   (version "1.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://elpa.gnu.org/packages/svg-clock-"
                                version ".el"))
            (sha256
             (base32
              "15pmj07wnlcpv78av9qpnbfwdjlkf237vib8smpa7nvyikdfszfr"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-svg-lib))
   (home-page "http://elpa.gnu.org/packages/svg-clock.html")
   (synopsis "Analog clock using Scalable Vector Graphics")
   (description
    "svg-clock provides a scalable analog clock.  Rendering is done by means of svg
(Scalable Vector Graphics).  In order to use svg-clock you need to build Emacs
with svg support. (To check whether your Emacs supports svg, do \"M-:
(image-type-available-p svg) RET\" which must return t).  Call `svg-clock to
start a clock.  This will open a new buffer \"*clock*\" displaying a clock which
fills the buffer's window.  Use `svg-clock-insert to insert a clock
programmatically in any buffer, possibly specifying the clock's size, colours
and offset to the current-time.  Arbitrary many clocks can be displayed
independently.  Clock instances ared updated automatically.  Their resources
(timers etc.) are cleaned up automatically when the clocks are removed.")
   (license license:gpl3+)))
(define-public emacs-consult-recoll
  (package
    (name "emacs-consult-recoll")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://elpa.gnu.org/packages/consult-recoll-" version
                    ".tar"))
              (sha256
               (base32
                "02vg1rr2fkcqrrivqgggdjdq0ywvlyzazwq1xd02yah3j4sbv4ag"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-consult))
    (home-page "https://codeberg.org/jao/consult-recoll")
    (synopsis "Recoll queries using consult")
    (description "This package provides an emacs interface to perform recoll queries, and display its results, via consult. It is also recommened that you use a a package for vertical display of completions that works well with consult, such as vertico.")
    (license license:gpl3+)))
;; :var props=guix-bq() :noweb yes
;; ^^ wow

(define-public emacs-bigquery-mode
  (package
    (name "emacs-bigquery-mode")
    (version "20200412.155")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/christophstockhusen/bigquery-mode")
             (commit "308bdccf194f1bca230de3a070459836e30b502a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w1b2d64hdzxk58lq0zfpfavmi8w80lmgqybvgfkl7l3mhnwzgj2"))))
    (propagated-inputs
     (list emacs-sql-indent))
    (build-system emacs-build-system)
    (home-page "https://github.com/christophstockhusen/bigquery-mode")
    (synopsis "Major mode for interacting with Google Cloud Platform's BigQuery")
    (description "Major mode for interacting with Google Cloud Platform's BigQuery")
    (license license:gpl3+)))

(define-public emacs-dogears
  (package
  (name "emacs-dogears")
  (version "20220829.441")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/alphapapa/dogears.el.git")
                  (commit "5b8a85d03ca17d8b8185868fdbacf320784026d5")))
            (sha256
             (base32
              "0h4gh4ja9dnslj286skc8nzp9dvpyp53ig9y4kniq5lji6gn3r1f"))))
  (build-system emacs-build-system)
  (propagated-inputs (list emacs-map))
  (arguments
   '(#:include '("^[^/]+.el$" "^[^/]+.el.in$"
                 "^dir$"
                 "^[^/]+.info$"
                 "^[^/]+.texi$"
                 "^[^/]+.texinfo$"
                 "^doc/dir$"
                 "^doc/[^/]+.info$"
                 "^doc/[^/]+.texi$"
                 "^doc/[^/]+.texinfo$")
     #:exclude '("^.dir-locals.el$" "^test.el$" "^tests.el$" "^[^/]+-test.el$"
                 "^[^/]+-tests.el$" "^helm-dogears.el$")))
  (home-page "https://github.com/alphapapa/dogears.el")
  (synopsis "Never lose your place again")
  (description
   "This library automatically and smartly remembers where you've been, in and
across buffers, and helps you quickly return to any of those places.  It uses
the Emacs bookmarks system internally (but without modifying the
bookmarks-alist) to save and restore places with mode-specific functionality.")
  (license license:gpl3+)))
(define-public emacs-chatgpt-shell
  (package
    (name "emacs-chatgpt-shell")
    (version "2023-04-21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xenodium/chatgpt-shell")
             (commit "0ae307045fdad451f7a6e2a56ffec70c6300b8da")))
       (sha256
        (base32 "1s7pn8v79x2l02cj41z89f8rda2xd3naq6yv8vjzd0z4kag3jmzv"))))
    (build-system emacs-build-system)
    (home-page "")
    (synopsis "A minimal ChatGPT Emacs shel")
    (description
     "A minimal ChatGPT Emacs shell")
    (license license:gpl3+)))
(define-public emacs-ytdl-next
  (package
    (name "emacs-ytdl")
    (version "20230331.1804")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/tuedachu/ytdl.git")
                    (commit "2ea3daf2f6aa9d18b71fe3e15f05c30a56fca228")))
              (sha256
               (base32
                "0y62lkgsg19j05dpd6sp6zify8vq8xvpc8caqiy4rwi7p4ahacsf"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-async emacs-transient emacs-dash))
    (home-page "https://gitlab.com/tuedachu/ytdl")
    (synopsis "Emacs Interface for youtube-dl")
    (description
     "ytdl.el is an Emacs-based interface for youtube-dl, written in emacs-lisp.
youtube-dl is a command-line program to download videos from YouTube and a few
more sites.  More information at https://yt-dl.org.  youtube-dl supports many
more sites: PeerTube, BBC, IMDB, InternetVideoArchive (non-exhaustive list) *
Setup Add \"(require ytdl)\" to your \"init.el\" file.  Further customization can be
found in the documentation online.")
    (license license:gpl3+)))
(define-public emacs-selectric-mode
  (package
    (name "emacs-selectric-mode")
    (version "20200209.2107")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rbanffy/selectric-mode.git")
                    (commit "1840de71f7414b7cd6ce425747c8e26a413233aa")))
              (sha256 (base32
                       "1aabqqqcafkqmyarf5kb1k0gglmlpn6kr3h3x0yph5gd6sk3l4ll"))))
    (build-system emacs-build-system)
    (arguments '(#:include '("^[^/]+.el$" "^[^/]+.el.in$"
                             "^dir$"
                             "^[^/]+.info$"
                             "^[^/]+.texi$"
                             "^[^/]+.texinfo$"
                             "^doc/dir$"
                             "^doc/[^/]+.info$"
                             "^doc/[^/]+.texi$"
                             "^doc/[^/]+.texinfo$"
                             "^[^/]+.wav$")
                 #:exclude '("^.dir-locals.el$" "^test.el$" "^tests.el$"
                             "^[^/]+-test.el$" "^[^/]+-tests.el$")))
    (home-page "https://github.com/rbanffy/selectric-mode")
    (synopsis "IBM Selectric mode for Emacs")
    (description
     "This minor mode plays the sound of an IBM Selectric typewriter as you type.")
    (license license:gpl3+)))
(define-public emacs-atomic-chrome
  (package
   (name "emacs-atomic-chrome")
   (version "20230304.112")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/alpha22jp/atomic-chrome.git")
                  (commit "f1b077be7e414f457191d72dcf5eedb4371f9309")))
            (sha256 (base32
                     "01024ikcy23hkxjpy6qlsa8sj3cyf4p3igx5i31qkq21dm7b8xqv"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-let-alist emacs-websocket))
   (home-page "https://github.com/alpha22jp/atomic-chrome")
   (synopsis "Edit Chrome text area with Emacs using Atomic Chrome")
   (description
    "This is the Emacs version of Atomic Chrome which is an extension for Google
Chrome browser that allows you to edit text areas of the browser in Emacs.  It's
similar to Edit with Emacs, but has some advantages as below with the help of
websocket. * Live update The input on Emacs is reflected to the browser
instantly and continuously. * Bidirectional communication You can edit both on
the browser and Emacs, they are synced to the same.  Firefox is supported via
the @code{GhostText} browser addon.")
   (license license:gpl3+)))
(define-public emacs-spacious-padding
  (package
   (name "emacs-spacious-padding")
   (version "0.3.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://elpa.gnu.org/packages/spacious-padding-" version
                  ".tar"))
            (sha256 (base32
                     "1iiaxgl18k8hkwwyz3qnargv6q8kwzyh39ai46k5xbpmyrsj4b16"))))
   (build-system emacs-build-system)
   (home-page "https://git.sr.ht/~protesilaos/spacious-padding")
   (synopsis "Increase the padding/spacing of frames and windows")
   (description
    "# spacious-padding for GNU Emacs DEMO IMAGES: -
<https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/> -
<https://protesilaos.com/codelog/2023-11-15-spacious-padding-extra-ui-dev/> This
package provides a global minor mode to increase the spacing/padding of Emacs
windows and frames.  The idea is to make editing and reading feel more
comfortable.  Enable the mode with `M-x spacious-padding-mode`.  Adjust the
exact spacing values by modifying the user option `spacious-padding-widths`.
Inspiration for this package comes from [Nicolas Rougier's impressive
designs](https://github.com/rougier) and [Daniel Mendler's `org-modern`
package](https://github.com/minad/org-modern). + Package name (GNU ELPA):
`spacious-padding` + Official manual:
<https://protesilaos.com/emacs/spacious-padding> + Change log:
<https://protesilaos.com/emacs/spacious-padding> + Git repo on @code{SourceHut}:
<https://git.sr.ht/~protesilaos/spacious-padding> - Mirrors: + @code{GitHub}:
<https://github.com/protesilaos/spacious-padding> + @code{GitLab}:
<https://gitlab.com/protesilaos/spacious-padding> + Mailing list:
<https://lists.sr.ht/~protesilaos/general-issues> + Sample images: -
<https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/> -
<https://protesilaos.com/codelog/2023-11-15-spacious-padding-extra-ui-dev/> +
Backronyms: Space Perception Adjusted Consistently Impacts Overall Usability
State ...  padding; Spacious ...  Precise Adjustments to Desktop Divider
Internals Neatly Generated.")
   (license license:gpl3+)))
(define-public emacs-org-noter
  (package
   (name "emacs-org-noter")
   (version "20240509.1756")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/org-noter/org-noter.git")
           (commit "68646b685a0d8c02419234922a9e2d885d6419df")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "17kgy2b89cc0v3q49pgrp6x46f2908v6vscxg4pczf6zc83rmwzp"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-org emacs-org-roam))
   (arguments
    '(#:include '("^[^/]+.el$" "modules")
      #:exclude '("^[^/]+-test-utils.el$" "^[^/]+-devel.el$")))
   (home-page "https://github.com/org-noter/org-noter")
   (synopsis "A synchronized, Org-mode, document annotator")
   (description
    "The idea is to let you create notes that are kept in sync when you scroll
through the document, but that are external to it - the notes themselves live in
an Org-mode file.  As such, this leverages the power of Org-mode (the notes may
have outlines, latex fragments, babel, etc...) while acting like notes that are
made /in/ the document.  Also, I must thank Sebastian for the original idea and
inspiration! Link to the original Interleave package:
https://github.com/rudolfochrist/interleave.")
   (license #f)))
(define-public emacs-org-remark
  (package
   (name "emacs-org-remark")
   (version "1.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://elpa.gnu.org/packages/org-remark-" version
                         ".tar"))
     (sha256
      (base32 "01iprzgbyvbfpxp6fls4lfx2lxx7xkff80m35s9kc0ih5jlxc5qs"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-org))
   (home-page "https://github.com/nobiot/org-remark")
   (synopsis "Highlight & annotate text, Info, EPUB, EWW")
   (description
    "Org-remark lets you highlight and annotate text files, websites,
EPUB books and Info documentation using Org mode.")
   (license license:gpl3+)))
(use-modules (guix build-system python)  ; pypi-uri
             (gnu packages python-xyz)   ; python-lsp-server
             (gnu packages python-check) ; python-mypy
             (gnu packages python-build) ; python-toml
             (gnu packages check)        ; python coverage
             )

(define-public python-pylsp-mypy
  (package
   (name "python-pylsp-mypy")
   (version "0.6.3")
   (source (origin
            (method url-fetch)
            (uri (pypi-uri "pylsp-mypy" version))
            (sha256
             (base32
              "1gf865dj9na7jyp1148k27jafwb6bg0rdg9kyv4x4ag8qdlgv9h6"))))
   (build-system python-build-system)
   (propagated-inputs (list python-lsp-server
                            python-mypy
                            python-toml))
   (native-inputs (list python-coverage
                        python-pytest
                        python-pytest-cov
                        python-tox))
   (home-page "https://github.com/python-lsp/pylsp-mypy")
   (synopsis "Mypy linter for the Python LSP Server")
   (description "Mypy linter for the Python LSP Server")
   (license license:gpl3+)))
(use-modules
 (guix packages)
 (guix download)
 (guix gexp)
 (guix git-download)
 (guix build-system cargo)
 (guix build-system python)
 (gnu packages)
 (gnu packages python-crypto))

;; TODO upstream to python-crypto
;; TODO
;; * keyringrc.cfg
;; [backend]
;; default-keyring=keyring_pass.PasswordStoreBackend
;; [pass]
;; key-prefix=/path/to/pass/dir
;; OR, given pythonkeyring >=23.0.0, envar KEYRING_PROPERTY_PASS_KEY_PREFIX


(define-public python-keyring-pass
  (package
    (name "python-keyring-pass")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "keyring_pass" version))
        (sha256
          (base32 "0rqrsm0gxxvv6vkqyg3snf29m8q44ljsz63f3j2rkizry9csd1fl"))))
    (build-system python-build-system)
    (propagated-inputs (list python-keyring))
    (home-page "https://github.com/nazarewk/keyring_pass")
    (synopsis
      "https://www.passwordstore.org/ backend for https://pypi.org/project/keyring/")
    (description
      "https://www.passwordstore.org/ backend for https://pypi.org/project/keyring/")
    (license license:expat)))

;; TODO upgrade to 0.8.0 (needs jaraco-classes==4.0.0)
;; (define-public python-keyring-pass
;;   (package
;;   (name "python-keyring-pass")
;;   (version "0.8.0")
;;   (source (origin
;;             (method url-fetch)
;;             (uri (pypi-uri "keyring_pass" version))
;;             (sha256
;;              (base32
;;               "0dsjg5wv7n4gshpygnddg0w27fx7whdvwyms9h90xg5hqyxcpsdw"))))
;;   (build-system python-build-system)
;;   (propagated-inputs (list python-jaraco-classes python-keyring))
;;   (home-page "https://github.com/nazarewk/keyring_pass")
;;   (synopsis
;;    "https://www.passwordstore.org/ backend for https://pypi.org/project/keyring/")
;;   (description
;;    "https://www.passwordstore.org/ backend for https://pypi.org/project/keyring/")
;;   (license license:expat)))
(define %user-features
  (remove
   unspecified?
   (list
    my-feature-user-info
    my-feature-gnupg
    (feature-password-store)
    my-feature-mail-settings

    (feature-keyboard
     ;; To get all available options, layouts and variants run:
     ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
     #:keyboard-layout %thinkpad-layout))))
;;; TODO: feature-wallpapers https://wallhaven.cc/
;;; TODO: feature-icecat
;; PipeWire/iwd:
;; https://github.com/J-Lentz/iwgtk
;; https://github.com/krevedkokun/guix-config/blob/master/system/yggdrasil.scm
;; https://github.com/werman/noise-suppression-for-voice#pipewire

;;; WARNING: The order can be important for features extending
;;; services of other features.  Be careful changing it.

(define %main-features
  (remove
   (lambda (s) (or (not s) (unspecified? s)))
   (list
    ;;; BEGIN; main
    (feature-custom-services
     #:feature-name-prefix 'ixy
     ;; #:system-services
     ;; (list
     ;;  (simple-service 'nvidia-udev-rule udev-service-type
     ;;                  (list nvidia-driver)))
     #:home-services
     (list
      ((@ (gnu services) simple-service)
       'my-environment-variables
       (@ (gnu home services) home-environment-variables-service-type)
       `(
               ;;; GRAPHICS
         ;;("LIBGL_DRIVERS_PATH" . (string-join (list "/gnu/store/bg8mrp0ply34c76xq1i8b4hgjyh6hi8k-nvidia-driver-495.46/lib/gbm"
         ;;                                           "/gnu/store/bg8mrp0ply34c76xq1i8b4hgjyh6hi8k-nvidia-driver-495.46/lib"
         ;;                                           "/gnu/store/bg8mrp0ply34c76xq1i8b4hgjyh6hi8k-nvidia-driver-495.46") ":"))
         ;;("LIBGL_DEBUG" . "verbose")
         ;;("G_MESSAGES_DEBUG" . "1")
      
         ;;("MESA_LOADER_DRIVER_OVERRIDE" . "nvidia") ;; no nvidia_dri
         ;;("MESA_LOADER_DRIVER_OVERRIDE" . "nvidia-drm") ;; no nvidia-drm_dri
      
         ;;("MESA_DEBUG" . "1")
         ;;("MESA_LOG_FILE" . "/tmp/mesa.log")
      
         ;; glfw patched?
         ;; https://github.com/bulletphysics/bullet3/issues/2595#issuecomment-588080665
         ;; ("MESA_GL_VERSION_OVERRIDE" . "3.4")
         ;;("MESA_GLSL_VERSION_OVERRIDE" . "340")
      
         ;;("GBM_BACKEND" . "nvidia-drm") ;; [2024-01-28 Sun]
              ;;;; guix build --no-grafts -f /home/samuel/git/sys/nonguix/nongnu/packages/nvidia.scm | wl-copy
              ;;;; or
              ;;;; guix build nvidia-driver | wl-copy
         ;;,@(let ((driver-path "/gnu/store/cbj701jzy9dj6cv84ak0b151y9plb5sc-nvidia-driver-495.46"))
         ;;    `(("GBM_BACKENDS_PATH" . ,(string-join (list driver-path
         ;;                                                 (string-append driver-path "/lib")
         ;;                                                 (string-append driver-path "/lib/gbm")
         ;;                                                 "$PATH") ":"))
         ;;      ("VK_ICD_FILENAMES" . ,(string-append driver-path "/share/vulkan/icd.d/nvidia_icd.json"))
         ;;      ("LIBGL_DRIVERS_PATH" . ,(string-join (list driver-path
         ;;                                                  (string-append driver-path "/lib")
         ;;                                                  (string-append driver-path "/lib/gbm")
         ;;                                                  "$PATH") ":"))
         ;;      ;; https://github.com/NVIDIA/egl-wayland/issues/39#issuecomment-927288015
         ;;      ;; undocumented
         ;;      ;; might have an issue for containerised stuff, as set(uid|gid)
         ;;      ("__EGL_EXTERNAL_PLATFORM_CONFIG_DIRS" . ,(string-append driver-path "/share/egl/egl_external_platform.d"))
         ;;      ))
         ;;
         ;;("__GLX_VENDOR_LIBRARY_NAME" . "nvidia")
         ;;("WLR_NO_HARDWARE_CURSORS" . "1")
         ;;("WLR_DRM_NO_ATOMIC" . "1")
         ;; echo "/dev/dri/card$(udevadm info -a -n /dev/dri/card1 | grep boot_vga | rev | cut -c 2)"
         ;;("WLR_DRM_DEVICES" . "/dev/dri/card1")   ;; gpu only
         ;;("WLR_DRM_DEVICES" . "/dev/dri/card1") ;; cpu only
         ;;("WLR_DRM_DEVICES" . "/dev/dri/card0:/dev/dri/card1") ;; gpu:cpu
      
              ;;; GUILE
         ("GUILE_LOAD_PATH" .
          "$XDG_CONFIG_HOME/guix/current/share/guile/site/3.0\
      :$GUILE_LOAD_PATH")
         ("GUILE_LOAD_COMPILED_PATH" .
          "$XDG_CONFIG_HOME/guix/current/lib/guile/3.0/site-ccache\
      :$GUILE_LOAD_COMPILED_PATH")
      
              ;;; jsBABEL
         ;; javascript sucks, npm sucks
         ;; https://github.com/npm/npm/issues/6675#issuecomment-250318382
         ;; https://github.com/npm/cli/issues/1451
         ;; https://github.com/pnpm/pnpm/issues/2574
         ;; https://github.com/rxrc/zshrc/blob/3ca83703da5bd93b015747835a8a0164160c9b83/env.zsh#L33-L928
         ("NPM_CONFIG_USERCONFIG" . "${XDG_CONFIG_HOME}/npm/config")
         ("NPM_CONFIG_CACHE" . "${XDG_CACHE_HOME}/npm")
         ("NPM_CONFIG_TMP" . "${XDG_RUNTIME_DIR}/npm")
         ("YARN_CACHE_FOLDER" . "${YARN_CACHE_FOLDER:-$XDG_CACHE_HOME/yarn}")
         ("NODE_REPL_HISTORY" . "${NODE_REPL_HISTORY:-$XDG_CACHE_HOME/node/repl_history}")
         ("NVM_DIR" . "${NVM_DIR:-$XDG_DATA_HOME/nvm}")
         ("BABEL_CACHE_PATH" . "${BABEL_CACHE_PATH:-$XDG_CACHE_HOME/babel/cache.json}")
      
              ;;; DEVELOPMENT
         ("GUIX_CHECKOUT" . "$HOME/git/sys/guix")
         ("GUIX_EXTRA_PROFILES" . "$HOME/.guix-extra-profiles")
      
              ;;; ETC
         ("GDK_BACKEND" . "wayland") ;; ... for clipboarding emasc
         ;;("DISABLE_RTKIT" . "1") ;; TODO [2022-08-03] pipewire broken ;; commented [2022-08-11 Thu]
      
         ("XDG_DATA_DIRS" . ,(string-join
                              (list "$XDG_DATA_DIRS"
                                    ;;; flatpak: show paks alongside other apps
                                    ;; this sys path is not relevant for GUIX
                                    ;;"/var/lib/flatpak/exports/share"
                                    "/home/samuel/.local/share/flatpak/exports/share")
                              ":"))
         ;; TODO fix this path issue
         ("PATH" . ,(string-join
                     (list "$PATH"
                           "$HOME/go/bin"
                           "$HOME/.local/bin"
                           "$HOME/.krew/bin"
                           "${XDG_CACHE_HOME}/npm/bin")
                     ":"))))
      
      (simple-service
       'my-zshrc home-zsh-service-type
       (home-zsh-extension
        (zshrc
         (append %extra-zshrc
                 (list #~(format #f "" ;;#$example-program
                                 ))))))
      
      ;; ((@ (gnu services) simple-service)
      ;;  'extend-shell-profile
      ;;  (@ (gnu home-services shells) home-shell-profile-service-type)
      ;;  (list
      ;;   #~(string-append
      ;;      "alias superls="
      ;;      #$(file-append (@ (gnu packages base) coreutils) "/bin/ls"))))
          ;;; home jobs
      ;;
      ;; see logs at ~/.local/var/log/mcron.log
      ;;   tail --follow ~/.local/var/log/mcron.log
      ;;
      ;; see job spec at [[info:mcron#Guile Syntax][mcron#Guile Syntax]]
      ((@ (gnu services) simple-service)
       'home-jobs (@ (gnu home services mcron) home-mcron-service-type)
       (list
             ;;; job: commit my notes
        #~(job '(next-minute '(15))
               ;;(lambda ()
                 ;; (system*
                 ;;  (format #f "~a add . && ~a commit -m \"auto-commit | $( ~a -uIs )\""
                 ;;          #$(file-append (@ (gnu packages version-control) git) "/bin/git")
                 ;;          #$(file-append (@ (gnu packages version-control) git) "/bin/git")
                 ;;          #$(file-append (@ (gnu packages base) coreutils) "/bin/date"))))
                 "cd life \\
        && echo job: note-commit \\
        && git add . \\
        && git commit -m \"auto-commit | $(date -uIs)\""
               "backup: notes-commit"
               #:user #$my-user)
        
        ;; (use-modules (guix gexp))
        ;; (let ((f (lambda ()
        ;;          (call-with-output-file "/tmp/commit.log"
        ;;            (lambda (port)
        ;;              ;; (chdir "./life")
        ;;              (display
        ;;               (with-exception-handler
        ;;                   (lambda (exn)
        ;;                     (format #f "exception: ~s\n" exn))
        ;;                 (system*
        ;;                  (format #f "ls"
        ;;                          ;;"~a add . && ~a commit -m \"auto-commit | $( ~a -uIs )\""
        ;;                          ;;(file-append #$(@ (gnu packages version-control) git) "/bin/git")
        ;;                          ;;(file-append #$(@ (gnu packages version-control) git) "/bin/git")
        ;;                          ;;(file-append #$(@ (gnu packages base) coreutils) "/bin/date")
        ;;                          ))
               ;;                 port)))))))
        ;;   (f))
        
        ;; (call-with-output-file "/tmp/commit.log"
        ;;   (lambda (port)
        ;;   (display
        ;;    (system "git status")
        ;;    port)))
                 ;;; job: fulltext index the universe
        #~(job '(next-hour)
               (lambda ()
                 (system*
                  #$(file-append (@ (gnu packages search) recoll) "/bin/recollindex")))
               "index: recollindex"
               #:user #$my-user)
                 ;;; job: generate tags
        ;; ref :: https://guix.gnu.org/en/manual/devel/en/html_node/Scheduled-Job-Execution.html
        #~(job '(next-hour '(12 0)) ;; every 12 hours
               (lambda ()
                 (system*
                  #$(file-append (@ (gnu packages idutils) idutils) "/bin/mkid") "git"))
               "index: idutils"
               #:user #$my-user)
        ;;; job: generate tags
        ;; ref :: https://guix.gnu.org/en/manual/devel/en/html_node/Scheduled-Job-Execution.html
        #~(job '(next-hour) ;; every hour
               "aplay ~/vids/gong-cut.wav &>/dev/null; shuf -n1 ~/.config/cron/humanity | espeak -s 150"
               "gong"
               #:user #$my-user)
        )
       )
      
      ;; (simple-service 'emacs-my-package-rebuild
      ;;                 home-emacs-service-type
      ;;                 (home-emacs-extension
      ;;                  (rebuild-elisp-packages? #t)))
      (simple-service 'emacs-my-init-el
                      home-emacs-service-type
                      (home-emacs-extension
                       (init-el
                        (append
                         (list #~"(define-key key-translation-map [?\\C-x] [?\\C-u])\n"
                               #~"(define-key key-translation-map [?\\C-u] [?\\C-x])\n")
                         (list init-el)))))
      (simple-service 'emacs-my-package-list
                      home-emacs-service-type
                      (home-emacs-extension
                       (elisp-packages
                        (append
                          (list
                           ;;emacs-consult-dir
                           
                           ;;; QZDL
                           emacs-sql-indent
                           emacs-ob-go
                           emacs-org-ml
                           emacs-ox-jira
                           emacs-moldable-emacs
                           emacs-kubernetes
                           python-pylsp-mypy
                           emacs-ox-slack
                           emacs-svg-clock
                           emacs-consult-recoll
                           emacs-bigquery-mode
                           
                           ;; emacs-dogears ;; TODO broken [2024-03-31 Sun 16:08].  consider taking a "no logic in capture approach" anywy.  prescribing / filtering the list vs logging is not a robust thing.
                           ;; emacs-chatgpt-shell (superseded by upstream)
                           emacs-selectric-mode
                           emacs-atomic-chrome
                           emacs-spacious-padding
                           
                           ;;
                           ;; emacs-code-review    ; TODO qzdl pkg
                           )
                          (pkgs
                           "emacs-adaptive-wrap" ;; TODO feature-olivetti
                           "emacs-calfw"
                           "emacs-calibredb"
                           "emacs-chatgpt-shell"
                           "emacs-chess"
                           "emacs-company"
                           "emacs-consult-org-roam"
                           "emacs-crdt"
                           "emacs-csv-mode"
                           "emacs-debbugs"
                           "emacs-dimmer"
                           "emacs-edit-server"
                           "emacs-eglot"
                           "emacs-elfeed"
                           "emacs-eros"
                           ;; "emacs-ess" ;; TODO broken [2024-04-01 Mon 02:55]
                           "emacs-forge"
                           "emacs-fountain-mode" ;; TODO feature-emacs-fountain-mode
                           "emacs-ggtags"
                           "emacs-gnuplot"
                           "emacs-go-mode"
                           "emacs-gptel"
                           "emacs-highlight-indent-guides"
                           "emacs-hl-todo"
                           "emacs-htmlize" ;; ement: - ox-export html: org src blocks
                           "emacs-hyperbole"
                           "emacs-jq-mode"
                           "emacs-json-snatcher"
                           "emacs-jupyter"
                           "emacs-ligature"
                           "emacs-logview" ;; https://github.com/doublep/logview
                           "emacs-lsp-mode"
                           "emacs-lsp-ui"
                           "emacs-lua-mode"
                           "emacs-magit-todos"  ;; TODO feature-version-control
                           "emacs-ob-async"
                           "emacs-org-download"
                           "emacs-org-edit-latex"
                           "emacs-org-fragtog"
                           "emacs-org-jira"
                           "emacs-org-ql"
                           "emacs-org-reveal"
                           "emacs-org-roam-ui"
                           "emacs-org-super-agenda"
                           "emacs-org-transclusion"
                           "emacs-org-tree-slide"
                           "emacs-org-web-tools"
                           "emacs-orgit"        ;; TODO feature-version-control
                           "emacs-ox-hugo"
                           "emacs-ox-pandoc"
                           "emacs-paredit"
                           "emacs-plantuml-mode"
                           "emacs-protobuf-mode"
                           "emacs-py-isort"
                           "emacs-python-black"
                           "emacs-restart-emacs"
                           "emacs-slime"
                           "emacs-slime-repl-ansi-color"
                           "emacs-slime-volleyball"
                           "emacs-string-inflection"
                           "emacs-terraform-mode"
                           "emacs-yaml-mode"
                           ;; "emacs-explain-pause-mode" ;; FIXME upon reload, check fails :: https://ci.guix.gnu.org/search?query=emacs-explain-pause-mode%20spec:master
                           ;; "emacs-impostman"
                           ;; "emacs-org-autotangle"
                           ;; TODO feature-emacs-lsp
                           ;;"emacs-artbollocks"
                           ;;"emacs-repology"
                           ;;"emacs-vlf" ;; TODO guix: package emacs-vlf
                           )))))
        ;;; not exposed!!!!   such folly!!!
    )
     #:system-services
     (remove
      unspecified?
      (append (if gaming? (@@ (gnu services desktop) %desktop-services) '())
              (list
                       ;;;
               (service (@ (gnu services monitoring)
                           prometheus-node-exporter-service-type))
               
               (simple-service
                'system-jobs (@ (gnu services mcron) mcron-service-type)
                ;; ref :: https://guix.gnu.org/en/manual/devel/en/html_node/Scheduled-Job-Execution.html
                (list
                 ;; update locate database
                 ;; ref :: https://guix.gnu.org/en/manual/devel/en/html_node/Scheduled-Job-Execution.html
                 #~(job '(next-hour '(12 0)) ;; every 12 hours
                        (lambda ()
                          (execl (string-append #$(@ (gnu packages base) findutils) "/bin/updatedb")
                                 "updatedb"
                                 "--prunepaths=/tmp /var/tmp /gnu/store"))
                        "updatedb")
                 ))
                       ;;; udev: nvidia
               ;; (when gaming?
               ;;   (simple-service
               ;;    'nvidia-udev-rule udev-service-type
               ;;    (list nvidia-driver)))
                       ;;; desktop manager: X11 gdm + nvidia
               (when #f
                 (simple-service
                  'gdm-xorg-conf gdm-service-type
                  (gdm-configuration
                   (xorg-configuration
                    (xorg-configuration (keyboard-layout %thinkpad-layout)
                                        (modules (append
                                                  (list nvidia-driver)
                                                  %default-xorg-modules))
                                        (drivers (list "nvidia")))))))
               
               ;;(service nvidia-service-type)
               ;; User account and groups for QMK / ZSA.
               
               (simple-service 'udev-qmk-plugdev account-service-type
                               (list (user-group (name "plugdev"))))
               
               ;; (service-extension account-service-type
               ;;                  (const (list (user-group (name "plugdev")))))
               (udev-rules-service
                'zsa
                (udev-rule
                 "50-zsa.rules"
                 (string-append
                  ;; source :: https://github.com/zsa/wally/wiki/Linux-install
                  ;; requires USER to be in GROUP `plugdev'
                  "# Rules for Oryx web flashing and live training"
                  "KERNEL==\"hidraw*\", ATTRS{idVendor}==\"16c0\", MODE=\"0664\", GROUP=\"plugdev\""
                  "KERNEL==\"hidraw*\", ATTRS{idVendor}==\"3297\", MODE=\"0664\", GROUP=\"plugdev\""
               
                  "# Legacy rules for live training over webusb (Not needed for firmware v21+)"
                  "  # Rule for all ZSA keyboards"
                  "  SUBSYSTEM==\"usb\", ATTR{idVendor}==\"3297\", GROUP=\"plugdev\""
                  "  # Rule for the Moonlander"
                  "  SUBSYSTEM==\"usb\", ATTR{idVendor}==\"3297\", ATTR{idProduct}==\"1969\", GROUP=\"plugdev\""
                  "  # Rule for the Ergodox EZ"
                  "  SUBSYSTEM==\"usb\", ATTR{idVendor}==\"feed\", ATTR{idProduct}==\"1307\", GROUP=\"plugdev\""
                  "  # Rule for the Planck EZ"
                  "  SUBSYSTEM==\"usb\", ATTR{idVendor}==\"feed\", ATTR{idProduct}==\"6060\", GROUP=\"plugdev\""
               
                  "# Wally Flashing rules for the Ergodox EZ"
                  "ATTRS{idVendor}==\"16c0\", ATTRS{idProduct}==\"04[789B]?\", ENV{ID_MM_DEVICE_IGNORE}=\"1\""
                  "ATTRS{idVendor}==\"16c0\", ATTRS{idProduct}==\"04[789A]?\", ENV{MTP_NO_PROBE}=\"1\""
                  "SUBSYSTEMS==\"usb\", ATTRS{idVendor}==\"16c0\", ATTRS{idProduct}==\"04[789ABCD]?\", MODE:=\"0666\""
                  "KERNEL==\"ttyACM*\", ATTRS{idVendor}==\"16c0\", ATTRS{idProduct}==\"04[789B]?\", MODE:=\"0666\""
                  "# Keymapp / Wally Flashing rules for the Moonlander and Planck EZ"
                  "SUBSYSTEMS==\"usb\", ATTRS{idVendor}==\"0483\", ATTRS{idProduct}==\"df11\", MODE:=\"0666\", SYMLINK+=\"stm32_dfu\""
                  "# Keymapp Flashing rules for the Voyager"
                  "SUBSYSTEMS==\"usb\", ATTRS{idVendor}==\"3297\", MODE:=\"0666\", SYMLINK+=\"ignition_dfu\""
                  )))
               (udev-rules-service
                     'ydotool
                     (udev-rule
                      "80-uinput.rules"
                      ;; TODO: Take it from ydotool package
                      (string-append
                       "KERNEL==\"uinput\", MODE==\"0660\", "
                       "GROUP=\"input\", OPTIONS+=\"static_node=uinput\"")))
               
               (unless #t ;;gaming?
                 (service postgresql-service-type
                          (postgresql-configuration
                           (config-file
                            (postgresql-config-file
                             (hba-file
                              (plain-file "pg_hba.conf"
                                          "
               local	all	all			trust
               host	all	all	127.0.0.1/32    md5
               host	all	all	0.0.0.0/0       md5
               "
                                          ))))
                           (postgresql (@ (gnu packages databases) postgresql-10)))))
               ;; analytics ; timescaledb
               ;; (unless gaming?
               ;;   (service postgresql-service-type
               ;;            (name "postgres-tsdb-14")
               ;;            (postgresql-configuration
               ;;             (port 5435)
               ;;             (extension-packages
               ;;              (list (@ (gnu packages databases) timescaledb)
               ;;                    (@ (gnu packages geo) postgis)))
               ;;             (postgresql (@ (gnu packages databases) postgresql-14)))))
               (unless #t ;; gaming?
                 (service postgresql-role-service-type
                          (postgresql-role-configuration
                           (roles (list (postgresql-role
                                         (name "postgres")
                                         (permissions '(superuser))
                                         (create-database? #t))
                                        (postgresql-role
                                         (name "samuel")
                                         (permissions '(superuser login))
                                         (create-database? #t))
                                        (postgresql-role
                                         (name "newstore")
                                         (permissions '(login))
                                         (create-database? #t)))))))
               ;;; ssh
               ;; TODO key up, remove password method
               (service openssh-service-type
                        (openssh-configuration
                         (password-authentication? #t)
                         ;; (authorised-keys
                         ;;  `(("hww" ,(local-file "hww.pub"))
                         ;;    ))
                         ))
               ;; (gnu services nfs)
               (service nfs-service-type
                        (nfs-configuration
                               (exports
                                '(("/export"
                                   "*(ro,insecure,no_subtree_check,crossmnt,fsid=0)")))))
               ))))
    (unless gaming?
      (feature-base-services
       #:guix-substitute-urls '("https://substitutes.nonguix.org")
       #:guix-authorized-keys
       (list
         ;;; https://substitutes.nonguix.org/signing-key.pub
        ;; see also https://guix.gnu.org/manual/en/html_node/Substitutes.html
        (plain-file "nonguix.pub" "
    (public-key
     (ecc
      (curve Ed25519)
      (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
      )
     )
    "))))
    (unless gaming? (feature-desktop-services))
    (feature-docker)
    (feature-qemu)
    (feature-backlight #:step 5)
    (unless gaming? (feature-pipewire))
    (feature-fonts
     #:font-monospace
     (font
      (name "Iosevka")
      (package (@ (gnu packages fonts) font-iosevka))
      (size 14)
      (weight 'light)))
    (feature-vterm)
    (feature-bash)
    (feature-direnv)
    (feature-zsh
     #:enable-zsh-autosuggestions? #t)
    (feature-ssh
     #:ssh-configuration
     (home-ssh-configuration
      ;; (default-options
      ;;   '((hostkey-algorithms . "+ssh-rsa")
      ;;     (pubkey-accepted-algorithms "+ssh-rsa")))
      (extra-config
       (list (ssh-host
              (host "qz")
              ;; <(create ixy->qz key)>
              (options '((user . "samuel")
                         (hostname . "192.168.0.249")
                         (port . 22)
                         (identity-file . "~/.ssh/qzdl"))))
             (ssh-host
              (host "ko")
              (options '((user . "root")
                         (hostname . "192.168.0.240")
                         (port . 2222)
                         (identity-file . "~/.ssh/ko"))))
             (ssh-host
              (host "yoho")
              (options '((user . "yoho")
                         (hostname . "192.168.0.12")
                         (port . 22)
                         (identity-file . "~/.ssh/yoho"))))
             (ssh-host
              (host "boggartonius")
              (options '((user . "chopi")
                         (hostname . "192.168.0.43")
                         (port . 22)
                         (identity-file . "~/.ssh/ixy-bogg"))))
             (ssh-host
              (host "medmac")
              (options '((user . "sam")
                         (hostname . "192.168.1.118")
                         (port . 22)
                         (identity-file . "~/.ssh/ixy-medmac"))))))))
    (feature-git
     #:extra-config
     '((pull
        ((rebase . #t)))
       (diff
        ((context . 4)  ;; magit default=3;  4 shows org prop-drawer & heading.
         ))
       ;; (slurp-file-like (local-file "./etc/git/work_config"))
       ;; '(#~"[includeIf \"gitdir:~/git/ns\"]
       ;;     [user]
       ;;         signingkey = \"290D5A69F2021C4E\"
       ;;         email = \"sculpepper@newstore.com\"
       ;; ")
       ))
    (feature-bluetooth)
    ;;(feature-ssh-socks-proxy
    ;; #:host "204:cbf:3e07:e67a:424f:93bc:fc5c:b3dc")
    ;;(feature-i2pd
    ;; #:outproxy 'http://acetone.i2p:8888
    ;; ;; 'purokishi.i2p
    ;; #:less-anonymous? #t)
    (feature-transmission #:auto-start? #f)
    (unless gaming?
      (feature-sway
       ;;#:sway (replace-mesa sway)
       #:xwayland? #t
       ;; #:opacity 0.9 ;; TODO qzdl
       ;; #:wallpaper "$HOME/.cache/wallpaper.png" ;; TODO qzdl
       #:extra-config
       `(
         ;;(include ,(local-file "./config/sway/config"))
    
         ;; TODO sway: toggle opacity for WINDOW
    
         (,#~"output * bg ~/.cache/wallpaper.png fill")
         ;;(,#~"output eDP-1 bg ~/.cache/wallpaper.png fill")
         (,#~"output DP-2 res 3840x1080")
         (,#~"output DP-1 res 3840x1080")
    
         ;; TODO sway: wacom input rotation matrix
         (,#~"input \"*\" tool_mode \"*\" relative calibration_matrix 0.0 -1.0 1.0 1.0 0.0 0.0")
    
         ;; danke demis ht - Sharing Indicatortps://github.com/minikN/guix/blob/ca15b5a5954d50fe75e2b03f21afc019e002022b/config.scm#L173
         (for_window "[app_id=\"pavucontrol\"]" floating enable, border pixel)
         (for_window "[app_id=\"pinentry-qt\"]" floating enable, border pixel)
    
         (for_window "[title=\"Nightly - Sharing Indicator\"]" floating enable, border pixel)
         ;; (for_window "[title=\"Emacs (Client) [pass]\"]" floating enable, border pixel)
         ;; (for_window "[title=\"Application Launcher - Emacs Client\"]" floating enable, border pixel)
         ;; (for_window "[title=\"pass - Emacs Client\"]" floating enable, border pixel)
    
         (bindsym $mod+Ctrl+o opacity set 1)
         (bindsym $mod+Ctrl+p opacity minus 0.1)
    
         ;;(bindsym $mod+x exec $menu)
    
         (gaps inner 20)
    
         ;;; Mousepad sensitivity & behaviorr
         (input type:touchpad
                ;; TODO: Move it to feature-sway or feature-mouse?
                ((accel_profile flat)
                 (pointer_accel "0.2")
                 (natural_scroll enabled) ;; tablet / phone mindset
                 (tap enabled)))
    
         ;;; Nub Life.
         (input "2:10:TPPS/2_Elan_TrackPoint"
                ((pointer_accel "0.2")
                 (accel_profile flat)))
    
         ;;; Middle Mouse.
         (bindsym $mod+button2 "[con_mark=_prev]" focus)
    
         ;;; Volume Control.
         (bindsym $mod+bracketright exec "pactl set-sink-volume @DEFAULT_SINK@ +5%")
         (bindsym $mod+bracketleft exec "pactl set-sink-volume @DEFAULT_SINK@ -5%")
         (bindsym $mod+Ctrl+bracketright exec "pactl set-sink-mute @DEFAULT_SINK@ toggle")
         (bindsym $mod+Ctrl+bracketleft exec "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
         ;; alsa_input.usb-TEAC_Corporation_TASCAM_DR_Series-00.analog-stereo
         )))
    (feature-sway-run-on-tty
     #:sway-tty-number 2
     ;;#:launch-arguments "--unsupported-gpu 2> ${XDG_LOG_HOME}/sway.log"
     )
    
    (unless gaming? (feature-sway-screenshot))
    (unless gaming?
      (feature-waybar
       #:waybar-modules
       (list
        (waybar-sway-workspaces)
        ;; (waybar-sway-window)
        (waybar-tray)
        (waybar-idle-inhibitor)
        ;; (waybar-temperature)
        ;; (waybar-sway-language)
        (waybar-microphone)
        (waybar-volume #:scroll-step 10)
        (waybar-battery #:intense? #f)
        (waybar-clock))))
    (unless gaming?
      (feature-swaylock
       #:swaylock (@ (gnu packages wm) swaylock-effects)
       ;; The blur of last-screen on lock screen is not privacy-friendly.
       ;; TODO use blurred wallpaper from $HOME/.cache/wallpaper.png
       #:extra-config '(;; (screenshots)
                        (effect-blur . 7x5)
                        (image . "~/.cache/wallpaper.png")
                        (scale . fill)
                        (clock))))
    ;;(feature-swaynotificationcenter)
    
    ;; not found...
    ;; (feature-redshift
    ;;  #:redshift redshift-wayland)
    (feature-python)
    ;;(feature-clojure)
    (feature-plantuml)
    (feature-emacs
     ;;-portable
     ;;#:emacs (@ (declantsien packages emacs) emacs-next-wr)
     ;; #:emacs (@ (rde packages emacs) emacs-next-pgtk-latest)
     ;; #:emacs emacs-next-pgtk
     ;; #:emacs emacs-pgtk
     #:native-comp? #f
     )
    (feature-emacs-modus-themes)
    ;; TODO extra-modus-theme-overrides
    ;; [[file:~/git/sys/rde/src/rde/features/emacs-xyz.scm::(define* (feature-emacs-modus-themes][emacs-xyz.scm::feature-emacs-modus-themes]]
    ;; #:extra-elisp
    ;; `((setq modus-themes-syntax '(faint))
    ;;   ;; (setq modus-themes-region '(bg-only))
    ;;   ;; (setq modus-themes-paren-match '(underline))
    ;;   (setq modus-themes-org-blocks 'tinted-background))
    
    (feature-emacs-appearance)
    (feature-emacs-completion
     #:mini-frame? #f
     #:marginalia-align 'right)
    (feature-emacs-vertico)
    (feature-emacs-project)
    (feature-emacs-perspective)
    (feature-emacs-input-methods)
    (feature-emacs-which-key)
    (feature-emacs-keycast)
    
    ;; (feature-emacs-perfect-margin ;; TODO QZDL
    ;;  #:visible-width 150)
    
    (feature-emacs-dired)
    ;;(feature-emacs-vterm) ;; TODO QZDL merge with feature-vterm
    (feature-emacs-monocle)
    ;; (feature-emacs-message) ;; TODO compose mail.  broken with emacs-feature-portableu
    (feature-emacs-smartparens
     #:show-smartparens? #t)
    (feature-emacs-corfu
     #:corfu-doc-auto #f)
    (feature-emacs-help)
    (feature-emacs-info)
    (feature-emacs-tempel
     #:default-templates? #t
     #:templates `(fundamental-mode
                   ,#~""
                   (t (format-time-string "%Y-%m-%d"))
                   (ielisp (elisp "src_emacs-lisp{ " r> " }"))))
    (feature-emacs-elpher)
    ;;(feature-emacs-ement) ;; TODO qzdl
    ;; (feature-emacs-pdf-tools) ;; TODO broken [2024-04-01 Mon 02:19] portable emacs
    ;; (feature-emacs-nov-el) ;; TODO broken [2024-04-01 Mon 02:19] portable emacs
    ;;     ;; TODO: Revisit <https://en.wikipedia.org/wiki/Git-annex>
    (feature-emacs-git)
    (feature-emacs-org
     #:org-directory my-org-directory
     #:org-indent? #t
     #:org-modern? #t
     ;;#:org-agenda-directory my-notes-directory ;; TODO qzdl
     )
    (feature-emacs-org-agenda
     #:org-agenda-files '("~/life/roam/inbox.org"))
    ;; TODO: <https://www.labri.fr/perso/nrougier/GTD/index.html#table-of-contents>
    (feature-emacs-org-roam
     ;; TODO: Rewrite to states
     #:org-roam-directory my-notes-directory
     #:org-roam-todo? #f
     ;;#:org-roam-dailies-directory (string-append my-notes-directory "/daily")) ;; TODO qzdl
     )
    ;; FIXME guix: org-roam-ui: httpd communication problem (endemic to guix)
    ;; (feature-emacs-org-roam-ui)
    ;; (feature-emacs-ref
    ;;  ;; why error with nil for reftex-default-bibliography
    ;;  ;; TODO: Rewrite to states
    ;;  #:bibliography-paths
    ;;  (list (string-append my-org-directory "/tex.bib"))
    ;;  #:bibliography-notes
    ;;  (list(string-append my-org-directory "/bib.org")
    ;;       #:bibliography-directory my-notes-directory)
    ;;     ;; TODO qzdl (2) es/rest
    ;;     ;; (feature-emacs-es-mode
    ;;     ;;  #:package emacs-es-mode-latest)
    ;;     ;; (feature-emacs-restclient
    ;;     ;;  #:package-ob emacs-ob-restclient-latest)
    ;; (feature-mpv)
    (feature-isync
     #:isync-verbose #t
     #:isync-serializers
     (append %default-isync-serializers
             `((bravehost . ,bravehost-isync-settings)
               (gmail-tls . ,gmail-tls-isync-settings))))
    ;; (feature-l2md) ;; TODO  public-inbox mirrors& istupdate
    (feature-msmtp
     #:msmtp-provider-settings
     (append
      %default-msmtp-provider-settings
      `((bravehost . ((host . "mail.bravehost.com")
                      (port . 465)
                      (tls_starttls . off)))
        (gmail-tls . ((host . "smtp.gmail.com")
                      (port . 465)
                      (tls_starttls . off))))))
    (feature-notmuch
     #:extra-tag-updates-post
     '("notmuch tag +guix-home -- 'thread:\"{((subject:guix and subject:home) or (subject:service and subject:home) or subject:/home:/) and tag:new}\"'"
       "notmuch tag +calendar  -- 'tag:work and thread:\"{(subject:/nvitation/ or subject:/eclined/ or subject:/ccepted/ or subject:/anceled/)}\"'"
       "notmuch tag +noise -- 'tag:work and (tag:calendar or subject:/basecamp/)'")
     #:notmuch-saved-searches
     (cons*
      '(:name "Inbox :: Personal"  :key "P"
        :query "tag:unread and tag:inbox and tag:personal")
      '(:name "Inbox :: Work"      :key "Ww"
        :query "tag:unread and tag:inbox and tag:/work/")
      '(:name "Inbox :: Guix Home" :key "H"
        :query "tag:unread and tag:guix-home")
      '(:name "Inbox :: RDE"       :key "R"
        :query "tag:unread and (to:/rde/ or cc:/rde/)")
      '(:name "Watching"           :key "tw"
        :query "thread:{tag:watch}")
      '(:name "Work :: data"      :key "Wd"
        :query "tag:work and (from:/jlunan/ or to:/dre/)")
      %rde-notmuch-saved-searches))
    (feature-xdg
     #:xdg-user-directories-configuration
     (home-xdg-user-directories-configuration
      (music "$HOME/music")
      (videos "$HOME/vids")
      (pictures "$HOME/pics")
      (documents "$HOME/docs")
      (download "$HOME/dl")
      (desktop "$HOME")
      (publicshare "$HOME")
      (templates "$HOME")))
    (feature-base-packages
     #:home-packages
     (append
      (list
       ;; python-keyring-pass ;; TODO broken [2024-03-31 Sun 16:24]soehitng
       ;; perl-graph-easy
       )
    
      (pkgs
    
       "figlet" ;; TODO: Move to emacs-artist-mode
       ;;"calibre"
       "utox" "qtox" "jami"
    
       "vim"
    
       "alsa-utils"
       "pavucontrol"
    
       ;; wayland-dev
       "wev"
       "wl-clipboard" "grim"
    
    
       "obs" "obs-wlrobs"
       "recutils" "binutils"
       "fheroes2"
       ;; TODO: Enable pipewire support to chromium by default
       ;; chrome://flags/#enable-webrtc-pipewire-capturer
       "ispell"
       ;;
       "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-extra"
       "papirus-icon-theme" "arc-theme"
       "thunar"
       ;; "glib:bin"
    
       ;; TODO: Fix telega package!
       "ffmpeg"
    
       "ripgrep" "curl" "make"
       "sqlite"
    
       ;; python-base
       ;;; PYTHON
       "python"
       "python-black"
       "python-flake8"
       "python-isort"
       "python-lsp-server"
       "python-lz4"
       "python-numpy"
       "python-pandas"
       "python-pip"
       ;; "python-psycopg"
       ;; "python-pyan3"
       "python-pytest-black"
       "python-pytest-isort"
       ;; "python-pywal"
       ;; "python-pyzstd"
       "python-scipy"
       ;; "python-virtualenv"
       "python-yq"
       ;; "python-pydot"
       ;; "python-networkx"
    
       "libnotify"
    
       "gdk-pixbuf" ;; HACK to fix firefox dialog crashes
    
       "v4l-utils"
       "qmk-udev-rules"
       ))
     #:system-packages
     (pkgs "fwupd-nonfree")
     )
    (feature-yt-dlp
     #:emacs-ytdl emacs-ytdl-next)
    ;;(feature-nyxt)
    (feature-emacs-dashboard
     #:show-on-startup? #f)
    (feature-emacs-ednc)
    (feature-emacs-pulseaudio-control)
    (feature-emacs-display-wttr)
    (feature-slack-settings
     #:slack-accounts (list (slack-account
                             (workspace "newstoreinc")
                             (nick "sculpepper")
                             (cookie? #t))))
    (feature-emacs-slack)
    (feature-pantalaimon)
    (feature-matrix-settings
     #:matrix-accounts (list (matrix-account
                              (id "@sculp-ns:matrix.org")
                              (server "localhost:8009"))))
    (feature-emacs-ement)
    (feature-emacs-geiser)
    (feature-emacs-guix)
    (feature-emacs-elisp)
    (feature-emacs-emms)
    (feature-emacs-flymake)
    (feature-emacs-org-protocol)
    (feature-emacs-spelling)
    ;;; END; main
    )))

(define ixy-mapped-devices
  (list (mapped-device
         (source (uuid "cb453366-cc17-4742-ada1-91f7f569103f"))
         (target "sys-root")
         (type luks-device-mapping))))
(define ixy-file-systems
  (list (file-system
          (device (file-system-label "sys-root"))
          (mount-point "/")
          (type "ext4")
          (dependencies ixy-mapped-devices))
        (file-system
          (device "/dev/nvme0n1p1")
          (mount-point "/boot/efi")
          (type "vfat"))))
;; (define ixy-file-systems
;;   (append
;;    (map (match-lambda
;;        ((subvol . mount-point)
;;         (file-system
;;           (type "btrfs")
;;           (device "/dev/mapper/enc")
;;           (mount-point mount-point)
;;           (options (format #f "subvol=~a" subvol))
;;           (dependencies ixy-mapped-devices))))
;;      '((root . "/")
;;        (boot . "/boot")
;;        (gnu  . "/gnu")
;;        (home . "/home")
;;        (data . "/data")
;;        (log  . "/var/log")))
;;    (list
;;     (file-system
;;       (mount-point "/boot/efi")
;;       (type "vfat")
;;       (device (uuid "8C99-0704" 'fat32))))))
(use-modules
 (gnu packages linux)
 ((nongnu packages linux) #:prefix nongnu:)
 ((nongnu system linux-initrd) #:prefix nongnu-sys:))
(define %ixy-features
  (list
   (feature-host-info
    #:host-name "ixy"
    #:timezone  my-timezone)
   ;;; Allows to declare specific bootloader configuration,
   ;;; grub-efi-bootloader used by default
   (feature-bootloader
    #:bootloader-configuration
    (bootloader-configuration
     (bootloader grub-efi-removable-bootloader)
     (targets '("/boot/efi"))))
   ;; os
   (feature-kernel
    ;;#:kernel nongnu:linux-lts ;;; NVIDIA
    #:kernel nongnu:linux ;;; DEFAULT
    #:kernel-arguments
    '("quiet" "ipv6.disable=1" "net.ifnames=0"

      ;;; DEFAULT
       "nouveau.modeset=1"

      ;;; FORUM NVIDIA ATTEMPT https://forums.developer.nvidia.com/t/nvidia-495-on-sway-tutorial-questions-arch-based-distros/192212
      ;;"nvidia-drm.modeset=1" "nouveau.blacklist=1" "modprobe.blacklist=nouveau"

      ;;; NEW NVIDIA ATTEMPT 0
      ;;"nvidia_drm.modeset=1"
      ;;"modprobe.blacklist=nvidiafb,nouveau"
      ;;"nvidia.NVreg_OpenRmEnableUnsupportedGpus=1"

      ;;; NEW NVIDIA ATTEMPT 1 (fail, but progress; nvidia-smi runs)
      ;;"nvidia_drm.modeset=1"
      ;;"modprobe.blacklist=nouveau"
      )
    ;; removed "modprobe.blacklist=snd_hda_intel,snd_soc_skl"
    #:firmware (list nongnu:linux-firmware
                     nongnu:sof-firmware
                     intel-vaapi-driver)
    #:initrd nongnu-sys:microcode-initrd
    #:kernel-loadable-modules (list v4l2loopback-linux-module
                                    ;;nvidia-module ;; NVIDIA FORUM
                                    ))
   (feature-file-systems
    #:mapped-devices ixy-mapped-devices
    #:file-systems ixy-file-systems)
   ;;(feature-hidpi)
   (feature-networking)))

(pretty-print "post-%ixy-features")

;;; rde-config and helpers for generating home-environment and
;;; operating-system records.

(define-public ixy-config
  (rde-config
   (features
    (append
     %user-features
     %main-features
     %ixy-features
     ))))
;; (map pretty-print
;;      (append %user-features
;;              %main-features
;;              %ixy-features))
(define-public ixy-os
  (rde-config-operating-system ixy-config))
(define ixy-he
  (rde-config-home-environment ixy-config))
(use-modules (gnu system file-systems))

(define live-file-systems
  (list (file-system
          (mount-point "/")
          (device (file-system-label "Guix_image"))
          (type "ext4"))

        ;; Make /tmp a tmpfs instead of keeping the overlayfs.  This
        ;; originally was used for unionfs because FUSE creates
        ;; '.fuse_hiddenXYZ' files for each open file, and this confuses
        ;; Guix's test suite, for instance (see
        ;; <http://bugs.gnu.org/23056>).  We keep this for overlayfs to be
        ;; on the safe side.
        (file-system
          (mount-point "/tmp")
          (device "none")
          (type "tmpfs")
          (check? #f))

        ;; XXX: This should be %BASE-FILE-SYSTEMS but we don't need
        ;; elogind's cgroup file systems.
        ;; (list %pseudo-terminal-file-system
        ;;       %shared-memory-file-system
        ;;       %efivars-file-system
        ;;       %immutable-store)
        ))
(use-modules (gnu services))
(define-public live-config
  (rde-config
   (features
    (append
     %user-features
     %main-features
     (list
      (feature-host-info
       #:host-name "gnu"
       #:timezone  "Europe/Moscow")

      (feature-file-systems
       #:file-systems live-file-systems)
      (feature-hidpi)
      (feature-custom-services
       #:feature-name-prefix 'live
       #:system-services
       (list
        (simple-service
         'channels-and-sources
         etc-service-type
         `(("channels.scm" ,(local-file "live-channels"))
           ("guix-sources" ,(local-file "/home/bob/work/gnu/guix"
                                        #:recursive? #t))
           ("rde-sources" ,(local-file "/home/bob/work/abcdw/rde"
                                       #:recursive? #t))))
        ;; (service
        ;;  guix-home-service-type
        ;;  `(("bob" . ,ixy-he)))
        (service
         gc-root-service-type
         (list ixy-he))
        )))))))
(define-public live-os
  (rde-config-operating-system live-config))
(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("ixy-home" ixy-he)
      ("ixy-system" ixy-os)
      ("bogg-home" bogg-he)
      ("bogg-system" bogg-os)
      ("live-system" live-os)
      (_ ixy-he))))
(dispatcher)
