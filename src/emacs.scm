(define-module (emacs)
  #:use-module (rde serializers elisp)
  #:export (init-el))

(define init-el
  (elisp-serialize
   '(

     ;; NOWEB GENERAL START
     (defmacro qz/advice- (target-fn state advice-fn)
       "Helper to manage advice functions.
     
     Creates commands for on/off which state the name of the target and advised functions."
       (let* ((s-advice (lambda (e)
                          (intern (format "qz/advice-%s--%s--%s"
                                          e target-fn advice-fn))))
              (enable (funcall s-advice 'enable))
              (disable (funcall s-advice 'disable)))
         `(progn
            (defun ,enable ()
              (interactive)
              (advice-add ',target-fn ,state ',advice-fn))
     
            (defun ,(funcall s-advice 'disable) ()
              (interactive)
              (advice-remove ',target-fn ',advice-fn))
     
            (,enable)
            (list ',enable ',disable))))
     (defun cons->table (body &optional &key cols tail-fn)
       "a transformation helper for org-babel, which has defaults
     to parse robustly the proper-list[1] over the simple cons[2]
     
     body      *values you wish to transform*: a list; cons, proper,
               a-, etc.
     :cols     *column headers for the results*: wrap the result in
               ((co1 col2) . (hline . (..res..)); as such, they will
               be made in addition to any headers and/or `hlines'
               applied by `org-babel' (esp. those from `:colnames').
     :tail-fn  *control the parsing of each entry of `body'*:
               by default, `cdr' because for a simple `cons' '(a . b),
               cdr will yield 'b -> (cdr '(a . b)).  If operating on
               some `list' '(a b), then the analog for `'b' is `cadr'
               -> (cadr '(a b)) -> `'b'
     
     [1] proper-list: '(a b)   ; '(a . (b . nil))
     [2] simple-cons: '(a . b) ; '(a . b)"
       (let ((res (mapcar (lambda (c)
                            (list (car c)
                                  (funcall (or tail-fn 'cdr) c)))
                          body)))
         (if cols
             (cons cols (cons 'hline res))
           res)))
     
     ;;; e.g  {C-n C-SPC M-e C-p C-x C-;}
     ;; (cons->table
     ;;  '((56 . "/home/samuel/life/roam/20210420T114708Z-newstore.org")
     ;;    (11 . "/home/samuel/life/roam/20210813T161035Z-kubernetes.org")
     ;;    (10 . "/home/samuel/life/roam/20200515T151822Z-postgresql.org"))
     ;;  :cols '(count file))
     (defun qz/ensure-list (s)
       (if (listp s)
           s
         (list s)))
     (defun qz/save-no-hooks ()
       (interactive)
       (let ((before-save-hook nil))
         (save-buffer)))
     (defun qz/mode-is-or-derives (mode &rest modes)
       (or (eq major-mode mode)
           (apply 'derived-mode-p mode modes)))
     
     ;;  (trace-function 'qz/mode-is-or-derives)
     (defvar qz/debug 0 "debugging assists")
     
     (defmacro qz/debug- (&rest body)
       (when (< 0 (or qz/debug 0))
         `(progn ,@body)))
     
     (qz/debug- (message "yo"))
     (require 's)
     
     (setq qz/newstore-envs '(sandbox staging production)
           qz/newstore-env-current nil
           qz/newstore-envs-abbrev '((sandbox . x) (staging . s) (production . p))
           qz/newstore-tenant-current nil
           qz/newstore-tenants '("dodici" "windsor"
                                 "boardriders" "marine-layer"
                                 "frankandoak" "vince"))
     
     (defun qz/newstore-choose-env (&optional env)
       (interactive)
       (message "qz/newstore-env-current: %s"
                (setq qz/newstore-env-current
                      (or env (completing-read "env: " qz/newstore-envs))))
       (qz/restclient-choose-env qz/newstore-env-current)
       (qz/es-choose-url nil nil qz/newstore-env-current))
     
     (defun qz/newstore-choose-tenant (&optional tenant)
       (interactive)
       (message "qz/newstore-tenant-current: %s"
                (setq qz/newstore-tenant-current
                      (or tenant (completing-read "tenant: " qz/newstore-tenants))))
       (qz/restclient-choose-tenant qz/newstore-tenant-current))
     
     (defun qz/newstore-auth-current ()
       (message "qz/newstore-auth-cache: <for qz/newstore-env-current: %s>"
                qz/newstore-env-current)
       (setq qz/newstore-auth-cache
             (qz/newstore-auth qz/newstore-env-current)))
     
     (defun qz/newstore-auth (env)
       "get the auth (password) associated with
     a given `env' from `qz/newstore/envs'
     
     to populate, just fill a `pass' entry like so echo mypass | pass
       insert -e newstore/production"
       (s-trim (shell-command-to-string
                (format "pass newstore/%s" env))))
     
     ;; (defun qz/newstore-quick-auth ()
     ;;   (interactive)
     ;;   (qz/newstore-choose-tenant)
     ;;   (qz/newstore-choose-env)
     ;;   (org-sbe "newstore-token"))
     (defun qz/shell-command-to-list-of-strings (command)
       (remove "" (s-split "\n" (shell-command-to-string command))))
     (defun qz/revert-buffer-no-confirm ()
       "Revert buffer without confirmation."
       (interactive)
       (revert-buffer :ignore-auto :noconfirm))
     ;; NOWEB GENERAL END

     ;; NOWEB CONF START
     (defun dogears-remember (&rest _ignore)
       "Remember (\"dogear\") the current place."
       (interactive)
       (unless (cl-some 'funcall dogears-ignore-places-functions)
         (if-let* ((record (or (ignore-errors
                                 (funcall bookmark-make-record-function))
                               (dogears--buffer-record)))
                   (record (list (car record)
                                 (append (cdr record)
                                         (cons 'region (call-interactively
                                                        'qz/anno-yank-region-to-point))))))
     
             (progn
               (pcase (car record)
                 ;; Like `bookmark-make-record', we may have to add a string ourselves.
                 ;; And we want every record to have one as its first element, for
                 ;; consistency.  And sometimes, records have a nil name rather than an
                 ;; empty string, depending on the bookmark-make-record-function (I'm
                 ;; not sure if there are defined standards for what the first element
                 ;; of a bookmark record should be).
                 ((pred stringp)
                  ;; Record already has a string as its first element: do nothing.
                  nil)
                 (`nil (setf (car record) ""))
                 (_ (push "" record)))
               (setf (map-elt (cdr record) 'manual)
                     (if (called-interactively-p 'interactive) "✓" " "))
               (unless (map-elt (cdr record) 'buffer)
                 (setf (map-elt (cdr record) 'buffer) (buffer-name)))
               (when-let ((within (or (funcall dogears-within-function)
                                      (dogears--within)
                                      (car record))))
                 (setf (map-elt (cdr record) 'within) within))
               (setf (map-elt (cdr record) 'mode) major-mode
                     (map-elt (cdr record) 'line) (buffer-substring
                                                   (point-at-bol) (point-at-eol)))
               ;; It's hard to say whether push or pushnew is the best choice.  When returning
               ;; to a dogeared place, that place gets moved to the front of the list, or it
               ;; remains where it was.  Either way, unless we allow dupes, the list changes.
               (cl-pushnew record dogears-list :test 'dogears--equal)
               (setf dogears-list (delete-dups dogears-list)
                     dogears-list (seq-take dogears-list dogears-limit))
               (when (and dogears-update-list-buffer (buffer-live-p dogears-list-buffer))
                 (with-current-buffer dogears-list-buffer
                   (revert-buffer))))
           (when (called-interactively-p 'interactive)
             (message "Dogears: Couldn't dogear this place"))
           )))
     (define-key global-map (kbd "C-s-k") 'kill-this-buffer)
     (define-key global-map (kbd "C-M-s-k") 'kill-buffer-and-window)
     ;; NOWEB KBD START
     (define-key global-map (kbd "C-c C-x C-j") 'org-clock-goto)
     (define-key global-map (kbd "C-c !") 'org-time-stamp-inactive)
     (define-key global-map (kbd "C-s-j") 'org-roam-dailies-goto-today)
     (define-key global-map (kbd "C-M-j") 'delete-indentation)
     (define-key global-map (kbd "C-M-y") 'consult-yank-from-kill-ring)
     (define-key global-map (kbd "C-x C-M-f") 'consult-recent-file)
     ;;(custom-set-variables
     ;; '(org-disputed-keys '([(shift o)] . [(meta shift o)])))
     
     (defun qz/newline-above ()
       (interactive)
       (save-excursion
         (beginning-of-line)
         (newline))
       (indent-according-to-mode))
     
     (define-key global-map (kbd "C-z") 'qz/newline-above)
     ;;(define-key global-map (kbd "C-o") 'open-line)
     ;;
     ;;(org-remap org-mode-map
     ;;           'open-line 'org-open-line)
     
     (define-key global-map (kbd "M-s-h") 'windmove-swap-states-left)
     (define-key global-map (kbd "M-s-j") 'windmove-swap-states-down)
     (define-key global-map (kbd "M-s-k") 'windmove-swap-states-up)
     (define-key global-map (kbd "M-s-l") 'windmove-swap-states-right)
     (define-key global-map (kbd "H-M-s-h") 'windmove-swap-states-left)
     (define-key global-map (kbd "H-M-s-j") 'windmove-swap-states-down)
     (define-key global-map (kbd "H-M-s-k") 'windmove-swap-states-up)
     (define-key global-map (kbd "H-M-s-l") 'windmove-swap-states-right)
     (define-key global-map (kbd "H-s-h") 'windmove-left)
     (define-key global-map (kbd "s-h")   'windmove-left)
     (define-key global-map (kbd "H-s-j") 'windmove-down)
     (define-key global-map (kbd "s-j")   'windmove-down)
     (define-key global-map (kbd "H-s-k") 'windmove-up)
     (define-key global-map (kbd "s-k")   'windmove-up)
     (define-key global-map (kbd "H-s-l") 'windmove-right)
     (define-key global-map (kbd "s-l")   'windmove-right)
     ;; Activate occur easily inside isearch
     
     (define-key isearch-mode-map (kbd "C-o")
                 (lambda () (interactive)
                   (let ((case-fold-search isearch-case-fold-search))
                     (occur (if isearch-regexp
                                isearch-string
                              (regexp-quote isearch-string))))))
     (define-key isearch-mode-map (kbd "M-o")
                 (lambda () (interactive)
                   (let ((case-fold-search isearch-case-fold-search))
                     (consult-line (if isearch-regexp
                                       isearch-string
                                     (regexp-quote isearch-string))))))
     (global-set-key (kbd "C-s") 'isearch-forward-regexp)
     (global-set-key (kbd "C-r") 'isearch-backward-regexp)
     (global-set-key (kbd "C-M-s") 'isearch-forward)
     (global-set-key (kbd "C-M-r") 'isearch-backward)
     (define-key global-map (kbd "s-\\") 'org-store-link)
     (defun qz/dwim-indent ()
       "format / indent something.
     
     org-mode: indent the block at point
     prog-mode: indent the whole buffer
     
     for the rest: remember that indent-region still exists.
     
     good-boy mode: cleans up whitespace too
     
     dwim by mode is important
     
     as until now, I've been dodging
     org-errors about caching from my dodgy code"
       (interactive)
       (save-mark-and-excursion
         (when (symbolp 'whitespace-cleanup)
           (whitespace-cleanup))
         (when-let ((f (cond
                        ((and (derived-mode-p 'org-mode)
                              (and (call-interactively 'org-babel-mark-block)
                                   (not (seq-contains-p
                                         qz/org-babel-indent-exclude-lang
                                         (car (org-babel-get-src-block-info))))))
                         (when (equal "python" (car (org-babel-get-src-block-info)))
                           (mapc 'call-interactively '(python-black-region
                                                       py-isort-region)))
                         'indent-region)
                        ((derived-mode-p 'python-mode)
                         (python-black-buffer)
                         (py-isort-buffer))
                        ((and (derived-mode-p 'prog-mode)
                              (mark-whole-buffer))
                         'indent-region)
                        (t nil))))
           (call-interactively f))))
     (define-key rde-toggle-map (kbd "d")   'toggle-debug-on-error)
     
     (define-key rde-toggle-map (kbd "h c") 'highlight-changes-mode)
     (define-key rde-toggle-map (kbd "h C") 'global-highlight-changes-mode)
     
     (define-key rde-toggle-map (kbd "h i") 'highlight-indent-guides-mode)
     
     ;; a bit sus, but maybe equivalent to toggling off hi-lock-mode
     (define-key rde-toggle-map (kbd "h p") 'unhighlight-regexp)
     (define-key rde-toggle-map (kbd "h P") 'global-hi-lock-mode)
     
     ;; for the incessant observers demanding more than {M-g M-g}
     (define-key rde-toggle-map (kbd "l")   'linum-mode)
     (define-key rde-toggle-map (kbd "L")   'global-linum-mode)
     (define-key global-map (kbd "M-s L") 'consult-line-multi)
     (define-key global-map (kbd "C-c C-j") 'consult-imenu)
     (define-key global-map (kbd "C-c n j") 'org-roam-dailies-capture-today)
     (define-key global-map (kbd "C-c n J") 'org-roam-dailies-goto-today)
     
     (define-key global-map (kbd "C-c n C-r") 'org-roam-refile)
     (define-key global-map (kbd "C-c n r") 'org-roam-node-random)
     
     (define-key global-map (kbd "C-c n a r") 'org-roam-ref-add)
     (define-key global-map (kbd "C-c n a t") 'org-roam-tag-add)
     (define-key global-map (kbd "C-c n a a") 'org-roam-alias-add)
     
     (defun qz/consult-notes ()
       (interactive)
       (let ((default-directory org-roam-directory))
         (call-interactively 'consult-ripgrep)))
     
     (define-key global-map (kbd "C-c n s") 'qz/consult-notes)
     ;; NOWEB KBD END
     (defun qz/dwim-fold ()
       (interactive)
       (let ((fold-fn
              (cond ((qz/mode-is-or-derives 'org-mode)   'qz/org-fold)
                    ((qz/mode-is-or-derives 'magit-mode) 'magit-section-cycle)
                    ((qz/mode-is-or-derives 'outline-mode
                                            ;; .. i.e. elisp
                                            'lisp-data-mode
                                            ;; .. i.e. markdown
                                            'text-mode
                                            )
                     'outline-cycle)
                    (t (message "no dwim path configured, honey")))))
         (when (symbolp fold-fn)
           (qz/debug- (message "dn: %s" fold-fn))
           (call-interactively fold-fn))
         (recenter qz/recenter-line)))
     
     (defvar qz/recenter-line 4)
     (define-key global-map (kbd "s-TAB") 'qz/dwim-fold)
     (defun qz/dwim-unfold ()
       (interactive)
       (let ((fold-fn
              (cond ((qz/mode-is-or-derives 'org-mode)   'qz/org-unfold)
                    ((qz/mode-is-or-derives 'magit-mode) 'magit-section-cycle-global)
                    ((qz/mode-is-or-derives 'outline-mode
                                            ;; .. elisp
                                            'lisp-data-mode
                                            ;; .. markdown
                                            'text-mode)
                     'outline-cycle-buffer)
                    (t (message "no dwim path configured, honey")))))
         (when (symbolp fold-fn)
           (call-interactively fold-fn))))
     
     (define-key global-map (kbd "C-s-u") 'qz/dwim-unfold)
     (defun qz/org-show-all ()
       "
     - cycle-hook, show (headings blocks)
     
     call with prefix to really reveal ALL.
     
     "
       (interactive)
       (cond ((not current-prefix-arg)
              (message "denoise")
              (qz/org-denoise-point))
             (t
              (run-hook-with-args 'org-pre-cycle-hook 'all)
              (org-show-all '(headings blocks))
              (setq org-cycle-global-status 'all)
              (run-hook-with-args 'org-cycle-hook 'all)
              (org-cycle-hide-drawers 'all)
              (when (and (boundp 'org-indent-mode) org-indent-mode)
                ;; FIXME this is pretty slow -- maybe save-narrow?
                ;;  (scope 'visible' indent to current subtree)
                (org-indent-indent-buffer))
              (recenter qz/recenter-line)))
       ;; FOR SOME JAZZ, PRINT THE OUTLINE
       (org-display-outline-path 'with-self t))
     
     (defalias 'qz/org-unfold 'qz/org-show-all)
     (defun qz/org-denoise-point ()
       (interactive)
       (call-interactively 'qz/org-fold)
       ;;
       (save-excursion
         (org-back-to-heading)
         (org-cycle)
         (recenter qz/recenter-line))
       (org-cycle-hide-drawers 'all)
       (when (and (boundp 'org-indent-mode) org-indent-mode)
         ;; FIXME this is pretty slow -- maybe save-narrow?
         ;;  (scope 'visible' indent to current subtree)
         (org-indent-indent-buffer)))
     (define-key global-map (kbd "C-s-=") 'org-roam-node-insert)
     (define-key global-map (kbd "C-s-+") 'org-roam-node-find)
     
     (define-key global-map (kbd "C-s-f") 'org-roam-node-find)
     (define-key global-map (kbd "C-s-b") 'org-roam-node-insert)
     (defun qz/recenter ()
       (interactive)
       (recenter qz/recenter-line))
     
     (define-key global-map (kbd "C-M-=") 'qz/recenter)
     ;; NOWEB CUSTOM START
     (custom-set-variables
      '(org-imenu-depth 99))
     ;; NOWEB CUSTOM END
     (defun qz/sway-choose-window ()
       (interactive)
       (let* ((cmd "swaymsg -t get_tree | jq -r '
     .nodes | map(.
     | .current_workspace as $ws
     | .nodes
      | map(.
       | .nodes | map(.
        | \"\\(.id)::\\($ws)::\\(.app_id):\\(.name)\"
        ))) | add | add' | sed -e 's/\\[//g' -e 's/\\]//g' -e 's/,//g' -e 's/\"//g'")
              (cmd-res (shell-command-to-string cmd))
              (windows (s-split "\n" cmd-res))
              ;; cleanup 'windows,
              ;; - remove trailing/leading ws,
              ;; - remove empty strings
              (windows (remove "" (mapcar 's-trim windows)))
              (window-choice (completing-read "choose a window: " windows))
              ;; e.g "173::1::emacs:<2023-04-04>"
              (con-id (car (s-split "::" window-choice)))
              (focus-cmd (format "swaymsg '[con_id=%s] focus'" con-id)))
         (message "stuff %s" (list windows con-id focus-cmd))
         (list focus-cmd window-choice (shell-command-to-string focus-cmd))))
     
     (define-key global-map (kbd "M-<iso-lefttab>") 'qz/sway-choose-window)
     (define-key global-map (kbd "s-x") 'qz/sway-choose-window)
     (defvar qz/aws-env nil
       "the aws login configuration, managed through saml2aws
     
     to manipulate, run
     $ saml2aws login -a PROFILE_ALIAS
     
     files of note
     `$HOME/.aws/'
     `$HOME/.saml2aws'")
     (defun qz/choose-aws-env (&optional env)
       (interactive)
       (setq qz/aws-env
             (or env (completing-read
                      "aws-env: "
                      (->> (shell-command-to-string
                            "cat ~/.saml2aws | grep '^name' | cut -d'=' -f2")
                           (s-split "\n")
                           (remove "")))))
       (async-shell-command
        (format "saml2aws login -a %s && echo all good yo || echo uh-oh" qz/aws-env)
        "*aws*"
        "*error - aws*"))
     (defvar qz/kubectl-context nil
       "the operating kubernetes context.
     
     to check, at a shell, run:
     `$ kubectl config get-contexts -o name'
     or
     `$ kubectl config current-context")
     (defun qz/choose-kubectl-context (&optional ctx)
       (interactive)
       (setq qz/kubectl-context
             (or ctx (completing-read "k8s ctx: "
                                      (qz/shell-command-to-list-of-strings
                                       "kubectl config get-contexts -o name"))))
       (async-shell-command (format "kubectl config use-context %s"
                                    qz/kubectl-context)
                            "*kubectl*"))
     
     ;; optional; quality of life improvement to bury kubectl buffer
     (add-to-list 'display-buffer-alist '("*kubectl*" display-buffer-no-window))
     
     (defalias 'qz/choose-kubernetes-context 'qz/choose-kubectl-context )
     (defun qz/get-tabs ()
       (when-let* ((file (s-trim (shell-command-to-string "python3 $HOME/life/scratch/tabs.py")))
                   (file (and (not (string-empty-p file)) file)))
         (mapcar (lambda (o) (cons (gethash "title" o) o))
                 (cl-sort ;; accessed ASC
                  (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer))
                  'lt :key (lambda (o)
                             (gethash "accessed" o))))))
     
     (defun qz/tab (&optional tab insert?)
       (when-let* ((tab (or tab (cdr (car (qz/get-tabs)))))
                   (title (gethash "title" tab))
                   (url   (gethash "url" tab))
                   (text (if (derived-mode-p 'org-mode)
                             (org-make-link-string url title)
                           (format "%s %s :: %s" comment-start title url))))
         (when insert?
           (insert text)
           (org-end-of-line)
           (org-return))
         text))
     
     (defun qz/read-tab ()
       (interactive)
       (let* ((jd (qz/get-tabs))
              (choice (cdr (assoc
                            (consult--read
                             (mapcar 'car jd)
                             :prompt "choose tab: "
                             :default (car (seq-take jd 1))
                             :sort nil)
                            jd))))
         (qz/tab choice 'as-link)))
     
     (define-key global-map (kbd "C-c C-s-l") 'qz/read-tab)
     
     ;; nigari tofu heat at DuckDuckGo :: https://duckduckgo.com/?t=ffab&q=nigari+tofu+heat+&ia=web
     ;; Decompression (GNU Emacs Lisp Reference Manual) :: https://www.gnu.org/software/emacs/manual/html_node/elisp/Decompression.html
     ;; (qz/tab)
     (defun eos/narrow-or-widen-dwim (p)
       "Widen if buffer is narrowed, narrow-dwim otherwise.
     Dwim means: region, org-src-block, org-subtree, or
     defun, whichever applies first. Narrowing to
     org-src-block actually calls `org-edit-src-code'.
     
     With prefix P, don't widen, just narrow even if buffer
     is already narrowed."
       (interactive "P")
       (declare (interactive-only))
       (cond ((and (buffer-narrowed-p) (not p)) (widen))
             ((region-active-p)
              (narrow-to-region (region-beginning)
                                (region-end)))
             ((derived-mode-p 'org-mode)
              ;; `org-edit-src-code' is not a real narrowing
              ;; command. Remove this first conditional if
              ;; you don't want it.
              (cond ((ignore-errors (org-edit-src-code) t)
                     (delete-other-windows))
                    ((ignore-errors (org-narrow-to-block) t))
                    (t (org-narrow-to-subtree))))
             ((derived-mode-p 'latex-mode)
              (LaTeX-narrow-to-environment))
             (t (narrow-to-defun))))
     
     (define-key global-map (kbd "C-x C-n") 'eos/narrow-or-widen-dwim)
     (defun qz/yq-interactively ()
       "haha yaml loophole"
       (interactive)
       (let ((jq-interactive-command "yq"))
         (call-interactively 'jq-interactively)))
     (defun qz/insert-gpl ()
       "Insert the short brief of GNU GPL v3."
       (interactive)
       (save-mark-and-excursion
         (push-mark)
         (insert "
     <one line to give the program's name and a brief idea of what it does.>
     Copyright (C) <year>  <name of author>
     
     This program is free software: you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation, either version 3 of the License, or
     (at your option) any later version.
     
     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
     GNU General Public License for more details.
     
     You should have received a copy of the GNU General Public License
     along with this program. If not, see <http://www.gnu.org/licenses/>.")
         (comment-region (mark) (point))))
     (defun qz/recover-this-file-and-diff-buffer-with-file ()
       (interactive)
       (call-interactively 'recover-this-file)
       (call-interactively 'diff-buffer-with-file))
     (defun qz/p (token)
       (auth-source-pass-get 'secret token))
     
     (custom-set-variables
      '(sqlind-indentation-offsets-alist
        '((syntax-error sqlind-report-sytax-error)
          (in-string sqlind-report-runaway-string)
     
     
          (comment-continuation sqlind-indent-comment-continuation)
          (comment-start sqlind-indent-comment-start)
          (toplevel 0)
          (in-block +)
          (in-begin-block +)
          (block-start 0)
          (block-end 0)
          (declare-statement +)
          (package ++)
          (package-body 0)
          (create-statement +)
          (defun-start +)
          (labeled-statement-start 0)
          (statement-continuation +)
          (nested-statement-open sqlind-use-anchor-indentation +)
          (nested-statement-continuation sqlind-use-previous-line-indentation)
          (nested-statement-close sqlind-use-anchor-indentation)
          (with-clause sqlind-use-anchor-indentation)
          (with-clause-cte +)
          (with-clause-cte-cont ++)
          (case-clause 0)
          (case-clause-item sqlind-use-anchor-indentation +)
          (case-clause-item-cont sqlind-right-justify-clause)
          (select-clause sqlind-right-justify-clause)
          (select-column sqlind-indent-select-column)
          (select-column-continuation sqlind-indent-select-column +)
          ;;(select-table-continuation 0)
          ;; ((default . ++) (kinda . +) ( . sqlind-use-anchor-indentation))
          (select-join-condition ++) ; this should wrap
          (select-table sqlind-indent-select-table)
          (select-table-continuation sqlind-indent-select-table +)
          (in-select-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
          (insert-clause sqlind-right-justify-clause)
          (in-insert-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
          (delete-clause sqlind-right-justify-clause)
          (in-delete-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
          (update-clause sqlind-right-justify-clause)
          (in-update-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator))))
     (custom-set-variables
      '(sqlind-default-indentation-offsets-alist
        '((syntax-error sqlind-report-sytax-error)
          (in-string sqlind-report-runaway-string)
          (comment-continuation sqlind-indent-comment-continuation)
          (comment-start sqlind-indent-comment-start)
          (toplevel 0)
          (in-block +)
          (in-begin-block +)
          (block-start 0)
          (block-end 0)
          (declare-statement +)
          (package ++)
          (package-body 0)
          (create-statement +)
          (defun-start +)
          (labeled-statement-start 0)
          (statement-continuation +)
          (nested-statement-open sqlind-use-anchor-indentation +)
          (nested-statement-continuation sqlind-use-previous-line-indentation)
          (nested-statement-close sqlind-use-anchor-indentation)
          (with-clause sqlind-use-anchor-indentation)
          (with-clause-cte +)
          (with-clause-cte-cont ++)
          (case-clause 0)
          (case-clause-item sqlind-use-anchor-indentation +)
          (case-clause-item-cont sqlind-right-justify-clause)
          (select-clause sqlind-right-justify-clause)
          (select-column sqlind-indent-select-column)
          (select-column-continuation sqlind-indent-select-column +)
          (select-join-condition -- --)
          (select-table sqlind-indent-select-table)
          ;;(select-table-continuation sqlind-indent-select-table +)
          (select-table-continuation sqlind-lineup-joins-to-anchor)
          (in-select-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
          (insert-clause sqlind-right-justify-clause)
          (in-insert-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
          (delete-clause sqlind-right-justify-clause)
          (in-delete-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
          (update-clause sqlind-right-justify-clause)
          (in-update-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator))))
     (defun qz/sqlfluff-region (&optional beg end info)
       "uses src block by default"
       (interactive)
       (let* ((info (or info (org-babel-get-src-block-info)))
              (dialect (or (and info (cl-case
                                         (intern (cdr (assoc :engine (nth 2 info))))
                                       (bq "bigquery")
                                       (postgres "postgresql")
                                       (t (error "🙄 my lazy ass"))))
                           ;; default
                           "bigquery")))
         (if info
             (save-mark-and-excursion
               (org-babel-mark-block))
           ;; default
           (unless (region-active-p)
             (mark-whole-buffer)))
         (cl-destructuring-bind
             (beg end)
             (or (and beg end (list beg end))
                 (list (region-beginning) (region-end)))
           (shell-command-on-region
            beg end
            (format "sqlfluff fix --FIX-EVEN-UNPARSABLE --force --disable-progress-bar --dialect %s -"
                    dialect)
            nil ;; no output buffer
            'replace))))
     (setq org-babel-default-header-args:sqlite '((:colnames . "yes")))
     (defun qz/bq-consult-attrs ()
       (interactive)
       (let ((d (mapcar
                 (lambda (line) (s-split "::" line))
                 ;; lines
                 (s-split "\n"
                          ;; remove last empty line
                          (s-trim (with-temp-buffer
                                    ;; TODO memoize
                                    (mapc 'insert-file
                                          '("~/.cache/bq-scrape-p-t-business-intelligence-0da1.txt"
                                            "~/.cache/bq-scrape-p-t-order-management-svc-6996.txt"))
                                    (buffer-string)))))))
         (consult--read
          d
          :prompt "choose attr: "
          :annotate
          (lambda (c)
            (concat
             (propertize " " 'display '(space :align-to center))
             (nth 1 (assoc c d))
             (propertize " " 'display '(space))
                                             ;(nth 2 (assoc c d))
     
             ))
          :sort nil)))
     
     (defun qz/bq-read-attrs ()
       (interactive)
       (let ((d (mapcar
                 (lambda (line) (s-split "::" line))
                 ;; lines
                 (s-split "\n"
                          ;; remove last empty line
                          (s-trim (with-temp-buffer
                                    ;; TODO memoize
                                    (insert-file "~/.cache/bq-schema.txt")
                                    (buffer-string)))))))
         (insert (completing-read
                  "choose attr: "
                  (mapcar 'car d))
                 )))
     
     
     (setq temp-buffer-show-function 'hkey-help-show
           temp-buffer-show-hook (remove 'hkey-help-show
                                         temp-buffer-show-hook))
     (add-to-list 'auto-mode-alist
                  '("\\.tml\\'" . yaml-mode))
     (defun qz/add-pdb-py-debug ()
       "add debug code and move line down"
       (interactive)
       (back-to-indentation)
       (insert "import pdb; pdb.set_trace();\n"))
     (custom-set-variables '(python-indent-offset 4))
     (add-to-list 'auto-mode-alist
                  '("\\.pyx\\'" . python-mode))
     (with-eval-after-load 'python-black
       (setq python-black--base-args '("--quiet" "-l 78" "--preview")))
     ;; NOWEB GOLANG START
     (with-eval-after-load 'go-mode
       (setq gofmt-command "golines")
       (add-hook 'go-mode-hook
                 (lambda () (add-hook 'before-save-hook
                                      'gofmt-before-save
                                      nil 'local)))
       )
     ;; NOWEB GOLANG END
     ;; NOWEB ES START
     (with-eval-after-load 'restclient
       (defun qz/es-choose-url (&optional url backend env)
         (interactive)
         (and qz/debug (message "DEBUG qz/es-choose-url: %s"
                                (list url backend env)))
         (let* ((backend (qz/es-choose-backend backend))
                (url (or url
                         (and backend env
                              (qz/es-choose-env env)
                              (format qz/newstore-es-string backend env)))))
           (message "es-default-url: %s"
                    (setq es-default-url
                          (or url (completing-read
                                   "es-url: " qz/newstore-es-urls)))))
         es-default-url)
       
       (defun qz/es-choose-backend (&optional backend)
         (interactive)
         (and qz/debug (message "DEBUG qz/es-choose-backend: %s" backend))
         (message "qz/newstore-es-backend-current: %s"
                  (setq qz/newstore-es-backend-current
                        (or backend (completing-read "es-backend: " qz/newstore-es-backends))))
         qz/newstore-es-backend-current)
       
       (defun qz/es-choose-env (&optional env)
         (interactive)
         (and qz/debug (message "DEBUG qz/es-choose-env: %s" env))
         (message "qz/newstore-es-env-current: %s"
                  (setq qz/newstore-es-env-current
                        (or env (completing-read "es-env: " qz/newstore-envs))))
         qz/newstore-es-env-current)
       
       (defun qz/test-es-ui (&optional url backend env)
         (setq qz/newstore-es-env-current nil
               qz/newstore-es-backend-current nil)
         (funcall-interactively 'qz/es-choose-url url backend env)
         (list
          qz/newstore-es-env-current
          qz/newstore-es-backend-current
          es-default-url))
       
       ;;(qz/test-es-ui)              ;; prompt, noset
       ;;(qz/test-es-ui nil)          ;; prompt, noset
       ;;(qz/test-es-ui nil nil)      ;; prompt, noset
       ;;(qz/test-es-ui nil nil nil)  ;; prompt, noset
       ;;(qz/test-es-ui nil 'kibana 'production)    ;; noprompt, set
       
       (defun qz/es-choose-cookie-headers ()
         "TODO"
         (interactive)
         (message
          "es-default-headers: %s"
          (setq es-default-headers `(("Content-Type" . "application/json; charset=UTF-8")
                                     ("Cookie" . ,(format "ACCEZZIOCOOKIE=%s"
                                                          (read-from-minibuffer "es cookie: ")))))))
       (setq es-default-url "https://elasticsearch-production.newstore.luminatesec.com"
             es-current-url es-default-url
             es-default-headers nil
             es-always-pretty-print t
             es-default-headers
             `(("Content-Type" . "application/json; charset=UTF-8")
               ("Cookie" . ,(format "ACCEZZIOCOOKIE=%s"
                                    "11fdbe68-b0f3-4dd0-9894-f97afe3662dc"))))
       
       (setq qz/newstore-es-string "https://%s-%s.newstore.luminatesec.com"
             qz/newstore-es-backends '(kibana elasticsearch)
             qz/newstore-es-backend-current nil
             qz/newstore-es-env-current nil
             qz/newstore-es-urls (cl-loop for env in qz/newstore-envs
                                          append (cl-loop for es-backend in qz/newstore-es-backends
                                                          collect (format qz/newstore-es-string es-backend env))))
       )
     ;; NOWEB ES END
     ;; NOWEB RESTCLIENT START
     (with-eval-after-load 'restclient
       (defvar qz/restclient-env nil)
       
       (defun qz/restclient-choose-env (&optional env)
         (interactive)
         (message "qz/restclient-env: %s"
                  (setq qz/restclient-env
                        (cdr (assoc (intern (or env
                                                (completing-read "restclient-env: " qz/newstore-envs)))
                                    qz/newstore-envs-abbrev))))
         qz/restclient-env)
       (defvar qz/restclient-tenant nil)
       
       (defun qz/restclient-choose-tenant (&optional tenant)
         (interactive)
         (message "qz/restclient-tenant: %s"
                  (setq qz/restclient-tenant
                        (or tenant (completing-read
                                    "restclient-tenant: " qz/newstore-tenants))))
         qz/restclient-tenant)
       (defvar qz/restclient-token nil)
       (defvar qz/restclient-token-field 'access_token)
       
       (defun qz/restclient-hook ()
         "Update token from a request."
         ;; url is visible while the hook is running.
         (let ((result))
           (save-excursion
             (cond
              ((string-suffix-p "/token" url)
               (condition-case nil
                   (progn
                     (setq result (cdr (assoc qz/restclient-token-field (json-read))))
                     (when (stringp result)
                       (progn
                         (setq qz/restclient-token result)
                         (message (concat "stored token: " qz/restclient-token)))))
                 (error (message "That wasn't cleanly handled."))))))))
       
       (add-hook 'restclient-response-loaded-hook 'qz/restclient-hook)
       (provide 'restclient-hooks)
       )
     ;; NOWEB RESTCLIENT END
     (setq magit-bind-magit-project-status t)
     (with-eval-after-load 'project
       (with-eval-after-load 'magit
         ;; Only more recent versions of project.el have `project-prefix-map' and
         ;; `project-switch-commands', though project.el is available in Emacs 25.
         (when (and magit-bind-magit-project-status
                    (boundp 'project-prefix-map))
           (unless ;; Only modify if it hasn't already been modified.
               (equal project-switch-commands
                      (eval (car (get 'project-switch-commands 'standard-value))
                            t))
             (message "qz: setting magit-project-status, but project-switch-commands has been changed already"))
           (progn
             (define-key project-prefix-map "m" 'magit-project-status)
             (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))))
     (with-eval-after-load 'magit
       (if (fboundp 'magit-todos-mode)
           (funcall 'magit-todos-mode)
         (message "qz: config :: magit-todos-mode is not loaded")))
     (setq js-indent-level 2)
     (with-eval-after-load 'chatgpt-shell
       (setq chatgpt-shell-openai-key
             (auth-source-pass-get 'secret "chatgpt/api/emacs")
             dall-e-shell-openai-key chatgpt-shell-openai-key))
     
     (with-eval-after-load 'gptel
       (setq gptel-api-key
             (lambda () (auth-source-pass-get 'secret "chatgpt/api/emacs")))
       (setq gptel-default-mode 'org-mode)
       ;; Llama.cpp offers an OpenAI compatible API
       (gptel-make-openai "llama-cpp"          ;Any name
                          :stream t                             ;Stream responses
                          :protocol "http"
                          :host "localhost:8080"                ;Llama.cpp server location
                          :models '("test"))                    ;Any names, doesn't matter for Llama
       )
     (setq qz/llamafile-args
           '(;; gpu offload
             "-ngl 9999"
             ;;"--nobrowser"
             ))
     
     (defun qz/llamafile ()
       (interactive)
       (async-shell-command
        (format "%s %s"
                ;; choose model,
                (cadr (s-split
                       "	" (completing-read
                                "model: " (s-split "\n" (s-trim (shell-command-to-string
                                                                 "du -sh ~/dl/*.llamafile"))))))
                ;; args
                (s-join " " qz/llamafile-args))
        "*process:llamafile*"))
     ;; NOWEB CONSULT START
     (with-eval-after-load 'consult
       (require 's)
       
       (defun qz/consult-ripgrep-files (files)
         (let* ((consult-ripgrep-args (concat consult-ripgrep-args " -L"))
                (rg-dir "/tmp/null"))
           (f-delete rg-dir t)
           (mkdir rg-dir t)
           (mapcar (lambda (f)
                     (f-symlink (expand-file-name f)
                                (format "%s/%s-%s"
                                        rg-dir (gensym) (s-replace "/" "-" f))))
                   files)
           (consult-ripgrep rg-dir)))
       (defun qz/consult-ripgrep-bookmark ()
         (interactive)
         (let ((files (mapcar (lambda (b) (cdr (assoc 'filename b)))
                              bookmark-alist)))
           (qz/consult-ripgrep-files files)))
       
       (define-key global-map (kbd "C-c b s") 'qz/consult-ripgrep-bookmark)
       (define-key global-map (kbd "C-x C-M-SPC") 'consult-global-mark)
       (with-eval-after-load 'project
         (define-key project-prefix-map (kbd "M-g") 'consult-ripgrep)
         (add-to-list 'project-switch-commands '(consult-ripgrep "ripgrep") t))
       (mapcar (lambda (bind)
                 (define-key global-map (kbd (car bind)) (cadr bind)))
               '(("C-x b" consult-buffer)))
       )
     ;; NOWEB CONSULT END
     (defun qz/consult-org-agenda-dailies ()
       (interactive)
       (let ((org-agenda-files (s-split "\n" (s-trim (shell-command-to-string "
     
     find $HOME/life/roam/daily -type f -name '*.org' -printf '%T+\t%p\n' \
       | sort | tail -n30 \
       | awk '{print $2}'
     
     ")))))
         (consult-org-agenda)))
     
     (define-key global-map (kbd "C-c n C-s") 'qz/consult-org-agenda-dailies)
     ;;; SRC as of [2023-07-04 Tue 11:12]
     ;;
     ;; (defun consult--fontify-all ()
     ;;   "Ensure that the whole buffer is fontified."
     ;;   ;; Font-locking is lazy, i.e., if a line has not been looked at yet, the line
     ;;   ;; is not font-locked.  We would observe this if consulting an unfontified
     ;;   ;; line.  Therefore we have to enforce font-locking now, which is slow.  In
     ;;   ;; order to prevent is hang-up we check the buffer size against
     ;;   ;; `consult-fontify-max-size'.
     ;;   (when (and consult-fontify-preserve jit-lock-mode
     ;;              (< (buffer-size) consult-fontify-max-size))
     ;;     (jit-lock-fontify-now)))
     
     (defun qz/fontify-all ()
       "Useful when using org-modern, which is *abhorrent* when
      partially font-locked"
       (interactive)
       (consult--fontify-all))
     ;; original, as of commit TODO
     ;;
     ;; (defun consult-recoll--command (text)
     ;;   "Command used to perform queries for TEXT."
     ;;   (setq consult-recoll--current nil)
     ;;   (setq consult-recoll--index 0)
     ;;   (setq consult-recoll--snippets nil)
     ;;   `("recoll" ,@(consult-recoll--search-flags) ,text))
     ;;
     ;; we need to add the -t arg (to make it exactly like recollq)
     
     (defun consult-recoll--command (text)
       "Command used to perform queries for TEXT."
       (setq consult-recoll--current nil)
       (setq consult-recoll--index 0)
       (setq consult-recoll--snippets nil)
       `("recoll" "-t" ,@(consult-recoll--search-flags) ,text))
     ;; NOWEB EMBARK START
     (define-key global-map (kbd "C-.") 'embark-act)
     (with-eval-after-load 'embark
       ;;; https://www.reddit.com/r/emacs/comments/pac8kp/how_to_send_an_embark_command_without_pulling_up/
       ;;; (defun shell-command-on-current-completion-candidate ()
       ;;;   (interactive)
       ;;;   (embark--act 'shell-command (car (embark--targets))))
       
       (defun qz/example-embark-act-arbitrary ()
         (interactive)
         (embark--act 'shell-command (car (embark--targets))))
       
       (defun qz/embark-select-and-next ()
         (interactive)
         (call-interactively 'embark-select)
         (call-interactively 'vertico-next))
       (defun qz/embark-select-and-previous ()
         (interactive)
         (call-interactively 'embark-select)
         (call-interactively 'vertico-previous))
       
       (define-key minibuffer-mode-map (kbd "C-M-n") 'qz/embark-select-and-next)
       (define-key minibuffer-mode-map (kbd "C-M-p") 'qz/embark-select-and-previous)
       )
     ;; NOWEB EMBARK END
     (with-eval-after-load 'eww
       (define-key eww-mode-map (kbd "C-<return>") 'eww-open-in-new-buffer)
       )
     (with-eval-after-load 'git-gutter-transient
       (defun git-gutter-transient:magit-commit ()
         "Close hunk buffer and call `magit-file-dispatch'."
         (interactive)
         (git-gutter-transient:quit)
         (magit-commit))
       
       (defun git-gutter-transient:stage-magit-commit ()
         "Close hunk buffer and call `magit-file-dispatch'."
         (interactive)
         (git-gutter-transient:quit)
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
                   ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
           (git-gutter:stage-hunk))
         (magit-commit))
       
       
       (let ((end (- (length (nth 3 (seq--into-list
                                     (transient-get-suffix  'git-gutter-transient
                                                            '(0)))))
                     1)))
         ;; ... seems a totally goofy way to simply push stuff to the end
         ;; of a list...
         (transient-append-suffix 'git-gutter-transient `(0 ,end)
           (vector '("c" "Commit" git-gutter-transient:magit-commit)))
         (transient-append-suffix 'git-gutter-transient `(0 ,(+ 1 end))
           (vector '("C" "Stage & Commit" git-gutter-transient:stage-magit-commit))))
       (defun qz/git-gutter:popup-hunk ()
         (interactive)
         (ignore-errors (git-gutter:popup-hunk)))
       
       (qz/advice- git-gutter-transient :after qz/git-gutter:popup-hunk)
       (defun qz/filter--updated-timestamp (orig-fun &rest R)
         (let ((before-save-hook (remove 'qz/org-roam--updated-timestamp
                                         before-save-hook)))
           (funcall orig-fun)))
       
       (qz/advice- git-gutter:revert-hunk :around qz/filter--updated-timestamp)
     )
     (with-eval-after-load 'justify-kp
       (defun pj-line-width ()
         (* (max 1 text-scale-mode-amount) 600)))
     (with-eval-after-load 'ligature
       (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "--n-" "-<<"
                                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                            "\\\\" "://")) ;;
       ;; Enables ligature checks globally in all buffers. You can also do it
       ;; per mode with `ligature-mode'.
       (global-ligature-mode t))
     ;; (string-to-char "⋈")
     (setq qz/pretty-logicals
           (mapcan (lambda (e) (list e (cons (upcase (car e)) (cdr e))))
                   '(("lambda"       . 955)  ; λ  _LAMBDA
                     ("union"        . 8746) ; ∪  _UNION
                     ("union all"    . 8746) ; ""
                     ("and"          . 8743) ; ∧  LOGICAL AND
                     ("or"           . 8744) ; ∨  LOGICAL OR
                     ("intersect"    . 8745) ; ∩  INTERSECTION
                     ("with"         . 9723) ; ◻ IT IS NECESSARY THAT  (dubious)
                     ("select"       . 928)  ; Π  PROJECT
                     ("as"           . 8788) ; ≔  IS DEFINED AS         (dubious)
                     ("from"         . 8704) ; ∀  FOR ALL               (dubious)
                     ("where"        . 963)  ; σ  SELECT
                     ("not"          . 172)  ; ¬  NOT SIGN
                     ("in"           . 8712) ; ∈  MEMBER OF
                     ("inner join"   . 8904) ; ⋈  NATURAL (INNER) JOIN
                     ("contains"     . 8712) ; ""
                     ("not in"       . 8713) ; ∉ NOT MEMBER OF
                     ("not contains" . 8712) ; ""
                     ;; ∴ therefore
                     ;; ∵ because
                     ;; ∃ 8707
                     ;; ∃ EXISTS ONE (UNIQUE)
                     ;; ∄ not exists
                     ;; σ_φ(R)
                     ))
           qz/pretty-text
           '(("[...]" . 8943) ; ⋯  MIDLINE HORIZONTAL ELLIPSIS
             ;;("clocking" . 128339)
             ))
     
     (defun my-pretty--add (s)
       "extend prettify-symbols-alist with s"
       (setq prettify-symbols-alist (seq-uniq (append prettify-symbols-alist s))))
     
     (defun my-pretty-logicals ()
       (my-pretty--add qz/pretty-logicals))
     
     (defun my-pretty-text ()
       (my-pretty--add qz/pretty-text))
     
     (add-hook 'prog-mode-hook 'my-pretty-logicals)
     (add-hook 'text-mode 'my-pretty-text)
     (add-hook 'org-mode 'my-pretty-text)
     (global-prettify-symbols-mode 1)
     (cl-loop for c from (- 8746 10) to (+ 8746 50)
              collect (cons c (char-to-string c)))
     (global-hl-todo-mode 1)
     (require 'hyperbole)
     (define-key global-map (kbd "C-<mouse-2>") 'hkey-either)
     (define-key global-map (kbd "M-<return>") 'hkey-either)
     (defun qz/mold ()
       (interactive)
       (unless (fboundp 'me-mold)
         (require 'moldable-emacs)
         (me-setup-molds))
       (call-interactively 'me-mold))
     ;;(setq me-molds-debug-on t)
     
     (with-eval-after-load 'moldable-emacs
       (require 'dash)
     
       (defun qz/me-usable-molds (&optional molds buffer)
         "Return the usable molds among the `me-available-molds'.
     Optionally you can pass your own candidate MOLDS.
     Optionally you can pass a BUFFER to use instead of the `current-buffer'."
         (let ((_ (setq me-usable-mold-stats nil))
               (molds (or molds me-available-molds))
               (buffer (or buffer (current-buffer))))
           (with-current-buffer buffer
             (--filter
              (let* ((beginning (current-time))
                     (result (save-excursion
                               (condition-case err
                                   (me-mold-run-given it)
                                 (error (message "me-usable-molds: error in :given of %s:\n   %s" (plist-get it :key) err))))) ; TODO composite molds
                     (ending (current-time))
                     (_ (when me-molds-debug-on
                          (add-to-list 'me-usable-mold-stats (list :time
                                                                   (time-to-seconds
                                                                    (time-subtract
                                                                     ending
                                                                     beginning))
                                                                   :nmold (plist-get it q:key)
                                                                   :given (plist-get it :given))))))
                result) ;; TODO run this in parallel when time goes over 100ms)
              molds))))
     
       ;; TODO macro; dump all molds as M-x
       (defmacro qz/me-mold--M-x ()
         `(progn
            (ignore-errors
              ,@(--map
                 (let* ((k (plist-get it :key))
                        (f (intern (concat "qz/me-mold--" k))))
                   `((fmakunbound ',f)
                     (defun ,f ()
                       (interactive)
                       (if (me-mold-run-given ',it)
                           (me-mold , k)
                         (message "me-mold: Cannot run '%s' here" ,k)))))
                 me-available-molds))))
     
       (qz/me-mold--M-x))
     ;;(qz/me-mold--Stats)
     (with-eval-after-load 'modus-themes
       (defun qz/modus-themes-reload ()
         (interactive)
         (when current-prefix-arg
           (setq modus-themes-common-palette-overrides qz/modus-overrides))
         (when-let* ((modus-themes-custom-auto-reload t)
                     (theme (modus-themes--current-theme)))
           (modus-themes-load-theme theme)))
       ;; Apply gray scale foreground, background, and overline (headings 0-8)
       ;; headings are oblique (face:bold)
       (setq
        qz/modus-overrides
        (append
         '(
           (default light) ;; TODO default text is light
           )
         (cl-loop
          for pre in '("" "fg-") append
          (cl-loop
           for n from 1 to 8
           append (cl-labels ((s (pre) (intern (format "%s-heading-%s" pre n))))
                    `((,(s 'fg)
                       ;;; what color should the headings be?
                       ;;fg-main   ;; 1.  whatever the main text color is.
                       ;;"#333"    ;; 2.  a lightening.
                       "#666"      ;; 2.  a lightening, 2.
                       )
                      ;;(,(s 'bg) nil)
                      ))))))
       ;;; (qz/modus-themes-reload)
       (qz/modus-themes-reload)
       )
     (defun mpv-osd ()
       "Show the osd"
       (interactive)
       (mpv--enqueue '("set_property" "osd-level" "3") 'ignore))
     
     (defun mpv-screenshot ()
       "Take a screenshot"
       (interactive)
       (mpv--enqueue '("screenshot") 'ignore))
     
     
     
     (defun mpv-frame-step ()
       "Step one frame forward."
       (interactive)
       (mpv--enqueue '("frame-step") 'ignore))
     
     
     (defun mpv-frame-back-step ()
       "Step one frame backward."
       (interactive)
       (mpv--enqueue '("frame-back-step") 'ignore))
     
     
     (with-eval-after-load
         'project
       (defun project-vterm ()
         "Start vterm in the current project's root directory.
     If a buffer already exists for running vterm in the project's root,
     switch to it.  Otherwise, create a new vterm buffer.
     With \\[universal-argument] prefix arg, create a new vterm buffer even
     if one already exists."
         (interactive)
         (let* ((default-directory (project-root (project-current t)))
                (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
                (vterm-buffer (get-buffer vterm-buffer-name)))
           (if (and vterm-buffer (not current-prefix-arg))
               (pop-to-buffer-same-window vterm-buffer)
             (vterm t))))
       (define-key project-prefix-map (kbd "t") 'project-vterm)
       (add-to-list 'project-switch-commands
                    '(project-vterm
                      "Start vterm in the current project's root directory.")))
     (setq qz/screenshot-tool "grim"
           qz/screenshot-rect "screenshot") ;; grim -g \"$(slurp)\"
     ;; ~/.local/bin/screenshot
     
     (defun password-store-otp-append-from-image (entry)
       "Check clipboard for an image and scan it to get an OTP URI, append it to ENTRY."
       (interactive (list (read-string "Password entry: ")))
       (let ((qr-image-filename (password-store-otp--get-qr-image-filename entry)))
         (when (not (zerop
                     (shell-command (format "%s %s" qz/screenshot-rect qr-image-filename))))
           (error "Couldn't get image from clipboard"))
         (with-temp-buffer
           (condition-case nil
               (call-process "zbarimg" nil t nil "-q" "--raw"
                             qr-image-filename)
             (error
              (error "It seems you don't have `zbar-tools' installed")))
           (password-store-otp-append
            entry
            (buffer-substring (point-min) (point-max))))
         (when (not password-store-otp-screenshots-path)
           (delete-file qr-image-filename))))
     (require 'atomic-chrome)
     (atomic-chrome-start-server)
     (setq atomic-chrome-port--firefox 4001
           atomic-chrome-port--chrome 4002
           atomic-chrome-port--gcp 4003)
     
     (defun qz/atomic-chrome-choose (browser)
       (interactive
        (list
         (completing-read "browser, port: " '(firefox chrome gcp))))
       (setq atomic-chrome-server-ghost-text-port
             (symbol-value (intern (concat "atomic-chrome-port--" browser))))
       (atomic-chrome-stop-server)
       (atomic-chrome-start-server)
       (message "qz: atomic-chrome: Using %s:%s"
                browser atomic-chrome-server-ghost-text-port))
     (setq atomic-chrome-url-major-mode-alist
           '(;;; jupyter
             ("localhost:8890" . python-mode)
             ("localhost:8888" . python-mode)
             ("datacamp.com" . python-mode)
             ("notebooks.googleusercontent.com" . python-mode)
             ;;; etc
             ))
     (setq atomic-chrome-default-major-mode 'org-mode)
     (defun qz/month-files ()
       (split-string
        (shell-command-to-string
         "rg '2023-04-1' ~/life/roam/ -c -t org | grep -vE 'org#' | awk -F '[,:]' '{print $1}'")))
     (defun qz/org-link-highlight-last (&rest args)
       (qz/debug- (message "highlight-last: args :: %s" args))
       ;; XXX search back through stack & determine if org-store-link is
       ;; interactive?
       (when-let ((s (cadar org-stored-links)))
         (highlight-phrase (s-trim s)
                           (qz/anno-face))))
     
     (qz/advice- org-store-link :after qz/org-link-highlight-last)
     (setq org-timer-default-timer  "00:25:00")
     (setq qz/clock-sounds '("~/vids/gong-cut.wav"
                             "~/vids/sing-cut.wav")
           org-clock-sound (cadr qz/clock-sounds))
     (defun qz/org-timer-set-timer (&optional duration)
       "ergonomics
     
     if no prefix arg, set timer with value org-timer-default-timer.
     if prefix arg, prompt for time (no arg call of 'org-timer-set-timer)
     
     how this is achieved, ontop of org semantics:
     
     we use the '(4) prefix arg, to respect a running timer, as follows (from doc of org-timer-set-timer):
     
     > Called with a C-u prefix arguments, use ‘org-timer-default-timer’
     > without prompting the user for a duration."
       (interactive)
       (let* ((current-prefix-arg (if (or current-prefix-arg prefix-arg)
                                      nil '(4))))
         (call-interactively 'org-timer-set-timer)))
     (define-key global-map (kbd "s-u") 'qz/org-timer-set-timer)
     (defun qz/slug (str &optional allow-double-hyphens)
       "Convert string STR to a `slug' and return that string.
     
     A `slug' is the part of a URL which identifies a particular page
     on a website in an easy to read form.
     
     Example: If STR is \"My First Post\", it will be converted to a
     slug \"my-first-post\", which can become part of an easy to read
     URL like \"https://example.com/posts/my-first-post/\".
     
     In general, STR is a string.  But it can also be a string with
     Markdown markup because STR is often a post's sub-heading (which
     can contain bold, italics, link, etc markup).
     
     The `slug' generated from that STR follows these rules:
     
     - Contain only lower case alphabet, number and hyphen characters
       ([[:alnum:]-]).
     - Not have *any* HTML tag like \"<code>..</code>\",
       \"<span class=..>..</span>\", etc.
     - Not contain any URLs (if STR happens to be a Markdown link).
     - Replace \".\" in STR with \"dot\", \"&\" with \"and\",
       \"+\" with \"plus\".
     - Replace parentheses with double-hyphens.  So \"foo (bar) baz\"
       becomes \"foo--bar--baz\".
     - Replace non [[:alnum:]-] chars with spaces, and then one or
       more consecutive spaces with a single hyphen.
     - If ALLOW-DOUBLE-HYPHENS is non-nil, at most two consecutive
       hyphens are allowed in the returned string, otherwise consecutive
       hyphens are not returned.
     - No hyphens allowed at the leading or trailing end of the slug."
       (let* (;; All lower-case
              (str (downcase str))
              ;; Remove "<FOO>..</FOO>" HTML tags if present.
              (str (replace-regexp-in-string "<\\(?1:[a-z]+\\)[^>]*>.*</\\1>" "" str))
              ;; Remove URLs if present in the string.  The ")" in the
              ;; below regexp is the closing parenthesis of a Markdown
              ;; link: [Desc](Link).
              (str (replace-regexp-in-string (concat "\\](" ffap-url-regexp "[^)]+)") "]" str))
              ;; Replace "&" with " and ", "." with " dot ", "+" with
              ;; " plus ".
              (str (replace-regexp-in-string
                    "&" " and "
                    (replace-regexp-in-string
                     "\\." " dot "
                     (replace-regexp-in-string
                      "\\+" " plus " str))))
              ;; Replace all characters except alphabets, numbers and
              ;; parentheses with spaces.
              (str (replace-regexp-in-string "[^[:alnum:]()]" " " str))
              ;; On emacs 24.5, multibyte punctuation characters like "："
              ;; are considered as alphanumeric characters! Below evals to
              ;; non-nil on emacs 24.5:
              ;;   (string-match-p "[[:alnum:]]+" "：")
              ;; So replace them with space manually..
              (str (if (version< emacs-version "25.0")
                       (let ((multibyte-punctuations-str "：")) ;String of multibyte punctuation chars
                         (replace-regexp-in-string (format "[%s]" multibyte-punctuations-str) " " str))
                     str))
              ;; Remove leading and trailing whitespace.
              (str (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" str))
              ;; Replace 2 or more spaces with a single space.
              (str (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " str))
              ;; Replace parentheses with double-hyphens.
              (str (replace-regexp-in-string "\\s-*([[:space:]]*\\([^)]+?\\)[[:space:]]*)\\s-*" " -\\1- " str))
              ;; Remove any remaining parentheses character.
              (str (replace-regexp-in-string "[()]" "" str))
              ;; Replace spaces with hyphens.
              (str (replace-regexp-in-string " " "-" str))
              ;; Remove leading and trailing hyphens.
              (str (replace-regexp-in-string "\\(^[-]*\\|[-]*$\\)" "" str)))
         (unless allow-double-hyphens
           (setq str (replace-regexp-in-string "--" "-" str)))
         str))
     (defun qz/ensure-alias (alias &optional node)
       (let ((node (or node  (org-roam-node-at-point 'assert))))
         (save-excursion
           (with-current-buffer (find-file-noselect (org-roam-node-file node))
             (goto-char (org-roam-node-point node))
             (org-roam-alias-add alias)))))
     (defun qz/should-be-private-p (file)
       (with-current-buffer (or (find-buffer-visiting file)
                                (find-file-noselect file))
         (qz/is-private-p)))
     
     (defun qz/is-file-private ()
       (interactive)
       (message (concat "should " (f-this-file) " be private..? "
                        (or (and (qz/should-be-private-p (f-this-file)) "yes") "no"))))
     (defun qz/is-daily-p (&optional node &rest _)
       (if-let ((title (qz/node-title node)))
           (string-match-p qz/daily-title-regexp title)))
     (defun qz/is-project-p (&optional node &rest _)
       (or (qz/file-has-todo-p node _)
           (qz/is-daily-p node _)))
     (defun qz/is-person-p (&optional node &rest _)
       (qz/has-link-to-p (qz/title->roam-id "person")
                         (and node (org-roam-node-p node)
                              (org-roam-node-id node))))
     (defun qz/file-has-todo-p (&optional node &rest _)
       "Return non-nil if current buffer has any todo entry.
     
     TODO entries marked as done are ignored, meaning the this
     function returns nil if current buffer contains only completed
     tasks.
     
     (1) parse the buffer using org-element-parse-buffer. It
       returns an abstract syntax tree of the current Org buffer. But
       since we care only about headings, we ask it to return only them
       by passing a GRANULARITY parameter - 'headline. This makes
       things faster.
     
     (2) Then we extract information about TODO keyword from
       headline AST, which contains a property we are interested in -
       :todo-type, which returns the type of TODO keyword according to
       org-todo-keywords - 'done, 'todo or nil (when keyword is not
       present).
     
     (3) Now all we have to do is to check if the buffer list contains
       at least one keyword with 'todo type. We could use seq=find on
       the result of org-element-map, but it turns out that it provides
       an optional first-match argument that can be used for our needs."
       (save-excursion
         (with-current-buffer (or (and node (org-roam-node-p node)
                                       (find-file-noselect (org-roam-node-file node)))
                                  (current-buffer))
           (org-with-wide-buffer
            (org-element-map                          ; (2)
                (org-element-parse-buffer 'headline) ; (1)
                'headline
              (lambda (h)
                (eq (org-element-property :todo-type h)
                    'todo))
              nil 'first-match)))))                     ; (3)
     
     (defun qz/has-link-p (src dst)
       "undirected connection exists, from `src' to `dst'"
       (org-roam-db-query
        [:select [source dest]
                 :from links
                 :where (or (and (= dest $s1) (= source $s2))
                            (and (= dest $s2) (= source $s1)))]
        src dst))
     
     (defun qz/node-has-link-p (src dst)
       (qz/has-link-p (org-roam-node-id src)
                      (org-roam-node-id dst)))
     (setq qz/transitive-query "
     with recursive cte (id, degree) as (
     
       select n.id, 0 as degree
       from nodes n
       where n.id = $s1
     
       union all
     
       select distinct
         source as id, c.degree + 1 as degree
       from links l
       join cte c on l.dest = c.id
       where degree <= 1 -- therefore, yield 1st degree transitivity
     
     )
     select distinct id, degree
     from cte
     --where cte.id = $s2
     --where degree = 2
     order by degree desc
     ")
     
     (defun qz/transitive-links (dst)
       (org-roam-db-query qz/transitive-query dst))
     
     
     
     (defun qz/has-transitive-link-p (dst &optional src)
       (if-let* ((nap (or src (org-roam-node-at-point)))
                 (src (or src (org-roam-node-id nap))))
           (seq-contains
            (seq-map 'car
                     (org-roam-db-query qz/transitive-query dst src))
            src)
         ))
     
     ;;(seq-contains '(1 2 4) 1)
     
     (defun qz/node-has-transitive-link-p (dst &optional src)
       (qz/has-transitive-link (org-roam-node-id dst) src))
     
     ;;x(qz/has-transitive-link-p (qz/title->roam-id "emacs"))
     (defun qz/org-roam-migrate-jobs ()
       (interactive )
       (dolist (file (org-roam--list-all-files))
         (with-current-buffer (or (find-buffer-visiting file)
                                  (find-file-noselect file))
           (message "%s visiting" file)
           (qz/dispatch-hook)
           (save-buffer))))
     
                                             ;(qz/org-roam-migrate-jobs)
     
     (defun qz/file-created-as-timestamp (&optional file)
       (when-let* ((file (or file (buffer-file-name (current-buffer))))
                   (ffile (f-base file))
                   (p (string-match "-" ffile))
                   (s (substring ffile 0 p)))
         (cond
          ((= p 16) (let* ((td (timezone-parse-date s))
                           (tt (timezone-parse-time (elt td 3))))
                      (format "[%s %s]"
                              (s-join "-" (cl-subseq td nil 3))
                              (s-join ":" tt))))
          ((= p 14) (let* ((yy (substring s 0 4))
                           (mm (substring s 4 6))
                           (dd (substring s 6 8))
                           (hh (substring s 8 10))
                           (MM (substring s 10 12))
                           (ss (substring s 12 14)))
                      (format "[%s-%s-%s %s:%s:%s]"
                              yy mm dd
                              hh MM ss))))))
     
     (defun qz/org-roam-node-updated-precedence (pt file)
       (list (org-roam-get-keyword "UPDATED")
             (let ((v (s-join " " (org-entry-get-multivalued-property pt "UPDATED"))))
               (when (not (string-empty-p v)) v))
             (format-time-string
              "[%Y-%m-%d %H:%M:%S]"
              (file-attribute-modification-time (file-attributes file)))))
     
     (defun qz/org-roam-node-created-precedence (pt file)
       (list (org-roam-get-keyword "CREATED")
             (let ((v (s-join " " (org-entry-get-multivalued-property pt "CREATED"))))
               (when (not (string-empty-p v)) v))
             (qz/file-created-as-timestamp file)))
     
     (defun qz/org-roam-node-date-precedence (node date-fn)
       (save-excursion
         (let* ((pt (org-roam-node-point node))
                (file (org-roam-node-file node)))
           (with-current-buffer (or (find-buffer-visiting file)
                                    (find-file-noselect file))
             (goto-char pt)
             ;;(message "qz: getting node updated: %s" (org-roam-node-title node))
             (org-with-wide-buffer
              (car (remove nil (funcall date-fn pt file))))))))
     
     (defun qz/org-roam-node-updated-date (node)
       (qz/org-roam-node-date-precedence
        node
        (lambda (pt file)
          (append (qz/org-roam-node-updated-precedence pt file)
                  (qz/org-roam-node-created-precedence pt file)))))
     
     (defun qz/org-roam-node-created-date (node)
       (qz/org-roam-node-date-precedence
        node
        '(lambda (pt file)
           (append (qz/org-roam-node-created-precedence pt file)
                   (reverse (qz/org-roam-node-updated-precedence pt file))))))
     
     ;;(cl-subseq [1 2 3] 1 2)
     ;;(car [1 2 3])
     ;;(or (s-join "a" nil) "b")
     
     ;; (let ((node (org-roam-node-from-title-or-alias "wine")))
     ;;   (list (qz/org-roam-node-updated-date node)
     ;;         (qz/org-roam-node-created-date node)))
     
     ;;(funcall (lambda (a b c) (message "%s %s %s" a b c)) 1 2 3)
     
     (defun qz/do-migration--created-updated ()
       (let ((org-mode-hook nil))
         (cl-loop for node in (seq-map 'car (org-roam-db-query [:select id :from nodes]))
                  ;;(org-roam-node-list) groups erroneously
                  collect
                  (let ((node (org-roam-populate (org-roam-node-create :id node))))
                    (format "%s,%s,%s,%s,%s"
                            ;;"%s:%s,%s,%s,%s" to show with point
                            (org-roam-node-id node)
                            ;;(org-roam-node-point node)
                            (qz/org-roam-node-updated-date node)
                            (qz/org-roam-node-created-date node)
                            (org-roam-node-title node))
                    (kill-buffer (get-file-buffer (org-roam-node-file node)))))))
     (cl-defun qz/org-web-tools-read-url-as-org (url &key (show-buffer-fn 'switch-to-buffer))
       "Read URL's readable content in an Org buffer.
     Buffer is displayed using SHOW-BUFFER-FN."
       (interactive (list (org-web-tools--get-first-url)))
       (let ((entry (org-web-tools--url-as-readable-org url)))
         (when entry
           (funcall show-buffer-fn url)
           (org-mode)
           (insert entry)
           ;; Set buffer title
           (goto-char (point-min))
           (org-next-link)
           (rename-buffer (format "*org-web [%s]*"
                                  (cdr (qz/org-web-tools--read-org-bracket-link)))))))
     
     (defun qz/read-org-bracket-link (&optional link)
       "Return (TARGET . DESCRIPTION) for Org bracket LINK or next link on current line."
       ;; Searching to the end of the line seems the simplest way
       (save-excursion
         (when (org-in-regexp org-link-any-re)
           (let* ((full (match-string-no-properties 0))
                  (target (or (match-string-no-properties 2)
                              (match-string-no-properties 0)))
                  (description (match-string-no-properties 3)))
             (cons target description)))))
     
     ;;(ignore-errors (call-interactively 'qz/org-web-tools-read-url-as-org))
     ;;(concat org-directory "data/web/")
     (defun qz/org-download-image-at-point ()
       (interactive)
       (when-let ((l (qz/read-org-bracket-link)))
         (org-download-image l)))
     ;; NOWEB ORG START
     (message "pre org: %s" (shell-command-to-string "date"))
     (with-eval-after-load 'org
       (require 's)
       ;;; FIXME :: org-roam init kills id stuff ?
       ;;; BEGIN ORGID HACKS
       ;;(org-id-update-id-locations)
       ;; (setq org-id-locations "/home/samuel/.config/org-id-locations")
       ;;   after setting in-session, (org-roam-update-org-id-locations)
       ;;; END ORGID HACKS
       (message "loading org: %s" (shell-command-to-string "date"))
       (define-key org-mode-map (kbd "C-c C-j") 'consult-org-heading)
       (defvar qz/org-babel-indent-exclude-lang
         '("yaml")
         "org-babel languages to exclude from auto indent/format.")
       
       ;;(setq qz/org-babel-indent-exclude-lang nil)
       ;;(setq qz/debug t)
       
       
       ;; TODO bug with 'org-open-at-mouse :: { M-x toggle-debug-on-error } :: [2023-07-04 Tue]
       (defun qz/org-babel-indent-block-hook (beg end &rest args)
         (interactive "r")
         (qz/debug- (message "qz/org-babel-indent-block: BEG %s END %s ARGS %s" beg end args))
         (qz/dwim-indent))
       
       (define-key org-mode-map
                   (kbd "C-c C-v C-\\") 'qz/dwim-indent)
       (define-key global-map ;; this one is burned in hard
                   (kbd "C-M-|") 'qz/dwim-indent)
       
       ;; NOTE: blocks default
       ;;(add-to-list 'org-ctrl-c-ctrl-c-hook 'qz/org-babel-indent-block)
       ;;(setq org-ctrl-c-ctrl-c-hook nil)
       ;;
       ;; NOTE: not the right eval/exec fn for `{C-c C-c}'
       ;;(advice-add 'org-babel-eval :before 'qz/org-babel-indent-block)
       ;;(advice-remove 'org-babel-eval 'qz/org-babel-indent-block)
       ;;
       ;; conclusion: use `advice' so as not to block standard org-mode
       ;; `{C-c C-c}' behaviour like with `org-ctrl-c-ctrl-c-hook'
       
       (qz/advice- org-babel-execute-src-block :before qz/org-babel-indent-block-hook)
       (defun qz/org-refresh-inline-images (&rest args)
         (org-toggle-inline-images t)
         (org-toggle-inline-images t))
       
       (qz/advice- org-babel-execute-src-block :after qz/org-refresh-inline-images)
       ;; current subtree
       ;; given some tree
       
       "
       a
       aa
        aa
         ab
       ab
       ac
       aca
        cb
         ba
        cd
       ad
       "
       (define-key org-mode-map (kbd "C-c C-M-i") 'qz/org-fold)
       (defun qz/org-fold ()
         "go to default opening mode -- see `org-startup-folded'
       
       when prefix arg single,    fold to current level.
       when prefix arg numerical, fold to given level.
       when prefix arg nil,       fold to all levels."
         (interactive)
         (let ((prefix-arg
                (if current-prefix-arg
                    (cond
                     ((equal '(4) current-prefix-arg)
                      ;; with C-u prefix: reveal all
                      9999)
                     ((eq 0 current-prefix-arg)
                      ;; with 0 prefix; reveal sibling + 1
                      (+ 1 (org-outline-level)))
                     (t current-prefix-arg))
                  ;; by default:  reveal sibling-level
                  (org-outline-level))))
           (qz/debug- ;; HACK I don't understand prefix / current-prefix
            ;; ... is it next/ now respectively?
            (message "prefix: %s" (list current-prefix-arg prefix-arg)))
       
           ;;; DO THE FOLD
           (funcall-interactively 'org-content prefix-arg)
       
           ;;; HACK ENSURE NICE DISPLAY IF NICE DISPLAY
           ;; org-modern-mode doesn't handle "demoting" and manual link
           ;; editing, etc... all too well
           (when (and (boundp 'org-modern-mode) org-modern-mode)
             (ignore-errors
               (org-modern-mode 1)))
       
           ;;; HACK Indent, if active
           (when (and (boundp 'org-indent-mode) org-indent-mode)
             (org-indent-indent-buffer))
           ;; FOR SOME JAZZ, PRINT THE OUTLINE
           (org-display-outline-path 'with-self t)))
       (defvar qz/org-refile-last-prior-location nil)
       
       (defun qz/org-refile-to-point--undo ()
         (interactive)
         (when qz/org-refile-last-prior-location
           (org-id-goto (plist-get qz/org-refile-last-prior-location :id))
           (org-refile
            nil nil
            (list nil
                  (plist-get qz/org-refile-last-prior-location :file)
                  nil
                  (plist-get qz/org-refile-last-prior-location :pos)))))
       
       (defun qz/org-refile-to-point (file pos)
         (interactive)
         ;;; FROM {M-x describe-function RET org-refile RET}
         ;;
         ;; RFLOC can be a refile location obtained in a different way.  It
         ;; should be a list with the following 4 elements:
         ;;
         ;; 1. Name - an identifier for the refile location, typically the
         ;; headline text
         ;; 2. File - the file the refile location is in
         ;; 3. nil - used for generating refile location candidates, not
         ;; needed when passing RFLOC
         ;; 4. Position - the position in the specified file of the
         ;; headline to refile under
         (let ((rfloc (list (org-id-get)
                            (buffer-file-name (current-buffer))
                            nil
                            (point))))
           (save-mark-and-excursion
             (with-current-buffer (find-file-noselect file)
               (goto-char pos)
               (let ((id (org-id-get))
                     (heading (org-link-display-format (org-get-heading))))
                 (message "qz: refiling last capture: id :: %s" id)
                 (message "qz: refiling last capture: heading :: %s" heading)
                 (setq qz/org-refile-last-prior-location
                       (list :id id :heading heading :file file :pos pos)))
               (org-refile nil nil rfloc nil)))))
       
       (defun qz/org-refile-last-capture-to-point ()
         (interactive)
         (qz/org-refile-to-point
          (bookmark-get-filename "org-capture-last-stored")
          (bookmark-get-position "org-capture-last-stored")))
       (defun qz/org-search-from-heading ()
         (interactive)
         (let* ((cleaned-heading (->> (org-get-heading)
                                      (replace-regexp-in-string "\\[.*\\] " "")))
                (searchified (->> cleaned-heading
                                  url-encode-url
                                  (concat "https://html.duckduckgo.com/html/?q="))))
           (call-interactively 'org-insert-subheading-respect-content)
           (insert (org-make-link-string
                    searchified
                    (concat "!ddg :: " cleaned-heading)))
           (ignore-errors
             (org-web-tools-read-url-as-org (org-web-tools--read-url)))
           ;;; FIXME ddg sometimes has a super long guy which doesn't properly fontify
           ;; (save-excursion
           ;;   (org-next-visible-heading 3)
           ;;   (org-beginning-of-line)
           ;;   (let ((kill-ring nil))
           ;;     (org-kill-line)))
           ;;; goto first result
           ;;; get summary last (link description) as 4th-level
           (goto-char (point-min))
           (org-next-visible-heading 1)
           (save-excursion
             (org-map-entries
              (lambda ()
                (unless (> (org-outline-level) 4)
                  (qz/debug-
                   (message "%s" (org-link-display-format
                                  (org-no-properties (org-get-heading)))))
                  (org-narrow-to-subtree)
                  (end-of-buffer)
                  (let ((kill-ring nil))
                    (call-interactively 'org-previous-link)
                    (embark-org-copy-link-description)
                    (end-of-buffer)
                    (call-interactively
                     'org-insert-subheading-respect-content)
                    (insert (concat (car kill-ring))))
                  (widen)))))
           (goto-char (point-min))
           (let ((current-prefix-arg 4))
             (qz/org-fold))
           (org-next-visible-heading 2)
           ;;; NOTE ddg results
           ))
       
       (define-key org-mode-map (kbd "C-s-0") 'qz/org-search-from-heading)
       
       (setq qz/org-babel-default-header-args:sql:postgres
             '((:engine . "postgres")
               (:dbport . 5432) ;; NEVER connect to an external on 5433 by default
               (:dbhost . "localhost"))
             qz/bq/bi "`p-t-business-intelligence-0da1`"
             qz/org-babel-default-header-args:sql:bq
             `((:engine . "bq")
               (:results . "raw"))
             qz/sql:bq:etc
             `((:var bi . ,qz/bq/bi)
               (:var datep . "> timestamp_add(current_date(), interval -$period day)")
               (:var timep . "> timestamp_add(current_timestamp(), interval -$period day)")
               ;; this var 'period' must go "at the end", such that it may be
               ;; referenced by vars "at the start".  'timep' & 'datep' here
               ;; would fail otherwise.
               (:var period . 7)
               (:var return . ,(concat qz/bq/bi ".t_checkout.vw_item_return"))
               (:var soi    . ,(concat qz/bq/bi ".t_order_management.vw_dim_sales_order_item_all_time"))
               (:var so     . "`p-t-business-intelligence-0da1`.t_order_management.vw_order")
               (:var vsoi   . "`p-t-order-management-svc-6996`.validation_stream.om_public_sales_order_item")
               (:var rsoi   . "`p-t-order-management-svc-6996`.raw_stream.om_public_sales_order_item")
               (:var dmsoi  . "`p-t-order-management-svc-6996`.dm.dimension_sales_order_item")))
       (defun qz/choose-org-babel-default-header-args:sql (&optional dialect)
         (interactive)
         (setq org-babel-default-header-args:sql
               (symbol-value (intern
                              (format "qz/org-babel-default-header-args:sql:%s"
                                      (or dialect
                                          (completing-read "dialect: "
                                                           '(bq postgres))))))))
       
       (qz/choose-org-babel-default-header-args:sql 'bq)
       (defun org-babel-execute:bq (orig-fun body params)
         (let* ((dry? (assq :dry params))
                (cmd (format "bq query --format=json --nouse_legacy_sql %s '
       %s
       '"
                             (if dry? "--dry_run" "")
                             (s-replace-all '(
                                              ("'" . "\"")
                                              ;;("*" . "\*") ;; TODO \* (rde)
                                              ;; ("`" . "\\`")
                                              ;;   ("\"" . "\\\"")
                                              )
                                            (org-babel-expand-body:sql body params)))))
           (if (string-equal-ignore-case (cdr (assq :engine params)) "bq")
               (let ((res (org-babel-execute:shell
                           ;; ....the quoting ..... i know......
                           cmd
                           params)))
                 (ignore-errors
                   (message "Estimated Bytes: %.2f MB"
                            (let ((b (string-to-number (or (cdr (assoc 'totalBytesProcessed
                                                                       (cdr (assoc 'statistics
                                                                                   (json-read-from-string res)))))
                                                           0))))
                              ;; get MiB
                              (/ (/ b 1024.0) 1024.0))))
                 (json-to-org-table-parse-json-string res))
             (org-babel-execute:sql body params))))
       
       (qz/advice- org-babel-execute:sql :around org-babel-execute:bq)
       (setq org-babel-python-command "python3")
       (setq org-babel-default-header-args:jq
             '((:results . "output")
               (:compact . "no")
               ;;(:wrap . "src json")
               ))
       (setq org-babel-default-header-args:shell '((:results . "drawer"))
             org-babel-default-header-args:sh org-babel-default-header-args:shell
             org-babel-default-header-args:bash org-babel-default-header-args:shell
             org-babel-default-header-args:zsh org-babel-default-header-args:shell)
       ;; NOWEB AGENDA START
       
       (with-eval-after-load 'org-agenda
         (message "AGENDA start")
         (defun qz/agenda-files-update (&optional &rest force?)
           "Update the value of `org-agenda-files' with relevant candidates"
           (interactive)
           (unless (and (boundp 'qz/agenda-files-updated)
                        (or force? (not qz/agenda-files-updated)))
             (setq org-agenda-files (qz/files-agenda)
                   qz/agenda-daily-files (qz/agenda-daily-files-f)
                   qz/agenda-files-updated t)))
         (defun qz/agenda-files-update-clock (&rest _)
           "An optimisation for org-clock, which is SO SLOW.
          Returns a LIST of files that contain CLOCK, which reduces
         processing a lot"
           (interactive)
           (setq org-agenda-files (qz/clock-files)))
         (list
          ;; optimisation setup: setup subset of clock files
          (qz/advice- org-clock-resolve :before qz/agenda-files-update-clock)
          ;; optimisation teardown: restore full set of agenda-files
          (qz/advice- org-clock-resolve :after qz/agenda-files-update))
         (setq qz/daily-title-regexp ".?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.?")
         
         (defun qz/agenda-daily-files-f (&optional k)
           (let ((k 60))
             (seq-take (sort (seq-filter (lambda (s) (string-match qz/daily-title-regexp s))
                                         org-agenda-files)
                             'string-greaterp)
                       k)))
         (defun qz/clock-files ()
           (split-string
            (shell-command-to-string
             (format "
         rg CLOCK ~/life/roam/ -c -t org \
           | grep -v 'org#' \
           | grep -v '%s' \
           | awk -F '[,:]' '{print $1}'
         " (format-time-string "%Y")))))
         (defun qz/files-agenda ()
           (seq-uniq (append qz/org-agenda-files (qz/project-files))))
         (defun qz/project-files ()
           "Return a list of note files containing Project tag."
           (seq-map
            'car
            (org-roam-db-query
             '(:select :distinct file
                       :from tags
                       :inner :join nodes
                       :on (= tags:node_id nodes:id)
                       :where (= tags:tag "project")))))
         (defun qz/org-roam-private-files ()
           "Return a list of note files containing tag =private="
           (seq-map
            'car
            (org-roam-db-query
             [:select :distinct file
                      :from tags
                      :inner :join nodes
                      :on (= tags:node_id nodes:id)
                      :where (= tags:tag "private")])))
         ;; current (default) sorting strat
         '((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))
         
         
         (defun qz/agenda-todo-dailies ()
           "the most necessary simple invention in months.
         (as of [2022-01-19 Wed])
         
         get a list of `TODO' entries, from daily files, ordered by date (from filename/category) DESCENDING.
         
         - see `qz/agenda-daily-files-f' for the subset view of `org-agenda-files'
         - see `org-agenda-sorting-strategy' for sort permutations."
           (interactive)
           (qz/agenda-files-update)
           (let* ((org-agenda-files (qz/agenda-daily-files-f))
                  (org-agenda-sorting-strategy '(timestamp-down category-down)))
             (org-todo-list)))
         
         (define-key global-map (kbd "C-c n t") 'qz/agenda-todo-dailies)
         (defun qz/org-agenda-gtd ()
           (interactive)
           (org-agenda nil "g")
           (goto-char (point-min))
           (org-agenda-goto-today))
         
         ;; HACK safe agenda ops on load
         (when (not (boundp 'org-agenda-custom-commands))
           (setq org-agenda-custom-commands nil))
         
         (when (not (boundp 'org-agenda-custom-commands))
           (setq org-roam-directory "~/life/roam"))
         ;; TODO why require this? because of org-roam-directory?
         ;(require 'org-roam)
         
         ;; (message "agenda: setting custom commands\n%s" org-agenda-custom-commands)
         
         (add-to-list
          'org-agenda-custom-commands
          `("g" "GTD"
            ((agenda "" ((org-agenda-span 'day) (org-deadline-warning-days 60)))
             (tags-todo "now"
                        ((org-agenda-overriding-header "\nnow\n")))
             (tags-todo "wip"
                        ((org-agenda-overriding-header "\nwip\n")))
             (todo "TODO"
                   ((org-agenda-overriding-header "\nto process\n")
                    (org-agenda-files '(,(format "%s/%s" org-roam-directory "inbox.org")))))
             (todo "TODO"
                   ((org-agenda-overriding-header "\ndaily inbox\n")
                    (org-agenda-files qz/agenda-daily-files)))
             (todo "TODO"
                   ((org-agenda-overriding-header "\nemails\n")
                    (org-agenda-files '(,(format "%s/%s" org-roam-directory "emails.org")))))
             (todo "TODO"
                   ((org-agenda-overriding-header "\none-off Tasks\n")
                    (org-agenda-files '(,(format "%s/%s" org-roam-directory "next.org")))))
             (todo "TODO"
                   ((org-agenda-overriding-header "\nto yak shave\n")
                    (org-agenda-files '(,(format "%s/%s" org-roam-directory "emacs.org"))))))))
         (add-to-list
          'org-agenda-custom-commands
          `("c" "create"
            ((agenda "" ((org-agenda-span 'day) (org-deadline-warning-days 60)))
             (tags-todo "diy+create+do+buy+make+wip"
                        ((org-agenda-overriding-header "wip")))
             (tags-todo "diy+create+do"
                        ((org-agenda-overriding-header "create")))
             (tags-todo "buy"
                        ((org-agenda-overriding-header "buy")))
             (tags-todo "make"
                        ((org-agenda-overriding-header "make"))))))
         (add-to-list
          'org-agenda-custom-commands
          `("w" "work"
            ((tags-todo "{work}+wip"
                        ((org-agenda-overriding-header "wip")
                         (org-tags-match-list-sublevels nil) ;; show subheadings!!!! inherited!!!!
                         ;; (org-agenda-hide-tags-regexp
                         ;;  (concat org-agenda-hide-tags-regexp "\\|work"))
                         ))
             (tags-todo "{work}"
                        ((org-agenda-overriding-header "work")))
             )))
         
         ;;(pp org-agenda-custom-commands)
         (add-to-list
          'org-agenda-custom-commands
          '("1" "Events" agenda "display deadlines and exclude scheduled"
            ((org-agenda-span 'year)
             (org-agenda-time-grid nil)
             (org-agenda-show-all-dates nil)
             (org-agenda-entry-types '(:deadline)) ;; this entry excludes :scheduled
             (org-deadline-warning-days 0) )))
         
         
         (defun zin/org-agenda-skip-tag (tag &optional others)
           "Skip all entries that correspond to TAG.
         
         If OTHERS is true, skip all entries that do not correspond to TAG."
           (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
                 (current-headline (or (and (org-at-heading-p)
                                            (point))
                                       (save-excursion (org-back-to-heading)))))
             (if others
                 (if (not (member tag (org-get-tags-at current-headline)))
                     next-headline
                   nil)
               (if (member tag (org-get-tags-at current-headline))
                   next-headline
                 nil))))
         
         
         (add-to-list
          'org-agenda-custom-commands
          '("0" "moving"
            ;; TODO: Add A priority to the top.
            ((agenda
              ""
              ((org-agenda-time-grid nil)
               (org-agenda-start-on-weekday nil)
               (org-agenda-start-day "+1d")
               (org-agenda-span 160)
               (org-agenda-show-all-dates nil)
               (org-agenda-time-grid nil)
               (org-agenda-show-future-repeats nil)
               (org-agenda-block-separator nil)
               (org-agenda-entry-types '(:deadline))
               (org-agenda-skip-function
                '(not
                  (zin/org-agenda-skip-tag "moving" 't)
                  ;; '(org-agenda-skip-entry-if 'notregexp":moving:") ; no love for inherited tags
                  ))
               (org-agenda-overriding-header "\nUpcoming deadlines (+160d)\n")))
             (agenda
              "*"
              ((org-agenda-block-separator nil)
               (org-agenda-span 160)
               (org-agenda-show-future-repeats nil)
               (org-agenda-skip-deadline-prewarning-if-scheduled t)
               (org-agenda-skip-function
                '(org-agenda-skip-entry-if 'notregexp":moving:"))
               (org-agenda-overriding-header "\nAgenda\n")))
             (tags-todo "moving"
                        ((org-agenda-block-separator nil)
                         (org-agenda-skip-function '(or (org-agenda-skip-if nil '(scheduled))))
                         (org-agenda-overriding-header "\nMoving Backlog\n"))))))
         (defvar qz/agenda-daily-files nil)
         (setq org-agenda-hide-tags-regexp "project")
         (defun qz/org-category (&optional len)
           (let* ((len (or len 25)))
             (->>
              (if buffer-file-name
                  (file-name-sans-extension (file-name-nondirectory buffer-file-name))
                "")
              (replace-regexp-in-string "private-" "")
              (replace-regexp-in-string
               ;; datetime from file, could do "[0-9]\\{6\\}T[0-9]\\{6\\}Z?-"
               (concat "[0-9][0-9][0-9][0-9]" "[0-9][0-9]" "[0-9][0-9]"
                       "T" "[0-9][0-9]" "[0-9][0-9]" "[0-9][0-9]" "Z-")
               "")
              (s-pad-right len " ")
              (s-truncate len))))
         
         ;;(qz/org-category)
         (let* ((agenda "  %(qz/org-category)%-12t% s")
                (other "%i%(qz/org-category 12)%l"))
           (setq org-agenda-prefix-format (list (cons 'agenda agenda)
                                                (cons 'todo other)
                                                (cons 'todo other)
                                                (cons 'todo other)
                                                (cons 'search other))))
         
         (defun vulpea-agenda-category (&optional len)
           "Get category of item at point for agenda.
         
         Category is defined by one of the following items:
         - CATEGORY property
         - TITLE keyword
         - TITLE property
         - filename without directory and extension
         
         When LEN is a number, resulting string is padded right with
         spaces and then truncated with ... on the right if result is
         longer than LEN.
         
         Usage example:
         
           (setq org-agenda-prefix-format
                 '((agenda . \" Emacs Configuration %?-12t %12s\")))
         
         Refer to `org-agenda-prefix-format' for more information."
           (let* ((file-name (when buffer-file-name
                               (file-name-sans-extension
                                (file-name-nondirectory buffer-file-name))))
                  (title (qz/node-title))
                  (category (org-get-category))
                  (result
                   (or (if (and title
                                (string-equal category file-name))
                           title
                         category)
                       "")))
             (if (numberp len)
                 (s-truncate len (s-pad-right len " " result))
               result)))
         (org-no-warnings (defvar date))
         (defun qz/org-lunar-phases ()
           "Show lunar phase in Agenda buffer."
           (require 'lunar)
           (let* ((phase-list (lunar-phase-list (nth 0 date)
                                                (nth 2 date)))
                  (phase (cl-find-if (lambda (phase)
                                       (equal (car phase) date))
                                     phase-list)))
             (when phase
               (setq ret (concat (lunar-phase-name (nth 2 phase)))))))
         ;; 🌑🌒🌓🌔🌕🌖🌗🌘🌙🌚🌛🌜
         (setq lunar-phase-names
               '("🌚 new moon" ; unicode symbol : 🌑 use full circle as fallback
                 "🌛 first quarter moon"
                 "🌝 full moon" ; unicode symbol: 🌕 use empty circle as fallback
                 "🌜 last quarter moon"))
         (setq calendar-latitude 52.5)  ; imprecise
         (setq calendar-longitude 13.4)
         (setq calendar-location-name "berlin")
         
         (autoload 'solar-sunrise-sunset "solar.el")
         (autoload 'solar-time-string "solar.el")
         (defun qz/diary-sunrise ()
           "Local time of sunrise as a diary entry.
         The diary entry can contain `%s' which will be replaced with
         `calendar-location-name'."
           (let ((l (solar-sunrise-sunset date)))
             (when (car l)
               (concat
                (if (string= entry "")
                    "🌄 sunrise"
                  (format entry (eval calendar-location-name))) " "
                (solar-time-string (caar l) nil)))))
         
         (defun qz/diary-sunset ()
           "Local time of sunset as a diary entry.
         The diary entry can contain `%s' which will be replaced with
         `calendar-location-name'."
           (let ((l (solar-sunrise-sunset date)))
             (when (cadr l)
               (concat
                (if (string= entry "")
                    "🌅 sunset"
                  (format entry (eval calendar-location-name))) " "
                (solar-time-string (caadr l) nil)))))
         )
       
       ;; NOWEB AGENDA END
       
       (setq org-columns-default-format "%22UPDATED %100ITEM %TODO %3PRIORITY %TAGS")
       (defun qz/unorg-columns ()
         (interactive)
         (org-columns-remove-overlays))
       ;;;;
       (defun qz/org-ql (select from where order limit transform)
         (let ((limit (or limit 100)))
           (mapcar
            transform
            (seq-take
             (org-ql-query
               :select select
               ;;:from org-agenda-files
               ;;:from (seq-take (cl-sort (org-roam-dailies--list-files) 'string-greaterp) 1)
               :from from
               ;;:where '(clocked)
               ;;:where '(todo)
               ;; NOTE arbitrary function doesn't work
               ;; :where '((lambda (&rest e)
               ;;            (message "where: %s" e)
               ;;            t))
               :order-by order)
             limit))))
       
       (defun qz/org-ql-:select-default ()
         (lambda ()
           (list :title   (cadar (org-collect-keywords '("TITLE")))
                 :outline (org-get-outline-path)
                 :heading (org-get-heading)
                 :id      (org-id-get)
                 :tags    (s-join ":" (org-get-tags))
                 :node    (org-roam-node-at-point)
                 :updated (or (org-entry-get (point) "UPDATED") "[0000-00-00]")
                 :created (or (org-entry-get (point) "CREATED") "[0000-00-00]"))))
       
       (defun qz/org-ql-:transform-default (cols)
         (lambda (e)
           (let ((trunc 75))
             (append
              (mapcar (lambda (col) (plist-get e col)) cols)
              (list
               ;; heading
               (org-make-link-string
                (concat "id:" (plist-get e :id))
                (s-truncate trunc
                            (org-no-properties (org-link-display-format
                                                (plist-get e :heading)))))
               ;; node
               (let ((n (plist-get e :node)))
                 (org-make-link-string
                  (org-roam-node-id n)
                  (s-truncate trunc (org-roam-node-title n)))))))))
       
       (defun qz/org-ql-:order-time (col)
         (let ((col (or col :updated)))
           `(lambda (a b)
              (qz/debug- (message "order pred %s" ,col))
              (when-let* ((aa (plist-get a ,col))
                          (bb (plist-get b ,col)))
                (not (time-less-p (date-to-time aa)
                                  (date-to-time bb)))))))
       
       ;; (funcall (qz/org-ql-:order-time :created)
       ;;          '(:updated "2023-01-01")
       ;;          '(:updated "2023-01-02"))
       
       (defun qz/org-ql--time (time-col &optional limit cols)
         (let ((cols (or cols
                         (list time-col :title ))))
           (qz/org-ql
            ;; select
            (qz/org-ql-:select-default)
            ;;; from
            (qz/month-files)
            ;; (length (org-roam-list-files)) ; 3497... this is just too much
            ;; where
            nil
            ;; order
            (qz/org-ql-:order-time time-col)
            ;; limit
            limit
            ;; transform
            (qz/org-ql-:transform-default cols))))
       
       
       (defun qz/org-ql--updated ()
         (qz/org-ql--time :updated))
       
       (defun qz/org-ql--created ()
         (qz/org-ql--time :created))
       
       ;;(car (qz/org-ql--updated))
       ;;(qz/org-ql--created)
       
       (defun qz/org-now ()
         "
       default ::
       - feed
         - stored
           - [headline]
         - clocking
           - [headline]
       
       prefix C-u ::
       [default]
       -
       
       "
         (interactive)
         (let* ((b (get-buffer-create "*org-now*"))
                (n 5))
           (save-excursion
             (with-current-buffer b
               (setq-local default-directory org-directory)
               (read-only-mode -1)
               (org-mode)
               (olivetti-mode -1)
       
               (mark-whole-buffer)
               (delete-region (region-beginning)
                              (region-end))
       
               (cl-labels ((line (&rest s)
                             (insert (format "%s\n" (or (and s (s-join " " s)) "")))))
                 ;;(line "* org-now")
                 (eval-after-load 'svg-clock
                   (svg-clock-insert))
                 (line)
       
                 (insert "* feed\n")
       
       
                 ;; (insert "** looking at\n")
                 ;; (line)
                 ;; (mapcar (lambda (s)
                 ;;           (ignore-errors
                 ;;             (line "-" (plist-get (car s) :timestamp) "::"
                 ;;                   (org-link-make-string
                 ;;                    (car (cdr s))
                 ;;                    ;; FIXME ugly ass conditions
                 ;;                    (or (cadr (cdr s))
                 ;;                        (car (cdr s)))))))
                 ;;         qz/looking-at)
                 ;; (line)
       
       
                 (insert "** stored")
                 (line)
                 (mapcar (lambda (s) (line "- " (apply 'org-link-make-string s)))
                         org-stored-links)
                 (line)
       
                 (insert "** clocking")
                 (line)
       
                 ;; XXX copied from above
                 ;; (headline "" (mapcar (line (links (link-fn)))))
                 (mapcar (lambda (s)
                           (and s
                              (line "- " (apply 'org-link-make-string s))))
                         ;; FYI to get max value from this
                         ;; (setq org-clock-history-length 1000000) ;; 1 million
                         ;; (setq org-clock-persist t)
                         ;; (find-file org-clock-persist-file)
                         ;; (org-clock-load) ;; to historic clocks
                         (save-excursion
                           (mapcar (lambda (h)
                                     (when-let* ((b (and h (marker-buffer h))))
                                       (with-current-buffer b
                                         (goto-char h)
                                         (list (concat "id:" (org-id-get-create))
                                               (org-get-heading)))))
                                   org-clock-history)))
                 (line)
       
                 ;; TODO new section? actions/refile actions/captured
                 (message "org-now: prefix %s" current-prefix-arg)
                 (setq fmt (lambda (r)
                             (insert (format "| %s |\n"
                                             (s-join " | " r))))
                  pref (lambda ()
                              (insert "** heading\n")
                              (insert "*** updated\n")
                              ;; TODO lexical closure issue (because of when?)
       
                              (funcall fmt '("date" "title" "heading" "node"))
                              (insert "|----------\n")
                              (mapcar fmt (seq-take (qz/org-ql--updated) n))
                              (org-table-align)
                              (insert "\n")
       
                              (insert "*** created\n")
                              (funcall fmt '("date" "title" "heading" "node"))
                              (insert "|----------\n")
                              (mapcar fmt (seq-take (qz/org-ql--created) n))
                              (org-table-align)
                              (insert "\n")
       
                              (insert "** node\n")
                              (insert "*** updated\n")
                              (funcall fmt '("date" "node-title"))
                              (insert "|----------\n")
                              (mapcar fmt (org-babel-execute:sqlite
                                           (format "
       select
         (
           case when instr(properties, '(\"UPDATED\" .') > 0
             then substr(properties, instr(properties, '(\"UPDATED\" .') + 15, 20)
           else null end
         ) as created,
         title
         from nodes
        where (
          instr(properties, '(\"UPDATED\" .') > 0
        )
        order by created desc
        limit %s" n)
                                           '((:db . "~/.config/emacs/org-roam.db"))))
                              (org-table-align)
                              (insert "\n")
       
       
                              (insert "*** created\n")
                              (funcall fmt '("date" "node-title"))
                              (insert "|----------\n")
                              (mapcar fmt (org-babel-execute:sqlite
                                           (format "
       select
         (
           case when instr(properties, '(\"CREATED\" .') > 0
             then substr(properties, instr(properties, '(\"CREATED\" .') + 15, 20)
           else null end
         ) as created,
         title
         from nodes
        where (
          instr(properties, '(\"CREATED\" .') > 0
        )
        order by created desc
        limit %s" n)
                                           '((:db . "~/.config/emacs/org-roam.db"))))
                              (org-table-align)
                              (insert "\n")
                              )))
                 (when current-prefix-arg
                   (funcall pref)
                   ;;             (insert "** heading\n")
                   ;;             (insert "*** updated\n")
                   ;;             ;; TODO lexical closure issue (because of when?)
       
                   ;;             (funcall 'fmt '("date" "title" "heading" "node"))
                   ;;             (insert "|----------\n")
                   ;;             (mapcar 'fmt (seq-take (qz/org-ql--updated) n))
                   ;;             (org-table-align)
                   ;;             (insert "\n")
       
                   ;;             (insert "*** created\n")
                   ;;             (funcall 'fmt '("date" "title" "heading" "node"))
                   ;;             (insert "|----------\n")
                   ;;             (mapcar 'fmt (seq-take (qz/org-ql--created) n))
                   ;;             (org-table-align)
                   ;;             (insert "\n")
       
                   ;;             (insert "** node\n")
                   ;;             (insert "*** updated\n")
                   ;;             (funcall 'fmt '("date" "node-title"))
                   ;;             (insert "|----------\n")
                   ;;             (mapcar 'fmt (org-babel-execute:sqlite
                   ;;                          (format "
                   ;; select
                   ;;   (
                   ;;     case when instr(properties, '(\"UPDATED\" .') > 0
                   ;;       then substr(properties, instr(properties, '(\"UPDATED\" .') + 15, 20)
                   ;;     else null end
                   ;;   ) as created,
                   ;;   title
                   ;;   from nodes
                   ;;  where (
                   ;;    instr(properties, '(\"UPDATED\" .') > 0
                   ;;  )
                   ;;  order by created desc
                   ;;  limit %s" n)
                   ;;                          '((:db . "~/.config/emacs/org-roam.db"))))
                   ;;             (org-table-align)
                   ;;             (insert "\n")
       
       
                   ;;             (insert "*** created\n")
                   ;;             (funcall fmt '("date" "node-title"))
                   ;;             (insert "|----------\n")
                   ;;             (mapcar fmt (org-babel-execute:sqlite
                   ;;                          (format "
                   ;; select
                   ;;   (
                   ;;     case when instr(properties, '(\"CREATED\" .') > 0
                   ;;       then substr(properties, instr(properties, '(\"CREATED\" .') + 15, 20)
                   ;;     else null end
                   ;;   ) as created,
                   ;;   title
                   ;;   from nodes
                   ;;  where (
                   ;;    instr(properties, '(\"CREATED\" .') > 0
                   ;;  )
                   ;;  order by created desc
                   ;;  limit %s" n)
                   ;;                          '((:db . "~/.config/emacs/org-roam.db"))))
                   ;;             (org-table-align)
                   ;;             (insert "\n"))
       
                   ;; end tables
                   )
                 (read-only-mode 1)
                 ;; end display
                 ))
             (display-buffer b)))
       
         (define-key global-map (kbd "C-s-n") 'qz/org-now)
       (defun qz/choose-anno-mode ()
         (completing-read "anno: " '(full light)))
       
       (defun qz/anno-yank-region-to-point ()
         "use with prefix argument to make an index to the buffer"
         (interactive)
         (when-let* ((s (qz/peek-region))
                     (s (s-trim s))
                     (r (bookmark-make-record))
                     (L (or (ignore-errors (let ((org-stored-links '()))
                                             (call-interactively 'org-store-link)))
                            '("" . "")))
                     (l (apply 'org-link-make-string l))
                     (info (buffer-name (current-buffer)))
                     (f   (qz/anno-face nil))
                     (buf (qz/anno-buffer info)))
           (with-current-buffer buf
             ;; hl in org-anno
             (highlight-phrase s f)
             ;;; ensure org
             (when (not (eq major-mode 'org-mode))
               (org-mode))
             ;; cl-case choose-anno-mode
             (cond
              ;; new default
              ((eq 1 current-prefix-arg)
               (end-of-buffer)
               (org-insert-heading-respect-content)
               (insert l))
              ;;
              (current-prefix-arg
               (end-of-buffer)
               ;; (ignore-errors
               ;;   (end-of-line)
               ;;   (org-forward-sentence))
               (org-insert-heading-respect-content)
               (insert (format "%s :: %s\n"
                               l
                               (org-link-make-string
                                (format "elisp:(with-current-buffer (get-buffer \"%s\") (occur \"%s\"))" info s)
                                (s-truncate 30 s))))
               (insert (format "#+begin_quote %s\n%s\n#+end_quote"
                               l s)))
              (t
               (end-of-buffer)
               (insert (format "\n- \"%s\"\n" s))
               (display-buffer buf))))
           ;; hl in source buffer
           (highlight-phrase s f)
           s))
       
       (setq org-link-elisp-confirm-function 'y-or-n-p)
       (defun qz/anno-face (&optional phrase)
         ;; TODO
         'underline
         'gnus-emphasis-underline-bold-italic)
       
       (defun qz/anno-buffer (&optional source)
         "future; there may be many"
         (get-buffer-create (format "*org-anno :: [%s]*" source)))
       
       (define-key global-map (kbd "s-i") 'qz/anno-yank-region-to-point)
       
       
       
       (define-key global-map (kbd "C-<mouse-down-1>")
                   'qz/anno-yank-region-to-point)
       (define-key global-map (kbd "C-<mouse-1>")
                   'qz/anno-yank-region-to-point)
       (define-key global-map (kbd "C-s-<down-mouse-1>")
                   'qz/anno-yank-region-to-point)
       (setq org-clock-idle-time 15)
       
       (setq jiralib-url "https://goodscloud.atlassian.net")
       (defun qz/org-jira-get-issues-by-board (&optional board-id)
         "Get list of ISSUES from agile board."
         (interactive)
         (let ((board-id (or board-id (cdr (org-jira-read-board)))))
           (jiralib-get-board-issues
            board-id
            :callback org-jira-get-issue-list-callback
            :limit 999
            :query-params (org-jira--make-jql-queryparams board-id))))
       ;(require 'ob-async)
       (setq org-confirm-babel-evaluate nil)
       (setq org-structure-template-alist
             '(;; yp
               ("d"  . "definition")
               ("ee" . "example")
               ("es" . "src es")
               ("el" . "src emacs-lisp")
               ("q"  . "quote")
               ("sb" . "src shell")
               ("se" . "src emacs-lisp")
               ("sl" . "src scheme")
               ;; ("sp" . "src sql :engine postgres")
               ("sp" . "src python")
               ("sr" . "src R")
               ("ss" . "src")
               ("jp" . "src jupyter-python")
               ("jr" . "src jupyter-R")
               ("r"  . "src restclient")))
       (org-babel-do-load-languages
        'org-babel-load-languages
        '((emacs-lisp . t)
          (lisp . t)
          ;;(jupyter . t)
          (python . t)
          (jq . t)
          ;;(ipython . t)
          (scheme . t)
          (sql . t)
          ;;(plant-uml . t)
          (shell . t)
          (sqlite . t)
          ;;(elasticsearch . t)j ;; TODO rde package es/rest
          ;;(restclient . t)
          (R . t)))
       (defun qz/org-babel-choose-block (&optional lob)
         "choose block, insert scaffold for args.
       
       might honestly be better to generate `yas' template when we load
       blocks with `qz/org-babel-do-lob-ingest-files', but I've never used
       yas so idk
       
       use a prefix arg to shortcut (org-table-get-constant \"bonk\")
       "
         (interactive)
         ;; HACK I don't understand prefix / current-prefix
         ;; (message "prefix: %s" (list current-prefix-arg prefix-arg lob))
         (let ((lob (or lob
                        (intern (completing-read
                                 "lob: " (mapcar 'car org-babel-library-of-babel))))))
           (with-current-buffer (current-buffer)
             (end-of-line)
             (newline)
             (insert (format "#+name: call-%s\n#+call: %s(%s)"
                             lob lob (or (and current-prefix-arg
                                              "(org-table-get-constant \"bonk\")")
                                         "")))
       
             (when-let
                 ((args (remove
                         nil (cl-loop for a in (assoc lob org-babel-library-of-babel)
                                      append
                                      (when (listp a)
                                        (cl-loop for b in a
                                                 collect
                                                 (when (eq :var (car b)) (cdr b))))))))
               (message "%s" args)
               (insert (format "(%s)" (s-join ", " args)))))))
       
       ;;(qz/org-babel-choose-block 'newstore-get-order-by-type)
       (defun qz/lob-get-named-src-block-body (name)
         (cl-destructuring-bind
             (file . pt) (qz/lob-get-named-src-block name)
           (with-current-buffer (find-file-noselect file)
             (save-excursion
               (goto-char pt)
               (org-babel-expand-src-block)))))
       
       ;;(apply 'format "hey %s %s %s" (list 1 2 4))
       
       (defun qz/named (name &rest args)
         "shorthand wrapper of `qz/lob-get-named-src-block-body', for clearer header args"
         (apply 'format (qz/lob-get-named-src-block-body name) args))
       
       (defun qz/lob-get-named-src-block (name)
         (message "checking name: %s" name)
         (cl-block named    ; thank u cltl, thank u 1980s, thank u guy steele
           (save-excursion  ; check current-buffer
             (when (not (org-babel-goto-named-src-block name))
               (cl-return-from named (cons (buffer-file-name) (point)))))
           (mapcar (lambda (f)
                     (with-current-buffer (find-file-noselect f)
                       (save-excursion
                         ;; it's odd that nil means "i found it"
                         (when (not (org-babel-goto-named-src-block name))
                           (cl-return-from named (cons f (point)))))))
                   (remove nil qz/org-babel-lob-ingest-files))))
       
       (defun qz/lob-goto-named-src-block (name)
         (interactive
          (list
           (completing-read "lob: " (mapcar 'car org-babel-library-of-babel))))
         (cl-destructuring-bind
             (file . pt) (qz/lob-get-named-src-block name)
           (find-file file)
           (goto-char pt)))
       (defun qz/lob-restclient-copy-curl-command (&optional name)
         "this one was a struggle"
         (interactive)
         (when-let ((name (or name (thing-at-point 'symbol))))
           (cl-destructuring-bind
               (file . pt) (qz/lob-get-named-src-block name)
             (save-excursion
               (with-current-buffer (find-file-noselect file)
                 (goto-char pt)
                 (next-line)
                 (let ((expanded (org-babel-expand-src-block)))
                   (message "expanded: %s" expanded)
                   (with-temp-buffer ;;(get-buffer-create "*restclient*") ;;TODO replace w temp
                     (restclient-mode)
                     (insert expanded)
                     (goto-char (point-min))
                     (restclient-jump-next)
                     (restclient-copy-curl-command))))))))
       (define-key org-babel-map (kbd "M-l") 'qz/org-babel-choose-block)
       (define-key org-babel-map (kbd "M-l") 'qz/org-babel-choose-block)
       (define-key org-babel-map (kbd "M-g") 'qz/lob-goto-named-src-block)
       (defun qz/org-babel-make-table-constants ()
         "exec from the top of a tree"
         (interactive)
         (let* ((hi-lock-auto-select-face t)
                (write-constants (equal '(4) current-prefix-arg))
                ;; above is 100x better when you patch `hi-lock-face-symbol-at-point'
                ;; with `(or (and hi-lock-auto-select-face (hi-lock-read-face-name)) 'hi-yellow)'
                (col '()))
           (save-mark-and-excursion
             (org-map-tree
              (lambda ()
                (when-let* ((s (org-get-heading))
                            (s (org-no-properties s))
                            (i (string-match "::" s))
                            (k (substring s 0 (- i 1)))
                            (v (substring s (+ 3 i))))
                  (message "key: %s" k)
                  (message "value: %s" v)
                  (setq col (cons (format "%s=%s" k v) col))
                  (funcall-interactively 'highlight-phrase v)
                  (message "applied highlight for '%s'" v)
                  )))
             (when write-constants
               (org-back-to-heading)
               (next-line)
               (newline)
               (previous-line)
               (insert (format "#+constants: %s" (s-join " " (reverse col))))))
           (message "col: %s" col)
           col))
       
       (define-key org-babel-map (kbd "M-d") 'qz/org-babel-make-table-constants)
       (defun qz/to-shell (command)
         (interactive)
         (with-current-buffer (vterm "*to-shell*")
           (mapc
            (lambda (c)
              (message c)
              (vterm-send-string c)
              (vterm-send-return))
            (qz/ensure-list command))))
       
       (defun qz/current-src-block ()
         (interactive)
         (s-split
          "[\n]"
          (let ((src
                 (nth 6 (car ;; lspec
                         (cdr (car
                               (save-excursion
                                 (when-let ((head (org-babel-where-is-src-block-head)))
                                   (goto-char head))
                                 (org-babel-tangle-single-block 1 t))))))))
            (kill-new src)
            src)))
       
       (defun qz/shell-current-src-block ()
         (interactive)
         (when-let ((command (qz/current-src-block)))
           (qz/to-shell command)))
       
       (define-key org-babel-map (kbd "C-<return>") 'qz/shell-current-src-block)
       (defun qz/org-kill-expanded-src-block ()
         (interactive)
         (kill-new (org-babel-expand-src-block))
         (message "copied: %s" (s-truncate 25 (car kill-ring))))
       
       (define-key org-babel-map (kbd "M-w") 'qz/org-kill-expanded-src-block)
       (defun qz/org-expand-src-or-call-block ()
         (when-let* ((context (org-element-lineage
                               (org-element-context)
                               ;; limit to call, src
                               '(inline-babel-call
                                 babel-call
                                 inline-src-block
                                 src-block)
                               t))
                     (finfo
                      (cl-case (org-element-type context)
                        ((or babel-call inline-babel-call) 'org-babel-lob-get-info)
                        (t 'org-babel-get-src-block-info)))
                     (info (funcall finfo context)))
           (let ((old-info (copy-tree info)))
             (setf (nth 2 info) (org-babel-process-params (nth 2 info)))
             (list (car info) (org-babel-expand-src-block nil info)))))
       (defun qz/preview-org-expand-src-or-call-block ()
         (interactive)
         (let* ((buf (progn (and (get-buffer "*qz/Org Src*")
                                 (kill-buffer (get-buffer "*qz/Org Src*")))
                            (get-buffer-create "*qz/Org Src*")))
                (info (qz/org-expand-src-or-call-block)))
           (with-current-buffer buf
             (funcall (intern (concat (car info) "-mode")))
             (insert (cadr info)))
           (select-window (temp-buffer-window-show buf))))
       
       (define-key org-babel-map (kbd "v")
                   'qz/preview-org-expand-src-or-call-block)
       
       ;; NOTE default is:
       ;; (define-key org-babel-map (kbd "C-v") 'org-babel-expand-src-block)
       (defun qz/org-babel--list->rows (name lst)
         (cons (list name)
               (cons 'hline (mapcar 'list lst))))
       (defun org-mode-<>-syntax-fix (start end)
         "Change syntax of characters ?< and ?> to symbol within source code blocks."
         (let ((case-fold-search t))
           (when (eq major-mode 'org-mode)
             (save-excursion
               (goto-char start)
               (while (re-search-forward "<\\|>" end t)
                 (when (save-excursion
                         (and
                          (re-search-backward "[[:space:]]*#\\+\\(begin\\|end\\)_src\\_>" nil t)
                          (string-equal (downcase (match-string 1)) "begin")))
                   ;; This is a < or > in an org-src block
                   (put-text-property (point) (- (point) 1)
                                      'syntax-table (string-to-syntax "_"))))))))
       
       (defun org-setup-<>-syntax-fix ()
         "Setup for characters ?< and ?> in source code blocks.
       Add this function to `org-mode-hook'."
         (make-local-variable 'syntax-propertize-function)
         (setq syntax-propertize-function 'org-mode-<>-syntax-fix)
         (syntax-propertize (point-max)))
       
       ;;; TODO fix this slow fucker
       ;;(add-hook 'org-mode-hook 'org-setup-<>-syntax-fix)
       (with-eval-after-load 'org-roam
         ;; NOWEB ROAM START
         (message "roam start")
         (require 'org-roam-protocol)
         (setq qz/org-roam-dailies-filespec "private-%<%Y-%m-%d>.org")
       
         (defun qz/inspect-agenda-files ()
           `((org-files-list . ,(length (org-files-list)))
             ((org-agenda-files . ,(length (org-agenda-files)))
              ((qz/project-files . ,(length (qz/project-files)))
               (qz/agenda-daily-files-f . ,(length (qz/agenda-daily-files-f)))))))
         (defun qz/inspect-agenda-updates ()
           (mapcar (lambda (s) `(,s . (,(progn (funcall s)
                                               (qz/inspect-agenda-files)))))
                   '(qz/agenda-files-update qz/agenda-files-update-clock)))
         (setq qz/org-agenda-files
               (mapcar (lambda (f) (expand-file-name (format "%s/%s" org-roam-directory f)))
                       '("calendar-home.org" "calendar-work.org" "schedule.org")))
         ;; (makunbound 'qz/org-babel-lob-ingest-files)
         (defvar qz/org-babel-lob-ingest-files
           (append (mapcar (lambda (s)
                             (when-let ((n (org-roam-node-from-title-or-alias s)))
                               (org-roam-node-file n)))
                           '("NewStore"
                             "kubernetes"
                             "postgres"
                             "es-mode"
                             "elisp"
                             "plantuml"
                             "GNU Guix"
                             "git"
                             "Apache Kafka"
                             "kafka-connect"
                             "bigquery"
                             "gcloud cli"
                             ))
                   ;; .. other files
                   nil
                   ;; ..
                   )
           "files from which named `src' blocks should be loaded")
         
         (defun qz/org-babel-do-lob-ingest-files (&optional files)
           (interactive)
           (let ((r (mapcar (lambda (f) (cons (org-babel-lob-ingest f) f))
                            (append qz/org-babel-lob-ingest-files files))))
             ;;(message "%s" (pp r))
             r))
         
         (cons->table
          (qz/org-babel-do-lob-ingest-files))
         (unless (boundp 'org-agenda-directory)
           (setq org-agenda-directory nil))
         (defun org-roam-complete-everywhere ()
           "Complete symbol at point as a link completion to an Org-roam node.
         This is a `completion-at-point' function, and is active when
         `org-roam-completion-everywhere' is non-nil.
         
         Unlike `org-roam-complete-link-at-point' this will complete even
         outside of the bracket syntax for links (i.e. \"[[roam:|]]\"),
         hence \"everywhere\"."
           (when (and org-roam-completion-everywhere
                      (thing-at-point 'symbol) ;; ✂️
                      (not (org-in-src-block-p))
                      (not (save-match-data (org-in-regexp org-link-any-re))))
             (let ((bounds (bounds-of-thing-at-point 'symbol))) ;; ✂️
               (list (car bounds) (cdr bounds)
                     (org-roam--get-titles)
                     :exit-function
                     (lambda (str _status)
                       (delete-char (- (length str)))
                       (insert "[[roam:" str "]]"))
                     ;; Proceed with the next completion function if the returned titles
                     ;; do not match. This allows the default Org capfs or custom capfs
                     ;; of lower priority to run.
                     :exclusive 'no))))
         (setq org-roam-node-display-template "${title}")
         (defun qz/create-node ()
           "assumes point is at the desired headline"
           (interactive)
           (org-id-get-create)
           (org-delete-property "ROAM_EXCLUDE"))
         
         (defun qz/exclude-node ()
           "assumes point is at the desired headline -- unlikely to work for files"
           (org-set-property "ROAM_EXCLUDE" "t"))
         
         (define-key org-mode-map (kbd "C-c C-x i") 'qz/create-node)
         (defun org-roam-extract-subtree ()
           "Convert current subtree at point to a node, and extract it into a new file."
           (interactive)
           (save-excursion
             (org-back-to-heading-or-point-min t)
             (when (bobp) (user-error "Already a top-level node"))
             (org-id-get-create)
             (save-buffer)
             (org-roam-db-update-file)
             (let* ((template-info nil)
                    (node (org-roam-node-at-point))
                    (template (org-roam-format-template
                               (string-trim (org-capture-fill-template org-roam-extract-new-file-path))
                               (lambda (key default-val)
                                 (let ((fn (intern key))
                                       (node-fn (intern (concat "org-roam-node-" key)))
                                       (ksym (intern (concat ":" key))))
                                   (cond
                                    ((fboundp fn)
                                     (funcall fn node))
                                    ((fboundp node-fn)
                                     (funcall node-fn node))
                                    (t (let ((r (read-from-minibuffer (format "%s: " key) default-val)))
                                         (plist-put template-info ksym r)
                                         r)))))))
                    (file-path
                     ;; ✂️ removing the "read" interactive bit
                     (expand-file-name
                      (concat (file-name-as-directory org-roam-directory) template)
                      org-roam-directory)))
               (when (file-exists-p file-path)
                 (user-error "%s exists. Aborting" file-path))
               (org-cut-subtree)
               (save-buffer)
               (with-current-buffer (find-file-noselect file-path)
                 (org-paste-subtree)
                 (save-buffer) ;; HACK this save is  just for `org-roam-promote-entire-buffer' to find the file....
                 (while (> (org-current-level) 1) (org-promote-subtree))
                 (org-roam-promote-entire-buffer)
                 (save-buffer)
                 (org-roam-node-at-point)))))
         
         (defun qz/org-roam-extract-subtree-as-link (&rest args)
           (interactive)
           (save-excursion
             (qz/create-node)
             (let ((n (org-roam-extract-subtree)))
               (org-previous-visible-heading 1)
               (org-insert-subheading-respect-content nil)
               (insert (org-link-make-string (concat "id:" (org-roam-node-id n))
                                             (org-roam-node-title n))))))
         (defun qz/org-roam-capture-current ()
           (interactive)
           "Capture a task in agenda mode."
           (org-capture nil "I"))
         
         (defun qz/roam-capture-todo ()
           (interactive)
           "Capture a task in agenda mode."
           (cl-destructuring-bind (thing region)
               (qz/thing-at-point-or-region-and-region)
             (org-roam-capture- :goto t
                                :keys "n"
                                :node (org-roam-node-create :title thing)
                                :props `(:immediate-finish t :jump-to-captured nil
                                                           :region ,region     :insert-at ,(point-marker)
                                                           :finalize 'insert-link))
             (qz/capture-last-captured)))
         (defun qz/utc-timestamp ()
           (format-time-string "%Y%m%dT%H%M%SZ" (current-time) t))
         (setq qz/org-roam-capture-head "#+title: ${title}\n\n")
         (setq qz/capture-title-timestamp-roam "%(qz/utc-timestamp)-${slug}.org"
               org-roam-extract-new-file-path qz/capture-title-timestamp-roam)
         
         (setq org-roam-capture-templates
               `(("d" "default" plain "%?"
                  :if-new (file+head ,qz/capture-title-timestamp-roam
                                     ,qz/org-roam-capture-head)
                  :unnarrowed t)
                 ("n" "empty" plain "%?"
                  :if-new (file+head ,qz/capture-title-timestamp-roam
                                     ,qz/org-roam-capture-head)
                  :immediate-finish t)))
         (setq org-roam-mode-sections
               (list 'org-roam-backlinks-section
                     'org-roam-reflinks-section
                     ;; fuck it why not
                     'qz/org-roam-unlinked-references-section))
         (defun qz/org-roam-unlinked-references-section (node)
           "The unlinked references section for NODE.
         References from FILE are excluded."
           (when (and (executable-find "rg")
                      (org-roam-node-title node)
                      (not (string-match "PCRE2 is not available"
                                         (shell-command-to-string "rg --pcre2-version"))))
             (let* ((titles (cons (org-roam-node-title node)
                                  (org-roam-node-aliases node)))
                    (rg-command (concat "rg -L -o --vimgrep -P -i "
                                        (mapconcat (lambda (glob) (concat "-g " glob))
                                                   (org-roam--list-files-search-globs org-roam-file-extensions)
                                                   " ")
                                        (format " '\\[([^[]]++|(?R))*\\]%s' "
                                                (mapconcat (lambda (title)
                                                             (format "|(\\b%s\\b)" (shell-quote-argument title)))
                                                           titles ""))
                                        org-roam-directory))
                    (results (split-string (shell-command-to-string rg-command) "\n"))
                    f row col match)
               (magit-insert-section (unlinked-references)
                 (magit-insert-heading "Unlinked References:")
                 (dolist (line results)
                   (save-match-data
                     (when (string-match org-roam-unlinked-references-result-re line)
                       (setq f (match-string 1 line)
                             row (string-to-number (match-string 2 line))
                             col (string-to-number (match-string 3 line))
                             match (match-string 4 line))
                       (when (and match
                                  (not (file-equal-p (org-roam-node-file node) f))
                                  (member (downcase match) (mapcar 'downcase titles)))
                         (magit-insert-section section (org-roam-grep-section)
                           (oset section file f)
                           (oset section row row)
                           (oset section col col)
                           (insert
                            (propertize
                             (format "%s:%s:%s"
                                                       ;;; ✂️
                                     ;; removing my utc data
                                     ;; i.e.
                                     ;;   (replace-regexp-in-string "^.+Z-" "" "20210607T1536e21Z-mm.org")
                                     (s-pad-right
                                      25 " " (truncate-string-to-width
                                              (replace-regexp-in-string
                                               "^.+Z-" ""
                                               (file-name-base f))
                                              25 nil nil t))
                                                       ;;; ✂️
                                     row col)
                             'font-lock-face 'org-roam-dim)
                            " "
                            (org-roam-fontify-like-in-org-mode
                             (org-roam-unlinked-references-preview-line f row))
                            "\n"))))))
                 (insert ?\n)))))
         (defun qz/solar-moment (&optional time)
           (symbol-name
            (let ((i (string-to-number
                      (format-time-string "%H" (or time (current-time))))))
              (cond
               ((and (>= i 4)  (< i 7))  'dawn)
               ((and (>= i 7)  (< i 12)) 'morning)
               ((and (>= i 12) (< i 17)) 'afternoon)
               ((and (>= i 17) (< i 20)) 'evening)
               ((and (>= i 20)) 'night)
               (t 'void-time)))))
         ;; (qz/solar-moment)
         
         ;; (list
         ;;  (solar-sunrise-sunset-string
         ;;   (org-date-to-gregorian (format-time-string "%Y-%m-%d"))
         ;;   'no-location)
         ;;  (solar-exact-local-noon
         ;;   (org-date-to-gregorian
         ;;    (format-time-string "%Y-%m-%d"))))
         
         
         (defun qz/org-roam-dailies-capture-fulcrum (&optional time)
           ;; '(((> 4) . 'dawn)
           ;;   ((> 7) . 'morning)
           ;;   ((> 12) . 'afternoon)
           ;;   ((> 5) . 'evening)
           ;;   ((> 8) . 'night))
           (list "journal" (qz/solar-moment time)))
         
         ;; (qz/org-roam-dailies-capture-fulcrum)
         ;; TODO this is bonk and doesn't work
         (defun qz/region-quotada-o-nada (&rest args)
           "returns a (formatted) region if hacky-captured before dailies-capture
         
         intended for use in an `org-capture-template', which may safely
         drop this in; optionally including itself if it has any content.
         
         this is my workaround for the `%i' template being total nonsense
         to an expression in a regular template, such as `%(my-fun %i)' or
         `%(my-fun \"%i\")'.
         
         `%i' is true to form, but I can't grasp it as a list of
         args.
         "
           (if qz/hoist--org-capture
               (format "- region      ::\n  #+begin_quote\n%s\n  #+end_quote\n"
                       qz/hoist--org-capture)
             ""))
         (defun qz/peek-region ()
           "get the region.
         
         how many times have I written this"
           (interactive)
           (when-let ((s (and (region-active-p)
                              (filter-buffer-substring (region-beginning)
                                                       (region-end)))))
             (kill-new s)
             (qz/debug- (message "QZ peek-region :: %s" s))
             s))
         ;; TODO think about this; before/after with hoisted var as
         ;; hoist--hookfn-actfn
         ;;
         ;; (defmacro qz/hoist- (fn &optional unset?)
         ;;   `(setq ,(intern (format "qz/hoist--%s" name))
         ;;          ,(when (not unset?) (funcall fn))))
         
         (qz/advice- org-capture :before qz/hoist-peek-region)
         (qz/advice- org-capture :after  qz/unhoist-peek-region)
         
         (defun qz/hoist-peek-region (&rest args)
           (qz/debug- (message "QZ hoist"))
           (setq qz/hoist--org-capture (qz/peek-region)))
         
         (defun qz/unhoist-peek-region (&rest args)
           (qz/debug- (message "QZ unhoist"))
           (setq qz/hoist--org-capture nil))
         (defun qz/peek-thing ()
           (interactive)
           (let ((s ())
                 (kill-new s)
                 (qz/debug- (message "QZ peek-region :: %s" s))
                 s)))
         
         (defun qz/peek-look-mouse ()
           (interactive)
           (let ((s (posn-col-row
                     (let* ((m (mouse-pixel-position))
                            (xy (cdr m)))
                       (posn-at-x-y (car xy) (cdr xy) (car m))))))
             (kill-new s)
             (qz/debug- (message "QZ peek-region :: %s" s))
             s))
         (defun qz/capture-tab-or-nada (&rest args)
           (if-let ((d (qz/tab nil 'as-link)))
               (format "- from: tab   :: %s" d)
             ""))
         
         (defun qz/dailies-capture-template (&optional time)
           `(("d" "default" entry
              "* [%<%H:%M:%S>]
         - from: point :: %a
         - clocking    :: %K
         %(qz/capture-tab-or-nada)
         %(qz/region-quotada-o-nada)
         
         %?"
              :if-new (file+head+olp
                       ,qz/org-roam-dailies-filespec
                       ,(s-join "\n" '("#+title: <%<%Y-%m-%d>>"
                                       "#+filetags: daily private project" "" ""
                                       "%(qz/today-dateref)" "" ""
                                       "* today, I will"
                                       "** daily review"
                                       "** life"
                                       "** work"))
                       ,(qz/org-roam-dailies-capture-fulcrum time)))))
         
         (defun qz/dailies-capture-template--set (&rest args)
           (setq org-roam-dailies-capture-templates
                 (qz/dailies-capture-template (current-time))))
         
         (qz/advice-
          org-roam-dailies-capture-today
          :before qz/dailies-capture-template--set)
         ;;; day lookup
         (defvar qz/day-lookup
           '((Mon . "[[id:d5ad0bac-e82b-43d0-960f-26eeb1daf91b][Monday]]")
             (Tue . "[[id:cb662cc6-bde2-4f9c-b3fa-62346c6df27a][Tuesday]]")
             (Wed . "[[id:411a8e5a-8d89-4886-b2ea-047a3970710a][Wednesday]]")
             (Thu . "[[id:659b9931-ae09-422b-8e91-1bf4cc58e94c][Thursday]]")
             (Fri . "[[id:b3255cd1-db37-4e07-99cf-5e60d52a2579][Friday]]")
             (Sat . "[[id:b63897c3-30cc-42eb-83b5-c8e372e5af9a][Saturday]]")
             (Sun . "[[id:2e28574b-4793-4c05-b83d-e36e9a77515b][Sunday]]"))
           "an index; get days from abbrev (assoc 'Mon qz/day-lookup)")
         (defvar qz/month-lookup
           '("[[id:b92355d7-110e-467c-b7a7-d02b2043af3f][January]]"
             "[[id:7e0af966-8d3e-4e88-b53f-d074902e175a][February]]"
             "[[id:f41751f8-a2a9-4b38-ba03-2ceec2fae4cc][March]]"
             "[[id:ae0ae458-2216-4178-8073-4a26f23747d9][April]]"
             "[[id:6a680100-e842-4257-819f-8cf6cbedddbc][May]]"
             "[[id:f811621c-1b37-43f7-9d01-52bdf9f27637][June]]"
             "[[id:a4d5c8fe-3910-4483-b59e-ce50cd6699a7][July]]"
             "[[id:94e9b0a7-6cd0-4104-821e-613876fe76e3][August]]"
             "[[id:f9ad8160-cae5-4195-a85f-0160710ce8dd][September]]"
             "[[id:da9f0d53-e3f7-4f72-bc1a-d060bc2d1745][October]]"
             "[[id:a4e3a97a-dac9-4bc6-a5e9-5949f707a6de][November]]"
             "[[id:f874ca1a-0d3f-4840-8340-511ed0ac286f][December]]")
           "an index; get days from abbrev (nth 0 qz/month-lookup)")
         (defun qz/today-dateref (&optional time)
           (cl-destructuring-bind (day nday month year)
               (split-string
                (format-time-string "%a:%d:%m:%Y" (or nil (current-time))) ":")
             (format "%s %s %s, %s"
                     (cdr (assoc (intern day) qz/day-lookup))
                     nday
                     (nth (- (string-to-number month) 1) qz/month-lookup)
                     (or (if-let ((node (org-roam-node-from-title-or-alias year)))
                             (org-link-make-string
                              (concat "id:" (org-roam-node-id node))
                              (org-roam-node-title node)))
                         year))))
         (defun qz/org-daily-tangent-capture ()
           (interactive)
           "Capture the inevitable tangent"
           (org-capture nil "t"))
         (defun qz/today-as-daily-file ()
           (format-time-string "private-%Y-%m-%d.org"))
         ;; [[file:~/.doom.d/config.org::*capture convenience functions][capture convenience functions]]
         (defun qz/current-roam-link ()
           "Get link to org-roam file with title"
           (interactive)
         
           (concat "* TODO "
                   (let ((n (qz/org-roam-node-at-point)))
                     (org-link-make-string
                      (concat "id:" (org-roam-node-id n))
                      (org-roam-node-title n)))))
         (defun qz/node-tags (&optional node)
           (or (and node (org-roam-node-tags node))
               (save-excursion
                 (goto-char (org-roam-node-point (org-roam-node-at-point 'assert)))
                 (if (= (org-outline-level) 0)
                     (split-string-and-unquote (or (cadr (car (org-collect-keywords '("filetags")))) ""))
                   (org-get-tags)))))
         
         (defun qz/node-title (&optional node limit)
           (or (and node (org-roam-node-title node))
               (save-excursion
                 (goto-char (org-roam-node-point (org-roam-node-at-point 'assert)))
                 (if (= (org-outline-level) 0)
                     (cadr (car (org-collect-keywords '("title"))))
                   (substring-no-properties (org-get-heading t t t))))))
         (defun qz/title->roam-id (title)
           (org-roam-node-id (org-roam-node-from-title-or-alias title)))
         (defun qz/ensure-tag (tagstring tag)
           "Apply `org-roam-tag-add' for `tag' to `(OR node@pt NODE)'"
           (let ((ltag (-flatten (or (and (listp tag) tag)
                                     (list tag)))))
             (message "ensuring tag for %s" ltag)
             (org-roam-tag-add ltag)))
         
         (defun qz/timestamp ()
           (format-time-string "[%Y-%m-%d %a %H:%M]"))
         
         (defun qz/org-roam--insert-timestamp (&rest args)
           (when (not (org-entry-get nil "CREATED"))
             (org-entry-put nil "CREATED" (qz/timestamp)))
           (qz/org-roam--updated-timestamp))
         
         (defun qz/org-roam--updated-timestamp (&rest args)
           "on the current-heading, and current-node"
           (interactive)
           (mapcar (lambda (pt)
                     (when pt
                       (org-entry-put
                        pt "UPDATED"
                        (qz/timestamp))))
                   (list (and (org-roam-node-at-point)
                              (org-roam-node-point (org-roam-node-at-point)))
                         (save-excursion
                           (org-back-to-heading-or-point-min)
                           (point)))))
         
         ;; FIXME
         (add-hook 'org-roam-capture-new-node-hook 'qz/org-roam--insert-timestamp)
         
         (add-hook 'org-mode-hook (lambda ()
                                    (add-hook 'before-save-hook
                                              'qz/org-roam--updated-timestamp nil t)))
         (qz/advice- org-id-get-create :before qz/org-roam--insert-timestamp)
         (defun qz/hard-refresh-org-tags-in-buffer ()
           (interactive)
           (setq org-file-tags nil)      ; blast the cache
           (org-set-regexps-and-options) ; regen property detection regexp
           (org-get-tags))               ; write to cache
         (defun qz/title-to-tag (title &optional capitalize?)
           "Convert TITLE to tag."
           (if (equal "@" (cl-subseq title 0 1))
               title
             (concat "@" (s-replace " " ""
                                    (or (and capitalize?
                                             (capitalize title))
                                        title)))))
         (defun qz/org-roam-node-from-tag (tag)
           (seq-map
            'car
            (org-roam-db-query
             [:select :distinct file
                      :from tags
                      :inner :join nodes
                      :on (= tags:node_id nodes:id)
                      :where (= tags:tag tag)])))
         (defun qz/note-buffer-p (&optional node &rest _)
           "Return non-nil if the currently visited buffer is a note."
           (interactive)
           (or (org-roam-node-p node)
               (and buffer-file-name (org-roam-file-p buffer-file-name))))
         (defun qz/is-private-p (&optional node &rest _)
           (interactive)
           (let ((title (qz/node-title node)))
             (if (not title)
                 (and (message "unable to evaluate privateness; no title") nil) ; return false (not private)
               (or (string-match-p qz/daily-title-regexp title) ; daily
                   (string-match-p "meeting" title)             ; concerns a meeting
                   (qz/has-link-to-p                            ; concerns work
                    (mapcar 'qz/title->roam-id
                            (list "thinkproject" "NewStore")))))))
         (defun qz/has-links (node)
           "connections exist, for id of `node'"
           (org-roam-db-query
            [:select [source dest]
                     :from links
                     :where (or  (= dest $s1)
                                 (= source $s1))]
            node))
         
         (defun qz/node-has-links (node)
           "connections exist, for `node'"
           (qz/has-links (org-roam-node-id node)))
         (defun qz/has-link-to-p (dst &optional src)
           "directed connection exists, from `src' to `dst'"
           (if-let* ((nap (or src (org-roam-node-at-point)))
                     (src (or src (org-roam-node-id nap))))
               (org-roam-db-query
                [:select dest
                         :from links
                         :where (and (= source $s1)
                                     (IN dest $v2))]
                src (apply 'vector (qz/ensure-list dst)))))
         
         (defun qz/node-has-link-to-p (dst &optional src)
           (qz/has-link-to-p (org-roam-node-id dst)
                             (and dst (org-roam-node-id dst))))
         ;;; ref capture
         (setq org-roam-capture-ref-templates
               `(("r" "ref" plain
                  "\n#+begin_quote\n${body}\n#+end_quote\n%?"
                  :if-new (file+head ,qz/capture-title-timestamp-roam
                                     "#+title: ${title}\n")
                  :unnarrowed t)
                 ("y" "ref" plain
                  ,(s-join "\n" '("* %U"
                                  "${body}" ""
                                  ;; TODO why a buffer "*Capture*" is popped when the mpv function is run?
                                  "(rde-mpv-play-url \"${ref}\" \"best\" :select nil)"
                                  ;; "%(sleep-for 1)" ;; slee
                                  "%?"))
                  :if-new (file+head ,qz/capture-title-timestamp-roam
                                     "#+title: ${title}\n")
                  :unnarrowed t)
                 ))
         (defun qz/roam-buffer-image-width ()
           (setq-local org-image-actual-width 150)
           (org-redisplay-inline-images))
         
         (add-hook 'org-roam-mode-hook 'qz/roam-buffer-image-width)
         (cons->table
          (add-to-list 'magit-section-initial-visibility-alist (cons 'org-roam-node-section 'hide)))
         (setq org-roam-graph-executable "neato")
         (setq org-roam-graph-extra-config '(("overlap" . "false")))
         ;; [[file:~/.doom.d/config.org::*templates][templates]]
         (setq org-capture-templates
               `(;; basic fire&forget
                 ("i" "inbox" entry
                  (file ,(concat org-roam-directory "/inbox.org"))
                  "* TODO %? \n\n - from :: %a")
         
                 ;; spanish language capturing
                 ("v" "vocab; spanish" entry
                  (file+headline ,(concat org-roam-directory "/spanish_language.org") "vocab, phrases")
                  ,(s-join "\n" '("** \"%?\" :es:"
                                  "- from :: %a" ""
                                  "*** :en:" "")))
         
                 ;; capture link to live `org-roam' thing
                 ("n" "now, as in NOW" entry (file ,(concat org-roam-directory "/wip.org"))
                  ,(s-join "\n" '("* TODO [#A1] %? "
                                  "DEADLINE: %T"
                                  "CREATED: %u")))
         
                 ;; fire directly into inbox from outside of emacs
                 ("c" "org-protocol-capture" entry (file ,(concat
                                                           org-roam-directory "/inbox.org"))
                  ,(s-join "\n" '("* TODO [[%:link][%:description]]" ""
                                  "#+begin_quote" ""
                                  "%i"
                                  "#+end_quote"))
                  :immediate-finish t)
         
                 ;; push last captured item into inbox
                 ("l" "last-capture" entry (file ,(concat org-roam-directory "/inbox.org"))
                  (function qz/inbox-last-captured)
                  :immediate-finish t)
         
                 ("I" "current-roam" entry (file ,(concat org-roam-directory "/inbox.org"))
                  (function qz/current-roam-link)
                  :immediate-finish t)
         
                 ("W" "weekly review" entry
                  (file+datetree ,(concat org-roam-directory "/reviews.org"))
                  ;; ... from template
                  (file ,(concat org-roam-directory "/templates/weekly_review.org")))
         
                 ("D" "daily review" entry
                  (file+datetree ,(concat org-roam-directory "/reviews.org"))
                  ;; ... from template
                  (file ,(concat org-roam-directory "/templates/daily_review.org")))
         
                 ("S" "screenshot" entry
                  (file ,(concat org-roam-directory "/screenshots.org"))
                  ,(s-join "\n" '("* screenshot: %?" ""
                                  "%(qz/screenshot-clip)"
                                  )))
                 ))
         
         
         (defun qz/screenshot-clip ()
           (interactive)
           (let ((default-directory (concat org-roam-directory "/images")))
             (s-replace "file:" (format "file:%s/" default-directory)
                        (with-temp-buffer
                          (let ((default-directory default-directory))
                            (org-mode)
                            (org-download-clipboard)
                            (buffer-string))))))
         
         
         
         
         ;; (setq org-screenshot-method ;;"/gnu/store/n4arghf8l3f6svv2xlxwnvw4jcwa48qk-sway-shot-output %s"
         ;;       "/gnu/store/am017g4gdhf45kvg5xkp3s2lhkbfdwzh-sway-shot-window-or-selection %s")
         
         
         ;; [[file:~/.doom.d/config.org::*capture templates][roam capture templates]]
         
         ;; NOWEB ROAM END
         )
       (require 'org-download)
       (defun org-download--dir-2 () "screenshots")
       (setq qz/bullets--newline? nil
             qz/bullets--spacing 2
             qz/bullets--list
             '((◉ ○ ◈ ◇ ✳)
               (🐕 🦮 🌭 🐶) ;; #{🐕\x200d;🦺}# 🐕‍🦺
               (🍀 🍮 🫰 🫶)
               (🙈 🙉 🙊)
       
               (🌑 🌒 🌓 🌔 🌕 🌖 🌗 🌘)
               (🌑 🌒 🌓 🌔 🌕 🌖 🌗 🌘 🌙 🌚 🌛 🌜)
       
               ("ฅ՞•ﻌ•՞ฅ" "ʕっ￫ᴥ￩ʔっ" "ʕ ꈍᴥꈍʔ" "ᵒ̴̶̷̥ ᆽᵒ̴̶̷̥") ;; best with no spacing per-level
               (ہ ﻩ)
               ("") ; minimalist
       
       
               ;;(¹ ) char 185
               ))
       
       
       (defun qz/org-modern--choose-bullets (&optional n)
         (interactive)
         (setq qz/bullets
               (or (and n (nth n qz/bullets--list))
                   (let ((v (mapcar (lambda (b) (cons (format "%s" b) b))
                                    qz/bullets--list)))
                     (cdr (assoc (completing-read "bullets? " (mapcar 'car v))
                                 v))))
               org-modern-star
               (cl-loop for star in
                        (seq-take
                         (-cycle qz/bullets
                                 ;; ╱|、
                                 ;;(˚ˎ。7
                                 ;; |、˜〵
                                 ;; じしˍ,)ノ
                                 ) 60)
                        and i from 0
                        collect (format "%s%s%s"
                                        (if qz/bullets--newline? "\n" "")
                                        (make-string (* i qz/bullets--spacing) 32) ;; spc
                                        star))))
       
       (qz/org-modern--choose-bullets 8)
       (setq org-modern-list
             '((43 . "◦")
               (45 . "–")
               (42 . "•")))
       
       (mapcar (lambda (e) (char-to-string (car e))) org-modern-list)
       (setq org-modern-tag t)
       
       (defvar qz/orgtbl-plot-function
         ;;'orgtbl-uc-draw-cont
         'orgtbl-uc-draw-grid
         ;;'orgtbl-ascii-draw
         )
       
       (defun qz/orgtbl-plot (&optional ask)
         "Draw an ASCII bar plot in a column.
       
       With cursor in a column containing numerical values, this function
       will draw a plot in a new column.
       
       ASK, if given, is a numeric prefix to override the default 12
       characters width of the plot.  ASK may also be the `\\[universal-argument]' \
       prefix,
       which will prompt for the width."
         (interactive "P")
         (let ((col (org-table-current-column))
               (min most-positive-fixnum)		 ; will be converted to infinity
               (max most-negative-fixnum)		 ; which is the desired result
               (table (org-table-to-lisp))
               (length
                (cond ((consp ask)
                       (read-number "Length of column " 12))
                      ((numberp ask) ask)
                      (t 12))))
           ;; Skip any hline a the top of table.
           (while (eq (car table) 'hline) (pop table))
           ;; Skip table header if any.
           (dolist (x (or (cdr (memq 'hline table)) table))
             (when (consp x)
               (setq x (nth (- col 1) x))
               (when (string-match
                      "^[-+]?\\([0-9]*[.]\\)?[0-9]*\\([eE][+-]?[0-9]+\\)?$"
                      x)
                 (setq x (string-to-number x))
                 (when (> min x) (setq min x))
                 (when (< max x) (setq max x)))))
           (org-table-insert-column)
           (org-table-move-column-right)
           (org-table-store-formulas
            (cons
             (cons
              (concat "$" (number-to-string (+ 1 col)))
              (format "'(%s $%s %s %s %s)"
                      qz/orgtbl-plot-function col min max length))
             (org-table-get-stored-formulas)))
           (org-table-recalculate t)))
       
       (define-key org-mode-map (kbd "C-c \" a") 'qz/orgtbl-plot)
       (defun qz/org-inbox-capture ()
         (interactive)
         "Capture a task in agenda mode."
         (org-capture nil "i"))
       (define-key org-mode-map (kbd "C-<return>")
                   'org-insert-heading-respect-content)
       (define-key org-mode-map (kbd "C-S-<return>")
                   'org-insert-heading)
       
       (defun org-insert-subheading-respect-content (&rest arg)
         (interactive "P")
         (let ((org-insert-heading-respect-content t))
           (org-insert-subheading arg)
           ;; [[id:28622111-aa1e-4d9a-ac23-0140a67e3d7f]]
           (end-of-line)))
       
       
       (define-key org-mode-map (kbd "C-M-<return>")
                   'org-insert-subheading-respect-content)
       (define-key org-mode-map (kbd "C-M-S-<return>")
                   'org-insert-subheading)
       
       (define-key org-mode-map (kbd "C-c C-M-<") 'org-do-promote)
       (define-key org-mode-map (kbd "C-c C-M->") 'org-do-demote)
       
       (define-key org-mode-map (kbd "C-c C-M-p")
                   (lambda ()
                     (interactive)
                     (make-marker)
                     (org-up-heading-or-point-min)))
       (setq org-agenda-columns-add-appointments-to-effort-sum t)
       (setq org-agenda-default-appointment-duration 30)
       (add-to-list 'org-global-properties
                    '("Effort_ALL" . "0 0:15 0:30 1:00 2:00 4:00 8:00"))
       (setq org-confirm-babel-evaluate nil)
       ;; [[file:~/.doom.d/config.org::*refile][refile]]
       (setq org-refile-targets '(("reading.org" :level . 0)
                                  ("emacs.org" :level . 0)
                                  ("watching.org" :level . 0)
                                  ("learning.org" :level . 0)
                                  ("inbox.org" :level . 0)
                                  ("sample.org" :level . 0)
                                  ("wip.org" :level . 0)))
       (setq org-log-refile 'note)
       (setq org-log-redeadline 'note)
       (setq org-log-reschedule 'note)
       (setq org-log-done 'note)
       (setq org-todo-keywords '((sequence "TODO" "NEXT" "DONE")))
       ;;(setq org-startup-folded 'content)
       (setq org-tags-column -85)    ;; auto works better for olivetti
       ;; (setq org-tags-column 120) ;; for wide screens & no 80char limiting
       (setq org-tag-alist
             '(("@errand" . ?e)
               ("@work" . ?w)
               ("@home" . ?h)
               ("@blog" . ?B)
               (:newline)
               ("emacs" . ?E)
               ("wip" . ?W)
               ("CANCELLED" . ?c)
               (:newline)
               ("learning" . ?l)
               ("research" . ?r)
               (:newline)
               ("book" . ?b)
               ("article" . ?a)
               ("paper" . ?p)
               (:newline)
               ("talk" . ?t)
               ("film" . ?f)))
       
       ;;(cons->table org-tag-alist)
       (setq org-enforce-todo-dependencies t)
       (setq org-enforce-todo-checkbox-dependencies t)
       (setq org-tags-exclude-from-inheritance '("daily" "project"))
       (setq org-hierarchical-todo-statistics nil)
       (defun qz/org-choose-current-attachment ()
         (let ((attach-dir (org-attach-dir)))
           (if attach-dir
               (let* ((file (pcase (org-attach-file-list attach-dir)
                              (`(,file) file)
                              (files (completing-read "Open attachment: "
                                                      (mapcar 'list files) nil t))))
                      (path (expand-file-name file attach-dir)))
                 path))))
       
       (defun qz/org-insert-current-attachment ()
         (interactive)
         (insert
          (format "[[file:./%s]]"
                  (dired-make-relative
                   (qz/org-choose-current-attachment)))))
       
       (define-key org-mode-map (kbd "C-c M-a") 'qz/org-insert-current-attachment)
       
       (defun qz/org-insert-last-stored-link (arg)
         "Insert the last link stored in `org-stored-links'."
         (interactive "p")
         (qz/org-insert-all-links arg "" "\n"))
       
       (defun qz/org-insert-all-links (arg &optional pre post)
         "Insert all links in `org-stored-links'.
       When a universal prefix, do not delete the links from `org-stored-links'.
       When `ARG' is a number, insert the last N link(s).
       `PRE' and `POST' are optional arguments to define a string to
       prepend or to append."
         (interactive "P")
         (let ((org-link-keep-stored-after-insertion (equal arg '(4)))
               (links (copy-sequence org-stored-links))
               (pr (or pre "- "))
               (po (or post "\n"))
               (cnt 1) l)
           (if (null org-stored-links)
               (message "No link to insert")
             (while (and (or (listp arg) (>= arg cnt))
                         (setq l (if (listp arg)
                                     (pop links)
                                   (pop org-stored-links))))
               (setq cnt (+ 1 cnt))
               (insert pr)
               (message "%s" `((:l ,l)
                               (:car-l ,(car l))
                               (:cadr-l ,(cadr l))
                               (:mod-l ,(car (last (s-split "/" (car l)))))))
               (org-insert-link
                nil (car l)
                (or (cadr l)
                    (qz/ol-file l)))
               (insert po)))))
       
       (define-key org-mode-map (kbd "C-c M-l") 'qz/org-insert-last-stored-link)
       (require 'f)
       
       (defun qz/ol-file (link)
         "transform file path into pretty ol-output
                   - respect projects; truncate prior path, keeping only basename
       "
         ;; (car (last (s-split "/" "file:~/sys/rde/goop.boop::pattern")))
         ;; == "goop.boop::pattern"
         ;; (message  "HELLO :: %s/%s" p (car (last (s-split "/" p))))
         (let* ((p (car link))
                (inner (mapcar
                        (lambda (s) (let ((ss (car s)))
                                      (and (s-contains? (f-base ss) p)
                                           (cons (f-base ss) ss))))
                        project--list))
                (suffix (s-join
                         " . "
                         ;; sort by length of path desc, taking the innermost subproj
                         ;; take first value of first result
                         (--tb (cl-sort (remove nil inner)
                                        (lambda (a b) (gt (length a) (length b)))
                                        :key 'cdr)
                               (mapcar 'car)
                               (reverse)))))
           (format "%s%s"
                   (or (and suffix (format "(%s)" suffix)) "")
                   (car (last (s-split "/" p))))))
       
       ;;(car org-stored-links)
       ;;(qz/ol-file (car org-stored-links))
       (defalias '--tb '->>)
       (defalias '--tf '->)
       (defalias 'gt '>)
       (defalias 'lt '<)
       (defun qz/create-excluded-ids-for-headlines-in-buffer ()
         "Add ID properties to all headlines in the current file which
       do not already have one."
         (interactive)
         (when (file-exists-p (buffer-file-name (current-buffer)))
           (org-map-entries (lambda (&rest r)
                              (unless (ignore-errors (org-id-get))
                                (ignore-errors (org-id-get-create))
                                (org-set-property "ROAM_EXCLUDE" "t"))))))
       
       
       (add-hook 'org-mode-hook
                 (lambda ()
                   (add-hook 'before-save-hook
                             'qz/create-excluded-ids-for-headlines-in-buffer nil 'local)))
       
       (setq org-id-link-to-org-use-id t)
       (setq qz/org-image-actual-width 640)
       (defun qz/org-image-width (&optional n)
         (setq org-image-actual-width (or n qz/org-image-actual-width)))
       (qz/org-image-width)
       ;;(require 'org)
       
       (defun org-cycle-hide-drawers (state)
         "Re-hide all drawers after a visibility state change."
         (when (and (derived-mode-p 'org-mode)
                    (not (memq state '(overview folded contents))))
           (save-excursion
             (let* ((globalp (memq state '(contents all)))
                    (beg (if globalp
                             (point-min)
                           (point)))
                    (end (if globalp
                             (point-max)
                           (if (eq state 'children)
                               (save-excursion
                                 (outline-next-heading)
                                 (point))
                             (org-end-of-subtree t)))))
               (goto-char beg)
               (while (re-search-forward org-drawer-regexp end t)
                 (save-excursion
                   (beginning-of-line 1)
                   (when (looking-at org-drawer-regexp)
                     (let* ((start (- (match-beginning 0) 1))
                            (limit
                             (save-excursion
                               (outline-next-heading)
                               (point)))
                            (msg (format
                                  (concat
                                   "org-cycle-hide-drawers:  "
                                   "`:END:`"
                                   " line missing at position %s")
                                  (+ 1 start))))
                       (if (re-search-forward "^[ \t]*:END:" limit t)
                           (outline-flag-region start (point-at-eol) t)
                         (user-error msg))))))))))
       
       (defun qz/org-align-tags ()
         (interactive)
         (org-align-tags 'yes-all-the-bloody-tags))
       (defun qz/org-sort-subtree ()
         (interactive)
         (save-mark-and-excursion
           (org-up-heading-or-point-min)
           (call-interactively 'org-sort)))
       
       (define-key org-mode-map (kbd "C-c C-M-6") 'qz/org-sort-subtree)
       (require 'ox)
       (require 'ox-md)
       
       (defun qz/org-md-nolink (link contents info)
         (format "%s" contents))
       
       (org-export-define-derived-backend 'my-md 'md
         :menu-entry
         '(?M "Export to Markdown without links" (lambda (a s v b) (org-md-export-to-markdown a s v)))
         :translate-alist '((link . qz/org-md-nolink)))
       (defun qz/org-export-headline (&optional backend async subtreep visible-only body-only ext-plist)
         "Export the current Org headline using BACKEND.
       
       The available backends are the ones of `org-export-backends' and
       'pdf.
       
       When optional argument SUBTREEP is non-nil, transcode the
       sub-tree at point, extracting information from the headline
       properties first.
       
       When optional argument VISIBLE-ONLY is non-nil, don't export
       contents of hidden elements.
       
       When optional argument BODY-ONLY is non-nil, only return body
       code, without surrounding template.
       
       Optional argument EXT-PLIST, when provided, is a property list
       with external parameters overriding Org default settings, but
       still inferior to file-local settings."
         (interactive)
         (let* ((backend (unless backend
                           (intern
                            (completing-read "Available backends: "
                                             (append org-export-backends '(pdf slack))))))
                (headline (car (last (org-get-outline-path t))))
                (headline-alnum (replace-regexp-in-string "[^[:alnum:]-_]" "-" headline))
                (file-prefix (file-name-sans-extension (buffer-file-name)))
                (filename (format "%s-%s.%s" file-prefix headline-alnum
                                  (cl-case backend
                                    (pdf   "tex")
                                    (slack "md")
                                    (t backend)))))
           (save-restriction
             (org-narrow-to-subtree)
             (kill-new (s-join " -> " (org-get-outline-path t nil)))
             (org-export-to-file
                 (if (eq backend 'pdf) 'latex backend)
                 filename async subtreep visible-only body-only ext-plist
                 (when (eq backend 'pdf)
                   (lambda (file) (org-latex-compile file))))
             (widen))
           (with-temp-buffer
             (insert-file-contents filename)
             (kill-new (buffer-string)))))
       (define-key org-mode-map (kbd "C-c C-M-e") 'qz/org-export-headline)
       (defun org-slack-headline (headline contents info)
         "Transcode HEADLINE element into Markdown format.
       CONTENTS is the headline contents.  INFO is a plist used as
       a communication channel."
         (unless (org-element-property :footnote-section-p headline)
           (let* ((level (org-export-get-relative-level headline info))
                  (title (org-export-data (org-element-property :title headline) info))
                  (todo (and (plist-get info :with-todo-keywords)
                             (let ((todo (org-element-property :todo-keyword
                                                               headline)))
                               (and todo (concat (org-export-data todo info) " ")))))
                  (tags (and (plist-get info :with-tags)
                             (let ((tag-list (org-export-get-tags headline info)))
                               (and tag-list
                                    (concat "     " (org-make-tag-string tag-list))))))
                  (priority
                   (and (plist-get info :with-priority)
                        (let ((char (org-element-property :priority headline)))
                          (and char (format "[#%c] " char)))))
                  ;; Headline text without tags.
                  (heading (concat todo priority title)))
             (format "%s*%s*\n\n%s"
                     (if (gt level 1)
                         (concat (s-repeat (- level 3) ;; starting flush
                                           "  ")
                                 "- ")
                       "")
                     title contents))))
       )
     (message "post org: %s" (shell-command-to-string "date"))
     ;; NOWEB ORG END
     (with-eval-after-load 'slack
       (defun slack-conversations-list (team success-callback &optional types)
         (let ((lexical t)
               (cursor nil)
               (channels nil)
               (groups nil)
               (ims nil)
               (types (or types (list ;;"public_channel"
                                 "private_channel"
                                 "mpim"
                                 "im"))))
           (cl-labels
               ((on-success
                  (&key data &allow-other-keys)
                  (slack-request-handle-error
                   (data "slack-conversations-list")
                   (cl-loop for c in (plist-get data :channels)
                            do (cond
                                ((eq t (plist-get c :is_channel))
                                 (push (slack-room-create c 'slack-channel)
                                       channels))
                                ((eq t (plist-get c :is_im))
                                 (push (slack-room-create c 'slack-im)
                                       ims))
                                ((eq t (plist-get c :is_group))
                                 (push (slack-room-create c 'slack-group)
                                       groups))))
                   (slack-if-let*
                       ((meta (plist-get data :response_metadata))
                        (next-cursor (plist-get meta :next_cursor))
                        (has-cursor (< 0 (length next-cursor))))
                       (progn
                         (setq cursor next-cursor)
                         (request))
                     (funcall success-callback
                              channels groups ims))))
                (request ()
                  (slack-request
                   (slack-request-create
                    slack-conversations-list-url
                    team
                    :params (list (cons "types" (mapconcat 'identity types ","))
                                  (and slack-exclude-archived-channels (cons "exclude_archived" "true"))
                                  (and cursor (cons "cursor" cursor)))
                    :success 'on-success))))
             (request)))))
     (when (fboundp 'adaptive-wrap-prefix-mode)
       (add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode))
     (setq org-tag-alist
           '(("@errand" . ?e)
             ("@work" . ?w)
             ("@home" . ?h)
             ("@blog" . ?B)
             (:newline)
             ("emacs" . ?E)
             ("task" . ?t)
             ("CANCELLED" . ?C)
             (:newline)
             ("learning" . ?l)
             ("research" . ?r)
             (:newline)
             ("book" . ?b)
             ("article" . ?a)
             ("paper" . ?p)
             ("talk" . ?t)
             ("film" . ?f)))
     (setq sentence-end-double-space t)
     (defun qz/get-mail ()
       (interactive)
       (async-shell-command "mbsync -Va && notmuch new"))
     (defun qz/rde-sanity ()
       (interactive)
       (async-shell-command
        (concat "cd $HOME/git/sys/rde"
                "&& guix repl -L src -L examples/src dev/sanity.scm")))
     (setq qz/emacs/config "~/git/sys/rde/rde/examples/abcdw/configs.org"
           qz/sh/tangle "make -C $HOME/git/sys/rde/rde/examples/abcdw tangle ")
     
     (defun qz/tangle ()
       (interactive)
       (async-shell-command
        (concat
         "make -C $HOME/git/sys/rde/examples tangle"
         " && echo 'tangle--ehg--di' | espeak --stdin")))
     
     (defun qz/suspend ()
       (interactive)
       (async-shell-command "loginctl suspend"))
     
     
     (defun qz/redshift-fulldark ()
       (interactive)
       (async-shell-command "guix shell redshift-wayland -- redshift -O 1000K -r -b .4 "
                            "*process:redshift*"))
     
     (defun qz/redshift-full ()
       (interactive)
       (async-shell-command "guix shell redshift-wayland -- redshift -O 1000K -r -b .7 "
                            "*process:redshift*"))
     
     (defun qz/redshift-fullbright ()
       (interactive)
       (async-shell-command "guix shell redshift-wayland -- redshift -O 1000K -r -b 1 "
                            "*process:redshift*"))
     
     (defun qz/redshift-mellow ()
       (interactive)
       (async-shell-command "guix shell redshift-wayland -- redshift -O 2000K -r -b .7 "
                            "*process:redshift*"))
     (defun qz/redshift-mellowbright ()
       (interactive)
       (async-shell-command "guix shell redshift-wayland -- redshift -O 2000K -r -b 1 "
                            "*process:redshift*"))
     
     (defun qz/dmesg ()
       (interactive)
       (async-shell-command
        "sudo dmesg -w"
        "*process:dmesg*"))
     
     (defun qz/reload-config-home ()
       (interactive)
       (save-some-buffers)
       (async-shell-command
        (concat
         "make -C $HOME/git/qz/dotfiles home"
         "&& echo 'home--bal-ehg--di' | espeak --stdin ")
        "*process:dotfiles-reload*"))
     
     (defun qz/build-config-home ()
       (interactive)
       (save-some-buffers)
       (async-shell-command
        (concat
         "make -C $HOME/git/qz/dotfiles hbuild"
         "&& echo 'home--bal-ehg--di' | espeak --stdin ")
        "*process:dotfiles-reload*"))
     
     (defun qz/reload-config-system ()
       (interactive)
       (async-shell-command
        (concat
         "make -C $HOME/git/qz/dotfiles tangle && sudo -E make -C $HOME/git/qz/dotfiles system"
         "&& echo 'system--bal-ehg--di' | espeak --stdin")
        "*process:dotfiles-reload*"))
     
     (defun qz/reload-config-all ()
       (interactive)
       (async-shell-command
        (concat
         "   make -C $HOME/git/qz/dotfiles -B guix"
         "&& make -C $HOME/git/qz/dotfiles home"
         "&& sudo -E make -C $HOME/git/qz/dotfiles system"
         "&& echo 'do the do, like ooo; pull & home & system bal-ehg--di'"
         "   | espeak --stdin")
        "*process:dotfiles-reload*"))
     
     (defun qz/reload-config-emacs ()
       (interactive)
       (load-file "~/.config/emacs/init.el"))
     
     (defun qz/guix-pull ()
       (interactive)
       (async-shell-command
        "make -C $HOME/git/qz/dotfiles -B guix"
        "*process:dotfiles-reload*"))
     (defun qz/guix-describe-lag ()
       "
     
     "
     
     
       (interactive))
     
     (defun qz/guix-upgrade ()
       (interactive)
       (async-shell-command
        (s-join " "
                '("make -C $HOME/git/sys/rde/examples -B guix"
                  "&& make rde/channels/update-locked"
                  "&& make rde/channels/pull-locked"
                  "&& guix package -u"
                  "&& guix upgrade"
                  "&& make"))))
     (defun qz/sway-choose-output-res (&optional display res)
       (interactive)
       (let* ((cur (s-trim (shell-command-to-string
                            "swaymsg -t get_outputs | jq -r 'map( . | select(.focused == true) | .name) | first'")))
              (cmd (format "swaymsg 'output %s enable res %s'"
                           (or display
                               (completing-read "display: "
                                                '("DP-1" "DP-2"
                                                  "eDP-1"
                                                  "HDMI-1" "HDMI-2")
                                                nil t cur))
                           (or res
                               (completing-read "resolution: "
                                                '("1920x1080"
                                                  "3840x1080"
                                                  "5120x1440")
                                                nil t)))))
         (when (y-or-n-p (format "exec ~%s~?" cmd))
           (shell-command cmd))))
     
     (defun qz/bt-connect (mac)
       (interactive "saddress: ")
       (async-shell-command
        (format "bluetoothctl connect %s" mac)
        "*bluetoothctl*"))
     
     
     ;;  [AnnePro2 P4]# block 44:F0:9E:51:52:7B
     ;; [CHG] Device 44:F0:9E:51:52:7B Blocked: yes
     ;; Changing 44:F0:9E:51:52:7B block succeeded
     ;; [CHG] Device 44:F0:9E:51:52:7B Connected: no
     ;; [DEL] Transport /org/bluez/hci0/dev_44_F0_9E_51_52_7B/sep1/fd5
     ;; [DEL] Endpoint /org/bluez/hci0/dev_44_F0_9E_51_52_7B/sep1
     ;; [DEL] Endpoint /org/bluez/hci0/dev_44_F0_9E_51_52_7B/sep2
     ;; [DEL] Endpoint /org/bluez/hci0/dev_44_F0_9E_51_52_7B/sep3
     ;; [AnnePro2 P4]# disconnect 44:F0:9E:51:52:7B
     
     
     (defun qz/bt-do (commands mac)
       (interactive "saddress: ")
       (mapcar (lambda (c) (async-shell-command
                            (format "bluetoothctl %s %s" c mac)
                            "*bluetoothctl*"))
     
               (qz/ensure-list command)))
     
     (defun qz/bt-ignore (mac)
       (interactive "saddress: ")
       (qz/bt-do '(block disconnect) mac))
     
     (defmacro qz/bt-ui (command-devices)
       "A macro as a UI to manage bluetooth actions.
     
     Creates mapped commands against each named device."
       (let ((f-name (lambda (e)
                       (intern (format "qz/bt-%s--%s"
                                       command device))))
     
         (mapcar (lambda ()
     
                   (defun ,TODO ()
                     (qz/bt-, ,actions mac))
     
            (defun ,enable ()
              (interactive)
              (advice-add ',target-fn ,state ',advice-fn))
     
            (defun ,(funcall s-advice 'disable) ()
              (interactive)
              (advice-remove ',target-fn ',advice-fn))
     
            (,enable)
            (list ',enable ',disable))))))
     
     (defvar qz/bluetooth-devices
       '((airpods . "44:F0:9E:51:52:7B")
         (boomzo . nil)
         (aiaia . nil)
         (ap2 . nil)))
     
     
     (defun qz/bt-airpods ()
       (interactive)
       (qz/bt-connect "44:F0:9E:51:52:7B"))
     
     (defun qz/bt-aiaiai ()
       (interactive)
       (qz/bt-connect "44:F0:9E:51:52:7B"))
     
     (defun qz/bt-boomzo ()
       (interactive)
       (qz/bt-connect "44:F0:9E:51:52:7B"))
     
     ;; (setq minibuffer-mode-hook nil)
     ;; (add-hook 'minibuffer-mode-hook 'olivetti-mode)
     
     (add-hook 'minibuffer-mode-hook
               (lambda ()
                 (setq-local olivetti-body-width 200)
                 (olivetti-mode)))
     
     
     (defun qz/big-mode ()
       (interactive)
       (call-interactively 'global-text-scale-adjust)
       (setq olivetti-body-width 150)
     
       )
     
     ;; (require 'perfect-margin)
     
     ;; (perfect-margin-mode 1)
     ;; (setq perfect-margin-ignore-regexps nil
     ;;       perfect-margin-ignore-filters nil)
     (custom-set-variables
      '(cursor-type 'bar))
     (setq outline-default-state 'outline-show-only-headings)
     (defun hi-lock-face-symbol-at-point ()
       "Highlight each instance of the symbol at point.
     Uses the next face from `hi-lock-face-defaults' without prompting,
     unless you use a prefix argument.
     Uses `find-tag-default-as-symbol-regexp' to retrieve the symbol at point.
     
     If REGEXP contains upper case characters (excluding those preceded by `\\')
     and `search-upper-case' is non-nil, the matching is case-sensitive.
     
     This uses Font lock mode if it is enabled; otherwise it uses overlays,
     in which case the highlighting will not update as you type.  The Font
     Lock mode is considered \"enabled\" in a buffer if its `major-mode'
     causes `font-lock-specified-p' to return non-nil, which means
     the major mode specifies support for Font Lock."
       (interactive)
       (let* ((regexp (hi-lock-regexp-okay
                       (find-tag-default-as-symbol-regexp)))
              (hi-lock-auto-select-face t)
              (face (hi-lock-read-face-name)))
         (or (facep face)
             (setq face (or (and hi-lock-auto-select-face (hi-lock-read-face-name))
                            'hi-yellow)))
         (unless hi-lock-mode (hi-lock-mode 1))
         (hi-lock-set-pattern
          regexp face nil nil
          (if (and case-fold-search search-upper-case)
              (isearch-no-upper-case-p regexp t)
            case-fold-search))))
     
     (with-eval-after-load 'highlight-indent-guides
       
       ;; highlight-indent-guides-character
       ;; 🐶
       ;; 🐩
       ;; |
       ;; default (char-to-string 9474)
       ;; default │
       
       (custom-set-variables '(highlight-indent-guides-method 'character)
                             `(highlight-indent-guides-character 9474)
                             '(highlight-indent-guides-auto-enabled nil)
                             '(highlight-indent-guides-responsive 'top))
       
       ;; I don't use this AT ALL so give some ridulous defaults
       (set-face-background 'highlight-indent-guides-odd-face "green")
       (set-face-background 'highlight-indent-guides-even-face "red")
       
       ;; this is the /only/ based highlight-indent guide variety
       (set-face-foreground 'highlight-indent-guides-character-face "#333")
       (let ((accent "#555"))
         (set-face-foreground 'highlight-indent-guides-stack-character-face accent)
         (set-face-foreground 'highlight-indent-guides-top-character-face accent))
       (defun qz/highlight-indent-toggle-responsive (&optional arg)
         (interactive)
         (let ((val (if (or (and arg (> 0 arg))
                            (and (not arg) highlight-indent-guides-responsive))
                        nil qz/highlight-indent-guides-responsive)))
           (custom-set-variables
            `(highlight-indent-guides-responsive ',val))
           (highlight-indent-guides-mode)
           val))
       (defvar qz/lazy-last -1)
       (defun qz/lazy-eyes (&optional arg)
         (interactive)
         (setq qz/lazy-last (if (> 0 qz/lazy-last) 1 -1))
         (qz/highlight-indent-toggle-responsive qz/lazy-last)
         (global-hl-line-mode qz/lazy-last))
       )
     
     (defvar qz/font-initial-size (face-attribute 'default :height))
     (defvar qz/resize-mini-windows-initial resize-mini-windows)
     (defvar qz/max-mini-window-height-initial max-mini-window-height)
     
     (defun qz/reset-visual-initial ()
       (interactive)
       (set-face-attribute 'default nil :height qz/font-initial-size)
       (setq resize-mini-windows    qz/resize-mini-windows-initial
             max-mini-window-height qz/max-mini-window-height-initial))
     (defun qz/font-big-80 ()
       (interactive)
       (set-face-attribute 'default nil :height 300)
       (setq resize-mini-windows t
             max-mini-window-height nil))
     (defvar qz/unsplash-tags nil)
     (defun qz/unsplash ()
       "yet another lazy shell-command wrapper; wallpaper edition"
       (interactive)
       (let ((tag (read-from-minibuffer
                   "unsplash tags: " (car qz/unsplash-tags))))
         (async-shell-command
          (format "TAGS='%s'
     mv \"$XDG_CACHE_HOME/wallpaper.png\" \"$XDG_CACHE_HOME/$(date +%%Y-%%m-%%d--%%H-%%M-%%S)-wallpaper.png\"
     curl -L \"https://source.unsplash.com/5120x1440?$TAGS\" -o \"$XDG_CACHE_HOME/wallpaper.png\"
     swaymsg output \"*\" background ~/.cache/wallpaper.png fill" tag))
         (setq qz/unsplash-tags (seq-uniq (cons tag qz/unsplash-tags)))))
     ;;; json-to-org-table.el --- Converts json string to linked org table -*- lexical-binding: t; -*-
     ;;
     ;; Copyright (C) 2020 Joshua Person
     ;;
     ;; Author: Joshua Person <http://github.com/noonker>
     ;; Maintainer: Joshua Person <ceo@legitimate.company>
     ;; Created: December 06, 2020
     ;; Modified: December 06, 2020
     ;; Version: 0.0.1
     ;; Keywords:
     ;; Homepage: https://github.com/noonker/json-to-org-table
     ;; Package-Requires: ((emacs 27.1))
     ;;
     ;; This file is not part of GNU Emacs.
     ;;
     ;;; Commentary:
     ;;
     ;;  Converts json string to linked org table
     ;;
     ;;; Code:
     ;;; TODO: Better Examples
     
     (defvar j2t-debug nil)
     
     (defvar j2t-cs-map (vector '("\r" "")
                                '("\n" ""))
       "Map of characters to replace in json string.")
     
     (defun j2t-cs (str)
       "Clean String.
     Replace any string org mode wouldn't like according to the j2t-cs-map
     STR: String to be replaced
     RETURNS: A string with problematic characters returned"
       (seq-reduce
        (lambda (s m) (replace-regexp-in-string (car m) (cadr m) s))
        j2t-cs-map (format "%s" str)))
     
     (defun j2t-lf (key &optional ref i)
       "Convert to link Link Format based on available args.
     KEY: String or Symbol that becomes the name of the table
     REF: If there is a Reference that becomes a subkey of the link
     I: Is the Index for links in vectors"
       (cond (i (format "[[%s_%s%s]]" key ref (number-to-string i)))
             (ref (format "[[%s_%s]]" key ref))
             (t (format "[[%s]]" key))))
     
     (defun j2t-hp (key value)
       "Hashmap Print prints a hashmap key-value table row.
     KEY: Hashmap key column
     VALUE: Hashmap value column"
       (format "|%s|%s|\n" (j2t-cs key) (j2t-cs value)))
     
     (defmacro j2t-c+ (&rest str)
       "HACK: Concatenates all args and update the value of cur with new STR.
     There's probably a better way to do this but this keeps things as clean
     as possible in the =tablify= function."
       `(setq cur (concat cur (concat ,@str ))))
     
     (defun j2t-parse-vector-vector (elt)
       "The row parser for a vector of vectors.
     ELT: Is a vector to be turned into a table row
     RETURNS: A table row representing the values of a vector"
       (let ((cur ""))
         (j2t-c+ "|")
         (mapc (lambda (x) (j2t-c+ (j2t-cs (format "%s" x)) "|" )) elt)
         (j2t-c+ "\n")
         cur
         )
       )
     
     (defun j2t-parse-hashmap-vector (elt &optional ref i)
       "A row parser for a vector element composed of hashmaps.
     ELT: A dotted pair cons representing a json hashmap
     REF: Reference if this is a linked table
     I: Optional Index for multiple linked tables
     RETURNS: The table row representing values of a hashmap and a
              list of subtables to create if applicable
     EXAMPLE: ((a . (b . 2)) (c . d) (e . f)) -> '(\"|[[a]]|d|f|]\" '(a (b .2) 1))"
       (let ((cur "")
             (keys (mapcar 'car elt))
             (nex '()))
         (mapcar (lambda (key)
                   (let ((value (alist-get key elt)))
                     (if (consp value)
                         (progn
                           (j2t-c+ (j2t-lf key ref i) "|")
                           (setq nex (append nex '('(key value i)))))
                       (j2t-c+ (j2t-cs value) "|" )))
                   ) keys)
         `(,cur ,nex)
         ))
     
     
     (defun j2t-parse-hash-element (elt &optional ref)
       "A row parser for elements of a hash map.
     ELT: A dotted pair cons representing a json hashmap
     REF: Reference if this is a linked table
     RETURNS: Return an object who's first element is the generated string
              and the second element is the key if a new table is required.
     EXAMPLE: (a . b) -> '(\"|a|b|\n\" '())"
       (let ((key (car elt))
             (val (cdr elt)))
         (cond ((not val) `(,(j2t-hp key "") nil))
               ((vectorp val) `(,(j2t-hp key (j2t-lf key ref)) ,key))
               ((consp val) `(,(j2t-hp key (j2t-lf key ref)) ,key))
               (t `(,(j2t-hp key (format "%s" val)) nil))
               )))
     
     (defun j2t-tablify (elt &optional ref)
       "Function to be called recusrively to build an table.
     ELT: a json object
     REF: a reference is this is a linked table"
       (let ((cur "")
             (nex '()))
         (if j2t-debug (message (format "Got here! I was called with:\n  elt: %s\n  ref: %s\n" elt ref)))
         (if ref (j2t-c+ (format "#+name: %s\n" ref))) ;; If there's a reference add a name block to establish the linkage
     
         (cond
          ;; ----- Element is a hash-map -----
          ((consp elt)
           (progn
             (j2t-c+ "|key|value|\n|-\n") ;; Add headers for hashmap table
             ;; For each element in the hashmap either add the value or add a link to the table of values
             (mapc (lambda (x) (let ((parsed (j2t-parse-hash-element x ref)))
                                 (format "x: %s\nparsed: %s" x parsed)
                                 (j2t-c+ (car parsed))
                                 (if (cadr parsed) (setq nex (append (cdr parsed) nex))))) elt)
             (j2t-c+ "\n")
             ;; Recursively call this function to create any subtables
             (mapc (lambda (x)  (progn  (if j2t-debug (message (format "\nThe symbol I'm going to look up is: %s\n  it's type is: %s\n  and the value is: %s" x (type-of x) (alist-get x elt))))
                                        (if ref
                                            (j2t-c+ (j2t-tablify (alist-get x elt) (format "%s_%s" x ref)))
                                          (j2t-c+ (j2t-tablify (alist-get x elt) (format "%s" x)))))) nex)
             ))
     
          ;; ----- Element is a vector and is a vector of hash-maps -----
          ((and (vectorp elt)
                (consp (aref elt 0)))
           (let ((keys (mapc 'car (aref elt 0)))
                 )
             (j2t-c+ (format "|%s|\n" (string-join (mapcar (lambda (x) (format "%s" (car x))) keys) "|")))
             (j2t-c+ "|-\n")
             (seq-map-indexed
              (lambda (elt idx)
                (let ((parsed (j2t-parse-hashmap-vector elt ref idx)))
                  (j2t-c+ "|")
                  (j2t-c+ (car parsed))
                  (j2t-c+ "\n")
                  (if (cadr parsed) (setq nex (append (cdr parsed) nex))))
                ) elt)
             )
     
           ;; Recursively call this function to create any subtables
           (mapc (lambda (x) (let ((key (nth 0 x))
                                   (value (nth 1 x))
                                   (i (nth 2 x)))
                               (j2t-c+ (j2t-tablify value (format "%s_%s%s" key ref (format "%s" i)) )))) nex)
           )
     
          ;; ----- Element is a vector of vectors -----
          ((and (vectorp elt)
                (vectorp (aref elt 0)))
           (let ((a nil))
             (mapc (lambda (x) (j2t-c+ (j2t-parse-vector-vector x))) elt)
             (j2t-c+ "\n")
             ))
     
          ;; ----- Element is an empty vector -----
          ((and (vectorp elt)
                (= (length elt) 0))
           (j2t-c+ "| |\n")
           )
     
          ;; ----- Element is a vector of strings -----
          ((vectorp elt)
           (j2t-c+ (format "|%s|\n|-\n" ref))
           (mapc (lambda (x) (j2t-c+ "|" (j2t-cs x) "|" "\n")) elt)
           )
          )
         cur
         )
       )
     
     (defun json-to-org-table-parse-json-string (str)
       "Read a json string, parse it, and return a tablified string.
     STR: json string"
       (j2t-tablify (json-read-from-string str)))
     
     (defun json-to-org-table-parse-json (js)
       "Read an Emacs json object, parse it, and return a tablified string.
     The json should be in the format:
      - lists -> vectors
      - hashmaps -> alist cons
      - null -> \"\"
      - bool -> :json-true / :json-false
     JS: json object"
       (j2t-tablify js))
     
     (provide 'json-to-org-table)
     
     ;;; json-to-org-table.el ends here
     ;; NOWEB CONF END (three parens and  terminate the file)
     )))

