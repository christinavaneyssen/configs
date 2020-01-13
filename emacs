;; -*- mode: elisp -*-

;; Ensure all packages in package.el are initialized before
;; attempting to edit them
(require 'package)


;; Activate all packages (in particular autoloads)
(package-initialize)

;; Proxies
;;(load "~/.emacs.d/proxies")

(add-to-list 'load-path "~/.emacs.d/local/")
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Package repos
;; GnuELPA package archive is available by default in Emacs 24+
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))


;; Retrieve package list already available from
;; package.el initialized by the first 2 commands
(unless package-archive-contents
  (package-refresh-contents))


;; Required packages
(setq package-list '(use-package smartparens request zenburn))


;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; Disable automatic package loading at start up
(setq package-enable-at-startup nil)


;; Paradox github stars
(setq paradox-github-token "257bc04184e4f02a375aa3f91cb430b7ea9154b1")

(setq custom-file (concat user-emacs-directory "/custom.el"))


;; Ensure packages are install automatically if not already present
;; When set to t, there is no need to specify :ensure t
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 'epa-file)
(epa-file-enable)

(use-package general)

;;(unless (package-installed-p 'use-package)
;;  (package-refresh-contents)
;;  (package-install 'use-package))


(eval-when-compile
  (require 'use-package))


;; Start Emacs server that listens for external edit requests
;; Allows Emacsclient and org-protol to run
(server-start)

(use-package virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/.envs/")


;; Use UTF-8
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)


;; Disable the splash screen
;; To enable, replace t with 0
(setq inhibit-splash-screen t)


;; Enable transient mark mode
(transient-mark-mode 1)

;; Personalised
(setq user-full-name "Christina van Eyssen")
(setq user-mail-address "christinavaneyssen@gmail.com")


;; Highlight matching parent
(show-paren-mode 1)


;; https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org
;; Delete/copy region if selected else del/copy line
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))


;; Backward line delete
(bind-key "C-<backspace>" (lambda ()
                            (interactive)
                            (kill-line 0)
                            (indent-according-to-mode)))


;; Sentences end with 2 spaces by default
;; This code changes that to 1 space only
(defadvice forward-sentence (around real-forward)
  "Consider a sentence to have one space at the end."
  (let ((sentence-end-double-space nil))
    ad-do-it))

(defadvice backward-sentence (around real-backward)
  "Consider a sentence to have one space at the end."
  (let ((sentence-end-double-space nil))
    ad-do-it))

(defadvice kill-sentence (around real-kill)
  "Consider a sentence to have one space at the end."
  (let ((sentence-end-double-space nil))
    ad-do-it))

(ad-activate 'forward-sentence)
(ad-activate 'backward-sentence)
(ad-activate 'kill-sentence)


;; Better paragraph selection key bindings
(bind-keys ("M-A" . backward-paragraph)
           ("M-E" . forward-paragraph)
           ("M-K" . kill-paragraph))


;; Platform specific
(defun is-mac-p
    ()
  (eq system-type 'darwin))


(if (is-mac-p) (setq osx t)
  (setq osx nil))


(when (is-mac-p)
  (set-face-attribute 'default nil :height 165))


;; Choose the correct shell
(use-package shell
  :bind ("<f1>" . shell)
  :init
  (dirtrack-mode)
  (setq explicit-shell-file-name (cond ((eq system-type 'darwin) "/bin/bash")
                                       ((eq system-type 'gnu/linux) "/usr/bin/bash")))
  (when (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :ensure t
      :init
      (exec-path-from-shell-initialize)))
  :config
  (bind-keys :map shell-mode-map
             ("<s-up>" . comint-previous-input)
             ("<s-down>" . comint-next-input)))


(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


;; Group related commands together
(use-package hydra
  :ensure t
  :init
  (defhydra hydra-zoom ()
    "zoom"
    ("+" text-scale-increase "in")
    ("=" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("_" text-scale-decrease "out")
    ("0" (text-scale-adjust 0) "reset")
    ("q" nil "quit" :color blue))
  (bind-keys ("C-x C-0" . hydra-zoom/body)
             ("C-x C-=" . hydra-zoom/body)
             ("C-x C--" . hydra-zoom/body)
             ("C-x C-+" . hydra-zoom/body))
  (setq hydra-lv nil))


;; Emacs window management
(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t))


;; Undo/Redo changes in the window configuration with
;; C-c left or C-c right
(use-package winner
  :init (winner-mode))


;; Quit bottom / side windows
(defun quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))

(bind-key "C-c q" #'quit-bottom-side-windows)


;; Deletion
(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :init
  (global-hungry-delete-mode))


(use-package easy-kill
  :ensure t
  :bind ("M-w" . easy-kill))


;; History of what's been deleted
(use-package browse-kill-ring
  :ensure t
  :bind ("C-x C-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-quit-action 'kill-and-delete-window))


;; Highlight where the cursor is
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init
  (beacon-mode 1)
  (setq beacon-push-mark 35)
  (setq beacon-color "#666600"))


;; Work through your undo history without changing
;; anything; which you can then jump to
(use-package goto-chg
  :ensure t
  :bind (("C-c ," . goto-last-change)
         ("C-c ." . goto-last-change-reverse)))


;; Search specific websites
;; TODO Add Jira in here
(use-package engine-mode
  :ensure t
  :disabled t
  :init
  (engine-mode t)
  (setq engine/browser-function 'eww-browse-url)
  (engine/set-keymap-prefix (kbd "C-c e"))
  :config
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")

  (defengine duckduckgo
    "https://duckduckgo.com/html/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "i")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m"
    :docstring "Mappin' it up.")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y"))


;; Make RESTful API calls from a text file
(use-package restclient
  :ensure t)


;; Syntax checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (use-package flycheck-clojure
    :ensure t)
  (global-flycheck-mode)
  (setq flycheck-indication-mode 'right-fringe)
  :config
  (flycheck-clojure-setup))


;; Git & Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-status))
  :init
  (use-package git-timemachine
    :ensure t
    :bind (("C-x v t" . git-timemachine)))
  (use-package git-link
    :ensure t
    :bind (("C-x v L" . git-link))
    :init
    (setq git-link-open-in-browser t))
  :config
  (setq magit-use-overlays nil
        magit-completing-read-function 'ivy-completing-read
        magit-push-always-verify nil)
  (diminish 'magit-backup-mode)

  (defun visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-remote)
                         "url"))
             (cdr (magit-get-remote-branch)))))

  (bind-key "v" 'visit-pull-request-url magit-mode-map)

  (bind-keys :map magit-status-mode-map
             ("TAB" . magit-section-toggle)
             ("<C-tab>" . magit-section-cycle))
  (bind-keys :map magit-branch-section-map
             ("RET" . magit-checkout)))


(use-package reveal-in-osx-finder
  :if osx
  :ensure t)


;; Manage parenthesis
(use-package smartparens
  :diminish smartparens-mode
  :bind
  (("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-a" . sp-backward-down-sexp)
   ("C-S-a" . sp-beginning-of-sexp)
   ("C-S-d" . sp-end-of-sexp)
   ("C-M-e" . sp-up-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-t" . sp-transpose-sexp)
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-<delete>" . sp-unwrap-sexp)
   ("M-S-<backspace>" . sp-backward-unwrap-sexp)
   ("C-<right>" . sp-forward-slurp-sexp)
   ("C-<left>" . sp-forward-barf-sexp)
   ("C-M-<left>" . sp-backward-slurp-sexp)
   ("C-M-<right>" . sp-backward-barf-sexp)
   ("M-D" . sp-splice-sexp)
   ("C-M-<delete>" . sp-splice-sexp-killing-forward)
   ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
   ("C-M-S-<backspace>" . sp-splice-sexp-killing-around)
   ("C-]" . sp-select-next-thing-exchange)
   ("C-<left_bracket>" . sp-select-previous-thing)
   ("C-M-]" . sp-select-next-thing)
   ("M-F" . sp-forward-symbol)
   ("M-B" . sp-backward-symbol)
   ("H-t" . sp-prefix-tag-object)
   ("H-p" . sp-prefix-pair-object)
   ("H-s c" . sp-convolute-sexp)
   ("H-s a" . sp-absorb-sexp)
   ("H-s e" . sp-emit-sexp)
   ("H-s p" . sp-add-to-previous-sexp)
   ("H-s n" . sp-add-to-next-sexp)
   ("H-s j" . sp-join-sexp)
   ("H-s s" . sp-split-sexp)
   ("M-9" . sp-backward-sexp)
   ("M-0" . sp-forward-sexp))
  :init
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  ;;(use-package smartparens-config)
  ;;(bind-key "s" 'smartparens-mode toggle-map)
  (when (eq system-type 'darwin)
    (bind-keys ("<s-right>" . sp-forward-slurp-sexp)
               ("<s-left>" . sp-forward-barf-sexp)))
  (sp-with-modes '(markdown-mode gfm-mode)
    (sp-local-pair "*" "*"))
  (sp-with-modes '(org-mode)
    (sp-local-pair "=" "=")
    (sp-local-pair "*" "*")
    (sp-local-pair "/" "/")
    (sp-local-pair "_" "_")
    (sp-local-pair "+" "+")
    (sp-local-pair "<" ">")
    (sp-local-pair "[" "]"))
  (use-package rainbow-delimiters
    :ensure t
    :init
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))


;; Project interaction library providing
;; features on the project level
(use-package projectile
  :ensure t
  :bind ("M-p" . projectile-find-file)
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy)
  (use-package ibuffer-projectile
    :ensure t
    :bind ("C-x C-b" . ibuffer)
    :init
    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))
    (bind-keys :map ibuffer-mode-map
               ("c" . clean-buffer-list)
               ("n" . ibuffer-forward-filter-group)
               ("p" . ibuffer-backward-filter-group))))


;; Indentation
(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :init
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (unbind-key "C-c C-q" aggressive-indent-mode-map))


;; Line numbers
(use-package linum-relative
  :ensure t
  :init
  (setq linum-format 'linum-relative)
  :config
  (setq linum-relative-current-symbol ""))


;; Code commenting
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))



;;;;;;


;; Themes
(add-hook 'after-init-hook (lambda () (load-theme 'zenburn t)))
;;(load-theme 'zenburn t)
;; (use-package monokai-theme)


(setq save-interprogram-paste-before-kill t)

;; Misc settings
(setq
 vc-handled-backends '(Git Hg)
 inhibit-startup-message t
 select-enable-clipboard t
 make-backup-files nil
 column-number-mode t
 case-fold-search t
 current-language-environment "English"
 confirm-nonexistent-file-or-buffer nil
 compilation-window-height 10
 compilation-scroll-output t
 dabbrev-case-fold-search t
 save-abbrevs nil
 font-lock-maximum-decoration t
 vc-follow-symlinks t
 display-time-world-list
 '(("Africa/Johannesburg" "Cape Town")
   ("America/Los_Angeles" "San Francisco")
   ("Europe/London" "London")
   ("Europe/Berlin" "Berlin")))

;; Misc buffer settings
(setq-default
 fill-column 78
 tab-width 4
 indent-tabs-mode nil)

;; Get rid of stupid GUI stuff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Yes-or-No queries become Y-or-N
(fset 'yes-or-no-p 'y-or-n-p)

(display-time-mode 1)


(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(use-package python
  :config
  (setq
   python-shell-interpreter "python3"
   python-shell-completion-native-enable nil)
  (use-package blacken))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package git-timemachine)

(use-package dockerfile-mode)

(use-package nim-mode)

(use-package guide-key
  :config
  (setq guide-key/guide-key-sequence t)
  (guide-key-mode 1))

;; For visual wrapping at 80 columns when editing markdown.
(use-package visual-fill-column)

(setq-default word-wrap t)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (defun my-gfm-mode-hook ()
    (visual-line-mode)
    (visual-fill-column-mode)
    (setq word-wrap t))
  (add-hook 'gfm-mode-hook #'my-gfm-mode-hook))

;;;;Org mode configuration
(use-package org
  :bind (("C-c d" . org-decrypt-entry)
         ("C-c C-x C-j" . org-clock-goto))
  :requires org-crypt
)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)


(setq org-tags-exclude-from-inheritance '("crypt"))
(setq org-crypt-key "769BFE40DA64FC9578757A1A9FFD8DB48CF9DF9F")
(setq auto-save-default nil)

(setq view-diary-entries-initially t
   mark-diary-entries-in-calendar t
   number-of-diary-entries 7)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

;;(Use-package org-crypt
;;  :init (org-crypt-use-before-save-magic)
;;  :custom
;;  (org-tags-exclude-from-inheritance '("crypt"))
;;  (org-crypt-key "769BFE40DA64FC9578757A1A9FFD8DB48CF9DF9F")
;;    ;; GPG key to use for encryption
;;    ;; Either the Key ID or set to nil to use symmetric encryption.
;;  (auto-save-default nil))


(org-babel-do-load-languages
    'org-babel-load-languages
     '((python . t)))

(defun org-insert-inactive-time-stamp ()
"Insert an inactive time stamp."
(interactive)
(org-insert-time-stamp (current-time) t t))
(define-key org-mode-map (kbd "C-c .") 'org-insert-inactive-time-stamp)

(use-package org-journal
:config
(setq
 org-journal-dir "~/org-files/journal/"
 org-journal-file-format "%Y%m%d.org"))

(require 'org-protocol)
(require 'org-inlinetask)
(require 'org-tempo)

(setq org-todo-keywords
     (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
             (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(add-to-list 'org-modules 'org-habit)
(add-to-list 'org-global-properties
           '("Effort_ALL". "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
           '("STYLE_ALL". "habit"))
(setq org-modules
    '(org-habit org-w3m org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail)
    org-habit-graph-column 105)

  (setq
   org-agenda-tags-column -100 ;; screen width
   org-agenda-sticky nil
   org-agenda-show-log t
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-include-diary t
   ;; https://www.reddit.com/r/orgmode/comments/e4stk2/adding_a_separator_line_between_days_in_emacs/
   org-agenda-format-date (lambda (date) (concat "\n"
                                                    (make-string (window-width) 9472)
                                                    "\n"
                                                    (org-agenda-format-date-aligned date)))
   org-agenda-time-grid
        '((daily today require-timed)
        (08:00 10:00 12:00 14:00 16:00 18:00 20:00)
        "...." "----------------")
   org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS"
   org-agenda-files '("~/org-files")
   org-refile-targets '((("~/org-files/work.org" "~/org-files/personal.org" "~/org-files/todo.org") :maxlevel . 1))
   org-deadline-warning-days 14
   org-default-notes-file "~/org-files/todo.org"
   org-reverse-note-order t
   org-fast-tag-selection-single-key 'expert
   org-use-fast-todo-selection t
   org-export-backends '(ascii html icalendar md)
   org-agenda-span 'week
   org-enforce-todo-dependencies t
   org-log-done 'time
   ;;org-log-redeadline 'time
   ;;org-log-reschedule 'time
   org-use-property-inheritance t
)

;; TODO -> DONE once all subtasks completed
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(load "~/.emacs.d/org-mode-config/org-capture-templates")

;; https://github.com/alphapapa/org-sidebar
(use-package org-sidebar
  :general
  (alpha-org/general-def
   "vs" #'org-sidebar-toggle
   "vt" #'org-sidebar-tree-toggle)
  :custom (org-sidebar-tree-side 'left))

;; https://github.com/alphapapa/org-web-tools
(use-package org-web-tools
  :general
  (alpha-org/general-def
    "ilu" #'org-web-tools-link-for-url))

(require 'org-protocol-capture-html)
(require 's)

(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode))

;; https://github.com/alphapapa/org-recent-headings
(use-package org-recent-headings
  :general
  (alpha-org/general-def
    "hr" #'org-recent-headings-helm)
  :config
  (org-recent-headings-mode)
  :custom
  (org-recent-headings-reverse-paths t)
  (org-recent-headings-candidate-number-limit 100))

(use-package helm-org
  :general
  (alpha-org/general-def
    "ha" #'helm-org-agenda-files-headings
    "hb" #'helm-org-in-buffer-headings
    "hp" #'helm-org-parent-headings)
  :custom
  (helm-org-format-outline-path t))


;; https://github.com/alphapapa/org-now
(require 'org-now)

;; https://github.com/alphapapa/org-clock-convenience
(use-package org-clock-convenience
  :bind (:map org-agenda-mode-map
        ("<S-up>" . org-clock-convenience-timestamp-up)
        ("<S-down>" . org-clock-convenience-timestamp-down)
        ("ö" . org-clock-convenience-fill-gap)
        ("é" . org-clock-convenience-fill-gap-both)))

;; https://github.com/alphapapa/org-recur
(use-package org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :demand t
  :config
  (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)

  ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
  (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
  (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)

  (setq org-recur-finish-done t
        org-recur-finish-archive t))

;; Refresh org-agenda after rescheduling a task.
(defun org-agenda-refresh ()
  "Refresh all `org-agenda' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-maybe-redo)))))

(defadvice org-schedule (after refresh-agenda activate)
  "Refresh org-agenda."
  (org-agenda-refresh))

;; Log time a task was set to Done.
(setq org-log-done (quote time))

;; Don't log the time a task was rescheduled or redeadlined.
(setq org-log-redeadline nil)
(setq org-log-reschedule nil)

(setq org-read-date-prefer-future 'time)


;; Elfeed
(global-set-key (kbd "C-x w") 'elfeed)


(use-package elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
              ("A" . my/elfeed-show-all)
              ("E" . my/elfeed-show-emacs)
              ("D" . my/elfeed-show-daily)
              ("q" . my/elfeed-save-db-and-bury)))


;; Managing RSS feeds
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org-files/elfeed.org")))


(defun my/elfeed-mark-all-as-read ()
      (interactive)
      (mark-whole-buffer)
      (elfeed-search-untag-all-unread))

(define-key elfeed-search-mode-map (kbd "R") 'my/elfeed-mark-all-as-read)


;; Elfeed shortcut functions
(defun my/elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))
(defun my/elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))
(defun my/elfeed-show-daily ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-daily"))


;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun my/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun my/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))



;; Log the time when a TODO item was finished
(setq org-log-done 'time)

;; Specify global tags with fast tag selection
(setq org-tag-alist '((:startgroup . nil) ("work" . ?w) ("personal" . ?p) (:endgroup . nil)
                      ("crypt" . ?c) ("reading" . ?r) ("research" . ?R)))

(add-hook 'org-agenda-finalize-hook (lambda () (delete-other-windows)))
(use-package org-bullets
    :ensure t
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
 :config
  (org-load-modules-maybe t)
(eval-after-load 'org-indent '(diminish 'org-indent-mode))


(defun org-buffer-todo ()
  (interactive)
  "Creates a todo-list for the current buffer. Equivalent to the sequence: org-agenda, < (restrict to current buffer), t (todo-list)."
  (progn
    (org-agenda-set-restriction-lock 'file)
    (org-todo-list)))


(defun org-buffer-agenda ()
  (interactive)
  "Creates an agenda for the current buffer. Equivalent to the sequence: org-agenda, < (restrict to current buffer), a (agenda-list)."
  (progn
    (org-agenda-set-restriction-lock 'file)
    (org-agenda-list)))

    (defun org-buffer-day-agenda ()
      (interactive)
      "Creates an agenda for the current buffer. Equivalent to the sequence: org-agenda, < (restrict to current buffer), a (agenda-list), d (org-agenda-day-view)."
      (progn
        (org-agenda-set-restriction-lock 'file)
        (org-agenda-list)
        (org-agenda-day-view)))

    (bind-key "y" 'org-agenda-todo-yesterday org-agenda-mode-map)

    (add-hook 'org-mode-hook
	      (lambda ()
		(push '("TODO"  . ?▲) prettify-symbols-alist)
		(push '("DONE"  . ?✓) prettify-symbols-alist)
		(push '("CANCELLED"  . ?✘) prettify-symbols-alist)
		(push '("QUESTION"  . ??) prettify-symbols-alist)))

;; Add time estimate on the fly when clocking in
;; FIXME: Requests effort for meetings 
;;    (add-hook 'org-clock-in-prepare-hook
;;	      'my-org-mode-ask-effort)
;;
;;    (defun my-org-mode-ask-effort ()
;;      "Ask for an effort estimate when clocking in."
;;      (unless (org-entry-get (point) "Effort")
;;	(let ((effort
;;	       (completing-read
;;		"Effort: "
;;		(org-entry-get-multivalued-property (point) "Effort"))))
;;	  (unless (equal effort "")
;;	    (org-set-property "Effort" effort)))))


;; Refresh agenda automatically every 5 mins
    (defun kiwon/org-agenda-redo-in-other-window ()
      "Call org-agenda-redo function even in the non-agenda buffer."
      (interactive)
      (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
	(when agenda-window
	  (with-selected-window agenda-window (org-agenda-redo)))))
    (run-at-time nil 300 'kiwon/org-agenda-redo-in-other-window)

;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


;; Open agenda in full screen
(defun open-agenda ()
  "Opens the org-agenda."
  (interactive)
  (let ((agenda "*Org Agenda*"))
    (if (equal (get-buffer agenda) nil)
        (org-agenda-list)
      (unless (equal (buffer-name (current-buffer)) agenda)
        (switch-to-buffer agenda))
      (org-agenda-redo t)
      (beginning-of-buffer))))

(bind-key "<f5>" 'open-agenda)
;;(bind-key "a" 'open-agenda launcher-map)

;; Kill this buffer
(defun kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(bind-key "C-x C-k" 'kill-this-buffer)


;; Kill all other buffers
(defun kill-other-buffers ()
   "Kill all other buffers."
   (interactive)
   (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))


;; Jump back to the nearest heading so that speed
;; commands can be used
(defun org-go-speed ()
  "Goes to the beginning of an element's header, so that you can execute speed commands."
  (interactive)
  (when (equal major-mode 'org-mode)
    (if (org-at-heading-p)
        (org-beginning-of-line)
      (org-up-element))))

(bind-key "C-c s" 'org-go-speed)


(setq org-agenda-custom-commands
'(("c" "Simple agenda view"
((agenda "")
(alltodo "")))))


;; Keep in Touch
(setq keepintouch-datafile "~/org-files/keepintouch.data")

(defun keptintouch (arg)
  "Request a contact in a keepintouch.data file, and update their last
  contacted date (either today, or, if a prefix is supplied, a user-supplied date.)"
  (interactive "P")
  (let ((contact (read-string "Who did you contact? "))
        (date (if (equal arg nil)
                  (format-time-string "%Y/%m/%d")
                (read-string "When did you contact them? (year/month/date): "))))
    (save-excursion
      (find-file keepintouch-datafile)
      (goto-char (point-min))
      (search-forward contact)
      (forward-line -1)
      (beginning-of-line)
      (kill-line)
      (insert date)
      (save-buffer)
      (switch-to-buffer (other-buffer))
      (kill-buffer (other-buffer)))
    (message "%s was contacted." contact)))

(defun keptintouch-backlog ()
  "Create a buffer with Keep In Touch backlog."
  (interactive)
  (let ((buf "*Keep In Touch Backlog*")
        (src "~/src/keepintouch/clj/keepintouch")
        (jar "-jar target/uberjar/keepintouch-0.1.0-SNAPSHOT-standalone.jar")
        (cur default-directory)) 
    (cd src)
    (shell-command
     (concat "java " jar " " keepintouch-datafile " schedule backlog") buf)
    (cd cur)
    (switch-to-buffer buf)))

(bind-keys ("C-c k" . keptintouch)
           ("C-c K" . keptintouch-backlog))
