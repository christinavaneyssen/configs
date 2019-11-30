;; -*- mode: elisp -*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(server-start)

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

(setq user-full-name "Christina van Eyssen")
(setq user-mail-address "christinavaneyssen@gmail.com")

;; Highlight matching parent
(show-paren-mode 1)

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

(setq use-package-always-ensure t)

(use-package monokai-theme)

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

;;(use-package python
;;  :config
;;  (setq
;;   python-shell-interpreter "python3"
;;   python-shell-completion-native-enable nil)
;;  (use-package blacken))

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
  :ensure t
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
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c C-x C-j" . org-clock-goto))
  :config

  (defun org-insert-inactive-time-stamp ()
    "Insert an inactive time stamp."
    (interactive)
    (org-insert-time-stamp (current-time) t t))
  (define-key org-mode-map (kbd "C-c .") 'org-insert-inactive-time-stamp)

  (use-package org-journal
    :config
    (setq
     org-journal-dir "~/org-files/journal/"
     org-journal-file-format "%Y%m%d.org")))

  (require 'org-protocol)
  (require 'org-inlinetask)

  (setq org-todo-keywords
         (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                 (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (add-to-list 'org-modules 'org-habit)
  (setq
   org-agenda-files '("~/org-files")
   org-refile-targets '((("~/org-files/work.org" "~/org-files/todo.org") :maxlevel . 1))
   org-deadline-warning-days 14
   org-default-notes-file "~/org-files/todo.org"
   org-reverse-note-order t
   org-fast-tag-selection-single-key 'expert
   org-use-fast-todo-selection t
   org-export-backends '(ascii html icalendar md)
   org-agenda-span 'day
   org-enforce-todo-dependencies t
   org-log-done 'time
   org-log-redeadline 'time
   org-log-reschedule 'time
   org-capture-templates
   '(("c" "Task" entry (file "~/org-files/inbox.org")
      "* TODO %?\n  %U")
     ("a" "Appointment" entry (file "~/org-files/gcal.org")
      "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
     ("s" "Safari" entry (file "~/org-files/inbox.org")
      "* TODO %(my-safari-link)\n%U")
     ("o" "P0 ops work scheduled and clocked in now" entry (file+headline "~/org-files/work.org" "Ops")
      "* P0 Ops :urgent:ops:\n  %t\n  %u" :clock-in t :clock-keep t :empty-lines 1)
     ("m" "Meeting now" entry (file+olp+datetree "~/org-files/meetings.org")
      "* %? :meeting:\n  %T" :clock-in t :clock-keep t :jump-to-captured t :empty-lines 1 :tree-type week)
     ("j" "Journal" entry (file+olp+datetree "~/org-files/journal.org")
      "* %?\n")
     ("p" "" entry (file "~/org-files/inbox.org")
      "* TODO %:description\n%U\n%:link\n\n#+BEGIN_QUOTE\n%:initial\n#+END_QUOTE" :immediate-finish t :jump-to-captured t)
     ("L" "" entry (file "~/org-files/inbox.org")
      "* TODO %:description\n%U\n%:link" :immediate-finish t :jump-to-captured t)))

;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; GCal Set Up
(setq package-check-signature nil)

(use-package org-gcal
    :ensure t
    :config
    (setq org-gcal-client-id ""
    org-gcal-client-secret ""))

(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))

(setq org-agenda-custom-commands
'(("c" "Simple agenda view"
((agenda "")
(alltodo "")))))

(use-package calfw
:ensure ;TODO:
:config
(require 'calfw)
(require 'calfw-org)
(setq cfw:org-overwrite-default-keybinding t)
(require 'calfw-ical))

(defun mycalendar ()
(interactive)
(cfw:open-calendar-buffer
:contents-sources
(list
;; (cfw:org-create-source "Green")  ; orgmode source
(cfw:ical-create-source "gcal" "" "IndianRed") ; gCal - Mine
(cfw:ical-create-source "gcal" "" "IndianRed") ; gcal - Work
(cfw:ical-create-source "gcal" "" "IndianRed") ; gcal - Xander
)))
(setq cfw:org-overwrite-default-keybinding t)

(use-package calfw-gcal
:ensure t
:config
(require 'calfw-gcal))

;; The above is the default in recent emacsen
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242728" "#ff0066" "#63de5d" "#E6DB74" "#06d8ff" "#ff8eff" "#53f2dc" "#f8fbfc"])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("e6d29075a0e4b95de11d2c7b61b1214ef9cb6901157b7e0e0bd8704d502c8c3f" default)))
 '(fci-rule-color "#323342")
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-tail-colors
   (quote
    (("#323342" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#323342" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages (quote (org-journal monokai-theme use-package ox-twbs)))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff0066")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#63de5d")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#53f2dc")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#06d8ff"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#242728" "#323342" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
