(setq org-capture-templates
      '(

        ;; TODO Add these:
        ;; Movies
        ;; Reading list

        ("t" "Todo" entry (file "~/org-files/refile.org")
         "* TODO %?
  :PROPERTIES:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :CLOCK:
  :END:" :clock-in t :clock-resume t :empty-lines 1)

        ("r" "Respond" entry (file "~/git/org/refile.org")
         "* NEXT Respond to %? on \nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)

        ("w" "Waiting" entry (file "~/org-files/refile.org")
         "* WAITING %?
  :PROPERTIES:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :END:" :empty-lines 1)

        ("m" "Meeting" entry (file "~/org-files/refile.org")
         "* MEETING %^{description}
  :PROPERTIES:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :END:
  %^T--%^T
  Notes:
  %?" :clock-in t :clock-keep t :jump-to-captured t :empty-lines 1 :tree-type week)

        ("a" "Appointment" entry (file  "~/org-files/gcal.org")
         "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:" :empty-lines 1)

        ("j" "Journal" entry (function org-journal-find-location)
         "* %(format-time-string org-journal-time-format)\n%i%?")

        ("n" "Note" entry (file "~/org-files/refile.org")
         "* %? :note:
  :PROPERTIES:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :END:" :empty-lines 1)

        ("g" "Gift" entry (file "~/org-files/refile.org")
         "* GIFT %?
  :PROPERTIES:
  :Link:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :END:" :empty-lines 1)

        ("v" "Visit/Outings" entry (file "~/org-files/refile.org")
         "* VISIT %?
  :PROPERTIES:
  :Location:
  :Type:
  :Note:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :END:
  %^t--%^t" :empty-lines 1)

        ("c" "Code Project" entry (file "~/org-files/refile.org")
         "* IDEA %?
  :PROPERTIES:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :END:" :empty-lines 1)

	("w" "Web site" entry
	  (file "")
	  "* %a :website:\n\n%U %?\n\n%:initial")
        ))

(setq
    org-journal-file-type 'monthly
    org-journal-enable-encryption t
    org-journal-encrypt-journal t
    org-journal-enable-agenda-integration t
    org-journal-new-schedule-entry t
)
(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

;; https://github.com/bastibe/org-journal#kill-journal-buffer-after-saving-buffer-by-dhruvparamhans
(defun org-journal-save-entry-and-exit()
  "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))
(define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit)


(defun my/org-capture-during-meeting (task)
  "Capture todo task with or without deadline, populate task :Via: field with meeting task,
and then insert a link in line of the new todo task."
  (interactive "sTask: ")
  (call-interactively 'org-store-link)
  (save-excursion
    (org-insert-heading-respect-content)
    (org-return)
    (org-capture 0)
    (org-previous-visible-heading 1)
    (org-cut-subtree)
    (org-do-demote)
    (org-end-of-line)
    (insert task)
    (let ((parent-task
           ;; ;; This implementation prompts due to the use of 'org-insert-last-stored-link.
           ;; (replace-regexp-in-string "\n" ""
           ;;                           (with-temp-buffer
           ;;                             (org-mode)
           ;;                             (org-insert-last-stored-link 1)
           ;;                             (buffer-string)))))
           ;; ;; This implementation requires 'set-window-buffer due to 'execute-kbd-macro.
           ;; ;; To prevent 'y-or-no-p dialog box, set use-dialog-box to nil.
           (with-temp-buffer
             (save-window-excursion
               (set-window-buffer nil (current-buffer))
               (org-mode)
               (execute-kbd-macro [?\C-c ?\C-l return return]))
             (buffer-string))))
      (org-set-property "Via" parent-task))
    (call-interactively 'org-store-link)
    (if (y-or-n-p "Set deadline?")
        (call-interactively 'org-deadline))
    (if (y-or-n-p "Set scheduled?")
        (call-interactively 'org-schedule))
    (org-cycle))
  ;; (org-insert-last-stored-link 1)
  (execute-kbd-macro [?\C-c ?\C-l return return]))
  ;;(org-delete-backward-char 1))

(define-key org-mode-map "\C-cm" 'my/org-capture-during-meeting)
