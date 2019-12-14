(setq org-capture-templates
      '(

        ;; TODO Add these:
        ;; Movies
        ;; Reading list

        ("t" "Todo" entry (file "~/org-files/todo.org")
         "* TODO %?
  :PROPERTIES:
  :Location:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :CLOCK:
  :END:" :empty-lines 1)

        ("w" "Waiting" entry (file "~/org-files/todo.org")
         "* WAITING %?
  :PROPERTIES:
  :Location:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :END:" :empty-lines 1)

        ("m" "Meeting" entry (file "~/org-files/inbox.org")
         "* MEETING %^{description}
  :PROPERTIES:
  :Attendees:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :END:
  %^T--%^T
  Notes:
  %?" :clock-in t :clock-keep t :jump-to-captured t :empty-lines 1 :tree-type week)

        ("a" "Appointment" entry (file "~/org-files/gcal.org")
         "* %?
  :PROPERTIES:
  :Note:
  :END:
  %^T--%^T" :empty-lines 1)

        ("j" "Journal" entry (file+olp+datetree "~/org-files/journal.org")
         "* Journal :org:
  :PROPERTIES:
  :Note:
  :END:
  :CREATED: %U
  %t\n\n  %?" :empty-lines 1)

        ("n" "Note" entry (file "~/org-files/notes.org")
         "* %? :note:
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :END:" :empty-lines 1)

        ;;("o" "Org-Protocol")
        ;; TODO     (t) Org-protocol todo template
        ;; Alternatively use [[%:link][%:description]] for :Via:
        ;;("tp" "TODO      (p) Org-Protocol Todo" entry (file "~/org-files/inbox.org")
        ;;        "* TODO %?
        ;;  :PROPERTIES:
        ;;  :Via:      %:annotation
        ;;  :Note:
        ;;  :END:
        ;;  :LOGBOOK:
        ;;  :CREATED: %U
        ;;  :END:" :empty-lines 1)

        ("g" "Gift" entry (file "~/org-files/gifts.org")
         "* GIFT %?
  :PROPERTIES:
  :Link:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :END:" :empty-lines 1)

        ("v" "Visit/Outings" entry (file "~/org-files/outings.org")
         "* VISIT %?
  :PROPERTIES:
  :Location:
  :Type:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :END:
  %^t--%^t" :empty-lines 1)

        ("c" "Code Project" entry (file "~/org-files/tech-notebook.org")
         "* IDEA %?
  :PROPERTIES:
  :Via:
  :Note:
  :END:
  :LOGBOOK:
  :CREATED: %U
  :END:" :empty-lines 1)

;;        ("m" "REFERENCE (f) Reference org-protocol" entry (file "~/org-files/inbox.org")
;;         "* REFERENCE [[%:link][%:description]]
;;  :PROPERTIES:
;;  :Creator:  %:creator
;;  :Created:  %:description
;;  :Source:   %:source
;;  :Via:      %:via
;;  :Link:     %:link
;;  :Date:     %:date
;;  :Note:     %:note
;;  :END:
;;  :LOGBOOK:
;;  :CREATED: %U
;;  :END:
;;  %:initial" :empty-lines 1)

        ("w" "Web site" entry
         (file "~/org-files/inbox.org")
         "* %a :website:\n\n%U %?\n\n%:initial")

        ))

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
