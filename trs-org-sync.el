(defun trs/org-sync ()
  "Change other entries with the same SYNCID as the current entry such that they match the current entry."
  (interactive)
  (let ((changed-buffer (current-buffer))
	(current-id (org-entry-get (point) "SYNCID")))
    (save-excursion
      (trs/goto-entry-beginning)
      (setq org-changed-subtree (org-get-subtree))
      (if current-id
;  This is legacy code.  Don't use unless you know what you are doing.
;	    (trs/org-sync-search-and-replace-using-agenda current-id)
	  (trs/org-sync-search-and-replace current-id changed-buffer org-changed-subtree))
      (switch-to-buffer changed-buffer))))

(defun trs/org-sync-search-and-replace (current-id changed-buffer org-changed-subtree)
  (let ((org-search-files (org-agenda-files nil 'ifmode))
	(org-search-file nil)
	(org-search-file-buffer nil)
	(updated-entry-indent-level nil))
    (while (setq org-search-file (pop org-search-files))
      (setq org-search-file-buffer (get-file-buffer org-search-file))
      (unless (eq changed-buffer org-search-file-buffer) 
	(switch-to-buffer org-search-file-buffer)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward current-id nil t)
	    (read-from-minibuffer "Hit enter is this an entry?")
	    (setq updated-entry-indent-level (org-current-level))
	    (trs/goto-entry-beginning)
	    (trs/org-sync-update-entry org-changed-subtree)
	    (trs/org-sync-correct-updated-entry-indentation updated-entry-indent-level)
	    (read-from-minibuffer "replaced entry")
	    (org-end-of-subtree)))))))

(defun trs/org-sync-correct-updated-entry-indentation (updated-entry-indent-level)
  "Correct the indentation level of the new item so that it matched the level of the item before it was updated."
  (while (not (eq (org-current-level) updated-entry-indent-level))
    (if (< updated-entry-indent-level (org-current-level))
	(org-promote-subtree)
      (org-demote-subtree))))

(defun trs/org-sync-update-entry (org-changed-subtree)
  "Update the entry by replacing it with the new entry in the kill-ring.  TODO:  Use a register for this."
  (let ((entry-beginning (point)))
    (org-end-of-subtree)
    (read-from-minibuffer "Hit enter end of subtree")
    (delete-region entry-beginning (point))
    (read-from-minibuffer "Hit enter before yank")
					;    (yank)
    (insert org-changed-subtree)
    (message "before mark")
    (goto-char entry-beginning)
    (message "after mark")))

(defun trs/goto-entry-beginning ()
  "Place the cursor at the begniing of the entry in column 0"
  (interactive)
  (if (not (looking-at "^*"))
      (search-backward-regexp "^*")))

(defun trs/org-syncid-get-create ()
  (interactive)
  (trs/org-syncid-get (point) 'create))

(defun trs/org-syncid-get (&optional pom create prefix)
  "Get the SYNCID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.
If the entry does not have an SYNCID, the function returns nil.
However, when CREATE is non nil, create an ID if none is present already.
PREFIX will be passed through to `org-id-new'.
In any case, the SYNCID of the entry is returned.
Note that this code was basically lifted from org-id.el with only minor modifications."
  (org-with-point-at pom
    (let ((id (org-entry-get nil "SYNCID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
	id)
       (create
	(setq id (org-id-new prefix))
	(org-entry-put pom "SYNCID" id)
	(org-id-add-location id (buffer-file-name (buffer-base-buffer)))
	id)))))

(defun org-get-subtree (&optional n)
  "Get and return the current subtree into a register.
With arg N, get this many sequential subtrees."
;  (interactive "p")
  (let (beg end folded (beg0 (point)))
    ;; (if (called-interactively-p 'any)
    ;; 	(org-back-to-heading nil) ; take what looks like a subtree
      (org-back-to-heading t)
;      ) ; take what is really there
    (setq beg (point))
    (skip-chars-forward " \t\r\n")
    (save-match-data
	(save-excursion (outline-end-of-heading)
			(setq folded (org-invisible-p)))
	(ignore-errors (org-forward-heading-same-level (1- n) t))
	(org-end-of-subtree t t))
    ;; Include the end of an inlinetask
    (when (and (featurep 'org-inlinetask)
	       (looking-at-p (concat (org-inlinetask-outline-regexp)
				     "END[ \t]*$")))
      (end-of-line))
    (setq end (point))
    (goto-char beg0)
    (when (> end beg)
      (buffer-substring-no-properties beg end))))

(defun trs/org-sync-switch-back-to-agenda (&rest r)
  "Simple function to switch back to agenda when trs/prg-sync functions are called from there"
  (switch-to-buffer org-agenda-buffer))

(defun trs/org-sync-property-state-change (&rest r)
  "Run trs/org-sync.  Used with org-trigger-hook to run trs/org-sync with any property state change."
  (trs/org-sync))

(defun trs/org-sync-agenda-property-state-change (&rest r)
  "Run trs/org-sync.  Used with org-trigger-hook to run trs/org-sync with any property state change."
  (org-agenda-goto)
  (trs/org-sync)
  (switch-to-buffer org-agenda-buffer)
  (move-beginning-of-line nil))


;(add-hook 'org-after-todo-state-change-hook 'trs/org-sync)
(add-hook 'org-after-tags-change-hook 'trs/org-sync)
;(add-hook 'org-trigger-hook 'trs/org-sync-property-state-change)
(add-hook 'org-property-changed-functions 'trs/org-sync-property-state-change)

(advice-add 'org-agenda-todo :after #'trs/org-sync-switch-back-to-agenda)
(advice-add 'org-agenda-priority-up :after #'trs/org-sync-agenda-property-state-change)
(advice-add 'org-agenda-priority-down :after #'trs/org-sync-agenda-property-state-change)
(advice-add 'org-agenda-priority :after #'trs/org-sync-agenda-property-state-change)
(advice-add 'org-agenda-set-tags :after #'trs/org-sync-agenda-property-state-change)
(advice-add 'org-agenda-add-note :after #'trs/org-sync-agenda-property-state-change)
(advice-add 'org-attach :after #'trs/org-sync-agenda-property-state-change)
(advice-add 'org-agenda-schedule :after #'trs/org-sync-agenda-property-state-change)


;; Everything bleow this point is legacy code
(defun trs/org-sync-search-and-replace-using-agenda (current-id)
  "Generate an agenda view with a list of the entries that have the 'current-id' SYNCID.  Iterate through them and change them to match the current (altered) entry."
  (let ((agenda-iteration 1)
	(matched-agenda-buffer nil)
	(indent-level nil)
	(original-agenda-buffer (get-buffer "*Org Agenda*"))
	(matched-agenda-buffer nil))
					;  (setq agenda-iteration 1)
    (if original-agenda-buffer
	(progn
	  (switch-to-buffer original-agenda-buffer)
	  (rename-uniquely)))
    (org-tags-view nil (concat "SYNCID=\"" current-id "\""))
    (setq matched-agenda-buffer (buffer-name))
    (while (not (eq (point) (point-max)))
      (trs/org-sync-goto-agenda-entry agenda-iteration)
      (setq indent-level (org-current-level))
      (trs/org-sync-update-entry)
      (trs/org-sync-correct-updated-entry-indentation indent-level)
      (message "replaced entry")
      (trs/org-sync-position-at-beginning-of-next-agenda-line matched-agenda-buffer agenda-iteration)
      (setq agenda-iteration (+ agenda-iteration 1)))
    (kill-buffer matched-agenda-buffer)
    (if original-agenda-buffer
	(progn
	  (switch-to-buffer original-agenda-buffer)
	  (rename-buffer "*Org Agenda*")))))

(defun trs/org-sync-position-at-beginning-of-next-agenda-line (matched-agenda-buffer agenda-iteration)
  "Position cursor at the beginning of the next entry line.  This will be the end of the file if the last entry has been processed."
  (switch-to-buffer matched-agenda-buffer)
  (beginning-of-buffer)
  (org-agenda-next-item agenda-iteration)
  (org-agenda-next-line))

(defun trs/org-sync-goto-agenda-entry (agenda-iteration)
  "Goto the entry represented by the next item in the agenda buffer."
  ;Have to use this hack with the 'agenda-iteration' iterator because for some reason
  ;the cursor jumps around when switching to and from the agenda buffer.
  (beginning-of-buffer)
  (org-agenda-next-item agenda-iteration)
  (org-agenda-goto)
  (trs/goto-entry-beginning))
