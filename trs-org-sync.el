(require 'org)

(defcustom trs/org-sync-with-property-change t
  "If non-nil sync current item automatically with a change in the following properties:  TODO state, priority and tags"
  :group 'org-syncid
  :type 'file)

(defcustom trs/org-sync-with-interactive-save t
  "If non-nil sync current item when the file is saved interactively."
  :group 'org-syncid
  :type 'file)


(defcustom org-syncid-locations-file (convert-standard-filename
				  (concat user-emacs-directory ".org-sync-syncids"))
  "The file for remembering in which file a SYNCID was defined."
  :group 'org-syncid
  :type 'file)


(defun trs/org-sync ()
  "Change other entries with the same SYNCID as the current entry such that they match the current entry."
  (interactive)
  (let ((changed-buffer (current-buffer))
	(current-id (org-entry-get (point) "SYNCID" t))
	(id (org-entry-get (point) "ID" t)))
    (if current-id
	(save-window-excursion
	  (save-excursion
	  (org-id-goto id)
	  (setq org-changed-subtree (trs/org-get-subtree))
	  (if (and current-id id)
	      (trs/org-sync-search-and-replace current-id id changed-buffer org-changed-subtree)
	    (if (called-interactively-p)
		(message "This item is not set up to sync.  Please run 'trs/org-sync-syncid-get-create' then copy the entry to other places"))))))))

(defun trs/org-sync-get-sync-locations-from-file ()
  "Get the list of id-syncid pairs from 'org-syncid-locations-file' and return them"
  (with-temp-buffer
    (insert-file-contents org-syncid-locations-file)
    (read (current-buffer))))

(defun trs/org-sync-search-and-replace (current-id id changed-buffer org-changed-subtree)
  "Go to the locations where entries with the 'current-id' syncid property and update them by replacing them with the 'org-changed-subtree'"
  (let ((org-search-file nil)
	(org-search-file-buffer nil)
	(updated-entry-indent-level nil)
	(following-subtrees nil)
	(org-sync-locations (trs/org-sync-get-sync-locations-from-file))
	(beginning-of-entry nil))
      (dolist (pair org-sync-locations)
	(if (and (equal current-id (cdr pair)) (org-id-find (car pair)))
	    (progn
	      (org-id-goto (car pair))
	      (setq org-search-file-buffer (make-indirect-buffer (get-file-buffer (buffer-name)) "delete-me.org"))
	      (unwind-protect
		  (progn
		    (switch-to-buffer org-search-file-buffer)
		    (org-mode)
		    (outline-show-all)
		    (org-show-subtree)
		    (org-narrow-to-subtree)
		    (setq beginning-of-entry (point))
		    (setq updated-entry-indent-level (org-current-level))
		    (trs/org-sync-update-entry org-changed-subtree)
		    (setq following-subtrees (buffer-substring-no-properties (point) (point-max)))
		    (trs/org-sync-correct-updated-entry-indentation updated-entry-indent-level)
		    (goto-char beginning-of-entry)
		    (org-entry-put (point) "ID" (car pair))
		    (org-end-of-subtree)
		    (delete-blank-lines)
		    (widen))
		(kill-buffer org-search-file-buffer)))))))

(defun trs/org-sync-correct-updated-entry-indentation (updated-entry-indent-level)
  "Correct the indentation level of the new item so that it matched the level of the item before it was updated."
  (goto-char (point-min))
  (while (not (eq (org-current-level) updated-entry-indent-level))
    (if (< updated-entry-indent-level (org-current-level))
	(org-promote-subtree)
      (org-demote-subtree)))
  (widen)
  )

(defun trs/org-sync-update-entry (org-changed-subtree)
  "Update the entry by replacing it with 'org-changed-sybtree'."
  (org-mark-subtree)
  (delete-region (point-min) (point-max))
  (insert org-changed-subtree))

(defun trs/org-sync-rebuild-database ()
  "Rebuild the 'org-syncid-locations-file'."
  (interactive)
  (let ((org-search-files (org-agenda-files nil 'ifmode))
	(org-search-file nil)
	(org-search-file-buffer nil)
	(org-sync-locations '(" " " ")))
    (save-window-excursion
      (trs/org-sync-write-syncid-locations-to-file org-syncid-locations-file org-sync-locations)
      (while (setq org-search-file (pop org-search-files))
	(setq org-search-file-buffer (make-indirect-buffer (get-file-buffer org-search-file) "delete-me.org"))
	(unwind-protect
	    (progn
	      (switch-to-buffer org-search-file-buffer)
	      (org-mode)
	      (outline-show-all)
	      (goto-char (point-min))
	      (while (re-search-forward "SYNCID:" nil t)
		(trs/org-sync-syncid-get-create))
	      )
	  (kill-buffer org-search-file-buffer))
	  )
	(let ((org-sync-locations (trs/org-sync-get-sync-locations-from-file)))
	  (pop org-sync-locations)
	  (pop org-sync-locations)
	  (trs/org-sync-write-syncid-locations-to-file org-syncid-locations-file org-sync-locations)))))

(defun trs/org-sync-write-syncid-locations-to-file (org-syncid-locations-file org-sync-locations)
  "Write 'org-sync-locations' to 'org-syncid-locations-file'"
  (with-temp-file org-syncid-locations-file
	  (print org-sync-locations (current-buffer))))

(defun trs/goto-entry-beginning ()
  "Place the cursor at the beginning of the entry in column 0"
  (interactive)
  (if (not (looking-at "^*"))
      (search-backward-regexp "^*")))

(defun trs/org-sync-syncid-get-create ()
  "Add the (id . syncid) pair to the 'org-syncid-locations' list.  Save 'org-sync-locations' to 'org-sync-locations-file'.  If no syncid and/or id exists, create them.'"
  (interactive)
  (let ((syncid (trs/org-sync-syncid-get (point) 'create))
	(id (org-id-get (point) 'create))
	(org-sync-locations (trs/org-sync-get-sync-locations-from-file)))
    (if (not (member (cons id syncid) org-sync-locations))
	(progn
	  (message "3")
	  (push (cons id syncid) (cdr (last org-sync-locations)))
	  (message "4")
	  (trs/org-sync-write-syncid-locations-to-file org-syncid-locations-file org-sync-locations)))))

(defun trs/org-sync-syncid-get (&optional pom create prefix)
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

(defun trs/org-get-subtree (&optional n)
  "Get and return the current subtree into a register.
With arg N, get this many sequential subtrees."
					;  (interactive "p")
  (let (beg end folded (beg0 (point)))
    (org-back-to-heading t)
    (setq beg (point))
    (skip-chars-forward " \t\r\n")
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (org-invisible-p)))
      (ignore-errors (org-forward-heading-same-level (1- n) t))
      (org-end-of-subtree t t))
    (when (and (featurep 'org-inlinetask)
	       (looking-at-p (concat (org-inlinetask-outline-regexp)
				     "END[ \t]*$")))
      (end-of-line))
    (setq end (point))
    (goto-char beg0)
    (when (> end beg)
      (buffer-substring-no-properties beg end))))

(defun trs/org-sync-file ()
  "Sync all of the SYNCID's in a file"
  (interactive)
  (save-excursion
    (trs/org-sync)
    (goto-char (point-min))
    (while (re-search-forward "SYNCID" nil t)
      (trs/org-sync)
      (org-end-of-subtree))))

;; Hooks and advice for limited automation

(defun trs/org-sync-create-new-id ()
  "Creates a new ID if the entry has a SYNCID but not ID.  Add the (id . syncid) pair to 'org-sync-locations-file' if necessary."
  (interactive)
  (if (trs/org-sync-syncid-get (point))
      (progn
	(org-id-get-create 'force)
	(trs/org-sync-syncid-get-create))))

(advice-add 'org-copy :after #'trs/org-sync-create-new-id)

(defun trs/after-manual-save-actions ()
  "Used in `after-save-hook'.  Run this only when the save function has been called manually by the user (i.e. not automatically in the background).."
  (when (and trs/org-sync-with-interactive-save (eq major-mode 'org-mode) (memq this-command '(save-buffer write-file)))
    (trs/org-sync)))

(add-hook 'after-save-hook 'trs/after-manual-save-actions)

(defun trs/org-sync-property-state-change (&rest r)
  "Run trs/org-sync.  Used with org-trigger-hook to run trs/org-sync with any property state change."
  (if trs/org-sync-with-property-change
      (trs/org-sync)))

(add-hook 'org-trigger-hook 'trs/org-sync-property-state-change)
(advice-add 'org-priority-up :after #'trs/org-sync-property-state-change)
(advice-add 'org-priority-down :after #'trs/org-sync-property-state-change)
(advice-add 'org-priority :after #'trs/org-sync-property-state-change)
(advice-add 'org-set-tags-command :after #'trs/org-sync-property-state-change)


(defun trs/org-sync-legacy ()
  "Change other entries with the same SYNCID as the current entry such that they match the current entry."
;  (interactive)
  (let ((changed-buffer (current-buffer))
	(current-id (org-entry-get (point) "SYNCID" t)))
    (setq org-changed-subtree (trs/org-get-subtree))
    (if current-id
	(let ((id (org-id-get (point) 'create)))
	  (trs/org-sync-search-and-replace-legacy current-id id changed-buffer org-changed-subtree))
      (if (called-interactively-p)
	  (message "This item is not set up to sync.  Please run 'trs/org-sync-syncid-get-create' then copy the entry to other places")))
    ))

(defun trs/org-sync-search-and-replace-legacy (current-id id changed-buffer org-changed-subtree)
  (let ((org-search-files (org-agenda-files nil 'ifmode))
	(org-search-file nil)
	(org-search-file-buffer nil)
	(updated-entry-indent-level nil))
    (while (setq org-search-file (pop org-search-files))
      (setq org-search-file-buffer (make-indirect-buffer (get-file-buffer org-search-file) "delete-me.org"))
      (unless (eq changed-buffer org-search-file-buffer) 
	(switch-to-buffer org-search-file-buffer)
	(org-mode)
	(outline-show-all)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward current-id nil t)
	    (org-show-subtree)
	    (trs/org-sync-syncid-get-create)
	    (setq id (org-id-get (point)))
	    (org-narrow-to-subtree)
	    (setq updated-entry-indent-level (org-current-level))
	    (trs/org-sync-update-entry org-changed-subtree)
	    (org-entry-put (point) "ID" id)
	    (let ((following-subtrees (buffer-substring-no-properties (point) (point-max))))
	      (trs/org-sync-correct-updated-entry-indentation updated-entry-indent-level)
	      (org-end-of-subtree)
	      (delete-blank-lines)
	      (widen)
	      )))
	(kill-buffer org-search-file-buffer)))))
