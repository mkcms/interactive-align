;;; interactive-align.el --- Interactive align-regexp.

;;
;; Author: Michał Kondraciuk <k.michal@zoho.com>
;; URL: https://github.com/mkcms/interactive-align
;; Package-Requires: ((emacs "24.4") (evil "1.2.0"))
;; Version: 0.0.1
;; Keywords: tools, editing, align, interactive

;;; Commentary:
;;
;; TODO: Write project summary.

(require 'align)
(require 'evil)

(defgroup interactive-align nil
  "Interactive align-regexp."
  :group 'align)

(defcustom ia-minibuffer-keymap
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-c r") #'ia-toggle-repeat)
    (define-key map (kbd "C-c t") #'ia-toggle-tabs)
    (define-key map (kbd "C-c +") #'ia-increment-spacing)
    (define-key map (kbd "C-c -") #'ia-decrement-spacing)
    (define-key map (kbd "C-c [") #'ia-decrement-group)
    (define-key map (kbd "C-c ]") #'ia-increment-group)
    (define-key map (kbd "C-c g") #'ia-set-group)
    (define-key map (kbd "C-c s") #'ia-set-spacing)
    (define-key map (kbd "C-c RET") #'ia-commit)
    (define-key map (kbd "C-c C-c") #'ia-update)
    map)
  "Keymap used in minibuffer during `ia-interactive-align'."
  :group 'interactive-align)

(defcustom ia-default-spacing align-default-spacing
  "An integer that represents the default amount of padding to use."
  :group 'interactive-align
  :type 'integer)

(defcustom ia-align-with-tabs nil
  "A value that says when the region should be aligned with tabs.
If it's nil, never use tabs.
If it's t, always use tabs.
If it's the symbol 'indent-tabs-mode, use value of variable
`indent-tabs-mode.'"
  :group 'interactive-align
  :type '(choice (const :tag "Never use tabs" nil)
		 (const :tag "Always use tabs" t)
		 (const :tag "Use value of variable `indent-tabs-mode'"
			indent-tabs-mode)))

(defcustom ia-auto-update t
  "A value that says when to align the region as the characters are typed.
If it is nil, never update (you can manually update with `ia-update').
If it is t, always update.
If it is an integer, update if the number of lines in the region is less than
or equal to this, otherwise do not update."
  :group 'interactive-align
  :type
  '(choice (const :tag "Never update" nil)
	   (const :tag "Always update" t)
	   (integer :tag "Update if number of lines is less than or equal")))

(defvar ia--buffer nil)
(defvar ia--start nil)
(defvar ia--end nil)
(defvar ia--region-contents nil)
(defvar ia--tabs nil)
(defvar ia--group nil)
(defvar ia--spacing nil)
(defvar ia--repeat nil)
(defvar ia--regexp nil)
(defvar ia--history nil)
(defvar ia--error nil)

(defmacro ia--with-region-narrowed (&rest forms)
  "Evaluate FORMS in `ia--buffer'.
The buffer is narrowed to region that is to be aligned."
  `(with-current-buffer ia--buffer
     (save-excursion
       (save-restriction
	 (widen)
	 (narrow-to-region ia--start ia--end)
	 (unwind-protect
	     (progn
	       ,@forms)
	   (set-marker ia--start (point-min))
	   (set-marker ia--end (point-max)))))))

(defun ia--active-p ()
  "Return non-nil if currently executing `ia-interactive-align'."
  ia--buffer)

(defun ia-toggle-repeat ()
  "Toggle 'repeat' argument passed to `align-regexp'.
When the repeat argument is non-nil, the alignment is repeated throughout
the line.
Does nothing when currently not aligning with `ia-interactive-align'."
  (interactive)
  (when (ia--active-p)
    (setq ia--repeat (not ia--repeat))
    (ia-update)))

(defun ia-toggle-tabs ()
  "Toggle tab usage during alignment.
After executing this command, the region is always aligned with either tabs
or spaces, regardless of value of the variable `ia-align-with-tabs'.
Does nothing when currently not aligning with `ia-interactive-align'."
  (interactive)
  (when (ia--active-p)
    (setq ia--tabs (not ia--tabs))
    (ia-update)))

(defun ia-increment-group ()
  "Increment the parenthesis group argument passed to `align-regexp'.
Use `ia-set-group' to set the group to a specific number.
Does nothing when currently not aligning with `ia-interactive-align'."
  (interactive)
  (ia-set-group (1+ ia--group)))

(defun ia-decrement-group ()
  "Decrement the parenthesis group argument passed to `align-regexp'.
Use `ia-set-group' to set the group to a specific number.
Does nothing when currently not aligning with `ia-interactive-align'."
  (interactive)
  (ia-set-group (1- ia--group)))

(defun ia-set-group (group)
  "Set the parenthesis group argument for the `align-regexp' command to GROUP.
This should be called with a numeric prefix argument that is
the group number to set.
Does nothing when currently not aligning with `ia-interactive-align'."
  (interactive "p")
  (or group (setq group 1))
  (when (ia--active-p)
    (setq ia--group group)
    (ia-update)))

(defun ia-increment-spacing ()
  "Increment the amount of spacing passed to `align-regexp' command.
Use `ia-set-spacing' to set the spacing to specific number.
Does nothing when currently not aligning with `ia-interactive-align'."
  (interactive)
  (when (ia--active-p)
    (setq ia--spacing (1+ ia--spacing))
    (ia-update)))

(defun ia-decrement-spacing ()
  "Decrement the amount of spacing passed to `align-regexp' command.
Use `ia-set-spacing' to set the spacing to specific number.
Does nothing when currently not aligning with `ia-interactive-align'."
  (interactive)
  (when (ia--active-p)
    (setq ia--spacing (1- ia--spacing))
    (ia-update)))

(defun ia-set-spacing (spacing)
  "Set the spacing parameter passed to `align-regexp' command to SPACING.
This should be called with a numeric prefix argument.
Does nothing when currently not aligning with `ia-interactive-align'."
  (interactive "p")
  (or spacing (setq spacing 1))
  (when (ia--active-p)
    (setq ia--spacing spacing)
    (ia-update)))

(defun ia-commit ()
  "Align the region using the current regexp and commit change in the buffer.
The region is aligned using the current regexp only if it's valid.
Next alignments will use the newly aligned region.
Does nothing when currently not aligning with `ia-interactive-align'."
  (interactive)
  (when (ia--active-p)
    (ia--with-region-narrowed
     (ia-update)
     (setq ia--region-contents (buffer-substring (point-min) (point-max)))
     (minibuffer-message "Commited regexp %s" ia--regexp))))

(defun ia--make-marker (location)
  "Make marker at LOCATION."
  (let ((marker (make-marker)))
    (set-marker marker location)
    (set-marker-insertion-type marker t)
    marker))

(defun ia--revert ()
  (ia--with-region-narrowed
   (delete-region ia--start ia--end)
   (insert ia--region-contents)))

(defun ia--enable-tabs-p ()
  (unless (ia--active-p)
    (error "Called outside `ia-interactive-align'"))
  (if (eq ia--tabs 'indent-tabs-mode)
      (with-current-buffer ia--buffer
	indent-tabs-mode)
    ia--tabs))

(defun ia--autoupdate-p ()
  (if (integerp ia-auto-update)
      (ia--with-region-narrowed
       (<= (- (line-number-at-pos (point-max))
	      (line-number-at-pos (point-min)))
	  ia-auto-update))
    ia-auto-update))

(defun ia--update-minibuffer-prompt ()
  (let ((inhibit-read-only t)
	(prompt (format "Align regexp %s(group %s%s, spacing %s%s, %s): "
			(if (ia--autoupdate-p) "" "(manual) ") ia--group
			(if (< ia--group 0) " (justify)" "") ia--spacing
			(if ia--repeat ", repeat" "")
			(if (ia--enable-tabs-p) "with tabs" "no tabs"))))
    (put-text-property (point-min) (minibuffer-prompt-end) 'display prompt)))

(defun ia--minibuffer-setup-hook ()
  (and (ia--active-p) (ia-update)))
(add-hook 'minibuffer-setup-hook #'ia--minibuffer-setup-hook)

(defun ia--align ()
  (ia--revert)
  (ia--with-region-narrowed
   (align-regexp (point-min) (point-max) ia--regexp
		 ia--group ia--spacing ia--repeat)))

(defun ia-update ()
  "Align the region with regexp in the minibuffer for preview.
Does temporary alignment for preview only.
Use `ia-commit' to actually align the region in the buffer."
  (interactive)
  (when (and (ia--active-p) (minibufferp))
    (ia--update-minibuffer-prompt)
    (when (or (called-interactively-p 'interactive) (ia--autoupdate-p))
      (let ((regexp (minibuffer-contents-no-properties))
	    (indent-tabs-mode (ia--enable-tabs-p)))
	(setq ia--regexp regexp)
	(ia--align)
	(redisplay)))))

(defun ia--after-change (beg end len)
  (when (and (ia--active-p) (minibufferp))
    (condition-case err
	(progn
	  (ia-update)
	  (setq ia--error nil))
      (error
       (progn
	 (setq ia--error err)
	 (run-with-timer
	  0.05 nil
	  (lambda ()
	    (when ia--error
	      (minibuffer-message (error-message-string ia--error))))))))))

;;;###autoload
(defun ia-interactive-align (from to)
  (interactive "r")
  (if (ia--active-p)
      (error "Already aligning")
    (let ((ia--buffer (current-buffer))
	  (ia--start (ia--make-marker from))
	  (ia--end (ia--make-marker to))
	  (ia--region-contents (buffer-substring from to))
	  (ia--repeat nil)
	  (ia--group 1)
	  (ia--spacing ia-default-spacing)
	  (ia--tabs ia-align-with-tabs)
	  (ia--regexp nil))
      (unwind-protect
	  (progn
	    (add-hook 'after-change-functions #'ia--after-change)
	    (evil-with-single-undo
	      (atomic-change-group
		(read-from-minibuffer " " "\\(\\s-+\\)" ia-minibuffer-keymap
				      nil 'ia--history))))
	(set-marker ia--start nil)
	(set-marker ia--end nil)))))

(provide 'interactive-align)

;;; interactive-align.el ends here
