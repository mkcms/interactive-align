;;; interactive-align.el --- Interactive align-regexp.

;;
;; Author: Michał Kondraciuk <k.michal@zoho.com>
;; URL: https://github.com/mkcms/interactive-align
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.0.1
;; Keywords: tools, editing, align, interactive

;;; Commentary:
;;
;; TODO: Write project summary.

(require 'align)

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
    map)
  "Keymap used in minibuffer during `ia-interactive-align'."
  :group 'interactive-align)

(defcustom ia-default-spacing align-default-spacing
  "An integer that represents the default amount of padding to use."
  :group 'interactive-align
  :type 'integer)

(defcustom ia-align-with-tabs nil
  "TODO: Document ia-align-with-tabs."
  :group 'interactive-align
  :type '(choice (const :tag "Never use tabs" nil)
		 (const :tag "Always use tabs" t)
		 (const :tag "Use value of variable `indent-tabs-mode'"
			indent-tabs-mode)))

(defvar ia--buffer nil)
(defvar ia--start nil)
(defvar ia--end nil)
(defvar ia--region-contents nil)
(defvar ia--tabs nil)
(defvar ia--group nil)
(defvar ia--spacing nil)
(defvar ia--repeat nil)
(defvar ia--regexp nil)

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
  "Return non-nil if currently doing interactive align."
  ia--buffer)

(defun ia-toggle-repeat ()
  "Toggle 'repeat' argument passed to `align-regexp'.
When the repeat argument is t, the alignment is repeated throughout
the line."
  (interactive)
  (when (ia--active-p)
    (setq ia--repeat (not ia--repeat))
    (ia--update)))

(defun ia-toggle-tabs ()
  "Toggle tab usage during alignment."
  (interactive)
  (when (ia--active-p)
    (setq ia--tabs (not ia--tabs))
    (ia--update)))

(defun ia-increment-group ()
  "Increment the parenthesis group to modify.
Use `ia-set-group' to set the group to a specific number."
  (interactive)
  (ia-set-group (1+ ia--group)))

(defun ia-decrement-group ()
  "Decrement the parenthesis group to modify.
Use `ia-set-group' to set the group to a specific number."
  (interactive)
  (ia-set-group (1- ia--group)))

(defun ia-set-group (group)
  "Set the parenthesis group to modify to GROUP.
This should be called with a numeric prefix argument that is
the group number to set."
  (interactive "p")
  (or group (setq group 1))
  (when (ia--active-p)
    (setq ia--group group)
    (ia--update)))

(defun ia-increment-spacing ()
  "Increment the amount of spacing.
Use `ia-set-spacing' to set the spacing to specific number."
  (interactive)
  (when (ia--active-p)
    (setq ia--spacing (1+ ia--spacing))
    (ia--update)))

(defun ia-decrement-spacing ()
  "Decrement the amount of spacing.
Use `ia-set-spacing' to set the spacing to specific number."
  (interactive)
  (when (ia--active-p)
    (setq ia--spacing (1- ia--spacing))
    (ia--update)))

(defun ia-set-spacing (spacing)
  "Set the spacing to SPACING.
This should be called with a numeric prefix argument."
  (interactive "p")
  (or spacing (setq spacing 1))
  (when (ia--active-p)
    (setq ia--spacing spacing)
    (ia--update)))

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

(defun ia--update-minibuffer-prompt ()
  (let ((inhibit-read-only t)
	(prompt (format "Align regexp (group %s%s, spacing %s%s, %s): "
			ia--group (if (< ia--group 0) " (justify)" "")
			ia--spacing (if ia--repeat ", repeat" "")
			(if (ia--enable-tabs-p) "with tabs" "no tabs"))))
    (put-text-property (point-min) (minibuffer-prompt-end) 'display prompt)))

(defun ia--minibuffer-setup-hook ()
  (and (ia--active-p) (ia--update)))
(add-hook 'minibuffer-setup-hook #'ia--minibuffer-setup-hook)

(defun ia--align ()
  (ia--revert)
  (ia--with-region-narrowed
   (align-regexp (point-min) (point-max) ia--regexp
		 ia--group ia--spacing ia--repeat)))

(defun ia--update ()
  (when (and (ia--active-p) (minibufferp))
    (ia--update-minibuffer-prompt)
    (let ((regexp (minibuffer-contents-no-properties))
	  (indent-tabs-mode (ia--enable-tabs-p)))
      (setq ia--regexp regexp)
      (ia--align)
      (redisplay))))

(defun ia--after-change (beg end len)
  (when (and (ia--active-p) (minibufferp))
    (condition-case err
	(ia--update)
      (error (minibuffer-message (error-message-string err))))))

;;;###autoload
(defun ia-interactive-align (from to)
  (interactive (list (region-beginning) (region-end)))
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
	  (ia--regexp nil)
	  success)
      (unwind-protect
	  (progn
	    (add-hook 'after-change-functions #'ia--after-change)
	    (read-from-minibuffer " " "\\(\\s-+\\)" ia-minibuffer-keymap)
	    (setq success t))
	(unless success
	  (ia--revert))
	(set-marker ia--start nil)
	(set-marker ia--end nil)))))

(provide 'interactive-align)

;;; interactive-align.el ends here
