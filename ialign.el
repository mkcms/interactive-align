;;; ialign.el --- visual align-regexp  -*- lexical-binding: t; -*-

;;
;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; URL: https://github.com/mkcms/interactive-align
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.4.2
;; Keywords: tools, editing, align, interactive

;; Copyright (C) 2017-2024 Michał Krzywkowski

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides command `ialign'
;; which can be used to visually align a region
;; using a regexp read from minibuffer, like `align-regexp'.
;;
;; See documentation for command `ialign'.
;;

;;; Code:

(require 'align)
(require 'pcre2el nil 'noerror)

(defgroup ialign nil
  "Interactive `align-regexp'."
  :group 'align)

(defcustom ialign-minibuffer-keymap
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-c C-r") #'ialign-toggle-repeat)
    (define-key map (kbd "C-c C-t") #'ialign-toggle-tabs)
    (define-key map (kbd "C-c M-c") #'ialign-toggle-case-fold)
    (define-key map (kbd "C-c C-p") #'ialign-toggle-pcre-mode)
    (define-key map (kbd "C-c +") #'ialign-increment-spacing)
    (define-key map (kbd "C-c -") #'ialign-decrement-spacing)
    (define-key map (kbd "C-c [") #'ialign-decrement-group)
    (define-key map (kbd "C-c ]") #'ialign-increment-group)
    (define-key map (kbd "C-c C-f") #'ialign-set-group)
    (define-key map (kbd "C-c C-s") #'ialign-set-spacing)
    (define-key map (kbd "C-c RET") #'ialign-commit)
    (define-key map (kbd "C-c C-c") #'ialign-update)
    (define-key map (kbd "C-c ?") #'ialign-show-help)
    map)
  "Keymap used in minibuffer during `ialign'."
  :group 'ialign
  :type '(restricted-sexp :match-alternatives (keymapp)))

(defvaralias 'ialign-initial-spacing 'ialign-default-spacing)

(defcustom ialign-default-spacing align-default-spacing
  "An integer that represents the default amount of padding to use."
  :group 'ialign
  :type 'integer)

(defcustom ialign-align-with-tabs nil
  "A value that says when the region should be aligned with tabs.
If it's nil, never use tabs.
If it's t, always use tabs.
If it's the symbol \\='indent-tabs-mode, use value of variable
`indent-tabs-mode'."
  :group 'ialign
  :type '(choice (const :tag "Never use tabs" nil)
		 (const :tag "Always use tabs" t)
		 (const :tag "Use value of variable `indent-tabs-mode'"
			indent-tabs-mode)))

(defcustom ialign-auto-update t
  "A value that says when to align the region as the characters are typed.
If it is nil, never update (you can manually update with `ialign-update').
If it is t, always update.
If it is an integer, update if the number of lines in the region is less than
or equal to this, otherwise do not update."
  :group 'ialign
  :type
  '(choice (const :tag "Never update" nil)
	   (const :tag "Always update" t)
	   (integer :tag "Update if number of lines is less than or equal")))

(defcustom ialign-initial-regexp "\\(\\s-+\\)"
  "Initial regexp to use when calling `ialign'."
  :group 'ialign
  :type 'regexp)

(defcustom ialign-initial-group 1
  "Initial group to use when calling `ialign'."
  :group 'ialign
  :type 'integer)

(defcustom ialign-initial-repeat nil
  "Initial value of repeat argument when calling `ialign'."
  :group 'ialign
  :type 'boolean)

(defcustom ialign-implicit-regexp nil
  "String to prepend to the regexp, if the regexp doesn't have a subgroup.

`align-regexp' expects the align regexp to contain a
parenthesized subexpression whose characters are replaced.
However, it adds such an expression automatically to the regexp
if necessary.  This option allows you to specify the string to
implicitly prepend to the string in case there's no group
subexpression."
  :group 'ialign
  :type '(choice (const :tag "Don't add implicit group" nil)
                 (const :tag "Whitespace" "\\(\\s-*\\)")
                 string))

(defcustom ialign-pcre-mode nil
  "Treat input as PCRE regexp.
This requires the `pcre2el' library.  You can still toggle PCRE
mode during alignment with `ialign-toggle-pcre-mode'."
  :group 'ialign
  :type 'boolean)

(defvar ialign--buffer nil)
(defvar ialign--beg nil)
(defvar ialign--end nil)
(defvar ialign--region-contents nil)
(defvar ialign--tabs nil)
(defvar ialign--group nil)
(defvar ialign--spacing nil)
(defvar ialign--repeat nil)
(defvar ialign--regexp nil)
(defvar ialign--pcre-mode nil)
(defvar ialign--history nil)
(defvar ialign--error nil)
(defvar ialign--case-fold-search nil)
(defvar ialign--minibuffer-overlay nil)
(defvar ialign--recursive-minibuffer nil)

(defmacro ialign--with-region-narrowed (&rest forms)
  "Evaluate FORMS in `ialign--buffer'.
The buffer is narrowed to region that is to be aligned."
  (declare (debug (&rest form)))
  `(with-current-buffer ialign--buffer
     (save-excursion
       (save-restriction
	 (widen)
	 (narrow-to-region ialign--beg ialign--end)
	 (unwind-protect
	     (progn
	       ,@forms)
	   (setq ialign--beg (point-min)
		 ialign--end (point-max)))))))

(defun ialign--active-p ()
  "Return non-nil if currently executing `ialign'."
  ialign--buffer)

(defun ialign--read-number (prompt)
  "Read number with PROMPT in a new minibuffer."
  (let ((enable-recursive-minibuffers t)
	(ialign--recursive-minibuffer t))
    (read-number prompt)))

(defun ialign--revert ()
  "Revert the aligned region to original `ialign--region-contents'."
  (ialign--with-region-narrowed
   (delete-region ialign--beg ialign--end)
   (insert ialign--region-contents)))

(defun ialign--enable-tabs-p ()
  "Return non-nil if tabs should be used for aligning current region."
  (unless (ialign--active-p)
    (error "Called outside `ialign'"))
  (if (eq ialign--tabs 'indent-tabs-mode)
      (with-current-buffer ialign--buffer
	indent-tabs-mode)
    ialign--tabs))

(defun ialign--autoupdate-p ()
  "Return non-nil if the region should be aligned as characters are typed."
  (if (integerp ialign-auto-update)
      (ialign--with-region-narrowed
       (<= (- (line-number-at-pos (point-max))
	      (line-number-at-pos (point-min)))
	   ialign-auto-update))
    ialign-auto-update))

(defun ialign--update-minibuffer-prompt ()
  "Update the minibuffer prompt to show arguments passed to `align-regexp'."
  (let ((inhibit-read-only t)
	(prompt
	 (format "Align regexp %s%s(group %s%s, spacing %s%s%s, %s): "
		 (if ialign--pcre-mode "[PCRE] " "")
                 (if (ialign--autoupdate-p) "" "(manual) ") ialign--group
		 (if (< ialign--group 0) " (justify)" "") ialign--spacing
		 (if ialign--repeat ", repeat" "")
		 (if (ialign--enable-tabs-p) ", with tabs" "")
		 (substitute-command-keys
		  "\\<ialign-minibuffer-keymap>\\[ialign-show-help]: \
help"))))
    (put-text-property (point-min) (minibuffer-prompt-end) 'display prompt)
    (when (overlayp ialign--minibuffer-overlay)
      (delete-overlay ialign--minibuffer-overlay)
      (setq ialign--minibuffer-overlay nil))
    (when ialign--error
      (let ((msg (concat " [" ialign--error "]")))
	(setq ialign--minibuffer-overlay
	      (make-overlay (point-max) (point-max) nil t t))
	(put-text-property 0 1 'cursor t msg)
	(overlay-put ialign--minibuffer-overlay 'after-string msg)))))

(defun ialign--align ()
  "Revert the current region, then align it."
  (ialign--revert)
  (let ((reg ialign--regexp))
    (when (and ialign--pcre-mode (fboundp 'rxt-pcre-to-elisp))
      (setq reg (rxt-pcre-to-elisp reg)))
    (when (and (null (string-match-p (regexp-quote "\\(") reg))
               (stringp ialign-implicit-regexp)
               (= 1 ialign--group))
      (setq reg (concat ialign-implicit-regexp reg))
      (setq ialign--error "Using implicit regexp")
      (when (minibufferp)
        (ialign--update-minibuffer-prompt)))
    (ialign--with-region-narrowed
     (let ((case-fold-search ialign--case-fold-search)
           (indent-tabs-mode (ialign--enable-tabs-p)))
       (align-regexp (point-min) (point-max) reg
                     ialign--group ialign--spacing ialign--repeat)))))

(defun ialign--undo (beg end orig)
  "Delete region BEG END and insert ORIG at BEG.
This function is used to undo changes made by command `ialign'."
  (save-excursion
    (save-restriction
      (widen)
      (undo-boundary)
      (delete-region beg end)
      (goto-char beg)
      (insert orig)
      (undo-boundary))))

(defun ialign--restore-arguments ()
  "Restore global variables stored in properties of minibuffer contents."
  (let ((props
	 (plist-get (text-properties-at 0 (minibuffer-contents)) 'ialign)))
    (when props
      (when minibuffer-text-before-history
	(let ((orig-props
	       (plist-get
		(text-properties-at 0 minibuffer-text-before-history)
		'ialign)))
	  (unless orig-props
	    (put-text-property
	     0 (min 1 (length minibuffer-text-before-history))
	     'ialign
	     (list ialign--group ialign--spacing ialign--repeat)
	     minibuffer-text-before-history))))
      (setq ialign--group (nth 0 props)
 	    ialign--spacing (nth 1 props)
	    ialign--repeat (nth 2 props)
	    ialign--pcre-mode (nth 3 props))
      (remove-list-of-text-properties
       (minibuffer-prompt-end) (point-max) '(ialign)))))

(defun ialign--regexp-with-state ()
  "Return `ialign--regexp' with properties that store current state.
These properties are restored with `ialign--restore-arguments'"
  (propertize ialign--regexp
	      'ialign
	      (list ialign--group
                    ialign--spacing
                    ialign--repeat
                    ialign--pcre-mode)))

(defun ialign--after-change (_beg _end _len)
  "Function called after change using.
Updates the minibuffer prompt and maybe realigns the region."
  (when (and (ialign--active-p) (minibufferp)
	     (not ialign--recursive-minibuffer))
    (ignore-errors
      (ialign--restore-arguments)
      (setq ialign--error nil)
      (ialign-update))))

(defun ialign-toggle-case-fold ()
  "Toggle case-fold searching on or off."
  (interactive)
  (when (ialign--active-p)
    (setq ialign--case-fold-search (not ialign--case-fold-search))
    (ialign-update 'quietly)
    (minibuffer-message
     (if ialign--case-fold-search "case insensitive" "case sensitive"))))

(defun ialign-toggle-pcre-mode ()
  "Toggle PCRE mode regexps.
This requires the `pcre2el' library."
  (interactive)
  (when (ialign--active-p)
    (if (featurep 'pcre2el)
        (setq ialign--pcre-mode (not ialign--pcre-mode))
      (error "Cannot enable PCRE mode: `pcre2el' library is not installed"))
    (ialign-update)))

(defun ialign-toggle-repeat ()
  "Toggle \\='repeat\\=' argument passed to `align-regexp'.
When the repeat argument is non-nil, the alignment is repeated throughout
the line.
Does nothing when currently not aligning with `ialign'."
  (interactive)
  (when (ialign--active-p)
    (setq ialign--repeat (not ialign--repeat))
    (ialign-update 'quietly)))

(defun ialign-toggle-tabs ()
  "Toggle tab usage during alignment.
After executing this command, the region is always aligned with either tabs
or spaces, regardless of value of the variable `ialign-align-with-tabs'.
Does nothing when currently not aligning with `ialign'."
  (interactive)
  (when (ialign--active-p)
    (setq ialign--tabs (not ialign--tabs))
    (ialign-update 'quietly)))

(defun ialign-increment-group ()
  "Increment the parenthesis group argument passed to `align-regexp'.
Use `ialign-set-group' to set the group to a specific number.
Does nothing when currently not aligning with `ialign'."
  (interactive)
  (ialign-set-group (1+ ialign--group)))

(defun ialign-decrement-group ()
  "Decrement the parenthesis group argument passed to `align-regexp'.
Use `ialign-set-group' to set the group to a specific number.
Does nothing when currently not aligning with `ialign'."
  (interactive)
  (ialign-set-group (1- ialign--group)))

(defun ialign-set-group (group)
  "Set the parenthesis group argument for the `align-regexp' command to GROUP.
Interactively, reads a number from minibuffer, unless this function was called
with a numeric prefix argument, in which case the prefix argument is used as
GROUP.
Does nothing when currently not aligning with `ialign'."
  (interactive (list
		(if current-prefix-arg
		    (prefix-numeric-value current-prefix-arg)
		  (ialign--read-number
		   "Parenthesis group to modify (justify if negative): "))))
  (or group (setq group 1))
  (when (ialign--active-p)
    (setq ialign--group group)
    (ialign-update 'quietly)))

(defun ialign-increment-spacing ()
  "Increment the amount of spacing passed to `align-regexp' command.
Use `ialign-set-spacing' to set the spacing to specific number.
Does nothing when currently not aligning with `ialign'."
  (interactive)
  (when (ialign--active-p)
    (ialign-set-spacing (1+ ialign--spacing))))

(defun ialign-decrement-spacing ()
  "Decrement the amount of spacing passed to `align-regexp' command.
Use `ialign-set-spacing' to set the spacing to specific number.
Does nothing when currently not aligning with `ialign'."
  (interactive)
  (when (ialign--active-p)
    (ialign-set-spacing (1- ialign--spacing))))

(defun ialign-set-spacing (spacing)
  "Set the spacing parameter passed to `align-regexp' command to SPACING.
Interactively, reads a number from minibuffer, unless this function was called
with a numeric prefix argument, in which case the prefix argument is used as
SPACING.
Does nothing when currently not aligning with `ialign'."
  (interactive (list
		(if current-prefix-arg
		    (prefix-numeric-value current-prefix-arg)
		  (ialign--read-number "Amount of spacing: "))))
  (or spacing (setq spacing 1))
  (when (ialign--active-p)
    (setq ialign--spacing spacing)
    (ialign-update 'quietly)))

(defun ialign-commit ()
  "Align the region using the current regexp and commit change in the buffer.
The region is aligned using the current regexp only if it's valid.
Next alignments will use the newly aligned region.
Does nothing when currently not aligning with `ialign'."
  (interactive)
  (when (ialign--active-p)
    (let ((ialign-auto-update t))
      (ialign-update))
    (ialign--with-region-narrowed
     (setq ialign--region-contents (buffer-substring (point-min) (point-max))))
    (minibuffer-message "Commited regexp %s" ialign--regexp)))

(defun ialign-update (&optional no-error)
  "Align the region with regexp in the minibuffer for preview.
Does temporary alignment for preview only.
The argument NO-ERROR, if non-nil means ignore any errors.
Use `ialign-commit' to actually align the region in the buffer."
  (interactive)
  (when (and (ialign--active-p) (minibufferp))
    (condition-case err
	(progn
	  (ialign--update-minibuffer-prompt)
	  (when (or (called-interactively-p 'interactive)
		    (ialign--autoupdate-p))
	    (let ((regexp (minibuffer-contents-no-properties)))
	      (setq ialign--regexp regexp)
	      (ialign--align)
	      (redisplay))))
      (error
       (progn
	 (setq ialign--error
	       (if (eq (car err) 'invalid-regexp)
		   (cadr err) (error-message-string err)))
	 (ialign--update-minibuffer-prompt)
	 (unless no-error
	   (signal (car err) (cdr err))))))))

(defun ialign-show-help ()
  "Show help to the user."
  (interactive)
  (with-help-window (help-buffer)
    (princ
     (substitute-command-keys
      "\\<ialign-minibuffer-keymap>Help for command `ialign':

\\[ialign-show-help]: help
\\[ialign-update]: update (realign)
\\[ialign-increment-group], \\[ialign-decrement-group]: increment/decrement \
parenthesis group
\\[ialign-set-group]: read group from minibuffer
\\[ialign-increment-spacing], \\[ialign-decrement-spacing]: increment/\
decrement spacing
\\[ialign-set-spacing]: read spacing from minibuffer
\\[ialign-toggle-repeat]: repeat the alignment throughout the line (toggle)
\\[ialign-toggle-tabs]: toggle tab usage
\\[ialign-toggle-case-fold]: toggle case fold searching
\\[ialign-toggle-pcre-mode]: toggle PCRE mode - treat input as a PCRE regexp.
\\[next-history-element], \\[previous-history-element]: next/previous history \
element
\\[ialign-commit]: commit the alignment in buffer"))))

(defvar ialign-initial-spacing)

;;;###autoload
(defun ialign (beg end &optional regexp group spacing repeat)
  "Interactively align region BEG END using regexp read from minibuffer.
As characters are typed in the minibuffer, the region is aligned
using `align-regexp' and the result is presented to the user.
\\<ialign-minibuffer-keymap>
Arguments REGEXP, GROUP, SPACING and REPEAT are passed to `align-regexp',
and default to `ialign-initial-regexp', `ialign-initial-group',
`ialign-initial-spacing' and `ialign-initial-repeat'
respectively.

If the custom option `ialign-auto-update' is not set to t, manual update is
possible with command `ialign-update' bound to \\[ialign-update].

Additional arguments passed to `align-regexp' are displayed in
the minibuffer prompt and all of them can be interactively
specified.  The parenthesis group argument can be changed using
\\[ialign-decrement-group], \\[ialign-increment-group] and \
\\[ialign-set-group], the spacing can be modified using
\\[ialign-decrement-spacing], \\[ialign-increment-spacing] \
and \\[ialign-set-spacing].

The keymap used in minibuffer is `ialign-minibuffer-keymap':
\\{ialign-minibuffer-keymap}"
  (interactive "r")
  (or regexp (setq regexp ialign-initial-regexp))
  (or group (setq group ialign-initial-group))
  (or spacing (setq spacing ialign-initial-spacing))
  (or repeat (setq repeat ialign-initial-repeat))
  (if (ialign--active-p)
      (error "Already aligning")
    (let* ((ialign--buffer (current-buffer))
	   (ialign--beg beg)
	   (ialign--end end)
	   (ialign--recursive-minibuffer nil)
	   (region-contents (buffer-substring beg end))
	   (ialign--region-contents region-contents)
	   (ialign--pcre-mode (and ialign-pcre-mode (featurep 'pcre2el)))
	   (ialign--repeat repeat)
	   (ialign--group group)
	   (ialign--spacing spacing)
	   (ialign--tabs ialign-align-with-tabs)
	   (ialign--regexp nil)
	   (ialign--case-fold-search case-fold-search)
	   success)
      (unwind-protect
	  (minibuffer-with-setup-hook
	      (lambda ()
		(unless ialign--recursive-minibuffer
		  (add-hook 'after-change-functions #'ialign--after-change nil t)
		  (ialign-update 'quietly)))
	    (let ((buffer-undo-list t)
		  (minibuffer-allow-text-properties t)
		  (history-add-new-input nil))
	      (read-from-minibuffer " " regexp
                                    ialign-minibuffer-keymap
				    nil 'ialign--history)
	      (add-to-history 'ialign--history
			      (ialign--regexp-with-state))
	      (setq success t)))
	(unwind-protect
	    (if success
		(progn
		  (unless (ialign--autoupdate-p)
		    (ialign--align))
		  (push (list 'apply #'ialign--undo
			      ialign--beg
			      ialign--end
			      region-contents)
			buffer-undo-list))
	      (let ((buffer-undo-list t))
		(ialign--revert)))
	  (when (overlayp ialign--minibuffer-overlay)
	    (delete-overlay ialign--minibuffer-overlay)))))
    (setq deactivate-mark t)))

;;;###autoload
(define-obsolete-function-alias 'ialign-interactive-align 'ialign "0.1.0")

(provide 'ialign)

;;; ialign.el ends here
