;;; default-font-presets.el --- Support selecting fonts from a list of presets -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2020  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-default-font-presets
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; This package provides a convenient method of selecting default fonts.
;;
;; To bind keys to next/previous font.
;;

;;; Usage

;; Example:
;;
;;   (when (display-graphic-p)
;;     (define-key global-map (kbd "<M-prior>") 'default-font-presets-forward)
;;     (define-key global-map (kbd "<M-next>") 'default-font-presets-backward))

;;; Code:

;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup default-font-presets nil
  "Convenience functions for scaling and cycling the default font."
  :group 'faces)

(defcustom default-font-presets-list nil
  "List font presets you wish to use."
  :type '(repeat string))

(defcustom default-font-presets-shared-state nil
  "Presets share state (currently only scale).

Otherwise each preset remembers the last scale set."
  :type 'boolean)

(defcustom default-font-presets-reset-scale-on-switch t
  "Reset the font scale when setting a new preset."
  :type 'boolean)

(defcustom default-font-presets-scale-fit-margin 1
  "Offset for `fill-column' indicator when fitting."
  :type 'integer)

(defcustom default-font-presets-scale-fit-reset-hscroll t
  "When resetting the scale, also reset the horizontal scroll."
  :type 'boolean)


;; ---------------------------------------------------------------------------
;; Internal Variables

;; The index of the font in `default-font-presets--index',
;; initialized on first use.
(defvar default-font-presets--index nil)
(defvar default-font-presets--scale-delta 0)
;; A hash: string -> delta (when non-nil).
(defvar default-font-presets--scale-per-preset nil)

;; List of interactive commands.
(defconst default-font-presets--commands
  (list
   'default-font-presets-step
   'default-font-presets-forward
   'default-font-presets-backward
   'default-font-presets-choose
   'default-font-presets-scale-increase
   'default-font-presets-scale-decrease
   'default-font-presets-scale-reset
   'default-font-presets-scale-fit))


;; ---------------------------------------------------------------------------
;; Internal Functions/Macros

(defsubst default-font-presets--current-font-get ()
  "Return the current default font."
  (font-get (face-attribute 'default :font) :name))

(defsubst default-font-presets--current-font-set (current-font)
  "Set the CURRENT-FONT font."
  (set-face-attribute 'default nil :font current-font))

(defun default-font-presets--per-preset-state-store ()
  "Store the state of the current preset."
  (declare (important-return-value nil))
  (when default-font-presets--index
    (let ((current-font (nth default-font-presets--index default-font-presets-list)))
      (cond
       ((zerop default-font-presets--scale-delta)
        (when default-font-presets--scale-per-preset
          (remhash current-font default-font-presets--scale-per-preset)))
       (t
        (unless default-font-presets--scale-per-preset
          (setq default-font-presets--scale-per-preset
                (make-hash-table :test #'equal :weakness 'key)))

        (puthash
         current-font
         default-font-presets--scale-delta
         default-font-presets--scale-per-preset))))))

(defun default-font-presets--per-preset-state-restore (current-font)
  "Restore the scale for CURRENT-FONT."
  (declare (important-return-value nil))
  (when default-font-presets--scale-per-preset
    (let ((scale-delta (gethash current-font default-font-presets--scale-per-preset)))
      (when scale-delta
        (setq default-font-presets--scale-delta scale-delta)))))

(defun default-font-presets--message (&rest args)
  "Format a message with ARGS (without logging)."
  (declare (important-return-value nil))
  (let ((message-log-max nil))
    (apply #'message (cons (concat "default-font: " (car args)) (cdr args)))))

(defun default-font-presets--split (font-name)
  "Simply split FONT-NAME that might be used for XFT properties.
For example: `A:B` is converted to (`A` `:B`)."
  (declare (important-return-value t))
  (let ((sep (string-match-p ":" font-name)))
    (cond
     (sep
      (cons (substring font-name 0 sep) (substring font-name sep)))
     (t
      (cons font-name "")))))

(defun default-font-presets--scale-by-delta (font-name scale-delta)
  "Scale font FONT-NAME by adding SCALE-DELTA."
  (declare (important-return-value t))
  (pcase-let ((`(,head . ,tail) (default-font-presets--split font-name)))
    (let ((beg (string-match-p "\\([[:blank:]]\\|-\\)[0-9]+[0-9.]*\\'" head)))
      (when beg
        (setq beg (1+ beg))
        (let* ((size-old (substring head beg))

               ;; Error checked, while it's unlikely this will fail.
               ;; Any errors here will seem like a bug, so show a message instead.
               (size-new
                (let ((size-test
                       (condition-case err
                           (string-to-number size-old)
                         (error
                          (message "Unable to convert %S to a number: %s"
                                   size-old
                                   (error-message-string err))
                          nil))))
                  (when size-test
                    (when (floatp size-test)
                      (setq size-test
                            (cond
                             ((< 0 scale-delta)
                              (ceiling size-test))
                             (t
                              (floor size-test)))))
                    (number-to-string (max 1 (+ scale-delta size-test)))))))
          (when size-new

            (setq font-name (concat (substring head 0 beg) size-new tail)))))))
  font-name)

(defun default-font-presets--font-update (current-font)
  "Update the font to CURRENT-FONT."
  (declare (important-return-value t))
  ;; Scale the font if needed.
  (unless (zerop default-font-presets--scale-delta)
    (setq current-font
          (default-font-presets--scale-by-delta current-font default-font-presets--scale-delta)))

  (cond
   ((condition-case _err
        (progn
          (default-font-presets--current-font-set current-font)
          t)
      (error
       nil))

    ;; Update the default font for new windows.
    (let ((cell (assoc 'font default-frame-alist 'eq)))
      (when cell
        (setcdr cell current-font)))

    ;; Return the font used, in-case we want to print it.
    current-font)

   (t
    ;; Failure, the font failed to load.
    nil)))

(defun default-font-presets--index-update ()
  "Set the current font based on the current index and scale delta."
  (declare (important-return-value t))
  (let ((current-font (nth default-font-presets--index default-font-presets-list)))
    (default-font-presets--font-update current-font)))

(defun default-font-presets--index-update-on-switch ()
  "Set the current font based on the current index and scale delta."
  (declare (important-return-value t))
  (let ((current-font (nth default-font-presets--index default-font-presets-list)))
    (unless default-font-presets-shared-state
      (default-font-presets--per-preset-state-restore current-font))
    (default-font-presets--font-update current-font)))

(defun default-font-presets--index-ensure (font-name font-name-no-attrs)
  "Add FONT-NAME list or return index if it's not already there.

Argument FONT-NAME-NO-ATTRS is simply to avoid re-calculating the value."
  (declare (important-return-value t))
  (catch 'result
    (let ((index 0))
      (dolist (font-name-iter default-font-presets-list)
        (when (string-equal font-name-no-attrs (car (default-font-presets--split font-name-iter)))
          (throw 'result index))
        (setq index (1+ index))))
    (push font-name default-font-presets-list)
    ;; First in the list (don't bother adding in ordered).
    (throw 'result 0)))

;; Use custom font from environment when available.
(defun default-font-presets--init ()
  "Initialize `default-font-presets--index'.

This adds to `default-font-presets-list' or replacing one of it's values
when the default font is already in the list.

Replacement is done so any fine tuning to the default font is kept,
so attributes are kept (for example)."
  (declare (important-return-value nil))
  (let ((font-index-test nil))
    (let ((current-font (default-font-presets--current-font-get)))
      (unless (string-equal current-font "")
        (let ((current-font-no-attrs (car (default-font-presets--split current-font))))
          (setq font-index-test
                (default-font-presets--index-ensure current-font current-font-no-attrs)))))
    (cond
     (font-index-test
      (setq default-font-presets--index font-index-test))
     (t
      ;; Fall-back to first font (unlikely we can't find our own font).
      (unless default-font-presets--index
        (setq default-font-presets--index 0))
      (default-font-presets--index-update)))))

(defun default-font-presets--ensure-once ()
  "Ensure we initialize the font list."
  (declare (important-return-value nil))
  (unless default-font-presets--index
    (default-font-presets--init)))

(defun default-font-presets--switch-pre ()
  "Run before switching away from the current font."
  (declare (important-return-value nil))
  (unless default-font-presets-shared-state
    (default-font-presets--per-preset-state-store))
  (when default-font-presets-reset-scale-on-switch
    (setq default-font-presets--scale-delta 0)))

(defun default-font-presets--scale-fit-impl (win)
  "Fit the `fill-column' to the window width for WIN.
Return non-nil when the scale changed."
  (declare (important-return-value nil))
  (default-font-presets--ensure-once)
  (let ((target-width (+ fill-column default-font-presets-scale-fit-margin))
        (win-width (window-max-chars-per-line win))
        ;; Only needed for scaling up.
        (scale-delta-prev default-font-presets--scale-delta)
        ;; Only needed to detect change.
        (scale-delta-init default-font-presets--scale-delta)
        ;; Compare with the previous final font
        ;; (prevent any clamping from causing an infinite loop).
        (font-prev nil)
        (font-curr t)
        ;; Don't redraw while resizing.
        (inhibit-redisplay t))
    (cond
     ((> target-width win-width)
      (while (and (>= target-width win-width) (not (eq font-curr font-prev)))
        (setq default-font-presets--scale-delta (1- default-font-presets--scale-delta))
        (setq font-prev font-curr)
        (setq font-curr (default-font-presets--index-update))
        (setq win-width (window-max-chars-per-line win))))
     ((< target-width win-width)
      (while (and (< target-width win-width) (not (eq font-curr font-prev)))
        (setq scale-delta-prev default-font-presets--scale-delta)
        (setq default-font-presets--scale-delta (1+ default-font-presets--scale-delta))
        (setq font-prev font-curr)
        (setq font-curr (default-font-presets--index-update))
        (setq win-width (window-max-chars-per-line win)))
      (setq default-font-presets--scale-delta scale-delta-prev)
      (default-font-presets--index-update)))

    ;; Avoids annoying situation where being zoomed in too far may have scrolled right,
    ;; where resetting the view removes the need for scrolling
    ;; but the horizontal scroll remains.
    ;; When horizontal scrolling, ensure the cursor remains in the view.
    ;; It's odd this isn't built in functionality.
    (when default-font-presets-scale-fit-reset-hscroll
      (let* ((column (current-column))
             (win-width (window-max-chars-per-line win))
             (hscroll-next (max 0 (- (+ column hscroll-margin) win-width))))
        (unless (eq hscroll-next (window-hscroll win))
          (set-window-hscroll win hscroll-next))))

    ;; Return t when the scale changed.
    (not (eq scale-delta-init default-font-presets--scale-delta))))

;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun default-font-presets-step (arg)
  "Cycle the font in `default-font-presets-list'.

ARG is added to the current index, a negative number cycles backwards.
When nil, 1 is used."
  (declare (important-return-value t))
  (interactive "p")
  (unless (display-graphic-p)
    (user-error "Cannot cycle fonts on non-graphical frame window!"))

  (default-font-presets--ensure-once)
  (default-font-presets--switch-pre)

  (setq arg (or arg 1))

  ;; Set the next/previous font index.
  ;; Note that this is done in a loop so we can skip fonts that aren't found.
  (let ((list-len (length default-font-presets-list))
        (skip-len 0)
        (current-font nil))
    ;; Use a while loop in case there are fonts missing.
    (while (and (null current-font) (< skip-len list-len))
      (setq default-font-presets--index (mod (+ default-font-presets--index arg) list-len))
      (unless (setq current-font (default-font-presets--index-update-on-switch))
        (setq skip-len (1+ skip-len))))

    (cond
     ((null current-font)
      (default-font-presets--message
       "warning, none of the %d font presets could be found!" list-len))
     ((not (zerop skip-len))
      (default-font-presets--message "%s, skipped %d" current-font skip-len))
     (t
      (default-font-presets--message "%s" current-font)))))

;;;###autoload
(defun default-font-presets-forward ()
  "Set the next font in the list."
  (declare (important-return-value nil))
  (interactive)
  (default-font-presets-step 1))

;;;###autoload
(defun default-font-presets-backward ()
  "Set the previous font in the list."
  (declare (important-return-value nil))
  (interactive)
  (default-font-presets-step -1))

;;;###autoload
(defun default-font-presets-choose ()
  "Select a font preset from the preset list."
  (declare (important-return-value nil))
  (interactive)

  (default-font-presets--ensure-once)
  (let ((prompt "Select font: ")
        (default-index default-font-presets--index)
        (content (list))
        (font-set-index-fn
         (lambda (index)
           (default-font-presets--switch-pre)
           (setq default-font-presets--index index)
           (condition-case _err
               (default-font-presets--index-update-on-switch)
             (error
              nil)))))

    (let ((index 0))
      (dolist (font-name default-font-presets-list)
        (push (cons font-name index) content)
        (setq index (1+ index)))
      (setq content (nreverse content)))

    (cond
     ;; Ivy (optional).
     ((fboundp 'ivy-read)
      (ivy-read
       prompt content
       :preselect
       (cond
        ((= default-index -1)
         nil)
        (t
         default-index))
       :require-match t
       :action
       (lambda (result)
         (pcase-let ((`(,_text . ,index) result))
           (funcall font-set-index-fn index)))
       :caller #'default-font-presets-choose))
     ;; Fallback to completing read.
     (t
      (let ((choice (completing-read prompt content nil t nil nil (nth default-index content))))
        (pcase-let ((`(,_text . ,index) (assoc choice content)))
          (funcall font-set-index-fn index)))))))

;;;###autoload
(defun default-font-presets-scale-increase ()
  "Increase the font size."
  (declare (important-return-value nil))
  (interactive)
  (default-font-presets--ensure-once)
  (setq default-font-presets--scale-delta (1+ default-font-presets--scale-delta))
  (let ((current-font (default-font-presets--index-update)))
    (default-font-presets--message current-font)))

;;;###autoload
(defun default-font-presets-scale-decrease ()
  "Decrease the font size."
  (declare (important-return-value nil))
  (interactive)
  (default-font-presets--ensure-once)
  (setq default-font-presets--scale-delta (1- default-font-presets--scale-delta))
  (let ((current-font (default-font-presets--index-update)))
    (default-font-presets--message current-font)))

;;;###autoload
(defun default-font-presets-scale-reset ()
  "Reset the font size."
  (declare (important-return-value nil))
  (interactive)
  (default-font-presets--ensure-once)
  (setq default-font-presets--scale-delta 0)
  (let ((current-font (default-font-presets--index-update)))
    (default-font-presets--message current-font)))

;;;###autoload
(defun default-font-presets-scale-fit ()
  "Fit the `fill-column' to the window width.
Return non-nil when the scale changed."
  (declare (important-return-value nil))
  (interactive)
  (let ((win (selected-window)))
    (cond
     (win
      (default-font-presets--scale-fit-impl win))
     (t
      (message "No active window")
      nil))))

;; Evil Mode (setup if in use).
;;
;; Don't let these commands repeat as they are for the UI, not editor.
;;
;; Notes:
;; - Package lint complains about using this command,
;;   however it's needed to avoid issues with `evil-mode'.
(declare-function evil-declare-not-repeat "ext:evil-common")
(with-eval-after-load 'evil
  (mapc #'evil-declare-not-repeat default-font-presets--commands))

(provide 'default-font-presets)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; default-font-presets.el ends here
