;;; ob-confirm-posframe.el --- Preview org-babel blocks before confirming -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kyle S Passarelli

;; Author: Kyle S Passarelli <kyle.passarelli@gmail.com>
;; Assisted-by: Claude:claude-opus-5-5
;; Maintainer: Kyle S Passarelli <kyle.passarelli@gmail.com>
;; URL: https://github.com/kpassapk/ob-confirm-posframe
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (posframe "1.4"))
;; Keywords: outlines, tools, convenience

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; With `org-confirm-babel-evaluate' on, org-babel asks "Evaluate this
;; code block (name) on your system?" for every block it runs, including
;; each block pulled in through `:var x=other-block'.  While it asks,
;; point sits on that dependency, so the window scrolls to it and then
;; snaps back once the chain finishes.
;;
;; `ob-confirm-posframe-mode' replaces that prompt with a fixed-size
;; posframe at a fixed position.  It shows the block's name, language,
;; the header arguments that say where it runs, the path of blocks that
;; led to it, and its fontified body.  Point stays where you started, so
;; the buffer no longer jumps.  Answer with one key:
;;
;;   y  run this block
;;   n  cancel the whole chain (nothing else in it runs)
;;   !  run this block and the rest of the chain without asking
;;   e  cancel the chain and jump to this block
;;   d  view the notices in full (only when there are notices)
;;
;; Functions on `ob-confirm-posframe-notice-functions' add highlighted
;; lines above the body: why this run deserves a second look.  A
;; notice made with `ob-confirm-posframe-notice' can carry a longer
;; detail, such as a multi-file diff, which `d' shows in a `diff-mode'
;; buffer; `q' there returns to the prompt.
;;
;; With `ob-confirm-posframe-expand-noweb' on, the body shows with its
;; noweb references expanded: the text that runs.  A call such as
;; `<<name()>>' would run the named block to expand it, so it is shown
;; as written, with a notice, and never run before you answer.
;;
;; In a terminal frame, where posframe cannot work, the same preview
;; shows in a fixed-height side window.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'ob-core)
(require 'org-element)
(require 'org-src)
(require 'posframe)

(defgroup ob-confirm-posframe nil
  "Preview org-babel blocks before confirming their evaluation."
  :group 'org-babel
  :prefix "ob-confirm-posframe-")

(defcustom ob-confirm-posframe-width 80
  "Width of the preview, in columns."
  :type 'natnum)

(defcustom ob-confirm-posframe-height 16
  "Number of body lines in the preview.
Shorter bodies are padded and longer ones cut, so the preview
always has the same size."
  :type 'natnum)

(defcustom ob-confirm-posframe-poshandler #'posframe-poshandler-frame-center
  "Posframe position handler for the preview.
See `posframe-show' for the available handlers."
  :type 'function)

(defcustom ob-confirm-posframe-border-width 2
  "Border width of the preview posframe, in pixels."
  :type 'natnum)

(defcustom ob-confirm-posframe-params '(:dir :session :var)
  "Header arguments shown in the preview, in this order.
`:var' shows variable names only, since values may be large."
  :type '(repeat symbol))

(defcustom ob-confirm-posframe-header-functions nil
  "Functions adding text to the preview's header-argument line.
Each is called with the block INFO, as returned by
`org-babel-get-src-block-info', and returns a string or nil."
  :type 'hook)

(defcustom ob-confirm-posframe-notice-functions nil
  "Functions adding notices above the body in the preview.
Each is called with the block INFO, as returned by
`org-babel-get-src-block-info', and returns a notice, a list of
notices, or nil.  A notice is a one-line string; make one with
`ob-confirm-posframe-notice' to give it a longer detail, which
the `d' key shows in a `diff-mode' buffer."
  :type 'hook)

(defcustom ob-confirm-posframe-expand-noweb nil
  "Non-nil means show the body with its noweb references expanded.
This applies to blocks whose `:noweb' header expands on
evaluation.  A plain reference `<<name>>' is replaced with the
text it stands for.  A call `<<name()>>' would run the named
block, so it is shown as written, with a notice."
  :type 'boolean)

(defface ob-confirm-posframe-title
  '((t :inherit bold))
  "Face for the block name in the preview.")

(defface ob-confirm-posframe-dim
  '((t :inherit shadow))
  "Face for secondary text in the preview.")

(defface ob-confirm-posframe-notice
  '((t :inherit warning))
  "Face for notices in the preview.")

(defface ob-confirm-posframe-key
  '((t :inherit help-key-binding))
  "Face for answer keys in the preview footer.")

(defface ob-confirm-posframe-border
  '((t :inherit font-lock-keyword-face))
  "Face whose foreground colors the preview border.")

(defconst ob-confirm-posframe--buffer " *ob-confirm-posframe*"
  "Buffer holding the preview.")

(defconst ob-confirm-posframe--notice-buffer "*ob-confirm-posframe notices*"
  "Buffer showing the notices in full.")

(define-error 'ob-confirm-posframe-cancelled
              "Evaluation cancelled" 'user-error)

(cl-defstruct (ob-confirm-posframe--chain
               (:constructor ob-confirm-posframe--chain-create))
  "State shared by every block evaluated for one top-level request."
  origin window start approved (step 0))

(defvar ob-confirm-posframe--chain nil
  "The chain being evaluated, or nil outside of one.")

(defvar ob-confirm-posframe--stack nil
  "Names of the blocks being evaluated, innermost first.")

(defvar ob-confirm-posframe--side-window nil
  "Side window showing the preview when posframe cannot work.")

;;;; Chain tracking

(defun ob-confirm-posframe--block-name (info)
  "Return the name of the block described by INFO, or of the block at point."
  (or (nth 4 info)
      (ignore-errors
        (and (derived-mode-p 'org-mode)
             (org-element-property :name (org-element-at-point))))))

(defun ob-confirm-posframe--new-chain ()
  "Return a chain whose origin is point in the current buffer."
  (let ((win (if (eq (window-buffer) (current-buffer))
                 (selected-window)
               (get-buffer-window))))
    (ob-confirm-posframe--chain-create
     :origin (point-marker)
     :window win
     :start (and win (window-start win)))))

(defun ob-confirm-posframe--execute-advice (fn &optional arg info &rest rest)
  "Around advice for `org-babel-execute-src-block'.
FN is the original function; ARG, INFO and REST are its arguments.
The outermost call starts a chain, and returns nil when it is cancelled."
  (let ((ob-confirm-posframe--stack
         (cons (ob-confirm-posframe--block-name info)
               ob-confirm-posframe--stack)))
    (if ob-confirm-posframe--chain
        (apply fn arg info rest)
      (let ((ob-confirm-posframe--chain (ob-confirm-posframe--new-chain)))
        (condition-case err
            (apply fn arg info rest)
          (ob-confirm-posframe-cancelled
           (message "%s" (cadr err))
           nil))))))

(defun ob-confirm-posframe--ref-advice (fn ref)
  "Around advice for `org-babel-ref-resolve'.
FN is the original function and REF its argument.  References are
usually resolved inside a chain; this covers callers that resolve
them before `org-babel-execute-src-block' is entered."
  (if ob-confirm-posframe--chain
      (funcall fn ref)
    (let ((ob-confirm-posframe--chain (ob-confirm-posframe--new-chain))
          (ob-confirm-posframe--stack
           (or ob-confirm-posframe--stack
               (list (ob-confirm-posframe--block-name nil)))))
      (condition-case err
          (funcall fn ref)
        (ob-confirm-posframe-cancelled
         (user-error "%s" (cadr err)))))))

;;;; Confirmation

(defun ob-confirm-posframe--read-answer (info &optional notices)
  "Read an answer for the block INFO: one of ?y, ?n, ?!, ?e or ?d.
?d is offered only when there are NOTICES."
  (read-char-choice
   (format "Run %s? (y, n, !, e%s) "
           (or (nth 4 info) (format "this %s block" (nth 0 info)))
           (if notices ", d" ""))
   (if notices '(?y ?n ?! ?e ?d) '(?y ?n ?! ?e))))

(defun ob-confirm-posframe--confirm-advice (fn info)
  "Around advice for `org-babel-confirm-evaluate'.
FN is the original function and INFO the block being confirmed."
  (let ((chain ob-confirm-posframe--chain))
    (cond
     ((or (null chain)
          (bound-and-true-p org-babel-confirm-evaluate-answer-no)
          (not (eq (org-babel-check-confirm-evaluate info) 'query)))
      (funcall fn info))
     ((ob-confirm-posframe--chain-approved chain) t)
     (t
      (cl-incf (ob-confirm-posframe--chain-step chain))
      (let ((name (or (nth 4 info) "unnamed block"))
            (root (car (last ob-confirm-posframe--stack))))
        (pcase (ob-confirm-posframe--ask info chain)
          (?y t)
          (?! (setf (ob-confirm-posframe--chain-approved chain) t) t)
          (?n (signal 'ob-confirm-posframe-cancelled
                      (list (if (equal name root)
                                (format "Evaluation of %s cancelled" name)
                              (format "Evaluation of %s cancelled at %s"
                                      (or root "chain") name)))))
          (?e (let ((target (copy-marker (nth 5 info))))
                (run-at-time 0 nil #'ob-confirm-posframe--jump target)
                (user-error "Evaluation cancelled; jumped to %s" name)))))))))

(defun ob-confirm-posframe--jump (marker)
  "Show the block at MARKER in the selected window."
  (when (buffer-live-p (marker-buffer marker))
    (pop-to-buffer-same-window (marker-buffer marker))
    (push-mark nil t)
    (goto-char marker)
    (when (derived-mode-p 'org-mode)
      (org-fold-show-context))))

(defun ob-confirm-posframe--ask (info chain)
  "Preview the block INFO of CHAIN and return the user's answer.
The answer ?d shows the notices in full, then asks again."
  (let* ((expanded (ob-confirm-posframe--expanded-body info))
         (notices (ob-confirm-posframe--notices info (cdr expanded)))
         (preview (ob-confirm-posframe--render
                   info chain notices (car expanded))))
    (ob-confirm-posframe--call-at-origin
     chain
     (lambda ()
       (let (answer)
         (while (eq ?d (setq answer
                             (progn
                               (ob-confirm-posframe--show preview)
                               (unwind-protect
                                   (ob-confirm-posframe--read-answer
                                    info notices)
                                 (ob-confirm-posframe--hide)))))
           (ob-confirm-posframe--view-notices notices))
         answer)))))

(defun ob-confirm-posframe--call-at-origin (chain thunk)
  "Call THUNK with point and window back at CHAIN's origin.
Babel moves point onto each dependency before confirming it, which
scrolls the window there while the prompt waits.  Putting point back
for the duration of the prompt keeps the view still."
  (let ((origin (ob-confirm-posframe--chain-origin chain))
        (win (ob-confirm-posframe--chain-window chain)))
    (if (not (and (window-live-p win)
                  (buffer-live-p (marker-buffer origin))))
        (funcall thunk)
      (save-window-excursion
        (with-current-buffer (marker-buffer origin)
          (save-excursion
            (unless (eq (window-buffer win) (current-buffer))
              (set-window-buffer win (current-buffer)))
            (goto-char origin)
            (set-window-point win origin)
            (set-window-start win (ob-confirm-posframe--chain-start chain) t)
            (funcall thunk)))))))

;;;; Notices

(defun ob-confirm-posframe-notice (line &optional detail)
  "Return a notice showing LINE, with DETAIL behind the `d' key.
DETAIL is a string, such as a unified diff, or a function of no
arguments returning one, called only when the notice is viewed.
Without DETAIL, `d' shows LINE itself."
  (if detail
      (propertize line 'ob-confirm-posframe-detail detail)
    line))

(defun ob-confirm-posframe--notice-detail (notice)
  "Return the detail of NOTICE as a string, or nil when it has none.
A detail function that fails returns its error instead, so the
prompt survives it."
  (let ((detail (get-text-property 0 'ob-confirm-posframe-detail notice)))
    (if (functionp detail)
        (condition-case err (funcall detail)
          (error (format "Detail failed: %s" (error-message-string err))))
      detail)))

(defun ob-confirm-posframe--notices (info &optional calls)
  "Return the notices for the block INFO, in order.
They come from `ob-confirm-posframe-notice-functions', then one
per noweb call in CALLS."
  (append
   (seq-filter
    (lambda (n) (and (stringp n) (not (string-empty-p n))))
    (mapcan (lambda (f)
              (let ((r (funcall f info)))
                (if (listp r) (copy-sequence r) (list r))))
            ob-confirm-posframe-notice-functions))
   (mapcar (lambda (c) (format "noweb call: %s" c)) calls)))

(defun ob-confirm-posframe--expanded-body (info)
  "Return (BODY . CALLS) for the block INFO, or nil.
BODY has the noweb references expanded, and CALLS lists each call
`<<name()>>', which stays as written in BODY.  Return nil unless
`ob-confirm-posframe-expand-noweb' is on and INFO expands noweb
on evaluation.  Expansion runs in the current buffer, which must
hold the block."
  (when (and ob-confirm-posframe-expand-noweb
             (org-babel-noweb-p (nth 2 info) :eval))
    (let (calls)
      (condition-case err
          ;; Expanding a call resolves it, which runs the block.  Put
          ;; it back as written instead, here and in every reference
          ;; expanded on the way.
          (cl-letf (((symbol-function 'org-babel-ref-resolve)
                     (lambda (id)
                       (cl-pushnew id calls :test #'equal)
                       (concat org-babel-noweb-wrap-start id
                               org-babel-noweb-wrap-end))))
            (cons (org-babel-expand-noweb-references info)
                  (nreverse calls)))
        (error
         (message "ob-confirm-posframe: noweb not expanded: %s"
                  (error-message-string err))
         nil)))))

(defun ob-confirm-posframe--view-notices (notices)
  "Show NOTICES in full in a `diff-mode' window until `q' is pressed.
Each notice shows its line, then its detail, if any."
  (let ((buf (get-buffer-create ob-confirm-posframe--notice-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (n notices)
          (let ((detail (ob-confirm-posframe--notice-detail n)))
            (insert (substring-no-properties n) "\n")
            (when detail
              (insert detail)
              (unless (string-suffix-p "\n" detail) (insert "\n")))
            (insert "\n"))))
      (diff-mode)
      (setq buffer-read-only t)
      (ob-confirm-posframe-notices-mode 1)
      (goto-char (point-min)))
    (save-window-excursion
      (pop-to-buffer buf)
      (message "%s returns to the prompt"
               (propertize "q" 'face 'ob-confirm-posframe-key))
      (recursive-edit))))

(defun ob-confirm-posframe-notices-quit ()
  "Return to the prompt, or quit the window outside of one."
  (interactive)
  (if (> (recursion-depth) 0)
      (exit-recursive-edit)
    (quit-window)))

(defvar-keymap ob-confirm-posframe-notices-mode-map
  :doc "Keymap for `ob-confirm-posframe-notices-mode'."
  "q" #'ob-confirm-posframe-notices-quit)

(define-minor-mode ob-confirm-posframe-notices-mode
  "Minor mode for the buffer showing a prompt's notices.
\\<ob-confirm-posframe-notices-mode-map>\\[ob-confirm-posframe-notices-quit] returns to the prompt."
  :lighter nil)

;;;; Rendering

(defun ob-confirm-posframe--fontify (body lang)
  "Return BODY fontified as LANG source code."
  (let ((mode (ignore-errors (org-src-get-lang-mode lang))))
    (with-temp-buffer
      (insert body)
      (when (fboundp mode)
        (ignore-errors
          (delay-mode-hooks (funcall mode))
          (font-lock-ensure)))
      (buffer-string))))

(defun ob-confirm-posframe--fit (string)
  "Return STRING cut or padded to the preview width, with a left margin."
  (let ((w ob-confirm-posframe-width))
    (concat " " (truncate-string-to-width string (1- w) nil ?\s "…"))))

(defun ob-confirm-posframe--param-line (info)
  "Return the header-argument line for the block INFO."
  (let* ((params (nth 2 info))
         (parts
          (delq nil
                (mapcar
                 (lambda (key)
                   (if (eq key :var)
                       (when-let* ((vars (org-babel--get-vars params)))
                         (format ":var %s"
                                 (mapconcat (lambda (v) (format "%s" (car v)))
                                            vars ", ")))
                     (when-let* ((val (cdr (assq key params))))
                       (unless (member val '("none" "no"))
                         (format "%s %s" key val)))))
                 ob-confirm-posframe-params))))
    (string-join
     (append parts
             (delq nil (mapcar (lambda (f) (funcall f info))
                               ob-confirm-posframe-header-functions)))
     "  ")))

(defun ob-confirm-posframe--notice-lines (notices height)
  "Return NOTICES as at most HEIGHT highlighted lines."
  (let* ((lines (mapcar
                 (lambda (n)
                   (let ((l (car (split-string n "\n"))))
                     (add-face-text-property 0 (length l)
                                             'ob-confirm-posframe-notice t l)
                     l))
                 notices))
         (n (length lines)))
    (cond
     ((<= n height) lines)
     ((<= height 0) nil)
     (t (append (seq-take lines (1- height))
                (list (propertize (format "… %d more notices, d to view"
                                          (- n (1- height)))
                                  'face 'ob-confirm-posframe-dim)))))))

(defun ob-confirm-posframe--body-lines (body lang h)
  "Return BODY, fontified as LANG, as exactly H lines."
  (let* ((lines (split-string
                 (ob-confirm-posframe--fontify (string-trim-right body) lang)
                 "\n"))
         (n (length lines)))
    (when (> n h)
      (setq lines (append (seq-take lines (1- h))
                          (list (propertize (format "… %d more lines"
                                                    (- n (1- h)))
                                            'face 'ob-confirm-posframe-dim)))))
    (append lines (make-list (max 0 (- h (length lines))) ""))))

(defun ob-confirm-posframe--render (info chain &optional notices body)
  "Return the preview text for the block INFO of CHAIN.
NOTICES show above the body, BODY in place of the block's own.
It always has `ob-confirm-posframe-height' plus six lines: the
notices take at most half of the body's lines."
  (let* ((notice-lines (ob-confirm-posframe--notice-lines
                        notices (/ ob-confirm-posframe-height 2)))
         (dim (lambda (s) (propertize s 'face 'ob-confirm-posframe-dim)))
         (key (lambda (k) (propertize k 'face 'ob-confirm-posframe-key)))
         (rule (funcall dim (make-string (1- ob-confirm-posframe-width) ?─)))
         (path (reverse (mapcar (lambda (n) (or n "unnamed"))
                                ob-confirm-posframe--stack))))
    (mapconcat
     #'ob-confirm-posframe--fit
     (append
      (list (concat (propertize (or (nth 4 info) "unnamed block")
                                'face 'ob-confirm-posframe-title)
                    (funcall dim (format " · %s · prompt %d"
                                         (nth 0 info)
                                         (ob-confirm-posframe--chain-step chain))))
            (funcall dim (if (cdr path) (string-join path " › ") ""))
            (funcall dim (ob-confirm-posframe--param-line info))
            rule)
      notice-lines
      (ob-confirm-posframe--body-lines
       (or body (nth 1 info)) (nth 0 info)
       (max 0 (- ob-confirm-posframe-height (length notice-lines))))
      (list rule
            (concat (funcall key "y") " run   "
                    (funcall key "n") " cancel chain   "
                    (funcall key "!") " run whole chain   "
                    (funcall key "e") " jump to block"
                    (if notices
                        (concat "   " (funcall key "d") " notices")
                      ""))))
     "\n")))

;;;; Display

(defun ob-confirm-posframe--show (preview)
  "Display PREVIEW in a posframe, or a side window in a terminal."
  (let ((buf (get-buffer-create ob-confirm-posframe--buffer))
        (w (1+ ob-confirm-posframe-width))
        (h (+ ob-confirm-posframe-height 6)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert preview))
      (setq-local truncate-lines t
                  cursor-type nil
                  mode-line-format nil)
      (goto-char (point-min)))
    (if (posframe-workable-p)
        (posframe-show buf
                       :poshandler ob-confirm-posframe-poshandler
                       :width w :height h :min-width w :min-height h
                       :border-width ob-confirm-posframe-border-width
                       :border-color (face-foreground
                                      'ob-confirm-posframe-border nil t)
                       :lines-truncate t
                       :accept-focus nil)
      (setq ob-confirm-posframe--side-window
            (display-buffer buf `((display-buffer-in-side-window)
                                  (side . bottom)
                                  (window-height . ,h)
                                  (preserve-size . (nil . t))))))))

(defun ob-confirm-posframe--hide ()
  "Hide the preview."
  (when (get-buffer ob-confirm-posframe--buffer)
    (posframe-hide ob-confirm-posframe--buffer))
  (when (window-live-p ob-confirm-posframe--side-window)
    (delete-window ob-confirm-posframe--side-window))
  (setq ob-confirm-posframe--side-window nil))

;;;; Mode

;;;###autoload
(define-minor-mode ob-confirm-posframe-mode
  "Preview each org-babel block in a posframe before confirming it.
Takes effect only while `org-confirm-babel-evaluate' would ask."
  :global t
  :group 'ob-confirm-posframe
  (if ob-confirm-posframe-mode
      (progn
        ;; Outermost, so a block another advice runs while setting
        ;; up its own block prompts inside that block's chain.
        (advice-add 'org-babel-execute-src-block :around
                    #'ob-confirm-posframe--execute-advice
                    '((depth . -90)))
        (advice-add 'org-babel-ref-resolve :around
                    #'ob-confirm-posframe--ref-advice)
        (advice-add 'org-babel-confirm-evaluate :around
                    #'ob-confirm-posframe--confirm-advice))
    (advice-remove 'org-babel-execute-src-block
                   #'ob-confirm-posframe--execute-advice)
    (advice-remove 'org-babel-ref-resolve #'ob-confirm-posframe--ref-advice)
    (advice-remove 'org-babel-confirm-evaluate
                   #'ob-confirm-posframe--confirm-advice)))

(provide 'ob-confirm-posframe)
;;; ob-confirm-posframe.el ends here
