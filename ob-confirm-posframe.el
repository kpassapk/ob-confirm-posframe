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

(defface ob-confirm-posframe-title
  '((t :inherit bold))
  "Face for the block name in the preview.")

(defface ob-confirm-posframe-dim
  '((t :inherit shadow))
  "Face for secondary text in the preview.")

(defface ob-confirm-posframe-key
  '((t :inherit help-key-binding))
  "Face for answer keys in the preview footer.")

(defface ob-confirm-posframe-border
  '((t :inherit font-lock-keyword-face))
  "Face whose foreground colors the preview border.")

(defconst ob-confirm-posframe--buffer " *ob-confirm-posframe*"
  "Buffer holding the preview.")

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

(defun ob-confirm-posframe--read-answer (info)
  "Read an answer for the block INFO: one of ?y, ?n, ?! or ?e."
  (read-char-choice
   (format "Run %s? (y, n, !, e) "
           (or (nth 4 info) (format "this %s block" (nth 0 info))))
   '(?y ?n ?! ?e)))

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
  "Preview the block INFO of CHAIN and return the user's answer."
  (let ((preview (ob-confirm-posframe--render info chain)))
    (ob-confirm-posframe--call-at-origin
     chain
     (lambda ()
       (ob-confirm-posframe--show preview)
       (unwind-protect (ob-confirm-posframe--read-answer info)
         (ob-confirm-posframe--hide))))))

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

(defun ob-confirm-posframe--body-lines (info)
  "Return the body of INFO as exactly `ob-confirm-posframe-height' lines."
  (let* ((h ob-confirm-posframe-height)
         (lines (split-string
                 (ob-confirm-posframe--fontify
                  (string-trim-right (nth 1 info)) (nth 0 info))
                 "\n"))
         (n (length lines)))
    (when (> n h)
      (setq lines (append (seq-take lines (1- h))
                          (list (propertize (format "… %d more lines"
                                                    (- n (1- h)))
                                            'face 'ob-confirm-posframe-dim)))))
    (append lines (make-list (max 0 (- h (length lines))) ""))))

(defun ob-confirm-posframe--render (info chain)
  "Return the preview text for the block INFO of CHAIN.
It always has `ob-confirm-posframe-height' plus six lines."
  (let* ((dim (lambda (s) (propertize s 'face 'ob-confirm-posframe-dim)))
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
      (ob-confirm-posframe--body-lines info)
      (list rule
            (concat (funcall key "y") " run   "
                    (funcall key "n") " cancel chain   "
                    (funcall key "!") " run whole chain   "
                    (funcall key "e") " jump to block")))
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
        (advice-add 'org-babel-execute-src-block :around
                    #'ob-confirm-posframe--execute-advice)
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
