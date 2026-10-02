;;; ob-confirm-posframe-test.el --- Tests for ob-confirm-posframe -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run with `make test'.  Answers are scripted by replacing
;; `ob-confirm-posframe--read-answer'.

;;; Code:

(require 'ert)
(require 'ob-confirm-posframe)

(defvar ob-confirm-posframe-test--ran nil
  "Names of blocks that actually executed, in order.")

(defconst ob-confirm-posframe-test--doc
  "#+name: root
#+begin_src emacs-lisp :var x=mid
(push \"root\" ob-confirm-posframe-test--ran)
(+ x 1)
#+end_src

Some text so the dependencies sit away from the root block.

#+name: mid
#+begin_src emacs-lisp :var y=leaf
(push \"mid\" ob-confirm-posframe-test--ran)
(+ y 1)
#+end_src

#+name: leaf
#+begin_src emacs-lisp
(push \"leaf\" ob-confirm-posframe-test--ran)
1
#+end_src
"
  "Three blocks: root depends on mid, which depends on leaf.")

(defmacro ob-confirm-posframe-test--with (answers &rest body)
  "Run BODY in the test document, answering prompts from ANSWERS.
Binds `prompts' to the list of (NAME STACK POINT BUFFER) seen at each
prompt, in order."
  (declare (indent 1))
  `(let ((queue (copy-sequence ,answers))
         (prompts nil)
         (ob-confirm-posframe-test--ran nil)
         (org-confirm-babel-evaluate t))
     (with-temp-buffer
       (org-mode)
       (insert ob-confirm-posframe-test--doc)
       ;; The origin is only restored for a buffer shown in a window.
       (set-window-buffer nil (current-buffer))
       (goto-char (point-min))
       (search-forward "(push")
       (ob-confirm-posframe-mode 1)
       (unwind-protect
           (cl-letf (((symbol-function 'ob-confirm-posframe--read-answer)
                      (lambda (info)
                        (push (list (nth 4 info) ob-confirm-posframe--stack
                                    (point) (current-buffer))
                              prompts)
                        (or (pop queue) (error "Unexpected prompt")))))
             (prog1 (progn ,@body)
               (setq prompts (nreverse prompts))))
         (ob-confirm-posframe-mode -1)))))

(defun ob-confirm-posframe-test--run ()
  "Execute the root block, returning its result."
  (save-excursion
    (goto-char (point-min))
    (org-babel-next-src-block)
    (org-babel-execute-src-block)))

(ert-deftest ob-confirm-posframe-test-yes-runs-everything ()
  (let (result prompts-seen)
    (ob-confirm-posframe-test--with '(?y ?y ?y)
      (setq result (ob-confirm-posframe-test--run)
            prompts-seen (mapcar #'car prompts))
      (should (equal ob-confirm-posframe-test--ran '("root" "mid" "leaf"))))
    (should (equal result 3))
    ;; Babel confirms a block before resolving its :var references.
    (should (equal prompts-seen '("root" "mid" "leaf")))))

(ert-deftest ob-confirm-posframe-test-no-cancels-chain ()
  (ob-confirm-posframe-test--with '(?n)
    (should (null (ob-confirm-posframe-test--run)))
    (should (= 1 (length prompts)))
    (should (null ob-confirm-posframe-test--ran))))

(ert-deftest ob-confirm-posframe-test-no-on-root-cancels ()
  (ob-confirm-posframe-test--with '(?y ?y ?n)
    (should (null (ob-confirm-posframe-test--run)))
    (should (equal ob-confirm-posframe-test--ran '("mid" "leaf")))))

(ert-deftest ob-confirm-posframe-test-bang-approves-chain ()
  (ob-confirm-posframe-test--with '(?!)
    (should (equal (ob-confirm-posframe-test--run) 3))
    (should (= 1 (length prompts)))
    (should (equal ob-confirm-posframe-test--ran '("root" "mid" "leaf")))))

(ert-deftest ob-confirm-posframe-test-bang-is-per-chain ()
  (ob-confirm-posframe-test--with '(?! ?y ?y ?y)
    (ob-confirm-posframe-test--run)
    (ob-confirm-posframe-test--run)
    (should (= 4 (length prompts)))))

(ert-deftest ob-confirm-posframe-test-e-cancels-with-user-error ()
  (ob-confirm-posframe-test--with '(?e)
    (should-error (ob-confirm-posframe-test--run) :type 'user-error)
    (should (null ob-confirm-posframe-test--ran))))

(ert-deftest ob-confirm-posframe-test-point-stays-at-origin ()
  (ob-confirm-posframe-test--with '(?y ?y ?y)
    (let ((origin (save-excursion
                    (goto-char (point-min))
                    (org-babel-next-src-block)
                    (point))))
      (ob-confirm-posframe-test--run)
      (dolist (p prompts)
        (should (= (nth 2 p) origin))
        (should (eq (nth 3 p) (current-buffer)))))))

(ert-deftest ob-confirm-posframe-test-stack-shows-path ()
  (ob-confirm-posframe-test--with '(?y ?y ?y)
    (ob-confirm-posframe-test--run)
    (should (equal (nth 1 (car (last prompts))) '("leaf" "mid" "root")))))

(ert-deftest ob-confirm-posframe-test-no-prompt-when-not-asking ()
  (ob-confirm-posframe-test--with nil
    (let ((org-confirm-babel-evaluate nil))
      (should (equal (ob-confirm-posframe-test--run) 3))
      (should (null prompts)))))

(ert-deftest ob-confirm-posframe-test-render-fixed-size ()
  (let ((chain (ob-confirm-posframe--chain-create))
        (expected (+ ob-confirm-posframe-height 6)))
    (dolist (body (list "x"
                        (mapconcat #'number-to-string (number-sequence 1 100)
                                   "\n")
                        (make-string 500 ?a)))
      (let* ((info (list "emacs-lisp" body '((:dir . "/tmp")) "" "blk" 1))
             (lines (split-string (ob-confirm-posframe--render info chain)
                                  "\n")))
        (should (= (length lines) expected))
        (dolist (l lines)
          (should (<= (string-width l) ob-confirm-posframe-width)))))))

(ert-deftest ob-confirm-posframe-test-render-truncation-note ()
  (let* ((ob-confirm-posframe-height 5)
         (info (list "sh" "a\nb\nc\nd\ne\nf\ng" nil "" "blk" 1))
         (text (ob-confirm-posframe--render
                info (ob-confirm-posframe--chain-create))))
    (should (string-match-p "… 3 more lines" text))
    (should (string-match-p "^ d +$" text))
    (should-not (string-match-p "^ e +$" text))))

(ert-deftest ob-confirm-posframe-test-param-line ()
  (let ((ob-confirm-posframe-header-functions (list (lambda (_) "host=web1"))))
    (should (equal (ob-confirm-posframe--param-line
                    (list "sh" "" '((:dir . "/srv") (:session . "none")
                                    (:var . (a . 1)) (:var . (b . 2)))))
                   ":dir /srv  :var a, b  host=web1"))))

(ert-deftest ob-confirm-posframe-test-mode-off-removes-advice ()
  (ob-confirm-posframe-mode 1)
  (ob-confirm-posframe-mode -1)
  (should-not (advice-member-p #'ob-confirm-posframe--confirm-advice
                               'org-babel-confirm-evaluate))
  (should-not (advice-member-p #'ob-confirm-posframe--execute-advice
                               'org-babel-execute-src-block)))

(provide 'ob-confirm-posframe-test)
;;; ob-confirm-posframe-test.el ends here
