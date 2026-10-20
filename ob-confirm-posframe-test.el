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
Binds `prompts' to the list of (NAME STACK POINT BUFFER NOTICES
PREVIEW) seen at each prompt, latest first while BODY runs, then
in order.  PREVIEW is the text shown.
An answer ?d queues a `q', which leaves the notices at once."
  (declare (indent 1))
  `(ob-confirm-posframe-test--with-doc ob-confirm-posframe-test--doc ,answers
     ,@body))

(defmacro ob-confirm-posframe-test--with-doc (doc answers &rest body)
  "Like `ob-confirm-posframe-test--with', in the document DOC.
ANSWERS and BODY are as there."
  (declare (indent 2))
  `(let ((queue (copy-sequence ,answers))
         (prompts nil)
         (ob-confirm-posframe-test--ran nil)
         (org-confirm-babel-evaluate t))
     (with-temp-buffer
       (org-mode)
       (insert ,doc)
       ;; The origin is only restored for a buffer shown in a window.
       (set-window-buffer nil (current-buffer))
       (goto-char (point-min))
       (search-forward "(push")
       (ob-confirm-posframe-mode 1)
       (unwind-protect
           (cl-letf (((symbol-function 'ob-confirm-posframe--read-answer)
                      (lambda (info &optional notices)
                        (push (list (nth 4 info) ob-confirm-posframe--stack
                                    (point) (current-buffer) notices
                                    (with-current-buffer
                                        ob-confirm-posframe--buffer
                                      (buffer-string)))
                              prompts)
                        (let ((answer (or (pop queue)
                                          (error "Unexpected prompt"))))
                          (when (eq answer ?d)
                            (setq unread-command-events (list ?q)))
                          answer))))
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
    ;; Latest first: babel resolves :var references before confirming.
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

;;;; Notices

(defun ob-confirm-posframe-test--lines (info &optional notices body)
  "Return the lines of the preview for INFO with NOTICES and BODY."
  (split-string (ob-confirm-posframe--render
                 info (ob-confirm-posframe--chain-create) notices body)
                "\n"))

(ert-deftest ob-confirm-posframe-test-render-notices-above-body ()
  (let* ((info (list "sh" "echo hi" nil "" "blk" 1))
         (lines (ob-confirm-posframe-test--lines
                 info (list "changed since you ran it" "second"))))
    (should (= (length lines) (+ ob-confirm-posframe-height 6)))
    (should (string-match-p "^ changed since you ran it +$" (nth 4 lines)))
    (should (eq (get-text-property 1 'face (nth 4 lines))
                'ob-confirm-posframe-notice))
    (should (string-match-p "^ second +$" (nth 5 lines)))
    (should (string-match-p "^ echo hi" (nth 6 lines)))
    (should (string-match-p "d notices" (car (last lines))))
    (should-not (string-match-p
                 "d notices"
                 (car (last (ob-confirm-posframe-test--lines info)))))))

(ert-deftest ob-confirm-posframe-test-render-notices-capped ()
  (let* ((ob-confirm-posframe-height 6)
         (info (list "sh" "a\nb\nc\nd\ne\nf" nil "" "blk" 1))
         (lines (ob-confirm-posframe-test--lines
                 info (mapcar #'number-to-string (number-sequence 1 5)))))
    (should (= (length lines) 12))
    (should (string-match-p "^ 1 " (nth 4 lines)))
    (should (string-match-p "^ 2 " (nth 5 lines)))
    (should (string-match-p "… 3 more notices, d to view" (nth 6 lines)))
    ;; The body gets the other half, with its own truncation note.
    (should (string-match-p "^ a " (nth 7 lines)))
    (should (string-match-p "… 4 more lines" (nth 9 lines)))))

(ert-deftest ob-confirm-posframe-test-notice-functions ()
  (let* ((seen nil)
         (ob-confirm-posframe-notice-functions
          (list (lambda (info) (push (nth 4 info) seen)
                  (when (equal (nth 4 info) "root") "root changed"))
                (lambda (_) (list "one" "two"))
                #'ignore)))
    (ob-confirm-posframe-test--with '(?y ?y ?y)
      (ob-confirm-posframe-test--run)
      (should (equal (nth 4 (car prompts)) '("root changed" "one" "two")))
      (should (equal (nth 4 (cadr prompts)) '("one" "two")))
      (should (string-match-p "^ root changed"
                              (nth 4 (split-string (nth 5 (car prompts))
                                                   "\n")))))
    ;; Babel resolves :var references before confirming the block.
    (should (equal (reverse seen) '("leaf" "mid" "root")))))

(defconst ob-confirm-posframe-test--diff
  "diff -u a/one b/one
--- a/one
+++ b/one
@@ -1 +1 @@
-old
+new
diff -u a/two b/two
--- a/two
+++ b/two
@@ -1 +1 @@
-was
+is
"
  "A two-file unified diff.")

(ert-deftest ob-confirm-posframe-test-d-shows-notices ()
  (let* ((detail-calls 0)
         (ob-confirm-posframe-notice-functions
          (list (lambda (_)
                  (list (ob-confirm-posframe-notice
                         "files changed: one +1/-1, two +1/-1"
                         (lambda ()
                           (cl-incf detail-calls)
                           ob-confirm-posframe-test--diff))
                        "plain notice"))))
         (shown nil)
         (record (lambda (&rest _)
                   (push (list (current-buffer) major-mode
                               (eq (window-buffer) (current-buffer))
                               (buffer-string))
                         shown))))
    (advice-add 'recursive-edit :before record)
    (unwind-protect
        (ob-confirm-posframe-test--with '(?d ?y ?y ?y)
          (let ((origin (current-buffer)))
            (should (equal (ob-confirm-posframe-test--run) 3))
            ;; d, then the same prompt again, then mid and root.
            (should (equal (mapcar #'car (reverse prompts))
                           '("leaf" "leaf" "mid" "root")))
            (should (eq (window-buffer) origin))))
      (advice-remove 'recursive-edit record))
    (should (= detail-calls 1))
    (should (= 1 (length shown)))
    (pcase-let ((`(,buf ,mode ,selected ,text) (car shown)))
      (should (equal (buffer-name buf) ob-confirm-posframe--notice-buffer))
      (should (eq mode 'diff-mode))
      (should selected)
      (should (string-prefix-p "files changed: one +1/-1, two +1/-1\ndiff -u"
                               text))
      (should (string-match-p "^\\+is$" text))
      (should (string-match-p "^plain notice$" text))
      (should-not (get-buffer-window buf)))))

(ert-deftest ob-confirm-posframe-test-d-needs-notices ()
  (let (choices)
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_ chars) (setq choices chars) ?y)))
      (ob-confirm-posframe--read-answer '("sh" "" nil "" "b") nil)
      (should-not (memq ?d choices))
      (ob-confirm-posframe--read-answer '("sh" "" nil "" "b") '("n"))
      (should (memq ?d choices)))))

;;;; Noweb preview

(defconst ob-confirm-posframe-test--noweb-doc
  "#+name: root
#+begin_src sh :noweb yes
echo start
# <<plain>>
echo <<side()>>
#+end_src

#+name: plain
#+begin_src sh :noweb yes
echo plain
echo <<side(x=1)>>
#+end_src

#+name: side
#+begin_src emacs-lisp :var x=0
(push \"side\" ob-confirm-posframe-test--ran)
\"side ran\"
#+end_src
"
  "A block referring to `plain' and calling `side', which `plain' calls too.")

(ert-deftest ob-confirm-posframe-test-expand-noweb-never-runs-calls ()
  (let ((ob-confirm-posframe-test--ran nil)
        (ob-confirm-posframe-expand-noweb t)
        ;; Nothing would stop a call from running.
        (org-confirm-babel-evaluate nil))
    (with-temp-buffer
      (org-mode)
      (insert ob-confirm-posframe-test--noweb-doc)
      (goto-char (point-min))
      (org-babel-next-src-block)
      (let ((expanded (ob-confirm-posframe--expanded-body
                       (org-babel-get-src-block-info))))
        (should (equal (car expanded)
                       "echo start\n# echo plain\n# echo <<side(x=1)>>\necho <<side()>>"))
        (should (equal (cdr expanded) '("side(x=1)" "side()")))))
    (should (null ob-confirm-posframe-test--ran))))

(ert-deftest ob-confirm-posframe-test-expand-noweb-off ()
  (with-temp-buffer
    (org-mode)
    (insert ob-confirm-posframe-test--noweb-doc)
    (goto-char (point-min))
    (org-babel-next-src-block)
    (let ((info (org-babel-get-src-block-info)))
      (let ((ob-confirm-posframe-expand-noweb nil))
        (should-not (ob-confirm-posframe--expanded-body info)))
      (let ((ob-confirm-posframe-expand-noweb t))
        (setf (alist-get :noweb (nth 2 info)) "no")
        (should-not (ob-confirm-posframe--expanded-body info))))))

(ert-deftest ob-confirm-posframe-test-expand-noweb-in-prompt ()
  (let ((ob-confirm-posframe-expand-noweb t))
    (ob-confirm-posframe-test--with-doc ob-confirm-posframe-test--noweb-doc '(?n)
      (should (null (ob-confirm-posframe-test--run)))
      (should (= 1 (length prompts)))
      (let ((preview (nth 5 (car prompts))))
        (should (string-match-p "^ noweb call: side(x=1) *$" preview))
        (should (string-match-p "^ noweb call: side() *$" preview))
        (should (string-match-p "^ # echo plain *$" preview))
        (should (string-match-p "^ echo <<side()>> *$" preview)))
      (should (equal (nth 4 (car prompts))
                     '("noweb call: side(x=1)" "noweb call: side()"))))
    (should (null ob-confirm-posframe-test--ran))))

;;;; Advice depth

(defconst ob-confirm-posframe-test--resolve-doc
  "#+name: check-disk
#+begin_src emacs-lisp :resolve-with pick-host
(push \"check-disk\" ob-confirm-posframe-test--ran)
#+end_src

#+name: pick-host
#+begin_src emacs-lisp
(push \"pick-host\" ob-confirm-posframe-test--ran)
#+end_src
"
  "A block whose header names another block to run before it.")

(defun ob-confirm-posframe-test--stand-in (fn &optional arg info &rest rest)
  "Stand-in for an advice that runs a block to set up its own.
Before calling FN with ARG, INFO and REST, run the block named
by the `:resolve-with' header, as a dynamic target would."
  (let* ((own (or info (org-babel-get-src-block-info 'no-eval)))
         (other (cdr (assq :resolve-with (nth 2 own)))))
    (when other
      (save-excursion
        (org-babel-goto-named-src-block other)
        (org-babel-execute-src-block)))
    (apply fn arg info rest)))

(defmacro ob-confirm-posframe-test--with-stand-in (answers &rest body)
  "Run BODY in the resolve document with the stand-in advice.
ANSWERS are as in `ob-confirm-posframe-test--with'.  The stand-in
goes on after the mode, at the default depth, so only the depth
keeps it inside."
  (declare (indent 1))
  `(ob-confirm-posframe-test--with-doc ob-confirm-posframe-test--resolve-doc
       ,answers
     (advice-add 'org-babel-execute-src-block :around
                 #'ob-confirm-posframe-test--stand-in)
     (unwind-protect (progn ,@body)
       (advice-remove 'org-babel-execute-src-block
                      #'ob-confirm-posframe-test--stand-in))))

(ert-deftest ob-confirm-posframe-test-advice-is-outermost ()
  (ob-confirm-posframe-test--with-stand-in nil
    (let (order)
      (advice-mapc (lambda (f _) (push f order)) 'org-babel-execute-src-block)
      (should (eq (car (last order)) #'ob-confirm-posframe--execute-advice)))))

(ert-deftest ob-confirm-posframe-test-inner-block-shows-chain ()
  (ob-confirm-posframe-test--with-stand-in '(?y ?y)
    (ob-confirm-posframe-test--run)
    (setq prompts (reverse prompts))
    (should (equal (mapcar #'car prompts) '("pick-host" "check-disk")))
    (should (equal (nth 1 (car prompts)) '("pick-host" "check-disk")))
    (should (string-match-p "^ check-disk › pick-host *$"
                            (nth 5 (car prompts))))
    (should (equal ob-confirm-posframe-test--ran
                   '("check-disk" "pick-host")))))

(ert-deftest ob-confirm-posframe-test-inner-no-cancels-outer ()
  (ob-confirm-posframe-test--with-stand-in '(?n)
    (should (null (ob-confirm-posframe-test--run)))
    (should (= 1 (length prompts)))
    (should (null ob-confirm-posframe-test--ran))))

(ert-deftest ob-confirm-posframe-test-inner-bang-approves-outer ()
  (ob-confirm-posframe-test--with-stand-in '(?!)
    (ob-confirm-posframe-test--run)
    (should (= 1 (length prompts)))
    (should (equal ob-confirm-posframe-test--ran
                   '("check-disk" "pick-host")))))

(provide 'ob-confirm-posframe-test)
;;; ob-confirm-posframe-test.el ends here
