;; The definition of `java-mode-tamed` - a tamer, more controllable                 -*- lexical-binding: t; -*-
;; Java mode.
;;
;; USAGE
;; ─────
;;   In your initialization file:
;;
;;      (require 'java-mode-tamed)
;;      (add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode-tamed))
;;
;;   Working example:
;;
;;       http://reluk.ca/sys/computer/Havoc/etc/emacs/user-initialization.el
;;       http://reluk.ca/.Xresources (pending)
;;
;;
;; NOTES  (see at bottom)
;; ─────


(let (is-initialized)


  (define-derived-mode java-mode-tamed java-mode
    "Java" "A tamer, more controllable Java mode" :group 'java-mode-tamed
    (jtam-ensure-initialization)
    (set 'font-lock-defaults; ‘It automatically becomes buffer-local when set.’ [FLB]
         ;; Following are the alternative values of `font-lock-keywords`, each ordered
         ;; according to the value of `font-lock-maximum-decoration` that selects it.  [MD]
         '((jtam-fontifiers-1 jtam-fontifiers-1 jtam-fontifiers-2 jtam-fontifiers-3))))
           ;;;       nil or 0,                1,                2,           t or 3



  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════
  ;;  P r e l i m i n a r y   d e c l a r a t i o n s
  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════


  (defvar java-font-lock-keywords-2); [FV]
  (defvar java-font-lock-keywords-3)



  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════
  ;;  D e c l a r a t i o n s   i n   l e x i c o g r a p h i c   o r d e r
  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════


  (defgroup java-mode-tamed nil
    "A tamer, more controllable Java mode"
    :group 'languages :group 'faces
    :prefix "jtam-"
    :link '(url-link "http://reluk.ca/project/Java/Emacs/"))



  (defun jtam-ensure-initialization ()
    "Finishes any remaining initialization of `java-mode-tamed`."
    (when (not is-initialized)
      (setq is-initialized t)
      (require 'cc-mode)
      (define-error 'jtam-x "Runtime patch failure")
      (condition-case x
          (let ((s 'c-font-lock-<>-arglists)); The symbol of the function.
            (when (not (functionp s)) (signal 'jtam-x `("No such function loaded" ,s)))
            (let* ((fl (symbol-file s)); File whence the function was loaded, probably a compiled `.elc`.
                   (f (locate-library (concat (file-name-base fl) ".el") t))); Related source file `.el`.
              (when (not f) (signal 'jtam-x `("File has no corresponding source" ,fl)))
          ;;; (message (prin1-to-string f)); TEST
              ))
        (jtam-x (display-warning 'java-mode-tamed (error-message-string x) :error)))))



  (defun jtam-fontifiers-1 ()
    "Returns the value of `font-lock-keywords` to use for minimal highlighting."
    'java-font-lock-keywords-1)



  (defun jtam-fontifiers-2 ()
    "Returns the value of `font-lock-keywords` to use for fast, normal highlighting."
    (append
     java-font-lock-keywords-2
     jtam-specific-fontifiers))



  (defun jtam-fontifiers-3 ()
    "Returns the value of `font-lock-keywords` to use for accurate, normal highlighting."
    (append
     java-font-lock-keywords-3
     jtam-specific-fontifiers))



  (defface jtam-modifier-keyword
    `((t . (:inherit font-lock-keyword-face)))
    "The face for a modifier keyword.  See `jtam-modifier-keyword-pattern`."
    :group 'java-mode-tamed)



  (defconst jtam-modifier-keyword-pattern
    (concat
     "\\<abstract\\|final\\|native"
     "\\|p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)"
     "\\|s\\(?:t\\(?:atic\\|rictfp\\)\\|ynchronized\\)"
     "\\|transient\\|volatile\\>")
    "The regexp pattern of a keyword-form modifier in a class, interface, constructor,
  method or field declaration; of any modifier, that is, except an annotation modifier.")



  (defconst jtam-specific-fontifiers
    (list

     ;; Modifier keyword
     ;; ────────────────
     (cons; Refontify it using face `jtam-modifier-keyword`.
      (lambda (limit)
        (catch 'result
          (while (< (point) limit)
            (let ((face (get-text-property (point) 'face))
                  (face-end (next-single-property-change (point) 'face (current-buffer) limit)))
              (when (and (eq face 'font-lock-keyword-face)
                         (re-search-forward jtam-modifier-keyword-pattern face-end t))
                (throw 'result t))
              (goto-char face-end)))
          (throw 'result nil)))
      '(0 'jtam-modifier-keyword t))

     ;; Type identifier
     ;; ───────────────
     (list; Refontify it using either `jtam-type-declaration` or  `jtam-type-reference` face.
      (lambda (limit)
        (catch 'result
          (while (< (point) limit)
            (let ((face (get-text-property (point) 'face))
                  (face-end (next-single-property-change (point) 'face (current-buffer) limit)))
              (when (eq face 'font-lock-type-face)
                (let ((face-beg (point)))
                  (when (re-search-backward "\\(?:\\`\\|\\s-\\)\\(\\S-+\\)" nil t)
                    (let ((pre (match-string 1))); The preceding string of non-whitespace characters.
                      (when (or (string= pre "class") (string= pre "enum") (string= pre "interface"))
                        (set-match-data; Capturing the identifier as group 1.
                         (list face-beg face-end face-beg face-end (current-buffer)))
                        (throw 'result t))))
                  (set-match-data; Capturing the identifier as group 2.
                   (list face-beg face-end nil nil face-beg face-end (current-buffer)))
                  (throw 'result t)))
              (goto-char face-end)))
          (throw 'result nil)))
      '(1 'jtam-type-declaration t t) '(2 'jtam-type-reference t t)))

    "Elements of `jtam-fontifiers-2` and `jtam-fontifiers-3` that are specific to `java-mode-tamed`.")



  (defface jtam-type-declaration
    `((t . (:inherit font-lock-type-face)))
    "The face for the type identifier in a class or interface declaration."
    :group 'java-mode-tamed)



  (defface jtam-type-reference
    `((t . (:inherit font-lock-type-face)))
    "The face for the type identifier in a class or interface reference."
    :group 'java-mode-tamed)



  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════


  (provide 'java-mode-tamed)); Providing these features
    ;;; of `java-mode-tamed.el` for any who `require` them.


;; NOTES
;; ─────
;;   FLB  Font lock basics.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html
;;
;;   FV · Suppressing sporadic compiler warnings ‘reference to free variable’
;;        or ‘assignment to free variable’.
;;
;;   MD · How the value of `font-lock-maximum-decoration` governs the value of `font-lock-keywords`
;;        is documented inconsistently by Emacs.  See instead the `font-lock-choose-keywords` function
;;        of `http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/font-lock.el`.  It verifies the cor-
;;        rectness of `https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html`.


                                       ;;; Copyright © 2019 Michael Allan and contributors.  Licence MIT.
