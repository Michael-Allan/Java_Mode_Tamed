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


  (eval-when-compile (require 'cl-lib))

  (defvar c-maybe-decl-faces)
  (defvar java-font-lock-keywords-2)
  (defvar java-font-lock-keywords-3)
    ;;; Suppressing sporadic compiler warnings ‘reference to free variable’
    ;;; or ‘assignment to free variable’.



  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════
  ;;  D e c l a r a t i o n s   i n   l e x i c o g r a p h i c   o r d e r
  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════


  (defgroup java-mode-tamed nil
    "A tamer, more controllable Java mode"
    :group 'languages :group 'faces
    :prefix "jtam-"
    :link '(url-link "http://reluk.ca/project/Java/Emacs/"))



  (defun jtam-faces-are-equivalent( f1 f2 )
    "Answers whether the given faces should be treated as equivalent by the underlying (Java mode) code."
    (eq (jtam-original f1) (jtam-original f2)))



  (defun jtam-fontifiers-1 ()
    "Returns the value of \\=`font-lock-keywords\\=` to use for minimal highlighting."
    'java-font-lock-keywords-1)



  (defun jtam-fontifiers-2 ()
    "Returns the value of \\=`font-lock-keywords\\=` to use for fast, normal highlighting."
    (append
     java-font-lock-keywords-2
     jtam-specific-fontifiers))



  (defun jtam-fontifiers-3 ()
    "Returns the value of \\=`font-lock-keywords\\=` to use for accurate, normal highlighting."
    (append
     java-font-lock-keywords-3
     jtam-specific-fontifiers))



  (defun jtam-initialize ()
    "Finishes the initialization of \\=`java-mode-tamed\\=` and sets `is-initialized`.  Call once only."
    (cl-assert (not is-initialized))
    (setq is-initialized t)

    ;; Monkey patch the underlying (Java mode) code
    ;; ────────────────────────────────────────────
    (define-error 'jtam-x "Monkey patch failure")
    (condition-case x
        (let ((s 'c-font-lock-<>-arglists); The symbol of the function.
              original-was-compiled)
          (when (not (functionp s)) (signal 'jtam-x `("No such function loaded" ,s)))
          (let* ((fl (symbol-file s)); File whence the function was loaded, probably a compiled `.elc`.
                 (f (locate-library (concat (file-name-base fl) ".el") t)); Related source file `.el`.
                 x y)
            (when (not f) (signal 'jtam-x `("File has no corresponding `.el` source file" ,fl)))
            (with-temp-buffer
              (insert-file-contents f)
              (if (not (re-search-forward (concat "^(\\s-*defun\\s-+" (symbol-name s) "\\s-*(") nil t))
                  (signal 'jtam-x `("Function declaration not found in source file" ,s ,f))
                (setq x (match-beginning 0))
                (setq y (if (re-search-forward "^(" nil 0) (1- (point)) (point)))
                    ;;; To just before the next top-level declaration, that is, or the end of the buffer.
                (narrow-to-region x y); Narrowing the buffer to the function declaration alone.
                (goto-char (point-min))
                (while (re-search-forward "(\\s-*\\(eq\\)\\s-+[^)]+?-face" nil t)
                  (replace-match "jtam-faces-are-equivalent"  t t nil 1)); Patching the declaration.
                (setq original-was-compiled (byte-code-function-p (symbol-function s)))
                (eval-buffer)))); Redefining the function to the patched version.
          (when original-was-compiled; Then recompile the redefined function.
            (unless (byte-compile s)
              (display-warning 'java-mode-tamed
                               (format "Unable to recompile monkey-patched function `%S`" s)))))
      (jtam-x (display-warning 'java-mode-tamed (error-message-string x) :error))))



  (defface jtam-modifier-keyword
    `((t . (:inherit font-lock-keyword-face))); [RPI]
    "The face for a modifier keyword.  See \\=`jtam-modifier-keyword-pattern\\=`."
    :group 'java-mode-tamed)



  (defconst jtam-modifier-keyword-pattern
    (concat
     "\\<abstract\\|final\\|native"
     "\\|p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)"
     "\\|s\\(?:t\\(?:atic\\|rictfp\\)\\|ynchronized\\)"
     "\\|transient\\|volatile\\>")
    "The regexp pattern of a keyword-form modifier in a class, interface, constructor,
  method or field declaration; of any modifier, that is, except an annotation modifier.")



  (defun jtam-original( face )
    "If \\=`face\\=` is used by \\=`java-mode-tamed\\=` to replace a Java mode face,
then this function returns the original face it replaces; otherwise it returns \\=`face\\=`."
    (if (string-prefix-p "jtam-" (symbol-name face))
        (face-attribute face :inherit nil); [RPI]
      face))



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

    "Elements of \\=`jtam-fontifiers-2\\=` and \\=`jtam-fontifiers-3\\=`
that are specific to \\=`java-mode-tamed\\=`.")



  (defface jtam-type-declaration
    `((t . (:inherit font-lock-type-face))); [RPI]
    "The face for the type identifier in a class or interface declaration."
    :group 'java-mode-tamed)



  (defface jtam-type-reference
    `((t . (:inherit font-lock-type-face))); [RPI]
    "The face for the type identifier in a class or interface reference."
    :group 'java-mode-tamed)



  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════


  (define-derived-mode java-mode-tamed java-mode
    "Java" "A tamer, more controllable Java mode" :group 'java-mode-tamed
    (when (not is-initialized) (jtam-initialize))
    (set 'c-maybe-decl-faces (append c-maybe-decl-faces '('jtam-modifier-keyword
                                                          'jtam-type-declaration
                                                          'jtam-type-reference)))
    (cl-assert (local-variable-p 'c-maybe-decl-faces))
    (set 'font-lock-defaults
         ;; Following are the alternative values of `font-lock-keywords`, each ordered
         ;; according to the value of `font-lock-maximum-decoration` that selects it.  [MD]
         '((jtam-fontifiers-1 jtam-fontifiers-1 jtam-fontifiers-2 jtam-fontifiers-3)))
           ;;;       nil or 0,                1,                2,           t or 3
    (cl-assert (local-variable-p 'font-lock-defaults)))



  (provide 'java-mode-tamed)); Providing these features
    ;;; of `java-mode-tamed.el` for any who `require` them.



;; NOTES
;; ─────
;;   MD · How the value of `font-lock-maximum-decoration` governs the value of `font-lock-keywords`
;;        is documented inconsistently by Emacs.  See instead the `font-lock-choose-keywords` function
;;        of `http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/font-lock.el`.  It verifies the cor-
;;        rectness of `https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html`.
;;
;;   RPI  Every replacement face inherits from the face it replaces.  Function `jtam-original`
;;        depends on this.


                                       ;;; Copyright © 2019 Michael Allan and contributors.  Licence MIT.
