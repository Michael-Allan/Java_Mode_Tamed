;; The definition of `java-mode-tamed` - a tamer, more controllable Java mode.
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
;;       http://reluk.ca/.emacs
;;       http://reluk.ca/.Xresources (pending)
;;
;;
;; NOTES  (see at bottom)
;; ─────


(define-derived-mode java-mode-tamed java-mode
  "Java" "A tamer, more controllable Java mode"
  (set 'font-lock-defaults; ‘It automatically becomes buffer-local when set.’ [FLB]
       ;; Following are the alternative values of `font-lock-keywords`, each ordered
       ;; according to the value of `font-lock-maximum-decoration` that selects it.  [MD]
       '((jtam-fontifiers-1 jtam-fontifiers-1 jtam-fontifiers-2 jtam-fontifiers-3))))
         ;;;       nil or 0,                1,                2,           t or 3



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  D e c l a r a t i o n s   i n   l e x i c a l   o r d e r
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(defun jtam-fontifiers-1()
  "Returns the value of `font-lock-keywords` to use for minimal highlighting."
  'java-font-lock-keywords-1)



(defun jtam-fontifiers-2()
  "Returns the value of `font-lock-keywords` to use for fast, normal highlighting."
  (append
   java-font-lock-keywords-2
   jtam-specific-fontifiers))



(defun jtam-fontifiers-3()
  "Returns the value of `font-lock-keywords` to use for accurate, normal highlighting."
  (append
   java-font-lock-keywords-3
   jtam-specific-fontifiers))



(defface jtam-modifier-keyword-face
  `((t . (:inherit font-lock-keyword-face)))
  "The face for a modifier keyword.  See `jtam-modifier-keyword-pattern`.")



(defvar jtam-modifier-keyword-pattern
  (concat
  "\\<abstract\\|final\\|native"
  "\\|p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)"
  "\\|s\\(?:t\\(?:atic\\|rictfp\\)\\|ynchronized\\)"
  "\\|transient\\|volatile\\>")
  "The regexp pattern of keyword-form modifier in a class,
interface, constructor, method or field declaration; for any
modifier, that is, except an annotation modifier.")



(defconst jtam-specific-fontifiers
  (list

   ;; Modifier keyword
   ;; ────────────────
   (cons; Refontify it using `jtam-modifier-keyword-face`.
    (lambda( limit )
      (catch 'result
        (while (< (point) limit)
          (let ((face (get-text-property (point) 'face))
                (face-end (next-single-property-change (point) 'face (current-buffer) limit)))
            (when (and (eq face 'font-lock-keyword-face)
                       (re-search-forward jtam-modifier-keyword-pattern face-end t))
              (throw 'result t))
            (goto-char face-end)))
        (throw 'result nil)))
    '(0 'jtam-modifier-keyword-face t))

   ;; Type identifier
   ;; ───────────────
   (list; Refontify it using either `jtam-type-declaration-face` or  `jtam-type-reference-face`.
    (lambda( limit )
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
    '(1 'jtam-type-declaration-face t t) '(2 'jtam-type-reference-face t t)))

  "Elements of `jtam-fontifiers-2` and `jtam-fontifiers-3` that are specific to Java mode tamed.")



(defface jtam-type-declaration-face
  `((t . (:inherit font-lock-type-face)))
  "The face for the type identifier in a class or interface declaration.")



(defface jtam-type-reference-face
  `((t . (:inherit font-lock-type-face)))
  "The face for the type identifier in a class or interface reference.")



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(provide 'java-mode-tamed); Providing these features of `java-mode-tamed.el` for all who `require` them.


;; NOTES
;; ─────
;;   FLB  Font lock basics.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html
;;
;;   MD · How the value of `font-lock-maximum-decoration` governs the value of `font-lock-keywords`
;;        is documented inconsistently by Emacs.  See instead the `font-lock-choose-keywords` function
;;        of `http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/font-lock.el`.  It verifies the cor-
;;        rectness of `https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html`.


                                       ;;; Copyright © 2019 Michael Allan and contributors.  Licence MIT.
