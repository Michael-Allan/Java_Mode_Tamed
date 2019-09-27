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

(require 'cc-mode); It includes the definition of Java mode.



(define-derived-mode java-mode-tamed java-mode
  "Java" "A tamer, more controllable Java mode"
  (set 'font-lock-defaults; ‘It automatically becomes buffer-local when set.’ [FLB]
       ;; Following are the alternative values of `font-lock-keywords`, each ordered
       ;; according to the value of `font-lock-maximum-decoration` that selects it.
       '((jtam-keywords-1 jtam-keywords-1 jtam-keywords-2 jtam-keywords-3))))
         ;;;     nil or 0,              1,              2,         t or 3      [MD]



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  D e c l a r a t i o n s   i n   l e x i c a l   o r d e r
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(defun jtam-keywords-1()
  "Returns the value of `font-lock-keywords` to use for minimal highlighting."
  'java-font-lock-keywords-1)



(defun jtam-keywords-2()
  "Returns the value of `font-lock-keywords` to use for fast, normal highlighting."
  (append
   java-font-lock-keywords-2
   jtam-specific-keywords))



(defun jtam-keywords-3()
  "Returns the value of `font-lock-keywords` to use for accurate, normal highlighting."
  (append
   java-font-lock-keywords-3
   jtam-specific-keywords))



(defface jtam-modifier-keyword-face
  `((default . (:inherit font-lock-keyword-face)))
  "The face for a keyword-form modifier of a class, interface, constructor, method or field declaration;
any Java modifier, that is, except an annotation modifier.")



(defvar jtam-modifier-keyword-pattern
  (concat
  "\\<abstract\\|final\\|native"
  "\\|p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)"
  "\\|s\\(?:t\\(?:atic\\|rictfp\\)\\|ynchronized\\)"
  "\\|transient\\|volatile\\>")
  "The regexp pattern of a modifier keyword.  See `jtam-modifier-keyword-face`.")



(defconst jtam-specific-keywords
  (list

   ;; ═════════════════
   ;; Modifier keywords
   ;; ═════════════════
   (cons; Refontify them.
    (lambda( limit )
      (catch 'result
        (while (< (point) limit)
          (let ((face (get-text-property (point) 'face))
                (face-limit (next-single-property-change (point) 'face (current-buffer) limit)))
            (when (and (eq face 'font-lock-keyword-face)
                       (re-search-forward jtam-modifier-keyword-pattern face-limit t))
              (throw 'result t))
            (goto-char face-limit)))
        (throw 'result nil)))
    '(0 'jtam-modifier-keyword-face t)))
  "Elements of `jtam-keywords-2` and `jtam-keywords-3` that are specific to Java mode tamed.")



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(provide 'java-mode-tamed); Providing these features of `java-mode-tamed.el` for all who `require` them.


;; NOTES
;; ─────
;;   FLB  Font lock basics.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html
;;
;;   MD · How the value of `font-lock-maximum-decoration` governs the value of `font-lock-keywords` is
;;        documented inconsistently by Emacs.  See instead the `font-lock-choose-keywords` function
;;        of `http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/font-lock.el`, which confirms
;;        `https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html`.


                                       ;;; Copyright © 2019 Michael Allan and contributors.  Licence MIT.
