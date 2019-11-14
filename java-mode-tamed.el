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


(let ()


  (eval-when-compile
    (require 'cc-mode)
    (require 'cl-lib))

  (defvar c-maybe-decl-faces)
  (defvar java-font-lock-keywords-2)
  (defvar java-font-lock-keywords-3)
    ;;; Suppressing sporadic compiler warnings ‘reference to free variable’
    ;;; or ‘assignment to free variable’.



  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════
  ;;  P r e l i m i n a r y   d e c l a r a t i o n s
  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════


  (defvar-local jtam--is-level-3 nil); An in-buffer cache of this boolean flag.  It works only because any
    ;;; ‘customization of `font-lock-maximum-decoration` should be done *before* the file is visited’.
    ;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html



  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════
  ;;  D e c l a r a t i o n s   i n   l e x i c o g r a p h i c   o r d e r
  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════


  (defgroup java-mode-tamed nil
    "A tamer, more controllable Java mode"
    :group 'languages :group 'faces
    :prefix "jtam-"
    :link '(url-link "http://reluk.ca/project/Java/Emacs/"))



  (defun jtam-c-lisp-top-declaration-end ()
    "Returns the position just before the next top-level ELisp declaration, if any,
else `point-max`.  Leaves point indeterminate.  For use on buffers that contain
Java mode (CC mode) source code."
    (if (re-search-forward "^(" nil t) (1- (point)) (point-max)))



  (defun jtam--c/put-type-face (range)
    "Called from a monkey patch over the Java-mode code, this function
overrides Java mode’s application of ‘font-lock-type-face’ in order
to stabilize the facing of type parameter lists.  RANGE is a cons cell." ; [TP, TA]
    ;; Without this patched override the corresponding refontifications of `jtam-specific-fontifiers`
    ;; alternately appear and disappear.  This occurs because Java mode applies `font-lock-type-face`
    ;; using a mechanism of its own, outside of Font Lock, which puts the two in an endless tug of war.
    (let ((beg (car range))
          (end (cdr range)))
      (if (and jtam--is-level-3
               (eq major-mode 'java-mode-tamed)
               (or (let ((p beg) c)
                     (while; Set `c` to the first non-whitespace character before `range`.
                         (progn (setq c (char-before p))
                                (char-equal (char-syntax c) ?\s))
                       (setq p (1- p)))
                     (or (char-equal c ?<)    ; A leading delimiter ‘<’,
                         (char-equal c ?&)    ; additional bound operator,
                         (char-equal c ?,)))  ; or separator ‘,’ in a type parameter list.
                   (let ((p end) c)
                     (while; Set `c` to the first non-whitespace character after `range`.
                         (progn (setq c (char-after p))
                                (char-equal (char-syntax c) ?\s))
                       (setq p (1+ p)))
                     (or (char-equal c ?>)    ; A trailing delimiter ‘>’,
                         (char-equal c ?&)    ; additional bound operator,
                         (char-equal c ?,))))); or separator ‘,’.
          (progn
            (unless; Unless already region `beg`…`end` is faced `jtam-type-parameter-declaration`. [SF]
                (and (eq (get-text-property beg 'face) 'jtam-type-parameter-declaration)
                     (>= (next-single-property-change beg 'face (current-buffer) end) end))
              (c-put-font-lock-face beg end 'jtam--type-reference-in-parameter-list))); [RP]
                ;;; Later this facing may be overridden and corrected by `jtam-specific-fontifiers`,
                ;;; refacing it as `jtam-type-parameter-declaration`.
        (c-put-font-lock-face beg end 'font-lock-type-face))))



  (defvar jtam--early-initialization-was-begun nil)



  (defun jtam-faces-are-equivalent (f1 f2); [RP]
    "Answers whether F1 and F2 (face symbols) should be treated as equivalent
by the underlying (Java mode) code."
    (eq (jtam-untamed-face f1) (jtam-untamed-face f2)))



  (defun jtam-fontifiers-1 ()
    "Returns the value of ‘font-lock-keywords’ to use for minimal highlighting."
    'java-font-lock-keywords-1)



  (defun jtam-fontifiers-2 ()
    "Returns the value of ‘font-lock-keywords’ to use for fast, normal highlighting."
    'java-font-lock-keywords-2)



  (defun jtam-fontifiers-3 ()
    "Returns the value of ‘font-lock-keywords’ to use
for accurate, normal highlighting."
    (append
     java-font-lock-keywords-3
     jtam-specific-fontifiers))



  (defun jtam-is-modifier-keyword (s)
    "Answers whether string S is a Java modifier in keyword form.
See face ‘jtam-modifier-keyword’."
    ;;       `ClassModifier` https://docs.oracle.com/javase/specs/jls/se13/html/jls-8.html#jls-8.1.1
    ;;   `InterfaceModifier` https://docs.oracle.com/javase/specs/jls/se13/html/jls-9.html#jls-9.1.1
    ;;      `MethodModifier` https://docs.oracle.com/javase/specs/jls/se13/html/jls-8.html#jls-8.4.3
    ;; `ConstructorModifier` https://docs.oracle.com/javase/specs/jls/se13/html/jls-8.html#jls-8.8.3
    ;;       `FieldModifier` https://docs.oracle.com/javase/specs/jls/se13/html/jls-8.html#jls-8.3.1
    (or (jtam-is-type-modifier-keyword s)
        (string= s "synchronized")
        (string= s "volatile")
        (string= s "transient")
        (string= s "native")))



  (defun jtam-is-type-declarant (s)
    "Answers whether string S is the principle keyword of a type declaration."
    (or (string= s "class")
        (string= s "interface")
        (string= s "enum")))



  (defun jtam-is-type-modifier-keyword (s)
    "Answers whether string S is a type declaration modifier in keyword form,
e.g. as opposed to annotation form."
    ;;     `ClassModifier` https://docs.oracle.com/javase/specs/jls/se13/html/jls-8.html#jls-8.1.1
    ;; `InterfaceModifier` https://docs.oracle.com/javase/specs/jls/se13/html/jls-9.html#jls-9.1.1
    (or (string= s "public")
        (string= s "final")
        (string= s "static")
        (string= s "private")
        (string= s "abstract")
        (string= s "protected")
        (string= s "srictfp")))



  (defvar jtam--late-initialization-was-begun nil)



  (defun jtam-message (format-string &rest arguments)
    "Calls function ‘message’ without translation of embedded \\=`\\=`\\=`
and \\=`\\='\\=` quotes."
    (message "%s" (format format-string arguments)))



  (defface jtam-modifier-keyword; [MDF]
    `((t . (:inherit font-lock-keyword-face))); [UAF, RP]
    "The face for a keyword-form modifier in a class, interface, method,
constructor or field declaration; any modifier, that is, except an annotation
modifier.  Use it customize the appearance of these keywords, e.g. to give them
less prominence than other, more important keywords."
    :group 'java-mode-tamed)



  (defconst jtam-name-character-set "[:alnum:]_$"
    "The set of characters from which a Java identifier may be formed.")
    ;;; https://docs.oracle.com/javase/specs/jls/se13/html/jls-3.html#jls-3.8



  (defun jtam--patch (source-file source-base-name function-symbol patch-function)
    "Monkey patches function FUNCTION-SYMBOL of file SOURCE-FILE (a string,
which has the given BASE-NAME) using the named PATCH-FUNCTION.  The patch
function must return t on success, nil on failure."
    (unless (functionp function-symbol)
      (signal 'jtam-x `("No such function loaded" ,function-symbol)))
    (let ((load-file (symbol-file function-symbol)))
      (unless (string= (file-name-base load-file) source-base-name)
        (signal 'jtam-x `("Function loaded from file of base name contradictory to source file"
                          ,function-symbol ,load-file ,source-file))))
    (goto-char (point-min))
    (unless (re-search-forward
             (concat "^(defun\\s-+" (symbol-name function-symbol) "\\s-*(") nil t)
      (signal 'jtam-x `("Function declaration not found in source file"
                        ,function-symbol ,source-file)))
    (narrow-to-region (match-beginning 0) (jtam-c-lisp-top-declaration-end))
      ;;; Narrowing the temporary patch buffer to the function declaration alone.
    (goto-char (point-min))
    (unless (funcall patch-function); Patching the declaration.
      (signal 'jtam-x `("Patch failed to apply" ,function-symbol)))
    (let ((original-was-compiled (byte-code-function-p (symbol-function function-symbol))))
      (eval-buffer); Redefining the function to the patched version.
  ;;; (delete-region point-min point-max); Removing the declaration, in case it speeds later patching.
  ;;;;;; Or might the deletion time exceed the time saved?
      (widen)
      (when original-was-compiled; Then recompile the redefined function.
        (run-with-idle-timer; Recompile it during idle time.  This might improve initial load times,
         1.3 nil            ; though early timing tests (with a single patch) showed no such effect.
         (lambda ()
           (unless (byte-compile function-symbol)
             (jtam-message "(java-mode-tamed): Unable to recompile monkey-patched function `%s`"
                           (symbol-name function-symbol))))))))



  (defconst jtam-specific-fontifiers
    (list

     ;; ════════════════
     ;; Modifier keyword
     ;; ════════════════
     (cons; Refontify each using face `jtam-modifier-keyword`. [RP]
      (lambda (limit)
        (catch 'to-refontify
          (while (< (point) limit)
            (let* ((match-beg (point))
                   (face (get-text-property match-beg 'face))
                   (match-end (next-single-property-change match-beg 'face (current-buffer) limit)))
              (goto-char match-end)
              (when (and (eq face 'font-lock-keyword-face)
                         (jtam-is-modifier-keyword (buffer-substring-no-properties match-beg match-end)))
                (set-match-data (list match-beg match-end (current-buffer)))
                (throw 'to-refontify t))))
          (throw 'to-refontify nil)))
      '(0 'jtam-modifier-keyword t))

     ;; ═════════
     ;; Type name  [↑T]
     ;; ═════════
     (list; Refontify each using either `jtam-type-declaration` or  `jtam-type-reference` face. [RP]
      (lambda (limit)
        (catch 'to-refontify
          (while (< (point) limit)
            (let* ((match-beg (point))
                   (face (get-text-property match-beg 'face))
                   (match-end (next-single-property-change match-beg 'face (current-buffer) limit)))
              (when (eq face 'font-lock-type-face)

                ;; Either declaring a type
                ;; ────────────────
                (when; Keyword `class`, `enum` or `interface` directly precedes the type name.
                    (and (< (skip-syntax-backward "-") 0); [QSB, FC]
                         (let ((p (point)))
                           (and (< (skip-chars-backward jtam-name-character-set) 0)
                                (jtam-is-type-declarant (buffer-substring-no-properties (point) p)))))
                  (set-match-data; Capturing the already fontified name as group 1.
                   (list match-beg match-end match-beg match-end (current-buffer)))
                  (goto-char match-end)
                  (throw 'to-refontify t))

                ;; Or merely referring to one
                ;; ───────────────────
                (set-match-data; Capturing the name instead as group 2.
                 (list match-beg match-end nil nil match-beg match-end (current-buffer)))
                (goto-char match-end)
                (throw 'to-refontify t))

              (goto-char match-end)))
          (throw 'to-refontify nil)))
      '(1 'jtam-type-declaration t t) '(2 'jtam-type-reference t t))

     (cons; Fontify type declaration names missed by Java mode
      (lambda (limit)
        (catch 'to-fontify
          (while (< (point) limit)
            (let* ((p (point))
                   (face (get-text-property p 'face))
                   (match-end (next-single-property-change p 'face (current-buffer) limit)))
              (when; A type declarant keyword is found, `class`, `enum` or `interface`.
                  (and (eq face 'font-lock-keyword-face)
                       (jtam-is-type-declarant (buffer-substring-no-properties (point) match-end)))
                (goto-char match-end)
                (skip-syntax-forward "-"); [FC]
                (let ((match-beg (point))
                      (annotation-count 0))
                  (when (and (> (skip-chars-forward jtam-name-character-set) 0); A name follows.
                             (<= (point) limit)
                             (not (get-text-property match-beg 'face))); The name is unfontified.
                    (setq match-end (point))
                    (goto-char p); Back to the type declarant keyword.
                    (skip-syntax-backward "-"); [FC]
                    (when (eq (char-before (point)) ?@); A ‘@’ marks this declaration as
                      (backward-char); that of an annotation type.  Move back past the ‘@’.
                      (skip-syntax-backward "-")); [FC]
                    (catch 'is-modifier; Thrown as nil on encountering *not* a type declaration modifier.
                      (while t; Now point should (invariant) be directly after such a modifier.  So test:
                        (when (eq (char-before (point)) ?\)); Annotation parameters.
                          (condition-case _x
                              (forward-sexp -1); Skip past them.
                            (scan-error (throw 'is-modifier nil)))
                          (skip-syntax-backward "-")); Holding still the (would be) invariant. [FC]
                        (setq p (point))
                        (when (= (skip-chars-backward jtam-name-character-set) 0)
                          (throw 'is-modifier nil))
                        ;; The modifier should be either a keyword or an annotation.
                        (if (jtam-is-type-modifier-keyword (buffer-substring-no-properties (point) p))

                            ;; Keyword, the modifier is a keyword
                            ;; ───────
                            (if (= annotation-count 0)
                                (skip-syntax-backward "-"); [FC]
                              (set-match-data (list match-beg match-end (current-buffer)))
                              (goto-char match-end)
                              (throw 'to-fontify t)); The keyword precedes an annotation.  Java mode
                                ;;; expects keywords to *succeed* any annotation, which ‘is customary’
                                ;;; according to the Java specification.  Still it is ‘not required’,
                                ;;; therefore proceed to apply the missing fontification.
                                ;;; https://docs.oracle.com/javase/specs/jls/se13/html/jls-8.html#jls-8.1.1

                          ;; Annotation, the modifier is an annotation, or should be
                          ;; ──────────
                          (skip-syntax-backward "-"); A form unconventional, but allowed. [FC]
                          (unless (eq (char-before (point)) ?@)
                            (throw 'is-modifier nil))
                          (setq annotation-count (1+ annotation-count))
                          (backward-char)
                          (skip-syntax-backward "-"))))))); [FC]
              (goto-char match-end)))
          (throw 'to-fontify nil)))
      '(0 'jtam-type-declaration t))

     ;; ═══════════════════
     ;; Type parameter name in a type parameter declaration  [↑T]
     ;; ═══════════════════
     (cons; Refontify each using face `jtam-type-parameter-declaration`. [RP]
      (lambda (limit)
        (catch 'to-refontify
          (while (< (point) limit)
            (let* ((match-beg (point))
                   (face (get-text-property match-beg 'face))
                   (match-end (next-single-property-change match-beg 'face (current-buffer) limit)))
              (when (and (eq face 'jtam--type-reference-in-parameter-list)

                         ;; And directly preceding that already fontified parameter name
                         ;; is no additional bound operator (`&`) nor `extends` keyword. [TP]
                         (let* ((gap (- (skip-syntax-backward "-"))); [QSB, FC]
                                (p (point)))
                           (not (or (char-equal (char-before p) ?&)
                                    (and (> gap 0)
                                         (< (skip-chars-backward jtam-name-character-set) 0)
                                         (string= (buffer-substring-no-properties (point) p)
                                                  "extends")))))

                         ;; And that parameter name occurs at the top level of the parameter list (depth
                         ;; of angle bracing 1).  And the list directly follows one of (a) the name of
                         ;; a type declaration, indicating a generic class or interface declaration,
                         ;; or (b) neither a type name, type parameter nor `.` delimiter (of a method
                         ;; call), indicating a generic method or contructor declaration.  [TP, MI]
                         (catch 'is-proven
                           (let ((depth 1); Depth of name in bracing, presumed to be 1 as required.
                                 (p (point)) c)
                             (while (> (setq p (1- p)) 0); Move `p` leftward to emerge from all braces.
                               (setq c (char-after p))
                               (cond ((char-equal c ?<); Ascending from the present brace pair.
                                      (setq depth (1- depth))
                                      (when (= 0 depth); Then presumeably `p` has emerged left of list.
                                        (goto-char p)
                                        (skip-syntax-backward "-"); [FC]
                                        (when (bobp) (throw 'is-proven t))
                                           ;;; Apparently a generic method or contructor declaration,
                                           ;;; though outside of any class body and so misplaced.
                                        (setq p (1- p); Into direct predecessor of parameter list.
                                              c (char-after p))
                                        (when (or (char-equal c ?.); `.` delimiter of a method call.
                                                  (char-equal c ?<)); `p` had *not* emerged, and so the
                                                    ;;; parameter name is *not* at top level, after all.
                                          (throw 'is-proven nil))
                                        (setq c (get-text-property p 'face))
                                        (throw 'is-proven; As the type parameter declaration of:
                                               (or (eq c 'jtam-type-declaration); [↑T]
                                                     ;;; (a) a generic class or interface declaration;
                                                   (not (jtam-faces-are-equivalent
                                                         c 'font-lock-type-face))))))
                                                     ;;; (b) a generic method or contructor declaration.
                                     ((char-equal c ?>); Descending into another brace pair.
                                      (setq depth (1+ depth)))))
                             nil)))
                (set-match-data (list match-beg match-end (current-buffer)))
                (goto-char match-end)
                (throw 'to-refontify t))
              (goto-char match-end)))
          (throw 'to-refontify nil)))
      '(0 'jtam-type-parameter-declaration t)))

    "Elements of ‘jtam-fontifiers-3’ that are specific to ‘java-mode-tamed’.")



  (defface jtam-type-declaration; [MDF]
    `((t . (:inherit font-lock-type-face))); [UAF, RP]
    "The face for the identifier of a class or interface in a type declaration.
Use it to highlight the identifier where it is declared, as opposed to merely
referenced; like ‘font-lock-variable-name-face’ does for variable identifiers.
See also face ‘jtam-type-reference’."
    :group 'java-mode-tamed)



  (defface jtam-type-parameter-declaration; [TP, MDF, SF]
    `((t . (:inherit jtam-type-declaration))); [UAF, RP]
    "The face for the identifier of a type parameter in a type parameter declaration.
Use it to highlight the identifier where it is declared, as opposed to merely
referenced; like ‘font-lock-variable-name-face’ does for variable identifiers.
See also face ‘jtam-type-reference’."
    :group 'java-mode-tamed)



  (defface jtam-type-reference; [MDF]
    `((t . (:inherit font-lock-type-face))); [UAF, RP]
    "The face for the identifier of a class, interface or type parameter
where it appears as a type reference.  See also faces ‘jtam-type-declaration’
and ‘jtam-type-parameter-declaration’."
    :group 'java-mode-tamed)



  (defface jtam--type-reference-in-parameter-list; [TP, TA, MDF]
    `((t . (:inherit jtam-type-reference))); [UAF, RP]
    "The face for the identifier of a class, interface or type parameter where it
appears as a type reference in a type parameter list, one delimited by the sym-
bols ‘<’ and ‘>’.  Do not customize this face — it is for internal use only —
leave it to inherit from ‘jtam-type-reference’."
    :group 'java-mode-tamed)



  (defun jtam-untamed-face (face)
    "Returns the untamed ancestral face of FACE from which ultimately it inherits,
or FACE itself if untamed." ; [UAF]
    (while (string-prefix-p "jtam-" (symbol-name face))
      (setq face (face-attribute face :inherit nil nil)))
    face)



  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════


  (unless jtam--early-initialization-was-begun
    (set 'jtam--early-initialization-was-begun t)
    (set 'c-default-style (cons '(java-mode-tamed . "java") c-default-style)))
      ;;; Though it appears to have no effect.

  (define-derived-mode java-mode-tamed java-mode
    "Java" "A tamer, more controllable Java mode" :group 'java-mode-tamed
    (set 'c-maybe-decl-faces
         (append c-maybe-decl-faces
                 '('jtam-modifier-keyword
                   'jtam-type-declaration
                   'jtam-type-parameter-declaration
                   'jtam-type-reference
                   'jtam--type-reference-in-parameter-list)))
    (cl-assert (local-variable-p 'c-maybe-decl-faces))
    (let ((level (font-lock-value-in-major-mode font-lock-maximum-decoration)))
      (set 'jtam--is-level-3 (or (eq level t) (and (numberp level) (>= level 3)))))
    (set 'font-lock-defaults
         ;; Following are the alternative values of `font-lock-keywords`, each ordered
         ;; according to the value of `font-lock-maximum-decoration` that selects it.  [MD]
         '((jtam-fontifiers-1 jtam-fontifiers-1 jtam-fontifiers-2 jtam-fontifiers-3)))
           ;;;       nil or 0,                1,                2,           t or 3
    (cl-assert (local-variable-p 'font-lock-defaults))
    (unless jtam--late-initialization-was-begun
      (set 'jtam--late-initialization-was-begun t)

      ;; Monkey patch the underlying (Java mode) functions
      ;; ─────────────────────────────────────────────────
      (define-error 'jtam-x "Broken monkey patch")
      (condition-case x
          (let ((source-file (locate-library "cc-fonts.el" t))
                (source-base-name "cc-fonts"))
            (unless source-file
              (signal 'jtam-x `("No such source file on load path: `cc-fonts.el`")))
            (with-temp-buffer
              (insert-file-contents source-file)

              (jtam--patch
               source-file source-base-name 'c-font-lock-<>-arglists
               (lambda ()
                 (let (is-patched)
                   (while (re-search-forward "(\\s-*\\(eq\\)\\s-+[^)]+?-face" nil t)
                     (replace-match "jtam-faces-are-equivalent" t t nil 1)
                     (setq is-patched t))
                   is-patched)))

              (jtam--patch
               source-file source-base-name 'c-font-lock-declarations
               (lambda ()
                 (when (re-search-forward
                        (concat "(\\s-*\\(eq\\)\\s-*(get-text-property\\s-*(point)\\s-*'face)\\s-*"
                                "'font-lock-keyword-face)")
                        nil t)
                   (replace-match "jtam-faces-are-equivalent" t t nil 1)
                   t)))

              (jtam--patch
               source-file source-base-name 'c-fontify-recorded-types-and-refs
               (lambda ()
                 (when (re-search-forward
                        (concat "(c-put-font-lock-face\\s-*(car\\s-*\\(\\w+\\))\\s-*(cdr\\s-*\\1)\\s-*"
                                "'font-lock-type-face)")
                        nil t)
                   (replace-match "(jtam--c/put-type-face \\1)" t)
                   t)))))

        (jtam-x (display-warning 'java-mode-tamed (error-message-string x) :error)))))



  (provide 'java-mode-tamed)); Providing these features
    ;;; of `java-mode-tamed.el` for any who `require` them.



;; NOTES
;; ─────
;;   ↑T · This marks section *Type name* of `jtam-specific-fontifiers` and all code that depends on its
;;        prior execution.
;;
;;   FC · `forward-comment` would be more robust here.
;;
;;   MD · How the value of `font-lock-maximum-decoration` governs the value of `font-lock-keywords`
;;        is documented inconsistently by Emacs.  See instead the `font-lock-choose-keywords` function
;;        of `http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/font-lock.el`.  It verifies the cor-
;;        rectness of `https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html`.
;;
;;   MDF  `c-maybe-decl-faces`: Any replacement face [RP] for a face listed in `c-maybe-decl-faces`
;;        must itself be appended to the list.
;;
;;   MI · See `MethodInvocation`.
;;        https://docs.oracle.com/javase/specs/jls/se13/html/jls-15.html#jls-15.12
;;
;;   QSB  Quickly searching backward from an anchor at point.  Regular expressions are inapt here;
;;        one needs the anchor for sake of speed, but `looking-back` ‘can be quite slow’ regardless.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Search.html
;;
;;   RP · Replacement face.  Ultimately every face used by `java-mode-tamed` to override and replace
;;        a face earlier applied by Java mode (replacement face) inherits from the face it replaces.
;;        Function `jtam-faces-are-equivalent` depends on this.
;;
;;   SF · Stuck face `jtam-type-parameter-declaration`.  Note that the facing guard
;;        in `jtam/c-put-type-face` may cause this face to stick on occaision.
;;        A viable workaround is to delete and re-type the affected text, which tends to be short.
;;
;;   TA · See `TypeArgument`.  https://docs.oracle.com/javase/specs/jls/se13/html/jls-4.html#jls-4.5.1
;;
;;   TP · See `TypeParameter`.  https://docs.oracle.com/javase/specs/jls/se13/html/jls-4.html#jls-4.4
;;
;;   UAF  Untamed ancestral face.  Ultimately every face defined by `java-mode-tamed` (tamed face)
;;        inherits from a face defined elsewhere (untamed ancestral face).  Function `jtam-untamed-face`
;;        depends on this.


;; - - - - - - - - - -
;; Local Variables:
;; byte-compile-warnings: (not make-local)
;; End:
                                       ;;; Copyright © 2019 Michael Allan and contributors.  Licence MIT.
