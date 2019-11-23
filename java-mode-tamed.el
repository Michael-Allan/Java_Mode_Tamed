;; The definition of `java-mode-tamed` - a tamer, more controllable Java mode.  -*- lexical-binding: t; -*-
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
;;       http://reluk.ca/.Xresources
;;
;;
;; CUSTOMIZATION
;; ─────────────
;;   For the customizeable faces, see the `defface` declarations further below.
;;
;;
;; TEXT PROPERTIES
;; ───────────────
;;   jmt-stabilized
;;       Marks text whose fontification is stabilized by Java mode tamed.  Blocks the underlying code
;;       of Java mode from overriding the `face` properties of the marked text by its own mechanisms,
;;       outside of Font Lock that is. [SF]
;;
;;
;; NOTES  (see at bottom)
;; ─────


(let ()


  (eval-when-compile
    (require 'cc-mode)
    (require 'cl-lib))

  (defvar c-maybe-decl-faces)
    ;;; Suppressing sporadic compiler warnings ‘reference to free variable’
    ;;; or ‘assignment to free variable’.



  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════
  ;;  P r e l i m i n a r y   d e c l a r a t i o n s
  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════


  (defvar-local jmt--is-level-3 nil); An in-buffer cache of this boolean flag.  It works only because any
    ;;; ‘customization of `font-lock-maximum-decoration` should be done *before* the file is visited’.
    ;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html



  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════
  ;;  D e c l a r a t i o n s   i n   l e x i c o g r a p h i c   o r d e r
  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════


  (defgroup java-mode-tamed nil
    "A tamer, more controllable Java mode"
    :group 'languages :group 'faces
    :prefix "jmt-"
    :link '(url-link "http://reluk.ca/project/Java/Emacs/"))



  (defface jmt-annotation-mark
    `((t . (:inherit c-annotation-face))); [TF, RP]
    "The face for the ‘@’ symbol denoting annotation.  Use it customize
the appearance of the symbol, e.g. to give it less prominence than
the ‘c-annotation-face’ of the type name that follows it."
    :group 'java-mode-tamed)



  (defface jmt-annotation-qualifier
    `((t . (:inherit c-annotation-face))); [TF, RP]
    "The face for the element assignments of annotation.  Use it customize
the appearance of the assignments, e.g. to give them less prominence than
the ‘c-annotation-face’ of the preceding type name."
    :group 'java-mode-tamed)



  (defface jmt-annotation-qualifier-delimiter
    `((t . (:inherit jmt-annotation-qualifier))); [TF]
    "The face for the ‘(’ and ‘)’ delimiters of an annotation qualifier."
    :group 'java-mode-tamed)



  (defun jmt-c-lisp-top-declaration-end ()
    "Returns the position just before the next top-level ELisp declaration, if any,
else `point-max`.  Leaves point indeterminate.  For use on buffers that contain
Java mode (CC mode) source code."
    (if (re-search-forward "^(" nil t) (1- (point)) (point-max)))



  (defun jmt--c/put-type-face (range)
    "Called from a monkey patch over the underlying Java-mode code, this function
overrides Java mode’s application of ‘font-lock-type-face’ in order to stabilize
the facing of type and type parameter identifiers.  RANGE is a cons cell."
    ;; Without this patched override the corresponding refontifications of `jmt-specific-fontifiers-3`
    ;; alternately appear and disappear.  This occurs because Java mode applies `font-lock-type-face`
    ;; using a mechanism of its own, outside of Font Lock, which puts the two in an endless tug of war.
    (let ((beg (car range))
          (end (cdr range)))
      (if
          (and jmt--is-level-3
               (eq major-mode 'java-mode-tamed))
          (if (catch 'is-enlisted; In a type parameter list delimited by ‘<’ and ‘>’, that is.
                (or
                 (let ((p beg) c)
                   (while; Set `c` to the first non-whitespace character before `range`. [NAC]
                       (progn (setq c (char-before p))
                              (when (eq c nil) (throw 'is-enlisted nil)); Out of bounds.
                              (char-equal (char-syntax c) ?\s))
                     (setq p (1- p)))
                   (or (char-equal c ?<)    ; A leading delimiter ‘<’,
                       (char-equal c ?&)    ; additional bound operator,
                       (char-equal c ?,)))  ; or separator ‘,’ in a type parameter list.
                 (let ((p end) c)
                   (while; Set `c` to the first non-whitespace character after `range`. [NAC]
                       (progn (setq c (char-after p))
                              (when (eq c nil) (throw 'is-enlisted nil)); Out of bounds.
                              (char-equal (char-syntax c) ?\s))
                     (setq p (1+ p)))
                   (or (char-equal c ?>)    ; A trailing delimiter ‘>’,
                       (char-equal c ?&)    ; additional bound operator,
                       (char-equal c ?,))))); or separator ‘,’.

              ;; Taming an identifier in a type parameter list  [TP, TA]
              ;; ─────────────────────────────────────────────
              (progn
                (unless; Unless already `beg`…`end` is faced `jmt-type-parameter-declaration`. [SF]
                    (and (eq (get-text-property beg 'face) 'jmt-type-parameter-declaration)
                         (>= (next-single-property-change beg 'face (current-buffer) end) end))
                  (c-put-font-lock-face beg end 'jmt--type-reference-in-parameter-list))); [RP]
                    ;;; Immediately `jmt-specific-fontifiers-3` may override and correct this facing,
                    ;;; replacing it with `jmt-type-parameter-declaration`.

            ;; Taming an identifier elsewhere
            ;; ──────────────────────────────
            (unless (get-text-property beg 'jmt-stabilized); [SF]
              (c-put-font-lock-face beg end 'jmt--type)))
                ;;; Normally `jmt-specific-fontifiers-3` will immediately override this facing,
                ;;; replacing it with `jmt-type-declaration` or `jmt-type-reference`.  Occaisionally
                ;;; this replacement may fail to occur, occur late, or prove to be unstable. [UF]

        ;; Leaving the identifier untamed
        ;; ──────────────────────────────
        (c-put-font-lock-face beg end 'font-lock-type-face))))



  (defvar jmt--early-initialization-was-begun nil)



  (defun jmt-faces-are-equivalent (f1 f2); [RP]
    "Answers whether F1 and F2 (face symbols) should be treated as equivalent
by the underlying (Java mode) code."
    (eq (jmt-untamed-face f1) (jmt-untamed-face f2)))



  (defvar jmt-face nil "A face indirectly referred to by a fontifier.")



  (defun jmt-is-modifier-keyword (s)
    "Answers whether string S is a Java modifier in keyword form.
See face ‘jmt-modifier-keyword’."
    ;;       `ClassModifier` https://docs.oracle.com/javase/specs/jls/se13/html/jls-8.html#jls-8.1.1
    ;;   `InterfaceModifier` https://docs.oracle.com/javase/specs/jls/se13/html/jls-9.html#jls-9.1.1
    ;;      `MethodModifier` https://docs.oracle.com/javase/specs/jls/se13/html/jls-8.html#jls-8.4.3
    ;; `ConstructorModifier` https://docs.oracle.com/javase/specs/jls/se13/html/jls-8.html#jls-8.8.3
    ;;       `FieldModifier` https://docs.oracle.com/javase/specs/jls/se13/html/jls-8.html#jls-8.3.1
    (or (jmt-is-type-modifier-keyword s)
        (string= s "synchronized")
        (string= s "volatile")
        (string= s "transient")
        (string= s "native")))



  (defun jmt-is-type-declarant (s)
    "Answers whether string S is the principle keyword of a type declaration."
    (or (string= s "class")
        (string= s "interface")
        (string= s "enum")))



  (defun jmt-is-Java-mode-type-face (f)
    "Answers whether F (face symbol) is a type face which might be set
by the underlying (Java mode) code."
    (or (eq f 'jmt--type); Set by Java mode via `jmt--c/put-type-face`.
        (eq f 'font-lock-type-face))); Or via (if it occurs) other means.



  (defun jmt-is-type-modifier-keyword (s)
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



  (defvar jmt--late-initialization-was-begun nil)



  (defun jmt-message (format-string &rest arguments)
    "Calls function ‘message’ without translation of embedded \\=`\\=`\\=`
and \\=`\\='\\=` quotes."
    (message "%s" (apply 'format format-string arguments)))



  (defface jmt-modifier-keyword; [MDF]
    `((t . (:inherit font-lock-keyword-face))); [TF, RP]
    "The face for a keyword-form modifier in a class, interface, method,
constructor or field declaration; any modifier, that is, except an annotation
modifier.  Use it customize the appearance of these keywords, e.g. to give them
less prominence than other, more important keywords."
    :group 'java-mode-tamed)



  (defconst jmt-name-character-set "[:alnum:]_$"
    "The set of characters from which a Java identifier may be formed.")
    ;;; https://docs.oracle.com/javase/specs/jls/se13/html/jls-3.html#jls-3.8



  (defun jmt-new-fontifiers-2 ()
    "Builds a ‘font-lock-keywords’ list for fast, untamed highlighting.
See also ‘java-font-lock-keywords-1’, which is for minimal untamed highlighting."
    (java-font-lock-keywords-2))



  (defun jmt-new-fontifiers-3 ()
    "Builds a ‘font-lock-keywords’ list for accurate, tamed highlighting."
    (nconc

    ;; Underlying Java-mode fontifiers, lightly modified
    ;; ───────────────────────────────
     (let* ((kk (java-font-lock-keywords-3)); List of Java mode’s fontifiers.
            was-found-annotation; Whether the annotation fontifier of was found in `kk`.
            (k kk); Current fontifier element of `kk`.
            k-last); Previous fontifier element.
       (while; Searching the list, fontifier by fontifier.
           (progn
             (if (equal (car k) '(eval list "\\<\\(@[a-zA-Z0-9]+\\)\\>" 1 c-annotation-face))
                 ;; Dud fontifier: works under Java mode, fails under Java mode tamed unless
                 ;; changed in two places `"\\_<\\(@[a-zA-Z0-9]+\\)\\>" 1 c-annotation-face t`.
                 (progn;                    1 ↑                                           2 ↑
                   ;; Moreover its pattern does not cover the complete, valid form of annotation.
                   ;; Therefore `jmt-new-fontifiers-3` adds a more general, replacement fontifier.
                   (setq was-found-annotation t)
                   (setcdr k-last (cdr k))); Deleting this one here in case somehow it starts working
               (setq k-last k              ; and interferes with the replacement.
                     k (cdr k)))
             (and (not was-found-annotation) k)))
       (unless was-found-annotation
         (jmt-message "(java-mode-tamed): Failed to remove unwanted Java-mode fontifier: `%s` = nil"
                       (symbol-name 'was-found-annotation)))
       kk)

    ;; Overlying fontifiers to tame them
    ;; ────────────────────
     jmt-specific-fontifiers-3))



  (defun jmt--patch (source-file source-base-name function-symbol patch-function)
    "Monkey patches function FUNCTION-SYMBOL of file SOURCE-FILE (a string,
which has the given BASE-NAME) using the named PATCH-FUNCTION.  The patch
function must return t on success, nil on failure."
    (unless (functionp function-symbol)
      (signal 'jmt-x `("No such function loaded" ,function-symbol)))
    (let ((load-file (symbol-file function-symbol)))
      (unless (string= (file-name-base load-file) source-base-name)
        (signal 'jmt-x `("Function loaded from file of base name contradictory to source file"
                          ,function-symbol ,load-file ,source-file))))
    (goto-char (point-min))
    (unless (re-search-forward
             (concat "^(defun\\s-+" (symbol-name function-symbol) "\\s-*(") nil t)
      (signal 'jmt-x `("Function declaration not found in source file"
                        ,function-symbol ,source-file)))
    (narrow-to-region (match-beginning 0) (jmt-c-lisp-top-declaration-end))
      ;;; Narrowing the temporary patch buffer to the function declaration alone.
    (goto-char (point-min))
    (unless (funcall patch-function); Patching the declaration.
      (signal 'jmt-x `("Patch failed to apply" ,function-symbol)))
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
             (jmt-message "(java-mode-tamed): Failed to recompile monkey-patched function `%s`"
                           (symbol-name function-symbol))))))))



  (defun preceding->-marks-generic-return-type ()
    "Answers whether the ‘>’ character before point could be a delimiter within a
function declaration, namely the trailing delimiter of a list of type parameters
for the function’s return type, making it a *generic* return type.  May move point."
    (when
        (condition-case _x
            (progn (forward-sexp -1) t); Skip to the front of the leading delimiter.
          (scan-error nil))
      (forward-comment most-negative-fixnum); [←CW]
      (not (eq (char-before) ?.)))); (not `char-equal`, in case nil)
        ;;; Here a `.` would indicate a method call, as opposed to a declaration.


  (defun jmt-set-for-buffer (variable value)
    "Sets VARIABLE (a symbol) to VALUE.  Signals an error if the setting
is not buffer local."
    (set variable value)
    (cl-assert (local-variable-p variable)))



  (defconst jmt-specific-fontifiers-3
    (list

     ;; ══════════
     ;; Annotation  [↑A]
     ;; ══════════
     (list; Fontify each, overriding any misfontification of Java mode.
      (lambda (limit)
        (catch 'to-fontify
          (let ((m1-beg (point)); Start of leading annotation mark ‘@’, till proven otherwise.
                (m1-beg-limit (1- limit)); Room for two characters, the minimal length.
                eol face m1-end m2-beg m2-end m3-beg m3-end m4-beg m4-end m5-beg m5-end)
            (while (< m1-beg m1-beg-limit)
              (setq m1-end (1+ m1-beg))
              (if (not (char-equal ?@ (char-after m1-beg)))
                  (setq m1-beg m1-end)
                (goto-char m1-end)
                (catch 'is-annotation
                  (when (eolp) (throw 'is-annotation nil)); [SL]
                  (skip-syntax-forward "-" limit); Though unconventional, whitespace is allowed
                    ;;; between ‘@’ and name.  Nevertheless this fontifier excludes newlines. [AST, SL]
                    ;;; Also it excludes commentary, which would be perverse here, not worth coding for.
                  (setq m2-beg (point))
                  (skip-chars-forward jmt-name-character-set limit)
                  (setq m2-end (point))
                  (unless (< m2-beg m2-end) (throw 'is-annotation nil))
                  (setq face (get-text-property m2-beg 'face))
                  (unless (or (eq face nil); The most common case.  Less commonly, a misfontification:
                              (eq face 'font-lock-function-name-face); ← This one occurs in the case
                              (jmt-is-Java-mode-type-face face))    ;  e.g. of an empty `()` qualifier.
                    (throw 'is-annotation nil))
                  (skip-syntax-forward "-" limit); [SL]
                  (when (eq ?\( (char-after)); (and not nil)
                    (setq m3-beg (point); Start of trailing qualifier, it would be.
                          eol (line-end-position))
                    (condition-case _x
                        (progn
                          (forward-list 1)
                          (setq m5-end (point))); End of qualifier.  Point now stays here.
                      (scan-error
                       (setq m5-end (point-max)))); Forcing the qualifier to be ignored below.
                    (if (> m5-end eol); The qualifier crosses lines, or a `scan-error` occured above.
                        (goto-char m2-end); Ignoring it. [SL]

                      ;; Qualified
                      ;; ─────────
                      (setq m3-end (1+ m3-beg); ‘(’
                            m4-beg m3-end
                            m5-beg (1- m5-end); ‘)’
                            m4-end m5-beg)
                      (set-match-data (list m1-beg m5-end m1-beg m1-end m2-beg m2-end m3-beg m3-end
                                            m4-beg m4-end m5-beg m5-end (current-buffer)))
                      (throw 'to-fontify t))); With point (still) at `m5-end` as Font Lock stipulates.

                  ;; Unqualified
                  ;; ───────────
                  (set-match-data (list m1-beg m2-end m1-beg m1-end m2-beg m2-end (current-buffer)))
                  (goto-char m2-end)
                  (throw 'to-fontify t))

                (setq m1-beg (point)))))
          (throw 'to-fontify nil)))
      '(1 'jmt-annotation-mark t) '(2 'c-annotation-face t)
      '(3 'jmt-annotation-qualifier-delimiter t t) '(4 'jmt-annotation-qualifier t t)
      '(5 'jmt-annotation-qualifier-delimiter t t))


     ;; ════════════════
     ;; Modifier keyword
     ;; ════════════════
     (cons; Refontify each using face `jmt-modifier-keyword`. [RP]
      (lambda (limit)
        (catch 'to-refontify
          (while (< (point) limit)
            (let* ((match-beg (point))
                   (face (get-text-property match-beg 'face))
                   (match-end (next-single-property-change match-beg 'face (current-buffer) limit)))
              (goto-char match-end)
              (when (and (eq face 'font-lock-keyword-face)
                         (jmt-is-modifier-keyword (buffer-substring-no-properties match-beg match-end)))
                (set-match-data (list match-beg match-end (current-buffer)))
                (throw 'to-refontify t))))
          (throw 'to-refontify nil)))
      '(0 'jmt-modifier-keyword t))


     ;; ═════════
     ;; Type name  [↑T]
     ;; ═════════
     (list; Refontify each using either `jmt-type-declaration` or  `jmt-type-reference` face. [RP]
      (lambda (limit)
        (catch 'to-refontify
          (while (< (point) limit)
            (let* ((match-beg (point))
                   (face (get-text-property match-beg 'face))
                   (match-end (next-single-property-change match-beg 'face (current-buffer) limit)))
              (when (jmt-is-Java-mode-type-face face)
                (forward-comment most-negative-fixnum); [←CW, QSB]

                ;; Either declaring (1) a type
                ;; ────────────────
                (when; Keyword `class`, `enum` or `interface` directly precedes the type name.
                    (let ((p (point)))
                      (and (< (skip-chars-backward jmt-name-character-set) 0)
                           (jmt-is-type-declarant (buffer-substring-no-properties (point) p))))
                  (set-match-data; Capturing the already fontified name as group 1.
                   (list match-beg match-end match-beg match-end (current-buffer)))
                  (goto-char match-end)
                  (throw 'to-refontify t))

                ;; Or merely referring (2) to one
                ;; ───────────────────
                (set-match-data; Capturing the name instead as group 2.
                 (list match-beg match-end nil nil match-beg match-end (current-buffer)))
                (goto-char match-end)
                (throw 'to-refontify t))

              (goto-char match-end)))
          (throw 'to-refontify nil)))
      '(1 '(face jmt-type-declaration jmt-stabilized t) t t) '(2 'jmt-type-reference t t)); [SF]
        ;;; The stabilizer is for a minority of cases which have no discerned pattern.

     (cons; Fontify type declaration names unfaced by Java mode.
      (lambda (limit)
        (catch 'to-fontify
          (while (< (point) limit)
            (let* ((p (point))
                   (face (get-text-property p 'face))
                   (match-end (next-single-property-change p 'face (current-buffer) limit)))
              (when; A type declarant keyword is found, `class`, `enum` or `interface`.
                  (and (eq face 'font-lock-keyword-face)
                       (jmt-is-type-declarant (buffer-substring-no-properties (point) match-end)))
                (goto-char match-end)
                (forward-comment most-positive-fixnum); [CW→]
                (let ((match-beg (point))
                      (annotation-count 0))
                  (when (and (< match-beg limit)
                             (> (skip-chars-forward jmt-name-character-set limit) 0); A name follows.
                             (not (get-text-property match-beg 'face))); The name is unfontified.
                    (setq match-end (point))
                    (goto-char p); Back to the type declarant keyword.
                    (forward-comment most-negative-fixnum); [←CW]
                    (when (eq (char-before (point)) ?@); (and not nil)  A ‘@’ marks this declaration
                      (backward-char); as that of an annotation type.  Move back past the ‘@’.
                      (forward-comment most-negative-fixnum)); [←CW]
                    (catch 'is-modifier; Thrown as nil on encountering *not* a type declaration modifier.
                      (while t; Now point should (invariant) be directly after such a modifier.  So test:
                        (when (eq (char-before (point)) ?\)); (and not nil)  A list of anno-
                          (condition-case _x                ; tation parameters, presumeably.
                              (forward-sexp -1); Skip to the front of it.
                            (scan-error (throw 'is-modifier nil)))
                          (forward-comment most-negative-fixnum)); [←CW]
                            ;;; Holding still the (would be) invariant.
                        (setq p (point))
                        (when (= (skip-chars-backward jmt-name-character-set) 0)
                          (throw 'is-modifier nil))
                        ;; The modifier should be either a keyword or annotation.
                        (if (jmt-is-type-modifier-keyword (buffer-substring-no-properties (point) p))

                            ;; Keyword, the modifier is a keyword
                            ;; ───────
                            (if (= annotation-count 0)
                                (forward-comment most-negative-fixnum); [←CW]
                              (set-match-data (list match-beg match-end (current-buffer)))
                              (goto-char match-end)
                              (throw 'to-fontify t)); The keyword precedes annotation.  With this.
                                ;;; Java mode fails at times to face the type name.  This was seen,
                                ;;; for instance, here in the sequence `public @ThreadSafe class ID`:
                                ;;; `https://github.com/Michael-Allan/waymaker/blob/3eaa6fc9f8c4137bdb463616dd3e45f340e1d34e/waymaker/spec/ID.java#L8`.
                                ;;;     It seems Java mode expects to find keywords *before* annotation,
                                ;;; which, although it ‘is customary’, is nevertheless ‘not required’,
                                ;;; `https://docs.oracle.com/javase/specs/jls/se13/html/jls-8.html#jls-8.1.1`.
                                ;;; Here therefore the missing face is applied.

                          ;; Annotation, the modifier is an annotation modifier, or should be
                          ;; ──────────
                          (forward-comment most-negative-fixnum); [←CW]
                            ;;; A form unconventional, but allowed. [AST]
                          (unless (eq (char-before (point)) ?@); (and not nil)
                            (throw 'is-modifier nil))
                          (setq annotation-count (1+ annotation-count))
                          (backward-char)
                          (forward-comment most-negative-fixnum))))))); [←CW]
              (goto-char match-end)))
          (throw 'to-fontify nil)))
      '(0 'jmt-type-declaration t))


     ;; ═══════════════════
     ;; Type parameter name in a type parameter declaration  [↑T]
     ;; ═══════════════════
     (cons; Refontify each using face `jmt-type-parameter-declaration`. [RP]
      (lambda (limit)
        (catch 'to-refontify
          (while (< (point) limit)
            (let* ((match-beg (point))
                   (face (get-text-property match-beg 'face))
                   (match-end (next-single-property-change match-beg 'face (current-buffer) limit)))
              (when (eq face 'jmt--type-reference-in-parameter-list)
                (forward-comment most-negative-fixnum); [←CW, QSB]
                (when
                    (and; And directly preceding that already fontified parameter name
                      ;;; is no additional bound operator (`&`) nor `extends` keyword. [TP]
                     (let ((p (point)))
                       (not (or (char-equal (char-before p) ?&)
                                (and (< (skip-chars-backward jmt-name-character-set) 0)
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
                                    (forward-comment most-negative-fixnum); [←CW]
                                    (when (bobp) (throw 'is-proven t))
                                           ;;; Apparently a generic method or contructor declaration,
                                           ;;; though outside of any class body and so misplaced.
                                    (setq p (1- (point)); Into direct predecessor of parameter list.
                                          c (char-after p))
                                    (when (or (char-equal c ?.); `.` delimiter of a method call.
                                              (char-equal c ?<)); `p` had *not* emerged, and so the
                                                    ;;; parameter name is *not* at top level, after all.
                                      (throw 'is-proven nil))
                                    (setq c (get-text-property p 'face))
                                    (throw 'is-proven; As the type parameter declaration of:
                                           (or (eq c 'jmt-type-declaration); [↑T]
                                                     ;;; (a) a generic class or interface declaration;
                                               (not (jmt-faces-are-equivalent
                                                     c 'font-lock-type-face))))))
                                                     ;;; (b) a generic method or contructor declaration.
                                 ((char-equal c ?>); Descending into another brace pair.
                                  (setq depth (1+ depth)))))
                         nil)))
                  (set-match-data (list match-beg match-end (current-buffer)))
                  (goto-char match-end)
                  (throw 'to-refontify t)))
              (goto-char match-end)))
          (throw 'to-refontify nil)))
      '(0 'jmt-type-parameter-declaration t))


     ;; ════════════════════════════════
     ;; Method or constructor identifier  [↑A, ↑T]
     ;; ════════════════════════════════
     (cons; Fontify where misfaced by Java mode, or incorrectly unfaced.
      (let ((identifier-pattern (concat "[" jmt-name-character-set "]+"))
            face i match-beg)
        (lambda (limit)
          (set
           'jmt-face
           (catch 'to-fontify
             (while (re-search-forward identifier-pattern limit t)
               (setq match-beg (match-beginning 0)
                     face (get-text-property match-beg 'face))
               (when (or (eq face nil) (eq face 'font-lock-function-name-face); Unfaced or misfaced.
                         (eq face 'jmt-type-reference)); [↑T]
                   ;;; Vanguard, redundant but for sake of speed.  See the other face guards below.
                 (forward-comment most-positive-fixnum); [CW→]
                 (when (eq ?\( (char-after)); (and not nil)

                   ;; Constructor declaration  (assumption: point is directly before the ‘(’)
                   ;; ───────────────────────
                   (catch 'is-constructor-declaration; One that needs fontifying, that is.  Or some
                     ;; cases of method declaration in need; this section will fontify those, too,
                     ;; just because it happens to precede the method declaration section, below.
                     (when (not (or (eq face nil) (eq face 'jmt-type-reference))); [↑T]
                       (throw 'is-constructor-declaration nil)); Only identifiers left unfaced
                       ;;; or misfaced as type references have been seen.  See for instance
                       ;;; the sequences `public @Warning("non-API") ApplicationX()` at
                       ;;; `https://github.com/Michael-Allan/waymaker/blob/3eaa6fc9f8c4137bdb463616dd3e45f340e1d34e/waymaker/gen/ApplicationX.java#L23`,
                       ;;; and `ID( final String string, final int cN ) throws MalformedID` at
                       ;;; `https://github.com/Michael-Allan/waymaker/blob/3eaa6fc9f8c4137bdb463616dd3e45f340e1d34e/waymaker/spec/ID.java#L30`.

                     ;; After the identifier, in any parameter list
                     ;; ·····················
                     (forward-char); Past the ‘(’.
                     (forward-comment most-positive-fixnum); [CW→]
                     (setq i (point))
                     (when (> (skip-chars-forward jmt-name-character-set) 0)
                       (when (string= "final" (buffer-substring-no-properties i (point)))
                         (throw 'to-fontify 'font-lock-function-name-face))
                       (catch 'is-past-qualifier; Leaving `i` at either the next token to deal with,
                         (while t; or the buffer end, scan past any itervening package qualifier.
                           ;; Now point is (invariant) directly after a name (in form).
                           (forward-comment most-positive-fixnum); [CW→]
                           (when (eobp) (throw 'is-past-qualifier t))
                           (when (not (char-equal ?. (char-after))); Namely the delimiting dot of
                             (throw 'is-past-qualifier t))         ; a preceding package segment.
                           (forward-char); Past the ‘.’.
                           (forward-comment most-positive-fixnum); [CW→] To the next token.
                           (setq i (point)); What follows the qualifier follows its last dot.
                           (when (= (skip-chars-forward jmt-name-character-set) 0)
                             (throw 'is-past-qualifier t)))))
                     (when (and (/= i (point-max))
                                (or (char-equal ?@ (char-after i))
                                    (eq (get-text-property i 'face) 'jmt-type-reference))); [↑T]
                       (throw 'to-fontify 'font-lock-function-name-face))

                     ;; Before the identifier
                     ;; ·····················
                     (goto-char match-beg)
                     (forward-comment most-negative-fixnum); [←CW]
                     (when (bobp) (throw 'is-constructor-declaration nil))
                     (when (char-equal (char-before) ?>)
                       (if (preceding->-marks-generic-return-type)
                           (throw 'to-fontify 'font-lock-function-name-face)
                         (throw 'is-constructor-declaration nil)))
                     ;; A constructor modifier here before point would also indicate a declaration.
                     ;; However, the earlier test of ‘final’ (above) has eliminated the only case
                     ;; in which Java mode is known to fail when a keyword modifier appears here.
                     ;; That leaves only the case of an *annotation* modifier to remedy.
                     (when (jmt-faces-are-equivalent
                            'c-annotation-face (get-text-property (1- (point)) 'face)); [↑A]
                       (throw 'to-fontify 'font-lock-function-name-face)))

                   ;; Method declaration
                   ;; ──────────────────
                   (catch 'is-method-declaration; One that needs fontifying, that is.
                     (when (not (eq face nil)) (throw 'is-method-declaration nil)); Declarations
                       ;;; left unfaced have been seen, but misfacing has not.  See for instance
                       ;;; the sequence `public @Override @Warning("non-API") void onCreate()`:
                       ;;; `https://github.com/Michael-Allan/waymaker/blob/3eaa6fc9f8c4137bdb463616dd3e45f340e1d34e/waymaker/gen/ApplicationX.java#L40`.
                     (goto-char match-beg)
                     (forward-comment most-negative-fixnum); [←CW]
                     (when (bobp) (throw 'is-method-declaration nil))
                     (setq i (char-before))
                     (when (char-equal i ?\]); Return type declared as an array.
                       (throw 'to-fontify 'font-lock-function-name-face))
                     (when (char-equal i ?>)
                       (if (preceding->-marks-generic-return-type)
                           (throw 'to-fontify 'font-lock-function-name-face)
                         (throw 'is-method-declaration nil)))
                     (when (eq (get-text-property (1- (point)) 'face) 'jmt-type-reference); [↑T]
                       ;; The return type is declared simply by a type name.
                       (throw 'to-fontify 'font-lock-function-name-face)))

                   ;; Method call
                   ;; ───────────
                   (catch 'is-method-call; One that needs refontifying, that is.
                     (when (not (eq face 'font-lock-function-name-face)) (throw 'is-method-call nil))
                       ;;; Only calls misfaced as declarations have been seen.
                     (goto-char match-beg)
                     (forward-comment most-negative-fixnum); [←CW]
                     (when (bobp) (throw 'is-method-call nil))
                     (when (char-equal (char-before) ?.); Always the misfaced identifier directly
                       ;; follows a ‘.’, which excludes the possibility of it being a declaration.
                       ;; See for instance the sequence `assert stators.getClass()`:
                       ;; `https://github.com/Michael-Allan/waymaker/blob/3eaa6fc9f8c4137bdb463616dd3e45f340e1d34e/waymaker/gen/KittedPolyStatorSR.java#L58`.
                       (throw 'to-fontify 'default))))
                 (goto-char (match-end 0)))); Whence the next leg of the search begins.
             (throw 'to-fontify nil)))))
      '(0 jmt-face t)))

    "Elements for ‘jmt-new-fontifiers-3’ which are specific to ‘java-mode-tamed’.")



  (defface jmt--type; [MDF, UF]
    `((t . (:inherit jmt-type-reference))); [TF]
    "A signalling face set via ‘jmt--c/put-type-face’.  Do not customize it —
it is for internal use only — leave it to inherit from ‘jmt-type-reference’."
    :group 'java-mode-tamed)



  (defface jmt-type-declaration; [MDF, SF]
    `((t . (:inherit font-lock-type-face))); [TF, RP]
    "The face for the identifier of a class or interface in a type declaration.
Use it to highlight the identifier where it is declared, as opposed to merely
referenced; like ‘font-lock-variable-name-face’ does for variable identifiers.
See also face ‘jmt-type-reference’."
    :group 'java-mode-tamed)



  (defface jmt-type-parameter-declaration; [TP, MDF, SF]
    `((t . (:inherit jmt-type-declaration))); [TF]
    "The face for the identifier of a type parameter in a type parameter declaration.
Use it to highlight the identifier where it is declared, as opposed to merely
referenced; like ‘font-lock-variable-name-face’ does for variable identifiers.
See also face ‘jmt-type-reference’."
    :group 'java-mode-tamed)



  (defface jmt-type-reference; [MDF, UF]
    `((t . (:inherit font-lock-type-face))); [TF, RP]
    "The face for the identifier of a class, interface or type parameter
where it appears as a type reference.  See also faces ‘jmt-type-declaration’
and ‘jmt-type-parameter-declaration’."
    :group 'java-mode-tamed)



  (defface jmt--type-reference-in-parameter-list; [TP, TA, MDF]
    `((t . (:inherit jmt-type-reference))); [TF]
    "The face for the identifier of a class, interface or type parameter where it
appears as a type reference in a type parameter list, one delimited by the sym-
bols ‘<’ and ‘>’.  Do not customize this face — it is for internal use only —
leave it to inherit from ‘jmt-type-reference’."
    :group 'java-mode-tamed)



  (defun jmt-untamed-face (face); [TF]
    "Returns FACE itself if untamed, else the untamed ancestral face
from which ultimately it inherits."
    (while (string-prefix-p "jmt-" (symbol-name face))
      (setq face (face-attribute face :inherit nil nil)))
    face)



  ;; ════════════════════════════════════════════════════════════════════════════════════════════════════


  (unless jmt--early-initialization-was-begun
    (set 'jmt--early-initialization-was-begun t)
    (require 'cc-mode)
    (set 'c-default-style (cons '(java-mode-tamed . "java") c-default-style)))
      ;;; Though it appears to have no effect.

  (define-derived-mode java-mode-tamed java-mode
    "Java"
    "Java mode tamed - A tamer, more controllable Java mode.
        Home page URL ‘http://reluk.ca/project/Java/Emacs/’
User instructions URL ‘http://reluk.ca/project/Java/Emacs/java-mode-tamed.el’"
    :group 'java-mode-tamed

    ;; Finish initializing the mode
    ;; ────────────────────────────
    (unless jmt--late-initialization-was-begun
      (set 'jmt--late-initialization-was-begun t)

      ;; Monkey patch the underlying (Java mode) functions. Only now the first Java file is loaded,
      ;; else patching might needlessly delay the start of Emacs.
      (define-error 'jmt-x "Broken monkey patch")
      (condition-case x
          (let ((source-file (locate-library "cc-fonts.el" t))
                (source-base-name "cc-fonts"))
            (unless source-file
              (signal 'jmt-x `("No such source file on load path: `cc-fonts.el`")))
            (with-temp-buffer
              (insert-file-contents source-file)

              (jmt--patch
               source-file source-base-name 'c-font-lock-<>-arglists
               (lambda ()
                 (let (is-patched)
                   (while (re-search-forward "(\\s-*\\(eq\\)\\s-+[^)]+?-face" nil t)
                     (replace-match "jmt-faces-are-equivalent" t t nil 1)
                     (setq is-patched t))
                   is-patched)))

              (jmt--patch
               source-file source-base-name 'c-font-lock-declarations
               (lambda ()
                 (when (re-search-forward
                        (concat "(\\s-*\\(eq\\)\\s-*(get-text-property\\s-*(point)\\s-*'face)\\s-*"
                                "'font-lock-keyword-face)")
                        nil t)
                   (replace-match "jmt-faces-are-equivalent" t t nil 1)
                   t)))

              (jmt--patch
               source-file source-base-name 'c-fontify-recorded-types-and-refs
               (lambda ()
                 (when (re-search-forward
                        (concat "(c-put-font-lock-face\\s-*(car\\s-*\\(\\w+\\))\\s-*(cdr\\s-*\\1)\\s-*"
                                "'font-lock-type-face)")
                        nil t)
                   (replace-match "(jmt--c/put-type-face \\1)" t)
                   t)))))

        (jmt-x (display-warning 'java-mode-tamed (error-message-string x) :error))))

    ;; Initialize the buffer
    ;; ─────────────────────
    (jmt-set-for-buffer 'c-maybe-decl-faces
         (append c-maybe-decl-faces
                 '('jmt-modifier-keyword
                   'jmt--type
                   'jmt-type-declaration
                   'jmt-type-parameter-declaration
                   'jmt-type-reference
                   'jmt--type-reference-in-parameter-list)))
    (let ((level (font-lock-value-in-major-mode font-lock-maximum-decoration)))
      (set 'jmt--is-level-3 (or (eq level t) (and (numberp level) (>= level 3)))))

    (make-local-variable 'font-lock-extra-managed-props); With ‘the same value’ it ‘previously had’.
    (push 'jmt-stabilized font-lock-extra-managed-props)

    (jmt-set-for-buffer 'font-lock-defaults
         '((java-font-lock-keywords-1; 0 or nil    The alternative values of `font-lock-keywords`,
            java-font-lock-keywords-1; 1           each ordered according to the value of `font-lock-
            jmt-new-fontifiers-2     ; 2           -maximum-decoration` that selects it.  [MD]
            jmt-new-fontifiers-3)))) ; 3 or t



  (provide 'java-mode-tamed))



;; NOTES
;; ─────
;;   ↑A · This marks code section *Annotation* of `jmt-specific-fontifiers-3` and all other code
;;        that depends on its prior execution.
;;
;;   ↑T · This marks code section *Type name* of `jmt-specific-fontifiers-3` and all other code
;;        that depends on its prior execution.
;;
;;   ←CW  Backward across commentary and whitespace.
;;
;;   AST  At-sign as a token.  ‘It is possible to put whitespace between it and the TypeName,
;;        but this is discouraged as a matter of style.’
;;        https://docs.oracle.com/javase/specs/jls/se13/html/jls-9.html#jls-9.7
;;
;;   CW→  Forward across commentary and whitespace.
;;
;;   NAC  Not allowing for comments.  If ever that proves necessary in practice, then the code here
;;        (and its facing of type references in type parameter lists) might have to be removed from
;;        `jmt--c/put-type-face` to a tamed fontifier where the speed constraints are more relaxed.
;;        Then too the fontification of type parameter lists would need a different stabilization,
;;        e.g. using text property `jmt-stabilized`.
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
;;   RP · Replacement face.  Every tamed face used by `java-mode-tamed` to override and replace a face
;;        earlier applied by Java mode (replacement face) ultimately inherits from the face it replaces.
;;        Function `jmt-faces-are-equivalent` depends on this.
;;
;;   SF · Stuck face.  The use of text property `jmt-stabilized` and other stabilization guards
;;        may cause certain faces to become stuck on occaision.  A viable workaround in the event
;;        would be to delete and re-type the affected text, which tends to be short in length.
;;
;;   SL · Restricting the fontifier to a single line.  Multi-line fontifiers can be hairy.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Multiline-Font-Lock.html
;;
;;   TA · See `TypeArgument`.  https://docs.oracle.com/javase/specs/jls/se13/html/jls-4.html#jls-4.5.1
;;
;;   TF · Tamed face.  Ultimately every face defined by `java-mode-tamed` (tamed face) inherits from a
;;        face defined elsewhere (untamed ancestral face).  Function `jmt-untamed-face` depends on this.
;;
;;   TP · See `TypeParameter`.  https://docs.oracle.com/javase/specs/jls/se13/html/jls-4.html#jls-4.4
;;
;;   UF · Unstable faces `jmt--type` and `jmt-type-reference`.  Certain applications of these faces
;;        may be mutually unstable, alternating at times between one and the other.  This was seen,
;;        for instance, in the facing of the identifier in the sequence `new Precounter` here:
;;        https://github.com/Michael-Allan/waymaker/blob/3eaa6fc9f8c4137bdb463616dd3e45f340e1d34e/waymaker/top/android/ForestCache.java#L493


;; - - - - - - - - - -
;; Local Variables:
;; byte-compile-warnings: (not make-local)
;; End:
                                       ;;; Copyright © 2019 Michael Allan and contributors.  Licence MIT.
