;; Java Mode Tamed  │ -*- lexical-binding: t; -*-
;;
;; This is the definition of Java Mode Tamed, a derived major mode designed to give the user more control
;; over the Java mode that comes bundled with Emacs, particularly in regard to syntax highlighting.
;;
;;
;; USER INSTRUCTIONS
;; ─────────────────
;;   See `./user_instructions.brec` or `http://reluk.ca/project/Java/Emacs/user_instructions.brec`.
;;
;;
;; NOTES  (see at bottom)
;; ─────


(eval-when-compile
  (require 'cc-mode)
  (require 'cl-lib))



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  P r e l i m i n a r y   d e c l a r a t i o n s
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(defun jmt-make-Javadoc-tag-facing (f)
  "Makes a face property for a Javadoc tag using F (face symbol) as a base."
  (list f 'font-lock-doc-face)); [PDF]



(defvar jmt-name-character-set); [FV]
(defvar jmt-value-tag-name-f)



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════
;;  D e c l a r a t i o n s   i n   l e x i c o g r a p h i c   o r d e r
;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(defgroup delimiter-faces nil "\
Faces for Java separators and other delimiters."
  :group 'java-mode-tamed
  :prefix "jmt-")



(defgroup javadoc-faces nil "\
Faces for Java documentation comments."
  :group 'java-mode-tamed
  :prefix "jmt-")



(declare-function java-font-lock-keywords-2 "cc-fonts" ()); [FD]
(declare-function java-font-lock-keywords-3 "cc-fonts" ())



(defgroup java-mode-tamed nil "\
Customizable items of Java Mode Tamed."
  :group 'languages :group 'faces
  :prefix "jmt-"
  :link '(url-link "http://reluk.ca/project/Java/Emacs/"))



(defface jmt-angle-bracket
  `((t . (:inherit jmt-bracket))) "\
The face for an angle bracket, ‘<’ or ‘>’."
  :group 'delimiter-faces)



(defface jmt-annotation-delimiter; ↙ Only for sake of replacement subface `jmt-annotation-mark`.
  `((t . (:inherit c-annotation-face))) "\
The face for the ‘@’, ‘(’ and ‘)’ delimiters of annotation.
Customize it to better distinguish the delimiters from the content
they delimit; making them more prominent or less prominent, for example.
See also ‘jmt-delimiter’ and the faces that inherit from it."
  :group 'delimiter-faces)



(defface jmt-annotation-mark; [RF]
  `((t . (:inherit jmt-annotation-delimiter))) "\
The face for the ‘@’ symbol denoting annotation."
  :group 'delimiter-faces)



(defface jmt-annotation-package-name; [MDF, RF]
  `((t . (:inherit jmt-package-name))) "\
The face for each segment of a package name in an annotation type reference.
It defaults to ‘jmt-package-name’; customize it if the default fits poorly
with your other annotation faces."
  :group 'java-mode-tamed)



(defface jmt-annotation-string; [BC, LF, RF]
  `((t . (:inherit font-lock-string-face))) "\
The face for a string in an annotation qualifier.  It defaults
to ‘font-lock-string-face’; customize it if the default fits poorly
with your other annotation faces."
  :group 'java-mode-tamed)



(defface jmt-annotation-string-delimiter; [BC, LF, RF]
  `((t . (:inherit jmt-string-delimiter))) "\
The face for a string delimiter in an annotation qualifier.  It defaults
to ‘jmt-string-delimiter’; customize it if the default fits poorly with your
other annotation faces."
  :group 'delimiter-faces)



(defface jmt-annotation-qualifier
  `((t . (:inherit c-annotation-face))) "\
The face for the element assignments of annotation.  Customize it
e.g. to give the assignments less prominence than the ‘c-annotation-face’
of the preceding type name."
  :group 'java-mode-tamed)



(defface jmt-block-tag-name; [NDF, RF]
  `((t . (:inherit jmt-Javadoc-tag-name))) "\
The face for the proper identifier of a Javadoc block tag."
  :group 'javadoc-faces)

(defconst jmt-block-tag-name-f (jmt-make-Javadoc-tag-facing 'jmt-block-tag-name))



(defface jmt-block-tag-parameter; [CI]
  `((t . (:inherit font-lock-doc-face))) "\
The face for a non-descriptive parameter of a Javadoc block tag.
See also subfaces ‘jmt-param-tag-parameter’ and ‘jmt-throws-tag-parameter’."
  :group 'javadoc-faces)



(defface jmt-boilerplate-keyword; [MDF, RF]
  `((t . (:inherit jmt-principal-keyword))) "\
The face for the keyword of a formal Java declaration in the preamble
of a compilation unit."
  :group 'keyword-faces)



(defface jmt-bracket
  `((t . (:inherit jmt-delimiter))) "\
The face for a bracket.  See also ‘jmt-angle-bracket’, ‘jmt-curly-bracket’,
‘jmt-round-bracket’ and ‘jmt-square-bracket’."
  :group 'delimiter-faces)



(defun jmt--c/try-putting-face (beg end face)
  "Calls ‘c-put-font-lock-face’ on condition that either a) BEG and END delimit
a region under fontification by Font Lock, or b) the present buffer is untamed."
  ;; Called from within a monkey-patched version of the underlying Java-mode code, this function prevents
  ;; an endless tug of war between Java mode and Java Mode Tamed, which otherwise would destabilize tamed
  ;; faces, causing them alternately to appear and disappear.
  (defvar jmt--is-level-3); [FV]
  (defvar jmt--present-fontification-beg)
  (defvar jmt--present-fontification-end)
  (if (and jmt--is-level-3
           (eq major-mode 'java-mode-tamed))
      (unless (or
               (> end jmt--present-fontification-end)
               (< beg jmt--present-fontification-beg))
        (c-put-font-lock-face beg end face))
    (c-put-font-lock-face beg end face)))



(defface jmt-curly-bracket
  `((t . (:inherit jmt-bracket))) "\
The face for a curly bracket, ‘{’ or ‘}’."
  :group 'delimiter-faces)



(defface jmt-delimiter nil "\
The face for a delimiter not already faced by Java mode.  Customize it
to better distinguish the delimiters from the content they delimit; making them
more prominent or less prominent, for example.  See also subfaces ‘jmt-bracket’
‘jmt-separator’.  And for delimiters that *are* already faced by Java mode,
see ‘jmt-annotation-delimiter’, ‘jmt-annotation-mark’, ‘jmt-string-delimiter’
and ‘font-lock-comment-delimiter-face’."
  :group 'delimiter-faces)



(defvar jmt--early-initialization-was-begun nil)



(defface jmt-expression-keyword; [MDF, RF]
  `((t . (:inherit jmt-principal-keyword))) "\
The face for the keyword of an operator or other element
of a formal Java expression."
  :group 'keyword-faces)



(defvar jmt-f nil); [GVF]



(defun jmt-faces-are-equivalent (f1 f2)
  "Answers whether F1 and F2 (face symbols) should be treated as equivalent
by the underlying (Java-mode) code."
  (eq (jmt-untamed-face f1) (jmt-untamed-face f2))); [RF]



(defface jmt-HTML-end-tag-name; [NDF, RF]
  `((t . (:inherit jmt-HTML-tag-name))) "\
The face for the tag name in the end tag of an HTML element
in a Javadoc comment."
  :group 'javadoc-faces)

(defconst jmt-HTML-end-tag-name-f (jmt-make-Javadoc-tag-facing 'jmt-HTML-end-tag-name))



(defface jmt-HTML-start-tag-name; [NDF, RF]
  `((t . (:inherit jmt-HTML-tag-name))) "\
The face for the tag name in the start tag of an HTML element
in a Javadoc comment."
  :group 'javadoc-faces)

(defconst jmt-HTML-start-tag-name-f (jmt-make-Javadoc-tag-facing 'jmt-HTML-start-tag-name))



(defface jmt-HTML-tag-name; [NDF, RF]
  `((t . (:inherit jmt-Javadoc-tag-name))) "\
The face for the tag name of an HTML element in a Javadoc comment.
See also subfaces ‘jmt-HTML-start-tag-name’ and ‘jmt-HTML-end-tag-name’."
  :group 'javadoc-faces)



(defface jmt-inline-rendered-parameter; [NDF, RF]
  `((t . (:inherit jmt-inline-tag-parameter))) "\
The face for a rendered parameter of a Javadoc inline tag; one that appears
more-or-less literally in the resulting Javadocs, that is."
  :group 'javadoc-faces)

(defconst jmt-inline-rendered-parameter-f (jmt-make-Javadoc-tag-facing 'jmt-inline-rendered-parameter))



(defface jmt-inline-tag-name; [NDF, RF]
  `((t . (:inherit jmt-Javadoc-tag-name))) "\
The face for the proper identifier of a Javadoc inline tag."
  :group 'javadoc-faces)

(defconst jmt-inline-tag-name-f (jmt-make-Javadoc-tag-facing 'jmt-inline-tag-name))



(defface jmt-inline-tag-parameter; [NDF, RF]
  `((t . (:inherit jmt-Javadoc-tag))) "\
The face for a parameter of a Javadoc inline tag, or attribute of an HTML tag.
See also subface ‘jmt-inline-rendered-parameter’. And for block tags,
see ‘jmt-block-tag-parameter’."
  :group 'javadoc-faces)

(defconst jmt-inline-tag-parameter-f (jmt-make-Javadoc-tag-facing 'jmt-inline-tag-parameter))



(defun jmt-is-annotation-ish-before (p)
  "Answers whether the position before P (integer) might be within annotation."
  (let ((f (get-text-property (1- p) 'face)))
    (or (eq 'c-annotation-face (jmt-untamed-face f))
        (eq 'jmt-annotation-string f)
        (eq 'jmt-annotation-string-delimiter f)
        (eq 'jmt-annotation-package-name f); A package name in an annotation type reference.
        (and (eq 'jmt-separator f) (char-equal ?. (char-before p)))))); A dot ‘.’ in the package name.



(defun jmt-is-annotation-terminal-face (f)
  "Answers whether F (face symbol) is a face that might be set
on the last character of annotation."
  (eq 'c-annotation-face (jmt-untamed-face f)))



(defun jmt-is-Java-mode-tag-faced (v)
  "Answers whether property value V (face symbol or list) is one which might have
been set on a Javadoc tag by the underlying (Java-mode) code."
  (and (consp v); Testing for precisely `(font-lock-constant-face font-lock-doc-face)`. [PDF]
       (eq 'font-lock-constant-face (car v))
       (eq 'font-lock-doc-face (car (setq v (cdr v))))
       (eq nil (cdr v))))



(defun jmt-is-Java-mode-type-face (f)
  "Answers whether F (face symbol) is a type face which might have been set
by the underlying (Java-mode) code."
  (eq f 'font-lock-type-face)); Java mode alone sets this face.



(defvar-local jmt--is-level-3 nil); An in-buffer cache of this boolean flag.  It works only because any
  ;;; ‘customization of `font-lock-maximum-decoration` should be done *before* the file is visited’.
  ;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html



(defun jmt-is-type-definitive-keyword (s)
  "Answers whether string S is the principal keyword of a type definition."
  (or (string= s "class")
      (string= s "interface")
      (string= s "enum")))



(defun jmt-is-type-modifier-keyword (s)
  "Answers whether string S is a type definition modifier in keyword form,
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



(defface jmt-Javadoc-outer-delimiter; [CI]
  `((t . (:inherit font-lock-doc-face))) "\
The face for the outermost delimiters `/**` and `*/` that between them
contain a Javadoc comment, and for the left-marginal asterisks `*`
that may lead any of its lines.  Customize it to better distinguish
the delimiters from the content they delimit; making them more prominent
or less prominent, for example."
  :group 'delimiter-faces :group 'javadoc-faces)



(defface jmt-Javadoc-tag; [NDF, RF]
  `((t . (:inherit font-lock-constant-face))) "\
The face for a Javadoc or HTML tag embedded in a Javadoc comment.
It inherits from ‘font-lock-constant-face’; customize it to distinguish
Javadoc tags from other constructs that use ‘font-lock-constant-face’.
See also subfaces ‘jmt-Javadoc-tag-delimiter’, ‘jmt-Javadoc-tag-name’
and  ‘jmt-inline-tag-parameter’."
  :group 'javadoc-faces)



(defface jmt-Javadoc-tag-delimiter; [NDF, RF]
  `((t . (:inherit jmt-Javadoc-tag))) "\
The face for the ‘@’, ‘{’ and ‘}’ delimiters of a Javadoc tag,
and the ‘<’, ‘</’, ‘/>’ and ‘>’ delimiters of an HTML tag.
Customize it to better distinguish the delimiters from the content
they delimit; making them more prominent or less prominent, for example.
See also subface ‘jmt-Javadoc-tag-mark’."
  :group 'javadoc-faces)

(defconst jmt-Javadoc-tag-delimiter-f (jmt-make-Javadoc-tag-facing 'jmt-Javadoc-tag-delimiter))



(defface jmt-Javadoc-tag-mark; [NDF, RF]
  `((t . (:inherit jmt-Javadoc-tag-delimiter))) "\
The face for the ‘@’ symbol denoting a Javadoc tag."
  :group 'javadoc-faces)

(defconst jmt-Javadoc-tag-mark-f (jmt-make-Javadoc-tag-facing 'jmt-Javadoc-tag-mark))



(defface jmt-Javadoc-tag-name; [NDF, RF]
  `((t . (:inherit jmt-Javadoc-tag))) "\
The face for the proper identifier of a Javadoc or HTML tag.  See also subfaces
‘jmt-block-tag-name’, ‘jmt-inline-tag-name’ and ‘jmt-HTML-tag-name’."
  :group 'javadoc-faces)

(defconst jmt-Javadoc-tag-name-f (jmt-make-Javadoc-tag-facing 'jmt-Javadoc-tag-name))



(defun jmt-keyword-face (keyword beg end)
  "Returns the face (symbol) proper to the given Java keyword (string),
given to be present in the buffer from position BEG (inclusive number)
to END (exclusive number).  Leaves point indeterminate."
  (defvar jmt-keyword-face-alist); [FV]
  (let ((f (assoc keyword jmt-keyword-face-alist)))
    (if (not f) 'jmt-principal-keyword; Returning either a default face,
      (setq f (cdr f))                ; or, from `jmt-keyword-face-alist`,
      (if (not (functionp f)) f       ; a face either directly named
        (funcall f beg end)))))       ; or given by a named function.



(defconst jmt-keyword-face-alist
  '(
    ;; Frequent
    ;; ────────
    ("assert"       .     jmt-principal-keyword); Of a statement.
;;; ("boolean"      .          jmt-type-keyword); (but faced rather as a type by Java mode)
    ("break"        .     jmt-principal-keyword); Of a statement.
    ("else"         .     jmt-principal-keyword); Of a statement clause.
    ("final"        .     jmt-qualifier-keyword)
    ("if"           .     jmt-principal-keyword); Of a statement.
    ("import"       .   jmt-boilerplate-keyword)
;;; ("int"          .          jmt-type-keyword); (but faced rather as a type by Java mode)
;;; ("long"         .          jmt-type-keyword); (but faced rather as a type by Java mode)
    ("private"      .     jmt-qualifier-keyword)
    ("public"       .     jmt-qualifier-keyword)
    ("return"       .     jmt-principal-keyword); Of a statement.
    ("static"       .   jmt-keyword-face-static); (q.v.)

    ;; Infrequent; typically a few times per buffer
    ;; ──────────
    ("abstract"     .     jmt-qualifier-keyword)
    ("case"         .     jmt-principal-keyword); Of a statement clause.
    ("catch"        .     jmt-principal-keyword); Of a statement clause.
;;; ("char"         .          jmt-type-keyword); (but faced rather as a type by Java mode)
    ("class"        .    jmt-keyword-face-class); (q.v.)
    ("continue"     .     jmt-principal-keyword); Of a statement.
;;; ("float"        .          jmt-type-keyword); (but faced rather as a type by Java mode)
    ("for"          .     jmt-principal-keyword); Of a statement.
    ("new"          .    jmt-expression-keyword)
    ("protected"    .     jmt-qualifier-keyword)
    ("super"        .    jmt-expression-keyword)
    ("synchronized" .     jmt-keyword-face-sync); (q.v.)
    ("this"         .    jmt-expression-keyword)
    ("throw"        .     jmt-principal-keyword); Of a statement.
    ("throws"       .     jmt-qualifier-keyword)
    ("try"          .     jmt-principal-keyword); Of a statement.
;;; ("void"         .          jmt-type-keyword); (but faced rather as a type by Java mode)
    ("while"        .     jmt-principal-keyword); Of a statement.

    ;; Rare; typically once per buffer, if at all
    ;; ────
;;; ("_"            .       jmt-misused-keyword); (reserved, yet unfaced by Java mode)
;;; ("byte"         .          jmt-type-keyword); (but faced rather as a type by Java mode)
    ("const"        .     jmt-qualifier-keyword); (but reserved)
    ("enum"         .     jmt-principal-keyword); Of a type definition.
    ("default"      .     jmt-principal-keyword); Of a statement clause.
    ("do"           .     jmt-principal-keyword); Of a statement.
;;; ("double"       .          jmt-type-keyword); (but faced rather as a type by Java mode)
    ("extends"      .     jmt-qualifier-keyword)
    ("finally"      .     jmt-principal-keyword); Of a statement clause.
    ("goto"         .     jmt-principal-keyword); (but reserved)
    ("implements"   .     jmt-qualifier-keyword)
    ("instanceof"   .    jmt-expression-keyword)
    ("interface"    .     jmt-principal-keyword); Of a type definition.
    ("native"       .     jmt-qualifier-keyword)
    ("package"      .   jmt-boilerplate-keyword)
;;; ("short"        .          jmt-type-keyword); (but faced rather as a type by Java mode)
    ("strictfp"     .     jmt-qualifier-keyword)
    ("switch"       .     jmt-principal-keyword); Of a statement.
    ("transient"    .     jmt-qualifier-keyword)
    ("volatile"     .     jmt-qualifier-keyword)
    ) "\
A list of cons cells of Java keywords (string @ car) each with the name
(symbol @ cdr) of either its proper face, or a function in the form
of ‘jmt-keyword-face-class’ that returns a face symbol.  The list excludes
keywords that Java mode does not face with ‘font-lock-keyword-face’.")



(defun jmt-keyword-face-class (beg _end)
  "Returns the face (symbol) to give to the `class` keyword present
in the buffer from position BEG (inclusive number) to _END (exclusive number).
Leaves point indeterminate."
  (goto-char beg)
  (forward-comment most-negative-fixnum); [←CW]
  (if (eq ?. (char-before)); [NCE]
      'jmt-expression-keyword
        ;;; https://docs.oracle.com/javase/specs/jls/se13/html/jls-15.html#jls-ClassLiteral
    'jmt-principal-keyword)); Of a type definition.



(defun jmt-keyword-face-static (beg end)
  "Returns the face (symbol) to give to the `static` keyword present
in the buffer from position BEG (inclusive number) to _END (exclusive number).
Leaves point indeterminate."
  (goto-char beg)
  (forward-comment most-negative-fixnum); [←CW]
  (setq end (point)); The presumed end of the preceding keyword.
  (if (and (< (skip-chars-backward jmt-name-character-set) 0)
           (string= "import" (buffer-substring-no-properties (point) end)))
      'jmt-boilerplate-keyword; In a static import declaration. [SI]
    'jmt-qualifier-keyword)); Elsewhere.



(defun jmt-keyword-face-sync (_beg end)
  "Returns the face (symbol) to give to the `synchronized` keyword present
in the buffer from position _BEG (inclusive number) to END (exclusive number).
Leaves point indeterminate."
  (goto-char end)
  (forward-comment most-positive-fixnum); [CW→]
  (if (eq ?\( (char-after)); [NCE]
      'jmt-principal-keyword; Of a statement.
        ;;; https://docs.oracle.com/javase/specs/jls/se13/html/jls-14.html#jls-14.19
    'jmt-qualifier-keyword))



(defvar jmt--late-initialization-was-begun nil)



(defun jmt-message (format-string &rest arguments)
  "Calls function ‘message’ without translation of embedded \\=`\\=`\\=`
and \\=`\\='\\=` quotes."
  (message "%s" (apply 'format format-string arguments)))



(defconst jmt-name-character-set "[:alnum:]_$" "\
The set of characters from which a Java identifier may be formed.")
  ;;; https://docs.oracle.com/javase/specs/jls/se13/html/jls-3.html#jls-3.8



(defface jmt-named-literal; [MDF, RF]
  `((t . (:inherit font-lock-constant-face))) "\
The face for literal of type boolean or null; namely `true`, `false` or `null`.
It inherits from ‘font-lock-constant-face’; customize it to distinguish named
literals from other constructs that use ‘font-lock-constant-face’, or to subdue
the facing if you prefer to have these literals not stand out."
  :group 'java-mode-tamed)



(defun jmt-new-fontifiers-2 ()
  "Builds a ‘font-lock-keywords’ list for fast, untamed highlighting.
See also ‘java-font-lock-keywords-1’, which is for minimal untamed highlighting."
  (java-font-lock-keywords-2)); [L2U]



(defun jmt-new-fontifiers-3 ()
  "Builds a ‘font-lock-keywords’ list for accurate, tamed highlighting."
  (defvar jmt-specific-fontifiers-3); [FV]
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
               ;; Dud fontifier: works under Java mode, fails under Java Mode Tamed unless
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



(defvar jmt-p nil); [GVF]



(defface jmt-package-name; [MDF, RF]
  `((t . (:inherit font-lock-constant-face))) "\
The face for each segment of a package name in a type reference.  It inherits
from ‘font-lock-constant-face’; customize it to distinguish package names from
other constructs that use ‘font-lock-constant-face’."
  :group 'java-mode-tamed)



(defface jmt-package-name-declared; [MDF, RF]
  `((t . (:inherit jmt-package-name))) "\
The face for each segment of a package name in a package declaration, as op-
posed to a type reference.  Customize it to better distinguish between the two."
  :group 'java-mode-tamed)



(defface jmt-param-tag-parameter
  `((t . (:inherit jmt-block-tag-parameter))) "\
The face for the parameter-name parameter of a Javadoc `param` tag.
An exception applies to type parameters; for those, see instead
‘jmt-type-variable-tag-parameter’."
  :group 'javadoc-faces)



(defun jmt--patch (source-file source-base-name function-symbol patch-function)
  "Called from within a temporary buffer, this function monkey patches
the function FUNCTION-SYMBOL, originally from file SOURCE-FILE (a string,
which has the given BASE-NAME), using the named PATCH-FUNCTION.  PATCH-FUNCTION
must return t on success, nil on failure.  Syntactically the configuration
of the temporary buffer must be equivalent to that of Emacs Lisp mode."; [ELM]
  (unless (functionp function-symbol)
    (signal 'jmt-x `("No such function loaded" ,function-symbol)))
  (let ((load-file (symbol-file function-symbol)))
    (unless (string= (file-name-base load-file) source-base-name)
      (signal 'jmt-x `("Function loaded from file of base name contradictory to source file"
                        ,function-symbol ,load-file ,source-file))))
  (goto-char (point-min))
  (unless (re-search-forward
           (concat "^(defun[[:space:]\n]+" (symbol-name function-symbol) "[[:space:]\n]*(") nil t)
    (signal 'jmt-x `("Function definition not found in source file"
                      ,function-symbol ,source-file)))
  (let ((beg (match-beginning 0))); Narrow the temporary buffer to the function definition alone:
    (narrow-to-region beg (scan-lists beg 1 0)))
  (goto-char (point-min))
  (unless (funcall patch-function); Patching the definition.
    (signal 'jmt-x `("Patch failed to apply" ,function-symbol)))
  (let ((original-was-compiled (byte-code-function-p (symbol-function function-symbol))))
    (eval-buffer); Redefining the function to the patched version.
;;; (delete-region point-min point-max); Removing the definition, in hope it speeds later patching.
;;;;;; Or might the deletion time exceed the time saved?
    (widen)
    (when original-was-compiled; Then recompile the redefined function.
      (run-with-idle-timer; Recompile it during idle time.  This might improve initial load times,
       1.3 nil            ; though early timing tests (with a single patch) showed no such effect.
       (lambda ()
         (unless (byte-compile function-symbol)
           (jmt-message "(java-mode-tamed): Failed to recompile monkey-patched function `%s`"
                         (symbol-name function-symbol))))))))



(defun jmt-preceding->-marks-generic-return-type ()
  "Answers whether the ‘>’ character before point could be a delimiter within a
function definition, namely the trailing delimiter of a list of type variables
for the function’s return type, making it a *generic* return type.  May move point."
  (when
      (condition-case _x
          (progn (forward-sexp -1) t); Move backward to the front of the leading delimiter.
        (scan-error nil))
    (forward-comment most-negative-fixnum); [←CW]
    (not (eq (char-before) ?.)))); (not `char-equal`, in case nil)
      ;;; Here a `.` would indicate a method call, as opposed to a definition.



(defvar jmt--present-fontification-beg 0)
(defvar jmt--present-fontification-end 0); Non-zero only when Font Lock is fontifying via
  ;;; the default `font-lock-fontify-region-function`.



(defface jmt-principal-keyword; [MDF, RF]
  `((t . (:inherit font-lock-keyword-face))) "\
The face for the principal keyword of a definition.
Cf. ‘jmt-qualifier-keyword’.  See also subfaces
‘jmt-boilerplate-keyword’ and ‘jmt-expression-keyword’."
  :group 'keyword-faces)



(defvar jmt-q nil); [GVF]



(defface jmt-qualifier-keyword; [MDF, RF]
  `((t . (:inherit font-lock-keyword-face))) "\
The face for a secondary keyword in a definition.
Cf. ‘jmt-principal-keyword’."
  :group 'keyword-faces)



(defface jmt-round-bracket
  `((t . (:inherit jmt-bracket))) "\
The face for a round bracket, ‘(’ or ‘)’."
  :group 'delimiter-faces)



(defface jmt-separator
  `((t . (:inherit jmt-delimiter))) "\
The face for a separator: a comma ‘,’ semicolon ‘;’ colon ‘:’ or dot ‘.’."
  :group 'delimiter-faces)



(defun jmt-set-for-buffer (variable value)
  "Sets VARIABLE (a symbol) to VALUE.  Signals an error if the setting
is not buffer local."
  (set variable value)
  (cl-assert (local-variable-p variable)))



(defface jmt-shebang
  `((t . (:inherit font-lock-comment-delimiter-face))) "\
The face for a shebang ‘#!’."
  :group 'shebang-faces)



(defface jmt-shebang-body
  `((t . (:inherit font-lock-comment-face))) "\
The face for the body of a shebang line, exclusive of the shebang itself
and any trailing comment."
  :group 'shebang-faces)



(defface jmt-shebang-comment
  `((t . (:inherit font-lock-comment-face))) "\
The face for the body of a trailing comment in a shebang line,
in case of an `env` interpreter."
  :group 'shebang-faces
  :link '(url-link "https://www.gnu.org/software/coreutils/manual/html_node/env-invocation.html"))



(defface jmt-shebang-comment-delimiter
  `((t . (:inherit font-lock-comment-delimiter-face))) "\
The face for the delimiter ‘\\c’ of a trailing comment in a shebang line,
in case of an `env` interpreter."
  :group 'shebang-faces
  :link '(url-link "https://www.gnu.org/software/coreutils/manual/html_node/env-invocation.html"))



(defconst jmt-specific-fontifiers-3
  (list


   ;; ══════════
   ;; Annotation  [A, T↓]
   ;; ══════════

   (list; Fontify each instance of annotation, overriding any misfontification of Java mode.
    (lambda (limit)
      (catch 'to-fontify
        (let ((m1-beg (point)); Presumed start of leading annotation mark ‘@’.
              (m1-beg-limit (1- limit)); Room for two characters, the minimal length.
              eol face m1-end m2-beg m2-end m3-beg m3-end m4-beg m4-end m5-beg m5-end)
          (while (< m1-beg m1-beg-limit)
            (setq m1-end (1+ m1-beg))
            (if (not (char-equal ?@ (char-after m1-beg)))
                (setq m1-beg m1-end)
              (goto-char m1-end)
              (catch 'is-annotation
                (when (eolp) (throw 'is-annotation nil)); [SL]
                (while; Capture as group 2 the simple annotation name.
                    (progn
                      (skip-syntax-forward "-" limit); Though unconventional, whitespace is allowed
                        ;;; between ‘@’ and name.  Nevertheless this fontifier excludes newlines; also
                        ;;; commentary, which would be perverse here, not worth coding for. [AST, SL]
                      (setq m2-beg (point))
                      (skip-chars-forward jmt-name-character-set limit)
                      (setq m2-end (point))
                      (unless (< m2-beg m2-end) (throw 'is-annotation nil))
                      (setq face (get-text-property m2-beg 'face))
                      (if (eq face 'font-lock-constant-face); [P↓] Then the (mis)captured name should
                          (progn; be dot terminated, so forming a segment of a package name. [PPN]
                            (skip-syntax-forward "-" limit); [SL]
                            (unless (eq ?. (char-after)); [NCE]
                              (throw 'is-annotation nil))
                            (forward-char); Past the ‘.’.
                            t); Continuing the loop, so skipping past this segment of the name.
                        (unless (or (eq face nil); The most common case.  Else a misfontification:
                                    (eq face 'font-lock-function-name-face); This one occurs in the case,
                                      ;;; for instance, of an empty `()` annotation qualifier.
                                    (jmt-is-Java-mode-type-face face)); [T↓]
                          (throw 'is-annotation nil))
                        nil))); Quitting the loop, having matched the simple annotation name.
                (skip-syntax-forward "-" limit); [SL]
                (when (eq ?\( (char-after)); [NCE]
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
                    (goto-char m5-end)
                    (throw 'to-fontify t))); With point (still) at `m5-end` as Font Lock stipulates.

                ;; Unqualified
                ;; ───────────
                (set-match-data (list m1-beg m2-end m1-beg m1-end m2-beg m2-end (current-buffer)))
                (goto-char m2-end)
                (throw 'to-fontify t))

              (setq m1-beg (point)))))
        nil))
    '(1 'jmt-annotation-mark t) '(2 'c-annotation-face t) '(3 'jmt-annotation-delimiter t t); [QTF]
    '(4 'jmt-annotation-qualifier nil t)                  '(5 'jmt-annotation-delimiter t t))



   ;; ═══════
   ;; Keyword  [K, T↓]
   ;; ═══════

   (cons; Reface each Java keyword as defined in `jmt-keyword-face-alist`.
    (let (match-beg match-end)
      (lambda (limit)
        (setq match-beg (point)); Presumptively.
        (catch 'to-reface
          (while (< match-beg limit)
            (setq match-end (next-single-property-change match-beg 'face (current-buffer) limit))
            (when (eq 'font-lock-keyword-face (get-text-property match-beg 'face))
              (set 'jmt-f (jmt-keyword-face
                           (buffer-substring-no-properties match-beg match-end) match-beg match-end))
              (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
              (throw 'to-reface t))
            (setq match-beg match-end))
          nil)))
    '(0 jmt-f t))


   (cons; Fontify each `assert` keyword that was misfaced by Java mode, or incorrectly left unfaced.
    (let (f match-beg)
      (lambda (limit)
        (catch 'to-reface
          (while (re-search-forward "\\<assert\\>" limit t)
            (setq match-beg (match-beginning 0)
                  f (get-text-property match-beg 'face))
            (when (or (eq f nil) (jmt-is-Java-mode-type-face f)); [T↓]
                ;; Only identifiers left unfaced or misfaced as type names have been seen.  Unfaced is
                ;; the more common.  For an instance of misfacing, see `assert stators.getClass()`. [AM]
                ;; [https://github.com/Michael-Allan/waymaker/blob/3eaa6fc9f8c4137bdb463616dd3e45f340e1d34e/waymaker/gen/KittedPolyStatorSR.java#L58]
              (set 'jmt-f (jmt-keyword-face "assert" match-beg (match-end 0)))
              (throw 'to-reface t)))
          nil)))
    '(0 jmt-f t))



   ;; ═════════
   ;; Type name  [↑K, T]
   ;; ═════════

   (cons; Unface each terminal token of a static import incorrectly faced as a type name by Java mode.
    (let (i j)
      (lambda (limit)
        (setq i (point)); Presumptively.
        (catch 'to-reface
          (while (< i limit)
            (setq j (next-single-property-change i 'face (current-buffer) limit))
            (when (and (eq 'jmt-boilerplate-keyword (get-text-property i 'face)); [↑K]
                       (string= "static" (buffer-substring-no-properties i j)))
              (goto-char j)
              (when (and (re-search-forward
                          (concat "[[:space:]\n." jmt-name-character-set "]+\\_>"
                              ;;;  └───────────────────────────────────────┘
                              ;;;                 TypeName                       [SI]

                                  "\\.\\([" jmt-name-character-set "]+\\);")
                              ;;;    ·   └───────────────────────────┘   ·
                              ;;;   dot            Identifier            ;
                          limit t)
                         (jmt-is-Java-mode-type-face (get-text-property (match-beginning 1) 'face)))
                (throw 'to-reface t)))
            (setq i j))
          nil)))
    '(1 'default t))


   (list; Reface each Java type name using either `jmt-type-definition` or  `jmt-type-reference`.
    (lambda (limit)
      (catch 'to-reface
        (while (< (point) limit)
          (let* ((match-beg (point)); Presumptively.
                 (match-end (next-single-property-change match-beg 'face (current-buffer) limit)))
            (when (jmt-is-Java-mode-type-face (get-text-property match-beg 'face))
              (forward-comment most-negative-fixnum); [←CW]

              ;; Either defining a type
              ;; ───────────────
              (when; A keyword `class`, `enum` or `interface` directly precedes the type name.
                  (let ((p (point)))
                    (and (< (skip-chars-backward jmt-name-character-set) 0)
                         (jmt-is-type-definitive-keyword (buffer-substring-no-properties (point) p))))
                (set 'jmt-f 'jmt-type-definition)
                (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
                (throw 'to-reface t))

              ;; Or referring to one already defined
              ;; ────────────
              (set 'jmt-f 'jmt-type-reference)
              (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
              (throw 'to-reface t))

            (goto-char match-end)))
        nil))
    '(0 jmt-f t t))


   (cons; Face each name of a type definition that was incorrectly left unfaced by Java mode.
    (lambda (limit)
      (catch 'to-face
        (while (< (point) limit)
          (let* ((p (point))
                 (match-end (next-single-property-change p 'face (current-buffer) limit)))
            (when; A type definitive keyword (`class`, `enum` or `interface`) is found.
                (and (eq 'jmt-principal-keyword (get-text-property p 'face)); [↑K]
                     (jmt-is-type-definitive-keyword
                      (buffer-substring-no-properties (point) match-end)))
              (goto-char match-end)
              (forward-comment most-positive-fixnum); [CW→]
              (let ((match-beg (point)); Presumptively.
                    (annotation-count 0))
                (when (and (< match-beg limit)
                           (> (skip-chars-forward jmt-name-character-set limit) 0); A name follows.
                           (not (get-text-property match-beg 'face))); The name is unfaced.
                  (setq match-end (point))
                  (goto-char p); Back to the type definitive keyword.
                  (forward-comment most-negative-fixnum); [←CW]
                  (when (eq (char-before (point)) ?@); (and not nil)  A ‘@’ marks this definition
                    (backward-char); as that of an annotation type.  Move back past the ‘@’.
                    (forward-comment most-negative-fixnum)); [←CW]
                  (catch 'is-modifier; Thrown as nil on encountering *not* a type definition modifier.
                    (while t; Now point should (invariant) be directly after such a modifier.  So test:
                      (when (eq (char-before (point)) ?\)); (and not nil)  A list of anno-
                        (condition-case _x                ; tation parameters, presumeably.
                            (forward-sexp -1); Skip to the front of it.
                          (scan-error (throw 'is-modifier nil)))
                        (forward-comment most-negative-fixnum)); [←CW]
                          ;;; Holding still the (would be) loop invariant.
                      (setq p (point))
                      (when (= (skip-chars-backward jmt-name-character-set) 0)
                        (throw 'is-modifier nil))
                      ;; The modifier should be either a keyword or annotation.
                      (if (jmt-is-type-modifier-keyword (buffer-substring-no-properties (point) p))

                          ;; Keyword, the modifier is a keyword
                          ;; ───────
                          (if (= annotation-count 0)
                              (forward-comment most-negative-fixnum); [←CW]
                            (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
                            (throw 'to-face t)); The keyword precedes annotation.  With this.
                              ;;; Java mode fails at times to face the type name.  This was seen,
                              ;;; for instance, here in the sequence `public @ThreadSafe class ID`.
                              ;;; [https://github.com/Michael-Allan/waymaker/blob/3eaa6fc9f8c4137bdb463616dd3e45f340e1d34e/waymaker/spec/ID.java#L8`]
                              ;;;     It seems Java mode expects to find keywords *before* annotation,
                              ;;; which, although it ‘is customary’, is nevertheless ‘not required’.
                              ;;; [https://docs.oracle.com/javase/specs/jls/se13/html/jls-8.html#jls-8.1.1]
                              ;;; Here therefore the missing face is applied.

                        ;; Annotation, the modifier is an annotation modifier, or should be
                        ;; ──────────
                        (forward-comment most-negative-fixnum); [←CW]
                          ;;; A form unconventional, but allowed. [AST]
                        (unless (eq (char-before (point)) ?@); (and not nil)
                          (throw 'is-modifier nil))
                        (setq annotation-count (1+ annotation-count))
                        (backward-char); To before the ‘@’.
                        (forward-comment most-negative-fixnum))))))); [←CW]
            (goto-char match-end)))
        nil))
    '(0 'jmt-type-definition t)); [QTF]



   ;; ════════════
   ;; Package name, or a type name misfaced as such  [↑A, ↑K, P, T]
   ;; ════════════

   ;; Package declaration
   ;; ───────────────────
   (let; Reface each name segment of a package declararation using face `jmt-package-name-declared`.
       (face last-seg-was-found match-beg match-end)
     (list

      (lambda (limit); (1) Anchoring on the `package` keyword that begins each declaration.
        (setq match-beg (point)); Presumptively.
        (catch 'is-package-declaration
          (while (< match-beg limit)
            (setq match-end (next-single-property-change match-beg 'face (current-buffer) limit))
            (when (and (eq 'jmt-boilerplate-keyword (get-text-property match-beg 'face)); [↑K]
                       (string= "package" (buffer-substring-no-properties match-beg match-end)))
              (setq last-seg-was-found nil)
              (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
              (throw 'is-package-declaration t))
            (setq match-beg match-end))
          nil))

      (list; (2, anchored highlighter) Thence scanning rightward for each name segment.
       (lambda (limit)
         (catch 'to-reface
           (when last-seg-was-found (throw 'to-reface nil)); The last segment was already refaced.
           (while (< (point) limit); Now point should (invariant) be before any remaining name segment,
             (skip-syntax-forward "-" limit); with at most whitespace intervening. [PPN, SL]
             (setq match-beg (point))
             (skip-chars-forward jmt-name-character-set limit)
             (setq match-end (point))
             (unless (< match-beg match-end) (throw 'to-reface nil)); Malformed or multi-line. [SL]
             (skip-syntax-forward "-" limit); [SL]
             (if (eq ?. (char-after)); [NCE]
                 (forward-char); To the (would be) loop invariant.
               (setq last-seg-was-found t))
             (setq face (get-text-property match-beg 'face))
             (when (or (eq face nil); Java mode leaves unfaced all but the last segment.
                       (eq face 'font-lock-constant-face))
               (set-match-data (list match-beg (point) match-beg match-end (current-buffer)))
               (throw 'to-reface t)))
           nil))
       nil nil '(1 'jmt-package-name-declared t)))); [QTF]


   ;; Package qualifier on an annotation type reference
   ;; ─────────────────
   (let (match-beg match-end seg-beg seg-end); Reface each package name segment using
     (list                                   ; face `jmt-annotation-package-name`.

      ;; 1. Anchor on the simple type name of each annotation type reference
      ;; ─────────
      (lambda (limit)
        (setq match-beg (point)); Presumptively.
        (catch 'is-type-ref
          (while (< match-beg limit)
            (setq match-end (next-single-property-change match-beg 'face (current-buffer) limit))
            (when (eq 'c-annotation-face (get-text-property match-beg 'face)); [↑A]
              (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
              (set 'jmt-p match-beg); Saving the anchor’s bounds.
              (set 'jmt-q match-end)
              (throw 'is-type-ref t))
            (setq match-beg match-end))
          nil))

       ;; 3. Reface name segments by unstacking (from 2) the bounds of each
       ;; ───────────────────────
      (list; (anchored highlighter)
       (lambda (limit)
         (when (setq seg-beg (car jmt-f))
           (set 'jmt-f (cdr jmt-f)); Popping the bounds from the stack.
           (setq seg-end (car jmt-f))
           (set 'jmt-f (cdr jmt-f))
           (cl-assert (<= seg-end limit))
           (set-match-data (list seg-beg (goto-char seg-end) seg-beg seg-end (current-buffer)))
           t))

       ;; 2. Seek name segments leftward of the anchor, stacking the bounds of each  [PPN]
       ;; ─────────────────────
       '(let (seg-beg seg-end); (pre-form)
          (set 'jmt-f nil); The stack of bounds, two per segment, initially empty.
          (goto-char jmt-p); To `match-beg` effectively.
          (while
              (progn; Now point should (invariant) be after the dot delimiter of any preceding segment,
                (skip-syntax-backward "-"); with at most whitespace intervening. [SL]
                (when (eq ?. (char-before)); [NCE]
                  (backward-char); To before the dot.
                  (skip-syntax-backward "-"); [SL]
                  (setq seg-end (point))
                  (skip-chars-backward jmt-name-character-set)
                  (setq seg-beg (point))
                  (when (< seg-beg seg-end)
                    (set 'jmt-f (cons seg-end jmt-f)); Pushing the bounds to the stack.
                    (set 'jmt-f (cons seg-beg jmt-f))))))
          jmt-p); Returning > point in order to delimit the search region, in effect to `match-beg`.

       ;; 4. Reposition in readiness for the next anchor search
       ;; ─────────────
       '(goto-char jmt-q); (post-form) To `match-end` effectively.
       '(0 'jmt-annotation-package-name t)))); [QTF]


   ;; Package name occuring elsewhere
   ;; ────────────
   (cons; Reface each name segment using face `jmt-package-name`, and each apparently misfaced
      ;;; type name using face `jmt-type-reference`.
    (let (match-beg match-end)
      (lambda (limit)
        (setq match-beg (point)); Presumptively.
        (catch 'to-reface
          (while (< match-beg limit)
            (setq match-end (next-single-property-change match-beg 'face (current-buffer) limit))
            (when (eq 'font-lock-constant-face (get-text-property match-beg 'face))
              (goto-char match-end)
              (forward-comment most-positive-fixnum); [CW→, PPN]
              (when (eobp) (throw 'to-reface nil))
              (when (char-equal ?. (char-after))
                (set 'jmt-f
                     (if (string= "Lu" (get-char-code-property (char-after match-beg) 'general-category))
                         'jmt-type-reference; Workaround for what is probably a misfacing by Java mode.
                           ;;; It occurs e.g. with a class-qualified reference to a class member
                           ;;; whose name begins in upper case, such as `Foo.BAR` or `Foo.FooBar`;
                           ;;; here Java mode misfaces the class name (`Foo`) as a package name segment.
                           ;;; This workaround assumes that a real segment would begin in lower case,
                           ;;; which itself is not quite correct. [BUG]
                       'jmt-package-name))
                (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
                (throw 'to-reface t)))
            (setq match-beg match-end))
          nil)))
    '(0 jmt-f t))



   ;; ═════════
   ;; Delimiter
   ;; ═════════

   (cons; Face each Java delimiter that was left unfaced by Java mode.
    (let (i j match-beg match-end)
      (lambda (limit)
        (setq match-beg (point)); Presumptively.
        (set
         'jmt-f
         (catch 'to-face
           (while (< match-beg limit)
             (setq i (syntax-after match-beg)
                   j (car i); Numeric syntax code.
                   match-end (1+ match-beg))
             (when (or (= j 4) (= j 5)); When it has bracket syntax, that is.
               (setq j (cdr i)); The character of the opposing bracket.

               ;; Angle bracket
               ;; ─────────────
               (when (or (char-equal j ?<)
                         (char-equal j ?>))
                 (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
                 (throw 'to-face 'jmt-angle-bracket))

               ;; Curly bracket
               ;; ─────────────
               (when (or (char-equal j ?{)
                         (char-equal j ?}))
                 (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
                 (throw 'to-face 'jmt-curly-bracket))

               ;; Round bracket
               ;; ─────────────
               (when (or (char-equal j ?\()
                         (char-equal j ?\)))
                 (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
                 (throw 'to-face 'jmt-round-bracket))

               ;; Square bracket
               ;; ──────────────
               (when (or (char-equal j ?\[)
                         (char-equal j ?\]))
                 (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
                 (throw 'to-face 'jmt-square-bracket)))

             ;; Separator
             ;; ─────────
             (setq j (char-after match-beg))
             (when (or (char-equal j ?,)
                       (char-equal j ?\;)
                       (char-equal j ?:)
                       (char-equal j ?.))
               (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
               (throw 'to-face 'jmt-separator))

             (setq match-beg match-end))
           nil))))
    '(0 jmt-f))



   ;; ═══════════════
   ;; Javadoc comment
   ;; ═══════════════

   ;; Outer delimiter
   ;; ───────────────
   (cons; Reface each Javadoc outermost delimiter `/**` using face `jmt-Javadoc-outer-delimiter`.
    (let (p)
      (lambda (limit)
        (catch 'to-reface
          (while (search-forward "/**" limit t)
            (setq p (match-beginning 0))
            (when (and (eq 'font-lock-doc-face (get-text-property p 'face)); When inside is a Javadoc,
                       (or (= p (point-min))                               ; but outside is not.
                           (not (eq 'font-lock-doc-face (get-text-property (1- p) 'face)))))
              (throw 'to-reface t)))
          nil)))
    '(0 'jmt-Javadoc-outer-delimiter prepend)); [PDF, QTF]


   (cons; Reface each Javadoc left-marginal delimiter `*` using face `jmt-Javadoc-outer-delimiter`.
    (lambda (limit)
      (catch 'to-reface
        (while (re-search-forward "^\\s-*\\(\\*\\)" limit t)
          (when (eq 'font-lock-doc-face (get-text-property (match-beginning 1) 'face))
            (throw 'to-reface t)))
        nil))
    '(1 'jmt-Javadoc-outer-delimiter prepend)); [PDF, QTF]


   (cons; Reface each Javadoc outermost delimiter `*/` using face `jmt-Javadoc-outer-delimiter`.
    (let (p)
      (lambda (limit)
        (catch 'to-reface
          (while (search-forward "*/" limit t)
            (setq p (match-end 0))
            (when (and (eq 'font-lock-doc-face (get-text-property (1- p) 'face)); When inside is,
                       (or (= p (point-max))                      ; but outside is not, a Javadoc.
                           (not (eq 'font-lock-doc-face (get-text-property p 'face)))))
              (throw 'to-reface t)))
          nil)))
    '(0 'jmt-Javadoc-outer-delimiter prepend)); [PDF, QTF]


   ;; Inline tag
   ;; ──────────
   (list; Reface each Javadoc inline tag. [JIL]
    (let (match-beg tag-name)
      (lambda (limit)
        (catch 'to-reface
          (while (re-search-forward
                  (concat "\\({\\)\\s-*\\(@\\)\\s-*\\([[:alnum:]]+\\)";    This search might be made
                      ;;;     ·           ·          └──────────────┘        more reliable. [JIL]
                      ;;;     {           @              tag name

                          "\\(?:\\s-+\\([^[:space:]}\n]+\\)\\)?\\(?:\\s-+\\([^}\n]*\\)\\)?\\(}\\)"); [SL]
                      ;;;              └──────────────────┘                └─────────┘       ·
                      ;;;                 parameters 1,                        2, …          }
                  limit t)
            (setq match-beg (match-beginning 0))
            (when (and (jmt-is-Java-mode-tag-faced (get-text-property match-beg 'face))
                       (>= (match-end 0); And that facing is uniform.
                           (next-single-property-change match-beg 'face (current-buffer) limit)))
              (setq tag-name (match-string-no-properties 3))
              (cond ((or (string= tag-name "link")
                         (string= tag-name "linkplain"))
                     (setq jmt-f jmt-inline-tag-name-f
                           jmt-p jmt-inline-tag-parameter-f
                           jmt-q jmt-inline-rendered-parameter-f))

                    ((string= tag-name "value")
                     (setq jmt-f jmt-value-tag-name-f
                           jmt-p jmt-inline-tag-parameter-f; (if any)
                           jmt-q jmt-inline-tag-parameter-f))

                    ((or (string= tag-name "code")
                         (string= tag-name "index")
                         (string= tag-name "literal")
                         (string= tag-name "summary"))
                     (setq jmt-f jmt-inline-tag-name-f
                           jmt-p jmt-inline-rendered-parameter-f
                           jmt-q jmt-inline-tag-parameter-f)); (if any)

                    (t; All the rest.
                     (setq jmt-f jmt-inline-tag-name-f
                           jmt-p jmt-inline-tag-parameter-f; (if any)
                           jmt-q jmt-inline-tag-parameter-f)))
              (throw 'to-reface t)))
          nil)))
    '(1 jmt-Javadoc-tag-delimiter-f t) '(2 jmt-Javadoc-tag-mark-f t)
    '(3 jmt-f t) '(4 jmt-p t t) '(5 jmt-q t t) '(6 jmt-Javadoc-tag-delimiter-f t))


   ;; HTML tag
   ;; ────────
   (list; Reface each HTML tag.
    (let (match-beg match-end)
      (lambda (limit)
        (setq match-beg (point)); Presumptively.
        (catch 'to-reface
          (while (< match-beg limit)
            (setq match-end (next-single-property-change match-beg 'face (current-buffer) limit))
            (when (and (char-equal ?< (char-after match-beg))
                       (jmt-is-Java-mode-tag-faced (get-text-property match-beg 'face)))
              (goto-char match-beg)
              (save-restriction
                (narrow-to-region match-beg match-end)

                ;; End tag
                ;; ───────
                (when (looking-at "\\\(./\\)\\([[:alnum:]]+\\)\\(\\s-*\\)\\(>\\)$"); ‘$’ [NBE]
                              ;;;     └────┘  └──────────────┘              ·
                              ;;;       </        tag name                  >

                  (setq jmt-f jmt-HTML-end-tag-name-f
                        jmt-p jmt-inline-tag-parameter-f); (actually this is just space, if present)
                  (goto-char match-end)
                  (throw 'to-reface t))

                ;; Start tag
                ;; ─────────
                (when (looking-at "\\(.\\)\\([[:alnum:]]+\\)\\(\\s-.*\\)?\\(/?>\\)$"); ‘$’ [NBE]
                              ;;;     ·     └──────────────┘  └────────┘    · ·
                              ;;;     <         tag name      attributes    / >

                  (setq jmt-f jmt-HTML-start-tag-name-f
                        jmt-p jmt-inline-tag-parameter-f); (or maybe just space, if present at all)
                  (goto-char match-end)
                  (throw 'to-reface t))))
            (setq match-beg match-end))
          nil)))
    '(1 jmt-Javadoc-tag-delimiter-f t) '(2 jmt-f t) '(3 jmt-p t t) '(4 jmt-Javadoc-tag-delimiter-f t))


   ;; Block tag
   ;; ─────────
   (let; Reface each Javadoc block tag. [JBL]
       (match-beg match-end tag-name)
     (list

      (lambda (limit); (1) Anchoring on the ‘@’ mark and name that begins each tag.
        (setq match-beg (point)); Presumptively.
        (catch 'to-reface
          (while (< match-beg limit)
            (setq match-end (next-single-property-change match-beg 'face (current-buffer) limit))
            (when (and (char-equal ?@ (char-after match-beg))
                       (jmt-is-Java-mode-tag-faced (get-text-property match-beg 'face)))
              (goto-char match-beg)
              (save-restriction
                (narrow-to-region match-beg match-end)
                (when (looking-at "\\(.\\)\\([[:alnum:]]+\\)$"); ‘$’ [NBE]
                              ;;;     ·     └──────────────┘
                              ;;;     @         tag name

                  (setq tag-name (match-string-no-properties 2))
                  (goto-char match-end)
                  (throw 'to-reface t))))
            (setq match-beg match-end))
          nil))
      '(1 jmt-Javadoc-tag-mark-f t) '(2 jmt-block-tag-name-f t)

      (list; (2, anchored highlighter) Thence rightward, refacing any parameter that needs it.
       (lambda (_limit)
         (catch 'to-reface
           (cond
            ((string= tag-name "param")
             (when (looking-at (concat "\\s-+<\\s-*\\([" jmt-name-character-set "]+\\).*$"))
               (set 'jmt-f 'jmt-type-variable-tag-parameter)
               (goto-char (match-end 0))
               (throw 'to-reface t))
             (when (looking-at (concat "\\s-+\\([" jmt-name-character-set "]+\\).*$"))
               (set 'jmt-f 'jmt-param-tag-parameter)
               (goto-char (match-end 0))
               (throw 'to-reface t)))

            ((string= tag-name "see")
             (when (looking-at "\\s-+\\([^<\n].*\\)$"); Excepting an HTML tag.
               (set 'jmt-f 'jmt-block-tag-parameter)
               (goto-char (match-end 0))
               (throw 'to-reface t)))

            ((or (string= tag-name "throws")
                 (string= tag-name "exception"))
             (when (looking-at (concat "\\s-+\\([" jmt-name-character-set "]+\\).*$"))
               (set 'jmt-f 'jmt-throws-tag-parameter)
               (goto-char (match-end 0))
               (throw 'to-reface t)))

            ((or (string= tag-name "uses")
                 (string= tag-name "provides"))
             (when (looking-at "\\s-+\\([^[:space:]\n]+\\).*$")
               (set 'jmt-f 'jmt-block-tag-parameter)
               (goto-char (match-end 0))
               (throw 'to-reface t)))

            ((or (string= tag-name "since")
                 (string= tag-name "author")
                 (string= tag-name "version"))
             (when (looking-at "\\s-+\\([^[:space:]\n].+\\)$")
               (set 'jmt-f 'jmt-block-tag-parameter)
               (goto-char (match-end 0))
               (throw 'to-reface t)))

            ((string= tag-name "serial")
             (when (looking-at "\\s-+\\(\\(?:ex\\|in\\)clude\\)\\s-*$")
               (set 'jmt-f 'jmt-block-tag-parameter)
               (goto-char (match-end 0))
               (throw 'to-reface t)))

            ((string= tag-name "serialField")
             (when (looking-at (concat "\\s-+\\([" jmt-name-character-set
                                       "]+\\s-+[" jmt-name-character-set "]+\\).*$"))
               (set 'jmt-f 'jmt-block-tag-parameter)
               (goto-char (match-end 0))
               (throw 'to-reface t))))
           nil))
       nil nil '(1 jmt-f prepend)))); [PDF]



   ;; ════════════════════════════════
   ;; Method or constructor identifier  [↑A, ↑T]
   ;; ════════════════════════════════

   (cons; Fontify each identifier that was misfaced by Java mode, or incorrectly left unfaced.
    (let ((identifier-pattern (concat "[" jmt-name-character-set "]+"))
          face i match-beg match-end)
      (lambda (limit)
        (set
         'jmt-f
         (catch 'to-fontify
           (while (re-search-forward identifier-pattern limit t)
             (setq match-beg (match-beginning 0); Presumptively.
                   match-end (match-end 0)
                   face (get-text-property match-beg 'face))
             (when (or (eq face nil) (eq face 'font-lock-function-name-face); Unfaced or misfaced.
                       (eq face 'jmt-type-reference)); [↑T]
                 ;;; Vanguard, redundant but for sake of speed.  See the other face guards below.
               (forward-comment most-positive-fixnum); [CW→]
               (when (eq ?\( (char-after)); [NCE]

                 ;; Constructor definition  (assumption: point is directly before the ‘(’)
                 ;; ──────────────────────
                 (catch 'is-constructor-definition; One that needs fontifying, that is.  Or some
                   ;; cases of method definition in need; this section will fontify those, too,
                   ;; just because it happens to precede the method definition section, below.
                   (unless (or (eq face nil) (eq face 'jmt-type-reference)); [↑T]
                     (throw 'is-constructor-definition nil)); Only identifiers left unfaced
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
                       (goto-char match-end)
                       (throw 'to-fontify 'font-lock-function-name-face))
                     (catch 'is-past-package; Leaving `i` at either the next token to deal with,
                       (while t; or the buffer end, scan past any itervening package name.
                         ;; Now point lies (invariant) directly after a name (in form).
                         (forward-comment most-positive-fixnum); [CW→, PPN]
                         (when (eobp) (throw 'is-past-package t))
                         (unless (char-equal ?. (char-after)); Namely the delimiting dot of a
                           (throw 'is-past-package t))       ; preceding package name segment.
                         (forward-char); Past the ‘.’.
                         (forward-comment most-positive-fixnum); [CW→] To the next token.
                         (setq i (point)); What follows the package name follows its last dot.
                         (when (= (skip-chars-forward jmt-name-character-set) 0)
                           (throw 'is-past-package t)))))
                   (when (and (/= i (point-max))
                              (or (char-equal ?@ (char-after i))
                                  (eq (get-text-property i 'face) 'jmt-type-reference))); [↑T]
                     (goto-char match-end)
                     (throw 'to-fontify 'font-lock-function-name-face))

                   ;; Before the identifier
                   ;; ·····················
                   (goto-char match-beg)
                   (forward-comment most-negative-fixnum); [←CW]
                   (when (bobp) (throw 'is-constructor-definition nil))
                   (when (char-equal (char-before) ?>)
                     (if (jmt-preceding->-marks-generic-return-type)
                         (goto-char match-end)
                         (throw 'to-fontify 'font-lock-function-name-face)
                       (throw 'is-constructor-definition nil)))
                   ;; A constructor modifier here before point would also indicate a definition.
                   ;; However, the earlier test of ‘final’ (above) has eliminated the only case
                   ;; in which Java mode is known to fail when a keyword modifier appears here.
                   ;; That leaves only the case of an *annotation* modifier to remedy.
                   (when (jmt-is-annotation-terminal-face (get-text-property (1- (point)) 'face)); [↑A]
                     (goto-char match-end)
                     (throw 'to-fontify 'font-lock-function-name-face)))

                 ;; Method definition
                 ;; ─────────────────
                 (catch 'is-method-definition; One that needs fontifying, that is.
                   (unless (eq face nil)) (throw 'is-method-definition nil); Definitions
                     ;;; unfaced have been seen, but misfaced have not.  See for instance
                     ;;; the sequence `public @Override @Warning("non-API") void onCreate()`.
                     ;;; [https://github.com/Michael-Allan/waymaker/blob/3eaa6fc9f8c4137bdb463616dd3e45f340e1d34e/waymaker/gen/ApplicationX.java#L40]
                   (goto-char match-beg)
                   (forward-comment most-negative-fixnum); [←CW]
                   (when (bobp) (throw 'is-method-definition nil))
                   (setq i (char-before))
                   (when (char-equal i ?\]); Return type declared as an array.
                     (goto-char match-end)
                     (throw 'to-fontify 'font-lock-function-name-face))
                   (when (char-equal i ?>)
                     (if (jmt-preceding->-marks-generic-return-type)
                         (goto-char match-end)
                         (throw 'to-fontify 'font-lock-function-name-face)
                       (throw 'is-method-definition nil)))
                   (when (eq (get-text-property (1- (point)) 'face) 'jmt-type-reference); [↑T]
                     ;; The return type is declared simply by a type name.
                     (goto-char match-end)
                     (throw 'to-fontify 'font-lock-function-name-face)))

                 ;; Method call
                 ;; ───────────
                 (catch 'is-method-call; One that needs refacing, that is.
                   (unless (eq face 'font-lock-function-name-face) (throw 'is-method-call nil))
                     ;;; Only calls misfaced as definitions have been seen.
                   (goto-char match-beg)
                   (forward-comment most-negative-fixnum); [←CW]
                   (when (bobp) (throw 'is-method-call nil))
                   (when (char-equal (char-before) ?.); Always the misfaced identifier directly
                     ;; follows a ‘.’, which excludes the possibility of it being a definition.
                     ;; See for instance the sequence `assert stators.getClass()`. [AM]
                     ;; [https://github.com/Michael-Allan/waymaker/blob/3eaa6fc9f8c4137bdb463616dd3e45f340e1d34e/waymaker/gen/KittedPolyStatorSR.java#L58]
                     (goto-char match-end)
                     (throw 'to-fontify 'default))))
               (goto-char match-end))); Whence the next leg of the search begins.
           nil))))
    '(0 jmt-f t))



   ;; ═════════════
   ;; Named literal
   ;; ═════════════

   (cons; Reface each literal of type boolean or null.
    (let (m match-beg match-end)
      (lambda (limit)
        (setq match-beg (point)); Presumptively.
        (catch 'to-reface
          (while (< match-beg limit)
            (setq match-end (next-single-property-change match-beg 'face (current-buffer) limit))
            (when (eq 'font-lock-constant-face (get-text-property match-beg 'face))
              (setq m (buffer-substring-no-properties match-beg match-end))
              (when (or (string= "true" m) (string= "false" m) (string= "null" m))
                (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
                (throw 'to-reface t)))
            (setq match-beg match-end))
          nil)))
    '(0 'jmt-named-literal t)); [QTF]



   ;; ════════════
   ;; Shebang line, as per `http://openjdk.java.net/jeps/330#Shebang_files`
   ;; ════════════

   (list; Fontify any shebang line in a source-launch file.
    "\\`\\(#!\\)\\(.+?\\)\\(?:\\(\\\\c\\)\\(.*\\)\\)?$"
      ;;;  ·      └─────┘  └───────────────────────┘
      ;;;  #!      body          comment [CSL]

    '(1 'jmt-shebang t) '(2 'jmt-shebang-body t) '(3 'jmt-shebang-comment-delimiter t t); [QTF]
    '(4 'jmt-shebang-comment t t))



   ;; ══════════════
   ;; String literal  [↑A]
   ;; ══════════════

   (list; Refontify each Java string literal using face `jmt-string-delimiter`,
      ;;; `or faces jmt-annotation-string` and `jmt-annotation-string-delimiter`.
    (let (body-beg body-end f match-beg match-end)
      (lambda (limit)
        (setq match-beg (point)); Presumptively.
        (catch 'to-refontify
          (while (< match-beg limit)
            (setq match-end (next-single-property-change match-beg 'face (current-buffer) limit))
            (when (eq 'font-lock-string-face (get-text-property match-beg 'face))
              (setq body-beg (1+ match-beg)
                    body-end (1- match-end)
                    f (get-text-property match-end 'face))
              (if (or (eq f 'jmt-annotation-delimiter); A rough test only, it could fail. [↑A, BUG]
                      (eq f 'jmt-annotation-qualifier))
                  (progn

                  ;; Within annotation
                  ;; ─────────────────
                    (set 'jmt-f 'jmt-annotation-string-delimiter)
                    (set-match-data (list match-beg match-end match-beg body-beg body-beg body-end
                                          body-end match-end (current-buffer))))
                ;; Elsewhere
                ;; ─────────
                (set 'jmt-f 'jmt-string-delimiter)
                (set-match-data (list match-beg match-end match-beg body-beg nil nil
                                      body-end match-end (current-buffer))))
              (goto-char match-end)
              (throw 'to-refontify t))
            (setq match-beg match-end))
          nil)))
    '(1 jmt-f t) '(2 'jmt-annotation-string t t) '(3 jmt-f t)); [QTF]



   ;; ═════════════
   ;; Type variable in a type parameter declaration  [↑A, ↑T]
   ;; ═════════════

   (cons; Reface each variable using face `jmt-type-variable-declaration`.  See optimization note. [TV]
    (let (depth i j match-beg match-end p p-min)
      (lambda (limit)
        (catch 'to-reface
          (while (< (point) limit)
            (setq match-beg (point); Presumptively.
                  match-end (next-single-property-change match-beg 'face (current-buffer) limit))
            (when (eq 'jmt-type-reference (get-text-property match-beg 'face)); [↑T]
              (setq p-min (point-min))
              (while; Skipping back over both any commentary and/or the annotations that
                  (progn; may appear here, and setting `p` to the resulting point. [TP]
                    (forward-comment most-negative-fixnum); [←CW]
                    (setq p (point))
                    (if (or (= p p-min)
                            (not (jmt-is-annotation-ish-before p))); [↑A]
                        nil; Quitting the `while` loop.
                      (goto-char (previous-single-property-change p 'face))
                      t))); Continuing the loop.
              (setq p (1- p)); Before what should be the delimiter of a type variable list. [TVL]
              (when
                  (and
                   (>= p p-min)
                   (progn
                     (setq i (char-after p))
                     (or; And the character after `p` is indeed such a list delimiter, viz. one of:
                      (eq 'c-<-as-paren-syntax (setq j (get-text-property p 'category))); ‘<’
                      (eq 'c->-as-paren-syntax j)                                       ; ‘>’
                      (and (eq (get-text-property p 'c-type) 'c-<>-arg-sep)             ; ‘,’
                           (eq i ?,)))); Not ‘&’, that is.
                     ;;; Leaving `i` set to that delimiter character.
                   (catch 'is-proven; And the matched name occurs at the top level of that list (depth
                     ;; of angle brackets 1).  And the list directly follows either (a) the identifier
                     ;; of a type definition, indicating the definition of a generic class or inter-
                     ;; face, or (b) neither a type name, type variable nor `.` delimiter (of a method
                     ;; call), indicating the definition of a generic method or constructor. [MC]
                     (setq depth 1); Nested depth of name in brackets, presumed to be 1 as required.
                     (while; Ensure `p` has emerged from all brackets,
                         (progn; moving it leftward as necessary.
                           (cond ((char-equal i ?<); Ascending from the present bracket pair.
                                  (setq depth (1- depth))
                                  (when (= 0 depth); Then presumeably `p` has emerged left of list.
                                    (goto-char p)
                                    (forward-comment most-negative-fixnum); [←CW]
                                    (when (bobp) (throw 'is-proven t))
                                      ;;; Apparently a generic method or constructor definition,
                                      ;;; though outside of any class body and so misplaced.
                                    (setq p (1- (point)); Into direct predecessor of variable list.
                                          i (char-after p))
                                    (when (or (char-equal i ?.); `.` delimiter of a method call.
                                              (char-equal i ?<)); `p` had *not* emerged, and so the
                                                ;;; type variable is *not* at top level, after all.
                                      (throw 'is-proven nil))
                                    (setq j (get-text-property p 'face))
                                    (throw 'is-proven; As the type parameter declaration of:
                                           (or (eq 'jmt-type-definition j); [↑T]
                                                 ;;; (a) a generic class or interface definition;
                                               (not (eq 'font-lock-type-face
                                                     (jmt-untamed-face j)))))))
                                                 ;;; (b) a generic method or constructor definition.
                                 ((char-equal i ?>); Descending into another bracket pair.
                                  (setq depth (1+ depth))))
                           (if (= p p-min)
                               nil; Quitting the `while` loop.
                             (setq p (1- p))
                             (setq i (char-after p))
                             t))); Continuing the loop.
                     nil))
                (set-match-data (list match-beg (goto-char match-end) (current-buffer)))
                (throw 'to-reface t)))
            (goto-char match-end))
          nil)))
    '(0 'jmt-type-variable-declaration t))); [QFS]
  "\
Elements of ‘jmt-new-fontifiers-3’ which are specific to Java Mode Tamed.")



(defface jmt-square-bracket
  `((t . (:inherit jmt-bracket))) "\
The face for a square bracket, ‘[’ or ‘]’."
  :group 'delimiter-faces)



(defface jmt-string-delimiter; [BC, LF, RF]
  `((t . (:inherit font-lock-string-face))) "\
The face for a string (\") or character (\\=') delimiter.
Customize it to better distinguish the delimiters from the content
they delimit; making them more prominent or less prominent, for example.
See also ‘jmt-delimiter’ and the faces that inherit from it."
  :group 'delimiter-faces)



(defface jmt-throws-tag-parameter
  `((t . (:inherit jmt-block-tag-parameter))) "\
The face for the type-reference parameter of a Javadoc `throws` tag."
  :group 'java-mode-tamed)



(defface jmt-type-definition; [MDF, RF]
  `((t . (:inherit font-lock-type-face))) "\
The face for the identifier of a class or interface in a type definition.
Customize it to highlight the identifier where initially it is defined (like
‘font-lock-variable-name-face’ does for variable identifiers), as opposed
to merely referenced after the fact.  See also face ‘jmt-type-reference’."
  :group 'java-mode-tamed)



(defface jmt-type-reference; [MDF, RF]
  `((t . (:inherit font-lock-type-face))) "\
The face for the identifier of a class, interface or type parameter
(viz. type variable) where it appears as a type reference.  See also
faces ‘jmt-type-definition’ and ‘jmt-type-variable-declaration’."
  :group 'java-mode-tamed)



(defface jmt-type-variable-declaration; [TP, MDF, RF]
  `((t . (:inherit jmt-type-definition))) "\
The face for a type variable in a type parameter declaration.
Customize it to highlight the variable where initially it is declared
(as ‘font-lock-variable-name-face’ does for non-type variables), rather than
merely referenced after the fact.  See also face ‘jmt-type-reference’."
  :group 'java-mode-tamed)



(defface jmt-type-variable-tag-parameter; [NDF, RF]
  `((t . (:inherit jmt-Javadoc-tag))) "\
The face for a type variable in a Javadoc `param` tag."
  ;; Java mode has misfaced it as an HTML tag (the two have the same delimiters).  Therefore this face
  ;; (like `jmt-HTML-tag-name`, and unlike `jmt-param-tag-parameter`) is necessarily a replacement
  ;; face for `font-lock-constant-face` (via `jmt-Javadoc-tag` as it happens).  A better solution
  ;; would be to repair Java mode’s error in order to elimate this complication. [BUG]
  :group 'javadoc-faces)



(defun jmt-untamed-face (face)
  "Returns FACE itself if untamed, else the untamed ancestral face
from which ultimately it inherits.  Necessarily every face defined
by Java Mode Tamed (tamed face) ultimately inherits from a face
defined elsewhere, namely its untamed ancestral face."
  (catch 'untamed-face
    (while (string-prefix-p "jmt-" (symbol-name face))
      (setq face (face-attribute face :inherit nil nil))
      (when (eq 'unspecified face)
        (throw 'untamed-face 'default)))
    face))



(defface jmt-value-tag-name; [NDF, RF]
  `((t . (:inherit jmt-inline-tag-name))) "\
The face for the proper identifier `value` of a Javadoc value tag."
  :group 'javadoc-faces)

(defconst jmt-value-tag-name-f (jmt-make-Javadoc-tag-facing 'jmt-value-tag-name))



(defgroup keyword-faces nil "\
Faces for Java keywords."
  :group 'java-mode-tamed
  :prefix "jmt-")



(defgroup shebang-faces nil "\
Faces for a shebang line atop a source-launch file."
  :group 'java-mode-tamed
  :prefix "jmt-"
  :link '(url-link "http://openjdk.java.net/jeps/330#Shebang_files"))



;; ══════════════════════════════════════════════════════════════════════════════════════════════════════


(unless jmt--early-initialization-was-begun
  (set 'jmt--early-initialization-was-begun t)
  (require 'cc-mode)
  (set 'c-default-style (cons '(java-mode-tamed . "java") c-default-style)))
    ;;; Though it appears to have no effect.



(define-derived-mode java-mode-tamed java-mode
  "Java" "\
This is Java Mode Tamed, a derived major mode designed to give the user
more control over the Java mode that comes bundled with Emacs, particularly
in regard to syntax highlighting.

        Home page URL ‘http://reluk.ca/project/Java/Emacs/’
User instructions URL ‘http://reluk.ca/project/Java/Emacs/user_instructions.el’"
  :group 'java-mode-tamed

  ;; ════════════════════════════
  ;; Finish initializing the mode
  ;; ════════════════════════════

  (unless jmt--late-initialization-was-begun
    (set 'jmt--late-initialization-was-begun t)
    (cl-assert (not (char-equal ?\s (char-syntax ?\n)))); Newlines have no whitespace syntax.
    (cl-assert parse-sexp-ignore-comments)
    (set 'c-literal-faces
         (append c-literal-faces; [LF]
                 '(jmt-annotation-string
                   jmt-annotation-string-delimiter
                   jmt-string-delimiter)))

    ;; Apply monkey patches
    ;; ────────────────────
    ;; Patching the underlying (Java-mode) functions.  Done only now, after loading the first Java file,
    ;; so not to needlessly delay the start of Emacs.
    (define-error 'jmt-x "Broken monkey patch")
    (condition-case x
        (progn
          (let (source-file source-base-name)

            ;; `cc-fonts` functions
            ;; ────────────────────
            (setq source-file (locate-library "cc-fonts.el" t)
                  source-base-name "cc-fonts")
            (unless source-file
              (signal 'jmt-x `("No such source file on load path: `cc-fonts.el`")))
            (with-temp-buffer; [ELM]
              (setq-local parse-sexp-ignore-comments t)
              (with-syntax-table emacs-lisp-mode-syntax-table
                (insert-file-contents source-file)

                (jmt--patch
                 source-file source-base-name 'c-fontify-recorded-types-and-refs
                 (lambda ()
                   (let (is-patched)
                     (while (search-forward "(c-put-font-lock-face " nil t)
                       (replace-match "(jmt--c/try-putting-face " t t)
                       (setq is-patched t))
                     is-patched)))

                (jmt--patch
                 source-file source-base-name 'c-font-lock-<>-arglists
                 (lambda ()
                   (let (is-patched)
                     (while (search-forward "(eq id-face" nil t)
                       (replace-match "(jmt-faces-are-equivalent id-face" t t)
                       (setq is-patched t))
                     is-patched)))

                (jmt--patch
                 source-file source-base-name 'c-font-lock-declarations
                 (lambda ()
                   (when (re-search-forward
                          (concat "(\\(eq\\) (get-text-property (point) 'face)[[:space:]\n]*"
                                  "'font-lock-keyword-face)")
                          nil t)
                     (replace-match "jmt-faces-are-equivalent" t t nil 1)
                     t)))))

            ;;; (jmt--patch
            ;;;  source-file source-base-name 'c-font-lock-labels
            ;;;  (lambda ()
            ;;;    (when (re-search-forward
            ;;;           (concat "(\\(eq\\) (get-text-property (1- (point)) 'face)[[:space:]\n]*"
            ;;;                   "c-label-face-name)"); [FLC]
            ;;;           nil t)
            ;;;      (replace-match "jmt-faces-are-equivalent" t t nil 1)
            ;;;      t)))))
            ;;;;;; ‘This function is only used on decoration level 2’, ∴ no patch is needed. [L2U]

            ;; `cc-mode` functions
            ;; ───────────────────
            (setq source-file (locate-library "cc-mode.el" t)
                  source-base-name "cc-mode")
            (unless source-file
              (signal 'jmt-x `("No such source file on load path: `cc-mode.el`")))
            (with-temp-buffer; [ELM]
              (setq-local parse-sexp-ignore-comments t)
              (with-syntax-table emacs-lisp-mode-syntax-table
                (insert-file-contents source-file)

                (jmt--patch
                 source-file source-base-name 'c-before-change
                 (lambda (); Java mode uses the following list of faces for a `memq` test.
                   (when (search-forward "'(font-lock-comment-face font-lock-string-face)" nil t)
                     (backward-char); Before the trailing ‘)’, insert their replacement faces: [BC]
                     (insert
                      " jmt-annotation-string jmt-annotation-string-delimiter jmt-string-delimiter")
                     t)))

                (jmt--patch
                 source-file source-base-name 'c-font-lock-fontify-region
                 (lambda ()
                   (when (re-search-forward
                          (concat "(funcall (default-value 'font-lock-fontify-region-function)[[:space:]\n]*"
                                  "new-beg new-end verbose)")
                          nil t)
                     (replace-match (concat; Wrapping the `funcall` to update relevant state variables.
                                     "(set 'jmt--present-fontification-beg new-beg)"
                                     "(set 'jmt--present-fontification-end new-end)"
                                     "\\&"; `(funcall … new-end verbose)`
                                     "(set 'jmt--present-fontification-end 0)")
                                    t)
                     t))))))

          ;; Javadoc block tag fontifier
          ;; ───────────────────────────
          (let* ((fontifier (nth 1 javadoc-font-lock-doc-comments))
                 (pattern (car fontifier))
                 (pattern-end "\\(@[a-z]+\\)"))
            (if (not (string-suffix-p pattern-end pattern))
                (jmt-message
                 "(java-mode-tamed): Patch failed to apply, `javadoc-font-lock-doc-comments`")
              (setq pattern (substring pattern 0 (- (length pattern-end)))
                    pattern (concat pattern "\\(@[a-zA-Z]+\\)")); Allowing for e.g. `serialData`. [JBL]
              (setcar fontifier pattern))))

      (jmt-x (display-warning 'java-mode-tamed (error-message-string x) :error))))


  ;; ═════════════════════
  ;; Initialize the buffer
  ;; ═════════════════════

  ;; Cache the decoration level in `jmt--is-level-3`, q.v.
  ;; ──────────────────────────
  (let ((level (font-lock-value-in-major-mode font-lock-maximum-decoration)))
    (set 'jmt--is-level-3 (or (eq level t) (and (numberp level) (>= level 3)))))

  ;; Tell Java mode of crucial changes
  ;; ─────────────────────────────────
  (jmt-set-for-buffer
   'c-maybe-decl-faces
   (append c-maybe-decl-faces; [MDF]
           ;;   Quoted individually only because `c-maybe-decl-faces` “must be evaluated (with ‘eval’)
           ;; ↙  at runtime to get the actual list of faces”; e.g. `(eval c-maybe-decl-faces)`. [QTF]
           '('jmt-annotation-package-name
             'jmt-boilerplate-keyword
             'jmt-expression-keyword
             'jmt-named-literal
             'jmt-package-name
             'jmt-package-name-declared
             'jmt-principal-keyword
             'jmt-qualifier-keyword
             'jmt-type-definition
             'jmt-type-variable-declaration
             'jmt-type-reference)))

  ;; Tell Font Lock how to fontify this buffer
  ;; ─────────────────────────────────────────
  (jmt-set-for-buffer 'font-lock-defaults
       '((java-font-lock-keywords-1; 0 or nil    The alternative values of `font-lock-keywords`,
          java-font-lock-keywords-1; 1           each ordered according to the value of `font-lock-
          jmt-new-fontifiers-2     ; 2           -maximum-decoration` that selects it. [MD]
          jmt-new-fontifiers-3)))) ; 3 or t



(provide 'java-mode-tamed)



;; NOTES
;; ─────
;;   A ·· Section *Annotation* of `jmt-specific-fontifiers-3`.
;;
;;  ↑A ·· Code that must execute after section *Annotation*.
;;
;;   AM · This marks two misfacings that likely are related.  See the `assert` keyword section
;;        at `http://reluk.ca/project/Java/Emacs/action_plan.brec`.
;;
;;   AST  At-sign as a token.  ‘It is possible to put whitespace between it and the TypeName,
;;        but this is discouraged as a matter of style.’
;;        https://docs.oracle.com/javase/specs/jls/se13/html/jls-9.html#jls-9.7
;;
;;   BC · `c-before-change`: Any replacement face [RF] for a face referenced by this function
;;        must be included in its monkey patch.
;;
;;   BUG  This code is incorrect.
;;
;;   CI · Conservative inheritance.  This face inherits from `font-lock-doc-face` only to preserve
;;        the original appearance of the fontified text in default of user action to change it,
;;        and not because it is a replacement face; in fact, it is a prepended face. [PDF, RF]
;;
;;   CSL  A comment in a shebang line is supported by the `env` interpreter.
;;        https://www.gnu.org/software/coreutils/manual/html_node/env-invocation.html
;;
;;  ←CW · Backward across commentary and whitespace.
;;
;;   CW→  Forward across commentary and whitespace.
;;
;;   ELM  The syntax-related code that directly follows the opening of the temporary buffer effects a
;;        fast simulation of Emacs Lisp mode, faster presumeably than would a call to `emacs-lisp-mode`.
;;
;;   FD · Suppressing compiler warnings, ‘the following functions might not be defined at runtime…’.
;;
;;   FLC  `font-lock-constant-face`: the Java-mode code refers to `font-lock-constant-face` indirectly
;;        by way of variables `c-constant-face-name`, `c-doc-markup-face-name`, `c-label-face-name`
;;        and `c-reference-face-name`.
;;
;;   FV · Suppressing sporadic compiler warnings ‘reference to free variable’
;;        or ‘assignment to free variable’.
;;
;;   GVF  A global variable for the use of fontifiers, e.g. from within forms they quote and pass
;;        to Font Lock to be evaluated outside of their lexical scope.
;;
;;   JBL  Javadoc block tags.
;;        https://docs.oracle.com/en/java/javase/13/docs/specs/javadoc/doc-comment-spec.html#block-tags
;;
;;   JIL  Javadoc inline tags.  Double anchoring the tag search on the bounds of a `jmt-is-Java-mode-
;;        -tag-faced` region, like the other tag fontifiers do, might make it more reliable.
;;
;;        https://docs.oracle.com/en/java/javase/13/docs/specs/javadoc/doc-comment-spec.html#inline-tags
;;
;;   K ·· Section *Keyword* of `jmt-specific-fontifiers-3`.
;;
;;  ↑K ·· Code that must execute after section *Keyword*.
;;
;;   L2U  Level-two highlighting is untamed.  ‘L2U’ marks code that enforces the fact and code
;;        that depends on it.
;;
;;   LF · `c-literal-faces`: Any replacement face of a face listed in `c-literal-faces` must itself
;;        be appended to that list.  This applies only to replacement faces, not to prepended faces;
;;        all tests against `c-literal-faces` are done using `c-got-face-at`, which knows how to deal
;;        with prepended faces. [PDF, RF]
;;
;;   MC · Method call.  See `MethodInvocation` at
;;        `https://docs.oracle.com/javase/specs/jls/se13/html/jls-15.html#jls-15.12`.
;;
;;   MD · How the value of `font-lock-maximum-decoration` governs the value of `font-lock-keywords`
;;        is documented inconsistently by Emacs.  See instead the `font-lock-choose-keywords` function
;;        of `http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/font-lock.el`.  It verifies the cor-
;;        rectness of `https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html`.
;;
;;   MDF  `c-maybe-decl-faces`: Any replacement face [RF] for a face listed in `c-maybe-decl-faces`
;;        must itself be appended to that list.
;;
;;   NBE  Not ‘\'’ to match only the buffer end.  Rather ‘$’ to include the line end in the event
;;        the (narrowed) buffer happens to cross lines. [SL]
;;
;;   NCE  Not `char-equal`; it fails if the position is out of bounds.  Rather `eq`, which instead
;;        returns nil.
;;
;;   NDF  Not a declaration face, therefore it need not be appended to `c-maybe-decl-faces`. [MDF]
;;        The face only appears in Javadoc comments.  Meanwhile `c-maybe-decl-faces` is only used,
;;        in conjunction with `c-decl-start-re`, as a bounding argument in a call to `c-find-decl-spots`.
;;        [http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/progmodes/cc-fonts.el?id=fd1b34bfba#n1490]
;;           A trace in the source of both references indicates that a ‘decl-spot’ is not something
;;        that would appear in a Javadoc comment.
;;
;;   P↓ · Code that must execute before section *Package name*  of `jmt-specific-fontifiers-3`.
;;
;;   P ·· Section *Package name* itself.
;;
;;   PDF  Prepending to the documentation face.  In order to duplicate the behaviour of Java mode
;;        (e.g. see `jmt-is-Java-mode-tag-faced`), a special face applied within a Javadoc comment
;;        (e.g. to a Javadoc tag) must not simply override the `font-lock-doc-face` of the surrounding
;;        comment, but rather prepend to it.  E.g. see `prepend` at
;;        `https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html`.
;;
;;   PPN  Parsing a package name segment.  Compare with similar code elsewhere.
;;
;;   QFS  Quote each *facespec* formed as either a face symbol or a list, because Font lock evaluates it.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
;;
;;   QTF  Quoting of tamed face references.  Their quoting is required for passage to evaluators,
;;        e.g. in the case of a Font Lock *facespec*. [QFS]
;;            That might seem obvious, but many packages define a namesake variable for each face symbol.
;;        ‘In the vast majority of cases, this is not necessary’.
;;        [https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html]
;;        ‘Simply using faces directly is enough.’
;;        [http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/font-lock.el?id=fd1b34bfba]
;;
;;   RF · Replacement face: a tamed face used by `java-mode-tamed` to override and replace a face
;;        earlier applied by Java mode.  Every replacement face ultimately inherits from the face
;;        it replaces.  Function `jmt-faces-are-equivalent` depends on this.
;;
;;   SI · Static import declaration.
;;        https://docs.oracle.com/javase/specs/jls/se13/html/jls-7.html#jls-7.5.3
;;
;;   SL · Restricting the fontifier to a single line.  Multi-line fontifiers can be hairy. [BUG]
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Multiline-Font-Lock.html
;;
;;   T↓ · Code that must execute before section *Type name*  of `jmt-specific-fontifiers-3`.
;;
;;   T ·· Section *Type name* itself, or code that must execute in unison with it.
;;
;;  ↑T ·· Code that must execute after section *Type name*.
;;
;;   TP · See `TypeParameter`.  https://docs.oracle.com/javase/specs/jls/se13/html/jls-4.html#jls-4.4
;;
;;   TV · Type variable in a type parameter declaration.  One might think it slow to seek every
;;        type reference, as this fontifier does, and test each against the form of a type variable.
;;        Yet anchoring the search instead on the relatively infrequent characters that delimit type
;;        variable lists, while it would reduce the number of tests, might not yield the time savings
;;        one would expect; the anchoring matcher would have to extend across multiple lines and the
;;        addition to `font-lock-extend-region-functions` that this entails would burden all fontifiers.
;;
;;   TVL  Type variable list, aka `TypeParameters`.
;;        https://docs.oracle.com/javase/specs/jls/se13/html/jls-8.html#jls-8.1.2


;; - - - - - - - - - -
;; Local Variables:
;; byte-compile-warnings: (not make-local)
;; End:
                                  ;;; Copyright © 2019-2020 Michael Allan and contributors.  Licence MIT.
