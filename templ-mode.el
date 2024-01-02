;; templ-mode.el --- major mode for editing templ source in Emacs

(require 'go-ts-mode)

(defvar templ-ts--go-font-lock-rules
  (list

   :language 'templ
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'templ
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'templ
   :feature 'constant
   `([(false) (nil) (true)] @font-lock-constant-face
     ,@(when (go-ts-mode--iota-query-supported-p)
         '((iota) @font-lock-constant-face))
     (const_declaration
      (const_spec name: (identifier) @font-lock-constant-face)))

   :language 'templ
   :feature 'delimiter
   '((["," "." ";" ":"]) @font-lock-delimiter-face)

   :language 'templ
   :feature 'definition
   '((function_declaration
      name: (identifier) @font-lock-function-name-face)
     (method_declaration
      name: (field_identifier) @font-lock-function-name-face)
     (method_spec
      name: (field_identifier) @font-lock-function-name-face)
     (field_declaration
      name: (field_identifier) @font-lock-property-name-face)
     (parameter_declaration
      name: (identifier) @font-lock-variable-name-face)
     (short_var_declaration
      left: (expression_list
             (identifier) @font-lock-variable-name-face
             ("," (identifier) @font-lock-variable-name-face)*))
     (var_spec name: (identifier) @font-lock-variable-name-face
               ("," name: (identifier) @font-lock-variable-name-face)*))

   :language 'templ
   :feature 'function
   '((call_expression
      function: (identifier) @font-lock-function-call-face)
     (call_expression
      function: (selector_expression
                 field: (field_identifier) @font-lock-function-call-face)))

   :language 'templ
   :feature 'keyword
   `([,@go-ts-mode--keywords] @font-lock-keyword-face)

   :language 'templ
   :feature 'label
   '((label_name) @font-lock-constant-face)

   :language 'templ
   :feature 'number
   '([(float_literal)
      (imaginary_literal)
      (int_literal)] @font-lock-number-face)

   :language 'templ
   :feature 'string
   '([(interpreted_string_literal)
      (raw_string_literal)
      (rune_literal)] @font-lock-string-face)

   :language 'templ
   :feature 'type
   '([(package_identifier) (type_identifier)] @font-lock-type-face)

   :language 'templ
   :feature 'property
   '((selector_expression field: (field_identifier) @font-lock-property-use-face)
     (keyed_element (_ (identifier) @font-lock-property-use-face)))

   :language 'templ
   :feature 'variable
   '((identifier) @font-lock-variable-use-face)

   :language 'templ
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'templ
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face)))

(defvar templ-ts--templ-font-lock-rules
  `(:language templ
    :feature keyword
    (["templ" "css" "script"] @font-lock-keyword-face)

    :language templ
    :feature definition
    ((component_declaration name: (component_identifier) @font-lock-function-name-face)
     (css_declaration name: (css_identifier) @font-lock-function-name-face)
     (script_declaration name: (script_identifier) @font-lock-function-name-face))

    :language templ
    :feature delimiter
    (["<!" "<" ">" "/>" "</"] @font-lock-bracket-face)

    :language templ
    :feature attribute
    ((attribute name: (attribute_name) @font-lock-constant-face
                "=" @font-lock-bracket-face
                value: (quoted_attribute_value) @font-lock-string-face)
     (attribute name: (attribute_name) @font-lock-constant-face
                "=" @font-lock-bracket-face
                value: (attribute_value) @font-lock-constant-face))

    :language templ
    :feature tag
    ((tag_start name: (element_identifier) @font-lock-function-call-face)
     (tag_end name: (element_identifier) @font-lock-function-call-face)
     (self_closing_tag name: (element_identifier) @font-lock-function-call-face))

    :language templ
    :feature function
    ((component_import "@" @font-lock-bracket-face
                       name: (component_identifier) @font-lock-function-call-face))
    ))

(defvar templ-ts--indent-rules
  `((templ
     ,@(cdar go-ts-mode--indent-rules)
     ((parent-is "css_declaration") parent-bol 2)

     )))

(defun templ-ts--notch-range (range notch)
  "Return the portions of RANGE on either side of NOTCH.
RANGE, NOTCH are both (start . end) cons pairs.  The return value
is a list of 2 ranges in the same format, giving the portions of
RANGE that fall before and after NOTCH.  Both values can be nil
if NOTCH overlaps one or both ends of RANGE."
  (let* ((range-start (car range))
         (range-end (cdr range))
         (notch-start (min (max (car notch) range-start)
                           range-end))
         (notch-end (max (min (cdr notch) range-end)
                         range-start))
         (rem-start (if (equal range-start notch-start)
                        nil
                      (cons range-start notch-start)))
         (rem-end (if (equal range-end notch-end)
                      nil
                    (cons notch-end range-end))))
    (list rem-start rem-end)))

(ert-deftest templ-ts--test-notch-range ()
  "Tests the range notching function."
  (should (equal (templ-ts--notch-range '(100 . 200) '(0 . 50))
                 '(nil (100 . 200))))
  (should (equal (templ-ts--notch-range '(100 . 200) '(0 . 300))
                 '(nil nil)))
  (should (equal (templ-ts--notch-range '(100 . 200) '(50 . 150))
                 '(nil (150 . 200))))
  (should (equal (templ-ts--notch-range '(100 . 200) '(150 . 250))
                 '((100 . 150) nil)))
  (should (equal (templ-ts--notch-range '(100 . 200) '(250 . 300))
                 '((100 . 200) nil)))
  (should (equal (templ-ts--notch-range '(100 . 200) '(120 . 180))
                 '((100 . 120) (180 . 200))))
  )

(defun templ-ts--accumulate-notch (acc notch)
  "Notch the first element of ACC with NOTCH.
Places the results of the notching back onto the front of ACC.
Intended for use in a seq-reduction."
  (let* ((range (car acc))
         (rest (cdr acc))
         (split (templ-ts--notch-range range notch))
         (clean (nreverse (seq-filter #'identity split))))
    (append clean rest)))

(defun templ-ts--invert-source-ranges (ranges start end)
  ""
  ; Empirically, treesit-query-range returns ranges in order of
  ; appearance in the buffer. However, the documentation doesn't
  ; guarantee that behavior. Defensively sort, just in case.
  (sort ranges (lambda (r1 r2) (< (car r1) (car r2))))
  (let* ((init-range (cons start end))
         (reduction (seq-reduce #'templ-ts--accumulate-notch
                                ranges
                                (list init-range))))
    (nreverse reduction)))

(defun templ-ts--range-is-whitespace (range)
  ""
  (string-blank-p (buffer-substring-no-properties (car range) (cdr range))))

(defun templ-ts--range-length (range)
  ""
  (- (cdr range) (car range)))

(defun templ-ts--remove-whitespace (ranges)
  ""
  (seq-filter (lambda (range)
                (or (> (templ-ts--range-length range) 10)
                    (not (templ-ts--range-is-whitespace range))))
              ranges))

(defun templ-ts--top-level-templ-ranges (start end)
  ""
  (let ((query '((source_file [(css_declaration)
                                (script_declaration)
                                (component_declaration)] @templ))))
    (treesit-query-range 'templ query start end)))

(defun templ-ts--top-level-go-ranges (start end)
  ""
  (interactive)
  (templ-ts--remove-whitespace
   (templ-ts--invert-source-ranges (templ-ts--top-level-templ-ranges start end)
                                   start end)))

(defun templ-ts--set-go-ranges (start end)
  ""
  (let ((ranges (templ-ts--top-level-go-ranges (point-min) (point-max)))
         (parser (treesit-parser-create 'go)))
    (treesit-parser-set-included-ranges parser ranges)))

(defvar templ-ts--range-rules
  (list

   #'templ-ts--set-go-ranges

   ;; :embed 'go
   ;; :host 'templ
   ;; '((element (expression "{" (_) @capture "}")))

   ))

(defun templ-ts-setup ()
  (interactive)
  (setq-local treesit-font-lock-feature-list
              '((comment definition error todo
                keyword string type
                constant escape-sequence label number tag attribute
                bracket delimiter function operator property variable)))

  (setq-local treesit-font-lock-settings
              (append go-ts-mode--font-lock-settings
                      (apply #'treesit-font-lock-rules
                             templ-ts--templ-font-lock-rules)))

  (setq-local treesit-simple-indent-rules templ-ts--indent-rules)

  (setq-local treesit-range-settings
              (apply #'treesit-range-rules templ-ts--range-rules))

  (treesit-major-mode-setup))

(setq combobulate-rules-templ '())
(setq combobulate-rules-templ-inverted '())

(define-derived-mode templ-ts-mode go-ts-mode "Templ"
  "Major mode for editing Templ files."
  (when (treesit-ready-p 'templ)
    (treesit-parser-create 'templ)
    (templ-ts-setup)))

(defun ultravomit ()
  (interactive)
  (cl-flet ((box (lambda (face color)
                   (face-remap-set-base face :box `(:line-width 3 :color ,color :style 'released-button))))
            (bg (lambda (face color)
                  (face-remap-set-base face :background color :foreground "#ffffff"))))
    (box 'font-lock-warning-face "#ff0000")
    (box 'font-lock-function-name-face "#0000ff")
    (box 'font-lock-function-call-face "#000088")
    (box 'font-lock-variable-name-face "#00aa00")
    (box 'font-lock-variable-use-face "#008800")
    (box 'font-lock-keyword-face "#880000")
    (box 'font-lock-comment-face "#888888")
    (box 'font-lock-comment-delimiter-face "#555555")
    (box 'font-lock-type-face "#aaaa00")
    (box 'font-lock-constant-face "#008888")
    (box 'font-lock-builtin-face "#ffc0cb")
    (box 'font-lock-preprocessor-face "#ffffff")
    (box 'font-lock-string-face "#005500")
    (box 'font-lock-doc-face "#fffacd")
    (box 'font-lock-doc-markup-face "#ffdab9")
    (box 'font-lock-negation-char-face "#00ffff")
    (bg 'font-lock-escape-face "#ffff00")
    (bg 'font-lock-number-face "#ff0000")
    (bg 'font-lock-operator-face "#0000ff")
    (bg 'font-lock-property-name-face "#0000aa")
    (bg 'font-lock-property-use-face "#000088")
    (bg 'font-lock-punctuation-face "#444444")
    (bg 'font-lock-bracket-face "#008888")
    (bg 'font-lock-delimiter-face "#aaaa00")
    (bg 'font-lock-misc-punctuation-face "#ffd700")
  ))

(defun ultravomit-clear ()
  (interactive)
  (mapc #'face-remap-reset-base
        '(font-lock-warning-face
          font-lock-function-name-face
          font-lock-function-call-face
          font-lock-variable-name-face
          font-lock-variable-use-face
          font-lock-keyword-face
          font-lock-comment-face
          font-lock-comment-delimiter-face
          font-lock-type-face
          font-lock-constant-face
          font-lock-builtin-face
          font-lock-preprocessor-face
          font-lock-string-face
          font-lock-doc-face
          font-lock-doc-markup-face
          font-lock-negation-char-face
          font-lock-escape-face
          font-lock-number-face
          font-lock-operator-face
          font-lock-property-name-face
          font-lock-property-use-face
          font-lock-punctuation-face
          font-lock-bracket-face
          font-lock-delimiter-face
          font-lock-misc-punctuation-face
          )
        ))

(defvar ultravomit-parser-overlays nil)

(defun ultravomit-subparsers ()
  ""
  (interactive)
  (dolist (parser (treesit-parser-list))
    (let ((language (treesit-parser-language parser))
          (ranges (treesit-parser-included-ranges parser))
          (overlays nil))
      (dolist (range ranges)
        (let ((overlay (make-overlay (car range) (cdr range))))
          (overlay-put overlay 'face '(:background "#330000"))
          (push overlay overlays)))
      (dolist (overlay (plist-get ultravomit-parser-overlays language))
        (delete-overlay overlay))
      (setq-local ultravomit-parser-overlays (plist-put ultravomit-parser-overlays language overlays)))))

(defun ultravomit-subparsers-clear ()
  ""
  (interactive)
  (let ((alist (seq-partition ultravomit-parser-overlays 2)))
    (dolist (language alist)
      (dolist (overlay (cadr language))
        (delete-overlay overlay))))
  (setq-local ultravomit-parser-overlays nil))

(provide 'templ-mode)
