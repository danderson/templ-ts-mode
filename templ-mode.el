;; templ-mode.el --- major mode for editing templ source in Emacs

(require 'go-ts-mode)

(defvar templ-ts--go-font-lock-rules
  `(;; :language templ
    ;; :feature bracket
    ;; ((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

    ;; :language templ
    ;; :feature comment
    ;; ((comment) @font-lock-comment-face)

    ;; :language templ
    ;; :feature constant
    ;; ([(false) (nil) (true)] @font-lock-constant-face
    ;;  ,@(when (go-ts-mode--iota-query-supported-p)
    ;;      '((iota) @font-lock-constant-face))
    ;;  (const_declaration
    ;;   (const_spec name: (identifier) @font-lock-constant-face)))

    ;; :language templ
    ;; :feature delimiter
    ;; ((["," "." ";" ":"]) @font-lock-delimiter-face)

    ;; :language templ
    ;; :feature definition
    ;; ((function_declaration
    ;;   name: (identifier) @font-lock-function-name-face)
    ;;  (method_declaration
    ;;   name: (field_identifier) @font-lock-function-name-face)
    ;;  (method_spec
    ;;   name: (field_identifier) @font-lock-function-name-face)
    ;;  (field_declaration
    ;;   name: (field_identifier) @font-lock-property-name-face)
    ;;  (parameter_declaration
    ;;   name: (identifier) @font-lock-variable-name-face)
    ;;  (short_var_declaration
    ;;   left: (expression_list
    ;;          (identifier) @font-lock-variable-name-face
    ;;          ("," (identifier) @font-lock-variable-name-face)*))
    ;;  (var_spec name: (identifier) @font-lock-variable-name-face
    ;;            ("," name: (identifier) @font-lock-variable-name-face)*))

    ;; :language templ
    ;; :feature function
    ;; ((call_expression
    ;;   function: (identifier) @font-lock-function-call-face)
    ;;  (call_expression
    ;;   function: (selector_expression
    ;;              field: (field_identifier) @font-lock-function-call-face)))

    ;; :language templ
    ;; :feature keyword
    ;; ([,@go-ts-mode--keywords] @font-lock-keyword-face)

    ;; :language templ
    ;; :feature label
    ;; ((label_name) @font-lock-constant-face)

    ;; :language templ
    ;; :feature number
    ;; ([(float_literal)
    ;;   (imaginary_literal)
    ;;   (int_literal)] @font-lock-number-face)

    ;; :language templ
    ;; :feature string
    ;; ([(interpreted_string_literal)
    ;;   (raw_string_literal)
    ;;   (rune_literal)] @font-lock-string-face)

    ;; :language templ
    ;; :feature type
    ;; ([(package_identifier) (type_identifier)] @font-lock-type-face)

    ;; :language templ
    ;; :feature property
    ;; ((selector_expression field: (field_identifier) @font-lock-property-use-face)
    ;;  (keyed_element (_ (identifier) @font-lock-property-use-face)))

    ;; :language templ
    ;; :feature variable
    ;; ((identifier) @font-lock-variable-use-face)

    ;; :language templ
    ;; :feature escape-sequence
    ;; :override t
    ;; ((escape_sequence) @font-lock-escape-face)

    ;; :language templ
    ;; :feature error
    ;; :override t
    ;; ((ERROR) @font-lock-warning-face))
  ))

(defvar templ-ts-font-lock-rules
  `(:language templ
    :feature keyword
    ("templ" @font-lock-keyword-face
     [,@go-ts-mode--keywords] @font-lock-keyword-face)

    :language templ
    :feature error
    ((ERROR) @font-lock-warning-face)

    :language templ
    :feature todo
    ((function_declaration name: (identifier) @font-lock-function-name-face)
     (component_declaration name: (component_identifier) @font-lock-function-name-face))

    :language templ
    :feature todo
    ((call_expression function: (identifier) @font-lock-function-call-face)
     (call_expression
      function: (selector_expression
                 field: (field_identifier) @font-lock-function-call-face)))

    :language templ
    :feature todo
    ((parameter_declaration name: (identifier) @font-lock-variable-name-face))

    :language templ
    :feature todo
    ((short_var_declaration
      left: (expression_list
             (identifier) @font-lock-variable-name-face
             ("," (identifier) @font-lock-variable-name-face)*)))

    :language templ
    :feature todo
    ([(true) (false) (nil)] @font-lock-constant-face
     (const_spec name: (identifier) @font-lock-constant-face)
     (iota) @font-lock-constant-face)

    :language templ
    :feature todo
    ((type_spec name: (type_identifier) @font-lock-type-face))

    ;; TODO: builtin face?

    :language templ
    :feature todo
    (["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

    :language templ
    :feature todo
    ((escape_sequence) @font-lock-escape-face)

    :language templ
    :feature todo
    ((interpreted_string_literal) @font-lock-string-face)

    :language templ
    :feature todo
    ((comment) @font-lock-comment-face)

    :language templ
    :feature todo
    ((identifier) @font-lock-variable-use-face)

    ;; :language templ
    ;; :feature definition
    ;; ((component_declaration
    ;;   name: (component_identifier) @font-lock-function-name-face))

    ;; :language templ
    ;; :feature tag
    ;; ((element
    ;;   [(tag_start name: (element_identifier) @font-lock-variable-name-face)
    ;;    (self_closing_tag name: (element_identifier) @font-lock-variable-name-face)
    ;;    (tag_end name: (element_identifier) @font-lock-variable-name-face)]))

    ;; :language templ
    ;; :feature attribute
    ;; ((attribute
    ;;   name: (attribute_name) @font-lock-constant-face
    ;;   "="
    ;; value: (quoted_attribute_value) @font-lock-string-face))
    ))

(defun templ-ts-setup ()
  (interactive)
  (setq-local treesit-font-lock-feature-list
              '((comment definition error todo
                keyword string type
                constant escape-sequence label number tag attribute
                bracket delimiter function operator property variable)))

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules (append templ-ts--go-font-lock-rules templ-ts-font-lock-rules)))
  (treesit-major-mode-setup))

(define-derived-mode templ-ts-mode go-ts-mode "Templ"
  "Major mode for editing Templ files."
  (when (treesit-ready-p 'templ)
    (treesit-parser-create 'templ)
    (templ-ts-setup)))

(defun combobulate-templ-setup ())
(defconst combobulate-rules-templ '())
(defconst combobulate-rules-templ-inverted '())

(add-to-list 'combobulate-setup-functions-alist '(templ . combobulate-templ-setup))

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
