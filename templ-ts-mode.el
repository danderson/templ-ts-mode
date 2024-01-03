;;; templ-ts-mode.el --- major mode for editing Templ files     -*- lexical-binding: t; -*-

;; Copyright (C) 2024 David Anderson

;; Author: David Anderson <dave@natulte.net>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(require 'go-ts-mode)
(require 'js)
(require 'css-mode)

(defvar templ-ts--go-font-lock-rules
  ;; Rules taken from go-ts-mode--font-lock-settings.  Unfortunately I
  ;; need to retarget these rules to :language templ, but go-ts-mode
  ;; only provides the compiled font lock settings in a var, not the
  ;; source.  As a result, I had to copy it here rather than being able
  ;; to borrow it live.
  (list

   :language 'templ
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'templ
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'templ
   :feature 'constant
   `([(false) (nil) (true) (iota)] @font-lock-constant-face
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
  ;; Unlike the previous var, these rules are specific to Templ's
  ;; syntax extensions.
  '(:language templ
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

    :language templ
    :feature property
    ((css_property_name) @css-property)))

(defvar templ-ts--indent-rules
  `(;; Javascript used for script blocks that use the javascript
    ;; sub-parser.
    ,(car js--treesit-indent-rules)
    ;; Templ rules, for the rest of the file.
    (templ
     ;; Steal all of Go's rules, which covers almost all of what Templ
     ;; needs.
     ,@(cdar go-ts-mode--indent-rules)

     ((parent-is "css_declaration") parent-bol go-ts-mode-indent-offset)
     ((parent-is "component_declaration") parent-bol go-ts-mode-indent-offset)
     ;; No script or component indent rules for the first body line,
     ;; they get handled by Go's 'block' rule. Subsequent lines are
     ;; handled by the js language rules for script blocks, and by
     ;; element indent logic below for components.

     ;; HTML elements and attributes within components.
     ((node-is "/>") parent-bol 1)
     ((node-is "tag_end") parent-bol 0)
     ((node-is "attribute") prev-sibling 0)
     ((parent-is "element") parent-bol go-ts-mode-indent-offset)
     ((parent-is "tag_start") parent-bol go-ts-mode-indent-offset)
     ((parent-is "self_closing_tag") parent-bol go-ts-mode-indent-offset))))

(defvar templ-ts--range-rules
  '(:embed javascript
    :host templ
    '((script_block_text) @js)))

(defvar templ-ts--font-lock-feature-list
  '((comment definition)
    (keyword string type)
    (constant escape-sequence label number assignment jsx pattern string-interpolation tag attribute)
    (bracket delimiter error function operator property variable)))

(defun templ-ts--treesit-language-at-point (point)
  "Return the language at POINT."
  (let* ((js (treesit-parser-create 'javascript))
         (js-range (treesit-parser-range-on js point)))
    (cond
     ((null js-range)
      'templ)
     ((eq point (car js-range))
      'templ)
     ((eq point (cdr js-range))
      'templ)
     (t 'javascript))))

(defun templ-ts--setup ()
  "Setup for `templ-ts-mode`."
  (treesit-parser-create 'javascript)
  (treesit-parser-create 'templ)

  ;; Comments.
  (setq-local comment-start "// "
              comment-end ""
              comment-start-skip (rx "//" (* (syntax whitespace))))

  ;; Child language handling.
  (setq-local treesit-language-at-point-function
              #'templ-ts--treesit-language-at-point)

  (setq-local treesit-range-settings
              (apply #'treesit-range-rules
                     templ-ts--range-rules))

  ;; Indent.
  (setq-local treesit-simple-indent-rules
              templ-ts--indent-rules)

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}()<>" electric-indent-chars))

  ;; Font-lock.
  (setq-local treesit-font-lock-feature-list
              templ-ts--font-lock-feature-list)

  (setq-local treesit-font-lock-settings
              (let* ((root-rules (append templ-ts--templ-font-lock-rules
                                         templ-ts--go-font-lock-rules))
                     (root-compiled (apply #'treesit-font-lock-rules
                                           root-rules))
                     (js-compiled js--treesit-font-lock-settings))
                (append js-compiled root-compiled)))

  (treesit-major-mode-setup))

(define-derived-mode templ-ts-mode prog-mode "Templ"
  "Major mode for editing Templ files."
  (when (and (treesit-ready-p 'templ)
             (treesit-ready-p 'javascript))
    (templ-ts--setup)))

;; Debugging stuff

(defun templ-ts--ultravomit ()
  "Highlight every font-lock face with ugly colored boxes."
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

(defun templ-ts--ultravomit-clear ()
  "Undo the effect of templ-ts--ultravomit."
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

(defvar templ-ts--ultravomit-parser-overlays nil)

(defun templ-ts--ultravomit-subparsers ()
  "Highlight buffer regions that are delegated to a subparser."
  (dolist (parser (treesit-parser-list))
    (let ((language (treesit-parser-language parser))
          (ranges (treesit-parser-included-ranges parser))
          (overlays nil))
      (dolist (range ranges)
        (let ((overlay (make-overlay (car range) (cdr range))))
          (overlay-put overlay 'face '(:background "#110022"))
          (push overlay overlays)))
      (dolist (overlay (plist-get templ-ts--ultravomit-parser-overlays language))
        (delete-overlay overlay))
      (setq-local ultravomit-parser-overlays (plist-put ultravomit-parser-overlays language overlays)))))

(defun templ-ts--ultravomit-subparsers-clear ()
  "Undo the effect of templ-ts--ultravomit-subparsers."
  (let ((alist (seq-partition templ-ts--ultravomit-parser-overlays 2)))
    (dolist (language alist)
      (dolist (overlay (cadr language))
        (delete-overlay overlay))))
  (setq-local ultravomit-parser-overlays nil))

(provide 'templ-ts-mode)
;;; templ-ts-mode.el ends here
