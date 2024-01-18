;;; templ-ts-mode.el --- Major mode for editing Templ files -*- lexical-binding: t; -*-

;; Copyright (C) 2024 David Anderson

;; Author: David Anderson <dave@natulte.net>
;; Created: 2024-01-01
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/danderson/templ-ts-mode

;; This file is not a part of GNU Emacs.

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

;; This package provides syntax highlighting and indentation for Templ
;; files.  To use the `templ-ts-mode` major mode, the Templ and
;; Javascript tree-sitter grammars must be installed.  For more
;; information about Templ's syntax and use, visit the Templ website
;; at <https://templ.guide>.

;;; Code:

(require 'treesit)
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'rx))

(require 'eglot)
(require 'go-ts-mode)
(require 'js)
(require 'css-mode)

(defgroup templ-ts nil
  "Major mode for Templ, using Tree-Sitter."
  :group 'languages
  :link '(emacs-library-link :tag "Source" "templ-ts-mode.el")
  :prefix "templ-ts-mode-")

(defcustom templ-ts-mode-grammar "https://github.com/vrischmann/tree-sitter-templ"
  "Configuration for downloading and installing the tree-sitter language grammar."
  :type '(string)
  :group 'templ-ts
  :version "0.1")

(defcustom templ-ts-mode-grammar-install 'prompt
  "Automatic installation of the tree-sitter language grammar library."
  :type '(choice (const :tag "Install automatically" auto)
                 (const :tag "Prompt to install" prompt)
                 (const :tag "Do not install" nil))
  :group 'templ-ts
  :version "0.1")

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
     ((node-is "script_element_text") parent-bol go-ts-mode-indent-offset)
     ((parent-is "script_element") parent-bol 0)
     ((node-is "style_element_text") parent-bol go-ts-mode-indent-offset)
     ((parent-is "style_element") parent-bol 0)
     ((parent-is "element") parent-bol go-ts-mode-indent-offset)
     ((parent-is "tag_start") parent-bol go-ts-mode-indent-offset)
     ((parent-is "self_closing_tag") parent-bol go-ts-mode-indent-offset)

     ;; Steal all of Go's rules, which covers almost all of what Templ
     ;; needs. They need to get evaluated last because they end with a
     ;; no-node rule that would overrule all rules that come after.
     ,@(cdar go-ts-mode--indent-rules))))

(defun templ-ts--treesit-update-ranges (start end)
  "Update child parser ranges between START and END."
  (ignore start end)
  (let ((js-ranges (or (treesit-query-range 'templ '((script_block_text) @js))
                       '((1 . 1)))))
    (treesit-parser-set-included-ranges (treesit-parser-create 'javascript) js-ranges)))

(defvar templ-ts--range-rules
  '(templ-ts--treesit-update-ranges))

(defvar templ-ts--font-lock-feature-list
  '((comment definition)
    (keyword string type)
    (constant escape-sequence label number assignment jsx pattern string-interpolation tag attribute)
    (bracket delimiter error function operator property variable)))

(defun templ-ts--treesit-language-at-point (point)
  "Return the language at POINT."
  (let ((js (treesit-parser-create 'javascript)))
    (if (null (treesit-parser-included-ranges js))
        'templ
      (let ((js-range (treesit-parser-range-on js point)))
        (cond
         ((null js-range)
          'templ)
         ((eq point (car js-range))
          'templ)
         ((eq point (cdr js-range))
          'templ)
         (t 'javascript))))))

(defun templ-ts--setup ()
  "Setup for `templ-ts-mode`."
  (unless (treesit-available-p)
    (error "Tree-sitter is not available"))

  (unless (treesit-language-available-p 'javascript)
    (error "Tree-sitter for Javascript isn't available"))

  ;; Grammar.
  (setq-local treesit-language-source-alist
              `((templ . (,templ-ts-mode-grammar))))

  (when (and (not (treesit-language-available-p 'templ))
             (pcase templ-ts-mode-grammar-install
               ('auto t)
               ('prompt
                (let ((y-or-n-p-use-read-key t))
                  (y-or-n-p
                   (format "Tree-sitter grammar for Templ is missing.  Install from %s?"
                           (car (alist-get 'templ treesit-language-source-alist))))))
               (_ nil)))
    (message "Installing the tree-sitter grammar for Templ")
    (treesit-install-language-grammar 'templ))

  (unless (treesit-language-available-p 'templ)
    (error "Tree-sitter for Templ isn't available"))

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
  (setq-local indent-tabs-mode t
              treesit-simple-indent-rules templ-ts--indent-rules)

  ;; Electric.
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

;;;###autoload
(define-derived-mode templ-ts-mode prog-mode "Templ"
  "Major mode for editing Templ files."
  :group 'templ-ts

  (templ-ts--setup))

(add-to-list 'eglot-server-programs '(templ-ts-mode
                                      "templ" "lsp"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.templ\\'" . templ-ts-mode))

(provide 'templ-ts-mode)
;;; templ-ts-mode.el ends here
