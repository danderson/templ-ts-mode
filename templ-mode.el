;; templ-mode.el --- major mode for editing templ source in Emacs

(require 'go-ts-mode)

(defvar templ-ts--go-font-lock-rules
  `(
    ))

(defvar templ-ts--templ-font-lock-rules
  `(
    ))

(defvar templ-ts-range-rules
  '(:embed go
    :host templ
    ((source_file [(package_clause) (import_declaration) (const_declaration) (function_declaration) (type_declaration)]))
    ; ((source_file [(package_clause) (import_declaration) (const_declaration) (function_declaration) (type_declaration) (comment) (method_declaration) (var_declaration)] @h))
    ))

(defun templ-ts--treesit-node-to-range (node)
  (cons (treesit-node-start node)
        (treesit-node-end node)))

(defun templ-ts--locate-non-go-bits ()
  (let* ((query '((source_file [(component_declaration)
                                (css_declaration)
                                (script_declaration)] @templ)))
         (nodes (treesit-query-capture 'templ query nil nil t))
         (ranges (mapcar #'templ-ts--treesit-node-to-range nodes)))
    ranges))

(defun templ-ts--locate-go-bits ()
  (let* ((all (templ-ts--treesit-node-to-range (treesit-buffer-root-node 'templ)))
         (templ-bits (templ-ts--locate-non-go-bits))
         (diff (range-difference all templ-bits))
         (remove-end (lambda (range) (or (listp range)
                                         (not (equal range (buffer-end 1))))))
         (canonicalize (lambda (range) (if (listp range)
                                           range
                                         (cons range (+ 1 range))))))
    (seq-map canonicalize (seq-filter remove-end diff))))

(defun templ-ts-setup ()
  (interactive)
  (setq-local treesit-font-lock-feature-list
              '((comment definition error todo
                keyword string type
                constant escape-sequence label number tag attribute
                bracket delimiter function operator property variable)))

  (setq-local treesit-font-lock-settings
              (append
               go-ts-mode--font-lock-settings
               (apply #'treesit-font-lock-rules templ-ts-font-lock-rules)))


  (setq-local treesit-range-settings
              (treesit-range-rules
               (lambda (start end)
                 (treesit-parser-set-included-ranges (treesit-parser-create 'go)
                                                     (templ-ts--locate-go-bits)))))

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
