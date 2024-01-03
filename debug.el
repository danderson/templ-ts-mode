(setq combobulate-rules-templ '())
(setq combobulate-rules-templ-inverted '())

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
          (overlay-put overlay 'face '(:background "#110022"))
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

