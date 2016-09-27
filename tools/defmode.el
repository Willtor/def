(setq def-keywords
      '("def" "begin" "end" "do" "done" "while" "if" "then"
        "fi" "return"))
(setq def-comments "//.*")
(setq def-multiline-comments "/\\*\\([^*]*\\*+\\)+/")

(setq def-keywords-regexp
      (regexp-opt def-keywords 'words))
(setq def-comments-regexp def-comments)
(setq def-multiline-comments-regexp def-multiline-comments)

(setq def-font-lock-keywords
      `(
        (,def-keywords-regexp . font-lock-keyword-face)
        (,def-comments-regexp . font-lock-comment-face)
        (,def-multiline-comments-regexp . font-lock-comment-delimiter-face)
        ))

(define-derived-mode def-mode fundamental-mode "def mode"
  "Major mode for editing DEF"
  (setq font-lock-defaults '((def-font-lock-keywords))))

(setq def-keywords nil)
(setq def-comments nil)
(setq def-multiline-comments nil)

(setq def-keywords-regexp nil)
(setq def-comments-regexp nil)
(setq def-multiline-comments-regexp nil)

(provide 'def-mode)
