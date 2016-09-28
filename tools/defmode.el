
;; Font coloring.
(setq def-font-lock-keywords
      `(
        ; Keywords
        (,(regexp-opt '("def" "begin" "end" "do" "done" "while" "if"
                        "then" "fi" "return")
                      'words)
         . font-lock-keyword-face)
        ; Constants.
        (,(regexp-opt '("true" "false") 'words) . font-lock-constant-face)
        ; Single-line comments.
        (,"//.*" . font-lock-comment-face)
        ; Multi-line comments.
        (,"/\\*\\([^*]*\\*+\\)+/" . font-lock-comment-delimiter-face)
        ))

(define-derived-mode def-mode fundamental-mode "def mode"
  "Major mode for editing DEF"
  (setq font-lock-defaults '((def-font-lock-keywords))))

(add-to-list 'auto-mode-alist '("\\.def\\'" . def-mode))

(provide 'def-mode)
