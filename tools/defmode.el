
;; Font coloring.
(setq def-font-lock-keywords
      `(
        ; Single-line comments.
        (,"//.*" . font-lock-comment-face)
        ; Multi-line comments.
        (,"/\\*\\([^*]*\\*\\)+/" . font-lock-comment-delimiter-face)
        ; Keywords
        (,(regexp-opt '("def" "decl" "begin" "end" "do" "done" "while" "for"
                        "if" "then" "elif" "else" "fi" "return" "var" "export"
                        "continue" "typedef" "cast" "as" "goto" "type")
                      'words)
         . font-lock-keyword-face)
        ; Constants.
        (,(regexp-opt '("true" "false" "nil")
                      'words)
         . font-lock-constant-face)
        ; Builtins
        (,(regexp-opt '("sizeof" "new" "delete" "retire" "builtin_cas")
                      'words)
         . font-lock-builtin-face)
        ))

(define-derived-mode def-mode fundamental-mode "def mode"
  "Major mode for editing DEF"
  (setq font-lock-defaults '((def-font-lock-keywords))))

(add-to-list 'auto-mode-alist '("\\.def\\'" . def-mode))

(provide 'def-mode)
