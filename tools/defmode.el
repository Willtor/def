(defvar def-mode-hook nil)

(defvar def-tab-width 4)

;;;
;;; Custom faces for keywords
;;;

(defface xcase
  '((((class color) (min-colors 88) (background light))
     :background "misty rose"
     :foreground "dark red")
    (((class color) (min-colors 88) (background dark))
     :background "firebrick"
     :foreground "white")
    (((class color) (min-colors 16) (background light))
     :background "misty rose"
     :foreground "dark red")
    (((class color) (min-colors 16) (background dark))
     :background "firebrick"
     :foreground: "white")
    (((class color) (min-colors 8))
     :background "red" :foreground "white")
    (t :inverse-video t))
  "Highlighting for the xcase keyword."
  :group 'def-mode)

(defface ocase
  '((((class color) (min-colors 88) (background light))
     :background "light goldenrod yellow"
     :foreground "saddle brown")
    (((class color) (min-colors 88) (background dark))
     :background "gold"
     :foreground "black")
    (((class color) (min-colors 16) (background light))
     :background "light goldenrod yellow"
     :foreground "saddle brown")
    (((class color) (min-colors 16) (background dark))
     :background "gold"
     :foreground: "black")
    (((class color) (min-colors 8))
     :background "yellow" :foreground "black")
    (t :inverse-video t))
  "Highlighting for the xcase keyword."
  :group 'def-mode)

;; Font coloring.
(setq def-font-lock-keywords
      `(
        ; Single-line comments.
        (,"//.*" . font-lock-comment-face)
        ; Multi-line comments.
        (,"/\\*\\([^*]\\|\\*+[^*/]\\)*\\*+*/"
         . font-lock-comment-delimiter-face)
        ; Keywords
        (,(regexp-opt '("def" "decl" "begin" "end" "do" "od" "while" "for"
                        "parfor" "xbegin" "xend"
                        "if" "then" "elif" "else" "fi" "return" "var" "export"
                        "continue" "typedef" "goto" "type"
                        "switch" "with" "esac")
                      'words)
         . font-lock-keyword-face)
        (,(regexp-opt '("xcase")
                      'words)
         . 'xcase)
        (,(regexp-opt '("ocase")
                      'words)
         . 'ocase)
        ; Constants.
        (,(regexp-opt '("true" "false" "nil")
                      'words)
         . font-lock-constant-face)
        ; Builtins
        (,(regexp-opt '("import" "sizeof" "new" "delete" "retire"
                        "builtin_cas" "builtin_swap" "spawn" "sync" "cast"
                        "atomic" "case")
                      'words)
         . font-lock-builtin-face)
        ))

;;;
;;; Indentation
;;;

;; Keywords.
(setq def-keywords-open
      (regexp-opt '("begin" "then" "do" "with" "xbegin") 'words))
(setq def-keywords-close
      (regexp-opt '("end" "fi" "od" "esac" "xend") 'words))
(setq def-keywords-case
      (regexp-opt '("xcase" "ocase") 'words))

; Adapted from wpdl-mode.el.
(defun def-indent-line ()
  "DEF indentation for the current line."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t)
          (offset (if (looking-at (concat "^[ ]*" def-keywords-case))
                      (- 0 def-tab-width) 0)
                  )
          cur-indent)
      (if (looking-at (concat "^[ ]*" def-keywords-close))
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) def-tab-width)))
            (if (< cur-indent 0) (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at (concat "^[ ]*" def-keywords-close))
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at (concat "^.*" def-keywords-open))
                  (progn
                    (setq cur-indent
                          (if (looking-at (concat "^.*" def-keywords-close))
                              (current-indentation)
                            (+ (current-indentation) def-tab-width offset)))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

(add-to-list 'auto-mode-alist '("\\.def\\'" . def-mode))
(add-to-list 'auto-mode-alist '("\\.defi\\'" . def-mode))

(defun def-mode ()
  "Major mode for editing DEF Engineering Framework files"
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults) '(def-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'def-indent-line)
  (setq major-mode 'def-mode)
  (setq mode-name "DEF")
  (run-hooks 'def-mode-hook))

(provide 'def-mode)
