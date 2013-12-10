(deftheme steady
  "Created 2013-03-06.")

;; Set attributes of the default face for existing frames and new frames.
(set-face-attribute 'default nil :background "#ffffff" :foreground "#272727")
(set-face-attribute 'cursor nil :background "#c7243a")

(lexical-let ((gray   "gray70")
              (black  "#272727")
              (red    "#c7243a")
              (yellow "#d59b0a")
              (green  "#839e19")
              (blue   "#006ea5")
              (purple "#744199"))
  (custom-theme-set-faces
   'steady
   ;; Standard
   `(default ((t (:background "#ffffff" :foreground ,black))))
   `(fixed-pitch ((t (:family "monospace"))))
   `(variable-pitch ((t (:family "sans serif"))))
   ;; Frame
   `(cursor ((t (:background ,red))))
   `(fringe ((t (:background "gray90" :foreground "gray40"))))
   `(linum ((t (:inherit (fringe)))))
   `(mode-line ((t (:background "gray80" :foreground "gray10" :box nil :weight bold))))
   `(mode-line-inactive ((t (:background "gray20" :foreground "gray50" :box nil))))
   `(powerline-mode-normal   ((t (:background "#afd700" :foreground "#005f00" :box nil :weight bold))))
   `(powerline-mode-insert   ((t (:background "white"   :foreground "#005f5f" :box nil :weight bold))))
   `(powerline-mode-visual   ((t (:background "#ffaf00" :foreground "#875f00" :box nil :weight bold))))
   `(powerline-mode-replace  ((t (:background "#d70000" :foreground "white"   :box nil :weight bold))))
   `(powerline-mode-operator ((t (:background "#d70000" :foreground "white"   :box nil :weight bold))))
   `(powerline-mode-emacs    ((t (:background "gray60"  :foreground "black"   :box nil :weight bold))))
   `(powerline-mode-inactive ((t (:background "gray60"  :foreground "gray30"  :box nil))))
   `(powerline-first-normal   ((t (:background "gray80" :foreground "gray30"))))
   `(powerline-first-insert   ((t (:background "#87d7ff" :foreground "#005f87"))))
   `(powerline-first-inactive ((t (:background "gray60" :foreground "gray30"))))
   `(powerline-second-normal   ((t (:background "gray60" :foreground "white"))))
   `(powerline-second-insert   ((t (:background "#0087af" :foreground "#87d7ff"))))
   `(powerline-second-inactive ((t (:background "gray40" :foreground "gray70"))))
   `(powerline-third-normal   ((t (:background "gray40" :foreground "gray70"))))
   `(powerline-third-insert   ((t (:background "#005f87" :foreground "#87d7ff"))))
   `(powerline-third-inactive ((t (:background "gray20" :foreground "gray40"))))
   `(powerline-buffer-id-normal   ((t (:inherit (powerline-second-normal)))))
   `(powerline-buffer-id-insert   ((t (:inherit (powerline-second-insert)))))
   `(powerline-buffer-id-inactive ((t (:inherit (powerline-second-inactive)))))

   `(minibuffer-prompt ((t (:foreground ,black))))
   ;; Highlight
   `(highlight ((t (:background "gray75"))))
   `(isearch ((t (:background ,blue :foreground "black"))))
   `(query-replace ((t (:inherit (isearch)))))
   `(lazy-highlight ((t (:background "#ffaa33" :foreground "black"))))
   `(region ((t (:background "#f6d4d8" :foreground unspecified))))
   `(secondary-selection ((t (:background "#272822"))))
   `(trailing-whitespace ((((class color)) (:background "red")) (t (:inverse-video t))))
   `(escape-glyph ((t (:foreground "#5599ff"))))
   ;; whitespace
   `(whitespace-line ((t (:background "yellow" :foreground "black"))))
   ;; Main
   `(font-lock-builtin-face ((t (:foreground ,red))))
   `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
   `(font-lock-comment-face ((t (:foreground ,gray))))
   `(font-lock-constant-face ((t (:foreground ,purple))))
   `(font-lock-doc-face ((t (:foreground ,gray :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,green :slant italic))))
   `(font-lock-keyword-face ((t (:foreground ,red))))
   `(font-lock-negation-char-face ((t (:weight bold :foreground "#e7f6da"))))
   `(font-lock-preprocessor-face ((t (:foreground ,red))))
   `(font-lock-regexp-grouping-backslash ((t (:weight bold))))
   `(font-lock-regexp-grouping-construct ((t (:weight bold))))
   `(font-lock-string-face ((t (:foreground ,purple))))
   `(font-lock-type-face ((t (:foreground ,blue))))
   `(font-lock-variable-name-face ((t (:foreground ,yellow))))
   `(font-lock-warning-face ((t (:background "#ffff00" :foreground "#000000"))))
   ;; show-paren-mode
   `(show-paren-match-face ((t (:background nil :foreground "red"))))
   ;; auto-complete
   `(ac-completion-face ((t (:foreground ,gray :underline nil))))
   `(ac-candidate-face ((t (:background "gray90" :foreground "gray20"))))
   `(ac-selection-face ((t (:background "#cbe6f3" :foreground "#007ab7" :weight bold))))
   ;; diff-mode
   `(diff-header ((t (:background "gray90" :foreground ,blue))))
   `(diff-file-header ((t (:background "gray90" :weight bold :inherit (diff-header)))))
   `(diff-index ((t (:inherit (diff-file-header)))))
   `(diff-hunk-header ((t (:foreground ,purple :inherit (diff-header)))))
   `(diff-removed ((t (:background "#f6d4d8"))))
   `(diff-added ((t (:background "#eef5d3"))))
   `(diff-changed ((t)))
   `(diff-function ((t (:foreground "#e06800" :background "white" :inherit (diff-header)))))
   `(diff-context ((t (:foreground "gray50"))))
   `(diff-nonexistent ((t (:inherit (diff-file-header)))))
   `(diff-refine-removed ((t (:background "#DA6272"))))
   `(diff-refine-added ((t (:background "#C0D860"))))
   `(diff-refine-change ((t (:background "#C0D860"))))
   ;; Magit
   `(magit-item-highlight ((t (:background nil))))
   ;; Minimap
   `(minimap-active-region-background ((t (:background "gray30"))))
   ;; Ediff
   `(ediff-current-diff-A ((t (:background "#4f3030" :foreground "white"))))
   `(ediff-current-diff-B ((t (:background "#4f3030" :foreground "white"))))
   `(ediff-current-diff-C ((t (:background "#4f3030" :foreground "white"))))
   `(ediff-fine-diff-A ((t (:background "#6f1010" :foreground "red"))))
   `(ediff-fine-diff-B ((t (:background "#6f1010" :foreground "red"))))
   `(ediff-fine-diff-C ((t (:background "#6f1010" :foreground "red"))))
   `(ediff-odd-diff-A ((t (:background "gray20" :foreground "white"))))
   `(ediff-odd-diff-B ((t (:background "gray20" :foreground "white"))))
   `(ediff-odd-diff-C ((t (:background "gray20" :foreground "white"))))
   `(ediff-even-diff-A ((t (:background "gray20" :foreground "white"))))
   `(ediff-even-diff-B ((t (:background "gray20" :foreground "white"))))
   `(ediff-even-diff-C ((t (:background "gray20" :foreground "white"))))
   ;; Helm
   `(helm-source-header ((t (:background "gray90" :foreground "black" :height 1.0))))
   `(helm-header ((t (:background "gray80" :foreground "black"))))
   `(helm-candidate-number ((t (:background unspecified :foreground "gray50"))))
   `(helm-selection ((t (:background "#cbe6f3" :foreground "#007ab7" :weight bold :underline nil))))
   ;; ElScreen
   `(elscreen-tab-background-face ((t (:background "gray80"))))
   `(elscreen-tab-control-face ((t (:background "white" :foreground "black" :underline "gray50"))))
   `(elscreen-tab-current-screen-face ((t (:background "white" :foreground "gray20"))))
   `(elscreen-tab-other-screen-face ((t (:background "gray80" :foreground "gray40"))))))

;; highlight-parentheses
(setq hl-paren-colors '("#ff0000" "#bf4040" "#9f6060" "#8f7070" "#808080"))

(provide-theme 'steady)
