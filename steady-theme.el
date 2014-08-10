;;; steady-theme.el --- A steady theme for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2013  Yuta Taniguchi

;; Author: Yuta Taniguchi <yuta.taniguchi.y.t@gmail.com>
;; Keywords: theme
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(deftheme steady
  "A steady theme for Emacs")

(let ((bg  "#ffffff")
      (fg  "#272727")
      (shadow "gray70")
      (fg-red    "#c7243a") (fg-red-256 "#d70000")
      (fg-yellow "#d59b0a")
      (fg-green  "#93B11D")
      (fg-blue   "#006ea5")
      (fg-purple "#744199") (fg-purple-256 "#8700d7")
      (bg-yellow "#f9db57")
      (diff-removed "#f6d4d8")
      (diff-added   "#eef5d3"))
  ;; Set attributes of the default face for existing frames and new frames.
  (set-face-attribute 'default nil :background bg :foreground fg)
  (set-face-attribute 'cursor  nil :background fg-red)
  ;; Custom theme
  (custom-theme-set-faces
   'steady
   ;; Standard
   `(default        ((t :background ,bg :foreground ,fg)))
   `(fixed-pitch    ((t :family "monospace")))
   `(variable-pitch ((t :family "sans serif")))
   `(shadow         ((t :foreground ,shadow)))
   ;; Frame
   `(cursor             ((((class color) (min-colors 257)) :background ,fg-red)
                         (t                                :background ,fg-red-256)))
   `(fringe             ((((class color) (min-colors 257)) :background "#e3e0c9" :foreground "#737266")
                         (t                                :background "#dfdfaf" :foreground "#87875f")))
   `(menu               ((t :background "gray50"  :foreground "#ffffff" :inverse-video nil)))
   `(linum              ((t :inherit (fringe))))
   `(mode-line          ((t :background "gray80"  :foreground "gray10" :box nil :weight bold)))
   `(mode-line-inactive ((t :background "gray20"  :foreground "gray50" :box nil)))
   `(minibuffer-prompt  ((t :foreground ,fg)))
   ;; powerline
   `(powerline-mode-normal        ((t :background "#afd700" :foreground "#005f00" :box nil :weight bold)))
   `(powerline-mode-insert        ((t :background "#ffffff" :foreground "#005f5f" :box nil :weight bold)))
   `(powerline-mode-visual        ((t :background "#ffaf00" :foreground "#875f00" :box nil :weight bold)))
   `(powerline-mode-replace       ((t :background "#d70000" :foreground "#ffffff" :box nil :weight bold)))
   `(powerline-mode-operator      ((t :background "#d70000" :foreground "#ffffff" :box nil :weight bold)))
   `(powerline-mode-emacs         ((t :background "gray60"  :foreground "black"   :box nil :weight bold)))
   `(powerline-mode-inactive      ((t :background "gray60"  :foreground "gray30"  :box nil)))
   `(powerline-first-normal       ((t :background "gray80"  :foreground "gray30")))
   `(powerline-first-insert       ((t :background "#87d7ff" :foreground "#005f87")))
   `(powerline-first-inactive     ((t :background "gray60"  :foreground "gray30")))
   `(powerline-second-normal      ((t :background "gray60"  :foreground "#ffffff")))
   `(powerline-second-insert      ((t :background "#0087af" :foreground "#87d7ff")))
   `(powerline-second-inactive    ((t :background "gray40"  :foreground "gray70")))
   `(powerline-third-normal       ((t :background "gray40"  :foreground "gray70")))
   `(powerline-third-insert       ((t :background "#005f87" :foreground "#87d7ff")))
   `(powerline-third-inactive     ((t :background "gray20"  :foreground "gray40")))
   `(powerline-buffer-id-normal   ((t :inherit (powerline-second-normal))))
   `(powerline-buffer-id-insert   ((t :inherit (powerline-second-insert))))
   `(powerline-buffer-id-inactive ((t :inherit (powerline-second-inactive))))
   ;; Highlight
   `(highlight           ((t :background "gray75")))
   `(isearch             ((t :background ,bg-yellow :foreground ,fg :box (:line-width 1 :color ,fg-yellow :style nil))))
   `(query-replace       ((t :inherit (isearch))))
   `(lazy-highlight      ((t :background nil :inherit (isearch))))
   `(region              ((default :foreground unspecified)
                          (((class color) (min-colors 257)) :background "#fbe9e2")
                          (t                                :background "#ffdfdf")))
   `(secondary-selection ((t :background "#272822")))
   `(trailing-whitespace ((((class color)) :background "#ff0000") (t :inverse-video t)))
   `(escape-glyph        ((t :foreground "#5599ff")))
   ;; whitespace
   `(whitespace-line ((t :background "#ffff00" :foreground ,fg)))
   ;; Main
   `(font-lock-builtin-face              ((((class color) (min-colors 257)) :foreground ,fg-red)
                                          (t                                :foreground ,fg-red-256)))
   `(font-lock-comment-delimiter-face    ((t :inherit (font-lock-comment-face))))
   `(font-lock-comment-face              ((t :foreground unspecified :inherit (shadow))))
   `(font-lock-constant-face             ((((class color) (min-colors 257)) :foreground ,fg-purple)
                                          (t                                :foreground ,fg-purple-256)))
   `(font-lock-doc-face                  ((t :slant italic :inherit (shadow))))
   `(font-lock-function-name-face        ((t :foreground ,fg-green :slant italic)))
   `(font-lock-keyword-face              ((((class color) (min-colors 257)) :foreground ,fg-red)
                                          (t                                :foreground ,fg-red-256)))
   `(font-lock-negation-char-face        ((((class color) (min-colors 257)) :foreground ,fg-red)
                                          (t                                :foreground ,fg-red-256)))
   `(font-lock-preprocessor-face         ((t :foreground ,fg-red)))
   `(font-lock-regexp-grouping-backslash ((t :weight bold)))
   `(font-lock-regexp-grouping-construct ((t :weight bold)))
   `(font-lock-string-face               ((((class color) (min-colors 257)) :foreground ,fg-purple)
                                          (t                                :foreground ,fg-purple-256)))
   `(font-lock-type-face                 ((t :foreground ,fg-blue)))
   `(font-lock-variable-name-face        ((t :foreground ,fg-yellow)))
   `(font-lock-warning-face              ((t :background "#ffff00" :foreground ,fg)))
   ;; completions
   `(completions-common-part      ((t :foreground "gray50")))
   `(completions-first-difference ((t :foreground ,fg-red)))
   ;; show-paren-mode
   `(show-paren-match-face ((t :background nil :foreground "#ff0000")))
   ;; auto-complete
   `(ac-completion-face ((t :underline unspecified :inherit (shadow))))
   `(ac-candidate-face  ((t :background "gray90"  :foreground "gray20")))
   `(ac-selection-face  ((t :background "#cbe6f3" :foreground "#007ab7" :weight bold)))
   ;; diff-mode
   `(diff-header         ((t :background "gray85" :foreground ,fg-blue)))
   `(diff-file-header    ((t :background "gray85" :weight bold :inherit (diff-header))))
   `(diff-index          ((t :inherit (diff-file-header))))
   `(diff-hunk-header    ((default :inherit (diff-header))
                          (((class color) (min-colors 257)) :foreground ,fg-purple)
                          (t                                :foreground ,fg-purple-256)))
   `(diff-removed        ((t :background ,diff-removed)))
   `(diff-added          ((t :background ,diff-added)))
   `(diff-changed        ((t)))
   `(diff-function       ((t :foreground "#e06800" :background ,bg :inherit (diff-header))))
   `(diff-context        ((t :foreground "gray50")))
   `(diff-nonexistent    ((t :inherit (diff-file-header))))
   `(diff-refine-removed ((t :background "#DA6272")))
   `(diff-refine-added   ((t :background "#C0D860")))
   `(diff-refine-change  ((t :inherit (diff-refine-added))))
   ;; Magit
   `(magit-item-highlight ((t :background "gray95" :weight bold)))
   ;; Minimap
   `(minimap-active-region-background ((t :background "gray30")))
   ;; Ediff
   `(ediff-current-diff-A ((t :inherit (diff-removed) :background unspecified :foreground unspecified :weight bold)))
   `(ediff-current-diff-B ((t :inherit (diff-added)   :background unspecified :foreground unspecified :weight bold)))
   `(ediff-current-diff-C ((t                         :background "#cbe6f3"   :foreground unspecified :weight bold)))
   `(ediff-odd-diff-A     ((t :inherit (diff-removed) :background unspecified :foreground unspecified)))
   `(ediff-odd-diff-B     ((t :inherit (diff-added)   :background unspecified :foreground unspecified)))
   `(ediff-odd-diff-C     ((t                         :background "#cbe6f3"   :foreground unspecified)))
   `(ediff-even-diff-A    ((t :inherit (diff-removed) :background unspecified :foreground unspecified)))
   `(ediff-even-diff-B    ((t :inherit (diff-added)   :background unspecified :foreground unspecified)))
   `(ediff-even-diff-C    ((t                         :background "#cbe6f3"   :foreground unspecified)))
   `(ediff-fine-diff-A    ((t :inherit (diff-refine-removed)                  :foreground unspecified)))
   `(ediff-fine-diff-B    ((t :inherit (diff-refine-added)                    :foreground unspecified)))
   `(ediff-fine-diff-C    ((t                         :background "#cbe6f3"   :foreground unspecified)))
   ;; Helm
   `(helm-source-header    ((t :background "gray90"    :foreground ,fg :height 1.0)))
   `(helm-header           ((t :background "gray80"    :foreground ,fg)))
   `(helm-candidate-number ((t :background unspecified :foreground "gray50")))
   `(helm-selection        ((t :background "#cbe6f3"   :foreground "#007ab7" :weight bold :underline nil)))
   ;; ElScreen
   `(elscreen-tab-background-face     ((t :background "gray80")))
   `(elscreen-tab-control-face        ((t :background "#ffffff" :foreground ,fg :underline "gray50")))
   `(elscreen-tab-current-screen-face ((t :background "#ffffff" :foreground "gray20")))
   `(elscreen-tab-other-screen-face   ((t :background "gray80"  :foreground "gray40"))))
  ;; highlight-parentheses
  (setq hl-paren-colors '("#ff0000" "#bf4040" "#9f6060" "#8f7070" "#808080")))

(provide-theme 'steady)

;;; steady-theme.el ends here
