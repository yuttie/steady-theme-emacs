;;; steady-dark-theme.el --- A steady dark theme for Emacs  -*- lexical-binding: t -*-

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

(deftheme steady-dark
  "A steady dark theme for Emacs")

(let ((bg  "#272727")
      (fg  "#c0c0c0")
      (shadow    "gray40")
      (fg-red    "#ed5c5c") (fg-red-256    "#ff5f5f")  (bg-red    "#ff9594") (bg-red-256    "#ff8787")
      (fg-yellow "#e2a206") (fg-yellow-256 "#dfaf00")  (bg-yellow "#f5df60") (bg-yellow-256 "#ffdf5f")
      (fg-green  "#88a718") (fg-green-256  "#87af00")  (bg-green  "#d6f660") (bg-green-256  "#dfff5f")
      (fg-blue   "#0697e1") (fg-blue-256   "#00afff")  (bg-blue   "#80d5ff") (bg-blue-256   "#87d7ff")
      (fg-purple "#aa62f8") (fg-purple-256 "#af5fff")  (bg-purple "#e89bff") (bg-purple-256 "#d787ff")
      (yellow-light  "#f7e189")
      (yellow-medium "#efc310")
      (yellow-dark   "#b4930c")
      (diff-removed "#6b151e")
      (diff-added   "#434d13"))
  ;; Set attributes of the default face for existing frames and new frames.
  (set-face-attribute 'default nil :background bg :foreground fg)
  (set-face-attribute 'cursor  nil :background bg-blue)
  ;; Custom theme
  (custom-theme-set-faces
   'steady-dark
   ;; Standard
   `(default        ((t :background ,bg :foreground ,fg)))
   `(fixed-pitch    ((t :family "monospace")))
   `(variable-pitch ((t :family "sans serif")))
   `(shadow         ((t :foreground ,shadow)))
   ;; Frame
   `(cursor             ((((class color) (min-colors 257)) :background ,bg-blue)
                         (t                                :background ,bg-blue-256)))
   `(fringe             ((((class color) (min-colors 257)) :background "gray18" :foreground "gray40" :weight bold)
                         (t                                :background "gray18" :foreground "gray40" :weight bold)))
   `(menu               ((t :background "gray50"  :foreground "#ffffff" :inverse-video nil)))
   `(linum              ((t :inherit (fringe))))
   `(mode-line          ((t :background "gray45"  :foreground "gray90" :box nil)))
   `(mode-line-inactive ((t :background "gray40"  :foreground "gray55" :box nil)))
   `(minibuffer-prompt  ((t :foreground ,fg)))
   ;; hlinum
   `(linum-highlight-face ((t :inherit (linum) :foreground "#41c0ff")))
   ;; powerline
   `(powerline-mode-normal        ((t :background "#afd700" :foreground "#005f00" :box nil :weight bold)))
   `(powerline-mode-insert        ((t :background "#ffffff" :foreground "#005f5f" :box nil :weight bold)))
   `(powerline-mode-visual        ((t :background "#ffaf00" :foreground "#875f00" :box nil :weight bold)))
   `(powerline-mode-replace       ((t :background "#d70000" :foreground "#ffffff" :box nil :weight bold)))
   `(powerline-mode-operator      ((t :background "#d70000" :foreground "#ffffff" :box nil :weight bold)))
   `(powerline-mode-emacs         ((t :background "gray60"  :foreground "black"   :box nil :weight bold)))
   `(powerline-mode-inactive      ((t :background "gray40"  :foreground "gray55"  :box nil)))
   `(powerline-first-normal       ((t :background "gray45"  :foreground "gray90")))
   `(powerline-first-insert       ((t :background "#87d7ff" :foreground "#005f87")))
   `(powerline-first-inactive     ((t :background "gray40"  :foreground "gray55")))
   `(powerline-second-normal      ((t :background "gray35"  :foreground "gray80")))
   `(powerline-second-insert      ((t :background "#0087af" :foreground "#87d7ff")))
   `(powerline-second-inactive    ((t :background "gray30"  :foreground "gray45")))
   `(powerline-third-normal       ((t :background "gray25"  :foreground "gray70")))
   `(powerline-third-insert       ((t :background "#005f87" :foreground "#87d7ff")))
   `(powerline-third-inactive     ((t :background "gray20"  :foreground "gray35")))
   `(powerline-buffer-id-normal   ((t :inherit (powerline-second-normal))))
   `(powerline-buffer-id-insert   ((t :inherit (powerline-second-insert))))
   `(powerline-buffer-id-inactive ((t :inherit (powerline-second-inactive))))
   ;; Highlight
   `(highlight           ((t :background "#24343c")))
   `(isearch             ((t :background ,bg-yellow :foreground "black" :box (:line-width -1 :color ,fg-yellow :style nil))))
   `(query-replace       ((t :inherit (isearch))))
   `(lazy-highlight      ((t :background nil :inherit (isearch))))
   `(region              ((((class color) (min-colors 257)) :background ,fg-yellow     :foreground ,bg-yellow)
                          (t                                :background ,fg-yellow-256 :foreground ,bg-yellow-256)))
   `(secondary-selection ((t :background "#272822")))
   `(trailing-whitespace ((((class color)) :background "#ff0000") (t :inverse-video t)))
   `(escape-glyph        ((t :foreground "#5599ff")))
   ;; whitespace
   `(whitespace-line ((t :background "#ffff00" :foreground "black")))
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
   `(font-lock-warning-face              ((t :background "#ffff00" :foreground "#000000")))
   ;; completions
   `(completions-common-part      ((t :foreground "gray50")))
   `(completions-first-difference ((t :foreground ,fg-red)))
   ;; show-paren-mode
   `(show-paren-match-face ((t :background nil :foreground "#ff0000")))
   ;; auto-complete
   `(ac-completion-face ((t :inherit (shadow) :underline unspecified)))
   `(ac-candidate-face  ((t :inherit(default) :background "gray25")))
   `(ac-selection-face  ((t :inherit (ac-candidate-face) :foreground "#41c0ff" :weight bold)))
   ;; company-mode
   `(company-tooltip                  ((t :inherit (default) :background "gray25")))
   `(company-tooltip-selection        ((t :inherit (company-tooltip) :background "#3c5664" :weight bold)))
   `(company-tooltip-search           ((t :background ,bg-yellow :foreground "black" :box (:line-width -1 :color ,fg-yellow :style nil))))
   `(company-tooltip-mouse            ((t :inherit (company-tooltip-selection))))
   `(company-tooltip-common           ((t :inherit (company-tooltip) :foreground ,yellow-medium :weight bold)))
   `(company-tooltip-common-selection ((t :inherit (company-tooltip-selection) :foreground ,yellow-medium)))
   `(company-tooltip-annotation       ((t :inherit (company-tooltip) :foreground "#c83737")))
   `(company-scrollbar-fg             ((t :background "gray40")))
   `(company-scrollbar-bg             ((t :inherit (company-tooltip) :foreground "gray50")))
   `(company-preview                  ((t :foreground "#ffffff")))
   `(company-preview-common           ((t :foreground ,yellow-medium)))
   `(company-preview-search           ((t :background ,bg-yellow :foreground "black" :box (:line-width -1 :color ,fg-yellow :style nil))))
   `(company-echo                     nil)
   `(company-echo-common              ((t :inherit (company-echo) :foreground ,yellow-medium :weight bold)))
   ;; diff-mode
   `(diff-header         ((t :background "gray25" :foreground ,fg-blue)))
   `(diff-file-header    ((t :background "gray30" :weight bold :inherit (diff-header))))
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
   `(diff-refine-removed ((t :background "#9b2534")))
   `(diff-refine-added   ((t :background "#62751a")))
   `(diff-refine-change  ((t :inherit (diff-refine-added))))
   ;; Magit
   `(magit-item-highlight ((t :inherit (highlight))))
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
   `(helm-source-header    ((t :background "gray30"    :foreground ,fg :height 1.0)))
   `(helm-header           ((t :background "gray40"    :foreground ,fg)))
   `(helm-candidate-number ((t :background unspecified :foreground "gray50")))
   `(helm-selection        ((t :background "#133f55"   :foreground "#41c0ff" :weight bold :underline nil)))
   `(helm-match            ((t :foreground ,fg-yellow :weight bold)))
   ;; ElScreen
   `(elscreen-tab-background-face     ((((class color) (min-colors 257)) :background "#404040" :foreground "#686868" :underline nil :height 1.3)
                                       (t                                :background "#444444" :foreground "#6c6c6c" :underline nil :height 1.3)))
   `(elscreen-tab-control-face        ((t :background "#ffffff" :foreground ,fg :underline nil)))
   `(elscreen-tab-current-screen-face ((t :background "gray35"  :foreground "#41c0ff" :weight bold :underline nil)))
   `(elscreen-tab-other-screen-face   ((((class color) (min-colors 257)) :inherit (elscreen-tab-background-face) :foreground "gray50" :height 0.77)
                                       (t                                :inherit (elscreen-tab-background-face) :foreground "gray51" :height 0.77)))
   ;; Shell
   `(comint-highlight-prompt ((t :foreground ,bg-red   :weight bold)))
   `(comint-highlight-input  ((t :foreground "#ffffff" :weight bold)))
   ;; DDSKK
   `(skk-henkan-face ((t :inherit (default) :foreground "#41c0ff")))
   `(skk-jisyo-registration-badge-face ((t :background "#41c0ff" :foreground "#133f55")))
   `(skk-inline-show-face ((t :inherit (default))))
   `(skk-verbose-intention-face ((t :inherit (shadow))))
   `(skk-dcomp-face ((t :inherit (shadow))))
   `(skk-dcomp-multiple-face  ((t :inherit(default) :background "gray25")))
   `(skk-dcomp-multiple-trailing-face  ((t :inherit (skk-dcomp-multiple-face) :foreground "#ffffff" :weight bold)))
   `(skk-dcomp-multiple-selected-face ((t :inherit (skk-dcomp-multiple-face) :foreground "#41c0ff" :weight bold)))
   )
  ;; highlight-parentheses
  (setq hl-paren-colors '("#ff0000" "#bf4040" "#9f6060" "#8f7070" "#808080")))
  ;; DDSKK
  (defface skk-henkan-face nil "*変換候補の face 属性。")
  (setq skk-henkan-face 'skk-henkan-face)
  (defface skk-inline-show-face nil "")
  (setq skk-inline-show-face 'skk-inline-show-face)
  (setq skk-inline-show-background-color "gray25")
  (setq skk-inline-show-background-color-odd "gray30")

(provide-theme 'steady-dark)

;; Local Variables:
;; eval: (add-to-list 'custom-theme-load-path (file-name-directory (buffer-file-name)))
;; End:

;;; steady-dark-theme.el ends here
