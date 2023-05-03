;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Matthew Battagel"
      user-mail-address "matthew@battagel.me.uk")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight
      doom-themes-treemacs-theme "doom-colors"
      doom-font (font-spec
                 ;; :family "Fira Code"
                 :size 14))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/OneDrive/Org/")
;; (use-package typescript-mode
;;   :mode (rx ".ts" string-end)
;;   :init
;;   (define-derived-mode typescript-tsx-mode typescript-mode "typescript-tsx")
;;   (add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) #'typescript-tsx-mode)))
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;; Rebind hash key
(map!
 :desc "Override M-3 to insert # rather than change workspace when in insert mode"
 :i "M-3"
 #'(lambda () (interactive) (insert "#")))
(map!
 :desc "Next in search" :n "n" #'(lambda () (interactive) (evil-ex-search-next) (evil-scroll-line-to-center nil))
 :desc "Previous in search" :n "N" #'(lambda () (interactive) (evil-ex-search-previous) (evil-scroll-line-to-center nil)))

;; Latex
;;
(defun latex-word-count ()
        (interactive)
        (let* ((this-file (buffer-file-name))
                (word-count
                (with-output-to-string
                (with-current-buffer standard-output
                (call-process "texcount" nil t nil "-brief" this-file)))))
        (string-match "\n$" word-count)
        (message (replace-match "" nil nil word-count))))

(map! (:when (featurep! :lang latex) ; custom keymap using local leader
              (:map LaTeX-mode-map
                       :localleader
                       :desc "Insert environment" "e" #'LaTeX-environment
                       :desc "Insert section" "s" #'LaTeX-section
                       :desc "Format document" "f" #'LaTeX-fill-buffer
                       :desc "Show all previews" "d" #'preview-document
                       :desc "Clear previews" "D" #'preview-clearout-document
                       :desc "Count words" "w" #'latex-word-count
                       :desc "Fold buffer" "," #'TeX-fold-buffer
                       :desc "Unfold buffer" "." #'TeX-fold-clearout-buffer)))

(add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode) ; Midnight mode always

(setq font-latex-fontify-sectioning 1.3) ; increase section font scaling

(add-hook! 'LaTeX-mode-hook
           #'TeX-fold-mode      ; enable folding of tex commands
        #'auto-fill-mode
        (add-hook 'after-save-hook #'(lambda () (interactive) (TeX-command-run-all nil)) nil t) ; Compile on save
           #'orgtbl-mode)       ; enable embedded org-mode tables

;; GitHub Copilot
(global-copilot-mode 1)
(use-package! copilot
  :bind (("C-<tab>" . 'copilot-accept-completion-by-word)
         ("M-TAB" . 'copilot-clear-overlay)
         ("M-<tab>" . 'copilot-clear-overlay)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

;; Provides configuration for working with 'org-mode' - Ripped from Harry
(use-package! org
  ;; Wrap text at 80 characters for better Git diffs and readability
  :hook (org-mode . auto-fill-mode)
  :config
  ;; Hide emphasis markers that wrap text (i.e. bold, italics)
  (setq org-hide-emphasis-markers t)

  ;; Use 'pdf-tools' as the default viewer for exported Org documents
  (add-to-list 'org-file-apps '("\\.pdf\\'" . pdf-tools))
  ;; Enlarge top and second level heading fonts
  (custom-set-faces!
    '(org-level-1
      :height 1.2
      :inherit outline-1)
    '(org-level-2
      :height 1.1
      :inherit outline-2))
  ;; Enable export support for LaTeX and BibTeX formats
  (require 'ox-latex)
  (require 'ox-bibtex)
  ;; Better syntax highlighting in exported LaTeX
  (setq org-latex-src-block-backend 'minted)
  ;; Enable additional packages for exported LaTeX, takes the form:
  ;;    ("options" "package" SNIPPET-FLAG COMPILERS)
  (setq org-latex-packages-alist '(("" "booktabs")
                                   ("" "tabularx")
                                   ("" "color")
                                   ("newfloat" "minted")))
  ;; Define 'mimore' LaTeX document class for use in exports
  (add-to-list 'org-latex-classes
               '("mimore"
                 "\\documentclass{mimore}\n[NO-DEFAULT-PACKAGES\]\n[PACKAGES\]\n[EXTRA\]"
                 ("\\section{%s}" . "\\section\*{%s}")
                 ("\\subsection{%s}" . "\\subsection\*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection\*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph\*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph\*{%s}"))))


;; Provides 'org-modern' configuration in place of Doom's (org +pretty)
(use-package! org-modern
  :after org
  :config
  ;; Disable table formatting and star hiding, increase label border
  (setq org-modern-table nil
        org-modern-hide-stars nil
        org-modern-label-border 0.3)
  ;; Enable org-modern globally
  (global-org-modern-mode))

;; Provides support for presenting directly from 'org-mode' buffers
(use-package! org-tree-slide
  :after org
  :config
  ;; Hide formatting characters, use top-level headings as slides
  (setq org-tree-slide-skip-outline-level 2)
  ;; Use the fancy presentation profile, shiny animations!
  (org-tree-slide-presentation-profile))

;; Provides configuration for 'org-roam', an Emacs knowledge graph
(use-package! org-roam
  :after org
  :config
  ;; Hide common link types from org-roam graph
  (setq org-roam-graph-link-hidden-types
        '("file"
          "http"
          "https")))

;; Provides 'websocket', a dependency of 'org-roam-ui'
(use-package! websocket
  :after org-roam)

;; Provides 'org-roam-ui' a web frontend for 'org-roam'
(use-package! org-roam-ui
  :after org-roam
  :config
  ;; Sync UI theme with Emacs, follow current the buffer, update on save, and
  ;; open browser on start
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
