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
                 :family "Menlo"
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
(defun latex-word-count ()
        (interactive)
        (let* ((this-file (buffer-file-name))
                (word-count
                (with-output-to-string
                (with-current-buffer standard-output
                (call-process "texcount" nil t nil "-brief" this-file)))))
        (string-match "\n$" word-count)
        (message (replace-match "" nil nil word-count))))

(map! (:when (modulep! :lang latex) ; custom keymap using local leader
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

(add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode) ; Match background with theme

(setq font-latex-fontify-sectioning 1.3) ; increase section font scaling

(add-hook! 'LaTeX-mode-hook
           #'TeX-fold-mode      ; enable folding of tex commands
        #'auto-fill-mode
        (add-hook 'after-save-hook #'(lambda () (interactive) (TeX-command-run-all nil)) nil t) ; Compile on save
           #'orgtbl-mode)       ; enable embedded org-mode tables

;; Smudge - Spotify
(use-package! smudge
  :init
  :config
        (setq smudge-oauth2-client-secret "a3103c179ddc443da041531f229917d1")
        (setq smudge-oauth2-client-id "51dfc5a70ae4410ea0b691b429d25b9e")
        (setq smudge-player-status-truncate-length 25)
        (setq smudge-transport 'connect)
        (global-smudge-remote-mode))

(map! :leader
      :prefix "k"
      :desc "Spotify"
      :desc "Next track" "n" #'smudge-controller-next-track
      :desc "Previous track" "N" #'smudge-controller-previous-track
      :desc "Find track" "f" #'smudge-track-search
      :desc "My playlists" "m" #'smudge-my-playlists
      :desc "Featured playlists" "p" #'smudge-featured-playlists
      :desc "User playlists" "u" #'smudge-user-playlists
      :desc "Toggle play" "SPC" #'smudge-controller-toggle-play
      :desc "Toggle repeat" "r" #'smudge-controller-toggle-repeat
      :desc "Toggle shuffle" "s" #'smudge-controller-toggle-shuffle
      :desc "Volume up" "+" #'smudge-controller-volume-up
      :desc "Volume down" "-" #'smudge-controller-volume-down
      :desc "Volume mute/unmute" "x" #'smudge-controller-volume-mute-unmute
      :desc "Select device" "d" #'smudge-select-device
      :desc "Quit" "q" #'quit-window)

;; GitHub Copilot
(global-copilot-mode 1)
(global-company-mode 0)
(use-package! copilot
  :bind (("C-<tab>" . 'copilot-accept-completion-by-word)
         ("M-TAB" . 'copilot-clear-overlay)
         ("M-<tab>" . 'copilot-clear-overlay)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

;; Multi-vterm
(use-package multi-vterm
  :config
  (add-hook 'vterm-mode-hook
        (lambda ()
        (setq-local evil-insert-state-cursor 'box)
        (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

;; Org - Ripped from Harry
(use-package! org
  ;; Wrap text at 80 characters for better Git diffs and readability
  :hook (org-mode . auto-fill-mode)
  :config
  ;; Hide ephasis markers that wrap text (i.e. bold, italics)
  (setq org-hide-emphasis-markers t))

;; ;; Provides 'org-modern' configuration in place of Doom's (org +pretty)
;; (use-package! org-modern
;;   :after org
;;   :config
;;   ;; Disable table formatting and star hiding, increase label border
;;   (setq org-modern-table nil
;;         org-modern-hide-stars nil
;;         org-modern-label-border 0.3)
;;   ;; Enable org-modern globally
;;   (global-org-modern-mode))

;; ;; Org-Roam
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

(map! (:when (modulep! :lang org) ; custom keymap using local leader
              (:map org-mode-map
                       :localleader
                       :desc "Open roam UI" "w" #'org-roam-ui-open)))

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
