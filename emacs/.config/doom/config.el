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
                 :size 12))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Google Drive/My Drive/Org/")
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
;; General
(setq which-key-idle-delay 0.2)
(setq tab-width 4)
(evil-set-undo-system 'undo-tree)
(setq org-hide-block-startup t)
(setq display-line-numbers t)
(defun my-fill-column-hook ()
  (setq fill-column 132))
(add-hook 'c-mode-hook 'my-fill-column-hook)

;; If running on a Mac, automatically fullscreen on launch
(if IS-MAC (add-to-list 'default-frame-alist'(fullscreen . fullboth)))

;; Workspace keybinds
(map! :map doom-leader-workspace-map
      :desc "Swap Left" "h" #'+workspace/swap-left
      :desc "Swap Right" "l" #'+workspace/swap-right)

(map! :after lsp-ui-peek
      :map c-mode-map
      :localleader
      :desc "Callers List" "c" #'cscope-find-functions-calling-this-function
      :desc "Callees List" "C" #'cscope-find-called-functions
      :desc "Index Files" "i" #'cscope-index-files
      :desc "Find Symbol" "s" #'cscope-find-this-symbol
      :desc "Find Text" "t" #'cscope-find-this-text-string
      :desc "Find File" "f" #'cscope-find-this-file
      :desc "Find References" "D" #'cscope-find-assignments-to-this-symbol
      :desc "Find Definition" "d" #'cscope-find-global-definition)
;; Provides configuration for working with 'eshell', a shell within Emacs
(use-package! eshell
  :defer
  :config
  ;; Sets the $TERM environment variable to indicate colour support
  (setq eshell-term-name "xterm-256-color"))

(use-package! projectile
  :defer
  :config
  ;; When looking for Go project files, I don't care about vendored dependencies
  (add-to-list 'projectile-globally-ignored-directories "*vendor"))

;; GitHub Copilot
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config (setq copilot-max-char 1000000))

;; Provides better Emacs support for Python docstrings
;; (use-package! python-docstring-mode
;;   :hook python-mode)

;; Rebind hash key
(map!
 :desc "Override M-3 to insert # rather than change workspace when in insert mode"
 :i "M-3"
 #'(lambda () (interactive) (insert "#")))

;; Vim Commands
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

;; ;; Smudge - Spotify
;; (use-package! smudge
;;   :init
;;   :config
;;         (setq smudge-oauth2-client-secret "a3103c179ddc443da041531f229917d1")
;;         (setq smudge-oauth2-client-id "51dfc5a70ae4410ea0b691b429d25b9e")
;;         (setq smudge-player-status-truncate-length 25)
;;         (setq smudge-transport 'connect)
;;         (global-smudge-remote-mode))

;; (map! :leader
;;       :prefix "k"
;;       :desc "Spotify"
;;       :desc "Next track" "n" #'smudge-controller-next-track
;;       :desc "Previous track" "N" #'smudge-controller-previous-track
;;       :desc "Find track" "f" #'smudge-track-search
;;       :desc "My playlists" "m" #'smudge-my-playlists
;;       :desc "Featured playlists" "p" #'smudge-featured-playlists
;;       :desc "User playlists" "u" #'smudge-user-playlists
;;       :desc "Toggle play" "SPC" #'smudge-controller-toggle-play
;;       :desc "Toggle repeat" "r" #'smudge-controller-toggle-repeat
;;       :desc "Toggle shuffle" "s" #'smudge-controller-toggle-shuffle
;;       :desc "Volume up" "+" #'smudge-controller-volume-up
;;       :desc "Volume down" "-" #'smudge-controller-volume-down
;;       :desc "Volume mute/unmute" "x" #'smudge-controller-volume-mute-unmute
;;       :desc "Select device" "d" #'smudge-select-device
;;       :desc "Quit" "q" #'quit-window)

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

;; TRAMP settings
(use-package! tramp
  :defer
  :config
  ;; Use sshx by default for speed
  ;; Don't use /bin/sh as the default shell
  (let ((+tramp-shell "/bin/zsh"))
    (setq tramp-default-remote-shell +tramp-shell)
    (setq vterm-tramp-shells `(("ssh" ,+tramp-shell)
                               ("sshx" ,+tramp-shell)
                               ("docker" ,+tramp-shell)))))
(defun remote-vdt ()
  "Start an SSH session to the remote server 'VDT'."
  (interactive)
  (let* ((vterm-buffer-name (format "*VDT-%s*" (format-time-string "%H%M%S")))
         (default-directory "/sshx:vdt:"))
    (vterm)
    (vterm-send-string "zsh\n clear\n")))

(defun remote-vdt-tmux ()
  "Start an SSH-TMUX session to the remote server 'VDT'."
  (interactive)
  (let* ((vterm-buffer-name (format "*VDT-TMUX-%s*" (format-time-string "%H%M%S"))))
    (vterm)
    (rename-buffer vterm-buffer-name)
    (vterm-send-string "ssh vdt\n")
    (vterm-send-string "tmux a -d\n")))

(defun remote-cxo ()
  "Start an SSH session to the remote server 'CXO'."
  (interactive)
  (let* ((vterm-buffer-name (format "*CXO-%s*" (format-time-string "%H%M%S")))
         (default-directory "/sshx:cxo:"))
    (vterm)
    (vterm-send-string "zsh\n clear\n")))

(defun remote-cxo-tmux ()
  "Start an SSH-TMUX session to the remote server 'CXO'."
  (interactive)
  (let* ((vterm-buffer-name (format "*VDT-TMUX-%s*" (format-time-string "%H%M%S"))))
    (vterm)
    (rename-buffer vterm-buffer-name)
    (vterm-send-string "ssh cxo\n")
    (vterm-send-string "tmux a -d\n")))

(defun remote-bsrv ()
  "Start an SSH session to the remote server 'Build Server'."
  (interactive)
  (let* ((vterm-buffer-name (format "*Build-Server-%s*" (format-time-string "%H%M%S")))
         (default-directory "/sshx:bsrv:"))
    (vterm)
    (rename-buffer vterm-buffer-name)
    (vterm-send-string "bash\n source .profile\n clear\n")))

(defun remote-bsrv-tmux ()
  "Start an SSH-TMUX session to the remote server 'Build Server'."
  (interactive)
  (let* ((vterm-buffer-name (format "*Build-Server-TMUX-%s*" (format-time-string "%H%M%S"))))
    (vterm)
    (rename-buffer vterm-buffer-name)
    (vterm-send-string "ssh bsrv\n")
    (vterm-send-string "tmux a -d\n")))

(defun remote-jh ()
  "Start an SSH session to the remote server 'Jumphost'."
  (interactive)
  (let* ((vterm-buffer-name (format "*JH-%s*" (format-time-string "%H%M%S")))
         (default-directory "/sshx:jh:"))
    (vterm)))

(defun remote-jh-tmux ()
  "Start an SSH-TMUX session to the remote server 'Jumphost'."
  (interactive)
  (let* ((vterm-buffer-name (format "*JH-TMUX-%s*" (format-time-string "%H%M%S"))))
    (vterm)
    (rename-buffer vterm-buffer-name)
    (vterm-send-string "ssh jh\n")
    (vterm-send-string "tmux a -d\n")))

(map! :leader
      :prefix "k"
      :desc "Remote Terminal Manager"
      :desc "VDT" "v" #'remote-vdt
      :desc "VDT tmux" "V" #'remote-vdt-tmux
      :desc "CXO" "c" #'remote-cxo
      :desc "CXO tmux" "C" #'remote-cxo-tmux
      :desc "CXO Jump Host" "j" #'remote-jh
      :desc "CXO Jump Host tmux" "J" #'remote-jh-tmux
      :desc "Build Server" "b" #'remote-bsrv
      :desc "Build Server tmux" "B" #'remote-bsrv-tmux)

;; C
;; Block auto formatting
(after! format
  (appendq! +format-on-save-disabled-modes '(c-mode)))
;; Ignore directories
(after! lsp-mode
  (setq lsp-file-watch-ignored-directories
        (append lsp-file-watch-ignored-directories
                '("[/\\\\]build_cache\\'"
                  "[/\\\\]container\\'"
                  "[/\\\\]repotools\\'"
                  "[/\\\\]flavors\\'"
                  "[/\\\\]tpdgatetools\\'"))))
;; Cscope
(use-package! xcscope
  :config
  (cscope-setup)
  (setq cscope-initial-directory "~/repos/proj_swiss_npi"))

(map! :after lsp-ui-peek
      :map c-mode-map
      :localleader
      :desc "Callers List" "c" #'cscope-find-functions-calling-this-function
      :desc "Callees List" "C" #'cscope-find-called-functions
      :desc "Index Files" "i" #'cscope-index-files
      :desc "Find Symbol" "s" #'cscope-find-this-symbol
      :desc "Find Text" "t" #'cscope-find-this-text-string
      :desc "Find File" "f" #'cscope-find-this-file
      :desc "Find References" "D" #'cscope-find-assignments-to-this-symbol
      :desc "Find Definition" "d" #'cscope-find-global-definition)
;;clangd
(setq lsp-clangd-binary-path "/opt/homebrew/opt/llvm/bin/clangd")
(setq lsp-clients-clangd-args '("-j=3"
				"--background-index"
				"--clang-tidy"
				"--completion-style=detailed"
				"--header-insertion=never"
				"--header-insertion-decorators=0"))

;; ;; TMUX
;; (use-package! tmux-pane
;;   :config
;;   (tmux-pane-mode)
;;   (map! :leader
;;         (:prefix ("v" . "tmux pane")
;;           :desc "Open vpane" :nv "o" #'tmux-pane-open-vertical
;;           :desc "Open hpane" :nv "h" #'tmux-pane-open-horizontal
;;           :desc "Open hpane" :nv "s" #'tmux-pane-open-horizontal
;;           :desc "Open vpane" :nv "v" #'tmux-pane-open-vertical
;;           :desc "Close pane" :nv "c" #'tmux-pane-close
;;           :desc "Rerun last command" :nv "r" #'tmux-pane-rerun))
;;   (map! :leader
;;         (:prefix "t"
;;           :desc "vpane" :nv "v" #'tmux-pane-toggle-vertical
;;           :desc "hpane" :nv "h" #'tmux-pane-toggle-horizontal)))

;; Org - Ripped from Harry
(use-package! org
  ;; Wrap text at 80 characters for better Git diffs and readability
  :hook (org-mode . auto-fill-mode)
  :config
  ;; Hide ephasis markers that wrap text (i.e. bold, italics)
  (setq org-hide-emphasis-markers t)
  (setq org-tree-slide-heading-emphasis t))

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

;; Org-Roam
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
