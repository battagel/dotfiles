;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Doom
(setq user-full-name "Matthew Battagel"
      user-mail-address "matthew@battagel.me.uk")
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight
      doom-themes-treemacs-theme "doom-colors"
      doom-font (font-spec
                 :family "Menlo"
                 :size 12))
(setq display-line-numbers-type t)
(setq org-directory "~/Google-Drive/Org/")

;; General
(setq which-key-idle-delay 0.2)
(setq tab-width 4)
(setq custom-line-length 132) ;; Custom variable for line length
(setq flycheck-flake8-maximum-line-length custom-line-length)
(defun my-fill-column-hook ()
  (setq fill-column 80)) ;; 80 is small for small buffers
(add-hook 'c-mode-hook 'my-fill-column-hook)
(evil-set-undo-system 'undo-tree)
(setq display-line-numbers t)
(setq company-ispell-available nil) ;; This really slows down auto complete

;; If running on a Mac, automatically fullscreen on launch
(if (eq system-type 'darwin) (add-to-list 'default-frame-alist '(fullscreen . fullboth)))

;; Workspace keybinds
(map! :map doom-leader-workspace-map
      :desc "Swap Left" "h" #'+workspace/swap-left
      :desc "Swap Right" "l" #'+workspace/swap-right)

;; DragStuff
(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;; Dired
(defun kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (mapc 'kill-buffer (delq nil (mapcar #'get-buffer (delq nil (mapcar #'buffer-name (buffer-list)))))))

(defun dired-kill-all-buffers-on-quit ()
  "Override `q` in dired to kill all dired buffers."
  (define-key dired-mode-map (kbd "q") 'kill-all-dired-buffers))

(add-hook 'dired-mode-hook 'dired-kill-all-buffers-on-quit)

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

;; GitHub Copilot Configuration
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-<tab>" . copilot-accept-completion)
              ;; ("<tab>" . copilot-accept-completion-by-word)
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion))
  :config
  (setq copilot-max-char 1000000)
  (setq copilot-idle-delay 0.5))

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

;; Vterm
(map! :after vterm
      :map vterm-mode-map
      :ni "C-c" #'vterm--self-insert)


(defun remote-vdt ()
  "Start an SSH session to the remote server 'VDT'."
  (interactive)
  (let* ((vterm-buffer-name (format "*VDT-%s*" (format-time-string "%H%M%S")))
         (default-directory "/sshx:vdt:"))
    (vterm)
    (rename-buffer vterm-buffer-name)
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
    (rename-buffer vterm-buffer-name)
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
  (let* ((new-vterm-buffer-name (format "*Build-Server-TMUX-%s*" (format-time-string "%H%M%S"))))
    (+vterm/here nil)
    (rename-buffer new-vterm-buffer-name)
    (vterm-send-string "ssh bsrv\n")
    (vterm-send-string "tmux a -d\n")))

(map! :leader
      :prefix "k"
      :desc "Remote Terminal Manager"
      :desc "VDT" "v" #'remote-vdt
      :desc "VDT tmux" "V" #'remote-vdt-tmux
      :desc "CXO" "c" #'remote-cxo
      :desc "CXO tmux" "C" #'remote-cxo-tmux
      :desc "Build Server" "b" #'remote-bsrv
      :desc "Build Server tmux" "B" #'remote-bsrv-tmux)

;; Python
;; Disable eglot check and enable flake8 and pylint for python linting
(after! flycheck
  (add-hook 'python-mode-hook
        (lambda ()
        (setq-local flycheck-disabled-checkers '(eglot))
        (setq-local flycheck-select-checker 'python-flake8)
        (flycheck-add-next-checker 'python-flake8 'python-pylint)))

;; C
;; Block auto formatting
(after! format
  (appendq! +format-on-save-disabled-modes '(c-mode)))
;; Ignore directories
(after! lsp-mode
  (setq lsp-file-watch-ignored-directories
        (append lsp-file-watch-ignored-directories
                '("[/\\\\]build_cache\\'"
                  "[/\\\\]repotools\\'"
                  "[/\\\\]flavors\\'"
                  "[/\\\\]tpdgatetools\\'"))))
;; ;;clangd
(setq lsp-clangd-binary-path "/opt/homebrew/opt/llvm/bin/clangd")
(setq lsp-clients-clangd-args '("-j=3"
				"--background-index"
				"--clang-tidy"
				"--completion-style=detailed"
				"--header-insertion=never"
				"--header-insertion-decorators=0"))

;; Org Mode Configuration
(use-package org
  :hook (org-mode . auto-fill-mode) ; Wrap text at fill-column
  :config
  (setq org-hide-emphasis-markers t          ; Hide emphasis markers (*bold*, /italic/)
        org-tree-slide-heading-emphasis t)) ; Highlight slide headings

;; Org Modern Configuration
(use-package! org-modern
  :after org
  :config
  (setq org-modern-table nil                 ; Disable table formatting
        org-modern-hide-stars nil            ; Show org stars
        org-modern-star '("◉" "○" "●" "◆" "◇" "•")) ; Optional: custom bullets
  (global-org-modern-mode))

;; Org Tree Slide (Presentation Mode)
(use-package! org-tree-slide
  :after org
  :config
  (setq org-tree-slide-skip-outline-level 2  ; Use top-level headings as slides
        org-tree-slide-presentation-profile t)) ; Enable fancy presentation

;; Org Roam Configuration
(use-package! org-roam
  :after org
  :config
  (setq org-roam-graph-link-hidden-types '("file" "http" "https"))) ; Exclude common link types

;; Org Roam UI Configuration
(use-package! websocket
  :after org-roam) ; Dependency for org-roam-ui

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t             ; Sync UI theme with Emacs
        org-roam-ui-follow t                 ; Follow the current buffer
        org-roam-ui-update-on-save t         ; Update UI on save
        org-roam-ui-open-on-start t))        ; Open UI in browser on start

;; Org Mermaid (for diagrams)
(use-package! ob-mermaid
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(mermaid . t))) ; Enable Mermaid for Org Babel
