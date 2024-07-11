(defvar bootstrap-version)
    (let ((bootstrap-file
	   (expand-file-name
	    "straight/repos/straight.el/bootstrap.el"
	    (or (bound-and-true-p straight-base-dir)
		user-emacs-directory)))
	  (bootstrap-version 7))
      (unless (file-exists-p bootstrap-file)
	(with-current-buffer
	    (url-retrieve-synchronously
	     "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	     'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(setq make-backup-files nil) ; stop creating ~ files

(setq create-lockfiles nil)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(set-face-attribute 'default nil
  :font "MesloLGS NF 16"
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "MesloLGS NF 16"
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "MesloLGS NF 16"
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "MesloLGS NF 16"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(setq inhibit-startup-message t  ; Don't show the splash screen
	visible-bell t)            ; Flash when the bell rings

  ;; Turn off some unneeded UI elements
  (menu-bar-mode -1)  ; Leave this one on if you're a beginner!
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
(setq display-line-numbers-type 'relative)

  ;; Display line numbers in every buffer
  (global-display-line-numbers-mode 1)

(use-package autothemer :ensure t)

(straight-use-package
 '(rose-pine-emacs
   :host github
   :repo "thongpv87/rose-pine-emacs"
   :branch "master"))
(load-theme 'rose-pine-color t)

(use-package evil
      :ensure t ;; install the evil package if not installed
      :init ;; tweak evil's configuration before loading it
      (setq evil-search-module 'evil-search)
      (setq evil-ex-complete-emacs-commands nil)
      (setq evil-vsplit-window-right t)
      (setq evil-split-window-below t)
      (setq evil-shift-round nil)
      (setq evil-want-C-u-scroll t)
      (setq evil-want-keybinding nil)
      :config ;; tweak evil after loading it
      (evil-mode)
     )
  (use-package evil-collection
    :after evil
    :config
    ;; Do not uncomment this unless you want to specify each and every mode
    ;; that evil-collection should works with.  The following line is here 
    ;; for documentation purposes in case you need it.  
    ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
    (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
    (evil-collection-init))
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(use-package general
  :ensure t 
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer vraton/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (vraton/leader-keys
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "b i" '(ibuffer :wk "IBuffer")
    "bk" '(kill-this-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer"))

  (vraton/leader-keys
    "e" '(:ignore t :wk "Evaluate")    
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    )

  (vraton/leader-keys
    "." '(find-file :wk "Find file")
    "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config")
    "TAB TAB" '(comment-line :wk "Comment lines")
    )


  (vraton/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h r r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload emacs config")

    ;;"h r r" '(reload-init-file :wk "Reload emacs config")
    )

  (vraton/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm-toggle :wk "Toggle vterm")
    "t e" '(eshell :which-key "Eshell")
    )

  (vraton/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right")
    )

  )

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; run once
;;(all-the-icons-install-fonts t)
;;(nerd-icons-install-fonts t)

(use-package which-key
  :ensure t
  :init
    (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
	which-key-sort-order #'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.25
	which-key-idle-delay 0.8
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit t
	which-key-separator " → " ))

(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets :ensure t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)

(require 'org-tempo)

(use-package sudo-edit
  :config
    (vraton/leader-keys
      "fu" '(sudo-edit-find-file :wk "Sudo find file")
      "fU" '(sudo-edit :wk "Sudo edit file")))

(use-package rainbow-mode
  :hook 
  ((org-mode prog-mode) . rainbow-mode))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; eshell-syntax-highlighting -- adds fish/zsh-like syntax highlighting.
;; eshell-rc-script -- your profile for eshell; like a bashrc for eshell.
;; eshell-aliases-file -- sets an aliases file for the eshell.
  
(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

(use-package vterm
:config
(setq shell-file-name "/bin/fish"
      vterm-max-scrollback 5000))

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.3))))
