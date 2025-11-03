(setq inhibit-startup-message t)
(editorconfig-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(load-theme 'leuven-dark t)
(xterm-mouse-mode 1)
(setq-default tab-width 4)
(electric-pair-mode 1)

;; Keep emphemeral customization in separate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Auto-install packages
(dolist (package '(company editorconfig vertico consult diff-hl))
  (unless (package-installed-p package)
    (package-install package)))

;; Auto-completion
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)

;; Fuzzy file finding
(vertico-mode 1)
(if (executable-find "fd")
    (global-set-key (kbd "C-c f") 'consult-fd)
  (progn
    (setq consult-find-args "find . -not ( -path '*/.git/*' -prune )")
    (global-set-key (kbd "C-c f") 'consult-find)))
(if (executable-find "rg")
    (global-set-key (kbd "C-c g") 'consult-ripgrep)
  (global-set-key (kbd "C-c g") 'consult-grep))
(global-set-key (kbd "C-c l") 'consult-line)

;; Git gutter
(global-diff-hl-mode 1)

;; Diagnostics navigation
(add-hook 'prog-mode-hook 'flymake-mode)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)

;; Undo/redo
(global-set-key (kbd "M-_") 'undo-redo)

;; Format on save
(defun my-elisp-autoformat-on-save ()
  "Auto-indent Emacs Lisp buffers on save."
  (when (derived-mode-p 'emacs-lisp-mode)
    (save-excursion
      (untabify (point-min) (point-max))
      (delete-trailing-whitespace)
      (indent-region (point-min) (point-max)))))

;; Go

(add-hook 'before-save-hook #'my-elisp-autoformat-on-save)

(setq treesit-language-source-alist
      '((go    "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-ts-mode))

(add-hook 'go-ts-mode-hook #'eglot-ensure)
(add-hook 'gomod-ts-mode-hook #'eglot-ensure)

;; Format and organize imports on save using gopls via Eglot
(defun my-go-eglot-format+imports ()
  "Format buffer and organize imports using Eglot/gopls."
  (interactive)
  (when (eglot-managed-p)
    (eglot-format)
    (eglot-code-actions nil nil "source.organizeImports" t)))

(add-hook 'go-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my-go-eglot-format+imports nil t)))
