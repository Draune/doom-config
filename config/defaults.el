(use-package! rainbow-delimiters
  :demand t
  :defer t
  )
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Enable global-visual-line-mode (words aren't cut in half at the end of line)
(global-visual-line-mode)

;; Increase font size
(set-face-attribute 'default nil :height 130)

;; Install corfu-terminal
(use-package! corfu-terminal
  :demand t
  :config
  (corfu-terminal-mode t)
  (setq corfu-terminal-disable-on-gui nil)
  )

(use-package! google-translate :demand t :defer t)

;; So dired will show all files
(remove-hook 'dired-mode-hook #'dired-omit-mode)

;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)

;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

;; For Jupyter Notebook in Emacs
(use-package! ein :demand t :defer t)

;; To set $PATH (for ein and eshell)
(let ((my-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":" my-path))
  (add-to-list 'exec-path my-path))
