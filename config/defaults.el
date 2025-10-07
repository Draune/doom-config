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
