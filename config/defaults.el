(use-package! rainbow-delimiters
  :demand t
  :defer t
  )
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Enable global-visual-line-mode (words aren't cut in half at the end of line)
(global-visual-line-mode)

;; Increase font size
(set-face-attribute 'default nil :height 130)

(use-package! google-translate :demand t :defer t)

;; So dired will show all files
(remove-hook 'dired-mode-hook #'dired-omit-mode)

;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)

;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

;; For Jupyter Notebook in Emacs
(use-package! ein
  :demand t
  :defer t
  :config
  ;; polymode buffer name, for compatibility with ein
  (unless (fboundp 'pm--visible-buffer-name)
    (defun pm--visible-buffer-name (&rest _args)
      (buffer-name)))
  ;; Images displayed in the buffer
  (setq ein:output-area-inlined-images t)
  )

;; To set $PATH (for ein and eshell)
(let ((my-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":" my-path))
  (add-to-list 'exec-path my-path))

;; Install lemon (system monitor in echo area)
(use-package! lemon
  :demand t
  :config
  (setq lemon-delay 0.2)
  (setq lemon-update-interval 2)
  ;; to display graphics
  (setq lemon-sparkline-use-xpm 1)
  (setq lemon-monitors
	'(((lemon-time :display-opts '(:format "%d %b %H:%M"))
           (custom-set-faces
            '(lemon-time-face ((t (:foreground "orange")))))
	   (lemon-battery)
	   (lemon-cpu-linux :display-opts '(:sparkline (:type gridded)))
	   (lemon-memory-linux :display-opts '(:sparkline (:type gridded)))
	   (lemon-linux-network-rx :display-opts '(:sparkline (:type gridded)))
	   (lemon-linux-network-tx :display-opts '(:sparkline (:type gridded)))
	   )))

  (lemon-mode 1))
