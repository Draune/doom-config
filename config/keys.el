;; Install devil (translate "," to "C-"; and use support of which-key from fbrosda)
(use-package devil
  :demand t
  :custom
  ;; Don't use keys that are usefull when repeated (for exemple ";" for comments)
  (devil-exit-key "q")
  (devil-all-keys-repeatable t)
  (devil-highlight-repeatable t)

  :bind
  ([remap describe-key] . devil-describe-key)
  :config
  (setq devil-repeatable-keys '(("%k p" "%k n" "%k b" "%k f" "%k a" "%k e")
			        ;; repeatable keys for window and buffer management and find-file and dired
			        ;; I went a little crazy here but since most of the time actions on buffers
			        ;; and windows are followed by other commands like "C-n" I believe it's ok
			        ("%k c k" "%k x o" "%k x &" "%k x é" "%k x \"" "%k x à" "%k x %k f" "%k x d" "%k x b" "%k x 0" "%k x 1" "%k x 2" "%k x 3")
			        ;; repeatable keys for movement M- keybindings
			        ("%k %k n" "%k %k p" "%k %k f" "%k %k b")))
  (if exwm_up
      (push '("%k <XF86AudioLowerVolume>" "%k <XF86AudioRaiseVolume>" "%k <XF86AudioMute>" "%k <XF86MonBrightnessDown>" "%k <XF86MonBrightnessUp>") devil-repeatable-keys))

  ;; change the devil prompt for a devil smiley
  (setq devil-lighter " \U0001F608")
  (setq devil-prompt "\U0001F608 %t")

  (global-devil-mode)
  (global-set-key (kbd "C-,") 'global-devil-mode)
  ;; I don't really know why but if I don't do this line before calling assoc-delete-all
  ;; it will not do it. I use asoc-delete-all to delete the devil special key "%k %k" (", ,")
  ;; so ", ," will be translated to "M-". Plus this line is usefull to mark things
  (add-to-list 'devil-special-keys `(", , SPC" . ,(devil-key-executor "C-SPC")))
  (assoc-delete-all "%k %k" devil-special-keys)
  (setq devil-translations '((", , ," . "C-M-")
			     (", ," . "M-")
			     ("," . "C-")
			     )))

;; My keybindings
;; Global keybindings:
(map! "C-c d e" #'eshell)
(map! "C-c d w" #'eww)
(map! "C-c d t" #'google-translate-buffer)
(map! "C-c d v" #'vterm)

;; + devil repeatable keys
(map! "C-c k" #'kill-current-buffer)
;; Doublons here for better experience with Azerty keyboards
(map! "C-x \"" #'split-window-right)
(map! "C-x é" #'split-window-below)
(map! "C-x à" #'delete-window)
(map! "C-x &" #'delete-other-windows)

;; better functions fo default keybindings
(map! "C-k" #'kill-whole-line)
(map! "M-n" (lambda () (interactive) (next-line 10)))
(map! "M-p" (lambda () (interactive) (previous-line 10)))
(map! "C-o" (lambda () (interactive)
              (call-interactively 'move-beginning-of-line)
              (call-interactively 'open-line)
              (call-interactively 'indent-for-tab-command)))
(map! "M-a" #'beginning-of-buffer)
(map! "M-e" #'end-of-buffer)
;; Use M-p and M-n for the command history when using M-&
;; Already binded but (I don't really know why) if I don't declare it
;; there is a bug when repeated
(map! "M-p" #'previous-line-or-history-element :map minibuffer-local-shell-command-map)
(map! "M-n" #'next-line-or-history-element :map minibuffer-local-shell-command-map)

;; Change "C-x b" so it will show all buffers
(map! "C-x b" #'consult-buffer)

;; Setup which-key (key cheatsheet that is displayed during key sequences)
(which-key-mode)
(which-key-setup-minibuffer)
