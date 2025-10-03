;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
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
(setq doom-theme 'doom-molokai)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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

;; Install devil (translate "," to "C-"; and use support of which-key from fbrosda)
(use-package devil
  :demand t
  :custom
  ;; Don't use keys that are usefull when repeated (for exemple ";" for comments)
  (devil-exit-key "q")
  (devil-all-keys-repeatable t)
  (devil-highlight-repeatable t)
  (devil-repeatable-keys '(("%k p" "%k n" "%k b" "%k f" "%k a" "%k e")
			   ;; repeatable keys for window and buffer management and find-file and dired
			   ;; I went a little crazy here but since most of the time actions on buffers
			   ;; and windows are followed by other commands like "C-n" I believe it's ok
			   ("%k c k" "%k x o" "%k x &" "%k x é" "%k x \"" "%k x à" "%k x %k f" "%k x d" "%k x b" "%k x 0" "%k x 1" "%k x 2" "%k x 3")
			   ;; repeatable keys for movement M- keybindings
			   ("%k %k n" "%k %k p" "%k %k f" "%k %k b")))
  
  :bind
  ([remap describe-key] . devil-describe-key)
  :config
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

;; Setup which-key (key cheatsheet that is displayed during key sequences)
(which-key-mode)
(which-key-setup-minibuffer)

(defun emacs-ppid ()
  "Retourne le PID (string) du processus parent d’Emacs."  
  (string-trim
   (shell-command-to-string
    (format "ps -o ppid= -p %d" (emacs-pid)))))

(defun emacs-parent-name ()
  "Retourne le nom du processus parent d’Emacs (Linux uniquement)."
  (string-trim
   (shell-command-to-string
    (format "ps -o comm= -p %s" (emacs-ppid)))))

;; Install exwm (just if Emacs was called by xinit)
(if (equal (emacs-parent-name) "xinit")
    (progn
      (use-package! exwm
	:demand t
	:config
	(setq exwm-workspace-number 4)
	;; Make class name the buffer name.
	(add-hook 'exwm-update-class-hook
		  (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
	;; Special EXWM bindings
	(setq exwm-input-global-keys
	      '(
		([?\C-q] . exwm-input-send-next-key)
		))
	;; To use devil when working with X windows (like ", x o")
	(push ?, exwm-input-prefix-keys)
	;; To get Emacs bindings inside X windows (don't works with devil, ie. ", n" will not work but "C-n" will)
	(setq exwm-input-simulation-keys
	      '(([?\C-b] . [left])
		([?\C-f] . [right])
		([?\C-p] . [up])
		([?\C-n] . [down])
		([?\C-a] . [home])
		([?\C-e] . [end])
		([?\M-v] . [prior])
		([?\C-v] . [next])
		([?\C-d] . [delete])
		([?\C-k] . [S-end delete])
		([?\C-w] . [?\C-x])
		([?\M-w] . [?\C-c])
		([?\C-y] . [?\C-v])
		([?\M-d] . [C-delete])
		([?\;] . [?,])))
	;; Lauch app
	(map! "C-c r" #'exwm-reset)
	(map! "C-c SPC" (lambda (cmd)
			  (interactive (list (read-shell-command "$ ")))
			  (start-process-shell-command cmd nil cmd)))
	;; Enable EXWM
	(exwm-wm-mode)
	)
      
      ;; Install lemon (system monitor in echo area)
      (use-package! lemon
	:demand t
	:config
	(setq lemon-delay 0.2)
	(setq lemon-update-interval 2)
	;; to display graphics
	(setq lemon-sparkline-use-xpm 1)
	(setq lemon-monitors
	      '(((lemon-time :display-opts '(:format "%H:%M"))
                 (custom-set-faces
                  '(lemon-time-face ((t (:foreground "orange")))))
		 (lemon-battery)
		 (lemon-cpu-linux :display-opts '(:sparkline (:type gridded)))
		 (lemon-memory-linux :display-opts '(:sparkline (:type gridded)))
		 (lemon-linux-network-rx :display-opts '(:sparkline (:type gridded)))
		 (lemon-linux-network-tx :display-opts '(:sparkline (:type gridded)))
		 )))

	(lemon-mode 1))
      
      ;; Screenshots
      (if (executable-find "maim")
	  (progn
	    (map! :map 'override "C-c d s" (lambda () (interactive) (shell-command (format-time-string "maim -s '/home/louis/Pictures/%F_%X.png'"))))
	    ))
      
      (if (executable-find "xrandr")
	  (progn
	    (setq brightness 1.0)
	    (defun brightness_add (to_add)
	      "Add brightness, take account of the current brightness (no more than 1.0 or less than 0.0)"
	      (setq final_brightness (+ brightness to_add))
	      (if (and (<= final_brightness 1.0) (>= final_brightness 0.0))
		  (progn
		    (shell-command (format "xrandr --output eDP-1 --brightness %f" final_brightness))
		    (setq brightness final_brightness)
		    ))
	      )
	    (map! "<XF86MonBrightnessDown>" (lambda () (interactive) (brightness_add -0.05)))
	    (map! "<XF86MonBrightnessUp>" (lambda () (interactive) (brightness_add 0.05)))
	    ))
      (if (executable-find "xtrlock")
	  (progn
	    (require 'zone)
	    (setq zone-programs [zone-pgm-whack-chars])  
	    (defun lock-screen ()
	      "Lock screen using (zone) and xtrlock
 calls M-x zone on all frames and runs xtrlock"
	      (interactive)
	      (delete-other-windows)
	      (switch-to-buffer "*scratch*")
	      (save-excursion
					;(shell-command "xtrlock &")
		(set-process-sentinel
		 (start-process "xtrlock" nil "xtrlock")
		 '(lambda (process event)
		    (zone-leave-me-alone)))
		(zone-when-idle 1)))
	    (map! "C-c l" #'lock-screen)
	    ))
      ))

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
