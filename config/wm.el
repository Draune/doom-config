;; use of EXWM and all the things needed by a wm

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
                ([?\s-r] . exwm-reset)
		))
	;; To use devil when working with X windows (like ", x o")
	(push ?, exwm-input-prefix-keys)
	;; To get Emacs bindings inside X windows (don't works with devil, ie. ", n" will not work but "C-n" will)
	(setq exwm-input-simulation-keys
	      '(([?\C-b] . [left])
		([?\C-f] . [right])
		([?\C-p] . [up])
		([?\C-n] . [down])
                ([?\M-b] . [C-left])
                ([?\M-f] . [C-right])
                ([?\M-p] . [C-up])
                ([?\M-n] . [C-down])
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
	  (map! "<print>" (lambda () (interactive) (shell-command (format-time-string (format "maim -s '%s/Pictures/%%F_%%X.png'" (getenv "HOME"))))))
	)

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
              (message "Brightness: %d%%" (round (* brightness 100)))
              )
	    (map! "<XF86MonBrightnessDown>" (lambda () (interactive) (brightness_add -0.05)))
	    (map! "<XF86MonBrightnessUp>" (lambda () (interactive) (brightness_add 0.05)))
	    ))
      (if (executable-find "xtrlock")
	  (progn
	    (defun lock-screen ()
	      "Lock screen using (zone) and xtrlock
 calls M-x zone on all frames and runs xtrlock"
	      (interactive)
	      (save-excursion
		(start-process "xtrlock" nil "xtrlock")
		))
	    (map! "C-c l" #'lock-screen)
	    ))
      ))
