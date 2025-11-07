# My Doom Emacs config

What's in it:

- Devil package (working with wich key)
- EXWM (will only be launched if Emacs was started by xinit, with everything set up for sound and brightness)
- Eat terminal emulator with Eshell and also Vterm
- Some other defaults

Use with Emacs 30.1

Install config after installing DOOM: 

```shell
rm -rf ~/.config/doom
git clone https://github.com/Draune/doom-config.git ~/.config/doom
~/.config/emacs/bin/doom sync
```

To use with all the fonctionnalities with EXWM install (name of the executables):

- to take sreenshots ("\<print\>"): maim
- for sound control (sound keys like "\<XF86AudioLowerVolume\>"): pactl 
- for brightness control (brightness keys like "\<XF86MonBrightnessDown\>"): xrandr
- to lock the screen ("C-c l"): xtrlock
