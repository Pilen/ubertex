xdotool mousemove 10000 10000                         # Flyt cursor til meget langt væk
xsetroot -cursor .emptycursor.xbm .emptycursor.xbm    # slå cursor fra
xset -b                                               # fjern bell/bip lyd
xset -dpms                                            # Slå energi besparelse for skærmen fra
xset s off                                            # Slå pauseskærm fra
wmname LG3D                                           # Omdøb for at gøre java glad
sxhkd &                                               # start keyboard daemon
exec bspwm                                            # Start xmonad/windowmanager
