Battery Monitor
=====

I wrote this quick and dirty battery monitor because the battery monitor for xmobar takes up too much screen space and doesn't work well with my Asus T100 Transformer book.

Also, much needed Haskell practice.

Todo: Use System.Daemon to run as a daemon because having to configure a cron job is incredibly annoying.

Crontab fixes (Deprecated)
-----

Typically crontab can't access the X display. Thus, add:
    
    xhost local:[your username here without brackets] > /dev/null

to your .bashrc.

For example, I would add:
    
    xhost local:chokboy > /dev/null
