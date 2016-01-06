Battery Monitor
=====

I wrote this quick and dirty battery monitor because the battery monitor for xmobar takes up too much screen space and doesn't work well with my Asus T100 Transformer book.

Also, much needed Haskell practice.

Run the built binary in a cron job every couple of minutes, and an alert will appear if the remaining battery is less than 10% or less than 5% (10% exists because sometimes my laptop will just die at 5%).

I'm planning on implementing a way for the battery monitor to check whether the user has already been delivered a notification because as of now, every X minutes (depending on your cron job configuration) you'll receive a notification if your battery is below a certain percentage.

Also planning on making the addition of the binary to the crontab less annoying because configuring cronjobs is annoying.

Crontab fixes
-----

Typically crontab can't access the X display. Thus, add:
    
    xhost local:[your username here without brackets] > /dev/null

to your .bashrc.

For example, I would add:
    
    xhost local:chokboy > /dev/null
