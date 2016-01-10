Battery Monitor
=====

I wrote this battery monitor daemon because typical battery monitors take up too much screen space and don't work well with my Asus T100 Transformer book.

Also, much needed Haskell practice.

Run battery-monitor on startup. Running it without any arguments will result in it providing an alert when charge falls below 10%. You can choose to run it with an argument to pick a different threshold.

Currently, the .battery-monitorinfo file generated in your home directory is used for preventing alerts from being generated when an alert has already generated and the charge is still below the alert threshold.

TODO: Obtain battery file location from command line argument and save the location in ~/.battery-monitorinfo.

~~Crontab fixes~~
-----

~~Typically crontab can't access the X display. Thus, add:
    
    xhost local:[your username here without brackets] > /dev/null

~~to your .bashrc.~~

~~For example, I would add:~~
    
    xhost local:chokboy > /dev/null
