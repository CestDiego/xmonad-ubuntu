xmonad-ubuntu by @CestDiego
==================

Still has only been tested in Ubuntu 14.04 and also install variety

# Installation

git clone git@github.com/CestDiego/xmonad-ubuntu ~/.xmonad

## Then you can run the installation script by just doing

sudo chmod +x ~/.xmonad/install-xmonad
~/.xmonad/install-xmonad

By default the Block Mayus key is Set to be an extra Control Keyh (no more Emacs Pinky)

Also xbacklight (monitor brightness) is set to be 20% of brightness by default you can change this in the ~/.xmonad/startup-hook

If you have Emacs installed it opens by default on the first workspace

To enable urxvt support in remote ssh sessions

--------------------------------------------------------
FULL URXVT TERMINAL SUPPORT FOR REMOTE (SSH) CONNECTIONS
                  Written on 1.8.2012
--------------------------------------------------------
Problem description

You open a SSH connection to an external host and when you try to do
something that requires extended terminal support you get an error along the
lines: "'rxvt-unicode-256color': unknown terminal type."

Solution

1. Run the following command on the external host to make a terminfo directory
   under the logged in user's home folder.

       mkdir -p ~/.terminfo/r/

2. Copy the appropriate terminal profile from your local machine to the newly
   created folder on the remote host.

       scp /usr/share/terminfo/r/rxvt-unicode-256color user@helloworld.com:.terminfo/r/

3. Restart the SSH connection. It should work now.







