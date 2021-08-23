  
**cemacs**  
  
cemacs is a configuration for emacs centered around c based language development,  
useful code auto-formatting and highlighing packages are included improves readability   
and reduces repetetive formatting work, as well as easing debugging somewhat.  
  
This project strives to stay close to an standard emacs setup whilst  
adjusting many behaviors to be more user friendly and efficient  
  
# Notes on the Differences from Vinilla Emacs  
  
New Functions  
cemacs-kill-volatile-buffer  
cemacs-scroll-up-in-place  
cemacs-scroll-down-in-place  
slay-function  
slay-whole-buffer  
  
transient-mark-mode is disabled by default, if you wish to wrap a visible region  
press the set mark command twice  
C-SPC C-SPC  
  
Keybind Changes  
C-j is now 'crux-top-join-line  
C-s will a now instead pull up 'hydra-slayer, a prefix command that allows you   
to kill entire regions of text more easilly, for text searching see hydra-query  
C-q is now 'hydra-query, a prefix command for searching, C-q s replaces isearch   
with helm-swoop  
C-r is now 'avy-goto-char this is an extremely efficient way of text navigation  
M-r is now 'avy-pop-mark which will navigate through previous locations before  
avy enhanced movement  
C-x e is now 'crux-find-user-init-file  
C-x k now calls a new function 'cemacs-kill-volatile-buffer  
C-x M-a calls 'restart-emacs  
C-a : Is 'crux-beginning-of-line', a more intelligent way of navigating in a line
C-e : Is 'cemacs-natural-end-of-line'
M-m is made redundant by 'crux-move-beginning-of-line', 'mc/edit-lines' is used instead
# Credit
It's nice to show some appreciation for the hacks and help you come  
across.  
As well as pointing people towards the various places where they might  
get a little more information than from here.  

This is a hack responsible for the coloured checkbox lines in
org-mode,  
which is largely based on these posts
[Fontify done checkbox items in org-mode](https://fuco1.github.io/2017-05-25-Fontify-done-checkbox-items-in-org-mode.html)  
[Change color of org-mode checkboxes - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/45291/change-color-of-org-mode-checkboxes)  
A simple way of checking if an org heading under the point is folded
[org-folded-p by dan](https://sourcehut.org/consultancy/)
[a](https://sourcehut.org/consultancy/)
