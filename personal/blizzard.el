;; Blizzard specific Emacs functions
(defun ido-remove-tramp-from-cache nil
"Remove any TRAMP entries from `ido-dir-file-cache'.
This stops tramp from trying to connect to remote hosts on emacs startup,
which can be very annoying."
(interactive)
(setq ido-dir-file-cache
  (cl-remove-if
   (lambda (x)
     (string-match "/\\(rsh\\|ssh\\|telnet\\|su\\|sudo\\|sshx\\|krlogin\\|ksu\\|rcp\\|scp\\|rsync\\|scpx\\|fcp\\|nc\\|ftp\\|smb\\|adb\\):" (car x)))
   ido-dir-file-cache)))
;; redefine `ido-kill-emacs-hook' so that cache is cleaned before being saved
(defun ido-kill-emacs-hook ()
(ido-remove-tramp-from-cache)
(ido-save-history))
