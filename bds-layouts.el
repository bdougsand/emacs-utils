(require 'eyebrowse)


(defun bds/eyebrowse--make-with-window-maximized ()
  (interactive)
  (let* ((window-configs (eyebrowse--get 'window-configs))
         (new-index (1+ (length window-configs))))
    (eyebrowse-switch-to-window-config new-index)
    (spacemacs/toggle-maximize-buffer)))
