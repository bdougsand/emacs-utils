(require 'key-chord)

(defun bds/project-capture-file ()
  (let ((project projectile-project-name)
        (default-directory "~/org-mode/agenda"))
    (let (capture-file (expand-file-name (if project
                                             (format "%s.org.txt" project)
                                           "other.org.txt")))
      capture-file)))

(defun python-shell-dwim-send ()
  (interactive)
  (if (region-active-p)
      (python-shell-send-region (region-beginning) (region-end))
    (python-shell-send-defun)))

(defun bds/eyebrowse--make-with-window-maximized ()
  "Make a new layout in the current perspective by maximizing the active window."
  (interactive)
  (let* ((window-configs (eyebrowse--get 'window-configs))
         (new-index (1+ (length window-configs))))
    (eyebrowse-switch-to-window-config new-index)
    (spacemacs/toggle-maximize-buffer)))

;; Indirect buffers

;; By making org-last-indirect-buffer a buffer-local variable, each buffer can
;; have its own indirect buffer. Nice if you're switching among multiple org
;; files.
(make-variable-buffer-local 'org-last-indirect-buffer)

(defun bds/indirect-buffer-jump ()
  "Create a new indirect buffer from the current header then make its window active."
  (interactive)
  (org-tree-to-indirect-buffer)
  (select-window (get-buffer-window org-last-indirect-buffer))
  (end-of-buffer))

(defun bds/indirect-buffer-prev (&optional n)
  (interactive "p")

  (bds/indirect-buffer-next (* (or n 1) -1)))

(defun bds/org-forward (&optional n)
  "Intended to behave like org-forward-heading-same-level for the most part, but with some enhancements (IMO) for working with indirect buffers.

When run inside an indirect buffer, it changes the contents to the next (or previous) subtree in the parent buffer."
  (interactive)
  (let ((bbuff (buffer-base-buffer))
        (win (selected-window)))
    (if (and (buffer-narrowed-p) bbuff)
        (let ((start (save-mark-and-excursion
                       (org-back-to-heading t)
                       (point))))
          (select-window (get-buffer-window bbuff))
          (save-mark-and-excursion
            (goto-char start)
            (org-forward-heading-same-level n)
            (org-tree-to-indirect-buffer))
          (select-window win))

      (progn
        (org-forward-heading-same-level n)

        (when (buffer-live-p org-last-indirect-buffer)
          (org-tree-to-indirect-buffer))))))

(defun bds/org-backward (&optional n)
  (interactive)
  (bds/org-forward (* (or n 1) -1)))

;; Override evil-org-mode's bindings for navigating headings so that it updates
;; the indirect buffer (if there is one).
(with-eval-after-load 'org
  (spacemacs/set-leader-keys-for-major-mode "org-mode" "B" 'bds/indirect-buffer-jump)

  (evil-define-key 'normal evil-org-mode-map
    "gj" 'bds/org-forward
    "gk" 'bds/org-backward))


;;; QuickTime
;; A very minor minor mode to help me take notes on videos.

(defun bds/quicktime-get-position ()
  "Returns the position of the playhead (in seconds) of the foremost QuickTime movie."
  (save-excursion
    (let ((osabuff (get-buffer-create "*osascript-output*")))
      (with-current-buffer osabuff
        (erase-buffer)
        (shell-command "osascript -e 'tell application \"QuickTime Player\" to tell the front document to get the current time'"
                       osabuff)
        (string-to-number (buffer-string))))))

(defun bds/quicktime-toggle ()
  "Pause or play the foremost movie."
  (shell-command "osascript -e 'tell application \"QuickTime Player\" to tell the front document
	if playing then
		pause
	else
		play
	end if
end tell
'"))

(defun bds/quicktime-offset (seconds)
  (shell-command (format "osascript -e 'tell application \"QuickTime Player\" to tell the front document to set the current time to ((current time) + %d)'"
                         (or seconds 5))))

(defvar bds-quicktime-mode-map nil
  "Keymap used in bds-quicktime-mode")

(define-minor-mode bds-quicktime-mode
  "Key bindings for manipulating the foremost QuickTime video"
  :keymap bds-quicktime-mode-map)

(defun bds/quicktime-insert-position ()
  (interactive)
  (insert (format-seconds "[%m:%.2s] " (bds/quicktime-get-position))))

(defun bds/quicktime-back ()
  (interactive)
  (bds/quicktime-offset -2))

(defun bds/quicktime-forward ()
  (interactive)
  (bds/quicktime-offset 2))


(unless bds-quicktime-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-T") 'bds/quicktime-insert-position)
    (key-chord-define map ";]" 'bds/quicktime-forward)
    (key-chord-define map ";[" 'bds/quicktime-back)

    (setq bds-quicktime-mode-map map)))


(spacemacs/set-leader-keys "w]" 'bds/eyebrowse--make-with-window-maximized)

;; Shortcuts for switching perspective:
(global-set-key (kbd "s-}") 'persp-next)
(global-set-key (kbd "s-{") 'persp-prev)
(global-set-key (kbd "s-]") 'eyebrowse-next-window-config)
(global-set-key (kbd "s-[") 'eyebrowse-prev-window-config)

(global-set-key (kbd "s--") 'zoom-frm-out)
(global-set-key (kbd "s-=") 'zoom-frm-in)

(global-set-key (kbd "s-b") 'switch-to-buffer)

(with-eval-after-load 'paredit
  (add-hook 'clojure-mode-hook
            (lambda ()
              (paredit-mode 1)
              (smartparens-mode 0)))
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (paredit-mode 1)
                                    (smartparens-mode 0)))
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "C-)") 'paredit-forward-slurp-sexp))

(require 'dash)
(--each (number-sequence 1 9)
  (let ((n (int-to-string it)))
    (global-set-key (kbd (concat "s-" n))
                    (intern (concat "spacemacs/persp-switch-to-" n)))))

(with-eval-after-load 'python-mode
  (define-key python-mode-map (kbd "C-M-x") 'python-shell-dwim-send))

(with-eval-after-load 'clojure-mode
  (put-clojure-indent 'defui '(1 :form (1))))




;; (defun bds/indirect-buffer-next (&optional dir)
;;   "Move"
;;   (interactive "p")
;;   (let ((pos (point))
;;         beg end)
;;     (save-excursion
;;       (org-back-to-heading t)
;;       (widen)
;;       (org-forward-heading-same-level
;;        (or (not dir) (= dir 0)) t)
;;       (setq beg (point))
;;       (org-end-of-subtree t t)
;;       (when (org-at-heading-p)
;;         (backward-char 1)
;;         (setq end (point))))
;;     (when end
;;       (narrow-to-region beg end)
;;       (outline-show-all)
;;       (goto-char pos))))