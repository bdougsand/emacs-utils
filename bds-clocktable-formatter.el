;; org-update-dblock
;; setq
;; (org-agenda-files t)

;; Want to do this:
;; https://emacs.stackexchange.com/questions/21388/including-archives-in-org-mode-clock-table

;; org-dblock-write:clocktable
;; General idea: before calling org-clocktable-write-default, fold the archive
;; file tables into their corresponding entries. Then call the default function.

;; Challenges:
;; - Keep track of the "current" path when scanning through the destination
;; file's list
;; - Need to also add in top-level trees from the archive
;; - Need to add minutes for each sub-entry to its parents
;;

;; Format of ARCHIVE_OLPATH:
;;  - absent (if top-level)
;;  - tree/subtree
;; Example of how to get table data from an agenda file:

;; (org-add-archive-files) splices in the archive files right after the
;; corresponding org file. Could this be useful?
(defun bds/org-clocktable-prepare-tables (tables params)
  "Given tables as produced by org-dblock-write:clocktable, merge the archived
entries into the appropriate entries in the same file."
  )

(defun bds/clocktable/entry-title (entry)
  (let ((text (nth 1 entry)))
    (set-text-properties 0 (length text) nil text)
    text))


(defun bds/org-clock/index-clock-data (table)
  "Build a hashtable that maps entry names to its corresponding
position in the entries table."
  (let ((idx 0)
        (parent-path (list))
        (last-level 0))
    (-reduce-from
     (lambda (index-hash entry)
       (let* ((level (car entry))
              (name (bds/clocktable/entry-title entry))
              (path (progn (cond
                            ((= level 1)
                             (setq parent-path (list name)))

                            ((<= level last-level)
                             (--dotimes (1+ (- last-level level))
                               (pop parent-path)))

                            (t (push name parent-path)))
                           (string-join
                            (reverse parent-path) "/"))))
         (puthash path idx index-hash)

         (setq last-level level)
         (incf idx)
         index-hash))
     (make-hash-table)
     (nth 2 table))))

(defun bds/org-agenda-corresponding-archives (file-path)
  (with-current-buffer (org-get-agenda-file-buffer file-path)
    (org-all-archive-files)))

(defun bds/merge-table-archive (table)
  (let* ((file-path (nth 0 table))
         (agenda-file-paths (bds/org-agenda-corresponding-archives file-path))
         )
    ))

(let* ((file (nth 8 org-agenda-files))
       (params (org-combine-plists org-clocktable-defaults
                                   '(:properties ("ARCHIVE_FILE" "ARCHIVE_OLPATH")
                                                 :scope 'agenda-with-archives)))
       (table (with-current-buffer (find-buffer-visiting file)
                (save-excursion
                  (save-restriction
                    (org-clock-get-table-data file params))))))
                                        ;(bds/clocktable/entry-title (->> table (nth 2) (nth 0)))
  ;; (index-clock-data table)

  (bds/org-agenda-corresponding-archives (nth 0 table)))

;; could try to merge on ARCHIVE_FILE
(pp (let* ((file (concat (nth 8 org-agenda-files) "_archive"))
           (params (org-combine-plists org-clocktable-defaults
                                       '(:properties ("ARCHIVE_FILE" "ARCHIVE_OLPATH")
                                                     :scope 'agenda-with-archives))))
      (with-current-buffer (find-buffer-visiting file)
        (save-excursion
          (save-restriction
            (org-clock-get-table-data file params))))))

;; Default formatter:
;; https://github.com/jwiegley/org-mode/blob/433103fc5e5bb6d401e37707703a652683b859eb/lisp/org-clock.el#L2321

;; (filename total-minutes (entries))
;; entry: (level entry-header-stuff ?? entry-minutes property-alist)
;; entry-header-stuff: #(entry-name line-start-pos line-end-pos face-properties?)
