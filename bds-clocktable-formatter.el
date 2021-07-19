;; org-update-dblock
;; setq
;; (org-agenda-files t)

;; Want to do this:
;; https://emacs.stackexchange.com/questions/21388/including-archives-in-org-mode-clock-table

;; org-dblock-write:clocktable
;; General idea: before calling org-clocktable-write-default, fold the archive
;; file tables into their corresponding entries. Then call the default function.

;; org-clocktable already supports file-with-archives as a scope, but it
;; separates the archive entries from the file entries.


;; Challenges:
;; - Keep track of the "current" path when scanning through the destination
;; file's list
;; - Need to also add in top-level trees from the archive
;; - Need to add minutes for each sub-entry to its parents
;;

;; Format of ARCHIVE_OLPATH:
;;  - absent (if top-level)
;;  - tree/subtree
(require 'org)
(require 'org-archive)

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

(defun bds/org-clock/index-clock-data-1 (table)
  "Build a hashtable that maps entry names to its corresponding
position in the entries table. Indexed by ARCHIVE_OLPATH
strings (e.g., Heading 1/Subheading/Entry)."
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
                               (pop parent-path))
                             (push name parent-path))

                            (t (push name parent-path)))
                           (string-join
                            (reverse parent-path) "/"))))
         (puthash path entry index-hash)

         (setq last-level level)
         (incf idx)
         index-hash))
     (make-hash-table :test 'equal)
     (nth 2 table))))

(defun bds/org-clock/index-clock-data (table)
  "Build a hashtable that maps entry names to its corresponding
position in the entries table. Indexed by ARCHIVE_OLPATH
strings (e.g., Heading 1/Subheading/Entry)."
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
     (make-hash-table :test 'equal)
     (nth 2 table))))

(defun bds/org-agenda-corresponding-archives (file-path)
  (with-current-buffer (org-get-agenda-file-buffer file-path)
    (org-all-archive-files)))

(defun bds/org-clock-get-table-data (file params)
  (with-current-buffer (find-buffer-visiting file)
    (save-excursion
      (save-restriction
        (org-clock-get-table-data file params)))))

;; (defun bds/inc-entries-for-file (file-path index)
;;   (let* ))

(defun bds/list-insert (n x list)
  (if (zerop n)
      (error "Cannot insert-in-place at position 0")

    (let* ((sublist (nthcdr (- n 1) list)))
      (setcdr sublist (cons x (cdr sublist)))
      list)))

(defun bds/split-table-path (s)
  (-map (lambda (crumb)
          (s-chop-prefix "/" crumb))
   (s-slice-at "/.*$" s)))

(defun bds/pop-path-str (s)
  (if-let ((idx (string-match-p "/[^/]+?$" s)))
      (substring s 0 idx)

    ""))

(defun bds/entry-title-from-path (s)
  (if-let ((idx (string-match-p "/[^/]+?$" s)))
      (substring s (+ 1 idx))

    s))

(defun bds/make-entry (path time)
  (list (s-count-matches "/" path)
        (bds/entry-title-from-path path)
        nil
        nil
        time
        nil ;; Properties
        ))

(defun bds/merge-table-archive (table)
  "Takes the org-clock data table for a file--as returned by org-clock-get-table-data--and merges in data from the archive files associated with that file."
  (let* ((file-path (nth 0 table))
         (agenda-file-paths (bds/org-agenda-corresponding-archives file-path))
         (entries (nth 2 table))
         (index (bds/org-clock/index-clock-data table))
         (params (org-combine-plists org-clocktable-defaults
                                     '(:properties ("ARCHIVE_FILE" "ARCHIVE_OLPATH")))))
    (-each agenda-file-paths
      (lambda (file-path)
        (let* ((archive-table (bds/org-clock-get-table-data file-path params)))
          (-each (nth 2 archive-table)
            ;; Get each clock entry
            (lambda (entry)
              (let* ((properties (nth 5 entry))
                     (ol-path (cdr (assoc "ARCHIVE_OLPATH" properties)))
                     (ol-subpath ol-path)
                     (time (nth 4 entry)))
                (while (not (string-empty-p ol-subpath))
                  (if-let ((original-entry (gethash ol-subpath index)))
                      ;; The archive path has existing data
                      ;; Add the minutes
                      (incf (nth 4 original-entry) time)

                    ;; No existing archive path
                    ;; Create a new entry
                    (let ((new-entry (bds/make-entry ol-subpath)))
                      
                      )
                    )

                  (setq ol-subpath (bds/pop-path-str ol-subpath)))
                )))
          )))
    ))


;; Table:
;; (file-path-str size-bytes-int ((level #(heading-str start-pos-int len-int (:org-clock-minutes count-int)))+ ))

(let* ((file (nth 12 org-agenda-files))
       (params (org-combine-plists org-clocktable-defaults
                           '(:properties ("ARCHIVE_FILE" "ARCHIVE_OLPATH" "ABC")
                                         :scope 'agenda-with-archives))))
  (with-current-buffer (find-buffer-visiting file)
    (save-excursion
      (save-restriction
        ;; (get-text-property 0 :org-clock-minutes )
        (let* ((table (org-clock-get-table-data file params))
               (index (bds/org-clock/index-clock-data-1 table))
               (entry (gethash "XX" index))
               )

          ;; entry
          (incf (nth 4 entry) 1000)
          table
          ;(nth 0 (nth 2 table))
          )
        ))))



(let* ((file (nth 12 org-agenda-files))
       (params (org-combine-plists org-clocktable-defaults
                                   '(:properties ("ARCHIVE_FILE" "ARCHIVE_OLPATH")
                                                 :scope 'agenda-with-archives)))
       (table (with-current-buffer (find-buffer-visiting file)
                (save-excursion
                  (save-restriction
                    (org-clock-get-table-data file params)))))
       (archive-files (bds/org-agenda-corresponding-archives (nth 0 table)))
       ;; (archive-table
       ;;  (with-current-buffer (find-buffer-visiting (first archive-files))
       ;;    (save-excursion
       ;;      (save-restriction
       ;;        (print "hello")
       ;;        (org-clock-get-table-data (first archive-files) params)))))
       )
                                        ;(bds/clocktable/entry-title (->> table (nth 2) (nth 0)))
  ;; (index-clock-data table)

  (first archive-files)
  ;; (print archive-table)
  )


;; could try to merge on ARCHIVE_FILE
;; (pp (let* ((file (concat (nth 8 org-agenda-files) "_archive"))
;;            (params (org-combine-plists org-clocktable-defaults
;;                                        '(:properties ("ARCHIVE_FILE" "ARCHIVE_OLPATH")
;;                                                      :scope 'agenda-with-archives))))
;;       (with-current-buffer (find-buffer-visiting file)
;;         (save-excursion
;;           (save-restriction
;;             (org-clock-get-table-data file params))))))

;; Default formatter:
;; https://github.com/jwiegley/org-mode/blob/433103fc5e5bb6d401e37707703a652683b859eb/lisp/org-clock.el#L2321

;; (filename total-minutes (entries))
;; entry: (level entry-header-stuff ?? entry-minutes property-alist)
;; entry-header-stuff: #(entry-name line-start-pos line-end-pos face-properties?)

;; Experiments...

(s-slice-at "/.*$" "hello/world/how")
;; Testing setf/incf... manipulate places (cons cells?)
(let* ((nested '((1 2 3) (4 5 (6 7))))
       (sl1 (nth 1 nested))
       (sl2 (nth 2 sl1)))
  (setf (nth 0 (nth 2 (nth 1 nested))) 10000)
  (incf (nth 0 sl1))
  nested)
