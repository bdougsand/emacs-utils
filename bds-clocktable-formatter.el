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


(defvar bds/org-clock/default-properties
  '("ARCHIVE_FILE" "ARCHIVE_OLPATH"))
(defvar bds/org-clock-default-params
  (org-combine-plists org-clocktable-defaults
                      (list :properties bds/org-clock/default-properties)))
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

(defun bds/clocktable/prepare-params (params)
  (let ((properties (cl-concatenate 'list (plist-get params :properties) bds/org-clock/default-properties)))
    (org-combine-plists org-clocktable-defaults
                        (list :properties bds/org-clock/default-properties)
                        params)))

(defun bds/org-clock/index-clock-data (table)
  "Build a hashtable that maps the path of each clocktable entry to
its corresponding entry. Indexed by ARCHIVE_OLPATH strings (e.g.,
Heading 1/Subheading/Entry)."
  (let ((parent-path (list))
        (last-level 0)
        (index-hash (make-hash-table :test 'equal))
        (cur (nth 2 table)))
    (while cur
      (let* ((entry (car cur))
             (level (car entry))
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
        (puthash path cur index-hash)
        (setq last-level level)
        (setq cur (cdr cur))))
    index-hash))

(defun bds/org-agenda-corresponding-archives (file-path)
  (with-current-buffer (org-get-agenda-file-buffer file-path)
    (org-all-archive-files)))

(defun bds/org-clock-get-table-data (file &optional params)
  ;; (org-clock-get-table-data file (or params bds/org-clock-default-params))
  (with-current-buffer (find-file-noselect file 't)
    (save-excursion
      (save-restriction
        (org-clock-get-table-data file (bds/clocktable/prepare-params params)))))
  )

(defun plist-get-default (plist prop default)
  (if-let ((p (plist-member plist prop)))
      (cadr p)
    default))

(defun list-last-cell (list)
  (if-let ((rest (cdr list)))
      (list-last-cell rest)

    list))

(defun list-get-cell (n list &optional was-extended-fn)
  (when (> 0 n)
    (error "Index must be 0 or greater"))

  (let ((was-extended))
    (while (> n 0)
      (unless (cdr list)
        (setcdr list (cons nil nil))
        (setq was-extended 't))

      (setq list (cdr list))
      (setq n (- n 1)))
    (when (and was-extended was-extended-fn)
      (funcall was-extended-fn)))
  list)

(defun bds/splice-list (n xs list)
  "Insert xs into a list at index n. Modifies the list in place.
list must not be nil."
  (let* ((extended)
         (sublist (list-get-cell n list (lambda () (setq extended 't))))
         (head (car sublist))
         (rest (cdr sublist)))
    (setcar sublist (car xs))
    (setcdr sublist (cdr xs))
    (unless extended
      (setcdr (list-last-cell sublist) (cons head rest)))
    list))


(defun bds/insert-where (fn xs list &rest kargs)
  (let* ((idx 0)
         (last)
         (cur list)
         (after (plist-get kargs :after))
         (before (plist-get-default kargs :before (eq after nil))))
    (while cur
      (if (funcall fn cur idx)
          (progn
            (when after
              (bds/splice-list 1 xs cur))
            (when before
              (if list
                  (bds/splice-list 0 xs cur)

                (setq list (cons x list))))
            (setq cur nil))

        (progn
          (incf idx)
          (setq last cur)
          (setq cur (cdr cur)))))

    list))

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

(defun bds/make-entry (path time &optional props)
  (list (+ (s-count-matches "/" path) 1)
        (bds/entry-title-from-path path)
        nil
        nil
        time
        props))

(defun bds/merge-table-archive (table)
  "Merge archived entries into a clocktable data table. TABLE
contains clocking data as produced by `org-clock-get-table-data`
and will be modified in place."
  (let* ((file-path (nth 0 table))
         (agenda-file-paths (bds/org-agenda-corresponding-archives file-path))
         (entries (nth 2 table))
         (index (bds/org-clock/index-clock-data table)))
    (-each agenda-file-paths
      (lambda (file-path)
        (let* ((archive-table (bds/org-clock-get-table-data file-path)))
          (-each (nth 2 archive-table)
            ;; Get each clock entry
            (lambda (entry)
              (let* ((properties (nth 5 entry))
                     (ol-path (concat
                               (cdr (assoc "ARCHIVE_OLPATH" properties))
                               "/" (bds/clocktable/entry-title entry)))
                     (ol-subpath ol-path)
                     (time (nth 4 entry))
                     (new-subtree nil))
                (while (not (string-empty-p ol-subpath))
                  (if-let ((original-entry (gethash ol-subpath index)))
                      ;; The archive path has existing data
                      ;; Add the minutes
                      (let ((level (caar original-entry)))
                        (cl-incf (nth 4 (car original-entry)) time)
                        (cl-incf (nth 1 table) time)
                        (when new-subtree
                          (bds/insert-where
                           (lambda (cell idx)
                             (or (not (cdr cell))
                                 (<= (caadr cell) level)))
                           new-subtree
                           original-entry
                           :after 't)
                          (setq new-subtree nil)))

                    ;; No existing archive path
                    ;; Create a new entry
                    (let ((new-entry (bds/make-entry ol-subpath time)))
                      (puthash ol-subpath new-entry index)
                      (setq new-subtree (cons new-entry new-subtree))))

                  (setq ol-subpath (bds/pop-path-str ol-subpath)))
                )))
          )))
    table))

(defun bds/map-hash (fn htable)
  (let ((list nil)
        (last nil))
    (maphash (lambda (k v)
               (let* ((x (funcall fn k v))
                      (cell (cons x nil)))
                 (if last
                     (setcdr last cell)

                   (setq list cell))
                 (setq last cell)))
             htable)
    list))

(defun bds/clocktable/write-table (ipos tables params)
  (if (eq (plist-get params :scope) `file-with-archives)
      (let* ((merged-tables (--map (bds/merge-table-archive it)
                                   (-remove (lambda (tb) (string-suffix-p "_archive" (car tb))) tables))))
        (org-clocktable-write-default ipos
                                      merged-tables
                                      (plist-put params :scope `file)))

    (org-clocktable-write-default ipos tables params)))

;; Table:
;; (filename total-minutes (entries))
;; entry: (level entry-header-stuff ?? entry-minutes property-alist)
;; entry-header-stuff: #(entry-name line-start-pos line-end-pos face-properties?)
