;; Change to 'entities'. Make buffer local? Maybe build it from an org-mode
;; table or tree?
(defvar bds-my-characters
  (make-hash-table))

;; (defmacro make-hash-from (&rest pairs)
;;   `(let ((myhash (make-hash-table)))
;;      ,@(-map
;;         (lambda (kv) (list 'puthash (car kv) (cadr kv) 'myhash))
;;         (-partition 2 pairs))
;;      myhash))

(defun make-hash-from (pairs)
  (let ((myhash (make-hash-table)))
    (-each (-partition 2 pairs)
      (lambda (kv)
        (puthash (car kv) (cadr kv) myhash)))
    myhash))

(defun add-character (char-id &rest pairs)
  (puthash char-id (make-hash-from pairs) bds-my-characters))

(defun update-character (char-id &rest pairs)
  (if-let ((charhash (gethash char-id bds-my-characters)))
      (-each (-partition 2 pairs)
        (lambda (kv)
          (puthash (car kv) (cadr kv) charhash)))

    (apply #'add-character char-id pairs)))

(add-character 'milo :first-name "Milo")
(add-character 'tock :first-name "Tock")
;; (add-character ')

(defun bds/get-pos ()
  (if (region-active-p) (region-beginning) (point)))

(defun range-for-prop-at (pos property)
  (if-let ((val (get-text-property pos property)))
      (list  (previous-single-char-property-change (1+ pos) property)
             (next-single-char-property-change pos property)
             val)
    nil))

(defun range-for-prop (property)
  (range-for-prop-at (bds/get-pos) property))

(defun each-prop-range (prop fn)
  "Applies a function FN to each range of text where the property PROP is not nil, starting at the current point."
  (let ((pos (set-marker (make-marker) (point)))
        (max (point-max-marker))
        range)
    (while (< pos max)
      (setq range (range-for-prop-at pos prop))
      (when range
        (apply fn range))
      (set-marker pos
                  (next-single-char-property-change
                   (if range (nth 1 range) pos) prop)))
    (set-marker pos nil)
    (set-marker max nil)))

(defmacro if-property-range (prop start-sym end-sym val-sym then &rest else)
  "If the text property PROP is set at the current point, binds variables to the start, end, and property value for the range and executes the THEN clause. Otherwise, executes ELSE."
  `(if-let ((range (range-for-prop ,prop)))
       (let ((,val-sym (nth 2 range))
             (,start-sym (nth 0 range))
             (,end-sym (nth 1 range)))
         ,then)

     (progn ,@else)))

(defun update-text (prop lookup-table)
  (if-property-range
   prop
   start end lookup-id
   (when-let ((entity (and lookup-id (gethash lookup-id lookup-table))) )
     (let* ((pos (bds/get-pos))
            (props (text-properties-at start))
            (field (get-text-property start :character-field))
            (field-value (gethash field entity "??"))
            (offset (- pos start))
            (restore-region? (region-active-p)))
       (delete-region start end)
       (goto-char start)
       (insert field-value)
       (set-text-properties start (point) props)
       (goto-char (max start (min (1- (point)) pos)))))))

(defun update-character-text ()
  (update-text :character bds-my-characters))

(defun select-text-with-prop (property)
  (when-let ((result (range-for-prop property)))
    (destructuring-bind (start end val)
        result
      (goto-char end)
      (push-mark start)
      (setq mark-active t)
      val)))


;; Experimenting...
(defmacro in-other-win (&rest body)
  `(let ((w (get-buffer-window)))
     (winum-select-window-1)
     (let ((result (progn ,@body)))
       (select-window w)
       result))  )

(in-other-win
 (update-character 'milo :first-name "Milo!")
 (beginning-of-buffer)
 (let ((entities (list)))
   (each-prop-range
    :character
    (lambda (start end val)
      (goto-char start)
      (update-character-text)
      ;(push (buffer-substring start end) entities)
      ))
   entities))

(in-other-win
 ;; (text-properties-at (point))
 (update-character 'milo :first-name "Milos")
 (update-character-text)
 ;; (range-for-prop :character) ;; (update-text)
 ;; (select-text-with-prop :character)
 ;; (when (region-active-p)
 ;;   (buffer-substring (region-beginning) (region-end)))
 )
(in-other-win
 ;; (bds/get-pos)
 (get-text-property (bds/get-pos) :character)
 (range-for-prop :character)
)
(in-other-win
 ;; (set-text-properties 20 30 '(:brian-test :hello))
                                        ;(get-char-property 29 :brian-test)
 ;; (length (buffer-substring 20 30))
 (- (region-end) (region-beginning))
 (buffer-substring (region-beginning) (region-end))
 )

(in-other-win
 (select-text-with-prop :character)
 (buffer-substring (region-beginning) (region-end)))

(in-other-win
 (bds/get-pos)
 )

(in-other-win
 (add-text-properties (region-beginning) (region-end)
                      '(:character milo
                                   :character-field :first-name)))
;; (in-other-win
;;  (add-text-properties (region-beginning) (region-end)
;;                       '(:character milo
;;                         :character-field :first-name)))
