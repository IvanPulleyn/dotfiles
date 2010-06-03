;;; timeclock-janrain.el -- Add-ons for timeclock.el and timeclock-x.el
;;
;; Version: 1.2
;;
;;; History:
;;
;; 1.1  Initial revision.
;; 1.2  Added "by day/by project" report generator.  Sort the output of
;;      "by-project".
;; 1.3  Fixed a startup issue and a bug.
;;
;;; Todo:
;;
;; Add a "by project/by day" report generator.
;;
;;; Code:

(provide 'timeclock-janrain)
(require 'timeclock-x)

;; timeclock-x acts weird if this dir doesn't exist:
(mkdir "~/.timeclock/" t)
;; Must do this before timeclock-initialize to avoid the stupid prompt:
(timeclock-query-project-on t)
(timeclock-initialize)

(defun timeclock-generate-project-list ()
  "Generate a list of projects."
  (interactive)
  (dolist (proj (timeclock-by-project))
    (insert (car proj) "\n")))

(defun timeclock-generate-report-by-project (&optional fmt)
  "Generate a report of hours spent on each project based on the
current timelog file.  By default, the report is in plain text; a
prefix argument adds additional markup: 1 results in table
output, 2 in HTML."
  (interactive "P")
  (setq fmt (timeclock-normalize-fmt fmt))
  (let ((data (timeclock-by-project)))
    ;; Output the header.
    (choose fmt
            '()
            '(progn
               (table-insert 2 (+ (length data) 1))
               (timeclock-insert-header-row fmt "Project" "Time spent"))
            '(progn
               (insert "<table border=1 cellpadding=3>")
               (timeclock-insert-header-row fmt "Project" "Time spent")))
    ;; Output the body.
    (dolist (proj data)
      (timeclock-insert-row fmt
                            (car proj)
                            (timeclock-seconds-to-string (car (cdr proj)))))
    ;; Output the footer.
    (choose fmt
            '()
            '()
            '(insert "\n</table>"))))

(defun timeclock-generate-report-by-day-by-project (&optional fmt)
  "Generate a report by day of hours spent on each project based
on the current timelog file.  By default, the report is in plain
text; a prefix argument adds additional markup: 1 results in
table output, 2 in HTML."
  (interactive "P")
  (setq fmt (timeclock-normalize-fmt fmt))
  (let ((data (timeclock-by-day-by-project)) (rows 0) tmp)
    ;; Calculate number of rows.
    (setq tmp data)
    (dolist (day tmp)
      (dolist (entry (cdr day))
        (setq rows (+ rows 1))))
    ;; Output the header.
    (choose fmt
            '()
            '(progn
               (table-insert 3 (+ rows 1))
               (timeclock-insert-header-row fmt "Day" "Project" "Time spent"))
            '(progn
               (insert "<table border=1 cellpadding=3>")
               (timeclock-insert-header-row fmt "Day" "Project" "Time spent")))
    ;; Output the body.
    (dolist (day data)
      (let ((first t) (curr-day (car day)))
        (dolist (entry (cdr day))
          (if first
              (progn
                (timeclock-insert-row fmt
                                      curr-day
                                      (car entry)
                                      (timeclock-seconds-to-string (car (cdr entry))))
                (setq first nil))
            (timeclock-insert-row fmt
                                  (choose fmt "          " "" "")
                                  (car entry)
                                  (timeclock-seconds-to-string (car (cdr entry))))))))
    ;; Output the footer.
    (choose fmt
            '()
            '()
            '(insert "\n</table>"))))

(defun timeclock-normalize-fmt (fmt)
  "Normalize the format selector."
  (if fmt () (setq fmt 0))
  (if (> fmt 2) (setq fmt 2))
  (if (< fmt 0) (setq fmt 0))
  fmt)

(defun timeclock-by-project (&optional project-alist)
  "Return the times spent summed up by project as an alist of
alists in the format (PROJECT TIME)."
  (let (sums)
    (dolist (proj (or project-alist (timeclock-project-alist)) sums)
      (let ((p (car proj)) (s (timeclock-entry-list-length (cdr proj))))
        (setq sums (cons (list p s) sums))))
    (sort sums (lambda (a b) (string< (car a) (car b))))))

(defun timeclock-by-day-by-project (&optional day-alist)
  "Return the times spent by day summed up by project as an alist
of alists in the format (DAY (PROJECT TIME) (PROJECT TIME) ...)."
  (let (result)
    (dolist (day (or day-alist (timeclock-day-alist)))
      (let ((sums (make-hash-table :test 'equal :size 13)) ptimes)
        ;; Build a hash of total project times for this day.
        (dolist (entry (cdr (cdr day)))
          (let (sum
                (proj (timeclock-entry-project entry))
                (len (timeclock-entry-length entry)))
            (setq sum (gethash proj sums 0))
            (puthash proj (+ sum len) sums)))
        ;; Convert the hash into a list.
        (maphash (lambda (k v) (setq ptimes (cons (list k v) ptimes))) sums)
        ;; Sort the list
        (setq ptimes (sort ptimes (lambda (a b) (string< (car a) (car b)))))
        ;; Add an entry for the current day to the result.
        (setq result (cons (cons (car day) ptimes) result))))
    ;; Sort the result.
    (sort result (lambda (a b) (string> (car a) (car b))))))

(defun timeclock-insert-row (fmt &rest vals)
  (choose fmt
          '(insert "\n")                ; text
          '()                           ; table
          '(insert "\n<tr>"))           ; html
  (let ((first t))
    (dolist (val vals)
      (timeclock-insert-cell fmt first val)
      (setq first nil)))
  (choose fmt
          '()                           ; text
          '()                           ; table
          '(insert "\n</tr>")))         ; html

(defun timeclock-insert-cell (fmt first text)
  (if first
      (choose fmt
              '()                       ; text
              '()                       ; table
              '(insert "\n <td>"))      ; html
    (choose fmt
            '(insert "\t")              ; text
            '()                         ; table
            '(insert "\n <td>")))       ; html
  (timeclock-insert-text fmt text)
  (choose fmt
          '()                           ; text
          '(table-forward-cell)         ; table
          '(insert "</td>")))           ; html

(defun timeclock-insert-header-row (fmt &rest vals)
  (choose fmt
          '(insert "\n")                ; text
          '()                           ; table
          '(insert "\n<tr>"))           ; html
  (let ((first t))
    (dolist (val vals)
      (timeclock-insert-header-cell fmt first val)
      (setq first nil)))
  (choose fmt
          '()                           ; text
          '()                           ; table
          '(insert "\n</tr>")))         ; html

(defun timeclock-insert-header-cell (fmt first text)
  (if first
      (choose fmt
              '()                       ; text
              '()                       ; table
              '(insert "\n <th>"))      ; html
    (choose fmt
            '(insert "\t")              ; text
            '()                         ; table
            '(insert "\n <th>")))       ; html
  (timeclock-insert-text fmt text)
  (choose fmt
          '()                           ; text
          '(table-forward-cell)         ; table
          '(insert "</th>")))           ; html

(defun timeclock-insert-text (fmt text)
  (choose fmt
          '(insert text)                  ; text
          '(table--cell-insert-char text) ; table
          '(insert text)))                ; html

(defun choose (fmt &rest alt)
  (eval (nth fmt alt)))
