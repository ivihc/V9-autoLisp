(vl-load-com)

;;=========================================================
;; Shared configuration and helpers
;;=========================================================
(if (not (boundp 'Pipe:*Ver*)) (setq Pipe:*Ver* "3.4"))
(if (not (boundp 'Pipe:*Sep*)) (setq Pipe:*Sep* ","))

(defun Pipe:Msg (m) (prompt (strcat "\n[Pipe] " m)))
(defun Pipe:Warn (m) (prompt (strcat "\n[Pipe] Warning: " m)))

(defun *error* (msg)
  (if (and msg (not (member msg '("Function cancelled" "quit / exit abort"))))
    (prompt (strcat "\n[Pipe] Error: " msg)))
  (princ))

(defun Pipe:SaveCSV (fn header data / path fp)
  (if (setq path (getfiled "Save CSV File" (strcat "D:\\" fn) "csv" 1))
    (if (setq fp (open path "w"))
      (progn
        (write-line header fp)
        (foreach line data (write-line line fp))
        (close fp)
        path))))

(princ "\n[Pipe] Common module loaded.")
(princ)
