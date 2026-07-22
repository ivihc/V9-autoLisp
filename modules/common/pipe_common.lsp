(vl-load-com)

;;=========================================================
;; Shared configuration and helpers
;;=========================================================
(if (not (boundp 'Pipe:*Ver*)) (setq Pipe:*Ver* "3.4"))
(if (not (boundp 'Pipe:*Sep*)) (setq Pipe:*Sep* ","))

(defun Pipe:ShowMessage (m) (prompt (strcat "\n[Pipe] " m)))
(defun Pipe:ShowWarning (m) (prompt (strcat "\n[Pipe] Warning: " m)))

(defun *error* (msg)
  (if (and msg (not (member msg '("Function cancelled" "quit / exit abort"))))
    (prompt (strcat "\n[Pipe] Error: " msg)))
  (princ))

(defun Pipe:WriteCsvFile (fn header data / path fp)
  (if (setq path (getfiled "Save CSV File" (strcat "D:\\" fn) "csv" 1))
    (if (setq fp (open path "w"))
      (progn
        (write-line header fp)
        (foreach line data (write-line line fp))
        (close fp)
        path))))

(defun Pipe:Msg (m) (Pipe:ShowMessage m))
(defun Pipe:Warn (m) (Pipe:ShowWarning m))
(defun Pipe:SaveCSV (fn header data) (Pipe:WriteCsvFile fn header data))

(princ "\n[Pipe] Common module loaded.")
(princ)
