;;=========================================================
;; CHƯƠNG TRÌNH 2: CỘNG GIÁ TRỊ DIMENSION (OVERRIDE)
;; Lệnh: TONGDIM
;;=========================================================

(defun c:TONGDIM ( / *error* old_osmode ss i ent ed ov_txt actual_val val total_sum count_dim count_ov)
  (defun *error* (msg)
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*EXIT*")))
      (prompt (strcat "\n[Lỗi] " msg)))
    (princ)
  )

  (setq old_osmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  (prompt "\n=== CHUONG TRINH CONG GIA TRI DIMENSION ===")
  (prompt "\nChon cac DIMENSION de cong gia tri: ")
  
  (setq ss (ssget '((0 . "DIMENSION"))))
  
  (if (not ss)
    (prompt "\n[!] Khong chon duoc DIMENSION nao.")
    (progn
      (setq total_sum 0.0 count_dim 0 count_ov 0 i -1)
      
      (while (setq ent (ssname ss (setq i (1+ i))))
        (setq ed (entget ent))
        (setq ov_txt (cdr (assoc 1 ed)))      ;; Text Override (Group code 1)
        (setq actual_val (cdr (assoc 42 ed))) ;; Gia tri do thuc te (Group code 42)
        
        (setq count_dim (1+ count_dim))
        
        (if (and ov_txt (/= ov_txt ""))
          (progn
            ;; Co Override: Try to extract number from string
            (setq val (atof ov_txt)) 
            ;; Neu atof tra ve 0 nhung chuoi khong phai "0", co the chuoi co ky tu dac biet
            ;; Ta dung distof de lay so dau tien
            (if (and (= val 0.0) (/= (vl-string-left-trim "0 " ov_txt) ""))
              (setq val (car (distof ov_txt 2))) ;; Mode 2: Decimal
            )
            (if val
              (setq total_sum (+ total_sum val))
              (setq total_sum (+ total_sum actual_val)) ;; Fallback
            )
            (setq count_ov (1+ count_ov))
          )
          (progn
            ;; Khong co Override: Lay gia tri thuc te
            (setq total_sum (+ total_sum actual_val))
          )
        )
      )
      
      ;; Hiển thị kết quả
      (prompt "\n\n--- KET QUA TONG DIMENSION ---")
      (prompt (strcat "\n  Tong so DIM da chon : " (itoa count_dim)))
      (prompt (strcat "\n  So DIM co Override: " (itoa count_ov)))
      (prompt (strcat "\n  => TONG GIA TRI    : " (rtos total_sum 2 2)))
      (prompt "\n----------------------------\n")
    )
  )

  (setvar "OSMODE" old_osmode)
  (princ)
)
(princ "\nDa load lenh TONGDIM (Cong gia tri Dim). Gõ TONGDIM de chay.")
(princ)
