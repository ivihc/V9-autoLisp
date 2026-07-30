(vl-load-com)

;;=========================================================
;; Command: PIPE3D_CREATE
;;=========================================================
(defun c:PIPE3D_CREATE ( / ss i ent edata pt1 pt2 diam radius h
                          old_osmode old_cmdecho old_dynmode
                          count *error* old_error ans)

  ;; 1. Hàm xử lý lỗi (Đảm bảo Ctrl+Z hoạt động ngay cả khi bấm ESC)
  (setq old_error *error*)
  (defun *error* (msg)
    (command "_.UNDO" "_E") ; Đóng nhóm UNDO nếu lệnh bị hủy
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if old_dynmode (setvar "DYNMODE" old_dynmode))
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (prompt (strcat "\n[Error] " msg))
    )
    (setq *error* old_error)
    (princ)
  )

  ;; 2. Nhập dữ liệu
  (initget 6)
  (setq diam (getreal "\nNhap duong kinh ong <50>: "))
  (if (null diam) (setq diam 50.0))
  (setq radius (/ diam 2.0))

  ;; 3. Chọn LINE
  (prompt "\n--- QUET CHON CAC LINE CENTER ---")
  (setq ss (ssget '((0 . "LINE"))))
  
  (if (null ss)
    (prompt "\n[!] Khong chon duoc LINE nao. Lenh ket thuc.")
    (progn
      ;; 4. Lưu và tắt biến hệ thống
      (setq old_osmode  (getvar "OSMODE"))
      (setq old_cmdecho (getvar "CMDECHO"))
      (setq old_dynmode (getvar "DYNMODE"))
      (setvar "OSMODE" 0)
      (setvar "CMDECHO" 0)
      (setvar "DYNMODE" 0)

      ;; 5. Bắt đầu nhóm UNDO (Ctrl+Z 1 lần)
      (command "_.UNDO" "_BE")

      (setq i 0 count 0)
      (repeat (sslength ss)
        (setq ent   (ssname ss i)
              edata (entget ent)
              pt1   (cdr (assoc 10 edata))
              pt2   (cdr (assoc 11 edata))
              h     (distance pt1 pt2))

        (if (> h 0.001)
          (progn
            ;; Vẽ Cylinder theo trục đi qua 2 điểm (Không cần đổi UCS)
            (command "_.CYLINDER" "_non" pt1 radius "_AXis" "_non" pt1 "_non" pt2)
            ;; Đảm bảo lệnh kết thúc dứt khoát
            (while (> (getvar "CMDACTIVE") 0) (command ""))
            
            (setq count (1+ count))
            (prompt (strcat "\n + Pipe " (itoa count)
                            " - D=" (rtos diam 2 2)
                            " L=" (rtos h 2 2) " mm"))
          )
        )
        (setq i (1+ i))
      )

      ;; 6. Kết thúc nhóm UNDO
      (command "_.UNDO" "_E")

      ;; 7. Trả lại biến hệ thống
      (setvar "OSMODE" old_osmode)
      (setvar "CMDECHO" old_cmdecho)
      (setvar "DYNMODE" old_dynmode)

      ;; 8. Thông báo và hỏi Visual Style
      (prompt (strcat "\n-----------------------------"
                      "\nDA TAO: " (itoa count) " PIPE"
                      "\nDUONG KINH: " (rtos diam 2 2) " mm"))

      (initget "Yes No")
      (setq ans (getkword "\nChuyen sang visual style Realistic? [Yes/No] <Yes>: "))
      (if (or (null ans) (= ans "Yes"))
        (command "_.VSCURRENT" "_R")
      )
    )
  )

  (setq *error* old_error)
  (princ)
)

;; Alias cũ
(defun c:PIPE3D () (c:PIPE3D_CREATE) (princ))

(princ "\n[Pipe] PIPE3D_CREATE module loaded.")
(princ)
