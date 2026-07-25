(vl-load-com)

;;=========================================================
;; Command: PIPE_SLOPE (Vẽ ống có độ nghiêng C)
;;=========================================================
(defun c:PIPE_SLOPE ( / *error* old_vars diam slope_C dir_mode len_mode 
                       pt1 pt2 pt_end dist2D angle2D L0_new dZ 
                       count old_osmode old_cmdecho old_solidhist)

  ;; 1. Error Handler cục bộ
  (setq old_err *error*)
  (defun *error* (msg)
    (if (and msg (not (member msg '("Function cancelled" "quit / exit abort"))))
      (prompt (strcat "\n[Pipe] Error: " msg)))
    (if old_vars
      (progn
        (setvar "OSMODE" (cdr (assoc 'OSMODE old_vars)))
        (setvar "CMDECHO" (cdr (assoc 'CMDECHO old_vars)))
        (setvar "SOLIDHIST" (cdr (assoc 'SOLIDHIST old_vars)))
      )
    )
    (setq *error* old_err)
    (princ)
  )

  ;; 2. Lưu biến hệ thống
  (setq old_vars (list (cons 'OSMODE (getvar "OSMODE"))
                       (cons 'CMDECHO (getvar "CMDECHO"))
                       (cons 'SOLIDHIST (getvar "SOLIDHIST"))))
  
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "SOLIDHIST" 1) ;; Để hiển thị lịch sử Solid nếu cần chỉnh sửa

  ;; 3. Nhập thông số chung
  (initget 6)
  (setq diam (getreal "\nNhap duong kinh ong <50>: "))
  (if (null diam) (setq diam 50.0))
  (setq radius (/ diam 2.0))

  (prompt "\n--- CAU HINH DO NGHIENG ---\n")
  (initget "75 100")
  (setq slope_C (getkword "\nChon do nghieng C [1/75 or 1/100] <1/75>: "))
  (if (null slope_C) (setq slope_C "75"))
  (setq C_val (if (= slope_C "75") (/ 1.0 75.0) (/ 1.0 100.0)))

  (setq count 0)
  (command "_.UNDO" "_BE")

  ;; 4. Vòng lặp vẽ chính
  (while (setq pt1 (getpoint "\nChon diem bat dau (Enter de thoat): "))
    
    ;; Lấy điểm định hướng (chỉ lấy góc trong mặt phẳng XY)
    (setq pt2 (getpoint pt1 "\nChon huong di (Enter de thoat): "))
    (if (null pt2) (setq pt1 nil) ;; Thoát nếu user bấm Enter
      (progn
        ;; Tính khoảng cách và góc trong mặt phẳng 2D (bỏ qua Z)
        (setq dist2D (distance (list (car pt1) (cadr pt1) 0.0) 
                               (list (car pt2) (cadr pt2) 0.0)))
        
        ;; TRƯỜNG HỢP 1: ĐƯỜNG ỐNG ĐỨNG (PHƯƠNG Z)
        (if (< dist2D 0.001) 
          (progn
            (setq h (abs (- (caddr pt2) (caddr pt1))))
            (if (> h 0.001)
              (progn
                (command "_.CYLINDER" pt1 radius "_AXis" pt1 pt2)
                (while (> (getvar "CMDACTIVE") 0) (command ""))
                (command "_.LINE" "_non" pt1 "_non" pt2 "")
                (while (> (getvar "CMDACTIVE") 0) (command ""))
                (setq count (1+ count))
                (prompt (strcat "\n + Pipe " (itoa count) " (Vertical) L=" (rtos h 2 2)))
                (setq pt1 pt2) ;; Tiếp tục vẽ từ điểm cuối
              )
              (prompt "\n[!] Doan qua ngan, bo qua.")
            )
          )
          
          ;; TRƯỜNG HỢP 2: ĐƯỜNG ỐNG NGHIÊNG HOẶC NẰM NGANG
          (progn
            ;; Hỏi hướng lên/xuống
            (initget "Up Down Horizontal")
            (setq dir_mode (getkword "\nHuong di [Up/Down/Horizontal] <Horizontal>: "))
            (if (null dir_mode) (setq dir_mode "Horizontal"))

            ;; Hỏi cách nhập chiều dài
            (initget "L0 L")
            (setq len_mode (getkword "\nNhap chieu dai [L0 (chieu dai ngang)/L (chieu dai thuc)] <Enter de su dung khoang cach da chon>: "))

            ;; Tính toán L0_moi (Chiều dài hình chiếu ngang thực tế sẽ vẽ)
            (cond
              ;; User nhập L (Cạnh huyền)
              ((= len_mode "L")
               (initget 6)
               (setq L_input (getreal "\nNhap chieu dai thuc L (canh huyen): "))
               (if L_input
                 ;; Công thức: L0 = L / sqrt(1 + C^2)
                 (setq L0_new (/ L_input (sqrt (+ 1.0 (* C_val C_val)))))
                 (setq L0_new dist2D) ;; Nếu user hủy, dùng khoảng cách cũ
               )
              )
              ;; User nhập L0 (Cạnh góc vuông)
              ((= len_mode "L0")
               (initget 6)
               (setq L0_input (getreal "\nNhap chieu dai ngang L0: "))
               (if L0_input
                 (setq L0_new L0_input)
                 (setq L0_new dist2D)
               )
              )
              ;; User bấm Enter (Dùng khoảng cách giữa pt1 và pt2)
              (t
               (setq L0_new dist2D)
              )
            )

            ;; Tính Delta_H (Độ cao chênh lệch)
            (setq dZ (* L0_new C_val))
            (cond
              ((= dir_mode "Up") (setq dZ dZ))
              ((= dir_mode "Down") (setq dZ (- dZ)))
              (t (setq dZ 0.0)) ;; Horizontal
            )

            ;; Tính toán điểm cuối Pt_end (3D)
            ;; Vector đơn vị trong mặt phẳng XY
            (setq angle2D (angle (list (car pt1) (cadr pt1)) (list (car pt2) (cadr pt2))))
            (setq pt_end (list 
                           (+ (car pt1) (* L0_new (cos angle2D)))
                           (+ (cadr pt1) (* L0_new (sin angle2D)))
                           (+ (caddr pt1) dZ)
                         ))

            ;; Vẽ Ống và Line
            (command "_.CYLINDER" pt1 radius "_AXis" pt1 pt_end)
            (while (> (getvar "CMDACTIVE") 0) (command ""))

            (command "_.LINE" "_non" pt1 "_non" pt_end "")
            (while (> (getvar "CMDACTIVE") 0) (command ""))

            (setq count (1+ count))
            (prompt (strcat "\n + Pipe " (itoa count) " L0=" (rtos L0_new 2 2) 
                            " | dZ=" (rtos dZ 2 2) 
                            " | L_3D=" (rtos (distance pt1 pt_end) 2 2)))
            
            ;; Cập nhật pt1 để vẽ đoạn tiếp theo
            (setq pt1 pt_end)
          )
        )
      )
    )
  )

  (command "_.UNDO" "_E")
  
  ;; Restore biến hệ thống
  (foreach var_pair old_vars
    (setvar (car var_pair) (cdr var_pair))
  )

  (if (> count 0)
    (prompt (strcat "\n-----------------------------"
                    "\nDA VE: " (itoa count) " PIPE"
                    "\nDUONG KINH: " (rtos diam 2 2) " mm"
                    "\nDO NGHIENG: 1/" slope_C))
    (prompt "\n[!] Khong ve duoc doan nao."))
  
  (setq *error* old_err)
  (princ)
)

;; Lệnh tắt
(defun c:PSLOPE () (c:PIPE_SLOPE) (princ))

(princ "\n[Pipe] PIPE_SLOPE module loaded. Type PSLOPE or PIPE_SLOPE to start.")
(princ)