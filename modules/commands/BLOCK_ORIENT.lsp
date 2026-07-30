(vl-load-com)

;;=========================================================
;; LENH: BO - XOAY BLOCK QUA 24 HUONG (KHONG HIEN COMMAND)
;;=========================================================
(defun c:BO ( / *error* old_err old_vars old_ucs
               ent elist ins_pt original_data
               orientations i orient
               axis_name axis_vec ang_deg
               input_res done
               ax_line_x ax_line_y ax_line_z
               ax_len pt2)
  
  ;; 1. Ham xu ly loi
  (setq old_err *error*)
  (defun *error* (msg)
    (if ax_line_x (entdel ax_line_x))
    (if ax_line_y (entdel ax_line_y))
    (if ax_line_z (entdel ax_line_z))
    (if old_ucs (command "_.UCS" "_P"))
    (if old_vars (foreach v old_vars (setvar (car v) (cdr v))))
    (grtext -1 "")  ; Xoa status bar
    (setq *error* old_err)
    (princ)
  )
  
  ;; 2. Luu bien he thong
  (setq old_vars (list (cons 'OSMODE (getvar "OSMODE"))
                       (cons 'CMDECHO (getvar "CMDECHO"))))
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  
  ;; 3. Chon 1 block (chi hien prompt nay)
  (setq ent (car (entsel "\nChon block: ")))
  
  (if (or (null ent)
          (/= (cdr (assoc 0 (entget ent))) "INSERT"))
    (progn
      (foreach v old_vars (setvar (car v) (cdr v)))
      (setq *error* old_err)
      (princ)
      (exit)
    )
  )
  
  ;; 4. Lay thong tin block
  (setq elist (entget ent))
  (setq ins_pt (cdr (assoc 10 elist)))
  (setq original_data elist)
  
  ;; 5. SET UCS = WCS
  (command "_.UCS" "_W")
  (setq old_ucs t)
  
  ;; 6. Ve duong truc X Y Z
  (setq ax_len 500.0)
  
  (command "_.LINE" "_non" ins_pt "_non" (list (+ (car ins_pt) ax_len) (cadr ins_pt) (caddr ins_pt)) "")
  (setq ax_line_x (entlast))
  (command "_.CHPROP" ax_line_x "" "_C" "1" "")
  
  (command "_.LINE" "_non" ins_pt "_non" (list (car ins_pt) (+ (cadr ins_pt) ax_len) (caddr ins_pt)) "")
  (setq ax_line_y (entlast))
  (command "_.CHPROP" ax_line_y "" "_C" "3" "")
  
  (command "_.LINE" "_non" ins_pt "_non" (list (car ins_pt) (cadr ins_pt) (+ (caddr ins_pt) ax_len)) "")
  (setq ax_line_z (entlast))
  (command "_.CHPROP" ax_line_z "" "_C" "5" "")
  
  ;; 7. Tao danh sach 24 huong
  (setq orientations '(
    ("+X" 1 0 0 0)   ("+X" 1 0 0 90)   ("+X" 1 0 0 180)   ("+X" 1 0 0 270)
    ("+Y" 0 1 0 0)   ("+Y" 0 1 0 90)   ("+Y" 0 1 0 180)   ("+Y" 0 1 0 270)
    ("+Z" 0 0 1 0)   ("+Z" 0 0 1 90)   ("+Z" 0 0 1 180)   ("+Z" 0 0 1 270)
    ("-X" -1 0 0 0)  ("-X" -1 0 0 90)  ("-X" -1 0 0 180)  ("-X" -1 0 0 270)
    ("-Y" 0 -1 0 0)  ("-Y" 0 -1 0 90)  ("-Y" 0 -1 0 180)  ("-Y" 0 -1 0 270)
    ("-Z" 0 0 -1 0)  ("-Z" 0 0 -1 90)  ("-Z" 0 0 -1 180)  ("-Z" 0 0 -1 270)
  ))
  
  ;; 8. Vong lap chinh
  (setq i 0 done nil)
  
  (while (not done)
    ;; Lay huong hien tai
    (setq orient (nth i orientations))
    (setq axis_name (nth 0 orient))
    (setq axis_vec (list (nth 1 orient) (nth 2 orient) (nth 3 orient)))
    (setq ang_deg (nth 4 orient))
    
    ;; RESTORE BLOCK VE TRANG THAI GOC
    (entmod original_data)
    (entupd ent)
    
    ;; XOAY TU TRANG THAI GOC DEN HUONG MOI
    (if (> ang_deg 0)
      (progn
        (setq pt2 (list (+ (car ins_pt) (car axis_vec))
                        (+ (cadr ins_pt) (cadr axis_vec))
                        (+ (caddr ins_pt) (caddr axis_vec))))
        
        (command "_.ROTATE3D" ent "" 
                 "_2" 
                 ins_pt 
                 pt2
                 (rtos ang_deg 2 0))
        (while (> (getvar "CMDACTIVE") 0) (command ""))
      )
    )
    
    ;; Hien thi tren STATUS BAR (khong hien command)
    (grtext -1 (strcat "  [" (itoa (1+ i)) "/24] " axis_name " " (itoa ang_deg) "°  |  N:Next  P:Prev  Enter:OK  Esc:Cancel  "))
    
    ;; CHO INPUT
    (setq input_res (grread t 2 0))
    
    (cond
      ;; N - NEXT
      ((and (= (car input_res) 2)
            (or (= (cadr input_res) 78) (= (cadr input_res) 110)))
       (if (< i 23)
         (setq i (1+ i))
       )
      )
      
      ;; P - PREVIOUS
      ((and (= (car input_res) 2)
            (or (= (cadr input_res) 80) (= (cadr input_res) 112)))
       (if (> i 0)
         (setq i (1- i))
       )
      )
      
      ;; Enter - CONFIRM
      ((and (= (car input_res) 2) (= (cadr input_res) 13))
       (setq done t)
      )
      
      ;; Esc - CANCEL
      ((= (car input_res) 25)
       (entmod original_data)
       (entupd ent)
       (setq done t)
      )
    )
  )
  
  ;; 9. Xoa duong truc
  (if ax_line_x (entdel ax_line_x))
  (if ax_line_y (entdel ax_line_y))
  (if ax_line_z (entdel ax_line_z))
  
  ;; 10. KHOI PHUC UCS CU
  (command "_.UCS" "_P")
  
  ;; 11. Xoa status bar
  (grtext -1 "")
  
  ;; 12. Ket thuc
  (foreach v old_vars (setvar (car v) (cdr v)))
  (setq *error* old_err)
  (princ)
)

(princ "\n[Tool] BO loaded.")
(princ)
