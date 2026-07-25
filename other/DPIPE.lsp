(defun c:DPIPE ( / diam radius pt1 pt2 h count 
                   old_osmode old_cmdecho old_solidhist grip_mode
                   old_clayer old_color old_ltype old_lw)
  
  ;; 1. Hoi duong kinh ong
  (initget 6)
  (setq diam (getreal "\nNhap duong kinh ong <50>: "))
  (if (null diam) (setq diam 50.0))
  (setq radius (/ diam 2.0))
  
  ;; 2. Hoi che do grip
  (initget "Yes No")
  (setq grip_mode (getkword "\nCho phep keo grip sau khi ve? [Yes/No] <Yes>: "))
  (if (null grip_mode) (setq grip_mode "Yes"))
  
  ;; 3. Hoi diem dau tien
  (setq pt1 (getpoint "\nChon diem dau tien: "))
  (if (null pt1) (progn (prompt "\n[!] Lenh bi huy.") (exit)))
  
  ;; 4. Luu trang thai
  (setq old_osmode    (getvar "OSMODE"))
  (setq old_cmdecho   (getvar "CMDECHO"))
  (setq old_solidhist (getvar "SOLIDHIST"))
  (setq old_clayer    (getvar "CLAYER"))
  (setq old_color     (getvar "CECOLOR"))
  (setq old_ltype     (getvar "CELTYPE"))
  (setq old_lw        (getvar "CELWEIGHT"))
  
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  
  (if (= grip_mode "Yes")
    (setvar "SOLIDHIST" 1)
    (setvar "SOLIDHIST" 0)
  )
  
  ;; Bat dau undo group
  (command "_.UNDO" "_BE")
  
  ;; 5. Vong lap ve cac doan ong + center line
  (setq count 0)
  (while (setq pt2 (getpoint pt1 "\nChon diem tiep theo [Enter de ket thuc]: "))
    
    (setq h (distance pt1 pt2))
    
    (if (> h 0.001)
      (progn
        ;; Tao cylinder (tren layer hien tai)
        (command "_.CYLINDER" pt1 radius "_AXis" pt1 pt2)
        (while (> (getvar "CMDACTIVE") 0)
          (command "")
        )
        
        ;; [NEW] Tao center line tren LAYER 0 - HOAN TOAN BYLAYER
        (setvar "CLAYER" "0")
        (setvar "CECOLOR" "BYLAYER")
        (setvar "CELTYPE" "BYLAYER")
        (setvar "CELWEIGHT" -1)
        
        (command "_.LINE" "_non" pt1 "_non" pt2 "")
        (while (> (getvar "CMDACTIVE") 0)
          (command "")
        )
        
        (setq count (1+ count))
        (prompt (strcat "\n + Pipe " (itoa count) " L=" (rtos h 2 2) " mm"))
        
        ;; Diem cuoi = diem dau doan tiep theo
        (setq pt1 pt2)
      )
      (prompt "\n[!] Doan qua ngan, bo qua.")
    )
  )
  
  ;; 6. Ket thuc undo
  (command "_.UNDO" "_E")
  
  ;; 7. Restore bien he thong
  (setvar "OSMODE" old_osmode)
  (setvar "CMDECHO" old_cmdecho)
  (setvar "SOLIDHIST" old_solidhist)
  (setvar "CLAYER" old_clayer)
  (setvar "CECOLOR" old_color)
  (setvar "CELTYPE" old_ltype)
  (setvar "CELWEIGHT" old_lw)
  
  ;; 8. Thong bao ket qua
  (if (> count 0)
    (prompt (strcat "\n-----------------------------"
                    "\nDA VE: " (itoa count) " PIPE + " (itoa count) " CENTER LINE"
                    "\nDUONG KINH: " (rtos diam 2 2) " mm"
                    "\nCENTER LINE: Layer 0 - Hoan toan ByLayer"
                    "\nCHINH SUA: Ctrl+1 (Properties) de sua Height/Radius"))
    (prompt "\n[!] Khong ve duoc doan nao.")
  )
  (princ)
)