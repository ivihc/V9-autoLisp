(defun c:PIPE3D ( / ss i ent edata pt1 pt2 diam radius h
                    old_osmode old_cmdecho old_dynmode
                    count *error* old_error)

  ;; Error handler - dam bao cleanup khi co loi hoac nhan ESC
  (setq old_error *error*)
  (defun *error* (msg)
    (if (tblsearch "UCS" "PIPE3D_TEMP")
      (progn
        (command "_.UCS" "_R" "PIPE3D_TEMP")
        (command "_.UCS" "_D" "PIPE3D_TEMP")))
    (if old_osmode (setvar "OSMODE" old_osmode))
    (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
    (if old_dynmode (setvar "DYNMODE" old_dynmode))
    (command "_.UNDO" "_E")
    (setq *error* old_error)
    (if (and msg (/= msg "Function cancelled"))
      (prompt (strcat "\n[Error] " msg)))
    (princ))

  (vl-load-com)

  ;; 1. Hoi duong kinh ong
  (initget 6)
  (setq diam (getreal "\nNhap duong kinh ong <50>: "))
  (if (null diam) (setq diam 50.0))
  (setq radius (/ diam 2.0))

  ;; 2. Quet chon cac LINE
  (prompt "\n--- QUET CHON CAC LINE CENTER ---")
  (setq ss (ssget '((0 . "LINE"))))
  (if (null ss)
    (progn
      (prompt "\n[!] Khong chon duoc LINE nao.")
      (setq *error* old_error)
      (exit)))

  ;; 3. Luu trang thai hien tai
  (setq old_osmode  (getvar "OSMODE"))
  (setq old_cmdecho (getvar "CMDECHO"))
  (setq old_dynmode (getvar "DYNMODE"))
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "DYNMODE" 0)

  ;; 4. Bat dau undo group
  (command "_.UNDO" "_BE")

  ;; 5. Xoa UCS tam neu con sot tu lan chay truoc (bi crash)
  (if (tblsearch "UCS" "PIPE3D_TEMP")
    (command "_.UCS" "_D" "PIPE3D_TEMP"))

  ;; 6. Luu UCS hien tai
  (command "_.UCS" "_S" "PIPE3D_TEMP")

  ;; 7. Lap qua tung LINE va tao cylinder
  (setq i 0 count 0)
  (repeat (sslength ss)
    (setq ent   (ssname ss i)
          edata (entget ent)
          pt1   (cdr (assoc 10 edata))
          pt2   (cdr (assoc 11 edata))
          h     (distance pt1 pt2))

    (if (> h 0.001)
      (progn
        ;; Doi UCS: goc tai pt1, truc +Z huong ve pt2
        (command "_.UCS" "_ZA" pt1 pt2)

        ;; Tao cylinder tai goc UCS (0,0,0) = pt1
        ;; Khong can rotate vi UCS da align voi LINE
        ;; Cylinder giu nguyen tinh primitive -> co grip keo
        (command "_.CYLINDER" '(0.0 0.0 0.0) radius h)

        ;; Restore UCS ve trang thai ban dau cho vong lap tiep theo
        (command "_.UCS" "_R" "PIPE3D_TEMP")

        (setq count (1+ count))
        (prompt (strcat "\n + Pipe " (itoa count)
                        " - D=" (rtos diam 2 2)
                        " L=" (rtos h 2 2) " mm"))))

    (setq i (1+ i)))

  ;; 8. Xoa UCS tam
  (command "_.UCS" "_D" "PIPE3D_TEMP")

  ;; 9. Ket thuc undo group
  (command "_.UNDO" "_E")

  ;; 10. Restore bien he thong
  (setvar "OSMODE" old_osmode)
  (setvar "CMDECHO" old_cmdecho)
  (setvar "DYNMODE" old_dynmode)

  ;; 11. Thong bao ket qua
  (prompt (strcat "\n-----------------------------"
                  "\nDA TAO: " (itoa count) " PIPE"
                  "\nDUONG KINH: " (rtos diam 2 2) " mm"))

  ;; 12. Hoi chuyen sang Realistic
  (initget "Yes No")
  (setq ans (getkword "\nChuyen sang visual style Realistic? [Yes/No] <Yes>: "))
  (if (or (null ans) (= ans "Yes"))
    (command "_.VSCURRENT" "_R"))

  (setq *error* old_error)
  (princ))