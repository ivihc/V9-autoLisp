;;=========================================================
;; Command: PIPE3D_CREATE
;;=========================================================
(defun c:PIPE3D_CREATE ( / ss i ent edata pt1 pt2 diam radius h
                          old_osmode old_cmdecho old_dynmode
                          count *error* old_error)

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

  (initget 6)
  (setq diam (getreal "\nNhap duong kinh ong <50>: "))
  (if (null diam) (setq diam 50.0))
  (setq radius (/ diam 2.0))

  (prompt "\n--- QUET CHON CAC LINE CENTER ---")
  (setq ss (ssget '((0 . "LINE"))))
  (if (null ss)
    (progn
      (prompt "\n[!] Khong chon duoc LINE nao.")
      (setq *error* old_error)
      (exit)))

  (setq old_osmode  (getvar "OSMODE"))
  (setq old_cmdecho (getvar "CMDECHO"))
  (setq old_dynmode (getvar "DYNMODE"))
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "DYNMODE" 0)

  (command "_.UNDO" "_BE")

  (if (tblsearch "UCS" "PIPE3D_TEMP")
    (command "_.UCS" "_D" "PIPE3D_TEMP"))
  (command "_.UCS" "_S" "PIPE3D_TEMP")

  (setq i 0 count 0)
  (repeat (sslength ss)
    (setq ent   (ssname ss i)
          edata (entget ent)
          pt1   (cdr (assoc 10 edata))
          pt2   (cdr (assoc 11 edata))
          h     (distance pt1 pt2))

    (if (> h 0.001)
      (progn
        (command "_.UCS" "_ZA" pt1 pt2)
        (command "_.CYLINDER" '(0.0 0.0 0.0) radius h)
        (command "_.UCS" "_R" "PIPE3D_TEMP")
        (setq count (1+ count))
        (prompt (strcat "\n + Pipe " (itoa count)
                        " - D=" (rtos diam 2 2)
                        " L=" (rtos h 2 2) " mm"))))

    (setq i (1+ i)))

  (command "_.UCS" "_D" "PIPE3D_TEMP")
  (command "_.UNDO" "_E")

  (setvar "OSMODE" old_osmode)
  (setvar "CMDECHO" old_cmdecho)
  (setvar "DYNMODE" old_dynmode)

  (prompt (strcat "\n-----------------------------"
                  "\nDA TAO: " (itoa count) " PIPE"
                  "\nDUONG KINH: " (rtos diam 2 2) " mm"))

  (initget "Yes No")
  (setq ans (getkword "\nChuyen sang visual style Realistic? [Yes/No] <Yes>: "))
  (if (or (null ans) (= ans "Yes"))
    (command "_.VSCURRENT" "_R"))

  (setq *error* old_error)
  (princ))

(defun c:PIPE3D () (c:PIPE3D_CREATE) (princ))

(princ "\n[Pipe] PIPE3D_CREATE module loaded.")
(princ)
