(vl-load-com)

;;=========================================================
;; PIPE BLOCK HIERARCHY ANALYZER (Fixed Version)
;;=========================================================

(defun c:BLKHIER ( / old_err ent obj blkName result csvData header)
  ;; 1. Setup Error Handler cục bộ an toàn
  (setq old_err *error*)
  (defun *error* (msg)
    (if (and msg (not (member msg '("Function cancelled" "quit / exit abort"))))
      (prompt (strcat "\n[Pipe] Error: " msg)))
    (setq *error* old_err)
    (princ)
  )

  (prompt "\n=== PHAN TICH CAU TRUC BLOCK LONG NHAU ===\n")
  (prompt "Chon mot Block instance trong ban ve de phan tich cau truc:\n")
  
  (if (setq ent (car (entsel)))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (if (= (vla-get-ObjectName obj) "AcDbBlockReference")
        (progn
          (setq blkName (vla-get-EffectiveName obj))
          (Pipe:Msg (strcat "Dang phan tich Block: " blkName))
          (setq result (Pipe:AnalyzeBlockStructure blkName))
          
          (if result
            (progn
              (Pipe:PrintTree result blkName)
              (setq header (strcat "Parent_Block" Pipe:*Sep* 
                                   "Child_Block" Pipe:*Sep* 
                                   "Direct_Count" Pipe:*Sep* 
                                   "Total_Quantity" Pipe:*Sep* 
                                   "Depth_Level" Pipe:*Sep* 
                                   "Full_Path"))
              (setq csvData (mapcar '(lambda (x) 
                                       (strcat (nth 0 x) Pipe:*Sep* 
                                               (nth 1 x) Pipe:*Sep* 
                                               (itoa (nth 2 x)) Pipe:*Sep* 
                                               (itoa (nth 3 x)) Pipe:*Sep* 
                                               (itoa (nth 4 x)) Pipe:*Sep* 
                                               "\"" (nth 5 x) "\"")) 
                                     result))
              (if (Pipe:SaveCSV "Block_Hierarchy_Report" header csvData)
                (Pipe:Msg "Da xuat bao cao thanh cong!")
                (Pipe:Warn "Khong the luu file CSV."))
            )
            (Pipe:Warn "Block nay khong chua block con hoac co loi cau truc.")
          )
        )
        (Pipe:Warn "Doi tuong chon khong phai la Block.")
      )
    )
    (Pipe:Warn "Khong chon duoc doi tuong.")
  )

  ;; Restore Error Handler
  (setq *error* old_err)
  (princ)
)

;;=========================================================
;; HAM LOI: PHAN TICH CAU TRUC (RECURSIVE)
;;=========================================================
(defun Pipe:AnalyzeBlockStructure (rootBlkName / visited)
  (setq visited nil)
  (Pipe:TraverseBlockDef rootBlkName 1 1 "" visited)
)

(defun Pipe:TraverseBlockDef (currentBlkName currentDepth multiplier currentPath visited / 
                              blkDef ent entData childObj childName childCount childMultiplier 
                              newPath subResult results)
  (if (member currentBlkName visited)
    (progn
      (Pipe:Warn (strcat "Phat hien vong lap tai: " currentBlkName " -> Bo qua de tranh crash."))
      nil
    )
    (progn
      (setq visited (cons currentBlkName visited))
      (setq blkDef (tblsearch "BLOCK" currentBlkName))
      (if blkDef
        (progn
          (setq ent (entnext (cdr (assoc -1 blkDef))))
          (setq results nil)
          (while ent
            (setq entData (entget ent))
            (if (= (cdr (assoc 0 entData)) "INSERT")
              (progn
                ;; Lay ten block con an toan bang VLA (xu ly ca Dynamic Block)
                (setq childObj (vlax-ename->vla-object ent))
                (setq childName (vla-get-EffectiveName childObj))
                
                (setq childCount 1) 
                (setq childMultiplier (* multiplier childCount))
                (setq newPath (if (= currentPath "") 
                                childName 
                                (strcat currentPath " -> " childName)))
                
                (setq results (cons (list currentBlkName childName childCount childMultiplier currentDepth newPath) results))
                
                (setq subResult (Pipe:TraverseBlockDef childName 
                                                       (1+ currentDepth) 
                                                       childMultiplier 
                                                       newPath 
                                                       visited))
                (if subResult
                  (setq results (append results subResult))
                )
              )
            )
            (setq ent (entnext ent))
          )
          results
        )
        (progn
          (Pipe:Warn (strcat "Khong tim thay dinh nghia Block: " currentBlkName))
          nil
        )
      )
    )
  )
)

;;=========================================================
;; HAM HIEN THI: IN RA MAN HINH DANG CAY (Da sua loi let*)
;;=========================================================
(defun Pipe:PrintTree (data rootName / lastDepth child direct total depth indent)
  (prompt (strcat "\n--- CAY PHAN CAP CUA: " rootName " ---\n"))
  (setq lastDepth 0)
  
  (foreach item data
    (setq child (nth 1 item)
          direct (nth 2 item)
          total (nth 3 item)
          depth (nth 4 item)
          indent (apply 'strcat (make-list (- depth 1) "    ")))
    
    (if (> depth lastDepth)
      (prompt "\n")
    )
    
    (prompt (strcat indent "└─ " child 
                    " [Truc tiep: " (itoa direct) 
                    " | Tong: " (itoa total) "]\n"))
    (setq lastDepth depth)
  )
  (prompt "------------------------------------------\n")
)

;; Ham phu tao list khoang trang
(defun make-list (n str / lst i)
  (setq i 0 lst nil)
  (repeat n
    (setq lst (cons str lst)
          i (1+ i))
  )
  (reverse lst)
)

(princ "\nLoaded Pipe Block Hierarchy Analyzer. Type BLKHIER to start.")
(princ)