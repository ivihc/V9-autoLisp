(vl-load-com)

;;=========================================================
;; GLOBAL STATE
;;=========================================================
(setq *SP_ExcelFile* "")
(setq *SP_PlaneData* nil) ;; ((STT Z X Y) ...)

;;=========================================================
;; 1. HAM DOC EXCEL (GIU NGUYEN)
;;=========================================================
(defun Pipe:ReadSectionPlanes (excelFile / excelApp workbook sheet maxRow row stt zVal xVal yVal)
  (if (not (findfile excelFile))
    (progn (Pipe:Warn (strcat "Khong tim thay file: " excelFile)) nil)
    (progn
      (setq excelApp (vlax-create-object "Excel.Application"))
      (vlax-put-property excelApp 'Visible 0)
      (setq workbook (vlax-invoke-method (vlax-get-property excelApp 'Workbooks) 'Open excelFile))
      (setq sheet (vlax-get-property (vlax-get-property workbook 'Sheets) 'Item 1))
      (setq maxRow (vlax-get-property (vlax-get-property sheet 'UsedRange) 'Rows 'Count))
      
      (setq *SP_PlaneData* nil row 2)
      (repeat (- maxRow 1)
        (setq stt (vlax-variant-value (vlax-get-property (vlax-get-property sheet 'Cells) 'Item row 1)))
        (setq zVal (vlax-variant-value (vlax-get-property (vlax-get-property sheet 'Cells) 'Item row 2)))
        (setq xVal (vlax-variant-value (vlax-get-property (vlax-get-property sheet 'Cells) 'Item row 3)))
        (setq yVal (vlax-variant-value (vlax-get-property (vlax-get-property sheet 'Cells) 'Item row 4)))
        
        (if (and (/= stt "") (numberp stt))
          (setq *SP_PlaneData* (cons (list (atoi stt) 
                                           (if (/= zVal "") (atof zVal) 0.0)
                                           (if (/= xVal "") (atof xVal) 0.0)
                                           (if (/= yVal "") (atof yVal) 0.0))
                                     *SP_PlaneData*))
        )
        (setq row (1+ row))
      )
      
      (vlax-invoke-method workbook 'Close 0)
      (vlax-invoke-method excelApp 'Quit)
      (vlax-release-object sheet) (vlax-release-object workbook) (vlax-release-object excelApp)
      (setq *SP_PlaneData* (vl-sort *SP_PlaneData* '(lambda (a b) (< (car a) (car b)))))
      (Pipe:Msg (strcat "Da doc " (itoa (length *SP_PlaneData*)) " dong tu Excel."))
      *SP_PlaneData*
    )
  )
)

;;=========================================================
;; 2. HAM PHAN TICH TEN BLOCK
;;=========================================================
(defun Pipe:ParseBlockName (blkName / firstChar numStr numVal)
  (if (> (strlen blkName) 1)
    (progn
      (setq firstChar (strcase (substr blkName 1 1)))
      (setq numStr (substr blkName 2))
      (setq numVal (atoi numStr))
      (if (and (member firstChar '("T" "F" "B" "R" "L")) (> numVal 0))
        (list firstChar numVal) nil)
    ) nil)
)

;;=========================================================
;; 3. HAM THIET LAP UCS CHUAN CHO TUNG MAT CAT (QUAN TRONG NHAT)
;;=========================================================
(defun Pipe:SetUCSForPlane (planeType planeNum / planeData zVal xVal yVal origin xPt yPt)
  (setq planeData (assoc planeNum *SP_PlaneData*))
  (if (not planeData)
    (progn (Pipe:Warn (strcat "Khong tim thay STT " (itoa planeNum) " trong Excel.")) nil)
    (progn
      (setq zVal (cadr planeData) xVal (caddr planeData) yVal (cadddr planeData))
      
      (cond
        ;; TOP: Mat phang nam ngang. X = World X, Y = World Y
        ((= planeType "T")
         (setq origin (list 0.0 0.0 zVal))
         (setq xPt    (list 1.0 0.0 zVal)) ; X huong sang phai
         (setq yPt    (list 0.0 1.0 zVal)) ; Y huong ra sau (dung chieu 2D)
        )
        
        ;; FRONT: Mat phang dung. X = World X, Y = World Z (Len tren)
        ((= planeType "F")
         (setq origin (list 0.0 yVal 0.0))
         (setq xPt    (list 1.0 yVal 0.0)) ; X huong sang phai
         (setq yPt    (list 0.0 yVal 1.0)) ; Y huong len tren (Z)
        )
        
        ;; BACK: Nhin tu sau toi. X = World -X, Y = World Z
        ((= planeType "B")
         (setq origin (list 0.0 (- yVal) 0.0))
         (setq xPt    (list -1.0 (- yVal) 0.0)) ; X huong sang trai (de dung chieu nhin)
         (setq yPt    (list 0.0 (- yVal) 1.0))  ; Y huong len tren
        )
        
        ;; RIGHT: Nhin tu phai sang. X = World -Y, Y = World Z
        ((= planeType "R")
         (setq origin (list xVal 0.0 0.0))
         (setq xPt    (list xVal -1.0 0.0)) ; X huong vao trong (Back)
         (setq yPt    (list xVal 0.0 1.0))  ; Y huong len tren
        )
        
        ;; LEFT: Nhin tu trai sang. X = World Y, Y = World Z
        ((= planeType "L")
         (setq origin (list (- xVal) 0.0 0.0))
         (setq xPt    (list (- xVal) 1.0 0.0)) ; X huong ra ngoai (Front)
         (setq yPt    (list (- xVal) 0.0 1.0)) ; Y huong len tren
        )
      )
      
      ;; Thuc thi lenh UCS 3 diem
      (command "_.UCS" "_3" origin xPt yPt)
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      T
    )
  )
)

;;=========================================================
;; 4. LENH CHINH: SP_PLACE
;;=========================================================
(defun c:SP_PLACE (/ excelFile ss i ent obj blkName parsed planeType planeNum count ans)
  
  ;; Kiem tra va doc Excel neu can
  (if (not *SP_PlaneData*)
    (progn
      (Pipe:Msg "Chua doc file Excel. Hay chon file de doc du lieu.")
      (setq excelFile (getfiled "Chon file Excel chua toa do mat cat" "" "xlsx;xls" 8))
      (if excelFile
        (progn (setq *SP_ExcelFile* excelFile) (Pipe:ReadSectionPlanes excelFile))
        (progn (Pipe:Warn "Khong chon file Excel. Lenh bi huy.") (exit))
      )
    )
  )
  
  ;; Quet chon block
  (prompt "\n=== QUET CHON CAC BLOCK MAT CAT ===\n")
  (setq ss (ssget '((0 . "INSERT"))))
  (if (not ss) (progn (Pipe:Warn "Khong chon duoc block nao.") (exit)))
  
  (setq count 0 i 0)
  
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq obj (vlax-ename->vla-object ent))
    (setq blkName (vla-get-EffectiveName obj))
    (setq parsed (Pipe:ParseBlockName blkName))
    
    (if parsed
      (progn
        (setq planeType (car parsed) planeNum (cadr parsed))
        
        ;; 1. Chuyen UCS den dung mat cat va dung huong
        (if (Pipe:SetUCSForPlane planeType planeNum)
          (progn
            ;; 2. Xoa block cu (dang nam sai vi tri/huong)
            (vla-delete obj)
            
            ;; 3. Chen block moi tai goc (0,0,0) cua UCS hien tai
            ;; Block se tu dong \"dung len\" hoac \"nam xuong\" theo UCS
            (command "_.INSERT" blkName "0,0,0" "1" "1" "0")
            (while (> (getvar "CMDACTIVE") 0) (command ""))
            
            (setq count (1+ count))
            (Pipe:Msg (strcat "Da dat block " blkName " (STT " (itoa planeNum) ")"))
          )
        )
      )
      (Pipe:Warn (strcat "Block " blkName " khong dung dinh dang (T/F/B/R/L + So), bo qua."))
    )
    (setq i (1+ i))
  )
  
  (Pipe:Msg (strcat "=== HOAN TAT: " (itoa count) " block duoc dat dung vi tri va huong ==="))
  
  ;; Tra UCS ve World va hoi chuyen Isometric
  (command "_.UCS" "_W")
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  
  (initget "Yes No")
  (setq ans (getkword "\nChuyen ve goc nhin Isometric SW? [Yes/No] <Yes>: "))
  (if (or (null ans) (= ans "Yes"))
    (progn
      (command "_.VIEW" "_SWISO")
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (Pipe:Msg "Da chuyen ve Isometric SW.")
    )
  )
  (princ)
)

;;=========================================================
;; 5. LENH DOC EXCEL RIENG
;;=========================================================
(defun c:SP_READ (/ excelFile)
  (setq excelFile (getfiled "Chon file Excel" "" "xlsx;xls" 8))
  (if excelFile
    (progn
      (setq *SP_ExcelFile* excelFile)
      (Pipe:ReadSectionPlanes excelFile)
      (foreach item *SP_PlaneData*
        (Pipe:Msg (strcat "  STT " (itoa (car item)) ": Z=" (rtos (cadr item) 2 0) 
                          ", X=" (rtos (caddr item) 2 0) ", Y=" (rtos (cadddr item) 2 0))))
    )
  )
  (princ)
)

(princ "\n[Pipe] Section Planes module loaded (Fixed UCS Logic).")
(princ "\nCommands: SP_READ, SP_PLACE")
(princ)
EOF}]}]}]}]},