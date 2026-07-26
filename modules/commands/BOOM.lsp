(vl-load-com)

;;=========================================================
;; 1. HAM TIEN ICH & THONG BAO
;;=========================================================
(if (not (boundp 'Pipe:Msg)) 
  (defun Pipe:Msg (s) (prompt (strcat "\n[Pipe] " s "\n")))
)
(if (not (boundp 'Pipe:Warn)) 
  (defun Pipe:Warn (s) (prompt (strcat "\n[Pipe] WARNING: " s "\n")))
)

(defun Pipe:Trim (str) (vl-string-trim " \t" str))

(defun Pipe:SplitCSV (str / result pos token)
  (setq result nil)
  (while (setq pos (vl-string-search "," str))
    (setq token (substr str 1 pos))
    (setq result (cons token result))
    (setq str (substr str (+ pos 2)))
  )
  (setq result (cons str result))
  (reverse result)
)

;;=========================================================
;; 2. HAM QUAN LY UCS (TONG QUAT)
;;=========================================================
(defun Pipe:SetUCS (origin xDir yDir / xPt yPt)
  (if (and origin xDir yDir
           (= (length origin) 3)
           (= (length xDir) 3)
           (= (length yDir) 3))
    (progn
      (setq xPt (mapcar '+ origin xDir))
      (setq yPt (mapcar '+ origin yDir))
      (command "_.UCS" "_3" "_non" origin "_non" xPt "_non" yPt)
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      T
    )
    (progn (Pipe:Warn "Tham so UCS khong hop le.") nil)
  )
)

(defun Pipe:UCSReset ()
  (command "_.UCS" "_W")
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  T
)

;;=========================================================
;; 3. GLOBAL STATE & CAU HINH LAYER
;;=========================================================
(setq *SP_ExcelFile* "")
(setq *SP_SectionData* nil)

(setq *SP_LayerConfig* '(
  ("F" "SECTION_FRONT" 5)
  ("B" "SECTION_BACK" 1)
  ("T" "SECTION_TOP" 2)
  ("L" "SECTION_LEFT" 6)
  ("R" "SECTION_RIGHT" 3)
))

;;=========================================================
;; 4. HAM DOC FILE CSV (THONG MINH)
;;=========================================================
(defun Pipe:ReadCSV (csvFile / f line data section type xVal yVal zVal l0 w0 h0 lastIdx)
  (if (not (findfile csvFile))
    (progn (Pipe:Warn (strcat "Khong tim thay file: " csvFile)) nil)
    (progn
      (setq f (open csvFile "r"))
      (setq *SP_SectionData* nil)
      (read-line f)
      
      (while (setq line (read-line f))
        (setq line (vl-string-trim " \t" line))
        (if (/= line "")
          (progn
            (setq data (Pipe:SplitCSV line))
            (if (>= (length data) 7)
              (progn
                (setq section (strcase (Pipe:Trim (nth 0 data))))
                (setq xVal (atof (Pipe:Trim (nth 1 data))))
                (setq yVal (atof (Pipe:Trim (nth 2 data))))
                (setq zVal (atof (Pipe:Trim (nth 3 data))))
                
                (setq lastIdx (1- (length data)))
                (setq h0 (atof (Pipe:Trim (nth lastIdx data))))
                (setq w0 (atof (Pipe:Trim (nth (1- lastIdx) data))))
                (setq l0 (atof (Pipe:Trim (nth (- lastIdx 2) data))))
                
                (if (> (strlen section) 1)
                  (progn
                    (setq type (substr section 1 1))
                    (if (member type '("F" "B" "T" "L" "R"))
                      (setq *SP_SectionData* 
                            (cons (list section type xVal yVal zVal l0 w0 h0) 
                                  *SP_SectionData*))
                    )
                  )
                )
              )
            )
          )
        )
      )
      (close f)
      (Pipe:Msg (strcat "Da doc " (itoa (length *SP_SectionData*)) " section tu CSV."))
      *SP_SectionData*
    )
  )
)

;;=========================================================
;; 5. HAM TIM KIEM & LAYER
;;=========================================================
(defun Pipe:FindSectionData (sectionName / result)
  (setq result nil)
  (foreach item *SP_SectionData*
    (if (= (car item) (strcase sectionName))
      (setq result item)
    )
  )
  result
)

(defun Pipe:CreateSectionLayers ()
  (foreach cfg *SP_LayerConfig*
    (setq lname (cadr cfg))
    (setq lcolor (caddr cfg))
    (if (not (tblsearch "LAYER" lname))
      (progn
        (command "_.LAYER" "_M" lname "_C" lcolor "" "")
        (while (> (getvar "CMDACTIVE") 0) (command ""))
      )
    )
  )
  T
)

(defun Pipe:GetLayerForSection (type / cfg)
  (setq cfg (assoc type *SP_LayerConfig*))
  (if cfg (cadr cfg) "0")
)

;;=========================================================
;; 6. HAM THIET LAP UCS (DA LOAI BO DO LECH Z)
;;=========================================================
(defun Pipe:SetUCSForSection (sectionData / section type xVal yVal zVal l0 w0 h0 origin xDir yDir)
  (setq section (car sectionData))
  (setq type (cadr sectionData))
  (setq xVal (nth 2 sectionData))
  (setq yVal (nth 3 sectionData))
  (setq zVal (nth 4 sectionData))
  (setq l0 (nth 5 sectionData))
  (setq w0 (nth 6 sectionData))
  (setq h0 (nth 7 sectionData))
  
  ;; Tat ca cac mat cat deu su dung truc tiep toa do (X, Y, Z) tu CSV
  (setq origin (list xVal yVal zVal))
  
  (cond
    ;; F - Front: Mat truoc
    ((= type "F")
     (setq xDir '(1.0 0.0 0.0))
     (setq yDir '(0.0 0.0 1.0))
    )
    ;; B - Back: Mat sau
    ((= type "B")
     (setq xDir '(-1.0 0.0 0.0))
     (setq yDir '(0.0 0.0 1.0))
    )
    ;; T - Top: Mat tren
    ((= type "T")
     (setq xDir '(1.0 0.0 0.0))
     (setq yDir '(0.0 1.0 0.0))
    )
    ;; L - Left: Mat trai
    ((= type "L")
     (setq xDir '(0.0 -1.0 0.0))
     (setq yDir '(0.0 0.0 1.0))
    )
    ;; R - Right: Mat phai
    ((= type "R")
     (setq xDir '(0.0 1.0 0.0))
     (setq yDir '(0.0 0.0 1.0))
    )
  )
  
  (Pipe:SetUCS origin xDir yDir)
)

;;=========================================================
;; 7. LENH CHINH: SP_PLACE
;;=========================================================
(defun c:SP_PLACE (/ csvFile ss i ent obj sectionName sectionData count ans lname)
  (Pipe:CreateSectionLayers)
  
  (setq csvFile (getfiled "Chon file CSV chua du lieu mat cat" "" "csv" 8))
  (if (not csvFile)
    (progn (Pipe:Warn "Khong chon file. Lenh bi huy.") (exit))
  )
  
  (setq *SP_ExcelFile* csvFile)
  (Pipe:ReadCSV csvFile)
  
  (if (not *SP_SectionData*)
    (progn (Pipe:Warn "Khong doc duoc du lieu tu file CSV.") (exit))
  )
  
  (prompt "\n=== QUET CHON CAC BLOCK MAT CHIEU ===\n")
  (setq ss (ssget '((0 . "INSERT"))))
  (if (not ss) (progn (Pipe:Warn "Khong chon duoc block.") (exit)))
  
  (setq count 0 i 0)
  
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq obj (vlax-ename->vla-object ent))
    (setq sectionName (vla-get-EffectiveName obj))
    (setq sectionData (Pipe:FindSectionData sectionName))
    
    (if sectionData
      (progn
        (setq lname (Pipe:GetLayerForSection (cadr sectionData)))
        
        (if (Pipe:SetUCSForSection sectionData)
          (progn
            (vla-delete obj)
            (setvar "CLAYER" lname)
            (command "_.INSERT" sectionName "0,0,0" "1" "1" "0")
            (while (> (getvar "CMDACTIVE") 0) (command ""))
            (Pipe:UCSReset)
            
            (setq count (1+ count))
            (Pipe:Msg (strcat "Da dat " sectionName " -> " lname))
          )
        )
      )
      (Pipe:Warn (strcat "Khong tim thay " sectionName " trong CSV."))
    )
    (setq i (1+ i))
  )
  
  (Pipe:Msg (strcat "=== HOAN TAT: " (itoa count) " block ==="))
  
  (initget "Yes No")
  (setq ans (getkword "\nChuyen ve Isometric SW? [Yes/No] <Yes>: "))
  (if (or (null ans) (= ans "Yes"))
    (progn
      (command "_.VIEW" "_SWISO")
      (while (> (getvar "CMDACTIVE") 0) (command ""))
    )
  )
  (princ)
)

(princ "\n[Pipe] Section Planes (Clean Version) loaded.")
(princ "\nLenh su dung: SP_PLACE")
(princ)
