(vl-load-com)

;;=========================================================
;; GLOBAL STATE
;;=========================================================
(setq *PS_Diam* 50.0)
(setq *PS_SlopeDenom* 75)
(setq *PS_Mode* "Flat")

;;=========================================================
;; HAM VE CHUNG
;;=========================================================
(defun Pipe:DrawPipeSegment (pt1_wcs pt2_wcs radius)
  (command "_.CYLINDER" "_non" pt1_wcs radius "_AXis" "_non" pt1_wcs "_non" pt2_wcs)
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (command "_.LINE" "_non" pt1_wcs "_non" pt2_wcs "")
  (while (> (getvar "CMDACTIVE") 0) (command ""))
)

;;=========================================================
;; HAM TINH DIEM CUOI (CHO ONG NGHIENG/NGANG)
;;=========================================================
(defun Pipe:CalcSlopeEndpoint (pt1_wcs pt2_wcs slopeDenom direction input_type / 
                                dist2D angle2D C_val dZ pt_end L0)
  (setq dist2D (distance (list (car pt1_wcs) (cadr pt1_wcs) 0.0) 
                         (list (car pt2_wcs) (cadr pt2_wcs) 0.0)))
  
  (if (< dist2D 0.001)
    (setq pt_end nil)  ; Ong dung thi khong dung ham nay
    (progn
      (setq angle2D (angle (list (car pt1_wcs) (cadr pt1_wcs) 0.0) 
                           (list (car pt2_wcs) (cadr pt2_wcs) 0.0)))
      (setq C_val (/ 1.0 slopeDenom))
      
      (if (= input_type "L")
        (setq L0 (/ dist2D (sqrt (+ 1.0 (* C_val C_val)))))
        (setq L0 dist2D)
      )
      
      (setq dZ (* L0 C_val))
      (cond
        ((= direction "Up")   (setq dZ dZ))
        ((= direction "Down") (setq dZ (- dZ)))
        (t                    (setq dZ 0.0))
      )
      
      (setq pt_end (list 
                     (+ (car pt1_wcs) (* L0 (cos angle2D)))
                     (+ (cadr pt1_wcs) (* L0 (sin angle2D)))
                     (+ (caddr pt1_wcs) dZ)
                   ))
    )
  )
  pt_end
)

;;=========================================================
;; VONG LAP VE CHINH
;;=========================================================
(defun Pipe:DrawSlopeLoop (slopeDenom direction / *error* old_err old_vars 
                            pt1_ucs pt1_wcs pt2_ucs pt2_wcs pt_end_wcs pt_end_ucs
                            radius count input_res temp_diam
                            dist_input temp_slope l_val ang_ucs ang_wcs
                            C_val L0_calc dZ_calc vec_ucs vec_wcs)
  
  (setq old_err *error*)
  (defun *error* (msg)
    (command "_.UNDO" "_E")
    (if (and msg (not (member msg '("Function cancelled" "quit / exit abort"))))
      (prompt (strcat "\n[Pipe] Error: " msg)))
    (if old_vars (foreach var_pair old_vars (setvar (car var_pair) (cdr var_pair))))
    (setq *error* old_err)
    (princ)
  )
  
  (setq old_vars (list (cons 'OSMODE (getvar "OSMODE"))
                       (cons 'CMDECHO (getvar "CMDECHO"))
                       (cons 'SOLIDHIST (getvar "SOLIDHIST"))))
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "SOLIDHIST" 1)

  (initget 6)
  (setq temp_diam (getreal (strcat "\nDuong kinh ong <" (rtos *PS_Diam* 2 0) ">: ")))
  (if temp_diam (setq *PS_Diam* temp_diam))
  (setq radius (/ *PS_Diam* 2.0))

  (if slopeDenom (setq *PS_SlopeDenom* slopeDenom))
  (setq direction *PS_Mode*)

  (prompt (strcat "\n=== CHE DO: " (strcase direction) " - Do nghieng 1/" (rtos *PS_SlopeDenom* 2 0) " ===\n"))
  (prompt "[Toggle] doi che do | [Slope] doi do nghieng | [Length] chieu dai thuc | [Pick/So] chieu dai ngang | [Enter] ket thuc\n")

  (setq count 0)
  (command "_.UNDO" "_BE")
  
  (setq pt1_ucs (getpoint "\nChon diem dau tien: "))
  (setq pt1_wcs (trans pt1_ucs 1 0))
  
  (while pt1_ucs
    (initget 0 "Toggle Slope Length")
    (setq input_res (getpoint pt1_ucs (strcat "\nDiem tiep theo or [Toggle/Slope/Length] <" (strcase direction) " - 1/" (rtos *PS_SlopeDenom* 2 0) ">: ")))
    
    (cond
      ;; DOI HUONG
      ((= input_res "Toggle")
       (cond
         ((= direction "Flat") (setq direction "Up"))
         ((= direction "Up")   (setq direction "Down"))
         ((= direction "Down") (setq direction "Flat"))
       )
       (setq *PS_Mode* direction)
       (prompt (strcat "\n*** DOI SANG: " (strcase direction) " - 1/" (rtos *PS_SlopeDenom* 2 0) " ***\n"))
      )
      
      ;; DOI DO NGHIENG
      ((= input_res "Slope")
       (initget 6)
       (setq temp_slope (getreal (strcat "\nNhap MAU SO do nghieng moi <" (rtos *PS_SlopeDenom* 2 0) ">: ")))
       (if temp_slope 
         (progn
           (setq *PS_SlopeDenom* temp_slope)
           (prompt (strcat "\n*** DO NGHIENG MOI: 1/" (rtos *PS_SlopeDenom* 2 0) " ***\n"))
         )
       )
      )
      
      ;; NHAP CHIEU DAI THUC L
      ((= input_res "Length")
       (initget 6)
       (setq l_val (getreal "\nNhap chieu dai thuc L: "))
       (if l_val
         (progn
           (setq ang_ucs (getangle pt1_ucs (strcat "\nChon huong di <" (angtos (getvar "LASTANGLE") 0 2) ">: ")))
           (if (not ang_ucs) (setq ang_ucs (getvar "LASTANGLE")))
           
           ;; Chuyen goc tu UCS sang WCS
           (setq vec_ucs (list (cos ang_ucs) (sin ang_ucs) 0))
           (setq vec_wcs (mapcar '- (trans (mapcar '+ pt1_ucs vec_ucs) 1 0) pt1_wcs))
           (setq ang_wcs (angle '(0 0 0) (list (car vec_wcs) (cadr vec_wcs) 0)))
           
           ;; Tinh nguoc L0 tu L
           (setq C_val (/ 1.0 *PS_SlopeDenom*))
           (setq L0_calc (/ l_val (sqrt (+ 1.0 (* C_val C_val)))))
           (setq dZ_calc (* L0_calc C_val))
           
           (cond
             ((= direction "Up")   (setq dZ_calc dZ_calc))
             ((= direction "Down") (setq dZ_calc (- dZ_calc)))
             (t                    (setq dZ_calc 0.0))
           )
           
           (setq pt_end_wcs (list 
                              (+ (car pt1_wcs) (* L0_calc (cos ang_wcs)))
                              (+ (cadr pt1_wcs) (* L0_calc (sin ang_wcs)))
                              (+ (caddr pt1_wcs) dZ_calc)
                            ))
           
           (setq pt_end_ucs (trans pt_end_wcs 0 1))
           
           (if (> (distance pt1_wcs pt_end_wcs) 0.001)
             (progn
               (Pipe:DrawPipeSegment pt1_wcs pt_end_wcs radius)
               (setq count (1+ count))
               (prompt (strcat "\n + Pipe " (itoa count) 
                               " | L=" (rtos l_val 2 2)
                               " | L0=" (rtos L0_calc 2 2)
                               " | dZ=" (rtos (abs dZ_calc) 2 2)))
               (setq pt1_ucs pt_end_ucs)
               (setq pt1_wcs pt_end_wcs)
             )
           )
         )
       )
      )
      
      ;; PICK DIEM HOAC GO SO
      ((listp input_res)
       (setq pt2_ucs input_res)
       (setq pt2_wcs (trans pt2_ucs 1 0))
       
       (setq dist_input (distance (list (car pt1_wcs) (cadr pt1_wcs) 0.0) 
                                  (list (car pt2_wcs) (cadr pt2_wcs) 0.0)))
       
       (if (> dist_input 0.001)
         ;; ONG NGHIENG/NGANG: dung ham tinh toan
         (progn
           (setq pt_end_wcs (Pipe:CalcSlopeEndpoint pt1_wcs pt2_wcs *PS_SlopeDenom* direction "L0"))
           (setq pt_end_ucs (trans pt_end_wcs 0 1))
           
           (if pt_end_wcs
             (progn
               (Pipe:DrawPipeSegment pt1_wcs pt_end_wcs radius)
               (setq count (1+ count))
               (prompt (strcat "\n + Pipe " (itoa count) 
                               " | L0=" (rtos dist_input 2 2)
                               " | dZ=" (rtos (abs (- (caddr pt_end_wcs) (caddr pt1_wcs))) 2 2)))
               (setq pt1_ucs pt_end_ucs)
               (setq pt1_wcs pt_end_wcs)
             )
           )
         )
         
         ;; ONG THANG DUNG: LOGIC DON GIAN - Lay Z cua pt2, giu nguyen X,Y cua pt1
         (progn
           (setq pt_end_wcs (list (car pt1_wcs) (cadr pt1_wcs) (caddr pt2_wcs)))
           (setq pt_end_ucs (trans pt_end_wcs 0 1))
           
           (if (> (distance pt1_wcs pt_end_wcs) 0.001)
             (progn
               (Pipe:DrawPipeSegment pt1_wcs pt_end_wcs radius)
               (setq count (1+ count))
               (prompt (strcat "\n + Pipe " (itoa count) " (VERTICAL) | H=" 
                               (rtos (abs (- (caddr pt2_wcs) (caddr pt1_wcs))) 2 2)))
               (setq pt1_ucs pt_end_ucs)
               (setq pt1_wcs pt_end_wcs)
             )
           )
         )
       )
      )
      
      ;; ENTER / ESC
      ((null input_res) (setq pt1_ucs nil))
    )
  )

  (command "_.UNDO" "_E")
  (foreach var_pair old_vars (setvar (car var_pair) (cdr var_pair)))
  (setq *error* old_err)
  (if (> count 0) (prompt (strcat "\n-----------------------------\nDA VE: " (itoa count) " PIPE\n")))
  (princ)
)

;;=========================================================
;; HAM CHON DO NGHIENG
;;=========================================================
(defun Pipe:AskSlope ( / temp_slope)
  (initget 6)
  (setq temp_slope (getreal (strcat "\nNhap MAU SO do nghieng <" (rtos *PS_SlopeDenom* 2 0) ">: ")))
  (if temp_slope (setq *PS_SlopeDenom* temp_slope))
)

;;=========================================================
;; CAC LENH
;;=========================================================
(defun c:P0 ()
  (Pipe:AskSlope)
  (setq *PS_Mode* "Flat")
  (Pipe:DrawSlopeLoop nil "Flat") 
  (princ)
)

(defun c:P_UP ()
  (Pipe:AskSlope)
  (setq *PS_Mode* "Up")
  (Pipe:DrawSlopeLoop *PS_SlopeDenom* "Up")
  (princ)
)

(defun c:P_DOWN ()
  (Pipe:AskSlope)
  (setq *PS_Mode* "Down")
  (Pipe:DrawSlopeLoop *PS_SlopeDenom* "Down")
  (princ)
)

(princ "\n[Pipe] PIPE_SLOPE module loaded. Commands: P0, P_UP, P_DOWN")
(princ)
