(vl-load-com)

 (vl-load-com)

 ;;=========================================================
 ;; GLOBAL STATE
 ;;=========================================================
 (setq *PS_Diam* 50.0)
 (setq *PS_SlopeDenom* 75)

 ;;=========================================================
 ;; 1. HAM VE CHUNG
 ;;=========================================================
 (defun Pipe:DrawPipeSegment (pt1 pt2 radius / )
   (command "_.CYLINDER" "_non" pt1 radius "_AXis" "_non" pt1 "_non" pt2)
   (while (> (getvar "CMDACTIVE") 0) (command ""))
   
   (command "_.LINE" "_non" pt1 "_non" pt2 "")
   (while (> (getvar "CMDACTIVE") 0) (command ""))
 )

 ;;=========================================================
 ;; 2. HAM TINH DIEM CUOI CO DO NGHIENG
 ;;=========================================================
 (defun Pipe:CalcSlopeEndpoint (pt1 pt2 slopeDenom direction / dist2D angle2D L0 C_val dZ pt_end)
   (setq dist2D (distance (list (car pt1) (cadr pt1) 0.0) 
                          (list (car pt2) (cadr pt2) 0.0)))
   (setq angle2D (angle (list (car pt1) (cadr pt1)) (list (car pt2) (cadr pt2))))
   
   (setq L0 dist2D)
   (setq C_val (/ 1.0 slopeDenom))
   (setq dZ (* L0 C_val))
   
   (cond
     ((= direction "Up")   (setq dZ dZ))
     ((= direction "Down") (setq dZ (- dZ)))
     (t                    (setq dZ 0.0))
   )
   
   (setq pt_end (list 
                  (+ (car pt1) (* L0 (cos angle2D)))
                  (+ (cadr pt1) (* L0 (sin angle2D)))
                  (+ (caddr pt1) dZ)
                ))
   pt_end
 )

 ;;=========================================================
 ;; 3. HAM XU LY DAU VAO (GRREAD) - DA SUA LOI
 ;;=========================================================
 (defun Pipe:GetInputWithTab (pt1 / input key pt_temp str_num ang_temp dist_temp)
   (setq pt_temp pt1
         str_num "")
   
   (redraw)
   
   (while T
     (setq input (grread T 15 0))
     
     (cond
       ;; 1. Di chuyển chuột (Type 5)
       ((= (car input) 5)
        (setq pt_temp (cadr input))
        (grdraw pt1 pt_temp 7 1)
        (if (> (strlen str_num) 0)
          (prompt (strcat "\rKhoang cach: " str_num))
        )
       )
       
       ;; 2. Click chuột (Type 3) - SUA O DAY
       ((= (car input) 3)
        (setq pt_temp (cadr input))
        (if (> (strlen str_num) 0)
          (progn
            (setq ang_temp (angle pt1 pt_temp)
                  dist_temp (atof str_num))
            (setq pt_temp (list (+ (car pt1) (* dist_temp (cos ang_temp)))
                                (+ (cadr pt1) (* dist_temp (sin ang_temp)))
                                (caddr pt1)))
          )
        )
        (redraw)
        ;; TRA VE LIST CHUAN: (3 diem) thay vi (2 phan tu)
        (list "POINT" (car pt_temp) (cadr pt_temp) (caddr pt_temp))
       )
       
       ;; 3. Gõ phím (Type 2)
       ((= (car input) 2)
        (setq key (cadr input))
        (cond
          ;; Phím TAB
          ((= key 9)
           (redraw)
           (list "TAB" nil)
          )
          ;; Phím Enter hoặc Space
          ((or (= key 13) (= key 32))
           (if (> (strlen str_num) 0)
             (progn
               (setq ang_temp (angle pt1 pt_temp)
                     dist_temp (atof str_num))
               (setq pt_temp (list (+ (car pt1) (* dist_temp (cos ang_temp)))
                                   (+ (cadr pt1) (* dist_temp (sin ang_temp)))
                                   (caddr pt1)))
               (redraw)
               (list "POINT" (car pt_temp) (cadr pt_temp) (caddr pt_temp))
             )
             (progn (redraw) (list "EXIT" nil))
           )
          )
          ;; Phím số
          ((or (and (>= key 48) (<= key 57)) (= key 46))
           (setq str_num (strcat str_num (chr key)))
           (prompt (strcat "\rNhap khoang cach: " str_num))
          )
          ;; Backspace
          ((= key 8)
           (if (> (strlen str_num) 0)
             (setq str_num (substr str_num 1 (1- (strlen str_num))))
           )
           (prompt (strcat "\rNhap khoang cach: " str_num))
          )
          ;; ESC
          ((= key 27)
           (redraw)
           (list "EXIT" nil)
          )
        )
       )
     )
   )
 )

 ;;=========================================================
 ;; 4. VONG LAP VE CHINH - DA SUA LOI CAP NHAT DIEM
 ;;=========================================================
 (defun Pipe:DrawSlopeLoop (slopeDenom direction / *error* old_vars pt1 pt2_temp pt_end radius count input_res)
   
   (setq old_err *error*)
   (defun *error* (msg)
     (if (and msg (not (member msg '("Function cancelled" "quit / exit abort"))))
       (prompt (strcat "\n[Pipe] Error: " msg)))
     (redraw)
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

   (prompt (strcat "\n=== CHE DO: " (strcase direction) " - Do nghieng 1/" (itoa (if slopeDenom slopeDenom 0)) " ===\n"))
   (prompt "[TAB] de doi che do | [So] de nhap khoang cach | [Click] de ve\n")

   (setq count 0)
   (command "_.UNDO" "_BE")

   (setq pt1 (getpoint "\nChon diem dau tien: "))
   
   ;; VONG LAP CHINH - SUA LOGIC UPDATE DIEM
   (while pt1
     (setq input_res (Pipe:GetInputWithTab pt1))
     
     ;; Kiem tra loai input
     (if (= (car input_res) "TAB")
       ;; XU LY TAB - DOI CHE DO
       (progn
         (cond
           ((= direction "Flat") (setq direction "Up"))
           ((= direction "Up")   (setq direction "Down"))
           ((= direction "Down") (setq direction "Flat"))
         )
         (prompt (strcat "\n*** DOI SANG: " (strcase direction) " ***\n"))
       )
       
       ;; KIEM TRA EXIT
       (if (= (car input_res) "EXIT")
         (setq pt1 nil)
         
         ;; XU LY DIEM VE
         (progn
           ;; Lay toa do tu input_res (3 phan tu sau "POINT")
           (setq pt2_temp (list (nth 1 input_res) 
                                (nth 2 input_res) 
                                (nth 3 input_res)))
           
           ;; Tinh diem cuoi co do nghieng
           (if slopeDenom
             (setq pt_end (Pipe:CalcSlopeEndpoint pt1 pt2_temp slopeDenom direction))
             (setq pt_end pt2_temp)
           )
           
           ;; Ve neu du do dai
           (if (> (distance pt1 pt_end) 0.001)
             (progn
               (Pipe:DrawPipeSegment pt1 pt_end radius)
               (setq count (1+ count))
               (prompt (strcat "\n + Pipe " (itoa count) 
                               " | dZ=" (rtos (abs (- (caddr pt_end) (caddr pt1))) 2 2)))
               ;; CAP NHAT PT1 = PT_END (QUAN TRONG!)
               (setq pt1 pt_end)
             )
             ;; Neu qua ngan, van cap nhat de ve tiep
             (setq pt1 pt_end)
           )
         )
       )
     )
   )

   (command "_.UNDO" "_E")
   (foreach var_pair old_vars (setvar (car var_pair) (cdr var_pair)))
   (setq *error* old_err)
   (redraw)
   
   (if (> count 0)
     (prompt (strcat "\n-----------------------------\nDA VE: " (itoa count) " PIPE\n"))
   )
   (princ)
 )

 ;;=========================================================
 ;; 5. CAC LENH
 ;;=========================================================
 (defun c:P0 ( / ) (Pipe:DrawSlopeLoop nil "Flat") (princ))

 (defun c:P_UP ( / temp_slope)
   (initget "75 100")
   (setq temp_slope (getkword (strcat "\nDo nghieng [1/75 or 1/100] <1/" (itoa *PS_SlopeDenom*) ">: ")))
   (if temp_slope (setq *PS_SlopeDenom* (atoi temp_slope)))
   (Pipe:DrawSlopeLoop *PS_SlopeDenom* "Up")
   (princ)
 )

 (defun c:P_DOWN ( / temp_slope)
   (initget "75 100")
   (setq temp_slope (getkword (strcat "\nDo nghieng [1/75 or 1/100] <1/" (itoa *PS_SlopeDenom*) ">: ")))
   (if temp_slope (setq *PS_SlopeDenom* (atoi temp_slope)))
   (Pipe:DrawSlopeLoop *PS_SlopeDenom* "Down")
   (princ)
 )

 (princ "\n[Pipe] PIPE_SLOPE module loaded. Type P0, P_UP, or P_DOWN to start.")
 (princ)
      )
      
      ;; Xử lý điểm vẽ
      ((= (car input_res) "POINT")
       (setq pt_end (cadr input_res))
       
       ;; Tính toán điểm cuối thực tế (có độ nghiêng)
       (if slopeDenom
         (setq pt_end (Pipe:CalcSlopeEndpoint pt1 pt_end slopeDenom direction))
       )
       
       (if (> (distance pt1 pt_end) 0.001)
         (progn
           (Pipe:DrawPipeSegment pt1 pt_end radius)
           (setq count (1+ count))
           (prompt (strcat "\n + Pipe " (itoa count) 
                           " | L0=" (rtos (distance (list (car pt1) (cadr pt1) 0.0) (list (car pt_end) (cadr pt_end) 0.0)) 2 2)
                           " | dZ=" (rtos (abs (- (caddr pt_end) (caddr pt1))) 2 2)))
           (setq pt1 pt_end)
         )
       )
      )
      
      ;; Thoát
      (T (setq pt1 nil))
    )
  )

  (command "_.UNDO" "_E")
  (foreach var_pair old_vars (setvar (car var_pair) (cdr var_pair)))
  (setq *error* old_err)
  (redraw)
  
  (if (> count 0)
    (prompt (strcat "\n-----------------------------\nDA VE: " (itoa count) " PIPE\n"))
  )
  (princ)
)

;;=========================================================
;; 5. CAC LENH RIENG BIET
;;=========================================================
(defun c:P0 ( / ) (Pipe:DrawSlopeLoop nil "Flat") (princ))

(defun c:P_UP ( / temp_slope)
  (initget "75 100")
  (setq temp_slope (getkword (strcat "\nDo nghieng [1/75 or 1/100] <1/" (itoa *PS_SlopeDenom*) ">: ")))
  (if temp_slope (setq *PS_SlopeDenom* (atoi temp_slope)))
  (Pipe:DrawSlopeLoop *PS_SlopeDenom* "Up")
  (princ)
)

(defun c:P_DOWN ( / temp_slope)
  (initget "75 100")
  (setq temp_slope (getkword (strcat "\nDo nghieng [1/75 or 1/100] <1/" (itoa *PS_SlopeDenom*) ">: ")))
  (if temp_slope (setq *PS_SlopeDenom* (atoi temp_slope)))
  (Pipe:DrawSlopeLoop *PS_SlopeDenom* "Down")
  (princ)
)

(princ "\n[Pipe] PIPE_SLOPE module loaded. Type P0, P_UP, or P_DOWN to start.")
(princ)