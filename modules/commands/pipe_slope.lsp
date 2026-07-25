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
(defun Pipe:DrawPipeSegment (pt1 pt2 radius)
  (command "_.CYLINDER" "_non" pt1 radius "_AXis" "_non" pt1 "_non" pt2)
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (command "_.LINE" "_non" pt1 "_non" pt2 "")
  (while (> (getvar "CMDACTIVE") 0) (command ""))
)

;;=========================================================
;; HAM TINH DIEM CUOI
;;=========================================================
(defun Pipe:CalcSlopeEndpoint (pt1 pt2 slopeDenom direction input_type / dist2D angle2D C_val dZ pt_end L0)
  (setq dist2D (distance (list (car pt1) (cadr pt1) 0.0) 
                         (list (car pt2) (cadr pt2) 0.0)))
  
  (if (< dist2D 0.001)
    (setq pt_end (list (car pt1) (cadr pt1) (caddr pt2)))
    (progn
      (setq angle2D (angle (list (car pt1) (cadr pt1) 0.0) 
                           (list (car pt2) (cadr pt2) 0.0)))
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
                     (+ (car pt1) (* L0 (cos angle2D)))
                     (+ (cadr pt1) (* L0 (sin angle2D)))
                     (+ (caddr pt1) dZ)
                   ))
    )
  )
  pt_end
)

;;=========================================================
;; HAM XU LY INPUT (DA SUA LOI STRINGP VA NUMBERP)
;;=========================================================
(defun Pipe:ParseInput (input / str_len first_char num_str num_val)
  (cond
    ;; 1. Nếu là điểm (LIST)
    ((listp input) 
     (list "POINT" (car input) (cadr input) (caddr input))
    )
    
    ;; 2. Nếu là số (NUMBER) - XỬ LÝ TRƯỚC ĐỂ TRÁNH LỖI VALUE MUST BE NONZERO
    ((numberp input)
     (if (> input 0.0)
       (list "L0" input)
       (list "INVALID" nil)
     )
    )
    
    ;; 3. Nếu là chuỗi (STRING) - DÙNG TYPE THAY VÌ STRINGP
    ((= (type input) 'STR)
     (setq str_len (strlen input))
     (if (> str_len 0)
       (progn
         (setq first_char (strcase (substr input 1 1)))
         (setq num_str (substr input 2))
         (cond
           ((= first_char "T") (list "TOGGLE" nil))
           ((and (= first_char "L") (> (strlen num_str) 0))
            (setq num_val (atof num_str))
            (if (> num_val 0) (list "L" num_val) (list "INVALID" nil))
           )
           (t (list "INVALID" nil))
         )
       )
       (list "EXIT" nil)
     )
    )
    
    ;; Các trường hợp còn lại (nil, symbol...)
    (t (list "EXIT" nil))
  )
)

;;=========================================================
;; VONG LAP VE CHINH
;;=========================================================
(defun Pipe:DrawSlopeLoop (slopeDenom direction / *error* old_err old_vars 
                            pt1 pt2_temp pt_end radius count input_res temp_diam
                            parsed input_type dist_input)
  
  (setq old_err *error*)
  (defun *error* (msg)
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

  (prompt (strcat "\n=== CHE DO: " (strcase direction) " - Do nghieng 1/" (itoa *PS_SlopeDenom*) " ===\n"))
  (prompt "[T] doi che do | [L1500] chieu dai thuc | [1500] chieu dai ngang | [Pick] diem\n")

  (setq count 0)
  (command "_.UNDO" "_BE")
  (setq pt1 (getpoint "\nChon diem dau tien: "))
  
  (while pt1
    (initget 0 "T") 
    (setq input_res (getpoint pt1 (strcat "\nDiem tiep theo <" (strcase direction) ">: ")))
    (setq parsed (Pipe:ParseInput input_res))
    (setq input_type (car parsed))
    
    (cond
      ((= input_type "TOGGLE")
       (cond
         ((= direction "Flat") (setq direction "Up"))
         ((= direction "Up")   (setq direction "Down"))
         ((= direction "Down") (setq direction "Flat"))
       )
       (setq *PS_Mode* direction)
       (prompt (strcat "\n*** DOI SANG: " (strcase direction) " - 1/" (itoa *PS_SlopeDenom*) " ***\n"))
      )
      ((= input_type "EXIT") (setq pt1 nil))
      ((= input_type "INVALID") (prompt "\n[!] Input khong hop le."))
      ((or (= input_type "POINT") (= input_type "L0") (= input_type "L"))
       (setq dist_input (cadr parsed))
       (if (= input_type "POINT")
         (setq pt2_temp (list (nth 1 parsed) (nth 2 parsed) (nth 3 parsed))
               dist_input (distance (list (car pt1) (cadr pt1) 0.0) 
                                    (list (car pt2_temp) (cadr pt2_temp) 0.0)))
         (setq pt2_temp (polar pt1 (getvar "LASTANGLE") dist_input))
       )
       (setq pt_end (Pipe:CalcSlopeEndpoint pt1 pt2_temp *PS_SlopeDenom* direction input_type))
       (if (> (distance pt1 pt_end) 0.001)
         (progn
           (Pipe:DrawPipeSegment pt1 pt_end radius)
           (setq count (1+ count))
           (if (< (distance (list (car pt1) (cadr pt1) 0.0) (list (car pt_end) (cadr pt_end) 0.0)) 0.001)
             (prompt (strcat "\n + Pipe " (itoa count) " (VERTICAL) | H=" (rtos (abs (- (caddr pt_end) (caddr pt1))) 2 2)))
             (prompt (strcat "\n + Pipe " (itoa count) 
                             " | L0=" (rtos (distance (list (car pt1) (cadr pt1) 0.0) (list (car pt_end) (cadr pt_end) 0.0)) 2 2)
                             " | dZ=" (rtos (abs (- (caddr pt_end) (caddr pt1))) 2 2)
                             " | L=" (rtos (distance pt1 pt_end) 2 2)))
           )
           (setq pt1 pt_end)
         )
       )
      )
    )
  )

  (command "_.UNDO" "_E")
  (foreach var_pair old_vars (setvar (car var_pair) (cdr var_pair)))
  (setq *error* old_err)
  (if (> count 0) (prompt (strcat "\n-----------------------------\nDA VE: " (itoa count) " PIPE\n")))
  (princ)
)

;;=========================================================
;; CAC LENH
;;=========================================================
(defun c:P0 (/ temp_slope)
  (initget "75 100")
  (setq temp_slope (getkword (strcat "\nChon do nghieng (75 or 100) <1/" (itoa *PS_SlopeDenom*) ">: ")))
  (if temp_slope (setq *PS_SlopeDenom* (atoi temp_slope)))
  (setq *PS_Mode* "Flat")
  (Pipe:DrawSlopeLoop nil "Flat") 
  (princ)
)

(defun c:P_UP (/ temp_slope)
  (initget "75 100")
  (setq temp_slope (getkword (strcat "\nChon do nghieng (75 or 100) <1/" (itoa *PS_SlopeDenom*) ">: ")))
  (if temp_slope (setq *PS_SlopeDenom* (atoi temp_slope)))
  (setq *PS_Mode* "Up")
  (Pipe:DrawSlopeLoop *PS_SlopeDenom* "Up")
  (princ)
)

(defun c:P_DOWN (/ temp_slope)
  (initget "75 100")
  (setq temp_slope (getkword (strcat "\nChon do nghieng (75 or 100) <1/" (itoa *PS_SlopeDenom*) ">: ")))
  (if temp_slope (setq *PS_SlopeDenom* (atoi temp_slope)))
  (setq *PS_Mode* "Down")
  (Pipe:DrawSlopeLoop *PS_SlopeDenom* "Down")
  (princ)
)

(princ "\n[Pipe] PIPE_SLOPE module loaded. Type P0, P_UP, or P_DOWN to start.")
(princ)