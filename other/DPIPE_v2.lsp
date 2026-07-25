(vl-load-com)

;;;================================================================
;;; BANG DATA ELBOW
;;;================================================================
(setq v9:ElbowData
  '(
    (10    17.2    25.0) (15    21.7    30.0) (20    27.2    40.0)
    (25    34.0    50.0) (32    42.7    60.0) (40    48.6    75.0)
    (50    60.5    90.0) (65    76.3   110.0) (80    89.1   135.0)
    (100  114.2   170.0) (125  140.0   210.0) (150  165.2   225.0)
    (200  216.3   300.0) (250  267.0   375.0) (300  318.0   450.0)
   )
)

;;;================================================================
;;; HAM HELPER
;;;================================================================
(defun v9:cross (u v)
  (list (- (* (cadr u) (caddr v)) (* (caddr u) (cadr v)))
        (- (* (caddr u) (car v)) (* (car u) (caddr v)))
        (- (* (car u) (cadr v)) (* (cadr u) (car v)))))

(defun v9:dot (u v)
  (+ (* (car u) (car v)) (* (cadr u) (cadr v)) (* (caddr u) (caddr v))))

(defun v9:vlen (v) (sqrt (v9:dot v v)))

(defun v9:vnorm (v / l)
  (setq l (v9:vlen v))
  (if (> l 1e-10) (mapcar '(lambda (x) (/ x l)) v) '(0.0 0.0 0.0)))

(defun v9:acos (x)
  (cond ((>= x 1.0) 0.0) ((<= x -1.0) pi) (t (atan (sqrt (- 1.0 (* x x))) x))))
(defun v9:tan (x) (/ (sin x) (cos x)))

(defun v9:GetElbowRadius (diam / best_entry best_diff diff entry od)
  (setq best_entry nil best_diff 1e9)
  (foreach entry v9:ElbowData
    (setq od (nth 1 entry) diff (abs (- od diam)))
    (if (< diff best_diff) (setq best_diff diff best_entry entry)))
  (if best_entry (nth 2 best_entry) nil))

;;;================================================================
;;; HAM: TAO ELBOW (PHUONG PHAP 2 UCS - CHINH XAC TUYET DOI)
;;;================================================================
(defun v9:CreateElbow (P V1 V2 r_pipe R_elbow /
                        angle neg_V1 tan_len A B
                        bisector bis_len C normal
                        arc_ent circle_ent ucs_name)
  
  (setq neg_V1 (mapcar '- V1))
  (setq angle (v9:acos (v9:dot neg_V1 V2)))
  
  (if (or (< angle 0.087) (> angle (- pi 0.087)))
    (progn (prompt "\n[!] Goc khong hop le") (exit)))
  
  ;; 1. Tinh toan hinh hoc co ban
  (setq tan_len (/ R_elbow (v9:tan (/ angle 2.0))))
  (setq A (mapcar '+ P (mapcar '* neg_V1 (list tan_len tan_len tan_len))))
  (setq B (mapcar '+ P (mapcar '* V2 (list tan_len tan_len tan_len))))
  
  (setq bisector (v9:vnorm (mapcar '+ neg_V1 V2)))
  (setq bis_len (/ R_elbow (sin (/ angle 2.0))))
  (setq C (mapcar '+ P (mapcar '* bisector (list bis_len bis_len bis_len))))
  
  ;; Vector phap tuyen cua mat phang chua elbow
  (setq normal (v9:vnorm (v9:cross (v9:vnorm (mapcar '- A C)) (v9:vnorm (mapcar '- B C)))))
  
  (setq ucs_name "ELBOW_UCS_TEMP")
  (if (tblsearch "UCS" ucs_name) (command "_.UCS" "_D" ucs_name))
  
  ;; 2. TAO UCS 1: De ve ARC (Duong dan)
  ;; Origin tai C, truc X huong den A, truc Y huong theo normal
  (command "_.UCS" "_3" "_non" C "_non" A "_non" (mapcar '+ C normal))
  (command "_.UCS" "_S" ucs_name)
  
  ;; Ve ARC trong UCS nay: Center tai (0,0,0), Start tai (R,0,0), Quet 1 goc = angle
  (command "_.ARC" "_C" "_non" (list 0.0 0.0 0.0) "_non" (list R_elbow 0.0 0.0) "_A" angle)
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (setq arc_ent (entlast))
  
  (if (null arc_ent)
    (progn (prompt "\n[!] Loi tao ARC") (command "_.UCS" "_World") (exit)))
  
  ;; 3. TAO UCS 2: De ve CIRCLE (Mat cat ngang) tai diem A
  ;; Chung ta can mat cat ngang vuong goc voi tiep tuyen cua ARC tai A.
  ;; Trong UCS 1, tiep tuyen tai A dang huong theo truc +Y.
  ;; Nen ta dat UCS moi tai A, truc X huong ve C, truc Y huong theo normal.
  ;; Khi do truc Z cua UCS 2 se trung voi tiep tuyen -> Ve circle se dung huong.
  (command "_.UCS" "_3" "_non" A "_non" C "_non" (mapcar '+ A normal))
  
  ;; Ve CIRCLE profile
  (command "_.CIRCLE" "_non" (list 0.0 0.0 0.0) r_pipe)
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (setq circle_ent (entlast))
  
  ;; 4. SWEEP
  (command "_.SWEEP" circle_ent "" "_P" arc_ent)
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  
  ;; 5. Don dep
  (if (entget arc_ent) (entdel arc_ent))
  (if (entget circle_ent) (entdel circle_ent))
  
  (command "_.UCS" "_World")
  (if (tblsearch "UCS" ucs_name) (command "_.UCS" "_D" ucs_name))
  
  (list A B)
)

;;;================================================================
;;; LENH: DPIPE
;;;================================================================
(defun c:DPIPE ( / diam radius R_elbow pt1 pt2
                   old_osmode old_cmdecho old_solidhist
                   vec_prev vec_curr elbow_pts A B
                   pipe_start last_pipe_start
                   ent_pipe_prev ent_cl_prev
                   *error* old_error count h)
  
  (setq old_error *error*)
  (defun *error* (msg)
    (if (and msg (not (wcmatch msg "Function cancelled,quit / exit abort")))
      (prompt (strcat "\n[Error] " msg)))
    (if (= (getvar "CMDACTIVE") 0)
      (progn
        (if old_osmode (setvar "OSMODE" old_osmode))
        (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
        (if old_solidhist (setvar "SOLIDHIST" old_solidhist))
        (command "_.UNDO" "_E")
        (command "_.UCS" "_World")
      )
    )
    (setq *error* old_error)
    (princ))
  
  (initget 6)
  (setq diam (getreal "\nNhap duong kinh ong <50>: "))
  (if (null diam) (setq diam 50.0))
  (setq radius (/ diam 2.0))
  
  (setq R_elbow (v9:GetElbowRadius diam))
  (if (null R_elbow) (progn (prompt "\n[!] Khong tim thay R_elbow") (exit)))
  (prompt (strcat "\n[OK] D=" (rtos diam 2 2) " mm -> R_elbow=" (rtos R_elbow 2 2) " mm"))
  
  (setq old_osmode (getvar "OSMODE"))
  (setq old_cmdecho (getvar "CMDECHO"))
  (setq old_solidhist (getvar "SOLIDHIST"))
  (setvar "OSMODE" 0)
  (setvar "CMDECHO" 0)
  (setvar "SOLIDHIST" 1)
  (command "_.UNDO" "_BE")
  
  (setq pt1 (getpoint "\nChon diem dau tien: "))
  (if (null pt1) (progn (command "_.UNDO" "_E") (setq *error* old_error) (exit)))
  
  (setq pipe_start pt1
        last_pipe_start pt1
        vec_prev nil
        ent_pipe_prev nil
        ent_cl_prev nil
        count 0)
  
  (while (setq pt2 (getpoint pipe_start "\nChon diem tiep theo [Enter de ket thuc]: "))
    (setq vec_curr (v9:vnorm (mapcar '- pt2 pipe_start)))
    (setq h (distance pipe_start pt2))
    
    (if (> h 0.001)
      (progn
        (if (and vec_prev (> (v9:vlen (v9:cross vec_prev vec_curr)) 0.087))
          ;; === CO ELBOW ===
          (progn
            (prompt "\n + Tao Elbow...")
            (setq elbow_pts (v9:CreateElbow pipe_start vec_prev vec_curr radius R_elbow))
            (setq A (car elbow_pts))
            (setq B (cadr elbow_pts))
            
            (if ent_pipe_prev (entdel ent_pipe_prev))
            (if ent_cl_prev (entdel ent_cl_prev))
            
            ;; Ve doan ong truoc elbow
            (if (> (distance last_pipe_start A) 0.001)
              (progn
                (command "_.CYLINDER" "_non" last_pipe_start radius "_AXis" "_non" last_pipe_start "_non" A)
                (while (> (getvar "CMDACTIVE") 0) (command ""))
                (setq ent_pipe_prev (entlast))
                
                (command "_.LINE" "_non" last_pipe_start "_non" A "")
                (while (> (getvar "CMDACTIVE") 0) (command ""))
                (setq ent_cl_prev (entlast))
              ))
            
            ;; Ve doan ong sau elbow
            (if (> (distance B pt2) 0.001)
              (progn
                (command "_.CYLINDER" "_non" B radius "_AXis" "_non" B "_non" pt2)
                (while (> (getvar "CMDACTIVE") 0) (command ""))
                (setq ent_pipe_prev (entlast))
                
                (command "_.LINE" "_non" B "_non" pt2 "")
                (while (> (getvar "CMDACTIVE") 0) (command ""))
                (setq ent_cl_prev (entlast))
                
                (setq count (1+ count))
                (prompt (strcat "\n + Pipe " (itoa count) " L=" (rtos (distance B pt2) 2 2)))
              )
            )
            
            (setq last_pipe_start B)
            (setq pipe_start pt2)
            (setq vec_prev vec_curr)
          )
          ;; === KHONG CO ELBOW ===
          (progn
            (if ent_pipe_prev (entdel ent_pipe_prev))
            (if ent_cl_prev (entdel ent_cl_prev))
            
            (if (> (distance last_pipe_start pt2) 0.001)
              (progn
                (command "_.CYLINDER" "_non" last_pipe_start radius "_AXis" "_non" last_pipe_start "_non" pt2)
                (while (> (getvar "CMDACTIVE") 0) (command ""))
                (setq ent_pipe_prev (entlast))
                
                (command "_.LINE" "_non" last_pipe_start "_non" pt2 "")
                (while (> (getvar "CMDACTIVE") 0) (command ""))
                (setq ent_cl_prev (entlast))
                
                (setq count (1+ count))
                (prompt (strcat "\n + Pipe " (itoa count) " L=" (rtos (distance last_pipe_start pt2) 2 2)))
              )
            )
            
            (setq pipe_start pt2)
            (setq vec_prev vec_curr)
          )
        )
      )
      (prompt "\n[!] Doan qua ngan.")
    )
  )
  
  (command "_.UNDO" "_E")
  (setvar "OSMODE" old_osmode)
  (setvar "CMDECHO" old_cmdecho)
  (setvar "SOLIDHIST" old_solidhist)
  
  (prompt (strcat "\n-----------------------------\nDA VE: " (itoa count) " ELBOW/PIPE"))
  (setq *error* old_error)
  (princ)
)