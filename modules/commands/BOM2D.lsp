(vl-load-com)

;;=========================================================
;; HÀM LẤY TEXT TỪ MLEADER
;;=========================================================
(defun GetMLeaderText (ml_ent / ed txt str found)
  (setq ed (entget ml_ent) txt "" found nil)
  (foreach pair ed
    (if (= (car pair) 304)
      (progn
        (setq str (cdr pair))
        (if (and str (> (strlen str) 0)
                 (or (= str "OF")
                     (= (substr str 1 1) "P")
                     (= (substr str 1 1) "E")
                     (= (substr str 1 1) "R")
                     (= (substr str 1 1) "F")
                     (= (substr str 1 1) "G")
                     (= (substr str 1 1) "B")))
          (progn (setq txt str) (setq found t))
        )
      )
    )
  )
  (if (not found)
    (foreach pair ed
      (if (= (type (cdr pair)) 'STR)
        (progn
          (setq str (cdr pair))
          (if (and str (> (strlen str) 0)
                   (not (wcmatch str "*LEADER*"))
                   (not (wcmatch str "*{*"))
                   (not (wcmatch str "AcDb*"))
                   (not (wcmatch str "*DIM*"))
                   (or (= str "OF")
                       (= (substr str 1 1) "P")
                       (= (substr str 1 1) "E")
                       (= (substr str 1 1) "R")
                       (= (substr str 1 1) "F")
                       (= (substr str 1 1) "G")
                       (= (substr str 1 1) "B")))
            (progn (setq txt str) (setq found t))
          )
        )
      )
    )
  )
  txt
)

;;=========================================================
;; HÀM LẤY ĐIỂM MŨI TÊN
;;=========================================================
(defun GetArrowPoint (ent)
  (cdr (assoc 10 (entget ent)))
)

;;=========================================================
;; HÀM PHÂN LOẠI BALLOON
;;=========================================================
(defun ClassifyBalloon (txt / first_char)
  (if (= txt "") (setq txt "UNKNOWN"))
  (setq first_char (substr txt 1 1))
  (cond
    ((= txt "OF") "OF")
    ((and (> (strlen txt) 2) (= (substr txt 1 2) "OF")) "OF")
    ((= first_char "P") "P")
    ((= first_char "E") txt)
    ((= first_char "R") txt)
    ((= first_char "F") txt)
    ((= first_char "G") txt)
    ((= first_char "B") txt)
    (t "OTHER")
  )
)

;;=========================================================
;; HÀM FORMAT SỐ
;;=========================================================
(defun FormatNumber (num)
  (if (= (rem num 1) 0)
    (rtos num 2 0)
    (rtos num 2 2)
  )
)

;;=========================================================
;; LỆNH CHÍNH: BOM2D
;;=========================================================
(defun c:BOM2D ( / *error* old_vars tol
                 ss_all lst_ml lst_ln lst_dim
                 ent ent_type
                 ml_txt ml_type arrow_pt
                 ln_p1 ln_p2 ln_mid
                 dim_p1 dim_p2 override_str
                 line_dim_map
                 res_dict item
                 csv f stt key
                 pipe_keys fitting_keys sorted_keys)

  (defun *error* (msg)
    (if old_vars (foreach v old_vars (setvar (car v) (cdr v))))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*EXIT*")))
      (prompt (strcat "\n[Lỗi] " msg)))
    (princ)
  )

  (setq old_vars (list (cons 'OSMODE (getvar "OSMODE"))
                       (cons 'CMDECHO (getvar "CMDECHO"))))
  (setvar "OSMODE" 0) (setvar "CMDECHO" 0)
  (setq tol 0.5)

  (prompt "\n=== QUÉT CHỌN TẤT CẢ ===\n")
  (setq ss_all (ssget '((0 . "MULTILEADER,LINE,DIMENSION"))))
  (if (not ss_all) (progn (prompt "[!] Không chọn được.") (princ) (exit)))

  (setq lst_ml '() lst_ln '() lst_dim '())
  (foreach ent (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss_all)))
    (setq ent_type (cdr (assoc 0 (entget ent))))
    (cond
      ((= ent_type "MULTILEADER") (setq lst_ml (append lst_ml (list ent))))
      ((= ent_type "LINE") (setq lst_ln (append lst_ln (list ent))))
      ((= ent_type "DIMENSION") (setq lst_dim (append lst_dim (list ent))))
    )
  )

  (prompt (strcat "\n[OK] " (itoa (length lst_ml)) " Balloons, "
                  (itoa (length lst_ln)) " Lines, "
                  (itoa (length lst_dim)) " Dims.\n"))

  ;; BƯỚC 1: DIM → LINE
  (setq line_dim_map '())
  (foreach dim_ent lst_dim
    (setq dim_ed (entget dim_ent))
    (setq dim_p1 (cdr (assoc 13 dim_ed)))
    (setq dim_p2 (cdr (assoc 14 dim_ed)))
    (setq override_str (cdr (assoc 1 dim_ed)))
    (if (not override_str) (setq override_str ""))
    (foreach ln_ent lst_ln
      (setq ln_ed (entget ln_ent))
      (setq ln_p1 (cdr (assoc 10 ln_ed)))
      (setq ln_p2 (cdr (assoc 11 ln_ed)))
      (if (or (and (< (distance dim_p1 ln_p1) tol) (< (distance dim_p2 ln_p2) tol))
              (and (< (distance dim_p1 ln_p2) tol) (< (distance dim_p2 ln_p1) tol)))
        (setq line_dim_map (append line_dim_map (list (cons ln_ent override_str))))
      )
    )
  )

  ;; BƯỚC 2: XỬ LÝ BALLOON
  (setq res_dict '())
  (foreach ml_ent lst_ml
    (setq ml_txt (GetMLeaderText ml_ent))
    (setq ml_type (ClassifyBalloon ml_txt))
    (setq arrow_pt (GetArrowPoint ml_ent))
    (cond
      ((= ml_type "P")
       (setq best_line nil best_dist 99999.0)
       (foreach map_item line_dim_map
         (setq ln_ent (car map_item))
         (setq ln_ed (entget ln_ent))
         (setq ln_p1 (cdr (assoc 10 ln_ed)))
         (setq ln_p2 (cdr (assoc 11 ln_ed)))
         (setq ln_mid (list (/ (+ (car ln_p1) (car ln_p2)) 2.0)
                            (/ (+ (cadr ln_p1) (cadr ln_p2)) 2.0)
                            0.0))
         (setq dist (distance arrow_pt ln_mid))
         (if (< dist best_dist) (setq best_line ln_ent best_dist dist))
       )
       (setq pipe_len 0.0)
       (if (and best_line (< best_dist 5.0))
         (progn
           (setq item (assoc best_line line_dim_map))
           (if item (setq pipe_len (atof (cdr item))))
         )
       )
       (prompt (strcat "  " ml_txt " → Length: " (rtos pipe_len 2 2) "\n"))
       (setq item (assoc ml_txt res_dict))
       (if item
         (setq res_dict (subst (cons ml_txt (+ (cdr item) pipe_len)) item res_dict))
         (setq res_dict (append res_dict (list (cons ml_txt pipe_len))))
       )
      )
      ((and ml_type (/= ml_type "OTHER"))
       (setq item (assoc ml_type res_dict))
       (if item
         (setq res_dict (subst (cons ml_type (1+ (cdr item))) item res_dict))
         (setq res_dict (append res_dict (list (cons ml_type 1))))
       )
      )
    )
  )

  ;;=========================================================
  ;; SẮP XẾP: P TRƯỚC, FITTING SAU
  ;;=========================================================
  (setq pipe_keys '() fitting_keys '())
  (foreach key (mapcar 'car res_dict)
    (if (= (substr key 1 1) "P")
      (setq pipe_keys (cons key pipe_keys))
      (setq fitting_keys (cons key fitting_keys))
    )
  )
  
  ;; Sort P theo số thứ tự (P1, P2, P3...)
  (setq pipe_keys (vl-sort pipe_keys '(lambda (a b) 
    (< (atoi (substr a 2)) (atoi (substr b 2))))))
  
  ;; Sort fitting theo alphabet
  (setq fitting_keys (vl-sort fitting_keys '(lambda (a b) (< a b))))
  
  ;; Nối lại: P trước, fitting sau
  (setq sorted_keys (append pipe_keys fitting_keys))

  ;;=========================================================
  ;; XUẤT KẾT QUẢ RA MÀN HÌNH
  ;;=========================================================
  (prompt "\n=== KẾT QUẢ BẢNG KÊ ===\n")
  (foreach key sorted_keys
    (setq item (assoc key res_dict))
    (if (= (substr key 1 1) "P")
      (prompt (strcat "  " key ": " (FormatNumber (cdr item)) " mm\n"))
      (prompt (strcat "  " key ": " (itoa (cdr item)) " cái\n"))
    )
  )

  ;;=========================================================
  ;; XUẤT CSV (KHÔNG BOM, TIÊU ĐỀ GỌN)
  ;;=========================================================
  (setq csv (getfiled "Lưu file CSV" "" "csv" 1))
  (if csv
    (progn
      (setq f (open csv "w") stt 0)
      ;; KHÔNG dùng BOM, dùng encoding ANSI
      (write-line "STT,Ma vat tu,So luong" f)
      (foreach key sorted_keys
        (setq item (assoc key res_dict))
        (setq stt (1+ stt))
        (if (= (substr key 1 1) "P")
          (write-line (strcat (itoa stt) "," key "," (FormatNumber (cdr item)) " mm") f)
          (write-line (strcat (itoa stt) "," key "," (itoa (cdr item)) " cai") f)
        )
      )
      (close f)
      (prompt (strcat "\n[OK] Đã xuất: " csv "\n"))
    )
  )

  (foreach v old_vars (setvar (car v) (cdr v)))
  (princ)
)

(princ "\nGõ BOM2D để chạy lệnh.")
(princ)