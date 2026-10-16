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
                     (= (substr str 1 2) "OF")
                     (= (substr str 1 2) "BF")
                     (= (substr str 1 1) "T")
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
                       (= (substr str 1 2) "OF")
                       (= (substr str 1 2) "BF")
                       (= (substr str 1 1) "T")
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
;; HÀM PHÂN LOẠI BALLOON
;;=========================================================
(defun ClassifyBalloon (txt / first_char)
  (if (= txt "") (setq txt "UNKNOWN"))
  (setq first_char (substr txt 1 1))
  (cond
    ((= txt "OF") "OF")
    ((and (> (strlen txt) 2) (= (substr txt 1 2) "OF")) "OF")
    ((= (substr txt 1 2) "BF") txt)
    ((= first_char "T") txt)
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
;; LỆNH CHÍNH: DEMBL
;;=========================================================
(defun c:DEMBL ( / *error* old_vars ss_all lst_ml ent ml_txt ml_type res_dict item sorted_keys key)

  (defun *error* (msg)
    (if old_vars (foreach v old_vars (setvar (car v) (cdr v))))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*EXIT*")))
      (prompt (strcat "\n[Lỗi] " msg)))
    (princ)
  )

  (setq old_vars (list (cons 'OSMODE (getvar "OSMODE"))
                       (cons 'CMDECHO (getvar "CMDECHO"))))
  (setvar "OSMODE" 0) (setvar "CMDECHO" 0)

  (prompt "\nChọn các Balloon (Bỏ qua P): ")
  (setq ss_all (ssget '((0 . "MULTILEADER"))))
  (if (not ss_all) (progn (prompt "\n[!] Không chọn được.") (princ) (exit)))

  (setq lst_ml (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss_all))))
  (prompt (strcat "\nĐã chọn " (itoa (length lst_ml)) " Balloon.\n"))

  ;; XỬ LÝ VÀ ĐẾM
  (setq res_dict '())
  (foreach ml_ent lst_ml
    (setq ml_txt (GetMLeaderText ml_ent))
    (setq ml_type (ClassifyBalloon ml_txt))

    (if (and ml_type (/= ml_type "P") (/= ml_type "OTHER") (/= ml_type "UNKNOWN"))
      (progn
        (setq item (assoc ml_type res_dict))
        (if item
          (setq res_dict (subst (cons ml_type (1+ (cdr item))) item res_dict))
          (setq res_dict (append res_dict (list (cons ml_type 1))))
        )
      )
    )
  )

  ;; SẮP XẾP VÀ HIỂN THỊ
  (setq sorted_keys (vl-sort (mapcar 'car res_dict) '(lambda (a b) (< a b))))

  (prompt "\n--- KẾT QUẢ ---\n")
  (if sorted_keys
    (foreach key sorted_keys
      (setq item (assoc key res_dict))
      (prompt (strcat "  " key ": " (itoa (cdr item)) " cái\n"))
    )
    (prompt "  Không có Balloon nào được đếm.\n")
  )

  (foreach v old_vars (setvar (car v) (cdr v)))
  (princ)
)

(princ "\nGõ DEMBL để chạy lệnh.")
(princ)