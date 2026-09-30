(vl-load-com)

;;=========================================================
;; HÀM ĐỌC FILE CSV (3 CỘT: STT, FILENAME, BLOCKNAME)
;;=========================================================
(defun ReadCSV (csv_file / f line data_list fields)
  (setq f (open csv_file "r"))
  (if (not f)
    (progn (prompt (strcat "\n[!] Không mở được file: " csv_file)) (exit))
  )
  (setq data_list '())
  (while (setq line (read-line f))
    (if (> (strlen line) 0)
      (progn
        (setq fields (ParseCSVLine line))
        (if (>= (length fields) 3)
          (setq data_list (append data_list 
                                   (list (list (nth 0 fields) (nth 1 fields) (nth 2 fields)))))
          (if (= (length fields) 2)
            (setq data_list (append data_list 
                                     (list (list "0" (nth 0 fields) (nth 1 fields)))))
          )
        )
      )
    )
  )
  (close f)
  data_list
)

;;=========================================================
;; HÀM TÁCH DÒNG CSV
;;=========================================================
(defun ParseCSVLine (line / result field in_quotes ch)
  (setq result '() field "" in_quotes nil)
  (foreach ch (vl-string->list line)
    (cond
      ((= ch 34) (setq in_quotes (not in_quotes)))
      ((and (= ch 44) (not in_quotes))
       (setq result (append result (list field)))
       (setq field ""))
      (t (setq field (strcat field (chr ch))))))
  (if (> (strlen field) 0) (setq result (append result (list field))))
  (mapcar '(lambda (x) (vl-string-trim " " x)) result)
)

;;=========================================================
;; HÀM LẤY ĐUÔI FILE
;;=========================================================
(defun GetFileExtension (filename)
  (strcase (vl-filename-extension filename))
)

;;=========================================================
;; HÀM LÀM SẠCH TÊN BLOCK
;;=========================================================
(defun CleanBlockName (name / result ch valid_chars)
  (setq valid_chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-$")
  (setq result "")
  (foreach ch (vl-string->list name)
    (if (vl-string-search (chr ch) valid_chars)
      (setq result (strcat result (chr ch)))
    )
  )
  (if (= result "") (setq result "BLOCK_DEFAULT"))
  result
)

;;=========================================================
;; HÀM XÓA BLOCK CŨ
;;=========================================================
(defun DeleteExistingBlock (block_name / doc blocks old_block)
  (if (tblsearch "BLOCK" block_name)
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq blocks (vla-get-Blocks doc))
      (setq old_block (vla-Item blocks block_name))
      (vla-Delete old_block)
      (prompt (strcat "\n  [INFO] Đã xóa block cũ: \"" block_name "\""))
    )
  )
)

;;=========================================================
;; HÀM IMPORT FILE 3D (DÙNG LỆNH IMPORT TỔNG QUÁT)
;;=========================================================
(defun Import3DFile (full_path / old_filedia)
  ;; Tắt hộp thoại để chạy tự động
  (setq old_filedia (getvar "FILEDIA"))
  (setvar "FILEDIA" 0)
  
  ;; Dùng lệnh IMPORT (tự động nhận diện định dạng SAT, STL, STEP, IGES, 3DS...)
  (command "_.IMPORT" full_path)
  (while (> (getvar "CMDACTIVE") 0) (command pause))
  
  ;; Khôi phục FILEDIA
  (setvar "FILEDIA" old_filedia)
)

;;=========================================================
;; HÀM IMPORT FILE DWG/DXF
;;=========================================================
(defun ImportDWGFile (full_path / old_filedia)
  (setq old_filedia (getvar "FILEDIA"))
  (setvar "FILEDIA" 0)
  (command "_.-INSERT" full_path "0,0,0" "1" "1" "0")
  (while (> (getvar "CMDACTIVE") 0) (command pause))
  (setvar "FILEDIA" old_filedia)
)

;;=========================================================
;; LỆNH CHÍNH: IMPORT_BLOCKS
;;=========================================================
(defun c:IMPORT_BLOCKS ( / 
  folder_path csv_file data_list
  i filename block_name full_path ext
  ss_solids ss_temp idx new_ent blk_ref dest_x
  clean_name stt_val old_filedia)

  (prompt "\n=== IMPORT FILE VÀ TẠO BLOCK (TỰ ĐỘNG) ===\n")
  
  ;; Lưu và tắt FILEDIA để chạy tự động
  (setq old_filedia (getvar "FILEDIA"))
  (setvar "FILEDIA" 0)
  
  ;; 1. CHỌN THƯ MỤC
  (prompt "\n--- Bước 1: Chọn thư mục chứa các file ---\n")
  (prompt "(Chọn BẤT KỲ file nào trong thư mục đó để lấy đường dẫn)\n")
  (setq folder_path (vl-filename-directory 
                      (getfiled "CHỌN 1 FILE BẤT KỲ TRONG THƯ MỤC" "" "*" 8)))
  
  (if (not folder_path)
    (progn (prompt "[!] Đã hủy.") (setvar "FILEDIA" old_filedia) (princ) (exit))
  )
  (prompt (strcat "=> Thư mục: " folder_path "\n"))
  
  ;; 2. CHỌN FILE CSV
  (prompt "\n--- Bước 2: Chọn file danh sách (CSV) ---\n")
  (setq csv_file (getfiled "Chọn file CSV" "" "csv" 8))
  (if (not csv_file) (progn (prompt "[!] Đã hủy.") (setvar "FILEDIA" old_filedia) (princ) (exit)))
  (prompt (strcat "=> File CSV: " csv_file "\n"))
  
  ;; 3. ĐỌC DANH SÁCH TỪ CSV
  (prompt "\n--- Bước 3: Đọc danh sách từ CSV ---\n")
  (setq data_list (ReadCSV csv_file))
  
  (if (= (length data_list) 0)
    (progn 
      (prompt "[!] File CSV rỗng hoặc lỗi định dạng.") 
      (setvar "FILEDIA" old_filedia)
      (princ) 
      (exit)
    )
  )
  
  (prompt (strcat "\n  [OK] Tìm thấy " (itoa (length data_list)) " mục trong CSV\n"))
  
  ;; SẮP XẾP THEO STT TĂNG DẦN
  (setq data_list (vl-sort data_list '(lambda (a b) (< (atoi (car a)) (atoi (car b))))))
  
  ;; HIỂN THỊ DANH SÁCH
  (prompt "\n  Danh sách sẽ import (theo thứ tự STT):\n")
  (setq i 0)
  (foreach item data_list
    (setq i (1+ i))
    (setq stt_val (car item))
    (setq filename (nth 1 item))
    (setq clean_name (CleanBlockName (nth 2 item)))
    (setq full_path (strcat folder_path "\\" filename))
    (if (findfile full_path)
      (prompt (strcat "    [" (itoa i) "] STT=" stt_val " | File: " filename " -> Block: " clean_name " [OK]\n"))
      (prompt (strcat "    [" (itoa i) "] STT=" stt_val " | File: " filename " -> Block: " clean_name " [KHÔNG TÌM THẤY]\n"))
    )
  )
  
  ;; 4. DUYỆT VÀ IMPORT
  (prompt "\n--- Bước 4: Đang xử lý tự động... ---\n")
  (setq i 0)
  
  (foreach item data_list
    (setq stt_val (car item))
    (setq filename (nth 1 item))
    (setq block_name (nth 2 item))
    (setq clean_name (CleanBlockName block_name))
    (setq full_path (strcat folder_path "\\" filename))
    (setq ext (GetFileExtension filename))
    
    (if (findfile full_path)
      (progn
        (prompt (strcat "\n[" (itoa (1+ i)) "/" (itoa (length data_list)) "] STT=" stt_val " | " 
                        filename " -> " clean_name))
        
        ;; === BẮT ĐẦU NHÓM UNDO ===
        (command "_.UNDO" "_BE")
        
        ;; 1. Import file
        (if (or (= ext ".DWG") (= ext ".DXF"))
          (ImportDWGFile full_path)
          (Import3DFile full_path)
        )
        
        ;; 2. CHỌN ĐỐI TƯỢNG
        (setq ss_solids (ssadd))
        
        (if (or (= ext ".DWG") (= ext ".DXF"))
          (progn
            (setq new_ent (entlast))
            (if new_ent (ssadd new_ent ss_solids))
          )
          (progn
            (setq ss_temp (ssget "_X" '((0 . "3DSOLID"))))
            (if ss_temp
              (progn
                (setq idx 0)
                (repeat (sslength ss_temp)
                  (ssadd (ssname ss_temp idx) ss_solids)
                  (setq idx (1+ idx))
                )
              )
            )
          )
        )
        
        ;; 3. TẠO BLOCK
        (if (> (sslength ss_solids) 0)
          (progn
            (DeleteExistingBlock clean_name)
            
            (command "_.-BLOCK" clean_name "0,0,0" ss_solids "")
            (while (> (getvar "CMDACTIVE") 0) (command pause))
            
            (command "_.-INSERT" clean_name "0,0,0" "1" "1" "0")
            (while (> (getvar "CMDACTIVE") 0) (command pause))
            
            (setq blk_ref (entlast))
            
            ;; 4. Tính vị trí MOVE
            (if (= i 0)
              (setq dest_x 0.0)
              (setq dest_x (+ 1000.0 (* (1- i) 100.0)))
            )
            
            ;; 5. MOVE Block
            (if (> dest_x 0.0)
              (progn
                (command "_.MOVE" blk_ref "" "0,0,0" (list dest_x 0.0 0.0))
                (while (> (getvar "CMDACTIVE") 0) (command pause))
              )
            )
            
            (prompt (strcat "  => OK (X=" (rtos dest_x 2 0) ")"))
          )
          (prompt "\n  => [!] Lỗi: Không tìm thấy đối tượng sau khi import.")
        )
        
        ;; === KẾT THÚC NHÓM UNDO ===
        (command "_.UNDO" "_E")
        
        (setq i (1+ i))
      )
      (progn
        (prompt (strcat "\n[" (itoa (1+ i)) "] [!] Không tìm thấy file: " filename))
        (setq i (1+ i))
      )
    )
  )
  
  ;; Khôi phục FILEDIA
  (setvar "FILEDIA" old_filedia)
  
  (prompt "\n\n=== HOÀN THÀNH ===\n")
  (prompt (strcat "Đã xử lý " (itoa i) " file.\n"))
  (princ)
)

(princ "\nGõ IMPORT_BLOCKS để chạy lệnh.")
(princ)
