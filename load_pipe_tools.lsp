(vl-load-com) ;; Luôn nạp thư viện Visual LISP đầu tiên

;;=========================================================
;; Main loader for AutoCAD (Bulletproof Version)
;;=========================================================
(defun Load-Pipe-Tools (/ baseDir fileList fullPath missingFiles)
  
  ;; 1. LẤY ĐƯỜNG DẪN THƯ MỤC CHA MỘT CÁCH CHÍNH XÁC NHẤT
  ;; Biến *load* chứa tên file đang được thực thi (load)
  (setq baseDir (vl-filename-directory *load*))
  
  ;; Fallback: Nếu *load* không hoạt động (hiếm gặp), dùng cách cũ
  (if (or (not baseDir) (= baseDir ""))
    (setq baseDir (vl-filename-directory (findfile "load_pipe_tools.lsp")))
  )
  
  ;; Đảm bảo baseDir không phải là nil để tránh lỗi strcat
  (if (not baseDir) (setq baseDir ""))

  ;; 2. DANH SÁCH CÁC MODULE CẦN NẠP (Dễ dàng thêm/bớt)
  (setq fileList '(
    "modules\\common\\pipe_common.lsp"
    "modules\\commands\\pipe_export.lsp"
    "modules\\commands\\pipe_draw_simple.lsp"
    "modules\\commands\\pipe3d.lsp"
    "modules\\commands\\plant_asset_extractor.lsp"
    "modules\\commands\\pipe_block_hierarchy.lsp"
  ))

  (setq missingFiles nil)
  (princ "\n[Pipe] === Starting Module Loader ===")

  ;; 3. VÒNG LẶP TỰ ĐỘNG NẠP FILE
  (foreach file fileList
    ;; Tạo đường dẫn tuyệt đối: [Thu_muc_loader] + "\\" + [đường_dẫn_tương_đối]
    (setq fullPath (strcat baseDir "\\" file))
    
    (cond
      ;; Ưu tiên 1: Tìm thấy theo đường dẫn tuyệt đối từ file loader (Chính xác nhất)
      ((findfile fullPath)
       (load fullPath)
       (princ (strcat "\n  [OK] Loaded: " file)))
      
      ;; Ưu tiên 2: Tìm thấy theo đường dẫn tương đối (Dự phòng nếu baseDir bị sai)
      ((findfile file)
       (load file)
       (princ (strcat "\n  [OK] Loaded (relative): " file)))
      
      ;; Ưu tiên 3: Không tìm thấy -> Ghi nhận lỗi
      (t
       (princ (strcat "\n  [FAIL] Missing: " file))
       (setq missingFiles (cons file missingFiles)))
    )
  )

  ;; 4. BÁO CÁO KẾT QUẢ
  (princ "\n[Pipe] === Loader Finished ===")
  (if missingFiles
    (progn
      (princ "\n[Pipe] WARNING: Một số module KHÔNG được nạp!")
      (princ "\n[Pipe] Vui lòng kiểm tra lại cấu trúc thư mục:")
      (foreach f (reverse missingFiles) 
        (princ (strcat "\n  -> " f)))
      (princ "\n[Pipe] Gợi ý: Đảm bảo thư mục 'modules' nằm cùng cấp với file loader.")
    )
    (princ "\n[Pipe] SUCCESS: Tất cả modules đã được nạp thành công!")
  )
  
  (princ)
)

;;=========================================================
;; Lệnh gọi lại Loader
;;=========================================================
(defun c:LOADPIPETOOLS ()
  (Load-Pipe-Tools)
  (princ)
)

;;=========================================================
;; Tự động chạy khi file này được APPLOAD
;;=========================================================
(Load-Pipe-Tools)
(princ "\n[Pipe] System ready. Type LOADPIPETOOLS to reload, or use specific commands.")
(princ)
