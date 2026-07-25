(vl-load-com) ;; Luôn nạp thư viện Visual LISP đầu tiên

;;=========================================================
;; Main loader for AutoCAD (Bulletproof Version - Fixed nil error)
;;=========================================================
(defun Load-Pipe-Tools (/ baseDir safeFindFile fileList fullPath missingFiles)
  
  ;; 1. LẤY ĐƯỜNG DẪN THƯ MỤC MỘT CÁCH AN TOÀN TUYỆT ĐỐI
  (setq baseDir "")
  
  ;; Cách 1: Dùng biến *load* (Hoạt động 99% khi APPLOAD)
  (if *load*
    (setq baseDir (vl-filename-directory *load*))
  )
  
  ;; Cách 2: Fallback nếu *load* bị rỗng
  (if (or (not baseDir) (= baseDir ""))
    (progn
      (setq safeFindFile (findfile "load_pipe_tools.lsp"))
      (if safeFindFile ;; KIỂM TRA NIL TRƯỚC KHI DÙNG vl-filename-directory
        (setq baseDir (vl-filename-directory safeFindFile))
      )
    )
  )
  
  ;; Cách 3: Nếu vẫn không được, để rỗng (sẽ dùng đường dẫn tương đối)
  (if (not baseDir) (setq baseDir ""))

  ;; 2. DANH SÁCH CÁC MODULE CẦN NẠP
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
    ;; Xây dựng đường dẫn an toàn: Nếu baseDir rỗng, chỉ dùng đường dẫn tương đối
    (if (= baseDir "")
      (setq fullPath file)
      (setq fullPath (strcat baseDir "\\" file))
    )
    
    (cond
      ;; Ưu tiên 1: Tìm thấy theo đường dẫn tuyệt đối
      ((findfile fullPath)
       (load fullPath)
       (princ (strcat "\n  [OK] Loaded: " file)))
      
      ;; Ưu tiên 2: Tìm thấy theo đường dẫn tương đối (dự phòng)
      ((findfile file)
       (load file)
       (princ (strcat "\n  [OK] Loaded (relative): " file)))
      
      ;; Ưu tiên 3: Không tìm thấy
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
(princ "\n[Pipe] System ready. Type LOADPIPETOOLS to reload.")
(princ)
