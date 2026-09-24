; import_block.lsp - Mẫu AutoLISP để chèn (import) một DWG như một block
; Đặt file này tại thư mục gốc của repository.
;
; Sử dụng: gọi lệnh IMPORTBLOCK trong AutoCAD (load tệp trước bằng APPLOAD hoặc (load "import_block.lsp")).
;
; Tác giả: Copilot (template)
;
(vl-load-com)

(defun c:IMPORTBLOCK ( / blkname filepath inspt)
  (prompt "\n[IMPORTBLOCK] Nhập DWG như block vào bản vẽ hiện hành.")
  (setq blkname (getstring T "\nTên block (không bắt buộc, Enter để dùng tên file): "))
  (setq filepath (getfiled "Chọn file DWG chứa block" "" "dwg" 0))
  (if (or (equal filepath "") (not filepath))
    (progn
      (prompt "\nHủy: không có file được chọn.")
      (princ)
    )
    (progn
      (setq inspt (getpoint "\nChọn điểm chèn: "))
      (if (not inspt) (setq inspt '(0 0 0)))

      ;; Nếu người dùng nhập tên block, dùng tên đó; nếu để trống, AutoCAD sẽ tạo block từ file DWG
      ;; Lệnh -INSERT chấp nhận đường dẫn tới DWG và sẽ chèn nội dung như một block.
      (if (and blkname (not (equal blkname "")))
        (progn
          (command "_.-insert" blkname filepath inspt "1" "1" "1" "0")
          (prompt (strcat "\nĐã chèn block '" blkname "' từ: " filepath))
        )
        (progn
          ;; Nếu không có tên block, truyền trực tiếp file path để AutoCAD chuyển thành block tạm.
          (command "_.-insert" filepath inspt "1" "1" "1" "0")
          (prompt (strcat "\nĐã chèn nội dung từ: " filepath))
        )
      )
      (princ)
    )
  )
  (princ)
)

(princ "\nimport_block.lsp loaded. Gõ IMPORTBLOCK để chạy.")
(princ)
