# AutoLISP Pipe Tools

## English

This repository contains a set of AutoLISP utilities for pipe-related tasks in AutoCAD.

### What it includes
- Pipe drawing command for creating simple pipe segments with center lines
- 3D pipe creation command based on selected center lines
- Export commands for line metrics and block attribute summaries
- A single loader file for AutoCAD app loading

### Main commands
- LOADPIPETOOLS: load all modules
- PIPE_DRAW: create simple pipe geometry from picked points
- PIPE3D_CREATE: create 3D pipe objects from selected center lines
- PIPE_LENGTH_ORDERED: export line lengths one by one
- PIPE_LENGTH_WINDOW: export line lengths from a selected area
- PIPE_BLOCK_SUMMARY: export block attribute summaries from a selected area
- PIPE_BLOCK_SUMMARY_ALL: export block attribute summaries from the whole drawing
- PLANT_EXTRACT: probe Plant 3D asset metadata and export it to CSV
- PLANT_PROBE: inspect metadata from a single selected object

### Installation
1. Open AutoCAD.
2. Use APPLOAD.
3. Load the file `load_pipe_tools.lsp`.
4. The loader will automatically load all required modules.

### Notes
- The scripts are written for AutoCAD environments that support AutoLISP.
- Some commands depend on standard AutoCAD entities such as LINE and INSERT blocks.
- You can still use the older commands `DPIPE`, `PIPE3D`, `PLEN`, `CBLK`, and `V9` as compatibility aliases.

---

## Tiếng Việt

Kho lưu trữ này chứa các tiện ích AutoLISP dùng cho công việc liên quan đến ống trong AutoCAD.

### Nội dung chính
- Lệnh vẽ ống đơn giản với đường center line
- Lệnh tạo ống 3D từ các đường center line đã chọn
- Các lệnh xuất dữ liệu về độ dài line và tổng hợp thuộc tính block
- Một file loader duy nhất để nạp toàn bộ module vào AutoCAD

### Các lệnh chính
- LOADPIPETOOLS: nạp toàn bộ module
- PIPE_DRAW: tạo hình ống đơn giản từ các điểm chọn
- PIPE3D_CREATE: tạo đối tượng ống 3D từ các line center đã chọn
- PIPE_LENGTH_ORDERED: xuất độ dài line theo từng đối tượng được chọn
- PIPE_LENGTH_WINDOW: xuất độ dài line từ vùng chọn
- PIPE_BLOCK_SUMMARY: xuất tổng hợp thuộc tính block từ vùng chọn
- PIPE_BLOCK_SUMMARY_ALL: xuất tổng hợp thuộc tính block từ toàn bộ bản vẽ
- PLANT_EXTRACT: dò và xuất metadata của Plant 3D asset sang CSV
- PLANT_PROBE: kiểm tra metadata của một đối tượng đã chọn

### Cài đặt
1. Mở AutoCAD.
2. Chọn APPLOAD.
3. Nạp file `load_pipe_tools.lsp`.
4. File loader sẽ tự động nạp các module cần thiết.

### Ghi chú
- Các script này phù hợp với môi trường AutoCAD hỗ trợ AutoLISP.
- Một số lệnh phụ thuộc vào các đối tượng chuẩn của AutoCAD như LINE và block INSERT.
- Bạn vẫn có thể dùng các lệnh cũ `DPIPE`, `PIPE3D`, `PLEN`, `CBLK` và `V9` như alias tương thích.
