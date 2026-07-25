# AutoLISP Pipe Tools for AutoCAD and Plant 3D

A practical collection of AutoLISP utilities for piping and plant design workflows in AutoCAD. This project helps users create pipe geometry, export line length data, summarize block attributes, and extract Plant 3D asset metadata into CSV files.

Developed by ivihc for practical piping and plant design automation tasks.

## Keywords
AutoCAD AutoLISP, pipe tools, piping utilities, Plant 3D, block attribute export, line length export, CSV export, AutoCAD script, pipe drawing, 3D pipe generation.

## English

This repository contains a set of AutoLISP utilities for pipe-related tasks in AutoCAD and Plant 3D environments.

### What it includes
- Pipe drawing command for creating simple pipe segments with center lines
- 3D pipe creation command based on selected center lines
- Export commands for line metrics and block attribute summaries
- A block hierarchy analyzer for nested block structures and quantity calculation
- A single loader file for AutoCAD app loading

### Main commands
- LOADPIPETOOLS: load all modules
- PIPE_DRAW: create simple pipe geometry from picked points
- PIPE3D_CREATE: create 3D pipe objects from selected center lines
- PIPE_LENGTH_ORDERED: export line lengths one by one
- PIPE_LENGTH_WINDOW: export line lengths from a selected area
- PIPE_BLOCK_SUMMARY: export block attribute summaries from a selected area
- PIPE_BLOCK_SUMMARY_ALL: export block attribute summaries from the whole drawing
- BLKHIER: analyze nested block hierarchy and export a CSV report
- PLANT_EXTRACT: probe Plant 3D asset metadata and export it to CSV
- PLANT_PROBE: inspect metadata from a single selected object

- PIPE_SLOPE / PSLOPE: draw pipe segments with a controlled slope (1/75 or 1/100). Supports direction `Up`, `Down`, or `Horizontal`, accepts length as horizontal projection (`L0`) or actual pipe length (`L`), and automatically detects vertical (Z) segments. Segments chain automatically (end of one becomes start of next).

- BOOM / Section Planes: read section plane coordinates from an Excel file and place section blocks in correct UCS orientation. Adds `SP_READ` (read Excel) and `SP_PLACE` (scan and place blocks) commands. Use `getfiled` to select the Excel file; block names must follow the format `T/F/B/R/L` + number (e.g. `T1`, `F2`).

Usage example:
1. APPLOAD `load_pipe_tools.lsp`
2. Run `PSLOPE` (or `PIPE_SLOPE`)
3. Follow prompts: diameter, slope (75/100), pick start point and direction, choose `Up/Down/Horizontal`, enter `L0` or `L` when requested.

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

Kho lưu trữ này chứa các tiện ích AutoLISP dùng cho công việc liên quan đến ống trong AutoCAD và môi trường Plant 3D. Dự án này hỗ trợ tạo hình ống, xuất dữ liệu độ dài line, tổng hợp thuộc tính block và trích xuất metadata của Plant 3D asset sang file CSV.

### Nội dung chính
- Lệnh vẽ ống đơn giản với đường center line
- Lệnh tạo ống 3D từ các đường center line đã chọn
- Các lệnh xuất dữ liệu về độ dài line và tổng hợp thuộc tính block
- Công cụ phân tích cấu trúc block lồng nhau và tính số lượng vật tư
- Một file loader duy nhất để nạp toàn bộ module vào AutoCAD

### Các lệnh chính
- LOADPIPETOOLS: nạp toàn bộ module
- PIPE_DRAW: tạo hình ống đơn giản từ các điểm chọn
- PIPE3D_CREATE: tạo đối tượng ống 3D từ các line center đã chọn
- PIPE_LENGTH_ORDERED: xuất độ dài line theo từng đối tượng được chọn
- PIPE_LENGTH_WINDOW: xuất độ dài line từ vùng chọn
- PIPE_BLOCK_SUMMARY: xuất tổng hợp thuộc tính block từ vùng chọn
- PIPE_BLOCK_SUMMARY_ALL: xuất tổng hợp thuộc tính block từ toàn bộ bản vẽ
- BLKHIER: phân tích cấu trúc block lồng nhau và xuất báo cáo CSV
- PLANT_EXTRACT: dò và xuất metadata của Plant 3D asset sang CSV
- PLANT_PROBE: kiểm tra metadata của một đối tượng đã chọn

- BLKHIER: phân tích cấu trúc block lồng nhau và xuất báo cáo CSV
- PIPE_SLOPE / PSLOPE: vẽ đoạn ống có độ nghiêng cố định (1/75 hoặc 1/100). Hỗ trợ hướng `Up`, `Down`, hoặc `Horizontal`, cho phép nhập chiều dài theo ảnh chiếu ngang (`L0`) hoặc chiều dài thực (`L`) và tự động nhận diện đoạn đứng (theo Z). Các đoạn nối tiếp sẽ tự động nối tiếp nhau.

- BOOM / Section Planes: đọc tọa độ các mặt cắt từ file Excel và đặt các block mặt cắt về đúng vị trí và hướng bằng cách thiết lập UCS tương ứng. Thêm các lệnh `SP_READ` (đọc Excel) và `SP_PLACE` (quét và đặt block). Dùng `getfiled` để chọn file Excel; tên block phải theo định dạng `T/F/B/R/L` + số (ví dụ `T1`, `F2`).

Ví dụ sử dụng:
1. APPLOAD `load_pipe_tools.lsp`
2. Gõ `PSLOPE` (hoặc `PIPE_SLOPE`)
3. Thực hiện theo hướng dẫn: nhập đường kính, chọn slope (75/100), chọn điểm bắt đầu và hướng, chọn `Up/Down/Horizontal`, và nhập `L0` hoặc `L` khi được hỏi.

### Cài đặt
1. Mở AutoCAD.
2. Chọn APPLOAD.
3. Nạp file `load_pipe_tools.lsp`.
4. File loader sẽ tự động nạp các module cần thiết.

### Ghi chú
- Các script này phù hợp với môi trường AutoCAD hỗ trợ AutoLISP.
- Một số lệnh phụ thuộc vào các đối tượng chuẩn của AutoCAD như LINE và block INSERT.
- Bạn vẫn có thể dùng các lệnh cũ `DPIPE`, `PIPE3D`, `PLEN`, `CBLK` và `V9` như alias tương thích.
