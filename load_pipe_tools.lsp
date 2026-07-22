;;=========================================================
;; Main loader for AutoCAD
;;=========================================================
(defun Load-Pipe-Tools (/ baseDir commonFile exportFile drawFile pipe3dFile)
  (setq baseDir (vl-filename-directory (findfile "load_pipe_tools.lsp")))
  (if (not baseDir) (setq baseDir ""))

  (setq commonFile (strcat baseDir "\\modules\\common\\pipe_common.lsp"))
  (setq exportFile (strcat baseDir "\\modules\\commands\\pipe_export.lsp"))
  (setq drawFile (strcat baseDir "\\modules\\commands\\pipe_draw_simple.lsp"))
  (setq pipe3dFile (strcat baseDir "\\modules\\commands\\pipe3d.lsp"))

  (if (findfile commonFile) (load commonFile) (princ "\n[Pipe] Missing common module."))
  (if (findfile exportFile) (load exportFile) (princ "\n[Pipe] Missing export module."))
  (if (findfile drawFile) (load drawFile) (princ "\n[Pipe] Missing drawing module."))
  (if (findfile pipe3dFile) (load pipe3dFile) (princ "\n[Pipe] Missing PIPE3D module."))

  (princ "\n[Pipe] All modules loaded. Type DPIPE, PIPE3D, PLEN, CBLK, or V9."))

(defun c:LOADPIPETOOLS ()
  (Load-Pipe-Tools)
  (princ))

;; Auto-load when the file is APPLOAD'd in AutoCAD
(Load-Pipe-Tools)
(princ "\n[Pipe] Loader ready. Type LOADPIPETOOLS if you want to reload modules.")
(princ)
