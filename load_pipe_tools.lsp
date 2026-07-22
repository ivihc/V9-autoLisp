;;=========================================================
;; Main loader for AutoCAD
;;=========================================================
 (defun Load-Pipe-Tools (/ baseDir commonFile exportFile drawFile pipe3dFile plantFile hierarchyFile)
  ;; Try to locate the loader file directory; fall back to empty string to avoid nil in strcat
  (setq baseDir (vl-filename-directory (findfile "load_pipe_tools.lsp")))
  (if (not baseDir) (setq baseDir ""))

  ;; Build module paths (works even if baseDir is empty)
  (setq commonFile (strcat baseDir "\\modules\\common\\pipe_common.lsp"))
  (setq exportFile (strcat baseDir "\\modules\\commands\\pipe_export.lsp"))
  (setq drawFile (strcat baseDir "\\modules\\commands\\pipe_draw_simple.lsp"))
  (setq pipe3dFile (strcat baseDir "\\modules\\commands\\pipe3d.lsp"))
  (setq plantFile (strcat baseDir "\\modules\\commands\\plant_asset_extractor.lsp"))
  (setq hierarchyFile (strcat baseDir "\\modules\\commands\\pipe_block_hierarchy.lsp"))

  ;; Try to load each module: first check full path, then try relative path if not found
  (if (findfile commonFile)
    (load commonFile)
    (if (findfile "modules\\common\\pipe_common.lsp") (load "modules\\common\\pipe_common.lsp") (princ "\n[Pipe] Missing common module.")))

  (if (findfile exportFile)
    (load exportFile)
    (if (findfile "modules\\commands\\pipe_export.lsp") (load "modules\\commands\\pipe_export.lsp") (princ "\n[Pipe] Missing export module.")))

  (if (findfile drawFile)
    (load drawFile)
    (if (findfile "modules\\commands\\pipe_draw_simple.lsp") (load "modules\\commands\\pipe_draw_simple.lsp") (princ "\n[Pipe] Missing drawing module.")))

  (if (findfile pipe3dFile)
    (load pipe3dFile)
    (if (findfile "modules\\commands\\pipe3d.lsp") (load "modules\\commands\\pipe3d.lsp") (princ "\n[Pipe] Missing PIPE3D module.")))

  (if (findfile plantFile)
    (load plantFile)
    (if (findfile "modules\\commands\\plant_asset_extractor.lsp") (load "modules\\commands\\plant_asset_extractor.lsp") (princ "\n[Pipe] Missing Plant module.")))

  (if (findfile hierarchyFile)
    (load hierarchyFile)
    (if (findfile "modules\\commands\\pipe_block_hierarchy.lsp") (load "modules\\commands\\pipe_block_hierarchy.lsp") (princ "\n[Pipe] Missing hierarchy module.")))

  (princ "\n[Pipe] All modules loaded. Use PIPE_DRAW, PIPE3D_CREATE, PLANT_EXTRACT, PIPE_LENGTH_WINDOW, PIPE_BLOCK_SUMMARY, BLKHIER, or PIPETOOLS."))

(defun c:LOADPIPETOOLS ()
  (Load-Pipe-Tools)
  (princ))

;; Auto-load when the file is APPLOAD'd in AutoCAD
(Load-Pipe-Tools)
(princ "\n[Pipe] Loader ready. Type LOADPIPETOOLS if you want to reload modules.")
(princ)
