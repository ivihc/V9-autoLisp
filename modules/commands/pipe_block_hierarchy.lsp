;;=========================================================
;; PIPE BLOCK HIERARCHY ANALYZER
;; Analyze nested block structure and calculate quantities
;;=========================================================

(defun Pipe:SelectBlockInstance (/ ent obj name)
  (prompt "\nSelect a block instance to analyze: ")
  (if (setq ent (car (entsel)))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (if (= (vla-get-ObjectName obj) "AcDbBlockReference")
        (progn
          (setq name (vla-get-EffectiveName obj))
          name)
        (progn
          (Pipe:Warn "Selected object is not a block reference.")
          nil)))
    (progn
      (Pipe:Warn "No object selected.")
      nil)))

(defun Pipe:AnalyzeBlockStructure (rootBlkName / visited)
  (setq visited nil)
  (Pipe:TraverseBlockDef rootBlkName 1 1 "" visited))

(defun Pipe:TraverseBlockDef (currentBlkName currentDepth multiplier currentPath visited / blkDef ent entData childName childCount childMultiplier newPath subResult results)
  (if (member currentBlkName visited)
    (progn
      (Pipe:Warn (strcat "Circular reference detected at: " currentBlkName " -> skipping."))
      nil)
    (progn
      (setq visited (cons currentBlkName visited))
      (setq blkDef (tblsearch "BLOCK" currentBlkName))
      (if blkDef
        (progn
          (setq ent (entnext (cdr (assoc -1 blkDef)))
                results nil)
          (while ent
            (setq entData (entget ent))
            (if (= (cdr (assoc 0 entData)) "INSERT")
              (progn
                (setq childName (cdr (assoc 2 entData)))
                ;; Guard against missing or empty childName
                (if (and childName (> (strlen childName) 0) (= (substr childName 1 1) "*"))
                  (setq childName (cdr (assoc 3 entData))))

                (if (and childName (> (strlen childName) 0))
                  (progn
                    (setq childCount 1
                          childMultiplier (* multiplier childCount)
                          newPath (if (equal currentPath "")
                                    childName
                                    (strcat currentPath " -> " childName)))

                    (setq results (cons (list currentBlkName childName childCount childMultiplier currentDepth newPath) results))

                    (setq subResult (Pipe:TraverseBlockDef childName (1+ currentDepth) childMultiplier newPath visited))
                    (if subResult
                      (setq results (append results subResult))))
                  ;; else: missing childName, skip and warn
                  (Pipe:Warn (strcat "Found INSERT without block name inside: " currentBlkName ". Skipping entity."))))
              )
            (setq ent (entnext ent)))
          results)
        (progn
          (Pipe:Warn (strcat "Block definition not found: " currentBlkName))
          nil))))

(defun Pipe:MakeIndent (depth / indent)
  (setq indent "")
  (repeat depth (setq indent (strcat indent "    ")))
  indent)

(defun Pipe:PrintTree (data rootName / lastDepth)
  (prompt (strcat "\n--- BLOCK HIERARCHY TREE: " rootName " ---\n"))
  (setq lastDepth 0)
  (foreach item data
    (let* ((child (nth 1 item))
           (direct (nth 2 item))
           (total (nth 3 item))
           (depth (nth 4 item))
           (indent (Pipe:MakeIndent (1- depth))))
      (if (> depth lastDepth)
        (prompt "\n"))
      (prompt (strcat indent "└─ " child " [Direct: " (itoa direct) " | Total: " (itoa total) "]\n"))
      (setq lastDepth depth)))
  (prompt "------------------------------------------\n"))

(defun Pipe:BuildHierarchyCsvData (data / header rows)
  (setq header (strcat "Parent_Block" Pipe:*Sep*
                       "Child_Block" Pipe:*Sep*
                       "Direct_Count" Pipe:*Sep*
                       "Total_Quantity" Pipe:*Sep*
                       "Depth_Level" Pipe:*Sep*
                       "Full_Path"))
  (setq rows (mapcar '(lambda (x)
                        (strcat (nth 0 x) Pipe:*Sep*
                                (nth 1 x) Pipe:*Sep*
                                (itoa (nth 2 x)) Pipe:*Sep*
                                (itoa (nth 3 x)) Pipe:*Sep*
                                (itoa (nth 4 x)) Pipe:*Sep*
                                "\"" (nth 5 x) "\""))
                     data))
  (list header rows))

(defun Pipe:ExportBlockHierarchyReport (/ blkName result payload)
  (if (setq blkName (Pipe:SelectBlockInstance))
    (progn
      (Pipe:Msg (strcat "Analyzing block: " blkName))
      (if (setq result (Pipe:AnalyzeBlockStructure blkName))
        (progn
          (Pipe:PrintTree result blkName)
          (setq payload (Pipe:BuildHierarchyCsvData result))
          (if (Pipe:SaveCSV "Block_Hierarchy_Report" (car payload) (cadr payload))
            (Pipe:Msg "Hierarchy report exported successfully.")
            (Pipe:Warn "Unable to save CSV report.")))
        (Pipe:Warn "The selected block has no child blocks or the structure could not be analyzed.")))
    (Pipe:Warn "No block selected.")))

(defun c:BLKHIER ()
  (prompt "\n=== BLOCK HIERARCHY ANALYZER ===\n")
  (Pipe:ExportBlockHierarchyReport)
  (princ))

(defun c:BLOCKHIER ()
  (c:BLKHIER)
  (princ))

(princ "\n[Pipe] Block hierarchy module loaded. Type BLKHIER to analyze nested block structures.")
(princ)
