;;=========================================================
;; Export commands: LINE / BLOCK statistics
;;=========================================================
(defun Pipe:GetLines (mode / ss ent lst)
  (cond
    ((eq mode 'area)
     (prompt "\nSelect LINEs in area (Press ENTER to finish): ")
     (if (setq ss (ssget '((0 . "LINE"))))
       (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))) nil))
    ((eq mode 'one)
     (Pipe:Msg "Select LINEs one by one in order. Press ENTER to finish.")
     (while (setq ent (car (entsel "\nSelect LINE: ")))
       (if (= (cdr (assoc 0 (entget ent))) "LINE")
         (if (not (member ent lst))
           (setq lst (cons ent lst))
           (Pipe:Warn "Line already selected."))
         (Pipe:Warn "Selected object is not a LINE.")))
     (reverse lst))))

(defun Pipe:GetLineProps (e / data layer ltype color p1 p2 len handle p1Str p2Str)
  (setq data (entget e)
        layer  (cdr (assoc 8 data))
        ltype  (cond ((cdr (assoc 6 data))) ("ByLayer"))
        color  (cond ((cdr (assoc 62 data)) (itoa (cdr (assoc 62 data)))) ("ByLayer"))
        handle (cdr (assoc 5 data))
        p1     (cdr (assoc 10 data))
        p2     (cdr (assoc 11 data))
        len    (distance p1 p2)
        p1Str  (strcat (rtos (car p1) 2 2) " " (rtos (cadr p1) 2 2) " " (rtos (caddr p1) 2 2))
        p2Str  (strcat (rtos (car p2) 2 2) " " (rtos (cadr p2) 2 2) " " (rtos (caddr p2) 2 2)))
  (list layer ltype color p1Str p2Str handle len))

(defun Pipe:ExportLine (mode / lines total data props idx path header lineStr)
  (if (setq lines (Pipe:GetLines mode))
    (progn
      (setq total 0.0 idx 0 data nil)
      (setq header (strcat "No" Pipe:*Sep* "Layer" Pipe:*Sep* "Linetype" Pipe:*Sep*
                           "Color" Pipe:*Sep* "Start_XYZ" Pipe:*Sep* "End_XYZ" Pipe:*Sep*
                           "Handle" Pipe:*Sep* "Length"))
      (foreach e lines
        (setq props (Pipe:GetLineProps e)
              idx   (1+ idx)
              total (+ total (nth 6 props))
              lineStr (strcat (itoa idx) Pipe:*Sep*
                              (nth 0 props) Pipe:*Sep*
                              (nth 1 props) Pipe:*Sep*
                              (nth 2 props) Pipe:*Sep*
                              "\"" (nth 3 props) "\"" Pipe:*Sep*
                              "\"" (nth 4 props) "\"" Pipe:*Sep*
                              (nth 5 props) Pipe:*Sep*
                              (rtos (nth 6 props) 2 2))
              data  (cons lineStr data)))
      (setq lineStr (strcat "TOTAL" Pipe:*Sep* Pipe:*Sep* Pipe:*Sep* Pipe:*Sep* Pipe:*Sep* Pipe:*Sep* Pipe:*Sep* (rtos total 2 2)))
      (setq data (reverse (cons lineStr data)))
      (if (Pipe:SaveCSV "Pipe_Length_Detailed" header data)
        (Pipe:Msg (strcat "Exported " (itoa (length lines)) " LINE(s) with full details."))))
    (Pipe:Warn "No LINE selected.")))

(defun Pipe:GetAttList (obj / atts lst)
  (if (and (= (vla-get-HasAttributes obj) :vlax-true)
           (setq atts (vlax-invoke obj 'GetAttributes)))
    (foreach att atts
      (setq lst (cons (cons (vla-get-TagString att) (vla-get-TextString att)) lst))))
  (reverse lst))

(defun Pipe:ExportBlockPivot (mode / ss i e obj name layer attLst allTags keyData blkData exist total
                                      header csvLines path fp val lineStr)
  (setq ss (if (eq mode 'all)
             (ssget "_X" '((0 . "INSERT")))
             (progn (prompt "\nSelect BLOCKs (Press ENTER to finish): ") (ssget '((0 . "INSERT"))))))
  (if ss
    (progn
      (setq total 0 blkData nil allTags nil i (sslength ss))
      (repeat i
        (setq e (ssname ss (setq i (1- i)))
              obj (vlax-ename->vla-object e)
              name (vla-get-EffectiveName obj))
        (if (/= (substr name 1 1) "*")
          (progn
            (setq layer (vla-get-Layer obj)
                  attLst (Pipe:GetAttList obj)
                  total (1+ total))
            (foreach pair attLst
              (if (not (member (car pair) allTags))
                (setq allTags (cons (car pair) allTags))))
            (setq keyData (list name layer attLst)
                  exist (assoc keyData blkData))
            (if exist
              (setq blkData (subst (cons keyData (1+ (cdr exist))) exist blkData))
              (setq blkData (cons (cons keyData 1) blkData))))))
      (setq allTags (vl-sort allTags '<))
      (setq header (strcat "Block_Name" Pipe:*Sep* "Layer"))
      (foreach tag allTags
        (setq header (strcat header Pipe:*Sep* tag)))
      (setq header (strcat header Pipe:*Sep* "Count"))
      (setq csvLines nil)
      (foreach item blkData
        (setq keyData (car item)
              name    (nth 0 keyData)
              layer   (nth 1 keyData)
              attLst  (nth 2 keyData)
              lineStr (strcat name Pipe:*Sep* layer))
        (foreach tag allTags
          (setq val (cdr (assoc tag attLst)))
          (setq lineStr (strcat lineStr Pipe:*Sep* (if val val ""))))
        (setq lineStr (strcat lineStr Pipe:*Sep* (itoa (cdr item))))
        (setq csvLines (cons lineStr csvLines)))
      (setq csvLines (vl-sort csvLines '<))
      (setq lineStr (strcat "TOTAL" Pipe:*Sep*))
      (foreach tag allTags (setq lineStr (strcat lineStr Pipe:*Sep*)))
      (setq lineStr (strcat lineStr Pipe:*Sep* (itoa total)))
      (setq csvLines (append csvLines (list lineStr)))
      (if (Pipe:SaveCSV "Block_Count_Detailed" header csvLines)
        (Pipe:Msg (strcat "Exported " (itoa total) " BLOCK(s) with " (itoa (length allTags)) " Attribute Columns."))
        (Pipe:Warn "Cannot create CSV file.")))
    (Pipe:Warn "No BLOCK selected.")))

(defun Pipe:Menu (/ opt)
  (prompt (strcat "\n----------------------------------------"
                  "\n[Pipe] Piping Library V" Pipe:*Ver*
                  "\n----------------------------------------"
                  "\n[1] Export LINE (Select One by One in Order)"
                  "\n[2] Export LINE (Select Window / Area)"
                  "\n[B] Export BLOCK (Select Window / Area)"
                  "\n[BA] Export BLOCK (Select All Drawing)"
                  "\n----------------------------------------\n"))
  (initget "1 2 B BA")
  (setq opt (getkword "Select option [1/2/B/BA]: "))
  (cond ((= opt "1")  (Pipe:ExportLine 'one))
        ((= opt "2")  (Pipe:ExportLine 'area))
        ((= opt "B")  (Pipe:ExportBlockPivot 'area))
        ((= opt "BA") (Pipe:ExportBlockPivot 'all)))
  (princ))

(defun c:V9 () (Pipe:Menu) (princ))
(defun c:PLEN1 () (Pipe:ExportLine 'one) (princ))
(defun c:PLEN () (Pipe:ExportLine 'area) (princ))
(defun c:CBLK () (Pipe:ExportBlockPivot 'area) (princ))
(defun c:CBLKA () (Pipe:ExportBlockPivot 'all) (princ))

(princ (strcat "\n[Pipe] Export module loaded. Type 'PLEN1' or 'PLEN' to measure lines."))
(princ)
