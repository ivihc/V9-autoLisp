;;; V9 Quantity Tools 1.1 - standalone, ASCII-only AutoLISP for Windows AutoCAD.
;;; Commands: DEMBL (count balloon codes), TONGDIM (sum dimension values).
;;; Diagnostic: V9QTINSPECT (read one MLeader without modifying it).
;;; Load this file instead of the original DemBL.lsp and Length.lsp.
;;; No dependency on the V9Framework bundle. No drawing or sysvar changes.

(vl-load-com)

;;; Remove known MText formatting without removing code digits or punctuation.
;;; Return nil for an incomplete formatting sequence; do not guess its content.
(defun V9:QT:PlainText (text / out pos size ch next stop chunk valid)
  (if (eq (type text) 'STR)
    (progn
      (setq out "" pos 1 size (strlen text) valid T)
      (while (and valid (<= pos size))
        (setq ch (substr text pos 1))
        (cond
          ((member ch '("{" "}")) (setq pos (1+ pos)))
          ((= ch "\\")
            (if (= pos size)
              (setq valid nil)
              (progn
                (setq next (substr text (1+ pos) 1))
                (cond
                  ((member next '("\\" "{" "}"))
                    (setq out (strcat out next) pos (+ pos 2)))
                  ((member next '("P" "X" "~"))
                    (setq out (strcat out " ") pos (+ pos 2)))
                  ((member next '("L" "l" "O" "o" "K" "k"))
                    (setq pos (+ pos 2)))
                  ((member next '("A" "C" "c" "F" "f" "H" "Q" "T" "W" "p" "S"))
                    (setq stop (vl-string-search ";" text (1+ pos)))
                    (if stop
                      (progn
                        (if (= next "S")
                          (progn
                            (setq chunk (if (> (- stop pos 1) 0)
                                          (substr text (+ pos 2) (- stop pos 1)) ""))
                            (setq out (strcat out (vl-string-translate "#^" "//" chunk)))))
                        (setq pos (+ stop 2)))
                      (setq valid nil)))
                  (T
                    ;; Preserve unknown escapes, including Unicode codes.
                    (setq out (strcat out "\\" next) pos (+ pos 2)))))))
          ((member ch '("\t" "\r" "\n"))
            (setq out (strcat out " ") pos (1+ pos)))
          (T (setq out (strcat out ch) pos (1+ pos)))))
      (if valid out nil))))

;;; A complete decimal only: optional sign, digits and at most one decimal dot.
;;; Do not use atof: e.g. "100x200" must not silently become 100.
(defun V9:QT:Decimal (text / value pos size ch digits dots valid)
  (if (eq (type text) 'STR)
    (progn
      (setq value (vl-string-trim " \t\r\n" text)
            pos 1 size (strlen value) digits 0 dots 0 valid T)
      (if (member (substr value 1 1) '("+" "-")) (setq pos 2))
      (while (and valid (<= pos size))
        (setq ch (substr value pos 1))
        (cond
          ((and (>= (ascii ch) 48) (<= (ascii ch) 57))
            (setq digits (1+ digits)))
          ((= ch ".") (setq dots (1+ dots) valid (<= dots 1)))
          (T (setq valid nil)))
        (setq pos (1+ pos)))
      (if (and valid (> digits 0)) (distof value 2) nil))))

;;; Return (canonical-tag group-rank numeric-sort-key).
;;; Ranks increase in the requested BOTTOM-TO-TOP order.
;;; BF is retained as a B-family code from the original file. P is excluded later.
;;; Accept bare family names for existing drawings, and optional space before digits.
(defun V9:QT:BalloonCode (text / plain code pos size ch prefix suffix rank valid digits)
  (if (setq plain (V9:QT:PlainText text))
    (progn
      (setq code (strcase (vl-string-trim " \t\r\n" plain))
            pos 1 size (strlen code))
      (while (and (<= pos size)
                  (>= (ascii (substr code pos 1)) 65)
                  (<= (ascii (substr code pos 1)) 90))
        (setq pos (1+ pos)))
      (setq prefix (if (> pos 1) (substr code 1 (1- pos)) "")
            suffix (vl-string-trim " \t\r\n" (substr code pos))
            rank (cdr (assoc prefix '(("E" . 0) ("T" . 1) ("R" . 2)
                                     ("F" . 3) ("G" . 4) ("B" . 5)
                                     ("BF" . 5) ("N" . 6) ("OF" . 7)
                                     ("P" . -1))))
            valid (not (null rank)) pos 1)
      (while (and valid (<= pos (strlen suffix)))
        (setq ch (ascii (substr suffix pos 1)))
        (if (or (< ch 48) (> ch 57)) (setq valid nil))
        (setq pos (1+ pos)))
      (if valid
        (progn
          (setq digits (vl-string-left-trim "0" suffix))
          (if (and (/= suffix "") (= digits "")) (setq digits "0"))
          (list (strcat prefix suffix) rank digits))))))

;;; Sort TOP-TO-BOTTOM, reversing the requested bottom-to-top family/number order.
;;; Record = (tag rank digits unique-selection-index). The final index is a strict
;;; tie-breaker so vl-sort cannot discard duplicate balloon occurrences.
;;; Compare digit lengths/text instead of atoi, avoiding integer overflow.
(defun V9:QT:RecordBefore (left right / ld rd)
  (setq ld (caddr left) rd (caddr right))
  (cond
    ((/= (cadr left) (cadr right)) (> (cadr left) (cadr right)))
    ((/= (strlen ld) (strlen rd)) (> (strlen ld) (strlen rd)))
    ((/= ld rd) (> ld rd))
    ((/= (car left) (car right)) (> (car left) (car right)))
    (T (< (cadddr left) (cadddr right)))))

;;; Sort once, then aggregate consecutive identical tags in a single pass.
(defun V9:QT:CountRows (records / ordered record current qty rows)
  (setq ordered (vl-sort records 'V9:QT:RecordBefore)
        current nil qty 0 rows nil)
  (foreach record ordered
    (if (equal current (car record))
      (setq qty (1+ qty))
      (progn
        (if current (setq rows (cons (cons current qty) rows)))
        (setq current (car record) qty 1))))
  (if current (setq rows (cons (cons current qty) rows)))
  (reverse rows))

;;; One MLeader can contribute at most one full numbered code.
;;; Bare family labels plus unrelated numbers must not be silently combined.
(defun V9:QT:BlockCode (values / value code candidates)
  (setq candidates nil)
  (foreach value values
    (if (and (setq code (V9:QT:BalloonCode value))
             (/= (caddr code) "")
             (not (member (car code) candidates)))
      (setq candidates (cons (car code) candidates))))
  (cond
    ((not candidates) (list 'NOCODE nil))
    ((cdr candidates) (list 'AMBIGUOUS nil))
    (T (list 'OK (car candidates)))))

;;; Read direct, visible attributes and TEXT/MTEXT in the content block.
;;; Editable values come from this MLeader, never the block definition default.
;;; Detail row: (tag value visible constant readable object-type).
(defun V9:QT:ReadBlockData (obj / document name block item kind tag visible constant
                              value readable rows values failed result)
  (setq document (vla-get-Document obj)
        name (vla-get-ContentBlockName obj)
        block (vla-Item (vla-get-Blocks document) name)
        rows nil values nil failed nil)
  (vlax-for item block
    (setq kind (vla-get-ObjectName item))
    (if (member kind '("AcDbAttributeDefinition" "AcDbText" "AcDbMText"))
      (progn
        (setq visible (/= (vla-get-Visible item) :vlax-false)
              constant T tag "<TEXT>")
        (if (= kind "AcDbAttributeDefinition")
          (progn
            (setq tag (vla-get-TagString item)
                  visible (and visible (= (vla-get-Invisible item) :vlax-false))
                  constant (/= (vla-get-Constant item) :vlax-false))
            (setq value
              (if constant
                (vl-catch-all-apply 'vla-get-TextString (list item))
                (vl-catch-all-apply 'vla-GetBlockAttributeValue
                                   (list obj (vla-get-ObjectID item))))))
          (setq value (vl-catch-all-apply 'vla-get-TextString (list item))))
        (setq readable (and (not (vl-catch-all-error-p value)) (eq (type value) 'STR)))
        (setq rows (cons (list tag (if readable value nil) visible constant readable kind) rows))
        (if visible
          (if readable
            (setq values (cons value values))
            (setq failed T))))))
  (setq result (if failed (list 'UNREADABLE nil) (V9:QT:BlockCode values)))
  (list (car result) 'BLOCK (cadr result) (reverse rows) name))

;;; Return (status content-kind text details block-name [error]).
;;; Never inspect unrelated layer/style/block names as potential balloon codes.
(defun V9:QT:MLeaderContent (entity / obj content text result)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list entity)))
  (if (vl-catch-all-error-p obj)
    (list 'UNREADABLE 'UNKNOWN nil nil nil (vl-catch-all-error-message obj))
    (progn
      (setq content (vl-catch-all-apply 'vlax-get-property (list obj 'ContentType)))
      (cond
        ((vl-catch-all-error-p content)
          (list 'UNREADABLE 'UNKNOWN nil nil nil (vl-catch-all-error-message content)))
        ((= content 1)
          (setq result (vl-catch-all-apply 'V9:QT:ReadBlockData (list obj)))
          (if (vl-catch-all-error-p result)
            (list 'UNREADABLE 'BLOCK nil nil nil (vl-catch-all-error-message result))
            result))
        ((/= content 2) (list 'UNREADABLE 'NONE nil nil nil))
        (T
          (setq text (vl-catch-all-apply 'vla-get-TextString (list obj)))
          (if (or (vl-catch-all-error-p text) (not (eq (type text) 'STR)))
            (list 'UNREADABLE 'MTEXT nil nil nil)
            (list 'OK 'MTEXT text nil nil)))))))

(defun V9:QT:CountBalloons (/ *error* ss index extracted code records rows row
                           selected counted pipes blocks skipped ambiguous blockskipped)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*")))
      (prompt (strcat "\n[Quantity Tools] Error: " msg)))
    (princ))
  (prompt "\nSelect balloon MULTILEADERs (P codes are excluded): ")
  (setq ss (ssget '((0 . "MULTILEADER"))))
  (if ss
    (progn
      (setq index 0 selected (sslength ss) counted 0 pipes 0 blocks 0 skipped 0
            ambiguous 0 blockskipped 0 records nil)
      (repeat selected
        (setq extracted (V9:QT:MLeaderContent (ssname ss index)))
        (if (eq (cadr extracted) 'BLOCK) (setq blocks (1+ blocks)))
        (if (and (eq (cadr extracted) 'BLOCK) (not (eq (car extracted) 'OK)))
          (setq blockskipped (1+ blockskipped)))
        (cond
          ((eq (car extracted) 'AMBIGUOUS) (setq ambiguous (1+ ambiguous)))
          ((not (eq (car extracted) 'OK)) (setq skipped (1+ skipped)))
          ((not (setq code (V9:QT:BalloonCode (caddr extracted))))
            (setq skipped (1+ skipped)))
          ((< (cadr code) 0) (setq pipes (1+ pipes)))
          (T
            (setq records (cons (append code (list index)) records)
                  counted (1+ counted))))
        (setq index (1+ index)))
      (setq rows (V9:QT:CountRows records))
      (prompt (strcat "\n--- BALLOON COUNT ---"
                      "\nSelected: " (itoa selected) " | Counted: " (itoa counted)
                      " | Codes: " (itoa (length rows))
                      "\nBlock-content selected: " (itoa blocks)
                      " | Block content without a usable code: " (itoa blockskipped)
                      "\nExcluded P: " (itoa pipes) " | Ambiguous: " (itoa ambiguous)
                      " | No code/unreadable: " (itoa skipped)))
      (if (> blockskipped 0)
        (prompt "\nRun V9QTINSPECT on a skipped block balloon to inspect its actual values."))
      (if rows
        (progn
          (prompt "\nRead bottom to top: E, T, R, F, G, B, N, OF; numbers increase.\n")
          (foreach row rows
            (prompt (strcat "\n  " (car row) ": " (itoa (cdr row)) " pcs"))))
        (prompt "\nNo supported balloon codes found."))
      rows)
    (progn (prompt "\nNo MULTILEADERs selected.") nil)))

;;; Read-only diagnostic for the actual balloon structure, including split values.
(defun V9:QT:InspectMLeader (/ *error* picked data row)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*")))
      (prompt (strcat "\n[Quantity Tools] Error: " msg)))
    (princ))
  (setq picked (entsel "\nSelect one MULTILEADER to inspect: "))
  (cond
    ((not picked) (prompt "\nNothing selected."))
    ((/= (cdr (assoc 0 (entget (car picked)))) "MULTILEADER")
      (prompt "\nThe selected entity is not a MULTILEADER."))
    (T
      (setq data (V9:QT:MLeaderContent (car picked)))
      (prompt "\n--- MLEADER CONTENT (READ ONLY) ---\nStatus: ") (prin1 (car data))
      (prompt " | Content type: ") (prin1 (cadr data))
      (prompt "\nText / resolved code: ") (prin1 (caddr data))
      (if (eq (cadr data) 'BLOCK)
        (progn
          (prompt "\nContent block: ") (prin1 (nth 4 data))
          (foreach row (nth 3 data)
            (prompt "\n  Object: ") (prin1 (nth 5 row))
            (prompt " | Tag: ") (prin1 (car row))
            (prompt " | Visible: ") (prin1 (caddr row))
            (prompt " | Constant: ") (prin1 (cadddr row))
            (prompt " | Readable: ") (prin1 (nth 4 row))
            (prompt "\n    Value: ") (prin1 (cadr row)))))
      (if (nth 5 data)
        (progn (prompt "\nRead error: ") (prin1 (nth 5 data))))))
  (princ))

;;; Return (value source), where source is MEASURED, OVERRIDE, FALLBACK or SKIPPED.
;;; A <> placeholder always means measurement, even in a prefix/suffix string.
;;; An ambiguous override uses measurement with an explicit fallback count.
(defun V9:QT:DimensionValue (raw measured / plain parsed value source)
  (cond
    ((or (not raw)
         (and (eq (type raw) 'STR) (= (vl-string-trim " \t\r\n" raw) ""))
         (and (eq (type raw) 'STR) (vl-string-search "<>" raw)))
      (setq value measured source 'MEASURED))
    ((and (setq plain (V9:QT:PlainText raw))
          (numberp (setq parsed (V9:QT:Decimal plain))))
      (setq value parsed source 'OVERRIDE))
    (T (setq value measured source 'FALLBACK)))
  (if (numberp value) (list value source) (list nil 'SKIPPED)))

(defun V9:QT:SumDimensions (/ *error* ss index data result selected used overridden
                            measured fallback skipped total)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*")))
      (prompt (strcat "\n[Quantity Tools] Error: " msg)))
    (princ))
  (prompt "\nSelect DIMENSIONs to sum: ")
  (setq ss (ssget '((0 . "DIMENSION"))))
  (if ss
    (progn
      (setq index 0 selected (sslength ss) used 0 overridden 0 measured 0
            fallback 0 skipped 0 total 0.0)
      (repeat selected
        (setq data (entget (ssname ss index))
              result (V9:QT:DimensionValue (cdr (assoc 1 data)) (cdr (assoc 42 data))))
        (cond
          ((eq (cadr result) 'SKIPPED) (setq skipped (1+ skipped)))
          (T
            (setq total (+ total (car result)) used (1+ used))
            (cond
              ((eq (cadr result) 'OVERRIDE) (setq overridden (1+ overridden)))
              ((eq (cadr result) 'MEASURED) (setq measured (1+ measured)))
              (T (setq fallback (1+ fallback))))))
        (setq index (1+ index)))
      (prompt (strcat "\n--- DIMENSION SUM ---"
                      "\nSelected: " (itoa selected) " | Used: " (itoa used)
                      " | Skipped: " (itoa skipped)
                      "\nMeasured: " (itoa measured) " | Numeric overrides: " (itoa overridden)
                      "\nNon-numeric/ambiguous overrides using measurement: " (itoa fallback)
                      "\nTOTAL: " (rtos total 2 2)))
      total)
    (progn (prompt "\nNo DIMENSIONs selected.") nil)))

(defun c:DEMBL () (V9:QT:CountBalloons) (princ))
(defun c:TONGDIM () (V9:QT:SumDimensions) (princ))
(defun c:V9QTINSPECT () (V9:QT:InspectMLeader) (princ))

(princ "\nQuantity Tools 1.1 loaded. DEMBL = balloon count; TONGDIM = dimension sum.")
(princ "\nV9QTINSPECT = read-only MLeader content inspection.")
(princ)
