(vl-load-com)

;;=========================================================
;; PIPE PLANT 3D ASSET EXTRACTOR
;; Automatically probe Plant 3D assets and export metadata
;;=========================================================

(defun c:PLANT_EXTRACT (/)
  (Plant:ExtractAssets)
  (princ)
)

(defun Plant:ExtractAssets (/ ss i ent obj assetData allKeys csvLines header assetInfo)
  (prompt "\n=== PIPE PLANT 3D ASSET EXTRACTOR ===\n")
  (prompt "Select fitting/assets (Elbow, Tee, Valve, Flange, etc.)...\n")

  (setq ss (ssget '((0 . "*LINE,*POLYLINE,CIRCLE,ARC,INSERT"))))

  (if (not ss)
    (progn
      (prompt "\nNo objects selected.\n")
      (exit)
    )
  )

  (setq i 0 assetData nil allKeys nil)
  (prompt (strcat "\nSelected " (itoa (sslength ss)) " object(s). Processing...\n"))

  (repeat (sslength ss)
    (setq ent (ssname ss i)
          obj (vlax-ename->vla-object ent)
          i (1+ i))

    (if (vlax-object-p obj)
      (progn
        (prompt (strcat "\rProcessing object " (itoa i) "..."))
        (setq assetInfo (Plant:ProbeObject ent obj))
        (if assetInfo
          (progn
            (foreach pair assetInfo
              (if (not (member (car pair) allKeys))
                (setq allKeys (cons (car pair) allKeys))))
            (setq assetData (cons assetInfo assetData))
          )
        )
      )
    )
  )

  (prompt "\n")

  (if (= (length assetData) 0)
    (progn
      (prompt "\nNo Plant 3D metadata was found in the selected objects.\n")
      (prompt "The objects may not be Plant 3D assets or the data is stored in a database.\n")
      (exit)
    )
  )

  (setq allKeys (vl-sort allKeys '(lambda (a b) (string< a b))))

  (setq priorityKeys '("Spec" "Class" "Size" "Material" "LongDescription" "Description" "Type" "Tag" "Line" "Area" "Plant"))
  (setq sortedKeys nil)

  (foreach key priorityKeys
    (if (member key allKeys)
      (setq sortedKeys (append sortedKeys (list key))
            allKeys (vl-remove key allKeys))))

  (setq sortedKeys (append sortedKeys allKeys))

  (setq header "STT,Handle")
  (foreach key sortedKeys
    (setq header (strcat header "," key)))

  (setq csvLines nil i 0)
  (foreach asset assetData
    (setq i (1+ i)
          handle (cdr (assoc "Handle" asset))
          lineStr (strcat (itoa i) "," handle))

    (foreach key sortedKeys
      (setq val (cdr (assoc key asset)))
      (if val
        (setq val (vl-string-translate "," ";" (vl-string-trim "\"" val)))
        (setq val "")
      )
      (setq lineStr (strcat lineStr "," val))
    )

    (setq csvLines (cons lineStr csvLines))
  )

  (if (Plant:SaveCSV "Plant_Asset_Report" header csvLines)
    (progn
      (prompt "\n\n=== SUCCESS ===\n")
      (prompt (strcat "Exported " (itoa (length assetData)) " asset(s).\n"))
      (prompt "Data fields found:\n")
      (foreach key sortedKeys
        (prompt (strcat "  - " key "\n")))
      (prompt "\nCSV file saved.\n")
    )
    (prompt "\nError: Could not save CSV file.\n")
  )
)

(defun Plant:ProbeObject (ent obj / xdataInfo vlxInfo combinedInfo)
  (setq xdataInfo (Plant:GetXData ent))
  (setq vlxInfo (Plant:GetVLaxProperties obj))
  (setq combinedInfo (Plant:MergeData xdataInfo vlxInfo))
  (setq combinedInfo (cons (cons "Handle" (cdr (assoc 5 (entget ent)))) combinedInfo))

  (if (> (length combinedInfo) 1)
    combinedInfo
    nil
  )
)

(defun Plant:GetXData (ent / xdata result regName pairs)
  (setq xdata (entget ent '("*"))
        result nil
        regName nil
        pairs nil)

  (foreach item xdata
    (cond
      ((= (car item) 1001)
       (setq regName (cdr item)
             pairs nil))
      ((= (car item) 1000)
       (if regName
         (setq pairs (cons (cons (strcat regName "_Str") (cdr item)) pairs))))
      ((and (>= (car item) 1000) (<= (car item) 1071))
       (if regName
         (setq pairs (cons (cons (itoa (car item)) (vl-princ-to-string (cdr item))) pairs))))
    )
  )

  (if pairs
    (setq result (append result (Plant:ParseXDataPairs pairs regName))))

  (setq result (Plant:NormalizeFieldNames result))
  result
)

(defun Plant:ParseXDataPairs (pairs regName / result cleanedPairs)
  (setq cleanedPairs nil)
  (foreach pair pairs
    (setq key (car pair)
          val (cdr pair))

    (if (and (stringp val)
             (= (substr val 1 1) "\"")
             (= (substr val (strlen val) 1) "\""))
      (setq val (substr val 2 (- (strlen val) 2))))

    (setq cleanedPairs (cons (cons key val) cleanedPairs))
  )

  cleanedPairs
)

(defun Plant:NormalizeFieldNames (data / result mapping key val normalizedKey)
  (setq mapping '(
    ("Spec" . "Spec")
    ("Specification" . "Spec")
    ("P3D_Spec" . "Spec")
    ("Class" . "Class")
    ("Rating" . "Class")
    ("P3D_Class" . "Class")
    ("Size" . "Size")
    ("NominalSize" . "Size")
    ("Diameter" . "Size")
    ("P3D_Size" . "Size")
    ("Material" . "Material")
    ("Mat" . "Material")
    ("P3D_Material" . "Material")
    ("Description" . "Description")
    ("LongDescription" . "LongDescription")
    ("Desc" . "Description")
    ("Type" . "Type")
    ("ComponentType" . "Type")
    ("Tag" . "Tag")
    ("ComponentTag" . "Tag")
    ("Line" . "Line")
    ("LineNumber" . "Line")
    ("Area" . "Area")
    ("Plant" . "Plant")
    ("Project" . "Project")
  ))

  (setq result nil)
  (foreach pair data
    (setq key (car pair)
          val (cdr pair))
    (setq normalizedKey (cdr (assoc key mapping)))
    (if normalizedKey
      (setq result (cons (cons normalizedKey val) result))
      (setq result (cons pair result))
    )
  )

  result
)

(defun Plant:GetVLaxProperties (obj / result props prop val)
  (setq result nil)
  (setq props '(
    "Spec" "Specification" "Class" "Rating" "Size" "NominalSize"
    "Diameter" "Material" "Mat" "Description" "LongDescription"
    "Type" "ComponentType" "Tag" "ComponentTag" "Line" "LineNumber"
    "Area" "Plant" "Project" "PartNumber" "Manufacturer"
  ))

  (foreach prop props
    (vl-catch-all-apply
      '(lambda ()
         (setq val (vlax-get-property obj (read prop)))
         (if val
           (setq result (cons (cons prop (vl-princ-to-string val)) result))
         )
       )
    )
  )

  (vl-catch-all-apply
    '(lambda ()
       (vlax-dump-object obj nil)
     )
  )

  result
)

(defun Plant:MergeData (data1 data2 / result)
  (setq result data1)
  (foreach pair data2
    (if (not (assoc (car pair) result))
      (setq result (cons pair result))
    )
  )
  result
)

(defun Plant:SaveCSV (fn header data / path fp)
  (if (setq path (getfiled "Save CSV File" (strcat "D:\\" fn) "csv" 1))
    (if (setq fp (open path "w"))
      (progn
        (write-line header fp)
        (foreach line data (write-line line fp))
        (close fp)
        path)
      nil)
    nil)
)

(defun c:PLANT_PROBE (/ ent obj)
  (prompt "\nSelect one Plant 3D asset to probe:\n")
  (if (setq ent (car (entsel)))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (prompt "\n=== XDATA ===\n")
      (foreach pair (Plant:GetXData ent)
        (prompt (strcat (car pair) ": " (cdr pair) "\n")))

      (prompt "\n=== VLAX PROPERTIES ===\n")
      (foreach pair (Plant:GetVLaxProperties obj)
        (prompt (strcat (car pair) ": " (cdr pair) "\n")))

      (prompt "\n=== COMBINED ===\n")
      (foreach pair (Plant:ProbeObject ent obj)
        (prompt (strcat (car pair) ": " (cdr pair) "\n")))
    )
    (prompt "\nNo object selected.\n")
  )
  (princ)
)

(princ "\n[Plant] Plant 3D asset extractor loaded. Use PLANT_EXTRACT or PLANT_PROBE.")
(princ)
