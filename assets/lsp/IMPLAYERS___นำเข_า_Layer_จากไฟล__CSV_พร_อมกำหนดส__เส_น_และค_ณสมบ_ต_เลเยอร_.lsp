(defun IMPLAYERS:Pause (/ until)
  (setq until (+ (getvar "DATE") (/ 0.05 86400.0)))
  (while (< (getvar "DATE") until))
)

(defun IMPLAYERS:SelectCSVFile (/ psFile resultFile ps args status deadline stream selected)
  ;; Use the modern Windows Open File dialog because AutoCAD's GETFILED dialog
  ;; is fixed at the small legacy size for non-DWG file types.
  (setq psFile     (vl-filename-mktemp "IMPLAYERS-SelectFile" nil ".ps1")
        resultFile (vl-filename-mktemp "IMPLAYERS-SelectedFile" nil ".txt"))

  (if (setq ps (open psFile "w"))
    (progn
      (write-line "param([string]$ResultFile)" ps)
      (write-line "Add-Type -AssemblyName System.Windows.Forms" ps)
      (write-line "$dialog = New-Object System.Windows.Forms.OpenFileDialog" ps)
      (write-line "$dialog.Title = 'Select File'" ps)
      (write-line "$dialog.Filter = 'CSV files (*.csv)|*.csv'" ps)
      (write-line "$dialog.FilterIndex = 1" ps)
      (write-line "$dialog.CheckFileExists = $true" ps)
      (write-line "$dialog.CheckPathExists = $true" ps)
      (write-line "$dialog.Multiselect = $false" ps)
      (write-line "$dialog.RestoreDirectory = $true" ps)
      (write-line "$dialog.AutoUpgradeEnabled = $true" ps)
      (write-line "$owner = New-Object System.Windows.Forms.Form" ps)
      (write-line "$owner.TopMost = $true" ps)
      (write-line "$owner.ShowInTaskbar = $false" ps)
      (write-line "$owner.StartPosition = 'Manual'" ps)
      (write-line "$owner.Location = New-Object System.Drawing.Point(-32000,-32000)" ps)
      (write-line "$owner.Size = New-Object System.Drawing.Size(1,1)" ps)
      (write-line "$selectedFile = ''" ps)
      (write-line "if ($dialog.ShowDialog($owner) -eq [System.Windows.Forms.DialogResult]::OK) {" ps)
      (write-line "  $selectedFile = $dialog.FileName" ps)
      (write-line "}" ps)
      (write-line "$owner.Dispose()" ps)
      (write-line "$dialog.Dispose()" ps)
      (write-line "[System.IO.File]::WriteAllText($ResultFile, $selectedFile, (New-Object System.Text.UTF8Encoding($false)))" ps)
      (close ps)

      ;; VL-FILENAME-MKTEMP may create the result file; remove it so its
      ;; reappearance can be used as the completion signal from PowerShell.
      (if (findfile resultFile) (vl-file-delete resultFile))
      (setq args (strcat "-NoLogo -NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -File \""
                         psFile "\" \"" resultFile "\""))
      (setq status (vl-catch-all-apply 'startapp (list "powershell.exe" args)))

      ;; STARTAPP is asynchronous. Wait until the dialog writes its result.
      (if (and (not (vl-catch-all-error-p status)) status)
        (progn
          (setq deadline (+ (getvar "DATE") (/ 1800.0 86400.0)))
          (while (and (not (findfile resultFile)) (< (getvar "DATE") deadline))
            (IMPLAYERS:Pause))))

      (if (and (not (vl-catch-all-error-p status))
               (findfile resultFile)
               (setq stream (open resultFile "r" "utf8")))
        (progn
          (setq selected (read-line stream))
          (close stream)))
    ))

  (if (findfile psFile) (vl-file-delete psFile))
  (if (findfile resultFile) (vl-file-delete resultFile))
  selected
)

;;; Return the names of user-created Layer Group Filters in the current drawing.
;;; Property filters and AutoCAD's built-in filters are intentionally excluded.
(defun IMPLAYERS:GetLayerGroupNames (/ doc layersObj extDict aclyDict item ent data names result)
  (vl-load-com)
  (setq doc       (vla-get-ActiveDocument (vlax-get-acad-object))
        layersObj (vla-get-Layers doc))
  (if (= (vla-get-HasExtensionDictionary layersObj) :vlax-true)
    (progn
      (setq extDict (vla-GetExtensionDictionary layersObj)
            aclyDict (vl-catch-all-apply 'vla-Item (list extDict "AcLyDictionary")))
      (if (not (vl-catch-all-error-p aclyDict))
        (vlax-for item aclyDict
          (setq ent  (vlax-vla-object->ename item)
                data (entget ent))
          (if (and (= (strcase (cond ((cdr (assoc 1 data))) (t ""))) "ACLYLAYERGROUP")
                   (setq result (cdr (assoc 300 data))))
            (setq names (cons result names)))))))
  (acad_strlsort names)
)

(defun IMPLAYERS:GetLayerGroupMembers (groupName / doc layersObj extDict aclyDict item ent data marker pair layerName members)
  (vl-load-com)
  (setq doc       (vla-get-ActiveDocument (vlax-get-acad-object))
        layersObj (vla-get-Layers doc))
  (if (= (vla-get-HasExtensionDictionary layersObj) :vlax-true)
    (progn
      (setq extDict (vla-GetExtensionDictionary layersObj)
            aclyDict (vl-catch-all-apply 'vla-Item (list extDict "AcLyDictionary")))
      (if (not (vl-catch-all-error-p aclyDict))
        (vlax-for item aclyDict
          (setq ent  (vlax-vla-object->ename item)
                data (entget ent))
          (if (and (= (strcase (cond ((cdr (assoc 1 data))) (t ""))) "ACLYLAYERGROUP")
                   (setq marker (assoc 300 data))
                   (= (strcase groupName) (strcase (cdr marker))))
            (foreach pair (vl-remove-if '(lambda (x) (/= (car x) 330)) (member marker data))
              (if (setq layerName (cdr (assoc 2 (entget (cdr pair)))))
                (setq members (cons layerName members)))))))))
  (reverse members)
)

(defun IMPLAYERS:MemberCI (value items)
  (if items
    (or (= (strcase value) (strcase (car items)))
        (IMPLAYERS:MemberCI value (cdr items)))
    nil)
)

(defun IMPLAYERS:UniqueCI (items / result item)
  (foreach item items
    (if (not (IMPLAYERS:MemberCI item result))
      (setq result (cons item result))))
  (reverse result)
)

(defun IMPLAYERS:AllMembersCI (required actual / ok item)
  (setq ok T)
  (foreach item required
    (if (not (IMPLAYERS:MemberCI item actual))
      (setq ok nil)))
  ok
)

;;; Escape wildcard metacharacters so -LAYER/FILTER receives exact layer names.
(defun IMPLAYERS:EscapeLayerPattern (value / index char result)
  (setq index 1 result "")
  (while (<= index (strlen value))
    (setq char (substr value index 1))
    (if (vl-string-position (ascii char) "`~@#.*?[]-,")
      (setq result (strcat result "`" char))
      (setq result (strcat result char)))
    (setq index (1+ index)))
  result
)

;;; Split the layer-name list into reasonably sized command-line batches.
(defun IMPLAYERS:LayerPatternBatches (layerNames / item current batches escaped)
  (setq current "")
  (foreach item layerNames
    (setq escaped (IMPLAYERS:EscapeLayerPattern item))
    (if (and (/= current "") (> (+ (strlen current) (strlen escaped) 1) 900))
      (setq batches (cons current batches)
            current escaped)
      (setq current (if (= current "") escaped (strcat current "," escaped)))))
  (if (/= current "") (setq batches (cons current batches)))
  (reverse batches)
)

;;; Ask whether to create a new group or append to an existing group.
;;; Return: ("NEW" group-name) or ("EXISTING" group-name).
(defun IMPLAYERS:PromptLayerGroup (csvFile / groups mode index choice name defaultName valid)
  (setq groups      (IMPLAYERS:GetLayerGroupNames)
        defaultName (vl-filename-base csvFile))
  (if groups
    (progn
      (initget "New Existing")
      (setq mode (getkword "\nจัด Layer จาก CSV เข้า Group Filter [New/Existing] <New>: ")))
    (progn
      (setq mode "New")
      (princ "\nยังไม่มี Layer Group Filter ในแบบ จะสร้าง Group ใหม่")))
  (if (= mode "Existing")
    (progn
      (princ "\nLayer Group Filter ที่มีอยู่:")
      (setq index 0)
      (foreach name groups
        (setq index (1+ index))
        (princ (strcat "\n  " (itoa index) ". " name)))
      (while (not valid)
        (initget 1)
        (setq choice (getint (strcat "\nเลือกหมายเลข Group <1-" (itoa (length groups)) ">: ")))
        (if (and choice (<= 1 choice) (<= choice (length groups)))
          (setq valid T)
          (princ "\nหมายเลขไม่ถูกต้อง กรุณาเลือกใหม่")))
      (list "EXISTING" (nth (1- choice) groups)))
    (progn
      (setq valid nil)
      (while (not valid)
        (setq name (getstring T (strcat "\nชื่อ Group Filter ใหม่ <" defaultName ">: ")))
        (if (= name "") (setq name defaultName))
        (cond
          ((= name "")
            (princ "\nชื่อ Group ห้ามว่าง"))
          ((IMPLAYERS:MemberCI name groups)
            (princ (strcat "\nมี Group ชื่อ '" name "' อยู่แล้ว กรุณาใช้ชื่ออื่นหรือเลือก Existing")))
          (t (setq valid T))))
      (list "NEW" name)))
)

(defun IMPLAYERS:AddBatchToExistingGroup (groupName batch / result)
  (setq result
    (vl-catch-all-apply
      'command-s
      (list "_.-LAYER" "_FILTER" "_EDIT" groupName "_ADD" batch "")))
  (not (vl-catch-all-error-p result))
)

;;; Create a group from the first batch, then append any remaining batches.
(defun IMPLAYERS:PutLayersInGroup (mode groupName layerNames / batches firstBatch ok batch)
  (setq batches (IMPLAYERS:LayerPatternBatches layerNames)
        ok T)
  (if batches
    (progn
      (if (= mode "NEW")
        (progn
          (setq firstBatch (car batches)
                batches    (cdr batches)
                ok
                  (not
                    (vl-catch-all-error-p
                      (vl-catch-all-apply
                        'command-s
                        (list "_.-LAYER" "_FILTER" "_NEW" "_GROUP" "" firstBatch groupName "")))))))
      (if (= mode "EXISTING")
        (setq batches (IMPLAYERS:LayerPatternBatches layerNames)))
      (foreach batch batches
        (if (not (IMPLAYERS:AddBatchToExistingGroup groupName batch))
          (setq ok nil)))))
  (and ok
       (IMPLAYERS:MemberCI groupName (IMPLAYERS:GetLayerGroupNames))
       (IMPLAYERS:AllMembersCI layerNames (IMPLAYERS:GetLayerGroupMembers groupName)))
)

(defun c:IMPLAYERS (/ fn file line lines data layName layColor layPlot layLw layLtype layDesc acadObj doc layersObj ltypesObj layObj overWrite overwriteAll skipAll ans rgbLst guessedBook groupSelection groupMode groupName importedLayers groupOK)
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object)
        doc (vla-get-ActiveDocument acadObj)
        layersObj (vla-get-Layers doc)
        ltypesObj (vla-get-Linetypes doc)
        overwriteAll nil
        skipAll nil)

  ;; เปิดหน้าต่างเลือกไฟล์ CSV
  (setq fn (IMPLAYERS:SelectCSVFile))

  (if fn
    (if (setq file (open fn "r"))
      (progn
        ;; Ask for the destination Layer Group Filter before importing.
        (setq groupSelection (IMPLAYERS:PromptLayerGroup fn)
              groupMode      (car groupSelection)
              groupName      (cadr groupSelection))

        ;; ดูดข้อมูลทุกบรรทัด (ตัด \r เผื่อไว้)
        (while (setq line (read-line file))
          (setq lines (cons (vl-string-translate "\r" "" line) lines)))
        (close file)
        
        ;; วนลูปข้ามบรรทัดแรก (Header)
        (foreach line (cdr (reverse lines))
          (setq data (ParseCSV line)
                layName (nth 0 data))
                
          (if (and layName (/= layName ""))
            (if (snvalid layName)
              (progn
                (setq overWrite T)
                ;; ตรวจสอบว่ามีเลเยอร์นี้อยู่แล้วหรือไม่
                (if (tblsearch "LAYER" layName)
                  (cond
                    (overwriteAll (setq overWrite T))
                    (skipAll (setq overWrite nil))
                    (t
                      (setq ans (getstring (strcat "\nพบเลเยอร์ '" layName "' อยู่แล้ว ต้องการเขียนทับหรือไม่? [Yes/No/All/Skip all] <N>: ")))
                      (cond
                        ((wcmatch (strcase ans) "A,ALL") (setq overWrite T overwriteAll T))
                        ((wcmatch (strcase ans) "S,SKIP") (setq overWrite nil skipAll T))
                        ((wcmatch (strcase ans) "Y,YES") (setq overWrite T))
                        (t (setq overWrite nil))
                      )
                    )
                  )
                )

                ;; ทำการสร้างหรือทับ
                (if overWrite
                  (progn
                    ;; สร้างเลเยอร์ โดยป้องกัน error หากมีปัญหาลึกๆ ในระบบ
                    (setq layObj (vl-catch-all-apply 'vla-Add (list layersObj layName)))
                    
                    (if (not (vl-catch-all-error-p layObj))
                      (progn
                        (setq layColor (nth 1 data)
                              layPlot (nth 2 data)
                              layLw (nth 3 data)
                              layLtype (nth 4 data)
                              layDesc (nth 5 data))
                        
                        ;; จัดการเรื่องสี (Index, True Color, Color Book)
                        (if (and layColor (/= layColor ""))
                          (cond
                            ;; 1. RGB True Color: คั่นด้วยจุลภาค (เช่น "31,127,0")
                            ((vl-string-search "," layColor)
                              (setq rgbLst (SplitString layColor ","))
                              (if (>= (length rgbLst) 3)
                                (vl-catch-all-apply
                                  '(lambda ( / tc )
                                     (setq tc (vla-get-truecolor layObj))
                                     (vla-SetRGB tc (atoi (nth 0 rgbLst)) (atoi (nth 1 rgbLst)) (atoi (nth 2 rgbLst)))
                                     (vla-put-truecolor layObj tc)
                                   )
                                )
                              )
                            )
                            ;; 2. Color Book: คั่นด้วย $ (เช่น "DIC COLOR GUIDE(R)$DIC 100")
                            ((vl-string-position 36 layColor) ; 36 คือรหัส ASCII ของ $
                              (vl-catch-all-apply
                                '(lambda ( / tc idx bookName colorName)
                                   (setq idx (vl-string-position 36 layColor)
                                         bookName (substr layColor 1 idx)
                                         colorName (substr layColor (+ idx 2))
                                         tc (vla-get-truecolor layObj))
                                   (vla-SetColorBookColor tc bookName colorName)
                                   (vla-put-truecolor layObj tc)
                                 )
                              )
                            )
                            ;; 3. ACI Index Color หรือข้อความเดี่ยวๆ (เช่น "DIC 578", "red")
                            (t
                              (if (> (atoi layColor) 0)
                                ;; เป็นตัวเลข 1-255
                                (vl-catch-all-apply 'vla-put-color (list layObj (atoi layColor)))
                                ;; ถ้าไม่ใช่ตัวเลข ลองเดา Color Book จากชื่อ
                                (if (setq guessedBook (GuessColorBook layColor))
                                  (vl-catch-all-apply
                                    '(lambda ( / tc )
                                       (setq tc (vla-get-truecolor layObj))
                                       (vla-SetColorBookColor tc guessedBook layColor)
                                       (vla-put-truecolor layObj tc)
                                     )
                                  )
                                )
                              )
                            )
                          )
                        )
                        ;; จัดการ Plot, Lineweight, Linetype, Description
                        (if (and layPlot (/= layPlot ""))
                          (vl-catch-all-apply 'vla-put-plottable (list layObj (if (= layPlot "0") :vlax-false :vlax-true))))
                        (if (and layLw (/= layLw ""))
                          (vl-catch-all-apply 'vla-put-lineweight (list layObj (atoi layLw))))
                        
                        ;; โหลดและตั้งค่า Linetype
                        (if (and layLtype (/= layLtype ""))
                          (progn
                            (if (not (tblsearch "LTYPE" layLtype))
                              (progn
                                (vl-catch-all-apply 'vla-Load (list ltypesObj layLtype "acadiso.lin"))
                                (if (not (tblsearch "LTYPE" layLtype))
                                  (vl-catch-all-apply 'vla-Load (list ltypesObj layLtype "acad.lin"))
                                )
                              )
                            )
                            (vl-catch-all-apply 'vla-put-linetype (list layObj layLtype))
                          )
                        )

                        (if layDesc
                          (vl-catch-all-apply 'vla-put-description (list layObj layDesc)))

                        (princ (strcat "\n-> อัปเดตเลเยอร์: " layName " สำเร็จ"))
                      )
                      (princ (strcat "\n-> ข้ามเลเยอร์: " layName " (สร้างเลเยอร์ไม่สำเร็จ)"))
                    )
                  )
                  (princ (strcat "\n-> ข้ามเลเยอร์: " layName))
                )
              )
              (princ (strcat "\n-> ข้ามเลเยอร์: " layName " (ชื่อเลเยอร์มีตัวอักษรที่ห้ามใช้)"))
            )
          )

          ;; Include every valid CSV layer that now exists in the drawing,
          ;; including an existing layer that the user chose not to overwrite.
          (if (and layName (snvalid layName) (tblsearch "LAYER" layName))
            (setq importedLayers (cons layName importedLayers)))
        )

        (setq importedLayers (IMPLAYERS:UniqueCI (reverse importedLayers)))
        (if importedLayers
          (progn
            (setq groupOK (IMPLAYERS:PutLayersInGroup groupMode groupName importedLayers))
            (if groupOK
              (princ
                (strcat "\n\n-> เพิ่ม " (itoa (length importedLayers))
                        " Layer เข้า Group Filter '" groupName "' สำเร็จ"))
              (princ
                (strcat "\n\n-> คำเตือน: ไม่สามารถเพิ่ม Layer บางส่วนเข้า Group Filter '"
                        groupName "' ได้"))))
          (princ "\n\n-> ไม่พบ Layer ที่สามารถเพิ่มเข้า Group Filter ได้"))
        (princ "\n\n=== นำเข้าเลเยอร์เสร็จสมบูรณ์ ===")
      )
      (princ "\nเปิดไฟล์ไม่ได้! (ตรวจสอบว่าคุณกำลังคลิกแก้ไขช่องใดใน Excel ค้างไว้หรือไม่)")
    )
    (princ "\nไม่ได้เลือกไฟล์ ยกเลิกคำสั่ง")
  )
  (princ)
)

;;; ฟังก์ชันเสริมสำหรับแยกข้อความ CSV รองรับเครื่องหมายฟันหนู (Double Quotes)
(defun ParseCSV (str / lst inQuote buf char)
  (setq inQuote nil buf "")
  (while (/= str "")
    (setq char (substr str 1 1)
          str (substr str 2))
    (cond
      ((= char "\"")
       (setq inQuote (not inQuote)))
      ((and (= char ",") (not inQuote))
       (setq lst (cons buf lst)
             buf ""))
      (t
       (setq buf (strcat buf char)))
    )
  )
  (reverse (cons buf lst))
)

;;; ฟังก์ชันเสริมสำหรับแยกข้อความด้วยตัวคั่นทั่วไป
(defun SplitString (str delim / pos lst)
  (while (setq pos (vl-string-search delim str))
    (setq lst (cons (substr str 1 pos) lst)
          str (substr str (+ pos 1 (strlen delim)))))
  (reverse (cons str lst))
)

;;; ฟังก์ชันเดาชื่อสมุดสี (Color Book) จากชื่อสีที่กรอกเข้ามาสั้นๆ
(defun GuessColorBook (colorName / upColor)
  (setq upColor (strcase colorName))
  (cond
    ((wcmatch upColor "DIC *") "DIC COLOR GUIDE(R)")
    ((wcmatch upColor "PANTONE * C") "PANTONE+ Solid Coated")
    ((wcmatch upColor "PANTONE * U") "PANTONE+ Solid Uncoated")
    ((wcmatch upColor "RAL *") "RAL CLASSIC")
    (t nil)
  )
)
