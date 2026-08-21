(defun C:VPLAY ( / *error* action ent ss laylist selected_layers support_layers picked_layer current_color color value index rgb transparency in_viewport old_selectioncycling)
  (vl-load-com)

  ;; คืนค่า Selection Cycling เสมอ แม้ผู้ใช้กด Esc ระหว่างเลือก
  (defun *error* (msg)
    (if old_selectioncycling
      (setvar "SELECTIONCYCLING" old_selectioncycling)
    )
    (if
      (and
        msg
        (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*"))
      )
      (prompt (strcat "\n❌ Error: " msg))
    )
    (princ)
  )

  ;; แยกการทำงานระหว่างหน้า Model กับ Layout Viewport
  (setq in_viewport (and (= (getvar "TILEMODE") 0) (> (getvar "CVPORT") 1)))
  (if (or (= (getvar "TILEMODE") 1) in_viewport)
    (progn

  ;; หน้า Model ใช้ OFF/ON สำหรับ Isolate/Open เพื่อไม่เปลี่ยนสถานะ Freeze เดิม
  ;; ส่วนใน Viewport ใช้ VP Freeze/VP Thaw เพื่อไม่กระทบ Viewport อื่น
  (if in_viewport
    (progn
      (initget "Hide Color Transparency Isolate Open")
      (setq action (getkword "\nเลือก [Hide=ซ่อน(Freeze)/Color=เปลี่ยนสี/Transparency=โปร่งใส/Isolate=แสดงเฉพาะ/Open=เปิดที่ซ่อน] <Hide>: "))
      (if (null action) (setq action "Hide"))
    )
    (progn
      (initget "Hide Isolate Open")
      (setq action (getkword "\nเลือก [Hide=ซ่อน(Freeze)/Isolate=แสดงเฉพาะ/Open=เปิดที่ซ่อน] <Hide>: "))
      (if (null action) (setq action "Hide"))
    )
  )

  ;; 2. เลือกต่อเนื่องจนกว่าจะกด Enter/Esc
  ;;    Color ใช้ nentsel แบบเดียวกับ Freeze รุ่นเดิม เพื่อเอา Layer
  ;;    ของวัตถุ Nested ที่อยู่ใกล้จุดคลิกจริง ไม่ดึงทุก Layer ใน Pickbox
  ;;    Isolate ใช้ Selection Set หลายวัตถุ; คำสั่งอื่นยังเลือกแบบจุดคลิกเดิม
  (setq old_selectioncycling (getvar "SELECTIONCYCLING"))

  ;; Open ไม่ต้องเลือกวัตถุ: เปิด Layer ที่ซ่อนไว้แล้วจบทันที
  (if (= action "Open")
    (VPLAY:Restore-Hidden in_viewport)
    (while
    (and
      (/= action "Done")
      (progn
      (setq ent nil
            ss nil
            laylist nil
            selected_layers nil
            picked_layer nil
            support_layers nil)
      ;; ปิดหน้าต่าง Selection Cycling เดิมชั่วคราว แล้วใช้ Popup ของ VPLAY
      (setvar "SELECTIONCYCLING" 0)
      (setvar "ERRNO" 0)
      (cond
        ((= action "Color")
         (while
           (and
             (not (setq ent (nentsel "\nคลิกใกล้วัตถุที่ต้องการเปลี่ยนสี หรือกด Enter เพื่อจบ: ")))
             (= (getvar "ERRNO") 7)
           )
           (prompt "\nไม่พบวัตถุใกล้จุดคลิก กรุณาลองใหม่.")
         )
        )
        ((= action "Isolate")
         (prompt "\nเลือกวัตถุหลายชิ้นด้วย Window/Crossing แล้วกด Enter: ")
         (setq ss (ssget "_:N"))
        )
        (T
         (prompt "\nคลิกวัตถุที่ต้องการ หรือกด Enter เพื่อจบ: ")
         (setq ss (ssget "_:E:S:N"))
        )
      )
      (setvar "SELECTIONCYCLING" old_selectioncycling)
      (if (= action "Color") ent ss)
      )
    )

    (cond
      ((= action "Color")
       (if (setq picked_layer (Get-Layer-From-Nested-Selection ent))
         (setq laylist (list picked_layer))
       )
      )
      ((= action "Isolate")
       ;; เหมือน LCC: การคลุม Block/Xref ใช้ Layer ของวัตถุระดับบน
       (setq laylist (Get-Top-Level-Layers-From-Selection ss))
       ;; ปล่อย Highlight ของ Selection Set ก่อนปิด Layer อื่น
       (sssetfirst nil nil)
       (setq ss nil)
       (redraw)
      )
      (T
       (setq laylist (Get-Layers-At-Pickbox ss))
      )
    )

  (if laylist
    (progn
          ;; 3. Isolate ใช้ทุก Layer จากวัตถุที่คลุมทันที
          ;;    ส่วนคำสั่งเดิมยังแสดง Popup เมื่อจุดคลิกพบหลาย Layer
          (if (= action "Isolate")
            (setq selected_layers laylist)
            (if (or (= action "Color") (> (length laylist) 1))
              (setq selected_layers (Show-Layer-Select-Dialog laylist))
              (setq selected_layers laylist)
            )
          )
          
          ;; 4. ดำเนินการตาม Action ที่เลือก
          (if selected_layers
            (cond
              ;; ---- FREEZE ----
              ((= action "Hide")
               (foreach lay selected_layers
                 (if in_viewport
                   (progn
                     (command "_.vplayer" "freeze" lay "current" "")
                     (prompt (strcat "\n✅ VP Freeze Layer: " lay " เรียบร้อย."))
                   )
                   (if (Freeze-Layer-In-Model lay)
                     (prompt (strcat "\n✅ Freeze Layer: " lay " ในหน้า Model เรียบร้อย."))
                   )
                 )
               )
              )

              ;; ---- ISOLATE / แสดงเฉพาะ LAYER ที่เลือก ----
              ((= action "Isolate")
               (VPLAY:Isolate-Layers selected_layers nil in_viewport)
               ;; เลือกหลายชิ้นหนึ่งชุดแล้วจบ ไม่วนถามซ้ำเหมือน Freeze
               (setq action "Done")
              )

              ;; ---- COLOR ----
              ((= action "Color")
               (foreach lay selected_layers
                 (prompt (strcat "\n🔎 Layer ที่เลือก: " lay))
               )
               (if (setq color (acad_truecolordlg '(62 . 7) nil))
                 (progn
                   (cond
                     ((setq value (cdr (assoc 430 color))) ; ColorBook
                      (setq index (vl-string-position 36 value))
                      (foreach lay selected_layers
                        (command "_.vplayer" "color" "_CO" (substr value 1 index) (substr value (+ index 2)) lay "current" "")
                        (prompt (strcat "\n🎨 เปลี่ยนสี Layer: " lay " เป็น ColorBook เรียบร้อย."))
                      )
                     )
                     ((setq value (cdr (assoc 420 color))) ; TrueColor RGB
                      (setq rgb (LM:True->RGB value))
                      (foreach lay selected_layers
                        (command "_.vplayer" "color" "_T" (strcat (itoa (car rgb)) "," (itoa (cadr rgb)) "," (itoa (caddr rgb))) lay "current" "")
                        (prompt (strcat "\n🎨 เปลี่ยนสี Layer: " lay " เป็น RGB เรียบร้อย."))
                      )
                     )
                     ((setq value (cdr (assoc 62 color))) ; Index Color
                      (foreach lay selected_layers
                        (command "_.vplayer" "color" value lay "current" "")
                        (prompt (strcat "\n🎨 เปลี่ยนสี Layer: " lay " เรียบร้อย."))
                      )
                     )
                   )
                 )
                 (prompt "\n❌ ยกเลิกการเปลี่ยนสี")
               )
              )

              ;; ---- TRANSPARENCY ----
              ((= action "Transparency")
               (initget 1)
               (setq transparency (getint "\nระบุค่าความโปร่งใส (0-90): "))
               (if (and (>= transparency 0) (<= transparency 90))
                 (foreach lay selected_layers
                   (command "_.vplayer" "transparency" (itoa transparency) lay "current" "")
                   (prompt (strcat "\n✨ เปลี่ยนความโปร่งใส Layer: " lay " เป็น " (itoa transparency) "% เรียบร้อย."))
                 )
                 (prompt "\n❌ ค่าความโปร่งใสต้องอยู่ระหว่าง 0 ถึง 90.")
               )
              )
            )
            (prompt "\n❌ ไม่ได้เลือก Layer")
          )
    )
    (prompt "\n❌ ไม่สามารถอ่าน Layer ของวัตถุที่เลือกได้")
    )
  )

  )

    )
    (prompt "\n❌ ขณะนี้อยู่ใน Paper Space: กรุณาดับเบิลคลิกภายใน Viewport แล้วเรียก VPLAY อีกครั้ง")
  )
  (princ)
)

;; ----------------------------------------------------------------------
;; ฟังก์ชันย่อย: Freeze Layer ทั้งแบบเมื่อทำงานอยู่ในหน้า Model
;; ----------------------------------------------------------------------
(defun Freeze-Layer-In-Model (lay / doc layers layerobj result)
  (if (= (strcase lay) (strcase (getvar "CLAYER")))
    (progn
      (prompt (strcat "\n❌ ไม่สามารถ Freeze Layer ปัจจุบันได้: " lay))
      nil
    )
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq layers (vla-get-Layers doc))
      (setq layerobj (vl-catch-all-apply 'vla-Item (list layers lay)))

      (if (vl-catch-all-error-p layerobj)
        (progn
          (prompt (strcat "\n❌ ไม่พบ Layer: " lay))
          nil
        )
        (progn
          (setq result
            (vl-catch-all-apply 'vla-put-Freeze (list layerobj :vlax-true))
          )
          (if (vl-catch-all-error-p result)
            (progn
              (prompt
                (strcat
                  "\n❌ Freeze Layer ไม่สำเร็จ: "
                  (vl-catch-all-error-message result)
                )
              )
              nil
            )
            T
          )
        )
      )
    )
  )
)

;; ----------------------------------------------------------------------
;; ฟังก์ชันย่อย: แสดงเฉพาะ Layer ที่เลือก และเปิด Layer ที่ซ่อนไว้
;; Model ใช้ OFF/ON ตามแนวทาง LCC; Viewport ใช้ VP Freeze/VP Thaw
;; ----------------------------------------------------------------------
(defun VPLAY:Layer-Object (doc name / result)
  (setq result
    (vl-catch-all-apply
      'vla-Item
      (list (vla-get-Layers doc) name)
    )
  )
  (if (vl-catch-all-error-p result) nil result)
)

(defun VPLAY:Set-Layer-On (doc name state / layerobj result)
  (if (setq layerobj (VPLAY:Layer-Object doc name))
    (progn
      (setq result
        (vl-catch-all-apply
          'vla-put-LayerOn
          (list layerobj state)
        )
      )
      (not (vl-catch-all-error-p result))
    )
    nil
  )
)

(defun VPLAY:Add-Unique-CI (value values)
  (if (member (strcase value) (mapcar 'strcase values))
    values
    (cons value values)
  )
)

(defun VPLAY:Member-CI (value values)
  (member (strcase value) (mapcar 'strcase values))
)

(defun VPLAY:Xref-Block-P (block-name / record flags)
  (and
    (setq record (tblobjname "BLOCK" block-name))
    (setq flags (cdr (assoc 70 (entget record))))
    (/= 0 (logand flags 12))
  )
)

(defun VPLAY:Xrefs-On-Layer (layer-name / ss i data block-name names)
  (if (setq ss (ssget "_X" '((0 . "INSERT"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq data (entget (ssname ss i))
              i (1+ i))
        (if
          (and
            (= (strcase (cdr (assoc 8 data))) (strcase layer-name))
            (setq block-name (cdr (assoc 2 data)))
            (VPLAY:Xref-Block-P block-name)
          )
          (setq names (VPLAY:Add-Unique-CI block-name names))
        )
      )
    )
  )
  names
)

(defun VPLAY:Xref-Layer-P (layer-name xref-name / prefix len)
  (setq prefix (strcat xref-name "|")
        len (strlen prefix))
  (and
    (>= (strlen layer-name) len)
    (= (strcase (substr layer-name 1 len)) (strcase prefix))
  )
)

(defun VPLAY:Turn-On-Xref-Layers (doc xref-name / layer layer-name)
  (vlax-for layer (vla-get-Layers doc)
    (setq layer-name (vla-get-Name layer))
    (if (VPLAY:Xref-Layer-P layer-name xref-name)
      (vl-catch-all-apply
        'vla-put-LayerOn
        (list layer :vlax-true)
      )
    )
  )
)

(defun VPLAY:Isolate-Layers (selected support in_viewport / doc layer lay keep xref-name)
  (setq keep selected)
  (foreach lay support
    (setq keep (VPLAY:Add-Unique-CI lay keep))
  )

  (if in_viewport
    (progn
      (command "_.VPLAYER" "_Freeze" "*" "_Current" "")
      (foreach lay keep
        (command "_.VPLAYER" "_Thaw" lay "_Current" "")
      )
      (command "_.REGEN")
      (prompt "\n✅ แสดงเฉพาะ Layer ที่เลือกใน Viewport เรียบร้อยแล้ว")
    )
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (vlax-for layer (vla-get-Layers doc)
        (vl-catch-all-apply
          'vla-put-LayerOn
          (list layer :vlax-false)
        )
      )
      (foreach lay keep
        (VPLAY:Set-Layer-On doc lay :vlax-true)
        ;; ถ้า Layer ที่เลือกเป็น Layer วาง Xref ให้เปิด Layer ภายใน Xref ด้วย
        (foreach xref-name (VPLAY:Xrefs-On-Layer lay)
          (VPLAY:Turn-On-Xref-Layers doc xref-name)
        )
      )
      (vla-Regen doc 1)
      (prompt "\n✅ แสดงเฉพาะ Layer ที่เลือก รวม Layer ภายใน Xref เรียบร้อยแล้ว")
    )
  )
  (princ)
)

(defun VPLAY:Restore-Hidden (in_viewport / doc layer count result)
  (if in_viewport
    (progn
      (command "_.VPLAYER" "_Thaw" "*" "_Current" "")
      (command "_.REGEN")
      (prompt "\n✅ เปิด Layer ที่ซ่อนด้วย VP Freeze ใน Viewport ปัจจุบันเรียบร้อยแล้ว")
    )
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
            count 0)
      (vlax-for layer (vla-get-Layers doc)
        (setq result
          (vl-catch-all-apply
            'vla-put-LayerOn
            (list layer :vlax-true)
          )
        )
        (if (not (vl-catch-all-error-p result))
          (setq count (1+ count))
        )
      )
      (vla-Regen doc 1)
      (prompt
        (strcat
          "\n✅ เปิด Layer ที่ OFF โดยไม่เปลี่ยนสถานะ Freeze จำนวน "
          (itoa count)
          " Layer"
        )
      )
    )
  )
  (princ)
)

;; คืน Layer ของ Block/Xref ที่ครอบวัตถุซึ่งตรงกับ Layer ที่ผู้ใช้เลือก
(defun Get-Support-Layers-At-Pickbox (ss selected / i info record effective enames ent lay layers)
  (setq i 0
        layers '())
  (if ss
    (repeat (sslength ss)
      (setq info (ssnamex ss i))
      (if (and info (setq record (car info)))
        (progn
          (setq effective (Get-Layer-From-SSNameX record))
          (if (and effective (VPLAY:Member-CI effective selected))
            (progn
              (setq enames
                (vl-remove-if-not
                  '(lambda (item) (= (type item) 'ENAME))
                  record
                )
              )
              ;; ตัวแรกคือวัตถุ Nested; ตัวที่เหลือคือ Block/Xref ที่ครอบอยู่
              (foreach ent (cdr enames)
                (if (setq lay (cdr (assoc 8 (entget ent))))
                  (if (/= lay "0")
                    (setq layers (VPLAY:Add-Unique-CI lay layers))
                  )
                )
              )
            )
          )
        )
      )
      (setq i (1+ i))
    )
  )
  layers
)

;; ----------------------------------------------------------------------
;; ฟังก์ชันย่อย: อ่าน Layer จากข้อมูล Nested ของ ssnamex
;; record มีวัตถุที่เลือกเป็น ENAME ตัวแรก และ Block/Xref ที่ครอบอยู่ถัดไป
;; ----------------------------------------------------------------------
(defun Get-Layer-From-SSNameX (record / enames edata layq containers nlayq)
  (setq enames
    (vl-remove-if-not
      '(lambda (item) (= (type item) 'ENAME))
      record
    )
  )

  (if enames
    (progn
      (setq edata (entget (car enames)))
      (setq layq (cdr (assoc 8 edata)))

      (if (= layq "0")
        (progn
          (setq containers (cdr enames))
          (while
            (and
              containers
              (= (setq nlayq (cdr (assoc 8 (entget (car containers))))) "0")
            )
            (setq containers (cdr containers))
          )
          (if nlayq
            (setq layq nlayq)
          )
        )
      )
    )
  )

  layq
)

;; ----------------------------------------------------------------------
;; ฟังก์ชันย่อย: เก็บ Layer จาก Selection Set แบบหลายวัตถุ
;; ใช้กับ Isolate เพื่อรองรับ Window/Crossing และ Block/Xref แบบเดียวกับ LCC
;; ----------------------------------------------------------------------
(defun Get-Top-Level-Layers-From-Selection (ss / i ent data lay layers)
  (setq i 0
        layers '())
  (if ss
    (repeat (sslength ss)
      (setq ent  (ssname ss i)
            data (entget ent)
            lay  (cdr (assoc 8 data)))
      (if lay
        (setq layers (VPLAY:Add-Unique-CI lay layers))
      )
      (setq i (1+ i))
    )
  )
  (acad_strlsort layers)
)

;; ----------------------------------------------------------------------
;; ฟังก์ชันย่อย: เก็บเฉพาะ Layer ของวัตถุทุกชิ้นภายใน Pickbox
;; ----------------------------------------------------------------------
(defun Get-Layers-At-Pickbox (ss / i info lay layers)
  (setq i 0
        layers '())

  (repeat (sslength ss)
    (setq info (ssnamex ss i))
    (if (and info (car info))
      (if (setq lay (Get-Layer-From-SSNameX (car info)))
        (if (not (member lay layers))
          (setq layers (cons lay layers))
        )
      )
    )
    (setq i (1+ i))
  )

  (acad_strlsort layers)
)

;; ----------------------------------------------------------------------
;; ฟังก์ชันย่อย: หา Layer ของวัตถุย่อยที่ nentsel คลิกโดนจริง
;; ใช้เส้นทาง Block/Xref จากผลการคลิกครั้งเดิม และไล่ผ่าน Layer 0
;; ----------------------------------------------------------------------
(defun Get-Layer-From-Nested-Selection (sel / edata layq nestlist nlayq)
  (setq edata (entget (car sel)))
  (setq layq (cdr (assoc 8 edata)))

  (if (= layq "0")
    (setq layq
      (cond
        ((and (> (length sel) 3) (listp (cadddr sel)))
         (setq nestlist (cadddr sel))
         (while
           (and
             nestlist
             (= (setq nlayq (cdr (assoc 8 (entget (car nestlist))))) "0")
           )
           (setq nestlist (cdr nestlist))
         )
         (if nlayq nlayq "0")
        )
        ((= (cdr (assoc 0 edata)) "ATTRIB")
         (cdr (assoc 8 (entget (cdr (assoc 330 edata)))))
        )
        ("0")
      )
    )
  )

  layq
)

;; ----------------------------------------------------------------------
;; ฟังก์ชันย่อย: สแกน Layer ทั้งหมดอย่างปลอดภัย
;; ----------------------------------------------------------------------
(defun Get-Layers-From-Selection (ss / i ent edata lay layerlist checked-blocks process-block itm2)
  (setq layerlist '())
  (setq checked-blocks '()) ; กันบั๊ก Block วนลูปไม่รู้จบ

  (defun process-block (bname / blk bent bdata blay)
    (if (and (= (type bname) 'STR) (not (member (strcase bname) checked-blocks)))
      (progn
        (setq checked-blocks (cons (strcase bname) checked-blocks))
        (if (setq blk (tblsearch "BLOCK" bname))
          (progn
            (setq bent (cdr (assoc -2 blk)))
            (while bent
              (if (setq bdata (entget bent))
                (progn
                  (if (setq blay (cdr (assoc 8 bdata)))
                    (if (and (= (type blay) 'STR) (not (member blay layerlist)))
                      (setq layerlist (cons blay layerlist))
                    )
                  )
                  (if (= (cdr (assoc 0 bdata)) "INSERT")
                    (if (setq itm2 (assoc 2 bdata))
                      (process-block (cdr itm2))
                    )
                  )
                )
              )
              (setq bent (entnext bent))
            )
          )
        )
      )
    )
  )
  
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (if (setq edata (entget ent))
      (progn
        (if (setq lay (cdr (assoc 8 edata)))
          (if (and (= (type lay) 'STR) (not (member lay layerlist)))
            (setq layerlist (cons lay layerlist))
          )
        )
        (if (= (cdr (assoc 0 edata)) "INSERT")
          (if (setq itm2 (assoc 2 edata))
            (process-block (cdr itm2))
          )
        )
      )
    )
    (setq i (1+ i))
  )
  
  ;; คืนค่า Layer แบบ Reverse (เอาฟังก์ชันเรียงตัวอักษรออก ป้องกันโปรแกรมล่ม)
  (reverse layerlist)
)

;; ----------------------------------------------------------------------
;; ฟังก์ชันย่อย: แสดง Layer Color และ VP Color ในรายการเลือก Layer
;; ----------------------------------------------------------------------
(defun VPLAY:RGB-Label (value)
  (strcat
    (itoa (logand (lsh value -16) 255)) ","
    (itoa (logand (lsh value -8) 255)) ","
    (itoa (logand value 255))
  )
)

(defun VPLAY:Encoded-Color-Label (value / method)
  (setq method (logand (lsh value -24) 255))
  (cond
    ((= method 195) (strcat "ACI " (itoa (logand value 65535))))
    ((= method 192) "BYLAYER")
    ((= method 193) "BYBLOCK")
    (T (strcat "RGB " (VPLAY:RGB-Label value)))
  )
)

(defun VPLAY:Layer-Color-Label (lay / data value)
  (if (setq data (tblsearch "LAYER" lay))
    (cond
      ((setq value (cdr (assoc 420 data)))
       (VPLAY:Encoded-Color-Label value))
      ((setq value (cdr (assoc 62 data)))
       (strcat "ACI " (itoa (abs value))))
      (T "UNKNOWN")
    )
    "NOT FOUND"
  )
)

(defun VPLAY:Current-Viewport-Ename (/ ss)
  (if
    (and
      (= (getvar "TILEMODE") 0)
      (> (getvar "CVPORT") 1)
      (setq ss
        (ssget "_X"
          (list
            '(0 . "VIEWPORT")
            (cons 69 (getvar "CVPORT"))
            (cons 410 (getvar "CTAB"))
          )
        )
      )
    )
    (ssname ss 0)
  )
)

(defun VPLAY:VP-Color-Data (lay viewport / layer_ent layer_data dict_ent xrec pair active matched result)
  (if
    (and
      viewport
      (setq layer_ent (tblobjname "LAYER" lay))
      (setq layer_data (entget layer_ent))
      (setq dict_ent (cdr (assoc 360 layer_data)))
      (setq xrec (dictsearch dict_ent "ADSK_XREC_LAYER_COLOR_OVR"))
    )
    (foreach pair xrec
      (cond
        ((= 102 (car pair))
         (cond
           ((wcmatch (cdr pair) "{ADSK_LYR_COLOR_OVERRIDE*")
            (setq active T
                  matched nil))
           ((= (cdr pair) "}")
            (setq active nil
                  matched nil))
         )
        )
        ((and active (= 335 (car pair)))
         (setq matched (equal (cdr pair) viewport)))
        ((and active matched (= 420 (car pair)))
         (setq result (cons pair result)))
        ((and active matched (= 430 (car pair)))
         (setq result (cons pair result)))
      )
    )
  )
  (reverse result)
)

(defun VPLAY:VP-Color-Label (lay viewport / data value method)
  (cond
    ((not viewport) "N/A")
    ((setq data (VPLAY:VP-Color-Data lay viewport))
     (cond
       ((setq value (cdr (assoc 420 data)))
        (setq method (logand (lsh value -24) 255))
        (if (= method 192)
          (strcat "NO OVERRIDE (" (VPLAY:Layer-Color-Label lay) ")")
          (VPLAY:Encoded-Color-Label value)
        )
       )
       (T "UNKNOWN")
     )
    )
    (T (strcat "NO OVERRIDE (" (VPLAY:Layer-Color-Label lay) ")"))
  )
)

(defun VPLAY:Fit-Text (text width / keep)
  (cond
    ((< (strlen text) width)
     (strcat
       text
       (substr "                                                                                "
               1
               (- width (strlen text)))
     )
    )
    ((= (strlen text) width) text)
    (T
     (setq keep (- width 3))
     (strcat (substr text 1 keep) "..."))
  )
)

(defun VPLAY:Layer-Row (lay viewport)
  (strcat
    (VPLAY:Fit-Text lay 34) " | "
    (VPLAY:Fit-Text (VPLAY:Layer-Color-Label lay) 20) " | "
    (VPLAY:VP-Color-Label lay viewport)
  )
)

;; ----------------------------------------------------------------------
;; ฟังก์ชันย่อย: สร้าง DCL Popup Dialog เด้งขึ้นมาให้เลือก Layer
;; ----------------------------------------------------------------------
(defun Show-Layer-Select-Dialog (laylist / dcl_file f dcl_id result sel_indices viewport display_rows)
  (setq viewport (VPLAY:Current-Viewport-Ename))
  (setq display_rows
    (mapcar
      '(lambda (lay) (VPLAY:Layer-Row lay viewport))
      laylist
    )
  )
  (setq dcl_file (vl-filename-mktemp "laysel.dcl"))
  (setq f (open dcl_file "w"))
  (write-line "laysel : dialog {" f)
  (write-line "  label = \"Layer ใกล้จุดคลิก - เลือก Layer ที่ต้องการ\";" f)
  (write-line "  : text { label = \"LAYER                              | LAYER COLOR          | VP COLOR\"; fixed_width_font = true; }" f)
  (write-line "  : list_box {" f)
  (write-line "    key = \"lstLayers\";" f)
  (write-line "    multiple_select = true;" f)
  (write-line "    fixed_width_font = true;" f)
  (write-line "    height = 20;" f)
  (write-line "    width = 100;" f)
  (write-line "  }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  
  (setq dcl_id (load_dialog dcl_file))
  (if (and dcl_id (new_dialog "laysel" dcl_id))
    (progn
      (start_list "lstLayers")
      (mapcar 'add_list display_rows)
      (end_list)
      
      (set_tile "lstLayers" "0")
      (setq sel_indices "0")
      
      (action_tile "lstLayers" "(setq sel_indices $value)")
      (action_tile "accept" "(done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")
      
      (setq result (start_dialog))
      (unload_dialog dcl_id)
    )
    (setq result 0)
  )
  (vl-file-delete dcl_file)
  
  (if (= result 1)
    ;; แก้บั๊ก "stringp 8" ตรงจุดนี้ (ตัดคำสั่งแปลงตัวเลขซ้ำซ้อนทิ้งไป)
    (mapcar '(lambda (x) (nth x laylist)) (read (strcat "(" sel_indices ")")))
    nil
  )
)

;; ----------------------------------------------------------------------
;; ฟังก์ชันย่อย: True -> RGB  -  Lee Mac 2011
;; ----------------------------------------------------------------------
(defun LM:True->RGB ( c )
    (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(8 16 24))
)

(princ "\nพิมพ์คำสั่ง VPLAY เพื่อเริ่มต้นใช้งาน!")
(princ)
