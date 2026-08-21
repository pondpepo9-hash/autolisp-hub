(defun c:ACI2RGB ( / ss i ent obj doc layers blocks processed-blocks process-color process-object)
  (vl-load-com)
  
  ;; เตรียมดึงข้อมูล Document, Layers และ Blocks ของไฟล์นี้
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))
  (setq blocks (vla-get-Blocks doc))
  (setq processed-blocks nil) ; ตัวแปรเก็บรายชื่อ Block ที่แปลงแล้ว (ป้องกันการทำซ้ำและลดเวลาประมวลผล)

  ;; ฟังก์ชันย่อย: สำหรับแปลงสีวัตถุ (ACI หรือ ByLayer -> RGB)
  (defun process-color (obj / tColor cMethod layName layObj layTColor layMethod r g b)
    ;; เช็คก่อนว่าวัตถุนี้มีคุณสมบัติ TrueColor ให้เปลี่ยนหรือไม่
    (if (vlax-property-available-p obj 'TrueColor)
      (progn
        (setq tColor (vla-get-TrueColor obj))
        (setq cMethod (vla-get-ColorMethod tColor))
        
        (cond
          ;; กรณีที่ 1: วัตถุเป็นสี Index Color (195)
          ((= cMethod 195)
            (setq r (vla-get-Red tColor)
                  g (vla-get-Green tColor)
                  b (vla-get-Blue tColor))
            (vla-SetRGB tColor r g b)
            ;; ใช้ vl-catch-all-apply ป้องกัน Error กรณี Layer ถูก Lock ไว้
            (vl-catch-all-apply 'vla-put-TrueColor (list obj tColor))
          )
          
          ;; กรณีที่ 2: วัตถุเป็นสี ByLayer (192)
          ((= cMethod 192)
            (setq layName (vla-get-Layer obj))
            (setq layObj (vla-Item layers layName))
            (setq layTColor (vla-get-TrueColor layObj))
            (setq layMethod (vla-get-ColorMethod layTColor))
            
            (if (or (= layMethod 195) (= layMethod 194))
              (progn
                (setq r (vla-get-Red layTColor)
                      g (vla-get-Green layTColor)
                      b (vla-get-Blue layTColor))
                (vla-SetRGB tColor r g b)
                (vl-catch-all-apply 'vla-put-TrueColor (list obj tColor))
              )
            )
          )
        )
      )
    )
  )

  ;; ฟังก์ชันย่อย: สำหรับคัดกรองว่าเป็นวัตถุธรรมดา หรือ Block
  (defun process-object (obj / objName blkName blkDef subObj)
    (setq objName (vlax-get-property obj 'ObjectName))
    
    ;; ถ้าเป็น Block
    (if (= objName "AcDbBlockReference")
      (progn
        ;; หาชื่อ Block (รองรับ Dynamic Block ด้วย EffectiveName)
        (if (vlax-property-available-p obj 'EffectiveName)
          (setq blkName (vla-get-EffectiveName obj))
          (setq blkName (vla-get-Name obj))
        )
        
        ;; เช็คว่า Block นี้เคยโดนแปลงไปหรือยัง (ถ้ายัง ให้มุดเข้าไปทำ)
        (if (not (member blkName processed-blocks))
          (progn
            (setq processed-blocks (cons blkName processed-blocks))
            (setq blkDef (vla-Item blocks blkName))
            
            ;; วนลูปจัดการวัตถุทุกชิ้นที่อยู่ใน Block (ถ้าเจอ Block ซ้อน Block มันจะเรียกตัวเองซ้ำเพื่อมุดต่อ)
            (vlax-for subObj blkDef
              (process-object subObj)
            )
          )
        )
        ;; อย่าลืมแปลงสีของตัว Block Reference เองด้วย (เผื่อมันโดน Override สีไว้ข้างนอก)
        (process-color obj)
      )
      
      ;; ถ้าเป็นวัตถุธรรมดา (Line, Polyline, Circle ฯลฯ)
      (process-color obj)
    )
  )

  ;; ------------------- เริ่มต้นการทำงานหลัก -------------------
  (prompt "\nSelect objects or Blocks to convert ACI/ByLayer to True Color (RGB): ")
  (if (setq ss (ssget))
    (progn
      ;; สร้างจุด Undo เพื่อให้กด Ctrl+Z ทีเดียวกลับมาได้ทั้งหมด
      (vla-StartUndoMark doc)
      
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        
        ;; ส่งวัตถุไปให้ฟังก์ชันจัดการ
        (process-object obj)
        
        (setq i (1+ i))
      )
      
      ;; รีเฟรชหน้าจอภาพเพื่อให้ Block ที่ถูกอัปเดตไส้ในแสดงผลสีใหม่ทันที
      (vla-Regen doc acAllViewports)
      (vla-EndUndoMark doc)
      
      (princ "\n--- ACI & ByLayer to RGB (Including Blocks) Conversion Complete! ---")
    )
    (princ "\nNo objects selected.")
  )
  (princ)
)
(princ "\nType ACI2RGB to run the command.")
(princ)