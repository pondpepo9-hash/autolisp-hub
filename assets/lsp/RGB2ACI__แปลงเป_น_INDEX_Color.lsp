(defun c:RGB2ACI ( / ss i ent obj doc blocks processed-blocks process-color process-object)
  (vl-load-com)
  
  ;; เตรียมดึงข้อมูล Document และ Blocks
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blocks (vla-get-Blocks doc))
  (setq processed-blocks nil) ; ตัวแปรเก็บรายชื่อ Block ที่แปลงแล้ว

  ;; ฟังก์ชันย่อย: สำหรับแปลงสีวัตถุ (RGB -> ACI)
  (defun process-color (obj / tColor cMethod cIndex)
    (if (vlax-property-available-p obj 'TrueColor)
      (progn
        (setq tColor (vla-get-TrueColor obj))
        (setq cMethod (vla-get-ColorMethod tColor))
        
        ;; เช็คว่าวัตถุถูกตั้งค่าเป็น True Color (RGB) หรือไม่ (Method = 194)
        (if (= cMethod 194)
          (progn
            ;; ดึงค่า Index Color (1-255) ที่ใกล้เคียงกับสี RGB เดิมที่สุด
            (setq cIndex (vla-get-ColorIndex tColor))
            
            ;; เปลี่ยน Method ให้กลับเป็นแบบ ACI (Method = 195)
            (vla-put-ColorMethod tColor 195)
            (vla-put-ColorIndex tColor cIndex)
            
            ;; อัปเดตสีกลับเข้าไปในวัตถุ (ใช้ vl-catch-all-apply ป้องกัน Error เลเยอร์ล็อค)
            (vl-catch-all-apply 'vla-put-TrueColor (list obj tColor))
          )
        )
      )
    )
  )

  ;; ฟังก์ชันย่อย: สำหรับมุดเข้า Block
  (defun process-object (obj / objName blkName blkDef subObj)
    (setq objName (vlax-get-property obj 'ObjectName))
    
    (if (= objName "AcDbBlockReference")
      (progn
        ;; หาชื่อ Block
        (if (vlax-property-available-p obj 'EffectiveName)
          (setq blkName (vla-get-EffectiveName obj))
          (setq blkName (vla-get-Name obj))
        )
        
        ;; เช็คว่า Block นี้เคยแปลงไปหรือยัง
        (if (not (member blkName processed-blocks))
          (progn
            (setq processed-blocks (cons blkName processed-blocks))
            (setq blkDef (vla-Item blocks blkName))
            
            ;; วนลูปจัดการวัตถุทุกชิ้นใน Block
            (vlax-for subObj blkDef
              (process-object subObj)
            )
          )
        )
        ;; เปลี่ยนสีของตัว Block Reference เองด้วย
        (process-color obj)
      )
      
      ;; ถ้าเป็นวัตถุธรรมดา
      (process-color obj)
    )
  )

  ;; ------------------- เริ่มต้นการทำงานหลัก -------------------
  (prompt "\nSelect objects or Blocks to convert True Color (RGB) back to Index Color (ACI): ")
  (if (setq ss (ssget))
    (progn
      ;; สร้างจุด Undo (Ctrl+Z)
      (vla-StartUndoMark doc)
      
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        
        ;; ส่งวัตถุไปให้ฟังก์ชันจัดการ
        (process-object obj)
        
        (setq i (1+ i))
      )
      
      ;; รีเฟรชหน้าจอภาพ
      (vla-Regen doc acAllViewports)
      (vla-EndUndoMark doc)
      
      (princ "\n--- RGB to ACI (Including Blocks) Conversion Complete! ---")
    )
    (princ "\nNo objects selected.")
  )
  (princ)
)
(princ "\nType RGB2ACI to run the command.")
(princ)