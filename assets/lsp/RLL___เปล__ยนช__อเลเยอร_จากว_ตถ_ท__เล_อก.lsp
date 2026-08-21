(defun c:RLL (/ ent old-name new-name doc layers lay-obj)
  (vl-load-com)
  
  ;; 1. คลิกเลือกวัตถุ (เลือกได้ทีละ 1 ชิ้นเพื่อดึงชื่อ Layer)
  (setq ent (entsel "\nคลิกวัตถุเพื่อแก้ไขชื่อ Layer: "))
  
  (if ent
    (progn
      ;; ดึงชื่อ Layer เดิมจากวัตถุที่คลิก
      (setq old-name (cdr (assoc 8 (entget (car ent)))))
      
      ;; ป้องกันการเผลอไปเปลี่ยนชื่อ Layer ระบบ (0 และ Defpoints)
      (if (or (= (strcase old-name) "0") (= (strcase old-name) "DEFPOINTS"))
        (princ (strcat "\nไม่สามารถเปลี่ยนชื่อ Layer ระบบ '" old-name "' ได้ครับ!"))
        (progn
          ;; 2. โชว์ชื่อเดิม และให้พิมพ์ชื่อใหม่ (รองรับการเคาะ Spacebar)
          (setq new-name (getstring T (strcat "\nตั้งชื่อใหม่ให้ Layer [" old-name "]: ")))
          
          ;; เช็คว่าผู้ใช้พิมพ์ชื่อใหม่หรือไม่ และชื่อต้องไม่ซ้ำกับของเดิม
          (if (and (/= new-name "") (/= new-name old-name))
            ;; เช็คว่าชื่อใหม่ที่พิมพ์ มีคนใช้ตั้งเป็น Layer อื่นไปแล้วหรือยัง
            (if (tblsearch "LAYER" new-name)
              (princ (strcat "\nยกเลิก: มี Layer ชื่อ '" new-name "' อยู่ในไฟล์แล้ว!"))
              (progn
                ;; 3. ใช้ Visual LISP เข้าไปแก้ชื่อในระบบ Layer
                (setq doc (vla-get-activedocument (vlax-get-acad-object)))
                (setq layers (vla-get-layers doc))
                (setq lay-obj (vla-item layers old-name))
                
                ;; ทำการเปลี่ยนชื่อ
                (vl-catch-all-apply 'vla-put-Name (list lay-obj new-name))
                
                (princ (strcat "\nเปลี่ยนชื่อ Layer จาก '" old-name "' เป็น '" new-name "' สำเร็จ!"))
              )
            )
            (princ "\nยกเลิก: ไม่ได้ระบุชื่อใหม่ หรือชื่อซ้ำกับของเดิม")
          )
        )
      )
    )
    (princ "\nยกเลิกคำสั่ง: ไม่พบวัตถุ")
  )
  (princ)
)
(princ "\nโหลด LISP สำเร็จ! พิมพ์ RL เพื่อเริ่มใช้งาน")
(princ)