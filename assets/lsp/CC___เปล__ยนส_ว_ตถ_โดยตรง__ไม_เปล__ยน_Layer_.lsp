(defun c:CC (/ ss col i ent vla-obj)
  (vl-load-com)
  
  ;; 1. สั่งให้คลุมเลือกวัตถุ
  (princ "\nเลือกวัตถุที่ต้องการเปลี่ยนสี: ")
  (setq ss (ssget))
  
  (if ss
    (progn
      ;; 2. ให้ใส่ตัวเลขสี (หลังกด Enter จบการเลือก)
      (setq col (getint "\nใส่หมายเลขสี (0=ByBlock, 256=ByLayer, หรือ 1-255): "))
      
      ;; เช็คว่าใส่ตัวเลขถูกต้องหรือไม่ (ต้องอยู่ระหว่าง 0 ถึง 256)
      (if (and col (>= col 0) (<= col 256))
        (progn
          (setq i 0)
          
          ;; 3. วนลูปเปลี่ยนสีวัตถุทุกชิ้นที่คลุมเลือกไว้
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq vla-obj (vlax-ename->vla-object ent))
            
            ;; ใช้คำสั่งเปลี่ยนสี (ใส่ vl-catch-all-apply ดัก Error ไว้เผื่อเจอเลเยอร์ที่ถูก Lock)
            (vl-catch-all-apply 'vla-put-color (list vla-obj col))
            
            (setq i (1+ i))
          )
          
          ;; แจ้งเตือนเมื่อทำงานเสร็จ
          (princ (strcat "\nเปลี่ยนสีวัตถุจำนวน " (itoa (sslength ss)) " ชิ้น สำเร็จ!"))
        )
        (princ "\nยกเลิกคำสั่ง: หมายเลขสีไม่ถูกต้อง (ต้องใส่ตัวเลข 0 - 256 เท่านั้น)")
      )
    )
    (princ "\nยกเลิกคำสั่ง: ไม่ได้เลือกวัตถุ")
  )
  (princ)
)
(princ "\nโหลด LISP สำเร็จ! พิมพ์ CC เพื่อเริ่มใช้งาน")
(princ)