(defun C:C2L ( / ent edata lay col_idx col_true col_book rgb value index)
  (vl-load-com)
  
  ;; 1. ให้ผู้ใช้คลิกเลือกวัตถุ
  (if (setq ent (car (entsel "\nคลิกเลือกวัตถุที่ต้องการดึงสีไปที่ Layer: ")))
    (progn
      (setq edata (entget ent))
      (setq lay (cdr (assoc 8 edata))) ; ดึงชื่อ Layer ของวัตถุ
      
      ;; ดึงค่าสีประเภทต่างๆ จาก Data ของวัตถุ
      (setq col_idx (cdr (assoc 62 edata)))  ; สี Index Color (1-255)
      (setq col_true (cdr (assoc 420 edata))) ; สี True Color (RGB)
      (setq col_book (cdr (assoc 430 edata))) ; สี Color Book
      
      ;; 2. ตรวจสอบและตั้งค่าสีให้ Layer
      (cond
        ;; กรณีที่ 1: เป็นสีประเภท Color Book (ลำดับความสำคัญสูงสุด)
        (col_book
          (setq value col_book)
          (setq index (vl-string-position 36 value)) ; แยกชื่อ Book กับชื่อสี ด้วยตัวอักษร $ (ASCII 36)
          (if index
            (progn
              (command "_.-layer" "_C" "_CO" (substr value 1 index) (substr value (+ index 2)) lay "")
              (prompt (strcat "\n✅ เปลี่ยนสี Layer '" lay "' เป็น ColorBook: " value " สำเร็จ!"))
            )
          )
        )
        
        ;; กรณีที่ 2: เป็นสีประเภท True Color (RGB)
        (col_true
          (setq rgb (LM:True->RGB col_true))
          (command "_.-layer" "_C" "_T" (strcat (itoa (car rgb)) "," (itoa (cadr rgb)) "," (itoa (caddr rgb))) lay "")
          (prompt (strcat "\n✅ เปลี่ยนสี Layer '" lay "' เป็น True Color RGB(" (itoa (car rgb)) "," (itoa (cadr rgb)) "," (itoa (caddr rgb)) ") สำเร็จ!"))
        )
        
        ;; กรณีที่ 3: เป็นสีประเภท Index Color (1-255) โดยต้องไม่เป็น 0 (ByBlock) หรือ 256 (ByLayer)
        ((and col_idx (/= col_idx 256) (/= col_idx 0))
          (command "_.-layer" "_C" col_idx lay "")
          (prompt (strcat "\n✅ เปลี่ยนสี Layer '" lay "' เป็นสี Index Color: " (itoa col_idx) " สำเร็จ!"))
        )
        
        ;; กรณีที่ 4: วัตถุเป็น ByLayer อยู่แล้ว
        (t
          (prompt (strcat "\n⚠️ วัตถุนี้เป็นสี ByLayer อยู่แล้ว ไม่มีการเปลี่ยนแปลงสี Layer"))
        )
      )

      ;; 3. เปลี่ยนสีของตัววัตถุให้กลับเป็น ByLayer (หากตอนแรกมีการตั้งค่าสีอื่นไว้)
      (if (or col_book col_true (and col_idx (/= col_idx 256) (/= col_idx 0)))
        (progn
          (command "_.chprop" ent "" "_C" "_ByLayer" "")
          (prompt "\n✨ คืนค่าสีวัตถุให้เป็น ByLayer เรียบร้อยแล้ว")
        )
      )
    )
    (prompt "\n❌ ไม่ได้เลือกวัตถุ")
  )
  
  (princ)
)

;; ฟังก์ชันแปลงค่า True Color เป็น RGB (Lee Mac 2011)
(defun LM:True->RGB ( c )
    (mapcar '(lambda ( x ) (lsh (lsh (fix c) x) -24)) '(8 16 24))
)

(princ "\nโหลดคำสั่งสำเร็จ! พิมพ์ C2L เพื่อใช้งาน")
(princ)