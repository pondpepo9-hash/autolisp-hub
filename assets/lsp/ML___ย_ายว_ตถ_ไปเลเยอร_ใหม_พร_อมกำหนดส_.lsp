(defun c:ML (/ ss layname col i ent vla-obj)
  (vl-load-com)
  
  ;; 1. คลุมเลือกวัตถุ
  (princ "\nเลือกวัตถุที่ต้องการย้ายไป Layer ใหม่: ")
  (setq ss (ssget))
  
  (if ss
    (progn
      ;; 2. ตั้งชื่อ Layer (รองรับการเคาะ Spacebar ในชื่อ)
      (setq layname (getstring T "\nระบุชื่อ Layer: "))
      
      (if (/= layname "")
        (progn
          ;; 3. เลือก Code สี (Index Color 1-255)
          (setq col (getint "\nระบุหมายเลขสี (1-255): "))
          (if (not col) (setq col 7)) ; ถ้าไม่ใส่ จะตั้งเป็นสีขาว/ดำ (Color 7)

          ;; ตรวจสอบว่ามี Layer นี้หรือยัง ถ้าไม่มีให้สร้างใหม่ ถ้ามีแล้วให้อัปเดตสี
          (if (not (tblsearch "LAYER" layname))
            (command "-LAYER" "M" layname "C" col "" "")
            (command "-LAYER" "C" col layname "")
          )

          ;; 4. วนลูปย้ายวัตถุเข้า Layer และปรับสีเป็น ByLayer
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq vla-obj (vlax-ename->vla-object ent))
            
            ;; ย้าย Layer
            (vl-catch-all-apply 'vla-put-Layer (list vla-obj layname))
            ;; ปรับสีวัตถุให้เป็น ByLayer (256) เพื่อให้สีวิ่งตาม Layer ที่ตั้งไว้
            (vl-catch-all-apply 'vla-put-color (list vla-obj 256))
            
            (setq i (1+ i))
          )
          
          (princ (strcat "\nเรียบร้อย! ย้ายวัตถุ " (itoa (sslength ss)) " ชิ้น ไปยัง Layer: " layname))
        )
        (princ "\nยกเลิก: ไม่ได้ระบุชื่อ Layer")
      )
    )
    (princ "\nยกเลิก: ไม่พบวัตถุที่เลือก")
  )
  (princ)
)

(princ "\nโหลด LISP สำเร็จ! พิมพ์ ML เพื่อเริ่มใช้งาน")
(princ)