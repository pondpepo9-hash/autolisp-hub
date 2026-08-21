(defun c:colorlines (/ start-pt line-length line-spacing color-index y-pos x1 x2 y-current text-pt line-data text-data)
  (setq start-pt '(0.0 0.0 0.0))      ; จุดเริ่มต้น
  (setq line-length 10.0)              ; ความยาวเส้น 10 mm
  (setq line-spacing 1.0)              ; ระยะห่างระหว่างเส้น 1 mm
  (setq color-index 1)                 ; เริ่มต้นที่สี index 1
  
  (princ "\nกำลังสร้างเส้นทั้งหมด 255 เส้น พร้อมหมายเลขสี...")
  
  ; วนลูปสร้างเส้นทั้งหมด 255 เส้น
  (while (<= color-index 255)
    ; คำนวณตำแหน่ง Y ปัจจุบัน (เส้นแรกที่ y=0, เส้นที่สองที่ y=-1, ...)
    (setq y-current (- 0.0 (* (1- color-index) line-spacing)))
    
    ; คำนวณจุดเริ่มต้นและจุดสิ้นสุดของเส้น
    (setq x1 (car start-pt))
    (setq x2 (+ x1 line-length))
    
    ; สร้างข้อมูลเส้น (LINE entity)
    (setq line-data
      (list
        (cons 0 "LINE")                 ; ชนิด entity
        (cons 100 "AcDbEntity")
        (cons 67 0)
        (cons 8 "0")                    ; layer
        (cons 62 color-index)           ; สี index color
        (cons 100 "AcDbLine")
        (cons 10 (list x1 y-current 0.0))    ; จุดเริ่มต้น
        (cons 11 (list x2 y-current 0.0))    ; จุดสิ้นสุด
      )
    )
    
    ; สร้างเส้น
    (entmake line-data)
    
    ; คำนวณตำแหน่งสำหรับ text (หน้าเส้น ห่างออกไป 2 mm)
    (setq text-pt (list (- x1 2.0) y-current 0.0))
    
    ; สร้างข้อมูล text
    (setq text-data
      (list
        (cons 0 "TEXT")                 ; ชนิด entity
        (cons 100 "AcDbEntity")
        (cons 67 0)
        (cons 8 "0")                    ; layer
        (cons 62 color-index)           ; สีเดียวกับเส้น
        (cons 100 "AcDbText")
        (cons 10 text-pt)               ; ตำแหน่ง text
        (cons 40 0.5)                   ; ขนาด text (0.5 mm)
        (cons 1 (itoa color-index))     ; ข้อความ (หมายเลขสี)
        (cons 50 0.0)                   ; มุมหมุน
        (cons 7 "STANDARD")             ; text style
        (cons 71 0)                     ; text generation flags
        (cons 72 2)                     ; horizontal alignment (right)
        (cons 73 1)                     ; vertical alignment (middle)
        (cons 11 text-pt)               ; alignment point
      )
    )
    
    ; สร้าง text
    (entmake text-data)
    
    ; แสดงความคืบหน้า
    (if (= 0 (rem color-index 25))
      (princ (strcat "\nสร้างแล้ว " (itoa color-index) " เส้น..."))
    )
    
    (setq color-index (1+ color-index))
  )
  
  (princ "\nสร้างเส้นและหมายเลขสีทั้งหมด 255 เส้น เรียบร้อย!")
  (princ "\nใช้คำสั่ง ZOOM EXTENTS เพื่อดูทั้งหมด")
  (princ)
)

; คำสั่งสร้างเส้นแนวนอน (เหมือนเดิม)
(defun c:hcolorlines (/ start-pt line-length line-spacing color-index x-pos y1 y2 x-current text-pt line-data text-data)
  (setq start-pt '(0.0 0.0 0.0))      ; จุดเริ่มต้น
  (setq line-length 10.0)              ; ความยาวเส้น 10 mm
  (setq line-spacing 1.0)              ; ระยะห่างระหว่างเส้น 1 mm
  (setq color-index 1)                 ; เริ่มต้นที่สี index 1
  
  (princ "\nกำลังสร้างเส้นแนวตั้งทั้งหมด 255 เส้น พร้อมหมายเลขสี...")
  
  ; วนลูปสร้างเส้นทั้งหมด 255 เส้น (แนวตั้ง)
  (while (<= color-index 255)
    ; คำนวณตำแหน่ง X ปัจจุบัน
    (setq x-current (* (1- color-index) line-spacing))
    
    ; คำนวณจุดเริ่มต้นและจุดสิ้นสุดของเส้น
    (setq y1 (cadr start-pt))
    (setq y2 (+ y1 line-length))
    
    ; สร้างข้อมูลเส้น (LINE entity)
    (setq line-data
      (list
        (cons 0 "LINE")
        (cons 100 "AcDbEntity")
        (cons 67 0)
        (cons 8 "0")
        (cons 62 color-index)
        (cons 100 "AcDbLine")
        (cons 10 (list x-current y1 0.0))     ; จุดเริ่มต้น
        (cons 11 (list x-current y2 0.0))     ; จุดสิ้นสุด
      )
    )
    
    ; สร้างเส้น
    (entmake line-data)
    
    ; คำนวณตำแหน่งสำหรับ text (ใต้เส้น ห่างออกไป 1 mm)
    (setq text-pt (list x-current (- y1 1.0) 0.0))
    
    ; สร้างข้อมูล text
    (setq text-data
      (list
        (cons 0 "TEXT")
        (cons 100 "AcDbEntity")
        (cons 67 0)
        (cons 8 "0")
        (cons 62 color-index)
        (cons 100 "AcDbText")
        (cons 10 text-pt)
        (cons 40 0.5)
        (cons 1 (itoa color-index))
        (cons 50 0.0)
        (cons 7 "STANDARD")
        (cons 71 0)
        (cons 72 1)                     ; horizontal alignment (center)
        (cons 73 1)                     ; vertical alignment (middle)
        (cons 11 text-pt)
      )
    )
    
    ; สร้าง text
    (entmake text-data)
    
    ; แสดงความคืบหน้า
    (if (= 0 (rem color-index 25))
      (princ (strcat "\nสร้างแล้ว " (itoa color-index) " เส้น..."))
    )
    
    (setq color-index (1+ color-index))
  )
  
  (princ "\nสร้างเส้นแนวตั้งและหมายเลขสีทั้งหมด 255 เส้น เรียบร้อย!")
  (princ "\nใช้คำสั่ง ZOOM EXTENTS เพื่อดูทั้งหมด")
  (princ)
)

(princ "\nโหลด LISP เรียบร้อย!")
(princ "\nคำสั่งที่ใช้ได้:")
(princ "\n  COLORLINES   - สร้างเส้นแนวนอน 255 เส้น พร้อมหมายเลขสี")
(princ "\n  HCOLORLINES  - สร้างเส้นแนวตั้ง 255 เส้น พร้อมหมายเลขสี")
(princ)