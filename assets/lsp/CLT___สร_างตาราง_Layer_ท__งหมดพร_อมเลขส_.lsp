(defun C:CLT (/ layer_table layer_name layer_list current_pt text_height row_height col_width_no col_width_layer col_width_color old_cmd pt1 pt2 i n)
  (vl-load-com) ; โหลดฟังก์ชัน ActiveX
  (setq old_cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0) ; ปิดการแสดงผลคำสั่ง
  ; กำหนดค่าเริ่มต้น
  (setq current_pt '(0.0 0.0 0.0) ; ตำแหน่งเริ่มต้นของตาราง
        text_height 2.5 ; ความสูงของ Text
        row_height 5.0 ; ความสูงของแถว
        col_width_no 15.0 ; ความกว้างคอลัมน์ No.
        col_width_layer 50.0 ; ความกว้างคอลัมน์ Layer Name
        col_width_color 15.0 ; ความกว้างคอลัมน์ Color
        layer_list '() ; รายการเก็บชื่อเลเยอร์และสี
  )
  ; รีเซ็ตตารางเลเยอร์เพื่อเริ่มจากเลเยอร์แรก
  (tblnext "LAYER" T)
  ; รวบรวมชื่อเลเยอร์และสี
  (while (setq layer_table (tblnext "LAYER"))
    (setq layer_name (cdr (assoc 2 layer_table)) ; ดึงชื่อเลเยอร์
          color_code (cdr (assoc 62 layer_table))) ; ดึงรหัสสี
    ; ตรวจสอบว่าเลเยอร์ถูกต้องและใช้งานได้
    (if (and layer_name
             (tblobjname "LAYER" layer_name)
             (not (wcmatch layer_name "*|*")) ; ข้ามเลเยอร์ Xref
             (not (minusp (cdr (assoc 70 layer_table)))) ; ข้ามเลเยอร์ที่ถูกล็อก/แช่แข็ง
        )
      (setq layer_list (append layer_list (list (list layer_name color_code))))
      (if layer_name
        (princ (strcat "\nSkipped invalid or locked layer: " layer_name))
      )
    )
  )
  ; ตรวจสอบว่ามีเลเยอร์หรือไม่
  (if layer_list
    (progn
      ; สร้างตารางด้วยเส้น (3 คอลัมน์)
      (setq i 0)
      ; วาดเส้นแนวนอน
      (repeat (+ (length layer_list) 2) ; รวมแถวหัวข้อและแถวสุดท้าย
        (command "_.LAYER" "_SET" "0" "")
        (setq pt1 (list (car current_pt) (- (cadr current_pt) (* i row_height)) 0.0)
              pt2 (list (+ (car current_pt) col_width_no col_width_layer col_width_color) (cadr pt1) 0.0))
        (command "_.LINE" "_non" pt1 "_non" pt2 "")
        (command "_.CHPROP" "_L" "" "_C" "ByLayer" "_LT" "ByLayer" "")
        (setq i (1+ i))
      )
      ; วาดเส้นแนวตั้ง (4 เส้นสำหรับ 3 คอลัมน์)
      (command "_.LAYER" "_SET" "0" "")
      (command "_.LINE" "_non" current_pt "_non" (list (car current_pt) (- (cadr current_pt) (* (1+ (length layer_list)) row_height)) 0.0) "")
      (command "_.CHPROP" "_L" "" "_C" "ByLayer" "_LT" "ByLayer" "")
      (command "_.LINE" "_non" (list (+ (car current_pt) col_width_no) (cadr current_pt) 0.0) "_non" (list (+ (car current_pt) col_width_no) (- (cadr current_pt) (* (1+ (length layer_list)) row_height)) 0.0) "")
      (command "_.CHPROP" "_L" "" "_C" "ByLayer" "_LT" "ByLayer" "")
      (command "_.LINE" "_non" (list (+ (car current_pt) col_width_no col_width_layer) (cadr current_pt) 0.0) "_non" (list (+ (car current_pt) col_width_no col_width_layer) (- (cadr current_pt) (* (1+ (length layer_list)) row_height)) 0.0) "")
      (command "_.CHPROP" "_L" "" "_C" "ByLayer" "_LT" "ByLayer" "")
      (command "_.LINE" "_non" (list (+ (car current_pt) col_width_no col_width_layer col_width_color) (cadr current_pt) 0.0) "_non" (list (+ (car current_pt) col_width_no col_width_layer col_width_color) (- (cadr current_pt) (* (1+ (length layer_list)) row_height)) 0.0) "")
      (command "_.CHPROP" "_L" "" "_C" "ByLayer" "_LT" "ByLayer" "")
      ; เติมหัวข้อ
      (command "_.LAYER" "_SET" "0" "")
      (command "_.TEXT" "_J" "MC" "_non" (list (+ (car current_pt) (/ col_width_no 2)) (- (cadr current_pt) (/ row_height 2)) 0.0) text_height 0 "No.")
      (command "_.CHPROP" "_L" "" "_C" "ByLayer" "_LT" "ByLayer" "")
      (command "_.TEXT" "_J" "MC" "_non" (list (+ (car current_pt) col_width_no (/ col_width_layer 2)) (- (cadr current_pt) (/ row_height 2)) 0.0) text_height 0 "Layer Name")
      (command "_.CHPROP" "_L" "" "_C" "ByLayer" "_LT" "ByLayer" "")
      (command "_.TEXT" "_J" "MC" "_non" (list (+ (car current_pt) col_width_no col_width_layer (/ col_width_color 2)) (- (cadr current_pt) (/ row_height 2)) 0.0) text_height 0 "Color")
      (command "_.CHPROP" "_L" "" "_C" "ByLayer" "_LT" "ByLayer" "")
      ; เติมลำดับ, ชื่อเลเยอร์, และรหัสสี
      (setq current_pt (list (car current_pt) (- (cadr current_pt) row_height) 0.0)
            n 1) ; ตัวนับลำดับ
      (foreach layer_data layer_list
        (setq layer_name (car layer_data)
              color_code (cadr layer_data))
        ; คอลัมน์ลำดับ
        (command "_.LAYER" "_SET" "0" "")
        (command "_.TEXT" "_J" "MC" "_non" (list (+ (car current_pt) (/ col_width_no 2)) (- (cadr current_pt) (/ row_height 2)) 0.0) text_height 0 (itoa n))
        (command "_.CHPROP" "_L" "" "_C" "ByLayer" "_LT" "ByLayer" "")
        ; คอลัมน์ชื่อเลเยอร์
        (if (tblobjname "LAYER" layer_name)
          (progn
            (command "_.LAYER" "_SET" layer_name "")
            (command "_.TEXT" "_J" "MC" "_non" (list (+ (car current_pt) col_width_no (/ col_width_layer 2)) (- (cadr current_pt) (/ row_height 2)) 0.0) text_height 0 layer_name)
            (command "_.CHPROP" "_L" "" "_C" "ByLayer" "_LT" "ByLayer" "")
            ; คอลัมน์สี
            (command "_.LAYER" "_SET" layer_name "")
            (command "_.TEXT" "_J" "MC" "_non" (list (+ (car current_pt) col_width_no col_width_layer (/ col_width_color 2)) (- (cadr current_pt) (/ row_height 2)) 0.0) text_height 0 (if color_code (itoa color_code) "ByBlock"))
            (command "_.CHPROP" "_L" "" "_C" "ByLayer" "_LT" "ByLayer" "")
          )
          (princ (strcat "\nError: Cannot set layer " layer_name))
        )
        ; อัปเดตตำแหน่งและลำดับ
        (setq current_pt (list (car current_pt) (- (cadr current_pt) row_height) 0.0)
              n (1+ n))
      )
      ; เพิ่มข้อความ "By PG THAI MOD" ใต้ตาราง
      (command "_.LAYER" "_SET" "0" "")
      (command "_.TEXT" "_J" "MC" "_non" (list (+ (car current_pt) (/ (+ col_width_no col_width_layer col_width_color) 2)) (- (cadr current_pt) (/ row_height 2)) 0.0) text_height 0 "By PG THAI MOD")
      (command "_.CHPROP" "_L" "" "_C" "ByLayer" "_LT" "ByLayer" "")
    )
    (princ "\nNo layers found in the drawing.")
  )
  (setvar "CMDECHO" old_cmd) ; คืนค่า CMDECHO
  (princ "\nDone creating table with layer names and colors.")
  (princ)
)
(princ "\nType CLT to run the command.")