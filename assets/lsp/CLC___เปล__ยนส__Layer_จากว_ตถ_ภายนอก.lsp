(defun c:CLC (/ ent obj layer_name current_color new_color layer_obj block_color)
  ;; CLC = Change Layer Color
  (princ "\nChange Layer Color by selecting object...")
  
  ;; ให้ผู้ใช้เลือก object
  (if (setq ent (car (entsel "\nSelect object to change its layer color: ")))
    (progn
      ;; ดึงข้อมูล object
      (setq obj (entget ent))
      
      ;; ดึงชื่อ layer
      (setq layer_name (cdr (assoc 8 obj)))
      
      ;; ดึงข้อมูล layer object
      (setq layer_obj (tblsearch "LAYER" layer_name))
      
      ;; ดึงสีปัจจุบันของ layer
      (setq current_color (cdr (assoc 62 layer_obj)))
      
      ;; ถ้าไม่มี color code แสดงว่าเป็นสี 7 (white/black)
      (if (null current_color)
        (setq current_color 7)
      )
      
      (princ (strcat "\nCurrent layer: " layer_name))
      (princ (strcat "\nCurrent color: " (itoa current_color)))
      
      ;; ตรวจสอบว่าเป็น Block หรือไม่
      (if (= (cdr (assoc 0 obj)) "INSERT")
        (progn
          (setq block_color (cdr (assoc 62 obj)))
          (princ "\nSelected object is a BLOCK.")
          (if block_color
            (princ (strcat "\nBlock has explicit color: " (itoa block_color)))
            (princ "\nBlock color: ByLayer")
          )
        )
      )
      
      ;; ให้ผู้ใช้เลือกสีใหม่
      (setq new_color (getint (strcat "\nEnter new color number (1-255) [Current: " (itoa current_color) "]: ")))
      (if (null new_color) (setq new_color current_color)) ; ใช้สีเดิมถ้ากด Enter
      
      (if (and (>= new_color 1) (<= new_color 255))
          (progn
            ;; เปลี่ยนสีของ layer (ใช้วิธี entmod แทน command layer)
            (change-layer-color layer_name new_color)
            
            ;; ถ้าเป็น Block และมีสีเฉพาะ ให้เปลี่ยนเป็น ByLayer
            (if (and (= (cdr (assoc 0 obj)) "INSERT") (cdr (assoc 62 obj)))
              (progn
                (princ "\nBlock has explicit color. Changing to ByLayer...")
                (command "._CHPROP" ent "" "_COLOR" "_BYLAYER" "")
              )
            )
            
            ;; ตรวจสอบและเปลี่ยน entities ภายใน Block ที่มีสีเฉพาะให้เป็น ByBlock
            (if (= (cdr (assoc 0 obj)) "INSERT")
              (fix-block-entities-color ent)
            )
            
            (princ (strcat "\nLayer '" layer_name "' color changed from " (itoa current_color) " to " (itoa new_color)))
            
            ;; Regen เพื่อให้เห็นการเปลี่ยนแปลง
            (command "._REGEN")
          )
          (princ "\nInvalid color number. Please enter 1-255.")
        )
    )
    (princ "\nNo object selected.")
  )
  (princ)
)

;; ฟังก์ชันเปลี่ยนสี layer โดยใช้ entmod
(defun change-layer-color (layer_name color_num / layer_ename layer_obj new_obj)
  (setq layer_ename (tblobjname "LAYER" layer_name))
  (if layer_ename
    (progn
      (setq layer_obj (entget layer_ename))
      ;; ลบ color code เดิมถ้ามี
      (if (assoc 62 layer_obj)
        (setq layer_obj (vl-remove (assoc 62 layer_obj) layer_obj))
      )
      ;; เพิ่ม color code ใหม่
      (setq new_obj (append layer_obj (list (cons 62 color_num))))
      ;; อัพเดต layer
      (entmod new_obj)
      T ; return success
    )
    (progn
      (princ (strcat "\nError: Cannot find layer '" layer_name "'"))
      nil ; return failure
    )
  )
)
(defun fix-block-entities-color (block_ent / block_def block_list sub_ent sub_obj)
  (princ "\nChecking block entities...")
  
  ;; ดึงข้อมูล Block definition
  (setq block_obj (entget block_ent))
  (setq block_def (cdr (assoc 2 block_obj))) ; Block name
  
  ;; หา Block definition ใน table
  (setq block_list (tblsearch "BLOCK" block_def))
  
  (if block_list
    (progn
      ;; วนลูปผ่าน entities ใน Block definition
      (setq sub_ent (tblobjname "BLOCK" block_def))
      (if sub_ent (setq sub_ent (entnext sub_ent))) ; ข้าม Block record header
      
      (while sub_ent
        (setq sub_obj (entget sub_ent))
        
        ;; ตรวจสอบว่ามีสีเฉพาะหรือไม่ (ไม่ใช่ ByBlock หรือ ByLayer)
        (if (and (cdr (assoc 62 sub_obj))
                 (not (= (cdr (assoc 62 sub_obj)) 0))    ; ไม่ใช่ ByBlock
                 (not (= (cdr (assoc 62 sub_obj)) 256))) ; ไม่ใช่ ByLayer
          (progn
            (princ (strcat "\nFound entity with explicit color: " (itoa (cdr (assoc 62 sub_obj)))))
            ;; เปลี่ยนเป็น ByBlock (color = 0)
            (entmod (subst (cons 62 0) (assoc 62 sub_obj) sub_obj))
          )
        )
        
        (setq sub_ent (entnext sub_ent))
      )
      (princ "\nBlock entities color check completed.")
    )
    (princ "\nCannot access block definition.")
  )
)

;; ฟังก์ชันเสริม: แก้ไข Block ที่เลือกให้แสดงสีตาม Layer
(defun c:FIXBLOCKCOLOR (/ ss i ent obj)
  (princ "\nFix Block Color to follow Layer color...")
  
  (if (setq ss (ssget '((0 . "INSERT"))))
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj (entget ent))
        
        ;; เปลี่ยน Block เป็น ByLayer ถ้ามีสีเฉพาะ
        (if (cdr (assoc 62 obj))
          (progn
            (princ (strcat "\nFixing block on layer: " (cdr (assoc 8 obj))))
            (command "._CHPROP" ent "" "_COLOR" "_BYLAYER" "")
            (fix-block-entities-color ent)
          )
        )
        
        (setq i (1+ i))
      )
      (command "._REGEN")
      (princ (strcat "\nFixed " (itoa (sslength ss)) " blocks."))
    )
    (princ "\nNo blocks selected.")
  )
  (princ)
)
