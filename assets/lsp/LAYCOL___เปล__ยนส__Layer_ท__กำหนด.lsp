(defun C:LAYCOL (/ lay ss col sblm ent laydata i allinlayer)
  (command "_.undo" "_mark") ; บันทึก undo point
  
  ; เลือก objects
  (princ "\nSelect objects to process: ")
  (setq ss (ssget)) ; ผู้ใช้เลือก objects
  
  (if ss
    (progn
      ; วนลูปผ่าน selection set เพื่อหา layer และสี
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq lay (cdr (assoc 8 (entget ent)))) ; ดึงชื่อ layer จาก entity
        (setq col (cdr (assoc 62 (entget ent)))) ; ดึงสีของ entity
        (if (and col (/= col 0) (/= col 256)) ; ถ้าสีไม่ใช่ ByLayer (256) หรือ ByBlock (0)
          (progn
            (setq laydata (entget (tblobjname "LAYER" lay)))
            (if laydata
              (progn
                (setq laydata (subst (cons 62 col) (assoc 62 laydata) laydata)) ; เปลี่ยนสีของ layer
                (entmod laydata) ; อัพเดท layer
              )
            )
          )
        )
        (setq i (1+ i))
      )
      
      ; ถามผู้ใช้ว่าจะเปลี่ยนทุกอย่างใน layer ที่เลือกเป็น ByLayer หรือไม่
      (initget "Yes No")
      (setq allinlayer (getkword "\nChange all objects in selected layers to ByLayer? [Yes/No]: "))
      (if (not allinlayer)
        (setq allinlayer "No") ; default เป็น No
      )
      
      ; ตั้งค่า ByLayer
      (setq sblm (getvar 'setbylayermode))
      (setvar 'setbylayermode 1)
      (if (equal allinlayer "Yes")
        (progn
          ; สร้าง selection set ใหม่สำหรับทุก objects ใน layers ที่เลือก
          (setq i 0)
          (setq laylist nil)
          (while (< i (sslength ss))
            (setq lay (cdr (assoc 8 (entget (ssname ss i)))))
            (if (not (member lay laylist))
              (setq laylist (cons lay laylist))
            )
            (setq i (1+ i))
          )
          (setq fullss (ssget "_X" (append '((0 . "~HATCH")) (mapcar '(lambda (x) (cons 8 x)) laylist))))
          (if fullss
            (command "_.SETBYLAYER" fullss "" "" "no")
            (princ "\nNo additional objects found in selected layers")
          )
        )
        ; เปลี่ยนเฉพาะ objects ที่เลือก
        (command "_.SETBYLAYER" ss "" "" "no")
      )
      (setvar 'setbylayermode sblm)
    )
    (princ "\nNo objects selected")
  )
  
  (princ)
)