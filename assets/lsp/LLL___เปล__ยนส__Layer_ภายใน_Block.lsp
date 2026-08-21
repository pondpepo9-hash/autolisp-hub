(vl-load-com)

(defun C:LLL (/ ss currlayer doc blocks i ent blkref blkname changed colorchoice allblocks blkrefs)
  (setvar "CMDECHO" 0)
  
  ; ดึง layer ปัจจุบัน
  (setq currlayer (getvar "CLAYER"))
  (if (not currlayer)
    (progn
      (princ "\nError: No current layer set!")
      (setvar "CMDECHO" 1)
      (exit)
    )
  )
  
  ; เข้าถึง document และ blocks collection
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blocks (vla-get-Blocks doc))
  
  ; ถามผู้ใช้ว่าต้องการเปลี่ยนสีเป็นอะไร
  (initget "ByLayer ByBlock")
  (setq colorchoice (getkword "\nSet color to [ByLayer/ByBlock]: "))
  (if (not colorchoice)
    (setq colorchoice "ByLayer") ; default เป็น ByLayer
  )
  
  ; ถามผู้ใช้ว่าต้องการเปลี่ยน block references ทั้งหมดที่มีชื่อเดียวกันหรือไม่
  (initget "Yes No")
  (setq allblocks (getkword "\nChange all block references with same name? [Yes/No]: "))
  (if (not allblocks)
    (setq allblocks "No") ; default เป็น No
  )
  
  ; เลือก objects
  (princ "\nSelect objects to change to current layer (including all nested contents): ")
  (setq ss (ssget))
  
  (if ss
    (progn
      (setq i 0)
      (setq changed 0) ; ตัวนับการเปลี่ยนแปลง
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (if (and ent (tblobjname "LAYER" currlayer))
          (progn
            ; เปลี่ยน layer และ color ของ entity หลัก
            (if (vl-catch-all-error-p
                  (vl-catch-all-apply 'vl-cmdf (list "_.CHPROP" ent "" "_LA" currlayer "_C" colorchoice "")))
              (princ (strcat "\nFailed to change entity: " (vl-princ-to-string ent)))
              (progn
                (setq changed (1+ changed))
                ; ถ้าเป็น block
                (if (= (cdr (assoc 0 (entget ent))) "INSERT")
                  (progn
                    (setq blkname (cdr (assoc 2 (entget ent))))
                    ; เปลี่ยนเนื้อหาภายใน block
                    (CHANGE-BLOCK-CONTENTS blkname currlayer colorchoice blocks)
                    ; ถ้าเลือก Yes ให้เปลี่ยน block references ทั้งหมดที่มีชื่อเดียวกัน
                    (if (equal allblocks "Yes")
                      (progn
                        (setq blkrefs (ssget "_X" (list '(0 . "INSERT") (cons 2 blkname))))
                        (if blkrefs
                          (progn
                            (command "_.CHPROP" blkrefs "" "_LA" currlayer "_C" colorchoice "")
                            (setq changed (1+ changed))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
          (princ (strcat "\nInvalid entity at index " (itoa i)))
        )
        (setq i (1+ i))
      )
      
      ; อัพเดท display
      (vl-cmdf "_.REGEN")
      (if (> changed 0)
        (princ (strcat "\nObjects changed to layer: " currlayer " and color: " colorchoice))
        (princ "\nNo objects were changed"))
    )
    (princ "\nNo objects selected")
  )
  
  (setvar "CMDECHO" 1)
  (princ)
)

; ฟังก์ชันสำหรับเปลี่ยนทุกอย่างใน block
(defun CHANGE-BLOCK-CONTENTS (blkname currlayer colorchoice blocks / blkobj)
  (vl-catch-all-apply
    '(lambda ()
       (setq blkobj (vla-Item blocks blkname))
       (if blkobj
         (vlax-for obj blkobj
           ; เปลี่ยน layer
           (if (vlax-property-available-p obj 'Layer)
             (if (vl-catch-all-error-p
                   (vl-catch-all-apply 'vla-put-Layer (list obj currlayer)))
               (princ (strcat "\nFailed to change layer of object in block " blkname ": " (vl-princ-to-string obj)))
             )
           )
           ; เปลี่ยน color
           (if (vlax-property-available-p obj 'Color)
             (if (vl-catch-all-error-p
                   (vl-catch-all-apply 'vla-put-Color (list obj (if (equal colorchoice "ByLayer") 256 0))))
               (princ (strcat "\nFailed to change color of object in block " blkname ": " (vl-princ-to-string obj)))
             )
           )
           ; ถ้าเจอ nested block
           (if (and (vlax-property-available-p obj 'ObjectName)
                    (= (vla-get-ObjectName obj) "AcDbBlockReference"))
             (progn
               (CHANGE-BLOCK-CONTENTS (vla-get-EffectiveName obj) currlayer colorchoice blocks) ; recursive call
             )
           )
         )
         (princ (strcat "\nBlock not found: " blkname))
       )
     )
  )
)