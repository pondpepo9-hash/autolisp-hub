;;; ========================================================
;;; Auto Numbering Sequence (NUMSEQ.lsp)
;;; ========================================================
(defun c:NUMSEQ (/ pt num prefix inc txtStr)
  (setq prefix (getstring t "\nใส่คำนำหน้า (Prefix) [Enter เพื่อข้าม]: "))
  (setq num (getint "\nใส่หมายเลขเริ่มต้น <1>: "))
  (if (null num) (setq num 1))
  (setq inc (getint "\nระยะก้าว (Increment) <1>: "))
  (if (null inc) (setq inc 1))
  
  (while (setq pt (getpoint "\nคลิกตำแหน่งที่ต้องการวางตัวเลข (กด Esc เพื่อจบ): "))
    (setq txtStr (strcat prefix (itoa num)))
    (command "._TEXT" "_MC" pt "" "0" txtStr)
    (setq num (+ num inc))
  )
  (princ "\nเสร็จสิ้นการรันตัวเลข.")
  (princ)
)
