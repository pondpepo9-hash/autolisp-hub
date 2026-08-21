(defun remove-mtext-color-codes (str / result pos start-C start-c start end)
  (setq result "" pos 0)
  (while (or (setq start-C (vl-string-search "\\C" str pos))
             (setq start-c (vl-string-search "\\c" str pos)))
    (setq start (if (and start-C start-c)
                  (min start-C start-c)
                  (if start-C start-C start-c)))
    (setq result (strcat result (substr str (+ 1 pos) (- start pos))))
    (setq end (vl-string-search ";" str start))
    (if end
      (setq pos (+ 1 end))
      (setq pos (+ 2 start))
    )
  )
  (strcat result (substr str (+ 1 pos)))
)

(defun add-mtext-color-code (str color-result / c420 c62 r g b bgr)
  ;; แปลงเลขสีเป็น string format สำหรับ MTEXT
  ;; \C สำหรับ Index Color (1-255)
  ;; \c สำหรับ True Color (BGR format)
  (cond
    ((setq c420 (cdr (assoc 420 color-result)))
     (setq b (rem c420 256)
           g (rem (/ c420 256) 256)
           r (fix (/ c420 65536)))
     (setq bgr (+ (* b 65536) (* g 256) r))
     (strcat "\\c" (itoa bgr) ";" str)
    )
    ((setq c62 (cdr (assoc 62 color-result)))
     (if (or (= c62 256) (= c62 0))
       str
       (strcat "\\C" (itoa c62) ";" str)
     )
    )
    (t str)
  )
)

(defun remove-dxf304-color (ent / dxf data mtext new304)
  (setq dxf (entget ent))
  (if (setq data (assoc 304 dxf))
    (progn
      (setq mtext (cdr data))
      (setq mtext (remove-mtext-color-codes mtext))
      (setq new304 (cons 304 mtext))
      (entmod (subst new304 data dxf))
      (entupd ent)
    )
  )
)

(defun set-dxf304-color (ent color-result / dxf data mtext cleaned new304)
  ;; ฟังก์ชันใหม่: ลบ color code เก่าแล้วใส่ color code ใหม่
  (setq dxf (entget ent))
  (if (setq data (assoc 304 dxf))
    (progn
      (setq mtext (cdr data))
      ;; ลบ color code เก่าออกก่อน
      (setq cleaned (remove-mtext-color-codes mtext))
      ;; ใส่ color code ใหม่เข้าไป
      (setq cleaned (add-mtext-color-code cleaned color-result))
      (setq new304 (cons 304 cleaned))
      (entmod (subst new304 data dxf))
      (entupd ent)
    )
  )
)

(defun Change-Object-Color (Obj color-result / entname txtstr cleaned mlcontent ename acCol cb split-pos book-name color-name c420 r g b c62 blk-name blk-eff bcol blk-def blk-def-eff)
  (setq entname (vla-get-objectname Obj))
  (setq ename (vlax-vla-object->ename Obj))
  (setq c62 (cdr (assoc 62 color-result)))
  
  (setq acCol nil)
  (if (vlax-property-available-p Obj 'TrueColor)
    (setq acCol (vla-get-TrueColor Obj))
  )

  (if acCol
    (progn
      (cond
        ((assoc 430 color-result) ; Color Book
         (setq cb (cdr (assoc 430 color-result)))
         (setq split-pos (vl-string-search "$" cb))
         (if split-pos
           (progn
             (setq book-name (substr cb 1 split-pos))
             (setq color-name (substr cb (+ 2 split-pos)))
             (vl-catch-all-apply 'vla-SetColorBookColor (list acCol book-name color-name))
           )
         )
         ;; Fallback RGB
         (if (setq c420 (cdr (assoc 420 color-result)))
           (progn
             (setq b (rem c420 256)
                   g (rem (/ c420 256) 256)
                   r (fix (/ c420 65536)))
             (vl-catch-all-apply 'vla-SetRGB (list acCol r g b))
           )
         )
        )
        ((assoc 420 color-result) ; True Color
         (setq c420 (cdr (assoc 420 color-result)))
         (setq b (rem c420 256)
               g (rem (/ c420 256) 256)
               r (fix (/ c420 65536)))
         (vl-catch-all-apply 'vla-SetRGB (list acCol r g b))
        )
        (t ; Index Color
         (vl-catch-all-apply 'vla-put-ColorIndex (list acCol c62))
        )
      )
      
      (if (vlax-write-enabled-p Obj)
        (vl-catch-all-apply 'vla-put-TrueColor (list Obj acCol))
      )
    )
    ;; Fallback if TrueColor property is not available
    (if (and (vlax-write-enabled-p Obj) (vlax-property-available-p Obj 'Color))
      (vl-catch-all-apply 'vla-put-Color (list Obj c62))
    )
  )

  (if (= entname "AcDbMText")
    (progn
      (setq txtstr (vla-get-TextString Obj))
      (setq cleaned (remove-mtext-color-codes txtstr))
      (vl-catch-all-apply 'vla-put-TextString (list Obj cleaned))
    )
  )

  (if (= entname "AcDbMLeader")
    (progn
      (if (vlax-method-applicable-p Obj 'GetLeaderLineCount)
        (progn
          (if (vlax-property-available-p Obj 'TextString)
            (progn
              (setq mlcontent (vla-get-TextString Obj))
              (setq cleaned (remove-mtext-color-codes mlcontent))
              (vl-catch-all-apply 'vla-put-TextString (list Obj cleaned))
            )
          )
        )
      )
      (if (vlax-property-available-p Obj 'TextColor)
        (vl-catch-all-apply 'vla-put-TextColor (list Obj c62)))
      
      ;; **ส่วนที่แก้ไข**: ใช้ฟังก์ชันใหม่ที่ใส่ color code เข้าไปด้วย
      (set-dxf304-color ename color-result)
    )
  )

  (if (and (vlax-property-available-p Obj 'TextString)
           (wcmatch entname "AcDbText"))
    (if acCol
      (vl-catch-all-apply 'vla-put-TrueColor (list Obj acCol))
      (vl-catch-all-apply 'vla-put-Color (list Obj c62))
    )
  )

  (if (= entname "AcDbBlockReference")
    (progn
      (if (= (vla-get-HasAttributes Obj) :vlax-true)
        (foreach att (vlax-safearray->list (vlax-variant-value (vla-GetAttributes Obj)))
          (if (vlax-write-enabled-p att)
            (if (vlax-property-available-p att 'TrueColor)
              (vl-catch-all-apply 'vla-put-TrueColor (list att acCol))
              (if (vlax-property-available-p att 'Color)
                (vl-catch-all-apply 'vla-put-Color (list att c62))
              )
            )
          )
        )
      )
      
      ;; 2. Penetrate Block Definition
      (if (vlax-property-available-p Obj 'Name)
        (progn
          (setq blk-name (vla-get-Name Obj))
          (if (vlax-property-available-p Obj 'EffectiveName)
            (setq blk-eff (vla-get-EffectiveName Obj))
            (setq blk-eff blk-name)
          )
          
          ;; Use a dynamically scoped list *cr-processed-blocks* to avoid infinite recursion
          (if (not (vl-position blk-name *cr-processed-blocks*))
            (progn
              (setq *cr-processed-blocks* (cons blk-name *cr-processed-blocks*))
              (if (not (vl-position blk-eff *cr-processed-blocks*))
                (setq *cr-processed-blocks* (cons blk-eff *cr-processed-blocks*))
              )
              
              ;; Process block definitions
              (setq bcol (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-Acad-Object))))
              
              (if (not (vl-catch-all-error-p (setq blk-def (vl-catch-all-apply 'vla-Item (list bcol blk-name)))))
                (vlax-for inner-obj blk-def
                  (Change-Object-Color inner-obj color-result)
                )
              )
              
              (if (/= blk-name blk-eff)
                (if (not (vl-catch-all-error-p (setq blk-def-eff (vl-catch-all-apply 'vla-Item (list bcol blk-eff)))))
                  (vlax-for inner-obj blk-def-eff
                    (Change-Object-Color inner-obj color-result)
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  (if (wcmatch entname "*Dimension*")
    (progn
      (vl-catch-all-apply 'vla-put-ExtensionLineColor (list Obj c62))
      (vl-catch-all-apply 'vla-put-DimensionLineColor (list Obj c62))
    )
  )
  (vl-catch-all-apply 'vla-Update (list Obj))
)

(defun c:CR (/ adoc color-result ss obj i processed skipped *cr-processed-blocks*)
  (vl-load-com)
  (defun *error* (msg)
    (princ (strcat "\nError: " msg))
    (princ)
  )

  (setq adoc (vla-get-ActiveDocument (vlax-get-Acad-Object)))

  ;; เปลี่ยนไปใช้ acad_truecolordlg เพื่อให้รองรับ True Color และ Color Books
  (if (and (setq color-result (acad_truecolordlg '(62 . 256) t))
           (setq ss (ssget ":L")))
    (progn
      (setq i 0 processed 0 skipped 0)
      (vla-StartUndoMark adoc)
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (if (and obj (vlax-write-enabled-p obj))
          (progn
            (Change-Object-Color obj color-result)
            (setq processed (1+ processed))
          )
          (setq skipped (1+ skipped))
        )
        (setq i (1+ i))
      )
      (vla-EndUndoMark adoc)
      (vl-cmdf "_.REGEN")
      (princ (strcat "\nProcessed: " (itoa processed) ", Skipped: " (itoa skipped)))
    )
    (princ "\nNo color selected or no objects selected."))
  (princ)
)
