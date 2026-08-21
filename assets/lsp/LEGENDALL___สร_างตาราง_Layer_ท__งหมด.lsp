(defun C:LEGENDALL ( / *error* acdoc acobj hs ht st ro dr laytbl la co lt lst p x y lineLen textOffset)
  (vl-load-com)
  (setq acObj (vlax-get-acad-object)
        acDoc (vla-get-activedocument acObj)
  )

  (vla-startundomark acDoc)

  (defun *error* (msg)
    (if msg (princ (strcat "\nError: " msg)))
    (vla-endundomark acDoc)
    (princ)
  )

  ;; 1. คำนวณค่าพื้นฐาน
  (setq st (entget (tblobjname "style" (getvar 'textstyle)) '("AcadAnnotative"))
        hs (cdr (assoc 40 st))
        ro 0
        dr (list 0 0 1)
        ht (if (> hs 0) hs 2.5) ; fallback height
        lineLen 15.0
        textOffset 2.0
  )

  ;; 2. อ่านเลเยอร์ทั้งหมดจากตาราง
  (setq laytbl (tblnext "LAYER" T))
  (setq lst '())
  (while laytbl
    (setq la (cdr (assoc 2 laytbl)))
    (setq co (cdr (assoc 62 laytbl)))
    (setq lt (cdr (assoc 6 laytbl)))
    (if (not lt) (setq lt "ByLayer"))
    (setq lst (cons (list la lt co) lst))
    (setq laytbl (tblnext "LAYER"))
  )

  ;; 3. เริ่มวาด
  (if (setq p (getpoint "\nSpecify insert point: "))
    (progn
      (setq lst (vl-sort lst '(lambda (a b) (< (car a) (car b)))))
      (setq x (car p))
      (setq y (cadr p))
      (foreach xData lst
        (setq la (nth 0 xData)
              lt (nth 1 xData)
              co (abs (nth 2 xData))
        )
        (setq p1 (list x y 0.0))
        (setq p2 (list (+ x lineLen) y 0.0))
        ;; วาดเส้นตัวอย่าง
        (entmake
          (list
            '(0 . "LINE")
            (cons 8 la)
            (cons 10 p1)
            (cons 11 p2)
            (cons 6 lt)
            (cons 62 co)
          )
        )
        ;; วาดชื่อเลเยอร์ (TEXT)
        (entmake
          (list
            '(0 . "TEXT")
            (cons 8 la)
            (cons 10 (list (+ x lineLen textOffset) y 0.0))
            (cons 40 ht)
            (cons 1 la)
            (cons 62 co)
            (cons 7 (getvar 'textstyle))
            (cons 50 ro)
            (cons 210 dr)
          )
        )
        (setq y (- y (* 1.8 ht))) ; ระยะระหว่างแถว
      )
    )
  )

  (*error* nil)
  (princ "\n✅ LEGEND created for all layers.")
  (princ)
)
