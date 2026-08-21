(vl-load-com)

(defun c:CLCN () (c:ChangeLayerColorNested))
(defun c:ChangeLayerColorNested (/ *error* entity layer layers color transparency acDoc oLayers)
  (princ "\rCHANGELAYERCOLORNESTED ")

  ;; ฟังก์ชันจัดการข้อผิดพลาด
  (defun *error* (msg)
    (if acDoc
      (progn
        (vla-endundomark acDoc)
        (vla-regen acDoc acAllViewports)
      )
    )
    (cond ((not msg))                                                   ; Normal exit
          ((member msg '("Function cancelled" "quit / exit abort")))    ; <esc> or (quit)
          ((princ (strcat " ** Error: " msg " ** ")))
    )                                                                   ; Fatal error, display it
    (princ)
  )

  ;; ฟังก์ชันตั้งค่า Transparency จากโค้ด cnlip
  (defun set_layer_transparency (lay trn / ent)
    (defun trans->dxf (x)
      (logior (fix (* 2.55 (- 100 x))) 33554432)
    )
    (if (setq ent (tblobjname "layer" lay))
      (progn
        (regapp "accmtransparency")
        (entmod (append (entget ent) (list
          (list -3
            (list "accmtransparency"
              (cons 1071 (trans->dxf trn))
            )
          )
        )))
      )
    )
  )

  ;; เก็บเลเยอร์จากออบเจกต์ซ้อน
  (while
    (/= nil
        (setq entity
               (nentsel
                 "\nSelect nested entity to change layer color and transparency: "
               )
        )
    )
     (if
       (not
         (vl-position
           (setq layer
                  (if
                    (= "0" (setq layer (cdr (assoc 8 (entget (car entity))))))
                     (cdr (assoc 8 (entget (car (last entity)))))
                     layer
                  )
           )
           layers
         )
       )
        (print (reverse (setq layers (cons layer layers))))
     )
  )

  ;; เลือกสีและ Transparency
  (if
    (and
      layers
      (setq color
             (if (= 1 (length layers))
               (cdr (assoc 62 (tblsearch "layer" (car layers))))
               1
             )
      )
      (princ "\nSelect replacement layer color: ")
      (setq color (acad_colordlg color nil))
      (progn
        (initget 1 "0 10 20 30 40 50 60 70 80 90")
        (setq transparency
               (cond
                 ((getint "\nEnter layer transparency [0-90]: "))
                 (0)
               )
        )
        (<= 0 transparency 90)
      )
    )
     (progn
       (vla-startundomark
         (setq acDoc (vla-get-activedocument (vlax-get-acad-object)))
       )
       (setq oLayers (vla-get-layers acDoc))
       (foreach layer layers
         (vla-put-color (vla-item oLayers layer) color) ; เปลี่ยนสี
         (set_layer_transparency layer transparency)    ; เปลี่ยน Transparency
       )
     )
  )
  (*error* nil)
  (princ)
)