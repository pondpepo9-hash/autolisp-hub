;;; LCC - Layer Control Center, consolidated single-file edition
;;; AutoCAD commands: LCC, 1 (Lock Layer), 3 (Unlock Layer)
;;; Standalone copy of LCC with a single-window command launcher.

(vl-load-com)

;; Remove legacy LCC commands that may still be resident in the current
;; AutoCAD session. LCC and its numeric Lock/Unlock shortcuts are
;; defined again by this file.
(foreach lccui:legacy-command
  '("C:LCC" "C:LC" "C:FE" "C:FZ" "C:FR" "C:FFE"
    "C:FF" "C:FFEE" "C:FXS" "C:," "C:/" "C:1" "C:3")
  (vl-catch-all-apply
    'vl-acad-undefun
    (list (read lccui:legacy-command))
  )
)
(setq lccui:legacy-command nil)

(defun lccui:doc ()
  (vla-get-ActiveDocument (vlax-get-Acad-Object))
)

(defun lccui:add-unique (value values)
  (if (member (strcase value) (mapcar 'strcase values))
    values
    (cons value values)
  )
)

(defun lccui:layer-object (doc name / result)
  (setq result
    (vl-catch-all-apply
      'vla-Item
      (list (vla-get-Layers doc) name)
    )
  )
  (if (vl-catch-all-error-p result) nil result)
)

(defun lccui:set-layer-on (doc name state / layer result)
  (if (setq layer (lccui:layer-object doc name))
    (progn
      (setq result
        (vl-catch-all-apply
          'vla-put-LayerOn
          (list layer state)
        )
      )
      (not (vl-catch-all-error-p result))
    )
    nil
  )
)

(defun lccui:freeze-layer (doc name / layer result)
  (if (setq layer (lccui:layer-object doc name))
    (progn
      (setq result
        (vl-catch-all-apply
          'vla-put-Freeze
          (list layer :vlax-true)
        )
      )
      (not (vl-catch-all-error-p result))
    )
    nil
  )
)

(defun lccui:xref-block-p (block-name / record flags)
  (and
    (setq record (tblobjname "BLOCK" block-name))
    (setq flags (cdr (assoc 70 (entget record))))
    (/= 0 (logand flags 12))
  )
)

(defun lccui:xrefs-on-layer (layer-name / ss index data block-name names)
  (if (setq ss (ssget "_X" '((0 . "INSERT"))))
    (progn
      (setq index 0)
      (repeat (sslength ss)
        (setq data (entget (ssname ss index))
              index (1+ index))
        (if
          (and
            (= (strcase (cdr (assoc 8 data))) (strcase layer-name))
            (setq block-name (cdr (assoc 2 data)))
            (lccui:xref-block-p block-name)
          )
          (setq names (lccui:add-unique block-name names))
        )
      )
    )
  )
  names
)

(defun lccui:xref-dependent-layer-p (layer-name xref-name / prefix length)
  (setq prefix (strcat xref-name "|")
        length (strlen prefix))
  (and
    (>= (strlen layer-name) length)
    (= (strcase (substr layer-name 1 length)) (strcase prefix))
  )
)

(defun lccui:set-xref-layers (doc xref-name state / layer layer-name count)
  (setq count 0)
  (vlax-for layer (vla-get-Layers doc)
    (setq layer-name (vla-get-Name layer))
    (if (lccui:xref-dependent-layer-p layer-name xref-name)
      (if
        (not
          (vl-catch-all-error-p
            (vl-catch-all-apply 'vla-put-LayerOn (list layer state))
          )
        )
        (setq count (1+ count))
      )
    )
  )
  count
)

(defun lccui:set-master-layer
       (doc layer-name state / xref-name xrefs count)
  ;; Same working mechanism as FE in the original LCC:
  ;; set the host layer, then cascade into every Xref placed on it.
  (setq xrefs (lccui:xrefs-on-layer layer-name))
  (lccui:set-layer-on doc layer-name state)
  (setq count 0)
  (foreach xref-name xrefs
    (setq count
      (+ count
         (lccui:set-xref-layers doc xref-name state)))
  )
  count
)

(defun lccui:selected-layers (/ ss index entity layer-name layers)
  (prompt "\nเลือกวัตถุหรือเลเยอร์: ")
  (if (setq ss (ssget))
    (progn
      (setq index 0)
      (repeat (sslength ss)
        (setq entity (ssname ss index)
              layer-name
               (cdr (assoc 8 (entget entity)))
              index (1+ index))
        (if layer-name
          (setq layers (lccui:add-unique layer-name layers))
        )
      )
      ;; SSGET keeps its selection highlight alive until the selection set
      ;; is released. Explicitly unhighlight every entity before returning.
      (setq index 0)
      (repeat (sslength ss)
        (redraw (ssname ss index) 4)
        (setq index (1+ index))
      )
      (sssetfirst nil nil)
      (setq ss nil)
      (redraw)
    )
  )
  (reverse layers)
)

(defun lccui:native-lock-loop
       (lock-command action-name / selection done error-code)
  ;; Use AutoCAD's native LAYLCK/LAYULK for correct display behavior,
  ;; but repeat one native operation per click until Enter or Esc.
  (setq done nil)
  (while (not done)
    (setvar "ERRNO" 0)
    (setq selection
      (entsel
        (strcat
          "\nคลิกวัตถุบน Layer ที่ต้องการ"
          action-name
          " <Enter เพื่อจบ>: "
        )
      )
          error-code (getvar "ERRNO"))
    (cond
      (selection
       (command lock-command (car selection) ""))
      ((= error-code 52)
       (setq done T))
      (T
       (princ "\nคลิกไม่โดนวัตถุ กรุณาคลิกใหม่ หรือกด Enter เพื่อจบ"))
    )
  )
  (princ)
)

(defun lccui:layer-from-ssnamex
       (record / enames data layer containers container-layer)
  ;; First ENAME is the picked object; following ENAME values are
  ;; its Block/Xref containers.
  (setq enames
    (vl-remove-if-not
      '(lambda (item) (= (type item) 'ENAME))
      record
    )
  )
  (if enames
    (progn
      (setq data  (entget (car enames))
            layer (cdr (assoc 8 data)))
      (if (= layer "0")
        (progn
          (setq containers (cdr enames)
                container-layer nil)
          (while
            (and
              containers
              (or
                (null
                  (setq container-layer
                    (cdr (assoc 8 (entget (car containers))))))
                (= container-layer "0")
              )
            )
            (setq containers (cdr containers))
          )
          (if container-layer
            (setq layer container-layer)
          )
        )
      )
    )
  )
  layer
)

(defun lccui:layers-at-pickbox (selection / index info layer layers)
  (setq index 0
        layers nil)
  (repeat (sslength selection)
    (setq info (ssnamex selection index))
    (if
      (and info
           (car info)
           (setq layer
             (lccui:layer-from-ssnamex (car info))))
      (setq layers (lccui:add-unique layer layers))
    )
    (setq index (1+ index))
  )
  (acad_strlsort layers)
)

(defun lccui:select-overlap-layers
       (layers / path file dcl-id result indices line)
  (setq path (vl-filename-mktemp "LCC-Layers-" nil ".dcl")
        file (open path "w"))
  (if file
    (progn
      (foreach line
        '(
          "layers : dialog {"
          "  label = \"พบเส้นซ้อนกัน - เลือก Layer\";"
          "  : list_box {"
          "    key = \"layers\";"
          "    multiple_select = true;"
          "    width = 60;"
          "    height = 18;"
          "  }"
          "  ok_cancel;"
          "}"
         )
        (write-line line file)
      )
      (close file)
      (setq dcl-id (load_dialog path))
      (if (and (> dcl-id 0) (new_dialog "layers" dcl-id))
        (progn
          (start_list "layers")
          (foreach line layers (add_list line))
          (end_list)
          (set_tile "layers" "0")
          (setq indices "0")
          (action_tile "layers" "(setq indices $value)")
          (action_tile "accept" "(done_dialog 1)")
          (action_tile "cancel" "(done_dialog 0)")
          (setq result (start_dialog))
        )
        (setq result 0)
      )
      (if (and dcl-id (> dcl-id 0))
        (unload_dialog dcl-id)
      )
      (if (findfile path)
        (vl-file-delete path)
      )
      (if (= result 1)
        (mapcar
          '(lambda (index) (nth index layers))
          (read (strcat "(" indices ")"))
        )
        nil
      )
    )
    nil
  )
)

(defun lccui:pick-layer-result
       (/ old-cycling caught selection error-code layers chosen)
  ;; Return (OK layer...) / (MISS) / (SKIP) / (DONE).
  (setq old-cycling (getvar "SELECTIONCYCLING"))
  (setvar "SELECTIONCYCLING" 0)
  (setvar "ERRNO" 0)
  (setq caught
    (vl-catch-all-apply
      'ssget
      (list "_:E:S:N")
    )
        error-code (getvar "ERRNO"))
  (setvar "SELECTIONCYCLING" old-cycling)
  (cond
    ((vl-catch-all-error-p caught)
     '(DONE))
    ((and caught (= (type caught) 'PICKSET))
     (setq selection caught
           layers (lccui:layers-at-pickbox selection))
     (cond
       ((null layers) '(MISS))
       ((= (length layers) 1) (cons 'OK layers))
       ((setq chosen (lccui:select-overlap-layers layers))
        (cons 'OK chosen))
       (T '(SKIP))))
    ((= error-code 52) '(DONE))
    (T '(MISS))
  )
)

(defun lccui:freeze-picked-layer
       (/ doc pick layers layer current-layer hidden done)
  ;; Long-term Freeze: never changes ON/OFF state.
  (setq doc           (lccui:doc)
        current-layer (getvar "CLAYER")
        hidden        0
        done          nil)
  (while (not done)
    (prompt
      "\nคลิกวัตถุบน Layer ที่ต้องการ Freeze ระยะยาว <Enter เพื่อจบ>: ")
    (setq pick (lccui:pick-layer-result))
    (cond
      ((eq (car pick) 'OK)
       (setq layers (cdr pick))
       (foreach layer layers
         (cond
           ((= (strcase layer) (strcase current-layer))
            (princ
              (strcat
                "\nไม่สามารถ Freeze Layer ปัจจุบันได้: "
                layer
              )
            ))
           ((lccui:freeze-layer doc layer)
            (setq hidden (1+ hidden))
            (princ (strcat "\nFreeze Layer: " layer " เรียบร้อยแล้ว")))
           (T
            (princ (strcat "\nไม่สามารถ Freeze Layer: " layer)))
         )
       )
       (vla-Regen doc 1))
      ((eq (car pick) 'MISS)
       (princ "\nคลิกไม่โดนวัตถุ กรุณาคลิกใหม่ หรือกด Enter เพื่อจบ"))
      ((eq (car pick) 'SKIP)
       (princ "\nไม่ได้เลือก Layer กรุณาคลิกใหม่"))
      (T
       (setq done T))
    )
  )
  (princ
    (strcat
      "\nจบการทำงาน Freeze แล้ว "
      (itoa hidden)
      " Layer"
    )
  )
  (princ)
)

(defun lccui:hide-picked-layer
       (/ doc pick layers layer hidden done)
  ;; Temporary OFF: never changes Freeze/Thaw state.
  (setq doc    (lccui:doc)
        hidden 0
        done   nil)
  (while (not done)
    (prompt
      "\nคลิกวัตถุบน Layer ที่ต้องการซ่อนชั่วคราว <Enter เพื่อจบ>: ")
    (setq pick (lccui:pick-layer-result))
    (cond
      ((eq (car pick) 'OK)
       (setq layers (cdr pick))
       (foreach layer layers
         (if (lccui:set-layer-on doc layer :vlax-false)
           (progn
             (setq hidden (1+ hidden))
             (princ (strcat "\nOFF Layer: " layer " เรียบร้อยแล้ว"))
           )
           (princ (strcat "\nไม่สามารถ OFF Layer: " layer))
         )
       )
       (vla-Regen doc 1))
      ((eq (car pick) 'MISS)
       (princ "\nคลิกไม่โดนวัตถุ กรุณาคลิกใหม่ หรือกด Enter เพื่อจบ"))
      ((eq (car pick) 'SKIP)
       (princ "\nไม่ได้เลือก Layer กรุณาคลิกใหม่"))
      (T
       (setq done T))
    )
  )
  (princ
    (strcat
      "\nจบการทำงาน ซ่อนชั่วคราวแล้ว "
      (itoa hidden)
      " Layer"
    )
  )
  (princ)
)

(defun lccui:outer-layer-from-selection (selection / data)
  ;; ENTSEL returns the top-level Block/Xref reference, which is exactly
  ;; the entity whose insertion layer controls the whole group.
  (if selection
    (progn
      (setq data (entget (car selection)))
      (cdr (assoc 8 data))
    )
  )
)

(defun lccui:hide-picked-group
       (/ doc selection layer hidden xref-count total-xref-count
          done error-code)
  ;; Use the original LCC FE mechanism and continue accepting clicks.
  (setq doc    (lccui:doc)
        hidden 0
        total-xref-count 0
        done nil)
  (while (not done)
    (setvar "ERRNO" 0)
    (setq selection
      (entsel
        "\nคลิกวัตถุหรือ Xref ที่ต้องการซ่อนทั้งก้อน <Enter เพื่อจบ>: ")
          error-code (getvar "ERRNO"))
    (cond
      (selection
       (setq layer (lccui:outer-layer-from-selection selection))
       (if (null layer)
         (princ "\nไม่สามารถอ่าน Layer ได้ กรุณาลองใหม่")
         (progn
           (setq xref-count
                  (lccui:set-master-layer
                    doc layer :vlax-false)
                 hidden (1+ hidden)
                 total-xref-count
                  (+ total-xref-count xref-count))
           (vla-Regen doc 1)
           (princ
             (strcat
               "\nOFF ทั้งก้อนบน Layer: "
               layer
               " และ Layer ภายใน Xref "
               (itoa xref-count)
               " Layer"
             )
           )
         )
       ))
      ((= error-code 52)
       (setq done T))
      (T
       (princ "\nคลิกไม่โดนวัตถุ กรุณาคลิกใหม่ หรือกด Enter เพื่อจบ"))
    )
  )
  (princ
    (strcat
      "\nจบการทำงาน ซ่อนชั่วคราวเป็นก้อนแล้ว "
      (itoa hidden)
      " Layer หลัก และ Layer ภายใน Xref "
      (itoa total-xref-count)
      " Layer"
    )
  )
  (princ)
)

(defun lccui:freeze-picked-group
       (/ doc selection layer current-layer frozen done error-code)
  ;; Long-term Freeze of the outer Block/Xref insertion layer.
  (setq doc           (lccui:doc)
        current-layer (getvar "CLAYER")
        frozen        0
        done          nil)
  (while (not done)
    (setvar "ERRNO" 0)
    (setq selection
      (entsel
        "\nคลิกวัตถุหรือ Xref ที่ต้องการ Freeze ทั้งก้อน <Enter เพื่อจบ>: ")
          error-code (getvar "ERRNO"))
    (cond
      (selection
       (setq layer (lccui:outer-layer-from-selection selection))
       (cond
         ((null layer)
          (princ "\nไม่สามารถอ่าน Layer ได้ กรุณาลองใหม่"))
         ((= (strcase layer) (strcase current-layer))
          (princ
            (strcat
              "\nไม่สามารถ Freeze Layer ปัจจุบันได้: "
              layer
            )
          ))
         ((lccui:freeze-layer doc layer)
          (setq frozen (1+ frozen))
          (vla-Regen doc 1)
          (princ
            (strcat
              "\nFreeze ทั้งก้อนบน Layer: "
              layer
              " เรียบร้อยแล้ว"
            )
          ))
         (T
          (princ (strcat "\nไม่สามารถ Freeze Layer: " layer)))
       ))
      ((= error-code 52)
       (setq done T))
      (T
       (princ "\nคลิกไม่โดนวัตถุ กรุณาคลิกใหม่ หรือกด Enter เพื่อจบ"))
    )
  )
  (princ
    (strcat
      "\nจบการทำงาน Freeze ทั้งก้อนแล้ว "
      (itoa frozen)
      " Layer"
    )
  )
  (princ)
)

(defun lccui:restore-off-layers (/ doc layer count result)
  ;; Temporary restore: ON only. Never changes Freeze/Thaw.
  (setq doc   (lccui:doc)
        count 0)
  (vlax-for layer (vla-get-Layers doc)
    (setq result
      (vl-catch-all-apply
        'vla-put-LayerOn
        (list layer :vlax-true)
      )
    )
    (if (not (vl-catch-all-error-p result))
      (setq count (1+ count))
    )
  )
  (vla-Regen doc 1)
  (princ
    (strcat
      "\nเปิด Layer ที่ OFF โดยไม่เปลี่ยนสถานะ Freeze จำนวน "
      (itoa count)
      " Layer"
    )
  )
  (princ)
)

(defun lccui:restore-frozen-layers (/ doc layer count result)
  ;; Long-term restore: THAW only. Never changes ON/OFF.
  (setq doc   (lccui:doc)
        count 0)
  (vlax-for layer (vla-get-Layers doc)
    (setq result
      (vl-catch-all-apply
        'vla-put-Freeze
        (list layer :vlax-false)
      )
    )
    (if (not (vl-catch-all-error-p result))
      (setq count (1+ count))
    )
  )
  (vla-Regen doc 1)
  (princ
    (strcat
      "\nThaw Layer โดยไม่เปลี่ยนสถานะ ON/OFF จำนวน "
      (itoa count)
      " Layer"
    )
  )
  (princ)
)

(defun lccui:sync-all-xrefs (/ doc layer layer-name state count)
  (setq doc (lccui:doc)
        count 0)
  (vlax-for layer (vla-get-Layers doc)
    (setq layer-name (vla-get-Name layer)
          state (vla-get-LayerOn layer))
    (foreach xref-name (lccui:xrefs-on-layer layer-name)
      (setq count (+ count (lccui:set-xref-layers doc xref-name state)))
    )
  )
  (vla-Regen doc 1)
  count
)

(defun lccui:move-to-current (/ ss current-layer)
  (setq current-layer (getvar "CLAYER"))
  (prompt "\nเลือกวัตถุที่ต้องการย้ายไปยัง Layer ปัจจุบัน: ")
  (if (setq ss (ssget))
    (command "_.CHPROP" ss "" "_LA" current-layer "")
  )
  (princ)
)

(defun lccui:isolate-selected (/ doc keep layer layer-name xref-name)
  (setq doc (lccui:doc))
  (if (setq keep (lccui:selected-layers))
    (progn
      (vlax-for layer (vla-get-Layers doc)
        (vl-catch-all-apply
          'vla-put-LayerOn
          (list layer :vlax-false)
        )
      )
      (foreach layer-name keep
        (lccui:set-layer-on doc layer-name :vlax-true)
        (foreach xref-name (lccui:xrefs-on-layer layer-name)
          (lccui:set-xref-layers doc xref-name :vlax-true)
        )
      )
      (vla-Regen doc 1)
      (princ "\nแสดงเฉพาะ Layer ที่เลือก รวม Layer ภายใน Xref เรียบร้อยแล้ว")
    )
    (princ "\nไม่ได้เลือกวัตถุ")
  )
  (princ)
)

(defun lccui:sync-xrefs (/ count)
  (setq count (lccui:sync-all-xrefs))
  (princ
    (strcat
      "\nซิงก์สถานะ Layer ภายใน Xref จำนวน "
      (itoa count)
      " Layer เรียบร้อยแล้ว"
    )
  )
  (princ)
)

(defun lccui:write-dcl (/ path file lines)
  (setq path (vl-filename-mktemp "LCCUI-" nil ".dcl")
        file (open path "w")
        lines
         '(
           "lccui : dialog {"
           "  label = \"LCC - จัดการเลเยอร์\";"
           "  : boxed_column {"
           "    label = \"ปิดชั่วคราว (OFF / ON)\";"
           "    : button { key = \"a1\"; label = \"ซ่อนเลเยอร์ต่อเนื่อง\"; }"
           "    : button { key = \"a2\"; label = \"ซ่อนทั้งก้อนต่อเนื่อง\"; }"
           "    : button { key = \"a3\"; label = \"แสดงเฉพาะเลเยอร์ที่เลือก\"; }"
           "    : button { key = \"a8\"; label = \"เปิดที่ซ่อนชั่วคราว\"; }"
           "  }"
           "  : boxed_column {"
           "    label = \"ปิดระยะยาว (FREEZE / THAW)\";"
           "    : button { key = \"a9\"; label = \"Freeze เลเยอร์ต่อเนื่อง\"; }"
           "    : button { key = \"a11\"; label = \"Freeze ทั้งก้อนต่อเนื่อง\"; }"
           "    : button { key = \"a10\"; label = \"Thaw ที่ Freeze ไว้\"; }"
           "  }"
           "  : boxed_column {"
           "    label = \"เครื่องมือ\";"
           "    : button { key = \"a0\"; label = \"ย้ายไปเลเยอร์ปัจจุบัน\"; }"
           "    : button { key = \"a4\"; label = \"ตั้งเป็นเลเยอร์ปัจจุบัน\"; }"
           "    : button { key = \"a5\"; label = \"ซิงก์ Xref\"; }"
           "    : button { key = \"a6\"; label = \"[1] ล็อกเลเยอร์\"; }"
           "    : button { key = \"a7\"; label = \"[3] ปลดล็อกเลเยอร์\"; }"
           "  }"
           "  : text { label = \"คลิกชื่อเพื่อเริ่มทำงานทันที\"; alignment = centered; }"
           "  spacer;"
           "  : button { key = \"cancel\"; label = \"ปิดหน้าต่าง\"; is_cancel = true; width = 16; alignment = centered; }"
           "}"
          ))
  (if file
    (progn
      (foreach line lines (write-line line file))
      (close file)
      path
    )
    nil
  )
)

(defun lccui:action-keys ()
  '(
    "a0" "a1" "a2" "a3" "a4" "a5"
    "a6" "a7" "a8" "a9" "a10" "a11"
   )
)

(defun lccui:run-action (index)
  (cond
    ((= index 0)  (lccui:move-to-current))
    ((= index 1)  (lccui:hide-picked-layer))
    ((= index 2)  (lccui:hide-picked-group))
    ((= index 3)  (lccui:isolate-selected))
    ((= index 4)  (command "_.LAYMCUR"))
    ((= index 5)  (lccui:sync-xrefs))
    ((= index 6)  (lccui:native-lock-loop "_.LAYLCK" "ล็อก"))
    ((= index 7)  (lccui:native-lock-loop "_.LAYULK" "ปลดล็อก"))
    ((= index 8)  (lccui:restore-off-layers))
    ((= index 9)  (lccui:freeze-picked-layer))
    ((= index 10) (lccui:restore-frozen-layers))
    ((= index 11) (lccui:freeze-picked-group))
  )
  (princ)
)

;; Frequently used Command Line shortcuts. Both commands retain LCC's
;; continuous-pick behavior: click as many objects as needed, then Enter.
(defun C:1 ()
  (lccui:native-lock-loop "_.LAYLCK" "ล็อก")
)

(defun C:3 ()
  (lccui:native-lock-loop "_.LAYULK" "ปลดล็อก")
)

(defun C:LCC (/ *error* dcl-path dcl-id result choice tile)
  (defun *error* (message)
    (if (and dcl-id (> dcl-id 0))
      (unload_dialog dcl-id)
    )
    (if (and dcl-path (findfile dcl-path))
      (vl-file-delete dcl-path)
    )
    (if
      (and message
           (not (wcmatch (strcase message) "*CANCEL*,*EXIT*")))
      (princ (strcat "\nLCC Error: " message))
    )
    (princ)
  )

  (setq choice   nil
        dcl-path (lccui:write-dcl))

  (if (null dcl-path)
    (princ "\nไม่สามารถสร้างหน้าต่าง LCC ได้")
    (progn
      (setq dcl-id (load_dialog dcl-path))
      (if (or (< dcl-id 0) (not (new_dialog "lccui" dcl-id)))
        (princ "\nไม่สามารถเปิดหน้าต่าง LCC ได้")
        (progn
          (foreach tile (lccui:action-keys)
            (action_tile
              tile
              (strcat
                "(setq choice \""
                tile
                "\")(done_dialog 1)"
              )
            )
          )
          (action_tile "cancel" "(done_dialog 0)")
          (setq result (start_dialog))
        )
      )
      (if (and dcl-id (> dcl-id 0))
        (unload_dialog dcl-id)
      )
      (if (findfile dcl-path)
        (vl-file-delete dcl-path)
      )
      (setq dcl-id nil
            dcl-path nil)
      (if (= result 1)
        (lccui:run-action (atoi (substr choice 2)))
      )
    )
  )
  (princ)
)

(princ "\nโหลด LCC เรียบร้อยแล้ว: LCC = เปิดศูนย์จัดการ Layer, 1 = ล็อก, 3 = ปลดล็อก")
(princ)
