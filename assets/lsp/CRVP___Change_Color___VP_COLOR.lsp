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

(defun CRVP:write-dcl (path lines / file)
  (if (setq file (open path "w"))
    (progn
      (foreach line lines (write-line line file))
      (close file)
      path
    )
  )
)

(defun CRVP:choose-mode (/ path dcl-id result)
  (setq path (vl-filename-mktemp "crvp_mode.dcl"))
  (if
    (CRVP:write-dcl
      path
      '("crvp_mode : dialog {"
        "  label = \"CRVP - Select operation\";"
        "  : text { label = \"Choose which type of color you want to change.\"; alignment = centered; }"
        "  spacer;"
        "  : row {"
        "    : button { key = \"object\"; label = \"OBJECT COLOR\"; width = 22; height = 2; is_default = true; }"
        "    : button { key = \"vp\"; label = \"VP COLOR\"; width = 22; height = 2; }"
        "  }"
        "  : button { key = \"cancel\"; label = \"Cancel\"; is_cancel = true; fixed_width = true; alignment = centered; }"
        "}"
       )
    )
    (progn
      (setq dcl-id (load_dialog path))
      (if (and (> dcl-id 0) (new_dialog "crvp_mode" dcl-id))
        (progn
          (action_tile "object" "(setq result \"OBJECT\")(done_dialog 1)")
          (action_tile "vp" "(setq result \"VP\")(done_dialog 1)")
          (action_tile "cancel" "(setq result nil)(done_dialog 0)")
          (start_dialog)
        )
      )
      (if (and dcl-id (> dcl-id 0)) (unload_dialog dcl-id))
      (vl-file-delete path)
    )
  )
  result
)

(defun CRVP:add-unique (item items)
  (if (or (null item) (= item "") (member (strcase item) (mapcar 'strcase items)))
    items
    (cons item items)
  )
)

(defun CRVP:effective-layer (layer inherited)
  (if (= (strcase layer) "0") inherited layer)
)

(defun CRVP:collect-block-layers (block-name inherited / visit-key block-ent ent data type layer effective nested)
  ;; *crvp-visited-blocks* and *crvp-found-layers* are dynamically scoped.
  (setq visit-key (strcat (strcase block-name) "|" (strcase inherited)))
  (if (not (member visit-key *crvp-visited-blocks*))
    (progn
      (setq *crvp-visited-blocks* (cons visit-key *crvp-visited-blocks*))
      (if (setq block-ent (tblobjname "BLOCK" block-name))
        (progn
          (setq ent (entnext block-ent))
          (while (and ent (/= "ENDBLK" (cdr (assoc 0 (setq data (entget ent))))))
            (setq type (cdr (assoc 0 data))
                  layer (cond ((cdr (assoc 8 data))) (t "0"))
                  effective (CRVP:effective-layer layer inherited))
            (setq *crvp-found-layers* (CRVP:add-unique effective *crvp-found-layers*))
            (if (and (= type "INSERT") (setq nested (cdr (assoc 2 data))))
              (CRVP:collect-block-layers nested effective)
            )
            (setq ent (entnext ent))
          )
        )
      )
    )
  )
  *crvp-found-layers*
)

(defun CRVP:get-block-layers (ent / data base-layer block-name *crvp-visited-blocks* *crvp-found-layers*)
  (setq data (entget ent)
        base-layer (cdr (assoc 8 data))
        block-name (cdr (assoc 2 data))
        *crvp-visited-blocks* nil
        *crvp-found-layers* nil)
  (setq *crvp-found-layers* (CRVP:add-unique base-layer *crvp-found-layers*))
  (CRVP:collect-block-layers block-name base-layer)
  (vl-sort *crvp-found-layers*
    '(lambda (a b) (< (strcase a) (strcase b))))
)

(defun CRVP:indices->items (indices items / result)
  (foreach index indices
    (if (nth index items) (setq result (cons (nth index items) result)))
  )
  (reverse result)
)

(defun CRVP:rgb-label (value)
  (strcat
    (itoa (logand (lsh value -16) 255)) ","
    (itoa (logand (lsh value -8) 255)) ","
    (itoa (logand value 255))
  )
)

(defun CRVP:encoded-color-label (value / method)
  (setq method (logand (lsh value -24) 255))
  (cond
    ((= method 195) (itoa (logand value 65535)))
    ((= method 192) "BYLAYER")
    ((= method 193) "BYBLOCK")
    (t (CRVP:rgb-label value))
  )
)

(defun CRVP:accolor-label (color / method)
  (setq method (vla-get-ColorMethod color))
  (cond
    ((= method 194)
     (strcat
       (itoa (vla-get-Red color)) ","
       (itoa (vla-get-Green color)) ","
       (itoa (vla-get-Blue color))
     )
    )
    ((= method 195) (itoa (vla-get-ColorIndex color)))
    ((= method 192) "BYLAYER")
    ((= method 193) "BYBLOCK")
    (t
     (strcat
       (itoa (vla-get-Red color)) ","
       (itoa (vla-get-Green color)) ","
       (itoa (vla-get-Blue color))
     )
    )
  )
)

(defun CRVP:layer-color-label (layer / adoc layers layer-obj color-obj result data value)
  ;; Read through AcCmColor first. This preserves the RGB components of a
  ;; True Color layer more reliably than relying on the layer table alone.
  (setq adoc (vla-get-ActiveDocument (vlax-get-Acad-Object))
        layers (vla-get-Layers adoc)
        layer-obj (vl-catch-all-apply 'vla-Item (list layers layer)))
  (if (not (vl-catch-all-error-p layer-obj))
    (progn
      (setq color-obj (vl-catch-all-apply 'vla-get-TrueColor (list layer-obj)))
      (if (not (vl-catch-all-error-p color-obj))
        (progn
          (setq result (vl-catch-all-apply 'CRVP:accolor-label (list color-obj)))
          (if (vl-catch-all-error-p result) (setq result nil))
        )
      )
    )
  )
  (if (not result)
    (if (setq data (tblsearch "LAYER" layer))
      (cond
        ((setq value (cdr (assoc 420 data)))
         (setq result (CRVP:encoded-color-label value)))
        ((setq value (cdr (assoc 62 data)))
         (setq result (itoa (abs value))))
        (t (setq result "UNKNOWN"))
      )
      (setq result "NOT FOUND")
    )
  )
  (if (and color-obj (not (vl-catch-all-error-p color-obj)))
    (vlax-release-object color-obj))
  (if (and layer-obj (not (vl-catch-all-error-p layer-obj)))
    (vlax-release-object layer-obj))
  (vlax-release-object layers)
  result
)

(defun CRVP:current-viewport-ename (/ ss)
  (if
    (setq ss
      (ssget "_X"
        (list
          '(0 . "VIEWPORT")
          (cons 69 (getvar "CVPORT"))
          (cons 410 (getvar "CTAB"))
        )
      )
    )
    (ssname ss 0)
  )
)

(defun CRVP:vp-color-data (layer viewport / layer-ent layer-data dict-ent xrec pair active matched result)
  ;; Viewport color overrides are stored in the layer extension dictionary.
  (if
    (and
      viewport
      (setq layer-ent (tblobjname "LAYER" layer))
      (setq layer-data (entget layer-ent))
      (setq dict-ent (cdr (assoc 360 layer-data)))
      (setq xrec (dictsearch dict-ent "ADSK_XREC_LAYER_COLOR_OVR"))
    )
    (foreach pair xrec
      (cond
        ((= 102 (car pair))
         (cond
           ((wcmatch (cdr pair) "{ADSK_LYR_COLOR_OVERRIDE*")
            (setq active T matched nil))
           ((= (cdr pair) "}")
            (setq active nil matched nil))
         )
        )
        ((and active (= 335 (car pair)))
         (setq matched (equal (cdr pair) viewport)))
        ((and active matched (= 420 (car pair)))
         (setq result (cons pair result)))
        ((and active matched (= 430 (car pair)))
         (setq result (cons pair result)))
      )
    )
  )
  (reverse result)
)

(defun CRVP:vp-color-label (layer viewport / data value method)
  (if (setq data (CRVP:vp-color-data layer viewport))
    (cond
      ((setq value (cdr (assoc 420 data)))
       (setq method (logand (lsh value -24) 255))
       (if (= method 192)
         (CRVP:layer-color-label layer)
         (CRVP:encoded-color-label value)
       )
      )
      (t "UNKNOWN")
    )
    (CRVP:layer-color-label layer)
  )
)

(defun CRVP:fit-text (text width / keep)
  (cond
    ((< (strlen text) width)
     (strcat text (substr "                                                                                " 1 (- width (strlen text)))))
    ((= (strlen text) width) text)
    (t
     (setq keep (- width 3))
     (strcat (substr text 1 keep) "..."))
  )
)

(defun CRVP:layer-row (layer viewport)
  (strcat
    (CRVP:fit-text layer 30) " | "
    (CRVP:fit-text (CRVP:layer-color-label layer) 22) " | "
    (CRVP:vp-color-label layer viewport)
  )
)

(defun CRVP:show-current-colors (layer viewport next-label)
  (alert
    (strcat
      "Layer: " layer
      "\nLayer Color: " (CRVP:layer-color-label layer)
      "\nVP Color: "
      (if viewport
        (CRVP:vp-color-label layer viewport)
        "N/A (no active Layout viewport)"
      )
      "\n\nChoose a new " next-label " next."
    )
  )
)

(defun CRVP:choose-layers (layers block-name viewport / path dcl-id selection result index rows)
  (setq rows (mapcar '(lambda (layer) (CRVP:layer-row layer viewport)) layers))
  (setq path (vl-filename-mktemp "crvp_layers.dcl"))
  (if
    (CRVP:write-dcl
      path
      '("crvp_layers : dialog {"
        "  label = \"CRVP - Layers in block\";"
        "  : text { key = \"block\"; label = \"Block\"; }"
        "  : text { label = \"Current colors are shown as ACI or R,G,B numbers.\"; }"
        "  : text { label = \"Select one or more layers to change:\"; }"
        "  : text { label = \"LAYER                          | LAYER COLOR            | VP COLOR\"; fixed_width_font = true; }"
        "  : list_box { key = \"layers\"; width = 88; height = 16; multiple_select = true; fixed_width_font = true; }"
        "  : row {"
        "    : button { key = \"all\"; label = \"Select All\"; }"
        "    : button { key = \"accept\"; label = \"Choose New Color\"; is_default = true; }"
        "    : button { key = \"cancel\"; label = \"Cancel\"; is_cancel = true; }"
        "  }"
        "}"
       )
    )
    (progn
      (setq dcl-id (load_dialog path))
      (if (and (> dcl-id 0) (new_dialog "crvp_layers" dcl-id))
        (progn
          (set_tile "block" (strcat "Block: " block-name))
          (start_list "layers")
          (mapcar 'add_list rows)
          (end_list)
          (set_tile "layers" "0")
          (setq selection "0")
          (action_tile "layers" "(setq selection $value)")
          (action_tile
            "all"
            "(setq selection \"\" index 0)(repeat (length layers)(setq selection (strcat selection (if (= selection \"\") \"\" \" \") (itoa index)) index (1+ index)))(set_tile \"layers\" selection)"
          )
          (action_tile "accept" "(if (/= selection \"\")(done_dialog 1)(alert \"Select at least one layer.\"))")
          (action_tile "cancel" "(setq selection nil)(done_dialog 0)")
          (if (= 1 (start_dialog))
            (setq result (CRVP:indices->items (read (strcat "(" selection ")")) layers))
          )
        )
      )
      (if (and dcl-id (> dcl-id 0)) (unload_dialog dcl-id))
      (vl-file-delete path)
    )
  )
  result
)

(defun CRVP:true->rgb (color)
  (list
    (logand (lsh (fix color) -16) 255)
    (logand (lsh (fix color) -8) 255)
    (logand (fix color) 255)
  )
)

(defun CRVP:set-vp-layer-color (layer color-result / value aci split rgb)
  (setq aci (cdr (assoc 62 color-result)))
  (cond
    ;; Check the special ACI values before 420/430 because some AutoCAD
    ;; versions may return an additional fallback color pair.
    ((and aci (or (= aci 256) (= aci 0)))
     (vl-cmdf "_.VPLAYER" "_RemoveOverrides" "_Color" layer "_Current" "")
    )
    ((setq value (cdr (assoc 430 color-result)))
     (if (setq split (vl-string-position 36 value))
       (vl-cmdf "_.VPLAYER" "_Color" "_Colorbook"
         (substr value 1 split) (substr value (+ split 2)) layer "_Current" "")
     )
    )
    ((setq value (cdr (assoc 420 color-result)))
     (setq rgb (CRVP:true->rgb value))
     (vl-cmdf "_.VPLAYER" "_Color" "_Truecolor"
       (strcat (itoa (car rgb)) "," (itoa (cadr rgb)) "," (itoa (caddr rgb)))
       layer "_Current" "")
    )
    (aci
     (vl-cmdf "_.VPLAYER" "_Color" aci layer "_Current" "")
    )
  )
  ;; Never leave VPLAYER waiting for more input if a host/version uses a
  ;; slightly different prompt sequence.
  (if (> (getvar "CMDACTIVE") 0) (command))
)

(defun CRVP:run-object-color (adoc / ss obj ent data layer viewport color-result i processed skipped *cr-processed-blocks*)
  ;; This is the original CR_Fixed selection and coloring flow.
  (if (setq ss (ssget ":L"))
    (progn
      (setq ent (ssname ss 0)
            data (entget ent)
            layer (cdr (assoc 8 data))
            viewport
              (if (and (= 0 (getvar "TILEMODE")) (> (getvar "CVPORT") 1))
                (CRVP:current-viewport-ename)
              )
      )
      (CRVP:show-current-colors layer viewport "object color")
      (if (setq color-result (acad_truecolordlg '(62 . 256) t))
        (progn
          (setq i 0 processed 0 skipped 0 *cr-processed-blocks* nil)
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
        (princ "\nNo new color selected; no object was changed.")
      )
    )
    (princ "\nNo objects selected.")
  )
)

(defun CRVP:run-vp-color (adoc / picked ent data type layers selected block-name viewport color-result old-cmdecho)
  (cond
    ((/= 0 (getvar "TILEMODE"))
     (alert "VP COLOR works in a Layout viewport.\nOpen a Layout and activate the viewport first."))
    ((<= (getvar "CVPORT") 1)
     (alert "Activate a Layout viewport first (double-click inside the viewport), then run CRVP again."))
    ((setq picked (entsel "\nSelect an object whose layer will receive VP COLOR: "))
     (setq ent (car picked)
           data (entget ent)
           type (cdr (assoc 0 data))
           viewport (CRVP:current-viewport-ename))
     (if (= type "INSERT")
       (progn
         (setq block-name (cdr (assoc 2 data))
               layers (CRVP:get-block-layers ent)
               selected (CRVP:choose-layers layers block-name viewport))
       )
       (progn
         (setq layers (list (cdr (assoc 8 data)))
               selected layers)
         (CRVP:show-current-colors (car layers) viewport "VP color")
       )
     )
     (if selected
       (if (setq color-result (acad_truecolordlg '(62 . 256) t))
         (progn
           (setq old-cmdecho (getvar "CMDECHO"))
           (setvar "CMDECHO" 0)
           (vla-StartUndoMark adoc)
           (foreach layer selected (CRVP:set-vp-layer-color layer color-result))
           (vla-EndUndoMark adoc)
           (setvar "CMDECHO" old-cmdecho)
           (vl-cmdf "_.REGEN")
           (princ (strcat "\nVP COLOR applied to " (itoa (length selected)) " layer(s)."))
         )
         (princ "\nNo new color selected; no layer was changed.")
       )
       (princ "\nVP COLOR cancelled; no layer was changed.")
     )
    )
    (t (princ "\nNo object selected."))
  )
)

(defun c:CRVP (/ adoc mode)
  (vl-load-com)
  (setq adoc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (if (setq mode (CRVP:choose-mode))
    (cond
      ((= mode "OBJECT") (CRVP:run-object-color adoc))
      ((= mode "VP") (CRVP:run-vp-color adoc))
    )
    (princ "\nCommand cancelled."))
  (princ)
)

(princ "\nCRVP loaded. Type CRVP to use OBJECT COLOR or VP COLOR.")
(princ)
