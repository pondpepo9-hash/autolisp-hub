(defun C:LEGEND ( / *error* a acdoc acobj an co e hs ht i la lst lt p p1 p2 p3 p4 p5 space ss st ro dr laydata entdata blk ent mode layerrec visitedBlocks drawlayer x)
  (vl-load-com)
  (setq acObj (vlax-get-acad-object)
        acDoc (vla-get-activedocument acObj)
        space (vlax-get acDoc (if (= 1 (getvar 'cvport)) 'PaperSpace 'ModelSpace))
  )
  (vla-startundomark acDoc)
  
  ;;;;;; Error function ;;;;;;;;;
  (defun *error* (msg)
    (and
      msg
      (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))
      (princ (strcat "\nError: " msg))
      )
    (if (and a (not (vlax-erased-p a))) (vla-delete a))
    (vla-endundomark acDoc)
    (princ)
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;; Function to get layer color data ;;;;;;;;;
  (defun get-layer-color-data (layername / layere layent co co420 co430)
    (if (setq layere (tblobjname "LAYER" layername))
      (progn
        (setq layent (entget layere))
        (setq co (cdr (assoc 62 layent))
              co420 (cdr (assoc 420 layent))
              co430 (cdr (assoc 430 layent)))
        (if (not co) (setq co 7))
        (if (< co 0) (setq co (- co)))
        (list co co420 co430)
      )
      (list 7 nil nil)
    )
  )

  (defun format-color-book (cb-str / pos)
    (if (setq pos (vl-string-search "$" cb-str))
      (substr cb-str (+ pos 2))
      cb-str
    )
  )
  
  (defun truecolor-to-rgb (tc / r g b)
    (setq b (logand tc 255)
          g (logand (lsh tc -8) 255)
          r (logand (lsh tc -16) 255))
    (strcat (itoa r) "," (itoa g) "," (itoa b))
  )

  (defun get-color-text (co co420 co430 laydata)
    (cond
      (co430 (format-color-book co430))
      (co420 (strcat "RGB " (truecolor-to-rgb co420)))
      ((and co (/= co 256)) (itoa co))
      (t
        (strcat "ByLayer ("
          (cond
            ((caddr laydata) (format-color-book (caddr laydata)))
            ((cadr laydata) (strcat "RGB " (truecolor-to-rgb (cadr laydata))))
            (t (itoa (car laydata)))
          )
        ")")
      )
    )
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;; Xref filters and layer-list helpers ;;;;;;;;;
  (defun xref-layer-p (layername / rec flags)
    (setq rec (if layername (tblsearch "LAYER" layername)))
    (setq flags (if rec (cdr (assoc 70 rec))))
    (if (null flags) (setq flags 0))
    (or
      (null layername)
      (vl-string-search "|" layername)
      (/= 0 (logand flags 16))
    )
  )

  (defun add-selected-layer (e / la lt co co420 co430)
    (setq la    (cdr (assoc 8 e))
          lt    (cdr (assoc 6 e))
          co    (cdr (assoc 62 e))
          co420 (cdr (assoc 420 e))
          co430 (cdr (assoc 430 e)))
    ;; Add one row per layer and exclude Xref-dependent layers.
    (if (and (not (xref-layer-p la))
             (not (assoc la lst)))
      (setq lst (cons (list la lt co co420 co430) lst))
    )
  )

  (defun collect-all-layers (/ rec name)
    (setq rec (tblnext "LAYER" T))
    (while rec
      (setq name (cdr (assoc 2 rec)))
      (if (not (xref-layer-p name))
        ;; The sample is created on this layer with ByLayer properties.
        (setq lst (cons (list name nil nil nil nil) lst))
      )
      (setq rec (tblnext "LAYER"))
    )
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;;;;; Function to process entities inside blocks ;;;;;;;;;
  (defun process-block-entities (blk-name / blk-obj flags ent e)
    (if (and
          (not (member blk-name visitedBlocks))
          (setq blk-obj (tblsearch "BLOCK" blk-name))
          (setq flags (cond ((cdr (assoc 70 blk-obj))) (0)))
          ;; Flags 4 and 8 identify attached and overlaid Xrefs.
          (= 0 (logand flags 12))
        )
      (progn
        (setq visitedBlocks (cons blk-name visitedBlocks))
        (setq ent (cdr (assoc -2 blk-obj)))
        (while
          (and ent
               (setq e (entget ent))
               (/= "ENDBLK" (cdr (assoc 0 e))))
          (process-entity e)
          (setq ent (entnext ent))
        )
      )
    )
  )
  
  ;;;;;; Function to process individual entity ;;;;;;;;;
  (defun process-entity (e / blk-name)
    ; Add the entity's own layer to the list (including block references).
    (add-selected-layer e)
    (if (= "INSERT" (cdr (assoc 0 e)))
      ; If it is a normal block, also process its contents.
      (progn
        (setq blk-name (cdr (assoc 2 e)))
        (process-block-entities blk-name)
      )
    )
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (setq st (entget (tblobjname "style" (getvar 'textstyle)) '("AcadAnnotative"))
        an (member '(1070 . 1) (cdr (member '(1070 . 1) (cadr (assoc -3 st)))))
        hs (cdr (assoc 40 st))
        ro (angle '(0 0 0) (trans (getvar "UCSXDIR") 0 (trans '(0 0 1) 1 0 T)))
        dr (trans '(0 0 1) 1 0 T)
        )
  (if
    an
    (setq ht (/ (if (> hs 0) hs 3.0) (cond ((getvar 'cannoscalevalue)) (1.0))))
    (setq ht (* (if (> hs 0) hs 3.0) (getvar 'ltscale)))
    )
  (initget "All Selected")
  (setq mode (getkword "\nLayer source [All/Selected] <Selected>: "))
  (if (null mode) (setq mode "Selected"))

  (cond
    ((= mode "All")
      (collect-all-layers)
    )
    ;; ssget accepts the prompt only after a selection mode/filter.
    ;; Passing the prompt as the first argument makes AutoCAD treat it as
    ;; an invalid selection mode and causes the Selected option to error.
    ((setq ss (ssget))
      (repeat (setq i (sslength ss))
        (setq e (entget (ssname ss (setq i (1- i)))))
        (process-entity e)
      )
    )
  )

  (if lst
    (progn
      (setq lst
        (vl-sort lst
          '(lambda (a b)
             (< (strcase (car a)) (strcase (car b))))
        )
      )
      (if
        (setq p (getpoint "\nSpecify insert point: "))
        (progn
          (foreach x lst
          ;; Every legend row belongs to the layer it represents.
          (setq drawlayer (car x)
                p1 (trans p 1 0)                           ; Start point for line
                p2 (trans (polar p 0.0 (* 8 ht)) 1 0)      ; End point for line
                p3 (trans (polar p 0.0 (* 10 ht)) 1 0)     ; Layer name position
                p4 (trans (polar p 0.0 (* 25 ht)) 1 0)     ; Color code position
                laydata (get-layer-color-data (car x))     ; Get layer's default color data
                )
          
          ; Column 1: Create line sample
          (setq entdata
            (list
              '(0 . "LINE")
              (cons 8 drawlayer)
              (cons 10 p1)
              (cons 11 p2)
              (cons 6  (cond ((cadr x)) ("ByLayer")))
              (cons 62 (cond ((caddr x)) (256)))
            )
          )
          (if (nth 3 x) (setq entdata (append entdata (list (cons 420 (nth 3 x))))))
          (if (nth 4 x) (setq entdata (append entdata (list (cons 430 (nth 4 x))))))
          (entmake entdata)
          
          ; Column 2: Layer name text
          (vla-put-textalignmentpoint
            (vlax-ename->vla-object
              (entmakex
                (list
                  '(0 . "TEXT")
                  (cons 8 drawlayer)
                  (cons 6 "ByLayer")
                  (cons 62 256)
                  '(100 . "AcDbText")
                  (list 10 0 0 0)
                  (cons 40 ht)
                  (cons 1 (car x))                          ; Layer name only
                  (cons 50 ro)
                  (cons 7 (getvar 'textstyle))
                  (cons 72 0)
                  (list 10 0 0 0)
                  (cons 210 dr)
                  (cons 73 2)
                )
              )
            )
            (vlax-3d-point p3)
          )
          
          ; Column 3: Color code text
          (vla-put-textalignmentpoint
            (vlax-ename->vla-object
              (entmakex
                (list
                  '(0 . "TEXT")
                  (cons 8 drawlayer)
                  (cons 6 "ByLayer")
                  (cons 62 256)
                  '(100 . "AcDbText")
                  (list 10 0 0 0)
                  (cons 40 ht)
                  (cons 1 (get-color-text (caddr x) (nth 3 x) (nth 4 x) laydata))
                  (cons 50 ro)
                  (cons 7 (getvar 'textstyle))
                  (cons 72 0)
                  (list 10 0 0 0)
                  (cons 210 dr)
                  (cons 73 2)
                )
              )
            )
            (vlax-3d-point p4)
          )
          
            (setq p (polar p (/ pi -2.0) (* 2 ht)))
          )
          (princ
            (strcat
              "\nLEGEND created: "
              (itoa (length lst))
              " layer(s), Xref layers excluded."
            )
          )
        )
      )
    )
    (princ "\nNo eligible layers found. Xref layers are excluded.")
  )
  (*error* nil)
  (princ)
  )
