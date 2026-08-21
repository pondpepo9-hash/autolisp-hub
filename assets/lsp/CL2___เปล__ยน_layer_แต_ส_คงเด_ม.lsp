(vl-load-com)

;; CL2 = Change Layer and keep the object's current appearance.
;; Properties which are already explicit or ByBlock are left untouched.
;; Properties which are ByLayer are resolved from the old layer before the
;; object is moved, so the new layer cannot change their displayed values.

(defun CL2:GetProperty (object property)
  (vl-catch-all-apply 'vlax-get-property (list object property))
)

(defun CL2:CaptureByLayerProperties (object layer / property value layer-value result)
  (setq result nil)

  ;; Color (supports ACI, TrueColor and Color Book colors).
  (if (and (vlax-property-available-p object 'Color)
           (vlax-property-available-p object 'TrueColor T)
           (vlax-property-available-p layer 'TrueColor))
    (progn
      (setq value (CL2:GetProperty object 'Color))
      (if (and (not (vl-catch-all-error-p value)) (= value 256))
        (progn
          (setq layer-value (CL2:GetProperty layer 'TrueColor))
          (if (not (vl-catch-all-error-p layer-value))
            (setq result (cons (cons 'TrueColor layer-value) result))
          )
        )
      )
    )
  )

  ;; Linetype.
  (if (and (vlax-property-available-p object 'Linetype T)
           (vlax-property-available-p layer 'Linetype))
    (progn
      (setq value (CL2:GetProperty object 'Linetype))
      (if (and (not (vl-catch-all-error-p value))
               (= (strcase value) "BYLAYER"))
        (progn
          (setq layer-value (CL2:GetProperty layer 'Linetype))
          (if (not (vl-catch-all-error-p layer-value))
            (setq result (cons (cons 'Linetype layer-value) result))
          )
        )
      )
    )
  )

  ;; Lineweight (-1 = ByLayer).
  (if (and (vlax-property-available-p object 'Lineweight T)
           (vlax-property-available-p layer 'Lineweight))
    (progn
      (setq value (CL2:GetProperty object 'Lineweight))
      (if (and (not (vl-catch-all-error-p value)) (= value -1))
        (progn
          (setq layer-value (CL2:GetProperty layer 'Lineweight))
          (if (not (vl-catch-all-error-p layer-value))
            (setq result (cons (cons 'Lineweight layer-value) result))
          )
        )
      )
    )
  )

  ;; Transparency, plot style and material are version/object dependent.
  ;; They are copied only when both the object and layer expose the property.
  (foreach property '(Transparency PlotStyleName Material)
    (if (and (vlax-property-available-p object property T)
             (vlax-property-available-p layer property))
      (progn
        (setq value (CL2:GetProperty object property))
        (if (and (not (vl-catch-all-error-p value))
                 (eq (type value) 'STR)
                 (= (strcase value) "BYLAYER"))
          (progn
            (setq layer-value (CL2:GetProperty layer property))
            (if (not (vl-catch-all-error-p layer-value))
              (setq result (cons (cons property layer-value) result))
            )
          )
        )
      )
    )
  )

  result
)

(defun CL2:ApplyProperties (object properties / item apply-result failures)
  (setq failures 0)
  (foreach item properties
    (setq apply-result
      (vl-catch-all-apply
        'vlax-put-property
        (list object (car item) (cdr item))
      )
    )
    (if (vl-catch-all-error-p apply-result)
      (setq failures (1+ failures))
    )
  )
  failures
)

(defun c:CL2 (/ *error* acad doc layers ss current-layer i entity object
                old-layer layer-object properties move-result moved skipped
                property-failures undo-open)
  (setq acad (vlax-get-acad-object)
        doc (vla-get-ActiveDocument acad)
        layers (vla-get-Layers doc)
        current-layer (getvar "CLAYER")
        moved 0
        skipped 0
        property-failures 0
        undo-open nil)

  (defun *error* (message)
    (if undo-open
      (progn
        (vla-EndUndoMark doc)
        (setq undo-open nil)
      )
    )
    (if (and message
             (/= message "Function cancelled")
             (/= message "quit / exit abort"))
      (princ (strcat "\nCL2 error: " message))
    )
    (princ)
  )

  (princ (strcat "\nSelect objects to move to layer: " current-layer))
  (setq ss (ssget))

  (if ss
    (progn
      (vla-StartUndoMark doc)
      (setq undo-open T
            i 0)

      (repeat (sslength ss)
        (setq entity (ssname ss i)
              object (vlax-ename->vla-object entity)
              old-layer (CL2:GetProperty object 'Layer))

        (if (or (vl-catch-all-error-p old-layer)
                (= (strcase old-layer) (strcase current-layer)))
          (setq skipped (1+ skipped))
          (progn
            (setq layer-object
              (vl-catch-all-apply 'vla-Item (list layers old-layer)))

            (if (vl-catch-all-error-p layer-object)
              (setq skipped (1+ skipped))
              (progn
                ;; Capture first, change only the layer, then restore every
                ;; appearance property which previously came from ByLayer.
                (setq properties
                  (CL2:CaptureByLayerProperties object layer-object))
                (setq move-result
                  (vl-catch-all-apply
                    'vlax-put-property
                    (list object 'Layer current-layer)
                  )
                )

                (if (vl-catch-all-error-p move-result)
                  (setq skipped (1+ skipped))
                  (progn
                    (setq moved (1+ moved))
                    (setq property-failures
                      (+ property-failures
                         (CL2:ApplyProperties object properties)))
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )

      (vla-EndUndoMark doc)
      (setq undo-open nil)
      (vla-Regen doc 1)

      (princ
        (strcat "\nCL2 complete: moved " (itoa moved) " object(s) to layer "
                current-layer "; original appearance retained."))
      (if (> skipped 0)
        (princ
          (strcat "\nSkipped " (itoa skipped)
                  " object(s) (same layer or object could not be modified)."))
      )
      (if (> property-failures 0)
        (princ
          (strcat "\nWarning: " (itoa property-failures)
                  " property value(s) could not be restored."))
      )
    )
    (princ "\nNo objects selected.")
  )
  (princ)
)

(princ "\nType CL2 to change layer while retaining the original object appearance.")
(princ)
