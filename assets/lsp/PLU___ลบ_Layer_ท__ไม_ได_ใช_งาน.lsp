;;; PLU - Purge unused layers
;;; Keeps Layer 0, the current layer, referenced layers, and Xref-dependent layers.

(defun c:PLU (/ *error* oldCmdecho item before after)

  (defun *error* (msg)
    (if oldCmdecho
      (setvar "CMDECHO" oldCmdecho)
    )
    (if (and msg
             (/= msg "Function cancelled")
             (/= msg "quit / exit abort"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; Count layers before purge.
  (setq before 0
        item   (tblnext "LAYER" T))
  (while item
    (setq before (1+ before)
          item   (tblnext "LAYER"))
  )

  (setq oldCmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; Use AutoCAD's native purge rules so referenced layers remain safe.
  (command "_.-PURGE" "_Layers" "*" "_No")

  (setvar "CMDECHO" oldCmdecho)

  ;; Count layers after purge and report the result.
  (setq after 0
        item  (tblnext "LAYER" T))
  (while item
    (setq after (1+ after)
          item  (tblnext "LAYER"))
  )

  (princ
    (strcat
      "\nPLU complete: "
      (itoa (- before after))
      " unused layer(s) removed. "
      (itoa after)
      " layer(s) remain."
    )
  )
  (princ)
)

(princ "\nPLU loaded. Type PLU to purge unused layers.")
(princ)
