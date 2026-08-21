;; Create New Layer Identical Properties
(defun c:cnlip ( / src lName color ltype transp lineweight srcEnt srcLayer srcLayerEnt)
	
	;; ฟังก์ชันสร้างเลเยอร์ใหม่
	(defun _create_layer_local (lName color ltype plot lw / _rgb lt)
		(defun _rgb (l) (+ (lsh (fix (car l)) 16) (lsh (fix (cadr l)) 8) (fix (caddr l))))
		(cond	((not (tblsearch "layer" lName))
			(entmakex (list '(0 . "LAYER")
					'(100 . "AcDbSymbolTableRecord")
					'(100 . "AcDbLayerTableRecord")
					'(70 . 0)
					(cons 2 lName)
					(if color
						(if (listp color)					;; see if color is a list of RGB
							(cons 420 (_rgb color))		    
							(cons 62 color)
						)
						;; else, default white
						(cons 62 0)
					)
					(cons 6
						(if ltype
							(if (tblsearch "ltype" ltype)
								ltype
								"continuous"
							)
							"continuous"
						)
					)
					(cons 290 plot)
					;; เพิ่มรหัส 370 สำหรับ lineweight
					(cons 370 (if lw lw -1))  ;; ถ้าไม่ระบุให้เป็น ByLayer (-1)
					;;1 = plottable 0 = not=plottable
				)
			)
		)
		((tblobjname "layer" lName))
		)
	)
	
	;; ฟังก์ชันตั้งค่า transparency ให้กับเลเยอร์
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
	
	;; ฟังก์ชันรับค่า transparency จากเลเยอร์
	(defun get_layer_transparency (layer_name / layer_obj transparency_val)
		(if (setq layer_obj (tblobjname "layer" layer_name))
			(progn
				(setq transparency_data (assoc -3 (entget layer_obj '("accmtransparency"))))
				(if transparency_data
					(progn
						(setq transparency_val (cdr (assoc 1071 (cdadr transparency_data))))
						(if (and transparency_val (not (= transparency_val 0)))
							(rtos (* 100 (* 0.01 (fix (/ (- 33554687 transparency_val) 2.55)))) 2 0)
							"0"
						)
					)
					"0"
				)
			)
			"0"
		)
	)
	
	;; ฟังก์ชันรับค่า transparency จากวัตถุ
	(defun get_transparency (enme / num)
		(setq num (cdr (assoc 440 (entget enme))))
		(cond
			((not num) "0")
			((zerop num) "0")
			((= 16777216 num) "0")
			(T
				(rtos (* 100 (* 0.01 (fix (/ (- 33554687 num) 2.55)))) 2 0)
			)
		)
	)
	
	;; ฟังก์ชันแปลงค่า RGB เป็น TrueColor
	(defun rgb->true (r g b)
		(logior (lsh (fix r) 16) (lsh (fix g) 8) (fix b))
	)
	
	;; ฟังก์ชันแปลงค่า TrueColor เป็น RGB
	(defun true->rgb (c)
		(mapcar '(lambda (x) (lsh (lsh (fix c) x) -24)) '(8 16 24))
	)
	
	;; ฟังก์ชันดึงค่าสีจากเลเยอร์
	(defun get_layer_color (layer_name / layer_obj)
		(if (setq layer_obj (tblobjname "layer" layer_name))
			(cond
				;; ถ้ามีค่า True Color (420)
				((assoc 420 (entget layer_obj))
					(true->rgb (cdr (assoc 420 (entget layer_obj)))))
				;; ถ้ามีค่า Color Index (62)
				((assoc 62 (entget layer_obj))
					(cdr (assoc 62 (entget layer_obj))))
				;; ถ้าไม่มีค่าสีกำหนด
				(T nil)
			)
			nil
		)
	)
	
	;; ฟังก์ชันดึงค่า linetype จากเลเยอร์
	(defun get_layer_linetype (layer_name / layer_obj)
		(if (setq layer_obj (tblobjname "layer" layer_name))
			(if (assoc 6 (entget layer_obj))
				(cdr (assoc 6 (entget layer_obj)))
				"continuous"
			)
			"continuous"
		)
	)
	
	;; ฟังก์ชันดึงค่า lineweight จากเลเยอร์
	(defun get_layer_lineweight (layer_name / layer_obj)
		(if (setq layer_obj (tblobjname "layer" layer_name))
			(if (assoc 370 (entget layer_obj))
				(cdr (assoc 370 (entget layer_obj)))
				-1
			)
			-1
		)
	)
	
	;; เริ่มการทำงานหลัก
	(setq src (car (entsel "\nSelect source object: ")))
	(setq srcEnt (entget src))
	
	;; เก็บชื่อเลเยอร์ต้นฉบับ
	(setq srcLayer (cdr (assoc 8 srcEnt)))
	(princ "\nSource layer: ")
	(princ srcLayer)
	
	;; ตรวจสอบว่าคุณสมบัติของวัตถุเป็น ByLayer หรือไม่
	(setq is_color_bylayer (or (= (cdr (assoc 62 srcEnt)) 256) (not (assoc 62 srcEnt))))
	(setq is_linetype_bylayer (or (= (cdr (assoc 6 srcEnt)) "BYLAYER") (not (assoc 6 srcEnt))))
	(setq is_lineweight_bylayer (or (= (cdr (assoc 370 srcEnt)) -1) (not (assoc 370 srcEnt))))
	
	;; ดึงข้อมูลเลเยอร์ต้นฉบับ
	(setq srcLayerEnt (tblsearch "layer" srcLayer))
	
	;; ดึงค่าสี - จากวัตถุหรือจากเลเยอร์ตามความเหมาะสม
	(setq color 
		(cond
			;; ถ้าสีเป็น ByLayer ให้ดึงจากเลเยอร์
			(is_color_bylayer
				(get_layer_color srcLayer))
			;; ถ้ามีค่า True Color (420) ในวัตถุ
			((assoc 420 srcEnt)
				(true->rgb (cdr (assoc 420 srcEnt))))
			;; ถ้ามีค่า Color Index (62) ในวัตถุ
			((assoc 62 srcEnt)
				(cdr (assoc 62 srcEnt)))
			;; ถ้าไม่มีค่าสีกำหนด
			(T nil)
		)
	)
	
	(princ "\nColor: ")
	(princ color)
	
	;; ดึงค่า linetype - จากวัตถุหรือจากเลเยอร์ตามความเหมาะสม
	(setq ltype 
		(cond
			;; ถ้า linetype เป็น ByLayer ให้ดึงจากเลเยอร์
			(is_linetype_bylayer
				(get_layer_linetype srcLayer))
			;; ถ้ามีค่า linetype ในวัตถุ
			((assoc 6 srcEnt)
				(cdr (assoc 6 srcEnt)))
			;; ถ้าไม่มีค่ากำหนด
			(T "continuous")
		)
	)
	
	(princ "\nLinetype: ")
	(princ ltype)
	
	;; ดึงค่า lineweight - จากวัตถุหรือจากเลเยอร์ตามความเหมาะสม
	(setq lineweight 
		(cond
			;; ถ้า lineweight เป็น ByLayer ให้ดึงจากเลเยอร์
			(is_lineweight_bylayer
				(get_layer_lineweight srcLayer))
			;; ถ้ามีค่า lineweight ในวัตถุ
			((assoc 370 srcEnt)
				(cdr (assoc 370 srcEnt)))
			;; ถ้าไม่มีค่ากำหนด
			(T -1) ;; ByLayer
		)
	)
	
	(princ "\nLineweight: ")
	(princ lineweight)
	
	;; ดึงค่า transparency - จากวัตถุหรือเลเยอร์
	(setq obj_transp (get_transparency src))
	(setq layer_transp (get_layer_transparency srcLayer))
	
	;; ถ้า transparency ของวัตถุไม่ใช่ 0 ให้ใช้ค่าจากวัตถุ
	;; ถ้าไม่ ให้ใช้ค่าจากเลเยอร์
	(setq transp (if (> (atoi obj_transp) 0)
		obj_transp
		layer_transp
	))
	
	(princ "\nTransparency: ")
	(princ transp)
	
	(setq lName (getstring "\nName of the new layer: "))
	
	;; สร้างเลเยอร์ใหม่
	(_create_layer_local 
		lName 
		color 
		ltype 
		1
		lineweight
	)
	
	;; ตั้งค่า transparency ให้กับเลเยอร์ใหม่
	(if (> (atoi transp) 0)
		(set_layer_transparency lName (atoi transp))
	)
	
	;; ตั้งให้เลเยอร์ใหม่เป็นเลเยอร์ปัจจุบัน
	(command "_.layer" "_s" lName "")
	
	;; ย้ายเฉพาะวัตถุที่เลือกไปยังเลเยอร์ใหม่
	(entmod (subst (cons 8 lName) (assoc 8 srcEnt) srcEnt))
	
	;; ตั้งคุณสมบัติของวัตถุให้เป็น ByLayer
	(entmod (subst '(62 . 256) (assoc 62 srcEnt) (entget src)))        ;; สี ByLayer (256)
	(entmod (subst '(6 . "BYLAYER") (assoc 6 srcEnt) (entget src)))     ;; Linetype ByLayer
	(entmod (subst '(370 . -1) (assoc 370 srcEnt) (entget src)))       ;; Lineweight ByLayer
	
	;; ลบค่า transparency ในวัตถุ (ให้ใช้ค่าจากเลเยอร์)
	(entmod (subst '(440 . 0) (assoc 440 srcEnt) (entget src)))
	
	(princ "\nSelected object moved to layer ")
	(princ lName)
	(princ " with ByLayer properties.")
	(princ)
)
