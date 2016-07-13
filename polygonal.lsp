;;=======================================================================================================================
;; programa para poligonal cerrada, version MODIFICADA
;; Fecha 16 ABRIL 2014
;; AGUSTIN IBARS
;; Modificado en 2013
;;=======================================================================================================================

 
(defun ini()
 (setvar "cmdecho" 0)
 (setq olde *ERROR*)
 (defun *ERROR* (msg) (princ "\n") (princ msg)
	(if ff (close ff))
	(setq ff nil)
	 (setvar  "cmdecho" 1)
	 (setq *ERROR* olde)
	 (princ)
 )
)

(defun sele (/ n nent lent)
	(setq num 0)
        (setq dir (getvar "angdir"))
	(while (= num 0)
	(princ "Seleccione puntos siguiendo la Poligonal en sentido ")
        (if (= dir 0 )( princ "horario: \n")(princ "antihorario: \n"))
	(setq conj (ssget))
	(if conj (setq num  (sslength conj)))
	(setq n 0)
	
	(repeat num 
		(setq nent (ssname conj n))
		(setq lent (entget nent))
		(if (/= (cdr (assoc 0 lent)) "POINT")
			(progn (ssdel nent conj) (setq n (- n 1)))
		)
		(setq n (+ n 1))
	) ; end repeat

	(if conj (setq num (sslength conj))(setq num 0))
	)
	
)


(defun pro1(/) 

		(setq n 0 )
		(setq  ptos nil )
	
		
	(while (> num n)
		(setq p (entget (ssname conj n)))
		(setq xy (cdr (assoc 10 p)))
		(setq xy (trans xy 0 1))
		(setq ptos (append ptos (list xy)))
		(setq n (+ n 1) )
	)
)

(defun pro2(/)
	(textscr)
	(setq n 0 sx 0 sy 0 sl 0 )
	(setq num (length ptos))
	(if (/= ff nil) (fheader) (header))
	(while (> num n)
	(setq i (1- n)) (if (< i 0) (setq i (- num 1)))
	(setq pt0 (nth i ptos))
	(setq pt1 (nth  n ptos))
	(setq i (1+ n)) (if (>= i num) (setq i 0)) 
	(setq pt2 (nth i ptos))
	(setq x1 (car pt1))
	(setq y1 (cadr pt1))
	(setq x2 (car pt2))
	(setq y2 (cadr pt2))
	(setq dx1 (- x2 x1) dy1 (- y2 y1))
	(setq sx (+ sx dx1) sy (+ sy dy1))
	(setq l1 (distance pt1 pt2))
	(setq sl (+ sl l1))
	(setq a0 (+ pi (angle pt0 pt1)))
	(setq a1 (angle pt1 pt2))
	(setq ai  (- a1 a0))
	(SETQ A3 (* 2.5  PI))
	(SETQ A2 (- A3  A1)) ; ------ azimut
	
	(if (AND (> DX1 0) (> DY1 0)) ;----------------------- 1�
		( PROGN (SETQ R1  A2)
			 (SETQ RUMBO (STRCAT "N " (ANGTOS R1 1 4) " E")))
		())
		
	(if (AND (> DX1 0) (< DY1 0)) ;----------------------- 2�
		(PROGN (SETQ R1 (- PI A2)) (SETQ RUMBO (STRCAT "S " (ANGTOS R1 1 4) " E")))	
		() )


	(if (AND (< DX1 0) (< DY1 0)) ;----------------------- 3�
		(PROGN (SETQ R1 (- A2 PI ))  (SETQ RUMBO (STRCAT "S " (ANGTOS R1 1 4) " O")))	
		())
		

	(if (AND (< DX1 0) (> DY1 0)) ;----------------------- 4�
		(PROGN (SETQ R1 (- (* 2 PI) A2)) (SETQ RUMBO (STRCAT "N " (ANGTOS R1 1 4) " O")))	
		())
		


	(if (/= ff nil) (fimpre) (progn (impre)(prompt "Pulse una tecla:")(grread)))
	(setq n (1+ n))	
	)
	(setq t (* (sqrt(+ (* 1.5 sl) (* 0.003 (expt sl 2)))) 0.02))
	(setq fl (lado sx sy))
	(if (/= ff nil) (ffooter) (footer))
)


(defun impre(/)
	
	(write-line "\n" )
	(write-line (strcat  (itoa (1+ n)) (panta " " 12)  (panta (angtos ai 1 4) 11)
	(panta (rtos (SIN a1) 2 6) 11) (panta (rtos Y1 2 2) 15) (panta (rtos X1 2 2) 15)
	(panta (rtos (COS a1) 2 6) 11)  )
	)
	(setq i (+ n 2)) (if (> i num) (setq i 1)) 
	(write-line (strcat (itoa (1+ n)) "-" (itoa i) (panta (rtos l1 2 2) 10) 
	(panta (angtos A2 1 4) 11) (panta (rtos dY1 2 2) 11) (panta (rtos dX1 2 2) 41))
	)
	(write-line (strcat  " " (panta " " 5)  (panta RUMBO 20))	
	)


)



(defun fimpre(/)
	
	(write-line (strcat "\n" margen ) ff )
	(write-line (strcat margen (itoa (1+ n)) (panta " " 12)  (panta (angtos ai 1 4) 11)
	(panta (rtos (SIN a1) 2 6) 11) (panta (rtos Y1 2 2) 15) (panta (rtos X1 2 2) 15)
	(panta (rtos (COS a1) 2 6) 11))
	 ff)
	(setq i (+ n 2)) (if (> i num) (setq i 1)) 
	(write-line (strcat margen (itoa (1+ n)) "-" (itoa i) (panta (rtos l1 2 2) 10) 
	(panta (angtos A2 1 4) 11) (panta (rtos dY1 2 2) 11) (panta (rtos dX1 2 2) 41))
	 ff)
	(write-line (strcat "        " (panta " " 5)  (panta RUMBO 20))	
	ff)


)

(defun panta (cad p)
	
	(while (< (strlen cad) p)
	(setq cad (strcat " " cad))
	)
)

(defun lado (dx dy)
	(sqrt (+ (* dx dx) (* dy dy)))
)



(defun c:poli() ; c: define a la funcion como comando de autocad
	
	(ini)
	(sele)
	(pro1)
	(dibuja)
	(command "_area" "_e" "_l") 
	(setq sup (getvar "area"))
	(command "_U" "_U")
	(setq margen "       ")
	(setq rpta "Repetir")
	(while (eq rpta "Repetir")
		(initget 1 "Repetir Salir Grabar")
		(pro2)
		(if (/= ff nil) (close ff))
		(setq rpta (getkword "Repetir/Grabar/Salir ? "))
		(if (eq rpta "Grabar") (salvar)
			
		(setq ff nil)
		)
	)

(princ)	
)

(defun salvar()
(if tit (prompt (strcat "Ingrese titulo  <Enter>: [" tit "] \n"))
	(prompt "Ingrese titulo:\n"))
(setq tmp (read-line))
(if (/= tmp "") (setq tit tmp))
(setq ma (getstring "Manzana ?: "))
(setq pa (getstring "Parcela ?: "))
(setq arch (strcat (substr ma 1 4) (substr pa 1 4 ) ".txt"))
(if (setq arch2 (findfile arch)) (existe) (noexiste))
)

(defun existe()
(initget 1 "Si No")
(prompt "El archivo ya existe, lo sobreescribe? <Si/No>:")
(if (= (getkword) "Si") (noexiste) )
)

(defun noexiste()
	(if (setq ff (open arch "w") )
	
	(progn (close ff) (setq arch2 (findfile arch))(setq ff (open arch "w"))
		(setq rpta "Repetir") (prompt (strcat "Grabando " arch2 "\n"))
	)

	(prompt "No se puede abrir archivo.")
	)
)
(defun dibuja()
 	(command "_pline" (nth 0 ptos))
	(setq n 1)
	(while (< n  num )
	(command (nth n ptos))
	(setq n (+ n 1)))
	(command "c" "")
)

(defun header()

(write-line "\n==============================================================================" )
(write-line "Vertice         Angulo      Sen.Ang.                                  Cos.Ang." )
(write-line "Lado   Lado     Azimut       +/-DN      Norte              Este        +/-DE  " )
(write-line "                Rumbo                                                         " )
(write-line "==============================================================================" )	
	)

(defun fheader()
(setq z (/ (- 73 (strlen tit)) 2));---------66
(setq mt (panta " " z))
(write-line (strcat margen mt tit "\n") ff)
(write-line (strcat margen "Manzana: " ma "\t" "Parcela: " pa) ff)
(write-line (strcat margen "==============================================================================") ff)
(write-line (strcat margen "Vertice         Angulo      Sen.Ang.                                  Cos.Ang.") ff)
(write-line (strcat margen "Lado     Lado   Azimut       +/-DN          Norte         Este         +/-DE  ") ff)
(write-line (strcat margen "                Rumbo                                                         ") ff)
(write-line (strcat margen "==============================================================================") ff)	
	)

(defun footer()
	(write-line        "\n=========================================================================\n" )
	
	(write-line (strcat "\tSum.lados:" (panta (rtos sl 2 2) 10) "\n")) ;--------

	(write-line (strcat  "Ex:\t" (panta (rtos sx 2 4) 10)))
		
	(write-line (strcat  "Ey:\t" (panta (rtos sy 2 4) 10)
	"\tSuperficie:\t" (panta (rtos sup 2 2) 20)))	;---------------			
		
 	(write-line (strcat  "Et:  \t"  (panta (rtos fl 2 4 ) 10)))
	(princ)
)

(defun ffooter()
	(write-line (strcat "\n" margen "==============================================================================\n") ff)
	
	(write-line (strcat margen "\tSum.lados:" (panta (rtos sl 2 2) 10) "\n") ff)

;	 "\tTolerancia:" (panta (rtos t 2 4) 10) "\n") ff)

	(write-line (strcat margen "Ex:\t" (panta (rtos sx 2 4) 10)) ff)
		
	(write-line (strcat margen "Ey:\t" (panta (rtos sy 2 4) 10)
	"\tSuperficie:\t" (panta (rtos sup 2 2) 20)) ff)	
		
 	(write-line (strcat margen "Et:\t"  (panta (rtos fl 2 4 ) 10)) ff)
	
)
(princ "Comando a invocar: POLI\n")(princ)
