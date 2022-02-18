(defun aplicar(lstRegla restoList)
    
    	(prog (variableRegla listaADevolver regla)
		(setf listaADevolver '() )
		(setf concatenado '() )
		(setf regla (first lstRegla))
		(setf variableRegla (second regla))
		
		(loop for aSustituir in restoList do
			(setf concatenado aSustituir)

			(if (listp aSustituir)
			    (when (not (equalp (first aSustituir) '?))
						(setf concatenado (aplicar lstRegla aSustituir))
				)
			)
			(when (equalp aSustituir variableRegla)
				(setf concatenado (first regla))
			)
			(setf listaADevolver (append listaADevolver (list concatenado)))
		)
	(return-from aplicar listaADevolver)
	)
)


(defun componerReglas (lstZ1 lstZ2)

    (when (not lstZ1)
        (return-from componerReglas lstZ2)
    )
    (when (not lstZ2)
        (return-from componerReglas lstZ1)
    )
    
	(prog (numZ1 denZ1 numZ2 denZ2)

		(setf numZ1 (first(first lstz1)))
		(setf denZ1 (second (first lstZ1)))
		(setf sustituto '())
		(setf reglaModificada '())
		(setf resultadoConcatenado '())

		(loop for subListZ2 in lstZ2 do

			;(A (? y))
			(when (and (atom (first subListZ2)  ))

				(setf numZ2 (list(first subListZ2)))
				(setf denZ2 (second subListZ2))

				(when (equalp numZ1 denZ2)
					
					(setf sustituto numZ2)
					(setf reglaModificada (append sustituto (list denZ1) ))
					(setf resultadoConcatenado (append (list reglaModificada) lstZ2))
				)
				
				(when (and (listp (first (first lstz1))) (not(equalp (first(first (first lstz1))) '?) ))
	                (setf denZ2 (second subListZ2))
	                (setf numZ2 (list(first subListZ2)))
	                (setf resultadoConcatenado (list(first(first (first lstZ1)))))
	                (setf listaVariables (rest(first (first lstZ1))))

	                (loop for variable in listaVariables do

	                    (when (equalp variable denZ2)
	                	    (setf  resultadoConcatenado (append resultadoConcatenado numZ2))
	                	    
	                	    (setf resultadoConcatenado (append (list resultadoConcatenado) (list denZ1)))
	                        (setf resultadoConcatenado (append (list resultadoConcatenado) lstZ2))
	                        (return-from componerReglas  resultadoConcatenado)
	                    )
	    
	                    (when (not(equalp variable denZ2))
	    	                (setf resultadoConcatenado (append (list resultadoConcatenado) (list variable)))
		                )
	               )

	                (setf resultadoConcatenado (append (list resultadoConcatenado) (list denZ1)))
	                (setf resultadoConcatenado (append (list resultadoConcatenado) lstZ2))
            )
				
		)
			;((? x)(? y))
			(when (and (listp (first subListZ2)) (equalp (first(first subListZ2)) '?) )
				(setf numZ2 (list(first subListZ2)))
				(setf denZ2 (second subListZ2))

				(when (equalp numZ1 denZ2)
					
					(setf sustituto numZ2)
					(setf reglaModificada (append sustituto (list denZ1) ))
					(setf resultadoConcatenado (append (list reglaModificada) lstZ2))

                    (return-from componerReglas resultadoConcatenado)
				)									
			)
		)
		(when (not resultadoConcatenado)
		    (setf resultadoConcatenado (append lstz1 lstZ2))
		    (return-from componerReglas  resultadoConcatenado)
		)
	)
(return-from componerReglas resultadoConcatenado)
)

(defun esLista(lista)

	(if (and (listp lista) (not (atomo lista)))
	T
	NIL
	)
)

(defun atomo(var)
    (cond
        ((atom var) T)
        ((eq (first var) '?) T)
        (T NIL)
    )
)

(defun esVariable(variable)

	(if (and (listp variable) (atomo variable))
	T
	NIL
	)
)

;busca var en list recursivamente.
(defun aparece (var list)
    (when (esLista list)
        (setf encontrado NIL) 
        (loop for elemento in list do 
            (prog ()
                (if (atomo elemento) 
                    (when (equal elemento var) 
                        (write-line "aaaaaaaaa")
                        (return-from aparece T)
                    )
                    (setf encontrado (aparece var elemento))  
                )
                (when (equal encontrado T) 
                    (return-from aparece T)
                )
            )
        )
    )
    (return-from aparece NIL) ; no se ha encontrado o habria salido antes O list no es una lista
)

(defun unificar(e1 e2)

	(when (and (esLista e1) (= 1 (length e1)))
        (setf e1 (first e1))
    )
    (when (and (esLista e2) (= 1 (length e2)))
        (setf e2 (first e2))
    )

    (when (and (not (atomo e1)) (atomo e2))
        (setf tmp e1)
        (setf e1 e2)
        (setf e2 tmp)
    )
    (when (atomo e1)
        (when (equal e1 e2)
            (return-from unificar '())
        )
        (when (esVariable e1)
            (if (aparece e1 e2)
                (return-from unificar 'FALLOUNIF)
                (return-from unificar (list (list e2 e1)))
            )
            
        )
        (when (esVariable e2)
            (return-from unificar (list (list e1 e2)))
        )
        (return-from unificar 'FALLOUNIF)
     )


    (prog (F1 F2 T1 T2 G1 G2 Z1 Z2)
    	(setf listaResultado '())
    	
         (setf F1 (first e1))
         (setf T1 (rest e1))
         (setf F2 (first e2))
         (setf T2 (rest e2))

         (setf Z1 (unificar F1 F2))
         
         (when (equal Z1 'FALLOUNIF)
             (return-from unificar 'FALLOUNIF)
         )
         
         (setf G1 (aplicar Z1 T1))
         (setf G2 (aplicar Z1 T2))
         (setf Z2 (unificar G1 G2))
         
         (when (equal Z2 'FALLOUNIF)
             (return-from unificar 'FALLOUNIF)
         )
         
         (setf listaResultado (append Z1 Z2))
         (return-from unificar (componerReglas Z1 Z2))
    )
)

