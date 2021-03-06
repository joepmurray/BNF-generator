;; ************************************************************************* 
;; Joe Murray INFO 680 Artificial Intelligence Drexel University 1997
;; ex2.lsp A BNF Grammar Parser 
;; updates:
;; -------------------------------------------------------------------------
;; 2022/04/29  jpmurray  Fixed a couple of bugs - got around to it 25 years later
;; ************************************************************************* 

(setq ans '()) 

;; function to count the number of elements in a list 
(defun num-elements (x) 
	(cond 
		(( null (cdr x)) 1)	;; if cdr of list is null 
					;; list has 1 element 
		(t (+ (num-elements (cdr x)) 1 )) 	;; otherwise add 1 to number 
							;; of elements of the tail of the list 
	)
)

;; function to return the nth element of a list x 
(defun return-nth-element (n x) 
  	(dotimes 
	  	(count (- n 1) (car x))
		(setq x (cdr x)) 
	) 
) 

;; function to return a random element of a list x 
;; uses above functions num-elements and return-nth-element 
;; note: because (random 5) returns 1..4, I add 1 to the parm of the fn 
(defun return-random-element (x) 
    (setf *random-state* (make-random-state t))
  	(return-nth-element (random (+ 1 (num-elements x))) x) 
) 

;; return a legal leafnode from any tree x 
(defun instance (x) 
  	(cond 
		((not (listp x)) x) 					;; if not list, return x 
		((eq (num-elements x) 1) (instance (eval (car x)))) 	;; if single value list, expand 
		(t (instance (return-random-element x))) 		;; if multi-value list return random sublist 
	)
)


(defun utter (x) 
	(cond
		;; if x is null return false 
		((null x) NIL) 

		;; if x is an atom 
		((atom x) 
			(cond 
				;; if x is a bound atom, expand it and throw the 
				;; resulting list back in 
				((boundp x) 
					(setf x (eval x)) ;; expand it 
						(utter x) 
				)

				;; if x is an unbound atom, its a grammar primitive 
				;; so add it to the answer list 
				(t 
					(push x ans) ;; build up the answer 
					x 
				)
			)
		)

		;; if car x is a list 
		((listp (car x)) 
			(utter (return-random-element x)) 
		) 

		;; otherwise if x is a list 
		((listp x) 
			(utter (car x)) 
			(utter (cdr x)) 
		) 
	)
)

;; at this point ans is built up to contain the utterance 
;; however, its in reverse order because push was used, so reverse it 
(defun display-ans () 
	(setq ans (reverse ans)) 
	
	;; print it 
	(print ans) 

	;; clear it for the next run 
	(setq ans '0) 
) 


;; *********************************************************************
;; * a few sample BNF-like grammars 
;; * simple BNF rules:
;; * (setq english2 '((question2) (statement2)) ) means "english2 can be a question2 _OR_ a statement2" 
;; *                 (noun-phrase verb adj noun)  means "noun-phrase THEN a verb THEN and adj THEN a noun, in that order"  
;; *********************************************************************

;; define a simple english grammar - needs work!
(setq english2 '((question2) (statement2)) ) 
(setq statement2 '((statement2 and statement2) (noun-phrase verb noun-phrase) (noun-phrase verb adj noun))) 
(setq question2 '((noun-phrase question-mark) (why does statement2 question-mark))) 
(setq article '((a) (the)))
(setq noun '((dog) (alien) (lion) (boy) (girl) (teacher))) 
(setq noun-phrase '((noun) (article noun)))
(setq verb '((attacks) (fights) (paints) (congratulates) (kisses) (hugs) (applauds))) 
(setq adj '((pretty) (skinny) (twisted) (purple) (mean) (nice) (dirty) (ugly) (huge) (small))) 
(setq question-mark '(?)) 

;; define an example lisp statement 
(setq yy '(a b c d e)) 
(setq lisp-statement '((car-statement) (cdr-statement) (cons-statement))) 
(setq car-statement '(car (somelist)) ) 
(setq cdr-statement '(cdr (somelist)) ) 
(setq cons-statement '(cons (somelist) (somelist)) ) (setq somelist 'yy) 

;; define a valid common lisp number 
(setq numbr '((pos-int) (ratio) (floating-point-number)) ) 
(setq pos-int '((zero) (one) (two) (three) (four) (five) (six) (seven) (eight) (nine)))
(setq integer '((sign pos-int) (pos-int)) ) 
(setq ratio '((sign pos-int over pos-int) (pos-int over pos-int)) ) 
(setq floating-point-number '(pos-int point pos-int) )          
(setq sign '((-) (+)) ) 

;;example run
(utter english2)
(display-ans)