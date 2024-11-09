;; Graphe de l'histoire
(defvar *story* '((initialNode s8 s5 s1)
              (s1 s2 s5)
              (s2 s3)
              (s3 s4 s7 island)
              (s4 s5 s6 s7)
              (s5 s6 s7)
              (s6 s7 )
              (s7 s8 outcome)
              (s8 outcome)
              (island)
              (outcome)
              ))

;; Scène initiale
(defvar *state* '((red-riding-hood (alive t) (place mum-home)) 
               (wolf (alive t) (place wood)) 
               (granny (alive t) (place granny-home))
               (hunter (alive t) (place hunter-home))))
#|
;; Après la transition s0-S5, en s5 l'état du monde sera le suivant :    
? *state* 
((red-riding-hood (alive nil) (place wolf-belly)) 
 (wolf (alive t) (place wood)) 
 (granny (alive t) (place granny-home))
 (hunter (alive t) (place hunter-home)))
;; Après la transition s5-S7, en s7 l'état du monde sera le suivant :    
? *state*
((red-riding-hood (alive t) (place wood)) 
 (wolf (alive nil) (place wood)) 
 (granny (alive t) (place granny-home))
 (hunter (alive t) (place wood)))
 |#
 (defun set-value (person property val state)
  ;;Fonction permettant de mettre à jour la valeur  
  ;;d'une propriété d'un protagoniste dans l'état du monde
  (setf (cadr (assoc property (cdr (assoc person state)))) val)
  )
#| 
? (set-value 'wolf 'alive nil *states*) 
  NIL 
? *state*
  ((RED-RIDING-HOOD (ALIVE T) (PLACE MUM-HOME)) (WOLF (ALIVE NIL) (PLACE WOOD)) 
  (GRANNY (ALIVE T) (PLACE GRANNY-HOME)) (HUNTER (ALIVE T) (PLACE HUNTER-HOME)))
|#

(defun get-value (person property state)
  ;;Fonction permettant d'obtenir la valeur d'une propriété 
  ;;d'un protagoniste depuis l'état du monde"
  (cadr (assoc property (cdr (assoc person state)))))
#|
? (get-value 'granny 'place *state*)
  GRANNY-HOME
|#
;; La fonction apply-effect modifie l'état du monde pour les effets suivants eaten, killed, moved et risen.
(defun apply-effect (effect person value state)
   ;; Fonction qui applique un seul changement de valeur dans l'état du monde après une action
  (format t "~%APPLY EFFECT : ~s ~s ~s" effect person value)
  (cond
  ((or (equal effect 'eaten) (equal effect 'killed) (equal effect 'risen)) (set-value person 'alive value state))
  ((equal effect 'eaten) (set-value person 'place 'wolf-belly state))
  ((equal effect 'moved) (set-value person 'place value state))
  (t (print "Effet donné invalide ! L'effet doit appartenir à l'ensenmble {eaten, killed, moved, risen}."))
  )
)
#|
;; s2 - s3
? (apply-effect 'moved 'wolf 'granny-home *state*)
? *states
((RED-RIDING-HOOD (ALIVE T) (PLACE WOOD)) (WOLF (ALIVE T) (PLACE GRANNY-HOM
(GRANNY (ALIVE T) (PLACE GRANNY-HOME)) (HUNTER (ALIVE T) (PLACE HUNTER-HOME))
;; s2 - s3
? (apply-effect 'eaten 'granny 'nil *state*)
? *states
((RED-RIDING-HOOD (ALIVE T) (PLACE WOOD)) (WOLF (ALIVE T) (PLACE GRANNY-HOM
(GRANNY (ALIVE NIL) (PLACE WOLF-BELLY)) (HUNTER (ALIVE T) (PLACE HUNTER-HOM
;; s3 - s7
? (apply-effect 'moved 'hunter 'granny-home *state*)
? *states
((RED-RIDING-HOOD (ALIVE T) (PLACE WOOD)) (WOLF (ALIVE T) (PLACE GRANNY-HOM
(GRANNY (ALIVE NIL) (PLACE WOLF-BELLY)) (HUNTER (ALIVE T) (PLACE GRANNY-HOME)
;; s3 - S7
? (apply-effect 'killed 'wolf 'nil *state*)
? *states
((RED-RIDING-HOOD (ALIVE T) (PLACE WOOD)) (WOLF (ALIVE NIL) (PLACE GRANNY-H
(GRANNY (ALIVE NIL) (PLACE WOLF-BELLY)) (HUNTER (ALIVE T) (PLACE GRANNY-HOM
;; s3 - s7
? (apply-effect 'risen 'granny 't *state*)
? *states
((RED-RIDING-HOOD (ALIVE T) (PLACE WOOD)) (WOLF (ALIVE NIL) (PLACE GRANNY-H
(GRANNY (ALIVE T) (PLACE WOLF-BELLY)) (HUNTER (ALIVE T) (PLACE GRANNY-HOME)
|#