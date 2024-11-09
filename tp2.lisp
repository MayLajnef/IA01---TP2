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

;; FONCTIONS DE SERVICE 

;; 1
;; La fonction apply-effect modifie l'état du monde pour les effets suivants eaten, killed, moved et risen.
(defun apply-effect (effect person value state)
  ;; Fonction qui applique un changement de valeur dans l'état du monde
  (format t "~%APPLY EFFECT : ~s ~s ~s" effect person value)
  (cond
   ((equal effect 'eaten)
    (set-value person 'alive value state)
    (set-value person 'place 'wolf-belly state))
   ((equal effect 'killed)
    (set-value person 'alive value state))
   ((equal effect 'risen)
    (set-value person 'alive value state))
   ((equal effect 'moved)
    (set-value person 'place value state))
   (t (print "Effet invalide ! L'effet doit être parmi {eaten, killed, moved, risen}."))
   )
)

(apply-effect 'eaten 'granny 'nil *state*)
*state*
#| TESTS
;; s2 - s3
? (apply-effect 'moved 'wolf 'granny-home *state*)
? *state*
((RED-RIDING-HOOD (ALIVE T) (PLACE WOOD)) (WOLF (ALIVE T) (PLACE GRANNY-HOM
(GRANNY (ALIVE T) (PLACE GRANNY-HOME)) (HUNTER (ALIVE T) (PLACE HUNTER-HOME))
;; s2 - s3
? (apply-effect 'eaten 'granny 'nil *state*)
? *state*
((RED-RIDING-HOOD (ALIVE T) (PLACE WOOD)) (WOLF (ALIVE T) (PLACE GRANNY-HOM
(GRANNY (ALIVE NIL) (PLACE WOLF-BELLY)) (HUNTER (ALIVE T) (PLACE HUNTER-HOM
;; s3 - s7
? (apply-effect 'moved 'hunter 'granny-home *state*)
? *state*
((RED-RIDING-HOOD (ALIVE T) (PLACE WOOD)) (WOLF (ALIVE T) (PLACE GRANNY-HOM
(GRANNY (ALIVE NIL) (PLACE WOLF-BELLY)) (HUNTER (ALIVE T) (PLACE GRANNY-HOME)
;; s3 - S7
? (apply-effect 'killed 'wolf 'nil *state*)
? *state*
((RED-RIDING-HOOD (ALIVE T) (PLACE WOOD)) (WOLF (ALIVE NIL) (PLACE GRANNY-H
(GRANNY (ALIVE NIL) (PLACE WOLF-BELLY)) (HUNTER (ALIVE T) (PLACE GRANNY-HOM
;; s3 - s7
? (apply-effect 'risen 'granny 't *state*)
? *state*
((RED-RIDING-HOOD (ALIVE T) (PLACE WOOD)) (WOLF (ALIVE NIL) (PLACE GRANNY-H
(GRANNY (ALIVE T) (PLACE WOLF-BELLY)) (HUNTER (ALIVE T) (PLACE GRANNY-HOME)
|#

;; 2
(defun rules (actor action &optional person cc state)
;; Fonction qui applique les effets des actions ou des dialogues"
  (case action
    ('kill 
      ;; Applique l'effet killed sur "person" et
      (apply-effect 'killed person 'nil state)
      ;; Applique risen et moved sur les personnes mangées si "person" = loup
      (when (equal person 'wolf)
       (when (equal (get-value 'granny 'place state) 'wolf-belly)
         (apply-effect 'risen 'granny 't state)
         (apply-effect 'moved 'granny cc state)
         (format t "~%Chasseur : \"La grand-mère est sauvée !\""))
       (when (equal (get-value 'red-riding-hood 'place state) 'wolf-belly)
         (apply-effect 'risen 'red-riding-hood 't state)
         (apply-effect 'moved 'red-riding-hood cc state)
         (format t "~%Chasseur : \"Le petit chaperon rouge est sauvé !\""))
      (format t "~%Chasseur : \"Le loup ne fera plus de mal à personne !\"")
      )
    )
    ('eat 
      ;; Applique l'effet eaten sur "person" et
      ;; moved sur "person" dans le ventre du loup 

    )
    ('move 
      ;; Applique moved de "actor" dans cc

    )
    ('greet 
      ;; Applique un dialogue
    
    )
    ('give 
      ;; Applique un dialogue

    )
    ('tell 
      #| Applique un dialogue (différent selon les scènes)
      vous pouvez utiliser cc pour les distinguer
      ou faire confiance à votre meilleur ami ChatGPT ou
      autre IA générative pour savoir quoi dire |#

    )
  )
)
(rules 'hunter 'kill 'wolf 'granny-home *state*)
*state*
#| TESTS
:: si *states* = ((RED-RIDING-HOOD (ALIVE T) (PLACE MUM-HOME))
(WOLF (ALIVE T) (PLACE GRANNY-HOME))
(GRANNY (ALIVE T) (PLACE GRANNY-HOME)) (HUNTER (ALIVE T) (PLACE WOOD)))
? (rules 'wolf 'eat 'granny 'granny-home *state*)
? *state*
((RED-RIDING-HOOD (ALIVE T) (PLACE MUM-HOME)) (WOLF (ALIVE T) (PLACE WOOD))
(GRANNY (ALIVE NIL) (PLACE WOLF-BELLY)) (HUNTER (ALIVE T) (PLACE WOOD)))
|#