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
;; Fonction qui applique les effets des actions ou des dialogues
(defun rules (actor action &optional person cc state)
  (case action
    ('kill 
      ;; Applique l'effet killed sur "person" 
      (apply-effect 'killed person 'nil state)
      (when (equal person 'wolf)
        (format t "~%*Le chasseur charge son fusil*")
        (format t "~%Chasseur: \"Cette fois-ci, tu ne feras plus de mal à personne, vilaine bête !\"")
        (format t "~%*BANG !*")
        ;; Applique risen et moved sur les personnes mangées si "person" = loup
        (when (equal (get-value 'granny 'place state) 'wolf-belly)
          (apply-effect 'risen 'granny 't state)
          (apply-effect 'moved 'granny cc state)
          (format t "~%*Le chasseur ouvre le ventre du loup et en sort la grand-mère*")
          (format t "~%Chasseur: \"Dieu soit loué, elle respire encore !\"")
          (format t "~%Grand-mère: \"Oh... Que s'est-il passé ?\""))
        (when (equal (get-value 'red-riding-hood 'place state) 'wolf-belly)
          (apply-effect 'risen 'red-riding-hood 't state)
          (apply-effect 'moved 'red-riding-hood cc state)
          (format t "~%*Le chasseur sort aussi le Petit Chaperon Rouge*")
          (format t "~%Petit Chaperon Rouge: \"Comme il faisait noir là-dedans !\"")
          (format t "~%Chasseur: \"Tout est fini maintenant, vous êtes en sécurité.\""))
    ))
    
    ('eat 
      ;; Applique l'effet eaten sur "person" 
      (apply-effect 'eaten person 'nil state)
      (cond 
        ((equal person 'granny)
         (format t "~%*Le loup frappe à la porte*")
         (format t "~%Loup: *d'une voix douce* \"C'est votre petite-fille !\"")
         (format t "~%*La grand-mère ouvre la porte*")
         (format t "~%Loup: \"GRRRR !\"")
         (format t "~%*En un bond, le loup se jette sur la grand-mère*"))
        ((equal person 'red-riding-hood)
         (format t "~%Loup: \"MAINTENANT, C'EST TON TOUR !\"")
         (format t "~%*Le loup bondit hors du lit*")
         (format t "~%Petit Chaperon Rouge: \"AU SECOURS !\"")
         (format t "~%*Mais il était déjà trop tard...*"))))
    
    ('move 
      ;; Applique moved de "actor" dans cc
      (apply-effect 'moved actor cc state)
      (cond
        ((equal actor 'red-riding-hood)
         (format t "~%*Le Petit Chaperon Rouge sautille gaiement vers ~a*" cc)
         (format t "~%Petit Chaperon Rouge: *chantonne* \"La la la...\""))
        ((equal actor 'wolf)
         (format t "~%*Le loup se faufile silencieusement vers ~a*" cc))
        ((equal actor 'hunter)
         (format t "~%*Le chasseur se dirige prudemment vers ~a*" cc))
        (t (format t "~%~a se déplace vers ~a" actor cc))))
    
    ('greet 
      ;; Applique un dialogue de salutations
      (cond
        ((and (equal actor 'red-riding-hood) (equal person 'wolf))
         (format t "~%Loup: \"Bonjour, ma petite.\"")
         (format t "~%Petit Chaperon Rouge: \"Bonjour, monsieur le Loup.\""))
        ((and (equal actor 'red-riding-hood) (equal person 'granny))
         (format t "~%Petit Chaperon Rouge: \"Bonjour, grand-mère !\""))
        (t (format t "~%~a: \"Bonjour, ~a.\"" actor person))))
    
    ('give 
      ;; Applique un dialogue où le petit chaperon rouge donne un gâteau à sa grand mère
      (cond
        ((and (equal actor 'red-riding-hood) (equal person 'granny))
         (format t "~%Petit Chaperon Rouge: \"Tenez grand-mère, maman vous envoie une galette et un petit pot de confiture.\"")
         (format t "~%Grand-mère: \"Oh, merci ma chérie ! Ta mère est si attentionnée.\""))
        (t (format t "~%~a: \"Voici un présent pour vous, ~a.\"" actor person))))
    
    ('tell 
      #| Applique un dialogue (différent selon les scènes)
      Vous pouvez utiliser cc pour les distinguer ou faire confiance à votre meilleur ami ChatGPT ou autre IA générative pour savoir quoi dire.
      Pour ma part, j'ai utilisé  Claude.ai. |#
      (cond
        ;; Quand le Petit Chaperon Rouge révèle sa destination au loup (s1 -> s2)
        ((and (equal actor 'red-riding-hood) (equal person 'wolf))
         (format t "~%Loup: \"Où vas-tu donc, ma petite ?\"")
         (format t "~%Petit Chaperon Rouge: \"Je vais voir ma grand-mère qui est malade.\"")
         (format t "~%Loup: \"Elle habite loin, ta grand-mère ?\"")
         (format t "~%Petit Chaperon Rouge: \"Oh oui ! C'est la première maison après le moulin, là-bas.\"")
         (format t "~%Loup: \"Eh bien, j'y vais aussi. Je prends ce chemin-ci, prends celui-là, nous verrons qui sera le premier !\""))
        
        ;; Quand le loup trompe la grand-mère (s3)
        ((and (equal actor 'wolf) (equal person 'granny))
         (format t "~%*Toc toc toc*")
         (format t "~%Grand-mère: \"Qui est là ?\"")
         (format t "~%Loup: *imitant la voix du Petit Chaperon Rouge* \"C'est moi, votre petite-fille !\"")
         (format t "~%Grand-mère: \"Tire la chevillette, et la bobinette cherra.\""))
        
        ;; La fameuse scène de dialogue (s4 -> s5)
        ((and (equal actor 'wolf) (equal person 'red-riding-hood))
         (format t "~%Petit Chaperon Rouge: \"Grand-mère, que vous avez de grandes oreilles !\"")
         (format t "~%Loup: \"C'est pour mieux t'écouter, mon enfant.\"")
         (format t "~%Petit Chaperon Rouge: \"Grand-mère, que vous avez de grands yeux !\"")
         (format t "~%Loup: \"C'est pour mieux te voir, mon enfant.\"")
         (format t "~%Petit Chaperon Rouge: \"Grand-mère, que vous avez de grandes dents !\"")
         (format t "~%Loup: \"C'EST POUR MIEUX TE MANGER !\""))
        
        ;; Quand le chasseur arrive (s6 -> s7)
        ((and (equal actor 'hunter) (equal cc 'granny-home))
         (format t "~%*Le chasseur entend des ronflements suspects*")
         (format t "~%Chasseur: \"Tiens, tiens... La mère-grand ne ronfle pas comme ça d'habitude.\""))
        
        ;; Le dénouement heureux (s8 -> outcome)
        ((and (equal actor 'red-riding-hood) (equal person 'granny))
         (format t "~%Petit Chaperon Rouge: \"Oh grand-mère, j'ai eu si peur !\"")
         (format t "~%Grand-mère: \"Nous pouvons remercier le brave chasseur.\"")
         (format t "~%Chasseur: \"Le plus important est que vous soyez saines et sauves.\"")
         (format t "~%Petit Chaperon Rouge: \"Je promets de ne plus jamais parler aux inconnus dans les bois !\""))

        ;; Quand le loup fait la sieste (s6)
        ((equal actor 'wolf)
         (format t "~%*Le loup, le ventre bien rempli, s'allonge sur le lit*")
         (format t "~%Loup: \"Aaaah... Une petite sieste me fera du bien... *RONFLE*\""))

        ;; Dialogue par défaut
        (t (format t "~%*Une conversation s'engage entre ~a et ~a*" actor person))))
    
    (t (format t "~%Erreur : l'action ~a est inconnue." action))))

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