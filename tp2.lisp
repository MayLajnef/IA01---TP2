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
#| TESTS
;; s2 - s3
? (apply-effect 'moved 'wolf 'granny-home *state*)
? *state*
((RED-RIDING-HOOD (ALIVE T) (PLACE WOOD)) (WOLF (ALIVE T) (PLACE GRANNY-HOM
(GRANNY (ALIVE T) (PLACE GRANNY-HOME)) (HUNTER (ALIVE T) (PLACE HUNTER-HOME))
;; s2 - s3
? (apply-effect 'eaten 'granny 'nil *state*)
? *state*((RED-RIDING-HOOD (ALIVE T) (PLACE WOOD)) (WOLF (ALIVE T) (PLACE GRANNY-HOM
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
        (when (equal (get-value 'red-riding-hood 'place state) 'wolf-belly)
          (apply-effect 'risen 'red-riding-hood 't state)
          (apply-effect 'moved 'red-riding-hood cc state)
          (format t "~%*Le chasseur ouvre le ventre du loup et en sort le Petit Chaperon Rouge*")
          (format t "~%Petit Chaperon Rouge: \"Comme il faisait noir là-dedans !\"")
          (format t "~%Chasseur: \"N'ayez pas peur, vous êtes en sécurité.\""))
        (when (equal (get-value 'granny 'place state) 'wolf-belly)
          (apply-effect 'risen 'granny 't state)
          (apply-effect 'moved 'granny cc state)
          (format t "~%*Le chasseur sort aussi la grand-mère du ventre de la bête*")
          (format t "~%Chasseur: \"Dieu soit loué, elle respire encore !\"")
          (format t "~%Grand-mère: \"Oh... Que s'est-il passé ?\"")
          (format t "~%Chasseur: \"Ne craignez rien, tout est fini maintenant, la vilaine bête n'est plus de ce monde.\""))
    ))
    
    ('eat 
      ;; Applique l'effet eaten sur "person" 
      (apply-effect 'eaten person 'nil state)
      (cond 
       ((equal person 'granny)
        (format t "~%*La grand-mère ouvre la porte*")
        (format t "~%Loup: \"GRRRR !\"")
        (format t "~%*En un bond, le loup se jette sur la grand-mère*"))
        ;; Cas où le loup mange le Petit Chaperon Rouge dans la maison de mère-grand
       ((and (equal person 'red-riding-hood) (equal cc 'granny-home))
        (format t "~%Loup: \"MAINTENANT, C'EST TON TOUR !\"")
        (format t "~%*Le loup bondit hors du lit*")
        (format t "~%Petit Chaperon Rouge: \"AU SECOURS !\"")
        (format t "~%*Mais il était déjà trop tard...*"))
       ;; Cas où le loup mange le Petit Chaperon Rouge dans la forêt
       ((and (equal person 'red-riding-hood) (equal cc 'wood))
        (format t "~%*Le loup voyant que la petite fille était sans défense s'empressa de la dévorer.*")
        (format t "~%Petit Chaperon Rouge: \"Non !\"")
        (format t "~%*Le silence retombe dans la forêt...")
        (format t "~%Loup: \"Voilà un mets bien jeune et bien tendre, un vrai régal ! \""))))
    ('move 
      ;; Applique moved de "actor" dans cc
      (apply-effect 'moved actor cc state)
      (cond
        ((equal actor 'red-riding-hood) 
         (when (equal cc 'granny-home) (format t "~%*Le Petit Chaperon Rouge sautille gaiement vers la maison de mère-grand*"))
         (when (equal cc 'granny-home) (format t "~%*D'humeur aventureuse, le Petit Chaperon Rouge passe par les bois pour se rendre chez mère-grand.*"))
         (format t "~%Petit Chaperon Rouge: *chantonne* \"La la la...\""))
        ((equal actor 'wolf)
         (when (equal cc 'granny-home) (format t "~%*Le loup se faufile silencieusement vers dans la maison de mère-grand*"))
         (when (equal cc 'wood) (format t "~%*Le loup sortit de la maison de mère-grand pour aller faire une sieste dans les bois.*")))
        ((equal actor 'hunter) 
         (when (equal cc 'granny-home) (format t "~%*Le chasseur se dirige prudemment vers la maison de mère-grand.*"))
         (when (equal cc 'wood) (format t "~%*Le chasseur se rend comme à son habitude aux bois pour chasser du gibier.*")))
        ; Dialogue par défaut pour décrire un déplacement
        (t (format t "~%~a se déplace vers ~a" actor cc))
      )
    ) 
    ('greet 
      ;; Applique un dialogue de salutations
      (cond
        ((and (equal actor 'red-riding-hood) (equal person 'wolf))
         (format t "~%Loup: \"Bonjour, ma petite.\"")
         (format t "~%Petit Chaperon Rouge: \"Bonjour, monsieur le Loup.\""))
        ((and (equal actor 'red-riding-hood) (equal person 'granny))
         (format t "~%Petit Chaperon Rouge: \"Bonjour, grand-mère !\"")
         (format t "~%Grand-mère: \"Bonjour, ma chérie !\"")
         )
        ; Dialogue par défaut pour décrire des salutations
        (t (format t "~%~a: \"Bonjour, ~a.\"" actor person))))
    
    ('give 
      ;; Applique un dialogue où le petit chaperon rouge donne un gâteau à sa grand mère
      (cond
        ((and (equal actor 'red-riding-hood) (equal person 'granny))
         (format t "~%Petit Chaperon Rouge: \"Tenez grand-mère, maman vous envoie une galette et un petit pot de confiture.\"")
         (format t "~%Grand-mère: \"Oh, merci ma chérie ! Ta mère est si attentionnée.\""))
        ; Dialogue par défaut
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
        
        ;; Quand le loup entre dans la maison de la grand mère et lui tend un piège (s3)
        ((and (equal actor 'wolf) (equal person 'granny))
         (format t "~%*Toc toc toc*")
         (format t "~%Grand-mère: \"Qui est là ?\"")
         (format t "~%Loup: *imitant la voix du Petit Chaperon Rouge* \"C'est moi, votre petite-fille !\""))
        
        ;; La fameuse scène de dialogue où le loup déguisé en mère-grand tend un piège au petit chaperon rouge (s4 -> s5)
        ((and (equal actor 'wolf) (equal person 'red-riding-hood) (equal cc 'granny-home))
         (format t "~%Petit Chaperon Rouge: \"Grand-mère, que vous avez de grandes oreilles !\"")
         (format t "~%Loup: \"C'est pour mieux t'écouter, mon enfant.\"")
         (format t "~%Petit Chaperon Rouge: \"Grand-mère, que vous avez de grands yeux !\"")
         (format t "~%Loup: \"C'est pour mieux te voir, mon enfant.\"")
         (format t "~%Petit Chaperon Rouge: \"Grand-mère, que vous avez de grandes dents !\"")
         (format t "~%Loup: \"C'EST POUR MIEUX TE MANGER !\""))
        
        ;; Quand le chasseur tombe sur le loup en train de faire sa sieste dans les bois (s6 -> s7) ou dans la maison de mère-grand (s3 -> s7 ou s3 -> s4 -> s7 )
        ((and (equal actor 'hunter) (equal person 'wolf))
         (format t "~%*Le chasseur entend des ronflements suspects*")
         (when (equal cc 'wood) 
          (format t "~%Chasseur: \"Tiens, tiens... La chasse s'annonce fructueuse aujourd'hui...\"")
          (format t "~%*Il s'avance prudemment, et soudain il aperçut un loup couché au pied d'un arbre.*"))
         (when (equal cc 'granny-home) 
          (format t "~%Chasseur: \"Tiens, tiens... La mère-grand ne ronfle pas comme ça d'habitude.\"")
          (format t "~%*Il entre dans la chambre. Quand il arrive devant le lit, il voit que c'est un loup qui y est couché.*"))
         (format t "~%Chasseur: \"Ah ! C’est toi, bandit ! Voilà bien longtemps que je te cherche...\""))
        ;; Le dénouement heureux (s8 -> outcome)
        ((and (equal actor 'red-riding-hood) (equal person 'granny))
         (format t "~%Petit Chaperon Rouge: \"Oh grand-mère, j'ai eu si peur !\"")
         (format t "~%Grand-mère: \"Nous pouvons remercier le brave chasseur.\"")
         (format t "~%Chasseur: \"Le plus important est que vous soyez saines et sauves.\"")
         (format t "~%Petit Chaperon Rouge: \"Je promets de ne plus jamais parler aux inconnus dans les bois !\""))

        ;; Quand le loup fait la sieste (s6)
        ((equal actor 'wolf)
         (when (equal cc 'wood) (format t "~%*Le loup, le ventre bien rempli, s'allonge au pied d'un arbre*"))
         (when (equal cc 'granny-home) (format t "~%*Le loup, le ventre bien rempli, s'allonge sur le lit*"))
         (format t "~%Loup: \"Aaaah... Une petite sieste me fera du bien... *RONFLE*\""))

        ;; Dialogue par défaut
        (t (format t "~%*Une conversation s'engage entre ~a et ~a dans le lieu ~a.*" actor person cc))))
    
    (t (format t "~%Erreur : l'action ~a est inconnue." action))))
*state*
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
;; Graphe de l'histoire dans la partie créative à enrichir 
(defvar *creative_story* '((initialNode s1 s9)
              (s1 s2 s3)
              (s2 s5)
              (s3 s4 s6)
              (s4 s6 outcome)
              (s5 s8 s10 island)
              (s6 s7 s10)
              (s7 s10 outcome)
              (s8 s9 s11)
              (s9 s10 island)
              (s10 s11 island)
              (s11 s12 outcome)
              (s12 outcome)
              (island)
              (outcome)
              ))

;; Scène initiale
(defvar *state* '((red-riding-hood (alive t) (place mum-home)) 
               (wolf (alive t) (place wood)) 
               (granny (alive t) (place granny-home))
               (hunter (alive t) (place hunter-home)))
)

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
  (cadr (assoc property (cdr (assoc person state))))
)
#|
? (get-value 'granny 'place *state*)
  GRANNY-HOME
|#

;; FONCTIONS DE SERVICE 

;; 1. La fonction apply-effect modifie l'état du monde pour les effets suivants eaten, killed, moved et risen.
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

;;(apply-effect 'eaten 'granny 'nil *state*)
;;(apply-effect 'eaten 'red-riding-hood 'nil *state*)
;;(apply-effect 'moved 'hunter 'granny-home *state*)
;;*state*

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
    (kill 
      ;; Applique l'effet killed sur "person" 
      (apply-effect 'killed person 'nil state)
      (when (equal person 'wolf)
        (format t "~%*Le chasseur charge son fusil*")
        (format t "~%Chasseur: \"Cette fois-ci, tu ne feras plus de mal à personne, vilaine bête !\"")
        (format t "~%*BANG !*")
        ;; Applique risen et moved sur les personnes mangées si "person" = loup
        (when (equal (get-value 'red-riding-hood 'place state) 'wolf-belly)
          (apply-effect 'risen 'red-riding-hood 't state)
          (apply-effect 'moved 'red-riding-hood cc state)
          (format t "~%*Le chasseur ouvre le ventre du loup et en sort le Petit Chaperon Rouge*")
          (format t "~%Petit Chaperon Rouge: \"Comme il faisait noir là-dedans !\"")
          (format t "~%Chasseur: \"N'ayez pas peur, vous êtes en sécurité.\""))
        (when (equal (get-value 'granny 'place state) 'wolf-belly)
          (apply-effect 'risen 'granny 't state)
          (apply-effect 'moved 'granny cc state)
          (format t "~%*Le chasseur sort aussi la grand-mère du ventre de la bête*")
          (format t "~%Chasseur: \"Dieu soit loué, elle respire encore !\"")
          (format t "~%Grand-mère: \"Oh... Que s'est-il passé ?\"")
          (format t "~%Chasseur: \"Ne craignez rien, tout est fini maintenant, la vilaine bête n'est plus de ce monde.\""))
      )
    )
    
    (eat 
      ;; Applique l'effet eaten sur "person" 
      (apply-effect 'eaten person 'nil state)
      (cond 
       ((equal person 'granny)
        (format t "~%*La grand-mère ouvre la porte*")
        (format t "~%Loup: \"GRRRR !\"")
        (format t "~%*En un bond, le loup se jette sur la grand-mère*"))
        ;; Cas où le loup mange le Petit Chaperon Rouge dans la maison de mère-grand
       ((and (equal person 'red-riding-hood) (equal cc 'granny-home))
        (format t "~%Loup: \"MAINTENANT, C'EST TON TOUR !\"")
        (format t "~%*Le loup bondit hors du lit*")
        (format t "~%Petit Chaperon Rouge: \"AU SECOURS !\"")
        (format t "~%*Mais il était déjà trop tard...*"))
       ;; Cas où le loup mange le Petit Chaperon Rouge dans la forêt
       ((and (equal person 'red-riding-hood) (equal cc 'wood))
        (format t "~%*Le loup voyant que la petite fille était sans défense s'empressa de la dévorer.*")
        (format t "~%Petit Chaperon Rouge: \"Non !\"")
        (format t "~%*Le silence retombe dans la forêt...")
        (format t "~%Loup: \"Voilà un mets bien jeune et bien tendre, un vrai régal ! \""))
      )
    )

    (move 
      ;; Applique moved de "actor" dans cc
      (apply-effect 'moved actor cc state)
      (cond
        ((equal actor 'red-riding-hood) 
         (when (equal cc 'granny-home) (format t "~%*Le Petit Chaperon Rouge sautille gaiement vers la maison de mère-grand*"))
         (when (equal cc 'granny-home) (format t "~%*D'humeur aventureuse, le Petit Chaperon Rouge passe par les bois pour se rendre chez mère-grand.*"))
         (format t "~%Petit Chaperon Rouge: *chantonne* \"La la la...\""))
        ((equal actor 'wolf)
         (when (equal cc 'granny-home) (format t "~%*Le loup se faufile silencieusement vers dans la maison de mère-grand*"))
         (when (equal cc 'wood) (format t "~%*Le loup sortit de la maison de mère-grand pour aller faire une sieste dans les bois.*")))
        ((equal actor 'hunter) 
         (when (equal cc 'granny-home) (format t "~%*Le chasseur se dirige prudemment vers la maison de mère-grand.*"))
         (when (equal cc 'wood) (format t "~%*Le chasseur se rend comme à son habitude aux bois pour chasser du gibier.*")))
        ; Dialogue par défaut pour décrire un déplacement
        (t (format t "~%~a se déplace vers ~a" actor cc))
      )
    )

    (greet 
      ;; Applique un dialogue de salutations
      (cond
        ((and (equal actor 'red-riding-hood) (equal person 'wolf))
         (format t "~%Loup: \"Bonjour, ma petite.\"")
         (format t "~%Petit Chaperon Rouge: \"Bonjour, monsieur le Loup.\""))
        ((and (equal actor 'red-riding-hood) (equal person 'granny))
         (format t "~%Petit Chaperon Rouge: \"Bonjour, grand-mère !\"")
         (format t "~%Grand-mère: \"Bonjour, ma chérie !\"")
         )
        ; Dialogue par défaut pour décrire des salutations
        (t (format t "~%~a: \"Bonjour, ~a.\"" actor person))))
    
    ('give 
      ;; Applique un dialogue où le petit chaperon rouge donne un gâteau à sa grand mère
      (cond
        ((and (equal actor 'red-riding-hood) (equal person 'granny))
         (format t "~%Petit Chaperon Rouge: \"Tenez grand-mère, maman vous envoie une galette et un petit pot de confiture.\"")
         (format t "~%Grand-mère: \"Oh, merci ma chérie ! Ta mère est si attentionnée.\""))
        ; Dialogue par défaut
        (t (format t "~%~a: \"Voici un présent pour vous, ~a.\"" actor person))))
    
    (tell 
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
        
        ;; Quand le loup entre dans la maison de la grand mère et lui tend un piège (s3)
        ((and (equal actor 'wolf) (equal person 'granny))
         (format t "~%*Toc toc toc*")
         (format t "~%Grand-mère: \"Qui est là ?\"")
         (format t "~%Loup: *imitant la voix du Petit Chaperon Rouge* \"C'est moi, votre petite-fille !\""))
        
        ;; La fameuse scène de dialogue où le loup déguisé en mère-grand tend un piège au petit chaperon rouge (s4 -> s5)
        ((and (equal actor 'wolf) (equal person 'red-riding-hood) (equal cc 'granny-home))
         (format t "~%Petit Chaperon Rouge: \"Grand-mère, que vous avez de grandes oreilles !\"")
         (format t "~%Loup: \"C'est pour mieux t'écouter, mon enfant.\"")
         (format t "~%Petit Chaperon Rouge: \"Grand-mère, que vous avez de grands yeux !\"")
         (format t "~%Loup: \"C'est pour mieux te voir, mon enfant.\"")
         (format t "~%Petit Chaperon Rouge: \"Grand-mère, que vous avez de grandes dents !\"")
         (format t "~%Loup: \"C'EST POUR MIEUX TE MANGER !\""))
        
        ;; Quand le chasseur tombe sur le loup en train de faire sa sieste dans les bois (s6 -> s7) ou dans la maison de mère-grand (s3 -> s7 ou s3 -> s4 -> s7 )
        ((and (equal actor 'hunter) (equal person 'wolf))
         (format t "~%*Le chasseur entend des ronflements suspects*")
         (when (equal cc 'wood) 
          (format t "~%Chasseur: \"Tiens, tiens... La chasse s'annonce fructueuse aujourd'hui...\"")
          (format t "~%*Il s'avance prudemment, et soudain il aperçut un loup couché au pied d'un arbre.*"))
         (when (equal cc 'granny-home) 
          (format t "~%Chasseur: \"Tiens, tiens... La mère-grand ne ronfle pas comme ça d'habitude.\"")
          (format t "~%*Il entre dans la chambre. Quand il arrive devant le lit, il voit que c'est un loup qui y est couché.*"))
         (format t "~%Chasseur: \"Ah ! C’est toi, bandit ! Voilà bien longtemps que je te cherche...\""))
        ;; Le dénouement heureux (s8 -> outcome)
        ((and (equal actor 'red-riding-hood) (equal person 'granny))
         (format t "~%Petit Chaperon Rouge: \"Oh grand-mère, j'ai eu si peur !\"")
         (format t "~%Grand-mère: \"Nous pouvons remercier le brave chasseur.\"")
         (format t "~%Chasseur: \"Le plus important est que vous soyez saines et sauves.\"")
         (format t "~%Petit Chaperon Rouge: \"Je promets de ne plus jamais parler aux inconnus dans les bois !\""))

        ;; Quand le loup fait la sieste (s6)
        ((equal actor 'wolf)
         (when (equal cc 'wood) (format t "~%*Le loup, le ventre bien rempli, s'allonge au pied d'un arbre*"))
         (when (equal cc 'granny-home) (format t "~%*Le loup, le ventre bien rempli, s'allonge sur le lit*"))
         (format t "~%Loup: \"Aaaah... Une petite sieste me fera du bien... *RONFLE*\""))

        ;; Dialogue par défaut
        (t (format t "~%*Une conversation s'engage entre ~a et ~a dans le lieu ~a.*" actor person cc))))
    
    (t (format t "~%Erreur : l'action ~a est inconnue." action))
  )
)

#| TESTS
:: si *states* = ((RED-RIDING-HOOD (ALIVE T) (PLACE MUM-HOME))
(WOLF (ALIVE T) (PLACE GRANNY-HOME))
(GRANNY (ALIVE T) (PLACE GRANNY-HOME)) (HUNTER (ALIVE T) (PLACE WOOD)))
? (rules 'wolf 'eat 'granny 'granny-home *state*)
? *state*
((RED-RIDING-HOOD (ALIVE T) (PLACE MUM-HOME)) (WOLF (ALIVE T) (PLACE WOOD))
(GRANNY (ALIVE NIL) (PLACE WOLF-BELLY)) (HUNTER (ALIVE T) (PLACE WOOD)))
|#

;; Cette fonction s'occupe des changements de scène, d'y appliquer les rules + moves nécessaires 
(defun apply-change-scene (change state)
  (case change
    ;; Scénarios spécifiques
    ((initialNode s1)
     (progn
       (rules 'red-riding-hood 'move nil 'wood state)
       (rules 'wolf 'kill 'red-riding-hood 'wood state)))

    ((initialNode s5)
     (progn
       (rules 'red-riding-hood 'move nil 'wood state)
       (rules 'red-riding-hood 'greet 'granny state)))

    ((initialNode s8)
     (progn
        (rules 'red-riding-hood 'move nil 'granny-home state)
        (rules 'red-riding-hood 'give 'granny 'cake state)
      ))

    ((s8 outcome)
     (progn
       (rules 'red-riding-hood 'give 'granny nil state)
       (format t "~%FIN HEUREUSE")))
    ((s1 s2)
     (rules 'red-riding-hood 'tell 'wolf 'wood state))

    ((s1 s5)
    (rules 'wolf 'eat 'red-riding-hood 'wood state))

    ((s2 s3)
     (progn
       (rules 'wolf 'move nil 'granny-home state)
       (rules 'wolf 'tell 'granny 'granny-home state)
       (rules 'wolf 'eat 'granny 'granny-home state)))
       
    ((s3 island)
     (format t "~%Fin Malheureuse"))

    ((s3 s4)
    (format t "~%Le loup prend se déguise comme granny et prend sa place."))

    ((s3 s7)
    (progn
      (rules 'hunter 'move nil 'granny-home state)
      (rules 'hunter 'tell 'wolf 'granny-home state)
      (rules 'hunter 'kill 'wolf 'granny-home state)
    ))

    ((s4 s5)
    (progn
      (rules 'red-riding-hood 'move nil 'granny-home state)
      (rules 'wolf 'tell 'red-riding-hood 'granny-home state)
      (rules 'wolf 'eat 'red-riding-hood 'granny-home state)
    ))

    ((s4 s6)
    (progn
      (rules 'wolf 'move nil 'wood state)
      (format t "~% Le loup fait une sieste dans la foret.")
    ))

    ((s4 s7)
    (progn
      (rules 'hunter 'move nil 'granny-home state)
      (rules 'hunter 'tell 'wolf 'granny-home state)
      (rules 'hunter 'kill 'wolf 'granny-home state)
    ))

    ((s5 s6)
    (progn
      (rules 'wolf 'move nil 'wood state)
      (format t "~% Le loup fait une sieste dans la foret.")
    ))

    ((s5 s7)
    (progn
      (rules 'hunter 'move nil 'granny-home state)
      (rules 'hunter 'tell 'wolf 'granny-home state)
      (rules 'hunter 'kill 'wolf 'granny-home state)
    ))

    ((s5 island)
     (format t "~%Fin Malheureuse"))

    ((s6 s7)
    (progn
      (rules 'hunter 'move nil 'wood state)
      (rules 'hunter 'tell 'wolf 'wood state)
      (rules 'hunter 'kill 'wolf 'wood state)
    ))

    ((s7 s8)
    (rules 'red-riding-hood 'give 'granny 'cake state)
    )

    ((s7 outcome)
    (format t "~%Fin Heureuse!!"))
    
    ((s8 outcome)
    (format t "~%Fin Heureuse!!"))
    
    (t
     (format t "~%Changement non reconnu : ~A" change))
     
  ))
#| 
;;(apply-change-scene '(initialNode s1) *state*)
;;(apply-change-scene '(initialNode s9) *state*)
;;(apply-change-scene '(s1 s2) *state*)
;;(apply-change-scene '(s1 s3) *state*)
;;(apply-change-scene '(s3 s4) *state*)
;;(apply-change-scene '(s3 s6) *state*)
;;(apply-change-scene '(s4 s6) *state*)
;;(apply-change-scene '(s4 outcome) *state*)
;;(apply-change-scene '(s7 outcome) *state*)
(apply-change-scene '(s5 island) *state*)
(apply-change-scene '(s10 island) *state*)
(apply-change-scene '(s9 island) *state*)
|#
;; fonction qui renvoie les successeurs valides d'un noeud du graphe de l'histoire
(defun successeurs-valides (etat story chemin)
  (let ((succ (cdr (assoc etat story))))  ; Obtenir les successeurs de l'état actuel dans l'histoire
    (dolist (x succ succ)
      (if (member x chemin)                  ; Vérifie si l'état a déjà été visité
          (setq succ (remove x succ))))))    ; Supprime les états déjà visités de la liste des successeurs


(successeurs-valides 's1 *story* '(initialNode s2))

;; Code NON TESTE
(defun generate_scenario (etat sortie story state &optional (scenario nil))
 (format t "~%========= Exploration de l'état : ~s =========" etat)
 (format t "Chemin actuel : ~s" (reverse (cons etat scenario)))
 (format t "~%État du monde actuel : ~s" state)
 ;; Condition d'arrêt : si l'état actuel est l'objectif
 (if (equal etat sortie)
 (progn
 (format t "~%Scénario complet trouvé : ~s" (reverse (cons etat scenario)))
 (reverse (cons etat scenario))) ; Retourne le scénario atteint
 ;; Exploration récursive en profondeur
 (let ((successeurs (successeursValides etat story scenario)))
 (if (null successeurs)
 (progn
 (format t "~%Impasse à l'état ~s, retour en arrière..." etat)
 nil) ; Retourne NIL si aucun successeur valide
 ;; Exploration de chaque successeur
 (dolist (succ successeurs)
 (format t "~%De ~s je vais en ~s" etat succ) ; Affiche la transition
 (let ((nouvel-etat-monde (copy-list state))) ; Copie de l'état du monde
 ;; Applique les changements de scène pour cette transition
 (apply-change-scene `(,etat ,succ) nouvel-etat-monde)
 ;; Appel récursif pour explorer le successeur
 (let ((result (generate_scenario succ sortie story nouvel-etat-monde (cons etat scenario))))
 (when result ; Retourne le premier chemin réussi trouvé
 (return result)))))))))


(generate_scenario 'initialNode 'outcome *story* *state*)

;; Graphe de l'histoire dans la partie créative à enrichir 
(defvar *creative_story* '((initialNode s1 s9)
              (s1 s2 s3)
              (s2 s5)
              (s3 s4 s6)
              (s4 s6 outcome)
              (s5 s8 s10 island)
              (s6 s7 s10)
              (s7 s10 outcome)
              (s8 s9 s11)
              (s9 s10 island)
              (s10 s11 island)
              (s11 s12 outcome)
              (s12 outcome)
              (island)
              (outcome)
              ))

(defun creative-apply-change-scene (change state)
  (cond
    ((equal change '(initialNode s1))
      (progn 
        (rules 'red-riding-hood 'move nil 'wood state)
        (rules 'red-riding-hood 'greet 'wolf 'wood state)
      )
    )
    ((equal change '(initialNode s9))
      (progn 
        (rules 'red-riding-hood 'move nil 'wood state)
        (rules 'red-riding-hood 'greet 'wolf state)
        (rules 'wolf 'eat 'red-riding-hood 'wood state)
      )
    )
    ((equal change '(s1 s2))
      (progn 
        (rules 'red-riding-hood 'tell 'wolf 'wood state)
        
      )
    )
    ((equal change '(s1 s3))
      (progn 
        (rules 'red-riding-hood 'move nil 'granny-home state)
        (rules 'red-riding-hood 'greet 'granny 'granny-home state)
      )
    )
    ((equal change '(s2 s5))
      (progn
        (rules 'wolf 'move nil 'granny-home state)
        (rules 'wolf 'tell 'granny 'granny-home state)
        (rules 'wolf 'eat 'granny 'granny-home state)
      )
    )
    ((equal change '(s3 s4))
      (progn
        (rules 'red-riding-hood 'give 'granny 'granny-home state)
      )
    )
    ((equal change '(s3 s6))
      (progn
        (rules 'wolf 'move nil 'granny-home state)
      )
    )
    ((equal change '(s4 s6))
      (progn
        (rules 'wolf 'move nil 'granny-home state)
      )
    )
    ((equal change '(s5 s8))
      (progn
        (rules 'wolf 'tell nil 'granny-home state)
      )
    )
    ((equal change '(s5 s10))
      (progn
        (rules 'hunter 'move nil 'granny-home state)
        (rules 'hunter 'tell 'wolf 'granny-home state)
        (rules 'hunter 'kill 'wolf 'granny-home state)
      )
    )
    ((equal change '(s6 s10))
      (progn
        (rules 'wolf 'eat 'red-riding-hood 'granny-home state)
        (rules 'wolf 'eat 'granny 'granny-home state)
      )
    )
    ((equal change '(s7 s10))
      (progn
        (rules 'wolf 'move nil 'wood state)
        (rules 'wolf 'eat 'red-riding-hood 'wood state)
        (rules 'wolf 'eat 'granny 'wood state)
      )
    )
    ((equal change '(s8 s9))
      (progn
        (rules 'red-riding-hood 'move nil 'granny-home state)
        (rules 'wolf 'tell 'red-riding-hood 'granny-home state)
        (rules 'wolf 'eat 'red-riding-hood 'granny-home state)
      )
    )
    ((equal change '(s8 s11))
      (progn
        (rules 'hunter 'move nil 'granny-home state)
        (rules 'hunter 'tell 'wolf 'granny-home state)
        (rules 'hunter 'kill 'wolf 'granny-home state)
      )
    )
    ((equal change '(s9 s10))
      (progn
        (rules 'wolf 'move nil 'wood state)
      )
    )
    ((equal change '(s10 s11))
      (progn
        (rules 'hunter 'move nil 'wood state)
        (rules 'hunter 'tell 'wolf 'wood state)
        (rules 'hunter 'kill 'wolf 'wood state)
      )
    )
    ((equal change '(s11 s12))
      (progn
        (rules 'red-riding-hood 'give 'granny nil state)
      )
    )
    ((or (equal change '(s4 outcome)) (equal change '(s7 outcome)))
      (progn
        (format t "~%FIN HEUREUSE\"")
      )
    )
    ((equal change '(s12 outcome))
      (progn
        (rules 'red-riding-hood 'tell 'granny nil state)
      )
    )
    ((or (or (equal change '(s10 island)) (equal change '(s9 island))) (equal change '(s5 island)))
      (progn 
        (format t "~%FIN MALHEUREUSE\"")
      )
    )
  )
)
 
