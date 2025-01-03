;; Graphe de l'histoire
(defvar *story* '((initialNode s8 s5 s1)
              (s1 s2 s5)
              (s2 s3)
              (s3 s4 s7 island)
              (s4 s5 s6 s7)
              (s5 s6 s7 island)
              (s6 s7)
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

 (defun set-value (person property val state)
  ;;Fonction permettant de mettre à jour la valeur  
  ;;d'une propriété d'un protagoniste dans l'état du monde
  (setf (cadr (assoc property (cdr (assoc person state)))) val)
  )

(defun get-value (person property state)
  ;;Fonction permettant d'obtenir la valeur d'une propriété 
  ;;d'un protagoniste depuis l'état du monde"
  (cadr (assoc property (cdr (assoc person state)))))

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
         (when (equal cc 'wood) (format t "~%*D'humeur aventureuse, le Petit Chaperon Rouge passe par les bois pour se rendre chez mère-grand.*"))
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
      ;; Applique un dialogue (différent selon les scènes)
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
    
    (otherwise (format t "~%Erreur : l'action ~a est inconnue." action))))
;;*state*
;;(rules 'hunter 'kill 'wolf 'granny-home *state*)
;;*state*
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
  (unless state
    (error "L'état est NIL, impossible d'appliquer le changement."))
  
  (let ((from (first change))
        (to (second change)))
    
    (cond
      ;; Scénarios depuis initialNode
      ((and (eq from 'initialNode) (eq to 's1))
       (progn
         (rules 'red-riding-hood 'move nil 'wood state)
         (rules 'red-riding-hood 'greet 'wolf 'wood state)
         state))
      
      ((and (eq from 'initialNode) (eq to 's5))
       (progn
         (rules 'red-riding-hood 'move nil 'wood state)
         (rules 'wolf 'eat 'red-riding-hood state)
         state))
      
      ((and (eq from 'initialNode) (eq to 's8))
       (progn
         (rules 'red-riding-hood 'move nil 'granny-home state)
         (rules 'red-riding-hood 'greet 'granny 'granny-home state)
         (rules 'red-riding-hood 'give 'granny 'granny-home state)
         state))
      
      ((and (eq from 's1) (eq to 's2))
       (progn
         (rules 'red-riding-hood 'tell 'wolf 'wood state)
         state))
      
      ((and (eq from 's1) (eq to 's5))
       (progn
         (rules 'wolf 'eat 'red-riding-hood 'wood state)
         state))
      
      ((and (eq from 's2) (eq to 's3))
       (progn
         (rules 'wolf 'move nil 'granny-home state)
         (rules 'wolf 'tell 'granny 'granny-home state)
         (rules 'wolf 'eat 'granny 'granny-home state)
         state))
      
      ((and (eq from 's3) (eq to 'island))
      (progn
       (format t "~%Fin Malheureuse")
       state))
      
      ((and (eq from 's3) (eq to 's4))
      (progn
       (format t "~%Le loup se déguise en mère-grand pour duper le petit chaperon rouge.")
       state))
      
      ((and (eq from 's3) (eq to 's7))
       (progn
         (rules 'hunter 'move nil 'granny-home state)
         (rules 'hunter 'tell 'wolf 'granny-home state)
         (rules 'hunter 'kill 'wolf 'granny-home state)
         state))
      
      ((and (eq from 's4) (eq to 's5))
       (progn
         (rules 'red-riding-hood 'move nil 'granny-home state)
         (rules 'wolf 'tell 'red-riding-hood 'granny-home state)
         (rules 'wolf 'eat 'red-riding-hood 'granny-home state)
         state))
      
      ((and (eq from 's4) (eq to 's6))
       (progn
         (rules 'wolf 'move nil 'wood state)
         state))
      
      ((and (eq from 's4) (eq to 's7))
       (progn
         (rules 'hunter 'move nil 'granny-home state)
         (rules 'hunter 'tell 'wolf 'granny-home state)
         (rules 'hunter 'kill 'wolf 'granny-home state)
         state))
      
      ((and (eq from 's5) (eq to 's6))
       (progn
         (rules 'wolf 'move nil 'wood state)
         state))
      
      ((and (eq from 's5) (eq to 's7))
       (progn
         (rules 'hunter 'move nil 'granny-home state)
         (rules 'hunter 'tell 'wolf 'granny-home state)
         (rules 'hunter 'kill 'wolf 'granny-home state)
         state))
      
      ((and (eq from 's5) (eq to 'island))
      (progn
        (format t "~%Fin Malheureuse")
        state))
      
      ((and (eq from 's6) (eq to 's7))
       (progn
         (rules 'hunter 'move nil 'wood state)
         (rules 'hunter 'tell 'wolf 'wood state)
         (rules 'hunter 'kill 'wolf 'wood state)
         state))
      
      ((and (eq from 's7) (eq to 's8))
       (progn
         (rules 'red-riding-hood 'give 'granny 'wood state)
         state))
      
      ((and (eq from 's7) (eq to 'outcome))
       (progn
       (format t "~%Fin Heureuse!!")
       state))
      
      ((and (eq from 's8) (eq to 'outcome))
       (progn
       (format t "~%Fin Heureuse!!")
       state))
      
      (t
       (progn
       (format t "~%Changement non reconnu : ~A" change)
       nil)))))

;; fonction qui renvoie les successeurs valides d'un noeud du graphe de l'histoire
(defun successeurs-valides (etat story chemin)
  (let ((succ (cdr (assoc etat story))))  ; Obtenir les successeurs de l'état actuel dans l'histoire
    (dolist (x succ succ)
      (if (member x chemin)                  ; Vérifie si l'état a déjà été visité
          (setq succ (remove x succ))))))    ; Supprime les états déjà visités de la liste des successeurs 

(successeurs-valides 's1 *story* '(initialNode s2))

(defun deep-copy-alist (alist)
  ;; Crée une copie profonde d'une aliste avec des propriétés imbriquées.
  ;; Exemple: ((character1 (prop1 val1) (prop2 val2)) (character2 (prop1 val1)))
  (mapcar #'(lambda (character-entry)
              (cons (car character-entry)  ; nom du personnage
                    (mapcar #'(lambda (property)
                              (copy-list property))  ; copie chaque propriété
                           (cdr character-entry))))
          alist))

(defun generate_scenario (etat sortie story state &optional (scenario nil))
  (format t "~%========= Explore =========~%Etat ~s state : ~s" etat state)
  (pushnew etat scenario)

  (if (equal etat sortie)
      (reverse scenario)
      (let* ((solution nil)
             (successeurs (successeurs-valides etat story scenario)) ; Liste des succeseurs valides de l'état dans le graphe story
             (previous_state (deep-copy-alist state))) ; Copie de l'état donné en argument en vue de le modifier et/ou restaurer
        ; Ajout de logs de débogage
        (format t "~%État initial pour ~s: ~s" etat state)
        (format t "~%Copie de l'état: ~s" previous_state)
        (format t "~%Successeurs valides: ~s" successeurs)
        
        (loop while (and successeurs (not solution)) do 

          (if (not (equal (car successeurs) 'island)) 
            (progn
              (let ((current-successor (car successeurs)))
              (format t "~%Tentative transition de ~s vers ~s" etat current-successor)
            
              (let ((new-state (apply-change-scene 
                            (list etat current-successor) 
                            previous_state)))
              
              (if new-state
                  (progn
                    (format t "~%Nouvel état calculé: ~s" new-state)
                    (setf state new-state)
                    (setf solution (generate_scenario current-successor sortie story state scenario)))
                  (format t "~%ERREUR: apply-change-scene a retourné NIL")) ; Log de débogage
              )))
            (progn
              (setf state previous_state)
              (format t "~%Impasse ! De ~s je retourne à ~s " (pop successeurs) etat)
            )
          )
        )
      solution)))

;; Test basique
;; (generate_scenario 'initialNode 'outcome *story* *state*)

;; Tests avec des variations de l'histoire
(defvar *test-story* '((initialNode s5 s1)
              (s1 s2 s5)
              (s2 s3)
              (s3 island s4 s7)
              (s4 s5 s6 s7)
              (s5 island s6 s7)
              (s6 s7)
              (s7 outcome)
              (island)
              (outcome)
              ))
(defvar *test-state* '((red-riding-hood (alive t) (place wood)) 
               (wolf (alive t) (place wood)) 
               (granny (alive t) (place granny-home))
               (hunter (alive t) (place hunter-home))))

(generate_scenario 's1 'outcome *test-story* *test-state*)
 
;; PARTIE CREATIVE

;; Graphe de l'histoire généré uniquement par IA avec ses commentaires
(defvar *creative-generated-story*
  '((initialNode s1)           ; Départ dans la forêt
    (s1 s2 s3 s9)             ; Rencontres dans la forêt : parle au loup (s2), évite le loup (s3), ou trouve un raccourci secret (s9)
    (s2 s4 s11)               ; Donne la localisation au loup (s4), ou un lapin prévient le chasseur (s11)
    (s3 s5 s10)               ; Évite le loup : trouve une cabane abandonnée (s5) ou prépare un piège (s10)
    (s4 s6 s7)                ; Le loup se rend chez la grand-mère (s6), mais peut être suivi par le chasseur (s7)
    (s5 s6 s7 island)         ; Transitions possibles à partir de la cabane abandonnée
    (s6 s8 s12)               ; À la maison : déguisement du loup (s8) ou confrontation directe (s12)
    (s7 s8 s13)               ; Le chasseur attend à la maison (s8), ou suit le loup dans un duel (s13)
    (s8 outcome)              ; Transition vers une fin heureuse
    (s9 s10 s11)              ; Raccourci secret : rejoint un allié (s10) ou alerte un chasseur (s11)
    (s10 s12 s13)             ; Prépare un piège pour le loup ou rejoint la maison à temps
    (s11 s8 s13)              ; Chasseur agit : sauve grand-mère (s8) ou affronte le loup (s13)
    (s12 outcome island)      ; Confrontation directe : réussit ou échoue
    (s13 s8 outcome)          ; Duel avec le loup mène à une fin heureuse ou malheureuse
    (island)                  ; Fin alternative ou malheureuse
    (outcome)))               ; Fin heureuse

;; Graphe de l'histoire optimisé et enrichi
(defvar *creative-story* '((initialNode s1 s9)
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
 ; (defun creative-rules (actor action &optional person cc state) ...)

; Fonciton apply-change-scene pour l'histoire creative
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

 ; (defun interactive-story-generator (etat sortie story state &optional (scenario nil)) ...)
