# Rapport TP n°2 - Little Red Riding Hood


## Page de Garde

**Binôme :**
- Julien HELLEC
- May LAJNEF

**Date :** 20/11/2024

**Cours :** IA01 - Intelligence Artificielle

**Université de Technologie de Compiègne**

![Image Petit Chaperon Rouge](https://i.natgeofe.com/n/cddedc8b-6fea-4699-b3a6-184a3ed246fc/73840.jpg?w=1280&h=1842)


## Sommaire

1. [Introduction](#introduction)
2. [Objectifs](#objectifs)
3. [Approche](#approche)
4. [Explication de fonctions](#explication-de-fonctions)
   1. [Fonctions de service](#fonctions-de-service)
   2. [Génération du scénario](#génération-du-scénario)
5. [Partie créative](#partie-créative)
6. [Conclusion](#conclusion)

## Introduction

Ce rapport présente notre travail sur le TP n°2 du cours IA01, portant sur la génération automatique d'histoires intéractives. Il s'agit de développer un générateur d'histoires utilisant une recherche en profondeur dans un espace d'états pour proposer des scénarios différents de l'histoire du Petit Chaperon Rouge.

## Objectifs

L'objectif principal de ce TP est de créer un système capable de générer des variations du conte du Petit Chaperon Rouge en utilisant un algorithme de recherche en profondeur d'abord. Plus spécifiquement, nous devions :

1. Développer des fonctions pour modifier l'état du monde à chaque changement de scène
2. Créer un générateur de scénarios basé sur une recherche en profondeur
3. Ajouter des éléments créatifs pour enrichir l'expérience narrative

## Approche

### Représentation de l'espace d'états

Nous avons choisi de représenter les personnages et l'état du monde sous forme de listes associatives. Chaque protagoniste possède des propriétés telles que "alive" et "place". Voici un exemple de la représentation de l'état initial :

```lisp
(defvar *state* 
  '((red-riding-hood (alive t) (place mum-home))
    (wolf (alive t) (place wood))
    (granny (alive t) (place granny-home))
    (hunter (alive t) (place hunter-home))))
```

## Explication de fonctions

### Fonctions de service

#### 1. Fonction apply-effect

Voici le code de la fonction apply-effect :

```
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
```

Cette fonction a pour but d'appliquer les effets des futurs actions des personnages à l'état de l'histoire. Elle nous servira par la suite pour faire état des personnages (localisation dans l'univers de l'histoire, en vie ou non) pour pouvoir décider des suites potentielles de l'histoire (successeurs valide).

Concernant l'algorithme de cette fonction, il n'y a rien de notable qui mérite d'être longuement commenté. On fait simplement une dinsjonction de cas à l'aide d'un cond pour différencier les différents états possibles ansi qu'un affichage par défaut lors de l'application de l'effet.

La seule particularité de cette fonction est le changement de la localisation du personnage mangé par le loup à "wolf-belly". 

#### 2. Fonction rules

Voici le code de la fonction rules :

```lisp
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
```

Cette fonction a pour but de définir les règles de passages entre les différents états de l'histoire. Elle fait la liste de tous les dialogues et cas possibles pour les différents effets. Par exemple, lorsque que le loup mange le chaperon rouge dans la maison de la grand-mère ou bien dans le bois ; les de dialogues ne sont pas les mêmes.

Cette fonction applique également les bons effets en fonction des prémisses valides.

#### 3. Fonction apply-change-scene

Voici le code de la fonction apply-change-scene :

```lisp
(defun apply-change-scene (change state)
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
```

Cette fonction a pour but de faire appliquer les bonnes règles lors des changements d'états (passages d'une scène à une autre) de l'histoire.
Elle gère les dialogues, les déplacments et les actions des personnages de manière cohérente.

### Génération du scénario 

#### Fonction successeursValides

Voici le code de la fonction successeursValides :
```lisp
(defun successeursValides (etat story chemin)
  (let ((succ (cdr (assoc etat story))))  ; Obtenir les successeurs de l'état actuel dans l'histoire
    (dolist (x succ succ)
      (if (member x chemin)                  ; Vérifie si l'état a déjà été visité
          (setq succ (remove x succ))))))    ; Supprime les états déjà visités de la liste des successeurs
```
Cette fonction permet de renvoyer les successeurs valides d'un noeud du graphe de l'histoire, c'est à dire tous ceux qui ne sont pas dans le chemin parcouru mis en argument. 

Exemples de tests et leurs outputs :
```lisp
(successeursValides 's1 *story* '(initialNode s2))
```
![Résultat du test](https://img.jpg)

```lisp
 (successeursValides 's4 *story* '(initialNode s1 s2 s3 s5))
```
![Résultat du test](https://img.jpg)


#### Fonction generate_scenario


## Partie créative


## Conclusion


