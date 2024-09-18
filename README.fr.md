# Code R pour 'Une cartographie réglementaire incohérente menace les rivières et les ruisseaux'
[![en](https://img.shields.io/badge/lang-en-red.svg)](https://github.com/messamat/cartographie_cours_deau_R/blob/master/README.md)  

Ce dépot contient le code R associé avec l'article scientifique _Messager, M. L., Pella, H., & Datry, T. (2024). 
Inconsistent regulatory mapping quietly threatens rivers and streams. Environmental Science & Technology. 
https://doi.org/10.1021/acs.est.4c01859_

Une copie en anglais de la version accepté de l'article (après revue par des pairs) sous licence CC-BY-NC
à l'adresse suivante: https://hal.inrae.fr/hal-04700243  
Une traduction en français est également disponible: https://hal.inrae.fr/hal-04699448

## Résumé
Même la législation environnementale la plus stricte ne peut protéger un cours d'eau si ses affluents restent 
exposés à la pollution et à d'autres menaces en amont. Exclure un sous-ensemble de cours d'eau de la protection
juridique menace donc d'altérer les écosystèmes d'eau douce de réseaux fluviaux entiers et les services qu'ils 
fournissent, tels que l'eau potable et la régulation des crues. Une attention considérable a été accordée à la 
définition du champ d'application des lois environnementales protégeant les cours d'eau. Cependant, la manière
dont ces définitions sont mises en œuvre par le biais de la cartographie réglementaire, c'est-à-dire la cartographie 
des masses d'eau qui sont légalement considérées comme des cours d'eau et donc protégées, n'a pas été étudiée 
en dehors des États-Unis. Nous démontrons ici les conséquences de la cartographie réglementaire sur l'étendue 
des réseaux fluviaux protégés, en utilisant la France comme étude de cas. En assemblant la première carte des 
cours d'eau français protégés au titre de la Loi sur l'eau, nous estimons qu'un quart des segments hydrographiques 
précédemment cartographiés ont été exclus, et constatons de fortes variations géographiques dans l'étendue des 
écosystèmes protégés. Les segments de tête de bassin et les segments non pérennes sont disproportionnellement 
exclus de 28% par rapport à leur prévalence (67 %) dans l'ensemble du réseau hydrographique, avec des implications
potentiellement considérables pour la biodiversité et les populations humaines. Nous nous attendons à ce que les 
cadres réglementaires de la plupart des pays soient également sensibles à l'interprétation locale des définitions 
juridiques. 

Voici la traduction en français en gardant les liens et les termes techniques intacts, notamment "dataset" traduit par "base de données" :

## Introduction

Ce dépôt inclut les parties de l'analyse effectuée dans R, qui repose sur des fichiers pré-formatés spatialement.
Ce flux de travail doit être réalisé après l'exécution du code Python dans le dépôt suivant : https://github.com/messamat/cartographie_cours_deau.
Ces scripts sont annotés mais peuvent être difficiles à suivre. Si vous rencontrez des difficultés, n'hésitez pas à 
contacter Mathis L. Messager pour des commentaires et des clarifications par e-mail ou à signaler un problème sur GitHub.

Les fichiers nécessaires pour exécuter cette analyse sont soit téléchargés directement depuis le code dans ce dépôt 
ou depuis le code Python, soit fournis par les Directions Départementales des Territoires. 
Veuillez contacter Mathis L. Messager pour obtenir de l'aide concernant ces données.

## Structure de l'analyse et bases de données sous-jacentes

Cette analyse s'appuie autant que possible sur les 
[bonnes pratiques de la recherche informatique](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005510), 
que les utilisateurs sont encouragés à lire.

**Structure** : le répertoire global du projet est structuré avec les sous-répertoires suivants :  
data/ (données brutes, en lecture seule, à ne pas modifier)  
results/ (résultats de l'analyse, principalement reproductibles par exécution de code. Cependant, inclut également des résultats modifiés manuellement)  
src/ (code écrit pour le projet)  
|---- cartographie_cours_deau_R (projet/code source pour l'analyse dans R)  

Tous les scripts dépendent de cette structure.

**Flux de travail R** : ce projet est configuré avec un [workflow targets](https://docs.ropensci.org/targets/), garantissant la reproductibilité.  
Dans la philosophie `targets`, chaque action est une fonction, et chaque objet R résultant d'une étape du flux de travail est une "cible" avec des dépendances.  
Les cibles/objets intermédiaires sont stockés dans un répertoire `_targets`.

**Gestion des dépendances** : la bibliothèque R de ce projet est gérée par [renv](https://rstudio.github.io/renv/articles/renv.html). 
Cela garantit que les mêmes versions de packages sont utilisées lors de la recréation du projet.
Lors de l'appel à `renv::restore()`, tous les packages requis seront installés avec leur version spécifique.  
Veuillez noter que ce projet a été créé avec la version R 4.4 sous Windows 10.

**Syntaxe** : cette analyse repose sur la syntaxe [data.table](https://rdatatable.gitlab.io/data.table/), 
qui fournit une version haute performance de data.frame. 
Elle est plus concise, rapide et efficace en mémoire que les data.frames conventionnels et la syntaxe du tidyverse.

## Démarrage
### Télécharger le dépôt pour R
Dans Git Bash, les commandes suivantes illustrent la procédure pour créer une copie locale du dépôt Github dans un nouveau répertoire à  
C://cartographie_cours_deau_R/src :

```{r, engine = 'bash', eval = FALSE}
Mathis@DESKTOP MINGW64 /c/cartographie_cours_deau/src
$ git clone https://github.com/messamat/cartographie_cours_deau_R.git
```

Dans R Studio pour Windows, la procédure suivante peut être utilisée :  

* Cliquez sur "Fichier" dans le menu  
* Sélectionnez "Nouveau projet..."  
* Choisissez l'option "Contrôle de version" dans la fenêtre de l'Assistant Nouveau Projet.  
* Ensuite, sélectionnez "Git" dans la fenêtre suivante.  
* Dans la fenêtre suivante, remplissez les champs comme suit :  
  * URL du dépôt : https://github.com/messamat/cartographie_cours_deau_R  
  * Nom du répertoire du projet : [sera rempli automatiquement avec "cartographie_cours_deau_R"]  
  * Créer le projet comme sous-répertoire de : [choisissez le répertoire parent de src]  
* Cochez "Ouvrir dans une nouvelle session" puis cliquez sur "Créer projet".  

### Structure du dépôt Github
- [**R/**](https://github.com/messamat/cartographie_cours_deau_R/tree/main/R) — cœur de l'analyse  
  - [*functions.R*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/R/functions.R) - toutes les fonctions personnalisées utilisées dans le formatage et l'analyse des données.  
  - [*packages.R*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/R/packages.R) - tous les packages utilisés dans le flux de travail.
- [*.Rprofile*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/.Rprofile) — utilisé pour activer renv pour les nouvelles sessions R lancées dans le projet.
- [*cartographie_cours_deau_R.Rproj*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/GeneticScaling.Rproj) — fichier projet R.  
- [*LICENSE*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/LICENSE) - conditions d'utilisation, de modification et de partage de ce logiciel.  
- [*README.md*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/README.md) — README pour Github (ce fichier).
- [*\_targets.R*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/_targets.R) — script de configuration pour le flux de travail targets, ce fichier spécifique est requis par le package targets.
Il contient le “plan” des cibles/targets, le catalogue des étapes du flux de travail (voir le chapitre correspondant dans le manuel d'utilisateur de targets).
Ce plan définit l'ordre des fonctions à utiliser, leurs entrées et sorties (généralement, des cibles), et la relation entre les cibles et les étapes.  
- [*renv.lock*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/renv.lock) — fichier de verrouillage renv, décrivant l'état de la bibliothèque du projet (packages installés et leurs versions).
- [*report_20231230.qmd*](https://github.com/messamat/cartographie_cours_deau_R/blob/main/report_20231230.qmd)  — fichier de rapport au format Quarto résumant
  les données et informations pour soutenir la rédaction du manuscrit.

## Exécuter l'analyse
À condition que vous disposiez des données nécessaires, l'analyse complète peut être réexécutée simplement avec le code suivant trouvé dans 
```{r rmake, eval = FALSE}
source('_targets.R')
tar_make()
```
`tar_make()` est la fonction centrale de l'approche targets. Elle exécute toutes les étapes du flux de travail dans le bon ordre, en évitant tout travail déjà à jour. Grâce au suivi des fonctions et objets globaux comme dépendances des cibles, l'utilisation de `tar_make()` est nécessaire pour exécuter le pipeline d'analyse dans un environnement propre et reproductible. Si toutes les cibles sont à jour dans le répertoire de cache, rien ne sera exécuté.

## Inspection des résultats
Si vous avez reçu des cibles intermédiaires (c'est-à-dire un répertoire `_targets/` ; ou une fois l'analyse réexécutée), vous pouvez charger des cibles individuelles dans l'environnement avec les commandes suivantes (même si les cibles ne sont pas à jour en raison, par exemple, d'un changement de chemin source).  
``` {r loadtarg, eval = FALSE}
tar_load(vulnerable_waters_analysis_tabs) #Charger la cible en mémoire (environnement R) avec son nom d'origine comme nom de variable 
vulnerable_waters_analysis_tabs <- tar_read(vulnerable_waters_analysis_tabs) #Charger la cible en mémoire avec un nouveau nom de variable
```
