# thot

[![forthebadge](https://forthebadge.com/images/badges/powered-by-water.svg)](https://forthebadge.com)
[![forthebadge](http://forthebadge.com/images/badges/built-with-love.svg)](http://forthebadge.com)
[![forthebadge](https://forthebadge.com/images/badges/uses-badges.svg)](https://forthebadge.com)

`thot` est un dashboard interactif basé sur les extraits de résumés d'unité médicale (RUM) de l'applicatif SIMPA, qui permet de visualiser rapidement les informations nécessaires aux missions quotidiennes des Directions d'Information Médicales (DIM) locaux concernant l'activité des services hospitaliers à leur charge (rapports d'activité, PIRAMIG, etc.).

 * Filtrage interactif par période, UH, CMD et catégorie de GHM
 * Sélection de sous-ensembles d'activité par diagnostics CIM-10 ou par actes CCAM
 * Génération automatique de rapports html interactifs
 * Référentiels CIM-10 et CCAM tenus à jour tirés de la librairie [`nomensland`](https://github.com/GuillaumePressiat/nomensland)

<p align="center">
    <img src="https://user-images.githubusercontent.com/20992061/59159661-63df0600-8acd-11e9-9e7d-ceb898f56ae3.gif" width="800">
</p>

## Instructions pour récupérer les données

* Extraire des données de l'applicatif SIMPA au **format CSV** (sélectionner "NON" dans l'option *Format Excel*)
* Se rendre sur l'applicatif SIMPA et cliquez sur l'onglet *Exploitation des données* après avoir sélectionné l'option `Nouveau menu` dans le *Type de menu sélectionné* de la page d'accueil

<p align="center">
    <img src="https://user-images.githubusercontent.com/20992061/59253197-fac2d400-8c2d-11e9-9f4b-5c9343f27cdf.png" width="800">
</p>

* Cliquer sur l'onglet *Résumés*

<p align="center">
    <img src="https://user-images.githubusercontent.com/20992061/59253198-fac2d400-8c2d-11e9-90bd-795413261c9e.png" width="800">
</p>

* Remplir les paramètres suivants et cliquer sur le bouton `Valider` :

    + *Format Excel souhaité* = `NON`
    + *Période d'hospitalisation* = la période d'intérêt (correspondant aux dates de sorties; on pourra filtrer sur la date d'entrée dans l'application)
    + *URM* = l'URM souhaitée (plusieurs URMs possibles en séparant par des virgules et sans espace; cliquer sur la petite étoile affiche la liste des URMs)
    + *Paramétrage de l'export* = nombre max de diagnostics et d'actes CCAM conseillé à `20` pour être exhaustif sans avoir un fichier trop lourd

<p align="center">
    <img src="https://user-images.githubusercontent.com/20992061/59253194-fa2a3d80-8c2d-11e9-9d2b-c19f007cc73f.png" width="800">
</p>

* L'applicatif SIMPA redirige alors vers la page *Traitements*. Appuyer sur le bouton `Rechercher` pour rafraîchir la page jusqu'à ce qu'une ligne correspondant à votre demande s'affiche (ce qui peut prendre plusieurs minutes). Puis dans un deuxième temps, une icône s'affichera dans la colonne *Télécharger*. Cliquer sur ![icone](https://user-images.githubusercontent.com/20992061/59254257-26df5480-8c30-11e9-952f-0e1164aad2b9.png) puis importer le fichier obtenu dans `thot`

<p align="center">
    <img src="https://user-images.githubusercontent.com/20992061/59253194-fa2a3d80-8c2d-11e9-9d2b-c19f007cc73f.png" width="800">
</p>



## Lancer l'application

Vous pouvez télécharger le contenu du dossier `shinyapps` et lancer la commande `shiny::runApp('app.R')` dans R/Rstudio (ou cliquer sur le bouton `Run App` dans Rstudio) après avoir installé les librairies requises. Un Dockerfile est également mis à disposition pour créer un container adéquat.

## To-do

- [ ] Compléter les instructions
- [ ] Ajouter des analyses
- [ ] Mapping pour utilisation sur une base au format OMOP
