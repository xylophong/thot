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
* (détails à rajouter)

## Lancer l'application

Vous pouvez télécharger le contenu du dossier `shinyapps` et lancer la commande `shiny::runApp('app.R')` dans R/Rstudio (ou cliquer sur le bouton `Run App` dans Rstudio) après avoir installé les librairies requises. Un Dockerfile est également mis à disposition pour créer un container adéquat.

## To-do

- [ ] Compléter les instructions
- [ ] Ajouter des analyses
- [ ] Mapping pour utilisation sur une base au format OMOP
