## app.R ##

library(shinydashboard)
library(shinyjs)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(nomensland)

# nomensland has to be installed from a github repo:
# install.packages("remotes")
# remotes::install_github('GuillaumePressiat/nomensland')

########
## UI ##
########

{
  #### UI HEADER ####
  DBheader <- dashboardHeader(
    title="thot"
  )
  
  #### UI SIDEBAR ####
  DBsidebar <- dashboardSidebar(
    sidebarMenu(
      tags$style(
        HTML(
          "
          .dataTables_filter {
            display: none;
          }

          .dataTables_wrapper .dt-buttons {
            float: right;
          }

          td[data-type='factor'] input {
            min-width: 50px;
          }

          select ~ .selectize-control .selectize-input {
            max-height: 55px;overflow-y: auto;
          }
          "
        )
      ),
      tags$head(
        HTML(
          "
          <script>
            var socket_timeout_interval
            var n=0
            $(document).on('shiny:connected', function(event) {
            socket_timeout_interval=setInterval(function(){
            Shiny.onInputChange('count', n++)
            }, 15000)
            });
            $(document).on('shiny:disconnected', function(event) {
            clearInterval(socket_timeout_interval)
            });
          </script>
          "
        )
      ),
      textOutput("keepAlive"),
      
      menuItem("Global", tabName="global", icon=icon("globe")),
      menuItem("Diagnostics", tabName="diagnostics", icon=icon("search")),
      menuItem("Actes", tabName="acts", icon=icon("search")),
      menuItem("GHM", tabName="ghm", icon=icon("search"))
    ),
    
    fileInput(
      inputId="file", 
      label=h4("Import des données"),
      multiple=FALSE,
      accept=c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      ),
      buttonLabel="Parcourir",
      placeholder=".csv SIMPA"
    ),
    
    uiOutput(outputId="dynamic_date_range"),
    uiOutput(outputId="dynamic_age_range"),
    uiOutput(outputId="dynamic_UH"),
    uiOutput(outputId="dynamic_cmd"),
    uiOutput(outputId="dynamic_GHM_lettre"),
    uiOutput(outputId="dynamic_mode_ent"),
    uiOutput(outputId="dynamic_dad")
  )
  
  #### UI BODY ####
  DBbody <- dashboardBody(
    useShinyjs(),
    
    tabItems(
      
      #### GLOBAL TAB ####
      tabItem(tabName="global",
        uiOutput(outputId="help"),
        fluidRow(
          column(uiOutput(outputId="download_report"), width=6),
          column(
            fluidRow(
              valueBoxOutput("global_n_sejours", width=6),
              valueBoxOutput("global_n_patients", width=6)
            ),
            fluidRow(
              valueBoxOutput("global_total_sejour", width=6),
              valueBoxOutput("global_moyenne_sejour", width=6)
            ), width=6
          )
        ),
        uiOutput(outputId="dynamic_diags_summary"),
        uiOutput(outputId="dynamic_acts_summary"),
        uiOutput(outputId="dynamic_ghm_summary"),
        uiOutput(outputId="dynamic_geographic_global"),
        uiOutput(outputId='dynamic_age_histogram'),
        uiOutput(outputId='dynamic_severite_histogram'),
        uiOutput(outputId='dynamic_mode_histogram'),
        uiOutput(outputId='dynamic_prov_histogram'),
        uiOutput(outputId="dynamic_evol_global")
      ), 
      
      #### DIAG TAB ####
      tabItem(tabName="diagnostics",
        fluidRow(
          box(
            selectizeInput(
              inputId='chosen_diagnoses',
              label=NULL,
              choices=NULL,
              multiple=TRUE,
              options=list(
                placeholder="Tapez un ou plusieurs code(s)/libellé(s)"
              )
            ), 
            actionButton("diags_tous", "En DP/DR/DAS"),
            actionButton("diags_dpdr", "En DP/DR"),
            actionButton("diags_reset", "Effacer"), 
            actionButton("diags_all", "Tout sélectionner"),
            width=4,
            title="Filtrer par codes CIM-10"
          ),
          
          box(
            HTML(
              "<div style='font-weight:normal;'>Le .csv (sep point-virgule) doit avoir une colonne
                <code>code</code> et une colonne <code>libelle</code>
                (cette dernière pouvant être vide). En option, une colonne 
                <code>categorie</code> peut être rajoutée pour 
                des analyses supplémentaires.</div>"
            ), br(),
            fileInput(
              inputId="diags_file", 
              label=NULL,
              multiple=FALSE,
              accept=c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              ),
              buttonLabel="Parcourir",
              placeholder="Fichier .csv (sep point-virgule)"
            ), 
            width=4, collapsible=T, collapsed=T,
            title="Liste de codes ?"
          ),
        uiOutput(outputId="download_report_diags")
        ),
        fluidRow(
            valueBoxOutput("n_sejours_by_diags", width=3),
            valueBoxOutput("n_patients_by_diags", width=3),
            valueBoxOutput("total_sejour_by_diags", width=3),
            valueBoxOutput("moyenne_sejour_by_diags", width=3)
        ),
        uiOutput(outputId="dynamic_diags_tables"),
        uiOutput(outputId="dynamic_categorie_diags"),
        uiOutput(outputId="dynamic_geographic_by_diags"),
        uiOutput(outputId='dynamic_age_histogram_by_diags'),
        uiOutput(outputId='dynamic_severite_histogram_by_diags'),
        uiOutput(outputId='dynamic_mode_histogram_by_diags'),
        uiOutput(outputId='dynamic_prov_histogram_by_diags'),
        uiOutput(outputId="dynamic_evol_by_diags")
      ),
      
      #### ACTS TAB ####
      tabItem(tabName="acts",
        fluidRow(
          box(
            selectizeInput(
              inputId='chosen_acts',
              label=NULL,
              choices=NULL,
              multiple=TRUE,
              options=list(
                placeholder="Tapez un ou plusieurs code(s)/libellé(s)"
              )
            ), 
            actionButton("acts_button", "Valider"), 
            actionButton("acts_reset", "Effacer"), 
            actionButton("acts_all", "Tout sélectionner"),
            width=4,
            title="Filtrer par codes CCAM"
          ),
          box(
            HTML(
              "<div style='font-weight:normal;'>Le .csv (sep point-virgule) doit avoir une colonne
                <code>code</code> et une colonne <code>libelle</code>
                (cette dernière pouvant être vide). En option, une colonne 
                <code>categorie</code> peut être rajoutée pour 
                des analyses supplémentaires.</div>"
            ), br(),
            fileInput(
              inputId="acts_file", 
              label=NULL,
              multiple=FALSE,
              accept=c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              ),
              buttonLabel="Parcourir",
              placeholder="Fichier .csv (sep point-virgule)"
            ),
            width=4, collapsible=T, collapsed=T,
            title="Liste de codes ?"
          ),
          uiOutput(outputId="download_report_acts")
        ),
        fluidRow(
          valueBoxOutput("n_sejours_by_act", width=3),
          valueBoxOutput("n_patients_by_act", width=3),
          valueBoxOutput("total_sejour_by_act", width=3),
          valueBoxOutput("moyenne_sejour_by_act", width=3)
        ),
        uiOutput(outputId="dynamic_acts_tables"),
        uiOutput(outputId="dynamic_categorie_acts"),
        uiOutput(outputId="dynamic_geographic_by_acts"),
        uiOutput(outputId='dynamic_age_histogram_by_acts'),
        uiOutput(outputId='dynamic_severite_histogram_by_acts'),
        uiOutput(outputId='dynamic_mode_histogram_by_acts'),
        uiOutput(outputId='dynamic_prov_histogram_by_acts'),
        uiOutput(outputId="dynamic_evol_by_acts")
      ),
      
      #### GHM TAB ####
      tabItem(tabName="ghm",
        fluidRow(
          box(
            selectizeInput(
              inputId='chosen_ghm',
              label=NULL,
              choices=NULL,
              multiple=TRUE,
              options=list(
                placeholder="Tapez un ou plusieurs code(s)/libellé(s)"
              )
            ), 
            actionButton("ghm_button", "Valider"), 
            actionButton("ghm_reset", "Effacer"), 
            actionButton("ghm_all", "Tout sélectionner"),
            width=4, 
            title="Filtrer par codes GHM"
          ),
          box(
            HTML(
              "<div style='font-weight:normal;'>Le .csv (sep point-virgule) doit avoir une colonne
                <code>code</code> et une colonne <code>libelle</code>
                (cette dernière pouvant être vide). En option, une colonne 
                <code>categorie</code> peut être rajoutée pour 
                des analyses supplémentaires.</div>"
            ), br(),
            fileInput(
              inputId="ghm_file", 
              label=NULL,
              multiple=FALSE,
              accept=c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              ),
              buttonLabel="Parcourir",
              placeholder="Fichier .csv (sep point-virgule)"
            ),
            width=4, collapsible=T, collapsed=T,
            title="Liste de codes ?"
          ),
          uiOutput(outputId="download_report_ghm")
        ),
        fluidRow(
          valueBoxOutput("n_sejours_by_ghm", width=3),
          valueBoxOutput("n_patients_by_ghm", width=3),
          valueBoxOutput("total_sejour_by_ghm", width=3),
          valueBoxOutput("moyenne_sejour_by_ghm", width=3)
        ),
        uiOutput(outputId="dynamic_ghm_tables"),
        uiOutput(outputId="dynamic_categorie_ghm"),
        uiOutput(outputId="dynamic_geographic_by_ghm"),
        uiOutput(outputId='dynamic_age_histogram_by_ghm'),
        uiOutput(outputId='dynamic_severite_histogram_by_ghm'),
        uiOutput(outputId='dynamic_mode_histogram_by_ghm'),
        uiOutput(outputId='dynamic_prov_histogram_by_ghm'),
        uiOutput(outputId="dynamic_evol_by_ghm")
      )
    )
  ) 
  
  #### GENERATING UI ####
  
  ui <- dashboardPage(
    DBheader,
    DBsidebar,
    DBbody
  )
}

############
## SERVER ##
############

server <- function(input, output, session) {
  
  ######################
  ### GLOBAL OPTIONS ###
  ######################
  
  # session$onSessionEnded(stopApp)
  
  options(
    shiny.maxRequestSize=30*1024^2,
    DT.options=list(
      style="bootstrap",
      pageLength=10,
      searching=FALSE,
      language=list(
        url='//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
      ),
      dom="tip",
      drawCallback=JS(
        "function(oSettings) {
            if (oSettings._iDisplayLength == -1
                || oSettings._iDisplayLength >= oSettings.fnRecordsDisplay())
            {
                jQuery(oSettings.nTableWrapper).find('.dataTables_paginate').hide();
            } else {
                jQuery(oSettings.nTableWrapper).find('.dataTables_paginate').show();
            }
        }"
      )
    )
  )
  
  output$keepAlive <- renderText({
    req(input$count)
    hide(paste("keep alive ", input$count))
  })
  
  ########################
  ### STATIC VARIABLES ###
  ########################
  
  idf_postal <- c(
    91, 92, 75, 77, 93, 95, 94, 78
  )
  
  cmd <- list(
    "01"="Affections du système nerveux", 
    "02"="Affections de l'œil",
    "03"="Affections des oreilles, du nez, de la gorge, de la bouche et des dents",
    "04"="Affections de l'appareil respiratoire",
    "05"="Affections de l'appareil circulatoire",
    "06"="Affections du tube digestif",
    "07"="Affections du système hépatobiliaire et du pancréas",
    "08"="Affections et traumatismes de l'appareil musculosquelettique et du tissu conjonctif",
    "09"="Affections de la peau, des tissus sous-cutanés et des seins",
    "10"="Affections endocriniennes, métaboliques et nutritionnelles",
    "11"="Affections du rein et des voies urinaires",
    "12"="Affections de l'appareil génital masculin",
    "13"="Affections de l'appareil génital féminin",
    "14"="Grossesses pathologiques, accouchements et affections du post-partum",
    "15"="Nouveau-nés, prématurés et affections de la période périnatale",
    "16"="Affections du sang et des organes hématopoïétiques",
    "17"="Affections myéloprolifératives et tumeurs de siège imprécis ou diffus",
    "18"="Maladies infectieuses et parasitaires",
    "19"="Maladies et troubles mentaux",
    "20"="Troubles mentaux organiques liés à l'absorption de drogues ou induits par celles-ci",
    "21"="Traumatismes, allergies et empoisonnements",
    "22"="Brûlures",
    "23"="Facteurs influant sur l'état de santé et autres motifs de recours aux services de santé", 
    "25"="Maladies dues à une infection par le VIH",
    "26"="Traumatismes multiples graves",
    "27"="Transplantations d'organes",
    "28"="Séances",
    "90"="Erreurs et autres séjours inclassables"
  )
  cmd <- data.frame(
    code=names(cmd), 
    libelle=unlist(cmd), 
    stringsAsFactors=FALSE
  )
  
  ghm <- list(
    "C"="Opératoire", 
    "K"="Non-opératoire",
    "M"="Acte(s) non-classant(s)",
    "Z"="Indifférencié"
  )
  ghm <- data.frame(
    code=names(ghm), 
    libelle=unlist(ghm),
    stringsAsFactors=FALSE
  )
  
  ghm_cancero <- read.csv2(
    "ghm_cancero.csv", 
    strip.white=TRUE, 
    stringsAsFactors=FALSE
  )
  
  keep_columns <- c(
    "No.resume",           
    "NIP",
    "NDA",
    "Nom",
    "Prenom",
    "URMC",                
    "UH",                 
    "URMP",                        
    "Date.naiss",          
    "Sexe",               
    "Age",                
    "Code.post.Resid",             
    "Comm.resid",          
    "Date.entree.resume", 
    "Mode.ent",            
    "Date.sortie.resume",
    "Mode.sor",            
    "Hop.prov",           
    "Finess.prov",         
    "Hop.dest",           
    "Finess.dest",                    
    "GHM",                             
    "CMD",
    "Duree.Resume",
    "Duree.sejour",
    "URM.orig",
    "URM.dest"
  )
  ###############
  #### INTRO ####
  ###############
  
  github <- a(
    "ce lien.", 
    href="https://github.com/deepPhong/thot"
  )
  
  mail <- a(
    "dinh-phong.nguyen@aphp.fr",
    href="mailto:dinh-phong.nguyen@aphp.fr?subject=Question/bug thot"
  )
  
  output$help <- renderUI({
    fluidRow(
      box(
        width=6,
        title="Instructions",
        solidHeader=TRUE,
        collapsible=TRUE,
        status="primary",
        "Instructions détaillées sur", tagList(github), br(),
        HTML("<u> Résumé</u> :"), br(),
        HTML("1) Se rendre sur l'interface web de votre SIMPA"), br(),
        HTML("2) Cliquer sur <i>Exploitation des données</i> > <i>Résumés</i>"), br(),
        HTML("3) Sélectionner <b>NON</b> dans <i>Format Excel souhaité</i>"), br(),
        HTML("4) Choisir la période"), br(),
        HTML("5) Choisir l'URM/les URMs</b>"), br(),
        HTML("6) Optimiser le <i>Paramétrage de l'export</i> et <b>Valider</b>"), br(),
        HTML("7) Cliquer sur <b>Rechercher</b> jusqu'à ce que"), 
        HTML("le lien de téléchargement apparaisse"), br(),
        HTML("8) Importer les données dans <code>thot</code>")
      ),
      box(
        width=6,
        title="Qu'est-ce que c'est ?",
        solidHeader=TRUE,
        collapsible=TRUE,
        status="primary",
        HTML("<code>thot</code>"), "est un dashboard réactif qui permet de",
        "visualiser rapidement l'activité d'une ou plusieurs unités hospitalières",
        "et d'en générer des rapports interactifs facilement partageables.", br(),
        br(), "Pour toute question ou bug :", tagList(mail)
      )
    )
  })
  
  observeEvent(load_data(), {
    req(load_data())
    output$help <- renderUI({hide("help")})
  })
  
  ####################
  ### LOADING DATA ###
  ####################
  
  load_data <- reactive({
    req(input$file)
    
    data <- read.csv2(
      input$file$datapath, 
      stringsAsFactors=FALSE, 
      fileEncoding="latin1", 
      na.strings=c("", " ", "NA")
    )
    
    check_codes <- function(data, regexp) {
      columns <- grep(regexp, colnames(data), value=TRUE)
      if (all(is.na(data[, columns]))) {
        return(NULL)
      } else {
        return(columns)
      }
    }
    
    diagnoses <- check_codes(data, "CIM.SIGN|CIM.principal|Diag.Relie$")
    dpdr <- check_codes(data, "CIM.principal|Diag.Relie$")
    acts <- check_codes(data, "CCAM.")
    dad <- check_codes(data, "CIM.DOC.")
    
    data <- data[, c(keep_columns, diagnoses, acts, dad)]
    
    data$Date.naiss <- dmy(data$Date.naiss)
    data$Date.entree.resume <- dmy_hm(data$Date.entree.resume)
    data$Date.sortie.resume <- dmy_hm(data$Date.sortie.resume)
    data$cmd <- substr(data$GHM, 0, 2)
    data$GHM_lettre <- substr(data$GHM, 3, 3)
    
    data$Age <- (
      as.duration(
        interval(data$Date.naiss, date(data$Date.entree.resume))
      ) %/% 
        as.duration(years(1))
    )
    
    data$Mode.ent <- substr(data$Mode.ent, 2, 3)
    data$Mode.ent <- (
      ifelse(
        data$Mode.ent == "/", "Domicile", 
        ifelse(
          data$Mode.ent == "/1", "UHCD",
          ifelse(
            data$Mode.ent == "/2", "SSR",
            ifelse(
              data$Mode.ent == "/3", "USLD",
              ifelse(
                data$Mode.ent == "/4", "PSY",
                ifelse(
                  data$Mode.ent == "/5", "Urgences", "Inconnu"
                )
              )
            )
          )
        )
      )
    )
    
    data$Mode.sor <- substr(data$Mode.sor, 0, 1)
    data$Mode.sor <- (
      ifelse(
        data$Mode.sor == 0, "Transfert provisoire", 
        ifelse(
          data$Mode.sor == 6, "Mutation",
          ifelse(
            data$Mode.sor == 7, "Transfert",
            ifelse(
              data$Mode.sor == 8, "Domicile",
              ifelse(
                data$Mode.sor == 9, "Deces", "Inconnu"
              )
            )
          )
        )
      )
    )
    
    data$annee.sortie <- as.numeric(year(data$Date.sortie.resume))
    data$mois.sortie <- as.numeric(month(data$Date.sortie.resume))
    
    if (length(diagnoses) > 0) {
      data$diagnoses=apply(
        data[, diagnoses], 
        1, 
        function(x) unique(unname(c(x[!is.na(x)])))
      )
    } else {
      data$diagnoses <- NA
    }
    
    if (length(dpdr) > 0) {
      data$dpdr=apply(
        data[, dpdr], 
        1, 
        function(x) unique(unname(c(x[!is.na(x)])))
      )
    } else {
      data$diagnoses <- NA
    }
    
    if (length(acts) > 0) {
      data$acts=apply(
        data[, acts], 
        1, 
        function(x) unique(unname(c(x[!is.na(x)])))
      )
    } else {
      data$acts <- NA
    }
    
    if (length(dad) > 0) {
      data$dad=apply(
        data[, dad], 
        1, 
        function(x) unique(unname(c(x[!is.na(x)])))
      )
    } else {
      data$dad <- NA
    }
    
    data <- data[
      , (!names(data) %in% diagnoses) & 
        (!names(data) %in% acts) & 
        (!names(data) %in% dad)
      ]
    
    return(data)
  })
  
  most_common_year <- reactive({
    req(load_data())
    year <- as.numeric(tail(names(sort(table(load_data()$annee.sortie))), 1))
    return(year)
  })
  
  data <- reactive({
    req(
      load_data(), 
      data_cmd(),
      input$age_filter,
      input$UH_filter, 
      input$cmd_filter, 
      input$GHM_lettre_filter,
      input$mode_ent_filter,
      input$dad_filter
    )
    data=load_data()
    UH_list=input$UH_filter
    cmd_list=unlist(data_cmd()[data_cmd()$value %in% input$cmd_filter, "code"])
    GHM_lettre_list=input$GHM_lettre_filter
    
    min_date <- input$date_range[1]
    max_date <- input$date_range[2]
    min_age <- input$age_filter[1]
    max_age <- input$age_filter[2]
    
    data <-(
      data[(
        (data$Age >= min_age)
        & (data$Age <= max_age)
        & (data$Date.sortie.resume >= min_date)
        & (data$Date.sortie.resume <= max_date)
        & (data$UH %in% UH_list)
        & (data$cmd %in% cmd_list)
        & (data$GHM_lettre %in% GHM_lettre_list)
        & (data$Mode.ent %in% input$mode_ent_filter)
      ), ]
    )
    
    if ((input$dad_filter == "Oui") & (!all(is.na(data$dad)))) {
      MR_data <- sapply(
        data$dad, function(x) length(grep("^MR", x, value=TRUE)) > 0
      )
      data <- data[MR_data, ]
    }
    return(data)
  })
  
  data_common <- reactive({
    data <- data()
    if (!is.null(by_lists$diags_list)) {
      if (diags_types$all==TRUE) {
        data <- (
          data[sapply(data[["diagnoses"]], function(x) any(by_lists$diags_list %in% x)),]
        )
      } else if (diags_types$all==FALSE) {
        data <- (
          data[sapply(data[["dpdr"]], function(x) any(by_lists$diags_list %in% x)),]
        )
      }
    }
    if (!is.null(by_lists$acts_list)) {
      data <- (
        data[sapply(data[["acts"]], function(x) any(by_lists$acts_list %in% x)),]
      )
    }
    if (!is.null(by_lists$ghm_list)) {
      data <- (
        data[sapply(data[["GHM"]], function(x) any(by_lists$ghm_list %in% x)),]
      )
    }
    return(data)
  })
  
  load_diags <- reactive({
    req(input$diags_file)
    data <- read.csv2(
      input$diags_file$datapath, 
      stringsAsFactors=FALSE, 
      fileEncoding="latin1", 
      na.strings=c("", " ", "NA"),
      sep=";",
      strip.white=TRUE
    )
    return(data)
  })
  
  load_acts <- reactive({
    req(input$acts_file)
    data <- read.csv2(
      input$acts_file$datapath, 
      stringsAsFactors=FALSE, 
      fileEncoding="latin1", 
      na.strings=c("", " ", "NA"),
      sep=";",
      strip.white=TRUE
    )
    return(data)
  })
  
  load_ghm <- reactive({
    req(input$ghm_file)
    data <- read.csv2(
      input$ghm_file$datapath, 
      stringsAsFactors=FALSE, 
      fileEncoding="latin1", 
      na.strings=c("", " ", "NA"),
      sep=";",
      strip.white=TRUE
    )
    return(data)
  })
  
  ##############################
  ### LOADING INPUTS CHOICES ###
  ##############################
  
  cim <- reactive({
    req(most_common_year())
    withProgress(
      message="Chargement du référentiel CIM-10",{
      incProgress(1/4, detail="Chargement de la version de l'année choisie...")
      cim <- get_table("cim", most_common_year())
      incProgress(1/4, detail="Mise en place des choix...")
      cim <- cim[, c("code", "lib_long")]
      colnames(cim) <- c("code", "libelle")
    })
    return(cim)
  })
  
  ccam <- reactive({
    withProgress(
      message="Chargement du référentiel CCAM",{
        incProgress(1/4, detail="Chargement de la dernière version...")
        ccam <- get_table("ccam_actes")
        incProgress(1/4, detail="Mise en place des choix...")
        ccam <- ccam[, c("code", "libelle_long")]
        colnames(ccam) <- c("code", "libelle")
      })
    return(ccam)
  })
  
  ghm_ref <- reactive({
    withProgress(
      message="Chargement du référentiel des GHM",{
        incProgress(1/4, detail="Chargement de la dernière version...")
        ghm_ref <- get_table("tarifs_mco_ghs", most_common_year())
        incProgress(1/4, detail="Mise en place des choix...")
        ghm_ref <- ghm_ref[, c("ghm", "libelle_ghm", "ghs", "tarif_base")]
        colnames(ghm_ref) <- c("code", "libelle", "ghs", "tarif_base")
      })
    return(ghm_ref)
  })
  
  data_cmd <- reactive({
    data_cmd <- unique(unlist(load_data()$cmd))
    cmd_part <- cmd[cmd$code %in% data_cmd, ]
    rownames(cmd_part) <- NULL
    cmd_part <- cbind(cmd_part, value=seq_len(nrow(cmd_part)))
    return(cmd_part)
  })
  
  data_cim <- reactive({
    req(sum(is.na(load_data()$diagnoses)) != nrow(load_data()))
    data_cim <- data.frame(table(unlist(load_data()$diagnoses), dnn="code")) %>%
      arrange(desc(Freq))
    cim <- cim()
    cim_part <- unique(merge(
      data_cim, cim, by="code", 
      all.x=TRUE, all.y=FALSE, sort=FALSE
    )[, c("code", "libelle")])
    rownames(cim_part) <- NULL
    return(cim_part)
  })
  
  data_acts <- reactive({
    req(sum(is.na(load_data()$acts)) != nrow(load_data()))
    data_acts <- data.frame(table(unlist(load_data()$acts), dnn="code")) %>%
      arrange(desc(Freq))
    ccam <- ccam()
    ccam_part <- unique(merge(
      data_acts, ccam, by="code", 
      all.x=TRUE, all.y=FALSE, sort=FALSE
    )[, c("code", "libelle")])
    rownames(ccam_part) <- NULL
    return(ccam_part)
  })
  
  data_ghm <- reactive({
    req(sum(is.na(load_data()$GHM)) != nrow(load_data()))
    data_ghm <- data.frame(table(unlist(load_data()$GHM), dnn="code")) %>%
      arrange(desc(Freq))
    ghm_ref <- ghm_ref()
    ghm_part <- unique(merge(
      data_ghm, ghm_ref, by="code", 
      all.x=TRUE, all.y=FALSE, sort=FALSE
    )[, c("code", "libelle", "ghs", "tarif_base")])
    rownames(ghm_part) <- NULL
    return(ghm_part)
  })
  
  observeEvent(data_cim(), {
    cim_part <- data_cim()
    updateSelectizeInput(
      session=session, 
      inputId='chosen_diagnoses',
      choices=cbind(
        cim_part,
        value=seq_len(nrow(cim_part))
      ),
      server=TRUE,
      options=list(
        optgroups=lapply(unique(cim_part$libelle), function(x){
          list(value=as.character(x), label=as.character(x))
        }),
        optgroupField='code',
        searchField=c('code', 'libelle'),
        labelField='code',
        render=I("{
                   option: function(item, escape) {
                   return '<div>' + escape(item.libelle) +'</div>';
                   }
                  }")
      )
    )
  })
  
  observeEvent(data_acts(), {
    ccam_part <- data_acts()
    updateSelectizeInput(
      session=session, 
      inputId='chosen_acts',
      choices=cbind(
        ccam_part,
        value=seq_len(nrow(ccam_part))
      ),
      server=TRUE,
      options=list(
        optgroups=lapply(unique(ccam_part$libelle), function(x){
          list(value=as.character(x), label=as.character(x))
        }),
        optgroupField='code',
        searchField=c('code', 'libelle'),
        labelField='code',
        render=I("{
                   option: function(item, escape) {
                   return '<div>' + escape(item.libelle) +'</div>';
                   }
                  }")
      )
    )
  })
  
  observeEvent(data_ghm(), {
    ghm_part <- data_ghm()
    updateSelectizeInput(
      session=session, 
      inputId='chosen_ghm',
      choices=cbind(
        ghm_part,
        value=seq_len(nrow(ghm_part))
      ),
      server=TRUE,
      options=list(
        optgroups=lapply(unique(ghm_part$libelle), function(x){
          list(value=as.character(x), label=as.character(x))
        }),
        optgroupField='code',
        searchField=c('code', 'libelle'),
        labelField='code',
        render=I("{
                   option: function(item, escape) {
                   return '<div>' + escape(item.libelle) +'</div>';
                   }
                  }")
      )
    )
  })
  
  observeEvent(load_diags(), {
    codes <- data_cim()
    choices <- cbind(
      codes,
      value=seq_len(nrow(codes))
    )
    loaded_diags <- by_lists$diags_loaded$code
    star_diags <- gsub("\\*$", "", loaded_diags[grepl("\\*$", loaded_diags)])
    not_star_diags <- loaded_diags[!grepl("\\*$", loaded_diags)]
    
    if (length(star_diags) > 0) {
      star_choices <- choices[grepl(paste(star_diags, collapse="|"), choices$code), ]
      star_codes$diags <- star_choices
      not_star_choices <- choices[choices$code %in% not_star_diags, ]
      selected_diags <- rbind(not_star_choices, star_choices)
    } else {
      selected_diags <- choices[choices$code %in% not_star_diags, ]
    }
    updateSelectizeInput(
        session=session,
        inputId='chosen_diagnoses',
        choices=choices,
        selected=selected_diags$value,
        server=TRUE
      )
  })
  
  observeEvent(load_acts(), {
    req(by_lists$acts_loaded)
    acts <- data_acts()
    choices <- cbind(
      acts,
      value=seq_len(nrow(acts))
    )
    loaded_acts <- by_lists$acts_loaded$code
    star_acts <- gsub("\\*$", "", loaded_acts[grepl("\\*$", loaded_acts)])
    not_star_acts <- loaded_acts[!grepl("\\*$", loaded_acts)]
    
    if (length(star_acts) > 0) {
      star_choices <- choices[grepl(paste(star_acts, collapse="|"), choices$code), ]
      star_codes$acts <- star_choices
      not_star_choices <- choices[choices$code %in% not_star_acts, ]
      selected_acts <- rbind(not_star_choices, star_choices)
    } else {
      selected_acts <- choices[choices$code %in% not_star_acts, ]
    }
    
    updateSelectizeInput(
      session=session,
      inputId='chosen_acts',
      choices=choices,
      selected=selected_acts$value,
      server=TRUE
    )
  })
  
  observeEvent(load_ghm(), {
    ghm_ref <- data_ghm()
    choices <- cbind(
      ghm_ref,
      value=seq_len(nrow(ghm_ref))
    )
    loaded_ghm <- by_lists$ghm_loaded$code
    star_ghm <- gsub("\\*$", "", loaded_ghm[grepl("\\*$", loaded_ghm)])
    not_star_ghm <- loaded_ghm[!grepl("\\*$", loaded_ghm)]
    
    if (length(star_ghm) > 0) {
      star_choices <- choices[grepl(paste(star_ghm, collapse="|"), choices$code), ]
      star_codes$ghm <- star_choices
      not_star_choices <- choices[choices$code %in% not_star_ghm, ]
      selected_ghm <- rbind(not_star_choices, star_choices)
    } else {
      selected_ghm <- choices[choices$code %in% not_star_ghm, ]
    }
    updateSelectizeInput(
      session=session,
      inputId='chosen_ghm',
      choices=choices,
      selected=selected_ghm$value,
      server=TRUE
    )
  })
  
  by_lists <- reactiveValues(
    diags_table=NULL, diags_list=NULL, diags_loaded=NULL,
    acts_table=NULL, acts_list=NULL, acts_loaded=NULL,
    ghm_table=NULL, ghm_list=NULL, ghm_loaded=NULL
  )
  
  all_selected <- reactiveValues(diags=FALSE, acts=FALSE, ghm=FALSE)
  star_codes <- reactiveValues(diags=NULL, acts=NULL, ghm=NULL)
  diags_types <- reactiveValues(all=TRUE)
  
  observeEvent(input$diags_dpdr, {
    req(input$chosen_diagnoses)
    diags_types$all <- FALSE
    chosen_diagnoses <- input$chosen_diagnoses
    by_lists$diags_table <- data_cim()[chosen_diagnoses,]
    by_lists$diags_list <- setNames(
      as.character(by_lists$diags_table$code), by_lists$diags_table$libelle
    )
  })
  
  observeEvent(input$diags_tous, {
    req(input$chosen_diagnoses)
    diags_types$all <- TRUE
    chosen_diagnoses <- input$chosen_diagnoses
    by_lists$diags_table <- data_cim()[chosen_diagnoses,]
    by_lists$diags_list <- setNames(
      as.character(by_lists$diags_table$code), by_lists$diags_table$libelle
    )
  })
  
  observeEvent(input$acts_button, {
    req(input$chosen_acts)
    chosen_acts <- input$chosen_acts
    by_lists$acts_table <- data_acts()[chosen_acts,]
    by_lists$acts_list <- setNames(
      as.character(by_lists$acts_table$code), by_lists$acts_table$libelle
    )
  })
  
  observeEvent(input$ghm_button, {
    req(input$chosen_ghm)
    chosen_ghm <- input$chosen_ghm
    by_lists$ghm_table <- data_ghm()[chosen_ghm,]
    by_lists$ghm_list <- setNames(
      as.character(by_lists$ghm_table$code), by_lists$ghm_table$libelle
    )
  })
  
  observeEvent(input$diags_reset, {
    updateSelectizeInput(
      session=session,
      inputId='chosen_diagnoses',
      selected=character(0)
    )
    by_lists$diags_table <- NULL
    by_lists$diags_list <- NULL
    by_lists$diags_loaded <- NULL
    all_selected$diags <- FALSE
    star_codes$diags <- NULL
  })
  
  observeEvent(input$acts_reset, {
    updateSelectizeInput(
      session=session,
      inputId='chosen_acts',
      selected=character(0)
    )
    by_lists$acts_table <- NULL
    by_lists$acts_list <- NULL
    by_lists$acts_loaded <- NULL
    all_selected$acts <- FALSE
    star_codes$acts <- NULL
  })
  
  observeEvent(input$ghm_reset, {
    updateSelectizeInput(
      session=session,
      inputId='chosen_ghm',
      selected=character(0)
    )
    by_lists$ghm_table <- NULL
    by_lists$ghm_list <- NULL
    by_lists$ghm_loaded <- NULL
    all_selected$ghm <- FALSE
    star_codes$ghm <- NULL
  })
  
  observeEvent(input$diags_all, {
    by_lists$diags_table <- data_cim()
    by_lists$diags_list <- setNames(
      as.character(by_lists$diags_table$code), by_lists$diags_table$libelle
    )
    all_selected$diags <- TRUE
  })
  
  observeEvent(input$acts_all, {
    by_lists$acts_table <- data_acts()
    by_lists$acts_list <- setNames(
      as.character(by_lists$acts_table$code), by_lists$acts_table$libelle
    )
    all_selected$acts <- TRUE
  })
  
  observeEvent(input$ghm_all, {
    by_lists$ghm_table <- data_ghm()
    by_lists$ghm_list <- setNames(
      as.character(by_lists$ghm_table$code), by_lists$ghm_table$libelle
    )
    all_selected$ghm <- TRUE
  })
  
  observeEvent(load_diags(), {
    by_lists$diags_loaded <- load_diags()
  })
  
  observeEvent(load_acts(), {
    by_lists$acts_loaded <- load_acts()
  })
  
  observeEvent(load_ghm(), {
    by_lists$ghm_loaded <- load_ghm()
  })
  
  
  ############################
  ### DYNAMIC UI RENDERING ###
  ############################
  
  #### SIDEBAR ####
  observeEvent(load_data(), {
    output$dynamic_date_range <- renderUI({
      if (is.null(load_data())) {
        hide("dynamic_date_range")
      } else {
        show("dynamic_date_range")
      }
      dateRangeInput(
        inputId="date_range",
        label=h4("Période"),
        start=min(as.Date(load_data()$Date.sortie.resume, na.rm=TRUE)),
        end=max(as.Date(load_data()$Date.sortie.resume, na.rm=TRUE)),
        format="dd-mm-yyyy",
        separator="-",
        weekstart=1,
        language="fr"
      )
    })
  })
  
  observeEvent(load_data(), {
    output$dynamic_age_range <- renderUI({
      if (is.null(load_data())) {
        hide("dynamic_age_range")
      } else {
        show("dynamic_age_range")
      }
      min_age=min(load_data()$Age)
      max_age=max(load_data()$Age)
      sliderInput(
        inputId="age_filter",
        label=h4("Filtrer par âge"),
        min=min_age, 
        max=max_age,
        value=c(min_age, max_age),
        ticks=FALSE
      )
    })
  })
  
  observeEvent(load_data(), {
    output$dynamic_UH <- renderUI({
      if (is.null(load_data())) {
        hide("dynamic_UH")
      } else {
        show("dynamic_UH")
      }
      selectizeInput(
        inputId='UH_filter',
        label=h4('Filtrer par UH'),
        choices=load_data()$UH,
        selected=load_data()$UH,
        multiple=TRUE,
        options=list(
          placeholder='Taper et/ou sélectionner'
        )
      )
    })
  })
  
  observeEvent(load_data(), {
    output$dynamic_cmd <- renderUI({
      if (is.null(load_data())) {
        hide("dynamic_cmd")
      } else {
        show("dynamic_cmd")
      }
      selectizeInput(
        inputId='cmd_filter',
        label=h4('CMD'),
        choices=NULL,
        multiple=TRUE,
        options=list(
          placeholder='Taper et/ou sélectionner'
        )
      )
    })
  })
  
  observeEvent(load_data(), {
    cmd_part <- data_cmd()
    updateSelectizeInput(
      session=session,
      inputId='cmd_filter',
      choices=cmd_part,
      selected=cmd_part$value,
      server=TRUE,
      options=list(
        optgroups=lapply(unique(cmd_part$libelle), function(x){
          list(value=as.character(x), label=as.character(x))
        }),
        optgroupField='code',
        searchField=c('code', 'libelle'),
        labelField='code',
        render=I("{
                 option: function(item, escape) {
                    return '<div>' + escape(item.libelle) +'</div>';
                   }
                 }"
        )
      )
    )
  })
  
  observeEvent(load_data(), {
    output$dynamic_GHM_lettre <- renderUI({
      if (is.null(load_data())) {
        hide("dynamic_GHM_lettre")
      } else {
        show("dynamic_GHM_lettre")
      }
      selectizeInput(
        inputId='GHM_lettre_filter',
        label=h4('Catégorie de GHM'),
        choices=NULL,
        multiple=TRUE,
        options=list(
          placeholder='Taper et/ou sélectionner'
        )
      )
    })
  })
  
  observeEvent(load_data(), {
    updateSelectizeInput(
      session, 'GHM_lettre_filter',
      choices=load_data()$GHM_lettre,
      selected=load_data()$GHM_lettre,
      server=TRUE
    )
  })
  
  observeEvent(load_data(), {
    output$dynamic_mode_ent <- renderUI({
      if (is.null(load_data())) {
        hide("dynamic_mode_ent")
      } else {
        show("dynamic_mode_ent")
      }
      selectizeInput(
        inputId='mode_ent_filter',
        label=h4("Mode d'entrée"),
        choices=NULL,
        multiple=TRUE
      )
    })
  })
  
  observeEvent(load_data(), {
    updateSelectizeInput(
      session, 'mode_ent_filter',
      choices=load_data()$Mode.ent,
      selected=load_data()$Mode.ent,
      server=TRUE
    )
  })
  
  observeEvent(load_data(), {
    output$dynamic_dad <- renderUI({
      selectInput(
        inputId='dad_filter',
        label=h4("Filtrer DAD en MR"),
        choices=c("Oui", "Non"),
        selected="Non",
        multiple=FALSE
      )
    })
  })
  
  #### TABS ####
  
  observeEvent(diags_table(), {
    output$dynamic_diags_tables <- renderUI({
      if (is.null(diags_table())) {
        hide("dynamic_diags_tables")
      } else {
        show("dynamic_diags_tables")
      }
      fluidRow(
        box(
          DTOutput("diags_table"),
          title="Décompte par diagnostic",
          width=12
        )
      )
    })
  })
  
  observeEvent(acts_table(), {
    output$dynamic_acts_tables <- renderUI({
      if (is.null(acts_table())) {
        hide("dynamic_acts_tables")
      } else {
        show("dynamic_acts_tables")
      }
      fluidRow(
        box(
          DTOutput("acts_table"),
          title="Décompte par acte",
          width=12
        )
      )
    })
  })
  
  observeEvent(ghm_table(), {
    output$dynamic_ghm_tables <- renderUI({
      if (is.null(ghm_table())) {
        hide("dynamic_ghm_tables")
      } else {
        show("dynamic_ghm_tables")
      }
      fluidRow(
        box(
          DTOutput("ghm_table"),
          title="Décompte par GHM",
          width=12
        )
      )
    })
  })
  
  observeEvent(diags_table(), {
    output$dynamic_diags_summary <- renderUI({
      if (is.null(diags_table())) {
        hide("dynamic_diags_summary")
      } else {
        show("dynamic_diags_summary")
      }
      fluidRow(
        box(
          DTOutput("diags_summary"),
          title="Décompte par diagnostic",
          width=12
        )
      )
    })
  })
  
  observeEvent(acts_table(), {
    output$dynamic_acts_summary <- renderUI({
      if (is.null(acts_table())) {
        hide("dynamic_acts_summary")
      } else {
        show("dynamic_acts_summary")
      }
      fluidRow(
        box(
          DTOutput("acts_summary"),
          title="Décompte par acte",
          width=12
        )
      )
    })
  })
  
  observeEvent(ghm_table(), {
    output$dynamic_ghm_summary <- renderUI({
      if (is.null(ghm_table())) {
        hide("dynamic_ghm_summary")
      } else {
        show("dynamic_ghm_summary")
      }
      fluidRow(
        box(
          DTOutput("ghm_summary"),
          title="Décompte par GHM",
          width=12
        )
      )
    })
  })
  
  observeEvent(age_table(), {
    output$dynamic_age_histogram <- renderUI({
      if (is.null(age_table())) {
        hide("dynamic_age_histogram")
      } else {
        show("dynamic_age_histogram")
      }
      fluidRow(
        box(plotOutput("age_histogram", height="370px"), width=6),
        box(
          DTOutput("age_table"), 
          title="Répartition des âges",
          width=6
        )
      )
    })
  })
  
  observeEvent(age_histogram_by_diags(), {
    output$dynamic_age_histogram_by_diags <- renderUI({
      if (is.null(age_histogram_by_diags())) {
        hide("dynamic_age_histogram_by_diags")
      } else {
        show("dynamic_age_histogram_by_diags")
      }
      fluidRow(
        box(
          plotOutput("age_histogram_by_diags", height="370px"), 
          width=6
        ),
        box(
          DTOutput("age_table_by_diags"), 
          title="Répartition des âges",
          width=6
        )
      )
    })
  })
  
  observeEvent(age_histogram_by_acts(), {
    output$dynamic_age_histogram_by_acts <- renderUI({
      if (is.null(age_histogram_by_acts())) {
        hide("dynamic_age_histogram_by_acts")
      } else {
        show("dynamic_age_histogram_by_acts")
      }
      fluidRow(
        box(
          plotOutput("age_histogram_by_acts", height="370px"), 
          width=6
        ),
        box(
          DTOutput("age_table_by_acts"), 
          title="Répartition des âges",
          width=6
        )
      )
    })
  })
  
  observeEvent(age_histogram_by_ghm(), {
    output$dynamic_age_histogram_by_ghm <- renderUI({
      if (is.null(age_histogram_by_ghm())) {
        hide("dynamic_age_histogram_by_ghm")
      } else {
        show("dynamic_age_histogram_by_ghm")
      }
      fluidRow(
        box(
          plotOutput("age_histogram_by_ghm", height="370px"), 
          width=6
        ),
        box(
          DTOutput("age_table_by_ghm"), 
          title="Répartition des âges",
          width=6
        )
      )
    })
  })
  
  observeEvent(severite_table(), {
    output$dynamic_severite_histogram <- renderUI({
      if (is.null(severite_table())) {
        hide("dynamic_severite_histogram")
      } else {
        show("dynamic_severite_histogram")
      }
      fluidRow(
        box(
          DTOutput("severite_table"),
          title="Sévérité", 
          width=6
        ),
        box(
          DTOutput("GHM_lettre_table"),
          title="Catégories de GHM", 
          width=6
        )
      )
    })
  })
  
  observeEvent(severite_table_by_diags(), {
    output$dynamic_severite_histogram_by_diags <- renderUI({
      if (is.null(severite_table_by_diags())) {
        hide("dynamic_severite_histogram_by_diags")
      } else {
        show("dynamic_severite_histogram_by_diags")
      }
      fluidRow(
        box(
          DTOutput("severite_table_by_diags"),
          title="Sévérité", 
          width=6
        ),
        box(
          DTOutput("GHM_lettre_table_by_diags"),
          title="Catégories de GHM", 
          width=6
        )
      )
    })
  })
  
  observeEvent(severite_table_by_acts(), {
    output$dynamic_severite_histogram_by_acts <- renderUI({
      if (is.null(severite_table_by_acts())) {
        hide("dynamic_severite_histogram_by_acts")
      } else {
        show("dynamic_severite_histogram_by_acts")
      }
      fluidRow(
        box(
          DTOutput("severite_table_by_acts"),
          title="Sévérité", 
          width=6
        ),
        box(
          DTOutput("GHM_lettre_table_by_acts"),
          title="Catégories de GHM", 
          width=6
        )
      )
    })
  })
  
  observeEvent(severite_table_by_ghm(), {
    output$dynamic_severite_histogram_by_ghm <- renderUI({
      if (is.null(severite_table_by_ghm())) {
        hide("dynamic_severite_histogram_by_ghm")
      } else {
        show("dynamic_severite_histogram_by_ghm")
      }
      fluidRow(
        box(
          DTOutput("severite_table_by_ghm"),
          title="Sévérité", 
          width=6
        ),
        box(
          DTOutput("GHM_lettre_table_by_ghm"),
          title="Catégories de GHM", 
          width=6
        )
      )
    })
  })
  
  observeEvent(mode_ent_table(), {
    output$dynamic_mode_histogram <- renderUI({
      if (is.null(mode_ent_table())) {
        hide("dynamic_mode_histogram")
      } else {
        show("dynamic_mode_histogram")
      }
      fluidRow(
        box(
          DTOutput("mode_ent_table"),
          title="Mode d'entrée", 
          width=6
        ),
        box(
          DTOutput("mode_sor_table"),
          title="Mode de sortie", 
          width=6
        )
      )
    })
  })
  
  observeEvent(mode_ent_table_by_diags(), {
    output$dynamic_mode_histogram_by_diags <- renderUI({
      if (is.null(mode_ent_table_by_diags())) {
        hide("dynamic_mode_histogram_by_diags")
      } else {
        show("dynamic_mode_histogram_by_diags")
      }
      fluidRow(
        box(
          DTOutput("mode_ent_table_by_diags"),
          title="Mode d'entrée", 
          width=6
        ),
        box(
          DTOutput("mode_sor_table_by_diags"),
          title="Mode de sortie", 
          width=6
        )
      )
    })
  })
  
  observeEvent(mode_ent_table_by_acts(), {
    output$dynamic_mode_histogram_by_acts <- renderUI({
      if (is.null(mode_ent_table_by_acts())) {
        hide("dynamic_mode_histogram_by_acts")
      } else {
        show("dynamic_mode_histogram_by_acts")
      }
      fluidRow(
        box(
          DTOutput("mode_ent_table_by_acts"),
          title="Mode d'entrée", 
          width=6
        ),
        box(
          DTOutput("mode_sor_table_by_acts"),
          title="Mode de sortie", 
          width=6
        )
      )
    })
  })
  
  observeEvent(mode_ent_table_by_ghm(), {
    output$dynamic_mode_histogram_by_ghm <- renderUI({
      if (is.null(mode_ent_table_by_ghm())) {
        hide("dynamic_mode_histogram_by_ghm")
      } else {
        show("dynamic_mode_histogram_by_ghm")
      }
      fluidRow(
        box(
          DTOutput("mode_ent_table_by_ghm"),
          title="Mode d'entrée", 
          width=6
        ),
        box(
          DTOutput("mode_sor_table_by_ghm"),
          title="Mode de sortie", 
          width=6
        )
      )
    })
  })
  
  observeEvent(URM_origine_table(), {
    output$dynamic_prov_histogram <- renderUI({
      if (is.null(URM_origine_table())) {
        hide("dynamic_prov_histogram")
      } else {
        show("dynamic_prov_histogram")
      }
      fluidRow(
        box(
          DTOutput("URM_origine_table"),
          title="URM d'origine", 
          width=6
        ),
        box(
          DTOutput("URM_destination_table"),
          title="URM de destination", 
          width=6
        )
      )
    })
  })
  
  observeEvent(URM_origine_table_by_diags(), {
    output$dynamic_prov_histogram_by_diags <- renderUI({
      if (is.null(URM_origine_table_by_diags())) {
        hide("dynamic_prov_histogram_by_diags")
      } else {
        show("dynamic_prov_histogram_by_diags")
      }
      fluidRow(
        box(
          DTOutput("URM_origine_table_by_diags"),
          title="URM d'origine", 
          width=6
        ),
        box(
          DTOutput("URM_destination_table_by_diags"),
          title="URM de destination", 
          width=6
        )
      )
    })
  })
  
  observeEvent(URM_origine_table_by_acts(), {
    output$dynamic_prov_histogram_by_acts <- renderUI({
      if (is.null(URM_origine_table_by_acts())) {
        hide("dynamic_prov_histogram_by_acts")
      } else {
        show("dynamic_prov_histogram_by_acts")
      }
      fluidRow(
        box(
          DTOutput("URM_origine_table_by_acts"),
          title="URM d'origine", 
          width=6
        ),
        box(
          DTOutput("URM_destination_table_by_acts"),
          title="URM de destination", 
          width=6
        )
      )
    })
  })
  
  observeEvent(URM_origine_table_by_ghm(), {
    output$dynamic_prov_histogram_by_ghm <- renderUI({
      if (is.null(URM_origine_table_by_ghm())) {
        hide("dynamic_prov_histogram_by_ghm")
      } else {
        show("dynamic_prov_histogram_by_ghm")
      }
      fluidRow(
        box(
          DTOutput("URM_origine_table_by_ghm"),
          title="URM d'origine", 
          width=6
        ),
        box(
          DTOutput("URM_destination_table_by_ghm"),
          title="URM de destination", 
          width=6
        )
      )
    })
  })
  
  observeEvent(diags_categorie_stats(), {
    output$dynamic_categorie_diags <- renderUI({
      if (is.null(diags_categorie_stats())) {
        hide("dynamic_categorie_diags")
      } else {
        show("dynamic_categorie_diags")
      }
      fluidRow(
        box(
          DTOutput("diags_categorie_stats"),
          title="Statistiques sur la catégorie de diagnostic",
          width=12
        )
      )
    })
  })
  
  observeEvent(acts_categorie_stats(), {
    output$dynamic_categorie_acts <- renderUI({
      if (is.null(acts_categorie_stats())) {
        hide("dynamic_categorie_acts")
      } else {
        show("dynamic_categorie_acts")
      }
      fluidRow(
        box(
          DTOutput("acts_categorie_stats"),
          title="Statistiques sur la catégorie d'acte",
          width=12
        )
      )
    })
  })
  
  observeEvent(ghm_categorie_stats(), {
    output$dynamic_categorie_ghm <- renderUI({
      if (is.null(ghm_categorie_stats())) {
        hide("dynamic_categorie_ghm")
      } else {
        show("dynamic_categorie_ghm")
      }
      fluidRow(
        box(
          DTOutput("ghm_categorie_stats"),
          title="Statistiques sur la catégorie de GHM",
          width=12
        )
      )
    })
  })
  
  observeEvent(geographic_global(), {
    output$dynamic_geographic_global <- renderUI({
      if (is.null(geographic_global())) {
        hide("dynamic_geographic_global")
      } else {
        show("dynamic_geographic_global")
      }
      fluidRow(
        box(
          DTOutput("geographic_global"),
          title="Répartition géographique",
          width=12
        )
      )
    })
  })
  
  observeEvent(geographic_by_diags(), {
    output$dynamic_geographic_by_diags <- renderUI({
      if (is.null(geographic_by_diags())) {
        hide("dynamic_geographic_by_diags")
      } else {
        show("dynamic_geographic_by_diags")
      }
      fluidRow(
        box(
          DTOutput("geographic_by_diags"),
          title="Répartition géographique pour les diagnostics sélectionnés",
          width=12
        )
      )
    })
  })
  
  observeEvent(geographic_by_acts(), {
    output$dynamic_geographic_by_acts <- renderUI({
      if (is.null(geographic_by_acts())) {
        hide("dynamic_geographic_by_acts")
      } else {
        show("dynamic_geographic_by_acts")
      }
      fluidRow(
        box(
          DTOutput("geographic_by_acts"),
          title="Répartition géographique pour les actes sélectionnés",
          width=12
        )
      )
    })
  })
  
  observeEvent(geographic_by_ghm(), {
    output$dynamic_geographic_by_ghm <- renderUI({
      if (is.null(geographic_by_ghm())) {
        hide("dynamic_geographic_by_ghm")
      } else {
        show("dynamic_geographic_by_ghm")
      }
      fluidRow(
        box(
          DTOutput("geographic_by_ghm"),
          title="Répartition géographique pour les GHM sélectionnés",
          width=12
        )
      )
    })
  })
  
  observeEvent(data(), {
    output$dynamic_evol_global <- renderUI({
      if (is.null(data())) {
        hide("dynamic_evol_global")
      } else {
        show("dynamic_evol_global")
      }
      min_year=min(data()$annee.sortie, na.rm=TRUE)
      max_year=max(data()$annee.sortie, na.rm=TRUE)
      fluidRow(
        box(
          div(
            style="display:inline-block;vertical-align:top;width:49%;float:left", 
            sliderInput(
              inputId="evol_global_year_filter",
              label="Filtrer par années",
              min=min_year, 
              max=max_year,
              value=c(min_year, max_year),
              step=1,
              ticks=FALSE,
              sep=""
            )
          ),
          div(
            style="display:inline-block;vertical-align:top;width:49%;float:right",
            sliderInput(
              inputId="evol_global_month_filter",
              label="Filtrer par mois",
              min=1, 
              max=12,
              value=c(1, 12),
              step=1,
              ticks=FALSE
            )
          ),
          DTOutput("evol_table_global"),
          title="Évolution",
          width=12
        )
      )
    })
  })
  
  observeEvent(data_by_diags(), {
    output$dynamic_evol_by_diags <- renderUI({
      if (is.null(data_by_diags())) {
        hide("dynamic_evol_by_diags")
      } else {
        show("dynamic_evol_by_diags")
      }
      min_year=min(data_by_diags()$annee.sortie, na.rm=TRUE)
      max_year=max(data_by_diags()$annee.sortie, na.rm=TRUE)
      fluidRow(
        box(
          div(
            style="display:inline-block;vertical-align:top;width:49%;float:left", 
            sliderInput(
              inputId="evol_diags_year_filter",
              label="Filtrer par années",
              min=min_year, 
              max=max_year,
              value=c(min_year, max_year),
              ticks=FALSE,
              sep=""
            )
          ),
          div(
            style="display:inline-block;vertical-align:top;width:49%;float:right",
            sliderInput(
              inputId="evol_diags_month_filter",
              label="Filtrer par mois",
              min=1, 
              max=12,
              value=c(1, 12),
              ticks=FALSE
            )
          ),
          DTOutput("evol_table_by_diags"),
          title="Évolution",
          width=12
        )
      )
    })
  })
  
  observeEvent(data_by_acts(), {
    output$dynamic_evol_by_acts <- renderUI({
      if (is.null(data_by_acts())) {
        hide("dynamic_evol_by_acts")
      } else {
        show("dynamic_evol_by_acts")
      }
      min_year=min(data_by_acts()$annee.sortie, na.rm=TRUE)
      max_year=max(data_by_acts()$annee.sortie, na.rm=TRUE)
      fluidRow(
        box(
          div(
            style="display:inline-block;vertical-align:top;width:49%;float:left", 
            sliderInput(
              inputId="evol_acts_year_filter",
              label="Filtrer par années",
              min=min_year, 
              max=max_year,
              value=c(min_year, max_year),
              ticks=FALSE,
              sep=""
            )
          ),
          div(
            style="display:inline-block;vertical-align:top;width:49%;float:right",
            sliderInput(
              inputId="evol_acts_month_filter",
              label="Filtrer par mois",
              min=1, 
              max=12,
              value=c(1, 12),
              ticks=FALSE
            )
          ),
          DTOutput("evol_table_by_acts"),
          title="Évolution",
          width=12
        )
      )
    })
  })
  
  observeEvent(data_by_ghm(), {
    output$dynamic_evol_by_ghm <- renderUI({
      if (is.null(data_by_ghm())) {
        hide("dynamic_evol_by_ghm")
      } else {
        show("dynamic_evol_by_ghm")
      }
      min_year=min(data_by_ghm()$annee.sortie, na.rm=TRUE)
      max_year=max(data_by_ghm()$annee.sortie, na.rm=TRUE)
      fluidRow(
        box(
          div(
            style="display:inline-block;vertical-align:top;width:49%;float:left", 
            sliderInput(
              inputId="evol_ghm_year_filter",
              label="Filtrer par années",
              min=min_year, 
              max=max_year,
              value=c(min_year, max_year),
              ticks=FALSE,
              sep=""
            )
          ),
          div(
            style="display:inline-block;vertical-align:top;width:49%;float:right",
            sliderInput(
              inputId="evol_ghm_month_filter",
              label="Filtrer par mois",
              min=1, 
              max=12,
              value=c(1, 12),
              ticks=FALSE
            )
          ),
          DTOutput("evol_table_by_ghm"),
          title="Évolution",
          width=12
        )
      )
    })
  })
  
  #####################################
  ### FUNCTIONS TO GENERATE OBJECTS ###
  #####################################
  
  data_by <- function(data, by_column) {
    if ((by_column == "diagnoses") | (by_column == "dpdr")) {
      by_list=by_lists$diags_list
    } else if (by_column == "acts") {
      by_list=by_lists$acts_list
    } else if (by_column == "GHM") {
      by_list=by_lists$ghm_list
    }
    df <- data
    if (nrow(data) > 0) {
      df <- (
        df[sapply(df[[by_column]], function(x) any(by_list %in% x)),]
      )
    } else {
      df <- data.frame(Age=numeric(0), GHM.lettre=character(0))
    }
    return(df)
  }
  
  table_by <- function(data_by, by_column){
    withProgress(
      message="Chargement de la table",{
      df <- data_by
      if ((by_column == "diagnoses") | (by_column == "dpdr")) {
        by_list=by_lists$diags_list
        by_table=by_lists$diags_table
      } else if (by_column == "acts") {
        by_list=by_lists$acts_list
        by_table=by_lists$acts_table
      } else if (by_column == "GHM") {
        by_list=by_lists$ghm_list
        by_table=by_lists$ghm_table
      }
      n_by=data.frame(
        n_sejours=integer(0), 
        n_patients=integer(0),
        tot_sej=integer(0),
        moy_sej=numeric(0),
        min_sej=numeric(0),
        max_sej=numeric(0),
        urgences=integer(0)
      )
      if (nrow(df) > 0) {
        for(element in by_list){
          incProgress(1/length(by_list))
          df[[element]]=(
            apply(
              df, 1, function(x) element %in% unlist(x[[by_column]])
            )
          )
          n_sejour=sum(
            tapply(
              df[[by_column]], df[["NDA"]],
              function(x) element %in% unlist(c(x))
            )
          )
          n_patient=sum(
            tapply(
              df[[by_column]], df[["NIP"]],
              function(x) element %in% unlist(c(x))
            )
          )
          total_sejour=sum(
            unique(df[df[[element]]==1, c("NDA", "Duree.sejour")])$Duree.sejour
          )
          moyenne_sejour=mean(
            unique(df[df[[element]]==1, c("NDA", "Duree.sejour")])$Duree.sejour, 
            na.rm=TRUE
          )
          min_sej=min(df[df[[element]]==1, "Duree.sejour"], na.rm=TRUE)
          max_sej=max(df[df[[element]]==1, "Duree.sejour"], na.rm=TRUE)
          entree_urgences=nrow(
            df[(df[[element]]==1) & (df[["Mode.ent"]]=="Urgences"), ]
          )
          
          n_by[element, ] <- c(
            n_sejour,
            n_patient,
            total_sejour,
            round(moyenne_sejour, digits=2),
            min_sej,
            max_sej,
            entree_urgences
          )
        }
      } else {
        for(element in by_list){
          n_by[element, ] <- c(
            0,
            0,
            0,
            round(0, digits=2),
            0,
            0,
            0
          )
        }
      }
      n_by <- merge(
        by_table, n_by,
        by.x="code", by.y="row.names",
        all.x=FALSE, all.y=TRUE
      )
      n_by$code <- as.character(n_by$code)
      return(n_by)
    })
  }
  
  evol_table_by <- function(data, by_column) {
    if (by_column == "diagnoses") {
      min_year <- input$evol_diags_year_filter[1]
      max_year <- input$evol_diags_year_filter[2]
      min_month <- input$evol_diags_month_filter[1]
      max_month <- input$evol_diags_month_filter[2]
    } else if (by_column == "acts") {
      min_year <- input$evol_acts_year_filter[1]
      max_year <- input$evol_acts_year_filter[2]
      min_month <- input$evol_acts_month_filter[1]
      max_month <- input$evol_acts_month_filter[2]
    } else if (by_column == "GHM") {
      min_year <- input$evol_ghm_year_filter[1]
      max_year <- input$evol_ghm_year_filter[2]
      min_month <- input$evol_ghm_month_filter[1]
      max_month <- input$evol_ghm_month_filter[2]
    } else if (by_column == "global"){
      min_year <- input$evol_global_year_filter[1]
      max_year <- input$evol_global_year_filter[2]
      min_month <- input$evol_global_month_filter[1]
      max_month <- input$evol_global_month_filter[2]
    }
    data <- data[
      (data$annee.sortie >= min_year)
      & (data$annee.sortie <= max_year)
      & (data$mois.sortie >= min_month)
      & (data$mois.sortie <= max_month), 
    ]
    evol_list <- list()
    if (length(unique(data$annee.sortie)) > 1) {
      period_list <- sort(unique(data$annee.sortie))
      for (period in period_list) {
        evol_list[[as.character(period)]] <- (
          global_stats_by(data[data$annee.sortie == period,])
        )
      }
    } else {
      period_list <- sort(unique(data$mois.sortie))
      for (period in period_list) {
        evol_list[[as.character(period)]] <- (
          global_stats_by(data[data$mois.sortie == period,])
        )
      }
      names(evol_list) <- month.abb[as.numeric(names(evol_list))]
    }
    table_df <- data.frame(evol_list)
    colnames(table_df) <- gsub("X", "", colnames(table_df))
    return(table_df)
  }
  
  global_stats_by <- function(data){
    n_sejour_global <- length(unique(data$NDA))
    n_patient_global <- length(unique(data$NIP))
    total_sejour_global <- sum(
      unique(data[, c("NDA", "Duree.sejour")])$Duree.sejour
    )
    moyenne_sejour_global <- round(
      mean(unique(data[, c("NDA", "Duree.sejour")])$Duree.sejour, na.rm=TRUE), 
      digits=3
    )
    global_stats=c(
      n_sejour_global,
      n_patient_global,
      total_sejour_global,
      moyenne_sejour_global
    )
    names(global_stats) <- c(
      "n_sejours",
      "n_patients",
      "total_sejour",
      "moyenne_sejour"
    )
    return(global_stats)
  }
  
  value_box_by <- function(stats_table, stat) {
    if (stat == "moyenne_sejour") {
      value_box <- valueBox(
        unlist(stats_table[stat]), 
        "Durée moyenne de séjour", 
        icon=icon("clock"),
        color="yellow"
      )
    } else if (stat == "total_sejour") {
      valueBox(
        unlist(stats_table[stat]), 
        "Total des durées de séjour", 
        icon=icon("clock"),
        color="yellow"
      )
    } else if (stat == "n_patients") {
      valueBox(
        unlist(stats_table[stat]), 
        "Nombre de patients", 
        icon=icon("list"),
        color="purple"
      )
    } else if (stat == "n_sejours") {
      valueBox(
        unlist(stats_table[stat]), 
        "Nombre de séjours", 
        icon=icon("list"),
        color="purple"
      )
    }
  } 
  
  plot_age_by <- function(data) {
    age_histogram <- ggplot(unique(data[, c("NDA", "Age")]), aes(x=Age)) + 
      geom_bar(color="coral", fill="coral", alpha=0.3) +
      labs(x="Age", y="Effectifs")
    return(age_histogram)
  }
  
  age_table_by <- function(data) {
    age_cat <- cut(
      unique(data[, c("NDA", "Age")])$Age, 
      breaks=c(0, 18, 25, 40, 60, 80, 100, 200),
      labels=c("<18", "18-25", "25-40", "40-60", "60-80", "80-100", "100+"),
      right=FALSE
    )
    
    age_table <- data.frame(table(age_cat))
    colnames(age_table) <- c("Âge", "n_sejours")
    age_table$`%` <- round((100 * age_table$n_sejours) / length(unique(data$NDA)), digits=2)
    return(age_table)
  }
  
  GHM_lettre_by <- function(data){
    ghm_lettre_table <- data.frame(
      table(data$GHM_lettre, dnn="Lettre.GHM")
    )
    ghm_lettre_table$Lettre.GHM <- (
      sprintf("%s", as.character(levels(ghm_lettre_table$Lettre.GHM)))
    )
    GHM_output <- merge(
      ghm_lettre_table, ghm, 
      by.x="Lettre.GHM", by.y="code", 
      all.x=TRUE, all.y=FALSE
    )
    rownames(GHM_output) <- GHM_output$Lettre.GHM
    GHM_output <- (
      GHM_output[
        order(GHM_output$Freq, decreasing=TRUE),
        c("libelle", "Freq")
        ]
    )
    GHM_output$`%` <- round((100 * GHM_output$Freq) / length(unique(data$NDA)), digits=2)
    GHM_output <- GHM_output %>% rename("n_sejours"="Freq")
    return(GHM_output)
  }
  
  URM_by <- function(data, direction){
    etablissement <- etablissement()
    if (etablissement == "bct") {
      URM_list <- read.csv("urm_bct.csv", strip.white=TRUE, header=TRUE)
    } else if (etablissement == "pbr") {
      URM_list <- read.csv2("urm_pbr.csv", strip.white=TRUE, header=TRUE)
    } else {
      URM_list <- data.frame(URM=character(0), libelle=character(0))
    }
    
    if (direction == "origine") {
      URM_table <- data.frame(
        table(data$URM.orig, dnn=direction)
      )
    } else if (direction == "destination") {
      URM_table <- data.frame(
        table(data$URM.dest, dnn=direction)
      )
    }
    URM_table[[direction]] <- (
      sprintf("%03d", as.numeric(levels(URM_table[[direction]])))
    )
    URM_output <- merge(
      URM_table, URM_list, 
      by.x=direction, by.y="URM", 
      all.x=TRUE, all.y=FALSE
    )
    rownames(URM_output) <- URM_output[[direction]]
    URM_output <- (
      URM_output[
        order(URM_output$Freq, decreasing=TRUE),
        c("libelle", "Freq")
        ]
    )
    URM_output$`%` <- round((100 * URM_output$Freq) / length(unique(data$NDA)), digits=2)
    URM_output <- URM_output %>% rename("n_sejours"="Freq")
    return(URM_output)
  }
  
  geographic_by <- function(data){
    df <- data
    df$is_idf <- substr(df$Code.post.Resid, 1, 2) %in% idf_postal
    df$is_france <- (
      (
        as.character(substr(df$Code.post.Resid, 1, 2)) 
        %in% sprintf("%02d", 1:97)
      )
    )
    n_patient_idf=sum(
      tapply(
        df$is_idf, df$NIP,
        function(x) sum(x) > 0
      )
    )
    n_patient_france=sum(
      tapply(
        df$is_france, df$NIP,
        function(x) sum(x) > 0
      )
    )
    n_sejour_idf <- sum(
      tapply(
        df$is_idf, df$NDA,
        function(x) sum(x) > 0
      )
    )
    n_sejour_france <- sum(
      tapply(
        df$is_france, df$NDA,
        function(x) sum(x) > 0
      )
    )
    tot_sejour=length(unique(df$NDA))
    tot_patient=length(unique(df$NIP))
    
    geographic_global=data.frame(
      "n_sejours"=integer(0), 
      "% sejours"=numeric(0),
      "n_patients"=integer(0),
      "% patients"=numeric(0)
    )
    colnames(geographic_global) <- c(
      "n_sejours", 
      "% sejours",
      "n_patients",
      "% patients"
    )
    geographic_global["Île-de-France", ] <- c(
      n_sejour_idf, round(100 * (n_sejour_idf / tot_sejour), digits=2),
      n_patient_idf, round(100 * (n_patient_idf / tot_patient), digits=2)
    )
    geographic_global["France métropolitaine", ] <- c(
      n_sejour_france, round(100 * (n_sejour_france / tot_sejour), digits=2),
      n_patient_france, round(100 * (n_patient_france / tot_patient), digits=2)
    )
    return(geographic_global)
  }
  
  mode_ent_by <- function(data) {
    mode_ent_table <- data.frame(
      table(data$Mode.ent, dnn="Mode entree")
    )
    mode_ent_table$Mode.entree <- (
      sprintf("%s", levels(mode_ent_table$Mode.entree))
    )
    output <- (
      mode_ent_table[order(mode_ent_table$Freq, decreasing=TRUE), ]
    )
    output$`%` <- round((100 * output$Freq) / length(unique(data$NDA)), digits=2)
    output <- output %>% rename("n_sejours"="Freq")
    return(output)
  }
  
  mode_sor_by <- function(data) {
    mode_sor_table <- data.frame(
      table(data$Mode.sor, dnn="Mode sortie")
    )
    mode_sor_table$Mode.sortie <- (
      sprintf("%s", levels(mode_sor_table$Mode.sortie))
    )
    output <- (
      mode_sor_table[order(mode_sor_table$Freq, decreasing=TRUE), ]
    )
    output$`%` <- round((100 * output$Freq) / length(unique(data$NDA)), digits=2)
    output <- output %>% rename("n_sejours"="Freq")
    return(output)
  }
  
  etablissement <- reactive({
    data <- load_data()
    nda <- data$NDA[1]
    etab_code <- substr(nda, 1, 3)
    if ((etab_code == 101) | (etab_code == 102)) {
      etablissement="bct"
    } else if (etab_code == 961) {
      etablissement="pbr"
    } else {
      etablissement="unk"
    }
    return(etablissement)
  })
  
  severite_by <- function(data) {
    severite_table <- data.frame(
      table(substr(data$GHM, 6, 6), dnn="Sévérité")
    )
    severite_table$`Sévérité` <- (
      sprintf("%s", levels(severite_table$`Sévérité`))
    )
    output <- (
      severite_table[order(severite_table$`Sévérité`), ]
    )
    output$`%` <- round((100 * output$Freq) / length(unique(data$NDA)), digits=2)
    output <- output %>% rename("n_sejours"="Freq")
    return(output)
  }
  
  #################################
  ### GENERATE TABLES AND PLOTS ###
  #################################
  
  data_by_diags <- reactive({
    req(data())
    req(by_lists$diags_list)
    if (diags_types$all == TRUE) {
      data_by(data(), "diagnoses")
    } else if (diags_types$all == FALSE) {
      data_by(data(), "dpdr")
    }
  })
  
  data_by_acts <- reactive({
    req(data())
    req(by_lists$acts_list)
    data_by(data(), "acts")
  })
  
  data_by_ghm <- reactive({
    req(data())
    req(by_lists$ghm_list)
    data_by(data(), "GHM")
  })
  
  mr_list <- reactive({
    req(input$dad_filter, data())
    if (input$dad_filter == "Oui") {
      dad <- unique(unlist(data()$dad))
      return(grep("^MR", dad, value=TRUE))
    } else {
      return("NA")
    }
  })
  
  diags_given <- reactive({
    if (is.null(by_lists$diags_list)) {
      return("NA")
    } else if (all_selected$diags == TRUE) {
      return("Tous")
    } else if (is.null(input$diags_file)) {
      return(unname(by_lists$diags_list))
    } else {
      diags <- unique(
        unlist(c(unname(by_lists$diags_list), by_lists$diags_loaded$code))
      )
      return(setdiff(diags, star_codes$diags))
    }
  })
  
  acts_given <- reactive({
    if (is.null(by_lists$acts_list)) {
      return("NA")
    } else if (all_selected$acts == TRUE) {
      return("Tous")
    } else if (is.null(input$acts_file)) {
      return(unname(by_lists$acts_list))
    } else {
      acts <- unique(
        unlist(c(unname(by_lists$acts_list), by_lists$acts_loaded$code))
      )
      return(setdiff(acts, star_codes$acts$code))
    }
  })
  
  ghm_given <- reactive({
    if (is.null(by_lists$ghm_list)) {
      return("NA")
    } else if (all_selected$ghm == TRUE) {
      return("Tous")
    } else if (is.null(input$ghm_file)) {
      return(unname(by_lists$ghm_list))
    } else {
      ghm_list <- unique(
        unlist(c(unname(by_lists$ghm_list), by_lists$ghm_loaded$code))
      )
      return(setdiff(ghm_list, star_codes$ghm_list))
    }
  })
  
  diags_table <- reactive({
    req(data_by_diags())
    if (diags_types$all == TRUE) {
      n_by_diags <- table_by(data_by_diags(), "diagnoses")
    } else if (diags_types$all == FALSE) {
      n_by_diags <- table_by(data_by_diags(), "dpdr")
    }
    return(n_by_diags)
  })
  
  acts_table <- reactive({
    req(data_by_acts())
    n_by_acts <- table_by(data_by_acts(), "acts")
    return(n_by_acts)
  })
  
  ghm_table <- reactive({
    req(data_by_ghm())
    n_by_ghm <- table_by(data_by_ghm(), "GHM")
    n_by_ghm$onco <- as.factor(
      ifelse(n_by_ghm$code %in% ghm_cancero$code, "oui", "non")
    )
    return(n_by_ghm)
  })
  
  diags_summary <- reactive({
    req(data_by_diags())
    if (diags_types$all == TRUE) {
      n_by_diags <- table_by(data_common(), "diagnoses")
    } else if (diags_types$all == FALSE) {
      n_by_diags <- table_by(data_common(), "dpdr")
    }
    return(n_by_diags)
  })
  
  acts_summary <- reactive({
    req(data_by_acts())
    n_by_acts <- table_by(data_common(), "acts")
    return(n_by_acts)
  })
  
  ghm_summary <- reactive({
    req(data_by_ghm())
    n_by_ghm <- table_by(data_common(), "GHM")
    n_by_ghm$onco <- as.factor(
      ifelse(n_by_ghm$code %in% ghm_cancero$code, "oui", "non")
    )
    return(n_by_ghm)
  })
  
  diags_categorie_stats <- reactive({
    req(by_lists$diags_loaded$categorie, diags_table())
    diags_table <- diags_table()
    loaded_diags <- by_lists$diags_loaded[, c("code", "categorie")]
    data <- merge(
      diags_table, loaded_diags,
      by="code", all.x=TRUE
    )
    
    data[is.na(data$categorie), "categorie"] <- sapply(
      data[is.na(data$categorie), "code"],
      function(x) loaded_diags[match(
        substr(x, 1, 3), substr(loaded_diags$code, 1, 3)
      ), "categorie"]
    )
    
    stats <- (
      data %>% 
        group_by(categorie) %>% 
        summarise(
          n_patients=sum(n_patients), 
          n_sejours=sum(n_sejours), 
          total_durée=sum(tot_sej),
          moy_durée=round(sum(tot_sej) / sum(n_sejours), digits=2),
          min_durée=min(min_sej, na.rm=TRUE),
          max_durée=max(max_sej, na.rm=TRUE)
        )
    )
    return(stats)
  })
  
  acts_categorie_stats <- reactive({
    req(by_lists$acts_loaded$categorie, acts_table())
    acts_table <- acts_table()
    loaded_acts <- by_lists$acts_loaded[, c("code", "categorie")]
    data <- merge(
      acts_table, loaded_acts,
      by="code", all.x=TRUE
    )

    data[is.na(data$categorie), "categorie"] <- sapply(
      data[is.na(data$categorie), "code"],
      function(x) loaded_acts[match(
        substr(x, 1, 4), substr(loaded_acts$code, 1, 4)
      ), "categorie"]
    )

    stats <- (
      data %>% 
        group_by(categorie) %>% 
        summarise(
          n_patients=sum(n_patients), 
          n_sejours=sum(n_sejours), 
          total_durée=sum(tot_sej),
          moy_durée=round(sum(tot_sej) / sum(n_sejours), digits=2),
          min_durée=min(min_sej, na.rm=TRUE),
          max_durée=max(max_sej, na.rm=TRUE)
        )
    )
    return(stats)
  })
  
  ghm_categorie_stats <- reactive({
    req(by_lists$ghm_loaded$categorie, ghm_table())
    ghm_table <- ghm_table()
    loaded_ghm <- by_lists$ghm_loaded[, c("code", "categorie")]
    data <- merge(
      ghm_table, loaded_ghm,
      by="code", all.x=TRUE
    )
    
    data[is.na(data$categorie), "categorie"] <- sapply(
      data[is.na(data$categorie), "code"],
      function(x) loaded_ghm[match(
        substr(x, 1, 3), substr(loaded_ghm$code, 1, 3)
      ), "categorie"]
    )
    
    stats <- (
      data %>% 
        group_by(categorie) %>% 
        summarise(
          n_patients=sum(n_patients), 
          n_sejours=sum(n_sejours), 
          total_durée=sum(tot_sej),
          moy_durée=round(sum(tot_sej) / sum(n_sejours), digits=2),
          min_durée=min(min_sej, na.rm=TRUE),
          max_durée=max(max_sej, na.rm=TRUE)
        )
    )
    return(stats)
  })
  
  diags_categorie_table <- reactive({
    if (!is.null(by_lists$diags_loaded)) {
      table <- datatable(
        data=diags_categorie_stats(),
        style="bootstrap",
        rownames=FALSE,
        options=list(dom="tp")
      )
      return(table)
    } else {
      return("NA")
    }
  })
  
  acts_categorie_table <- reactive({
    if (!is.null(by_lists$acts_loaded)) {
      table <- datatable(
        data=acts_categorie_stats(),
        style="bootstrap",
        rownames=FALSE,
        options=list(dom="tp")
      )
      return(table)
    } else {
      return("NA")
    }
  })
  
  ghm_categorie_table <- reactive({
    if (!is.null(by_lists$ghm_loaded)) {
      table <- datatable(
        data=ghm_categorie_stats(),
        style="bootstrap",
        rownames=FALSE,
        options=list(dom="tp")
      )
      return(table)
    } else {
      return("NA")
    }
  })
  
  diags_summary_output <- reactive({
    if (!is.null(by_lists$diags_list)) {
      diags_table <- datatable(
        data=diags_summary(),
        style="bootstrap",
        rownames=FALSE,
        filter='top',
        extensions='Buttons',
        options=list(
          paging=TRUE,
          columnDefs=list(list(visible=FALSE, targets=c(-1:-3))),
          dom="Blfrtip",
          searching=TRUE,
          search=list(regex=TRUE),
          buttons=list(
            list(extend='colvis', text='Voir/cacher colonne'),
            list(extend='collection',
                 buttons=c('copy', 'excel', 'csv'),
                 text='Exporter tableau')
          )
        )
      )
      return(diags_table)
    } else {
      return("NA")
    }
  })
  
  acts_summary_output <- reactive({
    if (!is.null(by_lists$acts_list)) {
      acts_table <- datatable(
        data=acts_summary(),
        style="bootstrap",
        rownames=FALSE,
        filter='top',
        extensions='Buttons',
        options=list(
          paging=TRUE,
          columnDefs=list(list(visible=FALSE, targets=c(-1:-3))),
          dom="Blfrtip", 
          searching=TRUE,
          search=list(regex=TRUE),
          buttons=list(
            list(extend='colvis', text='Voir/cacher colonne'),
            list(extend='collection',
                 buttons=c('copy', 'excel', 'csv'),
                 text='Exporter tableau')
          )
        )
      )
      return(acts_table)
    } else {
      return("NA")
    }
  })
  
  ghm_summary_output <- reactive({
    if (!is.null(by_lists$ghm_list)) {
      ghm_table <- datatable(
        data=ghm_summary(),
        style="bootstrap",
        rownames=FALSE,
        filter='top',
        extensions='Buttons',
        options=list(
          paging=TRUE,
          columnDefs=list(list(visible=FALSE, targets=c(-1:-4))),
          dom="Blfrtip", 
          searching=TRUE,
          search=list(regex=TRUE),
          buttons=list(
            list(extend='colvis', text='Voir/cacher colonne'),
            list(extend='collection',
                 buttons=c('copy', 'excel', 'csv'),
                 text='Exporter tableau')
          )
        )
      )
      return(ghm_table)
    } else {
      return("NA")
    }
  })
  
  evol_table_global <- reactive({
    req(data_common(), input$evol_global_year_filter)
    evol_table_by(data_common(), "global")
  })
  
  evol_table_by_diags <- reactive({
    req(data_by_diags(), input$evol_diags_year_filter)
    evol_table_by(data_by_diags(), "diagnoses")
  })
  
  evol_table_by_acts <- reactive({
    req(data_by_acts(), input$evol_acts_year_filter)
    evol_table_by(data_by_acts(), "acts")
  })
  
  evol_table_by_ghm <- reactive({
    req(data_by_ghm(), input$evol_ghm_year_filter)
    evol_table_by(data_by_ghm(), "GHM")
  })
  
  global_stats <- reactive({
    req(data_common())
    global_stats_by(data_common())
  })
  
  global_stats_by_diags <- reactive({
    req(data_by_diags())
    global_stats_by(data_by_diags())
  })
  
  global_stats_by_acts <- reactive({
    req(data_by_acts())
    global_stats_by(data_by_acts())
  })
  
  global_stats_by_ghm <- reactive({
    req(data_by_ghm())
    global_stats_by(data_by_ghm())
  })
  
  age_histogram <- reactive({
    req(data_common())
    plot_age_by(data_common())
  })
  
  age_histogram_by_diags <- reactive({
    req(data_by_diags())
    plot_age_by(data_by_diags())
  })
  
  age_histogram_by_acts <- reactive({
    req(data_by_acts())
    plot_age_by(data_by_acts())
  })
  
  age_histogram_by_ghm <- reactive({
    req(data_by_ghm())
    plot_age_by(data_by_ghm())
  })
  
  age_table <- reactive({
    req(data_common())
    age_table_by(data_common())
  })
  
  age_table_by_diags <- reactive({
    req(data_by_diags())
    age_table_by(data_by_diags())
  })
  
  age_table_by_acts <- reactive({
    req(data_by_acts())
    age_table_by(data_by_acts())
  })
  
  age_table_by_ghm <- reactive({
    req(data_by_ghm())
    age_table_by(data_by_ghm())
  })
  
  GHM_lettre_table <- reactive({
    req(data_common())
    GHM_lettre_by(data_common())
  })
  
  GHM_lettre_table_by_diags <- reactive({
    req(data_by_diags())
    GHM_lettre_by(data_by_diags())
  })
  
  GHM_lettre_table_by_acts <- reactive({
    req(data_by_acts())
    GHM_lettre_by(data_by_acts())
  })
  
  GHM_lettre_table_by_ghm <- reactive({
    req(data_by_ghm())
    GHM_lettre_by(data_by_ghm())
  })
  
  URM_origine_table <- reactive({
    req(data_common(), etablissement())
    URM_by(data_common(), "origine")
  })
  
  URM_origine_table_by_diags <- reactive({
    req(data_by_diags(), etablissement())
    URM_by(data_by_diags(), "origine")
  })
  
  URM_origine_table_by_acts <- reactive({
    req(data_by_acts(), etablissement())
    URM_by(data_by_acts(), "origine")
  })
  
  URM_origine_table_by_ghm <- reactive({
    req(data_by_ghm(), etablissement())
    URM_by(data_by_ghm(), "origine")
  })
  
  URM_destination_table <- reactive({
    req(data_common(), etablissement())
    URM_by(data_common(), "destination")
  })
  
  URM_destination_table_by_diags <- reactive({
    req(data_by_diags(), etablissement())
    URM_by(data_by_diags(), "destination")
  })
  
  URM_destination_table_by_acts <- reactive({
    req(data_by_acts(), etablissement())
    URM_by(data_by_acts(), "destination")
  })
  
  URM_destination_table_by_ghm <- reactive({
    req(data_by_ghm(), etablissement())
    URM_by(data_by_ghm(), "destination")
  })
  
  geographic_global <- reactive({
    req(data_common())
    geographic_by(data_common())
  })
  
  geographic_by_diags <- reactive({
    req(data_by_diags())
    geographic_by(data_by_diags())
  })
  
  geographic_by_acts <- reactive({
    req(data_by_acts())
    geographic_by(data_by_acts())
  })
  
  geographic_by_ghm <- reactive({
    req(data_by_ghm())
    geographic_by(data_by_ghm())
  })
  
  mode_ent_table <- reactive({
    req(data_common())
    mode_ent_by(data_common())
  })
  
  mode_ent_table_by_diags <- reactive({
    req(data_by_diags())
    mode_ent_by(data_by_diags())
  })
  
  mode_ent_table_by_acts <- reactive({
    req(data_by_acts())
    mode_ent_by(data_by_acts())
  })
  
  mode_ent_table_by_ghm <- reactive({
    req(data_by_ghm())
    mode_ent_by(data_by_ghm())
  })
  
  mode_sor_table <- reactive({
    req(data_common())
    mode_sor_by(data_common())
  })
  
  mode_sor_table_by_diags <- reactive({
    req(data_by_diags())
    mode_sor_by(data_by_diags())
  })
  
  mode_sor_table_by_acts <- reactive({
    req(data_by_acts())
    mode_sor_by(data_by_acts())
  })
  
  mode_sor_table_by_ghm <- reactive({
    req(data_by_ghm())
    mode_sor_by(data_by_ghm())
  })
  
  severite_table <- reactive({
    req(data_common())
    severite_by(data_common())
  })
  
  severite_table_by_diags <- reactive({
    req(data_by_diags())
    severite_by(data_by_diags())
  })
  
  severite_table_by_acts <- reactive({
    req(data_by_acts())
    severite_by(data_by_acts())
  })
  
  severite_table_by_ghm <- reactive({
    req(data_by_ghm())
    severite_by(data_by_ghm())
  })
  
  ##################################
  ### RENDERING TABLES AND PLOTS ###
  ##################################
  
  output$diags_table <- renderDT({
    req(diags_table())
    datatable(
      diags_table(),
      width="auto",
      rownames=FALSE,
      filter='top',
      extensions='Buttons',
      options=list(
        paging=TRUE,
        columnDefs=list(list(visible=FALSE, targets=c(-1:-3))),
        dom="Blfrtip", 
        searching=TRUE,
        search=list(regex=TRUE),
        buttons=list(
          list(extend='colvis', text='Voir/cacher colonne'),
          list(extend='collection',
               buttons=c('copy', 'excel', 'csv'),
               text='Exporter tableau')
        )
      )
    )
  })
  
  output$acts_table <- renderDT({
    req(acts_table())
    datatable(
      acts_table(),
      width="auto",
      rownames=FALSE,
      filter='top',
      extensions='Buttons',
      options=list(
        paging=TRUE,
        columnDefs=list(list(visible=FALSE, targets=c(-1:-3))),
        dom="Blfrtip", 
        searching=TRUE,
        search=list(regex=TRUE),
        buttons=list(
          list(extend='colvis', text='Voir/cacher colonne'),
          list(extend='collection',
               buttons=c('copy', 'excel', 'csv'),
               text='Exporter tableau')
        )
      )
    )
  })
  
  output$ghm_table <- renderDT({
    req(ghm_table())
    datatable(
      ghm_table(),
      width="auto",
      rownames=FALSE,
      filter='top',
      extensions='Buttons',
      options=list(
        paging=TRUE,
        columnDefs=list(list(visible=FALSE, targets=c(-1:-4))),
        dom="Blfrtip", 
        searching=TRUE,
        search=list(regex=TRUE),
        buttons=list(
          list(extend='colvis', text='Voir/cacher colonne'),
          list(extend='collection',
               buttons=c('copy', 'excel', 'csv'),
               text='Exporter tableau')
        )
      )
    )
  })
  
  output$diags_summary <- renderDT({
    req(diags_summary())
    datatable(
      diags_summary(),
      width="auto",
      rownames=FALSE,
      filter='top',
      extensions='Buttons',
      options=list(
        paging=TRUE,
        columnDefs=list(list(visible=FALSE, targets=c(-1:-3))),
        dom="Blfrtip", 
        searching=TRUE,
        search=list(regex=TRUE),
        buttons=list(
          list(extend='colvis', text='Voir/cacher colonne'),
          list(extend='collection',
               buttons=c('copy', 'excel', 'csv'),
               text='Exporter tableau')
        )
      )
    )
  })
  
  output$acts_summary <- renderDT({
    req(acts_summary())
    datatable(
      acts_summary(),
      width="auto",
      rownames=FALSE,
      filter='top',
      extensions='Buttons',
      options=list(
        paging=TRUE,
        columnDefs=list(list(visible=FALSE, targets=c(-1:-3))),
        dom="Blfrtip", 
        searching=TRUE,
        search=list(regex=TRUE),
        buttons=list(
          list(extend='colvis', text='Voir/cacher colonne'),
          list(extend='collection',
               buttons=c('copy', 'excel', 'csv'),
               text='Exporter tableau')
        )
      )
    )
  })
  
  output$ghm_summary <- renderDT({
    req(ghm_summary())
    datatable(
      ghm_summary(),
      width="auto",
      rownames=FALSE,
      filter='top',
      extensions='Buttons',
      options=list(
        paging=TRUE,
        columnDefs=list(list(visible=FALSE, targets=c(-1:-4))),
        dom="Blfrtip", 
        searching=TRUE,
        search=list(regex=TRUE),
        buttons=list(
          list(extend='colvis', text='Voir/cacher colonne'),
          list(extend='collection',
               buttons=c('copy', 'excel', 'csv'),
               text='Exporter tableau')
        )
      )
    )
  })
  
  output$diags_categorie_stats <- renderDT({
    req(diags_categorie_stats())
    datatable(
      diags_categorie_stats(),
      rownames=FALSE
    )
  })
  
  output$acts_categorie_stats <- renderDT({
    req(acts_categorie_stats())
    datatable(
      acts_categorie_stats(),
      rownames=FALSE
    )
  })
  
  output$ghm_categorie_stats <- renderDT({
    req(ghm_categorie_stats())
    datatable(
      ghm_categorie_stats(),
      rownames=FALSE
    )
  })
  
  output$evol_table_global <- renderDT({
    req(evol_table_global())
    datatable(
      evol_table_global(),
      rownames=TRUE,
      extensions='Buttons',
      options=list(
        paging=FALSE,
        dom="Blfrtip", 
        buttons=list(
          list(extend='collection',
               buttons=c('copy', 'excel', 'csv'),
               text='Exporter tableau')
        )
      )
    )
  })
  
  output$evol_table_by_diags <- renderDT({
    req(evol_table_by_diags())
    datatable(
      evol_table_by_diags(),
      rownames=TRUE,
      extensions='Buttons',
      options=list(
        paging=FALSE,
        dom="Blfrtip", 
        buttons=list(
          list(extend='collection',
               buttons=c('copy', 'excel', 'csv'),
               text='Exporter tableau')
        )
      )
    )
  })
  
  output$evol_table_by_acts <- renderDT({
    req(evol_table_by_acts())
    datatable(
      evol_table_by_acts(),
      rownames=TRUE,
      extensions='Buttons',
      options=list(
        paging=FALSE,
        dom="Blfrtip", 
        buttons=list(
          list(extend='collection',
               buttons=c('copy', 'excel', 'csv'),
               text='Exporter tableau')
        )
      )
    )
  })
  
  output$evol_table_by_ghm <- renderDT({
    req(evol_table_by_ghm())
    datatable(
      evol_table_by_ghm(),
      rownames=TRUE,
      extensions='Buttons',
      options=list(
        paging=FALSE,
        dom="Blfrtip", 
        buttons=list(
          list(extend='collection',
               buttons=c('copy', 'excel', 'csv'),
               text='Exporter tableau')
        )
      )
    )
  })
  
  output$global_n_sejours <- renderValueBox({
    req(global_stats())
    value_box_by(global_stats(), "n_sejours")
  })
  
  output$global_n_patients <- renderValueBox({
    req(global_stats())
    value_box_by(global_stats(), "n_patients")
  })
  
  output$global_total_sejour <- renderValueBox({
    req(global_stats())
    value_box_by(global_stats(), "total_sejour")
  })
  
  output$global_moyenne_sejour <- renderValueBox({
    req(global_stats())
    value_box_by(global_stats(), "moyenne_sejour")
  })
  
  output$n_sejours_by_diags <- renderValueBox({
    req(global_stats_by_diags())
    value_box_by(global_stats_by_diags(), "n_sejours")
  })
  
  output$n_patients_by_diags <- renderValueBox({
    req(global_stats_by_diags())
    value_box_by(global_stats_by_diags(), "n_patients")
  })
  
  output$total_sejour_by_diags <- renderValueBox({
    req(global_stats_by_diags())
    value_box_by(global_stats_by_diags(), "total_sejour")
  })
  
  output$moyenne_sejour_by_diags <- renderValueBox({
    req(global_stats_by_diags())
    value_box_by(global_stats_by_diags(), "moyenne_sejour")
  })
  
  output$n_sejours_by_act <- renderValueBox({
    req(global_stats_by_acts())
    value_box_by(global_stats_by_acts(), "n_sejours")
  })
  
  output$n_patients_by_act <- renderValueBox({
    req(global_stats_by_acts())
    value_box_by(global_stats_by_acts(), "n_patients")
  })
  
  output$total_sejour_by_act <- renderValueBox({
    req(global_stats_by_acts())
    value_box_by(global_stats_by_acts(), "total_sejour")
  })
  
  output$moyenne_sejour_by_act <- renderValueBox({
    req(global_stats_by_acts())
    value_box_by(global_stats_by_acts(), "moyenne_sejour")
  })
  
  output$n_sejours_by_ghm <- renderValueBox({
    req(global_stats_by_ghm())
    value_box_by(global_stats_by_ghm(), "n_sejours")
  })
  
  output$n_patients_by_ghm <- renderValueBox({
    req(global_stats_by_ghm())
    value_box_by(global_stats_by_ghm(), "n_patients")
  })
  
  output$total_sejour_by_ghm <- renderValueBox({
    req(global_stats_by_ghm())
    value_box_by(global_stats_by_ghm(), "total_sejour")
  })
  
  output$moyenne_sejour_by_ghm <- renderValueBox({
    req(global_stats_by_ghm())
    value_box_by(global_stats_by_ghm(), "moyenne_sejour")
  })
  
  output$geographic_global <- renderDT({
    req(geographic_global())
    datatable(
      geographic_global(),
      autoHideNavigation=TRUE,
      width="auto",
      rownames=TRUE
    )
  })
  
  output$geographic_by_diags <- renderDT({
    req(geographic_by_diags())
    datatable(
      geographic_by_diags(),
      autoHideNavigation=TRUE,
      width="auto",
      rownames=TRUE
    )
  })
  
  output$geographic_by_acts <- renderDT({
    req(geographic_by_acts())
    datatable(
      geographic_by_acts(),
      autoHideNavigation=TRUE,
      width="auto",
      rownames=TRUE
    )
  })
  
  output$geographic_by_ghm <- renderDT({
    req(geographic_by_ghm())
    datatable(
      geographic_by_ghm(),
      autoHideNavigation=TRUE,
      width="auto",
      rownames=TRUE
    )
  })
  
  output$age_histogram <- renderPlot({
    req(age_histogram())
    age_histogram()
  })
  
  output$age_histogram_by_diags <- renderPlot({
    req(age_histogram_by_diags())
    age_histogram_by_diags()
  })
  
  output$age_histogram_by_acts <- renderPlot({
    req(age_histogram_by_acts())
    age_histogram_by_acts()
  })
  
  output$age_histogram_by_ghm <- renderPlot({
    req(age_histogram_by_ghm())
    age_histogram_by_ghm()
  })
  
  output$age_table <- renderDT({
    req(age_table())
    datatable(
      age_table(),
      rownames=FALSE
    )
  })
  
  output$age_table_by_diags <- renderDT({
    req(age_table_by_diags())
    datatable(
      age_table_by_diags(),
      rownames=FALSE
    )
  })
  
  output$age_table_by_acts <- renderDT({
    req(age_table_by_acts())
    datatable(
      age_table_by_acts(),
      rownames=FALSE
    )
  })
  
  output$age_table_by_ghm <- renderDT({
    req(age_table_by_ghm())
    datatable(
      age_table_by_ghm(),
      rownames=FALSE
    )
  })
  
  output$GHM_lettre_table <- renderDT({
    req(GHM_lettre_table())
    datatable(
      GHM_lettre_table(),
      rownames=TRUE
    )
  })
  
  output$GHM_lettre_table_by_diags <- renderDT({
    req(GHM_lettre_table_by_diags())
    datatable(
      GHM_lettre_table_by_diags(),
      rownames=TRUE
    )
  })
  
  output$GHM_lettre_table_by_acts <- renderDT({
    req(GHM_lettre_table_by_acts())
    datatable(
      GHM_lettre_table_by_acts(),
      rownames=TRUE
    )
  })
  
  output$GHM_lettre_table_by_ghm <- renderDT({
    req(GHM_lettre_table_by_ghm())
    datatable(
      GHM_lettre_table_by_ghm(),
      rownames=TRUE
    )
  })
  
  output$URM_origine_table <- renderDT({
    req(URM_origine_table())
    datatable(
      URM_origine_table(),
      rownames=TRUE,
      options=list (
        pageLength=5
      )
    )
  })
  
  output$URM_origine_table_by_diags <- renderDT({
    req(URM_origine_table_by_diags())
    datatable(
      URM_origine_table_by_diags(),
      rownames=TRUE,
      options=list (
        pageLength=5
      )
    )
  })
  
  output$URM_origine_table_by_acts <- renderDT({
    req(URM_origine_table_by_acts())
    datatable(
      URM_origine_table_by_acts(),
      rownames=TRUE,
      options=list (
        pageLength=5
      )
    )
  })
  
  output$URM_origine_table_by_ghm <- renderDT({
    req(URM_origine_table_by_ghm())
    datatable(
      URM_origine_table_by_ghm(),
      rownames=TRUE,
      options=list (
        pageLength=5
      )
    )
  })
  
  output$URM_destination_table <- renderDT({
    req(URM_destination_table())
    datatable(
      URM_destination_table(),
      rownames=TRUE,
      options=list (
        pageLength=5
      )
    )
  })
  
  output$URM_destination_table_by_diags <- renderDT({
    req(URM_destination_table_by_diags())
    datatable(
      URM_destination_table_by_diags(),
      rownames=TRUE,
      options=list (
        pageLength=5
      )
    )
  })
  
  output$URM_destination_table_by_acts <- renderDT({
    req(URM_destination_table_by_acts())
    datatable(
      URM_destination_table_by_acts(),
      rownames=TRUE,
      options=list (
        pageLength=5
      )
    )
  })
  
  output$URM_destination_table_by_ghm <- renderDT({
    req(URM_destination_table_by_ghm())
    datatable(
      URM_destination_table_by_ghm(),
      rownames=TRUE,
      options=list (
        pageLength=5
      )
    )
  })
  
  output$mode_ent_table <- renderDT({
    req(mode_ent_table())
    datatable(
      mode_ent_table(),
      rownames=FALSE
    )
  })
  
  output$mode_ent_table_by_diags <- renderDT({
    req(mode_ent_table_by_diags())
    datatable(
      mode_ent_table_by_diags(),
      rownames=FALSE
    )
  })
  
  output$mode_ent_table_by_acts <- renderDT({
    req(mode_ent_table_by_acts())
    datatable(
      mode_ent_table_by_acts(),
      rownames=FALSE
    )
  })
  
  output$mode_ent_table_by_ghm <- renderDT({
    req(mode_ent_table_by_ghm())
    datatable(
      mode_ent_table_by_ghm(),
      rownames=FALSE
    )
  })
  
  output$mode_sor_table <- renderDT({
    req(mode_sor_table())
    datatable(
      mode_sor_table(),
      rownames=FALSE
    )
  })
  
  output$mode_sor_table_by_diags <- renderDT({
    req(mode_sor_table_by_diags())
    datatable(
      mode_sor_table_by_diags(),
      rownames=FALSE
    )
  })
  
  output$mode_sor_table_by_acts <- renderDT({
    req(mode_sor_table_by_acts())
    datatable(
      mode_sor_table_by_acts(),
      rownames=FALSE
    )
  })
  
  output$mode_sor_table_by_ghm <- renderDT({
    req(mode_sor_table_by_ghm())
    datatable(
      mode_sor_table_by_ghm(),
      rownames=FALSE
    )
  })
  
  output$severite_table <- renderDT({
    req(severite_table())
    datatable(
      severite_table(),
      rownames=FALSE,
      options=list(pageLength=4)
    )
  })
  
  output$severite_table_by_diags <- renderDT({
    req(severite_table_by_diags())
    datatable(
      severite_table_by_diags(),
      rownames=FALSE,
      options=list(pageLength=4)
    )
  })
  
  output$severite_table_by_acts <- renderDT({
    req(severite_table_by_acts())
    datatable(
      severite_table_by_acts(),
      rownames=FALSE,
      options=list(pageLength=4)
    )
  })
  
  output$severite_table_by_ghm <- renderDT({
    req(severite_table_by_ghm())
    datatable(
      severite_table_by_ghm(),
      rownames=FALSE,
      options=list(pageLength=4)
    )
  })
  
  global_report_title <- reactive({
    if (input$global_report_title == "") {
      title <- "Rapport d'activité global thot"
    } else {
      title <- input$global_report_title
    }
    return(title)
  })
  
  diags_report_title <- reactive({
    if (input$diags_report_title == "") {
      title <- "Rapport d'activité par diagnostics thot"
    } else {
      title <- input$diags_report_title
    }
    return(title)
  })
  
  acts_report_title <- reactive({
    if (input$acts_report_title == "") {
      title <- "Rapport d'activité par actes thot"
    } else {
      title <- input$acts_report_title
    }
    return(title)
  })
  
  ghm_report_title <- reactive({
    if (input$ghm_report_title == "") {
      title <- "Rapport d'activité par GHM thot"
    } else {
      title <- input$ghm_report_title
    }
    return(title)
  })
  
  ##########################
  ### GENERATING REPORTS ###
  ##########################
  #### GLOBAL ####
  
  observeEvent(data(), {
    output$download_report <- renderUI({
      fluidRow(
        box(
          HTML(
            "<div style='font-weight:normal;'>
              Deux fichiers sont disponibles : 
              <ul>
                <li>un rapport interactif au format HTML</li>
                <li>la liste des patients filtrés au format CSV</li>
              </ul>
            </div>"
          ),
          textInput(
            inputId="global_report_title",
            label=NULL,
            placeholder="Taper un titre pour le rapport (optionnel)"
          ),
          downloadButton("report", "Rapport global"),
          downloadButton("export_patients_global", "Liste patients"), 
          width=12,
          title="Exportation", collapsible=T
        )
      )
    })
  })
    
  observeEvent(data(), {
    output$report <- downloadHandler(
      filename=function() {
        paste(
          paste(unique(data()$URMP), collapse="-"), 
          "_global_", 
          Sys.Date(), 
          ".html", 
          sep=""
        )
      },
      content=function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite=TRUE)
        params <- list(
          title=global_report_title(),
          URM=unique(data()$URMP),
          age_filter=input$age_filter,
          UH_list=input$UH_filter,
          diags_types=diags_types$all,
          cmd_list=input$cmd_filter,
          GHM_lettre_list=input$GHM_lettre_filter,
          diags_given=diags_given(),
          acts_given=acts_given(),
          ghm_given=ghm_given(),
          dad_filter=input$dad_filter,
          mr_list=mr_list(),
          date_range=input$date_range,
          global_stats=global_stats(),
          geographic_global=datatable(
            data=geographic_global(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          age_table=datatable(
            data=age_table(),
            style="bootstrap",
            rownames=FALSE,
            options=list(dom="tp")
          ),
          diags_table=diags_summary_output(),
          acts_table=acts_summary_output(),
          ghm_table=ghm_summary_output(),
          URM_origine_table=datatable(
            data=URM_origine_table(),
            style="bootstrap",
            options=list(
              dom="tp",
              paging=FALSE,
              scrollY="400px",
              scrollCollapse=TRUE
            )
          ),
          URM_destination_table=datatable(
            data=URM_destination_table(),
            style="bootstrap",
            options=list(
              dom="tp",
              paging=FALSE,
              scrollY="400px",
              scrollCollapse=TRUE
            )
          ),
          GHM_lettre_table=datatable(
            data=GHM_lettre_table(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          severite_table=datatable(
            data=severite_table(),
            style="bootstrap",
            rownames=FALSE,
            options=list(
              dom="tp",
              pageLength=4
            )
          ),
          mode_ent_table=datatable(
            data=mode_ent_table(),
            style="bootstrap",
            rownames=FALSE,
            options=list(dom="tp")
          ),
          mode_sor_table=datatable(
            data=mode_sor_table(),
            style="bootstrap",
            rownames=FALSE,
            options=list(dom="tp")
          ),
          age_histogram=age_histogram()
        )
        rmarkdown::render(
          tempReport, output_file=file,
          params=params,
          clean=TRUE,
          envir=new.env(parent=globalenv()),
          encoding="UTF-8"
        )
      }
    )
    
    output$export_patients_global <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv2(
          unique(data_common()[, c("NIP", "Nom", "Prenom", "Date.naiss")]), 
          con, 
          row.names=FALSE
        )
      }
    )
  })
  
  #### diags ####
  
  observeEvent(diags_table(), {
    output$download_report_diags <- renderUI({
      if (is.null(diags_table())) {
        hide("download_report_diags")
      } else {
        show("download_report_diags")
      }
      box(
        HTML(
          "<div style='font-weight:normal;'>
              Deux fichiers sont disponibles : 
              <ul>
                <li>un rapport interactif au format HTML</li>
                <li>la liste des patients filtrés au format CSV</li>
              </ul>
            </div>"
        ),
        textInput(
          inputId="diags_report_title", 
          label=NULL,
          placeholder="Taper un titre pour le rapport (optionnel)"
        ),
        downloadButton("report_diags", "Rapport par diagnostics"),
        downloadButton("export_patients_diags", "Liste patients"), 
        width=4,
        title="Exportation", collapsible=T
      )
    })
  })
  
  observeEvent(diags_table(), {
    output$report_diags <- downloadHandler(
      filename=function() {
        paste(
          paste(unique(data_by_diags()$URMP), collapse="-"), 
          "_diags_", 
          Sys.Date(), 
          ".html", 
          sep=""
        )
      },
      content=function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite=TRUE)
        params <- list(
          title=diags_report_title(),
          URM=unique(data_by_diags()$URMP),
          age_filter=input$age_filter,
          UH_list=input$UH_filter,
          diags_types=diags_types$all,
          cmd_list=input$cmd_filter,
          GHM_lettre_list=input$GHM_lettre_filter,
          dad_filter=input$dad_filter,
          diags_given=diags_given(),
          date_range=input$date_range,
          global_stats=global_stats_by_diags(),
          geographic_global=datatable(
            data=geographic_by_diags(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          age_table=datatable(
            data=age_table_by_diags(),
            style="bootstrap",
            rownames=FALSE,
            options=list(dom="tp")
          ),
          diags_table=datatable(
            data=diags_table(),
            style="bootstrap",
            rownames=FALSE,
            filter='top',
            extensions='Buttons',
            options=list(
              paging=TRUE,
              columnDefs=list(list(visible=FALSE, targets=c(-1:-3))),
              dom="Blfrtip",
              searching=TRUE,
              search=list(regex=TRUE),
              buttons=list(
                list(extend='colvis', text='Voir/cacher colonne'),
                list(extend='collection',
                     buttons=c('copy', 'excel', 'csv'),
                     text='Exporter tableau')
              )
            )
          ),
          diags_categorie_table=diags_categorie_table(),
          URM_origine_table=datatable(
            data=URM_origine_table_by_diags(),
            style="bootstrap",
            options=list(
              dom="tp",
              paging=FALSE,
              scrollY="400px",
              scrollCollapse=TRUE
            )
          ),
          URM_destination_table=datatable(
            data=URM_destination_table_by_diags(),
            style="bootstrap",
            options=list(
              dom="tp",
              paging=FALSE,
              scrollY="400px",
              scrollCollapse=TRUE
            )
          ),
          GHM_lettre_table=datatable(
            data=GHM_lettre_table_by_diags(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          severite_table=datatable(
            data=severite_table_by_diags(),
            style="bootstrap",
            rownames=FALSE,
            options=list(
              dom="tp",
              pageLength=4
            )
          ),
          mode_ent_table=datatable(
            data=mode_ent_table_by_diags(),
            style="bootstrap",
            rownames=FALSE,
            options=list(dom="tp")
          ),
          mode_sor_table=datatable(
            data=mode_sor_table_by_diags(),
            style="bootstrap",
            rownames=FALSE,
            options=list(dom="tp")
          ),
          age_histogram=age_histogram_by_diags()
        )
        rmarkdown::render(
          tempReport, output_file=file,
          params=params,
          envir=new.env(parent=globalenv()),
          encoding="UTF-8"
        )
      }
    )
    
    output$export_patients_diags <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv2(
          unique(data_by_diags()[, c("NIP", "Nom", "Prenom", "Date.naiss")]), 
          con, 
          row.names=FALSE
        )
      }
    )
  })
  
  #### ACTS ####
  
  observeEvent(acts_table(), {
    output$download_report_acts <- renderUI({
      if (is.null(acts_table())) {
        hide("download_report_acts")
      } else {
        show("download_report_acts")
      }
      box(
        HTML(
          "<div style='font-weight:normal;'>
              Deux fichiers sont disponibles : 
              <ul>
                <li>un rapport interactif au format HTML</li>
                <li>la liste des patients filtrés au format CSV</li>
              </ul>
            </div>"
        ),
        textInput(
          inputId="acts_report_title", 
          label=NULL,
          placeholder="Taper un titre pour le rapport (optionnel)"
        ),
        downloadButton("report_acts", "Rapport par actes"),
        downloadButton("export_patients_acts", "Liste patients"), 
        width=4,
        title="Exportation", collapsible=T
      )
    })
  })
  
  observeEvent(acts_table(), {
    output$report_acts <- downloadHandler(
      filename=function() {
        paste(
          paste(unique(data_by_acts()$URMP), collapse="-"),
          "_acts_", 
          Sys.Date(), 
          ".html", 
          sep=""
        )
      },
      content=function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite=TRUE)
        params <- list(
          title=acts_report_title(),
          URM=unique(data_by_acts()$URMP),
          age_filter=input$age_filter,
          UH_list=input$UH_filter,
          cmd_list=input$cmd_filter,
          GHM_lettre_list=input$GHM_lettre_filter,
          dad_filter=input$dad_filter,
          acts_given=acts_given(),
          date_range=input$date_range,
          global_stats=global_stats_by_acts(),
          geographic_global=datatable(
            data=geographic_by_acts(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          age_table=datatable(
            data=age_table_by_acts(),
            style="bootstrap",
            rownames=FALSE,
            options=list(dom="tp")
          ),
          acts_table=datatable(
            data=acts_table(),
            style="bootstrap",
            rownames=FALSE,
            filter='top',
            extensions='Buttons',
            options=list(
              paging=TRUE,
              columnDefs=list(list(visible=FALSE, targets=c(-1:-3))),
              dom="Blfrtip", 
              searching=TRUE,
              search=list(regex=TRUE),
              buttons=list(
                list(extend='colvis', text='Voir/cacher colonne'),
                list(extend='collection',
                     buttons=c('copy', 'excel', 'csv'),
                     text='Exporter tableau')
              )
            )
          ),
          acts_categorie_table=acts_categorie_table(),
          URM_origine_table=datatable(
            data=URM_origine_table_by_acts(),
            style="bootstrap",
            options=list(
              dom="tp",
              paging=FALSE,
              scrollY="400px",
              scrollCollapse=TRUE
            )
          ),
          URM_destination_table=datatable(
            data=URM_destination_table_by_acts(),
            style="bootstrap",
            options=list(
              dom="tp",
              paging=FALSE,
              scrollY="400px",
              scrollCollapse=TRUE
            )
          ),
          GHM_lettre_table=datatable(
            data=GHM_lettre_table_by_acts(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          severite_table=datatable(
            data=severite_table_by_acts(),
            style="bootstrap",
            rownames=FALSE,
            options=list(
              dom="tp",
              pageLength=4
            )
          ),
          mode_ent_table=datatable(
            data=mode_ent_table_by_acts(),
            style="bootstrap",
            rownames=FALSE,
            options=list(dom="tp")
          ),
          mode_sor_table=datatable(
            data=mode_sor_table_by_acts(),
            style="bootstrap",
            rownames=FALSE,
            options=list(dom="tp")
          ),
          age_histogram=age_histogram_by_acts()
        )
        rmarkdown::render(
          tempReport, output_file=file,
          params=params,
          envir=new.env(parent=globalenv()),
          encoding="UTF-8"
        )
      }
    )
    
    output$export_patients_acts <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv2(
          unique(data_by_acts()[, c("NIP", "Nom", "Prenom", "Date.naiss")]), 
          con, 
          row.names=FALSE
        )
      }
    )
  })
  
  
  #### GHM ####
  
  observeEvent(ghm_table(), {
    output$download_report_ghm <- renderUI({
      if (is.null(ghm_table())) {
        hide("download_report_ghm")
      } else {
        show("download_report_ghm")
      }
      box(
        textInput(
          inputId="ghm_report_title", 
          label=NULL,
          placeholder="Taper un titre pour le rapport"
        ),
        downloadButton("report_ghm", "Rapport par GHM"),
        downloadButton("export_patients_ghm", "Liste patients"), 
        width=4,
        title="Exportation", collapsible=T
      )
    })
  })
  
  observeEvent(ghm_table(), {
    output$report_ghm <- downloadHandler(
      filename=function() {
        paste(
          paste(unique(data_by_ghm()$URMP), collapse="-"),
          "_ghm_", 
          Sys.Date(), 
          ".html", 
          sep=""
        )
      },
      content=function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite=TRUE)
        params <- list(
          title=ghm_report_title(),
          URM=unique(data_by_ghm()$URMP),
          age_filter=input$age_filter,
          UH_list=input$UH_filter,
          cmd_list=input$cmd_filter,
          GHM_lettre_list=input$GHM_lettre_filter,
          dad_filter=input$dad_filter,
          ghm_given=ghm_given(),
          date_range=input$date_range,
          global_stats=global_stats_by_ghm(),
          geographic_global=datatable(
            data=geographic_by_ghm(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          age_table=datatable(
            data=age_table_by_ghm(),
            style="bootstrap",
            rownames=FALSE,
            options=list(dom="tp")
          ),
          ghm_table=datatable(
            data=ghm_table(),
            style="bootstrap",
            rownames=FALSE,
            filter='top',
            extensions='Buttons',
            options=list(
              paging=TRUE,
              columnDefs=list(list(visible=FALSE, targets=c(-1:-4))),
              dom="Blfrtip", 
              searching=TRUE,
              search=list(regex=TRUE),
              buttons=list(
                list(extend='colvis', text='Voir/cacher colonne'),
                list(extend='collection',
                     buttons=c('copy', 'excel', 'csv'),
                     text='Exporter tableau')
              )
            )
          ),
          ghm_categorie_table=ghm_categorie_table(),
          URM_origine_table=datatable(
            data=URM_origine_table_by_ghm(),
            style="bootstrap",
            options=list(
              dom="tp",
              paging=FALSE,
              scrollY="400px",
              scrollCollapse=TRUE
            )
          ),
          URM_destination_table=datatable(
            data=URM_destination_table_by_ghm(),
            style="bootstrap",
            options=list(
              dom="tp",
              paging=FALSE,
              scrollY="400px",
              scrollCollapse=TRUE
            )
          ),
          GHM_lettre_table=datatable(
            data=GHM_lettre_table_by_ghm(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          severite_table=datatable(
            data=severite_table_by_ghm(),
            style="bootstrap",
            rownames=FALSE,
            options=list(
              dom="tp",
              pageLength=4
            )
          ),
          mode_ent_table=datatable(
            data=mode_ent_table_by_ghm(),
            style="bootstrap",
            rownames=FALSE,
            options=list(dom="tp")
          ),
          mode_sor_table=datatable(
            data=mode_sor_table_by_ghm(),
            style="bootstrap",
            rownames=FALSE,
            options=list(dom="tp")
          ),
          age_histogram=age_histogram_by_ghm()
        )
        rmarkdown::render(
          tempReport, output_file=file,
          params=params,
          envir=new.env(parent=globalenv()),
          encoding="UTF-8"
        )
      }
    )
    
    output$export_patients_ghm <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv2(
          unique(data_by_ghm()[, c("NIP", "Nom", "Prenom", "Date.naiss")]), 
          con, 
          row.names=FALSE
        )
      }
    )
  })
}

shinyApp(ui, server)