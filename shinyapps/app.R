## app.R ##

library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(lubridate)
library(nomensland)
library(DT)

########
## UI ##
########

{
  #### UI HEADER ####
  DBheader <- dashboardHeader(
    title = "thot"
  )
  
  #### UI SIDEBAR ####
  DBsidebar <- dashboardSidebar(
    sidebarMenu(
      tags$style(
        paste(
          "#report {color: #444; margin-bottom:15px;}",
          "#report_diags {color: #444; margin-bottom:15px;}",
          "#report_acts {color: #444; margin-bottom:15px;}",
          "#report_ghm {color: #444; margin-bottom:15px;}",
          sep=" "
        )
      ),
      
      menuItem("Global", tabName = "global", icon = icon("globe")),
      menuItem("Diagnostiques", tabName = "diagnostics", icon = icon("search")),
      menuItem("Actes", tabName = "acts", icon = icon("search")),
      menuItem("GHM", tabName = "ghm", icon = icon("search"))
    ),
    
    fileInput(
      inputId = "file", 
      label = h4("Import des données"),
      multiple = FALSE,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      ),
      buttonLabel = "Parcourir",
      placeholder = ".csv SIMPA"
    ),
    
    uiOutput(outputId = "dynamic_date_range"),
    uiOutput(outputId = "dynamic_UH"),
    uiOutput(outputId = "dynamic_cmd"),
    uiOutput(outputId = "dynamic_GHM_lettre"),
    uiOutput(outputId = "dynamic_mode_ent"),
    uiOutput(outputId = "dynamic_dad"),
    uiOutput(outputId = "download_report"),
    uiOutput(outputId = "download_report_diags"),
    uiOutput(outputId = "download_report_acts"),
    uiOutput(outputId = "download_report_ghm")
  )
  
  #### UI BODY ####
  DBbody <- dashboardBody(
    useShinyjs(),
    
    tabItems(
      
      #### GLOBAL TAB ####
      tabItem(tabName = "global",
        uiOutput(outputId = "help"),
        fluidRow(
          valueBoxOutput("global_n_sejours", width = 3),
          valueBoxOutput("global_n_patients", width = 3),
          valueBoxOutput("global_total_sejour", width = 3),
          valueBoxOutput("global_moyenne_sejour", width = 3)
        ), 
        uiOutput(outputId = "dynamic_geographic_global"),
        uiOutput(outputId = 'dynamic_age_histogram'),
        uiOutput(outputId = 'dynamic_prov_histogram')
      ), 
      
      #### DIAG TAB ####
      tabItem(tabName = "diagnostics",
        fluidRow(
          box(
            selectizeInput(
              inputId = 'chosen_diagnoses',
              label = h4('Filtrer par diagnostiques CIM-10'),
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Tapez un ou plusieurs code(s)/libellé(s)"
              )
            ), 
            actionButton("condition_button", "Valider"), 
            actionButton("condition_reset", "Effacer"), 
            actionButton("condition_all", "Tout sélectionner"),
            width = 6
          ),
          box(
            fileInput(
              inputId = "diag_file", 
              label = h4("Liste de codes ?"),
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              ),
              buttonLabel = "Parcourir",
              placeholder = ".csv codes/libellés"
            ), 
            width = 6
          )
        ),
        fluidRow(
          valueBoxOutput("n_sejours_by_condition", width = 3),
          valueBoxOutput("n_patients_by_condition", width = 3),
          valueBoxOutput("total_sejour_by_condition", width = 3),
          valueBoxOutput("moyenne_sejour_by_condition", width = 3)
        ), 
        uiOutput(outputId = "dynamic_condition_tables"),
        uiOutput(outputId = "dynamic_geographic_by_condition"),
        uiOutput(outputId = 'dynamic_age_histogram_by_condition'),
        uiOutput(outputId = 'dynamic_prov_histogram_by_condition')
      ),
      
      #### ACTS TAB ####
      tabItem(tabName = "acts",
        fluidRow(
          box(
            selectizeInput(
              inputId = 'chosen_acts',
              label = h4('Filtrer par actes CCAM'),
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Tapez un ou plusieurs code(s)/libellé(s)"
              )
            ), 
            actionButton("acts_button", "Valider"), 
            actionButton("acts_reset", "Effacer"), 
            actionButton("acts_all", "Tout sélectionner"),
            width = 6
          ),
          box(
            fileInput(
              inputId = "acts_file", 
              label = h4("Liste de codes ?"),
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              ),
              buttonLabel = "Parcourir",
              placeholder = ".csv codes/libellés"
            ),
            width = 6
          )
        ),
        fluidRow(
          valueBoxOutput("n_sejours_by_act", width = 3),
          valueBoxOutput("n_patients_by_act", width = 3),
          valueBoxOutput("total_sejour_by_act", width = 3),
          valueBoxOutput("moyenne_sejour_by_act", width = 3)
        ),
        uiOutput(outputId = "dynamic_acts_tables"),
        uiOutput(outputId = "dynamic_categorie"),
        uiOutput(outputId = "dynamic_geographic_by_acts"),
        uiOutput(outputId = 'dynamic_age_histogram_by_acts'),
        uiOutput(outputId = 'dynamic_prov_histogram_by_acts')
      ),
      
      #### GHM TAB ####
      tabItem(tabName = "ghm",
        fluidRow(
          box(
            selectizeInput(
              inputId = 'chosen_ghm',
              label = h4('Filtrer par GHM'),
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Tapez un ou plusieurs code(s)/libellé(s)"
              )
            ), 
            actionButton("ghm_button", "Valider"), 
            actionButton("ghm_reset", "Effacer"), 
            actionButton("ghm_all", "Tout sélectionner"),
            width = 6
          ),
          box(
            fileInput(
              inputId = "ghm_file", 
              label = h4("Liste de codes ?"),
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              ),
              buttonLabel = "Parcourir",
              placeholder = ".csv codes/libellés"
            ),
            width = 6
          )
        ),
        fluidRow(
          valueBoxOutput("n_sejours_by_ghm", width = 3),
          valueBoxOutput("n_patients_by_ghm", width = 3),
          valueBoxOutput("total_sejour_by_ghm", width = 3),
          valueBoxOutput("moyenne_sejour_by_ghm", width = 3)
        ),
        uiOutput(outputId = "dynamic_ghm_tables"),
        uiOutput(outputId = "dynamic_geographic_by_ghm"),
        uiOutput(outputId = 'dynamic_age_histogram_by_ghm'),
        uiOutput(outputId = 'dynamic_prov_histogram_by_ghm')
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
  
  session$onSessionEnded(stopApp) #stops app when closing browser tab
  
  options(
    shiny.maxRequestSize=30*1024^2,
    DT.options = list(
      scrollY="400px",
      scrollCollapse=TRUE,
      paging=FALSE,
      searching=FALSE,
      language=list(
        url='//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
      ),
      dom = "tip"
    )
  )
  
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
  cmd <- data.frame(code=names(cmd), libelle=unlist(cmd))
  cmd$code <- as.character(cmd$code)
  cmd$libelle <- as.character(cmd$libelle)
  
  ghm <- list(
    "C"="Opératoire", 
    "K"="Non-opératoire",
    "M"="Acte(s) non-classant(s)",
    "Z"="Indifférencié"
  )
  ghm <- data.frame(code=names(ghm), libelle=unlist(ghm))
  ghm$code <- as.character(ghm$code)
  ghm$libelle <- as.character(ghm$libelle)
  
  keep_columns <- c(
    "No.resume",           
    "NIP",                
    "NDA",                              
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
        title = "Instructions",
        solidHeader = TRUE,
        collapsible = TRUE,
        status = "primary",
        "Instructions détaillées sur", tagList(github), br(),
        HTML("<u> Résumé</u> :"), br(),
        HTML("1) Se rendre sur SIMPA"), br(),
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
        title = "Qu'est-ce que c'est ?",
        solidHeader = TRUE,
        collapsible = TRUE,
        status = "primary",
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
      stringsAsFactors = FALSE, 
      fileEncoding="latin1", 
      na.strings = c("", " ", "NA")
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
    
    data$annee.entree <- as.factor(year(data$Date.entree.resume))
    data$mois.entree <- as.factor(month(data$Date.entree.resume))
    
    if (length(diagnoses) > 0) {
      data$diagnoses = apply(
        data[, diagnoses], 
        1, 
        function(x) unique(unname(c(x[!is.na(x)])))
      )
    } else {
      data$diagnoses <- NA
    }
    
    if (length(acts) > 0) {
      data$acts = apply(
        data[, acts], 
        1, 
        function(x) unique(unname(c(x[!is.na(x)])))
      )
    } else {
      data$acts <- NA
    }
    
    if (length(dad) > 0) {
      data$dad = apply(
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
    year <- as.numeric(tail(names(sort(table(load_data()$annee.entree))), 1))
    return(year)
  })
  
  data <- reactive({
    req(
      load_data(), 
      data_cmd(),
      input$UH_filter, 
      input$cmd_filter, 
      input$GHM_lettre_filter,
      input$mode_ent_filter,
      input$dad_filter
    )
    data = load_data()
    UH_list = input$UH_filter
    cmd_list = unlist(data_cmd()[data_cmd()$value %in% input$cmd_filter, "code"])
    GHM_lettre_list = input$GHM_lettre_filter
    
    min_date <- input$date_range[1]
    max_date <- input$date_range[2]
    
    data <-(
      data[(
        (data$Date.entree.resume >= min_date)
        & (data$Date.entree.resume <= max_date)
        & (data$UH %in% UH_list)
        & (data$cmd %in% cmd_list)
        & (data$GHM_lettre %in% GHM_lettre_list)
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
  
  load_diags <- reactive({
    req(input$diag_file)
    data <- read.csv2(
      input$diag_file$datapath, 
      stringsAsFactors = FALSE, 
      fileEncoding="latin1", 
      na.strings = c("", " ", "NA"),
      sep = ";",
      strip.white = TRUE
    )
    return(data)
  })
  
  load_acts <- reactive({
    req(input$acts_file)
    data <- read.csv2(
      input$acts_file$datapath, 
      stringsAsFactors = FALSE, 
      fileEncoding="latin1", 
      na.strings = c("", " ", "NA"),
      sep = ";",
      strip.white = TRUE
    )
    return(data)
  })
  
  load_ghm <- reactive({
    req(input$ghm_file)
    data <- read.csv2(
      input$ghm_file$datapath, 
      stringsAsFactors = FALSE, 
      fileEncoding="latin1", 
      na.strings = c("", " ", "NA"),
      sep = ";",
      strip.white = TRUE
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
        ghm_ref <- get_table("ghm_ghm_regroupement")
        incProgress(1/4, detail="Mise en place des choix...")
        ghm_ref <- ghm_ref[, c("ghm", "libelle_ghm")]
        colnames(ghm_ref) <- c("code", "libelle")
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
    )[, c("code", "libelle")])
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
    loaded_diags <- unique(unlist(load_diags()$code))
    selected_diags <- choices[choices$code %in% loaded_diags, ]
    updateSelectizeInput(
        session=session,
        inputId='chosen_diagnoses',
        choices=choices,
        selected=selected_diags$value,
        server=TRUE
      )
  })
  
  observeEvent(load_acts(), {
    acts <- data_acts()
    choices <- cbind(
      acts,
      value=seq_len(nrow(acts))
    )
    loaded_acts <- unique(unlist(load_acts()$code))
    selected_acts <- choices[choices$code %in% loaded_acts, ]
    
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
    loaded_ghm <- unique(unlist(load_ghm()$code))
    selected_ghm <- choices[choices$code %in% loaded_ghm, ]
    
    updateSelectizeInput(
      session=session,
      inputId='chosen_ghm',
      choices=choices,
      selected=selected_ghm$value,
      server=TRUE
    )
  })
  
  by_lists <- reactiveValues(
    diag_table=NULL, diag_list=NULL, 
    acts_table=NULL, acts_list=NULL,
    ghm_table=NULL, ghm_list=NULL
  )
  
  all_selected <- reactiveValues(diags=FALSE, acts=FALSE, ghm=FALSE)
  
  observeEvent(input$condition_button, {
    req(input$chosen_diagnoses)
    chosen_diagnoses <- input$chosen_diagnoses
    by_lists$diag_table <- data_cim()[chosen_diagnoses,]
    by_lists$diag_list <- setNames(
      as.character(by_lists$diag_table$code), by_lists$diag_table$libelle
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
  
  observeEvent(input$condition_reset, {
    updateSelectizeInput(
      session = session,
      inputId = 'chosen_diagnoses',
      selected = character(0)
    )
    by_lists$diag_table <- NULL
    by_lists$diag_list <- NULL
    all_selected$diags <- FALSE
  })
  
  observeEvent(input$acts_reset, {
    updateSelectizeInput(
      session = session,
      inputId = 'chosen_acts',
      selected = character(0)
    )
    by_lists$acts_list <- NULL
    by_lists$acts_list <- NULL
    all_selected$acts <- FALSE
  })
  
  observeEvent(input$ghm_reset, {
    updateSelectizeInput(
      session = session,
      inputId = 'chosen_ghm',
      selected = character(0)
    )
    by_lists$ghm_list <- NULL
    by_lists$ghm_list <- NULL
    all_selected$ghm <- FALSE
  })
  
  observeEvent(input$condition_all, {
    by_lists$diag_table <- data_cim()
    by_lists$diag_list <- setNames(
      as.character(by_lists$diag_table$code), by_lists$diag_table$libelle
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
        inputId = "date_range",
        label = h4("Période"),
        start = min(as.Date(load_data()$Date.entree.resume, na.rm = TRUE)),
        end = max(as.Date(load_data()$Date.entree.resume, na.rm = TRUE)),
        format = "dd-mm-yyyy",
        separator = "-",
        weekstart = 1,
        language = "fr"
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
        inputId = 'UH_filter',
        label = h4('Filtrer par UH'),
        choices = load_data()$UH,
        selected = load_data()$UH,
        multiple = TRUE,
        options = list(
          placeholder = 'Taper et/ou sélectionner'
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
        inputId = 'cmd_filter',
        label = h4('CMD'),
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = 'Taper et/ou sélectionner'
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
        inputId = 'GHM_lettre_filter',
        label = h4('Catégorie de GHM'),
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = 'Taper et/ou sélectionner'
        )
      )
    })
  })
  
  observeEvent(load_data(), {
    updateSelectizeInput(
      session, 'GHM_lettre_filter',
      choices = load_data()$GHM_lettre,
      selected = load_data()$GHM_lettre,
      server = TRUE
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
        inputId = 'mode_ent_filter',
        label = h4("Mode d'entrée"),
        choices = NULL,
        multiple = TRUE
      )
    })
  })
  
  observeEvent(load_data(), {
    updateSelectizeInput(
      session, 'mode_ent_filter',
      choices = load_data()$Mode.ent,
      selected = load_data()$Mode.ent,
      server = TRUE
    )
  })
  
  observeEvent(load_data(), {
    output$dynamic_dad <- renderUI({
      selectInput(
        inputId = 'dad_filter',
        label = h4("Filtrer DAD en MR"),
        choices = c("Oui", "Non"),
        selected = "Non",
        multiple = FALSE
      )
    })
  })
  
  #### TABS ####
  
  observeEvent(condition_table(), {
    output$dynamic_condition_tables <- renderUI({
      if (is.null(condition_table())) {
        hide("dynamic_condition_tables")
      } else {
        show("dynamic_condition_tables")
      }
      fluidRow(
        box(
          DTOutput("condition_table"),
          title = "Décompte par diagnostique",
          width = 12
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
          title = "Décompte par acte",
          width = 12
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
          title = "Décompte par GHM",
          width = 12
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
        box(plotOutput("age_histogram", height="370px"), width = 6),
        box(
          DTOutput("age_table"), 
          title = "Répartition des âges",
          width = 6
        )
      )
    })
  })
  
  observeEvent(age_histogram_by_condition(), {
    output$dynamic_age_histogram_by_condition <- renderUI({
      if (is.null(age_histogram_by_condition())) {
        hide("dynamic_age_histogram_by_condition")
      } else {
        show("dynamic_age_histogram_by_condition")
      }
      fluidRow(
        box(
          plotOutput("age_histogram_by_condition", height="370px"), 
          width = 6
        ),
        box(
          DTOutput("age_table_by_condition"), 
          title = "Répartition des âges",
          width = 6
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
          width = 6
        ),
        box(
          DTOutput("age_table_by_acts"), 
          title = "Répartition des âges",
          width = 6
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
          width = 6
        ),
        box(
          DTOutput("age_table_by_ghm"), 
          title = "Répartition des âges",
          width = 6
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
          title = "URM d'origine", 
          width = 4
        ),
        box(
          DTOutput("mode_ent_table"),
          title = "Mode d'entrée", 
          width = 4
        ),
        box(
          DTOutput("GHM_lettre_table"),
          title = "Catégories de GHM", 
          width = 4
        )
      )
    })
  })
  
  observeEvent(URM_origine_table_by_condition(), {
    output$dynamic_prov_histogram_by_condition <- renderUI({
      if (is.null(URM_origine_table_by_condition())) {
        hide("dynamic_prov_histogram_by_condition")
      } else {
        show("dynamic_prov_histogram_by_condition")
      }
      fluidRow(
        box(
          DTOutput("URM_origine_table_by_condition"),
          title = "URM d'origine", 
          width = 4
        ),
        box(
          DTOutput("mode_ent_table_by_condition"),
          title = "Mode d'entrée", 
          width = 4
        ),
        box(
          DTOutput("GHM_lettre_table_by_condition"),
          title = "Catégories de GHM", 
          width = 4
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
          title = "URM d'origine", 
          width = 4
        ),
        box(
          DTOutput("mode_ent_table_by_acts"),
          title = "Mode d'entrée", 
          width = 4
        ),
        box(
          DTOutput("GHM_lettre_table_by_acts"),
          title = "Catégories de GHM", 
          width = 4
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
          title = "URM d'origine", 
          width = 4
        ),
        box(
          DTOutput("mode_ent_table_by_ghm"),
          title = "Mode d'entrée", 
          width = 4
        ),
        box(
          DTOutput("GHM_lettre_table_by_ghm"),
          title = "Catégories de GHM", 
          width = 4
        )
      )
    })
  })
  
  observeEvent(categorie_stats(), {
    output$dynamic_categorie <- renderUI({
      if (is.null(categorie_stats())) {
        hide("dynamic_categorie")
      } else {
        show("dynamic_categorie")
      }
      fluidRow(
        box(
          DTOutput("categorie_stats"),
          title = "Statistiques sur la catégorie",
          width = 8
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
          title = "Répartition géographique",
          width = 12
        )
      )
    })
  })
  
  observeEvent(geographic_by_condition(), {
    output$dynamic_geographic_by_condition <- renderUI({
      if (is.null(geographic_by_condition())) {
        hide("dynamic_geographic_by_condition")
      } else {
        show("dynamic_geographic_by_condition")
      }
      fluidRow(
        box(
          DTOutput("geographic_by_condition"),
          title = "Répartition géographique pour les diagnostiques sélectionnés",
          width = 12
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
          title = "Répartition géographique pour les actes sélectionnés",
          width = 12
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
          title = "Répartition géographique pour les GHM sélectionnés",
          width = 12
        )
      )
    })
  })
  
  #####################################
  ### FUNCTIONS TO GENERATE OBJECTS ###
  #####################################
  
  data_by <- function(data, by_column) {
    if (by_column == "diagnoses") {
      by_list = by_lists$diag_list
    } else if (by_column == "acts") {
      by_list = by_lists$acts_list
    } else if (by_column == "GHM") {
      by_list = by_lists$ghm_list
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
      if (by_column == "diagnoses") {
        by_list = by_lists$diag_list
        by_table = by_lists$diag_table
      } else if (by_column == "acts") {
        by_list = by_lists$acts_list
        by_table = by_lists$acts_table
      } else if (by_column == "GHM") {
        by_list = by_lists$ghm_list
        by_table = by_lists$ghm_table
      }
      n_by = data.frame(
        n_sejours=integer(0), 
        n_patients=integer(0),
        tot_sej=integer(0),
        moy_sej=numeric(0),
        urgences=integer(0)
      )
      if (nrow(df) > 0) {
        for(element in by_list){
          incProgress(1/length(by_list))
          df[[element]] = (
            apply(
              df, 1, function(x) element %in% unlist(x[[by_column]])
            )
          )
          n_sejour = sum(df[[element]])
          n_patient = sum(
            tapply(
              df[[by_column]], df[["NIP"]],
              function(x) element %in% unlist(c(x))
            )
          )
          total_sejour = sum(df[df[[element]]==1, "Duree.sejour"])
          moyenne_sejour = mean(df[df[[element]]==1, "Duree.sejour"])
          entree_urgences = nrow(
            df[(df[[element]]==1) & (df[["Mode.ent"]]=="Urgences"), ]
          )
          
          n_by[element, ] <- c(
            n_sejour,
            n_patient,
            total_sejour,
            round(moyenne_sejour, digits=2),
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
            0
          )
        }
      }
      n_by <- merge(
        by_table, n_by,
        by.x="code", by.y="row.names",
        all.x=FALSE, all.y=TRUE
      )
      return(n_by)
    })
  }
  
  global_stats_by <- function(data){
    n_sejour_global <- nrow(data)
    n_patient_global <- length(unique(data$NIP))
    total_sejour_global <- sum(data$Duree.sejour)
    moyenne_sejour_global <- mean(data$Duree.sejour)
    global_stats = c(
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
        round(
          unlist(stats_table[stat]), 
          digits=3
        ), 
        "Durée moyenne de séjour", 
        icon = icon("clock"),
        color = "yellow"
      )
    } else if (stat == "total_sejour") {
      valueBox(
        unlist(stats_table[stat]), 
        "Durée totale des séjours", 
        icon = icon("clock"),
        color = "yellow"
      )
    } else if (stat == "n_patients") {
      valueBox(
        unlist(stats_table[stat]), 
        "Nb de patients", 
        icon = icon("list"),
        color = "purple"
      )
    } else if (stat == "n_sejours") {
      valueBox(
        unlist(stats_table[stat]), 
        "Nb de séjours", 
        icon = icon("list"),
        color = "purple"
      )
    }
  } 
  
  plot_age_by <- function(data) {
    age_histogram <- ggplot(data, aes(x=Age)) + 
      geom_bar(color="coral", fill="coral", alpha=0.3) +
      labs(x="Age", y="Effectifs")
    return(age_histogram)
  }
  
  age_table_by <- function(data) {
    age_cat <- cut(
      data$Age, 
      breaks=c(0, 18, 25, 40, 60, 80, 100, 200),
      labels=c("<18", "18-25", "25-40", "40-60", "60-80", "80-100", "100+"),
      right = FALSE
    )
    
    age_table <- data.frame(table(age_cat))
    colnames(age_table) <- c("Âge", "n_sejours")
    age_table$`%` <- round((100 * age_table$n_sejours) / nrow(data), digits=2)
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
    GHM_output$`%` <- round((100 * GHM_output$Freq) / nrow(data), digits=2)
    return(GHM_output)
  }
  
  URM_origine_by <- function(data){
    etablissement <- etablissement()
    if (etablissement == "bct") {
      URM_list <- read.csv("urm_bct.csv", strip.white=TRUE, header=TRUE)
    } else if (etablissement == "pbr") {
      URM_list <- read.csv2("urm_pbr.csv", strip.white=TRUE, header=TRUE)
    } else {
      URM_list <- data.frame(URM=character(0), libelle=character(0))
    }
    URM_origine_table <- data.frame(
      table(data$URM.orig, dnn="URM origine")
    )
    URM_origine_table$URM.origine <- (
      sprintf("%03d", as.numeric(levels(URM_origine_table$URM.origine)))
    )
    URM_output <- merge(
      URM_origine_table, URM_list, 
      by.x="URM.origine", by.y="URM", 
      all.x=TRUE, all.y=FALSE
    )
    rownames(URM_output) <- URM_output$URM.origine
    URM_output <- (
      URM_output[
        order(URM_output$Freq, decreasing=TRUE),
        c("libelle", "Freq")
        ]
    )
    URM_output$`%` <- round((100 * URM_output$Freq) / nrow(data), digits=2)
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
    n_patient_idf = sum(
      tapply(
        df$is_idf, df$NIP,
        function(x) sum(x) > 0
      )
    )
    n_patient_france = sum(
      tapply(
        df$is_france, df$NIP,
        function(x) sum(x) > 0
      )
    )
    n_sejour_idf <- sum(df$is_idf)
    n_sejour_france <- sum(df$is_france)
    tot_sejour = nrow(df)
    tot_patient = length(unique(df$NIP))
    
    geographic_global = data.frame(
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
    output$`%` <- round((100 * output$Freq) / nrow(data), digits=2)
    return(output)
  }
  
  etablissement <- reactive({
    data <- load_data()
    nda <- data$NDA[1]
    etab_code <- substr(nda, 1, 3)
    if (etab_code == 102) {
      etablissement = "bct"
    } else if (etab_code == 961) {
      etablissement = "pbr"
    } else {
      etablissement = "unk"
    }
    return(etablissement)
  })
  
  #################################
  ### GENERATE TABLES AND PLOTS ###
  #################################
  
  data_by_condition <- reactive({
    req(data())
    req(by_lists$diag_list)
    data_by(data(), "diagnoses")
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
    if (all_selected$diags == TRUE) {
      return("Tous")
    } else if (is.null(input$diag_file)) {
      return(unname(by_lists$diags_list))
    } else {
      diags <- unique(unlist(c(unname(by_lists$diags_list), load_diags()$code)))
      return(diags)
    }
  })
  
  acts_given <- reactive({
    if (all_selected$acts == TRUE) {
      return("Tous")
    } else if (is.null(input$acts_file)) {
      return(unname(by_lists$acts_list))
    } else {
      acts <- unique(unlist(c(unname(by_lists$acts_list), load_acts()$code)))
      return(acts)
    }
  })
  
  ghm_given <- reactive({
    if (all_selected$ghm == TRUE) {
      return("Tous")
    } else if (is.null(input$ghm_file)) {
      return(unname(by_lists$ghm_list))
    } else {
      ghm_list <- unique(unlist(c(unname(by_lists$ghm_list), load_ghm()$code)))
      return(ghm_list)
    }
  })
  
  condition_table <- reactive({
    req(data_by_condition())
    n_by_condition <- table_by(data_by_condition(), "diagnoses")
    return(n_by_condition)
  })
  
  acts_table <- reactive({
    req(data_by_acts())
    n_by_acts <- table_by(data_by_acts(), "acts")
    return(n_by_acts)
  })
  
  ghm_table <- reactive({
    req(data_by_ghm())
    n_by_ghm <- table_by(data_by_ghm(), "GHM")
    return(n_by_ghm)
  })
  
  categorie_stats <- reactive({
    req(acts_table()$categorie)
    data <- acts_table()
    tot_sejour = sum(data$n_sejours)
    tot_jours = sum(data$total_sejour)
    stats <- (
      data %>% 
        group_by(categorie) %>% 
        summarise(
          n_sejours = sum(n_sejours), 
          `% sejours` = round(100 * sum(n_sejours) / tot_sejour, digits=2),
          total_durée = sum(total_sejour),
          `% duree` = round(100 * sum(total_sejour) / tot_jours, digits=2)
        )
    )
    return(stats)
  })
  
  categorie_table <- reactive({
    if (exists("categorie", where=acts_table())) {
      table <- datatable(
        data=categorie_stats(),
        style="bootstrap",
        rownames = FALSE,
        options=list(dom="tp")
      )
      return(table)
    } else {
      return("NA")
    }
  })
  
  global_stats <- reactive({
    req(data())
    global_stats_by(data())
  })
  
  global_stats_by_condition <- reactive({
    req(data_by_condition())
    global_stats_by(data_by_condition())
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
    req(data())
    plot_age_by(data())
  })
  
  age_histogram_by_condition <- reactive({
    req(data_by_condition())
    plot_age_by(data_by_condition())
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
    req(data())
    age_table_by(data())
  })
  
  age_table_by_condition <- reactive({
    req(data_by_condition())
    age_table_by(data_by_condition())
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
    req(data())
    GHM_lettre_by(data())
  })
  
  GHM_lettre_table_by_condition <- reactive({
    req(data_by_condition())
    GHM_lettre_by(data_by_condition())
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
    req(data(), etablissement())
    URM_origine_by(data())
  })
  
  URM_origine_table_by_condition <- reactive({
    req(data_by_condition(), etablissement())
    URM_origine_by(data_by_condition())
  })
  
  URM_origine_table_by_acts <- reactive({
    req(data_by_acts(), etablissement())
    URM_origine_by(data_by_acts())
  })
  
  URM_origine_table_by_ghm <- reactive({
    req(data_by_ghm(), etablissement())
    URM_origine_by(data_by_ghm())
  })
  
  geographic_global <- reactive({
    req(data())
    geographic_by(data())
  })
  
  geographic_by_condition <- reactive({
    req(data_by_condition())
    geographic_by(data_by_condition())
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
    req(data())
    mode_ent_by(data())
  })
  
  mode_ent_table_by_condition <- reactive({
    req(data_by_condition())
    mode_ent_by(data_by_condition())
  })
  
  mode_ent_table_by_acts <- reactive({
    req(data_by_acts())
    mode_ent_by(data_by_acts())
  })
  
  mode_ent_table_by_ghm <- reactive({
    req(data_by_ghm())
    mode_ent_by(data_by_ghm())
  })
  
  ##################################
  ### RENDERING TABLES AND PLOTS ###
  ##################################
  
  output$condition_table <- renderDT({
    req(condition_table())
    datatable(
      condition_table(),
      autoHideNavigation=FALSE,
      width = "auto",
      rownames = FALSE,
      extensions = 'Buttons',
      options=list(
        dom="Blfrtip", 
        searching=TRUE,
        buttons = list(
          list(extend = 'collection',
               buttons = c('copy', 'excel', 'csv'),
               text = 'Exporter tableau')
        )
      )
    )
  })
  
  output$acts_table <- renderDT({
    req(acts_table)
    datatable(
      acts_table(),
      autoHideNavigation=FALSE,
      width = "auto",
      rownames = FALSE,
      extensions = 'Buttons',
      options=list(
        dom="Blfrtip", 
        searching=TRUE,
        buttons = list(
          list(extend = 'collection',
               buttons = c('copy', 'excel', 'csv'),
               text = 'Exporter tableau')
        )
      )
    )
  })
  
  output$ghm_table <- renderDT({
    req(ghm_table)
    datatable(
      ghm_table(),
      autoHideNavigation=FALSE,
      width = "auto",
      rownames = FALSE,
      extensions = 'Buttons',
      options=list(
        dom="Blfrtip", 
        searching=TRUE,
        buttons = list(
          list(extend = 'collection',
               buttons = c('copy', 'excel', 'csv'),
               text = 'Exporter tableau')
        )
      )
    )
  })
  
  output$categorie_stats <- renderDT({
    req(categorie_stats())
    datatable(
      categorie_stats(),
      rownames = FALSE
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
  
  output$n_sejours_by_condition <- renderValueBox({
    req(global_stats_by_condition())
    value_box_by(global_stats_by_condition(), "n_sejours")
  })
  
  output$n_patients_by_condition <- renderValueBox({
    req(global_stats_by_condition())
    value_box_by(global_stats_by_condition(), "n_patients")
  })
  
  output$total_sejour_by_condition <- renderValueBox({
    req(global_stats_by_condition())
    value_box_by(global_stats_by_condition(), "total_sejour")
  })
  
  output$moyenne_sejour_by_condition <- renderValueBox({
    req(global_stats_by_condition())
    value_box_by(global_stats_by_condition(), "moyenne_sejour")
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
  
  output$geographic_by_condition <- renderDT({
    req(geographic_by_condition())
    datatable(
      geographic_by_condition(),
      autoHideNavigation=TRUE,
      width = "auto",
      rownames = TRUE
    )
  })
  
  output$geographic_by_acts <- renderDT({
    req(geographic_by_acts())
    datatable(
      geographic_by_acts(),
      autoHideNavigation=TRUE,
      width = "auto",
      rownames = TRUE
    )
  })
  
  output$geographic_by_ghm <- renderDT({
    req(geographic_by_ghm())
    datatable(
      geographic_by_ghm(),
      autoHideNavigation=TRUE,
      width = "auto",
      rownames = TRUE
    )
  })
  
  output$age_histogram <- renderPlot({
    req(age_histogram())
    age_histogram()
  })
  
  output$age_histogram_by_condition <- renderPlot({
    req(age_histogram_by_condition())
    age_histogram_by_condition()
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
  
  output$age_table_by_condition <- renderDT({
    req(age_table_by_condition())
    datatable(
      age_table_by_condition(),
      rownames=FALSE
    )
  })
  
  output$age_table_by_acts<- renderDT({
    req(age_table_by_acts())
    datatable(
      age_table_by_acts(),
      rownames=FALSE
    )
  })
  
  output$age_table_by_ghm<- renderDT({
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
  
  output$GHM_lettre_table_by_condition <- renderDT({
    req(GHM_lettre_table_by_condition())
    datatable(
      GHM_lettre_table_by_condition(),
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
      options = list (
        pageLength=5,
        paging=TRUE,
        scrollY=FALSE,
        dom = "tp"
      )
    )
  })
  
  output$URM_origine_table_by_condition <- renderDT({
    req(URM_origine_table_by_condition())
    datatable(
      URM_origine_table_by_condition(),
      rownames=TRUE,
      options = list (
        pageLength=5,
        paging=TRUE,
        scrollY=FALSE,
        dom = "tp"
      )
    )
  })
  
  output$URM_origine_table_by_acts <- renderDT({
    req(URM_origine_table_by_acts())
    datatable(
      URM_origine_table_by_acts(),
      rownames=TRUE,
      options = list (
        pageLength=5,
        paging=TRUE,
        scrollY=FALSE,
        dom = "tp"
      )
    )
  })
  
  output$URM_origine_table_by_ghm <- renderDT({
    req(URM_origine_table_by_ghm())
    datatable(
      URM_origine_table_by_ghm(),
      rownames=TRUE,
      options = list (
        pageLength=5,
        paging=TRUE,
        scrollY=FALSE,
        dom = "tp"
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
  
  output$mode_ent_table_by_condition <- renderDT({
    req(mode_ent_table_by_condition())
    datatable(
      mode_ent_table_by_condition(),
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
  
  ##########################
  ### GENERATING REPORTS ###
  ##########################
  #### GLOBAL ####
  
  observeEvent(data(), {
    output$download_report <- renderUI({
        div(
          style="display:inline-block;text-align: center;width: 100%;",
          downloadButton("report", "Rapport global")
        )
      })
    })
    
  observeEvent(data(), {
    output$report <- downloadHandler(
      filename = function() {
        paste(
          paste(unique(data()$URMP), collapse="-"), 
          "_global_", 
          Sys.Date(), 
          ".html", 
          sep=""
        )
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite=TRUE)
        params <- list(
          URM=unique(data()$URMP),
          UH_list=input$UH_filter,
          cmd_list=input$cmd_filter,
          GHM_lettre_list=input$GHM_lettre_filter,
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
            rownames = FALSE,
            options=list(dom="tp")
          ),
          URM_origine_table=datatable(
            data=URM_origine_table(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          GHM_lettre_table=datatable(
            data=GHM_lettre_table(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          mode_ent_table=datatable(
            data=mode_ent_table(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          age_histogram=age_histogram()
        )
        rmarkdown::render(
          tempReport, output_file=file,
          params=params,
          clean=TRUE,
          envir=new.env(parent = globalenv()),
          encoding="UTF-8"
        )
      }
    )
  })
  
  #### CONDITION ####
  
  observeEvent(condition_table(), {
    output$download_report_diags <- renderUI({
      if (is.null(condition_table())) {
        hide("download_report_diags")
      } else {
        show("download_report_diags")
      }
      div(
        style="display:inline-block;text-align: center;width: 100%;",
        downloadButton("report_diags", "Rapport par diagnostiques")
      )
    })
  })
  
  observeEvent(condition_table(), {
    output$report_diags <- downloadHandler(
      filename = function() {
        paste(
          paste(unique(data_by_condition()$URMP), collapse="-"), 
          "_diags_", 
          Sys.Date(), 
          ".html", 
          sep=""
        )
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        params <- list(
          URM=unique(data_by_condition()$URMP),
          UH_list=input$UH_filter,
          cmd_list=input$cmd_filter,
          GHM_lettre_list=input$GHM_lettre_filter,
          dad_filter=input$dad_filter,
          diags_given=diags_given(),
          date_range=input$date_range,
          global_stats=global_stats_by_condition(),
          geographic_global=datatable(
            data=geographic_by_condition(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          age_table=datatable(
            data=age_table_by_condition(),
            style="bootstrap",
            rownames = FALSE,
            options=list(dom="tp")
          ),
          condition_table=datatable(
            data=condition_table(),
            style="bootstrap",
            extensions = 'Buttons',
            options=list(
              dom="Blfrtip", 
              searching=TRUE,
              buttons = list(
                list(extend = 'collection',
                     buttons = c('copy', 'excel', 'csv'),
                     text = 'Exporter tableau')
              )
            )
          ),
          URM_origine_table=datatable(
            data=URM_origine_table_by_condition(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          GHM_lettre_table=datatable(
            data=GHM_lettre_table_by_condition(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          mode_ent_table=datatable(
            data=mode_ent_table_by_condition(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          age_histogram=age_histogram_by_condition()
        )
        rmarkdown::render(
          tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv()),
          encoding = "UTF-8"
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
      div(
        style="display:inline-block;text-align: center;width: 100%;",
        downloadButton("report_acts", "Rapport par actes")
      )
    })
  })
  
  observeEvent(acts_table(), {
    output$report_acts <- downloadHandler(
      filename = function() {
        paste(
          paste(unique(data_by_acts()$URMP), collapse="-"),
          "_acts_", 
          Sys.Date(), 
          ".html", 
          sep=""
        )
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        params <- list(
          URM=unique(data_by_acts()$URMP),
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
            rownames = FALSE,
            options=list(dom="tp")
          ),
          acts_table=datatable(
            data=acts_table(),
            style="bootstrap",
            rownames = FALSE,
            extensions = 'Buttons',
            options=list(
              dom="Blfrtip", 
              searching=TRUE,
              buttons = list(
                list(extend = 'collection',
                     buttons = c('copy', 'excel', 'csv'),
                     text = 'Exporter tableau')
              )
            )
          ),
          categorie_table=categorie_table(),
          URM_origine_table=datatable(
            data=URM_origine_table_by_acts(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          GHM_lettre_table=datatable(
            data=GHM_lettre_table_by_acts(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          mode_ent_table=datatable(
            data=mode_ent_table_by_acts(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          age_histogram=age_histogram_by_acts()
        )
        rmarkdown::render(
          tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv()),
          encoding = "UTF-8"
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
      div(
        style="display:inline-block;text-align: center;width: 100%;",
        downloadButton("report_ghm", "Rapport par GHM")
      )
    })
  })
  
  observeEvent(ghm_table(), {
    output$report_ghm <- downloadHandler(
      filename = function() {
        paste(
          paste(unique(data_by_ghm()$URMP), collapse="-"),
          "_ghm_", 
          Sys.Date(), 
          ".html", 
          sep=""
        )
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        params <- list(
          URM=unique(data_by_ghm()$URMP),
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
            rownames = FALSE,
            options=list(dom="tp")
          ),
          ghm_table=datatable(
            data=ghm_table(),
            style="bootstrap",
            rownames = FALSE,
            extensions = 'Buttons',
            options=list(
              dom="Blfrtip", 
              searching=TRUE,
              buttons = list(
                list(extend = 'collection',
                     buttons = c('copy', 'excel', 'csv'),
                     text = 'Exporter tableau')
              )
            )
          ),
          URM_origine_table=datatable(
            data=URM_origine_table_by_ghm(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          GHM_lettre_table=datatable(
            data=GHM_lettre_table_by_ghm(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          mode_ent_table=datatable(
            data=mode_ent_table_by_ghm(),
            style="bootstrap",
            options=list(dom="tp")
          ),
          age_histogram=age_histogram_by_ghm()
        )
        rmarkdown::render(
          tempReport, output_file = file,
          params = params,
          envir = new.env(parent = globalenv()),
          encoding = "UTF-8"
        )
      }
    )
  })
}

shinyApp(ui, server)