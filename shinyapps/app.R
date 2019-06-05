## app.R ##

library(shinydashboard)
library(tidyverse)
library(lubridate)
library(DT)

########
## UI ##
########

#### UI HEADER ####
DBheader <- dashboardHeader(
  title = "Thot"
)

#### UI SIDEBAR ####
DBsidebar <- dashboardSidebar(
  sidebarMenu(
    tags$style(
      paste(
        "#report {color: #444; margin-bottom:15px;}",
        "#report_diags {color: #444; margin-bottom:15px;}",
        "#report_acts {color: #444; margin-bottom:15px;}",
        sep=" "
      )
    ),
    
    menuItem("Global", tabName = "global", icon = icon("globe")),
    menuItem("Diagnostiques", tabName = "diagnostics", icon = icon("search")),
    menuItem("Actes", tabName = "acts", icon = icon("search"))
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
  uiOutput(outputId = "dynamic_GHM_num"),
  uiOutput(outputId = "dynamic_GHM_lettre"),
  uiOutput(outputId = "download_report"),
  uiOutput(outputId = "download_report_diags"),
  uiOutput(outputId = "download_report_acts")
)

#### UI BODY ####
DBbody <- dashboardBody(
  
  tabItems(
    
    tabItem(tabName = "global",
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
          ), width = 6
        )
      ),
      fluidRow(
        valueBoxOutput("n_sejours_by_condition", width = 3),
        valueBoxOutput("n_patients_by_condition", width = 3),
        valueBoxOutput("total_sejour_by_condition", width = 3),
        valueBoxOutput("moyenne_sejour_by_condition", width = 3)
      ), 
      uiOutput(outputId = "dynamic_condition_tables"),
      uiOutput(outputId = "dynamic_geographic_tables"),
      uiOutput(outputId = 'dynamic_age_histogram_by_condition')
    ),
    
    tabItem(tabName = "acts",
      fluidRow(
        box(
          selectizeInput(
            inputId = 'chosen_acts',
            label = h4('Filtrer par actes CCAM'),
            choices = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Tapez un ou plusieurs code(s)"
            )
          ), 
          actionButton("acts_button", "Valider"), 
          actionButton("acts_reset", "Effacer"), 
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
          ), width = 6
        )
      ),
      fluidRow(
        valueBoxOutput("n_sejours_by_act", width = 3),
        valueBoxOutput("n_patients_by_act", width = 3),
        valueBoxOutput("total_sejour_by_act", width = 3),
        valueBoxOutput("moyenne_sejour_by_act", width = 3)
      ),
      uiOutput(outputId = "dynamic_acts_tables"),
      fluidRow(
        uiOutput(outputId = "dynamic_intervention")
      )
    )
  )
) 

#### GENERATING UI ####

ui <- dashboardPage(
  DBheader,
  DBsidebar,
  DBbody
)

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
    "URM.dest",
    "Provenance.G"
  )
  
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
    
    diagnoses <- grep(
      "CIM.SIGN|CIM.principal|Diag.Relie$", colnames(data), value=TRUE
    )
    acts <- grep("CCAM.", colnames(data), value=TRUE)
    
    data <- data[, c(keep_columns, diagnoses, acts)]
    
    data$Date.naiss <- dmy(data$Date.naiss)
    data$Date.entree.resume <- dmy_hm(data$Date.entree.resume)
    data$Date.sortie.resume <- dmy_hm(data$Date.sortie.resume)
    data$GHM_num <- substr(data$GHM, 0, 2)
    data$GHM_lettre <- substr(data$GHM, 3, 3)
    
    data$Age <- (
      as.duration(
        interval(data$Date.naiss, date(data$Date.entree.resume))
      ) %/% 
        as.duration(years(1))
    )
    
    data$Mode.ent <- substr(data$Mode.ent, 0, 1)
    data$Mode.ent <- (
      ifelse(
        data$Mode.ent == 0, "Transfert provisoire", 
        ifelse(
          data$Mode.ent == 6, "Mutation",
          ifelse(
            data$Mode.ent == 7, "Transfert",
            ifelse(
              data$Mode.ent == 8, "Domicile", "Inconnu"
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
                data$Mode.sor == 8, "Deces", "Inconnu"
              )
            )
          )
        )
      )
    )
    
    data$Provenance.G <- substr(data$Provenance.G, 0, 1)
    data$Provenance.G <- (
      ifelse(
        data$Provenance.G == 1, "MCO", 
        ifelse(
          data$Provenance.G == 2, "SSR",
          ifelse(
            data$Provenance.G == 3, "SLD",
            ifelse(
              data$Provenance.G == 4, "PSY",
              ifelse(
                data$Provenance.G == 5, "Urgences", data$Provenance.G
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
        function(x) unname(c(x[!is.na(x)]))
      )
    }
    
    if (length(acts) > 0) {
      data$acts = apply(
        data[, acts], 
        1, 
        function(x) unname(c(x[!is.na(x)]))
      )
    }
    
    data = data[
      , (!names(data) %in% diagnoses) & (!names(data) %in% acts)
      ]
    
    return(data)
  })
  
  data <- reactive({
    req(load_data())
    data = load_data()
    UH_list = input$UH_filter
    GHM_num_list = input$GHM_num_filter
    GHM_lettre_list = input$GHM_lettre_filter
    
    min_date <- input$date_range[1]
    max_date <- input$date_range[2]
    
    data <-(
      data[(
        (data$Date.entree.resume >= min_date)
        & (data$Date.entree.resume <= max_date)
        & (data$UH %in% UH_list)
        & (data$GHM_num %in% GHM_num_list)
        & (data$GHM_lettre %in% GHM_lettre_list)
      ), ]
    )
    return(data)
  })
  load_acts <- reactive({
    req(input$acts_file)
    data <- read.csv2(
      input$acts_file$datapath, 
      stringsAsFactors = FALSE, 
      fileEncoding="latin1", 
      na.strings = c("", " ", "NA")
    )
    return(data)
  })
  load_diags <- reactive({
    req(input$diag_file)
    
    data <- read.csv2(
      input$diag_file$datapath, 
      stringsAsFactors = FALSE, 
      fileEncoding="latin1", 
      na.strings = c("", " ", "NA")
    )
    return(data)
  })
  
  ##############################
  ### LOADING INPUTS CHOICES ###
  ##############################
  
  cim <- reactive({
    withProgress(
      message="Chargement du référentiel CIM-10",{
      incProgress(1/4, detail="Téléchargement de la dernière version...")
      temp <- tempfile(fileext = ".zip")
      link <- file.path(
        "https://www.atih.sante.fr",
        "plateformes-de-transmission-et-logiciels",
        "logiciels-espace-de-telechargement",
        "telecharger/gratuit",
        "11616/456"
      )
      download.file(link, temp, mode="wb")
      
      incProgress(1/4, detail="Lecture des données...")
      
      cim <- read.csv2(
        unz(temp, "LIBCIM10MULTI.TXT"), 
        sep="|", 
        header=FALSE,
        stringsAsFactors=FALSE,
        strip.white=TRUE,
        encoding="latin1"
      )
      unlink(temp)
      
      incProgress(1/4, detail="Mise en place des choix...")
      
      cim <- cim[, c(1, 6)]
      colnames(cim) <- c("code", "libelle")
      
      cim$libelle <- as.character(cim$libelle)
    })
    return(cim)
  })
  
  data_cim <- reactive({
    data_cim <- unique(unlist(load_data()$diagnoses))
    cim <- cim()
    cim_part <- cim[cim$code %in% data_cim, ]
    rownames(cim_part) <- NULL
    return(cim_part)
  })
  
  data_acts <- reactive({
    data_acts <- unique(unlist(load_data()$acts))
    return(data_acts)
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
  
  observeEvent(load_diags(), {
    loaded_diags <- unique(unlist(load_diags()$code))
      updateSelectizeInput(
        session=session,
        inputId='chosen_acts',
        selected=loaded_diags,
        server=TRUE
      )
  })
  
  observeEvent(load_acts(), {
    loaded_acts <- unique(unlist(load_acts()$code))
    selected_acts <- base::intersect(data_acts(), loaded_acts)
    updateSelectizeInput(
      session=session,
      inputId='chosen_acts',
      choices=data_acts(),
      selected=selected_acts,
      server=TRUE
    )
  })
  
  observeEvent(data_acts(), {
    updateSelectizeInput(
      session=session, 
      inputId='chosen_acts',
      choices=data_acts(),
      server=TRUE
    )
  })
  
  by_lists <- reactiveValues(diag_table=NULL, diag_list=NULL, acts_list=NULL)
  
  observeEvent(input$condition_button, {
    req(input$chosen_diagnoses)
    chosen_diagnoses <- input$chosen_diagnoses
    by_lists$diag_table <- data_cim()[chosen_diagnoses,]
    by_lists$diag_list <- setNames(
      as.character(by_lists$diag_table$code), by_lists$diag_table$libelle
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
  })
  
  observeEvent(input$acts_button, {
    req(input$chosen_acts)
    by_lists$acts_list = input$chosen_acts
  })
  
  observeEvent(input$acts_reset, {
    updateSelectizeInput(
      session = session,
      inputId = 'chosen_acts',
      selected = character(0)
    )
    by_lists$acts_list = NULL
  })
  
  ############################
  ### DYNAMIC UI RENDERING ###
  ############################
  
  observeEvent(age_table(), {
    output$dynamic_age_histogram <- renderUI({
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
  observeEvent(age_plot_by_condition(), {
    output$dynamic_age_histogram_by_condition <- renderUI({
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
  observeEvent(URM_origine_table(), {
    output$dynamic_prov_histogram <- renderUI({
      fluidRow(
        box(
          DTOutput("URM_origine_table"),
          title = "URM d'origine", 
          width = 6
        ),
        box(
          DTOutput("GHM_lettre_table"),
          title = "Catégories de GHM", 
          width = 6
          )
      )
    })
  })
  observeEvent(load_data(), {
    output$dynamic_date_range <- renderUI({
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
      selectizeInput(
        inputId = 'UH_filter',
        label = h4('Filtrer par UH'),
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
      session, 'UH_filter',
      choices = load_data()$UH,
      selected = load_data()$UH,
      server = TRUE
    )
  })
  observeEvent(load_data(), {
    output$dynamic_GHM_num <- renderUI({
      selectizeInput(
        inputId = 'GHM_num_filter',
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
    updateSelectizeInput(
      session, 'GHM_num_filter',
      choices = load_data()$GHM_num,
      selected = load_data()$GHM_num,
      server = TRUE
    )
    # present_cmd <- unique(load_data()$GHM_num)
    # cmd <- cmd[cmd$code %in% present_cmd,]
    # 
    # choices = cbind(
    #   cmd,
    #   value=seq_len(nrow(cmd))
    # )
    # 
    # updateSelectizeInput(
    #   session=session, 
    #   inputId='GHM_num_filter',
    #   choices=choices,
    #   selected=choices$code,
    #   server=TRUE,
    #   options=list(
    #     optgroups=lapply(
    #       unique(cmd$libelle), 
    #       function(x){
    #         list(value=as.character(x), label=as.character(x))
    #       }
    #     ),
    #     optgroupField='code',
    #     searchField=c('code', 'libelle'),
    #     labelField='code',
    #     render=I("{
    #                    option: function(item, escape) {
    #                    return '<div>' + escape(item.libelle) +'</div>';
    #                    }
    #                   }")
    #   )
    # )
  })
  observeEvent(load_data(), {
    output$dynamic_GHM_lettre <- renderUI({
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
  observeEvent(condition_table(), {
    output$dynamic_condition_tables <- renderUI({
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
      fluidRow(
        box(
          DTOutput("acts_table"),
          title = "Décompte par acte",
          width = 12
        )
      )
    })
  })
  observeEvent(categorie_stats(), {
    output$dynamic_intervention <- renderUI({
      box(
        DTOutput("categorie_stats"),
        title = "Statistiques sur la catégorie",
        width = 6
      )
    })
  })
  observeEvent(geographic_global(), {
    output$dynamic_geographic_global <- renderUI({
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
    output$dynamic_geographic_tables <- renderUI({
      fluidRow(
        box(
          DTOutput("geographic_by_condition"),
          title = "Répartition géographique par diagnostique",
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
    }
    if (nrow(data) > 0) {
      df <- (
        data[
          (apply(data, 1, function(x) any(unlist(x[by_column]) %in% by_list))), 
        ]
      )
    } else {
      df <- data.frame()
    }
    return(df)
  }
  
  table_by <- function(data_by, by_column){
    df <- data_by
    
    if (by_column == "diagnoses") {
      by_list = by_lists$diag_list
    } else if (by_column == "acts") {
      by_list = by_lists$acts_list
    }
    
    n_by = data.frame(
      n_sejours=integer(0), 
      n_patients=integer(0),
      total_sejour=integer(0),
      moyenne_sejour=numeric(0)
    )
    
    if (nrow(df) > 0) {
      for(element in by_list){
        df[, element] = (
          apply(
            df, 1, function(x) element %in% unlist(x[by_column])
          )
        )
        n_sejour = nrow(
          df[df[element]==1, ]
        )
        n_patient = sum(
          tapply(
            df[, by_column], df[, "NIP"],
            function(x) element %in% unlist(c(x))
          )
        )
        total_sejour = sum(df[df[element]==1, "Duree.sejour"])
        moyenne_sejour = mean(df[df[element]==1, "Duree.sejour"])
        
        n_by[element, ] <- c(
          n_sejour,
          n_patient,
          total_sejour,
          round(moyenne_sejour, digits=2)
        )
      }
    } else {
      for(element in by_list){
        n_by[element, ] <- c(
          0,
          0,
          0,
          round(0, digits=2)
        )
      }
    }
    return(n_by)
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
    data$age_categories <- cut(
      data$Age, 
      breaks=c(0, 18, 25, 40, 60, 80, 100, 200),
      labels=c("<18", "18-25", "25-40", "40-60", "60-80", "80-100", "100+"),
      right = FALSE
    )
    
    age_table <- data.frame(table(data$age_categories))
    colnames(age_table) <- c("Âge", "n_sejours")
    age_table$`%` <- round((100 * age_table$n_sejours) / nrow(data), digits=2)
    return(age_table)
  }
  
  GHM_lettre_by <- function(data){
    data <- data[!is.na(data$GHM_lettre),]
    
    ghm_lettre_table <- data.frame(table(data$GHM_lettre, dnn="Lettre GHM"))
    ghm_lettre_table$`%` <- round(
      (100 * ghm_lettre_table$Freq) / nrow(data), digits=2
    )
    return(ghm_lettre_table[order(ghm_lettre_table$Freq, decreasing=TRUE),])
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
    URM_output <- (
      URM_output[
        order(URM_output$Freq, decreasing=TRUE),
        c("URM.origine", "libelle", "Freq")
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
  condition_table <- reactive({
    req(data_by_condition())
    n_by_condition <- table_by(data_by_condition(), "diagnoses")
    n_by_condition <- merge(
      by_lists$diag_table, n_by_condition,
      # diag_table(), n_by_condition, 
      by.x="code", by.y="row.names",
      all.x=FALSE, all.y=TRUE
    )
    return(n_by_condition)
  })
  acts_table <- reactive({
    req(data_by_acts())
    n_by_act <- table_by(data_by_acts(), "acts")
    if (!is.null(input$acts_file)){
      inter_list <- load_acts()
      n_by_act <- merge(
        inter_list, n_by_act,
        by.x="code", by.y="row.names",
        all.x=FALSE, all.y=TRUE
      )
    } else {
      n_by_act <- cbind(row.names(n_by_act), n_by_act)
      colnames(n_by_act)[1] <- "code"
    }
    return(n_by_act)
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
  age_plot <- reactive({
    req(data())
    plot_age_by(data())
  })
  age_plot_by_condition <- reactive({
    req(data_by_condition())
    plot_age_by(data_by_condition())
  })
  age_plot_by_acts <- reactive({
    req(data_by_acts())
    plot_age_by(data_by_acts())
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
  URM_origine_table <- reactive({
    req(etablissement())
    req(data())
    URM_origine_by(data())
  })
  URM_origine_table_by_condition <- reactive({
    req(etablissement())
    req(data_by_condition())
    URM_origine_by(data_by_condition())
  })
  URM_origine_table_by_acts <- reactive({
    req(etablissement())
    req(data_by_acts())
    URM_origine_by(data_by_acts())
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
  
  ##################################
  ### RENDERING TABLES AND PLOTS ###
  ##################################
  
  output$condition_table <- renderDT({
    req(condition_table())
    datatable(
      condition_table(),
      autoHideNavigation=TRUE,
      width = "auto",
      rownames = FALSE
    )
  })
  output$acts_table <- renderDT({
    req(acts_table)
    datatable(
      acts_table(),
      autoHideNavigation=TRUE,
      width = "auto",
      rownames = FALSE
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
  output$age_histogram <- renderPlot({
    req(age_plot())
    age_plot()
  })
  output$age_histogram_by_condition <- renderPlot({
    req(age_plot_by_condition())
    age_plot_by_condition()
  })
  output$age_histogram_by_acts <- renderPlot({
    req(age_plot_by_acts())
    age_plot_by_acts()
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
  output$GHM_lettre_table <- renderDT({
    req(GHM_lettre_table())
    datatable(
      GHM_lettre_table(),
      rownames=FALSE
    )
  })
  output$GHM_lettre_table_by_condition <- renderDT({
    req(GHM_lettre_by_condition())
    datatable(
      GHM_lettre_by_condition(),
      rownames=FALSE
    )
  })
  output$GHM_lettre_table_by_acts <- renderDT({
    req(GHM_lettre_by_acts())
    datatable(
      GHM_lettre_by_acts(),
      rownames=FALSE
    )
  })
  output$URM_origine_table <- renderDT({
    req(URM_origine_table())
    datatable(
      URM_origine_table(),
      rownames=FALSE,
      options = list (
        pageLength=5,
        paging=TRUE,
        scrollY=FALSE,
        dom = "tp"
      )
    )
  })
  output$URM_origine_table_by_condition <- renderDT({
    req(URM_origine_by_condition())
    datatable(
      URM_origine_by_condition(),
      rownames=FALSE,
      options = list (
        pageLength=5,
        paging=TRUE,
        scrollY=FALSE,
        dom = "tp"
      )
    )
  })
  output$URM_origine_table_by_acts <- renderDT({
    req(URM_origine_by_acts())
    datatable(
      URM_origine_by_acts(),
      rownames=FALSE,
      options = list (
        pageLength=5,
        paging=TRUE,
        scrollY=FALSE,
        dom = "tp"
      )
    )
  })
  
  ##########################
  ### GENERATING REPORTS ###
  ##########################
  
  observeEvent(URM_origine_table(), {
    output$download_report <- renderUI({
      div(
        style="display:inline-block;text-align: center;width: 100%;",
        downloadButton("report", "Rapport global")
      )
    })
  })
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
        GHM_num_list=input$GHM_num_filter,
        GHM_lettre_list=input$GHM_lettre_filter,
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
          rownames = FALSE,
          options=list(dom="tp")
        ),
        GHM_lettre_table=datatable(
          data=GHM_lettre_table(),
          style="bootstrap",
          rownames = FALSE,
          options=list(dom="tp")
        ),
        age_plot=age_plot()
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
  
  observeEvent(condition_table(), {
    output$download_report_diags <- renderUI({
      div(
        style="display:inline-block;text-align: center;width: 100%;",
        downloadButton("report_diags", "Rapport par diagnostiques")
      )
    })
  })
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
        GHM_num_list=input$GHM_num_filter,
        GHM_lettre_list=input$GHM_lettre_filter,
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
          rownames = FALSE,
          options=list(dom="tp", searching=TRUE)
        ),
        URM_origine_table=datatable(
          data=URM_origine_table_by_condition(),
          style="bootstrap",
          rownames = FALSE,
          options=list(dom="tp")
        ),
        GHM_lettre_table=datatable(
          data=GHM_lettre_table_by_condition(),
          style="bootstrap",
          rownames = FALSE,
          options=list(dom="tp")
        ),
        age_plot=age_plot_by_condition()
      )
      rmarkdown::render(
        tempReport, output_file = file,
        params = params,
        envir = new.env(parent = globalenv()),
        encoding = "UTF-8"
      )
    }
  )
  
  observeEvent(acts_table(), {
    output$download_report_acts <- renderUI({
      div(
        style="display:inline-block;text-align: center;width: 100%;",
        downloadButton("report_acts", "Rapport par actes")
      )
    })
  })
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
        GHM_num_list=input$GHM_num_filter,
        GHM_lettre_list=input$GHM_lettre_filter,
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
          options=list(dom="tp", searching=TRUE)
        ),
        categorie_table=categorie_table(),
        URM_origine_table=datatable(
          data=URM_origine_table_by_acts(),
          style="bootstrap",
          rownames = FALSE,
          options=list(dom="tp")
        ),
        GHM_lettre_table=datatable(
          data=GHM_lettre_table_by_acts(),
          style="bootstrap",
          rownames = FALSE,
          options=list(dom="tp")
        ),
        age_plot=age_plot_by_acts()
      )
      rmarkdown::render(
        tempReport, output_file = file,
        params = params,
        envir = new.env(parent = globalenv()),
        encoding = "UTF-8"
      )
    }
  )
}

shinyApp(ui, server)