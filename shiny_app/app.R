library(shiny)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthhires)
library(patchwork)
library(DT)
library(readr)
library(ggdist)

# --- 0. Precompute world maps ----
world_for_join <- ne_countries(scale="large", returnclass="sf") %>%
  filter(!grepl("Antarctica", name)) %>%
  sf::st_make_valid()
world_plot <- ne_countries(scale="small", returnclass="sf") %>%
  filter(!grepl("Antarctica", name))

# --- Helper to preprocess either the cleaned or curated dataset ----
preprocess_data <- function(df) {
  df2 <- df %>%
    dplyr::mutate(
      metric_group = dplyr::case_when(
        metric %in% c("CTmin","LT50_cold") ~ "LTL",
        metric %in% c("CTmax","LT50_hot")  ~ "UTL",
        metric == "Tpref"                  ~ "PBT",
        TRUE                               ~ NA_character_
      ),
      life_stage_tested = factor(
        tolower(life_stage_tested),
        levels = c("larvae","juveniles","metamorphs","adults")
      ),
      .row = dplyr::row_number()
    )
  coords_sf2 <- df2 %>%
    filter(!is.na(longitude) & !is.na(latitude)) %>%
    st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
    st_join(world_for_join["name_long"], left = TRUE) %>%
    st_drop_geometry() %>%
    dplyr::select(.row, country = name_long)
  df2 %>%
    dplyr::left_join(coords_sf2, by = ".row") %>%
    dplyr::select(-.row)
}

# --- 1. Load both CSVs and preprocess them ----
raw_uncurated <- read_csv("data/Cleaned_data_Amphitherm.csv")
raw_curated   <- read_csv("data/Cleaned_and_curated_data_Amphitherm.csv")
data_uncurated <- preprocess_data(raw_uncurated)
data_curated   <- preprocess_data(raw_curated)

# --- 2. UI ----
ui <- navbarPage(
  title = span("AmphiTherm Explorer", style="font-weight:bold; font-size:105%; color:black;"),
  header = tags$head(
    tags$style(HTML("
      .navbar-nav > li > a { font-weight: bold !important; font-size: 115% !important; color: black !important; }
      .dataTable thead th { font-size: 110% !important; }
      .dataTable td { padding: 4px 8px !important; white-space: nowrap !important; overflow: hidden !important; text-overflow: ellipsis !important; }
    "))
  ),
  
  tabPanel("Data summary",
           sidebarLayout(
             sidebarPanel(width = 3,
                          radioButtons("curated", "Curated database", choices = c("No","Yes"), selected = "Yes"),
                          tags$hr(),
                          selectInput("order",      "Order",      choices = c("All", sort(unique(data_uncurated$order))), selected = "All", multiple = TRUE),
                          selectInput("family",     "Family",     choices = "All", selected = "All", multiple = TRUE),
                          selectInput("species",    "Species",    choices = "All", selected = "All", multiple = TRUE),
                          selectInput("metric",     "Metric",     choices = c("All","UTL","LTL","PBT"), selected = "All", multiple = TRUE),
                          selectInput("country",    "Country",    choices = "All", selected = "All", multiple = TRUE),
                          selectInput("life_stage", "Life stage",
                                      choices = c("All", as.character(levels(data_uncurated$life_stage_tested)) ),
                                      selected = "All", multiple = TRUE)
             ),
             mainPanel(
               plotOutput("mapIucn", height = "550px"),
               fluidRow(
                 column(6, plotOutput("jitterMetric",    height = "400px")),
                 column(6, plotOutput("jitterLifeStage", height = "400px"))
               ),
               tags$div(style = "height:40px;"),
               plotOutput("iucnPlot", height = "220px"),
               tags$div(style = "height:40px;"),
               DTOutput("summaryTableDT")
             )
           )
  ),
  
  tabPanel("Database",
           downloadButton("downloadTop", "Download filtered data"),
           tags$div(style = "height:10px;"),
           p("Only a subset of the columns are displayed here. Nevertheless, you can download the entire filtered dataset by clicking on 'Download filtered data'."),
           DTOutput("table"),
           tags$div(style = "height:10px;"),
           downloadButton("downloadBottom", "Download filtered data")
  )
)

# --- 3. Server ----
server <- function(input, output, session) {
  
  baseData <- reactive({
    if (input$curated == "Yes") data_curated else data_uncurated
  })
  
  # ---------- helpers ----------
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # When All is present with others, decide if user added All or added real values
  make_all_handler <- function(id) {
    prev <- reactiveVal("All")
    observeEvent(input[[id]], {
      v <- input[[id]] %||% "All"
      p <- prev()
      
      if ("All" %in% v && length(v) > 1) {
        added <- setdiff(v, p)
        if ("All" %in% added) {
          # user clicked All -> keep only All
          freezeReactiveValue(input, id)
          updateSelectInput(session, id, selected = "All")
          prev("All")
          return(invisible(NULL))
        } else {
          # user added specific values while All was there -> drop All
          v2 <- setdiff(v, "All")
          freezeReactiveValue(input, id)
          updateSelectInput(session, id, selected = v2)
          prev(v2)
          return(invisible(NULL))
        }
      }
      prev(v)
    }, ignoreInit = TRUE)
    prev
  }
  
  prev_order      <- make_all_handler("order")
  prev_family     <- make_all_handler("family")
  prev_species    <- make_all_handler("species")
  prev_metric     <- make_all_handler("metric")
  prev_country    <- make_all_handler("country")
  prev_life_stage <- make_all_handler("life_stage")
  
  # expand All using valid choices computed upstream
  expand_all <- function(sel, all_vals) {
    if (is.null(sel) || identical(sel, "All") || (length(sel) == 1 && sel[1] == "All")) {
      all_vals
    } else sel
  }
  
  # keep current selection if still valid; otherwise "All"
  keep_or_all <- function(sel, valid_no_all) {
    if (is.null(sel) || identical(sel, "All") || (length(sel) == 1 && sel[1] == "All")) {
      "All"
    } else {
      x <- intersect(sel, valid_no_all)
      if (length(x)) x else "All"
    }
  }
  
  # ---------- initial population (show all options at load) ----------
  observeEvent(baseData(), {
    df <- baseData()
    
    freezeReactiveValue(input, "order")
    updateSelectInput(session, "order",
                      choices  = c("All", sort(unique(df$order))),
                      selected = "All")
    
    freezeReactiveValue(input, "family")
    updateSelectInput(session, "family",
                      choices  = c("All", sort(unique(df$family))),
                      selected = "All")
    
    freezeReactiveValue(input, "species")
    updateSelectInput(session, "species",
                      choices  = c("All", sort(unique(df$species))),
                      selected = "All")
    
    freezeReactiveValue(input, "metric")
    updateSelectInput(session, "metric",
                      choices  = c("All","UTL","LTL","PBT"),
                      selected = "All")
    
    freezeReactiveValue(input, "country")
    updateSelectInput(session, "country",
                      choices  = c("All", sort(unique(df$country))),
                      selected = "All")
    
    freezeReactiveValue(input, "life_stage")
    updateSelectInput(session, "life_stage",
                      choices  = c("All", as.character(levels(df$life_stage_tested))),
                      selected = "All")
    
    # reset "previous" trackers
    prev_order("All"); prev_family("All"); prev_species("All")
    prev_metric("All"); prev_country("All"); prev_life_stage("All")
  }, priority = 100)
  
  # ---------- cascading choices (downstream only) ----------
  # Order -> Family
  observeEvent(list(input$order, baseData()), {
    df <- baseData()
    ord_valid <- sort(unique(df$order))
    df1 <- df %>% filter(order %in% expand_all(input$order, ord_valid))
    fam_valid <- sort(unique(df1$family))
    
    new_sel <- keep_or_all(isolate(input$family), fam_valid)
    freezeReactiveValue(input, "family")
    updateSelectInput(session, "family",
                      choices  = c("All", fam_valid),
                      selected = new_sel)
    prev_family(new_sel)
  }, ignoreInit = TRUE)
  
  # Order + Family -> Species
  observeEvent(list(input$order, input$family, baseData()), {
    df <- baseData()
    ord_valid <- sort(unique(df$order))
    df1 <- df %>% filter(order  %in% expand_all(input$order,  ord_valid))
    fam_valid <- sort(unique(df1$family))
    df2 <- df1 %>% filter(family %in% expand_all(input$family, fam_valid))
    sp_valid  <- sort(unique(df2$species))
    
    new_sel <- keep_or_all(isolate(input$species), sp_valid)
    freezeReactiveValue(input, "species")
    updateSelectInput(session, "species",
                      choices  = c("All", sp_valid),
                      selected = new_sel)
    prev_species(new_sel)
  }, ignoreInit = TRUE)
  
  # Order + Family + Species -> Country
  observeEvent(list(input$order, input$family, input$species, baseData()), {
    df <- baseData()
    ord_valid <- sort(unique(df$order))
    df1 <- df %>% filter(order   %in% expand_all(input$order,   ord_valid))
    fam_valid <- sort(unique(df1$family))
    df2 <- df1 %>% filter(family  %in% expand_all(input$family,  fam_valid))
    sp_valid  <- sort(unique(df2$species))
    df3 <- df2 %>% filter(species %in% expand_all(input$species, sp_valid))
    ctry_valid <- sort(unique(df3$country))
    
    new_sel <- keep_or_all(isolate(input$country), ctry_valid)
    freezeReactiveValue(input, "country")
    updateSelectInput(session, "country",
                      choices  = c("All", ctry_valid),
                      selected = new_sel)
    prev_country(new_sel)
  }, ignoreInit = TRUE)
  
  # Order + Family + Species + Country -> Life stage
  observeEvent(list(input$order, input$family, input$species, input$country, baseData()), {
    df <- baseData()
    ord_valid <- sort(unique(df$order))
    df1 <- df %>% filter(order   %in% expand_all(input$order,   ord_valid))
    fam_valid <- sort(unique(df1$family))
    df2 <- df1 %>% filter(family  %in% expand_all(input$family,  fam_valid))
    sp_valid  <- sort(unique(df2$species))
    df3 <- df2 %>% filter(species %in% expand_all(input$species, sp_valid))
    ctry_valid <- sort(unique(df3$country))
    df4 <- df3 %>% filter(country %in% expand_all(input$country, ctry_valid))
    stage_valid <- sort(unique(as.character(df4$life_stage_tested)))
    
    new_sel <- keep_or_all(as.character(isolate(input$life_stage)), stage_valid)
    freezeReactiveValue(input, "life_stage")
    updateSelectInput(session, "life_stage",
                      choices  = c("All", stage_valid),
                      selected = new_sel)
    prev_life_stage(new_sel)
  }, ignoreInit = TRUE)
  
  # ---------- filtered data (uses upstream expansion) ----------
  filteredData <- reactive({
    df <- baseData()
    
    # upstream expansion sets for each level
    ord_set   <- expand_all(input$order,   sort(unique(df$order)))
    df1 <- df %>% filter(order %in% ord_set)
    
    fam_set   <- expand_all(input$family,  sort(unique(df1$family)))
    df2 <- df1 %>% filter(family %in% fam_set)
    
    sp_set    <- expand_all(input$species, sort(unique(df2$species)))
    df3 <- df2 %>% filter(species %in% sp_set)
    
    met_set   <- expand_all(input$metric,  c("UTL","LTL","PBT"))
    df4 <- df3 %>% filter(metric_group %in% met_set)
    
    ctry_set  <- expand_all(input$country, sort(unique(df4$country)))
    df5 <- df4 %>% filter(country %in% ctry_set)
    
    stage_set <- expand_all(as.character(input$life_stage),
                            sort(unique(as.character(df5$life_stage_tested))))
    df6 <- df5 %>% filter(as.character(life_stage_tested) %in% stage_set)
    
    df6
  })
  
  # ---------- table / downloads ----------
  output$table <- renderDT({
    cols <- c(
      "ref","title","pub_year","peer_reviewed","doi","language",
      "order","family","species","IUCN_status","origin","latitude","longitude",
      "elevation","acclimated","life_stage_acclimated","acclimation_temp",
      "acclimation_time","life_stage_tested","SVL","body_mass","sex","metric",
      "endpoint","ramping","set_time","duration_measurement","rate_measurement",
      "gradient_low_temp","gradient_high_temp","humidity","oxygen","salinity",
      "pH","photoperiod","chemical","hormone","infected","pathogen","flag",
      "mean_trait","error_trait","n_trait","error_type"
    )
    datatable(
      filteredData()[, intersect(cols, names(filteredData()))],
      rownames = FALSE,
      class    = 'compact stripe',
      options  = list(
        scrollX   = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      )
    )
  })
  
  makeDownloader <- function() {
    downloadHandler(
      filename = function() "AmphiTherm_filtered.csv",
      content  = function(file) write.csv(filteredData(), file, row.names = FALSE)
    )
  }
  output$downloadTop    <- makeDownloader()
  output$downloadBottom <- makeDownloader()
  
  output$summaryTableDT <- renderDT({
    df <- filteredData() %>%
      group_by(metric_group) %>%
      summarize(
        studies   = n_distinct(ref),
        species   = n_distinct(species),
        estimates = n(), .groups = "drop"
      ) %>%
      rename(
        Metric        = metric_group,
        `# studies`   = studies,
        `# species`   = species,
        `# estimates` = estimates
      ) %>%
      mutate(Metric = recode(Metric,
                             LTL="Lower thermal limits (LTL)",
                             PBT="Preferred body temperatures (PBT)",
                             UTL="Upper thermal limits (UTL)"
      ))
    datatable(df, rownames = FALSE, class='compact stripe', options = list(dom = 't'))
  })
  
  # mean±SD helper (draw only if n > 1 in that group)
  mean_sd_df <- function(y) {
    m <- mean(y, na.rm=TRUE); s <- sd(y, na.rm=TRUE)
    data.frame(y = m, ymin = m - s, ymax = m + s)
  }
  
  # --- MAP
  output$mapIucn <- renderPlot({
    dfm <- filteredData()
    ggplot() +
      annotate("rect",
               xmin = -180, xmax = 180,
               ymin = -23.43663, ymax = 23.43663,
               fill = "lightgray", alpha = 0.3
      ) +
      geom_hline(yintercept = c(-23.43663, 0, 23.43663),
                 colour = "gray", linetype = "dashed", linewidth = 0.5) +
      geom_sf(data = world_plot, fill = "gray75", col = NA) +
      coord_sf(xlim = c(-180, 180), ylim = c(-55, 80), expand = FALSE) +
      geom_point(
        data = dfm,
        aes(x = longitude, y = latitude, fill = metric_group),
        shape = 21, colour = "black", alpha = 0.7, size = 4
      ) +
      scale_fill_manual(
        name   = "Metric",
        values = c(LTL = "#2a9d8f", PBT = "#ffb703", UTL = "#e63946"),
        guide  = guide_legend(title.position = "top", title.hjust = 0,
                              override.aes = list(shape = 21, size = 5))
      ) +
      annotate("text",
               x = -175, y = -23.43663 + 2.0, label = "Tropics",
               hjust = 0, vjust = 0, size = 6, colour = "grey30"
      ) +
      labs(title = "Map of sampling locations", x = "Longitude", y = "Latitude") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title      = element_text(hjust = 0, face = "bold", size = 22, colour = "black"),
        legend.position = "right",
        legend.direction= "vertical",
        legend.key.size = unit(1.4, "lines"),
        legend.title    = element_text(size = 20, face = "bold", colour = "black"),
        legend.text     = element_text(size = 15, colour = "black"),
        axis.title      = element_text(size = 18, colour = "black"),
        axis.text       = element_text(size = 16, colour = "black"),
        panel.border    = element_rect(fill = NA, linewidth = 2)
      ) +
      annotation_north_arrow(
        style  = north_arrow_nautical(text_size = 14, fill = c("grey20","white")),
        height = unit(1.8, "cm"), width = unit(1.8, "cm")
      )
  })
  
  # --- IUCN stacked bar
  output$iucnPlot <- renderPlot({
    dfm <- filteredData()
    iucn_df <- dfm %>%
      filter(!is.na(IUCN_status), IUCN_status!="EX") %>%
      distinct(species, .keep_all=TRUE) %>%
      mutate(IUCN_status = factor(IUCN_status,
                                  levels=c("DD","LC","NT","VU","EN","CR")))
    ggplot(iucn_df, aes(x=1, fill=IUCN_status)) +
      geom_bar(width=0.15, position = position_stack(reverse = TRUE)) +
      geom_text(stat="count",
                aes(label=..count..),
                position = position_stack(vjust=0.5, reverse = TRUE),
                color="black", size=4.5) +
      coord_flip(expand=FALSE) +
      scale_fill_manual(
        name   = "IUCN status",
        limits = c("DD","LC","NT","VU","EN","CR"),
        breaks = c("DD","LC","NT","VU","EN","CR"),
        values = c(
          "CR"="#c1121f","EN"="#e36414","VU"="#ffc300",
          "NT"="#936639","LC"="#52b788","DD"="#B0B0B0"),
        guide=guide_legend(nrow=1, byrow=TRUE, title.position="top", title.hjust=0)
      ) +
      labs(title="Threat status distribution", caption="Number of species inside bars") +
      theme_classic(base_size=14) +
      theme(
        plot.title      = element_text(hjust=0, face="bold", size=22, color="black"),
        plot.caption    = element_text(size=14, color="black", hjust=0.01),
        legend.position = "bottom",
        legend.justification = "left",
        legend.key.size = unit(1.4,"lines"),
        legend.title    = element_text(size=18, face="bold", color="black"),
        legend.text     = element_text(size=14, color="black"),
        axis.title      = element_blank(),
        axis.text       = element_blank(),
        axis.ticks      = element_blank(),
        panel.border    = element_rect(fill=NA, linewidth=2)
      ) +
      ylab("Number of species")
  })
  
  # --- Jitter plots (mean±SD only when n > 1) ---
  output$jitterMetric <- renderPlot({
    dfm <- filteredData()
    ggplot(dfm, aes(x=metric_group, y=mean_trait, fill=metric_group)) +
      geom_point(shape=21, color="black", alpha=1, size=3,
                 position=position_jitter(width=0.15)) +
      stat_summary(
        aes(color = metric_group),
        fun.data = function(y) if (sum(!is.na(y)) > 1) mean_sd_df(y) else NULL,
        geom = "pointrange",
        size = 1.2,
        position = position_nudge(x = 0.3),
        show.legend = FALSE
      ) +
      scale_fill_manual(name = "Metric",
                        values=c("LTL"="#2a9d8f","PBT"="#ffb703","UTL"="#e63946")) +
      scale_color_manual(values=c("LTL"="#2a9d8f","PBT"="#ffb703","UTL"="#e63946")) +
      labs(title="Variation across thermal traits",
           x=NULL, y="Thermal tolerance or preference (°C)",
           caption="Mean ± SD beside individual estimates") +
      theme_minimal(base_size=14) +
      theme(
        plot.title   = element_text(hjust=0, face="bold", size=22, color="black"),
        plot.caption = element_text(size=12, color="black"),
        axis.title   = element_text(size=15, color="black"),
        axis.text    = element_text(size=15, color="black"),
        panel.border = element_rect(fill=NA, linewidth=2),
        legend.position = c(0.98, 0.02),
        legend.justification = c("right","bottom"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(size=14, face="bold"),
        legend.text  = element_text(size=13)
      )
  })
  
  output$jitterLifeStage <- renderPlot({
    dfm <- filteredData() %>% filter(!is.na(life_stage_tested))
    ggplot(dfm, aes(x=life_stage_tested, y=mean_trait, fill=metric_group)) +
      geom_point(shape=21, color="black", alpha=1, size=3,
                 position=position_jitter(width=0.15)) +
      stat_summary(
        aes(color = metric_group),
        fun.data = function(y) if (sum(!is.na(y)) > 1) mean_sd_df(y) else NULL,
        geom = "pointrange",
        size = 1.2,
        position = position_nudge(x = 0.3),
        show.legend = FALSE
      ) +
      scale_fill_manual(name = "Metric",
                        values=c("LTL"="#2a9d8f","PBT"="#ffb703","UTL"="#e63946"),
                        guide = guide_legend(nrow = 1, byrow = TRUE, title.position="top")) +
      scale_color_manual(values=c("LTL"="#2a9d8f","PBT"="#ffb703","UTL"="#e63946"),
                         guide = "none") +
      labs(title="Variation across life stages",
           x=NULL, y="Thermal tolerance or preference (°C)",
           caption="Mean ± SD beside individual estimates") +
      theme_minimal(base_size=14) +
      theme(
        plot.title   = element_text(hjust=0, face="bold", size=22, color="black"),
        plot.caption = element_text(size=12, color="black"),
        axis.title   = element_text(size=15, color="black"),
        axis.text    = element_text(size=15, color="black"),
        panel.border = element_rect(fill=NA, linewidth=2),
        legend.position = "none"
      )
  })
}

# --- 4. Run the App ----
shinyApp(ui, server)


# --- 5. Deploy Shiny App ----
# dr <- getwd()
# rsconnect::deployApp(
#   appDir         = paste(dr, "/shiny_app", sep=""),
#   appPrimaryDoc  = "app.R",
#   appName        = "AmphiTherm-Explorer"
# )
