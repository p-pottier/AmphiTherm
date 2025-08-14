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

# --- Helper to preprocess either CSV ----
preprocess_data <- function(df) {
  df2 <- df %>%
    dplyr::mutate(
      metric_group = dplyr::case_when(
        metric %in% c("CTmin","LT50_cold") ~ "LTL",
        metric %in% c("CTmax","LT50_hot")  ~ "UTL",
        metric == "Tpref"                 ~ "PBT",
        TRUE                              ~ NA_character_
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
                          selectInput("order",      "Order",      choices = c("All", sort(unique(data_uncurated$order))), selected = "All"),
                          selectInput("family",     "Family",     choices = "All", selected = "All"),
                          selectInput("species",    "Species",    choices = "All", selected = "All"),
                          selectInput("metric",     "Metric",     choices = c("All","UTL","LTL","PBT"), selected = "All"),
                          selectInput("country",    "Country",    choices = "All", selected = "All"),
                          selectInput("life_stage", "Life stage", choices = c("All", levels(data_uncurated$life_stage_tested)), selected = "All")
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
  
  # cascading filters
  observe({
    df1 <- baseData()
    if (input$order != "All") df1 <- df1 %>% filter(order == input$order)
    fams <- sort(unique(df1$family))
    updateSelectInput(session, "family",
                      choices  = c("All", fams),
                      selected = if (input$family %in% fams) input$family else "All"
    )
  })
  observe({
    df2 <- baseData()
    if (input$order  != "All") df2 <- df2 %>% filter(order == input$order)
    if (input$family != "All") df2 <- df2 %>% filter(family == input$family)
    specs <- sort(unique(df2$species))
    updateSelectInput(session, "species",
                      choices  = c("All", specs),
                      selected = if (input$species %in% specs) input$species else "All"
    )
  })
  observe({
    df3 <- baseData()
    if (input$order   != "All") df3 <- df3 %>% filter(order == input$order)
    if (input$family  != "All") df3 <- df3 %>% filter(family == input$family)
    if (input$species != "All") df3 <- df3 %>% filter(species == input$species)
    ctrs <- sort(unique(df3$country))
    updateSelectInput(session, "country",
                      choices  = c("All", ctrs),
                      selected = if (input$country %in% ctrs) input$country else "All"
    )
  })
  observe({
    df4 <- baseData()
    if (input$order   != "All") df4 <- df4 %>% filter(order == input$order)
    if (input$family  != "All") df4 <- df4 %>% filter(family == input$family)
    if (input$species != "All") df4 <- df4 %>% filter(species == input$species)
    if (input$country != "All") df4 <- df4 %>% filter(country == input$country)
    lss4 <- sort(unique(df4$life_stage_tested))
    updateSelectInput(session, "life_stage",
                      choices  = c("All", lss4),
                      selected = if (input$life_stage %in% lss4) input$life_stage else "All"
    )
  })
  
  filteredData <- reactive({
    df <- baseData()
    if (input$order      != "All") df <- df %>% filter(order == input$order)
    if (input$family     != "All") df <- df %>% filter(family == input$family)
    if (input$species    != "All") df <- df %>% filter(species == input$species)
    if (input$metric     != "All") df <- df %>% filter(metric_group == input$metric)
    if (input$country    != "All") df <- df %>% filter(country == input$country)
    if (input$life_stage != "All") df <- df %>% filter(life_stage_tested == input$life_stage)
    df
  })
  
  # Database display: subset columns
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
                             UTL="Upper thermal limits (UTL)"))
    datatable(
      df,
      rownames = FALSE,
      class    = 'compact stripe',
      options  = list(dom = 't')
    )
  })
  
  mean_sd_df <- function(x) {
    m <- mean(x, na.rm=TRUE); s <- sd(x, na.rm=TRUE)
    data.frame(y=m, ymin=m-s, ymax=m+s)
  }
  
  # --- MAP
  output$mapIucn <- renderPlot({
    dfm <- filteredData()
    
    ggplot() +
      # Tropics band across the map (no legend)
      annotate("rect",
               xmin = -180, xmax = 180,
               ymin = -23.43663, ymax = 23.43663,
               fill = "lightgray", alpha = 0.3
      ) +
      # Reference lines
      geom_hline(yintercept = c(-23.43663, 0, 23.43663),
                 colour = "gray", linetype = "dashed", linewidth = 0.5) +
      
      # Base map
      geom_sf(data = world_plot, fill = "gray75", col = NA) +
      coord_sf(xlim = c(-180, 180), ylim = c(-55, 80), expand = FALSE) +
      
      # Sampling points (as before)
      geom_point(
        data = dfm,
        aes(x = longitude, y = latitude, fill = metric_group),
        shape = 21, colour = "black", alpha = 0.7, size = 4
      ) +
      
      # Metric legend (unchanged, clean circular keys)
      scale_fill_manual(
        name   = "Metric",
        values = c(LTL = "#2a9d8f", PBT = "#ffb703", UTL = "#e63946"),
        guide  = guide_legend(title.position = "top", title.hjust = 0,
                              override.aes = list(shape = 21, size = 5))
      ) +
      
      # --- In-panel "Tropics" label (just text, no box) ---
      # Positioned just to the right and above the band’s bottom-left corner
      annotate("text",
               x = -175,                       # a little to the right of the left border
               y = -23.43663 + 2.0,           # slightly above the bottom edge of the band
               label = "Tropics",
               hjust = 0, vjust = 0,
               size = 6, colour = "grey30"
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
  
  # --- Jitter plots ---
  output$jitterMetric <- renderPlot({
    dfm <- filteredData()
    ggplot(dfm, aes(x=metric_group, y=mean_trait, fill=metric_group)) +
      geom_point(shape=21, color="black", alpha=1, size=3,
                 position=position_jitter(width=0.15)) +
      stat_summary(aes(color = metric_group),
                   fun.data = mean_sd_df,
                   geom = "pointrange",
                   size = 1.2,
                   position = position_nudge(x = 0.3),
                   show.legend = FALSE) +
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
      stat_summary(aes(color = metric_group),
                   fun.data = mean_sd_df,
                   geom = "pointrange",
                   size = 1.2,
                   position = position_nudge(x = 0.3),
                   show.legend = FALSE) +
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
