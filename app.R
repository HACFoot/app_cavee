library(shiny)
library(bslib)
library(reactable)
library(bsicons)
library(ggplot2)
library(dplyr)
library(lubridate)
library(data.table)
library(rsconnect)

# source("global.R")
# source("ui.R")
# source("server.R")

# rsconnect::writeManifest()

data_notes_sheet <- read.csv("data/data_notes_sheet.csv", row.names = NULL)
setDT(data_notes_sheet)
data_notes_sheet[, date := as.POSIXct(date, format = "%Y-%m-%d")]  # Convert `date` to Date object
data_notes_sheet[, date_opponent := paste0(format(date, "%Y-%m-%d"), " - ", opponent)]

# Months ----
month_separators <- data_notes_sheet %>%
  mutate(month = floor_date(date, "month")) %>%
  distinct(month) %>%
  pull(month)

# Minutes by category ----
category_tables <- lapply(unique(data_notes_sheet$category), function(cat) {
  # Filter data for the specific category
  cat_data <- data_notes_sheet[category == cat]
  
  # Ensure the date_opponent is ordered by date
  cat_data <- cat_data[order(date)]
  
  # Reshape data
  reshaped <- dcast(cat_data, player ~ date_opponent, value.var = "minutes", fun.aggregate = sum, fill = 0)
  
  return(reshaped)
})

# Assign names to the list based on categories
names(category_tables) <- unique(data_notes_sheet$category)

r1_minutes_table <- category_tables$`Réserve / Espoirs`
r1_minutes_table[, Total := rowSums(.SD), .SDcols = -1]
r1_minutes_table[r1_minutes_table == 0] <- NA
setnames(r1_minutes_table, "player", "Joueur")
setcolorder(r1_minutes_table, c("Joueur", "Total"))
U19_minutes_table <- category_tables$`U19 / Gambardella`
U19_minutes_table[, Total := rowSums(.SD), .SDcols = -1]
U19_minutes_table[U19_minutes_table == 0] <- NA
setnames(U19_minutes_table, "player", "Joueur")
setcolorder(U19_minutes_table, c("Joueur", "Total"))
U17_minutes_table <- category_tables$`U17`
U17_minutes_table[, Total := rowSums(.SD), .SDcols = -1]
U17_minutes_table[U17_minutes_table == 0] <- NA
setnames(U17_minutes_table, "player", "Joueur")
setcolorder(U17_minutes_table, c("Joueur", "Total"))
U16_minutes_table <- category_tables$`U16`
U16_minutes_table[, Total := rowSums(.SD), .SDcols = -1]
U16_minutes_table[U16_minutes_table == 0] <- NA
setnames(U16_minutes_table, "player", "Joueur")
setcolorder(U16_minutes_table, c("Joueur", "Total"))
U15_minutes_table <- category_tables$`U15`
U15_minutes_table[, Total := rowSums(.SD), .SDcols = -1]
U15_minutes_table[U15_minutes_table == 0] <- NA
setnames(U15_minutes_table, "player", "Joueur")
setcolorder(U15_minutes_table, c("Joueur", "Total"))

# Notes by category ----
category_tables <- lapply(unique(data_notes_sheet$category), function(cat) {
  # Filter data for the specific category
  cat_data <- data_notes_sheet[category == cat]
  
  # Ensure the date_opponent is ordered by date
  cat_data <- cat_data[order(date)]
  
  # Reshape data
  reshaped <- dcast(cat_data, player ~ date_opponent, value.var = "note", fun.aggregate = sum, fill = 0)
  
  return(reshaped)
})

# Assign names to the list based on categories
names(category_tables) <- unique(data_notes_sheet$category)

r1_notes_table <- category_tables$`Réserve / Espoirs`
r1_notes_table[r1_notes_table == 0] <- NA
r1_notes_table[, Moyenne := round(rowMeans(.SD, na.rm = TRUE), 2), .SDcols = -1]
# r1_notes_table[, Moyenne := round(rowMeans(do.call(cbind, lapply(.SD, function(x) ifelse(x > 0, x, NA))), na.rm = TRUE), 2), .SDcols = -1]
setnames(r1_notes_table, "player", "Joueur")
setcolorder(r1_notes_table, c("Joueur", "Moyenne"))
U19_notes_table <- category_tables$`U19 / Gambardella`
U19_notes_table[U19_notes_table == 0] <- NA
U19_notes_table[, Moyenne := round(rowMeans(.SD, na.rm = TRUE), 2), .SDcols = -1]
setnames(U19_notes_table, "player", "Joueur")
setcolorder(U19_notes_table, c("Joueur", "Moyenne"))
U17_notes_table <- category_tables$`U17`
U17_notes_table[U17_notes_table == 0] <- NA
U17_notes_table[, Moyenne := round(rowMeans(.SD, na.rm = TRUE), 2), .SDcols = -1]
setnames(U17_notes_table, "player", "Joueur")
setcolorder(U17_notes_table, c("Joueur", "Moyenne"))
U16_notes_table <- category_tables$`U16`
U16_notes_table[U16_notes_table == 0] <- NA
U16_notes_table[, Moyenne := round(rowMeans(.SD, na.rm = TRUE), 2), .SDcols = -1]
setnames(U16_notes_table, "player", "Joueur")
setcolorder(U16_notes_table, c("Joueur", "Moyenne"))
U15_notes_table <- category_tables$`U15`
U15_notes_table[U15_notes_table == 0] <- NA
U15_notes_table[, Moyenne := round(rowMeans(.SD, na.rm = TRUE), 2), .SDcols = -1]
setnames(U15_notes_table, "player", "Joueur")
setcolorder(U15_notes_table, c("Joueur", "Moyenne"))

# Functions ----
gradient_color <- function(value, min_value, max_value) {
  colors <- c(
    "#FFFFFF", "#FDFEFA", "#FCFEF5", "#FBFDF1", "#F9FDEC", "#F8FCE7", "#F7FCE3", "#F5FCDE", 
    "#F4FBDA", "#F3FBD5", "#F0FAD1", "#ECF9CE", "#E8F7CB", "#E4F6C8", "#E0F4C4", "#DBF2C1", 
    "#D7F1BE", "#D3EFBB", "#CFEEB8", "#CBECB5", "#C6EAB3", "#C0E8B2", "#BBE5B1", "#B5E3B0", 
    "#B0E0AF", "#AADDAE", "#A4DBAE", "#9FD8AD", "#99D6AC", "#94D3AB", "#8BD0AB", "#82CCAB", 
    "#78C9AB", "#6EC5AB", "#65C1AB", "#5BBEAB", "#51BAAB", "#48B7AB", "#3EB3AB", "#35B0AB"
  )
  
  normalized_value <- (value - min_value) / (max_value - min_value)
  color_index <- round(normalized_value * (length(colors) - 1)) + 1
  return(list(background = colors[color_index]))
}

apply_gradient <- function(column, data, decreasing = FALSE, width) {
  colDef(
    minWidth = 40,
    style = function(value) {
      if (decreasing) {
        # Reversed order: apply gradient based on decreasing values
        gradient_color(value, min(data[[column]], na.rm = TRUE), max(data[[column]], na.rm = TRUE))
      } else {
        # Default order: apply gradient based on increasing values
        gradient_color(value, max(data[[column]], na.rm = TRUE), min(data[[column]], na.rm = TRUE))
      }
    }
  )
}


# UI ----
ui <- page_navbar(
  selected = "Accueil",
  fluid = TRUE,
  underline = TRUE,
  window_title = "Dat'HAC - Cavée",
  title = "Dat'HAC - Cavée",
  
  # Custom CSS to style the navbar and dropdown font size
  header = tags$head(
    tags$style(HTML("
      /* Card header background for Home page */
      .home-card-header {
        background-color: #95C0E7 !important;
        color: #161B30;
      }
      
  .card-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
      
      /* Navbar text color */
      .nav-link, .navbar-brand {
        color: #161B30 !important;
      }
      
      /* Navbar hover and active states */
      .nav-link:hover, .nav-link.active {
        color: #161B30 !important;
      }
      
      /* Custom font size for dropdown menu items in bslib */
      .dropdown-menu .dropdown-item {
        font-size: 14px !important; /* Adjust size here */
        color: #161B30 !important;
      }
    "))
  ),
  
  # Home ----
  nav_panel(
    title = "Accueil", 
    icon = icon("home"), 
    href = "home"
  ),
  
  # Dashboards ----
  nav_panel(
    title = "Dashboards Joueurs", 
    icon = bs_icon("radar"), 
    href = "dashboards_joueurs",
    
    selectInput(inputId = "dashboard_players_list", label = NULL, choices = sort(unique(data_notes_sheet$player)), selected = NULL),
    layout_columns(
      height = "100px",
      fill = FALSE,
      reactableOutput("dashboard_player_stats")
    ),
    
    layout_columns(
      fill = FALSE,
      card(
        card_header("Temps de jeu"),
        plotOutput("dashboard_player_minutes")
      ),
      
      card(
        card_header("Notes"),
        plotOutput("dashboard_player_notes")
      ))
  ),
  
  # Notes ----
  nav_menu(
    title = "Notes", 
    icon = bs_icon("list-ol"), 
    
    ## Matches ----
    nav_panel(
      title = "Matches", 
      icon = icon("futbol"), 
      href = "matches_notes",
      
      layout_columns(
        fill = FALSE,
        height = "525px",
        card(
          div(class = "card-header",
              "Réserve",
              selectInput(inputId = "matches_r1", label = NULL, choices = sort(unique(data_notes_sheet[category == "Réserve / Espoirs",]$date_opponent), decreasing = TRUE), multiple = FALSE, selected = NULL)
          ),
          reactableOutput("matches_r1_notes")
        ),
        
        card(
          div(class = "card-header",
              "U19",
              selectInput(inputId = "matches_u19", label = NULL, choices = sort(unique(data_notes_sheet[category == "U19 / Gambardella",]$date_opponent), decreasing = TRUE), multiple = FALSE, selected = NULL)
          ),
          reactableOutput("matches_u19_notes")
        ),
        
        card(
          div(class = "card-header",
              "U17",
              selectInput(inputId = "matches_u17", label = NULL, choices = sort(unique(data_notes_sheet[category == "U17",]$date_opponent), decreasing = TRUE), multiple = FALSE, selected = NULL)
          ),
          reactableOutput("matches_u17_notes")
        )
      ),
      
      layout_columns(
        fill = FALSE,
        card(
          div(class = "card-header",
              "U16",
              selectInput(inputId = "matches_u16", label = NULL, choices = sort(unique(data_notes_sheet[category == "U16",]$date_opponent), decreasing = TRUE), multiple = FALSE, selected = NULL)
          ),
          reactableOutput("matches_u16_notes")
        ),
        
        card(
          div(class = "card-header",
              "U15",
              selectInput(inputId = "matches_u15", label = NULL, choices = sort(unique(data_notes_sheet[category == "U15",]$date_opponent), decreasing = TRUE), multiple = FALSE, selected = NULL)
          ),
          reactableOutput("matches_u15_notes")
        )
      )
    ),
    
    ## Par Catégorie ----
    nav_panel(
      title = "Par Catégorie", 
      icon = icon("medal"), 
      href = "notes",
      
      layout_columns(
        fill = FALSE,
        height = "350px", 
        card(
          card_header(
            tags$span("R1"),
            class = "home-card-header"
          ),
          reactableOutput("home_r1")
        ),
        card(
          card_header(
            tags$span("U19"),
            class = "home-card-header"
          ),
          reactableOutput("home_u19")
        ),
        card(
          card_header(
            tags$span("U17"),
            class = "home-card-header"
          ),
          reactableOutput("home_u17")
        )
      ),
      
      layout_columns(
        fill = TRUE,
        card(
          card_header(
            tags$span("U16"),
            class = "home-card-header"
          ),
          reactableOutput("home_u16")
        ),
        card(
          card_header(
            tags$span("U15"),
            class = "home-card-header"
          ),
          reactableOutput("home_u15")
        )
      )
      
    ),
    
    ## Par Joueur ----
    nav_panel("Par Joueur",
              href = "category_stats",
              icon = bs_icon("clipboard-data"),
              
              reactableOutput("all_players_table")
    ),
    
    ## Saison ----
    nav_panel("Saison",
              icon = bs_icon("graph-up"),
              layout_columns(
                fill = FALSE,
                col_widths = c(2, 2),
                selectInput(inputId = "season_matches_team_list", label = NULL, choices = c("Réserve / Espoirs", "U19 / Gambardella", "U17", "U16", "U15"), selected = "Réserve / Espoirs"),
                selectInput(inputId = "season_matches_table_choice_list", label = NULL, choices = c("Temps de jeu", "Notes"), selected = "Temps de jeu")
              ),
              
              reactableOutput("season_matches_table")
    )
  ),
  
  # Internationaux ----
  nav_panel(
    title = "Internationaux", 
    icon = icon("earth-europe"), 
    href = "internationaux"
  ),
  
  # Médical ----
  nav_panel(
    title = "Médical", 
    icon = icon("house-medical"), 
    href = "medical"
  ),
  
  # Scolarité ----
  nav_panel(
    title = "Scolarité", 
    icon = icon("graduation-cap"), 
    href = "scolarite"
  ),
  
  # Internat ----
  nav_panel(
    title = "Internat", 
    icon = icon("bed"), 
    href = "internat"
  ),
  
  # Centres de formation ----
  nav_panel(
    title = "Centres de formation", 
    icon = icon("school-flag"), 
    href = "centres_formation"
  )
  
)

# Server ----
server <- function(input, output, session) { 
  
  # Notes ----
  ## Matches ----
  ### R1 ----
  output$matches_r1_notes <- renderReactable({
    
    data <- data_notes_sheet[date_opponent == input$matches_r1 & category == "Réserve / Espoirs",
                             .(Joueur = player, Minutes = minutes, Note = note)]
    
    reactable(
      data,
      pagination = TRUE,
      sortable = FALSE,
      compact = TRUE,
      highlight = FALSE,
      borderless = TRUE,
      bordered = FALSE,
      outlined = FALSE,
      striped = FALSE,
      fullWidth = TRUE,
      searchable = FALSE,
      defaultPageSize = 20,
      theme = reactableTheme(
        style = list(
          fontFamily = "-apple-system, Helvetica Neue"
        ),
        searchInputStyle = list(width = "100%"),
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center", fontSize = "12px"),
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "a#555",
          border = "none",
          fontSize = "12px",
          fontWeight = "bold"
        )
      ),
      defaultColDef = colDef(
        align = "center",
        minWidth = 60
      ),
      columns = list(
        player = colDef(
          minWidth = 80
        ),
        Note = apply_gradient("Note", data, decreasing = TRUE, 50)
      )
    )
    
  })
  
  ### U19 ----
  output$matches_u19_notes <- renderReactable({
    
    data <- data_notes_sheet[date_opponent == input$matches_u19 & category == "U19 / Gambardella",
                             .(Joueur = player, Minutes = minutes, Note = note)]
    
    reactable(
      data,
      pagination = TRUE,
      sortable = FALSE,
      compact = TRUE,
      highlight = FALSE,
      borderless = TRUE,
      bordered = FALSE,
      outlined = FALSE,
      striped = FALSE,
      fullWidth = TRUE,
      searchable = FALSE,
      defaultPageSize = 20,
      theme = reactableTheme(
        style = list(
          fontFamily = "-apple-system, Helvetica Neue"
        ),
        searchInputStyle = list(width = "100%"),
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center", fontSize = "12px"),
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "a#555",
          border = "none",
          fontSize = "12px",
          fontWeight = "bold"
        )
      ),
      defaultColDef = colDef(
        align = "center",
        minWidth = 60
      ),
      columns = list(
        player = colDef(
          minWidth = 80
        ),
        Note = apply_gradient("Note", data, decreasing = TRUE, 50)
      )
    )
    
  })
  
  ### U17 ----
  output$matches_u17_notes <- renderReactable({
    
    data <- data_notes_sheet[date_opponent == input$matches_u17 & category == "U17",
                             .(Joueur = player, Minutes = minutes, Note = note)]
    
    reactable(
      data,
      pagination = TRUE,
      sortable = FALSE,
      compact = TRUE,
      highlight = FALSE,
      borderless = TRUE,
      bordered = FALSE,
      outlined = FALSE,
      striped = FALSE,
      fullWidth = TRUE,
      searchable = FALSE,
      defaultPageSize = 20,
      theme = reactableTheme(
        style = list(
          fontFamily = "-apple-system, Helvetica Neue"
        ),
        searchInputStyle = list(width = "100%"),
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center", fontSize = "12px"),
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "a#555",
          border = "none",
          fontSize = "12px",
          fontWeight = "bold"
        )
      ),
      defaultColDef = colDef(
        align = "center",
        minWidth = 60
      ),
      columns = list(
        player = colDef(
          minWidth = 80
        ),
        Note = apply_gradient("Note", data, decreasing = TRUE, 50)
      )
    )
    
  })
  
  ### U16 ----
  output$matches_u16_notes <- renderReactable({
    
    data <- data_notes_sheet[date_opponent == input$matches_u16 & category == "U16",
                             .(Joueur = player, Minutes = minutes, Note = note)]
    
    reactable(
      data,
      pagination = TRUE,
      sortable = FALSE,
      compact = TRUE,
      highlight = FALSE,
      borderless = TRUE,
      bordered = FALSE,
      outlined = FALSE,
      striped = FALSE,
      fullWidth = TRUE,
      searchable = FALSE,
      defaultPageSize = 20,
      theme = reactableTheme(
        style = list(
          fontFamily = "-apple-system, Helvetica Neue"
        ),
        searchInputStyle = list(width = "100%"),
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center", fontSize = "12px"),
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "a#555",
          border = "none",
          fontSize = "12px",
          fontWeight = "bold"
        )
      ),
      defaultColDef = colDef(
        align = "center",
        minWidth = 60
      ),
      columns = list(
        player = colDef(
          minWidth = 80
        ),
        Note = apply_gradient("Note", data, decreasing = TRUE, 50)
      )
    )
    
  })
  
  ### U15 ----
  output$matches_u15_notes <- renderReactable({
    
    data <- data_notes_sheet[date_opponent == input$matches_u15 & category == "U15",
                             .(Joueur = player, Minutes = minutes, Note = note)]
    
    reactable(
      data,
      pagination = TRUE,
      sortable = FALSE,
      compact = TRUE,
      highlight = FALSE,
      borderless = TRUE,
      bordered = FALSE,
      outlined = FALSE,
      striped = FALSE,
      fullWidth = TRUE,
      searchable = FALSE,
      defaultPageSize = 20,
      theme = reactableTheme(
        style = list(
          fontFamily = "-apple-system, Helvetica Neue"
        ),
        searchInputStyle = list(width = "100%"),
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center", fontSize = "12px"),
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "a#555",
          border = "none",
          fontSize = "12px",
          fontWeight = "bold"
        )
      ),
      defaultColDef = colDef(
        align = "center",
        minWidth = 60
      ),
      columns = list(
        player = colDef(
          minWidth = 80
        ),
        Note = apply_gradient("Note", data, decreasing = TRUE, 50)
      )
    )
    
  })
  
  ## Par catégorie ----
  ### R1 ----
  output$home_r1 <- renderReactable({
    
    data <- data_notes_sheet[category == "Réserve / Espoirs" & !is.na(note), .(Minutes = sum(minutes),
                                                                               `Note moy.` = round(mean(note, na.rm = T), 1)
    ), by = .(Joueur = player)][order(-`Note moy.`)]
    
    reactable(
      data,
      pagination = TRUE,
      sortable = FALSE,
      compact = TRUE,
      highlight = FALSE,
      borderless = TRUE,
      bordered = FALSE,
      outlined = FALSE,
      striped = FALSE,
      fullWidth = TRUE,
      searchable = FALSE,
      defaultPageSize = 50,
      theme = reactableTheme(
        style = list(
          fontFamily = "-apple-system, Helvetica Neue"
        ),
        searchInputStyle = list(width = "100%"),
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center", fontSize = "12px"),
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "a#555",
          border = "none",
          fontSize = "12px",
          fontWeight = "bold"
        )
      ),
      defaultColDef = colDef(
        align = "center",
        minWidth = 60
      ),
      columns = list(
        player = colDef(
          minWidth = 80
        ),
        `Note moy.` = apply_gradient("Note moy.", data, decreasing = TRUE, 50)
      )
    )
  })
  
  ### U19 ----
  output$home_u19 <- renderReactable({
    
    data <- data_notes_sheet[category == "U19 / Gambardella" & !is.na(note), .(Minutes = sum(minutes),
                                                                               `Note moy.` = round(mean(note, na.rm = T), 1)
    ), by = .(Joueur = player)][order(-`Note moy.`)]
    
    reactable(
      data,
      pagination = TRUE,
      sortable = FALSE,
      compact = TRUE,
      highlight = FALSE,
      borderless = TRUE,
      bordered = FALSE,
      outlined = FALSE,
      striped = FALSE,
      fullWidth = TRUE,
      searchable = FALSE,
      defaultPageSize = 50,
      theme = reactableTheme(
        style = list(
          fontFamily = "-apple-system, Helvetica Neue"
        ),
        searchInputStyle = list(width = "100%"),
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center", fontSize = "12px"),
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "a#555",
          border = "none",
          fontSize = "12px",
          fontWeight = "bold"
        )
      ),
      defaultColDef = colDef(
        align = "center",
        minWidth = 60
      ),
      columns = list(
        player = colDef(
          minWidth = 80
        ),
        `Note moy.` = apply_gradient("Note moy.", data, decreasing = TRUE, 50)
      )
    )
  })
  
  ### U17 ----
  output$home_u17 <- renderReactable({
    
    data <- data_notes_sheet[category == "U17" & !is.na(note), .(Minutes = sum(minutes),
                                                                 `Note moy.` = round(mean(note, na.rm = T), 1)
    ), by = .(Joueur = player)][order(-`Note moy.`)]
    
    reactable(
      data,
      pagination = TRUE,
      sortable = FALSE,
      compact = TRUE,
      highlight = FALSE,
      borderless = TRUE,
      bordered = FALSE,
      outlined = FALSE,
      striped = FALSE,
      fullWidth = TRUE,
      searchable = FALSE,
      defaultPageSize = 50,
      theme = reactableTheme(
        style = list(
          fontFamily = "-apple-system, Helvetica Neue"
        ),
        searchInputStyle = list(width = "100%"),
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center", fontSize = "12px"),
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "a#555",
          border = "none",
          fontSize = "12px",
          fontWeight = "bold"
        )
      ),
      defaultColDef = colDef(
        align = "center",
        minWidth = 60
      ),
      columns = list(
        player = colDef(
          minWidth = 80
        ),
        `Note moy.` = apply_gradient("Note moy.", data, decreasing = TRUE, 50)
      )
    )
  })
  
  ### U16 ----
  output$home_u16 <- renderReactable({
    
    data <- data_notes_sheet[category == "U16" & !is.na(note), .(Minutes = sum(minutes),
                                                                 `Note moy.` = round(mean(note, na.rm = T), 1)
    ), by = .(Joueur = player)][order(-`Note moy.`)]
    
    reactable(
      data,
      pagination = TRUE,
      sortable = FALSE,
      compact = TRUE,
      highlight = FALSE,
      borderless = TRUE,
      bordered = FALSE,
      outlined = FALSE,
      striped = FALSE,
      fullWidth = TRUE,
      searchable = FALSE,
      defaultPageSize = 50,
      theme = reactableTheme(
        style = list(
          fontFamily = "-apple-system, Helvetica Neue"
        ),
        searchInputStyle = list(width = "100%"),
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center", fontSize = "12px"),
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "a#555",
          border = "none",
          fontSize = "12px",
          fontWeight = "bold"
        )
      ),
      defaultColDef = colDef(
        align = "center",
        minWidth = 60
      ),
      columns = list(
        player = colDef(
          minWidth = 80
        ),
        `Note moy.` = apply_gradient("Note moy.", data, decreasing = TRUE, 50)
      )
    )
  })
  
  ### U15 ----
  output$home_u15 <- renderReactable({
    
    data <- data_notes_sheet[category == "U15" & !is.na(note), .(Minutes = sum(minutes),
                                                                 `Note moy.` = round(mean(note, na.rm = T), 1)
    ), by = .(Joueur = player)][order(-`Note moy.`)]
    
    reactable(
      data,
      pagination = TRUE,
      sortable = FALSE,
      compact = TRUE,
      highlight = FALSE,
      borderless = TRUE,
      bordered = FALSE,
      outlined = FALSE,
      striped = FALSE,
      fullWidth = TRUE,
      searchable = FALSE,
      defaultPageSize = 50,
      theme = reactableTheme(
        style = list(
          fontFamily = "-apple-system, Helvetica Neue"
        ),
        searchInputStyle = list(width = "100%"),
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center", fontSize = "12px"),
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "a#555",
          border = "none",
          fontSize = "12px",
          fontWeight = "bold"
        )
      ),
      defaultColDef = colDef(
        align = "center",
        minWidth = 60
      ),
      columns = list(
        player = colDef(
          minWidth = 80
        ),
        `Note moy.` = apply_gradient("Note moy.", data, decreasing = TRUE, 50)
      )
    )
  })
  
  ## Par joueur ----
  output$all_players_table <- renderReactable({
    
    data <- data_notes_sheet[, .(Minutes = sum(minutes, na.rm = T),
                                 R1 = round(mean(note[category == "Réserve / Espoirs"], na.rm = T), 2),
                                 U19 = round(mean(note[category == "U19 / Gambardella"], na.rm = T), 2),
                                 U17 = round(mean(note[category == "U17"], na.rm = T), 2),
                                 U16 = round(mean(note[category == "U16"], na.rm = T), 2),
                                 U15 = round(mean(note[category == "U15"], na.rm = T), 2)), by = .(Joueur = player)][order(Joueur)]
    
    reactable(
      data,
      pagination = FALSE,
      sortable = TRUE,
      compact = FALSE,
      highlight = TRUE,
      borderless = TRUE,
      bordered = FALSE,
      outlined = FALSE,
      striped = TRUE,
      fullWidth = TRUE,
      searchable = TRUE,
      theme = reactableTheme(
        style = list(
          fontFamily = "-apple-system, Helvetica Neue"
        ),
        searchInputStyle = list(width = "50%"),
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center", fontSize = "14px"),
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "#555",
          border = "none",
          fontSize = "12px",
          fontWeight = "bold"
        )
      ),
      defaultColDef = colDef(
        align = "center",
        minWidth = 60
      ),
      columns = list(
        Joueur = colDef(
          minWidth = 80
        ),
        R1 = apply_gradient("R1", data, decreasing = TRUE, 50),
        U19 = apply_gradient("U19", data, decreasing = TRUE, 50),
        U17 = apply_gradient("U17", data, decreasing = TRUE, 50),
        U16 = apply_gradient("U16", data, decreasing = TRUE, 50),
        U15 = apply_gradient("U15", data, decreasing = TRUE, 50)
      )
    )
    
  })
  
  ## Saison ----
  table_data_filtered <- reactive({
    
    if(input$season_matches_table_choice_list == "Temps de jeu"){
      if(input$season_matches_team_list == "Réserve / Espoirs"){
        r1_minutes_table
      } else if(input$season_matches_team_list == "U19 / Gambardella"){
        U19_minutes_table
      } else if(input$season_matches_team_list == "U17"){
        U17_minutes_table
      } else if(input$season_matches_team_list == "U16"){
        U16_minutes_table
      } else if(input$season_matches_team_list == "U15"){
        U15_minutes_table 
      }
    }else {
      if(input$season_matches_team_list == "Réserve / Espoirs"){
        r1_notes_table
      } else if(input$season_matches_team_list == "U19 / Gambardella"){
        U19_notes_table
      } else if(input$season_matches_team_list == "U17"){
        U17_notes_table
      } else if(input$season_matches_team_list == "U16"){
        U16_notes_table
      } else if(input$season_matches_team_list == "U15"){
        U15_notes_table 
      }
      
    }
    
  })
  
  output$season_matches_table <- renderReactable({
    
    reactable(
      table_data_filtered(),
      pagination = FALSE,
      sortable = TRUE,
      compact = FALSE,
      highlight = TRUE,
      borderless = TRUE,
      bordered = FALSE,
      outlined = FALSE,
      striped = TRUE,
      fullWidth = TRUE,
      searchable = FALSE,
      defaultPageSize = 50,
      theme = reactableTheme(
        style = list(
          fontFamily = "-apple-system, Helvetica Neue"
        ),
        searchInputStyle = list(width = "100%"),
        cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center", fontSize = "13px"),
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "a#555",
          border = "none",
          fontSize = "12px",
          fontWeight = "bold"
        )
      ),
      defaultColDef = colDef(
        align = "center",
        minWidth = 60
      ),
      columns = list(
        Joueur = colDef(
          minWidth = 80
        ),
        Moyenne = apply_gradient("Moyenne", table_data_filtered(), decreasing = TRUE, 50),
        Total = apply_gradient("Total", table_data_filtered(), decreasing = TRUE, 50)
      )
    )
    
  })
  
  # Dashboards ----
  ## Global stats ----
  player_data <- reactive({
    data_notes_sheet[player == input$dashboard_players_list,]
    
  })
  
  output$dashboard_player_stats <- renderReactable({
    
    reactable(player_data()[, .(Matches = .N,
                                Minutes = sum(minutes, na.rm = T),
                                `Note moy.` = round(mean(note, na.rm = T), 1)),
                            by = .(Catégorie = category)], 
              pagination = FALSE,
              theme = reactableTheme(
                cellStyle = list(fontSize = "13px",
                                 display = "flex",
                                 alignItems = "center",
                                 justifyContent = "center"),
                headerStyle = list(fontSize = "13px", justifyContent = "center")
              ), 
              defaultColDef = colDef(align = "center", minWidth = 40)
    )
    
  })
  
  ## Minutes per game ----
  output$dashboard_player_minutes <- renderPlot({
    
    ggplot() +
      geom_vline(xintercept = as.numeric(month_separators), linetype = "dashed", color = "grey", size = 0.5) +  # Add dashed vertical lines      geom_segment(player_data(), mapping = aes(x = date, xend = date, y = 0, yend = minutes, color = category), size = 2) +
      geom_segment(player_data(), mapping = aes(x = date, xend = date, y = 0, yend = minutes, color = category), size = 2) +
      geom_hline(yintercept = 0, color = "black", size = 0.5) +
      geom_point(player_data(), mapping = aes(x = date, y = minutes, fill = category), shape = 21, color = "black", size = 10, stroke = 0.5) +
      # geom_bar(player_data(), mapping = aes(x = date, y = minutes, fill = category), color = "black", stat = "identity") +
      geom_text(player_data(), mapping = aes(x = date, y = minutes, label = minutes), vjust = 0.5, family = "Helvetica Neue", fontface = "bold") +
      geom_text(player_data(), mapping = aes(x = date, y = minutes, label = opponent), vjust = -2, family = "Helvetica Neue") +
      labs(title = "Minutes jouées par match",
           x = "Date",
           y = "Minutes jouées") +
      scale_x_datetime(
        limits = range(data_notes_sheet$date)
      ) +
      scale_y_continuous(limits = c(0, 100), breaks = c(0, 15, 30, 45, 60, 75, 90)) +
      scale_color_manual(values = c("Réserve / Espoirs" = "#D9ECC0", "U19 / Gambardella" = "#FBE6A8", "U17" = "#F8D1CA", "U16" = "#E2D0F0", "U15" = "#CADBE0")) +
      scale_fill_manual(values = c("Réserve / Espoirs" = "#D9ECC0", "U19 / Gambardella" = "#FBE6A8", "U17" = "#F8D1CA", "U16" = "#E2D0F0", "U15" = "#CADBE0")) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(family = "Helvetica Neue"),
            axis.text.y = element_text(family = "Helvetica Neue"),
            axis.title = element_text(family = "Helvetica Neue"),
            plot.title = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_line(color = "grey", size = 0.25, linetype = "dashed"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_blank()
      )
    
  })
  
  ## Notes ----
  output$dashboard_player_notes <- renderPlot({
    
    ggplot() +
      geom_vline(xintercept = as.numeric(month_separators), linetype = "dashed", color = "grey", size = 0.5) +  # Add dashed vertical lines      geom_segment(player_data(), mapping = aes(x = date, xend = date, y = 0, yend = minutes, color = category), size = 2) +
      # geom_segment(player_data(), mapping = aes(x = date, xend = date, y = 0, yend = note, color = category), size = 2) +
      geom_hline(yintercept = 0, color = "black", size = 0.5) +
      geom_line(player_data(), mapping = aes(x = date, y = note, group = player), color = "black", size = 1) +
      geom_point(player_data(), mapping = aes(x = date, y = note, fill = category), shape = 21, color = "black", size = 10, stroke = 0.5) +
      # geom_bar(player_data(), mapping = aes(x = date, y = note, fill = category), color = "black", stat = "identity") +
      geom_text(player_data(), mapping = aes(x = date, y = note, label = note), vjust = 0.5, family = "Helvetica Neue", fontface = "bold") +
      geom_text(player_data(), mapping = aes(x = date, y = note, label = opponent), vjust = 2.5, family = "Helvetica Neue") +
      labs(title = "Minutes jouées par match",
           x = "Date",
           y = "Minutes jouées") +
      scale_x_datetime(
        limits = range(data_notes_sheet$date)
      ) +
      scale_y_continuous(limits = c(0, 10)) +
      scale_color_manual(values = c("Réserve / Espoirs" = "#D9ECC0", "U19 / Gambardella" = "#FBE6A8", "U17" = "#F8D1CA", "U16" = "#E2D0F0", "U15" = "#CADBE0")) +
      scale_fill_manual(values = c("Réserve / Espoirs" = "#D9ECC0", "U19 / Gambardella" = "#FBE6A8", "U17" = "#F8D1CA", "U16" = "#E2D0F0", "U15" = "#CADBE0")) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(family = "Helvetica Neue"),
            axis.text.y = element_text(family = "Helvetica Neue"),
            axis.title = element_text(family = "Helvetica Neue"),
            plot.title = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.grid.major.y = element_line(color = "grey", size = 0.25, linetype = "dashed"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_blank()
      )
    
  })
  
  # Data ----
  output$all_data_table <- renderReactable({
    reactable(data_notes_sheet,
              pagination = TRUE,
              sortable = TRUE,
              compact = FALSE,
              highlight = TRUE,
              borderless = TRUE,
              bordered = FALSE,
              outlined = FALSE,
              striped = TRUE,
              fullWidth = TRUE,
              searchable = TRUE,
              defaultPageSize = 50,
              theme = reactableTheme(
                style = list(
                  fontFamily = "-apple-system, Helvetica Neue"
                ),
                searchInputStyle = list(width = "50%"),
                cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center", fontSize = "12px"),
                headerStyle = list(
                  "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                  "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                  borderColor = "#555",
                  border = "none",
                  fontSize = "12px",
                  fontWeight = "bold"
                )
              ),
              defaultColDef = colDef(
                align = "center",
                minWidth = 60
              )
    )
    
  })
  
  # Stop app when session ends
  session$onSessionEnded(function() {
    stopApp()
  })
}


shinyApp(ui = ui, server = server)