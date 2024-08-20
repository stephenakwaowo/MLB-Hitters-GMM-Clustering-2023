# Load required packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(htmltools)
library(mclust)
library(DT)
library(ggplot2)
library(ggfortify)
library(webshot2)
library(htmlwidgets)
library(rvest)
library(dplyr)

merged_success <- read.csv("data/merged_success_v4.csv", check.names = FALSE)
node_data <- read.csv("data/node_data.csv")
edge_data <- read.csv("data/mst_edges.csv")

# Clean up column names: remove non-alphanumeric characters except for % and +, replace / with _
colnames(merged_success) <- gsub("[^[:alnum:]%+]", "", gsub("/", "_", colnames(merged_success)))

# Define metric columns with the cleaned column names
metrics_columns <- c('xwOBA', 'wRC+', 'BsR', 'Spd', 'OFF', 'PPA', '50thmaxvelo', 'BrlsBBE%', 'Swing%', 'SwStr%', 'HardHit%', 'HRFB%', 'ZSwing%', 'ZContact%', 'OSwing%', 'OContact%', 'K%', 'BB%')

# Filter out rows with NA values in the metrics columns
merged_success_filtered <- merged_success %>%
  filter(complete.cases(select(., all_of(metrics_columns))))

# Check for NAs introduced by coercion
na_summary <- sapply(merged_success_filtered[, metrics_columns], function(col) sum(is.na(col)))
print(na_summary)  

# Mapping for display names
display_names <- c(
  'xwOBA' = 'xwOBA',
  'wRC+' = 'wRC+',
  'BsR' = 'BsR',
  'Spd' = 'Spd',
  'OFF' = 'OFF',
  'PPA' = 'P/PA',
  '50thmaxvelo' = '50th Max Velo',
  'HardHit%' = 'HardHit%',
  'BrlsBBE%' = 'Brls/BBE%',
  'HRFB%' = 'HR/FB%',
  'K%' = 'K%',
  'BB%' = 'BB%',
  'Swing%' = 'Swing%',
  'SwStr%' = 'SwStr%',
  'ZSwing%' = 'Z-Swing%',
  'ZContact%' = 'Z-Contact%',
  'OSwing%' = 'O-Swing%',
  'OContact%' = 'O-Contact%'
)

# Check if all metric columns exist in the dataframe
print(all(metrics_columns %in% colnames(merged_success)))  # return TRUE if all are correct

# Convert columns to numeric, handling issues with non-numeric values
merged_success <- merged_success %>%
  mutate(across(all_of(metrics_columns), ~as.numeric(gsub("[^0-9.-]", "", as.character(.))), .names = "{col}"))

# Check for NAs introduced
na_summary <- sapply(merged_success %>% select(all_of(metrics_columns)), function(col) sum(is.na(col)))
print(na_summary) 

# Handle NAs before computing percentiles
percentiles <- sapply(metrics_columns, function(m) {
  quantile(merged_success[[m]], probs = seq(0, 1, length.out = 100), na.rm = TRUE)
}, simplify = FALSE)


# Define metrics that are better when lower
metrics_better_when_lower <- c('K%', 'SwStr%', 'OSwing%')

# Define the color scale
base_colors <- c('#2066ac', '#70acd0', '#adcfe4', '#e3eded', '#fbe8d7', '#fdd1bb', '#cd5041', '#B2172c')
color_gradient <- colorRampPalette(base_colors)

# Create a mapping from metrics_columns to actual column names in node_data
actual_columns <- c(
  'xwOBA' = NA,      
  'wRC+' = NA,
  'BsR' = "BsR",
  'Spd' = "Spd",
  'OFF' = NA,
  'PPA' = NA,
  '50thmaxvelo' = "X50th_max_velo",
  'BrlsBBE%' = "Brls.BBE.",
  'Swing%' = "Swing.",
  'SwStr%' = "SwStr.",
  'HardHit%' = "HardHit.",
  'HRFB%' = "HR.FB.",
  'ZSwing%' = "Z.Swing.",
  'ZContact%' = "Z.Contact.",
  'OSwing%' = "O.Swing.",
  'OContact%' = "O.Contact.",
  'K%' = NA,        
  'BB%' = NA  
)

# Filter out placeholder mappings that don't have actual columns in node_data
actual_columns <- actual_columns[!is.na(actual_columns)]

# Ensure the metrics_columns is filtered to match the actual_columns
filtered_metrics_columns <- names(actual_columns)

# Function to calculate weighted distances
calculate_weighted_distances <- function(data, filtered_metrics_columns, weights) {
  print("Inside calculate_weighted_distances function")
  
  # Apply weights to the selected data
  weighted_data <- data[, filtered_metrics_columns] * weights
  print("Weighted Data:")
  print(head(weighted_data))
  
  # Calculate Manhattan distances
  dist_matrix <- as.matrix(dist(weighted_data, method = "manhattan"))
  return(dist_matrix)
}

scrape_player_bio <- function(player_id) {
  url <- paste0("https://baseballsavant.mlb.com/savant-player/", player_id, "?stats=statcast-r-hitting-mlb")
  
  page <- tryCatch(rvest::read_html(url), error = function(e) {
    warning("Failed to read HTML page for player ID: ", player_id, " Error: ", e)
    return(NULL)
  })
  
  if (is.null(page)) {
    warning("Page is NULL for player ID: ", player_id)
    return(data.frame(
      PlayerID = player_id,
      Name = "", Position = "", BatsThrows = "",
      HeightWeight = "", Age = "", DraftInfo = "", College = "",
      stringsAsFactors = FALSE
    ))
  }
  
  name <- page %>% rvest::html_node(".bio-player-name div[style='display: inline-block']") %>% rvest::html_text(trim = TRUE)
  position <- page %>% rvest::html_node(".bio-player-name") %>% rvest::html_nodes("div") %>% .[2] %>% rvest::html_text(trim = TRUE)
  bats_throws <- page %>% rvest::html_node(".bio-player-name") %>% rvest::html_nodes("div") %>% .[2] %>% rvest::html_text(trim = TRUE) %>%
    stringr::str_extract("Bats/Throws: [^|]*") %>% stringr::str_replace("Bats/Throws: ", "")
  height_weight <- page %>% rvest::html_node(".bio-player-name") %>% rvest::html_nodes("div") %>% .[2] %>% rvest::html_text(trim = TRUE) %>%
    stringr::str_extract("[0-9]+' [0-9]\" [0-9]+LBS") %>% stringr::str_trim()
  age <- page %>% rvest::html_node(".bio-player-name") %>% rvest::html_nodes("div") %>% .[2] %>% rvest::html_text(trim = TRUE) %>%
    stringr::str_extract("Age: [0-9]+") %>% stringr::str_replace("Age: ", "")
  draft_info <- page %>% rvest::html_node(".bio-player-name") %>% rvest::html_nodes("div") %>% .[3] %>% rvest::html_text(trim = TRUE)
  college <- page %>% rvest::html_node(".bio-player-name") %>% rvest::html_nodes("div") %>% .[3] %>% rvest::html_text(trim = TRUE) %>%
    stringr::str_extract("Oregon State")
  
  data.frame(
    PlayerID = player_id,
    Name = ifelse(is.na(name) || length(name) == 0, "", name),
    Position = ifelse(is.na(position) || length(position) == 0, "", position),
    BatsThrows = ifelse(is.na(bats_throws) || length(bats_throws) == 0, "", bats_throws),
    HeightWeight = ifelse(is.na(height_weight) || length(height_weight) == 0, "", height_weight),
    Age = ifelse(is.na(age) || length(age) == 0, "", age),
    DraftInfo = ifelse(is.na(draft_info) || length(draft_info) == 0, "", draft_info),
    College = ifelse(is.na(college) || length(college) == 0, "", college),
    stringsAsFactors = FALSE
  )
}

# UI for Shinydashboard
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = tags$div(
      style = "width: 100%; display: flex; justify-content: space-between;", 
      tags$span("MLB Batter Analysis Dashboard"), 
      tags$span("|", style = "margin-left: 10px; margin-right: 10px;"),
      tags$span("Stephen Akwaowo", style = "margin-right: 15px;")
    ),
    titleWidth = 500
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Batter Cards (2023)", tabName = "generate", icon = icon("baseball"),
               menuSubItem("Single Player", tabName = "single_player"),
               menuSubItem("Multi Player Compare", tabName = "multi_player")
      ),
      menuItem("Batter Clusters (2023)", tabName = "clusters", icon = icon("sitemap"),
               menuSubItem("GMM Cluster Model", tabName = "gmm_cluster_model"),
               menuSubItem("Network Graph", tabName = "network_graph")
      ),
      menuItem("Player Stats (2023)", tabName = "stats_dashboard", icon = icon("table")),
      menuItem("BONDS Index (2023)", tabName = "bonds_index", icon = icon("chart-line")),
      menuItem("Methodology", tabName = "methodology", icon = icon("file-alt")),
      menuItem("Data Sources", tabName = "data_sources", icon = icon("database")),
      menuItem("About the Project", tabName = "about_project", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "icon", href = "baseball_favicon.png", type = "image/png"),
      tags$script(src = "html2canvas.min.js"),
      tags$script(HTML("
      $(document).ready(function(){
        $('#downloadPlot').removeClass('disabled');
        $('#downloadPlot').removeAttr('aria-disabled');
        $('#downloadPlot').removeAttr('tabindex');
      });
      $(document).on('click', '#downloadPlot', function(){
        html2canvas(document.getElementById('capture_area')).then(function(canvas) {
          var link = document.createElement('a');
          link.href = canvas.toDataURL('image/png');
          link.download = 'player_metrics.png';
          link.click();
        });
      });
    "))
    ),
    tags$script(HTML('document.title = "MLB Batter Analysis Dashboard by @stephenakwaowo";')),
    tags$link(rel = "stylesheet", type = "text/css", href = "shiny_app.css"),
    tags$script(src = "https://cdn.datatables.net/fixedcolumns/4.0.1/js/dataTables.fixedColumns.min.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.datatables.net/fixedcolumns/4.0.1/css/fixedColumns.dataTables.min.css")
    ,
    tags$style(HTML("
      .metric-box {
        border: 1px solid black;
        padding: 10px;
        margin: 5px;
        text-align: center;
        font-size: 14px;
        flex: 1 0 21%; /* responsive sizing */
      }
      .metric-container {
        display: flex;
        flex-wrap: wrap;
        justify-content: space-around;
      }
      .dashboard-header .title {
        white-space: normal; /* Ensures text wraps if it's too long */
      }
    ")),
    tags$style(HTML("
      .content-wrapper {
        overflow-x: auto;
      }
    ")),
    tabItems(
      tabItem(tabName = "single_player",
              fluidRow(
                box(width = 12, 
                    selectInput("player_dropdown", "Select Player", 
                                choices = sort(unique(merged_success$Name)),
                                selected = sample(unique(merged_success$Name), 1)),  # Random player selection
                    p("Select a player from the dropdown to view detailed metrics and performance visualizations.")
                ),
                div(id = "capture_area",
                    box(title = "2023 Player Metrics", width = 12, solidHeader = FALSE, color = "purple",
                        fluidRow(
                          column(width = 2,
                                 uiOutput("player_image_single"),  # Player image here
                                 uiOutput("player_bio")            # Player bio here
                          ),
                          column(width = 8,
                                 div(style = "display: flex; justify-content: flex-start;",
                                     plotlyOutput("player_graph", width = "95%")    # Radar chart here with adjusted width
                                 )
                          ),
                          column(width = 2,
                                 plotlyOutput("bonds_index_gauge")  # BONDS Index gauge chart here
                          )
                        ),
                        uiOutput("metrics_container_single")
                    )
                ),
              )
      ),
      tabItem(tabName = "multi_player",
              fluidRow(
                box(title = "Select Players", width = 12, 
                    selectInput("player1_dropdown", "Player 1", choices = sort(unique(merged_success$Name)),
                                selected = sample(unique(merged_success$Name), 1)),  # Random player 1 selection
                    selectInput("player2_dropdown", "Player 2", choices = sort(unique(merged_success$Name)),
                                selected = sample(unique(merged_success$Name), 1)),  # Random player 2 selection
                    p("Select two players to compare their metrics and performance.")
                ),
                box(title = "2023 Comparison Metrics", width = 12, solidHeader = FALSE, color = "purple", 
                    plotlyOutput("comparison_graph"),
                    fluidRow(
                      column(6, 
                             div(style = "text-align: center; display: flex; align-items: center; justify-content: space-around;",
                                 uiOutput("player_image_multi_1"),
                                 plotlyOutput("bonds_index_gauge1", height = "200px", width = "200px")  # BONDS Index gauge for Player 1
                             ),
                             uiOutput("player1_metrics")
                      ),
                      column(6, 
                             div(style = "text-align: center; display: flex; align-items: center; justify-content: space-around;",
                                 uiOutput("player_image_multi_2"),
                                 plotlyOutput("bonds_index_gauge2", height = "200px", width = "200px")  # BONDS Index gauge for Player 2
                             ),
                             uiOutput("player2_metrics")
                      )
                    )
                )
              )
      ),
      tabItem(tabName = "gmm_cluster_model",
              fluidRow(
                box(title = "Batter Clusters", width = 12, solidHeader = FALSE, color = "purple", 
                    p("This plot shows the clusters of batters based on their performance metrics using Gaussian Mixture Model (GMM) clustering. Each cluster is represented by an ellipse."),
                    plotlyOutput("gmm_cluster_graph")
                )
              )
      ),
      tabItem(tabName = "network_graph",
              fluidRow(
                box(title = "Weight Adjustments", width = 4, solidHeader = FALSE, color = "purple", 
                    lapply(filtered_metrics_columns, function(metric) {
                      sliderInput(paste0("weight_", metric), metric, min = 0.5, max = 2, value = 1, step = 0.1)
                    }),
                    actionButton("reset_filters", "Reset Filters"),  # Add reset button
                    p("Use the sliders to adjust the weight of each metric. The network graph will update to reflect these changes.")
                ),
                box(title = "Cluster Filter", width = 2, solidHeader = FALSE, color = "purple", 
                    checkboxGroupInput("selected_clusters", "Select Clusters", choices = 1:10, selected = 1:10),
                    p("Select clusters to view in the network graph."),
                    actionButton("select_all_clusters", "Select All"),
                    actionButton("unselect_all_clusters", "Unselect All")
                ),
                box(title = "Network Graph", width = 6, solidHeader = FALSE, color = "purple", 
                    plotlyOutput("cluster_network_graph")
                )
              ),
              fluidRow(
                box(title = "Adjusted Metrics Table", width = 12, solidHeader = FALSE, color = "purple",
                    p("This table shows the original and adjusted metrics for each player based on the weights you have selected."),
                    DT::dataTableOutput("adjusted_metrics_table")
                )
              )
      ),
      tabItem(tabName = "stats_dashboard",
              fluidRow(
                box(title = "Player Stats (2023)", width = 12, solidHeader = FALSE, color = "purple", 
                    p("This dashboard displays the key statistics of players for the 2023 season. Use the table below to explore and sort through various player metrics."),
                    DT::dataTableOutput("stats_table")
                )
              )
      ),
      tabItem(tabName = "bonds_index",
              fluidRow(
                box(title = "BONDS Index (2023)", width = 12, solidHeader = FALSE, color = "purple", 
                    p("The BONDS Index is a comprehensive metric designed to evaluate baseball players' offensive performance, emphasizing key areas such as power, plate discipline, and batted-ball outcomes. The index is named for Barry Bonds, reflecting the attributes that made him one of the greatest hitters in baseball history."),
                    DT::dataTableOutput("bonds_index_table")
                )
              )
      ),
      tabItem(tabName = "methodology",
              fluidRow(
                box(title = "Methodology", width = 12, 
                    h3("Sequential Dimensionality Reduction Strategy"),
                    p("Dimensionality reduction is a critical step in preparing high-dimensional data for clustering. In this project, I employed a two-step sequential dimensionality reduction strategy combining Principal Component Analysis (PCA) and t-Distributed Stochastic Neighbor Embedding (t-SNE)."),
                    tags$div(style = "margin-left: 20px;",
                             h4(HTML("<b>Principal Component Analysis (PCA)</b>")),
                             p("PCA transforms the original high-dimensional data into a new set of features called principal components. These components are ranked by the amount of variance they capture from the original data. I selected the number of components to retain based on capturing approximately 91% of the total variance."),
                             h4(HTML("<b>t-Distributed Stochastic Neighbor Embedding (t-SNE)</b>")),
                             p("t-SNE is a non-linear dimensionality reduction technique that preserves local relationships between data points. After reducing the dimensionality with PCA, t-SNE was applied to compress and visualize the data in two dimensions. Multiple runs of t-SNE were conducted to find the best representation based on the Kullback-Leibler (KL) divergence score.")
                    ),
                    h3("GMM Cluster Algorithm"),
                    p("Gaussian Mixture Models (GMM) is a probabilistic model used for clustering data points into groups or clusters. GMM represents each cluster as a Gaussian distribution, allowing for more flexibility compared to traditional clustering algorithms."),
                    tags$div(style = "margin-left: 20px;",
                             h4(HTML("<b>Initialization and Convergence</b>")),
                             p("Initialization is a critical step in the GMM algorithm, as it significantly influences the final clustering results. In this project, the default initialization method was used, which initializes the means with the k-means method. The Expectation-Maximization (EM) algorithm was then used to iteratively refine the cluster parameters, including means, covariances, and mixing coefficients. Convergence was assessed by monitoring the change in the log-likelihood between iterations, with the algorithm stopping when the change was below a predefined threshold (e.g., 1e-6), indicating that the model parameters had stabilized."),
                             h4(HTML("<b>Covariance Types</b>")),
                             p("GMM can model clusters with different covariance structures. In this project, I explored various covariance types, including:"),
                             tags$ul(
                               tags$li(HTML("<b>Spherical</b>: Each cluster is assumed to have the same variance in all directions.")),
                               tags$li(HTML("<b>Diagonal</b>: Each cluster can have different variances, but these are aligned with the axes.")),
                               tags$li(HTML("<b>Tied</b>: All clusters share the same general covariance matrix.")),
                               tags$li(HTML("<b>Full</b>: Each cluster has its own general covariance matrix."))
                             ),
                             p("The choice of covariance type can significantly affect the model's flexibility and complexity. The 'full' covariance type was ultimately chosen for its ability to capture correlations between features within each cluster, leading to a more accurate representation of the data structure."),
                             h4(HTML("<b>Model Validation</b>")),
                             p("To ensure the robustness and reliability of the clustering results, several validation techniques were employed:"),
                             tags$ul(
                               tags$li(HTML("<b>Akaike Information Criterion (AIC)</b> and <b>Bayesian Information Criterion (BIC)</b>: These metrics help assess model quality by penalizing complex models. Both AIC and BIC were minimized to select the best-fitting model, balancing goodness-of-fit and model complexity.")),
                               tags$li(HTML("<b>Silhouette Score</b>: This score measures the cohesion and separation of clusters. A higher silhouette score indicates well-defined and distinct clusters. The silhouette score confirmed the optimal number of clusters identified by AIC/BIC.")),
                               tags$li(HTML("<b>Median Distances</b>: This metric evaluates the tightness and separation of clusters by analyzing the median pairwise distances within and between clusters."))
                             ),
                             h4(HTML("<b>Handling of Outliers</b>")),
                             p("Outliers can significantly impact the results of GMM clustering by skewing the estimated parameters of the Gaussian distributions. In this project, robust techniques were used to handle outliers:"),
                             tags$ul(
                               tags$li(HTML("<b>Robust Scaling</b>: Metrics were scaled using robust statistical measures, such as the median and interquartile range (IQR), to minimize the influence of outliers on the model.")),
                               tags$li(HTML("<b>Trimming and Winsorization</b>: In some cases, extreme values were either trimmed or capped to reduce their impact. Trimming involved excluding the top and bottom percentiles of the data, while winsorization capped extreme values at specific percentiles."))
                             ),
                             p("These steps ensured that the GMM model accurately represented the underlying data distribution without being unduly influenced by extreme values.")
                    ),
                    h3("BONDS Index"),
                    p("The BONDS Index is a comprehensive metric designed to evaluate baseball players' offensive performance, emphasizing key areas such as power, plate discipline, and batted-ball outcomes. The index is named for Barry Bonds, reflecting the attributes that made him one of the greatest hitters in baseball history."),
                    tags$div(style = "margin-left: 20px;",
                             tags$ul(
                               tags$li(HTML("<b>B</b>: Batted-ball")),
                               tags$li(HTML("<b>O</b>: Outcomes")),
                               tags$li(HTML("<b>N</b>: Normalized")),
                               tags$li(HTML("<b>D</b>: Discipline")),
                               tags$li(HTML("<b>S</b>: Swing efficiency"))
                             ),
                             h4(HTML("<b>Weighting</b>")),
                             p("The BONDS Index uses a Ridge regression model to assign weights to key metrics. This model helps balance different aspects of hitting performance to provide a comprehensive evaluation of a player's offensive capabilities."),
                             p("The key metrics include:"),
                             tags$ul(
                               tags$li(HTML("<b>Brls/BBE%</b>: Percentage of batted balls hit with ideal exit velocity and launch angle, indicating quality contact.")),
                               tags$li(HTML("<b>50th Max Velo</b>: Consistency in making hard contact.")),
                               tags$li(HTML("<b>O-Swing%</b>: Percentage of swings at pitches outside the strike zone.")),
                               tags$li(HTML("<b>SwStr%</b>: Percentage of swinging strikes, reflecting the ability to avoid swings & misses.")),
                               tags$li(HTML("<b>Z-Swing%</b>: Aggressiveness on pitches in the strike zone.")),
                               tags$li(HTML("<b>Z-Contact%</b>: Ability to make contact with pitches in the strike zone.")),
                               tags$li(HTML("<b>Pulled FB%</b>: Percentage of fly balls hit to the pull side, highly positively correlated with power.")),
                               tags$li(HTML("<b>Oppo FB%</b>: Percentage of fly balls hit to the opposite field, negatively correlated with power."))
                             ),
                             h4(HTML("<b>BONDS Index Calculation</b>")),
                             p("The BONDS Index is calculated using the following steps:"),
                             tags$ol(
                               tags$li(HTML("<b>Standardize the Metrics</b>: Each metric is standardized using `StandardScaler`.")),
                               tags$li(HTML("<b>Fit a Ridge Regression Model</b>: The standardized metrics are used as predictors, and offensive Wins Above Replacement (oWAR) is the target.")),
                               tags$li(HTML("<b>Normalize the Coefficients</b>: The Ridge regression coefficients are normalized to sum to 1, creating the weights for the BONDS Index.")),
                               tags$li(HTML("<b>Calculate the BONDS Index</b>: The BONDS Index for each player is calculated by summing the weighted metrics.")),
                               tags$li(HTML("<b>Apply Adjusted Scaling</b>: Adjusted scaling is applied to provide a broader range for the index, centering the mean at 50 and extending the range beyond typical limits:"))
                             ),
                             code("Adjusted Scaling = 50 + ((BONDS Index - mean(BONDS Index)) / Range) × 50"),
                             h4(HTML("<b>Detailed Example</b>")),
                             p("For a specific player, the BONDS Index calculation would look like this:"),
                             tags$ol(
                               tags$li(HTML("<b>Standardize each metric</b>: Subtract the mean and divide by the standard deviation.")),
                               tags$li(HTML("<b>Multiply each standardized metric</b>: Multiply by its corresponding weight.")),
                               tags$li(HTML("<b>Sum the weighted metrics</b>: Sum to get the raw BONDS Index.")),
                               tags$li(HTML("<b>Apply the adjusted scaling formula</b>: Apply to the raw BONDS Index."))
                             )
                    ),
                    h3("Radar Charts"),
                    tags$div(style = "margin-left: 20px;",
                             p("Radar charts are used to visually compare the performance of players across different categories. Each axis of the radar chart represents a category of metrics:"),
                             tags$ul(
                               tags$li(HTML("<b>Power</b>: Includes 50th max velo, barrels per batted ball event, hard hit percentage, and home run per fly ball rate.")),
                               tags$li(HTML("<b>Swing Decision</b>: Includes out-of-zone swing percentage, in-zone swing percentage, pitches per plate appearance, meatball swing percentage, and first strike percentage.")),
                               tags$li(HTML("<b>Bat-to-Ball</b>: Includes out-of-zone contact percentage, in-zone contact percentage, swinging strike percentage, solid contact percentage, and line drive percentage.")),
                               tags$li(HTML("<b>Baserunning</b>: Includes Fangraphs' speed  and baserunning runs metrics."))
                             ),
                             p("The mean values for each category were calculated for each cluster, and the resulting radar charts provide a clear and intuitive visualization of the differences and similarities among clusters. Adjustments were made to ensure meaningful comparisons, including the application of weights to certain metrics and normalization of values to a common scale."),
                             p("The visualizations help to quickly identify the strengths and weaknesses of players within each cluster, making it a powerful tool for analysis and decision-making.")
                    ),
                    tags$div(style = "margin-left: 20px;",
                             h4(HTML("<b>Metric Weighting</b>")),
                             p("Metric weighting is a crucial step to ensure that the metrics contributing to the radar chart are appropriately emphasized based on their importance. In this project, I utilized Random Forest analysis to determine the importance of each metric within its respective category."),
                             h4(HTML("<b>Composite Metrics and Normalized Feature Importances</b>")),
                             p("For each category, a composite score was created using key metrics, and their importance was determined through Random Forest analysis. Below are the details for each category:"),
                             tags$ul(
                               tags$li(
                                 HTML("<b>Power:</b> Composite of xISO and xwOBACON."),
                                 tags$ul(
                                   tags$li("RMSE: 0.0539"),
                                   tags$li("Feature Importances:"),
                                   tags$ul(
                                     tags$li("Brls/BBE%: 31.88%"),
                                     tags$li("HR/FB%: 26.64%"),
                                     tags$li("HardHit%: 24.00%"),
                                     tags$li("50th_max_velo: 17.48%")
                                   )
                                 )
                               ),
                               tags$li(
                                 HTML("<b>Bat-to-Ball:</b> Composite of xBA and xwOBA."),
                                 tags$ul(
                                   tags$li("RMSE: 0.0297"),
                                   tags$li("Feature Importances:"),
                                   tags$ul(
                                     tags$li("O-Contact%: 22.30%"),
                                     tags$li("Line Drives%: 21.18%"),
                                     tags$li("Solid Contact%: 20.25%"),
                                     tags$li("Z-Contact%: 18.34%"),
                                     tags$li("SwStr%: 17.93%")
                                   )
                                 )
                               ),
                               tags$li(
                                 HTML("<b>Swing Decision:</b> Composite of K%-BB% and Z-Swing%-O-Swing%."),
                                 tags$ul(
                                   tags$li("RMSE: 1.32"),
                                   tags$li("Feature Importances:"),
                                   tags$ul(
                                     tags$li("Meatball_swing%: 36.36%"),
                                     tags$li("O-Swing%: 30.05%"),
                                     tags$li("Z-Swing%: 17.64%"),
                                     tags$li("P/PA: 9.57%"),
                                     tags$li("F_Strike%: 6.38%")
                                   )
                                 )
                               ),
                               tags$li(
                                 HTML("<b>Baserunning:</b> Based on Runner Runs."),
                                 tags$ul(
                                   tags$li("RMSE: 0.4190"),
                                   tags$li("Feature Importances:"),
                                   tags$ul(
                                     tags$li("BsR: 92.74%"),
                                     tags$li("Spd: 7.26%")
                                   )
                                 )
                               )
                             ),
                             h4(HTML("<b>Scaling Factor Determination</b>")),
                             p("During the initial visualization process, it became evident that the scales of different metrics were leading to disproportionate influences on the overall radar chart scores. To address this, a scaling factor of 5.875 was applied to the weights. This factor was determined through an iterative process, where the impact on radar charts across different players was observed and refined to achieve balanced visualizations."),
                             h4(HTML("<b>Re-weighting Adjustments</b>")),
                             p("The Baserunning category initially showed skewed results due to the high feature importance of BsR. To balance this, the weight of BsR was reduced and the weight of Spd was increased. This ensured that the Baserunning score accurately reflected player performance without being overly dominated by a single metric."),
                             h4(HTML("<b>Data Normalization</b>")),
                             p("Data normalization was performed to ensure that all metrics contributed equally to the radar charts, regardless of their original scale. The metrics were normalized to a range of 0 to 1, and adjustments were made to handle metrics where lower values indicated better performance."),
                             p("This comprehensive approach to metric weighting, scaling, and normalization ensures that the radar charts provide an accurate and balanced visualization of player performance, allowing for meaningful comparisons across different categories.")
                    )
                )
              )
      ),
      tabItem(tabName = "data_sources",
              fluidRow(
                box(title = "Data Sources", width = 12, 
                    p("The data used in this dashboard is sourced from various publicly available databases, primarily from FanGraphs and Statcast. This includes player performance metrics, clustering results, and network graph data."),
                    p("Sources include:"),
                    tags$ul(
                      tags$li(
                        a(href = "https://www.fangraphs.com/", "FanGraphs"),
                        ": Contains player performance metrics, such as wRC+, BsR, and more.",
                        tags$ul(
                          tags$li(a(href = "https://www.dropbox.com/scl/fi/s81eglnpwg52p0bsw7kom/fangraphs_2023.csv?rlkey=v7i29z3eg4n5e2iaokbj9wwj7&st=40zhpsz2&dl=0", "FanGraphs 2023 CSV"))
                        )
                      ),
                      tags$li(
                        a(href = "https://baseballsavant.mlb.com/", "Statcast"),
                        ": Provides advanced metrics, such as exit velocity, launch angle, sprint speed, etc.",
                        tags$ul(
                          tags$li(a(href = "https://www.dropbox.com/scl/fi/3lu86wiwqy09c9i77heob/statcast_2023.csv?rlkey=qtxq51rpvk4ixmf1olqfq9xmp&st=6j0uqcg6&dl=0", "Statcast 2023 CSV"))
                        )
                      ),
                    ),
                    p("These datasets were cleaned and preprocessed before being used in the analysis. The cleaned data was then used to perform PCA for dimensionality reduction and GMM for clustering."),
                    p("The visualizations in this dashboard were initially created in Python using Plotly and NetworkX libraries. However, they were re-implemented in R for integration into the Shiny app. These tools allow for interactive and dynamic exploration of the data, providing users with a comprehensive view of player performance and clusters.")
                )
              )
      ),
      tabItem(tabName = "about_project",
              fluidRow(
                box(title = "About the Project", width = 12, 
                    p("This project aims to provide a comprehensive analysis of MLB batter performance using advanced statistical techniques and interactive visualizations."),
                    p("Developed by me, Stephen Akwaowo, as part of a broader initiative to explore sports analytics and provide actionable insights into player performance."),
                    h3("Project Goals"),
                    p("The primary goal of this project is to offer a detailed and interactive analysis of MLB batters. By leveraging advanced statistical methods and machine learning techniques, this dashboard helps users identify key performance metrics and understand player capabilities in a visual and intuitive manner."),
                    p("Additionally, this project seeks to demonstrate the potential of data analytics in sports. By providing insights into player performance, it aims to support better decision-making processes for coaches, analysts, and fans alike."),
                    h3("Methodology"),
                    p("The data for this project was collected from various publicly available sources. After cleaning and preprocessing the data, several statistical techniques were employed to derive meaningful insights. Principal Component Analysis (PCA) was used for dimensionality reduction, while Gaussian Mixture Models (GMM) helped in clustering the players based on their performance metrics."),
                    p("The visualizations were created using Plotly and Shiny, allowing for interactive and dynamic exploration of the data. The network graphs, radar charts, and other visual tools provide users with a comprehensive view of player performance and clusters."),
                    h3("Inspirations"),
                    p("This project was inspired by various sources and projects that helped shape its design and functionality:"),
                    tags$ul(
                      tags$li("FIFA video games for the radar charts"),
                      tags$li(a(href = "https://edge.nhl.com/en/skater/20232024-regular-8482740", "NHL Edge"), " for their radar charts"),
                      tags$li(a(href = "https://www.patreon.com/tj_stats/posts", "TJ Stats"), " for his batter card pages"),
                      tags$li(
                        "Alex Stern for his project: ",
                        a(href = "https://alexcstern.github.io/hoopDown.html", 
                          "\"Clustering NBA Player Types: A Tutorial on K-Means, Gaussian Mixture Models, Principal Component Analysis, and Graphical Networks\"")
                      )
                    ),
                    h3("Future Work"),
                    p("Moving forward, this project can be expanded to include more metrics and data points. Integration with real-time data sources could provide up-to-date analysis, making the tool even more valuable. Additionally, similar analyses could be conducted for other sports, broadening the scope and applicability of the project."),
                    p("Your feedback and suggestions are highly appreciated. If you have any questions or would like to collaborate, please reach out to me through email at stephenak24@gmail.com or through my different social media accounts like Twitter, @stephenakwaowo.")
                )
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Category features and weights
  category_features <- list(
    'Power' = c('50thmaxvelo', 'BrlsBBE%', 'HardHit%', 'HRFB%'),
    'Swing Decision' = c('OSwing%', 'ZSwing%', 'PPA', 'Meatball_Swing%', 'F_Strike%'),
    'Bat-to-Ball' = c('OContact%', 'ZContact%', 'SwStr%', 'Solid_Contact%', 'LD%'),
    'Baserunning' = c('Spd', 'BsR')
  )
  
  scaling_factor <- 5.875  # Adjusted scaling factor
  
  # Adjusted weights for the Baserunning category
  weights <- c(
    '50thmaxvelo' = 0.1748 * scaling_factor,
    'BrlsBBE%' = 0.3188 * scaling_factor,
    'HardHit%' = 0.2400 * scaling_factor,
    'HRFB%' = 0.2664 * scaling_factor,
    'OSwing%' = 0.3005 * scaling_factor,
    'ZSwing%' = 0.1764 * scaling_factor,
    'PPA' = 0.0957 * scaling_factor,
    'Meatball_Swing%' = 0.3636 * scaling_factor,
    'F_Strike%' = 0.0638 * scaling_factor,
    'OContact%' = 0.2230 * scaling_factor,
    'ZContact%' = 0.1834 * scaling_factor,
    'SwStr%' = 0.1793 * scaling_factor,
    'Solid_Contact%' = 0.2025 * scaling_factor,
    'LD%' = 0.2118 * scaling_factor,
    'Spd' = 0.15 * scaling_factor,      # Increased weight for Spd
    'BsR' = 0.4 * scaling_factor         # Reduced weight for BsR
  )
  
  # Load the BONDS Index data
  bonds_index_data <- read.csv('data/BONDS_leaderboard_2023.csv')
  
  # Render the BONDS Index leaderboard
  output$bonds_index_table <- DT::renderDataTable({
    DT::datatable(
      bonds_index_data, 
      options = list(
        pageLength = 25, 
        scrollX = TRUE, 
        lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))
      ),
      colnames = c('Name', 'Age', 'Team', 'BONDS Index')
    )
  })
  
  # Render the BONDS Index gauge chart
  output$bonds_index_gauge <- renderPlotly({
    req(input$player_dropdown)
    player_name <- input$player_dropdown
    player_data <- bonds_index_data[bonds_index_data$Name == player_name, ]
    
    if (nrow(player_data) == 0) {
      return(NULL)  # Handle case where player data is not found
    }
    
    bonds_index_value <- player_data$BONDS.Index
    min_bonds <- min(bonds_index_data$BONDS.Index)
    max_bonds <- max(bonds_index_data$BONDS.Index)
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = bonds_index_value,
      title = list(text = "BONDS Index"),
      gauge = list(
        axis = list(range = list(min_bonds, max_bonds)),
        bar = list(color = "black"),
        steps = list(
          list(range = c(min_bonds, min_bonds + (max_bonds - min_bonds) * 0.125), color = "#2066ac"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.125, min_bonds + (max_bonds - min_bonds) * 0.25), color = "#70acd0"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.25, min_bonds + (max_bonds - min_bonds) * 0.375), color = "#adcfe4"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.375, min_bonds + (max_bonds - min_bonds) * 0.5), color = "#e3eded"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.5, min_bonds + (max_bonds - min_bonds) * 0.625), color = "#fbe8d7"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.625, min_bonds + (max_bonds - min_bonds) * 0.75), color = "#fdd1bb"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.75, min_bonds + (max_bonds - min_bonds) * 0.875), color = "#cd5041"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.875, max_bonds), color = "#B2172c")
        ),
        threshold = list(
          line = list(color = "black", width = 4),
          thickness = 1,
          value = bonds_index_value
        ),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "gray"
      )
    ) %>%
      layout(
        margin = list(l = 25, r = 50, t = 45, b = 45)
      )
  })
  
  # Function to generate the player image URL
  generate_image_url <- function(player_id) {
    base_url <- "https://img.mlbstatic.com/mlb-photos/image/upload/d_people:generic:headshot:67:current.png/w_426,q_auto:best/v1/people/"
    paste0(base_url, player_id, "/headshot/67/current")
  }
  
  # Render the player bio and image for the single player tab
  output$player_image_single <- renderUI({
    req(input$player_dropdown)  # Ensure a player is selected
    player_name <- input$player_dropdown
    player_id <- merged_success$playerid[merged_success$Name == player_name]
    
    # Fetch the player's bio data
    bio_data <- scrape_player_bio(player_id)
    
    img_src <- generate_image_url(player_id)
    
    # Display the bio information if available
    bio_info <- list()
    if (!is.na(bio_data$Name)) {
      bio_info <- c(bio_info, tags$p(strong("Name:"), bio_data$Name))
    }
    if (!is.na(bio_data$Position)) {
      bio_info <- c(bio_info, tags$p(strong("Position:"), bio_data$Position))
    }
    if (!is.na(bio_data$BatsThrows)) {
      bio_info <- c(bio_info, tags$p(strong("Bats/Throws:"), strong(bio_data$BatsThrows)))
    }
    if (!is.na(bio_data$HeightWeight)) {
      bio_info <- c(bio_info, tags$p(strong("Height/Weight:"), bio_data$HeightWeight))
    }
    if (!is.na(bio_data$Age)) {
      bio_info <- c(bio_info, tags$p(strong("Age:"), strong(bio_data$Age)))
    }
    if (!is.na(bio_data$DraftInfo)) {
      bio_info <- c(bio_info, tags$p(strong("Draft Info:"), bio_data$DraftInfo))
    }
    if (!is.na(bio_data$College)) {
      bio_info <- c(bio_info, tags$p(strong("College:"), bio_data$College))
    }
    
    # Combine the image and bio information in the UI
    tagList(
      tags$div(
        tags$img(src = img_src, 
                 height = "200px", 
                 width = "150px", 
                 style = "border: 2px solid #ddd; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);"),
        bio_info
      )
    )
  })
  
  # Scrape and display player bio information
  output$player_bio <- renderUI({
    req(input$player_dropdown)
    
    player_name <- input$player_dropdown
    player_id <- merged_success$playerid[merged_success$Name == player_name]
    
    bio_data <- scrape_player_bio(player_id)
    
    # Extract and clean individual components
    position_info <- sub(" \\|.*", "", bio_data$Position)
    bats_throws <- ifelse(grepl("Bats/Throws:", bio_data$Position), 
                          sub(".*Bats/Throws:\\s*", "", sub("\\s*\\|\\s*\\d.*", "", bio_data$Position)),
                          "")
    age <- ifelse(grepl("Age:", bio_data$Position), 
                  sub(".*Age:\\s*", "", bio_data$Position),
                  "")
    
    tagList(
      if (bio_data$Name != "") tags$p(tags$strong("Name: "), bio_data$Name),
      if (position_info != "") tags$p(tags$strong("Position: "), position_info),
      if (bats_throws != "") tags$p(tags$strong("Bats/Throws: "), bats_throws),
      if (bio_data$HeightWeight != "") tags$p(tags$strong("Height/Weight: "), bio_data$HeightWeight),
      if (age != "") tags$p(tags$strong("Age: "), age),
      if (bio_data$DraftInfo != "") tags$p(tags$strong("Draft Info: "), bio_data$DraftInfo),
      if (bio_data$College != "") tags$p(tags$strong("College: "), bio_data$College)
    )
  })
  
  # Render the player image for the single player tab
  output$player_image_single <- renderUI({
    req(input$player_dropdown)
    player_name <- input$player_dropdown
    player_id <- merged_success$playerid[merged_success$Name == player_name]
    
    img_src <- generate_image_url(player_id)
    
    tags$img(src = img_src, 
             height = "200px", 
             width = "150px", 
             style = "border: 2px solid #ddd; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
  })
  
  # Function to generate the player image URL
  generate_image_url <- function(player_id) {
    base_url <- "https://img.mlbstatic.com/mlb-photos/image/upload/d_people:generic:headshot:67:current.png/w_426,q_auto:best/v1/people/"
    paste0(base_url, player_id, "/headshot/67/current")
  }
  
  # Render the player image for the single player tab
  output$player_image_single <- renderUI({
    req(input$player_dropdown)  # Ensure a player is selected
    player_name <- input$player_dropdown
    player_id <- merged_success$playerid[merged_success$Name == player_name]
    
    if (is.na(player_id) || player_id == "") {
      print("Player ID is missing.")
    } else {
      print(paste("Player ID:", player_id))
    }
    
    img_src <- generate_image_url(player_id)
    print(paste("Generated URL:", img_src))
    
    # Adding border and shadow to the image
    tags$img(src = img_src, 
             height = "200px", 
             width = "150px", 
             style = "border: 2px solid #ddd; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
  })
  
  # Render the BONDS Index gauge chart for Player 1
  output$bonds_index_gauge1 <- renderPlotly({
    req(input$player1_dropdown)
    player_name <- input$player1_dropdown
    player_data <- bonds_index_data[bonds_index_data$Name == player_name, ]
    
    if (nrow(player_data) == 0) {
      return(NULL)
    }
    
    bonds_index_value <- player_data$BONDS.Index
    min_bonds <- min(bonds_index_data$BONDS.Index)
    max_bonds <- max(bonds_index_data$BONDS.Index)
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = bonds_index_value,
      title = list(text = "BONDS Index", font = list(size = 17)),
      gauge = list(
        axis = list(range = list(min_bonds, max_bonds)),
        bar = list(color = "black"),
        steps = list(
          list(range = c(min_bonds, min_bonds + (max_bonds - min_bonds) * 0.125), color = "#2066ac"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.125, min_bonds + (max_bonds - min_bonds) * 0.25), color = "#70acd0"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.25, min_bonds + (max_bonds - min_bonds) * 0.375), color = "#adcfe4"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.375, min_bonds + (max_bonds - min_bonds) * 0.5), color = "#e3eded"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.5, min_bonds + (max_bonds - min_bonds) * 0.625), color = "#fbe8d7"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.625, min_bonds + (max_bonds - min_bonds) * 0.75), color = "#fdd1bb"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.75, min_bonds + (max_bonds - min_bonds) * 0.875), color = "#cd5041"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.875, max_bonds), color = "#B2172c")
        ),
        threshold = list(
          line = list(color = "black", width = 4),
          thickness = 1,
          value = bonds_index_value
        ),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "gray"
      )
    ) %>%
      layout(
        margin = list(l = 45, r = 75, t = 45, b = 45)
      )
  })
  
  # Render the BONDS Index gauge chart for Player 2
  output$bonds_index_gauge2 <- renderPlotly({
    req(input$player2_dropdown)
    player_name <- input$player2_dropdown
    player_data <- bonds_index_data[bonds_index_data$Name == player_name, ]
    
    if (nrow(player_data) == 0) {
      return(NULL)
    }
    
    bonds_index_value <- player_data$BONDS.Index
    min_bonds <- min(bonds_index_data$BONDS.Index)
    max_bonds <- max(bonds_index_data$BONDS.Index)
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = bonds_index_value,
      title = list(text = "BONDS Index", font = list(size = 17)),
      gauge = list(
        axis = list(range = list(min_bonds, max_bonds)),
        bar = list(color = "black"),
        steps = list(
          list(range = c(min_bonds, min_bonds + (max_bonds - min_bonds) * 0.125), color = "#2066ac"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.125, min_bonds + (max_bonds - min_bonds) * 0.25), color = "#70acd0"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.25, min_bonds + (max_bonds - min_bonds) * 0.375), color = "#adcfe4"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.375, min_bonds + (max_bonds - min_bonds) * 0.5), color = "#e3eded"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.5, min_bonds + (max_bonds - min_bonds) * 0.625), color = "#fbe8d7"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.625, min_bonds + (max_bonds - min_bonds) * 0.75), color = "#fdd1bb"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.75, min_bonds + (max_bonds - min_bonds) * 0.875), color = "#cd5041"),
          list(range = c(min_bonds + (max_bonds - min_bonds) * 0.875, max_bonds), color = "#B2172c")
        ),
        threshold = list(
          line = list(color = "black", width = 4),
          thickness = 1,
          value = bonds_index_value
        ),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "gray"
      )
    ) %>%
      layout(
        margin = list(l = 45, r = 75, t = 45, b = 45)
      )
  })
  
  # Render the player image for the multi-player tab (Player 1)
  output$player_image_multi_1 <- renderUI({
    req(input$player1_dropdown)  # Ensure a player is selected
    player_name <- input$player1_dropdown
    player_id <- merged_success$playerid[merged_success$Name == player_name]
    
    if (is.na(player_id) || player_id == "") {
      print("Player ID is missing.")
    } else {
      print(paste("Player ID:", player_id))
    }
    
    img_src <- generate_image_url(player_id)
    print(paste("Generated URL:", img_src))
    
    tags$img(src = img_src, 
             height = "200px", 
             width = "150px", 
             style = "border: 2px solid #ddd; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
  })
  
  # Render the player image for the multi-player tab (Player 2)
  output$player_image_multi_2 <- renderUI({
    req(input$player2_dropdown)  # Ensure a player is selected
    player_name <- input$player2_dropdown
    player_id <- merged_success$playerid[merged_success$Name == player_name]
    
    if (is.na(player_id) || player_id == "") {
      print("Player ID is missing.")
    } else {
      print(paste("Player ID:", player_id))
    }
    
    img_src <- generate_image_url(player_id)
    print(paste("Generated URL:", img_src))
    
    tags$img(src = img_src, 
             height = "200px", 
             width = "150px", 
             style = "border: 2px solid #ddd; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);")
  })
  
  # Define consistent cluster colors
  cluster_colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')
  
  # Adjust cluster numbering to start from 1
  node_data$Cluster <- as.factor(as.numeric(node_data$Cluster) + 1)
  
  # Perform PCA
  pca <- prcomp(merged_success[, metrics_columns], scale. = TRUE)
  pca_result <- pca$x
  
  # Perform GMM clustering
  optimal_clusters <- 10
  gmm <- Mclust(pca_result[, 1:2], G = optimal_clusters)
  labels <- gmm$classification
  
  # Verify the number of clusters
  cat("Number of clusters:", length(unique(labels)), "\n")
  cat("Cluster sizes:", table(labels), "\n")
  
  # Add the PCA components and cluster labels to the merged_success data
  merged_success$PCA1 <- pca_result[, 1]
  merged_success$PCA2 <- pca_result[, 2]
  merged_success$Cluster <- labels
  
  # Reactive expression to filter the data based on selected clusters
  filtered_data <- reactive({
    req(input$selected_clusters)  # Ensure that the input is available
    selected_clusters <- input$selected_clusters
    if (is.null(selected_clusters) || length(selected_clusters) == 0) {
      return(merged_success)
    }
    filtered <- merged_success %>% filter(Cluster %in% selected_clusters)
    cat("Filtered data cluster sizes:", table(filtered$Cluster), "\n")
    filtered
  })
  
  observeEvent(input$reset_filters, {
    lapply(filtered_metrics_columns, function(metric) {
      updateSliderInput(session, paste0("weight_", metric), value = 1)
    })
  })
  
  observeEvent(input$select_all_clusters, {
    updateCheckboxGroupInput(session, "selected_clusters", selected = 1:10)
  })
  
  observeEvent(input$unselect_all_clusters, {
    updateCheckboxGroupInput(session, "selected_clusters", selected = character(0))
  })
  
  # Render the clustering plot
  output$gmm_cluster_graph <- renderPlotly({
    # Perform PCA again (if not using the globally defined pca_result)
    pca <- prcomp(merged_success[, metrics_columns], scale. = TRUE)
    pca_result <- pca$x
    
    # Perform GMM clustering
    optimal_clusters <- 10
    gmm <- Mclust(pca_result[, 1:2], G = optimal_clusters)
    labels <- as.factor(gmm$classification)  # Keep the original classification for plotting
    
    # Explained variance ratio for PCA components
    explained_variance_ratio <- pca$sdev^2 / sum(pca$sdev^2)
    
    # Convert PCA results and GMM clustering to Plotly-friendly format
    plot_data <- data.frame(
      PCA1 = pca_result[, 1],
      PCA2 = pca_result[, 2],
      Cluster = as.factor(labels),
      PlayerName = merged_success$Name
    )
    
    # Function to draw ellipses for each cluster with specified colors
    draw_ellipse <- function(p, x, y, cluster, plot_data) {
      cluster_data <- plot_data[plot_data$Cluster == cluster, ]
      if (nrow(cluster_data) < 2) return(p)
      cov_matrix <- cov(cluster_data[, c("PCA1", "PCA2")])
      mean_values <- colMeans(cluster_data[, c("PCA1", "PCA2")])
      color <- cluster_colors[as.integer(cluster)]
      for (nsig in 1:3) {
        ellipse_x <- mean_values[1] + nsig * sqrt(cov_matrix[1, 1]) * cos(seq(0, 2 * pi, length.out = 100))
        ellipse_y <- mean_values[2] + nsig * sqrt(cov_matrix[2, 2]) * sin(seq(0, 2 * pi, length.out = 100))
        p <- p %>%
          add_trace(
            type = 'scatter',
            mode = 'lines',
            x = ellipse_x,
            y = ellipse_y,
            line = list(color = color, dash = 'dot'),
            fill = 'tozeroy',  # Fill the ellipse with color
            fillcolor = paste0(color, "33"),  # Add transparency to fill color
            showlegend = FALSE,
            inherit = FALSE
          )
      }
      p
    }
    
    # Create the plot
    p <- plot_ly(
      data = plot_data,
      x = ~PCA1,
      y = ~PCA2,
      type = 'scatter',
      mode = 'markers+text',
      text = ~PlayerName,
      textfont = list(color = '#000000'),
      color = ~Cluster,
      colors = cluster_colors,
      hoverinfo = 'text',
      marker = list(size = 10),
      source = "cluster_graph"  # Ensure this matches the plotly output ID
    ) %>%
      layout(
        title = 'GMM Clustering with Ellipses',
        xaxis = list(title = sprintf('PCA Component 1 (Accounts for %.2f%% of Variance)', explained_variance_ratio[1] * 100)),
        yaxis = list(title = sprintf('PCA Component 2 (Accounts for %.2f%% of Variance)', explained_variance_ratio[2] * 100)),
        autosize = TRUE,
        height = 1000,  # Set the height of the plot
        width = 1400,   # Set the width of the plot
        margin = list(t = 50, b = 50, l = 50, r = 50),  # Adjust margins to fit better
        legend = list(
          title = list(text = 'Clusters'),
          itemsizing = 'constant',
          font = list(size = 12),
          bgcolor = 'rgba(255, 255, 255, 0.5)',  # Add a background color with transparency
          bordercolor = 'black',
          borderwidth = 1
        )
      ) %>%
      colorbar(title = NULL)  # Remove the color bar
    
    # Add ellipses to the plot
    for (cluster in unique(plot_data$Cluster)) {
      p <- draw_ellipse(p, pca_result[, 1], pca_result[, 2], cluster, plot_data)
    }
    
    p
  })
  
  
  # Handle multi-player comparison
  observeEvent(c(input$player1_dropdown, input$player2_dropdown), {
    req(input$player1_dropdown, input$player2_dropdown)
    
    player1_data <- reactive({
      merged_success %>%
        filter(Name == input$player1_dropdown) %>%
        mutate(across(all_of(metrics_columns), ~ as.numeric(.)))
    })
    
    player2_data <- reactive({
      merged_success %>%
        filter(Name == input$player2_dropdown) %>%
        mutate(across(all_of(metrics_columns), ~ as.numeric(.)))
    })
    
    # Function to calculate radar data
    calculate_radar_data <- function(player_data, category_features, weights, merged_success) {
      radar_data <- sapply(category_features, function(metrics) {
        scores <- sapply(metrics, function(metric) {
          value <- player_data[[metric]]
          if (is.null(value)) return(NA)
          
          # Normalize the metric within its range across the dataset
          max_val <- max(merged_success[[metric]], na.rm = TRUE)
          min_val <- min(merged_success[[metric]], na.rm = TRUE)
          normalized_value <- (value - min_val) / (max_val - min_val)
          
          # Invert the value if lower is better
          if (metric %in% metrics_better_when_lower) {
            normalized_value <- 1 - normalized_value
          }
          
          # Apply the weight to the normalized value
          normalized_value * weights[metric]
        })
        mean(scores, na.rm = TRUE)  # Average the scores for the category
      })
      return(radar_data)
    }
    
    output$comparison_graph <- renderPlotly({
      req(player1_data(), player2_data())
      
      # Calculate radar data for both players
      radar_data1 <- calculate_radar_data(player1_data(), category_features, weights, merged_success)
      radar_data2 <- calculate_radar_data(player2_data(), category_features, weights, merged_success)
      
      # Close the loop for the radar chart
      radar_data1 <- c(radar_data1, radar_data1[1])
      radar_data2 <- c(radar_data2, radar_data2[1])
      radar_categories <- c("Power", "Swing Decision", "Bat-to-Ball", "Baserunning", "Power")
      
      formatted_scores1 <- sprintf("%.2f%%", radar_data1 * 100)
      formatted_scores2 <- sprintf("%.2f%%", radar_data2 * 100)
      
      plot_ly() %>%
        add_trace(
          type = 'scatterpolar',
          mode = 'lines+markers',
          r = pmin(as.numeric(radar_data1), 1),
          theta = radar_categories,
          fill = 'toself',
          line = list(color = '#1f77b4'),
          marker = list(color = '#1f77b4'),
          fillcolor = 'rgba(31, 119, 180, 0.5)',
          text = paste(radar_categories, formatted_scores1, sep = ": "),
          hoverinfo = 'text',
          hovertemplate = '<b>%{text}</b><extra></extra>',
          name = input$player1_dropdown
        ) %>%
        add_trace(
          type = 'scatterpolar',
          mode = 'lines+markers',
          r = pmin(as.numeric(radar_data2), 1),
          theta = radar_categories,
          fill = 'toself',
          line = list(color = '#ff7f0e'),
          marker = list(color = '#ff7f0e'),
          fillcolor = 'rgba(255, 127, 14, 0.5)',
          text = paste(radar_categories, formatted_scores2, sep = ": "),
          hoverinfo = 'text',
          hovertemplate = '<b>%{text}</b><extra></extra>',
          name = input$player2_dropdown
        ) %>%
        layout(
          polar = list(
            radialaxis = list(range = c(0, 1)),
            angularaxis = list(tickfont = list(size = 10)),
            domain = list(x = c(0.145, 1))  # Shift chart to the right
          ),
          showlegend = TRUE,
          margin = list(t = 50, l = 50)  # Add left margin
        ) %>%
        plotly::config(displayModeBar = FALSE,
                       modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d"),
                       displaylogo = FALSE)
    })
    
    output$player1_metrics <- renderUI({
      req(player1_data())
      
      metric_boxes <- lapply(metrics_columns, function(metric) {
        value <- player1_data()[[metric]]
        exact_percentile <- ecdf(merged_success[[metric]])(value) * 100
        
        if (metric %in% metrics_better_when_lower) {
          exact_percentile <- 100 - exact_percentile
        }
        
        percentile_index <- findInterval(exact_percentile, seq(0, 100, length.out = 101), all.inside = TRUE)
        color_index <- min(percentile_index, length(color_gradient(100)))
        
        background_color <- color_gradient(100)[color_index]
        font_color <- "black"
        
        div(class = "metric-box", style = sprintf("background-color: %s; color: %s;", background_color, font_color),
            tags$div(
              if (metric == 'xwOBA') {
                sprintf("%.3f", round(value, 3))
              } else if (metric %in% c('HardHit%', 'BrlsBBE%', 'HRFB%', 'K%', 'BB%', 'Swing%', 'SwStr%', 'ZSwing%', 'ZContact%', 'OSwing%', 'OContact%')) {
                sprintf("%.1f%%", round(value, 2))
              } else if (metric == '50thmaxvelo') {
                sprintf("%.1f", round(value, 2))
              } else {
                round(value, 2)
              }
            ),
            tags$div(display_names[metric], style = "font-size: 12px; line-height: 1.2; text-align: center; vertical-align: middle;"),
            title = paste("MLB Percentile:", round(exact_percentile, 1), "%")
        )
      })
      
      tagList(
        div(class = "metric-container",
            metric_boxes[1], metric_boxes[2], metric_boxes[3], metric_boxes[4],  # xwOBA, wRC+, BsR, Spd
            metric_boxes[5], metric_boxes[6], metric_boxes[7], metric_boxes[8],  # OFF, PPA, 50th Max Velo, Brls/BBE%
            metric_boxes[9], metric_boxes[10], metric_boxes[11], metric_boxes[12],  # Swing%, SwStr%, Hard Hit%, HR/FB%
            metric_boxes[13], metric_boxes[14], metric_boxes[15], metric_boxes[16],  # Z-Swing%, Z-Contact%, O-Swing%, O-Contact%
            metric_boxes[17], metric_boxes[18]  # K%, BB%
        )
      )
    })
    
    output$player2_metrics <- renderUI({
      req(player2_data())
      
      metric_boxes <- lapply(metrics_columns, function(metric) {
        value <- player2_data()[[metric]]
        exact_percentile <- ecdf(merged_success[[metric]])(value) * 100
        
        if (metric %in% metrics_better_when_lower) {
          exact_percentile <- 100 - exact_percentile
        }
        
        percentile_index <- findInterval(exact_percentile, seq(0, 100, length.out = 101), all.inside = TRUE)
        color_index <- min(percentile_index, length(color_gradient(100)))
        
        background_color <- color_gradient(100)[color_index]
        font_color <- "black"
        
        div(class = "metric-box", style = sprintf("background-color: %s; color: %s;", background_color, font_color),
            tags$div(
              if (metric == 'xwOBA') {
                sprintf("%.3f", round(value, 3))
              } else if (metric %in% c('HardHit%', 'BrlsBBE%', 'HRFB%', 'K%', 'BB%', 'Swing%', 'SwStr%', 'ZSwing%', 'ZContact%', 'OSwing%', 'OContact%')) {
                sprintf("%.1f%%", round(value, 2))
              } else if (metric == '50thmaxvelo') {
                sprintf("%.1f", round(value, 2))
              } else {
                round(value, 2)
              }
            ),
            tags$div(display_names[metric], style = "font-size: 12px; line-height: 1.2; text-align: center; vertical-align: middle;"),
            title = paste("MLB Percentile:", round(exact_percentile, 1), "%")
        )
      })
      
      tagList(
        div(class = "metric-container",
            metric_boxes[1], metric_boxes[2], metric_boxes[3], metric_boxes[4],  # xwOBA, wRC+, BsR, Spd
            metric_boxes[5], metric_boxes[6], metric_boxes[7], metric_boxes[8],  # OFF, PPA, 50th Max Velo, Brls/BBE%
            metric_boxes[9], metric_boxes[10], metric_boxes[11], metric_boxes[12],  # Swing%, SwStr%, Hard Hit%, HR/FB%
            metric_boxes[13], metric_boxes[14], metric_boxes[15], metric_boxes[16],  # Z-Swing%, Z-Contact%, O-Swing%, O-Contact%
            metric_boxes[17], metric_boxes[18]  # K%, BB%
        )
      )
    })
  })
  
  # Perform PCA and add PCA1 and PCA2 to node_data
  pca_result <- prcomp(node_data[, c('BsR', 'Spd', 'X50th_max_velo', 'Brls.BBE.', 'Swing.', 'SwStr.', 'HardHit.', 'HR.FB.', 'Z.Swing.', 'Z.Contact.', 'O.Swing.', 'O.Contact.')], scale. = TRUE)
  node_data$PCA1 <- pca_result$x[, 1]
  node_data$PCA2 <- pca_result$x[, 2]
  
  # Function to calculate weighted distances with column mapping
  calculate_weighted_pca <- function(data, filtered_metrics_columns, weights) {
    print("Inside calculate_weighted_pca function")
    
    # Apply weights to the selected data
    weighted_data <- data[, filtered_metrics_columns] * weights
    print("Weighted Data:")
    print(head(weighted_data))
    
    # Perform PCA on the weighted data
    pca <- prcomp(weighted_data, scale. = TRUE)
    pca_result <- pca$x
    
    return(pca_result)
  }
  
  # Render the network graph with filtering functionality
  output$cluster_network_graph <- renderPlotly({
    print("Inside renderPlotly function")
    
    # Get the current weights from the sliders
    weights <- sapply(filtered_metrics_columns, function(metric) input[[paste0("weight_", metric)]])
    print("Weights from Sliders:")
    print(weights)
    
    # Recalculate the PCA coordinates with weighted data
    pca_result <- calculate_weighted_pca(merged_success, filtered_metrics_columns, weights)
    node_data$PCA1 <- pca_result[, 1]
    node_data$PCA2 <- pca_result[, 2]
    
    # Filter node_data based on selected clusters
    filtered_node_data <- node_data %>% filter(Cluster %in% input$selected_clusters)
    
    plot_ly(type = 'scatter', mode = 'markers') %>%
      add_markers(
        x = filtered_node_data$PCA1,
        y = filtered_node_data$PCA2,
        text = filtered_node_data$Name,
        color = filtered_node_data$Cluster,
        colors = cluster_colors,
        hoverinfo = 'text',
        marker = list(size = 15)
      ) %>%
      layout(
        title = 'MLB Hitters Cluster Network (2023 Season)',
        xaxis = list(title = 'PCA Component 1'),
        yaxis = list(title = 'PCA Component 2'),
        legend = list(
          title = list(text = 'Clusters'),
          itemsizing = 'constant',
          font = list(size = 12),
          bgcolor = 'rgba(255, 255, 255, 0.5)',  # Add a background color with transparency
          bordercolor = 'black',
          borderwidth = 1
        ),
        showlegend = TRUE,
        height = 1000,  # Increased height
        width = 1400   # Increased width
      ) %>%
      add_trace(
        type = 'scatter',
        mode = 'lines',
        x = c(rbind(node_data$PCA1[match(edge_data$source, node_data$Name)], node_data$PCA1[match(edge_data$target, node_data$Name)], NA)),
        y = c(rbind(node_data$PCA2[match(edge_data$source, node_data$Name)], node_data$PCA2[match(edge_data$target, node_data$Name)], NA)),
        line = list(color = 'rgba(0, 0, 0, 0.1)', width = 1),
        showlegend = FALSE,
        hoverinfo = 'none'
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  # Render the adjusted metrics table with color formatting and cluster information
  output$adjusted_metrics_table <- DT::renderDataTable({
    # Get the current weights from the sliders
    weights <- sapply(filtered_metrics_columns, function(metric) input[[paste0("weight_", metric)]])
    
    # Calculate the weighted data
    weighted_data <- merged_success[, filtered_metrics_columns] * weights
    
    # Calculate the difference between original and weighted values
    difference_data <- weighted_data - merged_success[, filtered_metrics_columns]
    
    # Combine into a data frame for display
    adjusted_metrics_table <- data.frame(
      Name = merged_success$Name,
      Cluster = merged_success$Cluster,  # Add cluster information
      Original = round(apply(merged_success[, filtered_metrics_columns], 1, sum), 2),
      Adjusted = round(apply(weighted_data, 1, sum), 2),
      Difference = round(apply(difference_data, 1, sum), 2)
    )
    
    # Create the datatable with color formatting
    datatable <- DT::datatable(adjusted_metrics_table, options = list(pageLength = 25, scrollX = TRUE)) %>%
      formatStyle(
        'Difference',
        backgroundColor = styleInterval(c(-1e-10, 1e-10), c('red', 'white', 'green')),
        color = styleInterval(c(-1e-10, 1e-10), c('white', 'black', 'white'))
      ) %>%
      formatStyle('Name', cursor = 'pointer')
    
    datatable
  })
  
  # Render the player stats dashboard
  output$stats_table <- DT::renderDataTable({
    # Remove columns containing "cleaned" and also remove the index column
    filtered_data <- merged_success %>% select(-contains("cleaned"))
    
    # Create the datatable with options to freeze the "Name" column (second column)
    DT::datatable(filtered_data, options = list(
      pageLength = 25,
      scrollX = TRUE,
      columnDefs = list(list(visible = FALSE, targets = 0)),  # Hide the index column
      fixedColumns = list(leftColumns = 1)  # Freeze the "Name" column (first visible column)
    ), extensions = 'FixedColumns')
  })
  
  # Handle single player selection
  observeEvent(input$player_dropdown, {
    req(input$player_dropdown)
    
    # Reactive data based on player selection
    player_data <- reactive({
      merged_success %>%
        filter(Name == input$player_dropdown) %>%
        mutate(across(all_of(metrics_columns), ~ as.numeric(.)))
    })
    
    # Generate the radar chart
    output$player_graph <- renderPlotly({
      req(player_data())
      
      radar_data <- sapply(category_features, function(metrics) {
        scores <- sapply(metrics, function(metric) {
          value <- player_data()[[metric]]
          if (is.null(value)) return(NA)
          max_val <- max(merged_success[[metric]], na.rm = TRUE)
          min_val <- min(merged_success[[metric]], na.rm = TRUE)
          normalized_value <- (value - min_val) / (max_val - min_val)
          if (metric %in% metrics_better_when_lower) {
            normalized_value <- 1 - normalized_value
          }
          normalized_value * weights[metric]
        })
        mean(scores, na.rm = TRUE)
      })
      
      radar_data <- c(radar_data, radar_data[1])
      radar_categories <- c("Power", "Swing Decision", "Bat-to-Ball", "Baserunning", "Power")
      
      formatted_scores <- sprintf("%.2f%%", radar_data * 100)
      
      plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = pmin(as.numeric(radar_data), 1),
        theta = radar_categories,
        fill = 'toself',
        line = list(color = '#1f77b4'),
        marker = list(color = '#1f77b4'),
        fillcolor = 'rgba(31, 119, 180, 0.5)',
        text = paste(radar_categories, formatted_scores, sep = ": "),
        hoverinfo = 'text',
        hovertemplate = '<b>%{text}</b><extra></extra>',
        showlegend = FALSE
      ) %>%
        layout(
          polar = list(
            radialaxis = list(range = c(0, 1)),
            angularaxis = list(tickfont = list(size = 10))
          ),
          showlegend = FALSE,
          title = list(
            text = paste("Radar Chart for:", input$player_dropdown),
            y = 1.1,
            yanchor = "top",
            xanchor = "center",
            x = 0.535
          ),
          margin = list(t = 100)
        ) %>%
        plotly::config(displayModeBar = FALSE,
                       modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d"),
                       displaylogo = FALSE)
    })
    
    # Generate the individual metric boxes for single player
    output$metrics_container_single <- renderUI({
      req(player_data())
      
      metric_boxes <- lapply(metrics_columns, function(metric) {
        value <- player_data()[[metric]]
        exact_percentile <- ecdf(merged_success[[metric]])(value) * 100
        
        if (metric %in% metrics_better_when_lower) {
          exact_percentile <- 100 - exact_percentile
        }
        
        percentile_index <- findInterval(exact_percentile, seq(0, 100, length.out = 101), all.inside = TRUE)
        color_index <- min(percentile_index, length(color_gradient(100)))
        
        background_color <- color_gradient(100)[color_index]
        font_color <- "black"
        
        div(class = "metric-box", style = sprintf("background-color: %s; color: %s;", background_color, font_color),
            tags$div(
              if (metric == 'xwOBA') {
                sprintf("%.3f", round(value, 3))
              } else if (metric %in% c('HardHit%', 'BrlsBBE%', 'HRFB%', 'K%', 'BB%', 'Swing%', 'SwStr%', 'ZSwing%', 'ZContact%', 'OSwing%', 'OContact%')) {
                sprintf("%.1f%%", round(value, 2))
              } else if (metric == '50thmaxvelo') {
                sprintf("%.1f", round(value, 2))
              } else {
                round(value, 2)
              }
            ),
            tags$div(display_names[metric], style = "font-size: 12px; line-height: 1.2; text-align: center; vertical-align: middle;"),
            title = paste("MLB Percentile:", round(exact_percentile, 1), "%")
        )
      })
      
      tagList(
        div(class = "metric-container",
            metric_boxes[1], metric_boxes[2], metric_boxes[3], metric_boxes[4],  # xwOBA, wRC+, BsR, Spd
            metric_boxes[5], metric_boxes[6], metric_boxes[7], metric_boxes[8],  # OFF, PPA, 50th Max Velo, Brls/BBE%
            metric_boxes[9], metric_boxes[10], metric_boxes[11], metric_boxes[12],  # Swing%, SwStr%, Hard Hit%, HR/FB%
            metric_boxes[13], metric_boxes[14], metric_boxes[15], metric_boxes[16],  # Z-Swing%, Z-Contact%, O-Swing%, O-Contact%
            metric_boxes[17], metric_boxes[18]  # K%, BB%
        )
      )
    })
  })
}

shinyApp(ui, server)