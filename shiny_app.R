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

# Load and prepare data
merged_success <- read.csv("/Users/stephenak24/Downloads/merged_success_v2.csv", check.names = FALSE)
node_data <- read.csv("/Users/stephenak24/Downloads/node_data.csv")
edge_data <- read.csv("/Users/stephenak24/Downloads/mst_edges.csv")

# Clean up column names: remove non-alphanumeric characters except for % and +, replace / with _
colnames(merged_success) <- gsub("[^[:alnum:]%+]", "", gsub("/", "_", colnames(merged_success)))

# Define metric columns with the cleaned column names
metrics_columns <- c('xwOBA', 'wRC+', 'BsR', 'Spd', 'OFF', 'PPA', '50thmaxvelo', 'BrlsBBE%', 'Swing%', 'SwStr%', 'HardHit%', 'HRFB%', 'ZSwing%', 'ZContact%', 'OSwing%', 'OContact%', 'K%', 'BB%')

# Mapping for display names
display_names <- c(
  'xwOBA' = 'xwOBA',
  'wRC+' = 'wRC+',
  'BsR' = 'BsR',
  'Spd' = 'Spd',
  'OFF' = 'OFF',
  'PPA' = 'P/PA',
  '50thmaxvelo' = '50th Max Velo%',
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
print(all(metrics_columns %in% colnames(merged_success)))  # Should return TRUE if all are correct

# Proceed with data processing using the cleaned column names
merged_success <- merged_success %>%
  mutate(across(all_of(metrics_columns), ~ as.numeric(as.character(.))))

# Define metrics that are better when lower
metrics_better_when_lower <- c('K%', 'SwStr%', 'OSwing%')

# Define the color scale
base_colors <- c('#2066ac', '#70acd0', '#adcfe4', '#e3eded', '#fbe8d7', '#fdd1bb', '#cd5041', '#B2172c')
color_gradient <- colorRampPalette(base_colors)

# Compute percentiles for each metric
percentiles <- sapply(metrics_columns, function(m) {
  quantile(merged_success[[m]], probs = seq(0, 1, length.out = 100))
}, simplify = FALSE)

# Create a mapping from metrics_columns to actual column names in node_data
actual_columns <- c(
  'xwOBA' = NA,       # Update with actual name if exists
  'wRC+' = NA,        # Update with actual name if exists
  'BsR' = "BsR",
  'Spd' = "Spd",
  'OFF' = NA,         # Update with actual name if exists
  'PPA' = NA,         # Update with actual name if exists
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
  'K%' = NA,          # Update with actual name if exists
  'BB%' = NA          # Update with actual name if exists
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

# UI definition using Shiny and ShinyDashboard
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = tags$div(style = "width: 100%; display: flex; justify-content: space-between;", 
                     tags$span("MLB Batter Analysis Dashboard"), 
                     tags$span("|", style = "margin-left: 10px; margin-right: 10px;"),
                     tags$span("Stephen Akwaowo", style = "margin-right: 15px;")),
    titleWidth = 500  # Increased width to accommodate longer titles
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",  # Ensure this id is set to "tabs"
      menuItem("Batter Cards (2023)", tabName = "generate", icon = icon("baseball"),
               menuSubItem("Single Player", tabName = "single_player"),
               menuSubItem("Multi Player Compare", tabName = "multi_player")
      ),
      menuItem("Batter Clusters (2023)", tabName = "clusters", icon = icon("sitemap"),
               menuSubItem("GMM Cluster Model", tabName = "gmm_cluster_model"),
               menuSubItem("Network Graph", tabName = "network_graph")
      ),
      menuItem("Player Stats Dashboard (2023)", tabName = "stats_dashboard", icon = icon("table")),
      menuItem("Data Sources", tabName = "data_sources", icon = icon("database")),
      menuItem("About the Project", tabName = "about_project", icon = icon("info-circle")),
      menuItem("Methodology", tabName = "methodology", icon = icon("file-alt"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "shiny_app.css"),  # Link to your custom CSS file
      tags$script(src = "https://cdn.datatables.net/fixedcolumns/4.0.1/js/dataTables.fixedColumns.min.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.datatables.net/fixedcolumns/4.0.1/css/fixedColumns.dataTables.min.css"),
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
      "))
    ),
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
                                choices = sort(unique(merged_success$Name))),
                    p("Select a player from the dropdown to view detailed metrics and performance visualizations.")
                ),
                div(id = "capture_area",  # Add id for capturing the screenshot
                    box(title = "Player Metrics", width = 12, solidHeader = FALSE, color = "purple", 
                        plotlyOutput("player_graph"),
                        uiOutput("metrics_container_single")
                    )
                ),
                downloadButton("downloadPlot", "Download Plot")
              )
      ),
      tabItem(tabName = "multi_player",
              fluidRow(
                box(title = "Select Players", width = 12, 
                    selectInput("player1_dropdown", "Player 1", choices = sort(unique(merged_success$Name))),
                    selectInput("player2_dropdown", "Player 2", choices = sort(unique(merged_success$Name))),
                    p("Select two players to compare their metrics and performance.")
                ),
                box(title = "Comparison Metrics", width = 12, solidHeader = FALSE, color = "purple", 
                    plotlyOutput("comparison_graph"),
                    fluidRow(
                      column(6, uiOutput("player1_metrics")),
                      column(6, uiOutput("player2_metrics"))
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
                box(title = "Player Stats Dashboard (2023)", width = 12, solidHeader = FALSE, color = "purple", 
                    p("This dashboard displays the key statistics of players for the 2023 season. Use the table below to explore and sort through various player metrics."),
                    DT::dataTableOutput("stats_table")
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
                      )
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
      ),
      tabItem(tabName = "methodology",
              fluidRow(
                box(title = "Methodology and Explanation", width = 12, 
                    h3("Sequential Dimensionality Reduction Strategy"),
                    p("Dimensionality reduction is a critical step in preparing high-dimensional data for clustering. In this project, I employed a two-step sequential dimensionality reduction strategy combining Principal Component Analysis (PCA) and t-Distributed Stochastic Neighbor Embedding (t-SNE)."),
                    h4("Principal Component Analysis (PCA)"),
                    p("PCA transforms the original high-dimensional data into a new set of features called principal components. These components are ranked by the amount of variance they capture from the original data. I selected the number of components to retain based on capturing approximately 91% of the total variance."),
                    h4("t-Distributed Stochastic Neighbor Embedding (t-SNE)"),
                    p("t-SNE is a non-linear dimensionality reduction technique that preserves local relationships between data points. After reducing the dimensionality with PCA, t-SNE was applied to compress and visualize the data in two dimensions. Multiple runs of t-SNE were conducted to find the best representation based on the Kullback-Leibler (KL) divergence score."),
                    h3("GMM Cluster Algorithm"),
                    p("Gaussian Mixture Models (GMM) is a probabilistic model used for clustering data points into groups or clusters. GMM represents each cluster as a Gaussian distribution, allowing for more flexibility compared to traditional clustering algorithms."),
                    h4("Defining the Number of Clusters"),
                    p("The optimal number of clusters was determined using multiple criteria:"),
                    tags$ul(
                      tags$li("Akaike Information Criterion (AIC) and Bayesian Information Criterion (BIC): These metrics helped to evaluate the models with different numbers of clusters, aiming to minimize both AIC and BIC values."),
                      tags$li("Silhouette Score: This score assesses the quality of clusters formed, with higher values indicating better-defined clusters. It confirmed the optimal cluster count suggested by AIC/BIC."),
                      tags$li("Median Distances: Analyzing the median pairwise distances within clusters provided additional insights into cluster tightness and separation.")
                    ),
                    h3("Radar Charts"),
                    p("Radar charts are used to visually compare the performance of players across different clusters. Each axis of the radar chart represents a category of metrics:"),
                    tags$ul(
                      tags$li("Power: Includes metrics like 50th max velo, barrels per batted ball event, and home run per fly ball rate."),
                      tags$li("Swing Decision: Encompasses metrics like out-of-zone swing percentage and walk percentage."),
                      tags$li("Bat-to-Ball: Includes metrics like contact percentage and strikeout percentage."),
                      tags$li("Baserunning: Consists of metrics like speed and baserunning runs.")
                    ),
                    p("The mean values for each category were calculated for each cluster, and the resulting radar charts provide a clear and intuitive visualization of the differences and similarities among clusters. Adjustments were made to ensure meaningful comparisons, including the application of weights to certain metrics and normalization of values to a common scale."),
                    p("The visualizations help to quickly identify the strengths and weaknesses of players within each cluster, making it a powerful tool for analysis and decision-making."),
                    h3("Metric Weighting"),
                    p("Metric weighting is a crucial step to ensure that the metrics contributing to the radar chart are appropriately emphasized based on their importance. In this project, I utilized the correlation to wRC+ to guide the weighting process."),
                    h4("Correlation Analysis"),
                    p("I calculated the correlation between each metric and wRC+. Correlation measures the strength and direction of the linear relationship between two variables. In this context, a higher correlation indicates that a particular metric is more strongly associated with the target performance metric."),
                    h4("Weight Assignment"),
                    p("The calculated correlation values were normalized to ensure that the sum of the weights equals 1. This normalization was done by dividing each correlation value by the total sum of all correlation values. The normalized weights were then applied to the metrics before plotting the radar charts. This weighting ensures that metrics with higher relevance to overall performance have a more significant impact on the visualization."),
                    h4("Implementation"),
                    p("The weights were implemented in the Shiny app, allowing users to adjust the weights interactively through sliders. This interactive feature provides flexibility for users to explore different weighting schemes and observe their effects on the radar charts."),
                    h3("Data Normalization"),
                    p("Data normalization is a fundamental preprocessing step that ensures all metrics are on a comparable scale. This process is essential because the metrics used in the radar charts have different units and ranges."),
                    h4("Standardization"),
                    p("Each metric was standardized to have a mean of 0 and a standard deviation of 1. This was achieved using the z-score normalization, calculated as follows:"),
                    code("Z = (X - µ) / σ"),
                    p("where \\( X \\) is the original metric value, \\( µ \\) is the mean of the metric, and \\( σ \\) is the standard deviation of the metric. Standardization ensures that each metric contributes equally to the analysis, regardless of its original scale or unit."),
                    h4("Min-Max Scaling"),
                    p("Alternatively, metrics were scaled to a range between 0 and 1 using min-max scaling. This method is calculated as follows:"),
                    code("X' = (X - X_min) / (X_max - X_min)"),
                    p("where \\( X' \\) is the normalized value, \\( X \\) is the original value, \\( X_min \\) is the minimum value of the metric, and \\( X_max \\) is the maximum value of the metric. Min-max scaling preserves the relative distances between values, making it suitable for visual comparisons."),
                    h4("Handling Outliers"),
                    p("Outliers can distort the normalization process. To mitigate this, robust scaling techniques were applied, such as using the median and interquartile range (IQR) instead of mean and standard deviation. This approach is less sensitive to extreme values."),
                    h4("Normalization in Practice"),
                    p("The chosen normalization method was applied uniformly across all metrics to maintain consistency. This step was performed before calculating the weighted scores for the radar charts.")
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
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
    # Perform PCA
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
    
    output$comparison_graph <- renderPlotly({
      req(player1_data(), player2_data())
      
      radar_data1 <- sapply(category_features, function(metrics) {
        scores <- sapply(metrics, function(metric) {
          value <- player1_data()[[metric]]
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
      
      radar_data2 <- sapply(category_features, function(metrics) {
        scores <- sapply(metrics, function(metric) {
          value <- player2_data()[[metric]]
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
        config(displayModeBar = FALSE,
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
              } else if (metric %in% c('HardHit%', 'BrlsBBE%', 'HRFB%', '50thmaxvelo', 'K%', 'BB%', 'Swing%', 'SwStr%', 'ZSwing%', 'ZContact%', 'OSwing%', 'OContact%')) {
                sprintf("%.1f%%", round(value, 2))
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
            metric_boxes[5], metric_boxes[6], metric_boxes[7], metric_boxes[8],  # OFF, PPA, 50th Max Velo%, Brls/BBE%
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
              } else if (metric %in% c('HardHit%', 'BrlsBBE%', 'HRFB%', '50thmaxvelo', 'K%', 'BB%', 'Swing%', 'SwStr%', 'ZSwing%', 'ZContact%', 'OSwing%', 'OContact%')) {
                sprintf("%.1f%%", round(value, 2))
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
            metric_boxes[5], metric_boxes[6], metric_boxes[7], metric_boxes[8],  # OFF, PPA, 50th Max Velo%, Brls/BBE%
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
      config(displayModeBar = FALSE)
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
        config(displayModeBar = FALSE,
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
              } else if (metric %in% c('HardHit%', 'BrlsBBE%', 'HRFB%', '50thmaxvelo', 'K%', 'BB%', 'Swing%', 'SwStr%', 'ZSwing%', 'ZContact%', 'OSwing%', 'OContact%')) {
                sprintf("%.1f%%", round(value, 2))
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
            metric_boxes[5], metric_boxes[6], metric_boxes[7], metric_boxes[8],  # OFF, PPA, 50th Max Velo%, Brls/BBE%
            metric_boxes[9], metric_boxes[10], metric_boxes[11], metric_boxes[12],  # Swing%, SwStr%, Hard Hit%, HR/FB%
            metric_boxes[13], metric_boxes[14], metric_boxes[15], metric_boxes[16],  # Z-Swing%, Z-Contact%, O-Swing%, O-Contact%
            metric_boxes[17], metric_boxes[18]  # K%, BB%
        )
      )
    })
  })
}

shinyApp(ui, server)
