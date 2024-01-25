# libraries
pacman::p_load("shiny", "shinyjs", "shinyWidgets", "DT", "readxl", "dplyr", "writexl", "ggplot2", "tidyverse")

# Read the data from the file at the start
dataset <- readxl::read_excel('./data/SO-Methods-Costs-selection.xlsx')
datasetCosts <- readxl::read_excel('./data/eLTER-SO_costs-V021.xlsx')
# changing the name of the variable
datasetCosts <- datasetCosts %>% 
  rename(type = method)

# Custom ggplot2 eLTER theme
# Defining custom colors
color1 <- "#ED9632" 
color2 <- "#0879C0" 
color3 <- "#40907A"
color4 <- "#96CE58"
color5 <- "#BADFFD"

# Named vector for sphere colors
sphere_colors <- c("Atmosphere" = color5, 
                   "Hydrosphere" = color2, 
                   "Geosphere" = color3, 
                   "Biosphere" = color4, 
                   "Sociosphere" = color1)

# creating a fill scale
scale_fill_elter <- function(...) scale_fill_manual(values = c(color1, color2, color3, color4, color5), ...)

# Custom functions
costs_elter <- function(dataset, cat, hab) {
  dataset %>%
    filter(category == cat & habitat == hab & !is.na(type) == TRUE) %>%
    distinct(sphere, code, standard_observation, type)
}

f_spheres <- function(dataset, spheres) {
  df1 <- dataset %>%
    filter(sphere %in% c(spheres))
  df2 <- dataset %>%
    filter(!sphere %in% c(spheres)) %>%
    mutate(type = "basic")
  bind_rows(df1, df2) %>%
    arrange(sphere, code)
}

station_requirements <- function(dataset, cat, hab, spheres) {
  df1 <- tryCatch({
    costs_elter(dataset = dataset, cat = cat, hab = hab)
  }, error = function(e) {
    message("Error in costs_elter:", e)
    data.frame()  # return an empty data frame in case of an error
  })
  f_spheres(dataset = df1, spheres = spheres)
}

# Function to calculate the costs per year
SO_cost <- function(input_code, input_type) {
  # Filter data for the specific code using dplyr::filter
  filtered_data <- datasetCosts %>%
    dplyr::filter(code == input_code, type == input_type)
  
  # Check if the filtered data is empty
  if (nrow(filtered_data) == 0) {
    # Return an empty data frame or a data frame with default/NA values
    return(data.frame(code = input_code, type = input_type, 
                      purchaseCostYear = NA, maintenanceCostYear = NA, 
                      samplingCostYear = NA, labAnalysisCostYear = NA, 
                      totalCostYear = NA))
  }
  
  # Calculate costs using dplyr::mutate
  results <- filtered_data %>%
    dplyr::mutate(
      purchaseCostYear = ifelse(purchasePrice > 0, round(((purchasePrice * minimumSamplePerSite) / upgradeInterval), 0), round((purchasePrice * minimumSamplePerSite), 0)),
      # TODO: Check the maintenance cost formula. I am sure that it is currently wrong. How upgrades should be treated? An upgrade means adding a new purchase or the addition of the maintenance costs? Is the maintenance cost related to the upgradeInterval?
      maintenanceCostYear = round(maintenancePrice * maintenanceEffort, 0), # it should not include the price per site, as it is priced at for the entire bundle
      samplingCostYear = round(samplingPrice * measurementsPerYear, 0), # removed the sampling per sites as it shouldn't affect the calculations
      labAnalysisCostYear = round(labAnalysisPrice * minimumSamplePerSite * measurementsPerYear, 0),
      totalCostYear = round(purchaseCostYear + maintenanceCostYear + samplingCostYear + labAnalysisCostYear, 0)
    ) %>%
    dplyr::select(code, type, purchaseCostYear, maintenanceCostYear, samplingCostYear, labAnalysisCostYear, totalCostYear)
  
  return(results)
}

# Shiny app UI
ui <- fluidPage(
  # Add a header or instructions at the top of the app
  tags$div(
    class = "app-description",
    tags$h2("Welcome to the Standard Observations costs App!"),
    tags$p("This interactive tool helps you define Standard Observations (SO) and calculate associated costs to implement them in your eLTER site. Follow the steps below to use the app:"),
    tags$ul(
      tags$li("Step 1: Select your desired parameters from the sidebar options."),
      tags$li("Step 2: Deselect any SO that your site have already covered by other means. If needed, check the SO codes in the tab: SO codes (for consultation)."),
      tags$li("Step 3: Review the produced SO list."),
      tags$li("Step 4: Download the results of the SO list (table and/or barplot) with the 'Download' buttons."),
      tags$li("Step 5: Go to the tab SO cost calculation and download the detailed table with the costs using the 'Download Cost Calculations' button.")
    ),
    tags$p("This is an unfinished beta product, errors and unexpected results might emerge. If you encounter any issues or have questions, please contact support (allan.souza@helsinki.fi).")
  ),
  
  tags$head(
    tags$style(HTML("
    .tabbable > .nav > li[class=active] > a { background-color: #F26522;  color:white}
    .navbar { background-color: #0073C2; }
    .sidebar { background-color: #F0F0F0; }
    .btn { background-color: #F26522; color: #fff; }
    .btn:hover { background-color: darken(#F26522, 10%); }
        .scrollable-checkbox {
            height: 300px; /* or any other height */
            overflow-y: auto;
            border: 1px solid #ccc;
            padding: 10px;
            border-radius: 5px;
        }
    .app-description { background-color: #226755;
      padding: 20px; 
      border-radius: 8px; 
      margin-bottom: 20px; 
    }
    .app-description h2 {
      color: #9ccb5b;
    }
        .app-description p, .app-description li {
      color: #f9c53e;
    }
    h4 { color: #0073C2; }
    "))
  ),
  
  titlePanel(div(
    style = "text-align: center;",
    img(src = "elter_logo.jpg", height = "150px")
  )),
  
  tabsetPanel(
    tabPanel("Identifying the SO needed",
             sidebarLayout(
               sidebarPanel(
                 h4("Select Parameters:"),
                 selectInput("cat", "eLTER site category", choices = c(1, 2), selected = 1),
                 selectInput("hab", "Site habitat", choices = unique(dataset$habitat)),
                 selectInput("sphere1", "Sphere of specialization #1", choices = unique(dataset$sphere)),
                 selectInput("sphere2", "Sphere of specialization #2", choices = unique(dataset$sphere)),
                 uiOutput("codeSelect"),
                 downloadButton("download", "Download Table", class = "btn"),
                 downloadButton("downloadPlot", "Download Barplot", class = "btn")
               ),
               mainPanel(
                 h4("List of the standard observations needed in your site"),
                 DTOutput("updatedTable"),
                 h4("Number of standard observations needed in your site grouped by sphere and type"),
                 plotOutput("updatedBarPlot")
               )
             )),
             
             tabPanel("SO codes (for consultation)",
                      fluidPage(
                        DTOutput("completeTable")
                      )),
             
    tabPanel("SO cost calculation",
             sidebarLayout(
               sidebarPanel(
                 downloadButton("downloadCosts", "Download Cost Calculations"),
                 downloadButton("downloadTypePlotCosts", "Download Type Cost Plot"),
                 downloadButton("downloadSpherePlot", "Download Sphere Cost Plot")
               ),
               mainPanel(
                 h4("Calculated Costs"),
                 DTOutput("costTable"),
                 h4("Cost Breakdown by Type"),
                 plotOutput("typeCostPlot"),
                 h4("Cost Breakdown by Sphere"),  
                 plotOutput("sphereCostPlot")
               )
             )
    )
    )
    )
    
    server <- function(input, output, session) {
      # Render the table with unique combinations of sphere, code, and standard_observation
      output$completeTable <- renderDT({
        req(dataset)
        unique_data <- dataset %>%
          distinct(sphere, code, standard_observation)
        datatable(unique_data, options = list(pageLength = 68))
      })
      
      # Reactive function to compute station requirements
      station_result <- reactive({
        req(input$cat, input$hab, input$sphere1, input$sphere2)
        station_requirements(
          dataset = dataset,
          cat = input$cat,
          hab = input$hab,
          spheres = c(input$sphere1, input$sphere2)
        )
      })
      
      # Dynamic UI for code deselection
      output$codeSelect <- renderUI({
        req(input$cat, input$hab, input$sphere1, input$sphere2)
        data <- station_requirements(dataset, input$cat, input$hab, c(input$sphere1, input$sphere2))
        codes <- unique(data$code)
        div(
          class = "scrollable-checkbox",
          checkboxGroupInput("selectedCodes", "Uncheck the SO codes that are not needed for your site (i.e. cost already covered by another source)", choices = codes, selected = codes)
        )
      })
      
      observeEvent(input$cat, {
        if (input$cat == 2) {
          # Disable sphere selections when category 2 is selected
          updateSelectInput(session, "sphere1", selected = character(0), choices = character(0))
          updateSelectInput(session, "sphere2", selected = character(0), choices = character(0))
        } else {
          # Enable sphere selections with available choices when category is not 2
          updateSelectInput(session, "sphere1", choices = unique(dataset$sphere), selected = unique(dataset$sphere)[1])
          updateSelectInput(session, "sphere2", choices = unique(dataset$sphere), selected = unique(dataset$sphere)[1])
        }
      })
      
      # Reactive expression for the filtered data
      filtered_data <- reactive({
        req(input$selectedCodes)
        data <- station_requirements(dataset, input$cat, input$hab, c(input$sphere1, input$sphere2))
        data[data$code %in% input$selectedCodes, ]
      })
      
      # Render the updated table based on the filtered data
      output$updatedTable <- renderDT({
        datatable(filtered_data(), options = list(pageLength = 68))
      })
      
      # Render the updated bar plot based on the filtered data
      output$updatedBarPlot <- renderPlot({
        req(filtered_data())
        filtered_data() %>% 
          mutate(type = fct_recode(type,
                                   "Prime" = "prime",
                                   "Basic" = "basic")) %>% 
          ggplot(aes(x = type, fill = sphere)) +
          geom_bar(position = "dodge", alpha = 0.8, col = 'black') +
          labs(x = "Type of Standard Observation", y = "Count", fill = "") +
          coord_flip() +
          scale_fill_manual(values = sphere_colors) +
          # scale_fill_elter() +
          theme_bw() +
          theme(text = element_text(size = 18), legend.position = "bottom")
      })
      
      # DownloadHandler for the table
      output$download <- downloadHandler(
        filename = function() {
          paste0("site_requirements_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          req(filtered_data())
          writexl::write_xlsx(filtered_data(), file)
        }
      )
      
      # DownloadHandler for the bar plot
      output$downloadPlot <- downloadHandler(
        filename = function() {
          paste0("barplot_", Sys.Date(), ".png")
        },
        content = function(file) {
          req(filtered_data())
          g <- filtered_data() %>% 
            mutate(type = fct_recode(type,
                                     "Prime" = "prime",
                                     "Basic" = "basic")) %>% 
            ggplot(aes(x = type, fill = sphere)) +
            geom_bar(position = "dodge", col = 'black') +
            labs(x = "Type of Standard Observation", y = "Count", fill = "Sphere") +
            coord_flip() +
            scale_fill_manual(values = sphere_colors) +
            theme_bw() +
            theme(text = element_text(size = 18),
              legend.position = "bottom")
      
      # Save the ggplot object to file
      ggsave(file, plot = g, width = 11, height = 8, dpi = 300)
    }
  )

      # Reactive expression for cost calculation
      cost_calculated_data <- reactive({
        req(filtered_data())  # Ensure the filtered data is available
        data <- filtered_data()
        
        # Get unique combinations of code and type
        unique_combinations <- unique(data[, c("code", "type")])
        
        # Apply the SO_cost function to each unique combination of code and type
        cost_data <- do.call(rbind, lapply(1:nrow(unique_combinations), function(i) {
          SO_cost(unique_combinations$code[i], unique_combinations$type[i])
        }))
        
        # Replace NA values with zero in selected columns
        cost_data <- cost_data %>%
          mutate(
            purchaseCostYear = ifelse(is.na(purchaseCostYear), 0, purchaseCostYear),
            maintenanceCostYear = ifelse(is.na(maintenanceCostYear), 0, maintenanceCostYear),
            samplingCostYear = ifelse(is.na(samplingCostYear), 0, samplingCostYear),
            labAnalysisCostYear = ifelse(is.na(labAnalysisCostYear), 0, labAnalysisCostYear),
            totalCostYear = ifelse(is.na(totalCostYear), 0, totalCostYear)
          ) %>%
          # Remove rows where all cost fields are zero
          filter(rowSums(select(., purchaseCostYear, maintenanceCostYear, samplingCostYear, labAnalysisCostYear, totalCostYear)) != 0)
        
        return(cost_data)
      })
      
  # Render the calculated costs table
  output$costTable <- renderDT({
    req(cost_calculated_data())  # Ensure the cost data is available
    datatable(cost_calculated_data(), options = list(pageLength = 68))
  })
  
  # DownloadHandler for the cost calculations
  output$downloadCosts <- downloadHandler(
    filename = function() {
      paste0("cost_calculations_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(cost_calculated_data())  # Ensure the cost data is available
      writexl::write_xlsx(cost_calculated_data(), file)
    }
  )
  
  # Render the cost by type plot
  output$typeCostPlot <- renderPlot({
    req(cost_calculated_data())  # Ensure cost data is available
    long_cost_data <- cost_calculated_data() %>%
      tidyr::pivot_longer(names_to = "costType", values_to = "amount", purchaseCostYear:totalCostYear)
    
    long_cost_data %>%
      ungroup() %>% 
      mutate(costType = as_factor(costType)) %>% 
      mutate(costType = fct_recode(costType, 
                                   Purchase = "purchaseCostYear",
                                   Maintenance = "maintenanceCostYear",
                                   "Lab analysis" = "labAnalysisCostYear",
                                   Sampling = "samplingCostYear",
                                   Total = "totalCostYear")) %>% 
      group_by(costType) %>%
      summarise(euro = sum(amount), .groups = "drop") %>%
      ggplot(aes(x = reorder(costType, euro), y = (euro/1000), fill = costType)) +
      geom_bar(stat = "identity", position = position_dodge(), fill = "#226755", col = "black", alpha = 0.8) +
      geom_text(aes(label = round(euro/1000, 1), vjust = -0.35), col = "gray15", size = 14/.pt) +
      # geom_text(position = position_dodge(1.2), col = "gray15", size = 14/.pt,
      #           aes(label = round(euro/1000, 1))) +
      labs(x = "Cost type", y = "Cost in k€") +
      coord_cartesian(clip = "off") +
      theme_bw() +
      theme(text = element_text(size = 18), legend.position = "bottom")
  })
  
  # Render the cost by sphere plot
  output$sphereCostPlot <- renderPlot({
    # Make sure this uses the same filtered data
    req(filtered_data())
    
    # Assuming 'cost_calculated_data()' returns cost data based on the filtered_data
    cost_with_spheres <- inner_join(cost_calculated_data(), filtered_data(), by = "code")
    
    # Aggregation
    aggregated_costs <- cost_with_spheres %>%
      group_by(sphere) %>%
      summarise(totalCostYear = sum(totalCostYear, na.rm = TRUE)) %>%
      ungroup()
    
    # Plot
    aggregated_costs %>% 
      ggplot(aes(x = reorder(sphere, totalCostYear), y = round(totalCostYear/1000, 1), fill = sphere)) +
      geom_bar(stat = "identity", position = position_dodge(), color = "black") +
      scale_fill_manual(values = sphere_colors) +
      geom_text(aes(label = round(totalCostYear/1000, 1), vjust = -0.35), col = "gray15", size = 14/.pt) +
      labs(x = "Sphere", y = "Cost in k€", fill = "") +
      coord_cartesian(clip="off") +
      guides(fill = "none") +
      theme_bw() +
      theme(text = element_text(size = 18), 
            legend.position = "bottom")
  })
  
  # Download handler for the cost by type plot
  output$downloadTypePlotCosts <- downloadHandler(
    filename = function() {
      paste0("cost_type_breakdown_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(cost_calculated_data())
      long_cost_data <- cost_calculated_data() %>%
        tidyr::pivot_longer(names_to = "costType", values_to = "amount", purchaseCostYear:totalCostYear)
      
      g <- long_cost_data %>% 
        ungroup() %>% 
        mutate(costType = as_factor(costType)) %>% 
        mutate(costType = fct_recode(costType, 
                                     Purchase = "purchaseCostYear",
                                     Maintenance = "maintenanceCostYear",
                                     "Lab analysis" = "labAnalysisCostYear",
                                     Sampling = "samplingCostYear",
                                     Total = "totalCostYear")) %>% 
        group_by(costType) %>%
        summarise(euro = sum(amount), .groups = "drop") %>%
        ggplot(aes(x = reorder(costType, euro), y = (euro/1000), fill = costType)) +
        geom_bar(stat = "identity", position = position_dodge(), fill = "#226755", col = "black", alpha = 0.8) +
        coord_flip() +
        geom_text(aes(label = round(euro/1000, 1), vjust = -0.35), col = "gray15", size = 14/.pt) +
        labs(x = "Cost Type", y = "Cost in k€", title = "Cost breakdown by type") +
        theme_bw() +
        theme(text = element_text(size = 18), legend.position = "bottom")
        
      ggsave(file, plot = g, width = 10, height = 7, dpi = 300)
      }
      )
  
  # download sphere costs plot
  output$downloadSpherePlot <- downloadHandler(
    filename = function() {
      paste0("sphere-cost-plot-", Sys.Date(), ".png")
    },
    content = function(file) {
      # Make sure this uses the same filtered data
      req(filtered_data())
      
      # Generate the plot
      # Assuming 'cost_calculated_data()' returns cost data based on the filtered_data
      cost_with_spheres <- inner_join(cost_calculated_data(), filtered_data(), by = "code")
      
      # Aggregation
      aggregated_costs <- cost_with_spheres %>%
        group_by(sphere) %>%
        summarise(totalCostYear = sum(totalCostYear, na.rm = TRUE)) %>%
        ungroup()
      
      # Plot
      plot <- aggregated_costs %>% 
        ggplot(aes(x = reorder(sphere, totalCostYear), y = round(totalCostYear/1000, 1), fill = sphere)) +
        geom_bar(stat = "identity", position = position_dodge(), color = "black") +
        scale_fill_manual(values = sphere_colors) +
        geom_text(aes(label = round(totalCostYear/1000, 1), vjust = -0.35), col = "gray15", size = 14/.pt) +
        labs(x = "Sphere", y = "Cost in k€", fill = "") +
        coord_cartesian(clip="off") +
        guides(fill = "none") +
        theme_bw() +
        theme(text = element_text(size = 18), 
              legend.position = "bottom")
      
      # Save the plot using ggsave
      ggsave(file, plot = plot, width = 8, height = 6, device = "png")
    }
  )
  
  
  
    } 
  

# Run the app
shinyApp(ui, server)
