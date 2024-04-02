# libraries ----
remotes::install_github("deepanshu88/summaryBox")
pacman::p_load(
  "shiny", "shinyjs", "shinyBS",
  "shinyWidgets", "DT", "readxl",
  "writexl", "htmltools", "tidyverse",
  "bslib"
)
library("summaryBox")

# Read the data from the files ----
## file with the SO costs
datasetCosts <- read_csv("./data/eLTER-SO_costs-V18_1.csv")

# Fixing some errors in the dataset before making it available for the app
datasetCosts <- datasetCosts %>%
  # renaming terms
  rename(type = method) %>%
  # removing the SOHYD_065 as requested by Steffen Zacharias
  filter(!code == "SOHYD_065") %>%
  # fixing the issue on the flip information on the so_short_name from SOSOC_030 and SOSOC_031
  mutate(so_short_name = replace(
    so_short_name,
    code == "SOSOC_030",
    "Land-based income (SOSOC_030)"
  )) %>%
  mutate(so_short_name = replace(
    so_short_name,
    code == "SOSOC_031",
    "Yield (SOSOC_031)"
  )) %>%
  # removing the sociosphere info from the app (as requested by Jaana Bäck on 2024-03-26
  filter(!sphere == "Sociosphere") %>% 
  # changing the upgradeInterval from SOHYD_168 to 10 years (requested by S. Zacharias on 2024-03-19)
  mutate(upgradeInterval = replace(
    upgradeInterval,
    code == "SOHYD_168",
    10
  ))

## file with the info on the SOs' spheres and method types
dataset <- readxl::read_excel("./data/SO-Methods-Costs-selection.xlsx")
### Improving user friendliness on the names
dataset <- dataset %>%
  mutate(habitat = gsub("_", " ", habitat)) %>%
  # removing the eLTSER from the app
  filter(!habitat == "e ltser platform") %>%
  arrange(habitat)

# creating a table containing the relationship between code and so_short_name
codes_coding <- datasetCosts %>%
  distinct(code, so_short_name)

# putting this information on the dataset
dataset <- inner_join(dataset, codes_coding) %>%
  relocate(so_short_name, .after = code) # reorganizing the order of the terms

# Custom eLTER colors ----
color1 <- "#ED9632"
color2 <- "#0879C0"
color3 <- "#40907A"
color4 <- "#96CE58"
color5 <- "#BADFFD"

# Named vector for sphere colors
sphere_colors <- c(
  "Atmosphere" = color5,
  "Hydrosphere" = color2,
  "Geosphere" = color3,
  "Biosphere" = color4
)

# creating a fill scale function for the plots
scale_fill_elter <- function(...) {
  scale_fill_manual(
    values =
      c( # color1,
        color2,
        color3,
        color4,
        color5
      ), ...
  )
}

# Custom functions ----
costs_elter <- function(dataset, cat, hab) {
  dataset %>%
    filter(category == cat & habitat == hab & !is.na(type) == TRUE) %>%
    distinct(sphere, code, so_short_name, standard_observation, type)
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
  df1 <- tryCatch(
    {
      costs_elter(dataset = dataset, cat = cat, hab = hab)
    },
    error = function(e) {
      message("Error in costs_elter:", e)
      data.frame() # return an empty data frame in case of an error
    }
  )
  f_spheres(dataset = df1, spheres = spheres)
}

# Function to calculate the costs per year
SO_cost <- function(input_code, input_type) {
  # Filter data for the specific code
  filtered_data <- datasetCosts %>%
    dplyr::filter(code == input_code, type == input_type)
  
  # Check if the filtered data is empty
  if (nrow(filtered_data) == 0) {
    # Return an empty data frame or a data frame with default/NA values
    return(data.frame(
      code = input_code, type = input_type,
      purchasePrice = NA, purchaseCostYear = NA,
      maintenanceCostYear = NA, samplingCostYear = NA,
      labAnalysisCostYear = NA, totalHumanLabor = NA,
      totalCostYear = NA
    ))
  }
  
  # Calculating costs
  results <- filtered_data %>%
    dplyr::mutate(
      # 2024-02-19; adding a column displaying the purchase costs independently from the upgrade interval
      purchasePrice,
      # purchase
      purchaseCostYear = case_when(
        purchasePrice == 0 ~ 0,
        purchasePrice > 0 & upgradeInterval == 0 ~ round(((purchasePrice * minimumSamplePerSite)), 0),
        purchasePrice > 0 & upgradeInterval > 0 ~ round(((purchasePrice * minimumSamplePerSite) / upgradeInterval), 0)
      ),
      # # 2024-02-19: adding the upgrade interval on the table as requested by Steffen Zacharias (2024-02-16)
      # maintenance
      maintenanceCostYear = round(maintenancePrice, 0), # 2024-02-19: removed the effort (maintenanceEffort) from the calculation following the comments from Steffen Zacharias on 2014-02-16. # it should not include the price per site, as it is priced at for the entire bundle
      # sampling
      samplingCostYear = round(samplingPrice, 0), # 2024-02-19: Steffen Zacharias requested to remove the samples per year (measurementsPerYear) from the calculations and consider only the value on the sampling. Removed the sampling per sites as it shouldn't affect the calculations
      # lab analysis
      labAnalysisCostYear = round(labAnalysisPrice * minimumSamplePerSite * measurementsPerYear, 0),
      # human labor
      totalHumanLabor = round(totalHumanLabor, 2)
    ) %>%
    # total cost
    mutate(totalCostYear = sum(purchaseCostYear, maintenanceCostYear,
                               samplingCostYear, labAnalysisCostYear,
                               na.rm = TRUE
    )) %>%
    dplyr::select(
      code, type, purchasePrice, purchaseCostYear,
      maintenanceCostYear, samplingCostYear, labAnalysisCostYear,
      totalHumanLabor, totalCostYear
    )
  
  return(results)
}

# Shiny app UI ----
ui <- fluidPage(
  # specifying the title shown in the browser
  tags$head(
    tags$title("eLTER SO Costs")
  ),
  
  # specifying the favicon for the web browser
  tags$head(
    tags$link(rel = "shortcut icon", type = "image/png", href = "elter_logomark.png")
  ),
  # specifying the image with the eLTER SO Costs logo
  titlePanel(div(
    style = "text-align: center;",
    tags$a(
      href = "https://elter-ri.eu/", target = "_blank",
      img(src = "elter logo_SO Costs_logo-03.jpg", height = "75px")
    )
  )),
  tags$head(
    tags$style(HTML("
                     .tabbable > .nav > li[class=active] > a { background-color: #F26522;  color:white}
                     .navbar { background-color: #0073C2; }
                     .btn { background-color: #F26522; color: #fff; }
                     .btn:hover { background-color: darken(#F26522, 10%); }
                     .scrollable-checkbox {
                                           height: 300px;
                                           overflow-y: auto;
                                           border: 1px solid #ccc;
                                           padding: 10px;
                                           border-radius: 5px;
                                           background-color: #FFFFFF;
                                          }
                     h1 { color: #0073C2; }
                     h2 { color: #0073C2; }
                     h3 { color: #0073C2; }
                     h4 { color: #000000; }
                    "))
  ),
  tabsetPanel(
    
    # Adding the Info tab ----
    tabPanel(
      HTML('Info <i class="fas fa-info-circle"></i>'),
      fluidRow(
        column(
          6,
          card(
            fill = FALSE,
            max_height = 1000,
            card_body(
              border_radius = "all",
              width = 1000,
              fillable = FALSE,
              fill = FALSE,
              plotOutput("id", width = "100%", height = "100%"),
              class = "align-items-center",
              HTML("<h1>Welcome</h1>"),
              p(HTML("This interactive tool is designed to assist researchers and site managers associated with the <a href = 'https://elter-ri.eu/' target = '_blank'> <b>Integrated European Long-Term Ecosystem, critical zone and socio-ecological research (eLTER)</b></a> network in defining <a href = 'https://vocabs.lter-europe.net/so/en/' target = '_blank'> <b>Standard Observations (SOs)</b></a> and calculating the associated costs to upgrade and operate them at their sites. The outputs of this tool are subjected to changes, as modifications in the inputs might occur in the future based on the agreement of eLTER consortium. Additionally, the values presented here might differ slightly from precise calculations of the costs due to different reasons (e.g. differences in management among institutions, different costs associated with the sampling, maintenance, lab analysis and etc.).")),
              HTML("<h2>Features</h2>"),
              tags$ul(
                tags$li(HTML("<b>Selecting parameters</b>")),
                tags$ul(
                  tags$li(HTML("Begin by selecting the site category, habitat and focus spheres of your eLTER site on the <b>Set up</b> tab to tailor the SOs list to your specific needs."))
                ),
                tags$li(HTML("<b>Customizing your SO list</b>")),
                tags$ul(
                  tags$li(HTML("Deselect any SO that your site has already covered on the <i>side panel</i> of the <b>Set up</b> tab."))
                ),
                tags$li(HTML("<b>Costs calculations</b>")),
                tags$ul(
                  tags$li(HTML("Navigate to the <b>SO costs</b> tab to view a detailed breakdown of costs for running the selected SOs at your eLTER site. This includes purchase, maintenance, sampling, lab analysis costs, and the total human labor involved."))
                ),
                tags$li(HTML("<b>Exporting the costs calculations</b>")),
                tags$ul(
                  tags$li(HTML("On the <b>SO costs</b> tab navigate to the bottom of the table and add the name of your eLTER site and download your results using the download button."))
                ),
                tags$li(HTML("<b>Informative visualizations</b>")),
                tags$ul(
                  tags$li("Explore various plots providing insights into the number of SOs by sphere, annual cost breakdown by type, labor effort by sphere, and more. These visualizations aid in understanding the distribution and financial implications of the SOs required for your site.")
                )
              ),
              tags$h2("FAQ"),
              tags$p(HTML("Click <a href = 'https://docs.google.com/document/d/1JchmwmXVXfhXL4iWhGFjD46rCNKTtfS6oaq68_nNZZA/edit?usp=sharing' target = '_blank'> <b>here</b></a> to access the Frequently Asked Questions.")),
              tags$h2("Accessing the source code"),
              tags$p(HTML("For those interested in exploring the underlying code, contributing to its development, or customizing the application for specific needs, the source code is available on <a href = 'https://github.com/allantsouza/eLTER-SO-costs-App' target = '_blank'> <b>GitHub</b></a>.")),
              tags$h2("Disclaimer"),
              tags$p(HTML("This app is a <i>beta product</i>, and we are continuously working to improve its accuracy and functionality. <br /> If you encounter any issues or have suggestions for improvement, please contact the developer at: <a href='mailto:allan.souza@helsinki.fi'><b>allan.souza@helsinki.fi</b></a>. <br /> Your feedback is invaluable in helping us enhance this tool."))
            )
          )
        ),
        column(
          6,
          # Creating a card for the image
          div(
            class = "card rounded-lg",
            div(
              class = "card-body",
              img(
                src = "landingPagePicture2.jpg",
                class = "img-fluid", alt = "Responsive image",
                style = "max-width: 100%; height: auto; padding:5px; background-color:#F1F1F1;"
              ),
              tags$h6(HTML("<i>Photo: Juho Aalto.</i>"), style = "padding:5px; text-align: right;")
            )
          )
        )
      )
      # )
    ),
    
    # Adding the Set up tab ----
    tabPanel(
      HTML('Set up <i class="fa-solid fa-gears"></i>'),
      sidebarLayout(
        sidebarPanel(
          h2("Select parameters"),
          radioGroupButtons(
            inputId = "cat",
            label = "eLTER site category",
            choices = c("1", "2"),
            justified = TRUE,
            width = "100%",
            size = "normal",
            checkIcon = list(
              yes = icon(class = "fa-solid", "fa-check-to-slot"),
              no = icon("square")
            ),
            status = "info"
          ),
          bsTooltip("cat", "Choose the category of your eLTER site", "right"), # Tooltips
          bsTooltip("hab", "Choose the habitat of your eLTER site", "right"), # Tooltips
          selectInput("hab", "Site habitat", choices = c(Select = "", unique(dataset$habitat))),
          bsTooltip("sphere1", "Choose the focus sphere of your eLTER site. Disabled to category 2 sites.", "right"), # Tooltips
          selectInput("sphere1", "Focus sphere #1", choices = unique(dataset$sphere)),
          bsTooltip("sphere2", "Choose the focus sphere of your eLTER site. Disabled to category 2 sites.", "right"), # Tooltips
          selectInput("sphere2", "Focus sphere #2", choices = unique(dataset$sphere)),
          uiOutput("codeSelect"),
          bsTooltip("codeSelect", "Remove the selection of SOs that are not pertinent in your case.", "right"), # Tooltips
          downloadButton("download", "Download",
                         class = "btn",
                         icon = icon(class = "fa-regular", name = "fa-circle-down")
          ),
        ),
        mainPanel(
          h2("List of the standard observations needed in your site"),
          DTOutput("updatedTable")
        )
      )
    ),
    
    # Adding the SO costs tab ----
    tabPanel(
      HTML('SO costs <i class="fa-solid fa-square-poll-horizontal"></i>'),
      fluidPage(
        fluidRow(
          column(
            12,
            summaryBox2(
              title = "Selected", "Parameters", width = 6,
              icon = "fa-solid fa-clipboard-list", style = "info"
            ),
            summaryBox2(
              title = "Estimated", "Costs", width = 6,
              icon = "fa-solid fa-euro-sign", style = "info"
            )
          )
        ),
        fluidRow(
          column(
            6,
            h4(textOutput("selectedCat"), style = "margin-left: 25px;"),
            h4(textOutput("selectedHab"), style = "margin-left: 25px;"),
            h4(textOutput("selectedSphere1"), style = "margin-left: 25px;"),
            h4(textOutput("selectedSphere2"), style = "margin-left: 25px;")
          ),
          column(
            6,
            h4(textOutput("totalCostsDisplay"), style = "margin-left: 15px;"),
            bsTooltip("totalCostsDisplay", "This value sums the annual costs of replacement costs of equipment, maintenance, sampling, and lab analysis. The value used for the purchase costs takes into account the upgrade interval of the equipment (formula: purchase price / upgrade interval).", "left"), # Tooltips
            h4(textOutput("UpfrontPurchaseCosts"), style = "margin-left: 15px;"),
            bsTooltip("UpfrontPurchaseCosts", "This value considers the total initial purchase costs and should be used to estimate the costs of establishing an eLTER site from the beginning.", "left"), # Tooltips
            h4(textOutput("totalHumanLaborDisplay"), style = "margin-left: 15px;"),
            bsTooltip("totalHumanLaborDisplay", "Warning: The labor costs are not included in the estimated costs.", "left"), # Tooltips
            p(HTML("<i>Note: The labor costs are not included in the costs. You must calculate the labor costs based on the labor needed at your site (indicated above) and the salary structure in your institution and/or country. Add this number to the estimated costs presented above to have the final cost for your eLTER site.</i>"),
              style = "margin-left: 15px; margin-right: 25px; text-align: justify;"
            )
          )
        ),
        # plots
        # Double line break as a space
        br(),
        br(),
        fluidRow(
          column(
            12,
            summaryBox2(
              title = "Detailed costs", "Upgrading and operating an eLTER site", width = 12,
              icon = "fa-solid fa-table-list", style = "info"
            ),
            h4(HTML("This table shows the costs (in €) of the standard observations (SOs) needed to upgrade and operate the eLTER site with the conditions selected at the <b>Set up</b> tab. The total cost is calculated by summing the different costs types (purchase, maintenance, sampling and lab analysis). Additionally, the table shows the human labor needed to operate the eLTER site, expressed as number of days needed to perform all tasks related to the specific SOs per year."),
               style = "margin-left: 20px; margin-right: 20px; text-align: justify;"
            ),
            h5(HTML("<br /> <i>Note #1: This table displays only the SOs which have costs associated to it (economic or human labor). <br /> Note #2: The orange bars displayed within each column visually represent the proportion of each SO's cost relative to the maximum cost found in that column. This graphical representation provides an intuitive understanding of how each SO's cost compares to the highest cost observed for that particular cost variable, allowing for quick visual assessment of cost distribution across SOs. <br /> Note #3: The SOs from the Sociosphere are not included in this tool.</i>"),
               style = "margin-left: 20px; margin-right: 20px; text-align: justify;"
            ),
            # Double line break as a space
            br(),
            br(),
            h5(DTOutput("costTable"),
               style = "margin-left: 25px; margin-right: 25px; text-align: justify;"
            )
          ),
          fluidRow(
            column(10, textInput("fileNameInput",
                                 HTML("Site Name of as displayed in <a href = 'https://www.deims.org' target = '_blank'><b>deims.org</b></a>:"),
                                 value = "Site Name"
            ),
            style = "padding-right: 0; margin-left: 35px; margin-right: 45px; width: 100%"
            ),
            column(2,
                   bsTooltip("downloadCosts", "Before downloading the table, please add the Site Name of your eLTER site in the box above.", "right"), # Tooltips
                   downloadButton("downloadCosts",
                                  "Download",
                                  style = "margin-right: 25px; text-align: center;",
                                  icon = icon(class = "fa-regular", name = "fa-circle-down")
                   ),
                   style = "padding-right: 0; margin-left: 35px; margin-right: 25px;"
            )
          )
        ),
        # Double line break as a space
        br(),
        br(),
        
        # plots
        fluidRow(
          column(
            12,
            summaryBox2(
              title = "Data Visualization", "Insights", width = 12,
              icon = "fa-solid fa-magnifying-glass-chart", style = "info"
            ),
          )
        ),
        fluidRow(
          column(
            6,
            h4("Number of standard observations breakdown by sphere",
               style = "margin-left: 20px; margin-right: 20px; text-align: center;"
            ),
            h5("This plot illustrates the total number of SOs needed to operate the eLTER site.",
               style = "margin-left: 20px; margin-right: 20px; text-align: center;"
            ),
            h5(plotOutput("updatedBarPlot"),
               style = "margin-left: 25px; margin-right: 25px;"
            ),
            downloadButton("downloadPlot", "Download",
                           style = "margin-left: 20px; margin-right: 20px;",
                           icon = icon(class = "fa-regular", name = "fa-circle-down")
            ),
            # Double line break as a space
            br(),
            br(),
            h4("Annual cost breakdown by type",
               style = "margin-left: 20px; margin-right: 20px; text-align: center;"
            ),
            h5("This plot depicts the total costs (by type) needed to operate the eLTER site.",
               style = "margin-left: 20px; margin-right: 20px; text-align: center;"
            ),
            h5(plotOutput("typeCostPlot"),
               style = "margin-left: 25px; margin-right: 25px;"
            ),
            downloadButton("downloadTypePlotCosts", "Download",
                           style = "margin-left: 20px; margin-right: 20px;",
                           icon = icon(class = "fa-regular", name = "fa-circle-down")
            ),
            # Double line break as a space
            br(),
            br(),
          ),
          column(
            6,
            h4("Annual labor effort by sphere",
               style = "margin-left: 20px; margin-right: 20px; text-align: center;"
            ),
            h5("This plot depicts the total working days by sphere needed to operate the eLTER site.",
               style = "margin-left: 20px; margin-right: 20px; text-align: center;"
            ),
            h5(plotOutput("sphereFTECostPlot"),
               style = "margin-left: 20px; margin-right: 20px; text-align: center;"
            ),
            downloadButton("downloadHumanCostPlot", "Download",
                           icon = icon(class = "fa-regular", name = "fa-circle-down")
            ),
            # Double line break as a space
            br(),
            br(),
            h4("Annual cost breakdown by sphere", style = "text-align: center;"),
            h5("This plot illustrates how costs are distributed across different spheres to operate the eLTER site.",
               style = "margin-left: 20px; margin-right: 20px; text-align: center;"
            ),
            h5(plotOutput("sphereCostPlot"), style = "margin-left: 20px; margin-right: 20px; text-align: center;"),
            downloadButton("downloadSpherePlot", "Download",
                           icon = icon(class = "fa-regular", name = "fa-circle-down")
            ),
            # Double line break as a space
            br(),
            br(),
          )
        )
      )
    )
  )
)

# Shiny app server ----
server <- function(input, output, session) {
  # rendering the table with unique combinations of sphere, code, and standard_observation
  output$completeTable <- renderDT({
    req(dataset)
    unique_data <- dataset %>%
      distinct(sphere, code, standard_observation)
    datatable(unique_data, options = list(pageLength = 100))
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
    
    # Sort so_short_names alphabetically
    sorted_data <- data %>%
      arrange(sphere, code)
    
    # Prepare choices as named vector, names are displayed, values are sent to server
    choices <- setNames(sorted_data$code, sorted_data$so_short_name)
    
    # Using pickerInput
    pickerInput(
      inputId = "selectedCodes",
      label = "Optionally, deselect the SOs that are not needed for your site (i.e. SOs covered by other means).",
      choices = choices,
      selected = sorted_data$code,
      multiple = TRUE, # allow multiple selections
      options = list(`actions-box` = TRUE) # enable actions box for select/deselect all
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
  
  # Total costs display
  output$totalCostsDisplay <- renderText({
    cost_data <- cost_calculated_data()
    if (nrow(cost_data) > 0) {
      total_cost <- sum(cost_data$totalCostYear, na.rm = TRUE)
      paste("Total annual cost: €", format(total_cost, big.mark = ",", decimal.mark = ".", digits = 2))
    } else {
      "No costs calculated yet."
    }
  })
  
  # For the upfront purchase costs display
  output$UpfrontPurchaseCosts <- renderText({
    cost_data <- cost_calculated_data()
    if (nrow(cost_data) > 0) {
      upfront_cost <- sum(cost_data$purchasePrice, na.rm = TRUE)
      paste("Upfront purchase cost: €", format(upfront_cost,
                                               big.mark = ",",
                                               decimal.mark = ".", digits = 2
      ))
    } else {
      "No costs calculated yet."
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
    datatable(
      filtered_data() %>%
        select(-so_short_name) %>%
        relocate(type, .after = code) %>%
        rename(
          "Sphere" = sphere,
          "SO code" = code,
          "Standard Observation" = standard_observation,
          "Method type" = type
        ),
      options = list(pageLength = 100),
      rownames = FALSE,
      selection = "none" # removing the option to highlight rows on the table
    )
  })
  
  # Render the updated bar plot based on the filtered data
  output$updatedBarPlot <- renderPlot({
    req(filtered_data())
    filtered_data() %>%
      mutate(type = fct_recode(type,
                               "Prime" = "prime",
                               "Basic" = "basic"
      )) %>%
      ggplot(aes(x = type, fill = sphere)) +
      geom_bar(position = "dodge", col = "black") +
      labs(x = "Type of SO (Standard Observation)", y = "Count", fill = "") +
      coord_flip() +
      scale_fill_manual(values = sphere_colors) +
      theme_bw() +
      theme(text = element_text(size = 18), legend.position = "bottom")
  })
  
  # DownloadHandler for the table
  output$download <- downloadHandler(
    filename = function() {
      paste0("eLTER-SO-Costs-requirements-table_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(filtered_data())
      
      filtered_data <- filtered_data() %>%
        select(-so_short_name) %>%
        relocate(type, .after = code) %>%
        rename(
          "Sphere" = sphere,
          "SO code" = code,
          "Standard Observation" = standard_observation,
          "Method type" = type
        )
      
      writexl::write_xlsx(filtered_data, file)
    }
  )
  
  # DownloadHandler for the bar plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("eLTER-SO-Costs-SO-type-plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(filtered_data())
      g <- filtered_data() %>%
        mutate(type = fct_recode(type,
                                 "Prime" = "prime",
                                 "Basic" = "basic"
        )) %>%
        ggplot(aes(x = type, fill = sphere)) +
        geom_bar(position = "dodge", col = "black") +
        labs(x = "Type of SO (Standard Observation)", y = "Count", fill = "Sphere") +
        coord_flip() +
        scale_fill_manual(values = sphere_colors) +
        theme_bw() +
        theme(
          text = element_text(size = 18),
          legend.position = "bottom"
        )
      
      # Save the ggplot object to file
      ggsave(file, plot = g, width = 11, height = 8, dpi = 300)
    }
  )
  
  # Reactive expression for cost calculation
  cost_calculated_data <- reactive({
    req(filtered_data())
    data <- filtered_data()
    
    # getting the unique combinations of code and type
    unique_combinations <- unique(data[, c("code", "type")])
    
    # applying the SO_cost function to each unique combination of code and type
    # cost_data <- do.call(rbind, lapply(1:nrow(unique_combinations), function(i) {
    cost_data <- do.call(rbind, lapply(seq_len(nrow(unique_combinations)), function(i) {
      SO_cost(unique_combinations$code[i], unique_combinations$type[i])
    }))
    
    # Join to include so_short_name
    cost_data <- cost_data %>%
      left_join(codes_coding, by = "code")
    
    # Replace NA values with zero in selected columns
    cost_data <- cost_data %>%
      mutate(
        purchasePrice = ifelse(is.na(purchasePrice), 0, purchasePrice),
        purchaseCostYear = ifelse(is.na(purchaseCostYear), 0, purchaseCostYear),
        maintenanceCostYear = ifelse(is.na(maintenanceCostYear), 0, maintenanceCostYear),
        samplingCostYear = ifelse(is.na(samplingCostYear), 0, samplingCostYear),
        labAnalysisCostYear = ifelse(is.na(labAnalysisCostYear), 0, labAnalysisCostYear),
        totalHumanLabor = ifelse(is.na(totalHumanLabor), 0, totalHumanLabor),
        totalCostYear = ifelse(is.na(totalCostYear), 0, totalCostYear)
      ) %>%
      # Remove rows where all cost fields are zero
      filter(rowSums(select(., purchasePrice, purchaseCostYear, maintenanceCostYear, samplingCostYear, labAnalysisCostYear, totalHumanLabor, totalCostYear)) != 0)
    
    
    return(cost_data)
  })
  
  # Render the calculated costs table
  output$costTable <- renderDT({
    req(cost_calculated_data())
    
    # Get the data from the reactive expression
    cost_data <- cost_calculated_data()
    
    # Calculate the sum for each column
    summary_row <- cost_data %>%
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      mutate(
        so_short_name = "Total",
        "type" = NA
      ) %>%
      relocate(so_short_name, .before = purchaseCostYear) %>%
      relocate(type, .after = so_short_name)
    
    # Append the summary row to the original data
    final_data <- bind_rows(cost_data, summary_row)
    
    # Render the DataTable with the final data
    datatable(
      final_data %>%
        relocate(totalHumanLabor, .after = totalCostYear) %>%
        relocate(so_short_name, .after = code) %>%
        select(-code) %>%
        arrange(totalCostYear, totalHumanLabor) %>%
        rename(
          "SO short name" = so_short_name,
          "Method type" = type,
          "Implementation" = purchasePrice,
          "Replacement costs of equipment" = purchaseCostYear,
          "Maintenance (per year)" = maintenanceCostYear,
          "Sampling (per year)" = samplingCostYear,
          "Lab analysis (per year)" = labAnalysisCostYear,
          "Total cost (per year)" = totalCostYear,
          "Person days (per year)" = totalHumanLabor
        ),
      options = list(
        pageLength = 100,
        columnDefs = list(
          list(width = "500px", targets = c(0)),
          list(width = "60px", targets = c(1))
        ),
        autoWidth = TRUE
      ),
      rownames = FALSE,
      selection = "none" # removing the option to highlight rows on the table
    ) %>%
      formatStyle(
        "Method type",
        fontWeight = styleEqual("prime", "bold") # making the font bold for prime method
      ) %>%
      formatCurrency(
        c(
          "Implementation", "Replacement costs of equipment",
          "Maintenance (per year)", "Sampling (per year)",
          "Lab analysis (per year)", "Total cost (per year)"
        ),
        currency = "€"
      ) %>%
      # adding the colored bars on the table
      formatStyle("Implementation",
                  background = styleColorBar(range(final_data$purchasePrice), color1),
                  backgroundSize = "100% 100%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      ) %>%
      formatStyle("Replacement costs of equipment",
                  background = styleColorBar(range(final_data$purchaseCostYear), "#F26522"),
                  backgroundSize = "100% 100%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      ) %>%
      formatStyle("Maintenance (per year)",
                  background = styleColorBar(range(final_data$maintenanceCostYear), "#F26522"),
                  backgroundSize = "100% 100%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      ) %>%
      formatStyle("Sampling (per year)",
                  background = styleColorBar(range(final_data$samplingCostYear), "#F26522"),
                  backgroundSize = "100% 100%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      ) %>%
      formatStyle("Lab analysis (per year)",
                  background = styleColorBar(range(final_data$labAnalysisCostYear), "#F26522"),
                  backgroundSize = "100% 100%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      ) %>%
      formatStyle("Total cost (per year)",
                  background = styleColorBar(range(final_data$totalCostYear), "#F26522"),
                  backgroundSize = "100% 100%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      ) %>%
      formatStyle("Person days (per year)",
                  background = styleColorBar(range(final_data$totalHumanLabor), color1),
                  backgroundSize = "100% 100%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      ) %>%
      # Highlighting the summary row
      formatStyle(
        "SO short name",
        target = "row",
        backgroundColor = styleEqual("Total", "#F26522"),
        backgroundSize = "100% 100%",
        fontWeight = styleEqual("Total", "bold"), # making the font bold for Total
        color = styleEqual("Total", "white")
      )
  })
  
  # 2024-03-27 - trying to add the variables that were deselected into the excel sheet
  
  # reactive expression for the codes available for deselection
  available_codes_for_deselection <- reactive({
    req(input$cat, input$hab, input$sphere1, input$sphere2)
    data <- station_requirements(dataset, input$cat, input$hab, c(input$sphere1, input$sphere2))
    
    # returning the codes available for deselection
    data$code
  })
  
  # download costs table
  output$downloadCosts <- downloadHandler(
    filename = function() {
      paste0("eLTER-SO-Costs-table_", input$fileNameInput, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # filename = function() {
      #   paste0("eLTER-SO-Costs-table_", Sys.Date(), ".xlsx")
      # },
      # content = function(file) {
      req(cost_calculated_data())
      
      # getting the available codes for deselection based on the reactive expression
      available_codes <- available_codes_for_deselection()
      selected_codes <- input$selectedCodes
      deselected_codes <- setdiff(available_codes, selected_codes)
      
      # filtering the dataset for deselected codes to get their information
      deselected_data <- dataset %>%
        filter(code %in% deselected_codes) %>%
        select(code, sphere, so_short_name) %>%
        distinct() %>%
        rename("SO short name" = so_short_name)
      
      # original data and selections data preparation
      cost_data <- cost_calculated_data()
      summary_row <- cost_data %>%
        summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        mutate(so_short_name = "Total", type = NA) %>%
        relocate(so_short_name, .before = purchaseCostYear) %>%
        relocate(type, .after = so_short_name)
      
      final_data <- bind_rows(cost_data, summary_row)
      
      # renaming and reorganizing variables
      final_data <- final_data %>%
        relocate(totalHumanLabor, .after = totalCostYear) %>%
        relocate(so_short_name, .after = code) %>%
        select(-code) %>%
        arrange(totalCostYear, totalHumanLabor) %>%
        rename(
          "SO short name" = so_short_name,
          "Method type" = type,
          "Implementation" = purchasePrice,
          "Replacement costs of equipment" = purchaseCostYear,
          "Maintenance (per year)" = maintenanceCostYear,
          "Sampling (per year)" = samplingCostYear,
          "Lab analysis (per year)" = labAnalysisCostYear,
          "Total cost (per year)" = totalCostYear,
          "Person days (per year)" = totalHumanLabor
        )
      
      selections_data <- data.frame(
        Parameter = c("eLTER site category", "Site habitat", "Focus sphere #1", "Focus sphere #2"),
        Selection = c(
          input$cat,
          input$hab,
          ifelse(input$cat == "2", "Not applicable", input$sphere1),
          ifelse(input$cat == "2", "Not applicable", input$sphere2)
        )
      )
      
      # creating a list with information for the three sheets in Excel
      sheets <- list(
        siteCharacteristics = selections_data,
        deselectedSOs = deselected_data,
        costs = final_data
      )
      
      # writing the list to an Excel file
      writexl::write_xlsx(sheets, file)
    }
  )
  
  # rendering the cost by type plot
  output$typeCostPlot <- renderPlot({
    req(cost_calculated_data())
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
                                   FTEs = "totalHumanLabor",
                                   Total = "totalCostYear"
      )) %>%
      filter(!costType == "FTEs") %>%
      group_by(costType) %>%
      summarise(euro = sum(amount), .groups = "drop") %>%
      ggplot(aes(x = reorder(costType, euro), y = (euro / 1000), fill = costType)) +
      geom_bar(stat = "identity", position = position_dodge(), fill = "gray50", col = "black") +
      geom_text(aes(label = round(euro / 1000, 1), vjust = -0.35), col = "gray15", size = 14 / .pt) +
      labs(x = "Cost type", y = "Cost per year in k€") +
      coord_cartesian(clip = "off") +
      theme_bw() +
      theme(text = element_text(size = 18), legend.position = "bottom")
  })
  
  # rendering the cost by sphere plot
  output$sphereCostPlot <- renderPlot({
    req(filtered_data())
    cost_with_spheres <- inner_join(cost_calculated_data(), filtered_data(), by = "code")
    
    # data agregation
    aggregated_costs <- cost_with_spheres %>%
      group_by(sphere) %>%
      summarise(totalCostYear = sum(totalCostYear, na.rm = TRUE)) %>%
      ungroup()
    
    # plot
    aggregated_costs %>%
      ggplot(aes(x = reorder(sphere, totalCostYear), y = round(totalCostYear / 1000, 1), fill = sphere)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_manual(values = sphere_colors) +
      geom_text(aes(label = round(totalCostYear / 1000, 1), vjust = -0.35), col = "gray15", size = 14 / .pt) +
      labs(x = "Sphere", y = "Cost per year in k€", fill = "") +
      coord_cartesian(clip = "off") +
      guides(fill = "none") +
      theme_bw() +
      theme(
        text = element_text(size = 18),
        legend.position = "bottom"
      )
  })
  
  
  # rendering the human resources cost by sphere plot
  output$sphereFTECostPlot <- renderPlot({
    req(filtered_data())
    
    cost_with_spheres2 <- inner_join(cost_calculated_data(), filtered_data(), by = "code")
    
    # data aggregation
    aggregated_costs2 <- cost_with_spheres2 %>%
      group_by(sphere) %>%
      summarise(FTE = sum(totalHumanLabor, na.rm = TRUE)) %>%
      ungroup()
    
    # plot
    aggregated_costs2 %>%
      ggplot(aes(x = reorder(sphere, FTE), y = round(FTE, 2), fill = sphere)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_manual(values = sphere_colors) +
      geom_text(aes(label = round(FTE, 2), vjust = -0.35), col = "gray15", size = 14 / .pt) +
      labs(x = "Sphere", y = "Total working days per year", fill = "") +
      coord_cartesian(clip = "off") +
      guides(fill = "none") +
      theme_bw() +
      theme(
        text = element_text(size = 18),
        legend.position = "bottom"
      )
  })
  
  # download handler for the cost by type plot
  output$downloadTypePlotCosts <- downloadHandler(
    filename = function() {
      paste0("eLTER-SO-Costs-type-plot_", Sys.Date(), ".png")
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
                                     FTEs = "totalHumanLabor",
                                     Total = "totalCostYear"
        )) %>%
        filter(!costType == "FTEs") %>%
        group_by(costType) %>%
        summarise(euro = sum(amount), .groups = "drop") %>%
        ggplot(aes(x = reorder(costType, euro), y = (euro / 1000), fill = costType)) +
        geom_bar(stat = "identity", position = position_dodge(), fill = "gray50", col = "black") +
        geom_text(aes(label = round(euro / 1000, 1), vjust = -0.35), col = "gray15", size = 14 / .pt) +
        labs(x = "Cost type", y = "Cost per year in k€") +
        coord_cartesian(clip = "off") +
        theme_bw() +
        theme(text = element_text(size = 18), legend.position = "bottom")
      
      ggsave(file, plot = g, width = 10, height = 7, dpi = 300)
    }
  )
  
  # download sphere costs plot
  output$downloadSpherePlot <- downloadHandler(
    filename = function() {
      paste0("eLTER-SO-Costs-spheres-plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(filtered_data())
      
      # Generate the plot
      cost_with_spheres <- inner_join(cost_calculated_data(), filtered_data(), by = "code")
      
      # data aggregation
      aggregated_costs <- cost_with_spheres %>%
        group_by(sphere) %>%
        summarise(totalCostYear = sum(totalCostYear, na.rm = TRUE)) %>%
        ungroup()
      
      # plot
      plot <- aggregated_costs %>%
        ggplot(aes(x = reorder(sphere, totalCostYear), y = round(totalCostYear / 1000, 1), fill = sphere)) +
        geom_bar(stat = "identity", position = position_dodge(), color = "black") +
        scale_fill_manual(values = sphere_colors) +
        geom_text(aes(label = round(totalCostYear / 1000, 1), vjust = -0.35), col = "gray15", size = 14 / .pt) +
        labs(x = "Sphere", y = "Cost per year in k€", fill = "") +
        coord_cartesian(clip = "off") +
        guides(fill = "none") +
        theme_bw() +
        theme(
          text = element_text(size = 18),
          legend.position = "bottom"
        )
      
      # saving the plot using ggsave
      ggsave(file, plot = plot, width = 8, height = 6, device = "png")
    }
  )
  
  # download human costs by sphere plot
  output$downloadHumanCostPlot <- downloadHandler(
    filename = function() {
      paste0("eLTER-SO-Costs-labour-sphere-plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(filtered_data())
      
      cost_with_spheres2 <- inner_join(cost_calculated_data(), filtered_data(), by = "code")
      
      # data aggregation
      aggregated_costs2 <- cost_with_spheres2 %>%
        group_by(sphere) %>%
        summarise(FTE = sum(totalHumanLabor, na.rm = TRUE)) %>%
        ungroup()
      
      # plot
      plot <- aggregated_costs2 %>%
        ggplot(aes(x = reorder(sphere, FTE), y = round(FTE, 2), fill = sphere)) +
        geom_bar(stat = "identity", color = "black") +
        scale_fill_manual(values = sphere_colors) +
        geom_text(aes(label = round(FTE, 2), vjust = -0.35), col = "gray15", size = 14 / .pt) +
        labs(x = "Sphere", y = "Total working days per year", fill = "") +
        coord_cartesian(clip = "off") +
        guides(fill = "none") +
        theme_bw() +
        theme(
          text = element_text(size = 18),
          legend.position = "bottom"
        )
      
      # saving the plot using ggsave
      ggsave(file, plot = plot, width = 8, height = 6, device = "png")
    }
  )
  
  # rendering the selected parameters
  # selected category
  output$selectedCat <- renderText({
    paste("eLTER site category:", if (input$cat != "") input$cat else "Not selected")
  })
  
  # rendering the selected habitat
  output$selectedHab <- renderText({
    paste("Site habitat:", if (input$hab != "") input$hab else "Not selected")
  })
  
  # rendering the selected focus sphere #1
  output$selectedSphere1 <- renderText({
    if (input$cat == "2") { # checking if category 2 is selected
      return("Focus sphere #1: Not applicable")
    } else {
      paste(
        "Focus sphere #1:",
        if (input$sphere1 != "") {
          input$sphere1
        } else {
          "Not selected"
        }
      )
    }
  })
  
  # rendering the selected focus #2
  output$selectedSphere2 <- renderText({
    if (input$cat == "2") { # Check if category 2 is selected
      return("Focus sphere #2: Not applicable")
    } else {
      paste(
        "Focus sphere #2:",
        if (input$sphere2 != "") {
          input$sphere2
        } else {
          "Not selected"
        }
      )
    }
  })
  
  # displaying the total costs
  output$totalCostsDisplay <- renderText({
    cost_data <- cost_calculated_data()
    if (nrow(cost_data) > 0) {
      total_costs <- cost_data %>%
        summarise(sum(totalCostYear)) %>%
        pull()
      paste("Annual cost: €", formatC(total_costs, format = "f", big.mark = ",", digits = 2), sep = "")
    } else {
      "Annual cost: €0"
    }
  })
  
  # displaying the total human labor
  output$totalHumanLaborDisplay <- renderText({
    cost_data <- cost_calculated_data()
    if (nrow(cost_data) > 0) {
      total_human_labor <- cost_data %>%
        summarise(sum(totalHumanLabor)) %>%
        pull()
      paste("Labor (person days per year):", formatC(total_human_labor, format = "f", big.mark = ",", digits = 2))
    } else {
      "Labor (person days per year): 0"
    }
  })
  
  # displaying the upfront purchase costs
  output$UpfrontPurchaseCosts <- renderText({
    cost_data <- cost_calculated_data()
    if (nrow(cost_data) > 0) {
      upfront_cost <- cost_data %>%
        summarise(sum(purchasePrice)) %>%
        pull()
      paste("Implementation: €", formatC(upfront_cost, format = "f", big.mark = ",", digits = 2), sep = "")
    } else {
      "Implementation: €0"
    }
  })
}

# running the app
shinyApp(ui, server)
