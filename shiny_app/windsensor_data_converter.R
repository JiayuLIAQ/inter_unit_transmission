library(shiny)
library(DT)
library(readr)
library(writexl)

library(data.table)
library(tidyr)
library(stringr)

read_wind_sensor <- function(path) {
  
  dt <- fread(path, sep = "", header = F)
  
  dt[, c("index", "n_comma", "path") := list(.I, str_count(V1, ","), path)]
  
  
  dt_1 <- copy(dt)[n_comma == 43]
  dt_2 <- copy(dt)[n_comma == 40]
  dt_3 <- copy(dt)[n_comma == 45]

  dt_1_sep <-
    dt_1 %>% separate(V1, paste0("C", c(1:48)), sep = c(",|\\$"))
  dt_2_sep <-
    dt_2 %>% separate(V1, paste0("C", c(1:44)), sep = c(",|\\$"))
  dt_3_sep <-
    dt_3 %>% separate(V1, paste0("C", c(1:50)), sep = c(",|\\$"))
  
  dt_1 <- dt_1_sep[-1] %>% setnames(c(
    paste0("GPGGA_", c(0:14)),
    paste0("GPZDA_", c(0:3, 6)),
    paste0("WIMDA_", c(0:20)),
    paste0("WIMWV_", c(0:5)),
    "index",
    "n_comma",
    "path"
  ))
  
  dt_2 <- dt_2_sep[-1] %>% setnames(c(
    paste0("GPGGA_", c(0:14)),
    paste0("GPZDA_", c(0:6)),
    paste0("WIMDA_", c(0:20)),
    "index",
    "n_comma",
    "path"
  ))
  
  dt_3 <- dt_3_sep[-1] %>% setnames(c(
    paste0("GPGGA_", c(0:14)),
    paste0("GPZDA_", c(0:6)),
    paste0("WIMDA_", c(0:20)),
    paste0("WIMWV_", c(0:5)),
    "index",
    "n_comma",
    "path"
  ))
  
  dt <- rbindlist(list(dt_3, dt_2, dt_1),
                  fill = T,
                  use.names = T)
  dt[, wind_sensor_unit := as.numeric(str_sub(basename(path), start = 9L, end = 10L))]
  
  dt %>% setorder(index)
  
  dt[, c("index",
         "n_comma",
         "path",
         "wind_sensor_unit") := NULL]
  dt
}

options(shiny.maxRequestSize = 50*1024^2)


ui <- fluidPage(
  titlePanel("Weather Station Data Converter"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      
      tags$div(
        tags$h4("Step 1: Choose your .txt file"),  # This is the title
        fileInput('file1', label = NULL,
                            accept = c('text/plain',
                                       '.txt'))
      ),
      

      tags$div(style = "margin-bottom: 150px;", 
               "This button allows you to upload the raw data in .txt format.",
               tags$br(),  # Line break added here
              "For NEA colleagues: uploading things are always NOT allowed on 
               SOE laptops."),
      
      tags$div(
        tags$h4("Step 2: Download Processed Data"),  # This is the title
        downloadButton('downloadData', 'Download')  # This is the button itself
      ),
      
      tags$div(style = "margin-top: 20px;", "This button allows you to download the processed data in .xlsx format."),
      
      tags$div(style = "margin-top: 100px; margin-bottom: 50px;", 
               "This tool is built to process the raw data from AIRMAR PB150 WeatherStations, 
  which may not be compatible with data from other sensors. The meaning of columns can be found",
               tags$a(href = "https://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=EABE0D34DB3D089142757DD0DE2327D1?doi=10.1.1.593.6789&rep=rep1&type=pdf", "here"), ". ", 
               tags$br(),  # Line break added here
               tags$br(),  # Line break added here
  "If you have questions, please contact ",
               tags$a(href = "https://www.researchgate.net/profile/Jiayu-Li-5", "Jiayu Li"), ", ", 
               tags$a(href = "mailto:jiayu.li@berkeley.edu", "jiayu.li@berkeley.edu"), "."
      ),
      
      tags$footer(HTML('<a rel="license" href="https://creativecommons.org/licenses/by/4.0/">
                   <img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a>
                   <br />This work is licensed under a <a rel="license" href="https://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.')
      )
    ),
    
    mainPanel(
      DTOutput('table')
    )
  )
)

server <- function(input, output) {
  # Placeholder for the dataset
  data <- reactiveVal()
  
  observe({
    file <- input$file1
    if (is.null(file)) {
      return(NULL)
    }
    
    # Read the uploaded file and set the reactiveVal
    
    df <-  read_wind_sensor(file$datapath)
    
    # df <- read_delim(file$datapath, delim = "\t") 
    # Add your reformatting and analysis here
    # e.g., df$new_col <- df$old_col * 2
    
    data(df)
  })
  
  output$table <- renderDT({
    datatable(data(), options = list(pageLength = 25, 
                                     lengthMenu = c(10, 25, 50, 100)))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("processed_data", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(data(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
