asumsi_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "asumsi",
    fluidRow(
      box(
        width = 4,
        title = "Pengaturan Uji Asumsi",
        selectInput(ns("selected_var"), "Pilih Variabel:", choices = names(data_main)),
        selectInput(ns("normal_method"), "Metode Uji Kenormalan:",
                    choices = c("Kolmogorov-Smirnov" = "ks",
                                "Shapiro-Wilk" = "shapiro",
                                "Lilliefors" = "lillie",
                                "Jarque-Bera" = "jb")),
        actionButton(ns("run_test"), "Jalankan Uji Asumsi"),
        downloadButton(ns("download_report"), "Unduh Hasil")
      ),
      box(
        width = 8,
        title = "Hasil dan Grafik",
        verbatimTextOutput(ns("normal_result")),
        splitLayout(
          plotOutput(ns("hist_plot")),
          plotOutput(ns("qq_plot"))
        )
      )
    )
  )
}


asumsi_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    library(nortest)
    library(tseries)
    
    selected_data <- reactive({
      req(input$selected_var)
      data_main[[input$selected_var]]
    })
    
    result <- reactiveVal(NULL)
    
    observeEvent(input$run_test, {
      x <- selected_data()
      method <- input$normal_method
      
      if (method == "shapiro") {
        test <- shapiro.test(x)
      } else if (method == "ks") {
        test <- ks.test(scale(x), "pnorm")
      } else if (method == "lillie") {
        test <- lillie.test(x)
      } else if (method == "jb") {
        test <- jarque.bera.test(x)
      } else {
        test <- list(statistic = NA, p.value = NA, method = "Unknown")
      }
      
      interpretation <- if (test$p.value < 0.05) {
        "Data tidak berdistribusi normal (tolak H0)"
      } else {
        "Data berdistribusi normal (gagal tolak H0)"
      }
      
      result(list(
        test = test,
        interpretation = interpretation
      ))
    })
    
    output$normal_result <- renderPrint({
      req(result())
      res <- result()
      cat("Hasil Uji Kenormalan:", res$test$method, "\n")
      print(res$test)
      cat("\nInterpretasi:", res$interpretation)
    })
    
    output$hist_plot <- renderPlot({
      req(selected_data())
      hist(selected_data(), main = "Histogram", xlab = input$selected_var, col = "skyblue", border = "white")
    })
    
    output$qq_plot <- renderPlot({
      req(selected_data())
      qqnorm(selected_data(), main = "QQ Plot")
      qqline(selected_data(), col = "red")
    })
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Hasil_Uji_Asumsi_", input$selected_var, ".docx")
      },
      content = function(file) {
        tempReport <- tempfile(fileext = ".Rmd")
        file.copy("report_asumsi.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(
          var = input$selected_var,
          test_result = result()$test,
          interpretation = result()$interpretation,
          data_x = selected_data()
        )
        
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
      }
    )
  })
}


