uji_varians_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Uji Varians"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("var1_var"), "Variabel 1:", choices = names(data_sovi)[-1]),
        selectInput(ns("var2_var"), "Variabel 2:", choices = names(data_sovi)[-1]),
        actionButton(ns("run_var"), "Jalankan Uji"),
        br(), br(),
        downloadButton(ns("dl_var_png"), "Download PNG"),
        downloadButton(ns("dl_var_word"), "Download Word")
      ),
      mainPanel(
        verbatimTextOutput(ns("var_result")),
        verbatimTextOutput(ns("var_interpretation")),
        plotOutput(ns("variance_plot"))
      )
    )
  )
}

server_uji_varians <- function(input, output, session) {
  ns <- session$ns
  
  results <- reactiveVal(NULL)
  observeEvent(input$run_var, {
    req(input$var1_var, input$var2_var)
    
    data1 <- data_sovi[[input$var1_var]]
    data2 <- data_sovi[[input$var2_var]]
    
    test <- var.test(data1, data2)
    
    interpretation <- paste0(
      "Uji F Varians:\n",
      "H0: Varians sama\nH1: Varians berbeda\n\n",
      "F = ", round(test$statistic, 4), ", p = ", format.pval(test$p.value, digits = 4), "\n",
      "Kesimpulan: ", ifelse(test$p.value < 0.05, "Tolak H0", "Gagal tolak H0")
    )
    
    results(list(
      var1 = input$var1_var,
      var2 = input$var2_var,
      data1 = data1,
      data2 = data2,
      test = test,
      interp = interpretation
    ))
  })
  
  output$var_result <- renderPrint({
    req(results())
    results()$test
  })
  
  output$var_interpretation <- renderText({
    req(results())
    results()$interp
  })
  
  output$variance_plot <- renderPlot({
    req(results())
    dat <- data.frame(
      value = c(results()$data1, results()$data2),
      group = rep(c(results()$var1, results()$var2), 
                  times = c(length(results()$data1), length(results()$data2)))
    )
    boxplot(value ~ group, data = dat, col = c("skyblue", "salmon"),
            main = "Perbandingan Varians", ylab = "Nilai")
  })
  
  output$dl_var_png <- downloadHandler(
    filename = function() paste0("uji_varians_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 800, height = 600)
      req(results())
      dat <- data.frame(
        value = c(results()$data1, results()$data2),
        group = rep(c(results()$var1, results()$var2), 
                    times = c(length(results()$data1), length(results()$data2)))
      )
      boxplot(value ~ group, data = dat, col = c("skyblue", "salmon"),
              main = "Perbandingan Varians", ylab = "Nilai")
      dev.off()
    }
  )
  
  output$dl_var_word <- downloadHandler(
    filename = function() paste0("laporan_uji_varians_", Sys.Date(), ".docx"),
    content = function(file) {
      req(results())
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "Laporan Uji Varians", style = "heading 1")
      doc <- officer::body_add_par(doc, results()$interp)
      print(doc, target = file)
    }
  )
}

