uji_varians_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Uji Varians"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("var1"), "Variabel 1:", choices = names(data_sovi)[-1]),
        selectInput(ns("var2"), "Variabel 2:", choices = names(data_sovi)[-1]),
        actionButton(ns("run_test"), "Jalankan Uji"),
        br(), br(),
        downloadButton(ns("download_png"), "Download PNG"),
        downloadButton(ns("download_report"), "Download Word")
      ),
      mainPanel(
        verbatimTextOutput(ns("test_result")),
        textOutput(ns("test_interpretation")),
        plotOutput(ns("variance_plot"))
      )
    )
  )
}

uji_varians_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    interpretasi_text <- reactiveVal("")
    plot_data <- reactiveVal(NULL)
    
    observeEvent(input$run_test, {
      # Ambil data dan bersihkan NA 
      var1 <- data_sovi[[input$var1]]
      var2 <- data_sovi[[input$var2]]
      var1 <- na.omit(var1)
      var2 <- na.omit(var2)
      
      # Jalankan uji varians
      result <- var.test(var1, var2)
      
      # Buat interpretasi 
      interpretasi <- paste0(
        "Hasil uji varians antara variabel ", input$var1, " dan ", input$var2, ":\n",
        "F-statistic = ", round(result$statistic, 4), "\n",
        "p-value = ", signif(result$p.value, 4), "\n",
        ifelse(result$p.value < 0.05,
               "Kesimpulan: Varians kedua populasi berbeda secara signifikan.",
               "Kesimpulan: Tidak ada perbedaan signifikan antara varians kedua populasi.")
      )
      
      # Simpan data untuk plot
      plot_data(data.frame(
        value = c(var1, var2),
        group = rep(c(input$var1, input$var2), 
                    times = c(length(var1), length(var2)))
      ))
      
      # Output hasil
      output$test_result <- renderPrint(result)
      output$test_interpretation <- renderText(interpretasi)
      interpretasi_text(interpretasi)
    })
    
    # Plot 
    output$variance_plot <- renderPlot({
      req(plot_data())
      dat <- plot_data()
      boxplot(value ~ group, data = dat, col = c("skyblue", "salmon"),
              main = "Perbandingan Varians", ylab = "Nilai")
    })
    
    # Download PNG
    output$download_png <- downloadHandler(
      filename = function() paste0("uji_varians_", Sys.Date(), ".png"),
      content = function(file) {
        png(file, width = 800, height = 600)
        req(plot_data())
        dat <- plot_data()
        boxplot(value ~ group, data = dat, col = c("skyblue", "salmon"),
                main = "Perbandingan Varians", ylab = "Nilai")
        dev.off()
      }
    )
    
    # Download Word 
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("hasil_uji_varians_", Sys.Date(), ".docx")
      },
      content = function(file) {
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Laporan Uji Varians", style = "heading 1")
        doc <- officer::body_add_par(doc, "Interpretasi Uji Varians", style = "heading 2")
        doc <- officer::body_add_par(doc, interpretasi_text())
        print(doc, target = file)
      }
    )
  })
}
