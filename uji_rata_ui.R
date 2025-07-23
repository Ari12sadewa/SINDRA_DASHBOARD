data_sovi <- read.csv("data/sovi_data.csv")

uji_rata_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Uji Rata-Rata"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(ns("uji_tipe"), "Jenis Uji:",
                     choices = c("Satu Populasi" = "satu", "Dua Populasi" = "dua")),
        selectInput(ns("var1"), "Variabel 1:", choices = names(data_sovi)[-1]),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'dua'", ns("uji_tipe")),
          selectInput(ns("var2"), "Variabel 2:", choices = names(data_sovi)[-1])
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'satu'", ns("uji_tipe")),
          numericInput(ns("mu"), "Hipotesis rata-rata (μ):", value = 0)
        ),
        actionButton(ns("run_test"), "Jalankan Uji"),
        br(), br(),
        downloadButton(ns("download_report"), "Unduh Interpretasi")
      ),
      mainPanel(
        verbatimTextOutput(ns("test_result")),
        textOutput(ns("test_interpretation"))
      )
    )
  )
}

uji_rata_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    interpretasi_text <- reactiveVal("")
    
    observeEvent(input$run_test, {
      var1 <- data_sovi[[input$var1]]
      var1 <- na.omit(var1)
      
      if (input$uji_tipe == "satu") {
        mu <- input$mu
        result <- t.test(var1, mu = mu)
        interpretasi <- paste0(
          "Hasil uji rata-rata satu populasi untuk variabel ", input$var1, ":\n",
          "p-value = ", signif(result$p.value, 4), "\n",
          ifelse(result$p.value < 0.05,
                 "Kesimpulan: Terdapat perbedaan signifikan antara rata-rata sampel dan nilai μ yang diuji.",
                 "Kesimpulan: Tidak terdapat perbedaan signifikan antara rata-rata sampel dan nilai μ yang diuji.")
        )
      } else {
        var2 <- data_sovi[[input$var2]]
        result <- t.test(var1, na.omit(var2))
        interpretasi <- paste0(
          "Hasil uji rata-rata dua populasi antara variabel ", input$var1, " dan ", input$var2, ":\n",
          "p-value = ", signif(result$p.value, 4), "\n",
          ifelse(result$p.value < 0.05,
                 "Kesimpulan: Rata-rata kedua populasi berbeda secara signifikan.",
                 "Kesimpulan: Tidak ada perbedaan signifikan antara rata-rata kedua populasi.")
        )
      }
      
      output$test_result <- renderPrint(result)
      output$test_interpretation <- renderText(interpretasi)
      interpretasi_text(interpretasi)
    })
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("hasil_uji_rata_", Sys.Date(), ".docx")
      },
      content = function(file) {
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Laporan Uji Rata-Rata", style = "heading 1")
        doc <- officer::body_add_par(doc, "Interpretasi Uji Rata-Rata", style = "heading 2")
        doc <- officer::body_add_par(doc, interpretasi_text())
        print(doc, target = file)
      }
    )
  })
}