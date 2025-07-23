uji_proporsi_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Uji Proporsi"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(ns("uji_tipe"), "Jenis Uji Proporsi:",
                     choices = c("Satu Populasi" = "satu", "Dua Populasi" = "dua")),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'satu'", ns("uji_tipe")),
          numericInput(ns("x1"), "Jumlah sukses (x):", value = 10, min = 0),
          numericInput(ns("n1"), "Ukuran sampel (n):", value = 30, min = 1),
          numericInput(ns("p0"), "Proporsi hipotesis (p):", value = 0.5, min = 0, max = 1, step = 0.01)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'dua'", ns("uji_tipe")),
          numericInput(ns("x1_2"), "Jumlah sukses Populasi 1 (x₁):", value = 20, min = 0),
          numericInput(ns("n1_2"), "Ukuran sampel Populasi 1 (n₁):", value = 50, min = 1),
          numericInput(ns("x2_2"), "Jumlah sukses Populasi 2 (x₂):", value = 30, min = 0),
          numericInput(ns("n2_2"), "Ukuran sampel Populasi 2 (n₂):", value = 60, min = 1)
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

uji_proporsi_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    interpretasi_text <- reactiveVal("")
    
    observeEvent(input$run_test, {
      if (input$uji_tipe == "satu") {
        # Validasi input
        req(input$x1 <= input$n1)
        
        # Uji proporsi satu populasi
        hasil <- prop.test(x = input$x1, n = input$n1, p = input$p0, correct = FALSE)
        
        proporsi_sampel <- input$x1 / input$n1
        z_score <- (proporsi_sampel - input$p0) / sqrt(input$p0 * (1 - input$p0) / input$n1)
        
        interpretasi <- paste0(
          "=== HASIL UJI PROPORSI SATU POPULASI ===\n\n",
          "Data Input:\n",
          "- Jumlah sukses (x): ", input$x1, "\n",
          "- Ukuran sampel (n): ", input$n1, "\n",
          "- Proporsi hipotesis (p₀): ", input$p0, "\n\n",
          "Hasil Perhitungan:\n",
          "- Proporsi sampel (p̂): ", round(proporsi_sampel, 4), "\n",
          "- Z-score: ", round(z_score, 4), "\n",
          "- p-value: ", signif(hasil$p.value, 4), "\n",
          "- Confidence Interval (95%): [", round(hasil$conf.int[1], 4), ", ", round(hasil$conf.int[2], 4), "]\n\n",
          "Interpretasi:\n",
          "H₀: p = ", input$p0, " (proporsi populasi sama dengan hipotesis)\n",
          "H₁: p ≠ ", input$p0, " (proporsi populasi berbeda dengan hipotesis)\n\n",
          "Kesimpulan (α = 0.05):\n",
          ifelse(hasil$p.value < 0.05,
                 paste0("Tolak H₀. Terdapat perbedaan signifikan antara proporsi sampel (", 
                        round(proporsi_sampel, 4), ") dan proporsi hipotesis (", input$p0, ")."),
                 paste0("Gagal tolak H₀. Tidak terdapat perbedaan signifikan antara proporsi sampel (", 
                        round(proporsi_sampel, 4), ") dan proporsi hipotesis (", input$p0, ")."))
        )
        
      } else {
        # Validasi input
        req(input$x1_2 <= input$n1_2, input$x2_2 <= input$n2_2)
        
        # Uji proporsi dua populasi
        hasil <- prop.test(x = c(input$x1_2, input$x2_2),
                           n = c(input$n1_2, input$n2_2),
                           correct = FALSE)
        
        p1 <- input$x1_2 / input$n1_2
        p2 <- input$x2_2 / input$n2_2
        selisih_proporsi <- p1 - p2
        
        interpretasi <- paste0(
          "=== HASIL UJI PROPORSI DUA POPULASI ===\n\n",
          "Data Input:\n",
          "Populasi 1:\n",
          "- Jumlah sukses (x₁): ", input$x1_2, "\n",
          "- Ukuran sampel (n₁): ", input$n1_2, "\n",
          "- Proporsi sampel (p̂₁): ", round(p1, 4), "\n\n",
          "Populasi 2:\n",
          "- Jumlah sukses (x₂): ", input$x2_2, "\n",
          "- Ukuran sampel (n₂): ", input$n2_2, "\n",
          "- Proporsi sampel (p̂₂): ", round(p2, 4), "\n\n",
          "Hasil Perhitungan:\n",
          "- Selisih proporsi (p̂₁ - p̂₂): ", round(selisih_proporsi, 4), "\n",
          "- Chi-squared: ", round(hasil$statistic, 4), "\n",
          "- p-value: ", signif(hasil$p.value, 4), "\n",
          "- Confidence Interval untuk p₁ (95%): [", round(hasil$conf.int[1], 4), ", ", round(hasil$conf.int[2], 4), "]\n\n",
          "Interpretasi:\n",
          "H₀: p₁ = p₂ (proporsi kedua populasi sama)\n",
          "H₁: p₁ ≠ p₂ (proporsi kedua populasi berbeda)\n\n",
          "Kesimpulan (α = 0.05):\n",
          ifelse(hasil$p.value < 0.05,
                 paste0("Tolak H₀. Terdapat perbedaan signifikan antara proporsi populasi 1 (", 
                        round(p1, 4), ") dan populasi 2 (", round(p2, 4), ")."),
                 paste0("Gagal tolak H₀. Tidak terdapat perbedaan signifikan antara proporsi populasi 1 (", 
                        round(p1, 4), ") dan populasi 2 (", round(p2, 4), ")."))
        )
      }
      
      output$test_result <- renderPrint(hasil)
      output$test_interpretation <- renderText(interpretasi)
      interpretasi_text(interpretasi)
    })
    
    # Download hasil interpretasi
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("hasil_uji_proporsi_", Sys.Date(), ".docx")
      },
      content = function(file) {
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "LAPORAN UJI PROPORSI", style = "heading 1")
        doc <- officer::body_add_par(doc, paste("Tanggal:", Sys.Date()))
        doc <- officer::body_add_par(doc, "")
        
        # Split interpretasi berdasarkan baris untuk format yang lebih baik
        interpretasi_lines <- strsplit(interpretasi_text(), "\n")[[1]]
        for(line in interpretasi_lines) {
          if(grepl("===", line)) {
            doc <- officer::body_add_par(doc, gsub("===", "", line), style = "heading 2")
          } else if(line != "") {
            doc <- officer::body_add_par(doc, line)
          } else {
            doc <- officer::body_add_par(doc, "")
          }
        }
        
        print(doc, target = file)
      }
    )
  })
}