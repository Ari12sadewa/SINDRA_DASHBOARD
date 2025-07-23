data_management_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Filter Berdasarkan Nama Provinsi",
        solidHeader = TRUE,
        status = "primary",
        uiOutput(ns("region_selector")),
        actionButton(ns("confirm_filter"), "Konfirmasi Perubahan", icon = icon("check")),
        br(), br(),
        uiOutput(ns("column_selector")),
        br(),
        fluidRow(
          column(6, downloadButton(ns("download_csv"), "Download CSV", icon = icon("file-csv"))),
          column(6, downloadButton(ns("download_xlsx"), "Download Excel", icon = icon("file-excel")))
        ),
        br(),
        DT::dataTableOutput(ns("data_preview"))
      )
    )
  )
}

data_management_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Peta Kode ke Nama Provinsi ---
    prov_mapping <- c(
      "11" = "Aceh", "12" = "Sumatera Utara", "13" = "Sumatera Barat", "14" = "Riau",
      "15" = "Jambi", "16" = "Sumatera Selatan", "17" = "Bengkulu", "18" = "Lampung",
      "19" = "Bangka Belitung", "21" = "Kepulauan Riau", "31" = "DKI Jakarta", "32" = "Jawa Barat",
      "33" = "Jawa Tengah", "34" = "DI Yogyakarta", "35" = "Jawa Timur", "36" = "Banten",
      "51" = "Bali", "52" = "Nusa Tenggara Barat", "53" = "Nusa Tenggara Timur",
      "61" = "Kalimantan Barat", "62" = "Kalimantan Tengah", "63" = "Kalimantan Selatan",
      "64" = "Kalimantan Timur", "65" = "Kalimantan Utara",
      "71" = "Sulawesi Utara", "72" = "Sulawesi Tengah", "73" = "Sulawesi Selatan",
      "74" = "Sulawesi Tenggara", "75" = "Gorontalo", "76" = "Sulawesi Barat",
      "81" = "Maluku", "82" = "Maluku Utara", "91" = "Papua", "92" = "Papua Barat"
    )
    
    data_sovi_local <- reactive({
      req(exists("data_sovi", envir = .GlobalEnv))
      get("data_sovi", envir = .GlobalEnv)
    })
    
    output$region_selector <- renderUI({
      df <- data_sovi_local()
      prov_codes <- unique(substr(df[[1]], 1, 2))
      nama_prov <- prov_mapping[prov_codes]
      names(nama_prov) <- prov_mapping[prov_codes]
      selectInput(ns("regionchoice"), "Pilih Provinsi", choices = nama_prov, selected = nama_prov[1], multiple = TRUE)
    })
    
    # Filter data sementara berdasarkan provinsi
    filtered_data_temp <- reactive({
      df <- data_sovi_local()
      
      # Jika tidak ada pilihan, kembalikan semua data
      if (is.null(input$regionchoice) || length(input$regionchoice) == 0) {
        return(df)
      }
      
      kode_selected <- names(prov_mapping)[prov_mapping %in% input$regionchoice]
      df[substr(df[[1]], 1, 2) %in% kode_selected, ]
    })
    
    # : Pilih kolom untuk ditampilkan - EXCLUDE kolom pertama dari pilihan
    output$column_selector <- renderUI({
      df <- filtered_data_temp()
      available_columns <- names(df)[-1]  # Exclude kolom pertama
      
      selectInput(
        ns("columnchoice"), 
        "Pilih Kolom Tambahan (kolom pertama akan selalu disertakan)",
        choices = available_columns,
        selected = available_columns,  # Default: pilih semua kolom kecuali kolom pertama
        multiple = TRUE
      )
    })
    
    # : Simpan ke global - selalu sertakan kolom pertama
    observeEvent(input$confirm_filter, {
      df_original <- get("data_sovi", envir = .GlobalEnv)
      first_col_name <- names(df_original)[1]
      
      if (is.null(input$regionchoice) || length(input$regionchoice) == 0) {
        # Jika tidak ada provinsi dipilih, gunakan semua data dengan kolom yang dipilih
        if (!is.null(input$columnchoice) && length(input$columnchoice) > 0) {
          selected_cols <- c(first_col_name, input$columnchoice)
          data_main <<- df_original[, selected_cols, drop = FALSE]
        } else {
          # Jika tidak ada kolom yang dipilih, minimal sertakan kolom pertama
          data_main <<- df_original[, first_col_name, drop = FALSE]
        }
        showNotification("Tidak ada provinsi dipilih. Menggunakan seluruh data_sovi dengan kolom yang dipilih.", type = "warning")
      } else {
        # Filter berdasarkan provinsi
        kode_selected <- names(prov_mapping)[prov_mapping %in% input$regionchoice]
        df_filtered <- df_original %>%
          filter(substr(.[[1]], 1, 2) %in% kode_selected)
        
        # Pilih kolom (selalu sertakan kolom pertama)
        if (!is.null(input$columnchoice) && length(input$columnchoice) > 0) {
          selected_cols <- c(first_col_name, input$columnchoice)
          data_main <<- df_filtered[, selected_cols, drop = FALSE]
        } else {
          # Jika tidak ada kolom yang dipilih, minimal sertakan kolom pertama
          data_main <<- df_filtered[, first_col_name, drop = FALSE]
        }
        showNotification("Perubahan data_main berhasil disimpan dengan kolom pertama disertakan!", type = "message")
      }
    })
    
    # : Tampilkan datatable - selalu sertakan kolom pertama
    output$data_preview <- DT::renderDataTable({
      df <- filtered_data_temp()
      first_col_name <- names(df)[1]
      
      if (!is.null(input$columnchoice) && length(input$columnchoice) > 0) {
        # Gabungkan kolom pertama dengan kolom yang dipilih
        selected_cols <- c(first_col_name, input$columnchoice)
        # Pastikan tidak ada duplikasi kolom
        selected_cols <- unique(selected_cols)
        df <- df[, selected_cols, drop = FALSE]
      } else {
        # Jika tidak ada kolom yang dipilih, tampilkan hanya kolom pertama
        df <- df[, first_col_name, drop = FALSE]
      }
      
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # : Download handler CSV - selalu sertakan kolom pertama
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("data_filtered_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- filtered_data_temp()
        first_col_name <- names(df)[1]
        
        if (!is.null(input$columnchoice) && length(input$columnchoice) > 0) {
          selected_cols <- c(first_col_name, input$columnchoice)
          selected_cols <- unique(selected_cols)
          df <- df[, selected_cols, drop = FALSE]
        } else {
          df <- df[, first_col_name, drop = FALSE]
        }
        
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    # : Download handler XLSX - selalu sertakan kolom pertama
    output$download_xlsx <- downloadHandler(
      filename = function() {
        paste0("data_filtered_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        df <- filtered_data_temp()
        first_col_name <- names(df)[1]
        
        if (!is.null(input$columnchoice) && length(input$columnchoice) > 0) {
          selected_cols <- c(first_col_name, input$columnchoice)
          selected_cols <- unique(selected_cols)
          df <- df[, selected_cols, drop = FALSE]
        } else {
          df <- df[, first_col_name, drop = FALSE]
        }
        
        writexl::write_xlsx(df, path = file)
      }
    )
  })
}
