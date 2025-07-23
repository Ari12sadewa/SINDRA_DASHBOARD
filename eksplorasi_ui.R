eksplorasi_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 4,
        title = "Pilihan Variabel",
        solidHeader = TRUE,
        status = "primary",
        uiOutput(ns("var_selector"))
      ),
      box(
        width = 8,
        title = "Statistik Deskriptif",
        solidHeader = TRUE,
        status = "info",
        verbatimTextOutput(ns("summary_stats"))
      )
    ),
    
    actionButton(ns("load_map"), "Load Peta", icon = icon("map")),
    
    fluidRow(
      box(
        width = 12,
        title = "Visualisasi Peta",
        solidHeader = TRUE,
        status = "success",
        leafletOutput(ns("map_plot"), height = 500)
      )
    ),
    
    fluidRow(
      box(
        width = 10,
        title = "Matriks Korelasi",
        solidHeader = TRUE,
        status = "warning",
        plotOutput(ns("correlation_plot"), height = 400)
      ),
      box(
        width = 2,
        downloadButton(ns("dl_corr_png"), "PNG", class = "btn-sm"),
        br(), br(),
        downloadButton(ns("dl_corr_word"), "Word", class = "btn-sm")
      )
    ),
    
    fluidRow(
      verbatimTextOutput(ns("corr_interpretation"))
    ),
    
    fluidRow(
      box(
        width = 4,
        title = "Boxplot",
        solidHeader = TRUE,
        status = "primary",
        plotOutput(ns("boxplot"), height = 300),
        downloadButton(ns("dl_box_png"), "PNG", class = "btn-sm"),
        downloadButton(ns("dl_box_word"), "Word", class = "btn-sm")
      ),
      box(
        width = 4,
        title = "Histogram",
        solidHeader = TRUE,
        status = "info",
        plotOutput(ns("histogram"), height = 300),
        downloadButton(ns("dl_hist_png"), "PNG", class = "btn-sm"),
        downloadButton(ns("dl_hist_word"), "Word", class = "btn-sm")
      ),
      box(
        width = 4,
        title = "QQ Plot",
        solidHeader = TRUE,
        status = "success",
        plotOutput(ns("qqplot"), height = 300),
        downloadButton(ns("dl_qq_png"), "PNG", class = "btn-sm"),
        downloadButton(ns("dl_qq_word"), "Word", class = "btn-sm")
      )
    ),
    
    fluidRow(
      box(width = 4, verbatimTextOutput(ns("box_interpretation"))),
      box(width = 4, verbatimTextOutput(ns("hist_interpretation"))),
      box(width = 4, verbatimTextOutput(ns("qq_interpretation")))
    ),
    fluidRow(
      box(
        width = 6,
        title = "Top 5 dan Bottom 5 Kabupaten/Kota",
        solidHeader = TRUE,
        status = "warning",
        tableOutput(ns("top_bottom_table"))
      ),
      box(
        width = 6,
        title = "Diagram Batang",
        solidHeader = TRUE,
        status = "primary",
        plotOutput(ns("barplot_topbottom"))
      )
    ),
    fluidRow(
      box(width = 12, verbatimTextOutput(ns("barplot_interpretation")))
    )
    
  )
}

eksplorasi_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Pastikan data tersedia
    req(data_main, data_area)
    
    # UI: Pilih variabel numerik
    output$var_selector <- renderUI({
      numeric_vars <- names(dplyr::select_if(data_main, is.numeric))
      if (length(numeric_vars) == 0) return(p("Tidak ada variabel numerik"))
      selectInput(ns("selected_var"), "Pilih Variabel:", choices = numeric_vars, selected = numeric_vars[1])
    })
    
    # Statistik deskriptif dari variabel terpilih
    output$summary_stats <- renderPrint({
      req(input$selected_var)
      summary(data_main[[input$selected_var]])
    })
    
    # Data gabungan antara data_area dan data_main untuk peta
    data_joined <- eventReactive(input$load_map, {
      req(input$selected_var)
      merged <- data_main %>%
        dplyr::left_join(data_area, by = c("DISTRICTCODE" = "kodeprkab"))
      merged <- st_as_sf(merged)
      return(merged)
    })
    
    # Peta Leaflet berdasarkan variabel
    output$map_plot <- renderLeaflet({
      data <- data_joined()
      View(data)
      View(input$selected_var)
      req(data)
      var_name <- input$selected_var
      req(var_name %in% names(data))
      var_values <- data[[var_name]]
      
      if (all(is.na(var_values))) {
        return(leaflet() %>% addTiles() %>% addControl("Data kosong untuk variabel ini", position = "topright"))
      }
      
      pal <- colorNumeric("YlOrRd", domain = var_values, na.color = "transparent")
      
      leaflet(data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(var_values),
          weight = 1, opacity = 1, color = "white", fillOpacity = 0.7,
          highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
          label = ~lapply(paste0("<strong>", nmkab, "</strong><br/>", var_name, ": ", format(var_values, digits = 2, big.mark = ",")), HTML)
        ) %>%
        addLegend(pal = pal, values = var_values, title = var_name, position = "bottomright")
    })
    
    # Matriks Korelasi antar variabel numerik
    output$correlation_plot <- renderPlot({
      numeric_vars <- data_main %>% select(where(is.numeric))
      if (ncol(numeric_vars) < 2) {
        plot.new()
        text(0.5, 0.5, "Tidak cukup variabel numerik untuk korelasi", cex = 1.2)
      } else {
        corr_matrix <- cor(numeric_vars, use = "complete.obs")
        corrplot::corrplot(corr_matrix, method = "color", type = "upper",
                           tl.col = "black", addCoef.col = "black", number.cex = 0.7)
      }
    })
    
    # Visualisasi Boxplot
    output$boxplot <- renderPlot({
      req(input$selected_var)
      boxplot(data_main[[input$selected_var]], main = paste("Boxplot", input$selected_var))
    })
    
    # Visualisasi Histogram dengan overlay kurva normal
    output$histogram <- renderPlot({
      req(input$selected_var)
      data_var <- data_main[[input$selected_var]]
      data_var <- data_var[!is.na(data_var)]
      
      hist(data_var, 
           main = paste("Histogram", input$selected_var),
           xlab = input$selected_var,
           ylab = "Frekuensi",
           col = "lightblue",
           border = "black",
           breaks = "Sturges")
      
      if (length(data_var) > 1) {
        curve(dnorm(x, mean = mean(data_var), sd = sd(data_var)) * length(data_var) * diff(hist(data_var, plot = FALSE)$breaks)[1], 
              add = TRUE, col = "red", lwd = 2)
      }
    })
    
    # Visualisasi QQ Plot
    output$qqplot <- renderPlot({
      req(input$selected_var)
      qqnorm(data_main[[input$selected_var]], main = paste("QQ Plot", input$selected_var))
      qqline(data_main[[input$selected_var]])
    })
    
    # Interpretasi Korelasi
    output$corr_interpretation <- renderText({
      numeric_vars <- data_main %>% select(where(is.numeric))
      if (ncol(numeric_vars) >= 2) {
        corr_matrix <- cor(numeric_vars, use = "complete.obs")
        max_corr <- max(abs(corr_matrix[upper.tri(corr_matrix)]), na.rm = TRUE)
        paste("Korelasi tertinggi:", round(max_corr, 3), 
              ifelse(max_corr > 0.7, "(sangat kuat)", ifelse(max_corr > 0.5, "(kuat)", "(lemah)")))
      }
    })
    
    # Interpretasi Boxplot
    output$box_interpretation <- renderText({
      req(input$selected_var)
      data_var <- data_main[[input$selected_var]]
      stats <- boxplot.stats(data_var)
      outliers <- length(stats$out)
      q1 <- quantile(data_var, 0.25, na.rm = TRUE)
      q3 <- quantile(data_var, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      skewness <- (3 * (mean(data_var, na.rm = TRUE) - median(data_var, na.rm = TRUE))) / sd(data_var, na.rm = TRUE)
      
      paste0("Outliers: ", outliers, " data\n",
             "IQR: ", round(iqr, 2), "\n",
             "Skewness: ", round(skewness, 2), " ",
             ifelse(abs(skewness) < 0.5, "(simetris)", 
                    ifelse(skewness > 0, "(miring kanan)", "(miring kiri)")))
    })
    
    # Interpretasi Histogram
    output$hist_interpretation <- renderText({
      req(input$selected_var)
      data_var <- data_main[[input$selected_var]]
      data_var <- data_var[!is.na(data_var)]
      
      mean_val <- mean(data_var)
      median_val <- median(data_var)
      sd_val <- sd(data_var)
      skewness <- (3 * (mean_val - median_val)) / sd_val
      
      shape <- ifelse(abs(skewness) < 0.5, "simetris/normal", 
                      ifelse(skewness > 0.5, "miring kanan (positif)", "miring kiri (negatif)"))
      
      hist_data <- hist(data_var, plot = FALSE)
      peaks <- length(which(diff(sign(diff(hist_data$counts))) == -2)) + 1
      modality <- ifelse(peaks == 1, "unimodal", ifelse(peaks == 2, "bimodal", "multimodal"))
      
      paste0("Bentuk distribusi: ", shape, "\n",
             "Mean: ", round(mean_val, 2), "\n",
             "Median: ", round(median_val, 2), "\n",
             "Std Dev: ", round(sd_val, 2), "\n",
             "Modalitas: ", modality)
    })
    
    # Interpretasi QQ Plot
    output$qq_interpretation <- renderText({
      req(input$selected_var)
      data_var <- data_main[[input$selected_var]]
      shapiro_p <- shapiro.test(sample(data_var, min(5000, length(data_var))))$p.value
      paste("Normalitas (p-value):", round(shapiro_p, 4), 
            ifelse(shapiro_p > 0.05, "(Normal)", "(Tidak Normal)"))
    })
    
    # Tabel Top 5 dan Bottom 5
    output$top_bottom_table <- renderTable({
      req(input$selected_var)
      var_data <- data_main %>%
        left_join(data_area, by = c("DISTRICTCODE"= "kodeprkab")) %>%
        select(nmkab, all_of(input$selected_var)) %>%
        filter(!is.na(.data[[input$selected_var]])) %>%
        arrange(desc(.data[[input$selected_var]])) 
      
      top5 <- head(var_data, 5)
      bottom5 <- tail(var_data, 5)
      rbind(top5, bottom5)
    }, digits = 2)
    
    # Diagram batang untuk top-bottom 5
    output$barplot_topbottom <- renderPlot({
      req(input$selected_var)
      var_data <- data_main %>%
        left_join(data_area, by = c("DISTRICTCODE"= "kodeprkab")) %>%
        select(nmkab, all_of(input$selected_var)) %>%
        filter(!is.na(.data[[input$selected_var]])) %>%
        arrange(desc(.data[[input$selected_var]]))
      
      top5 <- head(var_data, 5)
      bottom5 <- tail(var_data, 5)
      plot_data <- rbind(top5, bottom5)
      
      barplot(height = plot_data[[input$selected_var]],
              names.arg = plot_data$nmkab,
              las = 2,
              col = "steelblue",
              main = paste("Top & Bottom 5", input$selected_var),
              ylab = input$selected_var)
    })
    
    # Interpretasi barplot
    output$barplot_interpretation <- renderText({
      req(input$selected_var)
      var_data <- data_main[[input$selected_var]]
      mean_val <- round(mean(var_data, na.rm = TRUE), 2)
      max_val <- round(max(var_data, na.rm = TRUE), 2)
      min_val <- round(min(var_data, na.rm = TRUE), 2)
      
      paste0("Variabel '", input$selected_var, "' memiliki nilai maksimum sebesar ", max_val,
             ", minimum sebesar ", min_val, ", dan rata-rata sekitar ", mean_val,
             ". Diagram batang dan tabel menunjukkan kabupaten/kota dengan nilai tertinggi dan terendah.")
    })
    
    # Download handlers
    output$dl_corr_png <- downloadHandler(
      filename = "correlation_matrix.png",
      content = function(file) {
        png(file, width = 800, height = 600)
        numeric_vars <- data_main %>% select(where(is.numeric))
        if (ncol(numeric_vars) >= 2) {
          corr_matrix <- cor(numeric_vars, use = "complete.obs")
          corrplot::corrplot(corr_matrix, method = "color", type = "upper",
                             tl.col = "black", addCoef.col = "black", number.cex = 0.7)
        }
        dev.off()
      }
    )
    
    output$dl_box_png <- downloadHandler(
      filename = "boxplot.png",
      content = function(file) {
        png(file, width = 600, height = 400)
        boxplot(data_main[[input$selected_var]], main = paste("Boxplot", input$selected_var))
        dev.off()
      }
    )
    
    output$dl_hist_png <- downloadHandler(
      filename = "histogram.png",
      content = function(file) {
        png(file, width = 600, height = 400)
        data_var <- data_main[[input$selected_var]]
        data_var <- data_var[!is.na(data_var)]
        
        hist(data_var, 
             main = paste("Histogram", input$selected_var),
             xlab = input$selected_var,
             ylab = "Frekuensi",
             col = "lightblue",
             border = "black",
             breaks = "Sturges")
        
        if (length(data_var) > 1) {
          curve(dnorm(x, mean = mean(data_var), sd = sd(data_var)) * length(data_var) * diff(hist(data_var, plot = FALSE)$breaks)[1], 
                add = TRUE, col = "red", lwd = 2)
        }
        dev.off()
      }
    )
    
    output$dl_qq_png <- downloadHandler(
      filename = "qqplot.png",
      content = function(file) {
        png(file, width = 600, height = 400)
        qqnorm(data_main[[input$selected_var]], main = paste("QQ Plot", input$selected_var))
        qqline(data_main[[input$selected_var]])
        dev.off()
      }
    )
    
    # Word download handlers
    output$dl_corr_word <- downloadHandler(
      filename = "correlation_report.docx",
      content = function(file) {
        numeric_vars <- data_main %>% select(where(is.numeric))
        interpretation <- if (ncol(numeric_vars) >= 2) {
          corr_matrix <- cor(numeric_vars, use = "complete.obs")
          max_corr <- max(abs(corr_matrix[upper.tri(corr_matrix)]), na.rm = TRUE)
          paste("Korelasi tertinggi:", round(max_corr, 3), 
                ifelse(max_corr > 0.7, "(sangat kuat)", ifelse(max_corr > 0.5, "(kuat)", "(lemah)")))
        } else "Tidak cukup variabel numerik"
        
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Analisis Matriks Korelasi")
        doc <- officer::body_add_par(doc, interpretation)
        print(doc, target = file)
      }
    )
    
    output$dl_box_word <- downloadHandler(
      filename = "boxplot_report.docx",
      content = function(file) {
        req(input$selected_var)
        data_var <- data_main[[input$selected_var]]
        stats <- boxplot.stats(data_var)
        outliers <- length(stats$out)
        q1 <- quantile(data_var, 0.25, na.rm = TRUE)
        q3 <- quantile(data_var, 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        skewness <- (3 * (mean(data_var, na.rm = TRUE) - median(data_var, na.rm = TRUE))) / sd(data_var, na.rm = TRUE)
        
        interpretation <- paste0("Outliers: ", outliers, " data\n",
                                 "IQR: ", round(iqr, 2), "\n",
                                 "Skewness: ", round(skewness, 2), " ",
                                 ifelse(abs(skewness) < 0.5, "(simetris)", 
                                        ifelse(skewness > 0, "(miring kanan)", "(miring kiri)")))
        
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Analisis Boxplot")
        doc <- officer::body_add_par(doc, interpretation)
        print(doc, target = file)
      }
    )
    
    output$dl_hist_word <- downloadHandler(
      filename = "histogram_report.docx",
      content = function(file) {
        req(input$selected_var)
        data_var <- data_main[[input$selected_var]]
        data_var <- data_var[!is.na(data_var)]
        
        mean_val <- mean(data_var)
        median_val <- median(data_var)
        sd_val <- sd(data_var)
        skewness <- (3 * (mean_val - median_val)) / sd_val
        
        shape <- ifelse(abs(skewness) < 0.5, "simetris/normal", 
                        ifelse(skewness > 0.5, "miring kanan (positif)", "miring kiri (negatif)"))
        
        hist_data <- hist(data_var, plot = FALSE)
        peaks <- length(which(diff(sign(diff(hist_data$counts))) == -2)) + 1
        modality <- ifelse(peaks == 1, "unimodal", ifelse(peaks == 2, "bimodal", "multimodal"))
        
        interpretation <- paste0("Bentuk distribusi: ", shape, "\n",
                                 "Mean: ", round(mean_val, 2), "\n",
                                 "Median: ", round(median_val, 2), "\n",
                                 "Std Dev: ", round(sd_val, 2), "\n",
                                 "Modalitas: ", modality)
        
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Analisis Histogram")
        doc <- officer::body_add_par(doc, interpretation)
        print(doc, target = file)
      }
    )
    
    output$dl_qq_word <- downloadHandler(
      filename = "qq_report.docx",
      content = function(file) {
        req(input$selected_var)
        data_var <- data_main[[input$selected_var]]
        shapiro_p <- shapiro.test(sample(data_var, min(5000, length(data_var))))$p.value
        interpretation <- paste("Normalitas (p-value):", round(shapiro_p, 4), 
                                ifelse(shapiro_p > 0.05, "(Normal)", "(Tidak Normal)"))
        
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Analisis QQ Plot")
        doc <- officer::body_add_par(doc, interpretation)
        print(doc, target = file)
      }
    )
  })
}