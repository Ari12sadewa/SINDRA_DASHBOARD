regresi_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 6,
        title = "Pemilihan Variabel",
        solidHeader = TRUE,
        status = "primary",
        uiOutput(ns("y_selector")),
        uiOutput(ns("x_selector")),
        br(),
        actionButton(ns("run_regression"), "Jalankan Regresi", 
                     icon = icon("play"), class = "btn-success")
      ),
      box(
        width = 6,
        title = "Model Summary",
        solidHeader = TRUE,
        status = "info",
        verbatimTextOutput(ns("model_summary"))
      )
    ),
    
    fluidRow(
      box(
        width = 8,
        title = "Diagnostic Plots",
        solidHeader = TRUE,
        status = "warning",
        plotOutput(ns("diagnostic_plots"), height = 600)
      ),
      box(
        width = 4,
        title = "Uji Asumsi",
        solidHeader = TRUE,
        status = "success",
        verbatimTextOutput(ns("assumption_tests")),
        br(),
        downloadButton(ns("dl_full_report"), "Download Laporan Lengkap", 
                       icon = icon("download"), class = "btn-primary")
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Interpretasi Model",
        solidHeader = TRUE,
        status = "info",
        verbatimTextOutput(ns("model_interpretation"))
      )
    )
  )
}

regresi_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive untuk menyimpan model
    regression_model <- reactiveVal(NULL)
    model_data <- reactiveVal(NULL)
    
    # UI untuk pemilihan variabel
    output$y_selector <- renderUI({
      req(data_main)
      numeric_vars <- names(dplyr::select_if(data_main, is.numeric))
      if (length(numeric_vars) == 0) return(p("Tidak ada variabel numerik"))
      
      selectInput(
        ns("y_var"),
        "Variabel Dependen (Y):",
        choices = numeric_vars,
        selected = numeric_vars[1]
      )
    })
    
    output$x_selector <- renderUI({
      req(input$y_var)
      numeric_vars <- names(dplyr::select_if(data_main, is.numeric))
      x_choices <- numeric_vars[numeric_vars != input$y_var]
      
      if (length(x_choices) == 0) return(p("Tidak ada variabel independen"))
      
      checkboxGroupInput(
        ns("x_vars"),
        "Variabel Independen (X):",
        choices = x_choices,
        selected = x_choices[1:min(3, length(x_choices))]
      )
    })
    
    # Jalankan regresi
    observeEvent(input$run_regression, {
      req(input$y_var, input$x_vars)
      
      # Siapkan data
      model_vars <- c(input$y_var, input$x_vars)
      data_clean <- data_main[, model_vars, drop = FALSE] %>%
        na.omit()
      
      if (nrow(data_clean) < 10) {
        showNotification("Data terlalu sedikit setelah menghapus missing values", 
                         type = "error")
        return()
      }
      
      # Buat formula
      formula_str <- paste(input$y_var, "~", paste(input$x_vars, collapse = " + "))
      
      # Jalankan regresi
      model <- lm(as.formula(formula_str), data = data_clean)
      
      regression_model(model)
      model_data(data_clean)
    })
    
    # Output model summary
    output$model_summary <- renderPrint({
      model <- regression_model()
      if (is.null(model)) return("Silakan jalankan regresi terlebih dahulu")
      
      summary(model)
    })
    
    # Diagnostic plots
    output$diagnostic_plots <- renderPlot({
      model <- regression_model()
      req(model)
      
      par(mfrow = c(2, 2))
      plot(model)
    })
    
    # Uji asumsi
    output$assumption_tests <- renderPrint({
      model <- regression_model()
      data <- model_data()
      req(model, data)
      
      cat("=== UJI ASUMSI REGRESI ===\n\n")
      
      # 1. Uji Normalitas (Shapiro-Wilk)
      residuals <- residuals(model)
      if (length(residuals) <= 5000) {
        shapiro_test <- shapiro.test(residuals)
        cat("1. UJI NORMALITAS (Shapiro-Wilk):\n")
        cat("   p-value:", round(shapiro_test$p.value, 6), "\n")
        cat("   Kesimpulan:", ifelse(shapiro_test$p.value > 0.05, 
                                     "Residual berdistribusi normal", 
                                     "Residual TIDAK berdistribusi normal"), "\n\n")
      } else {
        ks_test <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
        cat("1. UJI NORMALITAS (Kolmogorov-Smirnov):\n")
        cat("   p-value:", round(ks_test$p.value, 6), "\n")
        cat("   Kesimpulan:", ifelse(ks_test$p.value > 0.05, 
                                     "Residual berdistribusi normal", 
                                     "Residual TIDAK berdistribusi normal"), "\n\n")
      }
      
      # 2. Uji Homoskedastisitas (Breusch-Pagan)
      if (requireNamespace("lmtest", quietly = TRUE)) {
        bp_test <- lmtest::bptest(model)
        cat("2. UJI HOMOSKEDASTISITAS (Breusch-Pagan):\n")
        cat("   p-value:", round(bp_test$p.value, 6), "\n")
        cat("   Kesimpulan:", ifelse(bp_test$p.value > 0.05, 
                                     "Tidak ada masalah heteroskedastisitas", 
                                     "Terdapat masalah HETEROSKEDASTISITAS"), "\n\n")
      }
      
      # 3. Uji Autokorelasi (Durbin-Watson)
      if (requireNamespace("lmtest", quietly = TRUE)) {
        dw_test <- lmtest::dwtest(model)
        cat("3. UJI AUTOKORELASI (Durbin-Watson):\n")
        cat("   Statistik DW:", round(dw_test$statistic, 4), "\n")
        cat("   p-value:", round(dw_test$p.value, 6), "\n")
        cat("   Kesimpulan:", ifelse(dw_test$p.value > 0.05, 
                                     "Tidak ada autokorelasi", 
                                     "Terdapat masalah AUTOKORELASI"), "\n\n")
      }
      
      # 4. Uji Multikolinearitas (VIF)
      if (length(input$x_vars) > 1 && requireNamespace("car", quietly = TRUE)) {
        vif_values <- car::vif(model)
        cat("4. UJI MULTIKOLINEARITAS (VIF):\n")
        for (i in 1:length(vif_values)) {
          cat("   ", names(vif_values)[i], ":", round(vif_values[i], 3), "\n")
        }
        max_vif <- max(vif_values)
        cat("   Kesimpulan:", ifelse(max_vif < 10, 
                                     "Tidak ada masalah multikolinearitas", 
                                     "Terdapat masalah MULTIKOLINEARITAS"), "\n")
        cat("   (VIF > 10 menunjukkan multikolinearitas)\n\n")
      } else if (length(input$x_vars) == 1) {
        cat("4. UJI MULTIKOLINEARITAS:\n")
        cat("   Tidak applicable (hanya 1 variabel independen)\n\n")
      }
    })
    
    # Interpretasi model
    output$model_interpretation <- renderPrint({
      model <- regression_model()
      req(model)
      
      model_summary <- summary(model)
      
      cat("=== INTERPRETASI MODEL REGRESI ===\n\n")
      
      # R-squared
      cat("1. GOODNESS OF FIT:\n")
      cat("   R-squared:", round(model_summary$r.squared, 4), 
          paste0("(", round(model_summary$r.squared * 100, 2), "% varians dijelaskan)\n"))
      cat("   Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
      
      # F-test
      f_stat <- model_summary$fstatistic
      f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
      cat("   F-statistic:", round(f_stat[1], 4), "dengan p-value:", 
          format(f_p_value, scientific = TRUE, digits = 4), "\n")
      cat("   Model secara keseluruhan:", 
          ifelse(f_p_value < 0.05, "SIGNIFIKAN", "TIDAK SIGNIFIKAN"), "\n\n")
      
      # Koefisien
      cat("2. INTERPRETASI KOEFISIEN:\n")
      coeffs <- model_summary$coefficients
      
      for (i in 1:nrow(coeffs)) {
        var_name <- rownames(coeffs)[i]
        coeff <- coeffs[i, 1]
        p_val <- coeffs[i, 4]
        
        cat("   ", var_name, ":\n")
        cat("     Koefisien:", round(coeff, 4), "\n")
        cat("     p-value:", format(p_val, scientific = TRUE, digits = 4), "\n")
        cat("     Signifikansi:", ifelse(p_val < 0.001, "***", 
                                         ifelse(p_val < 0.01, "**", 
                                                ifelse(p_val < 0.05, "*", 
                                                       ifelse(p_val < 0.1, ".", "")))), "\n")
        
        if (var_name != "(Intercept)") {
          cat("     Interpretasi: Jika", var_name, "naik 1 unit, maka", 
              input$y_var, ifelse(coeff > 0, "naik", "turun"), 
              abs(round(coeff, 4)), "unit\n")
        }
        cat("\n")
      }
      
      cat("Catatan: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1\n")
    })
    
    # Download handler untuk laporan
    output$dl_full_report <- downloadHandler(
      filename = function() {
        paste0("regression_report_", Sys.Date(), ".docx")
      },
      content = function(file) {
        model <- regression_model()
        data <- model_data()
        req(model, data)
        
        # Buat temporary files untuk plots
        temp_diagnostic <- tempfile(fileext = ".png")
        
        # Save diagnostic plots
        png(temp_diagnostic, width = 800, height = 600, res = 100)
        par(mfrow = c(2, 2))
        plot(model)
        dev.off()
        
        # Buat dokumen Word
        doc <- officer::read_docx()
        
        # Judul
        doc <- officer::body_add_par(doc, "LAPORAN ANALISIS REGRESI", 
                                     style = "heading 1")
        doc <- officer::body_add_par(doc, paste("Tanggal:", Sys.Date()))
        doc <- officer::body_add_par(doc, "")
        
        # Model specification
        doc <- officer::body_add_par(doc, "1. SPESIFIKASI MODEL", style = "heading 2")
        formula_text <- paste(input$y_var, "~", paste(input$x_vars, collapse = " + "))
        doc <- officer::body_add_par(doc, paste("Model:", formula_text))
        doc <- officer::body_add_par(doc, paste("Jumlah observasi:", nrow(data)))
        doc <- officer::body_add_par(doc, "")
        
        # Model summary
        doc <- officer::body_add_par(doc, "2. RINGKASAN MODEL", style = "heading 2")
        model_summary <- summary(model)
        
        # R-squared info
        r_sq_text <- paste0("R-squared: ", round(model_summary$r.squared, 4),
                            " (", round(model_summary$r.squared * 100, 2), "% varians dijelaskan)")
        doc <- officer::body_add_par(doc, r_sq_text)
        
        adj_r_sq_text <- paste("Adjusted R-squared:", round(model_summary$adj.r.squared, 4))
        doc <- officer::body_add_par(doc, adj_r_sq_text)
        
        # F-test
        f_stat <- model_summary$fstatistic
        f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
        f_text <- paste0("F-statistic: ", round(f_stat[1], 4), 
                         " (p-value: ", format(f_p_value, scientific = TRUE, digits = 4), ")")
        doc <- officer::body_add_par(doc, f_text)
        doc <- officer::body_add_par(doc, "")
        
        # Coefficients table
        doc <- officer::body_add_par(doc, "3. KOEFISIEN REGRESI", style = "heading 2")
        coeff_df <- as.data.frame(model_summary$coefficients)
        coeff_df$Variable <- rownames(coeff_df)
        coeff_df <- coeff_df[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
        names(coeff_df) <- c("Variabel", "Koefisien", "Std. Error", "t-value", "p-value")
        
        ft <- flextable::flextable(coeff_df)
        ft <- flextable::autofit(ft)
        doc <- flextable::body_add_flextable(doc, ft)
        doc <- officer::body_add_par(doc, "")
        
        # Assumption tests
        doc <- officer::body_add_par(doc, "4. UJI ASUMSI", style = "heading 2")
        
        # Normalitas
        residuals <- residuals(model)
        if (length(residuals) <= 5000) {
          shapiro_test <- shapiro.test(residuals)
          norm_text <- paste0("Uji Normalitas (Shapiro-Wilk): p-value = ", 
                              round(shapiro_test$p.value, 6),
                              ifelse(shapiro_test$p.value > 0.05, 
                                     " (Residual berdistribusi normal)", 
                                     " (Residual TIDAK berdistribusi normal)"))
        } else {
          ks_test <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
          norm_text <- paste0("Uji Normalitas (Kolmogorov-Smirnov): p-value = ", 
                              round(ks_test$p.value, 6),
                              ifelse(ks_test$p.value > 0.05, 
                                     " (Residual berdistribusi normal)", 
                                     " (Residual TIDAK berdistribusi normal)"))
        }
        doc <- officer::body_add_par(doc, norm_text)
        
        # Homoskedastisitas
        if (requireNamespace("lmtest", quietly = TRUE)) {
          bp_test <- lmtest::bptest(model)
          homo_text <- paste0("Uji Homoskedastisitas (Breusch-Pagan): p-value = ", 
                              round(bp_test$p.value, 6),
                              ifelse(bp_test$p.value > 0.05, 
                                     " (Tidak ada masalah heteroskedastisitas)", 
                                     " (Terdapat masalah HETEROSKEDASTISITAS)"))
          doc <- officer::body_add_par(doc, homo_text)
        }
        
        # Autokorelasi
        if (requireNamespace("lmtest", quietly = TRUE)) {
          dw_test <- lmtest::dwtest(model)
          auto_text <- paste0("Uji Autokorelasi (Durbin-Watson): Statistik DW = ", 
                              round(dw_test$statistic, 4), 
                              ", p-value = ", round(dw_test$p.value, 6),
                              ifelse(dw_test$p.value > 0.05, 
                                     " (Tidak ada autokorelasi)", 
                                     " (Terdapat masalah AUTOKORELASI)"))
          doc <- officer::body_add_par(doc, auto_text)
        }
        
        # Multikolinearitas
        if (length(input$x_vars) > 1 && requireNamespace("car", quietly = TRUE)) {
          vif_values <- car::vif(model)
          max_vif <- max(vif_values)
          multi_text <- paste0("Uji Multikolinearitas: VIF tertinggi = ", 
                               round(max_vif, 3),
                               ifelse(max_vif < 10, 
                                      " (Tidak ada masalah multikolinearitas)", 
                                      " (Terdapat masalah MULTIKOLINEARITAS)"))
          doc <- officer::body_add_par(doc, multi_text)
        }
        
        doc <- officer::body_add_par(doc, "")
        
        # Diagnostic plots
        doc <- officer::body_add_par(doc, "5. DIAGNOSTIC PLOTS", style = "heading 2")
        doc <- officer::body_add_img(doc, temp_diagnostic, width = 6, height = 4.5)
        
        # Interpretasi
        doc <- officer::body_add_par(doc, "6. INTERPRETASI", style = "heading 2")
        
        # Interpretasi koefisien
        coeffs <- model_summary$coefficients
        for (i in 1:nrow(coeffs)) {
          var_name <- rownames(coeffs)[i]
          coeff <- coeffs[i, 1]
          p_val <- coeffs[i, 4]
          
          if (var_name != "(Intercept)") {
            interp_text <- paste0("- ", var_name, ": Jika naik 1 unit, maka ", 
                                  input$y_var, ifelse(coeff > 0, " naik ", " turun "), 
                                  abs(round(coeff, 4)), " unit",
                                  ifelse(p_val < 0.05, " (signifikan)", " (tidak signifikan)"))
            doc <- officer::body_add_par(doc, interp_text)
          }
        }
        
        # Kesimpulan
        doc <- officer::body_add_par(doc, "")
        doc <- officer::body_add_par(doc, "7. KESIMPULAN", style = "heading 2")
        
        f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
        conclusion_text <- paste0("Model regresi secara keseluruhan ", 
                                  ifelse(f_p_value < 0.05, "signifikan", "tidak signifikan"),
                                  " dengan R-squared sebesar ", round(model_summary$r.squared, 4),
                                  " yang berarti ", round(model_summary$r.squared * 100, 2),
                                  "% variabilitas dalam ", input$y_var, 
                                  " dapat dijelaskan oleh variabel independen dalam model.")
        
        doc <- officer::body_add_par(doc, conclusion_text)
        
        # Simpan dokumen
        print(doc, target = file)
        
        # Hapus file temporary
        unlink(temp_diagnostic)
      }
    )
  })
}
