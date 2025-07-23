anova_ui <- function(id){
  titlePanel("ANOVA")
  sidebarLayout(
    sidebarPanel(
      selectInput("dep_var", "Variabel Dependen:", choices = names(data_sovi)[-1]),
      selectInput("group_var", "Variabel Grouping:", choices = names(data_sovi)[-1]),
      numericInput("n_groups", "Jumlah Grup:", value = 3, min = 2, max = 5),
      actionButton("run_anova", "Jalankan ANOVA")
    ),
    mainPanel(
      verbatimTextOutput("anova_result"),
      textOutput("anova_interpretation")
    )
  )
}

server_anova <- function(input, output, session) {
  observeEvent(input$run_anova, {
    output$anova_result <- renderPrint({
      dep <- data_sovi[[input$dep_var]]
      group_data <- data_sovi[[input$group_var]]
      breaks <- quantile(group_data, seq(0, 1, length.out = input$n_groups + 1), na.rm = TRUE)
      groups <- cut(group_data, breaks, include.lowest = TRUE)
      
      model <- aov(dep ~ groups)
      result <- summary(model)
      output$anova_interpretation <- renderText({
        paste("Interpretasi: ", ifelse(result[[1]]$`Pr(>F)`[1] < 0.05, 
                                       "Ada cukup bukti untuk menyatakan bahwa setidaknya satu grup berbeda dari yang lain.", 
                                       "Tidak ada cukup bukti untuk menyatakan bahwa semua grup rata-rata sama."))
      })
      result
    })
  })
}