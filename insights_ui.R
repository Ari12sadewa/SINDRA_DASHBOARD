

library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(plotly)
library(dplyr)
library(RColorBrewer)
library(sf)
source("global.R")

# Insight UI
ins_ui <- function(id){

  titlePanel("Insight")
  tagList(
              fluidRow(
                # Value Boxes
                valueBoxOutput("total_districts"),
                valueBoxOutput("avg_poverty"),
                valueBoxOutput("priority_districts")
              ),
              
              fluidRow(
                column(
                  width = 4,
                  box(
                    title = "Kontrol Visualisasi", 
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 12,
                    selectInput("map_variable", 
                                "Pilih Indikator untuk Peta:",
                                choices = list(
                                  "Kemiskinan (%)" = "POVERTY",
                                  "Tanpa Listrik (%)" = "NOELECTRIC", 
                                  "Buta Huruf (%)" = "ILLITERATE",
                                  "Tanpa Drainase (%)" = "NOSEWER"
                                ),
                                selected = "POVERTY"),
                    br(),
                    downloadButton("download_report", 
                                   "Download Laporan Lengkap",
                                   class = "btn-success",
                                   style = "width: 100%")
                  ),
                  box(
                    title = "Interpretasi Map", 
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 12,
                    textOutput("Interpretasi Map")
                  ),
                ),
                
                # Kolom kanan: peta
                column(
                  width = 8,
                  box(
                    title = textOutput("map_title", inline = TRUE), 
                    status = "primary", 
                    solidHeader = TRUE,
                    width = 12,
                    leafletOutput("choropleth_map_insight", height = 500)
                  )
                )
              )
              ,
              
              fluidRow(
                # Priority Regions Table
                box(
                  title = "Wilayah Prioritas Berdasarkan Multiple Indikator", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  DT::dataTableOutput("priority_table")
                ),
                
                # Top 10 Poorest
                box(
                  title = "10 Wilayah Termiskin", 
                  status = "danger", 
                  solidHeader = TRUE,
                  width = 6,
                  DT::dataTableOutput("poorest_table")
                )
              ),
              
              fluidRow(
                # Main Interpretation
                box(
                  title = "Interpretasi & Rekomendasi Strategis", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  # Temuan Utama
                  div(
                    style = "background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 5px; padding: 15px; margin: 10px 0;",
                    h4("Temuan Utama", style = "color: #0c5460;"),
                    p("Berdasarkan analisis indikator kemiskinan di Pulau Sulawesi, diperoleh beberapa poin penting berikut:", style = "color: #0c5460;"),
                    tags$ul(
                      tags$li("Rata-rata persentase penduduk miskin di Sulawesi mencapai 11,9%, menunjukkan masih tingginya tingkat kerentanan sosial di wilayah ini."),
                      tags$li("Provinsi Sulawesi Utara dan Sulawesi Tengah mendominasi daftar 10 besar kabupaten/kota dengan persentase kemiskinan tertinggi, mengindikasikan konsentrasi kemiskinan berada di wilayah tengah dan utara Sulawesi."),
                      tags$li("Secara nasional, indikator kemiskinan memiliki korelasi cukup kuat (>5%) dengan tiga indikator berikut: persentase rumah tanpa akses listrik (NOELECTRIC), persentase penduduk tidak bisa membaca/menulis (ILLITERATE), dan persentase perumahan tanpa drainase (NOSEWER). Ketiga indikator ini kemudian ditandai sebagai 'indikator kerentanan kemiskinan'"),
                      tags$li("Terdapat lima kabupaten/kota yang konsisten masuk dalam daftar 10 kabupaten dengan persentase terbesar pada lebih dari satu indikator kerentanan kemiskinan yaitu: Boalemo, Donggala, Konawe Kepulauan, Parigi Moutong, dan Tojo Una-una (terlampir dalam 'Wilayah Prioritas Berdasarkan Multiple Indikator'). Kelima wilayah ini dapat dikategorikan sebagai 'daerah prioritas' untuk program pengurangan kerentanan kemiskinan"),
                      tags$li("Meskipun tidak menonjol pada variabel kemiskinan, Provinsi Sulawesi Selatan memiliki banyak kabupaten dengan persentase penduduk tidak bisa membaca/menulis(ILLITERATE) tertinggi di Sulawesi. Fenomena ini dapat dikaitkan dengan karakteristik budaya lokal, seperti dominasi ekonomi informal dan pola pikir masyarakat yang lebih mengutamakan aktivitas perdagangan dibanding pendidikan formal."),
                      style = "color: #0c5460;"
                    )
                  ),
                  
                  # Kesimpulan
                  div(
                    style = "background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 5px; padding: 15px; margin: 10px 0;",
                    h4("Kesimpulan", style = "color: #155724;"),
                    p("Analisis ini menunjukkan bahwa kemiskinan di Sulawesi bukanlah fenomena tunggal, melainkan hasil interaksi antara beberapa faktor struktural seperti akses terhadap layanan dasar dan tingkat literasi. Penanganan yang efektif harus mempertimbangkan pendekatan multi-dimensional, bukan hanya aspek ekonomi, tetapi juga infrastruktur dasar dan pendidikan.", style = "color: #155724;"),
                    p("Pendekatan spasial melalui pemetaan kabupaten/kota juga membantu mengidentifikasi wilayah yang memerlukan penanganan prioritas. Dengan mengintegrasikan beberapa indikator kerentanan, pemangku kepentingan dapat menyusun strategi pembangunan yang lebih terarah dan berlandaskan data.", style = "color: #155724;")
                  ),
                  
                  # Catatan Metodologi
                  div(
                    style = "background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; padding: 15px; margin: 10px 0;",
                    h4("Catatan Metodologi", style = "color: #721c24;"),
                    p("Sebagai peneliti, penting untuk mempertimbangkan beberapa keterbatasan dalam interpretasi hasil:", style = "color: #721c24;"),
                    tags$ul(
                      tags$li("Data yang digunakan berasal dari SUSENAS tahun 2017, sehingga belum tentu mencerminkan kondisi sosial-ekonomi terkini."),
                      tags$li("Analisis ini bersifat kuantitatif dan berbasis indikator sekunder, tanpa konfirmasi lapangan atau konteks kualitatif lokal."),
                      style = "color: #721c24;"
                    ),
                    p(strong("Rekomendasi: "), "Analisis ini sebaiknya dikombinasikan dengan studi kualitatif seperti wawancara dengan pemangku kepentingan lokal. Hal ini dapat memperkuat konteks sosial dan budaya yang tidak tertangkap oleh angka statistik.", style = "color: #721c24;")
                  )
                )
                
              )
  )
}

# Define Server
ins_server <- function(input, output, session) {
  
  output$map_title <- renderText({
    var_titles <- c(
      "POVERTY" = "Peta Sebaran Indikator Kemiskinan",
      "NOELECTRIC" = "Peta Sebaran Indikator Tanpa Listrik", 
      "ILLITERATE" = "Peta Sebaran Indikator Buta Huruf",
      "NOSEWER" = "Peta Sebaran Indikator Tanpa Drainase"
    )
    var_titles[input$map_variable]
  })
  
  
  # Tambahkan di bagian server function, ganti bagian output interpretasi map yang ada
  
  # Interpretasi Map Output
  output$`Interpretasi Map` <- renderText({
    df <- data_sul
    selected_var <- input$map_variable
    
    # Temukan kota dengan nilai tertinggi untuk variabel yang dipilih
    max_row <- df %>% 
      filter(!!sym(selected_var) == max(!!sym(selected_var), na.rm = TRUE)) %>% 
      slice_head(n = 1)
    
    kota_tertinggi <- max_row$nmkab
    provinsi <- max_row$nmprov
    nilai_tertinggi <- round(max_row[[selected_var]], 1)
    
    # Definisi kategori berdasarkan variabel yang dipilih
    kategori <- switch(selected_var,
                       "POVERTY" = "penduduk miskin",
                       "ILLITERATE" = "penduduk buta huruf", 
                       "NOELECTRIC" = "perumahan tanpa akses listrik",
                       "NOSEWER" = "perumahan tanpa sistem drainase"
    )
    
    # Generate interpretasi text
    interpretasi_text <- paste0(
      "Kota ", kota_tertinggi, 
      " di provinsi ", provinsi, 
      " memiliki persentase ", kategori, 
      " sebesar ", nilai_tertinggi, "%"
    )
    
    return(interpretasi_text)
  })
  
  # Load sample data
  data_sovi$DISTRICTCODE <- as.character(data_sovi$DISTRICTCODE)
  data_sul <- data_sovi %>%filter(substr(DISTRICTCODE,1,2)%in% as.character(71:76)) %>% left_join(data_area, by = c("DISTRICTCODE" = "kodeprkab"))
  # Value Boxes
  output$total_districts <- renderValueBox({
    valueBox(
      value = nrow(data_sul),
      subtitle = "Total Kabupaten/Kota",
      icon = icon("map-marker-alt"),
      color = "blue"
    )
  })
  
  output$avg_poverty <- renderValueBox({
    valueBox(
      value = paste0(round(mean(data_sul$POVERTY), 1), "%"),
      subtitle = "Rata-rata Kemiskinan",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  output$priority_districts <- renderValueBox({
    # Count districts with high poverty (>15%)
    high_poverty <- sum(data_sul$POVERTY > 15)
    valueBox(
      value = high_poverty,
      subtitle = "Wilayah Prioritas (>15% kemiskinan)",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  # Priority regions analysis
  priority_data <- reactive({
    df <- data_sul
    # Empat indikator yang berkorelasi erat dengan kemiskinan
    indikator_list <- c("NOELECTRIC", "POVERTY", "ILLITERATE", "NOSEWER")
    
    # Create rankings for each indicator
    rankings <- data.frame()
    for(indicator in indikator_list) {
      top_districts <- df %>% 
        arrange(desc(!!sym(indicator))) %>% 
        slice_head(n = 10) %>% 
        select(nmkab, nmprov) %>% 
        mutate(Indikator = indicator)
      rankings <- rbind(rankings, top_districts)
    }
    
    # Hitung Frekuensi Kabupaten terseleksi
    priority_summary <- rankings %>% 
      group_by(nmkab,nmprov) %>% 
      summarise(
        Frekuensi = n(),
        Indikator_Dominan = paste(sort(unique(Indikator)), collapse = ", "),
        .groups = "drop"
      ) %>% 
      arrange(desc(Frekuensi)) %>% 
      slice_head(n = 15)
    
    return(priority_summary)
  })

  # Tabel
  output$priority_table <- DT::renderDataTable({
    DT::datatable(
      priority_data(),
      options = list(pageLength = 5, dom = 't'),
      rownames = FALSE
    ) %>% 
      DT::formatStyle('Frekuensi',
                      backgroundColor = DT::styleInterval(c(2, 3), c('#fff3e0', '#ffe0b2', '#ffcc02')))
  })
  
  output$poorest_table <- DT::renderDataTable({
    poorest <- data_sul %>% 
      arrange(desc(POVERTY)) %>% 
      slice_head(n = 10) %>% 
      select(nmkab, nmprov, POVERTY) %>% 
      mutate(POVERTY = round(POVERTY, 1))
    
    DT::datatable(
      poorest,
      options = list(pageLength = 10, dom = 't'),
      rownames = FALSE,
      colnames = c("Kabupaten/Kota", "Provinsi", "Kemiskinan (%)")
    ) %>% 
      DT::formatStyle('POVERTY', backgroundColor = '#ffebee')
  })
  
  # Map Choropleth
  output$choropleth_map_insight <- renderLeaflet({
    df <- data_sul %>% st_as_sf()

    selected_var <- input$map_variable
    print(selected_var)
    
    # top 10 kab/kota dengan variabel yang dipilih
    focusArea <- df  %>%  arrange(desc(!!sym(selected_var))) %>% head(10)

    # Palet warna
    pal <- colorNumeric(
      palette = "Reds",
      domain = focusArea[[selected_var]]
    )
    
    # Label variabel
    var_labels <- c(
      "POVERTY" = "Tingkat Kemiskinan (%)",
      "NOELECTRIC" = "Tanpa Listrik (%)", 
      "ILLITERATE" = "Buta Huruf (%)",
      "NOSEWER" = "Tanpa Drainase (%)"
    )

    
    # Leaflet Map
    leaflet(focusArea) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = pal(focusArea[[selected_var]]),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        label = ~paste0(nmkab, ", ", nmprov, ": ", round(get(selected_var), 2), "%"),
        highlight = highlightOptions(weight = 2, color = "blue", bringToFront = TRUE),
        popup = ~paste0(
          "<strong>", nmkab, "</strong><br/>",
          "Provinsi: ", nmprov, "<br/>",
          "Kemiskinan: ", round(POVERTY, 1), "%<br/>",
          "Tanpa Listrik: ", round(NOELECTRIC, 1), "%<br/>",
          "Buta Huruf: ", round(ILLITERATE, 1), "%<br/>",
          "Tanpa Drainase: ", round(NOSEWER, 1), "%"
        )
      ) %>%
      addLegend(
        "bottomright", 
        pal = pal, 
        values = focusArea[[selected_var]],
        title = var_labels[selected_var],
        opacity = 0.7
      )
  })
}
