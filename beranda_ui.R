beranda_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1(icon("brain"), " SINDRA"),
    p("Dashboard Interaktif untuk Analisis Kerentanan Sosial Indonesia"),
    
    fluidRow(
      box(
        title = tagList(icon("home"), "Selamat Datang di Dashboard SINDRA"),
        width = 12,
        p("SINDRA - Sistem Analisis Daerah Rentan dirancang untuk mengolah, memvisualisasi dan mengeksplorasi data ", strong("social vulnerability"), " dengan lebih mudah. Dengan bantuan SINDRA pengolahan dan analisis sederhana dapat dilakukan dengan lebih mudah bagi user, tanpa memerlukan kemampuan pemrograman khusus.")
      )
    ),
    
    fluidRow(
      column(12,
             h2("Fitur Utama Dashboard"),
             br(),
             box(
               title = tagList(icon("home"), "Beranda & Metadata"),
               width = 4, height = 3,
               br(),
               "Informasi umum mengenai dashboard, metadata, sumber data dan referensi yang digunakan."
             ),
             box(
               title = tagList(icon("database"), "Manajemen Data"),
               width = 4, height = 3,
               br(),
               "Fitur untuk memproses dan mengkategorikan data."
             ),
             box(
               title = tagList(icon("search"), "Eksplorasi Data"),
               width = 4, height = 3,
               br(),
               "Statistik deskriptif, visualisasi grafik dan peta interaktif untuk memahami data."
             ),
             box(
               title = tagList(icon("check-circle"), "Uji Asumsi"),
               width = 4, height = 3,
               br(),
               "Uji normalitas dan homogenitas data."
             ),
             box(
               title = tagList(icon("chart-line"), "Statistik Inferensia"),
               width = 4, height = 3,
               br(),
               "Berbagai metode uji statistik inferensial."
             ),
             box(
               title = tagList(icon("line-chart"), "Regresi Linear"),
               width = 4, height = 3,
               br(),
               "Analisis regresi linear berganda."
             ),
             box(
               title = tagList(icon("lightbulb"), "Insight"),
               width = 4, height = 3,
               br(),
               "Insight lebih mendalam mengenai pulau Sulawesi"
             )
      )
    ),
    
    fluidRow(
      box(
        title = tagList(icon("table"), "Struktur Data yang Digunakan"),
        width = 12,
        h5("Analisis dalam dashboard ini didasarkan pada dua set data utama:"),
        
        h4(icon("database"), " 1. Data Kerentanan Sosial (sovi_data.csv)"),
        p("Dataset ini berisi indikator pembangunan dan bencana dari ", strong("511 kabupaten"),
          " di Indonesia. Data primer dikompilasi dari Survei Sosial Ekonomi Nasional (SUSENAS) tahun 2017 yang dilaksanakan oleh Badan Pusat Statistik"),
        p(strong("DISTRICTCODE:"), " Kode wilayah/distrik (integer)"),
        p(strong("CHILDREN:"), " Persentase populasi di bawah lima tahun"),
        p(strong("FEMALE:"), " Persentase populasi perempuan"),
        p(strong("ELDERLY:"), " Persentase populasi 65 tahun ke atas"),
        p(strong("FHEAD:"), " Persentase rumah tangga dengan kepala rumah tangga perempuan"),
        p(strong("FAMILYSIZE:"), " Rata-rata jumlah anggota rumah tangga"),
        p(strong("NOELECTRIC:"), " Persentase rumah tangga tanpa listrik"),
        p(strong("LOWEDU:"), " Persentase populasi dengan pendidikan rendah"),
        p(strong("GROWTH:"), " Persentase perubahan populasi"),
        p(strong("POVERTY:"), " Persentase penduduk miskin"),
        p(strong("ILLITERATE:"), " Persentase populasi buta huruf"),
        p(strong("NOTRAINING:"), " Persentase tanpa pelatihan bencana"),
        p(strong("DPRONE:"), " Persentase tinggal di daerah rawan bencana"),
        p(strong("RENTED:"), " Persentase rumah tangga menyewa"),
        p(strong("NOSEWER:"), " Persentase tanpa sistem drainase"),
        p(strong("TAPWATER:"), " Persentase menggunakan air ledeng"),
        p(strong("POPULATION:"), " Jumlah populasi (integer)"),
        a(href = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv", "Akses Dataset sovi_data.csv"),
        
        br(), br(),
        h4(icon("map"), " 2. Matriks Jarak Antar-Distrik (distance.csv)"),
        p("Matriks jarak ini memberikan informasi tambahan untuk melakukan Fuzzy Geographically Weighted Clustering (FGWC). Matriks ini dibangun dari peta geografis Indonesia tahun 2013 menggunakan paket R ",
          code("rgeos"), ", ", code("rgdal"), ", dan ", code("sp"), "."),
        p("Terdiri dari ", strong("511 x 511"), " matriks yang merepresentasikan jarak kilometer antara setiap pasangan Kabupaten/Kota"),
        a(href = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv", "Akses Dataset distance.csv")
      )
    ),
    
    fluidRow(
      box(
        title = tagList(icon("download"), "Fitur Unduhan Output"),
        width = 12,
        p("Setiap output yang dihasilkan dashboard ini dilengkapi dengan fitur unduhan yang fleksibel:"),
        p(icon("image"), strong(" Gambar/Grafik:"), " Format JPG untuk presentasi dan laporan visual"),
        p(icon("file-text"), strong(" Laporan Tekstual:"), " Format Word (.docx) untuk dokumentasi")
      )
    ),
    
    fluidRow(
      box(
        title = tagList("Apresiasi dan Referensi"),
        width = 12,
        p("Pembuatan dashboard ini dapat terwujud atas bantuan dan bimbingan dari berbagai pihak:"),
        p(strong("Yuliagnis Transver Wijaya, S.S.T., M.Sc."), " - Dosen pengampu mata kuliah Komputasi Statistik"),
        p(strong("Robert Kurniawan, M.Si"), " - Konseptualisasi, metodologi, menulis-review dan editing pada penelitian yang menjadi refrensi"),
        p(strong("Bahrul Ilmi Nasution"), " - Kurasi data dan visualisasi pada penelitian yang menjadi refrensi"),
        p(strong("Neli Agustina, M.Si."), " - Investigasi dan penyuntingan data pada penelitian yang menjadi refrensi"),
        p(strong("Budi Yuniarto, S.S.T., M.Si"), " - Dukungan kurasi dan visualisasi pada penelitian yang menjadi refrensi"),
        
        h4(icon("book"), " Publikasi Referensi"),
        p(em("Robert Kurniawana, Bahrul Ilmi Nasutionc, Neli Agustina b, Budi Yuniartoa. Revisiting social vulnerability analysis in Indonesia data. ")),
        a(href = "https://doi.org/10.1016/j.dib.2021.107743", "DOI: 10.1016/j.dib.2021.107743"),
        
        br(), br(),
        p(em("Terima kasih kepada Badan Pusat Statistik (BPS) atas penyelenggaraan SUSENAS 2017 yang menjadi sumber data dasar penelitian ini."))
      )
    )
  )
}
