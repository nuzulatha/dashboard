# Memuat semua pustaka yang dibutuhkan
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(car)           # Untuk Levene's Test
library(EnvStats)      # Untuk varTest
library(lmtest)        # Untuk uji asumsi regresi
library(htmlwidgets)   # Opsional, jika ingin unduh widget
library(sf)       # Untuk membaca dan memanipulasi data geojson
library(leaflet)  # Untuk membuat peta interaktif
library(shinyWidgets)
library(writexl)
library(gridExtra)
library(zip)
library(rvest)
library(rmarkdown)
library(terra)
library(mapview)
library(webshot2)
library(broom)
library(spdep)
library(sp)
library(rsconnect)

#==============================================================================
#                             USER INTERFACE (UI)
#==============================================================================
# 1. Muat data indikator SOVI
sovi_data <- read.csv("sovi_data.csv", stringsAsFactors = FALSE)

# 2. Muat data jarak
distance_data <- read.csv("distance_clean.csv", stringsAsFactors = FALSE)

# 3. Muat data geospasial (peta) menggunakan library 'sf'
map_data <- sf::st_read("indonesia511.geojson")

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "SOCIAnalyze"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-simple")),
      menuItem("Uji Asumsi", tabName = "asumsi", icon = icon("check-double")),
      menuItem("Statistik Inferensia", tabName = "inferensia", icon = icon("calculator"),
               menuSubItem("Uji Rata-Rata", tabName = "uji_rata"),
               menuSubItem("Uji Proporsi & Ragam", tabName = "uji_prop_ragam"),
               menuSubItem("ANOVA", tabName = "anova")
      ),
      menuItem("Regresi Linear", tabName = "regresi", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tags$head(tags$style(HTML(".content-wrapper { overflow-y: auto; };
                              .welcome-card {
                                background-color: #f8f9fa; /* Warna background abu-abu muda */
                                  border: 1px solid #dee2e6; /* Border tipis */
                                border-radius: 8px; /* Sudut sedikit melengkung */
                                  padding: 25px;
                                text-align: center;
                                box-shadow: 0 4px 8px rgba(0,0,0,0.05); /* Bayangan halus */
                              }
                              .welcome-card h2 {
                                color: #004a99; /* Warna biru yang lebih gelap */
                                  font-weight: 600;
                              }
                              .welcome-card .project-info {
                                margin-top: 20px;
                                font-style: italic;
                                color: #6c757d; /* Warna abu-abu untuk info proyek */
                              }
                              "))
              ),
    tabItems(
      # Halaman Beranda
      tabItem(tabName = "beranda",
              fluidRow(
                # Box Selamat Datang (Tidak berubah)
                box(
                  width = 12,
                  title = tagList(shiny::icon("bullhorn"), "Selamat Datang"),
                  status = "primary",
                  solidHeader = TRUE,
                  div(class = "welcome-card",
                      h2("SOCIAnalyze: Analyzing Social Vulnerability in Indonesia"),
                      h3("Sebuah dashboard interaktif dan analitis untuk mengeksplorasi dan memahami Indeks Kerentanan Sosial (SOVI) di Indonesia."),
                      p(style = "font-size: 16px; text-align: justify; margin-top: 15px;", 
                        "SOCIAnalyze dirancang sebagai alat bantu visualisasi dan eksplorasi data yang memungkinkan pengguna untuk menggali lebih dalam mengenai tingkat kerentanan sosial di berbagai wilayah. Dengan pendekatan berbasis statistik dan multidimensi, dashboard ini menyajikan analisis komprehensif terhadap faktor-faktor seperti demografi, ekonomi, pendidikan, dan infrastruktur, yang memengaruhi tingkat kerentanan suatu komunitas terhadap risiko sosial dan bencana."), 
                      
                      hr(),
                      div(class = "project-info",
                          p(strong("Proyek Ujian Akhir Semester Komputasi Statistik")),
                          p(strong("Dosen Pengampu:"), "Yuliagnis Transver Wijaya, S.S.T., M.Sc.")
                      )
                  )
                )
              ),
              
              # Sorotan Utama Data (Tidak berubah)
              h3("Sorotan Utama Data"),
              fluidRow(
                valueBoxOutput("total_distrik"),
                valueBoxOutput("rata_kemiskinan"),
                valueBoxOutput("rata_tanpa_listrik")
              ),
              
              # --- BOX METADATA & DOWNLOAD (VERSI LENGKAP) ---
              fluidRow(
                shinydashboardPlus::box(
                  title = tagList(shiny::icon("database"), "Metadata dan Sumber Data"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  
                  p("Dasbor ini mengintegrasikan tiga sumber data utama untuk memberikan analisis kerentanan sosial yang komprehensif di seluruh Indonesia."),
                  hr(),
                  
                  # Detail per file data
                  fluidRow(
                    # File 1: Data Indikator SOVI
                    column(width = 4,
                           h4(shiny::icon("table"), " Data Indikator SOVI"),
                           tags$ul(
                             tags$li(strong("Nama File:"), " sovi_data.csv"),
                             tags$li(strong("Format:"), " CSV (Comma-Separated Values)"),
                             tags$li(strong("Sumber:"), " Potensi Desa (Podes) 2018 - BPS"),
                             tags$li(strong("Deskripsi:"), " Berisi 100+ indikator demografi, sosial, dan ekonomi untuk 511 distrik/kota di Indonesia.")
                           ),
                           downloadButton("downloadSOVI", "Unduh Data SOVI (.csv)", class = "btn-block")
                    ),
                    # File 2: Data Geospasial
                    column(width = 4,
                           h4(shiny::icon("map-marked-alt"), " Data Geospasial"),
                           tags$ul(
                             tags$li(strong("Nama File:"), " indonesia511.geojson"),
                             tags$li(strong("Format:"), " GeoJSON"),
                             tags$li(strong("Sumber:"), " Data batas wilayah administratif"),
                             tags$li(strong("Deskripsi:"), " Berisi data poligon (batas wilayah) untuk setiap distrik/kota di Indonesia.")
                           ),
                           downloadButton("downloadGeoJSON", "Unduh Peta (.geojson)", class = "btn-block")
                    ),
                    # File 3: Data Jarak
                    column(width = 4,
                           h4(shiny::icon("road"), " Data Jarak"),
                           tags$ul(
                             tags$li(strong("Nama File:"), " distance_clean.csv"),
                             tags$li(strong("Format:"), " CSV (Comma-Separated Values)"),
                             tags$li(strong("Sumber:"), " Perhitungan mandiri"),
                             tags$li(strong("Deskripsi:"), " Berisi data jarak dari pusat setiap distrik ke garis pantai atau kota besar terdekat.")
                           ),
                           downloadButton("downloadDistance", "Unduh Data Jarak (.csv)", class = "btn-block")
                    )
                  )
                )
              ),
              
              # Panduan Penggunaan Dashboard
              fluidRow(
                box(
                  title = tagList(shiny::icon("map-signs"), "Panduan Penggunaan Dashboard"), 
                  status = "primary", 
                  width = 12,
                  collapsible = TRUE, # Dibuat collapsible juga agar rapi
                  collapsed = TRUE,   # Defaultnya tersembunyi
                  tags$ul(
                    tags$li(strong("Beranda:"), " Anda di sini! Gambaran umum dan highlight data."),
                    tags$li(strong("Manajemen Data:"), " Ubah tipe variabel kontinu menjadi kategori."),
                    tags$li(strong("Eksplorasi Data:"), " Selami data lebih dalam melalui statistik deskriptif dan grafik."),
                    tags$li(strong("Uji Asumsi:"), " Lakukan uji prasyarat statistik seperti normalitas dan homogenitas."),
                    tags$li(strong("Statistik Inferensia:"), " Lakukan berbagai uji hipotesis (uji beda rata-rata, proporsi, ANOVA)."),
                    tags$li(strong("Regresi Linear:"), " Bangun model regresi linear untuk melihat hubungan antar variabel.")
                  )
                )
              )
      ),
      
      # Halaman Manajemen Data
      tabItem(tabName = "manajemen",
              h2("Manajemen Data"),
              tabBox(
                id = "tabset_manajemen", width = 12,
                tabPanel("Kategorisasi Variabel", icon = icon("object-group"),
                         fluidRow(
                           column(width = 4,
                                  # Panel Kontrol (Tidak ada perubahan di sini)
                                  box(title = "Panel Kontrol", status = "warning", solidHeader = TRUE, width = 12,
                                      selectInput("var_to_transform", "1. Pilih Variabel untuk Diubah:", choices = ""),
                                      radioButtons("transform_method", "2. Pilih Metode Kategorisasi:", choices = c("Kuantil (Sama Frekuensi)" = "quantile", "Interval Sama (Sama Lebar)" = "equal_width")),
                                      sliderInput("num_categories", "3. Pilih Jumlah Kategori:", min = 2, max = 10, value = 4, step = 1),
                                      br(),
                                      actionButton("apply_transform", "Terapkan Kategorisasi", icon = icon("play"), width = "100%", class = "btn-success"),
                                  )
                           ),
                           column(width = 8,
                                  fluidRow(
                                    # --- BOX SEBELUM TRANSFORMASI (DENGAN TOMBOL UNDUH) ---
                                    box(title = "Sebelum Transformasi", status = "info", solidHeader = TRUE, width = 6,
                                        plotOutput("before_plot", height = "250px"),
                                        verbatimTextOutput("before_summary"),
                                        downloadButton("downloadBeforePlot", "Unduh Grafik (.png)", class =  " btn-sm btn-block")
                                    ),
                                    
                                    # --- BOX SESUDAH TRANSFORMASI (DENGAN TOMBOL UNDUH) ---
                                    box(title = "Sesudah Transformasi", status = "info", solidHeader = TRUE, width = 6,
                                        plotOutput("after_plot", height = "250px"),
                                        verbatimTextOutput("after_summary"),
                                        downloadButton("downloadAfterPlot", "Unduh Grafik (.png)", class =  " btn-sm btn-block"),
                                    )
                                  )
                           ),
                           # Tabset Hasil (Tidak ada perubahan di sini)
                           tabBox(title = "Interpretasi & Data Hasil", id = "tabset_manajemen_hasil", width = 12,
                                  tabPanel("Interpretasi", icon = icon("comment-dots"), uiOutput("interpretation_text")),
                                  tabPanel("Lihat Data", icon = icon("table"),
                                           # Letakkan tombol dropdown di sini, SEBELUM tabel data
                                           shinyWidgets::dropdownButton(
                                             label = "Unduh Data Tabel",  # <-- LABEL DIUBAH
                                             icon = icon("download"),
                                             status = "primary",
                                             circle = FALSE,
                                             
                                             # ID tombol juga diubah agar sesuai dengan logika server yang baru
                                             downloadButton("downloadMainTableCSV", "Unduh sebagai CSV", icon = icon("file-csv"), class="btn-default btn-sm btn-block"),
                                             downloadButton("downloadMainTableXLSX", "Unduh sebagai Excel", icon = icon("file-excel"), class="btn-default btn-sm btn-block")
                                           ),
                                           
                                           br(), # Memberi sedikit jarak
                                           
                                           # Baru tampilkan tabel datanya di bawah tombol
                                           DTOutput("transformed_data_table")
                                  )
                           )
                         )
                ),
                tabPanel("Transformasi Variabel", icon = icon("wave-square"),
                         fluidRow(
                           column(width = 4,
                                  box(title = "Panel Kontrol", status = "warning", solidHeader = TRUE, width = 12,
                                      selectInput("var_to_math_transform", "1. Pilih Variabel untuk Ditransformasi:", choices = ""),
                                      radioButtons("math_transform_method", "2. Pilih Metode Transformasi:", choices = c("Logaritma (log(x+1))" = "log", "Akar Kuadrat (sqrt)" = "sqrt", "Standardisasi (Z-score)" = "scale")),
                                      br(),
                                      actionButton("apply_math_transform", "Terapkan Transformasi", icon = icon("play"), width = "100%", class = "btn-success")
                                  )
                           ),
                           column(width = 8,
                                  fluidRow(
                                    box(title = "Sebelum Transformasi", status = "info", solidHeader = TRUE, width = 6, 
                                        plotOutput("before_math_plot", height = "300px"),
                                        verbatimTextOutput("before_math_summary"),
                                        downloadButton("downloadBeforeMathPlot", "Unduh Grafik (.png)", class =  " btn-sm btn-block")
                                    ),
                                    box(title = "Sesudah Transformasi", status = "info", solidHeader = TRUE, width = 6, 
                                        plotOutput("after_math_plot", height = "300px"),
                                        verbatimTextOutput("after_math_summary"),
                                        downloadButton("downloadAfterMathPlot", "Unduh Grafik (.png)", class =  " btn-sm btn-block")
                                    )
                                  ),
                                  fluidRow(box(title = "Interpretasi", status = "success", solidHeader = TRUE, width = 12, uiOutput("math_interpretation_text")))
                           )
                         )
                )
              ),
              fluidRow(
                column(width = 12,
                       # --- PERUBAHAN DI SINI ---
                       # Ganti juga untuk tab ini dengan satu actionButton
                       div(style = "display: flex; justify-content: flex-end;",
                           actionButton("show_download_modal", "Unduh Halaman Ini (ZIP)", icon = icon("download"), class = "btn-primary")
                       )
                )
              )
      ),
      
      # Halaman Eksplorasi Data 
      tabItem(tabName = "eksplorasi",
              h2("Eksplorasi Data dan Statistik"),
              tabBox(
                id = "tabset_eksplorasi",
                width = 12,
                tabPanel("Statistik Deskriptif", icon = icon("table-list"),
                         fluidRow(
                           column(width = 4,
                                  box(title = "Pilih Variabel", status = "warning", solidHeader = TRUE, width = 12,
                                      selectizeInput("desc_vars", "Pilih satu atau lebih variabel numerik:", choices = NULL, multiple = TRUE)
                                  )
                           ),
                           column(width = 8,
                                  box(title = "Ringkasan Statistik", status = "primary", solidHeader = TRUE, width = 12,
                                      verbatimTextOutput("desc_summary_output"),
                                      downloadButton("downloadSummary", "Unduh Ringkasan (.docx)", class = "btn-default btn-sm btn-block")
                                  ),
                                  box(title = "Interpretasi", status = "success", solidHeader = TRUE, width = 12,
                                      uiOutput("desc_interpretation")
                                  )
                           )
                         )
                ),
                tabPanel("Visualisasi Grafik", icon = icon("chart-area"),
                         fluidRow(
                           column(width = 4,
                                  box(title = "Pengaturan Grafik", status = "warning", solidHeader = TRUE, width = 12,
                                      radioButtons("plot_type", "Pilih Jenis Grafik:",
                                                   choices = c("Histogram", "Box Plot", "Scatter Plot"), selected = "Histogram"),
                                      hr(),
                                      uiOutput("plot_ui")
                                  )
                           ),
                           column(width = 8,
                                  box(title = "Hasil Grafik", status = "primary", solidHeader = TRUE, width = 12,
                                      plotOutput("main_plot", height = "400px"),
                                      downloadButton("downloadPlot_eksplorasi", "Unduh Grafik (.png)", class =  " btn-sm btn-block")
                                  ),
                                  box(title = "Interpretasi", status = "success", solidHeader = TRUE, width = 12,
                                      uiOutput("plot_interpretation")
                                  )
                           )
                         )
                ),
                tabPanel("Peta Interaktif", icon = icon("map-marked-alt"),
                         fluidRow(
                           column(width = 4,
                                  box(title = "Pengaturan Peta", status = "warning", solidHeader = TRUE, width = 12,
                                      # Dropdown untuk memilih variabel yang akan ditampilkan di peta
                                      selectInput("map_var", 
                                                  label = "Pilih Variabel untuk Ditampilkan:",
                                                  choices = NULL) # Pilihan akan diisi dari server
                                  )
                           ),
                           column(width = 8,
                                  box(title = "Peta Sebaran Data", status = "primary", solidHeader = TRUE, width = 12,
                                      # Output untuk peta leaflet
                                      leafletOutput("interactive_map", height = "400px"),
                                      downloadButton("downloadMap", "Unduh Peta (.png)", class = "btn-default btn-sm btn-block")
                                  ),
                                  box(title = "Analisis dan Interpretasi Peta", status = "success", solidHeader = TRUE, width = 12,
                                      # Kita gunakan uiOutput agar bisa diisi dengan teks berformat dari server
                                      uiOutput("map_interpretation")
                                  )
                           )
                         )
                )
              ),
              fluidRow(
                column(width = 12,
                       # --- PERUBAHAN DI SINI ---
                       # Ganti juga untuk tab ini dengan satu actionButton
                       div(style = "display: flex; justify-content: flex-end;",
                           actionButton("tampil_modal_unduh", "Unduh Halaman Ini (ZIP)", icon = icon("download"), class = "btn-primary")
                       )
                )
              )
      ),
      
      # Halaman Uji Asumsi
      tabItem(tabName = "asumsi",
              h2("Uji Asumsi Prasyarat"),
              tabBox(
                id = "tabset_asumsi", width = 12,
                tabPanel("Uji Normalitas", icon = icon("signal"),
                         fluidRow(
                           column(width = 4,
                                  box(
                                    title = "Pengaturan Uji Normalitas", status = "warning", solidHeader = TRUE, width = 12,
                                    selectInput("norm_var", "1. Pilih Variabel Numerik:", choices = NULL),
                                    radioButtons("norm_test_method", "2. Pilih Metode Uji:", 
                                                 choices = c("Shapiro-Wilk" = "sw", "Kolmogorov-Smirnov" = "ks")),
                                    radioButtons("norm_alpha", "3. Pilih Tingkat Signifikansi (α):", 
                                                 choices = c(0.01, 0.05, 0.10), selected = 0.05, inline = TRUE),
                                    br(),
                                    actionButton("run_norm_test", "Jalankan Uji Normalitas", icon = icon("play"), width = "100%", class = "btn-success")
                                  )
                           ),
                           column(width = 8,
                                  box(
                                    title = "Hasil Uji Normalitas", status = "primary", solidHeader = TRUE, width = 12,
                                    fluidRow(
                                      column(width = 6, 
                                             h4("Visualisasi (Q-Q Plot)"),
                                             plotOutput("norm_plot", height = "300px"),
                                             downloadButton("download_norm_plot", "Unduh Grafik (.png)", class =  " btn-sm btn-block")
                                      ),
                                      column(width = 6, 
                                             h4("Output Statistik"),
                                             verbatimTextOutput("norm_test_output")
                                      )
                                    )
                                  ),
                                  box(
                                    title = "Interpretasi Hasil", status = "success", solidHeader = TRUE, width = 12,
                                    uiOutput("norm_interpretation")
                                  )
                           )
                         )
                ),
                tabPanel("Uji Homogenitas Varians", icon = icon("layer-group"),
                         fluidRow(
                           column(width = 4,
                                  box(
                                    title = "Pengaturan Uji Homogenitas", status = "warning", solidHeader = TRUE, width = 12,
                                    selectInput("homog_var_num", "1. Pilih Variabel Dependen (Numerik):", choices = NULL),
                                    selectInput("homog_var_cat", "2. Pilih Variabel Grup (Kategorik):", choices = NULL),
                                    radioButtons("homog_test_method", "3. Pilih Metode Uji:", 
                                                 choices = c("Levene's Test" = "levene", "Bartlett's Test" = "bartlett")),
                                    radioButtons("homog_alpha", "4. Pilih Tingkat Signifikansi (α):", 
                                                 choices = c(0.01, 0.05, 0.10), selected = 0.05, inline = TRUE),
                                    br(),
                                    actionButton("run_homog_test", "Jalankan Uji Homogenitas", icon = icon("play"), width = "100%", class = "btn-success")
                                  )
                           ),
                           column(width = 8,
                                  box(
                                    title = "Hasil Uji Homogenitas", status = "primary", solidHeader = TRUE, width = 12,
                                    fluidRow(
                                      column(width = 6, 
                                             h4("Visualisasi (Box Plot)"),
                                             plotOutput("homog_plot", height = "300px"),
                                             downloadButton("download_homog_plot", "Unduh Grafik (.png)", class =  " btn-sm btn-block")
                                      ),
                                      column(width = 6, 
                                             h4("Output Statistik"),
                                             verbatimTextOutput("homog_test_output")
                                      )
                                    )
                                  ),
                                  box(
                                    title = "Interpretasi Hasil", status = "success", solidHeader = TRUE, width = 12,
                                    uiOutput("homog_interpretation")
                                  )
                           )
                         )
                ),
                tabPanel("Uji Autokorelasi Spasial", icon = icon("map-signs"),
                         fluidRow(
                           column(width = 4,
                                  box(
                                    title = "Pengaturan Uji Moran's I", status = "warning", solidHeader = TRUE, width = 12,
                                    selectInput("moran_var", "1. Pilih Variabel untuk Diuji:", choices = NULL),
                                    selectInput("weights_method", "2. Pilih Metode Pembobot Spasial:",
                                                choices = c("Persinggungan (Queen)" = "queen",
                                                            "Jarak Inversi (Inverse Distance)" = "inv_dist")),
                                    radioButtons("moran_alpha", "3. Pilih Tingkat Signifikansi (α):",
                                                 choices = c(0.01, 0.05, 0.10), selected = 0.05, inline = TRUE),
                                    br(),
                                    actionButton("run_moran_test", "Jalankan Uji Moran's I", icon = icon("play"), width = "100%", class = "btn-success")
                                  )
                           ),
                           column(width = 8,
                                  box(
                                    title = "Hasil Uji Moran's I", status = "primary", solidHeader = TRUE, width = 12,
                                    fluidRow(
                                      column(width = 7,
                                             h4("Visualisasi (Moran Scatter Plot)"),
                                             plotOutput("moran_plot_output", height = "300px"),
                                             downloadButton("download_moran_plot", "Unduh Grafik (.png)", class =  " btn-sm btn-block")
                                      ),
                                      column(width = 5,
                                             h4("Output Statistik"),
                                             verbatimTextOutput("moran_test_output")
                                      )
                                    )
                                  ),
                                  box(
                                    title = "Interpretasi Hasil", status = "success", solidHeader = TRUE, width = 12,
                                    uiOutput("moran_interpretation")
                                  )
                           )
                         )
                )
              ),
              fluidRow(
                column(width = 12, align = "right",
                       # Tombol untuk memicu unduhan halaman
                       actionButton("download_asumsi_page", "Unduh Halaman Ini (ZIP)", 
                                    icon = icon("download"), class = "btn-primary")
                )
              )
      ), 
      
      # Halaman Uji Rata-Rata
      tabItem(tabName = "uji_rata",
              h2("Uji Hipotesis Rata-Rata"),
              tabBox(
                id = "tabset_uji_rata", width = 12,
                tabPanel("Uji t Satu Sampel", icon = icon("user"),
                         fluidRow(
                           column(width = 4,
                                  box(title = "Pengaturan Uji", status = "warning", solidHeader = TRUE, width = 12,
                                      selectInput("t_test_one_var", "1. Pilih Variabel (Numerik):", choices = NULL),
                                      numericInput("t_test_one_mu", "2. Nilai Hipotesis (μ₀):", value = 0),
                                      radioButtons("t_test_one_alternative", "3. Hipotesis Alternatif (H₁):",
                                                   choices = c("Rata-rata ≠ μ₀" = "two.sided", 
                                                               "Rata-rata > μ₀" = "greater", 
                                                               "Rata-rata < μ₀" = "less")),
                                      radioButtons("t_test_one_alpha", "4. Tingkat Signifikansi (α):",
                                                   choices = c(0.01, 0.05, 0.10), selected = 0.05, inline = TRUE),
                                      br(),
                                      actionButton("run_t_test_one", "Jalankan Uji", icon = icon("play"), width = "100%", class = "btn-success")
                                  )
                           ),
                           column(width = 8,
                                  box(title = "Hasil Uji Statistik", status = "primary", solidHeader = TRUE, width = 12,
                                      verbatimTextOutput("t_test_one_output"),
                                      downloadButton("download_summary_one", "Unduh Hasil Statistik (.docx)", class="btn-sm", style="margin-top: 10px;")
                                  ),
                                  box(title = "Interpretasi Hasil", status = "success", solidHeader = TRUE, width = 12,
                                      uiOutput("t_test_one_interpretation")
                                  )
                           )
                         )
                ),
                tabPanel("Uji t Dua Sampel", icon = icon("users"),
                         fluidRow(
                           column(width = 4,
                                  box(title = "Pengaturan Uji", status = "warning", solidHeader = TRUE, width = 12,
                                      selectInput("t_test_two_num_var", "1. Pilih Variabel Dependen (Numerik):", choices = NULL),
                                      selectInput("t_test_two_cat_var", "2. Pilih Variabel Grup (2 Kategori):", choices = NULL),
                                      radioButtons("t_test_two_alternative", "3. Hipotesis Alternatif (H₁):",
                                                   choices = c("Rata-rata Grup 1 ≠ Grup 2" = "two.sided", 
                                                               "Rata-rata Grup 1 > Grup 2" = "greater", 
                                                               "Rata-rata Grup 1 < Grup 2" = "less")),
                                      radioButtons("t_test_two_alpha", "4. Tingkat Signifikansi (α):",
                                                   choices = c(0.01, 0.05, 0.10), selected = 0.05, inline = TRUE),
                                      br(),
                                      actionButton("run_t_test_two", "Jalankan Uji", icon = icon("play"), width = "100%", class = "btn-success")
                                  )
                           ),
                           column(width = 8,
                                  box(title = "Hasil Uji Statistik", status = "primary", solidHeader = TRUE, width = 12,
                                      verbatimTextOutput("t_test_two_output"),
                                      downloadButton("download_summary_two", "Unduh Hasil Statistik (.docx)", class="btn-sm", style="margin-top: 10px;")
                                  ),
                                  box(title = "Interpretasi Hasil", status = "success", solidHeader = TRUE, width = 12,
                                      uiOutput("t_test_two_interpretation")
                                  )
                           )
                         )
                )
              ),
              fluidRow(
                column(width = 12, align = "right",
                       # Tombol untuk memicu unduhan halaman
                       actionButton("download_rata_page", "Unduh Halaman Ini (ZIP)", 
                                    icon = icon("download"), class = "btn-primary")
                )
              )
      ),
      
      # Halaman Uji Proporsi & Ragam
      tabItem(tabName = "uji_prop_ragam",
              h2("Uji Hipotesis Proporsi dan Ragam"),
              tabBox(
                id = "tabset_uji_prop_ragam", width = 12,
                tabPanel("Uji Proporsi", icon = icon("percentage"),
                         fluidRow(
                           column(width = 4,
                                  box(title = "Uji Proporsi Satu Sampel", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
                                      selectInput("prop_one_var", "1. Pilih Variabel (Kategorik):", choices = NULL),
                                      uiOutput("prop_one_success_level_ui"),
                                      numericInput("prop_one_p0", "3. Nilai Hipotesis (p₀):", value = 0.5, min = 0, max = 1, step = 0.01),
                                      radioButtons("prop_one_alternative", "4. Hipotesis Alternatif (H₁):",
                                                   choices = c("Proporsi ≠ p₀" = "two.sided",
                                                               "Proporsi > p₀" = "greater",
                                                               "Proporsi < p₀" = "less")),
                                      radioButtons("prop_one_alpha", "5. Tingkat Signifikansi (α):",
                                                   choices = c(0.01, 0.05, 0.10), selected = 0.05, inline = TRUE),
                                      br(),
                                      actionButton("run_prop_one_test", "Jalankan Uji 1 Sampel", icon = icon("play"), width = "100%", class = "btn-success"),
                                      downloadButton("download_prop_one", "Unduh Hasil (.docx)", class="btn-sm btn-info", style="width:100%; margin-top: 5px;")
                                  ),
                                  box(title = "Uji Proporsi Dua Sampel", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                                      selectInput("prop_two_var", "1. Pilih Variabel untuk Diuji:", choices = NULL),
                                      uiOutput("prop_two_success_level_ui"),
                                      selectInput("prop_two_group_var", "3. Pilih Variabel Grup (2 Kategori):", choices = NULL),
                                      radioButtons("prop_two_alternative", "4. Hipotesis Alternatif (H₁):",
                                                   choices = c("Proporsi Grup 1 ≠ Grup 2" = "two.sided",
                                                               "Proporsi Grup 1 > Grup 2" = "greater",
                                                               "Proporsi Grup 1 < Grup 2" = "less")),
                                      radioButtons("prop_two_alpha", "5. Tingkat Signifikansi (α):",
                                                   choices = c(0.01, 0.05, 0.10), selected = 0.05, inline = TRUE),
                                      br(),
                                      actionButton("run_prop_two_test", "Jalankan Uji 2 Sampel", icon = icon("play"), width = "100%", class = "btn-primary"),
                                      downloadButton("download_prop_two", "Unduh Hasil (.docx)", class="btn-sm btn-info", style="width:100%; margin-top: 5px;")
                                  )
                           ),
                           column(width = 8,
                                  box(title = "Hasil Uji Proporsi", status = "primary", solidHeader = TRUE, width = 12,
                                      verbatimTextOutput("prop_test_output"),
                                      
                                  ),
                                  box(title = "Interpretasi Hasil", status = "success", solidHeader = TRUE, width = 12,
                                      uiOutput("prop_test_interpretation")
                                  )
                           )
                         )
                ),
                tabPanel("Uji Ragam", icon = icon("chart-bar"),
                         fluidRow(
                           column(width = 4,
                                  box(title = "Uji Ragam Satu Sampel (Chi-Square)", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
                                      selectInput("var_one_var", "1. Pilih Variabel (Numerik):", choices = NULL),
                                      numericInput("var_one_sigma2", "2. Nilai Hipotesis Ragam (σ²₀):", value = 1, min = 0),
                                      radioButtons("var_one_alternative", "3. Hipotesis Alternatif (H₁):",
                                                   choices = c("Ragam ≠ σ²₀" = "two.sided",
                                                               "Ragam > σ²₀" = "greater",
                                                               "Ragam < σ²₀" = "less")),
                                      radioButtons("var_one_alpha", "4. Tingkat Signifikansi (α):",
                                                   choices = c(0.01, 0.05, 0.10), selected = 0.05, inline = TRUE),
                                      br(),
                                      actionButton("run_var_one_test", "Jalankan Uji 1 Sampel", icon = icon("play"), width = "100%", class = "btn-success"),
                                      downloadButton("download_var_one", "Unduh Hasil (.docx)", class="btn-sm btn-info", style="width:100%; margin-top: 5px;")
                                  ),
                                  box(title = "Uji Ragam Dua Sampel (F-Test)", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                                      selectInput("var_two_num_var", "1. Pilih Variabel Dependen (Numerik):", choices = NULL),
                                      selectInput("var_two_cat_var", "2. Pilih Variabel Grup (2 Kategori):", choices = NULL),
                                      radioButtons("var_two_alternative", "3. Hipotesis Alternatif (H₁):",
                                                   choices = c("Rasio Ragam ≠ 1" = "two.sided",
                                                               "Rasio Ragam > 1" = "greater",
                                                               "Rasio Ragam < 1" = "less")),
                                      radioButtons("var_two_alpha", "4. Tingkat Signifikansi (α):",
                                                   choices = c(0.01, 0.05, 0.10), selected = 0.05, inline = TRUE),
                                      br(),
                                      actionButton("run_var_two_test", "Jalankan Uji 2 Sampel", icon = icon("play"), width = "100%", class = "btn-primary"),
                                      downloadButton("download_var_two", "Unduh Hasil (.docx)", class="btn-sm btn-info", style="width:100%; margin-top: 5px;")
                                  )
                           ),
                           column(width = 8,
                                  box(title = "Hasil Uji Ragam", status = "primary", solidHeader = TRUE, width = 12,
                                      verbatimTextOutput("var_test_output"),
                                      
                                  ),
                                  box(title = "Interpretasi Hasil", status = "success", solidHeader = TRUE, width = 12,
                                      uiOutput("var_test_interpretation")
                                  )
                           )
                         )
                )
              ),
              fluidRow(
                column(width = 12, align = "right",
                       # Tombol untuk memicu unduhan halaman
                       actionButton("download_prop_ragam_page", "Unduh Halaman Ini (ZIP)", 
                                    icon = icon("download"), class = "btn-primary")
                )
              )
      ),
      
      # Halaman ANOVA
      tabItem(tabName = "anova",
              h2("Uji Beda Rata-rata > 2 Kelompok (ANOVA)"),
              fluidRow(
                column(width = 4,
                       box(
                         title = "Pengaturan ANOVA", status = "warning", solidHeader = TRUE, width = 12,
                         p("ANOVA digunakan untuk menguji apakah terdapat perbedaan rata-rata yang signifikan antara tiga atau lebih kelompok independen."),
                         hr(),
                         selectInput("anova_num_var", "1. Pilih Variabel Dependen (Numerik):", choices = NULL),
                         selectInput("anova_cat_var", "2. Pilih Variabel Grup (Kategorik > 2 Level):", choices = NULL),
                         radioButtons("anova_alpha", "3. Pilih Tingkat Signifikansi (α):",
                                      choices = c(0.01, 0.05, 0.10), selected = 0.05, inline = TRUE),
                         br(),
                         actionButton("run_anova", "Jalankan Analisis ANOVA", icon = icon("play"), width = "100%", class = "btn-success")
                       )
                ),
                column(width = 8,
                       tabBox(
                         id = "tabset_anova_results", width = 12,
                         tabPanel("Hasil Utama & Plot", icon = icon("table-list"),
                                  fluidRow(
                                    column(width = 7,
                                           h4("Tabel Ringkasan ANOVA"),
                                           verbatimTextOutput("anova_summary_output")
                                           
                                    ),
                                    column(width = 5,
                                           h4("Visualisasi Box Plot"),
                                           plotOutput("anova_plot_output", height = "250px")
                                    )
                                  ),
                                  fluidRow(
                                    column(width = 7,
                                           downloadButton("download_anova_summary", "Unduh Tabel ANOVA (.docx)", class="btn-default btn-sm btn-block", style="width:100%; margin-top:10px;")
                                    ),
                                    column(width = 5,
                                           downloadButton("download_anova_plot", "Unduh Box Plot (.png)", class="btn-default btn-sm btn-block", style="width:100%; margin-top:10px;")
                                    )
                                  )
                         ),
                         tabPanel("Uji Lanjutan (Post-Hoc)", icon = icon("magnifying-glass-chart"),
                                  h4("Tukey Honest Significant Differences (HSD)"),
                                  p("Tabel ini menunjukkan perbandingan rata-rata antar setiap pasangan kelompok. Perhatikan kolom 'p adj' (p-value yang disesuaikan). Jika 'p adj' < α, maka terdapat perbedaan yang signifikan antara kedua kelompok tersebut."),
                                  verbatimTextOutput("anova_posthoc_output"),
                                  downloadButton("download_anova_posthoc", "Unduh Hasil Uji Lanjutan (.docx)", class = "btn-default btn-sm btn-block")
                         ),
                         tabPanel("Interpretasi", icon = icon("comment-dots"),
                                  uiOutput("anova_interpretation"),
                                  downloadButton("download_anova_interpretation", "Unduh Laporan Interpretasi (.docx)", class = "btn-default btn-sm btn-block")
                         )
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       # --- PERUBAHAN DI SINI ---
                       # Ganti juga untuk tab ini dengan satu actionButton
                       div(style = "display: flex; justify-content: flex-end;",
                           actionButton("download_anova_page", "Unduh Halaman Ini (ZIP)", icon = icon("download"), class = "btn-primary")
                       )
                )
              )
      ),
      
      # Halaman Regresi Linear
      tabItem(tabName = "regresi",
              h2("Analisis Regresi Linear Berganda"),
              fluidRow(
                column(width = 4,
                       box(
                         title = "Pengaturan Model Regresi", status = "warning", solidHeader = TRUE, width = 12,
                         p("Bangun model untuk memprediksi satu variabel dependen (Y) dari satu atau lebih variabel independen (X)."),
                         hr(),
                         selectInput("reg_dep_var", "1. Pilih Variabel Dependen (Y):", choices = NULL),
                         selectizeInput("reg_indep_vars", "2. Pilih Variabel Independen (X):", choices = NULL, multiple = TRUE),
                         radioButtons("reg_alpha", "3. Pilih Tingkat Signifikansi (α):",
                                      choices = c(0.01, 0.05, 0.10), selected = 0.05, inline = TRUE),
                         br(),
                         actionButton("run_regression", "Jalankan Analisis Regresi", icon = icon("play"), width = "100%", class = "btn-success")
                       )
                ),
                column(width = 8,
                       tabBox(
                         id = "tabset_regression_results", width = 12,
                         tabPanel("Ringkasan Model", icon = icon("clipboard-list"),
                                  h4("Output Model Regresi"),
                                  verbatimTextOutput("reg_summary_output"),
                                  downloadButton("download_reg_summary", "Unduh Ringkasan Model (.docx)", class="btn-default btn-sm btn-block")
                         ),
                         tabPanel("Uji Asumsi Klasik", icon = icon("check-circle"),
                                  h4("1. Uji Normalitas Residual (Shapiro-Wilk)"),
                                  verbatimTextOutput("reg_normality_test"),
                                  hr(),
                                  h4("2. Uji Homoskedastisitas (Breusch-Pagan)"),
                                  verbatimTextOutput("reg_hetero_test"),
                                  hr(),
                                  h4("3. Uji Autokorelasi (Durbin-Watson)"),
                                  verbatimTextOutput("reg_autocor_test"),
                                  hr(),
                                  h4("4. Uji Multikolinearitas (Variance Inflation Factor - VIF)"),
                                  verbatimTextOutput("reg_vif_test"),
                                  downloadButton("download_reg_assumptions", "Unduh Hasil Uji Asumsi (.docx)", class="btn-default btn-sm btn-block")
                         ),
                         tabPanel("Visualisasi Residual", icon = icon("chart-line"),
                                  fluidRow(
                                    column(width = 6,
                                           h4("Residuals vs Fitted Plot"),
                                           plotOutput("reg_plot_residuals_fitted", height = "300px")
                                    ),
                                    column(width = 6,
                                           h4("Normal Q-Q Plot"),
                                           plotOutput("reg_plot_qq", height = "300px")
                                    )
                                  ),
                                  downloadButton("download_reg_plots", "Unduh Visualisasi Residual (.png)", class="btn-default btn-sm btn-block")
                         ),
                         tabPanel("Interpretasi Lengkap", icon = icon("comment-dots"),
                                  uiOutput("reg_interpretation"),
                                  downloadButton("download_reg_interpretation", "Unduh Laporan Interpretasi (.docx)", class="btn-default btn-sm btn-block")
                         )
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       # --- PERUBAHAN DI SINI ---
                       # Ganti juga untuk tab ini dengan satu actionButton
                       div(style = "display: flex; justify-content: flex-end;",
                           actionButton("download_regr_page", "Unduh Halaman Ini (ZIP)", icon = icon("download"), class = "btn-primary")
                       )
                )
              )
      )
    )
  )
)

#==============================================================================
#                                 SERVER
#==============================================================================

# 1. Muat data STATISTIK dan PASTIKAN TIPE DATA BENAR
sovi_plain_raw <- read.csv("sovi_data.csv")

# Perbaikan: Konversi semua kolom menjadi numerik, kecuali DISTRICTCODE
# Ini akan mengatasi masalah kolom yang terbaca sebagai teks
sovi_plain <- sovi_plain_raw %>%
  dplyr::mutate(across(-DISTRICTCODE, ~ as.numeric(as.character(.))))

# Tampilkan peringatan jika ada kolom yang gagal dikonversi (menjadi NA semua)
for (col_name in names(sovi_plain)) {
  if (all(is.na(sovi_plain[[col_name]]))) {
    warning(paste("Perhatian: Kolom", col_name, "menjadi NA semua setelah konversi. Cek file CSV Anda."))
  }
}

server <- function(input, output, session) {
  
  # --- 1. SETUP UTAMA & PEMUATAN DATA ---
  rv <- reactiveValues(
    data = sovi_plain,
    numeric_vars = names(sovi_plain %>% select_if(is.numeric) %>% select(-any_of("DISTRICTCODE"))),
    categorical_vars = names(sovi_plain %>% select_if(function(x) is.character(x) || is.factor(x))),
    data_gabungan = NULL
  )
  
  # Objek reaktif lainnya untuk menyimpan hasil
  transform_results <- reactiveValues()
  math_transform_results <- reactiveValues()
  prop_ragam_results <- reactiveValues()
  
  # --- 2. PUSAT KONTROL UI (Dropdown Updater) ---
  observe({
    # Hanya jalankan jika rv$data sudah terisi
    req(rv$data)
    
    # Update SEMUA dropdown di seluruh aplikasi
    updateSelectInput(session, "var_to_transform", choices = rv$numeric_vars)
    updateSelectInput(session, "var_to_math_transform", choices = rv$numeric_vars)
    updateSelectizeInput(session, "desc_vars", choices = rv$numeric_vars)
    updateSelectInput(session, "norm_var", choices = rv$numeric_vars)
    updateSelectInput(session, "homog_var_num", choices = rv$numeric_vars)
    updateSelectInput(session, "homog_var_cat", choices = rv$categorical_vars)
    updateSelectInput(session, "t_test_one_var", choices = rv$numeric_vars)
    updateSelectInput(session, "t_test_two_num_var", choices = rv$numeric_vars)
    updateSelectInput(session, "t_test_two_cat_var", choices = rv$categorical_vars)
    updateSelectInput(session, "prop_one_var", choices = rv$categorical_vars)
    updateSelectInput(session, "prop_two_var", choices = rv$categorical_vars)
    updateSelectInput(session, "prop_two_group_var", choices = rv$categorical_vars) 
    updateSelectInput(session, "var_one_var", choices = rv$numeric_vars)
    updateSelectInput(session, "var_two_num_var", choices = rv$numeric_vars)
    updateSelectInput(session, "var_two_cat_var", choices = rv$categorical_vars)
    updateSelectInput(session, "anova_num_var", choices = rv$numeric_vars)
    updateSelectInput(session, "anova_cat_var", choices = rv$categorical_vars)
    updateSelectInput(session, "reg_dep_var", choices = rv$numeric_vars)
    updateSelectizeInput(session, "reg_indep_vars", choices = rv$numeric_vars)
    updateSelectInput(session, "map_var", choices = rv$numeric_vars)
    updateSelectInput(session, "moran_var", choices = rv$numeric_vars, selected = "POVERTY")
  })
  
  # --- 3. LOGIKA UNTUK SETIAP TAB ---
  
  # --- LOGIKA BERANDA ---
  output$total_distrik <- renderValueBox({ req(rv$data); valueBox(nrow(rv$data), "Total Distrik", icon = icon("map-marker-alt"), color = "purple") })
  output$rata_kemiskinan <- renderValueBox({ req(rv$data, "POVERTY" %in% names(rv$data)); valueBox(paste0(round(mean(rv$data$POVERTY, na.rm=TRUE), 2), " %"), "Rata-Rata Kemiskinan", icon = icon("percent"), color = "orange") })
  output$rata_tanpa_listrik <- renderValueBox({ req(rv$data, "NOELECTRIC" %in% names(rv$data)); valueBox(paste0(round(mean(rv$data$NOELECTRIC, na.rm=TRUE), 2), " %"), "Rata-Rata Tanpa Listrik", icon = icon("bolt"), color = "green") })
  
  # --- LOGIKA UNTUK TOMBOL DOWNLOAD ---
  
  # 1. Download Data Indikator SOVI
  output$downloadSOVI <- downloadHandler(
    filename = function() { "sovi_data.csv" },
    content = function(file) {
      # Mengunduh data utama yang ada di rv$data
      req(rv$data)
      write.csv(rv$data, file, row.names = FALSE)
    }
  )
  
  # 2. Download Data Geospasial
  output$downloadGeoJSON <- downloadHandler(
    filename = function() { "indonesia511.geojson" },
    content = function(file) {
      # Menyalin file geojson yang ada di folder proyek Anda
      # Pastikan path "data/indonesia511.geojson" sudah benar!
      file.copy("indonesia511.geojson", file)
    }
  )
  
  # 3. Download Data Jarak
  output$downloadDistance <- downloadHandler(
    filename = function() { "distance_clean.csv" },
    content = function(file) {
      # Mengunduh dari variabel yang Anda gunakan untuk memuat distance.csv
      # Pastikan Anda punya variabel 'distance_data' atau ganti dengan nama yang sesuai
      req(distance_data) 
      write.csv(distance_data, file, row.names = FALSE)
    }
  )
  
  # --- LOGIKA MANAJEMEN DATA ---
  # A. KATEGORISASI
  observeEvent(input$apply_transform, {
    req(rv$data, input$var_to_transform)
    var_name <- input$var_to_transform
    original_var <- rv$data[[var_name]]
    
    transform_results$before_plot <- ggplot(data.frame(x=original_var), aes(x=x)) + geom_histogram(bins=30, fill="steelblue", color="white") + labs(title=paste("Histogram Asli:", var_name), x=var_name) + theme_minimal()
    transform_results$before_summary <- summary(original_var)
    
    breaks <- input$num_categories
    new_col_name <- paste0(var_name, "_cat", breaks)
    
    new_var <- NULL
    # Reset mapping setiap kali dijalankan
    transform_results$category_mapping <- NULL 
    
    if(input$transform_method == "quantile"){
      metode_teks <- "Kuantil (Sama Frekuensi)"
      cut_breaks <- quantile(original_var, probs = seq(0, 1, by = 1/breaks), na.rm = TRUE)
      # Membuat label K1, K2, dst.
      labels_to_use <- paste0("K", 1:(length(unique(cut_breaks))-1))
      new_var <- cut(original_var, breaks = unique(cut_breaks), include.lowest = TRUE, labels = labels_to_use)
      
    } else { # Metode "Interval (Sama Lebar)"
      metode_teks <- "Interval (Sama Lebar)"
      
      # 1. Pilih set label deskriptif berdasarkan jumlah kategori
      label_set <- list(
        `2` = c("Rendah", "Tinggi"),
        `3` = c("Rendah", "Sedang", "Tinggi"),
        `4` = c("Sangat Rendah", "Rendah", "Tinggi", "Sangat Tinggi"),
        `5` = c("Sangat Rendah", "Rendah", "Sedang", "Tinggi", "Sangat Tinggi")
      )
      labels_to_use <- label_set[[as.character(breaks)]]
      if (is.null(labels_to_use)) {
        labels_to_use <- paste("Grup", 1:breaks) # Fallback untuk jumlah lain
      }
      
      # 2. Buat variabel kategori dengan label deskriptif
      new_var <- cut(original_var, breaks = breaks, include.lowest = TRUE, labels = labels_to_use)
      
      # 3. Simpan rentang interval asli untuk ditampilkan di interpretasi
      temp_cut_for_ranges <- cut(original_var, breaks = breaks, include.lowest = TRUE)
      interval_ranges <- levels(temp_cut_for_ranges)
      transform_results$category_mapping <- setNames(interval_ranges, labels_to_use)
    }
    
    rv$data[[new_col_name]] <- new_var
    
    # Update list variabel numerik dan kategorikal
    rv$numeric_vars <- rv$data %>% select_if(is.numeric) %>% select(-any_of("DISTRICTCODE")) %>% names()
    rv$categorical_vars <- rv$data %>% select_if(function(x) is.character(x) || is.factor(x)) %>% names()
    
    # Update plot dan summary
    df_after <- data.frame(kategori = new_var) %>% filter(!is.na(kategori))
    transform_results$after_plot <- ggplot(df_after, aes(x=kategori)) + geom_bar(fill="seagreen", color="white") + labs(title=paste("Bar Chart:", new_col_name), x="Kategori") + theme_minimal()
    transform_results$after_summary <- table(new_var, dnn = "Frekuensi per Kategori")
    
    # --- INTERPRETASI KATEGORISASI (DIPERBARUI) ---
    tabel_frekuensi <- transform_results$after_summary
    frekuensi_teks <- paste(names(tabel_frekuensi), " (", tabel_frekuensi, ")", collapse = ", ", sep = "")
    
    keterangan_teks <- ""
    # Tambahkan keterangan rentang jika metode adalah interval
    if (input$transform_method != "quantile" && !is.null(transform_results$category_mapping)) {
      mapping_list <- sapply(names(transform_results$category_mapping), function(name) {
        paste0("<li><b>", name, "</b>: mencakup rentang nilai ", transform_results$category_mapping[[name]], "</li>")
      })
      keterangan_teks <- paste0(
        "<p><b>Keterangan Kategori:</b></p><ul>",
        paste(mapping_list, collapse = ""),
        "</ul>"
      )
    }
    
    interpretasi_html <- HTML(paste0(
      "<p>Variabel <b>", var_name, "</b> berhasil dikategorikan menjadi <b>", breaks, "</b> kelompok menggunakan metode <b>", metode_teks, "</b>. ",
      "Hasilnya disimpan dalam kolom baru bernama <b>", new_col_name, "</b>.</p>",
      "<p><b>Distribusi Frekuensi:</b> ", frekuensi_teks, ".</p>",
      keterangan_teks # Menambahkan keterangan rentang di sini
    ))
    
    transform_results$interpretation <- interpretasi_html
    transform_results$data_table <- rv$data
  })
  
  output$before_plot <- renderPlot({ transform_results$before_plot })
  output$before_summary <- renderPrint({ transform_results$before_summary })
  output$after_plot <- renderPlot({ transform_results$after_plot })
  output$after_summary <- renderPrint({ transform_results$after_summary })
  output$interpretation_text <- renderUI({ transform_results$interpretation })
  
  # --- Menampilkan Tabel Data Hasil dengan Tombol Ekspor ---
  output$transformed_data_table <- renderDT({
    req(transform_results$data_table)
    
    datatable(
      transform_results$data_table,
      extensions = 'Buttons', # <-- Menambahkan ekstensi tombol
      options = list(
        scrollX = TRUE,
        pageLength = 5,
        dom = 'frtip', 
        buttons = list(
          list(extend = 'csv', filename = paste0("sovi_data-", Sys.Date())),
          list(extend = 'excel', filename = paste0("sovi_data-", Sys.Date()))
        )
      )
    )
  })
  
  # --- LOGIKA UNTUK UNDUH GRAFIK KATEGORISASI VARIABEL ---
  
  # 1. Unduh Grafik Sebelum Transformasi
  output$downloadBeforePlot <- downloadHandler(
    filename = function() {
      paste0("grafik_sebelum_", input$var_to_transform, ".png")
    },
    content = function(file) {
      req(transform_results$before_plot)
      ggsave(file, plot = transform_results$before_plot, device = "png", width = 6, height = 4)
    }
  )
  
  # 2. Unduh Grafik Sesudah Transformasi
  output$downloadAfterPlot <- downloadHandler(
    filename = function() {
      paste0("grafik_sesudah_", input$var_to_transform, ".png")
    },
    content = function(file) {
      req(transform_results$after_plot)
      ggsave(file, plot = transform_results$after_plot, device = "png", width = 6, height = 4)
    }
  )
  
  
  # --- LOGIKA UNTUK UNDUH TABEL DATA UTAMA ---
  
  # 1. Unduh Tabel Utama sebagai CSV
  output$downloadMainTableCSV <- downloadHandler(
    filename = function() {
      paste0("data_hasil_kategorisasi-", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(transform_results$data_table)
      write.csv(transform_results$data_table, file, row.names = FALSE)
    }
  )
  
  # 2. Unduh Tabel Utama sebagai Excel
  output$downloadMainTableXLSX <- downloadHandler(
    filename = function() {
      paste0("data_hasil_kategorisasi-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(transform_results$data_table)
      writexl::write_xlsx(transform_results$data_table, file)
    }
  )
  
  # B. TRANSFORMASI MATEMATIKA
  observeEvent(input$apply_math_transform, {
    req(rv$data, input$var_to_math_transform)
    var_name <- input$var_to_math_transform
    original_var <- rv$data[[var_name]]
    
    math_transform_results$before_plot <- ggplot(data.frame(x=original_var), aes(x=x)) + geom_histogram(bins=30, fill="steelblue", color="white") + labs(title=paste("Distribusi Asli:", var_name), x=var_name) + theme_minimal()
    
    transformed_var <- NULL; suffix <- ""
    if(input$math_transform_method == "log"){
      transformed_var <- log(original_var + 1); suffix <- "_log"
    } else if(input$math_transform_method == "sqrt"){
      transformed_var <- sqrt(pmax(0, original_var)); suffix <- "_sqrt"
    } else {
      transformed_var <- scale(original_var); suffix <- "_scaled"
    }
    
    math_transform_results$before_summary <- summary(original_var)
    math_transform_results$after_summary <- summary(as.numeric(transformed_var))
    
    new_math_col_name <- paste0(var_name, suffix)
    rv$data[[new_math_col_name]] <- as.numeric(transformed_var)
    
    rv$numeric_vars <- rv$data %>% 
      select_if(is.numeric) %>% 
      select(-any_of("DISTRICTCODE")) %>% 
      names()
    
    rv$categorical_vars <- rv$data %>% 
      select_if(function(x) is.character(x) || is.factor(x)) %>% 
      names()
    
    math_transform_results$after_plot <- ggplot(data.frame(x=as.numeric(transformed_var)), aes(x=x)) + geom_histogram(bins=30, fill="darkorange", color="white") + labs(title=paste("Distribusi Hasil Transformasi"), x=new_math_col_name) + theme_minimal()
    
    # --- INTERPRETASI TRANSFORMASI ----
    # Tentukan teks metode transformasi berdasarkan input
    metode_math_teks <- dplyr::case_when(
      input$math_transform_method == "log" ~ "Logaritma (log(x+1))",
      input$math_transform_method == "sqrt" ~ "Akar Kuadrat (sqrt)",
      TRUE ~ "Standardisasi (Z-score)"
    )
    
    # Analisis perubahan distribusi menggunakan skewness
    skew_sebelum <- round(moments::skewness(original_var, na.rm = TRUE), 3)
    skew_sesudah <- round(moments::skewness(as.numeric(transformed_var), na.rm = TRUE), 3)
    
    # Tentukan deskripsi perubahan simetri
    # Jika absolut skewness berkurang, maka lebih simetris
    deskripsi_perubahan <- if (abs(skew_sesudah) < abs(skew_sebelum)) {
      "menjadi <b>lebih simetris</b>"
    } else {
      "<b>tidak menjadi lebih simetris</b>"
    }
    
    # Buat teks interpretasi yang dinamis
    interpretasi_math_html <- HTML(paste0(
      "<p>Variabel <b>", var_name, "</b> telah ditransformasi menggunakan metode <b>", metode_math_teks, "</b>. Hasilnya disimpan dalam kolom <b>", new_math_col_name, "</b>.</p>",
      "<p>Transformasi ini mengubah bentuk distribusi data. Berdasarkan analisis kemiringan (skewness), distribusi data ", deskripsi_perubahan, ". ",
      "Nilai skewness berubah dari ", skew_sebelum, " (sebelum) menjadi <b>", skew_sesudah, "</b> (sesudah).</p>"
    ))
    
    # Simpan interpretasi baru
    math_transform_results$interpretation <- interpretasi_math_html
    
  })
  
  output$before_math_summary <- renderPrint({
    req(math_transform_results$before_summary)
    math_transform_results$before_summary
  })
  
  output$after_math_summary <- renderPrint({
    req(math_transform_results$after_summary)
    math_transform_results$after_summary
  })
  
  # --- LOGIKA UNDUH GRAFIK TRASNFORMASI MATEMATIKA ---
  # 1. Unduh Grafik SEBELUM Transformasi Matematika
  output$downloadBeforeMathPlot <- downloadHandler(
    filename = function() {
      paste0("grafik_sebelum_transformasi_", input$var_to_math_transform, ".png")
    },
    content = function(file) {
      # Pastikan plot sudah ada sebelum mencoba menyimpan
      req(math_transform_results$before_plot)
      ggsave(file, plot = math_transform_results$before_plot, device = "png", width = 8, height = 6)
    }
  )
  
  # 2. Unduh Grafik SESUDAH Transformasi Matematika
  output$downloadAfterMathPlot <- downloadHandler(
    filename = function() {
      paste0("grafik_sesudah_transformasi_", input$var_to_math_transform, ".png")
    },
    content = function(file) {
      # Pastikan plot sudah ada sebelum mencoba menyimpan
      req(math_transform_results$after_plot)
      ggsave(file, plot = math_transform_results$after_plot, device = "png", width = 8, height = 6)
    }
  )
  
  output$before_math_plot <- renderPlot({ math_transform_results$before_plot })
  output$after_math_plot <- renderPlot({ math_transform_results$after_plot })
  output$math_interpretation_text <- renderUI({ math_transform_results$interpretation })
  
  # --- LOGIKA UNDUH HALAMAN MANAJEMEN DATA ---
  # --- GANTI SELURUH BLOK INI ---
  output$downloadAll <- downloadHandler(
    
    filename = function() {
      # Memberi nama unik pada file ZIP berdasarkan tanggal
      paste0("Hasil-Analisis-Lengkap-", Sys.Date(), ".zip")
    },
    
    content = function(file) {
      # Membuat direktori sementara yang bersih untuk setiap unduhan
      temp_directory <- file.path(tempdir(), paste0("hasil-lengkap-", as.integer(Sys.time())))
      dir.create(temp_directory, showWarnings = FALSE, recursive = TRUE)
      
      # ---- 1. SIMPAN DATA TERBARU (.csv) ----
      write.csv(rv$data, file.path(temp_directory, "data_gabungan_hasil.csv"), row.names = FALSE)
      
      # ---- 2. SIMPAN SEMUA GRAFIK (.png) ----
      if (!is.null(transform_results$before_plot)) {
        ggsave(filename = file.path(temp_directory, "1_grafik_sebelum_kategorisasi.png"), plot = transform_results$before_plot, device = "png")
      }
      if (!is.null(transform_results$after_plot)) {
        ggsave(filename = file.path(temp_directory, "2_grafik_sesudah_kategorisasi.png"), plot = transform_results$after_plot, device = "png")
      }
      if (!is.null(math_transform_results$before_plot)) {
        ggsave(filename = file.path(temp_directory, "3_grafik_sebelum_transformasi.png"), plot = math_transform_results$before_plot, device = "png")
      }
      if (!is.null(math_transform_results$after_plot)) {
        ggsave(filename = file.path(temp_directory, "4_grafik_sesudah_transformasi.png"), plot = math_transform_results$after_plot, device = "png")
      }
      
      # ---- 3. BUAT LAPORAN INTERPRETASI (.docx) ----
      temp_rmd_path <- file.path(temp_directory, "laporan_manajemen.Rmd")
      # (Logika pembuatan konten RMD Anda di sini, ini sudah benar)
      # ...
      # Pastikan Anda punya library rvest dan xml2
      library(rvest); library(xml2)
      teks_kategorisasi <- if (!is.null(transform_results$interpretation)) { html_text(read_html(as.character(transform_results$interpretation))) } else { "Tidak ada interpretasi kategorisasi yang dibuat." }
      teks_transformasi <- if (!is.null(math_transform_results$interpretation)) { html_text(read_html(as.character(math_transform_results$interpretation))) } else { "Tidak ada interpretasi transformasi yang dibuat." }
      konten_rmd <- paste("---", "title: 'Laporan Interpretasi Analisis Data'", paste("date:", Sys.Date()), "output: word_document", "---", "", "## Interpretasi Hasil Kategorisasi Variabel", teks_kategorisasi, "", "## Interpretasi Hasil Transformasi Variabel", teks_transformasi, sep = "\n")
      writeLines(konten_rmd, temp_rmd_path)
      
      # Render .Rmd menjadi .docx
      rmarkdown::render(
        input = temp_rmd_path,
        output_file = "laporan_interpretasi.docx", # Tentukan nama file outputnya
        output_dir = temp_directory,
        quiet = TRUE
      )
      
      # ---- 4. BUAT FILE ZIP (VERSI PERBAIKAN) ----
      
      # Simpan direktori kerja saat ini
      old_wd <- getwd()
      # Pindah ke direktori temporer agar path di dalam zip menjadi flat
      setwd(temp_directory)
      
      # Definisikan secara EKSPLISIT file mana saja yang ingin dimasukkan
      files_to_zip <- c(
        "data_gabungan_hasil.csv",
        "1_grafik_sebelum_kategorisasi.png",
        "2_grafik_sesudah_kategorisasi.png",
        "3_grafik_sebelum_transformasi.png",
        "4_grafik_sesudah_transformasi.png",
        "laporan_interpretasi.docx" # Nama file .docx hasil render
      )
      
      # Zip HANYA file-file yang ada di dalam daftar di atas
      # Kita gunakan utils::zip yang merupakan bawaan R dan lebih standar
      utils::zip(
        zipfile = file,
        files = files_to_zip
      )
      
      # KEMBALI ke direktori kerja semula (SANGAT PENTING!)
      setwd(old_wd)
    },
    
    contentType = "application/zip"
  )
  
  observeEvent(input$show_download_modal, {
    showModal(
      modalDialog(
        title = "Konfirmasi Unduhan Halaman",
        
        # Perbarui daftar file agar sesuai dengan isi ZIP
        p("Anda akan mengunduh sebuah file ZIP yang berisi:"),
        tags$ul(
          tags$li(tags$b("data_gabungan_hasil.csv"), " tabel data terbaru"),
          tags$li(tags$b("Grafik Kategorisasi"), " (Sebelum & Sesudah)"),
          tags$li(tags$b("Grafik Transformasi"), " (Sebelum & Sesudah)"),
          tags$li(tags$b("Laporan_Interpretasi.docx"), " berisi semua interpretasi")
        ),
        
        footer = tagList(
          modalButton("Batal"),
          downloadButton("downloadAll", "Ya, Lanjutkan Unduh")
        ),
        easyClose = TRUE
      )
    )
  })
  
  # --- LOGIKA EKSPLORASI DATA ---
  observeEvent(input$desc_vars, {
    req(rv$data, input$desc_vars)
    
    # Ambil subset data
    data_subset <- rv$data %>% select(all_of(input$desc_vars))
    
    # HITUNG DAN SIMPAN SUMMARY KE rv
    rv$summary_deskriptif <- summary(data_subset)
    
    # Anda juga bisa menyimpan data subsetnya jika perlu di tempat lain
    rv$desc_data_subset <- data_subset 
  })
  
  # Fungsi render sekarang HANYA menampilkan apa yang ada di rv
  output$desc_summary_output <- renderPrint({
    req(rv$summary_deskriptif)
    rv$summary_deskriptif
  })
  
  # Download handler Anda sekarang akan bekerja karena rv$summary_deskriptif sudah ada isinya
  output$downloadSummary <- downloadHandler(
    filename = function() {
      "Ringkasan_Statistik_Deskriptif.docx"
    },
    content = function(file) {
      library(rmarkdown)
      req(rv$summary_deskriptif)
      
      temp_rmd_path <- file.path(tempdir(), "summary_report.Rmd")
      
      # Ambil teks summary
      summary_text_output <- capture.output(rv$summary_deskriptif)
      
      # ---- PERUBAHAN DI SINI ----
      # Gabungkan teks summary menjadi satu string panjang
      # dan tambahkan backtick (`) untuk membuatnya jadi blok teks biasa
      formatted_summary_text <- paste("```", paste(summary_text_output, collapse = "\n"), "```", sep = "\n")
      
      konten_rmd <- paste(
        "---",
        "title: 'Ringkasan Statistik Deskriptif'",
        "output: word_document",
        "---",
        "",
        "Berikut adalah ringkasan statistik untuk variabel yang dipilih:",
        "",
        formatted_summary_text, # Masukkan teks yang sudah diformat di sini
        sep = "\n"
      )
      
      writeLines(konten_rmd, temp_rmd_path)
      
      render(
        input = temp_rmd_path,
        output_format = "word_document",
        output_file = file,
        quiet = TRUE
      )
    }
  )
  
  desc_data <- eventReactive(input$desc_vars, { req(rv$data, input$desc_vars); rv$data %>% select(all_of(input$desc_vars)) })
  output$desc_interpretation <- renderUI({ HTML("<p>Tabel di atas menampilkan ringkasan statistik dasar untuk variabel yang Anda pilih.</p>") })
  output$plot_ui <- renderUI({ req(rv$numeric_vars); if (input$plot_type == "Scatter Plot") { tagList(selectInput("scatter_x", "Variabel Sumbu X:", choices = rv$numeric_vars), selectInput("scatter_y", "Variabel Sumbu Y:", choices = rv$numeric_vars)) } else { selectInput("hist_box_var", "Pilih Variabel:", choices = rv$numeric_vars) } })
  output$main_plot <- renderPlot({ req(rv$data); if (input$plot_type == "Histogram") { req(input$hist_box_var); ggplot(rv$data, aes_string(x = input$hist_box_var)) + geom_histogram(bins = 30, fill = "cornflowerblue", color = "white") + theme_minimal(base_size = 14) + labs(title = paste("Histogram untuk", input$hist_box_var)) } else if (input$plot_type == "Box Plot") { req(input$hist_box_var); ggplot(rv$data, aes_string(y = input$hist_box_var)) + geom_boxplot(fill = "lightseagreen") + theme_minimal(base_size = 14) + labs(title = paste("Box Plot untuk", input$hist_box_var)) } else if (input$plot_type == "Scatter Plot") { req(input$scatter_x, input$scatter_y); ggplot(rv$data, aes_string(x = input$scatter_x, y = input$scatter_y)) + geom_point(alpha = 0.6, color = "dodgerblue") + geom_smooth(method = "lm", se = FALSE, color = "tomato") + theme_minimal(base_size = 14) + labs(title = paste("Scatter Plot antara", input$scatter_x, "dan", input$scatter_y)) } })
  output$desc_interpretation <- renderUI({
    req(desc_data())
    data_to_summarize <- desc_data()
    
    if (is.null(data_to_summarize) || ncol(data_to_summarize) == 0) {
      return(HTML("<p>Pilih satu atau lebih variabel untuk melihat interpretasi statistiknya.</p>"))
    }
    
    # Membuat list untuk menyimpan teks interpretasi per variabel
    interpretations <- lapply(names(data_to_summarize), function(var_name) {
      col_data <- data_to_summarize[[var_name]]
      
      # Cek jika variabel numerik
      if (is.numeric(col_data)) {
        stats <- summary(col_data)
        mean_val <- round(mean(col_data, na.rm = TRUE), 2)
        median_val <- round(median(col_data, na.rm = TRUE), 2)
        q1_val <- round(stats[["1st Qu."]], 2)
        q3_val <- round(stats[["3rd Qu."]], 2)
        min_val <- round(stats[["Min."]], 2)
        max_val <- round(stats[["Max."]], 2)
        
        # Membuat teks interpretasi dinamis untuk variabel numerik
        paste0("<li>Untuk variabel <b>", var_name, "</b>: ",
               "Nilai berkisar dari ", min_val, " hingga ", max_val, ". ",
               "Rata-rata (mean) adalah <b>", mean_val, "</b> dan nilai tengah (median) adalah <b>", median_val, "</b>. ",
               "Setengah dari data (IQR) terkonsentrasi antara ", q1_val, " dan ", q3_val, ".</li>")
      } else { # Jika variabel kategorikal (factor atau character)
        counts <- table(col_data)
        # Mengubah tabel frekuensi menjadi string
        count_str <- paste(names(counts), " (", counts, ")", collapse = ", ", sep = "")
        
        # Membuat teks interpretasi dinamis untuk variabel kategorikal
        paste0("<li>Variabel kategorikal <b>", var_name, "</b> memiliki distribusi frekuensi sebagai berikut: ", count_str, ".</li>")
      }
    })
    
    # Menggabungkan semua interpretasi menjadi satu blok HTML dengan unordered list
    HTML(paste0("<ul>", paste(interpretations, collapse = ""), "</ul>"))
  })
  
  # --- INTEPRETASI VISUALISASI GRAFIK ---
  output$plot_interpretation <- renderUI({
    req(rv$data)
    
    # Interpretasi untuk Histogram
    if (input$plot_type == "Histogram") {
      req(input$hist_box_var)
      HTML(paste0(
        "<p>Grafik Histogram ini menunjukkan sebaran data untuk variabel <b>", input$hist_box_var, "</b>. ",
        "Puncak tertinggi menunjukkan nilai yang paling sering muncul (modus), sedangkan lebar histogram memberikan gambaran tentang sebaran data secara keseluruhan. ",
        "Semakin lebar histogram, semakin tersebar datanya.</p>"
      ))
      
      # Interpretasi untuk Box Plot
    } else if (input$plot_type == "Box Plot") {
      req(input$hist_box_var)
      var_data <- rv$data[[input$hist_box_var]]
      # Cek apakah ada outlier
      outliers_detected <- length(boxplot.stats(na.omit(var_data))$out) > 0
      outlier_text <- if (outliers_detected) {
        " Titik di luar 'kumis' (whiskers) adalah pencilan (outlier) yang menunjukkan nilai yang jauh berbeda dari sebagian besar data."
      } else {
        "" # Tidak ada teks tambahan jika tidak ada outlier
      }
      
      HTML(paste0(
        "<p>Grafik Box Plot ini menggambarkan sebaran data untuk variabel <b>", input$hist_box_var, "</b>. ",
        "Garis tebal di tengah kotak adalah <b>median</b>, yang membagi data menjadi dua bagian sama besar. ",
        "Kotak itu sendiri menunjukkan <b>rentang interkuartil (IQR)</b>, tempat 50% data terpusat.",
        outlier_text, "</p>"
      ))
      
      # Interpretasi untuk Scatter Plot
    } else if (input$plot_type == "Scatter Plot") {
      req(input$scatter_x, input$scatter_y)
      x_var <- rv$data[[input$scatter_x]]
      y_var <- rv$data[[input$scatter_y]]
      
      # Hitung korelasi, pastikan tidak ada error jika ada data non-numerik
      corr_val <- tryCatch({
        round(cor(x_var, y_var, use = "complete.obs"), 3)
      }, error = function(e) { NA })
      
      if (is.na(corr_val)) {
        return(HTML("<p>Tidak dapat menghitung korelasi. Pastikan kedua variabel adalah numerik.</p>"))
      }
      
      # Tentukan kekuatan korelasi
      strength <- dplyr::case_when(
        abs(corr_val) >= 0.7 ~ "kuat",
        abs(corr_val) >= 0.4 ~ "sedang",
        TRUE ~ "lemah"
      )
      
      HTML(paste0(
        "<p>Grafik ini menunjukkan hubungan antara variabel <b>", input$scatter_x, "</b> dan <b>", input$scatter_y, "</b>. ",
        "Garis merah menunjukkan tren linear secara umum. ",
        "Nilai korelasi antar kedua variabel adalah <b>", corr_val, "</b>, menandakan hubungan yang <b>", strength, "</b>.</p>"
      ))
    }
  })
  
  # --- UNDUH GAMBAR VISUALISASI GRAFIK ---
  # Buat satu observeEvent untuk meng-update plot berdasarkan input
  observeEvent(list(input$plot_type, input$hist_box_var, input$scatter_x, input$scatter_y), {
    req(rv$data)
    
    # Logika untuk membuat plot (sama seperti yang Anda punya)
    plot_obj <- if (input$plot_type == "Histogram") {
      req(input$hist_box_var)
      ggplot(rv$data, aes_string(x = input$hist_box_var)) + geom_histogram(bins = 30, fill = "cornflowerblue") + labs(title = paste("Histogram untuk", input$hist_box_var))
    } else if (input$plot_type == "Box Plot") {
      req(input$hist_box_var)
      ggplot(rv$data, aes_string(y = input$hist_box_var)) + geom_boxplot(fill = "lightseagreen") + labs(title = paste("Box Plot untuk", input$hist_box_var))
    } else if (input$plot_type == "Scatter Plot") {
      req(input$scatter_x, input$scatter_y)
      ggplot(rv$data, aes_string(x = input$scatter_x, y = input$scatter_y)) + geom_point() + geom_smooth(method = "lm")
    }
    
    # SIMPAN PLOT KE rv
    rv$plot_visualisasi <- plot_obj
  })
  
  # Fungsi render sekarang HANYA menampilkan plot dari rv
  output$main_plot <- renderPlot({
    req(rv$plot_visualisasi)
    rv$plot_visualisasi
  })
  
  # Download handler Anda sekarang akan bekerja
  output$downloadPlot_eksplorasi <- downloadHandler(
    filename = function() { "Grafik_Eksplorasi.png" },
    content = function(file) {
      req(rv$plot_visualisasi)
      ggsave(file, plot = rv$plot_visualisasi, device = "png", width = 8, height = 6)
    }
  )
  
  # --- LOGIKA PETA INTERAKTIF ---
  # A. Buat data reaktif yang menggabungkan data peta dengan data SOVI (Ini sudah benar)
  map_data_reactive <- reactive({
    req(rv$data, input$map_var, map_data)
    sovi_df_map <- rv$data
    peta_lokal <- map_data
    
    sovi_df_map$DISTRICTCODE_clean <- sub("\\..*", "", trimws(sovi_df_map$DISTRICTCODE))
    peta_lokal$kodeprkab_clean <- trimws(as.character(peta_lokal$kodeprkab))
    
    data_gabungan <- merge(peta_lokal, sovi_df_map, 
                           by.x = "kodeprkab_clean", 
                           by.y = "DISTRICTCODE_clean", 
                           all.x = TRUE)
    
    data_gabungan <- sf::st_as_sf(data_gabungan)
    return(data_gabungan)
  })
  
  # B. Gunakan observeEvent untuk MEMBUAT dan MENYIMPAN objek peta
  observeEvent(input$map_var, {
    # Gunakan req() di awal untuk memastikan semua input siap
    req(map_data_reactive())
    
    map_data <- map_data_reactive()
    selected_var_with_suffix <- paste0(input$map_var, ".y")
    
    # Pastikan kolom data ada sebelum melanjutkan
    if (!selected_var_with_suffix %in% names(map_data) || all(is.na(map_data[[selected_var_with_suffix]]))) {
      # Buat peta kosong jika data tidak ada
      map_obj <- leaflet() %>% addTiles() %>%
        addControl("Data tidak tersedia untuk variabel ini.", position = "topright")
      rv$peta_interaktif <- map_obj # Simpan peta kosong ke rv
      return() # Hentikan proses lebih lanjut
    }
    
    # ----- PINDAHKAN SEMUA LOGIKA PEMBUATAN PETA KE SINI -----
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = map_data[[selected_var_with_suffix]],
      na.color = "#bdbdbd"
    )
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      map_data$nmkab, 
      input$map_var,
      format(map_data[[selected_var_with_suffix]], big.mark = ",", decimal.mark = ".", nsmall = 2)
    ) %>% lapply(htmltools::HTML)
    
    map_obj <- leaflet(data = map_data) %>%
      addTiles() %>%
      setView(lng = 118, lat = -2, zoom = 5) %>%
      addPolygons(
        fillColor = ~pal(get(selected_var_with_suffix)),
        weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
      ) %>%
      addLegend(pal = pal, values = ~get(selected_var_with_suffix), opacity = 0.7, title = input$map_var, position = "bottomright")
    
    # SIMPAN OBJEK PETA YANG SUDAH JADI KE rv
    rv$peta_interaktif <- map_obj
  })
  
  # C. Render HANYA MENAMPILKAN peta dari rv
  output$interactive_map <- renderLeaflet({
    req(rv$peta_interaktif)
    rv$peta_interaktif
  })
  
  # D. Download handler sekarang akan bekerja dengan benar
  output$downloadMap <- downloadHandler(
    filename = function() { paste0("Peta_Sebaran_", input$map_var, ".png") },
    content = function(file) {
      req(rv$peta_interaktif)
      mapview::mapshot(rv$peta_interaktif, file = file, webshot = "webshot2")
    }
  )
  
  # E. Logika Interpretasi Peta (Ini sudah benar, tidak perlu diubah)
  output$map_interpretation <- renderUI({
    # Pastikan variabel peta sudah dipilih
    req(input$map_var)
    
    # Kita gunakan kembali data reaktif yang sudah digabung untuk peta
    map_data <- map_data_reactive()
    
    # Ambil nama variabel asli (contoh: "CHILDREN")
    var_name_original <- input$map_var
    
    # Ambil nama kolom data yang benar (dengan akhiran .y, contoh: "CHILDREN.y")
    var_name_data <- paste0(var_name_original, ".y")
    
    # Lakukan pengecekan untuk menghindari error jika kolom tidak ada
    if (!var_name_data %in% names(map_data)) {
      return(p("Silakan pilih variabel untuk melihat analisis."))
    }
    
    # Ubah ke data frame biasa dan hapus baris NA untuk analisis
    analysis_data <- map_data %>% 
      as.data.frame() %>% # Hapus geometri untuk analisis lebih cepat
      filter(!is.na(.data[[var_name_data]]))
    
    # Jika tidak ada data tersisa setelah filter
    if (nrow(analysis_data) == 0) {
      return(p("Tidak ada data yang tersedia untuk dianalisis pada variabel ini."))
    }
    
    # --- Lakukan Analisis Sederhana ---
    max_value <- max(analysis_data[[var_name_data]])
    min_value <- min(analysis_data[[var_name_data]])
    avg_value <- mean(analysis_data[[var_name_data]])
    
    # Cari nama wilayah dengan nilai tertinggi dan terendah
    region_max <- analysis_data$nmkab[which.max(analysis_data[[var_name_data]])]
    region_min <- analysis_data$nmkab[which.min(analysis_data[[var_name_data]])]
    
    # --- Buat Teks Interpretasi dengan HTML ---
    HTML(paste0(
      "<p>Peta ini menunjukkan sebaran spasial untuk variabel <b>", var_name_original, "</b> di seluruh kabupaten/kota.</p>",
      "<ul>",
      "<li>Nilai <b>tertinggi</b> tercatat di <b>", region_max, "</b> dengan nilai sebesar ", round(max_value, 2), ".</li>",
      "<li>Nilai <b>terendah</b> tercatat di <b>", region_min, "</b> dengan nilai sebesar ", round(min_value, 2), ".</li>",
      "<li>Rata-rata nilai untuk variabel ini di semua wilayah adalah <b>", round(avg_value, 2), "</b>.</li>",
      "</ul>",
      "<p>Dengan mengamati warna pada peta, Anda dapat mengidentifikasi pola pengelompokan (klaster) wilayah dengan nilai tinggi (warna lebih gelap) dan wilayah dengan nilai rendah (warna lebih terang).</p>"
    ))
  })
  
  # --- LOGIKA UNDUH HALAMAN EKSPLORASI DATA ----
  # Langkah 1: Tampilkan Jendela Pop-up (Modal) saat tombol di UI diklik
  #    (Pastikan Anda sudah menambahkan actionButton("tampil_modal_unduh",...) di UI Anda)
  observeEvent(input$tampil_modal_unduh, {
    showModal(modalDialog(
      title = "Konfirmasi Unduhan Laporan",
      "Anda akan mengunduh sebuah file ZIP yang berisi:",
      tags$ul(
        tags$li(tags$b("Ringkasan_Statistik.docx")),
        tags$li(tags$b("Grafik_Eksplorasi.png")),
        tags$li(tags$b("Peta_Sebaran.png")),
        tags$li(tags$b("Laporan_Interpretasi.docx"))
      ),
      footer = tagList(
        modalButton("Batal"),
        downloadButton("unduh_zip_eksplorasi", "Ya, Lanjutkan Unduh")
      ),
      easyClose = TRUE
    ))
  })
  
  
  # Langkah 2: Handler untuk membuat dan mengirim file ZIP
  # Ganti lagi keseluruhan blok output$unduh_zip_eksplorasi Anda dengan ini:
  output$unduh_zip_eksplorasi <- downloadHandler(
    filename = function() {
      paste0("Laporan-Eksplorasi-Data-", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Buat direktori sementara
      temp_dir <- file.path(tempdir(), "eksplorasi_report")
      if (!dir.exists(temp_dir)) dir.create(temp_dir)
      
      # --- A. SIMPAN RINGKASAN STATISTIK (.docx) ---
      # <<< BAGIAN INI DIPERBAIKI >>>
      req(rv$summary_deskriptif)
      
      # 1. Simpan data summary ke file .rds agar bisa dibaca oleh R Markdown
      summary_rds_path <- file.path(temp_dir, "summary_data.rds")
      saveRDS(rv$summary_deskriptif, file = summary_rds_path)
      
      # 2. Buat konten untuk file R Markdown (variabel yang sebelumnya hilang)
      konten_rmd_summary <- paste(
        "---",
        "title: 'Ringkasan Statistik Deskriptif'",
        "output: word_document",
        "---",
        "",
        "```{r, echo=FALSE, message=FALSE, warning=FALSE}",
        "library(knitr)",
        "summary_df <- readRDS('summary_data.rds')",
        "kable(summary_df, caption = 'Tabel Statistik Deskriptif')",
        "```",
        sep = "\n"
      )
      
      # 3. Tulis konten ke file .Rmd
      rmd_path_summary <- file.path(temp_dir, "summary_report.Rmd")
      writeLines(konten_rmd_summary, rmd_path_summary)
      
      # 4. Render file .Rmd yang sekarang sudah ada
      rmarkdown::render(
        input = rmd_path_summary,
        output_file = "Ringkasan_Statistik.docx",
        output_dir = temp_dir,
        quiet = TRUE
      )
      
      # --- B. SIMPAN GRAFIK (.png) ---
      req(rv$plot_visualisasi)
      ggsave(
        filename = file.path(temp_dir, "Grafik_Eksplorasi.png"),
        plot = rv$plot_visualisasi, device = "png", width = 8, height = 6
      )
      
      # --- C. SIMPAN PETA (.png) ---
      req(rv$peta_interaktif)
      mapview::mapshot(
        rv$peta_interaktif,
        file = file.path(temp_dir, "Peta_Sebaran.png"),
        delay = 2
      )
      
      # --- D. BUAT LAPORAN INTERPRETASI GABUNGAN (.docx) ---
      # D.1. Ambil teks interpretasi deskriptif
      req(desc_data())
      interpretasi_desc <- paste(lapply(names(desc_data()), function(var_name) {
        col_data <- desc_data()[[var_name]]
        if (is.numeric(col_data)) {
          stats <- summary(col_data)
          mean_val <- round(mean(col_data, na.rm = TRUE), 2)
          median_val <- round(median(col_data, na.rm = TRUE), 2)
          paste0("- Untuk variabel **", var_name, "**: Nilai berkisar dari ", round(stats[["Min."]], 2), " hingga ", round(stats[["Max."]], 2), ". Rata-rata (mean) adalah **", mean_val, "** dan nilai tengah (median) adalah **", median_val, "**.")
        } else {
          paste0("- Variabel **", var_name, "** adalah variabel kategorikal.")
        }
      }), collapse = "\n")
      
      # D.2. Ambil teks interpretasi plot
      req(rv$data)
      interpretasi_plot <- if (input$plot_type == "Histogram") {
        paste0("Grafik Histogram ini menunjukkan sebaran data untuk variabel **", input$hist_box_var, "**. Puncak tertinggi menunjukkan nilai yang paling sering muncul, sedangkan lebar histogram memberikan gambaran tentang sebaran data secara keseluruhan.")
      } else if (input$plot_type == "Box Plot") {
        paste0("Grafik Box Plot ini menggambarkan sebaran data untuk variabel **", input$hist_box_var, "**. Garis tebal di tengah kotak adalah **median**, dan kotak itu sendiri menunjukkan **rentang interkuartil (IQR)**, tempat 50% data terpusat.")
      } else if (input$plot_type == "Scatter Plot") {
        req(input$scatter_x, input$scatter_y)
        corr_val <- tryCatch({ round(cor(rv$data[[input$scatter_x]], rv$data[[input$scatter_y]], use = "complete.obs"), 3) }, error = function(e) { NA })
        strength <- if (is.na(corr_val)) {"tidak dapat dihitung"} else {dplyr::case_when(abs(corr_val) >= 0.7 ~ "kuat", abs(corr_val) >= 0.4 ~ "sedang", TRUE ~ "lemah")}
        paste0("Grafik ini menunjukkan hubungan antara variabel **", input$scatter_x, "** dan **", input$scatter_y, "**. Garis regresi menunjukkan tren linear secara umum. Nilai korelasi antar kedua variabel adalah **", corr_val, "**, menandakan hubungan yang **", strength, "**.")
      } else { "" }
      
      # D.3. Ambil teks interpretasi peta
      req(map_data_reactive(), input$map_var)
      analysis_data_map <- map_data_reactive() %>% as.data.frame() %>% filter(!is.na(.data[[paste0(input$map_var, ".y")]]))
      max_value <- max(analysis_data_map[[paste0(input$map_var, ".y")]])
      min_value <- min(analysis_data_map[[paste0(input$map_var, ".y")]])
      avg_value <- mean(analysis_data_map[[paste0(input$map_var, ".y")]])
      region_max <- analysis_data_map$nmkab[which.max(analysis_data_map[[paste0(input$map_var, ".y")]])]
      region_min <- analysis_data_map$nmkab[which.min(analysis_data_map[[paste0(input$map_var, ".y")]])]
      interpretasi_peta <- paste0(
        "Peta ini menunjukkan sebaran spasial untuk variabel **", input$map_var, "** di seluruh kabupaten/kota.\n",
        "- Nilai **tertinggi** tercatat di **", region_max, "** dengan nilai sebesar ", round(max_value, 2), ".\n",
        "- Nilai **terendah** tercatat di **", region_min, "** dengan nilai sebesar ", round(min_value, 2), ".\n",
        "- Rata-rata nilai untuk variabel ini di semua wilayah adalah **", round(avg_value, 2), "**."
      )
      
      # D.4. Gabungkan semua teks dan buat file .Rmd
      konten_rmd_laporan <- paste("---", "title: 'Laporan Eksplorasi Data Lengkap'", "output: word_document", "---", "", "## Ringkasan Statistik", interpretasi_desc, "", "## Interpretasi Grafik", interpretasi_plot, "", "## Interpretasi Peta", interpretasi_peta, sep = "\n")
      rmd_path_laporan <- file.path(temp_dir, "laporan_lengkap.Rmd")
      writeLines(konten_rmd_laporan, rmd_path_laporan)
      
      # D.5. Render .Rmd menjadi .docx
      rmarkdown::render(
        input = rmd_path_laporan,
        output_file = "Laporan_Interpretasi.docx",
        output_dir = temp_dir,
        quiet = TRUE
      )
      
      # --- E. ZIP SEMUA FILE ---
      old_wd <- getwd()
      setwd(temp_dir)
      on.exit(setwd(old_wd))
      files_to_zip <- c(
        "Ringkasan_Statistik.docx",
        "Grafik_Eksplorasi.png",
        "Peta_Sebaran.png",
        "Laporan_Interpretasi.docx"
      )
      zip(zipfile = file, files = files_to_zip)
    }
  )
  
  
  # --- LOGIKA UJI ASUMSI ---
  # 1. Uji Normalitas
  norm_results <- eventReactive(input$run_norm_test, {
    req(rv$data, input$norm_var)
    var_data <- rv$data[[input$norm_var]]
    test_result <- NULL
    if (input$norm_test_method == "sw") {
      test_result <- shapiro.test(na.omit(var_data))
    } else {
      var_data_clean <- na.omit(var_data)
      test_result <- ks.test(var_data_clean, "pnorm", mean(var_data_clean), sd(var_data_clean))
    }
    return(list(test = test_result, data = var_data))
  })
  
  # Reaktif khusus untuk objek plot Q-Q
  norm_plot_object <- reactive({
    req(norm_results())
    res <- norm_results()
    p <- ggplot(data.frame(sample = na.omit(res$data)), aes(sample = sample)) +
      stat_qq(color = "dodgerblue") + stat_qq_line(color = "red2", linetype = "dashed") +
      labs(title = paste("Q-Q Plot untuk", input$norm_var), x = "Theoretical Quantiles", y = "Sample Quantiles") + 
      theme_minimal(base_size = 14)
    return(p)
  })
  
  # Render Plot dari Objek Reaktif
  output$norm_plot <- renderPlot({
    norm_plot_object()
  })
  
  # Logika untuk tombol download plot normalitas
  output$download_norm_plot <- downloadHandler(
    filename = function() {
      paste0("QQ-Plot-", input$norm_var, ".png")
    },
    content = function(file) {
      # Menggunakan ggsave untuk menyimpan objek plot
      ggsave(file, plot = norm_plot_object(), device = "png", width = 8, height = 6, dpi = 300)
    }
  )
  
  output$norm_test_output <- renderPrint({ norm_results()$test })
  
  output$norm_interpretation <- renderUI({
    res <- norm_results()$test; alpha <- as.numeric(input$norm_alpha); p_value <- res$p.value
    keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀")
    kesimpulan <- ifelse(p_value < alpha, "data tidak berdistribusi normal secara signifikan.", "tidak ada cukup bukti untuk menyatakan data tidak berdistribusi normal.")
    HTML(paste0("<h4>Langkah-langkah Interpretasi:</h4>","<p><b>1. Hipotesis:</b></p>","<ul>","<li><b>H₀:</b> Data berasal dari populasi yang berdistribusi normal.</li>","<li><b>H₁:</b> Data tidak berasal dari populasi yang berdistribusi normal.</li>","</ul>","<p><b>2. Tingkat Signifikansi (α):</b> ", alpha, "</p>","<p><b>3. Aturan Keputusan:</b> Jika p-value < α, maka H₀ ditolak.</p>","<hr>","<h4>Hasil:</h4>","<p>Berdasarkan uji <b>", res$method, "</b>, diperoleh nilai p-value sebesar <b>", round(p_value, 4), "</b>.</p>","<p>Karena p-value (", round(p_value, 4), ") ", ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), maka keputusannya adalah <b>", keputusan, "</b>.</p>","<h4>Kesimpulan:</h4>","<p>Pada tingkat signifikansi ", alpha * 100, "%, dapat disimpulkan bahwa <b>", kesimpulan, "</b></p>"))
  })
  
  # 2. Uji Homogenitas
  homog_results <- eventReactive(input$run_homog_test, {
    req(rv$data, input$homog_var_num, input$homog_var_cat, input$homog_var_cat != "")
    
    df <- rv$data
    formula <- as.formula(paste(input$homog_var_num, "~", input$homog_var_cat))
    df[[input$homog_var_cat]] <- as.factor(df[[input$homog_var_cat]])
    
    test_result <- NULL
    if (input$homog_test_method == "levene") {
      test_result <- leveneTest(formula, data = df)
    } else {
      test_result <- bartlett.test(formula, data = df)
    }
    return(list(test = test_result, data = df, formula = formula))
  })
  
  # BARU: Reaktif khusus untuk objek box plot
  homog_plot_object <- reactive({
    req(homog_results())
    res <- homog_results()
    p <- ggplot(res$data, aes_string(x = input$homog_var_cat, y = input$homog_var_num, fill = input$homog_var_cat)) +
      geom_boxplot() + 
      theme_minimal(base_size = 14) +
      labs(title = paste("Box Plot", input$homog_var_num, "berdasarkan", input$homog_var_cat), x = input$homog_var_cat, y = input$homog_var_num) +
      theme(legend.position = "none")
    return(p)
  })
  
  # MODIFIKASI: Render plot dari objek reaktif
  output$homog_plot <- renderPlot({
    homog_plot_object()
  })
  
  # Logika untuk tombol download plot homogenitas
  output$download_homog_plot <- downloadHandler(
    filename = function() {
      paste0("BoxPlot-Homogenitas-", input$homog_var_num, ".png")
    },
    content = function(file) {
      ggsave(file, plot = homog_plot_object(), device = "png", width = 8, height = 6, dpi = 300)
    }
  )
  
  output$homog_test_output <- renderPrint({ res <- homog_results(); if (!is.null(res$error)) { cat(res$error) } else { res$test } })
  
  output$homog_interpretation <- renderUI({
    res <- homog_results()$test; alpha <- as.numeric(input$homog_alpha)
    p_value <- if(input$homog_test_method == "levene") res$`Pr(>F)`[1] else res$p.value
    if (is.na(p_value) || is.null(p_value)) return(p("Hasil tidak tersedia. Pastikan variabel yang dipilih sesuai."))
    
    keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀")
    kesimpulan <- ifelse(p_value < alpha, "terdapat perbedaan varians yang signifikan antar kelompok (varians tidak homogen).", "tidak ada cukup bukti untuk menyatakan varians berbeda antar kelompok (asumsi homogenitas varians terpenuhi).")
    HTML(paste0("<h4>Langkah-langkah Interpretasi:</h4>","<p><b>1. Hipotesis:</b></p>","<ul>","<li><b>H₀:</b> Varians bersifat homogen (sama) di semua kelompok.</li>","<li><b>H₁:</b> Setidaknya ada satu kelompok dengan varians yang berbeda.</li>","</ul>","<p><b>2. Tingkat Signifikansi (α):</b> ", alpha, "</p>","<p><b>3. Aturan Keputusan:</b> Jika p-value < α, maka H₀ ditolak.</p>","<hr>","<h4>Hasil:</h4>","<p>Berdasarkan uji <b>", ifelse(input$homog_test_method == 'levene', "Levene's Test", "Bartlett's Test"), "</b>, diperoleh nilai p-value sebesar <b>", round(p_value, 4), "</b>.</p>","<p>Karena p-value (", round(p_value, 4), ") ", ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), maka keputusannya adalah <b>", keputusan, "</b>.</p>","<h4>Kesimpulan:</h4>","<p>Pada tingkat signifikansi ", alpha * 100, "%, dapat disimpulkan bahwa <b>", kesimpulan, "</b></p>"))
  })
  
  # 3. Uji Autokorelasi Spasial
  observeEvent(rv$data, {
    # Menggabungkan data spasial (peta) dengan data tabular (sovi)
    req(map_data, rv$data) 
    
    peta_lokal <- map_data
    sovi_df_map <- rv$data
    
    # Membersihkan kunci join
    sovi_df_map$DISTRICTCODE_clean <- sub("\\..*", "", trimws(sovi_df_map$DISTRICTCODE))
    peta_lokal$kodeprkab_clean <- trimws(as.character(peta_lokal$kodeprkab))
    
    # Melakukan penggabungan
    data_gabungan_raw <- merge(peta_lokal, sovi_df_map, 
                               by.x = "kodeprkab_clean", 
                               by.y = "DISTRICTCODE_clean", 
                               all.x = TRUE)
    
    # =========================================================================
    # BAGIAN PENTING: Membersihkan dan merapikan data setelah merge
    # =========================================================================
    data_gabungan_clean <- data_gabungan_raw %>%
      # 1. Pilih kolom penting dari peta (.x) dan semua kolom dari data csv (.y)
      select(
        nmkab,                           # Nama kabupaten dari peta
        geometry,                        # Geometri dari peta
        ends_with(".y")                  # Semua kolom yang berasal dari sovi_data.csv
      ) %>%
      # 2. Ganti nama kolom dengan menghapus akhiran ".y"
      rename_with(~ sub("\\.y$", "", .), .cols = ends_with(".y"))
    # =========================================================================
    
    # Simpan hasil gabungan yang SUDAH BERSIH ke reactiveValues
    rv$data_gabungan <- sf::st_as_sf(data_gabungan_clean)
    
    # Beri notifikasi jika berhasil
    showNotification("Data Peta dan Indikator SOVI berhasil digabungkan dan dibersihkan.", type = "message")
  }, once = TRUE)
  
  # --- LOGIKA UJI MORAN'S I ---
  moran_results <- eventReactive(input$run_moran_test, {
    req(rv$data_gabungan, input$moran_var, input$weights_method)
    data_sf <- rv$data_gabungan
    showNotification("Membuat matriks pembobot spasial...", type = "message", duration = 3)
    listw_obj <- NULL
    
    tryCatch({
      # --- Logika untuk Metode QUEEN ---
      if (input$weights_method == "queen") {
        neighbors <- poly2nb(data_sf, queen = TRUE)
        listw_obj <- nb2listw(neighbors, style = "W", zero.policy = TRUE)
        
        # --- Logika HANYA untuk Metode JARAK INVERSI ---
      } else if (input$weights_method == "inv_dist") {
        req(distance_data)
        
        # Karena 'distance_clean.csv' sudah bersih, kita bisa langsung mengubahnya ke matriks
        dist_mat <- as.matrix(distance_data)
        
        # Pemeriksaan dimensi harus ada DI DALAM blok ini
        if(nrow(dist_mat) != nrow(data_sf)) {
          showNotification("Matriks jarak tidak sesuai dengan data peta.", type = "error")
          return(NULL)
        }
        
        # Hitung invers jarak
        dist_inv <- 1 / dist_mat
        
        # Cukup bersihkan nilai Inf (dari 1/0 di diagonal) dan NA/NaN
        dist_inv[!is.finite(dist_inv)] <- 0
        
        # Buat objek pembobot
        listw_obj <- mat2listw(dist_inv, style = "W", zero.policy = TRUE)
      }
      # --- TIDAK ADA KODE LAIN DI ANTARA BAGIAN INI DAN tryCatch ---
      
    }, error = function(e) {
      showNotification(paste("Error saat membuat pembobot:", e$message), type = "error")
      return(NULL)
    })
    
    req(listw_obj)
    
    # Sisa kode di bawah ini sama untuk kedua metode
    variable_to_test <- data_sf[[input$moran_var]]
    test_result <- moran.test(variable_to_test, listw = listw_obj, zero.policy = TRUE, na.action = na.exclude)
    
    plot_df <- data.frame(
      var = variable_to_test,
      lag_var = lag.listw(listw_obj, variable_to_test, zero.policy = TRUE, na.action = na.exclude)
    ) %>% na.omit()
    
    plot_obj <- ggplot(plot_df, aes(x = var, y = lag_var)) +
      geom_point(alpha = 0.6, color = "dodgerblue") +
      geom_smooth(method = "lm", se = FALSE, color = "tomato") +
      geom_hline(yintercept = mean(plot_df$lag_var), lty = "dashed") +
      geom_vline(xintercept = mean(plot_df$var), lty = "dashed") +
      labs(
        title = paste("Moran Scatter Plot untuk", input$moran_var),
        x = "Nilai Variabel",
        y = "Rata-rata Nilai Tetangga (Lag Spasial)"
      ) + theme_minimal(base_size = 14)
    
    return(list(test = test_result, plot = plot_obj))
  })
  
  # Render output statistik
  output$moran_test_output <- renderPrint({
    req(moran_results())
    moran_results()$test
  })
  
  # Render plot
  output$moran_plot_output <- renderPlot({
    req(moran_results()$plot)
    moran_results()$plot # Langsung panggil objek plot
  })
  
  # Handler untuk tombol unduh plot Moran's I
  output$download_moran_plot <- downloadHandler(
    filename = function() {
      paste0("Moran-Scatter-Plot-", input$moran_var, ".png")
    },
    content = function(file) {
      req(moran_results()$plot)
      ggsave(file, plot = moran_results()$plot, device = "png", width = 8, height = 6, dpi = 300)
    }
  )
  
  # Render interpretasi (Tidak ada perubahan di sini, sudah benar)
  output$moran_interpretation <- renderUI({
    req(moran_results())
    res <- moran_results()$test
    alpha <- as.numeric(input$moran_alpha)
    p_value <- res$p.value
    moran_I <- res$estimate[1]
    
    keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀")
    
    pola_spasial <- if(p_value < alpha) {
      if(moran_I > 0) "terdapat autokorelasi spasial positif (pola mengelompok/clustered)."
      else "terdapat autokorelasi spasial negatif (pola menyebar/dispersed)."
    } else {
      "tidak ada cukup bukti untuk menyatakan adanya autokorelasi spasial (pola acak/random)."
    }
    
    HTML(paste0(
      "<h4>Langkah-langkah Interpretasi:</h4>",
      "<p><b>1. Hipotesis:</b></p>",
      "<ul>",
      "<li><b>H₀:</b> Tidak ada autokorelasi spasial (nilai-nilai tersebar secara acak).</li>",
      "<li><b>H₁:</b> Terdapat autokorelasi spasial (nilai-nilai tidak tersebar acak).</li>",
      "</ul>",
      "<p><b>2. Tingkat Signifikansi (α):</b> ", alpha, "</p>",
      "<hr>",
      "<h4>Hasil:</h4>",
      "<p>Berdasarkan uji, diperoleh statistik <b>Moran's I = ", round(moran_I, 4), "</b> dengan <b>p-value = ", format.pval(p_value, digits = 4), "</b>.</p>",
      "<p>Karena p-value (", format.pval(p_value, digits = 4), ") ", ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), maka keputusannya adalah <b>", keputusan, "</b>.</p>",
      "<h4>Kesimpulan:</h4>",
      "<p>Pada tingkat signifikansi ", alpha * 100, "%, dapat disimpulkan bahwa <b>", pola_spasial, "</b></p>"
    ))
  })
  
  # --- LOGIKA UNTUK UNDUH HALAMAN UJI ASUMSI ---
  
  # 1. Tampilkan Pop-up (Modal) Konfirmasi
  observeEvent(input$download_asumsi_page, {
    # Cek apakah SEMUA uji sudah dijalankan
    if (is.null(norm_results()) || is.null(homog_results()) || is.null(moran_results())) {
      showModal(modalDialog(
        title = "Peringatan 🚨",
        "Harap jalankan Uji Normalitas, Uji Homogenitas, DAN Uji Autokorelasi Spasial terlebih dahulu sebelum mengunduh halaman.",
        easyClose = TRUE,
        footer = modalButton("Tutup")
      ))
    } else {
      # Jika sudah, tampilkan modal konfirmasi yang sudah diperbarui
      showModal(modalDialog(
        title = "Konfirmasi Unduhan Halaman",
        p("Anda akan mengunduh sebuah file ZIP yang berisi:"),
        tags$ul(
          tags$li(strong("plot_normalitas.png:"), " Grafik Q-Q Plot."),
          tags$li(strong("plot_homogenitas.png:"), " Grafik Box Plot."),
          tags$li(strong("plot_autokorelasi.png:"), " Grafik Moran Scatter Plot."), # <-- item baru
          tags$li(strong("Laporan_Interpretasi.docx:"), " Dokumen Word berisi interpretasi lengkap ketiga uji.")
        ),
        footer = tagList(
          modalButton("Batal"),
          downloadButton("confirm_download_asumsi", "Ya, Lanjutkan Unduh")
        ),
        easyClose = TRUE
      ))
    }
  })
  
  # 2. Handler untuk Membuat dan Mengunduh File ZIP
  # Ganti keseluruhan blok downloadHandler ini
  output$confirm_download_asumsi <- downloadHandler(
    filename = function() {
      paste0("Hasil_Uji_Asumsi_", Sys.Date(), ".zip")
    },
    content = function(file) {
      showNotification("Sedang menyiapkan laporan... Mohon tunggu.", duration = 10, type = "message")
      
      temp_dir <- file.path(tempdir(), "asumsi_report")
      if (!dir.exists(temp_dir)) dir.create(temp_dir)
      
      # --- A. Simpan SEMUA Grafik ---
      norm_plot_path <- file.path(temp_dir, "plot_normalitas.png")
      ggsave(norm_plot_path, plot = norm_plot_object(), device = "png", width = 8, height = 6, dpi = 300)
      
      homog_plot_path <- file.path(temp_dir, "plot_homogenitas.png")
      ggsave(homog_plot_path, plot = homog_plot_object(), device = "png", width = 8, height = 6, dpi = 300)
      
      moran_plot_path <- file.path(temp_dir, "plot_autokorelasi.png")
      ggsave(moran_plot_path, plot = moran_results()$plot, device = "png", width = 8, height = 6, dpi = 300)
      
      # --- B. Buat Dokumen Word dengan Interpretasi Lengkap ---
      
      # B.1. Ambil semua nilai yang dibutuhkan
      norm_res <- norm_results()$test; alpha_norm <- as.numeric(input$norm_alpha)
      p_norm <- norm_res$p.value
      keputusan_norm <- ifelse(p_norm < alpha_norm, "TOLAK H₀", "GAGAL TOLAK H₀")
      kesimpulan_norm <- ifelse(p_norm < alpha_norm, "data tidak berdistribusi normal secara signifikan.", "tidak ada cukup bukti untuk menyatakan data tidak berdistribusi normal.")
      
      homog_res <- homog_results()$test; alpha_homog <- as.numeric(input$homog_alpha)
      p_homog <- if(input$homog_test_method == "levene") homog_res$`Pr(>F)`[1] else homog_res$p.value
      keputusan_homog <- ifelse(p_homog < alpha_homog, "TOLAK H₀", "GAGAL TOLAK H₀")
      kesimpulan_homog <- ifelse(p_homog < alpha_homog, "terdapat perbedaan varians yang signifikan antar kelompok (varians tidak homogen).", "tidak ada cukup bukti untuk menyatakan varians berbeda antar kelompok (asumsi homogenitas varians terpenuhi).")
      
      moran_res <- moran_results()$test; alpha_moran <- as.numeric(input$moran_alpha)
      p_moran <- moran_res$p.value
      moran_I <- moran_res$estimate[1]
      keputusan_moran <- ifelse(p_moran < alpha_moran, "TOLAK H₀", "GAGAL TOLAK H₀")
      pola_spasial <- if(p_moran < alpha_moran) { if(moran_I > 0) "terdapat autokorelasi spasial positif (pola mengelompok)." else "terdapat autokorelasi spasial negatif (pola menyebar)." } else { "pola spasial acak (random)." }
      
      # =========================================================================
      # PERUBAHAN UTAMA: Membuat konten R Markdown dengan format lengkap
      # =========================================================================
      konten_rmd <- paste(
        "---", "title: 'Laporan Hasil Uji Asumsi'", "output: word_document", "---", "",
        "## 1. Uji Normalitas", "",
        "#### Hipotesis",
        "* **H₀**: Data berasal dari populasi yang berdistribusi normal.",
        "* **H₁**: Data tidak berasal dari populasi yang berdistribusi normal.", "",
        paste0("**Tingkat Signifikansi (α):** ", alpha_norm), "",
        "#### Hasil",
        paste0("Berdasarkan uji **", norm_res$method, "**, diperoleh nilai p-value sebesar **", round(p_norm, 4), "**. ",
               "Karena p-value (", round(p_norm, 4), ") ", ifelse(p_norm < alpha_norm, "<", "≥"), " α (", alpha_norm, "), maka keputusannya adalah **", keputusan_norm, "**."), "",
        "#### Kesimpulan",
        paste0("Pada tingkat signifikansi ", alpha_norm * 100, "%, dapat disimpulkan bahwa **", kesimpulan_norm, "**."),
        "", "***", "",
        "## 2. Uji Homogenitas Varians", "",
        "#### Hipotesis",
        "* **H₀**: Varians bersifat homogen (sama) di semua kelompok.",
        "* **H₁**: Setidaknya ada satu kelompok dengan varians yang berbeda.", "",
        paste0("**Tingkat Signifikansi (α):** ", alpha_homog), "",
        "#### Hasil",
        paste0("Berdasarkan uji **", ifelse(input$homog_test_method == 'levene', "Levene's Test", "Bartlett's Test"), "**, diperoleh nilai p-value sebesar **", round(p_homog, 4), "**. ",
               "Karena p-value (", round(p_homog, 4), ") ", ifelse(p_homog < alpha_homog, "<", "≥"), " α (", alpha_homog, "), maka keputusannya adalah **", keputusan_homog, "**."), "",
        "#### Kesimpulan",
        paste0("Pada tingkat signifikansi ", alpha_homog * 100, "%, dapat disimpulkan bahwa **", kesimpulan_homog, "**."),
        "", "***", "",
        "## 3. Uji Autokorelasi Spasial (Moran's I)", "",
        "#### Hipotesis",
        "* **H₀**: Tidak ada autokorelasi spasial (pola acak).",
        "* **H₁**: Terdapat autokorelasi spasial (pola tidak acak).", "",
        paste0("**Tingkat Signifikansi (α):** ", alpha_moran), "",
        "#### Hasil",
        paste0("Diperoleh statistik **Moran's I = ", round(moran_I, 4), "** dengan **p-value = ", format.pval(p_moran, digits = 4), "**. ",
               "Karena p-value (", format.pval(p_moran, digits = 4), ") ", ifelse(p_moran < alpha_moran, "<", "≥"), " α (", alpha_moran, "), maka keputusannya adalah **", keputusan_moran, "**."), "",
        "#### Kesimpulan",
        paste0("Pada tingkat signifikansi ", alpha_moran * 100, "%, disimpulkan bahwa **", pola_spasial, "**."),
        sep = "\n"
      )
      
      rmd_path <- file.path(temp_dir, "Laporan_Asumsi.Rmd")
      writeLines(konten_rmd, rmd_path)
      
      rmarkdown::render(input = rmd_path, output_file = "Laporan_Interpretasi.docx", output_dir = temp_dir, quiet = TRUE)
      
      # --- C. Buat File ZIP ---
      doc_path <- file.path(temp_dir, "Laporan_Interpretasi.docx")
      zip::zipr(
        zipfile = file, 
        files = c(
          "plot_normalitas.png" = norm_plot_path,
          "plot_homogenitas.png" = homog_plot_path,
          "plot_autokorelasi.png" = moran_plot_path,
          "Laporan_Interpretasi.docx" = doc_path
        )
      )
    },
    contentType = "application/zip"
  )
  
  # --- LOGIKA STATISTIK INFERENSIA ---
  
  # -- UJI RATA-RATA ---
  t_test_one_results <- eventReactive(input$run_t_test_one, {
    req(input$t_test_one_var, !is.na(input$t_test_one_mu))
    var_data <- rv$data[[input$t_test_one_var]] %>% na.omit()
    if(length(var_data) < 2) return(list(error = "Data tidak cukup untuk uji-t."))
    test_result <- t.test(var_data, mu = input$t_test_one_mu, alternative = input$t_test_one_alternative)
    return(list(test = test_result))
  })
  output$t_test_one_output <- renderPrint({ res <- t_test_one_results(); if(!is.null(res$error)) cat(res$error) else res$test })
  output$t_test_one_interpretation <- renderUI({
    res_list <- t_test_one_results(); req(is.null(res_list$error)); res <- res_list$test
    alpha <- as.numeric(input$t_test_one_alpha); p_value <- res$p.value; mu0 <- input$t_test_one_mu
    h1_symbol <- switch(input$t_test_one_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<")
    keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀")
    kesimpulan <- ifelse(p_value < alpha, paste0("terdapat cukup bukti bahwa rata-rata populasi secara signifikan ", ifelse(h1_symbol == ">", "lebih besar dari ", ifelse(h1_symbol == "<", "lebih kecil dari ", "berbeda dari ")), mu0, "."), paste0("tidak ada cukup bukti bahwa rata-rata populasi berbeda dari ", mu0, "."))
    HTML(paste0("<h4>Langkah Interpretasi:</h4>","<p><b>1. Hipotesis:</b></p>","<ul>","<li><b>H₀:</b> μ = ", mu0, "</li>","<li><b>H₁:</b> μ ", h1_symbol, " ", mu0, "</li>","</ul>","<p><b>2. Tingkat Signifikansi (α):</b> ", alpha, "</p>","<p><b>3. Aturan Keputusan:</b> Jika p-value < α, maka H₀ ditolak.</p>","<hr>","<h4>Hasil:</h4>","<p>Berdasarkan <b>", res$method, "</b>, t = <b>", round(res$statistic, 3), "</b> dengan p-value = <b>", format.pval(p_value, digits = 4), "</b>.</p>","<p>Rata-rata sampel adalah <b>", round(res$estimate, 3), "</b>.</p>","<p>Karena p-value (", format.pval(p_value, digits = 4), ") ", ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), keputusannya <b>", keputusan, "</b>.</p>","<h4>Kesimpulan:</h4>","<p>Pada α = ", alpha * 100, "%, ", kesimpulan, "</p>"))
  })
  
  t_test_two_results <- eventReactive(input$run_t_test_two, {
    req(input$t_test_two_num_var, input$t_test_two_cat_var)
    if (input$t_test_two_cat_var == "") return(list(error = "Gagal: Pilih 'Variabel Grup'."))
    df_clean <- rv$data %>% select(num_var = !!sym(input$t_test_two_num_var), cat_var = !!sym(input$t_test_two_cat_var)) %>% na.omit()
    df_clean$cat_var <- as.factor(df_clean$cat_var)
    if (nlevels(df_clean$cat_var) != 2) return(list(error = "Gagal: Variabel grup harus punya 2 kategori."))
    formula <- as.formula("num_var ~ cat_var")
    test_result <- t.test(formula, data = df_clean, alternative = input$t_test_two_alternative)
    return(list(test = test_result, levels = levels(df_clean$cat_var)))
  })
  output$t_test_two_output <- renderPrint({ res <- t_test_two_results(); if(!is.null(res$error)) cat(res$error) else res$test })
  output$t_test_two_interpretation <- renderUI({
    res_list <- t_test_two_results(); req(is.null(res_list$error)); res <- res_list$test
    alpha <- as.numeric(input$t_test_two_alpha); p_value <- res$p.value; levels <- res_list$levels
    h1_symbol <- switch(input$t_test_two_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<")
    keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀")
    kesimpulan <- ifelse(p_value < alpha, paste0("terdapat perbedaan rata-rata yang signifikan antara grup '", levels[1], "' dan '", levels[2], "'."), paste0("tidak ada perbedaan rata-rata yang signifikan antara grup '", levels[1], "' dan '", levels[2], "'."))
    HTML(paste0("<h4>Langkah Interpretasi:</h4>","<p><b>1. Hipotesis:</b></p>","<ul>","<li><b>H₀:</b> μ₁ = μ₂</li>","<li><b>H₁:</b> μ₁ ", h1_symbol, " μ₂</li>","</ul>","<p><b>2. Tingkat Signifikansi (α):</b> ", alpha, "</p>","<p><b>3. Aturan Keputusan:</b> Jika p-value < α, maka H₀ ditolak.</p>","<hr>","<h4>Hasil:</h4>","<p>Berdasarkan <b>", res$method, "</b>, t = <b>", round(res$statistic, 3), "</b> dengan p-value = <b>", format.pval(p_value, digits = 4), "</b>.</p>","<p>Rata-rata grup '", levels[1], "' = <b>", round(res$estimate[1], 3), "</b> dan '", levels[2], "' = <b>", round(res$estimate[2], 3), "</b>.</p>","<p>Karena p-value (", format.pval(p_value, digits = 4), ") ", ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), keputusannya <b>", keputusan, "</b>.</p>","<h4>Kesimpulan:</h4>","<p>Pada α = ", alpha * 100, "%, ", kesimpulan, "</p>"))
  })
  
  # --- LOGIKA UNDUH HASIL STATISTIK PER TAB (.DOCX) ---
  
  # 1. Download Handler untuk Ringkasan Uji t Satu Sampel
  output$download_summary_one <- downloadHandler(
    filename = function() {
      paste0("Ringkasan_Uji_T_Satu_Sampel-", input$t_test_one_var, ".docx")
    },
    content = function(file) {
      # Ambil objek 'test' dari dalam list hasil reaktif Anda
      req(t_test_one_results()$test)
      hasil_uji_obj <- t_test_one_results()$test
      
      # Simpan objek hasil t-test ke file .rds
      temp_rds <- file.path(tempdir(), "summary_one.rds")
      saveRDS(hasil_uji_obj, file = temp_rds)
      
      # Buat file .Rmd sementara
      temp_rmd <- file.path(tempdir(), "report.Rmd")
      
      konten_rmd <- paste(
        "---",
        "title: 'Hasil Uji t Satu Sampel'",
        "output: word_document",
        "---",
        "```{r, echo=FALSE}",
        "hasil <- readRDS('summary_one.rds')",
        "print(hasil)",
        "```",
        sep = "\n"
      )
      
      writeLines(konten_rmd, temp_rmd)
      
      # Render Rmd ke docx
      rmarkdown::render(input = temp_rmd, output_file = file, quiet = TRUE)
    }
  )
  
  # 2. Download Handler untuk Ringkasan Uji t Dua Sampel
  output$download_summary_two <- downloadHandler(
    filename = function() {
      paste0("Ringkasan_Uji_T_Dua_Sampel-", input$t_test_two_num_var, ".docx")
    },
    content = function(file) {
      # Ambil objek 'test' dari dalam list hasil reaktif Anda
      req(t_test_two_results()$test)
      hasil_uji_obj <- t_test_two_results()$test
      
      # Simpan objek hasil t-test ke file .rds
      temp_rds <- file.path(tempdir(), "summary_two.rds")
      saveRDS(hasil_uji_obj, file = temp_rds)
      
      # Buat file .Rmd sementara
      temp_rmd <- file.path(tempdir(), "report.Rmd")
      
      konten_rmd <- paste(
        "---",
        "title: 'Hasil Uji t Dua Sampel'",
        "output: word_document",
        "---",
        "```{r, echo=FALSE}",
        "hasil <- readRDS('summary_two.rds')",
        "print(hasil)",
        "```",
        sep = "\n"
      )
      
      writeLines(konten_rmd, temp_rmd)
      
      # Render Rmd ke docx
      rmarkdown::render(input = temp_rmd, output_file = file, quiet = TRUE)
    }
  )
  
  # --- LOGIKA UNDUH HALAMAN UJI RATA-RATA ---
  # 1. OBSERVER UTAMA UNTUK TOMBOL UNDUH
  observeEvent(input$download_rata_page, {
    
    # Cek tab mana yang sedang aktif
    active_tab <- input$tabset_uji_rata
    
    # --- JIKA TAB SATU SAMPEL AKTIF ---
    if (active_tab == "Uji t Satu Sampel") {
      # Pastikan uji sudah dijalankan
      req(t_test_one_results())
      
      # Tampilkan pop-up konfirmasi
      showModal(modalDialog(
        title = "Konfirmasi Unduhan Laporan Uji t Satu Sampel",
        p("Anda akan mengunduh file ZIP berisi:"),
        tags$ul(
          tags$li(strong("1. Hasil_Uji_Statistik.docx:")),
          tags$li(strong("2. Interpretasi_Hasil.docx:"))
        ),
        footer = tagList(
          modalButton("Batal"),
          downloadButton("confirm_download_one_sample", "Ya, Lanjutkan")
        ),
        easyClose = TRUE
      ))
      
      # --- JIKA TAB DUA SAMPEL AKTIF ---
    } else if (active_tab == "Uji t Dua Sampel") {
      # Pastikan uji sudah dijalankan
      req(t_test_two_results())
      
      # Tampilkan pop-up konfirmasi
      showModal(modalDialog(
        title = "Konfirmasi Unduhan Laporan Uji t Dua Sampel",
        p("Anda akan mengunduh file ZIP berisi:"),
        tags$ul(
          tags$li(strong("1. Hasil_Uji_Statistik.docx:")),
          tags$li(strong("2. Interpretasi_Hasil.docx:"))
        ),
        footer = tagList(
          modalButton("Batal"),
          downloadButton("confirm_download_two_sample", "Ya, Lanjutkan")
        ),
        easyClose = TRUE
      ))
    }
  })
  
  
  # 2. DOWNLOAD HANDLER UNTUK LAPORAN SATU SAMPEL
  output$confirm_download_one_sample <- downloadHandler(
    filename = function() {
      paste0("Laporan_Uji_T_Satu_Sampel-", input$t_test_one_var, "-", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Setup direktori sementara
      temp_report_dir <- file.path(tempdir(), "report_one_sample")
      dir.create(temp_report_dir, showWarnings = FALSE)
      
      # --- File 1: Hasil Uji Statistik ---
      hasil_uji_obj <- t_test_one_results()
      saveRDS(hasil_uji_obj, file.path(temp_report_dir, "data.rds"))
      rmd_hasil <- paste("---", "title: 'Hasil Uji Statistik'", "output: word_document", "---",
                         "```{r, echo=FALSE}", "print(readRDS('data.rds'))", "```", sep="\n")
      path_hasil_rmd <- file.path(temp_report_dir, "hasil.Rmd")
      writeLines(rmd_hasil, path_hasil_rmd)
      rmarkdown::render(path_hasil_rmd, output_file = "Hasil_Uji_Statistik.docx", output_dir = temp_report_dir, quiet = TRUE)
      
      # --- File 2: Interpretasi Hasil ---
      alpha <- as.numeric(input$t_test_one_alpha)
      p_val <- hasil_uji_obj$p.value
      kesimpulan <- ifelse(p_val < alpha, 
                           paste0("cukup bukti bahwa rata-rata populasi (", round(hasil_uji_obj$estimate, 4), ") secara signifikan berbeda dari ", input$t_test_one_mu, "."),
                           paste0("tidak cukup bukti bahwa rata-rata populasi berbeda secara signifikan dari ", input$t_test_one_mu, "."))
      rmd_interpretasi <- paste("---", "title: 'Interpretasi Hasil'", "output: word_document", "---",
                                "## Interpretasi Uji t Satu Sampel",
                                paste0("Pada tingkat signifikansi **", alpha*100, "%**, dengan p-value sebesar **", round(p_val, 6), "**, dapat disimpulkan bahwa ", kesimpulan),
                                sep="\n")
      path_interpretasi_rmd <- file.path(temp_report_dir, "interpretasi.Rmd")
      writeLines(rmd_interpretasi, path_interpretasi_rmd)
      rmarkdown::render(path_interpretasi_rmd, output_file = "Interpretasi_Hasil.docx", output_dir = temp_report_dir, quiet = TRUE)
      
      # Zip kedua file
      zip::zipr(zipfile = file, files = c("Hasil_Uji_Statistik.docx", "Interpretasi_Hasil.docx"), root = temp_report_dir)
    }
  )
  
  
  # 3. DOWNLOAD HANDLER UNTUK LAPORAN DUA SAMPEL
  output$confirm_download_two_sample <- downloadHandler(
    filename = function() {
      paste0("Laporan_Uji_T_Dua_Sampel-", input$t_test_two_num_var, "-", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Setup direktori sementara
      temp_report_dir <- file.path(tempdir(), "report_two_sample")
      dir.create(temp_report_dir, showWarnings = FALSE)
      
      # --- File 1: Hasil Uji Statistik ---
      hasil_uji_obj <- t_test_two_results()
      saveRDS(hasil_uji_obj, file.path(temp_report_dir, "data.rds"))
      rmd_hasil <- paste("---", "title: 'Hasil Uji Statistik'", "output: word_document", "---",
                         "```{r, echo=FALSE}", "print(readRDS('data.rds'))", "```", sep="\n")
      path_hasil_rmd <- file.path(temp_report_dir, "hasil.Rmd")
      writeLines(rmd_hasil, path_hasil_rmd)
      rmarkdown::render(path_hasil_rmd, output_file = "Hasil_Uji_Statistik.docx", output_dir = temp_report_dir, quiet = TRUE)
      
      # --- File 2: Interpretasi Hasil ---
      alpha <- as.numeric(input$t_test_two_alpha)
      p_val <- hasil_uji_obj$p.value
      grup_nama <- names(hasil_uji_obj$estimate)
      kesimpulan <- ifelse(p_val < alpha, 
                           paste0("terdapat perbedaan rata-rata yang signifikan antara ", grup_nama[1], " (", round(hasil_uji_obj$estimate[1], 4), ") dan ", grup_nama[2], " (", round(hasil_uji_obj$estimate[2], 4), ")."),
                           "tidak terdapat perbedaan rata-rata yang signifikan antara kedua kelompok.")
      rmd_interpretasi <- paste("---", "title: 'Interpretasi Hasil'", "output: word_document", "---",
                                "## Interpretasi Uji t Dua Sampel",
                                paste0("Pada tingkat signifikansi **", alpha*100, "%**, dengan p-value sebesar **", round(p_val, 6), "**, dapat disimpulkan bahwa ", kesimpulan),
                                sep="\n")
      path_interpretasi_rmd <- file.path(temp_report_dir, "interpretasi.Rmd")
      writeLines(rmd_interpretasi, path_interpretasi_rmd)
      rmarkdown::render(path_interpretasi_rmd, output_file = "Interpretasi_Hasil.docx", output_dir = temp_report_dir, quiet = TRUE)
      
      # Zip kedua file
      zip::zipr(zipfile = file, files = c("Hasil_Uji_Statistik.docx", "Interpretasi_Hasil.docx"), root = temp_report_dir)
    }
  )
  
  # --- LOGIKA UJI PROPORSI & RAGAM ---
  
  # Buat satu object reactiveValues untuk menampung hasil yang akan ditampilkan di UI
  prop_ragam_results <- reactiveValues(output = NULL, interpretation = NULL)
  
  # Bagian UI dinamis (tanpa perubahan)
  output$prop_one_success_level_ui <- renderUI({
    req(rv$data, input$prop_one_var)
    var_levels <- levels(as.factor(rv$data[[input$prop_one_var]]))
    selectInput("prop_one_success_level", "2. Pilih Level 'Sukses':", choices = var_levels)
  })
  output$prop_two_success_level_ui <- renderUI({
    req(rv$data, input$prop_two_var)
    var_levels <- levels(as.factor(rv$data[[input$prop_two_var]]))
    selectInput("prop_two_success_level", "2. Pilih Level 'Sukses':", choices = var_levels)
  })
  
  # --- 1. UJI PROPORSI SATU SAMPEL ---
  prop_one_results <- eventReactive(input$run_prop_one_test, {
    req(rv$data, input$prop_one_var, input$prop_one_success_level, !is.na(input$prop_one_p0))
    var_data <- rv$data[[input$prop_one_var]]
    sukses <- sum(var_data == input$prop_one_success_level, na.rm = TRUE)
    total_n <- sum(!is.na(var_data))
    prop.test(x = sukses, n = total_n, p = input$prop_one_p0, alternative = input$prop_one_alternative)
  })
  observeEvent(prop_one_results(), {
    test_res <- prop_one_results()
    prop_ragam_results$output <- test_res
    # (logika interpretasi Anda)
    alpha <- as.numeric(input$prop_one_alpha); p_value <- test_res$p.value; p0 <- input$prop_one_p0; h1_symbol <- switch(input$prop_one_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<"); keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀"); kesimpulan <- ifelse(p_value < alpha, paste0("terdapat cukup bukti bahwa proporsi populasi berbeda dari ", p0, "."), paste0("tidak ada cukup bukti bahwa proporsi populasi berbeda dari ", p0, ".")); prop_ragam_results$interpretation <- HTML(paste0("<h4>Uji Proporsi Satu Sampel</h4>", "<p><b>Hipotesis:</b> H₀: p = ", p0, " vs. H₁: p ", h1_symbol, " ", p0, "</p>", "<p><b>Aturan Keputusan:</b> Tolak H₀ jika p-value < α (", alpha, ").</p>", "<hr>", "<p>Proporsi sampel (p̂) = <b>", round(test_res$estimate, 4), "</b>.</p>", "<p>Statistik X² = <b>", round(test_res$statistic, 3), "</b> dengan p-value = <b>", format.pval(p_value, digits=4), "</b>.</p>", "<p>Karena p-value ", ifelse(p_value < alpha, "<", "≥"), " α, keputusannya <b>", keputusan, "</b>.</p>", "<h4>Kesimpulan:</h4><p>Pada α = ", alpha*100, "%, ", kesimpulan, "</p>"))
  })
  
  # --- 2. UJI PROPORSI DUA SAMPEL ---
  prop_two_results <- eventReactive(input$run_prop_two_test, {
    req(rv$data, input$prop_two_var, input$prop_two_group_var, input$prop_two_success_level)
    df_clean <- rv$data %>% select(var = !!sym(input$prop_two_var), group = !!sym(input$prop_two_group_var)) %>% na.omit()
    df_clean$group <- as.factor(df_clean$group)
    req(nlevels(df_clean$group) == 2)
    contingency_table <- table(df_clean$var, df_clean$group)
    sukses_level <- input$prop_two_success_level
    req(sukses_level %in% rownames(contingency_table))
    x_counts <- contingency_table[sukses_level, ]
    n_counts <- colSums(contingency_table)
    prop.test(x = x_counts, n = n_counts, alternative = input$prop_two_alternative)
  })
  observeEvent(prop_two_results(), {
    test_res <- prop_two_results()
    prop_ragam_results$output <- test_res
    # (logika interpretasi Anda)
    alpha <- as.numeric(input$prop_two_alpha); p_value <- test_res$p.value; levels <- names(test_res$estimate); h1_symbol <- switch(input$prop_two_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<"); keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀"); kesimpulan <- ifelse(p_value < alpha, "terdapat perbedaan proporsi yang signifikan.", "tidak terdapat perbedaan proporsi yang signifikan."); prop_ragam_results$interpretation <- HTML(paste0("<h4>Uji Proporsi Dua Sampel</h4>", "<p><b>Hipotesis:</b> H₀: p₁ = p₂ vs. H₁: p₁ ", h1_symbol, " p₂</p>", "<p><b>Aturan Keputusan:</b> Tolak H₀ jika p-value < α (", alpha, ").</p>", "<hr>", "<p>Proporsi grup '", levels[1], "' = <b>", round(test_res$estimate[1], 4), "</b> dan '", levels[2], "' = <b>", round(test_res$estimate[2], 4), "</b>.</p>", "<p>Statistik X² = <b>", round(test_res$statistic, 3), "</b> dengan p-value = <b>", format.pval(p_value, digits=4), "</b>.</p>", "<p>Karena p-value ", ifelse(p_value < alpha, "<", "≥"), " α, keputusannya <b>", keputusan, "</b>.</p>", "<h4>Kesimpulan:</h4><p>Pada α = ", alpha*100, "%, ", kesimpulan, "</p>"))
  })
  
  # --- 3. UJI RAGAM SATU SAMPEL ---
  # Pastikan Anda sudah memuat library EnvStats yang berisi fungsi varTest
  library(EnvStats)
  var_one_results <- eventReactive(input$run_var_one_test, {
    req(rv$data, input$var_one_var, !is.na(input$var_one_sigma2))
    var_data <- na.omit(rv$data[[input$var_one_var]])
    req(length(var_data) >= 2)
    varTest(var_data, sigma.squared = input$var_one_sigma2, alternative = input$var_one_alternative)
  })
  observeEvent(var_one_results(), {
    test_res <- var_one_results()
    prop_ragam_results$output <- test_res
    # (logika interpretasi Anda)
    alpha <- as.numeric(input$var_one_alpha); p_value <- test_res$p.value; sigma2_0 <- input$var_one_sigma2; h1_symbol <- switch(input$var_one_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<"); keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀"); kesimpulan <- ifelse(p_value < alpha, "ragam populasi secara signifikan berbeda dari nilai hipotesis.", "tidak ada cukup bukti bahwa ragam populasi berbeda dari nilai hipotesis."); prop_ragam_results$interpretation <- HTML(paste0("<h4>Uji Ragam Satu Sampel</h4>", "<p><b>Hipotesis:</b> H₀: σ² = ", sigma2_0, " vs. H₁: σ² ", h1_symbol, " ", sigma2_0, "</p>", "<p><b>Aturan Keputusan:</b> Tolak H₀ jika p-value < α (", alpha, ").</p>", "<hr>", "<p>Ragam sampel (s²) = <b>", round(test_res$estimate, 4), "</b>.</p>", "<p>Statistik χ² = <b>", round(test_res$statistic, 3), "</b> dengan p-value = <b>", format.pval(p_value, digits=4), "</b>.</p>", "<p>Karena p-value ", ifelse(p_value < alpha, "<", "≥"), " α, keputusannya <b>", keputusan, "</b>.</p>", "<h4>Kesimpulan:</h4><p>Pada α = ", alpha*100, "%, ", kesimpulan, "</p>"))
  })
  
  # --- 4. UJI RAGAM DUA SAMPEL ---
  var_two_results <- eventReactive(input$run_var_two_test, {
    req(input$var_two_num_var, input$var_two_cat_var)
    df_clean <- rv$data %>% select(num_var = !!sym(input$var_two_num_var), cat_var = !!sym(input$var_two_cat_var)) %>% na.omit()
    df_clean$cat_var <- as.factor(df_clean$cat_var)
    req(nlevels(df_clean$cat_var) == 2)
    formula <- as.formula("num_var ~ cat_var")
    test_result <- var.test(formula, data = df_clean, alternative = input$var_two_alternative)
    # PERBAIKAN: Kembalikan hasil tes DAN nama level grup dalam satu list
    return(list(test = test_result, levels = levels(df_clean$cat_var)))
  })
  # Ganti observeEvent Anda dengan yang ini
  observeEvent(var_two_results(), {
    # Ambil "paket" berisi hasil tes dan nama level
    results_list <- var_two_results()
    
    # "Buka paketnya" ke dalam variabel terpisah
    test_res <- results_list$test   # PERBAIKAN 1: Ambil '$test' dari list
    levels <- results_list$levels # PERBAIKAN 2: Ambil '$levels' dari list
    
    # Kirim hasil statistik mentah ke UI
    prop_ragam_results$output <- test_res
    
    # Sekarang, sisa kode interpretasi Anda akan berfungsi
    alpha <- as.numeric(input$var_two_alpha)
    p_value <- test_res$p.value
    h1_symbol <- switch(input$var_two_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<")
    keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀")
    
    # Menggunakan variabel 'levels' yang sudah kita ambil agar lebih informatif
    kesimpulan <- ifelse(p_value < alpha, 
                         paste0("terdapat perbedaan ragam yang signifikan antara grup '", levels[1], "' dan '", levels[2], "'."), 
                         paste0("tidak ada cukup bukti adanya perbedaan ragam antara grup '", levels[1], "' dan '", levels[2], "'."))
    
    prop_ragam_results$interpretation <- HTML(paste0(
      "<h4>Uji Ragam Dua Sampel (F-Test)</h4>",
      "<p><b>Hipotesis:</b> H₀: σ₁²/σ₂² = 1 vs. H₁: σ₁²/σ₂² ", h1_symbol, " 1</p>",
      "<p><b>Aturan Keputusan:</b> Tolak H₀ jika p-value < α (", alpha, ").</p>",
      "<hr>",
      "<p>Rasio ragam sampel (s₁²/s₂²) = <b>", round(test_res$estimate, 4), "</b>.</p>",
      "<p>Statistik F = <b>", round(test_res$statistic, 3), "</b> dengan p-value = <b>", format.pval(p_value, digits=4), "</b>.</p>",
      "<p>Karena p-value ", ifelse(p_value < alpha, "<", "≥"), " α, keputusannya <b>", keputusan, "</b>.</p>",
      "<h4>Kesimpulan:</h4><p>Pada α = ", alpha*100, "%, ", kesimpulan, "</p>"
    ))
  })
  
  # Render output ke UI (tanpa perubahan)
  output$prop_test_output <- renderPrint({ prop_ragam_results$output })
  output$prop_test_interpretation <- renderUI({ prop_ragam_results$interpretation })
  output$var_test_output <- renderPrint({ prop_ragam_results$output })
  output$var_test_interpretation <- renderUI({ prop_ragam_results$interpretation })
  
  # --- KODE DOWNLOAD HANDLER ---
  # Kode ini sekarang akan berfungsi karena memanggil eventReactive yang sudah kita buat.
  # 1. Unduh Uji Proporsi Satu Sampel
  output$download_prop_one <- downloadHandler(
    filename = function() {
      paste0("Hasil_Proporsi_Satu_Sampel-", input$prop_one_var, ".docx")
    },
    content = function(file) {
      # Ganti 'prop_one_results()' dengan nama reactive Anda
      req(prop_one_results())
      saveRDS(prop_one_results(), file.path(tempdir(), "data.rds"))
      
      rmd_path <- file.path(tempdir(), "report.Rmd")
      konten_rmd <- paste(
        "---", "title: 'Hasil Uji Proporsi Satu Sampel'", "output: word_document", "---",
        "```{r, echo=FALSE}", "print(readRDS('data.rds'))", "```", sep = "\n"
      )
      writeLines(konten_rmd, rmd_path)
      rmarkdown::render(rmd_path, output_file = file, quiet = TRUE)
    }
  )
  
  # 2. Unduh Uji Proporsi Dua Sampel
  output$download_prop_two <- downloadHandler(
    filename = function() {
      paste0("Hasil_Proporsi_Dua_Sampel-", input$prop_two_var, ".docx")
    },
    content = function(file) {
      # Ganti 'prop_two_results()' dengan nama reactive Anda
      req(prop_two_results())
      saveRDS(prop_two_results(), file.path(tempdir(), "data.rds"))
      
      rmd_path <- file.path(tempdir(), "report.Rmd")
      konten_rmd <- paste(
        "---", "title: 'Hasil Uji Proporsi Dua Sampel'", "output: word_document", "---",
        "```{r, echo=FALSE}", "print(readRDS('data.rds'))", "```", sep = "\n"
      )
      writeLines(konten_rmd, rmd_path)
      rmarkdown::render(rmd_path, output_file = file, quiet = TRUE)
    }
  )
  
  # 3. Unduh Uji Ragam Satu Sampel
  output$download_var_one <- downloadHandler(
    filename = function() {
      paste0("Hasil_Ragam_Satu_Sampel-", input$var_one_var, ".docx")
    },
    content = function(file) {
      # Ganti 'var_one_results()' dengan nama reactive Anda
      req(var_one_results())
      saveRDS(var_one_results(), file.path(tempdir(), "data.rds"))
      
      rmd_path <- file.path(tempdir(), "report.Rmd")
      konten_rmd <- paste(
        "---", "title: 'Hasil Uji Ragam Satu Sampel (Chi-Square)'", "output: word_document", "---",
        "```{r, echo=FALSE}", "print(readRDS('data.rds'))", "```", sep = "\n"
      )
      writeLines(konten_rmd, rmd_path)
      rmarkdown::render(rmd_path, output_file = file, quiet = TRUE)
    }
  )
  
  # 4. Unduh Uji Ragam Dua Sampel
  output$download_var_two <- downloadHandler(
    filename = function() {
      paste0("Hasil_Ragam_Dua_Sampel-", input$var_two_num_var, ".docx")
    },
    content = function(file) {
      # Ganti 'var_two_results()' dengan nama reactive Anda
      req(var_two_results())
      saveRDS(var_two_results(), file.path(tempdir(), "data.rds"))
      
      rmd_path <- file.path(tempdir(), "report.Rmd")
      konten_rmd <- paste(
        "---", "title: 'Hasil Uji Ragam Dua Sampel (F-Test)'", "output: word_document", "---",
        "```{r, echo=FALSE}", "print(readRDS('data.rds'))", "```", sep = "\n"
      )
      writeLines(konten_rmd, rmd_path)
      rmarkdown::render(rmd_path, output_file = file, quiet = TRUE)
    }
  )
  
  # --- LOGIKA UNDUH HALAMAN UJI PROPORSI & RAGAM (DINAMIS) ---
  
  # 1. OBSERVER UNTUK TOMBOL UNDUH UTAMA
  observeEvent(input$download_prop_ragam_page, {
    
    # Cek tab mana yang aktif
    active_tab <- input$tabset_uji_prop_ragam
    
    # Variabel untuk melacak apakah ada uji yang sudah dijalankan
    test_ran <- FALSE
    modal_title <- ""
    download_button_id <- ""
    
    # Logika untuk Tab "Uji Proporsi"
    if (active_tab == "Uji Proporsi") {
      modal_title <- "Konfirmasi Unduhan Laporan Uji Proporsi"
      download_button_id <- "confirm_download_prop"
      # Cek apakah salah satu atau kedua uji proporsi sudah pernah dijalankan
      if (!is.null(prop_one_results()) || !is.null(prop_two_results())) {
        test_ran <- TRUE
      }
    } 
    # Logika untuk Tab "Uji Ragam"
    else if (active_tab == "Uji Ragam") {
      modal_title <- "Konfirmasi Unduhan Laporan Uji Ragam"
      download_button_id <- "confirm_download_var"
      # Cek apakah salah satu atau kedua uji ragam sudah pernah dijalankan
      if (!is.null(var_one_results()) || !is.null(var_two_results())) {
        test_ran <- TRUE
      }
    }
    
    # Tampilkan pop-up (modal) jika ada tes yang sudah dijalankan
    if (test_ran) {
      showModal(modalDialog(
        title = modal_title,
        p("Laporan ZIP akan dibuat berisi hasil dan interpretasi dari semua uji yang sudah Anda jalankan di tab ini."),
        footer = tagList(
          modalButton("Batal"),
          downloadButton(download_button_id, "Ya, Lanjutkan Unduh")
        ),
        easyClose = TRUE
      ))
    } else {
      # Tampilkan notifikasi jika belum ada uji yang dijalankan
      showNotification("Harap jalankan setidaknya satu uji di tab ini sebelum mengunduh laporan.", type = "warning")
    }
  })
  
  # 2. DOWNLOAD HANDLER UNTUK TAB UJI PROPORSI (VERSI LENGKAP)
  output$confirm_download_prop <- downloadHandler(
    filename = function() {
      paste0("Laporan_Uji_Proporsi-", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir <- file.path(tempdir(), "report_prop")
      dir.create(temp_dir, showWarnings = FALSE)
      
      files_to_zip <- c()
      interpretation_text <- c("---", "title: 'Laporan Interpretasi Uji Proporsi'", "output: word_document", "---", "")
      
      # --- Proses Uji 1 Sampel (jika dijalankan) ---
      if (!is.null(prop_one_results())) {
        # Buat file hasil statistik
        res <- prop_one_results()
        saveRDS(res, file.path(temp_dir, "prop1.rds"))
        rmd_hasil <- "--- \n title: 'Hasil Uji Proporsi Satu Sampel' \n output: word_document \n --- \n ```{r, echo=FALSE} \n print(readRDS('prop1.rds')) \n ```"
        path_hasil_rmd <- file.path(temp_dir, "hasil1.Rmd")
        writeLines(rmd_hasil, path_hasil_rmd)
        rmarkdown::render(path_hasil_rmd, output_file = "Hasil_Proporsi_Satu_Sampel.docx", output_dir = temp_dir, quiet = TRUE)
        files_to_zip <- c(files_to_zip, "Hasil_Proporsi_Satu_Sampel.docx")
        
        # Buat teks interpretasi LENGKAP dalam format Markdown
        alpha <- as.numeric(input$prop_one_alpha); p_value <- res$p.value; p0 <- input$prop_one_p0
        h1_symbol <- switch(input$prop_one_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<")
        keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀")
        kesimpulan <- ifelse(p_value < alpha, paste0("terdapat cukup bukti bahwa proporsi populasi berbeda dari ", p0, "."), paste0("tidak ada cukup bukti bahwa proporsi populasi berbeda dari ", p0, "."))
        
        detail_text_1 <- c(
          "## Uji Proporsi Satu Sampel",
          paste0("**Hipotesis:** H₀: p = ", p0, " vs. H₁: p ", h1_symbol, " ", p0),
          paste0("**Aturan Keputusan:** Tolak H₀ jika p-value < α (", alpha, ")."),
          "***",
          paste0("Proporsi sampel (p̂) = **", round(res$estimate, 4), "**."),
          paste0("Statistik X² = **", round(res$statistic, 3), "** dengan p-value = **", format.pval(p_value, digits=4), "**."),
          paste0("Karena p-value ", ifelse(p_value < alpha, "<", "≥"), " α, keputusannya **", keputusan, "**."),
          "",
          "### Kesimpulan:",
          paste0("Pada α = ", alpha*100, "%, ", kesimpulan),
          ""
        )
        interpretation_text <- c(interpretation_text, detail_text_1)
      }
      
      # --- Proses Uji 2 Sampel (jika dijalankan) ---
      if (!is.null(prop_two_results())) {
        # Buat file hasil statistik
        res <- prop_two_results()
        saveRDS(res, file.path(temp_dir, "prop2.rds"))
        rmd_hasil <- "--- \n title: 'Hasil Uji Proporsi Dua Sampel' \n output: word_document \n --- \n ```{r, echo=FALSE} \n print(readRDS('prop2.rds')) \n ```"
        path_hasil_rmd <- file.path(temp_dir, "hasil2.Rmd")
        writeLines(rmd_hasil, path_hasil_rmd)
        rmarkdown::render(path_hasil_rmd, output_file = "Hasil_Proporsi_Dua_Sampel.docx", output_dir = temp_dir, quiet = TRUE)
        files_to_zip <- c(files_to_zip, "Hasil_Proporsi_Dua_Sampel.docx")
        
        # Buat teks interpretasi LENGKAP
        alpha <- as.numeric(input$prop_two_alpha); p_value <- res$p.value; levels <- names(res$estimate)
        h1_symbol <- switch(input$prop_two_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<")
        keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀")
        kesimpulan <- ifelse(p_value < alpha, "terdapat perbedaan proporsi yang signifikan.", "tidak terdapat perbedaan proporsi yang signifikan.")
        
        detail_text_2 <- c(
          "## Uji Proporsi Dua Sampel",
          paste0("**Hipotesis:** H₀: p₁ = p₂ vs. H₁: p₁ ", h1_symbol, " p₂"),
          paste0("**Aturan Keputusan:** Tolak H₀ jika p-value < α (", alpha, ")."),
          "***",
          paste0("Proporsi grup '", levels[1], "' = **", round(res$estimate[1], 4), "** dan '", levels[2], "' = **", round(res$estimate[2], 4), "**."),
          paste0("Statistik X² = **", round(res$statistic, 3), "** dengan p-value = **", format.pval(p_value, digits=4), "**."),
          paste0("Karena p-value ", ifelse(p_value < alpha, "<", "≥"), " α, keputusannya **", keputusan, "**."),
          "",
          "### Kesimpulan:",
          paste0("Pada α = ", alpha*100, "%, ", kesimpulan),
          ""
        )
        interpretation_text <- c(interpretation_text, detail_text_2)
      }
      
      # --- Buat Laporan Interpretasi Gabungan ---
      path_interp_rmd <- file.path(temp_dir, "interpretasi.Rmd")
      writeLines(paste(interpretation_text, collapse="\n"), path_interp_rmd)
      rmarkdown::render(path_interp_rmd, output_file = "Laporan_Interpretasi_Proporsi.docx", output_dir = temp_dir, quiet = TRUE)
      files_to_zip <- c(files_to_zip, "Laporan_Interpretasi_Proporsi.docx")
      
      zip::zipr(zipfile = file, files = files_to_zip, root = temp_dir)
    }
  )
  
  # 3. DOWNLOAD HANDLER UNTUK TAB UJI RAGAM (VERSI LENGKAP)
  output$confirm_download_var <- downloadHandler(
    filename = function() {
      paste0("Laporan_Uji_Ragam-", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir <- file.path(tempdir(), "report_var")
      dir.create(temp_dir, showWarnings = FALSE)
      
      files_to_zip <- c()
      interpretation_text <- c("---", "title: 'Laporan Interpretasi Uji Ragam'", "output: word_document", "---", "")
      
      # --- Proses Uji 1 Sampel (jika dijalankan) ---
      if (!is.null(var_one_results())) {
        res <- var_one_results()
        saveRDS(res, file.path(temp_dir, "var1.rds"))
        rmd_hasil <- "--- \n title: 'Hasil Uji Ragam Satu Sampel' \n output: word_document \n --- \n ```{r, echo=FALSE} \n print(readRDS('var1.rds')) \n ```"
        path_hasil_rmd <- file.path(temp_dir, "hasil1.Rmd")
        writeLines(rmd_hasil, path_hasil_rmd)
        rmarkdown::render(path_hasil_rmd, output_file = "Hasil_Ragam_Satu_Sampel.docx", output_dir = temp_dir, quiet = TRUE)
        files_to_zip <- c(files_to_zip, "Hasil_Ragam_Satu_Sampel.docx")
        
        # Buat teks interpretasi LENGKAP
        alpha <- as.numeric(input$var_one_alpha); p_value <- res$p.value; sigma2_0 <- input$var_one_sigma2
        h1_symbol <- switch(input$var_one_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<")
        keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀")
        kesimpulan <- ifelse(p_value < alpha, "ragam populasi secara signifikan berbeda dari nilai hipotesis.", "tidak ada cukup bukti bahwa ragam populasi berbeda dari nilai hipotesis.")
        
        detail_text_1 <- c(
          "## Uji Ragam Satu Sampel",
          paste0("**Hipotesis:** H₀: σ² = ", sigma2_0, " vs. H₁: σ² ", h1_symbol, " ", sigma2_0),
          paste0("**Aturan Keputusan:** Tolak H₀ jika p-value < α (", alpha, ")."),
          "***",
          paste0("Ragam sampel (s²) = **", round(res$estimate, 4), "**."),
          paste0("Statistik χ² = **", round(res$statistic, 3), "** dengan p-value = **", format.pval(p_value, digits=4), "**."),
          paste0("Karena p-value ", ifelse(p_value < alpha, "<", "≥"), " α, keputusannya **", keputusan, "**."),
          "",
          "### Kesimpulan:",
          paste0("Pada α = ", alpha*100, "%, ", kesimpulan),
          ""
        )
        interpretation_text <- c(interpretation_text, detail_text_1)
      }
      
      # --- Proses Uji 2 Sampel (jika dijalankan) ---
      if (!is.null(var_two_results())) {
        res <- var_two_results()$test # Ingat, ini adalah list
        levels <- var_two_results()$levels
        saveRDS(res, file.path(temp_dir, "var2.rds"))
        rmd_hasil <- "--- \n title: 'Hasil Uji Ragam Dua Sampel' \n output: word_document \n --- \n ```{r, echo=FALSE} \n print(readRDS('var2.rds')) \n ```"
        path_hasil_rmd <- file.path(temp_dir, "hasil2.Rmd")
        writeLines(rmd_hasil, path_hasil_rmd)
        rmarkdown::render(path_hasil_rmd, output_file = "Hasil_Ragam_Dua_Sampel.docx", output_dir = temp_dir, quiet = TRUE)
        files_to_zip <- c(files_to_zip, "Hasil_Ragam_Dua_Sampel.docx")
        
        # Buat teks interpretasi LENGKAP
        alpha <- as.numeric(input$var_two_alpha); p_value <- res$p.value
        h1_symbol <- switch(input$var_two_alternative, "two.sided" = "≠", "greater" = ">", "less" = "<")
        keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀")
        kesimpulan <- ifelse(p_value < alpha, "terdapat perbedaan ragam yang signifikan antara kedua grup.", "tidak ada cukup bukti adanya perbedaan ragam antara kedua grup.")
        
        detail_text_2 <- c(
          "## Uji Ragam Dua Sampel (F-Test)",
          paste0("**Hipotesis:** H₀: σ₁²/σ₂² = 1 vs. H₁: σ₁²/σ₂² ", h1_symbol, " 1"),
          paste0("**Aturan Keputusan:** Tolak H₀ jika p-value < α (", alpha, ")."),
          "***",
          paste0("Rasio ragam sampel (s₁²/s₂²) = **", round(res$estimate, 4), "**."),
          paste0("Statistik F = **", round(res$statistic, 3), "** dengan p-value = **", format.pval(p_value, digits=4), "**."),
          paste0("Karena p-value ", ifelse(p_value < alpha, "<", "≥"), " α, keputusannya **", keputusan, "**."),
          "",
          "### Kesimpulan:",
          paste0("Pada α = ", alpha*100, "%, ", kesimpulan),
          ""
        )
        interpretation_text <- c(interpretation_text, detail_text_2)
      }
      
      # --- Buat Laporan Interpretasi Gabungan ---
      path_interp_rmd <- file.path(temp_dir, "interpretasi.Rmd")
      writeLines(paste(interpretation_text, collapse="\n"), path_interp_rmd)
      rmarkdown::render(path_interp_rmd, output_file = "Laporan_Interpretasi_Ragam.docx", output_dir = temp_dir, quiet = TRUE)
      files_to_zip <- c(files_to_zip, "Laporan_Interpretasi_Ragam.docx")
      
      zip::zipr(zipfile = file, files = files_to_zip, root = temp_dir)
    }
  )
  
  # --- LOGIKA ANOVA ---
  anova_results <- eventReactive(input$run_anova, {
    req(input$anova_num_var, input$anova_cat_var, input$anova_cat_var != "")
    
    df_clean <- rv$data %>%
      select(num_var = !!sym(input$anova_num_var), cat_var = !!sym(input$anova_cat_var)) %>%
      na.omit()
    
    df_clean$cat_var <- as.factor(df_clean$cat_var)
    
    if (nlevels(df_clean$cat_var) <= 2) {
      return(list(error = "Gagal: Variabel grup untuk ANOVA harus memiliki lebih dari 2 kategori. Untuk 2 kategori, gunakan 'Uji t Dua Sampel'."))
    }
    
    formula <- as.formula("num_var ~ cat_var")
    aov_result <- aov(formula, data = df_clean)
    posthoc_result <- TukeyHSD(aov_result)
    
    return(list(
      aov = aov_result,
      posthoc = posthoc_result,
      data = df_clean,
      error = NULL
    ))
  })
  
  output$anova_summary_output <- renderPrint({
    res_list <- anova_results()
    if (!is.null(res_list$error)) {
      cat(res_list$error)
    } else {
      summary(res_list$aov)
    }
  })
  
  output$anova_posthoc_output <- renderPrint({
    res_list <- anova_results()
    if (is.null(res_list$error)) {
      res_list$posthoc
    }
  })
  
  output$anova_plot_output <- renderPlot({
    res_list <- anova_results()
    req(is.null(res_list$error))
    
    ggplot(res_list$data, aes_string(x = "cat_var", y = "num_var", fill = "cat_var")) +
      geom_boxplot(alpha = 0.8) +
      stat_summary(fun=mean, geom="point", shape=23, size=3, fill="white") +
      labs(
        x = paste("Kelompok", input$anova_cat_var),
        y = paste("Nilai", input$anova_num_var)
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
  })
  
  output$anova_interpretation <- renderUI({
    res_list <- anova_results()
    req(is.null(res_list$error))
    
    alpha <- as.numeric(input$anova_alpha)
    aov_summary <- summary(res_list$aov)
    
    f_value <- aov_summary[[1]]$`F value`[1]
    p_value <- aov_summary[[1]]$`Pr(>F)`[1]
    
    keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀")
    kesimpulan_utama <- ifelse(p_value < alpha,
                               "terdapat cukup bukti untuk menyatakan bahwa <b>setidaknya ada satu kelompok yang memiliki rata-rata yang berbeda secara signifikan</b> dari kelompok lainnya.",
                               "tidak ada cukup bukti untuk menyatakan adanya perbedaan rata-rata yang signifikan di antara kelompok-kelompok yang diuji.")
    
    kesimpulan_posthoc <- ""
    if (p_value < alpha) {
      kesimpulan_posthoc <- paste0(
        "<hr><h4>Interpretasi Uji Lanjutan (Tukey HSD)</h4>",
        "<p>Karena hasil ANOVA signifikan (H₀ ditolak), kita melihat uji Tukey HSD untuk mengetahui pasangan kelompok mana yang berbeda secara signifikan. Pada tabel di tab 'Uji Lanjutan (Post-Hoc)', lihatlah pasangan dengan nilai <b>p adj < ", alpha, "</b>. Pasangan-pasangan inilah yang rata-ratanya berbeda secara nyata.</p>"
      )
    }
    
    HTML(paste0(
      "<h4>Langkah-langkah Interpretasi ANOVA:</h4>",
      "<p><b>1. Hipotesis:</b></p>",
      "<ul>",
      "<li><b>H₀ (Hipotesis Nol):</b> Rata-rata semua kelompok adalah sama (μ₁ = μ₂ = ... = μk).</li>",
      "<li><b>H₁ (Hipotesis Alternatif):</b> Setidaknya ada satu rata-rata kelompok yang berbeda.</li>",
      "</ul>",
      "<p><b>2. Tingkat Signifikansi (α):</b> ", alpha, "</p>",
      "<p><b>3. Aturan Keputusan:</b> Jika p-value < α, maka H₀ ditolak.</p>",
      "<hr>",
      "<h4>Hasil Analisis:</h4>",
      "<p>Berdasarkan analisis varians, diperoleh statistik uji F sebesar <b>", round(f_value, 3), "</b> dengan nilai p-value sebesar <b>", format.pval(p_value, digits = 4), "</b>.</p>",
      "<p>Karena p-value (", format.pval(p_value, digits = 4), ") ", ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), maka keputusannya adalah <b>", keputusan, "</b>.</p>",
      "<h4>Kesimpulan:</h4>",
      "<p>Pada tingkat signifikansi ", alpha * 100, "%, ", kesimpulan_utama, "</p>",
      kesimpulan_posthoc
    ))
  })
  
  # --- LOGIKA DOWNLOAD UNTUK ANOVA ---
  
  # 1. BUAT REAKTIF BARU UNTUK OBJEK PLOT (AGAR BISA DIPAKAI ULANG)
  anova_plot_object <- eventReactive(input$run_anova, {
    res_list <- anova_results()
    req(is.null(res_list$error))
    
    ggplot(res_list$data, aes_string(x = "cat_var", y = "num_var", fill = "cat_var")) +
      geom_boxplot(alpha = 0.8) +
      stat_summary(fun=mean, geom="point", shape=23, size=3, fill="white") +
      labs(
        title = paste("Box Plot untuk", input$anova_num_var, "berdasarkan", input$anova_cat_var),
        x = paste("Kelompok", input$anova_cat_var),
        y = paste("Nilai", input$anova_num_var)
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
  })
  
  # Gunakan reaktif baru ini untuk renderPlot Anda (opsional tapi disarankan)
  output$anova_plot_output <- renderPlot({
    anova_plot_object()
  })
  
  # --- DOWNLOAD HANDLER UNTUK TABEL ANOVA UTAMA ---
  output$download_anova_summary <- downloadHandler(
    filename = function() {
      paste0("Tabel_ANOVA-", input$anova_num_var, "-", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(anova_results()$aov)
      
      # Gunakan broom::tidy() untuk merapikan hasil ANOVA menjadi tabel yang bersih
      aov_tidy <- broom::tidy(anova_results()$aov)
      
      # Setup direktori sementara
      temp_dir <- tempdir()
      
      # Simpan tabel yang sudah rapi ke file .rds
      saveRDS(aov_tidy, file.path(temp_dir, "anova_summary.rds"))
      
      # Buat konten R Markdown
      konten_rmd <- paste(
        "---", 
        "title: 'Tabel Ringkasan ANOVA'", 
        "output: word_document", 
        "---",
        "```{r, echo=FALSE, message=FALSE, warning=FALSE}", 
        "library(knitr)",
        "summary_df <- readRDS('anova_summary.rds')",
        "kable(summary_df, caption = 'Hasil Analisis Varians (ANOVA)')",
        "```",
        sep = "\n"
      )
      
      # Tulis konten ke file .Rmd
      rmd_path <- file.path(temp_dir, "summary.Rmd")
      writeLines(konten_rmd, rmd_path)
      
      # Render file .Rmd ke .docx
      rmarkdown::render(rmd_path, output_file = file, quiet = TRUE)
    }
  )
  
  # 2. DOWNLOAD HANDLER UNTUK BOX PLOT (.png)
  output$download_anova_plot <- downloadHandler(
    filename = function() {
      paste0("BoxPlot_ANOVA-", input$anova_num_var, ".png")
    },
    content = function(file) {
      # ggsave adalah cara terbaik untuk menyimpan ggplot
      ggsave(file, plot = anova_plot_object(), device = "png", width = 8, height = 6, dpi = 300)
    }
  )
  
  # 3. DOWNLOAD HANDLER UNTUK UJI LANJUTAN/POST-HOC (.docx)
  output$download_anova_posthoc <- downloadHandler(
    filename = function() {
      paste0("Hasil_PostHoc_Tukey-", input$anova_num_var, ".docx")
    },
    content = function(file) {
      req(anova_results()$posthoc)
      
      # Simpan hasil post-hoc (yang sudah berbentuk tabel) ke .rds
      temp_dir <- tempdir()
      saveRDS(anova_results()$posthoc, file.path(temp_dir, "data.rds"))
      
      # Buat konten R Markdown
      konten_rmd <- paste(
        "---", "title: 'Hasil Uji Lanjutan Tukey HSD'", "output: word_document", "---",
        "```{r, echo=FALSE}", "print(readRDS('data.rds'))", "```", sep = "\n"
      )
      
      rmd_path <- file.path(temp_dir, "report.Rmd")
      writeLines(konten_rmd, rmd_path)
      rmarkdown::render(rmd_path, output_file = file, quiet = TRUE)
    }
  )
  
  
  # 4. DOWNLOAD HANDLER UNTUK INTERPRETASI LENGKAP (.docx)
  output$download_anova_interpretation <- downloadHandler(
    filename = function() {
      paste0("Interpretasi_ANOVA-", input$anova_num_var, ".docx")
    },
    content = function(file) {
      req(anova_results())
      res_list <- anova_results()
      
      # Ambil semua nilai yang dibutuhkan untuk interpretasi
      alpha <- as.numeric(input$anova_alpha)
      aov_summary <- summary(res_list$aov)
      f_value <- aov_summary[[1]]$`F value`[1]
      p_value <- aov_summary[[1]]$`Pr(>F)`[1]
      keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀")
      kesimpulan_utama <- ifelse(p_value < alpha,
                                 "terdapat cukup bukti untuk menyatakan bahwa **setidaknya ada satu kelompok yang memiliki rata-rata yang berbeda secara signifikan** dari kelompok lainnya.",
                                 "tidak ada cukup bukti untuk menyatakan adanya perbedaan rata-rata yang signifikan di antara kelompok-kelompok yang diuji.")
      
      # Buat konten interpretasi dalam format Markdown
      interpretation_text <- c(
        "---",
        "title: 'Laporan Interpretasi ANOVA'",
        "output: word_document",
        "---",
        "",
        "## Langkah-langkah Interpretasi ANOVA:",
        "",
        "**1. Hipotesis:**",
        "* **H₀ (Hipotesis Nol):** Rata-rata semua kelompok adalah sama (μ₁ = μ₂ = ... = μk).",
        "* **H₁ (Hipotesis Alternatif):** Setidaknya ada satu rata-rata kelompok yang berbeda.",
        "",
        paste0("**2. Tingkat Signifikansi (α):** ", alpha),
        "",
        "**3. Aturan Keputusan:** Jika p-value < α, maka H₀ ditolak.",
        "",
        "***",
        "",
        "## Hasil Analisis:",
        paste0("Berdasarkan analisis varians, diperoleh statistik uji F sebesar **", round(f_value, 3), "** dengan nilai p-value sebesar **", format.pval(p_value, digits = 4), "**."),
        paste0("Karena p-value (", format.pval(p_value, digits = 4), ") ", ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), maka keputusannya adalah **", keputusan, "**."),
        "",
        "## Kesimpulan:",
        paste0("Pada tingkat signifikansi ", alpha * 100, "%, ", kesimpulan_utama)
      )
      
      # Tambahkan interpretasi post-hoc jika ANOVA signifikan
      if (p_value < alpha) {
        kesimpulan_posthoc <- c(
          "",
          "***",
          "## Interpretasi Uji Lanjutan (Tukey HSD)",
          paste0("Karena hasil ANOVA signifikan (H₀ ditolak), kita melihat uji Tukey HSD untuk mengetahui pasangan kelompok mana yang berbeda secara signifikan. Pasangan-pasangan dengan nilai **p adj < ", alpha, "** pada tabel di tab 'Uji Lanjutan (Post-Hoc)' adalah yang rata-ratanya berbeda secara nyata.")
        )
        interpretation_text <- c(interpretation_text, kesimpulan_posthoc)
      }
      
      # Buat dan render file R Markdown
      rmd_path <- file.path(tempdir(), "interpretasi_anova.Rmd")
      writeLines(paste(interpretation_text, collapse="\n"), rmd_path)
      rmarkdown::render(rmd_path, output_file = file, quiet = TRUE)
    }
  )
  
  # --- LOGIKA UNDUH HALAMAN LENGKAP ANOVA ---
  
  # 1. OBSERVER UNTUK MENAMPILKAN POP-UP (MODAL)
  # GANTI BLOK observeEvent ANDA DENGAN VERSI DIAGNOSTIK INI
  
  observeEvent(input$download_anova_page, {
    # --- BLOK DIAGNOSTIK ---
    # Pesan ini HARUS muncul di konsol R Anda setiap kali tombol diklik.
    # Jika ini tidak muncul, ada masalah di file UI (ID tombol salah).
    message("\n--- Tombol 'Unduh Halaman Ini (ZIP)' ANOVA diklik ---")
    
    # Kita akan cek nilai anova_results() secara manual
    res_list <- anova_results()
    
    if (is.null(res_list)) {
      message("HASIL DIAGNOSIS: anova_results() saat ini NULL.")
    } else {
      message("HASIL DIAGNOSIS: anova_results() TIDAK NULL.")
      if (!is.null(res_list$error)) {
        message("HASIL DIAGNOSIS: anova_results() berisi pesan error.")
      } else {
        message("HASIL DIAGNOSIS: anova_results() valid dan tidak berisi error.")
      }
    }
    
    # --- LOGIKA ASLI (DENGAN req()) ---
    req(anova_results())
    
    # Pesan ini hanya akan muncul jika req() berhasil dilewati
    message("DIAGNOSIS: Baris setelah req(anova_results()) berhasil dieksekusi.")
    
    res_list_after_req <- anova_results() # Ambil lagi setelah req
    
    if (!is.null(res_list_after_req$error)) {
      message("DIAGNOSIS: Menjalankan notifikasi error.")
      showNotification(
        "Tidak bisa membuat laporan karena analisis ANOVA terakhir menghasilkan error.",
        type = "error"
      )
    } else {
      message("DIAGNOSIS: Menjalankan pop-up (showModal).")
      showModal(modalDialog(
        title = "Konfirmasi Unduhan Laporan ANOVA",
        p("Anda akan mengunduh file ZIP yang berisi:"),
        tags$ul(
          tags$li(strong("Box_Plot_ANOVA.png")),
          tags$li(strong("Hasil_ANOVA_Utama.docx")),
          tags$li(strong("Hasil_Uji_Lanjutan_Tukey.docx")),
          tags$li(strong("Laporan_Interpretasi.docx"))
        ),
        footer = tagList(
          modalButton("Batal"),
          downloadButton("confirm_download_anova", "Ya, Lanjutkan Unduh")
        ),
        easyClose = TRUE
      ))
    }
  })
  
  
  # 2. DOWNLOAD HANDLER UNTUK MEMBUAT FILE ZIP
  output$confirm_download_anova <- downloadHandler(
    filename = function() {
      paste0("Laporan_Lengkap_ANOVA-", input$anova_num_var, "-", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(anova_results())
      res_list <- anova_results()
      
      # Setup direktori sementara
      temp_dir <- file.path(tempdir(), "anova_report")
      dir.create(temp_dir, showWarnings = FALSE)
      
      # --- File 1: Simpan Box Plot (.png) ---
      plot_path <- file.path(temp_dir, "Box_Plot_ANOVA.png")
      ggsave(plot_path, plot = anova_plot_object(), device = "png", width = 8, height = 6, dpi = 300)
      
      # --- File 2: Simpan Tabel ANOVA Utama (.docx) ---
      aov_tidy <- broom::tidy(res_list$aov)
      saveRDS(aov_tidy, file.path(temp_dir, "aov.rds"))
      rmd_aov <- "--- \n title: 'Tabel Ringkasan ANOVA' \n output: word_document \n --- \n ```{r, echo=FALSE} \n library(knitr) \n kable(readRDS('aov.rds')) \n ```"
      path_aov_rmd <- file.path(temp_dir, "aov.Rmd")
      writeLines(rmd_aov, path_aov_rmd)
      rmarkdown::render(path_aov_rmd, output_file = "Hasil_ANOVA_Utama.docx", output_dir = temp_dir, quiet = TRUE)
      
      # --- File 3: Simpan Hasil Uji Lanjutan Tukey HSD (.docx) ---
      saveRDS(res_list$posthoc, file.path(temp_dir, "posthoc.rds"))
      rmd_posthoc <- "--- \n title: 'Hasil Uji Lanjutan (Tukey HSD)' \n output: word_document \n --- \n ```{r, echo=FALSE} \n print(readRDS('posthoc.rds')) \n ```"
      path_posthoc_rmd <- file.path(temp_dir, "posthoc.Rmd")
      writeLines(rmd_posthoc, path_posthoc_rmd)
      rmarkdown::render(path_posthoc_rmd, output_file = "Hasil_Uji_Lanjutan_Tukey.docx", output_dir = temp_dir, quiet = TRUE)
      
      # --- File 4: Simpan Laporan Interpretasi Lengkap (.docx) ---
      # Menggunakan kembali logika dari download interpretasi tunggal
      alpha <- as.numeric(input$anova_alpha)
      aov_summary <- summary(res_list$aov)
      f_value <- aov_summary[[1]]$`F value`[1]; p_value <- aov_summary[[1]]$`Pr(>F)`[1]
      keputusan <- ifelse(p_value < alpha, "TOLAK H₀", "GAGAL TOLAK H₀")
      kesimpulan_utama <- ifelse(p_value < alpha, "terdapat cukup bukti untuk menyatakan bahwa **setidaknya ada satu kelompok yang memiliki rata-rata yang berbeda secara signifikan** dari kelompok lainnya.", "tidak ada cukup bukti untuk menyatakan adanya perbedaan rata-rata yang signifikan di antara kelompok-kelompok yang diuji.")
      interpretation_text <- c("---", "title: 'Laporan Interpretasi ANOVA'","output: word_document","---", "## Langkah-langkah Interpretasi ANOVA:", "**1. Hipotesis:**", "* **H₀:** Rata-rata semua kelompok adalah sama.", "* **H₁:** Setidaknya ada satu rata-rata kelompok yang berbeda.", "", paste0("**2. Tingkat Signifikansi (α):** ", alpha), "**3. Aturan Keputusan:** Jika p-value < α, maka H₀ ditolak.", "***", "## Hasil Analisis:", paste0("Diperoleh statistik uji F sebesar **", round(f_value, 3), "** dengan p-value = **", format.pval(p_value, digits = 4), "**."), paste0("Karena p-value ", ifelse(p_value < alpha, "<", "≥"), " α, keputusannya adalah **", keputusan, "**."), "## Kesimpulan:", paste0("Pada tingkat signifikansi ", alpha * 100, "%, ", kesimpulan_utama))
      if (p_value < alpha) {
        kesimpulan_posthoc <- c("", "***", "## Interpretasi Uji Lanjutan (Tukey HSD)", paste0("Karena hasil ANOVA signifikan, uji Tukey HSD menunjukkan bahwa pasangan kelompok dengan nilai **p adj < ", alpha, "** pada tabel 'Uji Lanjutan (Post-Hoc)' memiliki rata-rata yang berbeda secara nyata."))
        interpretation_text <- c(interpretation_text, kesimpulan_posthoc)
      }
      path_interp_rmd <- file.path(temp_dir, "interpretasi_anova.Rmd")
      writeLines(paste(interpretation_text, collapse="\n"), path_interp_rmd)
      rmarkdown::render(path_interp_rmd, output_file = "Laporan_Interpretasi.docx", output_dir = temp_dir, quiet = TRUE)
      
      # --- Zip semua file yang telah dibuat ---
      files_to_zip <- c("Box_Plot_ANOVA.png", "Hasil_ANOVA_Utama.docx", "Hasil_Uji_Lanjutan_Tukey.docx", "Laporan_Interpretasi.docx")
      zip::zipr(zipfile = file, files = files_to_zip, root = temp_dir)
    }
  )
  
  # --- LOGIKA REGRESI LINEAR ---
  regression_results <- eventReactive(input$run_regression, {
    req(input$reg_dep_var, input$reg_indep_vars)
    if (length(input$reg_indep_vars) < 1) {
      return(list(error = "Gagal: Harap pilih minimal satu variabel independen (X)."))
    }
    if (input$reg_dep_var %in% input$reg_indep_vars) {
      return(list(error = "Gagal: Variabel dependen (Y) tidak boleh sama dengan variabel independen (X)."))
    }
    
    formula_str <- paste(input$reg_dep_var, "~", paste(input$reg_indep_vars, collapse = " + "))
    formula <- as.formula(formula_str)
    
    lm_model <- lm(formula, data = rv$data)
    
    norm_test <- shapiro.test(residuals(lm_model))
    hetero_test <- bptest(lm_model)
    autocor_test <- dwtest(lm_model)
    
    vif_test <- NULL
    if (length(input$reg_indep_vars) > 1) {
      vif_test <- vif(lm_model)
    }
    
    return(list(
      model = lm_model,
      summary = summary(lm_model),
      normality = norm_test,
      hetero = hetero_test,
      autocor = autocor_test,
      vif = vif_test,
      error = NULL
    ))
  })
  
  output$reg_summary_output <- renderPrint({
    res <- regression_results()
    if (!is.null(res$error)) cat(res$error) else res$summary
  })
  
  output$reg_normality_test <- renderPrint({ regression_results()$normality })
  output$reg_hetero_test <- renderPrint({ regression_results()$hetero })
  output$reg_autocor_test <- renderPrint({ regression_results()$autocor })
  
  output$reg_vif_test <- renderPrint({
    res <- regression_results()
    if (is.null(res$vif)) {
      cat("VIF tidak dapat dihitung karena hanya ada satu variabel independen.")
    } else {
      res$vif
    }
  })
  
  output$reg_plot_residuals_fitted <- renderPlot({
    res <- regression_results()
    req(is.null(res$error))
    plot(res$model, 1, main = "Residuals vs Fitted")
  })
  
  output$reg_plot_qq <- renderPlot({
    res <- regression_results()
    req(is.null(res$error))
    plot(res$model, 2, main = "Normal Q-Q")
  })
  
  output$reg_interpretation <- renderUI({
    res <- regression_results(); req(is.null(res$error))
    alpha <- as.numeric(input$reg_alpha)
    
    f_stat <- res$summary$fstatistic
    f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    f_keputusan <- ifelse(f_p_value < alpha, "SIGNIFIKAN", "TIDAK SIGNIFIKAN")
    f_interp <- paste0("Nilai p-value dari F-statistik (<b>", format.pval(f_p_value, digits=4), "</b>) lebih kecil dari α (", alpha, "), maka model secara keseluruhan <b>", f_keputusan, "</b> dalam menjelaskan variabel dependen.")
    
    r2_interp <- paste0("Nilai <b>Adjusted R-squared</b> sebesar <b>", round(res$summary$adj.r.squared, 4), "</b>. Ini berarti sekitar <b>", round(res$summary$adj.r.squared*100, 2), "%</b> variasi pada variabel dependen '", input$reg_dep_var, "' dapat dijelaskan oleh variabel-variabel independen dalam model.")
    
    coef_summary <- res$summary$coefficients
    coef_interp <- "<ul>"
    for (i in 1:nrow(coef_summary)) {
      var_name <- rownames(coef_summary)[i]
      p_val <- coef_summary[i, 4]
      signifikansi <- ifelse(p_val < alpha, "<b>berpengaruh signifikan</b>", "tidak berpengaruh signifikan")
      coef_interp <- paste0(coef_interp, "<li><b>", var_name, "</b>: p-value = ", round(p_val, 4), ". Variabel ini ", signifikansi, " terhadap '", input$reg_dep_var, "'.</li>")
    }
    coef_interp <- paste0(coef_interp, "</ul>")
    
    norm_p <- res$normality$p.value
    norm_interp <- ifelse(norm_p >= alpha, "Terpenuhi (residual berdistribusi normal)", "<b>Tidak Terpenuhi</b> (residual tidak berdistribusi normal)")
    
    hetero_p <- res$hetero$p.value
    hetero_interp <- ifelse(hetero_p >= alpha, "Terpenuhi (tidak ada masalah heteroskedastisitas/homoskedastis)", "<b>Tidak Terpenuhi</b> (terdapat masalah heteroskedastisitas)")
    
    autocor_p <- res$autocor$p.value
    autocor_interp <- ifelse(autocor_p >= alpha, "Terpenuhi (tidak ada masalah autokorelasi)", "<b>Tidak Terpenuhi</b> (terdapat masalah autokorelasi)")
    
    vif_interp <- "<ul>"
    if (is.null(res$vif)) {
      vif_interp <- "<li>Tidak dapat diuji (hanya satu prediktor).</li>"
    } else {
      for (i in 1:length(res$vif)) {
        vif_val <- res$vif[i]
        masalah <- ifelse(vif_val > 10, "<b>(Ada indikasi multikolinearitas kuat)</b>", "(Aman)")
        vif_interp <- paste0(vif_interp, "<li><b>", names(res$vif)[i], "</b>: VIF = ", round(vif_val, 3), " ", masalah, "</li>")
      }
    }
    vif_interp <- paste0(vif_interp, "</ul>")
    
    HTML(paste0(
      "<h4>1. Evaluasi Model</h4>",
      "<p><b>a. Kelayakan Model (Uji F):</b> ", f_interp, "</p>",
      "<p><b>b. Koefisien Determinasi (Kemampuan Menjelaskan):</b> ", r2_interp, "</p>",
      "<p><b>c. Pengaruh Variabel Individual (Uji t):</b>", coef_interp, "</p>",
      "<hr>",
      "<h4>2. Evaluasi Asumsi Klasik</h4>",
      "<p><b>a. Normalitas Residual:</b> p-value = ", round(norm_p, 4), ". Kesimpulan: <b>", norm_interp, "</b>.</p>",
      "<p><b>b. Homoskedastisitas (Varians Residual Konstan):</b> p-value = ", round(hetero_p, 4), ". Kesimpulan: <b>", hetero_interp, "</b>.</p>",
      "<p><b>c. Autokorelasi (Independensi Residual):</b> p-value = ", round(autocor_p, 4), ". Kesimpulan: <b>", autocor_interp, "</b>.</p>",
      "<p><b>d. Multikolinearitas (Hubungan Antar Variabel X):</b><br>", vif_interp, "</p>",
      "<hr>",
      "<h4>Kesimpulan Akhir</h4>",
      "<p>Evaluasi menyeluruh dari model dan asumsinya menunjukkan apakah model regresi yang dibangun valid secara statistik dan dapat diandalkan untuk interpretasi atau prediksi. Jika ada asumsi yang tidak terpenuhi, pertimbangkan untuk melakukan transformasi data atau menggunakan metode regresi yang lebih robust.</p>"
    ))
  })
  
  # --- LOGIKA DOWNLOAD UNTUK REGRESI LINEAR ---
  
  # 1. DOWNLOAD HANDLER UNTUK RINGKASAN MODEL (.docx)
  output$download_reg_summary <- downloadHandler(
    filename = function() {
      paste0("Ringkasan_Model_Regresi-", input$reg_dep_var, ".docx")
    },
    content = function(file) {
      req(regression_results())
      res <- regression_results()
      
      # Gunakan broom untuk merapikan output
      tidy_df <- broom::tidy(res$model)
      glance_df <- broom::glance(res$model)
      
      temp_dir <- tempdir()
      saveRDS(list(tidy = tidy_df, glance = glance_df), file.path(temp_dir, "reg_summary.rds"))
      
      konten_rmd <- paste(
        "---", "title: 'Ringkasan Model Regresi'", "output: word_document", "---",
        "## Ringkasan Koefisien (Uji t)", "```{r, echo=FALSE}", "library(knitr)", "data <- readRDS('reg_summary.rds')", "kable(data$tidy, digits=4, caption='Tabel Koefisien')", "```",
        "## Ringkasan Kebaikan Model (Goodness of Fit)", "```{r, echo=FALSE}", "kable(data$glance, digits=4, caption='Statistik Model')", "```",
        sep = "\n"
      )
      
      rmd_path <- file.path(temp_dir, "summary.Rmd")
      writeLines(konten_rmd, rmd_path)
      rmarkdown::render(rmd_path, output_file = file, quiet = TRUE)
    }
  )
  
  # 2. DOWNLOAD HANDLER UNTUK UJI ASUMSI KLASIK (.docx)
  output$download_reg_assumptions <- downloadHandler(
    filename = function() {
      paste0("Uji_Asumsi_Regresi-", input$reg_dep_var, ".docx")
    },
    content = function(file) {
      req(regression_results())
      res <- regression_results()
      
      temp_dir <- tempdir()
      rds_path <- file.path(temp_dir, "reg_asumsi.rds")
      saveRDS(res, rds_path)
      
      # --- PERBAIKAN: Ubah backslash menjadi forward slash ---
      safe_rds_path <- gsub("\\\\", "/", rds_path)
      
      # Membuat konten R Markdown dengan cara yang lebih aman
      konten_vector <- c(
        "---",
        "title: 'Hasil Uji Asumsi Klasik'",
        "output: word_document",
        "---",
        "",
        "## 1. Uji Normalitas Residual (Shapiro-Wilk)",
        "```{r, echo=FALSE}",
        paste0("res <- readRDS('", safe_rds_path, "')"), # Gunakan path yang aman
        "print(res$normality)",
        "```",
        "",
        "## 2. Uji Homoskedastisitas (Breusch-Pagan)",
        "```{r, echo=FALSE}",
        paste0("res <- readRDS('", safe_rds_path, "')"), # Gunakan path yang aman
        "print(res$hetero)",
        "```",
        "",
        "## 3. Uji Autokorelasi (Durbin-Watson)",
        "```{r, echo=FALSE}",
        paste0("res <- readRDS('", safe_rds_path, "')"), # Gunakan path yang aman
        "print(res$autocor)",
        "```",
        ""
      )
      
      # Tambahkan bagian VIF secara kondisional
      if (!is.null(res$vif)) {
        vif_text <- c(
          "## 4. Uji Multikolinearitas (VIF)",
          "```{r, echo=FALSE}",
          paste0("res <- readRDS('", safe_rds_path, "')"), # Gunakan path yang aman
          "print(res$vif)",
          "```"
        )
        konten_vector <- c(konten_vector, vif_text)
      } else {
        vif_text <- c(
          "## 4. Uji Multikolinearitas (VIF)",
          "VIF tidak dihitung (hanya 1 variabel independen)."
        )
        konten_vector <- c(konten_vector, vif_text)
      }
      
      konten_rmd <- paste(konten_vector, collapse = "\n")
      
      rmd_path <- file.path(temp_dir, "asumsi.Rmd")
      writeLines(konten_rmd, rmd_path)
      rmarkdown::render(rmd_path, output_file = file, quiet = TRUE)
    }
  )
  
  # 3. DOWNLOAD HANDLER UNTUK VISUALISASI RESIDUAL (.png)
  output$download_reg_plots <- downloadHandler(
    filename = function() {
      paste0("Visualisasi_Residual-", input$reg_dep_var, ".png")
    },
    content = function(file) {
      req(regression_results())
      res <- regression_results()
      
      # Buat file PNG, atur layout 1 baris 2 kolom
      png(file, width = 1000, height = 500, res = 120)
      par(mfrow = c(1, 2))
      
      # Gambar kembali kedua plot
      plot(res$model, 1, main = "Residuals vs Fitted")
      plot(res$model, 2, main = "Normal Q-Q")
      
      # Tutup perangkat PNG
      dev.off()
    }
  )
  
  # 4. DOWNLOAD HANDLER UNTUK INTERPRETASI LENGKAP (.docx)
  output$download_reg_interpretation <- downloadHandler(
    filename = function() {
      paste0("Interpretasi_Regresi-", input$reg_dep_var, ".docx")
    },
    content = function(file) {
      req(regression_results())
      res <- regression_results()
      alpha <- as.numeric(input$reg_alpha)
      
      # --- Kumpulkan semua teks interpretasi (logika disalin dari UI Anda) ---
      f_stat <- res$summary$fstatistic; f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
      f_interp <- paste0("Nilai p-value dari F-statistik (**", format.pval(f_p_value, digits=4), "**) lebih kecil dari α (", alpha, "), maka model secara keseluruhan **", ifelse(f_p_value < alpha, "SIGNIFIKAN", "TIDAK SIGNIFIKAN"), "** dalam menjelaskan variabel dependen.")
      
      r2_interp <- paste0("Nilai **Adjusted R-squared** sebesar **", round(res$summary$adj.r.squared, 4), "**. Ini berarti sekitar **", round(res$summary$adj.r.squared*100, 2), "%** variasi pada variabel dependen '", input$reg_dep_var, "' dapat dijelaskan oleh variabel-variabel independen dalam model.")
      
      coef_summary <- res$summary$coefficients
      coef_interp_md <- ""
      for (i in 1:nrow(coef_summary)) {
        var_name <- rownames(coef_summary)[i]; p_val <- coef_summary[i, 4]
        signifikansi <- ifelse(p_val < alpha, "**berpengaruh signifikan**", "tidak berpengaruh signifikan")
        coef_interp_md <- paste0(coef_interp_md, "* **", var_name, "**: p-value = ", round(p_val, 4), ". Variabel ini ", signifikansi, " terhadap '", input$reg_dep_var, "'.\n")
      }
      
      norm_p <- res$normality$p.value; hetero_p <- res$hetero$p.value; autocor_p <- res$autocor$p.value
      norm_interp <- ifelse(norm_p >= alpha, "Terpenuhi (residual berdistribusi normal)", "**Tidak Terpenuhi** (residual tidak berdistribusi normal)")
      hetero_interp <- ifelse(hetero_p >= alpha, "Terpenuhi (tidak ada masalah heteroskedastisitas/homoskedastis)", "**Tidak Terpenuhi** (terdapat masalah heteroskedastisitas)")
      autocor_interp <- ifelse(autocor_p >= alpha, "Terpenuhi (tidak ada masalah autokorelasi)", "**Tidak Terpenuhi** (terdapat masalah autokorelasi)")
      
      vif_interp_md <- ""
      if (is.null(res$vif)) { vif_interp_md <- "* Tidak dapat diuji (hanya satu prediktor).\n" } else {
        for (i in 1:length(res$vif)) {
          vif_val <- res$vif[i]; masalah <- ifelse(vif_val > 10, "**(Ada indikasi multikolinearitas kuat)**", "(Aman)")
          vif_interp_md <- paste0(vif_interp_md, "* **", names(res$vif)[i], "**: VIF = ", round(vif_val, 3), " ", masalah, "\n")
        }
      }
      
      # --- Gabungkan semua teks menjadi format R Markdown ---
      konten_rmd <- paste(
        "---", "title: 'Laporan Interpretasi Lengkap Regresi Linear'", "output: word_document", "---",
        "## 1. Evaluasi Model",
        "**a. Kelayakan Model (Uji F):**", f_interp,
        "**b. Koefisien Determinasi (Kemampuan Menjelaskan):**", r2_interp,
        "**c. Pengaruh Variabel Individual (Uji t):**", coef_interp_md,
        "***",
        "## 2. Evaluasi Asumsi Klasik",
        paste0("**a. Normalitas Residual:** p-value = ", round(norm_p, 4), ". Kesimpulan: **", norm_interp, "**."),
        paste0("**b. Homoskedastisitas (Varians Residual Konstan):** p-value = ", round(hetero_p, 4), ". Kesimpulan: **", hetero_interp, "**."),
        paste0("**c. Autokorelasi (Independensi Residual):** p-value = ", round(autocor_p, 4), ". Kesimpulan: **", autocor_interp, "**."),
        "**d. Multikolinearitas (Hubungan Antar Variabel X):**", vif_interp_md,
        "***",
        "## Kesimpulan Akhir",
        "Evaluasi menyeluruh dari model dan asumsinya menunjukkan apakah model regresi yang dibangun valid secara statistik dan dapat diandalkan untuk interpretasi atau prediksi. Jika ada asumsi yang tidak terpenuhi, pertimbangkan untuk melakukan transformasi data atau menggunakan metode regresi yang lebih robust.",
        sep = "\n\n"
      )
      
      rmd_path <- file.path(tempdir(), "interpretasi_reg.Rmd")
      writeLines(konten_rmd, rmd_path)
      rmarkdown::render(rmd_path, output_file = file, quiet = TRUE)
    }
  )
  
  # --- LOGIKA UNDUH HALAMAN LENGKAP REGRESI ---
  
  # 1. OBSERVER UNTUK MENAMPILKAN POP-UP (MODAL)
  observeEvent(input$download_regr_page, {
    # Pastikan analisis regresi sudah dijalankan dan valid
    req(regression_results())
    res_list <- regression_results()
    
    if (!is.null(res_list$error)) {
      showNotification("Tidak bisa membuat laporan karena analisis terakhir menghasilkan error.", type = "error")
    } else {
      # Tampilkan modal konfirmasi
      showModal(modalDialog(
        title = "Konfirmasi Unduhan Laporan Regresi",
        p("Anda akan mengunduh file ZIP yang berisi:"),
        tags$ul(
          tags$li(strong("Ringkasan_Model.docx")),
          tags$li(strong("Uji_Asumsi_Klasik.docx")),
          tags$li(strong("Visualisasi_Residual.png")),
          tags$li(strong("Laporan_Interpretasi.docx"))
        ),
        footer = tagList(
          modalButton("Batal"),
          downloadButton("confirm_download_regr", "Ya, Lanjutkan Unduh")
        ),
        easyClose = TRUE
      ))
    }
  })
  
  # 2. DOWNLOAD HANDLER UNTUK MEMBUAT FILE ZIP
  output$confirm_download_regr <- downloadHandler(
    filename = function() {
      paste0("Laporan_Lengkap_Regresi-", input$reg_dep_var, "-", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(regression_results())
      res <- regression_results()
      
      temp_dir <- file.path(tempdir(), "regr_report")
      dir.create(temp_dir, showWarnings = FALSE)
      
      # --- File 1: Ringkasan Model (.docx) ---
      saveRDS(list(tidy = broom::tidy(res$model), glance = broom::glance(res$model)), file.path(temp_dir, "reg_summary.rds"))
      rmd_summary <- "--- \n title: 'Ringkasan Model Regresi' \n output: word_document \n --- \n ## Koefisien \n ```{r, echo=FALSE} \n library(knitr) \n data <- readRDS('reg_summary.rds') \n kable(data$tidy) \n ``` \n ## Kebaikan Model \n ```{r, echo=FALSE} \n kable(data$glance) \n ```"
      path_summary_rmd <- file.path(temp_dir, "summary.Rmd")
      writeLines(rmd_summary, path_summary_rmd)
      rmarkdown::render(path_summary_rmd, output_file = "Ringkasan_Model.docx", output_dir = temp_dir, quiet = TRUE)
      
      # --- File 2: Uji Asumsi Klasik (.docx) ---
      saveRDS(res, file.path(temp_dir, "reg_asumsi.rds"))
      vif_chunk <- if (!is.null(res$vif)) {"## VIF \n ```{r, echo=FALSE} \n res <- readRDS('reg_asumsi.rds') \n print(res$vif) \n ```"} else {"## VIF \n Tidak dihitung."}
      # PERBAIKAN: Menggunakan path relatif 'reg_asumsi.rds' di dalam Rmd
      rmd_asumsi <- paste("--- \n title: 'Uji Asumsi Klasik' \n output: word_document \n ---",
                          "## Normalitas \n ```{r, echo=FALSE} \n res <- readRDS('reg_asumsi.rds') \n print(res$normality) \n ```",
                          "## Homoskedastisitas \n ```{r, echo=FALSE} \n res <- readRDS('reg_asumsi.rds') \n print(res$hetero) \n ```",
                          "## Autokorelasi \n ```{r, echo=FALSE} \n res <- readRDS('reg_asumsi.rds') \n print(res$autocor) \n ```",
                          vif_chunk, sep="\n")
      path_asumsi_rmd <- file.path(temp_dir, "asumsi.Rmd")
      writeLines(rmd_asumsi, path_asumsi_rmd)
      rmarkdown::render(path_asumsi_rmd, output_file = "Uji_Asumsi_Klasik.docx", output_dir = temp_dir, quiet = TRUE)
      
      # --- File 3: Visualisasi Residual (.png) ---
      plot_path <- file.path(temp_dir, "Visualisasi_Residual.png")
      png(plot_path, width = 1000, height = 500, res = 120)
      par(mfrow = c(1, 2)); plot(res$model, 1); plot(res$model, 2)
      dev.off()
      
      # --- File 4: Interpretasi Lengkap (.docx) ---
      # Logika ini tidak membaca file, jadi tidak perlu diubah
      alpha <- as.numeric(input$reg_alpha)
      f_stat <- res$summary$fstatistic; f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
      f_interp <- paste0("Nilai p-value dari F-statistik (**", format.pval(f_p_value, digits=4), "**) lebih kecil dari α (", alpha, "), maka model secara keseluruhan **", ifelse(f_p_value < alpha, "SIGNIFIKAN", "TIDAK SIGNIFIKAN"), "** dalam menjelaskan variabel dependen.")
      r2_interp <- paste0("Nilai **Adjusted R-squared** sebesar **", round(res$summary$adj.r.squared, 4), "**. Ini berarti sekitar **", round(res$summary$adj.r.squared*100, 2), "%** variasi pada variabel dependen '", input$reg_dep_var, "' dapat dijelaskan oleh variabel-variabel independen dalam model.")
      coef_summary <- res$summary$coefficients; coef_interp_md <- ""
      for (i in 1:nrow(coef_summary)) { var_name <- rownames(coef_summary)[i]; p_val <- coef_summary[i, 4]; signifikansi <- ifelse(p_val < alpha, "**berpengaruh signifikan**", "tidak berpengaruh signifikan"); coef_interp_md <- paste0(coef_interp_md, "* **", var_name, "**: p-value = ", round(p_val, 4), ". Variabel ini ", signifikansi, " terhadap '", input$reg_dep_var, "'.\n") }
      norm_p <- res$normality$p.value; hetero_p <- res$hetero$p.value; autocor_p <- res$autocor$p.value
      norm_interp <- ifelse(norm_p >= alpha, "Terpenuhi", "**Tidak Terpenuhi**"); hetero_interp <- ifelse(hetero_p >= alpha, "Terpenuhi", "**Tidak Terpenuhi**"); autocor_interp <- ifelse(autocor_p >= alpha, "Terpenuhi", "**Tidak Terpenuhi**")
      vif_interp_md <- ""; if (is.null(res$vif)) { vif_interp_md <- "* Tidak dapat diuji.\n" } else { for (i in 1:length(res$vif)) { vif_val <- res$vif[i]; masalah <- ifelse(vif_val > 10, "**(Multikolinearitas Kuat)**", "(Aman)"); vif_interp_md <- paste0(vif_interp_md, "* **", names(res$vif)[i], "**: VIF = ", round(vif_val, 3), " ", masalah, "\n") } }
      
      kesimpulan_akhir <- "Evaluasi menyeluruh dari model dan asumsinya menunjukkan apakah model regresi yang dibangun valid secara statistik dan dapat diandalkan untuk interpretasi atau prediksi. Jika ada asumsi yang tidak terpenuhi, pertimbangkan untuk melakukan transformasi data atau menggunakan metode regresi yang lebih robust."
      rmd_interp <- paste("---", "title: 'Laporan Interpretasi Regresi'", "output: word_document", "---",
                          "## 1. Evaluasi Model","**a. Uji F:**", f_interp, "**b. R-squared:**", r2_interp, "**c. Uji t:**", coef_interp_md,
                          "***", "## 2. Evaluasi Asumsi Klasik", paste0("**a. Normalitas:** p-value = ", round(norm_p, 4), ". (", norm_interp, ")"), paste0("**b. Homoskedastisitas:** p-value = ", round(hetero_p, 4), ". (", hetero_interp, ")"), paste0("**c. Autokorelasi:** p-value = ", round(autocor_p, 4), ". (", autocor_interp, ")"), "**d. Multikolinearitas:**", vif_interp_md,
                          "***", "## Kesimpulan Akhir", "Evaluasi menyeluruh dari model dan asumsinya menunjukkan apakah model regresi valid dan dapat diandalkan.",
                          sep = "\n\n")
      path_interp_rmd <- file.path(temp_dir, "interpretasi_reg.Rmd")
      writeLines(rmd_interp, path_interp_rmd)
      rmarkdown::render(path_interp_rmd, output_file = "Laporan_Interpretasi.docx", output_dir = temp_dir, quiet = TRUE)
      
      # --- Zip semua file yang telah dibuat ---
      files_to_zip <- c("Ringkasan_Model.docx", "Uji_Asumsi_Klasik.docx", "Visualisasi_Residual.png", "Laporan_Interpretasi.docx")
      zip::zipr(zipfile = file, files = files_to_zip, root = temp_dir)
    }
  )
  
}


#==============================================================================
#                             JALANKAN APLIKASI
#==============================================================================
shinyApp(ui, server)
