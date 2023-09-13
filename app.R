#DATA CLEANING----
library(DT)
library(dplyr)
library(fpp2)
library(forecast)
library(ggplot2)
library(httr)
library(jsonlite)
library(lubridate)
library(openxlsx)
library(plotly)
library(readr)
library(readxl)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(TTR)
library(tidyr)
library(tibble)
library(tidyverse)
library(likert)
library(bslib)
library(dashboardthemes)
library(highcharter)
library(leaflet)

# Read the Excel file without specifying data types
data_baru <- read_excel("31August.xlsx", 
                 col_types = c("text", "numeric", "text", 
                               "text", "text", "text", "logical", 
                               "text", "text", "text", "text", 
                               "numeric", "text", "numeric", "text", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "text", "text", "text", "text", 
                               "text", "text", "text", "numeric", 
                               "text", "text", "numeric", "text", 
                               "text", "text", "text", "text", 
                               "text", "text", "text", "text", 
                               "text", "numeric", "text", "numeric", 
                               "text", "numeric", "text", "text", 
                               "text", "text", "text", "text", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "text", "text", 
                               "text", "text"))
# Select significant column
df<-data_baru[,c("KOMPAUN_NOMBOR", "KOMPAUN_TARIKH", "KOMPAUN_MASA", "KOMPAUN_AMAUN", "KOMPAUN_STATUS",
                "KOMPAUN_NAMA_ORANG","KOMPAUN_NAMA_PERNIAGAAN", "KESALAHAN_UNDANG2", "KESALAHAN_PERUNTUKAN",
                "KESALAHAN_KETERANGAN", "PINDAAN_TARIKH","PINDAAN_TINDAKAN", "PINDAAN_KETERANGAN","KOMPAUN_AMAUN_LAMA",
                "BAYARAN_AMAUN", "BAYARAN_TARIKH", "BAYARAN_SALURAN", "BAYARAN_STATUS","BAYARAN_BOLEH","TEMPAT_GPS_LONGTITUDE",
                "TEMPAT_GPS_LATITUDE", "TEMPAT_ZON_NAMA", "TEMPAT_KAWASAN_NAMA")]
str(df)
# Change datatype
df1 <- df %>%
  mutate(KOMPAUN_TARIKH = as.Date(df$KOMPAUN_TARIKH, format = "%Y-%m-%d"),
         PINDAAN_TARIKH = as.Date(df$PINDAAN_TARIKH, format = "%Y-%m-%d"),
         BAYARAN_TARIKH = as.Date(df$BAYARAN_TARIKH, format = "%Y-%m-%d"))

str(df1)


# Remove null value (KOMPAUN_NOMBOR)
# Fill null value in TEMPAT_KAWASAN_NAMA, TEMPAT_ZON_NAMA, PINDAAN_TINDAKAN
# Rename E-BILLING in BAYARAN_SALURAN
# Create new column (BAYARAN) with value (SELESAI/BELUM SELESAI)
# Create new column (PERNIAGAAN) with value KOMPAUN_NAMA_PERNIAGAAN
df1 <- df1 %>% 
  drop_na(KOMPAUN_NOMBOR)  %>% 
  mutate(PINDAAN_TARIKH = if_else(PINDAAN_TARIKH == as.Date("2048-04-30"), as.Date("2018-04-30"), PINDAAN_TARIKH),
         BAYARAN_TARIKH = if_else(BAYARAN_TARIKH == as.Date("2048-04-30"), as.Date("2018-04-30"), BAYARAN_TARIKH),
         TEMPAT_KAWASAN_NAMA = ifelse(is.na(TEMPAT_KAWASAN_NAMA), "TIADA REKOD", TEMPAT_KAWASAN_NAMA),
         TEMPAT_ZON_NAMA = ifelse(is.na(TEMPAT_ZON_NAMA), "TIADA REKOD", TEMPAT_ZON_NAMA),
         PINDAAN_TINDAKAN = ifelse(is.na(PINDAAN_TINDAKAN), "TIADA TINDAKAN", PINDAAN_TINDAKAN),
         BAYARAN_SALURAN = ifelse(BAYARAN_SALURAN %in% c("E-BILING", "EBILLING"), "E-BILLING", BAYARAN_SALURAN),
         BAYARAN = ifelse(BAYARAN_STATUS == "BAYAR" & is.na(BAYARAN_BOLEH), "SELESAI",
                          ifelse(is.na(BAYARAN_STATUS) & is.na(BAYARAN_BOLEH), "TIADA BAYARAN",
                                 ifelse(is.na(BAYARAN_STATUS) & BAYARAN_BOLEH == "Y", "BELUM SELESAI", NA))),
         TOTAL_BAYARAN = ifelse(BAYARAN=="SELESAI", BAYARAN_AMAUN,
                                ifelse(BAYARAN=="TIADA BAYARAN", "0", 
                                       ifelse(BAYARAN=="BELUM SELESAI", KOMPAUN_AMAUN, "null"))),
         TOTAL_BAYARAN = as.numeric(TOTAL_BAYARAN),
         PERNIAGAAN = ifelse(is.na(KOMPAUN_NAMA_PERNIAGAAN), KOMPAUN_NAMA_ORANG, KOMPAUN_NAMA_PERNIAGAAN),
         BULAN_NOMBOR = format(KOMPAUN_TARIKH, "%m"),
         TAHUN =format(KOMPAUN_TARIKH, "%Y"))


#X<-df1[,c("KOMPAUN_AMAUN","BAYARAN_AMAUN","TOTAL_BAYARAN","BAYARAN")]
#X<-unique(X)

# Create new column "KESALAHAN" based on column KESALAHAN_UNDANG2 
df1$KESALAHAN <- ifelse(df1$KESALAHAN_UNDANG2 == "AKTA JALAN, PARIT DAN BANGUNAN 1974 (AKTA 133)", "AKTA BANGUNAN", 
                        ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL TRED, PERNIAGAAN & PERINDUSTRIAN (MDHS) 2007", "UUK PERNIAGAAN", 
                               ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL PENJAJA (MDHS) 2007", "UUK PENJAJA", 
                                      ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL PUSAT SIBER & KAFE SIBER 2007", "UUK PUSAT SIBER", 
                                             ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL IKLAN 2007", "UUK IKLAN", 
                                                    ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL KERJA TANAH 2007", "UUK KERJA TANAH", 
                                                           ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL PENGENDALI MAKANAN 2007", "UUK PENGENDALIAN MAKANAN", 
                                                                  ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL PASAR 2007", "UUK PASAR", 
                                                                         ifelse(df1$KESALAHAN_UNDANG2 == "ENAKMEN MENGAWAL LEMBU-KERBAU 1971", "ENAKMEN LEMBU-KERBAU", 
                                                                                ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL PELESENAN TEMPAT LETAK KERETA PERSENDIRIAN 2005", "UUK PARKIR", 
                                                                                       ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL PUSAT SUKAN PERSENDIRIAN 2007", "UUK PUSAT SUKAN", 
                                                                                              ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL ESTABLISYMEN MAKANAN 2007", "UUK MAKANAN",
                                                                                                     ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL TAMAN 2005", "UUK TAMAN", 
                                                                                                            ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL PELESENAN ANJING & RUMAH PEMBIAKAN ANJING 2007", "PERLESENAN ANJING", 
                                                                                                                   ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL HOTEL 2013", "UUK HOTEL",
                                                                                                                          ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL HOTEL 2007", "UUK HOTEL", 
                                                                                                                                 ifelse(df1$KESALAHAN_UNDANG2 == "KESALAHAN SEKSYEN YANG BERKAITAN DENGAN AKTA KERAJAAN TEMPATAN 1976 (AKTA 171)", "AKTA KERAJAAN TEMPATAN", 
                                                                                                                                        ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL PENTERNAKAN BURUNG WALIT  & PERUSAHAAN SARANG BURUNG WALIT 2013", "PENTERNAKAN BURUNG WALIT", 
                                                                                                                                               ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL PEMUNGUTAN, PEMBUANGAN & PELUPUSAN SAMPAH 2007", "UUK PELUPUSAN SAMPAH", 
                                                                                                                                                      ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL PUSAT KECANTIKAN & PENJAGAAN KESIHATAN 2007", "UUK PUSAT KECANTIKAN",
                                                                                                                                                             ifelse(df1$KESALAHAN_UNDANG2 == "UNDANG-UNDANG KECIL VANDALISME 2005", "UUK VANDALISME",
                                                                                                                                                                  ifelse(df1$KESALAHAN_UNDANG2 == "", "TIADA REKOD",
                                                                                                                                                                      ifelse(df1$KESALAHAN_UNDANG2 == "ENAKMEN HIBURAN & TEMPAT HIBURAN (PINDAAN) 1995", "ENAKMEN HIBURAN", "TIADA REKOD")))))))))))))))))))))))
#new df for current year only
start_date <- as.Date("2023-01-01")
end_date <- Sys.Date()
df3 <- subset(df1, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
str(df1)
df_baru <- subset(df1, is.na(KESALAHAN))
unique(df1$KESALAHAN)

# write_xlsx(df1, path = "C:/Users/Admin/Downloads/MPHSDATA.xlsx")

ui <- shinyUI(
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(tags$head(
      tags$style(HTML("
        .main-sidebar { width: 0%; }
        .content-wrapper { margin-left: 0%; margin-top: 0%}
      "))
    ),
    div(style = "visibility: hidden;", # Hide the collapse button
        tags$head(
          tags$style(HTML("
          .sidebar-toggle { display: none; }
        "))
        )
    )),
    dashboardBody(
      ### creating custom theme object
      customTheme <- shinyDashboardThemeDIY(
        
        ### general
        appFontFamily = "Arial"
        ,appFontColor = "rgb(0,0,0)"
        ,primaryFontColor = "rgb(0,0,0)"
        ,infoFontColor = "rgb(0,0,0)"
        ,successFontColor = "rgb(0,0,0)"
        ,warningFontColor = "rgb(0,0,0)"
        ,dangerFontColor = "rgb(0,0,0)"
        ,bodyBackColor = "rgb(255, 253, 250)"
        
        ### header
        ,logoBackColor = "rgb(255, 253, 250)"
        
        ,headerButtonBackColor = "rgb(6, 1, 26)"
        ,headerButtonIconColor = "rgb(75,75,75)"
        ,headerButtonBackColorHover = "rgb(210,210,210)"
        ,headerButtonIconColorHover = "rgb(0,0,0)"
        
        ,headerBackColor = "rgb(255, 253, 250)"
        ,headerBoxShadowColor = "#33403e"
        ,headerBoxShadowSize = "0px 0px 0px"
        
        ### sidebar
        ,sidebarBackColor = cssGradientThreeColors(
          direction = "down"
          ,colorStart = "rgb(6, 1, 26)"
          ,colorMiddle = "rgb(56,161,187)"
          ,colorEnd = "rgb(3,22,56)"
          ,colorStartPos = 0
          ,colorMiddlePos = 50
          ,colorEndPos = 100
        )
        ,sidebarPadding = 0
        
        ,sidebarMenuBackColor = "transparent"
        ,sidebarMenuPadding = 0
        ,sidebarMenuBorderRadius = 0
        
        ,sidebarShadowRadius = "3px 5px 5px"
        ,sidebarShadowColor = "#33403e"
        
        ,sidebarUserTextColor = "rgb(255,255,255)"
        
        ,sidebarSearchBackColor = "rgb(55,72,80)"
        ,sidebarSearchIconColor = "rgb(153,153,153)"
        ,sidebarSearchBorderColor = "rgb(55,72,80)"
        
        ,sidebarTabTextColor = "rgb(255,255,255)"
        ,sidebarTabTextSize = 13
        ,sidebarTabBorderStyle = "none none solid none"
        ,sidebarTabBorderColor = "rgb(35,106,135)"
        ,sidebarTabBorderWidth = 1
        
        ,sidebarTabBackColorSelected = cssGradientThreeColors(
          direction = "right"
          ,colorStart = "rgba(44,222,235,1)"
          ,colorMiddle = "rgba(44,222,235,1)"
          ,colorEnd = "rgba(0,255,213,1)"
          ,colorStartPos = 0
          ,colorMiddlePos = 30
          ,colorEndPos = 100
        )
        ,sidebarTabTextColorSelected = "rgb(0,0,0)"
        ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
        
        ,sidebarTabBackColorHover = cssGradientThreeColors(
          direction = "right"
          ,colorStart = "rgba(44,222,235,1)"
          ,colorMiddle = "rgba(44,222,235,1)"
          ,colorEnd = "rgba(0,255,213,1)"
          ,colorStartPos = 0
          ,colorMiddlePos = 30
          ,colorEndPos = 100
        )
        ,sidebarTabTextColorHover = "rgb(50,50,50)"
        ,sidebarTabBorderStyleHover = "none none solid none"
        ,sidebarTabBorderColorHover = "rgb(75,126,151)"
        ,sidebarTabBorderWidthHover = 1
        ,sidebarTabRadiusHover = "0px 20px 20px 0px"
        
        ### boxes
        ,boxBackColor = "linear-gradient(to bottom, #FFFDFA, #F8F8F8) !important"
        ,boxBorderRadius = 0
        ,boxShadowSize = "0px 5px 5px"
        ,boxShadowColor = "rgba(0, 0, 0, 0.2)"
        ,boxTitleSize = 16
        ,boxDefaultColor = "rgb(210,214,220)"
        ,boxPrimaryColor = "rgb(244, 243, 243)"
        ,boxInfoColor = "rgb(210,214,220)"
        ,boxSuccessColor = "rgba(0,255,213,1)"
        ,boxWarningColor = "rgb(244,156,104)"
        ,boxDangerColor = "rgb(255,88,55)"
        
        ,tabBoxTabColor = "rgb(255,255,255)"
        ,tabBoxTabTextSize = 14
        ,tabBoxTabTextColor = "rgb(0,0,0)"
        ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
        ,tabBoxBackColor = "rgb(255,255,255)"
        ,tabBoxHighlightColor = "rgb(6,1,26)"
        ,tabBoxBorderRadius = 1
        
        ### inputs
        ,buttonBackColor = "rgb(245,245,245)"
        ,buttonTextColor = "rgb(0,0,0)"
        ,buttonBorderColor = "rgb(200,200,200)"
        ,buttonBorderRadius = 5
        
        ,buttonBackColorHover = "rgb(235,235,235)"
        ,buttonTextColorHover = "rgb(100,100,100)"
        ,buttonBorderColorHover = "rgb(200,200,200)"
        
        ,textboxBackColor = "rgb(255,255,255)"
        ,textboxBorderColor = "rgb(200,200,200)"
        ,textboxBorderRadius = 5
        ,textboxBackColorSelect = "rgb(245,245,245)"
        ,textboxBorderColorSelect = "rgb(200,200,200)"
        
        ### tables
        ,tableBackColor = "rgb(255,255,255)"
        ,tableBorderColor = "rgb(240,240,240)"
        ,tableBorderTopSize = 1
        ,tableBorderRowSize = 1
        
      ),
      tags$head(
        tags$style(HTML("
                  /* Adjust content-wrapper margin to match new sidebar width */
                  .content-wrapper {
                    padding-left: 2%;
                    padding-right: 2%;
                  }
                   /* Customize page background color */
                  .content-wrapper {
                    
                    background: linear-gradient(to bottom, #FFFDFA, #F8F8F8) !important;
                  }
                 
                  /* Customize tab appearance */
                  .nav-tabs > li > a {
                    color: white;               /* Tab text color */
                    border-radius: 0 !important;
                  }
                  
                  .tab-content {
                    background: linear-gradient(to bottom, #FFFDFA, #F8F8F8) !important;
                  }
                  
                  /* Customize tab background color */
                  .nav-tabs {
                    background-color: #334bb7;  /* Tab background color */
                  }
                
                  /* Remove padding around tabBox */
                  .tabBox {
                    padding: 0;
                  }
    
                  /* Reduce margin around tabBox */
                  .tabBox > .tab-content {
                    margin-top: 0;
                    margin-left: 0;
                  }
                  
                  #my-container {
                  padding: 0px;
                  background-color: transparent;
                  width: 100%; /* Adjust the width as needed */
                  margin: 20px auto; /* Center the container horizontally */
                  
                  }
                  #my-container1 {
                    padding: 0px;
                    background-color: transparent;
                    width: 100%; /* Adjust the width as needed */
                    margin: 20px auto; /* Center the container horizontally */
                  }
                
                #my-table {
                  width: 100%;
                  border-collapse: collapse;
                  box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); /* Add box shadow */       
                }
                
                #my-table th {
                background-color: rgb(244, 243, 243);
                }
                
                #my-table th, #my-table td {
                  border: 1px solid #ccc;
                  padding: 10px;
                  text-align: left;
                }
                
                .label-and-input {
                  display: flex;
                  align-items: center;
                }
                
                .input-container {
                  margin-left: 10px;
                }
                
                .select-input {
                  width: 100%;
                  padding: 10px;
                  font-size: 14px;
                }
                
                #submit-btn {
                color: white; /* Set the text color to contrast with the background */
                padding: 10px 10px; /* Adjust padding as needed */
                border: none; /* Remove the default button border */
                border-radius: 2px; /* Add some border-radius for a rounded look */
                margin-top: 10px;
                text-align: right; /* Align the text to the right */
              }
                #tab-content>.tab-pane {
                  border-radius: 0;
                }
                
      
                "))
               ),
                fluidRow(
                  tabsetPanel(
                    tabPanel("LAPORAN KOMPAUN",
                             tags$div(id = "my-container",
                                      tags$table(id = "my-table",
                                                 tags$colgroup(
                                                   tags$col(width = "30%"),
                                                   tags$col(width = "30%"),
                                                   tags$col(width = "40%")
                                                 ),
                                                 tags$tr(
                                                   tags$th("Tajuk Laporan:"),
                                                   tags$th("Mengikut:"),
                                                   tags$th("Jenis Data:")
                                                 ),
                                                 tags$tr(
                                                   tags$td(
                                                     div(class = "label-and-input",
                                                         selectInput("LPR_TAJUK", "", 
                                                                     choices = c("Kompaun Dikeluarkan", "Kompaun Tunggakan (Belum Bayar)"))
                                                     )
                                                   ),
                                                   tags$td(
                                                     div(class = "label-and-input",
                                                         selectInput("MENGIKUT", "", 
                                                                     choices = c("Kesalahan", "Tempat - Zon", "Tempat - Kawasan"))
                                                     )
                                                   ),
                                                   tags$td(
                                                     div(class = "radio-custom radio-default radio-inline",
                                                         radioButtons("JENIS_DATA", label = NULL,
                                                                      choices = c("Tahunan", "Bulanan", "Harian"),
                                                                      selected = "Harian"))
                                                   )
                                                 )
                                      ),
                                      
                                      tags$table(id = "my-table",
                                                 tags$colgroup(
                                                   tags$col(width = "50%"),
                                                   tags$col(width = "50%")
                                                 ),
                                                 tags$tr(
                                                   tags$th("Tarikh Mula:"),
                                                   tags$th("Tarikh Akhir:")
                                                 ),
                                                 tags$tr(
                                                   tags$td(
                                                     div(class = "label-and-input",
                                                         dateInput("date1", "", value = Sys.Date()))
                                                   ),
                                                   tags$td(
                                                     div(class = "label-and-input",
                                                         dateInput("date2", "", value = Sys.Date()))
                                                   )
                                                 )
                                      ),
                                      tags$table(id = "my-table",
                                                 tags$colgroup(
                                                   tags$col(width = "50px")
                                                 ),
                                                 tags$tr(
                                                   tags$th("Pilihan:")
                                                 ),
                                                 tags$tr(
                                                   tags$td(
                                                     div(class = "label-and-input",
                                                         uiOutput("dynamicSelect"))
                                                   )
                                                 )
                                      ),
                                      
                                      tags$div(id = "submit-btn",
                                               actionButton("submit", "Papar")
                                      )
                             ),
                             tags$div(id = "my-container1",
                                      
                                      fluidRow(
                                        fluidRow(
                                          valueBoxOutput("valueBox1"),
                                          valueBoxOutput("valueBox2"),
                                          valueBoxOutput("valueBox3")
                                        ),
                                        column(width = 12,
                                               box(id="firstBox",
                                                 status = "primary", 
                                                 solidHeader = TRUE, 
                                                 width = NULL,
                                                 collapsible = TRUE,
                                                 tabBox(  
                                                   width = "800px",# Adjust the height of the tabBox
                                                   tabPanel("Bilangan",
                                                            style = "height: 100%; width: 100%;",  # Adjust the height of the tabPanel content
                                                            highchartOutput("line_plotKompaun", height = 500)
                                                   ),
                                                   tabPanel("Jumlah (RM)",
                                                            style = "height: 100%;",  # Adjust the height of the tabPanel content
                                                            highchartOutput("line_plotKompaunAmaun", height = 500)
                                                   ),
                                                   tabPanel("Lain-lain",
                                                            style = "height: 100%;",  # Adjust the height of the tabPanel content
                                                            fluidRow(
                                                              column(width = 6,
                                                                     highchartOutput("plotlain_1", height = 800)
                                                              ),
                                                              fluidRow(
                                                                column(width = 6,
                                                                       style = "margin-top: 10px;",
                                                                       leafletOutput("map", height = 390)
                                                                ),
                                                              column(width = 3,
                                                                       highchartOutput("plotlain_2", height = 390))
                                                              # column(width = 3,
                                                              #          highchartOutput("plotlain_3", height = 390)
                                                              #        ),
                                                              
                                                                        
                                                              )
                                                            )   
                                                   )
                                                 )
                                               )                                       
                                        )
                                      )
                             )
                           ),
                    tabPanel("LAPORAN KUTIPAN",
                             tags$div(id = "my-container",
                                      tags$table(id = "my-table",
                                                 tags$colgroup(
                                                   tags$col(width = "30%"),
                                                   tags$col(width = "30%"),
                                                   tags$col(width = "40%")
                                                 ),
                                                 tags$tr(
                                                   tags$th("Tajuk Laporan:"),
                                                   tags$th("Mengikut:"),
                                                   tags$th("Jenis Data:")
                                                 ),
                                                 tags$tr(
                                                   tags$td(
                                                     div(class = "label-and-input",
                                                         selectInput("LPR_TAJUK_KUTIPAN", "",
                                                                     choices = c("Jumlah Kutipan"))
                                                     )
                                                   ),
                                                   tags$td(
                                                     div(class = "label-and-input",
                                                         selectInput("MENGIKUT_KUTIPAN", "",
                                                                     choices = c("Kesalahan", "Tempat - Zon", "Tempat - Kawasan"))
                                                     )
                                                   ),
                                                   tags$td(
                                                     div(class = "radio-custom radio-default radio-inline",
                                                         radioButtons("JENIS_DATA_KUTIPAN", label = NULL,
                                                                      choices = c("Tahunan", "Bulanan", "Harian"),
                                                                      selected = "Harian"))
                                                   )
                                                 )
                                      ),

                                      tags$table(id = "my-table",
                                                 tags$colgroup(
                                                   tags$col(width = "50%"),
                                                   tags$col(width = "50%")
                                                 ),
                                                 tags$tr(
                                                   tags$th("Tarikh Mula:"),
                                                   tags$th("Tarikh Akhir:")
                                                 ),
                                                 tags$tr(
                                                   tags$td(
                                                     div(class = "label-and-input",
                                                         dateInput("date1_KUTIPAN", "", value = Sys.Date()))
                                                   ),
                                                   tags$td(
                                                     div(class = "label-and-input",
                                                         dateInput("date2_KUTIPAN", "", value = Sys.Date()))
                                                   )
                                                 )
                                      ),
                                      tags$table(id = "my-table",
                                                 tags$colgroup(
                                                   tags$col(width = "50px")
                                                 ),
                                                 tags$tr(
                                                   tags$th("Pilihan:")
                                                 ),
                                                 tags$tr(
                                                   tags$td(
                                                     div(class = "label-and-input",
                                                         uiOutput("dynamicSelect_KUTIPAN"))
                                                   )
                                                 )
                                      ),

                                      tags$div(id = "submit-btn",
                                               actionButton("submitKutipan", "Papar")
                                      )
                             ),
                             tags$div(id = "my-container1",
                                      fluidRow(
                                        column(width = 12,
                                               box(id="firstBox",
                                                   title = NULL,
                                                   status = "primary",
                                                   solidHeader = TRUE,
                                                   width = NULL,
                                                   collapsible = TRUE,
                                                   tabBox(
                                                     width = "800px",# Adjust the height of the tabBox
                                                     tabPanel("Bilangan",
                                                              style = "height: 100%; width: 100%;",  # Adjust the height of the tabPanel content
                                                              highchartOutput("plot1", height = 500)
                                                     ),
                                                     tabPanel("Jumlah (RM)",
                                                              style = "height: 100%;",  # Adjust the height of the tabPanel content
                                                              highchartOutput("plot2", height = 500)
                                                     ),
                                                     tabPanel("Lain-lain",
                                                              style = "height: 100%;",  # Adjust the height of the tabPanel content
                                                              fluidRow(
                                                                column(width = 8,
                                                                       highchartOutput("plot3", height = 500)),
                                                                column(width = 4,
                                                                       plotlyOutput("plot4", height = 500))
                                                                
                                                              )
                                                              
                                                     )
                                                   )
                                               )
                                        )
                                      )
                             )
                             )
                  ))
      )
    
    
  )
)



# Server function----
server <- function(input, output) {
  
  observeEvent(input$MENGIKUT, {
    if (input$MENGIKUT == "Kesalahan") {
      choices <- c("KESELURUHAN", setdiff(unique(df1$KESALAHAN), "NA"))
    } else if (input$MENGIKUT == "Tempat - Zon") {
      choices <- c("KESELURUHAN", setdiff(unique(df1$TEMPAT_ZON_NAMA), "TIADA REKOD"))
    } else if (input$MENGIKUT == "Tempat - Kawasan") {
      choices <- c("KESELURUHAN", setdiff(unique(df1$TEMPAT_KAWASAN_NAMA), "TIADA REKOD"))
    }
    
    output$dynamicSelect <- renderUI({
      selectInput("dynamicChoice", "", choices = choices)
    })
  })
  
  observeEvent(input$MENGIKUT_KUTIPAN, {
    if (input$MENGIKUT_KUTIPAN == "Kesalahan") {
      choices <- c("KESELURUHAN", setdiff(unique(df1$KESALAHAN), "NA"))
    } else if (input$MENGIKUT_KUTIPAN == "Tempat - Zon") {
      choices <- c("KESELURUHAN", setdiff(unique(df1$TEMPAT_ZON_NAMA), "TIADA REKOD"))
    } else if (input$MENGIKUT_KUTIPAN == "Tempat - Kawasan") {
      choices <- c("KESELURUHAN", setdiff(unique(df1$TEMPAT_KAWASAN_NAMA), "TIADA REKOD"))
    }
    
    output$dynamicSelect_KUTIPAN <- renderUI({
      selectInput("dynamicChoice_KUTIPAN", "", choices = choices)
    })
  })
  
  observeEvent(input$submit, {
    mengikut <- input$MENGIKUT
    if(input$LPR_TAJUK == "Kompaun Dikeluarkan")
    {
      #################################################################### VALUE BOX ###########################################################################
      if(input$MENGIKUT == "Kesalahan")
      {
        if (input$dynamicChoice == "KESELURUHAN")
        {
          # Render the value box
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            count_kompauns <- nrow(df3)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            sum_kompauns<-sum(df3$KOMPAUN_AMAUN)
            sum_kompauns<-paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox3 <- renderValueBox({
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(df3, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan<-paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } else 
        {
          # Render the value box
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$KESALAHAN == input$dynamicChoice, ]
            cat("Pilihan:", input$dynamicChoice)
            count_kompauns <- nrow(filtered_df)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$KESALAHAN == input$dynamicChoice, ]
            
            sum_kompauns <- sum(filtered_df$KOMPAUN_AMAUN)
            sum_kompauns <- paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox3 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$KESALAHAN == input$dynamicChoice, ]
            
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(filtered_df, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan <- paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } 
      } else if (input$MENGIKUT == "Tempat - Zon")
      {
        if (input$dynamicChoice == "KESELURUHAN")
        {
          cat("Masuk sini")
          # Render the value box
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            count_kompauns <- nrow(df3)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            sum_kompauns<-sum(df3$KOMPAUN_AMAUN)
            sum_kompauns<-paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox3 <- renderValueBox({
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(df3, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan<-paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } else 
        {
          # Render the value box
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_ZON_NAMA == input$dynamicChoice, ]
            cat("Pilihan:", input$dynamicChoice)
            count_kompauns <- nrow(filtered_df)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_ZON_NAMA == input$dynamicChoice, ]
            
            sum_kompauns <- sum(filtered_df$KOMPAUN_AMAUN)
            sum_kompauns <- paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox3 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_ZON_NAMA == input$dynamicChoice, ]
            
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(filtered_df, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan <- paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        }
      } else if (input$MENGIKUT == "Tempat - Kawasan")
      {
        if (input$dynamicChoice == "KESELURUHAN")
        {
          cat("Masuk sini")
          # Render the value box
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            count_kompauns <- nrow(df3)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            sum_kompauns<-sum(df3$KOMPAUN_AMAUN)
            sum_kompauns<-paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox3 <- renderValueBox({
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(df3, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan<-paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } else 
        {
          # Render the value box
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_KAWASAN_NAMA == input$dynamicChoice, ]
            cat("Pilihan:", input$dynamicChoice)
            count_kompauns <- nrow(filtered_df)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_KAWASAN_NAMA == input$dynamicChoice, ]
            
            sum_kompauns <- sum(filtered_df$KOMPAUN_AMAUN)
            sum_kompauns <- paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox3 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_KAWASAN_NAMA == input$dynamicChoice, ]
            
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(filtered_df, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan <- paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } 
      }
      ##########################################################################################################################################################
      
      ############################################################### TAB LAIN-LAIN ############################################################################
      ##########################################################################################################################################################
      
      if(input$JENIS_DATA == "Tahunan")
      {
        if(input$MENGIKUT == "Kesalahan")
        {
          output$line_plotKompaun <- renderHighchart({
          
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice & !is.na(df1$KESALAHAN)])
            }
            
            # Create a complete dataset of all combinations of YEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(df1$KOMPAUN_TARIKH)), max(lubridate::year(df1$KOMPAUN_TARIKH)), by = 1),
              KESALAHAN = unique_kesalahan
            )
            
            # Count occurrence by year and KESALAHAN
            a1 <- df1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), KESALAHAN) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "KESALAHAN")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1)
              end_year <- lubridate::year(input$date2)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KESALAHAN) %>%
                arrange(KOMPAUN_YEAR)  # Make sure data is ordered by year
              
              data_list <- split(grouped_data, grouped_data$KESALAHAN)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Tahun dan Kesalahan") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- seq(lubridate::year(input$date1), lubridate::year(input$date2), by = 1)
            
            for (kesalahan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kesalahan]]
              chart <- chart %>% hc_add_series(
                name = kesalahan,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          output$line_plotKompaunAmaun <- renderHighchart({
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice & !is.na(df1$KESALAHAN)])
            }
            
            # Create a complete dataset of all combinations of YEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(df1$KOMPAUN_TARIKH)), max(lubridate::year(df1$KOMPAUN_TARIKH)), by = 1),
              KESALAHAN = unique_kesalahan
            )
            
            a1 <- df1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), KESALAHAN) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "KESALAHAN")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1)
              end_year <- lubridate::year(input$date2)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              grouped_data <- filtered_data %>%
                group_by(KESALAHAN) %>%
                arrange(KOMPAUN_YEAR)
              
              data_list <- split(grouped_data, grouped_data$KESALAHAN)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Tahun dan Kesalahan") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- seq(lubridate::year(input$date1), lubridate::year(input$date2), by = 1)
            
            for (kesalahan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kesalahan]]
              chart <- chart %>% hc_add_series(
                name = kesalahan,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          output$plotlain_1 <- renderHighchart({
            #KOMPAUN (TOP 5)----
            # Create a complete dataset of all combinations of DATE, KAWASAN_TEMPAT_NAMA, and KOMPAUN_AMAUN
            complete_data2 <- expand.grid(
              KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              KESALAHAN = unique(df1$KESALAHAN),
              KOMPAUN_AMAUN = unique(df1$KOMPAUN_AMAUN)
            )
            
            # Count occurrence by date
            b1 <- df1 %>%
              group_by(KOMPAUN_TARIKH, KESALAHAN, KOMPAUN_AMAUN) %>%
              summarize(Count = n(), .groups = "drop") %>%
              filter(KESALAHAN != "TIADA REKOD")
            
            # Join the summarized data to the complete dataset and fill missing values with 0
            b2 <- complete_data2 %>%
              left_join(b1, by = c("KOMPAUN_TARIKH", "KESALAHAN", "KOMPAUN_AMAUN")) %>%
              mutate(Count = replace_na(Count, 0))
            
            
            # Reactive function to filter data based on selected date range
            filteredData6 <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              subset(b2, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
            })
            # Group the data by TEMPAT and calculate the sum of COUNT
            grouped_data2 <- filteredData6() %>% 
              group_by(KESALAHAN) %>% 
              summarise(total_count = sum(Count)) %>%
              arrange(desc(total_count)) %>%
              head(5) # Select top 5 highest areas
            
            # Create the stacked bar chart using highchart
            hchart(grouped_data2, "column", options3d = list(enabled = TRUE, beta = 15, alpha = 15), hcaes(x = KESALAHAN, y = total_count)) %>%
              hc_chart(type = "column", backgroundColor = "#f7f7f7") %>%
              hc_title(text = "Top 5 Mengikut Kategori") %>%
              hc_plotOptions(
                column = list(
                  stacking = "normal",
                  borderRadiusTopLeft = 6,
                  borderRadiusTopRight = 6,
                  colorByPoint = TRUE,
                  dataLabels = list(enabled = TRUE, format = "{y}")
                )
              ) %>%
              hc_xAxis(title = list(text = "KATEGORI"), labels = list(rotation = -90, align = "right"), gridLineWidth = 0) %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE) %>%
              hc_tooltip(formatter = JS("function() { 
      return '<b>' + this.point.category + '</b><br>' + 'Kompaun: ' + this.y; 
    }"))
          })
          output$plotlain_2 <- renderHighchart({
            filteredData <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_df <- df1[df1$KOMPAUN_TARIKH >= start_date & df1$KOMPAUN_TARIKH <= end_date, ]
              if (input$dynamicChoice != "KESELURUHAN") {
                filtered_df <- filtered_df[filtered_df$KESALAHAN == input$dynamicChoice, ]
              }
              filtered_df
            })
            
            # Calculate the count for each value of KOMPAUN_STATUS
            counts <- table(filteredData()$KOMPAUN_STATUS)
            # Create a dataframe for the pie chart
            pie_data <- data.frame(
              KOMPAUN_STATUS = names(counts),
              Count = as.numeric(counts)
            )
            
            # Create the pie chart using highchart
            highchart() %>%
              hc_chart(type = "pie", backgroundColor = "#f7f7f7") %>%
              hc_add_series(pie_data, type = "pie", hcaes(x = KOMPAUN_STATUS, y = Count)) %>%
              hc_title(text = "Bilangan Kompaun by STATUS") %>%
              hc_plotOptions(
                pie = list(
                  innerSize = "50%",
                  startAngle = 90,
                  endAngle = 90,
                  center = list('50%', '50%'),  # Adjust center to make it centered
                  size = '70%'
                )
              ) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE)
          })
          output$map <- renderLeaflet({
            # Define the bin size (10km = 0.09 degrees, adjust as needed)
            lat_bin_size <- 5
            lon_bin_size <- 5
            
            df1 <- df1 %>%
              mutate(
                lat_bin = round(TEMPAT_GPS_LATITUDE / lat_bin_size) * lat_bin_size,
                lon_bin = round(TEMPAT_GPS_LONGTITUDE / lon_bin_size) * lon_bin_size
              )
            df2 <- df1 %>%
              group_by(TEMPAT_GPS_LONGTITUDE, TEMPAT_GPS_LATITUDE) %>%
              summarise(count_of_kompaun = n())
            leaflet(data = df2) %>%
              addTiles() %>%
              addCircleMarkers(
                lng = ~TEMPAT_GPS_LONGTITUDE,
                lat = ~TEMPAT_GPS_LATITUDE,
                radius = ~count_of_kompaun,  # Calculate and set radius based on count_of_kompaun
                stroke = FALSE,
                fillOpacity = 0.7,
                label = ~paste("Count of Kompaun: ", count_of_kompaun),
                color = "blue"  # Set the marker color to red
              )
          })
        } else if (input$MENGIKUT == "Tempat - Zon")
        {
          output$line_plotKompaun <- renderHighchart({
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA == input$dynamicChoice & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            # Create a complete dataset of all combinations of YEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(df1$KOMPAUN_TARIKH)), max(lubridate::year(df1$KOMPAUN_TARIKH)), by = 1),
              TEMPAT_ZON_NAMA = unique_zon
            )
            
            # Count occurrence by year and KESALAHAN
            a1 <- df1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), TEMPAT_ZON_NAMA) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_ZON_NAMA")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1)
              end_year <- lubridate::year(input$date2)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_ZON_NAMA) %>%
                arrange(KOMPAUN_YEAR)  # Make sure data is ordered by year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_ZON_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Tahun dan Zon") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- seq(lubridate::year(input$date1), lubridate::year(input$date2), by = 1)
            
            for (zon in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[zon]]
              chart <- chart %>% hc_add_series(
                name = zon,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          output$line_plotKompaunAmaun <- renderHighchart({
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA == input$dynamicChoice & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            # Create a complete dataset of all combinations of YEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(df1$KOMPAUN_TARIKH)), max(lubridate::year(df1$KOMPAUN_TARIKH)), by = 1),
              TEMPAT_ZON_NAMA = unique_zon
            )
            
            a1 <- df1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), TEMPAT_ZON_NAMA) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_ZON_NAMA")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1)
              end_year <- lubridate::year(input$date2)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_ZON_NAMA) %>%
                arrange(KOMPAUN_YEAR)
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_ZON_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Tahun dan Zon") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- seq(lubridate::year(input$date1), lubridate::year(input$date2), by = 1)
            
            for (zon in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[zon]]
              chart <- chart %>% hc_add_series(
                name = zon,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })  
          output$plotlain_1 <- renderHighchart({
            #KOMPAUN (TOP 5)----
            # Create a complete dataset of all combinations of DATE, KAWASAN_TEMPAT_NAMA, and KOMPAUN_AMAUN
            complete_data2 <- expand.grid(
              KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_ZON_NAMA = unique(df1$TEMPAT_ZON_NAMA),
              KOMPAUN_AMAUN = unique(df1$KOMPAUN_AMAUN)
            )
            
            # Count occurrence by date
            b1 <- df1 %>%
              group_by(KOMPAUN_TARIKH, TEMPAT_ZON_NAMA, KOMPAUN_AMAUN) %>%
              summarize(Count = n(), .groups = "drop") %>%
              filter(TEMPAT_ZON_NAMA != "TIADA REKOD")
            
            # Join the summarized data to the complete dataset and fill missing values with 0
            b2 <- complete_data2 %>%
              left_join(b1, by = c("KOMPAUN_TARIKH", "TEMPAT_ZON_NAMA", "KOMPAUN_AMAUN")) %>%
              mutate(Count = replace_na(Count, 0))
            
            
            # Reactive function to filter data based on selected date range
            filteredData6 <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              subset(b2, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
            })
            # Group the data by TEMPAT and calculate the sum of COUNT
            grouped_data2 <- filteredData6() %>% 
              group_by(TEMPAT_ZON_NAMA) %>% 
              summarise(total_count = sum(Count)) %>%
              arrange(desc(total_count)) %>%
              head(5) # Select top 5 highest areas
            
            # Create the stacked bar chart using highchart
            hchart(grouped_data2, "column", options3d = list(enabled = TRUE, beta = 15, alpha = 15), hcaes(x = TEMPAT_ZON_NAMA, y = total_count)) %>%
              hc_chart(type = "column", backgroundColor = "#f7f7f7") %>%
              hc_title(text = "Top 5 Mengikut Kategori") %>%
              hc_plotOptions(
                column = list(
                  stacking = "normal",
                  borderRadiusTopLeft = 6,
                  borderRadiusTopRight = 6,
                  colorByPoint = TRUE,
                  dataLabels = list(enabled = TRUE, format = "{y}")
                )
              ) %>%
              hc_xAxis(title = list(text = "KATEGORI"), labels = list(rotation = -90, align = "right"), gridLineWidth = 0) %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE) %>%
              hc_tooltip(formatter = JS("function() { 
      return '<b>' + this.point.category + '</b><br>' + 'Kompaun: ' + this.y; 
    }"))
          })
          output$plotlain_2 <- renderHighchart({
            filteredData <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_df <- df1[df1$KOMPAUN_TARIKH >= start_date & df1$KOMPAUN_TARIKH <= end_date, ]
              if (input$dynamicChoice != "KESELURUHAN") {
                filtered_df <- filtered_df[filtered_df$TEMPAT_ZON_NAMA == input$dynamicChoice, ]
              }
              filtered_df
            })
            
            # Calculate the count for each value of KOMPAUN_STATUS
            counts <- table(filteredData()$KOMPAUN_STATUS)
            # Create a dataframe for the pie chart
            pie_data <- data.frame(
              KOMPAUN_STATUS = names(counts),
              Count = as.numeric(counts)
            )
            
            # Create the pie chart using highchart
            highchart() %>%
              hc_chart(type = "pie", backgroundColor = "#f7f7f7") %>%
              hc_add_series(pie_data, type = "pie", hcaes(x = KOMPAUN_STATUS, y = Count)) %>%
              hc_title(text = "Bilangan Kompaun by STATUS") %>%
              hc_plotOptions(
                pie = list(
                  innerSize = "50%",
                  startAngle = 90,
                  endAngle = 90,
                  center = list('50%', '50%'),  # Adjust center to make it centered
                  size = '70%'
                )
              ) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE)
          })
          output$map <- renderLeaflet({
            # Define the bin size (10km = 0.09 degrees, adjust as needed)
            lat_bin_size <- 5
            lon_bin_size <- 5
            
            df1 <- df1 %>%
              mutate(
                lat_bin = round(TEMPAT_GPS_LATITUDE / lat_bin_size) * lat_bin_size,
                lon_bin = round(TEMPAT_GPS_LONGTITUDE / lon_bin_size) * lon_bin_size
              )
            df2 <- df1 %>%
              group_by(TEMPAT_GPS_LONGTITUDE, TEMPAT_GPS_LATITUDE) %>%
              summarise(count_of_kompaun = n())
            leaflet(data = df2) %>%
              addTiles() %>%
              addCircleMarkers(
                lng = ~TEMPAT_GPS_LONGTITUDE,
                lat = ~TEMPAT_GPS_LATITUDE,
                radius = ~count_of_kompaun,  # Calculate and set radius based on count_of_kompaun
                stroke = FALSE,
                fillOpacity = 0.7,
                label = ~paste("Count of Kompaun: ", count_of_kompaun),
                color = "blue"  # Set the marker color to red
              )
          })
        } else if (input$MENGIKUT == "Tempat - Kawasan")
        {
          output$line_plotKompaun <- renderHighchart({
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA == input$dynamicChoice & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            # Create a complete dataset of all combinations of YEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(df1$KOMPAUN_TARIKH)), max(lubridate::year(df1$KOMPAUN_TARIKH)), by = 1),
              TEMPAT_KAWASAN_NAMA = unique_kawasan
            )
            
            # Count occurrence by year and KESALAHAN
            a1 <- df1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), TEMPAT_KAWASAN_NAMA) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_KAWASAN_NAMA")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1)
              end_year <- lubridate::year(input$date2)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_KAWASAN_NAMA) %>%
                arrange(KOMPAUN_YEAR)  # Make sure data is ordered by year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_KAWASAN_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Tahun dan Kawasan") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- seq(lubridate::year(input$date1), lubridate::year(input$date2), by = 1)
            
            for (kawasan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kawasan]]
              chart <- chart %>% hc_add_series(
                name = kawasan,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })  
          output$line_plotKompaunAmaun <- renderHighchart({
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA == input$dynamicChoice & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            # Create a complete dataset of all combinations of YEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(df1$KOMPAUN_TARIKH)), max(lubridate::year(df1$KOMPAUN_TARIKH)), by = 1),
              TEMPAT_KAWASAN_NAMA = unique_kawasan
            )
            
            a1 <- df1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), TEMPAT_KAWASAN_NAMA) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_KAWASAN_NAMA")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1)
              end_year <- lubridate::year(input$date2)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_KAWASAN_NAMA) %>%
                arrange(KOMPAUN_YEAR)
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_KAWASAN_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Tahun dan Kawasan") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- seq(lubridate::year(input$date1), lubridate::year(input$date2), by = 1)
            
            for (kawasan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kawasan]]
              chart <- chart %>% hc_add_series(
                name = kawasan,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          output$plotlain_1 <- renderHighchart({
            #KOMPAUN (TOP 5)----
            # Create a complete dataset of all combinations of DATE, KAWASAN_TEMPAT_NAMA, and KOMPAUN_AMAUN
            complete_data2 <- expand.grid(
              KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_KAWASAN_NAMA = unique(df1$TEMPAT_KAWASAN_NAMA),
              KOMPAUN_AMAUN = unique(df1$KOMPAUN_AMAUN)
            )
            
            # Count occurrence by date
            b1 <- df1 %>%
              group_by(KOMPAUN_TARIKH, TEMPAT_KAWASAN_NAMA, KOMPAUN_AMAUN) %>%
              summarize(Count = n(), .groups = "drop") %>%
              filter(TEMPAT_KAWASAN_NAMA != "TIADA REKOD")
            
            # Join the summarized data to the complete dataset and fill missing values with 0
            b2 <- complete_data2 %>%
              left_join(b1, by = c("KOMPAUN_TARIKH", "TEMPAT_KAWASAN_NAMA", "KOMPAUN_AMAUN")) %>%
              mutate(Count = replace_na(Count, 0))
            
            
            # Reactive function to filter data based on selected date range
            filteredData6 <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              subset(b2, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
            })
            # Group the data by TEMPAT and calculate the sum of COUNT
            grouped_data2 <- filteredData6() %>% 
              group_by(TEMPAT_KAWASAN_NAMA) %>% 
              summarise(total_count = sum(Count)) %>%
              arrange(desc(total_count)) %>%
              head(5) # Select top 5 highest areas
            
            # Create the stacked bar chart using highchart
            hchart(grouped_data2, "column", options3d = list(enabled = TRUE, beta = 15, alpha = 15), hcaes(x = TEMPAT_KAWASAN_NAMA, y = total_count)) %>%
              hc_chart(type = "column", backgroundColor = "#f7f7f7") %>%
              hc_title(text = "Top 5 Mengikut Kategori") %>%
              hc_plotOptions(
                column = list(
                  stacking = "normal",
                  borderRadiusTopLeft = 6,
                  borderRadiusTopRight = 6,
                  colorByPoint = TRUE,
                  dataLabels = list(enabled = TRUE, format = "{y}")
                )
              ) %>%
              hc_xAxis(title = list(text = "KATEGORI"), labels = list(rotation = -90, align = "right"), gridLineWidth = 0) %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE) %>%
              hc_tooltip(formatter = JS("function() { 
      return '<b>' + this.point.category + '</b><br>' + 'Kompaun: ' + this.y; 
    }"))
          })
          output$plotlain_2 <- renderHighchart({
            filteredData <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_df <- df1[df1$KOMPAUN_TARIKH >= start_date & df1$KOMPAUN_TARIKH <= end_date, ]
              if (input$dynamicChoice != "KESELURUHAN") {
                filtered_df <- filtered_df[filtered_df$TEMPAT_KAWASAN_NAMA == input$dynamicChoice, ]
              }
              filtered_df
            })
            
            # Calculate the count for each value of KOMPAUN_STATUS
            counts <- table(filteredData()$KOMPAUN_STATUS)
            # Create a dataframe for the pie chart
            pie_data <- data.frame(
              KOMPAUN_STATUS = names(counts),
              Count = as.numeric(counts)
            )
            
            # Create the pie chart using highchart
            highchart() %>%
              hc_chart(type = "pie", backgroundColor = "#f7f7f7") %>%
              hc_add_series(pie_data, type = "pie", hcaes(x = KOMPAUN_STATUS, y = Count)) %>%
              hc_title(text = "Bilangan Kompaun by STATUS") %>%
              hc_plotOptions(
                pie = list(
                  innerSize = "50%",
                  startAngle = 90,
                  endAngle = 90,
                  center = list('50%', '50%'),  # Adjust center to make it centered
                  size = '70%'
                )
              ) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE)
          })
          output$map <- renderLeaflet({
            # Define the bin size (10km = 0.09 degrees, adjust as needed)
            lat_bin_size <- 5
            lon_bin_size <- 5
            
            df1 <- df1 %>%
              mutate(
                lat_bin = round(TEMPAT_GPS_LATITUDE / lat_bin_size) * lat_bin_size,
                lon_bin = round(TEMPAT_GPS_LONGTITUDE / lon_bin_size) * lon_bin_size
              )
            df2 <- df1 %>%
              group_by(TEMPAT_GPS_LONGTITUDE, TEMPAT_GPS_LATITUDE) %>%
              summarise(count_of_kompaun = n())
            leaflet(data = df2) %>%
              addTiles() %>%
              addCircleMarkers(
                lng = ~TEMPAT_GPS_LONGTITUDE,
                lat = ~TEMPAT_GPS_LATITUDE,
                radius = ~count_of_kompaun,  # Calculate and set radius based on count_of_kompaun
                stroke = FALSE,
                fillOpacity = 0.7,
                label = ~paste("Count of Kompaun: ", count_of_kompaun),
                color = "blue"  # Set the marker color to red
              )
          })
        }
      } else if(input$JENIS_DATA == "Bulanan")
      {
        if(input$MENGIKUT == "Kesalahan")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice & !is.na(df1$KESALAHAN)])
            }
            
            # Create a complete dataset of all combinations of MONTHYEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_MONTHYEAR = seq(floor_date(min(df1$KOMPAUN_TARIKH), "month"), ceiling_date(max(df1$KOMPAUN_TARIKH), "month"), by = "month"),
              KESALAHAN = unique_kesalahan
            )
            
            # Count occurrence by month-year and KESALAHAN
            a1 <- df1 %>%
              group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), KESALAHAN) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_MONTHYEAR", "KESALAHAN")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected month-year range
            filteredDataList <- reactive({
              start_monthyear <- floor_date(input$date1, "month")
              end_monthyear <- ceiling_date(input$date2, "month")
              filtered_data <- a2 %>%
                filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KESALAHAN) %>%
                arrange(KOMPAUN_MONTHYEAR)  # Make sure data is ordered by month-year
              
              data_list <- split(grouped_data, grouped_data$KESALAHAN)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Bulan dan Kesalahan") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- format(seq(floor_date(input$date1, "month"), ceiling_date(input$date2, "month"), by = "month"), format = "%b-%y")
            
            for (kesalahan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kesalahan]]
              chart <- chart %>% hc_add_series(
                name = kesalahan,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice & !is.na(df1$KESALAHAN)])
            }
            
            # Create a complete dataset of all combinations of MONTHYEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_MONTHYEAR = seq(floor_date(min(df1$KOMPAUN_TARIKH), "month"), ceiling_date(max(df1$KOMPAUN_TARIKH), "month"), by = "month"),
              KESALAHAN = unique_kesalahan
            )
            
            # Calculate sum of KOMPAUN_AMAUN by month-year and KESALAHAN
            a1 <- df1 %>%
              group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), KESALAHAN) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_MONTHYEAR", "KESALAHAN")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected month-year range
            filteredDataList <- reactive({
              start_monthyear <- floor_date(input$date1, "month")
              end_monthyear <- ceiling_date(input$date2, "month")
              filtered_data <- a2 %>%
                filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KESALAHAN) %>%
                arrange(KOMPAUN_MONTHYEAR)  # Make sure data is ordered by month-year
              
              data_list <- split(grouped_data, grouped_data$KESALAHAN)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Bulan dan Kesalahan") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- format(seq(floor_date(input$date1, "month"), ceiling_date(input$date2, "month"), by = "month"), format = "%b-%y")
            
            for (kesalahan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kesalahan]]
              chart <- chart %>% hc_add_series(
                name = kesalahan,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          output$plotlain_1 <- renderHighchart({
            #KOMPAUN (TOP 5)----
            # Create a complete dataset of all combinations of DATE, KAWASAN_TEMPAT_NAMA, and KOMPAUN_AMAUN
            complete_data2 <- expand.grid(
              KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              KESALAHAN = unique(df1$KESALAHAN),
              KOMPAUN_AMAUN = unique(df1$KOMPAUN_AMAUN)
            )
            
            # Count occurrence by date
            b1 <- df1 %>%
              group_by(KOMPAUN_TARIKH, KESALAHAN, KOMPAUN_AMAUN) %>%
              summarize(Count = n(), .groups = "drop") %>%
              filter(KESALAHAN != "TIADA REKOD")
            
            # Join the summarized data to the complete dataset and fill missing values with 0
            b2 <- complete_data2 %>%
              left_join(b1, by = c("KOMPAUN_TARIKH", "KESALAHAN", "KOMPAUN_AMAUN")) %>%
              mutate(Count = replace_na(Count, 0))
            
            
            # Reactive function to filter data based on selected date range
            filteredData6 <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              subset(b2, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
            })
            # Group the data by TEMPAT and calculate the sum of COUNT
            grouped_data2 <- filteredData6() %>% 
              group_by(KESALAHAN) %>% 
              summarise(total_count = sum(Count)) %>%
              arrange(desc(total_count)) %>%
              head(5) # Select top 5 highest areas
            
            # Create the stacked bar chart using highchart
            hchart(grouped_data2, "column", options3d = list(enabled = TRUE, beta = 15, alpha = 15), hcaes(x = KESALAHAN, y = total_count)) %>%
              hc_chart(type = "column", backgroundColor = "#f7f7f7") %>%
              hc_title(text = "Top 5 Mengikut Kategori") %>%
              hc_plotOptions(
                column = list(
                  stacking = "normal",
                  borderRadiusTopLeft = 6,
                  borderRadiusTopRight = 6,
                  colorByPoint = TRUE,
                  dataLabels = list(enabled = TRUE, format = "{y}")
                )
              ) %>%
              hc_xAxis(title = list(text = "KATEGORI"), labels = list(rotation = -90, align = "right"), gridLineWidth = 0) %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE) %>%
              hc_tooltip(formatter = JS("function() { 
      return '<b>' + this.point.category + '</b><br>' + 'Kompaun: ' + this.y; 
    }"))
          })
          output$plotlain_2 <- renderHighchart({
            filteredData <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_df <- df1[df1$KOMPAUN_TARIKH >= start_date & df1$KOMPAUN_TARIKH <= end_date, ]
              if (input$dynamicChoice != "KESELURUHAN") {
                filtered_df <- filtered_df[filtered_df$KESALAHAN == input$dynamicChoice, ]
              }
              filtered_df
            })
            
            # Calculate the count for each value of KOMPAUN_STATUS
            counts <- table(filteredData()$KOMPAUN_STATUS)
            # Create a dataframe for the pie chart
            pie_data <- data.frame(
              KOMPAUN_STATUS = names(counts),
              Count = as.numeric(counts)
            )
            
            # Create the pie chart using highchart
            highchart() %>%
              hc_chart(type = "pie", backgroundColor = "#f7f7f7") %>%
              hc_add_series(pie_data, type = "pie", hcaes(x = KOMPAUN_STATUS, y = Count)) %>%
              hc_title(text = "Bilangan Kompaun by STATUS") %>%
              hc_plotOptions(
                pie = list(
                  innerSize = "50%",
                  startAngle = 90,
                  endAngle = 90,
                  center = list('50%', '50%'),  # Adjust center to make it centered
                  size = '70%'
                )
              ) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE)
          })
          output$map <- renderLeaflet({
            # Define the bin size (10km = 0.09 degrees, adjust as needed)
            lat_bin_size <- 5
            lon_bin_size <- 5
            
            df1 <- df1 %>%
              mutate(
                lat_bin = round(TEMPAT_GPS_LATITUDE / lat_bin_size) * lat_bin_size,
                lon_bin = round(TEMPAT_GPS_LONGTITUDE / lon_bin_size) * lon_bin_size
              )
            df2 <- df1 %>%
              group_by(TEMPAT_GPS_LONGTITUDE, TEMPAT_GPS_LATITUDE) %>%
              summarise(count_of_kompaun = n())
            leaflet(data = df2) %>%
              addTiles() %>%
              addCircleMarkers(
                lng = ~TEMPAT_GPS_LONGTITUDE,
                lat = ~TEMPAT_GPS_LATITUDE,
                radius = ~count_of_kompaun,  # Calculate and set radius based on count_of_kompaun
                stroke = FALSE,
                fillOpacity = 0.7,
                label = ~paste("Count of Kompaun: ", count_of_kompaun),
                color = "blue"  # Set the marker color to red
              )
          })
          
        } else if (input$MENGIKUT == "Tempat - Zon")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA == input$dynamicChoice & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            # Create a complete dataset of all combinations of MONTHYEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_MONTHYEAR = seq(floor_date(min(df1$KOMPAUN_TARIKH), "month"), ceiling_date(max(df1$KOMPAUN_TARIKH), "month"), by = "month"),
              TEMPAT_ZON_NAMA = unique_zon
            )
            
            # Count occurrence by month-year and KESALAHAN
            a1 <- df1 %>%
              group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), TEMPAT_ZON_NAMA) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_MONTHYEAR", "TEMPAT_ZON_NAMA")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected month-year range
            filteredDataList <- reactive({
              start_monthyear <- floor_date(input$date1, "month")
              end_monthyear <- ceiling_date(input$date2, "month")
              filtered_data <- a2 %>%
                filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_ZON_NAMA) %>%
                arrange(KOMPAUN_MONTHYEAR)  # Make sure data is ordered by month-year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_ZON_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Bulan dan Zon") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- format(seq(floor_date(input$date1, "month"), ceiling_date(input$date2, "month"), by = "month"), format = "%b-%y")
            
            for (zon in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[zon]]
              chart <- chart %>% hc_add_series(
                name = zon,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          }) 
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA == input$dynamicChoice & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            # Create a complete dataset of all combinations of MONTHYEAR and ZON
            Kompaun_data1 <- expand.grid(
              KOMPAUN_MONTHYEAR = seq(floor_date(min(df1$KOMPAUN_TARIKH), "month"), ceiling_date(max(df1$KOMPAUN_TARIKH), "month"), by = "month"),
              TEMPAT_ZON_NAMA = unique_zon
            )
            
            # Calculate sum of KOMPAUN_AMAUN by month-year and ZON
            a1 <- df1 %>%
              group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), TEMPAT_ZON_NAMA) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_MONTHYEAR", "TEMPAT_ZON_NAMA")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected month-year range
            filteredDataList <- reactive({
              start_monthyear <- floor_date(input$date1, "month")
              end_monthyear <- ceiling_date(input$date2, "month")
              filtered_data <- a2 %>%
                filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              
              # Group data by ZON for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_ZON_NAMA) %>%
                arrange(KOMPAUN_MONTHYEAR)  # Make sure data is ordered by month-year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_ZON_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Bulan dan Zon") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- format(seq(floor_date(input$date1, "month"), ceiling_date(input$date2, "month"), by = "month"), format = "%b-%y")
            
            for (zon in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[zon]]
              chart <- chart %>% hc_add_series(
                name = zon,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          output$plotlain_1 <- renderHighchart({
            #KOMPAUN (TOP 5)----
            # Create a complete dataset of all combinations of DATE, KAWASAN_TEMPAT_NAMA, and KOMPAUN_AMAUN
            complete_data2 <- expand.grid(
              KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_ZON_NAMA = unique(df1$TEMPAT_ZON_NAMA),
              KOMPAUN_AMAUN = unique(df1$KOMPAUN_AMAUN)
            )
            
            # Count occurrence by date
            b1 <- df1 %>%
              group_by(KOMPAUN_TARIKH, TEMPAT_ZON_NAMA, KOMPAUN_AMAUN) %>%
              summarize(Count = n(), .groups = "drop") %>%
              filter(TEMPAT_ZON_NAMA != "TIADA REKOD")
            
            # Join the summarized data to the complete dataset and fill missing values with 0
            b2 <- complete_data2 %>%
              left_join(b1, by = c("KOMPAUN_TARIKH", "TEMPAT_ZON_NAMA", "KOMPAUN_AMAUN")) %>%
              mutate(Count = replace_na(Count, 0))
            
            
            # Reactive function to filter data based on selected date range
            filteredData6 <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              subset(b2, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
            })
            # Group the data by TEMPAT and calculate the sum of COUNT
            grouped_data2 <- filteredData6() %>% 
              group_by(TEMPAT_ZON_NAMA) %>% 
              summarise(total_count = sum(Count)) %>%
              arrange(desc(total_count)) %>%
              head(5) # Select top 5 highest areas
            
            # Create the stacked bar chart using highchart
            hchart(grouped_data2, "column", options3d = list(enabled = TRUE, beta = 15, alpha = 15), hcaes(x = TEMPAT_ZON_NAMA, y = total_count)) %>%
              hc_chart(type = "column", backgroundColor = "#f7f7f7") %>%
              hc_title(text = "Top 5 Mengikut Kategori") %>%
              hc_plotOptions(
                column = list(
                  stacking = "normal",
                  borderRadiusTopLeft = 6,
                  borderRadiusTopRight = 6,
                  colorByPoint = TRUE,
                  dataLabels = list(enabled = TRUE, format = "{y}")
                )
              ) %>%
              hc_xAxis(title = list(text = "KATEGORI"), labels = list(rotation = -90, align = "right"), gridLineWidth = 0) %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE) %>%
              hc_tooltip(formatter = JS("function() { 
      return '<b>' + this.point.category + '</b><br>' + 'Kompaun: ' + this.y; 
    }"))
          })
          output$plotlain_2 <- renderHighchart({
            filteredData <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_df <- df1[df1$KOMPAUN_TARIKH >= start_date & df1$KOMPAUN_TARIKH <= end_date, ]
              if (input$dynamicChoice != "KESELURUHAN") {
                filtered_df <- filtered_df[filtered_df$TEMPAT_ZON_NAMA == input$dynamicChoice, ]
              }
              filtered_df
            })
            
            # Calculate the count for each value of KOMPAUN_STATUS
            counts <- table(filteredData()$KOMPAUN_STATUS)
            # Create a dataframe for the pie chart
            pie_data <- data.frame(
              KOMPAUN_STATUS = names(counts),
              Count = as.numeric(counts)
            )
            
            # Create the pie chart using highchart
            highchart() %>%
              hc_chart(type = "pie", backgroundColor = "#f7f7f7") %>%
              hc_add_series(pie_data, type = "pie", hcaes(x = KOMPAUN_STATUS, y = Count)) %>%
              hc_title(text = "Bilangan Kompaun by STATUS") %>%
              hc_plotOptions(
                pie = list(
                  innerSize = "50%",
                  startAngle = 90,
                  endAngle = 90,
                  center = list('50%', '50%'),  # Adjust center to make it centered
                  size = '70%'
                )
              ) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE)
          })
          output$map <- renderLeaflet({
            # Define the bin size (10km = 0.09 degrees, adjust as needed)
            lat_bin_size <- 5
            lon_bin_size <- 5
            
            df1 <- df1 %>%
              mutate(
                lat_bin = round(TEMPAT_GPS_LATITUDE / lat_bin_size) * lat_bin_size,
                lon_bin = round(TEMPAT_GPS_LONGTITUDE / lon_bin_size) * lon_bin_size
              )
            df2 <- df1 %>%
              group_by(TEMPAT_GPS_LONGTITUDE, TEMPAT_GPS_LATITUDE) %>%
              summarise(count_of_kompaun = n())
            leaflet(data = df2) %>%
              addTiles() %>%
              addCircleMarkers(
                lng = ~TEMPAT_GPS_LONGTITUDE,
                lat = ~TEMPAT_GPS_LATITUDE,
                radius = ~count_of_kompaun,  # Calculate and set radius based on count_of_kompaun
                stroke = FALSE,
                fillOpacity = 0.7,
                label = ~paste("Count of Kompaun: ", count_of_kompaun),
                color = "blue"  # Set the marker color to red
              )
          })
        } else if (input$MENGIKUT == "Tempat - Kawasan")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA == input$dynamicChoice & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            # Create a complete dataset of all combinations of MONTHYEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_MONTHYEAR = seq(floor_date(min(df1$KOMPAUN_TARIKH), "month"), ceiling_date(max(df1$KOMPAUN_TARIKH), "month"), by = "month"),
              TEMPAT_KAWASAN_NAMA = unique_kawasan
            )
            
            # Count occurrence by month-year and KESALAHAN
            a1 <- df1 %>%
              group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), TEMPAT_KAWASAN_NAMA) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_MONTHYEAR", "TEMPAT_KAWASAN_NAMA")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected month-year range
            filteredDataList <- reactive({
              start_monthyear <- floor_date(input$date1, "month")
              end_monthyear <- ceiling_date(input$date2, "month")
              filtered_data <- a2 %>%
                filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_KAWASAN_NAMA) %>%
                arrange(KOMPAUN_MONTHYEAR)  # Make sure data is ordered by month-year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_KAWASAN_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Bulan dan Kawasan") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- format(seq(floor_date(input$date1, "month"), ceiling_date(input$date2, "month"), by = "month"), format = "%b-%y")
            
            for (kawasan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kawasan]]
              chart <- chart %>% hc_add_series(
                name = kawasan,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA == input$dynamicChoice & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            # Create a complete dataset of all combinations of MONTHYEAR and KAWASAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_MONTHYEAR = seq(floor_date(min(df1$KOMPAUN_TARIKH), "month"), ceiling_date(max(df1$KOMPAUN_TARIKH), "month"), by = "month"),
              TEMPAT_KAWASAN_NAMA = unique_kawasan
            )
            
            # Calculate sum of KOMPAUN_AMAUN by month-year and KAWASAN
            a1 <- df1 %>%
              group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), TEMPAT_KAWASAN_NAMA) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_MONTHYEAR", "TEMPAT_KAWASAN_NAMA")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected month-year range
            filteredDataList <- reactive({
              start_monthyear <- floor_date(input$date1, "month")
              end_monthyear <- ceiling_date(input$date2, "month")
              filtered_data <- a2 %>%
                filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              
              # Group data by KAWASAN for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_KAWASAN_NAMA) %>%
                arrange(KOMPAUN_MONTHYEAR)  # Make sure data is ordered by month-year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_KAWASAN_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Bulan dan Kawasan") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- format(seq(floor_date(input$date1, "month"), ceiling_date(input$date2, "month"), by = "month"), format = "%b-%y")
            
            for (kawasan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kawasan]]
              chart <- chart %>% hc_add_series(
                name = kawasan,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          output$plotlain_1 <- renderHighchart({
            #KOMPAUN (TOP 5)----
            # Create a complete dataset of all combinations of DATE, KAWASAN_TEMPAT_NAMA, and KOMPAUN_AMAUN
            complete_data2 <- expand.grid(
              KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_KAWASAN_NAMA = unique(df1$TEMPAT_KAWASAN_NAMA),
              KOMPAUN_AMAUN = unique(df1$KOMPAUN_AMAUN)
            )
            
            # Count occurrence by date
            b1 <- df1 %>%
              group_by(KOMPAUN_TARIKH, TEMPAT_KAWASAN_NAMA, KOMPAUN_AMAUN) %>%
              summarize(Count = n(), .groups = "drop") %>%
              filter(TEMPAT_KAWASAN_NAMA != "TIADA REKOD")
            
            # Join the summarized data to the complete dataset and fill missing values with 0
            b2 <- complete_data2 %>%
              left_join(b1, by = c("KOMPAUN_TARIKH", "TEMPAT_KAWASAN_NAMA", "KOMPAUN_AMAUN")) %>%
              mutate(Count = replace_na(Count, 0))
            
            
            # Reactive function to filter data based on selected date range
            filteredData6 <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              subset(b2, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
            })
            # Group the data by TEMPAT and calculate the sum of COUNT
            grouped_data2 <- filteredData6() %>% 
              group_by(TEMPAT_KAWASAN_NAMA) %>% 
              summarise(total_count = sum(Count)) %>%
              arrange(desc(total_count)) %>%
              head(5) # Select top 5 highest areas
            
            # Create the stacked bar chart using highchart
            hchart(grouped_data2, "column", options3d = list(enabled = TRUE, beta = 15, alpha = 15), hcaes(x = TEMPAT_KAWASAN_NAMA, y = total_count)) %>%
              hc_chart(type = "column", backgroundColor = "#f7f7f7") %>%
              hc_title(text = "Top 5 Mengikut Kategori") %>%
              hc_plotOptions(
                column = list(
                  stacking = "normal",
                  borderRadiusTopLeft = 6,
                  borderRadiusTopRight = 6,
                  colorByPoint = TRUE,
                  dataLabels = list(enabled = TRUE, format = "{y}")
                )
              ) %>%
              hc_xAxis(title = list(text = "KATEGORI"), labels = list(rotation = -90, align = "right"), gridLineWidth = 0) %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE) %>%
              hc_tooltip(formatter = JS("function() { 
      return '<b>' + this.point.category + '</b><br>' + 'Kompaun: ' + this.y; 
    }"))
          })
          output$plotlain_2 <- renderHighchart({
            filteredData <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_df <- df1[df1$KOMPAUN_TARIKH >= start_date & df1$KOMPAUN_TARIKH <= end_date, ]
              if (input$dynamicChoice != "KESELURUHAN") {
                filtered_df <- filtered_df[filtered_df$TEMPAT_KAWASAN_NAMA == input$dynamicChoice, ]
              }
              filtered_df
            })
            
            # Calculate the count for each value of KOMPAUN_STATUS
            counts <- table(filteredData()$KOMPAUN_STATUS)
            # Create a dataframe for the pie chart
            pie_data <- data.frame(
              KOMPAUN_STATUS = names(counts),
              Count = as.numeric(counts)
            )
            
            # Create the pie chart using highchart
            highchart() %>%
              hc_chart(type = "pie", backgroundColor = "#f7f7f7") %>%
              hc_add_series(pie_data, type = "pie", hcaes(x = KOMPAUN_STATUS, y = Count)) %>%
              hc_title(text = "Bilangan Kompaun by STATUS") %>%
              hc_plotOptions(
                pie = list(
                  innerSize = "50%",
                  startAngle = 90,
                  endAngle = 90,
                  center = list('50%', '50%'),  # Adjust center to make it centered
                  size = '70%'
                )
              ) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE)
          })
          output$map <- renderLeaflet({
            # Define the bin size (10km = 0.09 degrees, adjust as needed)
            lat_bin_size <- 5
            lon_bin_size <- 5
            
            df1 <- df1 %>%
              mutate(
                lat_bin = round(TEMPAT_GPS_LATITUDE / lat_bin_size) * lat_bin_size,
                lon_bin = round(TEMPAT_GPS_LONGTITUDE / lon_bin_size) * lon_bin_size
              )
            df2 <- df1 %>%
              group_by(TEMPAT_GPS_LONGTITUDE, TEMPAT_GPS_LATITUDE) %>%
              summarise(count_of_kompaun = n())
            leaflet(data = df2) %>%
              addTiles() %>%
              addCircleMarkers(
                lng = ~TEMPAT_GPS_LONGTITUDE,
                lat = ~TEMPAT_GPS_LATITUDE,
                radius = ~count_of_kompaun,  # Calculate and set radius based on count_of_kompaun
                stroke = FALSE,
                fillOpacity = 0.7,
                label = ~paste("Count of Kompaun: ", count_of_kompaun),
                color = "blue"  # Set the marker color to red
              )
          })
        }
        
      } else if (input$JENIS_DATA == "Harian")
      {
        if(input$MENGIKUT == "Kesalahan")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice & !is.na(df1$KESALAHAN)])
            }
            
            # Create a complete dataset of all combinations of DATE and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_DATE = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              KESALAHAN = unique_kesalahan
            )
            
            # Count occurrence by date and KESALAHAN
            a1 <- df1 %>%
              group_by(KOMPAUN_DATE = KOMPAUN_TARIKH, KESALAHAN) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_DATE", "KESALAHAN")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected date range
            filteredDataList <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_data <- a2 %>%
                filter(KOMPAUN_DATE >= start_date & KOMPAUN_DATE <= end_date)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KESALAHAN) %>%
                arrange(KOMPAUN_DATE)  # Make sure data is ordered by date
              
              data_list <- split(grouped_data, grouped_data$KESALAHAN)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(
                categories = as.character(seq(floor_date(input$date1, "day"), ceiling_date(input$date2, "day"), by = "day")),
                labels = list(rotation = -45, align = "right")
              ) %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Hari dan Kesalahan") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            for (kesalahan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kesalahan]]
              chart <- chart %>% hc_add_series(
                name = kesalahan,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart
          })   
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice & !is.na(df1$KESALAHAN)])
            }
            
            # Create a complete dataset of all combinations of DATE and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_DATE = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              KESALAHAN = unique_kesalahan
            )
            
            # Calculate sum of KOMPAUN_AMAUN by date and KESALAHAN
            a1 <- df1 %>%
              group_by(KOMPAUN_DATE = KOMPAUN_TARIKH, KESALAHAN) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_DATE", "KESALAHAN")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected date range
            filteredDataList <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_data <- a2 %>%
                filter(KOMPAUN_DATE >= start_date & KOMPAUN_DATE <= end_date)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KESALAHAN) %>%
                arrange(KOMPAUN_DATE)  # Make sure data is ordered by date
              
              data_list <- split(grouped_data, grouped_data$KESALAHAN)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(
                categories = as.character(seq(floor_date(input$date1, "day"), ceiling_date(input$date2, "day"), by = "day")),
                labels = list(rotation = -45, align = "right")
              ) %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Hari dan Kesalahan") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            for (kesalahan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kesalahan]]
              chart <- chart %>% hc_add_series(
                name = kesalahan,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart
          })
          output$plotlain_1 <- renderHighchart({
            #KOMPAUN (TOP 5)----
            # Create a complete dataset of all combinations of DATE, KAWASAN_TEMPAT_NAMA, and KOMPAUN_AMAUN
            complete_data2 <- expand.grid(
              KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              KESALAHAN = unique(df1$KESALAHAN),
              KOMPAUN_AMAUN = unique(df1$KOMPAUN_AMAUN)
            )
            
            # Count occurrence by date
            b1 <- df1 %>%
              group_by(KOMPAUN_TARIKH, KESALAHAN, KOMPAUN_AMAUN) %>%
              summarize(Count = n(), .groups = "drop") %>%
              filter(KESALAHAN != "TIADA REKOD")
            
            # Join the summarized data to the complete dataset and fill missing values with 0
            b2 <- complete_data2 %>%
              left_join(b1, by = c("KOMPAUN_TARIKH", "KESALAHAN", "KOMPAUN_AMAUN")) %>%
              mutate(Count = replace_na(Count, 0))
            
            
            # Reactive function to filter data based on selected date range
            filteredData6 <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              subset(b2, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
            })
            # Group the data by TEMPAT and calculate the sum of COUNT
            grouped_data2 <- filteredData6() %>% 
              group_by(KESALAHAN) %>% 
              summarise(total_count = sum(Count)) %>%
              arrange(desc(total_count)) %>%
              head(5) # Select top 5 highest areas
            
            # Create the stacked bar chart using highchart
            hchart(grouped_data2, "column", options3d = list(enabled = TRUE, beta = 15, alpha = 15), hcaes(x = KESALAHAN, y = total_count)) %>%
              hc_chart(type = "column", backgroundColor = "#f7f7f7") %>%
              hc_title(text = "Top 5 Mengikut Kategori") %>%
              hc_plotOptions(
                column = list(
                  stacking = "normal",
                  borderRadiusTopLeft = 6,
                  borderRadiusTopRight = 6,
                  colorByPoint = TRUE,
                  dataLabels = list(enabled = TRUE, format = "{y}")
                )
              ) %>%
              hc_xAxis(title = list(text = "KATEGORI"), labels = list(rotation = -90, align = "right"), gridLineWidth = 0) %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE) %>%
              hc_tooltip(formatter = JS("function() { 
      return '<b>' + this.point.category + '</b><br>' + 'Kompaun: ' + this.y; 
    }"))
          })
          output$plotlain_2 <- renderHighchart({
            filteredData <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_df <- df1[df1$KOMPAUN_TARIKH >= start_date & df1$KOMPAUN_TARIKH <= end_date, ]
              if (input$dynamicChoice != "KESELURUHAN") {
                filtered_df <- filtered_df[filtered_df$KESALAHAN == input$dynamicChoice, ]
              }
              filtered_df
            })
            
            # Calculate the count for each value of KOMPAUN_STATUS
            counts <- table(filteredData()$KOMPAUN_STATUS)
            # Create a dataframe for the pie chart
            pie_data <- data.frame(
              KOMPAUN_STATUS = names(counts),
              Count = as.numeric(counts)
            )
            
            # Create the pie chart using highchart
            highchart() %>%
              hc_chart(type = "pie", backgroundColor = "#f7f7f7") %>%
              hc_add_series(pie_data, type = "pie", hcaes(x = KOMPAUN_STATUS, y = Count)) %>%
              hc_title(text = "Bilangan Kompaun by STATUS") %>%
              hc_plotOptions(
                pie = list(
                  innerSize = "50%",
                  startAngle = 90,
                  endAngle = 90,
                  center = list('50%', '50%'),  # Adjust center to make it centered
                  size = '70%'
                )
              ) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE)
          })
          output$map <- renderLeaflet({
            # Define the bin size (10km = 0.09 degrees, adjust as needed)
            lat_bin_size <- 5
            lon_bin_size <- 5
            
            df1 <- df1 %>%
              mutate(
                lat_bin = round(TEMPAT_GPS_LATITUDE / lat_bin_size) * lat_bin_size,
                lon_bin = round(TEMPAT_GPS_LONGTITUDE / lon_bin_size) * lon_bin_size
              )
            df2 <- df1 %>%
              group_by(TEMPAT_GPS_LONGTITUDE, TEMPAT_GPS_LATITUDE) %>%
              summarise(count_of_kompaun = n())
            leaflet(data = df2) %>%
              addTiles() %>%
              addCircleMarkers(
                lng = ~TEMPAT_GPS_LONGTITUDE,
                lat = ~TEMPAT_GPS_LATITUDE,
                radius = ~count_of_kompaun,  # Calculate and set radius based on count_of_kompaun
                stroke = FALSE,
                fillOpacity = 0.7,
                label = ~paste("Count of Kompaun: ", count_of_kompaun),
                color = "blue"  # Set the marker color to red
              )
          })

        } else if (input$MENGIKUT == "Tempat - Zon")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA == input$dynamicChoice & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            # Create a complete dataset of all combinations of DATE and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_DATE = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_ZON_NAMA = unique_zon
            )
            
            # Count occurrence by date and KESALAHAN
            a1 <- df1 %>%
              group_by(KOMPAUN_DATE = KOMPAUN_TARIKH, TEMPAT_ZON_NAMA) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_DATE", "TEMPAT_ZON_NAMA")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected date range
            filteredDataList <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_data <- a2 %>%
                filter(KOMPAUN_DATE >= start_date & KOMPAUN_DATE <= end_date)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_ZON_NAMA) %>%
                arrange(KOMPAUN_DATE)  # Make sure data is ordered by date
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_ZON_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(
                categories = as.character(seq(floor_date(input$date1, "day"), ceiling_date(input$date2, "day"), by = "day")),
                labels = list(rotation = -45, align = "right")
              ) %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Hari dan Zon") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            for (zon in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[zon]]
              chart <- chart %>% hc_add_series(
                name = zon,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart
          })  
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA == input$dynamicChoice & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            # Create a complete dataset of all combinations of DATE and ZON
            Kompaun_data1 <- expand.grid(
              KOMPAUN_DATE = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_ZON_NAMA = unique_zon
            )
            
            # Calculate sum of KOMPAUN_AMAUN by date and ZON
            a1 <- df1 %>%
              group_by(KOMPAUN_DATE = KOMPAUN_TARIKH, TEMPAT_ZON_NAMA) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_DATE", "TEMPAT_ZON_NAMA")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected date range
            filteredDataList <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_data <- a2 %>%
                filter(KOMPAUN_DATE >= start_date & KOMPAUN_DATE <= end_date)
              
              # Group data by ZON for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_ZON_NAMA) %>%
                arrange(KOMPAUN_DATE)  # Make sure data is ordered by date
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_ZON_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(
                categories = as.character(seq(floor_date(input$date1, "day"), ceiling_date(input$date2, "day"), by = "day")),
                labels = list(rotation = -45, align = "right")
              ) %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Hari dan Zon") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            for (zon in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[zon]]
              chart <- chart %>% hc_add_series(
                name = zon,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart
          })
          output$plotlain_1 <- renderHighchart({
            #KOMPAUN (TOP 5)----
            # Create a complete dataset of all combinations of DATE, KAWASAN_TEMPAT_NAMA, and KOMPAUN_AMAUN
            complete_data2 <- expand.grid(
              KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_ZON_NAMA = unique(df1$TEMPAT_ZON_NAMA),
              KOMPAUN_AMAUN = unique(df1$KOMPAUN_AMAUN)
            )
            
            # Count occurrence by date
            b1 <- df1 %>%
              group_by(KOMPAUN_TARIKH, TEMPAT_ZON_NAMA, KOMPAUN_AMAUN) %>%
              summarize(Count = n(), .groups = "drop") %>%
              filter(TEMPAT_ZON_NAMA != "TIADA REKOD")
            
            # Join the summarized data to the complete dataset and fill missing values with 0
            b2 <- complete_data2 %>%
              left_join(b1, by = c("KOMPAUN_TARIKH", "TEMPAT_ZON_NAMA", "KOMPAUN_AMAUN")) %>%
              mutate(Count = replace_na(Count, 0))
            
            
            # Reactive function to filter data based on selected date range
            filteredData6 <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              subset(b2, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
            })
            # Group the data by TEMPAT and calculate the sum of COUNT
            grouped_data2 <- filteredData6() %>% 
              group_by(TEMPAT_ZON_NAMA) %>% 
              summarise(total_count = sum(Count)) %>%
              arrange(desc(total_count)) %>%
              head(5) # Select top 5 highest areas
            
            # Create the stacked bar chart using highchart
            hchart(grouped_data2, "column", options3d = list(enabled = TRUE, beta = 15, alpha = 15), hcaes(x = TEMPAT_ZON_NAMA, y = total_count)) %>%
              hc_chart(type = "column", backgroundColor = "#f7f7f7") %>%
              hc_title(text = "Top 5 Mengikut Kategori") %>%
              hc_plotOptions(
                column = list(
                  stacking = "normal",
                  borderRadiusTopLeft = 6,
                  borderRadiusTopRight = 6,
                  colorByPoint = TRUE,
                  dataLabels = list(enabled = TRUE, format = "{y}")
                )
              ) %>%
              hc_xAxis(title = list(text = "KATEGORI"), labels = list(rotation = -90, align = "right"), gridLineWidth = 0) %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE) %>%
              hc_tooltip(formatter = JS("function() { 
      return '<b>' + this.point.category + '</b><br>' + 'Kompaun: ' + this.y; 
    }"))
          })
          output$plotlain_2 <- renderHighchart({
            filteredData <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_df <- df1[df1$KOMPAUN_TARIKH >= start_date & df1$KOMPAUN_TARIKH <= end_date, ]
              if (input$dynamicChoice != "KESELURUHAN") {
                filtered_df <- filtered_df[filtered_df$TEMPAT_ZON_NAMA == input$dynamicChoice, ]
              }
              filtered_df
            })
            
            # Calculate the count for each value of KOMPAUN_STATUS
            counts <- table(filteredData()$KOMPAUN_STATUS)
            # Create a dataframe for the pie chart
            pie_data <- data.frame(
              KOMPAUN_STATUS = names(counts),
              Count = as.numeric(counts)
            )
            
            # Create the pie chart using highchart
            highchart() %>%
              hc_chart(type = "pie", backgroundColor = "#f7f7f7") %>%
              hc_add_series(pie_data, type = "pie", hcaes(x = KOMPAUN_STATUS, y = Count)) %>%
              hc_title(text = "Bilangan Kompaun by STATUS") %>%
              hc_plotOptions(
                pie = list(
                  innerSize = "50%",
                  startAngle = 90,
                  endAngle = 90,
                  center = list('50%', '50%'),  # Adjust center to make it centered
                  size = '70%'
                )
              ) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE)
          })
          output$map <- renderLeaflet({
            # Define the bin size (10km = 0.09 degrees, adjust as needed)
            lat_bin_size <- 5
            lon_bin_size <- 5
            
            df1 <- df1 %>%
              mutate(
                lat_bin = round(TEMPAT_GPS_LATITUDE / lat_bin_size) * lat_bin_size,
                lon_bin = round(TEMPAT_GPS_LONGTITUDE / lon_bin_size) * lon_bin_size
              )
            df2 <- df1 %>%
              group_by(TEMPAT_GPS_LONGTITUDE, TEMPAT_GPS_LATITUDE) %>%
              summarise(count_of_kompaun = n())
            leaflet(data = df2) %>%
              addTiles() %>%
              addCircleMarkers(
                lng = ~TEMPAT_GPS_LONGTITUDE,
                lat = ~TEMPAT_GPS_LATITUDE,
                radius = ~count_of_kompaun,  # Calculate and set radius based on count_of_kompaun
                stroke = FALSE,
                fillOpacity = 0.7,
                label = ~paste("Count of Kompaun: ", count_of_kompaun),
                color = "blue"  # Set the marker color to red
              )
          })
        } else if (input$MENGIKUT == "Tempat - Kawasan")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA == input$dynamicChoice & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            # Create a complete dataset of all combinations of DATE and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_DATE = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_KAWASAN_NAMA = unique_kawasan
            )
            
            # Count occurrence by date and KESALAHAN
            a1 <- df1 %>%
              group_by(KOMPAUN_DATE = KOMPAUN_TARIKH, TEMPAT_KAWASAN_NAMA) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_DATE", "TEMPAT_KAWASAN_NAMA")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected date range
            filteredDataList <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_data <- a2 %>%
                filter(KOMPAUN_DATE >= start_date & KOMPAUN_DATE <= end_date)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_KAWASAN_NAMA) %>%
                arrange(KOMPAUN_DATE)  # Make sure data is ordered by date
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_KAWASAN_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(
                categories = as.character(seq(floor_date(input$date1, "day"), ceiling_date(input$date2, "day"), by = "day")),
                labels = list(rotation = -45, align = "right")
              ) %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Hari dan Kawasan") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            for (kawasan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kawasan]]
              chart <- chart %>% hc_add_series(
                name = kawasan,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart
          })
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA == input$dynamicChoice & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            # Create a complete dataset of all combinations of DATE and KAWASAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_DATE = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_KAWASAN_NAMA = unique_kawasan
            )
            
            # Calculate sum of KOMPAUN_AMAUN by date and KAWASAN
            a1 <- df1 %>%
              group_by(KOMPAUN_DATE = KOMPAUN_TARIKH, TEMPAT_KAWASAN_NAMA) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_DATE", "TEMPAT_KAWASAN_NAMA")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected date range
            filteredDataList <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_data <- a2 %>%
                filter(KOMPAUN_DATE >= start_date & KOMPAUN_DATE <= end_date)
              
              # Group data by KAWASAN for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_KAWASAN_NAMA) %>%
                arrange(KOMPAUN_DATE)  # Make sure data is ordered by date
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_KAWASAN_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(
                categories = as.character(seq(floor_date(input$date1, "day"), ceiling_date(input$date2, "day"), by = "day")),
                labels = list(rotation = -45, align = "right")
              ) %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Hari dan Kawasan") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            for (kawasan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kawasan]]
              chart <- chart %>% hc_add_series(
                name = kawasan,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart
          })
          output$plotlain_1 <- renderHighchart({
            #KOMPAUN (TOP 5)----
            # Create a complete dataset of all combinations of DATE, KAWASAN_TEMPAT_NAMA, and KOMPAUN_AMAUN
            complete_data2 <- expand.grid(
              KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_KAWASAN_NAMA = unique(df1$TEMPAT_KAWASAN_NAMA),
              KOMPAUN_AMAUN = unique(df1$KOMPAUN_AMAUN)
            )
            
            # Count occurrence by date
            b1 <- df1 %>%
              group_by(KOMPAUN_TARIKH, TEMPAT_KAWASAN_NAMA, KOMPAUN_AMAUN) %>%
              summarize(Count = n(), .groups = "drop") %>%
              filter(TEMPAT_KAWASAN_NAMA != "TIADA REKOD")
            
            # Join the summarized data to the complete dataset and fill missing values with 0
            b2 <- complete_data2 %>%
              left_join(b1, by = c("KOMPAUN_TARIKH", "TEMPAT_KAWASAN_NAMA", "KOMPAUN_AMAUN")) %>%
              mutate(Count = replace_na(Count, 0))
            
            
            # Reactive function to filter data based on selected date range
            filteredData6 <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              subset(b2, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
            })
            # Group the data by TEMPAT and calculate the sum of COUNT
            grouped_data2 <- filteredData6() %>% 
              group_by(TEMPAT_KAWASAN_NAMA) %>% 
              summarise(total_count = sum(Count)) %>%
              arrange(desc(total_count)) %>%
              head(5) # Select top 5 highest areas
            
            # Create the stacked bar chart using highchart
            hchart(grouped_data2, "column", options3d = list(enabled = TRUE, beta = 15, alpha = 15), hcaes(x = TEMPAT_KAWASAN_NAMA, y = total_count)) %>%
              hc_chart(type = "column", backgroundColor = "#f7f7f7") %>%
              hc_title(text = "Top 5 Mengikut Kategori") %>%
              hc_plotOptions(
                column = list(
                  stacking = "normal",
                  borderRadiusTopLeft = 6,
                  borderRadiusTopRight = 6,
                  colorByPoint = TRUE,
                  dataLabels = list(enabled = TRUE, format = "{y}")
                )
              ) %>%
              hc_xAxis(title = list(text = "KATEGORI"), labels = list(rotation = -90, align = "right"), gridLineWidth = 0) %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE) %>%
              hc_tooltip(formatter = JS("function() { 
      return '<b>' + this.point.category + '</b><br>' + 'Kompaun: ' + this.y; 
    }"))
          })
          output$plotlain_2 <- renderHighchart({
            filteredData <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_df <- df1[df1$KOMPAUN_TARIKH >= start_date & df1$KOMPAUN_TARIKH <= end_date, ]
              if (input$dynamicChoice != "KESELURUHAN") {
                filtered_df <- filtered_df[filtered_df$TEMPAT_KAWASAN_NAMA == input$dynamicChoice, ]
              }
              filtered_df
            })
            
            # Calculate the count for each value of KOMPAUN_STATUS
            counts <- table(filteredData()$KOMPAUN_STATUS)
            # Create a dataframe for the pie chart
            pie_data <- data.frame(
              KOMPAUN_STATUS = names(counts),
              Count = as.numeric(counts)
            )
            
            # Create the pie chart using highchart
            highchart() %>%
              hc_chart(type = "pie", backgroundColor = "#f7f7f7") %>%
              hc_add_series(pie_data, type = "pie", hcaes(x = KOMPAUN_STATUS, y = Count)) %>%
              hc_title(text = "Bilangan Kompaun by STATUS") %>%
              hc_plotOptions(
                pie = list(
                  innerSize = "50%",
                  startAngle = 90,
                  endAngle = 90,
                  center = list('50%', '50%'),  # Adjust center to make it centered
                  size = '70%'
                )
              ) %>%
              hc_legend(enabled = FALSE) %>%
              hc_exporting(enabled = TRUE)
          })
          output$map <- renderLeaflet({
            # Define the bin size (10km = 0.09 degrees, adjust as needed)
            lat_bin_size <- 5
            lon_bin_size <- 5
            
            df1 <- df1 %>%
              mutate(
                lat_bin = round(TEMPAT_GPS_LATITUDE / lat_bin_size) * lat_bin_size,
                lon_bin = round(TEMPAT_GPS_LONGTITUDE / lon_bin_size) * lon_bin_size
              )
            df2 <- df1 %>%
              group_by(TEMPAT_GPS_LONGTITUDE, TEMPAT_GPS_LATITUDE) %>%
              summarise(count_of_kompaun = n())
            leaflet(data = df2) %>%
              addTiles() %>%
              addCircleMarkers(
                lng = ~TEMPAT_GPS_LONGTITUDE,
                lat = ~TEMPAT_GPS_LATITUDE,
                radius = ~count_of_kompaun,  # Calculate and set radius based on count_of_kompaun
                stroke = FALSE,
                fillOpacity = 0.7,
                label = ~paste("Count of Kompaun: ", count_of_kompaun),
                color = "blue"  # Set the marker color to red
              )
          })    
        }
      }
      
    } else if(input$LPR_TAJUK == "Kompaun Tunggakan (Belum Bayar)")
    {
      
      
      output$plotlain_2 <- renderHighchart({
        filteredData <- reactive({
          start_date <- input$date1
          end_date <- input$date2
          filtered_df <- df1[df1$KOMPAUN_TARIKH >= start_date & df1$KOMPAUN_TARIKH <= end_date, ]
          if (input$dynamicChoice != "KESELURUHAN") {
            filtered_df <- filtered_df[filtered_df$KESALAHAN == input$dynamicChoice, ]
          }
          filtered_df
        })
        
        # Filter the data based on BAYARAN == "BELUM SELESAI"
        filteredDataBelumSelesai <- reactive({
          filteredData() %>%
            filter(BAYARAN == "BELUM SELESAI")
        })
        
        # Calculate the count for each value of KOMPAUN_STATUS
        counts <- table(filteredDataBelumSelesai()$KOMPAUN_STATUS)
        
        # Create a dataframe for the pie chart
        pie_data <- data.frame(
          KOMPAUN_STATUS = names(counts),
          Count = as.numeric(counts)
        )
        
        # Create the pie chart using highchart
        highchart() %>%
          hc_chart(type = "pie", backgroundColor = "transparent") %>%
          hc_add_series(pie_data, type = "pie", hcaes(x = KOMPAUN_STATUS, y = Count)) %>%
          hc_title(text = "Bilangan Kompaun by STATUS (BELUM SELESAI)") %>%
          hc_plotOptions(
            pie = list(
              innerSize = "50%",
              startAngle = 90,
              endAngle = 90,
              center = list('50%', '50%'),  # Adjust center to make it centered
              size = '70%'
            )
          ) %>%
          hc_legend(enabled = FALSE) %>%
          hc_exporting(enabled = TRUE)
      })
      
      cat("Kompaun Tunggakan (Belum Bayar)")
      #####################################################################VALUE BOX############################################################################
      if(input$MENGIKUT == "Kesalahan")
      {
        if (input$dynamicChoice == "KESELURUHAN")
        {
          # Render the value box for count of Kompaun
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            count_kompauns <- df3 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              nrow()
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Tunggakan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box for sum of Kompaun AMAUN
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            sum_kompauns <- df3 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN)) %>%
              pull(SumAmount)
            sum_kompauns <- paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum amount
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Tunggakan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          # Render the value box
          output$valueBox3 <- renderValueBox({
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(df3, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan<-paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } else 
        {
          # Render the value box for count of Kompaun
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice and BAYARAN "BELUM SELESAI"
            filtered_df <- df3 %>%
              filter(
                BAYARAN == "BELUM SELESAI",
                KESALAHAN == input$dynamicChoice
              )
            
            count_kompauns <- nrow(filtered_df)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Tunggakan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box for sum of Kompaun AMAUN
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice and BAYARAN "BELUM SELESAI"
            filtered_df <- df3 %>%
              filter(
                BAYARAN == "BELUM SELESAI",
                KESALAHAN == input$dynamicChoice
              )
            
            sum_kompauns <- filtered_df %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN)) %>%
              pull(SumAmount)
            sum_kompauns <- paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum amount
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Tunggakan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          # Render the value box
          output$valueBox3 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$KESALAHAN == input$dynamicChoice, ]
            
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(filtered_df, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan <- paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } 
      } else if (input$MENGIKUT == "Tempat - Zon")
      {
        if (input$dynamicChoice == "KESELURUHAN")
        {
          # Render the value box for count of Kompaun
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            count_kompauns <- df3 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              nrow()
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Tunggakan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          
          # Render the value box for sum of Kompaun AMAUN
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            sum_kompauns <- df3 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN)) %>%
              pull(SumAmount)
            sum_kompauns <- paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum amount
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Tunggakan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          # Render the value box
          output$valueBox3 <- renderValueBox({
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(df3, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan<-paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } else 
        {
          # Render the value box for count of Kompaun
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice and BAYARAN "BELUM SELESAI"
            filtered_df <- df3 %>%
              filter(
                BAYARAN == "BELUM SELESAI",
                TEMPAT_ZON_NAMA == input$dynamicChoice
              )
            
            count_kompauns <- nrow(filtered_df)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Tunggakan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box for sum of Kompaun AMAUN
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice and BAYARAN "BELUM SELESAI"
            filtered_df <- df3 %>%
              filter(
                BAYARAN == "BELUM SELESAI",
                TEMPAT_ZON_NAMA == input$dynamicChoice
              )
            
            sum_kompauns <- filtered_df %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN)) %>%
              pull(SumAmount)
            sum_kompauns <- paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum amount
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Tunggakan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          # Render the value box
          output$valueBox3 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_ZON_NAMA == input$dynamicChoice, ]
            
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(filtered_df, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan <- paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        }
      } else if (input$MENGIKUT == "Tempat - Kawasan")
      {
        if (input$dynamicChoice == "KESELURUHAN")
        {
          # Render the value box for count of Kompaun
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            count_kompauns <- df3 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              nrow()
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Tunggakan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box for sum of Kompaun AMAUN
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            sum_kompauns <- df3 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN)) %>%
              pull(SumAmount)
            sum_kompauns <- paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum amount
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Tunggakan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox3 <- renderValueBox({
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(df3, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan<-paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } else 
        {
          # Render the value box for count of Kompaun
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice and BAYARAN "BELUM SELESAI"
            filtered_df <- df3 %>%
              filter(
                BAYARAN == "BELUM SELESAI",
                TEMPAT_KAWASAN_NAMA == input$dynamicChoice
              )
            
            count_kompauns <- nrow(filtered_df)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Tunggakan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box for sum of Kompaun AMAUN
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice and BAYARAN "BELUM SELESAI"
            filtered_df <- df3 %>%
              filter(
                BAYARAN == "BELUM SELESAI",
                TEMPAT_KAWASAN_NAMA == input$dynamicChoice
              )
            
            sum_kompauns <- filtered_df %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN)) %>%
              pull(SumAmount)
            sum_kompauns <- paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum amount
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Tunggakan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          # Render the value box
          output$valueBox3 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_KAWASAN_NAMA == input$dynamicChoice, ]
            
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(filtered_df, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan <- paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } 
      }
      ########################################################################################################################################################
      if(input$JENIS_DATA == "Tahunan")
      {
        if(input$MENGIKUT == "Kesalahan")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice & !is.na(df1$KESALAHAN)])
            }
            
            # Create a complete dataset of all combinations of YEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(df1$KOMPAUN_TARIKH)), max(lubridate::year(df1$KOMPAUN_TARIKH)), by = 1),
              KESALAHAN = unique_kesalahan
            )
            
            # Count occurrence by year and KESALAHAN
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), KESALAHAN) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "KESALAHAN")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1)
              end_year <- lubridate::year(input$date2)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KESALAHAN) %>%
                arrange(KOMPAUN_YEAR)  # Make sure data is ordered by year
              
              data_list <- split(grouped_data, grouped_data$KESALAHAN)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Bilangan Kompaun Tunggakan Mengikut Tahun dan Kesalahan") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- seq(lubridate::year(input$date1), lubridate::year(input$date2), by = 1)
            
            for (kesalahan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kesalahan]]
              chart <- chart %>% hc_add_series(
                name = kesalahan,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice & !is.na(df1$KESALAHAN)])
            }
            
            # Create a complete dataset of all combinations of YEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(df1$KOMPAUN_TARIKH)), max(lubridate::year(df1$KOMPAUN_TARIKH)), by = 1),
              KESALAHAN = unique_kesalahan
            )
            
            # Calculate the sum of KOMPAUN_AMAUN by year and KESALAHAN
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), KESALAHAN) %>%
              summarise(Sum_Amount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "KESALAHAN")) %>%
              mutate(Sum_Amount = ifelse(is.na(Sum_Amount), 0, Sum_Amount))
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1)
              end_year <- lubridate::year(input$date2)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KESALAHAN) %>%
                arrange(KOMPAUN_YEAR)  # Make sure data is ordered by year
              
              data_list <- split(grouped_data, grouped_data$KESALAHAN)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Tunggakan Mengikut Tahun dan Kesalahan") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- seq(lubridate::year(input$date1), lubridate::year(input$date2), by = 1)
            
            for (kesalahan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kesalahan]]
              chart <- chart %>% hc_add_series(
                name = kesalahan,
                data = filtered_data$Sum_Amount,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          output$plotlain_1<-renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, KESALAHAN) %>%
              mutate(KOMPAUN_YEAR = floor_date(as.Date(KOMPAUN_TARIKH), unit = "year")) %>%
              filter(PINDAAN_TINDAKAN!="PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), KESALAHAN, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN) %>%
              summarise(TOTAL_BAYARAN= sum(TOTAL_BAYARAN))
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1)
                end_year <- lubridate::year(input$date2)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              })
              
            } else {
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1)
                end_year <- lubridate::year(input$date2)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year) %>%
                  filter(KESALAHAN == input$dynamicChoice)
              })
            }
            
            data_rayuan <- filteredDataList() %>%
              filter(PINDAAN_TINDAKAN == "RAYUAN") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(RAYUAN = sum(TOTAL_BAYARAN))
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(BELUM_SELESAI = sum(TOTAL_BAYARAN))
            
            data_combined <- left_join(data_rayuan, data_selesai, by = "KOMPAUN_YEAR") %>%
              left_join(data_belum, by = "KOMPAUN_YEAR")
            
            # Define the new category names you want
            new_categories <- data_combined$KOMPAUN_YEAR
            
            highchart() %>%
              hc_title(text = "Amaun bayaran dan rayuan mengikut Kawasan") %>%
              hc_xAxis(categories = new_categories, title = list(text = "Tahun")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "SELESAI", data = data_combined$SELESAI) %>%
              hc_add_series(type = "column", name = "BELUM SELESAI", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "RAYUAN", data = data_combined$RAYUAN) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{y:.0f}"  # Change this format as needed
                  )
                )
              )
            
            
          })
        } else if (input$MENGIKUT == "Tempat - Zon")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA== input$dynamicChoice & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            # Create a complete dataset of all combinations of YEAR and Zon
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(df1$KOMPAUN_TARIKH)), max(lubridate::year(df1$KOMPAUN_TARIKH)), by = 1),
              TEMPAT_ZON_NAMA = unique_zon
            )
            
            # Count occurrences of BAYARAN=="BELUM SELESAI" by year and Zon
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), TEMPAT_ZON_NAMA) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_ZON_NAMA")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1)
              end_year <- lubridate::year(input$date2)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by Zon for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_ZON_NAMA) %>%
                arrange(KOMPAUN_YEAR)  # Make sure data is ordered by year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_ZON_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Bilangan Kompaun Tunggakan Mengikut Tahun dan Zon") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- seq(lubridate::year(input$date1), lubridate::year(input$date2), by = 1)
            
            for (zon in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[zon]]
              chart <- chart %>% hc_add_series(
                name = zon,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA== input$dynamicChoice & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            # Create a complete dataset of all combinations of YEAR and Zon
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(df1$KOMPAUN_TARIKH)), max(lubridate::year(df1$KOMPAUN_TARIKH)), by = 1),
              TEMPAT_ZON_NAMA = unique_zon
            )
            
            # Calculate sum of KOMPAUN_AMAUN by year and Zon
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), TEMPAT_ZON_NAMA) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_ZON_NAMA")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1)
              end_year <- lubridate::year(input$date2)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by Zon for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_ZON_NAMA) %>%
                arrange(KOMPAUN_YEAR)  # Make sure data is ordered by year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_ZON_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Tunggakan Mengikut Tahun dan Zon") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- seq(lubridate::year(input$date1), lubridate::year(input$date2), by = 1)
            
            for (zon in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[zon]]
              chart <- chart %>% hc_add_series(
                name = zon,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })     
          output$plotlain_1<-renderHighchart({
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_ZON_NAMA) %>%
              mutate(KOMPAUN_YEAR = floor_date(as.Date(KOMPAUN_TARIKH), unit = "year")) %>%
              filter(PINDAAN_TINDAKAN!="PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), TEMPAT_ZON_NAMA, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN) %>%
              summarise(TOTAL_BAYARAN= sum(TOTAL_BAYARAN))
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1)
                end_year <- lubridate::year(input$date2)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              })
              
            } else {
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1)
                end_year <- lubridate::year(input$date2)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year) %>%
                  filter(TEMPAT_ZON_NAMA == input$dynamicChoice)
              })
            }
            
            data_rayuan <- filteredDataList() %>%
              filter(PINDAAN_TINDAKAN == "RAYUAN") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(RAYUAN = sum(TOTAL_BAYARAN))
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(BELUM_SELESAI = sum(TOTAL_BAYARAN))
            
            data_combined <- left_join(data_rayuan, data_selesai, by = "KOMPAUN_YEAR") %>%
              left_join(data_belum, by = "KOMPAUN_YEAR")
            
            # Define the new category names you want
            new_categories <- data_combined$KOMPAUN_YEAR
            
            highchart() %>%
              hc_title(text = "Amaun bayaran dan rayuan mengikut Zon") %>%
              hc_xAxis(categories = new_categories, title = list(text = "Tahun")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "SELESAI", data = data_combined$SELESAI) %>%
              hc_add_series(type = "column", name = "BELUM SELESAI", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "RAYUAN", data = data_combined$RAYUAN) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{y:.0f}"  # Change this format as needed
                  )
                )
              )
            
          })
          
        } else if (input$MENGIKUT == "Tempat - Kawasan")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA== input$dynamicChoice & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            # Create a complete dataset of all combinations of YEAR and Zon
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(df1$KOMPAUN_TARIKH)), max(lubridate::year(df1$KOMPAUN_TARIKH)), by = 1),
              TEMPAT_KAWASAN_NAMA = unique_kawasan
            )
            
            # Count occurrences of BAYARAN=="BELUM SELESAI" by year and Zon
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), TEMPAT_KAWASAN_NAMA) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_KAWASAN_NAMA")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1)
              end_year <- lubridate::year(input$date2)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by Zon for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_KAWASAN_NAMA) %>%
                arrange(KOMPAUN_YEAR)  # Make sure data is ordered by year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_KAWASAN_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Bilangan Kompaun Tunggakan Mengikut Tahun dan Kawasan") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- seq(lubridate::year(input$date1), lubridate::year(input$date2), by = 1)
            
            for (kawasan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kawasan]]
              chart <- chart %>% hc_add_series(
                name = kawasan,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA== input$dynamicChoice & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            # Create a complete dataset of all combinations of YEAR and Zon
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(df1$KOMPAUN_TARIKH)), max(lubridate::year(df1$KOMPAUN_TARIKH)), by = 1),
              TEMPAT_KAWASAN_NAMA = unique_kawasan
            )
            
            # Calculate sum of KOMPAUN_AMAUN by year and Zon
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), TEMPAT_KAWASAN_NAMA) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_KAWASAN_NAMA")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1)
              end_year <- lubridate::year(input$date2)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by Zon for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_KAWASAN_NAMA) %>%
                arrange(KOMPAUN_YEAR)  # Make sure data is ordered by year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_KAWASAN_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Tunggakan Mengikut Tahun dan Kawasan") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- seq(lubridate::year(input$date1), lubridate::year(input$date2), by = 1)
            
            for (kawasan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kawasan]]
              chart <- chart %>% hc_add_series(
                name = kawasan,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          
          output$plotlain_1<-renderHighchart({
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_KAWASAN_NAMA) %>%
              mutate(KOMPAUN_YEAR = floor_date(as.Date(KOMPAUN_TARIKH), unit = "year")) %>%
              filter(PINDAAN_TINDAKAN!="PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH), TEMPAT_KAWASAN_NAMA, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN) %>%
              summarise(TOTAL_BAYARAN= sum(TOTAL_BAYARAN))
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1)
                end_year <- lubridate::year(input$date2)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              })
              
            } else {
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1)
                end_year <- lubridate::year(input$date2)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year) %>%
                  filter(TEMPAT_KAWASAN_NAMA == input$dynamicChoice)
              })
            }
            
            data_rayuan <- filteredDataList() %>%
              filter(PINDAAN_TINDAKAN == "RAYUAN") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(RAYUAN = sum(TOTAL_BAYARAN))
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(BELUM_SELESAI = sum(TOTAL_BAYARAN))
            
            data_combined <- left_join(data_rayuan, data_selesai, by = "KOMPAUN_YEAR") %>%
              left_join(data_belum, by = "KOMPAUN_YEAR")
            
            # Define the new category names you want
            new_categories <- data_combined$KOMPAUN_YEAR
            
            highchart() %>%
              hc_title(text = "Amaun bayaran dan rayuan mengikut Kawasan") %>%
              hc_xAxis(categories = new_categories, title = list(text = "Tahun")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "SELESAI", data = data_combined$SELESAI) %>%
              hc_add_series(type = "column", name = "BELUM SELESAI", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "RAYUAN", data = data_combined$RAYUAN) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{y:.0f}"  # Change this format as needed
                  )
                )
              )
            
            
          })
          
        }
      } else if(input$JENIS_DATA == "Bulanan")
      {
        if(input$MENGIKUT == "Kesalahan")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice & !is.na(df1$KESALAHAN)])
            }
            
            # Create a complete dataset of all combinations of MONTHYEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_MONTHYEAR = seq(floor_date(min(df1$KOMPAUN_TARIKH), "month"), ceiling_date(max(df1$KOMPAUN_TARIKH), "month"), by = "month"),
              KESALAHAN = unique_kesalahan
            )
            
            # Count occurrence by month-year and KESALAHAN when BAYARAN is "BELUM SELESAI"
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), KESALAHAN) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_MONTHYEAR", "KESALAHAN")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected month-year range
            filteredDataList <- reactive({
              start_monthyear <- floor_date(input$date1, "month")
              end_monthyear <- ceiling_date(input$date2, "month")
              filtered_data <- a2 %>%
                filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KESALAHAN) %>%
                arrange(KOMPAUN_MONTHYEAR)  # Make sure data is ordered by month-year
              
              data_list <- split(grouped_data, grouped_data$KESALAHAN)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Bulan dan Kesalahan (Belum Selesai)") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- format(seq(floor_date(input$date1, "month"), ceiling_date(input$date2, "month"), by = "month"),format = "%b-%y")
            
            for (kesalahan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kesalahan]]
              chart <- chart %>% hc_add_series(
                name = kesalahan,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice & !is.na(df1$KESALAHAN)])
            }
            
            # Create a complete dataset of all combinations of MONTHYEAR and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_MONTHYEAR = seq(floor_date(min(df1$KOMPAUN_TARIKH), "month"), ceiling_date(max(df1$KOMPAUN_TARIKH), "month"), by = "month"),
              KESALAHAN = unique_kesalahan
            )
            
            # Calculate sum of KOMPAUN_AMAUN by month-year and KESALAHAN when BAYARAN is "BELUM SELESAI"
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), KESALAHAN) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_MONTHYEAR", "KESALAHAN")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected month-year range
            filteredDataList <- reactive({
              start_monthyear <- floor_date(input$date1, "month")
              end_monthyear <- ceiling_date(input$date2, "month")
              filtered_data <- a2 %>%
                filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KESALAHAN) %>%
                arrange(KOMPAUN_MONTHYEAR)  # Make sure data is ordered by month-year
              
              data_list <- split(grouped_data, grouped_data$KESALAHAN)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Bulan dan Kesalahan (Belum Selesai)") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- format(seq(floor_date(input$date1, "month"), ceiling_date(input$date2, "month"), by = "month"), format = "%b-%y")
            
            for (kesalahan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kesalahan]]
              chart <- chart %>% hc_add_series(
                name = kesalahan,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          
          output$plotlain_1<-renderHighchart({
            #group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), KESALAHAN) %>%
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, KESALAHAN) %>%
              mutate(KOMPAUN_MONTHYEAR = floor_date(as.Date(KOMPAUN_TARIKH), "month")) %>%
              filter(PINDAAN_TINDAKAN!="PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_MONTHYEAR, KESALAHAN, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN) %>%
              summarise(TOTAL_BAYARAN= sum(TOTAL_BAYARAN))
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_monthyear <- floor_date(input$date1, "month")
                end_monthyear <- floor_date(input$date2, "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              })
              
            } else {
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_monthyear <- floor_date(input$date1, "month")
                end_monthyear <- floor_date(input$date2, "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear) %>%
                  filter(KESALAHAN == input$dynamicChoice)
              })
            }
            
            data_rayuan <- filteredDataList() %>%
              filter(PINDAAN_TINDAKAN == "RAYUAN") %>%
              group_by(KOMPAUN_MONTHYEAR) %>%
              summarise(RAYUAN = sum(TOTAL_BAYARAN))
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_MONTHYEAR) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_MONTHYEAR) %>%
              summarise(BELUM_SELESAI = sum(TOTAL_BAYARAN))
            
            data_combined <- left_join(data_rayuan, data_selesai, by = "KOMPAUN_MONTHYEAR") %>%
              left_join(data_belum, by = "KOMPAUN_MONTHYEAR")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_MONTHYEAR, format = "%b-%y")
            
            highchart() %>%
              hc_title(text = "Amaun bayaran dan rayuan mengikut Kesalahan") %>%
              hc_xAxis(categories = new_categories, title = list(text = "Bulan")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "SELESAI", data = data_combined$SELESAI) %>%
              hc_add_series(type = "column", name = "BELUM SELESAI", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "RAYUAN", data = data_combined$RAYUAN) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{y:.0f}"  # Change this format as needed
                  )
                )
              )
            
            
          })
          
        } else if (input$MENGIKUT == "Tempat - Zon")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA== input$dynamicChoice & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            # Create a complete dataset of all combinations of MONTHYEAR and ZON
            Kompaun_data1 <- expand.grid(
              KOMPAUN_MONTHYEAR = seq(floor_date(min(df1$KOMPAUN_TARIKH), "month"), ceiling_date(max(df1$KOMPAUN_TARIKH), "month"), by = "month"),
              TEMPAT_ZON_NAMA = unique_zon
            )
            
            # Count occurrence by month-year and ZON when BAYARAN is "BELUM SELESAI"
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), TEMPAT_ZON_NAMA) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_MONTHYEAR", "TEMPAT_ZON_NAMA")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected month-year range
            filteredDataList <- reactive({
              start_monthyear <- floor_date(input$date1, "month")
              end_monthyear <- ceiling_date(input$date2, "month")
              filtered_data <- a2 %>%
                filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              
              # Group data by ZON for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_ZON_NAMA) %>%
                arrange(KOMPAUN_MONTHYEAR)  # Make sure data is ordered by month-year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_ZON_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Bulan dan Zon (Belum Selesai)") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- format(seq(floor_date(input$date1, "month"), ceiling_date(input$date2, "month"), by = "month"), format = "%b-%y")
            
            for (zon in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[zon]]
              chart <- chart %>% hc_add_series(
                name = zon,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA== input$dynamicChoice & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            # Create a complete dataset of all combinations of MONTHYEAR and ZON
            Kompaun_data1 <- expand.grid(
              KOMPAUN_MONTHYEAR = seq(floor_date(min(df1$KOMPAUN_TARIKH), "month"), ceiling_date(max(df1$KOMPAUN_TARIKH), "month"), by = "month"),
              TEMPAT_ZON_NAMA = unique_zon
            )
            
            # Calculate sum of KOMPAUN_AMAUN by month-year and ZON when BAYARAN is "BELUM SELESAI"
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), TEMPAT_ZON_NAMA) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_MONTHYEAR", "TEMPAT_ZON_NAMA")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected month-year range
            filteredDataList <- reactive({
              start_monthyear <- floor_date(input$date1, "month")
              end_monthyear <- ceiling_date(input$date2, "month")
              filtered_data <- a2 %>%
                filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              
              # Group data by ZON for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_ZON_NAMA) %>%
                arrange(KOMPAUN_MONTHYEAR)  # Make sure data is ordered by month-year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_ZON_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Bulan dan Zon (Belum Selesai)") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- format(seq(floor_date(input$date1, "month"), ceiling_date(input$date2, "month"), by = "month"), format = "%b-%y")
            
            for (zon in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[zon]]
              chart <- chart %>% hc_add_series(
                name = zon,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          
          output$plotlain_1<-renderHighchart({
            #group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), KESALAHAN) %>%
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_ZON_NAMA) %>%
              mutate(KOMPAUN_MONTHYEAR = floor_date(as.Date(KOMPAUN_TARIKH), "month")) %>%
              filter(PINDAAN_TINDAKAN!="PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_MONTHYEAR, TEMPAT_ZON_NAMA, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN) %>%
              summarise(TOTAL_BAYARAN= sum(TOTAL_BAYARAN))
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_monthyear <- floor_date(input$date1, "month")
                end_monthyear <- floor_date(input$date2, "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              })
              
            } else {
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_monthyear <- floor_date(input$date1, "month")
                end_monthyear <- floor_date(input$date2, "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear) %>%
                  filter(TEMPAT_ZON_NAMA) == input$dynamicChoice
              })
            }
            
            data_rayuan <- filteredDataList() %>%
              filter(PINDAAN_TINDAKAN == "RAYUAN") %>%
              group_by(KOMPAUN_MONTHYEAR) %>%
              summarise(RAYUAN = sum(TOTAL_BAYARAN))
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_MONTHYEAR) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_MONTHYEAR) %>%
              summarise(BELUM_SELESAI = sum(TOTAL_BAYARAN))
            
            data_combined <- left_join(data_rayuan, data_selesai, by = "KOMPAUN_MONTHYEAR") %>%
              left_join(data_belum, by = "KOMPAUN_MONTHYEAR")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_MONTHYEAR, format = "%b-%y")
            
            highchart() %>%
              hc_title(text = "Amaun bayaran dan rayuan mengikut Zon") %>%
              hc_xAxis(categories = new_categories, title = list(text = "Bulan")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "SELESAI", data = data_combined$SELESAI) %>%
              hc_add_series(type = "column", name = "BELUM SELESAI", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "RAYUAN", data = data_combined$RAYUAN) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{y:.0f}"  # Change this format as needed
                  )
                )
              )
            
            
          })
          
        } else if (input$MENGIKUT == "Tempat - Kawasan")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA== input$dynamicChoice & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            # Create a complete dataset of all combinations of MONTHYEAR and KAWASAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_MONTHYEAR = seq(floor_date(min(df1$KOMPAUN_TARIKH), "month"), ceiling_date(max(df1$KOMPAUN_TARIKH), "month"), by = "month"),
              TEMPAT_KAWASAN_NAMA = unique_kawasan
            )
            
            # Count occurrence by month-year and KAWASAN when BAYARAN is "BELUM SELESAI"
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), TEMPAT_KAWASAN_NAMA) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_MONTHYEAR", "TEMPAT_KAWASAN_NAMA")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected month-year range
            filteredDataList <- reactive({
              start_monthyear <- floor_date(input$date1, "month")
              end_monthyear <- ceiling_date(input$date2, "month")
              filtered_data <- a2 %>%
                filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              
              # Group data by KAWASAN for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_KAWASAN_NAMA) %>%
                arrange(KOMPAUN_MONTHYEAR)  # Make sure data is ordered by month-year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_KAWASAN_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Bulan dan Kawasan (Belum Selesai)") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- format(seq(floor_date(input$date1, "month"), ceiling_date(input$date2, "month"), by = "month"), format = "%b-%y")
            
            for (kawasan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kawasan]]
              chart <- chart %>% hc_add_series(
                name = kawasan,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA== input$dynamicChoice & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            # Create a complete dataset of all combinations of MONTHYEAR and KAWASAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_MONTHYEAR = seq(floor_date(min(df1$KOMPAUN_TARIKH), "month"), ceiling_date(max(df1$KOMPAUN_TARIKH), "month"), by = "month"),
              TEMPAT_KAWASAN_NAMA = unique_kawasan
            )
            
            # Calculate sum of KOMPAUN_AMAUN by month-year and KAWASAN when BAYARAN is "BELUM SELESAI"
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), TEMPAT_KAWASAN_NAMA) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_MONTHYEAR", "TEMPAT_KAWASAN_NAMA")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected month-year range
            filteredDataList <- reactive({
              start_monthyear <- floor_date(input$date1, "month")
              end_monthyear <- ceiling_date(input$date2, "month")
              filtered_data <- a2 %>%
                filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              
              # Group data by KAWASAN for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_KAWASAN_NAMA) %>%
                arrange(KOMPAUN_MONTHYEAR)  # Make sure data is ordered by month-year
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_KAWASAN_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(type = "category") %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Bulan dan Kawasan (Belum Selesai)") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            categories <- format(seq(floor_date(input$date1, "month"), ceiling_date(input$date2, "month"), by = "month"), format = "%b-%y")
            
            for (kawasan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kawasan]]
              chart <- chart %>% hc_add_series(
                name = kawasan,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart %>% hc_xAxis(categories = categories)
          })
          
          output$plotlain_1<-renderHighchart({
            #group_by(KOMPAUN_MONTHYEAR = floor_date(KOMPAUN_TARIKH, "month"), KESALAHAN) %>%
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_KAWASAN_NAMA) %>%
              mutate(KOMPAUN_MONTHYEAR = floor_date(as.Date(KOMPAUN_TARIKH), "month")) %>%
              filter(PINDAAN_TINDAKAN!="PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_MONTHYEAR, TEMPAT_KAWASAN_NAMA, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN) %>%
              summarise(TOTAL_BAYARAN= sum(TOTAL_BAYARAN))
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_monthyear <- floor_date(input$date1, "month")
                end_monthyear <- floor_date(input$date2, "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear)
              })
              
            } else {
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_monthyear <- floor_date(input$date1, "month")
                end_monthyear <- floor_date(input$date2, "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTHYEAR >= start_monthyear & KOMPAUN_MONTHYEAR <= end_monthyear) %>%
                  filter(TEMPAT_KAWASAN_NAMA) == input$dynamicChoice
              })
            }
            
            data_rayuan <- filteredDataList() %>%
              filter(PINDAAN_TINDAKAN == "RAYUAN") %>%
              group_by(KOMPAUN_MONTHYEAR) %>%
              summarise(RAYUAN = sum(TOTAL_BAYARAN))
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_MONTHYEAR) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_MONTHYEAR) %>%
              summarise(BELUM_SELESAI = sum(TOTAL_BAYARAN))
            
            data_combined <- left_join(data_rayuan, data_selesai, by = "KOMPAUN_MONTHYEAR") %>%
              left_join(data_belum, by = "KOMPAUN_MONTHYEAR")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_MONTHYEAR, format = "%b-%y")
            
            highchart() %>%
              hc_title(text = "Amaun bayaran dan rayuan mengikut Kawasan") %>%
              hc_xAxis(categories = new_categories, title = list(text = "Bulan")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "SELESAI", data = data_combined$SELESAI) %>%
              hc_add_series(type = "column", name = "BELUM SELESAI", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "RAYUAN", data = data_combined$RAYUAN) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{y:.0f}"  # Change this format as needed
                  )
                )
              )
            
            
          })
        }
        
      } else if (input$JENIS_DATA == "Harian")
      {
        if(input$MENGIKUT == "Kesalahan")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice & !is.na(df1$KESALAHAN)])
            }
            
            # Create a complete dataset of all combinations of DATE and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_DATE = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              KESALAHAN = unique_kesalahan
            )
            
            # Count occurrence by date and KESALAHAN when BAYARAN is "BELUM SELESAI"
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_DATE = KOMPAUN_TARIKH, KESALAHAN) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_DATE", "KESALAHAN")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected date range
            filteredDataList <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_data <- a2 %>%
                filter(KOMPAUN_DATE >= start_date & KOMPAUN_DATE <= end_date)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KESALAHAN) %>%
                arrange(KOMPAUN_DATE)  # Make sure data is ordered by date
              
              data_list <- split(grouped_data, grouped_data$KESALAHAN)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(
                categories = as.character(seq(floor_date(input$date1, "day"), ceiling_date(input$date2, "day"), by = "day")),
                labels = list(rotation = -45, align = "right")
              ) %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Hari dan Kesalahan (Belum Selesai)") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            for (kesalahan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kesalahan]]
              chart <- chart %>% hc_add_series(
                name = kesalahan,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart
          })
          
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice & !is.na(df1$KESALAHAN)])
            }
            
            # Create a complete dataset of all combinations of DATE and KESALAHAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_DATE = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              KESALAHAN = unique_kesalahan
            )
            
            # Calculate sum of KOMPAUN_AMAUN by date and KESALAHAN when BAYARAN is "BELUM SELESAI"
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_DATE = KOMPAUN_TARIKH, KESALAHAN) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_DATE", "KESALAHAN")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected date range
            filteredDataList <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_data <- a2 %>%
                filter(KOMPAUN_DATE >= start_date & KOMPAUN_DATE <= end_date)
              
              # Group data by KESALAHAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KESALAHAN) %>%
                arrange(KOMPAUN_DATE)  # Make sure data is ordered by date
              
              data_list <- split(grouped_data, grouped_data$KESALAHAN)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(
                categories = as.character(seq(floor_date(input$date1, "day"), ceiling_date(input$date2, "day"), by = "day")),
                labels = list(rotation = -45, align = "right")
              ) %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Hari dan Kesalahan (Belum Selesai)") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            for (kesalahan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kesalahan]]
              chart <- chart %>% hc_add_series(
                name = kesalahan,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart
          })
          
          output$plotlain_1<-renderHighchart({
            
            Kompaun_data1 <- expand.grid(
              KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              KESALAHAN = unique(df1$KESALAHAN)
            )
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, KESALAHAN) %>%
              filter(PINDAAN_TINDAKAN!="PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_TARIKH, KESALAHAN, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN) %>%
              summarise(TOTAL_BAYARAN= sum(TOTAL_BAYARAN))
            
            a3 <- Kompaun_data1 %>%
              left_join(a2, by = c("KOMPAUN_TARIKH", "KESALAHAN")) %>%
              mutate(TOTAL_BAYARAN = ifelse(is.na(TOTAL_BAYARAN), 0, TOTAL_BAYARAN))
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- input$date1
                end_year <- input$date2
                filtered_data <- a3 %>%
                  filter(KOMPAUN_TARIKH >= start_year & KOMPAUN_TARIKH <= end_year)
              })
              
            } else {
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- input$date1
                end_year <- input$date2
                filtered_data <- a3 %>%
                  filter(KOMPAUN_TARIKH >= start_year & KOMPAUN_TARIKH <= end_year) %>%
                  filter(KESALAHAN == input$dynamicChoice)
              })
            }
            
            data_rayuan <- filteredDataList() %>%
              filter(PINDAAN_TINDAKAN == "RAYUAN") %>%
              group_by(KOMPAUN_TARIKH) %>%
              summarise(RAYUAN = sum(TOTAL_BAYARAN))
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_TARIKH) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_TARIKH) %>%
              summarise(BELUM_SELESAI = sum(TOTAL_BAYARAN))
            
            data_combined <- left_join(data_rayuan, data_selesai, by = "KOMPAUN_TARIKH") %>%
              left_join(data_belum, by = "KOMPAUN_TARIKH")
            
            # Define the new category names you want
            new_categories <- data_combined$KOMPAUN_TARIKH
            
            highchart() %>%
              hc_title(text = "Amaun bayaran dan rayuan mengikut Kesalahan") %>%
              hc_xAxis(categories = new_categories, title = list(text = "Tarikh")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "SELESAI", data = data_combined$SELESAI) %>%
              hc_add_series(type = "column", name = "BELUM SELESAI", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "RAYUAN", data = data_combined$RAYUAN) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{y:.0f}"  # Change this format as needed
                  )
                )
              )
          })
          
        } else if (input$MENGIKUT == "Tempat - Zon")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA== input$dynamicChoice & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            # Create a complete dataset of all combinations of DATE and ZON
            Kompaun_data1 <- expand.grid(
              KOMPAUN_DATE = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_ZON_NAMA = unique_zon
            )
            
            # Count occurrence by date and ZON when BAYARAN is "BELUM SELESAI"
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_DATE = KOMPAUN_TARIKH, TEMPAT_ZON_NAMA) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_DATE", "TEMPAT_ZON_NAMA")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected date range
            filteredDataList <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_data <- a2 %>%
                filter(KOMPAUN_DATE >= start_date & KOMPAUN_DATE <= end_date)
              
              # Group data by ZON for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_ZON_NAMA) %>%
                arrange(KOMPAUN_DATE)  # Make sure data is ordered by date
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_ZON_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(
                categories = as.character(seq(floor_date(input$date1, "day"), ceiling_date(input$date2, "day"), by = "day")),
                labels = list(rotation = -45, align = "right")
              ) %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Hari dan Zon (Belum Selesai)") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            for (zon in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[zon]]
              chart <- chart %>% hc_add_series(
                name = zon,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart
          })
          
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA== input$dynamicChoice & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            # Create a complete dataset of all combinations of DATE and ZON
            Kompaun_data1 <- expand.grid(
              KOMPAUN_DATE = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_ZON_NAMA = unique_zon
            )
            
            # Calculate sum of KOMPAUN_AMAUN by date and ZON when BAYARAN is "BELUM SELESAI"
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_DATE = KOMPAUN_TARIKH, TEMPAT_ZON_NAMA) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_DATE", "TEMPAT_ZON_NAMA")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected date range
            filteredDataList <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_data <- a2 %>%
                filter(KOMPAUN_DATE >= start_date & KOMPAUN_DATE <= end_date)
              
              # Group data by ZON for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_ZON_NAMA) %>%
                arrange(KOMPAUN_DATE)  # Make sure data is ordered by date
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_ZON_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(
                categories = as.character(seq(floor_date(input$date1, "day"), ceiling_date(input$date2, "day"), by = "day")),
                labels = list(rotation = -45, align = "right")
              ) %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Hari dan Zon (Belum Selesai)") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            for (zon in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[zon]]
              chart <- chart %>% hc_add_series(
                name = zon,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart
          })
          
          output$plotlain_1<-renderHighchart({
            
            Kompaun_data1 <- expand.grid(
              KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_ZON_NAMA = unique(df1$TEMPAT_ZON_NAMA)
            )
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_ZON_NAMA) %>%
              filter(PINDAAN_TINDAKAN!="PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_TARIKH, TEMPAT_ZON_NAMA, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN) %>%
              summarise(TOTAL_BAYARAN= sum(TOTAL_BAYARAN))
            
            a3 <- Kompaun_data1 %>%
              left_join(a2, by = c("KOMPAUN_TARIKH", "TEMPAT_ZON_NAMA")) %>%
              mutate(TOTAL_BAYARAN = ifelse(is.na(TOTAL_BAYARAN), 0, TOTAL_BAYARAN))
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- input$date1
                end_year <- input$date2
                filtered_data <- a3 %>%
                  filter(KOMPAUN_TARIKH >= start_year & KOMPAUN_TARIKH <= end_year)
              })
              
            } else {
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- input$date1
                end_year <- input$date2
                filtered_data <- a3 %>%
                  filter(KOMPAUN_TARIKH >= start_year & KOMPAUN_TARIKH <= end_year) %>%
                  filter(TEMPAT_ZON_NAMA == input$dynamicChoice)
              })
            }
            
            data_rayuan <- filteredDataList() %>%
              filter(PINDAAN_TINDAKAN == "RAYUAN") %>%
              group_by(KOMPAUN_TARIKH) %>%
              summarise(RAYUAN = sum(TOTAL_BAYARAN))
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_TARIKH) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_TARIKH) %>%
              summarise(BELUM_SELESAI = sum(TOTAL_BAYARAN))
            
            data_combined <- left_join(data_rayuan, data_selesai, by = "KOMPAUN_TARIKH") %>%
              left_join(data_belum, by = "KOMPAUN_TARIKH")
            
            # Define the new category names you want
            new_categories <- data_combined$KOMPAUN_TARIKH
            
            highchart() %>%
              hc_title(text = "Amaun bayaran dan rayuan mengikut Zon") %>%
              hc_xAxis(categories = new_categories, title = list(text = "Tarikh")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "SELESAI", data = data_combined$SELESAI) %>%
              hc_add_series(type = "column", name = "BELUM SELESAI", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "RAYUAN", data = data_combined$RAYUAN) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{y:.0f}"  # Change this format as needed
                  )
                )
              )
          })
          
        } else if (input$MENGIKUT == "Tempat - Kawasan")
        {
          output$line_plotKompaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA== input$dynamicChoice & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            # Create a complete dataset of all combinations of DATE and KAWASAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_DATE = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_KAWASAN_NAMA = unique_kawasan
            )
            
            # Count occurrence by date and KAWASAN when BAYARAN is "BELUM SELESAI"
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_DATE = KOMPAUN_TARIKH, TEMPAT_KAWASAN_NAMA) %>%
              summarise(Count = n(), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_DATE", "TEMPAT_KAWASAN_NAMA")) %>%
              mutate(Count = ifelse(is.na(Count), 0, Count))
            
            # Reactive function to filter data based on selected date range
            filteredDataList <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_data <- a2 %>%
                filter(KOMPAUN_DATE >= start_date & KOMPAUN_DATE <= end_date)
              
              # Group data by KAWASAN for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_KAWASAN_NAMA) %>%
                arrange(KOMPAUN_DATE)  # Make sure data is ordered by date
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_KAWASAN_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(
                categories = as.character(seq(floor_date(input$date1, "day"), ceiling_date(input$date2, "day"), by = "day")),
                labels = list(rotation = -45, align = "right")
              ) %>%
              hc_title(text = "Graf: Bilangan Kompaun Mengikut Hari dan Kawasan (Belum Selesai)") %>%
              hc_yAxis(title = list(text = "BILANGAN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            for (kawasan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kawasan]]
              chart <- chart %>% hc_add_series(
                name = kawasan,
                data = filtered_data$Count,
                type = "spline"
              )
            }
            
            chart
          })
          
          output$line_plotKompaunAmaun <- renderHighchart({
            if(input$dynamicChoice == "KESELURUHAN")
            {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA== input$dynamicChoice & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            # Create a complete dataset of all combinations of DATE and KAWASAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_DATE = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_KAWASAN_NAMA = unique_kawasan
            )
            
            # Calculate sum of KOMPAUN_AMAUN by date and KAWASAN when BAYARAN is "BELUM SELESAI"
            a1 <- df1 %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_DATE = KOMPAUN_TARIKH, TEMPAT_KAWASAN_NAMA) %>%
              summarise(SumAmount = sum(KOMPAUN_AMAUN), .groups = "keep")
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_DATE", "TEMPAT_KAWASAN_NAMA")) %>%
              mutate(SumAmount = ifelse(is.na(SumAmount), 0, SumAmount))
            
            # Reactive function to filter data based on selected date range
            filteredDataList <- reactive({
              start_date <- input$date1
              end_date <- input$date2
              filtered_data <- a2 %>%
                filter(KOMPAUN_DATE >= start_date & KOMPAUN_DATE <= end_date)
              
              # Group data by KAWASAN for separate series
              grouped_data <- filtered_data %>%
                group_by(TEMPAT_KAWASAN_NAMA) %>%
                arrange(KOMPAUN_DATE)  # Make sure data is ordered by date
              
              data_list <- split(grouped_data, grouped_data$TEMPAT_KAWASAN_NAMA)
              data_list
            })
            
            chart <- highchart() %>%
              hc_xAxis(
                categories = as.character(seq(floor_date(input$date1, "day"), ceiling_date(input$date2, "day"), by = "day")),
                labels = list(rotation = -45, align = "right")
              ) %>%
              hc_title(text = "Graf: Jumlah Amaun Kompaun Mengikut Hari dan Kawasan (Belum Selesai)") %>%
              hc_yAxis(title = list(text = "JUMLAH AMAUN KOMPAUN")) %>%
              hc_exporting(enabled = TRUE)
            
            for (kawasan in names(filteredDataList())) {
              filtered_data <- filteredDataList()[[kawasan]]
              chart <- chart %>% hc_add_series(
                name = kawasan,
                data = filtered_data$SumAmount,
                type = "spline"
              )
            }
            
            chart
          })
          
          output$plotlain_1<-renderHighchart({
            
            Kompaun_data1 <- expand.grid(
              KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
              TEMPAT_KAWASAN_NAMA = unique(df1$TEMPAT_KAWASAN_NAMA)
            )
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_KAWASAN_NAMA) %>%
              filter(PINDAAN_TINDAKAN!="PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_TARIKH, TEMPAT_KAWASAN_NAMA, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN) %>%
              summarise(TOTAL_BAYARAN= sum(TOTAL_BAYARAN))
            
            a3 <- Kompaun_data1 %>%
              left_join(a2, by = c("KOMPAUN_TARIKH", "TEMPAT_KAWASAN_NAMA")) %>%
              mutate(TOTAL_BAYARAN = ifelse(is.na(TOTAL_BAYARAN), 0, TOTAL_BAYARAN))
            
            if(input$dynamicChoice == "KESELURUHAN")
            {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- input$date1
                end_year <- input$date2
                filtered_data <- a3 %>%
                  filter(KOMPAUN_TARIKH >= start_year & KOMPAUN_TARIKH <= end_year)
              })
              
            } else {
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- input$date1
                end_year <- input$date2
                filtered_data <- a3 %>%
                  filter(KOMPAUN_TARIKH >= start_year & KOMPAUN_TARIKH <= end_year) %>%
                  filter(TEMPAT_KAWASAN_NAMA == input$dynamicChoice)
              })
            }
            
            data_rayuan <- filteredDataList() %>%
              filter(PINDAAN_TINDAKAN == "RAYUAN") %>%
              group_by(KOMPAUN_TARIKH) %>%
              summarise(RAYUAN = sum(TOTAL_BAYARAN))
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_TARIKH) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              filter(BAYARAN == "BELUM SELESAI") %>%
              group_by(KOMPAUN_TARIKH) %>%
              summarise(BELUM_SELESAI = sum(TOTAL_BAYARAN))
            
            data_combined <- left_join(data_rayuan, data_selesai, by = "KOMPAUN_TARIKH") %>%
              left_join(data_belum, by = "KOMPAUN_TARIKH")
            
            # Define the new category names you want
            new_categories <- data_combined$KOMPAUN_TARIKH
            
            highchart() %>%
              hc_title(text = "Amaun bayaran dan rayuan mengikut Kawasan") %>%
              hc_xAxis(categories = new_categories, title = list(text = "Tarikh")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "SELESAI", data = data_combined$SELESAI) %>%
              hc_add_series(type = "column", name = "BELUM SELESAI", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "RAYUAN", data = data_combined$RAYUAN) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{y:.0f}"  # Change this format as needed
                  )
                )
              )
          })
        }
      }
    }
  })
  
  observeEvent(input$submitKutipan, {
    if(input$LPR_TAJUK_KUTIPAN == "Jumlah Kutipan")
    {
      #################################################################### VALUE BOX ###########################################################################
      if(input$MENGIKUT == "Kesalahan")
      {
        if (input$dynamicChoice == "KESELURUHAN")
        {
          # Render the value box
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            count_kompauns <- nrow(df3)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            sum_kompauns<-sum(df3$KOMPAUN_AMAUN)
            sum_kompauns<-paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox3 <- renderValueBox({
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(df3, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan<-paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } else 
        {
          # Render the value box
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$KESALAHAN == input$dynamicChoice, ]
            cat("Pilihan:", input$dynamicChoice)
            count_kompauns <- nrow(filtered_df)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$KESALAHAN == input$dynamicChoice, ]
            
            sum_kompauns <- sum(filtered_df$KOMPAUN_AMAUN)
            sum_kompauns <- paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox3 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$KESALAHAN == input$dynamicChoice, ]
            
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(filtered_df, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan <- paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } 
      } else if (input$MENGIKUT == "Tempat - Zon")
      {
        if (input$dynamicChoice == "KESELURUHAN")
        {
          cat("Masuk sini")
          # Render the value box
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            count_kompauns <- nrow(df3)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            sum_kompauns<-sum(df3$KOMPAUN_AMAUN)
            sum_kompauns<-paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox3 <- renderValueBox({
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(df3, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan<-paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } else 
        {
          # Render the value box
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_ZON_NAMA == input$dynamicChoice, ]
            cat("Pilihan:", input$dynamicChoice)
            count_kompauns <- nrow(filtered_df)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_ZON_NAMA == input$dynamicChoice, ]
            
            sum_kompauns <- sum(filtered_df$KOMPAUN_AMAUN)
            sum_kompauns <- paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox3 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_ZON_NAMA == input$dynamicChoice, ]
            
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(filtered_df, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan <- paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        }
      } else if (input$MENGIKUT == "Tempat - Kawasan")
      {
        if (input$dynamicChoice == "KESELURUHAN")
        {
          cat("Masuk sini")
          # Render the value box
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            count_kompauns <- nrow(df3)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            sum_kompauns<-sum(df3$KOMPAUN_AMAUN)
            sum_kompauns<-paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox3 <- renderValueBox({
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(df3, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan<-paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } else 
        {
          # Render the value box
          output$valueBox1 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_KAWASAN_NAMA == input$dynamicChoice, ]
            cat("Pilihan:", input$dynamicChoice)
            count_kompauns <- nrow(filtered_df)
            count_kompauns <- prettyNum(count_kompauns, big.mark = ",") # Format the count as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = count_kompauns,
              "Bilangan Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox2 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_KAWASAN_NAMA == input$dynamicChoice, ]
            
            sum_kompauns <- sum(filtered_df$KOMPAUN_AMAUN)
            sum_kompauns <- paste0("RM ", prettyNum(sum_kompauns, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the row count
            valueBox(
              value = sum_kompauns,
              "Amaun Kompaun Dikeluarkan (Tahun Semasa)",
              icon = icon("pen-to-square"), 
              color = "light-blue"
            )
          })
          
          # Render the value box
          output$valueBox3 <- renderValueBox({
            today <- Sys.Date()
            YEAR <- format(today, "%Y")
            
            # Filter the data based on input$dynamicChoice
            filtered_df <- df3[df3$TEMPAT_KAWASAN_NAMA == input$dynamicChoice, ]
            
            # Filter the data where BAYARAN is "BELUM SELESAI"
            df_rayuan <- subset(filtered_df, PINDAAN_TINDAKAN == "RAYUAN")
            
            # Calculate the sum of KOMPAUN_AMAUN where BAYARAN is "BELUM SELESAI"
            sum_rayuan <- sum(df_rayuan$KOMPAUN_AMAUN_LAMA - df_rayuan$BAYARAN_AMAUN, na.rm = TRUE)
            sum_rayuan <- paste0("RM ", prettyNum(sum_rayuan, big.mark = ",")) # Format the sum as a pretty number
            # Create the value box displaying the sum of RM
            valueBox(
              value = sum_rayuan,
              "Amaun Rayuan (Tahun Semasa)", 
              icon = icon("hand-holding-dollar"), 
              color = "light-blue")
          })
        } 
      }
      ##########################################################################################################################################################
      if(input$JENIS_DATA_KUTIPAN == "Tahunan")
      {
        if(input$MENGIKUT_KUTIPAN == "Kesalahan")
        {
          output$plot1 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, KESALAHAN, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_YEAR = floor_date(as.Date(KOMPAUN_TARIKH), unit = "year")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1_KUTIPAN)
                end_year <- lubridate::year(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              })
              
            } else {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1_KUTIPAN)
                end_year <- lubridate::year(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year) %>%
                  filter(KESALAHAN == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(SELESAI = n())
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(BELUM_SELESAI = n())
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_YEAR")
            
            # Define the new category names you want
            new_categories <- data_combined$KOMPAUN_YEAR
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut Kesalahan ", input$dynamicChoice_KUTIPAN))%>%
              hc_xAxis(categories = new_categories, title = list(text = "Tahun")) %>%
              hc_yAxis(title = list(text = "Jumlah")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{point.y:.0f}"  # Display the count as is
                  )
                )
              )
          })
          output$plot2 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, KESALAHAN, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_YEAR = floor_date(as.Date(KOMPAUN_TARIKH), unit = "year")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1_KUTIPAN)
                end_year <- lubridate::year(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              })
              
            } else {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1_KUTIPAN)
                end_year <- lubridate::year(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year) %>%
                  filter(KESALAHAN == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(BELUM_SELESAI = sum(KOMPAUN_AMAUN))
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_YEAR")
            
            # Define the new category names you want
            new_categories <- data_combined$KOMPAUN_YEAR
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut Kesalahan ", input$dynamicChoice_KUTIPAN)) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Tahun")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "RM {point.y:.0f}"  # Change this format as needed
                  )
                )
              )
          })
          output$plot3 <- renderHighchart({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice_KUTIPAN & !is.na(df1$KESALAHAN)])
            }
  
            a1 <- df1 %>%
              select(KESALAHAN, BAYARAN_TARIKH, BAYARAN_AMAUN, BAYARAN_SALURAN) %>%
              filter(!is.na(BAYARAN_TARIKH)) %>%
              mutate(KOMPAUN_YEAR = year(as.Date(BAYARAN_TARIKH))) 
            
            unique_saluran <- unique(a1$BAYARAN_SALURAN)
            
            # Create a complete dataset of all combinations of YEAR, KESALAHAN, and BAYARAN_SALURAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(a1$BAYARAN_TARIKH)), max(lubridate::year(a1$BAYARAN_TARIKH)), by = 1),
              KESALAHAN = unique_kesalahan,
              BAYARAN_SALURAN = unique_saluran
            )
            
            # Remove unnecessary columns
            a1 <- a1[, -which(names(a1) %in% c("BAYARAN_TARIKH"))]
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "KESALAHAN", 'BAYARAN_SALURAN')) %>%
              filter(!is.na(BAYARAN_AMAUN)) 
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1_KUTIPAN)
              end_year <- lubridate::year(input$date2_KUTIPAN)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN and BAYARAN_SALURAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KOMPAUN_YEAR, BAYARAN_SALURAN) %>%
                summarise(Count = n()) 
              
              return(grouped_data)  # Return the grouped data
            })
            
            # Get the grouped data from the reactive function
            grouped_data <- filteredDataList()
            
            # Create the Highcharter plot
            grouped_data %>%
              hchart(type = "line", hcaes(x = KOMPAUN_YEAR, y = Count, group = BAYARAN_SALURAN)) %>%
              hc_xAxis(type = "linear") %>%
              hc_yAxis(title = list(text = "BAYARAN_AMAUN")) %>%
              hc_legend(layout = "vertical") %>%
              hc_title(text = "Bilangan Bayaran mengikut Bayaran Saluran (Tahunan)")
          })
          output$plot4 <- renderPlotly({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_kesalahan <- unique(df1$KESALAHAN)
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date, ]
              })
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice_KUTIPAN & !is.na(df1$KESALAHAN)])
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                KESALAHAN <- unique_kesalahan
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date & df1$KESALAHAN == KESALAHAN, ]
              })
            }
            
           
            
            # Calculate sum of values
            gaugeValue <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN, na.rm = TRUE)
            })
            sum_kompaun <- reactive({
              sum(ifelse(is.na(filteredDataBayaran()$KOMPAUN_AMAUN_LAMA), filteredDataBayaran()$KOMPAUN_AMAUN, filteredDataBayaran()$KOMPAUN_AMAUN_LAMA))
            })
            
            # Reactive expression for calculating the sum of RM
            sum_bayaran <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN)
            })
            
            plot_ly(
              type = "indicator",
              value = gaugeValue(),
              mode = "gauge",
              gauge = list(
                axis = list(range = list(NULL, sum_kompaun())),
                bar = list(color = "#0077b6"),
                borderwidth = 2,
                bordercolor = "#000000",
                steps = list(
                  list(range = c(0, sum_kompaun() * 0.9), color = "lightgray"),
                  list(range = c(sum_kompaun() * 0.9, sum_kompaun()), color = "white")
                ),
                threshold = list(
                  line = list(color = "red", width = 3), #set threshold
                  thickness = 1,
                  value = sum_bayaran()
                )
              )
            ) %>%
              layout(
                title = list(font = list(size = 13, color = "black")),
                margin = list(t = 45), #defines the margins (spacing) around the plot area
                plot_bgcolor = "rgba(0,0,0,0)",
                paper_bgcolor = "rgba(0,0,0,0)",
                annotations = list(
                  
                  list(
                    text = paste("Jumlah Kutipan: RM ", format(sum_bayaran(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.5,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste("Jumlah Kompaun : RM", format(sum_kompaun(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.4,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste0(round((sum_bayaran()/sum_kompaun())*100, digits = 2), "%"),
                    showarrow = FALSE,
                    x = 0.5,
                    y = -0.1,
                    font = list(size = 35, color = "black")
                  )
                )
              )
          })    
        } else if(input$MENGIKUT_KUTIPAN == "Tempat - Zon")
        {
          output$plot1 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_ZON_NAMA, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_YEAR = floor_date(as.Date(KOMPAUN_TARIKH), unit = "year")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1_KUTIPAN)
                end_year <- lubridate::year(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              })
              
            } else {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1_KUTIPAN)
                end_year <- lubridate::year(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year) %>%
                  filter(TEMPAT_ZON_NAMA == input$dynamicChoice_KUTIPAN)
              })
              
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(SELESAI = n())
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(BELUM_SELESAI = n())
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_YEAR")
            
            # Define the new category names you want
            new_categories <- data_combined$KOMPAUN_YEAR
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut", input$dynamicChoice_KUTIPAN)) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Tahun")) %>%
              hc_yAxis(title = list(text = "Jumlah")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{point.y:.0f}"  # Display the count as is
                  )
                )
              )
          })
          output$plot2 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_ZON_NAMA, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_YEAR = floor_date(as.Date(KOMPAUN_TARIKH), unit = "year")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1_KUTIPAN)
                end_year <- lubridate::year(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              })
              
            } else {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1_KUTIPAN)
                end_year <- lubridate::year(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year) %>%
                  filter(TEMPAT_ZON_NAMA == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(BELUM_SELESAI = sum(KOMPAUN_AMAUN))
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_YEAR")
            
            # Define the new category names you want
            new_categories <- data_combined$KOMPAUN_YEAR
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut ", input$dynamicChoice_KUTIPAN))%>%
              hc_xAxis(categories = new_categories, title = list(text = "Tahun")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "RM {point.y:.0f}"  # Change this format as needed
                  )
                )
              )
          })
          output$plot3 <- renderHighchart({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA == input$dynamicChoice_KUTIPAN & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
          
            a1 <- df1 %>%
              select(TEMPAT_ZON_NAMA, BAYARAN_TARIKH, BAYARAN_AMAUN, BAYARAN_SALURAN) %>%
              filter(!is.na(BAYARAN_TARIKH)) %>%
              mutate(KOMPAUN_YEAR = year(as.Date(BAYARAN_TARIKH))) 
            
            unique_saluran <- unique(a1$BAYARAN_SALURAN)
            
            # Create a complete dataset of all combinations of YEAR, KESALAHAN, and BAYARAN_SALURAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(a1$BAYARAN_TARIKH)), max(lubridate::year(a1$BAYARAN_TARIKH)), by = 1),
              TEMPAT_ZON_NAMA = unique_zon,
              BAYARAN_SALURAN = unique_saluran
            )
            
            # Remove unnecessary columns
            a1 <- a1[, -which(names(a1) %in% c("BAYARAN_TARIKH"))]
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_ZON_NAMA", 'BAYARAN_SALURAN')) %>%
              filter(!is.na(BAYARAN_AMAUN)) 
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1_KUTIPAN)
              end_year <- lubridate::year(input$date2_KUTIPAN)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN and BAYARAN_SALURAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KOMPAUN_YEAR, BAYARAN_SALURAN) %>%
                summarise(Count = n()) 
              
              return(grouped_data)  # Return the grouped data
            })
            
            # Get the grouped data from the reactive function
            grouped_data <- filteredDataList()
            
            # Create the Highcharter plot
            grouped_data %>%
              hchart(type = "line", hcaes(x = KOMPAUN_YEAR, y = Count, group = BAYARAN_SALURAN)) %>%
              hc_xAxis(type = "linear") %>%
              hc_yAxis(title = list(text = "BAYARAN_AMAUN")) %>%
              hc_legend(layout = "vertical") %>%
              hc_title(text = "Bilangan Bayaran mengikut Bayaran Saluran (Tahunan)")
          })
          output$plot4 <- renderPlotly({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date, ]
              })
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA == input$dynamicChoice_KUTIPAN & !is.na(df1$TEMPAT_ZON_NAMA)])
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                TEMPAT_ZON_NAMA <- unique_zon
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date & df1$TEMPAT_ZON_NAMA == TEMPAT_ZON_NAMA, ]
              })
            }
            
            
            
            # Calculate sum of values
            gaugeValue <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN, na.rm = TRUE)
            })
            sum_kompaun <- reactive({
              sum(ifelse(is.na(filteredDataBayaran()$KOMPAUN_AMAUN_LAMA), filteredDataBayaran()$KOMPAUN_AMAUN, filteredDataBayaran()$KOMPAUN_AMAUN_LAMA))
            })
            
            # Reactive expression for calculating the sum of RM
            sum_bayaran <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN)
            })
            
            plot_ly(
              type = "indicator",
              value = gaugeValue(),
              mode = "gauge",
              gauge = list(
                axis = list(range = list(NULL, sum_kompaun())),
                bar = list(color = "#0077b6"),
                borderwidth = 2,
                bordercolor = "#000000",
                steps = list(
                  list(range = c(0, sum_kompaun() * 0.9), color = "lightgray"),
                  list(range = c(sum_kompaun() * 0.9, sum_kompaun()), color = "white")
                ),
                threshold = list(
                  line = list(color = "red", width = 3), #set threshold
                  thickness = 1,
                  value = sum_bayaran()
                )
              )
            ) %>%
              layout(
                title = list(font = list(size = 13, color = "black")),
                margin = list(t = 45), #defines the margins (spacing) around the plot area
                plot_bgcolor = "rgba(0,0,0,0)",
                paper_bgcolor = "rgba(0,0,0,0)",
                annotations = list(
                  
                  list(
                    text = paste("Jumlah Kutipan: RM ", format(sum_bayaran(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.5,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste("Jumlah Kompaun : RM", format(sum_kompaun(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.4,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste0(round((sum_bayaran()/sum_kompaun())*100, digits = 2), "%"),
                    showarrow = FALSE,
                    x = 0.5,
                    y = -0.1,
                    font = list(size = 35, color = "black")
                  )
                )
              )
          }) 
        } else if(input$MENGIKUT_KUTIPAN == "Tempat - Kawasan")
        {
          output$plot1 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_KAWASAN_NAMA, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_YEAR = floor_date(as.Date(KOMPAUN_TARIKH), unit = "year")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1_KUTIPAN)
                end_year <- lubridate::year(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              })
              
            } else {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1_KUTIPAN)
                end_year <- lubridate::year(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year) %>%
                  filter(TEMPAT_KAWASAN_NAMA == input$dynamicChoice_KUTIPAN)
              })
              cat("PILIHAN: ", input$dynamicChoice_KUTIPAN)
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(SELESAI = n())
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(BELUM_SELESAI = n())
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_YEAR")
            
            # Define the new category names you want
            new_categories <- data_combined$KOMPAUN_YEAR
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut Kawasan ", input$dynamicChoice_KUTIPAN)) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Tahun")) %>%
              hc_yAxis(title = list(text = "Jumlah")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{point.y:.0f}"  # Display the count as is
                  )
                )
              )
          })
          output$plot2 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_KAWASAN_NAMA, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_YEAR = floor_date(as.Date(KOMPAUN_TARIKH), unit = "year")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by year and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_YEAR = lubridate::year(KOMPAUN_TARIKH))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1_KUTIPAN)
                end_year <- lubridate::year(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              })
              
            } else {
              
              # Reactive function to filter data based on selected year range
              filteredDataList <- reactive({
                start_year <- lubridate::year(input$date1_KUTIPAN)
                end_year <- lubridate::year(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year) %>%
                  filter(TEMPAT_KAWASAN_NAMA == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_YEAR) %>%
              summarise(BELUM_SELESAI = sum(KOMPAUN_AMAUN))
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_YEAR")
            
            # Define the new category names you want
            new_categories <- data_combined$KOMPAUN_YEAR
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut Kawasan ", input$dynamicChoice_KUTIPAN)) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Tahun")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "RM {point.y:.0f}"  # Change this format as needed
                  )
                )
              )
          })
          output$plot3 <- renderHighchart({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA == input$dynamicChoice_KUTIPAN & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            a1 <- df1 %>%
              select(TEMPAT_KAWASAN_NAMA, BAYARAN_TARIKH, BAYARAN_AMAUN, BAYARAN_SALURAN) %>%
              filter(!is.na(BAYARAN_TARIKH)) %>%
              mutate(KOMPAUN_YEAR = year(as.Date(BAYARAN_TARIKH))) 
            
            unique_saluran <- unique(a1$BAYARAN_SALURAN)
            
            # Create a complete dataset of all combinations of YEAR, KESALAHAN, and BAYARAN_SALURAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(a1$BAYARAN_TARIKH)), max(lubridate::year(a1$BAYARAN_TARIKH)), by = 1),
              TEMPAT_KAWASAN_NAMA = unique_kawasan,
              BAYARAN_SALURAN = unique_saluran
            )
            
            # Remove unnecessary columns
            a1 <- a1[, -which(names(a1) %in% c("BAYARAN_TARIKH"))]
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_KAWASAN_NAMA", 'BAYARAN_SALURAN')) %>%
              filter(!is.na(BAYARAN_AMAUN)) 
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1_KUTIPAN)
              end_year <- lubridate::year(input$date2_KUTIPAN)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN and BAYARAN_SALURAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KOMPAUN_YEAR, BAYARAN_SALURAN) %>%
                summarise(Count = n()) 
              
              return(grouped_data)  # Return the grouped data
            })
            
            # Get the grouped data from the reactive function
            grouped_data <- filteredDataList()
            
            # Create the Highcharter plot
            grouped_data %>%
              hchart(type = "line", hcaes(x = KOMPAUN_YEAR, y = Count, group = BAYARAN_SALURAN)) %>%
              hc_xAxis(type = "linear") %>%
              hc_yAxis(title = list(text = "BAYARAN_AMAUN")) %>%
              hc_legend(layout = "vertical") %>%
              hc_title(text = "Bilangan Bayaran mengikut Bayaran Saluran (Tahunan)")
          })
          output$plot4 <- renderPlotly({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_kawasan <- unique(df1$TEMPAT_ZON_NAMA)
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date, ]
              })
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA == input$dynamicChoice_KUTIPAN & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                TEMPAT_KAWASAN_NAMA <- unique_kawasan
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date & df1$TEMPAT_KAWASAN_NAMA == TEMPAT_KAWASAN_NAMA, ]
              })
            }
            
            
            
            # Calculate sum of values
            gaugeValue <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN, na.rm = TRUE)
            })
            sum_kompaun <- reactive({
              sum(ifelse(is.na(filteredDataBayaran()$KOMPAUN_AMAUN_LAMA), filteredDataBayaran()$KOMPAUN_AMAUN, filteredDataBayaran()$KOMPAUN_AMAUN_LAMA))
            })
            
            # Reactive expression for calculating the sum of RM
            sum_bayaran <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN)
            })
            
            plot_ly(
              type = "indicator",
              value = gaugeValue(),
              mode = "gauge",
              gauge = list(
                axis = list(range = list(NULL, sum_kompaun())),
                bar = list(color = "#0077b6"),
                borderwidth = 2,
                bordercolor = "#000000",
                steps = list(
                  list(range = c(0, sum_kompaun() * 0.9), color = "lightgray"),
                  list(range = c(sum_kompaun() * 0.9, sum_kompaun()), color = "white")
                ),
                threshold = list(
                  line = list(color = "red", width = 3), #set threshold
                  thickness = 1,
                  value = sum_bayaran()
                )
              )
            ) %>%
              layout(
                title = list(font = list(size = 13, color = "black")),
                margin = list(t = 45), #defines the margins (spacing) around the plot area
                plot_bgcolor = "rgba(0,0,0,0)",
                paper_bgcolor = "rgba(0,0,0,0)",
                annotations = list(
                  
                  list(
                    text = paste("Jumlah Kutipan: RM ", format(sum_bayaran(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.5,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste("Jumlah Kompaun : RM", format(sum_kompaun(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.4,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste0(round((sum_bayaran()/sum_kompaun())*100, digits = 2), "%"),
                    showarrow = FALSE,
                    x = 0.5,
                    y = -0.1,
                    font = list(size = 35, color = "black")
                  )
                )
              )
          }) 
        } 
      } else if(input$JENIS_DATA_KUTIPAN == "Bulanan")
      {
        if(input$MENGIKUT_KUTIPAN == "Kesalahan")
        {
          output$plot1 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, KESALAHAN, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_MONTH = floor_date(as.Date(KOMPAUN_TARIKH), unit = "month")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by month and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_MONTH = lubridate::floor_date(KOMPAUN_TARIKH, unit = "month"))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected month range
              filteredDataList <- reactive({
                start_month <- lubridate::floor_date(input$date1_KUTIPAN, unit = "month")
                end_month <- lubridate::floor_date(input$date2_KUTIPAN, unit = "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTH >= start_month & KOMPAUN_MONTH <= end_month)
              })
              
            } else {
              
              # Reactive function to filter data based on selected month range
              filteredDataList <- reactive({
                start_month <- lubridate::floor_date(input$date1_KUTIPAN, unit = "month")
                end_month <- lubridate::floor_date(input$date2_KUTIPAN, unit = "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTH >= start_month & KOMPAUN_MONTH <= end_month) %>%
                  filter(KESALAHAN == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_MONTH) %>%
              summarise(SELESAI = n())
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_MONTH) %>%
              summarise(BELUM_SELESAI = n())
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_MONTH")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_MONTH, format = "%b-%y")
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut Kesalahan ", input$dynamicChoice_KUTIPAN, "(Bulanan)")) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Bulan")) %>%
              hc_yAxis(title = list(text = "Jumlah")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{point.y:.0f}"  # Display the count as is
                  )
                )
              )
          })
          output$plot2 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, KESALAHAN, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_MONTH = floor_date(as.Date(KOMPAUN_TARIKH), unit = "month")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by month and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_MONTH = lubridate::floor_date(KOMPAUN_TARIKH, unit = "month"))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected month range
              filteredDataList <- reactive({
                start_month <- lubridate::floor_date(input$date1_KUTIPAN, unit = "month")
                end_month <- lubridate::floor_date(input$date2_KUTIPAN, unit = "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTH >= start_month & KOMPAUN_MONTH <= end_month)
              })
              
            } else {
              
              # Reactive function to filter data based on selected month range
              filteredDataList <- reactive({
                start_month <- lubridate::floor_date(input$date1_KUTIPAN, unit = "month")
                end_month <- lubridate::floor_date(input$date2_KUTIPAN, unit = "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTH >= start_month & KOMPAUN_MONTH <= end_month) %>%
                  filter(KESALAHAN == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_MONTH) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_MONTH) %>%
              summarise(BELUM_SELESAI = sum(KOMPAUN_AMAUN))
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_MONTH")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_MONTH, format = "%b-%y")
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut Kesalahan ", input$dynamicChoice_KUTIPAN, "(Bulanan)")) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Bulan")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "RM {point.y:.0f}"  # Change this format as needed
                  )
                )
              )
          })
          output$plot3 <- renderHighchart({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice_KUTIPAN & !is.na(df1$KESALAHAN)])
            }
            
            cat(unique_kesalahan)
            a1 <- df1 %>%
              select(KESALAHAN, BAYARAN_TARIKH, BAYARAN_AMAUN, BAYARAN_SALURAN) %>%
              filter(!is.na(BAYARAN_TARIKH)) %>%
              mutate(KOMPAUN_YEAR = year(as.Date(BAYARAN_TARIKH))) 
            
            unique_saluran <- unique(a1$BAYARAN_SALURAN)
            
            # Create a complete dataset of all combinations of YEAR, KESALAHAN, and BAYARAN_SALURAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(a1$BAYARAN_TARIKH)), max(lubridate::year(a1$BAYARAN_TARIKH)), by = 1),
              KESALAHAN = unique_kesalahan,
              BAYARAN_SALURAN = unique_saluran
            )
            
            # Remove unnecessary columns
            a1 <- a1[, -which(names(a1) %in% c("BAYARAN_TARIKH"))]
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "KESALAHAN", 'BAYARAN_SALURAN')) %>%
              filter(!is.na(BAYARAN_AMAUN)) 
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1_KUTIPAN)
              end_year <- lubridate::year(input$date2_KUTIPAN)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN and BAYARAN_SALURAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KOMPAUN_YEAR, BAYARAN_SALURAN) %>%
                summarise(Count = n()) 
              
              return(grouped_data)  # Return the grouped data
            })
            
            # Get the grouped data from the reactive function
            grouped_data <- filteredDataList()
            
            # Create the Highcharter plot
            grouped_data %>%
              hchart(type = "line", hcaes(x = KOMPAUN_YEAR, y = Count, group = BAYARAN_SALURAN)) %>%
              hc_xAxis(type = "linear") %>%
              hc_yAxis(title = list(text = "BAYARAN_AMAUN")) %>%
              hc_legend(layout = "vertical") %>%
              hc_title(text = "Bilangan Bayaran mengikut Bayaran Saluran (Tahunan)")
          })
          output$plot4 <- renderPlotly({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_kesalahan <- unique(df1$KESALAHAN)
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date, ]
              })
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice_KUTIPAN & !is.na(df1$KESALAHAN)])
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                KESALAHAN <- unique_kesalahan
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date & df1$KESALAHAN == KESALAHAN, ]
              })
            }
            
            
            
            # Calculate sum of values
            gaugeValue <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN, na.rm = TRUE)
            })
            sum_kompaun <- reactive({
              sum(ifelse(is.na(filteredDataBayaran()$KOMPAUN_AMAUN_LAMA), filteredDataBayaran()$KOMPAUN_AMAUN, filteredDataBayaran()$KOMPAUN_AMAUN_LAMA))
            })
            
            # Reactive expression for calculating the sum of RM
            sum_bayaran <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN)
            })
            
            plot_ly(
              type = "indicator",
              value = gaugeValue(),
              mode = "gauge",
              gauge = list(
                axis = list(range = list(NULL, sum_kompaun())),
                bar = list(color = "#0077b6"),
                borderwidth = 2,
                bordercolor = "#000000",
                steps = list(
                  list(range = c(0, sum_kompaun() * 0.9), color = "lightgray"),
                  list(range = c(sum_kompaun() * 0.9, sum_kompaun()), color = "white")
                ),
                threshold = list(
                  line = list(color = "red", width = 3), #set threshold
                  thickness = 1,
                  value = sum_bayaran()
                )
              )
            ) %>%
              layout(
                title = list(font = list(size = 13, color = "black")),
                margin = list(t = 45), #defines the margins (spacing) around the plot area
                plot_bgcolor = "rgba(0,0,0,0)",
                paper_bgcolor = "rgba(0,0,0,0)",
                annotations = list(
                  
                  list(
                    text = paste("Jumlah Kutipan: RM ", format(sum_bayaran(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.5,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste("Jumlah Kompaun : RM", format(sum_kompaun(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.4,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste0(round((sum_bayaran()/sum_kompaun())*100, digits = 2), "%"),
                    showarrow = FALSE,
                    x = 0.5,
                    y = -0.1,
                    font = list(size = 35, color = "black")
                  )
                )
              )
          }) 
        } else if(input$MENGIKUT_KUTIPAN == "Tempat - Zon")
        {
          output$plot1 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_ZON_NAMA, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_MONTH = floor_date(as.Date(KOMPAUN_TARIKH), unit = "month")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by month and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_MONTH = lubridate::floor_date(KOMPAUN_TARIKH, unit = "month"))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected month range
              filteredDataList <- reactive({
                start_month <- lubridate::floor_date(input$date1_KUTIPAN, unit = "month")
                end_month <- lubridate::floor_date(input$date2_KUTIPAN, unit = "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTH >= start_month & KOMPAUN_MONTH <= end_month)
              })
              
            } else {
              
              # Reactive function to filter data based on selected month range
              filteredDataList <- reactive({
                start_month <- lubridate::floor_date(input$date1_KUTIPAN, unit = "month")
                end_month <- lubridate::floor_date(input$date2_KUTIPAN, unit = "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTH >= start_month & KOMPAUN_MONTH <= end_month) %>%
                  filter(TEMPAT_ZON_NAMA == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_MONTH) %>%
              summarise(SELESAI = n())
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_MONTH) %>%
              summarise(BELUM_SELESAI = n())
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_MONTH")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_MONTH, format = "%b-%y")
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut ", input$dynamicChoice_KUTIPAN, "(Bulanan)")) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Bulan")) %>%
              hc_yAxis(title = list(text = "Jumlah")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{point.y:.0f}"  # Display the count as is
                  )
                )
              )
          })
          output$plot2 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_ZON_NAMA, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_MONTH = floor_date(as.Date(KOMPAUN_TARIKH), unit = "month")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by month and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_MONTH = lubridate::floor_date(KOMPAUN_TARIKH, unit = "month"))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected month range
              filteredDataList <- reactive({
                start_month <- lubridate::floor_date(input$date1_KUTIPAN, unit = "month")
                end_month <- lubridate::floor_date(input$date2_KUTIPAN, unit = "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTH >= start_month & KOMPAUN_MONTH <= end_month)
              })
              
            } else {
              
              # Reactive function to filter data based on selected month range
              filteredDataList <- reactive({
                start_month <- lubridate::floor_date(input$date1_KUTIPAN, unit = "month")
                end_month <- lubridate::floor_date(input$date2_KUTIPAN, unit = "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTH >= start_month & KOMPAUN_MONTH <= end_month) %>%
                  filter(TEMPAT_ZON_NAMA == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_MONTH) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_MONTH) %>%
              summarise(BELUM_SELESAI = sum(KOMPAUN_AMAUN))
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_MONTH")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_MONTH, format = "%b-%y")
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut ", input$dynamicChoice_KUTIPAN, "(Bulanan)")) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Bulan")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "RM {point.y:.0f}"  # Change this format as needed
                  )
                )
              )
          })
          output$plot3 <- renderHighchart({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA == input$dynamicChoice_KUTIPAN & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            a1 <- df1 %>%
              select(TEMPAT_ZON_NAMA, BAYARAN_TARIKH, BAYARAN_AMAUN, BAYARAN_SALURAN) %>%
              filter(!is.na(BAYARAN_TARIKH)) %>%
              mutate(KOMPAUN_YEAR = year(as.Date(BAYARAN_TARIKH))) 
            
            unique_saluran <- unique(a1$BAYARAN_SALURAN)
            
            # Create a complete dataset of all combinations of YEAR, KESALAHAN, and BAYARAN_SALURAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(a1$BAYARAN_TARIKH)), max(lubridate::year(a1$BAYARAN_TARIKH)), by = 1),
              TEMPAT_ZON_NAMA = unique_zon,
              BAYARAN_SALURAN = unique_saluran
            )
            
            # Remove unnecessary columns
            a1 <- a1[, -which(names(a1) %in% c("BAYARAN_TARIKH"))]
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_ZON_NAMA", 'BAYARAN_SALURAN')) %>%
              filter(!is.na(BAYARAN_AMAUN)) 
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1_KUTIPAN)
              end_year <- lubridate::year(input$date2_KUTIPAN)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN and BAYARAN_SALURAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KOMPAUN_YEAR, BAYARAN_SALURAN) %>%
                summarise(Count = n()) 
              
              return(grouped_data)  # Return the grouped data
            })
            
            # Get the grouped data from the reactive function
            grouped_data <- filteredDataList()
            
            # Create the Highcharter plot
            grouped_data %>%
              hchart(type = "line", hcaes(x = KOMPAUN_YEAR, y = Count, group = BAYARAN_SALURAN)) %>%
              hc_xAxis(type = "linear") %>%
              hc_yAxis(title = list(text = "BAYARAN_AMAUN")) %>%
              hc_legend(layout = "vertical") %>%
              hc_title(text = "Bilangan Bayaran mengikut Bayaran Saluran (Tahunan)")
          })
          output$plot4 <- renderPlotly({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date, ]
              })
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA == input$dynamicChoice_KUTIPAN & !is.na(df1$TEMPAT_ZON_NAMA)])
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                TEMPAT_ZON_NAMA <- unique_zon
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date & df1$TEMPAT_ZON_NAMA == TEMPAT_ZON_NAMA, ]
              })
            }
            
            
            
            # Calculate sum of values
            gaugeValue <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN, na.rm = TRUE)
            })
            sum_kompaun <- reactive({
              sum(ifelse(is.na(filteredDataBayaran()$KOMPAUN_AMAUN_LAMA), filteredDataBayaran()$KOMPAUN_AMAUN, filteredDataBayaran()$KOMPAUN_AMAUN_LAMA))
            })
            
            # Reactive expression for calculating the sum of RM
            sum_bayaran <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN)
            })
            
            plot_ly(
              type = "indicator",
              value = gaugeValue(),
              mode = "gauge",
              gauge = list(
                axis = list(range = list(NULL, sum_kompaun())),
                bar = list(color = "#0077b6"),
                borderwidth = 2,
                bordercolor = "#000000",
                steps = list(
                  list(range = c(0, sum_kompaun() * 0.9), color = "lightgray"),
                  list(range = c(sum_kompaun() * 0.9, sum_kompaun()), color = "white")
                ),
                threshold = list(
                  line = list(color = "red", width = 3), #set threshold
                  thickness = 1,
                  value = sum_bayaran()
                )
              )
            ) %>%
              layout(
                title = list(font = list(size = 13, color = "black")),
                margin = list(t = 45), #defines the margins (spacing) around the plot area
                plot_bgcolor = "rgba(0,0,0,0)",
                paper_bgcolor = "rgba(0,0,0,0)",
                annotations = list(
                  
                  list(
                    text = paste("Jumlah Kutipan: RM ", format(sum_bayaran(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.5,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste("Jumlah Kompaun : RM", format(sum_kompaun(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.4,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste0(round((sum_bayaran()/sum_kompaun())*100, digits = 2), "%"),
                    showarrow = FALSE,
                    x = 0.5,
                    y = -0.1,
                    font = list(size = 35, color = "black")
                  )
                )
              )
          }) 
        } else if(input$MENGIKUT_KUTIPAN == "Tempat - Kawasan")
        {
          output$plot1 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_KAWASAN_NAMA, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_MONTH = floor_date(as.Date(KOMPAUN_TARIKH), unit = "month")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by month and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_MONTH = lubridate::floor_date(KOMPAUN_TARIKH, unit = "month"))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected month range
              filteredDataList <- reactive({
                start_month <- lubridate::floor_date(input$date1_KUTIPAN, unit = "month")
                end_month <- lubridate::floor_date(input$date2_KUTIPAN, unit = "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTH >= start_month & KOMPAUN_MONTH <= end_month)
              })
              
            } else {
              
              # Reactive function to filter data based on selected month range
              filteredDataList <- reactive({
                start_month <- lubridate::floor_date(input$date1_KUTIPAN, unit = "month")
                end_month <- lubridate::floor_date(input$date2_KUTIPAN, unit = "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTH >= start_month & KOMPAUN_MONTH <= end_month) %>%
                  filter(TEMPAT_KAWASAN_NAMA == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_MONTH) %>%
              summarise(SELESAI = n())
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_MONTH) %>%
              summarise(BELUM_SELESAI = n())
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_MONTH")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_MONTH, format = "%b-%y")
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut Kawasan ", input$dynamicChoice_KUTIPAN, "(Bulanan)")) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Bulan")) %>%
              hc_yAxis(title = list(text = "Jumlah")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{point.y:.0f}"  # Display the count as is
                  )
                )
              )
          })
          output$plot2 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_KAWASAN_NAMA, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_MONTH = floor_date(as.Date(KOMPAUN_TARIKH), unit = "month")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by month and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_MONTH = lubridate::floor_date(KOMPAUN_TARIKH, unit = "month"))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected month range
              filteredDataList <- reactive({
                start_month <- lubridate::floor_date(input$date1_KUTIPAN, unit = "month")
                end_month <- lubridate::floor_date(input$date2_KUTIPAN, unit = "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTH >= start_month & KOMPAUN_MONTH <= end_month)
              })
              
            } else {
              
              # Reactive function to filter data based on selected month range
              filteredDataList <- reactive({
                start_month <- lubridate::floor_date(input$date1_KUTIPAN, unit = "month")
                end_month <- lubridate::floor_date(input$date2_KUTIPAN, unit = "month")
                filtered_data <- a2 %>%
                  filter(KOMPAUN_MONTH >= start_month & KOMPAUN_MONTH <= end_month) %>%
                  filter(TEMPAT_KAWASAN_NAMA == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_MONTH) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_MONTH) %>%
              summarise(BELUM_SELESAI = sum(KOMPAUN_AMAUN))
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_MONTH")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_MONTH, format = "%b-%y")
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut Kawasan ", input$dynamicChoice_KUTIPAN, "(Bulanan)")) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Bulan")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "RM {point.y:.0f}"  # Change this format as needed
                  )
                )
              )
          })
          output$plot3 <- renderHighchart({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA == input$dynamicChoice_KUTIPAN & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            a1 <- df1 %>%
              select(TEMPAT_KAWASAN_NAMA, BAYARAN_TARIKH, BAYARAN_AMAUN, BAYARAN_SALURAN) %>%
              filter(!is.na(BAYARAN_TARIKH)) %>%
              mutate(KOMPAUN_YEAR = year(as.Date(BAYARAN_TARIKH))) 
            
            unique_saluran <- unique(a1$BAYARAN_SALURAN)
            
            # Create a complete dataset of all combinations of YEAR, KESALAHAN, and BAYARAN_SALURAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(a1$BAYARAN_TARIKH)), max(lubridate::year(a1$BAYARAN_TARIKH)), by = 1),
              TEMPAT_KAWASAN_NAMA = unique_kawasan,
              BAYARAN_SALURAN = unique_saluran
            )
            
            # Remove unnecessary columns
            a1 <- a1[, -which(names(a1) %in% c("BAYARAN_TARIKH"))]
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_KAWASAN_NAMA", 'BAYARAN_SALURAN')) %>%
              filter(!is.na(BAYARAN_AMAUN)) 
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1_KUTIPAN)
              end_year <- lubridate::year(input$date2_KUTIPAN)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN and BAYARAN_SALURAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KOMPAUN_YEAR, BAYARAN_SALURAN) %>%
                summarise(Count = n()) 
              
              return(grouped_data)  # Return the grouped data
            })
            
            # Get the grouped data from the reactive function
            grouped_data <- filteredDataList()
            
            # Create the Highcharter plot
            grouped_data %>%
              hchart(type = "line", hcaes(x = KOMPAUN_YEAR, y = Count, group = BAYARAN_SALURAN)) %>%
              hc_xAxis(type = "linear") %>%
              hc_yAxis(title = list(text = "BAYARAN_AMAUN")) %>%
              hc_legend(layout = "vertical") %>%
              hc_title(text = "Bilangan Bayaran mengikut Bayaran Saluran (Tahunan)")
          })
          output$plot4 <- renderPlotly({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_kawasan <- unique(df1$TEMPAT_ZON_NAMA)
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date, ]
              })
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA == input$dynamicChoice_KUTIPAN & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                TEMPAT_KAWASAN_NAMA <- unique_kawasan
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date & df1$TEMPAT_KAWASAN_NAMA == TEMPAT_KAWASAN_NAMA, ]
              })
            }
            
            
            
            # Calculate sum of values
            gaugeValue <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN, na.rm = TRUE)
            })
            sum_kompaun <- reactive({
              sum(ifelse(is.na(filteredDataBayaran()$KOMPAUN_AMAUN_LAMA), filteredDataBayaran()$KOMPAUN_AMAUN, filteredDataBayaran()$KOMPAUN_AMAUN_LAMA))
            })
            
            # Reactive expression for calculating the sum of RM
            sum_bayaran <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN)
            })
            
            plot_ly(
              type = "indicator",
              value = gaugeValue(),
              mode = "gauge",
              gauge = list(
                axis = list(range = list(NULL, sum_kompaun())),
                bar = list(color = "#0077b6"),
                borderwidth = 2,
                bordercolor = "#000000",
                steps = list(
                  list(range = c(0, sum_kompaun() * 0.9), color = "lightgray"),
                  list(range = c(sum_kompaun() * 0.9, sum_kompaun()), color = "white")
                ),
                threshold = list(
                  line = list(color = "red", width = 3), #set threshold
                  thickness = 1,
                  value = sum_bayaran()
                )
              )
            ) %>%
              layout(
                title = list(font = list(size = 13, color = "black")),
                margin = list(t = 45), #defines the margins (spacing) around the plot area
                plot_bgcolor = "rgba(0,0,0,0)",
                paper_bgcolor = "rgba(0,0,0,0)",
                annotations = list(
                  
                  list(
                    text = paste("Jumlah Kutipan: RM ", format(sum_bayaran(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.5,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste("Jumlah Kompaun : RM", format(sum_kompaun(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.4,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste0(round((sum_bayaran()/sum_kompaun())*100, digits = 2), "%"),
                    showarrow = FALSE,
                    x = 0.5,
                    y = -0.1,
                    font = list(size = 35, color = "black")
                  )
                )
              )
          }) 
        } 
      } else if(input$JENIS_DATA_KUTIPAN == "Harian")
      {
        if(input$MENGIKUT_KUTIPAN == "Kesalahan")
        {
          output$plot1 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, KESALAHAN, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_DAY = floor_date(as.Date(KOMPAUN_TARIKH), unit = "day")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by day and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_DAY = lubridate::floor_date(KOMPAUN_TARIKH, unit = "day"))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected date range
              filteredDataList <- reactive({
                start_date <- as.Date(input$date1_KUTIPAN)
                end_date <- as.Date(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_DAY >= start_date & KOMPAUN_DAY <= end_date)
              })
              
            } else {
              
              # Reactive function to filter data based on selected date range
              filteredDataList <- reactive({
                start_date <- as.Date(input$date1_KUTIPAN)
                end_date <- as.Date(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_DAY >= start_date & KOMPAUN_DAY <= end_date) %>%
                  filter(KESALAHAN == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_DAY) %>%
              summarise(SELESAI = n())
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_DAY) %>%
              summarise(BELUM_SELESAI = n())
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_DAY")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_DAY, format = "%d-%b-%y")
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut Kesalahan ", input$dynamicChoice_KUTIPAN, "(Harian)")) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Hari")) %>%
              hc_yAxis(title = list(text = "Jumlah")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{point.y:.0f}"  # Display the count as is
                  )
                )
              )
          })    
          output$plot2 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, KESALAHAN, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_DAY = floor_date(as.Date(KOMPAUN_TARIKH), unit = "day")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by day and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_DAY = lubridate::floor_date(KOMPAUN_TARIKH, unit = "day"))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected date range
              filteredDataList <- reactive({
                start_date <- as.Date(input$date1_KUTIPAN)
                end_date <- as.Date(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_DAY >= start_date & KOMPAUN_DAY <= end_date)
              })
              
            } else {
              
              # Reactive function to filter data based on selected date range
              filteredDataList <- reactive({
                start_date <- as.Date(input$date1_KUTIPAN)
                end_date <- as.Date(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_DAY >= start_date & KOMPAUN_DAY <= end_date) %>%
                  filter(KESALAHAN == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_DAY) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_DAY) %>%
              summarise(BELUM_SELESAI = sum(KOMPAUN_AMAUN))
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_DAY")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_DAY, format = "%d-%b-%y")
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut Kesalahan ", input$dynamicChoice_KUTIPAN, "(Harian)")) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Hari")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "RM {point.y:.0f}"  # Change this format as needed
                  )
                )
              )
          })
          output$plot3 <- renderHighchart({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_kesalahan <- unique(df1$KESALAHAN)
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice_KUTIPAN & !is.na(df1$KESALAHAN)])
            }
            
            cat(unique_kesalahan)
            a1 <- df1 %>%
              select(KESALAHAN, BAYARAN_TARIKH, BAYARAN_AMAUN, BAYARAN_SALURAN) %>%
              filter(!is.na(BAYARAN_TARIKH)) %>%
              mutate(KOMPAUN_YEAR = year(as.Date(BAYARAN_TARIKH))) 
            
            unique_saluran <- unique(a1$BAYARAN_SALURAN)
            
            # Create a complete dataset of all combinations of YEAR, KESALAHAN, and BAYARAN_SALURAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(a1$BAYARAN_TARIKH)), max(lubridate::year(a1$BAYARAN_TARIKH)), by = 1),
              KESALAHAN = unique_kesalahan,
              BAYARAN_SALURAN = unique_saluran
            )
            
            # Remove unnecessary columns
            a1 <- a1[, -which(names(a1) %in% c("BAYARAN_TARIKH"))]
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "KESALAHAN", 'BAYARAN_SALURAN')) %>%
              filter(!is.na(BAYARAN_AMAUN)) 
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1_KUTIPAN)
              end_year <- lubridate::year(input$date2_KUTIPAN)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN and BAYARAN_SALURAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KOMPAUN_YEAR, BAYARAN_SALURAN) %>%
                summarise(Count = n()) 
              
              return(grouped_data)  # Return the grouped data
            })
            
            # Get the grouped data from the reactive function
            grouped_data <- filteredDataList()
            
            # Create the Highcharter plot
            grouped_data %>%
              hchart(type = "line", hcaes(x = KOMPAUN_YEAR, y = Count, group = BAYARAN_SALURAN)) %>%
              hc_xAxis(type = "linear") %>%
              hc_yAxis(title = list(text = "BAYARAN_AMAUN")) %>%
              hc_legend(layout = "vertical") %>%
              hc_title(text = "Bilangan Bayaran mengikut Bayaran Saluran (Tahunan)")
          })
          output$plot4 <- renderPlotly({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_kesalahan <- unique(df1$KESALAHAN)
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date, ]
              })
            } else {
              unique_kesalahan <- unique(df1$KESALAHAN[df1$KESALAHAN == input$dynamicChoice_KUTIPAN & !is.na(df1$KESALAHAN)])
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                KESALAHAN <- unique_kesalahan
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date & df1$KESALAHAN == KESALAHAN, ]
              })
            }
            
            
            
            # Calculate sum of values
            gaugeValue <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN, na.rm = TRUE)
            })
            sum_kompaun <- reactive({
              sum(ifelse(is.na(filteredDataBayaran()$KOMPAUN_AMAUN_LAMA), filteredDataBayaran()$KOMPAUN_AMAUN, filteredDataBayaran()$KOMPAUN_AMAUN_LAMA))
            })
            
            # Reactive expression for calculating the sum of RM
            sum_bayaran <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN)
            })
            
            plot_ly(
              type = "indicator",
              value = gaugeValue(),
              mode = "gauge",
              gauge = list(
                axis = list(range = list(NULL, sum_kompaun())),
                bar = list(color = "#0077b6"),
                borderwidth = 2,
                bordercolor = "#000000",
                steps = list(
                  list(range = c(0, sum_kompaun() * 0.9), color = "lightgray"),
                  list(range = c(sum_kompaun() * 0.9, sum_kompaun()), color = "white")
                ),
                threshold = list(
                  line = list(color = "red", width = 3), #set threshold
                  thickness = 1,
                  value = sum_bayaran()
                )
              )
            ) %>%
              layout(
                title = list(font = list(size = 13, color = "black")),
                margin = list(t = 45), #defines the margins (spacing) around the plot area
                plot_bgcolor = "rgba(0,0,0,0)",
                paper_bgcolor = "rgba(0,0,0,0)",
                annotations = list(
                  
                  list(
                    text = paste("Jumlah Kutipan: RM ", format(sum_bayaran(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.5,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste("Jumlah Kompaun : RM", format(sum_kompaun(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.4,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste0(round((sum_bayaran()/sum_kompaun())*100, digits = 2), "%"),
                    showarrow = FALSE,
                    x = 0.5,
                    y = -0.1,
                    font = list(size = 35, color = "black")
                  )
                )
              )
          }) 
          
        } else if(input$MENGIKUT_KUTIPAN == "Tempat - Zon")
        {
          output$plot1 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_ZON_NAMA, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_DAY = floor_date(as.Date(KOMPAUN_TARIKH), unit = "day")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by day and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_DAY = lubridate::floor_date(KOMPAUN_TARIKH, unit = "day"))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected date range
              filteredDataList <- reactive({
                start_date <- as.Date(input$date1_KUTIPAN)
                end_date <- as.Date(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_DAY >= start_date & KOMPAUN_DAY <= end_date)
              })
              
            } else {
              
              # Reactive function to filter data based on selected date range
              filteredDataList <- reactive({
                start_date <- as.Date(input$date1_KUTIPAN)
                end_date <- as.Date(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_DAY >= start_date & KOMPAUN_DAY <= end_date) %>%
                  filter(TEMPAT_ZON_NAMA == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_DAY) %>%
              summarise(SELESAI = n())
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_DAY) %>%
              summarise(BELUM_SELESAI = n())
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_DAY")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_DAY, format = "%d-%b-%y")
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut ", input$dynamicChoice_KUTIPAN, "(Harian)")) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Hari")) %>%
              hc_yAxis(title = list(text = "Jumlah")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{point.y:.0f}"  # Display the count as is
                  )
                )
              )
          })         
          output$plot2 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_ZON_NAMA, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_DAY = floor_date(as.Date(KOMPAUN_TARIKH), unit = "day")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by day and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_DAY = lubridate::floor_date(KOMPAUN_TARIKH, unit = "day"))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected date range
              filteredDataList <- reactive({
                start_date <- as.Date(input$date1_KUTIPAN)
                end_date <- as.Date(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_DAY >= start_date & KOMPAUN_DAY <= end_date)
              })
              
            } else {
              
              # Reactive function to filter data based on selected date range
              filteredDataList <- reactive({
                start_date <- as.Date(input$date1_KUTIPAN)
                end_date <- as.Date(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_DAY >= start_date & KOMPAUN_DAY <= end_date) %>%
                  filter(TEMPAT_ZON_NAMA == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_DAY) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_DAY) %>%
              summarise(BELUM_SELESAI = sum(KOMPAUN_AMAUN))
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_DAY")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_DAY, format = "%d-%b-%y")
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut ", input$dynamicChoice_KUTIPAN, "(Harian)")) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Hari")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "RM {point.y:.0f}"  # Change this format as needed
                  )
                )
              )
          })
          output$plot3 <- renderHighchart({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA == input$dynamicChoice_KUTIPAN & !is.na(df1$TEMPAT_ZON_NAMA)])
            }
            
            a1 <- df1 %>%
              select(TEMPAT_ZON_NAMA, BAYARAN_TARIKH, BAYARAN_AMAUN, BAYARAN_SALURAN) %>%
              filter(!is.na(BAYARAN_TARIKH)) %>%
              mutate(KOMPAUN_YEAR = year(as.Date(BAYARAN_TARIKH))) 
            
            unique_saluran <- unique(a1$BAYARAN_SALURAN)
            
            # Create a complete dataset of all combinations of YEAR, KESALAHAN, and BAYARAN_SALURAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(a1$BAYARAN_TARIKH)), max(lubridate::year(a1$BAYARAN_TARIKH)), by = 1),
              TEMPAT_ZON_NAMA = unique_zon,
              BAYARAN_SALURAN = unique_saluran
            )
            
            # Remove unnecessary columns
            a1 <- a1[, -which(names(a1) %in% c("BAYARAN_TARIKH"))]
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_ZON_NAMA", 'BAYARAN_SALURAN')) %>%
              filter(!is.na(BAYARAN_AMAUN)) 
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1_KUTIPAN)
              end_year <- lubridate::year(input$date2_KUTIPAN)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN and BAYARAN_SALURAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KOMPAUN_YEAR, BAYARAN_SALURAN) %>%
                summarise(Count = n()) 
              
              return(grouped_data)  # Return the grouped data
            })
            
            # Get the grouped data from the reactive function
            grouped_data <- filteredDataList()
            
            # Create the Highcharter plot
            grouped_data %>%
              hchart(type = "line", hcaes(x = KOMPAUN_YEAR, y = Count, group = BAYARAN_SALURAN)) %>%
              hc_xAxis(type = "linear") %>%
              hc_yAxis(title = list(text = "BAYARAN_AMAUN")) %>%
              hc_legend(layout = "vertical") %>%
              hc_title(text = "Bilangan Bayaran mengikut Bayaran Saluran (Tahunan)")
          })
          output$plot4 <- renderPlotly({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA)
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date, ]
              })
            } else {
              unique_zon <- unique(df1$TEMPAT_ZON_NAMA[df1$TEMPAT_ZON_NAMA == input$dynamicChoice_KUTIPAN & !is.na(df1$TEMPAT_ZON_NAMA)])
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                TEMPAT_ZON_NAMA <- unique_zon
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date & df1$TEMPAT_ZON_NAMA == TEMPAT_ZON_NAMA, ]
              })
            }
            
            
            
            # Calculate sum of values
            gaugeValue <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN, na.rm = TRUE)
            })
            sum_kompaun <- reactive({
              sum(ifelse(is.na(filteredDataBayaran()$KOMPAUN_AMAUN_LAMA), filteredDataBayaran()$KOMPAUN_AMAUN, filteredDataBayaran()$KOMPAUN_AMAUN_LAMA))
            })
            
            # Reactive expression for calculating the sum of RM
            sum_bayaran <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN)
            })
            
            plot_ly(
              type = "indicator",
              value = gaugeValue(),
              mode = "gauge",
              gauge = list(
                axis = list(range = list(NULL, sum_kompaun())),
                bar = list(color = "#0077b6"),
                borderwidth = 2,
                bordercolor = "#000000",
                steps = list(
                  list(range = c(0, sum_kompaun() * 0.9), color = "lightgray"),
                  list(range = c(sum_kompaun() * 0.9, sum_kompaun()), color = "white")
                ),
                threshold = list(
                  line = list(color = "red", width = 3), #set threshold
                  thickness = 1,
                  value = sum_bayaran()
                )
              )
            ) %>%
              layout(
                title = list(font = list(size = 13, color = "black")),
                margin = list(t = 45), #defines the margins (spacing) around the plot area
                plot_bgcolor = "rgba(0,0,0,0)",
                paper_bgcolor = "rgba(0,0,0,0)",
                annotations = list(
                  
                  list(
                    text = paste("Jumlah Kutipan: RM ", format(sum_bayaran(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.5,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste("Jumlah Kompaun : RM", format(sum_kompaun(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.4,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste0(round((sum_bayaran()/sum_kompaun())*100, digits = 2), "%"),
                    showarrow = FALSE,
                    x = 0.5,
                    y = -0.1,
                    font = list(size = 35, color = "black")
                  )
                )
              )
          }) 
          
        } else if(input$MENGIKUT_KUTIPAN == "Tempat - Kawasan")
        {
          output$plot1 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_KAWASAN_NAMA, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_DAY = floor_date(as.Date(KOMPAUN_TARIKH), unit = "day")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by day and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_DAY = lubridate::floor_date(KOMPAUN_TARIKH, unit = "day"))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected date range
              filteredDataList <- reactive({
                start_date <- as.Date(input$date1_KUTIPAN)
                end_date <- as.Date(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_DAY >= start_date & KOMPAUN_DAY <= end_date)
              })
              
            } else {
              
              # Reactive function to filter data based on selected date range
              filteredDataList <- reactive({
                start_date <- as.Date(input$date1_KUTIPAN)
                end_date <- as.Date(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_DAY >= start_date & KOMPAUN_DAY <= end_date) %>%
                  filter(TEMPAT_KAWASAN_NAMA == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_DAY) %>%
              summarise(SELESAI = n())
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_DAY) %>%
              summarise(BELUM_SELESAI = n())
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_DAY")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_DAY, format = "%d-%b-%y")
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut Kawasan", input$dynamicChoice_KUTIPAN, "(Harian)")) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Hari")) %>%
              hc_yAxis(title = list(text = "Jumlah")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "{point.y:.0f}"  # Display the count as is
                  )
                )
              )
          })         
          output$plot2 <- renderHighchart({
            
            a1 <- df1 %>%
              select(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, TOTAL_BAYARAN, TEMPAT_KAWASAN_NAMA, KOMPAUN_AMAUN) %>%
              mutate(KOMPAUN_DAY = floor_date(as.Date(KOMPAUN_TARIKH), unit = "day")) %>%
              filter(PINDAAN_TINDAKAN != "PEMBATALAN")
            
            # Count occurrence by day and KESALAHAN
            a2 <- a1 %>%
              group_by(KOMPAUN_DAY = lubridate::floor_date(KOMPAUN_TARIKH, unit = "day"))
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              
              # Reactive function to filter data based on selected date range
              filteredDataList <- reactive({
                start_date <- as.Date(input$date1_KUTIPAN)
                end_date <- as.Date(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_DAY >= start_date & KOMPAUN_DAY <= end_date)
              })
              
            } else {
              
              # Reactive function to filter data based on selected date range
              filteredDataList <- reactive({
                start_date <- as.Date(input$date1_KUTIPAN)
                end_date <- as.Date(input$date2_KUTIPAN)
                filtered_data <- a2 %>%
                  filter(KOMPAUN_DAY >= start_date & KOMPAUN_DAY <= end_date) %>%
                  filter(TEMPAT_KAWASAN_NAMA == input$dynamicChoice_KUTIPAN)
              })
            }
            
            data_selesai <- filteredDataList() %>%
              filter(BAYARAN == "SELESAI") %>%
              group_by(KOMPAUN_DAY) %>%
              summarise(SELESAI = sum(TOTAL_BAYARAN))
            
            data_belum <- filteredDataList() %>%
              group_by(KOMPAUN_DAY) %>%
              summarise(BELUM_SELESAI = sum(KOMPAUN_AMAUN))
            
            data_combined <- left_join(data_belum, data_selesai, by = "KOMPAUN_DAY")
            
            # Define the new category names you want
            new_categories <- format(data_combined$KOMPAUN_DAY, format = "%d-%b-%y")
            
            highchart() %>%
              hc_title(text = paste("Kompaun Dikeluarkan dan Jumlah Kutipan mengikut Kawasan ", input$dynamicChoice_KUTIPAN, "(Harian)")) %>%
              hc_xAxis(categories = new_categories, title = list(text = "Hari")) %>%
              hc_yAxis(title = list(text = "Amaun (RM)")) %>%
              hc_add_series(type = "column", name = "KOMPAUN DIKELUARKAN", data = data_combined$BELUM_SELESAI) %>%
              hc_add_series(type = "spline", name = "KUTIPAN", data = data_combined$SELESAI) %>%
              hc_legend(align = "center", verticalAlign = "bottom", y = -30) %>%
              hc_plotOptions(
                series = list(
                  stacking = "normal",
                  dataLabels = list(
                    enabled = TRUE,
                    format = "RM {point.y:.0f}"  # Change this format as needed
                  )
                )
              )
          })
          output$plot3 <- renderHighchart({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA)
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA == input$dynamicChoice_KUTIPAN & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
            }
            
            a1 <- df1 %>%
              select(TEMPAT_KAWASAN_NAMA, BAYARAN_TARIKH, BAYARAN_AMAUN, BAYARAN_SALURAN) %>%
              filter(!is.na(BAYARAN_TARIKH)) %>%
              mutate(KOMPAUN_YEAR = year(as.Date(BAYARAN_TARIKH))) 
            
            unique_saluran <- unique(a1$BAYARAN_SALURAN)
            
            # Create a complete dataset of all combinations of YEAR, KESALAHAN, and BAYARAN_SALURAN
            Kompaun_data1 <- expand.grid(
              KOMPAUN_YEAR = seq(min(lubridate::year(a1$BAYARAN_TARIKH)), max(lubridate::year(a1$BAYARAN_TARIKH)), by = 1),
              TEMPAT_KAWASAN_NAMA = unique_kawasan,
              BAYARAN_SALURAN = unique_saluran
            )
            
            # Remove unnecessary columns
            a1 <- a1[, -which(names(a1) %in% c("BAYARAN_TARIKH"))]
            
            # Join the complete dataset with the summarized data and fill missing values with 0
            a2 <- Kompaun_data1 %>%
              left_join(a1, by = c("KOMPAUN_YEAR", "TEMPAT_KAWASAN_NAMA", 'BAYARAN_SALURAN')) %>%
              filter(!is.na(BAYARAN_AMAUN)) 
            
            # Reactive function to filter data based on selected year range
            filteredDataList <- reactive({
              start_year <- lubridate::year(input$date1_KUTIPAN)
              end_year <- lubridate::year(input$date2_KUTIPAN)
              filtered_data <- a2 %>%
                filter(KOMPAUN_YEAR >= start_year & KOMPAUN_YEAR <= end_year)
              
              # Group data by KESALAHAN and BAYARAN_SALURAN for separate series
              grouped_data <- filtered_data %>%
                group_by(KOMPAUN_YEAR, BAYARAN_SALURAN) %>%
                summarise(Count = n()) 
              
              return(grouped_data)  # Return the grouped data
            })
            
            # Get the grouped data from the reactive function
            grouped_data <- filteredDataList()
            
            # Create the Highcharter plot
            grouped_data %>%
              hchart(type = "line", hcaes(x = KOMPAUN_YEAR, y = Count, group = BAYARAN_SALURAN)) %>%
              hc_xAxis(type = "linear") %>%
              hc_yAxis(title = list(text = "BAYARAN_AMAUN")) %>%
              hc_legend(layout = "vertical") %>%
              hc_title(text = "Bilangan Bayaran mengikut Bayaran Saluran (Tahunan)")
          })
          output$plot4 <- renderPlotly({
            
            if (input$dynamicChoice_KUTIPAN == "KESELURUHAN") {
              unique_kawasan <- unique(df1$TEMPAT_ZON_NAMA)
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date, ]
              })
            } else {
              unique_kawasan <- unique(df1$TEMPAT_KAWASAN_NAMA[df1$TEMPAT_KAWASAN_NAMA == input$dynamicChoice_KUTIPAN & !is.na(df1$TEMPAT_KAWASAN_NAMA)])
              # Reactive expression to filter date in data
              filteredDataBayaran <- reactive({
                start_date <- input$date1_KUTIPAN
                end_date <- input$date2_KUTIPAN
                TEMPAT_KAWASAN_NAMA <- unique_kawasan
                df1[!is.na(df1$BAYARAN_TARIKH) & df1$BAYARAN_TARIKH >= start_date & df1$BAYARAN_TARIKH <= end_date & df1$TEMPAT_KAWASAN_NAMA == TEMPAT_KAWASAN_NAMA, ]
              })
            }
            
            
            
            # Calculate sum of values
            gaugeValue <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN, na.rm = TRUE)
            })
            sum_kompaun <- reactive({
              sum(ifelse(is.na(filteredDataBayaran()$KOMPAUN_AMAUN_LAMA), filteredDataBayaran()$KOMPAUN_AMAUN, filteredDataBayaran()$KOMPAUN_AMAUN_LAMA))
            })
            
            # Reactive expression for calculating the sum of RM
            sum_bayaran <- reactive({
              sum(filteredDataBayaran()$BAYARAN_AMAUN)
            })
            
            plot_ly(
              type = "indicator",
              value = gaugeValue(),
              mode = "gauge",
              gauge = list(
                axis = list(range = list(NULL, sum_kompaun())),
                bar = list(color = "#0077b6"),
                borderwidth = 2,
                bordercolor = "#000000",
                steps = list(
                  list(range = c(0, sum_kompaun() * 0.9), color = "lightgray"),
                  list(range = c(sum_kompaun() * 0.9, sum_kompaun()), color = "white")
                ),
                threshold = list(
                  line = list(color = "red", width = 3), #set threshold
                  thickness = 1,
                  value = sum_bayaran()
                )
              )
            ) %>%
              layout(
                title = list(font = list(size = 13, color = "black")),
                margin = list(t = 45), #defines the margins (spacing) around the plot area
                plot_bgcolor = "rgba(0,0,0,0)",
                paper_bgcolor = "rgba(0,0,0,0)",
                annotations = list(
                  
                  list(
                    text = paste("Jumlah Kutipan: RM ", format(sum_bayaran(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.5,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste("Jumlah Kompaun : RM", format(sum_kompaun(), big.mark = ",")),
                    showarrow = FALSE,
                    x = 0.5,
                    y = 0.4,
                    font = list(size = 10, color = "black")
                  ),
                  list(
                    text = paste0(round((sum_bayaran()/sum_kompaun())*100, digits = 2), "%"),
                    showarrow = FALSE,
                    x = 0.5,
                    y = -0.1,
                    font = list(size = 35, color = "black")
                  )
                )
              )
          }) 
        }
      }
    }
   
  })
  
  
  
  
  
  #KOMPAUN (LINE: JUMLAH KOMPAUN (RM))----
  
  
  
  output$line_plot2 <- renderPlotly({
    # Count occurrence by date
    a1 <- df1 %>%
      group_by(KOMPAUN_TARIKH) %>%
      summarize(Count = n(), .groups = "keep")
    
    # Join the complete dataset with the summarized data and fill missing values with 0
    a2 <- complete_data2 %>%
      left_join(a1, by = "KOMPAUN_TARIKH") %>%
      mutate(Count = replace_na(Count, 0))
    
    # Reactive function to filter data based on selected date range
    filteredData2 <- reactive({
      start_date <- input$date2[1]
      end_date <- input$date2[2]
      subset(a2, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
    })
    
    plot_ly(data = filteredData2(), 
            x = ~KOMPAUN_TARIKH, 
            y = ~Count, 
            type = 'scatter', 
            mode = 'lines') %>%
      layout(title = list(text = "Graf 1: Bilangan Kompaun Mengikut Hari"),
             xaxis = list(title = "HARI"), 
             yaxis = list(title = "BILANGAN KOMPAUN DIKELUARKAN"))
  })
  
  #KOMPAUN (GAUGE: JUMLAH KOMPAUN)----
  
  # Calculate sum of values
  gaugeValue <- reactive({
    sum(filteredDataBayaran()$BAYARAN_AMAUN, na.rm = TRUE)
  })
  
  # Render gauge graph
  output$gaugePlot <- renderPlotly({
    plot_ly(
      type = "indicator",
      value = gaugeValue(),
      mode = "gauge",
      gauge = list(
        axis = list(range = list(NULL, sum_kompaun())),
        bar = list(color = "#0077b6"),
        bgcolor = "#ffffff",
        borderwidth = 2,
        bordercolor = "#000000",
        steps = list(
          list(range = c(0, sum_kompaun() * 0.9), color = "lightgray"),
          list(range = c(sum_kompaun() * 0.9, sum_kompaun()), color = "white")
        ),
        threshold = list(
          line = list(color = "red", width = 3), #set threshold
          thickness = 1,
          value = sum_bayaran()
        )
      )
    ) %>%
      layout(
        title = list(font = list(size = 13, color = "black")),
        margin = list(t = 45), #defines the margins (spacing) around the plot area
        annotations = list(
          
          list(
            text = paste("Jumlah Kutipan: RM ", format(sum_bayaran(), big.mark = ",")),
            showarrow = FALSE,
            x = 0.5,
            y = 0.5,
            font = list(size = 10, color = "black")
          ),
          list(
            text = paste("Jumlah Kompaun : RM", format(sum_kompaun(), big.mark = ",")),
            showarrow = FALSE,
            x = 0.5,
            y = 0.4,
            font = list(size = 10, color = "black")
          ),
          list(
            text = paste0(round((sum_bayaran()/sum_kompaun())*100, digits = 2), "%"),
            showarrow = FALSE,
            x = 0.5,
            y = -0.1,
            font = list(size = 35, color = "black")
          )
        )
      )
  })
  
  
  
  
  ####Pie Chart Check Status
  # Reactive expression to filter date in data
  filteredData <- reactive({
    start_date <- input$date2[1]
    end_date <- input$date2[2]
    df1[df1$KOMPAUN_TARIKH >= start_date & df1$KOMPAUN_TARIKH <= end_date, ]
  })
  
  # Filter the data based on the selected KESALAHAN
  filteredDataStatus <- reactive({
    #if no selection is made, display all KESALAHAN graph
    if (is.null(input$selected_kesalahan) || length(input$selected_kesalahan) == 0) {
      
      filteredData()
      
    } else {
      
      #if there are selection made, display only selected KESALAHAN
      filteredData() %>% filter(KESALAHAN == input$selected_kesalahan)
    }
  })
  output$pieChart3 <- renderPlotly({
    # Calculate the count for each value of KOMPAUN_STATUS
    counts <- table(filteredDataStatus()$KOMPAUN_STATUS)
    
    # Create a dataframe for the pie chart
    pie_data <- data.frame(
      KOMPAUN_STATUS = names(counts),
      Count = as.numeric(counts),
      stringsAsFactors = FALSE
    )
    
    # Create the pie chart using plot_ly
    plot_ly(data = pie_data, labels = ~KOMPAUN_STATUS, values = ~Count, type = "pie")
  })
  
  #KOMPAUN (LINE CHART JENIS KESALAHAN) ---
  output$line_plotcopy <- renderPlotly({
    
    #exclude KOMPAUN_STATUS == BATAL & Count occurrence by DATE, KESALAHAN and KOMPAUN_STATUS
    d1 <- subset(df1, !(KOMPAUN_STATUS == "BATAL")) %>%
      group_by(KOMPAUN_TARIKH, KESALAHAN) %>%
      summarize(Count = n(), .groups = "drop")
    
    # Create a complete dataset of all combinations of DATE and KESALAHAN
    complete_data3 <- expand.grid(KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
                                  KESALAHAN = unique(df1$KESALAHAN))
    
    # Join the complete dataset with the summarized data and fill missing values with 0
    d2 <- complete_data3 %>%
      left_join(d1, by = c("KOMPAUN_TARIKH", "KESALAHAN")) %>%
      mutate(Count = replace_na(Count, 0))
    
    
    # Reactive function to filter data based on selected date range
    filteredData4 <- reactive({
      start_date <- input$date2[1]
      end_date <- input$date2[2]
      subset(d2, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
    })
    
    
    # Filter the data based on the selected KESALAHAN
    filteredData7 <- reactive({
      #if no selection is made, display all KESALAHAN graph
      if (is.null(input$selected_kesalahan) || length(input$selected_kesalahan) == 0) {
        filteredData4()
        #subset(filteredData4(),KESALAHAN  %in% input$selected_kesalahan)
      } else {
        #if there are selection made, display only selected KESALAHAN
        filteredData4() %>% filter(KESALAHAN == input$selected_kesalahan)
      }
      #subset(filteredData4(),KESALAHAN  %in% input$selected_kesalahan)
    })
    
    ggplot(filteredData7(), 
           aes(x = KOMPAUN_TARIKH, 
               y = Count, 
               color = KESALAHAN, 
               group = KESALAHAN)) +
      geom_line() +
      labs(x = "HARI", 
           y = "BILANGAN KOMPAUN", 
           title = "Graf 5: Bilangan Kompaun Mengikut Kesalahan") +
      scale_color_discrete(name = "KESALAHAN")
  })
  
  #KOMPAUN (BARPLOT: JENIS KESALAHAN)----
  
  output$barPlot <- renderPlotly({
    
    #Count occurrence by DATE and KESALAHAN
    c1 <- df1 %>%
      group_by(KOMPAUN_TARIKH, KESALAHAN) %>%
      summarize(Count = n(), .groups = "keep")
    
    # Join the complete dataset with the summarized data and fill missing values with 0
    c2 <- complete_data2 %>%
      left_join(c1, by = "KOMPAUN_TARIKH") %>%
      mutate(Count = replace_na(Count, 0))
    
    # Reactive function to filter data based on selected date range
    filteredData3 <- reactive({
      start_date <- input$date2[1]
      end_date <- input$date2[2]
      subset(c2, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
      
    })
    
    plot_ly(data = filteredData3(), 
            x = ~KESALAHAN, 
            y = ~Count, 
            type = "bar")   %>%
      layout(
        title = list(font = list(weight = "bold", size = 16)),
        xaxis = list(tickangle = 300, title = "KESALAHAN"), 
        yaxis = list(title = "BIL. KOMPAUN"))
    
  })
  
  #LINE PLOT: KESALAHAN----    
  output$line_plot <- renderPlotly({
    
    #exclude KOMPAUN_STATUS == BATAL & Count occurrence by DATE, KESALAHAN and KOMPAUN_STATUS
    d1 <- subset(df1, !(KOMPAUN_STATUS == "BATAL")) %>%
      group_by(KOMPAUN_TARIKH, KESALAHAN) %>%
      summarize(Count = n(), .groups = "keep")
    
    # Create a complete dataset of all combinations of DATE and KESALAHAN
    complete_data3 <- expand.grid(KOMPAUN_TARIKH = seq(min(df1$KOMPAUN_TARIKH), max(df1$KOMPAUN_TARIKH), by = "day"),
                                  KESALAHAN = unique(df1$KESALAHAN))
    
    # Join the complete dataset with the summarized data and fill missing values with 0
    d2 <- complete_data3 %>%
      left_join(d1, by = c("KOMPAUN_TARIKH", "KESALAHAN")) %>%
      mutate(Count = replace_na(Count, 0))
    
    
    # Reactive function to filter data based on selected date range
    filteredData4 <- reactive({
      start_date <- input$date2[1]
      end_date <- input$date2[2]
      subset(d2, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
    })
    
    
    # Filter the data based on the selected KESALAHAN
    filteredData7 <- reactive({
      #if no selection is made, display all KESALAHAN graph
      if (is.null(input$selected_kesalahan) || length(input$selected_kesalahan) == 0) {
        
        filteredData4()
        
      } else {
        
        #if there are selection made, display only selected KESALAHAN
        filteredData4() %>% filter(KESALAHAN == input$selected_kesalahan)
      }
      
    })
    
    ggplot(filteredData7(), 
           aes(x = KOMPAUN_TARIKH, 
               y = Count, 
               color = KESALAHAN, 
               group = KESALAHAN)) +
      geom_line() +
      labs(x = "HARI", 
           y = "BILANGAN KOMPAUN", 
           title = "Bilangan Kompaun Mengikut Kesalahan") +
      scale_color_discrete(name = "KESALAHAN")
  })
  
  #STACKED BAR: TUNGGAKAN----
  a1 <- df3 %>%
    #new_date <- floor_date(ymd(original_date), "month")
    #mutate(BULAN_TERDEKAT = floor_date(KOMPAUN_TARIKH, "month")) %>%
    group_by(KOMPAUN_TARIKH, BAYARAN) %>%
    summarise(TOTAL = sum(TOTAL_BAYARAN)) %>%
    filter(BAYARAN != "TIADA BAYARAN")
  
  filteredData1 <- reactive({
    start_date <- input$date1
    end_date <- input$date2
    subset(a1, KOMPAUN_TARIKH >= start_date & KOMPAUN_TARIKH <= end_date)
  })
  
  # Create the stacked bar chart using highcharter
  output$stackedBarPlotTunggakan <- renderHighchart({
    a2 <- filteredData1() %>%
      mutate(BULAN_TERDEKAT = floor_date(KOMPAUN_TARIKH, "month")) %>%
      group_by(BULAN_TERDEKAT, BAYARAN) %>% 
      summarise(TOTAL_AMAUN = sum(TOTAL))
    
    hchart(a2, "column", hcaes(x = BULAN_TERDEKAT, y = TOTAL_AMAUN, group = BAYARAN)) %>%
      hc_chart(backgroundColor = "rgba(0,0,0,0)") %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_xAxis(title = list(text = "Bulan"), tickInterval = 30 * 24 * 3600 * 1000, type = "datetime", labels = list(format = "{value: %b %Y}")) %>%
      hc_yAxis(title = list(text = "Amaun Kompaun (RM)")) %>%
      hc_legend(align = "center", verticalAlign = "top", layout = "horizontal") %>%
      hc_colors(c("#2196F3", "#FF5722", "#4CAF50", "#FFC107")) %>%
      hc_add_theme(hc_theme_elementary()) %>%
      hc_size(width = 1150, height = 800) %>%
      hc_exporting(enabled = TRUE)
  })
  
  #KESALAHAN (TABLE: BAYARAN DETAILS)---- 
  filteredData5 <- reactive({
    subset(df1[c("KOMPAUN_NOMBOR","KOMPAUN_AMAUN","KOMPAUN_STATUS","BAYARAN_AMAUN", "BAYARAN_TARIKH", "BAYARAN_SALURAN")],
           KOMPAUN_NOMBOR  %in% input$selected_ids)
  })
  
  # Render the table using DT::renderDT
  output$selected_table <- DT::renderDT({
    DT::datatable(filteredData5(), options = list(pageLength = 10))  # Set the page length to control the number of rows displayed
  })
  
  
  #KESALAHAN (PIE CHART: STATUS BAYARAN))----
  
  # Generate pie chart based on the filtered data
  output$pieChart <- renderPlotly({
    count_data <- table(filteredData()$BAYARAN)
    plot_ly(labels = names(count_data), 
            values = count_data, 
            type = "pie",
            marker = list( #0077b6
              colors = c("#0077B6","#0C4C8A","#00B4D8","#97D5E0", "#CAF0F8"))) %>%
      layout(height = 250, width = 500,
             showlegend = TRUE)
  })
  
  #KESALAHAN (PIE CHART: SALURAN BAYARAN))----
  # Generate pie chart based on the filtered data
  output$pieChart2 <- renderPlotly({
    count_data2 <- table(filteredData()$BAYARAN_SALURAN)
    plot_ly(labels = names(count_data2), 
            values = count_data2, 
            type = "pie", #"#1F375D", "#97D5E0", "#0077B6", "#00B4D8", "#CAF0F8"
            marker = list(  # 
              colors = c("#0C4C8A","#00B4D8","#0077B6","#97D5E0", "#CAF0F8"))) %>%
      layout(
        showlegend = TRUE)
  })
  
  
  #PREDICTION----
  
  predicted_data <- reactive({
    for_tbats <- forecast::forecast(model_tbats, h = input$duration)
  })
  
  # Generate the plot
  output$predictionPlot <- renderPlot({
    plot(predicted_data(), main = "Prediction Graph", 
         xlab = "TAHUN", ylab = "BIL. KOMPAUN")
  })
  
  predicted_table <- reactive({
    end_month<-max(pred1$MONTH)
    
    df3 <- data.frame(
      BULAN = format(seq(end_month, by = "months", length.out = input$duration), "%b %Y"),
      ANGGARAN = predicted_data()$mean)
    
    df3<- df3 %>% 
      mutate(ANGGARAN = format(ANGGARAN, digits = 2, nsmall = 0)) %>% 
      rename("ANGGARAN KOMPAUN" = ANGGARAN) 
  })
  
  # Render the table
  output$table <- renderTable({
    predicted_table()})
  
  #PRESCRIPTIVE (MULTIPLE BAR CHART: RAYUAN)----
  # Count occurrence by date
  e1 <- df1 %>%
    filter(PINDAAN_TINDAKAN != "PEMBATALAN") %>%
    group_by(KOMPAUN_TARIKH, PINDAAN_TINDAKAN, BAYARAN, KESALAHAN) %>%
    summarise(Count = n(), .groups = "keep")
  
  
  # Filter the data based on user inputs
  filteredData8 <- reactive({
    if (is.null(input$kesalahan)) {
      return(e1)  # Display all data when no selection is made
    } else {
      subset(e1, KESALAHAN == input$kesalahan)
    }
  })
  
  output$plot1 <- renderPlotly({
    e3 <- ggplot(filteredData8(), 
                 aes(x = PINDAAN_TINDAKAN, 
                     fill = BAYARAN, 
                     weight = Count)) +
      geom_bar(position = "dodge") +
      xlab("JENIS TINDAKAN") +
      ylab("BIL. KOMPAUN") +
      theme_minimal()
    ggplotly(e3)
  })
  #PRESCRIPTIVE (PIE CHART: JENIS UNDANG2)----
  # Count occurrence by date
  f1 <- df1 %>%
    filter(PINDAAN_TINDAKAN != "PEMBATALAN") %>%
    group_by( PINDAAN_TINDAKAN, BAYARAN, KESALAHAN) %>%
    summarise(Count = n(), .groups = "keep")
  
  # Filter the data based on user inputs
  filteredData9 <- reactive({
    if (is.null(input$tindakan) || is.null(input$bayaran)) {
      return(f1)  # Display all data when no selection is made
    } else {
      subset(f1, PINDAAN_TINDAKAN == input$tindakan & BAYARAN == input$bayaran)
    }
  })
  
  # Create the pie chart based on the filtered data
  output$plot2 <- renderPlotly({
    data <- filteredData9()
    plot <- plot_ly(data, labels = ~KESALAHAN, values = ~Count, type = "pie")
    plot
  })
  
  #PRESCRIPTIVE (BAR: BAYARAN KAUNTER)----
  # Count occurrence by date
  g1 <- df1 %>%
    filter(PINDAAN_TINDAKAN != "PEMBATALAN") %>%
    filter(BAYARAN_SALURAN == "KAUNTER") %>%
    filter(BAYARAN == "SELESAI") %>%
    filter(TEMPAT_KAWASAN_NAMA != "TIADA REKOD") %>%
    group_by( TEMPAT_KAWASAN_NAMA) %>%
    summarise(Count = n(), .groups = "keep")
  
  # Create the pie chart based on the filtered data
  output$plot3 <- renderPlotly({
    
    # Reorder the levels of KESALAHAN in descending order based on COUNT
    g1 <- g1[order(-g1$Count),]
    
    plot_ly(data = g1, 
            x = ~reorder(TEMPAT_KAWASAN_NAMA, -Count), 
            y = ~Count, 
            type = "bar")   %>%
      layout(
        title = list(font = list(weight = "bold", size = 16)),
        xaxis = list(tickangle = 300, 
                     title = "TEMPAT",
                     categoryorder = "array", 
                     categoryarray = ~TEMPAT_KAWASAN_NAMA), 
        yaxis = list(title = "BIL. KOMPAUN"))
  })
  
  #PRESCRIPTIVE (ACTION BOX)----
  
  e2 <- df1 %>%
    filter(PINDAAN_TINDAKAN != "PEMBATALAN") %>%
    group_by( PINDAAN_TINDAKAN, BAYARAN, KESALAHAN) %>%
    summarise(Count = n(), .groups = "keep")
  
  rayuan_belumSelesai<-sum(e2$Count[e2$PINDAAN_TINDAKAN == "RAYUAN" & e2$BAYARAN == "BELUM SELESAI"])
  rayuan_Selesai<-sum(e2$Count[e2$PINDAAN_TINDAKAN == "RAYUAN" & e2$BAYARAN == "SELESAI"])
  tiadaTindakan_belumSelesai<-sum(e2$Count[e2$PINDAAN_TINDAKAN == "TIADA TINDAKAN" & e2$BAYARAN == "BELUM SELESAI"])
  tiadaTindakan_Selesai<-sum(e2$Count[e2$PINDAAN_TINDAKAN == "TIADA TINDAKAN" & e2$BAYARAN == "SELESAI"])
  
  calculation1 <- round((rayuan_Selesai/sum(rayuan_Selesai,tiadaTindakan_Selesai))*100,2)
  calculation2 <- round((tiadaTindakan_Selesai/sum(rayuan_Selesai,tiadaTindakan_Selesai))*100,2)
  calculation3 <- round(((tiadaTindakan_belumSelesai-rayuan_belumSelesai)/sum(e2$Count))*100,2)
  
  
  # Get the TEMPAT value with the highest sum of AMAUN
  max_tempat <-g1$TEMPAT_KAWASAN_NAMA[which.max(g1$Count)]
  
  # Render the text based on the calculation result
  output$calculationText <- renderUI({
    text1 <- "1) Dalam kalangan individu yang TELAH MENYELESAIKAN bayaran kompaun, "
    text2 <- "% adalah golongan yang TELAH MEMBUAT RAYUAN pengurangan kompaun. "
    text3 <- "Manakala hanya "
    text4 <- "% dari golongan yang TIDAK MEMOHON pengurangan kompaun."
    text5 <- "1.1) Dalam kalangan individu yang masih BELUM MENYELESAIKAN bayaran kompaun, golongan yang TIDAK MEMOHON pengurangan kompaun adalah "
    text6 <- "% lebih tinggi berbanding golongan yang TELAH MEMBUAT RAYUAN pengurangan kompaun."
    text7 <- "ACTION: Menggalakkan lebih ramai penerima kompaun untuk MEMBUAT RAYUAN pengurangan kompaun, supaya lebih ramai dapat MENYELESAIKAN PEMBAYARAN dengan segera."
    text8 <- "2) Berdasarkan data, kawasan "
    text9 <- " mempunyai jumlah tertinggi yang MEMBUAT PEMBAYARAN di kaunter."
    text10 <- "ACTION: Membuka lebih banyak kaunter di kawasan tersebut."
    # Combine the texts with the <br> tag using paste() or paste0()
    combined_text <- paste0(text1, calculation1, text2, "<br>",
                            text3, calculation2, text4, "<br>", "<br>",
                            text5, calculation3, text6, "<br>", "<br>",
                            text7, "<br>", "<br>", text8, max_tempat, text9, "<br>", "<br>", text10)
    
    # Return the combined text wrapped in HTML()
    HTML(combined_text)
  })
}

# Run the Shiny app
shinyApp(ui, server)