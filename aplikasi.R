library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyverse)
library(readxl)
library(reshape)
library(plotly)
library(tidyr)
library(readxl)
library(sf)

df <- read.csv("telecom_churn.csv", sep = ";")
lang0 <- df[df$churn==0,"langganan_data"]
lang1 <- df[df$churn==1,"langganan_data"]
lang <- df[, "langganan_data"]

ui <- dashboardPage(
  skin = "purple-light",
  dashboardHeader(title = "Telco Churn Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("house")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("table-columns"),
      menuItem("Database", tabName = "database", icon = icon("database"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
    ),
    tabItems(
      tabItem(tabName = "beranda",
              titlePanel(
                carousel(width = 12,
                         id = "mycarousel",
                         carouselItem(
                           tags$img(src = "https://github.com/SFikri-Project/sehatirakyatdashboard.github.io/raw/main/home.jpg")
                         ))
              
              )
      ),
      tabItem(tabName = "dashboard"),
      tabItem(tabName = "visualisasi",
              fluidRow(box(title = h1(strong("Aktivitas Pelanggan Telco Dalam Se-Bulan")), status = "primary", solidHeader = FALSE,
                           width = 12)),
              fluidRow(box(title = strong("Filter Pelanggan"),
                           selectInput("Churndf", " ",
                                       choices = c("All" = "all", "Churn" = 1, "No-Churn" = 0)),
                           width = 12
              )),
              fluidRow(
                box(title = tags$b("Persentase Berlangganan Paket Data"), width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("BAR1DF", height = 300)),
                box(title = tags$b("Jumlah Menghubungi Customer Service"), width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("BAR2DF", height = 300))
              ),
              fluidRow(
                box(title = tags$b(tags$strong("Jumlah Denda Pelanggan (Dollar)"), style = "text-align:center;"),width = 6, status = "primary", solidHeader = TRUE,
                    sliderInput(input = "num",
                                label = "Pilih Panjang Bin",
                                value = 10, min = 1, max = 20),
                    plotOutput("HISTDF", height = 300)
                ),
                box(title = tags$b(tags$strong("Distribusi Variabel")), 
                    style = "text-align:center;", width = 6, status = "primary", solidHeader = TRUE,
                    selectInput(inputId = "selvio", 
                                label = "Pilih Variabel", 
                                choices = c("Lama Langganan" = "lama_langganan", "Penggunaan Data" = "penggunaan_data", 
                                            "Durasi Telepon" = "durasi_telpon","Banyak Telepon"="banyak_telpon", "Biaya Bulanan" = "biaya_bulanan", "Denda" = "denda", "Penggunaan Paket Roaming" = "durasi_min")),
                    plotOutput("violin", height = 320)
                )
              ),
              fluidRow(
                box(title = tags$b("Matriks Korelasi"), width = 12, status = "primary", solidHeader = TRUE,
                    DTOutput("correlation_table")
                ),
                box(title = tags$b("Pilih Variabel"), width = 12, 
                    box(checkboxGroupInput("selval1", label = "Pilih Variabel",
                                           choices = c("Lama Langganan" = "lama_langganan", "Contract Renewal" = "perbarui", "Penggunaan Data" = "penggunaan_data", "Telepon CS" = "telpon_cs", "Penggunaan Paket Roaming" = "durasi_min"),
                                           selected = c("penggunaan_data", "lama_langganan")),  
                        width = 6),
                    box(checkboxGroupInput("selval2", label = "Select Variabel",
                                           choices = c("Durasi Telepon" = "durasi_telpon","Banyak Telepon"="banyak_telpon", "Biaya Bulanan" = "biaya_bulanan", "Denda" = "denda"),
                                           selected = c("durasi_telpon", "biaya_bulanan")),  
                        width = 6)
                )
              )
      ),
      tabItem(tabName = "pemodelan",
              tabsetPanel(
                tabPanel("Regresi",
                         box(width = 6,title = "Prediksi Biaya Langganan Seluler Per Bulan",
                             fluidRow(
                )
              )),
      tabItem(tabName = "database",
              tabBox(id = "t2", width = 12,
                     tabPanel("Data", icon = icon("address-card"), dataTableOutput("dfb")),
                     tabPanel("Struktur", icon = icon("address-card"), verbatimTextOutput("structure")),
                     tabPanel("Summary", icon = icon("address-card"), verbatimTextOutput("sumari"))
              ))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  ### VISUALISASI
  output$Roam <- renderInfoBox({
    value <- if (input$Churndf == "0") {
      mean(df$durasi_min[df$churn == 0], na.rm = TRUE)
    } else if (input$Churndf == "1") {
      mean(df$durasi_min[df$churn == 1], na.rm = TRUE)
    } else {
      mean(df$durasi_min, na.rm = TRUE)
    }
    value = round(value,2)
    infoBox(tags$p("Durasi Roaming (Menit)", style = "font-weight:bold;"),
            value = paste(value, "Menit"),
            color = "red",
            fill = TRUE,
            icon = icon("globe"))
  })
  
  output$DayMins <- renderInfoBox({
    value <- if (input$Churndf == "0") {
      mean(df$durasi_telpon[df$churn == 0], na.rm = TRUE)
    } else if (input$Churndf == "1") {
      mean(df$durasi_telpon[df$churn == 1], na.rm = TRUE)
    } else {
      mean(df$durasi_telpon, na.rm = TRUE)
    }
    value = round(value,2)
    infoBox(tags$p("Durasi Telepon (Menit)", style = "font-weight:bold;"),
            value = paste(value,"Menit"),
            color = "green",
            fill = TRUE,
            icon = icon("phone-alt"))
  })
  
  
  output$MonthlyCharge <- renderInfoBox({
    value <- if (input$Churndf == "0") {
      mean(df$biaya_bulanan[df$churn == 0], na.rm = TRUE)
    } else if (input$Churndf == "1") {
      mean(df$biaya_bulanan[df$churn == 1], na.rm = TRUE)
    } else {
      mean(df$biaya_bulanan, na.rm = TRUE)
    }
    value = round(value,2)
    infoBox(tags$p("Biaya Bulanan", style = "font-weight:bold;"),
            value = paste("$", value,"/Month"),
            color = "yellow",
            fill = TRUE,
            icon = icon("money-check-alt"))
  })
  
  output$BAR1DF <- renderPlot({

    
    if (input$Churndf == "0") {
      lang_data <- lang0
    } else if (input$Churndf == "1") {
      lang_data <-lang1
    } else {
      lang_data <- lang
    }
    
    proportions <- prop.table(table(lang_data))
    
    pie_data <- data.frame(lang_data = factor(names(proportions)),
                           proportion = as.numeric(proportions))
    
    ggplot(pie_data, aes(x = "", y = proportion, fill = lang_data)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) + 
      theme_minimal() +
      labs(x = NULL, y = NULL, fill = "Status Berlangganan Data") +
      scale_fill_manual(values = c("#6495ed", "#E71D36"), 
                        labels = c("Tidak Berlangganan", "Berlangganan")) +
      geom_text(aes(label = scales::percent(proportion)), 
                position = position_stack(vjust = 0.5)) + 
      guides(fill = guide_legend(title = "Status Belangganan Data"))
  })
  
  
  output$BAR2DF <- renderPlot({
    value <- if (input$Churndf == "0") {
      df[df$churn == 0, ]
    } else if (input$Churndf == "1") {
      df[df$churn == 1, ]
    } else {
      df
    }
    
    ggplot(value, aes(x = factor(telpon_cs))) +  # Convert perbarui to factor
      geom_bar(fill = "#007bb8") +
      theme_minimal() +
      labs(x = "Jumlah Menghubungi CS", y = "Jumlah Pelanggan")
  })
  
  
  output$HISTDF <- renderPlot({
    value <- if (input$Churndf == "0") {
      df[df$churn == 0, ]
    } else if (input$Churndf == "1") {
      df[df$churn == 1, ]
    } else {
      df
    }
    
    ggplot(value) +
      aes(x = denda) +
      geom_histogram(bins = input$num, fill = "#007bb8") +
      theme_minimal() +
      labs(x = "Denda (Dollar)", y = "Jumlah Pelanggan") +
      theme(axis.text = element_text(size = 20))
  })
  
  output$correlation_table <- renderDataTable({
    df <- if (input$Churndf == "0") {
      df[df$churn == 0, ]
    } else if (input$Churndf == "1") {
      df[df$churn == 1, ]
    } else {
      df
    }
    selected_vars <- unique(c(input$selval1, input$selval2))
    filtered_df <- df %>%
      select(all_of(selected_vars))
    
    # Ensure only numeric columns are selected
    numeric_cols <- sapply(filtered_df, is.numeric)
    filtered_df <- filtered_df[, numeric_cols]
    
    corr_matrix <- cor(filtered_df, use = "complete.obs")
    corr_matrix_rounded <- round(corr_matrix, digits = 3)
    datatable(corr_matrix_rounded, options = list(scrollX = TRUE))
  })
  
  
  
  output$violin <- renderPlot({
    df_filtered <- if (input$Churndf == "0") {
      df[df$churn == 0, ]
    } else if (input$Churndf == "1") {
      df[df$churn == 1, ]
    } else {
      df
    }
    ggplot(df_filtered) +
      aes(x = as.factor(churn), y = !!sym(input$selvio), fill = as.factor(churn)) +  # Ensure dynamic column reference and fill
      geom_violin() +
      scale_fill_manual(values = c("#6495ed", "#E71D36")) +  # Add specified colors
      labs(x = "Churn Status", y = input$selvio, title = "Violin Plot by Churn Status") +
      theme_minimal()
  })

  observeEvent(input$predict_btn_klf, {
    
    new_data <- data.frame(
      lama_langganan = as.numeric(input$lama_langganan),
      perbarui = as.numeric(input$perbarui),
      penggunaan_data = as.numeric(input$penggunaan_data),
      telpon_cs = as.numeric(input$telpon_cs),
      durasi_telpon = as.numeric(input$durasi_telpon),
      banyak_telpon = as.numeric(input$banyak_telpon),
      biaya_bulanan = as.numeric(input$biaya_bulanan),
      denda = as.numeric(input$denda),
      durasi_min = as.numeric(input$durasi_min)
    )
    
    colnames(new_data) <- colnames(dfTrain[, -which(names(dfTrain) == "churn")])
    
    dnew <- xgb.DMatrix(data = as.matrix(new_data))
    prob_churn <- predict(model_xgboost, dnew)
    
    output$probability_pie <- renderPlot({
      df_pie <- data.frame(
        Category = c("Churn", "No-Churn"),
        Probability = c(prob_churn, 1 - prob_churn)
      )
      ggplot(df_pie, aes(x = "", y = Probability, fill = Category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        geom_text(aes(label = scales::percent(Probability)), position = position_stack(vjust = 0.5)) +
        labs(title = "Probabilitas Churn") +
        scale_fill_manual(values = c("#FF9999", "#66B2FF"))
    })
    
    output$interpretation <- renderText({
      paste("Interpretasi: Pelanggan berpotensi untuk churn sebesar", 
            round(prob_churn * 100, 2), "% dan tidak churn sebesar", 
            round((1 - prob_churn) * 100, 2), "%.")
    })
    
    output$model_summary <- renderPrint({
      summary(model_xgboost)
    })
    
    output$accuracy_output <- renderText({
      paste("Akurasi Model:", round(accuracy_xgboost, 4))
    })
    
    output$confusion_matrix_plot <- renderPlot({
      fourfoldplot(conf_matrix_xgboost$table, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Confusion Matrix")
    })
  })
}


shinyApp(ui, server)

