#===============================================================================================
#-- Importing Libraries 

library(shiny)
library(car)
library(DescTools)
library(DT)
library(tidyverse)
#===============================================================================================





#===============================================================================================
#-- UI section

ui <- navbarPage("Statistical Analysis App",
                 
                 
                 # Normality Panel #------------------------------------------------------------
                 
                 tabPanel("Normality",
                          fluidPage(
                            
                            # App title ----
                            titlePanel(h2("Normality Test ", align="left")),
                            br(),
                            
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(
                              
                              # Sidebar panel for inputs ----
                              sidebarPanel(
                                "Product by Kian.B",
                                br(),
                                Sys.Date(),
                                br(),
                                br(),
                                
                                # Input: Select a file ----
                                fileInput("file1_1", "Choose your CSV File",
                                          multiple = TRUE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Checkbox if file has header ----
                                checkboxInput("header1", "Header", TRUE),
                                
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Select number of rows to display ----
                                radioButtons("disp_1", "Display",
                                             choices = c(Head = "head",
                                                         All = "all"),
                                             selected = "head"),
                                
                                # Input: Select target column ----
                                selectInput(inputId = "target_column_normality",
                                            label = "Choose a column for Normality test:",
                                            choices = c("")),
                                
                                # Input: Select normality method ----
                                selectInput(inputId = "method_normality",
                                            label = "Normality Statistic Method:",
                                            choices = c("Shapiro-Wilk","Kolmogorov-Smirnov"),
                                            selected = "Shapiro-Wilk"),
                                
                                # Input: Apply the method on your data ----
                                actionButton("action", "Run"),
                                br(),
                                downloadButton(
                                  outputId = "report1",
                                  label = "Generate report"
                                )
                                
                              ),
                              
                              # Main panel for displaying outputs ----
                              mainPanel(
                                
                                # Output: Data file ----
                                tableOutput("contents_1"),
                                verbatimTextOutput("contents_2")
                                
                              ),
                            ),
                          )),  
                 
                 # Compare Many Groups #------------------------------------------------------------
                 
                 tabPanel("Compare Many Groups",
                          fluidPage(
                            
                            # App title ----
                            titlePanel(h2("Compare Many Groups ", align="left")),
                            br(),
                            
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(
                              
                              # Sidebar panel for inputs ----
                              sidebarPanel(
                                "Product by Kian.B",
                                br(),
                                Sys.Date(),
                                br(),
                                br(),
                                
                                # Input: Select a file ----
                                fileInput("file1_2", "Choose your CSV File",
                                          multiple = TRUE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Checkbox if file has header ----
                                checkboxInput("header2", "Header", TRUE),
                                
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Select number of rows to display ----
                                radioButtons("disp_2", "Display",
                                             choices = c(Head = "head",
                                                         All = "all"),
                                             selected = "head"),
                                
                                # Input: Select labels column ----
                                selectInput(inputId = "labels_column_comparing",
                                            label = "Choose a column as Labels column:",
                                            choices = c("")),
                                
                                # Input: Select values column ----
                                selectInput(inputId = "values_column_comparing",
                                            label = "Choose a column as Values column:",
                                            choices = c("")),
                                
                                # Input: Select control label ----
                                selectInput(inputId = "control_label",
                                            label = "Choose the label of control group:",
                                            choices = c("")),
                                
                                # Input: Select Comparing method ----
                                selectInput(inputId = "method_comparing",
                                            label = "Many Groups Comparing Method:",
                                            choices = c("One Way ANOVA","ANOVA on Ranks"),
                                            selected = "ANOVA one way"),
                                
                                # Input: Select normality auto-check method ----
                                selectInput(inputId = "method_normality_auto",
                                            label = "Normality Auto-check Method:",
                                            choices = c("Shapiro-Wilk","Kolmogorov-Smirnov"),
                                            selected = "Shapiro-Wilk"),
                                
                                # Input: Apply the method on your data ----
                                actionButton("action2", "Run"),
                                br(),
                                downloadButton(
                                  outputId = "report2",
                                  label = "Generate report"
                                )
                                
                              ),
                              
                              # Main panel for displaying outputs ----
                              mainPanel(
                                
                                # Output: Data file ----
                                tableOutput("contents_3"),
                                verbatimTextOutput("contents_4")
                                
                              ),
                            ),
                          )),
                          
                 
                 # Non-Numeric #------------------------------------------------------------
                 
                 tabPanel("Non-Numeric",
                          fluidPage(
                            
                            # App title ----
                            titlePanel(h2("Non-Numeric Tests ", align="left")),
                            br(),
                            
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(
                              
                              # Sidebar panel for inputs ----
                              sidebarPanel(
                                "Product by Kian.B",
                                br(),
                                Sys.Date(),
                                br(),
                                
                                # Input: Enter Data ----
                                DTOutput("my_datatable"),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Select Comparing method ----
                                selectInput(inputId = "method_comparing_non_numeric",
                                            label = "Non-Numeric Comparing Method:",
                                            choices = c("Chi-Square","Fisher Exact"),
                                            selected = "Chi-Square"),
                                
                                # Input: Apply the method on your data ----
                                actionButton("action3", "Run"),
                                downloadButton(
                                  outputId = "report3",
                                  label = "Generate report"
                                )
                                
                              ),
                              
                              # Main panel for displaying outputs ----
                              mainPanel(
                                
                                # Output: Data file ----
                                verbatimTextOutput("contents_5")
                                
                              ),
                            ),
                          )),
                 
                 
                 # Compare Two Groups #------------------------------------------------------------
                 
                 tabPanel("Compare Two Groups",
                          fluidPage(
                            
                            # App title ----
                            titlePanel(h2("Compare Two Groups ", align="left")),
                            br(),
                            
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(
                              
                              # Sidebar panel for inputs ----
                              sidebarPanel(
                                "Product by Kian.B",
                                br(),
                                Sys.Date(),
                                br(),
                                br(),
                                
                                # Input: Select a file ----
                                fileInput("file1_3", "Choose your CSV File",
                                          multiple = TRUE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Checkbox if file has header ----
                                checkboxInput("header3", "Header", TRUE),
                                
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Select number of rows to display ----
                                radioButtons("disp_3", "Display",
                                             choices = c(Head = "head",
                                                         All = "all"),
                                             selected = "head"),
                                
                                # Input: Select labels column ----
                                selectInput(inputId = "labels_column_comparing_2groups",
                                            label = "Choose a column as Labels column:",
                                            choices = c("")),
                                
                                # Input: Select values column ----
                                selectInput(inputId = "values_column_comparing_2groups",
                                            label = "Choose a column as Values column:",
                                            choices = c("")),
                                
                                # Input: Select Comparing method ----
                                selectInput(inputId = "method_comparing_2groups",
                                            label = "Two Groups Comparing Method:",
                                            choices = c("T-test","Mann-Whitney Rank Sum Test"),
                                            selected = "T-test"),
                                
                                # Input: Select normality auto-check method ----
                                selectInput(inputId = "method_normality_auto_2groups",
                                            label = "Normality Auto-check Method:",
                                            choices = c("Shapiro-Wilk","Kolmogorov-Smirnov"),
                                            selected = "Shapiro-Wilk"),
                                
                                # Input: Apply the method on your data ----
                                actionButton("action4", "Run"),
                                br(),
                                downloadButton(
                                  outputId = "report4",
                                  label = "Generate report"
                                )
                                
                              ),
                              
                              # Main panel for displaying outputs ----
                              mainPanel(
                                
                                # Output: Data file ----
                                tableOutput("contents_6"),
                                verbatimTextOutput("contents_7")
                                
                              ),
                            ),
                          )),
                 
                 
                 
                 
inverse = T                          
)
#===============================================================================================     


  

          
#===============================================================================================
#-- Define server logic to read selected file

server <- function(input, output, session) {
  
  
  
  # Output 1 #------------------------------------------------------------------
  
  output$contents_1 <- renderTable({
    
    req(input$file1_1)
    # Import Data ----
    df <- read.csv(input$file1_1$datapath,
                   header = input$header1,
                   sep = ",")
    # Update Selector ----
    observe({
      updateSelectInput(
        session,
        "target_column_normality",
        choices = c(names(df))
      )
    })
    
    # Heading check ---- 
    if(input$disp_1 == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
      
  })

  
  
  # Output 2 #------------------------------------------------------------------
  
  output$contents_2 <- renderPrint({
    
    #Import Data ----
    req(input$file1_1)
    df <- read.csv(input$file1_1$datapath,
                   header = input$header1,
                   sep = ",")
    
    # Applying Normality test method ----
    if(input$action){
      
      
      # Import Data ----
      target_column  <- isolate(input$target_column_normality)
      
      #Applying Normality Method on target column ----
      if(isolate(input$method_normality) == "Shapiro-Wilk"){
        result_normality = shapiro.test(df[,target_column])
      }
      else{
        result_normality = ks.test(df[,target_column],"pnorm",mean(df[,target_column]),sd(df[,target_column]))
      }
      
      # Print the Result ----
      result_normality$data.name = c("  ",isolate(input$target_column_normality))
      print(result_normality)
      if(result_normality$p.value>=0.05){
        print("Normality Test: Passed")
        flag = "Passed" # flag for report
      }
      else{print("Normality Test: Failed")
        flag = "Failed"
      }
    }
    
    # Design report ----
    output$report1 <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report1.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report1.Rmd")
        file.copy("report1.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(m = input$method_normality,p=result_normality$p.value,
                       c=isolate(input$target_column_normality),n=flag)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )

  })
  
  
  
  # Output 3 #------------------------------------------------------------------
  
  output$contents_3 <- renderTable({
    
    # Import Data ----
    req(input$file1_2)
    df <- read.csv(input$file1_2$datapath,
                   header = input$header2,
                   sep = ",")
    
    # Update Selector ----
    observe({
      updateSelectInput(
        session,
        "labels_column_comparing",
        choices = c(names(df))
      )
    })
    observe({
      updateSelectInput(
        session,
        "values_column_comparing",
        choices = c(names(df))
      )
    })
    observe({
      updateSelectInput(
        session,
        "control_label",
        choices = c(unique(df[,1]))
      )
    })
    
    
    # Heading check ---- 
    if(input$disp_2 == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  
  # Output 4 #------------------------------------------------------------------
  
  output$contents_4 <- renderPrint({
    
    # Import Data ----
    req(input$file1_2)
    df <- read.csv(input$file1_2$datapath,
                   header = input$header2,
                   sep = ",")
    
    # Applying Normality test method ----
    if(input$action2){
      variable_column  <- isolate(input$values_column_comparing)
      treatment_column <- isolate(input$labels_column_comparing)
      res_aov <- aov(df[,variable_column] ~ df[,treatment_column], data = df)
      
      # Checking for Equality variance ----
      result_Equality_Variance_auto_check = leveneTest(df[,variable_column]~df[,treatment_column],df)
      
      # Checking Normality-auto check Method on variable column ----
      if(isolate(input$method_normality_auto) == "Shapiro-Wilk"){
        result_normality_auto_check = shapiro.test(res_aov$residuals)
      }
      else{
        result_normality_auto_check = ks.test(res_aov$residuals,"pnorm",mean(res_aov$residuals),sd(res_aov$residuals))
      }
      
      # Printing Normality auto-check result ----
      if(result_normality_auto_check$p.value>=0.05){
        final_result_normality_auto_check <- "Passed"
      }
      else{final_result_normality_auto_check <- "Failed"
      }
      if(result_Equality_Variance_auto_check[1,3]>=0.05){
        final_result_Equality_Variance_auto_check <- "Passed"
      }
      else{final_result_Equality_Variance_auto_check <- "Failed"
      }
      cat("Normality Test: ", final_result_normality_auto_check," (P=", result_normality_auto_check$p.value,")", "\n")
      cat("Equal Variance Test: ", final_result_Equality_Variance_auto_check," (P=", result_Equality_Variance_auto_check[1,3],")","\n","","\n","","\n")
      
      # Applying Anova/Anova on ranks on the data & Dunnett/Dunn ----
      if(isolate(input$method_comparing) == "One Way ANOVA" && final_result_normality_auto_check == "Passed" 
         && final_result_Equality_Variance_auto_check == "Passed"){
        result_test = summary(res_aov)
        rownames(result_test[[1]])[1] <- "Data"
        cat("One Way ANOVA test: ","\n")
        print(result_test)
        p_value_test = summary(res_aov)[[1]][1,5]
        if(p_value_test<0.05){
          result_test_Dunn_Dunnet <- DunnettTest(df[,variable_column],df[,treatment_column],control=input$control_label)
          print(result_test_Dunn_Dunnet)
        }
      }
      else{
        result_test <- kruskal.test(df[,variable_column] ~ df[,treatment_column], data = df)
        result_test$data.name <- c(" <",isolate(input$values_column_comparing),">"," Column")
        p_value_test = result_test$p.value
        if(isolate(input$method_comparing) == "One Way ANOVA"){
          cat("Because of the P-value Quantity, Statitic Analysis is transfered to ANOVA on Ranks... ","\n")
        }
        else{
          cat("ANOVA on Ranks test: ","\n")
        }
        print(result_test)
        if(p_value_test<0.05){
          result_test_Dunn_Dunnet <- DunnTest(df[,variable_column],df[,treatment_column],control=input$control_label)
          print(result_test_Dunn_Dunnet)
        }
      }
      
      # Design Report ----
      output$report2 <- downloadHandler(
        
        # For PDF output, change this to "report.pdf" ----
        filename = "report2.pdf",
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report2.Rmd")
          file.copy("report2.Rmd", tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd document
          params <- list(m=input$method_comparing,c=variable_column,m1=isolate(input$method_normality_auto),
                         p=result_normality_auto_check$p.value,p1=result_Equality_Variance_auto_check[1,3],
                         r=result_test,pt=p_value_test,rr=if(exists("result_test_Dunn_Dunnet")){result_test_Dunn_Dunnet}else{0})
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )
      
    }
    
  })
  
  
  
  # Create a 5*5 DataFrame #----------------------------------------------------
  
  v <- reactiveValues(data = {data.frame(matrix(NA, nrow = 5, ncol = 5))
  })
  
  # Output the DataTable based on the DataFrame (and make it editable) ----
  output$my_datatable <- renderDT({
    DT::datatable(v$data, editable = TRUE)
  })
    
  observeEvent(input$my_datatable_cell_edit, {
    # get values
    info = input$my_datatable_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative 
      k <- k * -1
    }
    
    # write values to reactive ----
    v$data[i,j] <- k
  })  
  
  
  
  # Output 5 #------------------------------------------------------------------
  
  output$contents_5 <- renderPrint({
    if(input$action3){
      # checking for Test method selected ----
      if(isolate(input$method_comparing_non_numeric=="Chi-Square")){
        result_chisq <- chisq.test(na.omit(v$data[ , colSums(is.na(v$data)) < nrow(v$data)])) 
        result_chisq$data.name = " "
        result_test = result_chisq
        print(result_chisq)
      }
      else{
        result_ficher_exact <- fisher.test(na.omit(v$data[ , colSums(is.na(v$data)) < nrow(v$data)]))
        result_ficher_exact$data.name = " "
        result_test = result_ficher_exact
        print(result_ficher_exact)
      }
      
      # checking for P-value ----
      if(result_test$p.value>=0.05){
        cat("\n","The proportions of observations in different columns of the contingency table do not vary from row to row.","\n", 
            "The two characteristics that define the contingency table are not significantly related. ","(P=",result_test$p.value,")")
      }
      else{
        cat("\n","The proportions of observations in different columns of the contingency table vary from row to row.","\n", 
            "The two characteristics that define the contingency table are significantly related. ","(P=",result_test$p.value,")")
      }
    }
    
    # Design Report -----
    output$report3 <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report3.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report3.Rmd")
        file.copy("report3.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(m=input$method_comparing_non_numeric,r=result_test,
                       pt=result_test$p.value)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
  })
  
  
  
  # output 6 #------------------------------------------------------------------
  
  output$contents_6 <- renderTable({
    
    # Import Data
    req(input$file1_3)
    df <- read.csv(input$file1_3$datapath,
                   header = input$header3,
                   sep = ",")
    
    # Update Selector
    observe({
      updateSelectInput(
        session,
        "labels_column_comparing_2groups",
        choices = c(names(df))
      )
    })
    observe({
      updateSelectInput(
        session,
        "values_column_comparing_2groups",
        choices = c(names(df))
      )
    })
    
    
    # Heading check  
    if(input$disp_3 == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  
  # Output 7 #------------------------------------------------------------------
  
  output$contents_7 <- renderPrint({
    
    # Import Data
    req(input$file1_3)
    df <- read.csv(input$file1_3$datapath,
                   header = input$header3,
                   sep = ",")
    
    # Applying Normality test method
    if(input$action4){
      variable_column  <- isolate(input$values_column_comparing_2groups)
      treatment_column <- isolate(input$labels_column_comparing_2groups)
      res_aov <- aov(df[,variable_column] ~ df[,treatment_column], data = df)
      
      # Checking for Equality variance 
      result_Equality_Variance_auto_check = leveneTest(df[,variable_column]~df[,treatment_column],df)
      
      # Checking Normality-auto check Method on variable column
      if(isolate(input$method_normality_auto_2groups) == "Shapiro-Wilk"){
        result_normality_auto_check = shapiro.test(res_aov$residuals)
      }
      else{
        result_normality_auto_check = ks.test(res_aov$residuals,"pnorm",mean(res_aov$residuals),sd(res_aov$residuals))
      }
      
      # Printing Normality auto-check result
      if(result_normality_auto_check$p.value>=0.05){
        final_result_normality_auto_check <- "Passed"
      }
      else{final_result_normality_auto_check <- "Failed"
      }
      if(result_Equality_Variance_auto_check[1,3]>=0.05){
        final_result_Equality_Variance_auto_check <- "Passed"
      }
      else{final_result_Equality_Variance_auto_check <- "Failed"
      }
      cat("Normality Test: ", final_result_normality_auto_check," (P=", result_normality_auto_check$p.value,")", "\n")
      cat("Equal Variance Test: ", final_result_Equality_Variance_auto_check," (P=", result_Equality_Variance_auto_check[1,3],")","\n","","\n","","\n")
      
      # Applying t-test/Mann-Whitney on data
      if(isolate(input$method_comparing_2groups) == "T-test" && final_result_normality_auto_check == "Passed" 
         && final_result_Equality_Variance_auto_check == "Passed"){
        result_test <- t.test(df[,variable_column]~df[,treatment_column])
        print(result_test)
        p_value_test = result_test$p.value
      }
      else{
        result_test <- wilcox.test(df[,variable_column] ~ df[,treatment_column], data = df)
        result_test$data.name <- c(" <",isolate(input$values_column_comparing_2groups),">"," Column")
        p_value_test = result_test$p.value
        if(isolate(input$method_comparing_2groups) == "T-test"){
          cat("Because of the P-value Quantity in Normality/Equal Variance Test, Statitic Analysis is transfered to Mann-Whitney ... ","\n")
        }
        else{
          cat("Mann-Whitney test: ","\n")
        }
        
        print(result_test)
      }
      
      # Design Report
      output$report4 <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report4.pdf",
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report4.Rmd")
          file.copy("report4.Rmd", tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd document
          params <- list(m=input$method_comparing_2groups,c=variable_column,m1=isolate(input$method_normality_auto),
                         p=result_normality_auto_check$p.value,p1=result_Equality_Variance_auto_check[1,3],
                         r=result_test,pt=p_value_test)
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )
      
    }
    
  })
  
  
}
#===============================================================================================





#===============================================================================================
#-- Create Shiny app 

shinyApp(ui, server)
#===============================================================================================




