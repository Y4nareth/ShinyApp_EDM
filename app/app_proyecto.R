#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(caret)
require(C50)
require(randomForest)
require(kernlab)
require(nnet)
require(gbm)
require(plyr)
require(shiny)
require(cvAUC)
require(pROC)
require(ggplot2)
require(lime)
require(shinyjs)
require(shinycssloaders)
require(dplyr)
require(fastAdaboost)
require(shinyWidgets)
require(farff)
require(shinyalert)

jscode = 'shinyjs.closeWindow = function(){ window.close();}'


# Message in the info pop-up with instructions in HTML to align successfully
mensajeHTML <- '<p align="left">The uploaded arff or csv files must have the following requeriments:</p>
<p></p>
<ul>
    <li align="left">No missing values.</li>
    <li align="left">The class variable to predict must be in the last column.</li>
    <li align="left">The class must be a character factor (classification task).</li>
    <li align="left">Multiclass problems are not supported.</li>
</ul>
<p></p>
<p align="left">In the following link you can find some example files:</p>
<a href="https://github.com/Y4nareth/ShinyApp_EDM" target="_blank">https://github.com/Y4nareth/ShinyApp_EDM</a>'

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # App title ----
    titlePanel(windowTitle = "ROC & Explanations generator",tags$h1(tags$strong(tags$u("ROC & Explanations generator")))),
    
    tags$p(),
    useShinyalert(),
    actionButton("info", tags$strong("Info"), style="color:black;background-color:#90D450", width = "23.8%"),
    tags$p(),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(width = 3,
            
            tags$head(
                tags$style(".well {background-color: #DEF5F7; }")),
            
            # Input: Select a file ----
            fileInput("file1", "Select an arff or csv file:",
                      multiple = FALSE,
                      accept = c(".csv",".arff")),
            
            
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            checkboxInput("stringsasfactors", "Stringsasfactors", TRUE),
            selectInput("method", "Method:",
                        c("Random Forest" = "rf",
                          "Nearest Neighbours" = "knn",
                          "Decision Tree" = "C5.0",
                          "CART" = "rpart",
                          "Logistic Regression" = 'glm',
                          "Linear SVM" = "svmLinear",
                          "Radial SVM" = "svmRadial",
                          "Neural Network" = "nnet",
                          "GBM" = "gbm",
                          "AdaBoost" = "adaboost"
                          )),
            
            
            #Run button
            actionButton(inputId = "submit", label = "Run ROC", style="color:black;background-color: #0C9FC4"),
            
            # Horizontal line ----
            tags$hr(style = "border-top: 2px solid #000000;"),
            
            fileInput("file2", "Select an instance in a csv file to classify: ",
                      multiple = FALSE,
                      accept = c(".csv")),
            
            selectInput("dist_fun", "Distance Function:",
                        c("Euclidean" = "euclidean",
                          "Manhattan" = "manhattan",
                          "Gower" = "gower")),
            
            selectInput("feature_select", "Feature selection algorithm:",
                        c("Highest weights" = "highest_weights",
                          "Lasso path" = "lasso_path",
                          "Forward selection" = "forward_selection")),
            
            sliderInput("permutations", "Quantity of permutations:" , 0, 5000, 2500),
            
            #Run button
            actionButton(inputId = "submit_exp", label = "Run Explanations", style="color:black;background-color:#0C9FC4"),    
            
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(width = 9,
            
            # Output: Data file ----
            # tableOutput("contents")
            plotOutput("plotROC")%>% withSpinner(color = "#0dc5c1"),
            #renderPlot({#plot("contents", col = "blue")
            #   plot(c(1:10))})
            plotOutput("plotExplicaciones")%>% withSpinner(color = "#0dc5c1")
        )
        
    ),
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),
    actionButton("close", tags$strong("Close window"),
                 style="color:black;background-color:#C53800", width = "23.8%"),
    
    tags$footer("Created by Andres Calandin & Jose Giner",align="right",
                style="font-size:16px;")
    
)

# Define server logic required to draw ROC
server <- function(input, output) {
    
    site <- reactive({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        req(input$file1)
        #req(input$file2)
        
        if (strsplit(input$file1$datapath,".",fixed = TRUE)[[1]][2] == "csv"){
            df <- read.csv(input$file1$datapath,
                           header =input$header ,
                           stringsAsFactors=input$stringsasfactors)
        
        } else {
            if (strsplit(input$file1$datapath,".",fixed = TRUE)[[1]][2] == "arff") {
                df <- readARFF(input$file1$datapath)
            }
        }
        
        
        hv<-df
        namecl<-names(hv)[length(names(hv))]
        namepos<-as.character(hv[,namecl][1])
        ob<-paste(namecl,"~ .")
        
        set.seed(280)
        #hv<-hv[complete.cases(hv),]
        hv_index <- createDataPartition(hv[,namecl], p = .75, list = FALSE)
        tr <- hv[ hv_index, ]
        te <- hv[-hv_index, ]
        ob<-paste(namecl,"~ .")
        
        fitControl <-
            trainControl(
                method = "cv",
                number = 5,
                classProbs = T,
                savePredictions = T,
                summaryFunction = twoClassSummary
            )
        
        set.seed(7279)
        model <- train(as.formula(ob),
                        data = tr, 
                        trControl = fitControl,
                        method = input$method,
                        metric = "ROC")
        
        res.roc = roc(predictor = model$pred[,namepos], response = model$pred$obs)
        roc.data <- data.frame(
            thresholds = res.roc$thresholds,
            sensitivity = res.roc$sensitivities,
            specificity = res.roc$specificities
        )
        
        for_lift <- data.frame(Class = model$pred$obs, rf = model$pred[,namepos], resample = model$pred$Resample)
        lift_df <-  data.frame()
        for (fold in unique(for_lift$resample)) {
            fold_df <- dplyr::filter(for_lift, resample == fold)
            lift_obj_data <- lift(Class ~ rf, data = fold_df, class = namepos)$data
            lift_obj_data$fold = fold
            lift_df = rbind(lift_df, lift_obj_data)
        }
        lift_obj <- lift(Class ~ rf, data = for_lift, class = namepos)
        
        area = cvAUC(predictions = model$pred[,namepos], labels = model$pred$obs)
        
        
        #Ahora extraemos las explicaciones para la instancia de entrada
        explainer_caret = lime(tr, model)
        return(list("Curvas_cv" = lift_df,"Curva_ROC" = roc.data, "AUC" = area, "Explainer" = explainer_caret,"Clase_pos" = namepos))
        
    })
    
    
    output$plotROC <-  renderPlot({
        
        req(input$submit)
        lift_df = site()$Curvas_cv
        roc.data = site()$Curva_ROC
        area = site()$AUC
       
        
        # Plot ROC ----------------------------------------------------------------
        
        p = ggplot(lift_df) +
            geom_line(aes(1 - Sp, Sn, color = fold), size = 0.5)  +
            geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype="dashed", size=0.5) + 
            scale_color_discrete(guide = guide_legend(title = "Fold")) + ggtitle("Cross validation ROC") + 
            xlab("False Positive Rate") + ylab("True Positive Rate") +
            geom_label(label= paste("AUC: ", round(area$cvAUC,3)), x=0.75, y=0.25, size=5)
        
        p + geom_line(data = roc.data, aes(1 - specificity,sensitivity), size = 1, alpha = 0.75)
        
    })
    
    output$plotExplicaciones <-  renderPlot({
        req(input$file2)
        req(input$submit_exp)
        
        df2 <- read.csv(input$file2$datapath,
                        header =input$header ,
                        stringsAsFactors=input$stringsasfactors)
        
        explainer_caret = site()$Explainer
        namepos = site()$Clase_pos
        local_obs = df2
        set.seed(1506)
        explanation_caret = lime::explain(x = local_obs, explainer = explainer_caret, n_permutations = input$permutations, dist_fun = input$dist_fun, 
                                    kernel_width = 3, n_features = 10, feature_select = input$feature_select, labels = namepos)
        
        plot_features(explanation_caret)
    })  
    
    # Observes if the button Close is clicked
    observeEvent(input$close, {
        js$closeWindow()
        stopApp()
    })
    
    # Observes if the button Info is clicked
    observeEvent(input$info, {
        # Show a modal when the button is pressed
        shinyalert("Suggestions", mensajeHTML, type = "info", html=TRUE, confirmButtonCol="#0C9FC4")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server, options = list("launch.browser" = TRUE))


