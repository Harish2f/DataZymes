library(shiny)
library(shinyWidgets,quietly = TRUE)
library(shinydashboard,quietly = TRUE)
library(DT,quietly = TRUE)
library(shinydashboardPlus,quietly = TRUE)
library(lubridate,quietly = TRUE) 
library(readxl,quietly = TRUE)
library(janitor, quietly = TRUE)
library(plotly, quietly = TRUE)
library(viridis, quietly = TRUE)
library(cluster, quietly = TRUE)
library(fpc, quietly = TRUE)
library(rpart, quietly = TRUE)

US_States <- readxl::read_excel('US_States.xlsx',skip = 1)

myServer <- function(input, output,session){
    
    rv <- reactiveValues(data = NULL)
    
    observeEvent(input$file1,{
        file <- input$file1
        ext <- tools::file_ext(file$datapath)
        
        if(is.null(input$file1$datapath) | ext != "xls" ){
            showModal(modalDialog(
                title = "Warning",
                "Please upload a xls file",
                easyClose = TRUE
            ))
        } else{
            input$file1$datapath %>% 
                read_excel() %>% clean_names() -> rv$data
            
        }
    })
    
    # top range reactive to column selected
    output$top_rangeUI <- renderUI({
        req(input$column1)
        
        selectInput(
            "top_range",
            label = "Top range:",
            choices = if (input$column1 == "customer_name") {
                c("1-10", "11-20", "21-30", "31-40", "41-50")
                
            } else{
                c("1-10")
                
            },
            
            selected = "1-10",
            multiple = FALSE
        )
        
    })
    
    ## Select Top N Range Selection  ----
    observeEvent(input$top_range, {
        if (input$top_range == "1-10") {
            rv$min <- 1
            rv$max <- 10
        } else if (input$top_range == "11-20") {
            rv$min <- 11
            rv$max <- 20
        } else if (input$top_range == "21-30") {
            rv$min <- 21
            rv$max <- 30
        } else if (input$top_range == "31-40") {
            rv$min <- 31
            rv$max <- 40
        } else if (input$top_range == "41-50") {
            rv$min <- 41
            rv$max <- 50
        }
    })
    
    
    
    
    ## Drilldown plotly variable from Top N volume Graph ----
    
    Value_filter <- reactiveVal()
    
    ## Clear Drill down plots when "analyze by" column is changed
    observeEvent(input$column1, {
        Value_filter(NULL)
    })
    
    observeEvent(input$top_range, {
        Value_filter(NULL)
    })
    
    observeEvent(event_data("plotly_click", source = "Value_filter"), {
        Value_filter(event_data("plotly_click", source = "Value_filter")$y)
        
    })
    
    
    
    output$contents <- DT::renderDataTable({
        
        file <- input$file1
        ext <- tools::file_ext(file$datapath)
        # 
        req(file)
        
        rv$data <- rv$data %>%
            select_all(~gsub(" ","_", .)) %>%
            select_all(~gsub("[[:punct:]]", "_", .)) %>%
            select_all(~gsub("\r|\n", "_", .)) %>%
            select_all(~gsub("_+", "_", .)) %>%
            select_all(~gsub("_$", "", .))
        
        dt <- head(rv$data, n = 100)
        
        dt
        
    })
    
    output$Data_head <- renderText({
        if (!is.null(rv$data))
            HTML(paste0("<b> Data Overview - 100 Rows</b>"))
        
    })
    
    observeEvent(input$dateSelect1, {
        req(rv$data)
        
        rv$reqDataReceive <- rv$data %>%
            mutate(ordered_date  = as.Date(format(order_date,format = "%Y-%m-%d")),
                   shipped_date = as.Date(format(ship_date,format = "%Y-%m-%d")))
        
        rv$reqDataReceive <- rv$reqDataReceive %>% 
            mutate(
                ordered_floorMonth = floor_date(ordered_date, "month"),
                shipped_floorMonth  = floor_date(shipped_date, "month"),
                ordered_floorYear = floor_date(ordered_date, "year"),
                shipped_floorYear  = floor_date(shipped_date, "year"),
                ordered_month_label = factor(format(ordered_date,"%Y-%b"),
                                              levels = paste0(rep(2016:2019,each=12),"-",month.abb)),
                shipped_month_label = factor(format(shipped_date,"%Y-%b"),
                                            levels = paste0(rep(2016:2019,each=12),"-",month.abb)),
                ordered_year_label = factor(format(ordered_date,"%Y"),
                                              levels = c(2016,2017,2018,2019)),
                shipped_year_label = factor(format(shipped_date,"%Y"),
                                            levels = c(2016,2017,2018,2019))
                )
        
        rv$geodata <- dplyr::left_join(rv$reqDataReceive, US_States, by = c("state" = "State"))
        
        ## Data by received date
        
        rv$reqDataReceive1 <- rv$reqDataReceive %>%
            filter(ordered_year_label %in% c(input$dateSelect1))
        
    })
    
    observe({
        toggleState("breakd",
                    condition = !length(unique(rv$reqDataReceive1$ordered_year_label)) < 2 &&
                        !is.null(input$dateSelect1))
    })
    
    observe({
        shinyjs::hide("switch_tab")
        
        if(!is.null(rv$data))
            shinyjs::show("switch_tab")
        
        shinyjs::hide("dwdReceived")
        
        if(!is.null(rv$geo_Volume))
            shinyjs::show("dwdReceived")
        
        shinyjs::hide("dwdvolume")
        
        if(!is.null(rv$volumedf))
            shinyjs::show("dwdvolume")
        
        shinyjs::hide("switch_tab1")
        
        if(!is.null(input$dateSelect1))
            shinyjs::show("switch_tab1")
        
    })
    
    
    observeEvent(input$switch_tab, {
        req(input$file1)
        updateTabsetPanel(session, "inTabset",
                          selected = "Visualizations")
    })
    
    
    observeEvent(input$switch_tab1, {
        req(input$file1)
        req(input$dateSelect1)
        
        updateTabsetPanel(session, "inTabset",
                          selected = "K-means")
    })
    
    
    output$Volume <- renderPlotly({
        
        req(rv$reqDataReceive1)
        req(input$column1)
        req(input$top_range)
        req(input$dateSelect1)
        req(input$breakd)
        #req(input$loadData)
        
        rv$volumedf <- rv$reqDataReceive1 %>%
            count(get(input$column1)) %>%
            setNames(c("Column", "Frequency")) %>%
            arrange(desc(Frequency))
        
        if (
            input$breakd == "Overall") {
            rv$volumedf[rv$min:rv$max,] %>%
                plot_ly(source = "Value_filter",
                        y =  ~ reorder(Column, Frequency)) %>%
                add_bars(
                    x = ~ Frequency,
                    color = I("#0091d5"),
                    text = ~ Frequency,
                    textposition = "outside",
                    width = 0.6,
                    cliponaxis = FALSE,
                    textfont = list(color = "#000000", size = 11)
                ) %>%
                layout(
                    uniformtext = list(minsize = 7, mode = "show"),
                    xaxis  = list(
                        title = "Volume",
                        titlefont = list(size = 13),
                        tickfont = list(size = 10)
                    ),
                    yaxis  = list(
                        title = input$column1,
                        titlefont = list(size = 13),
                        tickfont = list(size = 10)
                    ),
                    title = list(
                        text = sprintf("Top Volume by %s", input$column1),
                        size = 8
                    )
                ) %>%
                plotly::config(displaylogo = FALSE,
                       modeBarButtons = list(list("toImage"), list("zoom2d"),
                                             list("resetScale2d")))
        } else {
            rv$reqDataReceive1 %>%
                count(get(input$column1), ordered_year_label) %>%
                setNames(c("Column", "Year", "Frequency")) %>%
                filter(Column %in% c(rv$volumedf[rv$min:rv$max,] %>%
                                         pull(Column))) %>%
                arrange(desc(Frequency)) %>%
                droplevels() %>%
                plot_ly(source = "Value_filter",
                        y =  ~ reorder(Column, Frequency)) %>%
                add_bars(
                    x = ~ Frequency,
                    color = ~ Year,
                    colors = c("#A3EF86", "#44DFA7", "#0992C9"),
                    text = ~ Frequency,
                    textposition = "outside",
                    cliponaxis = FALSE,
                    textfont = list(color = "#000000", size = 8)
                ) %>%
                layout(
                    legend = list(
                        traceorder = "reversed",
                        font = list(size = 10),
                        yanchor = "center", # use center of legend as anchor
                        x = 1,
                        y = 0.5
                    ),
                    uniformtext = list(minsize = 7, mode = "show"),
                    xaxis  = list(
                        title = "Volume",
                        tickfont = list(size = 10)
                    ),
                    yaxis  = list(title = input$column1, tickfont = list(size = 10)),
                    title = list(
                        text = sprintf("Top Volume by %s", input$column1),
                        size = 8
                    )
                ) %>%
                plotly::config(displaylogo = FALSE,
                       modeBarButtons = list(list("toImage"), list("zoom2d"),
                                             list("resetScale2d")))
            
        }
        
    })
    
    # get data button for first volume graph
    output$data_button <- renderUI({
        
        if (length(Value_filter()))  {
            actionButton(inputId = "Get_item_data",
                         label = "Data for Selected Variable")
        }
        
    })
    
    observeEvent(input$Get_item_data, {
        
        showModal(
            modalDialog(
                size = "l",
                easyClose = TRUE,
                radioButtons(
                    inputId = "radio1",
                    label = "Variable Selection Type:",
                    choices = list("All", "Manual Select"),
                    selected = "All"
                ),
                
                conditionalPanel(
                    condition = "input.radio1 == 'Manual Select'",
                    
                    checkboxGroupInput(
                        'show_vars',
                        'Columns to show:',
                        choices = c(colnames(rv$reqDataReceive1)),
                        selected = c(colnames(rv$reqDataReceive1)),
                        inline = T
                    )
                ),
                
                DT::dataTableOutput("Dt_item")
            )
        )
    })
    
    output$Dt_item <- DT::renderDataTable(server = FALSE,{
        req(rv$reqDataReceive1)
        
        if (is.null(Value_filter()))
            return(NULL)
        
        datatable(
            rv$reqDataReceive1 %>%
                filter(get(input$column1) %in% Value_filter()) %>%
                select(!!!input$show_vars),
            extensions = 'Buttons',
            options = list(
                dom = 'Bfrtip',
                extensions = 'Scroller',
                scrollY = 200,
                scrollX = 200,
                scroller = TRUE,
                buttons = c('csv', 'excel')
            )
        )
    })
    
    ### Geospatial Volume
    observe({
        req(rv$geodata)
        req(input$dateSelect1)
        
        rv$geo_Volume <- rv$geodata %>%
            filter(ordered_year_label %in% c(input$dateSelect1)) %>% 
            #count(get(input$column1), ordered_year_label) %>%
            count(Abbr, state)
    })
    
    
    output$Geo_Volume_Plot <- renderPlotly({
        
        req(rv$geo_Volume)
        req(input$dateSelect1)
        
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa')
            #showlakes = TRUE,
            #lakecolor = toRGB('white')
        )
        
        plot_geo(rv$geo_Volume, locationmode = 'USA-states')%>% 
            add_trace(
                z = ~n, text = ~state, locations = ~Abbr,
                color = ~n, 
                colors = viridis(50, alpha = 1, begin = 0, end = 1, direction = 1)
                # colorscale="Viridis"
                # colors = 'Purples'
            ) %>% 
            colorbar(title = "Volume") %>% 
            layout(title = 'Volume by State',
                   geo = g
            )%>%
            plotly::config(displaylogo = FALSE,
                   modeBarButtons = list(list("toImage"), list("zoom2d"),
                                         list("resetScale2d")))
        
        
        
    })
   
    # Download Map Data ----- 
    output$dwdReceived <- downloadHandler(
        filename = function() {
            "geo_visualization.csv"
        },
        content = function(file) {
            write.csv(rv$geo_Volume, file, row.names = FALSE)
        }
    ) 
    
    # Download Selected Received Data ----- 
    output$dwdvolume <- downloadHandler(
        filename = function() {
            "bars_data.csv"
        },
        content = function(file) {
            write.csv(rv$volumedf, file, row.names = FALSE)
        }
    )
    
    dInput = reactive({
        in.file = input$file1
        
        if (is.null(in.file))
            return(NULL)
        
        data <- read_excel(in.file$datapath) %>% clean_names()
        
        data <- data %>%
            mutate(ordered_date  = as.Date(format(order_date,format = "%Y-%m-%d")),
                   shipped_date = as.Date(format(ship_date,format = "%Y-%m-%d")))
        
        data <- data %>% 
            mutate(
                ordered_year_label = factor(format(ordered_date,"%Y"),
                                            levels = c(2016,2017,2018,2019)),
                shipped_year_label = factor(format(shipped_date,"%Y"),
                                            levels = c(2016,2017,2018,2019))
            )
        
        data <- data %>% 
            filter(ordered_year_label %in% c(2018,2019))
        
    })
    
    output$choose_columns <- renderUI({
        # If missing input, return to avoid error later in function
        d.input = dInput()
        if(is.null(d.input))
            return()
    
        pickerInput(
            inputId = "columns",
            label = "Cluster By :",
            choices = c("Sales" = "sales",
                        "Quantity" = "quantity",
                        "Discount" = "discount",
                        "Profit" = "profit"),
            multiple = TRUE,
            selected = c("sales","profit"),
            inline = FALSE,
            options = pickerOptions(actionsBox = TRUE, selectedTextFormat = 'values',
                                    tickIcon = "glyphicon-ok",showIcon = TRUE,
                                    showContent = TRUE, liveSearch = TRUE, showTick = TRUE,size = "auto",
                                    deselectAllText = "Deselect All"),
            choicesOpt = list(
                style = rep(("color: black; background: lightgrey; font-weight: bold; font-size: 10px;"),4)
            ))
            
    })
    
    # Target variable selection
    output$choose_target <- renderUI({
        # If missing input, return to avoid error later in function
        d.input = dInput()
        if(is.null(d.input))
            return()
        
        selectInput("target", "Choose Target Attribute", 
                    choices  = c("Sales" = "sales",
                                 "Profit" = "profit"))
    })
    
    output$choose_cluster <- renderUI({
        # If missing input, return to avoid error later in function
        d.input = dInput()
        if(is.null(d.input))
            return()
        
        numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
        
    })
    
    output$Data_head1 <- renderText({
        d.input = dInput()
        if (!is.null(d.input))
            HTML(paste0("<b> Cluster Means </b>"))
        
    })
    
    
    output$Data_head2 <- renderText({
        d.input = dInput()
        if (!is.null(d.input))
            HTML(paste0("<b> Prediction Table </b>"))
        
    })
    
    
    selectedData <- reactive({
        req(input$file1)
        
        d.input = dInput()
        dat_cluster <- d.input
        dat_cluster <- dat_cluster[, input$columns, drop = FALSE]
        for (x in 1:length(dat_cluster))      # Transform the variables
        { 
            if(!is.numeric(dat_cluster[,x])) {
                column <- paste(colnames(dat_cluster)[x],"transformed",sep="_")
                temp_data <- as.numeric(unlist(dat_cluster[,x]))
                temp_mat <- matrix(c(temp_data),ncol=1,byrow=TRUE)
                colnames(temp_mat) <- column
                data_clust_trans <- cbind(dat_cluster,temp_mat)
            }
            
        }
        nums <- sapply(dat_cluster, is.numeric)  
        dat_cluster_numeric <- dat_cluster[,nums]  #Considered only numeric values for K-Means
        dat_cluster_numeric[is.na(dat_cluster_numeric)]=FALSE
        
        dat_cluster_numeric
        
    })	
    
    clusters <- reactive({
        
        kmeans(selectedData(), input$clusters)
    })
    
    output$kmeans_plot <- renderPlot({
        
        req(input$file1)
        
        clusplot(selectedData(), clusters()$cluster, color=clusters()$cluster, labels=clusters()$cluster, cex=1.0, shade=TRUE,lines=0,main = "K-means Clustering")
    })
    
    output$clust_table <- renderTable({
        clust_table <- cbind(selectedData(), by=list(cluster=clusters()$cluster))
        clust_table
    })
    
    output$agg_table <- renderTable({
        agg_table <-  aggregate(selectedData(), by=list(cluster=clusters()$cluster),mean)
        agg_table
    })
    
    # Decision Tree #Function that calculates the output sent to the main panel in ui.R
    output$prediction = renderTable({
        req(input$file1)
        req(input$target)
        
        d.input = dInput()
        # If missing input, return to avoid error later in function
        if(is.null(d.input))
            return()
        
        # Get the data set
        dat <- d.input
        
        target <- input$target
        # print("target", target)
        dat <- dat[, input$columns, drop = FALSE]
        apply.pred_amt(dat,target)
        
    })
    
    apply.pred_amt <- function(df,v_target) {
        
        data_set <- df
        attach(data_set)
        
        target <- v_target 
        nobs <- nrow(data_set)
        form <- formula(paste(target, "~ ."))
        
        #Dataset Division
        form_column <- eval(parse(text=paste("data_set",target,sep="$")))
        new_data_set <- subset(data_set, is.na(form_column))
        nobs <- nrow(data_set) # 20000 observations 
        train <- sample(nobs, 0.6*nobs) # 14000 observations
        trainset <- data_set[train, ]
        validate <- sample(setdiff(seq_len(nobs), train), 0.15*nobs) # 3000 observations
        validateset <- data_set[validate, ]
        test <- setdiff(setdiff(seq_len(nobs), train), validate) # 3000 observations
        testset <- data_set[test,]
        
        motorVars <- setdiff(colnames(data_set),list(target))#,ignore))
        dsFormula <- as.formula(paste(target,paste(motorVars,collapse=' + '),sep=" ~ "))
        model <- rpart(dsFormula, data=trainset)
        model
        model$variable.importance
        summary(model)
        cp <- printcp(model)  #Complexity Parameter
        predicted <- predict(model, newdata=testset)
        # Extract the relevant variables from the dataset.
        pred_loc <- ncol(data_set)
        sdata <- subset(data_set[test,]) 
        
        variance <-  round((predicted - sdata[,pred_loc])/sdata[,pred_loc] * 100,2)
        variance_value <- (predicted - sdata[,pred_loc])
        res <- cbind(sdata, predicted,variance_value,variance)
        # New data prediction
        predicted_new <- predict(model, newdata=new_data_set)
        res_new <- cbind(predicted_new,new_data_set)
        names(res_new)[1]<-paste(target, "_predicted")
        res_new
        
        
    }
    
    
    
}