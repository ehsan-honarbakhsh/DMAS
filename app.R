library(shinydashboard)
library(DT)
library(shiny)
library(readxl)
library(caret)
library(ggplot2)
library(graphics)
library(Cubist)
library(frbs)
library(ipred)
library(e1071)
library(reshape2)
library(MLmetrics)
library(rsq)
library(rsconnect)
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "DMAS",dropdownMenu(type = "messages",
                                              messageItem(
                                                from = "Admin",
                                                message = "Hello wellcome to software"
                                              ),
                                              messageItem(
                                                from = "Support",
                                                message = "The new server is ready.",
                                                icon = icon("life-ring"),
                                                time = "2014-12-01"
                                              )
  ),dropdownMenu(type = "notifications",
                 notificationItem(
                   text = "5 new item",
                   icon("users")
                 ),
                 notificationItem(
                   text = "12 items delivered",
                   icon("truck"),
                   status = "success"
                 ),
                 notificationItem(
                   text = "Server load at 86%",
                   icon = icon("exclamation-triangle"),
                   status = "warning"
                 )
  )),
  dashboardSidebar(width = 300,
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data",menuSubItem("Calibration and testing data",tabName = "cal"),tabName = "data", icon = icon("folder-open")),
      menuItem("Feature Dependency",menuSubItem("Pearson Correlation",tabName = "pear"), tabName = "depend", icon = icon("th")),
      menuItem("Feature Ranking",menuSubItem("Cubist",tabName = "varimp"),menuSubItem("Bagging",tabName = "varimp2"), tabName = "rank", icon = icon("th")),
      menuItem("Prediction",menuSubItem("Wang and Mendel’s technique (WM)",tabName = "HGI"),menuSubItem("Cubist regression tree",tabName = "cub"), tabName = "depend", icon = icon("th")),
      menuItem("Model Evaluation",menuSubItem("MAPE & R Squared ",tabName = "map"),menuSubItem("Result Evaluation of Calibration data",tabName = "res"), tabName = "ss", icon = icon("th")),
      menuItem("Prediction New Data",menuSubItem("Insert Data",tabName = "data2"),menuSubItem("Prediction",tabName = "pred"), tabName = "ss", icon = icon("th")),
      menuItem("Data Visualization",menuSubItem("Scatterplot Matrix ",tabName = "scater"), tabName = "ss", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              h2("Wellcome to Data Mining Analysis System (DMAS) ")
      ),
      tabItem(tabName = "cal",
              fluidRow(
                box(solidHeader = TRUE,status = "danger",fileInput("file", "A data set to calibrate and test model(Insert Excel File)",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv",
                                ".xlsx")
                ),width = 300)
              ),
              
              fluidRow(
                column(6,
                       
                       sliderInput("slider1", label = h3("Training Data Percentage"), min = 0, 
                                   max = 100, value = 70)
                ),
                column(6,
                       
                       sliderInput("slider1", label = h3("Testing Data Percentage"), min = 0, 
                                   max = 100, value = 30)
                )
              
              ),
              fluidRow(
                box(
                  title = "Dataset",
                  div(style = 'overflow-x: scroll', DT::dataTableOutput("mytable")),width = 400
                )
              )
            
      ),
      tabItem(tabName = "pear",
              fluidRow(
                box(title="Pearson Correlation",solidHeader = TRUE,status = "danger", plotOutput("pearson",height = 500),height = 600,width = 500)
              )
      ),
      tabItem(tabName = "varimp",
              fluidRow(
                box(title="Cubist",solidHeader = TRUE,status = "danger",div(style = 'overflow-x: scroll', DT::dataTableOutput("varimp")),width = 400)
                ),
             
              fluidRow(
                     column(
                       width = 10,wellPanel(
                         radioButtons("method1", "Would you like to use Cubist variables to train the prediction model?(Please note that you can select only one between Cubist and Bagging variables at each time!)",selected = "no",
                                      c("yes", "no")))
                         )
                    
              ),
              fluidRow(
                
                box(title="Select the number  of important variables which use to train prediction model",solidHeader = TRUE,status = "danger",sliderInput("slider3", label = h3("Number of variables"), min = 0, 
                                                                                                                                                                        max = 24, value = 4,width = 400))
              ),
              fluidRow( 
                     column(width = 12,
                       box(title="Cubist Variables Ranking BarPlot",solidHeader = TRUE,status = "danger", plotOutput("plot6",width = 600),width = 600)
                            ),
                      column(width = 10,
                       box(title="Cubist Dotplot",solidHeader = TRUE,status = "danger",plotOutput("plot",width = 500),width = 500)
                            )
                       )
 
              
      ),
      tabItem(tabName = "varimp2",
              fluidRow(
                box(title = "Bagging" ,solidHeader = TRUE,status = "danger",div(style = 'overflow-x: scroll', DT::dataTableOutput("summaryBagging")),width = 500)
              ),
              fluidRow(
                column(
                  width = 10,wellPanel(
                    radioButtons("method2", "Would you like to use Bagging to train the prediction model?(Please note that you can select only one between Cubist and Bagging variables at each time!)",selected = "no",
                                 c("yes", "no")))
                )
                
              ),
              fluidRow(
                
                box(title="Select the number of important variables which use to train prediction model",solidHeader = TRUE,status = "danger",sliderInput("slider4", label = h3("Number of variables"), min = 0, 
                                                                                                                                                                        max = 24, value = 4,width = 400))
              ),
              fluidRow( 
                box(title="Bagging Variables Ranking Barplot",solidHeader = TRUE,status = "danger",plotOutput("plot7",width = 500),width = 300)
              )
              
      ),
      
      tabItem(tabName = "HGI",
              fluidRow(
                box(title="Summary of  Wang and Mendel’s technique (WM)",solidHeader = TRUE,status = "danger",verbatimTextOutput("learn"),width = 500)
              ),
             
             
              fluidRow(
                
                box(title="PlotMF For Wang and Mendel’s technique ",solidHeader = TRUE,status = "danger",plotOutput("plot3",width = 400),width =300)
                
              )
          
      ),
      tabItem(tabName = "cub",
              fluidRow(
                box(title = "Cubist regression tree" ,solidHeader = TRUE,status = "danger",div(style = 'overflow-x: scroll',verbatimTextOutput("summary")),width = 500)
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                box(title = "Model Evaluation" ,solidHeader = TRUE,status = "danger",DT::dataTableOutput("num"),width = 300)
              )
        
      ),
      tabItem(tabName = "res",
              fluidRow(
                box(title = "Result Evaluation" ,solidHeader = TRUE,status = "danger",DT::dataTableOutput("result"),width = 300)
              )
              
      ),
      tabItem(tabName = "data2",
              
              fluidRow(
                box(solidHeader = TRUE,status = "danger",fileInput("file2", "A data set for prediction (Insert Excel File)",
                                                                   accept = c(
                                                                     "text/csv",
                                                                     "text/comma-separated-values,text/plain",
                                                                     ".csv",
                                                                     ".xlsx")
                ),width = 300)
                
              ),
              fluidRow(
                box(
                  title = "Dataset",
                  div(style = 'overflow-x: scroll', DT::dataTableOutput("mytable2")),width = 400
                )
              )
      ),
      tabItem(tabName = "pred" ,       
              fluidRow(
                box(title = "Prediction of Wang and Mendel’s technique (WM)" ,solidHeader = TRUE,status = "danger",div(style = 'overflow-x: scroll', DT::dataTableOutput("HGI")),width = 500)
              )
      ),
      tabItem(tabName = "scater",
              fluidRow(
                box(title="Scatterplot",solidHeader = TRUE,status = "danger",plotOutput("plot2",width = 1200,height = 1200),width =1200)
              
              )
      )
     
      
      
      
    ))
)

server <- function(input, output)
{
  output$mytable = DT::renderDataTable({
    req(input$file)
    f <- read_excel(input$file$datapath)
    f
  })
  output$mytable2 = DT::renderDataTable({
    req(input$file2)
    o <- read_excel(input$file2$datapath)
    o
  })
  output$pearson=renderPlot({
    req(input$file)
    f <- read_excel(input$file$datapath)
    o=as.matrix(f)
    cormat <- round(cor(f[,1:as.numeric(ncol(o))]),2)
    # Get lower triangle of the correlation matrix
    get_lower_tri<-function(cormat){
      cormat[upper.tri(cormat)] <- NA
      return(cormat)
    }
    # Get upper triangle of the correlation matrix
    get_upper_tri <- function(cormat){
      cormat[lower.tri(cormat)]<- NA
      return(cormat)
    }
    upper_tri <- get_upper_tri(cormat)
    upper_tri
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
    
    ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1))+
      coord_fixed()
    # Print the heatmap
    print(ggheatmap)
    ggheatmap + 
      geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                   title.position = "top", title.hjust = 0.5))
    
    
  }
  )
  
  output$varimp= DT::renderDataTable({
    req(input$file)
    f <- read_excel(input$file$datapath)
    s=as.matrix(f)
    mdl=cubist(x=f[,1:as.numeric(ncol(s))-1],y=s[,colnames(s)[ncol(s)]],cubistControl(rules = 10,extrapolation = 5),committees = 1)
    o=varImp(mdl)
    #n=sort(o[,1],decreasing = TRUE)
    i=data.frame(t(o))
    l=sort(i,decreasing = TRUE)
    l
    
  })
  output$plot6=renderPlot({
    req(input$file)
    f <- read_excel(input$file$datapath)
    s=as.matrix(f)
    mdl=cubist(x=f[,1:as.numeric(ncol(s))-1],y=s[,colnames(s)[ncol(s)]],cubistControl(rules = 10,extrapolation = 5),committees = 1)
    q=varImp(mdl)
    r=as.data.frame(t(q))
    j=sort(r,decreasing = TRUE )
    h=as.matrix(j)
    barplot(h[1,1:ncol(s)-1], main="Variable Importance Cubist", horiz=FALSE,
          names.arg=colnames(j))

  }
  )
  output$summaryBagging = DT::renderDataTable({
    req(input$file)
    f <- read_excel(input$file$datapath)
    s=as.matrix(f)
    m=bagging(s[,colnames(s)[ncol(s)]]~.,data=f[,colnames(s)[1:ncol(s)-1]])
    x=varImp(m)
    i=data.frame(t(x))
    l=sort(i,decreasing = TRUE)
    l
  })
  output$plot7=renderPlot({
    req(input$file)
    f <- read_excel(input$file$datapath)
    s=as.matrix(f)
    m=bagging(s[,colnames(s)[ncol(s)]]~.,data=f[,colnames(s)[1:ncol(s)-1]])
    q=varImp(m)
    r=as.data.frame(t(q))
    j=sort(r,decreasing = TRUE )
    h=as.matrix(j)
    
    barplot(h[1,1:ncol(s)-1], main="Variable Importance Bagging", horiz=FALSE,
            names.arg=colnames(j))
  }
  )
  output$HGI= DT::renderDataTable({
    if(input$method1=="yes")
    {
    req(input$file)
    f <- read_excel(input$file$datapath)
    req(input$file2)
    o <- read_excel(input$file2$datapath)
    s=as.matrix(f)
    n=as.matrix(o)
    mdl=cubist(x=f[,1:as.numeric(ncol(s))-1],y=s[,colnames(s)[ncol(s)]],cubistControl(rules = 10,extrapolation = 5),committees = 1)
    e2=varImp(mdl)
    i=data.frame(t(e2))
    l=sort(i,decreasing = TRUE)
    e=l[,1:input$slider3]
    t=f[,c(colnames(e),colnames(s)[ncol(s)])]
    y=o[,c(colnames(e))]
    p=as.matrix(t)
    z=as.matrix(y)
    w=nrow(p)
    h=round(w*(input$slider1/100))
    trianp=p[1:h,]
    testp=z[1:nrow(n),]
    object.frbcs.w <- frbs.learn(trianp, range.data=NULL, method.type ="WM", control = list(num.labels =11,type.mf = "TRIANGLE"))
    pred <- predict(object.frbcs.w,testp)
    qq=as.data.frame(pred)
    qq
    }
    else if (input$method2=="yes")
    {
      req(input$file)
      f <- read_excel(input$file$datapath)
      req(input$file2)
      o <- read_excel(input$file2$datapath) 
      s=as.matrix(f)
      n=as.matrix(o)
      m=bagging(s[,colnames(s)[ncol(s)]]~.,data=f[,colnames(s)[1:ncol(s)-1]])
      x=varImp(m)
      i=data.frame(t(x))
      l=sort(i,decreasing = TRUE)
      e=l[,1:input$slider4]
      t=f[,c(colnames(e),colnames(s)[ncol(s)])]
      y=o[,c(colnames(e))]
      p=as.matrix(t)
      z=as.matrix(y)
      w=nrow(p)
      h=round(w*(input$slider1/100))
      trianp=p[1:h,]
      testp=z[1:nrow(n),]
      object.frbcs.w <- frbs.learn(trianp, range.data=NULL, method.type ="WM", control = list(num.labels =11,type.mf = "TRIANGLE"))
      pred <- predict(object.frbcs.w,testp)
      qq=as.data.frame(pred)
      qq
    }
  })
  
  output$plot2=renderPlot({
    req(input$file)
    f <- read_excel(input$file$datapath)
    s=as.matrix(f)
    panel.cor <- function(x, y, digits=2, prefix="",cex.cor, ...)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- abs(cor(x, y))
      txt <- format(c(r, 0.123456789), digits=digits)[1]
      txt <- paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex.cor * r)
    }
    pairs(~., data=s,
          lower.panel=panel.smooth, upper.panel=panel.cor, 
          pch=20, main="Scatterplot Matrix")
    
  }
  )
  output$plot=renderPlot({
    req(input$file)
    f <- read_excel(input$file$datapath)
    s=as.matrix(f)
    mdl=cubist(x=f[,1:as.numeric(ncol(s))-1],y=s[,colnames(s)[ncol(s)]],cubistControl(rules = 10,extrapolation = 5),committees = 1)
    dotplot(mdl, data = NULL, what = "splits",committee = NULL, rule = NULL)
  }
  )
  output$plot3=renderPlot({
    if(input$method1=="yes")
    {
    req(input$file)
    f <- read_excel(input$file$datapath)
    s=as.matrix(f)
    mdl=cubist(x=f[,1:as.numeric(ncol(s))-1],y=s[,colnames(s)[ncol(s)]],cubistControl(rules = 10,extrapolation = 5),committees = 1)
    o=varImp(mdl)
    #n=sort(o[,1],decreasing = TRUE)
    i=data.frame(t(o))
    l=sort(i,decreasing = TRUE)
    e=l[,1:input$slider3]
    t=f[,c(colnames(e),colnames(s)[ncol(s)])]
    p=as.matrix(t)
    #i=as.matrix(o)
    w=nrow(p)
    h=round(w*(input$slider1/100))
    #q=round(w*(input$slider2/100))
    trianp=p[1:h,]
    object.frbcs.w <- frbs.learn(trianp, range.data=NULL, method.type ="WM", control = list(num.labels =11,type.mf = "TRIANGLE"))
    plotMF(object.frbcs.w)
    }
    else if(input$method2=="yes")
    {
      req(input$file)
      f <- read_excel(input$file$datapath) 
      s=as.matrix(f)
      m=bagging(s[,colnames(s)[ncol(s)]]~.,data=f[,colnames(s)[1:ncol(s)-1]])
      x=varImp(m)
      i=data.frame(t(x))
      l=sort(i,decreasing = TRUE)
      e=l[,1:input$slider4]
      t=f[,c(colnames(e),colnames(s)[ncol(s)])]
     # y=o[,c(colnames(e))]
      p=as.matrix(t)
      #z=as.matrix(y)
      w=nrow(p)
      h=round(w*(input$slider1/100))
      trianp=p[1:h,]
      object.frbcs.w <- frbs.learn(trianp, range.data=NULL, method.type ="WM", control = list(num.labels =11,type.mf = "TRIANGLE"))
     plotMF(object.frbcs.w)
      
    }
  }
  )
  output$summary=renderPrint({
    req(input$file)
    f <- read_excel(input$file$datapath)
    s=as.matrix(f)
    mdl=cubist(x=f[,1:as.numeric(ncol(s))-1],y=s[,colnames(s)[ncol(s)]],cubistControl(rules = 10,extrapolation = 5),committees = 1)
    summary(mdl)
  }
  )
  output$num=DT::renderDataTable({
      req(input$file)
      f <- read_excel(input$file$datapath)
      s=as.matrix(f)
      mdl=cubist(x=f[,1:as.numeric(ncol(s))-1],y=s[,colnames(s)[ncol(s)]],cubistControl(rules = 10,extrapolation = 5),committees = 1)
      e2=varImp(mdl)
      i=data.frame(t(e2))
      l=sort(i,decreasing = TRUE)
      e=l[,1:input$slider3]
      t=f[,c(colnames(e),colnames(s)[ncol(s)])]
      y=f[,c(colnames(e))]
      p=as.matrix(t)
      z=as.matrix(y)
      w=nrow(p)
      h=round(w*(input$slider1/100))
      trianp=p[1:h,]
      testp=z[h:nrow(s),]
      object.frbcs.w <- frbs.learn(trianp, range.data=NULL, method.type ="WM", control = list(num.labels =11,type.mf = "TRIANGLE"))
      pred <- predict(object.frbcs.w,testp)
      vv=s[h:nrow(s),colnames(s)[ncol(s)]]
      rrr=as.data.frame(t(vv))
      ppp=as.data.frame(t(pred))
      maptest=MAPE(as.matrix(ppp),as.matrix(rrr))
      trainp2=p[1:h,c(colnames(e))]
      trainp22=as.matrix(trainp2)
      object.frbcs.w2 <- frbs.learn(trianp, range.data=NULL, method.type ="WM", control = list(num.labels =11,type.mf = "TRIANGLE"))
      pred2 <- predict(object.frbcs.w,trainp22)
      vvv=s[1:h,colnames(s)[ncol(s)]]
      rrrr=as.data.frame(t(vvv))
      pppp=as.data.frame(t(pred2))
      maptrain=MAPE(as.matrix(pppp),as.matrix(rrrr))
      xtrain=lm(s[,colnames(s)[ncol(s)]]~.,data=f[,colnames(s)[1:ncol(s)-1]])
     ytrain= summary(xtrain)$r.squared 
     xtest=lm(s[,colnames(s)[ncol(s)]]~.,data=f[,colnames(s)[1:ncol(s)-1]])
     ytest= summary(xtest)$r.squared 
     data.frame(Mape=c(maptrain,maptest),Rsquared=c(ytrain,ytest),row.names = c("Train","Test"))
      
  })
  output$learn= renderPrint({
    req(input$file)
    f <- read_excel(input$file$datapath)
   # req(input$file2)
    #o <- read_excel(input$file2$datapath)
    s=as.matrix(f)
    #n=as.matrix(o)
    mdl=cubist(x=f[,1:as.numeric(ncol(s))-1],y=s[,colnames(s)[ncol(s)]],cubistControl(rules = 10,extrapolation = 5),committees = 1)
    e2=varImp(mdl)
    i=data.frame(t(e2))
    l=sort(i,decreasing = TRUE)
    e=l[,1:input$slider3]
    t=f[,c(colnames(e),colnames(s)[ncol(s)])]
    #y=o[,c(colnames(e))]
    p=as.matrix(t)
    #z=as.matrix(y)
    w=nrow(p)
    h=round(w*(input$slider1/100))
    trianp=p[1:h,]
    #testp=z[1:nrow(n),]
    object.frbcs.w <- frbs.learn(trianp, range.data=NULL, method.type ="WM", control = list(num.labels =11,type.mf = "TRIANGLE"))
    object.frbcs.w
    
  })
  output$result= DT::renderDataTable({
    if(input$method1=="yes")
    {
      req(input$file)
      f <- read_excel(input$file$datapath)
      s=as.matrix(f)
      mdl=cubist(x=f[,1:as.numeric(ncol(s))-1],y=s[,colnames(s)[ncol(s)]],cubistControl(rules = 10,extrapolation = 5),committees = 1)
      e2=varImp(mdl)
      i=data.frame(t(e2))
      l=sort(i,decreasing = TRUE)
      e=l[,1:input$slider3]
      t=f[,c(colnames(e),colnames(s)[ncol(s)])]
      y=f[,c(colnames(e))]
      p=as.matrix(t)
      z=as.matrix(y)
      w=nrow(p)
      h=round(w*(input$slider1/100))
      trianp=p[1:h,]
      testp=z[h:nrow(s),]
      testp2=t[h:nrow(s),colnames(s)[ncol(s)]]
      object.frbcs.w <- frbs.learn(trianp, range.data=NULL, method.type ="WM", control = list(num.labels =11,type.mf = "TRIANGLE"))
      pred <- predict(object.frbcs.w,testp)
      data.frame(PREDICTED=pred,ACTUAL=testp2)
      
    }
    else if (input$method2=="yes")
    {
      req(input$file)
      f <- read_excel(input$file$datapath)
      s=as.matrix(f)
      m=bagging(s[,colnames(s)[ncol(s)]]~.,data=f[,colnames(s)[1:ncol(s)-1]])
      x=varImp(m)
      i=data.frame(t(x))
      l=sort(i,decreasing = TRUE)
      e=l[,1:input$slider4]
      t=f[,c(colnames(e),colnames(s)[ncol(s)])]
      y=o[,c(colnames(e))]
      p=as.matrix(t)
      z=as.matrix(y)
      w=nrow(p)
      h=round(w*(input$slider1/100))
      trianp=p[1:h,]
      testp=z[h:nrow(s),]
      testp2=t[h:nrow(s),colnames(s)[ncol(s)]]
      object.frbcs.w <- frbs.learn(trianp, range.data=NULL, method.type ="WM", control = list(num.labels =11,type.mf = "TRIANGLE"))
      pred <- predict(object.frbcs.w,testp)
      data.frame(PREDICTED=c(pred),ACTUAL=(testp2))
    }
  })

}

shinyApp(ui, server)