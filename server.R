library(shiny)
source("routin_version_comparison.R")
# #options(shiny.trace=TRUE)
# # Define server logic required to draw a histogram
 C=getCompanies()
 C=as.character(C[-which(C=="")])
 sc=C[1]
# shinyServer(function(input, output) {
#   
#  
#  C=as.character(C[-which(C=="")])
  print(C)
#  print(class(C))
# 
#  
#  output$choose_dataset <- renderUI({
#    selectInput("dataset", "Data set", as.list(as.character(C)))
#  })


shinyServer(function(input, output) {

 # comp= as.list(as.character(C))

    # Drop-down selection box for which data set
  output$choose_dataset <- renderUI({
    comp=getCompanies()
    comp=as.character(comp[-which(comp=="")])
    sc=comp[1]
    print("try")
    print(comp)
    selectInput("company", "Select company",comp, selected=sc)
  })
  
  
  
  output$input_type_text <- renderText({
    input$selCmp
    isolate(
    paste("The selected company",input$company)
    )
  })

#   company <- reactive({
#     if(input$selCmp == 0)
#     {
#       return()
#     }
#     isolate({
#     input$selCmp
#     print(paste("Cmp",input$choose_dataset))
#       return(input$choose_dataset)
#     })
# 
#   })
#   print("PPP")
#   print(company)
#  if(input$selCmp !=0)  print(input$company)
  output$text1 <- renderText({ 
    input$goButton
    isolate(
    paste("The selected initial version", input$vers1)
    )
  })
output$text2 <- renderText({ 
  input$goButton
  isolate(
    paste("The selected initial version", input$vers2)
  )
})
  output$vers1 <- renderText({ 
    input$selCmp
    input$goButton
    isolate(
    paste("The selected stage version", input$vers1))
  })
output$vers2 <- renderText({ 
  input$selCmp
  input$goButton
  isolate(
    paste("The selected stage version", input$vers2))
})
output$env1 <- renderText({ 
  input$selCmp
  input$goButton
  isolate(
    paste("The selected init environment", input$env1))
})
output$env2 <- renderText({ 
  input$selCmp
  input$goButton
  isolate(
    paste("The selected stage environment", input$env2))
})
  output$text3 <- renderText({ 
    input$selCmp
    input$goButton
    isolate(
    paste("The selected company", input$company))
  })
  output$text4 <- renderText({ 
      paste("Results assessment")
  })
  output$dates <- renderText({ 
    input$selCmp
    input$goButton
    isolate(
      paste("The selected dates", input$dates))
  })
  output$comp <- renderText(paste("Select company"))
  
 # output$comp.names <- getCompanies()
  
  
  datasetInput1 <- reactive({
    input$selCmp
    switch(input$dataset1,
           "2.0.7" = v1,
           "2.1.1" = v2,
           "2.1.0" = v3)
  })


  
  datasetInput2 <- reactive({
    input$selCmp
    switch(input$dataset2,
           "2.0.7" = v1,
           "2.1.1" = v2,
           "2.1.0" = v3)
  })
  
  output$caption<-renderText({
    switch(input$plot.type,
           "Assigned_activities"   =   "Assigned_activities",
           "Overdue_time" =	"Overdue_time",
           "Travel_time" 	=	"Travel_time",
           "SLA_violation" 		=	"SLA_violation",
           "Percent_of_pure_revenue"="Percent_of_pure_revenue",
           "Work_time" = "Work_time", 
           "Rout_density"="Rout_density",
           "Overtime"="Overtime",
           "Revenue"="Revenue")
  })

  plot.type<-reactive({
    input$selCmp
    input$goButton1
    isolate(
    switch(input$plot.type,
                    "Assigned_activities"   = 	"Assigned_activities",
                    "Overdue_time" =	 "Overdue_time",
                    "Travel_time" 	=	"Travel_time",
                    "SLA_violation" 		=	"SLA_violation",
           "Percent_of_pure_revenue"="Percent_of_pure_revenue",
           "Work_time" = "Work_time", 
           "Rout_density"="Rout_density",
           "Overtime"="Overtime",
           "Revenue"="Revenue"))
  })
  
 Dates <- reactive({
   input$selCmp
    input$goButton
    isolate(input$dates)
  })
 
 Dates2 <- reactive({
   input$selCmp
   input$goButton
   isolate(input$dates2)
 })
  
# C=reactive({ getCompanies()})
  #output$value <- renderPrint({ input$cmp })
  
#  R <- reactive({ R=getALLRes1(input$dataset1, input$dataset2, cmp=input$cpm)})

D <-  reactive({
  if(input$selCmp==0) return()
  else {D=getDBD(cmp=as.character(input$company ))
        print("DB selection2")      
        if(nrow(D)==0)
          return ()
        else return (D)
  }
})
output$textD1 <- renderDataTable({ 
  input$selCmp
  D <- D()
  if(is.null(D))
    paste("There is no data for the company", input$company)
  else {
    print(D)
    D
  }
})
x    <-  reactive({print(input$selCmp)
                   print(input$goButton)
                   
                   if(input$selCmp==0 | input$goButton==0)return()
                   
                   # isolate(getALLRes1(input$dataset1, input$dataset2, cmp=as.character(input$cmp ), input$dates, input$dates2))
                   # isolate(getALLRes1(input$vers1, input$vers2, cmp=as.character(input$company ), input$dates, input$dates2))
                   isolate( getALLResF(input$vers1, input$vers2, cmp=as.character(input$company ), input$dates, input$dates2, input$env1, input$env2)) }) 

xp    <-  reactive({
                   if(input$expert==0)return()
                   isolate( 
getALLResF(input$vers1, input$vers2, cmp=as.character(input$company ), input$dates, input$dates2, input$env1, input$env2, rp=input$rout_plan)) }) 



cap    <-  reactive({ if(input$selCmp==0 || input$goButton==0)return()
                    isolate(getCapacity(input$vers1, input$vers2, cmp=as.character( input$company ), input$dates, input$dates2, input$env1, input$env2))}) 
#x    <-  x()
#y=data.frame(as.character(input$dataset1)=median(x[,2]), as.character(input$dataset2)=median(x[,3]))
output$fintable = renderDataTable({
 # input$selCmp
  input$goButton

#  isolate({
    x    <-  x()
    print(x)
    print("OKKKK!")
  y=data.frame(N1=sum(as.numeric(x[,2]), na.rm=TRUE),vers1=median(as.numeric(x[,3]), na.rm=TRUE),vers2=median(as.numeric(x[,4]), na.rm=TRUE), N2=sum(as.numeric(x[,5]), na.rm=TRUE))
  colnames(y)=c("N1",input$vers1, input$vers2, "N2")
  colnames(y)=c("N1",substr(colnames(y)[2],1,5), substr(colnames(y)[3], 1,5),"N2" )
  y[,2]=round(as.numeric(as.character(y[,2])),3)
  y[,3]=round(as.numeric(as.character(y[,3])),3)
  #y=cbind(y, N=nrow(x))
  print(y)
  #})
  
})

output$finPlanTable = renderDataTable({
  # input$selCmp
  input$expert
  
  #  isolate({
  xp    <-  xp()
  print(xp)
  print("OKKKK!")
  #y=data.frame(N1=sum(as.numeric(x[,2]), na.rm=TRUE),vers1=median(as.numeric(x[,3]), na.rm=TRUE),vers2=median(as.numeric(x[,4]), na.rm=TRUE), N2=sum(as.numeric(x[,5]), na.rm=TRUE))
  #colnames(y)=c("N1",input$vers1, input$vers2, "N2")
  #colnames(y)=c("N1",substr(colnames(y)[2],1,5), substr(colnames(y)[3], 1,5),"N2" )
  #y[,2]=round(as.numeric(as.character(y[,2])),3)
  #y[,3]=round(as.numeric(as.character(y[,3])),3)
  #y=cbind(y, N=nrow(x))
  print(xp)
  #})
  
})


output$choose_dataset1 <- renderUI({
  input$goButton
  isolate(
  if (input$goButton==FALSE)  
    selectInput("rout_plan", "Select Routing plan","", selected="")
  else {
      x <-x();
    print(x)
    x=as.data.frame(x)
    rp=as.character(x[,1])
    rc=rp[1]
    print(rp)
    selectInput("rout_plan", "Select Routing plan",rp, selected=rc)
  }
  )
  
})

output$figPlot <- renderPlot({
  input$selCmp
  input$goButton
   # x    <-  reactive({ x=getALLRes1(input$dataset1, input$dataset2, cmp=input$cpm)})
#  x    <-  reactive({ x=getALLRes1(input$dataset1, input$dataset2, cmp=input$cpm)}) 
  
  #reactive({ x=getALLRes1(input$dataset1, input$dataset2, cmp=input$cpm)}) 
   # print((x))
    # draw the histogram with the specified number of bins
  isolate({
    x    <-  x()
    plot(x[,2], col = 'red', t = 'l', lwd=3, ylim=c(min(x[,(2:ncol(x))]), max(x[,(2:ncol(x))])))
    for(i in 3:ncol(x)){
      lines(x[,i], col = i*4, t = 'l', lwd=3)
    }
    legn=c(paste("Initial", input$dataset1))
    legc='red'
    for(t in 3:ncol(x)) {lines(x[,t], t='l', col=t*4, lwd=3)
                           legn=c(legn, paste("Stage", input$dataset2[t-2]))
                           legc=c(legc, t*4)
    }
    legend('topleft', legn, col=legc, cex=0.9, lty=1, lwd=3, bty='n');
    })
  })
  
  output$distPlot <- renderPlot({
    input$goButton
    # getALLRes(input$dataset1, input$dataset2, cmp=input$cpm)  # Old Faithful Geyser data
  # print(head(x))
    # draw the histogram with the specified number of bins
  isolate({
    x    <- x()
    hist(x[,2], breaks=round(nrow(x)/10, 0), xlim=c(min(x[,(2:ncol(x))]), max(x[,(2:ncol(x))])), col='red', main='Histogram of the routing quality')
    for(i in 3:ncol(x)){
      hist(x[,i], breaks = round(nrow(x)/10, 0), col = i*4, border = i*8, add=TRUE)
    }
    legn=c(paste("Initial", input$dataset1))
    legc='red'
    for(t in 3:ncol(x)) {lines(x[,t], t='l', col=t*4, lwd=3)
                         legn=c(legn, paste("Stage", input$dataset2[t-2]))
                         legc=c(legc, t*4)
    }
    legend('topleft', legn, col=legc, cex=0.9, lty=1, lwd=3, bty='n');})
  })

output$parPlot <- renderPlot({
 # input$selCmp
  input$goButton1
 # x    <- x() # getALLRes(input$dataset1, input$dataset2, cmp=input$cpm)  # Old Faithful Geyser data
  # print(head(x))
  # draw the histogram with the specified number of bins
 plot.type<-switch(input$plot.type,
                   "Assigned_activities"   = 	1,
                   "Overdue_time" =	2,
                   "Travel_time" 	=	3,
                   "SLA_violation" 		=	4,
                   "Percent_of_pure_revenue"=5,
                  "Work_time"=6, 
                  "Rout_density"=7,
                  "Overtime"=8,
                  "Revenue"=9
 )
  isolate({
    x    <-  x()
    #print(x)
    vmfd=read.csv(paste(input$company,"_FR.csv", sep=''), sep=',')
    print("Recognize")
    print(head(vmfd))
    assess=c("_AAs","_OvC","_TrC","_SlC","_AL","_WT","_RD","_Ovt", "_RV")
    assessN=c("Number of assigned activities","Overdue cost","Travel cost","SLA violation cost",
              "Summary relative cost","Work time","Rout density","Overtime", "Revenue")
    vmfdS=data.frame()
    #plot.type
    for(i in 1:length(assess)){
      if(i==1) {
        vmfdS=vmfd[,which(colnames(vmfd) %in% colnames(vmfd)[grep(assess[i], colnames(vmfd))])]
      }
      else{vmfdS=cbind(vmfdS, vmfd[,which(colnames(vmfd) %in% colnames(vmfd)[grep(assess[i], colnames(vmfd))])])}}
#     
#     for(h in 1:ncol(vmfdS)){
#       colnames(vmfdS)[h]=substr(colnames(vmfdS)[h], 1,(nchar(colnames(vmfdS)[h])-2))
#     }
   par(mfrow=c(2,2))
   # k=1
  
    vmfdSs= sapply(vmfdS, median, na.rm=TRUE)
   #print(head(vmfdS))
   #print(paste("!!!!WWWWWWWWW",plot.type))
   k=as.numeric(plot.type)*2-1
   #print(k)
   #print(head(vmfdS))
    #for(i in 1:length(assess)){
      dtes1=rbind(cbind(as.numeric(vmfdS[,k]), rep(colnames(vmfdS)[k], nrow(vmfdS))), cbind(as.numeric(vmfdS[,(k+1)]), rep(colnames(vmfdS)[(k+1)], nrow(vmfdS))))
colnames(dtes1)=c("val","vers")
dtes1=as.data.frame(dtes1)
dtes1$val=as.numeric(dtes1$val)
#print(head(dtes1))
ds1 <- ddply(dtes1, .(vers), summarise, mean = mean(val, na.rm=TRUE), median = median(val, na.rm=TRUE), sd = sd(val), sum = sum(val))
# p <- ggplot(dtes1, aes(x = vers, y = val)) +
#   geom_point() +geom_boxplot()+geom_point(color='black',alpha=0.5, position = 'jitter')+
#   geom_point(data = ds1, aes(y = median),
#              colour = 'red', size = 10)+ ggtitle(paste(assessN[as.numeric(plot.type)]))
p <- ggplot(dtes1, aes(x = vers, y = val)) +
  geom_point() +geom_boxplot()+geom_point(color='white',alpha=0.5, position = 'jitter')+
  geom_point(data = ds1, aes(y = median),
             colour = 'red', size = 10)+ ggtitle(paste(assessN[as.numeric(plot.type)]))
  print(p)
    #  print(seq(k,(k+1)))
   #   stripchart(vmfdS[,seq(k,(k+1))], vertical=TRUE, pch=19, col=c('red', 'blue'), method = "jitter", group.names=colnames(vmfdS)[seq(k,(k+1))], xlab="", ylab="", main=paste(input$cmp,assessN[i]))
    #  boxplot(vmfdS[,seq(k,(k+1))], add=TRUE)
      #   boxplot(vmfdS[,seq(k,(k+1))], col=c('red', 'blue', 'green'), main = paste(input$cmp,assessN[i]))
    
#k=k+2
 #   }
})

})

cap    <-  reactive({ if(input$selCmp==0 || input$goButton==0)return()
                      isolate({cc=getCapacity(input$vers1, input$vers2, cmp=as.character(input$company ), input$dates, input$dates2, input$env1, input$env2)
                               assess=c("_work_time_human","_work_time_machine")
                               vmfdS=data.frame()
                               #plot.type
                               for(i in 1:length(assess)){
                                 print(grep(assess[i], colnames(cc)))
                                 if(i==1) {
                                   vmfdS=cc[,which(colnames(cc) %in% colnames(cc)[grep(assess[i], colnames(cc))])]
                                   #vmfdS=cbind(vmfdS, diff=(as.numeric(as.character(vmfdS[,ncol(vmfdS)]))-as.numeric(as.character(vmfdS[,(ncol(vmfdS)-1)]))))
                                   #colnames(vmfdS)[ncol(vmfdS)]=paste("Diff_", assess[i], sep='')
                                 }
                                 else{vmfdS=cbind(vmfdS, cc[,which(colnames(cc) %in% colnames(cc)[grep(assess[i], colnames(cc))])])
                                     # vmfdS=cbind(vmfdS, diff=(as.numeric(as.character(vmfdS[,ncol(vmfdS)]))-as.numeric(as.character(vmfdS[,(ncol(vmfdS)-1)]))))
                                    #  colnames(vmfdS)[ncol(vmfdS)]=paste("Diff_", assess[i], sep='')
                                 }}
                               
                               for(h in 1:ncol(vmfdS)){
                                 if(!startsWith(colnames(vmfdS)[h], "Diff")) colnames(vmfdS)[h]=substr(colnames(vmfdS)[h], 1,(nchar(colnames(vmfdS)[h])-2))
                                 vmfdS[,h]=as.numeric(as.character(vmfdS[,h]))
                               }
                               vmfdS
                      })}) 

output$summary <- renderTable({
 # print("Summary")
  input$selCmp
  input$goButton
  isolate({
    cc    <-  cap()
#     print("QQQQQQ")
#     print(head(cc))
#     assess=c("_routing_work_time_human","_routing_work_time_machine")
#     vmfdS=data.frame()
#     #plot.type
#     for(i in 1:length(assess)){
#       print(grep(assess[i], colnames(cc)))
#       if(i==1) {
#         vmfdS=cc[,which(colnames(cc) %in% colnames(cc)[grep(assess[i], colnames(cc))])]
#         vmfdS=cbind(vmfdS, diff=(as.numeric(as.character(vmfdS[,ncol(vmfdS)]))-as.numeric(as.character(vmfdS[,(ncol(vmfdS)-1)]))))
#         colnames(vmfdS)[ncol(vmfdS)]=paste("Diff_", assess[i], sep='')
#       }
#       else{vmfdS=cbind(vmfdS, cc[,which(colnames(cc) %in% colnames(cc)[grep(assess[i], colnames(cc))])])
#            vmfdS=cbind(vmfdS, diff=(as.numeric(as.character(vmfdS[,ncol(vmfdS)]))-as.numeric(as.character(vmfdS[,(ncol(vmfdS)-1)]))))
#            colnames(vmfdS)[ncol(vmfdS)]=paste("Diff_", assess[i], sep='')
#       }}
#     
#     for(h in 1:ncol(vmfdS)){
#       if(!startsWith(colnames(vmfdS)[h], "Diff")) colnames(vmfdS)[h]=substr(colnames(vmfdS)[h], 1,(nchar(colnames(vmfdS)[h])-2))
#       vmfdS[,h]=as.numeric(as.character(vmfdS[,h]))
#     }
#     
#     print("Summary")
#     print(head(vmfdS))
#     print(class(vmfdS))
#     
#     
#     })
#   
#   TS=t(summary(vmfdS))
  print(head(cc))
for(i in 1:ncol(cc)){
  colnames(cc)[i]=gsub("initial_routing_work_","",colnames(cc)[i] )
  colnames(cc)[i]=gsub("stage_routing_work_","",colnames(cc)[i] )
  colnames(cc)[i]=gsub("routing_work_","",colnames(cc)[i] )
}
  TS=t(summary(cc))
  for (h in 1:ncol(TS)){
    colnames(TS)[h]=substr(TS[1,h], 1, as.numeric(gregexpr(pattern =':',TS[,h])[[1]][1]))
    TS[,h]=trim(gsub(colnames(TS)[h], "",TS[,h] ))
  }
  
  })
  #summary(vmfdS)
TS 
})
output$summary1 <- renderPrint({
  dataset <- cap()
  for(i in 1:ncol(dataset)){
    colnames(dataset)[i]=gsub("initial_routing_work_","",colnames(dataset)[i] )
    colnames(dataset)[i]=gsub("stage_routing_work_","",colnames(dataset)[i] )
  }
  summary(dataset)
})
output$histHT <- renderPlot({
  input$selCmp
  input$goButton
  isolate({
    cc    <-  cap()
    print(head(cc))
  cc1=cc[,which(colnames(cc) %in% colnames(cc)[grep("_routing_work_time_human", colnames(cc))])]
cc1=cc1[, -which(colnames(cc1) %in% colnames(cc1)[grep("Diff", colnames(cc1))])]
    hist(as.numeric(cc1[,1]), breaks=round(nrow(cc1)/10, 0), xlim=c(min(cc1[,(1:ncol(cc1))]), max(cc1[,(1:ncol(cc1))])), col='red', xlab="seconds",main='Histogram of Work time human')
hist(as.numeric(cc1[,2]), breaks=round(nrow(cc1)/10, 0), col='blue', add=TRUE)
    legend('topright', colnames(cc1), col=c("red","blue"), cex=1, lty=1, lwd=4, bty='n');
    
  })
})

output$histMT <- renderPlot({
  input$selCmp
  input$goButton
  isolate({
    cc    <-  cap()
    print(head(cc))
    cc1=cc[,which(colnames(cc) %in% colnames(cc)[grep("_routing_work_time_machine", colnames(cc))])]
    cc1=cc1[, -which(colnames(cc1) %in% colnames(cc1)[grep("Diff", colnames(cc1))])]
    hist(as.numeric(cc1[,1]), breaks=round(nrow(cc1)/10, 0), xlim=c(min(cc1[,(1:ncol(cc1))]), max(cc1[,(1:ncol(cc1))])), col='red', xlab="seconds",main='Histogram of Work time machine')
    hist(as.numeric(cc1[,2]), breaks=round(nrow(cc1)/10, 0), col='blue', add=TRUE)
    legend('topright', colnames(cc1), col=c("red","blue"), cex=1, lty=1, lwd=4, bty='n');
    
  })
})

output$CapPlot <- renderPlot({
  input$selCmp
  input$goButton
  # x    <- x() # getALLRes(input$dataset1, input$dataset2, cmp=input$cpm)  # Old Faithful Geyser data
  # print(head(x))
  # draw the histogram with the specified number of bins
  plot.type<-switch(input$plot.type,
                    "Assigned_activities"   =   1,
                    "Overdue_time" =	2,
                    "Travel_time" 	=	3,
                    "SLA_violation" 		=	4,
                    "Percent_of_pure_revenue"=5,
                    "Work_time"=6, 
                    "Rout_density"=7,
                    "Overtime"=8
  )
  isolate({
    cc    <-  cap()
print(head(cc))
    assess=c("_routing_work_time_human","_routing_work_time_machine")
    assessN=c("Number of assigned activities","Overdue cost","Travel cost","SLA violation cost","Summary relative cost","Work time","Rout density","Overtime")
    vmfdS=data.frame()
    #plot.type
    for(i in 1:length(assess)){
      if(i==1) {
        vmfdS=cc[,which(colnames(cc) %in% colnames(cc)[grep(assess[i], colnames(cc))])]
      }
      else{vmfdS=cbind(vmfdS, cc[,which(colnames(cc) %in% colnames(cc)[grep(assess[i], colnames(cc))])])}}
    
    for(h in 1:ncol(vmfdS)){
      colnames(vmfdS)[h]=substr(colnames(vmfdS)[h], 1,(nchar(colnames(vmfdS)[h])-2))
    }
    par(mfrow=c(2,2))
    # k=1
    
    vmfdSs= sapply(vmfdS, median, na.rm=TRUE)
    #print(head(vmfdS))
    print(paste("!!!!WWWWWWWWW",plot.type))
    k=as.numeric(plot.type)*2-1
    print(k)
    print(head(vmfdS))
    #for(i in 1:length(assess)){
    dtes1=rbind(cbind(as.numeric(vmfdS[,k]), rep(colnames(vmfdS)[k], nrow(vmfdS))), cbind(as.numeric(vmfdS[,(k+1)]), rep(colnames(vmfdS)[(k+1)], nrow(vmfdS))))
    colnames(dtes1)=c("val","vers")
    dtes1=as.data.frame(dtes1)
    dtes1$val=as.numeric(dtes1$val)
    print(head(dtes1))
    ds1 <- ddply(dtes1, .(vers), summarise, mean = mean(val, na.rm=TRUE), median = median(val, na.rm=TRUE), sd = sd(val))
    # p <- ggplot(dtes1, aes(x = vers, y = val)) +
    #   geom_point() +geom_boxplot()+geom_point(color='black',alpha=0.5, position = 'jitter')+
    #   geom_point(data = ds1, aes(y = median),
    #              colour = 'red', size = 10)+ ggtitle(paste(assessN[as.numeric(plot.type)]))
    p <- ggplot(dtes1, aes(x = vers, y = val)) +
      geom_point() +geom_boxplot()+geom_point(color='white',alpha=0.5, position = 'jitter')+
      geom_point(data = ds1, aes(y = median),
                 colour = 'red', size = 10)+ ggtitle(paste(assessN[as.numeric(plot.type)]))
    print(p)
    #  print(seq(k,(k+1)))
    #   stripchart(vmfdS[,seq(k,(k+1))], vertical=TRUE, pch=19, col=c('red', 'blue'), method = "jitter", group.names=colnames(vmfdS)[seq(k,(k+1))], xlab="", ylab="", main=paste(input$cmp,assessN[i]))
    #  boxplot(vmfdS[,seq(k,(k+1))], add=TRUE)
    #   boxplot(vmfdS[,seq(k,(k+1))], col=c('red', 'blue', 'green'), main = paste(input$cmp,assessN[i]))
    
    #k=k+2
    #   }
  })
  
})
# output$parPlot2 <- renderPlot({
#   input$selCmp
#   input$goButton
#   # x    <- x() # getALLRes(input$dataset1, input$dataset2, cmp=input$cpm)  # Old Faithful Geyser data
#   # print(head(x))
#   # draw the histogram with the specified number of bins
#   isolate({
#     x    <-  x()
#     vmfd=read.csv(paste(input$cmp,"_FR.csv", sep=''), sep=',')
#     assess=c("AL_","WT_","RD_","Ov_", "R_")
#     assessN=c("Number of assigned activities","Overdue cost","Travel cost","SLA violation cost","Summary relative cost","Work time","Rout density","Overtime","Sum of revenue")
#     vmfdS=data.frame()
#     for(i in 1:length(assess)){
#       if(i==1) {
#         vmfdS=vmfd[,which(colnames(vmfd) %in% colnames(vmfd)[grep(assess[i], colnames(vmfd))])]
#       }
#       else{vmfdS=cbind(vmfdS, vmfd[,which(colnames(vmfd) %in% colnames(vmfd)[grep(assess[i], colnames(vmfd))])])}}
#     for(h in 1:ncol(vmfdS)){
#       colnames(vmfdS)[h]=substr(colnames(vmfdS)[h], 1,(nchar(colnames(vmfdS)[h])-2))
#     }
#     par(mfrow=c(2,2))
#     k=1
#     
#     vmfdSs= sapply(vmfdS, median)
#     print(head(vmfdS))
#     for(i in 1:length(assess)){
#       print(seq(k,(k+1)))
#       boxplot(vmfdS[,seq(k,(k+1))], col=c('red', 'blue', 'green'), main = paste(input$cmp,assessN[i]))
#       #  barplot(vmfdSs[seq(k,(k+2))], col=c('red', 'blue', 'green'), main = paste(company,assessN[i]))
#       k=k+2
#     }
#   })
#   
# })

output$mytable1 = renderDataTable({
  input$goButton
  
  x()
})

output$mytable2 = renderDataTable({
  input$expert
  
  xp()
})
  

})
