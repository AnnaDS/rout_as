init <- function(need) {
  ip <- .packages(all.available = T)
  if (any((need %in% ip) == F)) {
    install.packages(need[!(need %in% ip)])
  }
  ok <- sapply(1:length(need), function(p) require(need[[p]], character.only = T))
}

init(c("MBESS", "metafor", "plyr", "RMySQL", "ggplot2", "gdata", "DBI"))



params=read.csv('params1.csv', sep=';')

getCompanies <- function (){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    if (length(dbListResults(con))>0)
      dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)}
  sql='SELECT distinct `company` FROM `rgt`  order by `company`'
  #con <- dbConnect(MySQL(), user="sla1", password="sla1SLA!", dbname="anna_rhs", host="slasrv2.ua1")
  con2 <- dbConnect(MySQL(), user="rhsuser", password="hwDQL45m6jqHXXYB", dbname="rhs", host="ed1rhs01.etadirect.com")
  RN=dbGetQuery(con2,paste(sql))
  print(head(as.character(RN[,1])))
  return(as.character(RN[,1]))
}
replNA <- function(w){
  w <- data.frame(w, stringsAsFactors=FALSE)
  #w <- data.frame(lapply(w, as.numeric), stringsAsFactors=FALSE)
  for(i in 1:ncol(w)){
   w[which(is.na(w[,i])),i]=0
    w[which(as.character(w[,i])=="<NA>"),i]=0
   w[which(as.character(w[,i])==""),i]=0
  }
  return(w)
}
replNAN <- function(w){
#   w <- data.frame(lapply(w, as.character), stringsAsFactors=FALSE)
#   w <- data.frame(lapply(w, as.numeric), stringsAsFactors=FALSE)
  for(i in 1:ncol(w)){
    w[which(is.nan(w[,i])),i]=0
  }
  return(w)
}

getDB <- function (initialV, stageV, params){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    if (length(dbListResults(con))>0)
    dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)}
  sqll='Select '
  h=seq(1,nrow(params))
  for (i in h){
    #print(params[i,1])
    if(sqll=='Select ') sqll=paste(sqll, ' stage.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), 
                                   ', initial.`',as.character(params[i,1]),'`  as initial_', as.character(params[i,1]), sep='')    
    else if(as.character(params[i,1])=="company"|
              as.character(params[i,1])=="original_run_id"|
              as.character(params[i,1])=="id_rgt" |
              as.character(params[i,1])=="routing_plan_name")
      sqll=paste(sqll, ', stage.`', as.character(params[i,1]),"` as  stage_", as.character(params[i,1]), 
                 ', initial.`',as.character(params[i,1]),'` as initial_', as.character(params[i,1]), sep='')
    else 
    {
      if(as.character(params[i,1])=="input_file" | as.character(params[i,1])=="routing_version") sqll=paste(sqll, ', stage.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), 
                                                                                                            ', initial.`',as.character(params[i,1]),'`  as initial_', as.character(params[i,1]), sep='')
      else if(i==h[1]) sqll=paste(sqll, ' stage.`', as.character(params[i,1]),"` as  stage_", as.character(params[i,1]), 
                                  ', initial.`',as.character(params[i,1]),'` as initial_', as.character(params[i,1]), sep='')
      else  sqll=paste(sqll, ', stage.`', as.character(params[i,1]),"` as  stage_", as.character(params[i,1]),  
                       ', initial.`',as.character(params[i,1]),'`   as initial_', as.character(params[i,1]), sep='')
    }
  }
  
  sqll=paste(sqll, ' FROM `rgt` stage, `rgt` initial WHERE stage.`original_run_id` = initial.`id_rgt`  and stage.`routing_version` like \'', stageV, '\' and initial.`routing_version` like \'', initialV, '\' and stage.`run_time` > \'2014-08-20 9:00:00\'', sep='')
  #con <- dbConnect(MySQL(), user="sla1", password="sla1SLA!", dbname="anna_rhs", host="slasrv2.ua1")
  con2 <- dbConnect(MySQL(), user="rhsuser", password="hwDQL45m6jqHXXYB", dbname="rhs", host="ed1rhs01.etadirect.com")
  RN=dbGetQuery(con2,paste(sqll))
  return(RN)
}

getDB1 <- function (version1=vvn, init=FALSE, cmp="", dates=""){
  
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    if (length(dbListResults(con))>0)
      dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)}
  params=data.frame(V1=c("id_rgt","original_run_id","routing_run_id","routing_version","routing_plan_name",
                         #"pid",
                         #"routing_work_time_human",
                         #"routing_work_time_machine","input_file","output_file",
                         "sum_travel_time","sum_waiting_time","sum_overdue_time",
                         "company","sum_work_time","sum_not_assigned", "sum_sla_violation", "raw_waiting_time_sum", "raw_work_time_sum",
                         "raw_travel_time_sum","raw_overtime_soft_sum","raw_overtime_hard_sum","raw_not_assigned_activities","raw_total_activities",
                         "fitness","providers_used","info_sum_assigned"))
  sqll='Select '
  h=seq(1,nrow(params))
  for (i in h){
    #print(params[i,1])
    if(sqll=='Select ') sqll=paste(sqll, 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    else if(as.character(params[i,1])=="company"|
              as.character(params[i,1])=="original_run_id"|
              as.character(params[i,1])=="id_rgt" |
              as.character(params[i,1])=="routing_plan_name" |
              as.character(params[i,1])=="routing_version")
      sqll=paste(sqll, ", ", 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    else 
    {
      sqll=paste(sqll, ", ", 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    }
  }
  if(length(dates)>0)
    sqll=paste(sqll, ' FROM `rgt.cp` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" and V.run_time>= \'",dates[1]," 00:00:00\' and V.run_time<= \'",dates[2]," 00:00:00\'",  sep='' )
  else 
    sqll=paste(sqll, ' FROM `rgt.cp` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" ",  sep='' )
  
  #print(paste('Hello New',sqll))
  con <- dbConnect(MySQL(), user="sla1", password="sla1SLA!", dbname="anna_rhs", host="slasrv2.ua1")
  #con2 <- dbConnect(MySQL(), user="rhsuser", password="hwDQL45m6jqHXXYB", dbname="rhs", host="ed1rhs01.etadirect.com")
  RN=dbGetQuery(con,paste(sqll))
 if(init) colnames(RN)[grep("stage_",colnames(RN))]=paste(gsub("stage_", "initial_",colnames(RN)[grep("stage_",colnames(RN))]),version1, sep='')
   else
 colnames(RN)[grep("stage_",colnames(RN))]=paste(colnames(RN)[grep("stage_",colnames(RN))],version1, sep='')
  return(RN)
}

getDBD <- function (cmp=""){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    if (length(dbListResults(con))>0)
      dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)}
 # sqll=paste("SELECT distinct `routing_version`  FROM `rgt` WHERE `company` LIKE \"", cmp ,  "\" ", sep='')
  sql=paste("SELECT `company`, `routing_version`, `run_type` FROM `rgt` WHERE `company` like \"", cmp ,  "\"  group by `routing_version`, `run_type`", sep='')
# con <- dbConnect(MySQL(), user="sla1", password="sla1SLA!", dbname="anna_rhs1", host="slasrv2.ua1")
 con <- dbConnect(MySQL(), user="rhsuser", password="hwDQL45m6jqHXXYB", dbname="rhs", host="ed1rhs01.etadirect.com") 
 vv=dbGetQuery(con,paste(sql))
  d1=NULL
 d2=NULL
  for(j in 1:nrow(vv)){
    d=vv[j,]
    sqll1=paste("SELECT min(`run_time`)  FROM `rgt` WHERE `company` LIKE \"", as.character(d[1]) , 
                "\" and `routing_version` like\"", as.character(d[2]) , 
                "\"  and `run_type` like\"", as.character(d[3]) , 
                "\" ", sep='')
    d1[j]=as.character(dbGetQuery(con,paste(sqll1)))
    sqll2=paste("SELECT max(`run_time`)  FROM `rgt` WHERE `company` LIKE \"", as.character(d[1]) , 
                "\" and `routing_version` like\"", as.character(d[2]) , 
                "\"  and `run_type` like\"", as.character(d[3]) , 
                "\" ", sep='')
    d2[j]=as.character(dbGetQuery(con,paste(sqll2)))
  }
 VV=cbind(vv,d1,d2)
 colnames(VV)=c(colnames(vv), "Start date", "End date")
  return(VV)
}

getDBF <- function (version1=vvn, init=FALSE, cmp="", dates="", env="prod", rp=""){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    if (length(dbListResults(con))>0)
      dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)}
  params=data.frame(V1=c("id_rgt","original_run_id","routing_run_id","routing_version","routing_plan_name",
                         #"routing_plan_name",
                         #"routing_work_time_human",
                         #"routing_work_time_machine","input_file","output_file",
                         "sum_travel_time","sum_waiting_time","sum_overdue_time","sum_overtime_soft", "sum_overtime_hard",
                         "company","sum_work_time","sum_not_assigned", "sum_sla_violation", "raw_waiting_time_sum", "raw_work_time_sum",
                         "raw_travel_time_sum","raw_final_travel_time_sum","raw_overdue_time_sum", "raw_sla_violation_sum",
                         "raw_overtime_soft_sum","raw_overtime_hard_sum",
                         "raw_not_assigned_activities", "raw_not_assigned_activities_unacceptable_travel_time",
                         "raw_not_assigned_activities_provider_workday_stop", "raw_not_assigned_activities_provider_overload",
                         "raw_total_activities","raw_assigned_activities","raw_rejected_activities", "sum_would_be_not_assigned",
                         "fitness","providers_used","info_sum_assigned"))
  
  sqll='Select '
  h=seq(1,nrow(params))
  for (i in h){
    #print(params[i,1])
    if(sqll=='Select ') sqll=paste(sqll, 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    else if(as.character(params[i,1])=="company"|
              as.character(params[i,1])=="original_run_id"|
              as.character(params[i,1])=="id_rgt" |
              as.character(params[i,1])=="routing_plan_name" |
              as.character(params[i,1])=="routing_version")
      sqll=paste(sqll, ", ", 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    else 
    {
      sqll=paste(sqll, ", ", 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    }
  }
#   if(length(dates)>0)
#     sqll=paste(sqll, ' FROM `rgt.cp` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" and V.run_time>= \'",dates[1]," 00:00:00\' and V.run_time<= \'",dates[2]," 00:00:00\' and V.run_type like \'",env,"\'",  sep='' )
#   else 
#     sqll=paste(sqll, ' FROM `rgt.cp` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" ",  sep='' )
#   

if(length(dates)>0)
  sqll=paste(sqll, ' FROM `rgt` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" and V.run_time>= \'",dates[1]," 00:00:00\' and V.run_time<= \'",dates[2]," 00:00:00\' and V.run_type like \'",env,"\'",  sep='' )
else 
  sqll=paste(sqll, ' FROM `rgt` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" ",  sep='' )

if(rp!="")
  sqll=paste(sqll, ' and V.routing_plan_name like \'',rp,'\'',  sep='' )

  print(paste('Hello New',sqll))
 # con <- dbConnect(MySQL(), user="sla1", password="sla1SLA!", dbname="anna_rhs", host="slasrv2.ua1")
  #con <- dbConnect(MySQL(), user="sla1", password="sla1SLA!", dbname="anna_rhs1", host="slasrv2.ua1")
  con2 <- dbConnect(MySQL(), user="rhsuser", password="hwDQL45m6jqHXXYB", dbname="rhs", host="ed1rhs01.etadirect.com")
  RN=dbGetQuery(con2,paste(sqll))
numC=c("id_rgt","original_run_id","routing_run_id",
       "sum_travel_time","sum_waiting_time","sum_overdue_time","sum_work_time","sum_not_assigned", "sum_sla_violation", "raw_waiting_time_sum", "raw_work_time_sum",
       "raw_travel_time_sum","raw_overtime_soft_sum","raw_overtime_hard_sum","raw_not_assigned_activities","raw_total_activities",
       "fitness","providers_used","info_sum_assigned")
for( i in 1:length(numC)){
  RN[,grep(numC[i],colnames(RN))]=as.numeric(as.character(RN[,grep(numC[i],colnames(RN))]))
}
  if(init) colnames(RN)[grep("stage_",colnames(RN))]=paste(gsub("stage_", "initial_",colnames(RN)[grep("stage_",colnames(RN))]),version1, sep='')
  else
    colnames(RN)[grep("stage_",colnames(RN))]=paste(colnames(RN)[grep("stage_",colnames(RN))],version1, sep='')
#print (head(RN))
  return(RN)
}


getDBF_id <- function (version1=vvn, init=FALSE,  env="prod", id=""){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    if (length(dbListResults(con))>0)
      dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)}
  params=data.frame(V1=c("id_rgt","original_run_id","routing_run_id","routing_version","routing_plan_name",
                         #"routing_plan_name",
                         #"routing_work_time_human",
                         #"routing_work_time_machine","input_file","output_file",
                         "sum_travel_time","sum_waiting_time","sum_overdue_time","sum_overtime_soft", "sum_overtime_hard",
                         "company","sum_work_time","sum_not_assigned", "sum_sla_violation", "raw_waiting_time_sum", "raw_work_time_sum",
                         "raw_travel_time_sum","raw_final_travel_time_sum","raw_overdue_time_sum", "raw_sla_violation_sum",
                         "raw_overtime_soft_sum","raw_overtime_hard_sum",
                         "raw_not_assigned_activities", "raw_not_assigned_activities_unacceptable_travel_time",
                         "raw_not_assigned_activities_provider_workday_stop", "raw_not_assigned_activities_provider_overload",
                         "raw_total_activities","raw_assigned_activities","raw_rejected_activities", "sum_would_be_not_assigned",
                         "fitness","providers_used","info_sum_assigned"))
  
  sqll='Select '
  h=seq(1,nrow(params))
  for (i in h){
    #print(params[i,1])
    if(sqll=='Select ') sqll=paste(sqll, 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    else if(as.character(params[i,1])=="company"|
              as.character(params[i,1])=="original_run_id"|
              as.character(params[i,1])=="id_rgt" |
              as.character(params[i,1])=="routing_plan_name" |
              as.character(params[i,1])=="routing_version")
      sqll=paste(sqll, ", ", 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    else 
    {
      sqll=paste(sqll, ", ", 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    }
  }
  #   if(length(dates)>0)
  #     sqll=paste(sqll, ' FROM `rgt.cp` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" and V.run_time>= \'",dates[1]," 00:00:00\' and V.run_time<= \'",dates[2]," 00:00:00\' and V.run_type like \'",env,"\'",  sep='' )
  #   else 
  #     sqll=paste(sqll, ' FROM `rgt.cp` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" ",  sep='' )
  #   
    sqll=paste(sqll, ' FROM `rgt` V WHERE V.`routing_version` like \'', version1,"\' and V.id_rgt = \'",id,"\'",  sep='' )
  
  print(paste('Hello New',sqll))
  # con <- dbConnect(MySQL(), user="sla1", password="sla1SLA!", dbname="anna_rhs", host="slasrv2.ua1")
  #con <- dbConnect(MySQL(), user="sla1", password="sla1SLA!", dbname="anna_rhs1", host="slasrv2.ua1")
  con2 <- dbConnect(MySQL(), user="rhsuser", password="hwDQL45m6jqHXXYB", dbname="rhs", host="ed1rhs01.etadirect.com")
  RN=dbGetQuery(con2,paste(sqll))
  numC=c("id_rgt","original_run_id","routing_run_id",
         "sum_travel_time","sum_waiting_time","sum_overdue_time","sum_work_time","sum_not_assigned", "sum_sla_violation", "raw_waiting_time_sum", "raw_work_time_sum",
         "raw_travel_time_sum","raw_overtime_soft_sum","raw_overtime_hard_sum","raw_not_assigned_activities","raw_total_activities",
         "fitness","providers_used","info_sum_assigned")
  for( i in 1:length(numC)){
    RN[,grep(numC[i],colnames(RN))]=as.numeric(as.character(RN[,grep(numC[i],colnames(RN))]))
  }
  if(init) colnames(RN)[grep("stage_",colnames(RN))]=paste(gsub("stage_", "initial_",colnames(RN)[grep("stage_",colnames(RN))]),version1, sep='')
  else
    colnames(RN)[grep("stage_",colnames(RN))]=paste(colnames(RN)[grep("stage_",colnames(RN))],version1, sep='')
  #print (head(RN))
  return(RN)
}


getDBwt <- function (version1=vvn,  init=FALSE, cmp="", dates="", env){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    if (length(dbListResults(con))>0)
      dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)}
  params=data.frame(V1=c("id_rgt","original_run_id","routing_run_id","routing_version","routing_plan_name",
                         "routing_work_time_human",
                         "routing_work_time_machine"))
  sqll='Select '
  h=seq(1,nrow(params))
  for (i in h){
    #print(params[i,1])
    if(sqll=='Select ') sqll=paste(sqll, 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    else 
#       if(as.character(params[i,1])=="company"|
#               as.character(params[i,1])=="original_run_id"|
#               as.character(params[i,1])=="id_rgt" |
#               as.character(params[i,1])=="routing_plan_name" |
#               as.character(params[i,1])=="routing_version")
#       sqll=paste(sqll, ", ", 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
#     else 
    {
      sqll=paste(sqll, ", ", 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    }
  }
#   if(length(dates)>0)
#     sqll=paste(sqll, ' FROM `rgt.cp` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" and V.run_time>= \'",dates[1]," 00:00:00\' and V.run_time<= \'",dates[2]," 00:00:00\'",  sep='' )
#   else 
#     sqll=paste(sqll, ' FROM `rgt.cp` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" ",  sep='' )
#   
if(length(dates)>0)
  sqll=paste(sqll, ' FROM `rgt` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" and V.run_time>= \'",dates[1]," 00:00:00\' and V.run_time<= \'",dates[2]," 00:00:00\' and run_type like \'", env, "\' ",  sep='' )
else 
  sqll=paste(sqll, ' FROM `rgt` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" ",  sep='' )

  print(paste('Buy',sqll))
 # con <- dbConnect(MySQL(), user="sla1", password="sla1SLA!", dbname="anna_rhs", host="slasrv2.ua1")
#con <- dbConnect(MySQL(), user="sla1", password="sla1SLA!", dbname="anna_rhs1", host="slasrv2.ua1")
  con2 <- dbConnect(MySQL(), user="rhsuser", password="hwDQL45m6jqHXXYB", dbname="rhs", host="ed1rhs01.etadirect.com")
  RN=dbGetQuery(con2,paste(sqll))
  if(init) colnames(RN)[grep("stage_",colnames(RN))]=paste(gsub("stage_", "initial_",colnames(RN)[grep("stage_",colnames(RN))]),version1, sep='')
  else
    colnames(RN)[grep("stage_",colnames(RN))]=paste(colnames(RN)[grep("stage_",colnames(RN))],version1, sep='')
numC=c("routing_work_time_human",
"routing_work_time_machine")
for(i in 1:2)
  RN[,grep(numC[i],colnames(RN))]=as.numeric(as.character(RN[,grep(numC[i],colnames(RN))]))
print(head(RN))
  return(RN)
}


is.null.col=function(l, name){
  for(i in 1:length(l)){
    if(length(l[[i]]$name[which(l[[i]]$name !=0 )])>0)
      return (FALSE)
  }
  return (TRUE)
}

getCapacity <- function(initialV, stageV,  cmp='', initD, stageD, envI, envS){
  RN=list()
 # print(initD, stageD)
  AFR=NULL
  cnv=c(initialV, stageV)
  dv=data.frame(v=c("2.0.7.2", "2.1.0.2","2.1.1.0"), vF=c("2.0.7", "2.1.0","2.1.1"))

  initialV[which(initialV %in%  dv$vF)]=as.character(dv$v[which(dv$vF %in% initialV)])
  stageV[which(stageV %in%  dv$vF)]=as.character(dv$v[which(dv$vF %in% stageV)])

  inr=getDBwt(version1=as.character(initialV),init=TRUE, cmp=cmp, dates=initD, env=envI)  
  colnames(inr)[grep("initial_id_rgt",colnames(inr))]='original_id'
  colnames(inr)[grep("_company",colnames(inr))]='company'
 print(head(inr))
  RES=NULL
 
    #print(cmp)
 
    t=getDBwt(stageV, cmp=cmp, dates=stageD, env=envS)
 if((as.numeric(gsub("\\.","",initialV))<2100 || as.numeric(gsub("\\.","",stageV))<2100) ) v=TRUE
 else v=FALSE
    if (v) colnames(t)[grep("stage_original_run_id",colnames(t))]='original_id'
    colnames(t)[grep("_company",colnames(t))]='company'
    #print(head(t))
if (v) RPt=merge(inr, t, by=intersect(names(inr), names(t)) )
else {
  colnames(t)[grep("_routing_plan_name",colnames(t))]='routing_plan_name'
  colnames(inr)[grep("_routing_plan_name",colnames(inr))]='routing_plan_name'
  rpiN=unique(inr$routing_plan_name)
  rpsN=unique(t$routing_plan_name)
  RPf=intersect(rpiN, rpsN)
  
  RPt=NULL
  for(j in 1:length(RPf)){
    inrRP=inr[which(as.character(inr$routing_plan_name) %in% RPf[j]),]
    tRP=t[which(as.character(t$routing_plan_name) %in% RPf[j]),]
   # print('Human time!!!!!')
    #print(as.character(inrRP[grep("_work_time_human",colnames(inrRP)),]))
    RPt=rbind(RPt, data.frame(routing_plan=RPf[j], 
                              FRRR_work_time_human=median(as.numeric(as.character(inrRP[,grep("_work_time_human",colnames(inrRP))]))),
                              FRRR_work_time_machine=median(inrRP[,grep("_work_time_machine",colnames(inrRP))]),
                              SRRR_work_time_human=median(tRP[,grep("_work_time_human",colnames(tRP))]), 
                              SRRR_work_time_machine=median(tRP[,grep("_work_time_machine",colnames(tRP))])))
  }
  #print(head(RPt))
  inrRP=inr[which(!(as.character(inr$routing_plan_name) %in% RPf)),]
  tRP=t[which(!(as.character(t$routing_plan_name) %in% RPf)),]
  RPt=rbind(RPt, data.frame(routing_plan="Others", FRRR_work_time_human=median(inrRP[,grep("_work_time_human",colnames(inrRP))]),
                            FRRR_work_time_machine=median(inrRP[,grep("_work_time_machine",colnames(inrRP))]),
                            SRRR_work_time_human=median(tRP[,grep("_work_time_human",colnames(tRP))]), 
                            SRRR_work_time_machine=median(tRP[,grep("_work_time_machine",colnames(tRP))])))
}
  
# print("Capacity!!!!!!!!!!!!!!")
 #print(summary(RPt))
 return(RPt)
 
  
}

getALLResF <- function(initialV, stageV,  cmp='vm', initD, stageD, init_env, stage_env, rp=""){
  RN=list()
#print(initD, stageD)
  AFR=NULL
  cnv=c(initialV, stageV)
  dv=data.frame(v=c("2.0.7.2", "2.1.0.2","2.1.1.0"), vF=c("2.0.7", "2.1.0","2.1.1"))
  #for(h in 1:length(initialV)){
  initialV[which(initialV %in%  dv$vF)]=as.character(dv$v[which(dv$vF %in% initialV)])
  stageV[which(stageV %in%  dv$vF)]=as.character(dv$v[which(dv$vF %in% stageV)])
  inr=getDBF(version1=as.character(initialV),init=TRUE, cmp=cmp, dates=initD, init_env, rp=rp)
  if(stage_env=="stage") colnames(inr)[grep("initial_id_rgt",colnames(inr))]='original_id'
  colnames(inr)[grep("initial_routing_plan_name",colnames(inr))]='routing_plan_name'
  colnames(inr)[grep("_company",colnames(inr))]='company'

  RES=NULL 

  t=getDBF(stageV, cmp=cmp, dates=stageD, stage_env, init=FALSE, rp=rp)
  if(stage_env=="stage") colnames(t)[grep("stage_original_run_id",colnames(t))]='original_id'
  colnames(t)[grep("stage_routing_plan_name",colnames(t))]='routing_plan_name'
    colnames(t)[grep("_company",colnames(t))]='company'
    #print(head(t))

inr=inr[which(inr[,grep("providers_used",colnames(inr))]!=0),]
t=t[which(t[,grep("providers_used",colnames(t))]!=0),]
if((as.numeric(gsub("\\.","",initialV))<2100 || as.numeric(gsub("\\.","",stageV))<2100)) v=TRUE
else v=FALSE
#orig_vec=c("RPA","AL", "OvW", "AAs", "RD")
orig_vec=c("TRAS","RAA", "AL", "OvWt", "RD", "AAs")
idr=c(0,1,1,0,0.5)
if(rp!=''){
  orig_vec=c("TRAS","RAA", "AL", "OvWt", "RD", "AAs")
  
  #R=data.frame(id_rgt=as.numeric(w[,grep("id_rgt",colnames(w))]),AA=AAPr, WrtPr=WrtPr, TrPr=TrPr, WtPr=WtPr, OvPr=OvPr,  stringsAsFactors=F)  
  
  vec_RP=c("id_rgt", "AA", "WrtPr", "TrPr", "WtPr", "OvPr", "PR")
  
  RA1=getRPdata(inr, (as.numeric(inr[,grep("_raw_total_activities",colnames(inr))])),  v=v)
  RA2=getRPdata(t, (as.numeric(t[,grep("_raw_total_activities",colnames(t))])),  v=v)
  #print("Start")
  RA11=getRelAs(inr, (as.numeric(inr[,grep("_raw_total_activities",colnames(inr))])), fin=FALSE, v=v)
  #print(head(RA11))
  RA22=getRelAs(t, (as.numeric(t[,grep("_raw_total_activities",colnames(t))])), fin=FALSE, v=v)
  #print(head(RA22))
  colN=1
  for(l in 1:length(orig_vec))
    colN=c(colN, grep(orig_vec[l],colnames(RA11)))
 
  RA11=RA11[, colN]
  RA22=RA22[, colN]
 #print(head(RA11))
 ra1c=NULL
 ra2c=NULL
  for(l in 1:nrow(RA11)) {#print(RA1[l,])
    print(paste("Area",l))
    ra1c=rbind(ra1c, data.frame(id_rgt=RA11[l,1], val=calc.v(RA11[l,seq(2, ncol(RA11))], idr, orig_vec )))
    if(l==1) {plot(idr, t='l', lwd=3, col='red', main=rp)}
    lines(as.numeric(RA11[l,seq(2, ncol(RA11))]), col='blue')}
  
  for(l in 1:nrow(RA22)) {#print(RA1[l,])
    print(paste("Area",l))
    ra2c=rbind(ra2c, data.frame(id_rgt=RA22[l,1], val=calc.v(RA22[l,seq(2, ncol(RA22))], idr, orig_vec )))
    #ra2c=c(ra2c,calc.v(RA22[l,seq(2, ncol(RA11))], idr, orig_vec ))
    lines(as.numeric(RA22[l,seq(2, ncol(RA11))]), col='green')}
  
  #print(head(RA1))
  #print(head(RA2))
  all_colls1=colnames(RA1)
  all_colls2=colnames(RA2)
  ra11=RA1[,c(grep(vec_RP[1], colnames(RA1)))]
  #print(ra11)
  ra22=RA2[,c(grep(vec_RP[1], colnames(RA2)))]
  #print(ra22)
  for(h in 2:length(vec_RP)){
    ra11=cbind(ra11,RA1[,c(grep(vec_RP[h], colnames(RA1)))])
    ra22=cbind(ra22,RA2[,c(grep(vec_RP[h], colnames(RA2)))])
  }
  #ra11=cbind(ra11, ra1c)
  #ra22=cbind(ra22, ra2c)
 colnames(ra11)[1]=vec_RP[1]
 colnames(ra22)[1]=vec_RP[1]
 print(class(ra11))
 print(class(ra22))
 ra11=merge(ra11, ra1c, by=intersect(names(ra11), names(ra1c)) )
 ra22=merge(ra22, ra2c, by=intersect(names(ra22), names(ra2c)) )
 
 
  RA1=ra11
  RA2=ra22
 print(head(RA1))
  colnames(RA1)=c("id_rgt", "Assigned activities", "Work time", "Travel time", "Wait time", "Overtime","Providers","Val")
  colnames(RA2)=c("id_rgt", "Assigned activities", "Work time", "Travel time", "Wait time", "Overtime","Providers", "Val")
  for(p in 1:ncol(RA1)){
    RA1[,p]=round(as.numeric(as.character( RA1[,p])),3)
    RA2[,p]=round(as.numeric(as.character( RA2[,p])),3)
  }
  print("Second stage")
  d=data.frame("Second", "comp", "res","","","","", "")
  colnames(d)=colnames(RA1)
  RA1=rbind(RA1,d)
  #RA3=data.frame("Second", "comp", "res","","","","","")
  #colnames(RA3)=all_colls11
 #print(tail(RA1))
  
  return (rbind(RA1,  RA2))
}
RPi=unique(inr[,grep("routing_plan_name",colnames(inr))])
RPs=unique(t[,grep("routing_plan_name",colnames(t))])
RPf=intersect(RPi, RPs)
RPi=RPi[-which(RPi %in% RPf)]
RPs=RPs[-which(RPs %in% RPf)]
# Add number of intersect Routing plans to the report!
# It may be the percentage


inrf=inr[which(as.character(inr$routing_plan_name) %in% RPf), ]
tf=t[which(as.character(t$routing_plan_name) %in% RPf), ]
inrr=inr[which(!(as.character(inr$routing_plan_name) %in% RPf)), ]
tr=t[which(!(as.character(t$routing_plan_name) %in% RPf)), ]

gc()


FR1=NULL
FR2=NULL
FR=NULL
FRS=NULL
all_colls=NULL
N1=NULL
N2=NULL

#print(RPf)

RRA1=NULL
RRA2=NULL
for(i in 1:length(RPf)){
  print(paste("Routing plan", RPf[i]))
  R1=inrf[which(as.character(inrf$routing_plan_name) %in% RPf[i]),]
  gc()
  R2=tf[which(as.character(tf$routing_plan_name) %in% RPf[i]),]
 gc()

  RA1=getRelAs(R1, (as.numeric(R1[,grep("_raw_total_activities",colnames(R1))])), fin=FALSE, v=v, orig_vec)
  RA2=getRelAs(R2, (as.numeric(R2[,grep("_raw_total_activities",colnames(R2))])), fin=FALSE, v=v, orig_vec)
  colnames(RA1)=paste("FRRR_", colnames(RA1), sep="")
  colnames(RA2)=paste("SRRR_", colnames(RA2), sep="")
  all_colls1=colnames(RA1)
  all_colls2=colnames(RA2)
  FR=rbind(FR,cbind(Routing_plan=RPf[i], t(as.table(sapply(RA1, median))), t(as.table(sapply(RA2, median)))))
FRS=rbind(FRS,cbind(Routing_plan=RPf[i], t(as.table(sapply(RA1, sum))), t(as.table(sapply(RA2, sum)))))
 ra11=RA1[,c(grep(orig_vec[1], colnames(RA1)))]
 ra22=RA2[,c(grep(orig_vec[1], colnames(RA2)))]
 all_colls11=all_colls1[c(grep(orig_vec[1], colnames(RA1)))]
 all_colls22=all_colls2[c(grep(orig_vec[1], colnames(RA2)))]
 for(h in 2:length(orig_vec)){
   ra11=cbind(ra11,RA1[,c(grep(orig_vec[h], colnames(RA1)))])
   ra22=cbind(ra22,RA2[,c(grep(orig_vec[h], colnames(RA2)))])
   all_colls11=c(all_colls11, all_colls1[c(grep(orig_vec[h], colnames(RA1)))])
   all_colls22=c( all_colls22, all_colls2[c(grep(orig_vec[h], colnames(RA2)))])
 }
 RA1=ra11
 RA2=ra22
 colnames(RA1)=all_colls11
 colnames(RA2)=all_colls22
  if(v) {
    Mlosses=max(RA1[, grep("_AL",colnames(RA1))], RA2[, grep("_AL",colnames(RA2))])
    RA1[, grep("_AL",colnames(RA1))]=RA1[, grep("_AL",colnames(RA1))]/Mlosses
    RA2[, grep("_AL",colnames(RA2))]=RA2[, grep("_AL",colnames(RA2))]/Mlosses
  }

  ra1c=NULL
#print(tail(RA1))
  for(l in 1:nrow(RA1)) {#print(RA1[l,])
                         ra1c=c(ra1c,calc.v(RA1[l,], idr, orig_vec ))
  if(l==1) {plot(idr, t='l', lwd=3, col='red', main=RPf[i])}
  lines(as.numeric(RA1[l,]), col='blue')}
 RRA1=rbind(RRA1, RA1)
  ra2c=NULL

  for(l in 1:nrow(RA2)) {ra2c=c(ra2c,calc.v(RA2[l,], idr, orig_vec))
                         lines(as.numeric(RA2[l,]), col='darkgreen')}
#print("end")
 RRA2=rbind(RRA2, RA2)
  if(length(which(is.nan(ra1c)))>0) warning("Not all asessments are numeric in the First selsection")
  if(length(which(is.nan(ra2c)))>0) warning("Not all asessments are numeric in the Second selsection")
  FR1[i]=median(ra1c)
  FR2[i]=median(ra2c)
  N1[i]=length(ra1c)
  N2[i]=length(ra2c)
}
write.csv(RRA1, "RRA1.csv", row.names=FALSE)
write.csv(RRA2, "RRA2.csv", row.names=FALSE)
if(nrow(inrr)>0) {Ifr=getRelAs(inrr, (as.numeric(inrr[,grep("_raw_total_activities",colnames(inrr))])), fin=FALSE, v=v, orig_vec)
                  Ifr= t(as.table(sapply(Ifr, median)))
                  IfrS= t(as.table(sapply(Ifr, sum)))
                  Ifr <- data.frame(matrix(c(as.numeric(Ifr)), nrow=1))
                  IfrS <- data.frame(matrix(c(as.numeric(IfrS)), nrow=1))
                  colnames(Ifr)=all_colls1
                  colnames(IfrS)=all_colls1
}
else {
  Ifr=data.frame(matrix(vector(), 1, length(all_colls1), dimnames=list(c(), all_colls1)), stringsAsFactors=F)
  IfrS=data.frame(matrix(vector(), 1, length(all_colls1), dimnames=list(c(), all_colls1)), stringsAsFactors=F)
}
if(nrow(tr)>0) {Sfr=getRelAs(tr, (as.numeric(tr[,grep("_raw_total_activities",colnames(tr))])), fin=FALSE, v=v, orig_vec)
                Sfr= t(as.table(sapply(Sfr, median)))
                SfrS= t(as.table(sapply(Sfr, sum)))
                Sfr <- data.frame(matrix(c(as.numeric(Sfr)), nrow=1))
                SfrS <- data.frame(matrix(c(as.numeric(SfrS)), nrow=1))
                colnames(Sfr)=all_colls2
                colnames(SfrS)=all_colls2}
else{
  Sfr=data.frame(matrix(vector(), 1, length(all_colls2), dimnames=list(c(), all_colls2)), stringsAsFactors=F)
  SfrS=Sfr
}

Ofr=data.frame(Routing_plan="Others")
if(length(is.na(Ifr))!=length(Ifr) & length(is.na(Sfr))!=length(Sfr) ){
Rrf=cbind(Ofr,Ifr, Sfr)
RrfS=cbind(Ofr,IfrS, SfrS)
#print(Ifr)
#print(Sfr)
if(!v) {
  FR=rbind(Rrf, FR)
  FRS=rbind(RrfS, FRS)
}

}
FR=as.data.frame(FR)
FRS=as.data.frame(FRS)
colnames(FR)=c("Routing_plan", all_colls1, all_colls2)
colnames(FRS)=c("Routing_plan", all_colls1, all_colls2)
write.csv(FR,  paste(cmp,"_FR.csv", sep=''), row.names = FALSE)
write.csv(FRS,  paste(cmp,"_FRS.csv", sep=''), row.names = FALSE)
  
FRR=cbind(Routing_plan=RPf, FN=N1, FRRR=FR1, SRRR=FR2, SN=N2)

Irrr=split(inrr, list(inrr[,grep("routing_plan_name",colnames(inrr))]), drop=TRUE)
Srrr=split(tr, list(tr[,grep("routing_plan_name",colnames(tr))]), drop=TRUE)
FRO=NULL
FR1O=NULL
if(length(Irrr)>0) {for(i in 1:length(Irrr)){
  RA=getRelAs(Irrr[[i]], (as.numeric(Irrr[[i]][,grep("_raw_total_activities",colnames(Irrr[[i]]))])), fin=TRUE, v=v, orig_vec)
  colnames(RA)=all_colls11

  if(v) {
    Mlosses=max(RA[, grep("_AL",colnames(RA))])
    RA[, grep("_AL",colnames(RA))]=RA[, grep("_AL",colnames(RA))]/Mlosses
  }
  rac=NULL
  #print(head(RA))
  for(l in 1:nrow(RA)) rac=c(rac,calc.v(RA[l,], idr, orig_vec))
  if(length(which(is.nan(rac)))>0) warning("Not all asessments are numeric in the First selsection")
  FR1O[i]=median(rac)
}}

FR2O=NULL
if(length(Srrr)>0) {for(i in 1:length(Srrr)){
  RA=getRelAs(Srrr[[i]], (as.numeric(Srrr[[i]][,grep("_raw_total_activities",colnames(Srrr[[i]]))])), fin=TRUE, v=v, orig_vec)
  colnames(RA)=all_colls22
 
  if(v) {
    Mlosses=max(RA[, grep("_AL",colnames(RA))])
    RA[, grep("_AL",colnames(RA))]=RA[, grep("_AL",colnames(RA))]/Mlosses
  }
  rac=NULL
  for(l in 1:nrow(RA)) rac=c(rac,calc.v(RA[l,], idr, orig_vec))
  if(length(which(is.nan(rac)))>0) warning("Not all asessments are numeric in the Second selsection")
  FR2O[i]=median(rac, na.rm=TRUE)
}}
Ofr=data.frame(Routing_plan="Others")
FR1O=median(FR1O, na.rm=TRUE)
#print(paste("FR1O",FR1O))
FR2O=median(FR2O, na.rm=TRUE)
#print(paste("FR2O",FR2O))
if(is.null(FR1O))FR1O=NA
if(is.null(FR2O))FR2O=NA
#print(data.frame(FN=nrow(inrr), FRRR=FR1O, SRRR=FR2O, SN=nrow(tr)))
#print(paste("FR2O",FR2O))
Ofr=cbind(Ofr, FN=nrow(inrr), FRRR=(FR1O), SRRR=(FR2O), SN=nrow(tr))
#print(paste("Ofr", Ofr))
FRR[,3]=round(as.numeric(as.character(FRR[,3])),4)
FRR[,4]=round(as.numeric(as.character(FRR[,4])),4)

# print(class(Ofr))
# print(dim(Ofr))
colnames(Ofr)=colnames(FRR)
Ofr[,2]=as.numeric(as.character(Ofr[,2]))
Ofr[,5]=as.numeric(as.character(Ofr[,5]))
if(!is.na(Ofr[,3]))Ofr[,3]=round(as.numeric(as.character(Ofr[,3])), 4)
if(!is.na(Ofr[,4]))Ofr[,4]=round(as.numeric(as.character(Ofr[,4])), 4)
#print(Ofr)

FRR[,1]=as.character(FRR[,1])
Ofr[,1]=as.character(Ofr[,1])

if(!v) FRR=rbind( Ofr, FRR,deparse.level = 0)
write.csv(FRR,  paste(cmp,".csv", sep=''), row.names = FALSE)
return (FRR)  
}

calc.v1 <- function(F, idr, orig_vec){
  cc=NULL
for(i in 1:nrow(F)){ 
  if(F[,grep("_AAs", colnames(F))]==1) idr=c(1,1,0,1,0) else idr=c(1,1,0,1,0)
  cc=c(cc,cor(as.numeric(F), idr))
}
return (cc)
}

calc.vC <- function(F, idr, orig_vec){
  cc=NULL
   # if(F[grep("_AAs", names(F))]==1) idr=c(1,1,0,0) else idr=c(1,1,1,0)
#   idr=c(1,0,1,0)
print(paste("find error!!!",F))
if(F[4]==1) idr=c(1,1,0,1,0) else idr=c(1,1,0,1,0)
    cc=cor(as.numeric(F), idr)
  return (cc)
}

calc.v <- function(F, idr, orig_vec){
  cc=NULL
  print(F)
 if(F[6]==1) idr=c(0,1,1,0,0.7) else idr=c(0,1, 1,0,0.9)
 S1=calc.area(idr)
 F=F[seq(1,5)]
 S2=calc.area(F)
ss=0  
tss=0  
for(i in 1:length(F)) {ss=sum(ss, ((F[i]-idr[i])^2))
                       tss=sum(tss, ((idr[i]-mean(idr))^2))
  }
#print(paste(ss, tss))
#cc=cor(as.numeric(F), idr)
# return (cc)
#return(ss)
# return((1-ss/tss))
#return(cc*1/abs(S1-S2))
print(paste(S1, S2))
return(1-abs(S1-S2)/S1)
}
calc.area <- function(idr){
  ss=0
  for(i in 1:(length(idr)-1)){
    ss=sum(ss,(min(idr[i:(i+1)])+(max(idr[i:(i+1)])-min(idr[i:(i+1)]))/2)) 
  }
  return(ss)
}

grepCol <- function(data, colname, not_contain=""){
  if(not_contain=="")
    return (as.numeric(data[,grep(colname,colnames(data))]))
}
getAssessments <- function(w, total=NULL, v=TRUE){
  
  w=replNA(w)
  fn=which(as.numeric(w[,grep("_fitness",colnames(w))])==0)
  if(length(fn)>0)  w=w[-fn,] # remove the routs that don't provide any results
  if(is.null(total))
    total=as.numeric(w[,grep("raw_total_activities",colnames(w))])
  else if(length(total)==1)  total=replNA(total)
 
  if (length(which(as.numeric(w[,grep("raw_assigned_activities",colnames(w))])==0))>0)
    w=w[-which(as.numeric(w[,grep("raw_assigned_activities",colnames(w))])==0),]
  total=as.numeric(w[,grep("raw_total_activities",colnames(w))])
  AA=as.numeric(w[,grep("raw_assigned_activities",colnames(w))])
  REJ_A=as.numeric(w[,grep("raw_rejected_activities",colnames(w))])
  AAs=AA/(as.numeric(total)-as.numeric(w[,grep("raw_rejected_activities",colnames(w))])
          -as.numeric(w[,grep("raw_not_assigned_activities_unacceptable_travel_time",colnames(w))])
          -as.numeric(w[,grep("raw_not_assigned_activities_provider_workday_stop",colnames(w))])
          -as.numeric(w[,grep("raw_not_assigned_activities_provider_overload",colnames(w))]))
#print(paste("AAs",AAs))
  wna=as.numeric(w[,grep("sum_not_assigned",colnames(w))])
  OvC=as.numeric(w[,grep("sum_overdue_time",colnames(w))])/AA
  OvCt=as.numeric(w[,grep("raw_overdue_time_sum",colnames(w))])/AA
  TrC=as.numeric(w[,grep("sum_travel_time",colnames(w))])/AA
  TrCt=as.numeric(w[,grep("raw_travel_time_sum",colnames(w))])/AA
  SlC=as.numeric(w[,grep("sum_sla_violation",colnames(w))])/AA
  SlCt=as.numeric(w[,grep("raw_sla_violation_sum",colnames(w))])/AA
  print(paste("OvCt", length(OvCt), "TrCt", length(TrCt), "SlCt", length(SlCt)))
  RP=as.numeric(w[,grep("info_sum_assigned",colnames(w))])
  SNA=as.numeric(w[,grep("sum_not_assigned",colnames(w))])/as.numeric(w[,grep("raw_not_assigned_activities2",colnames(w))])
  if(v){
    AL=(as.numeric(w[,grep("_sum_overdue_time",colnames(w))])+
          as.numeric(w[,grep("_sum_travel_time",colnames(w))])+
          as.numeric(w[,grep("_sum_sla_violation",colnames(w))]))/as.numeric(total)
    R=(as.numeric(w[,grep("_sum_overdue_time",colnames(w))])+
         as.numeric(w[,grep("_sum_travel_time",colnames(w))])+
         as.numeric(w[,grep("_sum_sla_violation",colnames(w))])+
         as.numeric(w[,grep("_sum_overtime_soft",colnames(w))])+
         as.numeric(w[,grep("_sum_overtime_hard",colnames(w))]))
    RAA=R/AA
   # RPL=(RP-AL)/(RP+AL)
  } else {
    #Rule if all activities are assigned and 
    R=(RP-((#as.numeric(w[,grep("_sum_overdue_time",colnames(w))])+
              as.numeric(w[,grep("sum_travel_time",colnames(w))])+
             # as.numeric(w[,grep("sum_sla_violation",colnames(w))])+
              as.numeric(w[,grep("sum_overtime_soft",colnames(w))])+
              as.numeric(w[,grep("sum_overtime_hard",colnames(w))])+
              as.numeric(w[,grep("sum_work_time",colnames(w))])+
              as.numeric(w[,grep("sum_waiting_time",colnames(w))])))
       #+
        # as.numeric(w[,grep("sum_would_be_not_assigned",colnames(w))])
       )
    R1=((#as.numeric(w[,grep("_sum_overdue_time",colnames(w))])+
      as.numeric(w[,grep("sum_travel_time",colnames(w))])+
        # as.numeric(w[,grep("sum_sla_violation",colnames(w))])+
        as.numeric(w[,grep("sum_overtime_soft",colnames(w))])+
        as.numeric(w[,grep("sum_overtime_hard",colnames(w))])+
        as.numeric(w[,grep("sum_work_time",colnames(w))])+
        as.numeric(w[,grep("sum_waiting_time",colnames(w))])))/RP
    R1[which(R1>1)]=((#as.numeric(w[which(R1>1),grep("_sum_overdue_time",colnames(w))])+
      as.numeric(w[which(R1>1),grep("sum_travel_time",colnames(w))])+
        # as.numeric(w[,grep("sum_sla_violation",colnames(w))])+
        as.numeric(w[which(R1>1),grep("sum_overtime_soft",colnames(w))])+
        as.numeric(w[which(R1>1),grep("sum_overtime_hard",colnames(w))])+
        as.numeric(w[which(R1>1),grep("sum_work_time",colnames(w))])+
        as.numeric(w[which(R1>1),grep("sum_waiting_time",colnames(w))])))/(RP[which(R1>1)]+as.numeric(w[which(R1>1),grep("sum_would_be_not_assigned",colnames(w))]))
    R1[which(R1>1)]=1
    
    R[which(R<0)]=0
    
    Rt=NULL
    for(h in 1:nrow(w)){
      if(as.numeric(w[h,grep("raw_not_assigned_activities2",colnames(w))])==0){
        Rt[h]=as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])/(as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])+
                                                                        as.numeric(w[h,grep("raw_travel_time_sum",colnames(w))]))
print(paste("case1", h, Rt[h]))
      
      }
        else if(as.numeric(w[h, grep("info_sum_assigned",colnames(w))])/AA[h]>
                  as.numeric(w[h, grep("sum_not_assigned",colnames(w))])/as.numeric(w[h, grep("raw_not_assigned_activities2",colnames(w))] )){
#         Rt[h]=(as.numeric(w[h, grep("info_sum_assigned",colnames(w))])/as.numeric(w[h,grep("raw_work_time_sum",colnames(w))]))/
#           (as.numeric(w[h, grep("info_sum_assigned",colnames(w))])/as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])+
#              as.numeric( w[h, grep("sum_waiting_time",colnames(w))])/as.numeric(w[h,grep("raw_waiting_time_sum",colnames(w))])+
#              as.numeric(w[h, grep("sum_travel_time",colnames(w))])/as.numeric(w[h,grep("raw_travel_time_sum",colnames(w))]))
        Rt[h]=as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])/(as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])+
                                                                        as.numeric(w[h,grep("raw_travel_time_sum",colnames(w))])+
                                                                        as.numeric(w[h,grep("raw_waiting_time_sum",colnames(w))]))
        print(paste("case2", h, Rt[h]))
      }
      else if(as.numeric(w[h,grep("raw_not_assigned_activities2",colnames(w))])==
        as.numeric(w[h,grep("raw_not_assigned_activities_unacceptable_travel_time",colnames(w))])
        #,
        #as.numeric(w[h,grep("raw_not_assigned_activities_unacceptable_travel_time",colnames(w))]),
        #as.numeric(w[h,grep("raw_not_assigned_activities_unacceptable_travel_time",colnames(w))]),))
      ){
        Rt[h]=(as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])#+ as.numeric(w[h,grep("raw_travel_time_sum",colnames(w))])
               )/
          sum(as.numeric(w[h,grep("raw_work_time_sum",colnames(w))]),
                                                                         as.numeric(w[h,grep("raw_waiting_time_sum",colnames(w))]),
                                                                         as.numeric(w[h,grep("raw_travel_time_sum",colnames(w))]))
      }
      else {
        Rt[h]=as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])/sum(as.numeric(w[h,grep("raw_work_time_sum",colnames(w))]),
                                                                         as.numeric(w[h,grep("raw_waiting_time_sum",colnames(w))]),
                                                                         as.numeric(w[h,grep("raw_travel_time_sum",colnames(w))]))
        print(paste("case3", h, Rt[h]))   
      }
      
     
    }
    
#     Rt=(as.numeric(w[,grep("raw_work_time_sum",colnames(w))])-
#       (#as.numeric(w[,grep("raw_overdue_time_sum",colnames(w))])+
#              as.numeric(w[,grep("raw_waiting_time_sum",colnames(w))])
#            +as.numeric(w[,grep("raw_travel_time_sum",colnames(w))])
#          #  +as.numeric(w[,grep("raw_sla_violation_sum",colnames(w))])
#          #  +as.numeric(w[,grep("raw_overtime_soft_sum",colnames(w))])
#         #   +as.numeric(w[,grep("raw_overtime_hard_sum",colnames(w))])
#        ))/(as.numeric(w[,grep("raw_work_time_sum",colnames(w))]))
    #AL=R/RP
AL=1-R1
    RAA=Rt
  }
  
  WT=as.numeric(w[,grep("raw_work_time_sum",colnames(w))])+as.numeric(w[,grep("raw_travel_time_sum",colnames(w))])+as.numeric(w[,grep("raw_waiting_time_sum",colnames(w))])
  RD=as.numeric(w[,grep("raw_work_time_sum",colnames(w))])/WT
# Overtime in minutes
Ov=(as.numeric(w[,grep("raw_overtime_soft_sum",colnames(w))])+as.numeric(w[,grep("raw_overtime_hard_sum",colnames(w))]))/as.numeric(w[,grep("providers_used",colnames(w))])
  OvWt=Ov/(WT/as.numeric(w[,grep("providers_used",colnames(w))]))

#OvWt[which(OvWt>1)]=1
#Overtime in costs
OVtC=  (as.numeric(w[,grep("sum_overtime_soft",colnames(w))])+
          as.numeric(w[,grep("sum_overtime_hard",colnames(w))]))/as.numeric(w[,grep("providers_used",colnames(w))])
OVtCW=OVtC/((as.numeric(w[,grep("sum_work_time",colnames(w))])+as.numeric(w[,grep("sum_travel_time",colnames(w))]))/as.numeric(w[,grep("providers_used",colnames(w))]))
OvWt[which(OvWt>1)]=OVtCW[which(OvWt>1)]
OvWt[which(OvWt>1)]=1
WT=WT/as.numeric(w[,grep("providers_used",colnames(w))])
  RSAA=RP/(RP+as.numeric(w[,grep("sum_not_assigned",colnames(w))])) # % of assigned cost
  RSLA=((as.numeric(w[,grep("sum_overdue_time",colnames(w))])+
      as.numeric(w[,grep("sum_travel_time",colnames(w))])+
      as.numeric(w[,grep("sum_sla_violation",colnames(w))])+
      as.numeric(w[,grep("sum_overtime_soft",colnames(w))])+
      as.numeric(w[,grep("sum_overtime_hard",colnames(w))])+
        as.numeric(w[,grep("sum_waiting_time",colnames(w))])+
        as.numeric(w[,grep("sum_work_time",colnames(w))])))/RP# % of losses
RCD= as.numeric(w[,grep("sum_work_time",colnames(w))])/(#as.numeric(w[,grep("_sum_overdue_time",colnames(w))])+
                                                             as.numeric(w[,grep("sum_travel_time",colnames(w))])+
                                                            # as.numeric(w[,grep("_sum_sla_violation",colnames(w))])+
                                                             as.numeric(w[,grep("sum_overtime_soft",colnames(w))])+
                                                             as.numeric(w[,grep("sum_overtime_hard",colnames(w))])+
                                                              as.numeric(w[,grep("sum_waiting_time",colnames(w))])+
                                                             as.numeric(w[,grep("sum_work_time",colnames(w))]))
  
  RPA=R/RP
Travel_as=(as.numeric(w[,grep("raw_travel_time_sum",colnames(w))])/as.numeric(w[,grep("providers_used",colnames(w))]))/
  sum((as.numeric(w[,grep("raw_travel_time_sum",colnames(w))])/as.numeric(w[,grep("providers_used",colnames(w))])),
      (as.numeric(w[,grep("raw_work_time_sum",colnames(w))])/as.numeric(w[,grep("providers_used",colnames(w))])),
      (as.numeric(w[,grep("raw_waiting_time_sum",colnames(w))])/as.numeric(w[,grep("providers_used",colnames(w))])))
# print("SL")
# print(SlCt)
# print("RP")
# print(RP)
# print("AL")
# print(AL)
# print("R")
# print(R)
 R=data.frame(id_rgt=as.numeric(w[,grep("id_rgt",colnames(w))]),AA=AA,  AAs=AAs,OvC=OvCt, TrC=TrCt, SlC=SlCt, RP=RP, AL=AL, WT=WT, RD=RD, RCD=RCD, Ovt=Ov, OvWt=OvWt,RV=R, SNA=SNA, RAA=RAA,RSAA=RSAA, RSLA=RSLA, TRAS=Travel_as, stringsAsFactors=F)
  if (length(AL[which(AL>1 | AL<0)])>0) stop(paste("Incorrect value N=", length(AL[which(AL>1 | AL<0)]), " ind = ",which(AL>1 | AL<0), 
             "routing plan name ", w[AL[which(AL>1 | AL<0)],grep("routing_plan_name",colnames(w))],
             "id_rgt ", w[which(AL>1 | AL<0),grep("_id_rgt",colnames(w))]), call.=TRUE)

  return (replNAN(R))
}

getRelAs <- function(w, total, fin=TRUE, v=FALSE, orig_vec){
  rw <- getAssessments(w, total, v)
  rw=as.data.frame(rw)
#print(head(rw))
if(fin){
rw1=rw[, c(grep(orig_vec[1], colnames(rw)))]
for(j in 2:length(orig_vec)){
  rw1=cbind(rw1,rw[, c(grep(orig_vec[j], colnames(rw)))])
}
#print(head(rw1))
return (rw1)
}
else return (rw)
}

getRPdata <- function(w, total=NULL, v=TRUE){
  print(head(w))
  w=replNA(w)
  fn=which(as.numeric(w[,grep("_fitness",colnames(w))])==0)
  if(length(fn)>0)  w=w[-fn,] # remove the routs that don't provide any results
  if(is.null(total))
    total=as.numeric(w[,grep("raw_total_activities",colnames(w))])
  else if(length(total)==1)  total=replNA(total)
  
  if (length(which(as.numeric(w[,grep("raw_assigned_activities",colnames(w))])==0))>0)
    w=w[-which(as.numeric(w[,grep("raw_assigned_activities",colnames(w))])==0),]
  pr=as.numeric(w[,grep("providers_used",colnames(w))])
  TrPr=as.numeric(w[,grep("raw_travel_time_sum",colnames(w))])/pr
  WrtPr=as.numeric(w[,grep("raw_work_time_sum",colnames(w))])/pr
  WtPr=as.numeric(w[,grep("raw_waiting_time_sum",colnames(w))])/pr
  total=as.numeric(w[,grep("raw_total_activities",colnames(w))])
  AA=as.numeric(w[,grep("raw_assigned_activities",colnames(w))])
  AAPr=AA/pr
  OvPr=(as.numeric(w[,grep("raw_overtime_soft_sum",colnames(w))])+as.numeric(w[,grep("raw_overtime_hard_sum",colnames(w))]))/pr
  id=as.numeric(w[,grep("id_rgt",colnames(w))])
  print(head(id))
  
  print(length(id))
  print(length(AAPr))

  R=data.frame(id_rgt=id, AA=AAPr, WrtPr=WrtPr, TrPr=TrPr, WtPr=WtPr, OvPr=OvPr, PR=pr, stringsAsFactors=F)  
  return (replNAN(R))
}


compapAs <- function(wl, max_not_ass){
  rws=getRelAs(ws)
  rwi=getRelAs(wi)
  idr=c(1,0,1,0)
  rws$AL=rws$AL/max(rws$AL,rwi$AL)
  rwi$AL=rwi$AL/max(rws$AL,rwi$AL)
  c1=NULL
  c2=NULL
  for(j in 1:nrow(rws)){
c1=c(c1,cor(as.numeric(rws[j,]), idr))
c2=c(c2,cor(as.numeric(rwi[j,]), idr))
}
D=cbind(c1=c1, c2=c2)
return (as.data.frame(D))
}

draw.As <- function(company){
  vmfd=read.csv(paste(company,"_FD.csv", sep=''), sep=',')
  assess=c("AA_","OvC_","TrC_","SlC_","AL_","WT_","RD_","Ovt_")
  assessN=c("Number of assigned activities","Overdue cost","Travel cost","SLA violation cost","Summary relative cost","Work time","Rout density","Overtime")
  vmfdS=data.frame()
  for(i in 1:length(assess)){
    if(i==1) {
      vmfdS=vmfd[,which(colnames(vmfd) %in% colnames(vmfd)[grep(assess[i], colnames(vmfd))])]
    }
    else{vmfdS=cbind(vmfdS, vmfd[,which(colnames(vmfd) %in% colnames(vmfd)[grep(assess[i], colnames(vmfd))])])}}
  par(mfrow=c(2,2))
  k=1
  vmfdSs= sapply(vmfdS, median)
  for(i in 1:length(assess)){
    boxplot(vmfdS[,seq(k,(k+2))], col=c('red', 'blue', 'green'), main = paste(company,assessN[i]))
    #  barplot(vmfdSs[seq(k,(k+2))], col=c('red', 'blue', 'green'), main = paste(company,assessN[i]))
    k=k+3
  }
}
