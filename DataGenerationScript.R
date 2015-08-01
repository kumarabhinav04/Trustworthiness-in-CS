
#initialize the parameters for experiment
trojanProbab=0.2
nonAdProbab=0.7
nonCompProbab=0.5

noOfTasks=100
noOfExpertWorkers=0
eWorkerProbab=0.65
noOfBiasedWorkers=0
bWorkerProbabilty=0.2
noOfSpammers=0
rsWorkerProbab=0.5
noOfUniformSpammers=10
usWorkerProbab=0.9
noOfAdvColludedWorkers=0
acLeadWorkerProbab=0.2
acCopiedWorkerProbab=0.9
noOfNonAdvColludedWorkers=0
nacLeadWorkerProbab=0.7
nacCopiedWorkerProbab=0.9

maindir="D:/Crowdsourcing/Simulated Data/Case XI"


#No. to track the ids of the worker
wIds=1

#Store the ids of the worker
workerIds<-vector()
#Store the ids of the task
taskIds<-vector()


#Store the final Worker, Tasks and Labels data for each categories
WorkerTaskTLabelMergedDataFrame<-data.frame()
WorkerTaskNALabelMergedDataFrame<-data.frame()
WorkerTaskNCLabelMergedDataFrame<-data.frame()

WorkerTaskTLabelDataFrame<-data.frame()
WorkerTaskNALabelDataFrame<-data.frame()
WorkerTaskNCLabelDataFrame<-data.frame()

#Store the task's true labels for each category
trojan<-vector()
nonadr<-vector()
noncom<-vector()

#Store the true label for each category in form of data frame
task<-data.frame()


#path to save different files
trueLabelPath="C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\Data\\Truelabels.txt"


###########################################################################################
#########################Code to generate Tasks and True Labels############################
###########################################################################################

#Function generate task and true labels for each category
generateTaskandTrueLabels<-function(noOfTasks,filePath){

  #Generate TaskIds and store it
  for(taskLoop in 1:noOfTasks)
  {
    taskIds[taskLoop]<<-paste('Task',as.character(taskLoop),sep="",collapse=NULL)
    
  }
  
  taskIds
  
  #Generate tasks and true labels considering the prior probability for each category
  trojan<<-rbinom(noOfTasks,1,0.2)
  nonadr<<-rbinom(noOfTasks,1,0.7)
  noncom<<-rbinom(noOfTasks,1,0.5)
  
  
  
  #Store all the true labels for each category
  task <<- data.frame(trojan,nonadr,noncom)
  
  #print(task)
  
  #Store the dataframe in text file, it's not gold standard labels
  write.table(task,paste(filePath,"Truelabels.txt",sep=""),sep="\t",row.names=FALSE,col.names = FALSE)
}


###########################################################################################
#########################Code to generate Expert Labels####################################
###########################################################################################
#An expert workers are those who gives most of the answers correctly(High Accuracy)

genearateExpertWorkers<-function(noOfExpertWorkers,noOfTasks)
{
  
  #list store the response of each expert workers for Trojan(T), Non-Adh(NA) and Non-comp(NC)
  eWorkersTResponse<-list()
  eWorkersNAResponse<-list()
  eWorkersNCResponse<-list()
  
  #for each workers 'k' generate the labels
  for(k in 1:noOfExpertWorkers)
  {
    eWorker<-vector()
    
    #Generate Unique WorkerIds
    workerIds[wIds]<<-paste("Worker",as.character(wIds),sep="",collapse=NULL)
    
    for(w in 1:noOfTasks)
    {
      #store redundant workerIds for each response
      eWorker[w]=workerIds[wIds]
    }
    
    
    #Generate the random variable for expert
    expert<-rbinom(noOfTasks,1,eWorkerProbab)
    
    
    #store the response of labels of expert for Trojan
    eTResponse<-vector()
    for(i in seq_along(task$trojan)){
      if(expert[i] == TRUE)
        eTResponse[i]<-task$trojan[i]
      else
        eTResponse[i]<-!task$trojan[i]
    }
    
    eWorkersTResponse[[k]]=eTResponse
    
    #Store the labels of expert for non adherence
    eNAResponse<-vector()
    for(i in seq_along(task$nonadr)){
      if(expert[i] == TRUE)
        eNAResponse[i]<-task$nonadr[i]
      else
        eNAResponse[i]<-!task$nonadr[i]
    }
    
    eWorkersNAResponse[[k]]=eNAResponse
    
    #Store the labels of expert for non compliant
    eNCResponse<-vector()
    for(i in seq_along(task$noncom)){
      if(expert[i] == TRUE)
        eNCResponse[i]<-task$noncom[i]
      else
        eNCResponse[i]<-!task$noncom[i]
    }
    
    eWorkersNCResponse[[k]]=eNCResponse
    
    #Generate the response text separately for each category T,NA,NC
    trojanLabels<-eTResponse
    NAdhLabels<-eNAResponse
    NCmpLabels<-eNCResponse
    workers<-eWorker
    
    WorkerTaskTLabelDataFrame<<-data.frame(workers,taskIds,trojanLabels)
    WorkerTaskNALabelDataFrame<<-data.frame(workers,taskIds,NAdhLabels)
    WorkerTaskNCLabelDataFrame<<-data.frame(workers,taskIds,NCmpLabels)
    
    #Merged data frame for each category
    WorkerTaskTLabelMergedDataFrame<<-rbind(WorkerTaskTLabelMergedDataFrame,WorkerTaskTLabelDataFrame)
    WorkerTaskNALabelMergedDataFrame<<-rbind(WorkerTaskNALabelMergedDataFrame,WorkerTaskNALabelDataFrame)
    WorkerTaskNCLabelMergedDataFrame<<-rbind(WorkerTaskNCLabelMergedDataFrame,WorkerTaskNCLabelDataFrame)
    
    
    WorkerTaskTLabelMergedDataFrame
    
    #increment workerIds
    wIds<<-wIds+1
  }
  
  
  ######This part of code is optional, required only when you want to save it in CSV#######
  #convert list to data frame for writing it to CSV
  eWorkersTResponseCSV <- data.frame(matrix(unlist(eWorkersTResponse), nrow=noOfExpertWorkers, byrow=T))
  eWorkersNAResponseCSV <- data.frame(matrix(unlist(eWorkersNAResponse), nrow=noOfExpertWorkers, byrow=T))
  eWorkersNCResponseCSV <- data.frame(matrix(unlist(eWorkersNCResponse), nrow=noOfExpertWorkers, byrow=T))
  #write to CSV
#   write.csv(eWorkersTResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\ExpertTrojanResponse.csv")
#   write.csv(eWorkersNAResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\ExpertNonCompResponse.csv")
#   write.csv(eWorkersNCResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\ExpertNonAdhResponse.csv")
  
}

###########################################################################################
#########################Code to generate Biased Labels####################################
###########################################################################################

#Biased workers are those which gives malicious(Wrong) answers, if we flipped their answers we get the true labels

generateBiasedWorker<-function(noOfBiasedWorkers,noOfTasks)
{

  #list store the response of each biased workers for Trojan(T), Non-Adh(NA) and Non-comp(NC)
  bWorkersTResponse<-list()
  bWorkersNAResponse<-list()
  bWorkersNCResponse<-list()
  
  for(k in 1:noOfBiasedWorkers)
  {
    bWorker<-vector()
    
    #Generate Unique WorkerIds
    workerIds[wIds]<<-paste("Worker",as.character(wIds),sep="",collapse=NULL)
    
    for(w in 1:noOfTasks)
    {
      #store redundant workerIds for each response
      bWorker[w]=workerIds[wIds]
    }
    
    
    #Generate the random variable for biased worker(malicious) for Trojan
    biased<-rbinom(noOfTasks,1,bWorkerProbabilty)
    
    #store the response of labels of biased for Trojan
    bTResponse<-vector()
    for(i in seq_along(task$trojan)){
      if(biased[i] == TRUE)
        bTResponse[i]<-task$trojan[i]
      else
        bTResponse[i]<-!task$trojan[i]
    }
    
    bWorkersTResponse[[k]]=bTResponse
    
    #Store the labels of expert for non adherence
    bNAResponse<-vector()
    for(i in seq_along(task$nonadr)){
      if(biased[i] == TRUE)
        bNAResponse[i]<-task$nonadr[i]
      else
        bNAResponse[i]<-!task$nonadr[i]
    }
    
    bWorkersNAResponse[[k]]=bNAResponse
    
    #Store the labels of expert for non compliant
    bNCResponse<-vector()
    for(i in seq_along(task$noncom)){
      if(biased[i] == TRUE)
        bNCResponse[i]<-task$noncom[i]
      else
        bNCResponse[i]<-!task$noncom[i]
    }
    
    bWorkersNCResponse[[k]]=bNCResponse
    
    
    WorkerTaskTLabelMergedDataFrame
    
    #Generate the response text separately for each category T,NA,NC
    trojanLabels<-bTResponse
    NAdhLabels<-bNAResponse
    NCmpLabels<-bNCResponse
    workers<-bWorker
    
    WorkerTaskTLabelDataFrame<<-data.frame(workers,taskIds,trojanLabels)
    WorkerTaskNALabelDataFrame<<-data.frame(workers,taskIds,NAdhLabels)
    WorkerTaskNCLabelDataFrame<<-data.frame(workers,taskIds,NCmpLabels)
    
    WorkerTaskTLabelMergedDataFrame<<-rbind(WorkerTaskTLabelMergedDataFrame,WorkerTaskTLabelDataFrame)
    WorkerTaskNALabelMergedDataFrame<<-rbind(WorkerTaskNALabelMergedDataFrame,WorkerTaskNALabelDataFrame)
    WorkerTaskNCLabelMergedDataFrame<<-rbind(WorkerTaskNCLabelMergedDataFrame,WorkerTaskNCLabelDataFrame)
    
    
    
    #increment workerIds
    wIds<<-wIds+1
    
  }
  
  ######This part of code is optional, required only when you want to save it in CSV#######
  #convert list to data frame for saving it to CSV
  bWorkersTResponseCSV <- data.frame(matrix(unlist(bWorkersTResponse), nrow=noOfBiasedWorkers, byrow=T))
  bWorkersNAResponseCSV <- data.frame(matrix(unlist(bWorkersNAResponse), nrow=noOfBiasedWorkers, byrow=T))
  bWorkersNCResponseCSV <- data.frame(matrix(unlist(bWorkersNCResponse), nrow=noOfBiasedWorkers, byrow=T))
  #write to CSV
#   write.csv(bWorkersTResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\BiasedTrojanResponse.csv")
#   write.csv(bWorkersNAResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\BiasedNonCompResponse.csv")
#   write.csv(bWorkersNCResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\BiasedNonAdhResponse.csv")

}

###########################################################################################
#########################Code to generate Random Workers###################################
###########################################################################################

#Random spammers are those which gives answers randomly irrespective of true labels
#Marking random answers is independent of what true labels is

generateRandomSpammers<-function(noOfSpammers,noOfTasks)
{
  
  #list store the response of each random spammer workers for Trojan(T), Non-Adh(NA) and Non-comp(NC)
  rsWorkersTResponse<-list()
  rsWorkersNAResponse<-list()
  rsWorkersNCResponse<-list()
  
  for(k in 1:noOfSpammers)
  {
    
    rsWorker<-vector()
    
    #Generate Unique WorkerIds
    workerIds[wIds]<<-paste("Worker",as.character(wIds),sep="",collapse=NULL)
    
    for(w in 1:noOfTasks)
    {
      #store redundant workerIds for each response
      rsWorker[w]=workerIds[wIds]
    }
    
    
    #Generate the random variable for random spammers
    #Marking for each category is independent of each other
    
    tRspammers<-rbinom(noOfTasks,1,rsWorkerProbab)
    rsWorkersTResponse[[k]]=tRspammers
    
    
    naRspammers<-rbinom(noOfTasks,1,rsWorkerProbab)
    rsWorkersNAResponse[[k]]=naRspammers
    
    ncRspammers<-rbinom(noOfTasks,1,rsWorkerProbab)
    rsWorkersNCResponse[[k]]=ncRspammers
    
    #Generate the response text separately for each category T,NA,NC
    trojanLabels<-tRspammers
    NAdhLabels<-naRspammers
    NCmpLabels<-ncRspammers
    workers<-rsWorker
    
    WorkerTaskTLabelDataFrame<<-data.frame(workers,taskIds,trojanLabels)
    WorkerTaskNALabelDataFrame<<-data.frame(workers,taskIds,NAdhLabels)
    WorkerTaskNCLabelDataFrame<<-data.frame(workers,taskIds,NCmpLabels)
    
    WorkerTaskTLabelMergedDataFrame<<-rbind(WorkerTaskTLabelMergedDataFrame,WorkerTaskTLabelDataFrame)
    WorkerTaskNALabelMergedDataFrame<<-rbind(WorkerTaskNALabelMergedDataFrame,WorkerTaskNALabelDataFrame)
    WorkerTaskNCLabelMergedDataFrame<<-rbind(WorkerTaskNCLabelMergedDataFrame,WorkerTaskNCLabelDataFrame)
    
    
    
    #increment workerIds
    wIds<<-wIds+1
  }
  
  
  ######This part of code is optional, required only when you want to save it in CSV#######
  #convert list to data frame
  rsWorkersTResponseCSV <- data.frame(matrix(unlist(rsWorkersTResponse), nrow=noOfSpammers, byrow=T))
  rsWorkersNAResponseCSV <- data.frame(matrix(unlist(rsWorkersNAResponse), nrow=noOfSpammers, byrow=T))
  rsWorkersNCResponseCSV <- data.frame(matrix(unlist(rsWorkersNCResponse), nrow=noOfSpammers, byrow=T))
  #write to CSV
#   write.csv(rsWorkersTResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\RSpammerTrojanResponse.csv")
#   write.csv(rsWorkersNAResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\RSpammerNonCompResponse.csv")
#   write.csv(rsWorkersNCResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\RSpammerNonAdhResponse.csv")
  
}



###########################################################################################
#########################Code to generate Uniform spammers###################################
###########################################################################################

#Random spammers are those which gives same answers irrespective of true labels
#Marking uniform answers is independent of what true labels is

generateUniformSpammers<-function(noOfUniformSpammers,noOfTasks)
{
  
  #list store the response of each uniform spammer workers for Trojan(T), Non-Adh(NA) and Non-comp(NC)
  usWorkersTResponse<-list()
  usWorkersNAResponse<-list()
  usWorkersNCResponse<-list()
  
  for(k in 1:noOfUniformSpammers)
  {
    
    usWorker<-vector()
    
    #Generate Unique WorkerIds
    workerIds[wIds]<<-paste("Worker",as.character(wIds),sep="",collapse=NULL)
    
    for(w in 1:noOfTasks)
    {
      #store redundant workerIds for each response
      usWorker[w]=workerIds[wIds]
    }
    
    
    #Generate the random variable for random spammers
    #Marking for each category is independent of each other
    
    tUspammers<-rbinom(noOfTasks,1,usWorkerProbab)
    usWorkersTResponse[[k]]=tUspammers
    
    
    naUspammers<-rbinom(noOfTasks,1,usWorkerProbab)
    usWorkersNAResponse[[k]]=naUspammers
    
    ncUspammers<-rbinom(noOfTasks,1,usWorkerProbab)
    usWorkersNCResponse[[k]]=ncUspammers
    
    #Generate the response text separately for each category T,NA,NC
    trojanLabels<-tUspammers
    NAdhLabels<-naUspammers
    NCmpLabels<-ncUspammers
    workers<-usWorker
    
    WorkerTaskTLabelDataFrame<<-data.frame(workers,taskIds,trojanLabels)
    WorkerTaskNALabelDataFrame<<-data.frame(workers,taskIds,NAdhLabels)
    WorkerTaskNCLabelDataFrame<<-data.frame(workers,taskIds,NCmpLabels)
    
    WorkerTaskTLabelMergedDataFrame<<-rbind(WorkerTaskTLabelMergedDataFrame,WorkerTaskTLabelDataFrame)
    WorkerTaskNALabelMergedDataFrame<<-rbind(WorkerTaskNALabelMergedDataFrame,WorkerTaskNALabelDataFrame)
    WorkerTaskNCLabelMergedDataFrame<<-rbind(WorkerTaskNCLabelMergedDataFrame,WorkerTaskNCLabelDataFrame)
    
    
    
    #increment workerIds
    wIds<<-wIds+1
  }
  
  
  ######This part of code is optional, required only when you want to save it in CSV#######
  #convert list to data frame
  usWorkersTResponseCSV <- data.frame(matrix(unlist(usWorkersTResponse), nrow=noOfUniformSpammers, byrow=T))
  usWorkersNAResponseCSV <- data.frame(matrix(unlist(usWorkersNAResponse), nrow=noOfUniformSpammers, byrow=T))
  usWorkersNCResponseCSV <- data.frame(matrix(unlist(usWorkersNCResponse), nrow=noOfUniformSpammers, byrow=T))
  #write to CSV
  #   write.csv(usWorkersTResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\RSpammerTrojanResponse.csv")
  #   write.csv(usWorkersNAResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\RSpammerNonCompResponse.csv")
  #   write.csv(usWorkersNCResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\RSpammerNonAdhResponse.csv")
  
}




###########################################################################################
####################Code to generate Adversarial Colluded Workers##########################
###########################################################################################

#Adversial colluded workers are those which colludes to give wrong answers

generateAdvColludedWorker<-function(noOfAdvColludedWorkers,noOfTasks)
{

  aCollWorkersTResponse<-list()
  aCollWorkersNAResponse<-list()
  aCollWorkersNCResponse<-list()
  
  
  #Generate labels for one worker biased one, dependent on true labels
  
  aColluded<-rbinom(noOfTasks,1,acLeadWorkerProbab)
  
  #store the response of labels of Adversarial Colluded for Trojan
  aCollTResponse<-vector()
  for(i in seq_along(task$trojan)){
    if(aColluded[i] == TRUE)
      aCollTResponse[i]<-task$trojan[i]
    else
      aCollTResponse[i]<-!task$trojan[i]
  }
  
  aCollWorkersTResponse[[1]]=aCollTResponse
  
  #Store the labels of Adversarial Colluded for non adherence
  aCollNAResponse<-vector()
  for(i in seq_along(task$nonadr)){
    if(aColluded[i] == TRUE)
      aCollNAResponse[i]<-task$nonadr[i]
    else
      aCollNAResponse[i]<-!task$nonadr[i]
  }
  
  aCollWorkersNAResponse[[1]]=aCollNAResponse
  
  #Store the labels of Adversarial Colluded for non compliant
  aCollNCResponse<-vector()
  for(i in seq_along(task$noncom)){
    if(aColluded[i] == TRUE)
      aCollNCResponse[i]<-task$noncom[i]
    else
      aCollNCResponse[i]<-!task$noncom[i]
  }
  
  aCollWorkersNCResponse[[1]]=aCollNCResponse
  
  #Adding it to data frame----Check
  
  aCollWorker<-vector()
  
  #Generate Unique WorkerIds
  workerIds[wIds]<<-paste("Worker",as.character(wIds),sep="",collapse=NULL)
  
  for(w in 1:noOfTasks)
  {
    #store redundant workerIds for each response
    aCollWorker[w]=workerIds[wIds]
  }
  
  
  #Generate the response text separately for each category T,NA,NC
  trojanLabels<-aCollTResponse
  NAdhLabels<-aCollNAResponse
  NCmpLabels<-aCollNCResponse
  workers<-aCollWorker
  
  WorkerTaskTLabelDataFrame<<-data.frame(workers,taskIds,trojanLabels)
  WorkerTaskNALabelDataFrame<<-data.frame(workers,taskIds,NAdhLabels)
  WorkerTaskNCLabelDataFrame<<-data.frame(workers,taskIds,NCmpLabels)
  
  WorkerTaskTLabelMergedDataFrame<<-rbind(WorkerTaskTLabelMergedDataFrame,WorkerTaskTLabelDataFrame)
  WorkerTaskNALabelMergedDataFrame<<-rbind(WorkerTaskNALabelMergedDataFrame,WorkerTaskNALabelDataFrame)
  WorkerTaskNCLabelMergedDataFrame<<-rbind(WorkerTaskNCLabelMergedDataFrame,WorkerTaskNCLabelDataFrame)
  
  wIds<<-wIds+1
  
  
  
  #Others workers copy this worker with probabilty of 0.9
  #dependednt on label given by leader
  for(k in 2:noOfAdvColludedWorkers)
  {
    aCollWorker<-vector()
    
    #Generate Unique WorkerIds
    workerIds[wIds]<<-paste("Worker",as.character(wIds),sep="",collapse=NULL)
    
    for(w in 1:noOfTasks)
    {
      #store redundant workerIds for each response
      aCollWorker[w]=workerIds[wIds]
    }
    
    
    #Generate the random variable for other workers, adversarial copied
    aCopied<-rbinom(noOfTasks,1,acCopiedWorkerProbab)
    
    #store the response of labels of copied Adversarial Colluded for Trojan
    aCopiedTResponse<-vector()
    for(i in seq_along(aCollTResponse)){
      if(aCopied[i] == TRUE)
        aCopiedTResponse[i]<-aCollTResponse[i]
      else
        aCopiedTResponse[i]<-!aCollTResponse[i]
    }
    
    aCollWorkersTResponse[[k]]=aCopiedTResponse
    
    #Store the labels of copied Adversarial Colluded for non adherence
    aCopiedNAResponse<-vector()
    for(i in seq_along(aCollNAResponse)){
      if(aCopied[i] == TRUE)
        aCopiedNAResponse[i]<-aCollNAResponse[i]
      else
        aCopiedNAResponse[i]<-!aCollNAResponse[i]
    }
    
    aCollWorkersNAResponse[[k]]=aCopiedNAResponse
    
    #Store the labels of copied  for non compliant
    aCopiedNCResponse<-vector()
    for(i in seq_along(aCollNCResponse)){
      if(aCopied[i] == TRUE)
        aCopiedNCResponse[i]<-aCollNCResponse[i]
      else
        aCopiedNCResponse[i]<-!aCollNCResponse[i]
    }
    
    aCollWorkersNCResponse[[k]]=aCopiedNCResponse
    
    
    #Generate the response text separately for each category T,NA,NC
    trojanLabels<-aCopiedTResponse
    NAdhLabels<-aCopiedNAResponse
    NCmpLabels<-aCopiedNCResponse
    workers<-aCollWorker
    
    WorkerTaskTLabelDataFrame<<-data.frame(workers,taskIds,trojanLabels)
    WorkerTaskNALabelDataFrame<<-data.frame(workers,taskIds,NAdhLabels)
    WorkerTaskNCLabelDataFrame<<-data.frame(workers,taskIds,NCmpLabels)
    
    WorkerTaskTLabelMergedDataFrame<<-rbind(WorkerTaskTLabelMergedDataFrame,WorkerTaskTLabelDataFrame)
    WorkerTaskNALabelMergedDataFrame<<-rbind(WorkerTaskNALabelMergedDataFrame,WorkerTaskNALabelDataFrame)
    WorkerTaskNCLabelMergedDataFrame<<-rbind(WorkerTaskNCLabelMergedDataFrame,WorkerTaskNCLabelDataFrame)
    
    wIds<<-wIds+1
  }
  
  #convert list to data frame
  aCollWorkersTResponseCSV <- data.frame(matrix(unlist(aCollWorkersTResponse), nrow=noOfAdvColludedWorkers, byrow=T))
  aCollWorkersNAResponseCSV <- data.frame(matrix(unlist(aCollWorkersNAResponse), nrow=noOfAdvColludedWorkers, byrow=T))
  aCollWorkersNCResponseCSV <- data.frame(matrix(unlist(aCollWorkersNCResponse), nrow=noOfAdvColludedWorkers, byrow=T))
  #write to CSV
#   write.csv(aCollWorkersTResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\AColludedTrojanResponse.csv")
#   write.csv(aCollWorkersNAResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\AColludedNonCompResponse.csv")
#   write.csv(aCollWorkersNCResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\AColludedNonAdhResponse.csv")

}

###########################################################################################
####################Code to generate Non Adversarial Colluded Workers######################
###########################################################################################

#Non Adversial colluded workers are those which colludes to give right answers, motive to earn money without doing work

generateNonAdvColludedWorker<-function(noOfAdvColludedWorkers,noOfTasks)
{
  
  naCollWorkersTResponse<-list()
  naCollWorkersNAResponse<-list()
  naCollWorkersNCResponse<-list()
  
  
  #Generate labels for one worker biased one, dependent on true labels
  
  naColluded<-rbinom(noOfTasks,1,0.9)
  
  #store the response of labels of NOn Adversarial Colluded for Trojan
  naCollTResponse<-vector()
  for(i in seq_along(task$trojan)){
    if(naColluded[i] == TRUE)
      naCollTResponse[i]<-task$trojan[i]
    else
      naCollTResponse[i]<-!task$trojan[i]
  }
  
  naCollWorkersTResponse[[1]]=naCollTResponse
  
  #Store the labels of Non Adversarial Colluded for non adherence
  naCollNAResponse<-vector()
  for(i in seq_along(task$nonadr)){
    if(naColluded[i] == TRUE)
      naCollNAResponse[i]<-task$nonadr[i]
    else
      naCollNAResponse[i]<-!task$nonadr[i]
  }
  
  naCollWorkersNAResponse[[1]]=naCollNAResponse
  
  #Store the labels of Non Adversarial Colluded for non compliant
  naCollNCResponse<-vector()
  for(i in seq_along(task$noncom)){
    if(naColluded[i] == TRUE)
      naCollNCResponse[i]<-task$noncom[i]
    else
      naCollNCResponse[i]<-!task$noncom[i]
  }
  
  naCollWorkersNCResponse[[1]]=naCollNCResponse
  
  
  
  
  naCollWorker<-vector()
  
  #Generate Unique WorkerIds
  workerIds[wIds]=paste("Worker",as.character(wIds),sep="",collapse=NULL)
  
  for(w in 1:noOfTasks)
  {
    #store redundant workerIds for each response
    naCollWorker[w]=workerIds[wIds]
  }
  
  
  #Generate the response text separately for each category T,NA,NC
  trojanLabels<-naCollTResponse
  NAdhLabels<-naCollNAResponse
  NCmpLabels<-naCollNCResponse
  workers<-naCollWorker
  
  WorkerTaskTLabelDataFrame<<-data.frame(workers,taskIds,trojanLabels)
  WorkerTaskNALabelDataFrame<<-data.frame(workers,taskIds,NAdhLabels)
  WorkerTaskNCLabelDataFrame<<-data.frame(workers,taskIds,NCmpLabels)
  
  WorkerTaskTLabelMergedDataFrame<<-rbind(WorkerTaskTLabelMergedDataFrame,WorkerTaskTLabelDataFrame)
  WorkerTaskNALabelMergedDataFrame<<-rbind(WorkerTaskNALabelMergedDataFrame,WorkerTaskNALabelDataFrame)
  WorkerTaskNCLabelMergedDataFrame<<-rbind(WorkerTaskNCLabelMergedDataFrame,WorkerTaskNCLabelDataFrame)
  
  wIds<<-wIds+1
  
  
  
  #Others workers copy this worker with probabilty of 0.9
  #dependednt on label given by leader
  for(k in 2:noOfNonAdvColludedWorkers)
  {
    
    naCollWorker<-vector()
    
    #Generate Unique WorkerIds
    workerIds[wIds]=paste("Worker",as.character(wIds),sep="",collapse=NULL)
    
    for(w in 1:noOfTasks)
    {
      #store redundant workerIds for each response
      naCollWorker[w]=workerIds[wIds]
    }
    
    #Generate the random variable for other workers, non adversarial copied
    naCopied<-rbinom(noOfTasks,1,0.9)
    
    #store the response of labels of biased for Trojan
    naCopiedTResponse<-vector()
    for(i in seq_along(naCollTResponse)){
      if(naCopied[i] == TRUE)
        naCopiedTResponse[i]<-naCollTResponse[i]
      else
        naCopiedTResponse[i]<-!naCollTResponse[i]
    }
    
    naCollWorkersTResponse[[k]]=naCopiedTResponse
    
    #Store the labels of expert for non adherence
    naCopiedNAResponse<-vector()
    for(i in seq_along(naCollNAResponse)){
      if(naCopied[i] == TRUE)
        naCopiedNAResponse[i]<-naCollNAResponse[i]
      else
        naCopiedNAResponse[i]<-!naCollNAResponse[i]
    }
    
    naCollWorkersNAResponse[[k]]=naCopiedNAResponse
    
    #Store the labels of expert for non compliant
    naCopiedNCResponse<-vector()
    for(i in seq_along(naCollNCResponse)){
      if(naCopied[i] == TRUE)
        naCopiedNCResponse[i]<-naCollNCResponse[i]
      else
        naCopiedNCResponse[i]<-!naCollNCResponse[i]
    }
    
    naCollWorkersNCResponse[[k]]=naCopiedNCResponse
    
    
    #Generate the response text separately for each category T,NA,NC
    trojanLabels<-naCopiedTResponse
    NAdhLabels<-naCopiedNAResponse
    NCmpLabels<-naCopiedNCResponse
    workers<-naCollWorker
    
    WorkerTaskTLabelDataFrame<<-data.frame(workers,taskIds,trojanLabels)
    WorkerTaskNALabelDataFrame<<-data.frame(workers,taskIds,NAdhLabels)
    WorkerTaskNCLabelDataFrame<<-data.frame(workers,taskIds,NCmpLabels)
    
    WorkerTaskTLabelMergedDataFrame<<-rbind(WorkerTaskTLabelMergedDataFrame,WorkerTaskTLabelDataFrame)
    WorkerTaskNALabelMergedDataFrame<<-rbind(WorkerTaskNALabelMergedDataFrame,WorkerTaskNALabelDataFrame)
    WorkerTaskNCLabelMergedDataFrame<<-rbind(WorkerTaskNCLabelMergedDataFrame,WorkerTaskNCLabelDataFrame)
    
    wIds<<-wIds+1
  }
  
  #convert list to data frame
  naCollWorkersTResponseCSV <- data.frame(matrix(unlist(naCollWorkersTResponse), nrow=noOfNonAdvColludedWorkers, byrow=T))
  naCollWorkersNAResponseCSV <- data.frame(matrix(unlist(naCollWorkersNAResponse), nrow=noOfNonAdvColludedWorkers, byrow=T))
  naCollWorkersNCResponseCSV <- data.frame(matrix(unlist(naCollWorkersNCResponse), nrow=noOfNonAdvColludedWorkers, byrow=T))
  #write to CSV
#   write.csv(naCollWorkersTResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\NonAColludedTrojanResponse.csv")
#   write.csv(naCollWorkersNAResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\NonAColludedNonCompResponse.csv")
#   write.csv(naCollWorkersNCResponseCSV, file = "C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\NonAColludedNonAdhResponse.csv")
  
  
}



#Function to save data frame in text file
saveDataFrameinTextFile<-function(filePath){
  #Store the marked labels by workers for all category
  write.table(WorkerTaskTLabelMergedDataFrame,paste(filePath,"WorkerLabel-Trojan.txt",sep=""),sep="\t",row.names=FALSE,col.names = FALSE)
  write.table(WorkerTaskNALabelMergedDataFrame,paste(filePath,"WorkerLabel-NonAd.txt",sep=""),sep="\t",row.names=FALSE,col.names = FALSE)
  write.table(WorkerTaskNCLabelMergedDataFrame,paste(filePath,"WorkerLabel-NonCompl.txt",sep=""),sep="\t",row.names=FALSE,col.names = FALSE)
  
}






#function to call all the functions of the script
callFunctions<-function(noOfTasks,noOfExpertWorkers,noOfBiasedWorkers,noOfSpammers,noOfUniformSpammers,noOfAdvColludedWorkers,noOfNonAdvColludedWorkers,i)
{
  ######Section of code to generate multiple files for same scenario#######
  ####This is not required if not generating multiple folder###############
  subdir=paste("Case",as.character(i),sep="")
  dir.create(file.path(maindir, subdir), showWarnings = FALSE)  
  filePath<-paste(maindir,as.character("/"),subdir,as.character("/"),sep="")
  ########################################################################
  
  generateTaskandTrueLabels(noOfTasks,filePath)
  
  if(noOfExpertWorkers>0)
    genearateExpertWorkers(noOfExpertWorkers,noOfTasks)
  
  if(noOfBiasedWorkers>0)
    generateBiasedWorker(noOfBiasedWorkers,noOfTasks)
  
  if(noOfSpammers>0)
    generateRandomSpammers(noOfSpammers,noOfTasks)
  
  if(noOfUniformSpammers>0)
    generateUniformSpammers(noOfUniformSpammers,noOfTasks)
  
  if(noOfAdvColludedWorkers>0)
    generateAdvColludedWorker(noOfAdvColludedWorkers,noOfTasks)
  
  if(noOfNonAdvColludedWorkers>0)
    generateNonAdvColludedWorker(noOfNonAdvColludedWorkers,noOfTasks)
  
  saveDataFrameinTextFile(filePath)
}


#Call function to execute the rest of the function
#Loop is used to create multiple instance for every case
for(i in 1:100){
  ##Reset the value
  
  #####This code is used to reset the value#############
  wIds=1
  WorkerTaskTLabelMergedDataFrame<-data.frame()
  WorkerTaskNALabelMergedDataFrame<-data.frame()
  WorkerTaskNCLabelMergedDataFrame<-data.frame()
  
  WorkerTaskTLabelDataFrame<-data.frame()
  WorkerTaskNALabelDataFrame<-data.frame()
  WorkerTaskNCLabelDataFrame<-data.frame()
 ########################################################### 
  
  callFunctions(noOfTasks,noOfExpertWorkers,noOfBiasedWorkers,noOfSpammers,noOfUniformSpammers,noOfAdvColludedWorkers,noOfNonAdvColludedWorkers,i)
}
