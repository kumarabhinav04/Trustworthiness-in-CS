import numpy as np
from operator import itemgetter
from math import sqrt


EMSum=0
MVSum=0
MCCSum=0
MCCdict={};
MCCAvg={}
temp={}

##This loop is just to run one case 100 times
for loop in range(1,101):

    # Store all the unique workers
    workers=[]
    # Store all the unique tasks
    tasks=[]
    # Store all the unique labels
    labels=[]
    # Store the list of worker, task and labels in form of lists, replica of text file in form of list
    workerLabelTaskLists=[]
    #Dictionary of workers, Tasks and labels assigned, help to create labels
    workerLabelTaskDict={}

    # Number of Workers
    NoOfWorkers=0
    # Number of tasks
    NoOfTasks=0
    # Number of Labels
    NoOfLabels=0

    # Store the true labels for each object, gold standard one
    trueLabel=[]


    #fetch the true labels from the dataset, to measure the accuracy of the algorithm
    correctLabel=[]

    #variable to store MV and EM accuracy value
    MVAccuracy=0
    EMAccuracy=0


    #funtion to read the correct label which are R generated
    def fetchTrueLabelRGenerated():
        global correctLabel
        
        #file to read the review of the reviewers, get the true labels generated by R
        fileTrueLabel=open('C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\FinalData\\Case II\\Case'+str(loop)+'\\Truelabels.txt','r');

        # read the line
        line=fileTrueLabel.readline()

        # Iterate until row exist in file
        while line:
            arrTrueLab=line.strip().split('\t');
            #Change for array index for different categories
            correctLabel.append(arrTrueLab[0]);

            # read the line
            line=fileTrueLabel.readline();

    #Call the function
    fetchTrueLabelRGenerated();

    #Function to load the files and intialize all the values
    def loadFilesNInitializeValues():

        global workerLabelTaskLists
        global workerLabelTaskDict
        global workers
        global tasks
        global labels
        
        #file to read the review of the reviewers, if it is not labelled by worker it will be -1
        file=open('C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\FinalData\\Case II\\Case'+str(loop)+'\\WorkerLabel-Trojan.txt','r');

        #file=open('C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\WorkerLabel-Trojan.txt','r');

        

        # read the line
        line=file.readline()
        
        # Iterate until row exist in file
        while line:

            # get all the three values separated by '\t'
            arr=line.strip().split('\t');

            # Store the text file structure in form of lists
            workerLabelTaskList=[]
            workerLabelTaskList.append(arr[0]);
            workerLabelTaskList.append(arr[1]);
            workerLabelTaskList.append(arr[2]);
            workerLabelTaskLists.append(workerLabelTaskList);


            # dictionary to form Task:Label pair
            dict={}
            dict[arr[1]]=arr[2];
            
            
            # read the line
            line=file.readline();

            # Store the unique value of workers, tasks and labels
            if arr[0] not in workers:
                workers.append(arr[0])
                # if workerId not present add it to dict. with no value
                workerLabelTaskDict[arr[0]]={};
            # Add the Task:Label pair for WorkerId key
            workerLabelTaskDict[arr[0]].update(dict);
            if arr[1] not in tasks:
                tasks.append(arr[1])
            if arr[2] not in labels:
                labels.append(arr[2])

    #Call the function
    loadFilesNInitializeValues();

    # get the count of no. of workers, tasks and labels
    NoOfWorkers=len(workers);
    NoOfTasks=len(tasks);
    NoOfLabels=len(labels);

    #print(labels);



    # To Do: Fetch the true label from file(object \t trueLabel) from file supplied, for other -1
    # Intitialize true labels, gold labels one

    def initializeTrueLabel():
        global NoOfTasks
        global trueLabel
        for t in range(NoOfTasks):
            trueLabel.append(-1);

    #Call the function
    initializeTrueLabel();

    # Declare and initialize confusion matrix with zero for each workers
    cMatrix=np.zeros((NoOfWorkers, NoOfLabels, NoOfLabels), float);

    def initializeConfusionMatrix():
        global cMatrix
        global NoOfWorkers
        global NoOfLabels
        # Initialize the confusion matrix for each workers, based on prior belief or assume each worker perfect
        for w in range(NoOfWorkers):
            for tl in range(NoOfLabels):
                for ol in range(NoOfLabels):
                    # if true label=observered label, 100%(1.0) else default 0.0
                    if tl==ol:
                        cMatrix[w,tl,ol]=0.8;
                    else:
                        cMatrix[w,tl,ol]=format(0.2/(NoOfLabels-1),'.3f')


    #Call the function
    initializeConfusionMatrix();

    # Initialize priors for each class, assume equal priors for all labels
    # This will be different in our case
    priors=np.zeros(NoOfLabels,float);

    def initializePriors():
        for l in range(NoOfLabels):
            if(labels[l]==0):
                priors[l]=0.5;
            else:
                priors[l]=0.5;
            #priors[l]=1.0/NoOfLabels;
              


    #Call the function
    initializePriors();


    # Initialize the labels l[worker][object]
    workerLabel=np.zeros((NoOfWorkers,NoOfTasks),int);

    def assignWorkerLabel():
        global NoOfWorkers
        global NoOfTasks
        global workerLabel
        
        for w in range(NoOfWorkers):
            for t in range(NoOfTasks):
                workerLabel[w][t]=workerLabelTaskDict[workers[w]][tasks[t]];

    #Call the function
    assignWorkerLabel();

    # Initialize class probabilities for every object for each class of that task, p(t)
    # for particular tasks , for eevry class-> count(class)/total label

    classProbab=np.empty((NoOfTasks,NoOfLabels),float);

    def initializeClassProbability():
        global NoOfTasks
        global NoOfLabels
        global trueLabel
        global classProbab
        
        for t in range(NoOfTasks):
            # if the true label is available for that object then no need to do anything, go for the next object
            # if true label for that object is there, so check what is the true label that will have 1.0 , others 0.0
            if trueLabel[t]!=-1:
                for l in range(NoOfLabels):
                    if trueLabel[t]==l:
                        classProbab[t][l]=1.0;
                    else:
                        classProbab[t][l]=0.0;
            # if true label not available
            else:
                #get the column vector for each task
                taskLabelVector=workerLabel[:,t];
                #print(taskLabelVector.tolist());
                #count the occurence of each element
                countLabelOccurence=[]
                for il in range(NoOfLabels):
                    labelValue=int(labels[il]);
                    countLabelOccurence.append(taskLabelVector.tolist().count(labelValue));

                # totalCount, excluding those labels which are labelled -1
                totalCount=len(taskLabelVector)-taskLabelVector.tolist().count(-1);
                    
                for il in range(NoOfLabels):
                    classProbabValue=(float(countLabelOccurence[il])/float(totalCount))
                    classProbab[t][il]=format(classProbabValue,'.2f');

    #Call the function
    initializeClassProbability();

    majorityVector=[];
    # Store the majority class for each task
    majorityClass=np.empty((NoOfTasks,1),int);
    #Store the labels majority labels for each tasks based on Majority Voting in form of dictionary {Task:Label}
    MVdict={};

    def findMajorityClass():
        global majorityVector
        global majorityClass
        global MVdict
        global NoOfTasks

        
        finalMaxIndex=-1;
        #get the true label for each object task on the majority voting, which have high class probability
        for t in range(NoOfTasks):
            majorityVector=classProbab[t,:].tolist();
            maxValue=max(majorityVector)
            # to get the all indices which have maximum value, if duplicacy then return all the instances, return list

            maxIndex=[]
            maxIndex=[i for i, j in enumerate(majorityVector) if j == maxValue]
            #if more than 1 index, consider which have higher prior probab, get the prior probab of all the index
            if(len(maxIndex)>1):
                # find max prior and one maxIndex for that
                for labelIndex in maxIndex:
                    maxprior=-1.0;
                    
                    if priors[labelIndex]>=maxprior:
                        maxprior=priors[labelIndex];
                        finalMaxIndex=labelIndex;
            else:
                finalMaxIndex=maxIndex[0];

            #for each object, get the majority class
                #final maxIndex is the index of the class or labels
                #print(finalMaxIndex);
                #majorityClass[t,0]=labels[finalMaxIndex];
            majorityClass[t,0]=labels[finalMaxIndex];

            labelsFinalCorrect=[labels[finalMaxIndex],correctLabel[t]]
            MVdict[tasks[t]]=labelsFinalCorrect

    #Call the function
    findMajorityClass();

    def predictMVAccuracy():
        global MVdict
        global MVAccuracy
        misclassifiedCount=0
        for t in range(NoOfTasks):
            if(MVdict[tasks[t]][0]!=MVdict[tasks[t]][1]):
                misclassifiedCount=misclassifiedCount+1;

        MVAccuracy=(NoOfTasks-misclassifiedCount)/NoOfTasks;


    #Call the function
    predictMVAccuracy();



    ############################## David Skene Iteration part######################################

    # class prior estimation for each class i
    # No return value, updating the parameter of 
    def classPriorEstimation():
        for l in range(NoOfLabels):
            #Declared here to reset the value of piSummation for next label
            piSummation=0;
            for t in range(NoOfTasks):
                piSummation=piSummation+classProbab[t,l];
            priors[l]=piSummation/NoOfTasks;



    # Approach to estimate class probability
    # 1. Iterarte for every tasks
    # 2. Compute the class probabilty for each class or label for that particular object
    # 3. Note the prior for each class, already computed
    # 4. For every workers which have reviewed that tasks, get the value of the label assigned by the worker
    # 5. Get the misclassification error rate for that worker where the observed label is the assigned label by that worker


    def classProbabilityEstimation():

        #Update only when we don't have true labels for that object else do nothing
        #for all the tasks
        for t in range(NoOfTasks):
            if trueLabel[t]==-1:
                #for all the labels
                for l in range(NoOfLabels):
                    #Initialization for every label
                    num=1;
                    labelValueIndex=-1;
                    
                    classPriorValue=priors[l];
                    num=num*classPriorValue;
                    #for all the workers: To get the misclassification value
                    for w in range(NoOfWorkers):
                        #get the value of the label, this is the observed value for that worker
                        labelValue=workerLabel[w][t];
                        labelValueIndex=labels.index(str(labelValue));
                        #if the label is assigned by worker, else skip this worker
                        if labelValue!=-1:
                            #get the value of the mis-error rate
                            
                            misclassificationValue=cMatrix[w,l,labelValueIndex];
                            #print(misclassificationValue);
                            num=num*misclassificationValue;


                    # for denominator
                    # Sum for all the labels
                    sdenom=0;
                    for q in range(NoOfLabels):
                        proddenom=1;
                        classPriorValue=priors[q];
                        proddenom=classPriorValue;
                        #for all the workers:
                        for w in range(NoOfWorkers):
                            #get the value of the label, this is the observed value for that worker
                            #this labelValue is actualValue, get the index from labels list
                            labelValue=workerLabel[w][t];
                            labelValueIndex=labels.index(str(labelValue));
                            #if the label is assigned by worker, else skip this worker
                            if labelValue!=-1:
                                #get the value of the mis-error rate
                                misclassificationValue=cMatrix[w,q,labelValueIndex];
                                proddenom=proddenom*misclassificationValue;

                        #sum this for all the labels q
                        sdenom=sdenom+proddenom;

                    #assign the class probability value
                    if sdenom!=0:
                        classProbab[t][l]=num/sdenom;
                    else:
                        classProbab[t][l]=0;



    #Estimate the error rate for each workers
    def errorRateEstimation():

        cWorkerMatrix=np.zeros((NoOfLabels,NoOfLabels),float);
        
        denom=np.zeros((NoOfLabels),float);
        #for each worker, compute the error rate
        for w in range(NoOfWorkers):
            #get the confusion matrix for the worker
            cWorkerMatrix=cMatrix[w,:,:]
            #Iterate for each tasks, get the label given by that worker for that object, get the classProb for that object
            for t in range(NoOfTasks):
                #get the label assigned by the worker to the task
               
                labelValue=workerLabel[w][t];
                
                if labelValue==-1:
                    continue;
                labelValueIndex=labels.index(str(labelValue));


                #Calculate the numerator
                for l in range(NoOfLabels):
                    cWorkerMatrix[l,labelValueIndex]=cWorkerMatrix[l,labelValueIndex]+classProbab[t][l];

            denom=np.zeros((NoOfLabels),float);
            #Compute denominator
            for trL in range(NoOfLabels):
                for obL in range(NoOfLabels):
                    denom[trL]=denom[trL]+cWorkerMatrix[trL][obL];


            #Compute Confusion matrix for each worker
            for trL in range(NoOfLabels):
                for obL in range(NoOfLabels):
                    if denom[trL]!=0:
                        cMatrix[w,trL,obL]=cWorkerMatrix[trL,obL]/denom[trL];

                    else:
                        cMatrix[w,trL,obL]=0;




    #Algorithm
    #while the values not converged:
    # 1. Estimate the object class probabilities
    # 2. Based on that, compute the class priors
    # 3. Estimate the error rates for every workers based on that
    #Main method which calls the other method
    def DSApproach():
        count=1;
        while count<10:
            classProbabilityEstimation();
            classPriorEstimation();
            errorRateEstimation();
            count+=1;





    EMdict={};

    def trueLabelByEM():
        #find the actual true label determined by the EM algorithm and predict its accuracy 
        for t in range(NoOfTasks):
            tempCProbab=[]
            #get the class probability for each task
            tempCProbab=classProbab[t];
            #find the index for that max value
            maxIndex=np.argmax(tempCProbab);
            #get the value from labels for that index
            lblValue=labels[maxIndex];
            #Store the computed correct label and true label
            labelsFinalCorrect=[lblValue,correctLabel[t]]
            #Store taskId, Computed True Label, True Label
            EMdict[tasks[t]]=labelsFinalCorrect
                   
                
    #Print function to print true label based on EM voting for each task
    def predictEMAccuracy():
        global EMdict
        global NoOfTasks
        global EMAccuracy
        
        misclassifiedCount=0
        for t in range(NoOfTasks):
            if(EMdict[tasks[t]][0]!=EMdict[tasks[t]][1]):
                misclassifiedCount=misclassifiedCount+1;

        EMAccuracy=(NoOfTasks-misclassifiedCount)/NoOfTasks;



    #This function is used to save the results on text file
    def saveResultsToTextFile():
        fResult = open("C:\\Users\\kumar_a_abhinav\\Desktop\\Simulated Data\\FinalResult\\Results-Case1a.txt", "a");
        fResult.write('\n\n\n\nCategory->NonAd\n\n\n')
        fResult.write('******************************************************\n')
        fResult.write('True labels after Majority Voting\n\n\n')
        fResult.write('******************************************************\n')
        fResult.write('Tasks \t\t MVLabels \t\t True Labels \n')
        fResult.write('------------------------------------------------\n')
        tempMVResult=""
        for t in range(NoOfTasks):
            if(t<9):
                tempMVResult="%s\t\t\t%s\t\t%s\n" % (str(tasks[t]),str(MVdict[tasks[t]][0]),str(MVdict[tasks[t]][1]))
            else:
                tempMVResult="%s\t\t%s\t\t%s\n" % (str(tasks[t]),str(MVdict[tasks[t]][0]),str(MVdict[tasks[t]][1]))
            fResult.write(str(tempMVResult))
        mvAcc='\n\n\nMajority Voting Accruracy->'+str(MVAccuracy)+'\n'
        fResult.write(str(mvAcc))
        fResult.write('\n\n******************************************************\n')
        fResult.write('True labels after EM\n')
        fResult.write('*************************************************************\n')
        fResult.write('Tasks \t\t EMLabels \t\t True Labels \n')
        fResult.write('-------------------------------------------------\n')
        tempEMResult=""
        for t in range(NoOfTasks):
            if(t<9):
                tempEMResult="%s\t\t\t%s\t\t%s\n" % (str(tasks[t]),str(EMdict[tasks[t]][0]),str(EMdict[tasks[t]][1]))
            else:
                tempEMResult="%s\t\t%s\t\t%s\n" % (str(tasks[t]),str(EMdict[tasks[t]][0]),str(EMdict[tasks[t]][1]))
            fResult.write(str(tempEMResult))
        emAcc='\n\n\nEM Accruracy->'+str(EMAccuracy)+'\n'
        fResult.write(str(emAcc))
        fResult.write('\n\nClass Probability\n\n')
        fResult.write('*************************************************************\n')
        fResult.write(str(classProbab))
        fResult.write('\n\n\n\n')
        fResult.write('Confusion Matrix\n\n')
        fResult.write('*************************************************************\n')
        fResult.write(str(cMatrix)+'\n')
        fResult.close()
                


    #Not used for the Thesis paper, may use it in future
    #Computing TP,TN,FP,FN based on the true labels
    def MCC():
        TP=0;
        TN=0;
        FP=0;
        FN=0;
        parameters=[];
        #for each worker
        for w in range(NoOfWorkers):
            TP=0;
            TN=0;
            FP=0;
            FN=0;
            #for each task
            for t in range(NoOfTasks):
                #get the correct value for that task
                correctValue=int(correctLabel[t]);
                observedValue=int(workerLabel[w][t]);
                if correctValue==0 and observedValue==0:
                    TP=TP+1;
                
                elif correctValue==0 and observedValue==1:
                    FN=FN+1;

                elif correctValue==1 and observedValue==0:
                    FP=FP+1;

                elif correctValue==1 and observedValue==1:
                    TN=TN+1;

                
            mccCoeff=((TP*TN)-(FP*FN))/(sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))
            parameters=[TP,FN,FP,TN,mccCoeff];
            #print(workers[w])
            MCCdict[workers[w]]=parameters;



    #Call all the functions    
    DSApproach();        
    trueLabelByEM();        
    predictEMAccuracy();
    #saveResultsToTextFile();           
    MCC();
    #print('MV Accuracy->',MVAccuracy)
    #print('EM Accuracy->',EMAccuracy)
    
    for w in range(NoOfWorkers):
        if workers[w] in MCCAvg:
            MCCAvg[workers[w]]=MCCAvg[workers[w]]+MCCdict[workers[w]][4];
        else:
            MCCAvg[workers[w]]= MCCdict[workers[w]][4];
            


    #Temporary code
    EMSum=EMSum+EMAccuracy
    MVSum=MVSum+MVAccuracy    


                        
print('MV Average->',MVSum/100)
print('EM Average->',EMSum/100)
print(MCCAvg)




