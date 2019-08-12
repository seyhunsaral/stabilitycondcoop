# 01_process_raw_data.R
# The Stability of Conditional Cooperation
# Andreozzi, Ploner, Saral

# This is the file that gets the raw data from ./data/rawdata, processes and saves it to
# ./data/rds  : R table format
# ./data/csv  : CSV Format

# Once they are generated, there is no need to run the code again
# I use rds 

# In the end data folder should look like:
## |-- data
## |   |-- csv
## |   |   |-- decbehav.csv
## |   |   |-- quest.csv
## |   |   `-- subjects.csv
## |   |-- rawdata
## |   |   |-- 170516_1052.xls
## |   |   |-- 170516_1335.xls
## |   |   |-- 170518_1001.xls
## |   |   |-- 170518_1343.xls
## |   |   |-- 171120_1317.xls
## |   |   `-- 171120_1546.xls
## |   `-- rds
## |       |-- decbehav.rds
## |       |-- quest.rds
## |       `-- subjects.rds


library("here") # gets project dir
library("dplyr") 
library("tidyr")
library("zTree") 
library("stringr")
library("vipor") # Using for number2digits and digits2number functions


# Here package gets the right project directory
dir_proj <- here::here()
setwd(dir_proj)

# defined relative to the project folder dir_proj
# file.path method is platform independent
dir_codes <- file.path("code")
dir_data_raw <- file.path(".","data","rawdata")

# get functions
source(file.path(dir_codes,"functions.R"))


# raw data folder files
files_data_raw <- list.files(path=dir_data_raw,pattern = "[0-9]{6}_[0-9]{4}.xls$",recursive=TRUE)
num_data_raw <-length(files_data_raw)

# z-Tree tables
subjects<-data.frame() 
decbehav<-data.frame()
quest<-data.frame()


# Read z-Tree Tables
for (fileno in 1:num_data_raw) {
    current_file<-files_data_raw[fileno]
    current_file_wpath=file.path(dir_data_raw,current_file)

    currentdata <- zTreeTables(current_file_wpath,tables=c("globals","subjects","DECBEHAV"))

    # Since I defined Treatment only in globals, I have to gather it from there
    
    # Moreover since we ran control questions file before there were more than one Treatment column
    
    # We gather here the right one manually but


    #Note to self: Always put the treatment at every table of your data!
    treatmentno<-currentdata$globals[2,"Treatment.1"]

    # Creating subjects table
    subjectsTEMP<-select(currentdata$subjects,Date,Treatment,Period,Subject,Group,FirstPlayer,uc,ccL,ccM,ccH,Action,
                         opp,ouc,occL,occM,occH,buc_L,buc_M,buc_H,bccL_L,bccL_M,bccL_H,bccM_L,bccM_M,bccM_H,bccH_L,
                         bccH_M,bccH_H,oAction,EarningKept,EarningReceived,EarningGuess,EarningGame,
                         age,female,faculty,esp)

    
    subjectsTEMP$Treatment<-treatmentno
    subjectsTEMP$Session <- fileno
    subjects<-rbind(subjects,subjectsTEMP)

    #Creating quest table
    questTEMP <- select(filter(subjectsTEMP,age>0),Subject,age,female,faculty,esp)
    questTEMP$Treatment<-treatmentno
    questTEMP$Session <- fileno
    quest<-rbind(quest,questTEMP)

    #Creating decbehav table
    decbehavTEMP<-currentdata$DECBEHAV
    decbehavTEMP$Treatment<-treatmentno
    decbehavTEMP$Session <- fileno
    decbehav<-rbind(decbehav,decbehavTEMP)
}

## Subjects table modifications 
columns_subjects<-c("Treatment",
                    "Session",
                    "Period",
                    "Subject",
                    "Group",
                    "FirstPlayer",
                    "uc",
                    "ccL",
                    "ccM",
                    "ccH",
                    "Action",
                    "opp",
                    "ouc",
                    "occL",
                    "occM",
                    "occH",
                    "buc_L",
                    "buc_M",
                    "buc_H",
                    "bccL_L",
                    "bccL_M",
                    "bccL_H",
                    "bccM_L",
                    "bccM_M",
                    "bccM_H",
                    "bccH_L",
                    "bccH_M",
                    "bccH_H",
                    "oAction",
                    "EarningKept",
                    "EarningReceived",
                    "EarningGuess",
                    "EarningGame")


subjects <- subjects %>%
    subset(select=columns_subjects,Period>=5) %>% #First four periods are trials
    dplyr::rename(treatment = Treatment, # R(eadable) varnames
                  session = Session,
                  period = Period,
                  subject = Subject,
                  group = Group,
                  first_player = FirstPlayer,
                  action = Action,
                  opp_action = oAction,
                  earning_kept = EarningKept,
                  earning_received = EarningReceived,
                  earning_guess = EarningGuess,
                  earning_game = EarningGame)

subjects <- subjects %>%
    mutate(
        period=period-4, # First four periods are trials
        ucf=uc/50, # Convert transfers(0,50,100) to factorizible format (0,1,2) for L, M, H
        ccLf=ccL/50,
        ccMf=ccM/50,
        ccHf=ccH/50,
        oucf=ouc/50, # Convert transfers(0,50,100) to factorizible format (0,1,2) for L, M, H
        occLf=occL/50,
        occMf=occM/50,
        occHf=occH/50,
        subject_unq=treatment * 10000 + session* 1000+ subject,
        session_unq=treatment * 10000 + session,
        opp_unq = treatment * 10000 + session * 1000 + opp
    ) # Unique subject and session IDs


##Decbehav table (response times) modifications
columns_decbehav<-c("Treatment","Stage","Session","Period","Subject",
                    "uc","ccL","ccM","ccH","buc_L","buc_M","buc_H",
                    "bccL_L","bccL_M","bccL_H","bccM_L","bccM_M",
                    "bccM_H","bccH_L","bccH_M","bccH_H","SEND",
                    "CANCEL","CONFIRM","time")

decbehav<-decbehav%>%
    select(columns_decbehav)%>%
    dplyr::rename(treatment = Treatment,
                  session = Session,
                  period = Period,
                  stage = Stage,
                  subject = Subject,
                  send = SEND,
                  cancel = CANCEL,
                  confirm = CONFIRM) %>%
    gather(key=action, value=choice,uc:confirm) %>% # converts to long format
    filter(choice!=-1)                          %>% # -1 means clicks without final choices. we get rid of them
    arrange(session,period,stage,time)          %>%
    mutate(period=period-4, #first four are trials
           subject_unq= treatment * 10000 + session* 1000+ subject) # generate unique subject id

# Quest(ionnaire) table 
columns_quest<-c("Treatment","Session","Subject","age","female","faculty","esp")
quest<- quest %>%
    mutate(subject_unq=Treatment * 10000 + Session* 1000+ Subject) %>% #Unique ids
    dplyr::rename(subject = Subject)
                  

# factorization of variables
levels_tr<-c(0,1,2)
levelsn_tr<-c("L","M","H")
#names_gr<- c("S","CC","PCC","HS","IHS","UC","ICC") # Types Classification



# Creating type numbers. (3base number system to 10 number base system, for instance LML is 010 and would be type 3)
subjects<- subjects %>%
    mutate(cond_type=ccLf*9+ccMf*3+ccHf,
           opp_cond_type = occLf*9+occMf*3+occHf) %>%
    mutate(cond_class_wide=classify_type_vector(cond_type,classification="wide"),   # wide (see the appendix or the functions.R)
           cond_class_wide_o=classify_type_vector(cond_type,classification="wide_o"),   # this is the same as narrow but perfect/imperfect 
           cond_class_narrow=classify_type_vector(cond_type,classification="narrow"),
           opp_cond_class_wide=classify_type_vector(opp_cond_type,classification="wide"),   # wide (see the appendix or the functions.R)
           opp_cond_class_wide_o=classify_type_vector(opp_cond_type,classification="wide_o"),   # this is the same as narrow but perfect/imperfect 
           opp_cond_class_narrow=classify_type_vector(opp_cond_type,classification="narrow")
           )  # narrow (Fishcbacher,2001) definition (selfish,cond.coop, hump-shaped and other.)





#TreatmentNames
subjects <- subjects %>%
    mutate(treatment = treatment_convert(treatment)) %>% # get names from functions.R
    mutate(treatment = as.factor(treatment))
# Change the order of NoCondInfo and CondInfo (only for RDS files)
subjects  <- subjects  %>% mutate(treatment = factor(treatment, levels = c("NoCondInfo","CondInfo")))


# function to convert actions to L M H
factorize3l<-function(column) {
    column <- factor(column,order=TRUE,levels=levels_tr)
    levels(column)<-levelsn_tr
    return(column)
}

subjects<-mutate_at(subjects,.funs=funs(factorize3l),.vars=vars(ucf:ccHf))
subjects<- mutate_at(subjects,.funs=funs(./100),.vars=vars(buc_L:bccH_H))

# Factorizing period and cctypes
subjects$period_f<-factor(subjects$period,order=TRUE,levels=c(1:10))
#subjects$cond_type<-factor(subjects$cond_type, order=TRUE, levels=c(0:26))
subjects$cond_class_wide<-factor(subjects$cond_class_wide)
subjects$cond_class_narrow<-factor(subjects$cond_class_narrow)


# zTree(Kirchkamp) uses plyr while tidyverse uses dplyr which have common function names
# Either to detach or explicit calls
# I used the second option


# Expected payoffs
# If other is the first mover
subjects <- subjects %>%
    mutate(exp_pay_uc=buc_M * 150 + buc_H * 300,
           exp_pay_ccL=bccL_M * 150 + bccL_H *300 + 100,
           exp_pay_ccM=bccM_M * 150 + bccM_H *300 + 50,
           exp_pay_ccH=bccH_M * 150 + bccH_H *300)


## Lagged Variables
subjects<- subjects %>%
    group_by(subject_unq) %>%
    mutate(opp_action_lag1=dplyr::lag(opp_action,n=1,default=NA,order_by=subject_unq))%>%
    mutate(opp_cond_type_lag1=dplyr::lag(opp_cond_type,n=1,default=NA,order_by=subject_unq))%>%
    mutate(opp_cond_class_wide_lag1=dplyr::lag(opp_cond_class_wide,n=1,default=NA,order_by=subject_unq))%>%
    mutate(opp_cond_class_wide_o_lag1=dplyr::lag(opp_cond_class_wide_o,n=1,default=NA,order_by=subject_unq))%>%
    mutate(opp_cond_class_narrow_lag1=dplyr::lag(opp_cond_class_narrow,n=1,default=NA,order_by=subject_unq))%>%
    mutate(first_player_lag1 = dplyr::lag(first_player, n=1, default = NA, order_by = subject_unq))%>% 
    mutate(opp_action_lag1=replace(opp_action_lag1, period==1, NA))  %>%
    mutate(opp_cond_type_lag1=replace(opp_cond_type_lag1, period==1, NA))  %>%
    mutate(opp_cond_class_wide_lag1=replace(opp_cond_class_wide_lag1, period==1, NA))  %>%
    mutate(opp_cond_class_wide_o_lag1=replace(opp_cond_class_wide_o, period==1, NA))  %>%
    mutate(opp_cond_class_narrow_lag1=replace(opp_cond_class_narrow, period==1, NA))  %>% 
    mutate(first_player_lag1=replace(first_player_lag1, period==1, NA))



## Binary variables: is C(ond)C(oop)  , is S(elfish), is H(ump)S(haped)
subjects <- subjects %>% mutate(isCC =(cond_class_narrow=="cond-coop"),
                                isS= (cond_class_narrow=="selfish"),
                                isHS = (cond_class_narrow =="humped") )


write.csv(subjects,file = file.path(".","data","csv","subjects.csv"))
saveRDS(subjects,file = file.path(".","data","rds","subjects.rds"))

write.csv(quest,file = file.path(".","data","csv","quest.csv"))
saveRDS(quest,file = file.path(".","data","rds","quest.rds"))

write.csv(decbehav,file = file.path(".","data","csv","decbehav.csv"))
saveRDS(decbehav,file = file.path(".","data","rds","decbehav.rds"))


