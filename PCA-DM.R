# Data Management

library(readxl)

pca_DM <- read_xlsx("lec12/module2 task sheet.xlsx")

attach(pca_DM)
#View(pca_DM)

library(tidyverse)

#summary(pca_DM)
#apply(pca_DM, 2, table)

#unique(pca_DM$`Service  (in years)`)
#unique(pca_DM$`Do you feel that "differential treatment of nurses or physicians" in the hospital , interfere with nurse-physician interaction?`)

# fix typos
pca_DM$`Service  (in years)` <- recode(pca_DM$`Service  (in years)`, 'Z 8' = "8")
pca_DM$`Do you feel that "differential treatment of nurses or physicians" in the hospital , interfere with nurse-physician interaction?` <- 
  recode(pca_DM$`Do you feel that "differential treatment of nurses or physicians" in the hospital , interfere with nurse-physician interaction?`,
         'Rarley' = "Rarely")

# participant ch.ch

part_ch <- pca_DM[c(1:12)]

#colnames(pca_DM[,c(13:44)]) <- c("not angry", "not frustrated", "understood",
 #                            "respected", "pleased", "satisfied", "equal understanding",
  #                           "joyful talking", "receiving correct information",
   #                          
    #                         "mutual understanding", "maintain patient safety",
     #                        "same understanding", "schedule taken into account",
      #                       "openly exchange information", "show concern",
       #                      "help each other", "listen to each other",
        #                     "receive information or advice",
         #                    
          #                   "non compliance", "negligence of duty", "abuse",
           #                  "poor attitude", "uncooperativeness", "gender difference",
            #                 "unfavorable attitude to other profession",
             #                "poor interpersonal communication", "disruptive behavior",
              #               "differential treatment", "absence of forum",
               #              "lack of shared vision", "malfunctioning equipment",
                #             "frequent supply shortage")
# Why is not working?!!!

# respect and satisfaction

resp_satis <- pca_DM[c(13:21)]

colnames(resp_satis) <- c("not angry", "not frustrated", "understood",
                          "respected", "pleased", "satisfied", "equal understanding",
                          "joyful talking", "receiving correct information")
#apply(resp_satis, 2, unique)
#####
#rs_num <- recode(resp_satis$`Feeling not angry after nurse and physician interaction?` ,
 #                "Never" = 1,
  #               "Rarely" = 2,
   #              "Sometimes" = 3,
    #             "Usually" = 4,
     #            "Always" = 5) # no

#code_df <- function(x){
 # for (i in x[c(1:i)])
  #  recode(x[c(i)],
   #        "Never" = 1,
    #       "Rarely" = 2,
      #     "Sometimes" = 3,
      #     "Usually" = 4,
       #    "Always" = 5)
#} # NOT WORKING
# code_df(resp_satis)

# recoding

(resp_satis <- resp_satis %>% 
  mutate_at(names(resp_satis),
            ~ replace(., . %in% "Never", 1)))

(resp_satis <- resp_satis %>% 
  mutate_at(names(resp_satis),
            ~ replace(., . %in% "Rarely", 2)))

(resp_satis <- resp_satis %>% 
  mutate_at(names(resp_satis),
            ~ replace(., . %in% "Sometimes", 3)))

(resp_satis <- resp_satis %>% 
  mutate_at(names(resp_satis),
            ~ replace(.,. %in% "Usually", 4)))

(resp_satis <- resp_satis %>% 
  mutate_at(names(resp_satis),
            ~ replace(., . %in% "Always", 5)))

(resp_satis <- resp_satis %>% 
    mutate_at(names(resp_satis),
              ~ replace(., . %in% NA, 0)))

#(resp_satis <- apply(resp_satis, 2, as.numeric)) # BAD IDEA!!!

# convert to numeric

resp_satis <- mutate_all(resp_satis, function(x) as.numeric(as.character(x)))

str(resp_satis)

#%>% recode("Never" = 1,
 #                      "Rarely" = 2,
  #                     "Sometimes" = 3,
   #                    "Usually" = 4,
    #                   "Always" = 5)

# openness and sharing

# recoding

open_share <- pca_DM[c(22:30)]
colnames(open_share) <- c("mutual understanding", "maintain patient safety",
                          "same understanding", "schedule taken into account",
                          "openly exchange information", "show concern",
                          "help each other", "listen to each other",
                          "receive information or advice")

(open_share <- open_share %>% 
    mutate_at(names(open_share),
              ~ replace(., . %in% "Never", 1)))

(open_share <- open_share %>% 
    mutate_at(names(open_share),
              ~ replace(., . %in% "Rarely", 2)))

(open_share <- open_share %>% 
    mutate_at(names(open_share),
              ~ replace(., . %in% "Sometimes", 3)))

(open_share <- open_share %>% 
    mutate_at(names(open_share),
              ~ replace(.,. %in% "Usually", 4)))

(open_share <- open_share %>% 
    mutate_at(names(open_share),
              ~ replace(., . %in% "Always", 5)))

(open_share <- open_share %>% 
    mutate_at(names(open_share),
              ~ replace(., . %in% NA, 0)))

# convert to numeric

open_share <- mutate_all(open_share, function(x) as.numeric(as.character(x)))

str(open_share)

# associated factors with communication

#ass_factors <- pca_DM[c(1:12, 31:44)]
ass_factors <- pca_DM[c(31:44)]
colnames(ass_factors) <- c("non compliance", "negligence of duty", "abuse",
                           "poor attitude", "uncooperativeness", "gender difference",
                           "unfavorable attitude to other profession",
                           "poor interpersonal communication", "disruptive behavior",
                           "differential treatment", "absence of forum",
                           "lack of shared vision", "malfunctioning equipment",
                           "frequent supply shortage")

#View(ass_factors)
#apply(ass_factors, 2, table)
#apply(ass_factors, 2, unique)


(ass_factors <- ass_factors %>% 
    mutate_at(names(ass_factors),
              ~ replace(., . %in% "Never", 1)))

(ass_factors <- ass_factors %>% 
    mutate_at(names(ass_factors),
              ~ replace(., . %in% c("Rarely", "rarely"), 2)))

(ass_factors <- ass_factors %>% 
    mutate_at(names(ass_factors),
              ~ replace(., . %in% c("Sometimes", "sometimes"), 3)))

(ass_factors <- ass_factors %>% 
    mutate_at(names(ass_factors),
              ~ replace(.,. %in% "Usually", 4)))

(ass_factors <- ass_factors %>% 
    mutate_at(names(ass_factors),
              ~ replace(., . %in% "Always", 5)))

(ass_factors <- ass_factors %>% 
    mutate_at(names(ass_factors),
              ~ replace(., . %in% NA, 0)))

# convert to numeric

ass_factors <- mutate_all(ass_factors, function(x) as.numeric(as.character(x)))

str(ass_factors)

#######
# ALL

pcall <- pca_DM[c(13:44)]
colnames(pcall) <- c("not angry", "not frustrated", "understood",
                     "respected", "pleased", "satisfied", "equal understanding",
                     "joyful talking", "receiving correct information",
                     
                     "mutual understanding", "maintain patient safety",
                     "same understanding", "schedule taken into account",
                     "openly exchange information", "show concern",
                     "help each other", "listen to each other",
                     "receive information or advice",
                     
                     "non compliance", "negligence of duty", "abuse",
                     "poor attitude", "uncooperativeness", "gender difference",
                     "unfavorable attitude to other profession",
                     "poor interpersonal communication", "disruptive behavior",
                     "differential treatment", "absence of forum",
                     "lack of shared vision", "malfunctioning equipment",
                     "frequent supply shortage")

(pcall <- pcall %>% 
    mutate_at(names(pcall),
              ~ replace(., . %in% "Never", 1)))

(pcall <- pcall %>% 
    mutate_at(names(pcall),
              ~ replace(., . %in% c("Rarely", "rarely"), 2)))

(pcall <- pcall %>% 
    mutate_at(names(pcall),
              ~ replace(., . %in% c("Sometimes", "sometimes"), 3)))

(pcall <- pcall %>% 
    mutate_at(names(pcall),
              ~ replace(.,. %in% "Usually", 4)))

(pcall <- pcall %>% 
    mutate_at(names(pcall),
              ~ replace(., . %in% "Always", 5)))

(pcall <- pcall %>% 
    mutate_at(names(pcall),
              ~ replace(., . %in% NA, 0)))

pcall <- mutate_all(pcall, function(x) as.numeric(as.character(x)))
str(pcall)

