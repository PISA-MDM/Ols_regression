## EdSurvey Regression analyses 
# Version only contains models for gcselfeff

library(EdSurvey)
library(lme4)
library(WeMix)
library(flexplot) # get statistics for hlm
library(tidyverse)
library(sjPlot) # For plotting models only lme4
library(jtools) # For plotting models only lme4


#####################################
# Read data 
###################################

sdf <- readPISA(path = "C:/Users/bergm/OneDrive/Dokumente/Applied Data Science/05_Frühjahr 2022/Project Consulting Course/Data/PISA/2018",countries="DEU")


global.scales <- c("GCSELFEFF",#Self-efficacy regarding global issues (WLE)
                   "GCAWARE",#Student's awareness of global issues (WLE)
                   "PERSPECT",#Perspective-taking (WLE)
                   "COGFLEX",#Cognitive flexibility/adaptability (WLE)
                   "AWACOM",#Awareness of intercultural communication (WLE)
                   "INTCULT",#Student's interest in learning about other cultures (WLE)
                   "RESPECT",#Respect for people from other cultures (WLE)
                   "GLOBMIND",#Global-mindedness (WLE)
                   "ATTIMM")
global.scales <- str_to_lower(global.scales)

pv <- c("PV1READ" , "PV2READ", "PV3READ", "PV4READ", "PV5READ" , "PV6READ", "PV7READ", "PV8READ", "PV9READ" , "PV10READ")
pv <- str_to_lower(pv)


id.vars <- c("cntschid","cntstuid")


wt.vars <- c("w_fstuwt", #FINAL TRIMMED NONRESPONSE ADJUSTED STUDENT WEIGHT
             "w_schgrnrabwt", #  GRADE NONRESPONSE ADJUSTED SCHOOL BASE WEIGHT
             "w_fstuwt_sch_sum") # Sum of W_FSTUW

control.vars <- c("ST001D01T",#Grade
                  "ST004D01T",#Student (Standardized) Gender
                  "HISCED",#Highest Education of parents (ISCED)
                  "HISEI",#Highest International Socio-Economic Index of Occupational Status
                  "PARED",#Index highest parental education in years of schooling
                  "IMMIG",#Index Immigration status
                  "ST127Q01TA",#Have you ever repeated a <grade>? At <ISCED 1>
                  "ST127Q02TA",#Have you ever repeated a <grade>? At <ISCED 2>
                  "repeatgrade",
                  "progn",  # School classification
                  "SC048Q01NA") # Percentage <national modal grade for 15-year-olds>: Students whose <heritage language> is different from <test language

control.vars <- str_to_lower(control.vars)

### Get Data

pisa.sel <- EdSurvey::getData(data = sdf,
                              varnames = c(id.vars,wt.vars,global.scales,control.vars,pv),
                              omittedLevels = F, # Do not drop omitted levels
                              returnJKreplicates = T) # Return replicate weights




# Object can be used in EdSurvey functions, if addAttributes = True
# pisa.sel2 <- EdSurvey::getData(data = sdf,
#                                varnames = c(id.vars,wt.vars,global.scales,control.vars,pv),
#                               omittedLevels = F,
#                               returnJKreplicates = TRUE, # Necessary to make functions work
#                               addAttributes = T) # dataframe can be used for EdSurvey functions


########### mutate progn -Tatjana ##############
attributes(pisa.sel$progn)$levels
class(pisa.sel$cntschid)
is.numeric(pisa.sel$progn)


pisa.sel<- pisa.sel%>%
  mutate(progn_ad = factor(case_when(progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACCESS TO UPPER SECONDARY; ACADEMIC EDUCATION" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: VOCATIONAL SCHOOL" ~ "7", #Berufsschule
                                     progn == "GERMANY: LOWER SECONDARY, SOME WITH ACCESS TO UPPER SECONDARY (SPECIAL EDUCATION)" ~ "1", #Förderschule
                                     progn == "GERMANY: UPPER SECONDARY (VOCATIONAL), QUALIFYING FOR SUBJECT-SPECIFIC TERTIARY EDUCATIO" ~ "4", #Gymnasium
                                     progn == "GERMANY: LOWER SECONDARY, SOME WITH ACCESS TO UPPER SECONDARY; BASIC GENERAL EDUCATION" ~ "2", #Hauptschule
                                     progn == "GERMANY: LOWER SECONDARY, EXPECTEDLY NO ACCESS TO UPPER; BASIC GENERAL EDUCATION" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER SECONDARY; EXTENSIVE GENERAL EDUCATION" ~ "3", #Realschule
                                     progn == "GERMANY: LOWER SECONDARY, EXPECTEDLY ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER SECONDARY; ACADEMIC EDUCATION (EXCLUSIVELY STU" ~ "4", #Gymnasium
                                     progn == "GERMANY: LOWER SECONDARY, NO ACCESS TO UPPER; BASIC GENERAL EDUCATION (STUDENTS OF DIFFE" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: UPPER SECONDARY (EXCLUSIVELY STUDENTS OF THE SAME TRACK [CF. KEY 4])" ~ "4", #Gymnasium
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION (STUDENTS OF DIFF" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACHIEVEMENT-BASED ACCESS TO UPPER SECONDARY (WIT" ~ "5", #Integrierte Gesamtschule
                                     progn == "GERMANY: LOWER SECONDARY WITH ACCESS TO UPPER (WALDORF SCHOOL)" ~ "5", #Integrierte Gesamtschule
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, NO ACCESS TO UPPER; BASIC GENERAL EDUCATION (DIF" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: PRE-VOCATIONAL TRAINING YEAR UPPER SECONDARY LEVEL" ~ "7", #Berufsschule
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION" ~ "6", # Schule mit mehreren Bildungsgängen
                                     progn == "GERMANY: VOCATIONAL SCHOOL UPPER SECONDARY LEVEL" ~ "7"))) #Berufsschule


# Relevel to baseline 2
pisa.sel$progn_ad <- relevel(pisa.sel$progn_ad, ref="2")

#is progn a factor?
class(pisa.sel$progn_ad)
#yes!

# 1. Förderschule (1), 
# 2. Hauptschule(2), 
# 3. Realschule(3), 
# 4. Gymnasium(4,5,21), 
# 5. Integrierte Gesamtschule(6-7,16-17), 
# 6. Schule mit mehreren Bildungsgängen(8-15) and 
# 7. Berufsschule (18-20)



### PROGN with German school names


pisa.sel<- pisa.sel%>%
  mutate(progn_de = factor(case_when(progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACCESS TO UPPER SECONDARY; ACADEMIC EDUCATION" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: VOCATIONAL SCHOOL" ~ "Berufsschule", #7
                                     progn == "GERMANY: LOWER SECONDARY, SOME WITH ACCESS TO UPPER SECONDARY (SPECIAL EDUCATION)" ~ "Förderschule", #1
                                     progn == "GERMANY: UPPER SECONDARY (VOCATIONAL), QUALIFYING FOR SUBJECT-SPECIFIC TERTIARY EDUCATIO" ~ "Gymnasium", # 4
                                     progn == "GERMANY: LOWER SECONDARY, SOME WITH ACCESS TO UPPER SECONDARY; BASIC GENERAL EDUCATION" ~ "Hauptschule", # 2
                                     progn == "GERMANY: LOWER SECONDARY, EXPECTEDLY NO ACCESS TO UPPER; BASIC GENERAL EDUCATION" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER SECONDARY; EXTENSIVE GENERAL EDUCATION" ~ "Realschule", # 3
                                     progn == "GERMANY: LOWER SECONDARY, EXPECTEDLY ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER SECONDARY; ACADEMIC EDUCATION (EXCLUSIVELY STU" ~ "Gymnasium", # 4
                                     progn == "GERMANY: LOWER SECONDARY, NO ACCESS TO UPPER; BASIC GENERAL EDUCATION (STUDENTS OF DIFFE" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: UPPER SECONDARY (EXCLUSIVELY STUDENTS OF THE SAME TRACK [CF. KEY 4])" ~ "Gymnasium", # 4
                                     progn == "GERMANY: LOWER SECONDARY, ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION (STUDENTS OF DIFF" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACHIEVEMENT-BASED ACCESS TO UPPER SECONDARY (WIT" ~ "Integrierte Gesamtschule", # 5
                                     progn == "GERMANY: LOWER SECONDARY WITH ACCESS TO UPPER (WALDORF SCHOOL)" ~ "Integrierte Gesamtschule", #5
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, NO ACCESS TO UPPER; BASIC GENERAL EDUCATION (DIF" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: PRE-VOCATIONAL TRAINING YEAR UPPER SECONDARY LEVEL" ~ "Berufsschule", # 7
                                     progn == "GERMANY: LOWER SECONDARY COMPREHENSIVE, ACCESS TO UPPER; EXTENSIVE GENERAL EDUCATION" ~ "Schule mit mehreren Bildungsgängen", # 6
                                     progn == "GERMANY: VOCATIONAL SCHOOL UPPER SECONDARY LEVEL" ~ "Berufsschule"))) # 7


# Relevel to baseline Hauptschule
pisa.sel$progn_de <- relevel(pisa.sel$progn_de, ref="Hauptschule")



####### mutate ST001D01T",#Grade  -  Tatjana #########
pisa.sel<- pisa.sel%>%
  mutate(#st001d01t = as.numeric(st001d01t),
    st001d01t_ad = factor(case_when(st001d01t <= 9 ~ "Grade 7-9",
                                    st001d01t >= 10 ~ "Grade 10-12")))

# Relevel to baseline Hauptschule
pisa.sel$st001d01t_ad <- relevel(pisa.sel$st001d01t_ad, ref="Grade 7-9")


# calculate school hisei
pisa.sel <- pisa.sel %>% group_by(cntschid) %>% mutate(avg_hisei = mean(hisei, na.rm = TRUE)) %>% ungroup()




##########################################################
######### Rebinding attributes to use EdSurvey functions
##########################################################
pisa.sel <- rebindAttributes(pisa.sel, sdf)





############################################################
###### Descriptive Statistics ###############################
###########################################################


# pv1read
summary2(data = pisa.sel, variable = "pv1read")


# Global competence scale
summary2(data = pisa.sel, variable = "gcselfeff")

# Gender - st004d01t
summary2(data = pisa.sel, variable = "st004d01t")

# Immig
summary2(data = pisa.sel, variable = "immig")

# Immig
summary2(data = pisa.sel, variable = "pv1read")

# st001d01t
summary2(data = pisa.sel, variable = "st001d01t")

# st001d01t_ad
summary2(data = pisa.sel, variable = "st001d01t_ad")

# repeatgrade
summary2(data = pisa.sel, variable = "repeatgrade")

# hisei
summary2(data = pisa.sel, variable = "hisei")

# Average hisei
summary2(data = pisa.sel, variable = "avg_hisei")

# sc048q01na - pct of students whose heritage language is different from test language
summary2(variable = "sc048q01na", data = pisa.sel)


# Progn_de
summary2(data = pisa.sel, variable = "progn_de")


##################################
## Prepare complete case analyis
#################################

#delete cases with missing values
omitted2018 <- getAttributes(sdf,'omittedLevels')

# save full dataset separately
pisa.full <- pisa.sel

# Create copy for regressions
pisa.sel2 <- pisa.sel


for (i in 1:ncol(pisa.sel2)) {
  pisa.sel2 <- pisa.sel2[!pisa.sel2[,i] %in% omitted2018,]
}


length(pisa.sel2$cntstuid) # 2034 obs

######################################################
## Run descriptive statistics for complete cases vs full dataset
#######################################################

# pisa.sel = full dataset
# pisa.sel2 = complete cases

# pv1read
summary2(data = pisa.sel2, variable = "pv1read", weightVar = NULL) 
summary2(data = pisa.sel, variable = "pv1read", weightVar = NULL)


# Global competence scale
summary2(data = pisa.sel2, variable = "gcselfeff", weightVar = NULL)
summary2(data = pisa.sel, variable = "gcselfeff", weightVar = NULL)


# Gender - st004d01t
summary2(data = pisa.sel2, variable = "st004d01t", weightVar = NULL)
summary2(data = pisa.sel, variable = "st004d01t", weightVar = NULL)


# Immig
summary2(data = pisa.sel2, variable = "immig", weightVar = NULL)
summary2(data = pisa.sel, variable = "immig", weightVar = NULL)


# st001d01t
summary2(data = pisa.sel2, variable = "st001d01t", weightVar = NULL)
summary2(data = pisa.sel, variable = "st001d01t", weightVar = NULL)


# st001d01t_ad
summary2(data = pisa.sel2, variable = "st001d01t_ad", weightVar = NULL)
summary2(data = pisa.sel, variable = "st001d01t_ad", weightVar = NULL)


# repeatgrade
summary2(data = pisa.sel2, variable = "repeatgrade", weightVar = NULL)
summary2(data = pisa.sel, variable = "repeatgrade", weightVar = NULL)


# hisei
summary2(data = pisa.sel2, variable = "hisei", weightVar = NULL)
summary2(data = pisa.sel, variable = "hisei", weightVar = NULL)


# Average hisei
summary2(data = pisa.sel2, variable = "avg_hisei", weightVar = NULL)
summary2(data = pisa.sel, variable = "avg_hisei", weightVar = NULL)


# sc048q01na - pct of students whose heritage language is different from test language
summary2(variable = "sc048q01na", data = pisa.sel2, weightVar = NULL)
summary2(variable = "sc048q01na", data = pisa.sel, weightVar = NULL)


# Progn_de
summary2(data = pisa.sel2, variable = "progn_de", weightVar = NULL)
summary2(data = pisa.sel, variable = "progn_de", weightVar = NULL)




