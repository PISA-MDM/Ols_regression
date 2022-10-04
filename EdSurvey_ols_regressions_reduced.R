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

# Path needs to be adjusted by user
sdf <- readPISA(path = "C:/Users/bergm/OneDrive/Dokumente/Applied Data Science/05_Frühjahr 2022/Project Consulting Course/Data/PISA/2018",countries="DEU")
#sdf <- readPISA(path = "C:/Users/isr/Desktop/Training IPSDS/Master project/pisa2018/data",countries="DEU")


global.scales <- c("GCSELFEFF")#Self-efficacy regarding global issues (WLE)
#  "GCAWARE",#Student's awareness of global issues (WLE)
# "PERSPECT",#Perspective-taking (WLE)
#  "COGFLEX",#Cognitive flexibility/adaptability (WLE)
#  "AWACOM",#Awareness of intercultural communication (WLE)
#  "INTCULT",#Student's interest in learning about other cultures (WLE)
#  "RESPECT",#Respect for people from other cultures (WLE)
#  "GLOBMIND",#Global-mindedness (WLE)
#  "ATTIMM")
global.scales <- str_to_lower(global.scales)

pv <- c("PV1READ" , "PV2READ", "PV3READ", "PV4READ", "PV5READ" , "PV6READ", "PV7READ", "PV8READ", "PV9READ" , "PV10READ")
pv <- str_to_lower(pv)


id.vars <- c("cntryid","cnt","cntschid","cntstuid")


wt.vars <- c("w_fstuwt", #FINAL TRIMMED NONRESPONSE ADJUSTED STUDENT WEIGHT
             "w_schgrnrabwt", #  GRADE NONRESPONSE ADJUSTED SCHOOL BASE WEIGHT
             "w_fstuwt_sch_sum") # Sum of W_FSTUW

control.vars <- c("ST001D01T",#Grade
                  "ST004D01T",#Student (Standardized) Gender
                  # "HISCED",#Highest Education of parents (ISCED)
                  "HISEI",#Highest International Socio-Economic Index of Occupational Status
                  #  "PARED",#Index highest parental education in years of schooling
                  "IMMIG",#Index Immigration status
                  #  "ST127Q01TA",#Have you ever repeated a <grade>? At <ISCED 1>
                  #  "ST127Q02TA",#Have you ever repeated a <grade>? At <ISCED 2>
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



############################################
# Demonstrating difference between pisa.sel & pisa.sel2

# lm.sdf(formula = pv1read ~ gcselfeff, data = pisa.sel) # Does not work without rebinding attribures first
# m.sdf(formula = pv1read ~ gcselfeff, data = pisa.sel2) # EdSurvey functions can be used, because of  returnJKreplicates = TRUE and  addAttributes = TRUE
###########################################


############################################
#### Start of data preparation ##############
###########################################



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
# hisei_gc = group-mean centered
pisa.sel <- pisa.sel %>% group_by(cntschid) %>% mutate(avg_hisei = mean(hisei, na.rm = TRUE),
                                                       hisei_gc = hisei - avg_hisei) %>% ungroup()


# Check group mean centering
pisa.hisei <- pisa.sel %>% select(hisei, avg_hisei, hisei_gc)

# Show school average hisei
pisa.sel %>% 
  group_by(cntschid) %>% 
  summarise(avg_hisei = mean(hisei, na.rm = TRUE)) %>% ungroup

##########################################################
######### Rebinding attributes to use EdSurvey functions
##########################################################
pisa.sel <- rebindAttributes(pisa.sel, sdf)


#############################################
# Test - get summary statistics for new variables

summary2(data = pisa.sel, variable = "progn_de")
summary2(data = pisa.sel, variable = "st001d01t_ad")
summary2(data = pisa.sel, variable = "avg_hisei")
# EdSurvey functions work fine


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


full.cases <- pisa.sel2
length(full.cases$cntstuid) # 2989 obs

# Number of schools
length(unique(pisa.sel2$cntschid))

# Descriptive statistics of full cases
t <- pisa.sel2 %>% group_by(cntschid) %>% summarize(number_stu = n()) %>% ungroup
summary(t$number_stu)
sd(t$number_stu)


# Create dummywt for HLM
pisa.sel2$dummywt <- 1




############################################
#### End of data preparation ##############
###########################################




######################################################
# Running regressions for global competence scales 
## with REDUCED DATASET !!!!
#####################################################
global.scales

lm.gcselfeff <- lm.sdf(formula = pv1read ~ gcselfeff, data = pisa.sel2)
summary(lm.gcselfeff)
# Multiple R-squared: 0.0858


# Using all scales
lm.global.scales <- lm.sdf(formula = pv1read ~ gcselfeff + gcaware + perspect + cogflex + awacom + intcult + respect +globmind +  attimm, data = pisa.sel2)
summary(lm.global.scales)
# Multiple R-squared: 0.1012


#####################################################################
### Running regressions for control variables 
###################################################################
control.vars

# progn
# levelsSDF(varnames = "progn", data = pisa.sel2)
# Original
lm.progn <- lm.sdf(formula = pv1read ~ progn, data = pisa.sel2)
summary(lm.progn)
# Multiple R-squared: 0.3526

## progn_de after recoding - Baseline Hauptschule
lm.progn_de <- lm.sdf(formula = pv1read ~ progn_de, data = pisa.sel2)
summary(lm.progn_de)
# Multiple R-squared: 0.3266
# Significant differences only to Realschule und Gymnasium


# st001d01t
# Grade
levelsSDF(varnames = "st001d01t", data = pisa.sel2)

lm.grade <- lm.sdf(formula = pv1read ~ st001d01t, data = pisa.sel2)
summary(lm.grade) # compared to baselevel grade 7
# Multiple R-squared: 0.123

 
lm.grade_ad<- lm.sdf(formula = pv1read ~ st001d01t_ad, data = pisa.sel2)
summary(lm.grade_ad) # compared to baselevel grade 7
# Multiple R-squared: 0.0815



# st004d01t
# Gender
levelsSDF(varnames = "st004d01t", data = pisa.sel2)

lm.gender <- lm.sdf(pv1read ~ st004d01t, data = pisa.sel2)
summary(lm.gender)
# Multiple R-squared: 0.0112

# hisei
EdSurvey::summary2(variable = "hisei", data = pisa.sel2)

lm.hisei <- lm.sdf(formula = pv1read ~ hisei, data = pisa.sel2)
summary(lm.hisei)
# Multiple R-squared: 0.1438

# immig
levelsSDF(varnames = "immig", data = pisa.sel2)

lm.immig <- lm.sdf(pv1read ~ immig, data = pisa.sel2)
summary(lm.immig)
# Multiple R-squared: 0.033

# repeatgrade
levelsSDF(varnames = "repeatgrade", data = pisa.sel2)

lm.repeatgrade <- lm.sdf(formula = pv1read ~ repeatgrade, data = pisa.sel2)
summary(lm.repeatgrade)
# Multiple R-squared: 0.0727

# sc048q01na - pct of students whose heritage language is different from test language
summary2(variable = "sc048q01na", data = pisa.sel2)

lm.sc048 <- lm.sdf(formula = pv1read ~ sc048q01na, data = pisa.sel2)
summary(lm.sc048)
# Multiple R-squared: 0.0816



######################################################
## Combined control variables #######################
#####################################################
control.vars

lm.control.vars <- lm.sdf(pv1read ~ progn_de + st001d01t_ad + st004d01t + hisei + immig + repeatgrade + sc048q01na, data = pisa.sel2 )

summary(lm.control.vars)
# Multiple R-squared:  0.4311


######################################################
## Effect of global competence after controlling for control.vars
######################################################


lm.gcselfeff.controlled <- lm.sdf(pv1read ~ gcselfeff + progn_de + st001d01t_ad + st004d01t + hisei + immig + repeatgrade + sc048q01na , data = pisa.sel2)
summary(lm.gcselfeff.controlled)
# Effect still significant
# Multiple R-squared: 0.449


###################### 
# Optional - needs to be aligned with Julia
# Using standardized regression coefficients
summary(lm.gcselfeff.controlled, src = TRUE)
#####################














###############################################
### Optional - Sensitivity analysis ###########
###############################################

######################################################
# 1. Create binay immig variable
######################################################


# Immigration dummy variable
pisa.sel$dummy.immig <- ifelse(pisa.sel$immig %in% c("SECOND-GENERATION",
                                                     "FIRST-GENERATION"), "MIGRATION-BACKROUND",
                               "NATIVE")
# Numeric immig dummy
pisa.sel$dummy.immig.num <- ifelse(pisa.sel$dummy.immig == "NATIVE",0,1)



######################################################
# 2. Create two dichotomous grade 9 / grade 10 dummy variables
######################################################


# Dummy variables for grade 9/10
pisa.sel <- pisa.sel %>% 
  mutate(G9 = ifelse(st001d01t == "GRADE 9",1,0),
         G9_fct = factor(G9,labels = c("Other","G9")),
         G10 = ifelse(st001d01t == "GRADE 10", 1,0),
         G10_fct = factor(G10, labels = c("Other","G10")))






####################################################
###################################################
# Subsetting Grades 9 & 10 only ##########################
##################################################

summary2(data = sdf, variable = "st001d01t")

# Estimates are weighted using the weight variable ‘w_fstuwt’
# st001d01t    N Weighted N Weighted Percent Weighted Percent SE
# 1   GRADE 7   23   3170.825       0.43145454          0.10509514
# 2   GRADE 8  426  59453.890       8.08989712          0.44595116
# 3   GRADE 9 2497 340709.202      46.36033755          1.02149356
# 4  GRADE 10 2412 323295.759      43.99088838          1.12557302
# 5  GRADE 11   91   8058.262       1.09648851          0.30919574
# 6  GRADE 12    2    227.338       0.03093391          0.02213777


#########################################################
# create new object with Grade 9 & 10 only
sdf2 <- subset(sdf, st001d01t == "GRADE 9" | st001d01t == "GRADE 10")



# Object can be used in EdSurvey functions, if addAttributes = True
pisa.sel2 <- EdSurvey::getData(data = sdf2,
                               varnames = c(id.vars,wt.vars,global.scales,control.vars,pv),
                               omittedLevels = F,
                               returnJKreplicates = TRUE, # Necessary to make functions work
                               addAttributes = T) # dataframe can be used for EdSurvey functions




#delete cases with missing values
omitted2018 <- getAttributes(sdf,'omittedLevels')

# save full dataset separately
pisa.full <- pisa.sel2


for (i in 1:ncol(pisa.sel2)) {
  pisa.sel2 <- pisa.sel2[!pisa.sel2[,i] %in% omitted2018,]
}


full.cases <- pisa.sel2$cntstuid
length(full.cases) # 1892

# Dataset further reduced to 1892 cases


######################################################
# Running regressions for global competence scales
#####################################################
#global.scales

lm.gcselfeff <- lm.sdf(formula = pv1read ~ gcselfeff, data = pisa.sel3)
summary(lm.gcselfeff)
# Multiple R-squared: 0.0748




# Using all scales
lm.global.scales <- lm.sdf(formula = pv1read ~ gcselfeff + gcaware + perspect + cogflex + awacom + intcult + respect +globmind +  attimm, data = pisa.sel2)
summary(lm.global.scales)
# Multiple R-squared: 0.1716


#####################################################################
### Running regressions for control variables 
###################################################################
control.vars

# progn
# levelsSDF(varnames = "progn", data = pisa.sel2)

lm.progn <- lm.sdf(formula = pv1read ~ progn, data = pisa.sel2)
summary(lm.progn)
# Multiple R-squared: 0.3319

###################################################################
### Only GRADE 9 and 10
# st001d01t
# Grade
#levelsSDF(varnames = "st001d01t", data = pisa.sel2)

lm.grade <- lm.sdf(formula = pv1read ~ st001d01t, data = pisa.sel2)
summary(lm.grade) # compared to baselevel grade 9

# Multiple R-squared: 0.0398


# st004d01t
# Gender
#levelsSDF(varnames = "st004d01t", data = pisa.sel2)

lm.gender <- lm.sdf(pv1read ~ st004d01t, data = pisa.sel2)
summary(lm.gender)
# Multiple R-squared: 0.0108

# hisei
#EdSurvey::summary2(variable = "hisei", data = pisa.sel2)

lm.hisei <- lm.sdf(formula = pv1read ~ hisei, data = pisa.sel2)
summary(lm.hisei)
# Multiple R-squared: 0.1243

# immig
#levelsSDF(varnames = "immig", data = pisa.sel2)

lm.immig <- lm.sdf(pv1read ~ immig, data = pisa.sel2)
summary(lm.immig)
# Multiple R-squared: 0.025

# repeatgrade
#levelsSDF(varnames = "repeatgrade", data = pisa.sel2)

lm.repeatgrade <- lm.sdf(formula = pv1read ~ repeatgrade, data = pisa.sel2)
summary(lm.repeatgrade)
# Multiple R-squared:  0.0161

# sc048q01na - pct of students whose heritage language is different from test language
summary2(variable = "sc048q01na", data = pisa.sel2)

lm.sc048 <- lm.sdf(formula = pv1read ~ sc048q01na, data = pisa.sel2)
summary(lm.sc048)
# Multiple R-squared:  0.0586
# sig only on .001



######################################################
## Combined control variables #######################
#####################################################
control.vars

lm.control.vars <- lm.sdf(pv1read ~ progn + st001d01t + st004d01t + hisei + immig + repeatgrade + sc048q01na, data = pisa.sel2 )

summary(lm.control.vars)
# Multiple R-squared: 0.3892

###################### 
# Optional - needs to be aligned with Julia
# Using standardized regression coefficients
# summary(lm.control.vars, src = TRUE)
#####################



## Effect of global competence after controlling for control.vars

lm.gcselfeff.controlled <- lm.sdf(pv1read ~ gcselfeff + progn + st001d01t + st004d01t + hisei + immig + repeatgrade + sc048q01na , data = pisa.sel2)
summary(lm.gcselfeff.controlled)
# Effect still significant
# Multiple R-squared: 0.4043






