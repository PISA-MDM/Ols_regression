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
                              returnJKreplicates = F) # don´t return replicate weights


# Object can be used in EdSurvey functions, if addAttributes = True
pisa.sel2 <- EdSurvey::getData(data = sdf,
                               varnames = c(id.vars,wt.vars,global.scales,control.vars,pv),
                              omittedLevels = F,
                              returnJKreplicates = TRUE, # Necessary to make functions work
                              addAttributes = T) # dataframe can be used for EdSurvey functions



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


####### mutate ST001D01T",#Grade  -  Tatjana #########
pisa.sel<- pisa.sel%>%
  mutate(st001d01t = as.numeric(st001d01t),
    st001d01t_ad = factor(case_when(st001d01t <= 9 ~ "Grade 7-9",
                                     st001d01t >= 10 ~ "Grade 10-12")))

head(pisa.sel$st001d01t_ad)

########
# Demonstrating difference between pisa.sel & pisa.sel2

lm.sdf(formula = pv1read ~ gcselfeff, data = pisa.sel) # Does not work
lm.sdf(formula = pv1read ~ gcselfeff, data = pisa.sel2) # Function can be used


######################################################
# Running regressiosn for global competence scales
#####################################################
global.scales

lm.gcselfeff <- lm.sdf(formula = pv1read ~ gcselfeff, data = sdf)
summary(lm.gcselfeff)
# Multiple R-squared: 0.105

#contourPlot(x=lm.gcselfeff$fitted.values, y = lm.gcselfeff$residuals[,1])

lm.awacom <- lm.sdf(formula = pv1read ~ awacom, data = sdf)
summary(lm.awacom)
# Multiple R-squared: 0.0795

lm.attimm <- lm.sdf(formula = pv1read ~ attimm, data = sdf)
summary(lm.attimm)
# Multiple R-squared: 0.0733

lm.respect <- lm.sdf(formula = pv1read ~ respect, data = sdf)
summary(lm.respect)
# Multiple R-squared: 0.0619


lm.gcaware <- lm.sdf(formula = pv1read ~ gcaware, data = sdf)
summary(lm.gcaware)
# Multiple R-squared: 0.0442


lm.globmind <- lm.sdf(formula = pv1read ~ globmind, data = sdf)
summary(lm.globmind)
# Multiple R-squared: 0.0328


lm.intcult <- lm.sdf(formula = pv1read ~ intcult, data = sdf)
summary(lm.intcult)
# Multiple R-squared: 0.0245

lm.cogflex <- lm.sdf(formula = pv1read ~ cogflex, data = sdf)
summary(lm.cogflex)
# Multiple R-squared: 0.0053

lm.perspect <- lm.sdf(formula = pv1read ~ perspect, data = sdf)
summary(lm.perspect)
# Multiple R-squared: 0.0049



# Using all scales
lm.global.scales <- lm.sdf(formula = pv1read ~ gcselfeff + gcaware + perspect + cogflex + awacom + intcult + respect +globmind +  attimm, data = sdf)
summary(lm.global.scales)
# Multiple R-squared: 0.188


#####################################################################
### Running regressions for control variables 
###################################################################
control.vars

# progn
levelsSDF(varnames = "progn", data = sdf)

lm.progn <- lm.sdf(formula = pv1read ~ progn, data = sdf)
summary(lm.progn)
# Multiple R-squared: 0.414

# st001d01t
# Grade
levelsSDF(varnames = "st001d01t", data = sdf)

lm.grade <- lm.sdf(formula = pv1read ~ st001d01t, data = sdf)
summary(lm.grade) # compared to baselevel grade 7
# Multiple R-squared: 0.1634

# st004d01t
# Gender
levelsSDF(varnames = "st004d01t", data = sdf)

lm.gender <- lm.sdf(pv1read ~ st004d01t, data = sdf)
summary(lm.gender)
# Multiple R-squared: 0.0152

# hisei
EdSurvey::summary2(variable = "hisei", data = sdf)

lm.hisei <- lm.sdf(formula = pv1read ~ hisei, data = sdf)
summary(lm.hisei)
# Multiple R-squared: 0.1361

# immig
levelsSDF(varnames = "immig", data = sdf)

lm.immig <- lm.sdf(pv1read ~ immig, data = sdf)
summary(lm.immig)
# Multiple R-squared: 0.0827

# repeatgrade
levelsSDF(varnames = "repeatgrade", data = sdf)

lm.repeatgrade <- lm.sdf(formula = pv1read ~ repeatgrade, data = sdf)
summary(lm.repeatgrade)
# Multiple R-squared: 0.0973

# sc048q01na - pct of students whose heritage language is different from test language
summary2(variable = "sc048q01na", data = sdf)

lm.sc048 <- lm.sdf(formula = pv1read ~ sc048q01na, data = sdf)
summary(lm.sc048)
# Multiple R-squared: 0.0879



######################################################
## Combined control variables #######################
#####################################################
control.vars

lm.control.vars <- lm.sdf(pv1read ~ progn + st001d01t + st004d01t + hisei + immig + repeatgrade, data = sdf )

summary(lm.control.vars)
# Multiple R-squared: 0.4673

###################### 
# Optional - needs to be aligned with Julia
# Using standardized regression coefficients
summary(lm.control.vars, src = TRUE)
#####################



## Effect of global competence after controlling for control.vars

lm.gcselfeff.controlled <- lm.sdf(pv1read ~ gcselfeff + progn + st001d01t + st004d01t + hisei + immig + repeatgrade, data = sdf )
summary(lm.gcselfeff.controlled)
# Effect still significant
# Multiple R-squared: 0.4663


# Regressing top 3 scales global competence
lm.global.controlled <- lm.sdf(pv1read ~ gcselfeff + awacom + attimm + + progn + st001d01t + st004d01t + hisei + immig + repeatgrade, data = sdf)

summary(lm.global.controlled)
# All three scales significant
# Multiple R-squared: 0.4665



#############################################################
# Checking into items of gcselfeff
###########################################################

# scale gcselfeff - Student's self-efficacy regardingglobal issues
items.gcselfeff <- c("st196q02ha",
                     "st196q03ha",
                     "st196q04ha",
                     "st196q05ha",
                     "st196q06ha",
                     "st196q07ha")

# Check variables
searchSDF("st196", data = sdf)

# Check levels of variables
levelsSDF( varnames = c("st196q02ha",
                        "st196q03ha",
                        "st196q04ha",
                        "st196q05ha",
                        "st196q06ha",
                        "st196q07ha"),
           data = sdf)


