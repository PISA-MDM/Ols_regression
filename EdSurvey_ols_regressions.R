## Retrieving All Variables in a Dataset

library(EdSurvey)
library(lme4)
library(WeMix)
library(flexplot)
library(tidyverse)


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
                             "progn") # School classification %>% 

control.vars <- str_to_lower(control.vars)

### Get Data

pisa.sel <- EdSurvey::getData(data = sdf,
                               varnames = c(id.vars,wt.vars,global.scales,control.vars,pv),
                              omittedLevels = F, # Do not drop omitted levels
                              returnJKreplicates = F) # don´t return replicate weights


 
pisa.sel2 <- EdSurvey::getData(data = sdf,
                               varnames = c(id.vars,wt.vars,global.scales,control.vars,pv),
                              omittedLevels = F,
                              returnJKreplicates = TRUE, # Necessary to make functions work
                              addAttributes = T) # dataframe can be used for EdSurvey functions

########
# Demonstrating difference between pisa.sel & pisa.sel2

lm.sdf(formula = pv1read ~ gcselfeff, data = pisa.sel)
lm.sdf(formula = pv1read ~ gcselfeff, data = pisa.sel2)


######################################################
# Running regressiosn for global competence scales
#####################################################
global.scales

lm.gcselfeff <- lm.sdf(formula = pv1read ~ gcselfeff, data = sdf)
summary(lm.gcselfeff)
# Multiple R-squared: 0.105

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

# Additional varibables not used
# pared
# Not used in further analyses
EdSurvey::summary2(variable = "pared", data = sdf)

lm.pared <- lm.sdf(formula = pv1read ~ pared, data = sdf)
summary(lm.pared)
# Multiple R-squared: 0.0715

# hisced
# Not used in further analyses
levelsSDF(varnames = "hisced", data = sdf)

lm.hisced <- lm.sdf(formula = pv1read ~ hisced, data = sdf)
summary(lm.hisced)
# Multiple R-squared: 0.0804

# ESCS
# Not used for further analyses
lm.escs <- lm.sdf(pv1read ~ escs, data = sdf)
summary(lm.escs)
# Multiple R-squared: 0.1722

######################################################
## Combined control variables #######################
#####################################################
control.vars

lm.control.vars <- lm.sdf(pv1read ~ progn + st001d01t + st004d01t + hisei + immig + repeatgrade, data = sdf )

summary(lm.control.vars)
# Multiple R-squared: 0.4673

###################### 
# Using standardized regression coefficients
summary(lm.control.vars, src = TRUE)
#####################



## Effect of global competence after controlling for control.vars

lm.gcselfeff.controlled <- lm.sdf(pv1read ~ gcselfeff + progn + st001d01t + st004d01t + hisei + immig + repeatgrade, data = sdf )
summary(lm.gcselfeff.controlled)
# Effect still significant
# Multiple R-squared: 0.4663

lm.awacom.controlled <- lm.sdf(pv1read ~ awacom + progn + st001d01t + st004d01t + hisei + immig + repeatgrade, data = sdf )
summary(lm.awacom.controlled)
# Effect still significant
# Multiple R-squared:  0.4576


lm.attimm.controlled <- lm.sdf(pv1read ~ attimm + progn + st001d01t + st004d01t + hisei + immig + repeatgrade, data = sdf )
summary(lm.attimm.controlled)
# Effect still significant
# Multiple R-squared: 0.4532


# Regressing top 3 scales global competence
lm.global.controlled <- lm.sdf(pv1read ~ gcselfeff + awacom + attimm + + progn + st001d01t + st004d01t + hisei + immig + repeatgrade, data = sdf )

summary(lm.global.controlled)
# All three scales significant
# Multiple R-squared: 0.4665



#############################################################
# Checking into different global competence scales
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


# Scale awacom -  Students' awareness of intercultural communication
items.awacom <- c( "st218q01ha",
                   "st218q02ha",
                   "st218q03ha",
                   "st218q04ha",
                   "st218q05ha",
                   "st218q06ha",
                   "st218q07ha")

searchSDF("st218", data = sdf)

levelsSDF(varnames =  c( "st218q01ha",
                         "st218q02ha",
                         "st218q03ha",
                         "st218q04ha",
                         "st218q05ha",
                         "st218q06ha",
                         "st218q07ha"),
          data = sdf)
                   

# Scale attimm - attitudes towards immigrants
items.attimm <- c("st204q02ha",
                  "st204q03ha",
                  "st204q04ha",
                  "st204q05ha")

searchSDF("st204", data = sdf)

levelsSDF( varnames = c("st204q02ha",
                        "st204q03ha",
                        "st204q04ha",
                        "st204q05ha"),
           data = sdf)


