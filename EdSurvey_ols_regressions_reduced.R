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

# pisa.sel <- EdSurvey::getData(data = sdf,
#                                varnames = c(id.vars,wt.vars,global.scales,control.vars,pv),
#                               omittedLevels = F, # Do not drop omitted levels
#                               returnJKreplicates = F) # don?t return replicate weights
# 

# Object can be used in EdSurvey functions, if addAttributes = True
pisa.sel2 <- EdSurvey::getData(data = sdf,
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
length(full.cases) # 2034

########
# Demonstrating difference between pisa.sel & pisa.sel2

#lm.sdf(formula = pv1read ~ gcselfeff, data = pisa.sel) # Does not work
lm.sdf(formula = pv1read ~ gcselfeff, data = pisa.sel2) # Function can be used


######################################################
# Running regressiosn for global competence scales
#####################################################
global.scales

lm.gcselfeff <- lm.sdf(formula = pv1read ~ gcselfeff, data = pisa.sel2)
summary(lm.gcselfeff)
# Multiple R-squared: 0.105




# Using all scales
lm.global.scales <- lm.sdf(formula = pv1read ~ gcselfeff + gcaware + perspect + cogflex + awacom + intcult + respect +globmind +  attimm, data = pisa.sel2)
summary(lm.global.scales)
# Multiple R-squared: 0.188


#####################################################################
### Running regressions for control variables 
###################################################################
control.vars

# progn
levelsSDF(varnames = "progn", data = pisa.sel2)

lm.progn <- lm.sdf(formula = pv1read ~ progn, data = pisa.sel2)
summary(lm.progn)
# Multiple R-squared: 0.3526


# st001d01t
# Grade
levelsSDF(varnames = "st001d01t", data = pisa.sel2)

lm.grade <- lm.sdf(formula = pv1read ~ st001d01t, data = pisa.sel2)
summary(lm.grade) # compared to baselevel grade 7
# Multiple R-squared: 0.0946

# st004d01t
# Gender
levelsSDF(varnames = "st004d01t", data = pisa.sel2)

lm.gender <- lm.sdf(pv1read ~ st004d01t, data = pisa.sel2)
summary(lm.gender)
# Multiple R-squared: 0.0104

# hisei
EdSurvey::summary2(variable = "hisei", data = pisa.sel2)

lm.hisei <- lm.sdf(formula = pv1read ~ hisei, data = pisa.sel2)
summary(lm.hisei)
# Multiple R-squared: 0.1416

# immig
levelsSDF(varnames = "immig", data = pisa.sel2)

lm.immig <- lm.sdf(pv1read ~ immig, data = pisa.sel2)
summary(lm.immig)
# Multiple R-squared: 0.033

# repeatgrade
levelsSDF(varnames = "repeatgrade", data = pisa.sel2)

lm.repeatgrade <- lm.sdf(formula = pv1read ~ repeatgrade, data = pisa.sel2)
summary(lm.repeatgrade)
# Multiple R-squared: 0.0364

# sc048q01na - pct of students whose heritage language is different from test language
summary2(variable = "sc048q01na", data = pisa.sel2)

lm.sc048 <- lm.sdf(formula = pv1read ~ sc048q01na, data = pisa.sel2)
summary(lm.sc048)
# Multiple R-squared: 0.0648



######################################################
## Combined control variables #######################
#####################################################
control.vars

lm.control.vars <- lm.sdf(pv1read ~ progn + st001d01t + st004d01t + hisei + immig + repeatgrade + sc048q01na, data = pisa.sel2 )

summary(lm.control.vars)
# Multiple R-squared: 0.4246

###################### 
# Optional - needs to be aligned with Julia
# Using standardized regression coefficients
# summary(lm.control.vars, src = TRUE)
#####################



## Effect of global competence after controlling for control.vars

lm.gcselfeff.controlled <- lm.sdf(pv1read ~ gcselfeff + progn + st001d01t + st004d01t + hisei + immig + repeatgrade + sc048q01na , data = pisa.sel2)
summary(lm.gcselfeff.controlled)
# Effect still significant
# Multiple R-squared: 0.4379


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


