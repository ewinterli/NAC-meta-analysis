########## STEP ONE: BACKGROUND ##########
#Load packages
library(MAd)
library(metafor)
library(effsize)
library(epitools)
library(readxl) #Only necessary if your datasets are in Excel format

#Aggregate ES for studies for which it's necessary
#1. Yoon
y20 <- data.frame(id=factor(rep(1,2)), measure = c("penn","ocds"), es=c(0.261, 0.61), var.es=c(0.005931818, 0.013863636), nT=rep(44, 2), nC=rep(44, 2))
agg(y20, es=es, var=var.es, n.1 = nT, n.2 = nC, cor = .5, method="BHHR", mod=NULL, data = y20)
#Aggregated ES for Yoon: 0.436
#2. Back (2016)
b16<-data.frame(id=factor(rep(1,3)), measure = c("amt","freq","int"), es=c(1.029, .931, .583), var.es=c(0.0381111111, .034481481, .021592593), nT=rep(27, 3), nC=rep(27, 3))
agg(b16, es=es, var=var.es, n.1 = nT, n.2 = nC, cor = .5, method="BHHR", mod=NULL, data = b16)
#Aggregated ES for Back 2016: 0.848
#3. Schulte
sc17 <- data.frame(id=factor(rep(1,2)), measure = c("VAS", "QSU"), es=c(-0.087, -0.036), var.es=c(0.002230769, 0.030382181), nT=rep(39, 2), nC=rep(39, 2))
agg(sc17, es=es, var=var.es, n.1 = nT, n.2 = nC, cor = .5, method="BHHR", mod=NULL, data = sc17)
#Aggregated ES for Schulte: -0.062
#4. Back (2023)
ba23 <- data.frame(id=factor(rep(1,2)), measure = c("obs", "comp"), es=c(-0.078, 0.019), var.es=c(0.000516556, 0.000126667), nT=rep(151, 2), nC=rep(150, 2))
agg(ba23, es=es, var=var.es, n.1 = nT, n.2 = nC, cor = .5, method="BHHR", mod=NULL, data = ba23)
#Aggregated ES for Back 2023: -0.030

#Studies which did not need to have results aggregated include:
# 1. McKetin et al., 2021: ES = 0
# 2. Morley et al., 2023: ES = -0.168
# 3. Back et al., 2021: ES = .158
# 4. Roten et al., 2013: ES = .157
# 5. Schmaal et al., 2011: 0.538

#McKetin's effect size calculated to be 0 on https://lbecker.uccs.edu, double checking with R
MCmeangroup1<-3.3
MCmeangroup2<-3.3
MCsdgroup1<-2.3
MCsdgroup2<-2.2
MCpooled_sd <- sqrt(((MCsdgroup1^2 + MCsdgroup2^2) / 2))
MCcohen_d <- (MCmeangroup1 - MCmeangroup2) / MCpooled_sd
print(paste("Cohen's d effect size:", round(MCcohen_d, 2)))
#[1] "Cohen's d effect size: 0"
#Will be transforming McKetin's SE (0.000 -> 0.001);
#R packages won't calculate I2 or Q statistics with a null variance, but subgroup analyses will be conducted to account for bias

#Calculate outliers, defined as scores 2.5 SDs from mean
data <- c(0.000, -.168, 0.436, .848, .158, -.030, .157, 0.538, -0.062)
z_scores <- scale(data)
threshold <- 2.5
outlier_indices <- which(abs(z_scores) > threshold)
outlier_values <- data[outlier_indices]
outlier_indices
outlier_values
#No outliers present

########### STEP TWO: META-ANALYSIS AND SUBGROUP ANALYSIS EXCLUDING MCKETIN ###########
#Conduct overall meta-analysis with all studies
meta_data <- data.frame(
  effect_size = c(0.000, -.168, 0.436, .848, .158, -.030, .157, 0.538, -0.062),
  se = c(0.00100, 0.06324, 0.08494, 0.14391, 0.04559, 0.01499, 0.04770, 0.15637, 0.10105),
  n = c(153, 42, 44, 27, 76, 151, 69, 22, 39))
result_meta <- rma(effect_size, sei = se, data = meta_data, method = "REML")
print(result_meta)
# Influence analysis (to calculate weight of each study)
infl1 <- influence(result_meta)
print(infl1)
#Leave one out analysis (LOOA; to estimate the effect of leaving out each study)
LOO_result1 <- rma(effect_size, sei=se, data = meta_data)
LOO1 <- leave1out(LOO_result1)
leave_one_out_results1 <- LOO1[, c("study", "loo", "se.loo", "ci.lb.loo", "ci.ub.loo")]
print(leave_one_out_results1)

#Subgroup analysis excluding McKetin
mcketin_SA_data <- data.frame(
  effect_size = c(-.168, 0.436, .848, .158, -.030, .157, 0.538, -0.062),
  se = c(0.06324, 0.08494, 0.14391, 0.04559, 0.01499, 0.04770, 0.15637, 0.10105),
  n = c(42, 44, 27, 76, 151, 69, 22, 39))
result_mcketin_SA <- rma(effect_size, sei = se, data = mcketin_SA_data, method = "REML")
print(result_mcketin_SA)
#Influence analysis
infl_mcketin <- influence(result_mcketin_SA)
print(infl_mcketin)
#LOO analysis
LOO_result_mcketin <- rma(effect_size, sei = se, data = mcketin_SA_data)
LOO_mcketin <- leave1out(LOO_result_mcketin)
leave_one_out_results_mcketin <- LOO_mcketin[, c("study", "loo", "se.loo", "ci.lb.loo", "ci.ub.loo")]
print(leave_one_out_results_mcketin)

#Subgroup analysis for alcohol-focused studies
alc_SA_data <- data.frame(
  effect_size = c(-.168, 0.436, -.030),
  se = c(0.06324, 0.08494,0.01499),
  n = c(42, 44, 151))
result_alc_SA <- rma(effect_size, sei = se, data = alc_SA_data, method = "REML")
print(result_alc_SA)
#Influence analysis
infl_alc <- influence(result_alc_SA)
print(infl_alc)

############  STEP 3: RISK RATIOS (RRs) FOR ADVERSE EVENTS & SUBGROUP ANALYSIS FOR RRs #############
#Note that these are calculated so that values below 0 indicate lower risk of AEs for NAC group
#logRRs will be calculated in funnel plot code
#Load in RR data sheet
RR <- read_excel("your/data/here")

#Calculate RRs
RR_calc <- escalc(measure = "RR", ai = ae_pos_e, bi = ae_neg_e, ci = ae_pos_p, di = ae_neg_p, data = RR, append = TRUE)
print(RR_calc[,c(11:12)], row.names = FALSE)

#Conduct RR SA
RR_SA <- rma(yi = yi, sei = se, data = RR_calc, method = "REML")
print(RR_SA)

#Influence analysis
infl_RR <- influence(RR_SA)
print(infl_RR)

############ STEP 4: FUNNEL PLOT  ###########

#Funnel 1 includes McKetin
funnel1 <- rma(yi = effect_size, sei=se, data = meta_data)
funnel(funnel1)

#Funnel 2 excludes McKetin
funnel2 <- rma(yi = effect_size, sei = se, data = mcketin_SA_data)
funnel(funnel2)

########### STEP 5: EGGER'S TEST ###########
#First with McKetin
regtest(result_meta)

#Then without McKetin
regtest(result_mcketin_SA)

############# STEP 6: FIGURES ##############
#Separate code: "forest-plots-code"
