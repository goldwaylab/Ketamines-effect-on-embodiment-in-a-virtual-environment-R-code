#Raw scores LMM
library(tidyverse) 
library(performance) 
library(psych) 
library(lmerTest)
library(readxl) 
library(nlme)

#Since factor scores regression, can be biased by the method of factor scoring itself
#(Skrondal & Laake, 2001) here we conduct a sensitivity analysis, to assess
#whether the results of the factor regression are replicable with raw scores. 

#Importing data
df = readxl::read_excel('self_report_all_07_06_2020_11_27.xlsx')

#Making dataframe for sum scores of Questions loading most strongly on these factors
df = df %>%
  mutate(Ownership = rowSums(across(c(Q1, Q3, Q6)), na.rm = FALSE),
         Agency = rowSums(across(c(Q4, Q8, Q11)), na.rm = FALSE),
         Loss_of_hand = rowSums(across(c(Q2, Q5, Q9)), na.rm = FALSE))

#Re-setting reference levels, so it is ketamines effect from placebo
df = df %>%
  mutate(session = factor(session, levels = c('p','k')))

#Conducting raw LMM
own_ket_sess_raw = lmer(Ownership ~ session + (session | subji), data = df)
summary(own_ket_sess_raw)
agen_ket_sess_raw = lmer(Agency ~ session + (session | subji), data = df)
summary(agen_ket_sess_raw)
loss_ket_sess_raw = lmer(Loss_of_hand ~ session + (session | subji), data = df) 
summary(loss_ket_sess_raw)

#Extracting raw p-values to adjust with holm (Bobbitt, 2023)
raw_pvals = c(ownership = summary(own_ket_sess_raw)$coefficients['sessionk','Pr(>|t|)'],
              agency = summary(agen_ket_sess_raw)$coefficients['sessionk','Pr(>|t|)'],
              loss = summary(loss_ket_sess_raw)$coefficients['sessionk','Pr(>|t|)'])

raw_pvals
# ownership      agency        loss 
#0.013938383 0.123637578 0.008347086 

holm_raw = p.adjust(raw_pvals, method = 'holm')
holm_raw
#ownership     agency       loss 
#0.02787677 0.12363758 0.02504126 

############################Checking assumptions################################
#Code adapted from: Stats.Stackexchange (2023)

#To assess heteroscadicity, I am using a residual vs fitted plot of conditional residuals
#which is applicable for LMMs (VSNi, 2022; StatsNotebook, 2020; Stats.Stackexchange, 2023). 
#Since session is entered as a categorical predictor, the usual linearity assumption
#for continuous predictors is not applicable (Nahhas, 2026). Normality of residuals is 
#assessed with qq plots (Schad, 2022). Singlularity was additionally checked, to see
#if the random effects structure was attainable with the data. 

check_heteroscedasticity(own_ket_sess_raw)
check_heteroscedasticity(agen_ket_sess_raw)
check_heteroscedasticity(loss_ket_sess_raw)
#This is violated, visual checks will also be conducted

plot(fitted(own_ket_sess_raw), resid(own_ket_sess_raw))
abline(h = 0)

qqnorm(resid(own_ket_sess_raw))
qqline(resid(own_ket_sess_raw))

isSingular(own_ket_sess_raw)
summary(own_ket_sess_raw)

plot(fitted(agen_ket_sess_raw), resid(agen_ket_sess_raw))
abline(h = 0)

qqnorm(resid(agen_ket_sess_raw))
qqline(resid(agen_ket_sess_raw))

isSingular(agen_ket_sess_raw)
summary(agen_ket_sess_raw)

plot(fitted(loss_ket_sess_raw), resid(loss_ket_sess_raw))
abline(h = 0)

qqnorm(resid(loss_ket_sess_raw))
qqline(resid(loss_ket_sess_raw))

isSingular(loss_ket_sess_raw)
summary(loss_ket_sess_raw)

#Although there is some evidence of non-normality, this is unlikely to be a major concern
#(see Knief & Forstmeier, 2021). Heteroscedasticity appears to be a more
#concern, so I will fit models that explicitly allow unequal residual
#variances.

################### Alternative accounting for variance #######################
#Code adapted from: (Magnusson, 2015; Pinheiro & Bates, 1998; Pustejovsky, 2024)
own_lme = lme(
  fixed = Ownership ~ session,
  random = ~ session | subji,
  data = df,
  method = 'REML')
summary(own_lme)

agen_lme = lme(
  fixed = Agency ~ session,
  random = ~ session | subji,
  data = df,
  method = 'REML')
summary(agen_lme)

loss_lme = lme(
  fixed = Loss_of_hand ~ session,
  random = ~ session | subji,
  data = df,
  method = 'REML')
summary(loss_lme)

own_lme_het = update(
  own_lme,
  weights = varIdent(form = ~ 1 | session))
summary(own_lme_het)

agen_lme_het = update(
  agen_lme,
  weights = varIdent(form = ~ 1 | session))
summary(agen_lme_het)

loss_lme_het = update(
  loss_lme,
  weights = varIdent(form = ~ 1 | session))
summary(loss_lme_het)

#################Attempting to account for heteroscedacistity###################

#General observation (Rdocumentation, n.d.-b)
plot(own_lme, abline = c(0,0))
plot(own_lme_het, abline = c(0,0))

plot(agen_lme, abline = c(0,0))
plot(agen_lme_het, abline = c(0,0))

plot(loss_lme, abline = c(0,0))
plot(loss_lme_het, abline = c(0,0))
#Slightly improved heteroscedasticity

check_heteroscedasticity(own_lme_het)
check_heteroscedasticity(agen_lme_het)
check_heteroscedasticity(loss_lme_het)
#Error variance appears to be homoscedastic

#Extracting CI's
#Extracting confidence intervals (Rdocumentation, n.d.-a)
intervals(own_lme_het) #[0.4505543, 8.426568]
intervals(agen_lme_het) #[-0.2742033, 2.477566]
intervals(loss_lme_het) #[0.5435878, 3.059454]

#Extracting p-values (Bobbitt, 2023)
pvals_het = c(ownership = summary(own_lme_het)$tTable['sessionk','p-value'],
              agency = summary(agen_lme_het)$tTable['sessionk','p-value'],
              loss = summary(loss_lme_het)$tTable['sessionk', 'p-value'])

pvals_het

#Adjusting p-values 
holm_het = p.adjust(pvals_het, method = 'holm')
holm_het
# ownership     agency       loss 
#0.01993282 0.11605774 0.01553943 

#Comparing p-values before and after holm-adjustment
holm_het
# ownership     agency       loss 
#0.01993282 0.11605774 0.01553943 
holm_raw
# ownership     agency       loss 
#0.02787677 0.12363758 0.02504126 

#Significance is the same, sensitivity check. 

################## Extra robustness paired permutation test ######################
#Although the heteroscedastic LMMs allowed residual variances to differ across
#sessions, there is some evidence of heteroscedasticity remained in the diagnostic 
#plots. LMMs are generally considered reasonably robust to some degree of
#heteroscedasticity, particularly for fixed-effect estimates
#(Jacqmin-Gadda et al., 2007; Schielzeth et al., 2020).Nevertheless, we conducted 
#permutation tests will be used as an additional robustness check, because they
#do not rely on the same parametric assumptions as standard parametric tests
#(Curley & Milewski, 2020).

#Code adapted from (Curley & Milewski, 2020; Bolker, 2024; Sleeper, 2026)

#Getting mean ownership, agency and loss of hand score for each participants
#within each session
own_wide = df %>%
  group_by(subji, session) %>%
  summarise(Ownership = mean(Ownership, na.rm = T), .groups = 'drop') %>%
  pivot_wider(names_from = session, values_from = Ownership)

agen_wide = df %>%
  group_by(subji, session) %>%
  summarise(Agency = mean(Agency, na.rm = T), .groups = 'drop') %>%
  pivot_wider(names_from = session, values_from = Agency)

loss_wide = df %>%
  group_by(subji, session) %>%
  summarise(Loss_of_hand = mean(Loss_of_hand, na.rm = T), .groups = 'drop') %>%
  pivot_wider(names_from = session, values_from = Loss_of_hand)

#Generating difference scores for permuation test
d_own = own_wide$k - own_wide$p
d_agen = agen_wide$k - agen_wide$p
d_loss = loss_wide$k - loss_wide$p

#Removing na's from difference scores
d_own  = d_own[!is.na(d_own)]
d_agen = d_agen[!is.na(d_agen)]
d_loss = d_loss[!is.na(d_loss)]

#Code adapted from: (Curley & Milewski, 2020)
paired_perm_test = function(d, B = 10000, seed = 143) {
  obs = mean(d) #Gives the test statistic the average difference across participants
  set.seed(seed) #Keep random permutations the same each time it is run
  perm_stats = replicate(B, { #Do 10000 random sign flips
    mean(d *sample(c(-1, 1), length(d), replace = T))}) 
  p_value = sum(abs(perm_stats) >= abs(obs)) / B #Calculate the number of times out
  #of B that we observe a mean difference as large or larger than the observed
  #mean difference, divided by the number of permutations
  c(n = length(d), observed_mean_difference = obs, p_value = p_value)}

#Applying function
res_own  = paired_perm_test(d_own)
res_agen = paired_perm_test(d_agen)
res_loss = paired_perm_test(d_loss)

res_own
res_agen
res_loss

#Making df to adjust p-values
results_perm = data.frame(
  outcome = c('Ownership', 'Agency', 'Loss_of_hand'),
  mean_difference = c(res_own['observed_mean_difference'], res_agen['observed_mean_difference'],res_loss['observed_mean_difference']),
  p_value = c(res_own['p_value'],res_agen['p_value'],res_loss['p_value']))

results_perm$p_holm = p.adjust(results_perm$p_value, method = 'holm')

results_perm
#       outcome mean_difference p_value p_holm
#1    Ownership        1.958333  0.0120 0.0240
#2       Agency        1.000000  0.2063 0.2063
#3 Loss_of_hand        1.850000  0.0064 0.0192

#The same pattern of results is observed

#References
##Bobbitt, Z. (2023, January 25). How to Extract P-Values from lm() Function in R. Statology. https://www.statology.org/r-extract-p-value-from-lm/
#Bolker, B. (2024, January 9). Simple permutation tests in R. Github.io. https://mac-theobio.github.io/QMEE/lectures/permutation_examples.notes.html
#Curley, J. P., & Milewski, T. M. (2020). 13 Permutation Testing | PSY317L Guidebook. In bookdown.org. https://bookdown.org/curleyjp0/psy317l_guides5/permutation-testing.html
#Finos, L. (2025). Package “flip” Type Package Title Multivariate Permutation Tests. https://cran.r-project.org/web/packages/flip/flip.pdf
#Jacqmin-Gadda, H., Sibillot, S., Proust, C., Molina, J.-M., & Thiébaut, R. (2007). Robustness of the linear mixed model to misspecified error distribution. Computational Statistics & Data Analysis, 51(10), 5142–5154. https://doi.org/10.1016/j.csda.2006.05.021
#Knief, U., & Forstmeier, W. (2021). Violating the normality assumption may be the lesser of two evils. Behavior Research Methods, 53(6). https://doi.org/10.3758/s13428-021-01587-5
#Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). LmerTest package: Tests in linear mixed effects models. Journal of Statistical Software, 82(13), 1–26. https://doi.org/10.18637/jss.v082.i13
#Lüdecke, D., Ben-Shachar, M., Patil, I., Waggoner, P., & Makowski, D. (2021). performance: An R Package for Assessment, Comparison and Testing of Statistical Models. Journal of Open Source Software, 6(60), 3139. https://doi.org/10.21105/joss.03139
#Magnusson, K. (2015). Using R and lme/lmer to fit different two- and three-level longitudinal models. Rpsychologist.com. https://rpsychologist.com/r-guide-longitudinal-lme-lmer
#Nahhas, R. W. (2026, March 27). 5.17 Checking the linearity assumption | Introduction to Regression Methods for Public Health Using R. Posit.cloud. https://019b2da8-edfb-a262-61be-7973c056d9ae.share.connect.posit.cloud/mlr-linearity.html
#Nichols, T. E., Brookhart, M. A., Lew, R. A., & Stedman, M. R. (2026). Permutation Test - an overview | ScienceDirect Topics. Www.sciencedirect.com. https://www.sciencedirect.com/topics/mathematics/permutation-test
#Pinheiro , J. C., & Bates, D. M. (1998). lme and nlme Mixed-Effects Methods and Classes for S and S-PLUS Version 3.0. https://www.stat.cmu.edu/~brian/720-2007-source/week07-08-ideas/pinheiro98mixedeffects-Sguide.pdf
#Pinheiro, J., Bates, D., & R-core Team. (2025). nlme: Linear and Nonlinear Mixed Effects Models. R-Packages. https://cran.r-project.org/web/packages/nlme/index.html
#Pustejovsky, J. E. (2024, December 28). A quirk of `nlme::varIdent`. James E. Pustejovsky. https://jepusto.com/posts/varIdent-function-in-nlme/
#Rdocumentation. (n.d.-a). intervals.lme function - RDocumentation. Rdocumentation.org. https://www.rdocumentation.org/packages/nlme/versions/3.1-168/topics/intervals.lme
#Rdocumentation. (n.d.-b). plot.lme function - RDocumentation. Rdocumentation.org. https://www.rdocumentation.org/packages/nlme/versions/3.1-168/topics/plot.lme
#Revelle, W. (2024). psych: Procedures for Psychological, Psychometric, and Personality Research. https://cran.r-project.org/package=psych. R package version 2.4.6.
#Schad, D. (2022, August 14). 3.3 Checking model assumptions | Linear Mixed Models in Linguistics and Psychology: A Comprehensive Introduction. Github.io. https://vasishth.github.io/Freq_CogSci/checking-model-assumptions.html
#Schielzeth, H., Dingemanse, N. J., Nakagawa, S., Westneat, D. F., Allegue, H., Teplitsky, C., Réale, D., Dochtermann, N. A., Garamszegi, L. Z., & Araya‐Ajoy, Y. G. (2020). Robustness of linear mixed‐effects models to violations of distributional assumptions. Methods in Ecology and Evolution, 11(9), 1141–1152. https://doi.org/10.1111/2041-210x.13434
#Skrondal, A., & Laake, P. (2001). Regression among factor scores. Psychometrika, 66(4), 563–575. https://doi.org/10.1007/bf02296196
#Sleeper, T. (2026). Permutation Tests. Thomasleeper.com. https://thomasleeper.com/Rcourse/Tutorials/permutationtests.html
#Stats.Stackexchange. (2023, September 19). Understanding residuals vs. fitted plot for a linear mixed model. Cross Validated. https://stats.stackexchange.com/questions/626850/understanding-residuals-vs-fitted-plot-for-a-linear-mixed-model
#StatsNotebook. (2020, October 16). Residual plots and assumption checking. StatsNotebook - Simple. Powerful. Reproducible. https://statsnotebook.io/blog/analysis/linearity_homoscedasticity/
#VSNi. (2022, November 2). Introduction to LMM For Designed Experiments | Part 4 - VSNi. VSNi. https://vsni.co.uk/a-lightning-introduction-to-linear-mixed-models-for-designed-experiments-part-4/
#Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L., François, R., Grolemund, G., Hayes, A., Henry, L., Hester, J., Kuhn, M., Pedersen, T., Miller, E., Bache, S., Müller, K., Ooms, J., Robinson, D., Seidel, D., Spinu, V., & Takahashi, K. (2019). Welcome to the Tidyverse. Journal of Open Source Software, 4(43), 1686. https://doi.org/10.21105/joss.01686
#Wickham, H., & Bryan, J. (2025, March 7). readxl: Read Excel Files. R-Packages. https://cran.r-project.org/web/packages/readxl/index.html

#Ai declaration
#Ai was used to suggest fixes to debug code, as well as to aid understanding of
#how functions work. Also to check spelling and grammar of comments. 

#Anthropic. (2025). Claude. Claude.ai. https://claude.ai
#Grammarly. (2025). Grammarly. Grammarly.com. https://app.grammarly.com/
#OpenAI. (2025). ChatGPT. ChatGPT; OpenAI. https://chatgpt.com/
