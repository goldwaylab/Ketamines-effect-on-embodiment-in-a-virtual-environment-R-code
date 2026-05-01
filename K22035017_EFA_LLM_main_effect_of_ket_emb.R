install.packages('r2glmm')
library(r2glmm)
library(tidyverse)
library(psych)
library(lmerTest)
library(readxl)
library(performance)
library(broom.mixed)
library(dotwhisker)
library(lme4)

#Importing data
df = readxl::read_excel('self_report_all_07_06_2020_11_27.xlsx')

################################Inspecting Data ################################

#Number of participants
length(unique(df$subji))

#Number of participants who completed all 8 conditions
df %>%
  count(subji) %>%
  filter(n == 8) %>%
  summarise(all_8 = n())

#All participants were retained regardless of whether they took part in all conditions
#As this would have unnecessarily reduced sample size for factor analysis (Watkins, 2018)
str(df)

######################## Data cleaning and pre-processing ######################

#Names of questionnaire items to include (Q10 and 12 removed)
items_noQ10 = c('Q1','Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9','Q11')

#Add a row ID so factor scores can be merged back to the correct row later
df2 = df %>%
  mutate(.row_id = row_number())

#Selecting row ID and EFA item columns
X = df2 %>%
  select(.row_id, all_of(items_noQ10))

#Identify rows with complete data on all EFA items, to see whether data handling is 
#required
complete.cases(X)
#no incomplete cases (rows)

#EFA and parallel analysis can be sensitive to matrix or dataframe hence, I'll
#adjust it
X_mat = df2 %>%
  select(all_of(items_noQ10)) %>%
  as.matrix()

############################### Assumption checks ##############################

#Kaiser Meyer-Olkin test for factor analysis suitability
KMO(X_mat) 
#All MSA's are high = 0.79-0.97, hence factor analysis is suitable, also no
#questions appear not to be suitable for analysis (Kaiser, 1974; Glen, 2016).

##Assessing multivariate outliers using Mahalanobis distance##

#This calculates how far each participant's overall response pattern
#is from the centre of the item distribution, taking covariance into account
#(Prabhakaran, n.d; Rdocumentation, 2019)
md = mahalanobis(X_mat, center = colMeans(X_mat), cov = cov(X_mat))

#Defining the cutoff for extreme multivariate cases. 
#The cutoff is defined as 99.9th percentile of the chi-square distribution,
# with degrees of freedom equal to the number of items in Xmat 
#(Prabhakaran, n.d; rstatix 0.7.2, 2025)
cutoff = qchisq(0.999, df = ncol(X_mat))

#Identify the row numbers of cases exceeding the cutoff
which(md > cutoff)
length(which(md > cutoff))
#12 outliers identified

#Calculate the proportion of cases flagged as outliers
nrow(X_mat)
length(which(md > cutoff)) / nrow(X_mat)

#Check that all item responses fall within the expected scale range (GeeksforGeeks, 2022)
apply(X_mat, 2, range, na.rm = TRUE)

#Since they are all within the plausible range (1-7) I will not remove them (Watkins, 2018)

##Assessing Multivariate normality##
mardia_res = psych::mardia(X_mat, na.rm = TRUE, plot = TRUE) #(Rdocumentation, n.d.-a)
mardia_res
#The data has significant multivariate skewness, p < .001, and multivariate 
#kurtosis, p < .001, indicating that the data is not multivariately normally
#distributed. Hence this will inform my analysis choice in my EFA. 

############################## Parallel Analysis ###############################
#Using principal axis factoring considering violated multivariate normality 
#(Costello & Osborne, 2005). Code adapted from (Rdocumentation, 2022b)
set.seed(42)
pa = fa.parallel(X_mat, fa = 'fa', fm = 'pa', n.iter = 1000, error.bars = TRUE)
k = pa$nfact
#3 factor solution identified

######################### Exploratory Factor Analysis #########################

#Exploratory factor analysis using principal axis factoring considering violated 
#multivariate normality (Costello & Osborne, 2005; Stats.Stackexchange, 2017)
#with oblimin rotation, allowing factors to correlate. Regression method was used
#to get scores most closely approximating factor solution (Distefano et al., 2009)
efa = fa(X_mat, nfactors = k, fm = 'pa', rotate = 'oblimin', scores = 'regression')

#Extracting communalities
#(proportion of each item's variance explained by the extracted factors)
efa$communality
#Average communality across items
mean(efa$communality, na.rm = TRUE)
#the average number of items per factor
ncol(X_mat)/k

############################Extracting factor scores############################

#Extract factor scores from the EFA object and convert to a dataframe
scores = as.data.frame(efa$scores)

#Renaming factor score columns to F1,2,3, for clarity
names(scores) = paste0('F', 1:k)

#Adding row ids in the same order as the rows used in X_mat
scores$.row_id = df2$.row_id

#Merging factor scores back into the original dataframe by rowid so they
#are in the right place
df_with_scores_fa = df2 %>%
  left_join(scores, by = '.row_id') %>%
  select(-.row_id)

#Check that the scores have gone to the right row
identical(scores$.row_id, df2$.row_id)
stopifnot(nrow(scores) == nrow(df2))
stopifnot(all.equal(df_with_scores_fa$subji, df2$subji))
stopifnot(all.equal(df_with_scores_fa$session, df2$session))
stopifnot(all.equal(df_with_scores_fa$sync, df2$sync))
stopifnot(all.equal(df_with_scores_fa$speed, df2$speed))

#Export the final dataframe for later analyses
library(writexl)
write_xlsx(df_with_scores_fa, 'df_with_scores_fa_real_2.xlsx')

#######################Identifying Names for each Factor########################
#Comparing what I think the questions are asking,
#to the factor solution of the EFA, using a cut off at 0.3 as this is the 
#minimal meaningful loading (Kılıç, 2019)
print(efa$loadings, digits = 3, cutoff = 0.30)
#Q1 = 1 (0.822) 
#Q2 = 3 (0.717) A
#Q3 = 1 (0.981) O
#Q4 = 2 (0.694) A
#Q5 = 3 (0.960) L
#Q6 = 1 (0.845) O
#Q7 = 2 (0.853) A
#Q8 = 2 (0.922) A
#Q9 = 3 (0.591) L
#Q11 = 2 (0.426)

#From this I think a good definition would be F1 = ownership, F2 = agency,
#F3 = Loss of own hand

##########################General Linear Mixed Models###########################

#BIC to assess best random effects structure

#Changing reference levels to make coefficients clearer: so positive Ketamine 
#coefficient indicates a greater effect
df_with_scores_fa = df_with_scores_fa %>%
  mutate(session = factor(session, levels = c('p', 'k')))

#LMM allowing for session to vary across subjects
own_ket_sess = lmerTest::lmer(F1 ~ session + (session | subji), data = df_with_scores_fa)
summary(own_ket_sess)
agen_ket_sess = lmerTest::lmer(F2 ~ session + (session | subji), data = df_with_scores_fa)
summary(agen_ket_sess)
loss_ket_sess = lmerTest::lmer(F3 ~ session + (session | subji), data = df_with_scores_fa) 
summary(loss_ket_sess)

#LMM assuming session doesn't vary across subjects
own_ket = lmerTest::lmer(F1 ~ session + (1 | subji), data = df_with_scores_fa)
summary(own_ket)
agen_ket = lmerTest::lmer(F2 ~ session + (1 | subji), data = df_with_scores_fa)
summary(agen_ket)
loss_ket = lmerTest::lmer(F3 ~ session + (1 | subji), data = df_with_scores_fa) 
summary(loss_ket)

BIC(own_ket_sess,agen_ket_sess,loss_ket_sess,own_ket,agen_ket,loss_ket)
#Session as a random effect is lower for all models
#own_ket_sess = 756.9085 vs own_ket = 763.8369
#agen_ket_sess = 695.0927 vs agen_ket = 719.6209
#loss_ket_sess = 667.8559 vs loss_ket = 681.8169

#Hence Holm p-values will be adjusted based on (session|subji)

#Checking whether confint supports confint.merMod (to extract CI's for LMM's)
methods(confint)

#Extracting confidence intervals (Rdocumentation, 2022a)
confint(own_ket_sess, method = 'profile')
confint(agen_ket_sess, method = 'profile')
confint(loss_ket_sess, method = 'profile')

#Effect sizes
r2beta(own_ket_sess, partial = F, method = 'sgv')
r2beta(agen_ket_sess, partial = F, method = 'sgv')
r2beta(loss_ket_sess, partial = F, method = 'sgv')

########################### Assumption testing #################################
#Code adapted from: Stats.Stackexchange (2023)

#To assess heteroscadicity, I am using a residual vs fitted plot of conditional residuals
#which is applicable for LMMs (VSNi, 2022; StatsNotebook, 2020; Stats.Stackexchange, 2023). 
#Since session is entered as a categorical predictor, the usual linearity assumption
#for continuous predictors is not applicable (Nahhas, 2026). Normality of residuals is 
#assessed with qq plots (Schad, 2022). Singlularity was additionally checked, to see
#if the random effects structure was attainable with the data. 

#Heteroscedasticity
plot(fitted(own_ket_sess), resid(own_ket_sess))
abline(h = 0)

#Normality of residuals 
qqnorm(resid(own_ket_sess))
qqline(resid(own_ket_sess))

#Singularity
isSingular(own_ket_sess)
summary(own_ket_sess)

#Heteroscadicity
plot(fitted(agen_ket_sess), resid(agen_ket_sess))
abline(h = 0)

#Normality of residuals
qqnorm(resid(agen_ket_sess))
qqline(resid(agen_ket_sess))

#Singularity
isSingular(agen_ket_sess)
summary(agen_ket_sess)

#Heteroscadicity
plot(fitted(loss_ket_sess), resid(loss_ket_sess))
abline(h = 0)

#Normality of residuals
qqnorm(resid(loss_ket_sess))
qqline(resid(loss_ket_sess))

#Singularity
isSingular(loss_ket_sess)
summary(loss_ket_sess)

#Using the check_heteroscedasticity function, as an extra check, although focusing
#on visual plots (Lüdecke et al., 2025; Shatz, 2023)
check_heteroscedasticity(own_ket_sess)
check_heteroscedasticity(agen_ket_sess)
check_heteroscedasticity(loss_ket_sess)

#Although there is some evidence of non-normality, this is unlikely to be a major concern
#(see Knief & Forstmeier, 2021).There is also mild heteroscedasticity, but formal test
#and diagnostic plot together suggest that it is unlikely to meaningfully affect results. This is
#further mitigated by the fact that LMMs are generally robust to moderate heteroscadasticity
#(Jacqmin-Gadda et al., 2007;Schielzeth et al., 2020). Hence, analysis continued.

#Extracting p-values, code adapted from (Bobbitt, 2023)
pvals = c(ownership = summary(own_ket_sess)$coefficients['sessionk','Pr(>|t|)'],
          agency = summary(agen_ket_sess)$coefficients['sessionk','Pr(>|t|)'],
          loss = summary(loss_ket_sess)$coefficients['sessionk', 'Pr(>|t|)'])

pvals

#Adjusting p-values (don't need to include length =, as it sets to default
#number of p values) (Rdocumentation, n.d.-b)
p.adjust(pvals, method = 'holm')

# ownership     agency       loss 
#0.02196529 0.152896551 0.01322391
#Ownership and loss of hand are significant, agency isn't

########################## Graphs for visualisation #############################

#Coefficient plots, code adapted from (Solt & Hu, 2024)

#Extracting statistics from lmms and combining them, for one plot (Bolker, 2026)
coef_df = bind_rows(tidy(own_ket_sess,effects = 'fixed',conf.int = TRUE) %>%
                      mutate(Factor = 'Ownership'),
                    tidy(agen_ket_sess,effects = 'fixed',conf.int = TRUE) %>%
                      mutate(Factor = 'Agency'),
                    tidy(loss_ket_sess,effects = 'fixed',conf.int = TRUE) %>%
                      mutate(Factor = 'Loss of hand'))

drug_df = coef_df %>%
  filter(term == 'sessionk') 

#Making the plot
dwplot(drug_df %>%
         mutate(term = Factor, model = 'Ketamines effect')) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = 'Estimated drug effect',y = NULL, colour = NULL)

#References

#Bobbitt, Z. (2023, January 25). How to Extract P-Values from lm() Function in R. Statology. https://www.statology.org/r-extract-p-value-from-lm/
#Bolker, B. (2026). Package “broom.mixed” Type Package Title Tidying Methods for Mixed Models. https://cran.r-project.org/web/packages/broom.mixed/broom.mixed.pdf
#Bolker, B., & Robinson, D. (2024). Tidying Methods for Mixed Models [R package broom.mixed version 0.2.9.6]. R-Project.org. https://cran.r-project.org/package=broom.mixed
#Costello, A. B., & Osborne, J. W. (2005). Best Practices in Exploratory Factor Analysis: Four Recommendations for Getting the Most From Your Analysis. Elsevier, Vol. 10 , Article 7. https://doi.org/10.7275/jyj1-4868
#Distefano, C., Zhu, M., & Mindrila, D. (2009). Understanding and Using Factor Scores: Considerations for the Applied Researcher. Practical Assessment, Research & Evaluation, 14(20). https://www.researchgate.net/publication/255643537_Understanding_and_Using_Factor_Scores_Considerations_for_the_Applied_Researcher
#GeeksforGeeks. (2022, February 11). apply(), lapply(), sapply(), and tapply() in R. GeeksforGeeks. https://www.geeksforgeeks.org/r-language/apply-lapply-sapply-and-tapply-in-r/
#Glen, S. (2016, May 11). Kaiser-Meyer-Olkin (KMO) Test for Sampling Adequacy. Statistics How To. https://www.statisticshowto.com/kaiser-meyer-olkin/
#Jacqmin-Gadda, H., Sibillot, S., Proust, C., Molina, J.-M., & Thiébaut, R. (2007). Robustness of the linear mixed model to misspecified error distribution. Computational Statistics & Data Analysis, 51(10), 5142–5154. https://doi.org/10.1016/j.csda.2006.05.021
#Kılıç, A. (2019). What is the communality cut-off value in EFA? https://www.researchgate.net/post/What-is-the-communality-cut-off-value-in-EFA/5ca1b16e979fdc022a5de377
#Knief, U., & Forstmeier, W. (2021). Violating the normality assumption may be the lesser of two evils. Behavior Research Methods, 53(6). https://doi.org/10.3758/s13428-021-01587-5
#Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). LmerTest package: Tests in linear mixed effects models. Journal of Statistical Software, 82(13), 1–26. https://doi.org/10.18637/jss.v082.i13
#Lüdecke, D., Ben-Shachar, M., Patil, I., Waggoner, P., & Makowski, D. (2021). performance: An R Package for Assessment, Comparison and Testing of Statistical Models. Journal of Open Source Software, 6(60), 3139. https://doi.org/10.21105/joss.03139
#Lüdecke, D., Makowski, D., Ben-Shachar, M. S., Patil, I., Waggoner, P., Wiernik, B. M., & Thériault, R. (2025). Visual check of model assumptions — check_model. Github.io. https://easystats.github.io/performance/reference/check_model.html
#Nahhas, R. W. (2026, March 27). 5.17 Checking the linearity assumption | Introduction to Regression Methods for Public Health Using R. Posit.cloud. https://019b2da8-edfb-a262-61be-7973c056d9ae.share.connect.posit.cloud/mlr-linearity.html
#Prabhakaran, S. (n.d.). Outlier Detection in R: Four Methods and the One Question You Must Ask First. R-Statistics.co. https://r-statistics.co/Outlier-Detection-in-R.html
#Rdocumentation. (n.d.). p.adjust function - RDocumentation. Www.rdocumentation.org. https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/p.adjust
#Rdocumentation. (2019). mahalanobis function - RDocumentation. Rdocumentation.org. https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/mahalanobis
#Rdocumentation. (2022a). confint.merMod function - RDocumentation. Rdocumentation.org. https://www.rdocumentation.org/packages/lme4/versions/1.1-38/topics/confint.merMod
#Rdocumentation. (2022b). fa.parallel function - RDocumentation. Rdocumentation.org. https://www.rdocumentation.org/packages/psych/versions/2.5.6/topics/fa.parallel
#Revelle, W. (2024). psych: Procedures for Psychological, Psychometric, and Personality Research. https://cran.r-project.org/package=psych. R package version 2.4.6.
#Schad, D. (2022, August 14). 3.3 Checking model assumptions | Linear Mixed Models in Linguistics and Psychology: A Comprehensive Introduction. Github.io. https://vasishth.github.io/Freq_CogSci/checking-model-assumptions.html
#Schielzeth, H., Dingemanse, N. J., Nakagawa, S., Westneat, D. F., Allegue, H., Teplitsky, C., Réale, D., Dochtermann, N. A., Garamszegi, L. Z., & Araya‐Ajoy, Y. G. (2020). Robustness of linear mixed‐effects models to violations of distributional assumptions. Methods in Ecology and Evolution, 11(9), 1141–1152. https://doi.org/10.1111/2041-210x.13434
#Shatz, I. (2023). Assumption-checking rather than (just) testing: The importance of visualization and effect size in statistical diagnostics. Behavior Research Methods, 56(2). https://doi.org/10.3758/s13428-023-02072-x
#Solt , F., & Hu, Y. (2025). Dot-and-Whisker Plots of Regression Results [R package dotwhisker version 0.8.4]. R-Project.org. https://cran.r-project.org/package=dotwhisker
#Solt, F., & Hu, Y. (2024). dotwhisker: Dot-and-Whisker Plots of Regression Results. Fsolt.org. https://fsolt.org/dotwhisker/articles/dotwhisker-vignette.html
#Stats.Stackexchange. (2017, May 20). What is the difference between PCA and PAF method in factor analysis? Cross Validated. https://stats.stackexchange.com/questions/280746/what-is-the-difference-between-pca-and-paf-method-in-factor-analysis
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
