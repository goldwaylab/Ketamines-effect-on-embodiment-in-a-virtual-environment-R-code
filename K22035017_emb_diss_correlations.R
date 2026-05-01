library(tidyverse)
library(tibble)
library(corrplot)
library(lmerTest)
library(rio)
library(readxl)
library(psych)
library(lme4)


#To correlate ketamine-related changes in CADSS scores with ketamine-related changes in
#embodiment subcomponents, we first need individual participant scores for ketamine's effect,
#from the embodiment data. 

######################## Extracting fixed and random effects ########################
df_with_scores_fa = read_excel('df_with_scores_fa_real_2.xlsx')

#Removing participants who only took part in the Placebo session
df_with_scores_fa %>%
  count(subji) %>%
  filter(n == 8) %>%
  summarise(n_participants = n())

#Visual inspection of participants who didn't take part in all conditions
incomplete_ids = df_with_scores_fa %>%
  count(subji, name = 'n_conditions') %>%
  filter(n_conditions < 8)

incomplete_ids
#They only took part in half of the conditions so the conditions they didn't take part
#in will be inspected

#Confirming what condition they didn't take part in
view(df_with_scores_fa %>%
       filter(subji %in% incomplete_ids$subji))
#They didn't take part in the ketamine session

#Removing the incomplete participants that didn't take part in both sessions (ketamine session)
df_with_scores_fa_complete = df_with_scores_fa %>%
  group_by(subji) %>%
  filter(n_distinct(session) == 2) %>%
  ungroup()

#Checking participants with incomplete scores have been removed
any(incomplete_ids %in% df_with_scores_fa_complete$subji)

######################## Extracting random and fixed effects ########################
#By summing random and fixed effects from LLM's, we can get individual participant
#estimates of ketamines effects

#resetting the reference levels so that the effects represent Ketamines effect relative
#to placebo
df_with_scores_fa_complete = df_with_scores_fa_complete %>%
  mutate(session = factor(session, levels = c('p','k')))

#LLM's
own_b = lmerTest::lmer(F1~session + (session|subji), data = df_with_scores_fa_complete)
summary(own_b)

agen_b = lmerTest::lmer(F2~session + (session|subji), data = df_with_scores_fa_complete)
summary(agen_b)

loss_b = lmerTest::lmer(F3~session + (session|subji), data = df_with_scores_fa_complete)
summary(loss_b)

#Ownership random + fixed effect (Datacamp, n.d.)
re_own = ranef(own_b)$subji #Subject random effect
fix_slope_own = fixef(own_b)['sessionk'] #fixed effect of session

own_df = data.frame(
  subji = rownames(re_own), #Currently subject id is stored as a row name, so needs to be a column for joining dfs
  F1_ket_effect = fix_slope_own + re_own$sessionk)

#Agency random + fixed effect
re_agen = ranef(agen_b)$subji
fix_slope_agen = fixef(agen_b)['sessionk']

agen_df = data.frame(
  subji = rownames(re_agen),
  F2_ket_effect = fix_slope_agen + re_agen$sessionk)

#Loss of hand random + fixed effect
re_loss = ranef(loss_b)$subji
fix_slope_loss = fixef(loss_b)['sessionk']

loss_df = data.frame(
  subji = rownames(re_loss),
  F3_ket_effect = fix_slope_loss + re_loss$sessionk)

#Combining dataframes
embodiment_df = own_df %>%
  left_join(agen_df, by = 'subji') %>%
  left_join(loss_df, by = 'subji')

length(unique(embodiment_df$subji))

##################### Generating CADSS delta for correlations ####################
#Code taken from (Goldway et al., 2025)
CADSS_wide = import('CADSS.xlsx')

#Seeing how many participants didn't take part in at least 1 condition
CADSS_wide %>%
  group_by(subji) %>%
  summarise(has_any_NA = any(is.na(across(everything())))) %>%
  filter(has_any_NA) %>%
  summarise(n_participants = n())

#Are there any na's in post_bolus as this is the timepoint we are looking at
sum(is.na(select(CADSS_wide, ends_with('_post'))))
#No - none will be removed

CADSS = CADSS_wide %>%pivot_longer(
  !subji,
  names_to = c('scale', 'sub_scale','session','time_point'),
  names_sep='_',
  values_to = 'rating'
)

#Map sub_scale labels
CADSS$sub_scale = ifelse(CADSS$sub_scale == 'Amnesiasum', 'Amnesia',
                         ifelse(CADSS$sub_scale == 'Depersonalizationsum', 'Depersonalization',
                                ifelse(CADSS$sub_scale == 'Derealisationsum', 'Derealisation', NA)))

#Convert to factors
CADSS$sub_scale = as.factor(CADSS$sub_scale)
CADSS$session = ifelse(CADSS$session == '2', 'Placebo', 'Ketamine')
CADSS$session = as.factor(CADSS$session)
levels(CADSS$session) = c('Placebo', 'Ketamine')

#Map time_point labels
CADSS$time_point = ifelse(CADSS$time_point == 'baseline', 'Pre-infusion',
                          ifelse(CADSS$time_point == 'post', 'Post-bolus',
                                 ifelse(CADSS$time_point == 'end', 'End of infusion', NA)))

# Convert to factors
CADSS$time_point = as.factor(CADSS$time_point)
CADSS = na.omit(CADSS)

#Extracting the delta of ketamine - placebo for post-bolus only
levels(CADSS$session)

#Checking that each participant has only one observation per subscale and session,
#as otherwise, it is unclear what ketamine value to minus from which placebo value
CADSS %>%
  filter(time_point == 'Post-bolus') %>%
  count(subji, sub_scale, session) %>%
  filter(n > 1)
#There are no multiple observations per participant x subscale x session

#Making dataframe with ketamine - placebo difference scores, from the post-bolus timepoint 
CADSS_delta = CADSS %>%
  filter(time_point == 'Post-bolus') %>%
  select(subji, sub_scale, session, rating) %>%
  pivot_wider(names_from = session, values_from = rating) %>%
  mutate(delta = Ketamine - Placebo) %>%
  select(subji, sub_scale, delta) %>%
  pivot_wider(names_from = sub_scale, values_from = delta)

#Renaming to delta_ for clarity
CADSS_delta = CADSS_delta %>%
  rename(delta_amnesia = Amnesia,
         delta_depersonalization = Depersonalization,
         delta_derealisation = Derealisation)

#Quick sanity check that it has correctly computed the delta
CADSS %>%
  filter(subji == 1008, time_point == 'Post-bolus') %>%
  select(subji, sub_scale, session, rating)

CADSS_delta %>% filter(subji == 1008)

##############Combining the embodiment scores with CADSS data#################
#Changing Subji in CADSS to a character so they can be joined
CADSS_delta$subji = as.character(CADSS_delta$subji)

#Combining embodiment data and CADSS delta data
Corr_delta_df = embodiment_df %>%
  left_join(CADSS_delta, by = 'subji')

#Checking the number of participants
Corr_delta_df %>%
  filter(complete.cases(.)) %>%
  summarise(n_participants = n_distinct(subji))

#Removing the participants without complete cases
Corr_delta_df_complete = Corr_delta_df %>%
  filter(complete.cases(.))

#Checking it is correctly 30 participants remaining
n_distinct(Corr_delta_df_complete$subji)

################## Deciding on best type of correlation for data ###############

#Assessing data for ties
sum(duplicated(Corr_delta_df_complete$F1_ket_effect))
sum(duplicated(Corr_delta_df_complete$F1_ket_effect))
sum(duplicated(Corr_delta_df_complete$F3_ket_effect))
sum(duplicated(Corr_delta_df_complete$delta_amnesia))
sum(duplicated(Corr_delta_df_complete$delta_depersonalization))
sum(duplicated(Corr_delta_df_complete$delta_derealisation))

#CADSS shows tied values

#Between Embodiment
plot(Corr_delta_df_complete$F1_ket_effect, Corr_delta_df_complete$F2_ket_effect)
plot(Corr_delta_df_complete$F1_ket_effect, Corr_delta_df_complete$F3_ket_effect)
plot(Corr_delta_df_complete$F2_ket_effect, Corr_delta_df_complete$F3_ket_effect)

#Between dissociation
plot(Corr_delta_df_complete$delta_amnesia, Corr_delta_df_complete$delta_depersonalization)
plot(Corr_delta_df_complete$delta_amnesia, Corr_delta_df_complete$delta_derealisation)
plot(Corr_delta_df_complete$delta_depersonalization, Corr_delta_df_complete$delta_derealisation)

#Between embodiment and dissociation
plot(Corr_delta_df_complete$F1_ket_effect, Corr_delta_df_complete$delta_amnesia)
plot(Corr_delta_df_complete$F1_ket_effect, Corr_delta_df_complete$delta_depersonalization)
plot(Corr_delta_df_complete$F1_ket_effect, Corr_delta_df_complete$delta_derealisation)
plot(Corr_delta_df_complete$F2_ket_effect, Corr_delta_df_complete$delta_amnesia)
plot(Corr_delta_df_complete$F2_ket_effect, Corr_delta_df_complete$delta_depersonalization)
plot(Corr_delta_df_complete$F2_ket_effect, Corr_delta_df_complete$delta_derealisation)
plot(Corr_delta_df_complete$F3_ket_effect, Corr_delta_df_complete$delta_amnesia)
plot(Corr_delta_df_complete$F3_ket_effect, Corr_delta_df_complete$delta_depersonalization)
plot(Corr_delta_df_complete$F3_ket_effect, Corr_delta_df_complete$delta_derealisation)

#The inspection indicates tied values, and the relationship is not strictly linear
#and largely monotonic. Hence,a Spearman's correlation will be used, with approximate
#p-values (Laerd Statistics, 2018; R.documentation, 2025; Schober et al., 2018; Siddiqui, 2026)

########################Looping spearmans correlations##########################
#Code adapted from:(FJCC, 2024; jm_t, 2019;Rdocumentation, n.d.-a; Rdocumentation, n.d.-b)

#Loop for all of the correlations
#Making embodiment and dissociation 
embodiment_vars = c('F1_ket_effect', 'F2_ket_effect', 'F3_ket_effect')
dissociation_vars = c('delta_amnesia', 'delta_depersonalization', 'delta_derealisation')

#Empty dataframe
results = data.frame()

#Within embodiment correlations
results$family = NA

for (pair in combn(embodiment_vars, 2, simplify = FALSE)) { #Loop each unique pairs within embodiment_vars
  tmp = Corr_delta_df_complete %>% select(all_of(pair)) #Select the two variables in the current pair from the corr_dataframe
  test = cor.test(tmp[[1]], tmp[[2]], method = 'spearman', exact = FALSE) #Put the two variables as numeric vectors for correlation, cannot get exact p-values because of ties
  results = rbind(results, data.frame(var1 = pair[1], var2 = pair[2], rho = unname(test$estimate), p_value = test$p.value, n = nrow(tmp), family = 'within embodiment'))
} #Making a dataframe where each row contains the pair being correlated, the rho, p-value, sample size, and family

#Within dissociation
for (pair in combn(dissociation_vars, 2, simplify = FALSE)) {
  tmp = Corr_delta_df_complete %>% select(all_of(pair)) 
  test = cor.test(tmp[[1]], tmp[[2]], method = 'spearman', exact = FALSE)
  results = rbind(results, data.frame(var1 = pair[1], var2 = pair[2], rho = unname(test$estimate), p_value = test$p.value, n = nrow(tmp), family = 'within dissociation' ))
}

#Between embodiment and dissociation
for (e in embodiment_vars) {
  for (d in dissociation_vars) {
    tmp = Corr_delta_df_complete %>% select(all_of(c(e, d)))
    test = cor.test(tmp[[1]], tmp[[2]], method = 'spearman', exact = FALSE)
    results = rbind(results, data.frame(var1 = e, var2 = d, rho = unname(test$estimate), p_value = test$p.value, n = nrow(tmp),family = 'between domains'))
  }}

results

#Getting confidence intervals since cor.test doesn't support, confidence intervals for spearmans
all_vars = c(embodiment_vars, dissociation_vars)

#Computing bootstrapped confidence intervals, on selecting all rows of only embodiment and dissociation variables
#using 1000 bootstrap resamples (Revelle, 2025)
ci_res = corCi(Corr_delta_df_complete[, all_vars], method = 'spearman', n.iter = 1000, plot = FALSE)
ci_res

#Adjusting P-values for multiple corrections
results = results %>%
  mutate(p_fwe = p.adjust(p_value, method = 'holm'))

#Significant results before holm
results_pre_h = results %>%
  filter(p_value < 0.05)

#Significant after holm
results_fwe = results %>%
  filter(p_fwe < 0.05)
#Only some within domains are significant

##############################Plotting in a heatmap#############################
#Code adapted from (STHDA, 2025; Wei & Simko, 2021)

#Recode variable names to clearer labels for the plot (Rdocumentation, 2018)
results_plot = results %>%
  mutate(var1 = recode(var1,'F1_ket_effect'='Ownership',
                       'F2_ket_effect'='Agency',
                       'F3_ket_effect'='Loss of hand',
                       'delta_amnesia'='Amnesia',
                       'delta_depersonalization'='Depersonalisation',
                       'delta_derealisation'='Derealisation'),
         var2 = recode(var2,'F1_ket_effect'='Ownership',
                       'F2_ket_effect'='Agency',
                       'F3_ket_effect'='Loss of hand',
                       'delta_amnesia'='Amnesia',
                       'delta_depersonalization'='Depersonalisation',
                       'delta_derealisation'='Derealisation'))

#Set the order of variables to appear in the heatmap
var_order = c('Ownership', 'Agency', 'Loss of hand', 'Amnesia', 'Depersonalisation', 'Derealisation')

#Making an empty 6x6 matrix to put the values in for the corrplot
M = matrix(NA, nrow = 6, ncol = 6, dimnames = list(var_order, var_order))

#Put 1's on the diagonal, as they will be perfectly correlated with themselves
diag(M) = 1

#Fill in the correlations from results_plot
for (i in 1:nrow(results_plot)) { #Loop over each row (each row contains one correlation pair)
  v1 = results_plot$var1[i] #Extracting name of variable 1
  v2 = results_plot$var2[i] #Extracting name of variable 2
  M[v1, v2]= results_plot$rho[i] #Putting rho coefficient of current row at position [v1,v2]
  M[v2, v1]= results_plot$rho[i]} #Filling in the mirrored side

col = colorRampPalette(c('#BB4444', '#EE9988', '#FFFFFF', '#77AADD', '#4477AA'))

#Plotting correlation heatmap 
corrplot(M, method = 'color',
         type = 'lower',
         col = col(200),
         addCoef.col = 'black',
         number.cex = 0.8,
         tl.col = 'black',
         tl.srt = 45,
         col.lim = c(-1, 1))

#References
#Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear mixed-effects models using lme4. Journal of Statistical Software, 67(1), 1–48. https://doi.org/10.18637/jss.v067.i01
#Chang  , C., Leeper, T. J., Becker, J., & Schoch, D. (2024). A Swiss-Army Knife for Data I/O [R package rio version 1.2.3]. R-Project.org. https://cran.r-project.org/package=rio
#Datacamp. (n.d.). Extracting coefficients | R. Datacamp.com. https://campus.datacamp.com/courses/hierarchical-and-mixed-effects-models-in-r/linear-mixed-effect-models?ex=9
#FJCC. (2024, April 5). How to create a for loop to perform correlation analysis in R? Posit Community. https://forum.posit.co/t/how-to-create-a-for-loop-to-perform-correlation-analysis-in-r/185289
#Goldway, N., Hendler, T., Jalon, I., Pasternak, Y., Sar-El, R., Mirelman, D., Sarna, N., Green, N., Agbaria, Y., & Sharon, H. (2025). The Analgesic and Dissociative Properties of Ketamine are Separate and Correspond to Distinct Neural Mechanisms. BioRxiv. https://doi.org/10.1101/2025.07.25.666594
#jm_t. (2019, October 29). Pair wise cor.test based on combinations of variables. Posit Community. https://forum.posit.co/t/pair-wise-cor-test-based-on-combinations-of-variables/43468
##Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). LmerTest package: Tests in linear mixed effects models. Journal of Statistical Software, 82(13), 1–26. https://doi.org/10.18637/jss.v082.i13
#Laerd Statistics. (2018). Spearman’s Rank-Order Correlation. Laerd Statistics. https://statistics.laerd.com/statistical-guides/spearmans-rank-order-correlation-statistical-guide.php
#Müller , K., & Wickham, H. (2023). Simple Data Frames [R package tibble version 3.1.3]. Cran.r-Project.org. https://cran.r-project.org/web/packages/tibble/index.html
#R.documentation. (2025). R: Test for Association/Correlation Between Paired Samples. Ethz.ch. https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.test.html
#Rdocumentation. (n.d.-a). cor.test function - RDocumentation. Www.rdocumentation.org. https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/cor.test
#Rdocumentation. (n.d.-b). R: Generate All Combinations of n Elements, Taken m at a Time. Ethz.ch. https://stat.ethz.ch/R-manual/R-devel/library/utils/html/combn.html
#Rdocumentation. (2018). recode function - RDocumentation. Rdocumentation.org. https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/recode
#Revelle, W. (2024). psych: Procedures for Psychological, Psychometric, and Personality Research. https://cran.r-project.org/package=psych. R package version 2.4.6.
#Revelle, W. (2025). Procedures for Psychological, Psychometric, and Personality Research. https://personality-project.org/r/psych/psych-manual.pdf
#Schober, P., Boer, C., & Schwarte, L. A. (2018). Correlation Coefficients: Appropriate Use and Interpretation. Anesthesia & Analgesia, 126(5), 1763–1768. https://doi.org/10.1213/ANE.0000000000002864
#Siddiqui, N. (2026). How to avoid the warning “Cannot compute exact p-value with ties” while perform correlation test for Spearman’s correlation in R? Tutorialspoint.com. https://www.tutorialspoint.com/article/how-to-avoid-the-warning-cannot-compute-exact-p-value-with-ties-while-perform-correlation-test-for-spearman-s-correlation-in-r
#STHDA. (2025). Visualize correlation matrix using correlogram - Easy Guides - Wiki - STHDA. Sthda.com. https://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
#Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L., François, R., Grolemund, G., Hayes, A., Henry, L., Hester, J., Kuhn, M., Pedersen, T., Miller, E., Bache, S., Müller, K., Ooms, J., Robinson, D., Seidel, D., Spinu, V., & Takahashi, K. (2019). Welcome to the Tidyverse. Journal of Open Source Software, 4(43), 1686. https://doi.org/10.21105/joss.01686
##Wickham, H., & Bryan, J. (2025, March 7). readxl: Read Excel Files. R-Packages. https://cran.r-project.org/web/packages/readxl/index.html
#Wei, T., & Simko, V. (2021, November 18). An Introduction to corrplot Package. Cran.r-Project.org. https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
#Wei, T., & Simko, V. (2024). R package “corrplot”: Visualization of a Correlation Matrix. GitHub. https://github.com/taiyun/corrplot

#Ai declaration
#Ai was used to suggest fixes to debug code, as well as to aid understanding of
#how functions work. Also to check spelling and grammar of comments. 

#Anthropic. (2025). Claude. Claude.ai. https://claude.ai
#Grammarly. (2025). Grammarly. Grammarly.com. https://app.grammarly.com/
#OpenAI. (2025). ChatGPT. ChatGPT; OpenAI. https://chatgpt.com/