#BPQ lmm
library(tidyverse) 
library(psych) 
library(lmerTest) 
library(rio) 
library(nlme) 
library(rstatix) 
library(ggplot2) 
library(performance)

#Assessing whether Ketamine significantly affects interoception measured by the 
#BPQ-vsf

#Importing data
df = import('BPQ_xcel.xlsx')

#Removing incomplete rows
clean_df = df[complete.cases(df),]
nrow(clean_df)

#Convert to long format, recode _1 = placebo and _2 = ketamine,and set factor
#levels so placebo is the reference (placebo, then ketamine)
df_long = clean_df %>%
  pivot_longer(
    cols = c(BPQBody_Awarenesssum_1_post, BPQBody_Awarenesssum_2_post),
    names_to = 'session',
    values_to = 'BPQ_score') %>%
  mutate(session = recode(session,'BPQBody_Awarenesssum_1_post' = 'placebo','BPQBody_Awarenesssum_2_post' = 'ketamine'),
         session = factor(session, levels = c('placebo','ketamine')))

#Conducting lmm
intro_lmm = lmerTest::lmer(BPQ_score ~ session + (1 | subji), data = df_long)
summary(intro_lmm)

#Confidence intervals (Rdocumentation, 2022a)
confint(intro_lmm, method = 'profile') #[-2.836129, 7.902796]

############################Assessing assumptions############################
#Checking heteroscedasticity, normality and singularity
check_heteroscedasticity(intro_lmm)

plot(fitted(intro_lmm), resid(intro_lmm))
abline(h = 0)

qqnorm(resid(intro_lmm))
qqline(resid(intro_lmm))

isSingular(intro_lmm)
summary(intro_lmm)

#Although there is some evidence of non-normality, this is unlikely to be a major concern
#(see Knief & Forstmeier, 2021). Heteroscedasticity appears to be a more
#concern, so I will fit models that explicitly allow unequal residual
#variances.

####################Adapting model for heteroscedasticity######################
#Code adapted from: (Magnusson, 2015; Pinheiro & Bates, 1998; Pustejovsky, 2024)
BPQ_lme = lme(
  fixed = BPQ_score ~ session,
  random = ~ 1 | subji,
  data = df_long,
  method = 'REML')

summary(BPQ_lme)

#Allowing different residual variance by session
BPQ_lme_het = update(
  BPQ_lme,
  weights = varIdent(form = ~ 1 | session))

summary(BPQ_lme_het)

#Extracting CI's
intervals(BPQ_lme_het)
#[-2.984071, 8.050737]

#Assessing heteroscedasticity
plot(fitted(BPQ_lme_het), resid(BPQ_lme_het))
abline(h = 0)

check_heteroscedasticity(BPQ_lme_het)

#Comparing results when allowing for variance in session
summary(BPQ_lme_het)
summary(intro_lmm)
#Allowing for variance in residuals did not significantly change results, Both showed
#the same results to 3dp, b=2.53, SE=2.70, t(29)=0.94, p=.355.

##################### Paired-wilcoxon test sensitivity test #####################
#Simple paired Wilcoxon as a non-parametric robustness check, as it is robust
#to heteroscadicisty and non-normality (Statistic Solutions, 2026)
wilcox_test(data = df_long, BPQ_score ~ session, paired = TRUE, detailed = TRUE)

#The results still shows insignficance (p = 0.428), consistent with the results 
#lmm. Strengthening evidence that ketamine does not signficantly effect bpq-vsf scores.

#Create boxplot with overlaid individual data points for BPQ scores by session 
#Code adapted from (Bobbitt, 2022; Thomas, 2020)
#Checking levels for plot
levels(df_long$session)

ggplot(df_long, aes(x = session, y = BPQ_score)) +
  geom_boxplot() +
  geom_point(aes(colour = session)) +
  scale_colour_manual(name = 'Session', values = c('placebo' = '#00BFC4', 'ketamine' = '#F8766D'), labels = c('placebo' = 'Placebo', 'ketamine' = 'Ketamine')) +
  labs(x = 'Session',y = 'BPQ-vsf score')+
  theme_classic()

#Median and IQR
df_long %>%
  group_by(session) %>%
  summarise(median = median(BPQ_score),
    q1 = quantile(BPQ_score,0.25),
    q3 = quantile(BPQ_score, 0.75))

#References
#Bobbitt, Z. (2022, March 12). A Complete Guide to the Default Colors in ggplot2. Statology. https://www.statology.org/ggplot-default-colors/
#Chang, C., Leeper, T. J., Becker, J., & Schoch, D. (2024). A Swiss-Army Knife for Data I/O [R package rio version 1.2.3]. R-Project.org. https://cran.r-project.org/package=rio
#Kassambara, A. (2023). rstatix: Pipe-Friendly Framework for Basic Statistical Tests. R-Packages. https://cran.r-project.org/web/packages/rstatix/index.html
#Knief, U., & Forstmeier, W. (2021). Violating the normality assumption may be the lesser of two evils. Behavior Research Methods, 53(6). https://doi.org/10.3758/s13428-021-01587-5
#Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). LmerTest package: Tests in linear mixed effects models. Journal of Statistical Software, 82(13), 1–26. https://doi.org/10.18637/jss.v082.i13
#Lüdecke, D., Ben-Shachar, M., Patil, I., Waggoner, P., & Makowski, D. (2021). performance: An R Package for Assessment, Comparison and Testing of Statistical Models. Journal of Open Source Software, 6(60), 3139. https://doi.org/10.21105/joss.03139
#Magnusson, K. (2015). Using R and lme/lmer to fit different two- and three-level longitudinal models. Rpsychologist.com. https://rpsychologist.com/r-guide-longitudinal-lme-lmer
#Pinheiro , J. C., & Bates, D. M. (1998). lme and nlme Mixed-Effects Methods and Classes for S and S-PLUS Version 3.0. https://www.stat.cmu.edu/~brian/720-2007-source/week07-08-ideas/pinheiro98mixedeffects-Sguide.pdf
#Pinheiro, J., Bates, D., & R-core Team. (2025). nlme: Linear and Nonlinear Mixed Effects Models. R-Packages. https://cran.r-project.org/web/packages/nlme/index.html
#Pustejovsky, J. E. (2024, December 28). A quirk of `nlme::varIdent`. James E. Pustejovsky. https://jepusto.com/posts/varIdent-function-in-nlme/
#Revelle, W. (2024). psych: Procedures for Psychological, Psychometric, and Personality Research. https://cran.r-project.org/package=psych. R package version 2.4.6.
#Rdocumentation. (2022). confint.merMod function - RDocumentation. Rdocumentation.org. https://www.rdocumentation.org/packages/lme4/versions/1.1-38/topics/confint.merMod
#Statistic Solutions. (2026). Assumptions of the Wilcoxon Sign Test. Statistics Solutions. https://www.statisticssolutions.com/free-resources/directory-of-statistical-analyses/assumptions-of-the-wilcox-sign-test/
#Thomas, J. (2020, July 27). Adding color to boxplot in ggplot2. Stack Overflow. https://stackoverflow.com/questions/63120335/adding-color-to-boxplot-in-ggplot2
#Wickham, H. (2016). Create Elegant Data Visualisations Using the Grammar of Graphics. Tidyverse.org. https://ggplot2.tidyverse.org/
#Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L., François, R., Grolemund, G., Hayes, A., Henry, L., Hester, J., Kuhn, M., Pedersen, T., Miller, E., Bache, S., Müller, K., Ooms, J., Robinson, D., Seidel, D., Spinu, V., & Takahashi, K. (2019). Welcome to the Tidyverse. Journal of Open Source Software, 4(43), 1686. https://doi.org/10.21105/joss.01686

#Ai declaration
#Ai was used to suggest fixes to debug code, as well as to aid understanding of
#how functions work. Also to check spelling and grammar of comments. 

#Anthropic. (2025). Claude. Claude.ai. https://claude.ai
#Grammarly. (2025). Grammarly. Grammarly.com. https://app.grammarly.com/
#OpenAI. (2025). ChatGPT. ChatGPT; OpenAI. https://chatgpt.com/
