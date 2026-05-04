
#Assessing the power required for these interactions
library(simr)
library(lmerTest)
library(rio)

df_with_scores_fa = import('df_with_scores_fa_real_2.xlsx')

#fitting the models
ag_main_4 = lmer(F2 ~ session * sync + (session | subji),data = df_with_scores_fa, REML = FALSE)
ag_main_5 = lmer(F2 ~ session * speed + (session | subji), data = df_with_scores_fa, REML = FALSE)
ag_main_6 = lmer(F2 ~ session * sync * speed + (session | subji), data = df_with_scores_fa, REML = FALSE)

summary(ag_main_4)
summary(ag_main_5)
summary(ag_main_6)

set.seed(123)

#Code adapted from Park et al., (2024)
#session:sync
power_4 = powerSim(ag_main_4,test = fixed('sessionp:syncsync', method = 't'),nsim = 500)
power_4

#session:speed
power_5 = powerSim(ag_main_5, test = fixed('sessionp:speedslow', method = 't'),nsim = 500)
power_5

#session:sync:speed
power_6 = powerSim(ag_main_6, test = fixed('sessionp:syncsync:speedslow', method = 't'), nsim = 500)
power_6

#loss of hand 
#fit models
loss_main_4 = lmer(F3 ~ session * sync + (session | subji),data = df_with_scores_fa, REML = FALSE)
loss_main_5 = lmer(F3 ~ session * speed + (session | subji), data = df_with_scores_fa, REML = FALSE)
loss_main_6 = lmer(F3 ~ session * sync * speed + (session | subji),data = df_with_scores_fa, REML = FALSE)

set.seed(123)

#session x sync
power_loss_4 = powerSim(loss_main_4,test = fixed('sessionp:syncsync', method = 't'),nsim = 500)
power_loss_4

#session x speed
power_loss_5 = powerSim(loss_main_5,test = fixed('sessionp:speedslow', method = 't'),nsim = 500)
power_loss_5

#session x sync x speed
power_loss_6 = powerSim(loss_main_6,test = fixed('sessionp:syncsync:speedslow', method = 't'),nsim = 500)
power_loss_6

#ownership
#fit models
own_main_4 = lmer(F1 ~ session * sync + (session | subji),data = df_with_scores_fa, REML = FALSE)
own_main_5 = lmer(F1 ~ session * speed + (session | subji),data = df_with_scores_fa, REML = FALSE)
own_main_6 = lmer(F1 ~ session * sync * speed + (session | subji), data = df_with_scores_fa, REML = FALSE)

set.seed(123)

#session x sync
power_own_4 = powerSim(own_main_4, test = fixed('sessionp:syncsync', method = 't'), nsim = 500)
power_own_4

#session x speed
power_own_5 = powerSim(own_main_5, test = fixed('sessionp:speedslow', method = 't'),nsim = 500)
power_own_5

#session x sync x speed
power_own_6 = powerSim(own_main_6, test = fixed('sessionp:syncsync:speedslow', method = 't'),nsim = 500)
power_own_6

#References
#Chang, C., Leeper, T. J., Becker, J., & Schoch, D. (2024). A Swiss-Army Knife for Data I/O [R package rio version 1.2.3]. R-Project.org. https://cran.r-project.org/package=rio
#Green, P., & MacLeod, C. J. (2016). SIMR: an R package for power analysis of generalized linear mixed models by simulation. Methods in Ecology and Evolution, 7(4), 493–498. https://doi.org/10.1111/2041-210x.12504
#Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). LmerTest package: Tests in linear mixed effects models. Journal of Statistical Software, 82(13), 1–26. https://doi.org/10.18637/jss.v082.i13
#Park, R. (2024, October 5). Post-Hoc Power Analysis for Multilevel Models in R. Stanford.edu. https://thechangelab.stanford.edu/tutorials/power-analysis/post-hoc-power-analysis-for-multilevel-models-in-r/

#Ai declaration
#Ai was used to suggest fixes to debug code, as well as to aid understanding of
#how functions work. Also to check spelling and grammar of comments. 

#Anthropic. (2025). Claude. Claude.ai. https://claude.ai
#Grammarly. (2025). Grammarly. Grammarly.com. https://app.grammarly.com/
#OpenAI. (2025). ChatGPT. ChatGPT; OpenAI. https://chatgpt.com/