#Comparing models to see if the inclusion of an interaction with synchroncity
#and affective touch (speed) with Ketamine, improves model fit. 
library(ggplot2)
library(readxl)
library(lmerTest)


df_with_scores_fa = read_excel('df_with_scores_fa_real_2.xlsx')
View(df_with_scores_fa)

############################# Model Comparison #################################
#All models were estimated with ML not REML, as for BIC model comparison, when the 
#fixed effects are not the same, models should be estimated with ML (Jones, 2011). 

#Ownership with (session|subject) random effect
own_main_0 = lmerTest::lmer(F1 ~ session + (session| subji), data = df_with_scores_fa, REML = FALSE)
own_main_1 = lmerTest::lmer(F1 ~ session + sync  + (session | subji), data = df_with_scores_fa, REML = FALSE)
own_main_2 = lmerTest::lmer(F1 ~ session + speed + (session | subji), data = df_with_scores_fa, REML = FALSE)
own_main_3 = lmerTest::lmer(F1 ~ session + sync + speed + (session | subji), data = df_with_scores_fa, REML = FALSE)
own_main_4 = lmerTest::lmer(F1 ~ session * sync  + (session | subji), data = df_with_scores_fa, REML = FALSE)
own_main_5 = lmerTest::lmer(F1 ~ session * speed + (session | subji), data = df_with_scores_fa, REML = FALSE)
own_main_6 = lmerTest::lmer(F1 ~ session * sync * speed + (session | subji), data = df_with_scores_fa, REML = FALSE)

#Ownership with only (1 | subject) random effect
own_main_0_v2 = lmerTest::lmer(F1 ~ session + (1 | subji), data = df_with_scores_fa, REML = FALSE)
own_main_1_v2 = lmerTest::lmer(F1 ~ session + sync + (1 | subji), data = df_with_scores_fa, REML = FALSE)
own_main_2_v2 = lmerTest::lmer(F1 ~ session + speed + (1 | subji), data = df_with_scores_fa, REML = FALSE)
own_main_3_v2 = lmerTest::lmer(F1 ~ session + sync + speed + (1 | subji), data = df_with_scores_fa, REML = FALSE)
own_main_4_v2 = lmerTest::lmer(F1 ~ session * sync + (1 | subji), data = df_with_scores_fa, REML = FALSE)
own_main_5_v2 = lmerTest::lmer(F1 ~ session * speed + (1 | subji), data = df_with_scores_fa, REML = FALSE)
own_main_6_v2 = lmerTest::lmer(F1 ~ session * sync * speed + (1 | subji), data = df_with_scores_fa, REML = FALSE)

#BIC model comparison for ownership
BIC(own_main_0, own_main_1, own_main_2, own_main_3, own_main_4, own_main_5, own_main_6,
    own_main_0_v2, own_main_1_v2, own_main_2_v2, own_main_3_v2, own_main_4_v2, own_main_5_v2, own_main_6_v2)

#Sorting from lowest to highest
#own_main_3 = 734.6380
#own_main_2 = 739.9347
#own_main_5 = 745.2752
#own_main_1 = 747.7058
#own_main_0 = 752.1228
#own_main_6 = 752.3498
#own_main_4 = 753.3734

#For all models the (session | subji) random effects structure has the lowest BIC

#Since the lowest BIC model it not the simplest I will conduct a likelihood ratio test
#to compare models

anova(own_main_3,own_main_2)
#p = .001, indicating that the model including session, synchrony, and speed
#provided a better fit than the model including only session and speed.

#Agency with (session | subject) random effect
ag_main_0 = lmerTest::lmer(F2 ~ session + (session| subji), data = df_with_scores_fa, REML = FALSE)
ag_main_1 = lmerTest::lmer(F2 ~ session + sync  + (session | subji), data = df_with_scores_fa, REML = FALSE)
ag_main_2 = lmerTest::lmer(F2 ~ session + speed + (session | subji), data = df_with_scores_fa, REML = FALSE)
ag_main_3 = lmerTest::lmer(F2 ~ session + sync + speed + (session | subji), data = df_with_scores_fa, REML = FALSE)
ag_main_4 = lmerTest::lmer(F2 ~ session * sync  + (session | subji), data = df_with_scores_fa, REML = FALSE)
ag_main_5 = lmerTest::lmer(F2 ~ session * speed + (session | subji), data = df_with_scores_fa, REML = FALSE)
ag_main_6 = lmerTest::lmer(F2 ~ session * sync * speed + (session | subji), data = df_with_scores_fa, REML = FALSE)

#Agency with (1|subject) random effect
ag_main_0_v2 = lmerTest::lmer(F2 ~ session + (1 | subji), data = df_with_scores_fa, REML = FALSE)
ag_main_1_v2 = lmerTest::lmer(F2 ~ session + sync + (1 | subji), data = df_with_scores_fa, REML = FALSE)
ag_main_2_v2 = lmerTest::lmer(F2 ~ session + speed + (1 | subji), data = df_with_scores_fa, REML = FALSE)
ag_main_3_v2 = lmerTest::lmer(F2 ~ session + sync + speed + (1 | subji), data = df_with_scores_fa, REML = FALSE)
ag_main_4_v2 = lmerTest::lmer(F2 ~ session * sync + (1 | subji), data = df_with_scores_fa, REML = FALSE)
ag_main_5_v2 = lmerTest::lmer(F2 ~ session * speed + (1 | subji), data = df_with_scores_fa, REML = FALSE)
ag_main_6_v2 = lmerTest::lmer(F2 ~ session * sync * speed + (1 | subji), data = df_with_scores_fa, REML = FALSE)

#BIC model comparison for agency
BIC(ag_main_0, ag_main_1, ag_main_2, ag_main_3, ag_main_4, ag_main_5, ag_main_6,
    ag_main_0_v2, ag_main_1_v2, ag_main_2_v2, ag_main_3_v2, ag_main_4_v2, ag_main_5_v2, ag_main_6_v2)

#For all models the (session | subji) random effects structure has the lowest BIC

#BIC ordered lowest to highest
#ag_main_1 = 687.9689
#ag_main_0 = 690.4060
#ag_main_3 = 691.6363
#ag_main_4 = 693.4606
#ag_main_2 = 694.1468
#ag_main_5 = 698.9204
#ag_main_6 = 709.2030

#Since the lowest BIC model it not the simplest I will conduct a likelihood ratio test
#to compare models

anova(ag_main_1,ag_main_0)

#p = .004, indicating that the model including session and synchrony fit better 
#than the model including session alone.

#Loss of hand with (session | subject) random effect
loss_main_0 = lmerTest::lmer(F3 ~ session + (session| subji), data = df_with_scores_fa, REML = FALSE)
loss_main_1 = lmerTest::lmer(F3 ~ session + sync  + (session | subji), data = df_with_scores_fa, REML = FALSE)
loss_main_2 = lmerTest::lmer(F3 ~ session + speed + (session | subji), data = df_with_scores_fa, REML = FALSE)
loss_main_3 = lmerTest::lmer(F3 ~ session + sync + speed + (session | subji), data = df_with_scores_fa, REML = FALSE)
loss_main_4 = lmerTest::lmer(F3 ~ session * sync  + (session | subji), data = df_with_scores_fa, REML = FALSE)
loss_main_5 = lmerTest::lmer(F3 ~ session * speed + (session | subji), data = df_with_scores_fa, REML = FALSE)
loss_main_6 = lmerTest::lmer(F3 ~ session * sync * speed + (session | subji), data = df_with_scores_fa, REML = FALSE)

#Loss of hand with (1 | subject) random effect
loss_main_0_v2 = lmerTest::lmer(F3 ~ session + (1 | subji), data = df_with_scores_fa, REML = FALSE)
loss_main_1_v2 = lmerTest::lmer(F3 ~ session + sync + (1 | subji), data = df_with_scores_fa, REML = FALSE)
loss_main_2_v2 = lmerTest::lmer(F3 ~ session + speed + (1 | subji), data = df_with_scores_fa, REML = FALSE)
loss_main_3_v2 = lmerTest::lmer(F3 ~ session + sync + speed + (1 | subji), data = df_with_scores_fa, REML = FALSE)
loss_main_4_v2 = lmerTest::lmer(F3 ~ session * sync + (1 | subji), data = df_with_scores_fa, REML = FALSE)
loss_main_5_v2 = lmerTest::lmer(F3 ~ session * speed + (1 | subji), data = df_with_scores_fa, REML = FALSE)
loss_main_6_v2 = lmerTest::lmer(F3 ~ session * sync * speed + (1 | subji), data = df_with_scores_fa, REML = FALSE)

#BIC model comparison for loss of hand
BIC(loss_main_0, loss_main_1, loss_main_2, loss_main_3, loss_main_4, loss_main_5, loss_main_6,
    loss_main_0_v2, loss_main_1_v2, loss_main_2_v2, loss_main_3_v2, loss_main_4_v2, loss_main_5_v2, loss_main_6_v2)

#For all models the (session | subji) random effects structure has the lowest BIC

#BIC ordered lowest to highest
#loss_main_2 = 643.7469
#loss_main_5 = 647.4073
#loss_main_3 = 649.3642
#loss_main_0 = 663.0023
#loss_main_6 = 668.0234
#loss_main_1 = 668.6259
#loss_main_4 = 674.2593

#Since the smallest BIC model, is also the simplest compared to the next best BIC
#no likelihood ratio test will be conducted

#######################Plotting the BIC scores of models#######################
#Code adapted from (Bobbitt, 2022; Scherer, 2021; Posit, n.d.; ggplot2, n.d.-a; ggplot2, n.d.-b ; ggplot2, n.d.-c)
#Since, the (session | subject) models were universally better we will only plot these models

#Making titles for model names (same 7 models across ownership, agency and loss of hand)
model_labels = c(
  '1) Session main effect only',
  '2) Session and synchrony main effects only',
  '3) Session and speed main effects only',
  '4) Session, speed and synchrony main effects only',
  '5) Session and synchrony main effects + interaction',
  '6) Session and speed main effects + interaction',
  '7) Session, speed, synchrony main effects + interaction')

#Putting the models into lists, to then 
own_models = list(own_main_0, own_main_1, own_main_2, own_main_3, own_main_4, own_main_5, own_main_6)
ag_models = list(ag_main_0, ag_main_1, ag_main_2, ag_main_3, ag_main_4, ag_main_5, ag_main_6)
loss_models = list(loss_main_0, loss_main_1, loss_main_2, loss_main_3, loss_main_4, loss_main_5, loss_main_6)

own_bic = BIC(own_main_0, own_main_1, own_main_2, own_main_3,own_main_4, own_main_5, own_main_6)
ag_bic = BIC(ag_main_0, ag_main_1, ag_main_2, ag_main_3, ag_main_4, ag_main_5, ag_main_6)
loss_bic = BIC(loss_main_0, loss_main_1, loss_main_2, loss_main_3, loss_main_4, loss_main_5, loss_main_6)

#Making a dataframe of BIC values, model number and model name
bic_df = bind_rows(
  data.frame(
    set = 'Ownership',
    model_num = 1:7,
    model_label = model_labels,
    bic = own_bic$BIC),
  data.frame(
    set = 'Agency',
    model_num = 1:7,
    model_label = model_labels,
    bic = ag_bic$BIC),
  data.frame(
    set = 'Loss of hand',
    model_num = 1:7,
    model_label = model_labels,
    bic = loss_bic$BIC))

#Order bars within each set from lowest BIC to highest BIC (Dplyr, n.d.; GeeksforGeeks, 2024)
bic_df = bic_df %>%
  group_by(set) %>% #treat each set separately when ordering 
  arrange(bic, .by_group = TRUE) %>% #Sorts within each set lowest BIC to highest BIC
  mutate(order_in_set = row_number()) %>% #Make a new column to define the order in 
  #which the models should be ranked within the set
  ungroup()

#Making positions on the x-axis so the 3 sets are in separate bar chart clusters 
offsets = c('Ownership' = 0, 'Agency' = 8, 'Loss of hand' = 16)

#Making xpos column so that the models are in order of BIC (best to worse)
#in their sets
bic_df = bic_df %>%
  mutate(xpos = order_in_set + offsets[set])

#Establishing colours for models 1-7 across all three sets, so it easy to read (Rpubs, 2024)
model_cols = c('1' = 'lightseagreen','2' = 'darkorange2','3' = '#7560B4', '4' = 'violetred', '5' = 'olivedrab4','6' = 'goldenrod2','7' = 'goldenrod4')

#Model num needs to be a factor, otherwise ggplot will treat it as continuous
bic_df = bic_df %>% 
  mutate(model_num = factor(model_num))

#Plotting
ggplot(bic_df, aes(x = xpos, y = bic, fill = model_num)) +
  geom_col(width = 0.8) + #Adding in bars
  geom_text(aes(label = model_num), vjust = -0.4, size = 3) + #Put the model number above the bar
  scale_fill_manual(values = model_cols, name = 'Model', labels = model_labels) +
  scale_x_continuous(breaks = c(4, 12, 20), labels = c('Ownership', 'Agency', 'Loss of hand')) + # Since xpos was labelled so that ownership is position 1-7, agency is position 9-15 etc, these x-titles will be at the centre of the set
  labs(x = NULL, y = 'BIC score') +
  theme_classic() +
  theme(legend.position = 'right')

#References
#Dplyr. (n.d.). Integer ranking functions — row_number. Tidyverse.org. https://dplyr.tidyverse.org/reference/row_number.html
#GeeksforGeeks. (2024, April 17). dplyr arrange() Function in R. GeeksforGeeks. https://www.geeksforgeeks.org/r-language/dplyr-arrange-function-in-r/
#ggplot2. (n.d.-a). Create your own discrete scale — scale_manual. Ggplot2.Tidyverse.org. https://ggplot2.tidyverse.org/reference/scale_manual.html
#ggplot2. (n.d.-b). Modify components of a theme — theme. Ggplot2.Tidyverse.org. https://ggplot2.tidyverse.org/reference/theme.html
#ggplot2. (n.d.-c). Position scales for continuous data (x & y) — scale_continuous. Ggplot2.Tidyverse.org. https://ggplot2.tidyverse.org/reference/scale_continuous.html
#Jones, R. H. (2011). Bayesian information criterion for longitudinal and clustered data. Statistics in Medicine, 30(25), 3050–3056. https://doi.org/10.1002/sim.4323
#Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). LmerTest package: Tests in linear mixed effects models. Journal of Statistical Software, 82(13), 1–26. https://doi.org/10.18637/jss.v082.i13
#Posit. (n.d.). Data visualization with ggplot2 :: Cheat Sheet. Rstudio.github.io. https://rstudio.github.io/cheatsheets/html/data-visualization.html
#Rpubs. (2024, October 28). RPubs - RGB and HEX Colors in R. Rpubs.com. https://rpubs.com/mbh/rgbhex
#Scherer, C. (2021). A Quick How-to on Labelling Bar Graphs in ggplot2. Cédric Scherer. https://www.cedricscherer.com/2021/07/05/a-quick-how-to-on-labelling-bar-graphs-in-ggplot2/
#Wickham, H. (2016). Create Elegant Data Visualisations Using the Grammar of Graphics. Tidyverse.org. https://ggplot2.tidyverse.org/
#Wickham, H., & Bryan, J. (2025, March 7). readxl: Read Excel Files. R-Packages. https://cran.r-project.org/web/packages/readxl/index.html

#Ai declaration
#Ai was used to suggest fixes to debug code, as well as to aid understanding of
#how functions work. Also to check spelling and grammar of comments. 

#Anthropic. (2025). Claude. Claude.ai. https://claude.ai
#Grammarly. (2025). Grammarly. Grammarly.com. https://app.grammarly.com/
#OpenAI. (2025). ChatGPT. ChatGPT; OpenAI. https://chatgpt.com/