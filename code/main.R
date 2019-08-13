##################################################

# The Stability of Conditional Cooperation
# Andreozzi, Ploner, Saral

# For inquiries, please write to Ali Seyhun Saral (www.saral.it)

##################################################


message("Loading packages")
library("here")
library("dplyr")
library("ggplot2")
library("tidyr")
library("stringr")
library("vipor")
library("ggrepel") # nice placement of geom_text
library("forcats")
library("purrr")
library("gmodels")
library("forcats")# ci function
library("lme4")
library("lmerTest")
library("texreg")
library("xtable")
library("nnet")
library("optimx")
library("ggalluvial")

# Getting working directory
dir_proj <- here::here()
setwd(dir_proj)
cat('Working directory is:', getwd(), "\n")

# Loading functions
source(file.path(".","code","functions.R"))

# Loading data
# Data is already processed. (see process_data.R)
cat("Loading data files...")
subjects <- readRDS(file.path(".","data","rds","subjects.rds"))
quest <- readRDS(file.path(".","data","rds","quest.rds"))
cat("DONE \n")


# Setting ggplot2 theme
theme_paper  <-  theme_linedraw() +
  theme(legend.position="bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed",color = "#dddddd", size = 0.4),
        text = element_text(size=14)) +
  theme(strip.background =element_rect(fill="#dddddd"))+
  theme(strip.text = element_text(colour = "black"))

theme_set(theme_paper)


# Getting common parameters
# These are generally useful for plot parameters
periods_range  <- range(subjects$period)
periods_seq  <- sort(unique(subjects$period))
number_of_subjects  <- length(unique(subjects$subject_unq))

#this is used to convert counts to frequencies in plots
axis_formatter  <- function(x) {round(x/number_of_subjects,digits=2)}



########################################
# Figure 2 - Average Earnings
########################################
tbl_avg_earnings_trt <-
  subjects %>%
  dplyr::group_by(period,treatment) %>%
  dplyr::summarise(avg_earning = mean(earning_game),
                   sd_earning = sd(earning_game),
                   ci_lower = ci(earning_game)[2],
                   ci_upper = ci(earning_game)[3],
                   n = n()
                   )  %>%
  dplyr::mutate( se_earning = sd_earning / sqrt(n))


ggplot2::ggplot(tbl_avg_earnings_trt, aes(y=avg_earning, x=period, color=treatment,shape = treatment, )) +
  scale_x_continuous("Period",breaks = periods_seq)+
  scale_y_continuous("Average Earnings (+/- std.err)",breaks=seq(100,300,25), limits = c(100,200))+
  scale_color_grey(start = 0.05, end = 0.6) +
  geom_line(alpha = 0.5) +
  geom_point(size = 3 , alpha = 0.9) +
  geom_errorbar(aes(ymin=avg_earning-se_earning, ymax=avg_earning+se_earning), alpha = 0.7, width=0.4) + # Standard error
  labs(color  = "Treatment", linetype = "Treatment", shape = "Treatment") +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=7))+
  theme(legend.text=element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

pdf_last_plot("fig_average_earnings")
#------------------------------------------------------------






########################################
# Figure 3 - Conditoinal Classifications over Periods
########################################

new_levels  <- c("selfish", "perf-cond-coop", "imp-cond-coop", "humped", "other")
new_level_indexes  <- match(new_levels,get_label_table(classification = "wide_o")[,"shortname"])


subjects  %>%
  select(subject_unq, period, cond_class_wide_o, treatment)  %>%
  spread(period, cond_class_wide_o)  %>%
  arrange(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`)  %>%
  group_by(treatment)  %>%
  mutate(subject_unq = n():1)  %>%
  gather(period, type, -subject_unq, -treatment)  %>%
  mutate( type = factor(type, levels = new_levels), period = as.integer(period))  %>%
  ggplot(aes(y = as.factor(subject_unq) , x = period, z= type)) +
  geom_tile(aes(fill = type),color="#000000") +
  scale_fill_manual("",values=get_label_table(classification = "wide_o")[,"color"][new_level_indexes]
                   ,labels = str_c(" ",  get_label_table(classification = "wide_o")[,"longname"][new_level_indexes], "  "))+
  scale_x_continuous("Period", breaks = 1:10) +
  facet_grid(~treatment) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



pdf_last_plot("transition_subject", width = 7, height = 8 )
##------------------------------------------



########################################
# Figure 4 - Conditional Strategies over Periods
########################################


tbl_strategies  <- subjects %>%
  dplyr::group_by(period, treatment) %>%
  dplyr::mutate(total_play=n()) %>%
  dplyr::group_by(period,cond_type, treatment) %>%
  dplyr::summarise(relative_freq=n()/mean(total_play)) %>%dplyr::mutate(cond_class_narrow=classify_type_vector(cond_type, classification = "narrow")) %>%
  dplyr::rowwise() %>% # <3
  dplyr::mutate(cond_type_letter = convert_type(cond_type,conv = "10tL"))  
  ggplot(tbl_strategies, aes(y=relative_freq,x=period,group=cond_type,color=cond_class_narrow)) +
  geom_line(size=0.5,alpha=0.8) +
  geom_point(size = 2) +
  geom_text_repel(data=filter(tbl_strategies,(period==1 | period==10) & relative_freq>0.03),
                  aes(label=cond_type_letter,y=relative_freq+0.00),
                  show.legend = FALSE,size=4,angle=0,
                  box.padding = unit(0.25, "lines"),
                  fontface = 'bold'
                  ) +  scale_color_manual(values=get_label_table(classification="narrow")[,"color"],labels =get_label_table(classification = "narrow")[,"longname"],name="types") +
  facet_grid(~treatment) +
  scale_x_continuous("Period",breaks = seq(1,10)) +
  guides(color=guide_legend(title=NULL))+
  scale_y_continuous("Fraction of subjects",breaks = seq(0,1,0.1),limits=c(0,0.7)) +
  theme(legend.text=element_text(size=10))

pdf_last_plot("fig_types_treatment", width = 8, height = 4)
#-----------------------------------------------------------


########################################
# Table 2 - Stability
########################################

message("##########################")
message("# Table 2 - Stability")



# Calculating stability measures and saving in a separate df
# We calculated stability in several methods
# The simple method in the paper is mode_lasthalf
df_stability  <- subjects  %>%
  select(subject_unq, cond_type, cond_class_wide, cond_class_narrow, cond_class_wide_o)  %>%
  group_by(subject_unq)  %>%
  summarise(
    first_type  = get_first(cond_type),
    first_class_wide = get_first(cond_class_wide),
    first_class_wide_o = get_first(cond_class_wide_o),
    first_class_narrow = get_first(cond_class_narrow),
    # Stability measure for types
    stb_psi_type =measure_stability(cond_type,method = "psi"),
    stb_and_type =measure_stability(cond_type,method = "andreozzi"),
    stb_mode_type =measure_stability(cond_type,method = "mode"),
    stb_mode_lasthalf_type =measure_stability(cond_type,method = "mode_lasthalf"),
    # Stability measure for wide classification
    stb_psi_class_wide =measure_stability(cond_class_wide,method = "psi"),
    stb_and_class_wide =measure_stability(cond_class_wide,method = "andreozzi"),
    stb_mode_class_wide =measure_stability(cond_class_wide,method = "mode"),
    stb_mode_lasthalf_class_wide =measure_stability(cond_class_wide,method = "mode_lasthalf"),
    # Stabilty measure for wide_o
    stb_psi_class_wide_o =measure_stability(cond_class_wide_o,method = "psi"),
    stb_and_class_wide_o =measure_stability(cond_class_wide_o,method = "andreozzi"),
    stb_mode_class_wide_o =measure_stability(cond_class_wide_o,method = "mode"),
    stb_mode_lasthalf_class_wide_o =measure_stability(cond_class_wide_o,method = "mode_lasthalf"),
    # Stability measure for types
    stb_psi_class_narrow =measure_stability(cond_class_narrow,method = "psi"),
    stb_and_class_narrow =measure_stability(cond_class_narrow,method = "andreozzi"),
    stb_mode_class_narrow =measure_stability(cond_class_narrow,method = "mode"),
    stb_mode_lasthalf_class_narrow =measure_stability(cond_class_narrow,method = "mode_lasthalf")
  )



####
# Table 2 Left Side Contingency
####

tbl_contingency_mode_lasthalf  <- table(df_stability$first_class_wide_o, df_stability$stb_mode_lasthalf_class_wide)


####
# Table 2 Right Side, Comparision
####
pvalues_df  <- data.frame()
types  <- c("selfish","perf-cond-coop","imp-cond-coop","humped","other")
ptable  <- matrix(nrow = 5, ncol = 5, dimnames = list(types,types))

tests <- rbind(
                c("selfish","imp-cond-coop"),
                c("selfish","perf-cond-coop"),
                c("selfish","humped"),
                c("selfish","other"),
                c("imp-cond-coop","humped"),
                c("imp-cond-coop","other"),
                c("perf-cond-coop","imp-cond-coop"),
                c("perf-cond-coop","humped"),
                c("perf-cond-coop","other"),
                c("humped","other")
            )



d.chi <-
df_stability %>% group_by(first_class_wide_o) %>% count(stb_mode_lasthalf_class_wide) %>% spread(stb_mode_lasthalf_class_wide,n)


for (i in 1:length(tests[,1])){

  chisqtest  <-     chisq.test(
    bind_rows(
      d.chi %>% filter(first_class_wide_o==tests[i,1]) %>% ungroup() %>% select(-first_class_wide_o),
      d.chi %>% filter(first_class_wide_o==tests[i,2]) %>% ungroup() %>% select(-first_class_wide_o))
  )

  ptable[tests[i,2],tests[i,1]]  <- chisqtest$p.value


  pvalues_df  <- rbind(pvalues_df, data.frame(group1 = tests[i,1], group2 = tests[i,2], p = chisqtest$p.value, y.position = 3))
}


ptable_rounded  <- round(ptable,4)
#xtable(ptable_rounded, digits = 4 )
#print(ptable_rounded)

stability_lasthalf_comparision  <- tbl_contingency_mode_lasthalf[c(1,3,2,4),c(2,1)]  %>% as.matrix()  %>% cbind(ptable_rounded[1:4,1:4])

rownames(stability_lasthalf_comparision)  <- c("Selfish", "Perf. Cond. Coop", "Imp. Cond.", "Humped")

colnames(stability_lasthalf_comparision)  <- c( "Stable", "Unstable", "Selfish", "Perf. Cond.", "Imp. Cond.", "Humped")


##xtable(stability_lasthalf_comparision, digits = 3)
print(stability_lasthalf_comparision)


#------------------------------------------


########################################
# Table 3  - Decline of Conditional Cooperation
########################################
message("##########################")
message("# Table 3 - Decline of Conditional Cooperation")


### ----------- Regression isCC

subjects_regression  <-  subjects  %>%
  mutate( opp_action_lag1_fact = as.factor(opp_action_lag1),
          exp_pay_uc_norm = exp_pay_uc / 300,
          exp_pay_ccL_norm = exp_pay_ccL / 300,
          exp_pay_ccM_norm = exp_pay_ccM / 300,
          exp_pay_ccH_norm = exp_pay_ccH / 300,
          observed_opp_cond_class = if_else(treatment == "CondInfo" & first_player_lag1 == "1", as.character(opp_cond_class_narrow_lag1), as.character("not-observed")),
          observed_opp_cond_class = relevel(factor(observed_opp_cond_class,
                                                   levels = c("not-observed", "cond-coop", "selfish", "humped", "other")), ref = "not-observed")
)


reg_iscc_model1 <- glmer( formula = isCC ~  period + treatment + (1 | subject_unq), family=binomial, data = subjects_regression)
reg_iscc_model1_nlminb <- glmer( formula = isCC ~  period + treatment + (1 | subject_unq), family=binomial, data = subjects_regression, glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
reg_iscc_model2 <- glmer( formula = isCC  ~  period * treatment + (1 | subject_unq), family=binomial, data = subjects_regression)

models_table  <- list(reg_iscc_model1, reg_iscc_model2)



# texreg(models_table, single.row = TRUE,
#       custom.coef.names = c(NA, "Period","Treatment \\textit{CondInfo}","Period:Treatment \\textit{CondInfo}"))

screenreg(models_table)
####----------------------------------



########################################
# Figure S2  - Summary
########################################

facet_labels <- c(
  'ucf'="First move",
  'ccLf'="Response to L",
  'ccMf'="Response to M",
  'ccHf'="Response to H"
)


subjects  %>%
  select(subject_unq, period,treatment, ucf, ccLf, ccMf, ccHf)  %>%
  gather(response_to, choice, -subject_unq, -period, -treatment)  %>%
  mutate( response_to = factor(response_to, levels =  c("ucf", "ccLf", "ccMf", "ccHf")))  %>%
  mutate( choice = factor(choice, levels = c("H", "M", "L")))  %>%
  ggplot(
    aes(x = period, fill = choice)) +
  geom_bar(position = "fill") +
  facet_grid(~ response_to, labeller = as_labeller(facet_labels) ) +
  scale_fill_manual("",limits=c("L","M","H"), values = c('red','dodgerblue','darkblue')) +
  scale_y_continuous("Fraction") +
  scale_x_continuous("Period",breaks = 1:10)

pdf_last_plot("actions_by_period", width = 9, height = 3)

#-------------------------------------------



########################################
# Fig  S3  - Sankey Plot of Conditional Classification
########################################


# Below is the trick for rearranging the positions.
new_levels  <- c("selfish", "perf-cond-coop", "imp-cond-coop", "humped", "other")
new_level_indexes  <- match(new_levels,get_label_table(classification = "wide_o")[,"shortname"])

subjects  %>%
  ggplot(
    aes(x = as.factor(period),
        stratum = factor(cond_class_wide_o,levels = new_levels),
        alluvium = subject_unq,
        fill = factor(cond_class_wide_o,levels = new_levels))) +
  geom_flow() +
  geom_stratum() +
  scale_fill_manual("",values=get_label_table(classification = "wide_o")[,"color"][new_level_indexes]
                   ,labels = str_c(" ",  get_label_table(classification = "wide_o")[,"longname"][new_level_indexes], "  ")) +
  scale_x_discrete("Period") +
  theme(axis.text.y=element_blank(),axis.title.y=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_grid(~treatment)

pdf_last_plot("transition_treatment", height = 6, width = 10)


facet_labels <- c(
  'ucf'="First move",
  'ccLf'="Response to L",
  'ccMf'="Response to M",
  'ccHf'="Response to H"
)
#----------------------------------


########################################
# Table S2  - Beliefs on Cond Types
########################################

message("##########################")
message("# Table S2 - Beliefs on Cond Types")


subjects_regression2  <- subjects  %>%
  mutate(bel_uc = exp_pay_uc/300, bel_ccL = exp_pay_ccL/300, bel_ccM = exp_pay_ccM/300, bel_ccH = exp_pay_ccH/300, opp_action_resc = opp_action_lag1/100)  

bel_model1  <- glmer(isCC ~ period +  treatment + bel_uc + bel_ccL + bel_ccM + bel_ccH + (1 | subject_unq), data = subjects_regression2, family = "binomial", glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
bel_model2  <- glmer(isS ~ period +  treatment + bel_uc + bel_ccL + bel_ccM + bel_ccH + (1 | subject_unq), data = subjects_regression2, family = "binomial", glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
bel_model3  <- glmer(isHS ~ period +  treatment + bel_uc + bel_ccL + bel_ccM + bel_ccH + (1 | subject_unq), data = subjects_regression2, family = "binomial", glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))


models2  <- list(bel_model1,bel_model2,bel_model3)

screenreg(list(bel_model1,bel_model2,bel_model3))


#texreg(models2, single.row = TRUE,
#       custom.coef.names = c(
#           NA,
#           "Period",
#           "Treatment \\textit{CondInfo}",
#           "Belief - Unconditional",
#           "Belief - Response to L",
#           "Belief - Response to M",
#           "Belief - Response to H"
#       ), custom.model.names = c("isCondCoop","isSelfish","isHumpShaped"))

#-------------------------------------




########################################
# Fig S4-S7 - Beliefs vs. Actual
########################################

action_categories  <- c("uc", "ccL", "ccM", "ccH")

for (action_variable in action_categories) {
#print(action_variable)
belief_variable  <- paste0("exp_pay_",action_variable)

subjects  %>%
  group_by(period, treatment)  %>%
  summarise(Belief = mean(!!sym(belief_variable)), Actual = mean(!!sym(action_variable)))  %>% 
  mutate(Belief = Belief/3)  %>% 
  gather(type, value, Actual, Belief)  %>%
    ggplot(aes(y=value,x = period,  linetype = type, color = type)) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_grey(start = 0.05, end = 0.6) +
  scale_alpha_manual(values = c(0.4,1)) +
  facet_grid(~treatment) +
  scale_x_continuous("Period",breaks = periods_seq) +
  scale_y_continuous("Average Transfer/Average Expected Transfer",breaks=seq(0,100,25), limits = c(0,100)) +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=7))+
  theme(legend.text=element_text(size=12),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank())

pdf_last_plot(paste0("beliefs_",action_variable), width = 8, height = 5)
}
#------------------------------------------------
