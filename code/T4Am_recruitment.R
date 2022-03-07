###
# The purpose of this script is to analyze withdraw rates to predict applicants that would most benefit from recruitment efforts.
# Data: Provided by Teach for America. Includes 2021 applicant pool
# Author: A. Z. Andis Arietta
# Project initiated: 20220301
###

### Set up the environment ====
library(tidyverse)
library(randomForest)
library(GGally)
library(lubridate)
library(ggbiplot)
library(cluster)
library(Rtsne)
library(extrafont)
library(cowplot)
library(magrittr)
library(ggVennDiagram)


theme_set(theme_bw() + 
            theme(panel.border = element_blank(), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  axis.line = element_blank(),
                  axis.title.x = element_text(vjust = -0.7, hjust = .02, size = 11, family = "Bahnschrift"),
                  axis.text.x = element_text(vjust = 0, size = 8),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_text(vjust = 0, hjust = 0, size = 11, family = "Bahnschrift"),
                  plot.margin = unit(c(1,1,1,1), "cm"),
                  text = element_text(family = "Bahnschrift")))

### . Read in the data ====

RAW <- read.csv("./data/HiringExercise_Data.csv") %>%
  mutate_at(c("signupdate", "starteddate", "appdeadline", "submitteddate"), .funs = mdy) %>% # convert all calendar dates to date formats
  mutate(t_reg2start = starteddate - signupdate,
         t_start2deadline = appdeadline - starteddate,
         t_start2sub = submitteddate - starteddate,
         t_sub2deadline = appdeadline - submitteddate) %>% # Create duration variables for application timelines
  mutate_at(vars(starts_with("t_")), .fun = as.numeric) %>% # Convert duration to numeric format
  select(-ends_with("date")) %>%
  mutate_at(c("completedadm", "attendedevent", "met", "stem"), .funs = function(x){ifelse(x == 0, "N", "Y")}) %>%
  mutate_at(c("completedadm", "attendedevent", "met", "stem", "schoolsel", "major1group", "major2group", "minorgroup", "appdeadline", "personid"), .funs = as.factor) %>% # Convert ordinal and binary variables to factors
  mutate(schoolsel = fct_relevel(schoolsel, c("unknown", "least_selective", "less_selective", "selective", "more_selective", "most_selective")))
  

## Assumptions for initial data preparation:

## 1. I assume that application that were submitted after the deadline were not accepted. So, I removed 1955 applicants whose submission date was after the deadline. I would like to check with the admissions team to see if those applications were granted extensions, or if there is another reason these persisted in the dataset after cleaning.
RAW %>% 
  filter(t_sub2deadline > -10 & t_sub2deadline < 1) %>%
  ggplot(aes(x = t_sub2deadline)) +
  geom_density() +
  facet_wrap(vars(undergrad_uni)) # Here I am mainly double-checking to make sure that ALL of the foreign applications do not appear late by a day due to timezine issues.

## 2. I removed applicants with GPA values less than 1. There were 628 applicants in the dataset with GPA values reported as 0 or ~0.3. The bulk of the distribution in GPA values is between 2.5 and 4. I suspect that the GPA values ~0.3 are a decimal shift error, but would need more information to confirm. The 0 value GPAs are almost entirely from "Foreign University" applicants; so, I suspect that there is no GPA equivalent. After removing GPA and running an initial random forest analysis (see below) for variable selection, it appears that GPA is one of the most important predictors of admission completion. Thus, it is important to address the Foreign GPA issue. I considered two options: I could assign all Foreign University students the mean GPA value (essentially negating the leverage of that variable for those applications in subsequent analysis), or I could set those applicants aside to analysis separately at another time. I opted for the latter approach because I expect that there may be other idiosyncrasies of Foreign applicants that warrants treating them separately.
RAW %>% 
  ggplot(aes(x = gpa)) +
  geom_density() +
  facet_wrap(vars(undergrad_uni))

# First data prep
X <- RAW %>%
  filter(t_sub2deadline >= 0) %>% # Remove applications that were submitted after the deadline
  filter(gpa > 1) %>% # Remove applicants with GPA values < 1
  mutate_at(vars(starts_with("t_")), .funs = function(x){log(x+1)}) # Log-transform the duration variables

## 3. Many of the variables are highly correlated and form natural groupings. For example, essay lengths and the number of unique words are highy positively correlated (Pearson's correlation > 0.7). The submission timeline durations are also correlated intrinsically.
ggpairs(X %>% 
          group_by(completedadm, met) %>%
          sample_n(size = 357, replace = FALSE) %>%
          ungroup() %>%
          select(-where(is.character), -personid, -ends_with("date"), -undergrad_uni, appdeadline))

ggpairs(X %>% 
          group_by(completedadm, met) %>%
          sample_n(size = 357, replace = FALSE) %>%
          ungroup() %>%
          select(ends_with("length"), essayuniquewords))
ggsave("./figs/Fig_CorEssay.pdf", width = 8, height = 8, device = cairo_pdf)


# For the essay variables, I decided to reduce the 3 essay length and unique words variables to a single composite variable via principle components analysis. The single variable accounts for 88% of the variance in the multidimmensional distribution.
set.seed(09253)
PC_essay <- prcomp(X %>% select(starts_with("essay"), -essayssentiment), center = TRUE, scale = TRUE)
summary(PC_essay)
ggbiplot(PC_essay)

X <- X %>% 
  select(-ends_with("length"), -essayuniquewords) %>% # Drop the essay length and words variables
  mutate(PC1_essay = -PC_essay$x[,1]) # Add the composite essay variable. Making it negative so that positive values corresond to longer essays with more words.

# For the application timeline variables, I used a random forest to determine which variables are most important in predicting completion outcomes. NOTE, I am subsampling the dataset to enforce equal representation of the outcomes (completion and withdraw).
plot_grid(
  ggplot(X, aes(x = t_reg2start)) + geom_density(),
  ggplot(X, aes(x = t_start2deadline)) + geom_density(),
  ggplot(X, aes(x = t_start2sub)) + geom_density(),
  ggplot(X, aes(x = t_sub2deadline)) + geom_density())

X %>% filter(t_reg2start == 0) %>% tally() %$% ./36504 # 93% of applicants register the day that they start the application.

VennY <- ggVennDiagram(list(complete_y = X %>% filter(completedadm == "Y") %>% .[,1] %>% as.numeric(),
            start2sub = X %>% filter(t_start2sub == 0) %>% .[,1] %>% as.numeric(),
            sub2deadline = X %>% filter(t_sub2deadline == 0) %>% .[,1] %>% as.numeric(),
            start2deadline = X %>% filter(t_start2deadline == 0) %>% .[,1] %>% as.numeric()),
            label_alpha = 0) +
  scale_fill_gradient(low = "white", high = "firebrick") +
  scale_color_manual(values = rep("grey", 4)) +
  theme(legend.position = "none") +
  labs(title = "Completed = Y")

VennN <- ggVennDiagram(list(complete_N = X %>% filter(completedadm == "N") %>% .[,1] %>% as.numeric(),
            start2sub = X %>% filter(t_start2sub == 0) %>% .[,1] %>% as.numeric(),
            sub2deadline = X %>% filter(t_sub2deadline == 0) %>% .[,1] %>% as.numeric(),
            start2deadline = X %>% filter(t_start2deadline == 0) %>% .[,1] %>% as.numeric()),
            label_alpha = 0) +
  scale_fill_gradient(low = "white", high = "firebrick") +
  scale_color_manual(values = rep("grey", 4)) +
  theme(legend.position = "none") +
  labs(title = "Completed = N")

plot_grid(VennY, VennN) # This graph looks at the number of applications that had 0 days between application timeline points (start, submission, and deadline). Of those that complete the admissions process, most start well before the deadline, take at least a day to work on the application before submission, and submit before the deadline. However, most of those that withdraw submit on the deadline date or finish the day that they begin the application. 
ggsave("./figs/Fig_Venn.pdf", width = 12, height = 6, device = cairo_pdf)

### Variable selection ====

### . Random forest ====
RFdat <- X %>% 
  group_by(completedadm) %>%
  sample_n(size = X %>% 
                  group_by(completedadm) %>% 
                  tally() %$% min(.$n),
           replace = FALSE) %>% # Downsample to the size of the minor category
  select(-undergrad_uni, -personid, -t_reg2start) %>%
  mutate_if(is.numeric, .funs = function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)) %>% # Standardize and center the numeric variables
  mutate(noise1 = rnorm(nrow(.), mean = 0, sd = 1),
         noise2 = rnorm(nrow(.), mean = 0, sd = 1),
         noiseb1 = rbinom(nrow(.), size = 1, prob = .5),
         noiseb2 = rbinom(nrow(.), size = 1, prob = .5)) # Create random noise variables to assess included varaibale importance

# Run the random forest. Run the entire code chunk to clock the model.
T0 <- Sys.time()
set.seed(864654)
RFmod1 <- randomForest(completedadm ~ ., data = RFdat, importance = TRUE, proximity = TRUE, ntree = 300)
T1 <- Sys.time()
T1 - T0

# Save a reload the model to save time in the future.
saveRDS(RFmod1, file = "./processed/RFmod1.rds")
RFmod1 <- readRDS("./processed/RFmod1.rds")

# Interpret the random forest model
# NOTE: I would generally run this model a few times with different downsample sets. I would also generally spend some time tuning the model.
RFmod1 # Our model is fairly accurate at classifying if applicants completed admissions. The model is slightly more likely to classify completed applications as withdrawn with 11% error rate. So, it is falsely optimistic with respect to admission outcomes, but overall error rates are only 9%.
plot(RFmod1)
importance(RFmod1)
varImpPlot(RFmod1) # The duration between application start and submission and how many days the application was submitted before the deadline are important predictors. GPA, essay values, and how far in advance of the deadline applications were started are also important variables.

ggpairs(X %>% select(starts_with("t_")) %>% sample_frac(0.2)) # Unfortunately, all of the application timeline variables are highly correlated (r > .3). Only the time between start to submission and between submission and the deadline are independent intrinsically, so I will drop "t_start2deadline" for further analysis.

# Thus, my candidate variables are t_start2sub, t_sub2deadline, gpa, PC1_essay, appdeadline, attendedevent, major1group, schoolsel

### Generalized linear model ====

# Ordinarily, I would use a grouped generalized linear LASSO or elastic net procedur here in order to retain the "Met Y:N" groups to assess the interaction, but still benefit from relying on cross-validation for independent variable selection. Given, the 2-hour time window for this exercise, instead I will use a more basic procedure of fitting a maximal GLM that included the 2-way interaction between "Met" and all other candidate variables and backwards model selection based on the significance of the terms.
# The, from the stripped down model, I try out 3-way interactions with the two two variables from the Random forest (t_start2sub and gpa). 

# Fit the maximal 2-way interaction model
glm(completedadm ~ gpa*met + t_start2sub*met + t_sub2deadline*met + PC1_essay*met + appdeadline*met + met*attendedevent + major1group*met + schoolsel*met, data = X %>% filter(schoolsel != "unknown") %>% mutate(completedadm = as.numeric(completedadm)-1), family = binomial) %>% summary()

# Backwards select variables for the stripped down model
GLMmod <- glm(completedadm ~ gpa + t_start2sub + t_sub2deadline + appdeadline + major1group + schoolsel + met*attendedevent + PC1_essay*met, data = X %>% filter(schoolsel != "unknown") %>% mutate(completedadm = as.numeric(completedadm)-1), family = binomial)
summary(GLMmod)

# Try 3-way interactions
glm(completedadm ~ gpa + t_start2sub + t_sub2deadline + appdeadline + major1group + schoolsel + t_start2sub*met*attendedevent + PC1_essay*met , data = X %>% filter(schoolsel != "unknown"), family = binomial) %>% summary()
glm(completedadm ~ gpa + t_start2sub + t_sub2deadline + appdeadline + major1group + schoolsel + met*attendedevent + t_start2sub*PC1_essay*met , data = X %>% filter(schoolsel != "unknown"), family = binomial) %>% summary()
glm(completedadm ~ gpa + t_start2sub + t_sub2deadline + appdeadline + major1group + schoolsel + gpa*met*attendedevent + PC1_essay*met , data = X %>% filter(schoolsel != "unknown"), family = binomial) %>% summary()
glm(completedadm ~ gpa + t_start2sub + t_sub2deadline + appdeadline + major1group + schoolsel + met*attendedevent + gpa*PC1_essay*met , data = X %>% filter(schoolsel != "unknown"), family = binomial) %>% summary()
glm(completedadm ~ gpa + t_start2sub + t_sub2deadline + appdeadline + major1group + schoolsel + PC1_essay*met*attendedevent + PC1_essay*met , data = X %>% filter(schoolsel != "unknown"), family = binomial) %>% summary()
# None of the 3-way interactions are significant

### . Model predictions ====
# Here, I create prediction frames to get a sense of how much meeting with a recruiter increases the probability of completion.
# NOTE: With more time, I would also estimate confidence intervals for these estimates.
NEWDAT <- X %>% filter(schoolsel != "unknown") %>% mutate(completedadm = as.numeric(completedadm)-1)

ND1 <- expand.grid(schoolsel = unique(NEWDAT$schoolsel),
            major1group = unique(NEWDAT$major1group),
            attendedevent = unique(NEWDAT$attendedevent),
            met = unique(NEWDAT$met),
            appdeadline = unique(NEWDAT$appdeadline)) %>%
  bind_cols(NEWDAT %>%
              summarise_if(is.numeric, mean))

ND1 %>% mutate(fit = predict(GLMmod, newdata = ND1 %>% mutate(PC1_essay = 2.4), type = "response")) %>%
  group_by(met) %>%
  dplyr::summarise(fit = (mean(fit))) # For an application with a high essay score, meeting with a recruiter increases the probability from 0.825 to 0.856 

ND1 %>% mutate(fit = predict(GLMmod, newdata = ND1, type = "response")) %>%
  group_by(met, attendedevent) %>%
  filter(attendedevent == "Y") %>%
  dplyr::summarise(fit = (mean(fit))) # For applicants that attend events, meeting with a recruiter increases the probability from 0.873 to 0.887

### . Visualize the relationships ====
# NOTE: Given more time, I would have written these plots as a function of of the variables to save many lines of repeated code.

X %>%
  ggplot(aes(x = PC1_essay, y = as.numeric(completedadm) - 1, col = met, fill = met)) +
  stat_smooth(method = 'glm', se = T, method.args = list(family=binomial)) +
  scale_fill_manual(values = c("#ff3300", "#63d400")) +
  scale_color_manual(values = c("#ff3300", "#63d400"))  +
  coord_cartesian(ylim = c(0,1))  +
  labs(y = "Proportion completed admission", x = "Essay length and unique words used")
ggsave("./figs/Fig_GLMessay.pdf", width = 6, height = 6, device = cairo_pdf)

X %>%
  ggplot(aes(x = exp(t_start2sub), y = as.numeric(completedadm) - 1, col = met, fill = met)) +
  stat_smooth(method = 'glm', se = T, method.args = list(family=binomial)) +
  scale_fill_manual(values = c("#ff3300", "#63d400")) +
  scale_color_manual(values = c("#ff3300", "#63d400"))  +
  coord_cartesian(ylim = c(0,1)) +
  labs(y = "Proportion completed admission", x = "Days between application start and submission")
ggsave("./figs/Fig_Start2Sub.pdf", width = 6, height = 6, device = cairo_pdf)

X %>%
  ggplot(aes(x = exp(t_sub2deadline), y = as.numeric(completedadm) - 1, col = met, fill = met)) +
  stat_smooth(method = 'glm', se = T, method.args = list(family=binomial)) +
  scale_fill_manual(values = c("#ff3300", "#63d400")) +
  scale_color_manual(values = c("#ff3300", "#63d400")) +
  coord_cartesian(ylim = c(0,1)) +
  labs(y = "Proportion completed admission", x = "Days application submitted before deadline")
ggsave("./figs/Fig_GLMsub2deadline.pdf", width = 6, height = 6, device = cairo_pdf)

X %>%
  ggplot(aes(x = gpa, y = as.numeric(completedadm) - 1, col = met, fill = met)) +
  stat_smooth(method = 'glm', se = T, method.args = list(family=binomial)) +
  scale_fill_manual(values = c("#ff3300", "#63d400")) +
  scale_color_manual(values = c("#ff3300", "#63d400"))  +
  coord_cartesian(ylim = c(0,1)) +
  labs(y = "Proportion completed admission", x = "GPA")
ggsave("./figs/Fig_GLMgpa.pdf", width = 6, height = 6, device = cairo_pdf)

# Categorical interactions

p_tab2 <- function(DAT, VAR1, VAR2){
  DAT %>% 
    group_by_(VAR1, VAR2) %>% 
    tally() %>% 
    dplyr::group_by_(VAR2) %>% 
    dplyr::mutate(p = n/sum(n)*100) %>% 
    select(-n) %>% 
    spread_(VAR1, "p")
}

p_tab3 <- function(DAT, VAR1, VAR2, VAR3){
  DAT %>% 
    group_by_(VAR1, VAR2, VAR3) %>% 
    tally() %>% 
    dplyr::group_by_(VAR2, VAR3) %>% 
    dplyr::mutate(p = n/sum(n)*100) %>% 
    select(-n) %>% 
    spread_(VAR1, "p")
}

p_tab2(X, "completedadm", "met") %>% # The marginal proportion of those who completed admissions did not vary whether they individual met with a recruiter or not.
  ggplot(aes(y = Y, x = met)) +
  geom_line(aes(group = NA), col = "grey40", size = 2) +
  geom_point(aes(col = met), size = 10) +
  geom_text(aes(label = round(Y, 1)), vjust = 0.5, col = "white") +
  labs(y = "Proportion completed admission", x = "Applicant met with recruiter") +
  coord_cartesian(ylim = c(0,100)) +
  scale_color_manual(values = c("#ff3300", "#63d400"))

p_tab3(X, "completedadm", "met", "attendedevent") %>% # The marginal proportion of completion was greater for those that attended an event.
  ggplot(aes(y = Y, x = met, col = met)) +
  geom_line(aes(group = attendedevent), size = 2, col = "grey40") +
  geom_point(size = 10) +
  geom_text(aes(label = round(Y, 1)), vjust = 0.5, col = "white") +
  labs(y = "Proportion completed admission", x = "Applicant met with recruiter") +
  coord_cartesian(ylim = c(0,100)) +
  scale_color_manual(values = c("#ff3300", "#63d400")) +
  facet_wrap(vars(attendedevent))
ggsave("./figs/Fig_attendedevent.pdf", width = 8, height = 6, device = cairo_pdf)

p_tab3(X, "completedadm", "met", "major1group") %>% # The marginal proportion of completion was greater for those that attended an event.
  ggplot(aes(y = Y, x = met, col = met)) +
  geom_line(aes(group = major1group), size = 2, col = "grey40") +
  geom_point(size = 10) +
  geom_text(aes(label = round(Y, 1)), vjust = 0.5, col = "white") +
  labs(y = "Proportion completed admission", x = "Applicant met with recruiter") +
  coord_cartesian(ylim = c(0,100)) +
  scale_color_manual(values = c("#ff3300", "#63d400")) +
  facet_wrap(vars(major1group), nrow = 3)
ggsave("./figs/Fig_majorgroup.pdf", width = 12, height = 6, device = cairo_pdf)

p_tab3(X, "completedadm", "met", "schoolsel") %>% # The marginal proportion of completion was greater for those that attended an event.
  ggplot(aes(y = Y, x = met, col = met)) +
  geom_line(aes(group = schoolsel), size = 2, col = "grey40") +
  geom_point(size = 10) +
  geom_text(aes(label = round(Y, 1)), vjust = 0.5, col = "white") +
  labs(y = "Proportion completed admission", x = "Applicant met with recruiter") +
  coord_cartesian(ylim = c(0,100)) +
  scale_color_manual(values = c("#ff3300", "#63d400")) +
  facet_wrap(vars(schoolsel))
ggsave("./figs/Fig_schoolsel.pdf", width = 8, height = 6, device = cairo_pdf)

p_tab3(X, "completedadm", "met", "stem") %>% # The marginal proportion of completion was greater for those that attended an event.
  ggplot(aes(y = Y, x = met, col = met)) +
  geom_line(aes(group = stem), size = 2, col = "grey40") +
  geom_point(size = 10) +
  geom_text(aes(label = round(Y, 1)), vjust = 0.5, col = "white") +
  labs(y = "Proportion completed admission", x = "Applicant met with recruiter") +
  coord_cartesian(ylim = c(0,100)) +
  scale_color_manual(values = c("#ff3300", "#63d400")) +
  facet_wrap(vars(stem))
ggsave("./figs/Fig_stem.pdf", width = 8, height = 6, device = cairo_pdf)

p_tab3(X, "completedadm", "met", "appdeadline") %>% # The marginal proportion of completion was greater for those that attended an event.
  ggplot(aes(y = Y, x = met, col = met)) +
  geom_line(aes(group = appdeadline), size = 2, col = "grey40") +
  geom_point(size = 10) +
  geom_text(aes(label = round(Y, 1)), vjust = 0.5, col = "white") +
  labs(y = "Proportion completed admission", x = "Applicant met with recruiter") +
  coord_cartesian(ylim = c(0,100)) +
  scale_color_manual(values = c("#ff3300", "#63d400")) +
  facet_wrap(vars(appdeadline))
ggsave("./figs/Fig_appdeadline.pdf", width = 8, height = 6, device = cairo_pdf)

### Clustering ====
# One major assumption in my analysis is that the pool of applicants that met with recruiters were a random sample of all applicants. This could be incorrect if recruiters were targetting specific groups or if specific groups are more likely to seek out recruiters.
# To get a sense of this, I performed a multivariate clustering analysis. Essentially, I want to know if there are natural 'group' of applicants, and if any group seems to have disproportionally met with recruiters.

Clustdat <- X %>% 
  group_by(completedadm, met) %>%
  sample_n(size = 357, replace = FALSE) %>%
  ungroup() %>% # Downsampling to ensure I have even class representation for completion and those who met with recruiters
  select(-undergrad_uni, -t_reg2start) %>%
  mutate_if(is.numeric, .funs = function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm))

set.seed(356)
dist <- daisy(Clustdat %>% select(-completedadm, -met, -personid),
              metric = "gower") # Here I am using Gower distance because the input variables are both categorical and continuous

summary(dist)
dist_mat <- as.matrix(dist)

sil_width <- data.frame("Cluster" = 1:10, "Sil_Width" = rep(NA, 10))
for(i in 2:10){
  pam_fit <- pam(dist,
                 diss = TRUE,
                 k = i)
  sil_width[i,2] <- pam_fit$silinfo$avg.width
}
sil_width %>%
  ggplot(aes(x = Cluster, y = Sil_Width)) +
  geom_line() +
  labs(x = "Number of clusters", y = "Silhouette Width") # It seems that there are 3 natural clusters in the data

pam_fit <- pam(dist, diss = TRUE, k = 3)
pam_results <- Clustdat %>%
  dplyr::select(-completedadm, -met, -personid) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

View(Clustdat[pam_fit$medoids, ]) # Most of the difference between groups relies on time to submission, essay scores, and to a lesser extent school selectivity

# Visualize the clusters with t-SNE plots
tsne_obj <- Rtsne(dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         met = Clustdat$met,
         completed = Clustdat$completedadm)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(col = cluster,
                 size = completed), alpha = 0.7, pch = 16) +
  theme_bw() +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Dark2")) # The clusters are fairly heterogenous overall, suggesting that there is not much natural associations between groups of applicants overall
ggsave("./figs/Fig_tSNEclusters.pdf", width = 8, height = 6, device = cairo_pdf)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(col = met,
                 size = completed), alpha = 0.7, pch = 16) +
  theme_bw() +
  scale_color_manual(values = c("#ff3300", "#63d400")) # It does not look like recruitment differs substantially between clusters
ggsave("./figs/Fig_tSNEmet.pdf", width = 8, height = 6, device = cairo_pdf)


tsne_data %>% group_by(cluster, met) %>% 
  tally() %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(p = n/sum(n)*100) %>%
  filter(met == "Y") %>%
  ggplot(aes(x = cluster, y = p, label = round(p,1))) +
    geom_bar(stat = "identity") +
    geom_text(col = "white", vjust = 2) +
    labs(y = "Percentage of applicants met with recruiter", x = "Cluster assignment") +
    coord_cartesian(ylim = c(0, 100))
ggsave("./figs/Fig_tSNEbar.pdf", width = 6, height = 6, device = cairo_pdf)

tsne_data %>% group_by(cluster) %>% 
  tally() # Cluster 1 is biased towards heavier recruitment effort, but it is the smallest group. Individuals in this cluster were more likely to attend an event, wrote longer essays, and started their applications much earlier compared to the other groups. Their GPA and essay sentiment falls between those of the other groups.
