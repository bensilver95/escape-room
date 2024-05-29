# analyses and graphs for escape room

library(tidyverse)
library(lmerTest)
library(brms)
library(bmlm)
library(ggpubr)
library(corrplot)

setwd("/Users/benjaminsilver/Google Drive/My Drive/Grad School/Projects/Escape_Room/manuscript/submissions/")

######### CLEAN #############
pre <- read_csv('sharing_github/pre_clean.csv')
post <- read_csv('sharing_github/post_clean.csv')
week <- read_csv('sharing_github/week_clean.csv')

change <- pre %>% 
  rename_at(vars(-c(ID,Age,Gender,Race,Group,Teammate,
                    contains("Pred"))),
            ~ paste0(.,"_pre")) %>% 
  left_join(post %>% 
              rename_at(vars(-c(ID,Group,Teammate,
                                contains("Pred"))),
                        ~ paste0(.,"_post")), 
            by = c("ID","Group","Teammate")) %>% 
  left_join(week %>% 
              rename_at(vars(-c(ID,Group,Teammate,
                                contains("Pred"))),
                        ~ paste0(.,"_week")), 
            by = c("ID","Group","Teammate")) %>% 
  pivot_longer(cols = c(starts_with("Morality"),
                        starts_with("Ability"),
                        starts_with("Sociability")),
               names_to = c("Dimension",".value"),
               names_sep = "_") %>% 
  mutate(postpreDiff.Real = post - pre,
         postpreDiff.Abs = abs(postpreDiff.Real),
         postpreDiff.d = ifelse(postpreDiff.Real <= 0, 0, 1),
         weekpostDiff.Real = week - post,
         weekpostDiff.Abs = abs(weekpostDiff.Real),
         weekpostDiff.d = ifelse(weekpostDiff.Real <= 0, 0, 1),
         Interact_pre.r = 7 - Interact_pre,
         Familiar_pre = (Interact_pre.r+FirstMeet_pre)/2,
         Familiar_pre.c = scale(Familiar_pre, center = T, 
                                scale = F),
         Like_pre.c = scale(Like_pre, center = T, scale = F),
         Closeness_pre.c = scale(Closeness_pre, center = T, scale = F),
         Similar_pre.c = scale(Similar_pre, center = T, scale = F),
         Like_post.c = scale(Like_post, center = T, scale = F),
         Closeness_post.c = scale(Closeness_post, center = T, scale = F),
         Similar_post.c = scale(Similar_post, center = T, scale = F),
         Like_pre.cs = scale(Like_pre,center = T, scale = T),
         Familiar_pre.cs = scale(Familiar_pre, center = T, scale = T),
         Closeness_pre.cs = scale(Closeness_pre,center = T, scale = T),
         Similar_pre.cs = scale(Similar_pre,center = T, scale = T),
         Like_post.cs = scale(Like_post,center = T, scale = T),
         Closeness_post.cs = scale(Closeness_post,center = T, scale = T)) %>% 
  select(-c(contains("Mastermind"),contains("ColoredLetters"),
            contains("Cables"),contains("FlyerCipher"),
            contains("Myspace"),contains("Maze")))

change1 <- change %>% 
  pivot_longer(cols = contains("Diff."),
               names_to = c("Comparison",".value"),
               names_sep = "Diff.") %>% 
  mutate(Comparison.d = case_when(
    Comparison == "postpre" ~ 0,
    Comparison == "weekpost" ~ 1
  ),
  Comparison.drev = case_when(
    Comparison == "postpre" ~ 1,
    Comparison == "weekpost" ~ 0
  ))

change2 <- change %>% 
  pivot_longer(cols = c("pre","post","week"),
               names_to = "Time",
               values_to = "Rating")


# get objective competence performance scores
video <- read_csv("sharing_github/video_data.csv")

video_sum <- video %>% 
  mutate(Puzzle = recode(Puzzle, EMPID = "flyer",
                         crumpet = "myspace")) %>% 
  group_by(Group) %>% 
  count(Subject) %>% 
  filter(Subject != 'None') %>% 
  rename(ParticipationObjective = "n") %>%
  rename(Teammate = "Subject")

# get objective sociability performance scores
transcript <- read_csv('sharing_github/transcript_info.csv')
transcript_sum <- transcript %>% 
  rename(Group = "group",Teammate = "participant") %>% 
  mutate(
    firstplural_pct = (firstplural/num_words)*100) %>% 
  filter(Teammate != "GM",Teammate != "GM2") %>% 
  group_by(Group) %>% 
  mutate(collab_ratio = ((firstplural + second)/sum((num_words)))*100) %>% 
  ungroup() %>% 
  mutate(collab_ratio2 = (firstplural + second)/first,
         collab_ratio2 = ifelse(is.infinite(collab_ratio2),NA,collab_ratio2))

change <- change %>% 
  left_join(video_sum) %>% 
  left_join(transcript_sum) %>% 
  mutate(ParticipationObjective = replace_na(ParticipationObjective,0),
         ParticipationObjective.cs = scale(ParticipationObjective, center = T, scale = T),
         collab_ratio.cs = scale(collab_ratio, center = T, scale = T),
         num_utterances.cs = scale(num_utterances, center = T, scale = T),
         collab_ratio2.cs = scale(collab_ratio2, center = T, scale = T))

###### demographics and descriptives ######
### demographics ######
# gender
change %>% 
  distinct(ID, .keep_all = T) %>% 
  count(Gender)

# age
change %>% 
  distinct(ID, .keep_all = T) %>% 
  summarize(mean(Age))

change %>% 
  distinct(ID, .keep_all = T) %>% 
  summarize(min(Age))

change %>% 
  distinct(ID, .keep_all = T) %>% 
  summarize(max(Age))

change %>% 
  distinct(ID, .keep_all = T) %>% 
  count(Age < 23)

# race
change %>% 
  distinct(ID, .keep_all = T) %>% 
  count(Race)

### descriptives #####
# means and sds
change_descriptives <- change %>% 
  distinct(ID, Teammate, .keep_all = T) %>% 
  summarize(Like_pre_sd = sd(Like_pre,na.rm = T),
            Interact_pre.r_sd = sd(Interact_pre, na.rm = T),
            FirstMeet_pre_sd = sd(FirstMeet_pre, na.rm = T),
            Similar_pre_sd = sd(Similar_pre,na.rm = T),
            Like_pre = mean(Like_pre, na.rm = T),
            Interact_pre.r = mean(Interact_pre, na.rm = T),
            FirstMeet_pre = mean(FirstMeet_pre, na.rm = T),
            Similar_pre = mean(Similar_pre, na.rm = T))

# correlation matrix
change_corrmat <- change %>% 
  distinct(ID, Teammate, .keep_all = T) %>% 
  select(Like_pre,
         Similar_pre,
         Familiar_pre)
cor(change_corrmat, use="complete.obs")

###### QUESTION 1 ######
## Models ####
mChangeComp1 <- brm(Rating ~ Time.d + 
                      (1 + Time.d | Group / ID),
                    data = change2 %>% 
                      filter(Dimension == 'Ability',
                             Time != 'week') %>% 
                      mutate(Time.d = if_else(Time == 'pre',0,1)),
                    cores = 4, chains = 2, iter = 6000)

mChangeComp2 <- brm(Rating ~ Time.d + 
                      (1 + Time.d | ID),
                    data = change2 %>% 
                      filter(Dimension == 'Ability',
                             Time != 'post') %>% 
                      mutate(Time.d = if_else(Time == 'pre',0,1)),
                    cores = 4, chains = 2, iter = 6000)

mChangeSoc1 <- brm(Rating ~ Time.d + 
                     (1 + Time.d | Group / ID),
                   data = change2 %>% 
                     filter(Dimension == 'Sociability') %>% 
                     mutate(Time.d = if_else(Time == 'pre',0,1)),
                   cores = 4, chains = 2, iter = 6000)

#5/8: Need to run
mChangeSoc2 <- brm(Rating ~ Time.d + 
                      (1 + Time.d | ID),
                    data = change2 %>% 
                      filter(Dimension == 'Sociability',
                             Time != 'post') %>% 
                      mutate(Time.d = if_else(Time == 'pre',0,1)),
                    cores = 4, chains = 2, iter = 6000)

## Figures ####

f2a <- ggplot(change2 %>% 
         filter(Dimension == "Ability") %>% 
         mutate(Time = factor(Time,
                              levels = c("pre","post","week"))),
       aes(x = Time, y = Rating)) +
  geom_line(alpha = .05,aes(group = interaction(ID,Teammate))) +
  geom_violin(fill = "orchid3") +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = .6) +
  theme_classic() +
  scale_x_discrete(labels = c("1-7 days prior\nto escape room",
                              "Immediately after\nescape room",
                              '1 week after\nescape room')) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  labs(y = "Competence Rating",
       title = "Change in perceived competence") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = .5),
        axis.title.x = element_blank())

f2b <- ggplot(change2 %>% 
         filter(Dimension == "Sociability") %>% 
         mutate(Time = factor(Time,
                              levels = c("pre","post","week"))),
       aes(x = Time, y = Rating)) +
  geom_line(alpha = .05,aes(group = interaction(ID,Teammate))) +
  geom_violin(fill = "darkgoldenrod2") +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = .6) +
  theme_classic() +
  scale_x_discrete(labels = c("1-7 days prior\nto escape room",
                              "Immediately after\nescape room",
                              '1 week after\nescape room')) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  labs(y = "Sociability Rating",
       title = "Change in perceived sociability") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = .5),
        axis.title.x = element_blank())

ggarrange(f2a,f2b,
          labels = c("A","B"),
          ncol = 2, nrow = 1,
          font.label = list(size = 20))
ggsave("figs/Fig2.jpg", scale = 1.8, width = 6, height = 3)


##### QUESTION 1 REDO #######
## Models ####

mDirChangeComp1 <- brm(Rating ~ Time.d + 
                      (1 + Time.d | Group / ID),
                    data = change2 %>% 
                      filter(Dimension == 'Ability',
                             Time != 'week') %>% 
                      mutate(Time.d = if_else(Time == 'pre',0,1)),
                    cores = 4, chains = 2, iter = 6000)

mDirChangeComp2 <- brm(Rating ~ Time.d + 
                      (1 + Time.d | ID),
                    data = change2 %>% 
                      filter(Dimension == 'Ability',
                             Time != 'post') %>% 
                      mutate(Time.d = if_else(Time == 'pre',0,1)),
                    cores = 4, chains = 2, iter = 6000)

mDirChangeSoc1 <- brm(Rating ~ Time.d + 
                     (1 + Time.d | Group / ID),
                   data = change2 %>% 
                     filter(Dimension == 'Sociability') %>% 
                     mutate(Time.d = if_else(Time == 'pre',0,1)),
                   cores = 4, chains = 2, iter = 6000)

mDirChangeSoc2 <- brm(Rating ~ Time.d + 
                     (1 + Time.d | ID),
                   data = change2 %>% 
                     filter(Dimension == 'Sociability',
                            Time != 'post') %>% 
                     mutate(Time.d = if_else(Time == 'pre',0,1)),
                   cores = 4, chains = 2, iter = 6000)

mChangeComp1 <- brm(Abs ~ Comparison.d +
                      (1 + Comparison.d | Group / ID), 
                    data = change1 %>%
                      filter(Dimension == 'Ability'),
                    cores = 4, chains = 2, iter = 6000)

mChangeComp2 <- brm(Abs ~ Comparison.drev +
                      (1 + Comparison.drev | Group / ID), 
                    data = change1 %>%
                      filter(Dimension == 'Ability'),
                    cores = 4, chains = 2, iter = 6000)

mChangeSoc1 <- brm(Abs ~ Comparison.d +
                      (1 + Comparison.d | Group / ID), 
                    data = change1 %>%
                      filter(Dimension == 'Sociability'),
                    cores = 4, chains = 2, iter = 6000)

mChangeSoc2 <- brm(Abs ~ Comparison.drev +
                      (1 + Comparison.drev | Group / ID), 
                    data = change1 %>%
                      filter(Dimension == 'Sociability'),
                    cores = 4, chains = 2, iter = 6000)



## Figures ####

lines <- tibble(Comparison = c("postpre","postpre","weekpost","weekpost"),
                Dimension = c("Ability","Sociability","Ability","Sociability"),
                mean = c(fixef(mChangeComp1)[1,1],fixef(mChangeSoc1)[1,1],
                         fixef(mChangeComp2)[1,1],fixef(mChangeSoc2)[1,1]),
                low = c(fixef(mChangeComp1)[1,3],fixef(mChangeSoc1)[1,3],
                        fixef(mChangeComp2)[1,3],fixef(mChangeSoc2)[1,3]),
                high = c(fixef(mChangeComp1)[1,4],fixef(mChangeSoc1)[1,4],
                         fixef(mChangeComp2)[1,4],fixef(mChangeSoc2)[1,4]))

change1 <- change1 %>% 
  left_join(lines)

f2a <- ggplot(change2 %>% 
                filter(Dimension == "Ability") %>% 
                mutate(Time = factor(Time,
                                     levels = c("pre","post","week"))),
              aes(x = Time, y = Rating)) +
  geom_line(alpha = .05,aes(group = interaction(ID,Teammate))) +
  geom_violin(fill = "orchid3") +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = .6) +
  theme_classic() +
  scale_x_discrete(labels = c("1-7 days prior\nto escape room",
                              "Immediately after\nescape room",
                              '1 week after\nescape room')) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  labs(y = "Competence Rating",
       title = "Change in perceived competence") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = .5),
        axis.title.x = element_blank())

f2b <- ggplot(change1 %>%
         filter(Dimension == 'Ability'),
       aes(x = Abs, fill = as.factor(Comparison.d),
           color = as.factor(Comparison.d))) +
  geom_histogram() +
  facet_grid(rows = vars(Comparison)) +
  geom_vline(aes(xintercept = mean)) +
  geom_vline(aes(xintercept = low), linetype = "dashed") +
  geom_vline(aes(xintercept = high), linetype = "dashed") +
  theme_bw() +
  labs(x = 'Absolute change in competence ratings\nbetween timepoints',
       y = 'Count',
       title = 'Competence rating change\nafter escape room game',
       fill = 'Comparison') +
  scale_fill_manual(values = c("orchid1","orchid4"),
                    labels = c('Immediately after',"1 week after")) +
  scale_color_manual(values = c("orchid1","orchid4"),
                    labels = c('Immediately after',"1 week after"),
                    guide = "none") +
  theme(legend.position = "bottom",
        strip.text = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = .5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

f2c <- ggplot(change2 %>% 
                filter(Dimension == "Sociability") %>% 
                mutate(Time = factor(Time,
                                     levels = c("pre","post","week"))),
              aes(x = Time, y = Rating)) +
  geom_line(alpha = .05,aes(group = interaction(ID,Teammate))) +
  geom_violin(fill = "darkgoldenrod2") +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = .6) +
  theme_classic() +
  scale_x_discrete(labels = c("1-7 days prior\nto escape room",
                              "Immediately after\nescape room",
                              '1 week after\nescape room')) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  labs(y = "Sociability Rating",
       title = "Change in perceived sociability") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = .5),
        axis.title.x = element_blank())

f2d <- ggplot(change1 %>%
                filter(Dimension == 'Sociability'),
              aes(x = Abs, fill = as.factor(Comparison.d),
                  color = as.factor(Comparison.d))) +
  geom_histogram() +
  facet_grid(rows = vars(Comparison)) +
  geom_vline(aes(xintercept = mean)) +
  geom_vline(aes(xintercept = low), linetype = "dashed") +
  geom_vline(aes(xintercept = high), linetype = "dashed") +
  theme_bw() +
  labs(x = 'Absolute change in sociability ratings\nbetween timepoints',
       y = 'Count',
       title = 'Sociability rating change\nafter escape room game',
       fill = 'Comparison') +
  scale_fill_manual(values = c("darkgoldenrod1","darkgoldenrod4"),
                    labels = c('Immediately after',"1 week after")) +
  scale_color_manual(values = c("darkgoldenrod1","darkgoldenrod4"),
                     labels = c('Immediately after',"1 week after"),
                     guide = "none") +
  theme(legend.position = "bottom",
        strip.text = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = .5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ggarrange(f2a,f2b,f2c,f2d,
          labels = c("A","B","C","D"),
          ncol = 2, nrow = 2,
          font.label = list(size = 20))
ggsave("figs/Fig2.jpg", scale = 1.8, width = 6, height = 6)


###### QUESTION 2 ######
## Models ######
mLikeFamiliarSimComp <- brm(post ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs + pre +
                              (1 + Like_pre.cs*Familiar_pre.cs* 
                                 Similar_pre.cs + pre | Group / ID),
                            data = change %>% 
                              filter(Dimension == 'Ability'),
                            cores = 4, chains = 2, iter = 8000,
                            control = list(adapt_delta = .9))

mLikeFamiliarSimSoc <- brm(post ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs + pre +
                             (1 + Like_pre.cs*Familiar_pre.cs* 
                                Similar_pre.cs + pre | Group / ID),
                           data = change %>% 
                             filter(Dimension == 'Sociability'),
                           cores = 4, chains = 2, iter = 8000,
                           control = list(adapt_delta = .9))

# not used in paper
mLikeFamiliarSimCompWeek <- brm(week ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs + pre +
                              (1 + Like_pre.cs*Familiar_pre.cs*
                                 Similar_pre.cs + pre | Group / ID),
                            data = change %>% 
                              filter(Dimension == 'Ability'),
                            cores = 4, chains = 2, iter = 8000,
                            control = list(adapt_delta = .9))

# not used in paper
mLikeFamiliarSimSocWeek <- brm(week ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs + pre +
                             (1 + Like_pre.cs*Familiar_pre.cs*
                                Similar_pre.cs + pre | Group / ID),
                           data = change %>% 
                             filter(Dimension == 'Sociability'),
                           cores = 4, chains = 2, iter = 8000,
                           control = list(adapt_delta = .9))

## Figures ######
# competence
x = c("Liking","Familiarity","Similarity",
      "Liking x\nFamiliarity","Liking x\nSimilarity",
      "Familiarity x\nSimilarity")
modelPers = brms::fixef(mLikeFamiliarSimComp)

effect = c(modelPers[2,1],modelPers[3,1],modelPers[4,1],
           modelPers[6,1],modelPers[7,1],modelPers[8,1])

lower = c(modelPers[2,3],modelPers[3,3],modelPers[4,3],
          modelPers[6,3],modelPers[7,3],modelPers[8,3])

upper = c(modelPers[2,4],modelPers[3,4],modelPers[4,4],
          modelPers[6,4],modelPers[7,4],modelPers[8,4])

ci80Pers = brms::posterior_samples(mLikeFamiliarSimComp)

# 80% CIs
lower80 = c(quantile(ci80Pers[,2],.1),
            quantile(ci80Pers[,3],.1),
            quantile(ci80Pers[,4],.1),
            quantile(ci80Pers[,6],.1),
            quantile(ci80Pers[,7],.1),
            quantile(ci80Pers[,8],.1))

# 80% CIs
upper80 = c(quantile(ci80Pers[,2],.9),
            quantile(ci80Pers[,3],.9),
            quantile(ci80Pers[,4],.9),
            quantile(ci80Pers[,6],.9),
            quantile(ci80Pers[,7],.9),
            quantile(ci80Pers[,8],.9))

pdf <- data.frame(x = x, 
                  effect = effect, lower = lower,
                  upper = upper,
                  lower80 = lower80, upper80 = upper80)
pdf$x <- factor(pdf$x, levels = rev(x))

f3a<- ggplot(pdf, aes(x = effect, y = x)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, color = 'orchid3') +
  geom_errorbar(aes(xmin = lower80, xmax = upper80),
                width = 0, linewidth = 1.5, color = 'orchid3') +  
  geom_point(size = 3, color = 'orchid3') +
  theme_classic() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta",
       title = "Effects of relational factors on\ntrait-level competence perceptions") +
  scale_x_continuous(n.breaks = 8) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(hjust = .5, size = 18))

# sociability

x = c("Liking","Familiarity","Similarity",
      "Liking x\nFamiliarity","Liking x\nSimilarity",
      "Familiarity x\nSimilarity")
modelPers = brms::fixef(mLikeFamiliarSimSoc)

effect = c(modelPers[2,1],modelPers[3,1],modelPers[4,1],
           modelPers[6,1],modelPers[7,1],modelPers[8,1])

lower = c(modelPers[2,3],modelPers[3,3],modelPers[4,3],
          modelPers[6,3],modelPers[7,3],modelPers[8,3])

upper = c(modelPers[2,4],modelPers[3,4],modelPers[4,4],
          modelPers[6,4],modelPers[7,4],modelPers[8,4])

ci80Pers = brms::posterior_samples(mLikeFamiliarSimSoc)

# 80% CIs
lower80 = c(quantile(ci80Pers[,2],.1),
            quantile(ci80Pers[,3],.1),
            quantile(ci80Pers[,4],.1),
            quantile(ci80Pers[,6],.1),
            quantile(ci80Pers[,7],.1),
            quantile(ci80Pers[,8],.1))

# 80% CIs
upper80 = c(quantile(ci80Pers[,2],.9),
            quantile(ci80Pers[,3],.9),
            quantile(ci80Pers[,4],.9),
            quantile(ci80Pers[,6],.9),
            quantile(ci80Pers[,7],.9),
            quantile(ci80Pers[,8],.9))

pdf <- data.frame(x = x, 
                  effect = effect, lower = lower,
                  upper = upper,
                  lower80 = lower80, upper80 = upper80)
pdf$x <- factor(pdf$x, levels = rev(x))

f3b<-ggplot(pdf, aes(x = effect, y = x)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, color = 'darkgoldenrod2') +
  geom_errorbar(aes(xmin = lower80, xmax = upper80),
                width = 0, linewidth = 1.5, color = 'darkgoldenrod2') +  
  geom_point(size = 3, color = 'darkgoldenrod2') +
  theme_classic() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta",
       title = "Effects of relational factors on\ntrait-level sociability perceptions") +
  scale_x_continuous(n.breaks = 8) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(hjust = .5, size = 18))

ggarrange(f3a,f3b,
          labels = c("A","B"),
          ncol = 2, nrow = 1,
          font.label = list(size = 20))
ggsave("figs/Fig3.jpg", scale = 1.8, width = 6, height = 3)

###### QUESTION 3 ######
## Models #####
# objective performance #####

#5/29: still can't quite converge
mPerfComp <- brm(post ~ ParticipationObjective.cs + pre + 
                   (1  + pre | Group / ID),
                 data = change %>% 
                   filter(Dimension == 'Ability'),
                 cores = 4, chains = 2, iter = 6000,
                 control = list(adapt_delta = .91))

#5/14: still can't converge
mInterComp <- brm(post ~ Like_pre.cs*Similar_pre.cs*Familiar_pre.cs*ParticipationObjective.cs + pre +
                     (1 + Like_pre.cs*Similar_pre.cs*
                        Familiar_pre.cs*ParticipationObjective.cs + pre | Group / ID),
                   data = change %>% 
                     filter(Dimension == 'Ability'),
                   cores = 4, chains = 2, iter = 8000, 
                  control = list(adapt_delta = .91))

# 5/29: still can't quite converge but pretty good
mPerfSoc <- brm(post ~ collab_ratio.cs + pre + 
                  (1 + pre | Group / ID),
                data = change %>% 
                  filter(Dimension == 'Sociability'),
                cores = 4, chains = 2, iter = 6000,
                control = list(adapt_delta = .91))

#5/14: still can't converge
mInterSoc <- brm(post ~ Like_pre.cs*Similar_pre.cs*Familiar_pre.cs*collab_ratio.cs + pre +
                    (1 + Like_pre.cs + Similar_pre.cs + 
                       Familiar_pre.cs + collab_ratio.cs + pre | Group / ID),
                  data = change %>% 
                    filter(Dimension == 'Sociability'),
                  cores = 4, chains = 2, iter = 8000, 
                  control = list(adapt_delta = .9))

# PAB #####
change <- change %>% 
  mutate(ParticipationObjective.rs = scales::rescale(ParticipationObjective,to = c(0,10)),
         collab_ratio.rs = scales::rescale(collab_ratio, to = c(0,10)),
         num_utterances.rs = scales::rescale(collab_ratio, to = c(0,10))) %>% 
  mutate(TeamCollabDiff_post = TeamCollab_post - collab_ratio.rs,
         SolvingPuzzlesDiff_post = SolvingPuzzles_post - ParticipationObjective.rs)

mAccComp <- brm(SolvingPuzzlesDiff_post ~ Familiar_pre.cs*Like_pre.cs*Similar_pre.cs + 
                  (1 + Familiar_pre.cs + Like_pre.cs + Similar_pre.cs | Group / ID), 
                data = change %>% 
                  distinct(ID,Teammate,.keep_all = T),
                cores = 4, chains = 2, iter = 8000)

mAccSoc <- brm(TeamCollabDiff_post ~ Familiar_pre.cs*Like_pre.cs*Similar_pre.cs + 
                 (1 + Familiar_pre.cs + Like_pre.cs + Similar_pre.cs | Group / ID), 
               data = change %>% 
                 distinct(ID,Teammate,.keep_all = T),
               cores = 4, chains = 2, iter = 8000)

# mediation #####

#5/14:Some trouble converging
mMediationComp <- brm(post ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs*SolvingPuzzlesDiff_post + pre +
                        (1 + Like_pre.cs + Familiar_pre.cs + 
                           Similar_pre.cs + SolvingPuzzlesDiff_post + pre | Group / ID),
                      data = change %>% 
                        filter(Dimension == 'Ability'),
                      cores = 4, chains = 2, iter = 8000,
                      control = list(adapt_delta = .9))

#5/14:Some trouble converging
mMediationSoc <- brm(post ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs*TeamCollabDiff_post + pre +
                       (1 + Like_pre.cs + Familiar_pre.cs + 
                          Similar_pre.cs + TeamCollabDiff_post + pre | Group / ID),
                     data = change %>% 
                       filter(Dimension == 'Sociability'),
                     cores = 4, chains = 2, iter = 8000,
                     control = list(adapt_delta = .9))

# don't use these in the paper but helpful for understanding mediation
change_med <- change %>% 
  select(ID, Teammate,Familiar_pre.cs,Like_pre.cs,Similar_pre.cs,
         SolvingPuzzlesDiff_post,TeamCollabDiff_post,Dimension,pre,post)

# similarity, competence 
change_med_comp <- change_med %>% 
  filter(Dimension == 'Ability') %>% 
  isolate(by = "ID",
          value = c("Similar_pre.cs","SolvingPuzzlesDiff_post","post"))

fit_med_comp <- mlm(d = change_med_comp %>% 
                      filter(!is.na(Similar_pre.cs_cw),
                             !is.na(SolvingPuzzlesDiff_post_cw),
                             !is.na(post_cw)),
                    id = "ID", x = "Similar_pre.cs_cw",
                    m = "SolvingPuzzlesDiff_post_cw",y = "post_cw",
                    iter = 8000, cores = 4)

# liking, sociability 
change_med_soc <- change_med %>% 
  filter(Dimension == 'Sociability') %>% 
  isolate(by = "ID",
          value = c("Like_pre.cs","TeamCollabDiff_post","post"))

fit_med_soc <- mlm(d = change_med_soc %>% 
                     filter(!is.na(Like_pre.cs_cw),
                            !is.na(TeamCollabDiff_post_cw),
                            !is.na(post_cw)),
                   id = "ID", x = "Like_pre.cs_cw",
                   m = "TeamCollabDiff_post_cw",y = "post_cw",
                   iter = 8000, cores = 4)

## Figures #####
# Objective performance x RF Interactions #####
medcomp <- median(change$ParticipationObjective.cs,na.rm = T)
medsoc <- median(change$collab_ratio.cs, na.rm = T)

f4a <- ggplot(change %>% 
         filter(Dimension == "Ability",
                !is.na(ParticipationObjective.cs)) %>% 
         mutate(ParticipationObjective.csd = as.factor(if_else(ParticipationObjective.cs > medcomp,1,0))),
       aes(x = Similar_pre, y = post, 
           color = ParticipationObjective.csd, group = ParticipationObjective.csd)) +
  geom_jitter(alpha = .2) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("orchid4","orchid1"),
                     labels = c("Low","High")) +
  theme_classic() +
  labs(x = "Pre-game similarity rating", y = "Post-game competence rating", color = "Performance\nScore",
       title = "Puzzle solving performance x\nSimilarity") +
  theme(plot.title = element_text(size = 18, hjust = .5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

f4b <- ggplot(change %>% 
         filter(Dimension == "Sociability",
                !is.na(collab_ratio.cs)) %>% 
         mutate(collab_ratio.csd = as.factor(if_else(collab_ratio.cs > medsoc,1,0))),
       aes(x = Like_pre, y = post, 
           color = collab_ratio.csd, group = collab_ratio.csd)) +
  geom_jitter(alpha = .2) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("darkgoldenrod4","darkgoldenrod1"),
                     labels = c("Low","High")) +
  theme_classic() +
  labs(x = "Pre-game liking rating", y = "Post-game sociability rating", color = "Performance\nScore",
       title = "Team collaboration performance x\nliking") +
  theme(plot.title = element_text(size = 18, hjust = .5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ggarrange(f4a,f4b,
          labels = c("A","B"),
          ncol = 2, nrow = 1,
          font.label = list(size = 20))
ggsave("figs/paper/Fig4.jpg", scale = 1.8, width = 6, height = 3)

# Effects of RFs on PAB (not in paper) #######

x = c("Liking","Familiarity","Similarity")
modelPers = brms::fixef(mAccComp)

effect = c(modelPers[3,1],modelPers[2,1],modelPers[4,1])

lower = c(modelPers[3,3],modelPers[2,3],modelPers[4,3])

upper = c(modelPers[3,4],modelPers[2,4],modelPers[4,4])

ci80Pers = brms::posterior_samples(mAccComp)

# 80% CIs
lower80 = c(quantile(ci80Pers[,3],.1),
            quantile(ci80Pers[,2],.1),
            quantile(ci80Pers[,4],.1))

# 80% CIs
upper80 = c(quantile(ci80Pers[,3],.9),
            quantile(ci80Pers[,2],.9),
            quantile(ci80Pers[,4],.9))

pdf <- data.frame(x = x, 
                  effect = effect, lower = lower,
                  upper = upper,
                  lower80 = lower80, upper80 = upper80)
pdf$x <- factor(pdf$x, levels = rev(x))

f5a<- ggplot(pdf, aes(x = effect, y = x)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, color = 'orchid3') +
  geom_errorbar(aes(xmin = lower80, xmax = upper80),
                width = 0, size = 1.5, color = 'orchid3') +  
  geom_point(size = 3, color = 'orchid3') +
  theme_classic() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta",
       title = "Effects of relational factors on\nPAB - puzzle solving ability") +
  scale_x_continuous(n.breaks = 8) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(hjust = .5, size = 18))

# sociability

x = c("Liking","Familiarity","Similarity")
modelPers = brms::fixef(mAccSoc)

effect = c(modelPers[3,1],modelPers[2,1],modelPers[4,1])

lower = c(modelPers[3,3],modelPers[2,3],modelPers[4,3])

upper = c(modelPers[3,4],modelPers[2,4],modelPers[4,4])

ci80Pers = brms::posterior_samples(mAccSoc)

# 80% CIs
lower80 = c(quantile(ci80Pers[,3],.1),
            quantile(ci80Pers[,2],.1),
            quantile(ci80Pers[,4],.1))

# 80% CIs
upper80 = c(quantile(ci80Pers[,3],.9),
            quantile(ci80Pers[,2],.9),
            quantile(ci80Pers[,4],.9))

pdf <- data.frame(x = x, 
                  effect = effect, lower = lower,
                  upper = upper,
                  lower80 = lower80, upper80 = upper80)
pdf$x <- factor(pdf$x, levels = rev(x))

f5b<-ggplot(pdf, aes(x = effect, y = x)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, color = 'darkgoldenrod2') +
  geom_errorbar(aes(xmin = lower80, xmax = upper80),
                width = 0, size = 1.5, color = 'darkgoldenrod2') +  
  geom_point(size = 3, color = 'darkgoldenrod2') +
  theme_classic() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta",
       title = "Effects of relational factors on\nPAB - team collaboration ability") +
  scale_x_continuous(n.breaks = 8) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(hjust = .5, size = 18))

ggarrange(f5a,f5b,
          labels = c("A","B"),
          ncol = 2, nrow = 1,
          font.label = list(size = 20))
ggsave("figs/Fig5.jpg", scale = 1.8, width = 6, height = 3)
