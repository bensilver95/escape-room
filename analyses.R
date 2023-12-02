# analyses and graphs for escape room

library(tidyverse)
library(lmerTest)
library(brms)
library(bmlm)

setwd("/Users/bensilver/Google Drive/My Drive/Grad School/Projects/Escape_Room/data")

######### CLEAN #############
pre <- read_csv('surveys/pre_clean.csv')
post <- read_csv('surveys/post_clean.csv')

change <- pre %>% 
  rename_at(vars(-c(ID,Age,Gender,Race,Group,Teammate,
                    contains("Pred"))),
            ~ paste0(.,"_pre")) %>% 
  left_join(post %>% 
              rename_at(vars(-c(ID,Group,Teammate,
                                contains("Pred"))),
                        ~ paste0(.,"_post")), 
            by = c("ID","Group","Teammate")) %>% 
  pivot_longer(cols = c(starts_with("Morality"),
                        starts_with("Ability"),
                        starts_with("Sociability")),
               names_to = c("Dimension",".value"),
               names_sep = "_") %>% 
  mutate(postpreDiff.Real = post - pre,
         postpreDiff.Abs = abs(postpreDiff.Real),
         postpreDiff.d = ifelse(postpreDiff.Real <= 0, 0, 1),
         Interact_pre.r = 7 - Interact_pre,
         Familiar_pre = Interact_pre.r * FirstMeet_pre,
         Familiar_pre.c = scale(Familiar_pre, center = T, 
                                scale = F),
         Like_pre.c = scale(Like_pre, center = T, scale = F),
         Closeness_pre.c = scale(Closeness_pre, center = T, scale = F),
         Similar_pre.c = scale(Similar_pre, center = T, scale = F),
         Like_post.c = scale(Like_post, center = T, scale = F),
         Closeness_post.c = scale(Closeness_post, center = T, scale = F),
         Similar_post.c = scale(Similar_post, center = T, scale = F),
         Like_week.c = scale(Like_week, center = T, scale = F),
         Closeness_week.c = scale(Closeness_week, center = T, scale = F),
         Similar_week.c = scale(Similar_week, center = T, scale = F),
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
    Comparison == "postpre" ~ 0
  ))

change2 <- change %>% 
  pivot_longer(cols = c("pre","post"),
               names_to = "Time",
               values_to = "Rating")


# get objective competence performance scores
video <- read_csv("video_data.csv")

video_sum <- video %>% 
  mutate(Puzzle = recode(Puzzle, EMPID = "flyer",
                         crumpet = "myspace")) %>% 
  group_by(Group) %>% 
  count(Subject) %>% 
  filter(Subject != 'None') %>% 
  rename(ParticipationObjective = "n") %>%
  rename(Teammate = "Subject")

# get objective sociability performance scores
transcript <- read_csv('transcript_info.csv')
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

###### QUESTION 1 ######
## Models ####
mChangeComp1 <- brm(Rating ~ Time.d + 
                      (1 + Time.d | ID),
                    data = change2 %>% 
                      filter(Dimension == 'Ability') %>% 
                      mutate(Time.d = if_else(Time == 'pre',0,1)),
                    cores = 4, chains = 2, iter = 6000)

mChangeSoc1 <- brm(Rating ~ Time.d + 
                     (1 + Time.d | ID),
                   data = change2 %>% 
                     filter(Dimension == 'Sociability') %>% 
                     mutate(Time.d = if_else(Time == 'pre',0,1)),
                   cores = 4, chains = 2, iter = 6000)

## Figures ####

ggplot(change2 %>% 
         filter(Dimension == "Ability") %>% 
         mutate(Time = factor(Time,
                              levels = c("pre","post"))),
       aes(x = Time, y = Rating)) +
  geom_line(alpha = .05,aes(group = interaction(ID,Teammate))) +
  geom_violin(fill = "orchid3") +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = .6) +
  theme_classic() +
  scale_x_discrete(labels = c("1-7 days prior\nto escape room",
                              "Immediately after\nescape room")) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  labs(y = "Competence Rating",
       title = "Change in perceived competence") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = .5),
        axis.title.x = element_blank())
ggsave("figs/paper/time_comp_noduration.jpg")

ggplot(change2 %>% 
         filter(Dimension == "Sociability") %>% 
         mutate(Time = factor(Time,
                              levels = c("pre","post"))),
       aes(x = Time, y = Rating)) +
  geom_line(alpha = .05,aes(group = interaction(ID,Teammate))) +
  geom_violin(fill = "darkgoldenrod2") +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = .6) +
  theme_classic() +
  scale_x_discrete(labels = c("1-7 days prior\nto escape room",
                              "Immediately after\nescape room")) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  labs(y = "Sociability Rating",
       title = "Change in perceived sociability") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = .5),
        axis.title.x = element_blank())
ggsave("figs/paper/time_soc_noduration.jpg")


###### QUESTION 2 ######
## Models ######
mLikeFamiliarSimComp <- brm(post ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs + pre +
                              (1 + Like_pre.cs*Familiar_pre.cs*Similar_pre.cs + pre | ID),
                            data = change %>% 
                              filter(Dimension == 'Ability'),
                            cores = 4, chains = 2, iter = 8000)

mLikeFamiliarSimSoc <- brm(post ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs + pre +
                             (1 + Like_pre.cs*Familiar_pre.cs*Similar_pre.cs + pre | ID),
                           data = change %>% 
                             filter(Dimension == 'Sociability'),
                           cores = 4, chains = 2, iter = 8000)

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

ggplot(pdf, aes(x = effect, y = x)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, color = 'orchid3') +
  geom_errorbar(aes(xmin = lower80, xmax = upper80),
                width = 0, size = 1.5, color = 'orchid3') +  
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
ggsave('figs/paper/mfs_target_comp.jpg')

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

ggplot(pdf, aes(x = effect, y = x)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, color = 'darkgoldenrod2') +
  geom_errorbar(aes(xmin = lower80, xmax = upper80),
                width = 0, size = 1.5, color = 'darkgoldenrod2') +  
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
ggsave('figs/paper/mfs_target_soc.jpg')

###### QUESTION 3 ######
## Models #####
# objective performance #####
mPerfComp <- brm(post ~ ParticipationObjective.cs + pre + 
                   (1 + ParticipationObjective.cs + pre | ID),
                 data = change %>% 
                   filter(Dimension == 'Ability'),
                 cores = 4, chains = 2, iter = 8000)

mInterComp1 <- brm(post ~ Like_pre.cs*Similar_pre.cs*ParticipationObjective.cs + pre +
                     (1 + Like_pre.cs*Similar_pre.cs*ParticipationObjective.cs + pre | ID),
                   data = change %>% 
                     filter(Dimension == 'Ability'),
                   cores = 4, chains = 2, iter = 8000)

mInterComp2 <- brm(post ~ Familiar_pre.cs*Similar_pre.cs*ParticipationObjective.cs + pre +
                     (1 + Familiar_pre.cs*Similar_pre.cs*ParticipationObjective.cs + pre | ID),
                   data = change %>% 
                     filter(Dimension == 'Ability'),
                   cores = 4, chains = 2, iter = 8000)

mPerfSoc <- brm(post ~ collab_ratio.cs + pre + 
                  (1 + collab_ratio.cs + pre | ID),
                data = change %>% 
                  filter(Dimension == 'Sociability'),
                cores = 4, chains = 2, iter = 8000)

mInterSoc1 <- brm(post ~ Like_pre.cs*Familiar_pre.cs*collab_ratio.cs + pre +
                    (1 + Like_pre.cs*Familiar_pre.cs*collab_ratio.cs + pre | ID),
                  data = change %>% 
                    filter(Dimension == 'Sociability'),
                  cores = 4, chains = 2, iter = 8000)

mInterSoc2 <- brm(post ~ Similar_pre.cs*Familiar_pre.cs*collab_ratio.cs + pre +
                    (1 + Similar_pre.cs*Familiar_pre.cs*collab_ratio.cs + pre | ID),
                  data = change %>% 
                    filter(Dimension == 'Sociability'),
                  cores = 4, chains = 2, iter = 8000)

# PAB #####
change <- change %>% 
  mutate(ParticipationObjective.rs = scales::rescale(ParticipationObjective,to = c(0,10)),
         collab_ratio.rs = scales::rescale(collab_ratio, to = c(0,10)),
         num_utterances.rs = scales::rescale(collab_ratio, to = c(0,10))) %>% 
  mutate(TeamCollabDiff_post = TeamCollab_post - collab_ratio.rs,
         SolvingPuzzlesDiff_post = SolvingPuzzles_post - ParticipationObjective.rs)

mAccComp <- brm(SolvingPuzzlesDiff_post ~ Familiar_pre.cs*Like_pre.cs*Similar_pre.cs + 
                  (1 + Familiar_pre.cs*Like_pre.cs*Similar_pre.cs | ID), 
                data = change %>% 
                  distinct(ID,Teammate,.keep_all = T),
                cores = 4, chains = 2, iter = 8000)

mAccSoc <- brm(TeamCollabDiff_post ~ Familiar_pre.cs*Like_pre.cs*Similar_pre.cs + 
                 (1 + Familiar_pre.cs*Like_pre.cs*Similar_pre.cs | ID), 
               data = change %>% 
                 distinct(ID,Teammate,.keep_all = T),
               cores = 4, chains = 2, iter = 8000)

# mediation #####
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

mMediationComp <- brm(post ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs*SolvingPuzzlesDiff_post + pre +
                        (1 + Like_pre.cs*Familiar_pre.cs*Similar_pre.cs*SolvingPuzzlesDiff_post + pre | ID),
                      data = change %>% 
                        filter(Dimension == 'Ability'),
                      cores = 4, chains = 2, iter = 8000)

mMediationSoc <- brm(post ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs*TeamCollabDiff_post + pre +
                       (1 + Like_pre.cs*Familiar_pre.cs*Similar_pre.cs*TeamCollabDiff_post + pre | ID),
                     data = change %>% 
                       filter(Dimension == 'Sociability'),
                     cores = 4, chains = 2, iter = 8000)




## Figures #####
med <- median(change$ParticipationObjective.cs,na.rm = T)
med3 <- median(change$collab_ratio.cs, na.rm = T)

ggplot(change %>% 
         filter(Dimension == "Ability",
                !is.na(ParticipationObjective.cs)) %>% 
         mutate(ParticipationObjective.csd = as.factor(if_else(ParticipationObjective.cs > med,1,0))),
       aes(x = Similar_pre, y = post, 
           color = ParticipationObjective.csd, group = ParticipationObjective.csd)) +
  geom_jitter(alpha = .2) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("orchid4","orchid1"),
                     labels = c("Low","High")) +
  theme_classic() +
  labs(x = "Similarity", y = "Post-game competence rating", color = "Performance\nScore",
       title = "Puzzle solving performance x\nSimilarity") +
  theme(plot.title = element_text(size = 18, hjust = .5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ggsave("figs/paper/inter_comp.jpg")


ggplot(change %>% 
         filter(Dimension == "Sociability",
                !is.na(collab_ratio.cs)) %>% 
         mutate(collab_ratio.csd = as.factor(if_else(collab_ratio.cs > med3,1,0))),
       aes(x = Familiar_pre, y = post, 
           color = collab_ratio.csd, group = collab_ratio.csd)) +
  geom_jitter(alpha = .2) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("darkgoldenrod4","darkgoldenrod1"),
                     labels = c("Low","High")) +
  theme_classic() +
  labs(x = "Familiarity", y = "Post-game sociability rating", color = "Performance\nScore",
       title = "Team collaboration performance x\nfamiliarity") +
  theme(plot.title = element_text(size = 18, hjust = .5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ggsave("figs/paper/inter_soc.jpg")




