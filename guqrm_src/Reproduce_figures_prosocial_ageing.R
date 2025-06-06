############################################
############ PNAS paper figures ############

# created: 19/11/19 (P Lockwood, L Priestley)

##### Setup #####

# set wd
getwd()

setwd('Your working directory here')

# load packages
library(tidyverse)
library(tidyselect)

# functions

# standard error
std.error <- function(x, na.rm = T){
  if(na.rm==T) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

##### K params #####

d <- read.csv('PM_debrief_final.csv') # get data

d$Group <- factor(d$Group, levels = c('1', '2'), labels = c('Young', 'Old')) # recode Group

# create d.long; data-frame with K params for each individual in each condition

d.long <- select(d, ID_Code, Group, Self_K, Other_K) %>% # select relevant variables
  pivot_longer(cols = c(Self_K, Other_K), names_to = 'Agent') # transform into long format (for ggplot)
d.long$Agent <- factor(d.long$Agent, levels = c("Self_K", "Other_K"), labels = c('Self', 'Other')) # recode Agent values
colnames(d.long)[4] <- 'K' # rename 'value' colname to 'K'
d.long <- unite(d.long, col = 'condition', Agent, Group) # combine Agent and Group into one column
d.long$Agent <- ifelse(grepl('Self', d.long$condition), '0', '1') # dummy code self vs other condition (in order to group by self
                                                                  # vs other during plotting) 

# create d.long.summary; data frame with mean K param + SE per condition
d.long.summary <- d.long %>% 
  group_by(condition) %>% 
  summarise(SE = std.error(K), m = mean(K, na.rm = T)) 
d.long.summary$Agent = c('1', '1', '0', '0') # dummy code self vs other condition (in order to group by self
                                             # vs other during plotting) 

# plot d.long.summary; barplot with mean K (+/- SE) in each condition 
plot.k <- ggplot(d.long.summary, aes(x = Agent, y = m, group = condition, fill = condition, color = condition)) + 
  geom_bar(stat = 'summary', fun.y = 'mean', position = position_dodge(0.9), alpha = 0.78, width = 0.85, size = 0.65) + 
  geom_errorbar(aes(y = m, ymin = m - SE, ymax = m + SE, group = condition), position = position_dodge(0.9), color = 'black', width = 0.2) + 
  scale_x_discrete(name = '', labels = c('Self K', 'Other K')) + 
  scale_y_continuous(name = 'K', breaks = seq(0, 0.25, 0.05)) + 
  coord_cartesian(ylim=c(0, 0.27)) + 
  scale_fill_manual(values = c('white', '#377EB8', 'white', '#E41A1C'), labels = c('Old (other)', 'Old (self)', 'Young (other)', 'Young (self)')) + 
  scale_color_manual(values = c('#377EB8', 'white', '#E41A1C', 'white'), labels = c('Old (other)', 'Old (self)', 'Young (other)', 'Young (self)')) + 
  theme_classic() + 
  theme(legend.position = c(0.3, 0.6)) + 
  theme(legend.title = element_blank()) + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.k
ggsave(filename = 'k_v1.pdf', plot.k, height = 5, width = 4)

# plot d.long.summary with d.long overlayed; barplot with mean K (+/- SE) in each condition, plus individual subject means
plot.k <- ggplot(d.long.summary, aes(x = Agent, y = m, group = condition, fill = condition, color = condition)) + 
  geom_bar(stat = 'summary', fun.y = 'mean', position = position_dodge(0.9), alpha = 0.78, width = 0.85, size = 0.75) + 
  geom_errorbar(aes(y = m, ymin = m - SE, ymax = m + SE, group = condition), position = position_dodge(0.9), color = 'black', width = 0.2) + 
  geom_jitter(data = d.long, aes(x = Agent, y = K, group = condition, color = Agent), position = position_jitterdodge(dodge.width = 0.4, jitter.height = 0, jitter.width = 0.05), size = 0.5, alpha = 0.5) + 
  scale_x_discrete(name = '', labels = c('Self K', 'Other K')) + 
  scale_y_continuous(name = 'K', breaks = seq(0, 1.5, 0.25)) + 
  coord_cartesian(ylim=c(0, 1.5)) + 
  scale_fill_manual(values = c('white', '#377EB8', 'white', '#E32929'), labels = c('Old (other)', 'Old (self)', 'Young (other)', 'Young (self)')) + 
  scale_color_manual(values = c('#FF9595', 'light blue', '#377EB8', 'white', '#E32929', 'white'), labels = c('Old (other)', 'Old (self)', 'Young (other)', 'Young (self)')) + 
  theme_classic() + 
  theme(legend.position = c(0.3, 0.8)) + 
  theme(legend.title = element_blank()) + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13)) + 
  theme(legend.position = 'none')
plot.k
ggsave(filename = 'k_v2.pdf', plot = plot.k, height = 5, width = 4)

##### self-rated pleasure vs K #####

rm(list = vars_select(ls(), starts_with('d', ignore.case = TRUE))) # remove previous dataset (and its derivatives)
rm(list = vars_select(ls(), starts_with('plot', ignore.case = TRUE))) # remove previous plots

d <- read.csv('PM_debrief_final.csv') # get data
d$Group <- factor(d$Group, levels = c('1', '2'), labels = c('Young', 'Old')) # recode Group

# create new data frames for self and other conditions

d.self <- select(d, ID_Code, Group,  Credits_feeling_Self, Self_K) # self data
d.other <- select(d, ID_Code, Group, Credits_feeling_Other, Other_K) # other data

# plot self; dotplot of K param vs self-reated pleasure at earning credits for self with overlayed 
# line of best fit; colour indicates age (young vs old)
plot.self <- ggplot(d.self, aes(x = Self_K, y = Credits_feeling_Self)) + 
  geom_point(aes(color = Group), size = 1, alpha = 0.5) + 
  geom_smooth(aes(group = Group, color = Group, fill = Group), method = 'lm', alpha = 0.3, size = 0.75) + 
  scale_x_continuous(name = 'K (self)', breaks = seq(0, 0.5, 0.1)) + 
  scale_y_continuous(name = 'Pleasure at winning for self', breaks = seq(0, 9, 3)) + 
  coord_cartesian(xlim=c(0, 0.5), ylim=c(0, 9.5)) + 
  scale_fill_manual(values = c('#99000d', '#fc9272')) + 
  scale_color_manual(values = c('#99000d', '#fc9272')) + 
  theme_classic() + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.self
ggsave(filename = 'pleasure_vs_k_self.pdf', plot = plot.self, width = 5, height = 4)

# plot self; dotplot of K param vs self-reated pleasure at earning credits for self with overlayed 
# line of best fit; colour indicates age (young vs old)
plot.other <- ggplot(d.other, aes(x = Other_K, y = Credits_feeling_Other)) + 
  geom_point(aes(color = Group), size = 1, alpha = 0.7) + 
  geom_smooth(aes(group = Group, color = Group, fill = Group), method = 'lm', alpha = 0.3, size = 0.75) + 
  scale_x_continuous(name = 'K (other)', breaks = seq(0, 1.5, 0.5)) + 
  scale_y_continuous(name = 'Pleasure at winning for other', breaks = seq(0, 9, 3)) + 
  coord_cartesian(xlim=c(0, 1.5), ylim=c(0, 9.5)) + 
  scale_fill_manual(values = c('#253494', '#4292c6')) + 
  scale_color_manual(values = c('#253494', '#4292c6')) + 
  theme_classic() + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.other
ggsave(filename = 'pleasure_vs_k_other.pdf', plot = plot.other, width = 5, height = 4)

##### Effort-level vs force exerted #####

# NB include only choice = 1 trials (i.e. trials on which force is exerted)

rm(list = vars_select(ls(), starts_with('d', ignore.case = TRUE))) # remove previous dataset (and its derivatives)
rm(list = vars_select(ls(), starts_with('plot', ignore.case = TRUE))) # remove previous plots

d <- read.csv('Force_choice_data_PM.csv') # get data
d <- filter(d, Choice == 1) # remove all trials on which there is no response

d$Group <- factor(d$Group, levels = c('1', '2'), labels = c('Young', 'Old')) # recode Group
d$Agent <- factor(d$Agent, levels = c('1', '2'), labels = c('Self', 'Other')) # recode Agent

# create data frame for mean force in self vs other conditions in the young group
d.force.young <- d %>% 
  filter(Group == 'Young') %>% # filter out old group
  group_by(ID, Agent, Effort) %>% 
  summarise(m = mean(Force_norm)) # get mean force for each participant in each effort*agent condition

# plot mean force (normalised AUC) per particpant at each effort-level
plot.force.young <- ggplot(d.force.young, aes(x = Effort, y = m)) + 
  geom_point(aes(group = Agent, color = Agent), position = position_dodge(width = 0.3), alpha = 0.3, shape = 1) + 
  scale_x_continuous(name = 'Effort level', breaks = seq(2, 6, 1)) + 
  scale_y_continuous(name = 'Force exerted (normalised AUC)', breaks = seq(-1.0, 1.0, 0.2)) + 
  scale_color_brewer(palette = 'Set1') + 
  theme_classic()
plot.force.young

# create data frame reflecting mean force (+/- SE) at each effort-level in young group 
d.force.young.summary <- d.force.young %>% 
  group_by(Agent, Effort) %>% 
  summarise(SE = std.error(m), m = mean(m))

# plot mean force (+/- SE) at each level of effort
plot.force.young.1 <- ggplot(d.force.young, aes(x = Effort, y = m)) + 
  #geom_point(aes(group = Agent, color = Agent), position = position_dodge(width = 0.3), alpha = 0.4, shape = 1, size = 0.5) + 
  geom_errorbar(data = d.force.young.summary, aes(group = Agent, color = Agent, ymin = m - SE, ymax = m + SE), position = position_dodge(0.02), alpha = 1, width = 0.2) + 
  geom_line(data = d.force.young.summary, aes(group = Agent, color = Agent), position = position_dodge(0.02)) + 
  #geom_point(data = d.force.young.summary, aes(group = Agent, color = Agent), position = position_dodge(0.25), size = 1) + 
  scale_x_continuous(name = 'Effort level', breaks = seq(2, 6, 1)) + 
  scale_y_continuous(name = 'Force exerted (normalised AUC)', breaks = seq(-1.0, 1.0, 0.2)) + 
  scale_color_brewer(palette = 'Set1') + 
  coord_cartesian(xlim = c(2, 6), ylim = c(0.30, 1.0)) + 
  theme_classic() + 
  theme(legend.position = c(0.9, 0.15)) + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.force.young.1
ggsave(filename = 'plot_force_young_v1.pdf', plot.force.young.1, height = 5, width = 4)

# plot mean force (+/- SE) at each level of effort AND show individual participant means (via dotplot)
plot.force.young.2 <- ggplot(d.force.young, aes(x = Effort, y = m)) + 
  geom_point(aes(group = Agent, color = Agent), position = position_dodge(width = 0.3), alpha = 0.4, shape = 1, size = 0.5) + 
  geom_errorbar(data = d.force.young.summary, aes(group = Agent, color = Agent, ymin = m - SE, ymax = m + SE), position = position_dodge(0.02), alpha = 1, width = 0.2) + 
  geom_line(data = d.force.young.summary, aes(group = Agent, color = Agent), position = position_dodge(0.02)) + 
  #geom_point(data = d.force.young.summary, aes(group = Agent, color = Agent), position = position_dodge(0.25), size = 1) + 
  scale_x_continuous(name = 'Effort level', breaks = seq(2, 6, 1)) + 
  scale_y_continuous(name = 'Force exerted (normalised AUC)', breaks = seq(-1.0, 1.0, 0.2)) + 
  scale_color_brewer(palette = 'Set1') + 
  coord_cartesian(xlim = c(2, 6), ylim = c(0.30, 1.0)) + 
  theme_classic() + 
  theme(legend.position = c(0.9, 0.15)) + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.force.young.2
ggsave(filename = 'plot_force_young_v2.pdf', plot.force.young.2, height = 5, width = 4)

# plot mean force line (no SE) AND show individual participant means (via dotplot)
plot.force.young.3 <- ggplot(d.force.young, aes(x = Effort, y = m, group = Agent, color = Agent, fill = Agent)) + 
  geom_point(data = d.force.young, aes(group = Agent, color = Agent), position = position_dodge(width = 0.3), alpha = 0.4, shape = 1, size = 0.5) + 
  geom_smooth(method = 'lm', alpha = 0.3, size = 0.6, se = F) + 
  coord_cartesian(xlim = c(2, 6), ylim = c(0.35, 1.0)) + 
  scale_x_continuous(name = 'Effort level', breaks = seq(2, 6, 1)) + 
  scale_y_continuous(name = 'Force exerted (normalised AUC)', breaks = seq(0, 1.0, 0.2)) + 
  scale_color_brewer(palette = 'Set1') + 
  scale_fill_brewer(palette = 'Set1') + 
  theme_classic() + 
  theme(legend.position = c(0.9, 0.15)) + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.force.young.3
ggsave(filename = 'plot_force_young_v3.pdf', plot.force.young.3, height = 5, width = 4)

# create data frame for mean force in self vs other conditions in the old group
d.force.old <- d %>% 
  filter(Group == 'Old') %>% # filter out 'young' participants
  group_by(ID, Agent, Effort) %>% 
  summarise(m = mean(Force_norm)) # get mean force for each participant in each effort*agent condition

# create data frame summarising mean force (+/- SE) at each effort-level in old group 
d.force.old.summary <- d.force.old %>% 
  group_by(Agent, Effort) %>% 
  summarise(SE = std.error(m), m = mean(m))

#plot mean force (+/- SE) at each effort-level in old group 
plot.force.old.1 <- ggplot(d.force.old, aes(x = Effort, y = m)) + 
  #geom_point(aes(group = Agent, color = Agent), position = position_dodge(width = 0.3), alpha = 0.4, shape = 1, size = 0.5) + 
  geom_errorbar(data = d.force.old.summary, aes(group = Agent, color = Agent, ymin = m - SE, ymax = m + SE), position = position_dodge(0.02), alpha = 1, width = 0.2) + 
  geom_line(data = d.force.old.summary, aes(group = Agent, color = Agent), position = position_dodge(0.02)) + 
  scale_x_continuous(name = 'Effort level', breaks = seq(2, 6, 1)) + 
  scale_y_continuous(name = 'Force exerted (normalised AUC)', breaks = seq(-1.0, 1.0, 0.2)) + 
  scale_color_brewer(palette = 'Set1') + 
  coord_cartesian(xlim = c(2, 6), ylim = c(0.30, 1.0)) + 
  theme_classic() + 
  theme(legend.position = c(0.9, 0.15)) + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.force.old.1
ggsave(filename = 'plot_force_old_v1.pdf', plot.force.old.1, height = 5, width = 4)

#plot mean force (+/- SE) at each effort-level in old group AND show individual data points
plot.force.old.2 <- ggplot(d.force.old, aes(x = Effort, y = m)) + 
  geom_point(aes(group = Agent, color = Agent), position = position_dodge(width = 0.3), alpha = 0.4, shape = 1, size = 0.5) + 
  geom_errorbar(data = d.force.old.summary, aes(group = Agent, color = Agent, ymin = m - SE, ymax = m + SE), position = position_dodge(0.02), alpha = 1, width = 0.2) + 
  geom_line(data = d.force.old.summary, aes(group = Agent, color = Agent), position = position_dodge(0.02)) + 
  scale_x_continuous(name = 'Effort level', breaks = seq(2, 6, 1)) + 
  scale_y_continuous(name = 'Force exerted (normalised AUC)', breaks = seq(0.0, 1.0, 0.2)) + 
  scale_color_brewer(palette = 'Set1') + 
  coord_cartesian(xlim = c(2, 6), ylim = c(0.30, 1.0)) + 
  theme_classic() + 
  theme(legend.position = c(0.9, 0.15)) + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.force.old.2
ggsave(filename = 'plot_force_old_v2.pdf', plot.force.old.2, height = 5, width = 4)

#plot mean force (no SE) at each effort-level in old group AND show individual data points
plot.force.old.3 <- ggplot(d.force.old, aes(x = Effort, y = m, group = Agent, color = Agent, fill = Agent)) + 
  geom_point(data = d.force.old, aes(group = Agent, color = Agent), position = position_dodge(width = 0.3), alpha = 0.4, shape = 1, size = 0.5) + 
  geom_smooth(method = 'lm', alpha = 0.3, size = 0.6, se = F) + 
  coord_cartesian(xlim = c(2, 6), ylim = c(0.35, 0.9)) + 
  scale_x_continuous(name = 'Effort level', breaks = seq(2, 6, 1)) + 
  scale_y_continuous(name = 'Force exerted (normalised AUC)', breaks = seq(0, 1.0, 0.2)) + 
  scale_color_brewer(palette = 'Set1') + 
  scale_fill_brewer(palette = 'Set1') + 
  theme_classic() + 
  theme(legend.position = c(0.9, 0.15)) + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.force.old.3
ggsave(filename = 'plot_force_old_v3.pdf', plot.force.old.3, height = 5, width = 4)

##### % acceptance rate vs effort-level #####

rm(list = vars_select(ls(), starts_with('d', ignore.case = TRUE))) # remove previous dataset (and its derivatives)
rm(list = vars_select(ls(), starts_with('plot', ignore.case = TRUE))) # remove previous plots

d <- read.csv('Force_choice_data_PM.csv') # get data
d$Group <- factor(d$Group, levels = c('1', '2'), labels = c('Young', 'Old')) # recode Group labels
d$Agent <- factor(d$Agent, levels = c('1', '2'), labels = c('Self', 'Other')) # recode Agent labels
d$Choice <- ifelse(d$Choice == 2, NA, d$Choice)

# create data frame for mean accept-rate (%) per participant, per effort-level in 'young' group
d.young.effort.1 <- filter(d, Group == 'Young') %>% 
  group_by(ID, Agent, Effort) %>% 
  summarise(m = mean(Choice)*100)

# create data frame summarising mean accept-rate (%) per effort-level across all participants in 'young' group
d.young.effort.2 <- d.young.effort.1 %>% 
  group_by(Agent, Effort) %>% 
  summarise(SE = std.error(m), M = mean(m, na.rm = T))

# plot effort young; barplot reflecing overall mean accept-rate (%) per effort-level in young group; 
# participant-level means overlayed as dotplot
plot.effort.young <- ggplot(d.young.effort.1, aes(x = Effort, y = m, group = Agent, fill = Agent)) + 
  geom_bar(stat = 'summary', fun.y = 'mean', color = 'black', position = 'dodge', width = 0.7, alpha = 0.78) +
  geom_errorbar(data = d.young.effort.2, aes(x = Effort, y = M, ymin = M - SE, ymax = M + SE, group = Agent), position = position_dodge(0.7), width = 0.1) + 
  geom_jitter(aes(color = Agent), position = position_jitterdodge(dodge.width = 1, jitter.width = 0.3, jitter.height = 0), alpha = 0.5, size = 0.50) + 
  scale_x_continuous(name = 'Effort level', breaks = seq(2, 6, 1)) + 
  scale_y_continuous(name = 'Percentage acceptance (%)', breaks = seq(0, 100, 20)) + 
  coord_cartesian(xlim = c(1.5, 6.5), ylim = c(0, 100)) + 
  scale_color_manual(values = c('#FF9595', 'light blue')) + 
  scale_fill_brewer(palette = 'Set1') + 
  theme_classic() + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.effort.young
ggsave(filename = 'effort_accept_young.pdf', plot = plot.effort.young, width = 4.5, height = 3)

# create data frame for mean accept-rate (%) per participant, per effort-level in 'old' group
d.effort.old.1 <- filter(d, Group == 'Old') %>%
  group_by(ID, Agent, Effort) %>%
  summarise(m = mean(Choice)*100)

# create data frame summarising mean accept-rate (%) per effort-level across all participants in 'old' group
d.effort.old.2 <- d.effort.old.1 %>%
  group_by(Agent, Effort) %>%
  summarise(SE = std.error(m), M = mean(m, na.rm = T))

# plot effort old; barplot reflecing overall mean accept-rate (%) per effort-level in old group; 
# participant-level means overlayed as dotplot
plot.effort.old <- ggplot(d.effort.old.1, aes(x = Effort, y = m, group = Agent, fill = Agent)) + 
  geom_bar(stat = 'summary', fun.y = 'mean', color = 'black', position = 'dodge', width = 0.7, alpha = 0.78) +
  geom_jitter(aes(color = Agent), position = position_jitterdodge(dodge.width = 1, jitter.width = 0.3, jitter.height = 0), alpha = 0.5, size = 0.65) + 
  geom_errorbar(data = d.effort.old.2, aes(x = Effort, y = M, ymin = M - SE, ymax = M + SE, group = Agent), position = position_dodge(0.7), width = 0.1) + 
  scale_x_continuous(name = 'Effort level', breaks = seq(2, 6, 1)) + 
  scale_y_continuous(name = 'Percentage acceptance (%)', breaks = seq(0, 100, 20)) + 
  coord_cartesian(xlim = c(1.5, 6.5), ylim = c(0, 100)) + 
  scale_color_manual(values = c('#FF9595', 'light blue')) + 
  scale_fill_brewer(palette = 'Set1') + 
  theme_classic() + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.effort.old
ggsave(filename = 'effort_accept_old.pdf', plot = plot.effort.old, width = 4.5, height = 3)

##### % acceptance rate vs reward #####

rm(list = vars_select(ls(), starts_with('d', ignore.case = TRUE))) # remove previous dataset (and its derivatives)
rm(list = vars_select(ls(), starts_with('plot', ignore.case = TRUE))) # remove previous plots

d <- read.csv('Force_choice_data_PM.csv') # get data
d$Group <- factor(d$Group, levels = c('1', '2'), labels = c('Young', 'Old')) # recode Group labels
d$Agent <- factor(d$Agent, levels = c('1', '2'), labels = c('Self', 'Other')) # recode Agent labels
d$Choice <- ifelse(d$Choice == 2, NA, d$Choice)

# create data frame reflecting mean accept rate (%) per participant at each reward-level in 'young' group
d.reward.young.1 <- filter(d, Group == 'Young') %>%
  group_by(ID, Agent, Reward) %>%
  summarise(m = mean(Choice)*100)
# create data frame reflecting overall mean accept rate (%) and SE at each reward-level in 'young' group
d.reward.young.2 <- d.reward.young.1 %>%
  group_by(Agent, Reward) %>%
  summarise(SE = std.error(m), M = mean(m, na.rm = T))

# plot reward young; barplot reflecing overall mean accept-rate (%) per reward-level in young group; 
# participant-level means overlayed as dotplot
plot.reward.young <- ggplot(d.reward.young.1, aes(x = Reward, y = m, group = Agent, fill = Agent)) + 
  geom_bar(stat = 'summary', fun.y = 'mean', color = 'black', position = 'dodge', width = 0.7, alpha = 0.78) +
  geom_jitter(aes(color = Agent), position = position_jitterdodge(dodge.width = 1, jitter.width = 0.3, jitter.height = 0), alpha = 0.5, size = 0.50) + 
  geom_errorbar(data = d.reward.young.2, aes(x = Reward, y = M, ymin = M - SE, ymax = M + SE, group = Agent), position = position_dodge(0.7), width = 0.1) + 
  scale_x_continuous(name = 'Reward level', breaks = seq(2, 6, 1)) + 
  scale_y_continuous(name = 'Percentage acceptance (%)', breaks = seq(0, 100, 20)) + 
  coord_cartesian(xlim = c(1.5, 6.5), ylim = c(0, 100)) + 
  scale_color_manual(values = c('#FF9595', 'light blue')) + 
  scale_fill_brewer(palette = 'Set1') + 
  theme_classic() + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.reward.young
ggsave(filename = 'reward_accept_young.pdf', plot.reward.young, height = 3, width = 4.5)

# create data frame reflecting mean accept rate (%) per participant at each reward-level in 'old' group
d.reward.old.1 <- filter(d, Group == 'Old') %>%
  group_by(ID, Agent, Reward) %>%
  summarise(m = mean(Choice)*100)
# create data frame reflecting overall mean accept rate (%) and SE at each reward-level in 'old' group
d.reward.old.2 <- d.reward.old.1 %>%
  group_by(Agent, Reward) %>%
  summarise(SE = std.error(m), M = mean(m, na.rm = T))

# plot reward young; barplot reflecing overall mean accept-rate (%) per reward-level in young group; 
# participant-level means overlayed as dotplot
plot.reward.old <- ggplot(d.reward.old.1, aes(x = Reward, y = m, group = Agent, fill = Agent)) + 
  geom_bar(stat = 'summary', fun.y = 'mean', color = 'black', position = 'dodge', width = 0.7, alpha = 0.78) +
  geom_jitter(aes(color = Agent), position = position_jitterdodge(dodge.width = 1, jitter.width = 0.3, jitter.height = 0), alpha = 0.5, size = 0.50) + 
  geom_errorbar(data = d.reward.old.2, aes(x = Reward, y = M, ymin = M - SE, ymax = M + SE, group = Agent), position = position_dodge(0.7), width = 0.1) + 
  scale_x_continuous(name = 'Reward level', breaks = seq(2, 6, 1)) + 
  scale_y_continuous(name = 'Percentage acceptance (%)', breaks = seq(0, 100, 20)) + 
  coord_cartesian(xlim = c(1.5, 6.5), ylim = c(0, 100)) + 
  scale_color_manual(values = c('#FF9595', 'light blue')) + 
  scale_fill_brewer(palette = 'Set1') + 
  theme_classic() + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.reward.old
ggsave(filename = 'reward_accept_old.pdf', plot.reward.old, height = 3, width = 4.5)

##### group difference (old - young) % acceptance rate #####

rm(list = vars_select(ls(), starts_with('d', ignore.case = TRUE))) # remove previous dataset (and its derivatives)
rm(list = vars_select(ls(), starts_with('plot', ignore.case = TRUE))) # remove previous plots

d <- read.csv('Force_choice_data_PM.csv') # det data
d$Group <- factor(d$Group, levels = c('1', '2'), labels = c('Young', 'Old')) # recode Group labels
d$Agent <- factor(d$Agent, levels = c('1', '2'), labels = c('Self', 'Other')) # recode Agent labels
d$Choice <- ifelse(d$Choice == 2, NA, d$Choice) # remove all trials on which no response is made (i.e. no effort exerted)

# create data frame reflecting overall difference in accept-rate (%) exerted for young vs old group at each effort-level
d.effort.comparison <- d %>% group_by(ID, Group, Agent, Effort) %>% summarise(m = mean(Choice)*100) # mean accept-rate (%) at each effort-level for each participant
d.effort.comparison <- d.effort.comparison %>% group_by(Group, Agent, Effort) %>% summarise(SE = std.error(m), m = mean(m, na.rm = T)) # mean accept-rate (%) at each effort-level in each Age group
d.effort.comparison <- d.effort.comparison %>% pivot_wider(names_from = Group, values_from = c(m, SE)) # reshape data for subtraction and plotting
d.effort.comparison$difference <- d.effort.comparison$m_Old - d.effort.comparison$m_Young # subtract mean accept-rate (%) in young group from mean accept-rate (%) in old group to get mean difference
d.effort.comparison$SE_mean <- (d.effort.comparison$SE_Old + d.effort.comparison$SE_Young)/2 # estimate SE of mean difference by taking average of SE in young and old groups, respectively

# plot effort difference; barplot reflecting mean difference in accept-rate (%) exerted by old vs young groups at each effort-level
plot.effort.comparison <- ggplot(d.effort.comparison, aes(x = Effort, y = difference, group = Agent, fill = Agent)) + 
  geom_bar(stat = 'summary', fun.y = 'mean', color = 'black', position = 'dodge', width = 0.7, alpha = 0.78) +
  geom_hline(yintercept = 0, linetype = 1, size = 0.5) + 
  geom_errorbar(aes(y = difference, ymin = difference - SE_mean, ymax = difference + SE_mean, group = Agent), position = position_dodge(0.75), width = 0.2, color = 'black') +
  xlab('Effort level') + 
  scale_y_continuous(name = 'Percentage acceptance (%)', breaks = seq(0, 100, 10)) + 
  coord_cartesian(xlim = c(1.5, 6.5), ylim = c(-2, 35)) + 
  scale_fill_brewer(palette = 'Set1') + 
  scale_color_brewer(palette = 'Set1') + 
  theme_classic() + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.effort.comparison
ggsave(filename = 'effort_difference.pdf', plot = plot.effort.comparison, height = 3, width = 4.5)

# create data frame reflecting overall difference in accept-rate (%) exerted for young vs old group at each reward-level
d.rw.comparison <- d %>% group_by(ID, Group, Agent, Reward) %>% summarise(m = mean(Choice)*100) # mean accept-rate (%) at each reward-level for each participant
d.rw.comparison <- d.rw.comparison %>% group_by(Group, Agent, Reward) %>% summarise(SE = std.error(m), m = mean(m, na.rm = T)) # mean accept-rate (%) at each effort-level in each Age group
d.rw.comparison <- d.rw.comparison %>% pivot_wider(names_from = Group, values_from = c(m, SE)) # reshape data for subtraction and plotting
d.rw.comparison$difference <- d.rw.comparison$m_Old - d.rw.comparison$m_Young # subtract mean accept-rate (%) in young group from mean accept-rate (%) in old group to get mean difference
d.rw.comparison$SE_mean <- (d.rw.comparison$SE_Old + d.rw.comparison$SE_Young)/2  # estimate SE of mean difference by taking average of SE in young and old groups, respectively

# plot reward difference; barplot reflecting mean difference in accept-rate (%) exerted by old vs young groups at each reward-level
plot.rw.comparison <- ggplot(d.rw.comparison, aes(x = Reward, y = difference, group = Agent, fill = Agent)) + 
  geom_bar(stat = 'summary', fun.y = 'mean', color = 'black', position = 'dodge', width = 0.7, alpha = 0.78) +
  geom_hline(yintercept = 0, linetype = 1, size = 0.5) + 
  geom_errorbar(aes(y = difference, ymin = difference - SE_mean, ymax = difference + SE_mean, group = Agent), position = position_dodge(0.74), width = 0.2, color = 'black') + 
  xlab('Reward level') + 
  scale_y_continuous(name = 'Percentage acceptance (%)', breaks = seq(0, 100, 10)) + 
  coord_cartesian(xlim = c(1.5, 6.5), ylim = c(-2, 35)) + 
  scale_fill_brewer(palette = 'Set1') + 
  scale_color_brewer(palette = 'Set1') + 
  theme_classic() + 
  theme(axis.text=element_text(size = 11), axis.title=element_text(size = 13))
plot.rw.comparison
ggsave(filename = 'reward_difference.pdf', plot = plot.rw.comparison, height = 3, width = 4.5)
