##GAM 

library(mgcv)
library(visreg)
library(xtable)

setwd('/Users/Michela/Documents/MSc Biostatistics/Seminar/Case Study') # change here

ted <- read.csv("ted_for_modeling2.csv")


ted$event_binary = 1*(ted$event_binary=='TED')
ted$days <- as.numeric(as.Date("2017-09-21") -as.Date(ted$published_date)) 

#fir the gam model - we fit all explanatory variables, INCLUDING THE DAYS
m2 <- gam(log_pop ~ s(duration,bs="ts") + s(languages, k=66, bs="ts")  + s(author_max, bs="ts") + s(tag_score, bs="ts") + 
            s(number_relate, bs="ts") + s(description_nwords, k=5, bs="ts") + s(description_words_per_sentence, bs="ts") + (season)
          +(event_binary) + s(publish_to_now) + science_tech + art_lit_entertain +social
          + activist + politics_scholar + business_leader + performance
          + occupation_number+ questionword + s(tag_length, bs="ts") + s(title_length, bs="ts")+ s(num_speaker, k=1,bs="ts"),  data=ted)

#summary - see which levels are significant 
summary(m2)

lang <- visreg(m2, 'languages', gg=TRUE)
dura <- visreg(m2, 'duration', gg=TRUE)
author <- visreg(m2, 'author_max', gg=TRUE)
tag_s <- visreg(m2, 'tag_score', gg=TRUE)
num_r <- visreg(m2, 'number_relate', gg=TRUE)
num_s <-visreg(m2, 'num_speaker', gg=TRUE)

png('gam.pdf')
grid.arrange(lang, dura, author, tag_s, num_r, num_s, ncol=3, nrow=2)
dev.off()


#residual analysis
par(mfrow=c(2,2))
#we want high p-values in gam.check 
gam.check(m2)

#visualization of the fitted functions
par(mfrow=c(1,1))
plot(m2)

#some nicer visualizations
visreg2d(m2, xvar="number_relate", yvar='languages')
visreg2d(m2, xvar="duration", yvar='languages')
visreg2d(m2, xvar="number_relate", yvar='languages')
visreg2d(m2, xvar="number_relate", yvar='season')
visreg2d(m2, xvar="number_relate", yvar='event')

par(mfrow=c(1,1))
vis.gam(m2, view=c("author_max","languages"), theta=50)
vis.gam(m2, view=c("duration","languages"), theta=50)
vis.gam(m2, view=c("number_relate","languages"), theta=50)


par(mfrow=c(4,2))
visreg(m2, xvar=duration)

#calcualte MSE
mse <- mean((m2$fitted.values - ted$log_pop)^(2)); mse

#modelling the themes
#motivating question: Do popularity predictors change with different themes?
#method: divide the video into 8 themes

m1.t <- gam(log_pop ~ s(duration,bs="ts") + s(languages, bs="ts")  + s(author_max, bs="ts") + s(tag_score, bs="ts") + 
              s(number_relate, bs="ts") + s(description_nwords, k=1, bs="ts") + s(description_words_per_sentence, bs="ts") + as.factor(season)
            +as.factor(event) + s(days,bs="ts") + as.factor(science_tech) + as.factor(art_lit_entertain) +as.factor(social)
            + as.factor(activist) + as.factor(politics_scholar) + as.factor(business_leader) + as.factor(performance)
            + occupation_number+ questionword + s(tag_length, bs="ts") + s(title_length, bs="ts")+ s(num_speaker, k=1,bs="ts"), data=ted[which(ted$tag_Culture>=1),])
m1.t.summary <- summary(m1.t)
m2.t <- gam(log_pop ~ s(duration,bs="ts") + s(languages, bs="ts")  + s(author_max, bs="ts") + s(tag_score, bs="ts") + 
              s(number_relate, bs="ts") + s(description_nwords, k=1, bs="ts") + s(description_words_per_sentence, bs="ts") + as.factor(season)
            +as.factor(event) + s(days,bs="ts") + as.factor(science_tech) + as.factor(art_lit_entertain) +as.factor(social)
            + as.factor(activist) + as.factor(politics_scholar) + as.factor(business_leader) + as.factor(performance)
            + occupation_number+ questionword + s(tag_length, bs="ts") + s(title_length, bs="ts")+ s(num_speaker, k=1,bs="ts"), data=ted[which(ted$tag_Environmental_Issues>=1),])
m2.t.summary <- summary(m2.t)
m3.t <- gam(log_pop ~ s(duration,bs="ts") + s(languages, bs="ts")  + s(author_max, bs="ts") + s(tag_score, bs="ts") + 
              s(number_relate, bs="ts") + s(description_nwords, k=1, bs="ts") + s(description_words_per_sentence, bs="ts") + as.factor(season)
            +as.factor(event) + s(days,bs="ts") + as.factor(science_tech) + as.factor(art_lit_entertain) +as.factor(social)
            + as.factor(activist) + as.factor(politics_scholar) + as.factor(business_leader) + as.factor(performance)
            + occupation_number+ questionword + s(tag_length, bs="ts") + s(title_length, bs="ts")+ s(num_speaker, k=1,bs="ts"), data=ted[which(ted$tag_Global_Issues>=1),])
m3.t.summary <- summary(m3.t)
m4.t <- gam(log_pop ~ s(duration,bs="ts") + s(languages, bs="ts")  + s(author_max, bs="ts") + s(tag_score, bs="ts") + 
              s(number_relate, bs="ts") + s(description_nwords, k=1, bs="ts") + s(description_words_per_sentence, bs="ts") + as.factor(season)
            +as.factor(event) + s(days,bs="ts") + as.factor(science_tech) + as.factor(art_lit_entertain) +as.factor(social)
            + as.factor(activist) + as.factor(politics_scholar) + as.factor(business_leader) + as.factor(performance)
            + occupation_number+ questionword + s(tag_length, bs="ts") + s(title_length, bs="ts")+ s(num_speaker, k=1,bs="ts"), data=ted[which(ted$tag_Medicine>=1),])
m4.t.summary <- summary(m4.t)
m5.t <- gam(log_pop ~ s(duration,bs="ts") + s(languages,bs="ts")  + s(author_max, bs="ts") + s(tag_score, bs="ts") + 
              s(number_relate, bs="ts") + s(description_nwords, k=1, bs="ts") + s(description_words_per_sentence, bs="ts") + as.factor(season)
            +as.factor(event) + s(days,bs="ts") + as.factor(science_tech) + as.factor(art_lit_entertain) +as.factor(social)
            + as.factor(activist) + as.factor(politics_scholar) + as.factor(business_leader) + as.factor(performance)
            + occupation_number+ questionword + s(tag_length, bs="ts") + s(title_length, bs="ts")+ s(num_speaker, k=1,bs="ts"), data=ted[which(ted$tag_Space_Astronomy>=1),])
m5.t.summary <- summary(m5.t)
m6.t <- gam(log_pop ~ s(duration,bs="ts") + s(languages, bs="ts")  + s(author_max, bs="ts") + s(tag_score, bs="ts") + 
              s(number_relate, bs="ts") + s(description_nwords, k=1, bs="ts") + s(description_words_per_sentence, bs="ts") + as.factor(season)
            +as.factor(event) + s(days,bs="ts") + as.factor(science_tech) + as.factor(art_lit_entertain) +as.factor(social)
            + as.factor(activist) + as.factor(politics_scholar) + as.factor(business_leader) + as.factor(performance)
            + occupation_number+ questionword + s(tag_length, bs="ts") + s(title_length, bs="ts")+ s(num_speaker, k=1,bs="ts"), data=ted[which(ted$tag_Technology>=1),])
m6.t.summary <- summary(m6.t)
m7.t <- gam(log_pop ~ s(duration,bs="ts") + s(languages, bs="ts")  + s(author_max, bs="ts") + s(tag_score, bs="ts") + 
              s(number_relate, bs="ts") + s(description_nwords, k=1, bs="ts") + s(description_words_per_sentence, bs="ts") + as.factor(season)
            +as.factor(event) + s(days,bs="ts") + as.factor(science_tech) + as.factor(art_lit_entertain) +as.factor(social)
            + as.factor(activist) + as.factor(politics_scholar) + as.factor(business_leader) + as.factor(performance)
            + occupation_number+ questionword + s(tag_length, bs="ts") + s(title_length, bs="ts")+ s(num_speaker, k=1,bs="ts"), data=ted[which(ted$tag_Society_Religion>=1),])
m7.t.summary <- summary(m7.t)
m8.t <- gam(log_pop ~ s(duration,bs="ts") + s(languages, bs="ts")  + s(author_max, bs="ts") + s(tag_score, bs="ts") + 
              s(number_relate, bs="ts") + s(description_nwords, k=1, bs="ts") + s(description_words_per_sentence, bs="ts") + as.factor(season)
            +as.factor(event) + s(days,bs="ts") + as.factor(science_tech) + as.factor(art_lit_entertain) +as.factor(social)
            + as.factor(activist) + as.factor(politics_scholar) + as.factor(business_leader) + as.factor(performance)
            + occupation_number+ questionword + s(tag_length, bs="ts") + s(title_length, bs="ts")+ s(num_speaker, k=1,bs="ts"), data=ted[which(ted$tag_Psychology>=1),])
m8.t.summary <- summary(m8.t)


#modeling time
m.time <- bam(log_pop ~ days + s(duration,bs='ps', by=days) +s(languages, bs='ps', by=days) +s(author_max,bs='ps', by=days)
          +s(tag_score,bs='ps', by=days) + s(number_relate,bs='ps', by=days) + s(description_nwords,bs='ps', by=days)
          + s(description_words_per_sentence,bs='ps', by=days) + s(occupation_number,bs='ps', by=days) 
          + s(title_length,bs='ps', by=days)+ s(title_length,bs='ps', by=days)+ s(num_speaker,bs='ps', by=days),   data=ted)


m.time <- bam(log_pop ~ s(duration,bs='cs', by=days) +s(languages, bs='cs', by=days) +s(author_max,bs='cs', by=days)
              +s(tag_score,bs='cs', by=days) + s(number_relate,bs='cs', by=days) + s(description_nwords,bs='cs', by=days)
              + s(description_words_per_sentence,bs='cs', by=days),   data=ted)

summary(m.time)
par(mfrow=c(2,2))
gam.check(m.time)

par(mfrow=c(1,1))
plot(m.time)

#making the table for the theme comparison

theme_tab <- matrix(0, nrow=24, ncol=8)
row.names(theme_tab) = c('Spring','Summer','Winter','TED event', 'Science/Tech', 'Artist','Sociologist','Activist','Politics/Scholar','Business leader',
'Performance','Number of occupations','Title question','duration', 'languages', 'author_max',
                         'tag_score', 'number_relate','description_nwords','description_words_per_sentence',
                         'days', 'tag_length','title_length','num_speaker')
colnames(theme_tab) <- c('Culture', 'Environment', 'Global Issues', 'Medicine', 'Space', 'Technology',
                         'Religion', 'Psycholgy')



theme_tab[,1]=1*(c(m1.t.summary$p.pv[-1]<0.05,m1.t.summary$s.table[,4]<0.05))
theme_tab[,2]=1*(c(m2.t.summary$p.pv[-1]<0.05,m2.t.summary$s.table[,4]<0.05))
theme_tab[,3]=1*(c(m3.t.summary$p.pv[-1]<0.05,m3.t.summary$s.table[,4]<0.05))
theme_tab[,4]=1*(c(m4.t.summary$p.pv[-1]<0.05,m4.t.summary$s.table[,4]<0.05))
theme_tab[,5]=1*(c(m5.t.summary$p.pv[-1]<0.05,m5.t.summary$s.table[,4]<0.05))
theme_tab[,6]=1*(c(m6.t.summary$p.pv[-1]<0.05,m6.t.summary$s.table[,4]<0.05))
theme_tab[,7]=1*(c(m7.t.summary$p.pv[-1]<0.05,m7.t.summary$s.table[,4]<0.05))
theme_tab[,8]=1*(c(m8.t.summary$p.pv[-1]<0.05,m8.t.summary$s.table[,4]<0.05))

xtable(theme_tab, digits=0)
png('time_sig_factors.png')
par(mfrow=c(2,4))
visreg(m.time, 'duration', by='days', breaks=3)
visreg(m2, 'languages')
visreg(m.time, 'author_max')
visreg(m.time, 'tag_score')
visreg(m.time, 'number_relate')
visreg(m.time, 'description_nwords')
visreg(m.time, 'performance')
visreg(m.time, 'num_speaker')
dev.off()
