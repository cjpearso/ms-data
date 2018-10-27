library(lmerTest)
library(lmer)
library(tidyverse) 
read.csv("D:\\Documents\\r_github_projects\\ms-data\\ms_dataset.csv",header=T) -> df

df <- rename(df,hum_trust=`ï..hum_trust`) #clean df names

#elgonate trust variables
df_long_trust <- df %>% 
  rownames_to_column(var="par") %>%
  select(hum_trust, auto_trust,adv_auto_lvl,adv_hum_lvl,par) %>%
  gather(adviser,trust,-adv_hum_lvl,-adv_auto_lvl,-par) %>%
  mutate(adviser=gsub("_trust","",adviser))  %>%
  mutate(trust_grand_c = trust-3.764583) #grand mean center trust


#ignore, written for SAS
#write.csv(df_long_trust,"C:\\Users\\carlj\\Documents\\r_github_projects\\ms-data\\ms_long2.csv")
#mean(df_long_trust$trust_grand_c)



lmm <- lmer(trust_grand_c ~ adviser + adv_auto_lvl + adv_hum_lvl + adv_auto_lvl*adv_hum_lvl + (1 | par), data = df_long_trust, REML = FALSE)

summary(lmm)


#how the hell do contrasts work
contest(lmm, 5, rhs = 0, joint = TRUE,
        collect = TRUE, confint = TRUE, level = 0.95,
        check_estimability = FALSE, ddf = c("Satterthwaite", "Kenward-Roger",
                                            "lme4"))



