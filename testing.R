library(lmerTest)
library(lmer)
library(tidyverse) 
read.csv("C:\\Users\\carlj\\Documents\\r_github_projects\\ms-data\\ms_dataset.csv",header=T) -> df

df <- rename(df,hum_trust=`ï..hum_trust`)

df_long_trust <- df %>% 
  rownames_to_column(var="par") %>%
  select(hum_trust, auto_trust,adv_auto_lvl,adv_hum_lvl,par) %>%
  gather(adviser,trust,-adv_hum_lvl,-adv_auto_lvl,-par) %>%
  mutate(adviser=gsub("_trust","",adviser))  %>%
  mutate(trust_grand_c = trust-3.764583)


lmm <- lmer(trust_grand_c ~ adviser + adv_auto_lvl + adv_hum_lvl + adv_auto_lvl*adv_hum_lvl + (1 | par), data = df_long_trust, REML = FALSE)
summary(lmm)
plot(lmm)

df_long_trust %>% 
  ggplot(aes(x=adviser, y=trust, group=adv_auto_lvl*adv_hum_lvl, color=adv_auto_lvl*adv_hum_lvl, linetype=adv_auto_lvl*adv_hum_lvl)) + 
  geom_bar() + theme_classic()




write.csv(df_long_trust,"C:\\Users\\carlj\\Documents\\r_github_projects\\ms-data\\ms_long2.csv")


mean(df_long_trust$trust_grand_c)
