library(tidyverse)
library(readxl) # read excel files

# Load ethiopia data
eth_df<-read_xlsx("input/preprocessed-data/tamassa_ethiopia.xlsx")

# Add N, P, K columns
eth_df <- eth_df %>% mutate(N=0,P=0,K=0)

# Assign the real fertiliser rates from tamassa database

eth_df_npk<-eth_df %>% mutate(N=case_when(
  treatment=="Control" ~ 0,
  treatment=="NPK+" ~ 120,
  treatment=="NPK" ~ 120,
  treatment=="NP" ~ 120,
  treatment=="NK" ~ 120,
  treatment=="PK" ~ 0,
  TRUE ~ N
),
P=case_when(
  treatment=="Control" ~ 0,
  treatment=="NPK+" ~ 40,
  treatment=="NPK" ~ 40,
  treatment=="NP" ~ 40,
  treatment=="NK" ~ 0,
  treatment=="PK" ~ 40,
  TRUE ~ P
),
K=case_when(
  treatment=="Control" ~ 0,
  treatment=="NPK+" ~ 40,
  treatment=="NPK" ~ 40,
  treatment=="NP" ~ 0,
  treatment=="NK" ~ 40,
  treatment=="PK" ~ 40,
  TRUE ~ K
))

write.csv(eth_df_npk,"input/processed-data/tamassa_ethiopia_npk.csv",row.names = F)

# Load Nigeria data
ng_df<-read_xlsx("input/preprocessed-data/tamassa_nigeria.xlsx")
# Add N, P, K columns
ng_df <- ng_df %>% mutate(N=0,P=0,K=0)

# Assign the real fertiliser rates from tamassa database
ng_df_npk<-ng_df %>% mutate(N=case_when(
  state=="Kano" & (LGA=="Bunkure"|LGA=="Tofa") & aez=="Sudan savanna" ~120, 
  TRUE ~ 140
),
P=case_when(
  state=="Kano" & (LGA=="Bunkure"|LGA=="Tofa") & aez=="Sudan savanna"~ 40, 
  TRUE ~ 50
),
K=case_when(
  state=="Kano" & (LGA=="Bunkure"|LGA=="Tofa") & aez=="Sudan savanna" ~ 40 ,
  TRUE ~ 50
))

write.csv(ng_df_npk,"input/processed-data/tamassa_nigeria_npk.csv",row.names = F)


# Load the Tanzania data
tz_df<-read_xlsx("input/preprocessed-data/tamassa_nigeria.xlsx")