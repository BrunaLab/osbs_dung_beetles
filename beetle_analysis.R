

# load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)

# load data ---------------------------------------------------------------

# data are in an xlsx file with multiple tabs so need to read each one in,
# standardize a few column names or data types, then bind the sheets together
xl_data <- "./data_raw/DungBeetles.Data/Beetle_Database.xlsx"
btl_data<-read_excel(xl_data, sheet = 1, skip=1)
sample_dates<-read_excel(xl_data, sheet = 2)
spp_names<-read_excel(xl_data, sheet = 3)



# clean up and standardize names - spp names ------------------------------

names(spp_names)<-tolower(names(spp_names))
spp_names<-spp_names %>% rename(id=`id #`, 
                                date_found=`date found`,
                                trap_id=`trap #`,
                                sp_code=abreviation) %>%
  select(-`...5`) %>% 
  mutate(sp_code=gsub("[.]","_",sp_code)) %>% 
  mutate(sp_code=substr(sp_code,1,5)) %>%
  mutate(sp_code=tolower(sp_code)) %>% 
  mutate(id=as.character(id)) %>% 
  mutate(id=as.factor(id)) %>% 
  mutate(trap_id=as.character(trap_id)) %>% 
  select(sp_code,family,genus,species)
spp_names


# clean up and standardize names - sample dates ---------------------------

sample_dates<-sample_dates %>% 
  rename(sample_round=`Month #`) %>% 
  rename(date_set=`date set`)
names(sample_dates)<-tolower(names(sample_dates))
# assign each trap to its site (i.e., transect)


# clean up and standardize names - btl data -------------------------------

btl_data <- btl_data %>%
  rename(sample_round=Month) %>% 
  rename(trap_id=`Trap ID`) %>% 
  mutate(site = NA, .after = trap_id) %>% 
  mutate(site = ifelse(trap_id<11,"site_1",site)) %>% 
  mutate(site = ifelse(trap_id>10&trap_id<21,"site_2",site)) %>% 
  mutate(site = ifelse(trap_id>20&trap_id<31,"site_3",site)) %>% 
  mutate(site = ifelse(trap_id>30&trap_id<41,"site_4",site)) %>% 
  mutate(site = ifelse(trap_id>40&trap_id<51,"site_5",site)) %>% 
  mutate(site = ifelse(trap_id>50&trap_id<61,"site_6",site))
# names(btl_data)


# assign each site to its habitat (longleaf pine or oak hammock)
btl_data <- btl_data %>%
  mutate(habitat=NA, .after = site) %>% 
  mutate(habitat = ifelse(site=="site_1"|site=="site_2"|site=="site_3","longleaf",habitat)) %>% 
  mutate(habitat = ifelse(site=="site_4"|site=="site_5"|site=="site_6","oak",habitat))  

# add the dates of each round of sampling
btl_data<-left_join(btl_data,sample_dates) %>% 
  relocate(c(month,date_set), .before = "sample_round") %>% 
  mutate(date_set=as.Date(date_set)) %>% 
  select(-month)
  
# order the factor "sample" so they are in temporal order

btl_data$sample_round <- ordered(btl_data$sample_round,seq(1:13)) 
btl_data



# make column names lower case and correct

names(btl_data)<-tolower(names(btl_data)) 
names(btl_data)<-gsub("[.]","_",names(btl_data)) 

btl_data<-btl_data %>% 
  rename(sp1=other,sp2=`big other`,unknown_spp=unknown)
  # covert the data to long format
  btl_data_long<-btl_data %>% 
    mutate_if(is.numeric,as.character) %>% 
  pivot_longer(cols = p_igneus:unknown_spp,
               names_to = "sp_code",values_to = "count") %>% 
    mutate(sp_code=substr(sp_code,1,5)) 
  
  btl_data_long<- left_join(btl_data_long,spp_names,by="sp_code")
  
  btl_data_long$id<-as.numeric(btl_data_long$id)
  btl_data_long$trap_id<-as.numeric(btl_data_long$trap_id)
  btl_data_long$habitat<-as.factor(btl_data_long$habitat)  
  btl_data_long$sp_code<-as.factor(btl_data_long$sp_code)  
  btl_data_long$count<-as.numeric(btl_data_long$count) 
  

# save clean data and metadata  -------------------------------------------


# save the codes / names for the metadata
# write_csv(name_codes,"./data_clean/name_codes.csv")

# save the data in long form
write_csv(btl_data_long,"./data_clean/dung_btl_data_clean.csv")

