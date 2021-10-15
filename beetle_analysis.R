

# load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)

# load data ---------------------------------------------------------------

# data are in an xlsx file with multiple tabs so need to read each one in,
# standardize a few column names or data types, then bind the sheets together

xl_data <- "./data_raw/Dung_Database_to_read.xlsx"
excel_sheets(path = xl_data)
tab_names <- excel_sheets(path = xl_data)

btl_data = tibble(File = tab_names) %>%
  extract(File, "date",  "(\\d+)\\s*\\.", remove = FALSE) %>%
  mutate(Data= lapply(xl_data, read_excel)) %>% 
  unnest(Data) %>%
  select(-date) 
btl_data
names(btl_data)


xl_data <- "./data_raw/Dung_Database_to_read.xlsx"
sample1<-read_excel(xl_data, 1) %>% mutate(sample="sample_1",.before="Key")
sample2<-read_excel(xl_data, 2) %>% mutate(sample="sample_2",.before="Key")
sample3<-read_excel(xl_data, 3) %>% mutate(sample="sample_3",.before="Key")
sample4<-read_excel(xl_data, 4) %>% mutate(sample="sample_4",.before="Key")
sample5<-read_excel(xl_data, 5) %>% mutate(sample="sample_5",.before="Key")
sample6<-read_excel(xl_data, 6) %>% mutate(sample="sample_6",.before="Key")
sample7<-read_excel(xl_data, 7) %>% mutate(sample="sample_7",.before="key") %>% rename("Key"="key")
sample8<-read_excel(xl_data, 8) %>% mutate(sample="sample_8",.before="Key")
sample9<-read_excel(xl_data, 9) %>% mutate(sample="sample_9",.before="ID") %>% rename("Key"="ID")
sample10<-read_excel(xl_data, 10) %>% mutate(sample="sample_10",.before="ID") %>% rename("Key"="ID") 
sample11<-read_excel(xl_data, 11) %>% mutate(sample="sample_11",.before="ID") %>% rename("Key"="ID")
sample12<-read_excel(xl_data, 12) %>% mutate(sample="sample_12",.before="ID") %>% rename("Key"="ID")
sample13<-read_excel(xl_data, 13) %>% mutate(sample="sample_13",.before="ID") %>% rename("Key"="ID")

sample5$`unknown (see pinned)`<-as.character(sample5$`unknown (see pinned)`)


btl_data<-bind_rows(sample1,
                    sample2,
                    sample3,
                    sample4,
                    sample5,
                    sample6,
                    sample7,
                    sample8,
                    sample9,
                    sample10,
                    sample11,
                    sample12,
                    sample13
                    )



# files <- list.files("./data-backup-august_2011", full.names = TRUE,  recursive = TRUE)
# btl_data = tibble(File = files) %>%
#   extract(File, "date",  "(\\d+)\\s*\\.", remove = FALSE) %>%
#   mutate(Data = lapply(File, read_csv)) %>%
#   unnest(Data) %>%
#   select(-File)
# btl_data
# names(btl_data)
#  


# clean up datafile -------------------------------------------------------

# names(btl_data)

# add the dates of each round of sampling

btl_data <- btl_data %>%
  mutate(date=NA, .after = sample) %>% 
  mutate(date = ifelse(sample=="sample_1", 20101016,date)) %>% 
  mutate(date = ifelse(sample=="sample_2", 20101113,date)) %>% 
  mutate(date = ifelse(sample=="sample_3", 20101212,date)) %>% 
  mutate(date = ifelse(sample=="sample_4", 20110116,date)) %>% 
  mutate(date = ifelse(sample=="sample_5", 20110213,date)) %>% 
  mutate(date = ifelse(sample=="sample_6", 20110312,date)) %>% 
  mutate(date = ifelse(sample=="sample_7", 20110409,date)) %>% 
  mutate(date = ifelse(sample=="sample_8", 20110508,date)) %>% 
  mutate(date = ifelse(sample=="sample_9", 20110604,date)) %>% 
  mutate(date = ifelse(sample=="sample_10", 20110702,date)) %>% 
  mutate(date = ifelse(sample=="sample_11", 20110807,date)) %>% 
  mutate(date = ifelse(sample=="sample_12", 20110904,date)) %>% 
  mutate(date = ifelse(sample=="sample_13", 20111001,date)) %>% 
  mutate(day=substr(date, 7, 8),.after=date) %>% 
  mutate(month=substr(date, 5, 6),.after=date) %>% 
  mutate(year=substr(date, 1, 4),.after=date) %>% 
  mutate(date=paste(year,month,day,sep="-"),.before=year) %>% 
  select(-year,-month,-day)
  
# order the factor "sample" so they are in temporal order
btl_data$date<-as.Date(btl_data$date)
btl_data$sample<-as.factor(btl_data$sample)
btl_data$sample <- ordered(btl_data$sample, 
                           levels = c("sample_1","sample_2","sample_3", 
                                      "sample_4", "sample_5", "sample_6", 
                                      "sample_7", "sample_8", "sample_9", 
                                      "sample_10", "sample_11","sample_12",
                                      "sample_13"))
btl_data

names(btl_data)

# fix column name
btl_data <- btl_data %>% 
  rename("trap_id"="Trap ID")

# make column names lower case
names(btl_data)<-tolower(names(btl_data))

# assign each trap to its site (i.e., transect)
btl_data <- btl_data %>%
  mutate(site = NA, .after = trap_id) %>% 
  mutate(site = ifelse(trap_id<11,"site_1",site)) %>% 
  mutate(site = ifelse(trap_id>10&trap_id<21,"site_2",site)) %>% 
  mutate(site = ifelse(trap_id>20&trap_id<31,"site_3",site)) %>% 
  mutate(site = ifelse(trap_id>30&trap_id<41,"site_4",site)) %>% 
  mutate(site = ifelse(trap_id>40&trap_id<51,"site_5",site)) %>% 
  mutate(site = ifelse(trap_id>50&trap_id<61,"site_6",site))


# assign each site to its habitat (longleaf pine or oak hammock)
btl_data <- btl_data %>%
  mutate(habitat=NA, .after = site) %>% 
  mutate(habitat = ifelse(site=="site_1"|site=="site_2"|site=="site_3","longleaf",habitat)) %>% 
  mutate(habitat = ifelse(site=="site_4"|site=="site_5"|site=="site_6","oak",habitat))  

# correct the column names for each species. 
# spelling errors in one column name for one species means there are two 
# columns for the same species, the data from one need to be merged from
# the other and the incorrect column deleted. Notes and Comments also 
# needed to be merged into one column. The various columns of unknown or 
# unidentified species need to be merged.

names_list<-levels(as.factor(names(btl_data)))

btl_data$`unknown (see pinned)`<- floor(btl_data$unknown)
btl_data<-btl_data %>% 
  mutate(`aphodius stupidus 16` = ifelse(is.na(`aphodius stupidus16`),`aphodius stupidus 16`,`aphodius stupidus16`)) %>% 
  select(-`aphodius stupidus16`) %>% 
  mutate(`onthophagus striatulus floridanus 22` = ifelse(is.na(`onthophagus striatulus fl 22`),`onthophagus striatulus floridanus 22`,`onthophagus striatulus fl 22`)) %>% 
  select(-`onthophagus striatulus fl 22`) %>% 
  mutate(`onthophagus pennsylvanicus 4` = ifelse(is.na(`onthophagus pennsylvanicus 25`),`onthophagus pennsylvanicus 4`,`onthophagus pennsylvanicus 25`)) %>% 
  select(-`onthophagus pennsylvanicus 25`) %>% 
  # mutate(key = ifelse(is.na(key2),key,key2)) %>% 
  # select(-`key2`) %>% 
  mutate(notes = ifelse(is.na(comment),notes,comment)) %>% 
  select(-comment) %>% 
  mutate(notes = ifelse(is.na(`unknown (see pinned)`),notes,"see pinned")) %>% 
  mutate(unknown = ifelse(is.na(`unknown (see pinned)`),unknown,`unknown (see pinned)`)) %>% 
  select(-`unknown (see pinned)`) %>% 
  rename("other_sp"="other","other_sp_big"="big other") %>% 
  rowwise() %>% mutate(unknown_spp=sum(other_sp,other_sp_big,unknown, na.rm = TRUE)) %>% 
  select(-"other_sp",-"other_sp_big",-"unknown") 

# this removes the number from the column names
name_codes<-names(btl_data)
name_codes<-gsub('[[:digit:]]+', '', name_codes)
name_codes<-trimws(name_codes)
name_codes<-str_split(name_codes,pattern = " ",simplify = TRUE)
name_codes<-as.data.frame(name_codes)

# a few more names that need to be corrected
name_codes<-name_codes %>% 
  mutate(V2=if_else(V1=="geotrupes" & V2 == "b","blackburnii",V2)) %>% 
  mutate(V3=if_else(V1=="geotrupes" & V2 == "blackburnii","",V3)) %>% 
mutate(V2=if_else(V1=="glaphyrocanthon" & V2 == "v","viridis",V2)) %>% 
  mutate(V3=if_else(V1=="glaphyrocanthon" & V2 == "viridis","",V3)) %>% 
mutate(V1=if_else(V1=="beetle" & V2 == "mistakenly","morphosp_1",V1)) %>% 
  mutate(V2=if_else(V1=="morphosp_1","",V2)) %>% 
  mutate(V3=if_else(V1=="morphosp_1","",V3))  
name_codes$V4<-NULL
name_codes$V5<-NULL

# this section takes the names of the species and converts them into codes 
# using the first two letters of the genus and first two letters of the species

name_codes<-name_codes %>%
  mutate(name1=substr(V1,1,2)) %>% 
  mutate(name1=if_else(V2=="",V1,name1)) %>% 
  mutate(name2=substr(V2,1,2)) %>% 
  mutate(name3=substr(V3,1,2)) %>% 
  mutate(name1=if_else(name2=="",name1,name1)) %>% 
  mutate(name2=if_else(name2=="","AAA",name2)) %>% 
  mutate(name3=if_else(name3=="","AAA",name3)) %>% 
  mutate(name1=if_else(name1=="histeridae","hi_sp",name1)) %>% 
  mutate(code=paste(name1,name2,name3, sep="_")) %>% 
  mutate(code=gsub("_AAA", '', code)) %>% 
  mutate(species=paste(V1,V2,V3, sep=" ")) %>% 
  select(species,code)
name_codes$species<-trimws(name_codes$species)
names(btl_data)<-name_codes$code

# standardize the counts of each species by replacing NA with zero
btl_data<-btl_data %>% mutate(across(ph_ig_fl:unknown_spp, ~replace_na(.x, 0)))

# move the "notes" column
btl_data <-btl_data %>% relocate(notes, .before = "ph_ig_fl")

# covert the data to long format
btl_data_long<-btl_data %>% 
  pivot_longer(cols = ph_ig_fl:unknown_spp,
               names_to = "code",values_to = "count")



# save clean data and metadata  -------------------------------------------


# save the codes / names for the metadata
write_csv(name_codes,"./data_clean/name_codes.csv")

# save the data in long form
write_csv(btl_data_long,"./data_clean/dung_btl_data_clean.csv")

