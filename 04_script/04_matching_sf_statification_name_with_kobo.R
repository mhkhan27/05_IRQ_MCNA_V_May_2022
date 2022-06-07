



hc_sf <- read.csv("03_outputs/01_sampling/02_sampling/01_host_community/sampling_summary.csv") %>%
  rename(sample = X..surveys,
         buffer = X..buffer,
         effective_sample =Effective.sample) %>% mutate(
           Stratification = Stratification %>% tolower() %>% str_replace_all("-","_") %>% str_replace_all(" ","_")
         )




returnees_sf<- read.csv("03_outputs/01_sampling/02_sampling/03_Returnees/sampling_summary.csv") %>%
  rename(sample = X..surveys,
         buffer = X..buffer,
         effective_sample =Effective.sample) %>% select(Stratification,sample,effective_sample,buffer,Population)%>% mutate(
           Stratification = Stratification %>% tolower() %>% str_replace_all("-","_") %>% str_replace_all(" ","_") %>% str_replace_all("'","")
         ) 


out_idp_sf <- read.csv("03_outputs/01_sampling/02_sampling/02_IDPs/01_out_camp/sampling_summary.csv") %>%
  rename(sample = X..surveys,
         buffer = X..buffer,
         effective_sample =Effective.sample) %>% select(Stratification,sample,effective_sample,buffer,Population)%>% mutate(
           Stratification = Stratification %>% tolower() %>% str_replace_all("-","_") %>% str_replace_all(" ","_")  %>% str_replace_all("'","")
         ) 


in_idp_sf <- read.csv("03_outputs/01_sampling/02_sampling/02_IDPs/02_in_camp/sampling_summary.csv") %>%
  rename(sample = X..surveys,
         buffer = X..buffer,
         effective_sample =Effective.sample) %>% select(Stratification,sample,effective_sample,buffer,Population)%>% mutate(
           Stratification = Stratification %>% tolower() %>% str_replace_all("-","_") %>% str_replace_all(" ","_")  %>% str_replace_all("'","")
         ) %>% mutate(
           Stratification = case_when(Stratification == "bajet_kandala" ~ "bajed_kandala",
                                      Stratification == "berseve_1" ~ "bersive_1",
                                      Stratification == "berseve_2" ~ "bersive_1",
                                      Stratification == "dawadia" ~"dawoudia",
                                      Stratification == "qayyarah_jadah_5" ~ "qayyarah_jadah_five" ,
                                      T~Stratification)
         )
