rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf , rio, 
               ggthemes, DataExplorer, haven, 
               corrplot, ggupset, ggridges, cowplot)




## BASES ----

data_raw <-  import("disease.csv")


## DEMOGRAPHIC ----

## CASOS ----


aux <-data_raw %>% 
  filter(!is.na(vaccination)) %>% 
  mutate(label=str_c(sex, vaccination), 
         label=str_replace(label, "yes", "_Vac"),
         label=str_replace(label, "no", "_NotVac")) %>% 
  dplyr::group_by(label,evolution) %>% 
  dplyr::summarise(text=n())%>%
  dplyr::ungroup() %>% 
  dplyr::mutate(text=if_else(text==1, str_c("n=", text,",\n age=33"), str_c("n=", text))) 

p <-data_raw %>% 
  filter(!is.na(vaccination)) %>% 
  mutate(label=str_c(sex, vaccination), 
         label=str_replace(label, "yes", "_Vac"),
         label=str_replace(label, "no", "_NotVac")) %>% 
  ggplot(aes(x=age, group=label, y=label))+
  geom_density_ridges(aes( fill=label), alpha=0.8)+
  geom_text(data=aux, aes(x=85, group=label, y=label,label=text), vjust=-1.5, size=3)+
  scale_fill_manual("", values=c("pink", "#e75480", "lightblue", "darkblue"),
                    labels = c("F", "F", "M", "M"))+
  scale_y_discrete(labels=c( "NotVac", "Vac", "NotVac", "Vac"), expand = c(0.01, 0))+
   scale_x_continuous(expand = c(0.01, 0))+
  facet_grid(.~evolution)+
   theme_ridges()


# pimage <- axis_canvas(p, axis = 'y') + 
#   draw_image("https://www.google.com/url?sa=i&url=https%3A%2F%2Fstock.adobe.com%2Fsearch%3Fk%3Dsyringe&psig=AOvVaw24xEGjBqP_XarOt21oVMY_&ust=1690049319031000&source=images&cd=vfe&opi=89978449&ved=0CBEQjRxqFwoTCIivtsCyoIADFQAAAAAdAAAAABAE", y = 2.5, scale = 0.5) +
#   draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/Iris_versicolor_3.jpg/320px-Iris_versicolor_3.jpg", y = 1.5, scale = 0.5) +
#   draw_image("https://www.google.com/url?sa=i&url=https%3A%2F%2Fstock.adobe.com%2Fsearch%3Fk%3Dsyringe&psig=AOvVaw24xEGjBqP_XarOt21oVMY_&ust=1690049319031000&source=images&cd=vfe&opi=89978449&ved=0CBEQjRxqFwoTCIivtsCyoIADFQAAAAAdAAAAABAE", y = 2.5, scale = 0.5) +
#   draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Kosaciec_szczecinkowaty_Iris_setosa.jpg/450px-Kosaciec_szczecinkowaty_Iris_setosa.jpg", y = 0.5, scale = 0.5)
# 

# insert the image strip into the plot
# ggdraw(insert_yaxis_grob(p, pimage))



data_raw %>% 
  filter(!is.na(vaccination), 
         !is.na(hospital)) %>% 
  mutate(label=str_c(sex, hospital)) %>% 
  ggplot()+
  geom_density(aes (x=age, fill=label), alpha=0.7)+
  scale_fill_manual("", values=c("pink", "#e75480", "lightblue", "darkblue"))+
  facet_grid(.~vaccination)+
  theme_bw(base_size = 14)
  

## Time series ----
data_raw %>% 
  ggplot(aes(x=date_onset ))+
  geom_bar()+
  theme_classic(base_size = 14)+
  labs(x="Date of onset", y="Number of cases")+
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d-%b") +
  theme(axis.text.x = element_text(angle=90, hjust=1))
  
ggsave("img/epidemic_simple.png", width=10)


data_raw %>% 
  ggplot(aes(x=date_onset ))+
  geom_bar(aes(fill=factor(region)))+
  theme_classic(base_size = 14)+
  labs(x="Date of onset", y="Number of cases")+
  scale_x_date(date_breaks = "4 day", 
               date_labels = "%d-%b") +
  scale_fill_brewer("Region",type="qal",  palette = "Dark2")+
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggsave("img/epidemic_region.png", width=10)



data_raw %>% 
  mutate(Onset=epiweek(date_onset),
         Notification=epiweek(date_notific))%>% 
  select(Onset, Notification)%>% 
  mutate(Notification=ifelse(is.na(Notification), Onset, Notification)) %>% 
  #mntidyr::complete(expand(Sint, Not)=1:29, fill= list(n=0))
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  count() %>% 
  tidyr::complete(name, value = 1:29, fill = list( freq = 0)) %>% 
  #mutate(value=paste0("W-", value)) %>% 
  ggplot(aes(x=factor(value), y=freq, color=name, group=name))+
  geom_point(size=2)+
  geom_line(size=1)+
  scale_color_manual("",values=c("darkred", "#6b6b6b") )+
  theme_bw(base_size = 14)+
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  labs(x="EpiWeek", y="Cases")

ggsave("img/epidemic_curve.png", width=10)



vac_region <- data_raw %>% 
  group_by(region) %>% 
  summarise(vaccnine = mean(vaccination=="yes")*100)


data <- data_raw %>% 
  filter(!is.na(hospital))%>% 
  ungroup() %>% 
  group_by(hospital) %>% 
  summarise(Tot=n())




# Basic piechart
ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = group), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")


## Symptoms ----

data_sintomas <- data_raw %>% 
  mutate(fever = ifelse(fever == "yes", "fever", NA), 
         jaundice = ifelse(jaundice == "yes", "jaundice", NA),
         FS= ifelse(faget_signal == "yes", "FS", NA)) %>% 
  unite(col = "all_symptoms",
        c(fever, jaundice, FS), 
        sep = "; ",
        remove = TRUE,
        na.rm = TRUE) %>% 
  mutate(
    # make a copy of all_symptoms column, but of class "list" (which is required to use ggupset() in next step)
    all_symptoms_list = as.list(strsplit(all_symptoms, "; "))
  )
  
  
graf <- ggplot(
  data = data_sintomas,
  mapping = aes(x = all_symptoms_list)) +
  geom_bar(color="#5c5c5c", fill="darkred") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1)+
  scale_x_upset(
    reverse = FALSE,
    n_intersections = 10,
    sets = c("fever", "jaundice", "FS"),
    size=12)+
  labs(
    title = "Signs & symptoms Yellow-Fever",
    subtitle = "The most frequent combinations",
    caption = "",
    x = "Symptom combination",
    y = "Frequency")+
  theme_bw(base_size = 16)+
  scale_y_continuous(limits = c(0,60))

ggsave(plot=graf, "img/sintoma.png", width=8)


##




