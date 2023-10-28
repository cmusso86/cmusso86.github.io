# Times series 
# Settings
options(scipen=999)
options(max.print = 99999999) 

# Libreries
pacman::p_load(rio,          # File import
               here,         # File locator
               tidyverse,    # data management + ggplot2 graphics
               tsibble,      # handle time series datasets
               slider,       # for calculating moving averages
               imputeTS,     # for filling in missing values
               feasts,       # for time series decomposition and autocorrelation
               forecast,     # fit sin and cosin terms to data (note: must load after feasts)
               trending,     # fit and assess models 
               tmaptools,    # for getting geocoordinates (lon/lat) based on place names
               ecmwfr,       # for interacting with copernicus sateliate CDS API
               stars,        # for reading in .nc (climate data) files
               units,        # for defining units of measurement (climate data)
               yardstick,    # for looking at model accuracy
               surveillance,  # for aberration detection
               patchwork
)

#Sys.setlocale(category = "LC_ALL", "es_ES.UTF-8") # Ajuste de idioma

family_text <- 'Roboto' # Estilo de las figuras

# Adjust graph
theme_set(theme_light() +
            theme(axis.text.y = element_text(size=8, angle=0), 
                  axis.text.x = element_text(size=8, angle=45, vjust=0.70, hjust=0.70),
                  plot.tag = element_text(face = "bold"),
                  legend.position = "none",
                  legend.title = element_blank(),
                  #panel.background = element_blank(),
                  #panel.grid.major = element_blank(), 
                  #panel.grid.minor = element_blank(),
                  legend.background = element_blank(),
                  plot.title = element_text(size=10, hjust=0.5),
                  plot.caption = element_text(hjust = 0.95), 
                  strip.background = element_rect(fill="white", color="white"), 
                  strip.text.x = element_text(size=10, hjust = 0)))

# 0. Explorer the data ----
data <- import("disease.csv") %>% janitor::clean_names()
data

glimpse(data)
table(data$date_onset, useNA = "ifany")
table(data$date_notific, useNA = "ifany") # 1 people no notification day, but have  onset day

# Epi-week
data <- data %>% 
  mutate(epiweek_onset = yearweek(date_onset)) %>% 
  mutate(epiweek_notific = yearweek(date_notific)) %>% 
  mutate(diff_on_not = difftime(date_notific, date_onset, units = c("days")),# diff days
         diff = as.numeric(diff_on_not)) 

data <- data %>% 
  arrange(epiweek_notific)

glimpse(data)
View(data)

min(data$epiweek_notific, na.rm = TRUE)
max(data$epiweek_notific, na.rm = TRUE)

min(data$epiweek_onset, na.rm = TRUE)
max(data$epiweek_onset, na.rm = TRUE)

glimpse(data)
ts_not <- data %>% 
  group_by(epiweek_notific) %>% 
  summarise(n_not=n()) %>% 
  ungroup() %>% 
  rename(epiweek=epiweek_notific)

min(ts_not$epiweek, na.rm = TRUE)
max(ts_not$epiweek, na.rm = TRUE)

ts_ons <- data %>% 
  group_by(epiweek_onset) %>% 
  summarise(n_ons=n()) %>% 
  ungroup() %>% 
  rename(epiweek=epiweek_onset)

min(ts_ons$epiweek, na.rm = TRUE)
max(ts_ons$epiweek, na.rm = TRUE)


ts <- ts_not %>% 
  full_join(ts_ons, by="epiweek")

ts_long <-  ts %>% 
  pivot_longer(cols = n_not:n_ons, names_to = "cases", values_to = "n") %>% 
  mutate()

g1 <- ts_long %>% 
  drop_na() %>% 
  ggplot(aes(x=epiweek, y=n, color=cases)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("orange", "dodgerblue4"),
                     labels = c("Notification", "Onset")) +
  scale_x_yearweek(date_breaks = '1 week') +
  scale_y_continuous(breaks = 0:20) +
  labs(y="Cases", x="Epiweek", caption = "") +
  theme(legend.position = "top")

g1

ggsave(g1, filename = paste0("g1_week.png"),
       res = 300,
       width = 30,
       height = 18,
       units = 'cm',
       scaling = 1,
       device = ragg::agg_png)



g1a <- ts_long %>% 
  drop_na() %>% 
  filter(cases=="n_not") %>% 
  ggplot(aes(x=epiweek, y=n, fill=cases)) +
  geom_bar(stat = "identity", width=7) +
  # geom_line() +
  # geom_point() +
  scale_fill_manual(values = c("orange"),
                    labels = c("Notification")) +
  scale_x_yearweek() +
  scale_y_continuous(breaks = 0:20) +
  labs(y="Cases", x="Epiweek", caption = "") +
  theme(legend.position = "top")

g1a

ggsave(g1a, filename = paste0("g1a_week.png"),
       res = 300,
       width = 30,
       height = 18,
       units = 'cm',
       scaling = 1,
       device = ragg::agg_png)

legenda <- c("W01", "W02", "W03", "W04",  "W05",  "W06" , 
             "W07",  "W08",  "W09" , "W10",  "W11" ,
             "W12",  "W13",  "W14","W15",  "W16", 
             "W17",  "W18", "W19", "W20", "W21", "W22", "W23", 
             "W24", "W25",  "W26" , "W27" , "W28",  "W29",  "W30", "W31" )

g1b <- ts_long %>% 
  drop_na() %>% 
  filter(cases=="n_ons") %>%  
  ggplot(aes(x=epiweek, y=n, fill=cases)) +
  geom_bar(stat = "identity", width=7, color="#5c5c5c") +
  # geom_line() +
  # geom_point() +
  scale_fill_manual("", values = c("darkred")) +
  scale_x_yearweek(date_breaks = '1 week', labels=legenda) +
  scale_y_continuous(breaks = 0:20) +
  labs(y="Cases", x="Epiweek of Onset (2023)", caption = "") +
  theme_classic(base_size = 16)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=45, hjust=1),
        panel.grid.major.y = element_line(size = 0.3,
                                          linetype = 2,
                                          color="#6b6b6b"))+
  geom_text(aes(x=as.Date(ts_long$epiweek[[4]]), 16, label="Phase 1"), 
            size=9 ,fontface="bold" )+
  geom_curve(aes(x = as.Date(ts_long$epiweek[[4]]), y = 15, xend = as.Date(ts_long$epiweek[[2]]), 
                 yend = 12),size = 1.8,
             arrow = arrow(length = unit(0.04, "npc")))+
  geom_text(aes(x=as.Date(ts_long$epiweek[[30]]), 17, label="Phase 2"), 
            size=9 ,fontface="bold")+
  geom_curve(aes(x = as.Date(ts_long$epiweek[[30]]), y = 16, xend = as.Date(ts_long$epiweek[[23]]), 
                 yend = 12),curvature = -0.2,size = 1.8,
             arrow = arrow(length = unit(0.04, "npc")))
  
g1b

ggsave(g1b, filename = paste0("g1b_week.png"),
       res = 300,
       width = 30,
       height = 18,
       units = 'cm',
       scaling = 1,
       device = ragg::agg_png)

g2 <- g1a / g1b

ggsave(g2, filename = paste0("g2_week.png"),
       res = 300,
       width = 30,
       height = 18,
       units = 'cm',
       scaling = 1,
       device = ragg::agg_png)
