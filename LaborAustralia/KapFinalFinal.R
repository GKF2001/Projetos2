#Bibliotecas---------------------------------------------------------------------------------------------------------------------
library(readabs)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(knitr)
library(gt)
library(gridExtra)

#
diretorio<-'C:/Users/Gabriel/Desktop/plot.pdf'
diretorio2<-'C:/Users/Gabriel/Desktop/tabela.pdf'

#Funcoes--------------------------------------------------------------------------------------------
#Calc
calculate_metrics <- function(data, frequency = "monthly") {
  if (frequency == "quarterly") {
    lag_period_yoy = 4  #
    lag_period_pop = 1  
  } else {
    lag_period_yoy = 12  
    lag_period_pop = 1  
  }
  
  data %>%
    arrange(date) %>%
    mutate(
      YoY = (value - lag(value, lag_period_yoy)) / lag(value, lag_period_yoy) * 100,  
      MoM = (value - lag(value, lag_period_pop)) / lag(value, lag_period_pop) * 100,  
      Yearly_Change = value - lag(value, lag_period_yoy),                            
      Monthly_Change = value - lag(value, lag_period_pop)                             
    )
}

#Last
extract_latest_two <- function(data, category) {
  data %>%
    arrange(desc(date)) %>%
    distinct(date, .keep_all = TRUE) %>%  
    slice_head(n = 2) %>%
    mutate(Category = category) %>%
    select(Category, date, value, YoY, MoM, Yearly_Change, Monthly_Change)
}

#Dates
find_closest_date <- function(input_date, reference_dates) {
  abs_diff <- abs(as.numeric(difftime(input_date, reference_dates, units = "days")))
  closest_date <- reference_dates[which.min(abs_diff)]
  return(closest_date)
}








#Labour Market------------------------------------------------------------------------------------------------------------------
all_Labour=read_abs('6202.0',tables = 1)
Under_Labour=read_abs('6202.0',tables = 22)

#CLean
all_Labour <- all_Labour %>%
  select(-table_no, -sheet_no, -series_id,-unit)

#Date
all_Labour <-all_Labour %>%
  filter(date >= as.Date("2000-01-01"))

#Labor Force Total
Lb_Labour <- all_Labour %>%
  filter(table_title=='Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original',
         series=='Labour force total ;  Persons ;',series_type=='Original')

#Employed
Em_Labour <- all_Labour %>%
  filter(table_title=='Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original',
         series=='Employed total ;  Persons ;',series_type=='Original')


#Unemplotment Total
UN_Labour <- all_Labour %>%
  filter(table_title=='Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original',
         series=='Unemployed total ;  Persons ;',series_type=='Original')

Em_Labour2<-Em_Labour

#Full-Time
FT_Labour <- all_Labour %>%
  filter(table_title=='Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original',
         series=='> Employed full-time ;  Persons ;',series_type=='Original')

#Part-Time
PT_Labour <- all_Labour %>%
  filter(table_title=='Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original',
         series=='> Employed part-time ;  Persons ;',series_type=='Original')



#Underployment Rate
UDRT_Labour <- Under_Labour %>%
  filter(table_title=='Table 22. Underutilised persons by Age and Sex - Trend, Seasonally adjusted and Original',
         series=='Underemployment ratio (proportion of employed) ;  Persons ;',series_type=='Original')

#Paticipation Rate
PPRT_Labour <- all_Labour %>%
  filter(table_title=='Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original',
         series=='Participation rate ;  Persons ;',series_type=='Original')

#Unemployment Rate
UNRT_Labour <- all_Labour %>%
  filter(table_title=='Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original',
         series=='Unemployment rate ;  Persons ;',series_type=='Original')

#Underutilisation Rate
UTRT_Labour <- Under_Labour %>%
  filter(table_title=='Table 22. Underutilised persons by Age and Sex - Trend, Seasonally adjusted and Original',
         series=='Underutilisation rate ;  Persons ;',series_type=='Original')

#SeasonAdj
SA_Labour<-all_Labour %>%
  filter(series_type=='Seasonally Adjusted',
         table_title=='Table 1. Labour force status by Sex, Australia - Trend, Seasonally adjusted and Original')

SA_Labour_Under<-Under_Labour %>%
  filter(series_type=='Seasonally Adjusted')


SA_UN_Labour<-SA_Labour %>%
  filter(series=='Unemployed total ;  Persons ;')

SA_EM_Labour<-SA_Labour %>%
  filter(series=='Employed total ;  Persons ;')

SA_Lb_Labour <- SA_Labour %>%
  filter(series == 'Labour force total ;  Persons ;')

SA_FT_Labour <- SA_Labour %>%
  filter(series == '> Employed full-time ;  Persons ;')

SA_PT_Labour <- SA_Labour %>%
  filter(series == '> Employed part-time ;  Persons ;')


SA_UNRT_Labour<-SA_Labour %>%
  filter(series=='Unemployment rate ;  Persons ;')

SA_PPRT_Labour<-SA_Labour %>%
  filter(series=='Participation rate ;  Persons ;')

SA_UDRT_Labour<-SA_Labour_Under %>%
  filter(series=='Underemployment ratio (proportion of employed) ;  Persons ;')

SA_UTRT_Labour<-SA_Labour_Under %>%
  filter(series=='Underutilisation rate ;  Persons ;')







#JobVancany-----------------------------------------------------------------------------------------------------------------------------

all_JV=read_abs('6354.0')

#Clean
cleaned_JV <- all_JV %>%
  select(-table_no, -sheet_no, -series_id)

#Series Type
og_JV <- cleaned_JV %>%
  filter(series_type=='Original')

#Australia
au_JV2 <- og_JV %>%
  filter(series=='Job Vacancies ;  Total All Industries ;')

au_JV3 <- og_JV %>%
  filter(series=='Job Vacancies ;  Australia ;')

au_JV4 <- au_JV3
au_JV5 <- au_JV3

# JV by Sector

sectors <- c(
  'Job Vacancies ;  Manufacturing ;',
  'Job Vacancies ;  Electricity, Gas, Water and Waste Services ;',
  'Job Vacancies ;  Construction ;',
  'Job Vacancies ;  Wholesale Trade ;',
  'Job Vacancies ;  Retail Trade ;',
  'Job Vacancies ;  Accommodation and Food Services ;',
  'Job Vacancies ;  Transport, Postal and Warehousing ;',
  'Job Vacancies ;  Information Media and Telecommunications ;',
  'Job Vacancies ;  Financial and Insurance Services ;',
  'Job Vacancies ;  Rental, Hiring and Real Estate Services ;',
  'Job Vacancies ;  Professional, Scientific and Technical Services ;',
  'Job Vacancies ;  Administrative and Support Services ;',
  'Job Vacancies ;  Public Administration and Safety ;',
  'Job Vacancies ;  Education and Training ;',
  'Job Vacancies ;  Health Care and Social Assistance ;',
  'Job Vacancies ;  Arts and Recreation Services ;',
  'Job Vacancies ;  Other Services ;'
)


Ind_jv <- og_JV %>%
  filter(series %in% sectors)










#Tabelas


#Wage Price Index -------------------------------------------------------------------------------------------------------------
all_wpi <- read_abs("6345.0",tables = 1)

#Clean
cleaned_wpi <- all_wpi %>%
  select(-table_no, -sheet_no, -series_id)

#WPI index
WPI_Index<- cleaned_wpi %>%
  filter(series=='Quarterly Index ;  Total hourly rates of pay excluding bonuses ;  Australia ;  Private and Public ;  All industries ;',
         series_type=='Original')

WPI_Index<- WPI_Index %>%
  mutate(YoY = (value - lag(value, 4)) / lag(value, 4) * 100)





#Tabela 1-------------------------------------------------------------------------------------------------------------------------


SA_EM_Labour <- calculate_metrics(SA_EM_Labour)
SA_FT_Labour <- calculate_metrics(SA_FT_Labour)
SA_PT_Labour <- calculate_metrics(SA_PT_Labour)
SA_UN_Labour <- calculate_metrics(SA_UN_Labour)
SA_UNRT_Labour <- calculate_metrics(SA_UNRT_Labour)
SA_UDRT_Labour <- calculate_metrics(SA_UDRT_Labour)
SA_UTRT_Labour <- calculate_metrics(SA_UTRT_Labour)
SA_PPRT_Labour <- calculate_metrics(SA_PPRT_Labour)



combined_datatableSA <- bind_rows(
  extract_latest_two(SA_EM_Labour, "Employed Total"),
  extract_latest_two(SA_FT_Labour, "Full-Time Employment"),
  extract_latest_two(SA_PT_Labour, "Part-Time Employment"),
  extract_latest_two(SA_UN_Labour, "Unemployed Total"),
  extract_latest_two(SA_UNRT_Labour, "Unemployment Rate"),
  extract_latest_two(SA_UDRT_Labour, "Underemployment Rate"),
  extract_latest_two(SA_UTRT_Labour, "Underutilisation Rate"),
  extract_latest_two(SA_PPRT_Labour, "Participation Rate")
)

table1 <- gt(combined_datatableSA) %>%
  tab_header(
    title = "Key Statistics - SA",
    subtitle = ""
  ) %>%
  cols_label(
    date = "Date",
    value = "Value",
    YoY = "YoY (%)",
    MoM = "MoM (%)",
    Yearly_Change = "Yearly Change",
    Monthly_Change = "Monthly Change",
    Category = "Variable"
  ) %>%
  fmt_date(
    columns = vars(date),
    date_style = 3
  ) %>%
  fmt_number(
    columns = vars(value, YoY, MoM, Yearly_Change, Monthly_Change),
    decimals = 2
  ) %>%
  tab_options(
    table.width = pct(100),
    column_labels.font.size = "small",
    heading.title.font.size = "medium",
    heading.subtitle.font.size = "small"
  ) %>%
  tab_style(
    style = cell_fill(color = "gray95"),
    locations = cells_body(
      columns = TRUE,
      rows = TRUE
    )
  )


print(table1)


#Tabela 2-------------------------------------------------------------------------------------------------------------------------






Em_Labour <- calculate_metrics(Em_Labour)
FT_Labour <- calculate_metrics(FT_Labour)
PT_Labour <- calculate_metrics(PT_Labour)
UN_Labour <- calculate_metrics(UN_Labour)
UNRT_Labour <- calculate_metrics(UNRT_Labour)
UDRT_Labour <- calculate_metrics(UDRT_Labour)
UTRT_Labour <- calculate_metrics(UTRT_Labour)
PPRT_Labour <- calculate_metrics(PPRT_Labour)

combined_datatableNSA <- bind_rows(
  extract_latest_two(Em_Labour, "Employed Total"),
  extract_latest_two(FT_Labour, "Full-Time Employment"),
  extract_latest_two(PT_Labour, "Part-Time Employment"),
  extract_latest_two(UN_Labour, "Unemployed Total"),
  extract_latest_two(UNRT_Labour, "Unemployment Rate"),
  extract_latest_two(UDRT_Labour, "Underemployment Rate"),
  extract_latest_two(UTRT_Labour, "Underutilisation Rate"),
  extract_latest_two(PPRT_Labour, "Participation Rate")
)

table2 <- gt(combined_datatableNSA) %>%
  tab_header(
    title = "Key Statistics - NSA",
    subtitle = ""
  ) %>%
  cols_label(
    date = "Date",
    value = "Value",
    YoY = "YoY (%)",
    MoM = "MoM (%)",
    Yearly_Change = "Yearly Change",
    Monthly_Change = "Monthly Change",
    Category = "Variable"
  ) %>%
  fmt_date(
    columns = vars(date),
    date_style = 3
  ) %>%
  fmt_number(
    columns = vars(value, YoY, MoM, Yearly_Change, Monthly_Change),
    decimals = 2
  ) %>%
  tab_options(
    table.width = pct(100),
    column_labels.font.size = "small",
    heading.title.font.size = "medium",
    heading.subtitle.font.size = "small"
  ) %>%
  tab_style(
    style = cell_fill(color = "gray95"),
    locations = cells_body(
      columns = TRUE,
      rows = TRUE
    )
  )


print(table2)



#Grafico--------------------------------------------------------------------------------------------------------------------------


#Grafico 1 ---------------------------------------------------------------------------------------------------------------------

#Joining Bases
UNRT_Labour <- UNRT_Labour %>% mutate(type = "Unemployment Rate")
UDRT_Labour <- UDRT_Labour %>% mutate(type = "Underemployment Rate")
PPRT_Labour <- PPRT_Labour %>% mutate(type = "Participation Rate")
combined_labour_data <- bind_rows(UNRT_Labour, UDRT_Labour, PPRT_Labour)

combined_labour_data <-combined_labour_data %>%
  filter(date>=as.Date('2009-01-01'))

#Plot
fig1 <-ggplot(data = combined_labour_data, aes(x = date)) +
  geom_line(data = filter(combined_labour_data, type != "Participation Rate"), 
            aes(y = value, color = type), size = 1) + 
  geom_line(data = filter(combined_labour_data, type == "Participation Rate"), 
            aes(y = rescale(value, to = range(combined_labour_data$value[combined_labour_data$type != "Participation Rate"])), color = type), size = 1) +
  scale_y_continuous(
    name = "(%)",
    sec.axis = sec_axis(
      ~ rescale(., from = range(combined_labour_data$value[combined_labour_data$type != "Participation Rate"]), 
                to = range(combined_labour_data$value[combined_labour_data$type == "Participation Rate"])),
      name = "(%)"
    )
  ) +
  labs(
    title = "Figure 1 - Labour Market, Summary",
    subtitle = "Australia Labour Market",
    x = "",
    color = "Metric"
  ) +
  scale_color_manual(
    values = c("Unemployment Rate" = "red", 
               "Underemployment Rate" = "green", 
               "Participation Rate" = "blue")
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "red", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, color = "black", size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1, "cm")
  )

print(fig1)

#Grafico 2 ---------------------------------------------------------------------------------------------------------------------


#Joining Bases
combined_labour_data2 <- bind_rows(
  Lb_Labour %>% mutate(type = "Labour Force Total"),
  Em_Labour %>% mutate(type = "Employed Total"),
  UN_Labour %>% mutate(type = "Unemployed Total")
)

#Scale PRoblem
max_labour_force <- max(combined_labour_data2$value[combined_labour_data2$type == "Labour Force Total"], na.rm = TRUE)
max_unemployed <- max(combined_labour_data2$value[combined_labour_data2$type == "Unemployed Total"], na.rm = TRUE)
scale_factor <- max_labour_force / max_unemployed

#plot
fig2 <- ggplot(combined_labour_data2, aes(x = date, y = value, color = type)) +
  geom_line(data = filter(combined_labour_data2, type != "Unemployed Total"), size = 1) +
  geom_line(data = filter(combined_labour_data2, type == "Unemployed Total"), 
            aes(y = value * scale_factor), size = 1) +
  scale_y_continuous(
    name = "Labour Force & Employed Total Thousand",
    sec.axis = sec_axis(~ . / scale_factor, name = "Unemployed Total Thousand")
  ) +
  scale_color_manual(values = c("Labour Force Total" = "red", 
                                "Employed Total" = "green", 
                                "Unemployed Total" = "blue")) +
  labs(title = "Figure 2: Labour Force, Summary",
       subtitle = "Labour Market",
       x = "",
       y = "") +
  theme_light() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, color = "red", size = 16), 
        plot.subtitle = element_text(hjust = 0.5, color = "black", size = 12),
        legend.text = element_text(size = 10))

print(fig2)
#Grafico 3 ---------------------------------------------------------------------------------------------------------------------

#YoY
Em_GrowthYoY <- Em_Labour %>%
  arrange(date) %>%
  mutate(growth = (value - lag(value, 12)) / lag(value, 12) * 100)

FT_GrowthYoY <- FT_Labour %>%
  arrange(date) %>%
  mutate(growth = (value - lag(value, 12)) / lag(value, 12) * 100)

PT_GrowthYoY <- PT_Labour %>%
  arrange(date) %>%
  mutate(growth = (value - lag(value, 12)) / lag(value, 12) * 100)

# Combine all
combined_growthYoY <- bind_rows(
  Em_GrowthYoY %>% mutate(type = "Total"),
  FT_GrowthYoY %>% mutate(type = "Full-Time"),
  PT_GrowthYoY %>% mutate(type = "Part-Time")
)

#plot
fig3<-ggplot(combined_growthYoY, aes(x = date, y = growth, color = type)) +
  geom_line(size = 1) +  
  scale_color_manual(values = c("Total" = "red", "Full-Time" = "green", "Part-Time" = "blue")) +
  labs(
    title = "Figure 3: Employment Growth",
    subtitle = "Employment Growth",
    x = "",
    y = "(%)"
  ) +
  theme_light() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, color = "red", size = 16),  
    plot.subtitle = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(fig3)
#Grafico 4 ---------------------------------------------------------------------------------------------------------------------

#Abs Change
Em_Abs_Change <- Em_Labour %>%
  arrange(date) %>%
  mutate(change = value - lag(value, 12))

FT_Abs_Change <- FT_Labour %>%
  arrange(date) %>%
  mutate(change = value - lag(value, 12))

PT_Abs_Change <- PT_Labour %>%
  arrange(date) %>%
  mutate(change = value - lag(value, 12))


combined_changes <- bind_rows(
  Em_Abs_Change %>% mutate(type = "Total"),
  FT_Abs_Change %>% mutate(type = "Full-Time"),
  PT_Abs_Change %>% mutate(type = "Part-Time")
) %>%
  filter(!is.na(change))

#plot
fig4<-ggplot(combined_changes, aes(x = date, y = change)) +
  geom_bar(data = filter(combined_changes, type != "Total"), aes(fill = type), 
           stat = "identity", position = "dodge", width = 45) +
  geom_line(data = filter(combined_changes, type == "Total"), aes(color = type), size = 1) +
  scale_fill_manual(values = c("Full-Time" = "green", "Part-Time" = "red")) +
  scale_color_manual(values = c("Total" = "blue")) +
  labs(title = "Figure 4: Employment Change",
       subtitle = "Australia − Employment Change",
       x = "",
       y = "Change in Jobs (YoY)") +
  theme_light() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, color = "red", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

print(fig4)


#Grafico 5 ---------------------------------------------------------------------------------------------------------------------

#Join Bases
employment_data <- bind_rows(
  Em_Labour %>% mutate(Type = "Total"),
  FT_Labour %>% mutate(Type = "Full-Time"),
  PT_Labour %>% mutate(Type = "Part-Time")
)

employment_data <- employment_data %>%
  filter(date >= as.Date("2010-01-01"))

#Index
base_values <- employment_data %>%
  filter(format(date, "%Y-%m") == "2010-01") %>%
  group_by(Type) %>%
  summarise(BaseValue = first(value))

employment_indexed <- employment_data %>%
  left_join(base_values, by = "Type") %>%
  mutate(Index = value / BaseValue * 100) %>%
  select(date, Type, Index)

#Plot
fig5<-ggplot(employment_indexed, aes(x = date, y = Index, color = Type)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("Total" = "red", "Full-Time" = "blue", "Part-Time" = "green")) +
  labs(title = "Figure 5: Employment Comparision",
       subtitle = "Employment Comparision: Level − Jan/10 = 100",
       x = "",
       y = "Index") +
  theme_light() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, color = "red", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(fig5)
#Grafico 6 ---------------------------------------------------------------------------------------------------------------------

fig6<-ggplot(au_JV2, aes(x = date, y = value)) +
  geom_line(color = "red", size = 1.2) +  
  geom_point(color = "red", size = 2, alpha = 0.6) +  
  labs(title = "Figure 6: Jobs Vacancies",
       subtitle = "Job Vacancies",
       x = "Date",
       y = "Vacancies in a Thousand") +
  theme_light(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5,face = "bold",color = "red", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_text(face = "bold", color = "darkred"), 
        axis.title.y = element_text(face = "bold", color = "darkgreen")) +  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  
  scale_y_continuous(labels = scales::comma)

print(fig6)

#Grafico 7 ---------------------------------------------------------------------------------------------------------------------

#Index
base_value_employment <- Em_Labour %>%
  filter(format(date, "%Y-%m") == "2007-01") %>%
  summarise(first_value = first(value)) %>%
  pull(first_value)

Em_Labour <- Em_Labour %>%
  mutate(Employment_Index = (value / base_value_employment) * 100)

#YoY
au_JV3 <- au_JV3 %>%
  arrange(date) %>%
  mutate(vacancies_yoy = ((value / lag(value, 4) - 1) * 100)) %>%
  filter(!is.na(vacancies_yoy))

#Date
Em_Labour <- Em_Labour %>%
  filter(date >= as.Date("2010-09-01"))
au_JV3 <- au_JV3 %>%
  filter(date >= as.Date("2010-09-01"))

#Plot
fig7<-ggplot() +
  geom_line(data = Em_Labour, aes(x = date, y = Employment_Index, color = "Employment Index"), size = 1.2) +
  geom_line(data = au_JV3, aes(x = date, y = vacancies_yoy * 0.35 + 115.14, color = "Job Vacancies - YoY"), size = 1.2) +
  scale_color_manual(values = c("Employment Index" = "red", "Job Vacancies - YoY" = "darkblue")) +
  labs(title = "Figure 7: Employment vs. Jobs Vacancies",
       subtitle = "Australia − Job Vacancies and Employment",
       x = "",
       y = "Job Vacancies - YoY (%)") +
  scale_y_continuous(
    name = "Employment Index (Jan/07 = 100)",
    labels = scales::comma,
    limits = c(100, 200), 
    sec.axis = sec_axis(
      ~ (. - 115.14) / 0.35,  
      name = "Job Vacancies - YoY (%)",
      labels = scales::comma
    )
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5,face = "bold",color = "red", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(fig7)

#Grafico 8 ---------------------------------------------------------------------------------------------------------------------

#Covid Change
Ind_jv$date <- as.Date(Ind_jv$date, format = "%Y-%m-%d")

CovidChange_data <- Ind_jv %>%
  filter(date >= as.Date("2020-02-15")) %>%
  arrange(series, date) %>%
  group_by(series) %>%
  summarize(
    ini = first(value), 
    fim = last(value),   
    changevacancies = fim - ini  
  )

CovidChange_data$series <- gsub("Job Vacancies ; ", "", CovidChange_data$series)
CovidChange_data$series <- gsub(" ;", "", CovidChange_data$series)

#plot
fig8<-ggplot(CovidChange_data, aes(x = reorder(series, changevacancies), y = changevacancies)) +
  geom_col(fill = "#E31A1C") + 
  geom_text(aes(label = changevacancies), 
            position = position_stack(vjust = 0.5), 
            angle = 90, 
            hjust = 1, 
            color = "black", 
            size = 3) +  
  labs(title = "Figure 8: Job Vacancy - By Industry (Covid)",
       subtitle = 'Job Vacancy by Industry − Covid Change (Feb/20 to Feb/24)',
       x = "",
       y = "Thousand",
       caption = "") +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5, color = "red",size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 80, hjust = 1),  
        axis.text.y = element_text(size = 5),
        legend.position = "none")

print(fig8)
#Grafico 9 ---------------------------------------------------------------------------------------------------------------------

#Last Quarter
last_two_variation <- Ind_jv %>%
  arrange(series, date) %>%
  group_by(series) %>%
  slice_tail(n = 2) %>%
  summarise(
    second_to_last_value = first(value),
    last_value = last(value),
    variation = last_value - second_to_last_value
  )

#Remove Industry Job Vancancies
last_two_variation$series <- gsub("Job Vacancies ; ", "", last_two_variation$series)
last_two_variation$series <- gsub(" ;", "", last_two_variation$series)

#Plot
fig9<-ggplot(last_two_variation, aes(x = reorder(series, variation), y = variation)) +
  geom_col(aes(fill = variation > 0), show.legend = FALSE) +
  geom_text(aes(label = round(variation, 1)), position = position_dodge(width = 0.9), hjust = -0.2, color = "black", size = 3) +  
  scale_fill_manual(values = c('red', 'red')) +  
  labs(title = "Figure 9: Job Vacancy - By Industry (Quarter)",
       subtitle = "Job Vacancy by Industry − Quarter Change (Nov/23 to Feb/24)",
       x = "",
       y = "(%)",
       caption = "") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5,size = 16,color = 'red', face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 75, hjust = 1),  
        axis.text.y = element_text(size = 5),
        legend.position = "none")
print(fig9)

#Grafico 10 ---------------------------------------------------------------------------------------------------------------------


#Quaterly
Em_Labour_quarterly <- Em_Labour2 %>%
  mutate(date = floor_date(date, "quarter")) %>%  
  group_by(date) %>%
  summarise(total_employment = mean(value, na.rm = TRUE), .groups = 'drop')

UNRT_Labour_quarterly <- UNRT_Labour %>%
  mutate(quarter = floor_date(date, "quarter")) %>%
  group_by(quarter) %>%
  summarise(UnemploymentRate = mean(value, na.rm = TRUE), .groups = 'drop') %>%
  mutate(date = quarter) %>%  
  select(date, UnemploymentRate)

#joining Bases
vacancy_data <- au_JV4 %>%
  mutate(date = floor_date(date, "quarter")) %>%
  left_join(Em_Labour_quarterly, by = "date") %>%
  mutate(JobVacancyRate = ifelse(is.na(total_employment), NA, (value / total_employment) * 100))

beveridge_data <- vacancy_data %>%
  left_join(UNRT_Labour_quarterly, by = "date")

#Date
beveridge_data <- beveridge_data %>%
  mutate(
    Period = case_when(
      date >= as.Date("2001-01-01") & date <= as.Date("2008-12-31") ~ "Jan 2001 - Dec 2008",
      date >= as.Date("2009-01-01") & date <= as.Date("2020-06-30") ~ "Jan 2009 - Jun 2020",
      date >= as.Date("2020-07-01") & date <= as.Date("2024-02-28") ~ "Jul 2020 - Feb 2024",
      date == max(date) ~ "Present"
    )
  ) %>%
  filter(!is.na(Period)) 

#Plot
fig10<-ggplot(beveridge_data, aes(x = UnemploymentRate, y = JobVacancyRate, color = Period)) +
  geom_point(aes(shape = Period), size = 3) + 
  geom_line(aes(group = Period), alpha = 0.5) +  
  scale_color_manual(values = c(
    "Jan 2001 - Dec 2008" = "blue",
    "Jan 2009 - Jun 2020" = "orange",
    "Jul 2020 - Feb 2024" = "green",
    "Present" = "red"
  )) +
  scale_shape_manual(values = c("Jan 2001 - Dec 2008" = 16, "Jan 2009 - Jun 2020" = 17, "Jul 2020 - Feb 2024" = 18, "Present" = 19)) +
  labs(
    title = "Figure 10: Beveridge Curve",
    subtitle = 'Australia, Beveridge Curve',
    x = "Unemployment Rate (%)",
    y = "Job Vacancy Rate (%)",
    caption = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,size = 16,color = 'red', face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(fig10)


#Grafico 11 ---------------------------------------------------------------------------------------------------------------------

#date
beveridge_data2 <- beveridge_data %>%
  filter(date >= as.Date("2010-01-01"))

#Plot
fig11<-ggplot(data = beveridge_data2, aes(x = date)) +
  geom_line(aes(y = UnemploymentRate, color = "Unemployment Rate"), size = 1.2) +
  geom_point(aes(y = JobVacancyRate, color = "Job Vacancy Rate"), shape = 17, size = 3) +  
  scale_y_continuous(
    name = "Unemployment Rate (%)",
    labels = scales::percent_format(scale = 1),  
    limits = c(1, 7), 
    sec.axis = sec_axis(
      ~ .,  
      name = "Job Vacancy Rate (%)",
      labels = scales::percent_format(scale = 1)
    )
  ) +
  scale_color_manual(values = c("Unemployment Rate" = "red", "Job Vacancy Rate" = "black")) +
  labs(
    title = "Figure 11: Unemployment Rate vs Jobs Vacancies",
    subtitle = 'Unemployment vs. Job Vacancy',
    x = "Date",
    y = "Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, color = 'red', face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank()
  )

#Grafico 12-----------------------------------------------------------------------------------------------------------------------

#Match Dates
filtered_unemployment <- UN_Labour %>%
  filter(month(date) %in% c(2, 5, 8, 11)) %>%
  select(date, unemployed = value)

au_JV3_V <- au_JV3 %>%
  select(date, vacancies = value)

filtered_unemployment <-filtered_unemployment %>%
  filter(date >= as.Date("2010-01-01"))

au_JV3_V <- au_JV3_V %>%
  filter(date >= as.Date("2010-01-01"))

filtered_unemployment <- filtered_unemployment %>%
  mutate(year_month = floor_date(date, unit = "month"))

au_JV3_V <- au_JV3_V %>%
  mutate(year_month = floor_date(date, unit = "month"))

UNRT_Labour_adj <- UNRT_Labour %>%
  mutate(year_month = floor_date(date, unit = "month")) %>%
  select(year_month, unemployment_rate = value) %>%
  filter(year_month >= as.Date("2010-01-01"))


#Vancancy to Unemployed
Vacancy <- left_join(filtered_unemployment, au_JV3_V, by = "year_month")

Vacancy <- Vacancy %>%
  mutate(vacancy_to_unemployment_rate = (vacancies / unemployed))

Vacancy_combined <- left_join(Vacancy, UNRT_Labour_adj, by = "year_month")


#Plot
fig12<-ggplot(Vacancy_combined, aes(x = year_month)) +
  geom_line(aes(y = unemployment_rate, color = "Unemployment Rate"), size = 1.2) +
  geom_point(aes(y = vacancy_to_unemployment_rate * 6 + 1, color = "Vacancy to Unemployment Rate"), shape = 17, size = 3) +
  scale_y_continuous(
    name = "Unemployment Rate (%)",
    limits = c(0, max(Vacancy_combined$unemployment_rate, Vacancy_combined$vacancy_to_unemployment_rate * 6 + 1)),  
    sec.axis = sec_axis(~ . * 6 + 1, name = "Vacancy to Unemployment Rate (%)") 
  ) +
  scale_color_manual(values = c("Unemployment Rate" = "red", "Vacancy to Unemployment Rate" = "purple")) +
  labs(
    title = "Figure 12: Unemployment Rate vs. Vacancies to Unemployment",
    subtitle = 'UR vs. Vacancies to Unemployment',
    x = "",
    y = "Rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust=0.5,color='red',size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

print(fig12)

#Grafico13------------------------------------------------------------------------------------

WPI_YoY <- WPI_Index %>%
  mutate(value = YoY) %>%  
  select(date, value) %>%
  mutate(Metric = "WPI YoY Growth")

#Join Base
data_for_plot <- WPI_YoY %>%
  rename(WPI_YoY = value) %>%
  inner_join(UNRT_Labour %>% rename(Unemployment = value), by = "date")

fig13<-ggplot(data_for_plot, aes(x = Unemployment, y = WPI_YoY)) +
  geom_point(color = "red") +  
  geom_smooth(method = "lm",   #
              se = TRUE,       
              color = "blue",  
              fill = "gray") + 
  labs(
    title = "Figure 13: WPI vs. Unemployment - Since 2000",
    subtitle = 'y=0.4823 - 0.325x',
    x = "Unemployment (%)",
    y = "WPI - Excluding Bonus (YoY)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5,color = 'red',size = 16),
    plot.subtitle = element_text(hjust = 0.5,size = 12)
  )

model <- lm(WPI_YoY ~ Unemployment, data = data_for_plot)

print(fig13)


#Grafico14-----------------------------------------------------------------------------------

#join DAta
data_for_plot2 <- WPI_YoY %>%
  rename(WPI_YoY = value) %>%
  inner_join(UTRT_Labour %>% rename(Underutilisation = value), by = "date")

#LM
model2 <- lm(WPI_YoY ~ Underutilisation, data = data_for_plot2)
intercept2 <- coef(model)[1]
slope2 <- coef(model)[2]
line_equation <- sprintf("y = %.4f + %.4fx", intercept2, slope2)


#plot
fig14 <- ggplot(data_for_plot2, aes(x = Underutilisation, y = WPI_YoY)) +
  geom_point(color = "red") +  
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "gray") +  
  annotate("text", x = max(data_for_plot2$Underutilisation), y = min(data_for_plot2$WPI_YoY), label = line_equation, hjust = 1.1, vjust = -1, size = 5, color = "blue") +
  labs(
    title = "Figure 14: WPI vs. Underutilisation - Since 2000",
    subtitle = "y=7.265 -0.329x",
    x = "Underutilisation (%)",
    y = "WPI - Excluding Bonus (YoY)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5,color = 'red',size = 16),
    plot.subtitle = element_text(hjust = 0.5,size = 12)
  )


print(fig14)
#Grafico15-------------------------------------------------------------------------------------

#Job VacancyRate

Em_Labour4 <- Em_Labour %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  group_by(date) %>%
  summarise(Total_Employed = sum(value, na.rm = TRUE))

au_JV5 <- au_JV5 %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  rowwise() %>%
  mutate(closest_date = find_closest_date(date, Em_Labour4$date))

vacancy_rate_data <- au_JV5 %>%
  left_join(Em_Labour4, by = c("closest_date" = "date")) %>%
  mutate(
    Job_Vacancy_Rate = (value / (Total_Employed + value)) * 100
  )

JobVacancyRate <- vacancy_rate_data

JobVacancyRate <- JobVacancyRate %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  rowwise() %>%
  mutate(closest_date = find_closest_date(date, WPI_YoY$date))

data_for_plot3 <- WPI_YoY %>%
  rename(WPI_YoY = value) %>%
  inner_join(JobVacancyRate %>% select(date, closest_date, Job_Vacancy_Rate), by = c("date" = "closest_date"))

data_for_plot3<-data_for_plot3 %>%
  filter(date >= as.Date("2000-01-01"))


clean_data <- data_for_plot3 %>%
  filter(!is.na(Job_Vacancy_Rate) & !is.na(WPI_YoY))

fig15 <- ggplot(clean_data, aes(x = Job_Vacancy_Rate, y = WPI_YoY)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "gray") +
  labs(
    title = "Figure 15: WPI vs. Job Vacancy Rate - Since 2000",
    subtitle = "",
    x = "Job Vacancy Rate (%)",
    y = "WPI - Excluding Bonus (YoY)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, color='red'),
    plot.subtitle = element_text(hjust = 0.5)
  )
print(fig15)







#Grafico16------------------------------------------------------------------------------

HoursWork<-read_abs('6202.0',table=19)

HoursWork<-HoursWork %>%
  filter(series=='Monthly hours worked in all jobs ;  Persons ;',
         series_type=='Seasonally Adjusted')



#pdf-----------------------------------------------




pdf(diretorio, width = 15, height = 10)

print(fig1)
print(fig2)
print(fig3)
print(fig4)
print(fig5)
print(fig6)
print(fig7)
print(fig8)
print(fig9)
print(fig10)
print(fig11)
print(fig12)
print(fig13)
print(fig14)
print(fig15)

dev.off()

pdf(diretorio2,width = 20, height = 20)

grid.arrange(tableGrob(combined_datatableSA), tableGrob(combined_datatableNSA),ncol = 1)

dev.off()

