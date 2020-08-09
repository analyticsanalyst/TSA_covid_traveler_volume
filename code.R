### disable scientific notation
options(scipen=999) 

### load and/or install required packages
required_packages <- c('tidyverse','rvest', 'readr', 
                       'lubridate', 'ggrepel', 'ggthemes')

for(p in required_packages){
      if(!require(p,character.only = TRUE)) install.packages(p)
      library(p,character.only = TRUE)
}

### get link to webpage to scrape
### permalinks are recommended in case web changes
url <- "https://www.tsa.gov/coronavirus/passenger-throughput"

### use the CSS selector for the table with data we want to explore
css_selector <- "#block-mainpagecontent > article > div > div > table"

### another option to read in table data
# tab <- read_html(url) %>% html_nodes("table")
# tab[[1]] %>% html_table(fill=TRUE)

raw_table <- read_html(url) %>%
      html_node(css = css_selector) %>% 
      html_table(fill=TRUE) %>%
      as_tibble() %>%
      # clean up column names
      rename(Date = X1,
             Traveler_Throughput_2020 = X2,
             Traveler_Throughput_2019_Same_Weekday = X3) %>%
      # filter out rows without clean data
      filter(!str_detect(Date, "Date") & !str_detect(Date, "NA"))

clean_table <- raw_table %>%
      # convert throughput columns to numeric data vs strings
      mutate_at(2:3,parse_number) %>%
      # convert date string to date time object
      mutate(Date = parse_date(Date, "%m/%d/%Y"))

### check that we don't have gaps in dates in the clean table
nrow(clean_table) == length(
      # creates a comparison sequence of dates using clean table
      seq(min(clean_table$Date), 
          max(clean_table$Date), 
          by = 1)
      )

pct_formater <- scales::label_percent(accuracy = 1)

### prep tibble for plotting
clean_table_2 <- clean_table %>%
      # derive % change YoY
      mutate(percent_change = (Traveler_Throughput_2020 - 
                                     Traveler_Throughput_2019_Same_Weekday) /
                   (Traveler_Throughput_2019_Same_Weekday),
             percent_change = pct_formater(percent_change)) %>%
      # get the data in tidy format 
      gather(key="metric", value="value", -Date, -percent_change) %>%
      # add wkd variable & include chart label var for geom_text_repel
      mutate(wkd_var = wday(Date, label=TRUE),
            chart_label = ifelse(metric=="Traveler_Throughput_2020" & 
                                       wkd_var=="Fri", 
                                  percent_change, NA))

### create vline df to plot vertical Friday lines for reference
vline_df <- clean_table_2 %>% filter(wkd_var=="Fri")

### generate plot
clean_table_2 %>%
      mutate(metric = factor(metric, 
                             levels=c("Traveler_Throughput_2020",
                                      "Traveler_Throughput_2019_Same_Weekday"))) %>%
      ggplot(aes(x=Date, y=value/1000000, color=metric, group=metric)) +
      geom_vline(xintercept = vline_df$Date, linetype="dashed", alpha=0.25) +
      geom_line() +
      geom_text_repel(aes(label=chart_label), 
                      color="grey40",
                      direction = "y",
                      fontface = "bold",
                      segment.color = NA,
                      vjust=5,
                      size=3.5) +
      scale_y_continuous(breaks=seq(0,4,by=0.5)) +
      scale_color_manual(values = c("salmon","dodgerblue")) +
      labs(title="2020 TSA Traveler Volume vs 2019 TSA Traveler Volume",
           subtitle="Traveler volume bottomed out in April. 
Slow recovery moving into summer months.
Dashed vertical lines = Fridays.
% labels = YoY percent change.",
           y="Traveler\nThroughput\n(millions)",
           x="Daily Trend",
           color="",
           caption = "
Data source: tsa.gov/coronavirus/passenger-throughput") +
      theme_classic() +
      theme(legend.position='top', 
            legend.justification='left',
            legend.direction='horizontal',
            panel.grid.major.x = element_blank(),
            axis.title.y = element_text(size=12, angle=0, vjust=0.5))
