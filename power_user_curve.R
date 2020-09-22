library(tidyverse)
library(dplyr)
library(bigrquery)
library(ggplot2)
library(ggthemes)

query <- "
SELECT num_active_days, COUNT(user_pseudo_id) AS count
FROM(
  SELECT user_pseudo_id, count(distinct event_date) as num_active_days
  FROM `analytics_196410282.events_*` 
  WHERE _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE_SUB(CURRENT_DATE('Asia/Seoul'), INTERVAL 28 DAY)) AND FORMAT_DATE('%Y%m%d', DATE_SUB(CURRENT_DATE('Asia/Seoul'), INTERVAL 1 DAY))
  GROUP BY 1
)
GROUP BY 1
ORDER BY 1
"

query %>%
  bq_project_query('neo-smart-gcm', .) %>%
  bq_table_download(start_index = 0) -> data

data %>% 
  mutate(users = round(count/10000, 1)) %>% 
  ggplot(aes(x = num_active_days, y= users)) +
  geom_bar(stat = "identity", fill = 'darkgray') +
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25)) +
  ggtitle('The Power User Curve : KIS MTS') +
  ylab('유저수, 만') + xlab('방문일자') +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        panel.background = element_rect(fill = 'white', colour = alpha('gray', 0.7)),
        panel.grid.major = element_line(color = alpha('gray', 0.3)))

library(plotly)
data %>% 
  mutate(
    users = round(count/10000, 1),
    rto   = paste0(round(users/sum(users), 2) * 100, '%')
  ) %>% 
  plot_ly(
    x = .$num_active_days,
    y = .$users,
    text = paste0(.$users, "(", .$rto, ")"),
    hoverinfo = 'text',
    type = 'bar',
    marker = list(color = alpha('gray', 0.5),
                  line = list(color = alpha('gray', 0.8),
                              width = 1.5))
  ) %>%
  layout(title = "The Power User Curve : KIS MTS",
         xaxis = list(title = "방문일자"),
         yaxis = list(title = "유저수(만)"))
