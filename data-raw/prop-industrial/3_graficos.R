library(tidyverse)

da <- read_rds("/mnt/dados/abj/levantamentos/prop_industrial/prop_industrial.rds")

p_tempo <- da %>% 
  # corrige datas de distribuição (originalmente mais recentes que decisão)
  mutate(data_distribuicao = case_when(
    n_processo == "10038382720168260016" ~ lubridate::dmy("04/04/2016"),
    n_processo == "10040922920188260016" ~ lubridate::dmy("11/04/2018"),
    n_processo == "10125125720178260016" ~ lubridate::dmy("03/10/2017"),
    n_processo == "10471867420158260002" ~ lubridate::dmy("15/10/2015"),
    n_processo == "20049943320168260016" ~ lubridate::dmy("11/10/2016"),
    n_processo == "00046327820098260091" ~ lubridate::dmy("13/08/2009"),
    TRUE ~ data_distribuicao
  )) %>% 
  mutate(tempo_dist_sentenca = data_sentenca - data_distribuicao,
         ano_distribuicao = lubridate::year(data_distribuicao)) %>% 
  filter(tempo_dist_sentenca >= 0) %>% 
  group_by(assunto) %>% 
  summarise(tempo_mediano = median(tempo_dist_sentenca)) %>% 
  mutate(assunto = fct_reorder(stringr::str_wrap(assunto, 12), tempo_mediano)) %>% 
  ggplot(aes(x = tempo_mediano, y = assunto)) +
  geom_col(fill = viridis::viridis(1, begin = 0.2)) +
  labs(title = "Tempo mediano entre distribuição e decisão por assunto", x = "", y = "") +
  theme_minimal()

ggsave("/mnt/dados/abj/levantamentos/prop_industrial/p_tempo.png", p_tempo)


da %>% 
  count(assunto, sort = TRUE) %>% 
  mutate(pct = n/sum(n))


