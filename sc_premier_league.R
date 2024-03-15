#---------------------------------------------------------------------------------
#           PROBABILIDADES DE GANAR LA PREMIER LEAGUE 
#
# Autor: César Anderson Huamaní Ninahuanca.
#---------------------------------------------------------------------------------

library(dplyr)
library(rvest)
#devtools::install_github("CesarAHN/datametria")
library(datametria)
library(ggplot2)
library(tidyr)
library(gt)
#devtools::install_github("jthomasmock/gtExtras")
library(gtExtras)
library(ggridges)

#----------------------
# Tabla de posiciones. 
#----------------------
pw<-read_html("https://www.skysports.com/premier-league-table")

tab_pos<-pw %>% html_elements("table.standing-table__table") %>% html_table() %>% as.data.frame() %>% as_tibble()
tab_pos$Team<-gsub(" \\*","",tab_pos$Team)
tab_pos$Last.6<-NULL
tab_pos$X.<-NULL
names(tab_pos)<-c("CLUB","J","G","E","P","GF","GC","DIF","PTS")

tab_pos %>% as_tibble() %>% gt() %>%
  gt_theme_espn() %>% tab_header(title = "TABLA DE POSICIONES PREMIER LEAGUE 2023 - 2024",
                                 subtitle = paste0("Actualizado al ", Sys.Date())) %>% 
         tab_source_note("ELABORACIÓN: https://github.com/CesarAHN") %>% 
  gtsave(r"(C:\OTROS\premier-league-23-24-pronosticos\plots\tab_pos.png)")
         

#-------------
# Calendario.
#-------------
pw<-read_html("https://www.skysports.com/premier-league-fixtures")

fechas<-pw %>% html_elements("span.swap-text__target") %>% html_text2()

fechas<-fechas[-1]
fechas[fechas!=""]->fechas

tab_calendario<-data.frame(LOCAL=fechas[seq(1,length(fechas),by=2)],
                           VISITANTE=fechas[seq(2,length(fechas),by=2)])

# Temporal. 
# falta el partido del Chelsea vs el Tottenham

if (tab_calendario %>% filter(LOCAL=="Chelsea" & VISITANTE=="Tottenham Hotspur") %>% nrow()==0){
  tab_calendario %>% bind_rows(data.frame(LOCAL="Chelsea",VISITANTE="Tottenham Hotspur"))->tab_calendario
}

#-------------------------------------------------------------------------------

#-----------------
# Los lesionados. 
#-----------------
# Se sacará de trnsfermarkt.
# pw<-read_html("https://www.premierinjuries.com/injury-table.php")
# 
# pw %>% html_elements("table.injury-table.injury-table-full") %>% html_table() %>% as.data.frame()





#------------------------
# Puntajes de desempeño.
#------------------------
# Puntaje de juego de local
# De 1 al 10, donde 1 es que le va muy mal jugando de visita y 10 que le va muy bien.
p_local<-tibble(CLUB=sort(tab_pos$CLUB), `PUNTAJE LOCAL`=c(9.6,8,4,4,6,3,7,5,5,5,9.5,3,9.8,7.5,6.5,4.5,2,7.5,6,5.5)/10)

# Puntaje de juego de visita 
# De 1 al 10, donde 1 es que le va muy mal jugando de local y 10 que le va muy bien.
p_visita<-tibble(CLUB=sort(tab_pos$CLUB), `PUNTAJE VISITA`=c(9.2,7,3.5,4,5.5,2,6.5,5,4.5,4.5,9.2,2.8,9.5,7,6,4.2,2,7,5.5,5)/10)

pp<-plyr::join_all(list(p_local,p_visita), by="CLUB", type = "inner") %>% as_tibble()

pp %>% as_tibble() %>% gt() %>%
  gt_theme_espn() %>% 
  tab_header(title = "ÍNDICE DE DESEMPEÑO DE LOCAL Y VISITANTE PARA CADA UNO DE LOS CLUBES", subtitle = "Puntajes según juicios de expertos") %>% 
  gtsave(r"(C:\OTROS\premier-league-23-24-pronosticos\plots\desempeño_clubes.png)")

#---------------------------------------------------------------------------------------
# creando la Función que simule los resultados de los partidos. 

simulador_partidos<-function(tab_pos,pp,tab_calendario){
  #---------------------
# Partidos por jugar.
  resultados<-data.frame()
  for (i in 1:nrow(tab_calendario)) {
    d<-(1-(pp[pp$CLUB==tab_calendario[i,1],2]-pp[pp$CLUB==tab_calendario[i,2],3])) %>% pull()
    if(d<1){
      a1<-rbind(data.frame(CLUB=tab_calendario[i,1], GF_=sample(0:4,1, prob = c(1*d/10,2*d/10,3*d/10,4*d/10,(1-d)))),
                data.frame(CLUB=tab_calendario[i,2], GF_=sample(0:4,1, prob = c((1-d),5*d/14,4*d/14,3*d/14,2*d/14)))) 
    } else {
      d<-(1-(pp[pp$CLUB==tab_calendario[i,2],2]-pp[pp$CLUB==tab_calendario[i,1],3])) %>% pull()
      a1<-rbind(data.frame(CLUB=tab_calendario[i,2], GF_=sample(0:4,1, prob = c(1*d/10,2*d/10,3*d/10,4*d/10,(1-d)))),
                data.frame(CLUB=tab_calendario[i,1], GF_=sample(0:4,1, prob = c((1-d),5*d/14,4*d/14,3*d/14,2*d/14)))) 
    }
    a1$GC_<-rev(a1$GF_)
    a1$PTS_<-ifelse(a1$GF_>a1$GC_,3,
                    ifelse(a1$GF_==a1$GC_,1,0))
    a1$G_<-ifelse(a1$PTS_==3,1,0)
    a1$P_<-rev(a1$G_)
    a1$E_<-ifelse(a1$PTS_==1,1,0) 
    resultados<-rbind(resultados,a1)
  }
  resultados %>% group_by(CLUB) %>% mutate(n=1) %>% summarise_all(~sum(.))->resultados
  
  tab_pos_final<-left_join(tab_pos,resultados, by="CLUB")

  tab_pos_final<-tab_pos_final %>% mutate(J=J+n, G=G+G_, E=E+E_, P=P+P_, GF=GF+GF_, GC=GC+GC_,
                                  PTS=PTS+PTS_, DIF=GF-GC) %>% select(!matches("_|^n$")) %>% arrange(-PTS,-DIF,-G) %>% 
    mutate(PUESTO=1:n())

  return(tab_pos_final)
}

simulador_partidos(tab_pos,pp,tab_calendario)

# Para tomar menos tiempo - Código eficiente.
# asignando el número de repeticiones.
repeticiones<-10000
resul<-data.frame(CLUB=vector("character",length = repeticiones*20),
                  PTS=vector("numeric",length = repeticiones*20),
                  PUESTO=vector("integer",length = repeticiones*20))

# Corriendo el número de repeticiones.
for (i in 1:repeticiones) {
  resul[(20*(i-1)+1):(20*(i-1)+20),]<-simulador_partidos(tab_pos,pp,tab_calendario)[,c(1,9,10)]
}


saveRDS(resul, "simulaciones.rds")
resul<-readRDS("simulaciones.rds")

# Save result

resul %>% arrange(CLUB) %>% 
  ggplot(aes(x=PTS))+
  geom_density(fill="sienna2", alpha=.7)+
  facet_wrap(~CLUB, scales = "free")+
  scale_x_continuous(breaks = seq(0,100,by=10), limits = c(0,110))+
  labs(title = "DISTRIBUCIÓN DE LOS POSIBLES PUNTAJES POR CLUBES",
       subtitle = "Premier league 2023 - 2024.", x="Puntos", y="Frecuencia",
       caption = "Resultados luego de 10,000 repeticiones.\nELABORACIÓN: https://github.com/CesarAHN")+
  theme_bw()+
  theme(plot.caption = element_text(face = "bold", size = 8),
        plot.title = element_text(face = "bold"))

resul %>% arrange(CLUB) %>% filter(grepl(paste0(tab_pos$CLUB[1:6], collapse = "|"),CLUB)) %>% 
  mutate(CLUB=factor(CLUB,levels=rev(tab_pos$CLUB[1:6]))) %>% 
  ggplot(aes(x=PTS, fill=CLUB, y=CLUB))+
  geom_density_ridges(alpha=.6)+
  scale_fill_brewer(palette = "Set1")+
  scale_x_continuous(breaks = seq(0,100,by=5), limits = c(50,100))+
  labs(title = "DISTRIBUCIÓN DE LOS POSIBLES PUNTAJES DE LOS 6 PRIMEROS EQUIPOS DE LA LIGA",
       subtitle = "Premier league 2023 - 2024.", x="Puntos", y="Clubes",
       caption = "Resultados luego de 10,000 repeticiones.\nELABORACIÓN: https://github.com/CesarAHN")+
  theme_bw()+
  theme(plot.caption = element_text(face = "bold", size = 8),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom")

#----
# Puesto.
resul %>% arrange(CLUB) %>% 
  ggplot(aes(x=PUESTO))+
  geom_histogram(fill="skyblue3", alpha=.7)+
  facet_wrap(~CLUB, scales = "free_x")+
  scale_x_continuous(breaks = seq(1,20,by=1))+
  labs(title = "DISTRIBUCIÓN DE LOS POSIBLES PUESTOS POR CLUBES",
       subtitle = "Premier league 2023 - 2024.", x="Puestos", y="Frecuencia",
       caption = "Resultados luego de 10,000 repeticiones.\nELABORACIÓN: https://github.com/CesarAHN")+
  theme_bw()+
  theme(plot.caption = element_text(face = "bold", size = 8),
        plot.title = element_text(face = "bold"))

#----

# Probabilidad por puesto.
logos<-data.frame(CLUB=sort(unique(tab_pos$CLUB)),
                     LOGO=c("https://logodownload.org/wp-content/uploads/2017/02/Arsenal-logo-escudo-shield-6.png", # Arsenal
                            "https://logodownload.org/wp-content/uploads/2019/10/aston-villa-logo-4.png", # Aston Villa.
                            "https://logodownload.org/wp-content/uploads/2019/10/bournemouth-fc-logo-4.png", # Bournemouth
                            "https://logodownload.org/wp-content/uploads/2022/09/brentford-fc-logo-4.png", # Brentford
                            "https://logodownload.org/wp-content/uploads/2019/10/brighton-hove-albion-logo-4.png", # Brighton and Hove Albion
                            "https://logodownload.org/wp-content/uploads/2019/10/burnley-fc-logo-2.png", # Burnley
                            "https://logodownload.org/wp-content/uploads/2017/02/chelsea-fc-logo-4.png", # Chelsea
                            "https://logodownload.org/wp-content/uploads/2019/05/crystal-palace-logo-6.png", # Crystal Palace
                            "https://logodownload.org/wp-content/uploads/2019/04/everton-logo-escudo-5.png", # Everton
                            "https://logodownload.org/wp-content/uploads/2022/09/fulham-fc-logo-4.png", # Fulham
                            "https://logodownload.org/wp-content/uploads/2017/02/liverpool-fc-logo-escudo-8.png", # Liverpool
                            "https://logodownload.org/wp-content/uploads/2023/12/luton-town-fc-logo-3.png", # Luton Town
                            "https://logodownload.org/wp-content/uploads/2017/02/manchester-city-fc-logo-escudo-badge-5.png", # Manchester City
                            "https://logodownload.org/wp-content/uploads/2016/10/Manchester-United-logo-escudo-6.png", # Manchester United
                            "https://logodownload.org/wp-content/uploads/2019/10/newcastle-united-logo-4.png", # Newcastle United
                            "https://cdn.worldvectorlogo.com/logos/nottingham-forest-fc.svg", # Nottingham Forest
                            "https://logodownload.org/wp-content/uploads/2019/10/sheffield-united-logo-4.png", # Sheffield United
                            "https://logodownload.org/wp-content/uploads/2018/11/tottenham-logo-escudo-6.png", # Tottenham Hotspur
                            "https://logodownload.org/wp-content/uploads/2019/05/west-ham-united-logo-5.png", # West Ham United
                            "https://logodownload.org/wp-content/uploads/2019/04/wolverhampton-logo-escudo-5.png")) # Wolverhampton Wanderers
resul<-left_join(resul, logos)

resul %>% group_by(LOGO) %>% count(PUESTO) %>% mutate(p=n/10000) %>% select(-n) %>% 
  mutate(PUESTO=paste0("PUESTO ",PUESTO)) %>% spread(PUESTO,p) %>% 
  select(LOGO,paste0("PUESTO ",1:20)) %>% as_tibble() %>% gt() %>% 
  fmt_percent(columns = matches("^PUES")) %>% fmt_missing(columns = matches("^PUES"), missing_text = "-") %>% 
  tab_header(title = "PROBABLIDADES POR PUESTO AL CULMINAR LA PREMIER LEAGUE.",
             subtitle = "Premier league 2023 - 2024.") %>% 
  gt_theme_538(quiet = TRUE) %>% gt_img_rows(columns = LOGO, height = 20) %>% 
  tab_source_note("ELABORACIÓN: https://github.com/CesarAHN") %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(paste0("PUESTO ",1:20))))) %>% 
  gt_color_rows(`PUESTO 1`:`PUESTO 20`, palette = "RColorBrewer::RdBu") %>% 
  gtsave(r"(C:\OTROS\premier-league-23-24-pronosticos\plots\probabilidades_clubes.png)", vwidth = 2000, vheight = 900)

# Probabilidad de ganar la premier league.
resul %>% mutate(clasificacion=case_when(PUESTO<=1~"SI",
                                         TRUE~"NO")) %>% group_by(LOGO) %>% 
  count(clasificacion) %>% mutate(p=n/10000) %>% select(-n) %>% 
  spread(clasificacion,p) %>% arrange(-SI) %>% as_tibble() %>% filter(!is.na(SI)) %>% gt() %>% 
  fmt_percent(column = c(NO,SI)) %>% # fmt_missing(columns = c(NO,SI), missing_text = "") %>% 
  tab_header(title = "PROBABLIDADES DE GANAR LA PREMIER LEAGUE.",
             subtitle = "Premier league 2023 - 2024.") %>% 
  gt_theme_538(quiet = TRUE) %>% gt_img_rows(columns = LOGO, height = 20) %>% 
  tab_source_note("\nELABORACIÓN: https://github.com/CesarAHN") %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(NO,SI)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(NO,SI)))) %>% 
  gt_color_rows(NO:SI, palette = "RColorBrewer::RdBu") %>% 
  gtsave(r"(C:\OTROS\premier-league-23-24-pronosticos\plots\probabilidades_ganar_premier.png)", vwidth = 300, vheight = 900)

# Probabilidad de clasificar a champions.
resul %>% mutate(clasificacion=case_when(PUESTO<=4~"SI",
                                         TRUE~"NO")) %>% group_by(LOGO) %>% 
  count(clasificacion) %>% mutate(p=n/10000) %>% select(-n) %>% 
  spread(clasificacion,p) %>% arrange(-SI) %>% as_tibble() %>% filter(!is.na(SI)) %>% gt() %>% 
  fmt_percent(column = c(NO,SI)) %>% # fmt_missing(columns = c(NO,SI), missing_text = "") %>% 
  tab_header(title = "PROBABLIDADES DE CLASIFICAR A CHAMPIONS LEAGUE",
             subtitle = "Premier league 2023 - 2024.") %>% 
  gt_theme_538(quiet = TRUE) %>% gt_img_rows(columns = LOGO, height = 20) %>% 
  tab_source_note("En el supuesto de que solo se asigne 4 cupos.\nELABORACIÓN: https://github.com/CesarAHN") %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(NO,SI)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(NO,SI)))) %>% 
  gt_color_rows(NO:SI, palette = "RColorBrewer::RdBu") %>% 
  gtsave(r"(C:\OTROS\premier-league-23-24-pronosticos\plots\probabilidades_clasificar_champions.png)", vwidth = 300, vheight = 1000)

# Probabilidad de descenso
resul %>% mutate(clasificacion=case_when(PUESTO>=18~"SI",
                                         TRUE~"NO")) %>% group_by(LOGO) %>% 
  count(clasificacion) %>% mutate(p=n/10000) %>% select(-n) %>% 
  spread(clasificacion,p) %>% arrange(-SI) %>% as_tibble() %>% filter(!is.na(SI)) %>% gt() %>% 
  fmt_percent(column = c(NO,SI)) %>% # fmt_missing(columns = c(NO,SI), missing_text = "") %>% 
  tab_header(title = "PROBABLIDADES DE DESCENDER EN LA PREMIER LEAGUE.",
             subtitle = "Premier league 2023 - 2024.") %>% 
  gt_theme_538(quiet = TRUE) %>% gt_img_rows(columns = LOGO, height = 20) %>% 
  tab_source_note("Descienden los 3 últimos puestos.\nELABORACIÓN: https://github.com/CesarAHN") %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(NO,SI)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(NO,SI)))) %>% 
  gt_color_rows(NO:SI, palette = "RColorBrewer::RdBu") %>% 
  gtsave(r"(C:\OTROS\premier-league-23-24-pronosticos\plots\probabilidades_descenso.png)", vwidth = 300, vheight = 900)
