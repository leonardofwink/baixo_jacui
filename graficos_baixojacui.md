---
title: "Gráficos Baixo Jacuí 2018-2020"
author: "Leonardo Fernandes Wink"
date: "08/09/2022"
output:
  html_document: 
    highlight: haddock
    keep_md: yes
    number_sections: yes
    theme: flatly
    toc: yes
    toc_float:
      collapsed: no
      smooth_scroll: no
    fig_width: 10
    fig_height: 6.66
    fig_caption: yes
    code_download: true
  pdf_document:
    toc: yes
  word_document: 
    toc: yes
    keep_md: yes
always_allow_html: yes
editor_options: 
  chunk_output_type: console
fig.align: center
---
# Importando os pacotes

```r
library(readr)
library(rmarkdown)
library(qboxplot)
library(readxl)
library(pillar)
library(dplyr)
library(tidyverse)
library(gapminder)
library(knitr)
library(kableExtra)
library(ggpubr)
library(gridExtra)
library(modelsummary)
library(gtsummary)
```

# Importando as planilhas

```r
BJ_od20182020<-read.csv("C:/Users/Léo/Desktop/coisas da bolsaa/RHG/G070_Baixo_Jacui/graficos-boxplot/Dados_Jacui - bj_od.csv",header=T)
BJ_dbo20182020<-read.csv("C:/Users/Léo/Desktop/coisas da bolsaa/RHG/G070_Baixo_Jacui/graficos-boxplot/Dados_Jacui - bj_dbo.csv",header=T)
BJ_ptot20182020<-read.csv("C:/Users/Léo/Desktop/coisas da bolsaa/RHG/G070_Baixo_Jacui/graficos-boxplot/Dados_Jacui - bj_ptot.csv",header=T)
BJ_namon20182020<-read.csv("C:/Users/Léo/Desktop/coisas da bolsaa/RHG/G070_Baixo_Jacui/graficos-boxplot/Dados_Jacui - bj_namon.csv",header=T)
BJ_ecoli20182020<-read.csv("C:/Users/Léo/Desktop/coisas da bolsaa/RHG/G070_Baixo_Jacui/graficos-boxplot/Dados_Jacui - bj_ecoli.csv",header=T)
BJ_turb20182020<-read.csv("C:/Users/Léo/Desktop/coisas da bolsaa/RHG/G070_Baixo_Jacui/graficos-boxplot/Dados_Jacui - bj_turb.csv",header=T)
```


```r
bj_rsagua <- read_excel("bj_rsagua.xls", 
    sheet = "Dados_Ajustados", col_types = c("numeric", 
        "text", "numeric", "numeric", "text", 
        "text", "text", "text", "text", "date", 
        "date", "text", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric"),
		range = "A1:AY44")
```

# Parâmetros físico químicos
## Oxigênio Dissolvido

```r
qboxplot(BJ_od20182020,
         main="Oxigênio Dissolvido",
         cex.main=2,
         probs=c(0.20,0.5,0.8),
         ylab="mg/L",
         las=0,
         ylim=c(0,15),
         col=c(8),
         cex.lab = 2, 
         cex.axis = 1.2 ,
         whisklwd=2.5,
         outpch="o" )
abline(h=6,col=4,lwd=2)
abline(h=5,col=3,lwd=2)
abline(h=4,col=7,lwd=2)
abline(h=2,col=2,lwd=2)
legend("topleft",
       title="Legenda",
       legend=c("Classe 1","Classe 2","Classe 3","Classe 4","Pior Classe"),
       lty=c(1,1,1,1,1),
       col=c(4,3,7,2,"#AC5079"),
       lwd=c(10,10,10,10,10),
       cex = 1.75,
       pt.cex = 1.75,
       bg="white")
legend("topright",
       legend=c("2018-2020"),
       pch = 22,
       pt.bg="gray",
       title="Período",
       cex = 2,
       pt.cex = 2,
       bg="white")
```

![](graficos_baixojacui_files/figure-html/versao1-1.png)<!-- -->


```r
(od_bj<-ggplot(bj_rsagua,
                       aes(`CÓD. ESTAÇÃO`,
                           `OXIGÊNIO DISSOLVIDO`,
                           fill=`OXIGÊNIO DISSOLVIDO`))+
   annotate("rect",
            xmin=-Inf,
            xmax=Inf,
            ymin=-Inf,
            ymax=2.0,
            alpha=1,
            fill="#ac5079")+ #>pior classe
   annotate("rect",
            xmin=-Inf,
            xmax=Inf,
            ymin=2,
            ymax=4,
            alpha=1,
            fill="#eb5661")+ #classe 4
   annotate("rect",
            xmin=-Inf,
            xmax=Inf,
            ymin=4,
            ymax=5,
            alpha=1,
            fill="#fcf7ab")+ #classe 3
   annotate("rect",
            xmin=-Inf,
            xmax=Inf,
            ymin=5,
            ymax=6,
            alpha=1,
            fill="#70c18c")+ #classe 2
   annotate("rect",
            xmin=-Inf,
            xmax=Inf,
            ymin=6,
            ymax=Inf,
            alpha=1,
            fill="#8dcdeb")+ #classe 1
   stat_boxplot(geom = 'errorbar',
                width=0.3,
                position = position_dodge(width = 0.65))+
   geom_boxplot(fill='#F8F8FF',
                color="black",
                outlier.shape = 1, #se deixar NA fica só o jitter, se não, deixa 1
                width= 0.7)+
   labs(title = "Oxigênio Dissolvido no período 2018-2020",
        x="Estação",
        y="mg/L")+
   # geom_jitter(width = .05,
   #             alpha=.2,
   #             size=1.5,
   #             color="black")+
   # scale_y_continuous(expand = expansion(mult = c(0,0)),
   #                    n.breaks = 11,
   #                    limits = c(-1,21))+
   scale_y_continuous(expand = expansion(mult = c(0.03,0.17)),
                      n.breaks = 8,
                      limits = c(min(0, na.rm = TRUE),
                                 max(bj_rsagua$`OXIGÊNIO DISSOLVIDO`), na.rm = TRUE))+
   # scale_x_discrete(limits = c("A", "B", "C", "D", "E", "F", "G", "H", "I"))+
   # geom_smooth(method = "lm",
   #             se=FALSE, #se deixar TRUE gera o intervalo de confiança de 95%
   #             aes(group=1),
   #             alpha=.5,
   #             na.rm = TRUE,
   #             size = 1)+
   # geom_line(
   #   aes(color="red"),
   #   alpha=.0)+
   # scale_color_manual("Legenda",
 #                    guide="legend",
 #                    values = c("Classe 1"="#8dcdeb",
 #                               "Classe 2"="#70c18c",
 #                               "Classe 3"="#fcf7ab",
 #                               "Classe 4"="#eb5661",
 #                               "Pior Classe"="#ac5079"))+
 # guides(color=guide_legend(override.aes = list(linetype=c(1,1,1,1,1),
 #                                               lwd=c(2,2,2,2,2),
 #                                               shape=c(NA,NA,NA,NA,NA),
 #                                               alpha=1)))+
 theme(axis.text.y = element_text(color = "black",
                                  size = 50))+
 theme_classic())
```

![](graficos_baixojacui_files/figure-html/Gráfico OD baixo jacui-1.png)<!-- -->


```r
ggsave("od_bj.png",
       plot = od_bj,
       path = "./graficos",
       dpi = 300,
       type = "cairo")
```

```
## Saving 10 x 6.66 in image
```

```
## Warning: Using ragg device as default. Ignoring `type` and `antialias` arguments
```

```r
# 
# ggsave("od_p2.png",
#        plot = od_p2,
#        path = "./graficos",
#        dpi = 300,
#        type = "cairo")
# 
# ggsave("od_p3.png",
#        plot = od_p3,
#        path = "./graficos",
#        dpi = 300,
#        type = "cairo")
# 
# ggsave("od_3periodos_2.png",
#        units = c("px"),
#        width = 4500,
#        height = 2993,
#        plot = grid.arrange(od_p1, od_p2, od_p3, ncol = 3),
#        path = "./graficos",
#        dpi = 300,
#        type = "cairo")
```
