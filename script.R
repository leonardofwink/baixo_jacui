"Plan_BJ_Fepam"

#pacote de dados
install.packages("qboxplot")
library(qboxplot)

#setando o diretório pra salvar os arquivos
setwd("C:/Users/Léo/Desktop/coisas da bolsaa/RHG/G070_Baixo_Jacuí/graficos-boxplot")

#importando as tabelas

##dados 2018-2020
BJ_od20182020<-read.csv("S:/DIPLAN/Leonardo Bolsista/Planilhas/RH Guaíba/Plan importação R/Baixo_Jacui/Dados_Jacui.xlsx - bj_od.csv",header=T)
BJ_dbo20182020<-read.csv("S:/DIPLAN/Leonardo Bolsista/Planilhas/RH Guaíba/Plan importação R/Baixo_Jacui/Dados_Jacui.xlsx - bj_dbo.csv",header=T)
BJ_ptot20182020<-read.csv("S:/DIPLAN/Leonardo Bolsista/Planilhas/RH Guaíba/Plan importação R/Baixo_Jacui/Dados_Jacui.xlsx - bj_ptot.csv",header=T)
BJ_namon20182020<-read.csv("S:/DIPLAN/Leonardo Bolsista/Planilhas/RH Guaíba/Plan importação R/Baixo_Jacui/Dados_Jacui.xlsx - bj_namon.csv",header=T)
BJ_ecoli20182020<-read.csv("S:/DIPLAN/Leonardo Bolsista/Planilhas/RH Guaíba/Plan importação R/Baixo_Jacui/Dados_Jacui.xlsx - bj_ecoli.csv",header=T)
BJ_turb20182020<-read.csv("S:/DIPLAN/Leonardo Bolsista/Planilhas/RH Guaíba/Plan importação R/Baixo_Jacui/Dados_Jacui.xlsx - bj_turb.csv",header=T)

#OD
BJ_od20182020<-read.csv("S:/DIPLAN/Leonardo Bolsista/Planilhas/RH Guaíba/Plan importação R/Baixo_Jacui/Dados_Jacui.xlsx - bj_od.csv",header=T)
pdf("BJ_od20182020.pdf",width=19, height= 17)
qboxplot(BJ_od20182020,main="Oxigênio Dissolvido",cex.main=2,probs=c(0.20,0.5,0.8),ylab="mg/L",las=2,ylim=c(0,15),col=c(8),
         cex.lab = 2, cex.axis = 1.2 ,whisklwd=2.5,outpch="o" )
abline(h=6,col=4,lwd=2)
abline(h=5,col=3,lwd=2)
abline(h=4,col=7,lwd=2)
abline(h=2,col=2,lwd=2)
legend("topleft",title="Legenda",legend=c("Classe 1","Classe 2","Classe 3","Classe 4","Pior Classe"),lty=c(1,1,1,1,1),
       col=c(4,3,7,2,"#AC5079"),lwd=c(10,10,10,10,10),cex = 1.75,pt.cex = 1.75,bg="white")
legend("topright",legend=c("2018-2020"),pch = 22,pt.bg="gray",title="Período",cex = 2,pt.cex = 2,bg="white")
dev.off()
#DBO
BJ_dbo20182020<-read.csv("S:/DIPLAN/Leonardo Bolsista/Planilhas/RH Guaíba/Plan importação R/Baixo_Jacui/Dados_Jacui.xlsx - bj_dbo.csv",header=T)
pdf("BJ_dbo20182020.pdf",width=19, height= 17)
qboxplot(BJ_dbo20182020,main="Demanda Bioquímica de Oxigênio",cex.main=2,probs=c(0.20,0.5,0.8),ylab="mg/L",las=2,ylim=c(0,15),col=c(8), 
         cex.lab = 2, cex.axis = 1.2 ,whisklwd=2.5,outpch="o")
abline(h=10,col=7,lwd=2)
abline(h=3,col=4,lwd=2)
abline(h=5,col=3,lwd=2)
legend("topleft",title="Legenda",legend=c("Classe 1","Classe 2","Classe 3","Pior Classe"),lty=c(1,1,1,1),
       col=c(4,3,7,"#AC5079"),lwd=c(10,10,10,10),cex = 1.75,pt.cex = 1.75,bg="white")
legend("topright",legend=c("2018-2020"),pch = 22,pt.bg="gray",title="Período",cex = 2,pt.cex = 2,bg="white")
dev.off()
#PTOT
BJ_ptot20182020<-read.csv("S:/DIPLAN/Leonardo Bolsista/Planilhas/RH Guaíba/Plan importação R/Baixo_Jacui/Dados_Jacui.xlsx - bj_ptot.csv",header=T)
pdf("BJ_ptot20182020.pdf",width=19, height= 17)
qboxplot(BJ_ptot20182020,main="Fósforo Total",cex.main=2,probs=c(0.20,0.5,0.8),ylab="mg/L",log="y",las=3,ylim=c(0.002,2),col=c(8), 
         cex.lab = 2, cex.axis = 1.2 ,whisklwd=2.5,outpch="o")
abline(h=0.1,col=4,lwd=2)
abline(h=0.15,col=7,lwd=2)
legend("topleft",title="Legenda",legend=c("Classe 1","Classe 3","Pior Classe"),lty=c(1,1,1),
       col=c(4,7,"#AC5079"),lwd=c(10,10,10),cex = 1.75,pt.cex = 1.75,bg="white")
legend("topright",legend=c("2018-2020"),pch = 22,pt.bg="gray",title="Período",cex = 2,pt.cex = 2,bg="white")
dev.off()
#NAMON
BJ_namon20182020<-read.csv("S:/DIPLAN/Leonardo Bolsista/Planilhas/RH Guaíba/Plan importação R/Baixo_Jacui/Dados_Jacui.xlsx - bj_namon.csv",header=T)
pdf("BJ_namon20182020.pdf",width=19, height= 17)
qboxplot(BJ_namon20182020,main="Nitrogênio amoniacal",cex.main=2,probs=c(0.20,0.5,0.8),ylab="mg/L",log="y",las=3,
         ylim=c(0.01,15),col=c(8), cex.lab = 2, cex.axis = 1.2 ,whisklwd=2.5,outpch="o")
abline(h=13.3,col=7,lwd=2)
abline(h=3.7,col=4,lwd=2)
legend("bottomleft",title="Legenda",legend=c("Classe 1","Classe 3","Pior Classe"),lty=c(1,1,1),
       col=c(4,7,"#AC5079"),lwd=c(10,10,10),cex = 1.75,pt.cex = 1.75,bg="white")
legend("bottomright",legend=c("2018-2020"),pch = 22,pt.bg="gray",title="Período",cex = 2,pt.cex = 2,bg="white")
dev.off()
#ECOLI
BJ_ecoli20182020<-read.csv("S:/DIPLAN/Leonardo Bolsista/Planilhas/RH Guaíba/Plan importação R/Baixo_Jacui/Dados_Jacui.xlsx - bj_ecoli.csv",header=T)
pdf("BJ_ecoli20182020.pdf",width=19, height= 17)
qboxplot(BJ_ecoli20182020,main="E.coli",cex.main=2,probs=c(0.20,0.5,0.8),ylab="NMP/100mL",log="y",las=3,ylim=c(1,10000),col=c("8"), 
         cex.lab = 2, cex.axis = 1.2 ,whisklwd=2.5,outpch="o")
abline(h=160,col=4,lwd=2)
abline(h=800,col=3,lwd=2)
abline(h=3200,col=7,lwd=2)
legend("topleft",title="Legenda",legend=c("Classe 1","Classe 2","Classe 3","Pior Classe"),lty=c(1,1,1,1),
       col=c(4,3,7,"#AC5079"),lwd=c(10,10,10,10),cex = 1.75,pt.cex = 1.75,bg="white")
legend("topright",legend=c("2018-2020"),pch = 22,pt.bg="gray",title="Período",cex = 2,pt.cex = 2,bg="white")
dev.off()

#TURB
BJ_turb20182020<-read.csv("S:/DIPLAN/Leonardo Bolsista/Planilhas/RH Guaíba/Plan importação R/Baixo_Jacui/Dados_Jacui.xlsx - bj_turb.csv",header=T)
pdf("BJ_turb20182020.pdf",width=19, height= 17)
qboxplot(BJ_turb20182020,log="y",cex.main=2,probs=c(0.20,0.5,0.8),main="Turbidez",ylab="UNT",ylim=c(5,500),las=3,col=c("8"), 
         cex.lab = 2, cex.axis = 1.2 ,whisklwd=2.5,outpch="o")
abline(h=40,col=4,lwd=2)
abline(h=100,col=7,lwd=2)
legend("topleft",title="Legenda",legend=c("Classe 1","Classe 3","Pior Classe"),lty=c(1,1,1),
       col=c(4,7,"#AC5079"),lwd=c(10,10,10),cex = 1.75,pt.cex = 1.75,bg="white")
legend("topright",legend=c("2018-2020"),pch = 22,pt.bg="gray",title="Período",cex = 2,pt.cex = 2,bg="white")
dev.off()