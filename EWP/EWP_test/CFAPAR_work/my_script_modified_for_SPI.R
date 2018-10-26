#CORRELATION
#tamsat
#data<-read.table("E:/WA/EWP/CFAPAR_work/TAMSAT resolution/correlation/FOR TUKEY test/reduced_data.csv", sep=",", header=TRUE)
#chirps
#data<-read.table("E:/WA/EWP/CFAPAR_work/CHIRPS resolution/correlation_chirps/FOR TUKEY test/reduced_data.csv", sep=",", header=TRUE)

#COEFF OF DET
#tamsat
#data<-read.table("E:/WA/EWP/CFAPAR_work/TAMSAT resolution/correlation/FOR TUKEY test/reduced_data_cod.csv", sep=",", header=TRUE)
#chirps
data<-read.table("E:/WA/EWP/CFAPAR_work/CHIRPS resolution/correlation_chirps/FOR TUKEY test/reduced_data_cod.csv", sep=",", header=TRUE)

summary(data)
#plot(data$Id,data$r)
#tmp_fn="D:/Users/meronmi/Documents/works/pubblicazioni/in preparazione/2012 Phenology FS/IJRS/submitted/revision/for new table 3/prova.png"
#png(file=tmp_fn,width=2200,height=2200, res = 300)
boxplot(data$r~Id,data=data,xlab="Id SPI",ylab="r/r2 with ZCFAPAR", cex.lab=1.1, ylim=c(-1.0,1.0)) ##,names=nm
dev.off()
#cor.test(data$IPC, data$Z, method="kendall", continuity=TRUE) ##kendal or spearman
data$Id = factor(data$Id)
is.factor(data$Id)
memory.limit(32000)
aov_res = aov(r~Id, data = data)
summary(aov_res)
#tuk_res=TukeyHSD(aov_res, "Id", conf.level = 0.95) #ordered = TRUE
#plot(tuk_res)
#see http://www.r-bloggers.com/anova-and-tukeys-test-on-r/
#install.packages("agricolae")
library(agricolae)
res = HSD.test(aov_res, "Id")
res


