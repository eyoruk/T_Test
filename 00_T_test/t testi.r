
T.TESTİ unpaired-çift yönlü

                                                                            Normalite
> library(readxl)
> datam = read_excel('datam.xlsx')
> View(datam)
> colnames(datam)[1]='Group'
> datax=as.data.frame(datam)
> View(datax)
> pdf('density.pdf')
> par(mfrow=c(2,3))
> n=colnames(datax)
> for(i in 2:6){
+     hist(datax[,i],main=n[i],probability=TRUE, col='gray',border='green')
+     x= density(datax[,i])
+     lines(x, col='red')
+ }
> dev.off()

> for(i in 2:6){w=shapiro.test(datax[,i])}
> print(w)

> for(i in 2:6){print(shapiro.test(datax[,i])$p.value)}

                                                                                T TESTİ

> hk = c(1,1,1.01,1,1,1.01)
> kmet1 = c(1.97,0.71,5.62,1.41,3.14,4.88)
> t.test(hk, kmet1, alternative = "two.sided", var.equal = FALSE)

> hk = c(1,1,1.01,1,1,1.01)
> kmet1 = c(1.97,0.71,5.62,1.41,3.14,4.88)
> my_data <- data.frame( group = rep(c("Kontrol", "Deney"), each = 6), gen_anlatimi = c(hk, kmet1) )
> print(my_data)

> library(dplyr) 

> group_by(my_data, group) %>% summarise( count = n(), mean = mean(gen_anlatimi, na.rm = TRUE), sd = sd(gen_anlatimi, na.rm = TRUE) )

> library(ggpubr)

> ggboxplot(my_data, x = "group", y = "gen_anlatimi", color = "group", palette = c("#00AFBB", "#E7B800"), ylab = "kmet1_gen_anlatimi", xlab = "Set")



