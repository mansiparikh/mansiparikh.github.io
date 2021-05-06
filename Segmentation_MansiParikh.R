# install.packages("readxl")
library("readxl")
# install.packages("rlang")
library("rlang")

# install.packages("poLCA")
library("poLCA")
# install.packages("dplyr")
library("dplyr")

# install.packages("corrplot")
library("corrplot")

# install.packages("xlsx")
library("xlsx")

# install.packages("reshape2")
library("reshape2")
# install.packages("ggplot2")
library("ggplot2")

# install.packages("plyr")
library("plyr")

# install.packages("sentimentr")
library("sentimentr")

# install.packages("fuzzyjoin")
library("fuzzyjoin")

# install.packages("stringr")
library("stringr")

# install.packages("e1071")
library("e1071")

lightspeed <- read_excel("C://Users//Mansi.Parikh//Documents//GE Survey//GE2.xlsx", 1)
lightspeed <- lightspeed[, colSums(is.na(lightspeed)) != nrow(lightspeed)]

# lightspeed[is.na(ge_lightspeed)] <- 11

c_ge <- lightspeed[lightspeed$GEaud == 1,]
c_gg <- lightspeed[lightspeed$GGaud == 1,]
c_md <- lightspeed[lightspeed$MDaud == 1,]
c_gemd <- lightspeed[lightspeed$GEaud == 1 | lightspeed$MDaud == 1,]

p_ge <- lightspeed[lightspeed$GEaud == 0,]
p_gg <- lightspeed[lightspeed$GGaud == 0,]
p_md <- lightspeed[lightspeed$MDaud == 0,]
p_gemd <- lightspeed[lightspeed$GEaud == 0 & lightspeed$MDaud == 0,]

# GIANT EAGLE - customer segmentation
# testing independence of several grocery variables, specific to GE customers
# vars_c_ge <- c_ge %>% dplyr::select(Q65r1,Q65r2,Q65r3,Q65r4,Q65r5,Q65r6,Q65r7,Q65r8,
#                                     Q49r1,Q49r2,Q49r3,Q49r4,Q49r5,Q49r6,Q49r7,Q49r8,
#                                     Q49r9,Q49r10,Q49r11,Q49r12,Q49r13,Q49r14,
#                                     Q50r1,Q50r2,Q50r3,Q50r4,Q50r5,Q50r6,Q50r7,Q50r8,
#                                     Q50r9,Q50r10,Q50r11,Q50r12,Q50r13,Q50r14,
#                                     Q51r1,Q51r2,Q51r3,Q51r4,Q51r5,Q51r6,Q51r7,Q51r8,
#                                     Q51r9,Q51r10,Q51r11,Q51r12,Q51r13,Q51r14,Q51r15,
#                                     Q51r16,Q51r17,Q51r18,Q51r19,Q51r20,Q51r21)
# c_ge_corr <- cor(vars_c_ge, use = "complete.obs")
# round(c_ge_corr, 2)
# corrplot(c_ge_corr, type = "lower", tl.col = "black", tl.srt = 45)
# c_ge_corr_list <- data.frame(row=rownames(c_ge_corr)[row(c_ge_corr)[upper.tri(c_ge_corr)]], 
#                                 col=colnames(c_ge_corr)[col(c_ge_corr)[upper.tri(c_ge_corr)]], 
#                                 corr=c_ge_corr[upper.tri(c_ge_corr)])
# c_ge_corr_list$corr <- abs(c_ge_corr_list$corr)
# write.xlsx(c_ge_corr_list, "C://Users//Mansi.Parikh//Documents//GE Survey//correl_c_ge.xlsx", 
#            sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
vars_c_ge <- c_ge %>% dplyr::select(Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                    Q65r1,Q65r2,Q65r8,
                                    Q49r3,Q49r5,Q49r10,Q49r12,Q49r14,
                                    Q50r2,Q50r3,Q50r4,Q50r5,Q50r6,Q50r8,
                                    Q50r9,Q50r10,Q50r11,Q50r12,
                                    Q51r4,Q51r6,Q51r7,Q51r12,Q51r16)
f_c_ge <- with(vars_c_ge, cbind(Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                Q65r1,Q65r2,Q65r8,
                                Q49r3,Q49r5,Q49r10,Q49r12,Q49r14,
                                Q50r2,Q50r3,Q50r4,Q50r5,Q50r6,Q50r8,
                                Q50r9,Q50r10,Q50r11,Q50r12,
                                Q51r4,Q51r6,Q51r7,Q51r12,Q51r16)~1)
# f_c_ge_cov <- with(vars_c_ge, cbind(Q)~Qcovariates)
k <- 3 #change value of k and nrep (probably k is between 2 and 3)
lc_c_ge <- poLCA(f_c_ge, c_ge, nclass=k, nrep=50, na.rm=FALSE, graphs=TRUE)
# lc_c_ge_cov <- poLCA(f_c_ge_cov, c_ge, nclass=k, nrep=50, na.rm=FALSE, graphs=TRUE)

# convert to list with melt() to make cleaner plot
lc_c_ge_probs <- melt(lc_c_ge$probs)
zp1 <- ggplot(lc_c_ge_probs, aes(x = L1, y = value, fill = Var2))
zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
zp1 <- zp1 + facet_wrap(~ Var1) 
zp1

c_ge$class <- as.factor(lc_c_ge$predclass)
table(c_ge$class)

# GIANT EALGE - typing tool for prospects
#create a model for any column that will need Naive Bayes model
lc_p_ge1 <- naiveBayes(vars_c_ge, c_ge$class)
class1 <- predict(lc_p_ge1, p_ge)
p_ge <- cbind(p_ge,class1)
table(p_ge$class1)

# check check check - checking if prospects are similar to customers
table(c_ge[c_ge$class == 1,]$Q10)/nrow(c_ge)
summary(c_ge[c_ge$class == 1,]$Q10)

table(p_ge[p_ge$class1 == 1,]$Q10)/nrow(p_ge)
summary(p_ge[p_ge$class1 == 1,]$Q10)

# check check check - check random customers to see if their new class matches the old class
#randomize order of data 
c_ge_rand <- c_ge[order(runif(as.integer(nrow(c_ge)))),]

##set up a test and train dataset for model##
#create object to find 60% of row count (round)
top_70 <- round(nrow(c_ge_rand) * .7, digits = 0)

#top 60% of rows will be used to train model
c_ge_train <- c_ge_rand[1:top_70[1],]

#bottom 40% will be used to test model
c_ge_test <- c_ge_rand[(top_70[1]+1):nrow(c_ge_rand),]


c_ge_bayes <- naiveBayes(c_ge_train[,-1], c_ge_train$class)
class_val <- predict(c_ge_bayes, c_ge_test)
c_ge_val <- cbind(c_ge_test,class_val)
table(c_ge_val$class_val)


# GIANT EAGLE - prospect segmentation
# testing independence of several grocery variables, specific to GE prospects
# vars_p_ge <- p_ge %>% dplyr::select(Q65r1,Q65r2,Q65r3,Q65r4,Q65r5,Q65r6,Q65r7,Q65r8,
#                                     Q49r1,Q49r2,Q49r3,Q49r4,Q49r5,Q49r6,Q49r7,Q49r8,
#                                     Q49r9,Q49r10,Q49r11,Q49r12,Q49r13,Q49r14,
#                                     Q50r1,Q50r2,Q50r3,Q50r4,Q50r5,Q50r6,Q50r7,Q50r8,
#                                     Q50r9,Q50r10,Q50r11,Q50r12,Q50r13,Q50r14,
#                                     Q51r1,Q51r2,Q51r3,Q51r4,Q51r5,Q51r6,Q51r7,Q51r8,
#                                     Q51r9,Q51r10,Q51r11,Q51r12,Q51r13,Q51r14,Q51r15,
#                                     Q51r16,Q51r17,Q51r18,Q51r19,Q51r20,Q51r21)
# p_ge_corr <- cor(vars_p_ge, use = "complete.obs")
# round(p_ge_corr, 2)
# corrplot(p_ge_corr, type = "lower", tl.col = "black", tl.srt = 45)
# p_ge_corr_list <- data.frame(row=rownames(p_ge_corr)[row(p_ge_corr)[upper.tri(p_ge_corr)]], 
#                              col=colnames(p_ge_corr)[col(p_ge_corr)[upper.tri(p_ge_corr)]], 
#                              corr=p_ge_corr[upper.tri(p_ge_corr)])
# p_ge_corr_list$corr <- abs(p_ge_corr_list$corr)
# write.xlsx(p_ge_corr_list, "C://Users//Mansi.Parikh//Documents//GE Survey//correl_p_ge.xlsx", 
#            sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
vars_p_ge <- p_ge %>% dplyr::select(Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                    Q65r1,Q65r2,Q65r8,
                                    Q49r3,Q49r5,
                                    Q49r9,Q49r10,Q49r12,Q49r14,
                                    Q50r4,Q50r5,Q50r6,Q50r8,
                                    Q50r9,Q50r10,Q50r11,Q50r12,Q50r14,
                                    Q51r7,Q51r20)
f_p_ge <- with(vars_p_ge, cbind(Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                Q65r1,Q65r2,Q65r8,
                                Q49r3,Q49r5,
                                Q49r9,Q49r10,Q49r12,Q49r14,
                                Q50r4,Q50r5,Q50r6,Q50r8,
                                Q50r9,Q50r10,Q50r11,Q50r12,Q50r14,
                                Q51r7,Q51r20)~1)
kk <- 2 #change value of k and nrep (probably k is between 2 and 3)
lc_p_ge2 <- poLCA(f_p_ge, p_ge, nclass=kk, nrep=50, na.rm=FALSE, graphs=TRUE)

lc_p_ge_probs <- melt(lc_p_ge2$probs)
zp11 <- ggplot(lc_p_ge_probs, aes(x = L1, y = value, fill = Var2))
zp11 <- zp11 + geom_bar(stat = "identity", position = "stack")
zp11 <- zp11 + facet_wrap(~ Var1) 
zp11

p_ge$class2 <- as.factor(lc_p_ge2$predclass)
table(p_ge$class2)

# GETGO - customer segmentation
vars_c_gg <- c_gg %>% dplyr::select(Q10,Q11a,Q12a,Q13a,Q15a,Q16a,Q17a,Q18r2a,Q18r34a,Q18r89a,
                                    Q22r1a,Q22r3a,Q22r4a,Q26,Q35,Q55)
c_gg_corr <- cor(vars_c_gg, use = "complete.obs")
round(c_gg_corr, 2)
corrplot(c_gg_corr, type = "lower", tl.col = "black", tl.srt = 45)
c_gg_corr_list <- data.frame(row=rownames(c_gg_corr)[row(c_gg_corr)[upper.tri(c_gg_corr)]], 
                             col=colnames(c_gg_corr)[col(c_gg_corr)[upper.tri(c_gg_corr)]], 
                             corr=c_gg_corr[upper.tri(c_gg_corr)])
c_gg_corr_list$corr <- abs(c_gg_corr_list$corr)
f_c_gg <- with(vars_c_gg, cbind(Q10,Q11a,Q12a,Q13a,Q15a,Q16a,Q17a,Q18r2a,Q18r34a,Q18r89a,
                                Q22r1a,Q22r3a,Q22r4a,Q26,Q35,Q55)~1)
m <- 2 #change value of m and nrep (probably m is between 2 and 3)
lc_c_gg <- poLCA(f_c_gg, c_gg, nclass=m, nrep=20, na.rm=FALSE, graphs=TRUE)

lc_c_gg_probs <- melt(lc_c_gg$probs)
zp2 <- ggplot(lc_c_gg_probs, aes(x = L1, y = value, fill = Var2))
zp2 <- zp2 + geom_bar(stat = "identity", position = "stack")
zp2 <- zp2 + facet_wrap(~ Var1) 
zp2

c_gg$class <- as.factor(lc_c_gg$predclass)
table(c_gg$class)

# GETGO - typing tool for prospects
lc_p_gg1 <- poLCA(f_c_gg, p_gg, nclass=m, nrep=20, na.rm=FALSE, graphs=TRUE)
p_gg$class1 <- as.factor(lc_p_gg1$predclass)
vars_c_gg_class <- c_gg %>% dplyr::select(record,Q10,Q11a,Q12a,Q13a,Q15a,Q16a,Q17a,Q18r2a,Q18r34a,Q18r89a,
                                          Q22r1a,Q22r3a,Q22r4a,Q26,Q35,Q55,class)
vars_p_gg_class <- p_gg %>% dplyr::select(record,Q10,Q11a,Q12a,Q13a,Q15a,Q16a,Q17a,Q18r2a,Q18r34a,Q18r89a,
                                          Q22r1a,Q22r3a,Q22r4a,Q26,Q35,Q55,class1)
write.xlsx(vars_c_gg_class, "C://Users//Mansi.Parikh//Documents//GE Survey//c_gg_class.xlsx", 
           sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(vars_p_gg_class, "C://Users//Mansi.Parikh//Documents//GE Survey//p_gg_class.xlsx", 
           sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

# GETGO - prospect segmentation
vars_p_gg <- p_gg %>% dplyr::select(Q10,Q11a,Q12a,Q13a,Q15a,Q16a,Q17a,Q18r2a,Q18r34a,Q18r89a,
                                    Q22r2a,Q22r3a,Q22r4a,Q26,Q35,Q55)
p_gg_corr <- cor(vars_p_gg, use = "complete.obs")
round(p_gg_corr, 2)
corrplot(p_gg_corr, type = "lower", tl.col = "black", tl.srt = 45)
p_gg_corr_list <- data.frame(row=rownames(p_gg_corr)[row(p_gg_corr)[upper.tri(p_gg_corr)]], 
                             col=colnames(p_gg_corr)[col(p_gg_corr)[upper.tri(p_gg_corr)]], 
                             corr=p_gg_corr[upper.tri(p_gg_corr)])
p_gg_corr_list$corr <- abs(p_gg_corr_list$corr)
f_p_gg <- with(vars_p_gg, cbind(Q10,Q11a,Q12a,Q13a,Q15a,Q16a,Q17a,Q18r2a,Q18r34a,Q18r89a,
                                Q22r2a,Q22r3a,Q22r4a,Q26,Q35,Q55)~1)
m <- 2 #change value of m and nrep (probably m is between 2 and 3)
lc_p_gg2 <- poLCA(f_p_gg, p_gg, nclass=m, nrep=100, na.rm=FALSE, graphs=TRUE)

lc_p_gg_probs <- melt(lp_p_gg2$probs)
zp22 <- ggplot(lc_p_gg_probs, aes(x = L1, y = value, fill = Var2))
zp22 <- zp22 + geom_bar(stat = "identity", position = "stack")
zp22 <- zp22 + facet_wrap(~ Var1) 
zp22

p_gg$class2 <- as.factor(lc_p_ge2$predclass)
table(p_gg$class2)

# MARKET DISTRICT
# testing independence of several grocery variables, specific to MD customers
vars_c_md <- c_md %>% dplyr::select(Q65r1,Q65r2,Q65r3,Q65r4,Q65r5,Q65r6,Q65r7,Q65r8,
                                    Q49r1,Q49r2,Q49r3,Q49r4,Q49r5,Q49r6,Q49r7,Q49r8,
                                    Q49r9,Q49r10,Q49r11,Q49r12,Q49r13,Q49r14,
                                    Q50r1,Q50r2,Q50r3,Q50r4,Q50r5,Q50r6,Q50r7,Q50r8,
                                    Q50r9,Q50r10,Q50r11,Q50r12,Q50r13,Q50r14,
                                    Q51r1,Q51r2,Q51r3,Q51r4,Q51r5,Q51r6,Q51r7,Q51r8,
                                    Q51r9,Q51r10,Q51r11,Q51r12,Q51r13,Q51r14,Q51r15,
                                    Q51r16,Q51r17,Q51r18,Q51r19,Q51r20,Q51r21)
c_md_corr <- cor(vars_c_md, use = "complete.obs")
round(c_md_corr, 2)
corrplot(c_md_corr, type = "lower", tl.col = "black", tl.srt = 45)
c_md_corr_list <- data.frame(row=rownames(c_md_corr)[row(c_md_corr)[upper.tri(c_md_corr)]], 
                             col=colnames(c_md_corr)[col(c_md_corr)[upper.tri(c_md_corr)]], 
                             corr=c_md_corr[upper.tri(c_md_corr)])
c_md_corr_list$corr <- abs(c_md_corr_list$corr)
write.xlsx(c_md_corr_list, "C://Users//Mansi.Parikh//Documents//GE Survey//correl_c_md.xlsx", 
           sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
vars_c_md <- c_md %>% dplyr::select(Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                    Q65r1,Q65r2,
                                    Q49r5,Q49r10,Q49r12,Q49r14,
                                    Q50r3,Q50r4,Q50r5,Q50r6,Q50r8,
                                    Q50r10,Q50r12,
                                    Q51r4,Q51r7,Q51r8,
                                    Q51r11,Q51r12)
f_c_md <- with(vars_c_md, cbind(Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                Q65r1,Q65r2,
                                Q49r5,Q49r10,Q49r12,Q49r14,
                                Q50r3,Q50r4,Q50r5,Q50r6,Q50r8,
                                Q50r10,Q50r12,
                                Q51r4,Q51r7,Q51r8,
                                Q51r11,Q51r12)~1)
n <- 3
lc_c_md <- poLCA(f_c_md, c_md, nclass=n, nrep=50, na.rm=FALSE, graphs=TRUE)

lc_c_md_probs <- melt(lc_c_md$probs)
zp3 <- ggplot(lc_c_md_probs, aes(x = L1, y = value, fill = Var2))
zp3 <- zp3 + geom_bar(stat = "identity", position = "stack")
zp3 <- zp3 + facet_wrap(~ Var1) 
zp3

c_md$class <- as.factor(lc_c_md$predclass)
table(c_md$class)

# MARKET DISTRICT - typing tool for prospects
lc_p_md1 <- poLCA(f_c_md, p_md, nclass=n, nrep=100, na.rm=FALSE, graphs=TRUE)
p_md$class1 <- as.factor(lc_p_md1$predclass)
vars_c_md_class <- c_md %>% dplyr::select(record,Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                          Q65r1,Q65r2,
                                          Q49r5,Q49r10,Q49r12,Q49r14,
                                          Q50r3,Q50r4,Q50r5,Q50r6,Q50r8,
                                          Q50r10,Q50r12,
                                          Q51r4,Q51r7,Q51r8,
                                          Q51r11,Q51r12,class)
vars_p_md_class <- p_md %>% dplyr::select(record,Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                          Q65r1,Q65r2,
                                          Q49r5,Q49r10,Q49r12,Q49r14,
                                          Q50r3,Q50r4,Q50r5,Q50r6,Q50r8,
                                          Q50r10,Q50r12,
                                          Q51r4,Q51r7,Q51r8,
                                          Q51r11,Q51r12,class1)
write.xlsx(vars_c_md_class, "C://Users//Mansi.Parikh//Documents//GE Survey//c_md_class.xlsx", 
           sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(vars_p_md_class, "C://Users//Mansi.Parikh//Documents//GE Survey//p_md_class.xlsx", 
           sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

# MARKET DISTRICT - prospect segmentation
# testing independence of several grocery variables, specific to MD prospects
vars_p_md <- p_md %>% dplyr::select(Q65r1,Q65r2,Q65r3,Q65r4,Q65r5,Q65r6,Q65r7,Q65r8,
                                    Q49r1,Q49r2,Q49r3,Q49r4,Q49r5,Q49r6,Q49r7,Q49r8,
                                    Q49r9,Q49r10,Q49r11,Q49r12,Q49r13,Q49r14,
                                    Q50r1,Q50r2,Q50r3,Q50r4,Q50r5,Q50r6,Q50r7,Q50r8,
                                    Q50r9,Q50r10,Q50r11,Q50r12,Q50r13,Q50r14,
                                    Q51r1,Q51r2,Q51r3,Q51r4,Q51r5,Q51r6,Q51r7,Q51r8,
                                    Q51r9,Q51r10,Q51r11,Q51r12,Q51r13,Q51r14,Q51r15,
                                    Q51r16,Q51r17,Q51r18,Q51r19,Q51r20,Q51r21)
p_md_corr <- cor(vars_p_md, use = "complete.obs")
round(p_md_corr, 2)
corrplot(p_md_corr, type = "lower", tl.col = "black", tl.srt = 45)
p_md_corr_list <- data.frame(row=rownames(p_md_corr)[row(p_md_corr)[upper.tri(p_md_corr)]], 
                             col=colnames(p_md_corr)[col(p_md_corr)[upper.tri(p_md_corr)]], 
                             corr=p_md_corr[upper.tri(p_md_corr)])
p_md_corr_list$corr <- abs(p_md_corr_list$corr)
write.xlsx(p_md_corr_list, "C://Users//Mansi.Parikh//Documents//GE Survey//correl_p_md.xlsx", 
           sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
vars_p_md <- p_md %>% dplyr::select(Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                    Q65r1,Q65r2,Q65r8,
                                    Q49r3,Q49r5,
                                    Q49r10,Q49r12,Q49r14,
                                    Q50r1,Q50r2,Q50r4,Q50r5,Q50r6,Q50r8,
                                    Q50r9,Q50r10,Q50r11,Q50r12,
                                    Q51r4,Q51r7,Q51r16)
f_p_md <- with(vars_p_md, cbind(Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                Q65r1,Q65r2,Q65r8,
                                Q49r3,Q49r5,
                                Q49r10,Q49r12,Q49r14,
                                Q50r1,Q50r2,Q50r4,Q50r5,Q50r6,Q50r8,
                                Q50r9,Q50r10,Q50r11,Q50r12,
                                Q51r4,Q51r7,Q51r16)~1)
nn <- 3
lc_p_md2 <- poLCA(f_p_md, p_md, nclass=nn, nrep=10, na.rm=FALSE, graphs=TRUE)

lc_p_md_probs <- melt(lc_p_md2$probs)
zp33 <- ggplot(lc_p_md_probs, aes(x = L1, y = value, fill = Var2))
zp33 <- zp33 + geom_bar(stat = "identity", position = "stack")
zp33 <- zp33 + facet_wrap(~ Var1) 
zp33

p_md$class2 <- as.factor(lc_p_md2$predclass)
table(p_md$class2)

# GIANT EAGLE AND MARKET DISTRICT - customer segmentation
# testing independence of several grocery variables, specific to GE or MD customers
vars_c_gemd <- c_gemd %>% dplyr::select(Q65r1,Q65r2,Q65r3,Q65r4,Q65r5,Q65r6,Q65r7,Q65r8,
                                        Q49r1,Q49r2,Q49r3,Q49r4,Q49r5,Q49r6,Q49r7,Q49r8,
                                        Q49r9,Q49r10,Q49r11,Q49r12,Q49r13,Q49r14,
                                        Q50r1,Q50r2,Q50r3,Q50r4,Q50r5,Q50r6,Q50r7,Q50r8,
                                        Q50r9,Q50r10,Q50r11,Q50r12,Q50r13,Q50r14,
                                        Q51r1,Q51r2,Q51r3,Q51r4,Q51r5,Q51r6,Q51r7,Q51r8,
                                        Q51r9,Q51r10,Q51r11,Q51r12,Q51r13,Q51r14,Q51r15,
                                        Q51r16,Q51r17,Q51r18,Q51r19,Q51r20,Q51r21)
c_gemd_corr <- cor(vars_c_gemd, use = "complete.obs")
round(c_gemd_corr, 2)
corrplot(c_gemd_corr, type = "lower", tl.col = "black", tl.srt = 45)
c_gemd_corr_list <- data.frame(row=rownames(c_gemd_corr)[row(c_gemd_corr)[upper.tri(c_gemd_corr)]], 
                             col=colnames(c_gemd_corr)[col(c_gemd_corr)[upper.tri(c_gemd_corr)]], 
                             corr=c_gemd_corr[upper.tri(c_gemd_corr)])
c_gemd_corr_list$corr <- abs(c_gemd_corr_list$corr)
write.xlsx(c_gemd_corr_list, "C://Users//Mansi.Parikh//Documents//GE Survey//correl_c_gemd.xlsx", 
           sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
vars_c_gemd <- c_gemd %>% dplyr::select(Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                        Q65r1,Q65r2,Q65r8,
                                        Q49r3,Q49r5,
                                        Q49r10,Q49r12,Q49r14,
                                        Q50r2,Q50r3,Q50r4,Q50r5,Q50r6,Q50r8,
                                        Q50r9,Q50r10,Q50r11,Q50r12,
                                        Q51r4,Q51r6,Q51r7,Q51r12,Q51r16)
f_c_gemd <- with(vars_c_gemd, cbind(Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                    Q65r1,Q65r2,Q65r8,
                                    Q49r3,Q49r5,
                                    Q49r10,Q49r12,Q49r14,
                                    Q50r2,Q50r3,Q50r4,Q50r5,Q50r6,Q50r8,
                                    Q50r9,Q50r10,Q50r11,Q50r12,
                                    Q51r4,Q51r6,Q51r7,Q51r12,Q51r16)~1)
o <- 3
lc_c_gemd <- poLCA(f_c_gemd, c_gemd, nclass=o, nrep=10, na.rm=FALSE, graphs=TRUE)

lc_c_gemd_probs <- melt(lc_c_gemd$probs)
zp4 <- ggplot(lc_c_gemd_probs, aes(x = L1, y = value, fill = Var2))
zp4 <- zp4 + geom_bar(stat = "identity", position = "stack")
zp4 <- zp4 + facet_wrap(~ Var1) 
zp4

c_gemd$class <- as.factor(lc_c_gemd$predclass)
table(c_gemd$class)

# GIANT EAGLE AND MARKET DISTRICT - typing tool for prospects
lc_p_gemd1 <- poLCA(f_c_gemd, p_gemd, nclass=o, nrep=10, na.rm=FALSE, graphs=TRUE)
p_gemd$class1 <- as.factor(lc_p_gemd1$predclass)
vars_c_gemd_class <- c_gemd %>% dplyr::select(record,Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                              Q65r1,Q65r2,Q65r8,
                                              Q49r3,Q49r5,
                                              Q49r10,Q49r12,Q49r14,
                                              Q50r2,Q50r3,Q50r4,Q50r5,Q50r6,Q50r8,
                                              Q50r9,Q50r10,Q50r11,Q50r12,
                                              Q51r4,Q51r6,Q51r7,Q51r12,Q51r16,class)
vars_p_gemd_class <- p_gemd %>% dplyr::select(record,Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                              Q65r1,Q65r2,Q65r8,
                                              Q49r3,Q49r5,
                                              Q49r10,Q49r12,Q49r14,
                                              Q50r2,Q50r3,Q50r4,Q50r5,Q50r6,Q50r8,
                                              Q50r9,Q50r10,Q50r11,Q50r12,
                                              Q51r4,Q51r6,Q51r7,Q51r12,Q51r16,class1)
write.xlsx(vars_c_gemd_class, "C://Users//Mansi.Parikh//Documents//GE Survey//c_gemd_class.xlsx", 
           sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(vars_p_gemd_class, "C://Users//Mansi.Parikh//Documents//GE Survey//p_gemd_class.xlsx", 
           sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

# GIANT EAGLE MARKET DISTRICT - prospect segmentation
vars_p_gemd <- p_gemd %>% dplyr::select(Q65r1,Q65r2,Q65r3,Q65r4,Q65r5,Q65r6,Q65r7,Q65r8,
                                        Q49r1,Q49r2,Q49r3,Q49r4,Q49r5,Q49r6,Q49r7,Q49r8,
                                        Q49r9,Q49r10,Q49r11,Q49r12,Q49r13,Q49r14,
                                        Q50r1,Q50r2,Q50r3,Q50r4,Q50r5,Q50r6,Q50r7,Q50r8,
                                        Q50r9,Q50r10,Q50r11,Q50r12,Q50r13,Q50r14,
                                        Q51r1,Q51r2,Q51r3,Q51r4,Q51r5,Q51r6,Q51r7,Q51r8,
                                        Q51r9,Q51r10,Q51r11,Q51r12,Q51r13,Q51r14,Q51r15,
                                        Q51r16,Q51r17,Q51r18,Q51r19,Q51r20,Q51r21)
p_gemd_corr <- cor(vars_p_gemd, use = "complete.obs")
round(p_gemd_corr, 2)
corrplot(p_gemd_corr, type = "lower", tl.col = "black", tl.srt = 45)
p_gemd_corr_list <- data.frame(row=rownames(p_gemd_corr)[row(p_gemd_corr)[upper.tri(p_gemd_corr)]], 
                               col=colnames(p_gemd_corr)[col(p_gemd_corr)[upper.tri(p_gemd_corr)]], 
                               corr=p_gemd_corr[upper.tri(p_gemd_corr)])
p_gemd_corr_list$corr <- abs(p_gemd_corr_list$corr)
write.xlsx(p_gemd_corr_list, "C://Users//Mansi.Parikh//Documents//GE Survey//correl_p_gemd.xlsx", 
           sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
vars_p_gemd <- p_gemd %>% dplyr::select(Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                        Q65r1,Q65r2,Q65r8,
                                        Q49r3,Q49r5,Q49r7,
                                        Q49r9,Q49r10,Q49r12,Q49r13,
                                        Q50r1,Q50r4,Q50r5,Q50r6,Q50r8,
                                        Q50r9,Q50r10,Q50r11,Q50r12,Q50r14,
                                        Q51r7,Q51r20)
f_p_gemd <- with(vars_p_gemd, cbind(Q10,Q11a,Q12a,Q13a,Q21,#Q36,Q45,Q46a,Q47,Q48,
                                    Q65r1,Q65r2,Q65r8,
                                    Q49r3,Q49r5,Q49r7,
                                    Q49r9,Q49r10,Q49r12,Q49r13,
                                    Q50r1,Q50r4,Q50r5,Q50r6,Q50r8,
                                    Q50r9,Q50r10,Q50r11,Q50r12,Q50r14,
                                    Q51r7,Q51r20)~1)
oo <- 3
lc_p_gemd2 <- poLCA(f_p_gemd, p_gemd, nclass=oo, nrep=10, na.rm=FALSE, graphs=TRUE)

lc_p_gemd_probs <- melt(lc_p_gemd2$probs)
zp44 <- ggplot(lc_p_gemd_probs, aes(x = L1, y = value, fill = Var2))
zp44 <- zp44 + geom_bar(stat = "identity", position = "stack")
zp44 <- zp44 + facet_wrap(~ Var1) 
zp44

p_gemd$class2 <- as.factor(lc_p_gemd2$predclass)
table(p_gemd$class2)
