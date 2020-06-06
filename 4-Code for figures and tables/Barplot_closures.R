

# get table 4
tab4 <- read.csv(paste(pathdir,"5-Output/Celtic seas/Table_closure_options.csv",sep="/"),
                header=T,sep=",",row.names = NULL)

colnames(tab4) <- c("Metric","s1o1_in_closure", "s1o1_ou_closure" ,"s1o2_in_closure" ,"s1o2_ou_closure", 
                    "s2o1_in_closure", "s2o1_ou_closure", "s2o2_in_closure" ,"s2o2_ou_closure")
tab4 <- tab4[-c(1,2,3,8,9,12,13,15,16,19,20,23,24,27,28,31,32,35,36),]

tab4$Theme <- c("VME protection", "VME protection", "VME protection", "VME protection",
                "VME protection and fishing impact threshold", "VME protection and fishing impact threshold", 
                "Fisheries footprint", "Fisheries overlap (presence/absence) (2009-2011)" , 
                "Fisheries overlap (presence/absence) (2009-2011)", "Fisheries overlap (core fishing ground) (2009-2011)",
                "Fisheries overlap (core fishing ground) (2009-2011)", "Fisheries consequences (presence/absence) (2012-2015)",   
                "Fisheries consequences (presence/absence) (2012-2015)",  "Fisheries consequences (core fishing ground) (2012-2015)",
                "Fisheries consequences (core fishing ground) (2012-2015)", "Fisheries consequences (presence/absence) (2016-2019)",   
                "Fisheries consequences (presence/absence) (2016-2019)",  "Fisheries consequences (core fishing ground) (2016-2019)",
                "Fisheries consequences (core fishing ground) (2016-2019)")
tab4$Group <- c("A","A","A","A","B","B","B", "C", "C", "D" ,"D" ,"E" ,"E", "F", "F", "G", "G", "H", "H")

for (i in 2:9){
  tab4[,i] <- as.numeric(as.character(tab4[,c(i)]))
}

tab4$tot <- tab4$s1o1_in_closure + tab4$s1o1_ou_closure

library(stringr)
library(tidyr)
library(ggplot2)


x1 = tab4

### sum of c-square per metric
In = x1[,str_which(colnames(x1), "in")]
percent = In/x1$tot*100
x2 = cbind(x1[,c(11,10,1)], percent)
toplot = x2 %>%
  pivot_longer(-c(1:3), names_to = "scenario", values_to = "Percentage")

#Full plot
ggplot(data=toplot, aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))

# Plots per thematics
A = ggplot(data=toplot[which(toplot$Group == "A"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  ggtitle("VME Protection") + xlab("")
B = ggplot(data=toplot[which(toplot$Group == "B"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  ggtitle("VME protection and fishing impact threshold") + xlab("")
C = ggplot(data=toplot[which(toplot$Group == "C"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  ggtitle("Fisheries overlap (presence/absence) (2009-2011)") + xlab("")
D = ggplot(data=toplot[which(toplot$Group == "D"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  ggtitle("Fisheries overlap (core fishing ground) (2009-2011)") + xlab("")
E = ggplot(data=toplot[which(toplot$Group == "E"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  ggtitle("Fisheries consequences (presence/absence) (2012-2015)") + xlab("")
F1 = ggplot(data=toplot[which(toplot$Group == "F"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  ggtitle("Fisheries consequences (core fishing ground) (2012-2015)") + xlab("")
G = ggplot(data=toplot[which(toplot$Group == "G"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  ggtitle("Fisheries consequences (presence/absence) (2016-2019)") + xlab("")
H = ggplot(data=toplot[which(toplot$Group == "H"),], aes(x=Metric, y=Percentage, fill=scenario)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle=45, hjust = 1, vjust = 1))+
  ggtitle("Fisheries consequences (core fishing ground) (2016-2019)") + xlab("")
        
