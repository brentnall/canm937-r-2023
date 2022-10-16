mydta = read.delim("brca_metabric_clinical_data.tsv", sep="\t")

head(mydta)

summary(mydta)

boxplot(mydta$Age.at.Diagnosis, xlab="Age")

hist(mydta$Tumor.Size, xlab="Tumour size", main="Tumour size Histogram")

plot(mydta$Tumor.Size, mydta$Lymph.nodes.examined.positive, log="x")
 
mydta$LNpos =  mydta$Lymph.nodes.examined.positive >0

cor.test(mydta$Tumor.Size, mydta$Lymph.nodes.examined.positive, method="spearman")

boxplot(split(mydta$Tumor.Size, mydta$LNpos))

tapply(mydta$Tumor.Size, mydta$LNpos, summary)

mysize_pos = subset(mydta$Tumor.Size, mydta$LNpos)

mysize_neg = subset(mydta$Tumor.Size, !mydta$LNpos)

summary(mysize_pos)

summary(mysize_neg)

wilcox.test(mysize_pos, mysize_neg)

npos = c(sum(mysize_neg>20, na.rm=TRUE), sum(mysize_pos>20, na.rm=TRUE))

ntot = c(sum(!is.na(mysize_neg)), sum(!is.na(mysize_pos)))

## number size >20mm
npos

## number with size available
ntot

## proportion
npos / ntot

prop.test(npos, ntot)
