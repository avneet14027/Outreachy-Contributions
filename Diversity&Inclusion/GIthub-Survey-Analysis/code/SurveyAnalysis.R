#Analysis of particiations in open source and factors affecting them.

#install.packages("ggplot2")
#install.packages("circlize")
library(ggplot2)
library(reshape2)
#library(circlize)
df <- read.csv("/home/reen/Downloads/data_for_public_release/data_for_public_release/survey_data.csv",header = TRUE,na.strings=" ",sep = ",")
df
rows<-nrow(df)
cols<-ncol(df)
rows
cols
#Number of contributors vs types of contributions made
#stacked chart chord viz pie simple bar/histo 

#distribution of gender of participators
gender_part<-(df[-which(df$GENDER == ""), ])
gender_part<-gender_part['GENDER']
table(gender_part)
pie(table(gender_part), labels = paste(round(prop.table(table(gender_part))*100), "%", sep = ""), 
    col = heat.colors(5), main = "distribution of gender of participators")

legend("topright", legend = c("-", "Man", "Non Binary/other", "Prefer Not To Say", "Woman"), 
       fill = heat.colors(5), title = "Categories", cex = 1)

types_participations = subset(df, , select = c("PARTICIPATION.TYPE.FOLLOW", "PARTICIPATION.TYPE.USE.APPLICATIONS","PARTICIPATION.TYPE.USE.DEPENDENCIES","PARTICIPATION.TYPE.CONTRIBUTE","PARTICIPATION.TYPE.OTHER","GENDER"))

#Types of contributions vs number (distribution od types of contributions)
n_follow<-length(types_participations$PARTICIPATION.TYPE.FOLLOW[types_participations$PARTICIPATION.TYPE.FOLLOW == "1"])
n_applications<-length(types_participations$PARTICIPATION.TYPE.USE.APPLICATIONS[types_participations$PARTICIPATION.TYPE.USE.APPLICATIONS=="1"])
n_dependencies<-length(types_participations$PARTICIPATION.TYPE.USE.DEPENDENCIES[types_participations$PARTICIPATION.TYPE.USE.DEPENDENCIES=="1"])
n_contribute<-length(types_participations$PARTICIPATION.TYPE.CONTRIBUTE[types_participations$PARTICIPATION.TYPE.CONTRIBUTE=="1"])
n_other<-length(types_participations$PARTICIPATION.TYPE.OTHER[types_participations$PARTICIPATION.TYPE.OTHER=="1"])
#plot for above
gdf <- data.frame(type_of_particiation=c("FOLLOW", "APPLICATIONS", "DEPENDENCIES","CONTRIBUTE","OTHER"),
                 Number=c(n_follow,n_applications,n_dependencies,n_contribute,n_other))
p<-ggplot(data=gdf, aes(x=type_of_particiation, y=Number)) + geom_bar(stat="identity",fill="steelblue") + geom_text(aes(label=Number), vjust=1.6, color="white", size=3.5)

#contributions genderwise data
follow_gender<-with(types_participations, table(PARTICIPATION.TYPE.FOLLOW, GENDER))
gender_applications<-with(types_participations, table(PARTICIPATION.TYPE.USE.APPLICATIONS, GENDER))
gender_dependencies<-with(types_participations, table(PARTICIPATION.TYPE.USE.DEPENDENCIES, GENDER))
gender_contribute<-with(types_participations, table(PARTICIPATION.TYPE.CONTRIBUTE, GENDER))
gender_other<-with(types_participations, table(PARTICIPATION.TYPE.OTHER, GENDER))

#Gender wise contributions Plot
gPart <- data.frame(supp=rep(c("Men", "Women","Non-Binary or Other","Prefer_Not_To_Say"), each=5),type_of_particiation=c( "APPLICATIONS", "CONTRIBUTE","DEPENDENCIES","FOLLOW","OTHER"),
                  Number=c(follow_gender[2,2],gender_applications[2,2],gender_contribute[2,2],gender_dependencies[2,2],gender_other[2,2],follow_gender[2,3],gender_applications[2,3],gender_contribute[2,3],gender_dependencies[2,3],gender_other[2,3],follow_gender[2,4],gender_applications[2,4],gender_contribute[2,4],gender_dependencies[2,4],gender_other[2,4],follow_gender[2,5],gender_applications[2,5],gender_contribute[2,5],gender_dependencies[2,5],gender_other[2,5]))
gpart<-ggplot(data=gPart, aes(x=type_of_particiation, y=Number, fill=supp)) +
  geom_bar(stat="identity") + scale_fill_brewer(palette="Blues")
# Use position=position_dodge()
gpart<-ggplot(data=gPart, aes(x=type_of_particiation, y=Number, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Blues")

#intersections (How people are contributing to more than one type)
A_C<-nrow(subset(types_participations, PARTICIPATION.TYPE.USE.APPLICATIONS==1 & PARTICIPATION.TYPE.CONTRIBUTE==1))
A_D<-nrow(subset(types_participations, PARTICIPATION.TYPE.USE.APPLICATIONS==1 & PARTICIPATION.TYPE.USE.DEPENDENCIES==1))
A_F<-nrow(subset(types_participations, PARTICIPATION.TYPE.USE.APPLICATIONS==1 & PARTICIPATION.TYPE.FOLLOW==1))
A_O<-nrow(subset(types_participations, PARTICIPATION.TYPE.USE.APPLICATIONS==1 & PARTICIPATION.TYPE.OTHER==1))
C_D<-nrow(subset(types_participations, PARTICIPATION.TYPE.CONTRIBUTE==1 & PARTICIPATION.TYPE.USE.DEPENDENCIES==1))
C_F<-nrow(subset(types_participations, PARTICIPATION.TYPE.CONTRIBUTE==1 & PARTICIPATION.TYPE.FOLLOW==1))
C_O<-nrow(subset(types_participations, PARTICIPATION.TYPE.CONTRIBUTE==1 & PARTICIPATION.TYPE.OTHER==1))
D_F<-nrow(subset(types_participations, PARTICIPATION.TYPE.USE.DEPENDENCIES==1 & PARTICIPATION.TYPE.FOLLOW==1))
D_O<-nrow(subset(types_participations, PARTICIPATION.TYPE.USE.DEPENDENCIES==1 & PARTICIPATION.TYPE.OTHER==1))
F_O<-nrow(subset(types_participations, PARTICIPATION.TYPE.FOLLOW==1 & PARTICIPATION.TYPE.OTHER==1))

Table_chord = matrix(c(0,A_C, A_D, A_F, A_O, A_C,0,C_D, C_F,C_O, A_D, C_D, 0,D_F, D_O,A_F,C_F,D_F,0,F_O,A_O,C_O,D_O,F_O,0),nrow=5,ncol=5,byrow = TRUE)
dimnames(Table_chord) = list(c("Applications","Contributions","Dependencies","Follow","Other"), c("Applications","Contributions","Dependencies","Follow","Other"))
df2 = data.frame(from = rep(rownames(Table_chord), times = ncol(Table_chord)),
                to = rep(colnames(Table_chord), each = nrow(Table_chord)),
                value = as.vector(Table_chord),
                stringsAsFactors = FALSE)
#chordDiagram(Table_chord)
library(chorddiag)
chorddiag(Table_chord, groupnamePadding = 20,showTicks=FALSE)

#how often they contribute?
#how frequently does each gender contribute?
#Type vs frequency vs Gender  
#Type vs Gender
#Frequency vs Gender (any Type)
types_contributions = subset(df, , select = c("CONTRIBUTOR.TYPE.CONTRIBUTE.CODE",	"CONTRIBUTOR.TYPE.CONTRIBUTE.DOCS",	"CONTRIBUTOR.TYPE.PROJECT.MAINTENANCE",	"CONTRIBUTOR.TYPE.FILE.BUGS",	"CONTRIBUTOR.TYPE.FEATURE.REQUESTS",	"CONTRIBUTOR.TYPE.COMMUNITY.ADMIN","GENDER","EMPLOYMENT.STATUS"))

frequency_code<-with(types_contributions, table(CONTRIBUTOR.TYPE.CONTRIBUTE.CODE, GENDER))
frequency_docs<-with(types_contributions, table(CONTRIBUTOR.TYPE.CONTRIBUTE.DOCS, GENDER))
frequency_maintenance<-with(types_contributions, table(CONTRIBUTOR.TYPE.PROJECT.MAINTENANCE, GENDER))
frequency_bugs<-with(types_contributions, table(CONTRIBUTOR.TYPE.FILE.BUGS, GENDER))
frequency_requests<-with(types_contributions, table(CONTRIBUTOR.TYPE.FEATURE.REQUESTS, GENDER))
frequency_admin<-with(types_contributions, table(CONTRIBUTOR.TYPE.COMMUNITY.ADMIN, GENDER))

gContri_frequent <- data.frame(supp=rep(c("Men","Non-Binary or Other","Prefer_Not_To_Say","Women"), each=6),type_of_contribution=c("ADMIN","BUGS","CODE", "DOCS","REQUESTS","MAINTENANCE"),
                    Number=c(frequency_admin[2,2],frequency_bugs[2,2],frequency_code[2,2],frequency_docs[2,2],frequency_maintenance[2,2],frequency_requests[2,2],frequency_admin[2,3],frequency_bugs[2,3],frequency_code[2,3],frequency_docs[2,3],frequency_maintenance[2,3],frequency_requests[2,3],frequency_admin[2,4],frequency_bugs[2,4],frequency_code[2,4],frequency_docs[2,4],frequency_maintenance[2,4],frequency_requests[2,4],frequency_admin[2,5],frequency_bugs[2,5],frequency_code[2,5],frequency_docs[2,5],frequency_maintenance[2,5],frequency_requests[2,5]))
gcontri_frequent<-ggplot(data=gContri_frequent, aes(x=type_of_contribution, y=Number, fill=supp)) +
geom_bar(stat="identity", position=position_dodge())

# #gContri_never <- data.frame(supp=rep(c("Men", "Women","Non-Binary or Other","Prefer_Not_To_Say"), each=6),type_of_contribution=c("CODE", "DOCS", "MAINTENANCE","BUGS","REQUESTS","ADMIN"),
#                             #Number=c(frequency_code[3,2],frequency_code[3,5],frequency_code[3,3],frequency_code[3,4],frequency_docs[3,2],frequency_docs[3,5],frequency_docs[3,3],frequency_docs[3,4],frequency_maintenance[3,2],frequency_maintenance[3,5],frequency_maintenance[3,3],frequency_maintenance[3,4],frequency_bugs[2,2],frequency_bugs[3,5],frequency_bugs[3,3],frequency_bugs[3,4],frequency_requests[3,2],frequency_requests[3,5],frequency_requests[3,3],frequency_requests[3,4],frequency_admin[2,2],frequency_admin[3,5],frequency_admin[3,3],frequency_admin[3,4]))
# 
# #gcontri_never<-ggplot(data=gContri_never, aes(x=type_of_contribution, y=Number, fill=supp)) +
#   geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Blues")

#how frequently does each individual contributes wrt employment time?
emp_code<-with(types_contributions, table(CONTRIBUTOR.TYPE.CONTRIBUTE.CODE, EMPLOYMENT.STATUS))
emp_docs<-with(types_contributions, table(CONTRIBUTOR.TYPE.CONTRIBUTE.DOCS, EMPLOYMENT.STATUS))
emp_maintenance<-with(types_contributions, table(CONTRIBUTOR.TYPE.PROJECT.MAINTENANCE, EMPLOYMENT.STATUS))
emp_bugs<-with(types_contributions, table(CONTRIBUTOR.TYPE.FILE.BUGS, EMPLOYMENT.STATUS))
emp_requests<-with(types_contributions, table(CONTRIBUTOR.TYPE.FEATURE.REQUESTS, EMPLOYMENT.STATUS))
emp_admin<-with(types_contributions, table(CONTRIBUTOR.TYPE.COMMUNITY.ADMIN, EMPLOYMENT.STATUS))

gContri_emp <- data.frame(supp=rep(c("Employed full time", "Employed part time","Full time student Other","Other","Retired or permanently not working"), each=6),type_of_contribution=c("ADMIN","BUGS","CODE", "DOCS","REQUESTS","MAINTENANCE"),
                               Number=c(emp_admin[2,2],emp_bugs[2,2],emp_code[2,2],emp_docs[2,2],emp_maintenance[2,2],emp_requests[2,2],emp_admin[2,3],emp_bugs[2,3],emp_code[2,3],emp_docs[2,3],emp_maintenance[2,3],emp_requests[2,3],emp_admin[2,4],emp_bugs[2,4],emp_code[2,4],emp_docs[2,4],emp_maintenance[2,4],emp_requests[2,4],emp_admin[2,5],emp_bugs[2,5],emp_code[2,5],emp_docs[2,5],emp_maintenance[2,5],emp_requests[2,5],emp_admin[2,6],emp_bugs[2,6],emp_code[2,6],emp_docs[2,6],emp_maintenance[2,6],emp_requests[2,6]))
gcontri_emp<-ggplot(data=gContri_emp, aes(x=type_of_contribution, y=Number, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())

#future contribution interest vs likelihood

future_contribution <- subset(df,,select=c("FUTURE.CONTRIBUTION.INTEREST","FUTURE.CONTRIBUTION.LIKELIHOOD","GENDER"))
future_contribution[future_contribution==""] <- NA
likliness<-with(future_contribution,table(FUTURE.CONTRIBUTION.INTEREST,FUTURE.CONTRIBUTION.LIKELIHOOD))
likeliness_matrix <- data.matrix(likliness)
likeliness_matrix<-na.omit(likeliness_matrix)
likeliness_heatmap <- heatmap(likeliness_matrix, Rowv=NA, Colv=NA, col = heat.colors(5,0.5), scale="col", margins=c(10,10),cexRow = 1,cexCol = 1)

#future contribution interest vs gender
future_contribution <- subset(df,,select=c("FUTURE.CONTRIBUTION.INTEREST","FUTURE.CONTRIBUTION.LIKELIHOOD","GENDER"))
interest<-with(future_contribution,table(GENDER,FUTURE.CONTRIBUTION.INTEREST))

ginterest <- data.frame(supp=rep(c("Not at all interested", "Not too interested","Somewhat interested","Very interested"), each=4),gender=c("Man","Non-binary  or Other","Prefer not to say", "Woman "),
                          Number=c(interest[2,2],interest[3,2],interest[4,2],interest[5,2],interest[2,3],interest[3,3],interest[4,3],interest[5,3],interest[2,4],interest[3,4],interest[4,4],interest[5,4],interest[2,5],interest[3,5],interest[4,5],interest[5,5]))
gInterest<-ggplot(data=ginterest, aes(x=gender, y=Number, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Blues")

#future contribution likelihood vs gender
future_contribution <- subset(df,,select=c("FUTURE.CONTRIBUTION.INTEREST","FUTURE.CONTRIBUTION.LIKELIHOOD","GENDER"))
likelihood<-with(future_contribution,table(GENDER,FUTURE.CONTRIBUTION.LIKELIHOOD))

glikelihood <- data.frame(supp=rep(c("Somewhat likely", "Somewhat unlikely","Very likely"," Very unlikely"), each=4),gender=c("Man","Non-binary  or Other","Prefer not to say", "Woman "),
                        Number=c(likelihood[2,2],likelihood[3,2],likelihood[4,2],likelihood[5,2],likelihood[2,3],likelihood[3,3],likelihood[4,3],likelihood[5,3],likelihood[2,4],likelihood[3,4],likelihood[4,4],likelihood[5,4],likelihood[2,5],likelihood[3,5],likelihood[4,5],likelihood[5,5]))
gLikelihood<-ggplot(data=glikelihood, aes(x=gender, y=Number, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge()) 

#employment status vs gender
emp_gender <- subset(df, , select = c("GENDER","EMPLOYMENT.STATUS"))
g_emp_gender<-with(emp_gender,table(EMPLOYMENT.STATUS,GENDER))

G_emp_gender <- data.frame(supp=rep(c("Employed full time ", "Employed part time","Full time student","Other","Retired or permanently not working (e.g. due to disability)","Temporarily not working"), each=4),gender=c("Man","Non-binary  or Other","Prefer not to say", "Woman "),
                          Number=c(g_emp_gender[2,2],g_emp_gender[3,2],g_emp_gender[4,2],g_emp_gender[5,2],g_emp_gender[6,2],g_emp_gender[7,2],g_emp_gender[2,3],g_emp_gender[3,3],g_emp_gender[4,3],g_emp_gender[5,3],g_emp_gender[6,3],g_emp_gender[7,3],g_emp_gender[2,4],g_emp_gender[3,4],g_emp_gender[4,4],g_emp_gender[5,4],g_emp_gender[6,4],g_emp_gender[7,4],g_emp_gender[2,5],g_emp_gender[3,5],g_emp_gender[4,5],g_emp_gender[5,5],g_emp_gender[6,5],g_emp_gender[7,5]))
G_Emp_Gender<-ggplot(data=G_emp_gender, aes(x=gender, y=Number, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Blues")


