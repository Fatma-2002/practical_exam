pometry<-read.csv("G2_pometrypometry.csv")
View(pometry)

#1 rows have NA
pometry[!complete.cases(pometry) , ]

#2 Replace each NA in foot_length according to the mean of
##foot_length to all males for males and foot_length to all females for females

m_mean<-mean(pometry[pometry$Sex=="Male" , 'foot_length' ], na.rm=T)
pometry[is.na(pometry$foot_length) & pometry$Sex=="Male"  , 'foot_length']=m_mean

f_mean<-mean(pometry[pometry$Sex=="Female" , 'foot_length' ], na.rm=T)
pometry[is.na(pometry$foot_length) & pometry$Sex=="Female"  , 'foot_length']=f_mean

#3 Rename the gender feature to Male and Female
pometry$Gender[pometry$gender=="F" ]="Female"
pometry$Gender[pometry$gender=="cm"]="Male"

#4 Replace age variable by interval of age
pometry$ageinterval[pometry$age <= 5]="0 _ 5"
pometry$ageinterval[pometry$age > 5 & pometry$age <= 10]="6 _ 10"
pometry$ageinterval[pometry$age >10 ]="11 _ .."

#5 remove the text(cm)in height feature to convert it to numeric to use in the analysis

pometry$height<-gsub(" cm" ,"" , pometry$height)
pometry$height<-as.numeric(pometry$height)


#6 Re_code the height feature using if else

x<-mean(pometry$height)

pometry$heightSEC<-as.factor(ifelse(pometry$height< x & pometry$age >10,"Abnormal kid" 
                                    ,"Normal kid"))
#7 Re_code of code

pometry$heightSEC2[pometry$heightSEC=="Normal kid"]='0'
pometry$heightSEC2[pometry$heightSEC=="Abnormal kid"]='1'
pometry$heightSEC2<-as.factor(pometry$heightSEC2)

#8 Display The ratio of normal and abnormal child 

a<-mean(pometry$heightSEC2==0)
b<-mean(pometry$heightSEC2==1)
a
b

#9 Subset only males who have foot_length greater than  200

sub1<-pometry[pometry$Sex=="Male" & pometry$foot_length >200 , ]

#10 Subset abnormal childs

sub2<-pometry[pometry$heightCat2== 1 , ]

#11 Subset childs who have foot_length greater than the median and have height 
#greater than or equal to 135  for specific features(age , sex)

sub3<-pometry[pometry$foot_length > median(pometry$foot_length) &
                pometry$height >= 135 ,c(1,5) ]

#12 sort the data set ascending according to 2 variables

sorted<-pometry[order(pometry$age ,pometry$height) , ]

#13 Get only the first 20 rows
h<-head(pometry ,20)

#14 Get only the last 20 rows
t<-tail(pometry ,20)

library(ggplot2)

#15 display the effect of the height on foot_length (co_relation) 
#using scatter plot,name the figure

fig1<-ggplot(pometry , aes(x=foot_length  , y= height))
fig1 + geom_point() + ggtitle("The co_relation between the Height and Foot_length")

#16 display the effect of height on foot_length colored by 
#the groups of age range using scatterplot
fig2<-ggplot(pometry , aes(foot_length , height))
fig2 + geom_point(aes(color=ageRange)) +stat_smooth(se=FALSE)  


#17 Show the distribution of footlength using histogram,name the figure and rename the x,y
fig3<-ggplot(pometry , aes(foot_length))
fig3 + geom_histogram(binwidth = 8)
fig3 + geom_histogram(fill = "orange")+ ggtitle("Child's foot length distribution")+labs(x="Foot Length" , y="Frequency")

#18 Show the distribution of Height using histogram ,name the figure
fig4<-ggplot(pometry , aes(height))
fig4 + geom_histogram(binwidth = 8)
fig4 + geom_histogram(fill = "red")+ ggtitle("Child's Height distribution") 

#19 summarize the heightcat2 0,1 to Sex and ageRange groups using Bar chart

fig5<-ggplot(pometry , aes(x=heightSEC2  ,fill= Sex))
fig5 +geom_bar()+labs(y=" Heightsec count" ,title="Height rate")
fig5 +geom_bar() +theme_light()+facet_wrap(~ageRange)
