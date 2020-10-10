library(ggplot2)
library(dplyr)
library(corrplot)
library(wordcloud2)
library(lubridate)
library(zoo)

df<-read.csv("NYC_Jobs_new.csv",header=TRUE,row.names = NULL, 
             stringsAsFactors = FALSE)
head(df)
dim(df)
#===========================
#Remove empty values in full time part time and posting date column residency
#===========================
table(df$Full.Time.Part.Time.indicator=="")
table(df$Posting.Date=="")
table(df$Residency.Requirement=="")
table(df$Salary.Range.From=="")
table(df$Salary.Range.To=="")
table(df$Salary.Frequency=="")

no_na_posting<-df[!(df$Full.Time.Part.Time.indicator==""),]
no_na_date<-no_na_posting[!(no_na_posting$Posting.Date==""),]
no_na_df<-no_na_date[!(no_na_date$Residency.Requirement==""),]

#=====================================
# Rename residency values
#====================================
select_status<-function(w){
  resident_status<-''
  if(grepl("Must be a resident", w)==TRUE | grepl("you must reside", w)==TRUE){
    resident_status<-paste("Must be resident")
    return (resident_status)
  }else{
      if(grepl("90", w)==TRUE){
        resident_status<-paste("Required within 90 days of appointment")
        return (resident_status)
      }else{
        resident_status<-paste("Not Required")
        return (resident_status)
      }
  }
}

no_na_df$Residency.Requirement<-sapply(no_na_df$Residency.Requirement,select_status)
#=====================================
#=============================
# Check min max date
#============================
min(as.Date(no_na_date$Posting.Date, format="%m/%d/%Y"))
max(as.Date(no_na_date$Posting.Date, format="%m/%d/%Y"))

#===================================

final_df<-no_na_df%>%select(1,2,3,4,5,6,8,9,10,11,12,13,15,24,27)
final_df<-final_df%>%mutate(year = year(as.Date(final_df$Posting.Updated, format="%m/%d/%Y")), 
                month = month(as.Date(final_df$Posting.Updated, format="%m/%d/%Y")))

monyr<-as.factor(paste(final_df$month,final_df$year,sep="/"))
final_df$date<-monyr
final_df<-final_df%>%mutate(quarter = as.yearqtr(final_df$Posting.Updated, format = "%m/%d/%Y"))

#==================================
#All position from 2012 to 2019
#==================================
position_by_level<-aggregate(final_df$X..Of.Positions, by=list(final_df$Level,final_df$year), FUN=sum)
position_by_level

total_position<-ggplot(position_by_level, aes(x=position_by_level$Group.2, y=position_by_level$x,group=position_by_level$Group.1, colour=position_by_level$Group.1))+geom_line(size=1)+
  labs(title = "Number of Vacancies by Skill Levels from 2012 - 2019", x ="Year", y= "Total vacancy")+
  scale_color_discrete(name = "Skill Levels")+
  scale_y_continuous(breaks = seq(0,700, by=100))+
  scale_x_continuous(labels = as.character(position_by_level$Group.2), breaks = position_by_level$Group.2)
total_position + theme(plot.title = element_text(hjust = 0.5),axis.title=element_text(size=14),  text = element_text(size=12))

#==================================
#Residency status requirement
#==================================
res_graph<-ggplot(final_df, aes(final_df$Residency.Requirement))+geom_bar()+
  labs(title="Residency Status Requirement", x="Status Requirement", y="Total Count")+
  scale_y_continuous(breaks = seq(0, 350, by = 50))
res_graph+theme(plot.title = element_text(hjust = 0.5),axis.title=element_text(size=16), axis.text=element_text(size=10))

#===================================
#===================================
# annual salary
#===================================
#===================================
annual_pay<-final_df[(final_df$Salary.Frequency=="Annual"),]

#Annual base salary by level
annual_base<-ggplot(annual_pay, aes(x=annual_pay$Level, y=annual_pay$Salary.Range.From))+geom_boxplot()+
  labs(title="Annual Base salary by different Levels", x ="Civil Service Levels", y= "Base Salary")+
  scale_y_continuous(breaks = seq(0, 200000, by = 20000))
annual_base+theme(plot.title = element_text(hjust = 0.5),axis.title=element_text(size=16), axis.text=element_text(size=10))



annual_graph<- ggplot(annual_pay, aes(x=annual_pay$Salary.Range.From, y=annual_pay$Salary.Range.To, color=ifelse(annual_pay$Business.Title=="OIL BURNER SPECIALIST","seagreen","black")))+
  geom_point(size = ifelse(annual_pay$Business.Title=="OIL BURNER SPECIALIST",4,1.5))+
  labs(title = "Annual Salary Rate", x="Base Salary", y ="Max Salary")+
  scale_y_continuous(breaks = seq(0,240000, by=20000))+
  scale_x_continuous(breaks = seq(0,220000, by=20000))+
  geom_text(aes(label=ifelse(annual_pay$Business.Title=="OIL BURNER SPECIALIST",as.character("OIL BURNER SPECIALIST"),'')),hjust=-0.35,vjust=0)
annual_graph+theme(plot.title = element_text(hjust = 0.5),axis.title=element_text(size=16),
                     axis.text=element_text(size=12),
                     legend.position = "none")


#===================================
#===================================
# Daily salary
#===================================
#===================================
daily_pay<-final_df[(final_df$Salary.Frequency=="Daily"),]

daily_graph<-ggplot(daily_pay, aes(x=daily_pay$Salary.Range.From, y=daily_pay$Salary.Range.To, color=ifelse(daily_pay$Business.Title=="Sewage Treatment Worker","seagreen","black")))+
  geom_point(size = ifelse(daily_pay$Business.Title=="Sewage Treatment Worker",4,2))+
  labs(title = "Daily Salary Rate", x="Base Salary", y ="Max Salary")+
  scale_y_continuous(breaks = seq(0,400, by=5))+
  scale_x_continuous(breaks = seq(0,400, by=5))+
  geom_text(aes(label=ifelse(daily_pay$Business.Title=="Sewage Treatment Worker",as.character("Sewage Treatment Worker"),'')),hjust=-0.05,vjust=0)
daily_graph+theme(plot.title = element_text(hjust = 0.5),axis.title=element_text(size=16),
                    axis.text=element_text(size=12),
                    legend.position = "none")

#===================================
#===================================
# Hourly salary
#===================================
#===================================
hourly_pay<-final_df[(final_df$Salary.Frequency=="Hourly"),]

hourly_graph<-ggplot(hourly_pay, aes(x=hourly_pay$Salary.Range.From, y=hourly_pay$Salary.Range.To, color=ifelse(hourly_pay$Business.Title=="Seasonal City Park Worker","seagreen","black")))+
  geom_point(size = ifelse(hourly_pay$Business.Title=="Seasonal City Park Worker",4,2))+
  labs(title = "Hourly Salary Rate", x="Base Salary", y ="Max Salary")+
  scale_y_continuous(breaks = seq(0,60, by=10))+
  scale_x_continuous(breaks = seq(0,60, by=10))+
  geom_text(aes(label=ifelse(hourly_pay$Business.Title=="Seasonal City Park Worker",as.character("Seasonal City Park Worker"),'')),hjust=-0.05,vjust=0)
hourly_graph+theme(plot.title = element_text(hjust = 0.5),axis.title=element_text(size=16),
                     axis.text=element_text(size=12),
                     legend.position = "none")




#===================================
# Most vacancy job category
#===================================
#===================================
all_jobs<-aggregate(final_df$X..Of.Positions, by=list(final_df$Job.Category), FUN=sum)
all_jobs <- all_jobs[order(all_jobs$x), ]
all_jobs
wordcloud2(all_jobs,size=0.2,color = "random-light", backgroundColor = "black")

#===================================
# Building Operations & Maintenance
#===================================
#===================================
building<-level_0[(level_0$Job.Category=="Building Operations & Maintenance"),]

pos_building<-aggregate(building$X..Of.Positions, by=list(building$Business.Title), FUN=sum)
pos_building <- pos_building[order(pos_building$x), ]
pos_building

ggplot(pos_building, aes(x=reorder(pos_building$Group.1,pos_building$x), y=pos_building$x)) +
  labs(title = "Building Operations & Maintenance Jobs with Total vacancy", y ="Total vacancy")+
  geom_bar(stat = "identity", width=0.5) +
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5),axis.title=element_text(size=16),
        axis.text=element_text(size=12),
        axis.title.y=element_blank())+
  scale_y_continuous(breaks = seq(0, 200, by = 20))


building_over_10<-building[(building$X..Of.Positions>10),]

ggplot(building, aes(x=building$Business.Title,y=building$X..Of.Positions, fill=building$Posting.Type))+geom_bar(stat="identity")


#=================================
#Level 0 annual
#==================================
annual_0<-annual_pay[(annual_pay$Level==0),]
annual_0_graph<- ggplot(annual_0, aes(x=annual_0$Salary.Range.From, y=annual_0$Salary.Range.To, color=ifelse(annual_0$Business.Title=="OIL BURNER SPECIALIST","seagreen","black")))+
  geom_point(size = ifelse(annual_0$Business.Title=="OIL BURNER SPECIALIST",4,2))+
  labs(title = "Civil Serivce Level 0 Annually rate", x="Base Salary", y ="Max Salary")+
  geom_text(aes(label=ifelse(annual_0$Business.Title=="OIL BURNER SPECIALIST",as.character("OIL BURNER SPECIALIST"),'')),hjust=-0.05,vjust=0)
annual_0_graph+theme(plot.title = element_text(hjust = 0.5),axis.title=element_text(size=16),
                     axis.text=element_text(size=12),
                     legend.position = "none")


#=================================
#Level 0 hourly
#==================================
hourly_0<-hourly_pay[(hourly_pay$Level==0),]
hourly_0_graph<-ggplot(hourly_0, aes(x=hourly_0$Salary.Range.From, y=hourly_0$Salary.Range.To, color=ifelse(hourly_0$Business.Title=="Seasonal City Park Worker","seagreen","black")))+
  geom_point(size = ifelse(hourly_0$Business.Title=="Seasonal City Park Worker",4,2))+
  labs(title = "Hourly rate Jobs", x="Base Salary", y ="Max Salary")+
  geom_text(aes(label=ifelse(hourly_0$Business.Title=="Seasonal City Park Worker",as.character("Seasonal City Park Worker"),'')),hjust=-0.05,vjust=0)
hourly_0_graph+theme(plot.title = element_text(hjust = 0.5),axis.title=element_text(size=16),
        axis.text=element_text(size=12),
        legend.position = "none")

#=================================
#Level 0 daily
#==================================
daily_0<-daily_pay[(daily_pay$Level==0),]
daily_0_graph<-ggplot(daily_0, aes(x=daily_0$Salary.Range.From, y=daily_0$Salary.Range.To, color=ifelse(daily_0$Business.Title=="Stationary Engineer (Electric)","seagreen","black")))+
  geom_point(size = ifelse(daily_0$Business.Title=="Stationary Engineer (Electric)",4,2))+
  labs(title = "Hourly rate Jobs", x="Base Salary", y ="Max Salary")+
  geom_text(aes(label=ifelse(daily_0$Business.Title=="Stationary Engineer (Electric)",as.character("Stationary Engineer (Electric)"),'')),hjust=0,vjust=0)
daily_0_graph+theme(plot.title = element_text(hjust = 0.5),axis.title=element_text(size=12),
                     axis.text=element_text(size=12),
                     legend.position = "none")



#=================================
#Skill Level
#==================================
posting<-ggplot(final_df, aes(final_df$Level, fill=final_df$Residency.Requirement))+geom_bar()+labs(title = "Number of Postings by Residency Requirement Status", x ="Levels", y= "Total Postings",fill="Residency Requirement")+scale_y_continuous(breaks=seq(0, 300, by=50))
posting+theme(plot.title = element_text(hjust = 0.5),axis.title=element_text(size=16), axis.text=element_text(size=10))
