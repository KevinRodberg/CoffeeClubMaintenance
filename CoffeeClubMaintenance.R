
library(editData)
require(XLConnect)
library(readxl)
library(ggplot2)
library(timeDate)

CCDues <- read_excel("~/Personal/CoffeeClubDuesByPivot.xlsx",sheet = "LineItemAcct")
CCDues<- CCDues[with(CCDues,order(-DisplayGraph,-xtfrm(Date),Source)),]

#
# create records for all unPaid membership dues
# and  members status list
#

# Vectorize coerces a function "timeLastDayInMonth" to work with a list
# timeLastDayInMonth function calculates the last day of the month a date falls in
eom <-(Vectorize(timeLastDayInMonth))(seq(as.Date(Sys.Date()-365), length=24, by="1 month") )

# eom[12] is last month
# eom[13] is this month
MmbrLast2Mon<-subset(CCDues,as.Date(Date) == as.Date((eom[[11]])))
MmbrLastMon<-subset(CCDues,as.Date(Date) == as.Date((eom[[12]])))
paidThisMonth <-subset(CCDues,as.Date(Date) == as.Date(eom[[13]]))

MmbrThisMon<-MmbrLast2Mon[MmbrLast2Mon$Source == 'Membership' ,]

MmbrThisMon<-rbind(MmbrThisMon,MmbrLastMon[MmbrLastMon$Source == 'Membership' ,])
MmbrThisMon<-aggregate(MmbrThisMon$Date,list(MmbrThisMon$Name,MmbrThisMon$Source,MmbrThisMon$DisplayGraph,MmbrThisMon$Paid),max)
colnames(MmbrThisMon) <- c('Name','Source','DisplayGraph','Paid','Date')
unPaidMmbrThisMon<- subset(MmbrThisMon,!(MmbrThisMon$Name %in% paidThisMonth$Name))
unPaidMmbrThisMon$Date<-as.Date((eom[[13]]))
unPaidMmbrThisMon$Paid<-0.00
MmbrThisMonStatus <-rbind(paidThisMonth[paidThisMonth$Source == 'Membership' ,]
                          ,unPaidMmbrThisMon[unPaidMmbrThisMon$Source == 'Membership' ,])
MemberList <-MmbrThisMonStatus[,c('Name','Source','Paid','Date')]

latest <- aggregate(CCDues$Date, list(CCDues$Name,CCDues$Source,CCDues$DisplayGraph, CCDues$Paid),max)
colnames(latest) <- c('Name','Source','DisplayGraph','Paid','Date')
latest$Date <- as.Date(latest$Date)
paidUntil <-subset(latest,as.Date(latest$Date) > Sys.Date() & latest$Source=='Membership')
paidUntil<-rbind(subset(MmbrThisMon,!(MmbrThisMon$Name %in% paidThisMonth$Name)),paidUntil)
paidUntil$Date <- as.Date(paidUntil$Date)
paidUntil$DisplayGraph <- NULL
write.csv(paidUntil,"~/Personal/Memberlist.csv")

CCDues <- rbind(CCDues,unPaidMmbrThisMon)


CCDues<- CCDues[with(CCDues,order(-DisplayGraph,-xtfrm(Date),Source)),]

#
# Create Graph for Past Year's Income and Expense
#
CCdata<-subset(CCDues,  as.Date(Date) > as.Date(eom[[1]])
               & as.Date(Date) < as.Date(eom[[14]]))
CCdata$Date <- as.Date(CCdata$Date)
reserves <-sum(CCDues$Paid)
library(scales)
library(timeDate)
base<-ggplot(CCdata, aes(x=Date,y=Paid,fill=Source)) + geom_col() 
  base <- base + scale_x_date(date_labels = "%m-%y",date_breaks ="4 week")
  base + ggtitle("B2-2N The 'Good Coffee' Club",
                 subtitle=paste('Monthly Income and Expense Summary\nCash Reserves = $',
                                formatC(as.numeric(reserves), format="f", digits =2)))

