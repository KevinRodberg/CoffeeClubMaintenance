
library(editData)
require(XLConnect)
library(readxl)
library(ggplot2)
library(timeDate)

CCDues <- read_excel("~/Personal/CoffeeClubDuesByPivot.xlsx",sheet = "LineItemAcct")
CCDues<- CCDues[with(CCDues,order(-DisplayGraph,-xtfrm(Date),Source)),]

#
# create records for all unPaid membership dues
# and update members status list
#

# Vectorize coerces a function "timeLastDayInMonth" to work with a list
# timeLastDayInMonth function calculates the last day of the month a date falls in
eom <-(Vectorize(timeLastDayInMonth))(seq(as.Date(Sys.Date()-365), length=24, by="1 month") )

# eom[12] is last month
# eom[13] is this month
MmbrLastMon<-subset(CCDues,as.Date(Date) == as.Date((eom[[12]])))
paidThisMonth <-subset(CCDues,as.Date(Date) == as.Date(eom[[13]]))

MmbrThisMon<-MmbrLastMon[MmbrLastMon$Source == 'Membership' ,]
               
unPaidMmbrThisMon<- subset(MmbrThisMon,!(MmbrThisMon$Name %in% paidThisMonth$Name))
unPaidMmbrThisMon$Date<-as.Date("2018-04-30")
unPaidMmbrThisMon$Paid<-0.00
MmbrThisMonStatus <-rbind(paidThisMonth[paidThisMonth$Source == 'Membership' ,]
                          ,unPaidMmbrThisMon[unPaidMmbrThisMon$Source == 'Membership' ,])
MemberList <-MmbrThisMonStatus[,c('Name','Date','Paid')]
CCDues <- rbind(CCDues,unPaidMmbrThisMon)
latest <- aggregate(CCDues$Date, list(CCDues$Name,CCDues$Source,CCDues$Paid),max)
colnames(latest) <- c('Name','Source','Paid','Date')
latest$Date <- as.Date(latest$Date)
paidUntil <-subset(latest,as.Date(latest$Date) > Sys.Date() & latest$Source=='Membership')
write.csv(paidUntil,"~/Personal/Memberlist.csv")

CCDues<- CCDues[with(CCDues,order(-DisplayGraph,-xtfrm(Date),Source)),]

#
# Create Graph for Past Year's Income and Expense
#
CCdata<-subset(CCDues,  as.Date(Date) > as.Date(Sys.Date())-365 
               & as.Date(Date) < as.Date(Sys.Date())+30)
CCdata$Date <- as.Date(CCdata$Date)
reserves <-sum(CCDues$Paid)
library(scales)
library(timeDate)
base<-ggplot(CCdata, aes(x=Date,y=Paid,fill=Source)) + geom_col() 
  base <- base + scale_x_date(date_labels = "%m-%y",date_breaks ="4 week")
  base + ggtitle("B2-2N The 'Good Coffee' Club",
                 subtitle=paste('Monthly Income and Expense Summary\nCash Reserves = $',
                                formatC(as.numeric(reserves), format="f", digits =2)))

