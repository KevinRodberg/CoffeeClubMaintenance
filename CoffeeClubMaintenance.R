
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

# Members From 2 month ago:
MmbrLast2Mon<-subset(CCDues,as.Date(Date) == as.Date((eom[[11]])))
CurrMmbrs<-MmbrLast2Mon[MmbrLast2Mon$Source == 'Membership' ,]

# Members from last month
MmbrLastMon<-subset(CCDues,as.Date(Date) == as.Date((eom[[12]])))

# Combine current member lists
CurrMmbrs<-rbind(CurrMmbrs,MmbrLastMon[MmbrLastMon$Source == 'Membership' ,])
CurrMmbrs<-aggregate(CurrMmbrs$Date,list(CurrMmbrs$Name,CurrMmbrs$Source,CurrMmbrs$DisplayGraph,CurrMmbrs$Paid),max)
colnames(CurrMmbrs) <- c('Name','Source','DisplayGraph','Paid','Date')

# Members already paid up for the month
PaidUp <-subset(CCDues,as.Date(Date) == as.Date(eom[[13]]))

# Determine who is not paid for this month and assign records $0.00 paid for This eom
unPaid<- subset(CurrMmbrs,!(CurrMmbrs$Name %in% PaidUp$Name))
unPaid$Date<-as.Date((eom[[13]]))
unPaid$Paid<-0.00

# Determine last month Membership dues are paid until
latest <- aggregate(CCDues$Date, list(CCDues$Name,CCDues$Source,CCDues$DisplayGraph, CCDues$Paid),max)
colnames(latest) <- c('Name','Source','DisplayGraph','Paid','Date')
latest$Date <- as.Date(latest$Date)

paidUntil <-subset(latest,as.Date(latest$Date) > Sys.Date() & latest$Source=='Membership')
paidUntil<-rbind(subset(CurrMmbrs,!(CurrMmbrs$Name %in% PaidUp$Name)),paidUntil)
paidUntil$Date <- as.Date(paidUntil$Date)

# Save memberlist for CoffeeClub Excel workbook
paidUntil$DisplayGraph <- NULL
write.csv(paidUntil,"~/Personal/Memberlist.csv")

#
# Create Graph for Past Year's Income and Expense
#

CCDues <- rbind(CCDues,unPaid)
CCDues<- CCDues[with(CCDues,order(-DisplayGraph,-xtfrm(Date),Source)),]

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

