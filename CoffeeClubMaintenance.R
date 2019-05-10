#--
#   package management:
#     provide automated means for first time use of script to automatically 
#	  install any new packages required for this code, with library calls 
#	  wrapped in a for loop.
#--
pkgChecker <- function(x){
  for( i in x ){
    if( ! require( i , character.only = TRUE ) ){
      install.packages( i , dependencies = TRUE )
      require( i , character.only = TRUE )
    }
  }
}
list.of.packages <-  c("editData","XLConnect","dplyr", "ggplot2","readxl",
                       "timeDate")

suppressWarnings(pkgChecker(list.of.packages))


CCDues <- read_excel("h:/Docs/Personal/CoffeeClubDuesByPivot.xlsm",sheet = "LineItems")
CCDues<- CCDues[with(CCDues,order(-DisplayGraph,-xtfrm(Date),Source)),c(1:5)]
CCDues<- CCDues[CCDues$DisplayGraph==TRUE ,]
#
# create records for all unPaid membership dues
# and  members status list
#

# Vectorize coerces a function "timeLastDayInMonth" to work with a list
# timeLastDayInMonth function calculates the last day of the month a date falls in
eom <-(Vectorize(timeLastDayInMonth))(seq(as.Date(Sys.Date()-365), length=24, by="1 month") )

# eom[12] is last month
# eom[13] is this month
monIndex = 12
# deduct a month if past due processing
if ((format(as.Date(Sys.Date()),"%m")< format(as.Date(eom[[monIndex+2]]),"%m")) |
    (format(as.Date(Sys.Date()),"%m")>= "12"))
    {
      monIndex= 11
      }
# Members From 2 month ago:
MmbrLast2Mon<-subset(CCDues,as.Date(Date) == as.Date(eom[[monIndex]]))
CurrMmbrs<-MmbrLast2Mon[MmbrLast2Mon$Source == 'Membership',]
# Members from last month
MmbrLastMon<-subset(CCDues,as.Date(Date) == as.Date((eom[[monIndex+1]])))

# Combine current member lists
CurrMmbrs<-rbind(CurrMmbrs,MmbrLastMon[MmbrLastMon$Source == 'Membership' ,])

CurrMmbrs<-aggregate(as.Date(CurrMmbrs$Date)~.,CurrMmbrs,FUN="max")
colnames(CurrMmbrs) <- c('Name','Source','DisplayGraph','Paid','Date')

# Members already paid up for the month
PaidUp <-subset(CCDues,as.Date(Date) == as.Date(eom[[monIndex+2]]))

# Determine who is not paid for this month and assign records $0.00 paid for This eom
unPaid<- subset(CurrMmbrs,!(CurrMmbrs$Name %in% PaidUp$Name))
if (nrow(unPaid) > 0){
unPaid$Date<-as.Date((eom[[monIndex+2]]))
unPaid$Paid<-0.00
}
# Determine last month Membership dues are paid until
#latest <- aggregate(CCDues$Date, list(CCDues$Name,CCDues$Source,CCDues$DisplayGraph, CCDues$Paid),max)
latest <- aggregate(as.Date(CCDues$Date)~., CCDues,FUN="max")

colnames(latest) <- c('Name','Source','DisplayGraph','Paid','Date')
latest$Date <- as.Date(latest$Date)

paidUntil <-subset(latest,as.Date(latest$Date) >= Sys.Date() & latest$Source=='Membership')
paidUntil<-rbind(subset(CurrMmbrs,!(CurrMmbrs$Name %in% PaidUp$Name)),paidUntil)
paidUntil$Date <- as.Date(paidUntil$Date)

# Save memberlist for CoffeeClub Excel workbook
paidUntil$DisplayGraph <- NULL
write.csv(paidUntil,"H:/Docs/Personal/Memberlist.csv")

#
# Create Graph for Past Year's Income and Expense
#

# CCDues <- rbind(CCDues,unPaid)
# CCDues<- CCDues[with(CCDues,order(-DisplayGraph,-xtfrm(Date),Source)),]
# 
# CCdata<-subset(CCDues,  as.Date(Date) > as.Date(eom[[1]])
#                & as.Date(Date) < as.Date(eom[[monIndex+2]]))
# CCdata$Date <- as.Date(CCdata$Date)
# reserves <-sum(CCDues$Paid)
# 
# 
# library(scales)
# library(timeDate)
# base<-ggplot(CCdata, aes(x=Date,y=Paid,fill=Source)) + geom_col() 
#   base <- base + scale_x_date(date_labels = "%m-%y",date_breaks ="4 week")
#   base + ggtitle("B2-2N The 'Good Coffee' Club",
#                  subtitle=paste('Monthly Income and Expense Summary\nCash Reserves = $',
#                                 formatC(as.numeric(reserves), format="f", digits =2)))
# 
