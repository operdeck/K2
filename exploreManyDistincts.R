# explore VAR_0404 and VAR_0493

d1 <- train$VAR_0404
d2 <- train$VAR_0493

u1 <- unique(d1)
u2 <- unique(d2)

match <- function(x, patterns) 
{
  r <- rep(FALSE, length(x))
  for (p in patterns) {
    r <- r | grepl(p, x, ignore.case=T)
  }
  return(r)
}

# new variables
data <- ifelse(d1 == "-1", d2, ifelse(d2 == "-1", d1, paste(d1, d2)))

# a better classification of these would be in order - some open source taxonomy of jobs??
isExecutive <- match( data, 
                     c("DIRECTOR", "PRESIDENT", "CEO", "MANAGER", "CHIEF EXECUTIVE OFFICER",
                       "BOARD MEMBER", "CFO", "CHIEF FINANCIAL OFFICER", "MANAGING MEMBER",
                       "VP", "CHAIRMAN", "MANAG") )
isEntrepeneur <- match( data, c("INDIVIDUAL - SOLE OWNER", "OWNER", "FOUNDER") )
isMedical <- match( data, c("MEDICAL ASSISTANT", "PHARMACY TECHNICIAN", "NURSE", "NURSING", 
                            "THERAPIST", "MEDICATION", "DENTAL",
                            "HYGIENIST", "BARBER", "MANICURIST", "PHARMACIST", "COSMETOLOGIST") )
isFinancial <- match( data, c("TREASURER","REGISTRANT","INSURANCE","TAX","LEGAL","ACCOUNTANT"))
isAssistant  <- match( data, c("SECRETARY","ASSISTANT"))
isOfficer  <- match( data, c("OFFICER"))
isLegal  <- match( data, c("ATTORNEY", "LAW", "LEGAL"))
isSocial  <- match( data, c("SOCIAL","COUNSELOR"))
isTech  <- match( data, c("TECH","ELECTRICIANS"))
isRealEstate <- match( data, c("REAL ESTATE","MORTGAGE"))
isMissing  <- match( data, c("-1","OTHER","TITLE NOT SPECIFIED")) | is.na(data) | data == "" | data == " "

# most frequent symbols
print("Most frequent:")
print(head( arrange(as.data.frame(table(data)), desc(Freq)),20 ))

print("Not matched:")
matched <- data[ which(!isExecutive & !isSmallBusiness & !isEntrepeneur & 
                       !isMedical & !isFinancial & !isAssistant & !isMissing &
                         !isOfficer & !isSocial & !isTech & !isRealEstate) ]
print(head( arrange(as.data.frame(table(matched)), desc(Freq)),50 ))

