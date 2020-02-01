#Εργασία 1
########################################################3
#Κάτω από κάθε ερώτηση να τοποθετήσετε το κώδικα-απάντηση της αντίστοιχης ερώτησης
#Μπορείτε για κάθε απάντηση να χρησιμοποιήσετε οποιοδήποτε μοτίβο κώδικα έχετε διδαχθεί
#An den emfanizontai sosta ta ellinika epilegetai apo to menu tools->global options->code->saving->default code encoding->utf-8
#epeita epilegetai apply kleinete to arxeio kai to ksanaanoigete

#Να υπολογίσετε και να εμφανίσετε τις απαντήσεις για κάθε ένα από τα παρακάτω ερωτήματα
         
          mydata = read.csv("DelayedFlights.csv")

#Ερώτηση 1:να βρείτε (αν υπάρχουν) και να εμφανίσετε το πλήθος των κενών γραμμών σε κάθε στήλη του dataset
         
          colSums(is.na(mydata))

#Ερώτηση 2:να υπολογίσετε και να εμφανίσετε ποια ημέρα σε ποιον μήνα σημειώθηκαν οι περισσότερες καθυστερήσεις πτήσεων
          
          for (i in 1:12)
          for (j in 1:31)
          info[i, j] <- nrow(subset(mydata, Month == i & DayofMonth == j))
          result = which(info==max(info),  arr.ind=TRUE)
          print("Month/Day")
          print(result)

#Ερώτηση 3: να υπολογίσετε και να εμφανίσετε τον ημερήσιο μέσο όρο καθυστερήσεων για καθέναν από τους θερινούς μήνες του 2008
          
          Summer = matrix(ncol=3)
          Summer[,1] = Summer[,2] = Summer[,3] = 0
          for (i in 6:8)
          for (j in 1:31)
          Summer[,i-5] = Summer[,i-5] + info[i,j]
          Summer[,1]=Summer[,1]/30
          Summer[,2]=Summer[,2]/31
          Summer[,3]=Summer[,3]/31
          print("June/July/August")
          print(Summer) 

#Ερώτηση 4: να υπολογίσετε και να εμφανίσετε το όνομα της αεροπορικής εταιρίας που είχε το μεγαλύτερο πλήθος κωδικών ακύρωσης τύπου Β
          
          codeb <- subset(mydata, CancellationCode=='B')
          table(codeb$UniqueCarrier)
          max(table(codeb$UniqueCarrier))

#Ερώτηση 5: να βρείτε τους κωδικούς των πτήσεων με τον μεγαλύτερο αριθμό καθυστερήσεων
         
          delay <- subset(mydata, ArrDelay > 0)
          table(delay$FlightNum)
          max(table(delay$FlightNum))

#Ερώτηση 6: να βρείτε και να υπολογίσετε το όνομα του μεγαλύτερου σε απόσταση προορισμού με τις περισσότερες καθυστερήσεις
          
          distance <- max(mydata$Distance)
          dest <- subset(mydata, Distance == distance)
          k=0
          l = length(table(dest$Dest))
          for (i in 1:l)
            if (table(dest$Dest)[i] == max(table(dest$Dest))) k=i
          names(table(dest$Dest))[k]

#Ερώτηση 7: να βρείτε και να εμφανίσετε τους προορισμούς που είχαν την μεγαλύτερη καθυστέρηση (πτήσεις που εκτελέστηκαν)

          code = subset(mydata, CancellationCode == 'N')
          for (i in 1:nrow(code))
             if (code$ArrDelay[i] == max(code$ArrDelay)) 
                 code$Dest[i]


#Ερώτηση 8: να βρείτε και να εμφανίσετε το όνομα της αεροπορικής εταιρείας που είχε τις μεγαλύτερες καθυστερήσεις που οφείλονται 
#             σε καθυστερημένη άφιξη αεροσκαφών

          air = subset(mydata, LateAircraftDelay >= 0)
          l = length(table(air$UniqueCarrier))
          k=0
          for (i in 1:l)
            if (table(air$UniqueCarrier)[i] == max(table(air$UniqueCarrier))) k=i
          names(table(air$UniqueCarrier))[k]
  

#Ερώτηση 9: να υπολογίσετε πόσες ακυρώσεις πτήσεων τύπου Α σημειώθηκαν την 13η ημέρα κάθε μήνα

          canc = subset(mydata, CancellationCode == 'A')
          canc13 = subset(canc, DayofMonth == 13)
          table(canc13$Month)


#Ερώτηση 10: υπολογίσετε και να εμφανίσετε την μέση καθυστέρηση πτήσεων που εκτελέστηκαν από την 10η μέχρι την 23 Απριλίου 2008
        
          ten = subset(mydata, Month==4 & DayofMonth >=10 & DayofMonth <= 23)
          ten2 = subset(ten, ArrDelay >0)
          mean(table(ten2$ArrDelay))

#Ερώτηση 11: να υπολογίσετε και να εμφανίσετε τον μήνα που σημειώθηκε η μεγαλύτερη καθυστέρηση που οφειλόταν σε έλεγχους 
#             ασφαλείας κατά τις ώρες 06.00-14.00

          security = subset(mydata, SecurityDelay > 0 & DepTime>=600 & DepTime <=1400)
          k=0
          for (i in 1:nrow(security))
             if (security$SecurityDelay[i]==max(security$SecurityDelay)) k=i
          security$Month[k]

#Ερώτηση 12: να υπολογίσετε και να εμφανίσετε ποιος κωδικός πτήσης(αριθμός πτήσης) είχε το πρώτο δεκαήμερο 
#              του Νοεμβρίου του 2008 την μεγαλύτερη προ του αναμενόμενου χρόνου άφιξη στον προορισμό της

          nov = subset(mydata, ArrDelay < 0 & Month == 11 & DayofMonth >=1 & DayofMonth <=10)
          k=0
          for (i in 1:nrow(nov))
             if (nov$ArrDelay[i]==min(nov$ArrDelay)) k=i
          nov$FlightNum[k]


#Ερώτηση 13: να υπολογίσετε και να εμφανίσετε ποιο αεροδρόμιο (τοποθεσία αναχώρησης) είχε το δεύτερο δεκαήμερο του Αυγούστου 2018 
#             τις περισσότερες πτήσεις με καθυστέρηση(αναχωρίσεων) μεγαλύτερη από μισή ώρα που οφείλονται στους αερομεταφορείς

          aug = subset(mydata, Month==8 & DayofMonth >=11 & DayofMonth <= 20 & CarrierDelay > 0 & DepDelay > 30)
          k=0
          for(i in 1:length(table(aug$Origin)))
             if (table(aug$Origin)[i]==max(table(aug$Origin))) k=i
          names(table(aug$Origin))[k]



#Ερώτηση 14: να βρείτε και να εμφανίσετε τις πτήσεις που εκτράπηκαν από την πορεία τους αλλά ολοκληρώθηκαν καθώς 
#             και τον συνολικό χρόνο που απαιτήθηκε
          
          nas = subset(mydata, NASDelay > 0)
          print(nas$FlightNum)
          print(nas$NASDelay)

#Ερώτηση 15: ποιος μήνας είχε την μεγαλύτερη τυπική απόκλιση σε καθυστερήσεις ("πιο απρόβλεπτος μήνας"). Ως απόκλιση να θεωρηθεί 
#             η διαφορά ανάμεσα στον προγραμματισμένο και τον πραγματικό χρόνο εκτέλεσης της πτήσης


      max = 0
      k = 0 
      for (i in 1:nrow(mydata))
        if ((mydata$CRSElapsedTime[i] - mydata$ActualElapsedTime[i]) > max)
          max = mydata$CRSElapsedTime[i] - mydata$ActualElapsedTime[i]
          k = i
      print("Η μεγαλύτερη Τυπική απόκλιση συνέβη τον μήνα: ")
      mydata$Month[k] 
         




