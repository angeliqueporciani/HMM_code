ACID16 <- modnull2_16$CIreal$AC$est
ACID23 <- modnull2_23$CIreal$AC$est
ACID34 <- modnull2_34$CIreal$AC$est
ACID63 <- modnull2_63$CIreal$AC$est
ACID115 <- modnull2115$CIreal$AC$est

AC_DF <- rbind(ACID16, ACID23,ACID34,ACID63,ACID115)

#trie des valeurs en croissant. 
for (i in 1:5){
  AC_DF[i,] <- sort(AC_DF[i,])
}

saveRDS(AC_DF,"./output/AC_DF_modinitialVS.rds")

class(AC_DF)
