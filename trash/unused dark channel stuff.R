
# After dehazing
# Note no mNDHI (as it's also an index of haze)
rast_tmp_masked$NDWI_dehazed <- (rast_tmp_masked$B3_dehazed-rast_tmp_masked$B8)/(rast_tmp_masked$B3_dehazed+rast_tmp_masked$B8)
rast_tmp_masked$NDCI_dehazed <- (rast_tmp_masked$B5-rast_tmp_masked$B4_dehazed)/(rast_tmp_masked$B5+rast_tmp_masked$B4_dehazed)
rast_tmp_masked$NDTI_dehazed <- (rast_tmp_masked$B4_dehazed-rast_tmp_masked$B3_dehazed)/(rast_tmp_masked$B4_dehazed+rast_tmp_masked$B3_dehazed)
rast_tmp_masked$MNDWI_dehazed <- (rast_tmp_masked$B3_dehazed-rast_tmp_masked$B11)/(rast_tmp_masked$B3_dehazed+rast_tmp_masked$B11)
rast_tmp_masked$NDVI_dehazed <- (rast_tmp_masked$B8-rast_tmp_masked$B4_dehazed)/(rast_tmp_masked$B8+rast_tmp_masked$B4_dehazed)

# After normalization
rast_tmp_masked$NDWI_normalised <- (rast_tmp_masked$B3_normalised-rast_tmp_masked$B8)/(rast_tmp_masked$B3_normalised+rast_tmp_masked$B8)
rast_tmp_masked$NDCI_normalised <- (rast_tmp_masked$B5_normalised-rast_tmp_masked$B4_normalised)/(rast_tmp_masked$B5_normalised+rast_tmp_masked$B4_normalised)
rast_tmp_masked$NDTI_normalised <- (rast_tmp_masked$B4_normalised-rast_tmp_masked$B3_normalised)/(rast_tmp_masked$B4_normalised+rast_tmp_masked$B3_normalised)
rast_tmp_masked$MNDWI_normalised <- (rast_tmp_masked$B3_normalised-rast_tmp_masked$B11)/(rast_tmp_masked$B3_normalised+rast_tmp_masked$B11)
rast_tmp_masked$NDVI_normalised <- (rast_tmp_masked$B8-rast_tmp_masked$B4_normalised)/(rast_tmp_masked$B8+rast_tmp_masked$B4_normalised)


# Add the average value of the dark channel across all pixels for each day
dark1 <- global(rast_tmp_masked1$dark_filtered,fun="mean",na.rm=T)
dark2 <- global(rast_tmp_masked2$dark_filtered,fun="mean",na.rm=T)
dark3 <- global(rast_tmp_masked3$dark_filtered,fun="mean",na.rm=T)
dark4 <- global(rast_tmp_masked4$dark_filtered,fun="mean",na.rm=T)
dark5 <- global(rast_tmp_masked5$dark_filtered,fun="mean",na.rm=T)
df_clean$dark_day <- NA
df_clean[which(df_clean$day==1),"dark_day"] <- dark1
df_clean[which(df_clean$day==2),"dark_day"] <- dark2
df_clean[which(df_clean$day==3),"dark_day"] <- dark3
df_clean[which(df_clean$day==4),"dark_day"] <- dark4
df_clean[which(df_clean$day==5),"dark_day"] <- dark5

# Summarise the effects of dehazing and save to file
# I've included B6 (one of the near-infrared bands) because it seems
# widely accepted that 740 nm penetrates haze very well
# So B6 helpfully indicates what a band might look like without haze
df_dehazing_summary <- data.frame("day"=c(1:5),
                                  "B6"=aggregate(B6~day,FUN=mean,data=df_clean,na.rm=T)$B6,
                                  "B2"=aggregate(B2~day,FUN=mean,data=df_clean,na.rm=T)$B2,
                                  "B2_normalised"=aggregate(B2_normalised~day,FUN=mean,data=df_clean,na.rm=T)$B2_normalised,
                                  "B2_dehazed"=aggregate(B2_dehazed~day,FUN=mean,data=df_clean,na.rm=T)$B2_dehazed,
                                  "A_B2" = c(A_1[1],A_2[1],A_3[1],A_4[1],A_5[1]),
                                  "B3"=aggregate(B3~day,FUN=mean,data=df_clean,na.rm=T)$B3,
                                  "B3_normalised"=aggregate(B3_normalised~day,FUN=mean,data=df_clean,na.rm=T)$B3_normalised,
                                  "B3_dehazed"=aggregate(B3_dehazed~day,FUN=mean,data=df_clean,na.rm=T)$B3_dehazed,
                                  "A_B3" = c(A_1[2],A_2[2],A_3[2],A_4[2],A_5[2]),
                                  "B4"=aggregate(B4~day,FUN=mean,data=df_clean,na.rm=T)$B4,
                                  "B4_dehazed"=aggregate(B4_dehazed~day,FUN=mean,data=df_clean,na.rm=T)$B4_dehazed,
                                  "B4_normalised"=aggregate(B4_normalised~day,FUN=mean,data=df_clean,na.rm=T)$B4_normalised,
                                  "A_B4" = c(A_1[3],A_2[3],A_3[3],A_4[3],A_5[3]))
write.csv(df_dehazing_summary,"intermediate/df_dehazing_summary.csv",row.names = F)
