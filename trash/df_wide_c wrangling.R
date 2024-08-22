
#Remove ID col
df_gt_rast <- df_gt_rast[,-35] 

# Order the columns correctly
namevec <- c(names(df_gt_rast)[c(1:4)],sort(names(df_gt_rast)[c(5:ncol(df_gt_rast))]))
df_gt_rast <- df_gt_rast[,namevec]
# Get the variable names
vnames <- substr(names(df_gt_rast)[c(5:ncol(df_gt_rast))],1,(nchar(names(df_gt_rast))[c(5:ncol(df_gt_rast))]-5))
vnames <- unique(vnames)

tmp_seq <- seq(5,ncol(df_gt_rast),1)
tmp_seq_split <- split(tmp_seq, as.integer(gl(length(tmp_seq), length(filenames), length(tmp_seq))))
names(tmp_seq_split) <- NULL

# Reshape to long format
df_wide <- reshape(df_gt_rast,
                   direction="long",
                   varying=tmp_seq_split,
                   v.names=vnames,
                   timevar="day",
                   idvar=c("pond"),
                   times=c(1:length(filenames)))

# Convert character columns to numeric
df_wide$ammonia <- as.numeric(df_wide$ammonia)
df_wide$chlorophyll <- as.numeric(df_wide$chlorophyll)
df_wide$do <- as.numeric(df_wide$do)
df_wide$ph <- as.numeric(df_wide$ph)
df_wide$phycocyanin <- as.numeric(df_wide$phycocyanin)
df_wide$temperature <- as.numeric(df_wide$temperature)

# Retain only complete cases
df_wide_c <- df_wide[complete.cases(df_wide),]

# Check average values by day
aggregate(B2~day,FUN=mean,df_wide_c)
aggregate(B3~day,FUN=mean,df_wide_c)
aggregate(B4~day,FUN=mean,df_wide_c)
