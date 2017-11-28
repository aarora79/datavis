####NOT RUN


df_cleanup = read.csv("vsat_info_from_nms.csv")
df_cleanup = df_cleanup %>%
             select(-ALLOW_IDU_SWAP, -ALLOW_MOVE, -BLACK_LISTED,
                    -CHASSIS_PN, -ESN, -FQDN, -GW_ID, -HW_TYPE,
                    -LAST_UPDATE_TIMESTAMP, -ODU_CHASSIS_DN, -ODU_CHASSIS_PN,
                    -ODU_CHASSIS_RN, -ODU_CHASSIS_RN, -ODU_POWER,
                    -ODU_RADIO_DN, -ODU_RADIO_PN, -ODU_RADIO_RN, -ODU_SN,
                    -STATIC_IPV4_ADDRESS, -STATUS, -SW_PROFILE_NAME, 
                    -SW_VERSION, -UL_SYMBOL_RATE,
                    -PROFILEGROUP_NAME, -RX_POLARIZATION, -SATELLITE_ID)
write.csv(df_cleanup, "vsat_info_from_nms1.csv")

df2 = read.csv("vsat_info_from_nms1.csv")
name_map = data.frame(DEVICE_ID = df2$DEVICE_ID, DEVICE_ID2=paste0("TERM", seq(1:NROW(df2))))

df3 = merge(df_cleanup, name_map, by="DEVICE_ID")
df3 = df3 %>% select(-DEVICE_ID) %>% rename(DEVICE_ID=DEVICE_ID2)
write.csv(df3, "df3_vsat_info_from_nms.csv")

df4 = read.csv("vsat_info_from_ipgws.csv") %>% rename(DEVICE_ID=DEVICEID)
df4 = merge(df4, name_map, by="DEVICE_ID")
df5 = df4 %>% select(-DEVICE_ID) %>% rename(DEVICE_ID=DEVICE_ID2)
write.csv(df5, "df5_vsat_info_from_ipgws.csv")



lat = c()
lng = c()
beam_id = c()
LAT_SD_DEV = 12.5
LNG_SD_DEV = 50

#3054.06, 7551.438 Ludhiana
N = 2
lat = append(lat, rnorm(N, 3054.06, LAT_SD_DEV))
lng = append(lng, rnorm(N, 7551.438, LNG_SD_DEV))
beam_id = append(beam_id, rep(19, N))


#2654.744, 7547.238, jaIPUR
N = 371
lat = append(lat, rnorm(N, 2654.744, LAT_SD_DEV))
lng = append(lng, rnorm(N, 7547.238, LNG_SD_DEV))
beam_id = append(beam_id, rep(20, N))

#1723.1, 7829.202 hyderabad
N = 6
lat = append(lat, rnorm(N, 1723.1, LAT_SD_DEV))
lng = append(lng, rnorm(N, 7829.202, LNG_SD_DEV))
beam_id = append(beam_id, rep(21, N))

#2526.148, 8150.778 Allahabad
N = 297
lat = append(lat, rnorm(N, 2526.148, LAT_SD_DEV))
lng = append(lng, rnorm(N, 8150.778, LNG_SD_DEV))
beam_id = append(beam_id, rep(25, N))

#2234.356, 8821.834 kolkata
N = 1
lat = append(lat, rnorm(N, 2234.356, LAT_SD_DEV))
lng = append(lng, rnorm(N, 8821.834, LNG_SD_DEV))
beam_id = append(beam_id, rep(26, N))

#2626.994, 8019.914 kanpur
N = 2
lat = append(lat, rnorm(N, 2626.994, LAT_SD_DEV))
lng = append(lng, rnorm(N, 8019.914, LNG_SD_DEV))
beam_id = append(beam_id, rep(28, N))

#2639.168, 9247.556 tezpur
N = 167
lat = append(lat, rnorm(N, 2639.168, LAT_SD_DEV))
lng = append(lng, rnorm(N, 9247.556, LNG_SD_DEV))
beam_id = append(beam_id, rep(29, N))

#	2315.594, 7724.756 bhopal
N = 525
lat = append(lat, rnorm(N, 2315.594, LAT_SD_DEV))
lng = append(lng, rnorm(N, 7724.756, LNG_SD_DEV))
beam_id = append(beam_id, rep(32, N))



dflat_lng2 = data.frame(lat=lat, lng=lng, ABSOLUTE_BEAM_ID=beam_id)
write.csv(dflat_lng2, "latlng.csv", row.names = F)
df2 = read.csv("vsat_info_from_nms.csv")
df2 = df2 %>%
      group_by(ABSOLUTE_BEAM_ID)

df2$LATITUDE = dflat_lng2$lng
df2$LONGITUDE = dflat_lng2$lat
write.csv(df2, "vsat_info_from_nms2.csv")


