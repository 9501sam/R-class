library(plyr)

address <- function(one_data)
{
    one_data$土地區段位置建物區段門牌 = paste(
        "臺中市",
        one_data$鄉鎮市區, 
        one_data$土地區段位置建物區段門牌, sep = ""
    )
    return(data.frame(one_data));
}

df <- read.csv("10904/B_lvr_land_A.csv")
df_n <- df[df$交易標的 == '房地(土地+建物)' || df$交易標的 == '房地(土地+建物)+車位', ]
df_n <- df_n[df_n$主要用途 == '住家用', ]

df_n <- adply(df_n, .margins = 1, .fun = address)

write.csv(df_n, "./out.csv", row.names = FALSE)

