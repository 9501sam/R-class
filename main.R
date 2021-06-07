# library(ggplot2)
library(plyr)

add_xy <- function(dir)
{
    df <- read.csv(paste("./", dir, "/", dir, ".csv", sep = ""))
    Address_Finish <- read.csv(paste("./", dir, "/Address_Finish.csv", sep = ""))
    df_all <- data.frame(df, Response_X = Address_Finish$Response_X, Response_Y = Address_Finish$Response_Y)
    
    df_all <- df_all[-which(is.na(df_all$Response_X)), ]
    print(-which(is.na(df_all$Response_X)))

    write.csv(df_all, paste("./", dir, "/all_data.csv", sep = ""), row.names = FALSE)
}

cntPublic <- function(h)
{
    count <- daply(df_p, .variables = 'id', .fun = cnt, h);
    return(sum(count))
}

cnt<- function(p, h)
{
    dist <- sqrt((h$Response_X-p$Response_X)^2+(h$Response_Y-p$Response_Y)^2)
    if (dist <= 1500) {
        return(T)
    }
    return(F)
}


main <- function(dir)
{
    # Address_Finish <- read.csv("Address_Finish.csv")
    # df <- read.csv("10904/B_lvr_land_A.csv")
    # df <- df[df$交易標的 == '房地(土地+建物)' || df$交易標的 == '房地(土地+建物)+車位', ]
    # df <- df[df$主要途 == '住家用', ]

    # df1 <- data.frame(df, Response_X = Address_Finish$Response_X, Response_Y = Address_Finish$Response_Y)
    # df1 <- df1[-which(is.na(df1$Response_X)), ]
    # df1 <- data.frame(id = 1:nrow(df1), df1)
    # write.csv(df1, "./data.csv", row.names = FALSE)
    # df2 <- data.frame(id = 1:nrow(df2), df2)
    # write.csv(df2, "./Elementart_result.csv", row.names = FALSE)

    df_h <- read.csv(paste("./", dir, "/Address_Finish.csv", sep = ""))

    df_h <- read.csv("data.csv")
    df_p <<- read.csv("Elementart_result.csv")
   
    count <- daply(df_h, .variables = 'id', .fun = cntPublic);

    df3 <- data.frame(df_h, Count = count)
    write.csv(df3, "./count.csv", row.names = FALSE)
    # ggplot(df3, aes(Count, 單價元平方公尺 ))+geom_point()
}
