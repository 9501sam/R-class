library(plyr)

cntPublic<- function(h)
{
    count <- daply(df2, .variables = 'id', .fun = cnt, h);
    return(sum(count))
}

cnt<- function(p, h)
{
    dist <- sqrt((h$Response_X-p$Response_X)^2+(h$Response_Y-p$Response_Y)^2)
    if (dist <= 1200) {
        return(T)
    }
    return(F)
}

myfun<- function()
{
    cntPublic(df1,df2)
}

main <- function()
{
    # Address_Finish <- read.csv("Address_Finish.csv")
    # df <- read.csv("10904/B_lvr_land_A.csv")
    # df <- df[df$交易標的 == '房地(土地+建物)' || df$交易標的 == '房地(土地+建物)+車位', ]
    # df <- df[df$主要用途 == '住家用', ]

    # df1 <- data.frame(df, Response_X = Address_Finish$Response_X, Response_Y = Address_Finish$Response_Y)
    # df1 <- df1[-which(is.na(df1$Response_X)), ]
    # df1 <- data.frame(id = 1:nrow(df1), df1)
    # write.csv(df1, "./data.csv", row.names = FALSE)
    # df2 <- data.frame(id = 1:nrow(df2), df2)
    # write.csv(df2, "./Elementart_result.csv", row.names = FALSE)

    # df1 <- read.csv("data_test.csv")
    # df2 <<- read.csv("Elementart_result_test.csv")

    df1 <- read.csv("data.csv")
    df2 <<- read.csv("Elementart_result.csv")
   
    count <- daply(df1, .variables = 'id', .fun = cntPublic);

    df3 <- data.frame(df1, Count = count)
    write.csv(df3, "./count.csv", row.names = FALSE)
}

main()
