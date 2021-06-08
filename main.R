library(ggplot2)
library(plyr)

add_xy <- function(dir)
{
    df <- read.csv(paste("./", dir, "/", dir, ".csv", sep = ""))
    Address_Finish <- read.csv(paste("./", dir, "/Address_Finish.csv", sep = ""))
    df_all <- data.frame(df, Response_X = Address_Finish$Response_X, Response_Y = Address_Finish$Response_Y)
    
    df_all <- df_all[-which(is.na(df_all$Response_X)), ]

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

add_count <- function(dir)
{
    # df_h <- read.csv(paste("./", dir, "/all_data.csv", sep = ""))
    # df_h <- data.frame(id = 1:nrow(df_h), df_h)

    # df_p <<- read.csv("Elementary_result.csv")
    # count <- daply(df_h, .variables = 'id', .fun = cntPublic);
    # df_h <- data.frame(df_h, Count_Elementary = count)

    # df_p <<- read.csv("Secondary_result.csv")
    # count <- daply(df_h, .variables = 'id', .fun = cntPublic);
    # df_h <- data.frame(df_h, Count_Secondary = count)

    # df_p <<- read.csv("High_result.csv")
    # count <- daply(df_h, .variables = 'id', .fun = cntPublic);
    # df_h <- data.frame(df_h, Count_High = count)

    # df_p <<- read.csv("Hospital_result.csv")

    # count <- daply(df_h, .variables = 'id', .fun = cntPublic);
    # df_h <- data.frame(df_h, Count_Hospital = count)

    # write.csv(df_h, paste("./", dir, "/all_data.csv", sep = ""), row.names = FALSE)
}

avg <- function(df)
{
    # return(data.frame(Count_Elementary = df$Count_Elementary, 
                      # avg = mean(df$單價元平方公尺)))
    return(mean(df$單價元平方公尺))
}

draw <- function(dir)
{
    df <- read.csv(paste("./", dir, "/all_data.csv", sep = ""))
    # print(unique(df$Count_Elementary))
    unique(df$Count_Elementary)
    x <- daply(df, .variables = 'Count_Elementary', .fun = avg);
    print(x)

    # ggplot(df, aes(Count_Elementary, y = 單價元平方公尺)) + geom_col() 
    # ggplot(df, aes(Count_Elementary, y = mean(單價元平方公尺))) + geom_col() 
    # ggplot(df, aes(鄉鎮市區) ) + geom_histogram(stat = "count")
}

