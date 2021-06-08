library(ggplot2)
library(plyr)

add_xy <- function(dir)
{
    df <- read.csv(paste("./", dir, "/", dir, ".csv", sep = ""))
    Address_Finish <- read.csv(paste("./", dir, "/Address_Finish.csv", sep = ""))
    df_all <- data.frame(df, Response_X = Address_Finish$Response_X, Response_Y = Address_Finish$Response_Y)
    
    df_all <- df_all[-which(is.na(df_all$Response_X)), ]

    write.csv(df_all, paste("./", dir, "/data.csv", sep = ""), row.names = FALSE)
}

add_xy_b <- function()
{
    df <- read.csv("./10904/10904_b.csv")

    Address_Finish <- read.csv("10904/Address_Finish.csv", sep = "")

    df_all <- data.frame(df, Response_X = Address_Finish$Response_X, Response_Y = Address_Finish$Response_Y)

     print(df_all)
    
     df_all <- df_all[-which(is.na(df_all$Response_X)), ]

     write.csv(df_all, "10904/data_b.csv", row.names = FALSE)
}

cntPublic <- function(h)
{
    count <- daply(df_p, .variables = 'id', .fun = cnt, h);
    return(sum(count))
}

cnt<- function(p, h)
{
    
    dist <- sqrt((h$Response_X-p$Response_X)^2+(h$Response_Y-p$Response_Y)^2)
    if (dist <= 300) {
        return(T)
    }
    return(F)
}

add_count <- function(dir)
{
    df_h <- read.csv(paste("./", dir, "/data.csv", sep = ""))
    df_h <- data.frame(id = 1:nrow(df_h), df_h)

    df_p <<- read.csv("Elementary_result.csv")
    count <- daply(df_h, .variables = 'id', .fun = cntPublic);
    df_h <- data.frame(df_h, Count_Elementary = count)

    df_p <<- read.csv("Secondary_result.csv")
    count <- daply(df_h, .variables = 'id', .fun = cntPublic);
    df_h <- data.frame(df_h, Count_Secondary = count)

    df_p <<- read.csv("High_result.csv")
    count <- daply(df_h, .variables = 'id', .fun = cntPublic);
    df_h <- data.frame(df_h, Count_High = count)

    df_p <<- read.csv("Hospital_result.csv")

    count <- daply(df_h, .variables = 'id', .fun = cntPublic);
    df_h <- data.frame(df_h, Count_Hospital = count)

    write.csv(df_h, paste("./", dir, "/all_data.csv", sep = ""), row.names = FALSE)
}

avg <- function(df)
{
    return(mean(df$單價元平方公尺))
}

draw <- function(dir)
{
    df <- read.csv(paste("./", dir, "/all_data.csv", sep = ""))

    # avg <- daply(df, .variables = 'Count_Elementary', .fun = avg);
    # print(as.vector(avg))
    # df_E <- data.frame(
    #            Count_Elementary = sort(unique(df$Count_Elementary)),
    #            Avg = avg)
    # ggplot(df_E, aes(Count_Elementary, y = avg)) + geom_col() 

    # avg <- daply(df, .variables = 'Count_Secondary', .fun = avg);
    # print(as.vector(avg))
    # df_S <- data.frame(
    #            Count_Secondary = sort(unique(df$Count_Secondary)),
    #            Avg = avg)
    # ggplot(df_S, aes(Count_Secondary, y = avg)) + geom_col() 

    # avg <- daply(df, .variables = 'Count_High', .fun = avg);
    # print(as.vector(avg))
    # df_H <- data.frame(
    #            Count_High = sort(unique(df$Count_High)),
    #            Avg = avg)
    # ggplot(df_H, aes(Count_High, y = avg)) + geom_col() 

    # avg <- daply(df, .variables = 'Count_Hospital', .fun = avg);
    # print(as.vector(avg))
    # df_H <- data.frame(
    #            Count_Hospital = sort(unique(df$Count_Hospital)),
    #            Avg = avg)
    # ggplot(df_H, aes(Count_Hospital, y = avg)) + geom_col() 
}

