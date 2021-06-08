library(ggplot2)
library(plyr)

add_dist <- function(one_data, x, y)
{
    dist <- sqrt((one_data$Response_X - x)^2
                 +(one_data$Response_Y - y)^2)

    dist <- ceiling(dist / 250) * 250
    if (dist > 1000)
        dist <- 1250
    return(data.frame(one_data, dist = dist))
}

main <- function()
{
    df <- read.csv("10904/data.csv")
    df_b <- read.csv("10904/data_b.csv")
    
    df_all <- data.frame(rbind(df, df_b))
    df_all <- rbind(df, df_b)
    df_all <- data.frame(id = 1:nrow(df_all), df_all)

    # 一中
    df_all <- ddply(df_all, .variables = 'id', .fun = add_dist, 218190.345, 2671578.953)
    # 女中
    df_all <- ddply(df_all, .variables = 'id', .fun = add_dist, 217210.098, 2670110.958)

    # 居仁
    df_all <- ddply(df_all, .variables = 'id', .fun = add_dist, 217347.768, 2670241.325)
    # 衛道
    df_all <- ddply(df_all, .variables = 'id', .fun = add_dist, 216936.138, 2675431.765)
    # 明道
    df_all <- ddply(df_all, .variables = 'id', .fun = add_dist, 212969.967, 2666887.203)
    # 大業
    df_all <- ddply(df_all, .variables = 'id', .fun = add_dist, 214686.655, 2672173.569)

    write.csv(df_all, "10904/all_data2.csv", row.names = FALSE)
}

draw <- function()
{
    df_all <- read.csv("10904/all_data2.csv")

    range <- c("a: 0-250", "b: 250-500", "c: 500-750", "d: 750-1000", "e: >1000")

    # 一中
    a <- mean(df_all$單價元平方公尺[df_all$dist == 250  ])
    b <- mean(df_all$單價元平方公尺[df_all$dist == 500  ])
    c <- mean(df_all$單價元平方公尺[df_all$dist == 750  ])
    d <- mean(df_all$單價元平方公尺[df_all$dist == 1000 ])
    e <- mean(df_all$單價元平方公尺[df_all$dist == 1250 ])
    price <- c(a, b, c, d, e)
    school <- data.frame(range, price = price, school = "一中")

    # 女中
    a <- mean(df_all$單價元平方公尺[df_all$dist.1 == 250  ])
    b <- mean(df_all$單價元平方公尺[df_all$dist.1 == 500  ])
    c <- mean(df_all$單價元平方公尺[df_all$dist.1 == 750  ])
    d <- mean(df_all$單價元平方公尺[df_all$dist.1 == 1000 ])
    e <- mean(df_all$單價元平方公尺[df_all$dist.1 == 1250 ])
    price <- c(a, b, c, d, e)
    school1 <- data.frame(range, price = price, school = "女中")

    # 居仁
    a <- mean(df_all$單價元平方公尺[df_all$dist.2 == 250  ])
    b <- mean(df_all$單價元平方公尺[df_all$dist.2 == 500  ])
    c <- mean(df_all$單價元平方公尺[df_all$dist.2 == 750  ])
    d <- mean(df_all$單價元平方公尺[df_all$dist.2 == 1000 ])
    e <- mean(df_all$單價元平方公尺[df_all$dist.2 == 1250 ])
    price <- c(a, b, c, d, e)
    school2 <- data.frame(range, price = price, school = "居人")

    # 衛道
    a <- mean(df_all$單價元平方公尺[df_all$dist.3 == 250  ])
    b <- mean(df_all$單價元平方公尺[df_all$dist.3 == 500  ])
    c <- mean(df_all$單價元平方公尺[df_all$dist.3 == 750  ])
    d <- mean(df_all$單價元平方公尺[df_all$dist.3 == 1000 ])
    e <- mean(df_all$單價元平方公尺[df_all$dist.3 == 1250 ])
    price <- c(a, b, c, d, e)
    school3 <- data.frame(range, price = price, school = "衛道")

    # 明道
    a <- mean(df_all$單價元平方公尺[df_all$dist.4 == 250  ])
    b <- mean(df_all$單價元平方公尺[df_all$dist.4 == 500  ])
    c <- mean(df_all$單價元平方公尺[df_all$dist.4 == 750  ])
    d <- mean(df_all$單價元平方公尺[df_all$dist.4 == 1000 ])
    e <- mean(df_all$單價元平方公尺[df_all$dist.4 == 1250 ])
    price <- c(a, b, c, d, e)
    school4 <- data.frame(range, price = price, school = "明道")

    # 大業
    a <- mean(df_all$單價元平方公尺[df_all$dist.5 == 250  ])
    b <- mean(df_all$單價元平方公尺[df_all$dist.5 == 500  ])
    c <- mean(df_all$單價元平方公尺[df_all$dist.5 == 750  ])
    d <- mean(df_all$單價元平方公尺[df_all$dist.5 == 1000 ])
    e <- mean(df_all$單價元平方公尺[df_all$dist.5 == 1250 ])
    price <- c(a, b, c, d, e)
    school5 <- data.frame(range, price = price, school = "大業")

    # df_show <- data.frame(rbind(school, school1, school2, school3))
    df_show <- data.frame(rbind(school2, school3, school4, school5))

    ggplot(df_show, aes(range, price, fill = school)) +
        geom_col(position = "dodge") 
}

