library(plyr)

complete_address <- function(one_data)
{
    first = substr(one_data$土地區段位置建物區段門牌, 1, 1)

    if (first != "台" && first != "臺") {
        one_data$土地區段位置建物區段門牌 = paste(
            "臺中市",
            one_data$鄉鎮市區, 
            one_data$土地區段位置建物區段門牌, sep = ""
        )
    }

    return(data.frame(one_data));
}

filter_data <- function(dir) # dir of transaction file
{
    df <- read.csv(paste("./", dir, "/B_lvr_land_A.csv", sep = ""))
    df <- df[df$交易標的 == '房地(土地+建物)', ] 
    df <- df[df$主要用途 == '住家用', ]

    df1 <- df[substr(df$土地區段位置建物區段門牌, 1, 2) == "臺中", ]
    df2 <- df[substr(df$土地區段位置建物區段門牌, 1, 2) == "台中", ]
    df4 <- df[substr(df$土地區段位置建物區段門牌, 1, 2) != "臺中", ]
    df4 <- df4[substr(df3$土地區段位置建物區段門牌, 1, 2) != "台中", ]

    # complete addresses
    df4 <- ddply(df4, .variable = NULL, .fun = complete_address, .id = NULL) 

    df <- data.frame(rbind(df1, df2, df4))

    write.csv(df, paste("./", dir, "/", dir, ".csv", sep = ""), row.names = FALSE)
}

filter_data_b <- function(dir) # dir of transaction file
{
    df <- read.csv(paste("./", dir, "/B_lvr_land_A.csv", sep = ""))
    df <- df[df$交易標的 == '房地(土地+建物)', ] 
    df <- df[df$主要用途 == '商業用', ]

    df1 <- df[substr(df$土地區段位置建物區段門牌, 1, 2) == "臺中", ]
    df2 <- df[substr(df$土地區段位置建物區段門牌, 1, 2) == "台中", ]
    df4 <- df[substr(df$土地區段位置建物區段門牌, 1, 2) != "臺中", ]
    df4 <- df4[substr(df4$土地區段位置建物區段門牌, 1, 2) != "台中", ]

    # complete addresses
    df4 <- ddply(df4, .variable = NULL, .fun = complete_address, .id = NULL) 

    df <- data.frame(rbind(df1, df2, df4))

    write.csv(df, paste("./", dir, "/", dir, "_b.csv", sep = ""), row.names = FALSE)
}

main <- function()
{
    filter_data("10904")
    # filter_data("10903")
    # filter_data("10902")
    # filter_data("10901")

    # filter_data("10804")
    # filter_data("10803")
    # filter_data("10802")
    # filter_data("10801")

    # filter_data("10704")
    # filter_data("10703")
    # filter_data("10702")
    # filter_data("10701")

    # filter_data("10604")
    # filter_data("10603")
    # filter_data("10602")
    # filter_data("10601")

    # filter_data("10504")
    # filter_data("10503")
    # filter_data("10502")
    # filter_data("10501")
}

