# 判斷套件是否安裝
if (!requireNamespace("rpart", quietly = TRUE)) {
#     install.packages("rpart")
}
library(rpart)

# 判斷指令是否存在
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
    stop("USAGE: Rscript hw3_studentID.R --fold k --input Archaeal_tfpssm.csv --output performance.csv", call.=FALSE)
}

# 引數變數初始化
f_in <- NA
f_out <- NA
f_k <- NA

# 判斷引數是否有帶入
for (i in args) {
    if (i == '--fold') {
        f_fold <- args[which(args==i)+1]
    }
    if (i == '--input') {
        f_in <- args[which(args==i)+1]
    } 
    if (i == '--output') {
        f_out <- args[which(args==i)+1]
    }
}

# 讀取 csv 檔
d <- read.csv(f_in, header = F)

# 總列數 805
n <- nrow(d)

# 設定種子
set.seed(42)

# shuffle
d <- d[sample(n), ]

# [0 0 0 0 0] 
accuracies <- numeric(f_fold)

# k 數
k <- strtoi(f_fold)

# 取得切分完後的每一個間距值
interval = (n / k)

# 資料範圍切分
li_r = list()
for (i in 1:k){
    if (i == 1){
        li_r[[i]] <- c( i, round(i * interval) )
    } else {
        li_r[[i]] <- c( round( (i-1) * interval ) + 1, round( i * interval) )
    }
}
# [[1]]
# [1]  1 80
# 
# [[2]]
# [1]  81 161
# 
# [[3]]
# [1] 162 242
# 
# [[4]]
# [1] 243 322
# 
# [[5]]
# [1] 323 402
# 
# [[6]]
# [1] 403 483
# 
# [[7]]
# [1] 484 564
# 
# [[8]]
# [1] 565 644
# 
# [[9]]
# [1] 645 724
# 
# [[10]]
# [1] 725 805



# 哪些部分要成為 test 和 val data
li_s = list()
for (i in 1:k){
    if (i != k){
        li_s[[i]] <- c(i, i+1)
    } else {
        li_s[[i]] <- c(k, 1)
    }
}
# [1] "1,2"
# [1] "2,3"
# [1] "3,4"
# [1] "4,5"
# [1] "5,6"
# [1] "6,7"
# [1] "7,8"
# [1] "8,9"
# [1] "9,10"
# [1] "10 1"



# 計算平均用的變數
total_train_acc <- 0
total_test_acc <- 0
total_val_acc <- 0


# 放罝每一個 fold 計算出的 accuracy
li_train_acc <- c()
li_val_acc <- c()
li_test_acc <- c()


# 放置 fold 標題
li_set <- c()


# k-fold CV
for (i in 1:k){
    # 1: test
    test_s_b <- li_r[[ li_s[[i]][[1]] ]][[1]] # 1: 1
    test_s_e <- li_r[[ li_s[[i]][[1]] ]][[2]] # 1: 161'
    
    # 2: val
    val_s_b <- li_r[[ li_s[[i]][[2]] ]][[1]] # 2: 162
    val_s_e <- li_r[[ li_s[[i]][[2]] ]][[2]] # 2: 322
    
    # 測試資料
    test_data <- d[test_s_b:test_s_e, ]
    
    # 驗證資料
    val_data <- d[val_s_b:val_s_e, ]
    
    # 取得訓練資料
    if (test_s_b == 1){
        # test, val, train, train, train
        train_data <- na.omit( d[val_s_e+1:li_r[[k]][[2]],] )
    } else if (val_s_e == li_r[[k]][[2]]){
        # train, train, train, test, val
        train_data <- na.omit( d[1:test_s_b-1,] )
    } else if (test_s_b != 1 & val_s_e != li_r[[k]][[2]]){
        # 可能是:
        # train, test, val, train, train
        # train, train, test, val, train
        train_data <- rbind( na.omit( d[ 1:test_s_b-1, ] ), na.omit( d[val_s_e+1:li_r[[k]][[2]], ] ) )
    } else if (val_s_e < test_s_b) {
        # val, train, train, train, test
        train_data <- na.omit( d[val_s_e+1:test_s_b-1, ] )
    }
    
    # 取得特徵與答案
    train_labels <- names(train_data)[2]
    train_features <- names(train_data)[3:ncol(d)]
    
    # 設定特徵範圍和答案，以便進行訓練
    formula <- as.formula(paste(train_labels, "~", paste(train_features, collapse = "+")))
    
    # 建立模型
    model <- rpart(
        formula, 
        data = train_data, 
        control=rpart.control(maxdepth=4),
        method="class"
    )
    
    # set
    # print( paste("fold", i, sep="") )
    li_set <- rbind(li_set, paste("fold", i, sep=""))
    
    # 預測 train data
    train_predictions <- predict(model, train_data, type = "class")
    train_accuracy <- mean(train_predictions == train_data[[2]])
    # print(paste("Training Accuracy:", train_accuracy))
    total_train_acc <- total_train_acc + train_accuracy
    li_train_acc <- rbind(li_train_acc, round(train_accuracy, digits = 2))
    
    # 預測 val data
    val_predictions <- predict(model, val_data, type = "class")
    val_accuracy <- mean(val_predictions == val_data[[2]])
    # print(paste("Validation Accuracy:", val_accuracy ))
    total_val_acc <- total_val_acc + val_accuracy
    li_val_acc <- rbind(li_val_acc, round(val_accuracy, digits = 2))
    
    # 預測 test data
    test_predictions <- predict(model, test_data, type = "class")
    test_accuracy <- mean(test_predictions == test_data[[2]])
    # print(paste("Test Accuracy:", test_accuracy ))
    total_test_acc <- total_test_acc + test_accuracy
    li_test_acc <- rbind(li_test_acc, round(test_accuracy, digits = 2))
}

# 計算所有 accuracy 的平均值
avg_train_acc <- round(total_train_acc / k, digits = 2)
avg_val_acc <- round(total_val_acc / k, digits = 2)
avg_test_acc <- round(total_test_acc / k, digits = 2)

# print( paste("ave.") )
# print( paste(avg_train_acc, avg_val_acc, avg_test_acc) )

# 輸出結果
result <- data.frame(set = li_set,
                     training = li_train_acc,
                     validation = li_train_acc,
                     test = li_test_acc)

# 整合平均欄位和對應的值
result <- rbind( result, c('ave.', avg_train_acc, avg_val_acc, avg_test_acc) )

# 儲存成 csv
write.csv(result, f_out, row.names = FALSE, quote = FALSE)