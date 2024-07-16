# 加载所需的库
library(dplyr)
library(tidyr)
library(purrr)

# 设置工作目录为包含CSV文件的文件夹（请根据实际情况修改）
setwd("path/to/your/csv/folder")

# 获取当前目录下所有的CSV文件的路径
csv_files <- list.files(pattern = "*.csv")

# Bootstrap函数来估计95%置信区间
bootstrap_confidence_interval <- function(data, n_boot = 1000) {
  means <- numeric(n_boot)
  for (i in 1:n_boot) {
    sample_data <- sample(data, replace = TRUE, size = length(data))
    means[i] <- mean(sample_data)
  }
  lower <- quantile(means, 0.025)
  upper <- quantile(means, 0.975)
  return(c(lower, upper))
}

# 批量处理每个CSV文件
process_csv_files <- function(file_list) {
  map_dfr(file_list, function(file) {
    # 读取CSV文件
    data <- read.csv(file, header = TRUE)
    # 假设所有CSV文件中都有一个名为"value"的列，包含我们想要分析的数据
    # 如果列名不同，需要修改列名
    values <- data$value
    
    # 计算95%置信区间
    ci <- bootstrap_confidence_interval(values)
    # 返回文件名和对应的置信区间
    data.frame(FileName = file, LowerCI = ci[1], UpperCI = ci[2])
  })
}

# 执行批量处理并存储结果
results <- process_csv_files(csv_files)

# 查看结果
print(results)