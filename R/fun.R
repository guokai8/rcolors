#' 将十六进制颜色代码转换为 HSL 值
#'
#' 该函数将十六进制颜色代码转换为 HSL（色相、饱和度、亮度）颜色空间的值。
#'
#' @param hex 字符向量，包含十六进制颜色代码（可带或不带 "#" 前缀）
#' @return 返回一个数据框，包含 \code{H}, \code{S}, \code{L} 三列，分别为色相、饱和度和亮度
#' @examples
#' hex_colors <- c("FF0000", "#00FF00", "0000FF")
#' hsl_values <- hex2HSL(hex_colors)
#' print(hsl_values)
#' @export
hex2HSL <- function(hex) {
  # 确保颜色代码以 "#" 开头
  hex <- ifelse(substr(hex, 1, 1) != "#", paste0("#", hex), hex)
  
  # 将十六进制颜色转换为 RGB 值（范围 [0, 1]）
  rgb_matrix <- t(col2rgb(hex)) / 255  # 转置后，每行代表一个颜色
  
  # 提取 R、G、B 分量
  r <- rgb_matrix[, 1]
  g <- rgb_matrix[, 2]
  b <- rgb_matrix[, 3]
  
  # 计算最大和最小的 RGB 值
  max_val <- pmax(r, g, b)
  min_val <- pmin(r, g, b)
  delta <- max_val - min_val
  
  # 计算亮度 L
  l <- (max_val + min_val) / 2
  
  # 初始化饱和度 S 和色相 H
  s <- numeric(length(r))
  h <- numeric(length(r))
  
  # 避免除以零的情况
  non_zero_delta <- delta != 0
  
  # 计算饱和度 S
  s[!non_zero_delta] <- 0
  s[non_zero_delta] <- delta[non_zero_delta] / (1 - abs(2 * l[non_zero_delta] - 1))
  
  # 计算色相 H
  h[!non_zero_delta] <- 0  # 当 delta 为零时，色相未定义，设为 0
  
  # 分别计算最大值为 R、G、B 的色相 H
  idx_r <- (max_val == r) & non_zero_delta
  idx_g <- (max_val == g) & non_zero_delta
  idx_b <- (max_val == b) & non_zero_delta
  
  h[idx_r] <- ((g[idx_r] - b[idx_r]) / delta[idx_r]) %% 6
  h[idx_g] <- ((b[idx_g] - r[idx_g]) / delta[idx_g]) + 2
  h[idx_b] <- ((r[idx_b] - g[idx_b]) / delta[idx_b]) + 4
  
  h <- h / 6  # 归一化到 [0,1]
  h[h < 0] <- h[h < 0] + 1  # 确保 H 在 [0,1] 范围内
  
  # 将 H, S, L 组合成数据框
  hsl <- data.frame(
    H = h,
    S = s,
    L = l
  )
  
  return(hsl)
}
