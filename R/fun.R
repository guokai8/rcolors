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
classify_color <- function(hsl) {
  h <- hsl[1] * 360  # 将 Hue 从 [0,1] 转换到 [0°,360°]
  s <- hsl[2]
  
  if (s < 0.1) {
    return("Neutral")
  } else if ((h >= 0 && h < 30) || (h >= 330 && h <= 360)) {
    return("Red")
  } else if (h >= 30 && h < 90) {
    return("Yellow")
  } else if (h >= 90 && h < 150) {
    return("Green")
  } else if (h >= 150 && h < 210) {
    return("Cyan")
  } else if (h >= 210 && h < 270) {
    return("Blue")
  } else if (h >= 270 && h < 330) {
    return("Purple")
  } else {
    return("Unknown")
  }
}

# 应用分类到数据框
#color_data$Category <- sapply(color_data$HSL, classify_color)

#' 获取指定类别的颜色
#'
#' @param category 颜色类别，例如 "Red", "Green", "Blue", "Neutral" 等
#' @param n 所需颜色数量
#' @return 返回颜色代码的向量
#' @export
get_colors_by_category <- function(category, n = NULL) {
  if (!category %in% names(color_categories)) {
    stop("未知的颜色类别。可用类别有：", paste(names(color_categories), collapse = ", "))
  }
  colors <- color_categories[[category]]
  if (is.null(n)) {
    return(colors)
  } else {
    if (n <= length(colors)) {
      return(colors[1:n])
    } else {
      warning("请求的颜色数量超过了类别中的颜色数量，将循环使用颜色。")
      return(rep(colors, length.out = n))
    }
  }
}

