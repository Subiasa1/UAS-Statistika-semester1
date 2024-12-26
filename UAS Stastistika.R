# 1. Data: Simulasi data penelitian di Mars
set.seed(123)  # Untuk hasil yang konsisten 
n <- 100  # Jumlah sampel
kandungan_air <- rnorm(n, mean = 50, sd = 10)  # Persentase kandungan air (%)
kandungan_karbon <- rnorm(n, mean = 30, sd = 5)  # Kandungan karbon (%)
kemungkinan_kehidupan <- 10 + 0.5 * kandungan_air + 0.7 * kandungan_karbon + rnorm(n, mean = 0, sd = 3)

# Membuat dataset
data_mars <- data.frame(kandungan_air, kandungan_karbon, kemungkinan_kehidupan)

# 2. Uji Asumsi
# 2.1. Multikolinearitas
library(car)
model <- lm(kemungkinan_kehidupan ~ kandungan_air + kandungan_karbon, data = data_mars)
vif(model)  # Variance Inflation Factor (VIF)

# 2.2. Normalitas residual
shapiro.test(residuals(model))  # Uji normalitas residual

# 2.3. Homoskedastisitas
library(lmtest)
bptest(model)  # Breusch-Pagan Test untuk homoskedastisitas

# 3. Analisis Regresi Linear Berganda
summary(model)

# 4. Visualisasi
library(ggplot2)

# Scatterplot untuk hubungan kandungan air dan kemungkinan kehidupan
ggplot(data_mars, aes(x = kandungan_air, y = kemungkinan_kehidupan)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Hubungan Kandungan Air dengan Kemungkinan Kehidupan",
       x = "Kandungan Air (%)", y = "Kemungkinan Kehidupan (%)") +
  theme_minimal()

# Scatterplot untuk hubungan kandungan karbon dan kemungkinan kehidupan
ggplot(data_mars, aes(x = kandungan_karbon, y = kemungkinan_kehidupan)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "orange") +
  labs(title = "Hubungan Kandungan Karbon dengan Kemungkinan Kehidupan",
       x = "Kandungan Karbon (%)", y = "Kemungkinan Kehidupan (%)") +
  theme_minimal()

# 5. Interpretasi
# Berdasarkan hasil analisis, interpretasi dilakukan pada koefisien regresi, p-value, dan adjusted R-squared.

