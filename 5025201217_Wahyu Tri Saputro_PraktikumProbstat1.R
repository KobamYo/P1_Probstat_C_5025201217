# No. 1
# Seorang penyurvei secara acak memilih orang-orang di jalan sampai dia bertemu dengan
# seseorang yang menghadiri acara vaksinasi sebelumnya.
#  a. Berapa peluang penyurvei bertemu x = 3 orang yang tidak menghadiri acara vaksinasi
#     sebelum keberhasilan pertama ketika p = 0,20 dari populasi menghadiri acara vaksinasi ?
#     (distribusi Geometrik)
#  b. mean Distribusi Geometrik dengan 10000 data random , prob = 0,20 dimana distribusi
#     geometrik acak tersebut X = 3 ( distribusi geometrik acak () == 3 )
#  c. Bandingkan Hasil poin a dan b , apa kesimpulan yang bisa didapatkan?
#  d. Histogram Distribusi Geometrik , Peluang X = 3 gagal Sebelum Sukses Pertama
#  e. Nilai Rataan (??) dan Varian (??²) dari Distribusi Geometrik.

#a.
x = 3
p = 0.20
peluang = dgeom(x - 1, p, log = FALSE) 
peluang

#b.
mean(rgeom(n = 1000, p) == 3)
mean

#c.
#peluang lebih besar daripada rata rata yang ada.

#d.
hist(rgeom(x, p), main = "Histogram Distribusi Geometrik", xlab = "X")

#e.
mean1 = 1 / p
mean1
var1 = (1 - p) / (p ^ 2)
var1
mean(rgeom(n = 10000, p)) + 1
var(rgeom(n = 100000, p))  


# No. 2
# Terdapat 20 pasien menderita Covid19 dengan peluang sembuh sebesar 0.2. Tentukan :
#   a. Peluang terdapat 4 pasien yang sembuh.
#   b. Gambarkan grafik histogram berdasarkan kasus tersebut.
#   c. Nilai Rataan (??) dan Varian ( ) dari ??² Distribusi Binomial.

n2 = 20
p2 = 0.2

#a.
x2 = 4
peluang2 = dbinom(x2, n2 ,p2 , log = FALSE)
peluang2

#b.
hist(rbinom(x2, n2, p2), xlab = "X", ylab = "Frekuensi", main = "Histogram Distribusi Binomial")

#c.
mean2 = n2 * p2
mean2
var2 = n2 * p2 * (1 - p2)
var2


# No. 3
# Diketahui data dari sebuah tempat bersalin di rumah sakit tertentu menunjukkan rata-rata historis
# 4,5 bayi lahir di rumah sakit ini setiap hari. (gunakan Distribusi Poisson)
#   a. Berapa peluang bahwa 6 bayi akan lahir di rumah sakit ini besok?
#   b. simulasikan dan buatlah histogram kelahiran 6 bayi akan lahir di rumah sakit ini selama
#      setahun (n = 365)
#   c. dan bandingkan hasil poin a dan b , Apa kesimpulan yang bisa didapatkan
#   d. Nilai Rataan (??) dan Varian ( ??² ) dari Distribusi Poisson.

lambda = 4.5
x3 = 6
n3 = 356

#a.
peluang3 = dpois(x3, lambda)
peluang3

#b.
hist(rpois(n3, lambda), main = "Histogram Distribusi Poisson")

#c.
# lebih banyak kelahiran bayi yang akan lahir dalam
# 1 tahun dibanding yang akan lahir besok

#d.
mean3 = var3 = lambda
mean3
var3


# No. 4
# Diketahui nilai x = 2 dan v = 10. Tentukan:
#   a. Fungsi Probabilitas dari Distribusi Chi-Square.
#   b. Histogram dari Distribusi Chi-Square dengan 100 data random.
#   c. Nilai Rataan (??) dan Varian ( ??² ) dari Distribusi Chi-Square.

x4 = 2
v = 10

#a. 
peluang4 = dchisq(x4, v, ncp = 0, log = FALSE)
peluang4

#b.
hist(rchisq(x4, v, ncp = 0), main = "Histogram Distribusi Chi-Square")

#c.
mean4 = v
mean4
var4 = v * 2
var4


# No. 5
# Diketahui bilangan acak (random variable) berdistribusi exponential (?? = 3). Tentukan
#   a. Fungsi Probabilitas dari Distribusi Exponensial
#   b. Histogram dari Distribusi Exponensial untuk 10, 100, 1000 dan 10000 bilangan random
#   c. Nilai Rataan (??) dan Varian ( ??² ) dari Distribusi Exponensial untuk n = 100 dan ?? = 3
#      Petunjuk:
#       ??? Gunakan set.seed(1)
#       ??? Gunakan fungsi bawaan R

lambda2 = 3

#a.
peluang5 = dexp(lambda2, rate = 1, log = FALSE)
peluang5

#b.
hist(rexp(10, rate = 1), main = "Histogram Distribusi Exponensial")
hist(rexp(100, rate = 1),main="Histogram Distribusi Exponensial")
hist(rexp(1000, rate = 1),main="Histogram Distribusi Exponensial")
hist(rexp(10000, rate = 1),main="Histogram Distribusi Exponensial")

#c.
n5 = 100
mean = 1 / lambda2
mean
var = ((mean) * (1 / sqrt(n5))) * ((mean) * (1 / sqrt(n5)))
var


# No. 6
# Diketahui generate random nilai sebanyak 100 data, mean = 50, sd = 8. Tentukan
#   a. Fungsi Probabilitas dari Distribusi Normal P(X1 ??? x ??? X2), hitung Z-Score Nya dan plot
#      data generate randomnya dalam bentuk grafik. Petunjuk(gunakan fungsi plot()).
#      Keterangan :
#       X1 = Dibawah rata-rata
#       X2 = Diatas rata-rata
#       Contoh data :
#         1,2,4,2,6,3,10,11,5,3,6,8
#         rata-rata = 5.083333
#         X1 = 5
#         X2 = 6
#   b. Generate Histogram dari Distribusi Normal dengan breaks 50 dan format penamaan:
#      NRP_Nama_Probstat_{Nama Kelas}_DNhistogram
#      Contoh :
#         312312312_Rola_Probstat_A_DNhistogram
#   c. Nilai Varian ( ) dari hasil generate random nilai ??² Distribusi Normal

set.seed(0)
sd <- 8 
#standar deviasi
x <- rnorm(100, 50, sd)

rata <- mean(x) 
x1 <- floor(mean(x))
x2 <- ceiling(mean(x)) 

#a.
z <- function(input)
{
  return ((1 / (sd * sqrt(2*22/7))) * exp(1) ^ (-0.5 * ((input - rata) / sd) ^ 2)) #rumus fungsi peluang murni
}

z(x1) 
z(x2) 

plot(x, type = "l", col = "blue") 

plot(z(x), type = "l", col = "blue")


sortedx <- sort(x, decreasing = FALSE)
i <- l <- odd <- even <- 0
j <- k <- 1


for(i in 1:100){
  if (i %% 2== 0){
    even[j] <- (sortedx[i])
    j <- j + 1
  }
  else{
    odd[k] <- (sortedx[i])
    k <- k + 1
  }
}

combined <- even
flipped_odd <- sort(odd, decreasing = TRUE)
for(l in 1:50){
  combined[50 + l] <- flipped_odd[l]
}

plot(combined, type = "l", col = "blue")

#b.
hist(x, main = "5025201217_Wahyu Tri Saputro_C_DNHistogram", breaks = 50, col = "lightblue", border = "white", xlim= c(30, 70))

#c.
paste("Nilai Varian = ", sd ^ 2)

