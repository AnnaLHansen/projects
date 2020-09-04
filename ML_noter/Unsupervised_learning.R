# Unsupervised learning
# Ud over unsupervised learning er der også supervised learning og 
# reinforcement learning.
# Unsupervised learning går ud på at finde struktur i data uden labels.
# 
# Det overordnede mål kan enten være at finde grupperinger 
# indenfor et datasæt, men det kan også være at 
# finde mønstre i features i ens data som gør 
# at det er muligt at reducere dimensioner. 
# det gør det også muligt at visualisere data
# men mange dimenttioner og stadig visualisere 
# variationen. 
# 
# k-means clustering algoritme 
# Indeler data i et bestemt antal 
# predefinede grupper. 
model <- kmean(data, centers = antal grupper, nstart = antal gange kørt)
summary(model)
plot(data, col = model$cluser, main = "title")

# Cluster center
# Hvor god en cluser indeling er afhænger
# af den "total within cluser sum of squares"
# - for hver observation i et cluser udregnes
# sqrt-distance til cluser centeret og summere det
# K-means gør dette. Der hvor "Total within sum of squares" 
# begynder at falde mindre igen, er typisk et godt bud på 
# hvor mange clusters man skal prøve af.
# 
# Udregn centeret af hver sub-gruppe
# Og hver punkt i data bliver assignet til 
# det center som ligger tættest på punktet.
# Dette gøres for hver iteration i kmeans algoritmen
# Total within minimum sum of squares afgører hvad
# der er den bedste model. Ligesom residual sum of squares i linear regression.
wss <-  0
for (i in 1:15) { # 1 til 15 mulige clusters
  model <- kmeans(data, centers = i, nstart = 20)
  wss[i] <- model$tot.withinss
}
plot(1:15, wss)
# Når k er blevet valgt ud fra ovenstående plot anvendes k til 
# at træne den endelige model og data plottes herefter.
model <- kmeans(data, centers = udvalgtk, nstart = 20)
plot(data[c(variabel1, variabel2)], col = model$cluster)

# Hierakisk clustering:
# bottom up clustering - hver observation får sit eget cluster.
# Herefter slåes de to nærmeste punkter sammen til samme cluster.
# Til sidst er der kun et cluster tilbage.
# Dette kræver kun at man kan måle afstanden mellem ens observationer. 
# #Euclidean distance
afstandsmatrix <- dist(data)
model <- hclust(d = afstandsmatrix)

# Dendogram med hierakisk clustering.
# Alogrimen i hierakisk clusering fungerer således at den måler afstanden 
# mellem datapunkterne. Denne heirakiske struktur kan visualiseres som et træ, 
# hvor alle punkterne i sidste ende er forbundet, men hvor det er muligt at se hvilken
# observationer der er tætte på hinanden. I sådan et træ vil det være højden
# som indikere hvor tæt de to observationer er på hinanden. 
# Et dendogram
plot(model)
# cutree() opdeler observationerne i clusters ud fra et input om hvor langt 
# observationerne skal være fra hinanden. 
# Eks:
cutree(model, h= 2) # En højde på 2 i dendogrammet ønskes. 
cutree(model, k= 2) # To klusters ønskes.









# 