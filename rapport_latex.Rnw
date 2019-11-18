\documentclass{report}

\usepackage[utf8]{inputenc}

\begin{document}
\SweaveOpts{concordance=TRUE}

\begin{titlepage}
    \begin{center}
        \vspace*{1cm}
 
        \Huge
        \textbf{Machine learning - DTU}
 
        \vspace{0.5cm}
        \LARGE
        Rapport 
 
        \vspace{1.5cm}
 
        \textbf{Anna Louise Hansen}
        \vfill
 
        Fra Udviklings- og Forenklingsstyrrelsen
 
        \vspace{0.8cm}
 
    \end{center}
\end{titlepage}

<<echo=FALSE>>=
library(dplyr)
library(knitr)
library(corrplot)
setwd("C:/Users/alhan/projects")
source("help_func_datainspec.R")
source("help_func_standardization.R")
@


\chapter{Part I}

\section{Beskrivelse af data}
Alle boligejere i Danmark betaler en skat, ejendomsværdiskat, som er baseret på værdien af deres ejendom. Dette vil sige hele ejendommen inkl. grunden som boligen ligger på. For at kunne gøre dette laver den danske stat offentlige ejendomsvurderinger som disse skatter bliver baseret på. 
Det er derfor vigtigt at disse vurderinger er retvisende og ikke mindst forklarbare, således at en borger kan forstå hvilke parametre der ligger til grund for ejendomsvurderingen. 
Til dette project har jeg valgt at arbejde med anonymiseret data fra mit arbejde i udviklings- og forenklingsstyrrelsen, hvor jeg til dagligt arbejder med netop dette. Datasættet består af ejendomssalg fra en 6 årig periode. Ud over selve huspriserne består data også af en lang række attributter som beskriver karakteristika ved selve boligen. Det kan f.eks. være tagmateriale, boligens opførelsesår, information om størrelsen af huset og grunden eller bbr koder som dækker over boligens anvendelse. 
Der ud over består data også af en lang række attributter som fortæller noget om hvor boligens beliggenhed. Det kan f.eks. være boligens koordinater eller information om afstanden til kyst og skov eller afstand til motorvej og jernbane. 
Data kommer fra en række forskellige registre og offentlige styrrelser som eks. BBR og Styrrelsen for Dataforsyning og Effektivisering.

Til dette projekt vil jeg overordnet set prøve at se hvor godt man kan forudsige ejendomsværdier ud fra salgspriserne fra en 6-årig periode. 

Jeg vil med Principal Component Analysis få et overblik over de data der er til rådighed og få et visuelt overblik over attributterne. 
Herefter vil jeg med en unsupervised learning forsøge at gruppere det data jeg har til at generer yderlige attributer som kan indgå i modellen. Jeg vil her specifikt prøve at se om det er muligt at gruppere salgene i forskellige boligtyper. Jeg vil i samme omgang også forsøge at frasorterer outliers i data med anomaly detection. 
Herefter vil jeg med regressions model forsøge at kaste lys over projektets overordnede problem ved at forsøge at forudsige huspriserne ud fra salgspriser. I tilfælde af at modellen ikke ikke kan komme med en god prædiktion af en given ejendom vil det være muligt at denne ejendom bliver manuelt værdiansat af en sagsbehandler. Jeg vil derfor til slut med en classifikation forsøge at estimerer om en ejendom skal ud til manuel sagsbehandling baseret på dens estimerede ejendomsværdi.


\section{Detaljeret beskrivelse af data}

<<echo=FALSE>>=
train <- readRDS("anonym_data_kursus.rds")
@


Det salgsdata som jeg har valgt at arbejde med dækker i udgangspunktet `r nrow(train)` observationer med `r ncol(train)` attributter. Inden jeg går i gang med at kigge på data har jeg valgt at lave en oprydning i data. Det har jeg gjort fordi mange af attributterne bliver i mit daglige arbejde brugt i forbindelse med imødekomme diverse forretningskrav. Desuden dækker observationerne mange forskellige typer af ejendomssalg. Det er en blanding af parcelhussalg, rækkehussalg, sommerhussalg, salg af ejerlejligheder mm. og ud fra et forretningsmæssigt perspektiv giver det ikke mening at træne en model på alle salg og ejendomstypen vil påvirke salgsprisen. F.eks. vil der på sommerhuse være restriktioner på hvor meget om året man må bo i sommerhuset og der kan være i sommerhusområder være andre regler for hvad man må bruge sin grund til end der er i et parcelhusområde. Jeg har derfor ligeledes valgt at reducerer antallet af observationer således at de kun dækker almindelige parcelhus. Dette er gjort ved kun at beholde alle de ejendomssalg, hvor ejendommen i BBR er registreret med enheds- og bygningsanvendelsen 120. 

<<echo=FALSE>>=
train <- readRDS("train_klargjort_salg.rds")
@

Herefter er der `r nrow(train)` observationer tilbage og `r ncol(train)` attributter.

Der er for en del af attributterne en stor andel af manglende værdier. Det er især i forhold til variable fra BBR, som beskriver forskellige karakteristika ved selve boligen. Her har jeg har valgt at fjerne alle de attributter som har mere end 95\% manglende værdier.

<<echo=FALSE>>=
attribut_oversigt <- readRDS("atrtribut.rds")
attribut_oversigt <- attribut_oversigt[order(attribut_oversigt$pct_missing_values, decreasing = TRUE),]
knitr::kable(attribut_oversigt)
@

Som udgangspunkt vil jeg træne en model som skal være i stand til at kunne prædiktere værdien af et standard
parcelhus. Data som der skal trænes på skal derfor også være salg af standard parcelhuse.
Data er derfor blevet ensrettet på følgende måde:

% - enhed.antalvaerelser > 1 og enhed.antalvaerelser < 10
% - bolig_areal < 500 og bolig_areal > 50
% - bolig_alder > 0 og bolig_alder < 100
% - bygning.antaletager > 0 og bygning.antaletager < 4
% - enhed.antalbadevaerelser > 0 og enhed.antalbadevaerelser < 4
% - enhed.antalvandskylledetoiletter > 0 og enhed.antalvandskylledetoiletter < 4
% - fremskreven_pris_M2 < 30000 og fremskreven_pris_M2 > 0

Slutteligt er der blevet taget et valg om kun at udvælge de rå attributter som menes at være de
vigtigste i forhold til at forudsige værdien af et standad parcelhus.

<<echo=FALSE>>=
train <- readRDS("train_behandlet.rds")
attribut <- undersoeger_attributter(train)
knitr::kable(attribut)
@

 
De to attributter der dækker over tagtypematriale og ydervægsmateriale er diskrete variable som fordel
kan normaliseres med en one-out-of-k transformering.

Afstand til kyst og afstand til motorvej er to variable som jeg har valgt at binariserer. Det har jeg da de to
features forud for denne rapport er blevet imputeret. For afstand til kyst er afstanden op til 1500 meter målt.
Alt herover er imputeret til 1501 meter. Ligeledes er gjort for afstand til motorvej.
For at håndtere disse variable har jeg som sagt valgt at binariserer dem. For afstand til kyst har jeg ud fra et
forretningsmæssigt synspunkt valgt at en ejendom ligger så tæt på kysten at det har en effekt i dens værdi hvis
den ligger inden for 300 meter af kysten. Det samme er gjort for afstand til vej. Her er grænsen dog blot 100
meter.


<<echo=FALSE>>=
train <- one_out_of_k(train)
train <- binarisere_afstande(train)
@

\section{Data visualisering heriblandt Principal Component Analysis (PCA)}

De forskellige attributter indeholder ikke værdier på samme skala og jeg har derfor valgt at standardisere data.
Selve standardiseringen betyder at data for hver attribut reskaleres således at det får et gennemsnit på 1 med en
standardafvigelse på 1.

% $(X-\mu)/sd$ 
% 
Efter at have standardiseret data laves der en correlationsanalyse hvor resultaterne af visualiseres i et correlationsplot.

<<echo=FALSE, fig=TRUE>>=
train <- standardize(train)
correlation <- cor(train)
print(colnames(correlation))
rownames(correlation) <- 1:23
colnames(correlation) <- 1:23
corrplot(correlation)
@

<<echo=FALSE, fig=TRUE>>=
attributeNames <- colnames(train[1:ncol(train)])
X <- train[, 1:ncol(train)]
N = dim(X)[1]
M = dim(X)[2]
## Laver simpelt plot af udvalgte variable:
plot(X[ ,"EV_NN_M2"], X[ , "fremskreven_pris_M2"])
@


Salgsdata består efter alle datatransformationerne af `r N` observationer (N) med `r M` features (M).
En af de stærkere correlerede variable i data er EV\_NN\_M2 og de fremskrevne handelspriser. EV\_NN\_M2 er en variabel som dækker over områdeprisen. Den dækker over et vægtet gennemsnit over de nærmeste 15 solgte naboers kvadratmeterpriser. Det giver altså rigtig god mening at der er en stor korrelation mellem de solgte naboers kvadratmeter priser og ejendommens egen kvadratmeterpris.
Når de to variable plottes mod hinanden ser det således ud:

Der er en stærk korrelation mellem naboernes kvadratmeterpriser og ejendommens egen kvadratmeterpris. Det er fordi at priserne for en given lokation i høj grad er med til at bestemme prisen for en ejendom.
Udover områdeprisen er det boligens egen karakteristika som er med til at udgøre den samlede ejendomsværdi.

Som en del af PCA udregnes herefter Singular Value Decomposition (SVD).

<<echo = FALSE, fig=TRUE>>=
# 1 er per kolonne, 2 er per row..
Y <- t(apply(X,1,'-', colMeans(X))) # subtract the column means form columns of X
# You can check the column means using: colMeans(Y)
# PCA by computing SVD of Y:
# Bliver der her standardiseret to gange???
s <- svd(Y)
diagS <- s$d # d contains a vector of the singular values of x (of the length min(n, p))

rho <- (diagS^2)/(sum(diagS^2))
#sum(pcvariance[1:3])
threshold = 0.9

xlimits <- c(1, M);
plot(rho,
     type='o',
     main="Variance explained by principal componenets",
     xlab="Principal components",
     ylab="Variance explained",
     xlim=xlimits,
     ylim=c(0,1),
     col='blue')
lines(cumsum(rho), type='o', col='orange')
lines(xlimits, c(threshold, threshold), lty='dashed')

legend("right", # Define position
       legend=c("Individual", "Cumulative", "Threshold"), # Set strings for legend
       col=c("orange", "blue", "black"), lty=c(1,1,2), # Match appereance of lines
       cex=1.5, bg='lightblue') # Setup how the box looks (cex controls size)
@

De foeste 16 principal components kan man forklare 90\% af variationen i data. For at kommme over 95\% skal man have de 18 foerste komponenter. Der er i alt 23 mulige komponenter.
Det som det umiddelbart synes at betyde er at der ikke kan reduceres mange attibutter væk uden at det betyder at man mister variation.
PCA analysen kan bruges til at visualiserer data, feature extraction og compression.
Principal component analyse kan bruges til at reducerer dementionerne i data.

<<echo=FALSE, fig= TRUE>>=
Z <- s$u%*%diag(s$d)
i <- 1
j <- 2
plot(Z[, i ], Z[, j])
@


\chapter{Part II}

\section{Regression - part A}
I denne første del af del2 vil jeg med en linear regression forsøge at forudsige handelspriser baseret på bl.a. områdeprisen. Da der er en klar sammenhæng mellem hvad en ejendom bliver solgt for og hvad naboerne i området bliver solgt for er det klart en variabel som skal indgå i den lineære regressions model. Desuden er størrelsen og standen på ejendommen også relevant. Jeg har ikke stand som en attribut i mit datasæt men vil i stedet bruge boligens opførelsesår og ombygningsår som et inddirekte mål for dette. Er ejendommen eks. opført i starten af 1900 men har siden ingen ombygninger haft vil det i langt de fleste tilfælde betyde at bygningen er i en dårligere stand end en ejendom opført i 1970 med en ombygning i 2010. Det er kun større ombygninger som registreres i BBR. 
Håbet med denne regressionsanalyse er at man ud fra relativt få variable og en relativt simpel model vil kunne forudsige ejendomspriserne. Årsagen til at dette er en interessant analyse er at der med de tidligere offentlige ejendomsvurderinger har været store problemer i forhold til at forklare den enkelte borger hvorfor vedkommenes ejendomsværdi er som den er. 

Forud for regressionsanalysen er data blevet tranformeret. For faktorvariablene tagtype og vægmateriale har jeg valgt at tranformere med en one-of-k transformering. Herefter er alle attributer blevet standardiseret, således at de har en gennemsnit på 0 og en standardafvigelse på 1. 

<<echo=FALSE, fig=TRUE>>=
X <- train$EV_NN_M2
y <- train$fremskreven_pris_M2
plot(X, y, main="Linear regression", xlab="X", ylab="y")
@

Til at estimere modelparametrene til den lineære regressionsmodel har jeg den fremskrevne handelspris som den afhængige variabel og nabopriserne som prediktorvariabel. 

<<echo=FALSE, fig=TRUE>>=
# Estimate model parameters
w_est = lm(y ~ X);
# Plot the predictions of the model
plot(X, y, main='Linear regression', xlab="X", ylab="y");
y_est = w_est$coef[1] +w_est$coef[2]*X;
lines(X, y_est, col='red');
legend("topleft", legend=c("Data", "Fitted model"), fill=c("black", "red"))
@

Evalueringen af den lineære model kan foregå ved at man træner på et datasæt og prædikterer på et andet. I følgende regression har jeg nabokvadratmeterpriserne og boligarealet med som prediktorvariable. 

<<echo=FALSE, fig=TRUE>>=
x1 <- train[1:80000, ] 
x2 <- train[80000:185018, ]

dat <- data.frame(x1[, c("EV_NN_M2","bolig_areal")])
xnam <- paste("X", 1:2, sep="")
colnames(dat) <- xnam
y <- train$fremskreven_pris_M2[1:80000]
(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))
w_est = lm(fmla, data=dat);

# Plot the predictions of the model
plot(train$EV_NN_M2, train$fremskreven_pris_M2, main="Linear regression", xlab="X", ylab="y")

newdat <- data.frame(x2[, c("EV_NN_M2","bolig_areal")])
colnames(newdat) <- xnam
y_est = predict(w_est, newdata=newdat)
lines(newdat[,1], y_est, col='red')
legend("topleft", legend=c('Data', 'Fitted model'), fill=c("black", "red"))
@

Ud fra den estimerede model kan man plotte sin afhængige variabel mod de predikterede værdier.
Forskellen mellem den faktiske afhænge variabel (i dette tilfælde de fremskrevne salgspiser) og den prædikterede variabel (modellens estimerede y) er residualet.
Selve den lineære regressionsmodel kan anvanceres ved at tranformere inputvariablene. Det kan eksempelvis være at opløfte variablene i anden potens og bruge dem som prediktor sammen med den originale version af variablen.
Målet for denne simple lineære regressionsmodel er reducere 'cost-funktionen' så meget som muligt. For en simpel lineær regressiosmodel er cost-funktionen en squared error, hvilket vil sige at målet her er at reducere 'mean squared error' så meget som muligt - dog uden at overfitte. 
Andre cost-funktioner kan bruges til andre problemstillinger. En logistisk funktion kan således bruges til at hvis man arbejder med et logistisk regressionsproblem. 

<<echo=FALSE, fig=TRUE>>=
# Fit en lineær regressionsmodel til at forudsige salgspriser ud fra
# variablene 'ev_nn_m2' og 'bolig_areal'.
y =  train[, c("fremskreven_pris_M2")]
Xr =  train[, c("EV_NN_M2", "bolig_areal")]
xnam <- names(Xr)
(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))
w_est = lm(fmla, data=Xr)

print(summary(w_est))

# Make a scatter plot of predicted versus true values of Alcohol
y_est = w_est$fitted.values
plot(y, y_est, xlab='fremskreven_pris (true)', ylab='fremskreven_pris (estimated)',
     main='salgspriser', pch=20)

# Make a histogram of the residual error
hist(y-y_est, breaks=41, main="Residual error")

# Compute mean squared error
mean((y-y_est)^2)
@

I denne næste sektion bliver begrebet regularisering introduceret. Regularisering er en metode man kan bruge til at styre kompleksisteten af sin model. 
Til denne øvelser bliver der lavet en dobbelt krydsvalidering hvor det indre krydsvalideringsloop som bruges til at bestemme den optimale regulariseringsparameter (lambda).


2. Introduce a regularization parameter λ as discussed in chapter 14 of the lecture
notes, and estimate the generalization error for different values of λ. Specifically, choose a reasonable range of values of λ (ideally one where the generalization error first drop and then increases), and for each value use K = 10
fold cross-validation (algorithm 5) to estimate the generalization error.
Include a figure of the estimated generalization error as a function of λ in the
report and briefly discuss the result.
3. Explain how a new data observation is predicted according to the linear model
with the lowest generalization error as estimated in the previous question.
I.e., what are the effects of the selected attributes in terms of determining the
predicted class. Does the result make sense?


\section{Regression - part B}
\section{Classification}

\chapter{Part III}


\end{document}