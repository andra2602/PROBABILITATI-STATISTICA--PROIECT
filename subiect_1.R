## a)

n <- 50 # vreau 50 de etape pentru activitate
lambda <- runif(n, min=0.1, max=3) # generez 50 rate aleatoare pentru fiecare etapa cu valori intre 0.1 si 3 in vectorul lambda
# daca am gandit bine, un timp cu un lambda subunitar va dura mai mult decat unul cu lambda supraunitar
# practic daca T1 are lambda 0.5 => E(T1)=1/0.5=2, iar daca T2 are lambda 2.3 => E(T2)=1/2.3=0.43, adica E(T2)<E(T1)
alpha <- runif(n, min=0.1, max=0.9) # 50 de probabilitati de continuare cu valori mai mici ca 1(=100%) pentru fiecare etapa de a trece la urmatoarea
nr_simulari <- 10^6 # fiecare simulare va face persoana A sa parcurga o activitate cu cate n etape

T_val <- numeric(nr_simulari) # vector pentru timpii totalizati

for(j in 1:nr_simulari)
{
  T_total <- 0 # timpul total se reinitializeaza pentru inceputul fiecarei noi simulari
  i <- 1 # contor T_i
  
  while(i <= n) # n etape
  {
    T_i <- rexp(1, rate=lambda[i])# timpul etapei curente folosind rexp care calculeaza pmf-ul distributiei exponentiale in lambda[i]
    # rexp(k, ...) genereaza k variabile aleatoare, dar nu le generez pe toate o data cu rexp(n, ...) deoarece fiecare are alta rata si ar trebui sa fie incrementat i-ul
    # T_i <- lambda[i]*e^(-lambda[i]*x)
    
    T_total <- T_total + T_i
    
    if(runif(1, min=0, max=1) < alpha[i])# conditie continuare la etapa i+1 deoarece runif(1) genereaza o valoare intre 0 si 1 care e comparata cu probabilitate curenta de continuare alfa[i]
    {
      i <- i + 1
    } else
    {
      break # oprire, nu trec la etapa urmatoare
    }
  }
  
  T_val[j] <- T_total
}

E_T_simulat <- mean(T_val) # media valorilor simulate pentru a determina E(T)

cat("Estimarea E(T) prin simulare:", E_T_simulat, "\n")
hist(T_val, col = "lightblue", main = "Distributia T Simulat", xlab = "T", breaks = 50) # plotarea distributiei valorilor simulate pentru T
abline(v = E_T_simulat, col = "red", lwd = 2) # linia rosie pentru valoarea exactă a lui E(T)






## b)

n <- 50 # n etape
lambda <- runif(n, min=0.1, max=3)
alpha <- runif(n, min=0.1, max=0.9)
nr_simulari <- 10^6 # fiecare simulare va face persoana A sa parcurga o activitate cu cate n etape

T_val <- numeric(nr_simulari) # vector pentru timpii totalizati

for(j in 1:nr_simulari)
{
  T_total <- 0 # timpul total se reinitializeaza pentru inceputul fiecarei noi simulari
  i <- 1 # contor T_i
  
  while(i <= n) # n etape
  {
    T_i <- rexp(1, rate=lambda[i])# timpul etapei curente folosind rexp care calculeaza pmf-ul distributiei exponentiale in lambda[i]
    # rexp(k, ...) genereaza k variabile aleatoare, dar nu le generez pe toate o data cu rexp(n, ...) deoarece fiecare are alta rata si ar trebui sa fie incrementat i-ul
    # T_i <- lambda[i]*e^(-lambda[i]*x)
    
    T_total <- T_total + T_i
    
    if(runif(1, min=0, max=1) < alpha[i])# conditie continuare la etapa i+1 deoarece runif(1) genereaza o valoare intre 0 si 1 care e comparata cu probabilitate curenta de continuare alfa[i]
    {
      i <- i + 1
    } else
    {
      break # oprire, nu trec la etapa urmatoare
    }
  }
  
  T_val[j] <- T_total
}

E_T_simulat <- mean(T_val) # media valorilor simulate pentru a determina E(T)

P_reaches <- numeric(n)
for (i in 1:n) # calculez probabilitatea de a ajunge la fiecare etapa
{
  if (i == 1)
  {
    P_reaches[i] <- 1 # prob de a ajunge la prima etapa este mereu 1
  } else
  {
    P_reaches[i] <- P_reaches[i-1]*alpha[i-1] # produsul probabilitatilor anterioare
  }
}

E_T_calculat <- sum((1/lambda)*P_reaches) # valoare exacta a lui E(T)

cat("Estimarea E(T) prin simulare:", E_T_simulat, "\n")
cat("Valoarea exactă a lui E(T):", E_T_calculat, "\n")
hist(T_val, col = "lightblue", main = "Distributia T Simulat", xlab = "T", breaks = 50) # plotarea distributiei valorilor simulate pentru T
abline(v = E_T_calculat, col = "red", lwd = 2) # linia rosie pentru valoarea exactă a lui E(T)





# c)

n <- 50 # n etape
lambda <- runif(n, min=0.1, max=3)
alpha <- runif(n, min=0.1, max=0.9)
nr_simulari <- 10^6 # fiecare simulare va face persoana A sa parcurga o activitate cu cate n etape

T_val <- numeric(nr_simulari) # vector pentru timpii totalizati
finalizat_count <- 0 # count pt etape finalizate

for(j in 1:nr_simulari)
{
  T_total <- 0 # timpul total se reinitializeaza pentru inceputul fiecarei noi simulari
  i <- 1 # contor T_i
  
  while(i <= n) # n etape
  {
    T_i <- rexp(1, rate=lambda[i])# timpul etapei curente folosind rexp care calculeaza pmf-ul distributiei exponentiale in lambda[i]
    # rexp(k, ...) genereaza k variabile aleatoare, dar nu le generez pe toate o data cu rexp(n, ...) deoarece fiecare are alta rata si ar trebui sa fie incrementat i-ul
    # T_i <- lambda[i]*e^(-lambda[i]*x)
    
    T_total <- T_total + T_i
    
    if(runif(1, min=0, max=1) < alpha[i])# conditie continuare la etapa i+1 deoarece runif(1) genereaza o valoare intre 0 si 1 care e comparata cu probabilitate curenta de continuare alfa[i]
    {
      i <- i + 1
    } else
    {
      break # oprire, nu trec la etapa urmatoare
    }
  }
  
  T_val[j] <- T_total
  
  if (i > n)
  {
    finalizat_count <- finalizat_count + 1
  }
}

E_T_simulat <- mean(T_val) # media valorilor simulate pentru a determina E(T)

P_reaches <- numeric(n)
for (i in 1:n) # calculez probabilitatea de a ajunge la fiecare etapa
{
  if (i == 1)
  {
    P_reaches[i] <- 1 # prob de a ajunge la prima etapa este mereu 1
  } else
  {
    P_reaches[i] <- P_reaches[i-1]*alpha[i-1] # produsul probabilitatilor anterioare
  }
}

E_T_calculat <- sum((1/lambda)*P_reaches) # valoare exacta a lui E(T)

cat("Estimarea E(T) prin simulare:", E_T_simulat, "\n")
cat("Valoarea exactă a lui E(T):", E_T_calculat, "\n")
hist(T_val, col = "lightblue", main = "Distributia T Simulat", xlab = "T", breaks = 50) # plotarea distributiei valorilor simulate pentru T
abline(v = E_T_calculat, col = "red", lwd = 2) # linia rosie pentru valoarea exactă a lui E(T)

P_finalizare_simulata <- finalizat_count/nr_simulari 
P_finalizare_exacta <- prod(alpha)

cat("Probabilitatea de finalizare in simulare:", P_finalizare_simulata, "\n")
cat("Probabilitatea exacta de finalizare:", P_finalizare_exacta, "\n") # va da o valoare foarte mica 3.720195×10^−18, adica 0.00...03720195





# d)

n <- 50 # n etape
lambda <- runif(n, min=0.1, max=3)
alpha <- runif(n, min=0.1, max=0.9)
nr_simulari <- 10^6 # fiecare simulare va face persoana A sa parcurga o activitate cu cate n etape
sigma <- 5

T_val <- numeric(nr_simulari) # vector pentru timpii totalizati
count_sigma <- 0 # contor pentru simularile cu T_total <= sigma


for(j in 1:nr_simulari)
{
  T_total <- 0 # timpul total se reinitializeaza pentru inceputul fiecarei noi simulari
  i <- 1 # contor T_i
  
  while(i <= n) # n etape
  {
    T_i <- rexp(1, rate=lambda[i])# timpul etapei curente folosind rexp care calculeaza pmf-ul distributiei exponentiale in lambda[i]
    # rexp(k, ...) genereaza k variabile aleatoare, dar nu le generez pe toate o data cu rexp(n, ...) deoarece fiecare are alta rata si ar trebui sa fie incrementat i-ul
    # T_i <- lambda[i]*e^(-lambda[i]*x)
    
    T_total <- T_total + T_i
    
    if(runif(1, min=0, max=1) < alpha[i])# conditie continuare la etapa i+1 deoarece runif(1) genereaza o valoare intre 0 si 1 care e comparata cu probabilitate curenta de continuare alfa[i]
    {
      i <- i + 1
    } else
    {
      break # oprire, nu trec la etapa urmatoare
    }
  }
  
  T_val[j] <- T_total
  
  if (T_total <= sigma)
  {
    count_sigma <- count_sigma + 1
  }
}

E_T_simulat <- mean(T_val) # media valorilor simulate pentru a determina E(T)

P_reaches <- numeric(n)
for (i in 1:n) # calculez probabilitatea de a ajunge la fiecare etapa
{
  if (i == 1)
  {
    P_reaches[i] <- 1 # prob de a ajunge la prima etapa este mereu 1
  } else
  {
    P_reaches[i] <- P_reaches[i-1]*alpha[i-1] # produsul probabilitatilor anterioare
  }
}

E_T_calculat <- sum((1/lambda)*P_reaches) # valoare exacta a lui E(T)

cat("Estimarea E(T) prin simulare:", E_T_simulat, "\n")
cat("Valoarea exactă a lui E(T):", E_T_calculat, "\n")
hist(T_val, col = "lightblue", main = "Distributia T Simulat", xlab = "T", breaks = 50) # plotarea distributiei valorilor simulate pentru T
abline(v = E_T_calculat, col = "red", lwd = 2) # linia rosie pentru valoarea exactă a lui E(T)

P_sigma_simulata <- count_sigma/nr_simulari

cat("Probabilitatea de a termina intr-un timp <= sigma:", P_sigma_simulata, "\n")




# e)

n <- 3 # scad numarul de etape
lambda <- runif(n, min=0.1, max=3)
alpha <- runif(n, min=0.1, max=0.9)
nr_simulari <- 10^6 # fiecare simulare va face persoana A sa parcurga o activitate cu cate n etape

T_val_valid <- numeric(0) # vector pentru a stoca valorile timpului total T doar pentru simulari care au terminat toate cele n etape

for(j in 1:nr_simulari)
{
  T_total <- 0 # timpul total se reinitializeaza pentru inceputul fiecarei noi simulari
  i <- 1 # contor T_i
  
  while(i <= n) # n etape
  {
    T_i <- rexp(1, rate=lambda[i])# timpul etapei curente folosind rexp care calculeaza pmf-ul distributiei exponentiale in lambda[i]
    # rexp(k, ...) genereaza k variabile aleatoare, dar nu le generez pe toate o data cu rexp(n, ...) deoarece fiecare are alta rata si ar trebui sa fie incrementat i-ul
    # T_i <- lambda[i]*e^(-lambda[i]*x)
    
    T_total <- T_total + T_i
    
    if(runif(1, min=0, max=1) < alpha[i])# conditie continuare la etapa i+1 deoarece runif(1) genereaza o valoare intre 0 si 1 care e comparata cu probabilitate curenta de continuare alfa[i]
    {
      i <- i + 1
    } else
    {
      break # oprire, nu trec la etapa urmatoare
    }
  }
  
  if(i == n + 1) # s-au terminat toate cele n etape
  {
    T_val_valid <- c(T_val_valid, T_total)
  }
}

if(length(T_val_valid) > 0) # exista valori in T_val_valid
{
  T_min_finalizare <- min(T_val_valid)
  T_max_finalizare <- max(T_val_valid)
  
  cat("Timpul minim de finalizare a activitatii:", T_min_finalizare, "\n")
  cat("Timpul maxim de finalizare a activitatii:", T_max_finalizare, "\n")
  
  hist(T_val_valid, col = "lightblue")
} else
{
  cat("Nu exista simulari valide in care persoana A finalizeaza activitatea\n")
}



# f)

n <- 7 # n etape
lambda <- runif(n, min=0.1, max=3)
alpha <- runif(n, min=0.1, max=0.9)
nr_simulari <- 10^6 # fiecare simulare va face persoana A sa parcurga o activitate cu cate n etape

opriri_inainte_de_etapa <- numeric(n) # vector pentru a numara opririle inainte de fiecare etapa
probabilitati_oprire_exacte <- numeric(n)

for(j in 1:nr_simulari)
{
  i <- 1
  while(i <= n)
  {
    if(runif(1, min=0, max=1) < alpha[i]) # conditie continuare la etapa i+1 deoarece runif(1) genereaza o valoare intre 0 si 1 care e comparata cu probabilitate curenta de continuare alfa[i]
    {
      i <- i + 1
    } else
    {
      break # oprire, nu trec la etapa urmatoare
    }
  }
  
  for(k in 1:n) # creste numarul de opriri pentru fiecare etapa la care s-a oprit
  {
    if(i < k)
    {
      opriri_inainte_de_etapa[k] <- opriri_inainte_de_etapa[k] + 1
    }
  }
}

probabilitati_oprire_simulate <- opriri_inainte_de_etapa/nr_simulari

for(k in 2:n)
{
  P_finalizare_pana_la_k_minus_1 <- prod(alpha[1:(k-1)]) # prob de finalizare pana la etapa k-1
  probabilitati_oprire_exacte[k] <- 1-P_finalizare_pana_la_k_minus_1 # prob de oprire inainte de etapa k
}

cat("Probabilitatile de oprire (simulata) inainte de etapa k:", probabilitati_oprire_simulate, "\n")
cat("Probabilitatile de oprire (exacta) inainte de etapa k:", probabilitati_oprire_exacte, "\n")

par(mfrow = c(1, 2)) # 2 grafice
plot(1:n, probabilitati_oprire_simulate, type = "b", col = "blue",
     main = "Probabilitatea de oprire (simulata)",
     xlab = "Etapa k", ylab = "Probabilitatea de oprire")
grid()

plot(1:n, probabilitati_oprire_exacte, type = "b", col = "red",
     main = "Probabilitatea de oprire (exacta)",
     xlab = "Etapa k", ylab = "Probabilitatea de oprire")
grid()

