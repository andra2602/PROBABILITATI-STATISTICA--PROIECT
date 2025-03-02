# PROBABILITATI-STATISTICA--PROIECT

Acest proiect a fost realizat alături de alți 3 studenți din cadrul Facultății de Matematică și Informatică, cu scopul de a aplica concepte din domeniul probabilităților și statisticii utilizând limbajul R. Echipa noastră a fost formată din patru membri, fiecare contribuind la diferite aspecte ale proiectului.

În ceea ce mă privește pe mine, alături de o colegă, am împărțit o sarcină mai voluminoasă, respectiv **Subiectul 2**, care a implicat analiza și procesarea unui volum semnificativ de date.

Am utilizat R pentru a implementa metode statistice avansate, pentru a efectua analize de probabilitate și pentru a vizualiza rezultatele într-un mod intuitiv. Proiectul nostru include atât calcule matematice complexe, cât și grafice care ajută la înțelegerea și interpretarea datelor din diverse perspective.

Prin acest proiect, am reușit să aprofundăm cunoștințele noastre în domeniul statisticii și al programării în R, aplicând teoria în practică și obținând rezultate valoroase.



# Aplicație Shiny pentru Analiza Distribuțiilor și Transformărilor Variabilelor Aleatoare

### **Descriere**

Această aplicație interactivă, dezvoltată în R Shiny, oferă utilizatorilor posibilitatea de a explora distribuțiile de variabile aleatoare și de a aplica transformări pe acestea pentru a observa comportamentele și efectele acestor schimbări. Proiectul integrează concepte fundamentale din probabilități și statistică, având ca scop atât educarea utilizatorilor, cât și facilitarea unui mediu interactiv pentru simularea și analiza diverselor tipuri de distribuții și transformări matematice.

Prin intermediul acestei aplicații, utilizatorii pot vizualiza distribuțiile clasice (Normală, Poisson, Binomială, Exponențială), aplica funcții personalizate pentru transformarea variabilelor aleatoare și analiza rezultatelor prin intermediul unor grafice și repartiții empirice dinamice.
___

### **Funcționalități**

- **Generarea și reprezentarea distribuțiilor**: Permite generarea de eșantioane din distribuțiile: **Normală**, **Poisson**, **Binomială**, **Exponențială**.
- **Aplicarea transformărilor pe variabile aleatoare**: Permite utilizatorului să aplice transformări precum **3 - 2X**, **X²**, **∑X**, **∑X²** pe variabilele aleatoare și să vizualizeze efectele acestora asupra distribuției.
- **Afișarea funcției de repartiție empirică (ECDF)**: Utilizatorii pot analiza distribuțiile și transformările acestora prin grafice interactive ale funcției de repartiție empirică.
- **Vizualizarea funcțiilor personalizate**: Permite utilizatorilor să creeze și să vizualizeze funcții personalizate definite analitic.
- **Interfață interactivă**: Utilizatorii pot ajusta parametrii distribuțiilor și transformărilor și pot explora grafic comportamentele acestora printr-o interfață dinamică și ușor de utilizat.
___

### **Arhitectura Aplicației**

Proiectul este structurat în două module principale:

1. **Distribuții Variabile Aleatoare**:
   - Generare de eșantioane din distribuțiile alese.
   - Aplicarea transformărilor pe variabile aleatoare.
   - Reprezentarea grafică a funcției de repartiție empirică (ECDF) pentru distribuții și transformări.
   - Permite compararea diferitelor distribuții și vizualizarea comportamentului acestora.

2. **Funcții Personalizate**:
   - Crearea și vizualizarea unor funcții definite analitic, care pot fi personalizate de utilizator.
   - Ajustarea parametrilor și intervalelor de definiție ale funcțiilor pentru explorare detaliată.
   - Posibilitatea de a compara diferite funcții personalizate și analiza efectele acestora asupra distribuțiilor.
___

### **Tehnologii Utilizate**

- **R Shiny**: Folosit pentru a crea interfața reactivă și pentru a gestiona interacțiunile utilizatorului.
- **ggplot2**: Utilizat pentru generarea de grafice vizuale pentru distribuții, transformări și repartiții empirice.
- **dplyr & tidyr**: Folosite pentru manipularea și preprocesarea datelor necesare aplicației.
___

### **Cum rulezi aplicația?**

1. Clonează repository-ul:
   ```bash
   git clone https://github.com/andra2602/PROBABILITATI-STATISTICA--PROIECT.git
   ```

2. Deschide RStudio și încarcă fișierul **App.R**.

3. Rulează aplicația cu comanda:
   ```r
   shiny::runApp()
   ```

---
