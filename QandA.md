# Fragen und Anworten während der Demonstration

**1. Es sind hier [Folie 25] wahrscheinlich Emoticons gemeint; Emojis sind Grafiken?**

Gemeint sind Emoticons und Emojis, letztere sind im Unicode aufgenommen und können von `R` dargestellt werden. Mit der Funktion `replace_emoji(x)` aus dem package [textclean](https://github.com/trinker/textclean) können Emojis in ihre Wortäquivalente transformiert werden.

**2. Was ist ein Toot? [Folie 2]?**

Ein "Tweet" auf [Mastodon](https://joinmastodon.org/de) (alternative, dezentrale Twitter-Variante) [*Antwort einer Teilnehmerin / eines Teilnehmers*]

**3. Warum wurden die Zeilenumbrüche (\n) nicht aus dem Text entfernt [Folie 28]?**

Hier wurde der Rohtext angezeigt. Beim Preprocessings wurden die Zeilenumbrüche durch die Tokenisierung entfernt.

**4. Gibt es eine Art "Erklärter Varianzanteil" - also wie bei der Wahl von Faktoren in der FA?**

Ein Äquivalent für Topics ist mir nicht bekannt.

**5. Gibt es Möglichkeiten oder bisherige Versuche der Standardisierung der semantischen Kohärenz und Exklusivität, die den Vergleich zwischen Studien möglich macht? [Folien 31]**

Hierzu sind mir keine bekannt.

**6. Kann man die namen der topics auch prädizieren lassen durchs modell oder muss man die selbst bestimmen?**

Zur Validierung der Themen empfiehlt es sich, die Namen der Topics (Labels) selbst zu bestimmen. Dadurch setzt man sich zwangsläufig genauer mit der Passung der Themen zu den Texten auseinander. 
Es gibt aber Ansätze der automatischen Vergabe der Labels, z. B.:

+ [Lau et al. (2011)](https://dl.acm.org/doi/abs/10.5555/2002472.2002658)
+ [Bhatia et al. (2016)](https://doi.org/10.48550/arXiv.1612.05340)
+ [Sorodoc et al. (2017)](https://aclanthology.org/E17-2111/)

**7. Wieviele rows braucht an mindestens und wieviel text in der text column, sprich wie groß müssen die eingangsdatensätze mind. sein?**

Darauf gibt es leider keine klare Antwort. [Banks et al.(2018, S. 450)](https://doi.org/10.1007/s10869-017-9528-3) empfehlen: "Typically, more text is better than less text. While there is no minimum number of words needed, we recommend at least 200 characters per document."
Daneben ist der Einfluss des Preprocessings zu bedenken: Werden dabei viele Wörter gefiltert oder nicht? Da Topic Modeling auf Wort-Kookkurenzen basiert, müssen auch genügend Wörter in den bereinigten Texten übrig bleiben.

**8. Kann man entscheiden, ob Wörter in mehreren Topics vorkommen dürfen, oder sind sie immer unique?**

Tatsächlich ist das die Stärke von Topic Modeling: Wörter können in mehrere Themen vorkommen ("mixed membership"). Einfluss darauf kann man mit dem `eta` hyperparameter (manchmal auch `beta` genannt) in LDA nehmen: Ist eta sehr klein (z. B. 0.00001), sind nur wenige Wörter für ein Thema wahrscheinlich.

**9. Welche Daten zur externen Validierung werden für Abstracts genutzt? Die meist angegebenen Keywords?**

Extern heißt: Daten, die nicht in das Topic Model eingeflossen sind, werden verwendet. Daher, ja: Datenbank-Metadaten können zur externen Validierung genutzt werden. Dies können standardisierte keywords (APA Thesaurus / MeSH Terms) oder Klassifikationsschemata (z. B. die [APA classification codes](https://www.apa.org/pubs/databases/training/class-codes)) sein (vgl. [Griffiths & Steyvers, 2004](https://www.pnas.org/doi/10.1073/pnas.0307752101)).
Ein [Skript zur Validierung mit den APA Codes](https://github.com/abitter/sdp22_supplements/blob/main/Analysis%20Code/Model%20evaluation/validate_with_classifications.R) wurde von [Bittermann & Rieger (2022)](https://aclanthology.org/2022.sdp-1.2) bereitgestellt.

**10. IdaPrototype: ist das bissl wie bootstrapping nur halt mit Worten?**

Bei [ldaPrototype](https://github.com/JonasRieger/ldaPrototype) wird die Modellinferenz für unterschiedliche *Seeds* wiederholt. Aus diesen wiederholten Modellinferenzen wird dann das Modell bestimmt, das den anderen am ähnlichsten ist. Ein *Seed* ist ein ganzzahliger Wert, welcher den Zufallszahlengenerator der Software reproduzierbar macht. Dies ist bei der Modellinferenz relevant, die  zumeist mit *collapsed [Gibbs sampling](https://en.wikipedia.org/wiki/Gibbs_sampling)* durchgeführt wird (vgl. [Griffiths & Steyvers, 2004](https://www.pnas.org/doi/10.1073/pnas.0307752101)). Hierbei werden Stichproben aus einer vorher spezifizierten Wahrscheinlichkeitsverteilung (Dirichlet) erzeugt, während beim Bootstrapping Resampling zur Bestimmung von Stichprobenverteilungen angewandt wird.

Eine ausführlichere Beschreibung des Gibbs-Samplings beim Topic Modeling (vgl. [Chen, 2011](https://blog.echen.me/2011/08/22/introduction-to-latent-dirichlet-allocation/)):

1. Zunächst wird jedem Wort in jedem Dokument zufällig ein Thema zugewiesen (sog. *random initialization*; hier kommt der Seed ins Spiel). Dadurch entstehen bereits Verteilungen von βK und θd, wenngleich noch keine sehr guten.

2. Diese Verteilungen werden für die vorliegenden Daten iterativ optimiert, indem für jedes Dokument d wie folgt vorgegangen wird:

  a. Für jedes Wort W in Dokument d und für jedes Thema K, berechne: 
  
  + den Anteil der Wörter im Dokument d, die momentan dem Thema K zugeordnet sind, und 
    
  + den Anteil der Zuordnungen zu diesem Thema in allen Dokumenten bei diesem Wort W. 
    
  b. Nun wird davon ausgegangen, dass alle Themenzuordnungen korrekt sind, außer für das aktuelle Wort W. Dessen Zuordnung wird aktualisiert mit der (durch die Dirichlet-Verteilung beeinflussten) Wahrscheinlichkeit, dass Thema K dieses Wort hat entstehen lassen. 

3. Je öfter dieser Aktualisierungsprozess wiederholt wird, desto besser werden die Zuordnungen, aus denen schließlich die finalen Verteilungen von βK und θd berechnet werden. Wie oft diese Aktualisierung stattfindet, wird von der analysierenden Person durch den Gibbs-Kontrollparameter "Anzahl der Iterationen"" festgelegt.
  
  
**11. Bzgl des Preprocessing; ist es möglich, Ausnahmen zu berücksichtigen? Bspw Stopwords entfernen, aber das Wort „wir“ beibehalten?**

Ja. Wenn man z. B. aus der default stopword-Liste des `quanteda` packages "wir" weglassen will, kann man das so coden:

```
# Zunächst die Position des Wortes im Stopword-Vektor finden:
index_wir <- which(quanteda::stopwords("de") == "wir")

# dann ein neues Stopword-Objekt erstellen unter Ausschluss dieser Position
stopwords_ohne_wir <- quanteda::stopwords("de")[-index_wir]
```

**12. Wie passen Präregistrierungen und topic modeling zusammen? Weil so iterativ. Wird das gemacht?**

Studien mit Topic Modeling kann man präregistrieren. Diese sind (meist) explorativer Natur und werden in der Präregistrierung entsprechend gekennzeichnet.
Man kann in der Präregistrierung angeben, in welchem Bereich man die Anzahl der Themen erwartet (falls das möglich ist).
Abweichungen von der Präregistrierung kann man dann im Manuskript berichten.
Entscheidend ist die Dokumentation (der Code), welche candidate models man geprüft hat und mit welcher Begründung man das finale Modell gewählt hat.
Meist genügt es, die Kriterien zu nennen (z. B. semantische Granularität, Ähnlichkeit der Themen, Interpretierbarkeit etc.).

**13. wie automatisiert läuft das mit dem Preprocessing?**

Grundsätzlich vollautomatisiert. Wenn es die Vermutung gibt, Teile des Preprocessings könnten Einfluss auf die Themeninterpretation nehmen, empfiehlt es sich, Modelle z. B. mit und ohne Lemmatisierung zu berechnen.
Beispielsweise könnte es interessant sein, in welchem Kontext das Wort "Flüchtlinge" vs. "Geflüchtete" vorkommen. Zu prüfen wäre, ob ein Stemming/Lemmatisierungsverfahren (es gibt verschiedene Ansätze) nicht beides zu "Flucht" transformiert und somit relevante Information verloren geht.

**14. Was sind das für linguistische Modelle, die die Beziehungen zwischen den Worten berücksichtigen?**

Bei Standard-LDA sind das Wort-Kookkurenzen. Es gibt aber auch Varianten, die die semantischen Beziehungen mit word embeddings berücksichten. z. B. [lda2vec](https://github.com/cemoody/lda2vec) oder [ETM](https://github.com/bnosac/ETM).

**15. Lassen sich die topics mit Persönlichkeitsmaßen korrelieren? Bspw wenn ein topic sehr prävalent bei Person 1 ist, dass die Person best. Persönlichkeit zeigt?**

Das ist denkbar. Man könnte die mittlere Themenprävalenz pro Person bestimmen (die document-topic probabilities in `theta` nach Autor:in aggregieren).
Dadurch erhält man für jedes Topic einen metrischen Vektor (mit Länge = Anzahl Personen), den man mit anderen Personenmerkmalen korrelieren kann.

**16. Können Topics, bspw in diesem Corpus, auch über die Zeit hinweg betrachtet werden?**

Ja, siehe Folie 49. Hierfür wurden die document-topic probabilities in `theta` nach Jahr aggregiert (Mittelwerte).

**17. Kann man die topics auch durch das Model prädizieren lassen oder muss man die selbst bestimmen?**

Wenn die Labels gemeint sind, siehe Frage 6. Wenn die Anzahl der Themen (`K`) gemeint ist: Die ist bei (Standard-)Topic Modeling selbst zu bestimmen. Eine Variante, bei der `K` automatisch bestimmt wird ist *Hierarchical Dirichlet Process* (HDP, [Teh et al., 2006](http://www.jstor.org/stable/27639773?origin=JSTOR-pdf)). Allerdings erfordert HDP das manuelle Setzen von Hyperparametern, die einen Einfluss auf `K` haben.

**18. Wurde TM mal mit qualitativen verfahren verglichen? Welches ist überlegen? (Abgesehen davon dass TM natürlich größere Datenmengen verarbeiten kann)**

Mir bekannt sind:

+ [Jacobs & Tschötschel (2019)](https://doi.org/10.1080/13645579.2019.1576317): Topic models meet discourse analysis: a quantitative tool for a qualitative approach
+ [Baumer et al. (2017)](https://doi.org/10.1002/asi.23786): Comparing Grounded Theory and Topic Modeling: Extreme Divergence or Unlikely Convergence?

**Weitere Literatur, die von Teilnehmenden empfohlen wurde:**

+ [Grimmer et al. (2022). Text as data](https://press.princeton.edu/books/paperback/9780691207551/text-as-data)
+ [Niekler (2016). Automatisierte Verfahren für die Themenanalyse nachrichtenorientierter Textquellen](http://asv.informatik.uni-leipzig.de/publication/file/350/Niekler_Diss.pdf)
+ [Silge & Robinson (n. d.). Text Mining with R](https://www.tidytextmining.com/)
+ [Rieger (2022). Reliability evaluation and an update algorithm for the latent Dirichlet allocation](https://eldorado.tu-dortmund.de/bitstream/2003/41102/2/Dissertation_mit_DOI.pdf)
