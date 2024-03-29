# TorScraper

Pentru a rula aplicatia avem nevoie ca Haskell si stack sa fie instalate pe calculator. Putem face acest lucru urmand pasii de [aici](https://docs.haskellstack.org/en/stable/).

Pentru a putea acesa reteaua TOR avem nevoie de [Tor Browser](https://www.torproject.org/download/) sa fie pornit pentru a avea procesul tor activ pe portul '9150'.

Applicatia are urmatoarele setari
```
λ stack exec -- TorScraper --help
Help Options:
  -h, --help
    Show option summary.
  --help-all
    Show all help options.

Application Options:
  --seed :: text
    seed url to start
    default: ""
  --export :: bool
    export data
    default: false
```

Pentru a incepe extragerea de link-uri avem nevoie de un URL de la care putem incepe. Pentru a seta acest URL avem optiunea 'seed' urmata de url. Exemplu:
```
λ stack exec -- TorScraper --seed=http://c32zjeghcp5tj3kb72pltz56piei66drc63vkhn5yixiyk4cmerrjtid.onion/
Deleting all pages not scraped from database
[Debug#SQL] DELETE FROM "page" WHERE ("scraped"=?); [PersistBool False]
[Debug#SQL] DELETE FROM "page" WHERE "to"=?; [PersistText "http://c32zjeghcp5tj3kb72pltz56piei66drc63vkhn5yixiyk4cmerrjtid.onion/"]
[Debug#SQL] SELECT "id","from","to","scraped","timestamp" FROM "page" WHERE "to"=?; [PersistText "http://c32zjeghcp5tj3kb72pltz56piei66drc63vkhn5yixiyk4cmerrjtid.onion/"]
```

Aplicatia va extrage toate link-urile din pagina data. Urmand ca fiecare link extras sa devina seed.

Dupa ce am adunat destule date, pentru a vedea de cate ori domeniul A a avut referinte catre domeniul B folosim comanda
```
λ stack exec -- TorScraper --export
```

Acceasta comanda va crea un fiser data.csv care contine pe fiecare linie trei valori, adresa domeniului A, adresa domeniului B si numarul de referinte. Exemplu: 
```
c32zjeghcp5tj3kb72pltz56piei66drc63vkhn5yixiyk4cmerrjtid.onion,c32zjeghcp5tj3kb72pltz56piei66drc63vkhn5yixiyk4cmerrjtid.onion,23
```
Este creat si fiserul pageinfo.json care contine pentru fiecare pagina in parte titlul si un set keywords care se regasesc in aceasta.
```
{
    "pageInfoAddr": "http://c32zjeghcp5tj3kb72pltz56piei66drc63vkhn5yixiyk4cmerrjtid.onion/f/AntiCopyright",
    "pageInfoKeywords": "Submitted, January, December, August, emoticons, February, October, devender, drummyfish, November, FuckTheRIAA, September, CircleA, Public, Unlicense, License, Simplified, Apache, General, Affero",
    "pageInfoTitle": "Anti-Capitalist, Anti-Copyright"
}
```

Mai este creat si fiserul result.json care contine un obiect json cu doua campuri 'nodes' si 'links'. Campul 'nodes' reprezinta lista cu toate adresele onion care au fost gasite. Campul 'links' reprezinta lista cu toate relatiile dintre aceste adrese. Acest obiect json este folosit pentru a afisa diagrama care ne ajuta sa vizualizam relatiile. 

Pentru afisarea diagramei am utlizat exemplul urmator: https://plnkr.co/edit/cVaILhLVwAumWr5QEvSW?p=preview&preview 

Exemplu de diagrama afisata:
![Alt text](graph.png?raw=true "diagrama")

Din cauza politici de siguranta CORS diagrama va fi afisata doar prin deschiderea fiserului index.html cu un server local. (Ex: brackets)

Adresa unui nod va aparea prin aducerea mouse-ului deaspura lui.