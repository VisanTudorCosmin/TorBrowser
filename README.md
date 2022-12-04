# TorScraper

TODO:
- [ ] extragerea metadatelor
- [ ] crearea unei diagrame pentru vizualizareea legaturilor dintre domenii

Pentru a rula aplicatia avem nevoie ca Haskell si stack sa fie instalate pe calculator. Putem face acest lucru urmand pasii de [aici](https://docs.haskellstack.org/en/stable/).

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
