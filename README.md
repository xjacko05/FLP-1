# FLP 2022/2023 - funkcionálny projekt: Haskell

## Popis

Program napísaný v jazyku Haskell implementujúci ECDSA. Určený pre Haskell 9.2.5 (pri použití s verziou 7.6.3 funkčnosť nezmenená).

## Rozsah implementácie

Program implementuje plnú funkcionalitu podľa zadania, formát vstupov je taktiež v súlade so zadaním (očakávané oddelenie pomocou medzery alebo tabulátoru). Formát výstupov je taktiež podľa zadania (oddelené iba medzerou za názvom parametra nasledovaným dvojbodkou a LF za hodnotou parametra).

## Testovanie

Pre testovanie funkčnosti implementácie bolo vytvorených 7 testov.
1. Načíta krivku a vypíše ju naspäť na stdout
2. Pre danú krivku vygeneruje nový KeyPair
3. Vytvorí podpis pomocou vygenerovaného kľúča
4. Overí podpis so správnym kľúčom
5. Overí podpis s nesprávnym kľúčom
6. Overí poskytnutý podpis pomocou poskytnutého kľuča (rovnaké ako v zadaní)
7. Overí nesprávny podpis pomocou poskytnutého kľuča (rovnaké ako v zadaní)
Pre prípad záujmu sú vstupy a výstupy testov zachované až do vykonania ``` make clean```.

## Použitie

Preklad (spúšťa GHC s voľbou -Wall):
```bash
make
```

Binárny súbor `flp22-fun` je následne možné spustiť:
```bash
flp22-fun <-i | -k | -s | -v> [input-file]
```

Spustenie testov:
```bash
make test
```

Pre upratanie po preklade a spustení testov:
```bash
make clean
```
