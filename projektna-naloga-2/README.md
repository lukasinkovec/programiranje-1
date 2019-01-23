# Projektna-naloga-2

Najboljši trenutno igralsko aktivni košarkarji lige NBA
========================================================

Analiziral bom vse košarkarje iz lige NBA, ki so v tej ligi trenutno igralsko aktivni, njihove podatke pa bom pobiral iz
neuradne španske strani te iste lige, imenovane [Hispanosnba.com](https://en.hispanosnba.com/players/nba-active/index).

Za vsakega igralca bom zajel njegov/-o:
* ime in priimek
* igralni položaj
* višino (v metrih) in težo (v kilogramih)
* letnico rojstva
* državo iz katere prihaja
* karierno statistiko (t.j. povprečje odigranih minut ter doseženih točk, skokov in podaj na tekmo)

Delovne hipoteze:
* Ali obstaja povezava med državo, iz katere nek igralec prihaja, in pa njegovo letnico rojstva ali njegovim igralnim položajem?
* Kakšna je povezava med višino in težo nekega igralca ter njegovo karierno statistiko?
* Kateri od trenutno igralno aktivnih košarkarjev lige NBA, so najboljši glede na svojo karierno statistiko, po posameznem igralnem položaju?


drzave_po_letih = nba[['drzava', 'leto']].groupby('drzava').mean()
drzave_po_letih['leto'] = (drzave_po_letih.leto // 1) + round(drzave_po_letih.leto % 1)
drzave_po_letih[drzave_po_letih.leto == 1991.0]
drzave_po_letih.loc["Slovenia"]

polozaj_po_letih = nba[['polozaj', 'leto']].groupby('polozaj').mean()
polozaj_po_letih['leto'] = (polozaj_po_letih.leto // 1) + round(polozaj_po_letih.leto % 1)


nba['zaokrozene visine'] = (10 * ((200 * nba.visina + 5) // 10)) / 200
nba['zaokrozene teze'] = (10 * ((2 * nba.teza + 5) // 10)) / 2
nov_nba = nba[['tocke', 'zaokrozene visine', 'zaokrozene teze']]
tocke_po_visinah = nov_nba.groupby('zaokrozene visine').mean()
tocke_po_visinah['tocke'].plot()
tocke_po_tezah = nov_nba.groupby('zaokrozene teze').mean()
tocke_po_tezah['tocke'].plot()