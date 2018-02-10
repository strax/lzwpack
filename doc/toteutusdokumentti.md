# Toteutusdokumentti

Järjestelmä muodostuu LZW-algoritmin toteuttavista funktioista sekä
tietorakenteista. Molemmat ovat toteutettu funktionaaliseen tyyliin sivuvaikutuksia ja mutaatioita välttäen.
Pakkauslogiikka rakentuu [ƒs2][1]-kirjaston rajapinnan päälle, kutsuen
`Stream`-tyypille erinäisiä funktioita jotka muuttavat tietovirtaa. Pakkauslogiikka
on jaettu kahteen tiedostoon: `Format.scala` pakkaa ja purkaa bittijonoja yksittäisten tavujen yli, ja
`LZW.scala`ssa olevat funktiot hoitavat itse sanakirjan ylläpitämisen ja koodien generoinnin.
