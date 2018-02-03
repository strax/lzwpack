# Viikkoraportti 3

Tällä viikolla keskityin algoritmin toiminnallisuuden valmiiksi saamiseen 
sekä koodin refaktorointiin. Funktionaalisella tyylillä tehdyn koodin
refaktorointi osoittautui huomattavasti oletettua haastavammaksi, joten 
isompien testaussyötteiden mukaanotto projektiin siirtyi ensi viikolle.
Joka tapauksessa refaktoroinnin ansiosta toteutin tietorakenteen `BitBuffer`, joka
tarjoaa linkitetyn listan tyylisen rajapinnan kokonaisluvun yksittäisiin bitteihin. Tämä puolestaan
yksinkertaisti algoritmin binääripakkausosaa huomattavasti.

Ensi viikon agendalla on integroida projektiin testikattavuustyökalu ja toteuttaa puuttuvat testit, sekä lisätä
hyväksyntätestit isommille syötteille. Lisäksi tarkoituksena on aloittaa algoritmin käyttämien tietorakenteiden toteutus;
hyvä ensimmäinen tietorakenne lienee yksinkertainen linkitetty lista, joka korvaa 
projektin monessa paikassa käytetyt `Seq` ja `List` -tyypit.