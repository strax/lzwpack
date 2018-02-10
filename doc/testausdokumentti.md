# Testausdokumentti

Järjestelmän testit muodostuvat kolmesta kokonaisuudesta:
yksikkötesteistä, integraatiotesteistä sekä suorituskykytesteistä.

Yksikkötestit testaavat yksittäisten funktioiden arvoja eri syötteillä,
integraatiotestit testaavat järjestelmän toimintaa kokonaisuutena ja
suorituskykytestit mittaavat järjestelmän suorituskykyä erilaisilla syötteillä.

Yksikkö- ja integraatiotestit on toteutettu [ScalaTest][1]-testauskirjastolla. Lisäksi
ne tietorakenteet, jotka muodostavat jonkin algebrallisen rakenteen, sisältävät
[ScalaCheck][2]-kirjastolla toteutetut aksioomatestit. Nämä testit on pitkälti toimitettu `cats`-kirjastossa, eikä
niitä ole kirjoitettu itse. Testikattavuusraportti generoidaan IntelliJ:n sisäänrakennetulla
kattavuustyökalulla. Tällä hetkellä testikattavuus on hieman harhaanjohtava, sillä se sisältää
sekä yksikkötestien että integraatiotestien kattavuuden. Tämä pitää korjata tulevaisuudessa.

[1]: http://www.scalatest.org/user_guide
[2]: https://www.scalacheck.org/
