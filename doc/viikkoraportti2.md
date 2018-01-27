# Viikkoraportti 2

Tällä viikolla olen aloittanut LZW-algoritmin toteutusta Scalan valmiilla tietorakenteilla.
Valitsin IO:n tekemiseen [fs2-kirjaston](https://github.com/functional-streams-for-scala/fs2/),
joka tarjoaa puhtaan funktionaalisen rajapinnan virtaavien IO-sovellusten tekemiseen.

Algoritmin toteutus on tällä hetkellä toimiva itse sanakirjan muodostuksen suhteen, mutta
purkuvaiheen binääritason pakkaus tavurajojen yli puuttuu vielä. Binääripakkaus toimii
ainakin sillä tasolla kun yksikkötesteissä on määritelty, mutta ensi viikolla tulee vielä tarkistaa, että
pakkaus toimii varmasti Welchin kuvaileman algoritmin mukaisesti.

Olen tuonut projektiin Canterburyn korpuksen, joka on ilmeisesti eniten käytetty tekstikokoelma pakkausalgoritmien testaamiseen.
Aionkin testata koko pakkausputken toimintaa (end-to-end testing)
korpuksella heti kun purkaminenkin toimii luotettavasti, eli tavoitteen mukaan ensi viikolla.
Olen myös selvittänyt, olisiko mahdollista saada työkalusta yhteensopiva Unixin `compress`-komentorivityökalun kanssa;
binääriyhteensopivuus takaisi mahdollisuuden testata ohjelman toimivuutta helposti siten, että
lzwpackin tulostetiedoston MD5-hajautusarvoa voitaisiin verrata `compress`-työkalun pakkaaman tiedoston
hajautusarvoon. Lisäksi yhteensopivuus tarjoaisi mielenkiintoista verrokkidataa tämän toteutuksen nopeuden suhteen.

Ensi viikon tavoitteena on siis saada algoritmi toimimaan pakkauksen ja purkamisen suhteen ja lisätä testipatteriin
isompia syötteitä Canterburyn korpuksesta sekä refaktorointi. Suunnitelmissa ei ole vielä aloittaa
Scalan tietorakenteiden korvaamista omilla tietorakenteilla ensi viikolla.