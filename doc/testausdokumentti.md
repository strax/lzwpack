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

## Suorituskykytestit

### `CompressionBenchmarks`

Nämä suorituskykytestit mittaavat koko järjestelmän suorituskykyä makrotasolla.

Tuloksissa yksikkö `op` tarkoittaa yhtä operaation suoritusta, mikä toisin sanoen
vastaa pakkausta tai purkua annetulla syötteellä. Näin ollen `nopeus (MB/s)` voidaan johtaa kaavalla
`(1 / t) * S`, jossa `t` on yhden operaation viemä aika sekunneissa ja `S` on operaatiolle annetun syötteen
koko.

Päivitetty 17.2.

| Testi | Syötteen koko (MB) | Nopeus (s/op), n = 10 | Nopeus (MB/s) |
| ----- | ------------- | --------------------- | ------------- |
| `compressAlice29` | 0,149 | 0,613 ± 0,061 | ~0,243 |
| `decompressAlice29` | 0,061 | 0,604 ± 0,264 | ~0,100 |
| `compressPtt5` | 0,501 | 1,880 ± 0,264 | ~0,266 |
| `decompressPtt5` | 0,061 | 0,598 ± 0,089 | ~0,102 |

Tällä hetkellä suorituskyky näyttäisi olevan hyvin heikkoa moneen "oikeaan"
pakkausalgoritmiin verrattuna. Toisaalta pakkausnopeudessa ei vaikuta olevan eroa
syötteen kokoon nähden, joten empiirisesti tarkasteltuna O(n) aikavaativuus voisi toteutua sekä pakkaukselle että purkamiselle.

[1]: http://www.scalatest.org/user_guide
[2]: https://www.scalacheck.org/
