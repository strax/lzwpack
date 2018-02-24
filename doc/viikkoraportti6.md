# Viikkoraportti 6

Olen ollut tällä viikolla kiireinen, joten projekti on edistynyt vähemmän kuin aikaisempina viikkoina. BitBuffer-luokan
puskurin kasvatus osoittautui luultua haastavammaksi, joten lisäsin sen sijaan hajautustaululuokkaan ylivuotolistat mahdollisten
hajautusfunktion törmäysten varalta. Lisäksi aloin toteuttamaan `ListVector`-tietorakennetta, joka on linkitetyn listan tyyppinen
rakenne mutta tarjoaa O(log32(n)) haun mille tahansa indeksille. Tämäkin tietorakenne on toteutettu sisäisesti `SparseVector`-trien avulla. Ensi viikon suunnitelmana
on saada tämä listarakenne valmiiksi ja muuttaa muualla koodissa käytetyt listatyypit ListVectoreiksi.