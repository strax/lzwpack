# Käyttöohje

Tämä käyttöohje olettaa, että suoritettava tiedosto on nimeltään `lzwpack.jar`.

Ohjelma suoritetaan komennolla `java -jar lzwpack.jar`. Komento `java -jar lzwpack.jar --help` tulostaa seuraavan ohjenäkymän:

```
lzwpack LZW compression utility
Usage: lzwpack [options] path

  -d, --decompress    decompress input file instead of compression
  -b, --bits <value>  the maximum number of bits to use for each code
  path
  --help              prints this usage text
```

Ohjelma ottaa siis argumenttinaan pakattavan tiedoston sekä asetuksia. Pakattu data tulostetaan stdout-virtaan.
Ohjelma voidaan asettaa purkutilaan antamalla sille `--decompress`-vipu, jolla `path`-argumentti muuttuu pakatun tiedoston nimeksi ja stdout-virtaan tulostetaan pakkaamaton data. `--bits`-vipu määrittää pakkauksessa käytetyn maksimibittikoon jokaiselle koodille.