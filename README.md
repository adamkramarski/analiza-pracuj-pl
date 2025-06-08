# analiza-pracuj-pl
A project that scrapes pracuj.pl job offers and presents them using text analysis and data science algorithms


## Podstawowa konfiguracja scrapera.

Na początku skryptu są ustawiane kluczowe zmienne. **BASE_URL** to główny link, który jest podstawą naszego zeskrobywania - posiada w swoim ciągu znaków wszelkie filtry jakie ustawiliśmy na stronie:
### https://www.pracuj.pl/praca 
możemy ustawić miasto, promień wyszukiwania wokół tego miasta, oferowaną pozycję i kategorii ogłoszeń.

* **GECKODRIVER_FILEPATH** powinien kierować na plik wykonywujący geckodriver. Na Windowsie powinien prowadzić do pliku .exe. Na linuxie do katalogu /usr/bin/geckodriver.
* **SCRIPT_DIR** i **CSV_FILEPATH** zapewniają, że nasza .csv powstanie w folderze wykonywania naszego skryptu jak używamy Jupytera. W innym przypadku należy odkomentować linijkę wyżej i skomentować tę widoczną teraz.
* **CSV_HEADERS** ustala kolumny w pliku .csv.
* **SCRAPE_CONFIG** ustala jakie informacje będziemy zeskrobywać ze stron ofert. Klucze powinny być zgodne z **CSV_HEADERS**, selektory z rzeczywistymi drogami .html na stronie ofert. Metoda **element** oznacza, że szukamy zapisamy tekst tam znajdujący się w jednym elemencie. Metoda **elements** z kolei utworzy listę elementów. Metoda **element_split1** i **element_split2** są dla elementów, gdzie pod jednym miejscem znajdują się dwa - przykładowo:

* ul. Aleje Jerozolimskie,
* Warszawa, Mazowieckie.
##### **element_split1** złapie pierwszą część, a **element_split2** drugi.

## Podstawowa konfiguracja analizy

Po wykonaniu scrapera powstanie plik **pracuj_pl_oferty.csv** w folderze wykonywania jupytera/skryptu - zmień jego nazwę i typ na odpowiadającą oczekiwanemu przez ciebie skryptowi R: 
* dla Analiza_IT_benefity.R jest to pracuj_pl_IT.xlsx
* dla Analiza_IT_technologie.R jest to pracuj_pl_IT.csv
##### i umieść go w tym samym folderze co ten skrypt R.
