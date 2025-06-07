#!/usr/bin/env python
# coding: utf-8

# ## Import bibliotek


import json
import os
import time
import csv
import pandas as pd
import numpy as np
from selenium import webdriver
from selenium.webdriver.firefox.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import NoSuchElementException, TimeoutException


# ## Podstawowa konfiguracja.
# 
# Tutaj ustawiamy podstawowe opcje naszego skryptu. **BASE_URL** to główny link, który jest podstawą naszego zeskrobywania - posiada w swoim ciągu znaków wszelkie filtry jakie ustawiliśmy na stronie:
# ### https://www.pracuj.pl/praca 
# możemy ustawić miasto, promień wyszukiwania wokół tego miasta, oferowaną pozycję i kategorii ogłoszeń.
# 
# * **GECKODRIVER_FILEPATH** powinien kierować na plik wykonywujący geckodriver. Na Windowskie powinien prowadzić do pliku .exe. Na linuxie do katalogu /usr/bin/geckodriver.
# * **SCRIPT_DIR** i **CSV_FILEPATH** zapewniają, że nasza .csv powstanie w folderze wykonywania naszego skryptu jak używamy Jupytera. W innym przypadku należy odkomentować linijkę wyżej i skomentować tę widoczną teraz.
# * **CSV_HEADERS** ustala kolumny w pliku .csv.
# * **SCRAPE_CONFIG** ustala jakie informacje będziemy zeskrobywać ze stron ofert. Klucze powinny być zgodne z **CSV_HEADERS**, selektory z rzeczywistymi drogami .html na stronie ofert. Metoda **element** oznacza, że szukamy zapisamy tekst tam znajdujący się w jednym elemencie. Metoda **elements** z kolei utworzy listę elementów. Metoda **element_split1** i **element_split2** są dla elementów, gdzie pod jednym miejscem znajdują się dwa - przykładowo:
# 
# * ul. Aleje Jerozolimskie,
# * Warszawa, Mazowieckie.
# ##### **element_split1** złapie pierwszą część, a **element_split2** drugi.
# 


# Konfiguracja, wejdź na stronę www.pracuj.pl/praca/ i pofiltruj po chcianych wartościach, następnie tutaj przeklej i dodaj "&pn={page_number}" na końcu
# Oferty stażu z Finansów / Ekonomii
#BASE_URL_TEMPLATE = "https://www.pracuj.pl/praca/warszawa;wp/finanse%20ekonomia;cc,5008?rd=30&et=1&pn={page_number}"

# Oferty stażu, asystentów i juniorów z frazy "Analityk Danych"
BASE_URL_TEMPLATE = "https://www.pracuj.pl/praca/analityk%20danych;kw/warszawa;wp?rd=30&et=1%2C3%2C17&pn={page_number}"

#Bardziej skomplikowany
#BASE_URL_TEMPLATE = "https://www.pracuj.pl/praca/warszawa;wp?rd=30&cc=5008%2C5015%2C5003%2C5032%2C5001&et=1%2C3&pn={page_number}"

# bcs=2 (Finanse/Ekonomia) 5008, 18 (IT-Admin), 1 (Bankowość), 16 (Ubezpieczenia), 5001 (Admin. biurowa)
#cc=5008%2C5015%2C5003%2C5032%2C5001
# et=1 (Asystent), 2 (Praktykant/Stażysta)
# wp/warszawa - lokalizacja Warszawa
# rd=30 - promień 30km
# pn - numer strony

#ma tylko dwie strony, na rzecz testu
# BASE_URL_TEMPLATE = "https://www.pracuj.pl/praca/warszawa;wp?rd=30&cc=5003%2C5015001%2C5015002%2C5015003%2C5015005%2C5015006%2C5015007&et=1%2C3&pn={page_number}"

GECKODRIVER_FILEPATH = '/usr/bin/geckodriver'
#ustawienie folderu ze skryptem
#SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__)) # miejsce folderu ze skryptem - używać dla suchego skryptu python
SCRIPT_DIR = os.getcwd()  # miejsce dla folderu ze skryptem - używać dla jupytera
CSV_FILEPATH = os.path.join(SCRIPT_DIR, "pracuj_pl_oferty.csv")
CSV_HEADERS = [
    'offer_id',
    'offer_url',
    'position_name',
    'employer_name',
    'benefit_workplace_adress', # Metoda elementsplit0
    'benefit_workplace_city',     # Metoda elementsplit1
    'benefit_contracts',
    'benefit_work_schedule',
    'benefit_employment_type_name',
    'work_modes',
    'immediate_employment',
    'many_vacancies',       # Metoda elementsplit0
    'many_vacancies_count',        # Metoda elementsplit1
    'salary_per_contract_section',
    'remote_recruitment',
    'ukrainian_friendly',
    'it_specializations',
    'salary_components',
    'required_languages',
    'work_hours',
    'shift_work',
    'work_days',
    'paycheck_period',
    'earning_amount',
    'technologies_expected',
    'technologies_optional',
    'benefit_titles',
    'responsibilities_section',
    'requirements_section',
    'offered_section',
    'description_block',
    'additional_module_section'
]
SCRAPE_CONFIG = [
    {'key': 'position_name', 'selector': 'h1[data-test="text-positionName"]', 'method': 'element'},
    {'key': 'employer_name', 'selector': 'h2[data-test="text-employerName"]', 'method': 'element'},
    {'key': 'benefit_workplace_adress', 'selector': '[data-test="sections-benefit-workplaces"]', 'method': 'element_split0'},
    {'key': 'benefit_workplace_city', 'selector': '[data-test="sections-benefit-workplaces"]', 'method': 'element_split1'},
    {'key': 'benefit_contracts', 'selector': '[data-test="sections-benefit-contracts"]', 'method': 'element'},
    {'key': 'benefit_work_schedule', 'selector': '[data-test="sections-benefit-work-schedule"]', 'method': 'element'},
    {'key': 'benefit_employment_type_name', 'selector': '[data-test="sections-benefit-employment-type-name"]', 'method': 'element'},
    {'key': 'work_modes', 'selector': '[data-scroll-id="work-modes"]', 'method': 'element'},
    {'key': 'immediate_employment', 'selector': '[data-scroll-id="attribute-primary-immediate-employment"]', 'method': 'element'},
    {'key': 'many_vacancies', 'selector': '[data-scroll-id="attribute-primary-many-vacancies"]', 'method': 'element_split0'},
    {'key': 'many_vacancies_count', 'selector': '[data-scroll-id="attribute-primary-many-vacancies"]', 'method': 'element_split1'},
    {'key': 'salary_per_contract_section', 'selector': '[data-test="section-salaryPerContract"]', 'method': 'element'},
    {'key': 'remote_recruitment', 'selector': '[data-scroll-id="remote-recruitement"]', 'method': 'element'},
    {'key': 'ukrainian_friendly', 'selector': '[data-scroll-id="attribute-primary-ukrainian-friendly"]', 'method': 'element'},
    {'key': 'it_specializations', 'selector': '[data-test="it-specializations"]', 'method': 'elements'},
    {'key': 'salary_components', 'selector': '[data-test="salary-components"]', 'method': 'element'},
    {'key': 'required_languages', 'selector': '[data-test="required-languages"]', 'method': 'element'},
    {'key': 'work_hours', 'selector': '[data-test="work-hours"]', 'method': 'element'},
    {'key': 'shift_work', 'selector': '[data-test="shift-work"]', 'method': 'element'},
    {'key': 'work_days', 'selector': '[data-test="work-days"]', 'method': 'element'},
    {'key': 'paycheck_period', 'selector': '[data-test="paycheck-period"]', 'method': 'element'},
    {'key': 'earning_amount', 'selector': '[data-test="text-earningAmount"]', 'method': 'element'},
    {'key': 'technologies_expected', 'selector': '[data-test="item-technologies-expected"]', 'method': 'elements'},
    {'key': 'technologies_optional', 'selector': '[data-test="item-technologies-optional"]', 'method': 'elements'},
    {'key': 'benefit_titles', 'selector': '[data-test="text-benefit-title"]', 'method': 'elements'},
    {'key': 'responsibilities_section', 'selector': 'section[data-test="section-responsibilities"]', 'method': 'element'},
    {'key': 'requirements_section', 'selector': 'section[data-test="section-requirements"]', 'method': 'element'},
    {'key': 'offered_section', 'selector': 'section[data-test="section-offered"]', 'method': 'element'},
    {'key': 'description_block', 'selector': 'section[data-test="block-description"]', 'method': 'element'},
    {'key': 'additional_module_section', 'selector': 'section[data-test="section-additional-module"]', 'method': 'element'},
]


# ## Funkcje pomocnicze


# Funkcje pomocnicze do bezpiecznego pobierania danych
# Zwraca jeden text
def safe_get_element(driver_or_element, by, value):
    try:
        return driver_or_element.find_element(by, value).text.strip()
    except NoSuchElementException:
        return None

# Zwraca listę textów
def safe_get_elements(driver_or_element, by, value):
    try:
        elements = driver_or_element.find_elements(by, value)
        return [el.text.strip() for el in elements if el.text.strip()]
    except NoSuchElementException:
        return []
# Zwraca html
def get_element_html_or_none(driver_or_element, by, value):
    try:
        return driver_or_element.find_element(by, value).get_attribute('innerHTML').strip()
    except NoSuchElementException:
        return None

# Dodaje słownik data_dict na koniec .csv-ki na końcu filepath
def append_dict_to_csv(data_dict, filepath, fieldnames):
    file_exists = os.path.isfile(filepath)
    write_header = not file_exists or os.path.getsize(filepath) == 0

    try:
        with open(filepath, 'a', newline='', encoding='utf-8') as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames, extrasaction='ignore')
            # extrasaction='ignore' spowoduje, że dodatkowe klucze w data_dict (niebędące w fieldnames) zostaną zignorowane
            if write_header:
                writer.writeheader()
            writer.writerow(data_dict)
    except IOError as e:
        print(f"Błąd I/O:({e.errno}): {e.strerror} podczas pisania do {filepath}")
    except Exception as e:
        print(f"Niespodziewany błąd: {e} podczas pisania do {filepath}")


# ## Inicjalizacja WebDrivera i akceptowanie ciasteczek
# 
# Po wejściu na **BASE_URL** mogą pojawić się trzy pop-upy:
# * Za zaakceptowanie ciasteczek odpowiada **cookie_button**
# * Za zamknięcie okna logowania kontem Google **google_cross**
# * Za zaakceptowanie regulaminu prywatności **privacy_cross**


# Inicjalizacja WebDrivera (Firefox)

service = Service(GECKODRIVER_FILEPATH)
driver = webdriver.Firefox(service=service)

print("Rozpoczynam scrapowanie...")
# Akceptacja ciasteczek (jeśli się pojawi)
# Najpierw otwieramy stronę bazową, aby obsłużyć ciasteczka
driver.get(BASE_URL_TEMPLATE)
time.sleep(2) # Czekamy na załadowanie strony i ewentualne pop-upy

try:
    cookie_button_selector = "button[data-test='button-submitCookie']"
    cookie_button = WebDriverWait(driver, 10).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, cookie_button_selector))
    )
    cookie_button.click()

except TimeoutException:
    print("Nie znaleziono przycisku akceptacji ciasteczek lub już zaakceptowano.")
except Exception as e:
    print(f"Błąd podczas akceptacji ciasteczek: {e}")

# Klikamy x na popupie od logowania się do google, znajduje się na innym iframie    
try:
    driver.switch_to.frame(driver.find_element(By.CSS_SELECTOR, "iframe:nth-child(1)"))
    google_cross_selector = "#close"
    google_cross = WebDriverWait(driver, 10).until(
        EC.element_to_be_clickable((By.CSS_SELECTOR, google_cross_selector))
    )
    google_cross.click()
    driver.switch_to.default_content()

except TimeoutException:
    print("Nie znaleziono przycisku zamknięcia logowania google lub już zaakceptowano.")
except Exception as e:
     print(f"Błąd podczas akceptacji google'a.")

# Klikamy x na popupie o prywatności
try:
    privacy_cross_selector = driver.find_element(By.XPATH, '//div[@role="dialog"]//button')
    privacy_cross_selector.click()

except TimeoutException:
    print("Nie znaleziono przycisku akceptacji prywatności lub już zaakceptowano.")
except Exception as e:
    print(f"Błąd podczas akceptacji prywatności.")

print("Zaakceptowano wszystko.")
time.sleep(2)


# ## Stworzenie .csv i zbieranie linków do ofert
# 
# Następnie sprawdzamy czy plik .csv już istnieje, jeżeli tak to zbieramy z niego już zapisane w nim **offer_id** do listy **offers_csv_ids**, aby nie powtarzać dla nich analizy, w przeciwnym przypadku tworzymy go z etykietami kolumn według **CSV_HEADERS**. Następnie wchodzimy na **BASE_URL**, znajdujemy na nim ilość stron ofert, a następnie przechodzimy przez nie, zbierając linki do każdej z ofert, których **offer_id** nie widnieje na naszej liście **offers_csv_ids**.


#wyszukuje na stronie numer ostatniej strony
max_page_text = driver.find_element(By.CSS_SELECTOR, '[data-test="top-pagination-max-page-number"]').text
max_page = int(max_page_text)

# Tworzymy listę offer_id które już mamy w csv
offer_csv_ids = []
try:
    with open(CSV_FILEPATH, mode='r', newline='', encoding='utf-8') as csvfile:
        reader = csv.reader(csvfile)

        try:
            # Przeczytaj pierwszy opis kolumny
            header = next(reader)
        except StopIteration:
            # To oznacza że pliku nie ma
            print(f"Pliku .csv w {file_path}' nie ma.")

        # Sprawdzamy cze "offer_id" jest pierwsze w wierszu
        if not header or header[0] != 'offer_id':
            actual_first_header = header[0] if header and len(header) > 0 else "None or empty"
            print(f"Error: Pierwszy opis kolumny w '{CSV_FILEPATH}' nie jest 'offer_id'. Zamiast niego jest: '{actual_first_header}'.")

        # Przechodzimy po kolejnych wierszach
        for row in reader:
            print(row[0])
            offer_csv_ids.append(row[0])        
except FileNotFoundError:
    print(f"Nie znaleziono '{CSV_FILEPATH}'.")
except Exception as e:
    # Wszystkie inne errory
    print(f"Niespodziewany błąd w: '{CSV_FILEPATH}': {e}")

# Mam nadzieję że to zmniejszy złożoność obliczeniową przy następnych sprawdzeniach
offer_csv_ids.sort()


# Przechodzimy po kolejnych stronach z ogłoszeniami
offer_urls = []
for page_number in range(1, max_page + 1):
    current_page_url = BASE_URL_TEMPLATE.format(page_number=page_number)
    print(f"\nPrzechodzę na stronę: {current_page_url}")
    driver.get(current_page_url)
    time.sleep(1)
    start_length = len(offer_urls)


    #sprawdza czy oferty się załadowały
    try:
        WebDriverWait(driver, 15).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, 'div[data-test="section-offers"]'))
        )
    except TimeoutException:
        print(f"Nie udało się załadować sekcji ofert na stronie {page_number}. Prawdopodobnie koniec wyników.")

    # Tworzy listę dostępnych URL z "/praca/" i "link-offer" i sprawdzamy czy już wystąpiły w .csv
    offer_links_elements = driver.find_elements(By.CSS_SELECTOR, 'a.offer-title__link[href^="/praca/"], a[data-test="link-offer"]')

    for el in offer_links_elements:
        try:
            href = el.get_attribute('href')
            # Sprawdza czy link jest poprawny
            if href and href.startswith("https://www.pracuj.pl/praca/"):
                # Sprawdza czy dane id już jest w .csv
                if(href.split(',oferta,')[-1].split('?')[0] not in offer_csv_ids):
                    print("Dodaję ID: {id}".format(id=href.split(',oferta,')[-1].split('?')[0]))
                    offer_urls.append(href)
        except Exception as e:
            print(f"Błąd przy pobieraniu href: {e}")

    offer_urls = list(dict.fromkeys(offer_urls))
    print(f"Znaleziono {len(offer_urls)-start_length} unikalnych ofert na stronie {page_number}.")


# ## Zbieranie danych z poszczególnych linków
# 
#  Przechodzimy po liście **offer_uls** z poprzedniego punktu, która zawiera wszystkie unikalne linki z naszego zapytania. Dane z poszczególnych stron są zapisywane w 
#  słowniku **offer_data**, który ma klucze według zdefiniowanego **CSV_HEADERS**, a wartości są dodawanego według klucza, selektora i metody podanych w **SCRAPE_CONFIG**.


counter = 0
for offer_url in offer_urls:
    print(f"  Przetwarzam ofertę: {offer_url}")
    counter = counter + 1
    driver.get(offer_url)

    # Inicjalizacja słownika `data` wszystkimi kluczami z CSV_HEADERS z wartością None
    # To zapewnia, że każdy słownik będzie miał te same klucze, nawet jeśli niektóre dane nie zostaną znalezione
    offer_data = {header: None for header in CSV_HEADERS}

    # Pierwsze miejsce w .csv to id oferty
    try:
        offer_data['offer_id'] = offer_url.split(',oferta,')[-1].split('?')[0]
        print("Numer {counter} o ID: {id}".format(counter = counter, id=offer_data['offer_id']))
    except IndexError:
        print(f"Nie udało się wyciągnąć ID z url: {offer_url}")
    # Drugie to url
    try:
        offer_data['offer_url'] = offer_url
    except IndexError:
        print(f"Nie udało się sparsować url: {offer_url}")

    for config_item in SCRAPE_CONFIG:
        key_name = config_item['key']
        selector = config_item['selector']
        method_type = config_item['method']

        raw_value = None
        # dla elementów które zostaną zapisane w jednym stringu
        if method_type == 'element':
            raw_value = safe_get_element(driver, By.CSS_SELECTOR, selector)
        # dla elementów które zostaną zapisane w liście
        elif method_type == 'elements':
            raw_value = safe_get_elements(driver, By.CSS_SELECTOR, selector)
        # dla elementów posiadających dwa komponenty
        elif method_type == 'element_split0':
            element = safe_get_element(driver, By.CSS_SELECTOR, selector)
            if(element):
                try:
                    raw_value = safe_get_element(driver, By.CSS_SELECTOR, selector).split('\n')[0]
                except:
                    raw_value = None
        elif method_type == 'element_split1':
            element = safe_get_element(driver, By.CSS_SELECTOR, selector)
            if(element):
                try:
                    raw_value = safe_get_element(driver, By.CSS_SELECTOR, selector).split('\n')[1]
                except:
                    raw_value = None
        else:
            print(f"Nieznana metoda '{method_type}' dla klucza: '{key_name}'")
            continue # Przejdź do następnego itemu w konfiguracji

        offer_data[key_name] = raw_value

    append_dict_to_csv(offer_data, CSV_FILEPATH, CSV_HEADERS)



print("We are done")
driver.quit()



